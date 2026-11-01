"""Record selection of the WPCT port: filters, treatment abbreviation, pooling, study numbering and
the two halting checks.

Spec items (``python/wpct/docs/sas_specification.md``):

- WPCT-F.07.02: S02-FLT-01, S02-TRN-02, S02-POP-01, S02-FLT-02, S02-FLT-03 and S02-TRN-03.
- WPCT-F.07.07: S07-FLT-01, S07-TRN-02, S07-FLT-02, S07-POP-01, S07-FLT-03, S07-TRN-04, S07-FLT-04,
  S07-TRN-05, S07-TRN-06 and S07-FLT-05.

Every function is pure. It takes a DataFrame, never mutates it, and returns a new frame with a
fresh ``RangeIndex``; the halting checks return nothing or raise. Filters keep the input row order,
as a DATA step with WHERE does. Variables are found as SAS finds them: the exact spelling first,
otherwise the one column whose name matches case-insensitively. Character comparisons ignore
trailing blanks, and a blank value is missing (D-11).

Only steps that change records or halt processing are ported. The ``attrib ... label=`` of
``trtp_short`` is presentation and has no counterpart here.
"""

import math
import numbers
from collections.abc import Iterable
from types import MappingProxyType

import numpy as np
import pandas as pd

from wpct.errors import AnalysisHalted
from wpct.sas_semantics import (
    SAS_BLANK,
    assign_char,
    char_missing_mask,
    is_char_missing,
    prefix_match,
    sas_sort,
)
from wpct.schemas import POOLED_STUDYID

__all__ = [
    "FLAG_YES",
    "TREATMENT_ABBREVIATIONS",
    "UNEXPECTED_TREATMENT",
    "abbreviate_treatment",
    "assert_unique_keys",
    "assert_var_nonmissing",
    "assign_studynum",
    "exclude_visit",
    "flag_filter",
    "keep_variables",
    "post_baseline_filter",
    "stack_pooled",
    "subset_param_timepoints",
]

#: The WHEN branches of ``select (trtp)`` [whitepapers/WPCT/WPCT-F.07.02.sas:97-102;
#: whitepapers/WPCT/WPCT-F.07.07.sas:152-157] (S02-TRN-02, S07-TRN-02). WHEN compares exactly, so
#: case matters and only trailing blanks are ignored.
TREATMENT_ABBREVIATIONS = MappingProxyType(
    {
        "Placebo": "P",
        "Xanomeline High Dose": "X-high",
        "Xanomeline Low Dose": "X-low",
    }
)

#: The OTHERWISE value, which the ``$6`` length stores as ``UNEXPE`` (D-12).
UNEXPECTED_TREATMENT = "UNEXPECTED"

#: The literal of the population and analysis-flag tests ``&p_fl = 'Y'`` and ``&a_fl = 'Y'``
#: [whitepapers/WPCT/WPCT-F.07.02.sas:164; whitepapers/WPCT/WPCT-F.07.07.sas:213].
FLAG_YES = "Y"


# --- Helpers --------------------------------------------------------------------------------------


def _name_list(names: "str | Iterable[str]", caller: str) -> list[str]:
    """Return variable names as a list; a string is a blank-separated SAS variable list."""
    if isinstance(names, str):
        return names.split()
    result = list(names)
    invalid = [name for name in result if not isinstance(name, str)]
    if invalid:
        raise TypeError(f"{caller}: variable names must be strings, got {invalid!r}")
    return result


def _find_column(df: pd.DataFrame, name: str, caller: str) -> "str | None":
    """Return the column SAS would take for variable ``name``, or None when there is none.

    SAS variable names are case-insensitive, pandas column names are not, so a name that is not
    spelled exactly as a column matches the one column equal to it case-insensitively.
    """
    if name in df.columns:
        return name
    folded = name.upper()
    matches = [
        column for column in df.columns if isinstance(column, str) and column.upper() == folded
    ]
    if len(matches) > 1:
        raise ValueError(f"{caller}: variable {name} matches several columns: {', '.join(matches)}")
    return matches[0] if matches else None


def _columns(df: pd.DataFrame, names: list[str], caller: str) -> list[str]:
    """Return the columns of ``names``, raising ``KeyError`` that names every absent variable."""
    found = [_find_column(df, name, caller) for name in names]
    absent = [name for name, column in zip(names, found, strict=True) if column is None]
    if absent:
        raise KeyError(f"{caller}: variables not in the frame: {' '.join(absent)}")
    return found


def _column(df: pd.DataFrame, name: str, caller: str) -> str:
    return _columns(df, [name], caller)[0]


def _is_numeric(series: pd.Series) -> bool:
    return pd.api.types.is_numeric_dtype(series.dtype) and not pd.api.types.is_bool_dtype(
        series.dtype
    )


def _require_numeric(series: pd.Series, caller: str) -> None:
    if not _is_numeric(series):
        raise TypeError(
            f"{caller}: variable {series.name} must be numeric, got dtype {series.dtype}"
        )


def _char_values(series: pd.Series, caller: str) -> np.ndarray:
    """Return a character variable's values as SAS compares them (D-11).

    Trailing blanks are dropped and a missing value (blanks, ``None`` or NaN) becomes ``''``.
    Raises ``TypeError`` for a non-missing value that is not a string.
    """

    def normalise(value: object) -> str:
        if is_char_missing(value):
            return ""
        if not isinstance(value, str):
            raise TypeError(
                f"{caller}: variable {series.name} must be character, got {type(value).__name__} "
                f"{value!r}"
            )
        return value.rstrip(SAS_BLANK)

    values = series.to_numpy(dtype=object)
    return np.array([normalise(value) for value in values], dtype=object)


def _group_values(series: pd.Series, caller: str) -> np.ndarray:
    """Return a BY or GROUP BY variable's values with SAS equality: missing values are one value."""
    if _is_numeric(series):
        return series.to_numpy(dtype="float64", na_value=np.nan)
    return _char_values(series, caller)


def _first_flags(series: pd.Series, caller: str) -> np.ndarray:
    """Return ``first.<var>`` for each row of ``series``, which is in BY order."""
    if len(series) == 0:
        return np.zeros(0, dtype=bool)
    values = _group_values(series, caller)
    same = values[1:] == values[:-1]
    if values.dtype.kind == "f":
        missing = np.isnan(values)
        same = same | (missing[1:] & missing[:-1])
    return np.concatenate(([True], ~same.astype(bool)))


def _timepoint_pairs(
    param_timepoints: "Iterable[tuple[str, Iterable[float]]]",
) -> list[tuple[str, tuple[float, ...]]]:
    """Validate the ``(paramcd, timepoints)`` pairs of S02-FLT-01 and S07-FLT-01."""
    caller = "subset_param_timepoints"
    if isinstance(param_timepoints, str):
        raise TypeError(f"{caller}: expected (paramcd, timepoints) pairs, got a string")
    pairs = []
    for pair in param_timepoints:
        try:
            paramcd, timepoints = pair
        except (TypeError, ValueError):
            raise TypeError(
                f"{caller}: expected a (paramcd, timepoints) pair, got {pair!r}"
            ) from None
        if not isinstance(paramcd, str):
            raise TypeError(f"{caller}: PARAMCD value must be a string, got {paramcd!r}")
        if isinstance(timepoints, str) or not isinstance(timepoints, Iterable):
            raise TypeError(
                f"{caller}: timepoints of {paramcd} must be a sequence, got {timepoints!r}"
            )
        values = tuple(timepoints)
        invalid = [
            value
            for value in values
            if isinstance(value, bool) or not isinstance(value, numbers.Real)
        ]
        if invalid:
            raise TypeError(f"{caller}: ATPTN values of {paramcd} must be numbers, got {invalid!r}")
        pairs.append((paramcd.rstrip(SAS_BLANK), tuple(float(value) for value in values)))
    return pairs


# --- Record filters -------------------------------------------------------------------------------


def subset_param_timepoints(
    df: pd.DataFrame, param_timepoints: "Iterable[tuple[str, Iterable[float]]]"
) -> pd.DataFrame:
    """Keep the records of the configured parameter and timepoint pairs (S02-FLT-01, S07-FLT-01).

    A record is kept when, for some ``(paramcd, timepoints)`` pair, ``PARAMCD`` equals ``paramcd``
    and ``ATPTN`` is in ``timepoints``:

    - 07.02 passes ``(("DIABP", (815,)), ("SYSBP", (816,)))``, the WHERE ``(paramcd in ('DIABP') and
      atptn in (815)) or (paramcd in ('SYSBP') and atptn in (816))``
      [whitepapers/WPCT/WPCT-F.07.02.sas:90-93].
    - 07.07 passes ``(("DIABP", (815, 817)),)``, the WHERE ``paramcd in ('DIABP') and atptn in
      (815 817)`` [whitepapers/WPCT/WPCT-F.07.07.sas:146-148].

    The ``PARAMCD`` comparison ignores trailing blanks (D-11). A missing ``ATPTN`` fails ``IN``
    (D-13). A pair with no data contributes no records, and no pairs select no records. Every input
    variable is kept, in input order.

    Raises ``KeyError`` when ``PARAMCD`` or ``ATPTN`` is absent and ``TypeError`` for a malformed
    pair or a non-numeric ``ATPTN``.
    """
    caller = "subset_param_timepoints"
    pairs = _timepoint_pairs(param_timepoints)
    paramcd = _char_values(df[_column(df, "PARAMCD", caller)], caller)
    atptn = df[_column(df, "ATPTN", caller)]
    _require_numeric(atptn, caller)
    keep = np.zeros(len(df), dtype=bool)
    for code, timepoints in pairs:
        keep |= (paramcd == code) & atptn.isin(timepoints).to_numpy(dtype=bool)
    return df.loc[keep].reset_index(drop=True)


def abbreviate_treatment(
    df: pd.DataFrame, source: str = "TRTP", target: str = "trtp_short", length: int = 6
) -> pd.DataFrame:
    """Add the abbreviated planned treatment (S02-TRN-02, S07-TRN-02).

    Ports ``attrib trtp_short length=$6`` and ``select (trtp)``
    [whitepapers/WPCT/WPCT-F.07.02.sas:95-102; whitepapers/WPCT/WPCT-F.07.07.sas:150-157]:
    ``Placebo`` gives ``P``, ``Xanomeline High Dose`` gives ``X-high`` and ``Xanomeline Low Dose``
    gives ``X-low``. Any other value, a missing one included, takes OTHERWISE, ``UNEXPECTED``, which
    the ``$6`` length stores as ``UNEXPE`` (D-12). WHEN ignores trailing blanks (D-11).

    The new variable follows every input variable, giving the 45 variables of ``ADVS_SUB``. A
    ``target`` the frame already holds keeps its position and is overwritten, as a variable read by
    SET keeps its place in the program data vector.

    Raises ``KeyError`` when ``source`` is absent, ``TypeError`` for a non-character ``source``
    value and ``ValueError`` for a ``length`` outside 1 to 32767.
    """
    caller = "abbreviate_treatment"
    stored = {value: assign_char(short, length) for value, short in TREATMENT_ABBREVIATIONS.items()}
    otherwise = assign_char(UNEXPECTED_TREATMENT, length)
    values = _char_values(df[_column(df, source, caller)], caller)
    abbreviated = pd.Series(
        [stored.get(value, otherwise) for value in values], index=df.index, dtype=object
    )
    out = df.copy()
    out[_find_column(df, target, caller) or target] = abbreviated
    return out.reset_index(drop=True)


def flag_filter(df: pd.DataFrame, flags: "str | Iterable[str]") -> pd.DataFrame:
    """Keep the records whose every flag variable is ``'Y'`` (S02-POP-01, S02-FLT-02, S07-POP-01,
    S07-FLT-03).

    - 07.02: ``where saffl = 'Y' and anl01fl = 'Y'`` [whitepapers/WPCT/WPCT-F.07.02.sas:164].
    - 07.07: ``where=(saffl = 'Y' and anl02fl = 'Y')`` [whitepapers/WPCT/WPCT-F.07.07.sas:213].

    ``flags`` is a sequence of variable names, or one blank-separated string. Trailing blanks are
    ignored, so ``'Y '`` matches, while ``'y'``, ``'N'`` and a missing ``''`` do not (D-11).

    Raises ``ValueError`` when no flag is named, ``KeyError`` naming every absent flag and
    ``TypeError`` for a non-character flag value.
    """
    caller = "flag_filter"
    names = _name_list(flags, caller)
    if not names:
        raise ValueError(f"{caller}: no flag variables named")
    keep = np.ones(len(df), dtype=bool)
    for column in _columns(df, names, caller):
        keep &= _char_values(df[column], caller) == FLAG_YES
    return df.loc[keep].reset_index(drop=True)


def exclude_visit(
    df: pd.DataFrame, var: str = "AVISITN", value: "float | None" = 0
) -> pd.DataFrame:
    """Drop the records of one visit (S02-FLT-03).

    Ports ``where also avisitn ne &b_visn`` with ``b_visn=0``
    [whitepapers/WPCT/WPCT-F.07.02.sas:166-167]. A missing ``AVISITN`` is not equal to 0, so it is
    kept (D-13). ``value=None`` or NaN is the SAS missing value ``.``, so it drops the missing
    records instead.

    Raises ``KeyError`` when ``var`` is absent and ``TypeError`` for a non-numeric ``var`` or
    ``value``.
    """
    caller = "exclude_visit"
    series = df[_column(df, var, caller)]
    _require_numeric(series, caller)
    if value is not None and (isinstance(value, bool) or not isinstance(value, numbers.Real)):
        raise TypeError(f"{caller}: visit value must be a number or None, got {value!r}")
    if value is None or math.isnan(value):
        keep = series.notna().to_numpy(dtype=bool)
    else:
        keep = (series != value).to_numpy(dtype=bool)
    return df.loc[keep].reset_index(drop=True)


def keep_variables(df: pd.DataFrame, names: "str | Iterable[str]") -> pd.DataFrame:
    """Keep only the named variables, as the SET option ``keep=`` does (S02-TRN-03, S07-TRN-04).

    ``keep=&ana_variables`` [whitepapers/WPCT/WPCT-F.07.02.sas:144,163;
    whitepapers/WPCT/WPCT-F.07.07.sas:190,213] does not reorder: the kept variables stay in the
    input frame's column order, not the order of ``names``. With ``schemas.ANA_VARIABLES`` this
    gives the 14 ``css_anadata`` columns of 07.02 and the 12 ``css_safana`` columns of 07.07,
    ``STUDYID USUBJID SAFFL TRTPN PARAMCD PARAM ATPTN ATPT trtp_short BASE CHG ANL02FL``. Names
    match case-insensitively and the frame's spelling is kept. ``names`` is a sequence, or one
    blank-separated string.

    Raises ``ValueError`` when no variable is named and ``KeyError`` naming every absent variable.
    """
    caller = "keep_variables"
    wanted = _name_list(names, caller)
    if not wanted:
        raise ValueError(f"{caller}: no variables named")
    kept = set(_columns(df, wanted, caller))
    return df.loc[:, [column for column in df.columns if column in kept]].reset_index(drop=True)


def post_baseline_filter(
    df: pd.DataFrame, var: str = "AVISIT", prefix: str = "Post-baseline", length: int = 20
) -> pd.DataFrame:
    """Keep the post-baseline records of the derivation output (S07-FLT-02).

    Ports ``where avisit =: 'Post-baseline'`` [whitepapers/WPCT/WPCT-F.07.07.sas:168-171], the SAS
    prefix comparison (D-11). ``length`` is the declared length of the derived ``AVISIT``, 20
    (LMM-TRN-07). The ``Baseline (MIN)`` records and any ``AVISIT`` not starting with the exact,
    case-sensitive prefix are dropped.

    Raises ``KeyError`` when ``var`` is absent and ``TypeError`` for a non-character value.
    """
    caller = "post_baseline_filter"
    keep = prefix_match(df[_column(df, var, caller)], prefix, length).to_numpy(dtype=bool)
    return df.loc[keep].reset_index(drop=True)


# --- Pooling and study numbering ------------------------------------------------------------------


def _pooled_usubjid(value: object) -> str:
    """``substr(usubjid,1,1) = 'P'``: replace the first character; a blank value becomes ``'P'``."""
    if is_char_missing(value):
        return "P"
    if not isinstance(value, str):
        raise TypeError(
            "stack_pooled: variable USUBJID must be character, "
            f"got {type(value).__name__} {value!r}"
        )
    return ("P" + value[1:]).rstrip(SAS_BLANK)


def stack_pooled(df: pd.DataFrame) -> pd.DataFrame:
    """Stack the records with a pooled copy of themselves (S07-TRN-05).

    Ports DATA ``css_anadata``: ``set css_safana css_safana (in=in_pool)``, where the copy gets
    ``studyid = 'A0'x !! 'Pooled'`` and ``substr(usubjid,1,1) = 'P'``
    [whitepapers/WPCT/WPCT-F.07.07.sas:222-230]. The input records come first, then the pooled copy,
    each in input order; on the real data 498 records become 996. The pooled ``STUDYID`` is
    ``schemas.POOLED_STUDYID``, ``'\\u00a0Pooled'``, which sorts after every ASCII study identifier
    (D-16). The pooled ``USUBJID`` keeps every character but the first, so ``01-701-1015`` becomes
    ``P1-701-1015``. Every other variable is copied unchanged.

    Raises ``KeyError`` when ``STUDYID`` or ``USUBJID`` is absent and ``TypeError`` for a
    non-character ``USUBJID`` value.
    """
    caller = "stack_pooled"
    studyid = _column(df, "STUDYID", caller)
    usubjid = _column(df, "USUBJID", caller)
    pooled = df.copy()
    pooled[studyid] = pd.Series([POOLED_STUDYID] * len(df), index=df.index, dtype=object)
    pooled[usubjid] = pd.Series(
        [_pooled_usubjid(value) for value in df[usubjid].to_numpy(dtype=object)],
        index=df.index,
        dtype=object,
    )
    return pd.concat([df, pooled], ignore_index=True)


def assign_studynum(
    df: pd.DataFrame,
    by: "str | Iterable[str]" = ("STUDYID", "TRTPN"),
    name: str = "studynum",
) -> pd.DataFrame:
    """Sort by study and treatment and number the studies (S07-TRN-06).

    Ports ``proc sort; by studyid trtpn;`` and the DATA step ``by studyid trtpn; if first.studyid
    then studynum+1;`` [whitepapers/WPCT/WPCT-F.07.07.sas:233-241]. The sort is
    ``sas_semantics.sas_sort``: stable, with missing values first (D-04, D-14). The sum statement
    retains ``studynum`` from 0, so the studies are numbered 1, 2, ... in sort order of the first BY
    variable, and the pooled study, which sorts last (D-16), gets the highest number: on the real
    data ``CDISCPILOT01`` is 1, ``CDISCPILOT02`` is 2 and the pooled study 3. ``studynum`` is int64
    and follows every input variable, the 13th variable of ``css_anadata``.

    Raises ``ValueError`` when ``by`` is empty or the frame already holds ``name``, and ``KeyError``
    naming every absent BY variable.
    """
    caller = "assign_studynum"
    keys = _columns(df, _name_list(by, caller), caller)
    if not keys:
        raise ValueError(f"{caller}: no BY variables named")
    # A studynum read by SET would be overwritten on every record before the sum statement adds
    # to it, which is not a study number, so an existing variable is refused rather than reused.
    if _find_column(df, name, caller) is not None:
        raise ValueError(f"{caller}: the frame already holds a variable {name}")
    out = sas_sort(df, keys)
    out[name] = np.cumsum(_first_flags(out[keys[0]], caller), dtype=np.int64)
    return out


# --- Halting checks -------------------------------------------------------------------------------


def assert_var_nonmissing(df: pd.DataFrame, var: str, dataset_name: str, program: str) -> None:
    """Halt when ``var`` has a missing value (S07-FLT-04).

    Ports ``%assert_var_nonmissing(css_safana, &c_var)`` followed by ``%assert_continue``
    [whitepapers/WPCT/WPCT-F.07.07.sas:219-220]. The macro opens the dataset with
    ``where=(missing(&var))`` and fails when any record remains
    [whitepapers/utilities/assert_var_nonmissing.sas:43-59]; ``%assert_continue`` then forces
    syntax-checking mode, so nothing further is produced
    [whitepapers/utilities/assert_continue.sas:22-39].
    A numeric NaN and a blank character value are missing (D-11).

    Raises ``AnalysisHalted`` with ``check="assert_var_nonmissing"`` when ``k > 0`` values are
    missing, with the message ``"<program>: assert_var_nonmissing failed: <k> missing <VAR> in
    <DATASET>"``, for example ``"WPCT-F.07.07: assert_var_nonmissing failed: 2 missing CHG in
    CSS_SAFANA"``. It also halts when ``var`` is absent, as the macro's ``%assert_var_exist`` check
    fails then. Returns None when every value is present, an empty frame included.
    """
    check = "assert_var_nonmissing"
    variable, dataset = var.upper(), dataset_name.upper()
    column = _find_column(df, var, check)
    if column is None:
        raise AnalysisHalted(
            f"{program}: {check} failed: variable {variable} does not exist in {dataset}",
            program_id=program,
            check=check,
        )
    missing = int(char_missing_mask(df[column]).sum())
    if missing > 0:
        raise AnalysisHalted(
            f"{program}: {check} failed: {missing} missing {variable} in {dataset}",
            program_id=program,
            check=check,
        )


def _duplicate_records(df: pd.DataFrame, columns: list[str], caller: str) -> int:
    """Return the record count of ``group by <keys> having count(<last key>) > 1``.

    The SELECT lists no summary function, so PROC SQL remerges the HAVING result with the data and
    returns every record of each failing group: this count is the macro's ``SQLOBS``. GROUP BY puts
    missing values in one group and ignores trailing blanks, and ``count`` counts non-missing values
    only (D-13).
    """
    if df.empty:
        return 0
    groups = pd.DataFrame(
        {
            f"key{position}": _group_values(df[column], caller)
            for position, column in enumerate(columns)
        }
    )
    key_names = list(groups.columns)
    groups["counted"] = (~char_missing_mask(df[columns[-1]])).to_numpy(dtype=np.int64)
    counts = groups.groupby(key_names, dropna=False, sort=False)["counted"].transform("sum")
    return int((counts > 1).sum())


def assert_unique_keys(
    df: pd.DataFrame, keys: "str | Iterable[str]", dataset_name: str, program: str
) -> None:
    """Halt when two or more records share the record key (S07-FLT-05).

    Ports ``%assert_unique_keys(css_anadata, studyid usubjid paramcd atptn)`` followed by
    ``%assert_continue`` [whitepapers/WPCT/WPCT-F.07.07.sas:244-245]. The macro runs ``group by
    <keys> having count(<last key>) > 1`` [whitepapers/utilities/assert_unique_keys.sas:84-101] and
    fails when the query returns any record; a duplicated group whose last key is missing is not
    detected, because ``count`` skips missing values. On the real data the check passes
    [whitepapers/WPCT/outputs_sas/WPCT-F.07.07.log:459]. ``keys`` is a sequence, or one
    blank-separated string.

    Raises ``AnalysisHalted`` with ``check="assert_unique_keys"`` when ``n > 0`` records fall in
    duplicated key groups, with the message ``"<program>: assert_unique_keys failed: <n> records in
    <DATASET> share keys <KEY1 KEY2 ...>"``. It also halts when no key is named or a key variable
    is absent, as the macro fails then too [whitepapers/utilities/assert_unique_keys.sas:51-67].
    Returns None when the keys are unique, an empty frame included.
    """
    check = "assert_unique_keys"
    names = _name_list(keys, check)
    dataset = dataset_name.upper()
    if not names:
        raise AnalysisHalted(
            f"{program}: {check} failed: no key variables named for {dataset}",
            program_id=program,
            check=check,
        )
    columns = [_find_column(df, name, check) for name in names]
    absent = [name.upper() for name, column in zip(names, columns, strict=True) if column is None]
    if absent:
        raise AnalysisHalted(
            f"{program}: {check} failed: key variables {' '.join(absent)} "
            f"do not exist in {dataset}",
            program_id=program,
            check=check,
        )
    duplicates = _duplicate_records(df, columns, check)
    if duplicates > 0:
        raise AnalysisHalted(
            f"{program}: {check} failed: {duplicates} records in {dataset} share keys "
            f"{' '.join(name.upper() for name in names)}",
            program_id=program,
            check=check,
        )
