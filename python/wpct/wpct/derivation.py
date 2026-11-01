"""Port of the ADaM macro ``%derive_lastminmax_measure`` (S07-TRN-03, items LMM-*).

Source: ``whitepapers/ADaM/derive_lastminmax_measure.sas``, macro defined at lines 77-213. 07.07
calls it with mode ``MIN``, ``flvars=anl02fl``, ``grpvars=studyid usubjid trtpn paramcd atptn``,
``ordvars=avisitn`` and ``incl=trtp_short saffl param atpt``
[whitepapers/WPCT/WPCT-F.07.07.sas:161-166].

Items realised (``python/wpct/docs/sas_specification.md``, Part B.3):

- LMM-FLT-01: delete a record lacking either date or ``AVAL``.
- LMM-TRN-01: ``ADT <= TRTSDT`` is baseline, every other record post-baseline.
- LMM-TRN-02: change modes ``LAST``, ``MIN`` and ``MAX``, with their flags.
- LMM-TRN-03: every record equal to its group's extremum (PROC SQL ``HAVING``), ties kept.
- LMM-TRN-04: the first record per group (PROC SORT ``NODUPKEY``).
- LMM-FLT-02: only groups with both a baseline and a post-baseline record survive.
- LMM-TRN-05, LMM-TRN-06: ``AVISIT``, ``AVISITN``, ``BASE`` and ``CHG`` of the derived records.
- LMM-TRN-07: the output structure and the mode flags.

D-15 and U-01: PROC SQL leaves the order of records equal on every ``ORDER BY`` key undefined, and
NODUPKEY keeps the first. The port sorts on an extra input-order column, so input order is the
stated tie rule.

SAS variable names are case-insensitive, so the argument lists match the frame's columns
case-insensitively and the output keeps the frame's spelling. The macro's closing
``%util_delete_dsets`` cleanup is not ported: Python holds no WORK library.
"""

import logging
from collections.abc import Iterable
from dataclasses import dataclass

import numpy as np
import pandas as pd

from wpct.sas_semantics import char_missing_mask, format_level, sas_sort

__all__ = ["LmmCounts", "LmmModeCounts", "derive_lastminmax_measure"]

logger = logging.getLogger("wpct.derivation")

_CALLER = "derive_lastminmax_measure"

#: Private input-position column behind the U-01 tie rule; never returned.
_ROW = "_ROW"

#: Change mode -> (SQL summary function, analysed variable)
#: [whitepapers/ADaM/derive_lastminmax_measure.sas:119-128].
_MODE_RULES = {"MIN": ("min", "AVAL"), "MAX": ("max", "AVAL"), "LAST": ("max", "ADT")}

#: ``AVISITN = 9900 + 10*idx + 1`` (baseline) or ``+ 2`` (post-baseline) (LMM-TRN-05, LMM-TRN-06).
_AVISITN_OFFSET = 9900

_INVALID_MODE = (
    "(DERIVE_LASTMINMAX_MEASURES) Invalid C_MODES value: %s. "
    "Defaulting to LAST (rather than MIN or MAX)."
)
_OMIT_NO_POST = (
    "(DERIVE_LASTMINMAX_MEASURES) Omitting obs without a post-baseline measure, such as %s"
)
_OMIT_NO_BASE = "(DERIVE_LASTMINMAX_MEASURES) Omitting obs without a baseline measure, such as %s"

#: The only variables an LMM-FLT-02 warning names, when they are among the GRPVARS: the subject and
#: the stratum of the omitted group. SAS ``PUT _ALL_`` also prints measurements, dates, treatment
#: and flags, which the port never logs.
_WARNING_CONTEXT = ("STUDYID", "USUBJID", "PARAMCD", "ATPTN")
_NO_WARNING_CONTEXT = "(no STUDYID, USUBJID, PARAMCD or ATPTN among GRPVARS)"


@dataclass(frozen=True)
class LmmModeCounts:
    """Record counts of one change mode, matching the observation counts in the SAS log.

    ``MODE`` is the mode processed (an invalid token is processed and reported as ``LAST``),
    ``FLAG`` its flag variable and ``IDX`` its 1-based position in ``c_modes`` (LMM-TRN-02).

    - ``SQL_BASE``, ``SQL_POST``: rows of ``LMM_BASE_ANL`` and ``LMM_POST_ANL`` after the PROC SQL
      ``HAVING`` step (LMM-TRN-03).
    - ``NODUPKEY_BASE``, ``NODUPKEY_POST``: rows left by PROC SORT ``NODUPKEY`` (LMM-TRN-04).
    - ``OMITTED_BASE``: baseline records dropped for lacking a post-baseline record, each logged as
      "Omitting obs without a post-baseline measure" (LMM-FLT-02).
    - ``OMITTED_POST``: post-baseline records dropped for lacking a baseline record, each logged as
      "Omitting obs without a baseline measure" (LMM-FLT-02).
    - ``MERGED_BASE``, ``MERGED_POST``: rows written by the two merge steps (LMM-TRN-05,
      LMM-TRN-06).
    """

    MODE: str
    FLAG: str
    IDX: int
    SQL_BASE: int = 0
    SQL_POST: int = 0
    NODUPKEY_BASE: int = 0
    NODUPKEY_POST: int = 0
    OMITTED_BASE: int = 0
    OMITTED_POST: int = 0
    MERGED_BASE: int = 0
    MERGED_POST: int = 0


@dataclass(frozen=True)
class LmmCounts:
    """Record counts of one derivation call.

    ``LMM_BASE`` and ``LMM_POST`` are the baseline and post-baseline records left by LMM-FLT-01 and
    split by LMM-TRN-01. ``DSOUT`` is the number of rows returned, and ``modes`` holds one
    :class:`LmmModeCounts` per change mode, in ``c_modes`` order.
    """

    LMM_BASE: int
    LMM_POST: int
    DSOUT: int
    modes: tuple[LmmModeCounts, ...] = ()


@dataclass(frozen=True)
class _Mode:
    """One resolved change mode (LMM-TRN-02); ``invalid`` is the upper-cased token it replaces."""

    name: str
    func: str
    variable: str
    flag: str
    idx: int
    invalid: "str | None" = None


@dataclass(frozen=True)
class _Layout:
    """Resolved column names of the derived records (LMM-TRN-07)."""

    aval: str
    adt: str
    avisit: str
    avisitn: str
    base: str
    chg: str
    columns: tuple[str, ...]
    flags: tuple[str, ...]

    @property
    def output_columns(self) -> list[str]:
        return [*self.columns, *self.flags]


def derive_lastminmax_measure(
    df: pd.DataFrame,
    c_modes: "str | list[str] | tuple[str, ...]",
    flvars: "str | list[str] | tuple[str, ...]" = (),
    grpvars: "str | list[str] | tuple[str, ...]" = (),
    ordvars: "str | list[str] | tuple[str, ...]" = (),
    incl: "str | list[str] | tuple[str, ...]" = (),
) -> "tuple[pd.DataFrame, LmmCounts]":
    """Return the derived baseline and post-baseline records of ``df`` and their record counts.

    Port of ``%derive_lastminmax_measure(ds, c_modes, flvars=, grpvars=, ordvars=, incl=)``
    [whitepapers/ADaM/derive_lastminmax_measure.sas:77-213], called by 07.07 as S07-TRN-03. Each
    variable list is a blank-delimited string, as in SAS, or a sequence of names.

    - ``c_modes``: change modes, upper-cased and processed in order with idx = 1, 2, ...
      (LMM-TRN-02). ``MIN`` and ``MAX`` take the minimum or maximum ``AVAL``, ``LAST`` the latest
      ``ADT``. Any other token is processed as ``LAST``, with a warning logged when its turn comes,
      after the warnings of the modes before it.
    - ``flvars``: flag variable per mode, by position and upper-cased; a mode without one gets
      ``ANL<mode>FL``.
    - ``grpvars``: required GROUP BY variables; each group yields one baseline and one
      post-baseline record.
    - ``ordvars``: ORDER BY variables after ``grpvars``, deciding which tied record survives.
    - ``incl``: further variables carried unchanged.

    Steps: LMM-FLT-01 deletes records with a missing ``ADT``, ``TRTSDT`` or ``AVAL``; LMM-TRN-01
    splits ``ADT <= TRTSDT`` (baseline) from the rest. Per mode, separately for both phases,
    LMM-TRN-03 keeps every record equal to its group's extremum, ordered by ``grpvars``, ``ordvars``
    and input position (D-15, U-01), and LMM-TRN-04 keeps the first record per group. LMM-FLT-02
    drops baseline records whose group has no post-baseline record, then post-baseline records
    whose group has no surviving baseline record, logging one warning per record that names only
    the group's ``STUDYID``, ``USUBJID``, ``PARAMCD`` and ``ATPTN`` among ``grpvars``, never the
    whole record that SAS ``PUT _ALL_`` prints. LMM-TRN-05 sets
    ``AVISIT='Baseline (<mode>)'``, ``AVISITN=9900+10*idx+1``, ``BASE=AVAL`` and ``CHG=0``;
    LMM-TRN-06 sets ``AVISIT='Post-baseline (<mode>)'``, ``AVISITN=9900+10*idx+2``, ``BASE`` from
    the group's baseline record and ``CHG=AVAL-BASE``.

    The result (LMM-TRN-07) holds each mode's baseline records, then its post-baseline records,
    both in ``grpvars`` order, after those of earlier modes. Its columns are ``AVISIT``; the
    ``grpvars``, ``ordvars`` and ``incl`` variables in the frame's column order (SAS ``KEEP=`` does
    not reorder); then ``AVAL ADT AVISITN BASE CHG``, each unless already present; then one flag per
    mode, ``'Y'`` on that mode's records and ``''`` on the others. Numeric columns are float64 and
    character columns hold ``''`` for missing. For 07.07 that is ``AVISIT STUDYID USUBJID SAFFL
    TRTPN PARAMCD PARAM ATPTN ATPT AVISITN trtp_short AVAL ADT BASE CHG ANL02FL``.

    ``AVISIT`` is never truncated (LMM-TRN-07). SAS declares it ``$20`` when the input ``AVISIT`` is
    shorter than 20 [whitepapers/ADaM/derive_lastminmax_measure.sas:97-106], as in the repository
    data (length 16), and then cuts no derived value either. For a longer input ``AVISIT`` SAS
    writes no LENGTH statement, so ``retain avisit ' '`` would leave it ``$1``; the port does not
    reproduce that legacy truncation.

    SAS behaviour for empty derivation input not established (U-03): AVLEN stays blank when
    ``%if &avlen < 20`` is evaluated [whitepapers/ADaM/derive_lastminmax_measure.sas:98-102]; this
    guard is a Python decision, not a proven SAS equivalent. When ``df`` has no rows, or LMM-FLT-01
    removes them all, the result is an empty frame with the columns above and every count is 0; an
    invalid mode still logs its warning.

    ``df`` is never modified. Raises ``ValueError`` for an empty ``c_modes`` or ``grpvars``, a name
    matching several columns, or a flag that would overwrite another output variable; ``KeyError``
    when ``df`` lacks ``AVAL``, ``ADT``, ``TRTSDT`` or a listed variable; and ``TypeError`` for a
    non-numeric ``AVAL``, ``ADT`` or ``TRTSDT`` (dates must be SAS day numbers, D-10).
    """
    if not isinstance(df, pd.DataFrame):
        raise TypeError(f"{_CALLER}: df must be a pandas DataFrame, got {type(df).__name__}")
    mode_tokens = _tokens(c_modes, "c_modes")
    if not mode_tokens:
        raise ValueError(
            f"{_CALLER}: c_modes must name at least one change mode (LAST, MIN or MAX)"
        )
    grp_tokens = _tokens(grpvars, "grpvars")
    if not grp_tokens:
        raise ValueError(f"{_CALLER}: grpvars must name at least one grouping variable")

    lookup = _column_lookup(df)
    missing: list[str] = []
    required = _resolve(lookup, ("AVAL", "ADT", "TRTSDT"), missing)
    grp = _resolve(lookup, grp_tokens, missing)
    ordered = _resolve(lookup, _tokens(ordvars, "ordvars"), missing)
    carried = _resolve(lookup, _tokens(incl, "incl"), missing)
    if missing:
        raise KeyError(f"{_CALLER}: input frame lacks required variables: {', '.join(missing)}")
    aval, adt, trtsdt = required
    for name in required:
        if not _is_numeric(df[name]):
            raise TypeError(
                f"{_CALLER}: {name} must be numeric (dates as SAS day numbers, D-10), "
                f"got dtype {df[name].dtype}"
            )

    selected = _unique([aval, adt, *grp, *ordered, *carried])
    if any(name.upper() == _ROW for name in selected):
        raise ValueError(f"{_CALLER}: variable name {_ROW} is reserved for the input-order column")
    modes = _resolve_modes(mode_tokens, _tokens(flvars, "flvars"))
    layout = _layout(df, selected, {*grp, *ordered, *carried}, aval, adt, modes)

    records = _normalised(df, selected)
    records[_ROW] = np.arange(len(records), dtype=np.int64)

    # LMM-FLT-01 [whitepapers/ADaM/derive_lastminmax_measure.sas:92] and LMM-TRN-01
    # [whitepapers/ADaM/derive_lastminmax_measure.sas:94-95].
    adt_days = records[adt].to_numpy()
    start_days = df[trtsdt].to_numpy(dtype="float64", na_value=np.nan)
    usable = ~(np.isnan(adt_days) | np.isnan(start_days) | np.isnan(records[aval].to_numpy()))
    baseline = usable & (adt_days <= start_days)
    post_baseline = usable & ~baseline

    if not usable.any():
        # U-03 guard: SAS behaviour for empty derivation input is not established. The macro's mode
        # loop runs whatever the data, so each invalid mode is still reported, in mode order.
        for mode in modes:
            _warn_invalid_mode(mode)
        empty_counts = tuple(LmmModeCounts(MODE=m.name, FLAG=m.flag, IDX=m.idx) for m in modes)
        return _empty_output(records, layout), LmmCounts(0, 0, 0, empty_counts)

    lmm_base = records.loc[baseline].reset_index(drop=True)
    lmm_post = records.loc[post_baseline].reset_index(drop=True)

    frames: list[pd.DataFrame] = []
    mode_counts: list[LmmModeCounts] = []
    for mode in modes:
        _warn_invalid_mode(mode)
        frame, counts = _derive_mode(lmm_base, lmm_post, mode, grp, ordered, layout)
        frames.append(frame)
        mode_counts.append(counts)

    non_empty = [frame for frame in frames if len(frame)]
    if non_empty:
        dsout = pd.concat(non_empty, ignore_index=True).loc[:, layout.output_columns]
    else:
        dsout = _empty_output(records, layout)
    return dsout, LmmCounts(
        LMM_BASE=len(lmm_base), LMM_POST=len(lmm_post), DSOUT=len(dsout), modes=tuple(mode_counts)
    )


def _derive_mode(
    lmm_base: pd.DataFrame,
    lmm_post: pd.DataFrame,
    mode: _Mode,
    grp: list[str],
    ordered: list[str],
    layout: _Layout,
) -> "tuple[pd.DataFrame, LmmModeCounts]":
    """Derive one mode's baseline then post-baseline records, with their flag columns set."""
    value = layout.aval if mode.variable == "AVAL" else layout.adt
    base_sql = _group_extremum(lmm_base, grp, ordered, value, mode.func)
    post_sql = _group_extremum(lmm_post, grp, ordered, value, mode.func)
    base_first = _first_per_group(base_sql, grp)
    post_first = _first_per_group(post_sql, grp)

    # LMM-FLT-02, LMM-TRN-05 [whitepapers/ADaM/derive_lastminmax_measure.sas:159-175]: the merge
    # BY grpvars keeps a baseline record only when its group has a post-baseline record.
    post_keys = set(_key_tuples(post_first, grp))
    base_key_list = _key_tuples(base_first, grp)
    has_post = np.fromiter(
        (key in post_keys for key in base_key_list), dtype=bool, count=len(base_first)
    )
    _warn_omitted(base_first.loc[~has_post], grp, _OMIT_NO_POST)
    base_rows = base_first.loc[has_post].reset_index(drop=True)
    base_rows[layout.avisit] = _constant(base_rows, f"Baseline ({mode.name})", object)
    base_rows[layout.avisitn] = _constant(base_rows, _AVISITN_OFFSET + 10 * mode.idx + 1, "float64")
    base_rows[layout.base] = base_rows[layout.aval].to_numpy(dtype="float64", copy=True)
    base_rows[layout.chg] = _constant(base_rows, 0.0, "float64")

    # LMM-FLT-02, LMM-TRN-06 [whitepapers/ADaM/derive_lastminmax_measure.sas:177-194]: the second
    # merge reads the updated baseline records, as SAS reads the rewritten LMM_BASE_ANL.
    base_by_key = dict(
        zip(_key_tuples(base_rows, grp), base_rows[layout.base].to_numpy(), strict=True)
    )
    post_key_list = _key_tuples(post_first, grp)
    has_base = np.fromiter(
        (key in base_by_key for key in post_key_list), dtype=bool, count=len(post_first)
    )
    _warn_omitted(post_first.loc[~has_base], grp, _OMIT_NO_BASE)
    post_rows = post_first.loc[has_base].reset_index(drop=True)
    matched = [base_by_key[key] for key, keep in zip(post_key_list, has_base, strict=True) if keep]
    post_rows[layout.base] = np.asarray(matched, dtype="float64")
    post_rows[layout.avisit] = _constant(post_rows, f"Post-baseline ({mode.name})", object)
    post_rows[layout.avisitn] = _constant(post_rows, _AVISITN_OFFSET + 10 * mode.idx + 2, "float64")
    post_rows[layout.chg] = post_rows[layout.aval] - post_rows[layout.base]

    # LMM-TRN-07 [whitepapers/ADaM/derive_lastminmax_measure.sas:197-206]: this mode's flag is 'Y'
    # on its own records; every other flag is character missing there.
    for rows in (base_rows, post_rows):
        for flag in layout.flags:
            rows[flag] = _constant(rows, "Y" if flag == mode.flag else "", object)
    frame = pd.concat(
        [rows for rows in (base_rows, post_rows) if len(rows)] or [base_rows], ignore_index=True
    )
    counts = LmmModeCounts(
        MODE=mode.name,
        FLAG=mode.flag,
        IDX=mode.idx,
        SQL_BASE=len(base_sql),
        SQL_POST=len(post_sql),
        NODUPKEY_BASE=len(base_first),
        NODUPKEY_POST=len(post_first),
        OMITTED_BASE=int((~has_post).sum()),
        OMITTED_POST=int((~has_base).sum()),
        MERGED_BASE=len(base_rows),
        MERGED_POST=len(post_rows),
    )
    return frame, counts


def _group_extremum(
    frame: pd.DataFrame, grp: list[str], ordered: list[str], value: str, func: str
) -> pd.DataFrame:
    """LMM-TRN-03: every record whose ``value`` equals its group's ``func``, in ORDER BY order.

    PROC SQL ``HAVING func(value) = value`` remerges the group statistic, so tied records all
    survive. They are ordered by ``grp``, ``ordered`` and input position (D-15, U-01). ``value`` is
    never missing here, because LMM-FLT-01 has removed such records.
    """
    if frame.empty:
        return frame.copy()
    codes = _group_codes(_key_tuples(frame, grp))
    extremum = frame[value].groupby(codes, sort=False).transform(func)
    hits = frame.loc[frame[value].to_numpy() == extremum.to_numpy()]
    return sas_sort(hits, _unique([*grp, *ordered, _ROW]))


def _first_per_group(frame: pd.DataFrame, grp: list[str]) -> pd.DataFrame:
    """LMM-TRN-04: PROC SORT ``NODUPKEY BY grp`` on a frame already in BY order (D-14)."""
    repeated = pd.Series(_key_tuples(frame, grp), dtype=object).duplicated(keep="first")
    return frame.loc[~repeated.to_numpy()].reset_index(drop=True)


def _key_tuples(frame: pd.DataFrame, names: list[str]) -> list[tuple]:
    """Return one BY key per row, equal where SAS treats the BY values as equal.

    Every missing value of a variable matches every other one, as in SAS GROUP BY and MERGE BY, and
    character values ignore trailing blanks (D-11).
    """
    columns: list[list[object]] = []
    for name in names:
        column = frame[name]
        if _is_numeric(column):
            numbers = column.to_numpy(dtype="float64", na_value=np.nan)
            columns.append([None if np.isnan(v) else float(v) for v in numbers])
        else:
            blank = char_missing_mask(column).to_numpy()
            columns.append(
                [
                    "" if is_blank else (v.rstrip(" ") if isinstance(v, str) else v)
                    for v, is_blank in zip(column.to_numpy(dtype=object), blank, strict=True)
                ]
            )
    return list(zip(*columns, strict=True))


def _group_codes(keys: list[tuple]) -> np.ndarray:
    """Number the distinct keys in order of first appearance."""
    codes, _ = pd.factorize(pd.Series(keys, dtype=object), sort=False)
    return codes.astype(np.int64, copy=False)


def _warn_omitted(omitted: pd.DataFrame, grp: list[str], message: str) -> None:
    """Log one LMM-FLT-02 warning per omitted record, naming only its group's identifiers.

    SAS ``PUT _ALL_`` prints every variable of the record
    [whitepapers/ADaM/derive_lastminmax_measure.sas:167,187]. The warning here names only the
    :data:`_WARNING_CONTEXT` variables among ``grp``, in ``grp`` order and in ``PUT name=value``
    style, such as ``STUDYID='CDISCPILOT01' USUBJID='01-703-1279' PARAMCD='DIABP' ATPTN=817``, so
    no measurement, date, treatment or flag value is logged. See :func:`_context_value`.
    """
    if omitted.empty:
        return
    names = [name for name in grp if name.upper() in _WARNING_CONTEXT]
    for values in omitted.loc[:, names].to_numpy(dtype=object):
        context = " ".join(
            f"{name}={_context_value(value)}" for name, value in zip(names, values, strict=True)
        )
        logger.warning(message, context or _NO_WARNING_CONTEXT)


def _context_value(value: object) -> str:
    """One value of an LMM-FLT-02 warning: text quoted by ``repr``, a number as a level label.

    ``repr`` escapes control characters, so a value holding a line break cannot forge a log line.
    A number reads as :func:`format_level` prints it, ``.`` when missing; any other value is quoted
    as text.
    """
    if isinstance(value, str):
        return repr(value)
    try:
        return format_level(value)
    except TypeError:
        return repr(str(value))


def _constant(frame: pd.DataFrame, value: object, dtype: object) -> pd.Series:
    """A column holding ``value`` on every row of ``frame``, typed even when ``frame`` is empty."""
    return pd.Series(value, index=frame.index, dtype=dtype)


def _empty_output(records: pd.DataFrame, layout: _Layout) -> pd.DataFrame:
    """An empty LMM-TRN-07 frame, with the dtypes a populated result would have."""
    empty = records.iloc[0:0].copy()
    for name in (layout.avisitn, layout.base, layout.chg):
        if name not in empty.columns:
            empty[name] = pd.Series(dtype="float64")
    for name in (layout.avisit, *layout.flags):
        if name not in empty.columns:
            empty[name] = pd.Series(dtype=object)
    return empty.loc[:, layout.output_columns].reset_index(drop=True)


def _resolve_modes(mode_tokens: tuple[str, ...], flag_tokens: tuple[str, ...]) -> list[_Mode]:
    """LMM-TRN-02 [whitepapers/ADaM/derive_lastminmax_measure.sas:81,113-130].

    An invalid token becomes ``LAST`` and is kept in ``invalid``; :func:`_warn_invalid_mode` logs
    it when the mode loop reaches it, not here.
    """
    modes: list[_Mode] = []
    for idx, token in enumerate(mode_tokens, start=1):
        name = token.upper()
        invalid = None
        if name not in _MODE_RULES:
            invalid, name = name, "LAST"
        func, variable = _MODE_RULES[name]
        flag = flag_tokens[idx - 1].upper() if idx <= len(flag_tokens) else f"ANL{name}FL"
        modes.append(
            _Mode(name=name, func=func, variable=variable, flag=flag, idx=idx, invalid=invalid)
        )
    return modes


def _warn_invalid_mode(mode: _Mode) -> None:
    """LMM-TRN-02: the macro's ``%put WARNING`` for an invalid token, as its loop reaches the mode.

    SAS issues it inside the ``%do %while`` pass of that mode
    [whitepapers/ADaM/derive_lastminmax_measure.sas:115-127], so it follows the warnings of the
    earlier modes and precedes those of its own.
    """
    if mode.invalid is not None:
        logger.warning(_INVALID_MODE, mode.invalid)


def _layout(
    df: pd.DataFrame,
    selected: list[str],
    kept_names: set[str],
    aval: str,
    adt: str,
    modes: list[_Mode],
) -> _Layout:
    """LMM-TRN-07 column order [whitepapers/ADaM/derive_lastminmax_measure.sas:105-110,197-206].

    The structure step gives ``AVISIT`` then the kept variables in input order; the appended
    records add ``AVAL ADT`` (the SQL select list), ``AVISITN`` when it is new, ``BASE CHG`` and
    the flag. A derived variable already among the selected ones keeps its place and spelling.
    """
    avisit = _named(selected, "AVISIT")
    avisitn = _named(selected, "AVISITN")
    base = _named(selected, "BASE")
    chg = _named(selected, "CHG")
    columns = [avisit, *(name for name in df.columns if name in kept_names and name != avisit)]
    for name in (aval, adt, avisitn, base, chg):
        if name not in columns:
            columns.append(name)

    # A flag may repeat across modes, as SAS then reuses the variable. A flag naming another
    # output variable is rejected rather than overwriting it on the derived records.
    flags = _unique(mode.flag for mode in modes)
    taken = {name.upper(): name for name in columns}
    for flag in flags:
        if flag in taken:
            raise ValueError(
                f"{_CALLER}: flag variable {flag} would overwrite the output variable {taken[flag]}"
            )
    return _Layout(
        aval=aval,
        adt=adt,
        avisit=avisit,
        avisitn=avisitn,
        base=base,
        chg=chg,
        columns=tuple(columns),
        flags=tuple(flags),
    )


def _named(columns: list[str], canonical: str) -> str:
    """The column of ``columns`` that is the SAS variable ``canonical``, else ``canonical``."""
    return next((name for name in columns if name.upper() == canonical), canonical)


def _normalised(df: pd.DataFrame, columns: list[str]) -> pd.DataFrame:
    """A new frame of ``columns`` with SAS storage types: numeric as float64, character with ''."""
    data: dict[str, np.ndarray] = {}
    for name in columns:
        column = df[name]
        if _is_numeric(column):
            data[name] = column.to_numpy(dtype="float64", na_value=np.nan, copy=True)
        elif _is_character(column):
            values = column.to_numpy(dtype=object, copy=True)
            values[char_missing_mask(column).to_numpy()] = ""
            data[name] = values
        else:
            data[name] = column.to_numpy(copy=True)
    return pd.DataFrame(data, index=pd.RangeIndex(len(df)), columns=columns)


def _is_numeric(column: pd.Series) -> bool:
    return pd.api.types.is_numeric_dtype(column.dtype)


def _is_character(column: pd.Series) -> bool:
    return pd.api.types.is_object_dtype(column.dtype) or isinstance(column.dtype, pd.StringDtype)


def _tokens(value: object, argument: str) -> tuple[str, ...]:
    """Split a SAS blank-delimited list given as one string or as a sequence of strings."""
    if value is None:
        return ()
    if isinstance(value, str):
        return tuple(value.split())
    try:
        items = list(value)
    except TypeError:
        raise TypeError(
            f"{_CALLER}: {argument} must be a string or a sequence of strings, "
            f"got {type(value).__name__}"
        ) from None
    tokens: list[str] = []
    for item in items:
        if not isinstance(item, str):
            raise TypeError(
                f"{_CALLER}: {argument} must hold names as strings, got {type(item).__name__}"
            )
        tokens.extend(item.split())
    return tuple(tokens)


def _column_lookup(df: pd.DataFrame) -> dict[str, list[str]]:
    """Map each upper-cased column name to the frame's columns of that name."""
    lookup: dict[str, list[str]] = {}
    for name in df.columns:
        if isinstance(name, str):
            lookup.setdefault(name.upper(), []).append(name)
    return lookup


def _resolve(
    lookup: dict[str, list[str]], tokens: tuple[str, ...], missing: list[str]
) -> list[str]:
    """Map SAS variable names to frame columns case-insensitively, recording absent names."""
    resolved: list[str] = []
    for token in tokens:
        matches = lookup.get(token.upper(), [])
        if not matches:
            if token not in missing:
                missing.append(token)
        elif len(matches) > 1:
            raise ValueError(
                f"{_CALLER}: variable {token} matches several columns: {', '.join(matches)}"
            )
        else:
            resolved.append(matches[0])
    return _unique(resolved)


def _unique(names: Iterable[str]) -> list[str]:
    """``names`` without repeats, in first-appearance order."""
    return list(dict.fromkeys(names))
