"""SAS sort, missing-value and character helpers shared by every module of the port.

Each helper encodes a SAS behaviour where the pandas or numpy default differs, as recorded in Part C
of ``docs/sas_specification.md``. Consumers import these helpers and never re-implement them.

- D-04, D-14: :func:`sas_sort` gives PROC SORT order, with missing values lowest and stable
  (``EQUALS``) ties.
- D-11: :func:`is_num_missing`, :func:`is_char_missing`, :func:`char_missing_mask` and
  :func:`prefix_match`. A blank character value is missing, and comparisons ignore trailing blanks.
- D-12: :func:`assign_char` truncates on assignment to a fixed-length character variable.
- D-16: character values order by code point, so the pooled ``'\\u00a0Pooled'`` follows ASCII IDs.
- D-07: :func:`format_level` gives CLASS level labels such as ``TRTPN 54``.

D-13 needs no helper. SAS ``AVISITN ne 0`` is true for a missing value, and so is pandas
``NaN != 0``. SAS ``ATPTN in (815)`` is false for a missing value, and so is ``Series.isin`` for
NaN. The selection steps use those pandas expressions directly (S02-FLT-01, S02-FLT-03, S07-FLT-01).
"""

import math
import numbers

import numpy as np
import pandas as pd

# SAS stores character values blank-padded to their declared length, of at most 32767 characters.
SAS_BLANK = " "
SAS_MAX_CHAR_LENGTH = 32767


def is_num_missing(value: object) -> bool:
    """Return True when ``value`` is a SAS numeric missing value (D-11).

    ``None``, NaN, ``pd.NA`` and ``pd.NaT`` are missing. ``pyreadstat`` reads ``.``, ``._`` and the
    special missing values ``.A``-``.Z`` from a transport file as NaN, so all of them are one
    missing value here.
    """
    if value is None or value is pd.NA or value is pd.NaT:
        return True
    if isinstance(value, numbers.Integral):
        return False
    if isinstance(value, numbers.Real):
        return math.isnan(value)
    return False


def is_char_missing(value: object) -> bool:
    """Return True when ``value`` is a SAS character missing value (D-11).

    SAS stores a missing character value as blanks, and ``pyreadstat`` strips trailing blanks so it
    reads back as ``''``. Any all-blank string is therefore missing, as are ``None`` and NaN, which
    pandas uses for missing entries of object columns. Only the SAS blank ``' '`` counts: a string
    holding other whitespace is a value.
    """
    if isinstance(value, str):
        return value.strip(SAS_BLANK) == ""
    return is_num_missing(value)


def char_missing_mask(series: pd.Series) -> pd.Series:
    """Vectorised :func:`is_char_missing`: a boolean Series aligned with ``series`` (D-11)."""
    missing = series.isna().to_numpy(dtype=bool)
    if not _is_numeric_series(series):
        values = series.to_numpy(dtype=object)
        blank = np.fromiter(
            (isinstance(v, str) and v.strip(SAS_BLANK) == "" for v in values),
            dtype=bool,
            count=len(values),
        )
        missing = missing | blank
    return pd.Series(missing, index=series.index, name=series.name, dtype=bool)


def sas_sort(df: pd.DataFrame, by: "str | list[str] | tuple[str, ...]") -> pd.DataFrame:
    """Return ``df`` sorted as PROC SORT ``BY`` would sort it (D-04, D-14, D-16).

    - Every key in ``by`` is ascending, in ``by`` order. A single string names one key.
    - Missing values are lowest, and all missing values of a key tie (D-04). For a numeric key that
      means NaN, ``None`` and ``pd.NA``; for a character key, blank strings as well.
    - Character values compare blank-padded to a common length, so trailing blanks are ignored
      (D-11), and by code point. A Latin-1 byte equals its code point and UTF-8 byte order matches
      code-point order, so this is the order SAS gives the bytes: the pooled ``'\\u00a0Pooled'``
      (code point 160) sorts after ``CDISCPILOT02`` (D-16).
    - The sort is stable, like SAS's ``EQUALS``: rows equal on every key keep their input order
      (D-14). NODUPKEY-style "first row per key" steps and the SQL tie rule (D-15, sorting on an
      extra input-order column) rely on this.

    The returned frame is new, with a fresh ``RangeIndex``, and the values are unchanged: blanks are
    normalised only inside the sort keys. An empty ``by`` returns a copy in input order.

    pandas drops NaN keys from ``groupby`` by default, whereas SAS keeps a missing BY value as its
    own BY group. Callers therefore group the result with
    ``groupby(list(by), dropna=False, sort=False)``, which keeps that group and this order.

    Raises ``KeyError`` for a BY variable absent from ``df``, and ``TypeError`` for one that names
    several columns or whose values are neither all character nor all numeric.
    """
    keys = [by] if isinstance(by, str) else list(by)
    absent = [key for key in keys if key not in df.columns]
    if absent:
        raise KeyError(f"sas_sort: BY variables not in the frame: {', '.join(map(str, absent))}")

    # np.lexsort treats its last array as the primary key, so the keys are pushed in reverse.
    # Each key contributes a presence flag (0 for missing, so missing sorts lowest) and its values.
    lexsort_keys: list[np.ndarray] = []
    for key in reversed(keys):
        column = df[key]
        if isinstance(column, pd.DataFrame):
            raise TypeError(f"sas_sort: BY variable {key!r} names more than one column")
        present, values = _sort_key(column, key)
        lexsort_keys.append(values)
        lexsort_keys.append(present)

    # np.lexsort is a stable sort, which gives the EQUALS behaviour.
    order = np.lexsort(lexsort_keys) if lexsort_keys else np.arange(len(df))
    return df.take(order).reset_index(drop=True)


def _is_numeric_series(series: pd.Series) -> bool:
    return pd.api.types.is_bool_dtype(series.dtype) or pd.api.types.is_numeric_dtype(series.dtype)


def _sort_key(column: pd.Series, name: object) -> "tuple[np.ndarray, np.ndarray]":
    """Return the (presence flag, ordering value) arrays of one BY variable."""
    if _is_numeric_series(column):
        return _numeric_key(column.to_numpy(dtype="float64", na_value=np.nan))

    values = column.to_numpy(dtype=object)
    missing = np.asarray(pd.isna(values), dtype=bool)
    present = values[~missing]
    if all(isinstance(v, str) for v in present):
        return _char_key(values, missing)
    if all(isinstance(v, numbers.Real) for v in present):
        numeric = np.array(
            [np.nan if m else float(v) for v, m in zip(values, missing, strict=True)],
            dtype="float64",
        )
        return _numeric_key(numeric)
    raise TypeError(
        f"sas_sort: BY variable {name!r} holds values that are neither all character "
        "nor all numeric"
    )


def _numeric_key(values: np.ndarray) -> "tuple[np.ndarray, np.ndarray]":
    missing = np.isnan(values)
    return (~missing).astype(np.int8), np.where(missing, 0.0, values)


def _char_key(values: np.ndarray, missing: np.ndarray) -> "tuple[np.ndarray, np.ndarray]":
    stripped = ["" if m else v.rstrip(SAS_BLANK) for v, m in zip(values, missing, strict=True)]
    missing = missing | np.fromiter((s == "" for s in stripped), dtype=bool, count=len(stripped))
    width = max(map(len, stripped), default=0)
    padded = [s.ljust(width, SAS_BLANK) for s in stripped]
    # Python string comparison is by code point; ranks turn it into an integer key for lexsort.
    rank = {text: position for position, text in enumerate(sorted(set(padded)))}
    codes = np.fromiter((rank[text] for text in padded), dtype=np.int64, count=len(padded))
    codes[missing] = 0
    return (~missing).astype(np.int8), codes


def _check_length(length: object, caller: str) -> int:
    if (
        isinstance(length, bool)
        or not isinstance(length, numbers.Integral)
        or not 1 <= length <= SAS_MAX_CHAR_LENGTH
    ):
        raise ValueError(
            f"{caller}: length must be an integer from 1 to {SAS_MAX_CHAR_LENGTH}, got {length!r}"
        )
    return int(length)


def assign_char(value: object, length: int) -> str:
    """Return ``value`` as stored in a ``$length`` character variable (S02-TRN-02, D-12, D-11).

    SAS truncates a longer value on assignment, so ``assign_char("UNEXPECTED", 6)`` is ``"UNEXPE"``.
    The stored value reads back without its trailing blanks, and a missing value (``None``, NaN or
    blanks) becomes ``''``. ``length`` counts characters, which equal bytes in a Latin-1 session.

    Raises ``ValueError`` for a length outside 1 to 32767 and ``TypeError`` for a non-missing value
    that is not a string.
    """
    width = _check_length(length, "assign_char")
    if is_char_missing(value):
        return ""
    if not isinstance(value, str):
        raise TypeError(f"assign_char: expected a character value, got {type(value).__name__}")
    return value[:width].rstrip(SAS_BLANK)


def format_level(value: object) -> str:
    """Return the label of an unformatted numeric CLASS level, as PROC GLM prints it (D-07).

    Integral values print without decimals (``54.0`` gives ``"54"``, ``-0.0`` gives ``"0"``), which
    is what BEST12. shows for the TRTPN levels in the ``SOLUTION`` table, as in ``TRTPN 54``. A
    missing value gives ``"."``.

    A non-integral value takes Python's shortest round-trip form, not BEST12.'s rounding to 12
    characters, so ``float(format_level(x)) == x`` and the drivers can turn a ``LEVEL`` label back
    into its TRTPN value for the p-value merge (S02-TRN-05, S07-TRN-08).

    Raises ``TypeError`` for a non-numeric value.
    """
    if is_num_missing(value):
        return "."
    if isinstance(value, numbers.Integral):
        return str(int(value))
    if not isinstance(value, numbers.Real):
        raise TypeError(f"format_level: expected a numeric level, got {type(value).__name__}")
    number = float(value)
    if number.is_integer():
        return str(int(number))
    return repr(number)


def prefix_match(series: pd.Series, prefix: str, length: "int | None" = None) -> pd.Series:
    """Vectorised SAS ``=:`` prefix comparison of a character variable with a literal (D-11).

    S07-FLT-02 keeps ``AVISIT =: 'Post-baseline'`` [whitepapers/WPCT/WPCT-F.07.07.sas:170]. SAS
    truncates the longer operand to the length of the shorter one and compares the rest
    blank-padded. A variable's operand length is its declared ``length``, not the length of its
    trimmed value, so a value shorter than the prefix is padded with blanks rather than compared on
    its own length.

    - With ``length``, the first ``min(length, len(prefix))`` characters of each blank-padded value
      are compared with the same slice of ``prefix``.
    - With ``length=None``, the declared length is taken to be at least ``len(prefix)``. That is
      ``value.startswith(prefix)``, except that trailing blanks in ``prefix`` match blank padding.

    A missing value is all blanks, so it matches only an all-blank ``prefix``. A SAS character
    literal holds at least one character, so an empty ``prefix`` is the blank literal ``' '``. The
    derived ``AVISIT`` has length 20 (LMM-TRN-07), at least the 13 characters of
    ``'Post-baseline'``, so both forms agree on the repository data.

    Raises ``TypeError`` for a non-string ``prefix`` or a non-missing value that is not a string,
    and ``ValueError`` for a ``length`` outside 1 to 32767.
    """
    if not isinstance(prefix, str):
        raise TypeError(f"prefix_match: prefix must be a string, got {type(prefix).__name__}")
    literal = prefix or SAS_BLANK
    width = len(literal)
    if length is not None:
        width = min(_check_length(length, "prefix_match"), width)
    target = literal[:width]

    def matches(value: object) -> bool:
        if is_char_missing(value):
            text = ""
        elif isinstance(value, str):
            text = value
        else:
            raise TypeError(
                f"prefix_match: expected character values, got {type(value).__name__} {value!r}"
            )
        return text[:width].ljust(width, SAS_BLANK) == target

    values = series.to_numpy(dtype=object)
    result = np.fromiter((matches(v) for v in values), dtype=bool, count=len(values))
    return pd.Series(result, index=series.index, name=series.name, dtype=bool)
