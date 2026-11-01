"""PROC SUMMARY port: the summary statistics of both programs (S02-MOD-01, S07-MOD-01).

Both programs compute the statistics with the same step, differing only in the BY variables:

- S02-MOD-01 [whitepapers/WPCT/WPCT-F.07.02.sas:258-263]:
  ``proc summary; by avisitn trtpn avisit trtp_short; var chg;``
- S07-MOD-01 [whitepapers/WPCT/WPCT-F.07.07.sas:329-334]:
  ``proc summary; by studyid trtpn studynum trtp_short; var chg;``

each with ``output out=css_stats (drop=_type_ _freq_) n= mean= std= median= min=datamin max=datamax
q1= q3=``. :func:`proc_summary` is the only implementation of these statistics; both drivers call it,
with BY lists ``("AVISITN", "TRTPN", "AVISIT", "trtp_short")`` (07.02) and
``("STUDYID", "TRTPN", "studynum", "trtp_short")`` (07.07).

The port follows the SAS defaults where the numpy and pandas defaults differ (Part C of
``docs/sas_specification.md``):

- D-01: quantiles use ``QNTLDEF=5`` (:func:`sas_quantile_def5`), not linear interpolation.
- D-02: ``VARDEF=DF``, so STD divides by n - 1 and is missing when n < 2.
- D-03: only the analysis variable is examined; a missing value leaves ``n`` and every statistic but
  not its BY group, and ``BASE`` is never consulted.
- D-04, D-14: BY groups follow the stable PROC SORT order with missing BY values lowest
  (:func:`wpct.sas_semantics.sas_sort`), and a missing BY value forms its own group.

Statistics stay at full precision: the display formats of ``util_value_format`` are excluded.
"""

import itertools
import math
from fractions import Fraction

import numpy as np
import pandas as pd

from wpct.sas_semantics import char_missing_mask, sas_sort
from wpct.schemas import STATISTICS

__all__ = ["proc_summary", "sas_quantile_def5"]

# Probabilities of the quantile statistics, exact so that definition 5 decides g = 0 without
# floating-point error (D-01).
_QUANTILE_PROBABILITIES = {
    "median": Fraction(1, 2),
    "q1": Fraction(1, 4),
    "q3": Fraction(3, 4),
}


def sas_quantile_def5(sorted_values: "object", p: Fraction) -> float:
    """Return the ``p`` quantile of ascending non-missing values under SAS ``QNTLDEF=5`` (D-01).

    PROC MEANS and PROC SUMMARY default to definition 5. For n sorted values x_1 ... x_n, let
    j = floor(np) and g = np - j; the quantile is (x_j + x_{j+1}) / 2 when g = 0 and x_{j+1}
    otherwise. np is computed as an exact ``Fraction``, so callers pass ``Fraction(1, 4)``,
    ``Fraction(1, 2)`` or ``Fraction(3, 4)``.

    The numpy and pandas default, ``linear`` interpolation (Hyndman-Fan type 7), differs: on the
    repository data the DIABP/815 Week 2 placebo Q3 is 6.0 under definition 5 and 5.5 under
    ``linear``. Definition 5 equals ``numpy.quantile(method="averaged_inverted_cdf")`` (type 2),
    which the tests use as a cross-check.

    An empty sequence gives NaN, as SAS gives a missing statistic for n = 0.

    Raises ``TypeError`` when ``p`` is not a ``Fraction``, and ``ValueError`` when ``p`` is outside
    the open interval (0, 1) or the values are not a one-dimensional, non-missing, ascending
    sequence.
    """
    if not isinstance(p, Fraction):
        raise TypeError(f"sas_quantile_def5: p must be a Fraction, got {type(p).__name__}")
    if not 0 < p < 1:
        raise ValueError(f"sas_quantile_def5: p must lie strictly between 0 and 1, got {p}")
    values = np.asarray(sorted_values, dtype="float64")
    if values.ndim != 1:
        raise ValueError(
            f"sas_quantile_def5: expected a one-dimensional sequence, got {values.ndim} dimensions"
        )
    n = values.size
    if n == 0:
        return math.nan
    if np.isnan(values).any():
        raise ValueError("sas_quantile_def5: values must be non-missing")
    if (np.diff(values) < 0).any():
        raise ValueError("sas_quantile_def5: values must be in ascending order")

    np_ = Fraction(n) * p
    j = math.floor(np_)
    # The text's 1-based x_j and x_{j+1} are values[j - 1] and values[j]. With 0 < p < 1, g = 0
    # implies 1 <= j <= n - 1, and g > 0 implies j <= n - 1, so both indices are in range.
    if np_ == j:
        return float((values[j - 1] + values[j]) / 2)
    return float(values[j])


def _group_statistics(values: np.ndarray) -> "dict[str, float | int]":
    """Return the statistics of one BY group from its ascending non-missing values (D-01, D-02)."""
    n = int(values.size)
    if n == 0:
        return {"n": 0, **{name: math.nan for name in STATISTICS if name != "n"}}
    mean = float(np.mean(values))
    # numpy's ddof=1 is SAS VARDEF=DF (divisor n - 1); STD stays missing below n = 2 (D-02).
    std = float(np.std(values, ddof=1)) if n >= 2 else math.nan
    statistics: dict[str, float | int] = {
        "n": n,
        "mean": mean,
        "std": std,
        "datamin": float(values[0]),
        "datamax": float(values[-1]),
    }
    for name, p in _QUANTILE_PROBABILITIES.items():
        statistics[name] = sas_quantile_def5(values, p)
    return statistics


def _group_identity(column: pd.Series) -> pd.Series:
    """Return the values that decide BY-group membership for one sorted BY variable (D-04).

    ``sas_sort`` ties every character missing value (``''``, blanks, NaN, None) and ignores
    trailing blanks (D-11). Grouping on the raw values would split such ties into separate groups,
    so character variables are grouped on their trimmed values with every missing value as ``''``.
    Numeric variables group as they are: pandas already treats NaN, None and ``pd.NA`` as one
    missing key when ``dropna=False``.
    """
    if pd.api.types.is_bool_dtype(column.dtype) or pd.api.types.is_numeric_dtype(column.dtype):
        return column
    missing = char_missing_mask(column).to_numpy(dtype=bool)
    values = column.to_numpy(dtype=object)
    if not all(isinstance(value, str) for value in values[~missing]):
        return column
    trimmed = ["" if absent else value.rstrip(" ") for value, absent in zip(values, missing)]
    return pd.Series(trimmed, index=column.index, dtype=object, name=column.name)


def proc_summary(
    df: pd.DataFrame,
    by: "str | list[str] | tuple[str, ...]",
    var: str = "CHG",
) -> pd.DataFrame:
    """Return the ``css_stats`` statistics of ``var`` per BY group (S02-MOD-01, S07-MOD-01).

    The frame is sorted with :func:`wpct.sas_semantics.sas_sort` (stable, missing BY values lowest;
    D-04, D-14) and split into BY groups with ``groupby(dropna=False, sort=False)``, so a record
    with a missing BY value still forms its own group. SAS sorts the data only by the leading BY
    variables (``AVISITN TRTPN``, ``STUDYID TRTPN``), while BY processing needs the records in
    ascending order of the full BY list. Sorting stably on every BY variable gives the same order
    whenever the records SAS sorted already follow the full BY order: the trailing BY variables are
    constant within the leading keys, as in the repository data, or already ascending within them.
    SAS stops the step with a not-sorted error, instead of producing statistics, only when the
    actual record sequence violates the full BY order.

    Within each group only ``var`` is examined (D-03): its missing values are dropped, ``BASE`` and
    every other column are ignored, and ``n`` counts the remaining values. The statistics are
    ``mean``, ``std`` (divisor n - 1, NaN when n < 2; D-02), ``median``, ``datamin``, ``datamax``,
    ``q1`` and ``q3`` (QNTLDEF=5; D-01). A group whose values are all missing keeps its row, with
    ``n = 0`` and every statistic NaN. Nothing is rounded.

    The result has one row per BY group in sorted order and a fresh ``RangeIndex``. Its columns are
    the BY variables, with their original names, dtypes and the values of each group's first row,
    followed by :data:`wpct.schemas.STATISTICS` (``n`` as int64, the rest float64): the 12 columns
    of the SAS ``css_stats`` before ``pval`` is merged in.

    Raises ``ValueError`` for an empty or repeated BY list or a BY variable named like a statistic,
    ``KeyError`` when ``var`` or a BY variable is absent, and ``TypeError`` when ``var`` is not a
    single numeric column.
    """
    keys = [by] if isinstance(by, str) else list(by)
    if not keys:
        raise ValueError("proc_summary: at least one BY variable is required")
    repeated = sorted({str(key) for key in keys if keys.count(key) > 1})
    if repeated:
        raise ValueError(f"proc_summary: BY variables repeated: {', '.join(repeated)}")
    clashing = [str(key) for key in keys if key in STATISTICS]
    if clashing:
        raise ValueError(
            f"proc_summary: BY variables named like output statistics: {', '.join(clashing)}"
        )
    if var not in df.columns:
        raise KeyError(f"proc_summary: analysis variable not in the frame: {var}")
    analysis = df[var]
    if isinstance(analysis, pd.DataFrame):
        raise TypeError(f"proc_summary: analysis variable {var!r} names more than one column")
    if pd.api.types.is_bool_dtype(analysis.dtype) or not pd.api.types.is_numeric_dtype(
        analysis.dtype
    ):
        raise TypeError(
            f"proc_summary: analysis variable {var!r} must be numeric, got {analysis.dtype}"
        )

    data = sas_sort(df, keys)
    codes = (
        data.groupby([_group_identity(data[key]) for key in keys], dropna=False, sort=False)
        .ngroup()
        .to_numpy()
    )
    # The data are sorted, so each BY group is one run of equal codes, as SAS BY processing reads
    # consecutive records. ``bounds`` holds each run's first position, then the row count.
    changes = np.ones(codes.size, dtype=bool)
    changes[1:] = codes[1:] != codes[:-1]
    starts = np.flatnonzero(changes)
    bounds = np.append(starts, codes.size)
    values = data[var].to_numpy(dtype="float64", na_value=np.nan)

    groups = []
    for start, end in itertools.pairwise(bounds):
        group_values = values[start:end]
        groups.append(_group_statistics(np.sort(group_values[~np.isnan(group_values)])))

    result = data[keys].iloc[starts].reset_index(drop=True)
    for name in STATISTICS:
        dtype = np.int64 if name == "n" else np.float64
        result[name] = np.array([group[name] for group in groups], dtype=dtype)
    return result
