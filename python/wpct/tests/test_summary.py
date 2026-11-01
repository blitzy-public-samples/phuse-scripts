"""Tests of the PROC SUMMARY port, ``wpct.summary`` (S02-MOD-01, S07-MOD-01).

Both programs compute their statistics with one PROC SUMMARY step
[whitepapers/WPCT/WPCT-F.07.02.sas:258-263; whitepapers/WPCT/WPCT-F.07.07.sas:329-334], which the
port reproduces with the SAS defaults where numpy and pandas differ (Part C of
``docs/sas_specification.md``): D-01 quantile definition 5, D-02 the n - 1 divisor, D-03 missing
analysis values, and D-04 and D-14 BY-group order. Expected values are hand-computed literals; the
only other quantile source is the ``numpy.quantile`` cross-check of D-01. Floating statistics are
judged by ``compare.check_value`` under the ``summary_statistics`` rule of ``tolerances.json``;
``n``, BY values and frame identities are exact. The frames are synthetic and nothing is written.
"""

import math
from collections.abc import Iterable
from fractions import Fraction

import numpy as np
import pandas as pd
import pytest

from wpct import compare, schemas
from wpct.summary import proc_summary, sas_quantile_def5

Q1, MEDIAN, Q3 = Fraction(1, 4), Fraction(1, 2), Fraction(3, 4)
QUANTILE_COLUMNS = {Q1: "q1", MEDIAN: "median", Q3: "q3"}

#: BY lists as the drivers pass them (S02-MOD-01, S07-MOD-01).
BY_07_02 = ("AVISITN", "TRTPN", "AVISIT", "trtp_short")
BY_07_07 = ("STUDYID", "TRTPN", "studynum", "trtp_short")

#: Every statistic except ``n``, the float64 columns of the result.
FLOAT_STATISTICS = schemas.STATISTICS[1:]
#: The ``tolerances.json`` class of the float statistics and of the def-5 quantiles.
SUMMARY_CLASS = "summary_statistics"

_TREATMENT_LABELS = {0.0: "P", 54.0: "X-low", 81.0: "X-high"}

#: (Q1, median, Q3) of 1..n under QNTLDEF=5, from j = floor(np) and g = np - j (D-01).
DEF5_QUARTILES = {
    1: (1.0, 1.0, 1.0),
    2: (1.0, 1.5, 2.0),
    3: (1.0, 2.0, 3.0),
    4: (1.5, 2.5, 3.5),
    5: (2.0, 3.0, 4.0),
    6: (2.0, 3.5, 5.0),
    7: (2.0, 4.0, 6.0),
    8: (2.5, 4.5, 6.5),
}


def _assert_agree(
    actual: "Iterable[object]", expected: "Iterable[object]", tolerances: compare.Tolerances
) -> None:
    """Assert equal lengths and each value agreeing with its expected value under the
    ``summary_statistics`` rule, whose ``missing`` rule lets a missing value match only another."""
    actual_values, expected_values = list(actual), list(expected)
    assert len(actual_values) == len(expected_values), (actual_values, expected_values)
    for position, (py, reference) in enumerate(zip(actual_values, expected_values)):
        outcome = compare.check_value(SUMMARY_CLASS, py, reference, tolerances)
        assert outcome.status == compare.PASS, (position, py, reference, outcome)


def _frame_07_02(rows: "list[tuple[float, float, float, float]]") -> pd.DataFrame:
    """A WPCT-F.07.02 ``css_nexttimept``-like frame from ``(AVISITN, TRTPN, BASE, CHG)`` rows.

    ``AVISIT`` and ``trtp_short`` follow ``AVISITN`` and ``TRTPN``, as the BY statement relies on;
    a missing ``AVISITN`` gets a blank ``AVISIT``.
    """
    frame = pd.DataFrame(rows, columns=["AVISITN", "TRTPN", "BASE", "CHG"], dtype="float64")
    visits = ["" if math.isnan(visit) else f"Week {visit:g}" for visit in frame["AVISITN"]]
    frame.insert(2, "AVISIT", visits)
    frame.insert(3, "trtp_short", [_TREATMENT_LABELS[trtpn] for trtpn in frame["TRTPN"]])
    return frame


def _assert_row(
    result: pd.DataFrame,
    position: int,
    tolerances: compare.Tolerances,
    n: int,
    **statistics: float,
) -> None:
    """Assert ``n`` exactly and every float statistic of one result row under the
    ``summary_statistics`` rule, NaN matching NaN through the ``missing`` rule."""
    assert result["n"].iat[position] == n
    assert sorted(statistics) == sorted(FLOAT_STATISTICS), statistics
    for name in FLOAT_STATISTICS:
        actual = float(result[name].iat[position])
        outcome = compare.check_value(SUMMARY_CLASS, actual, statistics[name], tolerances)
        assert outcome.status == compare.PASS, (name, position, actual, statistics[name], outcome)


# --- D-01: quantile definition 5 ------------------------------------------------------------------


@pytest.mark.parametrize(
    ("n", "expected"), sorted(DEF5_QUARTILES.items()), ids=[f"n{n}" for n in sorted(DEF5_QUARTILES)]
)
def test_d01_quantile_def5_n1_to_n8(
    n: int, expected: "tuple[float, float, float]", tolerances: compare.Tolerances
) -> None:
    """D-01: QNTLDEF=5 Q1, median and Q3 of 1..n for n = 1 to 8 (S02-MOD-01, S07-MOD-01).

    ``sas_quantile_def5`` receives the shuffled values sorted, and ``proc_summary`` receives them
    shuffled and sorts them itself. Each quartile also equals
    ``numpy.quantile(method="averaged_inverted_cdf")``, Hyndman-Fan type 2, which is definition 5.
    """
    shuffled = np.random.default_rng(n).permutation(np.arange(1, n + 1, dtype="float64"))
    values = np.sort(shuffled)

    actual = tuple(sas_quantile_def5(values, p) for p in (Q1, MEDIAN, Q3))
    _assert_agree(actual, expected, tolerances)
    numpy_type2 = tuple(
        float(np.quantile(values, float(p), method="averaged_inverted_cdf"))
        for p in (Q1, MEDIAN, Q3)
    )
    _assert_agree(actual, numpy_type2, tolerances)

    stats = proc_summary(pd.DataFrame({"TRTPN": 0.0, "CHG": shuffled}), "TRTPN")
    _assert_agree(
        (stats[QUANTILE_COLUMNS[p]].iat[0] for p in (Q1, MEDIAN, Q3)), expected, tolerances
    )


@pytest.mark.parametrize(
    ("n", "p", "def5", "linear"),
    [(4, Q1, 1.5, 1.75), (6, Q3, 5.0, 4.75)],
    ids=["n4-q1", "n6-q3"],
)
def test_d01_def5_differs_from_linear(
    n: int, p: Fraction, def5: float, linear: float, tolerances: compare.Tolerances
) -> None:
    """D-01: the port gives definition 5, not numpy's default ``linear`` (type 7) quantile.

    The two definitions disagree on these values beyond the ``summary_statistics`` rule, so
    agreement with ``def5`` from both ``sas_quantile_def5`` and ``proc_summary`` shows the numpy
    default is not used.
    """
    values = np.arange(1, n + 1, dtype="float64")
    numpy_default = float(np.quantile(values, float(p)))
    _assert_agree([numpy_default], [linear], tolerances)

    port = sas_quantile_def5(values, p)
    _assert_agree([port], [def5], tolerances)
    contrast = compare.check_value(SUMMARY_CLASS, port, numpy_default, tolerances)
    assert contrast.status == compare.FAIL, contrast
    stats = proc_summary(pd.DataFrame({"TRTPN": 0.0, "CHG": values}), "TRTPN")
    _assert_agree([stats[QUANTILE_COLUMNS[p]].iat[0]], [def5], tolerances)


@pytest.mark.parametrize(
    ("p", "expected"),
    [(Q1, 0.25), (MEDIAN, 0.45), (Q3, 0.65)],
    ids=["q1-x2-x3", "median-x4-x5", "q3-x6-x7"],
)
def test_d01_exact_fraction_boundary(
    p: Fraction, expected: float, tolerances: compare.Tolerances
) -> None:
    """D-01: with n * p an integer, g = 0 and the quantile averages x_np and x_(np+1).

    For n = 8 every quartile falls on that boundary. ``sas_quantile_def5`` decides g from the exact
    ``Fraction`` n * p alone, so values in 0.1 steps, which have no exact binary form, still give
    the two-value averages: (0.2 + 0.3) / 2, (0.4 + 0.5) / 2 and (0.6 + 0.7) / 2.
    """
    values = np.array([k / 10 for k in range(1, 9)])
    actual = sas_quantile_def5(values, p)
    _assert_agree([actual], [expected], tolerances)
    numpy_type2 = float(np.quantile(values, float(p), method="averaged_inverted_cdf"))
    _assert_agree([actual], [numpy_type2], tolerances)


@pytest.mark.parametrize(
    ("values", "p", "error"),
    [
        ([1.0, 2.0, 3.0, 4.0], 0.25, TypeError),
        ([1.0, 2.0, 3.0, 4.0], Fraction(0), ValueError),
        ([1.0, 2.0, 3.0, 4.0], Fraction(1), ValueError),
        ([[1.0, 2.0], [3.0, 4.0]], MEDIAN, ValueError),
        ([1.0, math.nan, 3.0], MEDIAN, ValueError),
        ([2.0, 1.0], MEDIAN, ValueError),
    ],
    ids=["float-p", "p-zero", "p-one", "two-dimensional", "missing-value", "unsorted"],
)
def test_d01_quantile_def5_rejects_invalid_arguments(
    values: "list[object]", p: object, error: "type[Exception]"
) -> None:
    """D-01: only an exact ``Fraction`` in (0, 1) and ascending non-missing values are accepted."""
    with pytest.raises(error, match="sas_quantile_def5"):
        sas_quantile_def5(values, p)


# --- S02-MOD-01, S07-MOD-01: proc_summary --------------------------------------------------------


def test_s02_mod_01_statistics_columns(tolerances: compare.Tolerances) -> None:
    """S02-MOD-01: the 12 ``css_stats`` columns and their values on the group ``[2, 4, 4, 5]``.

    The columns are the BY variables, then ``n mean std median datamin datamax q1 q3``. ``std`` is the sample standard deviation, sqrt(4.75 / 3) (D-02), and the quartiles follow
    definition 5 (D-01): Q1 = (2 + 4) / 2, median = (4 + 4) / 2, Q3 = (4 + 5) / 2.
    """
    frame = _frame_07_02([(2.0, 0.0, 80.0, chg) for chg in (4.0, 2.0, 5.0, 4.0)])
    result = proc_summary(frame, BY_07_02)

    assert list(result.columns) == [*BY_07_02, *schemas.STATISTICS]
    assert len(result.columns) == 12
    assert result["n"].dtype == np.int64
    assert all(result[name].dtype == np.float64 for name in FLOAT_STATISTICS)
    assert result.index.equals(pd.RangeIndex(1))
    assert result.loc[0, list(BY_07_02)].tolist() == [2.0, 0.0, "Week 2", "P"]
    _assert_row(
        result,
        0,
        tolerances,
        n=4,
        mean=3.75,
        std=1.2583057392117916,
        median=4.0,
        datamin=2.0,
        datamax=5.0,
        q1=3.0,
        q3=4.5,
    )


def test_d02_std_missing_for_n_below_2(tolerances: compare.Tolerances) -> None:
    """D-02: VARDEF=DF divides by n - 1, so STD is missing for a single value (S02-MOD-01).

    The single value is every other statistic; the two-value group shows STD defined from n = 2,
    sqrt(((1 - 2)^2 + (3 - 2)^2) / 1).
    """
    frame = _frame_07_02([(4.0, 0.0, 80.0, 7.5), (4.0, 54.0, 80.0, 3.0), (4.0, 54.0, 80.0, 1.0)])
    result = proc_summary(frame, BY_07_02)

    assert result["TRTPN"].tolist() == [0.0, 54.0]
    _assert_row(
        result,
        0,
        tolerances,
        n=1,
        mean=7.5,
        std=math.nan,
        median=7.5,
        datamin=7.5,
        datamax=7.5,
        q1=7.5,
        q3=7.5,
    )
    _assert_row(
        result,
        1,
        tolerances,
        n=2,
        mean=2.0,
        std=1.4142135623730951,
        median=2.0,
        datamin=1.0,
        datamax=3.0,
        q1=1.0,
        q3=3.0,
    )


def test_d03_all_missing_group(tolerances: compare.Tolerances) -> None:
    """D-03: a BY group whose ``CHG`` values are all missing keeps its row, n = 0 (S02-MOD-01).

    Every statistic of that row is missing, as is a quantile of an empty input.
    """
    frame = _frame_07_02(
        [(6.0, 0.0, 80.0, math.nan), (6.0, 54.0, 82.0, 1.0), (6.0, 0.0, 81.0, math.nan)]
    )
    result = proc_summary(frame, BY_07_02)

    assert result["TRTPN"].tolist() == [0.0, 54.0]
    _assert_row(result, 0, tolerances, n=0, **{name: math.nan for name in FLOAT_STATISTICS})
    _assert_row(
        result,
        1,
        tolerances,
        n=1,
        mean=1.0,
        std=math.nan,
        median=1.0,
        datamin=1.0,
        datamax=1.0,
        q1=1.0,
        q3=1.0,
    )
    assert all(math.isnan(sas_quantile_def5([], p)) for p in (Q1, MEDIAN, Q3))


def test_missing_chg_excluded(tolerances: compare.Tolerances) -> None:
    """D-03: a missing ``CHG`` is out of ``n`` and every statistic, not its BY group (S02-MOD-01).

    Each group's statistics equal those of the same group with its missing records removed, and
    every group stays in the output with its BY values. Placebo keeps [1, 3, 5]: Q1 is x_1 at
    np = 3/4 and Q3 is x_3 at np = 9/4 (D-01).
    """
    frame = _frame_07_02(
        [
            (8.0, 0.0, 80.0, 5.0),
            (8.0, 0.0, 81.0, math.nan),
            (8.0, 54.0, 82.0, math.nan),
            (8.0, 0.0, 83.0, 1.0),
            (8.0, 54.0, 84.0, 2.0),
            (8.0, 0.0, 85.0, math.nan),
            (8.0, 0.0, 86.0, 3.0),
        ]
    )
    result = proc_summary(frame, BY_07_02)

    assert result.loc[:, list(BY_07_02)].values.tolist() == [
        [8.0, 0.0, "Week 8", "P"],
        [8.0, 54.0, "Week 8", "X-low"],
    ]
    _assert_row(
        result,
        0,
        tolerances,
        n=3,
        mean=3.0,
        std=2.0,
        median=3.0,
        datamin=1.0,
        datamax=5.0,
        q1=1.0,
        q3=5.0,
    )
    _assert_row(
        result,
        1,
        tolerances,
        n=1,
        mean=2.0,
        std=math.nan,
        median=2.0,
        datamin=2.0,
        datamax=2.0,
        q1=2.0,
        q3=2.0,
    )
    complete = frame.loc[frame["CHG"].notna()]
    pd.testing.assert_frame_equal(result, proc_summary(complete, BY_07_02), check_exact=True)


def test_missing_base_ignored() -> None:
    """D-03: ``BASE`` is never consulted [whitepapers/WPCT/WPCT-F.07.02.sas:258-262] (S02-MOD-01).

    Setting ``BASE`` missing on some records, or dropping the column, leaves the output identical,
    and ``BASE`` is not an output column.
    """
    frame = _frame_07_02(
        [
            (2.0, 0.0, 80.0, -4.0),
            (2.0, 0.0, 78.0, 2.0),
            (2.0, 54.0, 90.0, 6.0),
            (99.0, 0.0, 80.0, -1.0),
            (99.0, 81.0, 70.0, 3.0),
            (99.0, 81.0, 75.0, 0.5),
        ]
    )
    expected = proc_summary(frame, BY_07_02)
    assert "BASE" not in expected.columns

    missing_base = frame.copy()
    missing_base.loc[[0, 2, 4], "BASE"] = math.nan
    pd.testing.assert_frame_equal(proc_summary(missing_base, BY_07_02), expected, check_exact=True)
    pd.testing.assert_frame_equal(
        proc_summary(frame.drop(columns="BASE"), BY_07_02), expected, check_exact=True
    )


def test_d04_missing_by_keys_sorted_first(tolerances: compare.Tolerances) -> None:
    """D-04: a missing ``AVISITN`` is its own BY group, ordered before the others (S02-MOD-01).

    The other groups ascend by the BY variables. pandas would drop the missing key from
    ``groupby`` and sort it last; SAS sorts it lowest.
    """
    frame = _frame_07_02(
        [
            (8.0, 54.0, 80.0, 1.0),
            (math.nan, 0.0, 80.0, 2.0),
            (2.0, 81.0, 80.0, 3.0),
            (2.0, 0.0, 80.0, 4.0),
            (math.nan, 0.0, 80.0, 5.0),
            (8.0, 0.0, 80.0, 6.0),
        ]
    )
    result = proc_summary(frame, BY_07_02)

    assert math.isnan(result["AVISITN"].iat[0])
    assert result["AVISITN"].iloc[1:].tolist() == [2.0, 2.0, 8.0, 8.0]
    assert result["TRTPN"].tolist() == [0.0, 0.0, 81.0, 0.0, 54.0]
    assert result["AVISIT"].tolist() == ["", "Week 2", "Week 2", "Week 8", "Week 8"]
    assert result["n"].tolist() == [2, 1, 1, 1, 1]
    _assert_agree(result["mean"], [3.5, 4.0, 3.0, 6.0, 1.0], tolerances)


@pytest.mark.parametrize(
    "labels",
    [("P ", "P"), ("P", "P ")],
    ids=["trailing-blank-first", "trailing-blank-second"],
)
def test_d14_tied_records_keep_input_order(
    labels: "tuple[str, str]", tolerances: compare.Tolerances
) -> None:
    """D-14: records tied on every BY variable keep their input order (S02-MOD-01).

    ``P`` and ``P `` are one ``trtp_short`` value in SAS, where trailing blanks are ignored (D-11),
    so they form one BY group. The group's BY values come from its first record, which the stable
    sort keeps first in input order.
    """
    frame = pd.DataFrame({"AVISITN": [2.0, 2.0], "trtp_short": list(labels), "CHG": [1.0, 3.0]})
    result = proc_summary(frame, ["AVISITN", "trtp_short"])

    assert result["trtp_short"].tolist() == [labels[0]]
    assert result["n"].tolist() == [2]
    _assert_agree(result["mean"], [2.0], tolerances)


def test_s07_mod_01_by_variables(tolerances: compare.Tolerances) -> None:
    """S07-MOD-01: BY ``STUDYID TRTPN studynum trtp_short``, as the WPCT-F.07.07 driver passes it.

    Groups ascend by study then treatment, and the pooled ``'\\u00a0Pooled'`` study (code point
    160) sorts after the ASCII study identifiers, so its groups come last (D-04, D-16). The BY
    variables keep their dtypes, ``studynum`` an integer.
    """
    pooled = "\u00a0Pooled"
    assert pooled == schemas.POOLED_STUDYID
    rows = [
        (pooled, 0.0, 3, "P", 3.0),
        ("CDISCPILOT02", 54.0, 2, "X-low", 2.0),
        ("CDISCPILOT01", 0.0, 1, "P", 3.0),
        (pooled, 54.0, 3, "X-low", 5.0),
        ("CDISCPILOT01", 54.0, 1, "X-low", 5.0),
        ("CDISCPILOT02", 0.0, 2, "P", 6.0),
        (pooled, 0.0, 3, "P", 6.0),
        (pooled, 54.0, 3, "X-low", 2.0),
    ]
    frame = pd.DataFrame(rows, columns=["STUDYID", "TRTPN", "studynum", "trtp_short", "CHG"])
    result = proc_summary(frame, BY_07_07)

    assert list(result.columns) == [*BY_07_07, *schemas.STATISTICS]
    assert result["STUDYID"].tolist() == [
        "CDISCPILOT01",
        "CDISCPILOT01",
        "CDISCPILOT02",
        "CDISCPILOT02",
        pooled,
        pooled,
    ]
    assert result["TRTPN"].tolist() == [0.0, 54.0, 0.0, 54.0, 0.0, 54.0]
    assert result["studynum"].dtype == np.int64
    assert result["studynum"].tolist() == [1, 1, 2, 2, 3, 3]
    assert result["trtp_short"].tolist() == ["P", "X-low", "P", "X-low", "P", "X-low"]
    assert result["n"].tolist() == [1, 1, 1, 1, 2, 2]
    _assert_agree(result["mean"], [3.0, 5.0, 6.0, 2.0, 4.5, 3.5], tolerances)


@pytest.mark.parametrize(
    ("by", "var", "error"),
    [
        ((), "CHG", ValueError),
        (("TRTPN", "TRTPN"), "CHG", ValueError),
        (("TRTPN", "mean"), "CHG", ValueError),
        (("TRTPN",), "AVAL", KeyError),
        (("TRTPN", "STUDYID"), "CHG", KeyError),
        (("TRTPN",), "trtp_short", TypeError),
    ],
    ids=["no-by", "repeated-by", "by-named-like-statistic", "absent-var", "absent-by", "char-var"],
)
def test_s02_and_s07_mod_01_proc_summary_rejects_invalid_arguments(
    by: "tuple[str, ...]", var: str, error: "type[Exception]"
) -> None:
    """S02-MOD-01, S07-MOD-01: a BY list or analysis variable SAS would reject raises."""
    frame = _frame_07_02([(2.0, 0.0, 80.0, 1.0)])
    with pytest.raises(error, match="proc_summary|sas_sort"):
        proc_summary(frame, by, var=var)
