"""Record-selection tests of ``wpct.selection``.

Each test but one builds a small synthetic frame and checks one rule of
``python/wpct/docs/sas_specification.md`` against the SAS step it ports: the WPCT-F.07.02 subset,
flags, baseline exclusion and keep list (S02-*), the WPCT-F.07.07 subset, post-baseline filter,
flags, keep list, pooling and study numbering (S07-*), the ``$6`` truncation (D-12), comparisons
with missing values (D-13), the pooled identifier and its sort order (D-16), and the halting checks
S07-FLT-04 and S07-FLT-05. The exception applies the ``advs_sub`` step of each program (S02-FLT-01
with S02-TRN-02, S07-FLT-01 with S07-TRN-02) to the repository ADVS and checks the result against
the ``WORK.ADVS_SUB`` line of the SAS log, both read through the session fixtures of
``conftest.py``. No test writes a file.
"""

import numpy as np
import pandas as pd
import pytest
from pandas.testing import assert_frame_equal

from wpct import selection
from wpct.errors import AnalysisHalted
from wpct.sas_semantics import assign_char, prefix_match, sas_sort
from wpct.schemas import (
    ADVS_SUB_EXTRA,
    ANA_VARIABLES,
    POOLED_STUDYID,
    PROGRAM_07_02,
    PROGRAM_07_07,
    export_spec,
)

#: The 07.02 subset WHERE as (PARAMCD, ATPTN values) pairs
#: [whitepapers/WPCT/WPCT-F.07.02.sas:90-93].
PAIRS_07_02 = (("DIABP", (815,)), ("SYSBP", (816,)))
#: The 07.07 subset WHERE [whitepapers/WPCT/WPCT-F.07.07.sas:146-148].
PAIRS_07_07 = (("DIABP", (815, 817)),)
#: The 07.07 ``css_anadata`` record key from :mod:`wpct.schemas`, the key ``%assert_unique_keys``
#: checks [whitepapers/WPCT/WPCT-F.07.07.sas:244].
POOLED_KEYS = export_spec(PROGRAM_07_07, "css_anadata").key
#: The 07.07 ``css_anadata`` columns from :mod:`wpct.schemas` before S07-TRN-06 appends
#: ``studynum``: the ``css_safana`` order that S07-TRN-04 keeps and S07-TRN-05 stacks.
CSS_SAFANA_COLUMNS = tuple(
    column for column in export_spec(PROGRAM_07_07, "css_anadata").columns if column != "studynum"
)

#: Unique ``css_safana`` records ``(STUDYID, USUBJID, TRTPN, ATPTN, CHG)``; their pooled stacking is
#: unique on :data:`POOLED_KEYS` too.
UNIQUE_RECORDS = (
    ("CDISCPILOT01", "01-701-1015", 0.0, 815.0, -3.0),
    ("CDISCPILOT01", "01-701-1015", 0.0, 817.0, -1.0),
    ("CDISCPILOT02", "02-701-1023", 54.0, 815.0, 4.0),
)

_TRTP_SHORT = {0.0: "P", 54.0: "X-low", 81.0: "X-high"}
_ATPT = {815.0: "AFTER LYING DOWN FOR 5 MINUTES", 817.0: "AFTER STANDING FOR 3 MINUTES"}


def _sas_list(text: str) -> tuple[str, ...]:
    """A blank-separated SAS variable list, as the programs write it, as a tuple of names."""
    return tuple(text.split())


def _rows(frame: pd.DataFrame, positions: list[int]) -> pd.DataFrame:
    """The records of ``frame`` at ``positions`` as a filter returns them, with a fresh index."""
    return frame.iloc[positions].reset_index(drop=True)


def _variables_frame(columns: tuple[str, ...]) -> pd.DataFrame:
    """Two records whose every value names its column, so a misplaced column cannot go unseen."""
    return pd.DataFrame({column: [f"{column}:1", f"{column}:2"] for column in columns})


def _css_safana(records) -> pd.DataFrame:
    """A 07.07 ``css_safana`` frame in its S07-TRN-04 column order, one row per record tuple.

    The order is :data:`CSS_SAFANA_COLUMNS`. Every row names exactly those columns, so a column the
    schema adds, drops or renames fails here instead of turning into a column of missing values.
    """
    rows = [
        {
            "STUDYID": studyid,
            "USUBJID": usubjid,
            "SAFFL": "Y",
            "TRTPN": trtpn,
            "PARAMCD": "DIABP",
            "PARAM": "Diastolic Blood Pressure (mmHg)",
            "ATPTN": atptn,
            "ATPT": _ATPT.get(atptn, ""),
            "trtp_short": _TRTP_SHORT[trtpn],
            "BASE": 80.0,
            "CHG": chg,
            "ANL02FL": "Y",
        }
        for studyid, usubjid, trtpn, atptn, chg in records
    ]
    for row in rows:
        assert sorted(row) == sorted(CSS_SAFANA_COLUMNS)
    return pd.DataFrame(rows, columns=list(CSS_SAFANA_COLUMNS))


def _css_anadata(records) -> pd.DataFrame:
    """The 07.07 pooled analysis set as the driver builds it: S07-TRN-05, then S07-TRN-06."""
    return selection.assign_studynum(selection.stack_pooled(_css_safana(records)))


# --- WPCT-F.07.02 ---------------------------------------------------------------------------------


def test_s02_flt_01_param_timepoint_subset():
    """S02-FLT-01 [whitepapers/WPCT/WPCT-F.07.02.sas:90-93]: keep (DIABP, 815) and (SYSBP, 816).

    The crossed pairings, another parameter and a missing ATPTN are dropped. Kept records keep their
    input order and every input variable, and a PARAMCD with trailing blanks still matches (D-11).
    """
    advs = pd.DataFrame(
        {
            "SEQ": [1, 2, 3, 4, 5, 6, 7],
            "PARAMCD": ["DIABP", "SYSBP", "SYSBP", "DIABP", "PULSE", "DIABP", "DIABP  "],
            "ATPTN": [815.0, 815.0, 816.0, 816.0, 815.0, np.nan, 815.0],
            "AVAL": [80.0, 120.0, 118.0, 79.0, 70.0, 81.0, 82.0],
        }
    )

    kept = selection.subset_param_timepoints(advs, PAIRS_07_02)

    assert kept["SEQ"].tolist() == [1, 3, 7]
    assert_frame_equal(kept, _rows(advs, [0, 2, 6]), check_exact=True)


@pytest.mark.parametrize(
    ("pairs", "timepoint"), [(PAIRS_07_02, 815.0), (PAIRS_07_07, 817.0)], ids=["07.02", "07.07"]
)
def test_d13_in_list_false_for_missing(pairs, timepoint):
    """D-13 for S02-FLT-01 and S07-FLT-01: ``atptn in (...)`` is false for a missing ATPTN.

    [whitepapers/WPCT/WPCT-F.07.02.sas:92; whitepapers/WPCT/WPCT-F.07.07.sas:148]. The port relies
    on pandas ``isin`` being false for NaN, as SAS ``IN`` is for ``.``.
    """
    advs = pd.DataFrame({"PARAMCD": ["DIABP", "DIABP"], "ATPTN": [timepoint, np.nan]})
    assert advs["ATPTN"].isin([timepoint]).tolist() == [True, False]

    kept = selection.subset_param_timepoints(advs, pairs)

    assert_frame_equal(kept, _rows(advs, [0]), check_exact=True)


@pytest.mark.parametrize(
    "missing", [float("nan"), np.nan, np.float32("nan")], ids=["float", "np.nan", "np.float32"]
)
@pytest.mark.parametrize("pairs", [PAIRS_07_02, PAIRS_07_07], ids=["07.02", "07.07"])
def test_d13_missing_configured_timepoint_rejected(pairs, missing):
    """D-13 for S02-FLT-01 and S07-FLT-01: a missing configured ATPTN is refused, never matched.

    The SAS IN lists hold only non-missing timepoints [whitepapers/WPCT/WPCT-F.07.02.sas:92-93;
    whitepapers/WPCT/WPCT-F.07.07.sas:148], so a missing ATPTN is never selected. pandas ``isin``
    matches NaN to NaN, so a missing value added to the DIABP list would select the missing-ATPTN
    record; the subset raises instead and leaves the input unchanged.
    """
    (code, timepoints), *others = pairs
    configured = ((code, (*timepoints, missing)), *others)
    advs = pd.DataFrame({"PARAMCD": ["DIABP", "DIABP"], "ATPTN": [815.0, np.nan]})
    original = advs.copy()
    assert advs["ATPTN"].isin([815.0, float(missing)]).tolist() == [True, True]

    with pytest.raises(ValueError, match="ATPTN values of DIABP must be non-missing") as raised:
        selection.subset_param_timepoints(advs, configured)

    assert str(raised.value).endswith(f"got {[missing]!r}")
    assert_frame_equal(advs, original, check_exact=True)


def test_s02_trn_02_abbreviate_treatment():
    """S02-TRN-02, S07-TRN-02 and D-12: the ``$6`` treatment abbreviation.

    [whitepapers/WPCT/WPCT-F.07.02.sas:95-102; whitepapers/WPCT/WPCT-F.07.07.sas:150-157]. The three
    WHEN values give ``P``, ``X-high`` and ``X-low``; WHEN ignores trailing blanks (D-11) but not
    case. Every other value, a blank or missing TRTP included, takes OTHERWISE ``'UNEXPECTED'``,
    which the ``$6`` length stores as ``UNEXPE``. ``trtp_short``, the only variable the ATTRIB
    statement adds and so the whole of ``schemas.ADVS_SUB_EXTRA``, follows every input variable.
    """
    assert ADVS_SUB_EXTRA == _sas_list("trtp_short")
    assert assign_char("UNEXPECTED", 6) == "UNEXPE"
    assert assign_char("P", 6) == "P"
    advs = pd.DataFrame(
        {
            "USUBJID": [f"01-701-10{number:02d}" for number in range(8)],
            "TRTP": [
                "Placebo",
                "Xanomeline High Dose",
                "Xanomeline Low Dose",
                "Placebo   ",
                "placebo",
                "Other",
                "",
                None,
            ],
            "TRTPN": [0.0, 81.0, 54.0, 0.0, 0.0, 99.0, np.nan, np.nan],
        }
    )
    original = advs.copy()

    out = selection.abbreviate_treatment(advs)

    assert out["trtp_short"].tolist() == [
        "P",
        "X-high",
        "X-low",
        "P",
        "UNEXPE",
        "UNEXPE",
        "UNEXPE",
        "UNEXPE",
    ]
    assert tuple(out.columns) == (*advs.columns, *ADVS_SUB_EXTRA)
    assert_frame_equal(out.drop(columns=list(ADVS_SUB_EXTRA)), advs, check_exact=True)
    assert_frame_equal(advs, original, check_exact=True)


def _assert_flag_filter(flags, analysis_flag: str) -> None:
    """Only records whose SAFFL and ``analysis_flag`` are both ``'Y'`` survive ``flag_filter``.

    Trailing blanks are ignored (``'Y '`` matches); a leading blank, lower case, ``'N'`` and the
    missing ``''`` do not match (D-11).
    """
    frame = pd.DataFrame(
        {
            "SEQ": [1, 2, 3, 4, 5, 6, 7, 8, 9],
            "SAFFL": ["Y", "Y", "N", "", "Y", "y", "Y", "Y ", " Y"],
            analysis_flag: ["Y", "N", "Y", "Y", "", "Y", "y", "Y", "Y"],
        }
    )

    kept = selection.flag_filter(frame, flags)

    assert kept["SEQ"].tolist() == [1, 8]
    assert_frame_equal(kept, _rows(frame, [0, 7]), check_exact=True)


@pytest.mark.parametrize(
    "flags", [("SAFFL", "ANL01FL"), "saffl anl01fl"], ids=["names", "sas-list"]
)
def test_s02_pop_01_and_flt_02_flag_filter(flags):
    """S02-POP-01 and S02-FLT-02: ``where saffl = 'Y' and anl01fl = 'Y'`` (D-11).

    [whitepapers/WPCT/WPCT-F.07.02.sas:164]. The flags may be named as a sequence or as the SAS
    variable list in the program's lower-case spelling.
    """
    _assert_flag_filter(flags, "ANL01FL")


#: AVISITN dtypes: the float64 that ``pyreadstat`` returns, whose missing value is NaN, and the
#: pandas nullable numeric dtypes, whose missing value is ``pd.NA``.
VISIT_DTYPES = ("float64", "Int64", "Float64")


def _visit_frame(dtype: str) -> pd.DataFrame:
    """Six records whose AVISITN of ``dtype`` holds visit 0 twice and the missing value once."""
    frame = pd.DataFrame(
        {"SEQ": [1, 2, 3, 4, 5, 6], "AVISITN": pd.array([0, 2, 99, None, 0, 4], dtype=dtype)}
    )
    assert frame["AVISITN"].dtype == dtype
    assert frame["AVISITN"].isna().tolist() == [False, False, False, True, False, False]
    return frame


@pytest.mark.parametrize(
    ("dtype", "ne_missing"),
    [("float64", True), ("Int64", pd.NA), ("Float64", pd.NA)],
    ids=VISIT_DTYPES,
)
def test_s02_flt_03_exclude_baseline_visit(dtype, ne_missing):
    """S02-FLT-03 and D-13: ``where also avisitn ne 0`` drops visit 0 and keeps a missing AVISITN.

    [whitepapers/WPCT/WPCT-F.07.02.sas:166-167]. SAS ``. ne 0`` is true. pandas ``NaN != 0`` is
    true too, but a nullable dtype's ``NA != 0`` is ``NA``, which the port resolves to true.
    """
    frame = _visit_frame(dtype)
    assert (frame["AVISITN"] != 0).tolist() == [False, True, True, ne_missing, False, True]

    kept = selection.exclude_visit(frame, "AVISITN", 0)

    assert kept["SEQ"].tolist() == [2, 3, 4, 6]
    assert_frame_equal(kept, _rows(frame, [1, 2, 3, 5]), check_exact=True)
    assert_frame_equal(selection.exclude_visit(frame), kept, check_exact=True)


@pytest.mark.parametrize("value", [None, np.nan], ids=["None", "NaN"])
@pytest.mark.parametrize("dtype", VISIT_DTYPES)
def test_s02_flt_03_exclude_missing_visit(dtype, value):
    """S02-FLT-03 and D-13: a missing ``b_visn`` gives ``avisitn ne .``, dropping missing AVISITN.

    [whitepapers/WPCT/WPCT-F.07.02.sas:166-167]. ``None`` and NaN both stand for the SAS missing
    value ``.``, which equals every missing AVISITN, NaN and ``pd.NA`` alike.
    """
    frame = _visit_frame(dtype)

    kept = selection.exclude_visit(frame, "AVISITN", value)

    assert kept["SEQ"].tolist() == [1, 2, 3, 5, 6]
    assert_frame_equal(kept, _rows(frame, [0, 1, 2, 4, 5]), check_exact=True)


def _assert_keep_variables(
    program_id: str, input_columns: tuple[str, ...], expected: tuple[str, ...]
) -> None:
    """``keep_variables`` with the program's ``&ana_variables`` keeps ``expected``, in input order.

    ``keep=`` does not reorder, so reversing the input columns reverses the result.
    """
    frame = _variables_frame(input_columns)

    kept = selection.keep_variables(frame, ANA_VARIABLES[program_id])

    assert tuple(kept.columns) == expected
    assert_frame_equal(kept, frame[list(expected)], check_exact=True)
    reversed_input = frame[list(reversed(input_columns))]
    reversed_kept = selection.keep_variables(reversed_input, ANA_VARIABLES[program_id])
    assert tuple(reversed_kept.columns) == tuple(reversed(expected))


def test_s02_trn_03_keep_variables():
    """S02-TRN-03 [whitepapers/WPCT/WPCT-F.07.02.sas:144,163]: ``keep=&ana_variables``.

    The 14 analysis variables are kept in their ``advs_sub`` order, which is the ``css_anadata``
    export order; ``TRTP``, ``AVAL`` and the dates are dropped.
    """
    assert ANA_VARIABLES[PROGRAM_07_02] == _sas_list(
        "STUDYID USUBJID SAFFL ANL01FL trtp_short TRTPN PARAM PARAMCD CHG BASE AVISIT AVISITN "
        "ATPT ATPTN"
    )
    advs_sub_columns = _sas_list(
        "STUDYID USUBJID SAFFL TRTP TRTPN PARAMCD PARAM ATPTN ATPT AVISIT AVISITN AVAL BASE CHG "
        "ANL01FL ADT TRTSDT trtp_short"
    )
    expected = export_spec(PROGRAM_07_02, "css_anadata").columns
    assert expected == tuple(
        column for column in advs_sub_columns if column in ANA_VARIABLES[PROGRAM_07_02]
    )

    _assert_keep_variables(PROGRAM_07_02, advs_sub_columns, expected)


# --- WPCT-F.07.07 ---------------------------------------------------------------------------------


def test_s07_flt_01_param_timepoint_subset():
    """S07-FLT-01 [whitepapers/WPCT/WPCT-F.07.07.sas:146-148]: keep DIABP at ATPTN 815 and 817.

    DIABP at 816, SYSBP at a listed timepoint and a missing ATPTN are dropped; kept records keep
    their input order.
    """
    advs = pd.DataFrame(
        {
            "SEQ": [1, 2, 3, 4, 5, 6],
            "PARAMCD": ["DIABP", "DIABP", "DIABP", "SYSBP", "SYSBP", "DIABP"],
            "ATPTN": [815.0, 816.0, 817.0, 815.0, 817.0, np.nan],
        }
    )

    kept = selection.subset_param_timepoints(advs, PAIRS_07_07)

    assert kept["SEQ"].tolist() == [1, 3]
    assert_frame_equal(kept, _rows(advs, [0, 2]), check_exact=True)


def test_s07_flt_02_post_baseline_prefix():
    """S07-FLT-02 [whitepapers/WPCT/WPCT-F.07.07.sas:168-171]: ``where avisit =: 'Post-baseline'``.

    The SAS prefix test is case-sensitive and compares the blank-padded 20-character AVISIT
    (LMM-TRN-07), so a value shorter than the prefix does not match (D-11). The filter keeps exactly
    the records ``sas_semantics.prefix_match`` selects.
    """
    avisit = [
        "Baseline (MIN)",
        "Post-baseline (MIN)",
        "post-baseline (MIN)",
        "",
        "Week 2",
        "Post-baselin",
        "Post-baseline (MIN)",
    ]
    frame = pd.DataFrame({"SEQ": [1, 2, 3, 4, 5, 6, 7], "AVISIT": avisit})
    expected_mask = [False, True, False, False, False, False, True]
    assert prefix_match(frame["AVISIT"], "Post-baseline", 20).tolist() == expected_mask

    kept = selection.post_baseline_filter(frame)

    assert kept["SEQ"].tolist() == [2, 7]
    assert_frame_equal(kept, _rows(frame, [1, 6]), check_exact=True)


@pytest.mark.parametrize(
    "flags", [("SAFFL", "ANL02FL"), "saffl anl02fl"], ids=["names", "sas-list"]
)
def test_s07_pop_01_and_flt_03_flag_filter(flags):
    """S07-POP-01 and S07-FLT-03: ``where=(saffl = 'Y' and anl02fl = 'Y')`` (D-11).

    [whitepapers/WPCT/WPCT-F.07.07.sas:212-214].
    """
    _assert_flag_filter(flags, "ANL02FL")


def test_s07_trn_04_keep_variables():
    """S07-TRN-04 [whitepapers/WPCT/WPCT-F.07.07.sas:190,213]: ``keep=&ana_variables``.

    The 12 analysis variables are kept in their ``ADVS_MIN`` order (LMM-TRN-07), the ``css_anadata``
    export order before ``studynum``; AVISIT, AVISITN, AVAL and ADT are dropped.
    """
    assert ANA_VARIABLES[PROGRAM_07_07] == _sas_list(
        "STUDYID USUBJID SAFFL ANL02FL trtp_short TRTPN PARAM PARAMCD CHG BASE ATPT ATPTN"
    )
    advs_min_columns = export_spec(PROGRAM_07_07, "advs_min").columns
    expected = CSS_SAFANA_COLUMNS
    assert (*expected, "studynum") == export_spec(PROGRAM_07_07, "css_anadata").columns
    assert expected == tuple(
        column for column in advs_min_columns if column in ANA_VARIABLES[PROGRAM_07_07]
    )

    _assert_keep_variables(PROGRAM_07_07, advs_min_columns, expected)


def test_s07_trn_05_stack_pooled():
    """S07-TRN-05 and D-16 [whitepapers/WPCT/WPCT-F.07.07.sas:222-230]: the pooled copy.

    The input records come first, unchanged, then their copy with ``STUDYID = 'A0'x !! 'Pooled'``
    and the first USUBJID character replaced by ``P``; every other variable is copied unchanged.
    """
    assert POOLED_STUDYID == "\u00a0Pooled"
    assert POOLED_STUDYID.encode("latin-1") == b"\xa0Pooled"
    css_safana = _css_safana(
        [
            ("CDISCPILOT01", "01-701-1015", 0.0, 815.0, -3.0),
            ("CDISCPILOT02", "01-702-1023", 54.0, 815.0, 2.0),
            ("CDISCPILOT01", "01-703-1042", 81.0, 817.0, 5.0),
        ]
    )
    original = css_safana.copy()

    stacked = selection.stack_pooled(css_safana)

    assert len(stacked) == 2 * len(css_safana) == 6
    assert list(stacked.columns) == list(css_safana.columns)
    assert_frame_equal(stacked.iloc[:3], css_safana, check_exact=True)
    pooled = _rows(stacked, [3, 4, 5])
    assert pooled["STUDYID"].tolist() == [POOLED_STUDYID] * 3
    assert pooled["USUBJID"].tolist() == ["P1-701-1015", "P1-702-1023", "P1-703-1042"]
    unchanged = [column for column in css_safana.columns if column not in ("STUDYID", "USUBJID")]
    assert_frame_equal(pooled[unchanged], css_safana[unchanged], check_exact=True)
    assert_frame_equal(css_safana, original, check_exact=True)


def test_d16_pooled_sorts_after_ascii():
    """D-16 [whitepapers/WPCT/WPCT-F.07.07.sas:226-227]: the leading ``'A0'x`` sorts pooled last.

    Code point 160 follows every ASCII character, a lower-case study identifier included, so PROC
    SORT order (``sas_semantics.sas_sort``) puts the pooled study after the individual studies.
    """
    frame = pd.DataFrame(
        {
            "STUDYID": [POOLED_STUDYID, "CDISCPILOT02", "CDISCPILOT01", "zz-study"],
            "SEQ": [1, 2, 3, 4],
        }
    )

    ordered = sas_sort(frame, ["STUDYID"])

    assert ordered["STUDYID"].tolist() == [
        "CDISCPILOT01",
        "CDISCPILOT02",
        "zz-study",
        POOLED_STUDYID,
    ]
    assert ordered["SEQ"].tolist() == [3, 2, 4, 1]


def test_s07_trn_06_assign_studynum():
    """S07-TRN-06, D-14 and D-16 [whitepapers/WPCT/WPCT-F.07.07.sas:233-241].

    ``proc sort; by studyid trtpn;`` is stable: the three replicates of each study and treatment,
    equal on both keys and scattered through the input, keep their input order. ``if first.studyid
    then studynum+1;`` numbers the studies 1, 2, 3 in sort order, the pooled study last, and
    ``studynum`` follows every input variable as an integer.
    """
    studies = ("CDISCPILOT01", "CDISCPILOT02", POOLED_STUDYID)
    treatments = (0.0, 54.0, 81.0)
    replicates = (1, 2, 3)
    unsorted = [(studyid, trtpn) for studyid in reversed(studies) for trtpn in (54.0, 81.0, 0.0)]
    records = []
    for replicate in replicates:
        for studyid, trtpn in unsorted[replicate:] + unsorted[:replicate]:
            records.append((len(records) + 1, studyid, trtpn, replicate))
    frame = pd.DataFrame(records, columns=["SEQ", "STUDYID", "TRTPN", "REPLICATE"])
    original = frame.copy()
    seq_of = {(studyid, trtpn, rep): seq for seq, studyid, trtpn, rep in records}
    expected_seq = [
        seq_of[(studyid, trtpn, rep)]
        for studyid in studies
        for trtpn in treatments
        for rep in replicates
    ]

    numbered = selection.assign_studynum(frame, ("STUDYID", "TRTPN"), "studynum")

    assert numbered["SEQ"].tolist() == expected_seq
    expected = _rows(frame, [seq - 1 for seq in expected_seq])
    expected["studynum"] = np.repeat(np.array([1, 2, 3], dtype=np.int64), 9)
    assert_frame_equal(numbered, expected, check_exact=True)
    assert_frame_equal(selection.assign_studynum(frame), numbered, check_exact=True)
    assert_frame_equal(frame, original, check_exact=True)


@pytest.mark.parametrize(
    ("studies", "expected"),
    [
        (
            ("CDISCPILOT01", "CDISCPILOT02"),
            [("CDISCPILOT01", 1), ("CDISCPILOT02", 2), (POOLED_STUDYID, 3)],
        ),
        (("CDISCPILOT01",), [("CDISCPILOT01", 1), (POOLED_STUDYID, 2)]),
    ],
    ids=["two-studies", "single-study"],
)
def test_s07_trn_06_pooled_studynum_is_last(studies, expected):
    """S07-TRN-06 and D-16 [whitepapers/WPCT/WPCT-F.07.07.sas:222-241]: pooled ``studynum`` is last.

    Stacking (S07-TRN-05) and numbering count only the studies present, so the pooled study takes
    ``&stdyn``: 3 with both studies, 2 when only ``CDISCPILOT01`` has data.
    """
    records = [
        (studyid, f"{index:02d}-701-10{subject}", trtpn, 815.0, 1.0)
        for index, studyid in enumerate(reversed(studies), start=1)
        for subject, trtpn in ((1, 81.0), (2, 0.0))
    ]

    numbered = _css_anadata(records)

    assert len(numbered) == 2 * len(records) > 0
    pairs = numbered[["STUDYID", "studynum"]].drop_duplicates()
    assert list(pairs.itertuples(index=False, name=None)) == expected


def _assert_frame_returned(result, frame: pd.DataFrame) -> None:
    """A passing halting check returns the frame unchanged, as a new frame with a fresh index."""
    assert isinstance(result, pd.DataFrame)
    assert result is not frame
    assert isinstance(result.index, pd.RangeIndex)
    assert_frame_equal(result, frame, check_exact=True)


@pytest.mark.parametrize(
    "keys", [POOLED_KEYS, " ".join(POOLED_KEYS).lower()], ids=["names", "sas-list"]
)
@pytest.mark.parametrize(
    ("extra_record", "duplicates"),
    [
        # The pooled copy of 11-701-1015 becomes P1-701-1015, the pooled copy of 01-701-1015.
        (("CDISCPILOT02", "11-701-1015", 81.0, 815.0, 2.0), 1),
        # An exact copy duplicates its study's key group and its pooled key group.
        (UNIQUE_RECORDS[2], 2),
    ],
    ids=["pooled-usubjid-collision", "duplicated-record"],
)
def test_unique_keys_halt(keys, extra_record, duplicates):
    """S07-FLT-05 [whitepapers/WPCT/WPCT-F.07.07.sas:244-245]: duplicate pooled keys halt.

    ``%assert_unique_keys(css_anadata, studyid usubjid paramcd atptn)`` selects only the grouped
    keys, so its query returns one row per duplicated key group and ``SQLOBS`` counts those groups
    [whitepapers/utilities/assert_unique_keys.sas:84-100]; ``%assert_continue`` then halts
    [whitepapers/utilities/assert_continue.sas:22-39]. Unique keys let processing continue.
    """
    unique = _css_anadata(UNIQUE_RECORDS)
    assert len(unique) == 6
    _assert_frame_returned(
        selection.assert_unique_keys(unique, keys, "CSS_ANADATA", PROGRAM_07_07), unique
    )
    duplicated = _css_anadata([*UNIQUE_RECORDS, extra_record])

    with pytest.raises(AnalysisHalted) as raised:
        selection.assert_unique_keys(duplicated, keys, "CSS_ANADATA", PROGRAM_07_07)

    halt = raised.value
    assert str(halt) == halt.message
    assert halt.message == (
        "WPCT-F.07.07: assert_unique_keys failed: unexpected duplicates in CSS_ANADATA with unique "
        f"keys {' '.join(POOLED_KEYS).upper()} (SQLOBS = {duplicates})"
    )
    assert (halt.program_id, halt.check, halt.checkpoints) == (
        PROGRAM_07_07,
        "assert_unique_keys",
        (),
    )


def test_s07_flt_05_missing_last_key_not_counted():
    """S07-FLT-05 and D-13 [whitepapers/utilities/assert_unique_keys.sas:84-97].

    ``having count(atptn) > 1`` counts non-missing ATPTN values only, so records duplicated on a
    missing ATPTN do not halt, as in SAS.
    """
    frame = _css_anadata([("CDISCPILOT01", "01-701-1015", 0.0, np.nan, -3.0)] * 2)
    assert len(frame) == 4

    _assert_frame_returned(
        selection.assert_unique_keys(frame, POOLED_KEYS, "CSS_ANADATA", PROGRAM_07_07), frame
    )


@pytest.mark.parametrize("var", ["CHG", "chg"], ids=["upper", "sas-name"])
def test_s07_flt_04_missing_chg_halts(var):
    """S07-FLT-04 [whitepapers/WPCT/WPCT-F.07.07.sas:219-220]: a missing CHG halts.

    ``%assert_var_nonmissing(css_safana, chg)`` fails when ``where=(missing(chg))`` selects a record
    [whitepapers/utilities/assert_var_nonmissing.sas:43-59], and ``%assert_continue`` halts
    [whitepapers/utilities/assert_continue.sas:22-39]. Only CHG is examined, so a missing BASE alone
    lets processing continue.
    """
    complete = _css_safana(UNIQUE_RECORDS)
    assert len(complete) == 3
    _assert_frame_returned(
        selection.assert_var_nonmissing(complete, var, "CSS_SAFANA", PROGRAM_07_07), complete
    )
    missing_base = complete.assign(BASE=np.nan)
    _assert_frame_returned(
        selection.assert_var_nonmissing(missing_base, var, "CSS_SAFANA", PROGRAM_07_07),
        missing_base,
    )
    missing_chg = complete.copy()
    missing_chg.loc[[0, 2], "CHG"] = np.nan

    with pytest.raises(AnalysisHalted) as raised:
        selection.assert_var_nonmissing(missing_chg, var, "CSS_SAFANA", PROGRAM_07_07)

    halt = raised.value
    assert str(halt) == halt.message
    assert halt.message == "WPCT-F.07.07: assert_var_nonmissing failed: 2 missing CHG in CSS_SAFANA"
    assert (halt.program_id, halt.check, halt.checkpoints) == (
        PROGRAM_07_07,
        "assert_var_nonmissing",
        (),
    )


@pytest.mark.parametrize(
    ("check", "args", "message"),
    [
        (
            selection.assert_var_nonmissing,
            ("CHG", "CSS_SAFANA"),
            "WPCT-F.07.07: assert_var_nonmissing failed: variable CHG does not exist in CSS_SAFANA",
        ),
        (
            selection.assert_unique_keys,
            (POOLED_KEYS, "CSS_ANADATA"),
            (
                "WPCT-F.07.07: assert_unique_keys failed: key variables ATPTN do not exist in "
                "CSS_ANADATA"
            ),
        ),
    ],
    ids=["assert_var_nonmissing", "assert_unique_keys"],
)
def test_s07_flt_04_05_halting_checks_halt_on_absent_variable(check, args, message):
    """S07-FLT-04 and S07-FLT-05: an absent variable halts, as the macros' existence checks fail.

    [whitepapers/utilities/assert_var_nonmissing.sas:34-40;
    whitepapers/utilities/assert_unique_keys.sas:51-67].
    """
    frame = _css_anadata(UNIQUE_RECORDS).drop(columns=["CHG", "ATPTN"])
    assert len(frame) == 6

    with pytest.raises(AnalysisHalted) as raised:
        check(frame, *args, PROGRAM_07_07)

    assert str(raised.value) == message
    assert raised.value.check == check.__name__


@pytest.mark.parametrize(
    ("check", "args"),
    [
        (selection.assert_var_nonmissing, ("CHG", "CSS_SAFANA")),
        (selection.assert_unique_keys, (POOLED_KEYS, "CSS_ANADATA")),
    ],
    ids=["assert_var_nonmissing", "assert_unique_keys"],
)
def test_s07_flt_04_flt_05_empty_frame_returned(check, args):
    """S07-FLT-04 and S07-FLT-05: an empty frame passes and comes back unchanged.

    ``where=(missing(chg))`` selects no record, so ``NLOBSF`` is 0 and the check passes
    [whitepapers/utilities/assert_var_nonmissing.sas:43-58]; the GROUP BY query returns no row, so
    ``SQLOBS = 0`` and the check passes [whitepapers/utilities/assert_unique_keys.sas:84-104].
    """
    frame = _css_anadata(UNIQUE_RECORDS).iloc[:0]
    assert frame.empty and "CHG" in frame and "ATPTN" in frame

    _assert_frame_returned(check(frame, *args, PROGRAM_07_07), frame)


# --- Both programs on the repository ADVS ---------------------------------------------------------


@pytest.mark.parametrize(
    ("program_id", "advs_fixture", "pairs"),
    [
        (PROGRAM_07_02, "advs_07_02_raw", PAIRS_07_02),
        (PROGRAM_07_07, "advs_07_07_raw", PAIRS_07_07),
    ],
    ids=["07.02", "07.07"],
)
def test_s02_flt_01_trn_02_s07_flt_01_trn_02_advs_sub_variables(
    program_id, advs_fixture, pairs, request, sas_checkpoints
):
    """S02-FLT-01 with S02-TRN-02, and S07-FLT-01 with S07-TRN-02, on the repository ADVS.

    DATA ``advs_sub`` [whitepapers/WPCT/WPCT-F.07.02.sas:90-103;
    whitepapers/WPCT/WPCT-F.07.07.sas:146-158] keeps every ``WORK.ADVS`` variable in input order and
    appends ``schemas.ADVS_SUB_EXTRA``. Applied to the ADVS that ``xpt_reader`` reads, the subset
    has the shape the SAS log gives ``WORK.ADVS_SUB``: 5925 (07.02) or 5926 (07.07) observations
    and 45 variables, the 44 of ``WORK.ADVS`` plus one
    [whitepapers/WPCT/outputs_sas/WPCT-F.07.02.log:32,59;
    whitepapers/WPCT/outputs_sas/WPCT-F.07.07.log:32,61]. Both log lines are read through the
    ``sas_checkpoints`` fixture.
    """
    advs = request.getfixturevalue(advs_fixture)
    logged = [
        checkpoint
        for checkpoint in sas_checkpoints[program_id]
        if checkpoint.spec.SAS_DATASET in ("ADVS", "ADVS_SUB")
    ]
    assert [checkpoint.spec.SAS_DATASET for checkpoint in logged] == ["ADVS", "ADVS_SUB"]
    advs_log, advs_sub_log = logged
    assert advs_log.ok and advs_sub_log.ok, (advs_log.problem, advs_sub_log.problem)
    assert advs.shape == (advs_log.NOBS, advs_log.NVARS)
    assert advs_sub_log.NVARS == advs_log.NVARS + len(ADVS_SUB_EXTRA) == 45

    advs_sub = selection.abbreviate_treatment(selection.subset_param_timepoints(advs, pairs))

    assert tuple(advs_sub.columns) == (*advs.columns, *ADVS_SUB_EXTRA)
    assert advs_sub.shape == (advs_sub_log.NOBS, advs_sub_log.NVARS)
