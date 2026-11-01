"""Tests of ``wpct.derivation``, the port of ``%derive_lastminmax_measure`` (S07-TRN-03, LMM-*).

SAS source: ``whitepapers/ADaM/derive_lastminmax_measure.sas``, macro lines 77-213. Every case runs
on a synthetic frame built by :func:`make_rows` and passes the arguments of the 07.07 call
[whitepapers/WPCT/WPCT-F.07.07.sas:161-166], taken from ``wpct.schemas``. Expected values are
hand-computed literals, never a second derivation; the record counts of the real data
[whitepapers/WPCT/outputs_sas/WPCT-F.07.07.log:79-195] are verified in ``test_wpct_f_07_07.py``.

Every test asserts its record counts before the record content, so none passes on an empty result.
"""

from __future__ import annotations

import dataclasses
import logging
import math
import re
from collections.abc import Callable, Mapping, Sequence

import pandas as pd
import pytest

from wpct import derivation
from wpct.sas_semantics import is_char_missing
from wpct.schemas import LMM_GRPVARS, LMM_INCL, LMM_ORDVARS, PROGRAM_07_07, export_spec

LOGGER = "wpct.derivation"

#: Treatment start date of every synthetic record unless overridden, as a SAS day number (D-10).
TRTSDT = 100.0

SUBJECT_A = "01-701-1015"
SUBJECT_B = "01-701-1023"
SUBJECT_C = "01-701-1028"

#: Columns of :func:`make_rows`: the S07-TRN-03 variables plus the LMM-FLT-01 dates and value.
INPUT_COLUMNS = (
    "STUDYID",
    "USUBJID",
    "TRTPN",
    "PARAMCD",
    "ATPTN",
    "AVISITN",
    "AVISIT",
    "AVAL",
    "ADT",
    "TRTSDT",
    "trtp_short",
    "SAFFL",
    "PARAM",
    "ATPT",
)
_NUMERIC_INPUTS = ("TRTPN", "ATPTN", "AVISITN", "AVAL", "ADT", "TRTSDT")

_DEFAULTS: Mapping[str, object] = {
    "STUDYID": "CDISCPILOT01",
    "USUBJID": SUBJECT_A,
    "TRTPN": 54.0,
    "PARAMCD": "DIABP",
    "ATPTN": 815.0,
    "TRTSDT": TRTSDT,
    "trtp_short": "X-low",
    "SAFFL": "Y",
    "PARAM": "Diastolic Blood Pressure (mmHg)",
    "ATPT": "AFTER LYING DOWN FOR 5 MINUTES",
}

#: LMM-TRN-07 columns for a :func:`make_rows` frame: ``AVISIT``, the GRPVARS, ORDVARS and INCL
#: variables in input order, ``AVAL ADT BASE CHG`` and the flag.
MAKE_ROWS_OUTPUT = (
    "AVISIT",
    "STUDYID",
    "USUBJID",
    "TRTPN",
    "PARAMCD",
    "ATPTN",
    "AVISITN",
    "trtp_short",
    "SAFFL",
    "PARAM",
    "ATPT",
    "AVAL",
    "ADT",
    "BASE",
    "CHG",
    "ANL02FL",
)

#: The derived variables of each output record (LMM-TRN-05, LMM-TRN-06) plus the selected values.
DERIVED = ("AVISIT", "AVISITN", "AVAL", "ADT", "BASE", "CHG")

#: The input variables of :func:`make_rows` and four further ADVS variables, in their order in
#: ``data/adam/cdisc-split/advs.xpt`` with ``trtp_short`` appended, as in ``WORK.ADVS_SUB``.
_ADVS_ORDER = (
    "STUDYID",
    "USUBJID",
    "SAFFL",
    "TRTSDT",
    "TRTP",
    "TRTPN",
    "PARAMCD",
    "PARAM",
    "ADT",
    "ATPTN",
    "ATPT",
    "AVISIT",
    "AVISITN",
    "AVAL",
    "BASE",
    "CHG",
    "ANL01FL",
    "trtp_short",
)


def visit(adt: float, aval: float, avisitn: float, **fields: object) -> dict[str, object]:
    """One input record: its ``ADT`` (SAS day number), ``AVAL`` and ``AVISITN``, plus overrides."""
    return {"ADT": adt, "AVAL": aval, "AVISITN": avisitn, **fields}


def make_rows(*rows: Mapping[str, object]) -> pd.DataFrame:
    """A frame of :data:`INPUT_COLUMNS`, one row per mapping, defaults filling unnamed variables.

    Numeric variables are float64, as ``pyreadstat`` reads them, with NaN for a missing value. An
    unnamed ``AVISIT`` is ``Visit <AVISITN>``; the derivation replaces it on every output record.
    """
    records = []
    for row in rows:
        unknown = set(row) - set(INPUT_COLUMNS)
        if unknown:
            raise ValueError(f"make_rows: unknown variables {sorted(unknown)}")
        record = {**_DEFAULTS, **row}
        record.setdefault("AVISIT", f"Visit {record['AVISITN']:g}")
        records.append(record)
    frame = pd.DataFrame(records, columns=list(INPUT_COLUMNS))
    return frame.astype({name: "float64" for name in _NUMERIC_INPUTS})


def mode_rows() -> pd.DataFrame:
    """Subject A with three baseline and three post-baseline records (``TRTSDT`` 100).

    MIN, MAX and LAST each select a different record per phase:

    ======== ===== ==== ======= ===========================
    phase    ADT   AVAL AVISITN selected by
    ======== ===== ==== ======= ===========================
    baseline 90    80   0       MIN
    baseline 95    85   1       MAX
    baseline 100   82   2       LAST (``ADT == TRTSDT``)
    post     110   72   3       MIN
    post     120   90   4       MAX
    post     130   78   5       LAST
    ======== ===== ==== ======= ===========================
    """
    return make_rows(
        visit(90, 80, 0),
        visit(95, 85, 1),
        visit(100, 82, 2),
        visit(110, 72, 3),
        visit(120, 90, 4),
        visit(130, 78, 5),
    )


#: Expected derived values on :func:`mode_rows` per mode, for mode index 1.
MIN_RECORDS = [
    {"AVISIT": "Baseline (MIN)", "AVISITN": 9911, "AVAL": 80, "ADT": 90, "BASE": 80, "CHG": 0},
    {
        "AVISIT": "Post-baseline (MIN)",
        "AVISITN": 9912,
        "AVAL": 72,
        "ADT": 110,
        "BASE": 80,
        "CHG": -8,
    },
]
MAX_RECORDS = [
    {"AVISIT": "Baseline (MAX)", "AVISITN": 9911, "AVAL": 85, "ADT": 95, "BASE": 85, "CHG": 0},
    {
        "AVISIT": "Post-baseline (MAX)",
        "AVISITN": 9912,
        "AVAL": 90,
        "ADT": 120,
        "BASE": 85,
        "CHG": 5,
    },
]
LAST_RECORDS = [
    {"AVISIT": "Baseline (LAST)", "AVISITN": 9911, "AVAL": 82, "ADT": 100, "BASE": 82, "CHG": 0},
    {
        "AVISIT": "Post-baseline (LAST)",
        "AVISITN": 9912,
        "AVAL": 78,
        "ADT": 130,
        "BASE": 82,
        "CHG": -4,
    },
]


def derive(
    frame: pd.DataFrame,
    c_modes: str | Sequence[str] = "MIN",
    flvars: str | Sequence[str] = "ANL02FL",
) -> tuple[pd.DataFrame, derivation.LmmCounts]:
    """``derive_lastminmax_measure`` with the S07-TRN-03 GRPVARS, ORDVARS and INCL."""
    return derivation.derive_lastminmax_measure(
        frame, c_modes, flvars=flvars, grpvars=LMM_GRPVARS, ordvars=LMM_ORDVARS, incl=LMM_INCL
    )


def derived_values(dsout: pd.DataFrame) -> list[dict[str, object]]:
    """The :data:`DERIVED` values of every output record, in output order."""
    return dsout.loc[:, list(DERIVED)].to_dict("records")


def derivation_warnings(caplog: pytest.LogCaptureFixture) -> list[str]:
    """The WARNING messages logged by ``wpct.derivation``."""
    return [
        record.getMessage()
        for record in caplog.records
        if record.name == LOGGER and record.levelno == logging.WARNING
    ]


# --- LMM-FLT-01, LMM-TRN-01: record deletion and phase split --------------------------------------


@pytest.mark.parametrize("missing", ["ADT", "TRTSDT", "AVAL"])
def test_lmm_flt_01_missing_values_deleted(missing: str) -> None:
    """LMM-FLT-01: a record lacking ``ADT``, ``TRTSDT`` or ``AVAL`` is deleted before the split.

    ``if n(adt, trtsdt) < 2 or missing(aval) then delete``
    [whitepapers/ADaM/derive_lastminmax_measure.sas:92]. Subject A's incomplete records would be its
    minimum in either phase if kept; subject B has incomplete records only, so it never forms a
    group and no omission is logged for it.
    """
    gone = {missing: math.nan}
    frame = make_rows(
        visit(90, 80, 0),
        visit(95, 10, 1, **gone),
        visit(110, 75, 2),
        visit(120, 5, 3, **gone),
        visit(90, 70, 0, USUBJID=SUBJECT_B, **gone),
        visit(110, 60, 2, USUBJID=SUBJECT_B, **gone),
    )

    dsout, counts = derive(frame)

    assert (counts.LMM_BASE, counts.LMM_POST) == (1, 1)
    (mode,) = counts.modes
    assert (mode.SQL_BASE, mode.SQL_POST) == (1, 1)
    assert (mode.OMITTED_BASE, mode.OMITTED_POST) == (0, 0)
    assert counts.DSOUT == len(dsout) == 2
    assert dsout["USUBJID"].tolist() == [SUBJECT_A, SUBJECT_A]
    assert derived_values(dsout) == [
        {"AVISIT": "Baseline (MIN)", "AVISITN": 9911, "AVAL": 80, "ADT": 90, "BASE": 80, "CHG": 0},
        {
            "AVISIT": "Post-baseline (MIN)",
            "AVISITN": 9912,
            "AVAL": 75,
            "ADT": 110,
            "BASE": 80,
            "CHG": -5,
        },
    ]


def test_lmm_trn_01_baseline_split() -> None:
    """LMM-TRN-01: ``ADT <= TRTSDT`` is baseline, a later ``ADT`` post-baseline, per record.

    ``if adt <= trtsdt then OUTPUT LMM_BASE; else OUTPUT LMM_POST``
    [whitepapers/ADaM/derive_lastminmax_measure.sas:94-95]. Subject A's minimum baseline value sits
    on ``ADT == TRTSDT``; subject B starts treatment on day 95, so its day-95 record is baseline and
    its day-96 record post-baseline.
    """
    frame = make_rows(
        visit(90, 80, 0),
        visit(100, 70, 1),
        visit(101, 85, 2),
        visit(95, 60, 0, USUBJID=SUBJECT_B, TRTSDT=95.0),
        visit(96, 66, 2, USUBJID=SUBJECT_B, TRTSDT=95.0),
    )

    dsout, counts = derive(frame)

    assert (counts.LMM_BASE, counts.LMM_POST) == (3, 2)
    assert counts.DSOUT == len(dsout) == 4
    assert dsout["USUBJID"].tolist() == [SUBJECT_A, SUBJECT_B, SUBJECT_A, SUBJECT_B]
    assert dsout.loc[:, ["AVISITN", "AVAL", "ADT", "BASE", "CHG"]].to_dict("records") == [
        {"AVISITN": 9911, "AVAL": 70, "ADT": 100, "BASE": 70, "CHG": 0},
        {"AVISITN": 9911, "AVAL": 60, "ADT": 95, "BASE": 60, "CHG": 0},
        {"AVISITN": 9912, "AVAL": 85, "ADT": 101, "BASE": 70, "CHG": 15},
        {"AVISITN": 9912, "AVAL": 66, "ADT": 96, "BASE": 60, "CHG": 6},
    ]


# --- LMM-TRN-03, LMM-TRN-04, D-15, U-01: tied extremum records -----------------------------------


def test_lmm_trn_03_04_min_ties_ordered_by_avisitn() -> None:
    """LMM-TRN-03..06 (D-15): tied minima all survive SQL, and NODUPKEY keeps the lowest AVISITN.

    PROC SQL ``having min(aval) = aval order by GRPVARS, AVISITN`` keeps both post-baseline records
    with ``AVAL`` 70 [whitepapers/ADaM/derive_lastminmax_measure.sas:133-147], and PROC SORT
    NODUPKEY keeps the first, ``AVISITN`` 2 (day 110), although day 120 comes first in the input
    [whitepapers/ADaM/derive_lastminmax_measure.sas:150-154]. The output carries the derived
    ``AVISIT``, ``AVISITN``, ``BASE`` and ``CHG``
    [whitepapers/ADaM/derive_lastminmax_measure.sas:171-174,191-193], the GRPVARS and INCL values
    unchanged, and the flag [whitepapers/ADaM/derive_lastminmax_measure.sas:197-206].
    """
    frame = make_rows(
        visit(90, 80, 0),
        visit(100, 75, 1),
        visit(120, 70, 3),
        visit(110, 70, 2),
    )

    dsout, counts = derive(frame)

    assert (counts.LMM_BASE, counts.LMM_POST) == (2, 2)
    (mode,) = counts.modes
    assert (mode.SQL_BASE, mode.SQL_POST) == (1, 2)
    assert (mode.NODUPKEY_BASE, mode.NODUPKEY_POST) == (1, 1)
    assert (mode.MERGED_BASE, mode.MERGED_POST) == (1, 1)
    assert counts.DSOUT == len(dsout) == 2
    assert derived_values(dsout) == [
        {
            "AVISIT": "Baseline (MIN)",
            "AVISITN": 9911,
            "AVAL": 75,
            "ADT": 100,
            "BASE": 75,
            "CHG": 0,
        },
        {
            "AVISIT": "Post-baseline (MIN)",
            "AVISITN": 9912,
            "AVAL": 70,
            "ADT": 110,
            "BASE": 75,
            "CHG": -5,
        },
    ]
    carried = [*LMM_GRPVARS, *LMM_INCL]
    assert (
        dsout.loc[:, carried].to_dict("records")
        == [{name: _DEFAULTS[name] for name in carried}] * 2
    )
    assert dsout["ANL02FL"].tolist() == ["Y", "Y"]


@pytest.mark.parametrize(
    ("first_adt", "second_adt"),
    [
        pytest.param(115.0, 110.0, id="later_date_first"),
        pytest.param(110.0, 115.0, id="earlier_date_first"),
    ],
)
def test_u01_ties_equal_on_all_order_keys_keep_input_order(
    first_adt: float, second_adt: float
) -> None:
    """U-01 (D-15): among records equal on every ORDER BY key, the earlier input record survives.

    SQL leaves their order undefined [whitepapers/ADaM/derive_lastminmax_measure.sas:133-147] and
    NODUPKEY keeps the first [whitepapers/ADaM/derive_lastminmax_measure.sas:150-154]; the port's
    stated tie rule is input order. Both post-baseline records share ``AVISITN`` 2 and ``AVAL`` 70
    and differ only in ``ADT``, so the kept ``ADT`` is whichever came first, never the earlier date.
    """
    frame = make_rows(
        visit(90, 80, 0),
        visit(first_adt, 70, 2),
        visit(second_adt, 70, 2),
    )

    dsout, counts = derive(frame)

    (mode,) = counts.modes
    assert (mode.SQL_POST, mode.NODUPKEY_POST) == (2, 1)
    assert counts.DSOUT == len(dsout) == 2
    assert dsout["AVISITN"].tolist() == [9911, 9912]
    assert dsout["ADT"].tolist() == [90, first_adt]
    assert dsout["CHG"].tolist() == [0, -10]


# --- LMM-FLT-02: both phases required -------------------------------------------------------------


#: The SAS warning texts up to ``such as`` [whitepapers/ADaM/derive_lastminmax_measure.sas:167,187].
OMIT_NO_POST = "(DERIVE_LASTMINMAX_MEASURES) Omitting obs without a post-baseline measure, such as "
OMIT_NO_BASE = "(DERIVE_LASTMINMAX_MEASURES) Omitting obs without a baseline measure, such as "

#: Variables of an omitted record that SAS ``PUT _ALL_`` prints and a port warning never names.
#: ``PARAM`` and ``ATPT`` are prefixes of the named ``PARAMCD`` and ``ATPTN``, so they carry ``=``.
UNLOGGED_NAMES = (
    "AVAL",
    "ADT",
    "TRTSDT",
    "TRTPN",
    "trtp_short",
    "SAFFL",
    "AVISITN",
    "PARAM=",
    "ATPT=",
)


def unlogged_values(record: Mapping[str, object]) -> list[str]:
    """The measurement, date, treatment and flag values of ``record``, as a warning shows them."""
    numbers = [f"{record[name]:g}" for name in ("AVAL", "ADT", "TRTSDT", "TRTPN")]
    texts = [str(record[name]) for name in ("trtp_short", "SAFFL", "PARAM", "ATPT")]
    return numbers + texts


@pytest.mark.parametrize(
    ("subject_b", "context_b"),
    [
        pytest.param(
            {},
            "STUDYID='CDISCPILOT01' USUBJID='01-701-1023' PARAMCD='DIABP' ATPTN=815",
            id="identifiers",
        ),
        pytest.param(
            {"ATPTN": math.nan},
            "STUDYID='CDISCPILOT01' USUBJID='01-701-1023' PARAMCD='DIABP' ATPTN=.",
            id="missing_atptn",
        ),
        pytest.param(
            {"USUBJID": f"{SUBJECT_B}\nWARNING: forged"},
            "STUDYID='CDISCPILOT01' USUBJID='01-701-1023\\nWARNING: forged' "
            "PARAMCD='DIABP' ATPTN=815",
            id="line_break_escaped",
        ),
    ],
)
def test_lmm_flt_02_both_phases_required(
    subject_b: Mapping[str, object], context_b: str, caplog: pytest.LogCaptureFixture
) -> None:
    """LMM-FLT-02, LMM-TRN-06: only groups with both phases survive; each omission is logged.

    Subject A has both phases at timepoints 815 and 817, two groups. Subject B has a baseline record
    only: the first merge deletes it with "Omitting obs without a post-baseline measure"
    [whitepapers/ADaM/derive_lastminmax_measure.sas:159-169]. Subject C has a post-baseline record
    only: the second merge deletes it with "Omitting obs without a baseline measure"
    [whitepapers/ADaM/derive_lastminmax_measure.sas:177-189]. Each post-baseline record takes
    ``BASE`` from its own group's baseline record
    [whitepapers/ADaM/derive_lastminmax_measure.sas:180-182,193], and the baseline records precede
    the post-baseline records, each in GRPVARS order.

    Where SAS ``PUT _ALL_`` prints the whole record, each warning names only the group's
    ``STUDYID``, ``USUBJID``, ``PARAMCD`` and ``ATPTN``: never a measurement, date, treatment or
    flag. A missing number reads ``.``, and a line break in a value is escaped, not written.
    """
    caplog.set_level(logging.WARNING, logger=LOGGER)
    record_b = visit(90, 85, 0, **{"USUBJID": SUBJECT_B, **subject_b})
    record_c = visit(110, 70, 2, USUBJID=SUBJECT_C)
    frame = make_rows(
        visit(90, 80, 0),
        visit(110, 72, 2),
        visit(90, 90, 0, ATPTN=817.0),
        visit(110, 95, 2, ATPTN=817.0),
        record_b,
        record_c,
    )

    dsout, counts = derive(frame)

    assert (counts.LMM_BASE, counts.LMM_POST) == (3, 3)
    (mode,) = counts.modes
    assert (mode.SQL_BASE, mode.SQL_POST) == (3, 3)
    assert (mode.NODUPKEY_BASE, mode.NODUPKEY_POST) == (3, 3)
    assert (mode.OMITTED_BASE, mode.OMITTED_POST) == (1, 1)
    assert (mode.MERGED_BASE, mode.MERGED_POST) == (2, 2)
    assert counts.DSOUT == len(dsout) == 4
    assert dsout.loc[:, ["USUBJID", "ATPTN", "AVISITN", "AVAL", "BASE", "CHG"]].to_dict(
        "records"
    ) == [
        {"USUBJID": SUBJECT_A, "ATPTN": 815, "AVISITN": 9911, "AVAL": 80, "BASE": 80, "CHG": 0},
        {"USUBJID": SUBJECT_A, "ATPTN": 817, "AVISITN": 9911, "AVAL": 90, "BASE": 90, "CHG": 0},
        {"USUBJID": SUBJECT_A, "ATPTN": 815, "AVISITN": 9912, "AVAL": 72, "BASE": 80, "CHG": -8},
        {"USUBJID": SUBJECT_A, "ATPTN": 817, "AVISITN": 9912, "AVAL": 95, "BASE": 90, "CHG": 5},
    ]

    warnings = derivation_warnings(caplog)
    assert warnings == [
        OMIT_NO_POST + context_b,
        OMIT_NO_BASE + "STUDYID='CDISCPILOT01' USUBJID='01-701-1028' PARAMCD='DIABP' ATPTN=815",
    ]
    omitted = ({**_DEFAULTS, **record_b}, {**_DEFAULTS, **record_c})
    for text, record in zip(warnings, omitted, strict=True):
        assert "\n" not in text
        assert [name for name in UNLOGGED_NAMES if name in text] == []
        assert [
            value
            for value in unlogged_values(record)
            if re.search(rf"(?<![\w-]){re.escape(value)}(?![\w-])", text)
        ] == []


# --- LMM-TRN-02: change modes and flags -----------------------------------------------------------


def test_lmm_trn_02_max_mode() -> None:
    """LMM-TRN-02: ``MAX`` keeps the largest ``AVAL`` of each phase.

    ``%if &nxtcm = MAX %then %let lmm_func = MAX`` with ``lmm_var = AVAL``
    [whitepapers/ADaM/derive_lastminmax_measure.sas:119-122]; see :func:`mode_rows`.
    """
    dsout, counts = derive(mode_rows(), "MAX", "ANL03FL")

    (mode,) = counts.modes
    assert (mode.MODE, mode.FLAG, mode.IDX) == ("MAX", "ANL03FL", 1)
    assert (mode.SQL_BASE, mode.SQL_POST) == (1, 1)
    assert counts.DSOUT == len(dsout) == 2
    assert derived_values(dsout) == MAX_RECORDS
    assert dsout["ANL03FL"].tolist() == ["Y", "Y"]


def test_lmm_trn_02_last_mode(caplog: pytest.LogCaptureFixture) -> None:
    """LMM-TRN-02, LMM-TRN-07: ``LAST`` keeps the latest ``ADT`` of each phase, untruncated.

    ``LAST`` is ``MAX(ADT)`` [whitepapers/ADaM/derive_lastminmax_measure.sas:123-128] and is valid,
    so no warning is logged [whitepapers/ADaM/derive_lastminmax_measure.sas:124]. Its post-baseline
    ``AVISIT`` ``Post-baseline (LAST)`` has 20 characters, the length the macro sets when the input
    ``AVISIT`` is shorter than 20 and the last input record survives LMM-FLT-01
    [whitepapers/ADaM/derive_lastminmax_measure.sas:92-106].
    """
    caplog.set_level(logging.WARNING, logger=LOGGER)

    dsout, counts = derive(mode_rows(), "LAST", "ANL04FL")

    (mode,) = counts.modes
    assert (mode.MODE, mode.FLAG, mode.IDX) == ("LAST", "ANL04FL", 1)
    assert (mode.SQL_BASE, mode.SQL_POST) == (1, 1)
    assert counts.DSOUT == len(dsout) == 2
    assert derived_values(dsout) == LAST_RECORDS
    assert [len(text) for text in dsout["AVISIT"]] == [15, 20]
    assert dsout["ANL04FL"].tolist() == ["Y", "Y"]
    assert derivation_warnings(caplog) == []


def test_lmm_trn_02_invalid_mode_defaults_to_last(caplog: pytest.LogCaptureFixture) -> None:
    """LMM-TRN-02: an invalid mode logs a warning and is processed as ``LAST``, flag ``ANLLASTFL``.

    ``%put WARNING: ... Invalid C_MODES value: &nxtcm..``, then ``nxtcm = LAST`` before the default
    flag ``anl&nxtcm.fl`` is formed [whitepapers/ADaM/derive_lastminmax_measure.sas:121-130]. The
    result equals that of an explicit ``LAST`` call.
    """
    caplog.set_level(logging.WARNING, logger=LOGGER)

    dsout, counts = derive(mode_rows(), "FOO", ())

    warnings = derivation_warnings(caplog)
    assert len(warnings) == 1
    assert "Invalid C_MODES value: FOO" in warnings[0]
    assert "Defaulting to LAST" in warnings[0]
    (mode,) = counts.modes
    assert mode.MODE == "LAST"
    assert mode.FLAG.upper() == "ANLLASTFL"
    assert mode.IDX == 1
    assert counts.DSOUT == len(dsout) == 2
    (flag,) = [name for name in dsout.columns if name.upper() == "ANLLASTFL"]
    assert dsout[flag].tolist() == ["Y", "Y"]
    assert derived_values(dsout) == LAST_RECORDS

    last_dsout, last_counts = derive(mode_rows(), "LAST", ())
    assert last_counts == counts
    pd.testing.assert_frame_equal(dsout, last_dsout, check_exact=True)
    assert len(derivation_warnings(caplog)) == 1


INVALID_BOGUS = (
    "(DERIVE_LASTMINMAX_MEASURES) Invalid C_MODES value: BOGUS. "
    "Defaulting to LAST (rather than MIN or MAX)."
)
OMITTED_B = OMIT_NO_POST + "STUDYID='CDISCPILOT01' USUBJID='01-701-1023' PARAMCD='DIABP' ATPTN=815"


@pytest.mark.parametrize(
    ("rows", "omitted", "expected"),
    [
        pytest.param(
            (visit(90, 80, 0), visit(110, 72, 2), visit(90, 85, 0, USUBJID=SUBJECT_B)),
            1,
            [OMITTED_B, INVALID_BOGUS, OMITTED_B],
            id="omission_per_mode",
        ),
        pytest.param((), 0, [INVALID_BOGUS], id="no_rows"),
    ],
)
def test_lmm_trn_02_invalid_mode_warned_in_mode_order(
    rows: tuple[dict[str, object], ...],
    omitted: int,
    expected: list[str],
    caplog: pytest.LogCaptureFixture,
) -> None:
    """LMM-TRN-02, LMM-FLT-02, U-03: an invalid mode's warning comes when the mode loop reaches it.

    The ``%put WARNING`` for an invalid token sits inside that token's ``%do %while`` pass
    [whitepapers/ADaM/derive_lastminmax_measure.sas:115-127], so with ``MIN bogus`` the MIN pass's
    omission warning for subject B comes first, then the upper-cased invalid-mode warning, then the
    LAST pass's omission warning. The loop runs whatever the data, so an input without records
    still logs the invalid-mode warning, once.
    """
    caplog.set_level(logging.WARNING, logger=LOGGER)

    _dsout, counts = derive(make_rows(*rows), "MIN bogus")

    assert [(mode.MODE, mode.FLAG, mode.IDX) for mode in counts.modes] == [
        ("MIN", "ANL02FL", 1),
        ("LAST", "ANLLASTFL", 2),
    ]
    assert [(mode.OMITTED_BASE, mode.OMITTED_POST) for mode in counts.modes] == [(omitted, 0)] * 2
    assert derivation_warnings(caplog) == expected


def test_lmm_trn_02_lowercase_mode_and_default_flag() -> None:
    """LMM-TRN-02: modes are upper-cased, and a mode without a FLVARS name gets ``ANL<mode>FL``.

    ``%let c_modes = %upcase(&c_modes)`` [whitepapers/ADaM/derive_lastminmax_measure.sas:81] and
    ``%if %length(&nxtflv) = 0 %then %let nxtflv = anl&nxtcm.fl``
    [whitepapers/ADaM/derive_lastminmax_measure.sas:130], so ``min`` writes ``Baseline (MIN)``.
    """
    dsout, counts = derive(mode_rows(), "min", ())

    (mode,) = counts.modes
    assert mode.MODE == "MIN"
    assert mode.FLAG.upper() == "ANLMINFL"
    assert counts.DSOUT == len(dsout) == 2
    (flag,) = [name for name in dsout.columns if name.upper() == "ANLMINFL"]
    assert dsout[flag].tolist() == ["Y", "Y"]
    assert derived_values(dsout) == MIN_RECORDS


# --- LMM-TRN-07: output structure -----------------------------------------------------------------


@pytest.mark.parametrize(
    ("c_modes", "flvars"),
    [
        pytest.param("MIN MAX", "ANL02FL ANL03FL", id="blank_delimited"),
        pytest.param(("MIN", "MAX"), ("ANL02FL", "ANL03FL"), id="sequences"),
    ],
)
def test_lmm_trn_07_multiple_modes_in_order(
    c_modes: str | tuple[str, ...], flvars: str | tuple[str, ...]
) -> None:
    """LMM-TRN-07, LMM-TRN-02: modes append in order, idx raising ``AVISITN`` by 10 per mode.

    Each pass of the ``%do %while`` loop appends its baseline then post-baseline records and sets
    its own flag to ``'Y'``; the other mode's flag stays blank on them
    [whitepapers/ADaM/derive_lastminmax_measure.sas:113-117,171-174,191-206]. MIN rows come first
    with ``AVISITN`` 9911/9912, then MAX rows with 9921/9922.
    """
    dsout, counts = derive(mode_rows(), c_modes, flvars)

    assert [(mode.MODE, mode.FLAG, mode.IDX) for mode in counts.modes] == [
        ("MIN", "ANL02FL", 1),
        ("MAX", "ANL03FL", 2),
    ]
    assert [(mode.MERGED_BASE, mode.MERGED_POST) for mode in counts.modes] == [(1, 1), (1, 1)]
    assert (counts.LMM_BASE, counts.LMM_POST) == (3, 3)
    assert counts.DSOUT == len(dsout) == 4
    assert list(dsout.columns[-2:]) == ["ANL02FL", "ANL03FL"]
    assert derived_values(dsout) == [
        MIN_RECORDS[0],
        MIN_RECORDS[1],
        {**MAX_RECORDS[0], "AVISITN": 9921},
        {**MAX_RECORDS[1], "AVISITN": 9922},
    ]
    min_flag = dsout["ANL02FL"].tolist()
    max_flag = dsout["ANL03FL"].tolist()
    assert min_flag[:2] == ["Y", "Y"] and all(is_char_missing(value) for value in min_flag[2:])
    assert max_flag[2:] == ["Y", "Y"] and all(is_char_missing(value) for value in max_flag[:2])


@pytest.mark.parametrize(
    ("input_order", "extra", "expected"),
    [
        pytest.param(INPUT_COLUMNS, {}, MAKE_ROWS_OUTPUT, id="make_rows_order"),
        # WORK.ADVS_MIN has 16 variables [whitepapers/WPCT/outputs_sas/WPCT-F.07.07.log:205]; the
        # export schema holds its order. ADVS_SUB also carries ADVS's own BASE and CHG, which the
        # derived values replace.
        pytest.param(
            _ADVS_ORDER,
            {"TRTP": "Xanomeline Low Dose", "BASE": 999.0, "CHG": 999.0, "ANL01FL": "Y"},
            export_spec(PROGRAM_07_07, "advs_min").columns,
            id="advs_order",
        ),
    ],
)
def test_lmm_trn_07_output_columns(
    input_order: tuple[str, ...], extra: Mapping[str, object], expected: tuple[str, ...]
) -> None:
    """LMM-TRN-07, S07-TRN-03: ``AVISIT``, the kept variables in input order, ``AVAL ADT BASE CHG``.

    The structure step keeps ``AVISIT`` then GRPVARS, ORDVARS and INCL, and ``KEEP=`` does not
    reorder [whitepapers/ADaM/derive_lastminmax_measure.sas:105-110]; the appended records add
    ``AVAL ADT`` (SQL select list), ``BASE CHG`` and the flag
    [whitepapers/ADaM/derive_lastminmax_measure.sas:135,173-174,193,203]. Input variables outside
    the call, such as ``TRTSDT``, are dropped, and the input frame is left unchanged.
    """
    frame = make_rows(visit(90, 80, 0), visit(110, 75, 2)).assign(**extra)
    frame = frame.loc[:, list(input_order)]
    before = frame.copy(deep=True)

    dsout, counts = derive(frame)

    assert counts.DSOUT == len(dsout) == 2
    assert len(expected) == 16
    assert tuple(dsout.columns) == tuple(expected)
    for name in ("TRTPN", "ATPTN", "AVISITN", "AVAL", "ADT", "BASE", "CHG"):
        assert dsout[name].dtype == "float64", name
    assert dsout["BASE"].tolist() == [80, 80]
    assert dsout["CHG"].tolist() == [0, -5]
    pd.testing.assert_frame_equal(frame, before, check_exact=True)


@pytest.mark.parametrize(
    "rows",
    [
        pytest.param((), id="no_rows"),
        pytest.param(
            (visit(90, math.nan, 0), visit(math.nan, 75, 2), visit(110, 70, 2, TRTSDT=math.nan)),
            id="all_rows_deleted",
        ),
    ],
)
def test_empty_input(rows: tuple[dict[str, object], ...], caplog: pytest.LogCaptureFixture) -> None:
    """U-03 guard, LMM-TRN-07: no usable input yields an empty frame with the output columns.

    SAS behaviour for an input without records is not established: ``avlen`` stays blank when
    ``%if &avlen < 20`` is evaluated [whitepapers/ADaM/derive_lastminmax_measure.sas:98-102]. The
    port returns no records, the LMM-TRN-07 columns with their populated dtypes and zero counts,
    without raising or warning; the same holds when LMM-FLT-01 deletes every record
    [whitepapers/ADaM/derive_lastminmax_measure.sas:92].
    """
    caplog.set_level(logging.WARNING, logger=LOGGER)

    dsout, counts = derive(make_rows(*rows))

    assert (counts.LMM_BASE, counts.LMM_POST, counts.DSOUT) == (0, 0, 0)
    assert len(dsout) == 0
    assert tuple(dsout.columns) == MAKE_ROWS_OUTPUT
    for name in ("AVISITN", "AVAL", "ADT", "BASE", "CHG"):
        assert dsout[name].dtype == "float64", name
    (mode,) = counts.modes
    assert dataclasses.asdict(mode) == {
        "MODE": "MIN",
        "FLAG": "ANL02FL",
        "IDX": 1,
        "SQL_BASE": 0,
        "SQL_POST": 0,
        "NODUPKEY_BASE": 0,
        "NODUPKEY_POST": 0,
        "OMITTED_BASE": 0,
        "OMITTED_POST": 0,
        "MERGED_BASE": 0,
        "MERGED_POST": 0,
    }
    assert derivation_warnings(caplog) == []


def _dates_as_text(frame: pd.DataFrame) -> pd.DataFrame:
    """``frame`` with ``ADT`` as text rather than a SAS day number."""
    return frame.assign(ADT=frame["ADT"].map(lambda day: f"{day:g}"))


@pytest.mark.parametrize(
    ("alter", "c_modes", "error", "fragment"),
    [
        pytest.param(lambda frame: frame, "", ValueError, "c_modes", id="no_change_mode"),
        pytest.param(
            lambda frame: frame.drop(columns="TRTSDT"), "MIN", KeyError, "TRTSDT", id="no_trtsdt"
        ),
        pytest.param(_dates_as_text, "MIN", TypeError, "ADT", id="adt_not_day_number"),
    ],
)
def test_lmm_trn_02_and_flt_01_invalid_arguments_raise(
    alter: Callable[[pd.DataFrame], pd.DataFrame],
    c_modes: str,
    error: type[Exception],
    fragment: str,
) -> None:
    """LMM-TRN-02, LMM-FLT-01 (D-10): a call the macro could not run raises a named error.

    No change mode leaves the ``%do %while`` loop nothing to process
    [whitepapers/ADaM/derive_lastminmax_measure.sas:115]; ``TRTSDT`` is read by the phase split
    [whitepapers/ADaM/derive_lastminmax_measure.sas:92-95]; and dates must be SAS day numbers.
    """
    frame = alter(make_rows(visit(90, 80, 0), visit(110, 75, 2)))

    with pytest.raises(error, match=fragment):
        derive(frame, c_modes)


def test_lmm_trn_03_04_missing_and_blank_padded_keys_match() -> None:
    """LMM-TRN-03, LMM-TRN-04, LMM-FLT-02 (D-11): missing and blank-padded GRPVARS values match.

    SAS GROUP BY, PROC SORT ``NODUPKEY BY`` and MERGE ``BY`` treat every missing value of a variable
    as equal and ignore trailing blanks [whitepapers/ADaM/derive_lastminmax_measure.sas:133-189].
    Subject A's records have a missing ``TRTPN`` and a ``STUDYID`` that is ``''``, ``None`` or
    blank, so they form one group per phase. Subject B's ``USUBJID`` is blank-padded on some
    records, so its padded and unpadded records form one group, and its unpadded baseline record
    matches its padded post-baseline record. Split groups would raise the SQL and NODUPKEY counts.
    """
    padded = SUBJECT_B + "  "
    frame = make_rows(
        visit(90, 80, 0, STUDYID="", TRTPN=math.nan),
        visit(95, 78, 1, STUDYID=None, TRTPN=math.nan),
        visit(110, 72, 2, STUDYID="  ", TRTPN=math.nan),
        visit(120, 72, 3, STUDYID="", TRTPN=math.nan),
        visit(90, 83, 0, USUBJID=SUBJECT_B),
        visit(95, 85, 1, USUBJID=padded),
        visit(120, 70, 3, USUBJID=SUBJECT_B),
        visit(110, 70, 2, USUBJID=padded),
    )

    dsout, counts = derive(frame)

    assert (counts.LMM_BASE, counts.LMM_POST) == (4, 4)
    (mode,) = counts.modes
    assert (mode.SQL_BASE, mode.SQL_POST) == (2, 4)
    assert (mode.NODUPKEY_BASE, mode.NODUPKEY_POST) == (2, 2)
    assert (mode.OMITTED_BASE, mode.OMITTED_POST) == (0, 0)
    assert (mode.MERGED_BASE, mode.MERGED_POST) == (2, 2)
    assert counts.DSOUT == len(dsout) == 4
    assert dsout["TRTPN"].isna().tolist() == [True, False, True, False]
    columns = ["STUDYID", "USUBJID", "AVISITN", "AVAL", "ADT", "BASE", "CHG"]
    assert dsout.loc[:, columns].to_numpy().tolist() == [
        ["", SUBJECT_A, 9911, 78, 95, 78, 0],
        ["CDISCPILOT01", SUBJECT_B, 9911, 83, 90, 83, 0],
        ["", SUBJECT_A, 9912, 72, 110, 78, -6],
        ["CDISCPILOT01", padded, 9912, 70, 110, 83, -13],
    ]
