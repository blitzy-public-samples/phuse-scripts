"""End-to-end equivalence of the WPCT-F.07.02 port with its SAS reference outputs.

The driver ``wpct.programs.wpct_f_07_02`` runs on ``data/adam/cdisc/advs.xpt`` (the session
fixture ``result_07_02``), and ``wpct.compare`` checks it with ``python/wpct/tolerances.json``
against ``whitepapers/WPCT/outputs_sas/WPCT-F.07.02.lst`` and ``WPCT-F.07.02.log``: 15 log
checkpoints, 40 listing ParameterEstimates values and 6 merged ``pval`` cells, 46 listing-based
values in all. Every SAS value compared is parsed from those files at run time, and no listing value
is written here. The record counts asserted directly are the ones the SAS log pins.

No silent pass: every equivalence test asserts the exact number of comparison rows that ran and that
each one is PASS, so neither a FAIL nor a NOT COMPARED row can satisfy it.

The edge-state tests run the driver again with other settings or on an altered private copy of
ADVS. Nothing here writes a file: the driver never writes, and no export or report is produced.
"""

from __future__ import annotations

import math
from collections.abc import Iterable, Mapping

import pandas as pd
import pytest

from wpct import compare, model, sas_reference
from wpct.programs import MODEL_ENDPOINT, ProgramResult, wpct_f_07_02
from wpct.schemas import ANA_VARIABLES, PROGRAM_07_02, STATISTICS, export_spec

PROGRAM = PROGRAM_07_02

#: The program's own settings (S02-TRN-01); ``e_visn`` is the endpoint visit, 99.
_DEFAULT = wpct_f_07_02.Config()

#: The strata of S02-TRN-04 in SAS loop order.
_STRATA = (("DIABP", 815.0), ("SYSBP", 816.0))

#: ``CLASS trtpn (ref="0")``: the reference level's label and value.
_REF_LEVEL = "0"
_REF_TRTPN = 0.0

#: The only subject whose analysis records lack BASE and CHG in the repository data.
_INCOMPLETE_SUBJECT = "01-718-1150"

#: Record key of ``css_anadata``, on which the analysis set is unique.
_ANADATA_KEY = ["STUDYID", "USUBJID", "PARAMCD", "ATPTN", "AVISITN"]

#: The reason ``wpct.compare`` gives for every model quantity of a run whose GLM is skipped.
_GLM_SKIPPED = "not produced: GLM skipped by setting"

_MODEL_GROUPS = (compare.GROUP_LISTING_PARAMETER_ESTIMATES, compare.GROUP_LISTING_MERGED_PVAL)

#: The record counts of WPCT-F.07.02.log in log order, as ``(SAS dataset, PARAMCD, ATPTN, NOBS,
#: NVARS)`` [whitepapers/WPCT/outputs_sas/WPCT-F.07.02.log:32-1471]. NVARS is ``None`` where the
#: checkpoint carries no variable count.
_EXPECTED_CHECKPOINTS = (
    ("ADVS", None, None, 32139, 44),
    ("ADVS_SUB", None, None, 5925, 45),
    ("CSS_ANADATA", None, None, 3548, 14),
    *(
        checkpoint
        for paramcd, atptn in _STRATA
        for checkpoint in (
            ("CSS_NEXTPARAM", paramcd, None, 1774, None),
            ("CSS_NEXTTIMEPT", paramcd, atptn, 1774, None),
            ("CSS_STATS", paramcd, atptn, 30, 12),
            ("CSS_PVALUES", paramcd, atptn, 5, None),
            ("CSS_PVALUES", paramcd, atptn, 3, None),
            ("CSS_STATS", paramcd, atptn, 30, 13),
        )
    ),
)


# --- Helpers --------------------------------------------------------------------------------------


def _group(groups: Iterable[compare.ComparisonGroup], key: str) -> compare.ComparisonGroup:
    """Return the one comparison group with ``key``."""
    matches = [group for group in groups if group.key == key]
    assert len(matches) == 1, f"{PROGRAM}: expected one {key!r} group, found {len(matches)}"
    return matches[0]


def _describe(rows: Iterable[compare.ComparisonRow]) -> str:
    return "\n".join(
        f"  {row.status}: {row.quantity} [{row.stratum}] SAS={row.sas_value} "
        f"Python={row.python_value} {row.reason}"
        for row in rows
    )


def assert_all_pass(group: compare.ComparisonGroup, expected_count: int) -> None:
    """Assert that exactly ``expected_count`` rows of ``group`` ran and that every one is PASS."""
    assert len(group.rows) == expected_count, (
        f"{group.program} {group.key}: {len(group.rows)} comparison rows ran, "
        f"expected {expected_count}"
    )
    failing = [row for row in group.rows if row.status != compare.PASS]
    assert not failing, (
        f"{group.program} {group.key}: {len(failing)} of {len(group.rows)} rows are not PASS:\n"
        f"{_describe(failing)}"
    )


def _stratum(frame: pd.DataFrame, paramcd: str, atptn: float) -> pd.DataFrame:
    return frame.loc[(frame["PARAMCD"] == paramcd) & (frame["ATPTN"] == atptn)]


def _strata_in_order(frame: pd.DataFrame) -> list[tuple[str, float]]:
    """The ``(PARAMCD, ATPTN)`` strata of ``frame`` in order of first appearance."""
    return list(dict.fromkeys(zip(frame["PARAMCD"], frame["ATPTN"], strict=True)))


@pytest.fixture(scope="module")
def groups_07_02(
    comparisons: Mapping[str, tuple[compare.ComparisonGroup, ...]],
) -> tuple[compare.ComparisonGroup, ...]:
    """Every comparison group of the default WPCT-F.07.02 run."""
    return comparisons[PROGRAM]


# --- Equivalence against the repository SAS references --------------------------------------------


def test_log_checkpoints_all_pass(
    result_07_02: ProgramResult,
    groups_07_02: tuple[compare.ComparisonGroup, ...],
    sas_checkpoints: Mapping[str, tuple[sas_reference.SasCheckpoint, ...]],
) -> None:
    """The 15 pinned record counts of the SAS log agree exactly.

    They evidence S02-FLT-01 and S02-TRN-02 (ADVS_SUB), S02-TRN-03, S02-POP-01, S02-FLT-02 and
    S02-FLT-03 (CSS_ANADATA), S02-TRN-04 (CSS_NEXTPARAM, CSS_NEXTTIMEPT), S02-MOD-01 (CSS_STATS
    30/12), S02-MOD-02 (CSS_PVALUES 5) and S02-TRN-05 (CSS_PVALUES 3, CSS_STATS 30/13).
    """
    pinned = sas_reference.LOG_CHECKPOINT_MAP[PROGRAM]
    assert len(pinned) == 15
    unverified = [sas.problem for sas in sas_checkpoints[PROGRAM] if not sas.ok]
    assert not unverified, f"pinned SAS log lines failed verification: {unverified}"

    assert_all_pass(_group(groups_07_02, compare.GROUP_LOG_CHECKPOINTS), 15)

    recorded = [
        (cp.SAS_DATASET, cp.PARAMCD, cp.ATPTN, cp.NOBS, cp.NVARS) for cp in result_07_02.checkpoints
    ]
    assert recorded == list(_EXPECTED_CHECKPOINTS)
    assert [
        (cp.SAS_DATASET, cp.OCCURRENCE, cp.PARAMCD, cp.ATPTN) for cp in result_07_02.checkpoints
    ] == [(spec.SAS_DATASET, spec.OCCURRENCE, spec.PARAMCD, spec.ATPTN) for spec in pinned]


def test_parameter_estimates_match_listing(
    groups_07_02: tuple[compare.ComparisonGroup, ...],
    listing_tables: Mapping[str, tuple[sas_reference.ListingTable, ...]],
) -> None:
    """S02-MOD-02: every ParameterEstimates value of the listing agrees at displayed precision.

    2 tables (DIABP/815, SYSBP/816) x 5 rows (Intercept, BASE, TRTPN 54, 81, 0) x 4 values
    (estimate, SE, t, p) = 40, in the SAS design-column order of D-05 and D-07, on the complete
    records of D-08.
    """
    tables = listing_tables[PROGRAM]
    assert [table.problems for table in tables] == [(), ()]
    assert [(table.spec.PARAMCD, table.spec.ATPTN) for table in tables] == list(_STRATA)
    assert [len(table.rows) for table in tables] == [5, 5]

    group = _group(groups_07_02, compare.GROUP_LISTING_PARAMETER_ESTIMATES)
    assert group.rounded
    assert_all_pass(group, 40)


def test_merged_pval_matches_listing(
    groups_07_02: tuple[compare.ComparisonGroup, ...],
    listing_tables: Mapping[str, tuple[sas_reference.ListingTable, ...]],
) -> None:
    """S02-TRN-05: the CSS_STATS ``pval`` cells the merge fills agree with the listing's Probt.

    3 TRTPN levels x 2 strata = 6 cells at the endpoint visit. The reference level's Probt is
    missing in SAS (D-07), so its two cells pass only as both missing.
    """
    group = _group(groups_07_02, compare.GROUP_LISTING_MERGED_PVAL)
    assert group.rounded
    assert group.heading_note == compare.MERGED_PVAL_NOTE
    assert_all_pass(group, 6)

    for table in listing_tables[PROGRAM]:
        reference = [row for row in table.rows if (row.EFFECT, row.LEVEL) == ("TRTPN", _REF_LEVEL)]
        assert len(reference) == 1
        # The listing prints a missing value as '.' or, after options missing=' ', as a blank.
        assert reference[0].PROBT.strip() in ("", ".")
    assert [row.python_value for row in group.rows].count(".") == 2


def test_listing_based_total_46(groups_07_02: tuple[compare.ComparisonGroup, ...]) -> None:
    """S02-MOD-02 and S02-TRN-05: 40 + 6 = 46 listing-based values, none NOT COMPARED."""
    listing = [_group(groups_07_02, key) for key in _MODEL_GROUPS]
    assert compare.count_statuses(listing) == {
        compare.PASS: 46,
        compare.FAIL: 0,
        compare.NOT_COMPARED: 0,
    }


def test_endpoint_models_use_226(result_07_02: ProgramResult) -> None:
    """S02-MOD-02, D-08: each endpoint model drops the one record missing BASE and CHG.

    Each parameter has 227 records at the endpoint visit, one of them subject 01-718-1150 with
    both BASE and CHG missing, so each fit uses 226 records with DFE 226 - 4 = 222.
    """
    models = {(record.PARAMCD, record.ATPTN): record for record in result_07_02.models}
    assert len(result_07_02.models) == 2
    assert list(models) == list(_STRATA)
    for record in result_07_02.models:
        assert (record.MODEL, record.STUDYNUM, record.NOBS, record.DFE, record.STATUS) == (
            MODEL_ENDPOINT,
            None,
            226,
            222,
            model.STATUS_FITTED,
        ), record

    anadata = result_07_02.dataset("css_anadata")
    endpoint = anadata.loc[anadata["AVISITN"] == _DEFAULT.e_visn]
    for paramcd, atptn in _STRATA:
        records = _stratum(endpoint, paramcd, atptn)
        assert len(records) == 227, (paramcd, atptn)
        incomplete = records.loc[records["BASE"].isna() | records["CHG"].isna()]
        assert list(incomplete["USUBJID"]) == [_INCOMPLETE_SUBJECT], (paramcd, atptn)
        assert incomplete["BASE"].isna().all()
        assert incomplete["CHG"].isna().all()
        assert len(records) - len(incomplete) == models[(paramcd, atptn)].NOBS


def test_missing_chg_rows_excluded_from_summary(result_07_02: ProgramResult) -> None:
    """D-03, S02-MOD-01: a missing CHG is left out of ``n``, and its BY group still forms.

    The 20 analysis records of subject 01-718-1150 (every post-baseline visit of both parameters)
    miss both BASE and CHG, and no record misses only one of them. Each stratum holds 1774 records,
    10 of them without CHG, so its ``n`` values sum to 1764.
    """
    anadata = result_07_02.dataset("css_anadata")
    stats = result_07_02.dataset("css_stats")

    base_missing = anadata["BASE"].isna()
    chg_missing = anadata["CHG"].isna()
    assert not (base_missing ^ chg_missing).any()
    subject = anadata.loc[anadata["USUBJID"] == _INCOMPLETE_SUBJECT]
    assert len(subject) == 20
    assert subject["BASE"].isna().all()
    assert subject["CHG"].isna().all()
    assert set(anadata.loc[chg_missing, "USUBJID"]) == {_INCOMPLETE_SUBJECT}

    for paramcd, atptn in _STRATA:
        records = _stratum(anadata, paramcd, atptn)
        assert len(records) == 1774, (paramcd, atptn)
        assert int(records["CHG"].isna().sum()) == 10, (paramcd, atptn)
        assert int(_stratum(stats, paramcd, atptn)["n"].sum()) == 1774 - 10, (paramcd, atptn)

    # Per BY group, n is the count of non-missing CHG among the group's records.
    by = ["PARAMCD", "ATPTN", "AVISITN", "TRTPN"]
    expected = anadata.groupby(by, sort=True)["CHG"].count()
    actual = stats.set_index(by)["n"].sort_index()
    pd.testing.assert_series_equal(actual, expected, check_names=False)


def test_pval_merge_mapping(result_07_02: ProgramResult) -> None:
    """S02-TRN-05: each endpoint row carries its own level's Probt; every other row is missing.

    The merge BY ``AVISITN TRTPN`` fills ``pval`` only at ``AVISITN = 99``. TRTPN 54 and 81 take
    exactly the PROBT of their stratum's ``TRTPN`` parameter row; the reference level, aliased by
    the Intercept and its siblings (D-05, D-07), has a missing Probt and so a missing ``pval``.
    """
    stats = result_07_02.dataset("css_stats")
    estimates = result_07_02.dataset("parameter_estimates")
    trt_rows = estimates.loc[estimates["EFFECT"] == "TRTPN"]
    probt = {
        (row.PARAMCD, row.ATPTN, float(row.LEVEL)): row.PROBT
        for row in trt_rows.itertuples(index=False)
    }
    assert len(probt) == 3 * len(_STRATA)

    at_endpoint = stats["AVISITN"] == _DEFAULT.e_visn
    assert stats.loc[~at_endpoint, "pval"].isna().all()

    endpoint_rows = stats.loc[at_endpoint]
    assert len(endpoint_rows) == 3 * len(_STRATA)
    for row in endpoint_rows.itertuples(index=False):
        level_probt = probt[(row.PARAMCD, row.ATPTN, row.TRTPN)]
        if row.TRTPN == _REF_TRTPN:
            assert math.isnan(level_probt), row
            assert math.isnan(row.pval), row
        else:
            assert not math.isnan(row.pval), row
            assert row.pval == level_probt, row


def test_s02_trn_04_strata_order(result_07_02: ProgramResult) -> None:
    """S02-TRN-04, S02-MOD-01: strata in loop order, rows sorted BY ``AVISITN TRTPN`` (D-04, D-14).

    ``css_stats`` stacks DIABP/815 then SYSBP/816, 30 rows each. Its columns are ``PARAMCD ATPTN``
    and the 13 SAS columns: the 4 BY variables, the 8 statistics and the merged ``pval``.
    """
    stats = result_07_02.dataset("css_stats")
    assert list(stats.columns) == list(export_spec(PROGRAM, "css_stats").columns)
    assert list(stats.columns[:2]) == ["PARAMCD", "ATPTN"]
    assert list(stats.columns[2:]) == [
        "AVISITN",
        "TRTPN",
        "AVISIT",
        "trtp_short",
        *STATISTICS,
        "pval",
    ]
    assert len(stats.columns[2:]) == 13

    assert _strata_in_order(stats) == list(_STRATA)
    assert _strata_in_order(result_07_02.dataset("parameter_estimates")) == list(_STRATA)
    for paramcd, atptn in _STRATA:
        block = _stratum(stats, paramcd, atptn)
        assert len(block) == 30, (paramcd, atptn)
        start = block.index[0]
        assert list(block.index) == list(range(start, start + 30)), "stratum rows not contiguous"
        assert not block[["AVISITN", "TRTPN"]].isna().any().any()
        keys = list(zip(block["AVISITN"], block["TRTPN"], strict=True))
        assert keys == sorted(keys), (paramcd, atptn)
        assert len(set(keys)) == 30, (paramcd, atptn)


def test_css_anadata_shape(result_07_02: ProgramResult) -> None:
    """S02-TRN-03 and the selections S02-FLT-01, S02-POP-01, S02-FLT-02, S02-FLT-03.

    3548 records of the 14 analysis variables, unique on ``STUDYID USUBJID PARAMCD ATPTN AVISITN``,
    without the baseline visit and with ``SAFFL = ANL01FL = 'Y'`` throughout.
    """
    anadata = result_07_02.dataset("css_anadata")
    assert anadata.shape == (3548, 14)
    assert list(anadata.columns) == list(export_spec(PROGRAM, "css_anadata").columns)
    assert sorted(anadata.columns) == sorted(ANA_VARIABLES[PROGRAM])
    assert not anadata.duplicated(_ANADATA_KEY).any()
    assert not (anadata["AVISITN"] == _DEFAULT.b_visn).any()
    assert (anadata["SAFFL"] == "Y").all()
    assert (anadata["ANL01FL"] == "Y").all()
    assert set(zip(anadata["PARAMCD"], anadata["ATPTN"], strict=True)) == set(_STRATA)


# --- Edge states ----------------------------------------------------------------------------------


@pytest.mark.parametrize("setting", ["ref_trtn", "b_var"])
def test_unset_reference_skips_glm(
    setting: str,
    result_07_02: ProgramResult,
    repo_paths,
    listing_tables: Mapping[str, tuple[sas_reference.ListingTable, ...]],
    sas_checkpoints: Mapping[str, tuple[sas_reference.SasCheckpoint, ...]],
    tolerances: compare.Tolerances,
) -> None:
    """S02-MOD-02, S02-TRN-01: a blank ``ref_trtn`` or ``b_var`` skips the GLM and the merge.

    No model is fitted, ``parameter_estimates`` is empty and ``css_stats`` has no ``pval``
    column, while the statistics themselves are those of the default run. No model quantity of
    the comparison passes.
    """
    result = wpct_f_07_02.run(wpct_f_07_02.Config(**{setting: None}), repo_paths)

    assert result.glm_skipped
    assert result.models == ()
    estimates = result.dataset("parameter_estimates")
    assert estimates.empty
    assert list(estimates.columns) == list(export_spec(PROGRAM, "parameter_estimates").columns)
    stats = result.dataset("css_stats")
    assert "pval" not in stats.columns
    pd.testing.assert_frame_equal(stats, result_07_02.dataset("css_stats").drop(columns="pval"))
    assert all(cp.SAS_DATASET != "CSS_PVALUES" for cp in result.checkpoints)

    # No SAS exports are passed, so the groups do not depend on files a user may have supplied.
    groups = compare.compare_program(
        result, listing_tables[PROGRAM], sas_checkpoints[PROGRAM], None, tolerances
    )
    # wpct.compare reports each listing value of a skipped model as a non-PASS row carrying the
    # skip reason and lists the skip under not compared. The test pins that no model quantity
    # passes and why, not which non-PASS status compare assigns to the listing rows.
    for key in _MODEL_GROUPS:
        group = _group(groups, key)
        assert group.n_pass == 0, _describe(group.rows)
        unexplained = [row for row in group.rows if _GLM_SKIPPED not in row.reason]
        assert not unexplained, _describe(unexplained)
    not_compared = _group(groups, compare.GROUP_NOT_COMPARED)
    assert any(
        row.status == compare.NOT_COMPARED and row.reason == _GLM_SKIPPED
        for row in not_compared.rows
    ), _describe(not_compared.rows)


def test_empty_selection(advs_07_02: pd.DataFrame, repo_paths) -> None:
    """S02-POP-01, S02-TRN-04: no safety-population record gives zero strata and no error.

    Every dataset is header-only with its export columns, no model is fitted, and the CSS_ANADATA
    checkpoint counts 0 records.
    """
    advs_07_02["SAFFL"] = "N"
    result = wpct_f_07_02.run(wpct_f_07_02.Config(), repo_paths, advs=advs_07_02)

    assert not result.halted
    assert not result.glm_skipped
    assert result.models == ()
    assert set(result.datasets) == {"css_anadata", "css_stats", "parameter_estimates"}
    for name, frame in result.datasets.items():
        assert frame.empty, name
        assert list(frame.columns) == list(export_spec(PROGRAM, name).columns), name

    checkpoints = {(cp.SAS_DATASET, cp.OCCURRENCE): cp for cp in result.checkpoints}
    assert list(checkpoints) == [("ADVS", 1), ("ADVS_SUB", 1), ("CSS_ANADATA", 1)]
    assert checkpoints[("ADVS_SUB", 1)].NOBS == 5925
    assert checkpoints[("CSS_ANADATA", 1)].NOBS == 0


def test_absent_param_timepoint(
    advs_07_02: pd.DataFrame, result_07_02: ProgramResult, repo_paths
) -> None:
    """S02-FLT-01, S02-TRN-04: a configured pair without records yields no stratum.

    Adding DIABP/999, which ADVS does not hold, leaves the strata, statistics, estimates, models
    and checkpoints of the default run unchanged.
    """
    absent = ("DIABP", (999,))
    assert not ((advs_07_02["PARAMCD"] == "DIABP") & (advs_07_02["ATPTN"] == 999)).any()
    config = wpct_f_07_02.Config(param_timepoints=(*_DEFAULT.param_timepoints, absent))
    result = wpct_f_07_02.run(config, repo_paths)

    assert _strata_in_order(result.dataset("css_stats")) == list(_STRATA)
    for name in ("css_stats", "parameter_estimates"):
        pd.testing.assert_frame_equal(result.dataset(name), result_07_02.dataset(name))
    assert result.models == result_07_02.models
    assert result.checkpoints == result_07_02.checkpoints


def test_lower_case_variable_names(
    advs_07_02: pd.DataFrame, result_07_02: ProgramResult, repo_paths
) -> None:
    """S02-DEP-assert_var_exist: lower-case variable names in ADVS give the default run.

    SAS finds a variable whatever the case of its name
    [whitepapers/utilities/assert_var_exist.sas:23], and every later step addresses it by any
    spelling, so the datasets, checkpoints and models equal those of the default run. The frame
    passed in keeps its lower-case names.
    """
    lower = advs_07_02.rename(columns=str.lower)
    lower_names = list(lower.columns)
    result = wpct_f_07_02.run(wpct_f_07_02.Config(), repo_paths, advs=lower)

    assert lower_names != list(advs_07_02.columns)
    assert list(lower.columns) == lower_names
    assert list(result.datasets) == list(result_07_02.datasets)
    for name, frame in result.datasets.items():
        pd.testing.assert_frame_equal(frame, result_07_02.dataset(name))
    assert result.checkpoints == result_07_02.checkpoints
    assert result.models == result_07_02.models
