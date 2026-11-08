"""End-to-end equivalence of the WPCT-F.07.02 port with its SAS reference outputs.

The driver ``wpct.programs.wpct_f_07_02`` runs on ``data/adam/cdisc/advs.xpt`` (the session
fixture ``result_07_02``), and ``wpct.compare`` checks it with ``python/wpct/tolerances.json``
against ``whitepapers/WPCT/outputs_sas/WPCT-F.07.02.lst`` and ``WPCT-F.07.02.log``: 15 log
checkpoints, 40 listing ParameterEstimates values and 6 merged ``pval`` cells, 46 listing-based
values in all. Every SAS value compared is parsed from those files at run time, and no listing value
is written here. The SAS record counts asserted directly are likewise read at run time from their
pinned log lines, through the ``sas_checkpoints`` fixture, and export keys and columns come from
``wpct.schemas``.

No silent pass: every equivalence test asserts the exact number of comparison rows that ran and that
each one is PASS, so neither a FAIL nor a NOT COMPARED row can satisfy it.

The edge-state tests run the driver again with other settings or on an altered private copy of
ADVS, and the comparison tests give ``wpct.compare`` an altered copy of the run or of the listing.
The settings tests only build ``Config``: SAS spellings fold to the schema spelling, and a
setting the export schemas cannot carry is refused (S02-TRN-01). Nothing here writes into the
checkout: the driver never writes, no export or report is produced, and the one altered listing
is a copy under ``tmp_path``.
"""

from __future__ import annotations

import dataclasses
import math
from collections.abc import Iterable, Mapping
from pathlib import Path

import pandas as pd
import pytest

from wpct import compare, model, sas_reference
from wpct.programs import MODEL_ENDPOINT, ProgramResult, wpct_f_07_02
from wpct.schemas import (
    ANA_VARIABLES,
    PROGRAM_07_02,
    SAS_EXPORT_STRATUM_COLUMNS,
    STATISTICS,
    export_spec,
)

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

#: The reason ``wpct.compare`` gives for every model quantity of a run whose GLM is skipped.
_GLM_SKIPPED = "not produced: GLM skipped by setting"

_MODEL_GROUPS = (compare.GROUP_LISTING_PARAMETER_ESTIMATES, compare.GROUP_LISTING_MERGED_PVAL)


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


def _sas_counts(
    sas_checkpoints: Mapping[str, tuple[sas_reference.SasCheckpoint, ...]],
    dataset: str,
    stratum: tuple[str, float] | None = None,
) -> tuple[sas_reference.SasCheckpoint, ...]:
    """The checkpoints of ``dataset`` read from the program's SAS log at run time, in log order.

    ``stratum`` is a ``(PARAMCD, ATPTN)`` pair; without it only the program-level checkpoints,
    which carry neither, match. At least one pinned log line must match, and each match must have
    passed verification with an observation count, so a missing or drifted line fails the caller.
    """
    paramcd, atptn = (None, None) if stratum is None else stratum
    matches = tuple(
        sas
        for sas in sas_checkpoints[PROGRAM]
        if (sas.spec.SAS_DATASET, sas.spec.PARAMCD, sas.spec.ATPTN) == (dataset, paramcd, atptn)
    )
    assert matches, f"{PROGRAM}: no pinned SAS log checkpoint {dataset} for stratum {stratum}"
    unverified = [sas.problem for sas in matches if not sas.ok]
    assert not unverified, f"{PROGRAM}: SAS log checkpoint {dataset} unverified: {unverified}"
    uncounted = [sas.spec.line for sas in matches if sas.NOBS is None]
    assert not uncounted, f"{PROGRAM}: SAS log lines {uncounted} carry no {dataset} count"
    return matches


@pytest.fixture(scope="module")
def groups_07_02(
    comparisons: Mapping[str, tuple[compare.ComparisonGroup, ...]],
) -> tuple[compare.ComparisonGroup, ...]:
    """Every comparison group of the default WPCT-F.07.02 run."""
    return comparisons[PROGRAM]


# --- Equivalence against the repository SAS references --------------------------------------------


def test_s02_flt_01_to_trn_05_log_checkpoints_all_pass(
    result_07_02: ProgramResult,
    groups_07_02: tuple[compare.ComparisonGroup, ...],
    sas_checkpoints: Mapping[str, tuple[sas_reference.SasCheckpoint, ...]],
) -> None:
    """The 15 pinned record counts of the SAS log agree exactly.

    They evidence S02-FLT-01 and S02-TRN-02 (ADVS_SUB), S02-TRN-03, S02-POP-01, S02-FLT-02 and
    S02-FLT-03 (CSS_ANADATA), S02-TRN-04 (CSS_NEXTPARAM, CSS_NEXTTIMEPT), S02-MOD-01 (the PROC
    SUMMARY CSS_STATS), S02-MOD-02 (CSS_PVALUES of every parameter row) and S02-TRN-05 (CSS_PVALUES
    of the TRTPN rows, CSS_STATS after the merge). Each count the driver records, and each variable
    count or its absence, equals the one parsed at run time from its pinned log line.
    """
    pinned = sas_reference.LOG_CHECKPOINT_MAP[PROGRAM]
    assert len(pinned) == 15
    unverified = [sas.problem for sas in sas_checkpoints[PROGRAM] if not sas.ok]
    assert not unverified, f"pinned SAS log lines failed verification: {unverified}"

    assert_all_pass(_group(groups_07_02, compare.GROUP_LOG_CHECKPOINTS), 15)

    recorded = [
        (cp.SAS_DATASET, cp.PARAMCD, cp.ATPTN, cp.NOBS, cp.NVARS) for cp in result_07_02.checkpoints
    ]
    parsed = [
        (sas.spec.SAS_DATASET, sas.spec.PARAMCD, sas.spec.ATPTN, sas.NOBS, sas.NVARS)
        for sas in sas_checkpoints[PROGRAM]
    ]
    assert len(parsed) == len(pinned)
    assert recorded == parsed
    assert [
        (cp.SAS_DATASET, cp.OCCURRENCE, cp.PARAMCD, cp.ATPTN) for cp in result_07_02.checkpoints
    ] == [(spec.SAS_DATASET, spec.OCCURRENCE, spec.PARAMCD, spec.ATPTN) for spec in pinned]


def test_s02_mod_02_parameter_estimates_match_listing(
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


def test_s02_trn_05_merged_pval_matches_listing(
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


def test_s02_mod_02_and_trn_05_listing_based_total_46(
    groups_07_02: tuple[compare.ComparisonGroup, ...]
) -> None:
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


def test_d03_missing_chg_rows_excluded_from_summary(
    result_07_02: ProgramResult,
    sas_checkpoints: Mapping[str, tuple[sas_reference.SasCheckpoint, ...]],
) -> None:
    """D-03, S02-MOD-01: a missing CHG is left out of ``n``, and its BY group still forms.

    The 20 analysis records of subject 01-718-1150 (every post-baseline visit of both parameters)
    miss both BASE and CHG, and no record misses only one of them. Each stratum holds as many
    records as its CSS_NEXTTIMEPT count, parsed at run time from the pinned log line, 10 of them
    without CHG, so its ``n`` values sum to that count less 10.
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
        (timepoint,) = _sas_counts(sas_checkpoints, "CSS_NEXTTIMEPT", (paramcd, atptn))
        records = _stratum(anadata, paramcd, atptn)
        assert len(records) == timepoint.NOBS, (paramcd, atptn)
        assert int(records["CHG"].isna().sum()) == 10, (paramcd, atptn)
        n_total = int(_stratum(stats, paramcd, atptn)["n"].sum())
        assert n_total == timepoint.NOBS - 10, (paramcd, atptn)

    # Per BY group, n is the count of non-missing CHG among the group's records; a group is one
    # css_stats record, identified by its schema record key.
    by = list(export_spec(PROGRAM, "css_stats").key)
    expected = anadata.groupby(by, sort=True)["CHG"].count()
    actual = stats.set_index(by)["n"].sort_index()
    pd.testing.assert_series_equal(actual, expected, check_names=False, check_exact=True)


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


def test_s02_trn_04_strata_order(
    result_07_02: ProgramResult,
    sas_checkpoints: Mapping[str, tuple[sas_reference.SasCheckpoint, ...]],
) -> None:
    """S02-TRN-04, S02-MOD-01: strata in loop order, rows sorted BY ``AVISITN TRTPN`` (D-04, D-14).

    ``css_stats`` stacks DIABP/815 then SYSBP/816, each block as many rows as the stratum's
    CSS_STATS after the merge, parsed at run time from its pinned log line. Its columns are the
    stratum columns a per-stratum SAS export adds, then as many SAS columns as that line's variable
    count: the 4 BY variables, the 8 statistics and the merged ``pval``, in schema order.
    """
    stats = result_07_02.dataset("css_stats")
    spec = export_spec(PROGRAM, "css_stats")
    assert list(stats.columns) == list(spec.columns)
    # The stratum columns a per-stratum SAS export prepends lead both the columns and the key.
    stratum = SAS_EXPORT_STRATUM_COLUMNS[spec.sas_export_file]
    assert stratum
    assert tuple(stats.columns[: len(stratum)]) == stratum
    assert spec.key[: len(stratum)] == stratum
    # Then the SAS columns in dataset order: the BY variables of ``by avisitn &tn_var avisit
    # &t_var`` [whitepapers/WPCT/WPCT-F.07.02.sas:259], which are the rest of the record key and
    # then the categorical labels, followed by the statistics and the merged pval.
    by_key = list(spec.key[len(stratum) :])
    by_variables = [*by_key, *spec.categorical]
    _, tn_var, _, t_var = by_variables
    assert (tn_var, t_var) == (_DEFAULT.tn_var, _DEFAULT.t_var)
    assert list(stats.columns[len(stratum) :]) == [
        *by_variables,
        *STATISTICS,
        *spec.optional_columns,
    ]

    assert _strata_in_order(stats) == list(_STRATA)
    assert _strata_in_order(result_07_02.dataset("parameter_estimates")) == list(_STRATA)
    for paramcd, atptn in _STRATA:
        # The stratum's two CSS_STATS lines in log order: PROC SUMMARY's, then the merged one.
        _, merged = _sas_counts(sas_checkpoints, "CSS_STATS", (paramcd, atptn))
        assert len(stats.columns) - len(stratum) == merged.NVARS, (paramcd, atptn)
        block = _stratum(stats, paramcd, atptn)
        assert len(block) == merged.NOBS, (paramcd, atptn)
        start = block.index[0]
        contiguous = range(start, start + merged.NOBS)
        assert list(block.index) == list(contiguous), "stratum rows not contiguous"
        assert not block[by_key].isna().any().any()
        keys = list(block[by_key].itertuples(index=False, name=None))
        assert keys == sorted(keys), (paramcd, atptn)
        assert len(set(keys)) == merged.NOBS, (paramcd, atptn)


def test_s02_trn_03_css_anadata_shape(
    result_07_02: ProgramResult,
    sas_checkpoints: Mapping[str, tuple[sas_reference.SasCheckpoint, ...]],
) -> None:
    """S02-TRN-03 and the selections S02-FLT-01, S02-POP-01, S02-FLT-02, S02-FLT-03.

    As many records and variables as the CSS_ANADATA counts parsed at run time from its pinned log
    line, the variables being the analysis variables in schema order. The records are unique on
    the schema record key, exclude the baseline visit and have ``SAFFL = ANL01FL = 'Y'``.
    """
    anadata = result_07_02.dataset("css_anadata")
    spec = export_spec(PROGRAM, "css_anadata")
    (sas,) = _sas_counts(sas_checkpoints, "CSS_ANADATA")
    assert anadata.shape == (sas.NOBS, sas.NVARS)
    assert list(anadata.columns) == list(spec.columns)
    assert sorted(anadata.columns) == sorted(ANA_VARIABLES[PROGRAM])
    assert not anadata.duplicated(list(spec.key)).any()
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
    groups_07_02: tuple[compare.ComparisonGroup, ...],
) -> None:
    """S02-MOD-02, S02-TRN-01: a blank ``ref_trtn`` or ``b_var`` skips the GLM and the merge.

    No model is fitted, ``parameter_estimates`` is empty and ``css_stats`` has no ``pval``
    column, while the statistics themselves are those of the default run. The comparison still
    holds exactly the 40 listing ParameterEstimates rows and the 6 merged-pval rows of the default
    run, in its order, and none of them passes: each carries the skip reason and no Python value.
    Pinning both counts (AAP 0.7.4, no silent pass) makes an empty or truncated group fail.
    """
    result = wpct_f_07_02.run(wpct_f_07_02.Config(**{setting: None}), repo_paths)

    assert result.glm_skipped
    assert result.models == ()
    estimates = result.dataset("parameter_estimates")
    assert estimates.empty
    assert list(estimates.columns) == list(export_spec(PROGRAM, "parameter_estimates").columns)
    stats = result.dataset("css_stats")
    assert "pval" not in stats.columns
    pd.testing.assert_frame_equal(
        stats, result_07_02.dataset("css_stats").drop(columns="pval"), check_exact=True
    )
    assert all(cp.SAS_DATASET != "CSS_PVALUES" for cp in result.checkpoints)

    # No SAS exports are passed, so the groups do not depend on files a user may have supplied.
    groups = compare.compare_program(
        result, listing_tables[PROGRAM], sas_checkpoints[PROGRAM], None, tolerances
    )
    # wpct.compare reports each listing value of a skipped model as a non-PASS row carrying the
    # skip reason and lists the skip under not compared. The test pins how many rows each model
    # group holds, which quantities they are, that none passes and why, not which non-PASS status
    # compare assigns to the listing rows.
    for key, count in zip(_MODEL_GROUPS, (40, 6), strict=True):
        group = _group(groups, key)
        assert len(group.rows) == count, (
            f"{PROGRAM} {key}: {len(group.rows)} comparison rows ran, expected {count}:\n"
            f"{_describe(group.rows)}"
        )
        default_rows = _group(groups_07_02, key).rows
        assert [(row.quantity, row.stratum) for row in group.rows] == [
            (row.quantity, row.stratum) for row in default_rows
        ], _describe(group.rows)
        passing = [row for row in group.rows if row.status == compare.PASS]
        assert not passing, _describe(passing)
        unexplained = [row for row in group.rows if _GLM_SKIPPED not in row.reason]
        assert not unexplained, _describe(unexplained)
        with_python_value = [row for row in group.rows if row.python_value != ""]
        assert not with_python_value, _describe(with_python_value)
    not_compared = _group(groups, compare.GROUP_NOT_COMPARED)
    assert any(
        row.status == compare.NOT_COMPARED and row.reason == _GLM_SKIPPED
        for row in not_compared.rows
    ), _describe(not_compared.rows)


def test_empty_selection(
    advs_07_02: pd.DataFrame,
    repo_paths,
    sas_checkpoints: Mapping[str, tuple[sas_reference.SasCheckpoint, ...]],
) -> None:
    """S02-POP-01, S02-TRN-04: no safety-population record gives zero strata and no error.

    Every dataset is header-only with its export columns, no model is fitted, and the CSS_ANADATA
    checkpoint counts 0 records, while ADVS_SUB, taken before the population flag, still holds the
    count parsed at run time from its pinned log line.
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
    (advs_sub,) = _sas_counts(sas_checkpoints, "ADVS_SUB")
    assert checkpoints[("ADVS_SUB", 1)].NOBS == advs_sub.NOBS
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
        pd.testing.assert_frame_equal(
            result.dataset(name), result_07_02.dataset(name), check_exact=True
        )
    assert result.models == result_07_02.models
    assert result.checkpoints == result_07_02.checkpoints


def test_u02_reference_level_absent(
    advs_07_02: pd.DataFrame,
    repo_paths,
    listing_tables: Mapping[str, tuple[sas_reference.ListingTable, ...]],
    sas_checkpoints: Mapping[str, tuple[sas_reference.SasCheckpoint, ...]],
    tolerances: compare.Tolerances,
) -> None:
    """U-02: without a reference-level record no model is fitted, and the reason is its status.

    With every ``TRTPN 0`` record removed, both endpoint models carry the status
    ``not fitted: reference level 0 absent``. Each listing ParameterEstimates and merged-pval value
    is a FAIL whose reason is that status, stated once, with no Python value, and each stratum is
    listed under not compared with the same reason.
    """
    advs = advs_07_02.loc[(advs_07_02["TRTPN"] != _REF_TRTPN).to_numpy(dtype=bool)]
    result = wpct_f_07_02.run(_DEFAULT, repo_paths, advs=advs)
    status = model.status_reference_absent(_DEFAULT.ref_trtn)
    assert [(record.PARAMCD, record.ATPTN, record.STATUS) for record in result.models] == [
        (paramcd, atptn, status) for paramcd, atptn in _STRATA
    ]

    groups = compare.compare_program(
        result, listing_tables[PROGRAM], sas_checkpoints[PROGRAM], None, tolerances
    )
    for key, count in zip(_MODEL_GROUPS, (40, 6), strict=True):
        group = _group(groups, key)
        assert len(group.rows) == count
        wrong = [
            row
            for row in group.rows
            if (row.status, row.reason, row.python_value) != (compare.FAIL, status, "")
        ]
        assert not wrong, _describe(wrong)
    not_fitted = [
        row
        for row in _group(groups, compare.GROUP_NOT_COMPARED).rows
        if row.quantity == f"GLM ParameterEstimates and merged pval ({MODEL_ENDPOINT} model)"
    ]
    assert [(row.stratum, row.status, row.reason) for row in not_fitted] == [
        (f"PARAMCD={paramcd} ATPTN={atptn:g}", compare.NOT_COMPARED, status)
        for paramcd, atptn in _STRATA
    ]


# --- Comparison of unusable Python data and drifted listings --------------------------------------


@pytest.mark.parametrize(
    ("dataset", "column", "failing", "passing"),
    [
        (
            "parameter_estimates",
            "ESTIMATE",
            compare.GROUP_LISTING_PARAMETER_ESTIMATES,
            compare.GROUP_LISTING_MERGED_PVAL,
        ),
        (
            "css_stats",
            "pval",
            compare.GROUP_LISTING_MERGED_PVAL,
            compare.GROUP_LISTING_PARAMETER_ESTIMATES,
        ),
    ],
    ids=["parameter_estimates", "css_stats"],
)
def test_s02_mod_02_python_columns_colliding_in_case_fail_every_value(
    dataset: str,
    column: str,
    failing: str,
    passing: str,
    result_07_02: ProgramResult,
    groups_07_02: tuple[compare.ComparisonGroup, ...],
    listing_tables: Mapping[str, tuple[sas_reference.ListingTable, ...]],
    sas_checkpoints: Mapping[str, tuple[sas_reference.SasCheckpoint, ...]],
    tolerances: compare.Tolerances,
) -> None:
    """S02-MOD-02, S02-TRN-05: a case-colliding Python dataset fails every value read from it.

    The ``parameter_estimates`` case covers the ParameterEstimates group (S02-MOD-02), and the
    ``css_stats`` case the merged-pval group (S02-TRN-05). SAS names are case-insensitive, so a
    second ``column`` spelled in the other case makes the dataset ambiguous. The comparison does not
    abort: every quantity and stratum of the clean group is still reported, each a FAIL naming the
    dataset and the column with no Python value shown, while the group that does not read the
    dataset keeps passing in full.
    """
    frame = result_07_02.dataset(dataset)
    colliding = frame.assign(**{column.swapcase(): frame[column]})
    malformed = dataclasses.replace(
        result_07_02, datasets={**result_07_02.datasets, dataset: colliding}
    )
    groups = compare.compare_program(
        malformed, listing_tables[PROGRAM], sas_checkpoints[PROGRAM], None, tolerances
    )

    group = _group(groups, failing)
    clean = _group(groups_07_02, failing)
    assert [(row.quantity, row.stratum) for row in group.rows] == [
        (row.quantity, row.stratum) for row in clean.rows
    ]
    assert group.n_fail == len(group.rows), _describe(group.rows)
    unexplained = [
        row
        for row in group.rows
        if not row.reason.startswith(f"Python {dataset}: ") or column.upper() not in row.reason
    ]
    assert not unexplained, _describe(unexplained)
    assert {row.python_value for row in group.rows} == {""}
    other = _group(groups, passing)
    assert_all_pass(other, len(_group(groups_07_02, passing).rows))


def test_s02_mod_02_listing_problem_keeps_python_values(
    tmp_path: Path,
    repo_paths,
    groups_07_02: tuple[compare.ComparisonGroup, ...],
    result_07_02: ProgramResult,
    sas_checkpoints: Mapping[str, tuple[sas_reference.SasCheckpoint, ...]],
    tolerances: compare.Tolerances,
) -> None:
    """S02-MOD-02, S02-TRN-05: a table the listing parser flags fails yet keeps the Python values.

    A copy of the listing prints the DIABP/815 ``TRTPN 54`` row as ``TRTPN 55``, so the parser
    reports a problem for that table only. The Python models are fitted, so every row of the
    ParameterEstimates (S02-MOD-02) and merged-pval (S02-TRN-05) groups carries the same Python
    value as in the clean comparison: the reference level's zero estimate stays ``0``, its missing
    standard error, t statistic, p-value and merged pval stay ``.``, and no row is left without a
    value. The flagged table's rows are FAIL with the listing problem; the other table's still pass.
    """
    lines = repo_paths.listing_file(PROGRAM).read_bytes().split(b"\n")
    printed, drifted = b"TRTPN     54", b"TRTPN     55"
    assert lines[11].startswith(printed)
    lines[11] = drifted + lines[11][len(printed) :]
    listing = tmp_path / f"{PROGRAM}.lst"
    listing.write_bytes(b"\n".join(lines))
    tables = sas_reference.parse_glm_listing(listing, sas_reference.GLM_TABLE_MAP[PROGRAM])
    assert [bool(table.problems) for table in tables] == [True, False]
    (problem,) = tables[0].problems
    flagged = f"PARAMCD={tables[0].spec.PARAMCD} "

    groups = compare.compare_program(
        result_07_02, tables, sas_checkpoints[PROGRAM], None, tolerances
    )
    for key in _MODEL_GROUPS:
        rows = _group(groups, key).rows
        clean = _group(groups_07_02, key).rows
        assert [(row.quantity, row.stratum, row.python_value) for row in rows] == [
            (row.quantity, row.stratum, row.python_value) for row in clean
        ]
        assert "" not in {row.python_value for row in rows}
        assert sum(row.stratum.startswith(flagged) for row in rows) == len(rows) // 2
        for row in rows:
            if row.stratum.startswith(flagged):
                assert (row.status, row.reason) == (compare.FAIL, problem), _describe([row])
            else:
                assert row.status == compare.PASS, _describe([row])


def test_s02_dep_assert_var_exist_lower_case_variable_names(
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
        pd.testing.assert_frame_equal(frame, result_07_02.dataset(name), check_exact=True)
    assert result.checkpoints == result_07_02.checkpoints
    assert result.models == result_07_02.models


# --- Rounded listing values: numeric negative controls --------------------------------------------

#: The exact row count of each listing group: 2 tables x 5 rows x 4 values, and 2 strata x 3 levels.
_LISTING_COUNTS = {
    compare.GROUP_LISTING_PARAMETER_ESTIMATES: 40,
    compare.GROUP_LISTING_MERGED_PVAL: 6,
}

#: The reasons ``wpct.compare`` gives a rounded listing value that fails, by cause.
_BEYOND_PRECISION = "values differ beyond the displayed precision ({decimals} dp)"
_OUTSIDE_BOUND = "Python value outside the bound "
_PYTHON_MISSING = "Python value missing, SAS not"
_SAS_MISSING = "SAS value missing, Python not"

#: The stratum and fitted TRTPN level whose values most controls alter: DIABP/815, level 54.
_TARGET_PARAMCD = _STRATA[0][0]
_TARGET_LEVEL = "54"


def _listing_token(
    tables: Iterable[sas_reference.ListingTable],
    paramcd: str,
    effect: str,
    level: str,
    field: str,
) -> str:
    """The token the SAS listing displays for one ParameterEstimates value, parsed at run time."""
    (table,) = [table for table in tables if table.spec and table.spec.PARAMCD == paramcd]
    (row,) = [row for row in table.rows if (row.EFFECT, row.LEVEL) == (effect, level)]
    return getattr(row, field)


def _displayed_decimals(token: str) -> int:
    """The number of decimals of a displayed listing number; fails for any other token."""
    whole, point, fraction = token.strip(" ").partition(".")
    digits = whole.removeprefix("-")
    assert point, f"not a displayed number: {token!r}"
    assert fraction.isdigit(), f"not a displayed number: {token!r}"
    assert digits.isdigit() or not digits, f"not a displayed number: {token!r}"
    return len(fraction)


def _assert_displayed_as(token: str, kind: str, tolerances: compare.Tolerances) -> None:
    """Assert that the listing shows ``token`` as ``kind``: number, bound, dot or blank."""
    if kind == "number":
        _displayed_decimals(token)
        return
    expected = {"bound": tolerances.bound_token, "dot": ".", "blank": ""}[kind]
    assert token.strip(" ") == expected, f"listing token {token!r} is not {kind}"


def _endpoint_stratum(paramcd: str) -> str:
    """The report stratum of the endpoint table of ``paramcd``, as ``wpct.compare`` writes it."""
    return f"PARAMCD={paramcd} ATPTN={dict(_STRATA)[paramcd]:g} AVISITN={_DEFAULT.e_visn:g}"


def _estimate_cell(paramcd: str, effect: str, level: str) -> dict[str, object]:
    """The ``parameter_estimates`` row of one parameter of the endpoint model of ``paramcd``."""
    return {"PARAMCD": paramcd, "ATPTN": dict(_STRATA)[paramcd], "EFFECT": effect, "LEVEL": level}


def _pval_cell(paramcd: str, level: str) -> dict[str, object]:
    """The ``css_stats`` endpoint row of one TRTPN level of ``paramcd``, which the merge fills."""
    return {
        "PARAMCD": paramcd,
        "ATPTN": dict(_STRATA)[paramcd],
        "AVISITN": _DEFAULT.e_visn,
        "TRTPN": float(level),
    }


def _compare_altered(
    result: ProgramResult,
    name: str,
    cell: Mapping[str, object],
    column: str,
    value: float,
    listing_tables: Mapping[str, tuple[sas_reference.ListingTable, ...]],
    sas_checkpoints: Mapping[str, tuple[sas_reference.SasCheckpoint, ...]],
    tolerances: compare.Tolerances,
) -> tuple[compare.ComparisonGroup, ...]:
    """Compare a copy of ``result`` holding ``value`` in one cell of dataset ``name``.

    ``cell`` names the one row by its column values, and ``column`` of that row is set on a private
    deep copy of the dataset; every other dataset is the session's own. The comparison uses the
    parsed SAS references and the shared tolerances, and afterwards every dataset of ``result`` is
    shown to equal its state before the call, so the session run is never altered.
    """
    before = {key: frame.copy(deep=True) for key, frame in result.datasets.items()}
    frame = result.dataset(name).copy(deep=True)
    mask = pd.Series(True, index=frame.index)
    for cell_column, wanted in cell.items():
        mask &= frame[cell_column] == wanted
    assert int(mask.sum()) == 1, f"{name}: {int(mask.sum())} rows match {dict(cell)}"
    frame.loc[mask, column] = value
    altered = dataclasses.replace(result, datasets={**result.datasets, name: frame})
    groups = compare.compare_program(
        altered, listing_tables[PROGRAM], sas_checkpoints[PROGRAM], None, tolerances
    )
    assert list(result.datasets) == list(before)
    for key, original in before.items():
        pd.testing.assert_frame_equal(result.dataset(key), original, check_exact=True)
    return groups


def _assert_listing_verdicts(
    groups: tuple[compare.ComparisonGroup, ...],
    target: tuple[str, str, str],
    status: str,
    reason: str,
    python_value: float,
) -> None:
    """Assert the altered row's verdict and that every other listing row still passes.

    ``target`` is the ``(group key, quantity, stratum)`` of the row whose Python value was set to
    ``python_value``. Both listing groups hold exactly their 40 and 6 rows, the target occurs once
    and shows ``python_value``, its status is ``status`` and its reason starts with ``reason`` (is
    empty for a PASS), and every other row of both groups is PASS.
    """
    key, quantity, stratum = target
    for group_key, count in _LISTING_COUNTS.items():
        group = _group(groups, group_key)
        assert len(group.rows) == count, (
            f"{PROGRAM} {group_key}: {len(group.rows)} comparison rows ran, expected {count}"
        )
        is_target = [(group_key, row.quantity, row.stratum) == target for row in group.rows]
        assert sum(is_target) == (1 if group_key == key else 0), _describe(group.rows)
        others = [row for row, hit in zip(group.rows, is_target, strict=True) if not hit]
        failing = [row for row in others if row.status != compare.PASS]
        assert not failing, _describe(failing)
        expected_pass = count - 1 if group_key == key and status != compare.PASS else count
        assert group.n_pass == expected_pass, _describe(group.rows)

    rows = _group(groups, key).rows
    (row,) = [row for row in rows if (row.quantity, row.stratum) == (quantity, stratum)]
    assert row.status == status, _describe([row])
    if reason:
        assert row.reason.startswith(reason), _describe([row])
    else:
        assert row.reason == "", _describe([row])
    if math.isnan(python_value):
        assert row.python_value == ".", _describe([row])
    else:
        assert float(row.python_value) == python_value, _describe([row])


@pytest.mark.parametrize(
    ("field", "decimals"),
    [("ESTIMATE", 8), ("TVALUE", 2), ("PROBT", 4)],
    ids=["estimate", "tvalue", "probt"],
)
@pytest.mark.parametrize(
    ("half_units", "epsilons", "status"),
    [
        (-1.1, 0.0, compare.FAIL),
        (-1.0, -2.0, compare.FAIL),
        (-1.0, -0.5, compare.PASS),
        (-1.0, 0.0, compare.PASS),
        (-0.9, 0.0, compare.PASS),
        (0.9, 0.0, compare.PASS),
        (1.0, 0.0, compare.PASS),
        (1.0, 0.5, compare.PASS),
        (1.0, 2.0, compare.FAIL),
        (1.1, 0.0, compare.FAIL),
    ],
    ids=[
        "below-by-1.1-half-units",
        "below-edge-by-2-epsilon",
        "below-edge-within-epsilon",
        "lower-edge",
        "below-by-0.9-half-units",
        "above-by-0.9-half-units",
        "upper-edge",
        "above-edge-within-epsilon",
        "above-edge-by-2-epsilon",
        "above-by-1.1-half-units",
    ],
)
def test_s02_mod_02_listing_value_judged_at_displayed_precision(
    field: str,
    decimals: int,
    half_units: float,
    epsilons: float,
    status: str,
    result_07_02: ProgramResult,
    listing_tables: Mapping[str, tuple[sas_reference.ListingTable, ...]],
    sas_checkpoints: Mapping[str, tuple[sas_reference.SasCheckpoint, ...]],
    tolerances: compare.Tolerances,
) -> None:
    """S02-MOD-02: a fitted value agrees with its listing token only within the displayed precision.

    A token with d decimals stands for every value that rounds to it, so the rounded listing rule
    passes a Python value only when its distance from the token is at most the half unit of the
    last displayed digit, ``half_unit x 10^-d``, plus the configured epsilon. The DIABP/815
    ``TRTPN 54`` value of ``field`` (8, 2 and 4 decimals) is set, on a private copy of the run, to
    the token moved by ``half_units`` half units and ``epsilons`` epsilons: up to the edge and
    within epsilon beyond it passes, while 1.1 half units or the edge plus twice epsilon fails with
    the displayed-precision reason. The other 39 ParameterEstimates rows and the 6 merged cells
    still pass.
    """
    token = _listing_token(listing_tables[PROGRAM], _TARGET_PARAMCD, "TRTPN", _TARGET_LEVEL, field)
    assert _displayed_decimals(token) == decimals, token
    half_unit = tolerances.half_unit * 10.0**-decimals
    value = float(token) + half_units * half_unit + epsilons * tolerances.epsilon

    groups = _compare_altered(
        result_07_02,
        "parameter_estimates",
        _estimate_cell(_TARGET_PARAMCD, "TRTPN", _TARGET_LEVEL),
        field,
        value,
        listing_tables,
        sas_checkpoints,
        tolerances,
    )
    target = (
        compare.GROUP_LISTING_PARAMETER_ESTIMATES,
        f"TRTPN {_TARGET_LEVEL} {field}",
        _endpoint_stratum(_TARGET_PARAMCD),
    )
    reason = "" if status == compare.PASS else _BEYOND_PRECISION.format(decimals=decimals)
    _assert_listing_verdicts(groups, target, status, reason, value)


@pytest.mark.parametrize(
    ("bound_fraction", "steps_down", "status"),
    [
        (0.0, 0, compare.PASS),
        (1.0, 1, compare.PASS),
        (1.0, 0, compare.FAIL),
        (0.0, 1, compare.FAIL),
    ],
    ids=["zero", "just-below-upper-bound", "upper-bound", "just-below-zero"],
)
def test_s02_mod_02_bound_token_passes_only_zero_up_to_upper_bound(
    bound_fraction: float,
    steps_down: int,
    status: str,
    result_07_02: ProgramResult,
    listing_tables: Mapping[str, tuple[sas_reference.ListingTable, ...]],
    sas_checkpoints: Mapping[str, tuple[sas_reference.SasCheckpoint, ...]],
    tolerances: compare.Tolerances,
) -> None:
    """S02-MOD-02: a Probt the listing prints as the bound token passes only in [0, upper bound).

    SAS prints a p-value too small to display as ``<.0001``, so the rounded listing rule accepts a
    Python value from zero inclusive to the configured upper bound exclusive. The DIABP/815
    Intercept Probt is set, on a private copy of the run, to ``bound_fraction`` times the upper
    bound, moved ``steps_down`` floats toward minus infinity: zero and the largest float below the
    upper bound pass, while the upper bound itself and the largest float below zero fail with the
    bound reason. Every other listing row still passes.
    """
    token = _listing_token(listing_tables[PROGRAM], _TARGET_PARAMCD, "INTERCEPT", "", "PROBT")
    assert token == tolerances.bound_token
    value = bound_fraction * tolerances.bound_upper
    for _ in range(steps_down):
        value = math.nextafter(value, -math.inf)

    groups = _compare_altered(
        result_07_02,
        "parameter_estimates",
        _estimate_cell(_TARGET_PARAMCD, "INTERCEPT", ""),
        "PROBT",
        value,
        listing_tables,
        sas_checkpoints,
        tolerances,
    )
    target = (
        compare.GROUP_LISTING_PARAMETER_ESTIMATES,
        "INTERCEPT PROBT",
        _endpoint_stratum(_TARGET_PARAMCD),
    )
    reason = "" if status == compare.PASS else _OUTSIDE_BOUND
    _assert_listing_verdicts(groups, target, status, reason, value)


@pytest.mark.parametrize(
    ("paramcd", "effect", "level", "field", "sas_shows", "value", "reason"),
    [
        (_TARGET_PARAMCD, "TRTPN", _TARGET_LEVEL, "ESTIMATE", "number", math.nan, _PYTHON_MISSING),
        (_TARGET_PARAMCD, "INTERCEPT", "", "PROBT", "bound", math.nan, _PYTHON_MISSING),
        (_TARGET_PARAMCD, "TRTPN", _REF_LEVEL, "STDERR", "dot", 0.0, _SAS_MISSING),
        (_STRATA[1][0], "TRTPN", _REF_LEVEL, "STDERR", "blank", 0.0, _SAS_MISSING),
    ],
    ids=[
        "python-missing-sas-number",
        "python-missing-sas-bound",
        "python-number-sas-dot",
        "python-number-sas-blank",
    ],
)
def test_s02_mod_02_value_missing_on_one_side_fails(
    paramcd: str,
    effect: str,
    level: str,
    field: str,
    sas_shows: str,
    value: float,
    reason: str,
    result_07_02: ProgramResult,
    listing_tables: Mapping[str, tuple[sas_reference.ListingTable, ...]],
    sas_checkpoints: Mapping[str, tuple[sas_reference.SasCheckpoint, ...]],
    tolerances: compare.Tolerances,
) -> None:
    """S02-MOD-02: a ParameterEstimates value missing on exactly one side fails.

    Under the missing-value rule both sides missing pass and exactly one side missing fails,
    whatever the other side holds. On a private copy of the run, a Python NaN against a displayed
    number or against the ``<.0001`` bound fails as Python missing, and a Python number against
    the reference level's missing SE, printed ``.`` in the DIABP table and blank in the SYSBP table
    after ``options missing=' '``, fails as SAS missing. Every other listing row still passes.
    """
    token = _listing_token(listing_tables[PROGRAM], paramcd, effect, level, field)
    _assert_displayed_as(token, sas_shows, tolerances)

    groups = _compare_altered(
        result_07_02,
        "parameter_estimates",
        _estimate_cell(paramcd, effect, level),
        field,
        value,
        listing_tables,
        sas_checkpoints,
        tolerances,
    )
    label = f"{effect} {level}" if level else effect
    target = (
        compare.GROUP_LISTING_PARAMETER_ESTIMATES,
        f"{label} {field}",
        _endpoint_stratum(paramcd),
    )
    _assert_listing_verdicts(groups, target, compare.FAIL, reason, value)


@pytest.mark.parametrize(
    ("paramcd", "level", "half_units", "status"),
    [
        (_TARGET_PARAMCD, _TARGET_LEVEL, -1.1, compare.FAIL),
        (_TARGET_PARAMCD, _TARGET_LEVEL, -0.9, compare.PASS),
        (_TARGET_PARAMCD, _TARGET_LEVEL, 0.9, compare.PASS),
        (_TARGET_PARAMCD, _TARGET_LEVEL, 1.0, compare.PASS),
        (_TARGET_PARAMCD, _TARGET_LEVEL, 1.1, compare.FAIL),
        (_STRATA[1][0], "81", -1.1, compare.FAIL),
    ],
    ids=[
        "diabp-54-below-by-1.1-half-units",
        "diabp-54-below-by-0.9-half-units",
        "diabp-54-above-by-0.9-half-units",
        "diabp-54-upper-edge",
        "diabp-54-above-by-1.1-half-units",
        "sysbp-81-below-by-1.1-half-units",
    ],
)
def test_s02_trn_05_merged_pval_cell_judged_at_displayed_precision(
    paramcd: str,
    level: str,
    half_units: float,
    status: str,
    result_07_02: ProgramResult,
    listing_tables: Mapping[str, tuple[sas_reference.ListingTable, ...]],
    sas_checkpoints: Mapping[str, tuple[sas_reference.SasCheckpoint, ...]],
    tolerances: compare.Tolerances,
) -> None:
    """S02-TRN-05: the merged-pval group judges the CSS_STATS cell itself, not the model row.

    Only a private copy of ``css_stats`` is altered; ``parameter_estimates`` keeps the model's
    Probt. One endpoint cell is set to its level's listing Probt moved by ``half_units`` half units
    of the last displayed digit. Under the rounded listing rule, within the half unit all 6 cells
    pass and the cell shows the value set, so the cell was what was compared; beyond it exactly
    that cell, at its own stratum and level, fails with the displayed-precision reason, while the
    other 5 cells and all 40 ParameterEstimates rows pass.
    """
    token = _listing_token(listing_tables[PROGRAM], paramcd, "TRTPN", level, "PROBT")
    decimals = _displayed_decimals(token)
    value = float(token) + half_units * tolerances.half_unit * 10.0**-decimals

    groups = _compare_altered(
        result_07_02,
        "css_stats",
        _pval_cell(paramcd, level),
        "pval",
        value,
        listing_tables,
        sas_checkpoints,
        tolerances,
    )
    target = (
        compare.GROUP_LISTING_MERGED_PVAL,
        "CSS_STATS.pval",
        f"{_endpoint_stratum(paramcd)} TRTPN={level}",
    )
    reason = "" if status == compare.PASS else _BEYOND_PRECISION.format(decimals=decimals)
    _assert_listing_verdicts(groups, target, status, reason, value)


@pytest.mark.parametrize(
    ("paramcd", "level", "sas_shows", "value", "reason"),
    [
        (_TARGET_PARAMCD, _TARGET_LEVEL, "number", math.nan, _PYTHON_MISSING),
        (_TARGET_PARAMCD, _REF_LEVEL, "dot", 0.0, _SAS_MISSING),
        (_STRATA[1][0], _REF_LEVEL, "blank", 0.0, _SAS_MISSING),
    ],
    ids=["python-missing-sas-number", "python-number-sas-dot", "python-number-sas-blank"],
)
def test_s02_trn_05_merged_pval_missing_on_one_side_fails(
    paramcd: str,
    level: str,
    sas_shows: str,
    value: float,
    reason: str,
    result_07_02: ProgramResult,
    listing_tables: Mapping[str, tuple[sas_reference.ListingTable, ...]],
    sas_checkpoints: Mapping[str, tuple[sas_reference.SasCheckpoint, ...]],
    tolerances: compare.Tolerances,
) -> None:
    """S02-TRN-05: a merged pval cell missing on exactly one side fails.

    Under the missing-value rule exactly one side missing fails. On a private copy of
    ``css_stats`` alone, a NaN in a fitted level's cell fails as Python missing, and a number in
    the reference level's cell, whose listing Probt is ``.`` (DIABP) or blank (SYSBP), fails as SAS
    missing. The other 5 cells and all 40 ParameterEstimates rows still pass.
    """
    token = _listing_token(listing_tables[PROGRAM], paramcd, "TRTPN", level, "PROBT")
    _assert_displayed_as(token, sas_shows, tolerances)

    groups = _compare_altered(
        result_07_02,
        "css_stats",
        _pval_cell(paramcd, level),
        "pval",
        value,
        listing_tables,
        sas_checkpoints,
        tolerances,
    )
    target = (
        compare.GROUP_LISTING_MERGED_PVAL,
        "CSS_STATS.pval",
        f"{_endpoint_stratum(paramcd)} TRTPN={level}",
    )
    _assert_listing_verdicts(groups, target, compare.FAIL, reason, value)


# --- Settings (S02-TRN-01) ------------------------------------------------------------------------

#: The variable settings as the SAS program writes them [whitepapers/WPCT/WPCT-F.07.02.sas:108-124],
#: with ``t_var`` upper-cased: each differs from the frames' spelling only in case.
_SAS_SPELLINGS = {
    "t_var": "TRTP_SHORT",
    "tn_var": "trtpn",
    "c_var": "chg",
    "b_var": "base",
    "p_fl": "saffl",
    "a_fl": "anl01fl",
}


@pytest.mark.parametrize(
    "settings",
    [
        *({name: value} for name, value in _SAS_SPELLINGS.items()),
        _SAS_SPELLINGS,
        {"c_var": "  chg  ", "b_var": " Base"},
    ],
    ids=[*_SAS_SPELLINGS, "all", "blank-padded"],
)
def test_s02_trn_01_sas_spellings_fold_to_schema(settings: dict[str, str]) -> None:
    """S02-TRN-01: a SAS spelling of a variable setting is stored as the frames spell it.

    SAS matches variable names ignoring case and ``%let`` drops surrounding blanks, so the
    program's own ``%let`` values give the program's settings, spelled as ``wpct.schemas`` spells
    the analysis variables.
    """
    config = wpct_f_07_02.Config(**settings)
    assert config == _DEFAULT
    for name in settings:
        assert getattr(config, name) == getattr(_DEFAULT, name)
        assert getattr(config, name) in ANA_VARIABLES[PROGRAM]


@pytest.mark.parametrize(
    ("settings", "error", "match"),
    [
        ({"t_var": "PARAM"}, ValueError, "Config.t_var must name trtp_short"),
        ({"t_var": "avisit"}, ValueError, "Config.t_var must name trtp_short"),
        ({"tn_var": "BASE"}, ValueError, "Config.tn_var must name TRTPN"),
        ({"tn_var": "AVISITN"}, ValueError, "Config.tn_var must name TRTPN"),
        ({"c_var": "PARAM"}, ValueError, "Config.c_var must name a numeric analysis variable"),
        ({"b_var": "param"}, ValueError, "Config.b_var must name a numeric analysis variable"),
        ({"p_fl": "CHG"}, ValueError, "Config.p_fl must name a character analysis variable"),
        ({"a_fl": "avisitn"}, ValueError, "Config.a_fl must name a character analysis variable"),
        ({"b_var": "TRTPN"}, ValueError, "Config.b_var must not name tn_var"),
        ({"b_var": "trtpn"}, ValueError, "Config.b_var must not name tn_var"),
        ({"c_var": "NOPE"}, ValueError, "'c_var': 'NOPE'"),
        ({"t_var": None}, ValueError, "'t_var': None"),
        ({"p_fl": "\u017faffl"}, ValueError, "'p_fl'"),
        ({"b_visn": "0"}, TypeError, "Config.b_visn must be a number or None, got '0'"),
        ({"b_visn": True}, TypeError, "Config.b_visn must be a number or None, got True"),
    ],
    ids=[
        "t_var-PARAM",
        "t_var-avisit",
        "tn_var-BASE",
        "tn_var-AVISITN",
        "c_var-character",
        "b_var-character",
        "p_fl-numeric",
        "a_fl-numeric",
        "b_var-tn_var",
        "b_var-tn_var-lower",
        "unknown-name",
        "not-a-string",
        "non-ascii-name",
        "b_visn-string",
        "b_visn-bool",
    ],
)
def test_s02_trn_01_rejects_settings_at_construction(
    settings: dict[str, object], error: type[Exception], match: str
) -> None:
    """S02-TRN-01: a setting the fixed export schemas cannot carry is refused when built.

    ``t_var`` and ``tn_var`` name the ``css_stats`` BY columns and key and the CLASS effect, so
    only ``trtp_short`` and ``TRTPN`` keep those shapes. The analysis and baseline variables are
    numeric, the flags character, the baseline covariate is not the CLASS variable, and
    ``b_visn`` is a number or ``None``. Each refusal names the setting before any input is read.
    """
    with pytest.raises(error, match=match):
        wpct_f_07_02.Config(**settings)


def test_s02_flt_03_missing_baseline_visit_accepted() -> None:
    """S02-FLT-03, S02-TRN-01: ``b_visn=None`` is the SAS missing value ``.`` and is accepted.

    ``Config`` keeps ``None`` as given, and the GLM remains enabled. SAS would then drop the
    records with a missing ``AVISITN`` through ``where also avisitn ne .``, ported by
    ``selection.exclude_visit``; this settings-only test runs neither that filter nor the GLM.
    """
    config = wpct_f_07_02.Config(b_visn=None)
    assert config.b_visn is None
    assert config.glm_enabled
