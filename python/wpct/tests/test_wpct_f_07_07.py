"""End-to-end equivalence of the WPCT-F.07.07 port with its SAS references.

Unit under test: ``wpct.programs.wpct_f_07_07.run`` composed with ``wpct.compare.compare_program``
(``python/wpct/docs/sas_specification.md`` Part B). The default run and its comparison groups come
from the session fixtures of ``conftest.py``; the edge states run the driver on an altered copy of
the ADVS frame (``advs_07_07``), so none re-reads the transport file or writes into the checkout.

- Repository references (AAP 0.7.3): the 29 pinned log checkpoints of
  ``whitepapers/WPCT/outputs_sas/WPCT-F.07.07.log`` and the 128 ParameterEstimates values plus the
  18 merged ``pval`` cells of ``WPCT-F.07.07.lst``, 146 listing-based values in all. Every SAS value
  is parsed by ``wpct.sas_reference`` and judged by ``wpct.compare`` under ``tolerances.json``; this
  module states no listing value, no log count and no tolerance. Where a test checks a record or
  variable count, it reads it at run time from the parsed log line (the ``sas_checkpoints``
  fixture) by its ``LOG_CHECKPOINT_MAP`` dataset name and occurrence.
- No silent pass (AAP 0.7.4): each equivalence test asserts the exact number of comparison rows and
  that every one is PASS, so a FAIL or a NOT COMPARED row fails the test.
- Edge states (AAP 0.6.4): the halt on a missing ``CHG`` (S07-FLT-04), the GLM skipped by setting,
  an empty population after the derivation, an empty derivation input (U-03) and a single study.
  Variable names stored in lower case give the default run (S07-DEP-assert_var_exist).
- Settings (S07-TRN-01): built only, without a run. SAS spellings fold to the schema spelling, and
  a setting the export schemas cannot carry is refused.
"""

from __future__ import annotations

import itertools
import math
from collections.abc import Mapping

import numpy as np
import pandas as pd
import pytest

from wpct import compare, derivation, model
from wpct.errors import AnalysisHalted
from wpct.export import write_program_outputs
from wpct.programs import MODEL_POOLED, MODEL_STUDY, ProgramResult, wpct_f_07_07
from wpct.sas_reference import GLM_TABLE_MAP, LOG_CHECKPOINT_MAP, SasCheckpoint
from wpct.schemas import ANA_VARIABLES, POOLED_STUDYID, PROGRAM_07_07, export_spec

PROGRAM = PROGRAM_07_07
PARAMCD = "DIABP"
#: The ATPTN strata of S07-FLT-01, in SAS loop order (S07-TRN-07).
TIMEPOINTS = (815.0, 817.0)
#: The PROC GLM ``CLASS trtpn (ref="0")`` reference level (S07-TRN-01).
REF_TRTN = 0.0
#: The 07.07 datasets ``ProgramResult.datasets`` carries, in export order.
DATASETS = ("advs_min", "css_anadata", "css_stats", "parameter_estimates")
#: The run note of an empty derivation input, spelled out as the plan words it for the report.
U03_NOTE = "SAS behaviour for empty derivation input not established (U-03)"


# --- Helpers --------------------------------------------------------------------------------------


def assert_all_pass(group: compare.ComparisonGroup, expected_count: int) -> None:
    """Assert that ``group`` holds exactly ``expected_count`` rows and that every row is PASS.

    A FAIL or a NOT COMPARED row fails the assertion, so a group can never pass by comparing
    nothing (AAP 0.7.4).
    """
    statuses = compare.count_statuses([group])
    problems = [
        f"{row.quantity} [{row.stratum}] {row.status}: {row.reason}"
        for row in group.rows
        if row.status != compare.PASS
    ]
    expected = {compare.PASS: expected_count, compare.FAIL: 0, compare.NOT_COMPARED: 0}
    assert statuses == expected, f"{group.program} {group.key}: {statuses}; {problems}"


def _group(groups: tuple[compare.ComparisonGroup, ...], key: str) -> compare.ComparisonGroup:
    """The one comparison group of ``groups`` with ``key``."""
    matches = [group for group in groups if group.key == key]
    assert len(matches) == 1, f"expected one {key} group, found {len(matches)}"
    return matches[0]


def _sas_checkpoint(
    sas_checkpoints: Mapping[str, tuple[SasCheckpoint, ...]], dataset: str, occurrence: int = 1
) -> SasCheckpoint:
    """The one parsed 07.07 log checkpoint named ``dataset`` at ``occurrence``, verified.

    ``dataset`` and ``occurrence`` are the ``LOG_CHECKPOINT_MAP`` identifiers; the counts are those
    ``wpct.sas_reference.parse_log_checkpoints`` reads from the pinned line of
    ``whitepapers/WPCT/outputs_sas/WPCT-F.07.07.log`` at run time, so no test states a SAS count
    (AAP 0.7.3). A line that failed verification carries no count and fails with its problem.
    """
    matches = [
        sas
        for sas in sas_checkpoints[PROGRAM]
        if (sas.spec.SAS_DATASET, sas.spec.OCCURRENCE) == (dataset, occurrence)
    ]
    assert len(matches) == 1, f"expected one {dataset} #{occurrence} checkpoint, found {matches}"
    (sas,) = matches
    assert sas.ok, sas.problem
    return sas


def _where(frame: pd.DataFrame, **values: object) -> pd.DataFrame:
    """The rows of ``frame`` whose columns equal ``values``."""
    mask = np.ones(len(frame), dtype=bool)
    for column, value in values.items():
        mask &= (frame[column] == value).to_numpy(dtype=bool)
    return frame.loc[mask]


def _rule_class(column: str) -> str:
    """The one tolerance class whose ``compare.compared_fields()`` (``wpct.schemas``) name
    ``column``, matched case-insensitively as SAS names are."""
    owners = [
        key
        for key, fields in compare.compared_fields().items()
        if column.upper() in {field.upper() for field in fields}
    ]
    assert len(owners) == 1, (column, owners)
    return owners[0]


def _run(advs: pd.DataFrame, paths, config: wpct_f_07_07.Config | None = None) -> ProgramResult:
    """Run the driver on an already-read ADVS frame, with the program's settings by default."""
    return wpct_f_07_07.run(wpct_f_07_07.Config() if config is None else config, paths, advs=advs)


def _assert_schema_shape(result: ProgramResult, name: str, nobs: int | None = None) -> None:
    """Assert dataset ``name`` has its ``wpct.schemas`` columns and, if given, ``nobs`` rows."""
    frame = result.dataset(name)
    assert list(frame.columns) == list(export_spec(PROGRAM, name).columns), name
    if nobs is not None:
        assert len(frame) == nobs, name


def _assert_no_strata(result: ProgramResult) -> None:
    """Assert that the parameter loop ran zero times: no stratum checkpoint and no model."""
    assert [cp.SAS_DATASET for cp in result.checkpoints if cp.PARAMCD is not None] == []
    assert result.models == ()
    for name in ("css_anadata", "css_stats", "parameter_estimates"):
        _assert_schema_shape(result, name, 0)


def _assert_pval_mapping(result: ProgramResult) -> None:
    """Assert that each ``css_stats`` cell holds the Probt of its own model and TRTPN level.

    S07-TRN-08: the per-study rows take the TRTPN rows of the model with the same ``studynum``,
    and the pooled rows those of the pooled model, whose rows carry ``studynum = &stdyn``
    (``pooled_studynum``). The reference level's Probt is missing, and so is its ``pval``.
    """
    stats = result.dataset("css_stats")
    estimates = result.dataset("parameter_estimates")
    treatments = estimates.loc[(estimates["EFFECT"] == "TRTPN").to_numpy(dtype=bool)]
    levels = treatments["LEVEL"].astype(float)
    kinds = {
        (record.PARAMCD, record.ATPTN, record.STUDYNUM): record.MODEL for record in result.models
    }
    assert len(stats) > 0
    for cell in stats.to_dict(orient="records"):
        studynum = cell["studynum"]
        expected_kind = MODEL_POOLED if studynum == result.pooled_studynum else MODEL_STUDY
        assert kinds[(cell["PARAMCD"], cell["ATPTN"], studynum)] == expected_kind, cell
        source = treatments.loc[
            (
                (treatments["PARAMCD"] == cell["PARAMCD"])
                & (treatments["ATPTN"] == cell["ATPTN"])
                & (treatments["STUDYNUM"] == studynum)
                & (levels == cell["TRTPN"])
            ).to_numpy(dtype=bool)
        ]
        assert len(source) == 1, cell
        probt = source["PROBT"].iloc[0]
        if cell["TRTPN"] == REF_TRTN:
            assert math.isnan(probt), cell
            assert math.isnan(cell["pval"]), cell
        else:
            assert cell["pval"] == probt, cell


# --- Equivalence against the repository references ------------------------------------------------


def test_s07_flt_01_to_trn_08_log_checkpoints_all_pass(result_07_07, comparisons, sas_checkpoints):
    """The 29 pinned log record counts agree exactly (S07-FLT-01 to S07-TRN-08, LMM-*).

    The driver records its checkpoints in log order under the names and occurrences of
    ``LOG_CHECKPOINT_MAP``, with the counts that ``wpct.sas_reference`` reads at run time from
    [whitepapers/WPCT/outputs_sas/WPCT-F.07.07.log:32-1581]: the observation count of every line
    and the variable count where the line is compared as ``obs/vars``. Every pinned line must pass
    verification, so a missing reference count can never match a missing driver count. The
    program-level checkpoints come first, then the parameter's, then each ATPTN stratum's in SAS
    loop order (S07-TRN-07).
    """
    assert_all_pass(_group(comparisons[PROGRAM], compare.GROUP_LOG_CHECKPOINTS), 29)
    references = sas_checkpoints[PROGRAM]
    unverified = [sas.problem for sas in references if not sas.ok]
    assert unverified == [], f"pinned SAS log lines failed verification: {unverified}"
    assert len(references) == 29
    observed = [
        (cp.SAS_DATASET, cp.OCCURRENCE, cp.PARAMCD, cp.ATPTN, cp.NOBS, cp.NVARS)
        for cp in result_07_07.checkpoints
    ]
    expected = [
        (
            sas.spec.SAS_DATASET,
            sas.spec.OCCURRENCE,
            sas.spec.PARAMCD,
            sas.spec.ATPTN,
            sas.NOBS,
            sas.NVARS,
        )
        for sas in references
    ]
    assert observed == expected
    strata = [(cp.PARAMCD, cp.ATPTN) for cp in result_07_07.checkpoints]
    assert [stratum for stratum, _ in itertools.groupby(strata)] == [
        (None, None),
        (PARAMCD, None),
        *((PARAMCD, atptn) for atptn in TIMEPOINTS),
    ]
    recorded = [
        (cp.SAS_DATASET, cp.OCCURRENCE, cp.PARAMCD, cp.ATPTN) for cp in result_07_07.checkpoints
    ]
    mapped = [
        (spec.SAS_DATASET, spec.OCCURRENCE, spec.PARAMCD, spec.ATPTN)
        for spec in LOG_CHECKPOINT_MAP[PROGRAM]
    ]
    assert recorded == mapped


def test_s07_mod_02_03_parameter_estimates_match_listing(comparisons):
    """Every listing ParameterEstimates value agrees at displayed precision.

    Covers S07-MOD-02 (per study) and S07-MOD-03 (pooled): 4 per-study tables of 5 rows and 2
    pooled tables of 6 rows, 4 values each, 128 in all.
    """
    group = _group(comparisons[PROGRAM], compare.GROUP_LISTING_PARAMETER_ESTIMATES)
    assert group.rounded
    assert_all_pass(group, 128)


def test_s07_trn_08_merged_pval_matches_listing(comparisons):
    """The merged ``css_stats`` pval cells agree with the listing's TRTPN Probt (S07-TRN-08).

    3 TRTPN levels for studynum 1, 2 and pooled per timepoint: 9 cells each, 18 in all.
    """
    group = _group(comparisons[PROGRAM], compare.GROUP_LISTING_MERGED_PVAL)
    assert group.rounded
    assert group.heading_note == compare.MERGED_PVAL_NOTE
    assert_all_pass(group, 18)


def test_s07_mod_02_03_and_trn_08_listing_based_total_146(comparisons):
    """S07-MOD-02, S07-MOD-03 and S07-TRN-08: 128 + 18 = 146 listing-based values, all PASS.

    The 128 ParameterEstimates values (S07-MOD-02 per study, S07-MOD-03 pooled) and the 18 merged
    pval cells (S07-TRN-08) total 146, none NOT COMPARED.
    """
    groups = comparisons[PROGRAM]
    listing = [
        _group(groups, compare.GROUP_LISTING_PARAMETER_ESTIMATES),
        _group(groups, compare.GROUP_LISTING_MERGED_PVAL),
    ]
    assert compare.count_statuses(listing) == {
        compare.PASS: 146,
        compare.FAIL: 0,
        compare.NOT_COMPARED: 0,
    }


def test_s07_mod_03_pooled_studynum_aliased(result_07_07, tolerances):
    """The constant pooled ``studynum`` is aliased by the Intercept (S07-MOD-03, D-06, DIV-01).

    Its row has estimate 0, flag ``B`` (``ALIASED``) and missing SE, t and p
    [whitepapers/WPCT/outputs_sas/WPCT-F.07.07.lst:47,147], so every other pooled row equals the
    fit of ``CHG = BASE TRTPN`` with ``CLASS TRTPN (REF="0")`` on the same pooled records, within
    the ``glm_estimates`` rule of ``tolerances.json`` for estimate, SE and t and its ``p_values``
    rule for p.
    """
    css_anadata = result_07_07.dataset("css_anadata")
    estimates = result_07_07.dataset("parameter_estimates")
    pooled_studynum = result_07_07.pooled_studynum
    pooled_tables = [spec for spec in GLM_TABLE_MAP[PROGRAM] if spec.MODEL == MODEL_POOLED]
    assert [spec.ATPTN for spec in pooled_tables] == list(TIMEPOINTS)
    for spec in pooled_tables:
        rows = _where(estimates, PARAMCD=PARAMCD, ATPTN=spec.ATPTN, STUDYNUM=pooled_studynum)
        rows = rows.reset_index(drop=True)
        # D-05, D-07: one row per design column, in the SAS column order the listing shows.
        assert list(zip(rows["EFFECT"], rows["LEVEL"], strict=True)) == list(spec.parameters)

        aliased = rows.loc[(rows["EFFECT"] == "STUDYNUM").to_numpy(dtype=bool)]
        assert len(aliased) == 1
        studynum_row = aliased.iloc[0]
        assert studynum_row["ESTIMATE"] == 0.0
        assert bool(studynum_row["ALIASED"])
        for column in ("STDERR", "TVALUE", "PROBT"):
            assert math.isnan(studynum_row[column]), column

        pooled_records = _where(
            css_anadata, STUDYID=POOLED_STUDYID, PARAMCD=PARAMCD, ATPTN=spec.ATPTN
        )
        refit, fit = model.glm_solution(pooled_records, "CHG", ["BASE", "TRTPN"], "TRTPN", 0)
        assert fit.STATUS == model.STATUS_FITTED
        kept = rows.loc[(rows["EFFECT"] != "STUDYNUM").to_numpy(dtype=bool)].reset_index(drop=True)
        assert list(zip(kept["EFFECT"], kept["LEVEL"], strict=True)) == list(
            zip(refit["EFFECT"], refit["LEVEL"], strict=True)
        )
        assert list(kept["ALIASED"]) == list(refit["ALIASED"])
        for column in ("ESTIMATE", "STDERR", "TVALUE", "PROBT"):
            class_key = _rule_class(column)
            pairs = zip(
                kept[column].to_numpy(dtype="float64"),
                refit[column].to_numpy(dtype="float64"),
                strict=True,
            )
            for position, (pooled_value, refit_value) in enumerate(pairs):
                outcome = compare.check_value(class_key, pooled_value, refit_value, tolerances)
                assert outcome.status == compare.PASS, (spec.ATPTN, column, position, outcome)
        (pooled_model,) = [
            record
            for record in result_07_07.models
            if record.MODEL == MODEL_POOLED and record.ATPTN == spec.ATPTN
        ]
        assert (pooled_model.NOBS, pooled_model.DFE) == (fit.NOBS, fit.DFE)


def test_s07_mod_02_03_model_records(result_07_07, sas_checkpoints):
    """Per timepoint, studies 1 and 2 are fitted, then the pooled model (S07-MOD-02, S07-MOD-03).

    The pooled records are a copy of both studies' records, so the pooled ``NOBS`` is their sum
    and half the timepoint's records, the CSS_NEXTTIMEPT count of the same ATPTN read at run time
    from [whitepapers/WPCT/outputs_sas/WPCT-F.07.07.log:936,1295] (S07-TRN-07); each fit retains
    the Intercept, BASE and two TRTPN indicators, so ``DFE = NOBS - 4`` (D-05). The pooled
    ``studynum`` is ``&stdyn``, the 3 studies including the pooled one (S07-TRN-07).
    """
    assert result_07_07.pooled_studynum == 3
    assert len(result_07_07.models) == 3 * len(TIMEPOINTS)
    for occurrence, atptn in enumerate(TIMEPOINTS, start=1):
        timepoint = _sas_checkpoint(sas_checkpoints, "CSS_NEXTTIMEPT", occurrence)
        assert (timepoint.spec.PARAMCD, timepoint.spec.ATPTN) == (PARAMCD, atptn)
        records = [record for record in result_07_07.models if record.ATPTN == atptn]
        assert [(record.STUDYNUM, record.MODEL) for record in records] == [
            (1, MODEL_STUDY),
            (2, MODEL_STUDY),
            (result_07_07.pooled_studynum, MODEL_POOLED),
        ]
        first, second, pooled = records
        assert 2 * pooled.NOBS == timepoint.NOBS
        assert pooled.NOBS == first.NOBS + second.NOBS
        for record in records:
            assert record.PARAMCD == PARAMCD
            assert record.STATUS == model.STATUS_FITTED
            assert record.DFE == record.NOBS - 4


def test_pval_merge_mapping(result_07_07):
    """Each statistics row carries its own model's TRTPN Probt (S07-TRN-08).

    Per timepoint: studynum 1, 2 and the pooled 3, each with TRTPN 0, 54 and 81; studies map by
    ``studynum``, pooled rows by ``&stdyn``, and the reference level is missing.
    """
    stats = result_07_07.dataset("css_stats")
    for atptn in TIMEPOINTS:
        cells = _where(stats, PARAMCD=PARAMCD, ATPTN=atptn)
        assert sorted(zip(cells["studynum"], cells["TRTPN"], strict=True)) == [
            (studynum, trtpn) for studynum in (1, 2, 3) for trtpn in (0.0, 54.0, 81.0)
        ]
    assert len(stats) == 9 * len(TIMEPOINTS)
    _assert_pval_mapping(result_07_07)


def test_s07_trn_03_to_06_derived_and_analysis_sets(result_07_07, sas_checkpoints):
    """The derived MIN records and the pooled analysis set have the SAS shapes and keys.

    Every count is that of the cited log line, read at run time. ``advs_min`` (S07-TRN-03,
    LMM-TRN-05 to LMM-TRN-07, S07-FLT-02): the post-baseline MIN records and variables of
    ADVS_MIN after the post-baseline filter [whitepapers/WPCT/outputs_sas/WPCT-F.07.07.log:229],
    flagged ``ANL02FL='Y'``, ``AVISITN = 9900 + 10*1 + 2`` and ``CHG = AVAL - BASE``.
    ``css_anadata`` (S07-TRN-04 to S07-TRN-06, D-16): the records and variables of CSS_ANADATA
    numbered by study [whitepapers/WPCT/outputs_sas/WPCT-F.07.07.log:427], half of them the pooled
    copy with ``USUBJID`` starting ``P``, numbered 1, 2 and pooled last. LMM-FLT-02 omits one
    baseline record per omitted-baseline WARNING line
    [whitepapers/WPCT/outputs_sas/WPCT-F.07.07.log:124-178], as many as LMM_BASE_ANL loses
    between NODUPKEY and the both-phases merge
    [whitepapers/WPCT/outputs_sas/WPCT-F.07.07.log:108,186], and as many post-baseline records as
    LMM_POST_ANL loses between the same steps
    [whitepapers/WPCT/outputs_sas/WPCT-F.07.07.log:117,195] (LMM-TRN-04, LMM-TRN-06).
    """
    sas_advs_min = _sas_checkpoint(sas_checkpoints, "ADVS_MIN", 2)
    advs_min = result_07_07.dataset("advs_min")
    _assert_schema_shape(result_07_07, "advs_min", sas_advs_min.NOBS)
    assert advs_min.shape == (sas_advs_min.NOBS, sas_advs_min.NVARS)
    assert advs_min["AVISIT"].str.startswith("Post-baseline").all()
    assert (advs_min["ANL02FL"] == "Y").all()
    assert (advs_min["AVISITN"] == 9912.0).all()
    assert (advs_min["CHG"] == advs_min["AVAL"] - advs_min["BASE"]).all()
    assert not advs_min.duplicated(list(export_spec(PROGRAM, "advs_min").key)).any()

    sas_css_anadata = _sas_checkpoint(sas_checkpoints, "CSS_ANADATA", 2)
    css_anadata = result_07_07.dataset("css_anadata")
    _assert_schema_shape(result_07_07, "css_anadata", sas_css_anadata.NOBS)
    assert css_anadata.shape == (sas_css_anadata.NOBS, sas_css_anadata.NVARS)
    assert "studynum" in css_anadata.columns
    assert not css_anadata.duplicated(list(export_spec(PROGRAM, "css_anadata").key)).any()
    pooled = (css_anadata["STUDYID"] == POOLED_STUDYID).to_numpy(dtype=bool)
    assert pooled.sum() == len(css_anadata) // 2
    assert css_anadata.loc[pooled, "USUBJID"].str.startswith("P").all()
    assert sorted(css_anadata.loc[pooled, "USUBJID"].str[1:]) == sorted(
        css_anadata.loc[~pooled, "USUBJID"].str[1:]
    )
    numbering = css_anadata.groupby("STUDYID", sort=True)["studynum"].unique()
    assert {study: list(values) for study, values in numbering.items()} == {
        "CDISCPILOT01": [1],
        "CDISCPILOT02": [2],
        POOLED_STUDYID: [3],
    }

    omitted_base = _sas_checkpoint(sas_checkpoints, "LMM_BASE_OMITTED").NOBS
    base_nodupkey, base_merged = (
        _sas_checkpoint(sas_checkpoints, "LMM_BASE_ANL", occurrence).NOBS for occurrence in (2, 3)
    )
    post_nodupkey, post_merged = (
        _sas_checkpoint(sas_checkpoints, "LMM_POST_ANL", occurrence).NOBS for occurrence in (2, 3)
    )
    assert base_nodupkey - base_merged == omitted_base
    assert dict(result_07_07.omission_counts) == {
        "LMM_BASE_OMITTED": omitted_base,
        "LMM_POST_OMITTED": post_nodupkey - post_merged,
    }


# --- Edge states ----------------------------------------------------------------------------------


def test_missing_chg_halts(monkeypatch, advs_07_07, repo_paths, write_paths, sas_checkpoints):
    """A missing CHG in CSS_SAFANA halts the run before anything is exported (S07-FLT-04).

    ``%assert_var_nonmissing`` and ``%assert_continue`` stop all later steps
    [whitepapers/WPCT/WPCT-F.07.07.sas:219-220]: the halt carries the checkpoints up to
    CSS_SAFANA, whose record count is that of
    [whitepapers/WPCT/outputs_sas/WPCT-F.07.07.log:367] read at run time, since blanking one CHG
    removes no record, and the export of the halted result writes nothing under the tmp write root.
    """
    derive = derivation.derive_lastminmax_measure

    def derive_with_missing_chg(*args, **kwargs):
        dsout, counts = derive(*args, **kwargs)
        dsout = dsout.copy()
        selected = dsout["AVISIT"].str.startswith("Post-baseline") & (dsout["SAFFL"] == "Y")
        target = dsout.index[selected.to_numpy(dtype=bool)][0]
        dsout.loc[target, "CHG"] = np.nan
        return dsout, counts

    monkeypatch.setattr(derivation, "derive_lastminmax_measure", derive_with_missing_chg)
    with pytest.raises(AnalysisHalted) as excinfo:
        _run(advs_07_07, repo_paths)
    stop = excinfo.value
    assert str(stop) == "WPCT-F.07.07: assert_var_nonmissing failed: 1 missing CHG in CSS_SAFANA"
    assert (stop.program_id, stop.check) == (PROGRAM, "assert_var_nonmissing")
    assert stop.checkpoints[-1].SAS_DATASET == "CSS_SAFANA"
    assert stop.checkpoints[-1].NOBS == _sas_checkpoint(sas_checkpoints, "CSS_SAFANA").NOBS

    result = ProgramResult.from_halt(stop)
    assert result.halted
    assert dict(result.datasets) == {}
    assert write_program_outputs(result, write_paths) == ()
    assert list(write_paths.write_root.rglob("*")) == []


def test_unset_baseline_skips_glm(
    advs_07_07, repo_paths, listing_tables, sas_checkpoints, sas_exports, tolerances
):
    """A blank ``b_var`` skips both GLMs (S07-MOD-02, S07-MOD-03; S07-TRN-01).

    ``%if %length(&b_var) > 0 and %length(&ref_trtn) > 0`` is false
    [whitepapers/WPCT/WPCT-F.07.07.sas:338], so no model is fitted, the statistics carry no
    ``pval`` and no listing-based comparison can pass.
    """
    result = _run(advs_07_07, repo_paths, wpct_f_07_07.Config(b_var=None))
    assert result.glm_skipped
    assert result.models == ()
    _assert_schema_shape(result, "parameter_estimates", 0)
    stats = result.dataset("css_stats")
    assert "pval" not in stats.columns
    assert list(stats.columns) == [
        column for column in export_spec(PROGRAM, "css_stats").columns if column != "pval"
    ]
    assert len(stats) == 9 * len(TIMEPOINTS)

    groups = compare.compare_program(
        result, listing_tables[PROGRAM], sas_checkpoints[PROGRAM], sas_exports, tolerances
    )
    for key, count in (
        (compare.GROUP_LISTING_PARAMETER_ESTIMATES, 128),
        (compare.GROUP_LISTING_MERGED_PVAL, 18),
    ):
        group = _group(groups, key)
        assert len(group.rows) == count
        assert group.n_pass == 0
        assert all("GLM skipped by setting" in row.reason for row in group.rows)
    not_compared = _group(groups, compare.GROUP_NOT_COMPARED)
    assert any(row.reason == "not produced: GLM skipped by setting" for row in not_compared.rows)


def test_empty_population_after_derivation(advs_07_07, repo_paths, sas_checkpoints):
    """No safety-population record after the derivation is no error (S07-POP-01, S07-TRN-07).

    The derivation precedes the population filter, so ``advs_min`` keeps the post-baseline record
    count of [whitepapers/WPCT/outputs_sas/WPCT-F.07.07.log:229], read at run time, while
    CSS_SAFANA is empty; ``%util_labels_from_var`` then lists no parameter and the loop runs zero
    times. Every later dataset is header-only, with its schema columns.
    """
    advs_07_07["SAFFL"] = "N"
    result = _run(advs_07_07, repo_paths)
    assert not result.halted
    (safana,) = [cp for cp in result.checkpoints if cp.SAS_DATASET == "CSS_SAFANA"]
    assert safana.NOBS == 0
    _assert_no_strata(result)
    _assert_schema_shape(result, "advs_min", _sas_checkpoint(sas_checkpoints, "ADVS_MIN", 2).NOBS)
    assert (result.dataset("advs_min")["SAFFL"] == "N").all()
    assert result.pooled_studynum is None
    assert result.notes == ()


def test_empty_selection_before_derivation(monkeypatch, advs_07_07, repo_paths):
    """With no DIABP record the derivation receives no rows and the run is empty (U-03).

    SAS behaviour for empty derivation input is not established; the port returns header-only
    datasets with zero counts, and the run note reaches the not-compared group.
    """
    derive = derivation.derive_lastminmax_measure
    received: list[int] = []

    def spy(df, *args, **kwargs):
        received.append(len(df))
        return derive(df, *args, **kwargs)

    monkeypatch.setattr(derivation, "derive_lastminmax_measure", spy)
    advs = advs_07_07.loc[(advs_07_07["PARAMCD"] != PARAMCD).to_numpy(dtype=bool)]
    result = _run(advs, repo_paths)
    assert received == [0]
    assert not result.halted
    assert U03_NOTE in result.notes
    _assert_no_strata(result)
    _assert_schema_shape(result, "advs_min", 0)
    assert set(result.datasets) == set(DATASETS)
    assert dict(result.omission_counts) == {"LMM_BASE_OMITTED": 0, "LMM_POST_OMITTED": 0}
    assert result.pooled_studynum is None
    not_compared = compare.not_compared_rows(PROGRAM, result, [])
    assert any(row.reason == U03_NOTE for row in not_compared.rows)


def test_single_study_numbering(advs_07_07, repo_paths):
    """With CDISCPILOT02 removed, study 1 is numbered 1 and the pooled study 2 (S07-TRN-06).

    ``&stdyn`` counts the studies present, so the pooled model carries ``studynum = 2`` and the
    pooled statistics take their p-values through it (S07-TRN-08). The statistics hold exactly the
    12 cells DIABP x ATPTN 815, 817 x ``studynum`` 1 (CDISCPILOT01), 2 (pooled) x TRTPN 0, 54, 81,
    in timepoint loop order and sorted BY ``STUDYID TRTPN`` within each timepoint, the pooled study
    last (S07-TRN-07, D-16); each cell is compared on the ``css_stats`` record key of
    ``wpct.schemas`` and its ``STUDYID``, and carries its own model's TRTPN Probt.
    """
    advs = advs_07_07.loc[(advs_07_07["STUDYID"] != "CDISCPILOT02").to_numpy(dtype=bool)]
    result = _run(advs, repo_paths)
    assert result.pooled_studynum == 2
    assert [(record.ATPTN, record.STUDYNUM, record.MODEL) for record in result.models] == [
        (atptn, studynum, kind)
        for atptn in TIMEPOINTS
        for studynum, kind in ((1, MODEL_STUDY), (2, MODEL_POOLED))
    ]
    assert all(record.STATUS == model.STATUS_FITTED for record in result.models)
    stats = result.dataset("css_stats")
    # The record key's fields and order come from wpct.schemas alone; a key field no cell below
    # names fails with a KeyError rather than being left uncompared.
    columns = (*export_spec(PROGRAM, "css_stats").key, "STUDYID")
    cells = [
        {
            "PARAMCD": PARAMCD,
            "ATPTN": atptn,
            "STUDYID": studyid,
            "studynum": studynum,
            "TRTPN": trtpn,
        }
        for atptn in TIMEPOINTS
        for studyid, studynum in (("CDISCPILOT01", 1), (POOLED_STUDYID, 2))
        for trtpn in (0.0, 54.0, 81.0)
    ]
    assert list(zip(*(stats[column] for column in columns), strict=True)) == [
        tuple(cell[column] for column in columns) for cell in cells
    ]
    _assert_pval_mapping(result)


def test_s07_dep_assert_var_exist_lower_case_variable_names(advs_07_07, result_07_07, repo_paths):
    """S07-DEP-assert_var_exist: lower-case variable names in ADVS give the default run.

    SAS finds a variable whatever the case of its name
    [whitepapers/utilities/assert_var_exist.sas:23], and every later step addresses it by any
    spelling, so the datasets, checkpoints, models, omission counts, notes and pooled ``studynum``
    equal those of the default run. The frame passed in keeps its lower-case names.
    """
    lower = advs_07_07.rename(columns=str.lower)
    lower_names = list(lower.columns)
    result = _run(lower, repo_paths)

    assert lower_names != list(advs_07_07.columns)
    assert list(lower.columns) == lower_names
    assert list(result.datasets) == list(DATASETS)
    for name in DATASETS:
        pd.testing.assert_frame_equal(
            result.dataset(name), result_07_07.dataset(name), check_exact=True
        )
    assert result.checkpoints == result_07_07.checkpoints
    assert result.models == result_07_07.models
    assert dict(result.omission_counts) == dict(result_07_07.omission_counts)
    assert result.pooled_studynum == result_07_07.pooled_studynum
    assert result.notes == result_07_07.notes


# --- Settings (S07-TRN-01) ------------------------------------------------------------------------

#: The variable settings as the SAS program writes them [whitepapers/WPCT/WPCT-F.07.07.sas:96-117],
#: with ``t_var`` upper-cased: each differs from the frames' spelling only in case.
SAS_SPELLINGS = {
    "t_var": "TRTP_SHORT",
    "tn_var": "trtpn",
    "c_var": "chg",
    "b_var": "base",
    "p_fl": "saffl",
    "a_fl": "anl02fl",
}


@pytest.mark.parametrize(
    "settings",
    [
        *({name: value} for name, value in SAS_SPELLINGS.items()),
        SAS_SPELLINGS,
        {"c_var": "  chg  ", "a_fl": " Anl02fl"},
    ],
    ids=[*SAS_SPELLINGS, "all", "blank-padded"],
)
def test_s07_trn_01_sas_spellings_fold_to_schema(settings):
    """A SAS spelling of a variable setting is stored as the frames spell it (S07-TRN-01).

    SAS matches variable names ignoring case and ``%let`` drops surrounding blanks, so the
    program's own ``%let`` values give the program's settings, spelled as ``wpct.schemas`` spells
    the analysis variables.
    """
    config = wpct_f_07_07.Config(**settings)
    default = wpct_f_07_07.Config()
    assert config == default
    for name in settings:
        assert getattr(config, name) == getattr(default, name)
        assert getattr(config, name) in ANA_VARIABLES[PROGRAM]


@pytest.mark.parametrize(
    ("settings", "match"),
    [
        ({"t_var": "PARAM"}, "Config.t_var must name trtp_short"),
        ({"tn_var": "BASE"}, "Config.tn_var must name TRTPN"),
        ({"a_fl": "SAFFL"}, "Config.a_fl must name ANL02FL"),
        ({"a_fl": "param"}, "Config.a_fl must name ANL02FL"),
        ({"c_var": "PARAMCD"}, "Config.c_var must name a numeric analysis variable"),
        ({"b_var": "trtp_short"}, "Config.b_var must name a numeric analysis variable"),
        ({"p_fl": "ATPTN"}, "Config.p_fl must name a character analysis variable"),
        ({"b_var": "TRTPN"}, "Config.b_var must not name tn_var"),
        ({"b_var": "trtpn"}, "Config.b_var must not name tn_var"),
        ({"c_var": "NOPE"}, "'c_var': 'NOPE'"),
        ({"a_fl": "ANL01FL"}, "'a_fl': 'ANL01FL'"),
        ({"p_fl": "\u017faffl"}, "'p_fl'"),
    ],
    ids=[
        "t_var-PARAM",
        "tn_var-BASE",
        "a_fl-SAFFL",
        "a_fl-param",
        "c_var-character",
        "b_var-character",
        "p_fl-numeric",
        "b_var-tn_var",
        "b_var-tn_var-lower",
        "unknown-name",
        "07.02-flag",
        "non-ascii-name",
    ],
)
def test_s07_trn_01_rejects_settings_at_construction(settings, match):
    """A setting the fixed export schemas cannot carry is refused when built (S07-TRN-01).

    ``t_var`` and ``tn_var`` name the ``css_stats`` BY columns and key and the CLASS effect, and
    ``a_fl`` the flag the derivation creates beside its fixed group and carried variables, so only
    ``trtp_short``, ``TRTPN`` and ``ANL02FL`` keep those shapes. The analysis and baseline
    variables are numeric, the population flag character, and the baseline covariate is not the
    CLASS variable. Each refusal names the setting before any input is read.
    """
    with pytest.raises(ValueError, match=match):
        wpct_f_07_07.Config(**settings)
