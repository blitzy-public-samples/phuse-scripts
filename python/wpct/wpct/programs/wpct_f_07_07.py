"""Driver for WPCT-F.07.07: change from MIN baseline to MIN post-baseline by study, with pooling.

Ports ``whitepapers/WPCT/WPCT-F.07.07.sas`` without its graphics. :func:`run` reads ADVS, derives
the minimum baseline and post-baseline DIABP records, keeps the post-baseline ones, stacks a pooled
copy, numbers the studies, loops over the parameter and timepoint strata in SAS loop order,
computes the PROC SUMMARY statistics by study and treatment, fits PROC GLM ``CHG = BASE TRTPN /
SOLUTION`` with ``CLASS TRTPN (REF="0")`` per study and ``CHG = BASE TRTPN studynum`` on the pooled
records, merges the treatment p-values into the statistics and returns one
:class:`~wpct.programs.ProgramResult`. It records a :class:`~wpct.programs.Checkpoint` wherever the
SAS reference log reports the size of a dataset the port reproduces
[whitepapers/WPCT/outputs_sas/WPCT-F.07.07.log:32-1581].

Spec items (``python/wpct/docs/sas_specification.md``):

- Implemented here: S07-TRN-01 (:class:`Config`), S07-TRN-07 (strata, record order and
  ``&stdyn``) and S07-TRN-08 (per-study and pooled p-value merge).
- By composition: S07-FLT-01 and S07-TRN-02 (``selection.subset_param_timepoints``,
  ``selection.abbreviate_treatment``), S07-TRN-03 (``derivation.derive_lastminmax_measure``, items
  LMM-FLT-01 to LMM-TRN-07), S07-FLT-02 (``selection.post_baseline_filter``), S07-TRN-04
  (``selection.keep_variables`` with ``schemas.ANA_VARIABLES``), S07-POP-01 and S07-FLT-03
  (``selection.flag_filter``), S07-FLT-04 (``selection.assert_var_nonmissing``), S07-TRN-05
  (``selection.stack_pooled``), S07-TRN-06 (``selection.assign_studynum``), S07-FLT-05
  (``selection.assert_unique_keys``), S07-MOD-01 (``summary.proc_summary``), and S07-MOD-02 and
  S07-MOD-03 (``model.glm_solution``).
- Dependencies: S07-DEP-util_access_test_data, S07-DEP-assert_dset_exist,
  S07-DEP-assert_depend and S07-DEP-assert_var_exist through ``xpt_reader``;
  S07-DEP-derive_lastminmax_measure through ``derivation``; S07-DEP-assert_var_nonmissing and
  S07-DEP-assert_unique_keys through the halting checks; S07-DEP-assert_continue through
  ``xpt_reader`` for the call after the dependency assertion
  [whitepapers/WPCT/WPCT-F.07.07.sas:204] and through the halting checks (S07-FLT-04, S07-FLT-05)
  for the data-content calls [whitepapers/WPCT/WPCT-F.07.07.sas:220,245];
  S07-DEP-util_labels_from_var and S07-DEP-util_count_unique_values through the stratum order and
  ``&stdyn``; S07-DEP-boxplot_each_param_tp through the loops, statistics, models and merge.

DIV-01: the pooled "+ STUDY" term is ``studynum`` entered as a continuous, non-CLASS effect. Every
pooled record carries the same ``STUDYID`` and hence the same ``studynum``, so SAS aliases the term
with the Intercept (estimate 0, flag ``B``, missing standard error)
[whitepapers/WPCT/outputs_sas/WPCT-F.07.07.lst:47,147]. The port reproduces SAS, so the pooled
treatment estimates equal those of ``CHG = BASE TRTPN`` fitted to the pooled records.

ENV-05: ``%let m_ds = &ds._&LMM`` precedes ``%let lmm = MIN`` and resolves only when ``&m_ds`` is
used [whitepapers/WPCT/WPCT-F.07.07.sas:100,111]; :attr:`Config.m_ds` gives the resolved name,
``ADVS_MIN``. ENV-04: the SAS input, autocall and output paths are UNC paths and are not ported.

Excluded (S07-PRS-01, under the legacy-disposition rule that carries over only steps reaching a
selected record set, an exported dataset, a statistic or a ``SOLUTION`` row): the ``&c_mode``
label, the GTL template, Y-axis range, display formats, page blocks, ``css_plot``, titles,
footnotes, axis ticks, ODS PDF and PROC SGRENDER, and the ``util_delete_dsets`` cleanup of WORK
datasets.

Data reach: input is read only through ``xpt_reader``, from ``paths.input_xpt(PROGRAM_ID)``
(``data/adam/cdisc-split/advs.xpt``). The driver reads no SAS reference file, writes no file and
opens no connection; the export step writes the datasets it returns. A failed halting check raises
:class:`~wpct.errors.AnalysisHalted` carrying the checkpoints recorded before it, and nothing after
the check runs.
"""

from __future__ import annotations

import numbers
from collections.abc import Callable
from dataclasses import dataclass, fields

import numpy as np
import pandas as pd

from wpct import derivation, model, selection, summary, xpt_reader
from wpct.errors import AnalysisHalted
from wpct.paths import RepoPaths
from wpct.programs import (
    MODEL_POOLED,
    MODEL_STUDY,
    Checkpoint,
    ModelRecord,
    ProgramResult,
)
from wpct.sas_semantics import SAS_BLANK, is_char_missing, sas_sort
from wpct.schemas import (
    ANA_VARIABLES,
    CHAR,
    COLUMN_DTYPES,
    LMM_GRPVARS,
    LMM_INCL,
    LMM_ORDVARS,
    NUM,
    POOLED_STUDYID,
    PROGRAM_07_07,
    REQUIRED_INPUT_VARIABLES,
    export_spec,
)

# Annotations use built-in generics and ``X | None`` rather than the ``typing.Optional``/``Tuple``
# spelling, as ``wpct.programs`` and the 07.02 driver do (ruff pyupgrade UP006, UP035, UP045);
# the runtime types are the same.

__all__ = ["PROGRAM_ID", "U03_NOTE", "Config", "run"]

PROGRAM_ID = PROGRAM_07_07

#: The report note of a run whose derivation received no usable record (U-03).
U03_NOTE = "SAS behaviour for empty derivation input not established (U-03)"

_ADVS_MIN = "advs_min"
_CSS_ANADATA = "css_anadata"
_CSS_STATS = "css_stats"
_PARAMETER_ESTIMATES = "parameter_estimates"

#: ``pval``, the merged GLM p-value (S07-TRN-08); absent from ``css_stats`` when the GLM is skipped.
_PVAL = "pval"

#: The study number as ``selection.assign_studynum`` creates it and the schema spells it in
#: ``css_anadata`` and ``css_stats``; the pooled model takes it as its continuous third term.
_STUDYNUM = "studynum"
#: The port-only stratum column of ``parameter_estimates`` (upper case, ``wpct.schemas``).
_PE_STUDYNUM = "STUDYNUM"

_STUDYID = "STUDYID"
_USUBJID = "USUBJID"
_PARAMCD = "PARAMCD"
_ATPTN = "ATPTN"

#: The record key ``%assert_unique_keys`` checks on the pooled analysis set (S07-FLT-05)
#: [whitepapers/WPCT/WPCT-F.07.07.sas:244].
_UNIQUE_KEYS = (_STUDYID, _USUBJID, _PARAMCD, _ATPTN)


@dataclass(frozen=True)
class Config:
    """The SAS ``%let`` user settings of WPCT-F.07.07 (S07-TRN-01).

    Each field mirrors one setting [whitepapers/WPCT/WPCT-F.07.07.sas:96-117]. A variable setting
    is matched to the S07-TRN-04 analysis variables ignoring case and surrounding blanks, as SAS
    matches the ``%let`` value, and is stored as the port's frames spell the variable: upper case
    for ADaM variables, as the transport file stores them, ``trtp_short`` as the subset step
    creates it, and ``ANL02FL`` as the derivation upper-cases ``flvars=anl02fl``
    (``wpct.schemas``). ``Config(a_fl="anl02fl")`` therefore holds ``"ANL02FL"`` and equals
    ``Config()``.

    Attributes:
        ds: ``DS=ADVS``, the input dataset name; with ``lmm`` it names the derived dataset.
        param: ``param='DIABP'``, the blank-separated PARAMCD codes of the subset WHERE; every code
            in ``param_timepoints`` must be one of them.
        t_var: Treatment label variable, a PROC SUMMARY BY variable; ``trtp_short`` only.
        tn_var: Treatment number variable: sort key, BY variable and the CLASS variable; ``TRTPN``
            only.
        c_var: Change from baseline, the analysis and response variable; a numeric analysis
            variable.
        b_var: Baseline covariate, a numeric analysis variable other than ``tn_var``; ``None`` or
            ``""`` is the blank SAS setting and skips both GLMs.
        ref_trtn: ``REF=`` level of ``tn_var``; ``None`` or ``""`` is blank and skips both GLMs.
        p_fl: Population flag, a character analysis variable; records need ``'Y'`` (S07-POP-01).
        lmm: ``lmm=MIN``, the single change mode of the derivation (S07-TRN-03).
        a_fl: Analysis record flag the derivation sets, then required ``'Y'`` (S07-FLT-03);
            ``ANL02FL`` only.
        param_timepoints: The ``(PARAMCD, ATPTN values)`` pairs of the subset WHERE ``paramcd in
            (&param) and atptn in (815 817)`` (S07-FLT-01)
            [whitepapers/WPCT/WPCT-F.07.07.sas:146-148].

    ``m_lb=work`` names the SAS WORK library, which the port does not have. ``c_mode`` (a title
    label, S07-PRS-01), ``max_boxes_per_page`` (pagination) and ``outputs_folder`` (the PDF
    location, ENV-04) are presentation and environment settings and are not ported.

    Raises:
        ValueError: A variable setting names no analysis variable (the analysis variables fix
            every exported shape); ``t_var``, ``tn_var`` or ``a_fl`` names a variable other than
            ``trtp_short``, ``TRTPN`` or ``ANL02FL``, the columns the export schemas fix for
            them, where SAS accepts any variable; ``c_var`` or a set ``b_var`` names a character
            variable, or ``p_fl`` a numeric one; ``b_var`` names ``tn_var``, which would enter the
            MODEL twice; ``ds`` or ``lmm`` is not one blank-free name (SAS builds the dataset name
            ``&ds._&LMM`` from them, so a list of modes cannot run); or ``param_timepoints`` names
            a code that ``param`` does not list.
        TypeError: ``param_timepoints`` cannot be iterated, such as ``param_timepoints=5`` (the
            plain ``TypeError`` of iterating it), or a set ``ref_trtn`` is not a number.
    """

    ds: str = "ADVS"
    param: str = "DIABP"
    t_var: str = "trtp_short"
    tn_var: str = "TRTPN"
    c_var: str = "CHG"
    b_var: str | None = "BASE"
    ref_trtn: float | None = 0
    p_fl: str = "SAFFL"
    lmm: str = "MIN"
    a_fl: str = "ANL02FL"
    param_timepoints: tuple[tuple[str, tuple[float, ...]], ...] = (("DIABP", (815, 817)),)

    def __post_init__(self) -> None:
        analysis_variables = ANA_VARIABLES[PROGRAM_ID]
        # SAS variable names are case-insensitive ASCII and %let drops surrounding blanks, so each
        # variable setting is matched ignoring both and stored in the frames' spelling.
        spellings = {variable.upper(): variable for variable in analysis_variables}
        settings = ["t_var", "tn_var", "c_var", "p_fl", "a_fl"]
        if not _is_blank(self.b_var):
            settings.append("b_var")
        unknown: dict[str, object] = {}
        for name in settings:
            value = getattr(self, name)
            stripped = value.strip(SAS_BLANK) if isinstance(value, str) else ""
            spelling = spellings.get(stripped.upper()) if stripped.isascii() else None
            if spelling is None:
                unknown[name] = value
            else:
                object.__setattr__(self, name, spelling)
        if unknown:
            raise ValueError(
                f"{PROGRAM_ID}: Config settings name variables outside the analysis variables "
                f"({' '.join(analysis_variables)}): {unknown}"
            )
        # SAS takes any variable for t_var, tn_var and a_fl, but the export schemas fix the columns
        # they name: the css_stats BY columns and key, the sort and merge keys, the CLASS effect of
        # parameter_estimates, and the flag the derivation creates beside its fixed group and
        # carried variables, which advs_min and css_anadata export. Only the S07-TRN-01 names, the
        # defaults, produce those shapes.
        defaults = {field.name: field.default for field in fields(self)}
        for name in ("t_var", "tn_var", "a_fl"):
            value = getattr(self, name)
            if value != defaults[name]:
                raise ValueError(
                    f"{PROGRAM_ID}: Config.{name} must name {defaults[name]}, the column the "
                    f"export schemas fix for it, got {value!r}"
                )
        # PROC SUMMARY analyses c_var and the GLMs regress on b_var, so both are numeric; the flag
        # filter compares p_fl with 'Y', so it is character (a_fl is fixed above). The types are
        # those of the exported analysis records.
        column_types = export_spec(PROGRAM_ID, _CSS_ANADATA).column_types
        for name, code, kind in (
            ("c_var", NUM, "numeric"),
            ("b_var", NUM, "numeric"),
            ("p_fl", CHAR, "character"),
        ):
            value = getattr(self, name)
            if name in settings and column_types[value] != code:
                allowed = " ".join(
                    variable for variable in analysis_variables if column_types[variable] == code
                )
                raise ValueError(
                    f"{PROGRAM_ID}: Config.{name} must name a {kind} analysis variable "
                    f"({allowed}), got {value!r}"
                )
        if "b_var" in settings and self.b_var == self.tn_var:
            raise ValueError(
                f"{PROGRAM_ID}: Config.b_var must not name tn_var ({self.tn_var}), which would "
                "enter the MODEL twice"
            )
        for name in ("ds", "lmm"):
            value = getattr(self, name)
            if not isinstance(value, str) or len(value.split()) != 1:
                raise ValueError(
                    f"{PROGRAM_ID}: Config.{name} must be one blank-free name, got {value!r}"
                )
        if not isinstance(self.param, str) or not self.param.split():
            raise ValueError(
                f"{PROGRAM_ID}: Config.param must list at least one PARAMCD, got {self.param!r}"
            )
        # param and param_timepoints both state the subset's PARAMCD codes; refusing a code that
        # param does not list keeps the two from disagreeing silently. Malformed pairs are left to
        # selection.subset_param_timepoints, which names the problem.
        codes = {
            pair[0].rstrip(SAS_BLANK)
            for pair in self.param_timepoints
            if isinstance(pair, (tuple, list)) and pair and isinstance(pair[0], str)
        }
        unlisted = sorted(codes - set(self.param.split()))
        if unlisted:
            raise ValueError(
                f"{PROGRAM_ID}: Config.param_timepoints names PARAMCD values that Config.param "
                f"({self.param}) does not list: {' '.join(unlisted)}"
            )
        # A non-numeric REF= level would fail only once a stratum reaches the models.
        if not _is_blank(self.ref_trtn) and not _is_number(self.ref_trtn):
            raise TypeError(
                f"{PROGRAM_ID}: Config.ref_trtn must be a number, None or '', got {self.ref_trtn!r}"
            )

    @property
    def glm_enabled(self) -> bool:
        """True when the per-study and pooled GLMs run (S07-MOD-02, S07-MOD-03).

        Mirrors ``%if %length(&b_var) > 0 and %length(&ref_trtn) > 0``
        [whitepapers/WPCT/WPCT-F.07.07.sas:338].
        """
        return not _is_blank(self.b_var) and not _is_blank(self.ref_trtn)

    @property
    def m_ds(self) -> str:
        """``&ds._&LMM``, the derived dataset the analysis reads: ``ADVS_MIN`` by default.

        SAS assigns ``m_ds`` before ``lmm`` and resolves it only when used (ENV-05); the port
        passes the derived frame on in memory and keeps the name for reference.
        """
        return f"{self.ds}_{self.lmm}"


def _is_blank(value: object) -> bool:
    """True for ``None`` or an all-blank string, the port's forms of a blank macro variable.

    ``%let`` drops leading and trailing blanks, so a setting of blanks has ``%length`` 0.
    """
    return value is None or (isinstance(value, str) and is_char_missing(value))


def _is_number(value: object) -> bool:
    return isinstance(value, numbers.Real) and not isinstance(value, bool)


class _CheckpointLog:
    """The checkpoints of one run, numbered per SAS dataset name in recording order."""

    def __init__(self) -> None:
        self._records: list[Checkpoint] = []
        self._occurrences: dict[str, int] = {}

    def record(
        self,
        name: str,
        data: pd.DataFrame | int,
        paramcd: str | None = None,
        atptn: float | None = None,
        *,
        nvars: bool = False,
    ) -> None:
        """Record the size of ``data`` as the next occurrence of SAS dataset ``name``.

        ``data`` is a frame, or a bare record count such as a derivation count. ``nvars`` fills the
        variable count of a frame, only where the SAS log line is mapped as ``obs/vars``.
        """
        if isinstance(data, pd.DataFrame):
            nobs, variables = len(data), (len(data.columns) if nvars else None)
        elif nvars:
            raise ValueError(f"{PROGRAM_ID}: checkpoint {name} has a count but no variables")
        else:
            nobs, variables = data, None
        occurrence = self._occurrences.get(name, 0) + 1
        self._occurrences[name] = occurrence
        self._records.append(
            Checkpoint(
                SAS_DATASET=name,
                OCCURRENCE=occurrence,
                PARAMCD=paramcd,
                ATPTN=None if atptn is None else float(atptn),
                NOBS=nobs,
                NVARS=variables,
            )
        )

    @property
    def checkpoints(self) -> tuple[Checkpoint, ...]:
        return tuple(self._records)


def _record_derivation(
    log: _CheckpointLog, counts: derivation.LmmCounts, config: Config
) -> dict[str, int]:
    """Record the derivation's log counts in log order and return its omission counts (S07-TRN-03).

    The SAS log reports ``LMM_BASE`` and ``LMM_POST``
    [whitepapers/WPCT/outputs_sas/WPCT-F.07.07.log:79-80], then per change mode the SQL rows, the
    rows left by NODUPKEY, one WARNING per baseline record without a post-baseline record, and the
    two merge results [whitepapers/WPCT/outputs_sas/WPCT-F.07.07.log:95-195]. ``LMM_BASE_OMITTED``
    counts those warnings. A derivation reporting no mode records its mode counts once, as 0.
    """
    log.record("LMM_BASE", counts.LMM_BASE)
    log.record("LMM_POST", counts.LMM_POST)
    modes = counts.modes or (
        derivation.LmmModeCounts(MODE=config.lmm.upper(), FLAG=config.a_fl.upper(), IDX=1),
    )
    for mode in modes:
        log.record("LMM_BASE_ANL", mode.SQL_BASE)
        log.record("LMM_POST_ANL", mode.SQL_POST)
        log.record("LMM_BASE_ANL", mode.NODUPKEY_BASE)
        log.record("LMM_POST_ANL", mode.NODUPKEY_POST)
        log.record("LMM_BASE_OMITTED", mode.OMITTED_BASE)
        log.record("LMM_BASE_ANL", mode.MERGED_BASE)
        log.record("LMM_POST_ANL", mode.MERGED_POST)
    return {
        "LMM_BASE_OMITTED": sum(mode.OMITTED_BASE for mode in modes),
        "LMM_POST_OMITTED": sum(mode.OMITTED_POST for mode in modes),
    }


def _halting_check(log: _CheckpointLog, check: Callable[..., pd.DataFrame], *args: object) -> None:
    """Run one halting check, re-raising its halt with the checkpoints recorded so far.

    ``%assert_continue`` stops all further processing
    [whitepapers/utilities/assert_continue.sas:22-39], so the halt carries the record counts logged
    before it and nothing after the check runs (S07-FLT-04, S07-FLT-05). ``check`` is passed as
    ``selection.<name>`` and so resolved at call time.
    """
    try:
        check(*args)
    except AnalysisHalted as exc:
        raise AnalysisHalted(
            exc.message,
            program_id=exc.program_id,
            check=exc.check,
            checkpoints=log.checkpoints,
        ) from exc


def _trimmed(series: pd.Series) -> pd.Series:
    """Character values as SAS compares them: trailing blanks dropped, a missing value as ``''``."""
    return series.map(lambda value: "" if is_char_missing(value) else str(value).rstrip(SAS_BLANK))


def _study_count(css_anadata: pd.DataFrame) -> int:
    """``&stdyn``: the distinct non-missing ``STUDYID`` values, the pooled study included.

    Ports ``%util_count_unique_values(css_anadata, studyid, stdyn)``, whose SQL
    ``count(unique(studyid))`` ignores a missing value (S07-TRN-07)
    [whitepapers/utilities/util_count_unique_values.sas:38-42]. 0 for an empty population.
    """
    studies = _trimmed(css_anadata[_STUDYID])
    return int(studies[studies != ""].nunique())


def _parameter_codes(css_anadata: pd.DataFrame) -> tuple[pd.Series, list[str]]:
    """Return the trimmed ``PARAMCD`` of each record and the codes to loop over (S07-TRN-07).

    ``%util_labels_from_var(css_anadata, paramcd, param)`` counts the distinct non-missing codes and
    lists them in PROC SORT order [whitepapers/utilities/util_labels_from_var.sas:59-82], so the
    loop takes each code present once, ascending in the ``sas_sort`` order. S07-FLT-01 admits only
    configured codes, so no missing code reaches the count.
    """
    codes = _trimmed(css_anadata[_PARAMCD])
    present = pd.DataFrame({_PARAMCD: pd.unique(codes[codes != ""].to_numpy(dtype=object))})
    return codes, list(sas_sort(present, _PARAMCD)[_PARAMCD])


def _timepoints(css_nextparam: pd.DataFrame) -> tuple[np.ndarray, np.ndarray]:
    """Return each record's ``ATPTN`` and the distinct non-missing values, ascending (S07-TRN-07).

    Ports ``%util_labels_from_var(css_nextparam, atptn, atpt)``
    [whitepapers/WPCT/WPCT-F.07.07.sas:300].
    """
    values = css_nextparam[_ATPTN].to_numpy(dtype="float64", na_value=np.nan)
    return values, np.unique(values[~np.isnan(values)])


def _pooled_mask(df: pd.DataFrame) -> np.ndarray:
    """True on the pooled records, ``studyid = 'A0'x !! 'Pooled'`` (S07-TRN-05, D-16)."""
    return (_trimmed(df[_STUDYID]) == POOLED_STUDYID).to_numpy(dtype=bool)


def _level_value(label: str) -> float:
    """``input(scan(parameter, -1, ' '), best8.)``: the TRTPN value of a CLASS level label.

    ``LEVEL`` is the last word of ``PARAMETER`` (``TRTPN 54`` has ``54``), written by
    ``sas_semantics.format_level`` so that it reads back as the exact level; ``.`` is missing.
    """
    return float("nan") if label == "." else float(label)


def _with_studynum(rows: pd.DataFrame, studynum: int) -> pd.DataFrame:
    """Return ParameterEstimates ``rows`` with the model's ``STUDYNUM`` prepended."""
    out = rows.copy()
    out.insert(0, _PE_STUDYNUM, np.full(len(out), studynum, dtype=np.int64))
    return out


def _stacked(frames: list[pd.DataFrame]) -> pd.DataFrame:
    """Stack frames of one shape in order; the first frame stands in when every frame is empty."""
    filled = [frame for frame in frames if not frame.empty]
    if not filled:
        return frames[0]
    return pd.concat(filled, ignore_index=True)


def _fit_models(
    log: _CheckpointLog,
    css_nexttimept: pd.DataFrame,
    paramcd: str,
    atptn: float,
    stdyn: int,
    config: Config,
) -> tuple[pd.DataFrame, pd.DataFrame, list[ModelRecord]]:
    """Fit the per-study and pooled models of one stratum (S07-MOD-02, S07-MOD-03).

    - Per study [whitepapers/WPCT/WPCT-F.07.07.sas:343-352]: ``WHERE studyid ^= 'A0'x !!
      'Pooled'``, ``BY studynum``, ``MODEL chg = base trtpn / solution``: one fit per ``studynum``
      present, ascending. Their stacked rows are ``CSS_PVAL_STDY``.
    - Pooled [whitepapers/WPCT/WPCT-F.07.07.sas:355-363]: ``WHERE studyid = 'A0'x !! 'Pooled'``,
      ``MODEL chg = base trtpn studynum / solution``. ``studynum`` is continuous and constant on
      these records, so ``model.glm_solution`` aliases it (D-06, DIV-01). Its rows are
      ``CSS_PVAL_POOL`` and carry ``STUDYNUM = &stdyn``.
    - The ``TRTPN`` rows of both, the pooled ones with ``studynum = &stdyn``, are the combined
      ``CSS_PVAL_STDY`` [whitepapers/WPCT/WPCT-F.07.07.sas:366-370].

    A model without usable records, or without its reference level, has a model record and no
    rows (U-02). Returns all parameter rows with ``STUDYNUM`` prepended, the combined ``TRTPN``
    rows, and the model records in fitting order.
    """
    pooled_mask = _pooled_mask(css_nexttimept)
    individual = css_nexttimept.loc[~pooled_mask]
    pooled = css_nexttimept.loc[pooled_mask]

    def record(number: int, kind: str, fit: model.GlmFitRecord) -> ModelRecord:
        return ModelRecord(
            PARAMCD=paramcd,
            ATPTN=float(atptn),
            STUDYNUM=number,
            MODEL=kind,
            NOBS=fit.NOBS,
            DFE=fit.DFE,
            STATUS=fit.STATUS,
        )

    frames: list[pd.DataFrame] = []
    models: list[ModelRecord] = []
    for studynum in np.unique(individual[_STUDYNUM].to_numpy(dtype=np.int64)):
        study = individual.loc[(individual[_STUDYNUM] == studynum).to_numpy(dtype=bool)]
        rows, fit = model.glm_solution(
            study, config.c_var, (config.b_var, config.tn_var), config.tn_var, config.ref_trtn
        )
        frames.append(_with_studynum(rows, int(studynum)))
        models.append(record(int(studynum), MODEL_STUDY, fit))
    log.record("CSS_PVAL_STDY", sum(len(frame) for frame in frames), paramcd, atptn)

    pooled_rows, pooled_fit = model.glm_solution(
        pooled,
        config.c_var,
        (config.b_var, config.tn_var, _STUDYNUM),
        config.tn_var,
        config.ref_trtn,
    )
    pooled_rows = _with_studynum(pooled_rows, stdyn)
    log.record("CSS_PVAL_POOL", pooled_rows, paramcd, atptn)
    frames.append(pooled_rows)
    models.append(record(stdyn, MODEL_POOLED, pooled_fit))

    estimates = _stacked(frames)
    # where=(parameter =: "%upcase(&tn_var)"): model.glm_solution names the CLASS effect by the
    # upper-cased variable, and the pooled studynum row does not start with it.
    trt_rows = estimates.loc[(estimates["EFFECT"] == config.tn_var.upper()).to_numpy(dtype=bool)]
    trt_rows = trt_rows.reset_index(drop=True)
    log.record("CSS_PVAL_STDY", trt_rows, paramcd, atptn)
    return estimates, trt_rows, models


def _merge_pvalues(stats: pd.DataFrame, trt_rows: pd.DataFrame, config: Config) -> pd.DataFrame:
    """Merge the per-study and pooled treatment p-values into the statistics (S07-TRN-08).

    Ports [whitepapers/WPCT/WPCT-F.07.07.sas:374-394]: each ``TRTPN`` parameter row gives its
    model's ``studynum``, the level's TRTPN value and ``pval = Probt``, and the rows are
    match-merged into ``css_stats`` BY ``studynum TRTPN``. The pooled rows match the pooled
    statistics because both carry ``&stdyn``. The DATA-step MERGE keeps the records of both
    inputs, so the merge is an outer join; each study-by-treatment row receives its own level's
    p-value, missing for the aliased reference level. Within a BY group the statistics keep their
    order. The result has the ``proc_summary`` columns, then ``pval``; ``studynum`` keeps its
    integer type wherever every merged record has one.
    """
    keys = [_STUDYNUM, config.tn_var]
    pvalues = pd.DataFrame(
        {
            _STUDYNUM: trt_rows[_PE_STUDYNUM].to_numpy(dtype="float64"),
            config.tn_var: np.array(
                [_level_value(label) for label in trt_rows["LEVEL"]], dtype="float64"
            ),
            _PVAL: trt_rows["PROBT"].to_numpy(dtype="float64"),
        }
    )
    left = stats.astype({key: "float64" for key in keys})
    order = "_STATS_ORDER"
    left[order] = np.arange(len(left), dtype="float64")
    merged = left.merge(pvalues, on=keys, how="outer", sort=False)
    merged = sas_sort(merged, [*keys, order])
    for key in keys:
        if pd.api.types.is_integer_dtype(stats[key].dtype) and merged[key].notna().all():
            merged[key] = merged[key].astype(stats[key].dtype)
    return merged[[*stats.columns, _PVAL]]


def _with_stratum(df: pd.DataFrame, paramcd: str, atptn: float) -> pd.DataFrame:
    """Return ``df`` with the stratum columns ``PARAMCD ATPTN`` prepended."""
    out = df.copy()
    out.insert(0, _PARAMCD, pd.Series([paramcd] * len(out), index=out.index, dtype=object))
    out.insert(1, _ATPTN, np.full(len(out), float(atptn)))
    return out


def _dataset(name: str, frames: list[pd.DataFrame], exclude: tuple[str, ...] = ()) -> pd.DataFrame:
    """Concatenate ``frames`` in loop order into the export ``name``'s columns, less ``exclude``.

    Without rows the result is header-only, typed by the export schema: an empty population is no
    error (S07-TRN-07).
    """
    spec = export_spec(PROGRAM_ID, name)
    columns = [column for column in spec.columns if column not in exclude]
    filled = [frame for frame in frames if not frame.empty]
    if not filled:
        return pd.DataFrame(
            {
                column: pd.Series(dtype=COLUMN_DTYPES[spec.column_types[column]])
                for column in columns
            }
        )
    return pd.concat([frame[columns] for frame in filled], ignore_index=True)


#: The program's own settings, the default of :func:`run`; ``Config`` is frozen, so one instance
#: serves every call.
_DEFAULT_CONFIG = Config()


def run(
    config: Config = _DEFAULT_CONFIG,
    paths: RepoPaths | None = None,
    advs: pd.DataFrame | None = None,
) -> ProgramResult:
    """Run WPCT-F.07.07 and return its datasets, checkpoints and model records.

    Args:
        config: The user settings (S07-TRN-01); the defaults are the program's.
        paths: The repository paths; :meth:`RepoPaths.checkout` when omitted.
        advs: Test-only: an already-read ADVS frame used instead of reading
            ``paths.input_xpt(PROGRAM_ID)``. It is not modified.

    Returns:
        A :class:`ProgramResult` whose ``datasets`` are ``advs_min``, ``css_anadata``,
        ``css_stats`` and ``parameter_estimates`` in the ``wpct.schemas`` column orders.
        ``css_stats`` stacks the strata in loop order with ``PARAMCD ATPTN`` prepended;
        ``parameter_estimates`` stacks, per stratum, the per-study models in ``studynum`` order and
        then the pooled model, with ``PARAMCD ATPTN STUDYNUM`` prepended (pooled ``STUDYNUM`` is
        ``&stdyn``). ``pooled_studynum`` is ``&stdyn``, or ``None`` when no study is present.
        ``omission_counts`` holds the derivation's omitted baseline and post-baseline records.
        With the GLM skipped by setting, ``css_stats`` has no ``pval`` column and ``models`` and
        ``parameter_estimates`` are empty. When the derivation receives no usable record,
        ``notes`` carries :data:`U03_NOTE`. On the repository data the run records the 29
        checkpoints of the SAS log, in log order.

    Raises:
        TypeError: ``config`` is not a :class:`Config`, or ``config.param_timepoints`` is not a
            sequence of ``(PARAMCD, timepoints)`` pairs, each with a string ``PARAMCD`` and a
            sequence of numeric timepoints (S07-FLT-01, ``selection.subset_param_timepoints``).
            :class:`Config` refuses only a ``param_timepoints`` that cannot be iterated or names a
            code that ``param`` does not list, so the pairs are checked once the input is read,
            before any record is selected. A setting that :class:`Config` refuses raises when the
            ``Config`` is built, before ``run`` is called. An input variable whose SAS type does not
            fit its use, such as a character ``ATPTN`` or ``AVAL`` or a numeric ``PARAMCD`` or
            ``USUBJID``, raises it too, from the step that relies on its type (``selection``,
            ``derivation.derive_lastminmax_measure`` or ``model.glm_solution``), before any record
            is exported.
        ValueError: ``config.param_timepoints`` lists a NaN timepoint, the SAS missing value,
            which would otherwise select the records whose ``ATPTN`` is missing (S07-FLT-01);
            raised at the same point as the ``param_timepoints`` ``TypeError``.
        InputDataError: The input file is missing or unreadable, lacks a required variable, or
            stores a required variable under more than one spelling, as columns that differ only
            in case (S07-DEP-assert_depend ``vars=``).
        AnalysisHalted: ``CHG`` is missing in ``CSS_SAFANA`` (S07-FLT-04) or the pooled analysis
            set repeats a ``STUDYID USUBJID PARAMCD ATPTN`` key (S07-FLT-05). The exception carries
            the checkpoints recorded before the halt.
    """
    if not isinstance(config, Config):
        raise TypeError(f"{PROGRAM_ID}: config must be a Config, got {type(config).__name__}")
    paths = RepoPaths.checkout() if paths is None else paths
    path = paths.input_xpt(PROGRAM_ID)
    if advs is None:
        advs, _metadata = xpt_reader.read_xpt(path, PROGRAM_ID)
    # The returned frame spells each required variable as the schema does, whatever its stored case.
    advs = xpt_reader.require_variables(
        advs, REQUIRED_INPUT_VARIABLES[PROGRAM_ID], path, PROGRAM_ID
    )

    log = _CheckpointLog()
    notes: list[str] = []
    log.record("ADVS", advs, nvars=True)

    # DATA advs_sub (S07-FLT-01, S07-TRN-02) [whitepapers/WPCT/WPCT-F.07.07.sas:146-158].
    advs_sub = selection.subset_param_timepoints(advs, config.param_timepoints)
    advs_sub = selection.abbreviate_treatment(advs_sub)
    log.record("ADVS_SUB", advs_sub, nvars=True)

    # %derive_lastminmax_measure (S07-TRN-03, LMM-*) [whitepapers/WPCT/WPCT-F.07.07.sas:161-166].
    dsout, counts = derivation.derive_lastminmax_measure(
        advs_sub,
        config.lmm,
        flvars=(config.a_fl,),
        grpvars=LMM_GRPVARS,
        ordvars=LMM_ORDVARS,
        incl=LMM_INCL,
    )
    omission_counts = _record_derivation(log, counts, config)
    # The U-03 guard applies whenever no record reaches the baseline/post-baseline split: an empty
    # ADVS_SUB, or one whose every record LMM-FLT-01 deletes. In both cases SAS never sets the
    # AVISIT length, because the record that would set it is never output.
    if counts.LMM_BASE + counts.LMM_POST == 0:
        notes.append(U03_NOTE)
    log.record("ADVS_MIN", dsout, nvars=True)

    # DATA advs_&LMM WHERE avisit =: 'Post-baseline' (S07-FLT-02)
    # [whitepapers/WPCT/WPCT-F.07.07.sas:168-171].
    advs_min = selection.post_baseline_filter(dsout)
    log.record("ADVS_MIN", advs_min, nvars=True)

    # DATA css_safana (S07-TRN-04, S07-POP-01, S07-FLT-03)
    # [whitepapers/WPCT/WPCT-F.07.07.sas:212-214]. The keep list is the schema's fixed
    # &ana_variables, so every run exports one shape: with b_var blank SAS would drop BASE from this
    # list, while the port keeps it (the models, not the record set, depend on b_var).
    css_safana = selection.keep_variables(advs_min, ANA_VARIABLES[PROGRAM_ID])
    css_safana = selection.flag_filter(css_safana, [config.p_fl, config.a_fl])
    log.record("CSS_SAFANA", css_safana, nvars=True)
    # S07-FLT-04 [whitepapers/WPCT/WPCT-F.07.07.sas:219-220].
    _halting_check(
        log, selection.assert_var_nonmissing, css_safana, config.c_var, "CSS_SAFANA", PROGRAM_ID
    )

    # DATA css_anadata, PROC SORT and the studynum DATA step (S07-TRN-05, S07-TRN-06)
    # [whitepapers/WPCT/WPCT-F.07.07.sas:222-241].
    css_anadata = selection.stack_pooled(css_safana)
    log.record("CSS_ANADATA", css_anadata, nvars=True)
    css_anadata = selection.assign_studynum(css_anadata, (_STUDYID, config.tn_var), _STUDYNUM)
    log.record("CSS_ANADATA", css_anadata, nvars=True)
    # S07-FLT-05 [whitepapers/WPCT/WPCT-F.07.07.sas:244-245].
    _halting_check(
        log, selection.assert_unique_keys, css_anadata, _UNIQUE_KEYS, "CSS_ANADATA", PROGRAM_ID
    )

    # %util_count_unique_values(css_anadata, studyid, stdyn) (S07-TRN-07)
    # [whitepapers/WPCT/WPCT-F.07.07.sas:262].
    stdyn = _study_count(css_anadata)

    stats_frames: list[pd.DataFrame] = []
    estimate_frames: list[pd.DataFrame] = []
    models: list[ModelRecord] = []

    # Loops 1 and 2 of %boxplot_each_param_tp (S07-TRN-07)
    # [whitepapers/WPCT/WPCT-F.07.07.sas:290-316].
    codes, parameters = _parameter_codes(css_anadata)
    for paramcd in parameters:
        css_nextparam = css_anadata.loc[(codes == paramcd).to_numpy(dtype=bool)]
        css_nextparam = css_nextparam.reset_index(drop=True)
        log.record("CSS_NEXTPARAM", css_nextparam, paramcd)

        atptn_values, timepoints = _timepoints(css_nextparam)
        for atptn in timepoints:
            css_nexttimept = sas_sort(
                css_nextparam.loc[atptn_values == atptn], [_STUDYID, config.tn_var]
            )
            log.record("CSS_NEXTTIMEPT", css_nexttimept, paramcd, atptn)

            # PROC SUMMARY (S07-MOD-01) [whitepapers/WPCT/WPCT-F.07.07.sas:329-334].
            stats = summary.proc_summary(
                css_nexttimept,
                by=(_STUDYID, config.tn_var, _STUDYNUM, config.t_var),
                var=config.c_var,
            )
            log.record("CSS_STATS", stats, paramcd, atptn, nvars=True)

            if config.glm_enabled:
                estimates, trt_rows, fits = _fit_models(
                    log, css_nexttimept, paramcd, atptn, stdyn, config
                )
                models.extend(fits)
                stats = _merge_pvalues(stats, trt_rows, config)
                log.record("CSS_STATS", stats, paramcd, atptn, nvars=True)
                estimate_frames.append(_with_stratum(estimates, paramcd, atptn))

            stats_frames.append(_with_stratum(stats, paramcd, atptn))

    stats_exclude = () if config.glm_enabled else (_PVAL,)
    datasets = {
        _ADVS_MIN: _dataset(_ADVS_MIN, [advs_min]),
        _CSS_ANADATA: _dataset(_CSS_ANADATA, [css_anadata]),
        _CSS_STATS: _dataset(_CSS_STATS, stats_frames, stats_exclude),
        _PARAMETER_ESTIMATES: _dataset(_PARAMETER_ESTIMATES, estimate_frames),
    }
    return ProgramResult(
        program_id=PROGRAM_ID,
        datasets=datasets,
        checkpoints=log.checkpoints,
        models=tuple(models),
        omission_counts=omission_counts,
        glm_skipped=not config.glm_enabled,
        # ProgramResult documents None for a run without studies; &stdyn is then 0.
        pooled_studynum=stdyn if stdyn > 0 else None,
        notes=tuple(notes),
    )
