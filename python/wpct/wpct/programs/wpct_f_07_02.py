"""Driver for WPCT-F.07.02: change from baseline by analysis timepoint, visit and treatment.

Ports ``whitepapers/WPCT/WPCT-F.07.02.sas`` without its graphics. :func:`run` reads ADVS, selects
the analysis records, loops over the parameter and timepoint strata in SAS loop order, computes the
PROC SUMMARY statistics, fits the endpoint PROC GLM ``CHG = BASE TRTPN / SOLUTION`` with
``CLASS TRTPN (REF="0")``, merges the treatment p-values into the statistics and returns one
:class:`~wpct.programs.ProgramResult`. It records a :class:`~wpct.programs.Checkpoint` wherever the
SAS reference log reports the size of a dataset the port reproduces
[whitepapers/WPCT/outputs_sas/WPCT-F.07.02.log:32-1471].

Spec items (``python/wpct/docs/sas_specification.md``):

- Implemented here: S02-TRN-01 (:class:`Config`), S02-TRN-04 (strata and record order) and
  S02-TRN-05 (endpoint p-value merge).
- By composition: S02-FLT-01 and S02-TRN-02 (``selection.subset_param_timepoints``,
  ``selection.abbreviate_treatment``), S02-TRN-03 (``selection.keep_variables`` with
  ``schemas.ANA_VARIABLES``), S02-POP-01 and S02-FLT-02 (``selection.flag_filter``), S02-FLT-03
  (``selection.exclude_visit``), S02-MOD-01 (``summary.proc_summary``) and S02-MOD-02
  (``model.glm_solution``).
- Dependencies: S02-DEP-util_access_test_data, S02-DEP-assert_dset_exist,
  S02-DEP-assert_depend and S02-DEP-assert_var_exist through ``xpt_reader``;
  S02-DEP-util_labels_from_var and S02-DEP-util_count_unique_values through the stratum order of
  S02-TRN-04; S02-DEP-boxplot_each_param_tp through the loops, statistics, model and merge.

Excluded (S02-PRS-01, under the legacy-disposition rule that carries over only steps reaching a
selected record set, an exported dataset, a statistic or a ``SOLUTION`` row): the visit labels,
``&trtn``, the GTL template, Y-axis range, display formats, page blocks, ``css_plot``, titles,
footnotes, ODS PDF and PROC SGRENDER, and the ``util_delete_dsets`` cleanup of WORK datasets.

Data reach: input is read only through ``xpt_reader``, from ``paths.input_xpt(PROGRAM_ID)``
(``data/adam/cdisc/advs.xpt``). The driver reads no SAS reference file, writes no file and opens
no connection; the export step writes the datasets it returns. 07.02 has no data-content halting
check, so the only failure it raises is the reader's ``InputDataError``, unchanged.
"""

from __future__ import annotations

import numbers
from dataclasses import dataclass

import numpy as np
import pandas as pd

from wpct import model, selection, summary, xpt_reader
from wpct.paths import RepoPaths
from wpct.programs import MODEL_ENDPOINT, Checkpoint, ModelRecord, ProgramResult
from wpct.sas_semantics import SAS_BLANK, is_char_missing, sas_sort
from wpct.schemas import (
    ANA_VARIABLES,
    BOOL,
    CHAR,
    INT,
    NULLABLE_INT,
    NUM,
    PROGRAM_07_02,
    REQUIRED_INPUT_VARIABLES,
    export_spec,
)

# Annotations use built-in generics and ``X | None`` rather than the ``typing.Optional``/``Tuple``
# spelling, as ``wpct.programs`` and ``wpct.schemas`` do (ruff pyupgrade UP006, UP035, UP045);
# the runtime types are the same.

__all__ = ["PROGRAM_ID", "Config", "run"]

PROGRAM_ID = PROGRAM_07_02

_CSS_ANADATA = "css_anadata"
_CSS_STATS = "css_stats"
_PARAMETER_ESTIMATES = "parameter_estimates"

#: ``pval``, the merged GLM p-value (S02-TRN-05); absent from ``css_stats`` when the GLM is skipped.
_PVAL = "pval"

_VISIT_NUMBER = "AVISITN"
_VISIT_LABEL = "AVISIT"
_PARAMCD = "PARAMCD"
_ATPTN = "ATPTN"

#: pandas dtypes of the ``schemas`` column type codes, for header-only frames of empty runs.
_EMPTY_DTYPES = {CHAR: object, NUM: "float64", INT: "int64", NULLABLE_INT: "Int64", BOOL: bool}


@dataclass(frozen=True)
class Config:
    """The SAS ``%let`` user settings of WPCT-F.07.02 (S02-TRN-01).

    Each field mirrors one setting [whitepapers/WPCT/WPCT-F.07.02.sas:108-124]. Variable names are
    spelled as the port's frames spell them: upper case for ADaM variables, as the transport file
    stores them, and ``trtp_short`` as the subset step creates it (``wpct.schemas``).

    Attributes:
        m_ds: ``advs_sub``, the dataset the analysis reads; the port passes the subset frame on in
            memory.
        t_var: Treatment label variable, a PROC SUMMARY BY variable.
        tn_var: Treatment number variable: sort key, BY variable and the CLASS variable.
        c_var: Change from baseline, the analysis and response variable.
        b_var: Baseline covariate; ``None`` or ``""`` is the blank SAS setting and skips the GLM.
        ref_trtn: ``REF=`` level of ``tn_var``; ``None`` or ``""`` is blank and skips the GLM.
        b_visn: Baseline ``AVISITN``, dropped from the analysis records (S02-FLT-03).
        e_visn: Endpoint ``AVISITN``, the visit the GLM is fitted at (S02-MOD-02).
        p_fl: Population flag; records need ``'Y'`` (S02-POP-01).
        a_fl: Analysis record flag; records need ``'Y'`` (S02-FLT-02).
        param_timepoints: The ``(PARAMCD, ATPTN values)`` pairs of the subset WHERE (S02-FLT-01)
            [whitepapers/WPCT/WPCT-F.07.02.sas:90-93].

    ``max_boxes_per_page`` (pagination, S02-PRS-01) and ``outputs_folder`` (the PDF location,
    ENV-03) are presentation and environment settings and are not ported. ``m_lb=work`` names the
    SAS WORK library, which the port does not have.

    Raises ``ValueError`` when a variable setting names a variable outside the S02-TRN-03 analysis
    variables, which fix every exported shape, and ``TypeError`` when ``e_visn`` or a set
    ``ref_trtn`` is not a number.
    """

    m_ds: str = "advs_sub"
    t_var: str = "trtp_short"
    tn_var: str = "TRTPN"
    c_var: str = "CHG"
    b_var: str | None = "BASE"
    ref_trtn: float | None = 0
    b_visn: float = 0
    e_visn: float = 99
    p_fl: str = "SAFFL"
    a_fl: str = "ANL01FL"
    param_timepoints: tuple[tuple[str, tuple[float, ...]], ...] = (
        ("DIABP", (815,)),
        ("SYSBP", (816,)),
    )

    def __post_init__(self) -> None:
        analysis_variables = ANA_VARIABLES[PROGRAM_ID]
        settings = {
            "t_var": self.t_var,
            "tn_var": self.tn_var,
            "c_var": self.c_var,
            "p_fl": self.p_fl,
            "a_fl": self.a_fl,
        }
        if not _is_blank(self.b_var):
            settings["b_var"] = self.b_var
        unknown = {
            name: value for name, value in settings.items() if value not in analysis_variables
        }
        if unknown:
            raise ValueError(
                f"{PROGRAM_ID}: Config settings name variables outside the analysis variables "
                f"({' '.join(analysis_variables)}): {unknown}"
            )
        # A non-numeric endpoint would match no record, and a non-numeric REF= level would fail
        # only once a stratum reaches the model, so both are refused here.
        if not _is_number(self.e_visn):
            raise TypeError(f"{PROGRAM_ID}: Config.e_visn must be a number, got {self.e_visn!r}")
        if not _is_blank(self.ref_trtn) and not _is_number(self.ref_trtn):
            raise TypeError(
                f"{PROGRAM_ID}: Config.ref_trtn must be a number, None or '', got {self.ref_trtn!r}"
            )

    @property
    def glm_enabled(self) -> bool:
        """True when the endpoint GLM runs (S02-MOD-02).

        Mirrors ``%if %length(&b_var) > 0 and %length(&ref_trtn) > 0``
        [whitepapers/WPCT/WPCT-F.07.02.sas:267].
        """
        return not _is_blank(self.b_var) and not _is_blank(self.ref_trtn)


def _is_blank(value: object) -> bool:
    """True for ``None`` or an all-blank string, the port's forms of a blank macro variable.

    ``%let`` drops leading and trailing blanks, so a setting of blanks has ``%length`` 0.
    """
    return value is None or (isinstance(value, str) and value.strip(SAS_BLANK) == "")


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
        df: pd.DataFrame,
        paramcd: str | None = None,
        atptn: float | None = None,
        *,
        nvars: bool = False,
    ) -> None:
        """Record ``df``'s size as the next occurrence of SAS dataset ``name``.

        ``nvars`` fills the variable count, only where the SAS log line is mapped as ``obs/vars``.
        """
        occurrence = self._occurrences.get(name, 0) + 1
        self._occurrences[name] = occurrence
        self._records.append(
            Checkpoint(
                SAS_DATASET=name,
                OCCURRENCE=occurrence,
                PARAMCD=paramcd,
                ATPTN=None if atptn is None else float(atptn),
                NOBS=len(df),
                NVARS=len(df.columns) if nvars else None,
            )
        )

    @property
    def checkpoints(self) -> tuple[Checkpoint, ...]:
        return tuple(self._records)


def _trimmed(series: pd.Series) -> pd.Series:
    """Character values as SAS compares them: trailing blanks dropped, a missing value as ``''``."""
    return series.map(lambda value: "" if is_char_missing(value) else str(value).rstrip(SAS_BLANK))


def _parameter_codes(css_anadata: pd.DataFrame) -> tuple[pd.Series, list[str]]:
    """Return the trimmed ``PARAMCD`` of each record and the codes to loop over (S02-TRN-04).

    ``%util_labels_from_var(css_anadata, paramcd, param)`` counts the distinct non-missing codes and
    lists them in PROC SORT order [whitepapers/utilities/util_labels_from_var.sas:59-82], so the
    loop takes each code present once, ascending in the ``sas_sort`` order. S02-FLT-01 admits only
    configured codes, so no missing code reaches the count.
    """
    codes = _trimmed(css_anadata[_PARAMCD])
    present = pd.DataFrame({_PARAMCD: pd.unique(codes[codes != ""].to_numpy(dtype=object))})
    return codes, list(sas_sort(present, _PARAMCD)[_PARAMCD])


def _timepoints(css_nextparam: pd.DataFrame) -> tuple[np.ndarray, np.ndarray]:
    """Return each record's ``ATPTN`` and the distinct non-missing values, ascending (S02-TRN-04).

    Ports ``%util_labels_from_var(css_nextparam, atptn, atpt)``
    [whitepapers/WPCT/WPCT-F.07.02.sas:230].
    """
    values = css_nextparam[_ATPTN].to_numpy(dtype="float64", na_value=np.nan)
    return values, np.unique(values[~np.isnan(values)])


def _level_value(label: str) -> float:
    """``input(scan(parameter, -1, ' '), best8.)``: the TRTPN value of a CLASS level label.

    ``LEVEL`` is the last word of ``PARAMETER`` (``TRTPN 54`` has ``54``), written by
    ``sas_semantics.format_level`` so that it reads back as the exact level; ``.`` is missing.
    """
    return float("nan") if label == "." else float(label)


def _merge_pvalues(stats: pd.DataFrame, trt_rows: pd.DataFrame, config: Config) -> pd.DataFrame:
    """Merge the endpoint treatment p-values into the statistics (S02-TRN-05).

    Ports [whitepapers/WPCT/WPCT-F.07.02.sas:283-305]: each ``TRTPN`` parameter row gives
    ``AVISITN = &e_visn``, the level's TRTPN value and ``pval = Probt``, and the rows are
    match-merged into ``css_stats`` BY ``AVISITN TRTPN``. The DATA-step MERGE keeps the records of
    both inputs, so the merge is an outer join; the endpoint rows receive their own level's p-value
    (missing for the aliased reference level) and every other row a missing ``pval``. Within a BY
    group the statistics keep their order. The result has the ``proc_summary`` columns, then
    ``pval``.
    """
    keys = [_VISIT_NUMBER, config.tn_var]
    pvalues = pd.DataFrame(
        {
            _VISIT_NUMBER: np.full(len(trt_rows), float(config.e_visn)),
            config.tn_var: [_level_value(label) for label in trt_rows["LEVEL"]],
            _PVAL: trt_rows["PROBT"].to_numpy(dtype="float64"),
        }
    )
    left = stats.astype({key: "float64" for key in keys})
    order = "_STATS_ORDER"
    left[order] = np.arange(len(left), dtype="float64")
    merged = left.merge(pvalues, on=keys, how="outer", sort=False)
    merged = sas_sort(merged, [*keys, order])
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
    error (S02-TRN-04).
    """
    spec = export_spec(PROGRAM_ID, name)
    columns = [column for column in spec.columns if column not in exclude]
    filled = [frame for frame in frames if not frame.empty]
    if not filled:
        return pd.DataFrame(
            {
                column: pd.Series(dtype=_EMPTY_DTYPES[spec.column_types[column]])
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
    """Run WPCT-F.07.02 and return its datasets, checkpoints and model records.

    Args:
        config: The user settings (S02-TRN-01); the defaults are the program's.
        paths: The repository paths; :meth:`RepoPaths.checkout` when omitted.
        advs: Test-only: an already-read ADVS frame used instead of reading
            ``paths.input_xpt(PROGRAM_ID)``. It is not modified.

    Returns:
        A :class:`ProgramResult` whose ``datasets`` are ``css_anadata``, ``css_stats`` and
        ``parameter_estimates`` in the ``wpct.schemas`` column orders. ``css_stats`` and
        ``parameter_estimates`` stack the strata in loop order, each with ``PARAMCD ATPTN``
        prepended. With the GLM skipped by setting, ``css_stats`` has no ``pval`` column and
        ``models`` and ``parameter_estimates`` are empty. On the repository data the run records
        the 15 checkpoints of the SAS log, in log order.

    Raises:
        InputDataError: The input file is missing or unreadable, or lacks a required variable
            (S02-DEP-assert_depend ``vars=``).
    """
    if not isinstance(config, Config):
        raise TypeError(f"{PROGRAM_ID}: config must be a Config, got {type(config).__name__}")
    paths = RepoPaths.checkout() if paths is None else paths
    path = paths.input_xpt(PROGRAM_ID)
    if advs is None:
        advs, _metadata = xpt_reader.read_xpt(path, PROGRAM_ID)
    xpt_reader.require_variables(advs, REQUIRED_INPUT_VARIABLES[PROGRAM_ID], path, PROGRAM_ID)

    log = _CheckpointLog()
    log.record("ADVS", advs, nvars=True)

    # DATA advs_sub (S02-FLT-01, S02-TRN-02) [whitepapers/WPCT/WPCT-F.07.02.sas:90-103].
    advs_sub = selection.subset_param_timepoints(advs, config.param_timepoints)
    advs_sub = selection.abbreviate_treatment(advs_sub)
    log.record("ADVS_SUB", advs_sub, nvars=True)

    # DATA css_anadata (S02-TRN-03, S02-POP-01, S02-FLT-02, S02-FLT-03)
    # [whitepapers/WPCT/WPCT-F.07.02.sas:162-168]. The keep list is the schema's fixed
    # &ana_variables, so every run exports one shape: with b_var blank SAS would drop BASE from
    # this list, while the port keeps it (the model, not the record set, depends on b_var).
    css_anadata = selection.keep_variables(advs_sub, ANA_VARIABLES[PROGRAM_ID])
    css_anadata = selection.flag_filter(css_anadata, [config.p_fl, config.a_fl])
    css_anadata = selection.exclude_visit(css_anadata, _VISIT_NUMBER, config.b_visn)
    log.record("CSS_ANADATA", css_anadata, nvars=True)

    stats_frames: list[pd.DataFrame] = []
    estimate_frames: list[pd.DataFrame] = []
    models: list[ModelRecord] = []

    # Loops 1 and 2 of %boxplot_each_param_tp (S02-TRN-04)
    # [whitepapers/WPCT/WPCT-F.07.02.sas:220-245].
    codes, parameters = _parameter_codes(css_anadata)
    for paramcd in parameters:
        css_nextparam = css_anadata.loc[(codes == paramcd).to_numpy(dtype=bool)]
        css_nextparam = css_nextparam.reset_index(drop=True)
        log.record("CSS_NEXTPARAM", css_nextparam, paramcd)

        atptn_values, timepoints = _timepoints(css_nextparam)
        for atptn in timepoints:
            css_nexttimept = sas_sort(
                css_nextparam.loc[atptn_values == atptn], [_VISIT_NUMBER, config.tn_var]
            )
            log.record("CSS_NEXTTIMEPT", css_nexttimept, paramcd, atptn)

            # PROC SUMMARY (S02-MOD-01) [whitepapers/WPCT/WPCT-F.07.02.sas:258-263].
            stats = summary.proc_summary(
                css_nexttimept,
                by=(_VISIT_NUMBER, config.tn_var, _VISIT_LABEL, config.t_var),
                var=config.c_var,
            )
            log.record("CSS_STATS", stats, paramcd, atptn, nvars=True)

            if config.glm_enabled:
                # PROC GLM at the endpoint visit (S02-MOD-02)
                # [whitepapers/WPCT/WPCT-F.07.02.sas:267-280].
                endpoint = css_nexttimept.loc[
                    (css_nexttimept[_VISIT_NUMBER] == config.e_visn).to_numpy(dtype=bool)
                ]
                rows, fit = model.glm_solution(
                    endpoint,
                    config.c_var,
                    (config.b_var, config.tn_var),
                    config.tn_var,
                    config.ref_trtn,
                )
                log.record("CSS_PVALUES", rows, paramcd, atptn)
                # where=(parameter =: "%upcase(&tn_var)"): model.glm_solution names the CLASS
                # effect by the upper-cased variable.
                trt_rows = rows.loc[(rows["EFFECT"] == config.tn_var.upper()).to_numpy(dtype=bool)]
                log.record("CSS_PVALUES", trt_rows, paramcd, atptn)
                models.append(
                    ModelRecord(
                        PARAMCD=paramcd,
                        ATPTN=float(atptn),
                        STUDYNUM=None,
                        MODEL=MODEL_ENDPOINT,
                        NOBS=fit.NOBS,
                        DFE=fit.DFE,
                        STATUS=fit.STATUS,
                    )
                )

                stats = _merge_pvalues(stats, trt_rows, config)
                log.record("CSS_STATS", stats, paramcd, atptn, nvars=True)
                estimate_frames.append(_with_stratum(rows, paramcd, atptn))

            stats_frames.append(_with_stratum(stats, paramcd, atptn))

    stats_exclude = () if config.glm_enabled else (_PVAL,)
    datasets = {
        _CSS_ANADATA: _dataset(_CSS_ANADATA, [css_anadata]),
        _CSS_STATS: _dataset(_CSS_STATS, stats_frames, stats_exclude),
        _PARAMETER_ESTIMATES: _dataset(_PARAMETER_ESTIMATES, estimate_frames),
    }
    return ProgramResult(
        program_id=PROGRAM_ID,
        datasets=datasets,
        checkpoints=log.checkpoints,
        models=tuple(models),
        omission_counts={},
        glm_skipped=not config.glm_enabled,
    )
