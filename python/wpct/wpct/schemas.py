"""Dataset names, column orders, keys, field classes and file names of the WPCT port.

This module is the single source of every dataset shape used by the drivers, the CSV exports, the
SAS-export loader, the comparisons and the READMEs. Other modules import these values and never
restate them. It holds data and lookup helpers only, imports nothing from the ``wpct`` package, and
holds no tolerance: comparison limits live only in ``python/wpct/tolerances.json``.

Spec items realised here (``python/wpct/docs/sas_specification.md``): S02-TRN-03 and S07-TRN-04
(analysis variable lists), LMM-TRN-07 (derived record structure), S02-MOD-01 and S07-MOD-01
(summary statistics), S02-MOD-02, S07-MOD-02 and S07-MOD-03 (parameter estimate rows), and D-16
(the pooled study identifier).

Column-name casing
------------------
SAS variable names are case-insensitive; pandas column names are not. The port therefore fixes one
spelling per column:

* ADaM-origin variables keep their upper-case XPT names (``STUDYID``, ``TRTPN``, ``CHG``, ...).
* Variables a SAS program creates keep the case the program writes: ``trtp_short``, ``studynum``,
  ``pval`` and the statistics ``n mean std median datamin datamax q1 q3``.
* The derivation flag is ``ANL02FL``, the upper-cased ``flvars=anl02fl``.
* Columns that exist only in the port are upper case: ``EFFECT LEVEL PARAMETER ESTIMATE STDERR
  TVALUE PROBT ALIASED STUDYNUM MODEL NOBS DFE STATUS SAS_DATASET OCCURRENCE NVARS``.

In 07.07 the derivation's DATA step re-creates ``BASE`` and ``CHG``, so SAS stores them as ``base``
and ``chg`` [whitepapers/WPCT/outputs_sas/WPCT-F.07.07.lst:7,13]. Both programs nevertheless spell
them ``BASE`` and ``CHG`` here, so the shared summary and model code addresses one name; the case
fold below makes the two spellings the same variable.

Every comparison folds names to upper case on both sides (:func:`upper_columns`). The 07.07
``css_stats`` record key is therefore written ``STUDYNUM`` in the plan while the frame column is
``studynum``.
"""

from collections.abc import Iterable, Mapping
from dataclasses import dataclass, field
from types import MappingProxyType

__all__ = [
    "ADVS_SUB_EXTRA",
    "ANA_VARIABLES",
    "BOOL",
    "CHAR",
    "COLUMN_DTYPES",
    "COLUMN_TYPES",
    "EXPORTS",
    "INT",
    "LMM_GRPVARS",
    "LMM_INCL",
    "LMM_ORDVARS",
    "MODEL_KINDS",
    "NULLABLE_INT",
    "NUM",
    "PARAMETER_ESTIMATE_COLUMNS",
    "PARAMETER_ESTIMATE_TYPES",
    "POOLED_STUDYID",
    "PROGRAMS",
    "PROGRAM_07_02",
    "PROGRAM_07_07",
    "REQUIRED_INPUT_VARIABLES",
    "SAS_EXPORT_FILES",
    "SAS_EXPORT_SOURCES",
    "SAS_EXPORT_STRATUM_COLUMNS",
    "SAS_PARAMETER_ESTIMATE_COLUMNS",
    "STATISTICS",
    "ExportSpec",
    "export_spec",
    "sas_export_spec",
    "upper_columns",
]


def _names(text: str) -> tuple[str, ...]:
    """Split a blank-separated SAS variable list into a tuple, keeping order and case."""
    return tuple(text.split())


# --- Program identifiers and constants -----------------------------------------------------------

PROGRAM_07_02 = "WPCT-F.07.02"
PROGRAM_07_07 = "WPCT-F.07.07"
#: Run order fixed by the request: 07.02, then 07.07.
PROGRAMS = (PROGRAM_07_02, PROGRAM_07_07)

#: ``'A0'x !! 'Pooled'`` [whitepapers/WPCT/WPCT-F.07.07.sas:227] (S07-TRN-05, D-16). Code point 160
#: sorts after every ASCII study identifier, so the pooled study gets the highest ``studynum``.
POOLED_STUDYID = "\u00a0Pooled"

#: PROC SUMMARY statistics in OUTPUT-statement order [whitepapers/WPCT/WPCT-F.07.02.sas:261-262;
#: whitepapers/WPCT/WPCT-F.07.07.sas:332-333] (S02-MOD-01, S07-MOD-01).
STATISTICS = _names("n mean std median datamin datamax q1 q3")

#: Row shape of ``model.glm_solution``: one row per GLM design column (S02-MOD-02, S07-MOD-02,
#: S07-MOD-03). ``EFFECT`` is ``INTERCEPT``, ``BASE``, ``TRTPN`` or ``STUDYNUM``; ``LEVEL`` is the
#: CLASS level label (``54``, ``81``, ``0``) or ``''``; ``PARAMETER`` is the SAS-style label.
PARAMETER_ESTIMATE_COLUMNS = _names("EFFECT LEVEL PARAMETER ESTIMATE STDERR TVALUE PROBT ALIASED")

#: Values of the ``MODEL`` column of ``models.csv``: the 07.02 endpoint fit (S02-MOD-02), the 07.07
#: per-study fits (S07-MOD-02) and the 07.07 pooled fit (S07-MOD-03).
MODEL_KINDS = ("endpoint", "study", "pooled")

#: Column type codes of :attr:`ExportSpec.column_types`: character (``''`` when missing), float64,
#: int64, nullable Int64 and bool.
CHAR = "char"
NUM = "num"
INT = "int"
NULLABLE_INT = "Int64"
BOOL = "bool"
COLUMN_TYPES = (CHAR, NUM, INT, NULLABLE_INT, BOOL)
#: The pandas dtype that holds each type code in memory; character values are Python strings in
#: ``object`` columns. The drivers' header-only frames of empty runs take their dtypes from it.
COLUMN_DTYPES: Mapping[str, object] = MappingProxyType(
    {CHAR: object, NUM: "float64", INT: "int64", NULLABLE_INT: "Int64", BOOL: bool}
)
#: Type code of each :data:`PARAMETER_ESTIMATE_COLUMNS` column, in that order (S02-MOD-02,
#: S07-MOD-02, S07-MOD-03): the character labels, the numeric estimate, standard error, t and p,
#: and the bool aliasing flag. The rows of ``model.glm_solution`` and both ``parameter_estimates``
#: exports take their ParameterEstimates column types from it.
PARAMETER_ESTIMATE_TYPES: Mapping[str, str] = MappingProxyType(
    dict(
        zip(
            PARAMETER_ESTIMATE_COLUMNS,
            (CHAR, CHAR, CHAR, NUM, NUM, NUM, NUM, BOOL),
            strict=True,
        )
    )
)


# --- Input and analysis variable lists -----------------------------------------------------------

#: Variables each program needs in its ADVS input, checked right after reading (port of the
#: ``assert_depend vars=`` and ``assert_var_exist`` checks). 07.02: the S02-FLT-01 subset keys, the
#: S02-TRN-02 source ``TRTP`` and the S02-TRN-03 keep list. 07.07: the S07-FLT-01 subset keys, the
#: S07-TRN-02 source ``TRTP``, the S07-TRN-03 derivation arguments and the LMM-FLT-01 dates and value.
REQUIRED_INPUT_VARIABLES: Mapping[str, tuple[str, ...]] = MappingProxyType(
    {
        PROGRAM_07_02: _names(
            "STUDYID USUBJID SAFFL ANL01FL TRTP TRTPN PARAM PARAMCD CHG BASE AVISIT AVISITN ATPT ATPTN"
        ),
        PROGRAM_07_07: _names(
            "STUDYID USUBJID SAFFL TRTP TRTPN PARAM PARAMCD ATPT ATPTN AVISIT AVISITN AVAL ADT TRTSDT"
        ),
    }
)

#: The SAS ``&ana_variables`` keep lists, in the order the programs write them: S02-TRN-03
#: [whitepapers/WPCT/WPCT-F.07.02.sas:144] and S07-TRN-04 [whitepapers/WPCT/WPCT-F.07.07.sas:190].
#: ``keep=`` does not reorder, so the kept frames follow input order; their resulting orders are the
#: ``css_anadata`` columns of :data:`EXPORTS` (07.07 adds ``studynum`` after the pooled stacking).
ANA_VARIABLES: Mapping[str, tuple[str, ...]] = MappingProxyType(
    {
        PROGRAM_07_02: _names(
            "STUDYID USUBJID SAFFL ANL01FL trtp_short TRTPN PARAM PARAMCD CHG BASE AVISIT AVISITN ATPT ATPTN"
        ),
        PROGRAM_07_07: _names(
            "STUDYID USUBJID SAFFL ANL02FL trtp_short TRTPN PARAM PARAMCD CHG BASE ATPT ATPTN"
        ),
    }
)

#: Arguments of the 07.07 ``%derive_lastminmax_measure`` call (S07-TRN-03
#: [whitepapers/WPCT/WPCT-F.07.07.sas:161-166]).
LMM_GRPVARS = _names("STUDYID USUBJID TRTPN PARAMCD ATPTN")
LMM_ORDVARS = ("AVISITN",)
LMM_INCL = _names("trtp_short SAFFL PARAM ATPT")

#: Variables the subset step appends after the 44 ADVS variables (S02-TRN-02, S07-TRN-02), giving
#: the 45 variables of ``WORK.ADVS_SUB`` [whitepapers/WPCT/outputs_sas/WPCT-F.07.02.log:59;
#: whitepapers/WPCT/outputs_sas/WPCT-F.07.07.log:61].
ADVS_SUB_EXTRA = ("trtp_short",)

#: The SAS column names of an ODS ParameterEstimates export; upper-folded they equal
#: ``PARAMETER ESTIMATE STDERR TVALUE PROBT`` of :data:`PARAMETER_ESTIMATE_COLUMNS`.
SAS_PARAMETER_ESTIMATE_COLUMNS = ("Parameter", "Estimate", "StdErr", "tValue", "Probt")


# --- Export specifications ---------------------------------------------------------------------

_FIELD_LISTS = (
    "key",
    "categorical",
    "record_numeric",
    "count_fields",
    "summary_fields",
    "glm_fields",
    "p_value_fields",
    "count_strata",
    "optional_columns",
)


@dataclass(frozen=True)
class ExportSpec:
    """Shape of one exported intermediate dataset of one program.

    ``name`` is the export name; for the stored intermediate frames (``css_anadata``, ``css_stats``,
    ``parameter_estimates``, and ``advs_min`` in 07.07) it is also the key of
    ``ProgramResult.datasets``, while the ``models`` and ``checkpoints`` exports are built from the
    ``ProgramResult.models`` and ``ProgramResult.checkpoints`` records and have no ``datasets``
    entry. ``file_name`` is the CSV written under ``python/wpct/outputs/<program_id>/``.
    ``columns`` is the SAS dataset order, with any stratum columns first. ``column_types`` maps
    every column to one of :data:`COLUMN_TYPES`. ``key`` identifies a record. ``categorical``
    fields are compared exactly, ``record_numeric`` fields as record-level numbers, and
    ``count_fields``, ``summary_fields``, ``glm_fields`` and ``p_value_fields`` under the tolerance
    classes of the same meaning.
    ``count_strata`` lists the fields whose combinations carry per-stratum record counts (empty when
    none apply). ``optional_columns`` may be absent from a frame: ``pval`` when the GLM is skipped
    by setting. ``sas_export_file`` names the user-supplied full-precision SAS export, or is None
    when no SAS dataset corresponds. ``sas_counterpart`` describes the SAS counterpart for the
    READMEs and the report.

    Construction coerces every list to a tuple, freezes ``column_types``, and raises ``ValueError``
    when a field list names a column outside ``columns``, so a schema edit cannot drift silently.
    """

    program_id: str
    name: str
    file_name: str
    columns: tuple[str, ...]
    # Excluded from the hash: a read-only mapping is unhashable, and the remaining fields already
    # identify the spec.
    column_types: Mapping[str, str] = field(hash=False)
    key: tuple[str, ...]
    categorical: tuple[str, ...] = ()
    record_numeric: tuple[str, ...] = ()
    count_fields: tuple[str, ...] = ()
    summary_fields: tuple[str, ...] = ()
    glm_fields: tuple[str, ...] = ()
    p_value_fields: tuple[str, ...] = ()
    count_strata: tuple[str, ...] = ()
    optional_columns: tuple[str, ...] = ()
    sas_export_file: str | None = None
    sas_counterpart: str = ""

    def __post_init__(self) -> None:
        object.__setattr__(self, "columns", tuple(self.columns))
        for list_name in _FIELD_LISTS:
            object.__setattr__(self, list_name, tuple(getattr(self, list_name)))
        object.__setattr__(self, "column_types", MappingProxyType(dict(self.column_types)))
        self._validate()

    def _validate(self) -> None:
        label = f"ExportSpec {self.program_id}/{self.name}"
        if not self.columns:
            raise ValueError(f"{label}: no columns")
        if len(set(self.columns)) != len(self.columns):
            raise ValueError(f"{label}: duplicate columns in {' '.join(self.columns)}")
        if not self.key:
            raise ValueError(f"{label}: empty record key")
        known = set(self.columns)
        for list_name in _FIELD_LISTS:
            unknown = [name for name in getattr(self, list_name) if name not in known]
            if unknown:
                raise ValueError(f"{label}: {list_name} names unknown columns: {' '.join(unknown)}")
        untyped = [name for name in self.columns if name not in self.column_types]
        if untyped:
            raise ValueError(f"{label}: columns without a type: {' '.join(untyped)}")
        extra = [name for name in self.column_types if name not in known]
        if extra:
            raise ValueError(f"{label}: types for unknown columns: {' '.join(extra)}")
        invalid = {name: code for name, code in self.column_types.items() if code not in COLUMN_TYPES}
        if invalid:
            raise ValueError(f"{label}: invalid column types: {invalid}")
        optional_keys = [name for name in self.optional_columns if name in self.key]
        if optional_keys:
            raise ValueError(f"{label}: optional columns in the record key: {' '.join(optional_keys)}")


def _spec(
    program_id: str,
    name: str,
    *,
    columns: str,
    types: Mapping[str, str],
    key: str,
    categorical: str = "",
    record_numeric: str = "",
    count_fields: str = "",
    summary_fields: str = "",
    glm_fields: str = "",
    p_value_fields: str = "",
    count_strata: str = "",
    optional_columns: str = "",
    sas_export: bool,
    sas_counterpart: str,
) -> ExportSpec:
    """Build an :class:`ExportSpec` from blank-separated name lists.

    ``types`` maps a type code to the columns of that type; every column it does not list is
    character. The SAS export name is ``<program_id>_<file_name>`` when ``sas_export`` is true.
    """
    column_names = _names(columns)
    column_types = {column: CHAR for column in column_names}
    for code, typed in types.items():
        for column in _names(typed):
            if column not in column_types:
                raise ValueError(f"ExportSpec {program_id}/{name}: typed column {column} not in columns")
            column_types[column] = code
    file_name = f"{name}.csv"
    return ExportSpec(
        program_id=program_id,
        name=name,
        file_name=file_name,
        columns=column_names,
        column_types=column_types,
        key=_names(key),
        categorical=_names(categorical),
        record_numeric=_names(record_numeric),
        count_fields=_names(count_fields),
        summary_fields=_names(summary_fields),
        glm_fields=_names(glm_fields),
        p_value_fields=_names(p_value_fields),
        count_strata=_names(count_strata),
        optional_columns=_names(optional_columns),
        sas_export_file=f"{program_id}_{file_name}" if sas_export else None,
        sas_counterpart=sas_counterpart,
    )


# The statistics after ``n``: float64 summary statistics (``n`` is an int64 count).
_SUMMARY_FIELDS = " ".join(STATISTICS[1:])
_PE_COLUMNS = " ".join(PARAMETER_ESTIMATE_COLUMNS)
_MODELS_COUNTERPART = "Model-level context; no direct SAS dataset"


def _with_parameter_estimate_types(stratum_types: Mapping[str, str]) -> dict[str, str]:
    """Return the ``types`` argument of :func:`_spec` for a ``parameter_estimates`` export.

    ``stratum_types`` maps a type code to the blank-separated stratum columns of that type. Each
    :data:`PARAMETER_ESTIMATE_COLUMNS` column is appended under its code in
    :data:`PARAMETER_ESTIMATE_TYPES`, so those columns are typed there alone.
    """
    types = dict(stratum_types)
    for column, code in PARAMETER_ESTIMATE_TYPES.items():
        types[code] = " ".join((*_names(types.get(code, "")), column))
    return types


def _checkpoints(program_id: str) -> ExportSpec:
    """The log-equivalent checkpoints of one run; identical in both programs.

    ``PARAMCD`` is ``''`` and ``ATPTN`` missing on checkpoints that are not stratum-specific;
    ``NVARS`` is filled only where the SAS log reports a variable count.
    """
    return _spec(
        program_id,
        "checkpoints",
        columns="SAS_DATASET OCCURRENCE PARAMCD ATPTN NOBS NVARS",
        types={NUM: "ATPTN", INT: "OCCURRENCE NOBS", NULLABLE_INT: "NVARS"},
        key="SAS_DATASET OCCURRENCE",
        sas_export=False,
        sas_counterpart="SAS log record counts",
    )


#: Every exported dataset per program, in export order. Each program's ``css_stats`` and
#: ``parameter_estimates`` carry the stratum columns ``PARAMCD ATPTN`` (and, for 07.07 parameter
#: rows, ``STUDYNUM``) ahead of the SAS columns.
EXPORTS: Mapping[str, tuple[ExportSpec, ...]] = MappingProxyType(
    {
        PROGRAM_07_02: (
            # S02-TRN-03: the 14 kept variables in their ADVS_SUB order.
            _spec(
                PROGRAM_07_02,
                "css_anadata",
                columns="STUDYID USUBJID SAFFL TRTPN PARAMCD PARAM ATPTN ATPT AVISIT AVISITN BASE CHG "
                "ANL01FL trtp_short",
                types={NUM: "TRTPN ATPTN AVISITN BASE CHG"},
                key="STUDYID USUBJID PARAMCD ATPTN AVISITN",
                categorical="STUDYID USUBJID SAFFL ANL01FL trtp_short TRTPN PARAM PARAMCD AVISIT AVISITN "
                "ATPT ATPTN",
                record_numeric="BASE CHG",
                count_strata="PARAMCD ATPTN AVISITN TRTPN",
                sas_export=True,
                sas_counterpart="WORK.CSS_ANADATA, 3548 × 14",
            ),
            # S02-MOD-01 statistics BY AVISITN TRTPN AVISIT trtp_short, then the S02-TRN-05 pval.
            _spec(
                PROGRAM_07_02,
                "css_stats",
                columns=f"PARAMCD ATPTN AVISITN TRTPN AVISIT trtp_short {' '.join(STATISTICS)} pval",
                types={NUM: f"ATPTN AVISITN TRTPN {_SUMMARY_FIELDS} pval", INT: "n"},
                key="PARAMCD ATPTN AVISITN TRTPN",
                categorical="AVISIT trtp_short",
                count_fields="n",
                summary_fields=_SUMMARY_FIELDS,
                p_value_fields="pval",
                optional_columns="pval",
                sas_export=True,
                sas_counterpart="WORK.CSS_STATS per stratum, with PARAMCD ATPTN prepended",
            ),
            # S02-MOD-02 endpoint ParameterEstimates, one row per design column.
            _spec(
                PROGRAM_07_02,
                "parameter_estimates",
                columns=f"PARAMCD ATPTN {_PE_COLUMNS}",
                types=_with_parameter_estimate_types({NUM: "ATPTN"}),
                key="PARAMCD ATPTN EFFECT LEVEL",
                categorical="EFFECT LEVEL",
                glm_fields="ESTIMATE STDERR TVALUE",
                p_value_fields="PROBT",
                sas_export=True,
                sas_counterpart="ODS ParameterEstimates per stratum",
            ),
            _spec(
                PROGRAM_07_02,
                "models",
                columns="PARAMCD ATPTN MODEL NOBS DFE STATUS",
                types={NUM: "ATPTN", INT: "NOBS", NULLABLE_INT: "DFE"},
                key="PARAMCD ATPTN",
                sas_export=False,
                sas_counterpart=_MODELS_COUNTERPART,
            ),
            _checkpoints(PROGRAM_07_02),
        ),
        PROGRAM_07_07: (
            # LMM-TRN-07 order after the S07-FLT-02 post-baseline filter; ADT is a SAS day number
            # (D-10).
            _spec(
                PROGRAM_07_07,
                "advs_min",
                columns="AVISIT STUDYID USUBJID SAFFL TRTPN PARAMCD PARAM ATPTN ATPT AVISITN trtp_short "
                "AVAL ADT BASE CHG ANL02FL",
                types={NUM: "TRTPN ATPTN AVISITN AVAL ADT BASE CHG"},
                key="STUDYID USUBJID TRTPN PARAMCD ATPTN AVISITN",
                categorical="AVISIT STUDYID USUBJID TRTPN PARAMCD ATPTN AVISITN trtp_short SAFFL PARAM "
                "ATPT ANL02FL",
                record_numeric="AVAL ADT BASE CHG",
                count_strata="PARAMCD ATPTN STUDYID TRTPN AVISITN",
                sas_export=True,
                sas_counterpart="WORK.ADVS_MIN after the post-baseline filter, 498 × 16",
            ),
            # S07-TRN-04 keep list in ADVS_MIN order, stacked with the pooled copy (S07-TRN-05)
            # and numbered (S07-TRN-06).
            _spec(
                PROGRAM_07_07,
                "css_anadata",
                columns="STUDYID USUBJID SAFFL TRTPN PARAMCD PARAM ATPTN ATPT trtp_short BASE CHG ANL02FL "
                "studynum",
                types={NUM: "TRTPN ATPTN BASE CHG", INT: "studynum"},
                key="STUDYID USUBJID PARAMCD ATPTN",
                categorical="STUDYID USUBJID SAFFL ANL02FL trtp_short TRTPN PARAM PARAMCD ATPT ATPTN "
                "studynum",
                record_numeric="BASE CHG",
                count_strata="PARAMCD ATPTN STUDYID TRTPN",
                sas_export=True,
                sas_counterpart="WORK.CSS_ANADATA with the pooled rows and studynum, 996 × 13",
            ),
            # S07-MOD-01 statistics BY STUDYID TRTPN studynum trtp_short, then the S07-TRN-08 pval.
            _spec(
                PROGRAM_07_07,
                "css_stats",
                columns=f"PARAMCD ATPTN STUDYID TRTPN studynum trtp_short {' '.join(STATISTICS)} pval",
                types={NUM: f"ATPTN TRTPN {_SUMMARY_FIELDS} pval", INT: "studynum n"},
                key="PARAMCD ATPTN studynum TRTPN",
                categorical="STUDYID trtp_short",
                count_fields="n",
                summary_fields=_SUMMARY_FIELDS,
                p_value_fields="pval",
                optional_columns="pval",
                sas_export=True,
                sas_counterpart="WORK.CSS_STATS per stratum, with PARAMCD ATPTN prepended",
            ),
            # S07-MOD-02 per-study and S07-MOD-03 pooled ParameterEstimates; pooled rows carry
            # STUDYNUM = &stdyn.
            _spec(
                PROGRAM_07_07,
                "parameter_estimates",
                columns=f"PARAMCD ATPTN STUDYNUM {_PE_COLUMNS}",
                types=_with_parameter_estimate_types({NUM: "ATPTN", INT: "STUDYNUM"}),
                key="PARAMCD ATPTN STUDYNUM EFFECT LEVEL",
                categorical="EFFECT LEVEL",
                glm_fields="ESTIMATE STDERR TVALUE",
                p_value_fields="PROBT",
                sas_export=True,
                sas_counterpart="ODS ParameterEstimates per study and pooled",
            ),
            _spec(
                PROGRAM_07_07,
                "models",
                columns="PARAMCD ATPTN STUDYNUM MODEL NOBS DFE STATUS",
                types={NUM: "ATPTN", INT: "STUDYNUM NOBS", NULLABLE_INT: "DFE"},
                key="PARAMCD ATPTN STUDYNUM",
                sas_export=False,
                sas_counterpart=_MODELS_COUNTERPART,
            ),
            _checkpoints(PROGRAM_07_07),
        ),
    }
)


_SPECS_BY_NAME: Mapping[tuple[str, str], ExportSpec] = MappingProxyType(
    {(spec.program_id, spec.name): spec for specs in EXPORTS.values() for spec in specs}
)

_SPECS_BY_SAS_EXPORT: Mapping[str, ExportSpec] = MappingProxyType(
    {
        spec.sas_export_file: spec
        for program_id in PROGRAMS
        for spec in EXPORTS[program_id]
        if spec.sas_export_file is not None
    }
)


# --- User-supplied full-precision SAS exports ----------------------------------------------------

#: The seven optional SAS export files, in program order and then export order.
SAS_EXPORT_FILES: tuple[str, ...] = tuple(_SPECS_BY_SAS_EXPORT)

#: Stratum columns the user adds to each SAS export when capturing it per stratum; matched
#: case-insensitively against the spec columns.
SAS_EXPORT_STRATUM_COLUMNS: Mapping[str, tuple[str, ...]] = MappingProxyType(
    {
        f"{PROGRAM_07_02}_css_anadata.csv": (),
        f"{PROGRAM_07_02}_css_stats.csv": ("PARAMCD", "ATPTN"),
        f"{PROGRAM_07_02}_parameter_estimates.csv": ("PARAMCD", "ATPTN"),
        f"{PROGRAM_07_07}_advs_min.csv": (),
        f"{PROGRAM_07_07}_css_anadata.csv": (),
        f"{PROGRAM_07_07}_css_stats.csv": ("PARAMCD", "ATPTN"),
        f"{PROGRAM_07_07}_parameter_estimates.csv": ("PARAMCD", "ATPTN", "studynum"),
    }
)

#: The SAS dataset and program point each export is captured from.
SAS_EXPORT_SOURCES: Mapping[str, str] = MappingProxyType(
    {
        f"{PROGRAM_07_02}_css_anadata.csv": (
            "WORK.CSS_ANADATA after [whitepapers/WPCT/WPCT-F.07.02.sas:162-168] (3548 rows)"
        ),
        f"{PROGRAM_07_02}_css_stats.csv": (
            "WORK.CSS_STATS after the merge [whitepapers/WPCT/WPCT-F.07.02.sas:302-305], for each stratum"
        ),
        f"{PROGRAM_07_02}_parameter_estimates.csv": (
            "The ODS ParameterEstimates table [whitepapers/WPCT/WPCT-F.07.02.sas:273-280] for each "
            "stratum: Parameter Estimate StdErr tValue Probt"
        ),
        f"{PROGRAM_07_07}_advs_min.csv": (
            "WORK.ADVS_MIN after [whitepapers/WPCT/WPCT-F.07.07.sas:168-171] (498 rows)"
        ),
        f"{PROGRAM_07_07}_css_anadata.csv": (
            "WORK.CSS_ANADATA after [whitepapers/WPCT/WPCT-F.07.07.sas:237-241] (996 rows, including "
            "studynum)"
        ),
        f"{PROGRAM_07_07}_css_stats.csv": (
            "WORK.CSS_STATS after the merge [whitepapers/WPCT/WPCT-F.07.07.sas:391-394], for each stratum"
        ),
        f"{PROGRAM_07_07}_parameter_estimates.csv": (
            "The ODS ParameterEstimates tables per study and pooled "
            "[whitepapers/WPCT/WPCT-F.07.07.sas:343-363]: Parameter Estimate StdErr tValue Probt"
        ),
    }
)


# --- Helpers ---------------------------------------------------------------------------------------


def upper_columns(names: Iterable[str]) -> tuple[str, ...]:
    """Fold SAS variable names to upper case, keeping their order.

    SAS names are case-insensitive, so comparisons and the SAS-export loader fold both sides with
    this function before matching columns.
    """
    if isinstance(names, str):
        raise TypeError("upper_columns expects a sequence of names, not a single string")
    return tuple(str(name).upper() for name in names)


def export_spec(program_id: str, name: str) -> ExportSpec:
    """Return the export spec named ``name`` of program ``program_id``."""
    try:
        return _SPECS_BY_NAME[(program_id, name)]
    except KeyError:
        known = ", ".join(f"{pid}/{spec_name}" for pid, spec_name in _SPECS_BY_NAME)
        raise KeyError(f"no export spec {name!r} for program {program_id!r}; known: {known}") from None


def sas_export_spec(file_name: str) -> ExportSpec:
    """Return the export spec whose user-supplied SAS export is ``file_name``."""
    try:
        return _SPECS_BY_SAS_EXPORT[file_name]
    except KeyError:
        raise KeyError(
            f"unknown SAS export file {file_name!r}; expected one of: {', '.join(SAS_EXPORT_FILES)}"
        ) from None


def _check_sas_export_tables() -> None:
    """Keep the SAS-export tables aligned with :data:`EXPORTS` (fails at import on drift)."""
    for table_name, table in (
        ("SAS_EXPORT_STRATUM_COLUMNS", SAS_EXPORT_STRATUM_COLUMNS),
        ("SAS_EXPORT_SOURCES", SAS_EXPORT_SOURCES),
    ):
        if tuple(table) != SAS_EXPORT_FILES:
            raise ValueError(f"{table_name} keys differ from SAS_EXPORT_FILES: {tuple(table)}")
    for file_name, stratum_columns in SAS_EXPORT_STRATUM_COLUMNS.items():
        spec_columns = set(upper_columns(_SPECS_BY_SAS_EXPORT[file_name].columns))
        unknown = [column for column in upper_columns(stratum_columns) if column not in spec_columns]
        if unknown:
            raise ValueError(f"{file_name}: stratum columns not in its spec: {' '.join(unknown)}")


_check_sas_export_tables()

