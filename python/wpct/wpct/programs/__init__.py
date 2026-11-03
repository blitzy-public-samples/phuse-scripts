"""Result records shared by the WPCT program drivers and every module that consumes their runs.

The drivers live in ``wpct.programs.wpct_f_07_02`` and ``wpct.programs.wpct_f_07_07``. Each run
returns one :class:`ProgramResult`, the only object a driver hands to ``wpct.export``,
``wpct.compare``, ``wpct.report`` and ``wpct.__main__``. Those modules and the tests import the
record types from here and never define their own:

* :class:`Checkpoint`: one log-equivalent record count, the counterpart of a SAS log line such as
  ``NOTE: The data set WORK.CSS_STATS has 30 observations and 12 variables.``
* :class:`ModelRecord`: the context of one PROC GLM fit, exported as ``models.csv``.
* :class:`ProgramResult`: a run's exported datasets, checkpoints, model records, derivation
  omission counts and halt state.

Importing this package does not import the drivers, because the drivers import it. The module
holds data only: it reads no file, writes nothing and opens no connection.
"""

from __future__ import annotations

import math
import numbers
import operator
from collections.abc import Iterable, Mapping
from dataclasses import dataclass, field
from types import MappingProxyType

import pandas as pd

from wpct.errors import AnalysisHalted

# Annotations use built-in generics and ``X | None`` rather than ``typing.Optional``/``Tuple``/
# ``Mapping``, matching ``wpct.schemas`` and keeping the module clean under ruff's pyupgrade rules
# (UP006, UP035, UP045); the runtime types are the same.

__all__ = [
    "MODEL_ENDPOINT",
    "MODEL_POOLED",
    "MODEL_STUDY",
    "Checkpoint",
    "ModelRecord",
    "ProgramResult",
]

#: :attr:`ModelRecord.MODEL` values: the 07.02 endpoint fit (S02-MOD-02), a 07.07 per-study fit
#: (S07-MOD-02) and the 07.07 pooled fit (S07-MOD-03). They equal ``wpct.schemas.MODEL_KINDS``.
MODEL_ENDPOINT = "endpoint"
MODEL_STUDY = "study"
MODEL_POOLED = "pooled"
_MODEL_KINDS = (MODEL_ENDPOINT, MODEL_STUDY, MODEL_POOLED)


def _count(value: object, name: str, *, minimum: int = 0) -> int:
    """Return ``value`` as a built-in ``int`` of at least ``minimum``.

    Counts taken from frames are often numpy integer scalars, which are accepted; floats, strings
    and bools are refused, so a non-integral value cannot reach an integer export column.
    """
    if isinstance(value, bool):
        raise TypeError(f"{name} must be an integer, got {value!r}")
    try:
        number = operator.index(value)
    except TypeError:
        raise TypeError(f"{name} must be an integer, got {value!r}") from None
    if number < minimum:
        raise ValueError(f"{name} must be at least {minimum}, got {number}")
    return number


def _optional_count(value: object, name: str) -> int | None:
    """Return ``None`` unchanged, otherwise ``value`` as a non-negative built-in ``int``."""
    return None if value is None else _count(value, name)


def _timepoint(value: object, name: str, *, optional: bool) -> float | None:
    """Return a SAS numeric timepoint as a built-in ``float``.

    A NaN, the pandas form of a SAS missing value, becomes ``None`` where the field is optional and
    is refused where it is required.
    """
    if value is None and optional:
        return None
    if isinstance(value, bool) or not isinstance(value, numbers.Real):
        raise TypeError(f"{name} must be a number, got {value!r}")
    number = float(value)
    if math.isnan(number):
        if optional:
            return None
        raise ValueError(f"{name} must not be missing")
    return number


def _text(value: object, name: str, *, optional: bool = False) -> str | None:
    """Check that ``value`` is a non-empty string (or ``None`` where ``optional``)."""
    if value is None and optional:
        return None
    if not isinstance(value, str) or not value:
        raise TypeError(f"{name} must be a non-empty string, got {value!r}")
    return value


def _records(items: Iterable[object], record_type: type, name: str) -> tuple:
    """Return ``items`` as a tuple after checking that each one is a ``record_type``."""
    if isinstance(items, str):
        # tuple() of a lone string would silently split it into characters.
        raise TypeError(f"{name} must be a sequence, not a single string")
    records = tuple(items)
    for position, item in enumerate(records):
        if not isinstance(item, record_type):
            raise TypeError(
                f"{name}[{position}] must be a {record_type.__name__}, got {type(item).__name__}"
            )
    return records


@dataclass(frozen=True)
class Checkpoint:
    """One log-equivalent record count of a program run.

    Drivers create checkpoints in SAS log order, one wherever the SAS reference log reports the
    size of a dataset the port reproduces. The field names are the ``checkpoints.csv`` columns, and
    ``SAS_DATASET OCCURRENCE`` is that export's record key.

    Attributes:
        SAS_DATASET: The SAS WORK dataset name in upper case, as
            ``sas_reference.LOG_CHECKPOINT_MAP`` spells it, e.g. ``"ADVS"``, ``"CSS_STATS"``,
            ``"LMM_BASE_ANL"``, or a derivation warning count such as ``"LMM_BASE_OMITTED"``.
        OCCURRENCE: The 1-based count of this ``SAS_DATASET`` name within the run; SAS overwrites
            WORK datasets per loop, so ``CSS_STATS`` occurs several times.
        PARAMCD: The stratum parameter of a stratum-level checkpoint, otherwise ``None``.
        ATPTN: The stratum timepoint of a timepoint-level checkpoint, otherwise ``None``.
        NOBS: The observation count.
        NVARS: The variable count of the SAS-equivalent frame before any stratum columns are
            prepended; filled only where the log reports ``obs/vars``, otherwise ``None``.

    Counts are stored as built-in ``int`` and ``ATPTN`` as ``float``; a NaN ``ATPTN`` is stored
    as ``None``.
    """

    SAS_DATASET: str
    OCCURRENCE: int
    PARAMCD: str | None = None
    ATPTN: float | None = None
    NOBS: int = 0
    NVARS: int | None = None

    def __post_init__(self) -> None:
        name = _text(self.SAS_DATASET, "Checkpoint.SAS_DATASET")
        if name != name.upper():
            raise ValueError(f"Checkpoint.SAS_DATASET must be upper case, got {name!r}")
        object.__setattr__(
            self, "OCCURRENCE", _count(self.OCCURRENCE, "Checkpoint.OCCURRENCE", minimum=1)
        )
        _text(self.PARAMCD, "Checkpoint.PARAMCD", optional=True)
        object.__setattr__(self, "ATPTN", _timepoint(self.ATPTN, "Checkpoint.ATPTN", optional=True))
        object.__setattr__(self, "NOBS", _count(self.NOBS, "Checkpoint.NOBS"))
        object.__setattr__(self, "NVARS", _optional_count(self.NVARS, "Checkpoint.NVARS"))


@dataclass(frozen=True)
class ModelRecord:
    """The context of one PROC GLM fit, one ``models.csv`` row.

    Attributes:
        PARAMCD: The stratum parameter.
        ATPTN: The stratum timepoint.
        STUDYNUM: ``None`` for 07.02. For 07.07 the ``studynum`` of a per-study fit, or the pooled
            ``&stdyn`` for the pooled fit (S07-TRN-06, S07-TRN-08).
        MODEL: :data:`MODEL_ENDPOINT` (07.02, S02-MOD-02), :data:`MODEL_STUDY` (S07-MOD-02) or
            :data:`MODEL_POOLED` (S07-MOD-03).
        NOBS: The observations used by the fit, after the complete-case exclusion (D-08).
        DFE: The error degrees of freedom, ``None`` where the fit has none.
        STATUS: The fit status reported by ``wpct.model``: ``"fitted"``, ``"no observations"`` or
            ``"not fitted: reference level 0 absent"`` (U-02).
    """

    PARAMCD: str
    ATPTN: float
    STUDYNUM: int | None
    MODEL: str
    NOBS: int
    DFE: int | None
    STATUS: str

    def __post_init__(self) -> None:
        _text(self.PARAMCD, "ModelRecord.PARAMCD")
        object.__setattr__(
            self, "ATPTN", _timepoint(self.ATPTN, "ModelRecord.ATPTN", optional=False)
        )
        object.__setattr__(self, "STUDYNUM", _optional_count(self.STUDYNUM, "ModelRecord.STUDYNUM"))
        if self.MODEL not in _MODEL_KINDS:
            raise ValueError(f"ModelRecord.MODEL must be one of {_MODEL_KINDS}, got {self.MODEL!r}")
        object.__setattr__(self, "NOBS", _count(self.NOBS, "ModelRecord.NOBS"))
        object.__setattr__(self, "DFE", _optional_count(self.DFE, "ModelRecord.DFE"))
        _text(self.STATUS, "ModelRecord.STATUS")


# eq=False: the datasets are DataFrames, whose ``==`` is element-wise, so field-wise equality would
# raise instead of answering. Results compare and hash by identity.
@dataclass(frozen=True, eq=False)
class ProgramResult:
    """Everything one run of a program driver produced.

    Attributes:
        program_id: ``"WPCT-F.07.02"`` or ``"WPCT-F.07.07"`` (``wpct.schemas.PROGRAMS``).
        datasets: The exported intermediate datasets keyed by export name
            (``wpct.schemas.EXPORTS``): ``css_anadata``, ``css_stats`` and
            ``parameter_estimates`` for 07.02, plus ``advs_min`` for 07.07. ``css_stats`` carries
            the merged ``pval`` (S02-TRN-05, S07-TRN-08). The ``models`` and ``checkpoints``
            exports are built from :attr:`models` and :attr:`checkpoints`, not stored here.
            Empty for a halted run.
        checkpoints: The log-equivalent record counts, in SAS log order.
        models: One record per GLM fit, in fitting order.
        omission_counts: For 07.07 the derivation's omitted-record counts,
            ``{"LMM_BASE_OMITTED": n, "LMM_POST_OMITTED": n}`` (S07-TRN-03, LMM-FLT-02); empty for
            07.02.
        halt_message: The ``AnalysisHalted`` message when a halting check stopped the run,
            otherwise ``None``.
        glm_skipped: True when the baseline variable or the reference treatment is unset, so the
            GLM step does not run (S02-MOD-02, S07-MOD-02): :attr:`models` and
            ``parameter_estimates`` are empty and ``css_stats`` has no ``pval`` column.
        pooled_studynum: For 07.07 the pooled ``studynum``, ``&stdyn`` (S07-TRN-07, S07-TRN-08);
            ``None`` for 07.02 and for a run without studies.
        notes: Statements the report repeats for this run, e.g. ``"SAS behaviour for empty
            derivation input not established (U-03)"``.

    The instance is read-only: ``datasets`` and ``omission_counts`` are stored as read-only
    mappings and the sequences as tuples. The DataFrames themselves are shared, not copied.
    """

    program_id: str
    datasets: Mapping[str, pd.DataFrame]
    checkpoints: tuple[Checkpoint, ...] = ()
    models: tuple[ModelRecord, ...] = ()
    omission_counts: Mapping[str, int] = field(default_factory=dict)
    halt_message: str | None = None
    glm_skipped: bool = False
    pooled_studynum: int | None = None
    notes: tuple[str, ...] = ()

    def __post_init__(self) -> None:
        _text(self.program_id, "ProgramResult.program_id")
        datasets = dict(self.datasets)
        for name, frame in datasets.items():
            _text(name, f"{self.program_id}: dataset name")
            if not isinstance(frame, pd.DataFrame):
                raise TypeError(
                    f"{self.program_id}: dataset {name!r} must be a pandas DataFrame, "
                    f"got {type(frame).__name__}"
                )
        object.__setattr__(self, "datasets", MappingProxyType(datasets))
        object.__setattr__(
            self,
            "checkpoints",
            _records(self.checkpoints, Checkpoint, f"{self.program_id}: checkpoints"),
        )
        object.__setattr__(
            self, "models", _records(self.models, ModelRecord, f"{self.program_id}: models")
        )
        omission_counts = {
            _text(key, f"{self.program_id}: omission count name"): _count(
                value, f"{self.program_id}: omission count {key}"
            )
            for key, value in dict(self.omission_counts).items()
        }
        object.__setattr__(self, "omission_counts", MappingProxyType(omission_counts))
        _text(self.halt_message, f"{self.program_id}: halt_message", optional=True)
        object.__setattr__(self, "glm_skipped", bool(self.glm_skipped))
        object.__setattr__(
            self,
            "pooled_studynum",
            _optional_count(self.pooled_studynum, f"{self.program_id}: pooled_studynum"),
        )
        object.__setattr__(self, "notes", _records(self.notes, str, f"{self.program_id}: notes"))

    @property
    def halted(self) -> bool:
        """True when a halting check stopped the run (S07-FLT-04, S07-FLT-05)."""
        return self.halt_message is not None

    @classmethod
    def from_halt(cls, halt: AnalysisHalted) -> ProgramResult:
        """Build the result of a run stopped by ``halt``.

        The result carries the halting program, the halt message and the checkpoints the driver
        recorded before the halt, and no datasets or model records, so nothing is exported.
        """
        if not isinstance(halt, AnalysisHalted):
            raise TypeError(f"from_halt expects an AnalysisHalted, got {type(halt).__name__}")
        return cls(
            program_id=halt.program_id,
            datasets={},
            checkpoints=halt.checkpoints,
            models=(),
            halt_message=str(halt),
        )

    def dataset(self, name: str) -> pd.DataFrame:
        """Return the dataset exported as ``name``, or raise ``KeyError`` naming what exists."""
        try:
            return self.datasets[name]
        except KeyError:
            available = ", ".join(self.datasets) or "none"
            raise KeyError(
                f"{self.program_id} has no dataset {name!r}; available: {available}"
            ) from None
