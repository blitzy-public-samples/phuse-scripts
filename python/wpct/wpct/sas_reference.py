"""Parsers of the SAS reference results: GLM listings, log record counts and full-precision exports.

This module is the only reader of SAS reference material. Every SAS value it returns is parsed at
run time from an unchanged repository file or a user-supplied export (AAP 0.7.3): no SAS value is
invented, approximated, or back-filled from Python output. The pinned maps below record only
*where* a value stands and what that line must name; they hold no SAS value, and the parsers never
read a value from them. Files are opened for reading only; nothing here writes.

Reference files ``whitepapers/WPCT/outputs_sas/WPCT-F.07.02.{lst,log}`` and
``whitepapers/WPCT/outputs_sas/WPCT-F.07.07.{lst,log}``: ASCII with LF line endings, produced by
SAS 9.4 TS1M2 with SAS/STAT 13.2, committed in ``6fc757d`` (2016-02-26).

- :data:`LOG_CHECKPOINT_MAP` and :func:`parse_log_checkpoints`: the dataset record counts of the
  logs at the pinned lines of AAP 0.7.3, 15 for 07.02 and 29 for 07.07.
- :func:`parse_log_warning_records` and :class:`LogWarningRecord`: the ``name=value`` pairs SAS
  prints with each WARNING a ``warning_count`` checkpoint counts, such as the LMM-FLT-02 omitted
  baseline records of the 07.07 log, displayed values kept as printed.
- :data:`GLM_TABLE_MAP` and :func:`parse_glm_listing`: the ParameterEstimates tables of the
  listings (S02-MOD-02, S07-MOD-02, S07-MOD-03), displayed tokens kept as printed.
- :func:`load_sas_exports`: the seven optional full-precision exports named in
  ``wpct.schemas.SAS_EXPORT_FILES``.

A pinned line that no longer names its dataset, or a listing table that differs from its map
entry, does not raise: it comes back as ``SasCheckpoint.ok = False`` or in
``ListingTable.problems``, which ``wpct.compare`` reports as FAIL rows, never as skips. A listing
or log that is missing, not a regular file or unreadable raises
:class:`~wpct.errors.InputDataError`.

The exports are optional: an absent export file maps to ``None``, and an absent export directory
maps every file to ``None``. An export directory or entry that is present but unusable (not a
directory, not a regular file, or a broken or looping symlink), or that cannot be examined,
raises ``InputDataError``, and so does a supplied export that is malformed: one that cannot be
read or parsed as CSV, has no header row, has an unnamed column, two columns with the same name
ignoring case or a record of the wrong width, holds a numeric value that is not a SAS number or
lies beyond double precision, or is a parameter-estimates export without a usable ``Parameter``
column.
"""

from __future__ import annotations

import csv
import itertools
import math
import numbers
import os
import re
from collections.abc import Mapping, Sequence
from dataclasses import dataclass, field
from pathlib import Path
from types import MappingProxyType

import numpy as np
import pandas as pd

from wpct.errors import InputDataError, message_path
from wpct.sas_semantics import is_char_missing
from wpct.schemas import (
    INT,
    MODEL_KINDS,
    NULLABLE_INT,
    NUM,
    PROGRAM_07_02,
    PROGRAM_07_07,
    SAS_EXPORT_FILES,
    sas_export_spec,
    upper_columns,
)

__all__ = [
    "GLM_TABLE_MAP",
    "KIND_DATASET",
    "KIND_TABLE",
    "KIND_WARNING_COUNT",
    "LOG_CHECKPOINT_MAP",
    "LOG_LINE_KINDS",
    "WARNING_COUNT_PREFIXES",
    "GlmTableSpec",
    "ListingRow",
    "ListingTable",
    "LogCheckpointSpec",
    "LogWarningRecord",
    "SasCheckpoint",
    "load_sas_exports",
    "parse_glm_listing",
    "parse_log_checkpoints",
    "parse_log_warning_records",
    "split_parameter",
]

_SAS_NAME = re.compile(r"^[A-Z_][A-Z0-9_]*$")


def _check_int(value: object, name: str, *, minimum: int) -> int:
    """Return ``value`` as a built-in ``int`` of at least ``minimum``; bools are refused."""
    if isinstance(value, bool) or not isinstance(value, numbers.Integral):
        raise TypeError(f"{name} must be an integer, got {value!r}")
    if value < minimum:
        raise ValueError(f"{name} must be at least {minimum}, got {value}")
    return int(value)


def _check_number(value: object, name: str) -> float:
    """Return ``value`` as a finite built-in ``float``."""
    if isinstance(value, bool) or not isinstance(value, numbers.Real):
        raise TypeError(f"{name} must be a number, got {value!r}")
    number = float(value)
    if not math.isfinite(number):
        raise ValueError(f"{name} must be finite, got {value!r}")
    return number


def _check_text(value: object, name: str, *, optional: bool = False) -> str | None:
    """Check that ``value`` is a non-empty string, or ``None`` where ``optional``."""
    if value is None and optional:
        return None
    if not isinstance(value, str) or not value:
        raise TypeError(f"{name} must be a non-empty string, got {value!r}")
    return value


def _read_reference_lines(path: str | os.PathLike[str], what: str) -> tuple[str, ...]:
    """Return the lines of a reference text file, numbered from 1 by position + 1.

    The bytes are decoded as Latin-1, which accepts every byte, so an edited copy cannot fail to
    decode, and split on LF only: universal-newline reading would also split on a lone CR and
    shift the pinned line numbers. A CR before the LF and trailing blanks are dropped from each
    line; a final LF does not start another line.
    """
    abs_path = Path(path).absolute()
    shown = message_path(abs_path)
    try:
        is_file = abs_path.is_file()
    except OSError as exc:
        # pathlib reports a missing path as False but raises for other stat failures.
        raise InputDataError(f"cannot read {what} {shown}: {exc}") from exc
    if not is_file:
        raise InputDataError(f"{what} not found: {shown}")
    try:
        text = abs_path.read_bytes().decode("latin-1")
    except OSError as exc:
        raise InputDataError(f"cannot read {what} {shown}: {exc}") from exc
    lines = text.split("\n")
    if lines[-1] == "":
        lines.pop()
    return tuple(line.rstrip("\r").rstrip(" ") for line in lines)


# --- Log checkpoints ------------------------------------------------------------------------------

#: Kinds of pinned log line. ``dataset``: ``NOTE: The data set WORK.<X> has <n> observations and
#: <v> variables.`` ``table``: ``NOTE: Table WORK.<X> created, with <n> rows and <v> columns.``
#: ``warning_count``: the number of WARNING lines with a fixed prefix in a pinned line range.
KIND_DATASET = "dataset"
KIND_TABLE = "table"
KIND_WARNING_COUNT = "warning_count"
LOG_LINE_KINDS = (KIND_DATASET, KIND_TABLE, KIND_WARNING_COUNT)

#: The prefix of the WARNING lines each ``warning_count`` checkpoint counts, by its SAS_DATASET.
#: ``%derive_lastminmax_measure`` writes one per baseline record without a post-baseline record
#: (LMM-FLT-02) [whitepapers/ADaM/derive_lastminmax_measure.sas:167;
#: whitepapers/WPCT/outputs_sas/WPCT-F.07.07.log:124-178]. The macro spells its tag MEASURES.
WARNING_COUNT_PREFIXES: Mapping[str, str] = MappingProxyType(
    {
        "LMM_BASE_OMITTED": (
            "WARNING: (DERIVE_LASTMINMAX_MEASURES) Omitting obs without a post-baseline measure"
        ),
    }
)

_DATASET_NOTE = re.compile(
    r"^NOTE: The data set WORK\.(?P<name>[A-Za-z_][A-Za-z0-9_]*) has (?P<nobs>\d+) "
    r"observations and (?P<nvars>\d+) variables\.$"
)
_TABLE_NOTE = re.compile(
    r"^NOTE: Table WORK\.(?P<name>[A-Za-z_][A-Za-z0-9_]*) created, with (?P<nobs>\d+) rows "
    r"and (?P<nvars>\d+) columns\.$"
)
_NOTE_PATTERNS = MappingProxyType({KIND_DATASET: _DATASET_NOTE, KIND_TABLE: _TABLE_NOTE})

# The SAS message tag that starts a log line, with the parenthesised macro tag that may follow it,
# such as ``WARNING: (DERIVE_LASTMINMAX_MEASURES)``. Each part must end at a blank or the line
# end, so no part of a ``name=value`` token is ever matched.
_LOG_TAG = re.compile(r"^(?:NOTE|WARNING|ERROR|INFO):(?: \([A-Za-z_][A-Za-z0-9_]{0,31}\))?(?= |$)")


def _described_line(text: str) -> str:
    """Describe a log line inside a problem message without echoing a value the line prints.

    A problem becomes a FAIL reason of the committed equivalence report, which holds only
    aggregates, estimates, counts and record keys (AAP 0.11.2), yet a log line can print
    subject-level values: ``put _all_`` writes ``name=value`` pairs such as ``USUBJID=`` and
    ``AVAL=`` on a WARNING line and its continuation lines, and SAS prints ERROR put lists and
    invalid-data dumps the same way. A line is therefore quoted in full only when it is wholly a
    dataset or table NOTE of ``_NOTE_PATTERNS``, whose only variable parts are a WORK dataset name
    and two counts. Any other line is shown by the SAS message and macro tags it starts with
    (``_LOG_TAG``), or else as a line of another form, and its text is not shown.
    """
    if not text:
        return "(blank line)"
    if any(pattern.fullmatch(text) for pattern in _NOTE_PATTERNS.values()):
        return text
    tag = _LOG_TAG.match(text)
    if tag:
        return f"a {tag[0]!r} line (text not shown)"
    return "a line of another form (text not shown)"


@dataclass(frozen=True)
class LogCheckpointSpec:
    """Where one SAS record count stands in a reference log, and what that line must name.

    Attributes:
        SAS_DATASET: The WORK dataset name in upper case, or for a ``warning_count`` a key of
            :data:`WARNING_COUNT_PREFIXES`. The drivers record their ``Checkpoint`` under the same
            name.
        OCCURRENCE: The 1-based count of this name among the mapped checkpoints of the program,
            in log order, which is the order the driver records them. Log lines that only
            restructure the same rows are not mapped and not counted, so ``CSS_PVALUES`` 3 of 07.02
            is log line 1414, not the third ``CSS_PVALUES`` NOTE of the log.
        PARAMCD: The stratum parameter of a stratum-level checkpoint, otherwise ``None``.
        ATPTN: The stratum timepoint of a timepoint-level checkpoint, otherwise ``None``.
        line: The 1-based log line of the NOTE, or the first line of a ``warning_count`` range.
        end_line: The last line of a ``warning_count`` range; ``None`` for the other kinds.
        kind: One of :data:`LOG_LINE_KINDS`.
        has_nvars: True where AAP 0.7.3 compares ``obs/vars``; only then is the variable count
            parsed and compared.
        description: What the count is, with the specification item it evidences.

    Construction validates every field and raises ``TypeError`` or ``ValueError`` on a bad value,
    so a map edit cannot drift silently. ``ATPTN`` is stored as a built-in ``float``.
    """

    SAS_DATASET: str
    OCCURRENCE: int
    PARAMCD: str | None
    ATPTN: float | None
    line: int
    end_line: int | None
    kind: str
    has_nvars: bool
    description: str

    def __post_init__(self) -> None:
        name = _check_text(self.SAS_DATASET, "LogCheckpointSpec.SAS_DATASET")
        if not _SAS_NAME.match(name):
            raise ValueError(f"LogCheckpointSpec.SAS_DATASET must be an upper-case name: {name!r}")
        occurrence = _check_int(self.OCCURRENCE, "LogCheckpointSpec.OCCURRENCE", minimum=1)
        object.__setattr__(self, "OCCURRENCE", occurrence)
        _check_text(self.PARAMCD, "LogCheckpointSpec.PARAMCD", optional=True)
        if self.ATPTN is not None:
            object.__setattr__(self, "ATPTN", _check_number(self.ATPTN, "LogCheckpointSpec.ATPTN"))
        object.__setattr__(self, "line", _check_int(self.line, "LogCheckpointSpec.line", minimum=1))
        if self.kind not in LOG_LINE_KINDS:
            raise ValueError(f"LogCheckpointSpec.kind must be in {LOG_LINE_KINDS}: {self.kind!r}")
        if not isinstance(self.has_nvars, bool):
            raise TypeError(f"LogCheckpointSpec.has_nvars must be a bool, got {self.has_nvars!r}")
        if not isinstance(self.description, str):
            raise TypeError("LogCheckpointSpec.description must be a string")
        if self.kind == KIND_WARNING_COUNT:
            if name not in WARNING_COUNT_PREFIXES:
                raise ValueError(f"no WARNING prefix is defined for warning count {name}")
            object.__setattr__(
                self,
                "end_line",
                _check_int(self.end_line, "LogCheckpointSpec.end_line", minimum=self.line),
            )
            if self.has_nvars:
                raise ValueError(f"warning count {name} has no variable count")
        elif self.end_line is not None:
            raise ValueError(f"LogCheckpointSpec.end_line applies only to {KIND_WARNING_COUNT}")


@dataclass(frozen=True)
class SasCheckpoint:
    """One record count read from a reference log at its pinned line.

    Attributes:
        spec: The map entry that was read.
        found_dataset: The WORK dataset the line names, as printed; ``None`` when the line is not a
            NOTE of the expected kind, and for a ``warning_count``.
        NOBS: The observation (or row, or WARNING) count read from the log; ``None`` unless ``ok``.
        NVARS: The variable count, filled only where ``spec.has_nvars`` and ``ok``.
        line_text: The pinned line as read, without trailing blanks; ``""`` beyond the end of the
            log. For a ``warning_count``, the first line of the range. It can hold the
            subject-level values a log line prints, so no message or report shows it.
        ok: True when the line names ``WORK.<SAS_DATASET>`` in the expected NOTE form, or, for a
            ``warning_count``, when both ends of the range are counted WARNING lines.
        problem: Why ``ok`` is False, otherwise ``None``; ``wpct.compare`` reports it as a FAIL.
            It names the line and the expected ``WORK.<SAS_DATASET>`` or WARNING prefix, and
            quotes the line found there only when it is a dataset or table NOTE: any other line is
            shown by its SAS message tag or its form, never its text.
    """

    spec: LogCheckpointSpec
    found_dataset: str | None
    NOBS: int | None
    NVARS: int | None
    line_text: str
    ok: bool
    problem: str | None


def _dataset(
    line: int,
    name: str,
    occurrence: int,
    description: str,
    *,
    paramcd: str | None = None,
    atptn: float | None = None,
    nvars: bool = False,
) -> LogCheckpointSpec:
    """A ``NOTE: The data set WORK.<name> has ...`` checkpoint."""
    return LogCheckpointSpec(
        SAS_DATASET=name,
        OCCURRENCE=occurrence,
        PARAMCD=paramcd,
        ATPTN=atptn,
        line=line,
        end_line=None,
        kind=KIND_DATASET,
        has_nvars=nvars,
        description=description,
    )


def _table(line: int, name: str, occurrence: int, description: str) -> LogCheckpointSpec:
    """A PROC SQL ``NOTE: Table WORK.<name> created, with ...`` checkpoint (rows only)."""
    return LogCheckpointSpec(
        SAS_DATASET=name,
        OCCURRENCE=occurrence,
        PARAMCD=None,
        ATPTN=None,
        line=line,
        end_line=None,
        kind=KIND_TABLE,
        has_nvars=False,
        description=description,
    )


def _warning_count(
    line: int, end_line: int, name: str, occurrence: int, description: str
) -> LogCheckpointSpec:
    """A count of the :data:`WARNING_COUNT_PREFIXES` lines of ``name`` in ``line``-``end_line``."""
    return LogCheckpointSpec(
        SAS_DATASET=name,
        OCCURRENCE=occurrence,
        PARAMCD=None,
        ATPTN=None,
        line=line,
        end_line=end_line,
        kind=KIND_WARNING_COUNT,
        has_nvars=False,
        description=description,
    )


# The trailing comment of each entry is the count its line holds, for readers only; the parser
# reads every value from the log itself.
_LOG_07_02 = (
    _dataset(32, "ADVS", 1, "ADVS as read (S02-DEP-util_access_test_data)",
             nvars=True),  # 32139/44
    _dataset(59, "ADVS_SUB", 1, "Parameter/timepoint subset (S02-FLT-01, S02-TRN-02)",
             nvars=True),  # 5925/45
    _dataset(202, "CSS_ANADATA", 1, "Analysis set (S02-POP-01, S02-FLT-02/03, S02-TRN-03)",
             nvars=True),  # 3548/14
    _dataset(726, "CSS_NEXTPARAM", 1, "Records of the parameter (S02-TRN-04)",
             paramcd="DIABP"),  # 1774
    _dataset(803, "CSS_NEXTTIMEPT", 1, "Records of the timepoint (S02-TRN-04)",
             paramcd="DIABP", atptn=815),  # 1774
    _dataset(978, "CSS_STATS", 1, "PROC SUMMARY statistics (S02-MOD-01)",
             paramcd="DIABP", atptn=815, nvars=True),  # 30/12
    _dataset(992, "CSS_PVALUES", 1, "Endpoint ParameterEstimates, all rows (S02-MOD-02)",
             paramcd="DIABP", atptn=815),  # 5
    _dataset(1024, "CSS_PVALUES", 2, "TRTPN rows kept for the merge (S02-TRN-05)",
             paramcd="DIABP", atptn=815),  # 3
    _dataset(1049, "CSS_STATS", 2, "Statistics with the merged pval (S02-TRN-05)",
             paramcd="DIABP", atptn=815, nvars=True),  # 30/13
    _dataset(1141, "CSS_NEXTPARAM", 2, "Records of the parameter (S02-TRN-04)",
             paramcd="SYSBP"),  # 1774
    _dataset(1219, "CSS_NEXTTIMEPT", 2, "Records of the timepoint (S02-TRN-04)",
             paramcd="SYSBP", atptn=816),  # 1774
    _dataset(1400, "CSS_STATS", 3, "PROC SUMMARY statistics (S02-MOD-01)",
             paramcd="SYSBP", atptn=816, nvars=True),  # 30/12
    _dataset(1414, "CSS_PVALUES", 3, "Endpoint ParameterEstimates, all rows (S02-MOD-02)",
             paramcd="SYSBP", atptn=816),  # 5
    _dataset(1446, "CSS_PVALUES", 4, "TRTPN rows kept for the merge (S02-TRN-05)",
             paramcd="SYSBP", atptn=816),  # 3
    _dataset(1471, "CSS_STATS", 4, "Statistics with the merged pval (S02-TRN-05)",
             paramcd="SYSBP", atptn=816, nvars=True),  # 30/13
)

_LOG_07_07 = (
    _dataset(32, "ADVS", 1, "ADVS as read (S07-DEP-util_access_test_data)",
             nvars=True),  # 32139/44
    _dataset(61, "ADVS_SUB", 1, "Parameter/timepoint subset (S07-FLT-01, S07-TRN-02)",
             nvars=True),  # 5926/45
    _dataset(79, "LMM_BASE", 1,
             "Derivation baseline records (LMM-FLT-01, LMM-TRN-01)"),  # 1514
    _dataset(80, "LMM_POST", 1,
             "Derivation post-baseline records (LMM-FLT-01, LMM-TRN-01)"),  # 4411
    _table(95, "LMM_BASE_ANL", 1,
           "Baseline records at the group minimum (LMM-TRN-02, LMM-TRN-03)"),  # 629
    _table(98, "LMM_POST_ANL", 1,
           "Post-baseline records at the group minimum (LMM-TRN-02, LMM-TRN-03)"),  # 794
    _dataset(108, "LMM_BASE_ANL", 2,
             "Baseline records after NODUPKEY (LMM-TRN-04)"),  # 508
    _dataset(117, "LMM_POST_ANL", 2,
             "Post-baseline records after NODUPKEY (LMM-TRN-04)"),  # 498
    _warning_count(124, 178, "LMM_BASE_OMITTED", 1,
                   "Baseline records without a post-baseline record (LMM-FLT-02)"),  # 10
    _dataset(186, "LMM_BASE_ANL", 3,
             "Baseline records of groups with both phases (LMM-FLT-02, LMM-TRN-05)"),  # 498
    _dataset(195, "LMM_POST_ANL", 3,
             "Post-baseline records of groups with both phases (LMM-TRN-06)"),  # 498
    _dataset(205, "ADVS_MIN", 1, "Derived MIN records (S07-TRN-03, LMM-TRN-07)",
             nvars=True),  # 996/16
    _dataset(229, "ADVS_MIN", 2, "Post-baseline derived records (S07-FLT-02)",
             nvars=True),  # 498/16
    _dataset(367, "CSS_SAFANA", 1, "Analysis set (S07-POP-01, S07-FLT-03, S07-TRN-04)",
             nvars=True),  # 498/12
    _dataset(399, "CSS_ANADATA", 1, "Stacked with the pooled copy (S07-TRN-05)",
             nvars=True),  # 996/12
    _dataset(427, "CSS_ANADATA", 2, "Numbered by study (S07-TRN-06)",
             nvars=True),  # 996/13
    _dataset(859, "CSS_NEXTPARAM", 1, "Records of the parameter (S07-TRN-07)",
             paramcd="DIABP"),  # 996
    _dataset(936, "CSS_NEXTTIMEPT", 1, "Records of the timepoint (S07-TRN-07)",
             paramcd="DIABP", atptn=815),  # 500
    _dataset(1111, "CSS_STATS", 1, "PROC SUMMARY statistics (S07-MOD-01)",
             paramcd="DIABP", atptn=815, nvars=True),  # 9/12
    _dataset(1128, "CSS_PVAL_STDY", 1, "Per-study ParameterEstimates (S07-MOD-02)",
             paramcd="DIABP", atptn=815),  # 10
    _dataset(1144, "CSS_PVAL_POOL", 1, "Pooled ParameterEstimates (S07-MOD-03)",
             paramcd="DIABP", atptn=815),  # 6
    _dataset(1162, "CSS_PVAL_STDY", 2, "TRTPN rows of the study and pooled fits (S07-TRN-08)",
             paramcd="DIABP", atptn=815),  # 9
    _dataset(1215, "CSS_STATS", 2, "Statistics with the merged pval (S07-TRN-08)",
             paramcd="DIABP", atptn=815, nvars=True),  # 9/13
    _dataset(1295, "CSS_NEXTTIMEPT", 2, "Records of the timepoint (S07-TRN-07)",
             paramcd="DIABP", atptn=817),  # 496
    _dataset(1476, "CSS_STATS", 3, "PROC SUMMARY statistics (S07-MOD-01)",
             paramcd="DIABP", atptn=817, nvars=True),  # 9/12
    _dataset(1493, "CSS_PVAL_STDY", 3, "Per-study ParameterEstimates (S07-MOD-02)",
             paramcd="DIABP", atptn=817),  # 10
    _dataset(1509, "CSS_PVAL_POOL", 2, "Pooled ParameterEstimates (S07-MOD-03)",
             paramcd="DIABP", atptn=817),  # 6
    _dataset(1528, "CSS_PVAL_STDY", 4, "TRTPN rows of the study and pooled fits (S07-TRN-08)",
             paramcd="DIABP", atptn=817),  # 9
    _dataset(1581, "CSS_STATS", 4, "Statistics with the merged pval (S07-TRN-08)",
             paramcd="DIABP", atptn=817, nvars=True),  # 9/13
)

#: The pinned log checkpoints of each program, in log order (AAP 0.7.3): 15 lines of
#: ``WPCT-F.07.02.log`` and 29 of ``WPCT-F.07.07.log``. Lines that only restructure the same rows
#: (``TEMP``, helper datasets, and the KEEP/RENAME/SORT steps on the p-value tables such as 07.02
#: line 1036 and 07.07 lines 1190 and 1202) are not mapped.
LOG_CHECKPOINT_MAP: Mapping[str, tuple[LogCheckpointSpec, ...]] = MappingProxyType(
    {PROGRAM_07_02: _LOG_07_02, PROGRAM_07_07: _LOG_07_07}
)


def parse_log_checkpoints(
    path: str | os.PathLike[str],
    # The published keyword is ``map``, so it shadows the builtin, which this body never calls.
    map: Sequence[LogCheckpointSpec],
) -> tuple[SasCheckpoint, ...]:
    """Read the record count at every pinned line of one reference log.

    Args:
        path: The SAS log, e.g. ``paths.log_file("WPCT-F.07.02")``.
        map: One program's entries, e.g. ``LOG_CHECKPOINT_MAP["WPCT-F.07.02"]``.

    Returns:
        One :class:`SasCheckpoint` per entry, in map order. A ``dataset`` or ``table`` line is
        ``ok`` only when it is a NOTE of that kind naming ``WORK.<SAS_DATASET>``; otherwise it
        carries ``"line <n> does not name WORK.<X>: <line>"`` and no counts. A ``warning_count``
        is ``ok`` only when both ends of its range start its WARNING prefix; otherwise it carries
        ``"line <n> does not start the warning '<prefix>': <line>"`` for each end that does not,
        and no count. ``<line>`` is the line in full when it is a dataset or table NOTE,
        ``(blank line)``, or its SAS message tag or form with its text not shown, since a log line
        can print subject-level values.

    Raises:
        InputDataError: ``"reference log not found: <absolute path>"`` when the path is not a
            regular file, or ``"cannot read reference log <absolute path>: <reason>"``.
        TypeError: When ``map`` is a mapping or holds anything but :class:`LogCheckpointSpec`
            entries.
    """
    if isinstance(map, (Mapping, str)):
        raise TypeError("map must hold one program's entries: LOG_CHECKPOINT_MAP[pid]")
    specs = tuple(map)
    for position, spec in enumerate(specs):
        if not isinstance(spec, LogCheckpointSpec):
            raise TypeError(
                f"map[{position}] must be a LogCheckpointSpec, got {type(spec).__name__}"
            )
    lines = _read_reference_lines(path, "reference log")
    return tuple(_read_checkpoint(spec, lines) for spec in specs)


def _read_checkpoint(spec: LogCheckpointSpec, lines: tuple[str, ...]) -> SasCheckpoint:
    if spec.kind == KIND_WARNING_COUNT:
        return _count_warnings(spec, lines)
    expected = f"WORK.{spec.SAS_DATASET}"
    if spec.line > len(lines):
        return SasCheckpoint(
            spec=spec,
            found_dataset=None,
            NOBS=None,
            NVARS=None,
            line_text="",
            ok=False,
            problem=(
                f"line {spec.line} does not name {expected}: the log has only {len(lines)} lines"
            ),
        )
    text = lines[spec.line - 1]
    match = _NOTE_PATTERNS[spec.kind].match(text)
    found = match["name"] if match else None
    if found is None or found.upper() != spec.SAS_DATASET:
        # The counts of another dataset's NOTE are not this checkpoint's, so none are returned.
        return SasCheckpoint(
            spec=spec,
            found_dataset=found,
            NOBS=None,
            NVARS=None,
            line_text=text,
            ok=False,
            problem=f"line {spec.line} does not name {expected}: {_described_line(text)}",
        )
    return SasCheckpoint(
        spec=spec,
        found_dataset=found,
        NOBS=int(match["nobs"]),
        NVARS=int(match["nvars"]) if spec.has_nvars else None,
        line_text=text,
        ok=True,
        problem=None,
    )


def _count_warnings(spec: LogCheckpointSpec, lines: tuple[str, ...]) -> SasCheckpoint:
    """Count the WARNING lines of ``spec`` in its pinned range.

    Both ends of the range must themselves be counted WARNING lines, not merely hold at least one
    between them: that pins the range as tightly as a NOTE line is pinned, so a shifted log is
    reported as a FAIL rather than counted over the wrong lines. Each end that fails is named with
    the prefix it lacks and described by ``_described_line``, which never echoes a ``name=value``
    pair that ``put _all_`` printed on it.
    """
    prefix = WARNING_COUNT_PREFIXES[spec.SAS_DATASET]
    if spec.end_line > len(lines):
        return SasCheckpoint(
            spec=spec,
            found_dataset=None,
            NOBS=None,
            NVARS=None,
            line_text=lines[spec.line - 1] if spec.line <= len(lines) else "",
            ok=False,
            problem=(
                f"lines {spec.line}-{spec.end_line} do not hold {spec.SAS_DATASET} warnings: "
                f"the log has only {len(lines)} lines"
            ),
        )
    first, last = lines[spec.line - 1], lines[spec.end_line - 1]
    problems = [
        f"line {number} does not start the warning {prefix!r}: {_described_line(text)}"
        for number, text in ((spec.line, first), (spec.end_line, last))
        if not text.startswith(prefix)
    ]
    if spec.end_line == spec.line:
        problems = problems[:1]
    count = sum(1 for text in lines[spec.line - 1 : spec.end_line] if text.startswith(prefix))
    return SasCheckpoint(
        spec=spec,
        found_dataset=None,
        NOBS=None if problems else count,
        NVARS=None,
        line_text=first,
        ok=not problems,
        problem="; ".join(problems) if problems else None,
    )


# A name SAS's ``put _all_`` prints before ``=``: a variable or automatic variable (``AVAL``,
# ``avisit``, ``_N_``), or a BY-group indicator ``FIRST.<var>`` or ``LAST.<var>``.
_PUT_NAME = r"[A-Za-z_][A-Za-z0-9_]*(?:\.[A-Za-z_][A-Za-z0-9_]*)?"
# A pair starts the joined record or follows a blank; a continuation line starts with a pair.
_PUT_PAIR = re.compile(rf"(?:^| )(?P<name>{_PUT_NAME})=")
_PUT_CONTINUATION = re.compile(rf"^{_PUT_NAME}=")


@dataclass(frozen=True)
class LogWarningRecord:
    """The record one counted WARNING of a ``warning_count`` checkpoint prints with ``_all_``.

    ``%derive_lastminmax_measure`` writes ``put '<text> ' _all_;`` for each baseline record it
    omits because its group has no post-baseline record (LMM-FLT-02)
    [whitepapers/ADaM/derive_lastminmax_measure.sas:167]: the WARNING text, then ``name=value``
    for every variable of the program data vector, wrapped across log lines between pairs
    [whitepapers/WPCT/outputs_sas/WPCT-F.07.07.log:124-183].

    Attributes:
        line: The 1-based log line of the WARNING.
        values: Each printed name mapped to its displayed value, in printed order, as a read-only
            mapping. Names keep their printed case (``AVAL``, ``avisit``, ``FIRST.STUDYID``).
            Values are the displayed text: ``"."`` for a missing numeric, ``""`` for a blank
            character value, and the ``DATE9.`` text of a date such as ``ADT=13MAY2013``, whose SAS
            day number (D-10) the caller derives.
    """

    line: int
    values: Mapping[str, str] = field(hash=False)

    def __post_init__(self) -> None:
        object.__setattr__(self, "values", MappingProxyType(dict(self.values)))


def parse_log_warning_records(
    path: str | os.PathLike[str], spec: LogCheckpointSpec
) -> tuple[LogWarningRecord, ...]:
    """Read the ``name=value`` pairs printed with each counted WARNING of a ``warning_count``.

    The counted WARNINGs are the lines :func:`parse_log_checkpoints` counts for ``spec``: those of
    ``spec.line``-``spec.end_line`` that start with ``WARNING_COUNT_PREFIXES[spec.SAS_DATASET]``.
    For LMM_BASE_OMITTED each prints one baseline record omitted for want of a post-baseline
    record (LMM-FLT-02) [whitepapers/WPCT/outputs_sas/WPCT-F.07.07.log:124-183].

    ``put '<text> ' _all_;`` [whitepapers/ADaM/derive_lastminmax_measure.sas:167] prints the
    WARNING text, then ``name=value`` for every variable, and the log wraps between pairs. A record
    is therefore the rest of its WARNING line after the prefix plus every following line that
    starts with a ``name=`` token, joined with one blank, so the record of the last counted line
    continues past ``spec.end_line`` (07.07 log lines 179-183). The text before the first pair is
    the rest of the WARNING text (``, such as``) and is not returned. Any other line of the range,
    such as a WARNING with another prefix, is skipped along with its continuation lines.

    A name is a SAS name, or ``FIRST.`` or ``LAST.`` followed by a BY variable. A value runs to the
    blank before the next ``name=`` token, so it may hold blanks
    (``PARAM=Diastolic Blood Pressure (mmHg)``) or be empty (``avisit=``). SAS prints values
    unquoted, so a value that itself contains a blank followed by ``name=`` cannot be told apart
    from the next pair: the record is split there.

    Args:
        path: The SAS log, e.g. ``paths.log_file("WPCT-F.07.07")``.
        spec: A ``warning_count`` entry of :data:`LOG_CHECKPOINT_MAP`.

    Returns:
        One :class:`LogWarningRecord` per counted line, in log order: none when the range holds no
        counted line or lies past the end of the log. :func:`parse_log_checkpoints` verifies the
        range, whose checkpoint is ``ok`` only when both of its ends are counted lines.

    Raises:
        TypeError: When ``spec`` is not a :class:`LogCheckpointSpec`.
        ValueError: When ``spec`` is not of kind ``warning_count``, or when one record prints a
            name twice, ignoring case as SAS names do.
        InputDataError: ``"reference log not found: <absolute path>"`` when the path is not a
            regular file, or ``"cannot read reference log <absolute path>: <reason>"``.
    """
    if not isinstance(spec, LogCheckpointSpec):
        raise TypeError(f"spec must be a LogCheckpointSpec, got {type(spec).__name__}")
    if spec.kind != KIND_WARNING_COUNT:
        raise ValueError(
            f"{spec.SAS_DATASET} at log line {spec.line} is a {spec.kind} checkpoint, not a "
            f"{KIND_WARNING_COUNT}"
        )
    prefix = WARNING_COUNT_PREFIXES[spec.SAS_DATASET]
    lines = _read_reference_lines(path, "reference log")
    records: list[LogWarningRecord] = []
    for number in range(spec.line, min(spec.end_line, len(lines)) + 1):
        text = lines[number - 1]
        if not text.startswith(prefix):
            continue
        parts = [text[len(prefix) :]]
        parts.extend(
            itertools.takewhile(_PUT_CONTINUATION.match, itertools.islice(lines, number, None))
        )
        records.append(LogWarningRecord(line=number, values=_put_pairs(" ".join(parts), number)))
    return tuple(records)


def _put_pairs(text: str, line: int) -> dict[str, str]:
    """Split one joined ``put _all_`` record into its ``name=value`` pairs, in printed order."""
    pairs: dict[str, str] = {}
    printed: dict[str, str] = {}
    matches = list(_PUT_PAIR.finditer(text))
    for match, following in itertools.zip_longest(matches, matches[1:]):
        name = match["name"]
        if name.upper() in printed:
            raise ValueError(
                f"the WARNING record at log line {line} prints {printed[name.upper()]}= and "
                f"{name}=, the same SAS name ignoring case"
            )
        printed[name.upper()] = name
        pairs[name] = text[match.end() : len(text) if following is None else following.start()]
    return pairs


# --- GLM listings ---------------------------------------------------------------------------------

_ENDPOINT, _STUDY, _POOLED = MODEL_KINDS

#: The response every listed model must name; SAS prints ``CHG`` in 07.02 and ``chg`` in 07.07
#: [whitepapers/WPCT/outputs_sas/WPCT-F.07.02.lst:5;
#: whitepapers/WPCT/outputs_sas/WPCT-F.07.07.lst:7].
_RESPONSE = "CHG"


@dataclass(frozen=True)
class GlmTableSpec:
    """The stratum, model and expected layout of one ParameterEstimates table of a listing.

    Tables map to strata by SAS loop order, not by page title: each title is set after the PROC
    GLM step it belongs to, so titles lag one loop iteration
    [whitepapers/WPCT/outputs_sas/WPCT-F.07.02.lst:15].

    Attributes:
        index: The 0-based position of the table in the listing.
        PARAMCD: The stratum parameter.
        ATPTN: The stratum timepoint.
        MODEL: ``"endpoint"`` (S02-MOD-02), ``"study"`` (S07-MOD-02) or ``"pooled"`` (S07-MOD-03).
        STUDYNUM: The ``studynum`` of a per-study fit; ``None`` for endpoint and pooled tables. The
            pooled rows are matched through ``ProgramResult.pooled_studynum`` (``&stdyn``).
        AVISITN: The visit the 07.02 endpoint fit selects (99); ``None`` for 07.07.
        by_line: The BY line printed above the table (``"studynum=1"``), or ``None``.
        lines: The 1-based listing lines of the column header and of the last parameter row.
        parameters: The expected ``(EFFECT, LEVEL)`` rows in SAS design-column order.
    """

    index: int
    PARAMCD: str
    ATPTN: float
    MODEL: str
    STUDYNUM: int | None
    AVISITN: float | None
    by_line: str | None
    lines: tuple[int, int]
    parameters: tuple[tuple[str, str], ...]

    def __post_init__(self) -> None:
        object.__setattr__(self, "index", _check_int(self.index, "GlmTableSpec.index", minimum=0))
        _check_text(self.PARAMCD, "GlmTableSpec.PARAMCD")
        object.__setattr__(self, "ATPTN", _check_number(self.ATPTN, "GlmTableSpec.ATPTN"))
        if self.MODEL not in MODEL_KINDS:
            raise ValueError(f"GlmTableSpec.MODEL must be one of {MODEL_KINDS}: {self.MODEL!r}")
        if self.MODEL == _STUDY:
            object.__setattr__(
                self, "STUDYNUM", _check_int(self.STUDYNUM, "GlmTableSpec.STUDYNUM", minimum=1)
            )
        elif self.STUDYNUM is not None:
            raise ValueError(f"GlmTableSpec.STUDYNUM applies only to {_STUDY} tables")
        if self.AVISITN is not None:
            object.__setattr__(self, "AVISITN", _check_number(self.AVISITN, "GlmTableSpec.AVISITN"))
        _check_text(self.by_line, "GlmTableSpec.by_line", optional=True)
        lines = tuple(self.lines)
        if len(lines) != 2:
            raise ValueError(f"GlmTableSpec.lines must be (header, last row), got {self.lines!r}")
        header = _check_int(lines[0], "GlmTableSpec.lines[0]", minimum=1)
        last = _check_int(lines[1], "GlmTableSpec.lines[1]", minimum=header)
        object.__setattr__(self, "lines", (header, last))
        parameters = tuple(tuple(pair) for pair in self.parameters)
        if not parameters or any(
            len(pair) != 2 or not all(isinstance(part, str) for part in pair) for pair in parameters
        ):
            raise ValueError("GlmTableSpec.parameters must be (EFFECT, LEVEL) string pairs")
        object.__setattr__(self, "parameters", parameters)


# SAS design-column order (D-05, D-07): Intercept, BASE, the TRTPN levels ascending with the
# REF="0" level last, then the continuous pooled studynum (D-06, aliased by the Intercept).
_PARAMETERS = (("INTERCEPT", ""), ("BASE", ""), ("TRTPN", "54"), ("TRTPN", "81"), ("TRTPN", "0"))
_POOLED_PARAMETERS = (*_PARAMETERS, ("STUDYNUM", ""))


def _glm_table(
    index: int,
    paramcd: str,
    atptn: float,
    model: str,
    lines: tuple[int, int],
    *,
    studynum: int | None = None,
    avisitn: float | None = None,
) -> GlmTableSpec:
    return GlmTableSpec(
        index=index,
        PARAMCD=paramcd,
        ATPTN=atptn,
        MODEL=model,
        STUDYNUM=studynum,
        AVISITN=avisitn,
        by_line=None if studynum is None else f"studynum={studynum}",
        lines=lines,
        parameters=_POOLED_PARAMETERS if model == _POOLED else _PARAMETERS,
    )


#: The ParameterEstimates tables of each listing, in file order: 07.02 DIABP/815 then SYSBP/816
#: [whitepapers/WPCT/outputs_sas/WPCT-F.07.02.lst:8-14,23-29]; 07.07 DIABP/815 for ``studynum`` 1,
#: 2 and pooled, then DIABP/817 for the same three
#: [whitepapers/WPCT/outputs_sas/WPCT-F.07.07.lst:10-16,26-32,40-47,58-64,100-106,140-147].
GLM_TABLE_MAP: Mapping[str, tuple[GlmTableSpec, ...]] = MappingProxyType(
    {
        PROGRAM_07_02: (
            _glm_table(0, "DIABP", 815, _ENDPOINT, (8, 14), avisitn=99),
            _glm_table(1, "SYSBP", 816, _ENDPOINT, (23, 29), avisitn=99),
        ),
        PROGRAM_07_07: (
            _glm_table(0, "DIABP", 815, _STUDY, (10, 16), studynum=1),
            _glm_table(1, "DIABP", 815, _STUDY, (26, 32), studynum=2),
            _glm_table(2, "DIABP", 815, _POOLED, (40, 47)),
            _glm_table(3, "DIABP", 817, _STUDY, (58, 64), studynum=1),
            _glm_table(4, "DIABP", 817, _STUDY, (100, 106), studynum=2),
            _glm_table(5, "DIABP", 817, _POOLED, (140, 147)),
        ),
    }
)


@dataclass(frozen=True)
class ListingRow:
    """One ParameterEstimates row as the listing displays it.

    Attributes:
        PARAMETER: The label with internal runs of blanks collapsed, e.g. ``"TRTPN 54"``.
        EFFECT: The upper-case effect, ``INTERCEPT``, ``BASE``, ``TRTPN`` or ``STUDYNUM``.
        LEVEL: The CLASS level label (``"54"``), or ``""`` for a continuous term.
        ESTIMATE: The displayed estimate, e.g. ``"-1.29895697"``.
        BIASED: True where SAS flags the estimate ``B`` (not uniquely estimable).
        STDERR: The displayed standard error.
        TVALUE: The displayed t statistic.
        PROBT: The displayed ``Pr > |t|``, e.g. ``"0.3031"`` or the bound ``"<.0001"``.
        line: The 1-based listing line.

    ``STDERR``, ``TVALUE`` and ``PROBT`` are ``"."`` where SAS prints a missing value, and ``""``
    where the listing shows nothing: after ``options missing=' '`` takes effect, SAS prints missing
    values as blanks [whitepapers/WPCT/outputs_sas/WPCT-F.07.02.lst:14,29]. Both mean missing.
    """

    PARAMETER: str
    EFFECT: str
    LEVEL: str
    ESTIMATE: str
    BIASED: bool
    STDERR: str
    TVALUE: str
    PROBT: str
    line: int


@dataclass(frozen=True)
class ListingTable:
    """One ParameterEstimates table of a listing, checked against its :class:`GlmTableSpec`.

    Attributes:
        index: The 0-based position in the listing (or in the map, for a missing table).
        by_line: The BY line printed above the table, or ``None``.
        dependent: The name after ``Dependent Variable:``, or ``None`` when absent.
        rows: The parameter rows in listing order; empty for a table missing from the listing.
        first_line: The 1-based line of the column header; ``None`` for a missing table.
        last_line: The 1-based line of the last parameter row (the header line when there is
            none); ``None`` for a missing table.
        spec: The map entry of this position, or ``None`` for a table the map does not expect.
        problems: Every difference from the spec and every unparseable row; empty when the table
            agrees with its map entry. ``wpct.compare`` turns each into FAIL rows.
    """

    index: int
    by_line: str | None
    dependent: str | None
    rows: tuple[ListingRow, ...]
    first_line: int | None
    last_line: int | None
    spec: GlmTableSpec | None
    problems: tuple[str, ...]


def split_parameter(label: str) -> tuple[str, str]:
    """Split a ParameterEstimates label into its canonical ``(EFFECT, LEVEL)`` pair.

    ``"Intercept"`` gives ``("INTERCEPT", "")``; a single word gives that word upper-cased, so
    ``"BASE"`` and ``"base"`` both give ``("BASE", "")`` and ``"studynum"`` gives
    ``("STUDYNUM", "")``; ``"<CLASS> <level>"`` gives the upper-cased CLASS variable and the level
    as printed, so ``"TRTPN     54"`` gives ``("TRTPN", "54")``. Used for the listing rows and the
    ``Parameter`` column of SAS exports, so both sides key on the same pair whatever the case SAS
    printed.

    Raises:
        TypeError: For a non-string label.
        ValueError: For a blank label or one with more than two words. The message gives the
            reason and the word count, never the label, which may hold a misplaced subject
            identifier or measurement.
    """
    if not isinstance(label, str):
        raise TypeError(f"split_parameter expects a string, got {type(label).__name__}")
    words = label.split()
    if len(words) == 1:
        return words[0].upper(), ""
    if len(words) == 2:
        return words[0].upper(), words[1]
    if not words:
        raise ValueError("parameter label is blank, so it names no effect")
    raise ValueError(
        f"parameter label of {len(words)} words cannot be split into an effect and a CLASS level"
    )


_HEADER = re.compile(r"^Parameter\s+Estimate\s+Error\s+t Value\s+Pr > \|t\|$")
_DEPENDENT = re.compile(r"^Dependent Variable: (\S+)")
_BY_LINE = re.compile(r"^[A-Za-z_][A-Za-z0-9_]*=\S+(?: +[A-Za-z_][A-Za-z0-9_]*=\S+)*$")
# The lazy optional second word lets "TRTPN     54" absorb its level: a level has no decimal
# point, so it cannot be the estimate. A CLASS level with a decimal point would be read as the
# estimate; the TRTPN levels of these programs are integers.
_ROW = re.compile(r"^(?P<param>\S+(?: +\S+)??) +(?P<est>-?\d*\.\d+)(?: (?P<b>B))?(?P<rest>.*)$")
_DISPLAYED_NUMBER = re.compile(r"^(?:-?\d*\.\d+|\.)$")
_DISPLAYED_P = re.compile(r"^(?:\d*\.\d+|<\.\d+|\.)$")


@dataclass(frozen=True)
class _ScannedTable:
    """A table as found in the listing, before it is checked against the map."""

    first_line: int
    last_line: int
    by_line: str | None
    dependent: str | None
    rows: tuple[ListingRow, ...]
    problems: tuple[str, ...]


def parse_glm_listing(
    path: str | os.PathLike[str], table_map: Sequence[GlmTableSpec] | None = None
) -> tuple[ListingTable, ...]:
    """Read every ParameterEstimates table of a SAS listing and check it against its map entry.

    A table starts at its column header (``Parameter  Estimate  Error  t Value  Pr > |t|``). Its
    dependent variable is the nearest ``Dependent Variable:`` line above the header, and its BY
    line the nearest ``name=value`` line, both searched back only to the end of the previous table.
    One blank line separates the header from the rows, which end at the first line that is not a
    parameter row: a blank line, a page title or a footnote. Each row keeps the displayed tokens.

    Args:
        path: The SAS listing, e.g. ``paths.listing_file("WPCT-F.07.07")``.
        table_map: The expected tables in file order; by default ``GLM_TABLE_MAP[<program>]`` for
            the program the file is named after (``WPCT-F.07.02.lst`` is 07.02).

    Returns:
        One :class:`ListingTable` per table found, in file order, each carrying its map entry and
        its problems: a BY line, a dependent variable other than ``CHG`` (any case), an
        ``(EFFECT, LEVEL)`` row sequence or a line range that differs from the entry, or a row
        that does not parse. A table beyond the map gets ``"unexpected table"``; each map entry
        beyond the tables found is appended as an empty table with ``"table <i> missing from
        listing"``. Problems never raise, so ``wpct.compare`` reports them as FAIL rows.

    Raises:
        InputDataError: ``"reference listing not found: <absolute path>"`` when the path is not a
            regular file, or ``"cannot read reference listing <absolute path>: <reason>"``.
        ValueError: When ``table_map`` is omitted and the file name names no program, or when the
            map's indices are not 0, 1, 2, ... in order.
    """
    specs = _table_specs(GLM_TABLE_MAP[_listing_program(path)] if table_map is None else table_map)
    lines = _read_reference_lines(path, "reference listing")
    found = _scan_tables(lines)
    tables: list[ListingTable] = []
    for position, scanned in enumerate(found):
        spec = specs[position] if position < len(specs) else None
        problems = list(scanned.problems)
        if spec is None:
            problems.append(
                f"table {position} at line {scanned.first_line}: unexpected table "
                f"(the map expects {len(specs)})"
            )
        else:
            problems.extend(_verify_table(position, scanned, spec))
        tables.append(
            ListingTable(
                index=position,
                by_line=scanned.by_line,
                dependent=scanned.dependent,
                rows=scanned.rows,
                first_line=scanned.first_line,
                last_line=scanned.last_line,
                spec=spec,
                problems=tuple(problems),
            )
        )
    for position in range(len(found), len(specs)):
        tables.append(
            ListingTable(
                index=position,
                by_line=None,
                dependent=None,
                rows=(),
                first_line=None,
                last_line=None,
                spec=specs[position],
                problems=(f"table {position} missing from listing",),
            )
        )
    return tuple(tables)


def _listing_program(path: str | os.PathLike[str]) -> str:
    """Return the program whose listing ``path`` is, from its file name ``<program>.lst``."""
    name = Path(path).name
    for program_id in GLM_TABLE_MAP:
        if name == f"{program_id}.lst":
            return program_id
    expected = ", ".join(f"{program_id}.lst" for program_id in GLM_TABLE_MAP)
    raise ValueError(
        f"cannot infer the program of listing {name!r} (expected {expected}); pass table_map"
    )


def _table_specs(table_map: Sequence[GlmTableSpec]) -> tuple[GlmTableSpec, ...]:
    if isinstance(table_map, (Mapping, str)):
        raise TypeError("table_map must hold one program's tables, e.g. GLM_TABLE_MAP[pid]")
    specs = tuple(table_map)
    for position, spec in enumerate(specs):
        if not isinstance(spec, GlmTableSpec):
            raise TypeError(
                f"table_map[{position}] must be a GlmTableSpec, got {type(spec).__name__}"
            )
        if spec.index != position:
            raise ValueError(
                f"table_map[{position}] has index {spec.index}; indices must be 0, 1, 2, ..."
            )
    return specs


def _scan_tables(lines: tuple[str, ...]) -> tuple[_ScannedTable, ...]:
    tables: list[_ScannedTable] = []
    start = 0  # 0-based index of the first line after the previous table
    index = 0
    while index < len(lines):
        if not _HEADER.match(lines[index]):
            index += 1
            continue
        label = f"table {len(tables)}"
        above = lines[start:index]
        dependent = next(
            (match.group(1) for match in map(_DEPENDENT.match, reversed(above)) if match), None
        )
        by_line = next((text for text in reversed(above) if _BY_LINE.match(text)), None)
        problems: list[str] = []
        rows: list[ListingRow] = []
        last_line = index + 1
        cursor = index + 1
        if cursor < len(lines) and lines[cursor] == "":
            cursor += 1
        else:
            problems.append(f"{label}: line {cursor + 1}: no blank line follows the column header")
        while cursor < len(lines):
            match = _ROW.match(lines[cursor])
            if match is None:
                break
            row, problem = _parse_row(match, cursor + 1)
            if problem is None:
                rows.append(row)
            else:
                problems.append(f"{label}: {problem}")
            last_line = cursor + 1
            cursor += 1
        tables.append(
            _ScannedTable(
                first_line=index + 1,
                last_line=last_line,
                by_line=by_line,
                dependent=dependent,
                rows=tuple(rows),
                problems=tuple(problems),
            )
        )
        start = index = cursor
    return tuple(tables)


def _parse_row(match: re.Match[str], number: int) -> tuple[ListingRow | None, str | None]:
    """Build the row of a line matched by ``_ROW``, or return why its value tokens are invalid."""
    text = match.string
    values = match["rest"].split()
    if len(values) == 3:
        stderr, tvalue, probt = values
    elif not values:
        stderr = tvalue = probt = ""
    else:
        return None, (
            f"line {number}: expected 3 values or none after the estimate, found "
            f"{len(values)}: {text}"
        )
    invalid = [
        token
        for token, pattern in (
            (stderr, _DISPLAYED_NUMBER),
            (tvalue, _DISPLAYED_NUMBER),
            (probt, _DISPLAYED_P),
        )
        if token and not pattern.match(token)
    ]
    if invalid:
        return None, f"line {number}: unrecognised displayed values {' '.join(invalid)}: {text}"
    label = " ".join(match["param"].split())
    effect, level = split_parameter(label)
    row = ListingRow(
        PARAMETER=label,
        EFFECT=effect,
        LEVEL=level,
        ESTIMATE=match["est"],
        BIASED=match["b"] is not None,
        STDERR=stderr,
        TVALUE=tvalue,
        PROBT=probt,
        line=number,
    )
    return row, None


def _render_by_line(by_line: str | None) -> str:
    return "none" if by_line is None else repr(by_line)


def _render_parameters(parameters: Sequence[tuple[str, str]]) -> str:
    rendered = ", ".join(f"{effect} {level}".rstrip() for effect, level in parameters)
    return f"({rendered})"


def _verify_table(position: int, table: _ScannedTable, spec: GlmTableSpec) -> list[str]:
    """Return every difference between a scanned table and its map entry."""
    label = f"table {position}"
    problems = []
    if table.by_line != spec.by_line:
        problems.append(
            f"{label}: BY line {_render_by_line(table.by_line)} differs from the expected "
            f"{_render_by_line(spec.by_line)}"
        )
    if table.dependent is None:
        problems.append(f"{label}: no 'Dependent Variable:' line precedes the table")
    elif table.dependent.upper() != _RESPONSE:
        problems.append(f"{label}: dependent variable {table.dependent} is not {_RESPONSE}")
    parameters = tuple((row.EFFECT, row.LEVEL) for row in table.rows)
    if parameters != spec.parameters:
        problems.append(
            f"{label}: parameter rows {_render_parameters(parameters)} differ from the expected "
            f"{_render_parameters(spec.parameters)}"
        )
    if (table.first_line, table.last_line) != spec.lines:
        problems.append(
            f"{label}: lines {table.first_line}-{table.last_line} differ from the pinned lines "
            f"{spec.lines[0]}-{spec.lines[1]}"
        )
    return problems


# --- Full-precision SAS exports -------------------------------------------------------------------

#: Export column types read as numbers; ``char`` columns stay text and ``bool`` has no SAS source.
_NUMERIC_TYPES = (NUM, INT, NULLABLE_INT)
#: SAS missing numerics as written to CSV: ``.``, ``._`` and the special missing ``.A``-``.Z``.
_SAS_MISSING_NUMBER = re.compile(r"^\.[A-Za-z_]?$")
#: A ``BEST32.`` number. ``float()`` alone would also accept ``nan`` and ``inf``, which SAS never
#: writes and which would turn a malformed value into a silent missing or infinite one. The
#: exponent is unbounded here, so :func:`_sas_numbers` checks the converted range.
_SAS_NUMBER = re.compile(r"^[+-]?(?P<mantissa>\d+\.?\d*|\.\d+)(?:[Ee][+-]?\d+)?$")
#: A digit that makes a mantissa, and so the number it scales, nonzero.
_NONZERO_DIGIT = re.compile(r"[1-9]")


def load_sas_exports(
    # The published keyword is ``dir``, so it shadows the builtin, which this body never calls.
    dir: str | os.PathLike[str],
) -> dict[str, pd.DataFrame | None]:
    """Load the user-supplied full-precision SAS exports found in ``dir``.

    The exports are optional inputs (``python/wpct/reference/sas_exports/README.md``); the run
    never produces them. Each present file is read as Latin-1 CSV, so the pooled identifier's byte
    ``A0`` reads back as ``'\\u00a0Pooled'``. Then:

    - column names are stripped and folded to upper case (SAS names are case-insensitive);
    - trailing blanks are stripped from every value;
    - only the columns numeric in the file's ``schemas.sas_export_spec`` (matched upper-case)
      become float64: ``.``, ``._``, ``.A``-``.Z`` and empty are NaN, and ``BEST32.`` text is
      parsed with ``float``. Every other column stays text, a column the spec lacks included
      even when it is named like a GLM statistic (an ``Estimate`` in ``css_anadata``);
    - a parameter-estimates export gains ``EFFECT`` and ``LEVEL`` from
      :func:`split_parameter` of its ``PARAMETER`` column, inserted before it.

    Columns are not required to be complete: a partial export loads, and ``wpct.compare`` reports
    what it lacks. Nothing is written.

    Returns:
        A dict keyed by every ``schemas.SAS_EXPORT_FILES`` name in that order, holding the frame,
        or ``None`` for an absent file. A directory that does not exist gives all ``None``.

    Raises:
        InputDataError: When ``dir`` exists but is not a directory, or is a broken or looping
            symlink; when ``dir`` or an export cannot be examined (access is denied or its path
            is too long, for example); or when an export: is a broken or looping symlink; is not
            a regular file; cannot be read or parsed as CSV (an unterminated quote or a NUL byte,
            for example); has no header row; has a header column without a name; has two
            columns with the same name ignoring case, spelled identically (``CHG,CHG``) or in
            different case (``CHG,chg``); has a record with more or fewer fields than the
            header; holds a numeric value that is not a SAS number, or one beyond double
            precision (``1E999`` overflowing to infinity, or a nonzero ``1E-999`` underflowing
            to zero); or is a parameter-estimates export without a usable ``Parameter`` column.
            Every message is one line naming the directory or file at fault, and the header
            field, column or data row where there is one, never a value from the file. A
            control character in the path, such as a line break, is shown escaped (``\\n``).
    """
    root = Path(dir).absolute()
    shown_root = message_path(root)
    exports: dict[str, pd.DataFrame | None] = dict.fromkeys(SAS_EXPORT_FILES)
    # ``exists`` follows symlinks and is False for a dangling or looping one, while ``lexists``
    # sees the entry itself: only an entry that is not there at all is an absent export.
    try:
        exists, is_dir = root.exists(), root.is_dir()
    except OSError as exc:
        raise InputDataError(f"cannot read SAS export directory {shown_root}: {exc}") from exc
    if not exists:
        if os.path.lexists(root):
            raise InputDataError(
                f"cannot read SAS export directory {shown_root}: broken or looping symlink"
            )
        return exports
    if not is_dir:
        raise InputDataError(f"SAS export directory is not a directory: {shown_root}")
    for file_name in SAS_EXPORT_FILES:
        path = root / file_name
        shown = message_path(path)
        try:
            exists, is_file = path.exists(), path.is_file()
        except OSError as exc:
            raise InputDataError(f"cannot read SAS export {shown}: {exc}") from exc
        if not exists:
            if os.path.lexists(path):
                raise InputDataError(f"cannot read SAS export {shown}: broken or looping symlink")
            continue
        if not is_file:
            raise InputDataError(f"SAS export is not a regular file: {shown}")
        exports[file_name] = _load_export(path, file_name)
    return exports


def _load_export(path: Path, file_name: str) -> pd.DataFrame:
    header, records = _read_records(path)
    frame = pd.DataFrame(records, columns=header, dtype=object)
    frame.columns = list(upper_columns(frame.columns))
    for column in frame.columns:
        frame[column] = frame[column].str.rstrip(" ")

    spec = sas_export_spec(file_name)
    column_types = dict(
        zip(upper_columns(spec.column_types), spec.column_types.values(), strict=True)
    )
    numeric = {name for name, code in column_types.items() if code in _NUMERIC_TYPES}
    for column in frame.columns:
        if column in numeric:
            frame[column] = _sas_numbers(frame[column], path, column)

    if spec.name == "parameter_estimates":
        frame = _with_effect_level(frame, path)
    return frame


def _read_records(path: Path) -> tuple[list[str], list[list[str]]]:
    """Return the stripped header and the data records of one export, each as wide as the header.

    One ``csv`` parse (comma delimiter, double-quote quoting, ``strict``) both validates the file
    and supplies the frame, so the checks see exactly what is loaded: quoted commas, quoted line
    breaks and doubled quotes read as written, and an explicitly empty field stays ``""``. Blank
    lines are skipped, and only empty physical lines count as blank: they are the only records
    ``csv`` yields with no field. Every other line is a record, a lone ``""`` or a line of blanks
    included, so a short one fails the width check instead of vanishing. The first record is the
    header. Messages name the file and positions or counts, never the contents of a field, which
    may be a subject identifier or a measurement.
    """
    header: list[str] | None = None
    records: list[list[str]] = []
    reader = None
    try:
        with path.open(encoding="latin-1", newline="") as handle:
            reader = csv.reader(handle, strict=True)
            for record in reader:
                if not record:
                    continue
                if header is None:
                    header = [name.strip() for name in record]
                    _check_header(header, path)
                elif len(record) != len(header):
                    raise InputDataError(
                        f"SAS export {message_path(path)}: data row {len(records) + 1}, ending "
                        f"at line {reader.line_num}, has {len(record)} fields but the header has "
                        f"{len(header)}"
                    )
                else:
                    records.append(record)
    except (csv.Error, OSError) as exc:
        # csv and OS messages carry no field contents; folding whitespace keeps them on one line.
        where = f"line {reader.line_num}: " if reader is not None else ""
        reason = " ".join(str(exc).split())
        raise InputDataError(
            f"cannot read SAS export {message_path(path)}: {where}{reason}"
        ) from exc
    if header is None:
        raise InputDataError(f"SAS export {message_path(path)} has no header row")
    return header, records


def _check_header(header: Sequence[str], path: Path) -> None:
    """Refuse a header column without a name, or two column names equal ignoring case.

    A repeated name is shown only when it has SAS-name syntax. Anything else, a data row standing
    where the header should be for example, is identified by its field positions alone.
    """
    unnamed = [position for position, name in enumerate(header, start=1) if not name]
    if unnamed:
        columns = "a header column" if len(unnamed) == 1 else "header columns"
        raise InputDataError(
            f"SAS export {message_path(path)} has {columns} without a name: {_fields(unnamed)}"
        )
    positions: dict[str, list[int]] = {}
    for position, name in enumerate(upper_columns(header), start=1):
        positions.setdefault(name, []).append(position)
    duplicated = [
        f"{name} at {_fields(at)}" if _SAS_NAME.match(name) and len(name) <= 32 else _fields(at)
        for name, at in positions.items()
        if len(at) > 1
    ]
    if duplicated:
        raise InputDataError(
            f"SAS export {message_path(path)} has columns with the same name ignoring case: "
            f"{'; '.join(duplicated)}"
        )


def _fields(positions: Sequence[int]) -> str:
    """Render 1-based header positions: ``field 3`` or ``fields 3, 4``."""
    if len(positions) == 1:
        return f"field {positions[0]}"
    return f"fields {', '.join(str(position) for position in positions)}"


def _sas_numbers(values: pd.Series, path: Path, column: str) -> pd.Series:
    """Parse one exported numeric column into float64, SAS missing values becoming NaN.

    A number must also be a finite double that is zero only when its text is: ``1E999``, which
    ``float`` turns into infinity, and a nonzero ``1E-999``, which it flushes to zero, are
    rejected, since an invented infinity or exact zero would be compared as a SAS value (an exact
    zero passes the GLM zero-reference rule). Zeros such as ``-0`` or ``0E999``, and subnormals
    such as ``5E-324``, load as ``float`` reads them.

    A rejected value is located by column and data row only: the cell may hold a measurement
    with its units or a subject identifier shifted into the column, so it is never shown.
    """
    numbers_out = np.empty(len(values), dtype="float64")
    for position, text in enumerate(values.to_numpy(dtype=object)):
        token = text.strip(" ")
        if is_char_missing(token) or _SAS_MISSING_NUMBER.match(token):
            numbers_out[position] = math.nan
            continue
        match = _SAS_NUMBER.match(token)
        number = math.nan if match is None else float(token)
        if match is None:
            reason = "the value is not a SAS numeric value"
        elif not math.isfinite(number):
            reason = "the value overflows double precision"
        elif number == 0.0 and _NONZERO_DIGIT.search(match["mantissa"]):
            reason = "the nonzero value underflows to zero in double precision"
        else:
            numbers_out[position] = number
            continue
        raise InputDataError(
            f"SAS export {message_path(path)}: column {column}, data row {position + 1}: {reason}"
        )
    return pd.Series(numbers_out, index=values.index, name=column)


def _with_effect_level(frame: pd.DataFrame, path: Path) -> pd.DataFrame:
    """Insert ``EFFECT`` and ``LEVEL``, derived from ``PARAMETER``, before ``PARAMETER``."""
    if "PARAMETER" not in frame.columns:
        raise InputDataError(
            f"SAS export {message_path(path)} lacks the Parameter column that EFFECT and LEVEL "
            "derive from"
        )
    pairs = []
    for position, label in enumerate(frame["PARAMETER"]):
        try:
            pairs.append(split_parameter(label))
        except ValueError as exc:  # split_parameter's message never quotes the label.
            raise InputDataError(
                f"SAS export {message_path(path)}: column PARAMETER, data row {position + 1}: "
                f"{exc}"
            ) from exc
    frame = frame.drop(columns=[name for name in ("EFFECT", "LEVEL") if name in frame.columns])
    at = frame.columns.get_loc("PARAMETER")
    frame.insert(at, "EFFECT", [effect for effect, _ in pairs])
    frame.insert(at + 1, "LEVEL", [level for _, level in pairs])
    return frame


# --- Map consistency ------------------------------------------------------------------------------


def _check_maps() -> None:
    """Keep the pinned maps self-consistent (fails at import on drift).

    Within each program the log entries ascend by line without overlapping, and each name's
    OCCURRENCE counts 1, 2, ... in map order, as the drivers number their checkpoints. The listing
    tables are indexed 0, 1, ... and their line ranges ascend without overlapping.
    """
    for program_id, specs in LOG_CHECKPOINT_MAP.items():
        seen: dict[str, int] = {}
        previous = 0
        for spec in specs:
            expected = seen[spec.SAS_DATASET] = seen.get(spec.SAS_DATASET, 0) + 1
            if expected != spec.OCCURRENCE:
                raise ValueError(
                    f"{program_id}: {spec.SAS_DATASET} at log line {spec.line} has OCCURRENCE "
                    f"{spec.OCCURRENCE}, expected {expected}"
                )
            if spec.line <= previous:
                raise ValueError(f"{program_id}: log line {spec.line} is out of order")
            previous = spec.line if spec.end_line is None else spec.end_line
    for program_id, tables in GLM_TABLE_MAP.items():
        _table_specs(tables)
        for before, after in itertools.pairwise(tables):
            if after.lines[0] <= before.lines[1]:
                raise ValueError(f"{program_id}: listing table {after.index} overlaps the previous")


_check_maps()
