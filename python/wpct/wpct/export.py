"""CSV export of the intermediate datasets of one program run.

This module and ``wpct.report`` are the package's only writers. Under the data reach and write
policy (Agent Action Plan 0.8), :func:`write_program_outputs` writes only to the fixed files below
``python/wpct/outputs/<program_id>/`` under ``paths.write_root``. It takes no destination argument,
derives every target from :meth:`wpct.paths.RepoPaths.program_outputs_dir`, and passes every target
of a call through :func:`wpct.paths.vet_fixed_destination` before it creates a directory or opens
a file. One rejection therefore leaves every target of the call unopened and unchanged. Each file
is then opened through :func:`wpct.paths.open_fixed_destination`, which refuses a destination
replaced since that check before truncating it.

The exports (Agent Action Plan 0.6.4) let the port's datasets be joined record by record with
their SAS counterparts. Their shapes come only from ``wpct.schemas.EXPORTS``; the spec items are
those of ``docs/sas_specification.md``:

- ``css_anadata.csv``: the analysis records, i.e. the S02-TRN-03 and S07-TRN-04 keep lists. In 07.07
  they include the pooled copy (S07-TRN-05) and ``studynum`` (S07-TRN-06).
- ``advs_min.csv`` (07.07 only): the derived MIN records (LMM-TRN-07) after the S07-FLT-02
  post-baseline filter.
- ``css_stats.csv``: the PROC SUMMARY statistics (S02-MOD-01, S07-MOD-01) with the merged ``pval``
  (S02-TRN-05, S07-TRN-08). ``pval`` is absent when the GLM is skipped by setting.
- ``parameter_estimates.csv``: the GLM ``SOLUTION`` rows (S02-MOD-02, S07-MOD-02, S07-MOD-03).
- ``models.csv``: one row per fit (``ProgramResult.models``).
- ``checkpoints.csv``: the log-equivalent record counts (``ProgramResult.checkpoints``).

CSV format: UTF-8 with a header row of the SAS variable names in ``spec.columns`` order. Fields are
separated by commas and lines end with ``\\n``. A field is quoted only when it holds a comma, a
double quote, a carriage return or a line feed, and each double quote inside it is doubled; a
record of one empty field is written ``""``. Floats are written with ``%.17g``, which round-trips
exactly, and integers as integers. Booleans are written as ``True`` or ``False``, and a missing
value of any type is an empty field. A character value holding a NUL character (U+0000), which CSV
readers do not return intact, or text that UTF-8 cannot encode, such as a lone surrogate, raises
``ValueError`` before anything is written. Rows are sorted by the record key with ``sas_sort``,
which is stable and puts missing values first. An empty dataset still gets its header row, and
identical inputs give identical bytes. :func:`read_exported_csv` reads a file back to the
in-memory dtypes.
"""

from __future__ import annotations

import dataclasses
import math
import numbers
import os
import re
from collections.abc import Sequence
from pathlib import Path

import numpy as np
import pandas as pd

from wpct.paths import RepoPaths, open_fixed_destination, vet_fixed_destination
from wpct.programs import Checkpoint, ModelRecord, ProgramResult
from wpct.sas_semantics import is_num_missing, sas_sort
from wpct.schemas import BOOL, CHAR, EXPORTS, INT, NULLABLE_INT, NUM, ExportSpec

__all__ = ["frame_for_export", "output_targets", "read_exported_csv", "write_program_outputs"]

#: Exports built from ``ProgramResult`` record tuples rather than from ``ProgramResult.datasets``:
#: export name -> (result attribute, record type).
_RECORD_EXPORTS = {
    "checkpoints": ("checkpoints", Checkpoint),
    "models": ("models", ModelRecord),
}

_TRUE = "True"
_FALSE = "False"

_ENCODING = "utf-8"

#: Characters that make a CSV field quoted: the delimiter, the quote character and both line-break
#: characters, so that a reader ends a record only at a line end written between records.
_QUOTED = re.compile('[,"\r\n]')

#: The NUL character (U+0000), refused in character values because CSV readers do not return it
#: intact.
_NUL = "\x00"


def _label(spec: ExportSpec) -> str:
    return f"{spec.program_id}/{spec.name}"


def _program_specs(program_id: str) -> tuple[ExportSpec, ...]:
    """Return the export specs of ``program_id``, or raise ``KeyError`` naming the known programs."""
    try:
        return EXPORTS[program_id]
    except KeyError:
        known = ", ".join(EXPORTS)
        raise KeyError(f"no exports defined for program {program_id!r}; expected one of: {known}") from None


# --- Frame assembly ------------------------------------------------------------------------------


def frame_for_export(result: ProgramResult, spec: ExportSpec) -> pd.DataFrame:
    """Return the frame exported for ``spec`` from ``result``, in its exported form.

    ``checkpoints`` is built from ``result.checkpoints`` and ``models`` from ``result.models``. A
    record field the spec has no column for, such as ``ModelRecord.STUDYNUM`` in 07.02, must be
    ``None`` on every record. Every other export is ``result.datasets[spec.name]``.

    The columns are those of ``spec.columns`` that the frame holds, in spec order. Only
    ``spec.optional_columns`` may be absent; this is the ``css_stats`` ``pval`` column when the GLM
    is skipped. A missing required column, an extra column or a duplicated column raises
    ``ValueError`` naming it, so a schema drift fails before anything is written.

    Each column is converted to its ``spec.column_types`` dtype:

    - ``num`` becomes float64 with NaN for missing values.
    - ``int`` becomes int64, and a missing or non-integral value raises ``ValueError``.
    - ``Int64`` becomes nullable Int64 with ``pd.NA`` for missing values.
    - ``bool`` becomes bool.
    - ``char`` becomes object with ``''`` for missing values.

    A value of the wrong kind raises ``TypeError``: a string or boolean in a numeric column, a
    missing or non-boolean value in a boolean column, or a non-string value in a character column.

    The rows are then sorted by ``spec.key`` with ``sas_sort`` (D-04, D-14), and the result has a
    fresh ``RangeIndex``. ``result`` and its frames are never modified.
    """
    if spec.program_id != result.program_id:
        raise ValueError(
            f"export spec {_label(spec)} does not belong to program {result.program_id!r}"
        )
    if spec.name in _RECORD_EXPORTS:
        attribute, record_type = _RECORD_EXPORTS[spec.name]
        source = _records_frame(getattr(result, attribute), record_type, spec)
    else:
        source = result.dataset(spec.name)
    columns = _export_columns(source, spec)
    label = _label(spec)
    data = {
        column: _coerce(source[column], spec.column_types[column], f"{label} column {column}")
        for column in columns
    }
    return sas_sort(pd.DataFrame(data, columns=list(columns)), spec.key)


def _records_frame(records: tuple, record_type: type, spec: ExportSpec) -> pd.DataFrame:
    """Return the records as an object-typed frame holding the spec's columns."""
    label = _label(spec)
    field_names = tuple(item.name for item in dataclasses.fields(record_type))
    unknown = [column for column in spec.columns if column not in field_names]
    if unknown:
        raise ValueError(
            f"{label}: columns that are not {record_type.__name__} fields: {' '.join(unknown)}"
        )
    for name in field_names:
        if name in spec.columns:
            continue
        filled = sum(getattr(record, name) is not None for record in records)
        if filled:
            raise ValueError(
                f"{label}: {record_type.__name__}.{name} is set on {filled} record(s), "
                f"but the export has no {name} column"
            )
    return pd.DataFrame(
        {
            column: pd.Series([getattr(record, column) for record in records], dtype=object)
            for column in spec.columns
        },
        columns=list(spec.columns),
    )


def _export_columns(frame: pd.DataFrame, spec: ExportSpec) -> tuple[str, ...]:
    """Return the spec columns ``frame`` holds, in spec order, after checking for schema drift."""
    present = frame.columns
    problems = []
    duplicated = present[present.duplicated()].unique()
    if len(duplicated):
        problems.append(f"duplicated columns: {' '.join(map(str, duplicated))}")
    missing = [
        column
        for column in spec.columns
        if column not in present and column not in spec.optional_columns
    ]
    if missing:
        problems.append(f"missing columns: {' '.join(missing)}")
    extra = [column for column in present.unique() if column not in spec.columns]
    if extra:
        problems.append(f"columns not in the schema: {' '.join(map(str, extra))}")
    if problems:
        raise ValueError(f"{_label(spec)}: {'; '.join(problems)}")
    return tuple(column for column in spec.columns if column in present)


def _coerce(column: pd.Series, code: str, label: str) -> np.ndarray | pd.api.extensions.ExtensionArray:
    """Return ``column`` converted to the dtype of type code ``code``, without its index."""
    if code == NUM:
        return _as_float(column, label)
    if code == INT:
        return np.array(_integers(column, label, nullable=False), dtype="int64")
    if code == NULLABLE_INT:
        return pd.array(_integers(column, label, nullable=True), dtype="Int64")
    if code == BOOL:
        return _as_bool(column, label)
    if code == CHAR:
        return _as_char(column, label)
    raise ValueError(f"{label}: unknown column type {code!r}")


def _as_float(column: pd.Series, label: str) -> np.ndarray:
    if pd.api.types.is_bool_dtype(column.dtype):
        raise TypeError(f"{label}: expected numbers, got booleans")
    if pd.api.types.is_numeric_dtype(column.dtype):
        return column.to_numpy(dtype="float64", na_value=np.nan)
    values = column.to_numpy(dtype=object)
    result = np.empty(len(values), dtype="float64")
    for position, value in enumerate(values):
        if is_num_missing(value):
            result[position] = np.nan
        elif isinstance(value, (bool, np.bool_)) or not isinstance(value, numbers.Real):
            raise TypeError(f"{label}: non-numeric value {value!r} in a numeric column")
        else:
            result[position] = float(value)
    return result


def _integers(column: pd.Series, label: str, *, nullable: bool) -> list[int | None]:
    if pd.api.types.is_bool_dtype(column.dtype):
        raise TypeError(f"{label}: expected integers, got booleans")
    result: list[int | None] = []
    missing = 0
    for value in column.to_numpy(dtype=object):
        if is_num_missing(value):
            missing += 1
            result.append(None)
        elif isinstance(value, (bool, np.bool_)) or not isinstance(value, numbers.Real):
            raise TypeError(f"{label}: non-integer value {value!r} in an integer column")
        elif isinstance(value, numbers.Integral) or (
            math.isfinite(float(value)) and float(value).is_integer()
        ):
            result.append(int(value))
        else:
            raise ValueError(f"{label}: non-integral value {value!r} in an integer column")
    if missing and not nullable:
        raise ValueError(f"{label}: {missing} missing value(s) in a non-nullable integer column")
    return result


def _as_bool(column: pd.Series, label: str) -> np.ndarray:
    values = column.to_numpy(dtype=object)
    result = np.empty(len(values), dtype=bool)
    for position, value in enumerate(values):
        if not isinstance(value, (bool, np.bool_)):
            raise TypeError(f"{label}: non-boolean value {value!r} in a boolean column")
        result[position] = bool(value)
    return result


def _as_char(column: pd.Series, label: str) -> np.ndarray:
    values = column.to_numpy(dtype=object)
    result = np.empty(len(values), dtype=object)
    for position, value in enumerate(values):
        if isinstance(value, str):
            result[position] = str(value)
        elif is_num_missing(value):
            result[position] = ""
        else:
            raise TypeError(f"{label}: non-character value {value!r} in a character column")
    return result


# --- CSV rendering and reading -------------------------------------------------------------------


def _format_column(column: pd.Series, code: str, label: str) -> list[str]:
    """Return the CSV fields of a column already converted by :func:`_coerce`.

    ``label`` names the export and the column in the error :func:`_char_fields` raises.
    """
    if code == NUM:
        # ".17g" is the "%.17g" of the export contract: 17 significant digits round-trip a double.
        return ["" if math.isnan(value) else f"{value:.17g}" for value in column.to_numpy()]
    if code == INT:
        return [str(int(value)) for value in column.to_numpy()]
    if code == NULLABLE_INT:
        return ["" if value is pd.NA else str(int(value)) for value in column.array]
    if code == BOOL:
        return [_TRUE if value else _FALSE for value in column.to_numpy()]
    if code == CHAR:
        return _char_fields(column, label)
    raise ValueError(f"unknown column type {code!r}")


def _char_fields(column: pd.Series, label: str) -> list[str]:
    """Return the values of a character column after checking that the CSV file can carry them.

    A NUL character (U+0000) raises ``ValueError``, because CSV readers do not return it intact,
    and so does text that UTF-8 cannot encode, such as a lone surrogate. The message names
    ``label`` and the 1-based data row but not the value; an encoding error is chained to it.
    """
    fields = list(column.to_numpy())
    for row, text in enumerate(fields, start=1):
        if _NUL in text:
            raise ValueError(
                f"{label}, data row {row}: a NUL character (U+0000) cannot be exported"
            )
        try:
            text.encode(_ENCODING)
        except UnicodeEncodeError as exc:
            raise ValueError(
                f"{label}, data row {row}: text that cannot be encoded as UTF-8 ({exc.reason})"
            ) from exc
    return fields


def _csv_field(text: str) -> str:
    """Return ``text`` as one CSV field, quoted only when it holds a :data:`_QUOTED` character."""
    if _QUOTED.search(text) is None:
        return text
    return '"' + text.replace('"', '""') + '"'


def _csv_line(fields: Sequence[str]) -> str:
    """Return one CSV record, ending with ``\\n``, of the text ``fields``."""
    if len(fields) == 1 and not fields[0]:
        # Unquoted, a record of one empty field would be a blank line, which readers skip.
        return '""\n'
    return ",".join(map(_csv_field, fields)) + "\n"


def _render_csv(frame: pd.DataFrame, spec: ExportSpec) -> bytes:
    """Return the UTF-8 bytes of the CSV of a frame built by :func:`frame_for_export` for ``spec``.

    The quoting rule of the module docstring is applied here rather than by the ``csv`` module,
    whose writer leaves a carriage return unquoted under a ``\\n`` line end and whose handling of
    NUL characters differs between Python versions. A character value the file cannot carry raises
    ``ValueError`` (:func:`_char_fields`). Every other field is a number, a boolean or one of the
    schema's column names, so encoding the finished text cannot fail.
    """
    label = _label(spec)
    columns = list(frame.columns)
    fields = [
        _format_column(frame[column], spec.column_types[column], f"{label} column {column}")
        for column in columns
    ]
    lines = [_csv_line(columns), *(_csv_line(row) for row in zip(*fields))]
    return "".join(lines).encode(_ENCODING)


def read_exported_csv(path: str | os.PathLike[str], spec: ExportSpec) -> pd.DataFrame:
    """Read a CSV written by :func:`write_program_outputs` back to the dtypes of :func:`frame_for_export`.

    Every field is read as text, and an empty field is not treated as NA at this stage. The header
    must be ``spec.columns`` in order, with only ``spec.optional_columns`` allowed to be absent.
    Each column is then parsed by its ``spec.column_types`` code:

    - ``num`` becomes float64, with an empty field read as NaN.
    - ``int`` becomes int64.
    - ``Int64`` becomes nullable Int64, with an empty field read as ``pd.NA``.
    - ``bool`` becomes bool, read from ``True`` or ``False``.
    - ``char`` stays text, so an empty field stays ``''``.

    The result has a ``RangeIndex`` and keeps the file's row order. A value that does not parse
    under its column type raises ``ValueError`` naming the file and the column.
    """
    raw = pd.read_csv(path, encoding="utf-8", dtype=str, keep_default_na=False)
    label = f"{path}"
    header = tuple(raw.columns)
    expected = tuple(
        column
        for column in spec.columns
        if column in header or column not in spec.optional_columns
    )
    if header != expected:
        raise ValueError(
            f"{label}: header {' '.join(map(str, header))} does not match the "
            f"{_label(spec)} columns {' '.join(expected)}"
        )
    data = {
        column: _parse_column(
            raw[column].to_numpy(dtype=object), spec.column_types[column], f"{label} column {column}"
        )
        for column in header
    }
    return pd.DataFrame(data, columns=list(header))


def _parse_column(
    fields: np.ndarray, code: str, label: str
) -> np.ndarray | pd.api.extensions.ExtensionArray:
    """Return the text ``fields`` of one CSV column parsed under type code ``code``."""
    try:
        if code == NUM:
            return np.array(
                [np.nan if field == "" else float(field) for field in fields], dtype="float64"
            )
        if code == INT:
            return np.array([int(field) for field in fields], dtype="int64")
        if code == NULLABLE_INT:
            return pd.array([None if field == "" else int(field) for field in fields], dtype="Int64")
    except ValueError as exc:
        raise ValueError(f"{label}: {exc}") from exc
    if code == BOOL:
        invalid = sorted({field for field in fields if field not in (_TRUE, _FALSE)})
        if invalid:
            raise ValueError(f"{label}: values other than {_TRUE}/{_FALSE}: {invalid}")
        return np.array([field == _TRUE for field in fields], dtype=bool)
    if code == CHAR:
        result = np.empty(len(fields), dtype=object)
        result[:] = [str(field) for field in fields]
        return result
    raise ValueError(f"{label}: unknown column type {code!r}")


# --- Guarded writer ------------------------------------------------------------------------------


def output_targets(result: ProgramResult, paths: RepoPaths) -> dict[str, Path]:
    """Return the fixed export file of every spec of ``result.program_id``, in export order.

    Each target is ``paths.program_outputs_dir(result.program_id) / spec.file_name``, joined and
    not resolved. :func:`write_program_outputs` vets each one before writing.
    """
    directory = paths.program_outputs_dir(result.program_id)
    return {spec.name: directory / spec.file_name for spec in _program_specs(result.program_id)}


def write_program_outputs(result: ProgramResult, paths: RepoPaths) -> tuple[Path, ...]:
    """Write every export of ``result`` and return the written paths, resolved, in export order.

    A halted run (S07-FLT-04, S07-FLT-05) writes nothing and returns ``()``, because a halting
    check stops the program before any export.

    Every frame is built and every CSV rendered and encoded as UTF-8 in memory first, so a schema
    drift (``ValueError`` or ``TypeError``) writes nothing, and neither does a character value the
    file cannot carry: a NUL character or text that UTF-8 cannot encode raises ``ValueError``
    naming the export, the column and the data row. Each target then goes through
    :func:`~wpct.paths.vet_fixed_destination` (Agent Action Plan 0.8): it must pass
    ``check_write_destination``, and its resolved path must equal its fixed destination under the
    resolved write root, so a target reached through a symlinked file or directory below the write
    root is refused even when the link stays inside the outputs tree. Symlinks at or above the
    resolved write root are followed. An existing target must also be a regular file with a single
    hard link, and every existing entry on its way a directory. Any refusal raises
    :class:`~wpct.errors.OutputPathError` before a directory is created or a file is opened. Only
    after every target passes are the directories created and the files written, at the paths the
    check accepted, each through :func:`~wpct.paths.open_fixed_destination`. That opens it through
    directory descriptors without following a symlink (or by path where the platform has no such
    opens, then requiring the opened file's own path to be the target) and raises
    ``OutputPathError`` before truncating it when it has become a symlink, a hard link or a special
    file since the check, or was opened elsewhere; the files that earlier targets of the call wrote
    then stay.
    """
    if result.halted:
        return ()
    specs = _program_specs(result.program_id)
    targets = output_targets(result, paths)
    payloads = {spec.name: _render_csv(frame_for_export(result, spec), spec) for spec in specs}
    destinations = {spec.name: vet_fixed_destination(targets[spec.name], paths) for spec in specs}
    for spec in specs:
        with open_fixed_destination(targets[spec.name], paths) as handle:
            handle.write(payloads[spec.name])
    return tuple(destinations[spec.name] for spec in specs)
