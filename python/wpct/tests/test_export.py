"""Tests of the CSV export of the intermediate datasets and of the write-destination guard.

Units under test: ``wpct.export`` (``output_targets``, ``frame_for_export``,
``write_program_outputs`` and ``read_exported_csv``) and ``wpct.paths.check_write_destination``,
with the ``vet_fixed_destination`` and ``open_fixed_destination`` the writer calls.

Export contract (Agent Action Plan 0.6.4): one CSV per ``schemas.EXPORTS`` spec in
``python/wpct/outputs/<program>/``, encoded as UTF-8. The header row holds the SAS variable names in
``spec.columns`` order, numbers are written with ``%.17g``, a missing value is an empty field, and
rows are sorted by the record key (D-04, D-14). A field holding a comma, a double quote, a carriage
return or a line feed is quoted, and a character value with a NUL character or text that UTF-8
cannot encode raises ``ValueError`` before anything is written. The record-level files carry the
S02-TRN-03 and S07-TRN-04 keep lists, the LMM-TRN-07 derived records, and the pooled copy with
``studynum`` (S07-TRN-05, S07-TRN-06), whose identifier ``'\\u00a0Pooled'`` must survive the
encoding (D-16).

Write policy (Agent Action Plan 0.8, "Guard" and "Boundary tests"): every write in this module goes
through the ``write_paths`` fixture, whose write root lies under pytest's ``tmp_path``. The
checkout-rooted ``repo_paths`` reaches only ``check_write_destination``, which writes nothing, so no
test here writes into the checkout. The guard applies one rule to every write root, so the symlink
cases on a ``tmp_path`` write root also cover the checkout's fixed destinations, and so do the
hard-link, special-file and replaced-destination cases.
"""

from __future__ import annotations

import csv
import dataclasses
import os
from collections.abc import Callable, Mapping
from pathlib import Path
from types import SimpleNamespace

import pandas as pd
import pytest

import wpct.paths as wpct_paths
from wpct import export
from wpct.errors import AnalysisHalted, OutputPathError
from wpct.paths import RepoPaths, check_write_destination
from wpct.programs import Checkpoint, ProgramResult
from wpct.schemas import (
    EXPORTS,
    NULLABLE_INT,
    NUM,
    POOLED_STUDYID,
    PROGRAM_07_02,
    PROGRAM_07_07,
    PROGRAMS,
    export_spec,
)

#: One case per exported dataset of each program, in program order and then export order.
_SPEC_CASES = [
    pytest.param(program_id, spec.name, id=f"{program_id}/{spec.name}")
    for program_id in PROGRAMS
    for spec in EXPORTS[program_id]
]

#: Exports of the default runs that hold the pooled study rows (S07-TRN-05, D-16).
_POOLED_EXPORTS = {(PROGRAM_07_07, "css_anadata"), (PROGRAM_07_07, "css_stats")}

#: Row counts of every export of the default runs on the clean checkout:
#:
#: - ``css_anadata`` and ``advs_min``: the AAP 0.6.4 sizes of ``WORK.CSS_ANADATA`` (07.02: 3548,
#:   07.07: 996) and ``WORK.ADVS_MIN`` (498) [whitepapers/WPCT/outputs_sas/WPCT-F.07.02.log:202;
#:   whitepapers/WPCT/outputs_sas/WPCT-F.07.07.log:229,427].
#: - ``css_stats``: two strata of ``WORK.CSS_STATS``, with 30 rows each in 07.02 and 9 in 07.07
#:   [whitepapers/WPCT/outputs_sas/WPCT-F.07.02.log:1049,1471;
#:   whitepapers/WPCT/outputs_sas/WPCT-F.07.07.log:1215,1581].
#: - ``parameter_estimates``: the listing rows, 2 tables of 5 in 07.02, and in 07.07 4 per-study
#:   tables of 5 plus 2 pooled tables of 6 [whitepapers/WPCT/outputs_sas/WPCT-F.07.02.lst:8-29;
#:   whitepapers/WPCT/outputs_sas/WPCT-F.07.07.lst:3-147].
#: - ``models``: one row per fit. ``checkpoints``: the 15 and 29 pinned log lines (AAP 0.7.3).
_EXPECTED_ROWS: Mapping[str, Mapping[str, int]] = {
    PROGRAM_07_02: {
        "css_anadata": 3548,
        "css_stats": 60,
        "parameter_estimates": 10,
        "models": 2,
        "checkpoints": 15,
    },
    PROGRAM_07_07: {
        "advs_min": 498,
        "css_anadata": 996,
        "css_stats": 18,
        "parameter_estimates": 32,
        "models": 6,
        "checkpoints": 29,
    },
}

#: The ``file_state`` fixture: ``(bytes, st_mtime_ns)`` of a file, or a directory's sorted listing.
_FileState = Callable[[str | Path], object]

#: Stand-in for an external SAS-export CSV that a symlinked destination could overwrite.
_SAS_EXPORT_STANDIN = b"STUDYID,USUBJID\nCDISCPILOT01,01-701-1015\n"


def _tree(root: Path) -> dict[str, str]:
    """Return every entry below ``root`` as ``relative path -> "dir" | "file" | "symlink"``.

    Symlinks are listed, never followed, so a write through a planted link shows up as a change at
    its target and not as a new entry here.
    """
    entries: dict[str, str] = {}
    for directory, dirnames, filenames in os.walk(root):
        for name in (*dirnames, *filenames):
            path = Path(directory, name)
            if path.is_symlink():
                kind = "symlink"
            elif path.is_dir():
                kind = "dir"
            else:
                kind = "file"
            entries[path.relative_to(root).as_posix()] = kind
    return entries


def _symlink(link: Path, target: Path) -> None:
    """Create ``link`` pointing at ``target``, or skip where the platform refuses symlinks."""
    try:
        os.symlink(target, link, target_is_directory=target.is_dir())
    except OSError as exc:
        pytest.skip(f"symlinks not supported on this platform: {exc}")


def _header(path: Path) -> list[str]:
    """Return the header row of the CSV at ``path``."""
    with open(path, encoding="utf-8", newline="") as handle:
        return next(csv.reader(handle))


def _written_by_name(result: ProgramResult, paths: RepoPaths) -> dict[str, Path]:
    """Write every export of ``result`` under ``paths`` and map each export name to its file."""
    names = [spec.name for spec in EXPORTS[result.program_id]]
    written = export.write_program_outputs(result, paths)
    assert len(written) == len(names)
    return dict(zip(names, written, strict=True))


def _with_param(result: ProgramResult, value: str) -> tuple[ProgramResult, int]:
    """Return ``result`` with ``value`` as one ``css_anadata`` ``PARAM``, and that value's data row.

    The value replaces the ``PARAM`` of the frame's first row in a copy, so the session result and
    its frames stay unchanged. The data row is the 1-based position of the value in the exported
    frame, which is sorted by the record key.
    """
    frame = result.dataset("css_anadata").copy()
    frame.loc[frame.index[0], "PARAM"] = value
    changed = dataclasses.replace(result, datasets={**result.datasets, "css_anadata": frame})
    exported = export.frame_for_export(changed, export_spec(result.program_id, "css_anadata"))
    rows = exported.index[exported["PARAM"] == value].tolist()
    assert len(rows) == 1
    return changed, rows[0] + 1


# --- Round trip, keys and halted runs (AAP 0.6.4) -------------------------------------------------


@pytest.mark.parametrize(("program_id", "name"), _SPEC_CASES)
def test_d16_round_trip_exact(
    program_id: str,
    name: str,
    results: Mapping[str, ProgramResult],
    write_paths: RepoPaths,
) -> None:
    """Each written CSV reads back to exactly the in-memory export frame (AAP 0.6.4).

    The writer returns the resolved fixed targets of ``output_targets`` in export order, all in the
    program's outputs directory under the ``tmp_path`` write root (AAP 0.8). The header is
    ``spec.columns``. Every missing number is an empty field rather than text such as ``nan``. The
    pooled ``'\\u00a0Pooled'`` identifier is stored as UTF-8 (D-16). ``check_exact`` holds every
    float to its ``%.17g`` round trip and every dtype to its spec type.
    """
    result = results[program_id]
    spec = export_spec(program_id, name)
    targets = export.output_targets(result, write_paths)
    written = _written_by_name(result, write_paths)

    outputs_dir = write_paths.program_outputs_dir(program_id).resolve()
    assert tuple(written.values()) == tuple(target.resolve() for target in targets.values())
    assert all(path.parent == outputs_dir for path in written.values())

    path = written[name]
    assert path.name == spec.file_name
    expected = export.frame_for_export(result, spec)
    assert _header(path) == list(spec.columns) == list(expected.columns)

    pd.testing.assert_frame_equal(export.read_exported_csv(path, spec), expected, check_exact=True)

    raw = pd.read_csv(path, encoding="utf-8", dtype=str, keep_default_na=False)
    for column in expected.columns:
        if spec.column_types[column] in (NUM, NULLABLE_INT):
            assert (raw[column] == "").sum() == expected[column].isna().sum(), (
                f"{program_id}/{name} column {column}: missing values are not empty fields"
            )

    if (program_id, name) in _POOLED_EXPORTS:
        assert (expected["STUDYID"] == POOLED_STUDYID).any()
        assert POOLED_STUDYID.encode("utf-8") in path.read_bytes()


@pytest.mark.parametrize("program_id", PROGRAMS)
def test_d04_14_export_keys_unique(
    program_id: str,
    results: Mapping[str, ProgramResult],
    write_paths: RepoPaths,
) -> None:
    """Every export file is unique on its record key and sorted by it (AAP 0.6.4, D-04, D-14).

    ``ExportSpec`` refuses an empty key, so every spec is checked. The sort check re-sorts the
    file's rows by key, ascending with missing values first, and requires the order to be
    unchanged. The row counts are those of the clean checkout. These are invariants of the Python
    exports only: agreement of a key set with its SAS dataset is established only by the
    record-level comparisons against supplied SAS exports in ``tests/test_sas_exports.py``
    (AAP 0.7.2), not here.
    """
    assert {spec.name for spec in EXPORTS[program_id]} == set(_EXPECTED_ROWS[program_id])
    written = _written_by_name(results[program_id], write_paths)

    for spec in EXPORTS[program_id]:
        label = f"{program_id}/{spec.name}"
        frame = export.read_exported_csv(written[spec.name], spec)
        key = list(spec.key)

        assert len(frame) == _EXPECTED_ROWS[program_id][spec.name], label
        duplicated = frame.loc[frame.duplicated(key, keep=False), key]
        assert duplicated.empty, f"{label}: duplicated record keys:\n{duplicated}"
        resorted = frame.sort_values(key, kind="stable", na_position="first", ignore_index=True)
        pd.testing.assert_frame_equal(frame, resorted, check_exact=True, obj=f"{label} key order")


@pytest.mark.parametrize(
    ("check", "message"),
    [
        pytest.param(
            "assert_var_nonmissing",
            f"{PROGRAM_07_07}: assert_var_nonmissing failed: 3 missing CHG in CSS_SAFANA",
            id="S07-FLT-04",
        ),
        pytest.param(
            "assert_unique_keys",
            f"{PROGRAM_07_07}: assert_unique_keys failed: unexpected duplicates in CSS_ANADATA "
            "with unique keys STUDYID USUBJID PARAMCD ATPTN (SQLOBS = 1)",
            id="S07-FLT-05",
        ),
    ],
)
def test_s07_flt_04_05_halted_result_writes_nothing(
    check: str, message: str, write_paths: RepoPaths
) -> None:
    """A run stopped by a halting check (S07-FLT-04, S07-FLT-05) exports nothing (AAP 0.6.4).

    The halted result keeps the checkpoints recorded before the halt, yet the writer returns ``()``
    and the write root stays empty: no directory and no file is created.
    """
    halt = AnalysisHalted(
        message,
        program_id=PROGRAM_07_07,
        check=check,
        checkpoints=(
            Checkpoint("ADVS", 1, NOBS=32139, NVARS=44),
            Checkpoint("CSS_SAFANA", 1, NOBS=498, NVARS=12),
        ),
    )
    result = ProgramResult.from_halt(halt)
    assert result.halted
    assert len(result.checkpoints) == 2

    assert export.write_program_outputs(result, write_paths) == ()
    assert _tree(write_paths.write_root) == {}


# --- Character values (AAP 0.6.4) -----------------------------------------------------------------


@pytest.mark.parametrize(
    "value",
    [
        pytest.param("cr\rret", id="inner_cr"),
        pytest.param("\r", id="lone_cr"),
        pytest.param("abc\r", id="trailing_cr"),
        pytest.param('say "hi",\r\nthen\rbye', id="crlf_comma_quote"),
    ],
)
def test_s02_trn_03_round_trip_carriage_return(
    value: str, result_07_02: ProgramResult, write_paths: RepoPaths
) -> None:
    """A character value holding a carriage return is quoted and reads back exactly (AAP 0.6.4).

    ``value`` is one ``PARAM`` of a copy of the WPCT-F.07.02 ``css_anadata`` frame. ``PARAM`` is one
    of the S02-TRN-03 analysis variables that frame keeps. The value's field must be quoted, with
    each double quote doubled, so ``csv.reader`` finds one record per data row plus the header and
    returns the value intact, and ``read_exported_csv`` returns the in-memory export frame exactly.
    """
    result, row = _with_param(result_07_02, value)
    spec = export_spec(PROGRAM_07_02, "css_anadata")
    expected = export.frame_for_export(result, spec)
    path = _written_by_name(result, write_paths)["css_anadata"]

    quoted = '"' + value.replace('"', '""') + '"'
    assert f",{quoted},".encode() in path.read_bytes()
    with open(path, encoding="utf-8", newline="") as handle:
        records = list(csv.reader(handle))
    assert len(records) == len(expected) + 1
    assert records[row][spec.columns.index("PARAM")] == value
    pd.testing.assert_frame_equal(export.read_exported_csv(path, spec), expected, check_exact=True)


@pytest.mark.parametrize(
    ("value", "cause"),
    [
        pytest.param("\x00nul", None, id="nul"),
        pytest.param("bad\ud800", UnicodeEncodeError, id="lone_surrogate"),
    ],
)
def test_s02_trn_03_rejects_unwritable_text(
    value: str,
    cause: type[Exception] | None,
    result_07_02: ProgramResult,
    write_paths: RepoPaths,
    file_state: _FileState,
) -> None:
    """A character value the CSV cannot carry raises ``ValueError`` before any write (AAP 0.8).

    ``value`` is one ``PARAM`` of a copy of the WPCT-F.07.02 ``css_anadata`` frame: a NUL
    character, which CSV readers do not return intact, or a lone surrogate, which UTF-8 cannot
    encode. ``PARAM`` is one of the S02-TRN-03 analysis variables that frame keeps. The error names
    the export, the column and the data row but not the value, and chains the encoding error. On an
    empty write root nothing is created. Over the files of a previous clean run, the tree and every
    file's bytes and ``st_mtime_ns`` are unchanged.
    """
    result, row = _with_param(result_07_02, value)

    with pytest.raises(ValueError) as excinfo:
        export.write_program_outputs(result, write_paths)

    message = str(excinfo.value)
    assert f"{PROGRAM_07_02}/css_anadata column PARAM, data row {row}:" in message
    assert value not in message
    if cause is None:
        assert excinfo.value.__cause__ is None
    else:
        assert isinstance(excinfo.value.__cause__, cause)
    assert _tree(write_paths.write_root) == {}

    previous = export.write_program_outputs(result_07_02, write_paths)
    planted = _tree(write_paths.write_root)
    before = {path: file_state(path) for path in previous}

    with pytest.raises(ValueError) as rerun:
        export.write_program_outputs(result, write_paths)

    assert str(rerun.value) == message
    assert _tree(write_paths.write_root) == planted
    assert {path: file_state(path) for path in previous} == before


# --- Protected destinations (AAP 0.8) -------------------------------------------------------------


@pytest.mark.parametrize(
    "relative",
    [
        pytest.param("whitepapers/WPCT/WPCT-F.07.02.sas", id="sas_program"),
        pytest.param("data/adam/cdisc/advs.xpt", id="input_xpt"),
        pytest.param("whitepapers/WPCT/outputs_sas/WPCT-F.07.02.lst", id="sas_listing"),
        pytest.param(None, id="outside_checkout"),
    ],
)
def test_rejects_protected_destination(
    relative: str | None,
    repo_paths: RepoPaths,
    tmp_path: Path,
    file_state: _FileState,
) -> None:
    """The guard refuses a protected repository file or a path outside the checkout (AAP 0.8).

    ``relative`` names a SAS-era file of the checkout, or ``None`` for ``tmp_path / "outside.csv"``.
    The guard is called with the checkout-rooted ``repo_paths``, the paths the command line uses. It
    must raise ``OutputPathError`` naming the target and its resolved path. The file's bytes and
    ``st_mtime_ns``, and its directory's listing, must be unchanged, and the outside path must still
    not exist. Each target lies outside the fixed locations under ``python/wpct/`` that receive the
    port's outputs (ENV-03).
    """
    if relative is None:
        target = tmp_path / "outside.csv"
        assert not target.exists()
        file_before = None
    else:
        target = repo_paths.repo_root / relative
        assert target.is_file(), f"protected input missing from the checkout: {target}"
        file_before = file_state(target)
    directory_before = file_state(target.parent)

    with pytest.raises(OutputPathError) as excinfo:
        check_write_destination(target, repo_paths)

    error = excinfo.value
    assert error.target == target
    assert error.resolved == target.resolve()
    assert str(error.target) in str(error)
    assert str(error.resolved) in str(error)
    assert file_state(target.parent) == directory_before
    if file_before is None:
        assert not target.exists()
    else:
        assert file_state(target) == file_before


@pytest.mark.parametrize("destination", ["output_file", "report_file"])
def test_env03_accepts_fixed_destinations(
    destination: str, repo_paths: RepoPaths, file_state: _FileState
) -> None:
    """Positive control: the guard accepts the checkout's two fixed destinations (AAP 0.8).

    It returns the resolved target and creates, opens and modifies nothing. Neither the target nor
    its directory comes into existence, and a target already there is left unchanged. The targets,
    the WPCT-F.07.02 ``css_stats`` export file and the report file, are the fixed locations under
    ``python/wpct/`` that receive the port's outputs (ENV-03).
    """
    if destination == "output_file":
        target = repo_paths.program_outputs_dir(PROGRAM_07_02) / export_spec(
            PROGRAM_07_02, "css_stats"
        ).file_name
    else:
        target = repo_paths.report_file
    watched = (target, target.parent, repo_paths.outputs_dir)
    before = {path: file_state(path) if path.exists() else None for path in watched}

    assert check_write_destination(target, repo_paths) == target.resolve()

    after = {path: file_state(path) if path.exists() else None for path in watched}
    assert after == before


@pytest.mark.parametrize("variant", ["output_file", "report_file", "sas_program", "write_root"])
def test_env03_rejects_nul_byte_destination(
    variant: str, repo_paths: RepoPaths, write_paths: RepoPaths, file_state: _FileState
) -> None:
    """A NUL byte in the target or the write root is refused with ``OutputPathError`` (AAP 0.8).

    Outputs go only to fixed locations under ``python/wpct/`` (ENV-03), and every other target
    raises ``OutputPathError``. No file name can hold a NUL byte, so ``Path.resolve()`` raises
    ``ValueError`` on one; the guard must refuse such a target instead of letting that error escape
    the ``OutputPathError`` handling of ``reproduce``, which exits 2 on a refusal (AAP 0.6.5).

    - ``output_file``: the checkout's WPCT-F.07.02 ``css_stats`` export path, with a NUL in place
      of the ``_`` in its file name.
    - ``report_file``: the checkout's report path followed by ``"\\x00.sas"``.
    - ``sas_program``: the protected ``whitepapers/WPCT/WPCT-F.07.02.sas`` followed by a NUL.
    - ``write_root``: the ``css_stats`` export path under ``write_paths`` with the write root moved
      to ``sub\\x00dir`` inside it.

    The first three are vetted against the checkout-rooted ``repo_paths``. The error must name the
    target and its absolute form (``os.path.abspath``), give the reason ``cannot resolve the
    destination`` and chain the ``ValueError``. The existing files and directories each target
    names or sits in are unchanged, and none that is absent comes into existence.
    """
    css_stats = export_spec(PROGRAM_07_02, "css_stats").file_name
    paths = repo_paths
    if variant == "output_file":
        directory = repo_paths.program_outputs_dir(PROGRAM_07_02)
        target = directory / css_stats.replace("_", "\x00", 1)
        watched = (directory / css_stats, directory, repo_paths.outputs_dir)
    elif variant == "report_file":
        target = Path(f"{repo_paths.report_file}\x00.sas")
        watched = (repo_paths.report_file, repo_paths.report_file.parent)
    elif variant == "sas_program":
        protected = repo_paths.repo_root / "whitepapers/WPCT/WPCT-F.07.02.sas"
        assert protected.is_file(), f"protected input missing from the checkout: {protected}"
        target = Path(f"{protected}\x00")
        watched = (protected, protected.parent)
    else:
        paths = dataclasses.replace(write_paths, write_root=write_paths.write_root / "sub\x00dir")
        target = paths.program_outputs_dir(PROGRAM_07_02) / css_stats
        watched = (write_paths.write_root,)
    before = {path: file_state(path) if path.exists() else None for path in watched}

    with pytest.raises(OutputPathError) as excinfo:
        check_write_destination(target, paths)

    error = excinfo.value
    assert error.target == target
    assert error.resolved == Path(os.path.abspath(target))
    assert error.reason is not None
    assert error.reason.startswith("cannot resolve the destination: ")
    assert isinstance(error.__cause__, ValueError)
    after = {path: file_state(path) if path.exists() else None for path in watched}
    assert after == before


# --- Symlinked destinations (AAP 0.8) -------------------------------------------------------------


@pytest.mark.parametrize("link", ["outputs_dir", "output_file"])
def test_rejects_symlinked_destination(
    link: str,
    write_paths: RepoPaths,
    result_07_02: ProgramResult,
    tmp_path: Path,
    file_state: _FileState,
) -> None:
    """A fixed destination reached through a symlink is refused, and nothing is opened (AAP 0.8).

    - ``outputs_dir``: the write root's ``python/wpct/outputs`` is a symlink to a directory outside
      the write root that already holds ``sentinel.csv``.
    - ``output_file``: the last export file of WPCT-F.07.02, inside a real outputs directory, is a
      symlink to an external stand-in for a SAS-export CSV. Every earlier target of the call passes
      the guard, so an unchanged tree also shows that the writer vets all targets before it opens
      any.

    ``write_program_outputs`` must raise ``OutputPathError``. The external file's bytes and
    ``st_mtime_ns`` and the external directory's listing must be unchanged, and the write root must
    hold only the directories and the link the test made. The refusal stops the WPCT-F.07.02
    exports from reaching any file outside their fixed locations under ``python/wpct/`` (ENV-03).
    """
    program_dir = write_paths.program_outputs_dir(PROGRAM_07_02)
    if link == "outputs_dir":
        write_paths.outputs_dir.parent.mkdir(parents=True)
        external_dir = tmp_path / "external_dir"
        external_dir.mkdir()
        external_file = external_dir / "sentinel.csv"
        external_file.write_bytes(_SAS_EXPORT_STANDIN)
        link_path = write_paths.outputs_dir
        _symlink(link_path, external_dir)
    else:
        program_dir.mkdir(parents=True)
        external_dir = tmp_path
        external_file = tmp_path / "external_sas_export.csv"
        external_file.write_bytes(_SAS_EXPORT_STANDIN)
        link_path = program_dir / EXPORTS[PROGRAM_07_02][-1].file_name
        _symlink(link_path, external_file)

    planted = _tree(write_paths.write_root)
    assert "file" not in planted.values()
    assert planted[link_path.relative_to(write_paths.write_root).as_posix()] == "symlink"
    file_before = file_state(external_file)
    directory_before = file_state(external_dir)
    targets = export.output_targets(result_07_02, write_paths)

    with pytest.raises(OutputPathError) as excinfo:
        export.write_program_outputs(result_07_02, write_paths)

    error = excinfo.value
    if link == "outputs_dir":
        assert error.target in targets.values()
        assert error.resolved.is_relative_to(external_dir.resolve())
    else:
        assert error.target == link_path
        assert error.resolved == external_file.resolve()
    assert file_state(external_file) == file_before
    assert file_state(external_dir) == directory_before
    assert _tree(write_paths.write_root) == planted


def test_env03_rejects_symlink_inside_outputs_tree(
    write_paths: RepoPaths,
    result_07_02: ProgramResult,
    file_state: _FileState,
) -> None:
    """A fixed destination symlinked to another file inside the outputs tree is refused (AAP 0.8).

    The policy refuses every fixed destination whose own path is a symlink, including one that
    leads elsewhere inside the permitted tree. Here the last WPCT-F.07.02 export file links to a
    decoy CSV directly below ``python/wpct/outputs``. The writer must raise ``OutputPathError``
    naming the link and the decoy, and leave the decoy and the tree unchanged. The refusal stops the
    export from reaching any file of the tree other than its own fixed location under
    ``python/wpct/`` (ENV-03).
    """
    program_dir = write_paths.program_outputs_dir(PROGRAM_07_02)
    program_dir.mkdir(parents=True)
    decoy = write_paths.outputs_dir / "decoy.csv"
    decoy.write_bytes(_SAS_EXPORT_STANDIN)
    link_path = program_dir / EXPORTS[PROGRAM_07_02][-1].file_name
    _symlink(link_path, decoy)

    planted = _tree(write_paths.write_root)
    decoy_before = file_state(decoy)

    with pytest.raises(OutputPathError) as excinfo:
        export.write_program_outputs(result_07_02, write_paths)

    assert excinfo.value.target == link_path
    assert excinfo.value.resolved == decoy.resolve()
    assert file_state(decoy) == decoy_before
    assert _tree(write_paths.write_root) == planted


# --- Hard links, special files and replaced destinations (AAP 0.8) --------------------------------


#: How ``wpct.paths.open_fixed_destination`` binds a write: ``dir_fd``, through directory
#: descriptors, or ``path``, its fallback for platforms whose ``os.open`` takes no ``dir_fd``.
_STRATEGIES = ("dir_fd", "path")

#: The entry each replaced-destination case swaps in after the preflight, and the refusal reason
#: the writer must then give.
_SWAP_REASONS = {
    "final_symlink": "is reached through a symlink",
    "final_hard_link": "is a hard link shared with 1 other path(s)",
    "final_fifo": "not a regular file",
    "ancestor_symlink": "is reached through a symlink",
}


def _hard_link(link: Path, target: Path) -> None:
    """Create ``link`` as a hard link to ``target``, or skip where the file system refuses one."""
    try:
        os.link(target, link)
    except OSError as exc:
        pytest.skip(f"hard links not supported here: {exc}")


def _fifo(path: Path) -> None:
    """Create a FIFO at ``path``, or skip where the platform has none."""
    mkfifo = getattr(os, "mkfifo", None)
    if mkfifo is None:
        pytest.skip("FIFOs not supported on this platform")
    try:
        mkfifo(path)
    except OSError as exc:
        pytest.skip(f"FIFOs not supported here: {exc}")


def _use_strategy(strategy: str, monkeypatch: pytest.MonkeyPatch) -> None:
    """Make ``open_fixed_destination`` bind writes by ``strategy``, or skip where it cannot."""
    if strategy == "path":
        monkeypatch.setattr(wpct_paths, "_DIR_FD_WRITES", False)
    elif not wpct_paths._DIR_FD_WRITES:
        pytest.skip("this platform cannot open files relative to a directory descriptor")


def _stage_swap(swap: str, staged: Path, sentinel: Path, external_dir: Path) -> None:
    """Create at ``staged`` the entry that the ``swap`` case later renames into place."""
    if swap == "final_symlink":
        _symlink(staged, sentinel)
    elif swap == "final_hard_link":
        _hard_link(staged, sentinel)
    elif swap == "final_fifo":
        _fifo(staged)
    else:
        _symlink(staged, external_dir)


def _swap_after_last_vet(
    monkeypatch: pytest.MonkeyPatch, last_target: Path, swap_in: Callable[[], None]
) -> None:
    """Run ``swap_in`` as soon as the writer's preflight has accepted ``last_target``.

    ``export.vet_fixed_destination`` is wrapped, so the swap falls between the check of every
    target of the call and the first open, the window a concurrent process could use.
    """
    vet = export.vet_fixed_destination

    def vet_then_swap(target: str | os.PathLike[str], paths: RepoPaths) -> Path:
        resolved = vet(target, paths)
        if Path(target) == last_target:
            swap_in()
        return resolved

    monkeypatch.setattr(export, "vet_fixed_destination", vet_then_swap)


@pytest.mark.parametrize("entry", ["hard_link_first", "hard_link_last", "fifo", "directory"])
def test_env03_rejects_unsafe_existing_destination(
    entry: str,
    write_paths: RepoPaths,
    result_07_02: ProgramResult,
    tmp_path: Path,
    file_state: _FileState,
) -> None:
    """An export path that aliases another file or is no regular file is refused (AAP 0.8).

    Outputs go only to fixed locations under ``python/wpct/`` (ENV-03), and no write may change a
    SAS-era file or a file outside the write root. Resolving a path follows symlinks but not hard
    links (CWE-59), so the entry itself is checked:

    - ``hard_link_first`` and ``hard_link_last``: the first or the last WPCT-F.07.02 export path is
      a hard link to a stand-in, under ``tmp_path``, for a SAS-era file or an external SAS export.
    - ``fifo``: the last export path is a FIFO, which an ordinary open for writing waits on until a
      reader appears.
    - ``directory``: the last export path is a directory.

    ``write_program_outputs`` must raise ``OutputPathError`` naming that export path and why, at
    once and before it creates a directory or opens a file for any target of the call: the
    stand-in's bytes and ``st_mtime_ns`` are unchanged and the write root holds only what the test
    planted.
    """
    specs = EXPORTS[PROGRAM_07_02]
    program_dir = write_paths.program_outputs_dir(PROGRAM_07_02)
    program_dir.mkdir(parents=True)
    path = program_dir / (specs[0] if entry == "hard_link_first" else specs[-1]).file_name
    sentinel = tmp_path / "sentinel.csv"
    sentinel.write_bytes(_SAS_EXPORT_STANDIN)
    if entry == "fifo":
        _fifo(path)
        reason = "is a FIFO, not a regular file"
    elif entry == "directory":
        path.mkdir()
        reason = "is a directory, not a regular file"
    else:
        _hard_link(path, sentinel)
        reason = "is a hard link shared with 1 other path(s)"
    planted = _tree(write_paths.write_root)
    sentinel_before = file_state(sentinel)

    with pytest.raises(OutputPathError) as excinfo:
        export.write_program_outputs(result_07_02, write_paths)

    error = excinfo.value
    assert error.target == path
    assert error.resolved == path.resolve()
    assert f"the fixed destination {path.resolve()} {reason}" in str(error)
    assert file_state(sentinel) == sentinel_before
    assert _tree(write_paths.write_root) == planted


@pytest.mark.parametrize("strategy", _STRATEGIES)
def test_env03_rewrites_existing_outputs(
    strategy: str,
    monkeypatch: pytest.MonkeyPatch,
    write_paths: RepoPaths,
    result_07_02: ProgramResult,
) -> None:
    """Exports already on disk are rewritten in place, each truncated first (AAP 0.8).

    Outputs go only to fixed locations under ``python/wpct/`` (ENV-03), so a second run writes the
    same files again instead of placing new ones beside them. ``strategy`` selects how
    ``open_fixed_destination`` binds the write: ``dir_fd`` or the ``path`` fallback, which must
    also create the directories of the first write. After a first clean write, a stale tail is
    appended to every file. A second write of the same result must return the same paths and leave
    every file with the first write's bytes, its inode and a single hard link, in an unchanged tree.
    """
    _use_strategy(strategy, monkeypatch)
    first = _written_by_name(result_07_02, write_paths)
    contents = {name: path.read_bytes() for name, path in first.items()}
    inodes = {name: os.stat(path).st_ino for name, path in first.items()}
    for path in first.values():
        with open(path, "ab") as handle:
            handle.write(b"stale tail\n" * 1000)
    tree = _tree(write_paths.write_root)

    second = _written_by_name(result_07_02, write_paths)

    assert second == first
    assert {name: path.read_bytes() for name, path in second.items()} == contents
    assert {name: os.stat(path).st_ino for name, path in second.items()} == inodes
    assert all(os.stat(path).st_nlink == 1 for path in second.values())
    assert _tree(write_paths.write_root) == tree


@pytest.mark.parametrize("swap", list(_SWAP_REASONS))
@pytest.mark.parametrize("strategy", _STRATEGIES)
def test_env03_refuses_destination_replaced_after_vetting(
    strategy: str,
    swap: str,
    monkeypatch: pytest.MonkeyPatch,
    write_paths: RepoPaths,
    result_07_02: ProgramResult,
    tmp_path: Path,
    file_state: _FileState,
) -> None:
    """A destination replaced after the preflight is refused before it is written (AAP 0.8).

    Outputs go only to fixed locations under ``python/wpct/`` (ENV-03), and no write may follow a
    symlink below the write root or change another path's file, even when an entry changes
    between the check and the open (CWE-367). Over a first clean write of the WPCT-F.07.02 exports,
    an entry staged beforehand is renamed into place as soon as the second call's preflight has
    accepted its last target:

    - ``final_symlink``: the last export file becomes a symlink to an external stand-in for a
      SAS-export CSV, in a directory outside the write root.
    - ``final_hard_link``: the last export file becomes a hard link to that stand-in.
    - ``final_fifo``: the last export file becomes a FIFO without a reader.
    - ``ancestor_symlink``: ``python/wpct/outputs/WPCT-F.07.02`` becomes a symlink to the external
      directory.

    ``strategy`` selects how ``open_fixed_destination`` binds the write (``dir_fd`` or the ``path``
    fallback). The writer must raise ``OutputPathError`` at once, naming the swapped file, or the
    first export file for the ancestor, with the reason. The stand-in's bytes and ``st_mtime_ns``
    and the external directory's listing are unchanged, and the write root holds what it held after
    the swap: earlier targets of the call are rewritten in place and nothing else is created.
    """
    _use_strategy(strategy, monkeypatch)
    targets = list(export.output_targets(result_07_02, write_paths).values())
    export.write_program_outputs(result_07_02, write_paths)
    external_dir = tmp_path / "external_dir"
    external_dir.mkdir()
    sentinel = external_dir / "sentinel.csv"
    sentinel.write_bytes(_SAS_EXPORT_STANDIN)
    if swap == "ancestor_symlink":
        replaced, refused = write_paths.program_outputs_dir(PROGRAM_07_02), targets[0]
    else:
        replaced, refused = targets[-1], targets[-1]
    staged = replaced.with_name("staged")
    _stage_swap(swap, staged, sentinel, external_dir)
    after_swap: list[dict[str, str]] = []

    def swap_in() -> None:
        if swap == "ancestor_symlink":
            replaced.rename(tmp_path / "replaced_dir")
        os.replace(staged, replaced)
        after_swap.append(_tree(write_paths.write_root))

    _swap_after_last_vet(monkeypatch, targets[-1], swap_in)
    sentinel_before = file_state(sentinel)
    external_before = file_state(external_dir)

    with pytest.raises(OutputPathError) as excinfo:
        export.write_program_outputs(result_07_02, write_paths)

    assert len(after_swap) == 1
    assert excinfo.value.target == refused
    assert _SWAP_REASONS[swap] in str(excinfo.value)
    assert file_state(sentinel) == sentinel_before
    assert file_state(external_dir) == external_before == ["sentinel.csv"]
    assert _tree(write_paths.write_root) == after_swap[0]


#: The cases in which the ``path`` fallback opens a file it cannot show to be the destination.
_UNBOUND_OPENS = ("ancestor_swapped_during_open", "unidentified_handle")


def _swap_ancestor_during_open(
    patched: pytest.MonkeyPatch, target: Path, ancestor: Path, staged: Path, parked: Path
) -> list[str]:
    """Swap the directory ``ancestor`` for the symlink ``staged`` while ``target`` is opened.

    Through ``patched``, ``wpct.paths`` gets a copy of ``os`` whose ``open`` and ``lstat`` schedule
    a concurrent process against that module's own calls, leaving path resolution elsewhere as it
    is: just before ``target`` is opened by path, ``ancestor`` moves to ``parked`` and ``staged``
    takes its place; right after the open the real directory is back, for a later check of the
    path to find, and the next ``os.lstat`` of ``ancestor`` swaps the symlink in again, so a last
    lookup of ``target`` would reach the file the symlink leads to. Returns the swaps made, in
    order.
    """
    opened_at, watched = target.resolve(), ancestor.resolve()
    real_open, real_lstat = os.open, os.lstat
    swaps: list[str] = []

    def to_symlink() -> None:
        ancestor.rename(parked)
        staged.rename(ancestor)

    def to_directory() -> None:
        ancestor.rename(staged)
        parked.rename(ancestor)

    def open_during_swap(
        path: str | os.PathLike[str], flags: int, mode: int = 0o777, *, dir_fd: int | None = None
    ) -> int:
        if dir_fd is None and not swaps and Path(path) == opened_at:
            to_symlink()
            swaps.append("open")
            try:
                return real_open(path, flags, mode)
            finally:
                to_directory()
        return real_open(path, flags, mode, dir_fd=dir_fd)

    def lstat_then_relink(
        path: str | os.PathLike[str], *, dir_fd: int | None = None
    ) -> os.stat_result:
        info = real_lstat(path, dir_fd=dir_fd)
        if swaps == ["open"] and Path(path) == watched:
            to_symlink()
            swaps.append("lstat")
        return info

    scheduled_os = SimpleNamespace(**vars(os))
    scheduled_os.open = open_during_swap
    scheduled_os.lstat = lstat_then_relink
    patched.setattr(wpct_paths, "os", scheduled_os)
    return swaps


@pytest.mark.parametrize("case", _UNBOUND_OPENS)
def test_env03_path_fallback_refuses_unbound_open(
    case: str,
    monkeypatch: pytest.MonkeyPatch,
    write_paths: RepoPaths,
    result_07_02: ProgramResult,
    tmp_path: Path,
    file_state: _FileState,
) -> None:
    """The ``path`` fallback truncates only a file it shows to be the destination (AAP 0.8).

    Outputs go only to fixed locations under ``python/wpct/`` (ENV-03), and no write may follow a
    symlink below the write root, even one swapped in and out while the file is opened (CWE-367).
    After a first clean write of the WPCT-F.07.02 exports with the ``path`` fallback, a directory
    outside the write root holds a stand-in for a SAS-export CSV under the first export's file
    name, and the second call meets one case:

    - ``ancestor_swapped_during_open``: ``python/wpct/outputs/WPCT-F.07.02`` is a symlink to that
      directory only while the first export is opened; the real directory is back for a later
      check of the path, and the symlink again for a last lookup of the file, so a fallback that
      checked paths alone would truncate the stand-in.
    - ``unidentified_handle``: the platform reports no path for the opened file
      (``wpct.paths._opened_path`` returns ``None``).

    The writer must raise ``OutputPathError`` for the first export file with the reason, and close
    the descriptor it opened. Every export of the first write and the stand-in keep their bytes and
    ``st_mtime_ns``, the external directory its listing, and the write root its tree.
    """
    _use_strategy("path", monkeypatch)
    first = _written_by_name(result_07_02, write_paths)
    refused = next(iter(export.output_targets(result_07_02, write_paths).values()))
    external_dir = tmp_path / "external_dir"
    external_dir.mkdir()
    standin = external_dir / refused.name
    standin.write_bytes(_SAS_EXPORT_STANDIN)
    staged = tmp_path / "staged_link"
    _symlink(staged, external_dir)
    outputs_before = {name: file_state(path) for name, path in first.items()}
    standin_before = file_state(standin)
    external_before = file_state(external_dir)
    tree = _tree(write_paths.write_root)
    locate = wpct_paths._opened_path
    located: list[int] = []

    def locate_recorded(descriptor: int) -> str | None:
        located.append(descriptor)
        return None if case == "unidentified_handle" else locate(descriptor)

    swaps: list[str] = []
    with monkeypatch.context() as patched:
        patched.setattr(wpct_paths, "_opened_path", locate_recorded)
        if case == "ancestor_swapped_during_open":
            swaps = _swap_ancestor_during_open(
                patched,
                refused,
                write_paths.program_outputs_dir(PROGRAM_07_02),
                staged,
                tmp_path / "parked_dir",
            )
        with pytest.raises(OutputPathError) as excinfo:
            export.write_program_outputs(result_07_02, write_paths)

    error = excinfo.value
    destination = refused.resolve()
    assert error.target == refused
    assert error.resolved == destination
    if case == "ancestor_swapped_during_open":
        assert swaps[:1] == ["open"]
        reason = f"{destination} was opened at {standin.resolve()}, not at itself"
    else:
        reason = f"{destination}: the platform reports no path for the opened file"
    assert f"the fixed destination {reason}" in str(error)
    assert len(located) == 1
    with pytest.raises(OSError):
        os.fstat(located[0])
    assert {name: file_state(path) for name, path in first.items()} == outputs_before
    assert file_state(standin) == standin_before
    assert file_state(external_dir) == external_before
    assert _tree(write_paths.write_root) == tree
