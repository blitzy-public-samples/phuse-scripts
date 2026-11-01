"""Tests of the CSV export of the intermediate datasets and of the write-destination guard.

Units under test: ``wpct.export`` (``output_targets``, ``frame_for_export``,
``write_program_outputs`` and ``read_exported_csv``) and ``wpct.paths.check_write_destination``.

Export contract (Agent Action Plan 0.6.4): one CSV per ``schemas.EXPORTS`` spec in
``python/wpct/outputs/<program>/``, encoded as UTF-8. The header row holds the SAS variable names in
``spec.columns`` order, numbers are written with ``%.17g``, a missing value is an empty field, and
rows are sorted by the record key (D-04, D-14). The record-level files carry the S02-TRN-03 and
S07-TRN-04 keep lists, the LMM-TRN-07 derived records, and the pooled copy with ``studynum``
(S07-TRN-05, S07-TRN-06), whose identifier ``'\\u00a0Pooled'`` must survive the encoding (D-16).

Write policy (Agent Action Plan 0.8, "Guard" and "Boundary tests"): every write in this module goes
through the ``write_paths`` fixture, whose write root lies under pytest's ``tmp_path``. The
checkout-rooted ``repo_paths`` reaches only ``check_write_destination``, which writes nothing, so no
test here writes into the checkout. The guard applies one rule to every write root, so the symlink
cases on a ``tmp_path`` write root also cover the checkout's fixed destinations.
"""

from __future__ import annotations

import csv
import os
from collections.abc import Callable, Mapping
from pathlib import Path

import pandas as pd
import pytest

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


# --- Round trip, keys and halted runs (AAP 0.6.4) -------------------------------------------------


@pytest.mark.parametrize(("program_id", "name"), _SPEC_CASES)
def test_round_trip_exact(
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
def test_export_keys_unique(
    program_id: str,
    results: Mapping[str, ProgramResult],
    write_paths: RepoPaths,
) -> None:
    """Every export file is unique on its record key and sorted by it (AAP 0.6.4, D-04, D-14).

    ``ExportSpec`` refuses an empty key, so every spec is checked. The sort check re-sorts the
    file's rows by key, ascending with missing values first, and requires the order to be
    unchanged. The row counts are those of the clean checkout, so a join with the SAS counterpart
    loses no record.
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
            f"{PROGRAM_07_07}: assert_unique_keys failed: 2 records in CSS_ANADATA share keys "
            "STUDYID USUBJID PARAMCD ATPTN",
            id="S07-FLT-05",
        ),
    ],
)
def test_halted_result_writes_nothing(check: str, message: str, write_paths: RepoPaths) -> None:
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
    not exist.
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
def test_accepts_fixed_destinations(
    destination: str, repo_paths: RepoPaths, file_state: _FileState
) -> None:
    """Positive control: the guard accepts the checkout's two fixed destinations (AAP 0.8).

    It returns the resolved target and creates, opens and modifies nothing. Neither the target nor
    its directory comes into existence, and a target already there is left unchanged.
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
    hold only the directories and the link the test made.
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


def test_rejects_symlink_inside_outputs_tree(
    write_paths: RepoPaths,
    result_07_02: ProgramResult,
    file_state: _FileState,
) -> None:
    """A fixed destination symlinked to another file inside the outputs tree is refused (AAP 0.8).

    The policy refuses every fixed destination whose own path is a symlink, including one that
    leads elsewhere inside the permitted tree. Here the last WPCT-F.07.02 export file links to a
    decoy CSV directly below ``python/wpct/outputs``. The writer must raise ``OutputPathError``
    naming the link and the decoy, and leave the decoy and the tree unchanged.
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
