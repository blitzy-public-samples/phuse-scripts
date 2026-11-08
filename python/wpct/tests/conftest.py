"""Shared fixtures of the WPCT equivalence suite.

The shared repository paths, input frames, driver runs, SAS references, tolerances and comparisons
that the test modules in ``python/wpct/tests/`` request come from the session fixtures here. Each is
built once per session through the package's own modules (``wpct.paths``, ``wpct.xpt_reader``, the
two drivers in ``wpct.programs``, ``wpct.sas_reference`` and ``wpct.compare`` with
``python/wpct/tolerances.json``), so no fixture parses the data, the references or the tolerances
any other way, or re-implements a driver step or a comparison rule. A test module may still compute
an expected value independently of the package to check it, as the raw transport-header decoding in
``test_xpt_reader.py`` and the hand-built least-squares fit in ``test_model.py`` do; such an oracle
checks the package and is not a second access path for the fixtures' data.

Session fixtures (read-only):

- ``repo_paths``: ``RepoPaths.checkout()``, the paths the command line uses; it reads from and
  would write to the checkout, so it is passed only to code that writes nothing.
- ``advs_07_02_raw``, ``advs_07_07_raw``: ADVS as ``xpt_reader.read_xpt`` returns it from
  ``data/adam/cdisc/advs.xpt`` (WPCT-F.07.02) and ``data/adam/cdisc-split/advs.xpt``
  (WPCT-F.07.07).
- ``result_07_02``, ``result_07_07``: one run of each driver with its default ``Config``, the
  program's own ``%let`` settings (S02-TRN-01, S07-TRN-01); ``results`` maps program ID to run.
- ``tolerances``: ``compare.load_tolerances`` of ``python/wpct/tolerances.json``.
- ``listing_tables``, ``sas_checkpoints``: per program, the GLM ParameterEstimates tables of
  ``whitepapers/WPCT/outputs_sas/<program>.lst`` and the pinned record counts of ``<program>.log``.
- ``sas_exports_dir``, ``sas_exports``: the export directory ``python/wpct/reference/sas_exports/``
  and the full-precision SAS exports found there, ``None`` for each absent file.
- ``comparisons``: per program, the ``compare.compare_program`` groups of its run.

Function fixtures:

- ``advs_07_02``, ``advs_07_07``: deep copies of the raw frames, which an edge-state test may
  alter and pass as ``run(..., advs=frame)`` without disturbing the session cache.
- ``write_paths``: paths whose write root is an empty directory under pytest's ``tmp_path``.
- ``file_state``: a snapshot callable that proves a file or directory unchanged.

Write policy (Agent Action Plan 0.8): no fixture writes into the checkout. The session fixtures
write nothing at all: the drivers never write, and ``export.write_program_outputs`` and
``report.write_report`` are not called here. ``write_paths`` is the suite's only call of
``RepoPaths.with_write_root``, so every test that writes does so under ``tmp_path``.

Session results, frames and mappings are shared by every test that requests them. A
``ProgramResult`` shares its DataFrames rather than copying them, so a test copies a frame before
altering it. Errors raised by the reader or a driver are not caught: an input error or a halt on
the clean checkout surfaces as an error of every test that depends on the run.
"""

from __future__ import annotations

from collections.abc import Callable, Mapping
from pathlib import Path
from types import MappingProxyType

import pandas as pd
import pytest

from wpct import compare, sas_reference, xpt_reader
from wpct.paths import RepoPaths
from wpct.programs import ProgramResult, wpct_f_07_02, wpct_f_07_07
from wpct.schemas import PROGRAM_07_02, PROGRAM_07_07, PROGRAMS

#: A snapshot taken by the ``file_state`` fixture: ``(bytes, st_mtime_ns)`` of a file, or the
#: sorted entry names of a directory.
Snapshot = tuple[bytes, int] | list[str]


def _read_advs(paths: RepoPaths, program_id: str) -> pd.DataFrame:
    """ADVS of ``program_id``, read as its driver reads it (S02/S07-DEP-util_access_test_data)."""
    frame, _metadata = xpt_reader.read_xpt(paths.input_xpt(program_id), program_id)
    return frame


def _snapshot(path: str | Path) -> Snapshot:
    """``(bytes, st_mtime_ns)`` of the file at ``path``, or the sorted entry names of a directory.

    Both follow symlinks, so a snapshot of a link describes its target. A path that does not exist
    raises ``FileNotFoundError`` rather than yielding a value two absent paths would share: a
    snapshot proves an existing file unchanged, and absence is asserted separately.
    """
    target = Path(path)
    if target.is_dir():
        return sorted(entry.name for entry in target.iterdir())
    return target.read_bytes(), target.stat().st_mtime_ns


# --- Repository paths and inputs ------------------------------------------------------------------


@pytest.fixture(scope="session")
def repo_paths() -> RepoPaths:
    """Paths of this checkout, identical to those the command line builds.

    Session scope; returns a ``RepoPaths``; no teardown: it only builds paths.
    """
    return RepoPaths.checkout()


@pytest.fixture(scope="session")
def advs_07_02_raw(repo_paths: RepoPaths) -> pd.DataFrame:
    """ADVS of WPCT-F.07.02 from ``data/adam/cdisc/advs.xpt``; never alter it.

    Session scope; returns a ``pd.DataFrame``; no teardown: it only reads the file.
    """
    return _read_advs(repo_paths, PROGRAM_07_02)


@pytest.fixture(scope="session")
def advs_07_07_raw(repo_paths: RepoPaths) -> pd.DataFrame:
    """ADVS of WPCT-F.07.07 from ``data/adam/cdisc-split/advs.xpt``; never alter it.

    Session scope; returns a ``pd.DataFrame``; no teardown: it only reads the file.
    """
    return _read_advs(repo_paths, PROGRAM_07_07)


@pytest.fixture
def advs_07_02(advs_07_02_raw: pd.DataFrame) -> pd.DataFrame:
    """A private deep copy of the WPCT-F.07.02 ADVS frame, free to alter.

    Function scope; returns a deep-copied ``pd.DataFrame``; no teardown: the copy is in memory only.
    """
    return advs_07_02_raw.copy(deep=True)


@pytest.fixture
def advs_07_07(advs_07_07_raw: pd.DataFrame) -> pd.DataFrame:
    """A private deep copy of the WPCT-F.07.07 ADVS frame, free to alter.

    Function scope; returns a deep-copied ``pd.DataFrame``; no teardown: the copy is in memory only.
    """
    return advs_07_07_raw.copy(deep=True)


# --- Driver runs ----------------------------------------------------------------------------------


@pytest.fixture(scope="session")
def result_07_02(repo_paths: RepoPaths) -> ProgramResult:
    """WPCT-F.07.02 run once with the program's own settings (S02-TRN-01); read-only.

    Session scope; returns a ``ProgramResult``; no teardown: the driver writes nothing.
    """
    return wpct_f_07_02.run(wpct_f_07_02.Config(), repo_paths)


@pytest.fixture(scope="session")
def result_07_07(repo_paths: RepoPaths) -> ProgramResult:
    """WPCT-F.07.07 run once with the program's own settings (S07-TRN-01); read-only.

    Session scope; returns a ``ProgramResult``; no teardown: the driver writes nothing.
    """
    return wpct_f_07_07.run(wpct_f_07_07.Config(), repo_paths)


@pytest.fixture(scope="session")
def results(
    result_07_02: ProgramResult, result_07_07: ProgramResult
) -> Mapping[str, ProgramResult]:
    """Both default runs keyed by program ID, in ``schemas.PROGRAMS`` order.

    Like every session mapping here it is a read-only view rather than a dict, because the tests
    share it and a test that altered it would change what every later test sees.

    Session scope; returns a read-only ``Mapping`` of program ID to ``ProgramResult``; no teardown:
    it only wraps the session runs.
    """
    by_program = {PROGRAM_07_02: result_07_02, PROGRAM_07_07: result_07_07}
    return MappingProxyType({program_id: by_program[program_id] for program_id in PROGRAMS})


# --- SAS references, tolerances and comparisons ---------------------------------------------------


@pytest.fixture(scope="session")
def tolerances(repo_paths: RepoPaths) -> compare.Tolerances:
    """The comparison limits of ``python/wpct/tolerances.json``, their only source.

    Session scope; returns a ``compare.Tolerances``; no teardown: it only reads the file.
    """
    return compare.load_tolerances(repo_paths.tolerances_file)


@pytest.fixture(scope="session")
def listing_tables(
    repo_paths: RepoPaths,
) -> Mapping[str, tuple[sas_reference.ListingTable, ...]]:
    """Per program, the ParameterEstimates tables of its SAS listing, checked against its map.

    Session scope; returns a read-only ``Mapping`` of program ID to a tuple of
    ``sas_reference.ListingTable``; no teardown: it only reads the listings.
    """
    return MappingProxyType(
        {
            program_id: sas_reference.parse_glm_listing(
                repo_paths.listing_file(program_id), sas_reference.GLM_TABLE_MAP[program_id]
            )
            for program_id in PROGRAMS
        }
    )


@pytest.fixture(scope="session")
def sas_checkpoints(
    repo_paths: RepoPaths,
) -> Mapping[str, tuple[sas_reference.SasCheckpoint, ...]]:
    """Per program, the record counts at the pinned lines of its SAS log.

    Session scope; returns a read-only ``Mapping`` of program ID to a tuple of
    ``sas_reference.SasCheckpoint``; no teardown: it only reads the logs.
    """
    return MappingProxyType(
        {
            program_id: sas_reference.parse_log_checkpoints(
                repo_paths.log_file(program_id), sas_reference.LOG_CHECKPOINT_MAP[program_id]
            )
            for program_id in PROGRAMS
        }
    )


@pytest.fixture(scope="session")
def sas_exports_dir(repo_paths: RepoPaths) -> Path:
    """The default directory of user-supplied full-precision SAS exports; a read-only input.

    Session scope; returns a ``Path``; no teardown: it only builds the path.
    """
    return repo_paths.sas_exports_dir


@pytest.fixture(scope="session")
def sas_exports(sas_exports_dir: Path) -> Mapping[str, pd.DataFrame | None]:
    """Each ``schemas.SAS_EXPORT_FILES`` name mapped to its loaded export, ``None`` when absent.

    Session scope; returns a read-only ``Mapping`` of file name to a ``pd.DataFrame`` or ``None``;
    no teardown: it only reads the exports present.
    """
    return MappingProxyType(sas_reference.load_sas_exports(sas_exports_dir))


@pytest.fixture(scope="session")
def comparisons(
    results: Mapping[str, ProgramResult],
    listing_tables: Mapping[str, tuple[sas_reference.ListingTable, ...]],
    sas_checkpoints: Mapping[str, tuple[sas_reference.SasCheckpoint, ...]],
    sas_exports: Mapping[str, pd.DataFrame | None],
    tolerances: compare.Tolerances,
) -> Mapping[str, tuple[compare.ComparisonGroup, ...]]:
    """Per program, every comparison group of its default run, in report order.

    ``compare_program`` receives all seven export entries and uses only its program's files.

    Session scope; returns a read-only ``Mapping`` of program ID to a tuple of
    ``compare.ComparisonGroup``; no teardown: it only compares in memory.
    """
    return MappingProxyType(
        {
            program_id: compare.compare_program(
                results[program_id],
                listing_tables[program_id],
                sas_checkpoints[program_id],
                sas_exports,
                tolerances,
            )
            for program_id in PROGRAMS
        }
    )


# --- Writes and file state ------------------------------------------------------------------------


@pytest.fixture
def write_paths(tmp_path: Path) -> RepoPaths:
    """The suite's only opt-in to a non-checkout write root (AAP 0.8).

    ``write_root`` is a new, empty ``write_root`` directory under ``tmp_path``; ``repo_root`` stays
    the checkout, so inputs and references are still read from it, and the write guard applies its
    single rule under the new root. Neither ``python/wpct/outputs`` nor ``python/wpct/reports`` is
    created: the writers create them, and the symlink tests plant their own links there.

    Function scope; returns a ``RepoPaths``; no teardown: the fixture deletes nothing, and
    ``tmp_path`` is pytest-managed temporary storage that pytest keeps or removes under its own
    retention policy (``tmp_path_retention_count``, ``tmp_path_retention_policy``).
    """
    root = tmp_path / "write_root"
    root.mkdir()
    return RepoPaths.with_write_root(root)


@pytest.fixture
def file_state() -> Callable[[str | Path], Snapshot]:
    """A callable taking a path and returning its :data:`Snapshot`.

    Two snapshots of a file are equal only when its bytes and ``st_mtime_ns`` are both unchanged;
    two snapshots of a directory are equal only when it lists the same entry names.

    Function scope; returns a ``Callable[[str | Path], Snapshot]``; no teardown: the callable only
    reads and holds no resource.
    """
    return _snapshot
