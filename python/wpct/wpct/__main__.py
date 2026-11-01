"""Command line of the WPCT port: ``python -m wpct reproduce [--sas-exports DIR]``.

Run from ``python/wpct/`` without installation. ``reproduce`` re-runs WPCT-F.07.02 and then
WPCT-F.07.07 from the repository ADVS transport files, exports their intermediate datasets to
``python/wpct/outputs/<program>/`` and regenerates ``python/wpct/reports/equivalence_report.md``
(Agent Action Plan 0.7.6). Each program uses its own ``%let`` settings, the default ``Config`` of
its driver (S02-TRN-01, S07-TRN-01).

Exit codes (Agent Action Plan 0.6.5):

* 0 (:data:`EXIT_OK`): no comparison failed and no program halted.
* 1 (:data:`EXIT_FAILED`): a comparison failed, or a halting check stopped a program
  (S07-FLT-04, S07-FLT-05). Like ``%assert_continue`` (S07-DEP-assert_continue), the halt stops
  that program before any export; the report records it.
* 2 (:data:`EXIT_INPUT_ERROR`): an input error, i.e. a missing or unreadable ADVS file, SAS
  reference, SAS export or tolerance file, a dataset lacking a required variable
  (S02-DEP-assert_depend, S07-DEP-assert_depend), or a ``--sas-exports`` that is not an existing
  directory; a write destination the guard rejects; or a command-line error from ``argparse``.

Data reach and write policy (Agent Action Plan 0.8): ``--sas-exports`` is the only option, and it
names a read-only input directory, which is never written. No option reaches an input file, an
output or report destination, or the write root: every path comes from :meth:`RepoPaths.checkout`
(:func:`build_paths`), and the only writes are those of ``wpct.export`` and ``wpct.report``, each
behind ``paths.check_write_destination``. The command runs no SAS and opens no network connection.
"""

from __future__ import annotations

import argparse
import os
import sys
from collections.abc import Mapping, Sequence
from pathlib import Path

from wpct import compare, export, provenance, report, sas_reference
from wpct.errors import AnalysisHalted, InputDataError, OutputPathError
from wpct.paths import RepoPaths
from wpct.programs import ProgramResult, wpct_f_07_02, wpct_f_07_07
from wpct.schemas import PROGRAM_07_02, PROGRAM_07_07

__all__ = [
    "EXIT_FAILED",
    "EXIT_INPUT_ERROR",
    "EXIT_OK",
    "PROG",
    "build_parser",
    "build_paths",
    "main",
    "reproduce",
]

#: The program name shown in usage and error messages.
PROG = "python -m wpct"

EXIT_OK = 0
EXIT_FAILED = 1
EXIT_INPUT_ERROR = 2

#: The drivers in the order the request fixes: WPCT-F.07.02, then WPCT-F.07.07.
_DRIVERS = ((PROGRAM_07_02, wpct_f_07_02), (PROGRAM_07_07, wpct_f_07_07))

_STATUS_ORDER = (compare.PASS, compare.FAIL, compare.NOT_COMPARED)


# --- Parser ---------------------------------------------------------------------------------------


def _sas_exports_argument(value: str) -> Path:
    """Convert the ``--sas-exports`` value to a path; an empty value is a usage error.

    ``Path("")`` would silently become the current directory, so it is refused here. Whether the
    path is an existing directory is checked by :func:`reproduce`, which tests also call directly.
    """
    if not value.strip():
        raise argparse.ArgumentTypeError("the directory must not be empty")
    return Path(value)


def _default_exports_text() -> str:
    """The default export directory as the help text shows it, relative to the checkout."""
    paths = RepoPaths.checkout()
    return f"{paths.sas_exports_dir.relative_to(paths.repo_root).as_posix()}/"


def build_parser() -> argparse.ArgumentParser:
    """Return the parser of ``python -m wpct``: the subcommand ``reproduce`` and nothing else.

    ``reproduce`` takes one option, ``--sas-exports DIR``. There is deliberately no option for an
    input, output, report or write-root path (Agent Action Plan 0.8). ``allow_abbrev=False`` on
    both parsers refuses option prefixes such as ``--sas-export``, so every option other than
    ``--sas-exports`` and ``-h/--help`` is an ``argparse`` error with exit code 2.
    """
    parser = argparse.ArgumentParser(
        prog=PROG,
        description=(
            "Python port of the PHUSE Central Tendency programs WPCT-F.07.02 and WPCT-F.07.07: "
            "re-run them from the repository data and verify the results against the SAS "
            "references."
        ),
        allow_abbrev=False,
    )
    commands = parser.add_subparsers(dest="command", required=True, metavar="COMMAND")
    reproduce_parser = commands.add_parser(
        "reproduce",
        help="re-run both programs, export their datasets and regenerate the equivalence report",
        description=(
            "Re-run WPCT-F.07.02 and WPCT-F.07.07 from data/adam/cdisc/advs.xpt and "
            "data/adam/cdisc-split/advs.xpt, write python/wpct/outputs/<program>/*.csv and "
            "regenerate python/wpct/reports/equivalence_report.md. Exit status: 0 when no "
            "comparison failed, 1 when a comparison failed or a program halted, 2 on an input "
            "error or a rejected destination."
        ),
        allow_abbrev=False,
    )
    reproduce_parser.add_argument(
        "--sas-exports",
        metavar="DIR",
        type=_sas_exports_argument,
        default=None,
        help=(
            "read-only input directory of user-supplied full-precision SAS exports "
            f"(default: {_default_exports_text()})"
        ),
    )
    return parser


def build_paths() -> RepoPaths:
    """Return the paths of this checkout, reading from and writing to the checkout itself.

    :meth:`RepoPaths.checkout` is the only constructor the command line uses, so no option can
    move the write root (Agent Action Plan 0.8).
    """
    return RepoPaths.checkout()


# --- reproduce ------------------------------------------------------------------------------------


def _error(message: str) -> int:
    """Report a failure of the run on stderr, without a traceback, and return exit code 2."""
    print(f"{PROG} reproduce: error: {message}", file=sys.stderr)
    return EXIT_INPUT_ERROR


def _display_path(path: Path, paths: RepoPaths) -> str:
    """``path`` relative to the checkout when it lies in it, otherwise absolute."""
    resolved = Path(path).resolve()
    root = paths.repo_root.resolve()
    if resolved.is_relative_to(root):
        return resolved.relative_to(root).as_posix()
    return str(resolved)


def _status_text(counts: Mapping[str, int]) -> str:
    return ", ".join(f"{status} {counts[status]}" for status in _STATUS_ORDER)


def reproduce(paths: RepoPaths, sas_exports_dir: str | os.PathLike[str] | None = None) -> int:
    """Re-run both programs, export their datasets, compare them and write the report.

    Args:
        paths: Where inputs and references are read and the two fixed destinations sit. The command
            line passes :func:`build_paths`; tests pass a write root under ``tmp_path``.
        sas_exports_dir: The read-only directory of full-precision SAS exports, or ``None`` for
            ``paths.sas_exports_dir``. A named directory must exist; the default may be absent, in
            which case every export comparison is NOT COMPARED.

    Returns:
        :data:`EXIT_OK`, :data:`EXIT_FAILED` or :data:`EXIT_INPUT_ERROR` (Agent Action Plan 0.6.5).

    Every input is read before anything is written: the tolerances, the SAS listings and logs, the
    SAS exports and then both program runs. An input error therefore writes nothing. A halted
    program becomes ``ProgramResult.from_halt``: it exports nothing, while the other program's
    exports and the report are still written, and the report records the halt.
    """
    if not isinstance(paths, RepoPaths):
        raise TypeError(f"paths must be a RepoPaths, got {type(paths).__name__}")

    exports_dir = paths.sas_exports_dir if sas_exports_dir is None else Path(sas_exports_dir)
    if sas_exports_dir is not None:
        try:
            is_directory = exports_dir.is_dir()
        except OSError as exc:
            return _error(f"--sas-exports: cannot access {exports_dir.absolute()}: {exc}")
        if not is_directory:
            return _error(f"--sas-exports: not an existing directory: {exports_dir.absolute()}")

    try:
        tolerances = compare.load_tolerances(paths.tolerances_file)
    except (OSError, ValueError) as exc:
        return _error(f"cannot load the tolerances: {exc}")

    try:
        listings = {
            pid: sas_reference.parse_glm_listing(
                paths.listing_file(pid), sas_reference.GLM_TABLE_MAP[pid]
            )
            for pid, _driver in _DRIVERS
        }
        logs = {
            pid: sas_reference.parse_log_checkpoints(
                paths.log_file(pid), sas_reference.LOG_CHECKPOINT_MAP[pid]
            )
            for pid, _driver in _DRIVERS
        }
        sas_exports = sas_reference.load_sas_exports(exports_dir)
    except InputDataError as exc:
        return _error(str(exc))

    results: dict[str, ProgramResult] = {}
    for pid, driver in _DRIVERS:
        try:
            results[pid] = driver.run(driver.Config(), paths)
        except AnalysisHalted as halt:
            results[pid] = ProgramResult.from_halt(halt)
        except InputDataError as exc:
            return _error(str(exc))

    try:
        for result in results.values():
            export.write_program_outputs(result, paths)
    except OutputPathError as exc:
        return _error(str(exc))

    comparisons = {
        pid: compare.compare_program(
            results[pid], listings[pid], logs[pid], sas_exports, tolerances
        )
        for pid, _driver in _DRIVERS
    }

    try:
        report_path = report.write_report(results, comparisons, provenance.collect(paths), paths)
    except OutputPathError as exc:
        return _error(str(exc))

    for pid, result in results.items():
        line = f"{pid}: {_status_text(compare.count_statuses(comparisons[pid]))}"
        if result.halted:
            line += f"; halted, nothing exported: {result.halt_message}"
        else:
            line += f"; outputs in {_display_path(paths.program_outputs_dir(pid), paths)}"
        print(line)
    overall = compare.count_statuses(group for groups in comparisons.values() for group in groups)
    print(f"Overall: {_status_text(overall)}")
    print(f"Report: {_display_path(report_path, paths)}")

    halted = any(result.halted for result in results.values())
    return EXIT_FAILED if overall[compare.FAIL] or halted else EXIT_OK


# --- Entry point ----------------------------------------------------------------------------------


def main(argv: Sequence[str] | None = None) -> int:
    """Parse ``argv`` (``sys.argv[1:]`` when ``None``) and run the chosen subcommand.

    ``reproduce`` is the only subcommand and ``argparse`` requires it, so a missing or unknown
    subcommand or option exits with code 2 before anything is read or written.
    """
    args = build_parser().parse_args(argv)
    return reproduce(build_paths(), args.sas_exports)


if __name__ == "__main__":
    sys.exit(main())
