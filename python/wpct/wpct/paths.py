"""Repository path model and write-destination guard of the WPCT port.

This module is the only place that builds repository paths. Every read location (the two ADVS
transport files, the SAS listings and logs, the SAS-export directory, the tolerance file and the
specification) and both write destinations are derived from :class:`RepoPaths`; other modules ask
it for a path and never join repository paths by hand.

Reads come from ``repo_root``, the checkout:

* ``data/adam/cdisc/advs.xpt`` for WPCT-F.07.02 and ``data/adam/cdisc-split/advs.xpt`` for
  WPCT-F.07.07. This is the local-read branch of ``%util_access_test_data``
  (S02-DEP-util_access_test_data, S07-DEP-util_access_test_data). The programs' own locations,
  the GitHub URL behind a blank ``local=`` (ENV-02) and the ``\\\\quintiles.net\\...`` share
  (ENV-04), are recorded in the specification and not reproduced.
* ``whitepapers/WPCT/outputs_sas/<program>.lst`` and ``.log``, the SAS reference results.
* ``python/wpct/reference/sas_exports/``, the default directory of user-supplied full-precision SAS
  exports; the command line may name another directory, never through this class.
* ``python/wpct/tolerances.json`` and ``python/wpct/docs/sas_specification.md``.

Writes go to ``write_root``, and only to two fixed destinations (ENV-03: outputs go to fixed
locations under ``python/wpct/``): anything strictly below ``python/wpct/outputs`` and the single
file ``python/wpct/reports/equivalence_report.md``. :func:`check_write_destination` applies the
resolved-path rule of that data reach and write policy (Agent Action Plan 0.8) to every target
before it is opened, and the writers additionally require each resolved target to be its own
fixed destination.

Importing this module touches no file: paths are resolved only when a constructor or the guard runs.
"""

from __future__ import annotations

import os
from dataclasses import dataclass
from pathlib import Path

from wpct.errors import OutputPathError
from wpct.schemas import PROGRAM_07_02, PROGRAM_07_07

__all__ = ["OUTPUTS_RELATIVE", "REPORT_RELATIVE", "RepoPaths", "check_write_destination"]

_PROJECT_RELATIVE = Path("python/wpct")

#: The permitted write destinations, relative to the write root: the outputs tree (anything
#: strictly below it) and the report file (that file only).
OUTPUTS_RELATIVE = _PROJECT_RELATIVE / "outputs"
REPORT_RELATIVE = _PROJECT_RELATIVE / "reports" / "equivalence_report.md"

#: The ADVS transport file each program reads, relative to the checkout.
_INPUT_XPT_RELATIVE = {
    PROGRAM_07_02: Path("data/adam/cdisc/advs.xpt"),
    PROGRAM_07_07: Path("data/adam/cdisc-split/advs.xpt"),
}

_OUTPUTS_SAS_RELATIVE = Path("whitepapers/WPCT/outputs_sas")
_SAS_EXPORTS_RELATIVE = _PROJECT_RELATIVE / "reference" / "sas_exports"
_TOLERANCES_RELATIVE = _PROJECT_RELATIVE / "tolerances.json"
_SPEC_RELATIVE = _PROJECT_RELATIVE / "docs" / "sas_specification.md"


def _checkout_root() -> Path:
    """Return the checkout root; this file is ``<repo>/python/wpct/wpct/paths.py``."""
    return Path(__file__).resolve().parents[3]


def _require_program(program_id: str) -> str:
    """Return ``program_id`` when the port implements it, else raise ``KeyError``.

    Only the two ported programs have inputs, references and output directories, so a program
    identifier never becomes a path component unchecked.
    """
    if program_id not in _INPUT_XPT_RELATIVE:
        known = ", ".join(_INPUT_XPT_RELATIVE)
        raise KeyError(f"unknown program {program_id!r}; expected one of: {known}")
    return program_id


@dataclass(frozen=True)
class RepoPaths:
    """Read and write locations of one run.

    ``repo_root`` is the checkout, from which every input and reference is read. ``write_root`` is
    the directory under which the two fixed write destinations sit. :meth:`checkout` sets both to
    the checkout and is the only constructor the command line uses; :meth:`with_write_root` is the
    test suite's opt-in to another write root. Write-side paths are joined, not resolved:
    :func:`check_write_destination` resolves them when it vets a write.
    """

    repo_root: Path
    write_root: Path

    def __post_init__(self) -> None:
        object.__setattr__(self, "repo_root", Path(self.repo_root))
        object.__setattr__(self, "write_root", Path(self.write_root))

    @classmethod
    def checkout(cls) -> RepoPaths:
        """Paths of this checkout, reading from and writing to the checkout itself."""
        root = _checkout_root()
        return cls(repo_root=root, write_root=root)

    @classmethod
    def with_write_root(cls, root: str | os.PathLike[str]) -> RepoPaths:
        """Test-only. Called solely by the `write_paths` fixture in `python/wpct/tests/conftest.py`,
        rooted under pytest's `tmp_path`.

        Inputs and references are still read from the checkout; only the write root changes, and
        :func:`check_write_destination` applies the same rule under it.
        """
        return cls(repo_root=_checkout_root(), write_root=Path(root))

    # --- Read side: under repo_root ---------------------------------------------------------------

    @property
    def project_dir(self) -> Path:
        """``python/wpct``, the project root of the port."""
        return self.repo_root / _PROJECT_RELATIVE

    def input_xpt(self, program_id: str) -> Path:
        """The ADVS transport file ``program_id`` reads.

        Local-read branch of ``%util_access_test_data`` (S02-DEP-util_access_test_data,
        S07-DEP-util_access_test_data). Raises ``KeyError`` for a program the port does not
        implement.
        """
        return self.repo_root / _INPUT_XPT_RELATIVE[_require_program(program_id)]

    @property
    def outputs_sas_dir(self) -> Path:
        """``whitepapers/WPCT/outputs_sas``, the saved SAS reference results."""
        return self.repo_root / _OUTPUTS_SAS_RELATIVE

    def listing_file(self, program_id: str) -> Path:
        """The SAS listing ``<program_id>.lst`` holding the GLM ParameterEstimates tables."""
        return self.outputs_sas_dir / f"{_require_program(program_id)}.lst"

    def log_file(self, program_id: str) -> Path:
        """The SAS log ``<program_id>.log`` holding the dataset record counts."""
        return self.outputs_sas_dir / f"{_require_program(program_id)}.log"

    @property
    def sas_exports_dir(self) -> Path:
        """Default read-only directory of user-supplied full-precision SAS exports."""
        return self.repo_root / _SAS_EXPORTS_RELATIVE

    @property
    def tolerances_file(self) -> Path:
        """``python/wpct/tolerances.json``, the single source of comparison tolerances."""
        return self.repo_root / _TOLERANCES_RELATIVE

    @property
    def spec_file(self) -> Path:
        """``python/wpct/docs/sas_specification.md``, the extracted SAS specification."""
        return self.repo_root / _SPEC_RELATIVE

    # --- Write side: under write_root, joined and not resolved ------------------------------------

    @property
    def outputs_dir(self) -> Path:
        """``python/wpct/outputs`` under the write root; exports go strictly below it."""
        return self.write_root / OUTPUTS_RELATIVE

    def program_outputs_dir(self, program_id: str) -> Path:
        """The export directory of ``program_id``, ``python/wpct/outputs/<program_id>``."""
        return self.outputs_dir / _require_program(program_id)

    @property
    def report_file(self) -> Path:
        """``python/wpct/reports/equivalence_report.md`` under the write root."""
        return self.write_root / REPORT_RELATIVE


def check_write_destination(target: str | os.PathLike[str], paths: RepoPaths) -> Path:
    """Return the resolved form of ``target`` when it is a permitted write destination.

    The data reach and write policy (Agent Action Plan 0.8) applies one rule to every write root,
    with no exception for paths outside the checkout. With ``base = paths.write_root.resolve()``,
    the permitted locations are ``base / OUTPUTS_RELATIVE`` (anything strictly below it) and
    ``base / REPORT_RELATIVE`` (that file only), joined without resolving them further. ``target``
    is resolved with ``Path.resolve()``, which follows every existing symlink in the target and its
    parent directories (a relative target is taken against the current directory), and it is
    accepted only when the resolved path lies strictly below the outputs path or equals the report
    path.

    Everything else raises :class:`~wpct.errors.OutputPathError`, naming the target and its
    resolved path: a protected repository file such as ``whitepapers/WPCT/WPCT-F.07.02.sas``, any
    target that resolves outside the write root, the outputs directory itself, any target whose own
    path or a parent directory is a symlink that makes it resolve outside the permitted locations
    (for example to an external SAS-export CSV), and a target that cannot be resolved.

    Because it compares resolved paths only, this check does not establish that a target is its
    own fixed destination: a fixed export CSV symlinked to another file in the outputs tree, or a
    ``python/wpct/reports`` directory symlinked into the outputs tree, resolves to a permitted
    location and is accepted.

    The writers (``export.write_program_outputs`` and ``report.write_report``) must call this for
    every target of a call before creating any directory or opening any file, and open only the
    returned path, so a single rejection leaves every target of that call unopened and unchanged.
    Before any directory is created they also apply a fixed-target check (``export._vet_target``
    and the same comparison in ``report.write_report``): the returned path must equal the
    unresolved target re-rooted at the resolved write root, or ``OutputPathError`` is raised.
    Together with this function, that refuses every fixed destination reached through a symlink
    below the write root, whether the link leads inside the checkout or outside it. The check
    itself creates, opens and modifies nothing.
    """
    try:
        base = paths.write_root.resolve()
        resolved = Path(target).resolve()
    except (OSError, RuntimeError) as exc:
        # Python 3.10 reports a symlink loop as RuntimeError. A path that cannot be resolved cannot
        # be shown to reach a permitted location, so it is refused.
        raise OutputPathError(
            target, os.path.abspath(target), f"cannot resolve the destination: {exc}"
        ) from exc
    allowed_outputs = base / OUTPUTS_RELATIVE
    allowed_report = base / REPORT_RELATIVE
    if resolved == allowed_report:
        return resolved
    if resolved != allowed_outputs and resolved.is_relative_to(allowed_outputs):
        return resolved
    raise OutputPathError(target, resolved)
