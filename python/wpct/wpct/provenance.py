"""Commit, source-integrity and library facts of a run: the port's only access path to git.

Git metadata and working-tree blob hashes are read only through this module (Agent Action Plan 0.8,
data reach and write policy). Its facts feed the Provenance and Source integrity sections of
``python/wpct/reports/equivalence_report.md`` and ``tests/test_sources_unchanged.py``, which shows
that every SAS-era file the port reads is byte-for-byte unchanged (Agent Action Plan 0.11.2).

Git runs through ``subprocess.run`` with list arguments and no shell, only with read-only
subcommands (``rev-parse``, ``cat-file -e`` and ``hash-object`` without ``-w``), and with no
network access. The module writes nothing. When git, the repository or the upstream commit is
unavailable, every function returns :data:`UNAVAILABLE`, ``None`` or ``False`` instead of raising,
and the integrity rows say so, so the report can state that integrity was not verified.

The reference constants restate the Sources section of ``python/wpct/docs/sas_specification.md``.
Returned facts are deterministic for a given checkout and environment: no timestamps, host names
or absolute paths.
"""

from __future__ import annotations

import importlib.metadata
import platform
import shutil
import subprocess
from collections.abc import Mapping, Sequence
from dataclasses import dataclass, field
from pathlib import Path, PurePath
from types import MappingProxyType

from wpct.paths import RepoPaths

__all__ = [
    "LIBRARIES",
    "READ_ONLY_INPUTS",
    "REFERENCE_COMMIT",
    "REFERENCE_COMMIT_DATE",
    "REFERENCE_NOTE",
    "REFERENCE_SAS_VERSION",
    "STATUS_CHANGED",
    "STATUS_UNCHANGED",
    "UNAVAILABLE",
    "UPSTREAM_COMMIT",
    "IntegrityRow",
    "Provenance",
    "blob_at",
    "collect",
    "commit_available",
    "git_available",
    "head_commit",
    "library_versions",
    "source_integrity",
    "working_blob",
]

#: The upstream commit the port was built against; every specification citation refers to it.
UPSTREAM_COMMIT = "398a6d33ced9359ffb58c46650a6d488811176b1"

#: Commit that last changed the SAS listings and logs in ``whitepapers/WPCT/outputs_sas/``.
REFERENCE_COMMIT = "6fc757d"
REFERENCE_COMMIT_DATE = "2016-02-26"

#: SAS release that produced the references [whitepapers/WPCT/outputs_sas/WPCT-F.07.02.log:2-16].
REFERENCE_SAS_VERSION = "SAS 9.4 TS1M2 with SAS/STAT 13.2 on Windows"

REFERENCE_NOTE = (
    "The SAS listings and logs were last committed in 6fc757d (2016-02-26). The programs were "
    "edited afterwards only in f2bd8d0 (whitepapers/WPCT/WPCT-F.07.02.sas, 2017-01-03) and "
    "1788bb7 (whitepapers/WPCT/WPCT-F.07.07.sas, 2017-02-27). Those edits changed only "
    "environment settings and parameterisation: "
    "git diff -w --ignore-cr-at-eol --ignore-blank-lines 6fc757d HEAD on either program shows "
    "no analysis step changed. The ADVS input files are unchanged since 6c47a90 (2015-12-28)."
)

#: The value reported for a fact git cannot supply.
UNAVAILABLE = "unavailable"

#: Integrity statuses; a row is :data:`UNAVAILABLE` when git or the upstream commit is missing.
STATUS_UNCHANGED = "unchanged"
STATUS_CHANGED = "CHANGED"

#: The direct runtime and test dependencies pinned in ``python/wpct/requirements.txt``.
LIBRARIES = ("numpy", "pandas", "scipy", "statsmodels", "pyreadstat", "pytest")

_NOT_INSTALLED = "not installed"

_GIT_TIMEOUT_SECONDS = 60

#: Every repository input the port opens read-only (Agent Action Plan 0.3.1), repository-relative
#: with forward slashes. Each must hash, byte for byte, to its blob at :data:`UPSTREAM_COMMIT`.
READ_ONLY_INPUTS: tuple[str, ...] = (
    "whitepapers/WPCT/WPCT-F.07.02.sas",
    "whitepapers/WPCT/WPCT-F.07.07.sas",
    "whitepapers/WPCT/WPCT-F.07.02_sas.yml",
    "whitepapers/WPCT/WPCT-F.07.07_sas.yml",
    "whitepapers/utilities/assert_depend.sas",
    "whitepapers/utilities/assert_continue.sas",
    "whitepapers/utilities/assert_var_nonmissing.sas",
    "whitepapers/utilities/assert_unique_keys.sas",
    "whitepapers/utilities/assert_dset_exist.sas",
    "whitepapers/utilities/assert_var_exist.sas",
    "whitepapers/utilities/assert_macro_exist.sas",
    "whitepapers/utilities/util_resolve_sasautos.sas",
    "whitepapers/utilities/util_labels_from_var.sas",
    "whitepapers/utilities/util_count_unique_values.sas",
    "whitepapers/utilities/util_get_var_min_max.sas",
    "whitepapers/utilities/util_value_format.sas",
    "whitepapers/utilities/util_delete_dsets.sas",
    "whitepapers/utilities/util_proc_template.sas",
    "whitepapers/utilities/util_boxplot_block_ranges.sas",
    "whitepapers/utilities/util_axis_order.sas",
    "whitepapers/utilities/util_access_test_data.sas",
    "whitepapers/ADaM/derive_lastminmax_measure.sas",
    "data/adam/cdisc/advs.xpt",
    "data/adam/cdisc-split/advs.xpt",
    "whitepapers/WPCT/outputs_sas/WPCT-F.07.02.lst",
    "whitepapers/WPCT/outputs_sas/WPCT-F.07.02.log",
    "whitepapers/WPCT/outputs_sas/WPCT-F.07.07.lst",
    "whitepapers/WPCT/outputs_sas/WPCT-F.07.07.log",
    (
        "whitepapers/WPCT/outputs_sas/"
        "WPCT-F.07.02_Box_plot_DIABP_Change_by_visit_for_timepoint_815.pdf"
    ),
    (
        "whitepapers/WPCT/outputs_sas/"
        "WPCT-F.07.02_Box_plot_SYSBP_Change_by_visit_for_timepoint_816.pdf"
    ),
    (
        "whitepapers/WPCT/outputs_sas/"
        "WPCT-F.07.07_Box_plot_DIABP_change_MIN_base_post_by_study_for_timepoint_815.pdf"
    ),
    (
        "whitepapers/WPCT/outputs_sas/"
        "WPCT-F.07.07_Box_plot_DIABP_change_MIN_base_post_by_study_for_timepoint_817.pdf"
    ),
    "LICENSE.md",
    "data/adam/README.md",
    "data/adam/cdisc/README.md",
    "data/adam/cdisc-split/README.md",
)


# --- git access -----------------------------------------------------------------------------------


def _root(repo_root: Path | None) -> Path:
    """``repo_root``, or this checkout's root when it is ``None``."""
    return RepoPaths.checkout().repo_root if repo_root is None else Path(repo_root)


def _git(args: Sequence[str], repo_root: Path) -> str | None:
    """Run one read-only git command in ``repo_root``; its stripped stdout on success, else None.

    Returns None when no ``git`` executable is on ``PATH``, when it cannot be started, when it runs
    longer than the timeout, or when it exits non-zero. No shell is involved and stdin is closed,
    so git can neither interpret metacharacters nor wait for input.
    """
    git = shutil.which("git")
    if git is None:
        return None
    try:
        completed = subprocess.run(
            [git, "-C", str(repo_root), *args],
            stdin=subprocess.DEVNULL,
            capture_output=True,
            text=True,
            encoding="utf-8",
            errors="replace",
            check=False,
            timeout=_GIT_TIMEOUT_SECONDS,
        )
    except (OSError, subprocess.SubprocessError):
        return None
    if completed.returncode != 0:
        return None
    return completed.stdout.strip()


def git_available(repo_root: Path | None = None) -> bool:
    """Whether git is on ``PATH`` and ``repo_root`` (default: this checkout) is a repository."""
    return _git(["rev-parse", "--git-dir"], _root(repo_root)) is not None


def head_commit(repo_root: Path | None = None) -> str:
    """The full ``HEAD`` commit of ``repo_root`` (default: this checkout), else ``UNAVAILABLE``."""
    commit = _git(["rev-parse", "--verify", "--quiet", "HEAD"], _root(repo_root))
    return commit if commit else UNAVAILABLE


def commit_available(commit: str, repo_root: Path | None = None) -> bool:
    """Whether ``commit`` names a commit object present in ``repo_root`` (default: this checkout).

    False also when git is unavailable, for instance in a shallow clone lacking the commit.
    """
    return _git(["cat-file", "-e", f"{commit}^{{commit}}"], _root(repo_root)) is not None


def blob_at(commit: str, path: str | PurePath, repo_root: Path | None = None) -> str | None:
    """The object id of repository-relative ``path`` at ``commit``, or None when git cannot name it.

    ``path`` is passed with forward slashes, as git tree paths are written.
    """
    spec = f"{commit}:{PurePath(path).as_posix()}"
    blob = _git(["rev-parse", "--verify", "--quiet", spec], _root(repo_root))
    return blob if blob else None


def working_blob(path: str | PurePath, repo_root: Path | None = None) -> str | None:
    """The blob id the working-tree file ``path`` (relative to ``repo_root``) would have in git.

    The id is computed with ``git hash-object --no-filters`` and is never written to the object
    database (no ``-w``). Returns None when the file is missing or unreadable, or when git is
    unavailable.
    """
    # ``--no-filters`` hashes the file's exact bytes. Without it a user's core.autocrlf or a clean
    # filter would rewrite the content before hashing, so a byte-identical CRLF SAS program could
    # hash differently from its committed blob, or a modified one identically.
    blob = _git(
        ["hash-object", "--no-filters", "--", PurePath(path).as_posix()], _root(repo_root)
    )
    return blob if blob else None


# --- facts ----------------------------------------------------------------------------------------


def library_versions() -> dict[str, str]:
    """Installed version of each package in :data:`LIBRARIES`, in that order.

    A package that is not installed is reported as ``"not installed"``.
    """
    versions: dict[str, str] = {}
    for name in LIBRARIES:
        try:
            versions[name] = importlib.metadata.version(name)
        except importlib.metadata.PackageNotFoundError:
            versions[name] = _NOT_INSTALLED
    return versions


@dataclass(frozen=True)
class IntegrityRow:
    """Integrity of one read-only input against :data:`UPSTREAM_COMMIT`.

    Attributes:
        path: Repository-relative path with forward slashes.
        upstream_blob: Blob id at the upstream commit; None when git cannot name it.
        working_blob: Blob id of the working-tree bytes; None when they cannot be hashed.
        status: :data:`STATUS_UNCHANGED` when both ids exist and are equal; :data:`UNAVAILABLE`
            when git, the repository or the upstream commit is missing, so nothing was verified;
            otherwise :data:`STATUS_CHANGED`, which includes a file missing or unreadable in the
            working tree or absent at the upstream commit.
    """

    path: str
    upstream_blob: str | None
    working_blob: str | None
    status: str


def source_integrity(repo_root: Path | None = None) -> tuple[IntegrityRow, ...]:
    """One :class:`IntegrityRow` per path in :data:`READ_ONLY_INPUTS`, in that order.

    Compares each working-tree file of ``repo_root`` (default: this checkout) with its blob at
    :data:`UPSTREAM_COMMIT`. Every row is :data:`UNAVAILABLE` when git or the commit is missing.
    """
    root = _root(repo_root)
    if not git_available(root) or not commit_available(UPSTREAM_COMMIT, root):
        return tuple(IntegrityRow(path, None, None, UNAVAILABLE) for path in READ_ONLY_INPUTS)
    rows = []
    for path in READ_ONLY_INPUTS:
        upstream = blob_at(UPSTREAM_COMMIT, path, root)
        working = working_blob(path, root)
        # A side that cannot be hashed while git and the commit are available cannot be shown to be
        # unchanged, so it counts as a change rather than as an unverified row.
        same = upstream is not None and upstream == working
        status = STATUS_UNCHANGED if same else STATUS_CHANGED
        rows.append(IntegrityRow(path, upstream, working, status))
    return tuple(rows)


@dataclass(frozen=True)
class Provenance:
    """The provenance facts of one run, as the report's Provenance and Source integrity sections
    print them.

    Attributes:
        upstream_commit: :data:`UPSTREAM_COMMIT`.
        head_commit: ``git rev-parse HEAD`` of the checkout, or :data:`UNAVAILABLE`.
        python_version: ``platform.python_version()``, e.g. ``"3.10.22"``.
        library_versions: :func:`library_versions`, read-only; takes no part in hashing.
        reference_commit, reference_commit_date, reference_sas_version, reference_note: The
            reference constants of this module.
        git_available: Whether git and the repository could be queried.
        integrity: :func:`source_integrity` of the checkout.
    """

    upstream_commit: str
    head_commit: str
    python_version: str
    library_versions: Mapping[str, str] = field(hash=False)
    reference_commit: str
    reference_commit_date: str
    reference_sas_version: str
    reference_note: str
    git_available: bool
    integrity: tuple[IntegrityRow, ...]

    @property
    def integrity_verified(self) -> bool:
        """Whether every input was checked against the upstream commit (no row unavailable)."""
        return bool(self.integrity) and all(row.status != UNAVAILABLE for row in self.integrity)


def collect(paths: RepoPaths) -> Provenance:
    """Gather the provenance facts of the checkout at ``paths.repo_root``.

    Reads git metadata and hashes the read-only inputs; writes nothing.
    """
    root = paths.repo_root
    return Provenance(
        upstream_commit=UPSTREAM_COMMIT,
        head_commit=head_commit(root),
        python_version=platform.python_version(),
        library_versions=MappingProxyType(library_versions()),
        reference_commit=REFERENCE_COMMIT,
        reference_commit_date=REFERENCE_COMMIT_DATE,
        reference_sas_version=REFERENCE_SAS_VERSION,
        reference_note=REFERENCE_NOTE,
        git_available=git_available(root),
        integrity=source_integrity(root),
    )
