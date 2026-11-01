"""Commit, source-integrity and library facts of a run: the port's only access path to git.

Git metadata and working-tree blob hashes are read only through this module (Agent Action Plan 0.8,
data reach and write policy). Its facts feed the Provenance and Source integrity sections of
``python/wpct/reports/equivalence_report.md`` and ``tests/test_sources_unchanged.py``, which shows
that every SAS-era file the port reads is byte-for-byte unchanged (Agent Action Plan 0.11.2).

Git runs through ``subprocess.run`` with list arguments and no shell, only with read-only
subcommands (``rev-parse``, ``cat-file -e`` and ``hash-object`` without ``-w``), and with no
network access. The git commands of one :func:`collect`, or of one call of a public git function,
share one time budget, and each command has a timeout of its own. Once git times out, exhausts the
budget, cannot be started or exits 0 with output that is not what the command prints (an object id
that is not 40 or 64 hex digits, for instance), it is treated as unavailable and not run again, so
a slow, hanging or misbehaving git cannot stall the run. The module writes nothing. When git, the
repository or the upstream commit is unavailable, every function returns :data:`UNAVAILABLE`,
``None`` or ``False`` instead of raising, and the integrity rows say so, so the report can state
that integrity was not verified.

The reference constants restate the Sources section of ``python/wpct/docs/sas_specification.md``.
Returned facts are deterministic for a given checkout and environment: no timestamps, host names
or absolute paths.
"""

from __future__ import annotations

import importlib.metadata
import platform
import re
import shutil
import subprocess
import time
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

#: Integrity statuses; a row is :data:`UNAVAILABLE` when git or the upstream commit is missing or
#: git became unusable before the row was verified.
STATUS_UNCHANGED = "unchanged"
STATUS_CHANGED = "CHANGED"

#: The direct runtime and test dependencies pinned in ``python/wpct/requirements.txt``.
LIBRARIES = ("numpy", "pandas", "scipy", "statsmodels", "pyreadstat", "pytest")

_NOT_INSTALLED = "not installed"

#: Longest a single git command may run before it is killed.
_GIT_CALL_TIMEOUT_SECONDS = 10

#: Time budget shared by every git command of one :class:`_GitSession`.
_GIT_DEADLINE_SECONDS = 30

#: A git object id as git prints it: SHA-1 (40 hex digits) or SHA-256 (64 hex digits).
_OBJECT_ID = re.compile(r"[0-9a-f]{40}|[0-9a-f]{64}")

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


class _GitSession:
    """The git commands of one run in one repository: one time budget, one verdict on git.

    The session is ``broken`` once git turns out to be unusable: no ``git`` is on ``PATH``, git
    cannot be started, a command outlives its timeout or the session's budget, or git exits 0 with
    output that is not what the command prints. From then on every command returns None at once
    without starting a process. A non-zero exit is an answer (no such object, not a repository) and
    leaves the session usable.
    """

    def __init__(self, repo_root: Path) -> None:
        self.repo_root = repo_root
        self._git = shutil.which("git")
        self._deadline = time.monotonic() + _GIT_DEADLINE_SECONDS
        self.broken = self._git is None

    def run(self, args: Sequence[str]) -> str | None:
        """Run one read-only git command in the repository; its stripped stdout on exit 0.

        Returns None when git exits non-zero, and when the session is broken or this command
        breaks it: git cannot be started, or the command runs longer than
        ``_GIT_CALL_TIMEOUT_SECONDS`` or than what is left of the session's budget. A command that
        times out, or is interrupted, is killed. No shell is involved and stdin is closed, so git
        can neither interpret metacharacters nor wait for input.
        """
        if self.broken or self._git is None:
            return None
        remaining = self._deadline - time.monotonic()
        if remaining <= 0:
            self.broken = True
            return None
        # On a timeout ``subprocess.run`` kills git itself, not processes git started, so the
        # session starts no git after the first timeout: a slow or hanging git costs one timed-out
        # command per run. Git stays in the caller's process group, so a Ctrl-C or a signal sent
        # to that group (by an outer ``timeout``, for instance) also reaches anything git started.
        try:
            completed = subprocess.run(
                [self._git, "-C", str(self.repo_root), *args],
                stdin=subprocess.DEVNULL,
                capture_output=True,
                text=True,
                encoding="utf-8",
                errors="replace",
                check=False,
                timeout=min(_GIT_CALL_TIMEOUT_SECONDS, remaining),
            )
        except (OSError, subprocess.SubprocessError):  # Includes ``TimeoutExpired``.
            self.broken = True
            return None
        if completed.returncode != 0:
            return None
        return completed.stdout.strip()

    def object_id(self, args: Sequence[str]) -> str | None:
        """Run ``args``, a command that prints one object id when it succeeds; that id, or None.

        Output on exit 0 that is not an object id breaks the session: a git that answers so cannot
        be trusted with the rest of the run either.
        """
        output = self.run(args)
        if output is None:
            return None
        if _OBJECT_ID.fullmatch(output) is None:
            self.broken = True
            return None
        return output


def _git_available(session: _GitSession) -> bool:
    return session.run(["rev-parse", "--git-dir"]) is not None


def git_available(repo_root: Path | None = None) -> bool:
    """Whether git is on ``PATH`` and ``repo_root`` (default: this checkout) is a repository."""
    return _git_available(_GitSession(_root(repo_root)))


def _head_commit(session: _GitSession) -> str:
    commit = session.object_id(["rev-parse", "--verify", "--quiet", "HEAD"])
    return UNAVAILABLE if commit is None else commit


def head_commit(repo_root: Path | None = None) -> str:
    """The full ``HEAD`` commit of ``repo_root`` (default: this checkout), else ``UNAVAILABLE``.

    ``UNAVAILABLE`` also when git answers with something that is not an object id.
    """
    return _head_commit(_GitSession(_root(repo_root)))


def _commit_available(session: _GitSession, commit: str) -> bool:
    output = session.run(["cat-file", "-e", f"{commit}^{{commit}}"])
    if output is None:
        return False
    if output:
        # ``cat-file -e`` prints nothing when the object exists, so this git cannot be trusted.
        session.broken = True
        return False
    return True


def commit_available(commit: str, repo_root: Path | None = None) -> bool:
    """Whether ``commit`` names a commit object present in ``repo_root`` (default: this checkout).

    False also when git is unavailable, for instance in a shallow clone lacking the commit, and
    when git reports success but prints something.
    """
    return _commit_available(_GitSession(_root(repo_root)), commit)


def _blob_at(session: _GitSession, commit: str, path: str | PurePath) -> str | None:
    spec = f"{commit}:{PurePath(path).as_posix()}"
    return session.object_id(["rev-parse", "--verify", "--quiet", spec])


def blob_at(commit: str, path: str | PurePath, repo_root: Path | None = None) -> str | None:
    """The object id of repository-relative ``path`` at ``commit``, or None when git cannot name it.

    ``path`` is passed with forward slashes, as git tree paths are written. None also when git
    answers with something that is not an object id.
    """
    return _blob_at(_GitSession(_root(repo_root)), commit, path)


def _working_blob(session: _GitSession, path: str | PurePath) -> str | None:
    # ``--no-filters`` hashes the file's exact bytes. Without it a user's core.autocrlf or a clean
    # filter would rewrite the content before hashing, so a byte-identical CRLF SAS program could
    # hash differently from its committed blob, or a modified one identically.
    return session.object_id(["hash-object", "--no-filters", "--", PurePath(path).as_posix()])


def working_blob(path: str | PurePath, repo_root: Path | None = None) -> str | None:
    """The blob id the working-tree file ``path`` (relative to ``repo_root``) would have in git.

    The id is computed with ``git hash-object --no-filters`` and is never written to the object
    database (no ``-w``). Returns None when the file is missing or unreadable, when git is
    unavailable, or when git answers with something that is not an object id.
    """
    return _working_blob(_GitSession(_root(repo_root)), path)


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
            when nothing was verified: git, the repository or the upstream commit is missing, or
            git became unusable (it timed out, ran out of its time budget, could not be started
            or answered with something other than an object id) during this row's lookups or
            before them. Such a row keeps any valid blob id obtained before that. Otherwise
            :data:`STATUS_CHANGED`, which includes a file missing or unreadable in the working
            tree or absent at the upstream commit.
    """

    path: str
    upstream_blob: str | None
    working_blob: str | None
    status: str


def _source_integrity(session: _GitSession, git_ok: bool) -> tuple[IntegrityRow, ...]:
    if not git_ok or not _commit_available(session, UPSTREAM_COMMIT):
        return tuple(IntegrityRow(path, None, None, UNAVAILABLE) for path in READ_ONLY_INPUTS)
    rows = []
    for path in READ_ONLY_INPUTS:
        upstream = _blob_at(session, UPSTREAM_COMMIT, path)
        working = _working_blob(session, path)
        if session.broken:
            # Git became unusable during this row's lookups or before them, so the row shows
            # neither a match nor a change; once broken, the session runs no further command.
            status = UNAVAILABLE
        else:
            # A side that cannot be hashed while git and the commit are available cannot be shown
            # to be unchanged, so it counts as a change rather than as an unverified row.
            same = upstream is not None and upstream == working
            status = STATUS_UNCHANGED if same else STATUS_CHANGED
        rows.append(IntegrityRow(path, upstream, working, status))
    return tuple(rows)


def source_integrity(repo_root: Path | None = None) -> tuple[IntegrityRow, ...]:
    """One :class:`IntegrityRow` per path in :data:`READ_ONLY_INPUTS`, in that order.

    Compares each working-tree file of ``repo_root`` (default: this checkout) with its blob at
    :data:`UPSTREAM_COMMIT`. Every row is :data:`UNAVAILABLE` when git or the commit is missing,
    and every row from the one where git became unusable onwards.
    """
    session = _GitSession(_root(repo_root))
    return _source_integrity(session, _git_available(session))


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
        git_available: Whether git and the repository could be queried for every fact. False
            also when git timed out, ran out of its time budget, could not be started or
            answered with something other than an object id at any point of the run.
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

    Reads git metadata and hashes the read-only inputs; writes nothing. Every git command of the
    run shares one :class:`_GitSession`, so the run's git time is bounded by one budget and git
    stops being queried as soon as it proves unusable.
    """
    session = _GitSession(paths.repo_root)
    head = _head_commit(session)
    available = _git_available(session)
    integrity = _source_integrity(session, available)
    return Provenance(
        upstream_commit=UPSTREAM_COMMIT,
        head_commit=head,
        python_version=platform.python_version(),
        library_versions=MappingProxyType(library_versions()),
        reference_commit=REFERENCE_COMMIT,
        reference_commit_date=REFERENCE_COMMIT_DATE,
        reference_sas_version=REFERENCE_SAS_VERSION,
        reference_note=REFERENCE_NOTE,
        git_available=available and not session.broken,
        integrity=integrity,
    )
