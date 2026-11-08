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
resolved-path rule of that data reach and write policy (Agent Action Plan 0.8).
:func:`vet_fixed_destination`, which both writers call for every target of a call before they
create a directory or open a file, applies that rule and also requires the target to be its own
fixed destination, reached through no symlink below the write root, with every existing entry on
its way a directory and an existing destination a regular file with a single hard link.
:func:`open_fixed_destination` then opens each target through directory descriptors without
following a symlink, and checks the opened file again before truncating it. Where the platform
cannot open a file relative to a directory descriptor, as on Windows, it opens the target by path
and, before truncating it, requires the path the operating system reports for the opened file to be
the target's fixed destination, refusing the write when the platform reports none.

Importing this module touches no file: paths are resolved only when a constructor or the guard runs.
"""

from __future__ import annotations

import errno
import os
import stat
from dataclasses import dataclass
from pathlib import Path
from typing import BinaryIO

from wpct.errors import OutputPathError
from wpct.schemas import PROGRAM_07_02, PROGRAM_07_07

__all__ = [
    "OUTPUTS_RELATIVE",
    "REPORT_RELATIVE",
    "RepoPaths",
    "check_write_destination",
    "open_fixed_destination",
    "vet_fixed_destination",
]

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

#: Whether :func:`open_fixed_destination` binds a write to directory descriptors: ``os.open``,
#: ``os.mkdir`` and a no-follow ``os.stat`` accept ``dir_fd``, and the ``open`` flags it needs
#: exist, as on Linux and macOS. Elsewhere, as on Windows, it opens the file by path and checks the
#: path the operating system reports for the opened file before truncating it.
_DIR_FD_WRITES = (
    {os.open, os.mkdir, os.stat} <= os.supports_dir_fd
    and os.stat in os.supports_follow_symlinks
    and all(hasattr(os, flag) for flag in ("O_DIRECTORY", "O_NOFOLLOW", "O_NONBLOCK", "O_CLOEXEC"))
)

#: ``open`` errors meaning that the last path component is a symlink under ``O_NOFOLLOW``:
#: ``ELOOP`` on Linux and macOS, ``EMLINK`` on FreeBSD.
_SYMLINK_ERRNOS = frozenset({errno.ELOOP, errno.EMLINK})

#: Names of the file types other than a regular file or a symlink, for refusal messages.
_ENTRY_KINDS = (
    (stat.S_ISDIR, "a directory"),
    (stat.S_ISFIFO, "a FIFO"),
    (stat.S_ISSOCK, "a socket"),
    (stat.S_ISCHR, "a character device"),
    (stat.S_ISBLK, "a block device"),
)


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
    (for example to an external SAS-export CSV), and a target that cannot be resolved, or whose
    write root cannot be, such as one holding a NUL byte.

    Because it compares resolved paths only, this check does not establish that a target is its
    own fixed destination: a fixed export CSV symlinked to another file in the outputs tree, or a
    ``python/wpct/reports`` directory symlinked into the outputs tree, resolves to a permitted
    location and is accepted.

    The writers (``export.write_program_outputs`` and ``report.write_report``) reach this check
    through :func:`vet_fixed_destination`, which they call for every target of a call before
    creating any directory or opening any file, so a single rejection leaves every target of that
    call unopened and unchanged. That function also requires the returned path to equal the
    unresolved target re-rooted at the resolved write root, which together with this check refuses
    every fixed destination reached through a symlink below the write root, whether the link leads
    inside the checkout or outside it, and it refuses an existing destination that is not a regular
    file with a single hard link. The check itself creates, opens and modifies nothing.
    """
    try:
        base = paths.write_root.resolve()
        resolved = Path(target).resolve()
    except (OSError, RuntimeError, ValueError) as exc:
        # Python 3.10 reports a symlink loop as RuntimeError, and a NUL byte in the target or the
        # write root as ValueError, since no file name can hold one. A path that cannot be resolved
        # cannot be shown to reach a permitted location, so it is refused.
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


def vet_fixed_destination(target: str | os.PathLike[str], paths: RepoPaths) -> Path:
    """Return the resolved ``target`` once it is shown to be its own fixed write destination.

    The writers call this for every target of a call before they create a directory or open a file
    (Agent Action Plan 0.8), so one refusal leaves every target of the call unopened and unchanged.
    It raises :class:`~wpct.errors.OutputPathError`, naming the target and its resolved path:

    - when :func:`check_write_destination` refuses the target;
    - when the target is not joined below ``paths.write_root``, or its resolved path is not that
      target re-rooted at the resolved write root, i.e. the target or a directory between it and
      the write root is a symlink, even one that leads elsewhere inside the outputs tree. Symlinks
      at or above the write root itself are followed;
    - when an existing entry between the resolved write root and the target is not a directory;
    - when the target exists and is not a regular file (a FIFO, a directory, a socket or a
      device), or is a regular file with more than one hard link, since writing it would change the
      file another path names, such as a SAS-era file or an external SAS export;
    - when one of those entries cannot be inspected: ``os.lstat`` fails for a reason other than a
      missing entry.

    Entries are inspected with ``os.lstat`` alone, so this creates, opens and modifies nothing; a
    missing entry is left for :func:`open_fixed_destination` to create.
    """
    resolved = check_write_destination(target, paths)
    base, parts = _fixed_parts(target, paths)
    expected = base.joinpath(*parts)
    # check_write_destination accepts any resolved path below the outputs tree, so a symlink there
    # that leads to another file in the tree would pass it. The write policy refuses every fixed
    # destination reached through a symlink, inside the checkout or outside it, so the resolved path
    # must be the unresolved target re-rooted at the resolved write root. Only symlinks at or above
    # the write root itself are followed.
    if resolved != expected:
        raise OutputPathError(target, resolved, _symlink_reason(expected))
    # Resolving follows symlinks but not hard links, and says nothing about the file type, so the
    # entries the write would pass through or replace are inspected as well.
    entry = base
    for name in parts[:-1]:
        entry = entry / name
        info = _lstat_or_none(entry, target, resolved)
        if info is None:
            return resolved
        reason = _parent_refusal(info, expected, entry)
        if reason is not None:
            raise OutputPathError(target, resolved, reason)
    info = _lstat_or_none(expected, target, resolved)
    if info is not None:
        reason = _file_refusal(info, expected)
        if reason is not None:
            raise OutputPathError(target, resolved, reason)
    return resolved


def open_fixed_destination(target: str | os.PathLike[str], paths: RepoPaths) -> BinaryIO:
    """Open the fixed destination ``target`` for writing and return it, truncated, as a binary file.

    Call it for a target :func:`vet_fixed_destination` accepted, once every target of the call has
    passed that check. The open is bound to the directories the check inspected rather than to a
    second lookup of the path, so an entry replaced after the check cannot redirect the write
    (CWE-367):

    - The resolved write root is created when missing, with its parents, and opened as a
      directory. Symlinks at or above it are followed, as the write policy allows.
    - Each directory between it and the target is opened relative to its parent's descriptor
      without following a symlink (``O_NOFOLLOW``), and created there when missing.
    - The target is opened in the last directory with ``O_NOFOLLOW`` and ``O_NONBLOCK``, created
      when missing and not truncated. Only once the opened file is a regular file with a single
      hard link is it truncated.

    A symlink, a parent that is not a directory, a FIFO, a directory or another special file, or a
    hard link met on the way raises :class:`~wpct.errors.OutputPathError` naming the target and
    its resolved path at that moment, before the target is truncated; every descriptor is closed,
    and directories already created on the way stay. Any other ``OSError``, such as ``EACCES`` or
    ``ENOSPC``, propagates.

    Where ``os.open`` and ``os.mkdir`` take no ``dir_fd`` or those flags are missing, as on Windows,
    each directory is created by path when missing and checked with ``os.lstat``, and the target is
    then opened by path without truncation, with ``O_NOFOLLOW`` and ``O_NONBLOCK`` where they exist.
    Before it is truncated, the opened file must be a regular file with a single hard link, and the
    path the operating system reports for that open file (``GetFinalPathNameByHandleW`` on
    Windows) must be the target's fixed destination. A file opened elsewhere because the target or a
    directory on its way was replaced after the check, or one the platform reports no path for,
    raises :class:`~wpct.errors.OutputPathError` and is closed untruncated, so a file found
    anywhere but at the destination is never truncated or written. Such a swap can still leave
    behind an empty new file or directory where the replaced entry led, since the open and the
    directory creation follow it.

    The returned file object is a context manager; closing it closes the descriptor.
    """
    base, parts = _fixed_parts(target, paths)
    expected = base.joinpath(*parts)
    base.mkdir(parents=True, exist_ok=True)
    if _DIR_FD_WRITES:
        descriptor = _open_through_descriptors(target, base, parts, expected)
    else:
        descriptor = _open_by_path(target, base, parts, expected)
    try:
        if getattr(os, "O_NONBLOCK", 0):
            # O_NONBLOCK only kept the open from waiting on a FIFO; a regular file is written as
            # usual.
            os.set_blocking(descriptor, True)
        os.ftruncate(descriptor, 0)
        return os.fdopen(descriptor, "wb")
    except BaseException:
        os.close(descriptor)
        raise


def _fixed_parts(target: str | os.PathLike[str], paths: RepoPaths) -> tuple[Path, tuple[str, ...]]:
    """Return the resolved write root and the components of ``target`` below ``paths.write_root``.

    A target that is not joined below the write root, is the write root itself or steps up with
    ``..`` cannot be a fixed destination, so it raises :class:`~wpct.errors.OutputPathError`, as
    does a write root that cannot be resolved.
    """
    try:
        base = paths.write_root.resolve()
    except (OSError, RuntimeError, ValueError) as exc:
        raise OutputPathError(
            target, _resolved_now(target), f"cannot resolve the write root: {exc}"
        ) from exc
    candidate = Path(target)
    parts: tuple[str, ...] = ()
    if candidate.is_relative_to(paths.write_root):
        parts = candidate.relative_to(paths.write_root).parts
    if not parts or os.pardir in parts:
        raise OutputPathError(
            target,
            _resolved_now(target),
            f"the target is not a fixed destination joined below the write root {paths.write_root}",
        )
    return base, parts


def _open_through_descriptors(
    target: str | os.PathLike[str], base: Path, parts: tuple[str, ...], expected: Path
) -> int:
    """Open ``expected`` component by component from a descriptor of ``base``, never following a
    symlink below it, and return the file's descriptor once it is a regular file with a single
    hard link. Nothing is truncated here."""
    directory_flags = os.O_RDONLY | os.O_DIRECTORY | os.O_NOFOLLOW | os.O_CLOEXEC
    file_flags = os.O_WRONLY | os.O_CREAT | os.O_NOFOLLOW | os.O_NONBLOCK | os.O_CLOEXEC
    descriptors = [os.open(base, os.O_RDONLY | os.O_DIRECTORY | os.O_CLOEXEC)]
    try:
        entry = base
        for name in parts[:-1]:
            entry = entry / name
            try:
                child = _open_entry(name, directory_flags, descriptors[-1], target, expected, entry)
            except FileNotFoundError:
                try:
                    os.mkdir(name, dir_fd=descriptors[-1])
                except FileExistsError:
                    # Created by another process since the open above; the no-follow open below
                    # checks whatever is there now.
                    pass
                child = _open_entry(name, directory_flags, descriptors[-1], target, expected, entry)
            descriptors.append(child)
        descriptor = _open_entry(parts[-1], file_flags, descriptors[-1], target, expected, entry)
    finally:
        for directory in descriptors:
            os.close(directory)
    try:
        _require_writable_file(descriptor, target, expected)
    except BaseException:
        os.close(descriptor)
        raise
    return descriptor


def _open_by_path(
    target: str | os.PathLike[str], base: Path, parts: tuple[str, ...], expected: Path
) -> int:
    """Fallback of :func:`_open_through_descriptors` where ``dir_fd`` opens are unavailable.

    Each directory below ``base`` is created by path when missing and checked with ``os.lstat``;
    ``expected`` is then opened by path without truncation. A directory on the way can still be
    swapped between those lookups, so the opened file itself is checked, never a further lookup of
    the path: the descriptor is returned only once it is a regular file with a single hard link
    and the path the operating system reports for it (:func:`_opened_path`) is ``expected``. When
    that path differs, or the platform reports none, the descriptor is closed and
    :class:`~wpct.errors.OutputPathError` is raised. Nothing is truncated here.
    """
    _check_parents_by_path(target, base, parts, expected)
    flags = (
        os.O_WRONLY
        | os.O_CREAT
        | getattr(os, "O_BINARY", 0)
        | getattr(os, "O_NOFOLLOW", 0)
        | getattr(os, "O_NONBLOCK", 0)
    )
    try:
        descriptor = os.open(expected, flags, 0o666)
    except OSError as exc:
        _refuse_open_error(exc, target, expected, expected.parent)
        raise
    try:
        _require_writable_file(descriptor, target, expected)
        opened = _opened_path(descriptor)
        if opened is None:
            reason = (
                f"cannot bind the write to the fixed destination {expected}: the platform reports "
                "no path for the opened file, so it cannot be shown to be that destination"
            )
        elif _same_path(opened, expected):
            reason = None
        else:
            reason = (
                f"the fixed destination {expected} was opened at {opened}, not at itself: it or a "
                "directory on its way was replaced while it was being opened"
            )
        if reason is not None:
            raise OutputPathError(target, _resolved_now(target), reason)
    except BaseException:
        os.close(descriptor)
        raise
    return descriptor


def _same_path(opened: str, expected: Path) -> bool:
    """Whether ``opened``, a path the operating system reported, is ``expected``.

    Both are compared after ``os.path.normpath`` and ``os.path.normcase``, so on Windows a
    difference in letter case or separators alone does not count.
    """
    return os.path.normcase(os.path.normpath(opened)) == os.path.normcase(
        os.path.normpath(str(expected))
    )


def _opened_path(descriptor: int) -> str | None:
    """The path the operating system reports for the open file ``descriptor``, or ``None``.

    The path is that of the opened file itself, as it stands when asked, with no symlink in it:
    ``GetFinalPathNameByHandleW`` on Windows, ``/proc/self/fd/<descriptor>`` where that exists, as
    on Linux, and ``fcntl.F_GETPATH`` where that exists, as on macOS. ``None`` means the platform
    offers none of these, or the one it offers failed, so the file cannot be located.
    """
    if os.name == "nt":
        return _windows_opened_path(descriptor)
    proc_fd = "/proc/self/fd"
    if os.path.isdir(proc_fd):
        try:
            return os.readlink(f"{proc_fd}/{descriptor}")
        except OSError:
            return None
    try:
        import fcntl
    except ImportError:
        return None
    get_path = getattr(fcntl, "F_GETPATH", None)
    if get_path is None:
        return None
    try:
        # F_GETPATH fills a MAXPATHLEN (1024-byte) buffer with a NUL-terminated path.
        raw = fcntl.fcntl(descriptor, get_path, bytes(1024))
    except OSError:
        return None
    path = raw.split(b"\0", 1)[0]
    return os.fsdecode(path) if path else None


def _windows_opened_path(descriptor: int) -> str | None:
    """:func:`_opened_path` on Windows, through ``GetFinalPathNameByHandleW``.

    The path is asked for normalized and with a drive letter (``FILE_NAME_NORMALIZED`` and
    ``VOLUME_NAME_DOS``, both 0), in a buffer grown until it fits. Its ``\\\\?\\`` prefix is
    removed, and ``\\\\?\\UNC\\`` becomes ``\\\\``, the form ``Path.resolve()`` gives.
    """
    try:
        import ctypes
        import msvcrt
        from ctypes import wintypes
    except ImportError:
        return None
    try:
        handle = msvcrt.get_osfhandle(descriptor)
    except OSError:
        return None
    get_final_path = ctypes.WinDLL("kernel32").GetFinalPathNameByHandleW
    get_final_path.argtypes = (wintypes.HANDLE, wintypes.LPWSTR, wintypes.DWORD, wintypes.DWORD)
    get_final_path.restype = wintypes.DWORD
    size = 260
    while True:
        buffer = ctypes.create_unicode_buffer(size)
        length = get_final_path(handle, buffer, size, 0)
        if length == 0:
            return None
        if length < size:
            break
        # Too small: the return value is the size needed, the terminating NUL included. Doubling
        # at the least ends the loop even if a call returned no more than the size offered.
        size = max(length, 2 * size)
    path = buffer.value
    if path.startswith("\\\\?\\UNC\\"):
        return "\\\\" + path[len("\\\\?\\UNC\\") :]
    if path.startswith("\\\\?\\"):
        return path[len("\\\\?\\") :]
    return path


def _check_parents_by_path(
    target: str | os.PathLike[str], base: Path, parts: tuple[str, ...], expected: Path
) -> None:
    """Create by path every missing directory between ``base`` and ``expected``, and require each
    to be a directory, not a symlink."""
    entry = base
    for name in parts[:-1]:
        entry = entry / name
        try:
            info = os.lstat(entry)
        except FileNotFoundError:
            try:
                os.mkdir(entry)
            except FileExistsError:
                # Created by another process since the lstat above; the lstat below checks it.
                pass
            info = os.lstat(entry)
        reason = _parent_refusal(info, expected, entry)
        if reason is not None:
            raise OutputPathError(target, _resolved_now(target), reason)


def _open_entry(
    name: str,
    flags: int,
    directory: int,
    target: str | os.PathLike[str],
    expected: Path,
    entry: Path,
) -> int:
    """``os.open`` the entry ``name`` of the directory descriptor ``directory`` with ``flags``.

    An error showing an unsafe entry becomes :class:`~wpct.errors.OutputPathError`
    (:func:`_refuse_open_error`). ``entry`` is the path a not-a-directory refusal names: ``name``
    itself when a directory is being opened, else the directory holding it. Any other ``OSError``
    propagates.
    """
    try:
        return os.open(name, flags, 0o666, dir_fd=directory)
    except OSError as exc:
        if exc.errno == errno.ENOTDIR and _is_symlink_in(directory, name):
            # Linux reports an O_DIRECTORY | O_NOFOLLOW open of a symlink as ENOTDIR, not ELOOP.
            raise OutputPathError(target, _resolved_now(target), _symlink_reason(expected)) from exc
        _refuse_open_error(exc, target, expected, entry)
        raise


def _is_symlink_in(directory: int, name: str) -> bool:
    """Whether the entry ``name`` of the directory descriptor ``directory`` is a symlink now."""
    try:
        info = os.stat(name, dir_fd=directory, follow_symlinks=False)
    except OSError:
        return False
    return stat.S_ISLNK(info.st_mode)


def _refuse_open_error(
    exc: OSError, target: str | os.PathLike[str], expected: Path, entry: Path
) -> None:
    """Raise :class:`~wpct.errors.OutputPathError` from ``exc`` when it shows an unsafe entry.

    ``ELOOP`` or ``EMLINK`` under ``O_NOFOLLOW`` means a symlink, ``ENOTDIR`` that ``entry`` is not
    a directory, ``EISDIR`` a directory at the target, and ``ENXIO`` a FIFO without a reader, a
    socket or a device without a driver at the target. Any other error returns, for the caller to
    re-raise.
    """
    if exc.errno in _SYMLINK_ERRNOS:
        reason = _symlink_reason(expected)
    elif exc.errno == errno.ENOTDIR:
        reason = f"the fixed destination {expected} lies below {entry}, which is not a directory"
    elif exc.errno == errno.EISDIR:
        reason = f"the fixed destination {expected} is a directory, not a regular file"
    elif exc.errno == errno.ENXIO:
        reason = f"the fixed destination {expected} is a special file, not a regular file"
    else:
        return
    raise OutputPathError(target, _resolved_now(target), reason) from exc


def _require_writable_file(
    descriptor: int, target: str | os.PathLike[str], expected: Path
) -> os.stat_result:
    """Return the ``fstat`` of ``descriptor`` once it is a regular file with a single hard link."""
    info = os.fstat(descriptor)
    reason = _file_refusal(info, expected)
    if reason is not None:
        raise OutputPathError(target, _resolved_now(target), reason)
    return info


def _lstat_or_none(
    entry: Path, target: str | os.PathLike[str], resolved: Path
) -> os.stat_result | None:
    """``os.lstat`` of ``entry``, or ``None`` when it does not exist.

    Any other failure raises :class:`~wpct.errors.OutputPathError`: an entry that cannot be
    inspected cannot be shown to be safe to write.
    """
    try:
        return os.lstat(entry)
    except FileNotFoundError:
        return None
    except OSError as exc:
        raise OutputPathError(target, resolved, f"cannot inspect {entry}: {exc}") from exc


def _parent_refusal(info: os.stat_result, expected: Path, entry: Path) -> str | None:
    """Why ``entry``, a directory on the way to ``expected`` described by ``info``, is unsafe.

    ``None`` when it is a directory and not a symlink.
    """
    if stat.S_ISLNK(info.st_mode):
        return _symlink_reason(expected)
    if not stat.S_ISDIR(info.st_mode):
        return f"the fixed destination {expected} lies below {entry}, which is not a directory"
    return None


def _file_refusal(info: os.stat_result, expected: Path) -> str | None:
    """Why the existing destination ``expected``, described by ``info``, is unsafe to write.

    ``None`` when it is safe: a regular file, not a symlink or a special file, with exactly one
    hard link. Writing a file with more links would change the file that another path names, and
    a file with none was unlinked while it was being opened.
    """
    mode = info.st_mode
    if stat.S_ISLNK(mode):
        return _symlink_reason(expected)
    if not stat.S_ISREG(mode):
        kind = next((name for test, name in _ENTRY_KINDS if test(mode)), "a special file")
        return f"the fixed destination {expected} is {kind}, not a regular file"
    if info.st_nlink > 1:
        return (
            f"the fixed destination {expected} is a hard link shared with "
            f"{info.st_nlink - 1} other path(s), which writing it would change"
        )
    if info.st_nlink < 1:
        return f"the fixed destination {expected} was removed while it was being opened"
    return None


def _symlink_reason(expected: Path) -> str:
    """The refusal reason for a fixed destination reached through a symlink below the write root."""
    return f"the fixed destination {expected} is reached through a symlink"


def _resolved_now(target: str | os.PathLike[str]) -> Path:
    """``target`` resolved at this moment, or its absolute form when it cannot be resolved."""
    try:
        return Path(target).resolve()
    except (OSError, RuntimeError, ValueError):
        return Path(os.path.abspath(target))
