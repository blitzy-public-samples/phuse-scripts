"""Exception types shared by every module of the WPCT port.

This module is the only definition of the package's exceptions; other modules
import them and never define their own. It imports nothing from the package,
so any module may depend on it.

* ``InputDataError`` ports the failures of ``%util_access_test_data`` and the
  ``vars=`` check of ``%assert_depend``, and the halt ``%assert_continue`` makes
  after it: a missing input file, an unreadable SAS transport file or one whose
  first member is not ``ADVS``, a dataset lacking required variables, a dataset
  storing a required variable under more than one spelling, in columns that
  differ only in case, or a dataset storing a required variable with the other
  SAS type, which the port checks beyond ``%assert_depend``
  (S02-DEP-util_access_test_data, S07-DEP-util_access_test_data,
  S02-DEP-assert_depend, S07-DEP-assert_depend; S02-DEP-assert_continue and
  S07-DEP-assert_continue for the call after the dependency assertion
  [whitepapers/WPCT/WPCT-F.07.02.sas:158; whitepapers/WPCT/WPCT-F.07.07.sas:204]).
  It also reports a missing or unreadable SAS listing or log, a SAS export that
  is present but unreadable (a broken or looping symlink included) or
  malformed, and a SAS export directory that exists but is not a directory or
  is a broken or looping symlink.
* ``AnalysisHalted`` ports ``%assert_continue`` forcing syntax-checking mode
  after a failed data-content assertion (S07-DEP-assert_continue
  [whitepapers/WPCT/WPCT-F.07.07.sas:220,245]): the data halts S07-FLT-04
  (missing ``CHG``) and S07-FLT-05 (duplicate keys). A driver raising it
  exports nothing.
* ``OutputPathError`` reports a target refused by the write policy: the package
  writes only below ``python/wpct/outputs/`` and to
  ``python/wpct/reports/equivalence_report.md`` under the write root, so every
  other target, including any fixed destination reached through a symlink
  below the write root or one that exists as anything but a regular file with
  a single hard link, is refused before it is opened. A destination that
  becomes one of those after that check is refused as it is opened, before it
  is truncated.

``message_path`` renders a path for an error message on one line; the
``InputDataError`` messages of ``sas_reference`` and the command line's
``--sas-exports`` checks show their paths through it.
"""

from __future__ import annotations

import functools
import os
import unicodedata
from pathlib import Path

__all__ = ["AnalysisHalted", "InputDataError", "OutputPathError", "WpctError", "message_path"]

_PERMITTED_DESTINATIONS = (
    "only python/wpct/outputs/** and python/wpct/reports/equivalence_report.md "
    "under the write root are permitted"
)

#: Unicode categories ``message_path`` escapes: control characters, which hold
#: every line break but U+2028 and U+2029; those two separators; and the lone
#: surrogates that stand for undecodable file-name bytes.
_ESCAPED_CATEGORIES = frozenset({"Cc", "Cs", "Zl", "Zp"})


def message_path(path: str | os.PathLike[str]) -> str:
    """Return ``path`` as text for a one-line message, control characters escaped.

    Each character of Unicode category Cc, Cs, Zl or Zp is replaced by its Python
    escape: a line break becomes ``\\n``, a carriage return ``\\r``, a tab
    ``\\t``, ESC ``\\x1b``, U+2028 ``\\u2028`` and an undecodable file-name byte
    ``\\udcff``. No character is left that ``str.splitlines`` splits on, and no
    control character a terminal would act on. Every other character, the
    backslash included, is kept, so an ordinary path renders exactly as
    ``str(path)`` and a Windows path stays readable; a path that already holds a
    backslash and an ``n`` therefore reads like one holding a line break.

    Raises:
        TypeError: When ``path`` is not a ``str`` or a path-like object giving a
            ``str``.
    """
    text = os.fspath(path)
    if not isinstance(text, str):
        raise TypeError(f"path must be a str or str path-like, got {type(text).__name__}")
    return "".join(
        char.encode("unicode_escape").decode("ascii")
        if unicodedata.category(char) in _ESCAPED_CATEGORIES
        else char
        for char in text
    )


class WpctError(Exception):
    """Common base of the package's operational errors.

    Its subclasses are ``InputDataError`` (a required input is missing, or an
    input is unreadable, malformed, lacks required variables, has a case clash
    or stores a required variable with the wrong type), ``AnalysisHalted`` (a
    halting check failed) and ``OutputPathError`` (a write destination was
    refused). The command line catches each of them and reports it without a
    traceback.

    Not every deliberate error belongs to this family. Argument, configuration
    and schema validation raise the standard ``TypeError``, ``ValueError`` or
    ``KeyError``; for example, constructing an inconsistent
    ``schemas.ExportSpec`` raises ``ValueError``. The tolerance loader,
    ``compare.load_tolerances``, raises ``OSError`` for a missing or unreadable
    tolerance file or ``ValueError`` for an invalid one, which the command line
    also reports as an input error. Any other exception the command line does
    not handle propagates with its traceback.
    """


class InputDataError(WpctError):
    """A required input is missing, or an input is unreadable, malformed, lacks
    required variables, has a case clash or stores a required variable with the
    wrong type.

    Raised by ``xpt_reader`` and ``sas_reference``. The raiser supplies the full
    message, which ``str(exc)`` returns verbatim, for example:

    * ``"WPCT-F.07.02: input dataset not found: <absolute path>"``
    * ``"WPCT-F.07.07: cannot read SAS transport file <absolute path>:
      <reader message>"``
    * ``"WPCT-F.07.02: cannot read SAS transport file <absolute path>: member
      ADVS not found (first member is ADSL)"``
    * ``"<program>: <absolute path> lacks required variables: <names>"``
    * ``"<program>: <absolute path> has variables that differ only in case:
      <columns>"``
    * ``"WPCT-F.07.02: <absolute path> has required variables of the wrong
      type: SAFFL (numeric; character expected)"``
    * ``"SAS export directory is not a directory: <absolute path>"``

    ``sas_reference`` renders each path in its messages with
    :func:`message_path`, so a control character in a path, such as a line break,
    is shown escaped and the message stays one line.

    SAS counterparts: ``ERROR: (UTIL_ACCESS_TEST_DATA) Please confirm that data
    set ADVS exists in transport file ADVS.``
    [whitepapers/utilities/util_access_test_data.sas:68-71], which a missing
    ``ADVS`` member also gives, and ``ERROR: (ASSERT_DEPEND) Data set ... does
    not contain required variable ...``
    [whitepapers/utilities/assert_depend.sas:114-116]. A case clash has none:
    SAS variable names are case-insensitive, and ``assert_var_exist`` looks up
    the upper-cased name with ``varnum()``
    [whitepapers/utilities/assert_var_exist.sas:23]. Nor has a wrong type:
    ``assert_depend`` checks existence only, so SAS meets a mistyped variable
    only in the later steps that use it.
    """


class AnalysisHalted(WpctError):
    """A halting check failed, so the program stops before producing any output.

    Python counterpart of ``%assert_continue`` after a failed data-content
    assertion: on ``CONTINUE = 0`` the macro runs an invalid DATA step to force
    syntax-checking mode, so no later step processes data
    [whitepapers/utilities/assert_continue.sas:22-39].

    Attributes:
        message: The full halt message, also returned by ``str(exc)``, e.g.
            ``"WPCT-F.07.07: assert_var_nonmissing failed: 3 missing CHG in
            CSS_SAFANA"``.
        program_id: The program that halted, e.g. ``"WPCT-F.07.07"``.
        check: The failed assertion, ``"assert_var_nonmissing"`` (S07-FLT-04) or
            ``"assert_unique_keys"`` (S07-FLT-05).
        checkpoints: The ``wpct.programs.Checkpoint`` records the driver logged
            before the halt. Typed as a plain tuple because the record type lives
            downstream of this module.
    """

    def __init__(
        self,
        message: str,
        *,
        program_id: str,
        check: str,
        checkpoints: tuple = (),
    ) -> None:
        super().__init__(message)
        self.message = message
        self.program_id = program_id
        self.check = check
        self.checkpoints = tuple(checkpoints)

    def __reduce__(self):
        # Default exception pickling replays ``self.args`` positionally, which
        # cannot supply the keyword-only fields.
        rebuild = functools.partial(
            type(self),
            program_id=self.program_id,
            check=self.check,
            checkpoints=self.checkpoints,
        )
        return rebuild, (self.message,)


class OutputPathError(WpctError):
    """A write target is refused by the write policy.

    Raised by ``paths.check_write_destination`` and
    ``paths.vet_fixed_destination`` before any directory is created or file
    opened: a target outside the permitted destinations, a fixed destination
    reached through a symlink below the write root, one below an entry that is
    not a directory, or an existing one that is not a regular file or is a hard
    link shared with another path. Raised as well by
    ``paths.open_fixed_destination`` when the destination is found in one of
    those states as it is opened, or, opened by path, the opened file is not at
    the destination or the platform reports no path for it, before it is
    truncated. ``target`` is the path as requested and ``resolved`` the path it
    resolves to once every symlink is followed, or its absolute form
    (``os.path.abspath``) when the target or the write root cannot be resolved;
    the message names both.

    Attributes:
        target: The requested write target, as a ``Path``.
        resolved: The resolved form of ``target``, or its absolute form when the
            target or the write root cannot be resolved, as a ``Path``.
        reason: The caller's explanation; when ``None`` or empty the message
            states the permitted destinations instead.
    """

    def __init__(
        self,
        target: str | os.PathLike[str],
        resolved: str | os.PathLike[str],
        reason: str | None = None,
    ) -> None:
        self.target = Path(target)
        self.resolved = Path(resolved)
        self.reason = reason
        super().__init__(
            f"refusing to write {self.target} (resolves to {self.resolved}): "
            f"{reason or _PERMITTED_DESTINATIONS}"
        )

    def __reduce__(self):
        # The constructor takes the paths, not the formatted message in ``self.args``.
        return type(self), (self.target, self.resolved, self.reason)
