"""Exception types shared by every module of the WPCT port.

This module is the only definition of the package's exceptions; other modules
import them and never define their own. It imports nothing from the package,
so any module may depend on it.

* ``InputDataError`` ports the failures of ``%util_access_test_data`` and the
  ``vars=`` check of ``%assert_depend``: a missing input file, an unreadable SAS
  transport file, a dataset lacking required variables, or a dataset storing a
  required variable under more than one spelling, in columns that differ only
  in case (S02-DEP-util_access_test_data, S07-DEP-util_access_test_data,
  S02-DEP-assert_depend, S07-DEP-assert_depend).
* ``AnalysisHalted`` ports ``%assert_continue`` forcing syntax-checking mode
  after a failed assertion (S02-DEP-assert_continue, S07-DEP-assert_continue),
  in particular the data halts S07-FLT-04 (missing ``CHG``) and S07-FLT-05
  (duplicate keys). A driver raising it exports nothing.
* ``OutputPathError`` reports a target refused by the write policy: the package
  writes only below ``python/wpct/outputs/`` and to
  ``python/wpct/reports/equivalence_report.md`` under the write root, so every
  other target, including any destination reached through a symlink, is
  refused before it is opened.
"""

from __future__ import annotations

import functools
import os
from pathlib import Path

__all__ = ["AnalysisHalted", "InputDataError", "OutputPathError", "WpctError"]

_PERMITTED_DESTINATIONS = (
    "only python/wpct/outputs/** and python/wpct/reports/equivalence_report.md "
    "under the write root are permitted"
)


class WpctError(Exception):
    """Base class of every error the package raises deliberately.

    The command line catches this family to report package failures without a
    traceback, while unexpected exceptions still propagate.
    """


class InputDataError(WpctError):
    """A required input is missing, unreadable, lacks required variables, or has a case clash.

    Raised by ``xpt_reader`` and ``sas_reference``. The raiser supplies the full
    message, which ``str(exc)`` returns verbatim, for example:

    * ``"WPCT-F.07.02: input dataset not found: <absolute path>"``
    * ``"WPCT-F.07.07: cannot read SAS transport file <absolute path>:
      <reader message>"``
    * ``"<program>: <absolute path> lacks required variables: <names>"``
    * ``"<program>: <absolute path> has variables that differ only in case:
      <columns>"``

    SAS counterparts: ``ERROR: (UTIL_ACCESS_TEST_DATA) Please confirm that data
    set ADVS exists in transport file ADVS.``
    [whitepapers/utilities/util_access_test_data.sas:68-71] and ``ERROR:
    (ASSERT_DEPEND) Data set ... does not contain required variable ...``
    [whitepapers/utilities/assert_depend.sas:114-116]. A case clash has none:
    SAS variable names are case-insensitive, and ``assert_var_exist`` looks up
    the upper-cased name with ``varnum()``
    [whitepapers/utilities/assert_var_exist.sas:23].
    """


class AnalysisHalted(WpctError):
    """A halting check failed, so the program stops before producing any output.

    Python counterpart of ``%assert_continue``, which on ``CONTINUE = 0`` runs an
    invalid DATA step to force syntax-checking mode, so no later step processes
    data [whitepapers/utilities/assert_continue.sas:22-39].

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
    """A write target lies outside the permitted output locations.

    Raised by ``paths.check_write_destination`` before any directory is created
    or file opened. ``target`` is the path as requested and ``resolved`` the
    path it resolves to once every symlink is followed; the message names both.

    Attributes:
        target: The requested write target, as a ``Path``.
        resolved: The resolved form of ``target``, as a ``Path``.
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
