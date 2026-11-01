"""SAS transport (XPT v5) reader: the port's only access path to the ADVS input data.

The drivers read data only through this module, and only from the repository copies
``data/adam/cdisc/advs.xpt`` (07.02) and ``data/adam/cdisc-split/advs.xpt`` (07.07). It opens its
inputs read-only and never writes.

Spec items realised here (``python/wpct/docs/sas_specification.md``):

- S02-DEP-util_access_test_data, S07-DEP-util_access_test_data: :func:`read_xpt` ports the local
  branch of ``%util_access_test_data``, which copies the transport member to ``WORK.ADVS``
  [whitepapers/utilities/util_access_test_data.sas:47-57,62-66]. Its remote-URL branch
  [whitepapers/utilities/util_access_test_data.sas:58-60] is not ported: the port opens no network
  connection and has no fallback to the GitHub URL (ENV-02, ENV-04). The SAS failure ``ERROR:
  (UTIL_ACCESS_TEST_DATA) Please confirm that data set ADVS exists in transport file ADVS.``
  [whitepapers/utilities/util_access_test_data.sas:68-71] becomes an :class:`InputDataError`.
- S02-DEP-assert_dset_exist, S07-DEP-assert_dset_exist: the file-existence check of
  :func:`read_xpt`.
- S02-DEP-assert_depend, S07-DEP-assert_depend, S02-DEP-assert_var_exist,
  S07-DEP-assert_var_exist: :func:`require_variables` ports the ``vars=`` check
  [whitepapers/utilities/assert_depend.sas:102-128], which halts the program when a variable is
  absent. The OS, SASV, SYSPROD, macros and symbols checks test the SAS session and are not ported.
  The :class:`InputDataError` that :func:`require_variables` raises is also the port of the
  ``%assert_continue`` halt after the dependency assertion: S02-DEP-assert_continue
  [whitepapers/WPCT/WPCT-F.07.02.sas:158] and S07-DEP-assert_continue for its line-204 call
  [whitepapers/WPCT/WPCT-F.07.07.sas:204].
- D-10: dates stay SAS day numbers (days since 1960-01-01) as floats.
- D-11: a blank character value reads back as ``''``, with trailing blanks and NULs stripped.
"""

import os
from collections.abc import Iterable, Mapping
from dataclasses import dataclass, field
from pathlib import Path
from types import MappingProxyType

import pandas as pd
import pyreadstat

from wpct.errors import InputDataError

__all__ = ["XptMetadata", "read_xpt", "require_variables"]


@dataclass(frozen=True)
class XptMetadata:
    """Variable-level metadata of one transport member, as ``pyreadstat`` reports it.

    Attributes:
        path: Absolute path of the transport file read.
        table_name: Name of the member read, e.g. ``ADVS``; ``''`` when the file records none.
            ``pyreadstat`` reads the first member of the file.
        column_names: Variable names in dataset order, exactly as stored.
        labels: Variable label per variable; ``''`` for an unlabelled variable.
        formats: SAS format per variable as the reader reports it, e.g. ``DATE9`` for ``ADT``,
            ``TRTSDT`` and ``TRTEDT``; ``''`` for a variable without a format.
        storage_widths: Storage width in bytes per variable, e.g. ``STUDYID`` 12, ``USUBJID`` 11,
            ``AVISIT`` 16, and 8 for numerics.
        n_rows: Number of observations.
        n_columns: Number of variables.

    The three mappings are read-only and take no part in hashing.
    """

    path: Path
    table_name: str
    column_names: tuple[str, ...]
    labels: Mapping[str, str] = field(hash=False)
    formats: Mapping[str, str] = field(hash=False)
    storage_widths: Mapping[str, int] = field(hash=False)
    n_rows: int
    n_columns: int


def _cannot_read(program: str, abs_path: Path, reason: object) -> InputDataError:
    return InputDataError(f"{program}: cannot read SAS transport file {abs_path}: {reason}")


def read_xpt(path: str | os.PathLike[str], program: str) -> tuple[pd.DataFrame, XptMetadata]:
    """Read a SAS transport file into a frame and its metadata (S02/S07-DEP-util_access_test_data).

    Values are returned as the reader produces them, with no post-processing: numeric variables
    are float64 with every SAS missing value (``.``, ``._``, ``.A``-``.Z``) as NaN; dates stay SAS
    day numbers (D-10); character variables are ``str`` with trailing blanks stripped, so a blank
    value is ``''`` (D-11). Column names keep their stored case.

    Args:
        path: The transport file, absolute or relative to the working directory.
        program: Program identifier that prefixes every error message, e.g. ``"WPCT-F.07.02"``.

    Returns:
        The data frame and its :class:`XptMetadata`.

    Raises:
        InputDataError: ``"<program>: input dataset not found: <absolute path>"`` when the path does
            not exist or is not a regular file (S02/S07-DEP-assert_dset_exist), or ``"<program>:
            cannot read SAS transport file <absolute path>: <reader message>"`` when the file cannot
            be read or holds no variables.
    """
    abs_path = Path(path).absolute()
    try:
        is_file = abs_path.is_file()
    except OSError as exc:
        # pathlib reports a missing path as False but raises for other stat failures, such as a
        # parent directory the process may not search.
        raise _cannot_read(program, abs_path, exc) from exc
    if not is_file:
        raise InputDataError(f"{program}: input dataset not found: {abs_path}")

    try:
        df, meta = pyreadstat.read_xport(str(abs_path), disable_datetime_conversion=True)
    except Exception as exc:  # Any reader failure is the same input error, whatever its type.
        raise _cannot_read(program, abs_path, exc) from exc
    if df.shape[1] == 0:
        raise _cannot_read(program, abs_path, "no variables found")

    column_names = tuple(str(name) for name in df.columns)
    labels = meta.column_names_to_labels or {}
    formats = meta.original_variable_types or {}
    widths = meta.variable_storage_width or {}
    metadata = XptMetadata(
        path=abs_path,
        table_name=str(meta.table_name or ""),
        column_names=column_names,
        labels=MappingProxyType({name: str(labels.get(name) or "") for name in column_names}),
        formats=MappingProxyType({name: str(formats.get(name) or "") for name in column_names}),
        storage_widths=MappingProxyType({str(name): int(width) for name, width in widths.items()}),
        n_rows=int(df.shape[0]),
        n_columns=int(df.shape[1]),
    )
    return df, metadata


def require_variables(
    df: pd.DataFrame,
    names: Iterable[str],
    path: str | os.PathLike[str],
    program: str,
) -> pd.DataFrame:
    """Return ``df`` with ``names`` spelled as given (S02/S07-DEP-assert_depend ``vars=`` check).

    SAS variable names are case-insensitive, and ``assert_var_exist`` looks up the upper-cased name
    with ``varnum()`` [whitepapers/utilities/assert_var_exist.sas:23]
    (S02/S07-DEP-assert_var_exist), so the comparison folds case. As ``assert_depend`` checks every
    listed variable before the program halts [whitepapers/utilities/assert_depend.sas:111-120], the
    error names every missing variable, as given and in the order given. Every later SAS step then
    addresses a variable by any spelling, so the frame returned carries each required variable under
    its spelling in ``names``, the one the drivers index, whatever case the file stores.

    Args:
        df: The frame to check, normally the one :func:`read_xpt` returned. It is not modified.
        names: Required variable names, a sequence of strings such as the tuples of
            ``schemas.REQUIRED_INPUT_VARIABLES`` the drivers pass.
        path: The file ``df`` was read from, named in the message.
        program: Program identifier that prefixes the message.

    Returns:
        ``df`` itself when each name already matches its column exactly. Otherwise a new frame in
        which each matched column is renamed to its spelling in ``names``; other columns keep their
        stored spelling, and the column order and values are those of ``df``.

    Raises:
        TypeError: ``names`` is a single string, or one of its items is not a string.
        ValueError: ``names`` lists a variable more than once, ignoring case, so no single spelling
            can be given to its column.
        InputDataError: ``"<program>: <absolute path> lacks required variables: <names>"`` when a
            variable is absent, or ``"<program>: <absolute path> has variables that differ only in
            case: <columns>"`` when a required variable matches more than one column of ``df``.
    """
    if isinstance(names, str):
        # Iterating a lone string would check each of its characters as a variable name.
        raise TypeError(
            "require_variables: names must be a sequence of variable names, not the single string "
            f"{names!r}"
        )
    required = tuple(names)
    for position, name in enumerate(required):
        if not isinstance(name, str):
            raise TypeError(
                f"require_variables: names[{position}] must be a variable name string, got "
                f"{type(name).__name__}"
            )
    keys = [name.upper() for name in required]
    repeated = [name for name, key in zip(required, keys, strict=True) if keys.count(key) > 1]
    if repeated:
        raise ValueError(
            "require_variables: names lists a variable more than once, ignoring case: "
            f"{' '.join(repeated)}"
        )

    columns_by_key: dict[str, list[object]] = {}
    for column in df.columns:
        columns_by_key.setdefault(str(column).upper(), []).append(column)
    missing = [name for name, key in zip(required, keys, strict=True) if key not in columns_by_key]
    if missing:
        raise InputDataError(
            f"{program}: {Path(path).absolute()} lacks required variables: {' '.join(missing)}"
        )
    clashing = [
        str(column)
        for key in keys
        if len(columns_by_key[key]) > 1
        for column in columns_by_key[key]
    ]
    if clashing:
        raise InputDataError(
            f"{program}: {Path(path).absolute()} has variables that differ only in case: "
            f"{' '.join(clashing)}"
        )

    renames = {
        columns_by_key[key][0]: name
        for name, key in zip(required, keys, strict=True)
        if columns_by_key[key][0] != name
    }
    if not renames:
        return df
    return df.rename(columns=renames)
