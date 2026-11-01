"""Tests of ``wpct.xpt_reader``, the port's only access path to the ADVS transport files.

Spec items verified (``python/wpct/docs/sas_specification.md``):

- S02-DEP-util_access_test_data, S07-DEP-util_access_test_data: the local read of ``ADVS``, the
  variable labels and storage widths it reports, checked against the raw transport header, and its
  failure messages for a missing, unreadable, truncated or damaged input (AAP 0.6.4) and for one
  whose first member is not ``ADVS``.
- S02-DEP-assert_dset_exist, S07-DEP-assert_dset_exist: the file-existence check.
- S02-DEP-assert_depend, S07-DEP-assert_depend, S02-DEP-assert_var_exist,
  S07-DEP-assert_var_exist: the ``vars=`` check of :func:`require_variables`, and the SAS type
  check it adds against ``schemas.REQUIRED_INPUT_TYPES``.
- D-10: dates stay SAS day numbers, matching the dates the 07.07 log prints for the LMM-FLT-02
  omitted baseline records. D-11: a blank character value reads back as ``''`` and is missing.

The repository transport files are only read, and the SAS logs are parsed only through
``wpct.sas_reference``. The tests write only under ``tmp_path``.
"""

from __future__ import annotations

import datetime
import errno
import re
import struct
from collections.abc import Callable, Mapping
from pathlib import Path
from typing import NamedTuple

import numpy as np
import pandas as pd
import pyreadstat
import pytest

from wpct.errors import InputDataError
from wpct.paths import RepoPaths
from wpct.sas_reference import (
    KIND_WARNING_COUNT,
    LOG_CHECKPOINT_MAP,
    SasCheckpoint,
    parse_log_warning_records,
)
from wpct.sas_semantics import char_missing_mask, is_char_missing
from wpct.schemas import (
    CHAR,
    NUM,
    PROGRAM_07_02,
    PROGRAM_07_07,
    PROGRAMS,
    REQUIRED_INPUT_TYPES,
    REQUIRED_INPUT_VARIABLES,
)
from wpct.xpt_reader import XptMetadata, read_xpt, require_variables

#: Per program, the repository copy of its ADVS input. Its observation and variable counts are
#: those of the ``WORK.ADVS`` NOTE, parsed from the program's log at run time.
EXPECTED_INPUTS: Mapping[str, str] = {
    PROGRAM_07_02: "data/adam/cdisc/advs.xpt",
    PROGRAM_07_07: "data/adam/cdisc-split/advs.xpt",
}

#: The study of every 07.02 ADVS row [data/adam/cdisc/advs.xpt:STUDYID], so that study's row count
#: is the SAS ADVS count.
CDISC_STUDY = "CDISCPILOT01"

#: Rows per study of the 07.07 ADVS. This split is a fact of the data file, not a SAS reference
#: value: no SAS log or listing reports it [data/adam/cdisc-split/advs.xpt:STUDYID]. Only its sum is
#: a SAS count, and that is parsed from the log.
CDISC_SPLIT_STUDY_ROWS: Mapping[str, int] = {"CDISCPILOT01": 16034, "CDISCPILOT02": 16105}

#: The ``DATE9.``-formatted variables of ADVS (D-10).
DATE_VARIABLES = ("ADT", "TRTSDT", "TRTEDT")

#: Day 0 of the SAS date scale.
SAS_EPOCH = datetime.date(1960, 1, 1)

#: The month abbreviations ``DATE9.`` prints, in English whatever the session locale (D-10). Dates
#: are converted through this table, not ``strptime``'s ``%b``, which follows the locale.
DATE9_MONTHS = (
    "JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC"
)
_DATE9 = re.compile(r"(?P<day>\d{2})(?P<month>[A-Z]{3})(?P<year>\d{4})", re.ASCII)

#: A missing numeric as SAS ``put`` shows it: ``.``, or a special missing ``.A``-``.Z`` or ``._``.
_SAS_MISSING = re.compile(r"\.[A-Z_]?", re.ASCII)

#: The variables of an omitted-baseline WARNING record that identify and check its ADVS row.
WARNING_RECORD_NAMES = ("USUBJID", "PARAMCD", "ATPTN", "AVISITN", "ADT", "AVAL", "STUDYID", "TRTPN")

#: Layout of a version 5 transport file, after SAS Institute TS-140, "The Record Layout of a Data
#: Set in SAS Transport (XPORT) Format": 80-byte header records, the first member's header at
#: record 3, holding its NAMESTR length at bytes 74:78, and its NAMESTR header at record 7, holding
#: the variable count at bytes 54:58. One NAMESTR record per variable follows at record 8. Each
#: opens with the big-endian shorts ``ntype`` (1 numeric, 2 character), ``nhfun``, ``nlng`` (the
#: storage width) and ``nvar0``, then the blank-padded ``nname`` (8 bytes) and ``nlabel`` (40).
XPT_RECORD_LENGTH = 80
XPT_MEMBER_RECORD = 3
XPT_MEMBER_TAG = b"HEADER RECORD*******MEMBER  HEADER RECORD!!!!!!!"
XPT_NAMESTR_RECORD = 7
XPT_NAMESTR_TAG = b"HEADER RECORD*******NAMESTR HEADER RECORD!!!!!!!"
XPT_NAMESTR_FIELDS = struct.Struct(">hhhh8s40s")
XPT_NUMERIC, XPT_CHARACTER = 1, 2

#: Label and storage width of ADVS variables as the NAMESTR records of both repository files hold
#: them (``data/adam/cdisc/advs.xpt``, ``data/adam/cdisc-split/advs.xpt``). They pin the header
#: parser below, so the parser cannot drift together with the reader.
ADVS_LABELS_AND_WIDTHS: Mapping[str, tuple[str, int]] = {
    "STUDYID": ("Study Identifier", 12),
    "USUBJID": ("Unique Subject Identifier", 11),
    "PARAMCD": ("Parameter Code", 8),
    "PARAM": ("Parameter", 100),
    "AVISIT": ("Analysis Visit", 16),
    "SAFFL": ("Safety Population Flag", 1),
    "ANL01FL": ("Analysis Record Flag 01", 1),
    "ADT": ("Analysis Date", 8),
    "BASE": ("Baseline Value", 8),
    "CHG": ("Change from Baseline", 8),
}

#: Storage width of every numeric variable in ADVS and in a file ``pyreadstat`` writes: the full
#: 8-byte value. The layout also allows shorter, truncated numerics, which neither holds.
XPT_NUMERIC_WIDTH = 8


class _Namestr(NamedTuple):
    """The fields of one NAMESTR record that :class:`XptMetadata` reports or depends on."""

    ntype: int
    label: str
    width: int


def _namestr_records(path: Path) -> dict[str, _Namestr]:
    """Per variable, in NAMESTR order, its type, label and storage width from the raw header.

    An oracle independent of ``pyreadstat``: the first member's header of a version 5 transport
    file is decoded byte by byte as ``XPT_*`` lays it out. Only the header is read, read-only.

    Raises:
        ValueError: The member or NAMESTR header is not where the layout puts it, the file ends
            inside the NAMESTR records, or two records carry the same name.
    """
    with path.open("rb") as stream:
        head = stream.read((XPT_NAMESTR_RECORD + 1) * XPT_RECORD_LENGTH)
        member = head[XPT_MEMBER_RECORD * XPT_RECORD_LENGTH :][:XPT_RECORD_LENGTH]
        namestr = head[XPT_NAMESTR_RECORD * XPT_RECORD_LENGTH :][:XPT_RECORD_LENGTH]
        if not member.startswith(XPT_MEMBER_TAG) or not namestr.startswith(XPT_NAMESTR_TAG):
            raise ValueError(f"{path}: no version 5 member and NAMESTR header records")
        record_length, count = int(member[74:78]), int(namestr[54:58])
        if record_length < XPT_NAMESTR_FIELDS.size:
            raise ValueError(f"{path}: NAMESTR length {record_length} is too short")
        body = stream.read(record_length * count)
    if len(body) != record_length * count:
        raise ValueError(f"{path}: file ends inside its {count} NAMESTR records")

    records: dict[str, _Namestr] = {}
    for offset in range(0, len(body), record_length):
        ntype, _nhfun, width, _nvar0, name, label = XPT_NAMESTR_FIELDS.unpack_from(body, offset)
        variable = name.decode("ascii").rstrip(" ")
        if variable in records:
            raise ValueError(f"{path}: NAMESTR records repeat variable {variable!r}")
        records[variable] = _Namestr(ntype, label.decode("ascii").rstrip(" "), width)
    return records


def _advs_checkpoint(
    sas_checkpoints: Mapping[str, tuple[SasCheckpoint, ...]], program_id: str
) -> SasCheckpoint:
    """The program's ``WORK.ADVS`` NOTE as parsed from its log, checked to carry both counts."""
    (checkpoint,) = [
        checkpoint
        for checkpoint in sas_checkpoints[program_id]
        if checkpoint.spec.SAS_DATASET == "ADVS" and checkpoint.spec.OCCURRENCE == 1
    ]
    assert checkpoint.ok, checkpoint.problem
    assert checkpoint.spec.has_nvars
    return checkpoint


def _date9_day_number(text: str) -> int:
    """The SAS day number of ``DATE9.`` text such as ``29FEB1960``: days since 1960-01-01 (D-10).

    Raises:
        ValueError: ``text`` is not ``ddMONyyyy`` with an upper-case English month, or names no
            calendar date.
    """
    match = _DATE9.fullmatch(text)
    if match is None or match["month"] not in DATE9_MONTHS:
        raise ValueError(f"not a DATE9. value: {text!r}")
    month = DATE9_MONTHS.index(match["month"]) + 1
    return (datetime.date(int(match["year"]), month, int(match["day"])) - SAS_EPOCH).days


def _equals_displayed(values: pd.Series, text: str) -> pd.Series:
    """Where ``values`` equal the numeric value SAS ``put`` displayed as ``text``.

    A displayed missing value matches the missing values, which :func:`read_xpt` returns as NaN.
    """
    if _SAS_MISSING.fullmatch(text):
        return values.isna()
    return values == float(text)


@pytest.fixture(scope="module")
def advs(repo_paths: RepoPaths) -> Mapping[str, tuple[pd.DataFrame, XptMetadata]]:
    """Per program, its ADVS input exactly as :func:`read_xpt` returns it; never alter it."""
    return {
        program_id: read_xpt(repo_paths.input_xpt(program_id), program_id)
        for program_id in PROGRAMS
    }


# --- Input errors (AAP 0.6.4) --------------------------------------------------------------------


def _absent_file(tmp_path: Path, monkeypatch: pytest.MonkeyPatch) -> tuple[Path, Path]:
    target = tmp_path / "absent.xpt"
    return target, target


def _directory(tmp_path: Path, monkeypatch: pytest.MonkeyPatch) -> tuple[Path, Path]:
    return tmp_path, tmp_path


def _relative_path(tmp_path: Path, monkeypatch: pytest.MonkeyPatch) -> tuple[Path, Path]:
    monkeypatch.chdir(tmp_path)
    return Path("absent.xpt"), tmp_path / "absent.xpt"


@pytest.mark.parametrize(
    "make_target",
    [
        pytest.param(_absent_file, id="absent-file"),
        pytest.param(_directory, id="directory"),
        pytest.param(_relative_path, id="relative-path"),
    ],
)
def test_missing_file(
    make_target: Callable[[Path, pytest.MonkeyPatch], tuple[Path, Path]],
    tmp_path: Path,
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    """A path that is absent or not a regular file is a missing input dataset.

    Verifies S02-DEP-util_access_test_data and S02-DEP-assert_dset_exist: SAS logs ``ERROR:
    (UTIL_ACCESS_TEST_DATA) Please confirm that data set ADVS exists in transport file ADVS.``
    [whitepapers/utilities/util_access_test_data.sas:68-71]; the port raises ``InputDataError``
    naming the absolute path, also when it was given relative to the working directory.
    """
    target, expected = make_target(tmp_path, monkeypatch)
    assert expected.is_absolute()

    with pytest.raises(InputDataError) as excinfo:
        read_xpt(target, PROGRAM_07_02)

    assert str(excinfo.value) == f"WPCT-F.07.02: input dataset not found: {expected}"


def _write_text(path: Path) -> None:
    path.write_bytes(b"this is not a SAS transport file\n")


def _write_empty(path: Path) -> None:
    path.write_bytes(b"")


def _write_no_variables(path: Path) -> None:
    pyreadstat.write_xport(pd.DataFrame(), str(path), table_name="ADVS")


@pytest.mark.parametrize(
    "write_input",
    [
        pytest.param(_write_text, id="not-transport"),
        pytest.param(_write_empty, id="empty"),
        pytest.param(_write_no_variables, id="no-variables"),
    ],
)
def test_unreadable_file(write_input: Callable[[Path], None], tmp_path: Path) -> None:
    """A regular file the reader cannot read, or that holds no variables, is unreadable.

    Verifies S07-DEP-util_access_test_data: SAS fails the copy of ``ADVS`` with the same
    ``UTIL_ACCESS_TEST_DATA`` error [whitepapers/utilities/util_access_test_data.sas:62-71]; the
    port raises ``InputDataError`` naming the absolute path and ending with the reader's message.
    """
    bad = tmp_path / "bad.xpt"
    write_input(bad)
    prefix = f"WPCT-F.07.07: cannot read SAS transport file {bad}: "

    with pytest.raises(InputDataError) as excinfo:
        read_xpt(bad, PROGRAM_07_07)

    message = str(excinfo.value)
    assert message.startswith(prefix)
    assert message[len(prefix) :].strip()


#: The version 5 observation header record; the first member's observations start in the record
#: after it, and blank padding completes their last record (TS-140, as ``XPT_*`` above).
XPT_OBS_TAG = b"HEADER RECORD*******OBS     HEADER RECORD!!!!!!!"


class _Layout(NamedTuple):
    """Where the observations of one repository ADVS file lie, taken from the file itself."""

    data: bytes
    data_start: int
    row_length: int
    n_rows: int


def _half(layout: _Layout) -> bytes:
    """The first half of the file, which is not a whole number of records."""
    return layout.data[: len(layout.data) // 2]


def _half_record_aligned(layout: _Layout) -> bytes:
    """The first half cut back to a record boundary that falls inside an observation."""
    cut = len(layout.data) // 2 // XPT_RECORD_LENGTH * XPT_RECORD_LENGTH
    while (cut - layout.data_start) % layout.row_length == 0:
        cut -= XPT_RECORD_LENGTH
    return layout.data[:cut]


def _tail_cut(layout: _Layout) -> bytes:
    """The file without its last 1000 bytes."""
    return layout.data[:-1000]


def _padding_cut(layout: _Layout) -> bytes:
    """The file without the blank padding after its last observation, and nothing more."""
    data_end = layout.data_start + layout.n_rows * layout.row_length
    assert layout.data[data_end:], "the file holds no padding to cut"
    assert not layout.data[data_end:].strip(b" "), "bytes after the last observation not blank"
    return layout.data[:data_end]


def _extra_record(layout: _Layout) -> bytes:
    """The whole file followed by one non-blank record that begins no member."""
    return layout.data + b"0" * XPT_RECORD_LENGTH


@pytest.mark.parametrize("program_id", PROGRAMS)
@pytest.mark.parametrize(
    ("cut", "reason"),
    [
        pytest.param(_half, "truncated", id="half"),
        pytest.param(_half_record_aligned, "truncated", id="half-record-aligned"),
        pytest.param(_tail_cut, "truncated", id="tail-cut"),
        pytest.param(_padding_cut, "truncated", id="padding-cut"),
        pytest.param(_extra_record, "damaged", id="extra-record"),
    ],
)
def test_s02_and_s07_dep_util_access_test_data_rejects_partial_file(
    cut: Callable[[_Layout], bytes],
    reason: str,
    program_id: str,
    advs: Mapping[str, tuple[pd.DataFrame, XptMetadata]],
    repo_paths: RepoPaths,
    tmp_path: Path,
) -> None:
    """A truncated or damaged copy of a program's ADVS is unreadable, never read in part.

    Verifies S02-DEP-util_access_test_data and S07-DEP-util_access_test_data: SAS copies the whole
    member to ``WORK.ADVS`` or fails with the ``UTIL_ACCESS_TEST_DATA`` error
    [whitepapers/utilities/util_access_test_data.sas:62-71]. ``pyreadstat`` reads each cut copy
    without complaint, dropping what follows the last whole observation, so the port raises
    ``InputDataError`` naming the absolute path and the observations the reader returned. The cuts
    come from the repository file's own size, observation header record and row length.
    """
    path = repo_paths.input_xpt(program_id)
    data = path.read_bytes()
    data_start = data.index(XPT_OBS_TAG) + XPT_RECORD_LENGTH
    assert data_start % XPT_RECORD_LENGTH == 0
    row_length = sum(record.width for record in _namestr_records(path).values())
    layout = _Layout(data, data_start, row_length, advs[program_id][1].n_rows)
    damaged = tmp_path / "advs.xpt"
    damaged.write_bytes(cut(layout))
    partial, _ = pyreadstat.read_xport(str(damaged), disable_datetime_conversion=True)
    prefix = f"{program_id}: cannot read SAS transport file {damaged}: {reason} transport file: "

    with pytest.raises(InputDataError) as excinfo:
        read_xpt(damaged, program_id)

    message = str(excinfo.value)
    assert message.startswith(prefix)
    assert f"(observations read: {len(partial)} of {row_length} bytes each)" in message


def _header_only_advs(path: Path, repo_paths: RepoPaths) -> int:
    data = repo_paths.input_xpt(PROGRAM_07_02).read_bytes()
    path.write_bytes(data[: data.index(XPT_OBS_TAG) + XPT_RECORD_LENGTH])
    return 0


def _written(
    frame: pd.DataFrame, version: int, member: str = "ADVS"
) -> Callable[[Path, RepoPaths], int]:
    def write(path: Path, repo_paths: RepoPaths) -> int:
        pyreadstat.write_xport(frame, str(path), table_name=member, file_format_version=version)
        return len(frame)

    return write


_PADDED = pd.DataFrame(
    {"USUBJID": ["01-701-1015", "01-701-1023", "x"], "AVAL": [64.0, np.nan, 1.0]}
)


@pytest.mark.parametrize(
    "write_input",
    [
        pytest.param(_header_only_advs, id="advs-header-only"),
        pytest.param(_written(_PADDED, 5), id="padded"),
        pytest.param(_written(_PADDED, 8), id="padded-version-8"),
        pytest.param(_written(_PADDED, 5, "advs"), id="member-lower-case"),
        pytest.param(
            _written(
                pd.DataFrame({"AVAL": np.arange(float(XPT_RECORD_LENGTH // XPT_NUMERIC_WIDTH))}),
                5,
            ),
            id="record-aligned",
        ),
        pytest.param(
            _written(pd.DataFrame({"AVAL": [64.0, 70.0], "ANL01FL": ["Y", ""]}), 5),
            id="blank-last-value",
        ),
        pytest.param(
            _written(
                pd.DataFrame(
                    {"USUBJID": pd.Series([], dtype=object), "AVAL": pd.Series([], dtype=float)}
                ),
                5,
            ),
            id="no-observations",
        ),
    ],
)
def test_s02_and_s07_dep_util_access_test_data_reads_whole_file(
    write_input: Callable[[Path, RepoPaths], int], repo_paths: RepoPaths, tmp_path: Path
) -> None:
    """A whole transport file reads as the reader returns it, whatever its padding.

    Verifies S02-DEP-util_access_test_data and S07-DEP-util_access_test_data: the layout check
    accepts a file whose observations end with or without blank padding, a version 8 file, a last
    observation ending in a blank value, and a member with no observations, including the
    repository ADVS header up to its observation header record. The member check accepts ``advs``,
    because SAS member names ignore case.
    """
    path = tmp_path / "whole.xpt"
    expected_rows = write_input(path, repo_paths)
    direct, _ = pyreadstat.read_xport(str(path), disable_datetime_conversion=True)

    frame, metadata = read_xpt(path, PROGRAM_07_07)

    assert metadata.n_rows == len(frame) == expected_rows
    pd.testing.assert_frame_equal(frame, direct, check_exact=True)


#: Bytes 0:16 of the first member's descriptor record (record 5, after its member and descriptor
#: header records, as ``XPT_*`` above): ``SAS`` and the member name, each blank-padded to 8 bytes.
XPT_ADVS_DESCRIPTOR = b"SAS     ADVS    "


def _member_adsl(path: Path) -> None:
    pyreadstat.write_xport(pd.DataFrame({"AVAL": [64.0]}), str(path), table_name="ADSL")


def _member_unnamed(path: Path) -> None:
    pyreadstat.write_xport(pd.DataFrame({"AVAL": [64.0]}), str(path), table_name="ADVS")
    data = path.read_bytes()
    assert data.count(XPT_ADVS_DESCRIPTOR) == 1
    path.write_bytes(data.replace(XPT_ADVS_DESCRIPTOR, b"SAS" + b" " * 13))


@pytest.mark.parametrize(
    ("write_input", "first"),
    [
        pytest.param(_member_adsl, "first member is ADSL", id="member-adsl"),
        pytest.param(_member_unnamed, "the first member has no name", id="member-unnamed"),
    ],
)
def test_s02_and_s07_dep_util_access_test_data_requires_member_advs(
    write_input: Callable[[Path], None], first: str, tmp_path: Path
) -> None:
    """A transport file whose first member is not ``ADVS`` is unreadable, naming that member.

    Verifies S02-DEP-util_access_test_data and S07-DEP-util_access_test_data: SAS copies member
    ``ADVS`` by name (``set source.&ds``) and logs the ``UTIL_ACCESS_TEST_DATA`` error when it is
    absent [whitepapers/utilities/util_access_test_data.sas:64-71]. ``pyreadstat`` reads the first
    member whatever its name, so the port raises ``InputDataError`` naming the absolute path and
    the member it found, as the port notes record.
    """
    path = tmp_path / "advs.xpt"
    write_input(path)

    with pytest.raises(InputDataError) as excinfo:
        read_xpt(path, PROGRAM_07_02)

    assert str(excinfo.value) == (
        f"WPCT-F.07.02: cannot read SAS transport file {path}: member ADVS not found ({first})"
    )


def test_s02_and_s07_dep_util_access_test_data_layout_read_error(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    """A file the layout check cannot open is unreadable, and the message ends with the OS error.

    Verifies S02-DEP-util_access_test_data and S07-DEP-util_access_test_data: an input error, not a
    traceback, when the file the reader has just read cannot be opened again.
    """
    path = tmp_path / "advs.xpt"
    pyreadstat.write_xport(pd.DataFrame({"AVAL": [64.0]}), str(path), table_name="ADVS")
    error = PermissionError(errno.EACCES, "Permission denied", str(path))
    original_open = Path.open

    def refuse_open(self: Path, *args: object, **kwargs: object) -> object:
        if self == path:
            raise error
        return original_open(self, *args, **kwargs)

    monkeypatch.setattr(Path, "open", refuse_open)

    with pytest.raises(InputDataError) as excinfo:
        read_xpt(path, PROGRAM_07_02)

    assert str(excinfo.value) == f"WPCT-F.07.02: cannot read SAS transport file {path}: {error}"
    assert excinfo.value.__cause__ is error


def test_missing_variables(tmp_path: Path, monkeypatch: pytest.MonkeyPatch) -> None:
    """Every absent required variable is named, in the order given; present names pass.

    Verifies S02-DEP-assert_depend and S02-DEP-assert_var_exist: ``assert_depend`` checks each
    ``vars=`` entry and halts the program when any is absent
    [whitepapers/utilities/assert_depend.sas:111-120]; ``assert_var_exist`` looks the name up
    upper-cased [whitepapers/utilities/assert_var_exist.sas:23], so the check ignores case.
    """
    monkeypatch.chdir(tmp_path)
    frame = pd.DataFrame({"USUBJID": ["a"]})

    with pytest.raises(InputDataError) as excinfo:
        require_variables(frame, ["USUBJID", "NOPE_A", "NOPE_B"], "advs.xpt", PROGRAM_07_02)

    assert str(excinfo.value) == (
        f"WPCT-F.07.02: {tmp_path / 'advs.xpt'} lacks required variables: NOPE_A NOPE_B"
    )
    require_variables(frame, ["USUBJID"], "advs.xpt", PROGRAM_07_02)
    require_variables(frame, ["usubjid"], "advs.xpt", PROGRAM_07_02)


@pytest.mark.parametrize(
    ("names", "error", "message"),
    [
        pytest.param(
            "SAFFL",
            TypeError,
            "require_variables: names must be a sequence of variable names, not the single "
            "string 'SAFFL'",
            id="lone-string",
        ),
        pytest.param(
            ("SAFFL", 7),
            TypeError,
            "require_variables: names[1] must be a variable name string, got int",
            id="non-string-item",
        ),
        pytest.param(
            ("SAFFL", "USUBJID", "saffl"),
            ValueError,
            "require_variables: names lists a variable more than once, ignoring case: SAFFL saffl",
            id="repeated-other-case",
        ),
        pytest.param(
            ("SAFFL", "SAFFL"),
            ValueError,
            "require_variables: names lists a variable more than once, ignoring case: SAFFL SAFFL",
            id="repeated-same-case",
        ),
    ],
)
def test_s02_and_s07_dep_assert_depend_require_variables_rejects_invalid_names(
    names: object, error: type[Exception], message: str
) -> None:
    """``names`` must list each variable once, as strings; a lone string is not split.

    Verifies S02-DEP-assert_depend and S07-DEP-assert_depend: the ``vars=`` check takes a list of
    variable names and checks each entry once [whitepapers/utilities/assert_depend.sas:102-128],
    and the port receives that list as data. Iterating a lone string would check each of its
    characters as a variable name, and a variable listed twice in different cases leaves no single
    spelling for its column, because names match case-insensitively: S02-DEP-assert_var_exist and
    S07-DEP-assert_var_exist look them up upper-cased
    [whitepapers/utilities/assert_var_exist.sas:23].
    """
    frame = pd.DataFrame({"SAFFL": ["Y"], "USUBJID": ["a"]})

    with pytest.raises(error) as excinfo:
        require_variables(frame, names, "advs.xpt", PROGRAM_07_02)

    assert type(excinfo.value) is error
    assert str(excinfo.value) == message


@pytest.mark.parametrize(
    "stored",
    [
        pytest.param(("SAFFL", "USUBJID"), id="exact"),
        pytest.param(("saffl", "usubjid"), id="lower-case"),
        pytest.param(("SafFL", "usubjID"), id="mixed-case"),
    ],
)
def test_s02_and_s07_dep_assert_var_exist_require_variables_returns_given_spelling(
    stored: tuple[str, str]
) -> None:
    """Each required column comes back under its spelling in ``names``; ``df`` is not modified.

    Verifies S02-DEP-assert_var_exist and S07-DEP-assert_var_exist: SAS finds a variable whatever
    the case of its name [whitepapers/utilities/assert_var_exist.sas:23], and every later step
    addresses it by any spelling, so the drivers' upper-case names reach the stored columns. A
    column not required keeps its stored spelling, and a frame already spelled as given is returned
    itself.
    """
    stored_saffl, stored_usubjid = stored
    frame = pd.DataFrame(
        {stored_saffl: ["Y", "N"], "trtp_short": ["P", "X-high"], stored_usubjid: ["a", "b"]}
    )
    original = frame.copy(deep=True)

    result = require_variables(frame, ("USUBJID", "SAFFL"), "advs.xpt", PROGRAM_07_02)

    assert (result is frame) == (stored == ("SAFFL", "USUBJID"))
    expected = original.set_axis(["SAFFL", "trtp_short", "USUBJID"], axis="columns")
    pd.testing.assert_frame_equal(result, expected, check_exact=True)
    pd.testing.assert_frame_equal(frame, original, check_exact=True)


def test_s02_and_s07_dep_assert_var_exist_require_variables_rejects_case_duplicates(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    """A required variable matching two columns that differ only in case is an input error.

    Verifies S02-DEP-assert_var_exist and S07-DEP-assert_var_exist: ``varnum()`` looks up the
    upper-cased name [whitepapers/utilities/assert_var_exist.sas:23], so SAS variable names are
    case-insensitive and such a frame holds no single variable to address. Verifies
    S02-DEP-assert_depend and S07-DEP-assert_depend: only the required names are checked, and the
    missing-variable error of the ``vars=`` check [whitepapers/utilities/assert_depend.sas:114-116]
    is reported first; the case clash raises the ``InputDataError`` their port notes document.
    """
    monkeypatch.chdir(tmp_path)
    frame = pd.DataFrame(
        {"SAFFL": ["Y"], "USUBJID": ["a"], "saffl": ["N"], "AVAL": [1.0], "aval": [2.0]}
    )

    with pytest.raises(InputDataError) as excinfo:
        require_variables(frame, ("USUBJID", "SAFFL"), "advs.xpt", PROGRAM_07_07)

    assert str(excinfo.value) == (
        f"WPCT-F.07.07: {tmp_path / 'advs.xpt'} has variables that differ only in case: SAFFL saffl"
    )
    with pytest.raises(InputDataError) as excinfo:
        require_variables(frame, ("SAFFL", "ADT"), "advs.xpt", PROGRAM_07_07)
    assert str(excinfo.value) == (
        f"WPCT-F.07.07: {tmp_path / 'advs.xpt'} lacks required variables: ADT"
    )


@pytest.mark.parametrize(
    ("columns", "names", "entries"),
    [
        pytest.param(
            {"SAFFL": [1.0, np.nan]},
            ("SAFFL",),
            "SAFFL (numeric; character expected)",
            id="character-stored-numeric",
        ),
        pytest.param(
            {"CHG": ["-2", ""]},
            ("CHG",),
            "CHG (character; numeric expected)",
            id="numeric-stored-character",
        ),
        pytest.param(
            {"PARAMCD": pd.Series([], dtype="float64")},
            ("PARAMCD",),
            "PARAMCD (numeric; character expected)",
            id="no-rows-numeric-character",
        ),
        pytest.param(
            {"SAFFL": [True, False], "TRTPN": [True, False]},
            ("SAFFL", "TRTPN"),
            "SAFFL (boolean; character expected), TRTPN (boolean; numeric expected)",
            id="boolean",
        ),
        pytest.param(
            {"PARAMCD": pd.Series(["DIABP", 1.0, 2, None], dtype=object)},
            ("PARAMCD",),
            "PARAMCD (dtype object with float and int values; character expected)",
            id="object-non-string-values",
        ),
        pytest.param(
            {"ADT": pd.Series([19000.0, None], dtype=object)},
            ("ADT",),
            "ADT (dtype object with float values; numeric expected)",
            id="numbers-in-object-column",
        ),
        pytest.param(
            {"saffl": [1.0], "ATPTN": ["815"], "USUBJID": ["01-701-1015"]},
            ("ATPTN", "USUBJID", "SAFFL"),
            "ATPTN (character; numeric expected), SAFFL (numeric; character expected)",
            id="every-offender-in-order-given",
        ),
    ],
)
def test_s02_and_s07_dep_assert_depend_rejects_wrong_type(
    columns: Mapping[str, object],
    names: tuple[str, ...],
    entries: str,
    tmp_path: Path,
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    """A required variable stored with the other SAS type is an input error, naming no value.

    Verifies S02-DEP-assert_depend and S07-DEP-assert_depend: the ``vars=`` check
    [whitepapers/utilities/assert_depend.sas:102-128] checks existence only, so SAS meets a
    mistyped variable only in the steps that use it; the port adds the type check its port notes
    record, against ``schemas.REQUIRED_INPUT_TYPES``. Every offender is named as given, in the
    order given and with the type it is stored with, so the CLI exits 2 instead of failing later
    with a ``TypeError``. Names match ignoring case (S02-DEP-assert_var_exist,
    S07-DEP-assert_var_exist).
    """
    monkeypatch.chdir(tmp_path)
    frame = pd.DataFrame(columns)
    original = frame.copy(deep=True)

    with pytest.raises(InputDataError) as excinfo:
        require_variables(frame, names, "advs.xpt", PROGRAM_07_02)

    assert str(excinfo.value) == (
        f"WPCT-F.07.02: {tmp_path / 'advs.xpt'} has required variables of the wrong type: "
        f"{entries}"
    )
    pd.testing.assert_frame_equal(frame, original, check_exact=True)


@pytest.mark.parametrize(
    ("columns", "names"),
    [
        pytest.param(
            {"SAFFL": ["Y", "", "   ", None, np.nan], "AVAL": [1.0, np.nan, 2.0, 3.0, 4.0]},
            ("SAFFL", "AVAL"),
            id="missing-character-values",
        ),
        pytest.param(
            {"PARAMCD": pd.Series([None, np.nan], dtype=object)},
            ("PARAMCD",),
            id="all-missing-character",
        ),
        pytest.param(
            {"TRTPN": pd.Series([0, 54, 81], dtype="int64"), "ATPTN": [815.0, 816.0, np.nan]},
            ("TRTPN", "ATPTN"),
            id="integer-and-float-numeric",
        ),
        pytest.param(
            {"trtp_short": [1.0], "ANL02FL": [True], "NOPE": ["x"]},
            ("trtp_short", "ANL02FL", "NOPE"),
            id="names-without-schema-type",
        ),
    ],
)
def test_s02_and_s07_dep_assert_depend_accepts_stored_types(
    columns: Mapping[str, object], names: tuple[str, ...]
) -> None:
    """Required variables stored with their SAS type pass; untyped names need only exist.

    Verifies S02-DEP-assert_depend and S07-DEP-assert_depend: a character variable may hold
    ``''``, blanks, ``None`` or NaN, all character-missing (D-11), and a numeric one any numeric
    dtype other than bool. A name ``schemas.REQUIRED_INPUT_TYPES`` does not type, such as the
    derived ``trtp_short`` and ``ANL02FL``, is checked for presence alone, and the frame comes back
    itself.
    """
    frame = pd.DataFrame(columns)

    assert require_variables(frame, names, "advs.xpt", PROGRAM_07_07) is frame


# --- Repository inputs (read-only) ---------------------------------------------------------------


@pytest.mark.parametrize("program_id", PROGRAMS)
def test_s02_and_s07_dep_util_access_test_data_reads_advs(
    program_id: str,
    advs: Mapping[str, tuple[pd.DataFrame, XptMetadata]],
    repo_paths: RepoPaths,
    sas_checkpoints: Mapping[str, tuple[SasCheckpoint, ...]],
) -> None:
    """Each program's ADVS reads whole, with its studies and every required variable.

    Verifies S02-DEP-util_access_test_data and S07-DEP-util_access_test_data against the
    observation and variable counts of the ``WORK.ADVS`` NOTE, parsed from each log at run time
    [whitepapers/WPCT/outputs_sas/WPCT-F.07.02.log:32;
    whitepapers/WPCT/outputs_sas/WPCT-F.07.07.log:32], and S02-DEP-assert_depend /
    S07-DEP-assert_depend on the real input. The study row counts sum to the SAS count. ``ANL02FL``
    is absent from both files, because the 07.07 derivation creates it (S07-TRN-03).
    """
    path = repo_paths.input_xpt(program_id)
    frame, metadata = advs[program_id]
    checkpoint = _advs_checkpoint(sas_checkpoints, program_id)
    shape = (checkpoint.NOBS, checkpoint.NVARS)
    study_rows = frame["STUDYID"].value_counts().to_dict()
    expected_study_rows = (
        {CDISC_STUDY: checkpoint.NOBS}
        if program_id == PROGRAM_07_02
        else dict(CDISC_SPLIT_STUDY_ROWS)
    )

    assert path == repo_paths.repo_root / EXPECTED_INPUTS[program_id]
    assert frame.shape == shape
    assert (metadata.n_rows, metadata.n_columns) == shape
    assert metadata.path == path
    assert metadata.table_name == "ADVS"
    assert metadata.column_names == tuple(frame.columns)
    assert study_rows == expected_study_rows
    assert sum(study_rows.values()) == checkpoint.NOBS
    assert "ANL02FL" not in frame.columns
    require_variables(frame, REQUIRED_INPUT_VARIABLES[program_id], path, program_id)


@pytest.mark.parametrize("program_id", PROGRAMS)
def test_s02_and_s07_dep_assert_depend_required_types_match_namestr(
    program_id: str, repo_paths: RepoPaths
) -> None:
    """``schemas.REQUIRED_INPUT_TYPES`` holds the SAS type each program's ADVS stores.

    Verifies S02-DEP-assert_depend and S07-DEP-assert_depend: every variable the program requires
    is typed, and its type is the ``ntype`` of its NAMESTR record in the program's ADVS, decoded
    without ``pyreadstat`` (1 numeric, 2 character).
    """
    header = _namestr_records(repo_paths.input_xpt(program_id))
    expected = {CHAR: XPT_CHARACTER, NUM: XPT_NUMERIC}

    for name in REQUIRED_INPUT_VARIABLES[program_id]:
        assert header[name].ntype == expected[REQUIRED_INPUT_TYPES[name]], name


@pytest.mark.parametrize("program_id", PROGRAMS)
def test_s02_and_s07_dep_assert_depend_rejects_mistyped_transport_variable(
    program_id: str,
    advs: Mapping[str, tuple[pd.DataFrame, XptMetadata]],
    tmp_path: Path,
) -> None:
    """A transport file storing ``SAFFL`` as numeric reads, then fails the required-variable check.

    Verifies S02-DEP-assert_depend and S07-DEP-assert_depend through :func:`read_xpt`: the first
    rows of the program's ADVS are written back to ``tmp_path`` with ``SAFFL`` recoded to
    ``1``/``0``, so the file stores a numeric ``SAFFL`` where the program needs a character flag
    (S02-POP-01, S07-POP-01). The message names the absolute path and the variable, not a value.
    """
    frame = advs[program_id][0].head(20).copy()
    frame["SAFFL"] = (frame["SAFFL"] == "Y").astype("float64")
    path = tmp_path / "advs.xpt"
    pyreadstat.write_xport(frame, str(path), table_name="ADVS")

    stored, _ = read_xpt(path, program_id)

    assert stored["SAFFL"].dtype == np.float64
    with pytest.raises(InputDataError) as excinfo:
        require_variables(stored, REQUIRED_INPUT_VARIABLES[program_id], path, program_id)
    assert str(excinfo.value) == (
        f"{program_id}: {path} has required variables of the wrong type: "
        "SAFFL (numeric; character expected)"
    )


@pytest.mark.parametrize("program_id", PROGRAMS)
def test_s02_and_s07_dep_util_access_test_data_reads_labels_and_storage_widths(
    program_id: str,
    advs: Mapping[str, tuple[pd.DataFrame, XptMetadata]],
    repo_paths: RepoPaths,
    sas_checkpoints: Mapping[str, tuple[SasCheckpoint, ...]],
) -> None:
    """Each ADVS variable's label and storage width read back as its NAMESTR record holds them.

    Verifies S02-DEP-util_access_test_data and S07-DEP-util_access_test_data: the variable
    attributes of ``WORK.ADVS`` are those the transport member carries
    [whitepapers/utilities/util_access_test_data.sas:62-66]. The expected mappings come from the
    file's header bytes, decoded without ``pyreadstat``, and both mappings cover every variable in
    dataset order with nothing more.
    """
    _, metadata = advs[program_id]
    header = _namestr_records(repo_paths.input_xpt(program_id))

    assert len(header) == _advs_checkpoint(sas_checkpoints, program_id).NVARS
    assert tuple(header) == metadata.column_names
    for name, pinned in ADVS_LABELS_AND_WIDTHS.items():
        assert (header[name].label, header[name].width) == pinned, name
    numeric = [name for name, record in header.items() if record.ntype == XPT_NUMERIC]
    assert {"ADT", "BASE", "CHG"} <= set(numeric)
    assert {header[name].width for name in numeric} == {XPT_NUMERIC_WIDTH}

    assert tuple(metadata.labels) == metadata.column_names
    assert tuple(metadata.storage_widths) == metadata.column_names
    assert dict(metadata.labels) == {name: record.label for name, record in header.items()}
    assert dict(metadata.storage_widths) == {
        name: record.width for name, record in header.items()
    }
    assert all(type(label) is str for label in metadata.labels.values())
    assert all(type(width) is int for width in metadata.storage_widths.values())


def test_s02_and_s07_dep_util_access_test_data_reads_supplied_labels_and_storage_widths(
    tmp_path: Path,
) -> None:
    """Labels and storage widths written into a transport file read back exactly.

    Verifies S02-DEP-util_access_test_data and S07-DEP-util_access_test_data on the cases ADVS does
    not hold: a variable written without a label reads back as ``''``, and a character variable is
    as wide as its longest value, not its first. The header bytes must hold the supplied metadata
    too, which checks the header parser on known input.
    """
    path = tmp_path / "labelled.xpt"
    frame = pd.DataFrame(
        {
            "USUBJID": ["01-701-1015", "01-701-1023"],
            "AVAL": [64.0, np.nan],
            "AVISIT": ["Week 2", "Week 24"],
        }
    )
    pyreadstat.write_xport(
        frame,
        str(path),
        table_name="ADVS",
        column_labels=["Unique Subject Identifier", "Analysis Value", None],
        file_format_version=5,
    )

    assert _namestr_records(path) == {
        "USUBJID": _Namestr(XPT_CHARACTER, "Unique Subject Identifier", 11),
        "AVAL": _Namestr(XPT_NUMERIC, "Analysis Value", XPT_NUMERIC_WIDTH),
        "AVISIT": _Namestr(XPT_CHARACTER, "", 7),
    }

    _, metadata = read_xpt(path, PROGRAM_07_02)

    assert metadata.column_names == ("USUBJID", "AVAL", "AVISIT")
    assert tuple(metadata.labels) == metadata.column_names
    assert tuple(metadata.storage_widths) == metadata.column_names
    assert dict(metadata.labels) == {
        "USUBJID": "Unique Subject Identifier",
        "AVAL": "Analysis Value",
        "AVISIT": "",
    }
    assert dict(metadata.storage_widths) == {"USUBJID": 11, "AVAL": 8, "AVISIT": 7}
    assert all(type(label) is str for label in metadata.labels.values())
    assert all(type(width) is int for width in metadata.storage_widths.values())


@pytest.mark.parametrize("program_id", PROGRAMS)
def test_d10_dates_kept_as_sas_day_numbers(
    program_id: str,
    advs: Mapping[str, tuple[pd.DataFrame, XptMetadata]],
    repo_paths: RepoPaths,
) -> None:
    """``DATE9.`` variables stay float day counts from 1960-01-01 (D-10).

    ``pyreadstat``'s default would return ``datetime.date`` objects, so the first rows are read with
    that default and their day counts must equal the values :func:`read_xpt` returned.
    """
    frame, metadata = advs[program_id]
    for name in DATE_VARIABLES:
        values = frame[name]
        assert pd.api.types.is_float_dtype(values.dtype), name
        assert metadata.formats[name].startswith("DATE"), name
        present = values.dropna().to_numpy()
        assert present.size > 0, name
        assert np.array_equal(present, np.floor(present)), name

    converted, _ = pyreadstat.read_xport(
        str(repo_paths.input_xpt(program_id)), usecols=list(DATE_VARIABLES), row_limit=5
    )
    for name in DATE_VARIABLES:
        dates = converted[name].tolist()
        assert all(isinstance(value, datetime.date) for value in dates), name
        assert [float((value - SAS_EPOCH).days) for value in dates] == (
            frame[name].iloc[:5].tolist()
        ), name


def test_d10_baseline_date_matches_sas_log(
    advs: Mapping[str, tuple[pd.DataFrame, XptMetadata]],
    repo_paths: RepoPaths,
    sas_checkpoints: Mapping[str, tuple[SasCheckpoint, ...]],
) -> None:
    """Each baseline record the 07.07 log prints holds its printed ``ADT`` as a day number (D-10).

    ``%derive_lastminmax_measure`` prints ``_all_`` of each baseline record it omits because its
    group has no post-baseline record (LMM-FLT-02)
    [whitepapers/ADaM/derive_lastminmax_measure.sas:167;
    whitepapers/WPCT/outputs_sas/WPCT-F.07.07.log:124-183]. The records are parsed from the log at
    run time, one per WARNING the LMM_BASE_OMITTED checkpoint counts. The 07.07 ADVS rows of each
    record's ``USUBJID``, ``PARAMCD``, ``ATPTN`` and ``AVISITN`` (a printed ``.`` matching a
    missing visit) include one whose ``ADT`` is the day number of the printed ``DATE9.`` date and
    whose ``AVAL`` is the printed value, and they carry the printed ``STUDYID`` and ``TRTPN``.
    """
    frame, _ = advs[PROGRAM_07_07]
    (spec,) = [
        spec
        for spec in LOG_CHECKPOINT_MAP[PROGRAM_07_07]
        if spec.kind == KIND_WARNING_COUNT and spec.SAS_DATASET == "LMM_BASE_OMITTED"
    ]
    (checkpoint,) = [
        checkpoint for checkpoint in sas_checkpoints[PROGRAM_07_07] if checkpoint.spec is spec
    ]

    records = parse_log_warning_records(repo_paths.log_file(PROGRAM_07_07), spec)

    assert checkpoint.ok, checkpoint.problem
    assert checkpoint.NOBS > 0
    assert len(records) == checkpoint.NOBS
    for record in records:
        printed = record.values
        assert set(WARNING_RECORD_NAMES) <= set(printed), record.line
        rows = frame[
            (frame["USUBJID"] == printed["USUBJID"])
            & (frame["PARAMCD"] == printed["PARAMCD"])
            & _equals_displayed(frame["ATPTN"], printed["ATPTN"])
            & _equals_displayed(frame["AVISITN"], printed["AVISITN"])
        ]
        dated = (rows["ADT"] == _date9_day_number(printed["ADT"])) & _equals_displayed(
            rows["AVAL"], printed["AVAL"]
        )
        assert dated.any(), record.line
        assert set(rows["STUDYID"]) == {printed["STUDYID"]}, record.line
        assert set(rows["TRTPN"]) == {float(printed["TRTPN"])}, record.line


@pytest.mark.parametrize(
    ("text", "day_number"),
    [
        pytest.param("01JAN1960", 0, id="epoch"),
        pytest.param("02JAN1960", 1, id="day-after-epoch"),
        pytest.param("31DEC1959", -1, id="day-before-epoch"),
        pytest.param("29FEB1960", 59, id="leap-day"),
        pytest.param("01MAR1960", 60, id="after-leap-day"),
        pytest.param("01JAN1961", 366, id="after-leap-year"),
    ],
)
def test_d10_date9_day_numbers(text: str, day_number: int) -> None:
    """``DATE9.`` text converts to its day count from 1960-01-01, the SAS date scale (D-10).

    Hand-chosen dates around the epoch and the 1960 leap day, not SAS reference values. They pin
    the conversion that :func:`test_d10_baseline_date_matches_sas_log` applies to the log's dates.
    """
    assert _date9_day_number(text) == day_number


@pytest.mark.parametrize(
    "text",
    [
        pytest.param("13May2013", id="lower-case-month"),
        pytest.param("13MAI2013", id="non-english-month"),
        pytest.param("1MAY2013", id="one-digit-day"),
        pytest.param("30FEB2013", id="no-such-day"),
        pytest.param("2013-05-13", id="iso-date"),
    ],
)
def test_d10_date9_rejects(text: str) -> None:
    """Text that is not an upper-case English ``DATE9.`` calendar date is refused (D-10)."""
    with pytest.raises(ValueError):
        _date9_day_number(text)


@pytest.mark.parametrize("program_id", PROGRAMS)
def test_d11_blank_character_is_missing(
    program_id: str,
    advs: Mapping[str, tuple[pd.DataFrame, XptMetadata]],
) -> None:
    """Character values arrive as ``str`` without trailing blanks, and blanks as ``''`` (D-11).

    ``ANL01FL`` holds ``'Y'`` or a blank in both files, so the S02-FLT-02 flag test keeps exactly
    the rows that are not character-missing.
    """
    frame, _ = advs[program_id]
    character = [name for name in frame.columns if frame[name].dtype == object]
    assert character

    for name in character:
        values = frame[name]
        assert not values.isna().any(), name
        assert values.map(lambda value: isinstance(value, str)).all(), name
        assert not values.str.endswith(" ").any(), name

    flag = frame["ANL01FL"]
    missing = char_missing_mask(flag)
    assert set(flag) == {"Y", ""}
    assert missing.equals(flag == "")
    assert missing.any()
    assert (~missing).equals(flag == "Y")


def test_d11_flag_test_matches_only_y() -> None:
    """``''``, an all-blank value and ``None`` are character-missing; ``'Y'`` is not (D-11)."""
    mask = char_missing_mask(pd.Series(["Y", "", "   ", None]))

    assert mask.tolist() == [False, True, True, True]
    assert not is_char_missing("Y")
    assert is_char_missing("")
