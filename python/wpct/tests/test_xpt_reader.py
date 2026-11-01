"""Tests of ``wpct.xpt_reader``, the port's only access path to the ADVS transport files.

Spec items verified (``python/wpct/docs/sas_specification.md``):

- S02-DEP-util_access_test_data, S07-DEP-util_access_test_data: the local read of ``ADVS`` and its
  failure messages for a missing or unreadable input (AAP 0.6.4).
- S02-DEP-assert_dset_exist, S07-DEP-assert_dset_exist: the file-existence check.
- S02-DEP-assert_depend, S07-DEP-assert_depend, S02-DEP-assert_var_exist,
  S07-DEP-assert_var_exist: the ``vars=`` check of :func:`require_variables`.
- D-10: dates stay SAS day numbers. D-11: a blank character value reads back as ``''`` and is
  missing.

The repository transport files are only read. The tests write only under ``tmp_path``.
"""

from __future__ import annotations

import datetime
from collections.abc import Callable, Mapping
from pathlib import Path

import numpy as np
import pandas as pd
import pyreadstat
import pytest

from wpct.errors import InputDataError
from wpct.paths import RepoPaths
from wpct.sas_semantics import char_missing_mask, is_char_missing
from wpct.schemas import (
    PROGRAM_07_02,
    PROGRAM_07_07,
    PROGRAMS,
    REQUIRED_INPUT_VARIABLES,
)
from wpct.xpt_reader import XptMetadata, read_xpt, require_variables

#: Both ADVS files hold 32139 observations and 44 variables
#: [whitepapers/WPCT/outputs_sas/WPCT-F.07.02.log:32;
#: whitepapers/WPCT/outputs_sas/WPCT-F.07.07.log:32].
ADVS_SHAPE = (32139, 44)

#: Per program: the repository copy of its ADVS input and the ``STUDYID`` counts it holds.
EXPECTED_INPUTS: Mapping[str, tuple[str, dict[str, int]]] = {
    PROGRAM_07_02: ("data/adam/cdisc/advs.xpt", {"CDISCPILOT01": 32139}),
    PROGRAM_07_07: (
        "data/adam/cdisc-split/advs.xpt",
        {"CDISCPILOT01": 16034, "CDISCPILOT02": 16105},
    ),
}

#: The ``DATE9.``-formatted variables of ADVS (D-10).
DATE_VARIABLES = ("ADT", "TRTSDT", "TRTEDT")

#: Day 0 of the SAS date scale.
SAS_EPOCH = datetime.date(1960, 1, 1)


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
def test_require_variables_rejects_invalid_names(
    names: object, error: type[Exception], message: str
) -> None:
    """``names`` must list each variable once, as strings; a lone string is not split.

    Iterating a lone string would check each of its characters as a variable name, and a variable
    listed twice in different cases leaves no single spelling for its column.
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
def test_require_variables_returns_given_spelling(stored: tuple[str, str]) -> None:
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
    pd.testing.assert_frame_equal(result, expected)
    pd.testing.assert_frame_equal(frame, original)


def test_require_variables_rejects_case_duplicates(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    """A required variable matching two columns that differ only in case is an input error.

    SAS variable names are case-insensitive, so such a frame holds no single variable to address.
    Only the required names are checked, and a missing variable is reported first.
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


# --- Repository inputs (read-only) ---------------------------------------------------------------


@pytest.mark.parametrize("program_id", PROGRAMS)
def test_reads_advs(
    program_id: str,
    advs: Mapping[str, tuple[pd.DataFrame, XptMetadata]],
    repo_paths: RepoPaths,
) -> None:
    """Each program's ADVS reads whole, with its studies and every required variable.

    Verifies S02-DEP-util_access_test_data and S07-DEP-util_access_test_data against ``WORK.ADVS has
    32139 observations and 44 variables`` [whitepapers/WPCT/outputs_sas/WPCT-F.07.02.log:32;
    whitepapers/WPCT/outputs_sas/WPCT-F.07.07.log:32], and S02-DEP-assert_depend /
    S07-DEP-assert_depend on the real input. ``ANL02FL`` is absent from both files, because the
    07.07 derivation creates it (S07-TRN-03).
    """
    relative_path, study_counts = EXPECTED_INPUTS[program_id]
    path = repo_paths.input_xpt(program_id)
    frame, metadata = advs[program_id]

    assert path == repo_paths.repo_root / relative_path
    assert frame.shape == ADVS_SHAPE
    assert (metadata.n_rows, metadata.n_columns) == ADVS_SHAPE
    assert metadata.path == path
    assert metadata.table_name == "ADVS"
    assert metadata.column_names == tuple(frame.columns)
    assert frame["STUDYID"].value_counts().to_dict() == study_counts
    assert "ANL02FL" not in frame.columns
    require_variables(frame, REQUIRED_INPUT_VARIABLES[program_id], path, program_id)


@pytest.mark.parametrize("program_id", PROGRAMS)
def test_dates_kept_as_sas_day_numbers(
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


def test_baseline_date_matches_sas_log(
    advs: Mapping[str, tuple[pd.DataFrame, XptMetadata]],
) -> None:
    """The record SAS prints as ``AVAL=70 ADT=13MAY2013`` holds day number 19491 (D-10).

    The derivation's omitted-record warning shows the 07.07 baseline record of ``01-703-1279``
    [whitepapers/WPCT/outputs_sas/WPCT-F.07.07.log:124-128].
    """
    frame, _ = advs[PROGRAM_07_07]
    record = frame[
        (frame["USUBJID"] == "01-703-1279")
        & (frame["PARAMCD"] == "DIABP")
        & (frame["ATPTN"] == 817)
        & (frame["AVISITN"] == 0)
    ]

    assert (datetime.date(2013, 5, 13) - SAS_EPOCH).days == 19491
    assert len(record) >= 1
    assert ((record["ADT"] == 19491) & (record["AVAL"] == 70)).any()
    assert set(record["STUDYID"]) == {"CDISCPILOT01"}
    assert set(record["TRTPN"]) == {54}


@pytest.mark.parametrize("program_id", PROGRAMS)
def test_blank_character_is_missing(
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


def test_flag_test_matches_only_y() -> None:
    """``''``, an all-blank value and ``None`` are character-missing; ``'Y'`` is not (D-11)."""
    mask = char_missing_mask(pd.Series(["Y", "", "   ", None]))

    assert mask.tolist() == [False, True, True, True]
    assert not is_char_missing("Y")
    assert is_char_missing("")
