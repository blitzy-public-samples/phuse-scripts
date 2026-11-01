"""Tests of ``wpct.sas_reference``: the SAS listing, log and full-precision export parsers.

The listings ``whitepapers/WPCT/outputs_sas/WPCT-F.07.02.lst`` and ``WPCT-F.07.07.lst`` carry the
GLM ParameterEstimates tables of S02-MOD-02, S07-MOD-02 and S07-MOD-03; the logs carry the dataset
record counts pinned in Agent Action Plan 0.7.3. Every expected value below was read from those
unchanged files and is stated here independently of the parser's maps, so a map that drifted from
its file fails here rather than agreeing with itself.

The reference files are located through the ``repo_paths`` fixture, parsed only through
``wpct.sas_reference`` and never written: a test that needs a mismatching reference copies it under
``tmp_path``, edits the copy, and proves the original unchanged with ``file_state``.
"""

from __future__ import annotations

import functools
import math
from collections.abc import Callable, Mapping
from pathlib import Path
from typing import NamedTuple

import pytest

from wpct import compare
from wpct.errors import InputDataError
from wpct.paths import RepoPaths
from wpct.programs import ProgramResult
from wpct.sas_reference import (
    GLM_TABLE_MAP,
    KIND_DATASET,
    KIND_TABLE,
    KIND_WARNING_COUNT,
    LOG_CHECKPOINT_MAP,
    WARNING_COUNT_PREFIXES,
    ListingTable,
    LogCheckpointSpec,
    LogWarningRecord,
    SasCheckpoint,
    load_sas_exports,
    parse_glm_listing,
    parse_log_checkpoints,
    parse_log_warning_records,
    split_parameter,
)
from wpct.schemas import (
    PROGRAM_07_02,
    PROGRAM_07_07,
    PROGRAMS,
    SAS_EXPORT_FILES,
    export_spec,
)

#: The SAS design-column order of every listed model (D-05, D-07): Intercept, BASE, the TRTPN
#: levels ascending with the REF="0" level last; the pooled 07.07 model adds the continuous
#: ``studynum``, aliased by the Intercept (D-06, DIV-01).
_PARAMETERS = (("INTERCEPT", ""), ("BASE", ""), ("TRTPN", "54"), ("TRTPN", "81"), ("TRTPN", "0"))
_POOLED_PARAMETERS = (*_PARAMETERS, ("STUDYNUM", ""))

#: The WARNING ``%derive_lastminmax_measure`` writes per omitted baseline record (LMM-FLT-02); the
#: macro spells its tag MEASURES [whitepapers/WPCT/outputs_sas/WPCT-F.07.07.log:124-178].
_OMITTED_WARNING = (
    "WARNING: (DERIVE_LASTMINMAX_MEASURES) Omitting obs without a post-baseline measure"
)


def _edited_copy(source: Path, destination_dir: Path, old: str, new: str) -> Path:
    """Write ``source`` to ``destination_dir`` under its own name with the first ``old`` replaced.

    The copy keeps the file name, so a listing copy still names its program. Latin-1 maps every
    byte to one character, so the copy differs from the original only at the edit.
    """
    text = source.read_bytes().decode("latin-1")
    assert old in text, f"{old!r} does not occur in {source}"
    copy = destination_dir / source.name
    copy.write_bytes(text.replace(old, new, 1).encode("latin-1"))
    return copy


def _edited_line_copy(source: Path, destination_dir: Path, line: int, old: str, new: str) -> Path:
    """Write ``source`` to ``destination_dir`` with ``old`` replaced on 1-based ``line`` only."""
    lines = source.read_bytes().split(b"\n")
    target = lines[line - 1].decode("latin-1")
    assert old in target, f"{old!r} does not occur on line {line} of {source}"
    lines[line - 1] = target.replace(old, new, 1).encode("latin-1")
    copy = destination_dir / source.name
    copy.write_bytes(b"\n".join(lines))
    return copy


# --- GLM listings ---------------------------------------------------------------------------------


def test_s02_mod_02_listing_row_counts(
    listing_tables: Mapping[str, tuple[ListingTable, ...]]
) -> None:
    """WPCT-F.07.02.lst holds 2 endpoint tables of 5 rows, 10 rows in all (S02-MOD-02).

    [whitepapers/WPCT/outputs_sas/WPCT-F.07.02.lst:8-14,23-29]: no BY line, dependent ``CHG``,
    rows Intercept, BASE, TRTPN 54, TRTPN 81, TRTPN 0.
    """
    tables = listing_tables[PROGRAM_07_02]

    assert len(tables) == 2
    assert sum(len(table.rows) for table in tables) == 10
    assert [table.index for table in tables] == [0, 1]
    assert [table.by_line for table in tables] == [None, None]
    assert [table.dependent for table in tables] == ["CHG", "CHG"]
    assert [(table.first_line, table.last_line) for table in tables] == [(8, 14), (23, 29)]
    for table in tables:
        assert [row.PARAMETER for row in table.rows] == [
            "Intercept",
            "BASE",
            "TRTPN 54",
            "TRTPN 81",
            "TRTPN 0",
        ]
        assert tuple((row.EFFECT, row.LEVEL) for row in table.rows) == _PARAMETERS
        assert table.spec is GLM_TABLE_MAP[PROGRAM_07_02][table.index]
        assert table.problems == ()


def test_s07_mod_02_03_listing_row_counts(
    listing_tables: Mapping[str, tuple[ListingTable, ...]]
) -> None:
    """WPCT-F.07.07.lst holds 4 per-study tables of 5 rows and 2 pooled of 6, 32 rows in all.

    S07-MOD-02 fits BY ``studynum``, so its tables carry ``studynum=`` BY lines; the S07-MOD-03
    pooled fits carry none and end with the aliased ``studynum`` row. SAS prints the dependent
    and the baseline in lower case (``chg``, ``base``)
    [whitepapers/WPCT/outputs_sas/WPCT-F.07.07.lst:3-47,51-147].
    """
    tables = listing_tables[PROGRAM_07_07]

    assert len(tables) == 6
    assert [len(table.rows) for table in tables] == [5, 5, 6, 5, 5, 6]
    assert sum(len(table.rows) for table in tables) == 32
    assert [table.by_line for table in tables] == [
        "studynum=1",
        "studynum=2",
        None,
        "studynum=1",
        "studynum=2",
        None,
    ]
    assert [table.dependent for table in tables] == ["chg"] * 6
    assert [(table.first_line, table.last_line) for table in tables] == [
        (10, 16),
        (26, 32),
        (40, 47),
        (58, 64),
        (100, 106),
        (140, 147),
    ]
    for table in tables:
        pooled = table.by_line is None
        assert tuple((row.EFFECT, row.LEVEL) for row in table.rows) == (
            _POOLED_PARAMETERS if pooled else _PARAMETERS
        )
        assert table.rows[1].PARAMETER == "base"
        assert table.rows[-1].PARAMETER == ("studynum" if pooled else "TRTPN 0")
        assert table.spec is GLM_TABLE_MAP[PROGRAM_07_07][table.index]
        assert table.problems == ()


@pytest.mark.parametrize("program_id", PROGRAMS)
def test_listing_default_map_from_file_name(
    program_id: str,
    repo_paths: RepoPaths,
    listing_tables: Mapping[str, tuple[ListingTable, ...]],
) -> None:
    """Without ``table_map`` the listing's file name selects its program's map."""
    assert parse_glm_listing(repo_paths.listing_file(program_id)) == listing_tables[program_id]


class _Tokens(NamedTuple):
    """One listing row's displayed values: estimate, ``B`` flag, SE, t and ``Pr > |t|``."""

    estimate: str
    biased: bool
    stderr: str
    tvalue: str
    probt: str


@pytest.mark.parametrize(
    "program_id, table_index, parameter, line, expected",
    [
        pytest.param(
            PROGRAM_07_02,
            0,
            "Intercept",
            10,
            _Tokens("33.33892013", True, "3.90872985", "8.53", "<.0001"),
            id="07.02-lst10-pvalue-bound",
        ),
        pytest.param(
            PROGRAM_07_02,
            0,
            "BASE",
            11,
            _Tokens("-0.46079578", False, "0.05043555", "-9.14", "<.0001"),
            id="07.02-lst11-negative-not-biased",
        ),
        pytest.param(
            PROGRAM_07_02,
            0,
            "TRTPN 0",
            14,
            _Tokens("0.00000000", True, ".", ".", "."),
            id="07.02-lst14-dot-missing",
        ),
        # SAS prints missing values as blanks once options missing=' ' takes effect.
        pytest.param(
            PROGRAM_07_02,
            1,
            "TRTPN 0",
            29,
            _Tokens("0.00000000", True, "", "", ""),
            id="07.02-lst29-blank-missing",
        ),
        # The pooled studynum is aliased by the Intercept (D-06, DIV-01).
        pytest.param(
            PROGRAM_07_07,
            2,
            "studynum",
            47,
            _Tokens("0.00000000", True, ".", ".", "."),
            id="07.07-lst47-pooled-815-studynum-dot",
        ),
        pytest.param(
            PROGRAM_07_07,
            5,
            "studynum",
            147,
            _Tokens("0.00000000", True, "", "", ""),
            id="07.07-lst147-pooled-817-studynum-blank",
        ),
        pytest.param(
            PROGRAM_07_07,
            3,
            "TRTPN 54",
            62,
            _Tokens("0.00134690", True, "1.48321417", "0.00", "0.9993"),
            id="07.07-lst62-817-study1-near-zero",
        ),
    ],
)
def test_listing_tokens(
    program_id: str,
    table_index: int,
    parameter: str,
    line: int,
    expected: _Tokens,
    listing_tables: Mapping[str, tuple[ListingTable, ...]],
) -> None:
    """Displayed tokens are kept as printed: ``<.0001``, ``.`` and blank all survive the parse."""
    table = listing_tables[program_id][table_index]
    (row,) = [row for row in table.rows if row.PARAMETER == parameter]

    assert row.line == line
    assert _Tokens(row.ESTIMATE, row.BIASED, row.STDERR, row.TVALUE, row.PROBT) == expected


def test_s02_trn_04_and_s07_trn_07_glm_table_map_order() -> None:
    """Tables map to strata by SAS loop order, not by the page titles, which lag one iteration.

    07.02 runs DIABP/815 then SYSBP/816 at the endpoint visit 99 (S02-TRN-04, S02-MOD-02); 07.07
    runs DIABP/815 for studynum 1, 2 and pooled, then DIABP/817 for the same three (S07-TRN-07,
    S07-MOD-02, S07-MOD-03) [whitepapers/WPCT/outputs_sas/WPCT-F.07.02.lst:15].
    """
    layout = {
        program_id: [
            (spec.index, spec.PARAMCD, spec.ATPTN, spec.MODEL, spec.STUDYNUM, spec.by_line)
            for spec in specs
        ]
        for program_id, specs in GLM_TABLE_MAP.items()
    }

    assert layout == {
        PROGRAM_07_02: [
            (0, "DIABP", 815.0, "endpoint", None, None),
            (1, "SYSBP", 816.0, "endpoint", None, None),
        ],
        PROGRAM_07_07: [
            (0, "DIABP", 815.0, "study", 1, "studynum=1"),
            (1, "DIABP", 815.0, "study", 2, "studynum=2"),
            (2, "DIABP", 815.0, "pooled", None, None),
            (3, "DIABP", 817.0, "study", 1, "studynum=1"),
            (4, "DIABP", 817.0, "study", 2, "studynum=2"),
            (5, "DIABP", 817.0, "pooled", None, None),
        ],
    }
    assert [spec.AVISITN for spec in GLM_TABLE_MAP[PROGRAM_07_02]] == [99.0, 99.0]
    assert [spec.AVISITN for spec in GLM_TABLE_MAP[PROGRAM_07_07]] == [None] * 6
    assert [spec.parameters for spec in GLM_TABLE_MAP[PROGRAM_07_07]] == [
        _PARAMETERS,
        _PARAMETERS,
        _POOLED_PARAMETERS,
        _PARAMETERS,
        _PARAMETERS,
        _POOLED_PARAMETERS,
    ]


@pytest.mark.parametrize(
    "old, new, table_index, problem",
    [
        # [WPCT-F.07.07.lst:19] the second per-study table's BY line.
        (
            "studynum=2",
            "studynum=9",
            1,
            "table 1: BY line 'studynum=9' differs from the expected 'studynum=2'",
        ),
        # [WPCT-F.07.07.lst:7] the first table's dependent variable.
        (
            "Dependent Variable: chg",
            "Dependent Variable: aval",
            0,
            "table 0: dependent variable aval is not CHG",
        ),
        # [WPCT-F.07.07.lst:15] a CLASS level of the first table's parameter rows.
        ("TRTPN     81", "TRTPN     82", 0, "table 0: parameter rows"),
    ],
    ids=["by-line", "dependent", "parameter-row"],
)
def test_listing_mismatch_reported(
    old: str,
    new: str,
    table_index: int,
    problem: str,
    tmp_path: Path,
    repo_paths: RepoPaths,
    file_state: Callable[[Path], object],
) -> None:
    """A listing table that differs from its map entry is reported, never silently accepted.

    The edited table keeps its rows and gains exactly one problem, which ``wpct.compare`` reports
    as FAIL; every other table stays clean.
    """
    reference = repo_paths.listing_file(PROGRAM_07_07)
    before = file_state(reference)
    copy = _edited_copy(reference, tmp_path, old, new)

    tables = parse_glm_listing(copy, GLM_TABLE_MAP[PROGRAM_07_07])

    assert len(tables) == 6
    assert [len(table.rows) for table in tables] == [5, 5, 6, 5, 5, 6]
    edited = tables[table_index]
    assert len(edited.problems) == 1
    assert problem in edited.problems[0]
    assert [table.problems for table in tables if table.index != table_index] == [()] * 5
    assert parse_glm_listing(copy) == tables
    assert file_state(reference) == before


def _assert_listing_failure_propagates(
    program_id: str,
    tables: tuple[ListingTable, ...],
    table_index: int,
    clean: tuple[ListingTable, ...],
    result: ProgramResult,
    tolerances: compare.Tolerances,
) -> None:
    """Keep all expected quantities and fail exactly the damaged table in both listing groups.

    The clean table's rendered comparison keys identify its stratum, including pooled study
    numbering and the merged cells' TRTPN levels, without duplicating comparison formatting.
    """
    counts = {PROGRAM_07_02: (40, 6), PROGRAM_07_07: (128, 18)}[program_id]
    table = tables[table_index]
    unexpected = table.spec is None
    reason = "; ".join(table.problems)
    assert reason
    for compare_group, count in zip(
        (compare.compare_listing, compare.compare_merged_pval), counts, strict=True
    ):
        baseline = compare_group(program_id, clean, result, tolerances)
        group = compare_group(program_id, tables, result, tolerances)
        assert len(baseline.rows) == baseline.n_pass == count
        assert baseline.n_not_compared == 0
        baseline_by_key = {(row.quantity, row.stratum): row for row in baseline.rows}
        assert len(baseline_by_key) == count
        expected_keys = list(baseline_by_key)
        if unexpected:
            affected_keys = {(f"listing table {table_index}", "—")}
            expected_keys.append((f"listing table {table_index}", "—"))
        else:
            affected = compare_group(program_id, (clean[table_index],), result, tolerances)
            affected_keys = {(row.quantity, row.stratum) for row in affected.rows}
            expected_affected = (
                4 * len(clean[table_index].rows) if compare_group is compare.compare_listing else 3
            )
            assert len(affected_keys) == expected_affected
            assert affected_keys <= baseline_by_key.keys()
        assert len(group.rows) == count + int(unexpected)
        assert [(row.quantity, row.stratum) for row in group.rows] == expected_keys
        assert group.n_not_compared == 0
        assert group.n_fail == len(affected_keys)
        assert group.n_pass == len(group.rows) - len(affected_keys)
        for row in group.rows:
            key = (row.quantity, row.stratum)
            if key in affected_keys:
                assert row.status == compare.FAIL
                assert row.reason == reason
            else:
                assert row.status == compare.PASS
                assert row == baseline_by_key[key]


@pytest.mark.parametrize(
    "program_id, last_line, table_index",
    [
        pytest.param(PROGRAM_07_02, 22, 1, id="07.02-missing-endpoint-table"),
        pytest.param(PROGRAM_07_07, 139, 5, id="07.07-missing-pooled-table"),
    ],
)
def test_s02_mod_02_s07_mod_03_missing_listing_table_fails(
    program_id: str,
    last_line: int,
    table_index: int,
    tmp_path: Path,
    repo_paths: RepoPaths,
    file_state: Callable[[Path], object],
    listing_tables: Mapping[str, tuple[ListingTable, ...]],
    results: Mapping[str, ProgramResult],
    tolerances: compare.Tolerances,
) -> None:
    """A missing final table retains its expected model and merged-pval quantities as FAIL.

    Cut before the header [whitepapers/WPCT/outputs_sas/WPCT-F.07.02.lst:23-29] or
    [whitepapers/WPCT/outputs_sas/WPCT-F.07.07.lst:140-147] (S02-MOD-02, S07-MOD-03,
    S02-TRN-05, S07-TRN-08); the preceding complete tables remain valid.
    """
    reference = repo_paths.listing_file(program_id)
    before = file_state(reference)
    lines = reference.read_bytes().split(b"\n")
    copy = tmp_path / reference.name
    copy.write_bytes(b"\n".join(lines[:last_line]) + b"\n")

    tables = parse_glm_listing(copy)

    assert len(tables) == table_index + 1
    assert sum(table.first_line is not None for table in tables) == table_index
    missing = tables[table_index]
    assert missing.index == table_index
    assert (missing.by_line, missing.dependent, missing.first_line, missing.last_line) == (
        None,
        None,
        None,
        None,
    )
    assert missing.rows == ()
    assert missing.spec is GLM_TABLE_MAP[program_id][table_index]
    assert missing.problems == (f"table {table_index} missing from listing",)
    assert tables[:table_index] == listing_tables[program_id][:table_index]
    assert all(table.problems == () for table in tables[:table_index])
    _assert_listing_failure_propagates(
        program_id, tables, table_index, listing_tables[program_id], results[program_id], tolerances
    )
    assert file_state(reference) == before


@pytest.mark.parametrize(
    "program_id, last_line, table_index, header_line, pinned_last_line, parameters",
    [
        pytest.param(
            PROGRAM_07_02,
            27,
            1,
            23,
            29,
            "(INTERCEPT, BASE, TRTPN 54, TRTPN 81, TRTPN 0)",
            id="07.02-truncated-endpoint-table",
        ),
        pytest.param(
            PROGRAM_07_07,
            144,
            5,
            140,
            147,
            "(INTERCEPT, BASE, TRTPN 54, TRTPN 81, TRTPN 0, STUDYNUM)",
            id="07.07-truncated-pooled-table",
        ),
    ],
)
def test_s02_mod_02_s07_mod_03_truncated_listing_table_fails(
    program_id: str,
    last_line: int,
    table_index: int,
    header_line: int,
    pinned_last_line: int,
    parameters: str,
    tmp_path: Path,
    repo_paths: RepoPaths,
    file_state: Callable[[Path], object],
    listing_tables: Mapping[str, tuple[ListingTable, ...]],
    results: Mapping[str, ProgramResult],
    tolerances: compare.Tolerances,
) -> None:
    """EOF after TRTPN 54 fails both the parameter sequence and pinned line range.

    Only three rows survive [whitepapers/WPCT/outputs_sas/WPCT-F.07.02.lst:25-29] or
    [whitepapers/WPCT/outputs_sas/WPCT-F.07.07.lst:142-147] (S02-MOD-02, S07-MOD-03);
    all model and merge quantities still appear (S02-TRN-05, S07-TRN-08).
    """
    reference = repo_paths.listing_file(program_id)
    before = file_state(reference)
    lines = reference.read_bytes().split(b"\n")
    copy = tmp_path / reference.name
    copy.write_bytes(b"\n".join(lines[:last_line]) + b"\n")

    tables = parse_glm_listing(copy)

    assert len(tables) == table_index + 1
    truncated = tables[table_index]
    assert truncated.index == table_index
    assert truncated.spec is GLM_TABLE_MAP[program_id][table_index]
    clean = listing_tables[program_id][table_index]
    assert (truncated.by_line, truncated.dependent) == (clean.by_line, clean.dependent)
    assert (truncated.first_line, truncated.last_line) == (header_line, last_line)
    assert len(truncated.rows) == 3
    assert truncated.rows == clean.rows[:3]
    assert truncated.problems == (
        (
            f"table {table_index}: parameter rows (INTERCEPT, BASE, TRTPN 54) "
            f"differ from the expected {parameters}"
        ),
        (
            f"table {table_index}: lines {header_line}-{last_line} differ from the pinned lines "
            f"{header_line}-{pinned_last_line}"
        ),
    )
    assert tables[:table_index] == listing_tables[program_id][:table_index]
    assert all(table.problems == () for table in tables[:table_index])
    _assert_listing_failure_propagates(
        program_id, tables, table_index, listing_tables[program_id], results[program_id], tolerances
    )
    assert file_state(reference) == before


@pytest.mark.parametrize(
    "program_id, page_end, header_line, table_index",
    [
        pytest.param(PROGRAM_07_02, 14, 8, 2, id="07.02-unexpected-third-table"),
        pytest.param(PROGRAM_07_07, 16, 10, 6, id="07.07-unexpected-seventh-table"),
    ],
)
def test_s02_mod_02_s07_mod_02_unexpected_listing_table_fails(
    program_id: str,
    page_end: int,
    header_line: int,
    table_index: int,
    tmp_path: Path,
    repo_paths: RepoPaths,
    file_state: Callable[[Path], object],
    listing_tables: Mapping[str, tuple[ListingTable, ...]],
    results: Mapping[str, ProgramResult],
    tolerances: compare.Tolerances,
) -> None:
    """An extra complete table adds one unstratified FAIL to each comparison group.

    Append the first page [whitepapers/WPCT/outputs_sas/WPCT-F.07.02.lst:1-14] or
    [whitepapers/WPCT/outputs_sas/WPCT-F.07.07.lst:1-16] (S02-MOD-02, S07-MOD-02,
    S02-TRN-05, S07-TRN-08), leaving every mapped table clean.
    """
    reference = repo_paths.listing_file(program_id)
    before = file_state(reference)
    content = reference.read_bytes()
    assert content.endswith(b"\n")
    lines = content.split(b"\n")[:-1]
    copy = tmp_path / reference.name
    copy.write_bytes(content + b"\n".join(lines[:page_end]) + b"\n")

    tables = parse_glm_listing(copy)

    assert len(tables) == table_index + 1
    extra = tables[table_index]
    assert extra.index == table_index
    assert extra.spec is None
    clean = listing_tables[program_id][0]
    assert (extra.by_line, extra.dependent) == (clean.by_line, clean.dependent)
    assert (extra.first_line, extra.last_line) == (
        len(lines) + header_line,
        len(lines) + page_end,
    )
    assert len(extra.rows) == 5
    assert tuple((row.EFFECT, row.LEVEL) for row in extra.rows) == _PARAMETERS
    assert extra.problems == (
        (
            f"table {table_index} at line {len(lines) + header_line}: unexpected table "
            f"(the map expects {table_index})"
        ),
    )
    assert tables[:table_index] == listing_tables[program_id]
    assert all(table.problems == () for table in tables[:table_index])
    _assert_listing_failure_propagates(
        program_id, tables, table_index, listing_tables[program_id], results[program_id], tolerances
    )
    assert file_state(reference) == before


@pytest.mark.parametrize(
    "old, new, problem",
    [
        pytest.param(
            "3.90872985",
            "3.9O872985",
            "unrecognised displayed values 3.9O872985",
            id="invalid-stderr-token",
        ),
        pytest.param(
            "8.53",
            "8.O3",
            "unrecognised displayed values 8.O3",
            id="invalid-tvalue-token",
        ),
        pytest.param(
            "<.0001",
            "<0.0001",
            "unrecognised displayed values <0.0001",
            id="invalid-pvalue-bound",
        ),
        pytest.param(
            "8.53",
            "",
            "expected 3 values or none after the estimate, found 2",
            id="missing-tvalue-token",
        ),
    ],
)
def test_s02_mod_02_rejected_listing_tokens_fail(
    old: str,
    new: str,
    problem: str,
    tmp_path: Path,
    repo_paths: RepoPaths,
    file_state: Callable[[Path], object],
    listing_tables: Mapping[str, tuple[ListingTable, ...]],
    results: Mapping[str, ProgramResult],
    tolerances: compare.Tolerances,
) -> None:
    """Reject malformed SE, t or p tokens, retaining all expected comparisons as FAIL.

    Edit only the Intercept [whitepapers/WPCT/outputs_sas/WPCT-F.07.02.lst:10]; the row is
    rejected, so S02-MOD-02's parameter sequence also differs, and S02-TRN-05's three merged
    cells fail even though their own TRTPN listing rows are unchanged.
    """
    reference = repo_paths.listing_file(PROGRAM_07_02)
    before = file_state(reference)
    original = reference.read_bytes().split(b"\n")[9].decode("latin-1").rstrip(" ")
    edited_line = original.replace(old, new, 1)
    copy = _edited_line_copy(reference, tmp_path, 10, old, new)

    tables = parse_glm_listing(copy)

    assert len(tables) == 2
    rejected = tables[0]
    assert rejected.index == 0
    assert rejected.spec is GLM_TABLE_MAP[PROGRAM_07_02][0]
    assert (rejected.by_line, rejected.dependent) == (None, "CHG")
    assert (rejected.first_line, rejected.last_line) == (8, 14)
    assert len(rejected.rows) == 4
    assert all(row.line != 10 and row.PARAMETER != "Intercept" for row in rejected.rows)
    assert rejected.rows == listing_tables[PROGRAM_07_02][0].rows[1:]
    assert rejected.problems == (
        f"table 0: line 10: {problem}: {edited_line}",
        (
            "table 0: parameter rows (BASE, TRTPN 54, TRTPN 81, TRTPN 0) differ from the expected "
            "(INTERCEPT, BASE, TRTPN 54, TRTPN 81, TRTPN 0)"
        ),
    )
    assert tables[1] == listing_tables[PROGRAM_07_02][1]
    assert tables[1].problems == ()
    _assert_listing_failure_propagates(
        PROGRAM_07_02, tables, 0, listing_tables[PROGRAM_07_02], results[PROGRAM_07_02], tolerances
    )
    assert file_state(reference) == before


@pytest.mark.parametrize(
    "label, expected",
    [
        ("TRTPN     54", ("TRTPN", "54")),
        ("Intercept", ("INTERCEPT", "")),
        ("base", ("BASE", "")),
        ("BASE", ("BASE", "")),
        ("studynum", ("STUDYNUM", "")),
    ],
    ids=["class-level", "intercept", "lower-case-base", "upper-case-base", "pooled-studynum"],
)
def test_split_parameter(label: str, expected: tuple[str, str]) -> None:
    """Labels key on the canonical ``(EFFECT, LEVEL)`` pair whatever case SAS printed."""
    assert split_parameter(label) == expected


@pytest.mark.parametrize(
    "label, error",
    [("", ValueError), ("   ", ValueError), ("TRTPN 54 extra", ValueError), (None, TypeError)],
    ids=["empty", "blank", "three-words", "not-a-string"],
)
def test_split_parameter_rejects(label: object, error: type[Exception]) -> None:
    """A label that is not one effect word, optionally followed by a CLASS level, is refused."""
    with pytest.raises(error):
        split_parameter(label)


# --- Log checkpoints ------------------------------------------------------------------------------


class _Pinned(NamedTuple):
    """One pinned log count of Agent Action Plan 0.7.3, as the log prints it."""

    line: int
    dataset: str
    kind: str
    nobs: int
    nvars: int | None
    paramcd: str | None = None
    atptn: float | None = None
    end_line: int | None = None


_PINNED_07_02 = (
    _Pinned(32, "ADVS", KIND_DATASET, 32139, 44),
    _Pinned(59, "ADVS_SUB", KIND_DATASET, 5925, 45),
    _Pinned(202, "CSS_ANADATA", KIND_DATASET, 3548, 14),
    _Pinned(726, "CSS_NEXTPARAM", KIND_DATASET, 1774, None, "DIABP"),
    _Pinned(803, "CSS_NEXTTIMEPT", KIND_DATASET, 1774, None, "DIABP", 815),
    _Pinned(978, "CSS_STATS", KIND_DATASET, 30, 12, "DIABP", 815),
    _Pinned(992, "CSS_PVALUES", KIND_DATASET, 5, None, "DIABP", 815),
    _Pinned(1024, "CSS_PVALUES", KIND_DATASET, 3, None, "DIABP", 815),
    _Pinned(1049, "CSS_STATS", KIND_DATASET, 30, 13, "DIABP", 815),
    _Pinned(1141, "CSS_NEXTPARAM", KIND_DATASET, 1774, None, "SYSBP"),
    _Pinned(1219, "CSS_NEXTTIMEPT", KIND_DATASET, 1774, None, "SYSBP", 816),
    _Pinned(1400, "CSS_STATS", KIND_DATASET, 30, 12, "SYSBP", 816),
    _Pinned(1414, "CSS_PVALUES", KIND_DATASET, 5, None, "SYSBP", 816),
    _Pinned(1446, "CSS_PVALUES", KIND_DATASET, 3, None, "SYSBP", 816),
    _Pinned(1471, "CSS_STATS", KIND_DATASET, 30, 13, "SYSBP", 816),
)

_PINNED_07_07 = (
    _Pinned(32, "ADVS", KIND_DATASET, 32139, 44),
    _Pinned(61, "ADVS_SUB", KIND_DATASET, 5926, 45),
    _Pinned(79, "LMM_BASE", KIND_DATASET, 1514, None),
    _Pinned(80, "LMM_POST", KIND_DATASET, 4411, None),
    _Pinned(95, "LMM_BASE_ANL", KIND_TABLE, 629, None),
    _Pinned(98, "LMM_POST_ANL", KIND_TABLE, 794, None),
    _Pinned(108, "LMM_BASE_ANL", KIND_DATASET, 508, None),
    _Pinned(117, "LMM_POST_ANL", KIND_DATASET, 498, None),
    _Pinned(124, "LMM_BASE_OMITTED", KIND_WARNING_COUNT, 10, None, end_line=178),
    _Pinned(186, "LMM_BASE_ANL", KIND_DATASET, 498, None),
    _Pinned(195, "LMM_POST_ANL", KIND_DATASET, 498, None),
    _Pinned(205, "ADVS_MIN", KIND_DATASET, 996, 16),
    _Pinned(229, "ADVS_MIN", KIND_DATASET, 498, 16),
    _Pinned(367, "CSS_SAFANA", KIND_DATASET, 498, 12),
    _Pinned(399, "CSS_ANADATA", KIND_DATASET, 996, 12),
    _Pinned(427, "CSS_ANADATA", KIND_DATASET, 996, 13),
    _Pinned(859, "CSS_NEXTPARAM", KIND_DATASET, 996, None, "DIABP"),
    _Pinned(936, "CSS_NEXTTIMEPT", KIND_DATASET, 500, None, "DIABP", 815),
    _Pinned(1111, "CSS_STATS", KIND_DATASET, 9, 12, "DIABP", 815),
    _Pinned(1128, "CSS_PVAL_STDY", KIND_DATASET, 10, None, "DIABP", 815),
    _Pinned(1144, "CSS_PVAL_POOL", KIND_DATASET, 6, None, "DIABP", 815),
    _Pinned(1162, "CSS_PVAL_STDY", KIND_DATASET, 9, None, "DIABP", 815),
    _Pinned(1215, "CSS_STATS", KIND_DATASET, 9, 13, "DIABP", 815),
    _Pinned(1295, "CSS_NEXTTIMEPT", KIND_DATASET, 496, None, "DIABP", 817),
    _Pinned(1476, "CSS_STATS", KIND_DATASET, 9, 12, "DIABP", 817),
    _Pinned(1493, "CSS_PVAL_STDY", KIND_DATASET, 10, None, "DIABP", 817),
    _Pinned(1509, "CSS_PVAL_POOL", KIND_DATASET, 6, None, "DIABP", 817),
    _Pinned(1528, "CSS_PVAL_STDY", KIND_DATASET, 9, None, "DIABP", 817),
    _Pinned(1581, "CSS_STATS", KIND_DATASET, 9, 13, "DIABP", 817),
)


def _pinned_cases(pinned: tuple[_Pinned, ...]) -> list[pytest.ParameterSet]:
    """One map position per pinned line, in log order, identified by line and dataset."""
    return [
        pytest.param(position, id=f"log{entry.line}-{entry.dataset}")
        for position, entry in enumerate(pinned)
    ]


def _assert_pinned(
    program_id: str,
    position: int,
    pinned: tuple[_Pinned, ...],
    sas_checkpoints: Mapping[str, tuple[SasCheckpoint, ...]],
) -> None:
    """Check map entry ``position`` against the pinned line and the count parsed from the log."""
    expected = pinned[position]
    spec = LOG_CHECKPOINT_MAP[program_id][position]
    checkpoint = sas_checkpoints[program_id][position]
    occurrence = sum(1 for entry in pinned[: position + 1] if entry.dataset == expected.dataset)

    assert (spec.line, spec.end_line, spec.SAS_DATASET, spec.kind) == (
        expected.line,
        expected.end_line,
        expected.dataset,
        expected.kind,
    )
    assert spec.OCCURRENCE == occurrence
    assert (spec.PARAMCD, spec.ATPTN) == (expected.paramcd, expected.atptn)
    assert spec.has_nvars is (expected.nvars is not None)
    assert checkpoint.spec is spec
    assert checkpoint.ok, checkpoint.problem
    assert checkpoint.problem is None
    assert (checkpoint.NOBS, checkpoint.NVARS) == (expected.nobs, expected.nvars)
    if expected.kind == KIND_WARNING_COUNT:
        assert WARNING_COUNT_PREFIXES[expected.dataset] == _OMITTED_WARNING
        assert checkpoint.found_dataset is None
        assert checkpoint.line_text.startswith(_OMITTED_WARNING)
    else:
        assert checkpoint.found_dataset == expected.dataset
        assert f"WORK.{expected.dataset} " in checkpoint.line_text



def test_log_checkpoint_map_sizes() -> None:
    """AAP 0.7.3 pins 15 log lines of WPCT-F.07.02 and 29 of WPCT-F.07.07."""
    assert tuple(LOG_CHECKPOINT_MAP) == PROGRAMS
    assert len(LOG_CHECKPOINT_MAP[PROGRAM_07_02]) == len(_PINNED_07_02) == 15
    assert len(LOG_CHECKPOINT_MAP[PROGRAM_07_07]) == len(_PINNED_07_07) == 29


@pytest.mark.parametrize("position", _pinned_cases(_PINNED_07_02))
def test_s02_flt_01_to_trn_05_log_pinned_lines(
    position: int,
    sas_checkpoints: Mapping[str, tuple[SasCheckpoint, ...]],
) -> None:
    """Each pinned WPCT-F.07.02.log line names its dataset and prints the AAP 0.7.3 count.

    The input and analysis sets (S02-DEP-util_access_test_data, S02-FLT-01, S02-POP-01), then per
    stratum the records (S02-TRN-04), statistics (S02-MOD-01), ParameterEstimates (S02-MOD-02)
    and merged statistics (S02-TRN-05) [whitepapers/WPCT/outputs_sas/WPCT-F.07.02.log:32-1471].
    """
    _assert_pinned(PROGRAM_07_02, position, _PINNED_07_02, sas_checkpoints)


@pytest.mark.parametrize("position", _pinned_cases(_PINNED_07_07))
def test_lmm_flt_01_to_s07_trn_08_log_pinned_lines(
    position: int,
    sas_checkpoints: Mapping[str, tuple[SasCheckpoint, ...]],
) -> None:
    """Each pinned WPCT-F.07.07.log line names its dataset and prints the AAP 0.7.3 count.

    The derivation counts (LMM-FLT-01 to LMM-TRN-07) include the PROC SQL row counts, the
    NODUPKEY results and the 10 omitted-baseline WARNING lines of LMM-FLT-02; then the pooled
    analysis set (S07-TRN-05, S07-TRN-06) and per timepoint the statistics (S07-MOD-01), the
    per-study and pooled ParameterEstimates (S07-MOD-02, S07-MOD-03) and the merge (S07-TRN-08)
    [whitepapers/WPCT/outputs_sas/WPCT-F.07.07.log:32-1581].
    """
    _assert_pinned(PROGRAM_07_07, position, _PINNED_07_07, sas_checkpoints)


@pytest.mark.parametrize(
    "program_id, line, old, new, position, problem",
    [
        # [WPCT-F.07.02.log:202] the analysis set's NOTE names another dataset.
        (
            PROGRAM_07_02,
            202,
            "WORK.CSS_ANADATA ",
            "WORK.CSS_ANADATX ",
            2,
            "line 202 does not name WORK.CSS_ANADATA: NOTE: The data set WORK.CSS_ANADATX has",
        ),
        # [WPCT-F.07.07.log:178] the last omitted-baseline WARNING of LMM-FLT-02 no longer ends
        # the pinned range.
        (
            PROGRAM_07_07,
            178,
            "WARNING: (DERIVE_LASTMINMAX_MEASURES)",
            "NOTE: (DERIVE_LASTMINMAX_MEASURES)",
            8,
            "line 178 does not start the warning",
        ),
    ],
    ids=["07.02-log202-dataset", "07.07-log178-warning-range"],
)
def test_log_mismatch_reported(
    program_id: str,
    line: int,
    old: str,
    new: str,
    position: int,
    problem: str,
    tmp_path: Path,
    repo_paths: RepoPaths,
    file_state: Callable[[Path], object],
) -> None:
    """A pinned line that no longer holds its count is reported with no count, never dropped."""
    reference = repo_paths.log_file(program_id)
    before = file_state(reference)
    copy = _edited_line_copy(reference, tmp_path, line, old, new)
    checkpoint_map = LOG_CHECKPOINT_MAP[program_id]

    checkpoints = parse_log_checkpoints(copy, checkpoint_map)

    assert len(checkpoints) == len(checkpoint_map)
    assert [checkpoint.spec for checkpoint in checkpoints] == list(checkpoint_map)
    failed = checkpoints[position]
    assert failed.spec.line == line or failed.spec.end_line == line
    assert not failed.ok
    assert (failed.NOBS, failed.NVARS) == (None, None)
    assert failed.problem is not None
    assert problem in failed.problem
    assert [checkpoint.ok for checkpoint in checkpoints if checkpoint is not failed] == [True] * (
        len(checkpoint_map) - 1
    )
    assert file_state(reference) == before


@pytest.mark.parametrize(
    "parse, file_name, message",
    [
        (
            parse_glm_listing,
            f"{PROGRAM_07_02}.lst",
            "reference listing not found: ",
        ),
        (
            functools.partial(parse_log_checkpoints, map=LOG_CHECKPOINT_MAP[PROGRAM_07_02]),
            f"{PROGRAM_07_02}.log",
            "reference log not found: ",
        ),
    ],
    ids=["listing", "log"],
)
def test_missing_reference_raises(
    parse: Callable[[Path], object], file_name: str, message: str, tmp_path: Path
) -> None:
    """Only a missing or unreadable reference raises, naming its absolute path."""
    missing = tmp_path / file_name

    with pytest.raises(InputDataError) as raised:
        parse(missing)

    assert str(raised.value) == f"{message}{missing.absolute()}"


def _synthetic_warning_spec(line: int, end_line: int) -> LogCheckpointSpec:
    """A LMM_BASE_OMITTED ``warning_count`` entry over ``line``-``end_line`` of a synthetic log."""
    return LogCheckpointSpec(
        SAS_DATASET="LMM_BASE_OMITTED",
        OCCURRENCE=1,
        PARAMCD=None,
        ATPTN=None,
        line=line,
        end_line=end_line,
        kind=KIND_WARNING_COUNT,
        has_nvars=False,
        description="synthetic omitted-baseline warnings (LMM-FLT-02)",
    )


def _write_log(directory: Path, lines: list[str]) -> Path:
    """Write ``lines`` as an LF-terminated synthetic log under ``directory``."""
    log = directory / "synthetic.log"
    log.write_bytes(("\n".join(lines) + "\n").encode("latin-1"))
    return log


def test_lmm_flt_02_parse_log_warning_records(tmp_path: Path) -> None:
    """Each counted WARNING yields the ``name=value`` pairs ``put _all_`` printed with it.

    Verifies the parser of the LMM-FLT-02 omission records: ``%derive_lastminmax_measure`` writes
    ``put '<text> ' _all_;`` per omitted baseline record
    [whitepapers/ADaM/derive_lastminmax_measure.sas:167], and the log wraps the pairs over
    continuation lines. The synthetic log holds hand-chosen values, not SAS output: a record whose
    pairs start on the next line and wrap, a value with blanks, an empty value, a missing ``.``,
    a WARNING with another prefix and a NOTE inside the range, the last counted record continuing
    past the range end, and a counted WARNING after the range.
    """
    spec = _synthetic_warning_spec(2, 9)
    log = _write_log(
        tmp_path,
        [
            "NOTE: There were 4 observations read from the data set WORK.LMM_BASE_ANL.",
            f"{_OMITTED_WARNING}, such as",
            "in_base=1 in_post=0 AVAL=61.5 ADT=29FEB2016 STUDYID=STUDYA USUBJID=A-001",
            "PARAM=Systolic Blood Pressure (mmHg) AVISITN=. avisit=",
            "FIRST.USUBJID=1 _N_=3",
            (
                "WARNING: (DERIVE_LASTMINMAX_MEASURES) Omitting obs without a baseline measure,"
                " such as"
            ),
            "in_base=0 in_post=1 AVAL=99 USUBJID=B-002",
            "NOTE: An unrelated note inside the range.",
            f"{_OMITTED_WARNING}, such as in_base=1 in_post=0 AVAL=72 USUBJID=C-003",
            "ATPT=AFTER LYING DOWN FOR 5 MINUTES base=.",
            "_N_=9",
            "NOTE: The data set WORK.LMM_BASE_ANL has 2 observations and 15 variables.",
            f"{_OMITTED_WARNING}, such as in_base=1 USUBJID=D-004",
        ],
    )
    expected = (
        (
            2,
            {
                "in_base": "1",
                "in_post": "0",
                "AVAL": "61.5",
                "ADT": "29FEB2016",
                "STUDYID": "STUDYA",
                "USUBJID": "A-001",
                "PARAM": "Systolic Blood Pressure (mmHg)",
                "AVISITN": ".",
                "avisit": "",
                "FIRST.USUBJID": "1",
                "_N_": "3",
            },
        ),
        (
            9,
            {
                "in_base": "1",
                "in_post": "0",
                "AVAL": "72",
                "USUBJID": "C-003",
                "ATPT": "AFTER LYING DOWN FOR 5 MINUTES",
                "base": ".",
                "_N_": "9",
            },
        ),
    )

    records = parse_log_warning_records(log, spec)

    assert records == tuple(LogWarningRecord(line, values) for line, values in expected)
    assert [(record.line, list(record.values.items())) for record in records] == [
        (line, list(values.items())) for line, values in expected
    ]
    assert len(set(records)) == len(expected)
    with pytest.raises(TypeError):
        records[0].values["AVAL"] = "0"  # type: ignore[index]
    (checkpoint,) = parse_log_checkpoints(log, [spec])
    assert checkpoint.ok, checkpoint.problem
    assert checkpoint.NOBS == len(records)


def _record_with_repeated_name(tmp_path: Path) -> tuple[Path, object]:
    log = _write_log(tmp_path, [f"{_OMITTED_WARNING}, such as AVAL=1 USUBJID=A-001", "aval=2"])
    return log, _synthetic_warning_spec(1, 1)


def _dataset_spec(tmp_path: Path) -> tuple[Path, object]:
    log = _write_log(tmp_path, [f"{_OMITTED_WARNING}, such as AVAL=1"])
    specs = LOG_CHECKPOINT_MAP[PROGRAM_07_07]
    return log, next(spec for spec in specs if spec.kind == KIND_DATASET)


def _not_a_spec(tmp_path: Path) -> tuple[Path, object]:
    log = _write_log(tmp_path, [f"{_OMITTED_WARNING}, such as AVAL=1"])
    return log, LOG_CHECKPOINT_MAP[PROGRAM_07_07]


def _missing_log(tmp_path: Path) -> tuple[Path, object]:
    return tmp_path / f"{PROGRAM_07_07}.log", _synthetic_warning_spec(1, 1)


@pytest.mark.parametrize(
    "make_input, error, message",
    [
        pytest.param(
            _not_a_spec,
            TypeError,
            "spec must be a LogCheckpointSpec, got tuple",
            id="not-a-spec",
        ),
        pytest.param(
            _dataset_spec,
            ValueError,
            "{spec.SAS_DATASET} at log line {spec.line} is a dataset checkpoint, not a "
            "warning_count",
            id="dataset-kind",
        ),
        pytest.param(
            _record_with_repeated_name,
            ValueError,
            "the WARNING record at log line 1 prints AVAL= and aval=, the same SAS name ignoring "
            "case",
            id="repeated-name",
        ),
        pytest.param(
            _missing_log, InputDataError, "reference log not found: {path}", id="missing-log"
        ),
    ],
)
def test_lmm_flt_02_parse_log_warning_records_rejects(
    make_input: Callable[[Path], tuple[Path, object]],
    error: type[Exception],
    message: str,
    tmp_path: Path,
) -> None:
    """A spec that is not a ``warning_count``, a repeated name or a missing log is refused.

    The LMM-FLT-02 omission records come from ``put _all_``, which prints each variable of the
    program data vector once [whitepapers/ADaM/derive_lastminmax_measure.sas:167], and SAS names
    ignore case, so a repeated name means the lines are not one such record. A missing log raises
    naming its absolute path.
    """
    log, spec = make_input(tmp_path)

    with pytest.raises(error) as raised:
        parse_log_warning_records(log, spec)  # type: ignore[arg-type]

    assert type(raised.value) is error
    assert str(raised.value) == message.format(spec=spec, path=log.absolute())


# --- Full-precision SAS exports -------------------------------------------------------------------


@pytest.mark.parametrize("create", [True, False], ids=["empty-dir", "absent-dir"])
def test_load_sas_exports_empty_dir(create: bool, tmp_path: Path) -> None:
    """Without exports every ``schemas.SAS_EXPORT_FILES`` name is present and maps to None."""
    directory = tmp_path / "sas_exports"
    if create:
        directory.mkdir()

    exports = load_sas_exports(directory)

    assert tuple(exports) == SAS_EXPORT_FILES
    assert len(exports) == 7
    assert all(frame is None for frame in exports.values())
    assert directory.exists() is create


def test_s07_trn_05_load_sas_exports_latin1(tmp_path: Path) -> None:
    """A Latin-1 export keeps the pooled ``'A0'x`` byte; names fold up and ``.`` becomes NaN.

    The pooled identifier ``'A0'x !! 'Pooled'`` (S07-TRN-05, D-16) survives only because the
    export is read as Latin-1.
    """
    file_name = export_spec(PROGRAM_07_07, "css_anadata").sas_export_file
    assert file_name in SAS_EXPORT_FILES
    export = tmp_path / file_name
    export.write_bytes(
        "studyid,usubjid,chg\n"
        "\u00a0Pooled,P1-701-1015,-4\n"
        "CDISCPILOT01,01-701-1015  ,.\n"
        "CDISCPILOT02,01-701-1023,\n".encode("latin-1")
    )
    assert b"\xa0Pooled" in export.read_bytes()

    exports = load_sas_exports(tmp_path)

    frame = exports[file_name]
    assert frame is not None
    assert list(frame.columns) == ["STUDYID", "USUBJID", "CHG"]
    assert frame["STUDYID"].tolist() == ["\u00a0Pooled", "CDISCPILOT01", "CDISCPILOT02"]
    assert frame["USUBJID"].tolist() == ["P1-701-1015", "01-701-1015", "01-701-1023"]
    assert frame["CHG"].dtype == "float64"
    assert frame["CHG"].iloc[0] == -4.0
    assert math.isnan(frame["CHG"].iloc[1])
    assert math.isnan(frame["CHG"].iloc[2])
    assert [name for name, loaded in exports.items() if loaded is not None] == [file_name]


def test_s02_mod_02_load_sas_exports_parameter_estimates(tmp_path: Path) -> None:
    """A ParameterEstimates export gains ``EFFECT`` and ``LEVEL`` before ``PARAMETER``.

    ``split_parameter`` keys the SAS labels on the same pair as the listing rows, and the four
    statistics are parsed from ``BEST32.`` text with SAS missing values as NaN (S02-MOD-02).
    """
    file_name = export_spec(PROGRAM_07_02, "parameter_estimates").sas_export_file
    assert file_name in SAS_EXPORT_FILES
    (tmp_path / file_name).write_bytes(
        "PARAMCD,ATPTN,Parameter,Estimate,StdErr,tValue,Probt\n"
        "DIABP,815,Intercept,33.338920131,3.908729851,8.53,1.0E-15\n"
        "DIABP,815,TRTPN     0,0,.,.,.\n".encode("latin-1")
    )

    frame = load_sas_exports(tmp_path)[file_name]

    assert frame is not None
    assert list(frame.columns) == [
        "PARAMCD",
        "ATPTN",
        "EFFECT",
        "LEVEL",
        "PARAMETER",
        "ESTIMATE",
        "STDERR",
        "TVALUE",
        "PROBT",
    ]
    pairs = list(zip(frame["EFFECT"], frame["LEVEL"], strict=True))
    assert pairs == [("INTERCEPT", ""), ("TRTPN", "0")]
    assert frame["ATPTN"].tolist() == [815.0, 815.0]
    assert frame["ESTIMATE"].tolist() == [33.338920131, 0.0]
    assert frame["PROBT"].iloc[0] == 1.0e-15
    assert frame[["STDERR", "TVALUE", "PROBT"]].iloc[1].isna().all()


def test_load_sas_exports_csv_quoting(tmp_path: Path) -> None:
    """Standard CSV quoting, CRLF records and blank lines load as written.

    A quoted comma, a quoted line break and a doubled quote keep their characters; an explicitly
    empty field is an empty character value or a missing number; blank lines are not records; an
    extra column such as ODS ``Dependent`` stays text.
    """
    file_name = export_spec(PROGRAM_07_07, "css_anadata").sas_export_file
    (tmp_path / file_name).write_bytes(
        b"\r\n"
        b"studyid,usubjid,saffl,param,base,chg,Dependent\r\n"
        b'CDISCPILOT01,01-701-1015,Y,"Diastolic, mmHg",80,-4,CHG\r\n'
        b"\r\n"
        b'CDISCPILOT01,01-701-1023,,"Line one\r\nline two",,,CHG\r\n'
        b'CDISCPILOT02,01-701-1028,Y,"Say ""hi""",.,2.5,\r\n'
    )

    frame = load_sas_exports(dir=tmp_path)[file_name]

    assert frame is not None
    columns = ["STUDYID", "USUBJID", "SAFFL", "PARAM", "BASE", "CHG", "DEPENDENT"]
    assert list(frame.columns) == columns
    assert len(frame) == 3
    assert frame["USUBJID"].tolist() == ["01-701-1015", "01-701-1023", "01-701-1028"]
    assert frame["SAFFL"].tolist() == ["Y", "", "Y"]
    assert frame["PARAM"].tolist() == ["Diastolic, mmHg", "Line one\r\nline two", 'Say "hi"']
    assert frame["DEPENDENT"].tolist() == ["CHG", "CHG", ""]
    assert frame["BASE"].iloc[0] == 80.0
    assert frame["BASE"].iloc[1:].isna().all()
    assert frame["CHG"].iloc[0] == -4.0
    assert math.isnan(frame["CHG"].iloc[1])
    assert frame["CHG"].iloc[2] == 2.5


def test_load_sas_exports_header_only(tmp_path: Path) -> None:
    """A header without records loads as an empty frame that keeps its columns."""
    file_name = export_spec(PROGRAM_07_07, "css_anadata").sas_export_file
    (tmp_path / file_name).write_bytes(b"studyid,usubjid,chg\n")

    frame = load_sas_exports(tmp_path)[file_name]

    assert frame is not None
    assert list(frame.columns) == ["STUDYID", "USUBJID", "CHG"]
    assert frame.empty


def test_load_sas_exports_one_column_quoted_empty(tmp_path: Path) -> None:
    """In a one-column export a lone ``""`` is a record holding a missing value, not a blank line.

    Only the empty physical line at the end is skipped.
    """
    file_name = export_spec(PROGRAM_07_07, "css_anadata").sas_export_file
    (tmp_path / file_name).write_bytes(b'CHG\n1\n""\n\n')

    frame = load_sas_exports(tmp_path)[file_name]

    assert frame is not None
    assert list(frame.columns) == ["CHG"]
    assert len(frame) == 2
    assert frame["CHG"].dtype == "float64"
    assert frame["CHG"].iloc[0] == 1.0
    assert math.isnan(frame["CHG"].iloc[1])


_HEADER_AND_ROW = b"studyid,usubjid,chg\nCDISCPILOT01,01-701-1015,1\n"
_ONE_FIELD_ROW_2 = ": data row 2, ending at line 3, has 1 fields but the header has 3$"


@pytest.mark.parametrize(
    "content, message",
    [
        (b"studyid,usubjid,chg\nCDISCPILOT01,01-701-1015,nan\n", "is not a SAS numeric value"),
        (
            b"studyid,STUDYID,chg\nCDISCPILOT01,CDISCPILOT01,1\n",
            "has columns with the same name ignoring case: STUDYID at fields 1, 2$",
        ),
        (
            b"studyid,usubjid,chg,chg\nCDISCPILOT01,01-701-1015,1,99\n",
            "has columns with the same name ignoring case: CHG at fields 3, 4$",
        ),
        (
            b"01-701-1015,01-701-1015,chg\nCDISCPILOT01,01-701-1015,1\n",
            "has columns with the same name ignoring case: fields 1, 2$",
        ),
        (
            b"studyid,usubjid,chg\nX,CDISCPILOT01,01-701-1015,1\nY,CDISCPILOT01,01-701-1023,2\n",
            ": data row 1, ending at line 2, has 4 fields but the header has 3$",
        ),
        (
            b"studyid,usubjid,chg\nCDISCPILOT01,01-701-1015\n",
            ": data row 1, ending at line 2, has 2 fields but the header has 3$",
        ),
        (
            (
                b"studyid,usubjid,chg\n\nCDISCPILOT01,01-701-1015,1\n"
                b"CDISCPILOT01,01-701-1023,2,extra,more\n"
            ),
            ": data row 2, ending at line 4, has 5 fields but the header has 3$",
        ),
        (_HEADER_AND_ROW + b'""\n', _ONE_FIELD_ROW_2),
        (_HEADER_AND_ROW + b'"   "\n', _ONE_FIELD_ROW_2),
        (_HEADER_AND_ROW + b'"\t"\n', _ONE_FIELD_ROW_2),
        (_HEADER_AND_ROW + b"  \n", _ONE_FIELD_ROW_2),
        (b" \t\nstudyid,usubjid,chg\n", "has a header column without a name: field 1$"),
        (
            b"studyid, ,chg\nCDISCPILOT01,01-701-1015,1\n",
            "has a header column without a name: field 2$",
        ),
        (
            b"studyid,,chg,\nCDISCPILOT01,01-701-1015,1,\n",
            "has header columns without a name: fields 2, 4$",
        ),
        (
            b'studyid,usubjid,chg\n"CDISCPILOT01,01-701-1015,1\n',
            ": line 2: unexpected end of data$",
        ),
        (bytes(range(256)) * 20, ": line 1: line contains NUL$"),
        (b"", " has no header row$"),
        (b"\n\r\n\n", " has no header row$"),
    ],
    ids=[
        "non-sas-number",
        "case-duplicate-columns",
        "identical-duplicate-columns",
        "duplicate-non-sas-names",
        "surplus-leading-field",
        "short-record",
        "ragged-record",
        "quoted-empty-record",
        "quoted-blanks-record",
        "quoted-tab-record",
        "unquoted-blanks-record",
        "blanks-before-header",
        "unnamed-header-column",
        "unnamed-header-columns",
        "unterminated-quote",
        "binary-garbage",
        "zero-byte-file",
        "blank-file",
    ],
)
def test_load_sas_exports_rejects(content: bytes, message: str, tmp_path: Path) -> None:
    """A malformed export raises one line naming the file, never loading an altered frame.

    pandas would rename an identical duplicate header to ``CHG.1``, take surplus leading fields
    as an ignored index and pad short records; the loader's own parse refuses all three.
    """
    file_name = export_spec(PROGRAM_07_07, "css_anadata").sas_export_file
    export = tmp_path / file_name
    export.write_bytes(content)

    with pytest.raises(InputDataError, match=message) as raised:
        load_sas_exports(tmp_path)

    text = str(raised.value)
    assert str(export.absolute()) in text
    assert "\n" not in text
    assert "01-701-1015" not in text


_ANADATA_HEAD = b"studyid,usubjid,base,chg\nCDISCPILOT01,01-701-1023,80,-4\n"


@pytest.mark.parametrize(
    "program_id, name, content, column, row, reason, hidden",
    [
        (
            PROGRAM_07_07,
            "css_anadata",
            _ANADATA_HEAD + b"CDISCPILOT01,01-701-1028,80,120 mmHg\n",
            "CHG",
            2,
            "the value is not a SAS numeric value",
            ("120 mmHg", "mmHg"),
        ),
        (
            PROGRAM_07_07,
            "css_anadata",
            _ANADATA_HEAD + b"CDISCPILOT01,,80,01-701-1015\n",
            "CHG",
            2,
            "the value is not a SAS numeric value",
            ("01-701-1015",),
        ),
        (
            PROGRAM_07_07,
            "css_anadata",
            _ANADATA_HEAD + b"CDISCPILOT01,,01-701-1015,-4\n",
            "BASE",
            2,
            "the value is not a SAS numeric value",
            ("01-701-1015",),
        ),
        (
            PROGRAM_07_02,
            "parameter_estimates",
            (
                b"PARAMCD,ATPTN,Parameter,Estimate,StdErr,tValue,Probt\n"
                b"DIABP,815,Intercept,1,1,1,0.5\n"
                b"DIABP,815,TRTPN 01-701-1015 54,1,1,1,0.5\n"
            ),
            "PARAMETER",
            2,
            "parameter label of 3 words cannot be split into an effect and a CLASS level",
            ("01-701-1015", "TRTPN 01-701-1015"),
        ),
    ],
    ids=["measurement-in-chg", "subject-id-in-chg", "subject-id-in-base", "parameter-label"],
)
def test_load_sas_exports_redacts_rejected_values(
    program_id: str,
    name: str,
    content: bytes,
    column: str,
    row: int,
    reason: str,
    hidden: tuple[str, ...],
    tmp_path: Path,
) -> None:
    """A rejected cell is located by file, column and data row, and its contents never shown.

    The CLI prints the message on stderr, so a measurement or a subject identifier misplaced in
    a numeric column or a ``Parameter`` label must not reach it.
    """
    export = tmp_path / export_spec(program_id, name).sas_export_file
    export.write_bytes(content)

    with pytest.raises(InputDataError) as raised:
        load_sas_exports(tmp_path)

    text = str(raised.value)
    assert text == f"SAS export {export.absolute()}: column {column}, data row {row}: {reason}"
    assert [token for token in hidden if token in text] == []
