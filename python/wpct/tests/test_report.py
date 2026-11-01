"""Tests of the equivalence report and of the ``python -m wpct`` command line.

Units under test: ``wpct.report`` (``SECTION_TITLES``, ``ROUNDED_NOTE_TEMPLATE``, ``render_report``
and ``write_report``) and ``wpct.__main__`` (``build_parser``, ``build_paths`` and ``reproduce``).

Report contract (Agent Action Plan 0.7.5): the sections Provenance, Summary, WPCT-F.07.02,
WPCT-F.07.07, Not compared and Source integrity, in that order. Provenance names the upstream
commit, the checkout ``HEAD`` (or ``unavailable``), the Python and library versions, the tolerance
table rendered from ``python/wpct/tolerances.json`` and the reference provenance (``6fc757d``,
SAS 9.4 TS1M2). The Summary counts PASS, FAIL and NOT COMPARED apart, per program and overall. The
merged ``CSS_STATS.pval`` cells (S02-TRN-05, S07-TRN-08) are their own group, separate from the
ParameterEstimates rows (S02-MOD-02, S07-MOD-02, S07-MOD-03). Every group against a rounded listing
carries the fixed rounded-reference wording. Every quantity without a SAS reference, among them the
PROC SUMMARY statistics (S02-MOD-01, S07-MOD-01), is listed with its reason under Not compared, and
the text is deterministic.

Command line (Agent Action Plan 0.6.5): ``reproduce`` is the only subcommand and ``--sas-exports``
its only option; exit code 0 means no comparison failed, 2 an input error.

Write policy (Agent Action Plan 0.8): every report this module writes goes through the
``write_paths`` fixture, whose write root lies under pytest's ``tmp_path``; ``reproduce`` is called
only with those paths and ``main`` never, so no test here writes into the checkout. The
checkout-rooted ``repo_paths`` reaches only ``check_write_destination``, which writes nothing. The
guard applies one rule to every write root, so the symlink cases on a ``tmp_path`` write root also
cover the checkout's fixed report destination.

The content tests render the comparisons of a clean checkout, in which no full-precision SAS export
is supplied (``clean_comparisons``). The report they check therefore does not depend on files a
user drops into ``python/wpct/reference/sas_exports/``; ``test_sas_exports.py`` covers those.
"""

from __future__ import annotations

import argparse
import json
import os
import platform
import re
from collections.abc import Callable, Mapping, Sequence
from pathlib import Path
from types import MappingProxyType
from typing import NamedTuple

import pytest

import wpct.__main__ as cli
from wpct import compare, provenance, report
from wpct.errors import OutputPathError
from wpct.paths import REPORT_RELATIVE, RepoPaths, check_write_destination
from wpct.schemas import EXPORTS, PROGRAM_07_02, PROGRAM_07_07, PROGRAMS, STATISTICS

#: The ``file_state`` fixture: ``(bytes, st_mtime_ns)`` of a file, or a directory's sorted listing.
_FileState = Callable[[str | Path], object]

#: Per program, the comparison groups the report renders, as ``compare.compare_program`` returns
#: them.
_Comparisons = Mapping[str, Sequence[compare.ComparisonGroup]]

#: The section order Agent Action Plan 0.7.5 fixes.
_EXPECTED_SECTIONS = (
    "Provenance",
    "Summary",
    PROGRAM_07_02,
    PROGRAM_07_07,
    "Not compared",
    "Source integrity",
)

#: Per program, the PASS rows of a clean checkout: every pinned log checkpoint plus every
#: listing-based value, 15 + 46 for 07.02 and 29 + 146 for 07.07 (Agent Action Plan 0.7.3).
_EXPECTED_PASS: Mapping[str, int] = {PROGRAM_07_02: 15 + 46, PROGRAM_07_07: 29 + 146}

#: Per program, the rows of the ParameterEstimates and merged-pval groups: 2 tables × 5 rows × 4
#: values and 2 × 3 cells for 07.02; 4 × 5 + 2 × 6 rows × 4 values and 6 × 3 cells for 07.07
#: (Agent Action Plan 0.7.3).
_LISTING_ROWS: Mapping[str, tuple[int, int]] = {PROGRAM_07_02: (40, 6), PROGRAM_07_07: (128, 18)}

#: The heading note of the merged-pval group, as Agent Action Plan 0.7.5 words it.
_MERGED_PVAL_WORDING = "verifies the CSS_STATS cells filled by the merge, not the model rows"

#: The fixed beginning and end of the rounded-reference wording (Agent Action Plan 0.7.5).
_ROUNDED_START = "Rounded reference only: agreement is shown at the precision displayed in "
_ROUNDED_END = "is not established for these quantities."

#: A summary-statistics limit no tolerance file holds, to show the table is rendered from its input.
_SENTINEL_LIMIT = 3e-7

#: Stand-in for an external SAS-export CSV that a symlinked report destination could overwrite.
_SAS_EXPORT_STANDIN = b"STUDYID,USUBJID\nCDISCPILOT01,01-701-1015\n"

#: A time of day, as a generation timestamp would print it.
_TIMESTAMP = re.compile(r"\d{2}:\d{2}:\d{2}")
#: An ISO 8601 date and time.
_ISO_DATETIME = re.compile(r"\d{4}-\d{2}-\d{2}[T ]\d{2}:\d{2}")

#: A Markdown cell separator: a ``|`` not escaped as ``\\|``.
_CELL_SEPARATOR = re.compile(r"(?<!\\)\|")
#: A number as the report prints a limit, e.g. ``0``, ``0.5`` or ``1e-08``.
_NUMBER = re.compile(r"\d+(?:\.\d+)?(?:e[-+]?\d+)?")
#: A single-digit count of displayed decimals.
_DECIMALS = re.compile(r"\b\d\b")
#: A full git object id.
_COMMIT_ID = re.compile(r"[0-9a-f]{40}")

_NO_VALUE = "—"


# --- Report parsing -------------------------------------------------------------------------------


def _level2_titles(text: str) -> list[str]:
    """The ``## `` section headings of the report, in order."""
    return [line[3:] for line in text.split("\n") if line.startswith("## ")]


def _section(text: str, title: str) -> list[str]:
    """The lines of the ``## title`` section, up to the next ``## `` heading.

    The title must head exactly one section, so a test cannot read the wrong one.
    """
    lines = text.split("\n")
    starts = [index for index, line in enumerate(lines) if line == f"## {title}"]
    assert len(starts) == 1, f"section {title!r} appears {len(starts)} times"
    body: list[str] = []
    for line in lines[starts[0] + 1 :]:
        if line.startswith("## "):
            break
        body.append(line)
    return body


def _subsections(lines: Sequence[str]) -> list[tuple[str, list[str]]]:
    """The ``### `` subsections of a section as ``(heading, lines)``, in order."""
    blocks: list[tuple[str, list[str]]] = []
    for line in lines:
        if line.startswith("### "):
            blocks.append((_unescape(line[4:]), []))
        elif blocks:
            blocks[-1][1].append(line)
    return blocks


def _subsection(lines: Sequence[str], heading: str) -> list[str]:
    """The lines of the one ``### heading`` subsection of ``lines``."""
    matches = [body for title, body in _subsections(lines) if title == heading]
    assert len(matches) == 1, f"subsection {heading!r} appears {len(matches)} times"
    return matches[0]


def _unescape(text: str) -> str:
    """Undo the report's Markdown escapes of ``|`` and of a raw-HTML ``<``."""
    return text.replace("\\|", "|").replace("\\<", "<")


def _code_text(cell: str) -> str:
    """The text of an inline code span, or ``cell`` itself when it is not one."""
    if len(cell) >= 2 and cell.startswith("`") and cell.endswith("`"):
        return cell[1:-1]
    return cell


def _cells(line: str) -> list[str]:
    parts = _CELL_SEPARATOR.split(line.strip())
    assert parts[0] == "" and parts[-1] == "", f"not a table row: {line!r}"
    return [_unescape(part.strip()) for part in parts[1:-1]]


def _tables(lines: Sequence[str]) -> list[list[dict[str, str]]]:
    """Every Markdown table in ``lines``, each as its rows keyed by the header cells."""
    tables: list[list[dict[str, str]]] = []
    block: list[str] = []
    for line in [*lines, ""]:
        if line.startswith("|"):
            block.append(line)
            continue
        if block:
            assert len(block) >= 2, f"table without a separator row: {block!r}"
            assert set(block[1]) <= set("|-"), f"not a separator row: {block[1]!r}"
            header = _cells(block[0])
            tables.append([dict(zip(header, _cells(row), strict=True)) for row in block[2:]])
            block = []
    return tables


def _single_table(lines: Sequence[str]) -> list[dict[str, str]]:
    tables = _tables(lines)
    assert len(tables) == 1, f"expected one table, found {len(tables)}"
    return tables[0]


def _status_counts(row: Mapping[str, str]) -> dict[str, int]:
    """The PASS, FAIL and NOT COMPARED cells of a Summary row, as integers."""
    return {status: int(row[status]) for status in compare.STATUSES}


def _group(groups: Sequence[compare.ComparisonGroup], key: str) -> compare.ComparisonGroup:
    matches = [group for group in groups if group.key == key]
    assert len(matches) == 1, f"comparison group {key!r} appears {len(matches)} times"
    return matches[0]


def _group_heading(group: compare.ComparisonGroup) -> str:
    """The heading the report gives ``group``: the merged-pval group carries its note in it."""
    if group.key == compare.GROUP_LISTING_MERGED_PVAL:
        return f"{group.title}: {group.heading_note}"
    return group.title


def _rows_with_status(comparisons: _Comparisons, program: str, status: str) -> int:
    """The rows of ``program`` whose status is exactly ``status``, counted row by row."""
    return sum(1 for group in comparisons[program] for row in group.rows if row.status == status)


# --- File-system helpers --------------------------------------------------------------------------


def _tree(root: Path) -> dict[str, str]:
    """Return every entry below ``root`` as ``relative path -> "dir" | "file" | "symlink"``.

    Symlinks are listed, never followed, so a write through a planted link shows up as a change at
    its target and not as a new entry here.
    """
    entries: dict[str, str] = {}
    for directory, dirnames, filenames in os.walk(root):
        for name in (*dirnames, *filenames):
            path = Path(directory, name)
            if path.is_symlink():
                kind = "symlink"
            elif path.is_dir():
                kind = "dir"
            else:
                kind = "file"
            entries[path.relative_to(root).as_posix()] = kind
    return entries


def _symlink(link: Path, target: Path) -> None:
    """Create ``link`` pointing at ``target``, or skip where the platform refuses symlinks."""
    try:
        os.symlink(target, link, target_is_directory=target.is_dir())
    except OSError as exc:
        pytest.skip(f"symlinks not supported on this platform: {exc}")


def _state_or_absent(path: Path, file_state: _FileState) -> object:
    """``file_state`` of ``path``, or ``None`` when nothing exists there."""
    return file_state(path) if path.exists() else None


# --- Fixtures -------------------------------------------------------------------------------------


class _Written(NamedTuple):
    """A report ``write_report`` wrote under the ``write_paths`` root."""

    path: Path
    text: str
    facts: provenance.Provenance


@pytest.fixture(scope="module")
def clean_comparisons(
    results: Mapping[str, object],
    listing_tables: Mapping[str, Sequence[object]],
    sas_checkpoints: Mapping[str, Sequence[object]],
    tolerances: compare.Tolerances,
) -> _Comparisons:
    """Per program, the comparison groups of a clean checkout, where no SAS export is supplied.

    Built with ``sas_exports=None`` rather than from the export directory, so the content asserted
    here is the same whatever a user has placed in ``python/wpct/reference/sas_exports/``.
    """
    return MappingProxyType(
        {
            program: compare.compare_program(
                results[program],
                listing_tables[program],
                sas_checkpoints[program],
                None,
                tolerances,
            )
            for program in PROGRAMS
        }
    )


@pytest.fixture
def written(
    results: Mapping[str, object], clean_comparisons: _Comparisons, write_paths: RepoPaths
) -> _Written:
    """The report of the clean-checkout comparisons, written by ``write_report`` under ``tmp_path``.

    ``provenance.collect`` reads git from ``write_paths.repo_root``, the checkout. The returned
    path must be the fixed report destination under the write root, resolved.
    """
    facts = provenance.collect(write_paths)
    path = report.write_report(results, clean_comparisons, facts, write_paths)
    assert path == write_paths.report_file.resolve()
    assert path.is_relative_to(write_paths.write_root.resolve())
    return _Written(path, path.read_bytes().decode("utf-8"), facts)


# --- Report content (Agent Action Plan 0.7.5) -----------------------------------------------------


def test_write_report_writes_only_the_report(
    written: _Written,
    write_paths: RepoPaths,
    results: Mapping[str, object],
    clean_comparisons: _Comparisons,
    tolerances: compare.Tolerances,
) -> None:
    """``write_report`` writes the rendered text to the fixed report path and nothing else.

    The write root then holds only ``python/wpct/reports/equivalence_report.md`` and the
    directories above it. The bytes are those of ``render_report`` with the tolerances of
    ``python/wpct/tolerances.json``, as UTF-8 with ``\\n`` line ends.
    """
    assert _tree(write_paths.write_root) == {
        "python": "dir",
        "python/wpct": "dir",
        "python/wpct/reports": "dir",
        REPORT_RELATIVE.as_posix(): "file",
    }
    expected = report.render_report(results, clean_comparisons, written.facts, tolerances)
    assert written.path.read_bytes() == expected.encode("utf-8")


def test_report_sections_in_order(written: _Written) -> None:
    """One title, then Provenance, Summary, both programs, Not compared and Source integrity.

    Each section heading appears once, in the order Agent Action Plan 0.7.5 fixes, and the
    tolerance table sits in Provenance, so it is printed before any result.
    """
    assert report.SECTION_TITLES == _EXPECTED_SECTIONS
    lines = written.text.split("\n")
    assert lines[0] == f"# {report.REPORT_TITLE}"
    assert [line for line in lines if line.startswith("# ")] == [lines[0]]
    assert _level2_titles(written.text) == list(_EXPECTED_SECTIONS)
    provenance_blocks = _subsections(_section(written.text, "Provenance"))
    assert "Tolerances" in [heading for heading, _ in provenance_blocks]


def test_report_provenance(written: _Written) -> None:
    """Provenance names both commits, the Python and library versions and the SAS references.

    The upstream commit is ``398a6d33…``; ``HEAD`` is a full commit id or ``unavailable``. The
    references come from commit ``6fc757d`` (2016-02-26), produced by SAS 9.4 TS1M2.
    """
    facts = written.facts
    section = _section(written.text, "Provenance")
    body = "\n".join(section)

    assert provenance.UPSTREAM_COMMIT == "398a6d33ced9359ffb58c46650a6d488811176b1"
    assert facts.upstream_commit == provenance.UPSTREAM_COMMIT
    upstream_lines = [line for line in section if "built against" in line]
    assert upstream_lines == [
        f"- Upstream commit, the commit the port was built against: `{provenance.UPSTREAM_COMMIT}`"
    ]

    head_lines = [line for line in section if "git rev-parse HEAD" in line]
    assert len(head_lines) == 1
    if facts.head_commit == provenance.UNAVAILABLE:
        assert head_lines[0].endswith(f": {provenance.UNAVAILABLE}")
    else:
        assert _COMMIT_ID.fullmatch(facts.head_commit)
        assert head_lines[0].endswith(f": `{facts.head_commit}`")

    assert provenance.REFERENCE_COMMIT == "6fc757d"
    assert "`6fc757d` (2016-02-26)" in body
    assert "SAS 9.4 TS1M2" in body
    assert facts.python_version == platform.python_version()
    assert f"- Python: `{platform.python_version()}`" in section

    libraries = _single_table(_subsection(section, "Library versions"))
    assert [row["Package"] for row in libraries] == list(provenance.LIBRARIES)
    assert {row["Package"]: row["Version"] for row in libraries} == dict(facts.library_versions)


def test_tolerance_table_rendered_from_file(
    written: _Written,
    results: Mapping[str, object],
    clean_comparisons: _Comparisons,
    tolerances: compare.Tolerances,
    repo_paths: RepoPaths,
    file_state: _FileState,
) -> None:
    """The tolerance table shows every class of ``tolerances.json`` with its limit, from its input.

    Each class appears in file order with its label, rule, limit (``—`` for a class without one)
    and scope. A modified copy of the loaded tolerances, whose summary-statistics limit is a
    sentinel, renders that sentinel, so the table is not written by hand. ``tolerances.json``
    itself is not edited.
    """
    tolerance_file = repo_paths.tolerances_file
    file_before = file_state(tolerance_file)
    raw = json.loads(tolerance_file.read_text(encoding="utf-8"))
    assert tolerances.to_dict() == raw

    def rows_of(text: str) -> dict[str, dict[str, str]]:
        table = _single_table(_subsection(_section(text, "Provenance"), "Tolerances"))
        return {_code_text(row["Class"]): row for row in table}

    rows = rows_of(written.text)
    assert list(rows) == list(raw["classes"])
    for class_key, entry in raw["classes"].items():
        row = rows[class_key]
        assert row["Label"] == entry["label"]
        assert row["Rule"] == entry["rule"]
        assert row["Applies to"] == entry["applies_to"]
        if "limit" in entry:
            assert float(row["Limit"]) == float(entry["limit"])
        else:
            assert row["Limit"] == _NO_VALUE

    edited = tolerances.to_dict()
    assert float(edited["classes"]["summary_statistics"]["limit"]) != _SENTINEL_LIMIT
    edited["classes"]["summary_statistics"]["limit"] = _SENTINEL_LIMIT
    modified = compare.Tolerances.from_mapping(edited)
    modified_text = report.render_report(results, clean_comparisons, written.facts, modified)
    modified_rows = rows_of(modified_text)

    assert float(modified_rows["summary_statistics"]["Limit"]) == _SENTINEL_LIMIT
    assert {key: row for key, row in modified_rows.items() if key != "summary_statistics"} == {
        key: row for key, row in rows.items() if key != "summary_statistics"
    }
    assert file_state(tolerance_file) == file_before


def test_summary_counts(written: _Written, clean_comparisons: _Comparisons) -> None:
    """The Summary counts PASS, FAIL and NOT COMPARED apart, per program and overall.

    Each count equals the rows with exactly that status, counted row by row here. On a clean
    checkout nothing fails, every log checkpoint and listing-based value passes (61 rows for
    07.02, 175 for 07.07) and each program has NOT COMPARED rows, which are not in its PASS count.
    """
    summary = {row["Program"]: row for row in _single_table(_section(written.text, "Summary"))}
    assert list(summary) == [*PROGRAMS, "Overall"]

    overall = dict.fromkeys(compare.STATUSES, 0)
    for program in PROGRAMS:
        expected = {
            status: _rows_with_status(clean_comparisons, program, status)
            for status in compare.STATUSES
        }
        assert _status_counts(summary[program]) == expected
        assert expected == compare.count_statuses(clean_comparisons[program])
        assert summary[program]["Halted"] == "no"
        program_line = (
            f"PASS {expected[compare.PASS]}, FAIL {expected[compare.FAIL]}, "
            f"NOT COMPARED {expected[compare.NOT_COMPARED]}; "
        )
        assert any(line.startswith(program_line) for line in _section(written.text, program))

        assert expected[compare.FAIL] == 0
        assert expected[compare.PASS] == _EXPECTED_PASS[program]
        assert expected[compare.NOT_COMPARED] > 0
        total_rows = sum(len(group.rows) for group in clean_comparisons[program])
        assert sum(expected.values()) == total_rows
        for status in compare.STATUSES:
            overall[status] += expected[status]

    assert _status_counts(summary["Overall"]) == overall
    assert (
        f"Result: no comparison failed and no program halted ({overall[compare.PASS]} PASS)."
        in _section(written.text, "Summary")
    )


def test_not_compared_rows_never_counted_as_pass(
    results: Mapping[str, object],
    clean_comparisons: _Comparisons,
    tolerances: compare.Tolerances,
    repo_paths: RepoPaths,
) -> None:
    """Adding NOT COMPARED rows raises only the NOT COMPARED counts, never PASS.

    A synthetic group of three NOT COMPARED rows and one PASS row is added to WPCT-F.07.02. Its
    Summary row gains exactly one PASS and three NOT COMPARED, WPCT-F.07.07 is unchanged, and the
    three rows are listed under Not compared with their reason.
    """
    key = compare.export_group_key("synthetic.csv")
    reason = "synthetic quantity without a SAS reference"

    def make_row(quantity: str, status: str) -> compare.ComparisonRow:
        return compare.ComparisonRow(
            program=PROGRAM_07_02,
            group=key,
            quantity=quantity,
            stratum="all strata",
            sas_value="1" if status == compare.PASS else "",
            python_value="1" if status == compare.PASS else "",
            difference="0" if status == compare.PASS else "",
            tolerance="exact" if status == compare.PASS else "",
            status=status,
            reason="" if status == compare.PASS else reason,
        )

    synthetic = compare.ComparisonGroup(
        program=PROGRAM_07_02,
        key=key,
        title="Synthetic group",
        heading_note=None,
        rounded=False,
        rows=(
            make_row("synthetic NC 1", compare.NOT_COMPARED),
            make_row("synthetic NC 2", compare.NOT_COMPARED),
            make_row("synthetic NC 3", compare.NOT_COMPARED),
            make_row("synthetic PASS", compare.PASS),
        ),
    )
    extended = {**clean_comparisons, PROGRAM_07_02: (*clean_comparisons[PROGRAM_07_02], synthetic)}
    facts = provenance.collect(repo_paths)

    def summary_of(comparisons: _Comparisons) -> dict[str, dict[str, int]]:
        text = report.render_report(results, comparisons, facts, tolerances)
        table = _single_table(_section(text, "Summary"))
        return {row["Program"]: _status_counts(row) for row in table}

    before = summary_of(clean_comparisons)
    after = summary_of(extended)
    assert after[PROGRAM_07_02] == {
        compare.PASS: before[PROGRAM_07_02][compare.PASS] + 1,
        compare.FAIL: before[PROGRAM_07_02][compare.FAIL],
        compare.NOT_COMPARED: before[PROGRAM_07_02][compare.NOT_COMPARED] + 3,
    }
    assert after[PROGRAM_07_07] == before[PROGRAM_07_07]
    assert after["Overall"][compare.PASS] == before["Overall"][compare.PASS] + 1
    assert after["Overall"][compare.NOT_COMPARED] == before["Overall"][compare.NOT_COMPARED] + 3

    text = report.render_report(results, extended, facts, tolerances)
    listed = [
        (entry["Comparison"], entry["Quantity"], entry["Reason"])
        for entry in _single_table(_section(text, "Not compared"))
        if entry["Comparison"] == synthetic.title
    ]
    assert listed == [(synthetic.title, f"synthetic NC {n}", reason) for n in (1, 2, 3)]


@pytest.mark.parametrize("program", PROGRAMS)
def test_merged_pval_group_separate(
    program: str, written: _Written, clean_comparisons: _Comparisons
) -> None:
    """The merged ``CSS_STATS.pval`` cells (S02-TRN-05, S07-TRN-08) have their own table.

    Its heading says that it verifies the statistics cells the merge fills, not the model rows, and
    differs from the ParameterEstimates heading (S02-MOD-02, S07-MOD-02, S07-MOD-03). Each heading
    appears once in the program section, followed by exactly one table holding that group's rows:
    40 and 6 for 07.02, 128 and 18 for 07.07, all PASS on a clean checkout.
    """
    groups = clean_comparisons[program]
    estimates = _group(groups, compare.GROUP_LISTING_PARAMETER_ESTIMATES)
    merged = _group(groups, compare.GROUP_LISTING_MERGED_PVAL)
    assert compare.MERGED_PVAL_NOTE == _MERGED_PVAL_WORDING
    assert merged.heading_note == _MERGED_PVAL_WORDING

    blocks = _subsections(_section(written.text, program))
    headings = [heading for heading, _ in blocks]
    merged_heading = _group_heading(merged)
    assert merged_heading.endswith(f": {_MERGED_PVAL_WORDING}")
    assert headings.count(merged_heading) == 1
    assert headings.count(estimates.title) == 1
    assert estimates.title != merged_heading
    assert _MERGED_PVAL_WORDING not in estimates.title

    by_heading = dict(blocks)
    estimate_rows = _single_table(by_heading[estimates.title])
    merged_rows = _single_table(by_heading[merged_heading])
    assert (len(estimate_rows), len(merged_rows)) == _LISTING_ROWS[program]
    assert (len(estimates.rows), len(merged.rows)) == _LISTING_ROWS[program]
    assert [row["Quantity"] for row in merged_rows] == [row.quantity for row in merged.rows]
    assert {row["Quantity"] for row in merged_rows} == {"CSS_STATS.pval"}
    assert [row["Quantity"] for row in estimate_rows] == [row.quantity for row in estimates.rows]
    assert "CSS_STATS.pval" not in {row["Quantity"] for row in estimate_rows}
    assert {row["Result"] for row in (*estimate_rows, *merged_rows)} == {compare.PASS}


def test_rounded_reference_wording(
    written: _Written,
    clean_comparisons: _Comparisons,
    tolerances: compare.Tolerances,
    repo_paths: RepoPaths,
) -> None:
    """Every group against a rounded listing carries the rounded-reference wording, once.

    The wording names the program's listing ``whitepapers/WPCT/outputs_sas/<program>.lst``, its
    displayed decimals (8, 2 and 4 for ParameterEstimates; 4 for the merged ``pval``) and the
    full-precision rule from ``tolerances.json`` it cannot establish. Groups against exact
    references, such as the log checkpoints, carry no such wording.
    """
    template = report.ROUNDED_NOTE_TEMPLATE
    assert template.startswith(_ROUNDED_START + "`{file}`")
    assert template.endswith(_ROUNDED_END)
    pattern = re.escape(template)
    for name in ("file", "d", "rule"):
        pattern = pattern.replace(re.escape("{" + name + "}"), f"(?P<{name}>.+?)")
    note = re.compile(pattern)

    glm_limit = tolerances.limit("glm_estimates")
    p_limit = tolerances.limit("p_values")
    expected = {
        compare.GROUP_LISTING_PARAMETER_ESTIMATES: ({"8", "2", "4"}, {glm_limit, p_limit}),
        compare.GROUP_LISTING_MERGED_PVAL: ({"4"}, {p_limit}),
    }
    notes_seen = 0
    for program in PROGRAMS:
        listing = repo_paths.listing_file(program).relative_to(repo_paths.repo_root).as_posix()
        assert listing == f"whitepapers/WPCT/outputs_sas/{program}.lst"
        blocks = dict(_subsections(_section(written.text, program)))
        groups = [g for g in clean_comparisons[program] if g.key != compare.GROUP_NOT_COMPARED]
        assert {group.key for group in groups if group.rounded} == set(expected)
        for group in groups:
            block = blocks[_group_heading(group)]
            notes = [line for line in block if line.startswith(_ROUNDED_START)]
            if not group.rounded:
                assert notes == [], f"{program} {group.key} is not rounded but carries the wording"
                continue
            assert len(notes) == 1, f"{program} {group.key}: {len(notes)} rounded notes"
            match = note.fullmatch(notes[0])
            assert match, f"{program} {group.key}: wording differs from the template: {notes[0]!r}"
            decimals, limits = expected[group.key]
            assert match["file"] == listing
            assert set(_DECIMALS.findall(match["d"])) == decimals
            assert {float(number) for number in _NUMBER.findall(match["rule"])} == limits
            notes_seen += 1
    assert notes_seen == len(PROGRAMS) * len(expected)
    assert sum(line.startswith(_ROUNDED_START) for line in written.text.split("\n")) == notes_seen


def test_not_compared_section(written: _Written, clean_comparisons: _Comparisons) -> None:
    """Not compared lists every NOT COMPARED row with its reason, and claims no equivalence.

    The table holds exactly the NOT COMPARED rows of both programs, in report order. Without SAS
    exports each program lists its record-level sets and categorical fields, its per-stratum
    record counts, each PROC SUMMARY statistic ``n mean std median datamin datamax q1 q3``
    (S02-MOD-01, S07-MOD-01) as held only in the PDF raster images, and its full-precision GLM
    values and merged ``pval``, of which only rounded listings exist (S02-TRN-05, S07-TRN-08).
    """
    section = _section(written.text, "Not compared")
    claim = "No equivalence is claimed for any quantity listed here"
    assert any(line.startswith(claim) for line in section)
    table = _single_table(section)
    expected = [
        (program, row.quantity, row.stratum, row.reason)
        for program in PROGRAMS
        for group in clean_comparisons[program]
        for row in group.rows
        if row.status == compare.NOT_COMPARED
    ]
    assert [(r["Program"], r["Quantity"], r["Stratum"], r["Reason"]) for r in table] == expected
    assert all(r["Reason"] and r["Reason"] != _NO_VALUE for r in table)

    assert STATISTICS == ("n", "mean", "std", "median", "datamin", "datamax", "q1", "q3")
    for program in PROGRAMS:
        rows = [r for r in table if r["Program"] == program]
        quantities = [r["Quantity"] for r in rows]
        assert any("record-level set and categorical fields" in q for q in quantities), program
        assert any("per-stratum record counts" in q for q in quantities), program
        for statistic in STATISTICS:
            label = f"PROC SUMMARY statistic {statistic}"
            matches = [r for r in rows if r["Quantity"].endswith(label)]
            assert len(matches) == 1, f"{program}: {label} listed {len(matches)} times"
            assert "PDF raster images" in matches[0]["Reason"]
        listing = f"{program}.lst"
        for quantity in ("full-precision GLM values", "merged pval at full precision"):
            matches = [r for r in rows if quantity in r["Quantity"]]
            assert len(matches) == 1, f"{program}: {quantity!r} listed {len(matches)} times"
            assert "only rounded listings exist" in matches[0]["Reason"]
            assert listing in matches[0]["Reason"]


def test_source_integrity_rows(written: _Written) -> None:
    """Source integrity has one row per read-only input, in ``READ_ONLY_INPUTS`` order.

    Each row shows the input's blob at the upstream commit, its working-tree blob and the result
    ``provenance.collect`` found (Agent Action Plan 0.11.2).
    """
    table = _single_table(_section(written.text, "Source integrity"))
    assert len(provenance.READ_ONLY_INPUTS) == 36
    assert [_code_text(row["Path"]) for row in table] == list(provenance.READ_ONLY_INPUTS)
    assert len(written.facts.integrity) == len(table)
    for row, fact in zip(table, written.facts.integrity, strict=True):
        assert fact.path == _code_text(row["Path"])
        for cell, blob in (
            (row["Blob at upstream commit"], fact.upstream_blob),
            (row["Working-tree blob"], fact.working_blob),
        ):
            assert _code_text(cell) == (provenance.UNAVAILABLE if blob is None else blob)
        assert row["Result"] == fact.status


def test_report_deterministic(
    written: _Written,
    results: Mapping[str, object],
    clean_comparisons: _Comparisons,
    tolerances: compare.Tolerances,
    write_paths: RepoPaths,
) -> None:
    """Equal inputs give equal text, with no timestamp, absolute path or ``\\r``.

    Two renders, one with freshly collected provenance, equal the written file. The report quotes
    no SAS page header, so no time of day appears anywhere in it.
    """
    first = report.render_report(results, clean_comparisons, written.facts, tolerances)
    second = report.render_report(
        results, clean_comparisons, provenance.collect(write_paths), tolerances
    )
    assert first == second == written.text
    assert first.endswith("\n") and not first.endswith("\n\n")
    assert "\r" not in first
    assert not _TIMESTAMP.search(first)
    assert not _ISO_DATETIME.search(first)
    for root in (write_paths.repo_root, write_paths.write_root, write_paths.write_root.resolve()):
        assert str(root) not in first


# --- reproduce under the test write root (Agent Action Plan 0.6.5, 0.7.6, 0.8) --------------------


@pytest.mark.parametrize("exports", ["default", "explicit_empty"])
def test_reproduce_under_write_root(
    exports: str,
    write_paths: RepoPaths,
    repo_paths: RepoPaths,
    sas_exports_dir: Path,
    results: Mapping[str, object],
    comparisons: _Comparisons,
    clean_comparisons: _Comparisons,
    tolerances: compare.Tolerances,
    tmp_path: Path,
    file_state: _FileState,
    capsys: pytest.CaptureFixture[str],
) -> None:
    """``reproduce`` exits 0 and writes only the report and the exports, under the write root.

    - ``default``: no ``--sas-exports``, so the run reads ``python/wpct/reference/sas_exports/``
      as the command line does; its report equals the one rendered from the session comparisons,
      which read the same directory.
    - ``explicit_empty``: ``--sas-exports`` names an existing empty directory under ``tmp_path``;
      the report equals the clean-checkout rendering, and the directory stays empty.

    The write root ends up holding exactly ``python/wpct/reports/equivalence_report.md`` and every
    ``python/wpct/outputs/<program>/`` file of ``schemas.EXPORTS``, and the checkout's own report
    and outputs locations are unchanged.
    """
    checkout_watched = (
        repo_paths.report_file,
        repo_paths.report_file.parent,
        repo_paths.outputs_dir,
    )
    checkout_before = {path: _state_or_absent(path, file_state) for path in checkout_watched}
    if exports == "default":
        assert write_paths.sas_exports_dir == sas_exports_dir
        exports_dir = None
        expected_comparisons = comparisons
    else:
        exports_dir = tmp_path / "sas_exports"
        exports_dir.mkdir()
        expected_comparisons = clean_comparisons

    code = cli.reproduce(write_paths, exports_dir)

    out = capsys.readouterr().out
    assert code == cli.EXIT_OK == 0, out
    report_lines = [line for line in out.split("\n") if line.startswith("Report: ")]
    assert len(report_lines) == 1
    assert report_lines[0].endswith(REPORT_RELATIVE.as_posix())

    root = write_paths.write_root
    tree = _tree(root)
    assert "symlink" not in tree.values()
    expected_files = {REPORT_RELATIVE.as_posix()} | {
        (write_paths.program_outputs_dir(program) / spec.file_name).relative_to(root).as_posix()
        for program in PROGRAMS
        for spec in EXPORTS[program]
    }
    assert {path for path, kind in tree.items() if kind == "file"} == expected_files
    for relative in expected_files - {REPORT_RELATIVE.as_posix()}:
        header = (root / relative).read_text(encoding="utf-8").split("\n", 1)[0]
        assert header, f"{relative} has no header row"

    facts = provenance.collect(write_paths)
    expected_text = report.render_report(results, expected_comparisons, facts, tolerances)
    assert write_paths.report_file.read_bytes() == expected_text.encode("utf-8")
    assert {
        path: _state_or_absent(path, file_state) for path in checkout_watched
    } == checkout_before
    if exports_dir is not None:
        assert list(exports_dir.iterdir()) == []


def test_reproduce_rejects_missing_sas_exports_dir(
    write_paths: RepoPaths, tmp_path: Path, capsys: pytest.CaptureFixture[str]
) -> None:
    """A ``--sas-exports`` that is not an existing directory is an input error: exit 2, no writes.

    The check runs before anything is read or written, so the write root stays empty and the named
    directory is not created.
    """
    missing = tmp_path / "no_such_exports"

    code = cli.reproduce(write_paths, missing)

    assert code == cli.EXIT_INPUT_ERROR == 2
    err = capsys.readouterr().err
    assert "not an existing directory" in err
    assert str(missing) in err
    assert _tree(write_paths.write_root) == {}
    assert not missing.exists()


# --- Write-destination guard (Agent Action Plan 0.8) ----------------------------------------------


@pytest.mark.parametrize(
    "relative",
    [
        pytest.param("whitepapers/WPCT/WPCT-F.07.02.sas", id="sas_program"),
        pytest.param("data/adam/cdisc/advs.xpt", id="advs_xpt"),
        pytest.param("whitepapers/WPCT/outputs_sas/WPCT-F.07.02.lst", id="sas_listing"),
        pytest.param(None, id="outside_checkout"),
    ],
)
def test_rejects_protected_destination(
    relative: str | None,
    repo_paths: RepoPaths,
    tmp_path: Path,
    file_state: _FileState,
) -> None:
    """The guard refuses a protected repository file or a path outside the checkout (AAP 0.8).

    ``relative`` names a SAS-era file of the checkout, or ``None`` for a report-named file under
    ``tmp_path``. The guard is called with ``RepoPaths.checkout()``, the paths the command line
    uses, and must raise ``OutputPathError`` naming the target and its resolved path. The file's
    bytes and ``st_mtime_ns`` and its directory's listing are unchanged, and the outside path is
    not created. As a control, the same paths accept the checkout's own report destination, and
    that check creates nothing either.
    """
    if relative is None:
        target = tmp_path / "equivalence_report.md"
        assert not target.exists()
        file_before = None
    else:
        target = repo_paths.repo_root / relative
        assert target.is_file(), f"protected input missing from the checkout: {target}"
        file_before = file_state(target)
    directory_before = file_state(target.parent)

    with pytest.raises(OutputPathError) as excinfo:
        check_write_destination(target, repo_paths)

    error = excinfo.value
    assert error.target == target
    assert error.resolved == target.resolve()
    assert str(error.target) in str(error)
    assert str(error.resolved) in str(error)
    assert file_state(target.parent) == directory_before
    if file_before is None:
        assert not target.exists()
    else:
        assert file_state(target) == file_before

    report_file = repo_paths.report_file
    control_watched = (report_file, report_file.parent)
    control_before = {path: _state_or_absent(path, file_state) for path in control_watched}
    assert check_write_destination(report_file, repo_paths) == report_file.resolve()
    assert {path: _state_or_absent(path, file_state) for path in control_watched} == control_before


@pytest.mark.parametrize("link", ["report_file", "reports_dir"])
def test_rejects_symlinked_destination(
    link: str,
    write_paths: RepoPaths,
    results: Mapping[str, object],
    clean_comparisons: _Comparisons,
    tmp_path: Path,
    file_state: _FileState,
) -> None:
    """A report destination reached through a symlink is refused, and nothing is opened (AAP 0.8).

    - ``report_file``: ``python/wpct/reports/`` under the write root is a real directory, and
      ``equivalence_report.md`` in it is a symlink to an external stand-in for a SAS-export CSV
      under ``tmp_path``, outside the write root.
    - ``reports_dir``: the write root's ``python/wpct/reports`` is a symlink to an external
      directory under ``tmp_path`` that already holds ``sentinel.csv``.

    ``write_report`` must raise ``OutputPathError`` naming the report path and where it leads. The
    external file's bytes and ``st_mtime_ns`` and the external directory's listing are unchanged,
    and the write root holds only the directories and the link the test made.
    """
    report_file = write_paths.report_file
    if link == "report_file":
        report_file.parent.mkdir(parents=True)
        external_dir = tmp_path
        external_file = tmp_path / "external_sas_export.csv"
        external_file.write_bytes(_SAS_EXPORT_STANDIN)
        link_path = report_file
        _symlink(link_path, external_file)
        expected_resolved = external_file.resolve()
    else:
        report_file.parent.parent.mkdir(parents=True)
        external_dir = tmp_path / "external_dir"
        external_dir.mkdir()
        external_file = external_dir / "sentinel.csv"
        external_file.write_bytes(_SAS_EXPORT_STANDIN)
        link_path = report_file.parent
        _symlink(link_path, external_dir)
        expected_resolved = external_dir.resolve() / report_file.name

    planted = _tree(write_paths.write_root)
    assert "file" not in planted.values()
    assert planted[link_path.relative_to(write_paths.write_root).as_posix()] == "symlink"
    file_before = file_state(external_file)
    directory_before = file_state(external_dir)
    facts = provenance.collect(write_paths)

    with pytest.raises(OutputPathError) as excinfo:
        report.write_report(results, clean_comparisons, facts, write_paths)

    error = excinfo.value
    assert error.target == report_file
    assert error.resolved == expected_resolved
    assert file_state(external_file) == file_before
    assert file_state(external_dir) == directory_before
    assert _tree(write_paths.write_root) == planted


def test_rejects_reports_dir_linked_into_outputs_tree(
    write_paths: RepoPaths,
    results: Mapping[str, object],
    clean_comparisons: _Comparisons,
    file_state: _FileState,
) -> None:
    """A reports directory symlinked into ``python/wpct/outputs`` is refused too (AAP 0.8).

    The guard alone accepts that target, because it resolves strictly below the outputs tree;
    the policy refuses every fixed destination reached through a symlink, inside the write root
    or outside it, so ``write_report`` must still raise ``OutputPathError``. The decoy directory
    stays empty and the write root unchanged.
    """
    report_file = write_paths.report_file
    decoy = write_paths.outputs_dir / "decoy_reports"
    decoy.mkdir(parents=True)
    _symlink(report_file.parent, decoy)
    linked = decoy.resolve() / report_file.name
    assert check_write_destination(report_file, write_paths) == linked

    planted = _tree(write_paths.write_root)
    decoy_before = file_state(decoy)
    facts = provenance.collect(write_paths)

    with pytest.raises(OutputPathError) as excinfo:
        report.write_report(results, clean_comparisons, facts, write_paths)

    assert excinfo.value.target == report_file
    assert excinfo.value.resolved == linked
    assert file_state(decoy) == decoy_before == []
    assert _tree(write_paths.write_root) == planted


# --- Command line (Agent Action Plan 0.6.5, 0.8) --------------------------------------------------


def _reproduce_parser(parser: argparse.ArgumentParser) -> argparse.ArgumentParser:
    """The ``reproduce`` sub-parser, after checking it is the parser's only subcommand."""
    actions = [a for a in parser._actions if isinstance(a, argparse._SubParsersAction)]
    assert len(actions) == 1
    assert set(actions[0].choices) == {"reproduce"}
    return actions[0].choices["reproduce"]


def _option_strings(parser: argparse.ArgumentParser) -> set[str]:
    return {option for action in parser._actions for option in action.option_strings}


def test_cli_accepts_only_sas_exports(tmp_path: Path, repo_paths: RepoPaths) -> None:
    """``reproduce`` takes ``--sas-exports`` and nothing else, and the paths are the checkout's.

    The top-level parser has only ``-h/--help``; the ``reproduce`` sub-parser adds only
    ``--sas-exports``, which defaults to ``None`` (the default export directory) and converts its
    value to a path. ``build_paths`` is ``RepoPaths.checkout()``, so its write root is the
    checkout and no option can move it.
    """
    parser = cli.build_parser()
    assert _option_strings(parser) == {"-h", "--help"}
    assert _option_strings(_reproduce_parser(parser)) == {"-h", "--help", "--sas-exports"}

    args = parser.parse_args(["reproduce"])
    assert (args.command, args.sas_exports) == ("reproduce", None)
    args = parser.parse_args(["reproduce", "--sas-exports", str(tmp_path)])
    assert (args.command, args.sas_exports) == ("reproduce", tmp_path)

    paths = cli.build_paths()
    assert paths.write_root == paths.repo_root
    assert paths == RepoPaths.checkout() == repo_paths


@pytest.mark.parametrize(
    "argv",
    [
        pytest.param(["reproduce", "--output", "v"], id="output"),
        pytest.param(["reproduce", "--report", "v"], id="report"),
        pytest.param(["reproduce", "--write-root", "v"], id="write_root"),
        pytest.param(["reproduce", "--outputs-dir", "v"], id="outputs_dir"),
        pytest.param(["reproduce", "-o", "v"], id="short_o"),
        pytest.param(["reproduce", "--sas-export", "v"], id="abbreviated_sas_exports"),
        pytest.param(["reproduce", "--sas-exports", ""], id="empty_sas_exports"),
        pytest.param(["--write-root", "v", "reproduce"], id="top_level_option"),
        pytest.param([], id="missing_command"),
        pytest.param(["report"], id="unknown_command"),
    ],
)
def test_cli_rejects_other_arguments(argv: list[str], capsys: pytest.CaptureFixture[str]) -> None:
    """Any other option, an option prefix, an empty export directory or a missing or unknown
    subcommand is an ``argparse`` error with exit code 2, raised before anything runs."""
    with pytest.raises(SystemExit) as excinfo:
        cli.build_parser().parse_args(argv)

    assert excinfo.value.code == 2
    assert "error:" in capsys.readouterr().err
