"""Tests of the equivalence report and of the ``python -m wpct`` command line.

Units under test: ``wpct.report`` (``SECTION_TITLES``, ``ROUNDED_NOTE_TEMPLATE``, ``render_report``
and ``write_report``) and ``wpct.__main__`` (``build_parser``, ``build_paths``, ``reproduce`` and
the logging set-up of ``main``).

Report contract (Agent Action Plan 0.7.5): the sections Provenance, Summary, WPCT-F.07.02,
WPCT-F.07.07, Not compared and Source integrity, in that order. Provenance names the upstream
commit, the checkout ``HEAD`` (or ``unavailable``), the Python and library versions, the tolerance
table rendered from ``python/wpct/tolerances.json`` and the reference provenance (``6fc757d``,
SAS 9.4 TS1M2). The Summary counts PASS, FAIL and NOT COMPARED apart, per program and overall. The
merged ``CSS_STATS.pval`` cells (S02-TRN-05, S07-TRN-08) are their own group, separate from the
ParameterEstimates rows (S02-MOD-02, S07-MOD-02, S07-MOD-03). Every group against a rounded listing
carries the fixed rounded-reference wording. Every quantity without a SAS reference, among them the
PROC SUMMARY statistics (S02-MOD-01, S07-MOD-01), is listed with its reason under Not compared, and
the text is deterministic. Each program section states its halt and lists every GLM fit with its
NOBS, DFE and status under Models (Agent Action Plan 0.6.4): a GLM skipped by setting, a model not
fitted (U-02, D-09), an empty population or empty derivation input (U-03) and a halted run
(S07-FLT-04) each have their own wording, and what such a run lacks FAILs against the listing or is
NOT COMPARED, never PASS. Source integrity says when git or the upstream commit is unavailable, so
integrity was not verified, and counts and names the CHANGED inputs.

Tolerances (Agent Action Plan 0.7.1): the table prints the ``compare.Tolerances`` snapshot the
comparison groups were judged under, which ``write_report`` takes from the groups rather than
re-reading ``tolerances.json``, and its field lists come from ``wpct.schemas``. Its Settings column
shows each class's remaining keys as the ``compare.Tolerances`` accessors apply them, a changed
setting included. An invalid tolerance file fails to load with its absolute path named.

Failure reasons (Agent Action Plan 0.9.2): every FAIL of a primitive rule of ``wpct.compare``
carries a reason that names neither compared value, and a failed row's reason reaches the report's
Reason column.

Command line (Agent Action Plan 0.6.5): ``reproduce`` is the only subcommand and ``--sas-exports``
its only option. Exit code 0 means no comparison failed and no program halted. Exit code 1 means a
comparison failed or a program halted (S07-FLT-04): the halted program exports nothing, while the
other program's exports and the report recording the halt are still written. Exit code 2 means an
input error or a rejected destination: a ``--sas-exports`` that is not an existing directory, a
malformed SAS export or a missing input dataset writes nothing, because every input is read before
any write; a destination the guard refuses stops the run at that writer call, which opens none of
its targets, while files of earlier successful writer calls remain.

Write policy (Agent Action Plan 0.8): every report this module writes goes through the
``write_paths`` fixture, whose write root lies under pytest's ``tmp_path``; ``reproduce`` is called
only with those paths and ``main`` never, so no test here writes into the checkout. The
checkout-rooted ``repo_paths`` never reaches a writer: it and the paths it names go only to code
that writes nothing, namely the readers ``provenance.collect`` (git metadata and input hashes),
``compare.load_tolerances`` and ``file_state``, and the guard ``check_write_destination``. The
``conftest.py`` session fixtures built on it only read. The guard applies one rule to every write
root, so the symlink cases on a ``tmp_path`` write root also cover the checkout's fixed report
destination, and so do the hard-link, special-file and replaced-report cases.

The content tests render the comparisons of a clean checkout, in which no full-precision SAS export
is supplied (``clean_comparisons``). The report they check therefore does not depend on files a
user drops into ``python/wpct/reference/sas_exports/``; ``test_sas_exports.py`` covers those.
"""

from __future__ import annotations

import argparse
import dataclasses
import json
import logging
import math
import os
import platform
import re
from collections.abc import Callable, Mapping, Sequence
from pathlib import Path
from types import MappingProxyType, SimpleNamespace
from typing import Any, NamedTuple

import pytest

import wpct.__main__ as cli
import wpct.paths as wpct_paths
from wpct import compare, provenance, report
from wpct.errors import AnalysisHalted, InputDataError, OutputPathError
from wpct.paths import REPORT_RELATIVE, RepoPaths, check_write_destination
from wpct.schemas import (
    EXPORTS,
    PROGRAM_07_02,
    PROGRAM_07_07,
    PROGRAMS,
    SAS_EXPORT_STRATUM_COLUMNS,
    SAS_PARAMETER_ESTIMATE_COLUMNS,
    STATISTICS,
    export_spec,
    upper_columns,
)

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

#: The halt line of a program section whose run passed every halting check.
_NO_HALT_LINE = "Halt: none; every halting check passed."

#: The rounded-reference wording, word for word as Agent Action Plan 0.7.5 fixes it, with the
#: listing, its displayed decimals and the full-precision rule as the ``{file}``, ``{decimals}``
#: and ``{rule}`` fields.
_ROUNDED_SENTENCE = (
    "Rounded reference only: agreement is shown at the precision displayed in `{file}` "
    "(`{decimals}` decimals; `<.0001` as a bound). Equivalence within the full-precision "
    "tolerance `{rule}` is not established for these quantities."
)
#: The words before the listing name, with which every rounded-reference note opens.
_ROUNDED_START = _ROUNDED_SENTENCE.partition("`")[0]

#: Per rounded group: the decimals its listing displays, each paired with its statistics as Agent
#: Action Plan 0.2.2.3 gives them, and the full-precision rule of 0.7.1 it cannot establish. In the
#: rule, ``{<class>}`` stands where that tolerance class's limit is printed, after the class's rule
#: kind and before the statistics it covers.
_ROUNDED_GROUPS: Mapping[str, tuple[str, str]] = {
    compare.GROUP_LISTING_PARAMETER_ESTIMATES: (
        "8 (Estimate, StdErr), 2 (t Value), 4 (Pr > |t|)",
        (
            "relative ≤ {glm_estimates} for Estimate, StdErr and t Value; "
            "absolute ≤ {p_values} for Pr > |t|"
        ),
    ),
    compare.GROUP_LISTING_MERGED_PVAL: ("4", "absolute ≤ {p_values}"),
}

#: GLM-estimate and p-value limits no tolerance file holds, distinct from each other, so a rounded
#: rule printing either limit in the other's place, or a limit other than the applied one, fails.
_ROUNDED_SENTINEL_LIMITS: Mapping[str, float] = {"glm_estimates": 2.5e-5, "p_values": 7e-10}

#: A summary-statistics limit no tolerance file holds, to show the table is rendered from its input.
_SENTINEL_LIMIT = 3e-7

#: An integer of 401 digits: a JSON number with no finite floating-point value.
_HUGE_INTEGER = 10**400

#: Stand-in for an external SAS-export CSV that a symlinked report destination could overwrite.
_SAS_EXPORT_STANDIN = b"STUDYID,USUBJID\nCDISCPILOT01,01-701-1015\n"

#: A time of day, as a generation timestamp would print it.
_TIMESTAMP = re.compile(r"\d{2}:\d{2}:\d{2}")
#: An ISO 8601 date and time.
_ISO_DATETIME = re.compile(r"\d{4}-\d{2}-\d{2}[T ]\d{2}:\d{2}")

#: What the report's literal text encodes, matched left to right as CommonMark reads it: a backslash
#: escape (a backslash before an ASCII punctuation character), an inline code span holding only a
#: run of ``@`` between single backticks, or any other backtick run, which is kept as written.
_ENCODED_LITERAL = re.compile(r"\\([!-/:-@\[-`{-~])|`(@+)`(?!`)|`+")
#: An inline code span holding only a run of ``@`` between single backticks: literal text.
_AT_CODE_SPAN = re.compile(r"`@+`(?!`)")
#: A run of backticks, which CommonMark reads as one code span delimiter.
_BACKTICK_RUN = re.compile(r"`+")
#: A control character: C0 (tab included), DEL or C1.
_CONTROL_CHARACTER = re.compile(r"[\x00-\x1f\x7f-\x9f]")
#: A number as the report prints a limit, e.g. ``0``, ``0.5`` or ``1e-08``.
_NUMBER = re.compile(r"\d+(?:\.\d+)?(?:e[-+]?\d+)?")
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
    """Undo the report's literal-text encoding, read left to right as CommonMark reads it: each
    backslash before an ASCII punctuation character gives that character, so ``\\\\`` is one
    literal backslash, and each inline code span holding only a run of ``@`` gives that run."""
    return _ENCODED_LITERAL.sub(
        lambda match: match.group(1) or match.group(2) or match.group(), text
    )


def _code_text(cell: str) -> str:
    """The text of an inline code span, or ``cell`` itself when it is not one."""
    if len(cell) >= 2 and cell.startswith("`") and cell.endswith("`"):
        return cell[1:-1]
    return cell


def _raw_cells(line: str) -> list[str]:
    """The cells of a Markdown table row as written, escapes kept, split as GFM splits them.

    Read left to right, a backslash takes the character after it, so only a ``|`` after an even
    run of backslashes, none included, ends a cell.
    """
    parts: list[str] = []
    cell: list[str] = []
    row = line.strip()
    index = 0
    while index < len(row):
        if row[index] == "\\":
            cell.append(row[index : index + 2])
            index += 2
            continue
        if row[index] == "|":
            parts.append("".join(cell))
            cell = []
        else:
            cell.append(row[index])
        index += 1
    parts.append("".join(cell))
    assert len(parts) >= 2 and parts[0] == "" and parts[-1] == "", f"not a table row: {line!r}"
    return [part.strip() for part in parts[1:-1]]


def _cells(line: str) -> list[str]:
    return [_unescape(cell) for cell in _raw_cells(line)]


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


def _summary_rows(text: str) -> dict[str, dict[str, str]]:
    """The rows of the report's Summary table, keyed by their Program cell."""
    return {row["Program"]: row for row in _single_table(_section(text, "Summary"))}


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


def _tolerance_rows(text: str) -> dict[str, dict[str, str]]:
    """The rows of the report's tolerance table, keyed by class."""
    table = _single_table(_subsection(_section(text, "Provenance"), "Tolerances"))
    return {_code_text(row["Class"]): row for row in table}


# --- Tolerance helpers ----------------------------------------------------------------------------


def _clean_comparisons_under(
    tolerances: compare.Tolerances,
    results: Mapping[str, object],
    listing_tables: Mapping[str, Sequence[object]],
    sas_checkpoints: Mapping[str, Sequence[object]],
) -> dict[str, tuple[compare.ComparisonGroup, ...]]:
    """Per program, the clean-checkout comparison groups judged under ``tolerances``."""
    return {
        program: compare.compare_program(
            results[program],
            listing_tables[program],
            sas_checkpoints[program],
            None,
            tolerances,
        )
        for program in PROGRAMS
    }


def _with_sentinel_limit(tolerances: compare.Tolerances) -> compare.Tolerances:
    """A copy of ``tolerances`` whose summary-statistics limit is :data:`_SENTINEL_LIMIT`."""
    edited = tolerances.to_dict()
    assert float(edited["classes"]["summary_statistics"]["limit"]) != _SENTINEL_LIMIT
    edited["classes"]["summary_statistics"]["limit"] = _SENTINEL_LIMIT
    return compare.Tolerances.from_mapping(edited)


#: The keys of a tolerance class that have their own column in the table; every other key is a
#: setting.
_SETTINGS_COLUMN_KEYS = ("label", "rule", "limit", "applies_to")

#: Per class, the settings it carries besides its columns, in file order, with the meaning Agent
#: Action Plan 0.7.1 gives them: trailing blanks stripped from keys and categorical fields, a SAS
#: GLM value of 0 matched only by an exact 0, the half unit and epsilon of a displayed-precision
#: value and the ``<.0001`` bound, and the outcomes of both and of exactly one value missing. Each
#: name is also the ``compare.Tolerances`` accessor that applies the setting.
_SETTINGS_NAMES: Mapping[str, tuple[str, ...]] = MappingProxyType(
    {
        "counts": (),
        "categorical": ("strip_trailing_blanks",),
        "summary_statistics": (),
        "glm_estimates": ("zero_reference",),
        "p_values": (),
        "record_numeric": (),
        "rounded_listing": ("half_unit", "epsilon", "bound_token", "bound_upper"),
        "missing": ("both_missing", "one_missing"),
    }
)

#: A rounded-listing epsilon no tolerance file holds, to show a changed setting is rendered.
_SETTINGS_EPSILON = 3e-13


def _settings_pairs(cell: str) -> list[tuple[str, str]]:
    """The ``name = value`` settings of a Settings cell as ``(name, value text)``, in order.

    ``—`` stands for no setting; any other cell must consist of ``; ``-separated pairs.
    """
    if cell == _NO_VALUE:
        return []
    pairs: list[tuple[str, str]] = []
    for part in cell.split("; "):
        name, separator, text = part.partition(" = ")
        assert separator and name and text, f"not a 'name = value' setting: {part!r} in {cell!r}"
        pairs.append((name, text))
    return pairs


def _settings_read(text: str, like: object) -> object:
    """The rendered setting ``text`` read back as the JSON type of ``like``.

    A boolean must read ``true`` or ``false``, a number is compared as a float, and a string is
    taken verbatim.
    """
    if isinstance(like, bool):
        assert text in ("true", "false"), f"not a JSON boolean: {text!r}"
        return text == "true"
    if isinstance(like, (int, float)):
        return float(text)
    assert isinstance(like, str), f"unexpected setting type {type(like).__name__}"
    return text


def _settings_assert_rendered(
    rows: Mapping[str, Mapping[str, str]], data: Mapping[str, Any], applied: compare.Tolerances
) -> None:
    """Every Settings cell of ``rows`` shows the settings of ``data``, which ``applied`` applies.

    Per class, the setting names are the keys of the ``data`` entry other than its columns, in
    file order, and they are those :data:`_SETTINGS_NAMES` pins. Each value reads back as its JSON
    value and as the value the ``compare.Tolerances`` accessor of that name returns.
    """
    assert set(rows) == set(data["classes"]) == set(_SETTINGS_NAMES)
    for class_key, entry in data["classes"].items():
        settings = {key: value for key, value in entry.items() if key not in _SETTINGS_COLUMN_KEYS}
        pairs = _settings_pairs(rows[class_key]["Settings"])
        names = [name for name, _ in pairs]
        assert names == list(settings) == list(_SETTINGS_NAMES[class_key]), class_key
        for name, text in pairs:
            value = _settings_read(text, settings[name])
            assert value == settings[name], f"{class_key}.{name} renders {text!r}"
            assert value == getattr(applied, name), f"{class_key}.{name} is not the applied value"


def _tolerance_bytes(data: Mapping[str, Any]) -> bytes:
    """``data`` written as a UTF-8 tolerance file."""
    return json.dumps(data, indent=2).encode("utf-8")


def _setting(class_key: str | None, key: str, value: object) -> Callable[[dict[str, Any]], bytes]:
    """An edit setting ``key`` to ``value`` in class ``class_key``, or at the top for ``None``."""

    def edit(data: dict[str, Any]) -> bytes:
        (data if class_key is None else data["classes"][class_key])[key] = value
        return _tolerance_bytes(data)

    return edit


def _latin1_label(data: dict[str, Any]) -> bytes:
    """The file with a non-ASCII counts label, encoded as Latin-1 rather than UTF-8."""
    data["classes"]["counts"]["label"] = "Comptés"
    return json.dumps(data, indent=2, ensure_ascii=False).encode("latin-1")


def _deeply_nested(data: dict[str, Any]) -> bytes:
    """The file with a first key whose value nests 5000 arrays, beyond the parser's recursion."""
    return b'{"deep": ' + b"[" * 5000 + b"]" * 5000 + b", " + _tolerance_bytes(data)[1:]


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


# --- Run-state helpers ----------------------------------------------------------------------------


#: The halt of WPCT-F.07.07 on one missing ``CHG`` in CSS_SAFANA (S07-FLT-04), worded as Agent
#: Action Plan 0.6.4 gives it.
_HALT_MESSAGE = f"{PROGRAM_07_07}: assert_var_nonmissing failed: 1 missing CHG in CSS_SAFANA"


def _css_safana_halt(result_07_07: Any) -> AnalysisHalted:
    """The ``AnalysisHalted`` WPCT-F.07.07 raises when ``%assert_var_nonmissing`` fails.

    It carries the checkpoints the driver records up to and including CSS_SAFANA, taken from the
    default run ``result_07_07``, as the driver hands them to the halt; the checkpoints after
    CSS_SAFANA are never reached.
    """
    checkpoints = tuple(result_07_07.checkpoints)
    names = [checkpoint.SAS_DATASET for checkpoint in checkpoints]
    assert "CSS_SAFANA" in names, "the default WPCT-F.07.07 run records no CSS_SAFANA checkpoint"
    reached = checkpoints[: names.index("CSS_SAFANA") + 1]
    assert len(reached) < len(checkpoints), "a halt at CSS_SAFANA must leave checkpoints unreached"
    return AnalysisHalted(
        _HALT_MESSAGE,
        program_id=PROGRAM_07_07,
        check="assert_var_nonmissing",
        checkpoints=reached,
    )


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
        _clean_comparisons_under(tolerances, results, listing_tables, sas_checkpoints)
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


def test_env03_write_report_writes_only_the_report(
    written: _Written,
    write_paths: RepoPaths,
    results: Mapping[str, object],
    clean_comparisons: _Comparisons,
    tolerances: compare.Tolerances,
) -> None:
    """``write_report`` writes the rendered text to the fixed report path and nothing else.

    The write root then holds only ``python/wpct/reports/equivalence_report.md`` and the
    directories above it: a fixed location under ``python/wpct/``, because the port has no
    counterpart of the SAS ``outputs_folder`` setting, blank in 07.02 (ENV-03) and a UNC share in
    07.07 (ENV-04). The bytes are those of ``render_report`` with the tolerances of
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


def test_s02_mod_02_report_sections_in_order(written: _Written) -> None:
    """One title, then Provenance, Summary, both programs, Not compared and Source integrity.

    Each section heading appears once, in the order Agent Action Plan 0.7.5 fixes: the
    WPCT-F.07.02 section, which judges among others the endpoint ANCOVA (S02-MOD-02), before the
    WPCT-F.07.07 section, which judges the per-study and pooled ANCOVAs (S07-MOD-02, S07-MOD-03).
    The tolerance table those results are judged under sits in Provenance, so it is printed before
    any result.
    """
    assert report.SECTION_TITLES == _EXPECTED_SECTIONS
    lines = written.text.split("\n")
    assert lines[0] == f"# {report.REPORT_TITLE}"
    assert [line for line in lines if line.startswith("# ")] == [lines[0]]
    assert _level2_titles(written.text) == list(_EXPECTED_SECTIONS)
    provenance_blocks = _subsections(_section(written.text, "Provenance"))
    assert "Tolerances" in [heading for heading, _ in provenance_blocks]


def test_env06_report_provenance(written: _Written) -> None:
    """Provenance names both commits, the Python and library versions and the SAS references.

    The upstream commit is ``398a6d33…``; ``HEAD`` is a full commit id or ``unavailable``. The
    references come from commit ``6fc757d`` (2016-02-26), produced by SAS 9.4 TS1M2, the release
    the programs' version gate demands (ENV-06). That gate has no Python counterpart, because
    ``requirements.txt`` fixes the Python environment, so the report records the Python and
    library versions the run used.
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


def test_s02_and_s07_mod_01_tolerance_table_rendered_from_file(
    written: _Written,
    results: Mapping[str, object],
    listing_tables: Mapping[str, Sequence[object]],
    sas_checkpoints: Mapping[str, Sequence[object]],
    tolerances: compare.Tolerances,
    repo_paths: RepoPaths,
    file_state: _FileState,
) -> None:
    """The tolerance table shows every class of ``tolerances.json`` with its limit, from its input.

    ``compare.load_tolerances``, the file's only reader, gives the session tolerances. Each class
    appears in file order with its label, rule, limit (``—`` for a class without one), scope and
    the fields ``wpct.schemas`` has it compare (``compare.compared_fields``, ``—`` for none). Its
    Settings cell lists its other keys as ``name = value`` in file order (``—`` for none): the
    names :data:`_SETTINGS_NAMES` pins, each value its JSON value (``true``/``false``, a number,
    a string) and the one its ``compare.Tolerances`` accessor applies, so every class, those
    without a limit included, shows how it compares (Agent Action Plan 0.7.1, 0.9.5).
    Comparisons judged under a modified copy of the loaded tolerances, whose summary-statistics
    limit, the one the PROC SUMMARY statistics of S02-MOD-01 and S07-MOD-01 are held to, is a
    sentinel, render that sentinel and leave every other row unchanged, so the table is not
    written by hand. ``tolerances.json`` itself is not edited.
    """
    tolerance_file = repo_paths.tolerances_file
    file_before = file_state(tolerance_file)
    loaded = compare.load_tolerances(tolerance_file).to_dict()
    assert loaded == tolerances.to_dict()
    fields = compare.compared_fields()
    assert list(fields) == list(compare.TOLERANCE_CLASSES)

    rows = _tolerance_rows(written.text)
    assert list(rows) == list(loaded["classes"])
    for class_key, entry in loaded["classes"].items():
        row = rows[class_key]
        assert row["Label"] == entry["label"]
        assert row["Rule"] == entry["rule"]
        assert row["Applies to"] == entry["applies_to"]
        if "limit" in entry:
            assert float(row["Limit"]) == float(entry["limit"])
        else:
            assert row["Limit"] == _NO_VALUE
        assert row["Fields"] == (" ".join(fields[class_key]) or _NO_VALUE)
    _settings_assert_rendered(rows, loaded, tolerances)

    modified = _with_sentinel_limit(tolerances)
    modified_comparisons = _clean_comparisons_under(
        modified, results, listing_tables, sas_checkpoints
    )
    modified_text = report.render_report(results, modified_comparisons, written.facts, modified)
    modified_rows = _tolerance_rows(modified_text)

    assert float(modified_rows["summary_statistics"]["Limit"]) == _SENTINEL_LIMIT
    assert {key: row for key, row in modified_rows.items() if key != "summary_statistics"} == {
        key: row for key, row in rows.items() if key != "summary_statistics"
    }
    assert file_state(tolerance_file) == file_before


def test_d11_changed_tolerance_setting_rendered(
    written: _Written,
    results: Mapping[str, object],
    listing_tables: Mapping[str, Sequence[object]],
    sas_checkpoints: Mapping[str, Sequence[object]],
    tolerances: compare.Tolerances,
    repo_paths: RepoPaths,
    file_state: _FileState,
) -> None:
    """A changed configurable setting reaches its Settings cell; nothing else in the table moves.

    A copy of the loaded tolerances takes a rounded-listing epsilon of :data:`_SETTINGS_EPSILON`
    and the opposite of the trailing-blank rule for keys and categorical fields, which by default
    strips trailing blanks because SAS character comparisons ignore them (D-11). Both are settings
    a user changes by editing ``tolerances.json`` alone (Agent Action Plan 0.7.1). The
    clean-checkout comparisons judged under that copy render both new values, read back as the
    copy's accessors apply them, and every other cell of every row as before, so the Settings
    column is rendered from the applied tolerances and not written by hand (Agent Action Plan
    0.9.5). Neither the session tolerances nor ``tolerances.json`` itself changes.
    """
    tolerance_file = repo_paths.tolerances_file
    file_before = file_state(tolerance_file)
    loaded = compare.load_tolerances(tolerance_file).to_dict()
    assert float(loaded["classes"]["rounded_listing"]["epsilon"]) != _SETTINGS_EPSILON
    strip_trailing_blanks = not loaded["classes"]["categorical"]["strip_trailing_blanks"]
    changed = {
        ("rounded_listing", "epsilon"): _SETTINGS_EPSILON,
        ("categorical", "strip_trailing_blanks"): strip_trailing_blanks,
    }
    edited = tolerances.to_dict()
    for (class_key, name), value in changed.items():
        edited["classes"][class_key][name] = value
    modified = compare.Tolerances.from_mapping(edited)
    assert modified.epsilon == _SETTINGS_EPSILON
    assert modified.strip_trailing_blanks is strip_trailing_blanks

    modified_comparisons = _clean_comparisons_under(
        modified, results, listing_tables, sas_checkpoints
    )
    modified_text = report.render_report(results, modified_comparisons, written.facts, modified)
    modified_rows = _tolerance_rows(modified_text)
    rows = _tolerance_rows(written.text)

    _settings_assert_rendered(modified_rows, edited, modified)
    changed_classes = {class_key for class_key, _ in changed}
    assert list(modified_rows) == list(rows)
    for class_key, row in rows.items():
        changed_columns = {"Settings"} if class_key in changed_classes else set()
        kept = [column for column in row if column not in changed_columns]
        assert list(modified_rows[class_key]) == list(row)
        assert {column: modified_rows[class_key][column] for column in kept} == {
            column: row[column] for column in kept
        }, class_key
    for (class_key, name), value in changed.items():
        before = _settings_pairs(rows[class_key]["Settings"])
        after = _settings_pairs(modified_rows[class_key]["Settings"])
        assert [setting for setting, _ in after] == [setting for setting, _ in before]
        for (setting, old_text), (_, new_text) in zip(before, after, strict=True):
            if setting == name:
                assert new_text != old_text, f"{class_key}.{name} still renders {old_text!r}"
                assert _settings_read(new_text, value) == value
            else:
                assert new_text == old_text, f"{class_key}.{setting} changed with {name}"
    assert tolerances.to_dict() == loaded
    assert file_state(tolerance_file) == file_before


@pytest.mark.parametrize(
    ("edit", "named"),
    [
        pytest.param(_latin1_label, None, id="not_utf8"),
        pytest.param(
            _setting("summary_statistics", "limit", _HUGE_INTEGER),
            "classes.summary_statistics.limit",
            id="integer_beyond_float_range",
        ),
        pytest.param(_deeply_nested, None, id="nested_beyond_recursion_limit"),
        pytest.param(
            _setting("categorical", "limit", 0.5),
            "classes.categorical.limit",
            id="categorical_limit",
        ),
        pytest.param(
            _setting("rounded_listing", "limit", 1e-3),
            "classes.rounded_listing.limit",
            id="rounded_listing_limit",
        ),
        pytest.param(
            _setting(
                "summary_statistics",
                "fields",
                list(compare.compared_fields()["summary_statistics"]),
            ),
            "classes.summary_statistics.fields",
            id="fields_in_file",
        ),
        pytest.param(_setting("counts", "limit", 1), "classes.counts.limit", id="counts_limit_1"),
        pytest.param(_setting(None, "comment", "extra"), "'comment'", id="unknown_top_level_key"),
        pytest.param(
            _setting(None, "description", ["not", "text"]), "description", id="description_list"
        ),
    ],
)
def test_s02_and_s07_mod_01_load_tolerances_rejects_invalid_file(
    edit: Callable[[dict[str, Any]], bytes],
    named: str | None,
    repo_paths: RepoPaths,
    tmp_path: Path,
) -> None:
    """An invalid tolerance file is an input error naming its absolute path and the key.

    Each case edits the repository file as ``compare.load_tolerances`` gives it and writes the
    result under ``tmp_path``: bytes that are not UTF-8, a limit beyond the floating-point range,
    nesting beyond the parser's recursion limit, a key the class does not read (a ``limit`` of
    ``categorical`` or ``rounded_listing``, or a field list, which only ``wpct.schemas`` holds,
    here the summary-statistics fields, the S02-MOD-01 and S07-MOD-01 statistics ``mean`` to
    ``q3``), a counts limit other than 0 (counts compare by exact integer equality), an unknown
    top-level key, and a description that is not a string. Loading raises ``ValueError``, which
    the command line reports with exit code 2, and the message names the absolute file path and
    the offending key where there is one; it never repeats the 401-digit integer.
    """
    target = tmp_path / "tolerances.json"
    target.write_bytes(edit(compare.load_tolerances(repo_paths.tolerances_file).to_dict()))

    with pytest.raises(ValueError) as excinfo:
        compare.load_tolerances(target)

    message = str(excinfo.value)
    assert f"invalid tolerance file {target.absolute()}: " in message
    if named is not None:
        assert named in message
    assert str(_HUGE_INTEGER) not in message


def test_s02_and_s07_mod_01_write_report_renders_applied_tolerances(
    results: Mapping[str, object],
    listing_tables: Mapping[str, Sequence[object]],
    sas_checkpoints: Mapping[str, Sequence[object]],
    tolerances: compare.Tolerances,
    repo_paths: RepoPaths,
    write_paths: RepoPaths,
    file_state: _FileState,
) -> None:
    """``write_report`` prints the tolerances the comparisons applied; it never re-reads the file.

    The comparisons are judged under a copy of the loaded tolerances whose summary-statistics
    limit, the one the S02-MOD-01 and S07-MOD-01 statistics are held to, is a sentinel. Every
    group that applies a tolerance carries that copy, the written table shows the sentinel while
    ``tolerances.json`` keeps its own limit and stays unchanged, and the bytes are those
    ``render_report`` gives for the copy.
    """
    tolerance_file = repo_paths.tolerances_file
    file_before = file_state(tolerance_file)
    modified = _with_sentinel_limit(tolerances)
    comparisons = _clean_comparisons_under(modified, results, listing_tables, sas_checkpoints)
    no_tolerance = {compare.GROUP_NOT_COMPARED, compare.GROUP_HALT}
    for program in PROGRAMS:
        for group in comparisons[program]:
            assert group.tolerances is (None if group.key in no_tolerance else modified)
    facts = provenance.collect(write_paths)

    path = report.write_report(results, comparisons, facts, write_paths)

    text = path.read_bytes().decode("utf-8")
    assert float(_tolerance_rows(text)["summary_statistics"]["Limit"]) == _SENTINEL_LIMIT
    assert compare.load_tolerances(tolerance_file).limit("summary_statistics") != _SENTINEL_LIMIT
    assert text == report.render_report(results, comparisons, facts, modified)
    assert file_state(tolerance_file) == file_before


@pytest.mark.parametrize("snapshots", ["mixed", "none"])
def test_s02_and_s07_mod_01_write_report_needs_one_applied_snapshot(
    snapshots: str,
    results: Mapping[str, object],
    clean_comparisons: _Comparisons,
    listing_tables: Mapping[str, Sequence[object]],
    sas_checkpoints: Mapping[str, Sequence[object]],
    tolerances: compare.Tolerances,
    write_paths: RepoPaths,
) -> None:
    """Without exactly one tolerance snapshot across the groups, ``write_report`` writes nothing.

    - ``mixed``: WPCT-F.07.02 is judged under the loaded tolerances and WPCT-F.07.07 under a copy
      with a sentinel summary-statistics limit, so the S02-MOD-01 and S07-MOD-01 statistics are
      held to different limits and no single table matches every verdict.
    - ``none``: no group carries the tolerances it was judged under.

    Either raises ``ValueError`` before anything is created, so the write root stays empty.
    """
    if snapshots == "mixed":
        other = _clean_comparisons_under(
            _with_sentinel_limit(tolerances), results, listing_tables, sas_checkpoints
        )
        comparisons = {**clean_comparisons, PROGRAM_07_07: other[PROGRAM_07_07]}
        match = "2 different tolerance snapshots"
    else:
        comparisons = {
            program: tuple(
                dataclasses.replace(group, tolerances=None) for group in clean_comparisons[program]
            )
            for program in PROGRAMS
        }
        match = "no comparison group carries the tolerances"
    facts = provenance.collect(write_paths)

    with pytest.raises(ValueError, match=match):
        report.write_report(results, comparisons, facts, write_paths)

    assert _tree(write_paths.write_root) == {}


def test_s02_and_s07_mod_01_render_report_rejects_other_tolerances(
    results: Mapping[str, object],
    clean_comparisons: _Comparisons,
    tolerances: compare.Tolerances,
    write_paths: RepoPaths,
) -> None:
    """``render_report`` refuses a tolerance table other than the one the comparisons applied.

    The clean-checkout groups carry the loaded tolerances, so rendering them with a copy whose
    summary-statistics limit, the one the S02-MOD-01 and S07-MOD-01 statistics are held to, is a
    sentinel raises ``ValueError`` naming the first such group.
    """
    facts = provenance.collect(write_paths)
    first = clean_comparisons[PROGRAM_07_02][0]
    assert first.tolerances == tolerances

    with pytest.raises(ValueError, match="judged under tolerances other than") as excinfo:
        report.render_report(results, clean_comparisons, facts, _with_sentinel_limit(tolerances))

    assert f"comparisons[{PROGRAM_07_02!r}][0] ({first.key})" in str(excinfo.value)


def test_s02_mod_02_summary_counts(written: _Written, clean_comparisons: _Comparisons) -> None:
    """The Summary counts PASS, FAIL and NOT COMPARED apart, per program and overall.

    Each count equals the rows with exactly that status, counted row by row here. On a clean
    checkout nothing fails and every log checkpoint and listing-based value passes: 61 rows for
    07.02, its 15 checkpoints, the 40 ParameterEstimates values of S02-MOD-02 and the 6 merged
    ``pval`` cells of S02-TRN-05; 175 for 07.07, its 29 checkpoints, the 128 values of S07-MOD-02
    and S07-MOD-03 and the 18 cells of S07-TRN-08. Each program has NOT COMPARED rows, which are
    not in its PASS count.
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


def test_s02_and_s07_mod_01_not_compared_rows_never_counted_as_pass(
    results: Mapping[str, object],
    clean_comparisons: _Comparisons,
    tolerances: compare.Tolerances,
    repo_paths: RepoPaths,
) -> None:
    """Adding NOT COMPARED rows raises only the NOT COMPARED counts, never PASS.

    NOT COMPARED marks a quantity without a SAS reference, such as a PROC SUMMARY statistic of
    S02-MOD-01 or S07-MOD-01, so no such row may count as PASS. A synthetic group of three NOT
    COMPARED rows and one PASS row is added to WPCT-F.07.02. Its Summary row gains exactly one PASS
    and three NOT COMPARED, WPCT-F.07.07 is unchanged, and the three rows are listed under Not
    compared with their reason.
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


#: Characters that are active Markdown wherever the report leaves them unescaped: escapes,
#: emphasis, links and images, strikethrough, cell separators, entity references and GitHub math.
#: A backtick and ``@`` have their own rules in :func:`_active_markdown`.
_ALWAYS_ACTIVE = frozenset("\\*[]~|&$")


def _active_markdown(raw: str) -> list[str]:
    """What in ``raw``, report text as written, would not render as literal text in cmark-gfm.

    Read left to right as CommonMark reads it, a backslash takes the character after it and a
    backtick run delimits a code span. Flagged are every control character; every ``@`` outside
    an inline code span, escaped or not, because cmark-gfm links an e-mail address, a
    ``<local@domain>`` autolink included, however its ``@`` is escaped; every unescaped backtick
    run except the single backticks around a code span holding only ``@``; and, outside an
    escape, ``\\ * [ ] ~ | & $``, a ``_`` that is not between two alphanumerics, a ``<`` before a
    letter, ``/``, ``!`` or ``?`` (raw HTML or an autolink), a ``:`` before ``//`` and a ``.``
    after ``www`` (GFM autolinks).
    """
    found = [f"control character {ord(char):02X}" for char in _CONTROL_CHARACTER.findall(raw)]
    index = 0
    while index < len(raw):
        char = raw[index]
        if char == "\\" and index + 1 < len(raw):
            if raw[index + 1] == "@":
                found.append(f"'@' at {index + 1}")
            index += 2
            continue
        if char == "`":
            span = _AT_CODE_SPAN.match(raw, index)
            if span is not None:
                index = span.end()
                continue
            run = _BACKTICK_RUN.match(raw, index)
            assert run is not None
            found.extend(f"'`' at {position}" for position in range(*run.span()))
            index = run.end()
            continue
        before, after = raw[index - 1 : index], raw[index + 1 : index + 2]
        if (
            char in _ALWAYS_ACTIVE
            or char == "@"
            or (char == "_" and not (before.isalnum() and after.isalnum()))
            or (char == "<" and ((after.isascii() and after.isalpha()) or after in ("/", "!", "?")))
            or (char == ":" and raw.startswith("//", index + 1))
            or (char == "." and raw[max(index - 3, 0) : index].lower() == "www")
        ):
            found.append(f"{char!r} at {index}")
        index += 1
    return found


def _literal_text(raw: str) -> str:
    """The literal text report text ``raw`` renders as, once nothing in it is found active.

    Text that is one inline code span, its content free of backticks, is a value the report holds
    as written; it must hold no backslash, ``|`` or control character, and gives its content. Any
    other text, inline code spans holding only ``@`` included, must hold no active Markdown
    (:func:`_active_markdown`) and gives its decoded text (:func:`_unescape`).
    """
    content = raw[1:-1]
    if len(raw) >= 2 and raw[0] == raw[-1] == "`" and "`" not in content:
        assert not re.search(r"[\\|]", content), f"code span cannot hold {content!r} as written"
        assert not _CONTROL_CHARACTER.search(content), f"control character in code span {raw!r}"
        return content
    active = _active_markdown(raw)
    assert not active, f"active Markdown in {raw!r}: {active}"
    return _unescape(raw)


def _raw_table(lines: Sequence[str]) -> list[dict[str, str]]:
    """The one table in ``lines``, its rows keyed by header cell, with each cell as written."""
    assert len(_tables(lines)) == 1, "expected one table"
    table = [line for line in lines if line.startswith("|")]
    header = _cells(table[0])
    return [dict(zip(header, _raw_cells(line), strict=True)) for line in table[2:]]


#: Untrusted text, as ``(text, literal)``: ``literal`` is what the report must show, the text on
#: one line with each control character as its ``<XX>`` code. Every text is synthetic; a key of a
#: user-supplied SAS export, which the report shows in a stratum or a reason, could carry any of
#: them.
_UNTRUSTED_TEXT = (
    pytest.param("a\\|b|c", "a\\|b|c", id="odd-backslashes-before-pipe"),
    pytest.param("p\\\\|q", "p\\\\|q", id="even-backslashes-before-pipe"),
    pytest.param(
        "\\<img src=x onerror=alert(4)>",
        "\\<img src=x onerror=alert(4)>",
        id="odd-backslashes-before-img",
    ),
    pytest.param(
        "\\\\<svg onload=alert(8)>", "\\\\<svg onload=alert(8)>", id="even-backslashes-before-svg"
    ),
    pytest.param(
        "\\<script>alert(2)\\</script>", "\\<script>alert(2)\\</script>", id="backslash-script"
    ),
    pytest.param("\\<br>", "\\<br>", id="backslash-br"),
    pytest.param("trailing backslash \\", "trailing backslash \\", id="trailing-backslash"),
    pytest.param("a|b||c |", "a|b||c |", id="pipes"),
    pytest.param("`code` and ``double``", "`code` and ``double``", id="backticks"),
    pytest.param(
        "![t](http://127.0.0.1:9/track.png)", "![t](http://127.0.0.1:9/track.png)", id="image"
    ),
    pytest.param("[click](javascript:alert(5))", "[click](javascript:alert(5))", id="link"),
    pytest.param("*em* _u_ ~~del~~ __strong__", "*em* _u_ ~~del~~ __strong__", id="emphasis"),
    pytest.param("&lt;b&gt;&#124;", "&lt;b&gt;&#124;", id="entities"),
    pytest.param(
        "http://evil.example/x www.evil.example WWW.EVIL.EXAMPLE",
        "http://evil.example/x www.evil.example WWW.EVIL.EXAMPLE",
        id="url-and-www-autolinks",
    ),
    pytest.param("x<1@evil.example>", "x<1@evil.example>", id="angle-bracket-email-autolink"),
    pytest.param("foo@bar.example", "foo@bar.example", id="bare-email-autolink"),
    pytest.param("mailto:x@y.example", "mailto:x@y.example", id="mailto-autolink"),
    pytest.param("xmpp:x@y.example/r", "xmpp:x@y.example/r", id="xmpp-autolink"),
    pytest.param("a@@@b.example", "a@@@b.example", id="at-run"),
    pytest.param("x`@", "x`@", id="backtick-before-at"),
    pytest.param("@`", "@`", id="at-before-backtick"),
    pytest.param("\\@x.example", "\\@x.example", id="backslash-before-at"),
    pytest.param("$x^2$", "$x^2$", id="math"),
    pytest.param(
        "CSS_STATS trtp_short _lead trail_ a__b",
        "CSS_STATS trtp_short _lead trail_ a__b",
        id="underscores",
    ),
    pytest.param("\x1b[31mRED\x1b]0;title\x07", "<1B>[31mRED<1B>]0;title<07>", id="esc-osc-bel"),
    pytest.param(
        "del\x7f csi\x9b nul\x00 tab\t", "del<7F> csi<9B> nul<00> tab<09>", id="del-c1-c0"
    ),
    pytest.param(
        "line one\nline two\r\nline three", "line one line two line three", id="line-breaks"
    ),
    pytest.param("<A0>Pooled", "<A0>Pooled", id="d16-pooled-marker"),
    pytest.param("<.0001", "<.0001", id="bound-token"),
)


@pytest.mark.parametrize(("text", "literal"), _UNTRUSTED_TEXT)
def test_s02_trn_03_untrusted_text_rendered_literally(
    text: str,
    literal: str,
    results: Mapping[str, object],
    clean_comparisons: _Comparisons,
    tolerances: compare.Tolerances,
    repo_paths: RepoPaths,
) -> None:
    """Untrusted text reaches the report as literal Markdown text, wherever the report shows it.

    The record keys STUDYID, USUBJID and PARAMCD (S02-TRN-03, S07-TRN-04) of a supplied SAS export
    reach the report in strata and reasons, and the pooled STUDYID shows its 'A0'x byte as
    ``<A0>`` (S07-TRN-05, D-16). A synthetic text fills every field of a synthetic FAIL row, the
    quantity, stratum and reason of a synthetic NOT COMPARED row, the group title, a synthetic
    library version, the Python version and one Source integrity path, so the report shows it in
    table cells, a heading, list items and a paragraph, as plain text or as an inline code span.
    Every table row splits into its header's cell count, the report holds no control character,
    and every place shows the literal text with no Markdown-active text outside a backslash escape
    and no ``@``, which cmark-gfm would link as an e-mail address, outside an inline code span.
    """
    key = compare.export_group_key("synthetic-untrusted.csv")
    heading = f"Synthetic group {literal}"

    def make_row(status: str, value: str) -> compare.ComparisonRow:
        return compare.ComparisonRow(
            program=PROGRAM_07_02,
            group=key,
            quantity=text,
            stratum=text,
            sas_value=value,
            python_value=value,
            difference=value,
            tolerance=value,
            status=status,
            reason=text,
        )

    synthetic = compare.ComparisonGroup(
        program=PROGRAM_07_02,
        key=key,
        title=f"Synthetic group {text}",
        heading_note=None,
        rounded=False,
        rows=(make_row(compare.FAIL, text), make_row(compare.NOT_COMPARED, "")),
    )
    comparisons = {
        **clean_comparisons,
        PROGRAM_07_02: (*clean_comparisons[PROGRAM_07_02], synthetic),
    }
    facts = provenance.collect(repo_paths)
    facts = dataclasses.replace(
        facts,
        python_version=text,
        library_versions={**facts.library_versions, "synthetic-package": text},
        integrity=(
            dataclasses.replace(facts.integrity[0], path=text, status=provenance.STATUS_CHANGED),
            *facts.integrity[1:],
        ),
    )
    rendered = report.render_report(results, comparisons, facts, tolerances)
    lines = rendered.split("\n")
    assert not _CONTROL_CHARACTER.search(rendered.replace("\n", ""))

    header_width: int | None = None
    for line in lines:
        if not line.startswith("|"):
            header_width = None
            continue
        width = len(_raw_cells(line))
        header_width = width if header_width is None else header_width
        assert width == header_width, f"{width} cells under a {header_width}-cell header: {line!r}"

    program_section = _section(rendered, PROGRAM_07_02)
    headings = [line[4:] for line in program_section if line.startswith("### Synthetic group ")]
    assert len(headings) == 1
    assert _literal_text(headings[0]) == heading

    failed, not_compared = _raw_table(_subsection(program_section, heading))
    values = ("SAS value", "Python value", "Difference", "Tolerance")
    for column in ("Quantity", "Stratum", *values, "Reason"):
        assert _literal_text(failed[column]) == literal, column
    for column in ("Quantity", "Stratum", "Reason"):
        assert _literal_text(not_compared[column]) == literal, column
    assert [not_compared[column] for column in values] == [""] * len(values)
    assert (_unescape(failed["Result"]), _unescape(not_compared["Result"])) == (
        compare.FAIL,
        compare.NOT_COMPARED,
    )

    listed = [
        row
        for row in _raw_table(_section(rendered, "Not compared"))
        if _unescape(row["Comparison"]) == heading
    ]
    assert len(listed) == 1
    for column in ("Comparison", "Quantity", "Stratum", "Reason"):
        assert _literal_text(listed[0][column]) == (heading if column == "Comparison" else literal)

    provenance_section = _section(rendered, "Provenance")
    versions = {
        _unescape(row["Package"]): row["Version"]
        for row in _raw_table(_subsection(provenance_section, "Library versions"))
    }
    assert _literal_text(versions["synthetic-package"]) == literal
    python_lines = [line for line in provenance_section if line.startswith("- Python: ")]
    assert len(python_lines) == 1
    assert _literal_text(python_lines[0].removeprefix("- Python: ")) == literal

    integrity_section = _section(rendered, "Source integrity")
    first = _raw_table(integrity_section)[0]
    assert _literal_text(first["Path"]) == literal
    assert _unescape(first["Result"]) == provenance.STATUS_CHANGED
    changed = [line for line in integrity_section if line.startswith("Changed inputs: ")]
    assert len(changed) == 1 and changed[0].endswith(".")
    assert _literal_text(changed[0].removeprefix("Changed inputs: ")[:-1]) == literal


#: Every outcome path of the primitive comparison rules, as ``(tolerance class, Python value, SAS
#: value or displayed token, expected status, expected reason)``. The values are functions of the
#: loaded tolerances, so no limit is restated here; ``rounded_listing`` cases go through
#: ``check_rounded`` and the others through ``check_value``.
_RULE_CASES = (
    pytest.param(
        "counts",
        lambda t: 3549 + int(t.limit("counts")),
        lambda t: 3548,
        compare.FAIL,
        "counts differ",
        id="count-differs",
    ),
    pytest.param(
        "categorical", lambda t: 815.0, lambda t: 817, compare.FAIL, "values differ", id="number"
    ),
    pytest.param(
        "categorical",
        lambda t: "CDISCPILOT01",
        lambda t: "CDISCPILOT02",
        compare.FAIL,
        "values differ",
        id="text",
    ),
    pytest.param(
        "summary_statistics",
        lambda t: 12.5 + 2 * t.limit("summary_statistics"),
        lambda t: 12.5,
        compare.FAIL,
        "values differ by more than",
        id="summary-absolute",
    ),
    pytest.param(
        "p_values",
        lambda t: 0.25 + 2 * t.limit("p_values"),
        lambda t: 0.25,
        compare.FAIL,
        "values differ by more than",
        id="p-value-absolute",
    ),
    pytest.param(
        "record_numeric",
        lambda t: 120.0 + 2 * t.limit("record_numeric"),
        lambda t: 120.0,
        compare.FAIL,
        "values differ by more than",
        id="record-absolute",
    ),
    pytest.param(
        "record_numeric",
        lambda t: 120.0,
        lambda t: "ABC-123",
        compare.FAIL,
        "SAS value is not a number",
        id="record-not-a-number",
    ),
    pytest.param(
        "glm_estimates",
        lambda t: 2.5 * (1 + 3 * t.limit("glm_estimates")),
        lambda t: 2.5,
        compare.FAIL,
        "× |SAS value|",
        id="glm-relative",
    ),
    pytest.param(
        "glm_estimates",
        lambda t: t.limit("glm_estimates"),
        lambda t: 0.0,
        compare.FAIL,
        "only an exact 0 agrees",
        id="glm-zero-reference",
    ),
    pytest.param(
        "rounded_listing",
        lambda t: 0.1234 + 3 * t.half_unit * 10.0**-4,
        lambda t: "0.1234",
        compare.FAIL,
        "displayed precision (4 dp)",
        id="displayed-precision",
    ),
    pytest.param(
        "rounded_listing",
        lambda t: 2 * t.bound_upper,
        lambda t: t.bound_token,
        compare.FAIL,
        "outside the bound",
        id="bound-above",
    ),
    pytest.param(
        "rounded_listing",
        lambda t: -t.bound_upper / 2,
        lambda t: t.bound_token,
        compare.FAIL,
        "outside the bound",
        id="bound-negative",
    ),
    pytest.param("counts", lambda t: 3548, lambda t: 3548, compare.PASS, "", id="count-equal"),
    pytest.param(
        "categorical",
        lambda t: "CDISCPILOT01",
        lambda t: "CDISCPILOT01",
        compare.PASS,
        "",
        id="text-equal",
    ),
    pytest.param(
        "summary_statistics",
        lambda t: 12.5 + t.limit("summary_statistics") / 2,
        lambda t: 12.5,
        compare.PASS,
        "",
        id="absolute-within",
    ),
    pytest.param(
        "glm_estimates",
        lambda t: 2.5 * (1 + t.limit("glm_estimates") / 2),
        lambda t: 2.5,
        compare.PASS,
        "",
        id="relative-within",
    ),
    pytest.param(
        "glm_estimates", lambda t: 0.0, lambda t: 0.0, compare.PASS, "", id="zero-reference-exact"
    ),
    pytest.param(
        "rounded_listing",
        lambda t: 0.1234 + t.half_unit * 10.0**-4 / 2,
        lambda t: "0.1234",
        compare.PASS,
        "",
        id="displayed-within",
    ),
    pytest.param(
        "rounded_listing",
        lambda t: t.bound_upper / 2,
        lambda t: t.bound_token,
        compare.PASS,
        "",
        id="bound-within",
    ),
    pytest.param(
        "record_numeric",
        lambda t: math.nan,
        lambda t: math.nan,
        compare.PASS,
        "both missing",
        id="both-missing",
    ),
)


@pytest.mark.parametrize(("class_key", "python", "sas", "status", "reason"), _RULE_CASES)
def test_d06_rule_outcome_reasons(
    class_key: str,
    python: Callable[[compare.Tolerances], object],
    sas: Callable[[compare.Tolerances], object],
    status: str,
    reason: str,
    tolerances: compare.Tolerances,
) -> None:
    """Every FAIL of a primitive comparison rule says why, without showing either value.

    Unequal counts and categorical values, an absolute or relative limit exceeded, a non-zero
    value against a SAS 0, which only an exact 0 matches because SAS reports an aliased column as
    0 (D-06), a displayed precision exceeded and a value outside the ``<.0001`` bound each FAIL
    with an explanation, so no failed report row lacks one. The explanation names neither value,
    so a record-level field mismatch can report only the keys of its records. A plain PASS has no
    reason, and a PASS of the missing rule keeps ``both missing``.
    """
    py_value, sas_value = python(tolerances), sas(tolerances)
    if class_key == "rounded_listing":
        outcome = compare.check_rounded(sas_value, py_value, tolerances)
    else:
        outcome = compare.check_value(class_key, py_value, sas_value, tolerances)
    assert outcome.status == status
    if status == compare.PASS:
        assert outcome.reason == reason
        return
    assert reason in outcome.reason
    for value in (py_value, sas_value):
        assert str(value) not in outcome.reason, f"{outcome.reason!r} shows the value {value!r}"


def test_s02_dep_util_access_test_data_failed_checkpoint_reason_in_report(
    results: Mapping[str, object],
    clean_comparisons: _Comparisons,
    sas_checkpoints: Mapping[str, Sequence[object]],
    tolerances: compare.Tolerances,
    repo_paths: RepoPaths,
) -> None:
    """A failed log checkpoint states why, once per count, in the report's Reason column.

    The first WPCT-F.07.02 checkpoint, ADVS at an ``obs/vars`` log line, counts the ``WORK.ADVS``
    copy that ``%util_access_test_data`` makes (S02-DEP-util_access_test_data). It is given more
    Python observations and variables than the counts limit allows. Its row FAILs as
    ``observation count differs; variable count differs``, every other checkpoint still passes
    without a reason, and the report's log-checkpoint table shows that reason beside the FAIL.
    """
    result = results[PROGRAM_07_02]
    first, *others = result.checkpoints
    assert first.NVARS is not None
    step = 1 + int(tolerances.limit("counts"))
    altered = dataclasses.replace(first, NOBS=first.NOBS + step, NVARS=first.NVARS + step)
    group = compare.compare_counts(
        PROGRAM_07_02, (altered, *others), sas_checkpoints[PROGRAM_07_02], tolerances
    )
    failed = [row for row in group.rows if row.status == compare.FAIL]
    assert [row.quantity for row in failed] == [f"{first.SAS_DATASET} #{first.OCCURRENCE}"]
    assert failed[0].reason == "observation count differs; variable count differs"
    assert all(row.reason == "" for row in group.rows if row.status == compare.PASS)
    assert len(group.rows) == len(failed) + group.n_pass

    groups = tuple(
        group if existing.key == compare.GROUP_LOG_CHECKPOINTS else existing
        for existing in clean_comparisons[PROGRAM_07_02]
    )
    comparisons = {**clean_comparisons, PROGRAM_07_02: groups}
    text = report.render_report(results, comparisons, provenance.collect(repo_paths), tolerances)
    table = _single_table(_subsection(_section(text, PROGRAM_07_02), group.title))
    reported = [
        (entry["Quantity"], entry["Reason"]) for entry in table if entry["Result"] == compare.FAIL
    ]
    assert reported == [(failed[0].quantity, failed[0].reason)]


@pytest.mark.parametrize("program", PROGRAMS)
def test_s02_trn_05_and_s07_trn_08_merged_pval_group_separate(
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


def _rounded_with_sentinel_limits(tolerances: compare.Tolerances) -> compare.Tolerances:
    """A copy of ``tolerances`` whose GLM-estimate and p-value limits are the rounded sentinels.

    The sentinels must differ from each other and from both loaded limits, so neither can be
    mistaken for the other class's limit or for a limit read from ``tolerances.json``.
    """
    sentinels = dict(_ROUNDED_SENTINEL_LIMITS)
    loaded = {tolerances.limit(key) for key in sentinels}
    assert len(set(sentinels.values())) == len(sentinels), f"sentinels coincide: {sentinels}"
    assert loaded.isdisjoint(sentinels.values()), f"a sentinel equals a loaded limit: {loaded}"
    edited = tolerances.to_dict()
    for key, limit in sentinels.items():
        edited["classes"][key]["limit"] = limit
    modified = compare.Tolerances.from_mapping(edited)
    assert {key: modified.limit(key) for key in sentinels} == sentinels
    return modified


def _rounded_note_pattern(listing: str, decimals: str, rule: str) -> re.Pattern[str]:
    """:data:`_ROUNDED_SENTENCE` for one group, to be matched in full.

    Every character is literal except each ``{<class>}`` field of ``rule``, which matches a number
    as the report prints a limit and captures it, at its own position, under the class key.
    """
    pattern = re.escape(_ROUNDED_SENTENCE.format(file=listing, decimals=decimals, rule=rule))
    for key in re.findall(r"\{(\w+)\}", rule):
        pattern = pattern.replace(re.escape("{" + key + "}"), f"(?P<{key}>{_NUMBER.pattern})", 1)
    return re.compile(pattern)


@pytest.mark.parametrize("limits", ["loaded", "sentinel"])
def test_s02_mod_02_rounded_reference_wording(
    limits: str,
    written: _Written,
    results: Mapping[str, object],
    clean_comparisons: _Comparisons,
    listing_tables: Mapping[str, Sequence[object]],
    sas_checkpoints: Mapping[str, Sequence[object]],
    tolerances: compare.Tolerances,
    repo_paths: RepoPaths,
) -> None:
    """Every group against a rounded listing carries the rounded-reference wording, once.

    The note is the sentence Agent Action Plan 0.7.5 fixes, word for word. It names the program's
    listing ``whitepapers/WPCT/outputs_sas/<program>.lst`` and the decimals that listing displays
    per statistic (Agent Action Plan 0.2.2.3): ``8 (Estimate, StdErr), 2 (t Value), 4 (Pr > |t|)``
    for ParameterEstimates (S02-MOD-02, S07-MOD-02, S07-MOD-03) and ``4`` for the merged ``pval``
    (S02-TRN-05, S07-TRN-08). Its rule gives the relative ``glm_estimates`` limit for Estimate,
    StdErr and t Value and the absolute ``p_values`` limit for ``Pr > |t|`` (Agent Action Plan
    0.7.1); each limit is read at its own position and must equal its own class's applied limit.
    A swapped limit, rule kind or precision therefore fails.

    - ``loaded``: the written report of the clean-checkout comparisons, under ``tolerances.json``.
    - ``sentinel``: the report rendered for comparisons judged under a copy whose two limits are
      distinct sentinels, which the rule must print in the same places.

    Groups against exact references, such as the log checkpoints, carry no such wording, and the
    report holds no note besides one per rounded group of each program.
    """
    if limits == "loaded":
        applied, comparisons, text = tolerances, clean_comparisons, written.text
    else:
        applied = _rounded_with_sentinel_limits(tolerances)
        comparisons = _clean_comparisons_under(applied, results, listing_tables, sas_checkpoints)
        text = report.render_report(results, comparisons, written.facts, applied)
    kinds = {
        key: kind
        for _, rule in _ROUNDED_GROUPS.values()
        for kind, key in re.findall(r"(\w+) ≤ \{(\w+)\}", rule)
    }
    assert kinds == {key: applied.entry(key)["rule"] for key in _ROUNDED_SENTINEL_LIMITS}

    notes_seen = 0
    for program in PROGRAMS:
        listing = repo_paths.listing_file(program).relative_to(repo_paths.repo_root).as_posix()
        assert listing == f"whitepapers/WPCT/outputs_sas/{program}.lst"
        blocks = dict(_subsections(_section(text, program)))
        groups = [g for g in comparisons[program] if g.key != compare.GROUP_NOT_COMPARED]
        assert {group.key for group in groups if group.rounded} == set(_ROUNDED_GROUPS)
        for group in groups:
            block = blocks[_group_heading(group)]
            notes = [line for line in block if line.startswith(_ROUNDED_START)]
            if not group.rounded:
                assert notes == [], f"{program} {group.key} is not rounded but carries the wording"
                continue
            assert len(notes) == 1, f"{program} {group.key}: {len(notes)} rounded notes"
            decimals, rule = _ROUNDED_GROUPS[group.key]
            match = _rounded_note_pattern(listing, decimals, rule).fullmatch(notes[0])
            assert match, f"{program} {group.key}: not the required wording: {notes[0]!r}"
            printed = match.groupdict()
            assert {key: float(number) for key, number in printed.items()} == {
                key: applied.limit(key) for key in printed
            }, f"{program} {group.key}: a limit is printed in the wrong place: {notes[0]!r}"
            # Secondary: the renderer's template, filled with the same fields, is the same sentence.
            filled = {"file": listing, "rule": rule.format(**printed)}
            expected = _ROUNDED_SENTENCE.format(decimals=decimals, **filled)
            assert report.ROUNDED_NOTE_TEMPLATE.format(d=decimals, **filled) == expected
            notes_seen += 1
    assert notes_seen == len(PROGRAMS) * len(_ROUNDED_GROUPS)
    assert sum(line.startswith(_ROUNDED_START) for line in text.split("\n")) == notes_seen


def test_s02_and_s07_mod_01_not_compared_section(
    written: _Written, clean_comparisons: _Comparisons
) -> None:
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


def test_env02_source_integrity_rows(written: _Written) -> None:
    """Source integrity has one row per read-only input, in ``READ_ONLY_INPUTS`` order.

    Each row shows the input's blob at the upstream commit, its working-tree blob and the result
    ``provenance.collect`` found (Agent Action Plan 0.11.2). The inputs include the two ADVS files
    the port reads from the checkout in place of the GitHub URL (ENV-02) and the UNC share
    (ENV-04), so the report shows whether the data read are the upstream blobs.
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


# --- Report states (Agent Action Plan 0.6.4, 0.7.5) -----------------------------------------------


#: The Models rows of each default run, ``(Model, Stratum, NOBS, DFE, Status)`` in fitting order.
#: Each 07.02 endpoint fit uses 226 of its 227 records (S02-MOD-02, D-08). Each 07.07 timepoint has
#: its per-study fits by ``studynum`` (S07-MOD-02), then the pooled fit at ``studynum = &stdyn = 3``
#: over both studies' records (S07-MOD-03). Every fit estimates the Intercept, BASE and the two
#: non-reference TRTPN levels, TRTPN 0 and the pooled ``studynum`` being aliased, so DFE = NOBS - 4.
_STATE_DEFAULT_MODELS: Mapping[str, tuple[tuple[str, str, str, str, str], ...]] = {
    PROGRAM_07_02: (
        ("endpoint", "PARAMCD=DIABP ATPTN=815", "226", "222", "fitted"),
        ("endpoint", "PARAMCD=SYSBP ATPTN=816", "226", "222", "fitted"),
    ),
    PROGRAM_07_07: (
        ("study", "PARAMCD=DIABP ATPTN=815 studynum=1", "124", "120", "fitted"),
        ("study", "PARAMCD=DIABP ATPTN=815 studynum=2", "126", "122", "fitted"),
        ("pooled", "PARAMCD=DIABP ATPTN=815 studynum=3", "250", "246", "fitted"),
        ("study", "PARAMCD=DIABP ATPTN=817 studynum=1", "122", "118", "fitted"),
        ("study", "PARAMCD=DIABP ATPTN=817 studynum=2", "126", "122", "fitted"),
        ("pooled", "PARAMCD=DIABP ATPTN=817 studynum=3", "248", "244", "fitted"),
    ),
}

#: The columns of the Models table, in report order.
_STATE_MODEL_COLUMNS = ("Model", "Stratum", "NOBS", "DFE", "Status")

#: How the report marks the model quantities of a GLM skipped by setting (Agent Action Plan 0.6.4).
_STATE_GLM_SKIPPED = "not produced: GLM skipped by setting"

#: The status of a fit whose data lack reference level 0 (U-02) and of a fit without usable rows
#: (D-09), as Agent Action Plan 0.6.4 words them.
_STATE_REFERENCE_ABSENT = "not fitted: reference level 0 absent"
_STATE_NO_OBSERVATIONS = "no observations"

#: Per WPCT-F.07.02 stratum, the reason its listing rows and its Not compared entry give once the
#: DIABP/815 fit lacks reference level 0 (U-02) and the SYSBP/816 fit has no observations (D-09).
_STATE_UNFITTED_REASONS: Mapping[str, str] = {
    "PARAMCD=DIABP ATPTN=815": "not fitted: reference level 0 absent",
    "PARAMCD=SYSBP ATPTN=816": "model not fitted: no observations",
}

#: The listing rows of one 07.02 stratum: 5 ParameterEstimates rows × 4 values, and the 3 merged
#: ``pval`` cells of its CLASS levels (Agent Action Plan 0.7.3).
_STATE_STRATUM_LISTING_ROWS = (20, 3)

#: The run note of an empty derivation input, worded as Agent Action Plan 0.6.4 gives it (U-03).
_STATE_U03_NOTE = "SAS behaviour for empty derivation input not established (U-03)"

#: The stratum of a Not compared entry that covers the whole program.
_STATE_ALL_STRATA = "all strata"

#: The listing groups of a program section, in report order.
_STATE_LISTING_GROUPS = (
    compare.GROUP_LISTING_PARAMETER_ESTIMATES,
    compare.GROUP_LISTING_MERGED_PVAL,
)

#: Synthetic git object ids, so the integrity cases do not depend on this host's git: the blob of
#: every read-only input at the upstream commit, the working blob of an edited input, a ``HEAD``.
_STATE_BLOB = "0123456789abcdef" * 2 + "01234567"
_STATE_EDITED_BLOB = "89abcdef01234567" * 2 + "89abcdef"
_STATE_HEAD = "fedcba9876543210" * 2 + "fedcba98"

#: The read-only inputs the ``changed`` integrity case alters: one edited in the working tree, the
#: other missing from it, so it has no working blob.
_STATE_EDITED_INPUT = "whitepapers/WPCT/WPCT-F.07.02.sas"
_STATE_MISSING_INPUT = "data/adam/cdisc-split/README.md"

#: The introduction of the Source integrity section, up to its integrity statement.
_STATE_INTEGRITY_INTRO = (
    "Each read-only input's working-tree blob (`git hash-object --no-filters`) against its blob at "
    "the upstream commit `398a6d33ced9359ffb58c46650a6d488811176b1`: "
)


def _state_report(
    runs: Mapping[str, Any],
    listing_tables: Mapping[str, Sequence[object]],
    sas_checkpoints: Mapping[str, Sequence[object]],
    tolerances: compare.Tolerances,
    facts: provenance.Provenance,
) -> tuple[str, dict[str, tuple[compare.ComparisonGroup, ...]]]:
    """The report of ``runs`` and, per program, the real comparisons of its run without exports."""
    comparisons = {
        program: compare.compare_program(
            runs[program], listing_tables[program], sas_checkpoints[program], None, tolerances
        )
        for program in PROGRAMS
    }
    return report.render_report(runs, comparisons, facts, tolerances), comparisons


def _state_models(text: str, program: str) -> list[str]:
    """The non-blank lines of the Models subsection of ``program``'s section."""
    return [line for line in _subsection(_section(text, program), "Models") if line]


def _state_model_rows(text: str, program: str) -> list[tuple[str, ...]]:
    """The one Models table of ``program``'s section, as rows of :data:`_STATE_MODEL_COLUMNS`."""
    table = _single_table(_subsection(_section(text, program), "Models"))
    return [tuple(row[column] for column in _STATE_MODEL_COLUMNS) for row in table]


def _state_halt_lines(text: str, program: str) -> list[str]:
    """The halt lines of ``program``'s section, ``Halt: …`` or ``Halted: …``."""
    return [line for line in _section(text, program) if line.startswith(("Halt: ", "Halted: "))]


def _state_not_compared(text: str, program: str) -> list[tuple[str, str, str, str]]:
    """``program``'s Not compared rows as ``(Comparison, Quantity, Stratum, Reason)``."""
    return [
        (row["Comparison"], row["Quantity"], row["Stratum"], row["Reason"])
        for row in _single_table(_section(text, "Not compared"))
        if row["Program"] == program
    ]


def _state_listing_rows(
    text: str, groups: Sequence[compare.ComparisonGroup]
) -> dict[str, list[dict[str, str]]]:
    """The rendered rows of the ParameterEstimates and merged-pval tables of one program, by key."""
    section = _section(text, groups[0].program)
    return {
        key: _single_table(_subsection(section, _group_heading(_group(groups, key))))
        for key in _STATE_LISTING_GROUPS
    }


def _state_integrity_rows(changed: bool) -> tuple[provenance.IntegrityRow, ...]:
    """Synthetic Source integrity rows, one per read-only input, in ``READ_ONLY_INPUTS`` order.

    Without ``changed`` nothing was verified, so every row is unavailable and has no blob. With
    it, every input is unchanged except :data:`_STATE_EDITED_INPUT`, whose working blob differs,
    and :data:`_STATE_MISSING_INPUT`, which has no working blob; both are CHANGED.
    """
    if not changed:
        return tuple(
            provenance.IntegrityRow(path, None, None, provenance.UNAVAILABLE)
            for path in provenance.READ_ONLY_INPUTS
        )
    working = {_STATE_EDITED_INPUT: _STATE_EDITED_BLOB, _STATE_MISSING_INPUT: None}
    return tuple(
        provenance.IntegrityRow(
            path,
            _STATE_BLOB,
            working.get(path, _STATE_BLOB),
            provenance.STATUS_CHANGED if path in working else provenance.STATUS_UNCHANGED,
        )
        for path in provenance.READ_ONLY_INPUTS
    )


@pytest.mark.parametrize("program", PROGRAMS)
def test_s02_mod_02_and_s07_mod_02_03_default_models(
    program: str, written: _Written, results: Mapping[str, Any]
) -> None:
    """The Models subsection lists every GLM fit of the default run with its NOBS, DFE and status.

    It is the program section's first subsection, after the halt line ``Halt: none; every halting
    check passed.``, and its one table holds :data:`_STATE_DEFAULT_MODELS` in fitting order: the
    07.02 endpoint fits on 226 of 227 records (S02-MOD-02, D-08); per 07.07 timepoint the fits for
    ``studynum`` 1 and 2 (S07-MOD-02), then the pooled fit at ``studynum`` 3 (S07-MOD-03), whose
    NOBS is their sum. Each row matches the run's ``ModelRecord`` at its position (Agent Action
    Plan 0.6.4, 0.7.5).
    """
    expected = list(_STATE_DEFAULT_MODELS[program])
    assert _state_halt_lines(written.text, program) == [_NO_HALT_LINE]
    assert _subsections(_section(written.text, program))[0][0] == "Models"

    rows = _state_model_rows(written.text, program)
    assert rows == expected
    for _model, _stratum, nobs, dfe, _status in rows:
        assert int(dfe) == int(nobs) - 4
    if program == PROGRAM_07_07:
        for study_1, study_2, pooled in (rows[0:3], rows[3:6]):
            assert int(pooled[2]) == int(study_1[2]) + int(study_2[2])
        assert results[program].pooled_studynum == 3

    records = results[program].models
    assert len(records) == len(expected)
    for record, (model, stratum, nobs, dfe, status) in zip(records, expected, strict=True):
        record_stratum = f"PARAMCD={record.PARAMCD} ATPTN={record.ATPTN:g}"
        if record.STUDYNUM is not None:
            record_stratum += f" studynum={record.STUDYNUM}"
        assert (record.MODEL, record_stratum, record.NOBS, record.DFE, record.STATUS) == (
            model,
            stratum,
            int(nobs),
            int(dfe),
            status,
        )


@pytest.mark.parametrize("program", PROGRAMS)
def test_s02_mod_02_glm_skipped_by_setting_state(
    program: str,
    results: Mapping[str, Any],
    listing_tables: Mapping[str, Sequence[object]],
    sas_checkpoints: Mapping[str, Sequence[object]],
    tolerances: compare.Tolerances,
    repo_paths: RepoPaths,
) -> None:
    """A GLM skipped by setting is stated, and every model value of the listing FAILs, none passing.

    The run gets the state its driver produces with the baseline variable or the reference
    treatment unset (S02-MOD-02, S07-MOD-02, S07-MOD-03; Agent Action Plan 0.6.4): no model
    records, an empty ``parameter_estimates`` and a ``css_stats`` without ``pval``. Its Models
    subsection is the one line ``GLM: not produced: GLM skipped by setting.``, with no table. All
    ParameterEstimates and merged-pval rows the listing holds FAIL with that reason and no Python
    value; Not compared lists the GLM quantities once with it; the Summary counts those FAILs, so
    its Result line reports the failure.
    """
    default = results[program]
    stats = default.datasets["css_stats"]
    assert "pval" in stats.columns
    datasets = {
        **default.datasets,
        "parameter_estimates": default.datasets["parameter_estimates"].iloc[0:0],
        "css_stats": stats.drop(columns="pval"),
    }
    skipped = dataclasses.replace(default, glm_skipped=True, models=(), datasets=datasets)
    text, comparisons = _state_report(
        {**results, program: skipped},
        listing_tables,
        sas_checkpoints,
        tolerances,
        provenance.collect(repo_paths),
    )

    assert _state_models(text, program) == [f"GLM: {_STATE_GLM_SKIPPED}."]
    assert _tables(_subsection(_section(text, program), "Models")) == []
    assert _state_halt_lines(text, program) == [_NO_HALT_LINE]

    listing_rows = _state_listing_rows(text, comparisons[program])
    assert tuple(len(listing_rows[key]) for key in _STATE_LISTING_GROUPS) == _LISTING_ROWS[program]
    for key in _STATE_LISTING_GROUPS:
        group = _group(comparisons[program], key)
        assert (group.n_pass, group.n_not_compared, group.n_fail) == (0, 0, len(group.rows))
        for row in listing_rows[key]:
            assert row["Result"] == compare.FAIL
            assert _STATE_GLM_SKIPPED in row["Reason"]
            assert row["Python value"] == ""

    entries = _state_not_compared(text, program)
    assert [entry for entry in entries if entry[1].startswith("GLM ")] == [
        (_NO_VALUE, "GLM ParameterEstimates and merged pval", _STATE_ALL_STRATA, _STATE_GLM_SKIPPED)
    ]

    failed = sum(_LISTING_ROWS[program])
    summary = _summary_rows(text)[program]
    assert _status_counts(summary)[compare.FAIL] == failed
    assert _status_counts(summary)[compare.PASS] == _EXPECTED_PASS[program] - failed
    assert summary["Halted"] == "no"
    assert f"Result: FAIL: {failed} comparisons failed." in _section(text, "Summary")
    assert "pval" in results[program].datasets["css_stats"].columns


def test_u02_and_d09_unfitted_models_not_compared(
    results: Mapping[str, Any],
    listing_tables: Mapping[str, Sequence[object]],
    sas_checkpoints: Mapping[str, Sequence[object]],
    tolerances: compare.Tolerances,
    repo_paths: RepoPaths,
) -> None:
    """A model left unfitted shows its status, and its stratum is listed under Not compared.

    The first WPCT-F.07.02 endpoint fit gets the status ``not fitted: reference level 0 absent``
    (U-02) and the second ``no observations`` with NOBS 0 (D-09), neither with a DFE. The Models
    table shows those statuses with DFE ``.``. Not compared lists each stratum's GLM quantities with
    the reason :data:`_STATE_UNFITTED_REASONS` gives it, and the stratum's 20 ParameterEstimates
    and 3 merged-pval rows FAIL with that reason, none passing or dropped (S02-MOD-02; Agent Action
    Plan 0.6.4, 0.7.5).
    """
    default = results[PROGRAM_07_02]
    absent, empty = default.models
    unfitted = dataclasses.replace(
        default,
        models=(
            dataclasses.replace(absent, STATUS=_STATE_REFERENCE_ABSENT, DFE=None),
            dataclasses.replace(empty, STATUS=_STATE_NO_OBSERVATIONS, NOBS=0, DFE=None),
        ),
    )
    text, comparisons = _state_report(
        {**results, PROGRAM_07_02: unfitted},
        listing_tables,
        sas_checkpoints,
        tolerances,
        provenance.collect(repo_paths),
    )

    assert _state_model_rows(text, PROGRAM_07_02) == [
        ("endpoint", "PARAMCD=DIABP ATPTN=815", "226", ".", "not fitted: reference level 0 absent"),
        ("endpoint", "PARAMCD=SYSBP ATPTN=816", "0", ".", "no observations"),
    ]
    assert _state_halt_lines(text, PROGRAM_07_02) == [_NO_HALT_LINE]

    quantity = "GLM ParameterEstimates and merged pval (endpoint model)"
    unfitted_entries = [
        entry for entry in _state_not_compared(text, PROGRAM_07_02) if entry[1] == quantity
    ]
    assert unfitted_entries == [
        (_NO_VALUE, quantity, stratum, reason)
        for stratum, reason in _STATE_UNFITTED_REASONS.items()
    ]

    listing_rows = _state_listing_rows(text, comparisons[PROGRAM_07_02])
    for key, per_stratum in zip(_STATE_LISTING_GROUPS, _STATE_STRATUM_LISTING_ROWS, strict=True):
        rows = listing_rows[key]
        assert len(rows) == len(_STATE_UNFITTED_REASONS) * per_stratum
        for stratum, reason in _STATE_UNFITTED_REASONS.items():
            own = [row for row in rows if row["Stratum"].startswith(f"{stratum} ")]
            assert len(own) == per_stratum, f"{key} {stratum}: {len(own)} rows"
            assert {(row["Result"], row["Reason"]) for row in own} == {(compare.FAIL, reason)}

    failed = sum(_LISTING_ROWS[PROGRAM_07_02])
    summary = _summary_rows(text)[PROGRAM_07_02]
    assert _status_counts(summary)[compare.FAIL] == failed
    assert summary["Halted"] == "no"
    assert f"Result: FAIL: {failed} comparisons failed." in _section(text, "Summary")
    assert [record.STATUS for record in default.models] == ["fitted", "fitted"]


@pytest.mark.parametrize(
    "notes",
    [
        pytest.param((), id="empty_population"),
        pytest.param((_STATE_U03_NOTE,), id="u03_empty_derivation_input"),
    ],
)
def test_empty_population_and_u03_note(
    notes: tuple[str, ...],
    results: Mapping[str, Any],
    listing_tables: Mapping[str, Sequence[object]],
    sas_checkpoints: Mapping[str, Sequence[object]],
    tolerances: compare.Tolerances,
    repo_paths: RepoPaths,
) -> None:
    """An empty analysis population is no halt and fits no model; only U-03 adds a run note.

    The WPCT-F.07.07 run gets the state of a population with no stratum (Agent Action Plan 0.6.4):
    no model records, header-only datasets and no pooled ``studynum``; the
    ``u03_empty_derivation_input`` case adds the note of an empty derivation input (U-03). The
    Models subsection reads ``No model was fitted: the analysis population holds no stratum.``,
    the halt line and the Summary report no halt, and every listing value FAILs rather than passes.
    Not compared lists the U-03 note as a ``run note`` with its wording; the plain empty population
    has no ``run note`` row and no GLM entry.
    """
    default = results[PROGRAM_07_07]
    datasets = {name: frame.iloc[0:0] for name, frame in default.datasets.items()}
    empty = dataclasses.replace(
        default, models=(), datasets=datasets, pooled_studynum=None, notes=notes
    )
    text, comparisons = _state_report(
        {**results, PROGRAM_07_07: empty},
        listing_tables,
        sas_checkpoints,
        tolerances,
        provenance.collect(repo_paths),
    )

    assert _state_models(text, PROGRAM_07_07) == [
        "No model was fitted: the analysis population holds no stratum."
    ]
    assert _tables(_subsection(_section(text, PROGRAM_07_07), "Models")) == []
    assert _state_halt_lines(text, PROGRAM_07_07) == [_NO_HALT_LINE]
    assert _summary_rows(text)[PROGRAM_07_07]["Halted"] == "no"
    assert not any(line.startswith(f"{PROGRAM_07_07} halted") for line in _section(text, "Summary"))

    listing_rows = _state_listing_rows(text, comparisons[PROGRAM_07_07])
    counts = tuple(len(listing_rows[key]) for key in _STATE_LISTING_GROUPS)
    assert counts == _LISTING_ROWS[PROGRAM_07_07]
    assert {row["Result"] for key in _STATE_LISTING_GROUPS for row in listing_rows[key]} == {
        compare.FAIL
    }

    entries = _state_not_compared(text, PROGRAM_07_07)
    assert [entry for entry in entries if entry[1] == "run note"] == [
        (_NO_VALUE, "run note", _STATE_ALL_STRATA, note) for note in notes
    ]
    assert not any(entry[1].startswith("GLM ") for entry in entries)
    assert (_STATE_U03_NOTE in text) == bool(notes)


def test_s07_flt_04_halt_rendered(
    results: Mapping[str, Any],
    listing_tables: Mapping[str, Sequence[object]],
    sas_checkpoints: Mapping[str, Sequence[object]],
    tolerances: compare.Tolerances,
    repo_paths: RepoPaths,
) -> None:
    """A halted run is reported as halted, and what it never reached is NOT COMPARED, never PASS.

    WPCT-F.07.07 halts at CSS_SAFANA on a missing CHG (S07-FLT-04), as ``ProgramResult.from_halt``
    records it. The Summary marks it halted, repeats the message and ends ``Result: FAIL: 1
    comparison failed; WPCT-F.07.07 halted.``. Its section states ``Halted: <message>``, says no
    model was fitted because the run halted before the GLM step, and has only the Models, Halt and
    log-checkpoint subsections. The Halt table's one FAIL row is ``program run`` with SAS value
    ``completed``, Python value ``halted`` and the message as reason; the 14 checkpoints reached
    PASS and the 15 after the halt are NOT COMPARED. Not compared lists the listing
    ParameterEstimates and merged pval as not compared because the program halted (Agent Action
    Plan 0.6.4, 0.7.5).
    """
    halt = _css_safana_halt(results[PROGRAM_07_07])
    assert len(halt.checkpoints) == 14
    halted = type(results[PROGRAM_07_07]).from_halt(halt)
    text, _ = _state_report(
        {**results, PROGRAM_07_07: halted},
        listing_tables,
        sas_checkpoints,
        tolerances,
        provenance.collect(repo_paths),
    )

    summary_lines = _section(text, "Summary")
    summary = _summary_rows(text)
    assert (summary[PROGRAM_07_02]["Halted"], summary[PROGRAM_07_07]["Halted"]) == ("no", "yes")
    counts = _status_counts(summary[PROGRAM_07_07])
    assert (counts[compare.PASS], counts[compare.FAIL]) == (14, 1)
    assert [line for line in summary_lines if " halted: " in line] == [
        f"WPCT-F.07.07 halted: {_HALT_MESSAGE}"
    ]
    assert "Result: FAIL: 1 comparison failed; WPCT-F.07.07 halted." in summary_lines

    section = _section(text, PROGRAM_07_07)
    log_title = "Log checkpoints (WPCT-F.07.07.log)"
    assert _state_halt_lines(text, PROGRAM_07_07) == [f"Halted: {_HALT_MESSAGE}"]
    assert _state_halt_lines(text, PROGRAM_07_02) == [_NO_HALT_LINE]
    assert _state_models(text, PROGRAM_07_07) == [
        "No model was fitted: the run halted before the GLM step."
    ]
    assert [heading for heading, _ in _subsections(section)] == ["Models", "Halt", log_title]

    halt_rows = _single_table(_subsection(section, "Halt"))
    assert [
        (r["Quantity"], r["Stratum"], r["SAS value"], r["Python value"], r["Result"], r["Reason"])
        for r in halt_rows
    ] == [("program run", _NO_VALUE, "completed", "halted", compare.FAIL, _HALT_MESSAGE)]

    checkpoints = _single_table(_subsection(section, log_title))
    reached = [f"{cp.SAS_DATASET} #{cp.OCCURRENCE}" for cp in halt.checkpoints]
    assert [row["Quantity"] for row in checkpoints[:14]] == reached
    statuses = [row["Result"] for row in checkpoints]
    assert statuses == [compare.PASS] * 14 + [compare.NOT_COMPARED] * 15
    assert {row["Reason"] for row in checkpoints[14:]} == {
        "not reached: the run halted before this step"
    }

    entries = _state_not_compared(text, PROGRAM_07_07)
    assert [entry[1] for entry in entries if entry[0] == log_title] == [
        row["Quantity"] for row in checkpoints[14:]
    ]
    halted_reason = f"program halted: {_HALT_MESSAGE}"
    assert [entry for entry in entries if entry[3] == halted_reason] == [
        (_NO_VALUE, "ParameterEstimates against the listing", _STATE_ALL_STRATA, halted_reason),
        (_NO_VALUE, "merged CSS_STATS.pval against the listing", _STATE_ALL_STRATA, halted_reason),
    ]


@pytest.mark.parametrize(
    ("git_available", "head", "head_shown", "changed", "statement"),
    [
        pytest.param(
            False,
            "unavailable",
            "unavailable",
            False,
            "git is unavailable, so integrity was not verified.",
            id="git_unavailable",
        ),
        pytest.param(
            True,
            _STATE_HEAD,
            f"`{_STATE_HEAD}`",
            False,
            "the upstream commit 398a6d33ced9359ffb58c46650a6d488811176b1 is unavailable in this "
            "checkout, so integrity was not verified.",
            id="commit_unavailable",
        ),
        pytest.param(
            True,
            _STATE_HEAD,
            f"`{_STATE_HEAD}`",
            True,
            "2 of 36 read-only inputs differ from their blobs at the upstream commit "
            "(see Source integrity).",
            id="changed",
        ),
    ],
)
def test_env01_source_integrity_explanations(
    git_available: bool,
    head: str,
    head_shown: str,
    changed: bool,
    statement: str,
    results: Mapping[str, Any],
    clean_comparisons: _Comparisons,
    tolerances: compare.Tolerances,
    repo_paths: RepoPaths,
) -> None:
    """The report says when integrity was not verified, and counts and names the changed inputs.

    The provenance facts of the checkout get synthetic git facts, so no case depends on this
    host's git (Agent Action Plan 0.1.1, 0.7.5, 0.11.2): git unavailable, with ``HEAD`` shown as
    ``unavailable``; the upstream commit unavailable; or two CHANGED inputs, one edited and one
    missing. The synthetic edit is to the 07.02 program, whose legacy environment settings the
    specification records without fixing them (ENV-01, ENV-02, ENV-03), so an edit there must show
    as CHANGED. The statement of each is the Provenance ``- Source integrity: `` line and ends the
    Source integrity introduction. Only changed inputs add ``Changed inputs: <paths>.``. The table
    shows every row's status, and ``unavailable`` for each missing blob.
    """
    assert {_STATE_EDITED_INPUT, _STATE_MISSING_INPUT} <= set(provenance.READ_ONLY_INPUTS)
    facts = dataclasses.replace(
        provenance.collect(repo_paths),
        git_available=git_available,
        head_commit=head,
        integrity=_state_integrity_rows(changed),
    )
    text = report.render_report(results, clean_comparisons, facts, tolerances)

    section = _section(text, "Provenance")
    assert [line for line in section if line.startswith("- Source integrity: ")] == [
        f"- Source integrity: {statement}"
    ]
    assert [line for line in section if "git rev-parse HEAD" in line] == [
        f"- Checkout `HEAD` when the report was generated (`git rev-parse HEAD`): {head_shown}"
    ]

    integrity = _section(text, "Source integrity")
    assert [line for line in integrity if line.startswith("Each read-only input")] == [
        _STATE_INTEGRITY_INTRO + statement
    ]
    assert [line for line in integrity if line.startswith("Changed inputs")] == (
        [f"Changed inputs: {_STATE_EDITED_INPUT}, {_STATE_MISSING_INPUT}."] if changed else []
    )
    assert "match their blobs" not in text

    blob, edited = f"`{_STATE_BLOB}`", f"`{_STATE_EDITED_BLOB}`"
    expected = {
        path: ("unavailable", "unavailable", "unavailable") for path in provenance.READ_ONLY_INPUTS
    }
    if changed:
        expected = {path: (blob, blob, "unchanged") for path in provenance.READ_ONLY_INPUTS}
        expected[_STATE_EDITED_INPUT] = (blob, edited, "CHANGED")
        expected[_STATE_MISSING_INPUT] = (blob, "unavailable", "CHANGED")
    table = _single_table(integrity)
    assert [
        (
            _code_text(row["Path"]),
            row["Blob at upstream commit"],
            row["Working-tree blob"],
            row["Result"],
        )
        for row in table
    ] == [(path, *cells) for path, cells in expected.items()]


def test_s02_and_s07_prs_01_report_deterministic(
    written: _Written,
    results: Mapping[str, object],
    clean_comparisons: _Comparisons,
    tolerances: compare.Tolerances,
    write_paths: RepoPaths,
) -> None:
    """Equal inputs give equal text, with no timestamp, absolute path or ``\\r``.

    Two renders, one with freshly collected provenance, equal the written file. The report quotes
    no SAS page header, presentation output the port excludes (S02-PRS-01, S07-PRS-01), so no time
    of day appears anywhere in it.
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
def test_env03_reproduce_under_write_root(
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
    ``python/wpct/outputs/<program>/`` file of ``schemas.EXPORTS``, the fixed locations the port
    writes to because it has no counterpart of the SAS ``outputs_folder`` setting (ENV-03,
    ENV-04), and the checkout's own report and outputs locations are unchanged.
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


def test_s02_and_s07_dep_assert_continue_reproduce_rejects_missing_sas_exports_dir(
    write_paths: RepoPaths, tmp_path: Path, capsys: pytest.CaptureFixture[str]
) -> None:
    """A ``--sas-exports`` that is not an existing directory is an input error: exit 2, no writes.

    The check runs before anything is read or written, so the write root stays empty and the named
    directory is not created. That is the outcome the specification gives an input error after a
    failed dependency assertion (S02-DEP-assert_continue, S07-DEP-assert_continue): the error on
    stderr, exit code 2 and nothing written.
    """
    missing = tmp_path / "no_such_exports"

    code = cli.reproduce(write_paths, missing)

    assert code == cli.EXIT_INPUT_ERROR == 2
    err = capsys.readouterr().err
    assert "not an existing directory" in err
    assert str(missing) in err
    assert _tree(write_paths.write_root) == {}
    assert not missing.exists()


#: The stderr prefix of every error ``reproduce`` reports before it returns exit code 2.
_CLI_ERROR_PREFIX = f"{cli.PROG} reproduce: error: "

#: The WPCT-F.07.02 ParameterEstimates export a user may supply (Agent Action Plan 0.7.3).
_CLI_ESTIMATES_EXPORT = export_spec(PROGRAM_07_02, "parameter_estimates").sas_export_file

#: The SAS variables of that export, as ``wpct.schemas`` alone defines them: the stratum columns the
#: user adds, then the ODS ParameterEstimates columns.
_CLI_ESTIMATES_COLUMNS = (
    *SAS_EXPORT_STRATUM_COLUMNS[str(_CLI_ESTIMATES_EXPORT)],
    *SAS_PARAMETER_ESTIMATE_COLUMNS,
)

#: The fabricated values of a synthetic stand-in for that export, by compared field. None is a SAS
#: result, and each lies far outside every tolerance around the Python fit, so each must FAIL.
_CLI_FABRICATED_VALUES: Mapping[str, str] = MappingProxyType(
    {"ESTIMATE": "-999.5", "STDERR": "0.25", "TVALUE": "-3998", "PROBT": "0.5"}
)

#: The stand-in's one row by upper-folded SAS variable: the real DIABP/815 Intercept key, then the
#: fabricated values. It is synthetic, and no SAS result.
_CLI_FABRICATED_RECORD: Mapping[str, str] = MappingProxyType(
    {"PARAMCD": "DIABP", "ATPTN": "815", "PARAMETER": "Intercept", **_CLI_FABRICATED_VALUES}
)

#: That row's cells in the export's column order, each matched to its column case-insensitively.
#: A schema column the row has no cell for fails at import with that column's name.
_CLI_FABRICATED_CELLS = tuple(
    _CLI_FABRICATED_RECORD[name] for name in upper_columns(_CLI_ESTIMATES_COLUMNS)
)

#: The synthetic stand-in: the schema's header row, then that row.
_CLI_FABRICATED_EXPORT = (
    f"{','.join(_CLI_ESTIMATES_COLUMNS)}\n{','.join(_CLI_FABRICATED_CELLS)}\n"
).encode("latin-1")

#: The schema's spelling of the export's ``ESTIMATE`` column.
_CLI_ESTIMATE_COLUMN = dict(zip(upper_columns(_CLI_ESTIMATES_COLUMNS), _CLI_ESTIMATES_COLUMNS))[
    "ESTIMATE"
]

#: A stand-in for the same export whose header is the schema's, plus ``Estimate`` repeated in
#: swapped case, which the export contract refuses
#: (``python/wpct/reference/sas_exports/README.md``). Its row repeats the fabricated estimate under
#: that column; no value is a SAS result.
_CLI_MALFORMED_EXPORT = (
    f"{','.join((*_CLI_ESTIMATES_COLUMNS, _CLI_ESTIMATE_COLUMN.swapcase()))}\n"
    f"{','.join((*_CLI_FABRICATED_CELLS, _CLI_FABRICATED_VALUES['ESTIMATE']))}\n"
).encode("latin-1")


def _cli_watch_drivers(
    monkeypatch: pytest.MonkeyPatch, raising: Mapping[str, Exception]
) -> list[tuple[str, bool, str]]:
    """Record every driver run ``reproduce`` starts; a program in ``raising`` raises instead.

    ``reproduce`` calls ``driver.run(driver.Config(), paths)`` for each driver module in
    ``cli._DRIVERS``, so replacing ``run`` on that module changes what it runs, and
    ``monkeypatch`` restores it after the test. A program not in ``raising`` runs its real driver.
    The returned list gains one ``(program, default Config, outcome)`` entry per run, in run order:
    whether the run got its program's own ``%let`` settings (S02-TRN-01, S07-TRN-01), and
    ``"returned"`` or the name of the exception the run raised.
    """
    assert set(raising) <= {program for program, _driver in cli._DRIVERS}
    runs: list[tuple[str, bool, str]] = []
    for program, driver in cli._DRIVERS:

        def run(
            config: Any,
            paths: RepoPaths,
            *,
            _program: str = program,
            _driver: Any = driver,
            _real: Callable[..., Any] = driver.run,
        ) -> Any:
            default = config == _driver.Config()
            if _program in raising:
                runs.append((_program, default, type(raising[_program]).__name__))
                raise raising[_program]
            try:
                result = _real(config, paths)
            except Exception as exc:
                runs.append((_program, default, type(exc).__name__))
                raise
            runs.append((_program, default, "returned"))
            return result

        monkeypatch.setattr(driver, "run", run)
    return runs


def _cli_exports_dir(tmp_path: Path, files: Mapping[str, bytes]) -> Path:
    """A new ``--sas-exports`` directory under ``tmp_path`` holding exactly ``files``, by name."""
    directory = tmp_path / "sas_exports"
    directory.mkdir()
    for name, data in files.items():
        (directory / name).write_bytes(data)
    return directory


def _cli_export_files(paths: RepoPaths, programs: Sequence[str]) -> set[str]:
    """Every ``schemas.EXPORTS`` file of ``programs``, relative to the write root."""
    root = paths.write_root
    return {
        (paths.program_outputs_dir(program) / spec.file_name).relative_to(root).as_posix()
        for program in programs
        for spec in EXPORTS[program]
    }


def _cli_expected_tree(planted: Mapping[str, str], files: set[str]) -> dict[str, str]:
    """``planted``, as :func:`_tree` lists it, plus ``files`` and the directories above each."""
    tree = dict(planted)
    for relative in files:
        for parent in Path(relative).parents:
            if parent != Path("."):
                tree.setdefault(parent.as_posix(), "dir")
        tree[relative] = "file"
    return tree


def _cli_shown_path(text: str, paths: RepoPaths) -> Path:
    """The path ``reproduce`` printed as ``text``: checkout-relative, or absolute outside it."""
    shown = Path(text)
    return (shown if shown.is_absolute() else paths.repo_root / shown).resolve()


def _cli_printed(out: str, paths: RepoPaths) -> dict[str, tuple[dict[str, int], str]]:
    """The status lines of a ``reproduce`` run that wrote its report, keyed by program and Overall.

    stdout must read one line per program in ``schemas.PROGRAMS`` order, ``<program>: PASS <a>,
    FAIL <b>, NOT COMPARED <c>; <outcome>``, then ``Overall:`` with the same three counts, then
    ``Report: <path>`` naming the report file under the write root. Each value is the counts and
    the outcome, which is empty for Overall.
    """
    *lines, report_line, end = out.split("\n")
    assert end == "", out
    assert report_line.startswith("Report: "), out
    shown_report = _cli_shown_path(report_line[len("Report: ") :], paths)
    assert shown_report == paths.report_file.resolve()
    printed: dict[str, tuple[dict[str, int], str]] = {}
    for line in lines:
        name, _, text = line.partition(": ")
        counts_text, _, outcome = text.partition("; ")
        counts: dict[str, int] = {}
        for part in counts_text.split(", "):
            status, _, number = part.rpartition(" ")
            counts[status] = int(number)
        assert tuple(counts) == compare.STATUSES, line
        printed[name] = (counts, outcome)
    assert list(printed) == [*PROGRAMS, "Overall"], out
    assert printed["Overall"][1] == "", out
    return printed


def _cli_assert_outputs_line(outcome: str, program: str, paths: RepoPaths) -> None:
    """``outcome`` is ``outputs in <dir>``, naming ``program``'s export directory."""
    prefix = "outputs in "
    assert outcome.startswith(prefix), outcome
    shown = _cli_shown_path(outcome[len(prefix) :], paths)
    assert shown == paths.program_outputs_dir(program).resolve()


def _cli_errors(err: str) -> list[str]:
    """The error lines ``reproduce`` wrote to stderr, skipping any logged warning line."""
    return [line for line in err.split("\n") if line.startswith(_CLI_ERROR_PREFIX)]


def test_reproduce_s07_flt_04_halt_exits_1(
    write_paths: RepoPaths,
    results: Mapping[str, Any],
    clean_comparisons: _Comparisons,
    tmp_path: Path,
    monkeypatch: pytest.MonkeyPatch,
    capsys: pytest.CaptureFixture[str],
) -> None:
    """A WPCT-F.07.07 halt (S07-FLT-04) makes ``reproduce`` exit 1 and is reported, not hidden.

    The 07.07 driver raises the ``AnalysisHalted`` of ``%assert_var_nonmissing`` with the real
    checkpoints up to CSS_SAFANA; 07.02 runs its real driver, and ``--sas-exports`` names an empty
    directory. Agent Action Plan 0.6.4 and 0.6.7: the halted program exports nothing, while the
    07.02 exports and the report are written; 0.6.5: the exit code is 1.

    stdout gives 07.07's halt message and 07.02's export directory. The report records the halt in
    the Summary (Halted ``yes``, one FAIL, the halt line and the Result line), in the WPCT-F.07.07
    section (``Halted:``, the Halt group's FAIL row carrying the message, and no listing group) and
    under Not compared (both listing groups, with the halt as reason). The checkpoints after
    CSS_SAFANA are NOT COMPARED and listed there, never PASS (Agent Action Plan 0.7.4), and the
    07.02 counts are those of a clean checkout.
    """
    exports_dir = _cli_exports_dir(tmp_path, {})
    halt = _css_safana_halt(results[PROGRAM_07_07])
    runs = _cli_watch_drivers(monkeypatch, {PROGRAM_07_07: halt})

    code = cli.reproduce(write_paths, exports_dir)

    out = capsys.readouterr().out
    assert code == cli.EXIT_FAILED == 1, out
    assert runs == [(PROGRAM_07_02, True, "returned"), (PROGRAM_07_07, True, "AnalysisHalted")]
    printed = _cli_printed(out, write_paths)
    assert printed[PROGRAM_07_07][1] == f"halted, nothing exported: {_HALT_MESSAGE}"
    _cli_assert_outputs_line(printed[PROGRAM_07_02][1], PROGRAM_07_02, write_paths)

    assert not write_paths.program_outputs_dir(PROGRAM_07_07).exists()
    files = {REPORT_RELATIVE.as_posix(), *_cli_export_files(write_paths, (PROGRAM_07_02,))}
    assert _tree(write_paths.write_root) == _cli_expected_tree({}, files)

    text = write_paths.report_file.read_bytes().decode("utf-8")
    rows = _summary_rows(text)
    assert {name: rows[name]["Halted"] for name in PROGRAMS} == {
        PROGRAM_07_02: "no",
        PROGRAM_07_07: "yes",
    }
    for name, (counts, _outcome) in printed.items():
        assert _status_counts(rows[name]) == counts, name
    clean_not_compared = _rows_with_status(clean_comparisons, PROGRAM_07_02, compare.NOT_COMPARED)
    assert _status_counts(rows[PROGRAM_07_02]) == {
        compare.PASS: _EXPECTED_PASS[PROGRAM_07_02],
        compare.FAIL: 0,
        compare.NOT_COMPARED: clean_not_compared,
    }
    assert _status_counts(rows[PROGRAM_07_07])[compare.FAIL] == 1
    summary = _section(text, "Summary")
    assert summary.count(f"{PROGRAM_07_07} halted: {_HALT_MESSAGE}") == 1
    assert [line for line in summary if line.startswith("Result: ")] == [
        f"Result: FAIL: 1 comparison failed; {PROGRAM_07_07} halted."
    ]

    section_02 = _section(text, PROGRAM_07_02)
    assert _NO_HALT_LINE in section_02
    assert not any(line.startswith("Halted: ") for line in section_02)
    section_07 = _section(text, PROGRAM_07_07)
    assert f"Halted: {_HALT_MESSAGE}" in section_07
    assert not any(line.startswith("Halt: none") for line in section_07)
    blocks = dict(_subsections(section_07))
    halt_rows = _single_table(blocks["Halt"])
    assert [(row["Result"], row["Reason"]) for row in halt_rows] == [(compare.FAIL, _HALT_MESSAGE)]
    clean_07 = clean_comparisons[PROGRAM_07_07]
    for key in (compare.GROUP_LISTING_PARAMETER_ESTIMATES, compare.GROUP_LISTING_MERGED_PVAL):
        assert _group_heading(_group(clean_07, key)) not in blocks, key

    checkpoint_title = _group(clean_07, compare.GROUP_LOG_CHECKPOINTS).title
    checkpoints = _single_table(blocks[checkpoint_title])
    assert len(checkpoints) == 29
    quantities = [row["Quantity"] for row in checkpoints]
    last = next(index for index, quantity in enumerate(quantities) if quantity == "CSS_SAFANA #1")
    reached, unreached = checkpoints[: last + 1], checkpoints[last + 1 :]
    assert unreached, "a halt at CSS_SAFANA must leave checkpoints unreached"
    assert {row["Result"] for row in reached} == {compare.PASS}
    assert {row["Result"] for row in unreached} == {compare.NOT_COMPARED}
    assert all(row["Reason"] and row["Reason"] != _NO_VALUE for row in unreached)
    assert _status_counts(rows[PROGRAM_07_07])[compare.PASS] == len(reached)

    not_compared = _single_table(_section(text, "Not compared"))
    listed = [
        row["Quantity"]
        for row in not_compared
        if (row["Program"], row["Comparison"]) == (PROGRAM_07_07, checkpoint_title)
    ]
    assert listed == [row["Quantity"] for row in unreached]
    halted = [
        (row["Program"], row["Quantity"])
        for row in not_compared
        if row["Reason"] == f"program halted: {_HALT_MESSAGE}"
    ]
    assert len(halted) == 2, halted
    (estimates_program, estimates), (pval_program, pval) = halted
    assert estimates_program == pval_program == PROGRAM_07_07
    assert "ParameterEstimates" in estimates
    assert "merged CSS_STATS.pval" in pval


def test_s02_mod_02_reproduce_failed_export_comparison_exits_1(
    write_paths: RepoPaths,
    clean_comparisons: _Comparisons,
    tmp_path: Path,
    file_state: _FileState,
    capsys: pytest.CaptureFixture[str],
) -> None:
    """A comparison that FAILs makes ``reproduce`` exit 1, with every output still written.

    ``--sas-exports`` names a directory under ``tmp_path`` holding only a synthetic stand-in for
    ``WPCT-F.07.02_parameter_estimates.csv``, the export of the endpoint ANCOVA's
    ParameterEstimates (S02-MOD-02): one row on the real DIABP/815 Intercept key with fabricated
    values. The real loader and comparison judge it (Agent Action Plan 0.7.3), so its
    ``Export <file>`` group in the WPCT-F.07.02 section FAILs on every row, each with a reason, the
    fabricated SAS values among them, while every listing and log comparison still passes. No
    program halted, so both programs' exports and the report are written, and the exit code is 1
    (Agent Action Plan 0.6.5). The export directory is a read-only input (0.8): the stand-in's
    bytes and ``st_mtime_ns`` and the directory's listing are unchanged.
    """
    exports_dir = _cli_exports_dir(tmp_path, {_CLI_ESTIMATES_EXPORT: _CLI_FABRICATED_EXPORT})
    standin = exports_dir / _CLI_ESTIMATES_EXPORT
    before = (file_state(standin), file_state(exports_dir))
    assert before[1] == [_CLI_ESTIMATES_EXPORT]

    code = cli.reproduce(write_paths, exports_dir)

    out = capsys.readouterr().out
    assert code == cli.EXIT_FAILED == 1, out
    printed = _cli_printed(out, write_paths)
    failed = printed["Overall"][0][compare.FAIL]
    assert failed > 0
    for program in PROGRAMS:
        _cli_assert_outputs_line(printed[program][1], program, write_paths)
    files = {REPORT_RELATIVE.as_posix(), *_cli_export_files(write_paths, PROGRAMS)}
    assert _tree(write_paths.write_root) == _cli_expected_tree({}, files)
    assert (file_state(standin), file_state(exports_dir)) == before

    text = write_paths.report_file.read_bytes().decode("utf-8")
    rows = _summary_rows(text)
    assert {rows[name]["Halted"] for name in PROGRAMS} == {"no"}
    for name, (counts, _outcome) in printed.items():
        assert _status_counts(rows[name]) == counts, name
    assert _status_counts(rows[PROGRAM_07_02])[compare.PASS] == _EXPECTED_PASS[PROGRAM_07_02]
    assert _status_counts(rows[PROGRAM_07_02])[compare.FAIL] == failed
    assert _status_counts(rows[PROGRAM_07_07]) == {
        compare.PASS: _EXPECTED_PASS[PROGRAM_07_07],
        compare.FAIL: 0,
        compare.NOT_COMPARED: _rows_with_status(
            clean_comparisons, PROGRAM_07_07, compare.NOT_COMPARED
        ),
    }
    summary = _section(text, "Summary")
    assert not any(" halted: " in line for line in summary)
    noun = "comparison" if failed == 1 else "comparisons"
    assert [line for line in summary if line.startswith("Result: ")] == [
        f"Result: FAIL: {failed} {noun} failed."
    ]

    section = _section(text, PROGRAM_07_02)
    assert _NO_HALT_LINE in section
    block = _subsection(section, f"Export {_CLI_ESTIMATES_EXPORT}")
    table = _single_table(block)
    assert len(table) == failed
    assert f"{compare.PASS} 0, {compare.FAIL} {failed}, {compare.NOT_COMPARED} 0." in block
    assert {row["Result"] for row in table} == {compare.FAIL}
    assert all(row["Reason"] and row["Reason"] != _NO_VALUE for row in table)
    judged = [row for row in table if row["Quantity"] in _CLI_FABRICATED_VALUES]
    assert [row["Quantity"] for row in judged] == list(_CLI_FABRICATED_VALUES)
    for row in judged:
        assert row["SAS value"] == _CLI_FABRICATED_VALUES[row["Quantity"]]
        assert all(part in row["Stratum"] for part in ("DIABP", "815", "INTERCEPT")), row

    not_compared = _single_table(_section(text, "Not compared"))
    glm_programs = [
        row["Program"] for row in not_compared if "full-precision GLM values" in row["Quantity"]
    ]
    assert glm_programs == [PROGRAM_07_07]


@pytest.mark.parametrize("failure", ["malformed_sas_export", "missing_07_07_input"])
def test_s02_mod_02_reproduce_input_error_exits_2_writing_nothing(
    failure: str,
    write_paths: RepoPaths,
    tmp_path: Path,
    file_state: _FileState,
    monkeypatch: pytest.MonkeyPatch,
    capsys: pytest.CaptureFixture[str],
) -> None:
    """An input error makes ``reproduce`` exit 2 and write nothing (Agent Action Plan 0.6.4, 0.6.5).

    - ``malformed_sas_export``: ``--sas-exports`` holds a ParameterEstimates export (S02-MOD-02)
      whose header names ``Estimate`` twice in different case. ``load_sas_exports`` raises
      ``InputDataError`` naming the file and the column, never a value, before either driver runs;
      the file and its directory are unchanged.
    - ``missing_07_07_input``: the WPCT-F.07.07 driver raises the ``input dataset not found``
      ``InputDataError`` that S07-DEP-util_access_test_data gives an ADVS file missing from
      ``tmp_path``. WPCT-F.07.02 has run by then, yet its exports are not written, because every
      input is read before any write (Agent Action Plan 0.6.7).

    stdout stays empty, stderr carries one ``python -m wpct reproduce: error:`` line with the
    message, and the write root stays empty.
    """
    message: str | None = None
    if failure == "malformed_sas_export":
        exports_dir = _cli_exports_dir(tmp_path, {_CLI_ESTIMATES_EXPORT: _CLI_MALFORMED_EXPORT})
        runs = _cli_watch_drivers(monkeypatch, {})
        expected_runs: list[tuple[str, bool, str]] = []
    else:
        exports_dir = _cli_exports_dir(tmp_path, {})
        missing = tmp_path / "missing_input" / "advs.xpt"
        message = f"{PROGRAM_07_07}: input dataset not found: {missing}"
        runs = _cli_watch_drivers(monkeypatch, {PROGRAM_07_07: InputDataError(message)})
        expected_runs = [
            (PROGRAM_07_02, True, "returned"),
            (PROGRAM_07_07, True, "InputDataError"),
        ]
    exports_before = {path: file_state(path) for path in (exports_dir, *exports_dir.iterdir())}

    code = cli.reproduce(write_paths, exports_dir)

    captured = capsys.readouterr()
    assert code == cli.EXIT_INPUT_ERROR == 2, captured.err
    assert captured.out == ""
    assert runs == expected_runs
    assert _tree(write_paths.write_root) == {}
    assert {path: file_state(path) for path in exports_before} == exports_before
    errors = _cli_errors(captured.err)
    if message is not None:
        assert errors == [_CLI_ERROR_PREFIX + message]
    else:
        assert len(errors) == 1, captured.err
        assert str(exports_dir / _CLI_ESTIMATES_EXPORT) in errors[0]
        assert "ESTIMATE" in errors[0]
        assert "-999.5" not in errors[0]


@pytest.mark.parametrize("link", ["outputs_dir", "program_outputs_dir", "reports_dir"])
def test_env03_reproduce_rejected_destination_exits_2(
    link: str,
    write_paths: RepoPaths,
    tmp_path: Path,
    file_state: _FileState,
    capsys: pytest.CaptureFixture[str],
) -> None:
    """A destination the write guard refuses makes ``reproduce`` exit 2 (AAP 0.6.5, 0.8).

    The guard holds every write to the fixed locations under ``python/wpct/``, as the port has no
    counterpart of the SAS ``outputs_folder`` setting (ENV-03, ENV-04). The write root's ``link``
    is a symlink to an external directory under ``tmp_path`` that already holds ``sentinel.csv``:

    - ``outputs_dir``: ``python/wpct/outputs``. The first export call, WPCT-F.07.02's, refuses its
      first target, so nothing is written.
    - ``program_outputs_dir``: ``python/wpct/outputs/WPCT-F.07.07``. The WPCT-F.07.02 export call
      has written its files, which remain; the WPCT-F.07.07 call refuses its first target and
      writes none of its files.
    - ``reports_dir``: ``python/wpct/reports``. Both export calls have written their files, which
      remain; ``write_report`` refuses the report path.

    Each writer call vets every target before it opens any, and an earlier call is not undone.
    The run stops at the refusal: stderr carries the ``OutputPathError`` message naming the refused
    target and its resolved path, stdout stays empty, so no ``Report:`` line is printed, and no
    report exists. The external directory's listing and the sentinel's bytes and ``st_mtime_ns``
    are unchanged, and the write root holds only the planted link and the earlier calls' files.
    """
    exports_dir = _cli_exports_dir(tmp_path, {})
    external_dir = tmp_path / "external_dir"
    external_dir.mkdir()
    sentinel = external_dir / "sentinel.csv"
    sentinel.write_bytes(_SAS_EXPORT_STANDIN)
    if link == "outputs_dir":
        link_path, kept = write_paths.outputs_dir, ()
        first_export = EXPORTS[PROGRAM_07_02][0].file_name
        target = write_paths.program_outputs_dir(PROGRAM_07_02) / first_export
    elif link == "program_outputs_dir":
        link_path, kept = write_paths.program_outputs_dir(PROGRAM_07_07), (PROGRAM_07_02,)
        target = link_path / EXPORTS[PROGRAM_07_07][0].file_name
    else:
        link_path, kept = write_paths.report_file.parent, PROGRAMS
        target = write_paths.report_file
    link_path.parent.mkdir(parents=True)
    _symlink(link_path, external_dir)
    resolved = external_dir.resolve() / target.relative_to(link_path)
    planted = _tree(write_paths.write_root)
    assert planted[link_path.relative_to(write_paths.write_root).as_posix()] == "symlink"
    sentinel_before = file_state(sentinel)
    external_before = file_state(external_dir)

    code = cli.reproduce(write_paths, exports_dir)

    captured = capsys.readouterr()
    assert code == cli.EXIT_INPUT_ERROR == 2, captured.err
    assert captured.out == ""
    errors = _cli_errors(captured.err)
    assert errors == [_CLI_ERROR_PREFIX + str(OutputPathError(target, resolved))]
    assert str(target) in errors[0] and str(resolved) in errors[0]
    assert file_state(sentinel) == sentinel_before
    assert file_state(external_dir) == external_before == ["sentinel.csv"]
    assert not write_paths.report_file.exists()
    expected = _cli_expected_tree(planted, _cli_export_files(write_paths, kept))
    assert _tree(write_paths.write_root) == expected


def test_env03_reproduce_nul_byte_write_root_exits_2(
    write_paths: RepoPaths,
    tmp_path: Path,
    file_state: _FileState,
    capsys: pytest.CaptureFixture[str],
) -> None:
    """A write root holding a NUL byte makes ``reproduce`` exit 2, not raise (AAP 0.6.5, 0.8).

    Outputs go only to fixed locations under ``python/wpct/`` (ENV-03). Here the write root of
    ``write_paths`` is moved to ``sub\\x00dir`` inside it, a name no directory can have, so the guard
    cannot resolve the first target of the WPCT-F.07.02 export call and refuses it with
    ``OutputPathError`` instead of the ``ValueError`` that ``Path.resolve()`` raises. ``reproduce``
    reports that refusal as one stderr error line naming the target and its absolute form with the
    reason ``cannot resolve the destination``, and prints no traceback. stdout stays empty, the
    real write root stays empty and the ``--sas-exports`` directory is unchanged.
    """
    exports_dir = _cli_exports_dir(tmp_path, {})
    nul_paths = dataclasses.replace(write_paths, write_root=write_paths.write_root / "sub\x00dir")
    target = nul_paths.program_outputs_dir(PROGRAM_07_02) / EXPORTS[PROGRAM_07_02][0].file_name
    refusal = OutputPathError(target, os.path.abspath(target), "cannot resolve the destination: ")
    exports_before = file_state(exports_dir)

    code = cli.reproduce(nul_paths, exports_dir)

    captured = capsys.readouterr()
    assert code == cli.EXIT_INPUT_ERROR == 2, captured.err
    assert captured.out == ""
    assert "Traceback" not in captured.err
    errors = _cli_errors(captured.err)
    assert len(errors) == 1, captured.err
    assert errors[0].startswith(_CLI_ERROR_PREFIX + str(refusal)), errors[0]
    assert _tree(write_paths.write_root) == {}
    assert file_state(exports_dir) == exports_before == []


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

    ``relative`` names a SAS-era file of the checkout: the 07.02 program, the ADVS file the port
    reads there in place of the GitHub URL (ENV-02, S02-DEP-util_access_test_data) or the listing
    holding the endpoint ANCOVA reference (S02-MOD-02); or ``None`` for a report-named file under
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

    The report has one fixed location under ``python/wpct/``, as the port has no counterpart of
    the SAS ``outputs_folder`` setting (ENV-03, ENV-04), and a link must not lead its write
    elsewhere:

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


def test_env03_rejects_reports_dir_linked_into_outputs_tree(
    write_paths: RepoPaths,
    results: Mapping[str, object],
    clean_comparisons: _Comparisons,
    file_state: _FileState,
) -> None:
    """A reports directory symlinked into ``python/wpct/outputs`` is refused too (AAP 0.8).

    The guard alone accepts that target, because it resolves strictly below the outputs tree;
    the policy refuses every fixed destination reached through a symlink, inside the write root
    or outside it, so the report keeps its one fixed location, the port having no counterpart of
    the SAS ``outputs_folder`` setting (ENV-03, ENV-04), and ``write_report`` must still raise
    ``OutputPathError``. The decoy directory stays empty and the write root unchanged.
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


#: How ``wpct.paths.open_fixed_destination`` binds a write: ``dir_fd``, through directory
#: descriptors, or ``path``, its fallback for platforms whose ``os.open`` takes no ``dir_fd``.
_STRATEGIES = ("dir_fd", "path")

#: The entry each replaced-report case swaps in after the preflight, and the refusal reason
#: ``write_report`` must then give.
_SWAP_REASONS = {
    "final_symlink": "is reached through a symlink",
    "final_hard_link": "is a hard link shared with 1 other path(s)",
    "final_fifo": "not a regular file",
    "ancestor_symlink": "is reached through a symlink",
}


def _hard_link(link: Path, target: Path) -> None:
    """Create ``link`` as a hard link to ``target``, or skip where the file system refuses one."""
    try:
        os.link(target, link)
    except OSError as exc:
        pytest.skip(f"hard links not supported here: {exc}")


def _fifo(path: Path) -> None:
    """Create a FIFO at ``path``, or skip where the platform has none."""
    mkfifo = getattr(os, "mkfifo", None)
    if mkfifo is None:
        pytest.skip("FIFOs not supported on this platform")
    try:
        mkfifo(path)
    except OSError as exc:
        pytest.skip(f"FIFOs not supported here: {exc}")


def _use_strategy(strategy: str, monkeypatch: pytest.MonkeyPatch) -> None:
    """Make ``open_fixed_destination`` bind writes by ``strategy``, or skip where it cannot."""
    if strategy == "path":
        monkeypatch.setattr(wpct_paths, "_DIR_FD_WRITES", False)
    elif not wpct_paths._DIR_FD_WRITES:
        pytest.skip("this platform cannot open files relative to a directory descriptor")


@pytest.mark.parametrize("entry", ["hard_link", "fifo", "directory"])
def test_env03_rejects_unsafe_existing_report(
    entry: str,
    write_paths: RepoPaths,
    results: Mapping[str, object],
    clean_comparisons: _Comparisons,
    tmp_path: Path,
    file_state: _FileState,
) -> None:
    """A report path that aliases another file or is no regular file is refused (AAP 0.8).

    Outputs go only to fixed locations under ``python/wpct/`` (ENV-03), and no write may change a
    SAS-era file or a file outside the write root. Resolving a path follows symlinks but not hard
    links (CWE-59), so the entry itself is checked:

    - ``hard_link``: the report path is a hard link to a stand-in, under ``tmp_path``, for a SAS-era
      file or an external SAS export.
    - ``fifo``: the report path is a FIFO, which an ordinary open for writing waits on until a
      reader appears.
    - ``directory``: the report path is a directory.

    ``write_report`` must raise ``OutputPathError`` naming the report path and why, at once and
    before it creates a directory or opens a file: the stand-in's bytes and ``st_mtime_ns`` are
    unchanged and the write root holds only what the test planted.
    """
    report_file = write_paths.report_file
    report_file.parent.mkdir(parents=True)
    sentinel = tmp_path / "sentinel.csv"
    sentinel.write_bytes(_SAS_EXPORT_STANDIN)
    if entry == "fifo":
        _fifo(report_file)
        reason = "is a FIFO, not a regular file"
    elif entry == "directory":
        report_file.mkdir()
        reason = "is a directory, not a regular file"
    else:
        _hard_link(report_file, sentinel)
        reason = "is a hard link shared with 1 other path(s)"
    planted = _tree(write_paths.write_root)
    sentinel_before = file_state(sentinel)
    facts = provenance.collect(write_paths)

    with pytest.raises(OutputPathError) as excinfo:
        report.write_report(results, clean_comparisons, facts, write_paths)

    error = excinfo.value
    assert error.target == report_file
    assert error.resolved == report_file.resolve()
    assert f"the fixed destination {report_file.resolve()} {reason}" in str(error)
    assert file_state(sentinel) == sentinel_before
    assert _tree(write_paths.write_root) == planted


@pytest.mark.parametrize("strategy", _STRATEGIES)
def test_env03_rewrites_existing_report(
    strategy: str,
    monkeypatch: pytest.MonkeyPatch,
    write_paths: RepoPaths,
    results: Mapping[str, object],
    clean_comparisons: _Comparisons,
) -> None:
    """A report already on disk is rewritten in place, truncated first (AAP 0.8).

    Outputs go only to fixed locations under ``python/wpct/`` (ENV-03), and the report destination
    is that one file, so a second run writes the same file again instead of placing a new one
    beside it. ``strategy`` selects how ``open_fixed_destination`` binds the write: ``dir_fd`` or
    the ``path`` fallback, which must also create the directories of the first write. After a
    first clean write, a stale tail is appended to the report. A second write must return the same
    path and leave the file with the first write's UTF-8 bytes, its inode and a single hard link,
    in an unchanged tree.
    """
    _use_strategy(strategy, monkeypatch)
    facts = provenance.collect(write_paths)
    first = report.write_report(results, clean_comparisons, facts, write_paths)
    content = first.read_bytes()
    inode = os.stat(first).st_ino
    with open(first, "ab") as handle:
        handle.write(b"stale tail\n" * 1000)
    tree = _tree(write_paths.write_root)

    second = report.write_report(results, clean_comparisons, facts, write_paths)

    assert second == first == write_paths.report_file.resolve()
    assert second.read_bytes() == content
    assert content.decode("utf-8").endswith("\n")
    assert os.stat(second).st_ino == inode
    assert os.stat(second).st_nlink == 1
    assert _tree(write_paths.write_root) == tree


@pytest.mark.parametrize("swap", list(_SWAP_REASONS))
@pytest.mark.parametrize("strategy", _STRATEGIES)
def test_env03_refuses_report_replaced_after_vetting(
    strategy: str,
    swap: str,
    monkeypatch: pytest.MonkeyPatch,
    write_paths: RepoPaths,
    results: Mapping[str, object],
    clean_comparisons: _Comparisons,
    tmp_path: Path,
    file_state: _FileState,
) -> None:
    """A report path replaced after the preflight is refused before it is written (AAP 0.8).

    Outputs go only to fixed locations under ``python/wpct/`` (ENV-03), and no write may follow a
    symlink below the write root or change another path's file, even when an entry changes
    between the check and the open (CWE-367). Over a first clean write of the report,
    ``report.vet_fixed_destination`` is wrapped so that, as soon as it has accepted the report path
    on the second call, an entry staged beforehand is renamed into place:

    - ``final_symlink``: the report file becomes a symlink to an external stand-in for a SAS-export
      CSV, in a directory outside the write root.
    - ``final_hard_link``: the report file becomes a hard link to that stand-in.
    - ``final_fifo``: the report file becomes a FIFO without a reader.
    - ``ancestor_symlink``: ``python/wpct/reports`` becomes a symlink to the external directory.

    ``strategy`` selects how ``open_fixed_destination`` binds the write (``dir_fd`` or the ``path``
    fallback). ``write_report`` must raise ``OutputPathError`` at once, naming the report path and
    the reason. The stand-in's bytes and ``st_mtime_ns`` and the external directory's listing are
    unchanged, and the write root holds what it held after the swap.
    """
    _use_strategy(strategy, monkeypatch)
    report_file = write_paths.report_file
    facts = provenance.collect(write_paths)
    report.write_report(results, clean_comparisons, facts, write_paths)
    external_dir = tmp_path / "external_dir"
    external_dir.mkdir()
    sentinel = external_dir / "sentinel.csv"
    sentinel.write_bytes(_SAS_EXPORT_STANDIN)
    replaced = report_file.parent if swap == "ancestor_symlink" else report_file
    staged = replaced.with_name("staged")
    if swap == "final_symlink":
        _symlink(staged, sentinel)
    elif swap == "final_hard_link":
        _hard_link(staged, sentinel)
    elif swap == "final_fifo":
        _fifo(staged)
    else:
        _symlink(staged, external_dir)
    after_swap: list[dict[str, str]] = []
    vet = report.vet_fixed_destination

    def vet_then_swap(target: str | os.PathLike[str], paths: RepoPaths) -> Path:
        resolved = vet(target, paths)
        if swap == "ancestor_symlink":
            replaced.rename(tmp_path / "replaced_dir")
        os.replace(staged, replaced)
        after_swap.append(_tree(write_paths.write_root))
        return resolved

    monkeypatch.setattr(report, "vet_fixed_destination", vet_then_swap)
    sentinel_before = file_state(sentinel)
    external_before = file_state(external_dir)

    with pytest.raises(OutputPathError) as excinfo:
        report.write_report(results, clean_comparisons, facts, write_paths)

    assert len(after_swap) == 1
    assert excinfo.value.target == report_file
    assert _SWAP_REASONS[swap] in str(excinfo.value)
    assert file_state(sentinel) == sentinel_before
    assert file_state(external_dir) == external_before == ["sentinel.csv"]
    assert _tree(write_paths.write_root) == after_swap[0]


#: The cases in which the ``path`` fallback opens a file it cannot show to be the report path.
_UNBOUND_OPENS = ("ancestor_swapped_during_open", "unidentified_handle")


def _swap_ancestor_during_open(
    patched: pytest.MonkeyPatch, target: Path, ancestor: Path, staged: Path, parked: Path
) -> list[str]:
    """Swap the directory ``ancestor`` for the symlink ``staged`` while ``target`` is opened.

    Through ``patched``, ``wpct.paths`` gets a copy of ``os`` whose ``open`` and ``lstat`` schedule
    a concurrent process against that module's own calls, leaving path resolution elsewhere as it
    is: just before ``target`` is opened by path, ``ancestor`` moves to ``parked`` and ``staged``
    takes its place; right after the open the real directory is back, for a later check of the
    path to find, and the next ``os.lstat`` of ``ancestor`` swaps the symlink in again, so a last
    lookup of ``target`` would reach the file the symlink leads to. Returns the swaps made, in
    order.
    """
    opened_at, watched = target.resolve(), ancestor.resolve()
    real_open, real_lstat = os.open, os.lstat
    swaps: list[str] = []

    def to_symlink() -> None:
        ancestor.rename(parked)
        staged.rename(ancestor)

    def to_directory() -> None:
        ancestor.rename(staged)
        parked.rename(ancestor)

    def open_during_swap(
        path: str | os.PathLike[str], flags: int, mode: int = 0o777, *, dir_fd: int | None = None
    ) -> int:
        if dir_fd is None and not swaps and Path(path) == opened_at:
            to_symlink()
            swaps.append("open")
            try:
                return real_open(path, flags, mode)
            finally:
                to_directory()
        return real_open(path, flags, mode, dir_fd=dir_fd)

    def lstat_then_relink(
        path: str | os.PathLike[str], *, dir_fd: int | None = None
    ) -> os.stat_result:
        info = real_lstat(path, dir_fd=dir_fd)
        if swaps == ["open"] and Path(path) == watched:
            to_symlink()
            swaps.append("lstat")
        return info

    scheduled_os = SimpleNamespace(**vars(os))
    scheduled_os.open = open_during_swap
    scheduled_os.lstat = lstat_then_relink
    patched.setattr(wpct_paths, "os", scheduled_os)
    return swaps


@pytest.mark.parametrize("case", _UNBOUND_OPENS)
def test_env03_path_fallback_refuses_unbound_report_open(
    case: str,
    monkeypatch: pytest.MonkeyPatch,
    write_paths: RepoPaths,
    results: Mapping[str, object],
    clean_comparisons: _Comparisons,
    tmp_path: Path,
    file_state: _FileState,
) -> None:
    """The ``path`` fallback truncates only a report it shows to be the report path (AAP 0.8).

    Outputs go only to fixed locations under ``python/wpct/`` (ENV-03), and no write may follow a
    symlink below the write root, even one swapped in and out while the file is opened (CWE-367).
    After a first clean write of the report with the ``path`` fallback, a directory outside the
    write root holds a stand-in for a SAS-export CSV named ``equivalence_report.md``, and the
    second call meets one case:

    - ``ancestor_swapped_during_open``: ``python/wpct/reports`` is a symlink to that directory only
      while the report is opened; the real directory is back for a later check of the path, and
      the symlink again for a last lookup of the file, so a fallback that checked paths alone
      would truncate the stand-in.
    - ``unidentified_handle``: the platform reports no path for the opened file
      (``wpct.paths._opened_path`` returns ``None``).

    ``write_report`` must raise ``OutputPathError`` for the report path with the reason, and close
    the descriptor it opened. The report of the first write and the stand-in keep their bytes and
    ``st_mtime_ns``, the external directory its listing, and the write root its tree.
    """
    _use_strategy("path", monkeypatch)
    report_file = write_paths.report_file
    facts = provenance.collect(write_paths)
    written = report.write_report(results, clean_comparisons, facts, write_paths)
    external_dir = tmp_path / "external_dir"
    external_dir.mkdir()
    standin = external_dir / report_file.name
    standin.write_bytes(_SAS_EXPORT_STANDIN)
    staged = tmp_path / "staged_link"
    _symlink(staged, external_dir)
    report_before = file_state(written)
    standin_before = file_state(standin)
    external_before = file_state(external_dir)
    tree = _tree(write_paths.write_root)
    locate = wpct_paths._opened_path
    located: list[int] = []

    def locate_recorded(descriptor: int) -> str | None:
        located.append(descriptor)
        return None if case == "unidentified_handle" else locate(descriptor)

    swaps: list[str] = []
    with monkeypatch.context() as patched:
        patched.setattr(wpct_paths, "_opened_path", locate_recorded)
        if case == "ancestor_swapped_during_open":
            swaps = _swap_ancestor_during_open(
                patched, report_file, report_file.parent, staged, tmp_path / "parked_dir"
            )
        with pytest.raises(OutputPathError) as excinfo:
            report.write_report(results, clean_comparisons, facts, write_paths)

    error = excinfo.value
    destination = report_file.resolve()
    assert error.target == report_file
    assert error.resolved == destination == written
    if case == "ancestor_swapped_during_open":
        assert swaps[:1] == ["open"]
        reason = f"{destination} was opened at {standin.resolve()}, not at itself"
    else:
        reason = f"{destination}: the platform reports no path for the opened file"
    assert f"the fixed destination {reason}" in str(error)
    assert len(located) == 1
    with pytest.raises(OSError):
        os.fstat(located[0])
    assert file_state(written) == report_before
    assert file_state(standin) == standin_before
    assert file_state(external_dir) == external_before
    assert _tree(write_paths.write_root) == tree


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
    value to a path, so no option changes a program's ``%let`` settings, which stay its driver's
    defaults (S02-TRN-01, S07-TRN-01). ``build_paths`` is ``RepoPaths.checkout()``, so the ADVS
    files are read from the checkout, not from a URL or a UNC share (ENV-02, ENV-04), and its
    write root is the checkout and no option can move it.
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
def test_env03_cli_rejects_other_arguments(
    argv: list[str], capsys: pytest.CaptureFixture[str]
) -> None:
    """Any other option, an option prefix, an empty export directory or a missing or unknown
    subcommand is an ``argparse`` error with exit code 2, raised before anything runs.

    The refused ``--output``, ``--report``, ``--write-root`` and ``--outputs-dir`` would each set a
    destination, as the SAS ``outputs_folder`` setting does; the port has no counterpart of that
    setting (ENV-03, ENV-04).
    """
    with pytest.raises(SystemExit) as excinfo:
        cli.build_parser().parse_args(argv)

    assert excinfo.value.code == 2
    assert "error:" in capsys.readouterr().err


@pytest.mark.parametrize(
    ("host_handler", "root_level", "expected_err"),
    [
        pytest.param(
            None,
            logging.WARNING,
            "WARNING: wpct.derivation: (DERIVE_LASTMINMAX_MEASURES) Omitting obs without a "
            "post-baseline measure, such as USUBJID='01-701-1015'\n",
            id="unconfigured_root",
        ),
        pytest.param(logging.NullHandler(), logging.DEBUG, "", id="host_configured_root"),
    ],
)
def test_lmm_flt_02_cli_logging_configuration(
    host_handler: logging.Handler | None,
    root_level: int,
    expected_err: str,
    capsys: pytest.CaptureFixture[str],
) -> None:
    """``main``'s logging set-up writes warnings to stderr as ``WARNING: <logger>: <message>``.

    It acts only on a root logger without handlers, as in a fresh ``python -m wpct``, and then
    passes WARNING records but not INFO ones. The warning logged is a synthetic one in the form of
    the derivation's omission warnings (LMM-FLT-02), which SAS writes to its log as ``WARNING:``
    lines. A root logger the host has configured keeps its handlers and level, and nothing reaches
    stderr. The set-up is called alone, because ``main`` would write the checkout's outputs and
    report; pytest's own root handlers are set aside for the call and restored afterwards.
    """
    root = logging.getLogger()
    saved_handlers, saved_level = root.handlers[:], root.level
    host_handlers = [] if host_handler is None else [host_handler]
    root.handlers = host_handlers[:]
    root.setLevel(root_level)
    try:
        cli._configure_logging()
        handlers, level = root.handlers[:], root.level
        logger = logging.getLogger("wpct.derivation")
        logger.info("not a warning")
        logger.warning(
            "(DERIVE_LASTMINMAX_MEASURES) Omitting obs without a post-baseline measure, such as %s",
            "USUBJID='01-701-1015'",
        )
    finally:
        for handler in root.handlers:
            if handler not in host_handlers:
                handler.close()
        root.handlers = saved_handlers
        root.setLevel(saved_level)

    assert capsys.readouterr().err == expected_err
    assert level == root_level
    if host_handler is None:
        assert len(handlers) == 1 and isinstance(handlers[0], logging.StreamHandler)
    else:
        assert handlers == host_handlers
