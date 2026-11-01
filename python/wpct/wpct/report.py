"""Equivalence report of the WPCT Python port: the Markdown renderer and its guarded writer.

:func:`render_report` turns the program runs, their comparison groups (``wpct.compare``), the
provenance facts (``wpct.provenance``) and the tolerances the comparisons applied (loaded from
``python/wpct/tolerances.json``) into the text of ``python/wpct/reports/equivalence_report.md``
(Agent Action Plan 0.7.5). The sections follow :data:`SECTION_TITLES`: Provenance, with the
tolerance table before any result; Summary; one section per program, WPCT-F.07.02 then
WPCT-F.07.07; Not compared; Source integrity.
:func:`write_report` writes that text to the single fixed report destination under the write
root, once the write-destination guard has accepted it (Agent Action Plan 0.8). The report is
committed as produced and never edited by hand.

The text is deterministic for a given checkout and environment. It has no timestamp and no
absolute path, rows keep the order ``wpct.compare`` gave them, and every count comes from
:func:`wpct.compare.count_statuses`, which keeps PASS, FAIL and NOT COMPARED apart, so a NOT
COMPARED row is never counted as PASS. The tolerance table is rendered from the ``Tolerances``
snapshot the comparison groups carry, the limits that produced their verdicts, and its field lists
from ``wpct.schemas``; this module states no tolerance of its own and never reads the tolerance
file. The report claims no equivalence for a NOT COMPARED quantity, and where only rounded SAS
listings exist it says so in the wording :data:`ROUNDED_NOTE_TEMPLATE` fixes.
"""

from __future__ import annotations

import math
import os
import re
from collections.abc import Iterable, Mapping, Sequence
from pathlib import Path

from wpct.compare import (
    FAIL,
    GROUP_LISTING_MERGED_PVAL,
    GROUP_LISTING_PARAMETER_ESTIMATES,
    GROUP_NOT_COMPARED,
    NOT_COMPARED,
    PASS,
    STATUSES,
    ComparisonGroup,
    ComparisonRow,
    Tolerances,
    compared_fields,
    count_statuses,
)
from wpct.errors import OutputPathError
from wpct.paths import RepoPaths, check_write_destination
from wpct.programs import ModelRecord, ProgramResult
from wpct.provenance import (
    STATUS_CHANGED,
    STATUS_UNCHANGED,
    UNAVAILABLE,
    IntegrityRow,
    Provenance,
)
from wpct.schemas import PROGRAMS

__all__ = [
    "REPORT_TITLE",
    "ROUNDED_NOTE_TEMPLATE",
    "SECTION_TITLES",
    "render_report",
    "write_report",
]

#: The level-1 heading of the report.
REPORT_TITLE = "WPCT Python port — equivalence report"

#: The level-2 section headings, in report order (Agent Action Plan 0.7.5).
SECTION_TITLES: tuple[str, ...] = (
    "Provenance",
    "Summary",
    *PROGRAMS,
    "Not compared",
    "Source integrity",
)

#: The wording every rounded-reference group carries (Agent Action Plan 0.7.5).
ROUNDED_NOTE_TEMPLATE = (
    "Rounded reference only: agreement is shown at the precision displayed in `{file}` "
    "(`{d}` decimals; `<.0001` as a bound). Equivalence within the full-precision tolerance "
    "`{rule}` is not established for these quantities."
)

#: Displayed decimals of the listing values each rounded group compares
#: [whitepapers/WPCT/outputs_sas/WPCT-F.07.02.lst:8-14].
_ROUNDED_DECIMALS: Mapping[str, str] = {
    GROUP_LISTING_PARAMETER_ESTIMATES: "8 (Estimate, StdErr), 2 (t Value), 4 (Pr > |t|)",
    GROUP_LISTING_MERGED_PVAL: "4",
}

_ROW_COLUMNS = (
    "Quantity",
    "Stratum",
    "SAS value",
    "Python value",
    "Difference",
    "Tolerance",
    "Result",
)
_REASON_COLUMN = "Reason"

#: Keys of a tolerance class that have their own table column; every other key is a setting.
_TOLERANCE_COLUMN_KEYS = ("label", "rule", "limit", "applies_to")

_NONE = "—"
_MISSING = "."

# Control characters: C0 (tab included), DEL and C1. Each is shown as "<XX>", its two-digit
# uppercase hex code, the notation wpct.compare uses for the 'A0'x of the pooled identifier (D-16),
# so no terminal escape sequence or invisible byte reaches the report. Every code starts with a
# digit, so its "<" opens no HTML and needs no escape.
_CONTROL = re.compile(r"[\x00-\x1f\x7f-\x9f]")

# What would make literal text active Markdown, encoded in a single pass, so a backslash already
# in the text is itself escaped and cannot cancel an escape added here.
# A run of "@" becomes one inline code span. cmark-gfm, GitHub's renderer, merges adjacent text
# before it looks for e-mail addresses, so neither a backslash escape nor "&#64;" stops it from
# linking a bare, "mailto:" or "xmpp:" address, or from turning "<local@domain>" into an
# autolink that also drops the angle brackets. A code span splits the text around it, so no
# address, autolink or GitHub @mention can form across it, and it shows the "@" literally, in
# monospace. The whole run is one span: a span per "@" would put two backticks side by side and
# open a longer span. No e-mail autolink therefore remains. Every other match is backslash-escaped:
# - always "\", "`", "*", "[", "]", "~", "|", "&" and "$": backslash escapes, code spans,
#   emphasis, links and images ("!" and parentheses are inert once the brackets are escaped),
#   strikethrough, table cell separators, entity references and GitHub math;
# - "_" unless it sits between two alphanumerics, where CommonMark's flanking rules never let it
#   open or close emphasis, so names such as CSS_STATS and trtp_short stay as written;
# - "<" followed by a letter, "/", "!" or "?", which would open raw HTML or an autolink and hide
#   the text, as for the pooled identifier that wpct.compare renders as "<A0>Pooled" (D-16);
#   "<.0001" and the "<XX>" control codes stay as written;
# - ":" before "//" and "." after "www", which GFM would turn into URL and www autolinks.
_MARKDOWN_ACTIVE = re.compile(
    r"@+|[\\`*\[\]~|&$]|<(?=[A-Za-z/!?])|(?<![^\W_])_|_(?![^\W_])|:(?=//)|(?<=www)\.",
    re.IGNORECASE,
)

# What an inline code span cannot hold literally: see _code.
_CODE_UNSAFE = re.compile(r"[`\\|\x00-\x1f\x7f-\x9f]")


# --- Text and number formatting -------------------------------------------------------------------


def _format_number(value: object) -> str:
    """Render a number deterministically: integers as written, other floats in shortest round-trip
    form (``repr``), and ``None`` or NaN as the SAS missing value ``.``."""
    if value is None:
        return _MISSING
    if isinstance(value, bool):
        return "true" if value else "false"
    if isinstance(value, int):
        return str(value)
    if isinstance(value, float):
        if math.isnan(value):
            return _MISSING
        if value.is_integer() and abs(value) < 1e16:
            return str(int(value))
        return repr(value)
    return str(value)


def _format_setting(value: object) -> str:
    """Render a tolerance-table value as readable report text rather than its JSON spelling.

    A string is shown as written, without quotes. A mapping becomes ``key = value`` pairs joined
    by ``; `` and a list or tuple its items joined by single spaces, each value and item rendered
    the same way. Any other value is rendered by :func:`_format_number`: booleans as ``true`` or
    ``false``, integers and integral floats below ``1e16`` as integers, other floats in shortest
    round-trip form, and ``None`` or NaN as the SAS missing value ``.`` rather than JSON ``null``.
    """
    if isinstance(value, str):
        return value
    if isinstance(value, Mapping):
        return "; ".join(f"{key} = {_format_setting(item)}" for key, item in value.items())
    if isinstance(value, (list, tuple)):
        return " ".join(_format_setting(item) for item in value)
    return _format_number(value)


def _flatten(text: object) -> str:
    return " ".join(str(text).splitlines())


def _cell(text: object) -> str:
    """Encode ``text`` as literal Markdown text for a table cell, a heading or a paragraph.

    Line breaks become single spaces, each control character becomes its ``<XX>`` code
    (:data:`_CONTROL`), and the text that would make it active Markdown is encoded in one pass
    (:data:`_MARKDOWN_ACTIVE`): each run of ``@`` becomes one inline code span, because GFM
    links an e-mail address however its ``@`` is escaped and a code span splits the text so no
    address can form, and every other match is backslash-escaped. The text therefore renders as
    written, with each ``@`` in monospace, stays in its table cell, and opens no raw HTML,
    emphasis, image or link, an e-mail autolink and an @mention included.
    """
    visible = _CONTROL.sub(lambda match: f"<{ord(match.group()):02X}>", _flatten(text))
    return _MARKDOWN_ACTIVE.sub(
        lambda match: f"`{match.group()}`" if match.group()[0] == "@" else "\\" + match.group(),
        visible,
    )


def _code(text: object) -> str:
    """Render ``text``, flattened to one line, as an inline code span, or by :func:`_cell`.

    A code span shows its content literally and processes no escape, so a value it cannot hold as
    written goes to :func:`_cell` instead: an empty value, or one with a backtick (it would end the
    span), a backslash (before ``|``, GFM table parsers split it differently), a ``|`` (it would
    split a table cell, and its escape would show outside a table) or a control character (it is
    shown as its ``<XX>`` code). Any other value is wrapped in single backticks unchanged.
    """
    value = _flatten(text)
    if not value or _CODE_UNSAFE.search(value):
        return _cell(value)
    return f"`{value}`"


def _table(header: Sequence[str], rows: Iterable[Sequence[str]]) -> list[str]:
    """A Markdown table; header and cells must already be escaped."""
    lines = [
        "| " + " | ".join(header) + " |",
        "|" + "|".join("---" for _ in header) + "|",
    ]
    lines.extend("| " + " | ".join(row) + " |" for row in rows)
    return lines


def _status_counts(counts: Mapping[str, int]) -> str:
    return ", ".join(f"{status} {counts[status]}" for status in STATUSES)


def _repo_relative(path: Path, paths: RepoPaths) -> str:
    """``path`` relative to the checkout with forward slashes: the report holds no absolute path."""
    return path.relative_to(paths.repo_root).as_posix()


# --- Input checks ---------------------------------------------------------------------------------


def _checked_groups(
    results: Mapping[str, ProgramResult],
    comparisons: Mapping[str, Sequence[ComparisonGroup]],
) -> dict[str, tuple[ComparisonGroup, ...]]:
    """Validate the program runs and their comparison groups; return each program's groups.

    Both mappings must hold exactly the programs of ``wpct.schemas.PROGRAMS``, each result must
    belong to its key and each group to its program, so no program section can be left out or
    filled with another program's rows. Each program's groups are returned as a tuple.
    """
    for name, mapping in (("results", results), ("comparisons", comparisons)):
        if not isinstance(mapping, Mapping):
            raise TypeError(
                f"{name} must be a mapping keyed by program, got {type(mapping).__name__}"
            )
        missing = [program for program in PROGRAMS if program not in mapping]
        unknown = sorted(str(key) for key in mapping if key not in PROGRAMS)
        if missing or unknown:
            raise ValueError(
                f"{name} must hold exactly the programs {', '.join(PROGRAMS)}; "
                f"missing: {', '.join(missing) or 'none'}; unknown: {', '.join(unknown) or 'none'}"
            )
    groups_by_program: dict[str, tuple[ComparisonGroup, ...]] = {}
    for program in PROGRAMS:
        result = results[program]
        if not isinstance(result, ProgramResult):
            raise TypeError(f"results[{program!r}] must be a ProgramResult, got {result!r}")
        if result.program_id != program:
            raise ValueError(f"results[{program!r}] is the run of {result.program_id}")
        groups = comparisons[program]
        if isinstance(groups, (str, ComparisonGroup)):
            raise TypeError(f"comparisons[{program!r}] must be a sequence of ComparisonGroup")
        groups = tuple(groups)
        for position, group in enumerate(groups):
            if not isinstance(group, ComparisonGroup):
                raise TypeError(
                    f"comparisons[{program!r}][{position}] must be a ComparisonGroup, got {group!r}"
                )
            if group.program != program:
                raise ValueError(
                    f"comparisons[{program!r}][{position}] ({group.key}) belongs to {group.program}"
                )
        groups_by_program[program] = groups
    return groups_by_program


def _checked_inputs(
    results: Mapping[str, ProgramResult],
    comparisons: Mapping[str, Sequence[ComparisonGroup]],
    provenance: Provenance,
    tolerances: Tolerances,
) -> dict[str, tuple[ComparisonGroup, ...]]:
    """Validate the inputs of :func:`render_report`; return each program's groups as a tuple.

    Besides the checks of :func:`_checked_groups`, every group that carries the tolerances its
    rows were judged under must carry ``tolerances`` itself, so the table the report prints is the
    one that produced the verdicts.
    """
    if not isinstance(provenance, Provenance):
        raise TypeError(f"provenance must be a Provenance, got {type(provenance).__name__}")
    if not isinstance(tolerances, Tolerances):
        raise TypeError(f"tolerances must be a Tolerances, got {type(tolerances).__name__}")
    groups_by_program = _checked_groups(results, comparisons)
    for program in PROGRAMS:
        for position, group in enumerate(groups_by_program[program]):
            if group.tolerances is not None and group.tolerances != tolerances:
                raise ValueError(
                    f"comparisons[{program!r}][{position}] ({group.key}) was judged under "
                    "tolerances other than those the report would print"
                )
    return groups_by_program


def _applied_tolerances(
    groups_by_program: Mapping[str, Sequence[ComparisonGroup]],
) -> Tolerances:
    """The one tolerance snapshot the comparison groups were judged under.

    ``wpct.compare`` stamps it on every group that applies a tolerance. Groups that carry none, or
    that carry snapshots with different values, have no single table that matches every verdict.
    """
    snapshots: list[Tolerances] = []
    for program in PROGRAMS:
        for group in groups_by_program[program]:
            if group.tolerances is not None and group.tolerances not in snapshots:
                snapshots.append(group.tolerances)
    if not snapshots:
        raise ValueError(
            "no comparison group carries the tolerances it was judged under, so the tolerance "
            "table cannot be rendered"
        )
    if len(snapshots) > 1:
        raise ValueError(
            f"the comparison groups were judged under {len(snapshots)} different tolerance "
            "snapshots; one report prints a single tolerance table"
        )
    return snapshots[0]


# --- Provenance -----------------------------------------------------------------------------------


def _integrity_statement(provenance: Provenance) -> str:
    """What the Source integrity section shows, including when nothing was verified."""
    total = len(provenance.integrity)
    if not provenance.git_available:
        return "git is unavailable, so integrity was not verified."
    if not provenance.integrity_verified:
        return (
            f"the upstream commit {provenance.upstream_commit} is unavailable in this checkout, "
            "so integrity was not verified."
        )
    changed = sum(1 for row in provenance.integrity if row.status != STATUS_UNCHANGED)
    if changed:
        return (
            f"{changed} of {total} read-only inputs differ from their blobs at the upstream commit "
            "(see Source integrity)."
        )
    return f"all {total} read-only inputs match their blobs at the upstream commit."


def _tolerance_lines(tolerances: Tolerances, paths: RepoPaths) -> list[str]:
    """The tolerance table, rendered from the tolerances the comparisons applied and the field
    lists of ``wpct.schemas``, so the report restates neither a limit nor a field list."""
    raw = tolerances.raw
    fields = compared_fields()
    schemas_file = paths.project_dir / "wpct" / "schemas.py"
    lines = [
        "### Tolerances",
        "",
        (
            f"Rendered from {_code(_repo_relative(paths.tolerances_file, paths))} "
            f"(schema version {_format_setting(raw['schema_version'])}), the single tolerance "
            "source; every comparison below applies these values. The Fields column lists the "
            "export variables each class compares, as "
            f"{_code(_repo_relative(schemas_file, paths))} defines them."
        ),
        "",
    ]
    if raw.get("description"):
        lines.extend([f"File description: {_cell(raw['description'])}", ""])
    rows = []
    for class_key in tolerances.classes:
        entry = tolerances.entry(class_key)
        settings = "; ".join(
            f"{name} = {_format_setting(value)}"
            for name, value in entry.items()
            if name not in _TOLERANCE_COLUMN_KEYS
        )
        rows.append(
            (
                _code(class_key),
                _cell(entry["label"]),
                _cell(entry["rule"]),
                _cell(_format_setting(entry["limit"])) if "limit" in entry else _NONE,
                _cell(settings) if settings else _NONE,
                _cell(_format_setting(fields[class_key])) if fields[class_key] else _NONE,
                _cell(entry["applies_to"]),
            )
        )
    lines.extend(
        _table(
            ("Class", "Label", "Rule", "Limit", "Settings", "Fields", "Applies to"),
            rows,
        )
    )
    return lines


def _provenance_lines(
    provenance: Provenance, tolerances: Tolerances, paths: RepoPaths
) -> list[str]:
    head = provenance.head_commit
    lines = [
        "## Provenance",
        "",
        (
            f"- Upstream commit, the commit the port was built against: "
            f"{_code(provenance.upstream_commit)}"
        ),
        (
            f"- Checkout `HEAD` when the report was generated (`git rev-parse HEAD`): "
            f"{UNAVAILABLE if head == UNAVAILABLE else _code(head)}"
        ),
        f"- Python: {_code(provenance.python_version)}",
        f"- Source integrity: {_integrity_statement(provenance)}",
        "",
        "### Library versions",
        "",
    ]
    lines.extend(
        _table(
            ("Package", "Version"),
            (
                (_cell(name), _cell(version))
                for name, version in provenance.library_versions.items()
            ),
        )
    )
    lines.append("")
    lines.extend(_tolerance_lines(tolerances, paths))
    spec = _repo_relative(paths.spec_file, paths)
    lines.extend(
        [
            "",
            "### Reference provenance",
            "",
            (
                f"- The SAS listings and logs used as references come from commit "
                f"{_code(provenance.reference_commit)} ({provenance.reference_commit_date}) and "
                f"were produced by {provenance.reference_sas_version}."
            ),
            f"- {_flatten(provenance.reference_note)}",
            f"- The Sources section of {_code(spec)} details these later edits.",
            (
                "- The box-plot PDFs hold their statistics only as raster images and are not used "
                "as references."
            ),
        ]
    )
    return lines


# --- Summary --------------------------------------------------------------------------------------


def _summary_lines(
    results: Mapping[str, ProgramResult], groups_by_program: Mapping[str, Sequence[ComparisonGroup]]
) -> list[str]:
    per_program = {program: count_statuses(groups_by_program[program]) for program in PROGRAMS}
    overall = count_statuses(group for program in PROGRAMS for group in groups_by_program[program])
    rows = [
        (
            _cell(program),
            *(str(per_program[program][status]) for status in STATUSES),
            "yes" if results[program].halted else "no",
        )
        for program in PROGRAMS
    ]
    rows.append(("Overall", *(str(overall[status]) for status in STATUSES), _NONE))
    lines = ["## Summary", ""]
    lines.extend(_table(("Program", *STATUSES, "Halted"), rows))
    lines.extend(
        [
            "",
            (
                f"{NOT_COMPARED} rows are counted on their own, never as {PASS}; no equivalence is "
                "claimed for them (see Not compared)."
            ),
            "",
        ]
    )
    halted = [program for program in PROGRAMS if results[program].halted]
    for program in halted:
        lines.extend([f"{program} halted: {_cell(results[program].halt_message)}", ""])
    if overall[FAIL] or halted:
        problems = []
        if overall[FAIL]:
            noun = "comparison" if overall[FAIL] == 1 else "comparisons"
            problems.append(f"{overall[FAIL]} {noun} failed")
        if halted:
            problems.append(f"{', '.join(halted)} halted")
        lines.append(f"Result: {FAIL}: {'; '.join(problems)}.")
    else:
        lines.append(
            f"Result: no comparison failed and no program halted ({overall[PASS]} {PASS})."
        )
    return lines


# --- Program sections -----------------------------------------------------------------------------


def _model_stratum(record: ModelRecord) -> str:
    stratum = f"PARAMCD={record.PARAMCD} ATPTN={_format_number(record.ATPTN)}"
    if record.STUDYNUM is not None:
        stratum += f" studynum={record.STUDYNUM}"
    return stratum


def _model_lines(result: ProgramResult) -> list[str]:
    """The GLM fit status of every model, or why the run has none (Agent Action Plan 0.6.4)."""
    lines = ["### Models", ""]
    if result.glm_skipped:
        lines.append("GLM: not produced: GLM skipped by setting.")
    elif result.models:
        lines.extend(
            _table(
                ("Model", "Stratum", "NOBS", "DFE", "Status"),
                (
                    (
                        _cell(record.MODEL),
                        _cell(_model_stratum(record)),
                        _format_number(record.NOBS),
                        _format_number(record.DFE),
                        _cell(record.STATUS),
                    )
                    for record in result.models
                ),
            )
        )
    elif result.halted:
        lines.append("No model was fitted: the run halted before the GLM step.")
    else:
        lines.append("No model was fitted: the analysis population holds no stratum.")
    return lines


def _rounded_rule(group_key: str, tolerances: Tolerances) -> str:
    """The full-precision tolerance a rounded group cannot establish, from ``tolerances.json``."""
    p_values = f"absolute ≤ {_format_number(tolerances.limit('p_values'))}"
    if group_key == GROUP_LISTING_PARAMETER_ESTIMATES:
        glm = f"relative ≤ {_format_number(tolerances.limit('glm_estimates'))}"
        return f"{glm} for Estimate, StdErr and t Value; {p_values} for Pr > |t|"
    return p_values


def _rounded_note(group: ComparisonGroup, tolerances: Tolerances, paths: RepoPaths) -> str:
    """The rounded-reference wording of a listing group, with its listing, decimals and rule."""
    if group.key not in _ROUNDED_DECIMALS:
        raise ValueError(
            f"no rounded-reference precision is defined for comparison group {group.key!r}"
        )
    return ROUNDED_NOTE_TEMPLATE.format(
        file=_repo_relative(paths.listing_file(group.program), paths),
        d=_ROUNDED_DECIMALS[group.key],
        rule=_rounded_rule(group.key, tolerances),
    )


def _row_cells(row: ComparisonRow, with_reason: bool) -> tuple[str, ...]:
    cells = (
        row.quantity,
        row.stratum,
        row.sas_value,
        row.python_value,
        row.difference,
        row.tolerance,
        row.status,
    )
    if with_reason:
        cells += (row.reason,)
    return tuple(_cell(text) for text in cells)


def _group_lines(group: ComparisonGroup, tolerances: Tolerances, paths: RepoPaths) -> list[str]:
    heading = group.title
    note = group.heading_note
    if group.key == GROUP_LISTING_MERGED_PVAL and note:
        # The heading itself says that these are the CSS_STATS cells the merge fills, so the table
        # cannot be read as a second check of the model rows.
        heading, note = f"{heading}: {note}", None
    lines = [f"### {_cell(heading)}", ""]
    if note:
        lines.extend([_cell(note), ""])
    if group.rounded:
        lines.extend([_rounded_note(group, tolerances, paths), ""])
    lines.append(
        f"{PASS} {group.n_pass}, {FAIL} {group.n_fail}, {NOT_COMPARED} {group.n_not_compared}."
    )
    if not group.rows:
        lines.extend(["", "No rows."])
        return lines
    with_reason = any(row.reason for row in group.rows)
    header = _ROW_COLUMNS + ((_REASON_COLUMN,) if with_reason else ())
    lines.append("")
    lines.extend(_table(header, (_row_cells(row, with_reason) for row in group.rows)))
    return lines


def _program_lines(
    result: ProgramResult,
    groups: Sequence[ComparisonGroup],
    tolerances: Tolerances,
    paths: RepoPaths,
) -> list[str]:
    program = result.program_id
    lines = [
        f"## {program}",
        "",
        (
            f"{_status_counts(count_statuses(groups))}; the {NOT_COMPARED} quantities are listed "
            "under Not compared."
        ),
        "",
    ]
    if result.halted:
        lines.append(f"Halted: {_cell(result.halt_message)}")
    else:
        lines.append("Halt: none; every halting check passed.")
    if result.omission_counts:
        counts = ", ".join(f"{name} {count}" for name, count in result.omission_counts.items())
        lines.extend(["", f"Derivation omission counts: {_cell(counts)}."])
    lines.append("")
    lines.extend(_model_lines(result))
    for group in groups:
        if group.key == GROUP_NOT_COMPARED:
            continue
        lines.append("")
        lines.extend(_group_lines(group, tolerances, paths))
    return lines


# --- Not compared and source integrity ------------------------------------------------------------


def _not_compared_lines(groups_by_program: Mapping[str, Sequence[ComparisonGroup]]) -> list[str]:
    """Every NOT COMPARED row of every group, with its reason, in report order."""
    rows = [
        (
            _cell(program),
            _NONE if group.key == GROUP_NOT_COMPARED else _cell(group.title),
            _cell(row.quantity),
            _cell(row.stratum),
            _cell(row.reason) if row.reason else _NONE,
        )
        for program in PROGRAMS
        for group in groups_by_program[program]
        for row in group.rows
        if row.status == NOT_COMPARED
    ]
    lines = [
        "## Not compared",
        "",
        (
            "No equivalence is claimed for any quantity listed here: each has no SAS reference "
            "value to compare with, or the run did not produce it. Each row gives the reason."
        ),
        "",
    ]
    if not rows:
        lines.append("Every quantity of both programs was compared.")
        return lines
    lines.extend(_table(("Program", "Comparison", "Quantity", "Stratum", "Reason"), rows))
    return lines


def _blob_text(blob: str | None) -> str:
    return UNAVAILABLE if blob is None else _code(blob)


def _integrity_lines(provenance: Provenance) -> list[str]:
    rows: Sequence[IntegrityRow] = provenance.integrity
    lines = [
        "## Source integrity",
        "",
        (
            "Each read-only input's working-tree blob (`git hash-object --no-filters`) against its "
            f"blob at the upstream commit {_code(provenance.upstream_commit)}: "
            f"{_integrity_statement(provenance)}"
        ),
        "",
    ]
    changed = [row.path for row in rows if row.status == STATUS_CHANGED]
    if changed:
        lines.extend([f"Changed inputs: {_cell(', '.join(changed))}.", ""])
    lines.extend(
        _table(
            ("Path", "Blob at upstream commit", "Working-tree blob", "Result"),
            (
                (
                    _code(row.path),
                    _blob_text(row.upstream_blob),
                    _blob_text(row.working_blob),
                    _cell(row.status),
                )
                for row in rows
            ),
        )
    )
    return lines


# --- Introduction ---------------------------------------------------------------------------------


def _intro_lines(paths: RepoPaths) -> list[str]:
    spec = _repo_relative(paths.spec_file, paths)
    # The link is relative to the report's own directory, so it resolves wherever the file is read.
    link = Path(os.path.relpath(paths.spec_file, paths.report_file.parent)).as_posix()
    return [
        f"# {REPORT_TITLE}",
        "",
        (
            f"This report compares the Python port of {' and '.join(PROGRAMS)} under "
            f"`{_repo_relative(paths.project_dir, paths)}/` with the SAS reference results: the "
            f"listings and logs in `{_repo_relative(paths.outputs_sas_dir, paths)}/` and any "
            "user-supplied full-precision SAS exports. It covers the intermediate datasets, the "
            "PROC SUMMARY statistics and the PROC GLM SOLUTION tables; the box-plot graphics are "
            f"not reproduced. The specification is [{spec}]({link}). `python -m wpct reproduce` "
            "generates this file; it is not edited by hand."
        ),
    ]


# --- Public API -----------------------------------------------------------------------------------


def render_report(
    results: Mapping[str, ProgramResult],
    comparisons: Mapping[str, Sequence[ComparisonGroup]],
    provenance: Provenance,
    tolerances: Tolerances,
) -> str:
    """Render the equivalence report as Markdown (Agent Action Plan 0.7.5).

    Args:
        results: The run of each program in ``wpct.schemas.PROGRAMS``, keyed by program; a halted
            run is ``ProgramResult.from_halt``.
        comparisons: ``wpct.compare.compare_program`` of each run, keyed by program.
        provenance: ``wpct.provenance.collect`` of the checkout.
        tolerances: ``wpct.compare.load_tolerances`` of ``python/wpct/tolerances.json``, the
            snapshot the comparisons applied; its table is printed before any result.

    Returns:
        The report text with ``\\n`` line ends and one trailing newline. Equal inputs give equal
        text: it holds no timestamp and no absolute path.

    Raises:
        TypeError, ValueError: An input of the wrong type, a program missing from or unknown to
            either mapping, a result or group filed under another program, a group whose
            ``tolerances`` differ from ``tolerances``, or a rounded group whose displayed precision
            is not defined here.
    """
    groups_by_program = _checked_inputs(results, comparisons, provenance, tolerances)
    # Repository-relative names only; the checkout paths are never opened while rendering.
    paths = RepoPaths.checkout()
    blocks = [
        _intro_lines(paths),
        _provenance_lines(provenance, tolerances, paths),
        _summary_lines(results, groups_by_program),
    ]
    blocks.extend(
        _program_lines(results[program], groups_by_program[program], tolerances, paths)
        for program in PROGRAMS
    )
    blocks.append(_not_compared_lines(groups_by_program))
    blocks.append(_integrity_lines(provenance))
    return "\n\n".join("\n".join(block) for block in blocks) + "\n"


def write_report(
    results: Mapping[str, ProgramResult],
    comparisons: Mapping[str, Sequence[ComparisonGroup]],
    provenance: Provenance,
    paths: RepoPaths,
) -> Path:
    """Render the report and write it to ``paths.report_file``; return the resolved path written.

    The destination is fixed: this function takes none (Agent Action Plan 0.8). The tolerance table
    is the ``Tolerances`` snapshot the comparison groups carry (``ComparisonGroup.tolerances``),
    the limits that produced their verdicts; ``tolerances.json`` is not read again, so an edit to
    it after the comparisons ran cannot relabel them. The text is rendered in memory first. The
    target then passes :func:`~wpct.paths.check_write_destination`, and its resolved form must be
    the report path under the resolved write root, before any directory is created or the file
    opened.

    Raises:
        OSError: The reports directory cannot be created, or the report file cannot be opened or
            written.
        OutputPathError: :func:`~wpct.paths.check_write_destination` refuses the report path
            (it resolves outside the permitted destinations or cannot be resolved), or the path is
            reached through a symlink below the write root: the report path itself or a directory
            between it and the write root, leading anywhere else, inside or outside the checkout.
            Symlinks at or above the write root itself are followed. Nothing is created or
            modified.
        TypeError, ValueError: As :func:`render_report`, ``paths`` is not a ``RepoPaths``, or no
            group carries a tolerance snapshot or the groups carry different ones; nothing is
            written.
    """
    if not isinstance(paths, RepoPaths):
        raise TypeError(f"paths must be a RepoPaths, got {type(paths).__name__}")
    groups_by_program = _checked_groups(results, comparisons)
    tolerances = _applied_tolerances(groups_by_program)
    text = render_report(results, groups_by_program, provenance, tolerances)
    target = paths.report_file
    resolved = check_write_destination(target, paths)
    # The guard also accepts any path strictly below the outputs tree, so a reports directory
    # symlinked into python/wpct/outputs would pass it. The write policy refuses a fixed destination
    # reached through a symlink below the write root, so the resolved path must be the report path
    # re-rooted at the resolved write root. Only symlinks at or above the write root itself are
    # followed.
    expected = paths.write_root.resolve() / target.relative_to(paths.write_root)
    if resolved != expected:
        raise OutputPathError(
            target, resolved, f"the fixed destination {expected} is reached through a symlink"
        )
    resolved.parent.mkdir(parents=True, exist_ok=True)
    with open(resolved, "w", encoding="utf-8", newline="\n") as handle:
        handle.write(text)
    return resolved
