"""Equivalence against the user-supplied full-precision SAS exports (AAP 0.7.4).

There is one case per file of ``schemas.SAS_EXPORT_FILES``: the seven exports whose contract is
``python/wpct/reference/sas_exports/README.md`` (AAP 0.7.3). The run never produces these files and
no SAS value is invented. A file absent from the ``sas_exports_dir`` directory skips its case with
the reason below, so a clean checkout reports seven skips. A file present there is loaded by
``sas_reference.load_sas_exports``, and ``compare.compare_program`` compares it with the default run
under the limits of ``python/wpct/tolerances.json`` (AAP 0.7.1):

- Record-level exports (``css_anadata``, ``advs_min``), per AAP 0.7.2: key-set agreement in both
  directions; on every matched record, the categorical fields exactly and ``AVAL BASE CHG ADT``
  under ``record_numeric``; and the per-stratum record counts exactly.
- ``css_stats``: ``n`` exactly; ``mean std median datamin datamax q1 q3`` under
  ``summary_statistics`` (D-01 quantile definition, D-02 STD divisor, D-03 missing values); and the
  merged ``pval`` under ``p_values``.
- ``parameter_estimates``: ``ESTIMATE STDERR TVALUE`` under ``glm_estimates`` and ``PROBT`` under
  ``p_values``. D-05 to D-09 govern these: the less-than-full-rank design, aliasing, CLASS order and
  ``REF=``, missing values, and zero error degrees of freedom.

A present export passes only when every row of its group is PASS and the group compared everything
its ``schemas.ExportSpec`` declares. A FAIL fails the case, and so does a NOT COMPARED row, which
never stands in for agreement. Coverage is checked against the spec, never against what the rows
happen to hold: one key-set and one record-count row, each with the SAS log's count on both sides,
and no key on one side only or repeated. A record-level export also needs one row per categorical
and ``record_numeric`` field, each over every matched record, and one per-stratum count row for
every ``count_strata`` combination. A statistics or estimates export needs a row for every
compared field of every matched key. So a group of key-set and count rows alone, or one that drops
a field, a key or a stratum, fails. File names, keys and field lists come from ``wpct.schemas``
alone (AAP 0.9.5). The export directory is a read-only input, and nothing here writes into it.

A README sync check, one case per file, needs no export and never skips: the "Expected files"
table of ``python/wpct/reference/sas_exports/README.md`` quotes each file's
``schemas.SAS_EXPORT_SOURCES`` entry verbatim as its SAS source, so the capture points are
described in ``wpct.schemas`` alone.

Synthetic mechanics controls run on a clean checkout, without the optional SAS files. Each one
writes a SAS-format export of the default run's own dataset under ``tmp_path``. The file follows the
README contract: Latin-1, a header of SAS names in lower and mixed case, and round-trip numbers with
``.`` for missing. Parameter estimates are written as ``Parameter Estimate StdErr tValue Probt``
plus the stratum columns, without ``EFFECT`` or ``LEVEL``. Each control then loads the file with
``sas_reference.load_sas_exports`` and compares it with ``compare.compare_program``. These files
echo Python output and are not SAS reference evidence: they show only that the loader and the
comparator work, and no SAS value is invented (AAP 0.7.3). An unedited echo must pass everything and
meet the coverage check. Editing one cell or record, by offsets taken from ``tolerances.json``, or
repeating one record, must fail exactly the rows it targets, and the key-set agreement row must
give exactly the reason expected. The coverage check must reject groups with rows removed. The
session frames are read, never altered: an edit applies to the written text or to a copy.
"""

from __future__ import annotations

import csv
import dataclasses
import itertools
import re
from collections.abc import Callable, Mapping, Sequence
from pathlib import Path
from types import MappingProxyType

import pandas as pd
import pytest

from wpct import compare, sas_reference, schemas
from wpct.paths import RepoPaths
from wpct.programs import ProgramResult

#: How many SAS exports the contract has: "the seven export files" of AAP 0.7.3 and 0.7.4. Only
#: the count is kept here; the file names themselves come from ``wpct.schemas`` alone (AAP 0.9.5).
_SAS_EXPORT_COUNT = 7

#: Each export's SAS file name, as its own ``schemas`` spec gives it.
_FILE_07_02_ANADATA = schemas.export_spec(schemas.PROGRAM_07_02, "css_anadata").sas_export_file
_FILE_07_02_STATS = schemas.export_spec(schemas.PROGRAM_07_02, "css_stats").sas_export_file
_FILE_07_02_ESTIMATES = schemas.export_spec(
    schemas.PROGRAM_07_02, "parameter_estimates"
).sas_export_file
_FILE_07_07_ADVS_MIN = schemas.export_spec(schemas.PROGRAM_07_07, "advs_min").sas_export_file
_FILE_07_07_ANADATA = schemas.export_spec(schemas.PROGRAM_07_07, "css_anadata").sas_export_file
_FILE_07_07_STATS = schemas.export_spec(schemas.PROGRAM_07_07, "css_stats").sas_export_file
_FILE_07_07_ESTIMATES = schemas.export_spec(
    schemas.PROGRAM_07_07, "parameter_estimates"
).sas_export_file

#: Records of each record-level SAS dataset, as its reference log reports them:
#: WORK.CSS_ANADATA of 07.02 [whitepapers/WPCT/outputs_sas/WPCT-F.07.02.log:202], WORK.ADVS_MIN
#: after the post-baseline filter [whitepapers/WPCT/outputs_sas/WPCT-F.07.07.log:229], and the
#: numbered WORK.CSS_ANADATA of 07.07 [whitepapers/WPCT/outputs_sas/WPCT-F.07.07.log:427].
_RECORD_COUNTS: Mapping[str | None, int] = MappingProxyType(
    {
        _FILE_07_02_ANADATA: 3548,
        _FILE_07_07_ADVS_MIN: 498,
        _FILE_07_07_ANADATA: 996,
    }
)

#: Keyed rows of each per-stratum SAS dataset, summed over the strata its reference log reports.
#: 07.02 WORK.CSS_STATS after the merge has 30 rows for DIABP/815 and for SYSBP/816
#: [whitepapers/WPCT/outputs_sas/WPCT-F.07.02.log:1049,1471], and WORK.CSS_PVALUES holds the 5
#: ParameterEstimates rows of each [whitepapers/WPCT/outputs_sas/WPCT-F.07.02.log:992,1414].
#: 07.07 WORK.CSS_STATS has 9 rows for ATPTN 815 and for 817
#: [whitepapers/WPCT/outputs_sas/WPCT-F.07.07.log:1215,1581]. Per timepoint, WORK.CSS_PVAL_STDY
#: holds 10 per-study rows and WORK.CSS_PVAL_POOL 6 pooled rows
#: [whitepapers/WPCT/outputs_sas/WPCT-F.07.07.log:1128,1144,1493,1509].
_KEY_COUNTS: Mapping[str | None, int] = MappingProxyType(
    {
        _FILE_07_02_STATS: 2 * 30,
        _FILE_07_02_ESTIMATES: 2 * 5,
        _FILE_07_07_STATS: 2 * 9,
        _FILE_07_07_ESTIMATES: 2 * (10 + 6),
    }
)

#: The SAS log's count for every export: records of a record-level export, keys of the others.
_EXPORT_COUNTS: Mapping[str | None, int] = MappingProxyType({**_RECORD_COUNTS, **_KEY_COUNTS})

#: Row quantities that ``compare`` writes for the record-key comparison of AAP 0.7.2.
_KEY_ONLY_IN_SAS = "record key only in SAS"
_KEY_ONLY_IN_PYTHON = "record key only in Python"
_KEY_REPEATED_IN_SAS = "repeated record key in SAS"
_KEY_SET_AGREEMENT = "key-set agreement"
_RECORD_COUNT = "record count"
_STRATUM_RECORD_COUNT = "stratum record count"
#: The stratum ``compare`` gives the record-count row, which spans the whole export.
_ALL_STRATA = "all strata"

#: Schema column types that a SAS export holds as numbers; every other column is text.
_NUMERIC_TYPES = (schemas.NUM, schemas.INT, schemas.NULLABLE_INT)
#: A SAS missing numeric value as a synthetic export writes it.
_SAS_MISSING = "."
#: Appended to a text cell by the ``relabel`` edit, so the value differs and stays Latin-1.
_RELABEL_SUFFIX = "#"

#: An assertion message lists at most this many rows, then the remainder's count.
_MAX_LISTED_ROWS = 40

#: The export contract, in the export directory of the checkout; read, never written.
_EXPORT_README = "README.md"
#: The heading of the README section whose first table names the expected files.
_EXPECTED_FILES_HEADING = "### Expected files"
#: The header of that table, in its column order.
_EXPECTED_FILES_COLUMNS = ("File", "SAS source", "Content", "Stratum columns to add", "Record key")
#: One cell of a Markdown table delimiter row, such as ``---`` or ``:---:``.
_DELIMITER_CELL = re.compile(r":?-+:?")
#: A cell holding one code span and nothing else; the group is the span's text.
_CODE_SPAN_CELL = re.compile(r"`([^`]+)`")


def _describe(rows: Sequence[compare.ComparisonRow]) -> str:
    """One line per row, giving its status, quantity, stratum, values, difference and reason."""
    lines = [
        f"  {row.status}: {row.quantity} [{row.stratum}] sas={row.sas_value!r} "
        f"python={row.python_value!r} difference={row.difference!r} "
        f"tolerance={row.tolerance!r} reason={row.reason!r}"
        for row in rows[:_MAX_LISTED_ROWS]
    ]
    if len(rows) > _MAX_LISTED_ROWS:
        lines.append(f"  ... and {len(rows) - _MAX_LISTED_ROWS} more")
    return "\n".join(lines)


def _export_group(
    file_name: str, groups: Sequence[compare.ComparisonGroup]
) -> compare.ComparisonGroup:
    """The single ``export:<file_name>`` group of a program's comparisons.

    A halted run has no export groups, so the assertion message carries any halt.
    """
    key = compare.export_group_key(file_name)
    matching = [group for group in groups if group.key == key]
    halts = [
        row.reason for group in groups if group.key == compare.GROUP_HALT for row in group.rows
    ]
    assert len(matching) == 1, (
        f"expected one comparison group {key!r}, found {len(matching)} among "
        f"{[group.key for group in groups]}; halts: {halts or 'none'}"
    )
    return matching[0]


def _assert_record_keys(
    file_name: str, rows: Sequence[compare.ComparisonRow], expected: int
) -> None:
    """Check that the AAP 0.7.2 key-set comparison ran and both sides hold ``expected`` records.

    Each key present on one side only would be its own FAIL row, in either direction. The
    key-set row counts distinct keys and the record-count row counts records, so ``expected`` on
    both rows and both sides also shows that the keys are unique.
    """
    one_sided = [row for row in rows if row.quantity in (_KEY_ONLY_IN_SAS, _KEY_ONLY_IN_PYTHON)]
    assert not one_sided, (
        f"{file_name}: {len(one_sided)} record keys on one side only:\n{_describe(one_sided)}"
    )
    for quantity in (_KEY_SET_AGREEMENT, _RECORD_COUNT):
        found = [row for row in rows if row.quantity == quantity]
        assert len(found) == 1, f"{file_name}: expected one {quantity!r} row, found {len(found)}"
        row = found[0]
        assert (row.sas_value, row.python_value) == (str(expected), str(expected)), (
            f"{file_name}: {quantity} is SAS {row.sas_value}, Python {row.python_value}; "
            f"the SAS log reports {expected} records"
        )


def _per_key(spec: schemas.ExportSpec) -> bool:
    """True when ``compare`` compares the export key by key, as it does statistics and estimates.

    This is the dispatch ``compare.compare_program`` applies: an export with summary or GLM
    fields is compared per key, every other export record by record.
    """
    return bool(spec.summary_fields or spec.glm_fields)


def _expected_fields(spec: schemas.ExportSpec) -> tuple[str, ...]:
    """The upper-case fields an export's comparison must cover, in spec order.

    These are the categorical, count, summary, GLM, p-value and ``record_numeric`` fields of the
    spec. A per-key comparison matches on the record key, so it does not compare the key fields.
    """
    names = (
        *spec.categorical,
        *spec.count_fields,
        *spec.summary_fields,
        *spec.glm_fields,
        *spec.p_value_fields,
        *spec.record_numeric,
    )
    fields = tuple(dict.fromkeys(schemas.upper_columns(names)))
    if _per_key(spec):
        key = set(schemas.upper_columns(spec.key))
        fields = tuple(field for field in fields if field not in key)
    assert fields, f"{spec.sas_export_file}: the spec declares no field to compare"
    return fields


def _count_value(file_name: str, row: compare.ComparisonRow, side: str, text: str) -> int:
    """The record count a stratum row shows for one side, which must be a non-negative integer."""
    assert text.isdigit(), (
        f"{file_name}: {side} count {text!r} of stratum {row.stratum!r} is not a record count"
    )
    return int(text)


def _assert_strata_covered(
    file_name: str,
    spec: schemas.ExportSpec,
    rows: Sequence[compare.ComparisonRow],
    python_frame: pd.DataFrame,
    expected: int,
) -> int:
    """Check one stratum-count row per ``count_strata`` combination; return the number of strata.

    The combinations are those of the Python dataset. A stratum present only in SAS would add a
    row, so the row count also bounds the SAS side. Both sides' counts must sum to ``expected``.
    """
    if not spec.count_strata:
        assert not rows, f"{file_name}: stratum rows for an export without count strata"
        return 0
    strata = len(python_frame.loc[:, list(spec.count_strata)].drop_duplicates())
    assert strata > 0, f"{file_name}: the Python dataset has no {' × '.join(spec.count_strata)}"
    assert len(rows) == strata, (
        f"{file_name}: {len(rows)} {_STRATUM_RECORD_COUNT!r} rows for {strata} strata of "
        f"{' × '.join(spec.count_strata)}"
    )
    assert len({row.stratum for row in rows}) == strata, (
        f"{file_name}: a stratum is counted more than once:\n{_describe(rows)}"
    )
    names = schemas.upper_columns(spec.count_strata)
    unnamed = [row for row in rows if not all(f"{name}=" in row.stratum for name in names)]
    assert not unnamed, (
        f"{file_name}: stratum rows not naming {' '.join(names)}:\n{_describe(unnamed)}"
    )
    for side, attribute in (("SAS", "sas_value"), ("Python", "python_value")):
        total = sum(_count_value(file_name, row, side, getattr(row, attribute)) for row in rows)
        assert total == expected, (
            f"{file_name}: the {side} stratum counts sum to {total}, not the {expected} records "
            "the SAS log reports"
        )
    return strata


def _assert_export_coverage(
    file_name: str, rows: Sequence[compare.ComparisonRow], python_frame: pd.DataFrame
) -> None:
    """Check that an export group compared every quantity its ``ExportSpec`` declares.

    ``python_frame`` is the run's dataset of the export, which gives the strata a record-level
    export is counted in. Required of every export: exactly one key-set and one record-count row,
    both showing the SAS log's count on both sides; no key on one side only or repeated; and no
    quantity the spec does not declare. A record-level export then needs exactly one row per
    expected field, over every matched record, and the stratum rows of
    :func:`_assert_strata_covered`. A per-key export needs every expected field for every matched
    key, so it has ``2 + keys × fields`` rows in all.
    """
    spec = schemas.sas_export_spec(file_name)
    expected = _EXPORT_COUNTS[file_name]
    fields = _expected_fields(spec)
    _assert_record_keys(file_name, rows, expected)
    allowed = {_KEY_SET_AGREEMENT, _RECORD_COUNT, *fields}
    if not _per_key(spec) and spec.count_strata:
        allowed.add(_STRATUM_RECORD_COUNT)
    unexpected = [row for row in rows if row.quantity not in allowed]
    assert not unexpected, (
        f"{file_name}: {len(unexpected)} rows of quantities the spec does not declare:\n"
        f"{_describe(unexpected)}"
    )
    by_quantity: dict[str, list[compare.ComparisonRow]] = {}
    for row in rows:
        by_quantity.setdefault(row.quantity, []).append(row)
    absent = [field for field in fields if field not in by_quantity]
    assert not absent, f"{file_name}: no comparison of the spec fields {' '.join(absent)}"
    if _per_key(spec):
        keys = {row.stratum for row in by_quantity[fields[0]]}
        for field in fields:
            found = [row.stratum for row in by_quantity[field]]
            assert len(found) == expected and len(set(found)) == expected, (
                f"{file_name}: {field} compared in {len(found)} rows for {len(set(found))} "
                f"distinct keys; the SAS log reports {expected} keys"
            )
            assert set(found) == keys, (
                f"{file_name}: {field} is compared for other keys than {fields[0]}"
            )
        total = 2 + expected * len(fields)
    else:
        for field in fields:
            found = by_quantity[field]
            assert len(found) == 1, f"{file_name}: {len(found)} rows compare {field}, not one"
            assert found[0].stratum == f"{expected} matched records", (
                f"{file_name}: {field} compared over {found[0].stratum!r}, not all {expected} "
                "matched records"
            )
        strata = _assert_strata_covered(
            file_name, spec, by_quantity.get(_STRATUM_RECORD_COUNT, []), python_frame, expected
        )
        total = 2 + len(fields) + strata
    assert len(rows) == total, f"{file_name}: {len(rows)} comparison rows, expected {total}"


def _assert_export_agrees(
    file_name: str, group: compare.ComparisonGroup, python_frame: pd.DataFrame
) -> None:
    """Every row of a full-precision export group is PASS, and the group covers its spec.

    A FAIL fails, and so does a NOT COMPARED row, which is not agreement (AAP 0.7.4).
    """
    rows = group.rows
    assert not group.rounded, f"{file_name}: a full-precision export was compared as rounded"
    assert len(rows) > 0, f"{file_name}: the export produced no comparison rows"
    failed = [row for row in rows if row.status == compare.FAIL]
    assert not failed, (
        f"{file_name}: {len(failed)} of {len(rows)} comparisons FAIL:\n{_describe(failed)}"
    )
    not_passed = [row for row in rows if row.status != compare.PASS]
    assert not not_passed, (
        f"{file_name}: {len(not_passed)} of {len(rows)} quantities of a present export were not "
        f"compared, which is not agreement:\n{_describe(not_passed)}"
    )
    _assert_export_coverage(file_name, rows, python_frame)


@pytest.mark.parametrize("file_name", schemas.SAS_EXPORT_FILES)
def test_d01_02_05_to_09_sas_export_equivalence(
    file_name: str,
    sas_exports_dir: Path,
    sas_exports: Mapping[str, pd.DataFrame | None],
    comparisons: Mapping[str, tuple[compare.ComparisonGroup, ...]],
    results: Mapping[str, ProgramResult],
) -> None:
    """A present SAS export agrees with the default run on every quantity its spec declares.

    The tolerances are those of AAP 0.7.1, read from ``tolerances.json`` by ``compare``. Every
    row must be PASS, and the group must meet :func:`_assert_export_coverage`, with the count the
    SAS log reports. For a record-level export that means the AAP 0.7.2 key sets, every
    categorical and numeric field, and every stratum count. For statistics and estimates it means
    every field of every key, governed by D-01, D-02 and D-05 to D-09. An absent export skips with
    the reason AAP 0.7.4 states.
    """
    if sas_exports[file_name] is None:
        pytest.skip(
            f"SAS reference export {file_name} not found in {sas_exports_dir}; "
            "see python/wpct/reference/sas_exports/README.md"
        )
    spec = schemas.sas_export_spec(file_name)
    group = _export_group(file_name, comparisons[spec.program_id])
    _assert_export_agrees(file_name, group, results[spec.program_id].datasets[spec.name])


def test_s02_trn_03_export_parameter_list_matches_schemas(
    sas_exports: Mapping[str, pd.DataFrame | None],
) -> None:
    """The parameter list is exactly the SAS exports ``schemas.EXPORTS`` declares (AAP 0.9.5).

    They come in program order, then export order within each program, with no name repeated, and
    there are seven of them (AAP 0.7.3). For 07.02 they are the datasets of S02-TRN-03, of
    S02-MOD-01 with the S02-TRN-05 ``pval``, and of S02-MOD-02. For 07.07 they are the LMM-TRN-07
    records after S07-FLT-02, the analysis dataset of S07-TRN-04, S07-TRN-05 and S07-TRN-06, the
    statistics of S07-MOD-01 with the S07-TRN-08 ``pval``, and the estimates of S07-MOD-02 and
    S07-MOD-03. The loader returns an entry for every name, so no case can miss its export. Each
    name is the SAS export of its own program's spec. Every export has a count from the SAS log.
    The record counts cover exactly the exports that have neither statistics nor GLM fields, which
    ``compare`` treats as record-level: the S02-TRN-03 and LMM-TRN-07 datasets and the S07-TRN-04,
    S07-TRN-05 and S07-TRN-06 analysis dataset. The key counts cover the rest, the exports of
    S02-MOD-01, S02-MOD-02, S07-MOD-01, S07-MOD-02 and S07-MOD-03.
    """
    assert tuple(schemas.EXPORTS) == schemas.PROGRAMS
    declared = tuple(
        spec.sas_export_file
        for program_id in schemas.PROGRAMS
        for spec in schemas.EXPORTS[program_id]
        if spec.sas_export_file is not None
    )
    assert schemas.SAS_EXPORT_FILES == declared
    assert len(set(declared)) == len(declared), f"a SAS export name repeats: {declared}"
    assert len(declared) == _SAS_EXPORT_COUNT
    assert tuple(sas_exports) == schemas.SAS_EXPORT_FILES
    record_level = set()
    for file_name in schemas.SAS_EXPORT_FILES:
        spec = schemas.sas_export_spec(file_name)
        assert spec.sas_export_file == file_name
        assert spec.program_id in schemas.PROGRAMS
        assert file_name == f"{spec.program_id}_{spec.file_name}"
        if not spec.summary_fields and not spec.glm_fields:
            record_level.add(file_name)
    assert set(_RECORD_COUNTS) == record_level
    assert set(_KEY_COUNTS) == set(schemas.SAS_EXPORT_FILES) - record_level
    assert set(_EXPORT_COUNTS) == set(schemas.SAS_EXPORT_FILES)


def _table_cells(readme: Path, line: str) -> tuple[str, ...]:
    """The stripped cells of one Markdown table row, which opens and closes with a pipe."""
    assert line.startswith("|") and line.endswith("|"), f"{readme}: not a table row: {line!r}"
    return tuple(cell.strip() for cell in line[1:-1].split("|"))


def _expected_files_rows(readme: Path) -> dict[str, tuple[str, ...]]:
    """The body rows of the README's "Expected files" table, keyed by the file each row names.

    The table is the first one in the section under :data:`_EXPECTED_FILES_HEADING`. Its header
    must be :data:`_EXPECTED_FILES_COLUMNS` and its second row a delimiter row of as many cells.
    Every body row must have that many cells, the first a code span naming a file not named before.
    """
    lines = readme.read_text(encoding="utf-8").splitlines()
    assert _EXPECTED_FILES_HEADING in lines, f"{readme}: no {_EXPECTED_FILES_HEADING!r} section"
    section = itertools.takewhile(
        lambda line: not line.startswith("#"), lines[lines.index(_EXPECTED_FILES_HEADING) + 1 :]
    )
    table = list(
        itertools.takewhile(
            lambda line: line.startswith("|"),
            itertools.dropwhile(lambda line: not line.startswith("|"), section),
        )
    )
    assert len(table) > 2, f"{readme}: the {_EXPECTED_FILES_HEADING!r} section has no table rows"
    header, delimiter, *body = (_table_cells(readme, line) for line in table)
    assert header == _EXPECTED_FILES_COLUMNS, f"{readme}: table header {header}"
    assert len(delimiter) == len(header), f"{readme}: delimiter row {delimiter}"
    assert all(_DELIMITER_CELL.fullmatch(cell) for cell in delimiter), (
        f"{readme}: delimiter row {delimiter}"
    )
    rows: dict[str, tuple[str, ...]] = {}
    for cells in body:
        assert len(cells) == len(header), f"{readme}: row of {len(cells)} cells: {cells}"
        named = _CODE_SPAN_CELL.fullmatch(cells[0])
        assert named, f"{readme}: the File cell is not one code span: {cells[0]!r}"
        assert named.group(1) not in rows, f"{readme}: {named.group(1)} is named twice"
        rows[named.group(1)] = cells
    return rows


@pytest.mark.parametrize("file_name", schemas.SAS_EXPORT_FILES)
def test_s02_trn_03_export_readme_quotes_sas_export_source(
    file_name: str, repo_paths: RepoPaths
) -> None:
    """The export README quotes ``schemas.SAS_EXPORT_SOURCES[file_name]`` verbatim.

    The "Expected files" table of ``python/wpct/reference/sas_exports/README.md`` is a five-column
    Markdown table naming the files of ``schemas.SAS_EXPORT_FILES``, each once and in that order.
    The "SAS source" cell of ``file_name``'s row is its ``SAS_EXPORT_SOURCES`` entry as one code
    span, so the capture point is stated in ``wpct.schemas`` alone and its
    ``[whitepapers/WPCT/<program>.sas:<lines>]`` citations render as written. The seven entries
    name the datasets of S02-TRN-03, of S02-MOD-01 with the S02-TRN-05 ``pval`` and of S02-MOD-02,
    the LMM-TRN-07 records after S07-FLT-02, the analysis dataset of S07-TRN-04, S07-TRN-05 and
    S07-TRN-06, the statistics of S07-MOD-01 with the S07-TRN-08 ``pval``, and the estimates of
    S07-MOD-02 and S07-MOD-03. The README is read from the checkout and never written; the case
    needs no SAS export and never skips.
    """
    rows = _expected_files_rows(repo_paths.sas_exports_dir / _EXPORT_README)
    assert tuple(rows) == schemas.SAS_EXPORT_FILES
    source = schemas.SAS_EXPORT_SOURCES[file_name]
    assert source and "`" not in source and "|" not in source, f"unquotable source {source!r}"
    assert rows[file_name][_EXPECTED_FILES_COLUMNS.index("SAS source")] == f"`{source}`"


# --- Synthetic mechanics controls -----------------------------------------------------------------
#
# The exports below are written from the default run's own datasets. They are mechanics fixtures
# that exercise the loader and the comparator, not SAS reference evidence, and they carry no SAS
# value (AAP 0.7.3). They exist only under each test's ``tmp_path``.

#: The edits a :class:`_Mutation` can make; see its ``edit`` attribute.
_EDIT_OFFSET = "offset"
_EDIT_MISSING = "missing"
_EDIT_RELABEL = "relabel"
_EDIT_DROP = "drop"
_EDIT_DUPLICATE = "duplicate"
_EDIT_PYTHON_OFFSET = "python offset"
_EDITS = (
    _EDIT_OFFSET,
    _EDIT_MISSING,
    _EDIT_RELABEL,
    _EDIT_DROP,
    _EDIT_DUPLICATE,
    _EDIT_PYTHON_OFFSET,
)
_OFFSET_EDITS = (_EDIT_OFFSET, _EDIT_PYTHON_OFFSET)
#: The edits that change which SAS records the export holds rather than a cell, so they need no
#: column and change the record count.
_RECORD_EDITS = (_EDIT_DROP, _EDIT_DUPLICATE)


@dataclasses.dataclass(frozen=True)
class _Mutation:
    """One edit of one record of a synthetic export, and the rows it must make FAIL.

    Attributes:
        label: The pytest id.
        file_name: The export edited.
        record: ``(upper-case column, value)`` pairs that pick exactly one record of the run's
            dataset, and so of the synthetic export written from it.
        edit: ``offset`` moves the SAS number by ``factor`` times the limit of ``class_key``, under
            that class's rule (see :func:`_offset`). ``missing`` writes the SAS number as ``.``.
            ``relabel`` appends a suffix to the SAS text. ``drop`` leaves the SAS record out.
            ``duplicate`` writes the SAS record twice, the copy right after it. ``python offset``
            moves the Python number, on a copy of the run's dataset, by ``factor`` times the limit
            of ``class_key``, absolute.
        column: The upper-case column edited; empty for ``drop`` and ``duplicate``.
        class_key: The ``tolerances.json`` class whose limit sizes an offset.
        factor: The multiple of that limit; above 1 lies beyond it, below 1 within it.
        fails: The exact ``(quantity, stratum)`` pairs of the rows that must FAIL. Every other row
            must PASS.
        key_set_reason: The exact reason of the key-set agreement row; empty when the key set
            agrees.
    """

    label: str
    file_name: str
    record: tuple[tuple[str, object], ...]
    edit: str
    column: str = ""
    class_key: str = ""
    factor: float = 0.0
    fails: frozenset[tuple[str, str]] = frozenset()
    key_set_reason: str = ""

    def __post_init__(self) -> None:
        if self.edit not in _EDITS:
            raise ValueError(f"{self.label}: unknown edit {self.edit!r}")
        if self.edit not in _RECORD_EDITS and not self.column:
            raise ValueError(f"{self.label}: the {self.edit} edit needs a column")
        if self.edit in _OFFSET_EDITS and (not self.class_key or self.factor <= 0):
            raise ValueError(f"{self.label}: an offset needs a tolerance class and a factor")
        object.__setattr__(self, "fails", frozenset(self.fails))


def _matched(file_name: str | None) -> str:
    """The stratum of a record-level field row when every record of the export is matched."""
    return f"{_EXPORT_COUNTS[file_name]} matched records"


def _key_set(file_name: str | None) -> tuple[str, str]:
    """The key-set agreement row of an export, whose stratum lists its upper-case key columns."""
    spec = schemas.sas_export_spec(str(file_name))
    return _KEY_SET_AGREEMENT, " ".join(schemas.upper_columns(spec.key))


#: Records each edited by one mutation, and the key strata ``compare`` renders for them. The pooled
#: ``STUDYID`` starts with the byte ``'A0'x`` (D-16), which ``compare`` renders as ``<A0>``.
_ANADATA_07_02_RECORD = (
    ("STUDYID", "CDISCPILOT01"),
    ("USUBJID", "01-701-1015"),
    ("PARAMCD", "DIABP"),
    ("ATPTN", 815),
    ("AVISITN", 4),
)
_ADVS_MIN_RECORD = (
    ("STUDYID", "CDISCPILOT01"),
    ("USUBJID", "01-701-1015"),
    ("TRTPN", 0),
    ("PARAMCD", "DIABP"),
    ("ATPTN", 815),
    ("AVISITN", 9912),
)
_ADVS_MIN_KEY = (
    "STUDYID=CDISCPILOT01 USUBJID=01-701-1015 TRTPN=0 PARAMCD=DIABP ATPTN=815 AVISITN=9912"
)
_ADVS_MIN_STRATUM = "PARAMCD=DIABP ATPTN=815 STUDYID=CDISCPILOT01 TRTPN=0 AVISITN=9912"
_POOLED_ANADATA_RECORD = (
    ("STUDYID", schemas.POOLED_STUDYID),
    ("USUBJID", "P1-701-1015"),
    ("PARAMCD", "DIABP"),
    ("ATPTN", 815),
)
_POOLED_ANADATA_KEY = "STUDYID=<A0>Pooled USUBJID=P1-701-1015 PARAMCD=DIABP ATPTN=815"
_POOLED_ANADATA_RELABELLED_KEY = (
    f"STUDYID=<A0>Pooled USUBJID=P1-701-1015{_RELABEL_SUFFIX} PARAMCD=DIABP ATPTN=815"
)
_STATS_07_02_RECORD = (("PARAMCD", "DIABP"), ("ATPTN", 815), ("AVISITN", 99), ("TRTPN", 54))
_STATS_07_02_KEY = "PARAMCD=DIABP ATPTN=815 AVISITN=99 TRTPN=54"
_POOLED_STATS_RECORD = (("PARAMCD", "DIABP"), ("ATPTN", 817), ("STUDYNUM", 3), ("TRTPN", 81))
_POOLED_STATS_KEY = "PARAMCD=DIABP ATPTN=817 STUDYNUM=3 TRTPN=81"
_ESTIMATES_07_02_RECORD = (
    ("PARAMCD", "SYSBP"),
    ("ATPTN", 816),
    ("EFFECT", "TRTPN"),
    ("LEVEL", "81"),
)
_ESTIMATES_07_02_KEY = "PARAMCD=SYSBP ATPTN=816 EFFECT=TRTPN LEVEL=81"
_STUDY_ESTIMATES_RECORD = (
    ("PARAMCD", "DIABP"),
    ("ATPTN", 817),
    ("STUDYNUM", 2),
    ("EFFECT", "TRTPN"),
    ("LEVEL", "54"),
)
_STUDY_ESTIMATES_KEY = "PARAMCD=DIABP ATPTN=817 STUDYNUM=2 EFFECT=TRTPN LEVEL=54"
#: The aliased continuous ``studynum`` term of the pooled model (S07-MOD-03, D-06, DIV-01).
_POOLED_STUDYNUM_RECORD = (
    ("PARAMCD", "DIABP"),
    ("ATPTN", 815),
    ("STUDYNUM", 3),
    ("EFFECT", "STUDYNUM"),
)
_POOLED_STUDYNUM_KEY = "PARAMCD=DIABP ATPTN=815 STUDYNUM=3 EFFECT=STUDYNUM LEVEL=."

_MUTATIONS = (
    _Mutation(
        "07.02-css_anadata-CHG-beyond-record_numeric",
        _FILE_07_02_ANADATA,
        _ANADATA_07_02_RECORD,
        _EDIT_OFFSET,
        "CHG",
        "record_numeric",
        2.0,
        frozenset({("CHG", _matched(_FILE_07_02_ANADATA))}),
    ),
    _Mutation(
        "07.02-css_anadata-CHG-within-record_numeric",
        _FILE_07_02_ANADATA,
        _ANADATA_07_02_RECORD,
        _EDIT_OFFSET,
        "CHG",
        "record_numeric",
        0.5,
    ),
    _Mutation(
        "07.02-css_anadata-trtp_short-relabelled",
        _FILE_07_02_ANADATA,
        _ANADATA_07_02_RECORD,
        _EDIT_RELABEL,
        "TRTP_SHORT",
        fails=frozenset({("TRTP_SHORT", _matched(_FILE_07_02_ANADATA))}),
    ),
    _Mutation(
        "07.02-css_anadata-BASE-missing-in-SAS-only",
        _FILE_07_02_ANADATA,
        _ANADATA_07_02_RECORD,
        _EDIT_MISSING,
        "BASE",
        fails=frozenset({("BASE", _matched(_FILE_07_02_ANADATA))}),
    ),
    _Mutation(
        "07.07-advs_min-ADT-missing-in-SAS-only",
        _FILE_07_07_ADVS_MIN,
        _ADVS_MIN_RECORD,
        _EDIT_MISSING,
        "ADT",
        fails=frozenset({("ADT", _matched(_FILE_07_07_ADVS_MIN))}),
    ),
    _Mutation(
        "07.07-advs_min-record-only-in-Python",
        _FILE_07_07_ADVS_MIN,
        _ADVS_MIN_RECORD,
        _EDIT_DROP,
        fails=frozenset(
            {
                (_KEY_ONLY_IN_PYTHON, _ADVS_MIN_KEY),
                _key_set(_FILE_07_07_ADVS_MIN),
                (_RECORD_COUNT, _ALL_STRATA),
                (_STRATUM_RECORD_COUNT, _ADVS_MIN_STRATUM),
            }
        ),
        key_set_reason="0 keys only in SAS, 1 only in Python",
    ),
    _Mutation(
        "07.07-advs_min-record-repeated-in-SAS",
        _FILE_07_07_ADVS_MIN,
        _ADVS_MIN_RECORD,
        _EDIT_DUPLICATE,
        fails=frozenset(
            {
                (_KEY_REPEATED_IN_SAS, _ADVS_MIN_KEY),
                _key_set(_FILE_07_07_ADVS_MIN),
                (_RECORD_COUNT, _ALL_STRATA),
                (_STRATUM_RECORD_COUNT, _ADVS_MIN_STRATUM),
            }
        ),
        key_set_reason="1 repeated SAS key, 0 repeated Python keys",
    ),
    _Mutation(
        "07.07-css_anadata-pooled-key-only-in-SAS-and-only-in-Python",
        _FILE_07_07_ANADATA,
        _POOLED_ANADATA_RECORD,
        _EDIT_RELABEL,
        "USUBJID",
        fails=frozenset(
            {
                (_KEY_ONLY_IN_SAS, _POOLED_ANADATA_RELABELLED_KEY),
                (_KEY_ONLY_IN_PYTHON, _POOLED_ANADATA_KEY),
                _key_set(_FILE_07_07_ANADATA),
            }
        ),
        key_set_reason="1 key only in SAS, 1 only in Python",
    ),
    _Mutation(
        "07.02-css_stats-mean-beyond-summary_statistics",
        _FILE_07_02_STATS,
        _STATS_07_02_RECORD,
        _EDIT_OFFSET,
        "MEAN",
        "summary_statistics",
        2.0,
        frozenset({("MEAN", _STATS_07_02_KEY)}),
    ),
    _Mutation(
        "07.02-css_stats-n-off-by-one",
        _FILE_07_02_STATS,
        _STATS_07_02_RECORD,
        _EDIT_OFFSET,
        "N",
        "counts",
        1.0,
        frozenset({("N", _STATS_07_02_KEY)}),
    ),
    _Mutation(
        "07.02-css_stats-merged-pval-beyond-p_values",
        _FILE_07_02_STATS,
        _STATS_07_02_RECORD,
        _EDIT_OFFSET,
        "PVAL",
        "p_values",
        2.0,
        frozenset({("PVAL", _STATS_07_02_KEY)}),
    ),
    _Mutation(
        "07.07-css_stats-pooled-std-beyond-summary_statistics",
        _FILE_07_07_STATS,
        _POOLED_STATS_RECORD,
        _EDIT_OFFSET,
        "STD",
        "summary_statistics",
        2.0,
        frozenset({("STD", _POOLED_STATS_KEY)}),
    ),
    _Mutation(
        "07.07-css_stats-pooled-std-within-summary_statistics",
        _FILE_07_07_STATS,
        _POOLED_STATS_RECORD,
        _EDIT_OFFSET,
        "STD",
        "summary_statistics",
        0.5,
    ),
    _Mutation(
        "07.02-parameter_estimates-ESTIMATE-beyond-glm_estimates",
        _FILE_07_02_ESTIMATES,
        _ESTIMATES_07_02_RECORD,
        _EDIT_OFFSET,
        "ESTIMATE",
        "glm_estimates",
        2.0,
        frozenset({("ESTIMATE", _ESTIMATES_07_02_KEY)}),
    ),
    _Mutation(
        "07.02-parameter_estimates-ESTIMATE-within-glm_estimates",
        _FILE_07_02_ESTIMATES,
        _ESTIMATES_07_02_RECORD,
        _EDIT_OFFSET,
        "ESTIMATE",
        "glm_estimates",
        0.5,
    ),
    _Mutation(
        "07.02-parameter_estimates-PROBT-beyond-p_values",
        _FILE_07_02_ESTIMATES,
        _ESTIMATES_07_02_RECORD,
        _EDIT_OFFSET,
        "PROBT",
        "p_values",
        2.0,
        frozenset({("PROBT", _ESTIMATES_07_02_KEY)}),
    ),
    _Mutation(
        "07.07-parameter_estimates-TVALUE-beyond-glm_estimates",
        _FILE_07_07_ESTIMATES,
        _STUDY_ESTIMATES_RECORD,
        _EDIT_OFFSET,
        "TVALUE",
        "glm_estimates",
        2.0,
        frozenset({("TVALUE", _STUDY_ESTIMATES_KEY)}),
    ),
    _Mutation(
        "07.07-parameter_estimates-STDERR-within-glm_estimates",
        _FILE_07_07_ESTIMATES,
        _STUDY_ESTIMATES_RECORD,
        _EDIT_OFFSET,
        "STDERR",
        "glm_estimates",
        0.5,
    ),
    _Mutation(
        "07.07-parameter_estimates-pooled-studynum-zero-reference",
        _FILE_07_07_ESTIMATES,
        _POOLED_STUDYNUM_RECORD,
        _EDIT_PYTHON_OFFSET,
        "ESTIMATE",
        "glm_estimates",
        1.0,
        frozenset({("ESTIMATE", _POOLED_STUDYNUM_KEY)}),
    ),
)

#: Row removals the coverage check must reject: every field row (leaving only the key-set and
#: record-count rows), one record-level field row, one key's field rows of a per-key export, and
#: one stratum-count row.
_REMOVE_FIELDS = "fields-removed"
_REMOVE_ONE_FIELD = "one-field-removed"
_REMOVE_ONE_KEY = "one-key-removed"
_REMOVE_ONE_STRATUM = "one-stratum-removed"


def _removals(file_name: str) -> tuple[str, ...]:
    """The row removals that apply to an export under its comparison dispatch."""
    spec = schemas.sas_export_spec(file_name)
    if _per_key(spec):
        return (_REMOVE_FIELDS, _REMOVE_ONE_KEY)
    if spec.count_strata:
        return (_REMOVE_FIELDS, _REMOVE_ONE_FIELD, _REMOVE_ONE_STRATUM)
    return (_REMOVE_FIELDS, _REMOVE_ONE_FIELD)


_REJECTIONS = tuple(
    (file_name, removal)
    for file_name in schemas.SAS_EXPORT_FILES
    for removal in _removals(file_name)
)


def _sas_columns(spec: schemas.ExportSpec) -> tuple[str, ...]:
    """The SAS variables of a synthetic export, as the export README lists them.

    A ParameterEstimates export holds the stratum columns plus the ODS columns
    ``Parameter Estimate StdErr tValue Probt``, without ``EFFECT`` or ``LEVEL``, which the loader
    derives. Every other export holds the spec's columns, its stratum columns among them.
    """
    if spec.glm_fields:
        return (
            *schemas.SAS_EXPORT_STRATUM_COLUMNS[str(spec.sas_export_file)],
            *schemas.SAS_PARAMETER_ESTIMATE_COLUMNS,
        )
    return spec.columns


def _frame_column(frame: pd.DataFrame, name: str) -> str:
    """The column of ``frame`` whose upper-case name is ``name``; SAS names ignore case."""
    columns = {str(column).upper(): str(column) for column in frame.columns}
    assert name in columns, f"the run's dataset has no column {name}"
    return columns[name]


def _sas_text(value: object, numeric: bool) -> str:
    """One cell as a synthetic export writes it.

    A number is written in shortest round-trip form and a missing number as ``.``. Text is
    written as it is, with a missing value left empty.
    """
    if numeric:
        return _SAS_MISSING if pd.isna(value) else repr(float(value))
    return "" if pd.isna(value) else str(value)


def _synthetic_rows(
    spec: schemas.ExportSpec, frame: pd.DataFrame
) -> tuple[tuple[str, ...], list[list[str]]]:
    """The SAS variable names and the text records of a synthetic export echoing ``frame``.

    Records keep the order of ``frame``, so a record's position is the same in both. ``frame`` is
    only read.
    """
    columns = _sas_columns(spec)
    kinds = dict(zip(schemas.upper_columns(spec.column_types), spec.column_types.values()))
    cells = []
    for name in schemas.upper_columns(columns):
        numeric = kinds[name] in _NUMERIC_TYPES
        cells.append([_sas_text(value, numeric) for value in frame[_frame_column(frame, name)]])
    return columns, [list(record) for record in zip(*cells)]


def _record_position(
    file_name: str, frame: pd.DataFrame, record: Sequence[tuple[str, object]]
) -> int:
    """The position in ``frame`` of the one record whose columns hold the values of ``record``."""
    columns = [frame[_frame_column(frame, name)].tolist() for name, _ in record]
    target = [value for _, value in record]
    hits = [position for position, values in enumerate(zip(*columns)) if list(values) == target]
    assert len(hits) == 1, f"{file_name}: {len(hits)} records match {dict(record)}, not one"
    return hits[0]


def _offset(value: float, class_key: str, factor: float, tolerances: compare.Tolerances) -> float:
    """``value`` moved by ``factor`` times the limit of ``class_key``, under that class's rule.

    An absolute class moves it by ``factor × limit``, and the relative ``glm_estimates`` by
    ``factor × limit × |value|``. The exact integer ``counts`` move by ``factor × (limit + 1)``,
    because the smallest step past an exact limit is one count more.
    """
    limit = tolerances.limit(class_key)
    rule = compare.TOLERANCE_CLASSES[class_key]
    if rule == "relative":
        return value + factor * limit * abs(value)
    if rule == "exact":
        return value + factor * (limit + 1)
    assert rule == "absolute", f"no offset is defined for the {rule} rule of {class_key}"
    return value + factor * limit


def _sas_edited(
    columns: Sequence[str],
    rows: Sequence[Sequence[str]],
    position: int,
    mutation: _Mutation,
    tolerances: compare.Tolerances,
) -> list[list[str]]:
    """A copy of the synthetic text records with ``mutation`` applied to the record at
    ``position``."""
    edited = [list(record) for record in rows]
    if mutation.edit == _EDIT_DROP:
        del edited[position]
        return edited
    if mutation.edit == _EDIT_DUPLICATE:
        edited.insert(position + 1, list(edited[position]))
        return edited
    index = schemas.upper_columns(columns).index(mutation.column)
    before = edited[position][index]
    if mutation.edit == _EDIT_MISSING:
        after = _SAS_MISSING
    elif mutation.edit == _EDIT_RELABEL:
        after = before + _RELABEL_SUFFIX
    elif mutation.edit == _EDIT_OFFSET:
        assert before != _SAS_MISSING, f"{mutation.label}: {mutation.column} is missing"
        after = repr(_offset(float(before), mutation.class_key, mutation.factor, tolerances))
    else:
        raise AssertionError(f"{mutation.label}: {mutation.edit} does not edit the SAS export")
    assert after != before, f"{mutation.label}: the edit leaves {mutation.column} unchanged"
    edited[position][index] = after
    return edited


def _python_edited(
    result: ProgramResult,
    spec: schemas.ExportSpec,
    position: int,
    mutation: _Mutation,
    tolerances: compare.Tolerances,
) -> ProgramResult:
    """A new run whose dataset of ``spec`` is a deep copy with one number moved, absolutely, by
    ``factor`` times the limit of ``class_key``. The session run and its frames are untouched."""
    edited = result.datasets[spec.name].copy(deep=True)
    column = edited.columns.get_loc(_frame_column(edited, mutation.column))
    before = edited.iat[position, column]
    assert not pd.isna(before), f"{mutation.label}: {mutation.column} is missing"
    after = float(before) + mutation.factor * tolerances.limit(mutation.class_key)
    assert after != before, f"{mutation.label}: the edit leaves {mutation.column} unchanged"
    edited.iat[position, column] = after
    return dataclasses.replace(result, datasets={**result.datasets, spec.name: edited})


def _write_export(path: Path, columns: Sequence[str], rows: Sequence[Sequence[str]]) -> None:
    """Write a synthetic export as the README specifies: comma-separated, Latin-1, one header
    row. The SAS names alternate between lower and capitalised case, because the loader must
    match them case-insensitively."""
    header = [
        name.lower() if position % 2 == 0 else name.capitalize()
        for position, name in enumerate(columns)
    ]
    with path.open("w", encoding="latin-1", newline="") as handle:
        writer = csv.writer(handle, lineterminator="\n")
        writer.writerow(header)
        writer.writerows(rows)


@pytest.fixture
def synthetic_export_group(
    tmp_path: Path,
    results: Mapping[str, ProgramResult],
    listing_tables: Mapping[str, tuple[sas_reference.ListingTable, ...]],
    sas_checkpoints: Mapping[str, tuple[sas_reference.SasCheckpoint, ...]],
    tolerances: compare.Tolerances,
) -> Callable[..., compare.ComparisonGroup]:
    """A callable that writes, loads and compares one synthetic export; a mechanics fixture.

    Called with an export file name and an optional :class:`_Mutation`, it writes that export
    alone into a new directory under ``tmp_path``. The export echoes the default run's dataset,
    with the mutation applied. The callable loads it with ``sas_reference.load_sas_exports`` and
    runs ``compare.compare_program`` for its program. It returns the export's comparison group,
    after checking that the directory loads as that one export and that the program gets exactly
    one export group.
    """
    directories = itertools.count(1)

    def build(file_name: str, mutation: _Mutation | None = None) -> compare.ComparisonGroup:
        spec = schemas.sas_export_spec(file_name)
        result = results[spec.program_id]
        frame = result.datasets[spec.name]
        columns, rows = _synthetic_rows(spec, frame)
        if mutation is not None:
            position = _record_position(file_name, frame, mutation.record)
            if mutation.edit == _EDIT_PYTHON_OFFSET:
                result = _python_edited(result, spec, position, mutation, tolerances)
            else:
                rows = _sas_edited(columns, rows, position, mutation, tolerances)
        directory = tmp_path / f"sas_exports_{next(directories)}"
        directory.mkdir()
        _write_export(directory / file_name, columns, rows)
        exports = sas_reference.load_sas_exports(directory)
        loaded = [name for name, export in exports.items() if export is not None]
        assert loaded == [file_name], f"{directory} loaded as {loaded}, not [{file_name!r}]"
        groups = compare.compare_program(
            result,
            listing_tables[spec.program_id],
            sas_checkpoints[spec.program_id],
            exports,
            tolerances,
        )
        export_keys = {compare.export_group_key(name) for name in schemas.SAS_EXPORT_FILES}
        found = [group.key for group in groups if group.key in export_keys]
        assert found == [compare.export_group_key(file_name)], (
            f"{file_name}: export groups {found}, expected only this export's"
        )
        return _export_group(file_name, groups)

    return build


@pytest.mark.parametrize("file_name", schemas.SAS_EXPORT_FILES)
def test_d16_synthetic_export_mechanics_all_pass(
    file_name: str,
    synthetic_export_group: Callable[..., compare.ComparisonGroup],
    results: Mapping[str, ProgramResult],
) -> None:
    """Mechanics control: an export that echoes the default run passes with full coverage.

    The file is written under ``tmp_path`` from the run's own dataset, so it is not SAS reference
    evidence. On a clean checkout it exercises what a real export would: Latin-1 and the pooled
    ``'A0'x`` identifier (D-16), names matched case-insensitively, ``.`` as missing,
    ``EFFECT``/``LEVEL`` derived from ``Parameter``, and the comparison dispatch. Every row must be
    PASS, and :func:`_assert_export_coverage` must hold.
    """
    spec = schemas.sas_export_spec(file_name)
    group = synthetic_export_group(file_name)
    _assert_export_agrees(file_name, group, results[spec.program_id].datasets[spec.name])


@pytest.mark.parametrize("mutation", _MUTATIONS, ids=[mutation.label for mutation in _MUTATIONS])
def test_s07_mod_03_synthetic_export_mismatch_fails_targeted_rows(
    mutation: _Mutation,
    synthetic_export_group: Callable[..., compare.ComparisonGroup],
    results: Mapping[str, ProgramResult],
) -> None:
    """Mechanics control: one edited cell or record fails exactly the rows it targets.

    The edits are sized from the ``tolerances.json`` limits (AAP 0.7.1). A value beyond its limit
    fails, and one within it passes. A changed label, or a number missing on one side only, fails
    its field. A record left out of SAS fails its key, the key set, the record count and its
    stratum count, and so does a record repeated in SAS. A changed key fails on both sides.
    Against a SAS-side 0, the value SAS gives the aliased pooled ``studynum`` (S07-MOD-03, D-06),
    a non-zero Python estimate fails, however small. Every other row must PASS. The one key-set
    agreement row gives exactly the reason expected, which is empty when the key set agrees. An
    edit that leaves the keys and the records alone must also leave the coverage of
    :func:`_assert_export_coverage` complete.
    """
    group = synthetic_export_group(mutation.file_name, mutation)
    rows = group.rows
    failed = sorted((row.quantity, row.stratum) for row in rows if row.status == compare.FAIL)
    assert failed == sorted(mutation.fails), (
        f"{mutation.label}: expected FAIL rows {sorted(mutation.fails)}, got:\n"
        f"{_describe([row for row in rows if row.status == compare.FAIL])}"
    )
    not_compared = [row for row in rows if row.status == compare.NOT_COMPARED]
    assert not not_compared, f"{mutation.label}: rows not compared:\n{_describe(not_compared)}"
    key_set = [row for row in rows if row.quantity == _KEY_SET_AGREEMENT]
    assert len(key_set) == 1, (
        f"{mutation.label}: expected one {_KEY_SET_AGREEMENT!r} row, found {len(key_set)}"
    )
    assert key_set[0].reason == mutation.key_set_reason, (
        f"{mutation.label}: {_KEY_SET_AGREEMENT} reason {key_set[0].reason!r}, expected "
        f"{mutation.key_set_reason!r}"
    )
    spec = schemas.sas_export_spec(mutation.file_name)
    key_columns = schemas.upper_columns(spec.key)
    if mutation.edit not in _RECORD_EDITS and mutation.column not in key_columns:
        _assert_export_coverage(
            mutation.file_name, rows, results[spec.program_id].datasets[spec.name]
        )


def _without(
    rows: Sequence[compare.ComparisonRow], file_name: str, removal: str
) -> tuple[compare.ComparisonRow, ...]:
    """``rows`` with the rows of one removal left out; at least one row must go."""
    fields = _expected_fields(schemas.sas_export_spec(file_name))
    if removal == _REMOVE_FIELDS:
        kept = [row for row in rows if row.quantity in (_KEY_SET_AGREEMENT, _RECORD_COUNT)]
    elif removal == _REMOVE_ONE_FIELD:
        kept = [row for row in rows if row.quantity != fields[-1]]
    elif removal == _REMOVE_ONE_KEY:
        key = next(row.stratum for row in rows if row.quantity == fields[0])
        kept = [row for row in rows if not (row.quantity in fields and row.stratum == key)]
    elif removal == _REMOVE_ONE_STRATUM:
        stratum = next(row for row in rows if row.quantity == _STRATUM_RECORD_COUNT)
        kept = [row for row in rows if row is not stratum]
    else:
        raise AssertionError(f"unknown removal {removal!r}")
    assert len(kept) < len(rows), f"{file_name}: {removal} removed no row"
    return tuple(kept)


@pytest.mark.parametrize(
    ("file_name", "removal"),
    _REJECTIONS,
    ids=[f"{file_name}-{removal}" for file_name, removal in _REJECTIONS],
)
def test_s02_trn_03_export_coverage_rejects_incomplete_group(
    file_name: str,
    removal: str,
    synthetic_export_group: Callable[..., compare.ComparisonGroup],
    results: Mapping[str, ProgramResult],
) -> None:
    """The coverage check rejects an all-PASS group that lacks rows its spec requires.

    The group of a synthetic mechanics export first passes the check whole. It is then rebuilt,
    with the same program and key, without some rows: every field row, one field row, one key's
    field rows or one stratum row. Every remaining row is PASS, so only the coverage check can
    reject it, and it must. A comparison that ran only the key-set and record-count rows cannot
    stand in for the record-level field agreement of the S02-TRN-03 and LMM-TRN-07 datasets and of
    the S07-TRN-04, S07-TRN-05 and S07-TRN-06 analysis dataset, for the statistics of S02-MOD-01
    and S07-MOD-01 with their S02-TRN-05 and S07-TRN-08 ``pval``, or for the estimates of
    S02-MOD-02, S07-MOD-02 and S07-MOD-03.
    """
    spec = schemas.sas_export_spec(file_name)
    python_frame = results[spec.program_id].datasets[spec.name]
    group = synthetic_export_group(file_name)
    _assert_export_agrees(file_name, group, python_frame)
    partial = dataclasses.replace(group, rows=_without(group.rows, file_name, removal))
    assert (partial.program, partial.key) == (group.program, group.key)
    assert partial.n_pass == len(partial.rows), f"{file_name}: the rebuilt group is not all PASS"
    with pytest.raises(AssertionError):
        _assert_export_agrees(file_name, partial, python_frame)
