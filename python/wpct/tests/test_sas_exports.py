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

A present export passes only when its group has rows and every row is PASS. A FAIL fails the case,
and so does a NOT COMPARED row, which never stands in for agreement. File names, keys and field
lists come from ``wpct.schemas`` alone (AAP 0.9.5). The export directory is a read-only input, and
nothing here writes.
"""

from __future__ import annotations

from collections.abc import Mapping, Sequence
from pathlib import Path
from types import MappingProxyType

import pandas as pd
import pytest

from wpct import compare, schemas

#: The seven export file names of AAP 0.7.3. They are restated here only to guard
#: ``schemas.SAS_EXPORT_FILES``, the parameter list the READMEs and the loader share.
_AAP_EXPORT_FILES = (
    "WPCT-F.07.02_css_anadata.csv",
    "WPCT-F.07.02_css_stats.csv",
    "WPCT-F.07.02_parameter_estimates.csv",
    "WPCT-F.07.07_advs_min.csv",
    "WPCT-F.07.07_css_anadata.csv",
    "WPCT-F.07.07_css_stats.csv",
    "WPCT-F.07.07_parameter_estimates.csv",
)

#: Records of each record-level SAS dataset, as its reference log reports them:
#: WORK.CSS_ANADATA of 07.02 [whitepapers/WPCT/outputs_sas/WPCT-F.07.02.log:202], WORK.ADVS_MIN
#: after the post-baseline filter [whitepapers/WPCT/outputs_sas/WPCT-F.07.07.log:229], and the
#: numbered WORK.CSS_ANADATA of 07.07 [whitepapers/WPCT/outputs_sas/WPCT-F.07.07.log:427].
_RECORD_COUNTS: Mapping[str | None, int] = MappingProxyType(
    {
        schemas.export_spec(schemas.PROGRAM_07_02, "css_anadata").sas_export_file: 3548,
        schemas.export_spec(schemas.PROGRAM_07_07, "advs_min").sas_export_file: 498,
        schemas.export_spec(schemas.PROGRAM_07_07, "css_anadata").sas_export_file: 996,
    }
)

#: Row quantities that ``compare`` writes for the record-key comparison of AAP 0.7.2.
_KEY_ONLY_IN_SAS = "record key only in SAS"
_KEY_ONLY_IN_PYTHON = "record key only in Python"
_KEY_SET_AGREEMENT = "key-set agreement"
_RECORD_COUNT = "record count"

#: An assertion message lists at most this many rows, then the remainder's count.
_MAX_LISTED_ROWS = 40


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


@pytest.mark.parametrize("file_name", schemas.SAS_EXPORT_FILES)
def test_d01_02_05_to_09_sas_export_equivalence(
    file_name: str,
    sas_exports_dir: Path,
    sas_exports: Mapping[str, pd.DataFrame | None],
    comparisons: Mapping[str, tuple[compare.ComparisonGroup, ...]],
) -> None:
    """A present SAS export agrees with the default run on every compared quantity.

    The tolerances are those of AAP 0.7.1, read from ``tolerances.json`` by ``compare``. For a
    record-level export, the AAP 0.7.2 key sets must agree and both sides must hold the record
    count of the SAS log. For statistics and estimates, D-01, D-02 and D-05 to D-09 govern the
    compared quantities. An absent export skips with the reason AAP 0.7.4 states.
    """
    if sas_exports[file_name] is None:
        pytest.skip(
            f"SAS reference export {file_name} not found in {sas_exports_dir}; "
            "see python/wpct/reference/sas_exports/README.md"
        )
    spec = schemas.sas_export_spec(file_name)
    group = _export_group(file_name, comparisons[spec.program_id])
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
    if file_name in _RECORD_COUNTS:
        _assert_record_keys(file_name, rows, _RECORD_COUNTS[file_name])


def test_export_parameter_list_matches_schemas(
    sas_exports: Mapping[str, pd.DataFrame | None],
) -> None:
    """The parameter list is exactly the seven AAP 0.7.3 export names, in AAP order (AAP 0.9.5).

    The loader returns an entry for every name, so no case can miss its export. Each name is the
    SAS export of its own program's spec. The record counts cover exactly the exports that have
    neither statistics nor GLM fields, which ``compare`` treats as record-level.
    """
    assert schemas.SAS_EXPORT_FILES == _AAP_EXPORT_FILES
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
