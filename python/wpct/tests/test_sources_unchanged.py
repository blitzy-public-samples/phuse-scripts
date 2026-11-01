"""Byte-for-byte preservation of the SAS-era files the port reads.

Every path in ``provenance.READ_ONLY_INPUTS`` (the programs, manifests and macros, the two ADVS
transport files, the SAS listings, logs and box-plot PDFs, the licence and the dataset READMEs)
must hash, as its exact working-tree bytes, to its blob at ``provenance.UPSTREAM_COMMIT``, the
upstream commit named in the Sources section of ``python/wpct/docs/sas_specification.md``.

Git is reached only through ``wpct.provenance``, and nothing here writes a file. The integrity cases
skip, with a stated reason, only when git cannot query the checkout or the upstream commit is absent
from it (for instance a shallow clone). The checks on the constants themselves need no git.
"""

from __future__ import annotations

import re
from pathlib import Path, PurePosixPath

import pytest

from wpct import provenance
from wpct.paths import RepoPaths

#: A SHA-1 object id as git prints it.
_BLOB_ID = re.compile(r"[0-9a-f]{40}")

#: The 17 repository macros, in ``whitepapers/utilities/``, of the programs' dependency closure.
_UTILITY_MACROS = (
    "assert_depend",
    "assert_continue",
    "assert_var_nonmissing",
    "assert_unique_keys",
    "assert_dset_exist",
    "assert_var_exist",
    "assert_macro_exist",
    "util_resolve_sasautos",
    "util_labels_from_var",
    "util_count_unique_values",
    "util_get_var_min_max",
    "util_value_format",
    "util_delete_dsets",
    "util_proc_template",
    "util_boxplot_block_ranges",
    "util_axis_order",
    "util_access_test_data",
)

_OUTPUTS_SAS = "whitepapers/WPCT/outputs_sas"

#: The read-only inputs the plan names, written out here rather than taken from ``provenance`` so
#: that ``READ_ONLY_INPUTS`` is checked against that list instead of against itself.
_EXPECTED_READ_ONLY_INPUTS = frozenset(
    (
        "whitepapers/WPCT/WPCT-F.07.02.sas",
        "whitepapers/WPCT/WPCT-F.07.07.sas",
        "whitepapers/WPCT/WPCT-F.07.02_sas.yml",
        "whitepapers/WPCT/WPCT-F.07.07_sas.yml",
        *(f"whitepapers/utilities/{macro}.sas" for macro in _UTILITY_MACROS),
        "whitepapers/ADaM/derive_lastminmax_measure.sas",
        "data/adam/cdisc/advs.xpt",
        "data/adam/cdisc-split/advs.xpt",
        f"{_OUTPUTS_SAS}/WPCT-F.07.02.lst",
        f"{_OUTPUTS_SAS}/WPCT-F.07.02.log",
        f"{_OUTPUTS_SAS}/WPCT-F.07.07.lst",
        f"{_OUTPUTS_SAS}/WPCT-F.07.07.log",
        f"{_OUTPUTS_SAS}/WPCT-F.07.02_Box_plot_DIABP_Change_by_visit_for_timepoint_815.pdf",
        f"{_OUTPUTS_SAS}/WPCT-F.07.02_Box_plot_SYSBP_Change_by_visit_for_timepoint_816.pdf",
        (
            f"{_OUTPUTS_SAS}/"
            "WPCT-F.07.07_Box_plot_DIABP_change_MIN_base_post_by_study_for_timepoint_815.pdf"
        ),
        (
            f"{_OUTPUTS_SAS}/"
            "WPCT-F.07.07_Box_plot_DIABP_change_MIN_base_post_by_study_for_timepoint_817.pdf"
        ),
        "LICENSE.md",
        "data/adam/README.md",
        "data/adam/cdisc/README.md",
        "data/adam/cdisc-split/README.md",
    )
)


@pytest.fixture(scope="session")
def upstream_root(repo_paths: RepoPaths) -> Path:
    """The checkout root, once git can query it and it holds the upstream commit.

    Otherwise every test requesting it skips with the reason, so an unverifiable checkout is never
    reported as unchanged.
    """
    root = repo_paths.repo_root
    if not provenance.git_available(root):
        pytest.skip("git is not available; source integrity cannot be verified")
    if not provenance.commit_available(provenance.UPSTREAM_COMMIT, root):
        pytest.skip(
            f"upstream commit {provenance.UPSTREAM_COMMIT} not present in this clone; "
            "source integrity cannot be verified"
        )
    return root


@pytest.mark.parametrize("path", provenance.READ_ONLY_INPUTS, ids=str)
def test_read_only_input_unchanged(path: str, upstream_root: Path) -> None:
    """The working-tree bytes of ``path`` are the blob committed at the upstream commit."""
    assert (upstream_root / path).is_file(), (
        f"{path}: read-only input missing from the working tree"
    )
    upstream = provenance.blob_at(provenance.UPSTREAM_COMMIT, path, upstream_root)
    working = provenance.working_blob(path, upstream_root)
    blobs = f"working-tree blob {working}, blob {upstream} at {provenance.UPSTREAM_COMMIT}"
    assert upstream is not None and _BLOB_ID.fullmatch(upstream), (
        f"{path}: no blob at the upstream commit ({blobs})"
    )
    assert working is not None and _BLOB_ID.fullmatch(working), (
        f"{path}: working-tree file could not be hashed ({blobs})"
    )
    assert working == upstream, f"{path}: changed since the upstream commit ({blobs})"


def test_read_only_inputs_cover_aap_list() -> None:
    """``READ_ONLY_INPUTS`` names each planned read-only input once, and no file of the port."""
    inputs = provenance.READ_ONLY_INPUTS
    missing = sorted(_EXPECTED_READ_ONLY_INPUTS.difference(inputs))
    assert not missing, f"READ_ONLY_INPUTS lacks: {', '.join(missing)}"
    assert len(set(inputs)) == len(inputs), "READ_ONLY_INPUTS lists a path more than once"
    under_python = [path for path in inputs if PurePosixPath(path).parts[0] == "python"]
    assert not under_python, f"READ_ONLY_INPUTS includes port files: {', '.join(under_python)}"


def test_upstream_commit_constant(repo_paths: RepoPaths) -> None:
    """The upstream commit is the one the specification cites; checking it needs no git."""
    assert provenance.UPSTREAM_COMMIT == "398a6d33ced9359ffb58c46650a6d488811176b1"
    specification = repo_paths.spec_file.read_text(encoding="utf-8")
    assert f"Upstream commit: `{provenance.UPSTREAM_COMMIT}`" in specification
