"""Byte-for-byte preservation of the SAS-era files the port reads.

Every path in ``provenance.READ_ONLY_INPUTS`` (the programs, manifests and macros, the two ADVS
transport files, the SAS listings, logs and box-plot PDFs, the licence and the dataset READMEs)
must hash, as its exact working-tree bytes, to its blob at ``provenance.UPSTREAM_COMMIT``, the
upstream commit named in the Sources section of ``python/wpct/docs/sas_specification.md``.

Git is reached only through ``wpct.provenance``. The integrity cases write nothing, and skip, with a
stated reason, only when git cannot query the checkout or the upstream commit is absent from it
(for instance a shallow clone). The checks on the constants themselves need no git.

The git boundary cases put a fake ``git`` shell script, alone, on ``PATH``: one that answers with
something other than an object id, hangs or answers slowly. They write only those scripts and the
files the scripts read or write, under pytest's ``tmp_path``. They show that provenance then never
reports integrity as verified, gives git up within its time budget and ends a timed-out git. The
cases whose fake git passes commands to the real git need git and the upstream commit, and skip
with the integrity cases' reasons without them.
"""

from __future__ import annotations

import os
import re
import shlex
import shutil
import time
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


# --- git boundary resilience ----------------------------------------------------------------------

#: Time limits the fake-git cases set, so that a hanging git costs a second rather than minutes.
_CALL_TIMEOUT_SECONDS = 1
_DEADLINE_SECONDS = 3

#: Allowance beyond the budget for starting processes and the real git commands on a loaded host.
_SLACK_SECONDS = 3

#: How long the slow fake git takes over every command before passing it to the real git.
_SLOW_SECONDS = 0.5

_posix_only = pytest.mark.skipif(
    os.name != "posix", reason="the fake git executables are POSIX shell scripts"
)


def _tool(name: str) -> str:
    """Absolute path of a tool a fake git calls, resolved while ``PATH`` is still the real one."""
    found = shutil.which(name)
    if found is None:
        pytest.skip(f"{name} is not on PATH, so the fake git cannot run")
    return found


def _real_git() -> str:
    """Absolute path of the real git, for a fake git that passes commands through to it."""
    found = shutil.which("git")
    if found is None:
        pytest.skip("git is not available; the pass-through fake git needs it")
    return found


def _install_fake_git(tmp_path: Path, monkeypatch: pytest.MonkeyPatch, body: str) -> Path:
    """Make a ``git`` script running ``body`` the only executable on ``PATH``; its call log.

    The script first appends its subcommand (the argument after ``-C <root>``) to the returned log.
    Provenance's time limits are shortened for the test.
    """
    directory = tmp_path / "fake-bin"
    directory.mkdir()
    calls = tmp_path / "git-calls.log"
    script = directory / "git"
    script.write_text(
        f'#!/bin/sh\nprintf "%s\\n" "$3" >> {shlex.quote(str(calls))}\n{body}\n',
        encoding="utf-8",
    )
    script.chmod(0o755)
    monkeypatch.setenv("PATH", str(directory))
    monkeypatch.setattr(provenance, "_GIT_CALL_TIMEOUT_SECONDS", _CALL_TIMEOUT_SECONDS)
    monkeypatch.setattr(provenance, "_GIT_DEADLINE_SECONDS", _DEADLINE_SECONDS)
    return calls


def _subcommands(calls: Path) -> list[str]:
    """The git subcommands the fake git was started with, in order."""
    return calls.read_text(encoding="utf-8").splitlines() if calls.exists() else []


def _assert_not_verified(facts: provenance.Provenance) -> None:
    """Git counts as unavailable, integrity is not verified, and no blob id is malformed."""
    assert facts.git_available is False
    assert facts.integrity_verified is False
    assert [row.path for row in facts.integrity] == list(provenance.READ_ONLY_INPUTS)
    for row in facts.integrity:
        for blob in (row.upstream_blob, row.working_blob):
            assert blob is None or _BLOB_ID.fullmatch(blob), f"{row.path}: blob {blob!r}"


def _process_ended(pid: int, within: float = 5.0) -> bool:
    """Whether process ``pid`` exits within ``within`` seconds: it is gone, or a zombie."""
    deadline = time.monotonic() + within
    while True:
        try:
            os.kill(pid, 0)
        except ProcessLookupError:
            return True
        try:
            stat = Path(f"/proc/{pid}/stat").read_text(encoding="utf-8", errors="replace")
        except OSError:
            stat = ""
        # The state is the first field after the parenthesised command name.
        if stat.rpartition(")")[2].split()[:1] == ["Z"]:
            return True
        if time.monotonic() >= deadline:
            return False
        time.sleep(0.05)


@_posix_only
@pytest.mark.parametrize(
    "output", [b"garbage-not-a-sha\n", b"\377\376\000bad"], ids=["text", "binary"]
)
def test_git_output_not_an_object_id_verifies_nothing(
    output: bytes, tmp_path: Path, monkeypatch: pytest.MonkeyPatch, repo_paths: RepoPaths
) -> None:
    """A git that exits 0 with something other than an object id is unusable.

    HEAD is unavailable, no row is verified and git is not queried again. Run on its own, each
    public lookup finds no HEAD, no blob and no upstream commit either.
    """
    payload = tmp_path / "git-output"
    payload.write_bytes(output)
    cat = _tool("cat")
    calls = _install_fake_git(
        tmp_path, monkeypatch, f"exec {shlex.quote(cat)} {shlex.quote(str(payload))}"
    )

    facts = provenance.collect(repo_paths)

    assert facts.head_commit == provenance.UNAVAILABLE
    _assert_not_verified(facts)
    for row in facts.integrity:
        assert (row.upstream_blob, row.working_blob, row.status) == (
            None,
            None,
            provenance.UNAVAILABLE,
        ), row.path
    assert _subcommands(calls) == ["rev-parse"], "git was queried again after a malformed answer"

    root = repo_paths.repo_root
    path = provenance.READ_ONLY_INPUTS[0]
    assert provenance.head_commit(root) == provenance.UNAVAILABLE
    assert provenance.blob_at(provenance.UPSTREAM_COMMIT, path, root) is None
    assert provenance.working_blob(path, root) is None
    assert provenance.commit_available(provenance.UPSTREAM_COMMIT, root) is False


@_posix_only
@pytest.mark.parametrize(
    ("hang_on", "expected_calls"),
    [
        ("*", ["rev-parse"]),
        ("hash-object", ["rev-parse", "rev-parse", "cat-file", "rev-parse", "hash-object"]),
    ],
    ids=["every-command", "hash-object-only"],
)
def test_hanging_git_is_given_up_after_one_timeout(
    hang_on: str,
    expected_calls: list[str],
    tmp_path: Path,
    monkeypatch: pytest.MonkeyPatch,
    repo_paths: RepoPaths,
    request: pytest.FixtureRequest,
) -> None:
    """A git command that hangs costs one timeout; git is then not started again.

    Every other command of the hash-object case goes to the real git, so HEAD and the first
    input's upstream blob are known, while every row stays unverified. That case needs git and the
    upstream commit, and skips with the integrity cases' reasons without them.
    """
    passes_through = hang_on != "*"
    if passes_through:
        # Resolved while PATH still finds the real git, so it skips when git cannot verify the
        # checkout rather than when the fake git answers.
        request.getfixturevalue("upstream_root")
    otherwise = f'exec {shlex.quote(_real_git())} "$@"' if passes_through else ""
    sleep = shlex.quote(_tool("sleep"))
    calls = _install_fake_git(
        tmp_path, monkeypatch, f'case "$3" in\n  {hang_on}) exec {sleep} 300 ;;\nesac\n{otherwise}'
    )

    started = time.monotonic()
    facts = provenance.collect(repo_paths)
    elapsed = time.monotonic() - started

    assert elapsed < _DEADLINE_SECONDS + _SLACK_SECONDS, f"collect took {elapsed:.1f} s"
    assert _subcommands(calls) == expected_calls
    _assert_not_verified(facts)
    assert {row.status for row in facts.integrity} == {provenance.UNAVAILABLE}
    if passes_through:
        assert _BLOB_ID.fullmatch(facts.head_commit), facts.head_commit
        first, *rest = facts.integrity
        assert first.upstream_blob is not None and first.working_blob is None
    else:
        assert facts.head_commit == provenance.UNAVAILABLE
        rest = list(facts.integrity)
    assert all(row.upstream_blob is None and row.working_blob is None for row in rest)


@_posix_only
def test_slow_git_is_bounded_by_the_time_budget(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch, repo_paths: RepoPaths, upstream_root: Path
) -> None:
    """A git that answers every command, but slowly, is given up once the budget is spent.

    Rows compared before that keep their result; every later row is unverified. The real git runs
    enough commands to spend the budget only in a checkout holding the upstream commit, so the case
    needs git and that commit, and skips with the integrity cases' reasons without them.
    """
    real_git = shlex.quote(_real_git())
    sleep = shlex.quote(_tool("sleep"))
    calls = _install_fake_git(
        tmp_path, monkeypatch, f'{sleep} {_SLOW_SECONDS}\nexec {real_git} "$@"'
    )

    started = time.monotonic()
    facts = provenance.collect(repo_paths)
    elapsed = time.monotonic() - started

    assert elapsed < _DEADLINE_SECONDS + _SLACK_SECONDS, f"collect took {elapsed:.1f} s"
    # Each command takes at least _SLOW_SECONDS, and none starts once the budget is spent.
    assert 0 < len(_subcommands(calls)) <= int(_DEADLINE_SECONDS / _SLOW_SECONDS) + 1
    _assert_not_verified(facts)
    statuses = [row.status for row in facts.integrity]
    assert provenance.UNAVAILABLE in statuses
    first_unverified = statuses.index(provenance.UNAVAILABLE)
    assert set(statuses[:first_unverified]) <= {provenance.STATUS_UNCHANGED}
    assert set(statuses[first_unverified:]) == {provenance.UNAVAILABLE}


@_posix_only
def test_timed_out_git_is_ended(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch, repo_paths: RepoPaths
) -> None:
    """A git command that outlives its timeout is ended, and the lookup returns soon after.

    The hanging git replaces its shell with ``sleep``, so a git left running would keep sleeping
    long after the lookup gave it up.
    """
    sleep = shlex.quote(_tool("sleep"))
    pid_file = tmp_path / "git.pid"
    calls = _install_fake_git(
        tmp_path, monkeypatch, f"echo $$ > {shlex.quote(str(pid_file))}\nexec {sleep} 30"
    )

    started = time.monotonic()
    head = provenance.head_commit(repo_paths.repo_root)
    elapsed = time.monotonic() - started

    assert head == provenance.UNAVAILABLE
    assert elapsed < _CALL_TIMEOUT_SECONDS + _SLACK_SECONDS, f"head_commit took {elapsed:.1f} s"
    assert _subcommands(calls) == ["rev-parse"]
    assert pid_file.is_file(), "the fake git did not record its process id"
    pid = int(pid_file.read_text(encoding="utf-8"))
    assert _process_ended(pid), f"timed-out git {pid} is still running"
