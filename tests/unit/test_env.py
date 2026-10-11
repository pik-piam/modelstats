"""``modelstats.env``: R semantics of the real calls, the overlay and canned scheduler of ``FakeEffects``.

The literal expectations come from R 4.6.1 under ``LC_ALL=C.utf8`` (probe of 2026-10-01; the R expression
is quoted next to each). The tests on ``tmp_path`` trees and on ``migration/cases`` run everywhere; those
on the gitignored fixture snapshot and the synthetic layer skip where the trees are absent
(``tests/conftest.py``). The mutating part (phase 5, ``06`` sections 3.2 and 4.4) is tested on temporary
trees: atomic writes, ``sed -i``, the file operations, streamed subprocesses, a loopback webhook, the lock
held by another process, ``CannedAmt`` against ``migration/harness/fakebin`` and ``migration/cases/amt``,
``RecordingEffects`` against the fake binaries' ``trace.jsonl`` of a golden case, and ``DryRunEffects``
against a tree digest.
"""

from __future__ import annotations

import contextlib
import hashlib
import json
import os
import pwd
import shlex
import socket
import stat
import subprocess
import sys
import threading
import time as wallclock
from collections.abc import Callable, Iterator
from dataclasses import dataclass
from datetime import datetime, time, timedelta
from http.server import BaseHTTPRequestHandler, HTTPServer
from pathlib import Path, PurePosixPath
from typing import Any
from zoneinfo import ZoneInfo

import pytest

from modelstats.env import (
    DRY_RUN_COMMIT,
    DRY_RUN_RESPONSE_BODY,
    INVALID_WEBHOOK_URL,
    NOT_A_WEBHOOK_URL,
    SELECT_SCENARIOS_SCRIPT,
    CannedAmt,
    CannedSlurm,
    ClusterPaths,
    CommandFailedError,
    DryRunEffects,
    EffectCall,
    Effects,
    FakeEffects,
    HttpResponse,
    LockHeldError,
    Mode,
    OverlayPaths,
    PostRecord,
    ProductionEffects,
    RecordingEffects,
    RunResult,
    Stream,
    TraceRecord,
    sort_like_r,
)
from tests.conftest import (
    CASES,
    RECORDED_SLURM,
    SANDBOX_TZ,
    SANDBOX_USER,
    SYNTHETIC,
    GoldenCase,
    golden_cases,
    make_case_effects,
    read_case_rows,
    resolve_frozen,
    slurm_case_dir,
)

BERLIN = ZoneInfo("Europe/Berlin")
NOW = datetime(2026, 9, 30, 12, 0, 0, tzinfo=BERLIN)
SIX = "%u %Z %j %M %T %q"
TEN = "%i %q %T %C %M %j %V %L %e %Z"
SACCT_PASCALFU = [
    "-u",
    "pascalfu",
    "-s",
    "cd,f,cancelled,timeout,oom",
    "-S",
    "2026-09-25",
    "-E",
    "now",
    "-P",
    "-n",
]

# A directory holding these names (the four of R_PROBE_DIRS as directories, the rest as files):
# dir(d) and sort(c(...)) agree, list.dirs(d, recursive = FALSE, full.names = FALSE) keeps the dot-directory,
# dir(d, pattern = "^a") is case-sensitive, basename(Sys.glob(file.path(d, "*"))) is byte order without
# dot-names, Sys.glob(file.path(d, ".h*")) finds both hidden entries.
R_PROBE_DIRS = [".hiddendir", "Dir1", "dir2", "Run1"]
R_PROBE_FILES = [
    "_x", "ä", "a1", "A1", "a10", "a2", "b", "B", "x_1", "x-1", "x.1", "Zed", ".hidden", "a b", "é", "E",
    "e", "~t", "1", "10", "2", "a-b", "a_b", "a.b", "ab", "A-B",
]  # fmt: skip
R_DIR = [
    "_x", "~t", "1", "10", "2", "ä", "a b", "a_b", "a-b", "A-B", "a.b", "a1", "A1", "a10", "a2", "ab", "b",
    "B", "Dir1", "dir2", "e", "E", "é", "Run1", "x_1", "x-1", "x.1", "Zed",
]  # fmt: skip
R_LIST_DIRS = [".hiddendir", "Dir1", "dir2", "Run1"]
R_DIR_PATTERN_A = ["a b", "a_b", "a-b", "a.b", "a1", "a10", "a2", "ab"]
R_GLOB = [
    "1", "10", "2", "A-B", "A1", "B", "Dir1", "E", "Run1", "Zed", "_x", "a b", "a-b", "a.b", "a1", "a10",
    "a2", "a_b", "ab", "b", "dir2", "e", "x-1", "x.1", "x_1", "~t", "ä", "é",
]  # fmt: skip
# sort() of the printable ASCII punctuation: the root collation's symbol order.
R_PUNCTUATION = list(" _-,;:!?.'\"()[]{}@*/\\&#%`^+<=>|~$")

# The same R calls on the canonical snapshot (dir(), dir(pattern = "NPi"), basename(list.dirs()), and
# sort(unique(c(dir(<fixture dir>), dir(<synthetic dir>)))) for the directory both layers hold).
FIXTURE_RUN = "projects/remind/modeltests/remind/output/SSP2-NPi-AMT_2026-09-28_10.30.27"
FIXTURE_OUTPUT = "projects/remind/modeltests/remind/output"
FIXTURE_MODELTESTS = "projects/remind/modeltests/remind"
R_FIXTURE_RUN_DIR = [
    "cfg.txt", "config.Rdata", "full.gms", "full.log", "fulldata.gdx", "historical.mif", "log.txt",
    "prepareAndRun.R", "projectSummations.rds", "REMIND_generic_SSP2-NPi-AMT_withoutPlus.mif",
    "REMIND_generic_SSP2-NPi-AMT.mif", "runstatistics.rda",
]  # fmt: skip
R_FIXTURE_OUTPUT_NPI = [
    "SSP2-EU21-NPi2025-AMT_2026-09-18_22.11.36", "SSP2-NPi-AMT_2026-08-28_22.06.57",
    "SSP2-NPi-AMT_2026-09-28_10.30.27", "SSP2-NPi2025-calibrate-AMT_2026-09-28_10.27.04",
    "SSP3-NPi2025-AMT_2026-09-28_17.12.58",
]  # fmt: skip
R_FIXTURE_OUTPUT_DIRS = [
    "archive", "default-AMT_2026-09-28_13.23.51", "export", "gamscompile",
    "SSP2-EcBudg500-AMT_2026-09-19_01.25.14", "SSP2-EU21-NPi2025-AMT_2026-09-18_22.11.36",
    "SSP2-EU21-PkBudg650-AMT_2026-09-28_13.54.59", "SSP2-NPi-AMT_2026-08-28_22.06.57",
    "SSP2-NPi-AMT_2026-09-28_10.30.27", "SSP2-NPi2025-calibrate-AMT_2026-09-28_10.27.04",
    "SSP3-NPi2025-AMT_2026-09-28_17.12.58", "testOneRegi", "testOneRegi-AMT",
]  # fmt: skip
R_UNION_MODELTESTS = [
    "config", "lastcommit.rds", "magpie", "main.gms", "Makefile", "modules", "output", "output.R",
    "runcode.rds", "runsToStart.rds", "runtime.rds", "scripts", "start.R", "tests",
]  # fmt: skip


def printed(path: Path) -> str:
    """What the fake prints when it re-emits a file line by line (every line newline-terminated)."""
    return "".join(f"{line}\n" for line in path.read_text().splitlines())


def write(path: Path, text: str) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(text)


# ---------------------------------------------------------------------------------------------
# Base R collation
# ---------------------------------------------------------------------------------------------


def test_sort_like_r_reproduces_the_r_probe() -> None:
    names = [n for n in R_PROBE_FILES + R_PROBE_DIRS if not n.startswith(".")]
    assert sort_like_r(names) == R_DIR


def test_sort_like_r_punctuation_order() -> None:
    assert sort_like_r(reversed(R_PUNCTUATION)) == R_PUNCTUATION


def test_sort_like_r_levels() -> None:
    # primary: digits before letters, shorter prefix first; secondary: accents after; tertiary: lower first
    assert sort_like_r(["é", "E", "e"]) == ["e", "E", "é"]
    assert sort_like_r(["a b", "ä", "A"]) == ["A", "ä", "a b"]
    assert sort_like_r(["a2", "a10", "A1", "a1"]) == ["a1", "A1", "a10", "a2"]


# R 4.6.1, LC_ALL=C.utf8 (icuGetCollate() == "root"): dir() of a directory holding these names (plus
# ".hidden", which dir() omits) returned exactly this order; the configuration-file names are the ones
# the unanchored regex of R/getRunStatus.R:41 sees (BUG-003).
R_DIR_CONFIG_NAMES = [
    "_x", "-a", "1", "10", "2", "a", "A", "ä", "a-b", "ab", "b", "B", "config.Rdata", "config.Rdata.bak",
    "config.yml", "Config.yml", "config.yml~", "old_config.yml", "sub.dir",
]  # fmt: skip
# R 4.6.1, same locale: sort(c("a b", "a_b", "a-b", "a1b", "aab", "aAb", "a", "A", "ab", "aB", "Ab", "AB",
#   "äb", "ab1", "ab10", "ab2", "abc", "ab-c", "ab.c"))
R_SORT_SAMPLE = [
    "a", "A", "a b", "a_b", "a-b", "a1b", "aab", "aAb", "ab", "aB", "Ab", "AB", "äb", "ab-c", "ab.c", "ab1",
    "ab10", "ab2", "abc",
]  # fmt: skip


def test_listdir_like_r_reproduces_the_config_name_probe(tmp_path: Path) -> None:
    for name in R_DIR_CONFIG_NAMES + [".hidden"]:
        if name == "sub.dir":
            (tmp_path / name).mkdir()
        else:
            (tmp_path / name).write_text("", encoding="utf-8")
    assert ProductionEffects().listdir_like_r(tmp_path) == R_DIR_CONFIG_NAMES
    assert ProductionEffects().listdir_like_r(str(tmp_path)) == R_DIR_CONFIG_NAMES


def test_sort_like_r_reproduces_the_mixed_case_sample() -> None:
    assert sort_like_r(reversed(R_SORT_SAMPLE)) == R_SORT_SAMPLE


# ---------------------------------------------------------------------------------------------
# ProductionEffects: the R semantics of the real calls
# ---------------------------------------------------------------------------------------------


@pytest.fixture
def probe_tree(tmp_path: Path) -> Path:
    """The directory of the R probe."""
    tree = tmp_path / "d"
    for name in R_PROBE_DIRS:
        (tree / name).mkdir(parents=True)
    for name in R_PROBE_FILES:
        (tree / name).write_text(name)
    return tree


@pytest.fixture
def production() -> ProductionEffects:
    environ = {"TZ": "Europe/Berlin", "PATH": os.environ.get("PATH", "/usr/bin:/bin"), "FOO": "bar"}
    return ProductionEffects(environ=environ)


def test_listdir_like_r_skips_dot_names_and_sorts_like_r(
    production: ProductionEffects, probe_tree: Path
) -> None:
    assert production.listdir_like_r(probe_tree) == R_DIR


def test_listdir_like_r_full_names_pastes_a_slash(production: ProductionEffects, probe_tree: Path) -> None:
    # dir(paste0(d, "/"), full.names = TRUE)[1:2] gives "<d>//_x" "<d>//~t"
    assert production.listdir_like_r(f"{probe_tree}/", full_names=True)[:2] == [
        f"{probe_tree}//_x",
        f"{probe_tree}//~t",
    ]
    assert production.listdir_like_r(probe_tree, full_names=True)[0] == f"{probe_tree}/_x"


def test_listdir_like_r_pattern_is_a_case_sensitive_regex(
    production: ProductionEffects, probe_tree: Path
) -> None:
    assert production.listdir_like_r(probe_tree, pattern="^a") == R_DIR_PATTERN_A


def test_listdir_like_r_of_a_file_or_a_missing_path_is_empty(
    production: ProductionEffects, probe_tree: Path
) -> None:
    assert production.listdir_like_r(probe_tree / "a1") == []
    assert production.listdir_like_r(probe_tree / "nope") == []


def test_list_dirs_like_r_keeps_dot_directories(production: ProductionEffects, probe_tree: Path) -> None:
    assert production.list_dirs_like_r(probe_tree, full_names=False) == R_LIST_DIRS
    assert production.list_dirs_like_r(f"{probe_tree}/")[0] == f"{probe_tree}//.hiddendir"


def test_glob_is_byte_ordered_and_skips_dot_names(production: ProductionEffects, probe_tree: Path) -> None:
    assert [os.path.basename(p) for p in production.glob(probe_tree / "*")] == R_GLOB
    assert len(production.glob(probe_tree / ".h*")) == 2
    assert production.glob(probe_tree / "nope*") == []


def test_stat_is_file_info_in_the_session_zone(production: ProductionEffects, probe_tree: Path) -> None:
    info = production.stat(probe_tree / "a10")
    assert info is not None
    assert (info.size, info.is_dir) == (3, False)
    assert info.mtime.tzinfo == BERLIN and info.ctime.tzinfo == BERLIN
    assert info.mtime_epoch == os.stat(probe_tree / "a10").st_mtime
    directory = production.stat(probe_tree / "Dir1")
    assert directory is not None and directory.is_dir
    assert production.stat(probe_tree / "nope") is None


def test_normalize_path_resolves_existing_paths_only(production: ProductionEffects, probe_tree: Path) -> None:
    link = probe_tree / "link"
    link.symlink_to(probe_tree / "a1")
    assert production.normalize_path(link) == str((probe_tree / "a1").resolve())
    assert production.normalize_path("does/not/exist") == "does/not/exist"


def test_is_symlink_is_the_link_itself_never_followed(
    production: ProductionEffects, probe_tree: Path
) -> None:
    """``find -P`` semantics for the realization sweep: the link is reported, its target is not."""
    link = probe_tree / "link-to-dir"
    link.symlink_to(probe_tree / "Dir1")
    assert production.is_symlink(link) and production.is_dir(link)
    assert not production.is_symlink(probe_tree / "Dir1") and not production.is_symlink(probe_tree / "nope")
    assert "link-to-dir" in production.list_dirs_like_r(probe_tree, full_names=False)  # list.dirs follows
    recorder = RecordingEffects(now=NOW, user="amt", on_cluster=False, cwd=probe_tree)
    assert recorder.is_symlink("link-to-dir") and not recorder.is_symlink("Dir1")
    assert recorder.calls[-2:] == [
        EffectCall("is_symlink", ("link-to-dir",)),
        EffectCall("is_symlink", ("Dir1",)),
    ]
    fake = FakeEffects(now=NOW, user="amt", on_cluster=False, cwd=probe_tree)
    assert fake.is_symlink("link-to-dir") and not fake.is_symlink("Dir1") and not fake.is_symlink("")
    dry = DryRunEffects(recorder)
    assert dry.is_symlink(link) and not dry.is_symlink(probe_tree / "a1")


def test_exists_is_dir_and_readers(production: ProductionEffects, probe_tree: Path) -> None:
    assert production.exists(probe_tree / "a1") and production.exists(probe_tree / "Dir1")
    assert not production.exists(probe_tree / "nope")
    assert production.is_dir(probe_tree / "Dir1") and not production.is_dir(probe_tree / "a1")
    assert production.read_text(probe_tree / "ä") == "ä"
    assert production.read_bytes(probe_tree / "ä") == "ä".encode()
    with production.open_binary(probe_tree / "a b") as stream:
        assert stream.read() == b"a b"


def test_run_captures_both_streams_and_the_status(production: ProductionEffects, tmp_path: Path) -> None:
    result = production.run(["sh", "-c", "echo out; echo err >&2; pwd; exit 3"], cwd=tmp_path)
    assert result.returncode == 3 and not result.ok
    assert result.lines() == ["out", str(tmp_path.resolve())]
    assert result.stderr == "err\n"
    with pytest.raises(CommandFailedError) as failure:
        production.run(["sh", "-c", "exit 2"], check=True)
    assert failure.value.result.returncode == 2


def test_run_env_overrides_the_inherited_variables(production: ProductionEffects) -> None:
    assert production.run(["sh", "-c", "echo $FOO"]).stdout == "bar\n"
    assert production.run(["sh", "-c", "echo $FOO"], env={"FOO": "baz"}).stdout == "baz\n"


def test_run_missing_program_is_status_127(production: ProductionEffects) -> None:
    result = production.run(["no-such-program-modelstats"])
    assert result.returncode == 127 and "command not found" in result.stderr
    assert RunResult(("x",), 0, "", "").lines() == []


def test_run_result_lines_split_on_newline_only() -> None:
    # intern = TRUE splits on \n only (printf 'a\rb\n' is one element in R 4.6.1), keeps an unterminated
    # last line and cuts each element at its first NUL (BUG-011)
    assert RunResult(("x",), 0, "a\rb\nc", "").lines() == ["a\rb", "c"]
    assert RunResult(("x",), 0, "ab\0cd\nef\n", "").lines() == ["ab", "ef"]


def test_the_empty_path_exists_nowhere(production: ProductionEffects, layers: Layers) -> None:
    # file.exists("") and dir.exists("") are FALSE in R 4.6.1; file.info("") is the NA row
    assert not production.exists("") and not production.is_dir("") and production.stat("") is None
    fake = make_fake(layers)
    assert not fake.exists("") and not fake.is_dir("") and fake.stat("") is None


def test_user_is_the_passwd_name_of_the_real_uid(production: ProductionEffects) -> None:
    assert production.user == pwd.getpwuid(os.getuid()).pw_name


def test_on_cluster_is_the_p_probe(production: ProductionEffects) -> None:
    assert production.on_cluster is os.path.exists("/p")


def test_now_is_aware_in_the_tz_variable(production: ProductionEffects) -> None:
    now = production.now()
    assert now.tzinfo == BERLIN and now.utcoffset() is not None
    assert ProductionEffects(environ={"TZ": "no/such/zone"}).now().tzinfo is not None


def test_getenv_and_the_magpie_archive(production: ProductionEffects) -> None:
    assert production.getenv("FOO") == "bar" and production.getenv("UNSET_VARIABLE") == ""
    assert production.magpie_results_archive == ""
    archive = ProductionEffects(environ={"MAGPIE_RESULTS_ARCHIVE_PATH": "/p/magpie"}).magpie_results_archive
    assert archive == "/p/magpie"


def test_cluster_paths_are_the_r_constants(production: ProductionEffects) -> None:
    paths = production.cluster_paths
    assert paths == ClusterPaths()
    assert paths.amt_output == "/p/projects/remind/modeltests/remind/output/"
    assert paths.amt_archive == "/p/projects/remind/modeltests/remind/output/archive"
    assert paths.amt_runcode == "/p/projects/remind/modeltests/remind/runcode.rds"
    assert paths.remind_results_archive == "/p/projects/rd3mod/models/results/remind/"
    assert production.cwd == os.getcwd()


# ---------------------------------------------------------------------------------------------
# FakeEffects: overlay paths with the sandbox semantics
# ---------------------------------------------------------------------------------------------


@dataclass(frozen=True)
class Layers:
    fixtures_p: Path
    synthetic_p: Path


@pytest.fixture
def layers(tmp_path: Path) -> Layers:
    """Two physical layers that overlap: the fixtures must win, the synthetic layer only add."""
    fx = tmp_path / "fixtures" / "p"
    sy = tmp_path / "synthetic" / "p"
    write(fx / "projects/remind/output/run1/config.Rdata", "cfg")
    write(fx / "projects/remind/output/run1/log.txt", "fixture log")
    write(fx / "projects/remind/output/run1/.remote_dir_mtime", "")
    (fx / "projects/remind/output/run2").mkdir(parents=True)
    write(fx / "projects/shared/both.txt", "fixture")
    write(fx / "projects/blocker", "a file in the fixtures")
    write(sy / "projects/shared/both.txt", "synthetic")
    write(sy / "projects/shared/only-synthetic.txt", "")
    write(sy / "projects/remind/output/run1/log.txt", "synthetic log")
    (sy / "projects/remind/output/syn-run").mkdir(parents=True)
    write(sy / "projects/synthetic/remind/output/running/log.txt", "")
    write(sy / "projects/blocker/inside.txt", "")
    return Layers(fx, sy)


def make_fake(layers: Layers, mode: Mode = Mode.ONCLUSTER, cwd: str | None = None) -> FakeEffects:
    return FakeEffects(
        now=NOW,
        user=SANDBOX_USER,
        on_cluster=mode.on_cluster,
        paths=OverlayPaths.sandbox(layers.fixtures_p, layers.synthetic_p, mode),
        slurm=CannedSlurm.from_case_dir(CASES / "slurm" / "running-amt"),
        cwd=cwd,
    )


def test_sandbox_overlay_stacks_fixtures_over_synthetic(layers: Layers, tmp_path: Path) -> None:
    overlay = OverlayPaths.sandbox(layers.fixtures_p, layers.synthetic_p, Mode.ONCLUSTER)
    assert overlay.logical_root == PurePosixPath("/p")
    assert overlay.layers == (layers.fixtures_p, layers.synthetic_p)
    assert OverlayPaths.sandbox(layers.fixtures_p, tmp_path / "unbuilt", Mode.LOCAL).layers == (
        layers.fixtures_p,
    )
    assert OverlayPaths.sandbox(layers.fixtures_p, None, Mode.LOCAL).logical_root == PurePosixPath(
        "/fixtures/p"
    )


def test_fixtures_win_over_synthetic_on_a_conflict(layers: Layers) -> None:
    effects = make_fake(layers)
    assert effects.read_text("/p/projects/shared/both.txt") == "fixture"
    assert effects.read_text("/p/projects/remind/output/run1/log.txt") == "fixture log"
    info = effects.stat("/p/projects/shared/both.txt")
    assert info is not None and info.size == len("fixture")
    assert info.mtime_epoch == os.stat(layers.fixtures_p / "projects/shared/both.txt").st_mtime
    assert info.mtime.tzinfo == BERLIN


def test_parent_components_return_to_the_merged_directory(layers: Layers, tmp_path: Path) -> None:
    """``..`` restores every layer merged at the parent, as overlayfs resolves it (phase 1 Codex finding 1).

    ``projects/synthetic`` exists in the synthetic layer only; walking into it and back must not drop the
    fixture layer (``bwrap --overlay`` in ``sandbox.sh``: ``/p/projects/synthetic/../remind/runs`` exists
    like ``/p/projects/remind/runs``, and ``ls`` of a ``..`` path lists both layers). A missing component
    before ``..`` stays ``ENOENT``, a file before ``..`` stays ``ENOTDIR``, and ``..`` at the overlay root
    climbs onto the real filesystem.
    """
    paths = OverlayPaths.sandbox(layers.fixtures_p, layers.synthetic_p, Mode.ONCLUSTER)
    via_synthetic = PurePosixPath("/p/projects/synthetic/../remind/output/run1/log.txt")
    assert paths.first_existing(via_synthetic) == layers.fixtures_p / "projects/remind/output/run1/log.txt"
    shared_via_synthetic = PurePosixPath("/p/projects/synthetic/../shared")
    assert paths.merged_names(shared_via_synthetic) == {"both.txt", "only-synthetic.txt"}
    assert paths.merged_names(shared_via_synthetic) == paths.merged_names(PurePosixPath("/p/projects/shared"))
    assert paths.first_existing(PurePosixPath("/p/projects/missing/../remind")) is None
    assert paths.first_existing(PurePosixPath("/p/projects/blocker/../shared")) is None
    outside = PurePosixPath("/p/..").joinpath(*tmp_path.parts[1:])
    assert paths.first_existing(outside) == tmp_path
    effects = make_fake(layers)
    assert effects.exists(via_synthetic)
    assert effects.read_text(via_synthetic) == "fixture log"
    assert effects.listdir_like_r(shared_via_synthetic) == ["both.txt", "only-synthetic.txt"]
    assert not effects.exists("/p/projects/missing/../remind")


def test_listings_are_the_union_of_the_layers(layers: Layers) -> None:
    effects = make_fake(layers)
    assert effects.listdir_like_r("/p/projects/shared") == ["both.txt", "only-synthetic.txt"]
    assert effects.listdir_like_r("/p/projects/remind/output") == ["run1", "run2", "syn-run"]
    assert effects.listdir_like_r("/p/projects/remind/output/run1") == ["config.Rdata", "log.txt"]
    assert effects.list_dirs_like_r("/p/projects/remind/output", full_names=False) == [
        "run1",
        "run2",
        "syn-run",
    ]
    assert effects.listdir_like_r("/p/projects/remind/output", pattern="^run") == ["run1", "run2"]
    assert effects.listdir_like_r("/p/projects/nowhere") == []


def test_synthetic_only_paths_resolve(layers: Layers) -> None:
    effects = make_fake(layers)
    assert effects.exists("/p/projects/synthetic/remind/output/running/log.txt")
    assert effects.is_dir("/p/projects/synthetic/remind/output/running")
    assert effects.read_bytes("/p/projects/shared/only-synthetic.txt") == b""
    with effects.open_binary("/p/projects/remind/output/run1/config.Rdata") as stream:
        assert stream.read() == b"cfg"
    assert not effects.exists("/p/projects/missing")
    with pytest.raises(FileNotFoundError):
        effects.read_text("/p/projects/missing")


def test_displayed_paths_keep_the_logical_form(layers: Layers) -> None:
    effects = make_fake(layers)
    assert effects.listdir_like_r("/p/projects/shared", full_names=True) == [
        "/p/projects/shared/both.txt",
        "/p/projects/shared/only-synthetic.txt",
    ]
    assert effects.list_dirs_like_r("/p/projects/remind/output/") == [
        "/p/projects/remind/output//run1",
        "/p/projects/remind/output//run2",
        "/p/projects/remind/output//syn-run",
    ]
    assert effects.glob("/p/projects/*/output/*") == [
        "/p/projects/remind/output/run1",
        "/p/projects/remind/output/run2",
        "/p/projects/remind/output/syn-run",
    ]
    assert effects.glob("/p/projects/remind/output/*/log.txt") == ["/p/projects/remind/output/run1/log.txt"]
    assert effects.glob("/p/projects/remind/output/*/*remote*") == []
    assert effects.glob("/p/projects/remind/output/*/.remote*") == [
        "/p/projects/remind/output/run1/.remote_dir_mtime"
    ]
    assert effects.normalize_path("/p/projects/shared/both.txt") == "/p/projects/shared/both.txt"
    assert effects.normalize_path("/p/projects/missing") == "/p/projects/missing"


def test_a_fixture_file_hides_a_synthetic_directory(layers: Layers) -> None:
    effects = make_fake(layers)
    assert effects.exists("/p/projects/blocker") and not effects.is_dir("/p/projects/blocker")
    assert effects.listdir_like_r("/p/projects/blocker") == []
    assert not effects.exists("/p/projects/blocker/inside.txt")


def test_local_mode_maps_the_fixtures_root(layers: Layers) -> None:
    effects = make_fake(layers, Mode.LOCAL)
    assert effects.on_cluster is False and effects.cwd == "/fixtures/p"
    assert effects.exists("/fixtures/p/projects/shared/both.txt")
    assert effects.read_text("/fixtures/p/projects/shared/both.txt") == "fixture"
    assert effects.listdir_like_r("/fixtures/p/projects/shared", full_names=True)[0] == (
        "/fixtures/p/projects/shared/both.txt"
    )
    assert effects.paths is not None
    assert effects.paths.candidates(PurePosixPath("/p/projects/shared")) == (Path("/p/projects/shared"),)


def test_physical_path_is_the_overlay_entry_or_the_logical_path(layers: Layers) -> None:
    """What a library opens: the fixture layer's file, a relative path under the logical cwd, or the path."""
    effects = make_fake(layers)
    shown = layers.fixtures_p / "projects" / "shared" / "both.txt"
    assert effects.physical_path("/p/projects/shared/both.txt") == shown
    assert effects.physical_path("projects/shared/both.txt") == shown  # the logical cwd is /p
    assert effects.physical_path("/p/projects/shared/absent") == Path("/p/projects/shared/absent")
    assert effects.calls == []  # no probe is logged: the open that follows is the library's


def test_paths_outside_the_root_pass_through(layers: Layers, tmp_path: Path) -> None:
    effects = make_fake(layers)
    assert effects.exists(tmp_path) and effects.is_dir(tmp_path)
    assert effects.listdir_like_r(tmp_path) == ["fixtures", "synthetic"]
    assert effects.glob(tmp_path / "f*") == [str(tmp_path / "fixtures")]


def test_relative_paths_resolve_against_the_logical_cwd(layers: Layers) -> None:
    effects = make_fake(layers, cwd="/p/projects/remind/output")
    assert effects.cwd == "/p/projects/remind/output"
    assert effects.listdir_like_r(".") == ["run1", "run2", "syn-run"]
    assert effects.listdir_like_r("run1", full_names=True) == ["run1/config.Rdata", "run1/log.txt"]
    assert effects.exists("run1/log.txt") and effects.is_dir("syn-run")
    assert effects.normalize_path("run1") == "/p/projects/remind/output/run1"
    assert effects.glob("*/log.txt") == ["/p/projects/remind/output/run1/log.txt"]


def test_fake_without_an_overlay_uses_the_real_filesystem(probe_tree: Path) -> None:
    effects = FakeEffects(now=NOW, user="someone", on_cluster=False, cwd=probe_tree)
    assert effects.listdir_like_r(".") == R_DIR
    assert effects.list_dirs_like_r(".", full_names=False) == R_LIST_DIRS
    assert effects.normalize_path("a1") == str(probe_tree / "a1")
    assert effects.stat("nope") is None and effects.listdir_like_r("nope") == []


def test_injected_identity_clock_and_variables(layers: Layers) -> None:
    effects = FakeEffects(now=NOW, user="alice", on_cluster=True, environ={"USER": "alice"})
    assert (effects.user, effects.on_cluster, effects.now()) == ("alice", True, NOW)
    assert effects.getenv("USER") == "alice" and effects.getenv("UNSET") == ""
    assert effects.magpie_results_archive == ""
    assert effects.cluster_paths == ClusterPaths()
    with pytest.raises(ValueError, match="aware"):
        FakeEffects(now=datetime(2026, 9, 30, 12), user="alice", on_cluster=True)


def test_every_call_is_logged(layers: Layers) -> None:
    effects = make_fake(layers)
    effects.exists("/p/projects/shared/both.txt")
    effects.read_text("/p/projects/shared/both.txt")
    effects.run(["squeue", "-h", "-o", SIX])
    assert effects.calls[:2] == [
        EffectCall("exists", ("/p/projects/shared/both.txt",)),
        EffectCall("read_text", ("/p/projects/shared/both.txt",)),
    ]
    assert effects.runs == [EffectCall("run", ("squeue", "-h", "-o", SIX), 0)]
    assert effects.runs[0].tool == "squeue"


# ---------------------------------------------------------------------------------------------
# CannedSlurm: synthetic cases under migration/cases/slurm (tracked, always available)
# ---------------------------------------------------------------------------------------------


def case(name: str) -> CannedSlurm:
    return CannedSlurm.from_case_dir(CASES / "slurm" / name)


def test_six_field_dump_is_the_all_users_file() -> None:
    result = case("running-amt").answer("squeue", ["-h", "-o", SIX])
    assert result.returncode == 0 and result.stderr == ""
    assert result.stdout == printed(CASES / "slurm" / "running-amt" / "squeue_all.txt")
    assert result.argv == ("squeue", "-h", "-o", SIX)
    assert case("running-amt").answer("squeue", ["-u", "alice", "-h", "-o", SIX]).stdout == ""


def test_workdir_and_jobname_formats_filter_by_user() -> None:
    canned = case("current-pascalfu")
    assert canned.answer("squeue", ["-u", "alice", "-h", "-o", "%Z"]).lines() == [
        "/p/projects/landuse/tests/magpie/output/weeklyTests_SSP1-Ref",
        "/p/projects/remind/modeltests/remind/output/testOneRegi",
    ]
    assert canned.answer("squeue", ["-u", "alice", "-h", "-o", "%j"]).lines() == [
        "weeklyTests_SSP1-Ref",
        "testOneRegi",
    ]
    assert canned.answer("squeue", ["-u", "pascalfu", "-h", "-o", "%j"]).lines()[:3] == [
        "SSP2-NPi-AMT",
        "default-AMT",
        "default-AMT",
    ]
    assert canned.answer("squeue", ["-u", "bob", "-h", "-o", "%Z"]).stdout == ""


def test_ten_field_dump_is_synthesised_from_the_six_field_rows() -> None:
    assert case("current-pascalfu").answer("squeue", ["-u", "alice", "-h", "-o", TEN]).lines() == [
        "100000 standby RUNNING 1 3:00:00 weeklyTests_SSP1-Ref 2026-09-30T00:00:00 N/A N/A"
        " /p/projects/landuse/tests/magpie/output/weeklyTests_SSP1-Ref",
        "100001 standby PENDING 1 0:00 testOneRegi 2026-09-30T00:00:00 N/A N/A"
        " /p/projects/remind/modeltests/remind/output/testOneRegi",
    ]


def test_sacct_answers_from_the_per_user_field_file() -> None:
    canned = case("current-pascalfu")
    workdir = canned.answer("sacct", [*SACCT_PASCALFU, "--format", "WorkDir"])
    assert workdir.stdout == (CASES / "slurm" / "current-pascalfu" / "sacct_WorkDir_pascalfu.txt").read_text()
    jobname = canned.answer("sacct", [*SACCT_PASCALFU, "--format", "JobName"])
    assert jobname.lines()[:2] == ["testOneRegi-AMT", "batch"]
    nobody = canned.answer("sacct", ["-u", "bob", "-P", "-n", "--format", "WorkDir"])
    assert nobody.returncode == 0 and nobody.stdout == ""
    assert case("running-amt").answer("sacct", [*SACCT_PASCALFU, "--format", "WorkDir"]).stdout == ""


def test_argument_spellings_are_equivalent() -> None:
    canned = case("current-pascalfu")
    reference = canned.answer("squeue", ["-u", "alice", "-h", "-o", "%Z"]).stdout
    assert canned.answer("squeue", ["--user=alice", "-h", "-o%Z"]).stdout == reference
    assert canned.answer("squeue", ["-u", "alice", "-h", "--format=%Z"]).stdout == reference
    assert canned.answer("squeue", ["-u", "alice", "-h", "--format", "%Z"]).stdout == reference


def test_unsupported_format_fails_like_the_fake() -> None:
    result = case("running-amt").answer("squeue", ["-h", "-o", "%i"])
    assert result.returncode == 1 and result.stdout == ""
    assert "unsupported format '%i'" in result.stderr


def test_exit_sequence_is_consumed_per_call_and_per_tool() -> None:
    canned = case("amt-squeue-3fail")
    statuses = [canned.answer("squeue", ["-h", "-o", SIX]).returncode for _ in range(5)]
    assert statuses == [1, 1, 1, 0, 0]
    failing = case("amt-squeue-3fail").answer("squeue", ["-h", "-o", SIX])
    assert failing.stdout == "" and failing.stderr == "squeue: error: fake squeue failure (exit 1)\n"
    assert canned.answer("sacct", [*SACCT_PASCALFU, "--format", "WorkDir"]).returncode == 0


def test_fixed_exit_status_applies_to_every_call() -> None:
    canned = case("failing")
    assert canned.answer("squeue", ["-h", "-o", SIX]).returncode == 1
    assert canned.answer("squeue", ["-h", "-o", SIX]).returncode == 1
    assert canned.answer("sacct", [*SACCT_PASCALFU, "--format", "WorkDir"]).returncode == 1


def test_empty_scheduler_and_missing_case_directory(tmp_path: Path) -> None:
    empty = CannedSlurm.empty()
    assert empty.answer("squeue", ["-h", "-o", "%i"]) == RunResult(("squeue", "-h", "-o", "%i"), 0, "", "")
    assert empty.answer("sacct", ["--format", "WorkDir"]).stdout == ""
    with pytest.raises(FileNotFoundError):
        CannedSlurm.from_case_dir(tmp_path / "no-such-case")
    bare = CannedSlurm.from_case_dir(tmp_path)  # an existing but empty case directory
    assert bare.answer("squeue", ["-h", "-o", SIX]).stdout == ""
    assert bare.answer("squeue", ["-h", "-o", "%i"]).returncode == 1


def test_fake_effects_routes_squeue_and_sacct_only(layers: Layers) -> None:
    effects = make_fake(layers)
    result = effects.run(["squeue", "-u", "pascalfu", "-h", "-o", "%Z"])
    assert "/p/projects/synthetic/remind/output/running" in result.lines()
    assert effects.run(["/usr/bin/sacct", *SACCT_PASCALFU, "--format", "WorkDir"]).stdout == ""
    with pytest.raises(NotImplementedError, match="git"):
        effects.run(["git", "status"])
    failing = FakeEffects(now=NOW, user="x", on_cluster=True, slurm=case("failing"))
    with pytest.raises(CommandFailedError):
        failing.run(["squeue", "-h", "-o", SIX], check=True)
    assert failing.runs == [EffectCall("run", ("squeue", "-h", "-o", SIX), 1)]
    with pytest.raises(ValueError, match="program"):
        failing.run([])


# ---------------------------------------------------------------------------------------------
# CannedSlurm: the recorded capture (fixture snapshot)
# ---------------------------------------------------------------------------------------------


@pytest.fixture
def recorded(fixtures_dir: Path) -> CannedSlurm:
    return CannedSlurm.from_case_dir(fixtures_dir / "_meta" / "slurm" / "latest")


def test_recorded_requests_answer_verbatim(recorded: CannedSlurm) -> None:
    assert (
        recorded.answer("squeue", ["-u", "pascalfu", "-h", "-o", "%Z"]).stdout
        == (RECORDED_SLURM / "03.txt").read_text()
    )
    assert recorded.answer("squeue", ["-u", "pascalfu", "-h", "-o", "%j"]).lines() == ["piam-runmerge"]
    assert recorded.answer("squeue", ["-h", "-o", SIX]).stdout == (RECORDED_SLURM / "01.txt").read_text()
    assert (
        recorded.answer("squeue", ["-u", "pascalfu", "-h", "-o", TEN]).stdout
        == (RECORDED_SLURM / "05.txt").read_text()
    )
    assert (
        recorded.answer("squeue", ["-u", "tobiasdi", "-h", "-o", "%Z"]).stdout
        == (RECORDED_SLURM / "13.txt").read_text()
    )


def test_recorded_sacct_wildcards_the_start_date(recorded: CannedSlurm) -> None:
    argv = [*SACCT_PASCALFU, "--format", "WorkDir"]
    argv[argv.index("2026-09-25")] = "2026-09-01"
    assert recorded.answer("sacct", argv).stdout == (RECORDED_SLURM / "06.txt").read_text()
    argv[-1] = "JobName"
    assert recorded.answer("sacct", argv).stdout == (RECORDED_SLURM / "07.txt").read_text()
    argv[argv.index("now")] = "2026-09-30"  # a different -E value is another request: nothing recorded
    assert recorded.answer("sacct", argv).stdout == ""


def test_unrecorded_user_is_derived_from_the_all_users_dump(recorded: CannedSlurm) -> None:
    rows = [line.split(" ") for line in (RECORDED_SLURM / "01.txt").read_text().splitlines()]
    user = rows[0][0]
    workdirs = [fields[1] for fields in rows if fields[0] == user]
    assert workdirs
    assert recorded.answer("squeue", ["-u", user, "-h", "-o", "%Z"]).lines() == workdirs
    ten = recorded.answer("squeue", ["-u", user, "-h", "-o", TEN]).lines()
    assert ten and all(line.split(" ")[-1] in workdirs for line in ten)
    assert all(line in (RECORDED_SLURM / "02.txt").read_text().splitlines() for line in ten)
    assert recorded.answer("squeue", ["-u", "nobody-in-the-capture", "-h", "-o", "%Z"]).stdout == ""


# ---------------------------------------------------------------------------------------------
# FakeEffects on the canonical snapshot (and the synthetic layer where built)
# ---------------------------------------------------------------------------------------------


@pytest.fixture
def snapshot(fixtures_dir: Path) -> FakeEffects:
    overlay = OverlayPaths.sandbox(fixtures_dir / "p", SYNTHETIC / "p", Mode.ONCLUSTER)
    return FakeEffects(now=NOW, user=SANDBOX_USER, on_cluster=True, paths=overlay)


def test_dir_on_a_fixture_run_skips_the_dot_file(snapshot: FakeEffects, fixtures_dir: Path) -> None:
    assert (fixtures_dir / "p" / FIXTURE_RUN / ".remote_dir_mtime").exists()
    assert snapshot.listdir_like_r(f"/p/{FIXTURE_RUN}") == R_FIXTURE_RUN_DIR
    assert snapshot.listdir_like_r(f"/p/{FIXTURE_RUN}", pattern="config.Rdata|config.yml") == ["config.Rdata"]


def test_dir_pattern_and_list_dirs_on_the_fixture_output(snapshot: FakeEffects) -> None:
    assert snapshot.listdir_like_r(f"/p/{FIXTURE_OUTPUT}", pattern="NPi") == R_FIXTURE_OUTPUT_NPI
    assert snapshot.list_dirs_like_r(f"/p/{FIXTURE_OUTPUT}", full_names=False) == R_FIXTURE_OUTPUT_DIRS
    assert snapshot.list_dirs_like_r(f"/p/{FIXTURE_OUTPUT}")[0] == f"/p/{FIXTURE_OUTPUT}/archive"
    assert "gRS.rds" in snapshot.listdir_like_r(f"/p/{FIXTURE_OUTPUT}")


def test_union_listing_with_the_synthetic_layer(
    snapshot: FakeEffects, fixtures_dir: Path, synthetic_dir: Path
) -> None:
    assert (synthetic_dir / "p" / FIXTURE_MODELTESTS).is_dir()
    assert snapshot.listdir_like_r(f"/p/{FIXTURE_MODELTESTS}") == R_UNION_MODELTESTS
    info = snapshot.stat(f"/p/{FIXTURE_MODELTESTS}")
    assert info is not None and info.is_dir
    assert info.mtime_epoch == os.stat(fixtures_dir / "p" / FIXTURE_MODELTESTS).st_mtime
    assert snapshot.is_dir("/p/projects/synthetic/remind/output/running")
    assert snapshot.normalize_path(f"/p/{FIXTURE_MODELTESTS}/output/") == f"/p/{FIXTURE_MODELTESTS}/output"


# ---------------------------------------------------------------------------------------------
# tests/conftest.py: golden cases and their FakeEffects
# ---------------------------------------------------------------------------------------------


def test_read_case_rows_parses_like_make_goldens(tmp_path: Path) -> None:
    path = tmp_path / "x.tsv"
    path.write_text("# comment\nid\tdir\tnote\n\nr1\tprojects/a\n# c\nr2\tprojects/b\tnote b\textra\n")
    assert read_case_rows(path) == [
        {"id": "r1", "dir": "projects/a", "note": ""},
        {"id": "r2", "dir": "projects/b", "note": "note b"},
    ]
    (tmp_path / "empty.tsv").write_text("# nothing\n")
    with pytest.raises(ValueError, match="header"):
        read_case_rows(tmp_path / "empty.tsv")


def test_golden_cases_resolve_the_sandbox_defaults() -> None:
    cases = golden_cases("slurm")
    first = cases[0]
    assert first.id == "running--running-amt" and first.family == "slurm"
    assert first.mode is Mode.ONCLUSTER and first.fixture_root == "/p" and first.cwd == "/p"
    assert first.user == "pascalfu" and first.slurm_case == "running-amt"
    assert first.frozen == datetime(2026, 9, 30, 12, tzinfo=SANDBOX_TZ)
    assert first.logical_path(first.row["dir"]) == "/p/projects/synthetic/remind/output/running"
    assert first.logical_path("/p/absolute") == "/p/absolute" and first.logical_path("") == ""
    assert {c.id: c for c in cases}["running--user-alice--otheruser"].user == "alice"


def test_golden_cases_apply_mode_cwd_frozen_and_slurm_columns() -> None:
    rs = {c.id: c for c in golden_cases("rs")}
    assert rs["cwd-run--local"].mode is Mode.LOCAL
    assert rs["cwd-run--local"].cwd == f"/fixtures/p/{FIXTURE_RUN}"
    assert rs["amt"].cwd == "/p" and rs["amt--current"].slurm_case == "current-pascalfu"
    looprun = {c.id: c for c in golden_cases("looprun")}
    assert looprun["live-runtime--local--later"].frozen == datetime(2026, 10, 15, 0, 0, tzinfo=SANDBOX_TZ)
    assert looprun["live-runtime--local--later"].logical_paths("dirs") == [
        "/fixtures/p/projects/landuse/tests/magpie/output/v39k_FSECc_BAU"
    ]
    assert looprun["errors--oncluster"].logical_paths("dirs")[-1] == "/p/projects/does/not/exist"
    assert looprun["empty--oncluster"].logical_paths("dirs") == []
    assert golden_cases("status")[0].mode is Mode.ONCLUSTER
    assert golden_cases("status", Mode.LOCAL)[0].mode is Mode.LOCAL


def test_resolve_frozen_and_slurm_case_dir() -> None:
    assert resolve_frozen("2026-10-15 00:00:00") == datetime(2026, 10, 15, 0, 0, tzinfo=SANDBOX_TZ)
    assert slurm_case_dir("recorded") == RECORDED_SLURM
    assert slurm_case_dir("pending") == CASES / "slurm" / "pending"


def test_resolve_frozen_ctime_preset(fixtures_dir: Path) -> None:
    ctime = datetime.fromtimestamp(os.stat(fixtures_dir / "p" / "projects").st_ctime, tz=SANDBOX_TZ)
    expected = datetime.combine((ctime + timedelta(days=3)).date(), time(12), tzinfo=SANDBOX_TZ)
    assert resolve_frozen("ctime+3d") == expected


def test_make_case_effects_matches_the_sandbox_settings(fixtures_dir: Path) -> None:
    effects = make_case_effects({c.id: c for c in golden_cases("slurm")}["running--user-alice--otheruser"])
    assert (effects.user, effects.on_cluster, effects.cwd) == ("alice", True, "/p")
    assert effects.now() == datetime(2026, 9, 30, 12, tzinfo=SANDBOX_TZ)
    assert effects.getenv("USER") == "alice" and effects.getenv("TZ") == "Europe/Berlin"
    assert effects.magpie_results_archive == "/p/projects/rd3mod/models/results/magpie"
    assert effects.exists(f"/p/{FIXTURE_RUN}/config.Rdata")
    squeue = effects.run(["squeue", "-h", "-o", SIX])
    assert squeue.stdout == printed(CASES / "slurm" / "otheruser" / "squeue_all.txt")


def test_case_effects_fixture_builds_a_local_mode_case(
    case_effects: Callable[[GoldenCase], FakeEffects],
) -> None:
    effects = case_effects({c.id: c for c in golden_cases("rs")}["cwd-run--local"])
    assert effects.on_cluster is False and effects.cwd == f"/fixtures/p/{FIXTURE_RUN}"
    assert effects.listdir_like_r(".") == R_FIXTURE_RUN_DIR
    assert effects.normalize_path(".") == f"/fixtures/p/{FIXTURE_RUN}"
    assert effects.run(["squeue", "-u", "pascalfu", "-h", "-o", "%Z"]).lines() == ["/home/pascalfu"]


# ---------------------------------------------------------------------------------------------
# The mutating part (06 3.2, 4.1, 4.2): atomic writes, sed -i, copy, move, rename, delete, mkdir
# ---------------------------------------------------------------------------------------------

SED_FORCE_DOWNLOAD = ("cfg$force_download <- FALSE", "cfg$force_download <- TRUE")
"""The two literals of ``sed -i 's/.../.../' config/default.cfg`` (``R/modeltests.R:100``)."""
WEBHOOK_PATH = "/hooks/fake-amt-token"
WEBHOOK = "https://mattermost.example.org" + WEBHOOK_PATH


def tree_digest(root: Path) -> str:
    """Hash every path, mode and content below ``root``: equal digests mean an untouched tree."""
    digest = hashlib.sha256()
    for path in sorted(root.rglob("*")):
        digest.update(f"{path.relative_to(root).as_posix()}\0{path.lstat().st_mode:o}\0".encode())
        if path.is_symlink():
            digest.update(os.readlink(path).encode())
        elif path.is_file():
            digest.update(path.read_bytes())
        digest.update(b"\0")
    return digest.hexdigest()


def temp_files(directory: Path) -> list[str]:
    """List what an interrupted atomic write could leave behind next to its target."""
    return sorted(p.name for p in directory.iterdir() if p.suffix == ".tmp")


def test_write_text_replaces_the_file_atomically_and_keeps_its_mode(
    production: ProductionEffects, tmp_path: Path
) -> None:
    target = tmp_path / "amt-state.json"
    target.write_text('{"phase": "idle"}')
    target.chmod(0o640)
    production.write_text(target, '{"phase": "started"}\n')
    assert target.read_text() == '{"phase": "started"}\n'
    assert stat.S_IMODE(target.stat().st_mode) == 0o640
    assert temp_files(tmp_path) == []


def test_write_creates_a_missing_file_with_the_umask_mode(
    production: ProductionEffects, tmp_path: Path
) -> None:
    target = tmp_path / "runcode.rds"
    previous = os.umask(0o027)
    try:
        production.write_bytes(target, b"\x1f\x8b\x00")
    finally:
        os.umask(previous)
    assert target.read_bytes() == b"\x1f\x8b\x00"
    assert stat.S_IMODE(target.stat().st_mode) == 0o640
    production.write_text(target, "é\udcff")
    assert target.read_bytes() == b"\xc3\xa9\xff"  # surrogateescape round-trips undecodable bytes


@pytest.mark.parametrize(
    ("step", "failure"),
    [("replace", KeyboardInterrupt()), ("fsync", OSError("No space left on device"))],
    ids=["interrupted-rename", "failed-flush"],
)
def test_an_interrupted_atomic_write_leaves_the_old_file_and_no_temp_file(
    production: ProductionEffects,
    tmp_path: Path,
    monkeypatch: pytest.MonkeyPatch,
    step: str,
    failure: BaseException,
) -> None:
    target = tmp_path / "amt-state.json"
    target.write_text('{"phase": "idle"}')

    def interrupted(*args: object, **kwargs: object) -> None:
        raise failure

    monkeypatch.setattr(os, step, interrupted)
    with pytest.raises(type(failure)):
        production.write_text(target, '{"phase": "started"}')
    monkeypatch.undo()
    assert target.read_text() == '{"phase": "idle"}'
    assert temp_files(tmp_path) == []


def test_replace_in_file_is_sed_i_on_the_first_occurrence_per_line(
    production: ProductionEffects, tmp_path: Path
) -> None:
    cfg = tmp_path / "default.cfg"
    cfg.write_bytes(
        b"cfg$force_download <- FALSE\n"
        b"# cfg$force_download <- FALSE and cfg$force_download <- FALSE\n"
        b'cfg$title <- "\xff"\n'
        b"cfg$force_download <- FALSE"
    )
    assert production.replace_in_file(cfg, *SED_FORCE_DOWNLOAD) == 3
    assert cfg.read_bytes() == (
        b"cfg$force_download <- TRUE\n"
        b"# cfg$force_download <- TRUE and cfg$force_download <- FALSE\n"
        b'cfg$title <- "\xff"\n'
        b"cfg$force_download <- TRUE"
    )


def test_replace_in_file_rewrites_the_file_even_without_a_match(
    production: ProductionEffects, tmp_path: Path
) -> None:
    cfg = tmp_path / "default.cfg"
    cfg.write_text("cfg$force_download <- TRUE\n")
    os.utime(cfg, (1_000_000_000, 1_000_000_000))
    assert production.replace_in_file(cfg, *SED_FORCE_DOWNLOAD) == 0
    assert cfg.read_text() == "cfg$force_download <- TRUE\n"
    assert cfg.stat().st_mtime > 1_000_000_000  # touched like sed -i (the R golden's effects.json)
    assert production.replace_in_file(cfg, "", "x") == 0  # an empty pattern matches nothing


def test_copy_move_rename_delete_tree_and_mkdir(production: ProductionEffects, tmp_path: Path) -> None:
    readme = tmp_path / "README.md"
    readme.write_text("results")
    readme.chmod(0o600)
    report = tmp_path / "testing_suite"
    production.mkdir(report / "nested")  # recursive
    production.mkdir(report)  # an existing directory is fine
    production.copy(readme, report)  # file.copy into a directory
    (report / "nested" / "README.md").write_text("stale")
    production.copy(readme, report / "nested" / "README.md")  # overwrite = TRUE
    assert (report / "README.md").read_text() == (report / "nested" / "README.md").read_text() == "results"
    assert stat.S_IMODE((report / "README.md").stat().st_mode) == 0o600
    run = tmp_path / "output" / "SSP2-NPi-AMT_2026-06-01_00.00.00"
    run.mkdir(parents=True)
    (run / "full.log").write_text("*** Status: Normal completion\n")
    archive = tmp_path / "output" / "archive"
    production.mkdir(archive)
    production.move(run, archive)  # mv <run> archive
    assert (archive / run.name / "full.log").exists() and not run.exists()
    log = tmp_path / "test-full.log"
    log.write_text("new")
    dated = tmp_path / "tests" / "test-full-2026-09-30.log"
    production.mkdir(dated.parent)
    dated.write_text("old")
    production.rename(log, dated)  # file.rename replaces the existing target
    assert dated.read_text() == "new" and not log.exists()
    production.delete_tree(archive)
    production.delete_tree(dated)
    production.delete_tree(tmp_path / "missing")
    assert not archive.exists() and not dated.exists()


# ---------------------------------------------------------------------------------------------
# run: line-by-line streaming (06 3.3), post_json over a loopback webhook, the flock
# ---------------------------------------------------------------------------------------------

STREAMING_CHILD = (
    "import sys\n"
    "print('a'); sys.stdout.flush()\n"
    "print('b', file=sys.stderr); sys.stderr.flush()\n"
    "sys.stdout.write('c\\rd\\n'); sys.stdout.write('tail')\n"
    "sys.exit(3)\n"
)


def test_run_streams_every_line_to_the_callback_and_keeps_the_text(production: ProductionEffects) -> None:
    seen: list[tuple[Stream, str]] = []
    result = production.run(
        [sys.executable, "-c", STREAMING_CHILD], on_line=lambda stream, line: seen.append((stream, line))
    )
    assert result.returncode == 3 and not result.ok
    assert [line for stream, line in seen if stream is Stream.STDOUT] == ["a", "c\rd", "tail"]
    assert [line for stream, line in seen if stream is Stream.STDERR] == ["b"]
    assert (result.stdout, result.stderr) == ("a\nc\rd\ntail", "b\n")
    assert result.lines() == ["a", "c\rd", "tail"]


def test_run_check_raises_with_the_result_and_the_argument_rules(
    production: ProductionEffects, tmp_path: Path
) -> None:
    with pytest.raises(CommandFailedError, match="exited with status 3") as info:
        production.run([sys.executable, "-c", STREAMING_CHILD], check=True)
    assert info.value.result.stdout == "a\nc\rd\ntail"
    with pytest.raises(ValueError, match="capture=True"):
        production.run(["true"], capture=False, on_line=lambda stream, line: None)
    with pytest.raises(ValueError, match="needs a program"):
        production.run([])
    with pytest.raises(FileNotFoundError):
        production.run(["true"], cwd=tmp_path / "missing")


def test_run_without_capture_inherits_the_streams(
    production: ProductionEffects, capfd: pytest.CaptureFixture[str]
) -> None:
    result = production.run([sys.executable, "-c", "print('inherited')"], capture=False)
    assert result == RunResult((sys.executable, "-c", "print('inherited')"), 0, "", "")
    assert capfd.readouterr().out == "inherited\n"


def test_a_failing_callback_still_drains_the_child(production: ProductionEffects) -> None:
    many = "for i in range(20000): print('x' * 100)\n"  # well beyond the pipe buffer

    def boom(stream: Stream, line: str) -> None:
        raise RuntimeError("callback failed")

    with pytest.raises(RuntimeError, match="callback failed"):
        production.run([sys.executable, "-c", many], on_line=boom)


@dataclass
class Webhook:
    url: str
    status: int
    received: list[tuple[str, str, bytes]]


@pytest.fixture
def webhook() -> Iterator[Webhook]:
    """A loopback HTTP server recording every POST (path, content type, body); ``status`` is its answer."""
    hook = Webhook("", 200, [])

    class Handler(BaseHTTPRequestHandler):
        def do_POST(self) -> None:  # noqa: N802 - the http.server hook name
            body = self.rfile.read(int(self.headers.get("Content-Length", "0")))
            hook.received.append((self.path, self.headers.get("Content-Type", ""), body))
            self.send_response(hook.status)
            self.send_header("Content-Type", "application/json")
            self.end_headers()
            self.wfile.write(b'{"message":"boom"}' if hook.status >= 400 else b"ok")

        def log_message(self, format: str, *args: Any) -> None:
            pass

    server = HTTPServer(("127.0.0.1", 0), Handler)
    thread = threading.Thread(target=server.serve_forever, daemon=True)
    thread.start()
    hook.url = f"http://127.0.0.1:{server.server_port}{WEBHOOK_PATH}"
    try:
        yield hook
    finally:
        server.shutdown()
        server.server_close()
        thread.join()


def test_post_json_posts_the_payload_as_json(production: ProductionEffects, webhook: Webhook) -> None:
    payload = {"text": "Summary: AMT runs look good.\n```\nrs -A\n```\nSSP2-NPi-AMT: 2.3 hours, 26/100 Iter"}
    assert production.post_json(webhook.url, payload) == HttpResponse(webhook.url, 200, "ok")
    assert webhook.received == [
        (WEBHOOK_PATH, "application/json", json.dumps(payload, ensure_ascii=False).encode())
    ]


def test_post_json_reports_http_and_transport_failures_without_raising(
    production: ProductionEffects, webhook: Webhook
) -> None:
    webhook.status = 500
    failed = production.post_json(webhook.url, {"text": "x"})
    assert failed == HttpResponse(webhook.url, 500, '{"message":"boom"}') and not failed.ok
    with socket.socket() as probe:
        probe.bind(("127.0.0.1", 0))
        closed = f"http://127.0.0.1:{probe.getsockname()[1]}{WEBHOOK_PATH}"
    refused = production.post_json(closed, {"text": "x"})
    assert refused.status == 0 and not refused.ok and "refused" in refused.body.lower()


@pytest.mark.parametrize(
    ("url", "body"),
    [
        ("not-a-url", NOT_A_WEBHOOK_URL),
        ("file:///etc/hostname", NOT_A_WEBHOOK_URL),
        ("mailto:x@y", NOT_A_WEBHOOK_URL),
        ("data:text/plain,hi", NOT_A_WEBHOOK_URL),
        ("", NOT_A_WEBHOOK_URL),
        ("http://", "no host given"),
        ("http://[::1", f"{INVALID_WEBHOOK_URL} (ValueError)"),
        ("http://exa mple.invalid/hook", f"{INVALID_WEBHOOK_URL} (InvalidURL)"),
        ("http://127.0.0.1/hook\x00", f"{INVALID_WEBHOOK_URL} (InvalidURL)"),
    ],
)
def test_post_json_reports_malformed_urls_without_raising(
    production: ProductionEffects, url: str, body: str
) -> None:
    """A misconfigured ``$AMT_MATTERMOST_URL`` is a failed exchange, never an exception (R's curl warned)."""
    response = production.post_json(url, {"text": "x"})
    assert response == HttpResponse(url, 0, body) and not response.ok
    assert url == "" or url not in response.body


LOCK_HOLDER = (
    "import fcntl, os, sys\n"
    "fd = os.open(sys.argv[1], os.O_RDWR | os.O_CREAT, 0o644)\n"
    "fcntl.flock(fd, fcntl.LOCK_EX)\n"
    "print('locked', flush=True)\n"
    "sys.stdin.read()\n"  # released when the parent closes our stdin
    "os.close(fd)\n"
)


@contextlib.contextmanager
def held_by_another_process(path: Path) -> Iterator[Callable[[], None]]:
    """Hold an exclusive flock on ``path`` in a child process; the yielded function releases it."""
    child = subprocess.Popen(
        [sys.executable, "-c", LOCK_HOLDER, str(path)],
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        text=True,
    )
    assert child.stdout is not None and child.stdin is not None
    assert child.stdout.readline() == "locked\n"
    stdin = child.stdin

    def release() -> None:
        if not stdin.closed:
            stdin.close()
            child.wait(timeout=30)

    try:
        yield release
    finally:
        release()
        child.stdout.close()


def test_lock_contention_fails_fast_without_wait(production: ProductionEffects, tmp_path: Path) -> None:
    path = tmp_path / "amt-state.lock"
    with held_by_another_process(path):
        with pytest.raises(LockHeldError, match="amt-state.lock") as info, production.lock(path, wait=False):
            pytest.fail("the lock was taken although another process holds it")
        assert isinstance(info.value, BlockingIOError) and info.value.path == str(path)
    with production.lock(path, wait=False):
        pass
    assert path.exists()  # created when missing, never deleted


def test_lock_waits_until_the_other_process_releases(production: ProductionEffects, tmp_path: Path) -> None:
    path = tmp_path / "amt-state.lock"
    released_at: list[float] = []
    with held_by_another_process(path) as release:

        def release_later() -> None:
            wallclock.sleep(0.3)
            released_at.append(wallclock.monotonic())
            release()

        releaser = threading.Thread(target=release_later)
        releaser.start()
        with production.lock(path):
            acquired_at = wallclock.monotonic()
        releaser.join()
    assert released_at and acquired_at >= released_at[0]


# ---------------------------------------------------------------------------------------------
# CannedAmt: the fake binaries' answers (harness/fakebin/_lib.sh, git, sbatch, rsync, make, Rscript, curl)
# ---------------------------------------------------------------------------------------------

GIT_LOG_1_DEFAULT = (
    "commit 3f5e2a1b9c8d7e6f5a4b3c2d1e0f9a8b7c6d5e4f\n"
    "Author: AMT Bot <amt@example.org>\n"
    "Date:   Mon Sep 28 10:00:00 2026 +0200\n"
    "\n"
    "    Merge pull request #999 from example/feature\n"
)


def test_canned_amt_defaults_are_the_fake_binaries_defaults() -> None:
    amt = CannedAmt.empty()
    assert amt.answer("git", ["log", "-1"]) == RunResult(("git", "log", "-1"), 0, GIT_LOG_1_DEFAULT, "")
    merges = amt.answer("git", ["log", "--merges", "--pretty=oneline", "a..b", "--abbrev-commit"])
    assert merges is not None and merges.lines() == [
        "3f5e2a1 Merge pull request #999 from example/feature",
        "1a2b3c4 Merge pull request #998 from example/bugfix",
    ]
    for argv in (["reset", "--hard", "origin/develop"], ["pull"], ["-C", "magpie", "pull"], ["push"]):
        assert amt.answer("git", argv) == RunResult(("git", *argv), 0, "", "")
    sbatch = ["--qos=priority", "cs2.sh"]
    assert amt.answer("sbatch", sbatch) == RunResult(("sbatch", *sbatch), 0, "Submitted batch job 4242\n", "")
    rsync = ["-e", "ssh", "-av", "fulldata.gdx", "rse@rse.pik-potsdam.de:/webservice/data/x.gdx"]
    assert amt.answer("rsync", rsync) == RunResult(("rsync", *rsync), 0, "", "")
    assert amt.answer("make", ["test-full-slurm"]) == RunResult(
        ("make", "test-full-slurm"), 0, "fake make: test-full-slurm\n", ""
    )
    start = ["start.R", "--testOneRegi", "titletag=AMT"]
    assert amt.answer("Rscript", start) == RunResult(
        ("Rscript", *start), 0, "fake Rscript: start.R --testOneRegi titletag=AMT\n", ""
    )
    assert amt.answer("Rscript", ["scripts/start.R"]) is not None
    assert amt.answer("Rscript", ["-e", "source('start.R')"]) is None  # -e first: the real Rscript runs
    assert amt.answer("Rscript", ["select_scenarios.R", "AMT"]) is None  # a bridge, not a fake's business
    assert amt.answer("sed", ["-i", "s/a/b/", "x"]) is None and amt.answer("curl", ["-i"]) is None
    assert amt.http_answer(WEBHOOK) == HttpResponse(WEBHOOK, 200, "ok\n")  # printf '%s\n' of the default


def test_canned_amt_reads_the_case_files_like_the_fakes() -> None:
    git_fail = CASES / "amt" / "remind-evaluate-git-fail"
    assert CannedAmt.from_case_dir(git_fail).answer("git", ["log", "-1"]) == RunResult(
        ("git", "log", "-1"), 128, (git_fail / "git-log-1.txt").read_text(), ""
    )
    make_fail = CASES / "amt" / "remind-start-make-fail"
    amt = CannedAmt.from_case_dir(make_fail)
    assert amt.answer("make", ["test-full-slurm"]) == RunResult(
        ("make", "test-full-slurm"), 2, (make_fail / "make.txt").read_text(), ""
    )
    assert amt.answer("Rscript", ["start.R"]) == RunResult(
        ("Rscript", "start.R"), 1, "fake Rscript: start.R\n", ""
    )
    curl_fail = CannedAmt.from_case_dir(CASES / "amt" / "remind-evaluate-curlfail")
    assert curl_fail.http_answer(WEBHOOK) == HttpResponse(WEBHOOK, 500, '{"message":"boom"}\n')
    assert curl_fail.exit_status("curl") == 22
    assert CannedAmt({"x.exit": " 7\n"}).exit_status("x") == 7
    assert CannedAmt({"x.exit": "abc"}).exit_status("x") == 0
    assert CannedAmt({"curl.txt": ""}).http_answer(WEBHOOK) == HttpResponse(
        WEBHOOK, 0, "fake curl failure (exit 0)"
    )
    assert CannedAmt({"sbatch.txt": "custom\n"}).answer("sbatch", []) == RunResult(
        ("sbatch",), 0, "custom\n", ""
    )
    assert CannedAmt({"hostname.txt": "login01\n"}).answer("hostname", []) == RunResult(
        ("hostname",), 0, "login01\n", ""
    )
    with pytest.raises(FileNotFoundError, match="case directory not found"):
        CannedAmt.from_case_dir(CASES / "amt" / "no-such-case")


# ---------------------------------------------------------------------------------------------
# RecordingEffects: the trace of the fake binaries, real mutations, canned programs, the webhook record
# ---------------------------------------------------------------------------------------------

SUBPROCESS_TOOLS = frozenset({"git", "Rscript", "make", "sbatch", "rsync", "squeue", "sacct"})
"""The programs the Python AMT runs as subprocesses; the fakes' ``sed``, ``mv`` and ``curl`` are effects."""
EFFECT_TOOLS = {"sed": "replace_in_file", "mv": "move", "curl": "post_json"}


def recording(cwd: Path, case: str, slurm: CannedSlurm | None = None) -> RecordingEffects:
    return RecordingEffects(
        now=NOW,
        user=SANDBOX_USER,
        on_cluster=True,
        amt=CannedAmt.from_case_dir(CASES / "amt" / case),
        slurm=slurm,
        cwd=cwd,
    )


@pytest.mark.parametrize("case", ["remind-start", "remind-evaluate"])
def test_recording_trace_equals_the_fake_binaries_trace_of_a_case(
    goldens_dir: Path, fixtures_dir: Path, tmp_path: Path, case: str
) -> None:
    """Replaying the golden's subprocess calls leaves the lines the fakes wrote, byte for byte."""
    lines = (goldens_dir / "amt" / case / "trace.jsonl").read_text(encoding="utf-8").splitlines()
    records = [json.loads(line) for line in lines]
    assert {r["tool"] for r in records} - SUBPROCESS_TOOLS <= EFFECT_TOOLS.keys()
    replayed = [(line, r) for line, r in zip(lines, records, strict=True) if r["tool"] in SUBPROCESS_TOOLS]
    effects = recording(tmp_path, case, CannedSlurm.from_case_dir(RECORDED_SLURM))
    for _, record in replayed:
        effects.run([record["tool"], *record["argv"]], cwd=record["cwd"], env=record["env"])
    assert effects.trace_lines() == [line for line, _ in replayed]
    assert [c.tool for c in effects.runs] == [r["tool"] for _, r in replayed]
    assert [t.exit for t in effects.trace if t.tool == "squeue"] == [0] * sum(
        r["tool"] == "squeue" for r in records
    )
    assert all(t.exit is None for t in effects.trace if t.tool != "squeue")
    effects.write_trace(tmp_path / "trace.jsonl")
    assert (tmp_path / "trace.jsonl").read_text(encoding="utf-8") == "".join(
        f"{line}\n" for line, _ in replayed
    )


def test_recording_mutations_are_real_and_relative_to_the_injected_cwd(tmp_path: Path) -> None:
    checkout = tmp_path / "remind"
    write(checkout / "config" / "default.cfg", "cfg$force_download <- FALSE\n")
    effects = RecordingEffects(now=NOW, user="amt", on_cluster=False, cwd=checkout)
    effects.write_text("amt-state.json", '{"phase": "idle"}')
    effects.write_bytes("runcode.rds", b"rds")
    assert effects.replace_in_file("config/default.cfg", *SED_FORCE_DOWNLOAD) == 1
    effects.mkdir("output/archive")
    effects.copy("amt-state.json", "output/archive")
    effects.move("output/archive/amt-state.json", "output/moved.json")
    effects.rename("output/moved.json", "output/renamed.json")
    effects.delete_tree("output/archive")
    assert (checkout / "amt-state.json").read_text() == '{"phase": "idle"}'
    assert (checkout / "runcode.rds").read_bytes() == b"rds"
    assert (checkout / "config" / "default.cfg").read_text() == "cfg$force_download <- TRUE\n"
    assert (checkout / "output" / "renamed.json").read_text() == '{"phase": "idle"}'
    assert not (checkout / "output" / "archive").exists()
    assert effects.read_text("amt-state.json") == '{"phase": "idle"}' and effects.exists("runcode.rds")
    assert effects.listdir_like_r("output") == ["renamed.json"]
    assert effects.normalize_path("output") == os.path.realpath(checkout / "output")
    assert effects.glob("*.rds") == ["runcode.rds"]
    assert [c.name for c in effects.calls] == [
        "write_text", "write_bytes", "replace_in_file", "mkdir", "copy", "move", "rename", "delete_tree",
        "read_text", "exists", "listdir_like_r", "normalize_path", "glob",
    ]  # fmt: skip
    assert effects.calls[0] == EffectCall("write_text", ("amt-state.json", '{"phase": "idle"}'))
    assert effects.calls[2] == EffectCall("replace_in_file", ("config/default.cfg", *SED_FORCE_DOWNLOAD))
    assert effects.cwd == str(checkout) and effects.trace == []
    assert effects.physical_path("runcode.rds") == checkout / "runcode.rds"  # no overlay: under the cwd


def test_recording_run_streams_the_canned_answer_and_honours_check(
    tmp_path: Path, capfd: pytest.CaptureFixture[str]
) -> None:
    effects = recording(tmp_path, "remind-start-make-fail")
    seen: list[tuple[Stream, str]] = []
    result = effects.run(
        ["make", "test-full-slurm"], on_line=lambda stream, line: seen.append((stream, line))
    )
    assert result.returncode == 2 and seen == [(Stream.STDOUT, line) for line in result.lines()] and seen
    with pytest.raises(CommandFailedError, match="make test-full-slurm exited with status 2"):
        effects.run(["make", "test-full-slurm"], check=True)
    inherited = effects.run(["sbatch", "cs2.sh"], capture=False)
    assert inherited == RunResult(("sbatch", "cs2.sh"), 0, "", "")
    assert capfd.readouterr().out == "Submitted batch job 4242\n"
    squeue = effects.run(["squeue", "-h", "-o", SIX])  # CannedSlurm.empty(): success, no output
    assert squeue.stdout == "" and squeue.ok
    assert [c.status for c in effects.runs] == [2, 2, 0, 0]
    assert [t.exit for t in effects.trace] == [None, None, None, 0]  # only the slurm fake records exit


def test_recording_run_falls_back_or_fails_for_programs_without_a_fake(
    production: ProductionEffects, tmp_path: Path
) -> None:
    strict = RecordingEffects(now=NOW, user="amt", on_cluster=False, cwd=tmp_path)
    with pytest.raises(NotImplementedError, match="no canned answer for 'hostname'"):
        strict.run(["hostname"])
    assert [t.tool for t in strict.trace] == ["hostname"] and strict.runs == []
    delegating = RecordingEffects(
        now=NOW,
        user="amt",
        on_cluster=False,
        cwd=tmp_path,
        environ={"TZ": "Europe/Berlin"},
        fallback=production,
    )
    script = "import os; print(os.getcwd())"
    result = delegating.run([sys.executable, "-c", script], env={"USER": "amt"})
    assert result.lines() == [os.path.realpath(tmp_path)]
    assert delegating.trace == [
        TraceRecord(
            os.path.basename(sys.executable),
            ("-c", script),
            str(tmp_path),
            {"USER": "amt", "TZ": "Europe/Berlin"},
        )
    ]


def test_recording_post_json_answers_from_curl_txt_and_records_like_the_fake(
    goldens_dir: Path, tmp_path: Path
) -> None:
    golden = json.loads(
        (goldens_dir / "amt" / "remind-evaluate" / "mattermost.json").read_text(encoding="utf-8")
    )
    effects = recording(tmp_path, "remind-evaluate")
    payload = {
        "text": "Please find below the status of the REMIND automated model tests (AMT) of 2026-09-30."
    }
    assert effects.post_json(golden["url"], payload) == HttpResponse(golden["url"], 200, "ok\n")
    record = effects.posts[0]
    assert record == PostRecord(golden["url"], payload) and json.loads(record.raw) == payload
    assert set(record.to_json()) == {"payload", "payload_json", "url"}
    assert set(golden) - {"payload_json", "payload_json_error"} == {"payload", "url"}  # R's string: BUG-015
    assert effects.post_lines() == [json.dumps(record.to_json(), ensure_ascii=False, sort_keys=True)]
    assert effects.calls[-1] == EffectCall("post_json", (golden["url"], record.raw), 200)
    failing = recording(tmp_path, "remind-evaluate-curlfail")
    assert failing.post_json(WEBHOOK, payload) == HttpResponse(WEBHOOK, 500, '{"message":"boom"}\n')


def test_recording_sleep_never_waits_and_can_advance_the_clock(tmp_path: Path) -> None:
    frozen = RecordingEffects(now=NOW, user="amt", on_cluster=False, cwd=tmp_path)
    started = wallclock.monotonic()
    frozen.sleep(600)
    assert wallclock.monotonic() - started < 1 and frozen.now() == NOW
    advancing = RecordingEffects(now=NOW, user="amt", on_cluster=False, cwd=tmp_path, advance_on_sleep=True)
    advancing.sleep(600)
    assert advancing.now() == NOW + timedelta(seconds=600)
    assert [c for c in advancing.calls if c.name == "sleep"] == [EffectCall("sleep", ("600",))]
    with advancing.lock("amt-state.lock"):
        assert (tmp_path / "amt-state.lock").exists()
    assert advancing.calls[-1] == EffectCall("lock", ("amt-state.lock",))
    assert (advancing.user, advancing.on_cluster, advancing.getenv("X")) == ("amt", False, "")
    with pytest.raises(ValueError, match="aware"):
        RecordingEffects(now=datetime(2026, 9, 30), user="amt", on_cluster=False)


# ---------------------------------------------------------------------------------------------
# DryRunEffects: reads for real, nothing written, run, posted or locked
# ---------------------------------------------------------------------------------------------


def test_dry_run_never_writes_runs_posts_or_locks(tmp_path: Path) -> None:
    checkout = tmp_path / "remind"
    run = checkout / "output" / "SSP2-NPi-AMT_2026-06-01_00.00.00"
    write(checkout / "amt-state.json", '{"phase": "idle"}')
    write(checkout / "config" / "default.cfg", "cfg$force_download <- FALSE\n")
    write(run / "full.log", "*** Status: Normal completion\n")
    write(checkout / "test-full.log", "ok")
    cfg = checkout / "config" / "default.cfg"
    old, new = SED_FORCE_DOWNLOAD
    before = tree_digest(tmp_path)
    inner = RecordingEffects(now=NOW, user="amt", on_cluster=False, cwd=checkout)
    logged: list[str] = []
    dry = DryRunEffects(inner, log=logged.append)
    dry.write_text(checkout / "amt-state.json", '{"phase": "started"}')
    dry.write_bytes(checkout / "runcode.rds", b"rds")
    assert dry.replace_in_file(checkout / "config" / "default.cfg", *SED_FORCE_DOWNLOAD) == 1
    dry.mkdir(checkout / "output" / "archive")
    dry.copy(checkout / "README.md", tmp_path / "testing_suite")
    dry.move(run, checkout / "output" / "archive")
    dry.rename(checkout / "test-full.log", checkout / "tests" / "test-full-2026-09-30.log")
    dry.delete_tree(checkout / "modules" / "01_macro" / "emptyReal")
    squeue = dry.run(["squeue", "-u", "amt", "-h", "-o", TEN], cwd=checkout)
    log1 = dry.run(["git", "log", "-1"], cwd=checkout, check=True)
    sbatch = dry.run(["sbatch", "cs2.sh"], check=True)
    revision = dry.run(["git", "rev-parse", "HEAD"])
    response = dry.post_json(WEBHOOK, {"text": "hi"})
    dry.sleep(600)
    with dry.lock(checkout / "amt-state.lock"):
        pass
    assert tree_digest(tmp_path) == before
    assert squeue == RunResult(("squeue", "-u", "amt", "-h", "-o", TEN), 0, "", "") and squeue.lines() == []
    assert log1.ok and log1.lines()[0] == f"commit {DRY_RUN_COMMIT}"
    assert sbatch.lines() == ["Submitted batch job 0"] and revision.lines() == [DRY_RUN_COMMIT]
    assert response == HttpResponse(WEBHOOK, 200, DRY_RUN_RESPONSE_BODY)
    assert (
        dry.messages
        == logged
        == [
            f"would write {checkout / 'amt-state.json'}",
            f"would write {checkout / 'runcode.rds'}",
            f"would replace {old!r} by {new!r} in {cfg}",
            f"would create directory {checkout / 'output' / 'archive'}",
            f"would copy {checkout / 'README.md'} to {tmp_path / 'testing_suite'}",
            f"would move {run} to {checkout / 'output' / 'archive'}",
            f"would rename {checkout / 'test-full.log'} to {checkout / 'tests' / 'test-full-2026-09-30.log'}",
            f"would delete {checkout / 'modules' / '01_macro' / 'emptyReal'}",
            f"would run {shlex.join(['squeue', '-u', 'amt', '-h', '-o', TEN])} in {checkout}",
            f"would run git log -1 in {checkout}",
            "would run sbatch cs2.sh",
            "would run git rev-parse HEAD",
            "would post JSON to the webhook at https://mattermost.example.org (path redacted, 14 characters)",
            "would sleep 600 s",
            f"would lock {checkout / 'amt-state.lock'}",
        ]
    )
    assert all(WEBHOOK_PATH not in message for message in dry.messages + logged)  # the secret stays out
    assert [c.name for c in dry.calls] == [
        "write_text", "write_bytes", "replace_in_file", "mkdir", "copy", "move", "rename", "delete_tree",
        "run", "run", "run", "run", "post_json", "sleep", "lock",
    ]  # fmt: skip
    assert dry.calls[-3] == EffectCall("post_json", (WEBHOOK, '{"text": "hi"}'), None)
    assert {c.name for c in inner.calls} <= {"read_text", "now"} and inner.trace == [] and inner.posts == []
    assert dry.read_text(checkout / "config" / "default.cfg") == "cfg$force_download <- FALSE\n"
    assert not dry.exists(checkout / "runcode.rds") and dry.is_dir(run) and dry.stat(run) is not None
    assert dry.listdir_like_r(checkout) == ["amt-state.json", "config", "output", "test-full.log"]
    assert dry.list_dirs_like_r(checkout, full_names=False) == ["config", "output"]
    assert dry.glob(str(checkout / "*.log")) == [str(checkout / "test-full.log")]
    assert dry.normalize_path(checkout / "output" / "..") == os.path.realpath(checkout)
    assert dry.read_bytes(checkout / "test-full.log") == b"ok"
    with dry.open_binary(checkout / "test-full.log") as stream:
        assert stream.read() == b"ok"
    assert (dry.user, dry.on_cluster, dry.cwd, dry.now(), dry.getenv("X")) == (
        "amt",
        False,
        str(checkout),
        NOW,
        "",
    )
    assert dry.magpie_results_archive == "" and dry.cluster_paths == ClusterPaths() and dry.inner is inner
    assert dry.physical_path("runcode.rds") == inner.physical_path("runcode.rds") == checkout / "runcode.rds"


def test_dry_run_redacts_an_unparseable_webhook_url_too() -> None:
    dry = DryRunEffects(RecordingEffects(now=NOW, user="amt", on_cluster=False, cwd="/"))
    assert dry.post_json("hooks/SECRET", {"text": "x"}) == HttpResponse(
        "hooks/SECRET", 200, DRY_RUN_RESPONSE_BODY
    )
    assert dry.messages == ["would post JSON to the webhook (URL redacted) (path redacted, 13 characters)"]


def test_dry_run_answers_the_scenario_bridge_with_an_empty_selection(tmp_path: Path) -> None:
    """``Rscript <...>/select_scenarios.R <group>`` sources the checkout's R code: not run, answered empty."""
    from modelstats.amt.bridges import parse_payload

    dry = DryRunEffects(RecordingEffects(now=NOW, user="amt", on_cluster=False, cwd=tmp_path))
    script = tmp_path / "pkg" / SELECT_SCENARIOS_SCRIPT
    result = dry.run(["Rscript", str(script), "AMT"], cwd=tmp_path)
    assert result.ok and json.loads(result.lines()[-1]) == {
        "schema": 1,
        "startgroup": "AMT",
        "names": [],
        "columns": {},
    }
    selection = parse_payload(result.lines()[-1])
    assert (selection.startgroup, selection.names, selection.columns, len(selection)) == ("AMT", (), {}, 0)
    assert dry.messages == [f"would run Rscript {script} AMT in {tmp_path}"]
    assert json.loads(dry.run(["Rscript", str(script), "1"]).lines()[-1])["startgroup"] == "1"
    assert json.loads(dry.run(["Rscript", str(script)]).lines()[-1])["startgroup"] == "AMT"
    assert dry.run(["Rscript", "start.R", "startgroup=AMT"]).lines() == []  # start.R stays silent


def test_dry_run_wraps_production_by_default_and_passes_an_overlay_through(layers: Layers) -> None:
    assert isinstance(DryRunEffects().inner, ProductionEffects)
    fake = make_fake(layers)
    dry = DryRunEffects(fake)
    shared = "/p/projects/shared/both.txt"
    assert dry.physical_path(shared) == fake.physical_path(shared) and dry.on_cluster and dry.cwd == "/p"
    assert dry.read_text("/p/projects/shared/both.txt") == "fixture"
    with pytest.raises(ValueError, match="needs a program"):
        dry.run([])
    seen: list[tuple[Stream, str]] = []
    dry.run(["git", "log", "-1"], on_line=lambda stream, line: seen.append((stream, line)))
    assert seen[0] == (Stream.STDOUT, f"commit {DRY_RUN_COMMIT}") and (Stream.STDERR, "") not in seen


# ---------------------------------------------------------------------------------------------
# The protocol and FakeEffects' mutating part
# ---------------------------------------------------------------------------------------------


def test_every_implementation_satisfies_the_effects_protocol(
    production: ProductionEffects, tmp_path: Path
) -> None:
    implementations: list[Effects] = [
        production,
        FakeEffects(now=NOW, user="amt", on_cluster=False),
        RecordingEffects(now=NOW, user="amt", on_cluster=False, cwd=tmp_path),
        DryRunEffects(production),
    ]  # the annotation is the static check (mypy --strict); the loop only touches every object
    assert all(effects.cluster_paths == ClusterPaths() for effects in implementations)


def test_fake_effects_mutates_outside_the_overlay_only(layers: Layers, tmp_path: Path) -> None:
    fake = make_fake(layers)
    outside = tmp_path / "outside"
    fake.mkdir(outside)
    fake.write_text(outside / "amt-state.json", "{}")
    fake.copy("/p/projects/shared/both.txt", outside)  # the overlay's file (the fixtures win) copied out
    assert (outside / "amt-state.json").read_text() == "{}" and (
        outside / "both.txt"
    ).read_text() == "fixture"
    with pytest.raises(NotImplementedError, match="read-only"):
        fake.write_text("/p/projects/shared/both.txt", "x")
    with pytest.raises(NotImplementedError, match="read-only"):
        fake.delete_tree("projects/shared")  # relative to the logical cwd /p
    assert (layers.fixtures_p / "projects/shared/both.txt").read_text() == "fixture"
    assert fake.post_json(WEBHOOK, {"text": "hi"}) == HttpResponse(WEBHOOK, 200, "ok\n")
    fake.sleep(600)
    with fake.lock(outside / "amt-state.lock"):
        pass
    assert fake.calls[-3:] == [
        EffectCall("post_json", (WEBHOOK, '{"text": "hi"}'), 200),
        EffectCall("sleep", ("600",)),
        EffectCall("lock", (str(outside / "amt-state.lock"),)),
    ]
    with pytest.raises(NotImplementedError, match="no canned answer for 'git'"):
        fake.run(["git", "log", "-1"])
