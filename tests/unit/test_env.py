"""``modelstats.env``: R semantics of the real calls, the overlay and canned scheduler of ``FakeEffects``.

The literal expectations come from R 4.6.1 under ``LC_ALL=C.utf8`` (probe of 2026-10-01; the R expression
is quoted next to each). The tests on ``tmp_path`` trees and on ``migration/cases`` run everywhere; those
on the gitignored fixture snapshot and the synthetic layer skip where the trees are absent
(``tests/conftest.py``).
"""

from __future__ import annotations

import os
import pwd
from collections.abc import Callable
from dataclasses import dataclass
from datetime import datetime, time, timedelta
from pathlib import Path, PurePosixPath
from zoneinfo import ZoneInfo

import pytest

from modelstats.env import (
    CannedSlurm,
    ClusterPaths,
    CommandFailedError,
    EffectCall,
    FakeEffects,
    Mode,
    OverlayPaths,
    ProductionEffects,
    RunResult,
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
