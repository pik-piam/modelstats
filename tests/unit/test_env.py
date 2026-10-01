"""Effects (read-only subset), R collation, and the FakeEffects test double.

Expected values marked "R" were obtained with R 4.6.1 / ICU 78.3 under ``LC_ALL=C.utf8`` on
2026-10-01 (``dir()``, ``sort()``, ``file.exists()``, ``Sys.glob()``); the scheduler cases
mirror ``migration/harness/fakebin/_slurm.py``.
"""

from __future__ import annotations

import datetime as dt
import os
import pwd
import sys
import time
import zoneinfo
from pathlib import Path

import pytest
from _fake_effects import DEFAULT_FROZEN, SIX, TEN, FakeCall, FakeEffects

from modelstats.env import Effects, FileStat, ProductionEffects, default_effects, r_collate_key, r_sort

# ---------------------------------------------------------------------------
# collation (R: sort() with ICU root, the order dir() uses)
# ---------------------------------------------------------------------------

R_SORTED: list[list[str]] = [
    ["a\tb", "a b", "a_b", "a-b", "a.b", "a1b", "aab", "ab"],
    ["a", "A", "ab", "aB", "Ab", "AB", "abc", "aBc", "ABC"],
    ["a", "A", "ä", "Ä", "ae", "æ", "b", "e", "E", "é", "É", "n", "ñ", "o", "ö", "Ö", "ø", "ss", "ß", "u", "U", "ü"],
    ["ab", "aB", "Ab", "äb", "ÄB"],
    ["aa", "aA", "Aa", "aä", "äa"],
    [" ", "_", "-", ".", "@", "#", "+", "<", "=", "~", "$", "1", "9", "a", "A"],
    ["a", "ab", "ab_", "ab-", "ab1", "aba", "abA", "abc", "abcd"],
    ["é", "É", "è"],
    [
        "_x", "A1", "a10", "a2", "archive", "b", "B", "C_SSP2EU-Base-rem-1", "C_SSP2EU-Base-rem-10",
        "C_SSP2EU-Base-rem-2",
        "default-AMT_2026-09-28_13.23.51", "run-rem-1", "run-rem-10", "run-rem-2", "Run1",
        "SSP2-EcBudg500-AMT_2026-09-19_01.25.14", "SSP2-EU21-NPi2025-AMT_2026-09-18_22.11.36",
        "SSP2-EU21-PkBudg650-AMT_2026-09-28_13.54.59", "SSP2-NPi-AMT_2026-08-28_22.06.57",
        "SSP2-NPi-AMT_2026-09-28_10.30.27", "SSP2-NPi2025-calibrate-AMT_2026-09-28_10.27.04",
        "SSP3-NPi2025-AMT_2026-09-28_17.12.58", "testOneRegi", "testOneRegi-AMT", "x 1", "x_1", "x-1", "x.1", "x1",
        "X1",
    ],
]  # fmt: skip

R_ASCII_ORDER = "_-,;:!?.'\"()[]{}@*/\\&#%`^+<=>|~$0123456789aAbBcCdDeEfFgGhHiIjJkKlLmMnNoOpPqQrRsStTuUvVwWxXyYzZ"


@pytest.mark.parametrize("expected", R_SORTED, ids=lambda x: x[0] + "...")
def test_r_sort_matches_icu_root(expected: list[str]) -> None:
    shuffled = sorted(expected, key=lambda s: (len(s), s), reverse=True)
    assert r_sort(shuffled) == expected


def test_r_sort_ascii_order_matches_r() -> None:
    chars = [chr(c) for c in range(33, 127)]
    assert "".join(r_sort(chars)) == R_ASCII_ORDER
    assert "".join(s[1] for s in r_sort(f"a{c}b" for c in chars)) == R_ASCII_ORDER


def test_r_sort_is_not_codepoint_order() -> None:
    assert sorted(["B", "a", "_x", "A1"]) == ["A1", "B", "_x", "a"]
    assert r_sort(["B", "a", "_x", "A1"]) == ["_x", "a", "A1", "B"]  # R


def test_r_collate_key_levels() -> None:
    assert r_collate_key("a")[0] == r_collate_key("A")[0] == r_collate_key("ä")[0]
    assert r_collate_key("a")[1] == r_collate_key("A")[1] != r_collate_key("ä")[1]
    assert r_collate_key("a")[2] != r_collate_key("A")[2]
    assert r_collate_key("æ")[0] == r_collate_key("ae")[0]
    assert r_collate_key("ß")[0] == r_collate_key("ss")[0]


# ---------------------------------------------------------------------------
# ProductionEffects
# ---------------------------------------------------------------------------

R_DIR_NAMES = [
    "B",
    "a",
    "_x",
    "x.1",
    "x-1",
    "x_1",
    "A1",
    "a10",
    "a2",
    "Z",
    "b",
    "ä",
    "config.Rdata",
    "config.Rdata.bak",
    "config.yml",
]
R_DIR_ORDER = [
    "_x", "a", "ä", "A1", "a10", "a2", "b", "B", "broken", "config.Rdata", "config.Rdata.bak", "config.yml",
    "subdir", "x_1", "x-1", "x.1", "Z",
]  # fmt: skip


@pytest.fixture
def dirtest(tmp_path: Path) -> Path:
    root = tmp_path / "dirtest"
    root.mkdir()
    for name in R_DIR_NAMES + [".hidden"]:
        (root / name).write_text("")
    (root / "subdir").mkdir()
    (root / ".hiddendir").mkdir()
    os.symlink("/nonexistent", root / "broken")
    return root


def test_listdir_like_r_matches_r_dir(dirtest: Path) -> None:
    assert ProductionEffects().listdir_like_r(dirtest) == R_DIR_ORDER
    assert ProductionEffects().listdir_like_r(str(dirtest)) == R_DIR_ORDER


def test_listdir_like_r_missing_or_file(dirtest: Path) -> None:
    effects = ProductionEffects()
    assert effects.listdir_like_r(dirtest / "nonexistent") == []  # R: character(0)
    assert effects.listdir_like_r(dirtest / "a") == []  # R: dir() of a file is character(0)


def test_exists_and_is_dir(dirtest: Path) -> None:
    effects = ProductionEffects()
    assert effects.exists(dirtest)
    assert effects.exists(str(dirtest) + "/")
    assert effects.exists(dirtest / "a")
    assert not effects.exists(dirtest / "broken")  # R: file.exists(dangling symlink) is FALSE
    assert not effects.exists(dirtest / "nonexistent")
    assert not effects.exists("")
    assert effects.is_dir(dirtest)
    assert effects.is_dir(dirtest / "subdir")
    assert not effects.is_dir(dirtest / "a")
    assert not effects.is_dir(dirtest / "nonexistent")


def test_stat(tmp_path: Path) -> None:
    path = tmp_path / "f"
    path.write_bytes(b"12345")
    os.utime(path, (1_700_000_000, 1_790_762_400.25))
    st = ProductionEffects().stat(path)
    assert isinstance(st, FileStat)
    assert st.size == 5
    assert st.mtime == pytest.approx(1_790_762_400.25)
    assert st.ctime == os.stat(path).st_ctime
    mtime, ctime, size = st
    assert (mtime, size) == (st.mtime, 5)
    with pytest.raises(FileNotFoundError):
        ProductionEffects().stat(tmp_path / "missing")


def test_read_text_and_bytes(tmp_path: Path) -> None:
    path = tmp_path / "f"
    path.write_bytes(b"ok \xc3\xa4 \xff end\n")
    effects = ProductionEffects()
    assert effects.read_bytes(path) == b"ok \xc3\xa4 \xff end\n"
    text = effects.read_text(path)
    assert text == "ok ä \udcff end\n"
    assert text.encode("utf-8", "surrogateescape") == b"ok \xc3\xa4 \xff end\n"


def test_glob_like_sys_glob(dirtest: Path) -> None:
    effects = ProductionEffects()
    # R: Sys.glob("dirtest/*1") -> A1, x-1, x.1, x_1 (code-point order, unlike dir())
    assert [Path(p).name for p in effects.glob(dirtest / "*1")] == ["A1", "x-1", "x.1", "x_1"]
    assert effects.glob(dirtest / "*hidden*") == []
    assert effects.glob(dirtest / "zzz*") == []
    assert effects.glob(str(dirtest / "sub*")) == [str(dirtest / "subdir")]


def test_tilde_is_expanded_like_path_expand(tmp_path: Path, monkeypatch: pytest.MonkeyPatch) -> None:
    # R: dir(), file.exists(), file.info(), Sys.glob() and file connections go through path.expand()
    monkeypatch.setenv("HOME", str(tmp_path))
    (tmp_path / "d").mkdir()
    (tmp_path / "d" / "f.txt").write_text("hello\n", encoding="utf-8")
    effects = ProductionEffects()
    assert effects.listdir_like_r("~/d") == ["f.txt"]
    assert effects.exists("~/d/f.txt")
    assert effects.is_dir("~/d")
    assert effects.stat("~/d/f.txt").size == 6
    assert effects.read_text("~/d/f.txt") == "hello\n"
    assert effects.read_bytes("~/d/f.txt") == b"hello\n"
    assert effects.glob("~/d/*") == [str(tmp_path / "d" / "f.txt")]  # Sys.glob returns the expanded paths
    cwd = effects.run([sys.executable, "-c", "import os; print(os.getcwd())"], cwd="~/d").stdout.strip()
    assert Path(cwd).resolve() == (tmp_path / "d").resolve()
    # path.expand() touches a leading tilde only: "a~/b" stays as it is
    assert not effects.exists("a~/b")
    assert effects.listdir_like_r("a~/d") == []


def test_run_captures_output_and_status() -> None:
    effects = ProductionEffects()
    result = effects.run([sys.executable, "-c", "import sys; print('out'); print('err', file=sys.stderr); sys.exit(3)"])
    assert result.returncode == 3
    assert result.stdout == "out\n"
    assert result.stderr == "err\n"


def test_run_input_cwd_and_env(tmp_path: Path) -> None:
    effects = ProductionEffects()
    code = "import os, sys; print(sys.stdin.read().upper()); print(os.getcwd()); print(os.environ.get('MODELSTATS_T'))"
    result = effects.run([sys.executable, "-c", code], cwd=tmp_path, env={"MODELSTATS_T": "x"}, input="abc")
    assert result.returncode == 0
    assert result.stdout.splitlines() == ["ABC", str(tmp_path.resolve()), "x"]
    inherited = effects.run([sys.executable, "-c", "import os; print('PATH' in os.environ)"], env={"MODELSTATS_T": "x"})
    assert inherited.stdout == "True\n"


def test_run_missing_executable_is_status_127() -> None:
    result = ProductionEffects().run(["modelstats-no-such-command-xyz", "--flag"])
    assert result.returncode == 127
    assert result.stdout == ""
    assert "command not found" in result.stderr


def test_now_today_user_cluster_env(monkeypatch: pytest.MonkeyPatch) -> None:
    effects = ProductionEffects()
    now = effects.now()
    assert now.tzinfo is not None and now.utcoffset() is not None
    assert abs(now.timestamp() - time.time()) < 60
    assert effects.today() == effects.now().date()
    assert effects.user == pwd.getpwuid(os.getuid()).pw_name
    assert effects.on_cluster == Path("/p").exists()
    monkeypatch.delenv("MODELSTATS_UNSET_XYZ", raising=False)
    assert effects.getenv("MODELSTATS_UNSET_XYZ") == ""  # R: Sys.getenv() of an unset name is ""
    assert effects.getenv("MODELSTATS_UNSET_XYZ", "d") == "d"
    monkeypatch.setenv("MODELSTATS_UNSET_XYZ", "v")
    assert effects.getenv("MODELSTATS_UNSET_XYZ") == "v"


def test_default_effects_is_a_process_wide_production_instance() -> None:
    assert default_effects() is default_effects()
    assert isinstance(default_effects(), ProductionEffects)
    assert isinstance(default_effects(), Effects)


def test_effects_is_abstract() -> None:
    with pytest.raises(TypeError):
        Effects()  # type: ignore[abstract]


# ---------------------------------------------------------------------------
# FakeEffects
# ---------------------------------------------------------------------------

SQUEUE_ALL = (
    "pascalfu /p/projects/synthetic/remind/output/running running 2:15:00 RUNNING standby\n"
    "pascalfu /p/projects/synthetic/remind/output/pend pend 0:00 PENDING standby\n"
    "alice /p/projects/landuse/tests/magpie/output/weekly weekly 1:02:03 RUNNING medium\n"
    "short line\n"
)


@pytest.fixture
def case_dir(tmp_path: Path) -> Path:
    case = tmp_path / "case"
    case.mkdir()
    (case / "squeue_all.txt").write_text(SQUEUE_ALL)
    (case / "sacct_WorkDir_pascalfu.txt").write_text("/p/projects/x\n\n\n")
    (case / "sacct_JobName_pascalfu.txt").write_text("x\nbatch\nextern\n")
    return case


def test_fake_clock_identity_and_env() -> None:
    berlin = zoneinfo.ZoneInfo("Europe/Berlin")
    fake = FakeEffects(user="alice", on_cluster=True, env={"HOME": "/h"})
    assert fake.now() == DEFAULT_FROZEN == dt.datetime(2026, 9, 30, 12, 0, tzinfo=berlin)
    assert fake.now().timestamp() == 1790762400
    assert fake.today() == dt.date(2026, 9, 30)
    assert fake.user == "alice"
    assert fake.on_cluster is True
    assert fake.getenv("HOME") == "/h"
    assert fake.getenv("PATH") == ""
    assert fake.calls == [
        FakeCall("now"),
        FakeCall("now"),
        FakeCall("today"),
        FakeCall("user"),
        FakeCall("on_cluster"),
        FakeCall("getenv", ("HOME",)),
        FakeCall("getenv", ("PATH",)),
    ]
    custom = FakeEffects(now=dt.datetime(2026, 3, 29, 1, 30, tzinfo=berlin))
    assert custom.today() == dt.date(2026, 3, 29)
    with pytest.raises(ValueError):
        FakeEffects(now=dt.datetime(2026, 1, 1))


def test_fake_defaults_match_the_harness() -> None:
    fake = FakeEffects()
    assert fake.user == "pascalfu"
    assert fake.on_cluster is False
    assert fake.run(["squeue", "-h", "-o", SIX]).stdout == ""  # no case: empty scheduler


def test_fake_filesystem_reads_are_real_and_logged(dirtest: Path) -> None:
    fake = FakeEffects()
    assert fake.listdir_like_r(dirtest) == R_DIR_ORDER
    assert fake.exists(dirtest / "a") and not fake.exists(dirtest / "broken")
    assert fake.is_dir(dirtest)
    assert fake.stat(dirtest / "a").size == 0
    assert fake.read_text(dirtest / "a") == ""
    assert fake.read_bytes(dirtest / "a") == b""
    assert fake.glob(dirtest / "sub*") == [str(dirtest / "subdir")]
    assert [c.method for c in fake.calls] == [
        "listdir_like_r",
        "exists",
        "exists",
        "is_dir",
        "stat",
        "read_text",
        "read_bytes",
        "glob",
    ]
    assert fake.calls[0].args == (str(dirtest),)


def test_fake_squeue_six_field_format(case_dir: Path) -> None:
    fake = FakeEffects(slurm_case_dir=case_dir)
    result = fake.run(["squeue", "-h", "-o", SIX])
    assert result.returncode == 0
    assert result.stdout == SQUEUE_ALL.replace("short line\n", "")
    assert fake.run(["squeue", "-u", "alice", "-h", "-o", SIX]).stdout == SQUEUE_ALL.splitlines()[2] + "\n"
    assert fake.run(["squeue", "-u", "nobody", "-h", "-o", SIX]).stdout == ""
    assert fake.runs == [
        ("squeue", "-h", "-o", SIX),
        ("squeue", "-u", "alice", "-h", "-o", SIX),
        ("squeue", "-u", "nobody", "-h", "-o", SIX),
    ]


def test_fake_squeue_derived_per_user_formats(case_dir: Path) -> None:
    fake = FakeEffects(slurm_case_dir=case_dir)
    assert fake.run(["squeue", "-u", "pascalfu", "-h", "-o", "%Z"]).stdout == (
        "/p/projects/synthetic/remind/output/running\n/p/projects/synthetic/remind/output/pend\n"
    )
    assert fake.run(["squeue", "-u", "pascalfu", "-h", "-o", "%j"]).stdout == "running\npend\n"
    ten = fake.run(["squeue", "-u", "alice", "-h", "-o", TEN]).stdout
    assert ten == (
        "100000 medium RUNNING 1 1:02:03 weekly 2026-09-30T00:00:00 N/A N/A "
        "/p/projects/landuse/tests/magpie/output/weekly\n"
    )
    assert fake.run(["squeue", "--user=alice", "-h", "--format=%j"]).stdout == "weekly\n"
    assert fake.run(["squeue", "-u", "alice", "-h", "-o%j"]).stdout == "weekly\n"


def test_fake_squeue_override_files_and_ten_all(case_dir: Path) -> None:
    (case_dir / "squeue_Z_pascalfu.txt").write_text("/override\n")
    (case_dir / "squeue_six_alice.txt").write_text("alice /x y 0:01 RUNNING q\n")
    (case_dir / "squeue_ten_all.txt").write_text(
        "1 standby RUNNING 4 2:15:00 running 2026-09-29T00:00:00 1:00 N/A "
        "/p/projects/synthetic/remind/output/running\n"
        "2 medium RUNNING 4 1:02:03 weekly 2026-09-29T00:00:00 1:00 N/A "
        "/p/projects/landuse/tests/magpie/output/weekly\n"
        "3 medium RUNNING 4 1:02:03 other 2026-09-29T00:00:00 1:00 N/A /elsewhere\n"
    )
    fake = FakeEffects(slurm_case_dir=case_dir)
    assert fake.run(["squeue", "-u", "pascalfu", "-h", "-o", "%Z"]).stdout == "/override\n"
    assert fake.run(["squeue", "-u", "alice", "-h", "-o", SIX]).stdout == "alice /x y 0:01 RUNNING q\n"
    assert fake.run(["squeue", "-u", "pascalfu", "-h", "-o", TEN]).stdout.startswith(
        "1 standby RUNNING 4 2:15:00 running "
    )
    assert fake.run(["squeue", "-h", "-o", TEN]).stdout.count("\n") == 3


def test_fake_squeue_unsupported_format(case_dir: Path) -> None:
    result = FakeEffects(slurm_case_dir=case_dir).run(["squeue", "-h", "-o", "%i"])
    assert result.returncode == 1
    assert result.stdout == ""
    assert "unsupported format" in result.stderr


def test_fake_sacct(case_dir: Path) -> None:
    fake = FakeEffects(slurm_case_dir=case_dir)
    base = ["sacct", "-u", "pascalfu", "-s", "cd,f,cancelled,timeout,oom", "-S", "2026-09-25", "-E", "now", "-P", "-n"]
    assert fake.run([*base, "--format", "WorkDir"]).stdout == "/p/projects/x\n\n\n"
    assert fake.run([*base, "--format", "JobName"]).stdout == "x\nbatch\nextern\n"
    assert fake.run([*base[:2], "alice", *base[3:], "--format", "WorkDir"]).stdout == ""


def test_fake_exit_control_and_sequence(case_dir: Path) -> None:
    (case_dir / "sacct_exit").write_text("1\n")
    (case_dir / "squeue_sequence").write_text("1\n1\n0\n")
    fake = FakeEffects(slurm_case_dir=case_dir)
    failed = fake.run(["sacct", "-u", "pascalfu", "--format", "WorkDir"])
    assert (failed.returncode, failed.stdout) == (1, "")
    assert failed.stderr == "sacct: error: fake sacct failure (exit 1)\n"
    statuses = [fake.run(["squeue", "-h", "-o", SIX]).returncode for _ in range(5)]
    assert statuses == [1, 1, 0, 0, 0]  # the last line of the sequence repeats


def test_fake_recorded_capture(tmp_path: Path) -> None:
    rec = tmp_path / "recorded"
    rec.mkdir()
    (rec / "01.txt.cmd").write_text("squeue -h -o '%u %Z %j %M %T %q'   (foundInSlurm, all users)\n")
    (rec / "01.txt").write_text("u1 /w j 0:01 RUNNING q\nu2 /v k 0:02 PENDING r\n")
    (rec / "06.txt.cmd").write_text(
        "sacct -u pascalfu -s cd,f,cancelled,timeout,oom -S 2026-09-25 -E now -P -n --format WorkDir\n"
    )
    (rec / "06.txt").write_text("/recorded/workdir\n")
    fake = FakeEffects(slurm_case_dir=rec)
    assert fake.run(["squeue", "-h", "-o", SIX]).stdout == "u1 /w j 0:01 RUNNING q\nu2 /v k 0:02 PENDING r\n"
    sacct = [
        "sacct",
        "-u",
        "pascalfu",
        "-s",
        "cd,f,cancelled,timeout,oom",
        "-S",
        "2026-09-27",
        "-E",
        "now",
        "-P",
        "-n",
        "--format",
        "WorkDir",
    ]
    assert fake.run(sacct).stdout == "/recorded/workdir\n"  # the -S date is wildcarded
    assert fake.run([*sacct[:-1], "JobName"]).stdout == ""  # not recorded, no synthetic file
    # derived from 01.txt when no NN.cmd matches
    assert fake.run(["squeue", "-u", "u2", "-h", "-o", "%Z"]).stdout == "/v\n"


def test_fake_run_table_and_unknown_tool(case_dir: Path) -> None:
    fake = FakeEffects(slurm_case_dir=case_dir, run_table={("git", "log", "-1"): ("commit abc\n", 0)})
    assert fake.run(["git", "log", "-1"]).stdout == "commit abc\n"
    other = fake.run(["git", "status"])
    assert other.returncode == 127 and other.stdout == ""
    assert fake.run(["/usr/bin/squeue", "-h", "-o", "%j"]).stdout == "running\npend\nweekly\n"
    assert fake.calls[-1] == FakeCall("run", (("/usr/bin/squeue", "-h", "-o", "%j"), None, None, None))
