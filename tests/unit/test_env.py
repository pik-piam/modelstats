"""Effects (read-only and mutating parts), R collation, DryRunEffects and the RecordingEffects double.

Expected values marked "R" were obtained with R 4.6.1 / ICU 78.3 under ``LC_ALL=C.utf8`` on
2026-10-01 (``dir()``, ``sort()``, ``file.exists()``, ``Sys.glob()``, ``dir(all.files = TRUE)``,
``list.dirs()``, ``file.copy()``, ``file.rename()``, ``unlink()``, ``dir.create()``, ``setwd()``,
``write()``, ``writeLines()``, ``system()``, ``saveRDS()`` file mode); the scheduler cases mirror
``migration/harness/fakebin/_slurm.py``. The HTTP tests talk to a loopback server started in the
test process; nothing leaves the machine.
"""

from __future__ import annotations

import datetime as dt
import hashlib
import http.server
import os
import pwd
import socket
import stat
import sys
import threading
import time
import zoneinfo
from collections.abc import Iterator
from pathlib import Path

import pytest
from _fake_effects import DEFAULT_FROZEN, SIX, TEN, FakeCall, FakeEffects, RecordingEffects, TraceEntry

from modelstats.env import (
    DRY_RUN_COMMIT,
    DryRunEffects,
    DryRunEvent,
    Effects,
    FileStat,
    ProductionEffects,
    default_effects,
    dry_run_answer,
    r_collate_key,
    r_sort,
    shell_segments,
    shell_tokens,
)
from modelstats.errors import RParityError
from modelstats.rdata_io import read_rds, scalar

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
# shell command lines
# ---------------------------------------------------------------------------


def test_shell_tokens_split_like_sh_for_the_modeltests_command_lines() -> None:
    # what the fake binaries see as argv (R: system() through /bin/sh)
    assert shell_tokens("git reset --hard origin/develop && git pull") == [
        "git", "reset", "--hard", "origin/develop", "&&", "git", "pull",
    ]  # fmt: skip
    assert shell_tokens("Rscript start.R runscripts=default submit='SLURM priority'") == [
        "Rscript", "start.R", "runscripts=default", "submit=SLURM priority",
    ]  # fmt: skip
    assert shell_tokens("squeue -u pascalfu -h -o '%i %q %T %C %M %j %V %L %e %Z'") == [
        "squeue", "-u", "pascalfu", "-h", "-o", TEN,
    ]  # fmt: skip
    wrap = 'sbatch --qos=standby --wrap="Rscript scripts/cs2/run_compareScenarios2.R outputdirs=/a,/b; mv x.pdf /a"'
    assert shell_tokens(wrap) == [
        "sbatch", "--qos=standby", "--wrap=Rscript scripts/cs2/run_compareScenarios2.R outputdirs=/a,/b; mv x.pdf /a",
    ]  # fmt: skip
    assert shell_tokens("sed -i 's/cfg$force_download <- FALSE/cfg$force_download <- TRUE/' config/default.cfg") == [
        "sed", "-i", "s/cfg$force_download <- FALSE/cfg$force_download <- TRUE/", "config/default.cfg",
    ]  # fmt: skip


def test_shell_segments_split_at_control_operators_only() -> None:
    tokens = shell_tokens("git log --merges a..b --abbrev-commit | grep 'Merge pull request'")
    assert shell_segments(tokens) == [
        ["git", "log", "--merges", "a..b", "--abbrev-commit"],
        ["grep", "Merge pull request"],
    ]
    assert shell_segments(shell_tokens("a; b && c || d & e")) == [["a"], ["b"], ["c"], ["d"], ["e"]]
    assert shell_segments(shell_tokens("mv run1 run2 archive")) == [["mv", "run1", "run2", "archive"]]
    assert shell_segments([]) == []


def test_dry_run_answer() -> None:
    stdout, status = dry_run_answer(["git", "log", "-1"])
    assert status == 0 and stdout.splitlines()[0] == f"commit {DRY_RUN_COMMIT}"
    assert dry_run_answer(shell_tokens("cd x && git log -1"))[0].startswith("commit ")
    assert dry_run_answer(shell_tokens("git log --merges a..b | grep 'Merge pull request'")) == ("", 0)
    assert dry_run_answer(["squeue", "-u", "pascalfu", "-h", "-o", TEN]) == ("", 0)
    assert dry_run_answer(["make", "test-full-slurm"]) == ("", 0)


# ---------------------------------------------------------------------------
# ProductionEffects: read-only subset
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
# R: dir(all.files = TRUE, no.. = TRUE) on the same directory (dot entries in, "." and ".." out; "_x" before ".hidden")
R_DIR_ALL_ORDER = [
    "_x", ".hidden", ".hiddendir", "a", "ä", "A1", "a10", "a2", "b", "B", "broken", "config.Rdata",
    "config.Rdata.bak", "config.yml", "subdir", "x_1", "x-1", "x.1", "Z",
]  # fmt: skip
R_LIST_DIRS = [".hiddendir", "subdir"]  # R: list.dirs(full.names = FALSE, recursive = FALSE)


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


def test_listdir_all_matches_r_all_files_and_list_dirs(dirtest: Path) -> None:
    effects = ProductionEffects()
    assert effects.listdir_all(dirtest) == R_DIR_ALL_ORDER
    assert [n for n in effects.listdir_all(dirtest) if effects.is_dir(dirtest / n)] == R_LIST_DIRS
    assert effects.listdir_all(dirtest / "a") == []  # R: character(0) for a file
    assert effects.listdir_all(dirtest / "nonexistent") == []


def test_walk_finds_files_like_find(tmp_path: Path) -> None:
    modules = tmp_path / "modules"
    for module, realizations in (("01_macro", ["a", "b"]), ("02_welfare", ["x"])):
        (modules / module).mkdir(parents=True)
        (modules / module / "module.gms").write_text("")
        for realization in realizations:
            (modules / module / realization).mkdir()
            (modules / module / realization / "realization.gms").write_text("")
    effects = ProductionEffects()
    with effects.chdir(tmp_path):
        found = sorted(
            os.path.join(dirpath, name) for dirpath, _dirs, files in effects.walk("modules/") for name in files
        )
    # find modules/ -name 'module.gms' prints these (single slash after the trailing-slash start point)
    assert [f for f in found if f.endswith("module.gms")] == [
        "modules/01_macro/module.gms",
        "modules/02_welfare/module.gms",
    ]
    assert "modules/01_macro/a/realization.gms" in found


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
    assert effects.listdir_all("~/d") == ["f.txt"]
    assert effects.exists("~/d/f.txt")
    assert effects.is_dir("~/d")
    assert effects.stat("~/d/f.txt").size == 6
    assert effects.read_text("~/d/f.txt") == "hello\n"
    assert effects.read_bytes("~/d/f.txt") == b"hello\n"
    assert effects.glob("~/d/*") == [str(tmp_path / "d" / "f.txt")]  # Sys.glob returns the expanded paths
    cwd = effects.run([sys.executable, "-c", "import os; print(os.getcwd())"], cwd="~/d").stdout.strip()
    assert Path(cwd).resolve() == (tmp_path / "d").resolve()
    effects.write_text("~/d/w.txt", "w")
    assert (tmp_path / "d" / "w.txt").read_text() == "w"
    assert effects.copy("~/d/w.txt", "~/d/c.txt") and (tmp_path / "d" / "c.txt").exists()
    effects.rename("~/d/c.txt", "~/d/r.txt")
    assert (tmp_path / "d" / "r.txt").exists()
    effects.mkdir("~/d/m")
    assert (tmp_path / "d" / "m").is_dir()
    effects.delete("~/d/m", recursive=True)
    assert not (tmp_path / "d" / "m").exists()
    with effects.chdir("~/d"):
        assert Path.cwd().resolve() == (tmp_path / "d").resolve()
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


def test_run_missing_executable_is_status_127(tmp_path: Path) -> None:
    result = ProductionEffects().run(["modelstats-no-such-command-xyz", "--flag"])
    assert result.returncode == 127
    assert result.stdout == ""
    assert "command not found" in result.stderr
    with pytest.raises(FileNotFoundError):  # a missing working directory is not a missing executable
        ProductionEffects().run([sys.executable, "-c", "pass"], cwd=tmp_path / "missing")


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


class _ReadOnlyEffects(ProductionEffects):
    """A phase 1-4 style double: only the read-only subset is implemented."""


def test_read_only_doubles_stay_instantiable_and_refuse_mutations(tmp_path: Path) -> None:
    class Stub(Effects):
        def listdir_like_r(self, directory: object) -> list[str]:
            return []

        def exists(self, path: object) -> bool:
            return False

        def is_dir(self, path: object) -> bool:
            return False

        def stat(self, path: object) -> FileStat:
            raise FileNotFoundError

        def read_text(self, path: object) -> str:
            return ""

        def read_bytes(self, path: object) -> bytes:
            return b""

        def glob(self, pattern: object) -> list[str]:
            return []

        def run(self, argv: object, cwd: object = None, env: object = None, input: object = None, **kw: object) -> None:  # type: ignore[override]  # noqa: A002
            raise NotImplementedError

        def now(self) -> dt.datetime:
            return DEFAULT_FROZEN

        def today(self) -> dt.date:
            return DEFAULT_FROZEN.date()

        @property
        def user(self) -> str:
            return "tester"

        @property
        def on_cluster(self) -> bool:
            return False

        def getenv(self, name: str, default: str = "") -> str:
            return default

    stub = Stub()  # the mutating part is not abstract: phase 1-4 doubles still instantiate
    for call in (
        lambda: stub.write_text(tmp_path / "x", "x"),
        lambda: stub.write_bytes(tmp_path / "x", b"x"),
        lambda: stub.write_rds(tmp_path / "x", "x"),
        lambda: stub.copy(tmp_path / "x", tmp_path / "y"),
        lambda: stub.rename(tmp_path / "x", tmp_path / "y"),
        lambda: stub.delete(tmp_path / "x"),
        lambda: stub.mkdir(tmp_path / "x"),
        lambda: stub.chdir(tmp_path),
        lambda: stub.getcwd(),
        lambda: stub.tempdir(),
        lambda: stub.listdir_all(tmp_path),
        lambda: stub.walk(tmp_path),
        lambda: stub.post_json("http://localhost/", "{}"),
        lambda: stub.sleep(1),
        lambda: stub.setenv("A", "b"),
    ):
        with pytest.raises(NotImplementedError, match="Stub does not implement"):
            call()


# ---------------------------------------------------------------------------
# ProductionEffects: mutating part
# ---------------------------------------------------------------------------


def _umask() -> int:
    mask = os.umask(0)
    os.umask(mask)
    return mask


def _mode(path: Path) -> int:
    return stat.S_IMODE(path.stat().st_mode)


def test_write_text_and_bytes_exact_and_append(tmp_path: Path) -> None:
    effects = ProductionEffects()
    status = tmp_path / ".testsstatus"
    effects.write_text(status, "next:evaluate\n")  # writeLines("next:evaluate", con = "../.testsstatus")
    assert status.read_bytes() == b"next:evaluate\n"  # R: 6e 65 78 74 3a 65 76 61 6c 75 61 74 65 0a
    effects.write_text(status, "next:start\n")
    assert status.read_bytes() == b"next:start\n"  # in place, no leftovers
    assert sorted(p.name for p in tmp_path.iterdir()) == [".testsstatus"]
    readme = tmp_path / "README.md"
    # write("l1", f); write("l2", f, append = TRUE); write(c("a", "b"), f, append = TRUE); write(character(0), ...)
    effects.write_text(readme, "l1\n")
    effects.write_text(readme, "l2\n", append=True)
    effects.write_text(readme, "a\nb\n", append=True)
    effects.write_text(readme, "", append=True)
    effects.write_text(readme, "end\n", append=True)
    assert readme.read_bytes() == b"l1\nl2\na\nb\nend\n"  # R: the bytes write() produced
    raw = tmp_path / "raw.bin"
    effects.write_bytes(raw, b"\x00\xff")
    effects.write_bytes(raw, b"\x01", append=True)
    assert raw.read_bytes() == b"\x00\xff\x01"
    effects.write_text(raw, "ä \udcff")  # surrogateescape round trip of undecodable bytes
    assert raw.read_bytes() == b"\xc3\xa4 \xff"
    with pytest.raises(FileNotFoundError):  # R: cannot open file 'nodir/file' -> cannot open the connection
        effects.write_text(tmp_path / "nodir" / "file", "x")
    with pytest.raises(ValueError):
        effects.write_text(raw, "x", append=True, atomic=True)


def test_atomic_write_keeps_mode_and_leaves_no_temp_file(tmp_path: Path) -> None:
    effects = ProductionEffects()
    fresh = tmp_path / "fresh.rds"
    effects.write_bytes(fresh, b"new", atomic=True)
    assert fresh.read_bytes() == b"new"
    assert _mode(fresh) == 0o666 & ~_umask()  # R: saveRDS() creates files with the umask mode (644), not 600
    existing = tmp_path / "existing.rds"
    existing.write_bytes(b"old")
    existing.chmod(0o640)
    before = existing.stat().st_ino
    effects.write_bytes(existing, b"replaced", atomic=True)
    assert existing.read_bytes() == b"replaced"
    assert _mode(existing) == 0o640  # the mode of the file written over is kept
    assert existing.stat().st_ino != before  # a rename, not an in-place write
    assert sorted(p.name for p in tmp_path.iterdir()) == ["existing.rds", "fresh.rds"]  # no temporary file left
    with pytest.raises(IsADirectoryError):
        effects.write_bytes(tmp_path, b"x", atomic=True)
    assert sorted(p.name for p in tmp_path.iterdir()) == ["existing.rds", "fresh.rds"]  # cleaned up on failure


def test_write_rds_round_trips_and_is_atomic(tmp_path: Path) -> None:
    effects = ProductionEffects()
    path = tmp_path / "lastcommit.rds"
    effects.write_rds(path, "3f5e2a1b9c8d7e6f5a4b3c2d1e0f9a8b7c6d5e4f")
    assert scalar(read_rds(path)) == "3f5e2a1b9c8d7e6f5a4b3c2d1e0f9a8b7c6d5e4f"
    assert _mode(path) == 0o666 & ~_umask()
    effects.write_rds(path, ".*-AMT_2026-09-30|.*-AMT_2026-10-01")
    assert scalar(read_rds(path)) == ".*-AMT_2026-09-30|.*-AMT_2026-10-01"
    assert sorted(p.name for p in tmp_path.iterdir()) == ["lastcommit.rds"]


def test_copy_like_file_copy(tmp_path: Path) -> None:
    effects = ProductionEffects()
    src = tmp_path / "README.md"
    src.write_bytes(b"readme")
    src.chmod(0o754)
    gitdir = tmp_path / "gitdir"
    gitdir.mkdir()
    assert effects.copy(tmp_path / "missing", gitdir) is False  # R: FALSE, silently
    assert effects.copy(gitdir, tmp_path / "copy-of-dir") is False  # a directory needs recursive = TRUE in R
    assert effects.copy(src, gitdir) is True  # file.copy(from, to = ".") copies into the directory
    assert (gitdir / "README.md").read_bytes() == b"readme"
    assert _mode(gitdir / "README.md") == 0o754  # R: copy.mode = TRUE
    src.write_bytes(b"changed")
    assert effects.copy(src, gitdir) is False  # exists, overwrite = FALSE
    assert (gitdir / "README.md").read_bytes() == b"readme"
    assert effects.copy(src, gitdir, overwrite=True) is True
    assert (gitdir / "README.md").read_bytes() == b"changed"
    assert effects.copy(src, tmp_path / "renamed.md") is True
    assert (tmp_path / "renamed.md").read_bytes() == b"changed"
    assert effects.copy(src, tmp_path / "nodir" / "x") is False  # a failed copy is FALSE, not an error


def test_rename_like_file_rename(tmp_path: Path) -> None:
    effects = ProductionEffects()
    log = tmp_path / "test-full.log"
    log.write_text("log")
    (tmp_path / "tests").mkdir()
    effects.rename(log, tmp_path / "tests" / "test-full-2026-09-30.log")
    assert not log.exists() and (tmp_path / "tests" / "test-full-2026-09-30.log").read_text() == "log"
    other = tmp_path / "other"
    other.write_text("other")
    effects.rename(other, tmp_path / "tests" / "test-full-2026-09-30.log")  # rename(2) replaces an existing file
    assert (tmp_path / "tests" / "test-full-2026-09-30.log").read_text() == "other"
    # R: FALSE with the warning "cannot rename file 'x' to 'y', reason 'No such file or directory'"
    with pytest.raises(FileNotFoundError) as info:
        effects.rename(tmp_path / "missing", tmp_path / "y")
    assert info.value.strerror == "No such file or directory"


def test_delete_like_unlink(tmp_path: Path) -> None:
    effects = ProductionEffects()
    (tmp_path / "f").write_text("")
    effects.delete(tmp_path / "f")
    assert not (tmp_path / "f").exists()
    effects.delete(tmp_path / "f")  # R: unlink of a missing path is a silent success
    realization = tmp_path / "modules" / "01_x" / "empty"
    realization.mkdir(parents=True)
    (realization / "leftover").write_text("")
    effects.delete(realization)  # R: a directory without recursive = TRUE stays (status 1, silent)
    assert realization.is_dir()
    effects.delete(realization, recursive=True)
    assert not realization.exists() and (tmp_path / "modules" / "01_x").is_dir()
    target = tmp_path / "target"
    target.write_text("keep")
    os.symlink(target, tmp_path / "link")
    effects.delete(tmp_path / "link")
    assert target.exists() and not (tmp_path / "link").is_symlink()  # the link goes, the target stays
    os.symlink(tmp_path / "nowhere", tmp_path / "dangling")
    effects.delete(tmp_path / "dangling")
    assert not (tmp_path / "dangling").is_symlink()


@pytest.mark.skipif(os.geteuid() == 0, reason="root ignores the mode bits")
def test_delete_swallows_permission_errors_like_unlink(tmp_path: Path) -> None:
    """Phase-5 Codex findings 2 and 11: R's ``unlink(recursive = TRUE)`` of a stale realization holding a
    read-only subdirectory returns status 1 silently, removes every removable entry and keeps the rest
    (R 4.6.1: ``exists`` TRUE FALSE TRUE for ``sub/f``, ``ok/g``, ``top``); ``startRuns()`` goes on."""
    effects = ProductionEffects()
    stale = tmp_path / "stale"
    (stale / "sub").mkdir(parents=True)
    (stale / "sub" / "f").write_text("")
    (stale / "ok").mkdir()
    (stale / "ok" / "g").write_text("")
    (stale / "top").write_text("")
    os.chmod(stale / "sub", 0o555)
    try:
        effects.delete(stale / "sub" / "f")  # a file in a read-only directory: R unlink() -> status 1, silent
        assert (stale / "sub" / "f").exists()
        effects.delete(stale, recursive=True)  # no exception; the removable siblings go, sub/f stays
        assert (stale / "sub" / "f").exists()
        assert not (stale / "ok").exists() and not (stale / "top").exists()
    finally:
        os.chmod(stale / "sub", 0o755)


def test_mkdir_like_dir_create(tmp_path: Path) -> None:
    effects = ProductionEffects()
    effects.mkdir(tmp_path / "archive")
    assert (tmp_path / "archive").is_dir()
    with pytest.raises(FileExistsError):  # R: warning "'archive' already exists", FALSE
        effects.mkdir(tmp_path / "archive")
    with pytest.raises(FileNotFoundError):  # R: "cannot create dir 'a/b/c', reason 'No such file or directory'"
        effects.mkdir(tmp_path / "a" / "b" / "c")
    effects.mkdir(tmp_path / "a" / "b" / "c", parents=True)
    assert (tmp_path / "a" / "b" / "c").is_dir()


def test_chdir_context_like_with_dir(tmp_path: Path) -> None:
    effects = ProductionEffects()
    outer = Path.cwd()
    (tmp_path / "output" / "run").mkdir(parents=True)
    with effects.chdir(tmp_path):
        assert Path.cwd() == tmp_path.resolve() == Path(effects.getcwd())
        with effects.chdir("output"):
            assert Path.cwd() == (tmp_path / "output").resolve()
            with effects.chdir("run"):
                assert Path.cwd() == (tmp_path / "output" / "run").resolve()
            with effects.chdir("../"):  # withr::local_dir("../") relative to the current directory
                assert Path.cwd() == tmp_path.resolve()
        assert Path.cwd() == tmp_path.resolve()
    assert Path.cwd() == outer
    with pytest.raises(RParityError) as info:
        with effects.chdir(tmp_path / "nonexistent"):
            pytest.fail("the block must not run")
    assert str(info.value) == "cannot change working directory"  # R: setwd()'s message
    assert info.value.call == "setwd(dir)"
    assert Path.cwd() == outer
    with pytest.raises(RuntimeError), effects.chdir(tmp_path):
        raise RuntimeError("inside")
    assert Path.cwd() == outer  # restored after an exception as well


def test_tempdir_is_one_directory_per_process(tmp_path: Path, monkeypatch: pytest.MonkeyPatch) -> None:
    effects = ProductionEffects()
    first = effects.tempdir()
    assert Path(first).is_dir()
    assert effects.tempdir() == first == default_effects().tempdir()
    readme = Path(first) / "README.md"
    effects.write_text(readme, "x")
    assert readme.read_text() == "x"
    readme.unlink()


def test_setenv_like_sys_setenv(monkeypatch: pytest.MonkeyPatch) -> None:
    monkeypatch.setenv("MODELSTATS_SETENV_T", "old")  # registers the restore
    effects = ProductionEffects()
    effects.setenv("MODELSTATS_SETENV_T", "TRUE")
    assert effects.getenv("MODELSTATS_SETENV_T") == "TRUE"
    assert os.environ["MODELSTATS_SETENV_T"] == "TRUE"
    # R: Sys.setenv(autoRenvFixDeps = "TRUE") is seen by every later system() child
    child = effects.run([sys.executable, "-c", "import os; print(os.environ['MODELSTATS_SETENV_T'])"])
    assert child.stdout == "TRUE\n"
    assert effects.run_shell("echo $MODELSTATS_SETENV_T").stdout == "TRUE\n"


def test_run_shell_like_system_with_a_command_line(tmp_path: Path) -> None:
    effects = ProductionEffects()
    result = effects.run_shell("echo first && echo second; echo 'quoted arg' | tr a-z A-Z")
    assert result.returncode == 0
    assert result.stdout == "first\nsecond\nQUOTED ARG\n"
    assert effects.run_shell("sh -c 'echo hi; exit 3'").returncode == 3  # R: status attribute 3
    missing = effects.run_shell("modelstats-no-such-command-xyz --flag")
    assert missing.returncode == 127  # R: system() returns 127 (intern = TRUE: "error in running command")
    assert missing.stdout == "" and "not found" in missing.stderr
    where = effects.run_shell("pwd", cwd=tmp_path).stdout.strip()
    assert Path(where).resolve() == tmp_path.resolve()
    assert effects.run_shell("echo $MODELSTATS_SHELL_T", env={"MODELSTATS_SHELL_T": "v"}).stdout == "v\n"
    assert effects.run_shell("cat", input="piped").stdout == "piped"
    joined = effects.run(["echo", "a b", "c"], shell=True)
    assert joined.stdout == "a b c\n"  # a sequence is joined with shlex.join: "a b" stays one word
    literal = effects.run(["echo", "a b", "&&", "echo", "c"], shell=True)
    assert literal.stdout == "a b && echo c\n"  # ... and an operator element is a quoted word, not an operator
    with pytest.raises(TypeError):
        effects.run("echo x")  # a command line needs shell=True
    with pytest.raises(ValueError):
        effects.run([])


def test_run_without_capture_inherits_the_process_streams(capfd: pytest.CaptureFixture[str]) -> None:
    effects = ProductionEffects()
    code = "import sys; print('to stdout'); print('to stderr', file=sys.stderr); sys.exit(5)"
    result = effects.run([sys.executable, "-c", code], capture=False)
    assert result.returncode == 5
    assert result.stdout == "" and result.stderr == ""  # system(cmd) returns the status only
    captured = capfd.readouterr()
    assert captured.out == "to stdout\n" and captured.err == "to stderr\n"
    assert effects.run_shell("echo via shell", capture=False).returncode == 0
    assert capfd.readouterr().out == "via shell\n"


def test_sleep_calls_time_sleep(monkeypatch: pytest.MonkeyPatch) -> None:
    slept: list[float] = []
    monkeypatch.setattr(time, "sleep", slept.append)
    ProductionEffects().sleep(600)
    assert slept == [600]


# ---------------------------------------------------------------------------
# ProductionEffects.post_json (loopback HTTP server, nothing leaves the machine)
# ---------------------------------------------------------------------------


class _WebhookHandler(http.server.BaseHTTPRequestHandler):
    received: list[tuple[str, str, bytes]] = []

    def do_POST(self) -> None:  # noqa: N802 - http.server API
        length = int(self.headers.get("Content-Length", "0"))
        body = self.rfile.read(length)
        type(self).received.append((self.path, self.headers.get("Content-Type", ""), body))
        if self.path == "/fail":
            self.send_response(500)
            self.end_headers()
            self.wfile.write(b"server error")
            return
        if self.path == "/redirect":
            self.send_response(302)
            self.send_header("Location", "/other")
            self.end_headers()
            self.wfile.write(b"moved")
            return
        self.send_response(200)
        self.send_header("Content-Type", "text/plain")
        self.end_headers()
        self.wfile.write(b"ok")

    def do_GET(self) -> None:  # noqa: N802 - http.server API
        type(self).received.append(("GET " + self.path, "", b""))
        self.send_response(200)
        self.end_headers()
        self.wfile.write(b"other-get")

    def log_message(self, format: str, *args: object) -> None:  # noqa: A002 - http.server API
        return None


@pytest.fixture
def webhook() -> Iterator[str]:
    _WebhookHandler.received = []
    server = http.server.HTTPServer(("127.0.0.1", 0), _WebhookHandler)
    thread = threading.Thread(target=server.serve_forever, daemon=True)
    thread.start()
    try:
        yield f"http://127.0.0.1:{server.server_address[1]}"
    finally:
        server.shutdown()
        server.server_close()


def test_post_json_posts_the_payload_as_json(webhook: str) -> None:
    effects = ProductionEffects()
    payload = '{"text": "Please find below the status of the REMIND automated model tests (AMT) of 2026-09-30."}'
    assert effects.post_json(f"{webhook}/hooks/token", payload) == (200, "ok")
    assert _WebhookHandler.received == [("/hooks/token", "application/json", payload.encode("utf-8"))]
    assert effects.post_json(f"{webhook}/hooks/bytes", b'{"text": "b"}') == (200, "ok")
    assert _WebhookHandler.received[-1][2] == b'{"text": "b"}'


def test_post_json_returns_the_status_of_a_failing_request(webhook: str) -> None:
    assert ProductionEffects().post_json(f"{webhook}/fail", '{"text": "x"}') == (500, "server error")


def test_post_json_does_not_follow_redirects(webhook: str) -> None:
    """Phase-5 Codex finding 6: R's curl has no --location (R/modeltests.R:58-61), so a 302 is the response;
    urllib would otherwise issue a second, body-less GET the AMT never makes."""
    assert ProductionEffects().post_json(f"{webhook}/redirect", '{"text": "x"}') == (302, "moved")
    assert _WebhookHandler.received == [("/redirect", "application/json", b'{"text": "x"}')]


def test_post_json_without_a_response_is_status_zero() -> None:
    effects = ProductionEffects()
    with socket.socket() as probe:
        probe.bind(("127.0.0.1", 0))
        port = probe.getsockname()[1]
    status, body = effects.post_json(f"http://127.0.0.1:{port}/hooks/x", "{}")  # nothing listens here
    assert status == 0 and "URLError" in body
    status, body = effects.post_json("not a url", "{}")
    assert status == 0 and body


# ---------------------------------------------------------------------------
# DryRunEffects
# ---------------------------------------------------------------------------


def tree_hash(root: Path) -> str:
    """Names, modes, sizes, mtimes and contents of everything below ``root`` (``root`` included)."""
    digest = hashlib.sha256()
    for path in [root, *sorted(root.rglob("*"))]:
        st = path.lstat()
        digest.update(f"{path.relative_to(root).as_posix()}|{oct(st.st_mode)}|{st.st_size}|{st.st_mtime_ns}\n".encode())
        if path.is_file() and not path.is_symlink():
            digest.update(path.read_bytes())
    return digest.hexdigest()


@pytest.fixture
def amt_tree(tmp_path: Path) -> Path:
    root = tmp_path / "modeltests"
    (root / "remind" / "output" / "run-AMT_2026-09-28_10.30.27").mkdir(parents=True)
    (root / ".testsstatus").write_text("next:evaluate\n")
    (root / "remind" / "lastcommit.rds").write_bytes(b"old")
    (root / "remind" / "output" / "gRS.rds").write_bytes(b"old")
    (root / "remind" / "output" / "run-AMT_2026-09-28_10.30.27" / "full.log").write_text("log\n")
    (root / "remind" / "config").mkdir()
    (root / "remind" / "config" / "default.cfg").write_text("cfg$force_download <- FALSE\n")
    return root


def test_dry_run_leaves_the_tree_unchanged(amt_tree: Path) -> None:
    before = tree_hash(amt_tree)
    lines: list[str] = []
    effects = DryRunEffects(report=lines.append)
    remind = amt_tree / "remind"
    with effects.chdir(remind):
        # reads execute for real
        assert effects.read_text("../.testsstatus") == "next:evaluate\n"
        assert effects.listdir_like_r("output") == ["gRS.rds", "run-AMT_2026-09-28_10.30.27"]
        assert effects.exists("output/gRS.rds") and effects.is_dir("output")
        assert effects.stat("output/gRS.rds").size == 3
        assert effects.getcwd() == str(remind.resolve())
        # every mutation is logged, nothing happens
        effects.write_text("../.testsstatus", "evaluateRuns() is running or stopped due to an error\n")
        effects.write_bytes("output/gRS.rds", b"new", atomic=True)
        effects.write_rds("lastcommit.rds", "3f5e2a1b")
        effects.write_text("README.md", "line\n", append=True)
        assert effects.copy("output/gRS.rds", "gRS-copy.rds", overwrite=True) is True
        effects.rename("../test-full.log", "../tests/test-full-2026-09-30.log")
        effects.delete("output/run-AMT_2026-09-28_10.30.27", recursive=True)
        effects.mkdir("archive")
        assert effects.run_shell("git reset --hard origin/develop && git pull").returncode == 0
        assert effects.run_shell("touch created-by-shell; echo x > redirected").returncode == 0
        assert effects.run(["sh", "-c", "echo x > created-by-sh"]).returncode == 0
        assert effects.run(["squeue", "-u", "pascalfu", "-h", "-o", TEN]).stdout == ""  # an empty scheduler
        log1 = effects.run_shell("git log -1")
        assert log1.stdout.splitlines()[0] == f"commit {DRY_RUN_COMMIT}"  # a fake commit hash
        assert effects.run_shell("git log --merges --pretty=oneline a..b --abbrev-commit | grep 'Merge'").stdout == ""
        assert effects.post_json("https://mattermost.example.org/hooks/t", '{"text": "hi"}') == (
            200,
            "dry run: nothing was sent",
        )
        started = time.monotonic()
        effects.sleep(600)
        assert time.monotonic() - started < 1
    assert tree_hash(amt_tree) == before
    assert not (remind / "created-by-shell").exists() and not (remind / "created-by-sh").exists()
    assert effects.log == lines
    assert lines == [
        f"would write ../.testsstatus ({len('evaluateRuns() is running or stopped due to an error') + 1} bytes)",
        "would write output/gRS.rds (3 bytes, atomic)",
        lines[2],
        "would write README.md (5 bytes, append)",
        "would copy output/gRS.rds -> gRS-copy.rds (overwrite)",
        "would rename ../test-full.log -> ../tests/test-full-2026-09-30.log",
        "would delete output/run-AMT_2026-09-28_10.30.27 (recursive)",
        "would create directory archive",
        f"would run git reset --hard origin/develop && git pull (cwd {remind.resolve()}, answered status 0)",
        f"would run touch created-by-shell; echo x > redirected (cwd {remind.resolve()}, answered status 0)",
        f"would run sh -c 'echo x > created-by-sh' (cwd {remind.resolve()}, answered status 0)",
        f"would run squeue -u pascalfu -h -o '{TEN}' (cwd {remind.resolve()}, answered status 0)",
        f"would run git log -1 (cwd {remind.resolve()}, answered status 0)",
        f"would run git log --merges --pretty=oneline a..b --abbrev-commit | grep 'Merge' (cwd {remind.resolve()}, "
        "answered status 0)",
        "would POST https://mattermost.example.org/hooks/t (14 bytes)",
        "would sleep 600 s",
    ]
    assert lines[2].startswith("would write lastcommit.rds (RDS, ") and lines[2].endswith(" bytes)")
    assert effects.events[0] == DryRunEvent("write", "../.testsstatus", "53 bytes")
    assert effects.events[0].data == b"evaluateRuns() is running or stopped due to an error\n"
    assert scalar(read_rds_bytes(effects.events[2].data)) == "3f5e2a1b"  # the RDS bytes it would have written
    assert effects.events[-2].data == b'{"text": "hi"}'


def read_rds_bytes(data: bytes | None) -> object:
    import rdata

    assert data is not None
    return rdata.conversion.convert(rdata.parser.parse_data(data, extension=".rds"))


def test_dry_run_setenv_is_applied_and_logged(monkeypatch: pytest.MonkeyPatch) -> None:
    monkeypatch.setenv("MODELSTATS_DRY_T", "old")
    effects = DryRunEffects()
    effects.setenv("MODELSTATS_DRY_T", "TRUE")
    assert os.environ["MODELSTATS_DRY_T"] == "TRUE" and effects.getenv("MODELSTATS_DRY_T") == "TRUE"
    assert effects.log == ["would set MODELSTATS_DRY_T=TRUE"]


def test_dry_run_answer_hook_and_cwd(tmp_path: Path) -> None:
    def answer(tokens: list[str] | tuple[str, ...]) -> tuple[str, int] | None:
        if tokens and tokens[0] == "Rscript":
            return '["SSP2-NPi-AMT"]\n', 0
        return None

    effects = DryRunEffects(answer=lambda tokens: answer(list(tokens)))
    bridge = effects.run(["Rscript", "select_scenarios.R", "out.rds"], cwd=tmp_path)
    assert (bridge.stdout, bridge.returncode) == ('["SSP2-NPi-AMT"]\n', 0)
    assert effects.run_shell("git log -1").stdout.startswith(f"commit {DRY_RUN_COMMIT}")  # the default still applies
    assert effects.run(["false"], capture=False).stdout == ""
    assert (
        effects.log[0] == f"would run Rscript select_scenarios.R out.rds (cwd {tmp_path.resolve()}, answered status 0)"
    )
    with pytest.raises(TypeError):
        effects.run("git log -1")
    with pytest.raises(RParityError):
        with effects.chdir(tmp_path / "missing"):
            pass
    with pytest.raises(ValueError):
        effects.write_text(tmp_path / "x", "x", append=True, atomic=True)


def test_dry_run_is_a_production_effects_for_reads() -> None:
    effects = DryRunEffects()
    assert isinstance(effects, ProductionEffects)
    assert effects.user == ProductionEffects().user
    assert effects.today() == ProductionEffects().today()
    assert Path(effects.tempdir()).is_dir()


# ---------------------------------------------------------------------------
# RecordingEffects / FakeEffects
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


def test_fake_effects_is_the_recording_double() -> None:
    assert FakeEffects is RecordingEffects
    assert isinstance(FakeEffects(), Effects)


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


def test_fake_setenv_stays_in_the_injected_env_or_the_process(monkeypatch: pytest.MonkeyPatch) -> None:
    injected = FakeEffects(env={})
    injected.setenv("autoRenvFixDeps", "TRUE")
    assert injected.getenv("autoRenvFixDeps") == "TRUE"
    assert "autoRenvFixDeps" not in os.environ
    assert injected.calls[0] == FakeCall("setenv", ("autoRenvFixDeps", "TRUE"))
    monkeypatch.setenv("MODELSTATS_FAKE_T", "old")
    FakeEffects().setenv("MODELSTATS_FAKE_T", "new")  # no injected env: the process environment, like R
    assert os.environ["MODELSTATS_FAKE_T"] == "new"


def test_fake_defaults_match_the_harness() -> None:
    fake = FakeEffects()
    assert fake.user == "pascalfu"
    assert fake.on_cluster is False
    assert fake.run(["squeue", "-h", "-o", SIX]).stdout == ""  # no case: empty scheduler


def test_fake_filesystem_reads_are_real_and_logged(dirtest: Path) -> None:
    fake = FakeEffects()
    assert fake.listdir_like_r(dirtest) == R_DIR_ORDER
    assert fake.listdir_all(dirtest) == R_DIR_ALL_ORDER
    assert fake.exists(dirtest / "a") and not fake.exists(dirtest / "broken")
    assert fake.is_dir(dirtest)
    assert fake.stat(dirtest / "a").size == 0
    assert fake.read_text(dirtest / "a") == ""
    assert fake.read_bytes(dirtest / "a") == b""
    assert fake.glob(dirtest / "sub*") == [str(dirtest / "subdir")]
    assert [dirpath for dirpath, _d, _f in fake.walk(dirtest / "subdir")] == [str(dirtest / "subdir")]
    assert [c.method for c in fake.calls] == [
        "listdir_like_r",
        "listdir_all",
        "exists",
        "exists",
        "is_dir",
        "stat",
        "read_text",
        "read_bytes",
        "glob",
        "walk",
    ]
    assert fake.calls[0].args == (str(dirtest),)


def test_fake_writes_are_real_and_logged(tmp_path: Path) -> None:
    fake = FakeEffects()
    status = tmp_path / ".testsstatus"
    fake.write_text(status, "next:start\n")
    fake.write_text(status, "more\n", append=True)
    fake.write_bytes(tmp_path / "b", b"\x00", atomic=True)
    fake.write_rds(tmp_path / "runcode.rds", ".*-AMT_2026-09-30|.*-AMT_2026-10-01")
    assert fake.copy(status, tmp_path / "copy", overwrite=True) is True
    fake.rename(tmp_path / "copy", tmp_path / "renamed")
    fake.mkdir(tmp_path / "d" / "e", parents=True)
    fake.delete(tmp_path / "d", recursive=True)
    with fake.chdir(tmp_path):
        assert fake.getcwd() == str(tmp_path.resolve())
    assert Path(fake.tempdir()).is_dir()
    assert status.read_bytes() == b"next:start\nmore\n"
    assert (tmp_path / "b").read_bytes() == b"\x00"
    assert scalar(read_rds(tmp_path / "runcode.rds")) == ".*-AMT_2026-09-30|.*-AMT_2026-10-01"
    assert (tmp_path / "renamed").read_bytes() == b"next:start\nmore\n"
    assert not (tmp_path / "d").exists()
    assert fake.calls == [
        FakeCall("write_text", (str(status), "next:start\n", False, False)),
        FakeCall("write_text", (str(status), "more\n", True, False)),
        FakeCall("write_bytes", (str(tmp_path / "b"), b"\x00", False, True)),
        FakeCall("write_rds", (str(tmp_path / "runcode.rds"), ".*-AMT_2026-09-30|.*-AMT_2026-10-01")),
        FakeCall("copy", (str(status), str(tmp_path / "copy"), True)),
        FakeCall("rename", (str(tmp_path / "copy"), str(tmp_path / "renamed"))),
        FakeCall("mkdir", (str(tmp_path / "d" / "e"), True)),
        FakeCall("delete", (str(tmp_path / "d"), True)),
        FakeCall("chdir", (str(tmp_path),)),
        FakeCall("getcwd"),
        FakeCall("tempdir"),
    ]


def test_fake_trace_records_tool_argv_and_cwd(tmp_path: Path) -> None:
    fake = FakeEffects(run_table={("git reset --hard origin/develop && git pull",): ("", 0)})
    with fake.chdir(tmp_path):
        assert fake.run_shell("git reset --hard origin/develop && git pull").returncode == 0
        fake.run(["squeue", "-u", "pascalfu", "-h", "-o", TEN])
        fake.run_shell("git log --merges --pretty=oneline a..b --abbrev-commit | grep 'Merge pull request'", cwd="..")
        fake.run(["rsync", "-e", "ssh", "-av", "fulldata.gdx", "rse@host:/x.gdx"], cwd=tmp_path / "run")
    here = str(tmp_path.resolve())
    assert fake.trace == [
        TraceEntry("git", ("reset", "--hard", "origin/develop"), here),
        TraceEntry("git", ("pull",), here),
        TraceEntry("squeue", ("-u", "pascalfu", "-h", "-o", TEN), here),
        TraceEntry(
            "git", ("log", "--merges", "--pretty=oneline", "a..b", "--abbrev-commit"), str(tmp_path.resolve().parent)
        ),
        TraceEntry("grep", ("Merge pull request",), str(tmp_path.resolve().parent)),
        TraceEntry("rsync", ("-e", "ssh", "-av", "fulldata.gdx", "rse@host:/x.gdx"), str(tmp_path / "run")),
    ]
    assert fake.tools[:2] == [("git", ("reset", "--hard", "origin/develop")), ("git", ("pull",))]
    assert fake.shell_runs == [
        "git reset --hard origin/develop && git pull",
        "git log --merges --pretty=oneline a..b --abbrev-commit | grep 'Merge pull request'",
    ]
    assert fake.runs == [
        ("squeue", "-u", "pascalfu", "-h", "-o", TEN),
        ("rsync", "-e", "ssh", "-av", "fulldata.gdx", "rse@host:/x.gdx"),
    ]
    assert fake.calls[1] == FakeCall("run_shell", ("git reset --hard origin/develop && git pull", None, None, None))
    assert fake.calls[3].method == "run_shell" and fake.calls[3].args[1] == ".."


def test_fake_run_table_hook_delegation_and_fallbacks(case_dir: Path, tmp_path: Path) -> None:
    seen: list[tuple[list[str], str]] = []

    def hook(tokens: list[str], cwd: str) -> tuple[str, int] | None:
        seen.append((tokens, cwd))
        if tokens[:2] == ["Rscript", "bridge.R"]:
            return '["a"]\n', 0
        return None

    fake = FakeEffects(slurm_case_dir=case_dir, run_table={("git", "log", "-1"): ("commit abc\n", 0)}, run_hook=hook)
    assert fake.run(["git", "log", "-1"]).stdout == "commit abc\n"  # the table wins
    assert fake.run_shell("git log -1").stdout == "commit abc\n"  # a shell line matches by its tokens too
    assert fake.run(["Rscript", "bridge.R", "out.rds"], cwd=tmp_path).stdout == '["a"]\n'  # the hook
    assert seen[-1] == (["Rscript", "bridge.R", "out.rds"], str(tmp_path.resolve()))
    other = fake.run(["git", "status"])
    assert other.returncode == 127 and other.stdout == ""  # not answered: like a missing executable
    assert fake.run(["/usr/bin/squeue", "-h", "-o", "%j"]).stdout == "running\npend\nweekly\n"  # scheduler fake
    assert fake.run_shell("squeue -u alice -h -o '%j'").stdout == "weekly\n"
    assert fake.run(["git", "log", "-1"], capture=False).stdout == ""  # system() without intern prints nothing here
    assert fake.calls[-1] == FakeCall("run", (("git", "log", "-1"), None, None, None))
    with pytest.raises(TypeError):
        fake.run("git log -1")

    delegating = FakeEffects(delegate_run=True, run_table={("echo", "canned"): ("canned\n", 0)})
    assert delegating.run(["echo", "canned"]).stdout == "canned\n"
    real = delegating.run([sys.executable, "-c", "print('real child')"], cwd=tmp_path)
    assert real.stdout == "real child\n"  # delegated to the real subprocess
    assert delegating.run_shell("echo via shell").stdout == "via shell\n"
    assert delegating.trace[-1] == TraceEntry("echo", ("via", "shell"), os.getcwd())


def test_fake_post_json_and_sleep_are_canned_and_recorded() -> None:
    fake = FakeEffects()
    assert fake.post_json("https://mattermost.example.org/hooks/t", '{"text": "hi"}') == (200, "ok")
    assert fake.post_json("https://mattermost.example.org/hooks/t", b'{"text": "b"}') == (200, "ok")
    fake.sleep(600)
    fake.sleep(300)
    assert fake.posts == [
        ("https://mattermost.example.org/hooks/t", '{"text": "hi"}'),
        ("https://mattermost.example.org/hooks/t", '{"text": "b"}'),
    ]
    assert fake.sleeps == [600, 300]
    assert fake.calls == [
        FakeCall("post_json", ("https://mattermost.example.org/hooks/t", '{"text": "hi"}')),
        FakeCall("post_json", ("https://mattermost.example.org/hooks/t", '{"text": "b"}')),
        FakeCall("sleep", (600,)),
        FakeCall("sleep", (300,)),
    ]
    failing = FakeEffects(post_json_response=(0, "URLError: <urlopen error [Errno 111] Connection refused>"))
    assert failing.post_json("https://x/hooks/t", "{}")[0] == 0


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
