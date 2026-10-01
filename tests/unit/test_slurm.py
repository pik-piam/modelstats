"""``modelstats.slurm`` against ``R/foundInSlurm.R`` and the ``-C`` branch of ``R/commandLineInterface.R``.

The 24 squeue / sacct case directories of ``migration/cases/slurm`` are copied under
``tests/unit/data/slurm`` and the 39 R goldens of the ``slurm`` family (``migration/goldens/slurm``,
R 4.6.1 in the sandbox) are embedded, so the unit tier runs without ``migration/``; one test
checks the copies against the originals when the migration tree is present. The R facts
pinned here (``system(intern = TRUE)``, ``normalizePath``, ``dirname``, ``strsplit``, the TRE
regular expression batteries) come from Rscript oracles run with R 4.6.1 on 2026-10-01.
"""

from __future__ import annotations

import datetime as dt
import os
import shlex
import warnings
from pathlib import Path
from typing import Any

import pytest
from _fake_effects import SIX, FakeEffects

from modelstats.errors import RParityError, RWarning
from modelstats.slurm import (
    SQUEUE_ALL_ARGV,
    SQUEUE_ALL_CALL,
    SQUEUE_ALL_COMMAND,
    current_runs,
    found_in_slurm,
    normalize_path,
    r_dirname,
    r_grepl,
    r_regex,
    r_strsplit_space,
    system_intern,
)

REPO = Path(__file__).resolve().parents[2]
CASES = Path(__file__).resolve().parent / "data" / "slurm"
MIGRATION_CASES = REPO / "migration" / "cases" / "slurm"
RECORDED = REPO / "migration" / "fixtures" / "_meta" / "slurm" / "latest"

# ---------------------------------------------------------------------------------------------
# migration/goldens/slurm: (id, dir below /p, user column, squeue case, R value)
# ---------------------------------------------------------------------------------------------

RUNNING = "projects/synthetic/remind/output/running"
COUPLED = "projects/remind/runs/REMIND-MAgPIE-2022-10-12"
SLURM_GOLDENS: list[tuple[str, str, str, str, str]] = [
    ("running--running-amt", RUNNING, "", "running-amt", "standby"),
    ("running--startup", RUNNING, "", "startup", "standby startup"),
    ("running--pending", RUNNING, "", "pending", "standby pending"),
    ("running--pending-qos1", RUNNING, "", "pending-qos1", "qos1 startup"),  # BUG-031
    ("running--otheruser", RUNNING, "", "otheruser", "alice"),
    ("running--otheruser-startup", RUNNING, "", "otheruser-startup", "alice startup"),
    ("running--otheruser-pending", RUNNING, "", "otheruser-pending", "alice pending"),
    ("running--twousers", RUNNING, "", "twousers", "2 users"),
    ("running--threejobs-twousers", RUNNING, "", "threejobs-twousers", "3 users"),  # BUG-035
    ("running--sameuser-twice", RUNNING, "", "sameuser-twice", "pascalfu"),  # BUG-019
    ("running--otheruser-twice", RUNNING, "", "otheruser-twice", "alice"),
    ("running--mag-fallback", RUNNING, "", "mag-fallback", "no"),
    ("running--substring", RUNNING, "", "substring", "no"),
    ("running--empty", RUNNING, "", "empty", "no"),
    ("running--failing", RUNNING, "", "failing", "no"),
    ("running--current-pascalfu", RUNNING, "", "current-pascalfu", "no"),
    ("running--current-nomagrun", RUNNING, "", "current-nomagrun", "no"),
    ("running--current-regex-dot", RUNNING, "", "current-regex-dot", "no"),
    ("running--current-regex-bracket", RUNNING, "", "current-regex-bracket", "no"),
    ("running--current-failing-sacct", RUNNING, "", "current-failing-sacct", "no"),
    ("running--amt-squeue-4fail", RUNNING, "", "amt-squeue-4fail", "no"),
    ("running--amt-squeue-3fail", RUNNING, "", "amt-squeue-3fail", "no"),
    ("running--amt-job-in-mydir", RUNNING, "", "amt-job-in-mydir", "no"),
    ("nolog--pending", "projects/synthetic/remind/output/nolog", "", "pending", "standby pending"),
    ("multistatus--substring", "projects/synthetic/remind/output/multistatus", "", "substring", "standby"),  # BUG-023
    ("multistatus--running-amt", "projects/synthetic/remind/output/multistatus", "", "running-amt", "standby"),
    (
        "testOneRegi-AMT--substring",
        "projects/remind/modeltests/remind/output/testOneRegi-AMT",
        "",
        "substring",
        "standby",
    ),
    ("testOneRegi--substring", "projects/remind/modeltests/remind/output/testOneRegi", "", "substring", "standby"),
    ("mag-1--mag-fallback", f"{COUPLED}/magpie/output/C_SSP2EU-Base-mag-1", "", "mag-fallback", "standby"),
    ("mag-3--mag-fallback", f"{COUPLED}/magpie/output/C_SSP2EU-Base-mag-3", "", "mag-fallback", "alice startup"),
    ("mag-2--mag-fallback", f"{COUPLED}/magpie/output/C_SSP2EU-Base-mag-2", "", "mag-fallback", "no"),
    ("rem-1--mag-fallback", f"{COUPLED}/remind/output/C_SSP2EU-Base-rem-1", "", "mag-fallback", "standby"),
    (
        "syn-rem-2--mag-fallback",
        "projects/synthetic/coupled-old/remind/output/C_SSP2EU-Base-rem-2",
        "",
        "mag-fallback",
        "standby",
    ),
    (
        "syn-mag-2--running-amt",
        "projects/synthetic/coupled-old/magpie/output/C_SSP2EU-Base-mag-2",
        "",
        "running-amt",
        "standby",
    ),
    ("running--recorded", RUNNING, "", "recorded", "no"),
    (
        "amt-run--recorded",
        "projects/remind/modeltests/remind/output/SSP2-NPi-AMT_2026-09-28_10.30.27",
        "",
        "recorded",
        "no",
    ),
    ("running--user-alice--otheruser", RUNNING, "alice", "otheruser", "standby"),
    ("running--user-bob--twousers", RUNNING, "bob", "twousers", "2 users"),
    ("running--user-comma--running-amt", RUNNING, "pascalfu,alice", "running-amt", "pascalfu"),  # BUG-009
]

# The commands the harness recorded on the cluster (``fixtures/_meta/slurm/latest/NN.txt.cmd``): the argv
# the port hands to ``Effects.run`` must be what the shell makes of R's command strings.
RECORDED_COMMANDS = {
    "six": "squeue -h -o '%u %Z %j %M %T %q'",
    "Z": "squeue -u pascalfu -h -o '%Z'",
    "j": "squeue -u pascalfu -h -o '%j'",
    "WorkDir": "sacct -u pascalfu -s cd,f,cancelled,timeout,oom -S 2026-09-25 -E now -P -n --format WorkDir",
    "JobName": "sacct -u pascalfu -s cd,f,cancelled,timeout,oom -S 2026-09-25 -E now -P -n --format JobName",
}


def case_dir(name: str) -> Path:
    if name == "recorded":
        if not RECORDED.is_dir():
            pytest.skip("the recorded squeue capture lives in migration/fixtures (absent)")
        return RECORDED
    return CASES / name


def effects_for(case: str, user: str = "pascalfu") -> FakeEffects:
    return FakeEffects(on_cluster=True, user=user, slurm_case_dir=case_dir(case))


# ---------------------------------------------------------------------------------------------
# foundInSlurm
# ---------------------------------------------------------------------------------------------


@pytest.mark.parametrize(
    ("case_id", "directory", "user", "case", "expected"), SLURM_GOLDENS, ids=[c[0] for c in SLURM_GOLDENS]
)
def test_found_in_slurm_matches_the_r_goldens(
    case_id: str, directory: str, user: str, case: str, expected: str
) -> None:
    """The 39 slurm goldens, replayed with the case files; ``/p/...`` does not exist here, so normalizePath keeps it."""
    eff = effects_for(case, user or "pascalfu")
    with warnings.catch_warnings():
        warnings.simplefilter("ignore", RWarning)  # the failing cases warn like R (tested separately)
        value = found_in_slurm(f"/p/{directory}", user=user or "pascalfu", effects=eff)
    assert value == expected
    assert eff.runs[0] == SQUEUE_ALL_ARGV


def test_squeue_argv_is_what_the_shell_makes_of_the_r_command() -> None:
    assert list(SQUEUE_ALL_ARGV) == shlex.split(RECORDED_COMMANDS["six"]) == shlex.split(SQUEUE_ALL_COMMAND)
    assert SQUEUE_ALL_CALL == "system(\"squeue -h -o '%u %Z %j %M %T %q'\", intern = TRUE)"


def test_default_user_is_the_passwd_user() -> None:
    eff = effects_for("otheruser", user="alice")
    assert found_in_slurm(f"/p/{RUNNING}", effects=eff) == "standby"  # alice's own job: the QOS
    assert found_in_slurm(f"/p/{RUNNING}", user="pascalfu", effects=eff) == "alice"


def test_failing_squeue_warns_like_r_and_passes_stderr_through(capsys: pytest.CaptureFixture[str]) -> None:
    eff = effects_for("failing")
    with warnings.catch_warnings(record=True) as caught:
        warnings.simplefilter("always")
        assert found_in_slurm(f"/p/{RUNNING}", effects=eff) == "no"
    assert [type(w.message) for w in caught] == [RWarning]
    warning = caught[0].message
    assert isinstance(warning, RWarning)
    assert warning.call == SQUEUE_ALL_CALL
    assert warning.text == "running command 'squeue -h -o '%u %Z %j %M %T %q'' had status 1"
    assert capsys.readouterr().err == "squeue: error: fake squeue failure (exit 1)\n"


def test_one_hit_with_fewer_than_three_fields_is_subscript_out_of_bounds() -> None:
    """``rev(strsplit(line, " ")[[1]])[[3]]`` on a two-token line."""
    eff = FakeEffects(run_table={SQUEUE_ALL_ARGV: ("/p/x/running running \n", 0)})
    with pytest.raises(RParityError, match="^subscript out of bounds$"):
        found_in_slurm("/p/x/running", user="pascalfu", effects=eff)


def test_normalized_path_is_matched(tmp_path: Path) -> None:
    """``normalizePath`` resolves a symlinked run directory before the substring match."""
    real = tmp_path / "real" / "output" / "run1"
    real.mkdir(parents=True)
    (tmp_path / "link").symlink_to(tmp_path / "real")
    resolved = os.path.realpath(real)
    line = f"pascalfu {resolved} run1 2:15:00 RUNNING standby\n"
    eff = FakeEffects(run_table={SQUEUE_ALL_ARGV: (line, 0)})
    assert found_in_slurm(str(tmp_path / "link" / "output" / "run1"), user="pascalfu", effects=eff) == "standby"
    assert found_in_slurm(str(tmp_path / "link" / "output" / "run1") + "/", user="pascalfu", effects=eff) == "standby"


def test_user_is_a_regular_expression_in_the_own_job_test() -> None:
    """``grepl(paste0("^", user, " "), line)``: an invalid user pattern fails like R."""
    line = f"pascalfu /p/{RUNNING} running 2:15:00 RUNNING standby\n"
    eff = FakeEffects(run_table={SQUEUE_ALL_ARGV: (line, 0)})
    assert found_in_slurm(f"/p/{RUNNING}", user="pascal.u", effects=eff) == "standby"
    with warnings.catch_warnings(record=True) as caught, pytest.raises(RParityError) as excinfo:
        warnings.simplefilter("always")
        found_in_slurm(f"/p/{RUNNING}", user="pascal[", effects=eff)
    assert str(excinfo.value) == "invalid regular expression '^pascal[ ', reason 'Missing ']''"
    assert isinstance(caught[0].message, RWarning)
    assert caught[0].message.call == 'grepl(paste0("^", user, " "), squeuefiltered)'


@pytest.mark.skipif(not MIGRATION_CASES.is_dir(), reason="migration/cases/slurm is absent (gitignored tree)")
def test_case_copies_match_the_migration_cases() -> None:
    """``tests/unit/data/slurm`` is a byte-for-byte copy of ``migration/cases/slurm``."""
    originals = {p.relative_to(MIGRATION_CASES): p.read_bytes() for p in MIGRATION_CASES.rglob("*") if p.is_file()}
    copies = {p.relative_to(CASES): p.read_bytes() for p in CASES.rglob("*") if p.is_file()}
    assert copies == originals


# ---------------------------------------------------------------------------------------------
# system(intern = TRUE)
# ---------------------------------------------------------------------------------------------


def test_system_intern_returns_the_lines() -> None:
    eff = FakeEffects(run_table={("cmd",): ("a\nb\n", 0)})
    with warnings.catch_warnings():
        warnings.simplefilter("error")
        assert system_intern("cmd", ["cmd"], "system(cmd, intern = TRUE)", eff) == ["a", "b"]


def test_system_intern_keeps_the_lines_and_warns_on_a_non_zero_status() -> None:
    eff = FakeEffects(run_table={("cmd",): ("hi\n", 3)})
    with pytest.warns(RWarning) as record:
        assert system_intern("sh -c 'echo hi; exit 3'", ["cmd"], "system(x, intern = TRUE)", eff) == ["hi"]
    assert len(record) == 1
    warning = record[0].message
    assert isinstance(warning, RWarning)
    assert (warning.call, warning.text) == (
        "system(x, intern = TRUE)",
        "running command 'sh -c 'echo hi; exit 3'' had status 3",
    )


def test_system_intern_status_127_is_error_in_running_command() -> None:
    eff = FakeEffects()  # an unknown command answers 127 like sh
    with pytest.raises(RParityError, match="^error in running command$"):
        system_intern("nonexistent_cmd_xyz", ["nonexistent_cmd_xyz"], "system(x, intern = TRUE)", eff)


def test_system_intern_signal_death_gives_the_lines_without_a_warning() -> None:
    eff = FakeEffects(run_table={("cmd",): ("", -9)})
    with warnings.catch_warnings():
        warnings.simplefilter("error")
        assert system_intern("cmd", ["cmd"], "system(x, intern = TRUE)", eff) == []


# ---------------------------------------------------------------------------------------------
# normalizePath, dirname, strsplit
# ---------------------------------------------------------------------------------------------


def test_normalize_path(tmp_path: Path, monkeypatch: pytest.MonkeyPatch) -> None:
    eff = FakeEffects()
    real = tmp_path / "real"
    real.mkdir()
    (tmp_path / "link").symlink_to(real)
    resolved = os.path.realpath(real)
    assert normalize_path(str(real) + "/", eff) == resolved
    assert normalize_path(str(tmp_path / "real" / ".." / "real"), eff) == resolved
    assert normalize_path(str(tmp_path / "link"), eff) == resolved
    assert normalize_path("/nonexistent/x/", eff) == "/nonexistent/x/"
    assert normalize_path("rel/x", eff) == "rel/x"
    assert normalize_path("", eff) == ""
    assert normalize_path(str(tmp_path / "link" / "missing"), eff) == str(tmp_path / "link" / "missing")
    assert normalize_path(".", eff) == os.path.realpath(os.getcwd())
    monkeypatch.setenv("HOME", str(tmp_path))
    assert normalize_path("~/definitely_missing_xyz", eff) == str(tmp_path / "definitely_missing_xyz")
    assert normalize_path("~/link", eff) == resolved


@pytest.mark.parametrize(
    ("path", "expected"),
    [
        ("", ""),
        ("/", "/"),
        ("a", "."),
        ("a/", "."),
        ("a/b/", "a"),
        ("/a", "/"),
        ("//a", "/"),
        ("a//b", "a"),
        ("a/b//", "a"),
        (".", "."),
        ("..", "."),
        ("/a/b/..", "/a/b"),
        ("/a/b", "/a"),
        ("a/b/c", "a/b"),
    ],
)
def test_r_dirname(path: str, expected: str) -> None:
    assert r_dirname(path) == expected


def test_r_dirname_expands_the_tilde(monkeypatch: pytest.MonkeyPatch) -> None:
    monkeypatch.setenv("HOME", "/home/someone")
    assert r_dirname("~") == "/home"
    assert r_dirname("~/x") == "/home/someone"


@pytest.mark.parametrize(
    ("text", "expected"),
    [
        ("", []),
        (" ", [""]),
        ("a", ["a"]),
        ("a ", ["a"]),
        (" a", ["", "a"]),
        ("a  b", ["a", "", "b"]),
        ("a b ", ["a", "b"]),
        ("  ", ["", ""]),
        ("a b  ", ["a", "b", ""]),
    ],
)
def test_r_strsplit_space(text: str, expected: list[str]) -> None:
    assert r_strsplit_space(text) == expected


# ---------------------------------------------------------------------------------------------
# TRE regular expressions (grepl)
# ---------------------------------------------------------------------------------------------

TEXTS = ["a", "aab", "x{", "C_SSP2[NDC", "(a)", "*a", "{1}a", "aaaaaaaaaaaa"]
# grepl(pattern, TEXTS) in R 4.6.1, or the TRE reason of the error; T/F strings keep the table readable.
# Not in the table (TRE features the port does not emulate, verified to differ): "a{-1}" is an
# approximate-matching bound in TRE (matches everything; the port rejects it as Invalid contents of {})
# and "a{,}" behaves like "a{1,}" in TRE where the port reads "a{0,}".
TRE_BATTERY: list[tuple[str, str]] = [
    ("C_SSP2[NDC", "Missing ']'"),
    ("a(b", "Missing ')'"),
    ("a)b", "FFFFFFFF"),
    ("a{2", "Missing '}'"),
    ("*a", "TTFFTTTT"),
    ("+a", "TTFFTTTT"),
    ("?a", "TTFFTTTT"),
    ("a**", "Invalid use of repetition operators"),
    ("a++", "Invalid use of repetition operators"),
    ("a\\", "Trailing backslash"),
    ("\\", "Trailing backslash"),
    ("[]", "Missing ']'"),
    ("[a", "Missing ']'"),
    ("a[]b", "Missing ']'"),
    ("a{1,2}", "TTFFTTTT"),
    ("a{,2}", "TTTTTTTT"),
    ("(", "Missing ')'"),
    (")", "FFFFTFFF"),
    ("[[:alpha:]", "Missing ']'"),
    ("a|*b", "TTFFTTTT"),
    ("a{x}", "Invalid contents of {}"),
    ("a{2,1}", "Invalid contents of {}"),
    ("x[b-a]", "Invalid character range"),
    ("a{", "Missing '}'"),
    ("}", "FFFFFFTF"),
    ("]", "FFFFFFFF"),
    ("a|", "TTTTTTTT"),
    ("|a", "TTTTTTTT"),
    ("()", "TTTTTTTT"),
    ("a{1", "Missing '}'"),
    ("[[:foo:]]", "Unknown character class name"),
    ("a{65536}", "Invalid contents of {}"),
    ("\\d", "FFFTFFTF"),
    ("x{", "Missing '}'"),
    ("(?i)a", "TTFFTTTT"),
    ("a\\1", "Invalid back reference"),
    ("\\(", "FFFFTFFF"),
    ("(a))", "FFFFTFFF"),
    ("a{1}{2}", "FTFFFFFT"),
    ("a{255}", "FFFFFFFF"),
    ("a{256}", "Invalid contents of {}"),
    ("a{}", "Invalid contents of {}"),
    ("a{1,}", "TTFFTTTT"),
    ("(*a)", "TTFFTTTT"),
    ("a|+b", "TTFFTTTT"),
    ("(+a)", "TTFFTTTT"),
    ("a(*b)", "FTFFFFFF"),
    ("x*", "TTTTTTTT"),
    ("{1}a", "TTFFTTTT"),
    ("a{0}", "TTTTTTTT"),
    ("a{1}*", "Invalid use of repetition operators"),
    ("a*{2}", "TTTTTTTT"),
    ("a?*", "Invalid use of repetition operators"),
    ("a*?", "TTTTTTTT"),
    ("a+?", "TTFFTTTT"),
    ("(a)(*b)", "FTFFFFFF"),
    ("[a-]", "TTFFTTTT"),
    ("[]a]", "TTFFTTTT"),
    ("[^]a]", "FTTTTTTF"),
    ("a{1,256}", "Invalid contents of {}"),
    ("a{256,}", "FFFFFFFF"),
    ("^*a", "TTFFFFFT"),
    ("a$*", "TFFFFTTT"),
    ("a|*", "TTTTTTTT"),
    ("*", "TTTTTTTT"),
    ("+", "TTTTTTTT"),
    ("?", "TTTTTTTT"),
    ("**", "Invalid use of repetition operators"),
    ("a{1,2}{3}", "FFFFFFFT"),
    ("a{1}+", "Invalid use of repetition operators"),
    ("a*+", "Invalid use of repetition operators"),
    ("a+*", "Invalid use of repetition operators"),
    ("{", "Missing '}'"),
    ("a{1,2", "Missing '}'"),
    ("a{1,x}", "Invalid contents of {}"),
    ("a]", "FFFFFFFF"),
    ("a[b", "Missing ']'"),
    ("\\a", "FFFFFFFF"),
    ("\\.", "FFFFFFFF"),
    ("a\\{", "FFFFFFFF"),
    ("a\\}", "FFFFFFFF"),
    ("\\*a", "FFFFFTFF"),
    ("a\\*", "FFFFFFFF"),
    ("(?:a)", "TTFFTTTT"),
    ("(?=a)", "Invalid regexp"),
    ("a{1}?", "TTFFTTTT"),
    ("a{1}{", "Missing '}'"),
    ("[[.a.]]", "Unknown collating element"),
    ("[[=a=]]", "Unknown collating element"),
    ("ab|", "TTTTTTTT"),
    ("(|a)", "TTTTTTTT"),
    ("a||b", "TTTTTTTT"),
]


@pytest.mark.parametrize(("pattern", "expected"), TRE_BATTERY, ids=[repr(p) for p, _ in TRE_BATTERY])
def test_r_regex_reads_patterns_like_tre(pattern: str, expected: str) -> None:
    if set(expected) <= {"T", "F"}:
        with warnings.catch_warnings():
            warnings.simplefilter("error")
            regex = r_regex(pattern, call="grepl(x, y)")
        assert "".join("T" if regex.search(t) else "F" for t in TEXTS) == expected
    else:
        with warnings.catch_warnings(record=True) as caught, pytest.raises(RParityError) as excinfo:
            warnings.simplefilter("always")
            r_regex(pattern, call="grepl(x, y)")
        assert str(excinfo.value) == f"invalid regular expression '{pattern}', reason '{expected}'"
        assert [type(w.message) for w in caught] == [RWarning]
        assert isinstance(caught[0].message, RWarning)
        assert (caught[0].message.call, caught[0].message.text) == (
            "grepl(x, y)",
            f"TRE pattern compilation error '{expected}'",
        )


def test_r_grepl() -> None:
    assert r_grepl("", "abc", call="g") is True
    assert r_grepl("C.SSP2", "C_SSP2", call="g") is True
    assert r_grepl("^pascalfu,alice ", "pascalfu /p/x run 1:00 RUNNING standby", call="g") is False
    assert r_grepl("PENDING [A-Za-z]*$", "pascalfu /p/x run 0:00 PENDING qos1", call="g") is False
    assert r_grepl("PENDING [A-Za-z]*$", "pascalfu /p/x run 0:00 PENDING standby", call="g") is True


# ---------------------------------------------------------------------------------------------
# rs -C: current_runs
# ---------------------------------------------------------------------------------------------

P = "/p/projects"
# The run directories the squeue / sacct cases name that exist in the fixture tree (the rs goldens list them).
EXISTING_RUNS = [
    f"{P}/remind/modeltests/remind/output/SSP2-NPi-AMT_2026-09-28_10.30.27",
    f"{P}/remind/modeltests/remind/output/default-AMT_2026-09-28_13.23.51",
    f"{P}/remind/modeltests/remind/output/SSP3-NPi2025-AMT_2026-09-28_17.12.58",
    f"{P}/remind/modeltests/remind/output/testOneRegi-AMT",
    f"{P}/remind/modeltests/remind/output/testOneRegi",
    f"{P}/remind/runs/REMIND-MAgPIE-2026-07-07/output/C_SSP2-NDC-LTS_2026-07-08_05.05.25",
    f"{P}/remind/runs/REMIND-MAgPIE-2026-07-07/magpie",
    f"{P}/landuse/tests/magpie/output/default_2026-09-19_04.05.47",
    f"{P}/landuse/tests/magpie/output/weeklyTests_SSP1-Ref",
    f"{P}/landuse/tests/magpie/output/weeklyTests_SSP1-PkBudg1000",
]


class TreeEffects(FakeEffects):
    """FakeEffects whose ``file.exists`` looks below ``root`` (the fixture tree is not mounted at /p here)."""

    def __init__(self, root: Path, **kwargs: Any) -> None:
        super().__init__(**kwargs)
        self.root = root

    def exists(self, path: str | os.PathLike[str]) -> bool:
        text = os.fspath(path)
        return super().exists(str(self.root) + text if text.startswith("/") else text)


@pytest.fixture
def tree(tmp_path: Path) -> Path:
    for run in EXISTING_RUNS:
        (tmp_path / run.lstrip("/")).mkdir(parents=True)
    return tmp_path


def tree_effects(tree: Path, case: str) -> TreeEffects:
    return TreeEffects(tree, on_cluster=True, user="pascalfu", slurm_case_dir=case_dir(case))


# (rs case id, -u, -d, squeue case, the run list; the rs goldens show these runs in their tables)
CURRENT_CASES: list[tuple[str, str, int, str, list[str]]] = [
    (
        "current",  # own jobs incl. coupled parent, batch, mag-run, repeated job, non-existent dir
        "pascalfu",
        0,
        "current-pascalfu",
        [
            f"{P}/remind/modeltests/remind/output/default-AMT_2026-09-28_13.23.51",
            f"{P}/remind/modeltests/remind/output/SSP2-NPi-AMT_2026-09-28_10.30.27",
            f"{P}/remind/runs/REMIND-MAgPIE-2026-07-07/magpie",
            f"{P}/remind/runs/REMIND-MAgPIE-2026-07-07/output/C_SSP2-NDC-LTS_2026-07-08_05.05.25",
        ],
    ),
    (
        "current--nomagrun",  # no mag-run job: the 'default' job is kept
        "pascalfu",
        0,
        "current-nomagrun",
        [
            f"{P}/landuse/tests/magpie/output/default_2026-09-19_04.05.47",
            f"{P}/remind/modeltests/remind/output/SSP2-NPi-AMT_2026-09-28_10.30.27",
        ],
    ),
    (
        "current--user-alice",
        "alice",
        0,
        "current-pascalfu",
        [
            f"{P}/landuse/tests/magpie/output/weeklyTests_SSP1-Ref",
            f"{P}/remind/modeltests/remind/output/testOneRegi",
        ],
    ),
    ("current--users-comma", "pascalfu,alice", 0, "current-pascalfu", []),  # BUG-009
    (
        "current--daysback",  # sacct with -S <frozen date - 5>
        "pascalfu",
        5,
        "current-pascalfu",
        [
            f"{P}/remind/modeltests/remind/output/default-AMT_2026-09-28_13.23.51",
            f"{P}/remind/modeltests/remind/output/SSP2-NPi-AMT_2026-09-28_10.30.27",
            f"{P}/remind/modeltests/remind/output/SSP3-NPi2025-AMT_2026-09-28_17.12.58",
            f"{P}/remind/modeltests/remind/output/testOneRegi-AMT",
            f"{P}/remind/runs/REMIND-MAgPIE-2026-07-07/magpie",
            f"{P}/remind/runs/REMIND-MAgPIE-2026-07-07/output/C_SSP2-NDC-LTS_2026-07-08_05.05.25",
        ],
    ),
    (
        "current--daysback-only",
        "alice",
        3,
        "current-pascalfu",
        [
            f"{P}/landuse/tests/magpie/output/weeklyTests_SSP1-PkBudg1000",
            f"{P}/landuse/tests/magpie/output/weeklyTests_SSP1-Ref",
            f"{P}/remind/modeltests/remind/output/testOneRegi",
        ],
    ),
    ("current--empty", "pascalfu", 0, "empty", []),
    ("current--empty-daysback", "pascalfu", 2, "empty", []),
    ("current--failing", "pascalfu", 0, "failing", []),
    (
        "current--failing-sacct",
        "pascalfu",
        5,
        "current-failing-sacct",
        [f"{P}/remind/modeltests/remind/output/SSP2-NPi-AMT_2026-09-28_10.30.27"],
    ),
    (
        "current--regex-dot",
        "pascalfu",
        0,
        "current-regex-dot",
        [f"{P}/remind/modeltests/remind/output/testOneRegi"],
    ),  # BUG-008
    ("current--recorded", "pascalfu", 0, "recorded", []),
    ("current--recorded-dklein", "dklein", 0, "recorded", []),
]


@pytest.mark.parametrize(
    ("case_id", "user", "daysback", "case", "expected"), CURRENT_CASES, ids=[c[0] for c in CURRENT_CASES]
)
def test_current_runs_matches_the_rs_cases(
    tree: Path, case_id: str, user: str, daysback: int, case: str, expected: list[str]
) -> None:
    eff = tree_effects(tree, case)
    with warnings.catch_warnings():
        warnings.simplefilter("ignore", RWarning)
        assert current_runs(user, daysback, eff) == expected
    since = (dt.date(2026, 9, 30) - dt.timedelta(days=daysback)).isoformat()
    sacct = ["sacct", "-u", user, "-s", "cd,f,cancelled,timeout,oom", "-S", since, "-E", "now", "-P", "-n", "--format"]
    queries = [("squeue", "-u", user, "-h", "-o", "%Z"), ("squeue", "-u", user, "-h", "-o", "%j")]
    if daysback > 0:
        queries += [(*sacct, "WorkDir"), (*sacct, "JobName")]
    assert eff.runs == queries


def test_current_runs_argv_is_what_the_shell_makes_of_the_r_commands(tree: Path) -> None:
    """The recorded cluster commands (``-S`` five days before the frozen 2026-09-30) token for token."""
    eff = tree_effects(tree, "current-pascalfu")
    current_runs("pascalfu", 5, eff)
    assert [list(argv) for argv in eff.runs] == [
        shlex.split(RECORDED_COMMANDS[k]) for k in ("Z", "j", "WorkDir", "JobName")
    ]


def test_current_runs_invalid_job_name_regex_fails_like_r(tree: Path) -> None:
    """BUG-008: the job name is a TRE pattern; ``C_SSP2[NDC`` is R's error, after TRE's warning."""
    eff = tree_effects(tree, "current-regex-bracket")
    with warnings.catch_warnings(record=True) as caught, pytest.raises(RParityError) as excinfo:
        warnings.simplefilter("always")
        current_runs("pascalfu", 0, eff)
    assert str(excinfo.value) == "invalid regular expression 'C_SSP2[NDC', reason 'Missing ']''"
    assert isinstance(caught[0].message, RWarning)
    assert caught[0].message.call == "grepl(runnames[[i]], myruns[[i]])"
    assert caught[0].message.text == "TRE pattern compilation error 'Missing ']''"


def test_current_runs_failing_queries_warn_like_r(tree: Path, capsys: pytest.CaptureFixture[str]) -> None:
    eff = tree_effects(tree, "failing")
    with warnings.catch_warnings(record=True) as caught:
        warnings.simplefilter("always")
        assert current_runs("pascalfu", 0, eff) == []
    messages = [(w.message.call, w.message.text) for w in caught if isinstance(w.message, RWarning)]
    assert messages == [
        (
            'system(paste0("squeue -u ", opt$user, " -h -o \'%Z\'"), intern = TRUE)',
            "running command 'squeue -u pascalfu -h -o '%Z'' had status 1",
        ),
        (
            'system(paste0("squeue -u ", opt$user, " -h -o \'%j\'"), intern = TRUE)',
            "running command 'squeue -u pascalfu -h -o '%j'' had status 1",
        ),
    ]
    assert capsys.readouterr().err == "squeue: error: fake squeue failure (exit 1)\n" * 2


def test_current_runs_failing_sacct_warns_like_r(tree: Path) -> None:
    eff = tree_effects(tree, "current-failing-sacct")
    with warnings.catch_warnings(record=True) as caught:
        warnings.simplefilter("always")
        current_runs("pascalfu", 5, eff)
    messages = [(w.message.call, w.message.text) for w in caught if isinstance(w.message, RWarning)]
    sacct = "sacct -u pascalfu -s cd,f,cancelled,timeout,oom -S 2026-09-25 -E now -P -n"
    assert messages == [
        (
            'system(paste(sacctcode, "--format WorkDir"), intern = TRUE)',
            f"running command '{sacct} --format WorkDir' had status 1",
        ),
        (
            'system(paste(sacctcode, "--format JobName"), intern = TRUE)',
            f"running command '{sacct} --format JobName' had status 1",
        ),
    ]


def _squeue_tables(user: str, workdirs: str, names: str) -> dict[tuple[str, ...], tuple[str, int]]:
    return {
        ("squeue", "-u", user, "-h", "-o", "%Z"): (workdirs, 0),
        ("squeue", "-u", user, "-h", "-o", "%j"): (names, 0),
    }


def test_current_runs_more_names_than_workdirs_is_subscript_out_of_bounds(tmp_path: Path) -> None:
    """BUG-028: ``myruns[[i]]`` beyond the shorter ``%Z`` answer."""
    (tmp_path / "a").mkdir()
    eff = TreeEffects(tmp_path, run_table=_squeue_tables("u", "/a\n", "a\nb\n"))
    with pytest.raises(RParityError, match="^subscript out of bounds$"):
        current_runs("u", 0, eff)


def test_current_runs_no_names_but_workdirs_is_subscript_out_of_bounds(tmp_path: Path) -> None:
    """``for (i in 1:length(runnames))`` with ``length(runnames) == 0`` visits ``runnames[[1]]``."""
    (tmp_path / "a").mkdir()
    eff = TreeEffects(tmp_path, run_table=_squeue_tables("u", "/a\n", ""))
    with pytest.raises(RParityError, match="^subscript out of bounds$"):
        current_runs("u", 0, eff)


def test_current_runs_more_workdirs_than_names_keeps_the_surplus(tmp_path: Path) -> None:
    (tmp_path / "a").mkdir()
    (tmp_path / "b").mkdir()
    eff = TreeEffects(tmp_path, run_table=_squeue_tables("u", "/a\n/b\n", "a\n"))
    assert current_runs("u", 0, eff) == ["/a", "/b"]


def test_current_runs_drops_batch_and_default_only_with_a_mag_run_job(tmp_path: Path) -> None:
    for name in ("x", "d", "b", "m"):
        (tmp_path / name).mkdir()
    (tmp_path / "x" / "output" / "coupled").mkdir(parents=True)
    workdirs = "/d\n/b\n/m\n/x\n"
    with_magrun = TreeEffects(tmp_path, run_table=_squeue_tables("u", workdirs, "default\nbatch\nmag-run\ncoupled\n"))
    assert current_runs("u", 0, with_magrun) == ["/m", "/x/output/coupled"]
    without = TreeEffects(tmp_path, run_table=_squeue_tables("u", workdirs, "default\nbatch\nmagrun\ncoupled\n"))
    assert current_runs("u", 0, without) == ["/x/output/coupled"]  # "/m/output/magrun" does not exist


def test_current_runs_sorts_unique_in_r_collation(tmp_path: Path) -> None:
    for name in ("B", "a", "_x", "A1"):
        (tmp_path / name).mkdir()
    eff = TreeEffects(tmp_path, run_table=_squeue_tables("u", "/B\n/a\n/_x\n/A1\n/a\n", "B\na\n_x\nA1\na\n"))
    assert current_runs("u", 0, eff) == ["/_x", "/a", "/A1", "/B"]


def test_six_field_format_constant_is_the_fake_scheduler_one() -> None:
    assert SQUEUE_ALL_ARGV[-1] == SIX
