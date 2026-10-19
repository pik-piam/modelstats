from __future__ import annotations

import importlib
import pwd
from datetime import date
from pathlib import Path
from typing import Any

import pytest

from modelstats import _console, _shell
from modelstats.errors import ModelstatsError
from modelstats.prompt_and_run import main, prompt_and_run
from tests.conftest import CapturedConsole, FakeShell, make_main_folder, make_run

# the package re-exports the function under the module's name
rs2 = importlib.import_module("modelstats.prompt_and_run")

RUNS = (
    "C_A-rem-1",
    "C_A-rem-2",
    "C_A-rem-10",
    "X_2026-01-01_10.00.00",
    "X_2026-02-01_10.00.00",
    "PkBudg500",
    "PkBudg1000",
)
RUN_ORDER = [
    "output/C_A-rem-1",
    "output/C_A-rem-2",
    "output/C_A-rem-10",
    "output/PkBudg500",
    "output/PkBudg1000",
    "output/X_2026-01-01_10.00.00",
    "output/X_2026-02-01_10.00.00",
]


@pytest.fixture
def calls(monkeypatch: pytest.MonkeyPatch) -> list[dict[str, Any]]:
    """Record every print_run_table call instead of printing."""
    seen: list[dict[str, Any]] = []

    def table(
        dirs: list[Path],
        user: str | None = None,
        color: bool = True,
        sort_by_time: bool = True,
        *,
        out: Any = None,
    ) -> None:
        seen.append(
            {"dirs": [str(d) for d in dirs], "user": user, "color": color, "sort_by_time": sort_by_time}
        )

    monkeypatch.setattr(rs2, "print_run_table", table)
    return seen


@pytest.fixture
def main_folder(tmp_path: Path, monkeypatch: pytest.MonkeyPatch) -> Path:
    """A REMIND main folder with seven runs in output/, as the working directory."""
    main_dir = make_main_folder(tmp_path / "remind", RUNS)
    monkeypatch.chdir(main_dir)
    return main_dir


@pytest.fixture
def plain_folder(tmp_path: Path, monkeypatch: pytest.MonkeyPatch) -> Path:
    """A folder that is neither a run nor a main folder, with run folders inside, as the working directory."""
    for name in ("C_A-rem-1", "C_A-rem-2", "SSP2-a"):
        make_run(tmp_path, name)
    monkeypatch.chdir(tmp_path)
    return tmp_path


@pytest.fixture
def stdout(out: CapturedConsole, monkeypatch: pytest.MonkeyPatch) -> CapturedConsole:
    """The stdout console of the package, captured."""
    monkeypatch.setattr(_console, "out", out.console)
    return out


def test_dot_lists_the_current_folder(calls: list[dict[str, Any]]) -> None:
    prompt_and_run(".")
    assert calls == [{"dirs": ["."], "user": "alice", "color": True, "sort_by_time": True}]
    prompt_and_run(".", user="bob")
    assert calls[-1]["user"] == "bob"


def test_no_argument_in_a_run_folder(
    tmp_path: Path, calls: list[dict[str, Any]], monkeypatch: pytest.MonkeyPatch
) -> None:
    run = make_run(
        tmp_path, "run", {"full.gms": "", "log.txt": "", "config.Rdata": "", "prepare_and_run.R": ""}
    )
    monkeypatch.chdir(run)
    prompt_and_run("")
    assert calls == [{"dirs": ["."], "user": "alice", "color": True, "sort_by_time": True}]
    prompt_and_run(",")  # strsplit(",", ",") is ""
    assert calls[-1]["dirs"] == ["."]


def test_no_argument_prompts_in_the_main_folder(
    main_folder: Path, calls: list[dict[str, Any]], monkeypatch: pytest.MonkeyPatch
) -> None:
    offered: list[list[str]] = []

    def choose(items: list[str], kind: str) -> list[str]:
        assert kind == "folders"
        offered.append(list(items))
        return items[1:3]

    monkeypatch.setattr(rs2, "choose_from_list", choose)
    prompt_and_run("")
    assert offered[0] == ["output", *RUN_ORDER]  # run order (BUG-054)
    assert calls == [
        {
            "dirs": ["output/C_A-rem-1", "output/C_A-rem-2"],
            "user": "alice",
            "color": True,
            "sort_by_time": False,
        }
    ]
    monkeypatch.setattr(rs2, "choose_from_list", lambda items, kind: [])
    prompt_and_run("")
    assert calls[-1]["dirs"] == []


def test_prompt_outside_a_main_folder(
    plain_folder: Path, calls: list[dict[str, Any]], monkeypatch: pytest.MonkeyPatch
) -> None:
    offered: list[list[str]] = []
    monkeypatch.setattr(rs2, "choose_from_list", lambda items, kind: offered.append(list(items)) or [])
    prompt_and_run("")
    assert offered == [[".", "C_A-rem-1", "C_A-rem-2", "SSP2-a"]]


def test_d_and_f_list_the_output_folder(main_folder: Path, calls: list[dict[str, Any]]) -> None:
    prompt_and_run("-d")
    assert calls[-1]["dirs"] == RUN_ORDER
    assert calls[-1]["sort_by_time"] is False and calls[-1]["color"] is True
    prompt_and_run("-fb")
    assert calls[-1]["sort_by_time"] is True and calls[-1]["color"] is False
    prompt_and_run("-db", user="bob")
    assert calls[-1]["color"] is False and calls[-1]["user"] == "bob"


def test_d_outside_a_main_folder(plain_folder: Path, calls: list[dict[str, Any]]) -> None:
    prompt_and_run("-d")
    assert calls[-1]["dirs"] == ["C_A-rem-1", "C_A-rem-2", "SSP2-a"]


def test_coupled_runs_all(main_folder: Path, calls: list[dict[str, Any]]) -> None:
    prompt_and_run("-p")
    assert calls[-1]["dirs"] == RUN_ORDER  # no magpie/output: the output folder alone
    make_run(main_folder / "magpie" / "output", "C_A-mag-1")
    make_run(main_folder / "magpie" / "output", "C_A-mag-2")
    prompt_and_run("-pb")
    assert calls[-1]["dirs"] == [  # iterations in order over both folders (BUG-080)
        "magpie/output/C_A-mag-1",
        "output/C_A-rem-1",
        "magpie/output/C_A-mag-2",
        "output/C_A-rem-2",
        "output/C_A-rem-10",
        "output/PkBudg500",
        "output/PkBudg1000",
        "output/X_2026-01-01_10.00.00",
        "output/X_2026-02-01_10.00.00",
    ]
    assert calls[-1]["sort_by_time"] is False and calls[-1]["color"] is False


def test_coupled_runs_last_only(main_folder: Path, calls: list[dict[str, Any]]) -> None:
    make_run(main_folder / "magpie" / "output", "C_A-mag-1")  # -s ignores magpie/output (R did too)
    make_run(main_folder / "output", "C_A-mag-2")
    prompt_and_run("-s")
    assert calls[-1]["dirs"] == [  # one per kind (BUG-060): R kept C_A-mag-2 alone
        "output/C_A-mag-2",
        "output/C_A-rem-10",
        "output/PkBudg500",
        "output/PkBudg1000",
        "output/X_2026-02-01_10.00.00",
    ]


def test_coupled_runs_outside_a_main_folder(plain_folder: Path, calls: list[dict[str, Any]]) -> None:
    prompt_and_run("-s")
    assert calls[-1]["dirs"] == ["C_A-rem-2", "SSP2-a"]
    make_run(plain_folder / "magpie" / "output", "C_A-mag-1")
    prompt_and_run("-p")
    assert calls[-1]["dirs"] == ["magpie/output/C_A-mag-1", "C_A-rem-1", "C_A-rem-2", "magpie", "SSP2-a"]


def test_no_argument_in_a_magpie_run_folder(
    tmp_path: Path, calls: list[dict[str, Any]], monkeypatch: pytest.MonkeyPatch
) -> None:
    run = make_run(
        tmp_path, "run", {"full.gms": "", "submit.R": "", "config.yml": "", "magpie_y1995.gdx": ""}
    )
    monkeypatch.chdir(run)
    prompt_and_run("")
    assert calls == [{"dirs": ["."], "user": "alice", "color": True, "sort_by_time": True}]


def test_t_lists_the_latest_amts(
    tmp_path: Path,
    calls: list[dict[str, Any]],
    shell: FakeShell,
    stdout: CapturedConsole,
    monkeypatch: pytest.MonkeyPatch,
    data_dir: Path,
) -> None:
    monkeypatch.chdir(tmp_path)
    amt = tmp_path / "amt"
    make_run(amt, "SSP2-NPi-AMT_2026-10-18_10.00.00")
    make_run(amt, "SSP2-NPi-AMT_2026-10-11_10.00.00")
    monkeypatch.setattr(_shell, "AMT_OUTPUT", amt)
    monkeypatch.setattr(_shell, "AMT_RUNCODE", data_dir / "runcode.rds")
    prompt_and_run("-t")
    assert stdout.text == f"Results from {amt}/ \n"
    assert calls[-1] == {
        "dirs": [str(amt / "SSP2-NPi-AMT_2026-10-18_10.00.00")],
        "user": None,
        "color": True,
        "sort_by_time": False,
    }
    prompt_and_run("-t", user="10-11")
    assert calls[-1]["dirs"] == [str(amt / "SSP2-NPi-AMT_2026-10-11_10.00.00")]
    with pytest.raises(ModelstatsError, match=r"^rs2 -t: invalid pattern '\['"):
        prompt_and_run("-t", user="[")
    assert stdout.text.endswith(f"Results from {amt}/ \n")  # printed before the pattern was looked at
    monkeypatch.setattr(_shell, "AMT_RUNCODE", data_dir / "runsToStart.rds")  # a data frame, no runcode
    with pytest.raises(ModelstatsError, match="expected one string"):
        prompt_and_run("-t")
    # "-tb" is not the AMT branch: R tested for "-t" before stripping the b (BUG-064), so it ends up
    # searching for the pattern "-t" and then for the runs of a user called "-t"
    prompt_and_run("-tb", user="10-11")
    assert ["squeue", "-u", "-t", "-h", "-o", "%j"] in shell.argvs


@pytest.mark.usefixtures("clock")
def test_current_runs_messages(
    calls: list[dict[str, Any]], shell: FakeShell, err: CapturedConsole, stdout: CapturedConsole
) -> None:
    prompt_and_run("-c")
    assert err.lines[-1] == (
        "! No runs found for this user. You can change the reporting period (here: 5 days) "
        "by running 'rs2 -c alice 5"
    )
    assert shell.argvs[2] == _shell.sacct_argv("alice", date(2026, 10, 15), "WorkDir")
    shell.squeue_answers.extend([["/nowhere"], ["gone"]])
    prompt_and_run("-a", user="bob")
    assert err.lines[-1] == (
        "! No runs found for this user. To change the reporting period (days) you need to specify "
        "also a user, e.g. rs2 -c USER 1"
    )
    assert calls == [] and stdout.text == ""


@pytest.mark.usefixtures("clock")
def test_current_runs_lists_the_folders(
    tmp_path: Path,
    calls: list[dict[str, Any]],
    shell: FakeShell,
    err: CapturedConsole,
    stdout: CapturedConsole,
) -> None:
    runs = [make_run(tmp_path, f"run{n}") for n in range(3)]
    shell.squeue_answers.extend([[str(r) for r in runs], [r.name for r in runs]])
    prompt_and_run("-a")
    assert err.lines[-5:] == ["", "Found 3 active runs.", *(str(r) for r in runs)]  # message("") first
    assert calls[-1] == {
        "dirs": [str(r) for r in runs],
        "user": "alice",
        "color": True,
        "sort_by_time": False,
    }
    assert len(shell.argvs) == 2  # -a asks squeue only
    many = [make_run(tmp_path / "m", f"r{n:03d}") for n in range(101)]
    shell.squeue_answers.extend([[str(r) for r in many], [r.name for r in many]])
    prompt_and_run("-cr", days_back=3)
    assert "Found 101 runs. Excuse me? You need a cluster only for yourself it seems." in err.lines
    assert err.lines[-100:] == [str(r) for r in many[:100]]  # print(myruns[1:100])
    assert shell.argvs[-1] == _shell.sacct_argv("alice", date(2026, 10, 15), "JobName")
    assert stdout.text == ""


@pytest.mark.usefixtures("clock")
def test_current_runs_with_zero_days(
    tmp_path: Path, calls: list[dict[str, Any]], shell: FakeShell, err: CapturedConsole
) -> None:
    run = make_run(tmp_path, "run")
    shell.squeue_answers.extend([[str(run)], ["run"]])
    prompt_and_run("-c", days_back=0)  # R: n / 0 is Inf, and sacct is asked from today
    assert "Found 1 runs. Excuse me? You need a cluster only for yourself it seems." in err.lines
    assert shell.argvs[2] == _shell.sacct_argv("alice", date(2026, 10, 18), "WorkDir")


def test_comma_lists_of_flags(
    tmp_path: Path, calls: list[dict[str, Any]], shell: FakeShell, err: CapturedConsole
) -> None:
    run = make_run(tmp_path, "run")
    shell.squeue_answers.extend([[str(run)], ["run"]])
    prompt_and_run("-cb,-ab", days_back=1)  # BUG-078: R stopped on the two-element flag
    assert "Found 1 runs." in err.lines and calls[-1]["color"] is False
    assert len(shell.argvs) == 2  # -a among the flags: no sacct


def test_paths_and_patterns(main_folder: Path, calls: list[dict[str, Any]], stdout: CapturedConsole) -> None:
    prompt_and_run("output/PkBudg500,PkBudg1000")  # a folder, and a folder found inside output
    assert calls[-1]["dirs"] == ["output/PkBudg500", "output/PkBudg1000"]
    assert calls[-1]["user"] == "output/PkBudg500"  # BUG-063: user <- mydir, foundInSlurm() used the first
    assert calls[-1]["sort_by_time"] is False
    prompt_and_run("rem-1$,X_2026")
    assert calls[-1]["dirs"] == [
        "output/C_A-rem-1",
        "output/X_2026-01-01_10.00.00",
        "output/X_2026-02-01_10.00.00",
    ]
    prompt_and_run("/PkBudg500")  # R: file.path("output", "/PkBudg500") is output//PkBudg500
    assert calls[-1]["dirs"] == ["output/PkBudg500"]
    prompt_and_run("output/PkBudg1000,rem-2$")  # a folder first, then the matches
    assert calls[-1]["dirs"] == ["output/PkBudg1000", "output/C_A-rem-2"]
    prompt_and_run("^output/Pk")  # BUG-073: the pattern sees "output/PkBudg500", R saw "./output/PkBudg500"
    assert calls[-1]["dirs"] == ["output/PkBudg500", "output/PkBudg1000"]
    prompt_and_run(",PkBudg500")  # "" is no folder, but output/ is: the output folder itself
    assert calls[-1]["dirs"] == ["output", "output/PkBudg500"]
    assert stdout.text == ""


def test_b_alone_is_the_pattern_dash(main_folder: Path, calls: list[dict[str, Any]]) -> None:
    prompt_and_run("-b")  # gsub("b", "", "-b") is "-": every folder with a dash, without colours
    assert calls[-1]["dirs"] == RUN_ORDER[:3] + RUN_ORDER[5:] and calls[-1]["color"] is False


def test_tilde_is_expanded(
    plain_folder: Path,
    calls: list[dict[str, Any]],
    shell: FakeShell,
    err: CapturedConsole,
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    monkeypatch.setenv("HOME", str(plain_folder))
    prompt_and_run("~/SSP2-a")
    assert calls[-1]["dirs"] == [str(plain_folder / "SSP2-a")]
    monkeypatch.setattr(pwd, "getpwnam", lambda name: (_ for _ in ()).throw(KeyError(name)))  # no NSS lookup
    prompt_and_run("~nosuchuser_xyz/run")  # an unknown user stays a pattern, as R's dir.exists() left it
    assert err.lines[0].startswith("Did not find any runs for ~nosuchuser_xyz/run.")


def test_patterns_outside_a_main_folder(
    plain_folder: Path, calls: list[dict[str, Any]], err: CapturedConsole
) -> None:
    prompt_and_run("^SSP2")  # R matched against "./SSP2-a" and found nothing (BUG-073)
    assert calls[-1]["dirs"] == ["SSP2-a"]
    prompt_and_run("rem")
    assert calls[-1]["dirs"] == ["C_A-rem-1", "C_A-rem-2"]


@pytest.mark.usefixtures("clock")
def test_pattern_falls_back_to_the_user(
    main_folder: Path, calls: list[dict[str, Any]], shell: FakeShell, err: CapturedConsole
) -> None:
    prompt_and_run("nomatch", days_back=2)
    assert err.lines == [
        "Did not find any runs for nomatch. Looking for runs of user nomatch",
        "! No runs found for this user. You can change the reporting period (here: 5 days) "
        "by running 'rs2 -c nomatch 5",
    ]
    assert shell.argvs[0] == ["squeue", "-u", "nomatch", "-h", "-o", "%Z"]
    assert shell.argvs[2] == _shell.sacct_argv("nomatch", date(2026, 10, 16), "WorkDir")
    assert calls == []
    run = make_run(main_folder, "elsewhere")
    shell.squeue_answers.extend([[str(run)], ["elsewhere"]])
    prompt_and_run("nomatch1,nomatch2")  # BUG-078: several patterns stopped R in the fallback
    assert calls[-1]["dirs"] == [str(run)] and calls[-1]["user"] == "nomatch1,nomatch2"
    assert shell.argvs[-1][2] == "nomatch1,nomatch2"  # the fallback asks for both as users
    shell.squeue_answers.clear()
    prompt_and_run("-c,-a")  # a list of flags with no jobs: the first message, as R printed it
    assert err.lines[-1].startswith("! No runs found for this user. You can change")


def test_invalid_pattern(main_folder: Path, calls: list[dict[str, Any]], err: CapturedConsole) -> None:
    with pytest.raises(ModelstatsError, match="rs2: invalid pattern"):
        prompt_and_run("PkBudg[")
    assert main(["PkBudg["]) == 1
    assert err.lines[-1].startswith("Error: rs2: invalid pattern 'PkBudg['")
    assert calls == []


def test_main_argument_convention(monkeypatch: pytest.MonkeyPatch) -> None:
    seen: list[tuple[str, str | None, int]] = []
    monkeypatch.setattr(
        rs2, "prompt_and_run", lambda paths, user=None, days_back=3: seen.append((paths, user, days_back))
    )
    assert main([]) == 0
    assert main(["-c", "5"]) == 0
    assert main(["-c", "bob", "7"]) == 0
    assert main([".", "bob"]) == 0
    assert main([".", ""]) == 0
    assert main(["-c", "²"]) == 0  # not a number: a user called ²
    assert seen == [
        ("", None, 3),
        ("-c", None, 5),
        ("-c", "bob", 7),
        (".", "bob", 3),
        (".", None, 3),
        ("-c", "²", 3),
    ]


def test_main_rejects_bad_days(err: CapturedConsole, monkeypatch: pytest.MonkeyPatch) -> None:
    monkeypatch.setattr(rs2, "prompt_and_run", lambda paths, user=None, days_back=3: None)
    assert main(["-c", "bob", "abc"]) == 1
    assert err.lines[-1] == "Error: DAYS must be a whole number up to 99999, not 'abc'"
    assert main(["-c", "bob", "2.5"]) == 1
    assert main(["-c", "2.5"]) == 1 and main(["-c", "-1"]) == 1  # a number, so DAYS, not a user
    assert main(["-c", "800000"]) == 1  # BUG-079: before the beginning of time
    assert main(["-c", "000005"]) == 0  # the value counts, not the digits
    assert main(["-c", "0" * 5000 + "5"]) == 0 and main(["-c", "0" * 5000]) == 0  # beyond int()'s limit
    assert main(["-c", "5", ""]) == 1
    assert err.lines[-1] == "Error: too many arguments: ''"
    assert main(["-c", "bob", "3", "extra", "args"]) == 1
    assert err.lines[-1] == "Error: too many arguments: extra args"
    assert main(["-c", "5", "7"]) == 1  # the wrapper ignored a third argument after DAYS


def test_main_reports_errors(err: CapturedConsole, monkeypatch: pytest.MonkeyPatch) -> None:
    def failing(*args: object, **kwargs: object) -> None:
        raise ModelstatsError("boom")

    monkeypatch.setattr(rs2, "prompt_and_run", failing)
    assert main(["-d"]) == 1
    assert err.lines[-1] == "Error: boom"


def test_main_interrupted(monkeypatch: pytest.MonkeyPatch) -> None:
    def interrupted(*args: object, **kwargs: object) -> None:
        raise KeyboardInterrupt

    monkeypatch.setattr(rs2, "prompt_and_run", interrupted)
    assert main([]) == 130
