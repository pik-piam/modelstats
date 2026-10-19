from __future__ import annotations

import io
import os
import sys
import warnings
from datetime import date
from pathlib import Path
from typing import Any

import pytest
from rich.text import Text

from modelstats import _console, _shell, cli
from modelstats.cli import current_run_dirs, expand_paths, is_main_folder, is_run_folder, main
from modelstats.errors import ModelstatsError
from tests.conftest import CapturedConsole, FakeShell, capture_console, make_main_folder, make_run

RUN_FILES = {"full.gms": "", "log.txt": "", "config.Rdata": "", "prepare_and_run.R": ""}


@pytest.fixture(autouse=True)
def _fixed_hint(monkeypatch: pytest.MonkeyPatch) -> None:
    monkeypatch.setattr(cli, "_hint", lambda: cli.HINTS[0])


@pytest.fixture
def calls(monkeypatch: pytest.MonkeyPatch) -> dict[str, Any]:
    """Record what rs hands to the table printers instead of printing."""
    seen: dict[str, Any] = {}

    def table(
        dirs: list[Path],
        user: str | None = None,
        color: bool = True,
        sort_by_time: bool = True,
        *,
        out: object = None,
    ) -> None:
        seen.update(kind="table", dirs=list(dirs), user=user, color=color, sort_by_time=sort_by_time)

    def sanity(dirs: list[Path] | None = None, *, out: object = None) -> None:
        seen.update(kind="sanity", dirs=list(dirs or []))

    monkeypatch.setattr(cli, "print_run_table", table)
    monkeypatch.setattr(cli, "print_sanity_checks", sanity)
    return seen


@pytest.fixture
def output(tmp_path: Path, monkeypatch: pytest.MonkeyPatch) -> Path:
    """A main folder with six runs and an archive folder in output/, as the working directory."""
    main_dir = make_main_folder(
        tmp_path / "remind",
        (
            "C_SSP2-rem-1",
            "C_SSP2-rem-2",
            "C_SSP2-rem-10",
            "SSP2-NPi_2026-10-18_10.00.00",
            "PkBudg500",
            "PkBudg1000",
        ),
        RUN_FILES,
    )
    make_run(main_dir / "output", "archive")
    monkeypatch.chdir(main_dir)
    return main_dir / "output"


def names(seen: dict[str, Any]) -> list[str]:
    return [p.name for p in seen["dirs"]]


# --- folder tests -------------------------------------------------------------------------------


def test_is_run_folder_and_main_folder(tmp_path: Path) -> None:
    remind = make_run(
        tmp_path, "r", {"full.gms": "", "log.txt": "", "config.Rdata": "", "prepareAndRun.R": ""}
    )
    assert is_run_folder(remind)
    three = make_run(tmp_path, "three", {"full.gms": "", "log.txt": "", "config.Rdata": ""})
    assert not is_run_folder(three)
    magpie = make_run(
        tmp_path, "m", {"full.gms": "", "submit.R": "", "config.yml": "", "magpie_y1995.gdx": ""}
    )
    assert is_run_folder(magpie)
    assert not is_run_folder(make_run(tmp_path, "m3", {"full.gms": "", "submit.R": "", "config.yml": ""}))
    main_dir = make_run(tmp_path, "main", {"output.R": "", "start.R": "", "main.gms": ""})
    assert not is_main_folder(main_dir)
    (main_dir / "output").mkdir()
    assert is_main_folder(main_dir)


def test_expand_paths(tmp_path: Path) -> None:
    run = make_run(
        tmp_path, "run", {"full.gms": "", "log.txt": "", "config.Rdata": "", "prepare_and_run.R": ""}
    )
    main_dir = make_run(tmp_path, "main", {"output.R": "", "start.R": "", "main.gms": ""})
    make_run(main_dir / "output", "b")
    make_run(main_dir / "output", "a")
    (main_dir / "output" / "file").write_text("")
    plain = make_run(tmp_path, "plain")
    make_run(plain, ".hidden")
    make_run(plain, "x")
    assert expand_paths([run, main_dir, plain, tmp_path / "missing"]) == [
        run,
        main_dir / "output" / "a",
        main_dir / "output" / "b",
        plain / ".hidden",
        plain / "x",
    ]


# --- current runs -------------------------------------------------------------------------------


def test_current_runs(tmp_path: Path, shell: FakeShell, clock: object) -> None:
    run = make_run(tmp_path, "SSP2-NPi")
    coupled = make_run(tmp_path / "coupled" / "output", "C_SSP2-rem-3")
    shell.squeue_answers.extend(
        [
            [str(run), str(tmp_path / "coupled"), str(tmp_path), str(tmp_path)],
            ["SSP2-NPi", "C_SSP2-rem-3", "batch", "preprocessing"],
        ]
    )
    assert current_run_dirs("alice") == [coupled, run]  # run order (BUG-054)
    assert shell.argvs == [
        ["squeue", "-u", "alice", "-h", "-o", "%Z"],
        ["squeue", "-u", "alice", "-h", "-o", "%j"],
    ]


def test_current_runs_with_days_back(tmp_path: Path, shell: FakeShell, clock: object) -> None:
    old = make_run(tmp_path, "old-run")
    shell.sacct_lines = {"WorkDir": [str(old)], "JobName": ["old-run"]}
    assert current_run_dirs("alice,bob", 5) == [old]
    assert shell.argvs[2][:6] == ["sacct", "-u", "alice,bob", "-s", "cd,f,cancelled,timeout,oom", "-S"]
    assert shell.argvs[2][6] == str(date(2026, 10, 13))


def test_current_runs_drop_default_only_with_mag_run(tmp_path: Path, shell: FakeShell) -> None:
    default = make_run(tmp_path, "default")
    shell.squeue_answers.extend([[str(default), str(tmp_path)], ["default", "mag-run"]])
    assert current_run_dirs("alice") == [tmp_path]  # the mag-run job's folder stays, "default" is dropped
    shell.squeue_answers.extend([[str(default)], ["default"]])
    assert current_run_dirs("alice") == [default]


def test_current_runs_job_name_as_pattern(tmp_path: Path, shell: FakeShell) -> None:
    run = make_run(tmp_path, "SSP2-NPi")
    weird = make_run(tmp_path / "w" / "output", "a[b")
    shell.squeue_answers.extend([[str(run), str(tmp_path / "w")], ["SSP2.NPi", "a[b"]])
    assert current_run_dirs("alice") == [weird, run]  # run order (BUG-054)
    literal = make_run(tmp_path, "run[1")
    shell.squeue_answers.extend([[str(literal)], ["run[1"]])
    assert current_run_dirs("alice") == [literal]  # BUG-052: no pattern, so the text counts
    nested = make_run(tmp_path, "run[[1]")
    shell.squeue_answers.extend([[str(nested)], ["run[[1]"]])
    with warnings.catch_warnings():
        warnings.simplefilter("error")  # a nested set is no pattern either, and does not warn
        assert current_run_dirs("alice") == [nested]
    huge = make_run(tmp_path, "run{4294967296}")
    shell.squeue_answers.extend([[str(huge)], ["run{4294967296}"]])
    assert current_run_dirs("alice") == [huge]  # re's OverflowError: no pattern, matched as text


def test_current_runs_nothing_in_slurm(shell: FakeShell) -> None:
    assert current_run_dirs("alice") == []


# --- the command --------------------------------------------------------------------------------


def test_default_path_is_the_main_folder(
    output: Path, calls: dict[str, Any], shell: FakeShell, err: CapturedConsole
) -> None:
    assert main([]) == 0
    assert calls["kind"] == "table"
    assert names(calls) == [
        "archive",
        "C_SSP2-rem-1",
        "C_SSP2-rem-2",
        "C_SSP2-rem-10",
        "PkBudg500",
        "PkBudg1000",
        "SSP2-NPi_2026-10-18_10.00.00",
    ]
    assert calls["user"] == "alice" and calls["color"] is True and calls["sort_by_time"] is False
    assert err.lines[0].startswith("ℹ Update 1:")
    assert err.lines[2] == f"ℹ Did you know? {cli.HINTS[0]}"
    assert err.lines[3] == "ℹ Runs found: 7"


def test_flags(output: Path, calls: dict[str, Any], shell: FakeShell, err: CapturedConsole) -> None:
    assert main(["-btf", "PkBudg", "-u", "bob"]) == 0
    assert names(calls) == ["PkBudg500", "PkBudg1000"]
    assert calls["color"] is False and calls["sort_by_time"] is True and calls["user"] == "bob"
    assert main(["-f", "PkBudg5,NPi", "-s"]) == 0
    assert calls["kind"] == "sanity" and names(calls) == ["PkBudg500", "SSP2-NPi_2026-10-18_10.00.00"]


def test_paths_argument(output: Path, calls: dict[str, Any], shell: FakeShell, err: CapturedConsole) -> None:
    assert main([f"{output / 'PkBudg500'},{output / 'archive'}"]) == 0
    assert names(calls) == ["PkBudg500"]  # a bare folder given explicitly lists its (absent) subfolders
    assert main([str(output)]) == 0
    assert len(names(calls)) == 7


def test_no_runs_found(tmp_path: Path, calls: dict[str, Any], shell: FakeShell, err: CapturedConsole) -> None:
    assert main([str(tmp_path / "empty-dir"), "-f", "nothing"]) == 0
    assert "kind" not in calls
    assert err.lines[-1] == "! No runs found"


def test_magpie_adds_the_magpie_output_folders(
    output: Path, calls: dict[str, Any], shell: FakeShell, err: CapturedConsole
) -> None:
    make_run(output.parent / "magpie" / "output", "C_SSP2-mag-1")
    make_run(output.parent / "magpie" / "output", "C_SSP2-mag-2")
    assert main(["-m"]) == 0
    assert names(calls) == [
        "C_SSP2-mag-1",
        "C_SSP2-rem-1",
        "C_SSP2-mag-2",
        "C_SSP2-rem-2",
        "C_SSP2-rem-10",
    ]  # BUG-074
    make_run(output, "C_SSP2-base", RUN_FILES)  # kept by ^C_ alone
    make_run(output, "SSP2-rem-4", RUN_FILES)  # kept by the suffix alone
    assert main(["-m"]) == 0
    assert set(names(calls)) >= {"C_SSP2-base", "SSP2-rem-4"} and "PkBudg500" not in names(calls)
    assert main(["-m", "-f", "nothing"]) == 0
    assert err.lines[-1] == "! No runs found"  # without -l there is no "No coupled runs found"


def test_last_iteration_only(
    output: Path, calls: dict[str, Any], shell: FakeShell, err: CapturedConsole
) -> None:
    assert main(["-ml"]) == 0
    assert names(calls) == ["C_SSP2-rem-10"]
    assert main(["-mlf", "PkBudg"]) == 0
    assert err.lines[-1] == "! No runs found"
    assert main([str(output / "PkBudg500"), "-ml"]) == 0
    assert err.lines[-1] == "! No coupled runs found"
    assert main(["-l"]) == 0  # -l without -m has no effect
    assert len(names(calls)) == 7
    make_run(
        output, "C_SSP2-mag-3", {"full.gms": "", "log.txt": "", "config.Rdata": "", "prepare_and_run.R": ""}
    )
    assert main(["-ml"]) == 0
    assert names(calls) == ["C_SSP2-mag-3", "C_SSP2-rem-10"]  # one per kind (BUG-050)


def test_current_option(
    output: Path, calls: dict[str, Any], shell: FakeShell, err: CapturedConsole, clock: object
) -> None:
    assert main(["-C"]) == 0
    assert (
        err.lines[-1]
        == "! No currently running runs found. To include recent runs please expand the time horizon by adding -d DAYS."
    )
    assert main(["-C", "-d", "3"]) == 0
    assert err.lines[-1] == "! No runs found in the past 3 days. Try to expand the time horizon."
    shell.squeue_answers.extend([[str(output / "PkBudg500")], ["PkBudg500"]])
    assert main(["-C", "-u", "bob"]) == 0
    assert names(calls) == ["PkBudg500"] and calls["user"] == "bob"
    assert shell.argvs[-1] == ["squeue", "-u", "bob", "-h", "-o", "%j"]


def test_amt_option(
    output: Path,
    calls: dict[str, Any],
    shell: FakeShell,
    err: CapturedConsole,
    monkeypatch: pytest.MonkeyPatch,
    data_dir: Path,
) -> None:
    amt = output.parent / "amt"
    for name in (
        "SSP2-NPi-AMT_2026-10-18_10.00.00",
        "SSP2-NPi-AMT_2026-10-11_10.00.00",
        "default-AMT_2026-10-19_01.00.00",
    ):
        make_run(amt, name)
    make_run(amt / "archive", "SSP2-NPi-AMT_2026-01-01_10.00.00")
    monkeypatch.setattr(_shell, "AMT_OUTPUT", amt)
    monkeypatch.setattr(_shell, "AMT_RUNCODE", data_dir / "runcode.rds")
    assert main(["-A"]) == 0
    assert names(calls) == ["default-AMT_2026-10-19_01.00.00", "SSP2-NPi-AMT_2026-10-18_10.00.00"]
    assert calls["user"] is None
    assert f"ℹ Results from '{amt}/'" in err.lines  # quoted like cli's {.file} without colour
    assert err.lines[err.lines.index(f"ℹ Results from '{amt}/'") + 1] == "ℹ Runs found: 2"  # no blank line
    assert main(["-A", "-f", "2026-01-01"]) == 0
    assert names(calls) == ["SSP2-NPi-AMT_2026-01-01_10.00.00"]
    assert f"ℹ Results from '{amt}/' and '{amt / 'archive'}'" in err.lines
    assert main(["-A", "-f", ".*,"]) == 0  # a trailing comma is dropped: no filter, no archive
    assert names(calls) == ["default-AMT_2026-10-19_01.00.00", "SSP2-NPi-AMT_2026-10-18_10.00.00"]
    assert main(["-As"]) == 0
    assert calls["kind"] == "sanity"


def test_prompt_selects_runs(
    output: Path,
    calls: dict[str, Any],
    shell: FakeShell,
    err: CapturedConsole,
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    monkeypatch.setattr(cli, "choose_from_list", lambda items, kind: items[:2])
    assert main(["-p"]) == 0
    assert names(calls) == ["archive", "C_SSP2-rem-1"]
    assert err.lines[-1] == "ℹ Runs found: 2"


def test_hint_for_many_runs(
    tmp_path: Path, calls: dict[str, Any], shell: FakeShell, err: CapturedConsole
) -> None:
    for number in range(41):
        make_run(tmp_path / "many", f"run{number:02d}")
    assert main([str(tmp_path / "many")]) == 0
    assert (
        "ℹ To reduce the number of runs, filter the runs with -f REGEX or select manually from the list with -p."
        in err.lines
    )


def test_errors_exit_with_status_1(
    output: Path,
    calls: dict[str, Any],
    shell: FakeShell,
    err: CapturedConsole,
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    def failing(*args: object, **kwargs: object) -> None:
        raise ModelstatsError("boom")

    monkeypatch.setattr(cli, "print_run_table", failing)
    assert main([]) == 1
    assert err.lines[-1] == "Error: boom"


def test_help(shell: FakeShell, capsys: pytest.CaptureFixture[str]) -> None:
    assert main(["-h"]) == 0
    text = capsys.readouterr().out
    assert "Usage: rs [OPTIONS] [PATH]" in text
    assert "--nocolor" in text and "Bugs and feedback" in text
    assert "[default" not in text  # optparse never showed defaults (BUG-051)
    assert "[OPTION] see below." in text and "to have a corresponding argument." in text


def test_unknown_option_is_a_usage_error(shell: FakeShell, capsys: pytest.CaptureFixture[str]) -> None:
    assert main(["--bogus"]) == 2
    assert "No such option" in capsys.readouterr().err


def test_hints_are_complete() -> None:
    assert (
        len(cli.HINTS) == 16
        and cli.HINTS[-1] == "Get a more detailed assessment of a specific run: remindstatus folder"
    )


def test_undecodable_folder_names_reach_stdout(
    output: Path, shell: FakeShell, err: CapturedConsole, monkeypatch: pytest.MonkeyPatch
) -> None:
    raw = io.BytesIO()
    monkeypatch.setattr(sys, "stdout", io.TextIOWrapper(raw, encoding="utf-8"))
    make_run(output, "run-" + os.fsdecode(b"\xe9"), RUN_FILES)

    def print_names(dirs: list[Path], **kwargs: object) -> None:
        for run_dir in dirs:
            _console.out.print(Text(str(run_dir)))

    monkeypatch.setattr(cli, "print_run_table", print_names)
    assert main([]) == 0
    sys.stdout.flush()
    assert b"output/run-\xe9\n" in raw.getvalue()


def test_stdout_carries_only_the_table(
    output: Path,
    shell: FakeShell,
    err: CapturedConsole,
    capsys: pytest.CaptureFixture[str],
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    monkeypatch.setattr(cli, "print_run_table", lambda dirs, **kw: _console.out.print(Text("TABLE")))
    monkeypatch.setattr(sys, "stdin", io.StringIO("1\n"))
    assert main(["-p"]) == 0
    assert capsys.readouterr().out == "TABLE\n"
    assert "1: output/archive" in err.lines and err.lines[-2].endswith("Selected: output/archive")


def test_prompt_reads_one_line_of_stdin_per_prompt(
    output: Path,
    calls: dict[str, Any],
    shell: FakeShell,
    err: CapturedConsole,
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    monkeypatch.setattr(sys, "stdin", io.StringIO("p\nPkBudg\n"))  # BUG-106: R's getLine() lost the 2nd line
    assert main(["-p"]) == 0
    assert names(calls) == ["PkBudg500", "PkBudg1000"]
    monkeypatch.setattr(sys, "stdin", io.StringIO("9\n3\n"))  # an answer out of range, then the second line
    assert main(["-p"]) == 0
    assert names(calls) == ["C_SSP2-rem-2"]


def test_bracketed_folder_names_in_the_prompt(
    output: Path, shell: FakeShell, err: CapturedConsole, monkeypatch: pytest.MonkeyPatch
) -> None:
    make_run(output, "run[bold]x", RUN_FILES)
    monkeypatch.setattr(cli, "print_run_table", lambda dirs, **kw: None)
    monkeypatch.setattr(sys, "stdin", io.StringIO("all\n"))
    assert main(["-p", "-f", "run"]) == 0
    assert "1: output/run[bold]x" in err.lines  # not rich markup


def test_invalid_filter_pattern(
    output: Path, calls: dict[str, Any], shell: FakeShell, err: CapturedConsole
) -> None:
    assert main(["-f", "PkBudg["]) == 1
    assert err.lines[-1].startswith("Error: -f: invalid pattern")


def test_trailing_commas_and_empty_elements(
    output: Path, calls: dict[str, Any], shell: FakeShell, err: CapturedConsole
) -> None:
    assert main(["-f", "PkBudg,"]) == 0
    assert names(calls) == ["PkBudg500", "PkBudg1000"]
    assert main([f"{output / 'PkBudg500'},"]) == 0
    assert names(calls) == ["PkBudg500"]
    assert main([f"{output / 'PkBudg500'},,{output / 'PkBudg1000'}"]) == 0
    assert names(calls) == ["PkBudg500", "PkBudg1000"]
    calls.clear()
    assert main([""]) == 0
    assert "kind" not in calls and err.lines[-1] == "! No runs found"


def test_filter_sees_normalised_paths(
    output: Path, calls: dict[str, Any], shell: FakeShell, err: CapturedConsole
) -> None:
    # BUG-073: R matched "./output/PkBudg500"; the port matches "output/PkBudg500"
    assert main(["-f", "^output/Pk"]) == 0
    assert names(calls) == ["PkBudg500", "PkBudg1000"]


def test_hint_for_many_runs_is_suppressed(
    tmp_path: Path,
    calls: dict[str, Any],
    shell: FakeShell,
    err: CapturedConsole,
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    for number in range(41):
        make_run(tmp_path / "many", f"run{number:02d}")
    hint = "ℹ To reduce the number of runs, filter the runs with -f REGEX or select manually from the list with -p."
    assert main([str(tmp_path / "many"), "-f", "run"]) == 0
    assert hint not in err.lines
    monkeypatch.setattr(cli, "choose_from_list", lambda items, kind: items[:1])
    assert main([str(tmp_path / "many"), "-p"]) == 0
    assert hint not in err.lines
    (tmp_path / "many" / "run40").rmdir()
    assert main([str(tmp_path / "many")]) == 0
    assert hint not in err.lines  # exactly 40 runs: no hint


def test_amt_with_current_asks_for_the_current_user(
    output: Path,
    calls: dict[str, Any],
    shell: FakeShell,
    err: CapturedConsole,
    monkeypatch: pytest.MonkeyPatch,
    data_dir: Path,
) -> None:
    monkeypatch.setattr(_shell, "AMT_RUNCODE", data_dir / "runcode.rds")
    monkeypatch.setattr(_shell, "AMT_OUTPUT", output)
    assert main(["-A", "-C"]) == 0  # BUG-075
    assert shell.argvs[0] == ["squeue", "-u", "alice", "-h", "-o", "%Z"]


def test_sanity_ignores_nocolor(
    output: Path, shell: FakeShell, err: CapturedConsole, monkeypatch: pytest.MonkeyPatch
) -> None:
    seen: list[object] = []
    monkeypatch.setattr(cli, "print_sanity_checks", lambda dirs, out=None: seen.append(out))
    assert main(["-sb"]) == 0
    assert seen == [None]  # BUG-077: the default console, colours as R kept them


def test_extra_work_dirs_are_kept(tmp_path: Path, shell: FakeShell) -> None:
    run, extra = make_run(tmp_path, "SSP2-NPi"), make_run(tmp_path, "run+1")
    shell.squeue_answers.extend([[str(run), str(extra), ""], ["SSP2-NPi"]])
    assert current_run_dirs("alice") == [extra, run]  # BUG-053; "" is no folder


def test_extra_job_names_are_dropped(tmp_path: Path, shell: FakeShell) -> None:
    run = make_run(tmp_path, "SSP2-NPi")
    shell.squeue_answers.extend([[str(run)], ["SSP2-NPi", "more", "batch"]])
    assert current_run_dirs("alice") == [run]  # BUG-053: R stopped with a subscript error
    shell.squeue_answers.extend([[str(run)], []])
    assert current_run_dirs("alice") == [run]  # BUG-053: R stopped on 1:0


def test_current_with_magpie(
    output: Path, calls: dict[str, Any], shell: FakeShell, err: CapturedConsole
) -> None:
    make_run(Path("magpie") / "output", "C_SSP2-mag-1")
    make_run(Path("magpie") / "output", "C_SSP2-mag-2")
    shell.squeue_answers.extend([[str(output / "C_SSP2-rem-2")], ["C_SSP2-rem-2"]])
    assert main(["-Cml"]) == 0
    assert names(calls) == ["C_SSP2-mag-2", "C_SSP2-rem-2"]


def test_empty_paths_skip_the_magpie_block(
    output: Path, calls: dict[str, Any], shell: FakeShell, err: CapturedConsole
) -> None:
    make_run(Path("magpie") / "output", "C_SSP2-mag-1")
    assert main(["", "-ml"]) == 0  # R: opt$magpie & !is.null(runfolders)
    assert "kind" not in calls and err.lines[-1] == "! No runs found"


def test_empty_user_means_the_current_user(
    output: Path, calls: dict[str, Any], shell: FakeShell, err: CapturedConsole
) -> None:
    shell.squeue_answers.extend([[str(output / "PkBudg500")], ["PkBudg500"]])
    assert main(["-C", "-u", ""]) == 0  # BUG-075
    assert shell.argvs[0] == ["squeue", "-u", "alice", "-h", "-o", "%Z"]
    assert calls["user"] == "alice"


def test_tilde_in_paths(
    tmp_path: Path,
    calls: dict[str, Any],
    shell: FakeShell,
    err: CapturedConsole,
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    monkeypatch.setenv("HOME", str(tmp_path))
    make_run(tmp_path / "mine", "run1", RUN_FILES)
    assert main(["~/mine/run1"]) == 0
    assert calls["dirs"] == [tmp_path / "mine" / "run1"]
    assert main(["-f", "run1,", "~/mine"]) == 0
    assert calls["dirs"] == [tmp_path / "mine" / "run1"]


def test_results_from_in_colour(
    output: Path, calls: dict[str, Any], shell: FakeShell, monkeypatch: pytest.MonkeyPatch, data_dir: Path
) -> None:
    monkeypatch.setattr(_shell, "AMT_RUNCODE", data_dir / "runcode.rds")
    monkeypatch.setattr(_shell, "AMT_OUTPUT", output)
    captured = capture_console(color=True)
    monkeypatch.setattr(_console, "err", captured.console)
    assert main(["-A"]) == 0
    assert f"\x1b[34m{output}/\x1b[0m" in captured.text  # blue, unquoted, as cli's {.file}


def test_amt_without_runcode(
    output: Path,
    calls: dict[str, Any],
    shell: FakeShell,
    err: CapturedConsole,
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    monkeypatch.setattr(_shell, "AMT_RUNCODE", output / "missing.rds")
    monkeypatch.setattr(_shell, "AMT_OUTPUT", output)
    assert main(["-A"]) == 1
    assert err.lines[-1].startswith("Error: ")


def test_leading_quantifier_is_an_error(
    output: Path, calls: dict[str, Any], shell: FakeShell, err: CapturedConsole
) -> None:
    assert main(["-f", "*SSP2*"]) == 1  # BUG-072: TRE ignored the leading quantifier
    assert err.lines[-1].startswith("Error: -f: invalid pattern '*SSP2*'")
    assert main(["-f", "a{4294967296}"]) == 1  # an OverflowError of re is an invalid pattern too
    assert err.lines[-1].startswith("Error: -f: invalid pattern 'a{4294967296}'")


@pytest.mark.usefixtures("clock")
def test_days_back_before_the_beginning_of_time(shell: FakeShell, err: CapturedConsole) -> None:
    with pytest.raises(ModelstatsError, match="before the beginning of time"):
        current_run_dirs("alice", 800000)
    assert main(["-C", "-d", "800000"]) == 1
    assert err.lines[-1] == "Error: 800000 days back is before the beginning of time"


def test_same_named_runs_keep_a_stable_order(tmp_path: Path, shell: FakeShell) -> None:
    runs = [make_run(tmp_path / letter / "output", "SSP2") for letter in "cab"]
    shell.squeue_answers.extend([[str(tmp_path / letter) for letter in "cab"], ["SSP2"] * 3])
    assert current_run_dirs("alice") == sorted(runs)  # R: sort(unique(myruns)), ties by full path


def test_magpie_output_given_as_path_is_listed_once(
    output: Path, calls: dict[str, Any], shell: FakeShell, err: CapturedConsole
) -> None:
    for name in ("C_SSP2-mag-1", "C_SSP2-mag-2"):
        make_run(Path("magpie") / "output", name)
    assert main(["magpie/output", "-m"]) == 0  # BUG-049: not twice
    assert names(calls) == ["C_SSP2-mag-1", "C_SSP2-mag-2"]
    assert err.lines[-1] == "ℹ Runs found: 2"


def test_empty_folder_list_runs_the_magpie_block(
    output: Path, calls: dict[str, Any], shell: FakeShell, err: CapturedConsole
) -> None:
    make_run(Path("magpie") / "output", "C_SSP2-mag-1")
    assert main(["nonexistent", "-ml"]) == 0  # R: character(0) is not NULL
    assert names(calls) == ["C_SSP2-mag-1"]
    assert main([",", "-ml"]) == 0
    assert names(calls) == ["C_SSP2-mag-1"]


@pytest.mark.usefixtures("clock")
def test_bundled_short_options(
    output: Path, calls: dict[str, Any], shell: FakeShell, err: CapturedConsole
) -> None:
    assert main(["-Cd2"]) == 0  # BUG-051: optparse read -2 here
    assert shell.argvs[2][:3] == ["sacct", "-u", "alice"]
    assert main(["-Cubob"]) == 0
    assert shell.argvs[-1][1:3] == ["-u", "bob"]


def test_posix_classes_are_an_error(
    output: Path, calls: dict[str, Any], shell: FakeShell, err: CapturedConsole
) -> None:
    assert main(["-f", "[[:digit:]]000"]) == 1  # BUG-072: TRE knew the class, re only warns
    assert err.lines[-1].startswith("Error: -f: invalid pattern '[[:digit:]]000'")
    assert main(["-f", "\\<Pk"]) == 0  # a literal <: nothing found
    assert err.lines[-1] == "! No runs found"


def test_magpie_output_given_as_absolute_path_is_listed_once(
    output: Path, calls: dict[str, Any], shell: FakeShell, err: CapturedConsole
) -> None:
    make_run(Path("magpie") / "output", "C_SSP2-mag-1")
    assert main([str(Path.cwd() / "magpie" / "output"), "-m"]) == 0  # BUG-049: not twice
    assert names(calls) == ["C_SSP2-mag-1"]


def test_amt_with_magpie(
    output: Path,
    calls: dict[str, Any],
    shell: FakeShell,
    err: CapturedConsole,
    monkeypatch: pytest.MonkeyPatch,
    data_dir: Path,
) -> None:
    monkeypatch.setattr(_shell, "AMT_RUNCODE", data_dir / "runcode.rds")
    monkeypatch.setattr(_shell, "AMT_OUTPUT", output)
    make_run(output, "C_SSP2-AMT_2026-10-18-rem-1", RUN_FILES)  # matches the runcode of the fixture
    make_run(Path("magpie") / "output", "C_SSP2-AMT_2026-10-18-mag-1")
    assert main(["-Am"]) == 0  # the -m block runs for -A: magpie/output of the current folder is added
    assert names(calls) == ["C_SSP2-AMT_2026-10-18-mag-1", "C_SSP2-AMT_2026-10-18-rem-1"]


def test_interrupt_at_the_prompt(
    output: Path,
    calls: dict[str, Any],
    shell: FakeShell,
    err: CapturedConsole,
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    def interrupted(items: list[str], kind: str) -> list[str]:
        raise KeyboardInterrupt

    monkeypatch.setattr(cli, "choose_from_list", interrupted)
    assert main(["-p"]) == 130  # BUG-094


def test_current_runs_are_unique(tmp_path: Path, shell: FakeShell) -> None:
    run = make_run(tmp_path, "SSP2-NPi")
    shell.squeue_answers.extend([[str(run), str(run)], ["SSP2-NPi", "SSP2-NPi"]])
    assert current_run_dirs("alice") == [run]  # R: unique(myruns)


def test_long_options_are_spelled_out(shell: FakeShell, capsys: pytest.CaptureFixture[str]) -> None:
    assert main(["--cur"]) == 2  # BUG-051: optparse accepted a unique prefix
    assert main(["-C", "-d", "1e2"]) == 2  # and read 1e2 as 100


def test_hint_is_one_of_the_hints(monkeypatch: pytest.MonkeyPatch) -> None:
    monkeypatch.undo()  # the autouse fixture replaced _hint
    assert cli._hint() in cli.HINTS


@pytest.mark.usefixtures("clock")
def test_option_values_typer_style(
    output: Path, calls: dict[str, Any], shell: FakeShell, err: CapturedConsole
) -> None:
    # BUG-051: optparse refused these; typer takes the token as the value
    assert main(["-f", "-rem-"]) == 0
    assert names(calls) == ["C_SSP2-rem-1", "C_SSP2-rem-2", "C_SSP2-rem-10"]
    assert main(["-f=Pk"]) == 0
    assert err.lines[-1] == "! No runs found"  # the pattern is "=Pk"
    assert main(["-C", "-d3"]) == 0  # 3 days, not -3
    assert shell.argvs[2][:3] == ["sacct", "-u", "alice"]
    assert main(["-C", "-d", "1_0"]) == 0 and shell.argvs[-1][6] == "2026-10-08"  # 10 days
