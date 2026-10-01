"""amt.__init__.modeltests and amt.cli: the ``.testsstatus`` dispatcher (``R/modeltests.R`` lines 24-55) and the
``modeltests`` console script around it.

Self-contained (a small tree in ``tmp_path``, the two steps replaced through the module seams
``modelstats.amt.start_runs`` / ``modelstats.amt.evaluate_runs``), no R needed. Written by unit p5-cli-goldens,
placed here by the phase 5 bookkeeper.

R facts pinned (R 4.6.1, 2026-10-01): a missing ``../.testsstatus`` warns ``cannot open file ...`` and fails with
``cannot open the connection`` (call ``file(con, "r")``); an empty file is ``argument is of length zero``, two lines
``the condition has length > 1`` (call ``if (readLines("../.testsstatus") == "next:start") {``); a final line
without a newline warns ``incomplete final line found on '../.testsstatus'``; ``withr::local_dir`` of a missing
directory is ``cannot change working directory`` (call ``setwd(dir = new)``).
"""

from __future__ import annotations

import os
import warnings
from pathlib import Path

import pytest
from typer.testing import CliRunner

import modelstats.amt as amt
from modelstats.amt import cli
from modelstats.amt.cli import AmtDryRunEffects, Options, app, is_bridge_argv, run_cli
from modelstats.env import DryRunEffects, ProductionEffects
from modelstats.errors import RParityError, RWarning


@pytest.fixture
def tree(tmp_path: Path) -> Path:
    """``<tmp>/amt/.testsstatus`` beside ``<tmp>/amt/remind/output``; returns the model directory."""
    model = tmp_path / "amt" / "remind"
    (model / "output").mkdir(parents=True)
    return model


def _status(model: Path, text: str) -> None:
    (model.parent / ".testsstatus").write_bytes(text.encode())


def _read_status(model: Path) -> str:
    return (model.parent / ".testsstatus").read_text()


class _Steps:
    """Records the calls of the two steps (and the cwd at the call) in place of the real ones."""

    def __init__(self) -> None:
        self.calls: list[tuple[str, tuple[object, ...], str]] = []

    def start(self, model: object, mydir: object, user: object, effects: object = None) -> None:
        self.calls.append(("start", (model, mydir, user), os.getcwd()))

    def evaluate(self, *args: object, effects: object = None) -> None:
        self.calls.append(("evaluate", tuple(args), os.getcwd()))


@pytest.fixture
def steps(monkeypatch: pytest.MonkeyPatch) -> _Steps:
    recorder = _Steps()
    monkeypatch.setattr(amt, "start_runs", recorder.start)
    monkeypatch.setattr(amt, "evaluate_runs", recorder.evaluate)
    return recorder


# ---------------------------------------------------------------------------
# modeltests()
# ---------------------------------------------------------------------------


def test_next_start_calls_start_runs_in_mydir_and_writes_next_evaluate(
    tree: Path, steps: _Steps, capsys: pytest.CaptureFixture[str]
) -> None:
    _status(tree, "next:start\n")
    mydir = f"{tree}/"
    amt.modeltests(mydir=mydir, model="REMIND", user="alice", effects=ProductionEffects())
    assert steps.calls == [("start", ("REMIND", mydir, "alice"), str(tree))]
    assert _read_status(tree) == "next:evaluate\n"
    err = capsys.readouterr().err
    assert err.startswith(f"\n{amt.BANNER}\nBegin of AMT procedure ")
    assert f" in {mydir}\n{amt.BANNER}\n\n" in err
    assert f"Found 'next:start' in {tree.parent / '.testsstatus'}\nCalling 'startRuns'\n" in err
    assert err.endswith(f"Writing 'next:evaluate' to {tree.parent / '.testsstatus'}\n")


def test_next_evaluate_calls_evaluate_runs_in_output_and_writes_next_start(
    tree: Path, steps: _Steps, capsys: pytest.CaptureFixture[str]
) -> None:
    _status(tree, "next:evaluate\n")
    mydir = f"{tree}/"
    amt.modeltests(
        mydir=mydir, gitdir="/git", model="MAgPIE", user="bob", email=False, comp_scen=False, mattermost_token="tok"
    )
    assert steps.calls == [("evaluate", ("MAgPIE", mydir, False, False, "tok", "/git", "bob"), str(tree / "output"))]
    assert _read_status(tree) == "next:start\n"
    err = capsys.readouterr().err
    assert "Found 'next:evaluate' in" in err and "Calling 'evaluateRuns'" in err
    assert err.endswith(f"Writing 'next:start' to {tree.parent / '.testsstatus'}\n")
    assert os.getcwd() != str(tree / "output")  # the directories are restored


def test_other_content_does_nothing(tree: Path, steps: _Steps, capsys: pytest.CaptureFixture[str]) -> None:
    _status(tree, "next:foo\n")
    amt.modeltests(mydir=f"{tree}/", model="REMIND")
    assert steps.calls == []
    assert _read_status(tree) == "next:foo\n"
    assert capsys.readouterr().err.endswith(f"Found 'next:foo' in {tree.parent / '.testsstatus'}. Doing nothing\n")


def test_missing_status_file_is_cannot_open_the_connection(tree: Path, steps: _Steps) -> None:
    with warnings.catch_warnings(record=True) as caught:
        warnings.simplefilter("always")
        with pytest.raises(RParityError, match="^cannot open the connection$") as info:
            amt.modeltests(mydir=f"{tree}/", model="REMIND")
    assert info.value.call == 'file(con, "r")'
    texts = [(w.message.call, w.message.text) for w in caught if isinstance(w.message, RWarning)]
    assert texts == [('file(con, "r")', "cannot open file '../.testsstatus': No such file or directory")]
    assert steps.calls == []


@pytest.mark.parametrize(
    ("content", "message"),
    [("", "argument is of length zero"), ("next:start\nnext:start\n", "the condition has length > 1")],
)
def test_empty_or_multi_line_status_fails_like_r_if(tree: Path, steps: _Steps, content: str, message: str) -> None:
    _status(tree, content)
    with pytest.raises(RParityError, match=f"^{message}$") as info:
        amt.modeltests(mydir=f"{tree}/", model="REMIND")
    assert info.value.call == 'if (readLines("../.testsstatus") == "next:start") {'
    assert steps.calls == []


def test_incomplete_final_line_warns_and_still_dispatches(tree: Path, steps: _Steps) -> None:
    _status(tree, "next:start")  # no newline
    with warnings.catch_warnings(record=True) as caught:
        warnings.simplefilter("always")
        amt.modeltests(mydir=f"{tree}/", model="REMIND")
    assert [c[0] for c in steps.calls] == ["start"]
    rw = [w.message for w in caught if isinstance(w.message, RWarning)]
    assert rw and all(w.text == "incomplete final line found on '../.testsstatus'" for w in rw)


def test_missing_mydir_is_setwd_error(tmp_path: Path, steps: _Steps) -> None:
    with pytest.raises(RParityError, match="^cannot change working directory$") as info:
        amt.modeltests(mydir=str(tmp_path / "nope"), model="REMIND")
    assert info.value.call == "setwd(dir = new)"


def test_read_status_lines_splits_like_readlines(tree: Path) -> None:
    _status(tree, "a\r\nb\rc\n")
    with amt.default_effects().chdir(tree):
        assert amt.read_status_lines() == ["a", "b", "c"]


# ---------------------------------------------------------------------------
# the console script
# ---------------------------------------------------------------------------


def test_run_cli_success_and_token_from_environment(
    tree: Path, steps: _Steps, monkeypatch: pytest.MonkeyPatch, capsys: pytest.CaptureFixture[str]
) -> None:
    _status(tree, "next:evaluate\n")
    monkeypatch.setenv("AMT_HOOK", "https://hook")
    status = run_cli(Options(mydir=f"{tree}/", model="REMIND", mattermost_token_env="AMT_HOOK", email=False))
    assert status == 0
    assert steps.calls[0][1][4] == "https://hook"
    assert "Execution halted" not in capsys.readouterr().err


def test_run_cli_unset_token_variable_sends_nothing_and_says_so(
    tree: Path, steps: _Steps, monkeypatch: pytest.MonkeyPatch, capsys: pytest.CaptureFixture[str]
) -> None:
    _status(tree, "next:evaluate\n")
    monkeypatch.delenv("AMT_HOOK", raising=False)
    assert run_cli(Options(mydir=f"{tree}/", model="REMIND", mattermost_token_env="AMT_HOOK")) == 0
    assert steps.calls[0][1][4] is None
    assert "the environment variable AMT_HOOK is not set or empty" in capsys.readouterr().err


def test_run_cli_r_error_prints_rscript_block_and_exits_1(
    tree: Path, steps: _Steps, capsys: pytest.CaptureFixture[str]
) -> None:
    assert run_cli(Options(mydir=f"{tree}/", model="REMIND")) == 1  # no .testsstatus
    err = capsys.readouterr().err
    assert err.endswith(
        'Error in file(con, "r") : cannot open the connection\n'
        "In addition: Warning message:\n"
        'In file(con, "r") :\n'
        "  cannot open file '../.testsstatus': No such file or directory\n"
        "Execution halted\n"
    )


def test_run_cli_prints_deferred_warnings_at_the_end(
    tree: Path, steps: _Steps, capsys: pytest.CaptureFixture[str]
) -> None:
    _status(tree, "next:foo")  # incomplete final line: three readLines() calls in R, three warnings
    assert run_cli(Options(mydir=f"{tree}/", model="REMIND")) == 0
    err = capsys.readouterr().err
    assert "Doing nothing\n" in err
    assert 'Warning messages:\n1: In readLines("../.testsstatus") :' in err


def test_dry_run_uses_the_dry_run_effects_and_touches_nothing(
    tree: Path, monkeypatch: pytest.MonkeyPatch, capsys: pytest.CaptureFixture[str]
) -> None:
    _status(tree, "next:start\n")
    seen: list[object] = []

    def fake_start(model: object, mydir: object, user: object, effects: object = None) -> None:
        seen.append(effects)
        assert isinstance(effects, AmtDryRunEffects)
        effects.write_text("runcode.rds", "x")  # logged, not written

    monkeypatch.setattr(amt, "start_runs", fake_start)
    assert run_cli(Options(mydir=f"{tree}/", model="REMIND", dry_run=True)) == 0
    assert seen and isinstance(seen[0], DryRunEffects)
    assert _read_status(tree) == "next:start\n"  # the 'next:evaluate' write was only logged
    assert not (tree / "runcode.rds").exists()
    err = capsys.readouterr().err
    assert f"dry run: would write {tree / 'runcode.rds'} (1 bytes)" in err  # absolute: logged from inside mydir
    assert f"dry run: would write {tree.parent / '.testsstatus'} (14 bytes)" in err
    assert err.endswith("dry run: 2 mutation(s) logged, nothing was changed\n")


def test_is_bridge_argv() -> None:
    assert is_bridge_argv(["Rscript", "/pkg/bridge_scripts/select_scenarios.R", "--out", "x"])
    assert is_bridge_argv(["/usr/bin/Rscript", "/pkg/bridge_scripts/add_to_data_changelog.R"])
    assert not is_bridge_argv(["Rscript", "start.R"])
    assert not is_bridge_argv(["git", "log", "-1"])


def test_dry_run_effects_execute_bridges_and_log_everything_else(monkeypatch: pytest.MonkeyPatch) -> None:
    executed: list[list[str]] = []

    def fake_run(self: object, argv: object, *args: object, **kwargs: object) -> object:
        executed.append(list(argv))  # type: ignore[call-overload]
        import subprocess

        return subprocess.CompletedProcess(list(argv), 0, '{"bridge": "x", "ok": true}\n', "")  # type: ignore[call-overload]

    monkeypatch.setattr(ProductionEffects, "run", fake_run)
    eff = AmtDryRunEffects()
    bridge = ["Rscript", "/pkg/bridge_scripts/select_scenarios.R", "--out", "/tmp/x.rds"]
    assert eff.run(bridge).returncode == 0
    assert executed == [bridge]
    assert eff.run_shell("git log -1").stdout.startswith("commit 0000000")
    assert executed == [bridge]  # not executed
    assert [e.action for e in eff.events] == ["run (executed: a read-only bridge)", "run"]


def test_help_lists_every_option() -> None:
    result = CliRunner().invoke(app, ["--help"])
    assert result.exit_code == 0
    assert result.output.startswith("Usage: modeltests [OPTION]")
    for flag in cli.OPTION_FLAGS:
        assert flag in result.output, flag


def test_main_maps_usage_errors_to_status_1(monkeypatch: pytest.MonkeyPatch) -> None:
    monkeypatch.setattr("sys.argv", ["modeltests", "--bogus"])
    with pytest.raises(SystemExit) as info:
        cli.main()
    assert info.value.code == 1
