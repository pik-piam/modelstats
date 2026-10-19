from __future__ import annotations

import dataclasses
import fcntl
import importlib
import json
import logging
import os
import shlex
import shutil
import subprocess
from collections import deque
from collections.abc import Callable, Sequence
from datetime import date, datetime
from pathlib import Path
from typing import Any

import pytest

from modelstats import _console, _shell, loop_runs, sanity_checks
from modelstats.errors import ModelstatsError
from modelstats.modeltests import (
    AmtPaths,
    AmtState,
    delete_empty_realization_folders,
    evaluate_runs,
    main,
    modeltests,
    notify_mattermost,
    start_runs,
)
from modelstats.run_status import RunStatus
from tests.conftest import BERLIN, FIXED_NOW, CapturedConsole, FakeShell, make_run

# the package re-exports the function under the module's name
amt = importlib.import_module("modelstats.modeltests")

SQUEUE = tuple(_shell.squeue_argv(amt.SQUEUE_FORMAT, user="alice"))
SELECT_SCENARIOS = ("Rscript", "-e", amt._SELECT_SCENARIOS_R)
HOOK = "https://chat.example/hooks/secret"


@pytest.fixture
def paths(tmp_path: Path) -> AmtPaths:
    """A REMIND checkout at tmp/modeltests/remind with an output folder, like the cluster layout."""
    model_dir = tmp_path / "modeltests" / "remind"
    (model_dir / "output").mkdir(parents=True)
    (model_dir / "config").mkdir()
    (model_dir / "config" / "default.cfg").write_text("cfg$force_download <- FALSE\n")
    (model_dir / "magpie").mkdir()
    (model_dir / "tests").mkdir()
    report = tmp_path / "testing_suite"
    report.mkdir()
    return AmtPaths(model_dir=model_dir, report_repo=report)


@pytest.fixture
def statuses(monkeypatch: pytest.MonkeyPatch) -> dict[str, RunStatus]:
    """Run statuses by folder name; unknown folders get an empty status."""
    table: dict[str, RunStatus] = {}

    def fake(dirs: Path, **_: object) -> list[RunStatus]:
        if dirs.name in table:
            return [dataclasses.replace(table[dirs.name], name=dirs.name, path=dirs)]
        return [RunStatus(name=dirs.name, path=dirs, job_in_slurm="no", warnings="NA", conv="NA")]

    monkeypatch.setattr(amt, "get_run_status", fake)
    return table


@pytest.fixture
def quiet_tables(monkeypatch: pytest.MonkeyPatch) -> list[str]:
    """Replace the table printers used for the Mattermost message by one-line stand-ins."""
    captured: list[str] = []

    def table(dirs: list[Path], **kwargs: Any) -> None:
        kwargs["out"].print("TABLE " + " ".join(d.name for d in dirs))
        captured.append("table")

    def sanity(dirs: object = None, *, out: Any = None) -> None:
        assert dirs is None  # the AMT branch, with its "Results from" line
        out.print("SANITY")
        captured.append("sanity")

    monkeypatch.setattr(amt, "print_run_table", table)
    monkeypatch.setattr(amt, "print_sanity_checks", sanity)
    return captured


@pytest.fixture
def stdout(out: CapturedConsole, monkeypatch: pytest.MonkeyPatch) -> CapturedConsole:
    monkeypatch.setattr(_console, "out", out.console)
    return out


def good_run(**cells: Any) -> RunStatus:
    status = RunStatus(
        name="",
        path=Path(),
        job_in_slurm="no",
        run_type="nash",
        modelstat="2: Locally Optimal",
        run_in_app_results="yes",
        mif="yes",
        iter="12/50",
        run_status="Normal completion",
        warnings="0",
        conv="converged",
        runtime=12345.0,
    )
    return dataclasses.replace(status, **cells)


def evaluated(paths: AmtPaths, shell: FakeShell, **kwargs: Any) -> str:
    """evaluate_runs() of a REMIND checkout whose git log and state are known."""
    shell.outputs[("git", "log", "-1")] = ["commit abcdef1234567", "Author: someone"]
    shell.outputs[
        ("git", "log", "--merges", "--pretty=oneline", "0000000..abcdef1234567", "--abbrev-commit")
    ] = [
        "1111111 Merge pull request #1 from x/y",
        "2222222 not a merge line",
    ]
    if not paths.state_file.exists():
        AmtState(
            runcode=".*-AMT_2026-10-18|.*-AMT_2026-10-19",
            last_commit="0000000",
            runs_to_start=["SSP2-NPi-AMT", "SSP2-PkBudg1000-AMT"],
        ).save(paths)
    defaults: dict[str, Any] = {"model": "REMIND", "user": "alice", "publish": False, "mattermost_url": None}
    defaults.update(kwargs)
    return evaluate_runs(paths, **defaults)


# --- state and paths ----------------------------------------------------------------------------


def test_paths_are_derived_from_the_model_folder(paths: AmtPaths, tmp_path: Path) -> None:
    assert paths.output_dir == paths.model_dir / "output"
    assert paths.status_file == paths.model_dir.parent / ".testsstatus"
    assert paths.state_file == paths.model_dir / "amt-state.json"
    assert paths.lock_file == paths.model_dir / ".modeltests.lock"
    assert paths.runcode_rds == paths.model_dir / "runcode.rds"
    assert paths.archive_dir == paths.output_dir / "archive"
    assert paths.magpie_dir == paths.model_dir / "magpie"
    assert paths.test_full_log == paths.model_dir / "test-full.log"
    relative = AmtPaths(model_dir=Path(os.path.relpath(paths.model_dir)), report_repo=Path("."))
    assert relative.model_dir == paths.model_dir and relative.report_repo == Path.cwd()


def test_state_round_trip(paths: AmtPaths) -> None:
    assert AmtState.load(paths) == AmtState()
    state = AmtState(
        runcode="x|y",
        last_commit="abc",
        runs_to_start=["a-AMT"],
        runs={"r": {"Conv": "converged", "Mif": None}},
    )
    state.save(paths)
    assert AmtState.load(paths) == state
    assert json.loads(paths.state_file.read_text())["runs"] == {"r": {"Conv": "converged", "Mif": None}}
    assert not paths.runcode_rds.exists() and not list(paths.model_dir.glob("*.tmp"))


def test_state_rejects_damaged_files(paths: AmtPaths) -> None:
    paths.state_file.write_text("")
    with pytest.raises(ModelstatsError, match="not a valid JSON state file"):
        AmtState.load(paths)
    paths.state_file.write_text("[]")
    with pytest.raises(ModelstatsError, match="expected a JSON object"):
        AmtState.load(paths)
    paths.state_file.write_text('{"runs": null, "runs_to_start": null, "runcode": null, "last_commit": null}')
    assert AmtState.load(paths) == AmtState()
    for damaged, message in [
        ('{"runcode": 5}', "'runcode' must be a string or null"),
        ('{"runs_to_start": "abc"}', "'runs_to_start' must be a list of strings"),
        ('{"runs_to_start": [1]}', "'runs_to_start' must be a list of strings"),
        ('{"runs": []}', "'runs' must be an object of objects"),
        ('{"runs": {"a": null}}', "'runs' must be an object of objects"),
        ('{"runs": {"a": {"Conv": ["x"]}}}', "'runs.a.Conv' must be a string or null"),
    ]:
        paths.state_file.write_text(damaged)
        with pytest.raises(ModelstatsError, match=message):
            AmtState.load(paths)
    paths.state_file.write_text('{"runs": {"b": {"Conv": "x"}}}')
    assert AmtState.load(paths).runs == {"b": {"Conv": "x", "Mif": None}}
    paths.state_file.unlink()
    paths.state_file.mkdir()  # cannot be read
    with pytest.raises(ModelstatsError, match="not a valid JSON state file"):
        AmtState.load(paths)


def test_state_migrates_from_the_r_files(paths: AmtPaths, data_dir: Path) -> None:
    shutil.copy(data_dir / "runcode.rds", paths.runcode_rds)
    shutil.copy(data_dir / "lastcommit.rds", paths.model_dir / "lastcommit.rds")
    shutil.copy(data_dir / "runsToStart.rds", paths.model_dir / "runsToStart.rds")
    shutil.copy(data_dir / "gRS.rds", paths.output_dir / "gRS.rds")
    state = AmtState.load(paths)
    assert state.runcode == ".*-AMT_2026-10-18|.*-AMT_2026-10-19"
    assert state.last_commit == "abc1234"
    assert state.runs_to_start == ["SSP2-NPi-AMT", "SSP2-PkBudg1000-AMT"]
    assert state.runs == {
        "SSP2-NPi-AMT_2026-09-01_10.00.00": {"Conv": "converged", "Mif": "yes"},
        "SSP2-NPi-AMT_2026-09-08_10.00.00": {"Conv": "not_converged", "Mif": "no"},
    }
    shutil.copy(data_dir / "runsToStart.rds", paths.runcode_rds)  # a data frame is no runcode
    with pytest.raises(ModelstatsError, match="expected one string"):
        AmtState.load(paths)


# --- modeltests dispatch ------------------------------------------------------------------------


@pytest.mark.usefixtures("clock")
def test_dispatch_on_the_status_file(
    paths: AmtPaths, shell: FakeShell, monkeypatch: pytest.MonkeyPatch, caplog: pytest.LogCaptureFixture
) -> None:
    calls: list[str] = []
    monkeypatch.setattr(amt, "start_runs", lambda p, *, model, user: calls.append(f"start {model} {user}"))
    monkeypatch.setattr(
        amt,
        "evaluate_runs",
        lambda p, **kwargs: calls.append(f"evaluate {kwargs['model']} {kwargs['user']}") or "",
    )
    paths.status_file.write_text("next:start\n")
    with caplog.at_level(logging.INFO):
        modeltests(paths.model_dir, model="REMIND", report_repo=paths.report_repo)
        assert calls == ["start REMIND alice"]
        assert paths.status_file.read_text() == "next:evaluate\n"
        modeltests(paths.model_dir, model="REMIND", user="bob")
        assert calls[-1] == "evaluate REMIND bob"
        assert paths.status_file.read_text() == "next:start\n"
        paths.status_file.write_text("evaluateRuns() is running or stopped due to an error\n")
        modeltests(paths.model_dir, model="REMIND")
        paths.status_file.write_text("next:start \nnext:evaluate\n")  # the first line counts (BUG-091)
        modeltests(paths.model_dir, model="REMIND")
        paths.status_file.write_bytes(b"next:evaluate\xff\n")  # not UTF-8: R's readLines() did not mind
        modeltests(paths.model_dir, model="REMIND")
        paths.status_file.write_text("")  # empty: nothing to do
        modeltests(paths.model_dir, model="REMIND")
    assert len(calls) == 3 and calls[-1] == "start REMIND alice"
    assert "Found 'next:evaluate\ufffd' in" in caplog.text and "Found '' in" in caplog.text
    assert "Doing nothing" in caplog.text
    assert "Begin of AMT procedure 2026-10-18 12:00:00" in caplog.text
    assert paths.lock_file.exists() and not paths.state_file.exists()


@pytest.mark.usefixtures("clock")
def test_missing_status_file(paths: AmtPaths, shell: FakeShell) -> None:
    with pytest.raises(ModelstatsError, match=r"\.testsstatus: No such file"):
        modeltests(paths.model_dir, model="REMIND")


@pytest.mark.usefixtures("clock")
def test_lock_refuses_a_second_run(
    paths: AmtPaths, shell: FakeShell, monkeypatch: pytest.MonkeyPatch, caplog: pytest.LogCaptureFixture
) -> None:
    with pytest.raises(ModelstatsError, match=r"\.modeltests\.lock: No such file"):
        modeltests(paths.model_dir / "missing", model="REMIND")  # no model folder: no lock file
    paths.status_file.write_text("next:start\n")
    monkeypatch.setattr(amt, "start_runs", lambda *a, **k: pytest.fail("must not start"))
    with paths.lock_file.open("a") as holder:
        fcntl.flock(holder, fcntl.LOCK_EX)
        with caplog.at_level(logging.WARNING):
            modeltests(paths.model_dir, model="REMIND")
    assert "Another modeltests is running" in caplog.text
    assert paths.status_file.read_text() == "next:start\n"


@pytest.mark.usefixtures("clock", "quiet_tables")
def test_first_evaluation_migrates_the_r_files(
    paths: AmtPaths, shell: FakeShell, statuses: dict[str, RunStatus], data_dir: Path
) -> None:
    shutil.copy(data_dir / "lastcommit.rds", paths.model_dir / "lastcommit.rds")
    shutil.copy(data_dir / "runcode.rds", paths.runcode_rds)
    shell.outputs[("git", "log", "-1")] = ["commit abcdef1234567"]
    paths.status_file.write_text("next:evaluate\n")
    modeltests(paths.model_dir, model="REMIND", publish=False)
    assert paths.status_file.read_text() == "next:start\n"
    state = AmtState.load(paths)
    assert state.last_commit == "abcdef1234567" and state.runcode == ".*-AMT_2026-10-18|.*-AMT_2026-10-19"
    assert ("git", "log", "--merges", "--pretty=oneline", "abc1234..abcdef1234567", "--abbrev-commit") in {
        c.argv for c in shell.calls
    }


@pytest.mark.usefixtures("clock")
def test_fresh_checkout_start_then_evaluate_needs_a_last_commit(
    paths: AmtPaths, shell: FakeShell, statuses: dict[str, RunStatus]
) -> None:
    shell.outputs[SELECT_SCENARIOS] = ["AMT-SCENARIO SSP2-NPi"]
    paths.status_file.write_text("next:start\n")
    modeltests(paths.model_dir, model="REMIND", publish=False)
    assert AmtState.load(paths).runs_to_start == ["SSP2-NPi-AMT"]
    assert paths.status_file.read_text() == "next:evaluate\n"
    with pytest.raises(ModelstatsError, match="no last tested commit"):  # BUG-083, as R's readRDS
        modeltests(paths.model_dir, model="REMIND", publish=False)
    assert paths.status_file.read_text().startswith("evaluateRuns() is running")


# --- startRuns ----------------------------------------------------------------------------------


@pytest.mark.usefixtures("clock")
def test_start_runs_remind(paths: AmtPaths, shell: FakeShell, monkeypatch: pytest.MonkeyPatch) -> None:
    shell.outputs[SELECT_SCENARIOS] = [
        "Loading settings",
        "AMT-SCENARIO SSP2-NPi",
        "AMT-SCENARIO SSP2-PkBudg1000",
    ]
    default_cfg = paths.model_dir / "config" / "default.cfg"
    seen_cfg: list[str] = []
    original = shell.run

    def run(argv: Sequence[str], **kwargs: Any) -> subprocess.CompletedProcess[str]:
        if "--testOneRegi" in argv or "startgroup=AMT" in argv:
            seen_cfg.append(default_cfg.read_text())
        return original(argv, **kwargs)

    monkeypatch.setattr(_shell, "run", run)
    start_runs(paths, model="REMIND", user="alice")
    argvs = shell.argvs
    assert argvs[:2] == [["git", "reset", "--hard", "origin/develop"], ["git", "pull"]]
    assert argvs[2] == [
        "Rscript",
        "start.R",
        "--testOneRegi",
        "titletag=AMT",
        "slurmConfig=--qos=priority --nodes=1 --tasks-per-node=1 --wait --time=2:00:00",
    ]
    assert argvs[3] == [
        "Rscript",
        "start.R",
        "startgroup=AMT",
        "titletag=AMT",
        "slurmConfig=--qos=standby --nodes=1 --tasks-per-node=12 --time=36:00:00",
        "config/scenario_config.csv",
    ]
    assert seen_cfg == ["cfg$force_download <- TRUE\n", "cfg$force_download <- FALSE\n"]
    assert argvs[4] == list(SELECT_SCENARIOS)
    assert argvs[5:] == [
        ["git", "reset", "--hard", "origin/develop"],
        ["git", "pull"],
        ["make", "test-full-slurm"],
    ]
    assert [c.cwd for c in shell.calls][5:7] == [paths.magpie_dir, paths.magpie_dir]
    assert all(c.cwd == paths.model_dir for c in shell.calls[:5]) and shell.calls[7].cwd == paths.model_dir
    assert all(c.env is not None and c.env.get("autoRenvFixDeps") == "TRUE" for c in shell.calls[2:])
    assert all(c.check for c in shell.calls)
    assert default_cfg.read_text() == "cfg$force_download <- FALSE\n"
    state = AmtState.load(paths)
    assert state.runcode == ".*-AMT_2026-10-18|.*-AMT_2026-10-19"
    assert state.runs_to_start == ["SSP2-NPi-AMT", "SSP2-PkBudg1000-AMT"]
    assert amt.read_rds_string(paths.runcode_rds) == state.runcode


@pytest.mark.usefixtures("clock")
def test_start_runs_magpie_waits_for_the_default_run(paths: AmtPaths, shell: FakeShell) -> None:
    shell.run_answers[SQUEUE] = deque(
        [
            [f"1 x RUNNING 1 0:10 default 0 0 0 {paths.model_dir}"],
            (1, []),  # a failing squeue: keep waiting
            ["2 y RUNNING 1 0:10 other 0 0 0 /elsewhere"],
        ]
    )
    start_runs(paths, model="MAgPIE", user="alice")
    argvs = shell.argvs
    assert argvs[2] == ["Rscript", "start.R", "runscripts=default", "submit=SLURM priority"]
    assert argvs[3:6] == [list(SQUEUE)] * 3
    assert argvs[6] == ["Rscript", "start.R", "runscripts=test_runs", "submit=SLURM standby"]
    assert shell.sleeps == [300, 300, 300]  # BUG-084: R's pattern never matched, it slept once
    assert (AmtState.load(paths).runcode or "").startswith(".*-AMT_2026-10-18")
    assert (paths.model_dir / "config" / "default.cfg").read_text() == "cfg$force_download <- TRUE\n"


@pytest.mark.usefixtures("clock")
def test_start_runs_aborts_when_the_model_update_fails(paths: AmtPaths, shell: FakeShell) -> None:
    shell.returncodes[("git", "pull")] = 1
    shell.errors[("git", "pull")] = "fatal: no network\n"
    with pytest.raises(subprocess.CalledProcessError):
        start_runs(paths, model="REMIND", user="alice")
    assert not paths.state_file.exists()
    shutil.rmtree(paths.model_dir / "config")
    shell.returncodes.clear()
    with pytest.raises(ModelstatsError, match="default.cfg"):
        start_runs(paths, model="REMIND", user="alice")


@pytest.mark.usefixtures("clock")
def test_start_runs_goes_on_when_make_fails(
    paths: AmtPaths, shell: FakeShell, caplog: pytest.LogCaptureFixture
) -> None:
    shell.outputs[SELECT_SCENARIOS] = ["AMT-SCENARIO SSP2-NPi"]
    shell.returncodes[("make", "test-full-slurm")] = 2
    shell.errors[("make", "test-full-slurm")] = "make: *** No rule to make target\n"
    shell.outputs[("git", "pull")] = ["Already up to date."]
    with caplog.at_level(logging.INFO):
        start_runs(paths, model="REMIND", user="alice")  # BUG-067: the runs are started, make is not required
    assert AmtState.load(paths).runs_to_start == ["SSP2-NPi-AMT"]
    assert "make: *** No rule to make target" in caplog.text
    assert "make test-full-slurm failed in" in caplog.text and "Already up to date." in caplog.text


@pytest.mark.usefixtures("clock")
def test_start_runs_goes_on_without_the_magpie_folder(
    paths: AmtPaths, shell: FakeShell, caplog: pytest.LogCaptureFixture
) -> None:
    shell.outputs[SELECT_SCENARIOS] = ["Loading settings"]  # an empty start group prints no scenario line
    shutil.rmtree(paths.magpie_dir)
    with caplog.at_level(logging.WARNING):
        start_runs(paths, model="REMIND", user="alice")
    assert "git reset --hard origin/develop could not run in" in caplog.text
    assert ["git", "pull"] not in shell.argvs[5:]  # reset && pull
    assert ["make", "test-full-slurm"] in shell.argvs
    assert AmtState.load(paths).runs_to_start == []


@pytest.mark.usefixtures("clock")
def test_start_runs_aborts_when_a_start_script_fails(paths: AmtPaths, shell: FakeShell) -> None:
    key = (
        "Rscript",
        "start.R",
        "--testOneRegi",
        "titletag=AMT",
        "slurmConfig=--qos=priority --nodes=1 --tasks-per-node=1 --wait --time=2:00:00",
    )
    shell.returncodes[key] = 1
    with pytest.raises(subprocess.CalledProcessError):
        start_runs(paths, model="REMIND", user="alice")
    shell.returncodes.clear()
    shell.returncodes[SELECT_SCENARIOS] = 1  # selectScenarios() failing
    with pytest.raises(subprocess.CalledProcessError):
        start_runs(paths, model="REMIND", user="alice")
    assert not paths.state_file.exists()


@pytest.mark.usefixtures("clock")
def test_select_scenarios_survives_unterminated_output(paths: AmtPaths, shell: FakeShell) -> None:
    # .Rprofile/renv may print without a final newline: the bridge starts on a fresh line
    shell.run_answers[SELECT_SCENARIOS] = deque([(0, ["- Project loaded.AMT-SCENARIO SSP2-NPi"])])
    shell.outputs[SELECT_SCENARIOS] = []
    start_runs(paths, model="REMIND", user="alice")
    assert AmtState.load(paths).runs_to_start == []  # glued text is no scenario line
    assert amt._SELECT_SCENARIOS_R.endswith(
        'writeLines(c("", paste0("AMT-SCENARIO ", row.names(runsToStart), recycle0 = TRUE)))'
    )


def test_replace_in_file_is_sed_like(tmp_path: Path) -> None:
    cfg = tmp_path / "default.cfg"
    cfg.write_bytes(b"a <- FALSE FALSE\r\nb <- FALSE\n")
    amt._replace_in_file(cfg, "FALSE", "TRUE")
    assert cfg.read_bytes() == b"a <- TRUE FALSE\r\nb <- TRUE\n"  # first per line, line endings kept
    cfg.write_bytes(b"\xffcfg$force_download <- FALSE\n")  # bytes that are not UTF-8 survive, as with sed
    cfg.chmod(0o754)  # a mode open() never creates: the copy must happen
    amt._replace_in_file(cfg, "FALSE", "TRUE")
    assert cfg.read_bytes() == b"\xffcfg$force_download <- TRUE\n"
    assert cfg.stat().st_mode & 0o777 == 0o754 and not list(
        tmp_path.glob("*.tmp")
    )  # the mode stays, as with sed -i
    with pytest.raises(ModelstatsError, match="default.cfg"):
        amt._replace_in_file(tmp_path / "missing" / "default.cfg", "a", "b")


def test_replace_in_file_cleans_up_after_a_failed_write(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    cfg = tmp_path / "default.cfg"
    cfg.write_text("cfg$force_download <- FALSE\n")
    monkeypatch.setattr(amt.shutil, "copymode", lambda src, dst: (_ for _ in ()).throw(OSError("denied")))
    with pytest.raises(ModelstatsError, match="denied"):
        amt._replace_in_file(cfg, "FALSE", "TRUE")
    assert cfg.read_text() == "cfg$force_download <- FALSE\n" and not list(tmp_path.glob("*.tmp"))


def test_creation_date_is_the_ctime(tmp_path: Path) -> None:
    assert amt._creation_date(tmp_path) == datetime.fromtimestamp(tmp_path.stat().st_ctime).date()


@pytest.mark.usefixtures("clock")
def test_start_runs_magpie_aborts_when_the_default_run_fails(paths: AmtPaths, shell: FakeShell) -> None:
    shell.returncodes[("Rscript", "start.R", "runscripts=default", "submit=SLURM priority")] = 1
    with pytest.raises(subprocess.CalledProcessError):
        start_runs(paths, model="MAgPIE", user="alice")
    shell.returncodes.clear()
    shell.run_answers[SQUEUE] = deque([(1, [])] * 4)
    with pytest.raises(ModelstatsError, match="squeue had exit status"):  # BUG-084: not forever
        start_runs(paths, model="MAgPIE", user="alice")
    assert shell.sleeps == [300] * 4


@pytest.mark.usefixtures("clock")
def test_a_required_program_that_cannot_start(paths: AmtPaths, shell: FakeShell) -> None:
    shutil.rmtree(paths.model_dir)
    with pytest.raises(ModelstatsError, match="git reset --hard origin/develop could not run in"):
        start_runs(paths, model="REMIND", user="alice")


def test_delete_empty_realization_folders(tmp_path: Path) -> None:
    module = tmp_path / "modules" / "29_CES_parameters"
    (module / "load").mkdir(parents=True)
    (module / "calibrate").mkdir()
    (module / "input").mkdir()
    (module / "stale").mkdir()
    (module / "stale.txt").write_text("")
    (module / "link").symlink_to(module / "load", target_is_directory=True)
    (module / ".hidden").mkdir()
    (module / "module.gms").write_text(
        '$Ifi "%CES_parameters%" == "load" $include "./modules/29_CES_parameters/load/realization.gms"\n'
        '$Ifi "%CES_parameters%" == "calibrate" $include "./modules/29_CES_parameters/calibrate/realization.gms"\n'
    )
    delete_empty_realization_folders(tmp_path)
    assert sorted(entry.name for entry in module.iterdir()) == [
        ".hidden",
        "calibrate",
        "input",
        "load",
        "module.gms",
    ]


def test_delete_empty_realization_folders_warns_instead_of_stopping(
    tmp_path: Path, caplog: pytest.LogCaptureFixture, monkeypatch: pytest.MonkeyPatch
) -> None:
    module = tmp_path / "modules" / "29_CES_parameters"
    (module / "stale").mkdir(parents=True)
    (module / "stale.txt").write_text("")
    (module / "module.gms").write_text("")
    monkeypatch.setattr(amt.shutil, "rmtree", lambda path: (_ for _ in ()).throw(OSError("busy")))
    with caplog.at_level(logging.WARNING):
        delete_empty_realization_folders(tmp_path)
    assert "Could not remove" in caplog.text and "busy" in caplog.text  # R's unlink() never stopped
    assert (module / "stale").exists() and not (module / "stale.txt").exists()


# --- evaluateRuns -------------------------------------------------------------------------------


@pytest.mark.usefixtures("clock", "statuses")
def test_evaluate_waits_for_the_jobs_inside_the_model_folder(paths: AmtPaths, shell: FakeShell) -> None:
    shell.run_answers[SQUEUE] = deque(
        [
            [
                f"1 q R 1 0:10 j 0 0 0 {paths.model_dir}/output/x",
                f"2 q R 1 0:10 make 0 0 0 {paths.model_dir}",
            ],
            [
                f"2 q R 1 0:10 make 0 0 0 {paths.model_dir}",
                f"3 q R 1 0:10 j 0 0 0 {paths.model_dir}2/output/y",
            ],
        ]
    )
    text = evaluated(paths, shell)
    assert shell.sleeps == [600]  # BUG-085: test-full in the model folder and a sibling folder do not count
    assert "Summary: No runs started" in text


@pytest.mark.usefixtures("clock")
def test_evaluate_gives_up_after_four_squeue_failures(paths: AmtPaths, shell: FakeShell) -> None:
    shell.returncodes[SQUEUE] = 1
    with pytest.raises(ModelstatsError, match="squeue had exit status"):
        evaluated(paths, shell)
    assert shell.sleeps == [600, 600, 600]
    assert paths.status_file.read_text().startswith("evaluateRuns() is running")


@pytest.mark.usefixtures("clock", "statuses")
def test_squeue_failure_counter_resets(
    paths: AmtPaths, shell: FakeShell, caplog: pytest.LogCaptureFixture
) -> None:
    failing: tuple[int, list[str]] = (1, [])
    working = (0, [f"1 q R 1 0:10 j 0 0 0 {paths.model_dir}/output/x"])
    shell.errors[SQUEUE] = "squeue: error: slurm_load_jobs error\n"
    shell.run_answers[SQUEUE] = deque(
        [failing, failing, failing, working, failing, failing, failing, (0, [])]
    )
    with caplog.at_level(logging.INFO):
        evaluated(paths, shell)
    assert shell.sleeps == [600] * 7
    assert caplog.text.count("squeue: error: slurm_load_jobs error") == 6  # BUG-081: the reason is logged
    assert "squeue failed with exit status 1 (3 in a row)" in caplog.text


@pytest.mark.usefixtures("clock", "quiet_tables")
def test_evaluate_remind_readme(
    paths: AmtPaths, shell: FakeShell, statuses: dict[str, RunStatus], caplog: pytest.LogCaptureFixture
) -> None:
    runs = [
        "SSP2-NPi-AMT_2026-10-18_10.00.00",
        "SSP2-PkBudg1000-AMT_2026-10-18_10.05.00",
        "testOneRegi-AMT_2026-10-18_09.00.00",
        "Calib-AMT_2026-10-18_08.00.00",
    ]
    for name in runs:
        make_run(paths.output_dir, name, config="remind")
    make_run(paths.output_dir, "SSP2-NPi-AMT_2026-10-11_10.00.00", {"runstatistics.rda": b""})
    statuses[runs[0]] = good_run()
    statuses[runs[1]] = good_run(conv="not_converged", mif="sumErr", run_in_app_results="no")
    statuses[runs[2]] = good_run(run_type="testOneRegi EUR", modelstat="7: Intermed Nonoptimal", conv="NA")
    statuses[runs[3]] = good_run(run_type="Calib_nash", conv="Clb_converged")
    paths.test_full_log.write_text("x\n[ FAIL 0 | WARN 0 | SKIP 1 | PASS 12 ]\n")
    os.utime(paths.test_full_log, (FIXED_NOW.timestamp(), FIXED_NOW.timestamp()))  # the log's date tag
    with caplog.at_level(logging.INFO):
        text = evaluated(paths, shell, mattermost_url=HOOK)
    lines = text.splitlines()
    assert lines[:9] == [
        "```",
        "This is the result of the automated model tests for REMIND on 2026-10-18.",
        f"Path to runs: {paths.model_dir}/output/",
        "Direct and interactive access to plots: open shinyResults::appResults, then use 'AMT' as keyword "
        "in the title search",
        "Each run folder below should contain a compareScenarios PDF comparing the output of the current "
        "and the last successful tests (comp_with_RUN-DATE.pdf)",
        "Note: 'Mif' = 'no' indicates a possible error in output generation, please check!",
        "If you are currently viewing the email: Overview of the last test is in red, and of the current "
        "test in green",
        "Tested commit: abcdef1234567",
        "The test of 2026-10-18 contains these merges:",
    ]
    assert lines[9:11] == ["1111111 Merge pull request #1 from x/y", "  ".join(amt._README_TITLES)]
    assert [line[:46].strip() for line in lines[11:15]] == [  # run order (BUG-054)
        "Calib-AMT_2026-10-18_08.00.00",
        "SSP2-NPi-AMT_2026-10-18_10.00.00",
        "SSP2-PkBudg1000-AMT_2026-10-18_10.05.00",
        "testOneRegi-AMT_2026-10-18_09.00.00",
    ]
    cells = [
        "3.4 hours",
        "",
        "nash",
        "Normal completion",
        "0",
        "12/50",
        "converged",
        "2: Locally Optimal",
        "yes",
        "yes",
    ]
    widths = [11, 0, 11, 18, 9, 16, 21, 19, 6, 3]
    assert lines[12] == "SSP2-NPi-AMT_2026-10-18_10.00.00".ljust(46) + "  " + "  ".join(
        c.ljust(w) for c, w in zip(cells, widths, strict=True)
    )
    assert lines[-2:] == [
        "Summary: Some run(s) did not converge. Summation checks for some run(s) revealed some gaps. "
        "Some run(s) did not report correctly. testOneRegi does not return an optimal solution",
        "```",
    ]
    assert "These scenarios did not start at all:" not in text
    assert "testOneRegi-AMT_2026-10-18_09.00.00 does not seem to have converged. Skipping!" in caplog.text
    assert "Calib-AMT_2026-10-18_08.00.00 does not seem to have converged. Skipping!" in caplog.text
    assert AmtState.load(paths).last_commit == "abcdef1234567"
    assert (paths.model_dir / "tests" / "test-full-2026-10-18.log").exists()
    # the Mattermost message
    url, payload = shell.posts[-1]
    assert url == HOOK and HOOK not in caplog.text
    message = payload["text"].splitlines()
    assert message[:7] == [
        "Please find below the status of the REMIND automated model tests (AMT) of 2026-10-18. "
        "Runs are here: `/p/projects/remind/modeltests/remind`.",
        lines[-2],
        "All tests pass in `make test-full`: [ FAIL 0 | WARN 0 | SKIP 1 | PASS 12 ]",
        "```",
        "Tested commit: abcdef1234567",
        "The test of 2026-10-18 contains these merges:",
        "1111111 Merge pull request #1 from x/y",
    ]
    assert message[7:] == [
        "```",
        "`rs -A` returns:",
        "```",
        "TABLE " + " ".join(["Calib-AMT_2026-10-18_08.00.00", *runs[:3]]),
        "```",
        "Sanity checks (`rs -s`) return:",
        "```",
        "SANITY",
        "```",
    ]


@pytest.mark.usefixtures("clock", "quiet_tables", "statuses")
def test_readme_without_compare_scenarios_and_without_notification(paths: AmtPaths, shell: FakeShell) -> None:
    text = evaluated(paths, shell, compare_scenarios=False)
    assert "compareScenarios PDF" not in text and shell.posts == []


@pytest.mark.usefixtures("clock", "statuses")
def test_message_block_for_no_runs_is_empty(
    paths: AmtPaths, shell: FakeShell, monkeypatch: pytest.MonkeyPatch, caplog: pytest.LogCaptureFixture
) -> None:
    def sanity(dirs: object = None, *, out: Any = None) -> None:
        out.print("SANITY")

    monkeypatch.setattr(amt, "print_sanity_checks", sanity)
    evaluated(paths, shell, mattermost_url=HOOK)  # the real print_run_table
    message = shell.posts[-1][1]["text"].splitlines()
    block = message[message.index("`rs -A` returns:") + 1 : message.index("Sanity checks (`rs -s`) return:")]
    assert block == ["```", "```"]  # BUG-041: R's block held '[1] "No runs found"'

    def failing_table(dirs: list[Path], **kwargs: Any) -> None:
        raise ModelstatsError("x")

    monkeypatch.setattr(amt, "print_run_table", failing_table)
    with caplog.at_level(logging.WARNING):
        evaluated(paths, shell, mattermost_url=HOOK)
    message = shell.posts[-1][1]["text"].splitlines()
    block = message[message.index("`rs -A` returns:") + 1 : message.index("Sanity checks (`rs -s`) return:")]
    assert block == ["```", "Error: x", "```"] and "The run table of the message failed: x" in caplog.text
    assert AmtState.load(paths).last_commit == "abcdef1234567"


@pytest.mark.usefixtures("clock", "quiet_tables")
def test_calib_nash_rule_and_dot_folders(
    paths: AmtPaths, shell: FakeShell, statuses: dict[str, RunStatus]
) -> None:
    make_run(paths.output_dir, "Calib-AMT_2026-10-18_08.00.00", config="remind")
    make_run(paths.output_dir, ".hidden-AMT_2026-10-18_08.00.00", config="remind")
    statuses["Calib-AMT_2026-10-18_08.00.00"] = good_run(run_type="Calib_nash", conv="converged")
    statuses[".hidden-AMT_2026-10-18_08.00.00"] = good_run()
    text = evaluated(paths, shell)
    assert "Summary: Some run(s) did not converge" in text  # Clb_converged expected
    assert ".hidden-AMT_2026-10-18_08.00.00" in text  # R's list.dirs() counted dot-folders as started


@pytest.mark.usefixtures("clock", "quiet_tables")
def test_unreadable_started_run(
    paths: AmtPaths, shell: FakeShell, statuses: dict[str, RunStatus], monkeypatch: pytest.MonkeyPatch
) -> None:
    make_run(paths.output_dir, "SSP2-NPi-AMT_2026-10-18_10.00.00", config="remind")
    original = amt.get_run_status
    asked: list[str] = []

    def failing(dirs: Path, **kwargs: object) -> list[RunStatus]:
        asked.append(dirs.name)
        if dirs.name.startswith("SSP2-NPi"):
            raise ModelstatsError("runstatistics.rda is damaged")
        return original(dirs)

    monkeypatch.setattr(amt, "get_run_status", failing)
    text = evaluated(paths, shell)
    assert "SSP2-NPi-AMT_2026-10-18_10.00.00  could not be read: runstatistics.rda is damaged" in text
    assert "Summary: Some run(s) could not be read" in text  # BUG-070
    assert asked.count("SSP2-NPi-AMT_2026-10-18_10.00.00") == 1  # read once, the error is reused


@pytest.mark.usefixtures("clock")
def test_unreadable_started_run_still_notifies(
    paths: AmtPaths,
    shell: FakeShell,
    statuses: dict[str, RunStatus],
    monkeypatch: pytest.MonkeyPatch,
    data_dir: Path,
) -> None:
    make_run(paths.output_dir, "SSP2-NPi-AMT_2026-10-18_10.00.00", config="remind")
    monkeypatch.setattr(_shell, "AMT_OUTPUT", paths.output_dir)  # the real tables of the message
    monkeypatch.setattr(_shell, "AMT_RUNCODE", data_dir / "runcode.rds")
    original = amt.get_run_status

    def failing(dirs: Path, **kwargs: Any) -> list[RunStatus]:
        if dirs.name.startswith("SSP2-NPi"):
            raise ModelstatsError("runstatistics.rda is damaged")
        return original(dirs, **kwargs)

    monkeypatch.setattr(amt, "get_run_status", failing)
    monkeypatch.setattr(sanity_checks, "get_run_status", failing)
    monkeypatch.setattr(loop_runs, "get_run_status", failing)
    evaluated(paths, shell, mattermost_url=HOOK)
    message = shell.posts[-1][1]["text"]
    assert "Error: runstatistics.rda is damaged" in message  # BUG-055: the sanity block, not the evaluation
    assert "skipped because of error" in message  # the run table goes on (BUG-058)
    assert AmtState.load(paths).last_commit == "abcdef1234567"


@pytest.mark.usefixtures("clock", "quiet_tables", "statuses")
def test_empty_user_watches_the_current_users_jobs(paths: AmtPaths, shell: FakeShell) -> None:
    evaluated(paths, shell, user="")  # BUG-090
    assert shell.argvs[0] == ["squeue", "-u", "alice", "-h", "-o", amt.SQUEUE_FORMAT]


@pytest.mark.usefixtures("clock", "quiet_tables")
def test_good_run_without_runtime(paths: AmtPaths, shell: FakeShell, statuses: dict[str, RunStatus]) -> None:
    make_run(paths.output_dir, "SSP2-NPi-AMT_2026-10-18_10.00.00", config="remind")
    statuses["SSP2-NPi-AMT_2026-10-18_10.00.00"] = good_run(runtime=None)
    lines = evaluated(paths, shell).splitlines()
    row = next(line for line in lines if line.startswith("SSP2-NPi-AMT"))
    assert row[46:59] == "  " + " " * 11  # a blank Runtime cell
    assert lines[-2] == "Summary: AMT runs look good."


@pytest.mark.usefixtures("clock", "quiet_tables")
def test_a_run_that_vanished_is_left_out(
    paths: AmtPaths, shell: FakeShell, statuses: dict[str, RunStatus], monkeypatch: pytest.MonkeyPatch
) -> None:
    make_run(paths.output_dir, "SSP2-NPi-AMT_2026-10-18_10.00.00", config="remind")
    original = amt.get_run_status
    monkeypatch.setattr(
        amt, "get_run_status", lambda dirs, **kw: [] if dirs.name.startswith("SSP2") else original(dirs, **kw)
    )
    text = evaluated(paths, shell)  # a folder gone between listing and reading: no row, no error
    assert "SSP2-NPi-AMT_2026-10-18_10.00.00" not in AmtState.load(paths).runs
    assert "SSP2-NPi-AMT_2026-10-18_10.00.00  " not in text and "could not be read" not in text


@pytest.mark.usefixtures("clock")
def test_unreadable_started_magpie_run(
    paths: AmtPaths, shell: FakeShell, statuses: dict[str, RunStatus], monkeypatch: pytest.MonkeyPatch
) -> None:
    run = make_run(paths.output_dir, "weeklyTests_SSP2_2026-10-18_11.00.00")
    monkeypatch.setattr(amt, "_creation_date", lambda path: date(2026, 10, 18))
    original = amt.get_run_status

    def failing(dirs: Path, **kwargs: Any) -> list[RunStatus]:
        if dirs.name == run.name:
            raise ModelstatsError("runstatistics.rda is damaged")
        return original(dirs, **kwargs)

    monkeypatch.setattr(amt, "get_run_status", failing)
    text = evaluated(paths, shell, model="MAgPIE", mattermost_url=HOOK)
    assert f"{run.name}  could not be read: runstatistics.rda is damaged" in text  # BUG-070, read in the loop
    assert "Summary: Some run(s) could not be read" in text
    assert shell.posts[-1][1]["text"].startswith("Some MAgPIE tests produce warnings.")


@pytest.mark.usefixtures("clock", "quiet_tables", "statuses")
def test_evaluate_needs_the_output_folder(paths: AmtPaths, shell: FakeShell) -> None:
    shutil.rmtree(paths.output_dir)
    paths.status_file.write_text("next:evaluate\n")
    AmtState(runcode="x", last_commit="0").save(paths)
    with pytest.raises(ModelstatsError, match="output: no such folder"):
        modeltests(paths.model_dir, model="REMIND", publish=False)
    assert paths.status_file.read_text() == "next:evaluate\n"  # R failed before writing the marker


@pytest.mark.usefixtures("clock", "quiet_tables")
def test_readme_off_the_cluster(paths: AmtPaths, shell: FakeShell, statuses: dict[str, RunStatus]) -> None:
    shell.cluster = False
    make_run(paths.output_dir, "SSP2-NPi-AMT_2026-10-18_10.00.00", config="remind")
    statuses["SSP2-NPi-AMT_2026-10-18_10.00.00"] = good_run(run_in_app_results=None)
    text = evaluated(paths, shell)
    assert "  ".join(amt._README_TITLES) in text  # BUG-096: the cluster's 11 columns
    assert "Summary: Some run(s) did not report correctly" in text  # R stopped on the missing column


@pytest.mark.usefixtures("clock", "quiet_tables")
def test_unanchored_title_match(paths: AmtPaths, shell: FakeShell, statuses: dict[str, RunStatus]) -> None:
    mif = {"REMIND_generic_SSP2-NPi-AMT.mif": "x"}
    current = make_run(paths.output_dir, "SSP2-NPi-AMT_2026-10-18_10.00.00", mif, config="remind")
    other = make_run(paths.output_dir, "SSP2-NPi-AMT-x_2026-10-17_10.00.00", mif)
    statuses[current.name] = good_run(conv="not_converged")
    statuses[other.name] = good_run()  # still converged when the table is refreshed (BUG-102)
    AmtState(
        runcode="AMT_2026-10-18", last_commit="0", runs={other.name: {"Conv": "converged", "Mif": "yes"}}
    ).save(paths)
    evaluated(paths, shell)
    sbatch = next(call for call in shell.calls if call.argv[0] == "sbatch")
    assert sbatch.argv[2] == f"--job-name=comp_with_{other.name}"  # BUG-097: grepl(cfg$title) is unanchored


@pytest.mark.usefixtures("quiet_tables")
def test_magpie_window_is_taken_after_the_wait(
    paths: AmtPaths,
    shell: FakeShell,
    statuses: dict[str, RunStatus],
    clock: Callable[[datetime], None],
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    run = make_run(paths.output_dir, "weeklyTests_SSP2_2026-10-18_11.00.00")
    statuses[run.name] = good_run(run_type="nlp_apr17", modelstat="222", conv=None)
    monkeypatch.setattr(amt, "_creation_date", lambda path: date(2026, 10, 18))
    shell.run_answers[SQUEUE] = deque([[f"1 q R 1 0:10 j 0 0 0 {paths.model_dir}/output/x"]])
    monkeypatch.setattr(_shell, "sleep", lambda seconds: clock(datetime(2026, 10, 25, 12, 0, tzinfo=BERLIN)))
    text = evaluated(paths, shell, model="MAgPIE")
    assert (
        "on 2026-10-18." in text and run.name not in text
    )  # BUG-086: created more than 3 days before the end


@pytest.mark.usefixtures("clock", "quiet_tables")
def test_runs_not_started_and_testthat_variants(
    paths: AmtPaths, shell: FakeShell, statuses: dict[str, RunStatus], caplog: pytest.LogCaptureFixture
) -> None:
    make_run(paths.output_dir, "SSP2-NPi-AMT_2026-10-18_10.00.00", config="remind")
    statuses["SSP2-NPi-AMT_2026-10-18_10.00.00"] = good_run()
    AmtState(
        runcode=".*-AMT_2026-10-18|.*-AMT_2026-10-19",
        last_commit="0000000",
        runs_to_start=["SSP2-NPi-AMT", "SSP2-PkBudg1000-AMT", "default-AMT"],  # default twice: setdiff
    ).save(paths)
    text = evaluated(paths, shell, mattermost_url=HOOK)
    assert "These scenarios did not start at all:\ndefault-AMT\nSSP2-PkBudg1000-AMT\n \nSummary" in text
    message = shell.posts[-1][1]["text"]
    assert message.endswith("These scenarios did not start at all:\ndefault-AMT\nSSP2-PkBudg1000-AMT")
    assert "Could not check for the results of `make test-full`, test-full.log not found" in message
    paths.test_full_log.write_text("[ FAIL 2 | WARN 0 | SKIP 0 | PASS 1 ]\n")
    evaluated(paths, shell, mattermost_url=HOOK)
    message = shell.posts[-1][1]["text"]
    assert (
        f"Not all tests pass in `make test-full`: [ FAIL 2 | WARN 0 | SKIP 0 | PASS 1 ]. "
        f"Check `{paths.model_dir}/tests/test-full-" in message
    )
    paths.test_full_log.write_text("[ FAIL 0 | WARN 0 ]\n[ FAIL 0 | WARN 0 ]\n")  # two lines (BUG-071)
    evaluated(paths, shell, mattermost_url=HOOK)
    assert "`make test-full` did not run properly. Check" in shell.posts[-1][1]["text"]
    paths.test_full_log.write_text("Error: could not run the tests\n")  # no line at all
    shutil.rmtree(paths.model_dir / "tests")  # and the log cannot be filed
    with caplog.at_level(logging.WARNING):
        evaluated(paths, shell, mattermost_url=HOOK)
    assert "`make test-full` did not run properly. Check" in shell.posts[-1][1]["text"]
    assert "Could not move" in caplog.text and paths.test_full_log.exists()


@pytest.mark.usefixtures("clock", "quiet_tables")
def test_compare_with_the_last_run(
    paths: AmtPaths, shell: FakeShell, statuses: dict[str, RunStatus], data_dir: Path, stdout: CapturedConsole
) -> None:
    current = make_run(
        paths.output_dir,
        "SSP2-NPi-AMT_2026-10-18_10.00.00",
        {"REMIND_generic_SSP2-NPi-AMT.mif": "x"},
        config="remind",
        stats="remind",
    )
    earlier = make_run(
        paths.output_dir,
        "SSP2-NPi-AMT_2026-10-11_10.00.00",
        {"REMIND_generic_SSP2-NPi-AMT.mif": "x"},
        config="remind",
    )
    shutil.copy(
        data_dir / "runstatistics_xz.rda", earlier / "runstatistics.rda"
    )  # no times: no runtime check
    statuses[current.name] = good_run()
    statuses[earlier.name] = good_run()  # the table is refreshed from disk (BUG-102)
    # the config.Rdata fixture names the run output/SSP2-NPi-AMT_2026-10-18_10.00.00 and the remind folder elsewhere
    AmtState(
        runcode=".*-AMT_2026-10-18",
        last_commit="0000000",
        runs={
            earlier.name: {"Conv": "converged", "Mif": "yes"},
            current.name: {"Conv": "converged", "Mif": "yes"},
        },
    ).save(paths)
    shell.returncodes[("rsync", "-e", "ssh", "-av", "fulldata.gdx", amt.RSE_GDX)] = (
        1  # not required (BUG-067)
    )
    evaluated(paths, shell)
    sbatch = next(call for call in shell.calls if call.argv[0] == "sbatch")
    wrap = (
        f"Rscript scripts/cs2/run_compareScenarios2.R outputdirs={current.resolve()},{earlier.resolve()} "
        f"profileName=default outFileName=comp_with_{earlier.name}; mv comp_with_{earlier.name}.pdf {current.resolve()}"
    )
    assert sbatch.argv == (
        "sbatch",
        "--qos=standby",
        f"--job-name=comp_with_{earlier.name}",
        "--comment=compareScenarios2",
        f"--output={current.resolve()}/comp_with_{earlier.name}.out",
        f"--error={current.resolve()}/comp_with_{earlier.name}.out",
        "--mail-type=END",
        "--time=200",
        "--mem-per-cpu=8000",
        f"--wrap={wrap}",
    )
    assert sbatch.cwd == Path("/p/projects/remind/modeltests/remind") and sbatch.check
    assert stdout.text == " ".join(sbatch.argv[:-1]) + f' --wrap="{wrap}" \n'  # cat(cs2com, "\n") (BUG-089)
    rsync = next(call for call in shell.calls if call.argv[0] == "rsync")
    assert rsync.argv == ("rsync", "-e", "ssh", "-av", "fulldata.gdx", amt.RSE_GDX) and rsync.cwd == current
    (current / ".comp_with_x.pdf").write_text("")  # a dot-file: dir() did not see it
    shell.calls.clear()
    evaluated(paths, shell, compare_scenarios=False)
    assert not any(call.argv[0] == "sbatch" for call in shell.calls)  # compareScenarios off
    evaluated(paths, shell)
    assert any(call.argv[0] == "sbatch" for call in shell.calls)
    (current / "comp_with_x.pdf").write_text("")
    shell.calls.clear()
    evaluated(paths, shell)
    assert not any(call.argv[0] == "sbatch" for call in shell.calls)  # a pdf exists already
    (current / "comp_with_x.pdf").unlink()
    (earlier / "REMIND_generic_SSP2-NPi-AMT.mif").unlink()
    shell.calls.clear()
    evaluated(paths, shell)
    assert not any(call.argv[0] == "sbatch" for call in shell.calls)  # a mif is missing
    # BUG-069: the earlier runs of the scenario all sort after the current one
    shutil.rmtree(earlier)  # otherwise the refresh of the table (BUG-102) would bring it back
    later = make_run(
        paths.output_dir, "SSP2-NPi-AMT_2026-10-19_10.00.00", {"REMIND_generic_SSP2-NPi-AMT.mif": "x"}
    )
    statuses[later.name] = good_run()
    AmtState(
        runcode=".*-AMT_2026-10-18",
        last_commit="0000000",
        runs={
            later.name: {"Conv": "converged", "Mif": "yes"},
            current.name: {"Conv": "converged", "Mif": "yes"},
        },
    ).save(paths)
    statuses[current.name] = good_run(conv="converged (had INFES)")
    shell.calls.clear()
    evaluated(paths, shell)
    assert not any(call.argv[0] == "sbatch" for call in shell.calls)
    assert any(call.argv[0] == "rsync" for call in shell.calls)  # had INFES counts as converged


@pytest.mark.usefixtures("clock", "quiet_tables")
def test_compare_with_an_archived_run_and_a_failed_run(
    paths: AmtPaths,
    shell: FakeShell,
    statuses: dict[str, RunStatus],
    stdout: CapturedConsole,
    monkeypatch: pytest.MonkeyPatch,
    caplog: pytest.LogCaptureFixture,
) -> None:
    current = make_run(
        paths.output_dir,
        "SSP2-NPi-AMT_2026-10-18_10.00.00",
        {"REMIND_generic_SSP2-NPi-AMT.mif": "x"},
        config="remind",
    )
    earlier = make_run(
        paths.archive_dir, "SSP2-NPi-AMT_2026-07-11_10.00.00", {"REMIND_generic_SSP2-NPi-AMT.mif": "x"}
    )
    statuses[current.name] = good_run(conv="not_converged")  # compared, but no runtime check
    AmtState(
        runcode=".*-AMT_2026-10-18",
        last_commit="0000000",
        runs={
            earlier.name: {"Conv": "converged", "Mif": "sumErr"},
            current.name: {"Conv": "not_converged", "Mif": "yes"},
        },
    ).save(paths)
    load_config = amt.load_config
    monkeypatch.setattr(amt, "load_config", lambda path: {**load_config(path), "remind_folder": "../.."})
    evaluated(paths, shell)
    sbatch = next(call for call in shell.calls if call.argv[0] == "sbatch")
    assert sbatch.cwd == current / "../.."  # a relative cfg$remind_folder is relative to the run folder
    monkeypatch.setattr(amt, "load_config", lambda path: {**load_config(path), "remind_folder": "/nowhere/x"})
    with caplog.at_level(logging.WARNING):
        evaluated(paths, shell)
    assert "sbatch --qos=standby" in caplog.text and "could not run in /nowhere/x" in caplog.text  # BUG-067
    assert sbatch.argv[2] == f"--job-name=comp_with_{earlier.name}"  # BUG-082: not archive/<run>
    assert f"outputdirs={current.resolve()},{earlier.resolve()}" in sbatch.argv[-1]
    assert not any(call.argv[0] == "rsync" for call in shell.calls)
    shutil.rmtree(earlier)  # gone from the archive too (BUG-095)
    shell.calls.clear()
    with caplog.at_level(logging.WARNING):
        evaluated(paths, shell)
    assert not any(call.argv[0] == "sbatch" for call in shell.calls)
    assert f"{earlier.name} is gone from output/ and archive/: not compared with" in caplog.text
    # the newest earlier run that still exists is compared with, the stale entry skipped
    older = make_run(
        paths.archive_dir, "SSP2-NPi-AMT_2026-06-01_10.00.00", {"REMIND_generic_SSP2-NPi-AMT.mif": "x"}
    )
    state = AmtState.load(paths)
    state.runs[older.name] = {"Conv": "converged", "Mif": "yes"}
    state.save(paths)
    shell.calls.clear()
    caplog.clear()
    with caplog.at_level(logging.WARNING):
        evaluated(paths, shell)
    sbatch = next(call for call in shell.calls if call.argv[0] == "sbatch")
    assert sbatch.argv[2] == f"--job-name=comp_with_{older.name}"
    assert f"{earlier.name} is gone from output/ and archive/: not compared with" in caplog.text


@pytest.mark.usefixtures("clock", "quiet_tables")
def test_runtime_regression_is_reported(
    paths: AmtPaths,
    shell: FakeShell,
    statuses: dict[str, RunStatus],
    data_dir: Path,
    caplog: pytest.LogCaptureFixture,
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    current = make_run(
        paths.output_dir, "SSP2-NPi-AMT_2026-10-18_10.00.00", config="remind", stats="remind"
    )  # 25 h
    earlier = make_run(
        paths.output_dir, "SSP2-NPi-AMT_2026-10-11_10.00.00", stats="remind"
    )  # the same 25 h: fine
    statuses[current.name] = good_run()
    statuses[earlier.name] = good_run()  # the table is refreshed from disk (BUG-102)
    AmtState(
        runcode="SSP2-NPi-AMT_2026-10-18",
        last_commit="0000000",
        runs={
            earlier.name: {"Conv": "converged", "Mif": "yes"},
            current.name: {"Conv": "converged", "Mif": "yes"},
        },
    ).save(paths)
    text = evaluated(paths, shell, compare_scenarios=False)
    assert "Check runtime" not in text
    shutil.copy(
        data_dir / "runstatistics_short.rda", earlier / "runstatistics.rda"
    )  # 3 h: the current run is slower
    text = evaluated(paths, shell, compare_scenarios=False)
    assert "Check runtime! Have some scenarios become slower?" in text
    assert not any(call.argv[0] == "sbatch" for call in shell.calls)
    shutil.copy(data_dir / "types.rda", earlier / "runstatistics.rda")  # no stats list: no runtime
    text = evaluated(paths, shell, compare_scenarios=False)
    assert "Check runtime" not in text
    (earlier / "runstatistics.rda").write_bytes(b"broken")  # BUG-070: R stopped; a warning, no runtime check
    with caplog.at_level(logging.WARNING):
        text = evaluated(paths, shell, compare_scenarios=False)
    assert "Check runtime" not in text and "No runtime check" in caplog.text and earlier.name in caplog.text
    for run in (current, earlier):  # compareScenarios still runs after the warning
        (run / "REMIND_generic_SSP2-NPi-AMT.mif").write_text("x")
    shell.calls.clear()
    evaluated(paths, shell)
    assert any(call.argv[0] == "sbatch" for call in shell.calls)
    shutil.copy(data_dir / "runstatistics_remind.rda", earlier / "runstatistics.rda")
    (current / "runstatistics.rda").unlink()  # the current run without stats: R stopped before the baseline
    caplog.clear()
    with caplog.at_level(logging.WARNING):
        text = evaluated(paths, shell, compare_scenarios=False)
    assert "Check runtime" not in text and "No runtime check" in caplog.text and current.name in caplog.text
    # an atomic stats, or times that are not both datetimes: silently no runtime check (R stopped)
    start = datetime(2026, 10, 18, 10, 0, tzinfo=BERLIN)
    for stats in ([1.0, 2.0], {"timeGAMSStart": start, "timeGAMSEnd": None}, {"timeGAMSStart": start}):
        monkeypatch.setattr(amt, "read_rda", lambda path, stats=stats: {"stats": stats})
        caplog.clear()
        with caplog.at_level(logging.WARNING):
            text = evaluated(paths, shell, compare_scenarios=False)
        assert "Check runtime" not in text and "No runtime check" not in caplog.text


@pytest.mark.usefixtures("clock", "quiet_tables")
def test_config_values_must_be_strings(
    paths: AmtPaths, shell: FakeShell, statuses: dict[str, RunStatus], monkeypatch: pytest.MonkeyPatch
) -> None:
    current = make_run(paths.output_dir, "SSP2-NPi-AMT_2026-10-18_10.00.00", config="remind")
    statuses[current.name] = good_run()
    AmtState(
        runcode="AMT_2026-10-18", last_commit="0", runs={current.name: {"Conv": "converged", "Mif": "yes"}}
    ).save(paths)
    monkeypatch.setattr(amt, "load_config", lambda path: {"title": None})
    with pytest.raises(ModelstatsError, match=r"cfg\$title is not a string"):
        evaluated(paths, shell)
    monkeypatch.setattr(amt, "load_config", lambda path: {"title": "SSP2-NPi-AMT[", "results_folder": "x"})
    with pytest.raises(ModelstatsError, match=r"cfg\$title: invalid pattern"):
        evaluated(paths, shell)


@pytest.mark.usefixtures("quiet_tables", "statuses")
def test_archive_old_runs(
    paths: AmtPaths,
    shell: FakeShell,
    clock: Callable[[datetime], None],
    caplog: pytest.LogCaptureFixture,
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    old = make_run(paths.output_dir, "SSP2-NPi-AMT_2026-07-01_10.00.00")
    older = make_run(paths.output_dir, "PkBudg-AMT_2026-06-01_10.00.00")
    recent = make_run(paths.output_dir, "SSP2-NPi-AMT_2026-09-01_10.00.00")
    make_run(paths.output_dir, "SSP2-NPi-AMT_nodate")  # BUG-087: R stopped here
    make_run(paths.output_dir, "SSP2-NPi-AMT_2026-13-01_10.00.00")
    make_run(paths.output_dir, ".snapshot-AMT_2026-01-01_10.00.00")
    (paths.output_dir / "old-AMT_2026-01-01_10.00.00.txt").write_text("")
    with caplog.at_level(logging.INFO):
        evaluated(paths, shell)
    assert not old.exists() and (paths.archive_dir / old.name).is_dir()
    assert not older.exists() and (paths.archive_dir / older.name).is_dir()
    assert recent.exists() and (paths.output_dir / "SSP2-NPi-AMT_nodate").exists()
    assert (paths.output_dir / "old-AMT_2026-01-01_10.00.00.txt").exists()
    assert (paths.output_dir / ".snapshot-AMT_2026-01-01_10.00.00").exists()
    assert "Moving 2 runs with timestamp older than 90 days (2026-07-20) to 'archive':" in caplog.text
    assert f"{older.name}\n{old.name}" in caplog.text
    blocked = make_run(paths.output_dir, "SSP2-NPi-AMT_2026-05-01_10.00.00")
    make_run(paths.archive_dir, blocked.name)  # mv refuses an existing target, R went on
    with caplog.at_level(logging.WARNING):
        evaluated(paths, shell)
    assert blocked.exists() and f"Could not move {blocked.name} to archive" in caplog.text
    (paths.archive_dir / blocked.name).rmdir()
    with monkeypatch.context() as patch, caplog.at_level(logging.WARNING):
        patch.setattr(amt.shutil, "move", lambda src, dst: (_ for _ in ()).throw(OSError("read-only")))
        evaluated(paths, shell)
    assert blocked.exists() and f"Could not move {blocked.name} to archive: read-only" in caplog.text
    shutil.rmtree(paths.archive_dir)
    paths.archive_dir.write_text("")  # a file in the way: mv failed in R, the evaluation went on
    with caplog.at_level(logging.WARNING):
        text = evaluated(paths, shell)
    assert (
        blocked.exists() and f"Could not create {paths.archive_dir}" in caplog.text and text.endswith("```\n")
    )
    paths.archive_dir.unlink()
    # BUG-086: the cut-off is taken after the wait for the runs, which may have lasted days
    shell.run_answers[SQUEUE] = deque([[f"1 q R 1 0:10 j 0 0 0 {paths.model_dir}/output/x"]])
    monkeypatch.setattr(_shell, "sleep", lambda seconds: clock(datetime(2026, 12, 1, 12, 0, tzinfo=BERLIN)))
    text = evaluated(paths, shell)
    assert "on 2026-10-18." in text and not recent.exists()


@pytest.mark.usefixtures("clock", "quiet_tables", "statuses")
def test_publish_commits_the_readme(
    paths: AmtPaths, shell: FakeShell, caplog: pytest.LogCaptureFixture
) -> None:
    assert paths.report_repo is not None
    shell.returncodes[("git", "commit", "-m", "Automated Test Results")] = (
        1  # nothing to commit: not required
    )
    with caplog.at_level(logging.WARNING):
        text = evaluated(paths, shell, publish=True)
    git = [list(call.argv) for call in shell.calls if call.argv[0] == "git" and call.cwd == paths.report_repo]
    assert git == [
        ["git", "reset", "--hard", "origin/master"],
        ["git", "pull"],
        ["git", "add", "README.md"],
        ["git", "commit", "-m", "Automated Test Results"],
        ["git", "push"],
    ]
    assert (paths.report_repo / "README.md").read_text() == text
    make_run(paths.output_dir, "SSP2-NPi-AMT_2026-10-18_10.00.00" + os.fsdecode(b"\xff"), config="remind")
    text = evaluated(paths, shell, publish=True)  # a run name that is not UTF-8 is written as its bytes
    assert b"SSP2-NPi-AMT_2026-10-18_10.00.00\xff" in (paths.report_repo / "README.md").read_bytes()
    assert "git commit -m 'Automated Test Results' failed in" in caplog.text
    assert AmtState.load(paths).last_commit == "abcdef1234567"
    paths.test_full_log.write_text("[ FAIL 0 | WARN 0 ]\n")
    paths.status_file.write_text("next:evaluate\n")
    shell.calls.clear()
    with pytest.raises(ModelstatsError, match="report-repo"):  # BUG-100: before the wait and the moves
        evaluated(AmtPaths(model_dir=paths.model_dir), shell, publish=True)
    assert shell.calls == [] and paths.test_full_log.exists()
    assert paths.status_file.read_text() == "next:evaluate\n"  # and before the marker


@pytest.mark.usefixtures("clock", "quiet_tables")
def test_git_log_must_name_a_commit(
    paths: AmtPaths, shell: FakeShell, statuses: dict[str, RunStatus], caplog: pytest.LogCaptureFixture
) -> None:
    shell.returncodes[("git", "log", "-1")] = 128
    AmtState(runcode="x", last_commit="0000000").save(paths)
    with pytest.raises(subprocess.CalledProcessError):
        evaluate_runs(paths, model="REMIND", user="alice", publish=False)
    shell.returncodes.clear()
    shell.outputs[("git", "log", "-1")] = ["commit abcdef1234567"]
    merges = ("git", "log", "--merges", "--pretty=oneline", "0000000..abcdef1234567", "--abbrev-commit")
    shell.returncodes[merges] = 128
    shell.errors[merges] = "fatal: bad revision '0000000..abcdef1234567'\n"
    with caplog.at_level(logging.INFO):
        text = evaluate_runs(paths, model="REMIND", user="alice", publish=False)
    assert "fatal: bad revision" in caplog.text and "Tested commit: abcdef1234567" in text  # BUG-081
    shell.returncodes.clear()
    shell.outputs[("git", "log", "-1")] = []
    with pytest.raises(ModelstatsError, match="did not name a commit"):  # BUG-083
        evaluate_runs(paths, model="REMIND", user="alice", publish=False)
    assert AmtState.load(paths).last_commit == "abcdef1234567"  # saved by the successful evaluation


@pytest.mark.usefixtures("clock", "quiet_tables", "statuses")
def test_runcode_is_required_and_validated(paths: AmtPaths, shell: FakeShell) -> None:
    make_run(paths.output_dir, "archive")
    AmtState(last_commit="0000000").save(paths)
    with pytest.raises(ModelstatsError, match="no runcode"):  # BUG-083: every folder would count
        evaluated(paths, shell)
    assert shell.calls == []  # checked before the wait
    AmtState(runcode="AMT[", last_commit="0000000").save(paths)
    with pytest.raises(ModelstatsError, match="invalid pattern"):
        evaluated(paths, shell)
    AmtState(runcode="AMT", last_commit="").save(paths)
    with pytest.raises(ModelstatsError, match="no last tested commit"):
        evaluated(paths, shell)


@pytest.mark.usefixtures("clock", "quiet_tables")
def test_known_runs_are_refreshed_and_never_forgotten(
    paths: AmtPaths, shell: FakeShell, statuses: dict[str, RunStatus], monkeypatch: pytest.MonkeyPatch
) -> None:
    make_run(paths.output_dir, "old-AMT_2026-09-01_10.00.00", config="remind")
    statuses["old-AMT_2026-09-01_10.00.00"] = good_run(conv="not_converged")  # changed since stored
    make_run(paths.output_dir, "new-AMT_2026-10-18_10.00.00", config="remind")
    statuses["new-AMT_2026-10-18_10.00.00"] = good_run()
    asked: list[str] = []
    original = amt.get_run_status

    def counting(dirs: Path, **kwargs: object) -> list[RunStatus]:
        asked.append(dirs.name)
        if dirs.name == "broken":
            raise ModelstatsError("unreadable")
        return original(dirs)

    monkeypatch.setattr(amt, "get_run_status", counting)
    make_run(paths.output_dir, "broken")
    AmtState(
        runcode="new-AMT",
        last_commit="0000000",
        runs={
            "old-AMT_2026-09-01_10.00.00": {"Conv": "converged", "Mif": "yes"},
            "gone-AMT_2026-08-01_10.00.00": {"Conv": "converged", "Mif": "yes"},  # archived meanwhile
            "broken": {"Conv": "converged", "Mif": "yes"},  # cannot be read any more
        },
    ).save(paths)
    evaluated(paths, shell)
    assert asked.count("old-AMT_2026-09-01_10.00.00") == 1  # BUG-102: every folder is read again
    assert "broken" in asked  # BUG-070: skipped, not fatal
    runs = AmtState.load(paths).runs
    assert runs["old-AMT_2026-09-01_10.00.00"] == {"Conv": "not_converged", "Mif": "yes"}  # refreshed
    assert runs["gone-AMT_2026-08-01_10.00.00"] == {"Conv": "converged", "Mif": "yes"}  # never forgotten
    assert runs["new-AMT_2026-10-18_10.00.00"] == {"Conv": "converged", "Mif": "yes"}
    assert runs["broken"] == {"Conv": "converged", "Mif": "yes"}  # unreadable: the entry stays (BUG-070)


@pytest.mark.usefixtures("clock")
def test_evaluate_magpie(
    paths: AmtPaths,
    shell: FakeShell,
    statuses: dict[str, RunStatus],
    monkeypatch: pytest.MonkeyPatch,
    caplog: pytest.LogCaptureFixture,
) -> None:
    assert paths.report_repo is not None
    (paths.report_repo / "data-changelog.csv").write_text("version,change\n")
    default = make_run(paths.output_dir, "default_2026-10-18_10.00.00", {"report.rds": b""})
    second = make_run(paths.output_dir, "default_2026-10-18_12.00.00", {"report.rds": b""})
    weekly = make_run(paths.output_dir, "weeklyTests_SSP2_2026-10-18_11.00.00")
    stale = make_run(paths.output_dir, "weeklyTests_SSP2_2026-10-01_11.00.00")
    make_run(paths.output_dir, ".hidden_2026-10-18_11.00.00")
    monkeypatch.setattr(
        amt, "_creation_date", lambda path: date(2026, 10, 1) if path == stale else date(2026, 10, 18)
    )
    statuses[default.name] = good_run(run_type="nlp_apr17", modelstat="222", conv=None)
    statuses[second.name] = good_run(run_type="nlp_apr17", modelstat="222", conv=None)
    statuses[weekly.name] = good_run(
        run_type="nlp_apr17", modelstat="227", conv=None, run_in_app_results="no"
    )
    monkeypatch.setattr(amt, "print_run_table", lambda *a, **k: pytest.fail("not for MAgPIE"))
    changelog_key = ("Rscript", "-e", amt._ADD_TO_CHANGELOG_R)
    evaluated(AmtPaths(model_dir=paths.model_dir), shell, model="MAgPIE")  # no report repository
    assert not any(call.argv[:3] == changelog_key for call in shell.calls)  # no changelog
    assert AmtState.load(paths).runs == {}  # R saved no gRS for MAgPIE
    shell.calls.clear()
    with caplog.at_level(logging.INFO):
        text = evaluated(paths, shell, model="MAgPIE", mattermost_url=HOOK, publish=True)
    assert "use 'weeklyTests' as keyword" in text
    assert "Summary: Some run(s) did not converge. Some run(s) did not report correctly" in text
    assert stale.name not in text and ".hidden" not in text
    changelogs = [call for call in shell.calls if call.argv[:3] == changelog_key]
    assert [call.argv[3:] for call in changelogs] == [
        (str(default), changelogs[0].argv[4], default.name),
        (str(second), changelogs[0].argv[4], second.name),
    ]  # BUG-088: one changelog file; BUG-099: run in the model folder
    assert changelogs[0].cwd == paths.model_dir
    assert ("git", "add", "data-changelog.csv") in {call.argv for call in shell.calls}
    assert (
        shell.posts[-1][1]["text"]
        == "Some MAgPIE tests produce warnings. Please check https://gitlab.pik-potsdam.de/landuse/testing_suite"
    )
    assert HOOK not in caplog.text
    statuses[weekly.name] = good_run(run_type="nlp_apr17", modelstat="222", conv=None)
    (paths.report_repo / "data-changelog.csv").unlink()

    original_run = shell.run

    def run(argv: Sequence[str], **kwargs: Any) -> subprocess.CompletedProcess[str]:
        if tuple(argv[:3]) == changelog_key:
            raise subprocess.CalledProcessError(1, list(argv), "", "Error in readRDS\n")
        return original_run(argv, **kwargs)

    monkeypatch.setattr(_shell, "run", run)
    with caplog.at_level(logging.INFO):
        evaluated(paths, shell, model="MAgPIE", mattermost_url=HOOK)
    assert shell.posts[-1][1]["text"].startswith("MAgPIE tests completed successfully.")
    assert "Could not copy" in caplog.text and "Error in readRDS" in caplog.text
    assert shlex.join(["Rscript", "-e", amt._ADD_TO_CHANGELOG_R, str(default)]) in caplog.text


def test_notify_mattermost_posts_json(shell: FakeShell) -> None:
    notify_mattermost('hello\nworld "quoted"', HOOK)
    assert shell.posts == [(HOOK, {"text": 'hello\nworld "quoted"'})]


# --- console script -----------------------------------------------------------------------------


def test_main_requires_a_model(shell: FakeShell, capsys: pytest.CaptureFixture[str]) -> None:
    assert main(["--model-dir", "."]) == 2
    assert "--model" in capsys.readouterr().err
    assert main(["--model", "remind"]) == 2
    assert main(["--model", "REMIND", "--model-dir", "."]) == 2  # --publish needs --report-repo
    assert "--report-repo" in capsys.readouterr().err


@pytest.mark.usefixtures("clock")
def test_main_runs_the_next_step(
    paths: AmtPaths, shell: FakeShell, monkeypatch: pytest.MonkeyPatch, capsys: pytest.CaptureFixture[str]
) -> None:
    seen: dict[str, Any] = {}
    monkeypatch.setattr(amt, "evaluate_runs", lambda p, **kwargs: seen.update(kwargs) or "")
    monkeypatch.setenv(amt.MATTERMOST_URL_ENV, HOOK)
    paths.status_file.write_text("next:evaluate\n")
    assert main(["--model", "REMIND", "--model-dir", str(paths.model_dir), "--no-publish"]) == 0
    assert seen["mattermost_url"] == HOOK and seen["publish"] is False and seen["user"] == "alice"
    assert "Calling 'evaluateRuns'" in capsys.readouterr().err
    paths.status_file.unlink()
    assert main(["--model", "REMIND", "--model-dir", str(paths.model_dir), "--no-publish"]) == 1
    captured = capsys.readouterr().err
    assert "Error: " in captured and ".testsstatus: No such file" in captured and HOOK not in captured
    assert not logging.getLogger("modelstats").handlers  # the command's handler is gone again
    assert main(["--model", "REMIND", "--model-dir", str(paths.model_dir / "nowhere"), "--no-publish"]) == 2
    assert "does not exist" in capsys.readouterr().err
    assert main(["--model", "REMIND", "--model-dir", str(paths.model_dir), "--report-repo", "/nowhere"]) == 2
    assert "does not exist" in capsys.readouterr().err
