from __future__ import annotations

import os
import shutil
from datetime import datetime
from pathlib import Path
from zoneinfo import ZoneInfo

import pytest

from modelstats import _shell, run_status
from modelstats._gdx import GdxReadError
from modelstats.run_status import EXPLAIN_MODELSTAT, RunStatus, get_run_status, to_dataframe
from tests.conftest import FIXED_NOW, GDX_NA, FakeShell, make_run, write_gdx

FULL_LOG_OK = "GAMS 46.1.0\n*** LOOPS = 12\n*** Status: Normal completion\n"
FULL_LOG_ERR = "*** LOOPS = 3\n*** Status: Execution error(s)\n"


def status_of(run: Path, *, user: str | None = None, detailed: bool = True) -> RunStatus:
    rows = get_run_status(run, user=user, detailed=detailed)
    assert len(rows) == 1
    return rows[0]


@pytest.fixture
def off_cluster(shell: FakeShell) -> FakeShell:
    shell.cluster = False
    return shell


@pytest.fixture
def in_slurm(monkeypatch: pytest.MonkeyPatch) -> dict[str, str]:
    """Replace found_in_slurm by a lookup keyed on the folder name (default: "no")."""
    answers: dict[str, str] = {}
    monkeypatch.setattr(run_status, "found_in_slurm", lambda run_dir, user: answers.get(run_dir.name, "no"))
    return answers


# --- selection and order of the run folders ---------------------------------------------------


def test_only_directories_newest_first(tmp_path: Path, off_cluster: FakeShell, clock: object) -> None:
    old = make_run(tmp_path, "old")
    new = make_run(tmp_path, "new")
    (tmp_path / "file.txt").write_text("x")
    os.utime(old, (1_000_000, 1_000_000))
    os.utime(new, (2_000_000, 2_000_000))
    rows = get_run_status([old, new, tmp_path / "file.txt", tmp_path / "missing"])
    assert [row.name for row in rows] == ["new", "old"]
    assert [row.path for row in rows] == [new.resolve(), old.resolve()]
    rows = get_run_status([old, new], newest_first=False)
    assert [row.name for row in rows] == ["old", "new"]
    assert get_run_status([]) == []
    assert get_run_status(tmp_path / "missing") == []


def test_bare_folder_off_cluster(tmp_path: Path, off_cluster: FakeShell, clock: object) -> None:
    row = status_of(make_run(tmp_path, "bare"))
    assert row == RunStatus(
        name="bare",
        path=(tmp_path / "bare").resolve(),
        job_in_slurm="NA",
        run_type="NA",
        modelstat="NA",
        run_in_app_results=None,
        mif="NA",
        iter="NA",
        run_status="full.log missing",
        warnings="NA",
        conv="NA",
        runtime=None,
    )


def test_same_folder_name_in_two_places_gives_two_rows(
    tmp_path: Path, off_cluster: FakeShell, clock: object
) -> None:
    a = make_run(tmp_path / "a", "run")
    b = make_run(tmp_path / "b", "run")
    assert [row.path for row in get_run_status([a, b], newest_first=False)] == [a.resolve(), b.resolve()]


def test_uses_current_user_and_found_in_slurm_on_cluster(
    tmp_path: Path, shell: FakeShell, clock: object, monkeypatch: pytest.MonkeyPatch
) -> None:
    seen: list[tuple[Path, str]] = []

    def fake(run_dir: Path, user: str) -> str:
        seen.append((run_dir, user))
        return "standby"

    monkeypatch.setattr(run_status, "found_in_slurm", fake)
    run = make_run(tmp_path, "r")
    assert status_of(run).job_in_slurm == "standby"
    assert status_of(run, user="bob").job_in_slurm == "standby"
    assert seen == [(run.resolve(), "alice"), (run.resolve(), "bob")]


def test_magpie_recursion_uses_the_callers_user(
    tmp_path: Path, shell: FakeShell, clock: object, monkeypatch: pytest.MonkeyPatch
) -> None:
    # BUG-025: R's recursive getRunStatus() used the default user
    seen: list[tuple[str, str]] = []
    monkeypatch.setattr(
        run_status, "found_in_slurm", lambda run_dir, user: seen.append((run_dir.name, user)) or "standby"
    )
    make_run(tmp_path / "magpie" / "output", "C_X-mag-3", {"full.log": "*** LOOPS = y2050\n"})
    cfg = (
        "model_name: REMIND\ngms:\n  optimization: nash\n"
        f"path_magpie: {tmp_path / 'magpie'}\ncfg_mag:\n  results_folder: output/C_X-mag-3\n"
    )
    run = make_run(
        tmp_path, "C_X-rem-3", {"full.log": "running\n", "log.txt": "Starting MAgPIE...\n", "config.yml": cfg}
    )
    assert status_of(run, user="bob").run_status == "mag-3 y2050"
    assert seen == [("C_X-rem-3", "bob"), ("C_X-mag-3", "bob")]


# --- RunType ------------------------------------------------------------------------------------


def test_run_type_needs_a_config(tmp_path: Path, off_cluster: FakeShell, clock: object) -> None:
    assert status_of(make_run(tmp_path, "cfg", config="remind")).run_type == "nash"
    assert status_of(make_run(tmp_path, "nocfg", {"full.lst": "x"})).run_type == "NA"


# --- modelstat ----------------------------------------------------------------------------------


def test_modelstat_from_gdx_scalar_and_vector(tmp_path: Path, off_cluster: FakeShell, clock: object) -> None:
    run = make_run(tmp_path, "r")
    write_gdx(run / "fulldata.gdx", scalars={"o_modelstat": 2})
    assert status_of(run).modelstat == "2: Locally Optimal"
    write_gdx(
        run / "fulldata.gdx",
        sets={"t": ["y1", "y2", "y3"]},
        parameters={"p80_modelstat": (["t"], [("y1", 2), ("y2", 0), ("y3", 7)])},
    )
    assert status_of(run).modelstat == "2.7"
    write_gdx(run / "fulldata.gdx", scalars={"o_modelstat": 13})
    assert status_of(run).modelstat == "13: Error No Solution"
    write_gdx(run / "fulldata.gdx", scalars={"other": 1})
    assert status_of(run).modelstat == "NA"


def test_modelstat_from_runstatistics(tmp_path: Path, off_cluster: FakeShell, clock: object) -> None:
    assert status_of(make_run(tmp_path, "remind", stats="remind")).modelstat == "2: Locally Optimal"
    assert status_of(make_run(tmp_path, "magpie", stats="magpie")).modelstat == "227"
    run = make_run(tmp_path, "gdxwins", stats="magpie")
    write_gdx(run / "fulldata.gdx", scalars={"o_modelstat": 4})
    assert status_of(run).modelstat == "4: Infeasible"


def test_modelstat_from_runstatistics_with_na(
    tmp_path: Path, off_cluster: FakeShell, clock: object, monkeypatch: pytest.MonkeyPatch
) -> None:
    run = make_run(tmp_path, "r", stats="remind")
    stats: dict[str, object] = {"config": {"model_name": "MAgPIE"}, "modelstat": [2.0, None, 7.0]}
    monkeypatch.setattr(run_status, "read_rda", lambda path: {"stats": stats})
    assert status_of(run).modelstat == "2NA7"  # paste0(as.character(c(2, NA, 7)))
    stats["modelstat"] = None
    assert status_of(run).modelstat == "NA"  # a MAgPIE NA printed NA in R
    stats["config"] = {"model_name": "REMIND"}
    assert status_of(run).modelstat == "NA"  # BUG-059: R left the cell blank
    stats["modelstat"] = [2.0, 2.0]
    assert status_of(run).modelstat == "NA"


def test_corrupt_gdx_falls_back_to_runstatistics(
    tmp_path: Path, off_cluster: FakeShell, clock: object
) -> None:
    run = make_run(tmp_path, "r", {"fulldata.gdx": b"garbage" * 20}, stats="remind")
    assert status_of(run).modelstat == "2: Locally Optimal"


def test_explain_modelstat_table() -> None:
    assert EXPLAIN_MODELSTAT["7"] == "Intermed Nonoptimal" and len(EXPLAIN_MODELSTAT) == 8


# --- latest gdx ---------------------------------------------------------------------------------


def test_latest_gdx_by_iteration_then_mtime(tmp_path: Path, off_cluster: FakeShell, clock: object) -> None:
    run = make_run(tmp_path, "r")
    write_gdx(run / "fulldata.gdx", scalars={"o_iterationNumber": 5, "o_modelstat": 2})
    write_gdx(run / "non_optimal.gdx", scalars={"o_iterationNumber": 7, "o_modelstat": 6})
    os.utime(run / "fulldata.gdx", (2_000_000, 2_000_000))
    os.utime(run / "non_optimal.gdx", (1_000_000, 1_000_000))
    assert status_of(run).modelstat == "6: Intermed Infes"  # higher iteration wins although older
    write_gdx(run / "non_optimal.gdx", scalars={"o_modelstat": 6})  # no iteration number: newest file wins
    os.utime(run / "non_optimal.gdx", (1_000_000, 1_000_000))
    assert status_of(run).modelstat == "2: Locally Optimal"
    (run / "non_optimal.gdx").write_bytes(b"corrupt" * 20)  # unreadable: newest file wins too
    os.utime(run / "non_optimal.gdx", (3_000_000, 3_000_000))
    assert status_of(run).modelstat == "NA"


# --- runInAppResults ----------------------------------------------------------------------------


def test_run_in_app_results(
    tmp_path: Path, shell: FakeShell, in_slurm: dict[str, str], clock: object, monkeypatch: pytest.MonkeyPatch
) -> None:
    archive = tmp_path / "archive"
    archive.mkdir()
    monkeypatch.setattr(_shell, "REMIND_RESULTS_ARCHIVE", archive)
    run = make_run(tmp_path, "r", stats="remind")  # id abc123
    assert status_of(run).run_in_app_results == "no"
    report = archive / "abc123.rds"
    report.write_text("")
    os.utime(report, (5_000_000, 5_000_000))
    assert status_of(run).run_in_app_results == "yes"
    overview = archive / "overview.rds"
    overview.write_text("")
    os.utime(overview, (5_000_000 - 700, 5_000_000 - 700))
    assert status_of(run).run_in_app_results == "no"
    os.utime(overview, (5_000_000 - 500, 5_000_000 - 500))
    assert status_of(run).run_in_app_results == "yes"
    assert status_of(make_run(tmp_path, "noid")).run_in_app_results == "no"


def test_run_in_app_results_magpie_archive_from_env(
    tmp_path: Path, shell: FakeShell, in_slurm: dict[str, str], clock: object, monkeypatch: pytest.MonkeyPatch
) -> None:
    run = make_run(tmp_path, "m", stats="magpie")  # id mag42
    monkeypatch.delenv(_shell.MAGPIE_RESULTS_ARCHIVE_ENV, raising=False)
    assert status_of(run).run_in_app_results == "no"
    archive = tmp_path / "magarchive"
    archive.mkdir()
    (archive / "mag42.rds").write_text("")
    monkeypatch.setenv(_shell.MAGPIE_RESULTS_ARCHIVE_ENV, str(archive))
    assert status_of(run).run_in_app_results == "yes"


# --- Mif ----------------------------------------------------------------------------------------


def test_iteration_max_from_config_unless_autoconverging(
    tmp_path: Path, off_cluster: FakeShell, clock: object
) -> None:
    gms = "cm_iteration_max = 50;\n"
    cfg = (
        "model_name: REMIND\ngms:\n  optimization: nash\n  cm_nash_autoconverge: 0\n  cm_iteration_max: 30\n"
    )
    run = make_run(tmp_path, "r", {"full.log": FULL_LOG_OK, "full.gms": gms, "config.yml": cfg})
    assert status_of(run).iter == "12/30"  # autoconverge off: the config wins over full.gms
    cfg = "model_name: REMIND\ngms:\n  optimization: negishi\n  cm_nash_autoconverge: 1\n  cm_iteration_max: 30\n"
    (run / "config.yml").write_text(cfg)
    assert status_of(run).iter == "12/30"  # not a nash run


def test_mif_remind(tmp_path: Path, off_cluster: FakeShell, clock: object) -> None:
    run = make_run(tmp_path, "r", config="remind")
    assert status_of(run).mif == "no"
    (run / "REMIND_generic_SSP2-NPi-AMT.mif").write_text("x")
    assert status_of(run).mif == "yes"
    (run / "REMIND_generic_SSP2-NPi-AMT_summation_errors.csv").write_text("variable\n")
    assert status_of(run).mif == "sumErr"
    assert status_of(make_run(tmp_path, "nocfg")).mif == "NA"


def test_mif_magpie_needs_a_big_validation_mif(tmp_path: Path, off_cluster: FakeShell, clock: object) -> None:
    run = make_run(tmp_path, "m", config="magpie", stats="magpie")
    assert status_of(run).mif == "no"
    (run / "validation.mif").write_text("x" * 99999)
    assert status_of(run).mif == "no"
    (run / "validation.mif").write_text("x" * 100000)
    assert status_of(run).mif == "yes"


# --- Iter and RunStatus from full.log -----------------------------------------------------------


def test_iter_and_status_from_full_log(tmp_path: Path, off_cluster: FakeShell, clock: object) -> None:
    run = make_run(tmp_path, "r", {"full.log": FULL_LOG_OK})
    row = status_of(run)
    assert (row.iter, row.run_status) == ("12", "Normal completion")
    row = status_of(make_run(tmp_path, "cfg", {"full.log": FULL_LOG_OK}, config="remind"))
    assert row.iter == "12/50"
    row = status_of(
        make_run(tmp_path, "noloop", {"full.log": "*** Status: Compilation error(s)\n"}, config="remind")
    )
    assert (row.iter, row.run_status) == ("NA/50", "Compilation error")


def test_iteration_max_from_full_gms_for_autoconverging_nash(
    tmp_path: Path, off_cluster: FakeShell, clock: object
) -> None:
    full_gms = "x\ncm_iteration_max = 20;\n$setglobal foo\ncm_iteration_max = 30;  \ncm_iteration_max = 0;\n"
    run = make_run(tmp_path, "r", {"full.log": FULL_LOG_OK, "full.gms": full_gms}, config="remind")
    assert status_of(run).iter == "12/30"
    run = make_run(tmp_path, "noline", {"full.log": FULL_LOG_OK, "full.gms": "nothing\n"}, config="remind")
    assert status_of(run).iter == "12"


def test_last_status_line_wins(tmp_path: Path, off_cluster: FakeShell, clock: object) -> None:
    run = make_run(
        tmp_path, "r", {"full.log": "*** Status: Execution error(s)\n*** Status: Normal completion\n"}
    )
    assert status_of(run).run_status == "Normal completion"
    run = make_run(tmp_path, "r2", {"full.log": "no stars Status: Normal completion\n"})
    assert status_of(run).run_status == "Run interrupted"


def test_status_is_cut_to_17_characters(tmp_path: Path, off_cluster: FakeShell, clock: object) -> None:
    run = make_run(tmp_path, "r", {"full.log": "*** Status: Terminated due to an unexpected problem\n"})
    assert status_of(run).run_status == "Terminated due to"


def test_full_log_missing_and_remind_lock(tmp_path: Path, off_cluster: FakeShell, clock: object) -> None:
    assert status_of(make_run(tmp_path, "r")).run_status == "full.log missing"
    run = make_run(tmp_path, "lock", {"log.txt": "starting\ntry to acquire model lock\n"})
    assert status_of(run).run_status == "Wait REMIND lock"
    run = make_run(tmp_path, "nolock", {"log.txt": "try to acquire model lock\ngot it\n"})
    assert status_of(run).run_status == "full.log missing"


def test_off_cluster_na_status_is_run_interrupted(
    tmp_path: Path, off_cluster: FakeShell, clock: object
) -> None:
    assert status_of(make_run(tmp_path, "r", {"full.log": "no status yet\n"})).run_status == "Run interrupted"


# --- RunStatus on the cluster -------------------------------------------------------------------


@pytest.mark.parametrize(
    ("message", "expected"),
    [
        ("slurmstepd: error: *** JOB 1 CANCELLED AT 2026 DUE TO TIME LIMIT ***", "Timeout interrupt"),
        ("slurmstepd: error: Detected 1 oom-kill event(s)", "Memory interrupt"),
        ("slurmstepd: error: exceeded memory limit", "Memory interrupt"),
        ("slurmstepd: error: *** JOB 1 CANCELLED DUE TO PREEMPTION ***", "Preempt interrupt"),
        ("slurmstepd: error: *** JOB 1 CANCELLED DUE TO JOB REQUEUE ***", "Run requeued"),
        ("slurmstepd: error: *** JOB 1 CANCELLED AT 2026 ***", "Run cancelled"),
        ("slurmstepd: something else", "NA"),
        ("no slurm line at all", "NA"),
    ],
)
def test_slurm_errors_in_log_txt(
    tmp_path: Path, shell: FakeShell, in_slurm: dict[str, str], clock: object, message: str, expected: str
) -> None:
    run = make_run(tmp_path, "r", {"full.log": "running\n", "log.txt": f"start\n{message}\n"})
    assert status_of(run).run_status == expected
    in_slurm["r"] = "priority pending"  # a pending job reads log.txt the same way
    assert status_of(run).run_status == expected


def test_interrupted_or_restarted_without_log_txt(
    tmp_path: Path, shell: FakeShell, in_slurm: dict[str, str], clock: object
) -> None:
    run = make_run(tmp_path, "r", {"full.log": "running\n"})
    assert status_of(run).run_status == "Run interrupted"
    in_slurm["r"] = "standby pending"
    assert status_of(run).run_status == "Run restarted"


def test_run_in_progress_and_conopt_spy(
    tmp_path: Path, shell: FakeShell, in_slurm: dict[str, str], clock: object
) -> None:
    run = make_run(tmp_path, "r", {"full.log": "running\n"})
    in_slurm["r"] = "standby"
    assert status_of(run).run_status == "Run in progress"
    grid = run / "225_nash" / "grid1" / "gmsgrid.log"
    grid.parent.mkdir(parents=True)
    grid.write_text("")
    stamp = (FIXED_NOW.timestamp() - 2.5 * 3600, FIXED_NOW.timestamp() - 2.5 * 3600)
    os.utime(grid, stamp)
    assert status_of(run).run_status == "conoptspy >2h"
    write_gdx(run / "fulldata.gdx", scalars={"o_modelstat": 2})
    os.utime(run / "fulldata.gdx", (FIXED_NOW.timestamp() - 600, FIXED_NOW.timestamp() - 600))
    assert status_of(run).run_status == "Run in progress"  # the gdx moved recently
    os.utime(run / "fulldata.gdx", stamp)
    assert status_of(run).run_status == "conoptspy >2h"
    os.utime(grid, (FIXED_NOW.timestamp() - 300, FIXED_NOW.timestamp() - 300))
    assert status_of(run).run_status == "Run in progress"  # conopt wrote 5 minutes ago


def test_magpie_running_in_coupled_run(
    tmp_path: Path, shell: FakeShell, in_slurm: dict[str, str], clock: object
) -> None:
    magpie = make_run(tmp_path / "magpie" / "output", "C_X-mag-3", {"full.log": "*** LOOPS = y2050\n"})
    cfg = (
        "model_name: REMIND\ntitle: C_X-rem-3\ngms:\n  optimization: nash\n"
        f"path_magpie: {tmp_path / 'magpie'}\ncfg_mag:\n  results_folder: output/C_X-mag-3\n"
    )
    run = make_run(
        tmp_path,
        "C_X-rem-3",
        {"full.log": "running\n", "log.txt": "x\nStarting MAgPIE...\n\n", "config.yml": cfg},
    )
    in_slurm["C_X-rem-3"] = "standby"
    assert status_of(run).run_status == "mag-3 y2050"
    (magpie / "full.log").unlink()
    assert status_of(run).run_status == "mag-3 NA"
    shutil.rmtree(magpie)
    assert status_of(run).run_status == "mag-3 NA"  # a missing folder was an NA row in R (BUG-018)
    magpie.write_text("")  # a file: R saw no row at all
    assert status_of(run).run_status == "mag-3"
    (run / "config.yml").write_text(cfg.replace("output/C_X-mag-3", "output/C_X-mag-123"))
    assert status_of(run).run_status == "mag-output/C_X-mag-123 NA"  # gsub() left a 3-digit iteration alone
    cfg_no_path = (
        "model_name: REMIND\ngms:\n  optimization: nash\ncfg_mag:\n  results_folder: output/C_X-mag-3\n"
    )
    run2 = make_run(
        tmp_path,
        "nopath",
        {"full.log": "running\n", "log.txt": "Starting MAgPIE...\n", "config.yml": cfg_no_path},
    )
    in_slurm["nopath"] = "standby"
    assert status_of(run2).run_status == "mag-3"
    run3 = make_run(
        tmp_path, "nomag", {"full.log": "running\n", "log.txt": "Starting MAgPIE...\n"}, config="remind"
    )
    in_slurm["nomag"] = "standby"
    assert status_of(run3).run_status == "mag-"


def test_running_reporting(tmp_path: Path, shell: FakeShell, in_slurm: dict[str, str], clock: object) -> None:
    run = make_run(tmp_path, "r", {"full.log": FULL_LOG_OK, "log.txt": "Starting output generation for r\n"})
    in_slurm["r"] = "standby"
    assert status_of(run).run_status == "Running reporting"
    shell.cluster = False  # off the cluster jobInSLURM is "NA", which is not "no": the rule applies too
    assert status_of(run).run_status == "Running reporting"
    shell.cluster = True
    (run / "log.txt").write_text("Starting output generation for r\nFinished output generation for r\n")
    assert status_of(run).run_status == "Normal completion"
    in_slurm["r"] = "no"
    (run / "log.txt").write_text("Starting output generation for r\n")
    assert status_of(run).run_status == "Normal completion"


def test_abort_on_consecutive_infeasibilities(tmp_path: Path, off_cluster: FakeShell, clock: object) -> None:
    run = make_run(tmp_path, "r", {"full.log": FULL_LOG_ERR})
    assert status_of(run).run_status == "Execution error"
    write_gdx(
        run / "abort.gdx",
        scalars={"cm_abortOnConsecFail": 4},
        sets={"all_regi": ["CAZ", "EUR", "USA"]},
        parameters={"p80_trackConsecFail": (["all_regi"], [("EUR", 4), ("USA", 2)])},
    )
    assert status_of(run).run_status == "Abort EUR 4*Infes"
    write_gdx(
        run / "abort.gdx",
        scalars={"cm_abortOnConsecFail": 4},
        sets={"all_regi": ["CAZ", "EUR", "USA"]},
        parameters={"p80_trackConsecFail": (["all_regi"], [("EUR", 4), ("USA", 4)])},
    )
    assert status_of(run).run_status == "Abort 2R*4*Infes"
    write_gdx(
        run / "abort.gdx",
        scalars={"cm_abortOnConsecFail": 0},
        sets={"all_regi": ["EUR"]},
        parameters={"p80_trackConsecFail": (["all_regi"], [("EUR", 0)])},
    )
    assert status_of(run).run_status == "Execution error"
    write_gdx(
        run / "abort.gdx",
        scalars={"cm_abortOnConsecFail": 4},
        sets={"regi": ["EUR"]},
        parameters={"p80_trackConsecFail": (["regi"], [("EUR", 3)])},  # no region at the maximum
    )
    assert status_of(run).run_status == "Execution error"
    write_gdx(
        run / "abort.gdx",
        scalars={"cm_abortOnConsecFail": 4},
        sets={"regi": ["EUR"]},
        parameters={
            "p80_trackConsecFail": (["regi"], [("EUR", 4)])
        },  # the first column stands in for all_regi
    )
    assert status_of(run).run_status == "Abort EUR 4*Infes"
    write_gdx(run / "abort.gdx", scalars={"cm_abortOnConsecFail": 4})
    assert status_of(run).run_status == "Execution error"
    write_gdx(run / "abort.gdx", scalars={"o_modelstat": 2})  # no cm_abortOnConsecFail at all
    assert status_of(run).run_status == "Execution error"
    (run / "abort.gdx").write_bytes(b"corrupt" * 20)
    assert status_of(run).run_status == "Execution error"


def test_run_magpie_from_log_mag_txt(
    tmp_path: Path, shell: FakeShell, in_slurm: dict[str, str], clock: object
) -> None:
    output = tmp_path / "output"
    run = make_run(output, "C_X-rem-2", {"full.log": "running\n", "log-mag.txt": "Preparing MAgPIE\n"})
    in_slurm["C_X-rem-2"] = "standby"
    assert status_of(run).run_status == "Run MAgPIE"
    make_run(tmp_path / "magpie" / "output", "C_X-mag-2", {"full.log": "*** LOOPS = y2030\n"})
    assert status_of(run).run_status == "Run MAgPIE y2030"
    (run / "log-mag.txt").write_text("Preparing MAgPIE\nStart getReport\n")
    assert status_of(run).run_status == "Run MAgPIE report"
    (run / "log-mag.txt").write_text("Preparing MAgPIE\nMAgPIE output was stored\n")
    assert status_of(run).run_status == "Run in progress"
    (run / "log-mag.txt").write_text("Preparing MAgPIE\ntry to acquire model lock\n")
    assert status_of(run).run_status == "Wait MAgPIE lock"
    in_slurm["C_X-rem-2"] = "no"
    assert status_of(run).run_status == "Run interrupted"


def test_run_magpie_from_log_txt_only_after_normal_completion(
    tmp_path: Path, shell: FakeShell, in_slurm: dict[str, str], clock: object
) -> None:
    run = make_run(tmp_path / "output", "r", {"full.log": "running\n", "log.txt": "Preparing MAgPIE\n"})
    in_slurm["r"] = "standby"
    assert status_of(run).run_status == "Run in progress"
    (run / "full.log").write_text(FULL_LOG_OK)
    assert status_of(run).run_status == "Run MAgPIE"
    (run / "log.txt").write_text("Preparing MAgPIE\ntry to acquire model lock\n")
    assert status_of(run).run_status == "Wait MAgPIE lock"  # from log.txt, after Normal completion
    odd = make_run(
        tmp_path / "log-mag.txt-tests", "r", {"full.log": "running\n", "log.txt": "Preparing MAgPIE\n"}
    )
    in_slurm["r"] = "standby"
    assert status_of(odd).run_status == "Run in progress"  # BUG-103: R's grepl() saw the folder name


def test_run_magpie_without_a_loops_line(
    tmp_path: Path, shell: FakeShell, in_slurm: dict[str, str], clock: object
) -> None:
    run = make_run(
        tmp_path / "output", "C_Y-rem-1", {"full.log": "running\n", "log-mag.txt": "Preparing MAgPIE\n"}
    )
    make_run(tmp_path / "magpie" / "output", "C_Y-mag-1", {"full.log": "starting\n"})  # no LOOPS line yet
    in_slurm["C_Y-rem-1"] = "standby"
    assert status_of(run).run_status == "Run MAgPIE"


def test_magpie_log_path_is_derived_by_replacing_output(
    tmp_path: Path, shell: FakeShell, in_slurm: dict[str, str], clock: object
) -> None:
    # R quirk kept (BUG-022): without an "output" folder in the path the run's own full.log is read
    run = make_run(tmp_path, "r", {"full.log": FULL_LOG_OK, "log-mag.txt": "Preparing MAgPIE\n"})
    in_slurm["r"] = "standby"
    assert status_of(run).run_status == "Run MAgPIE 12"


def test_not_detailed_stops_early(tmp_path: Path, off_cluster: FakeShell, clock: object) -> None:
    run = make_run(
        tmp_path,
        "r",
        {"full.log": FULL_LOG_OK, "log.txt": "There were 3 warnings\n"},
        config="remind",
        stats="remind",
    )
    row = status_of(run, detailed=False)
    assert (row.run_status, row.warnings, row.conv, row.runtime) == ("Normal completion", None, None, None)
    cfg = "model_name: REMIND\ntitle: c\ngms:\n  optimization: nash\n  CES_parameters: calibrate\n"
    calibrating = make_run(
        tmp_path,
        "c",
        {
            "full.log": FULL_LOG_OK,
            "config.yml": cfg,
            "log.txt": "CES calibration iteration 3\n",
            "REMIND_generic_c.mif": "x",
        },
    )
    row = status_of(calibrating, detailed=False)
    assert row.iter == "12" and row.summation_errors is None  # no Clb, no plausibility columns


# --- Warnings -----------------------------------------------------------------------------------


def test_warnings_magpie_from_slurm_log(tmp_path: Path, off_cluster: FakeShell, clock: object) -> None:
    run = make_run(
        tmp_path,
        "m",
        {"slurm.log": "x\nWarning messages:\n1: a\n2: b\n3\nWarning messages:\n1: c\n4\n"},
        stats="magpie",
    )
    assert status_of(run).warnings == "4"
    (run / "slurm.log").write_text("Warning message:\nsomething\n")
    assert status_of(run).warnings == "1"
    (run / "slurm.log").write_text("all fine\n")
    assert status_of(run).warnings == "0"
    (run / "slurm.log").unlink()
    assert status_of(run).warnings == "NA"


def test_warnings_remind_from_log_txt(tmp_path: Path, off_cluster: FakeShell, clock: object) -> None:
    run = make_run(
        tmp_path, "r", {"log.txt": "There were 3 warnings\nmore\nThere were 50 or more warnings\n"}
    )
    assert status_of(run).warnings == "50"
    (run / "log.txt").write_text(
        "Warning messages:\n1: a\n  explanation\n2: b\nother\nWarning messages:\n1: c\n"
    )
    assert status_of(run).warnings == "3"
    (run / "log.txt").write_text("nothing\n")
    assert status_of(run).warnings == "0"
    run = make_run(tmp_path, "mag", {"log.txt": "There were 3 warnings\n"}, stats="magpie")
    assert status_of(run).warnings == "NA"


# --- Conv ---------------------------------------------------------------------------------------


def nash_gdx(
    path: Path, s80_bool: float, iteration: float = 12, modelstats: list[float] | None = None
) -> None:
    write_gdx(
        path,
        scalars={"o_iterationNumber": iteration, "s80_bool": s80_bool, "o_modelstat": 2},
        sets={"all_regi": ["CAZ", "CHA", "EUR"], "solveinfo80": ["modelstat", "objval"]},
        parameters={
            "p80_repy": (
                ["all_regi", "solveinfo80"],
                [
                    (r, "modelstat", m)
                    for r, m in zip(["CAZ", "CHA", "EUR"], modelstats or [2, 2, 2], strict=True)
                ],
            )
        },
    )


def test_conv_from_gdx(tmp_path: Path, off_cluster: FakeShell, clock: object) -> None:
    run = make_run(tmp_path, "r", {"full.log": FULL_LOG_OK}, config="remind")
    nash_gdx(run / "fulldata.gdx", 1)
    assert status_of(run).conv == "converged"
    nash_gdx(run / "non_optimal.gdx", 1, iteration=3)
    assert status_of(run).conv == "converged (had INFES)"
    (run / "non_optimal.gdx").unlink()
    nash_gdx(run / "fulldata.gdx", 0, iteration=50)
    assert status_of(run).conv == "not_converged"
    nash_gdx(run / "fulldata.gdx", 0, iteration=20, modelstats=[2, 7, 2])
    assert status_of(run).conv == "272"
    write_gdx(run / "fulldata.gdx", scalars={"o_modelstat": 2})
    assert status_of(run).conv == "NA"
    (run / "fulldata.gdx").write_bytes(b"garbage" * 20)  # read through the capped worker
    assert status_of(run).conv == "NA"


def test_conv_without_p80_repy(tmp_path: Path, off_cluster: FakeShell, clock: object) -> None:
    run = make_run(tmp_path, "r", {"full.log": FULL_LOG_OK}, config="remind")
    write_gdx(run / "fulldata.gdx", scalars={"o_modelstat": 2, "s80_bool": 0, "o_iterationNumber": 3})
    assert status_of(run).conv == ""  # BUG-101: readGDX() gave NULL, paste() of it ""


def test_conv_only_for_nash_runs_with_a_gdx(tmp_path: Path, off_cluster: FakeShell, clock: object) -> None:
    run = make_run(tmp_path, "r", {"full.log": FULL_LOG_OK}, config="remind")
    assert status_of(run).conv == "NA"
    run = make_run(
        tmp_path,
        "negishi",
        {"full.log": FULL_LOG_OK, "config.yml": "model_name: REMIND\ngms:\n  optimization: negishi\n"},
    )
    nash_gdx(run / "fulldata.gdx", 1)
    assert status_of(run).conv == "NA"


def test_conv_not_converged_needs_a_known_iteration_max(
    tmp_path: Path, off_cluster: FakeShell, clock: object
) -> None:
    run = make_run(
        tmp_path,
        "r",
        {"full.log": FULL_LOG_OK, "config.yml": "model_name: REMIND\ngms:\n  optimization: nash\n"},
    )
    nash_gdx(run / "fulldata.gdx", 0, iteration=50, modelstats=[2, 2, 6])
    assert status_of(run).conv == "226"


# --- Calib Iter ---------------------------------------------------------------------------------


def test_calibration_iteration_and_convergence(tmp_path: Path, off_cluster: FakeShell, clock: object) -> None:
    cfg = "model_name: REMIND\ntitle: c\ngms:\n  optimization: nash\n  CES_parameters: calibrate\n  cm_iteration_max: 50\n"
    run = make_run(
        tmp_path,
        "c",
        {
            "full.log": FULL_LOG_OK,
            "config.yml": cfg,
            "log.txt": "CES calibration iteration 1\nCES calibration iteration 3\n",
        },
    )
    nash_gdx(run / "fulldata.gdx", 1)
    row = status_of(run)
    assert (row.run_type, row.iter, row.conv) == ("Calib_nash", "12/50 Clb: 3", "converged")
    for number in range(11):
        (run / f"fulldata_{number}.gdx").write_text("")
    assert status_of(run).conv == "Clb_converged"
    for number in range(11):
        (run / f"fulldata_{number}.gdx").unlink()
        (run / f"input_{number}.gdx").write_text("")
    assert status_of(run).conv == "Clb_converged"  # more than 10 input_*.gdx files count too
    (run / "log.txt").write_text("CES calibration iteration 0\n")
    assert status_of(run).iter == "12/50"
    # R quirk kept (BUG-024): 1-2 digit chunks
    (run / "log.txt").write_text("CES calibration iteration 123\n")
    assert status_of(run).iter == "12/50 Clb: 3"


# --- Runtime ------------------------------------------------------------------------------------


def test_runtime_from_gams_times(
    tmp_path: Path, off_cluster: FakeShell, clock: object, data_dir: Path
) -> None:
    assert status_of(make_run(tmp_path, "r", stats="remind")).runtime == 25 * 3600 + 30
    assert (
        status_of(
            make_run(tmp_path, "nostats", {"runstatistics.rda": (data_dir / "types.rda").read_bytes()})
        ).runtime
        is None
    )
    nostart = (data_dir / "runstatistics_nostart.rda").read_bytes()  # an end without a start
    assert status_of(make_run(tmp_path, "nostart", {"runstatistics.rda": nostart})).runtime is None


def test_runtime_since_prepare_start_while_running(
    tmp_path: Path, shell: FakeShell, in_slurm: dict[str, str], clock: object
) -> None:
    run = make_run(tmp_path, "m", stats="magpie")  # timePrepareStart 2026-03-28 10:00 Berlin
    assert status_of(run).runtime is None
    in_slurm["m"] = "standby"
    start = datetime(2026, 3, 28, 10, 0, tzinfo=ZoneInfo("Europe/Berlin"))
    # DST-safe: 1 h less than the wall clock difference
    assert status_of(run).runtime == FIXED_NOW.timestamp() - start.timestamp()
    off_cluster_run = make_run(tmp_path, "off", stats="magpie")
    shell.cluster = False  # jobInSLURM is "NA" off the cluster, which is not "no": still counted
    assert status_of(off_cluster_run).runtime == FIXED_NOW.timestamp() - start.timestamp()


# --- plausibility checks ------------------------------------------------------------------------


def test_plausibility_columns(tmp_path: Path, off_cluster: FakeShell, clock: object, data_dir: Path) -> None:
    run = make_run(tmp_path, "r", config="remind")
    assert status_of(run).summation_errors is None
    (run / "REMIND_generic_SSP2-NPi-AMT.mif").write_text("x")
    row = status_of(run)
    assert (row.summation_errors, row.range_errors, row.fix_errors) == (0, 0, 0)
    assert (row.missing_proj_vars, row.proj_summation_errors, row.proj_summation_errors_regional) == (
        None,
        None,
        None,
    )
    (run / "REMIND_generic_SSP2-NPi-AMT_summation_errors.csv").write_text(
        "variable,value\nA,1\nB,2\nA,3\n,,3\n\n"
    )
    (run / "REMIND_generic_SSP2-NPi-AMT_range_errors.txt").write_text("header\nline 1\n\nline 2\n")
    (run / "log_fixOnRef.csv").write_text("variable,diff\nX,1\n")
    (run / "projectSummations.rds").write_bytes((data_dir / "projectSummations.rds").read_bytes())
    row = status_of(run)
    assert (row.summation_errors, row.range_errors, row.fix_errors) == (2, 2, 1)
    assert (row.missing_proj_vars, row.proj_summation_errors, row.proj_summation_errors_regional) == (
        3,
        0,
        None,
    )
    (run / "REMIND_generic_SSP2-NPi-AMT_range_errors.txt").write_text("")
    assert status_of(run).range_errors == 0
    (run / "projectSummations.rds").write_bytes((data_dir / "projectSummations_null.rds").read_bytes())
    row = status_of(run)
    assert (row.missing_proj_vars, row.proj_summation_errors, row.proj_summation_errors_regional) == (
        None,
        1,
        None,
    )


def test_plausibility_scalars(
    tmp_path: Path, off_cluster: FakeShell, clock: object, monkeypatch: pytest.MonkeyPatch
) -> None:
    run = make_run(
        tmp_path, "r", {"REMIND_generic_SSP2-NPi-AMT.mif": "x", "projectSummations.rds": b""}, config="remind"
    )
    summations = {
        "ScenarioMIP": {"missingVars": [4], "checkSummations": [1, 2], "checkSummationsRegional": {"n": 3}}
    }
    monkeypatch.setattr(run_status, "read_rds", lambda path: summations)
    row = status_of(run)
    # BUG-019: a one-item list (named or not) held its item, a vector stopped R
    assert (row.missing_proj_vars, row.proj_summation_errors, row.proj_summation_errors_regional) == (
        4,
        None,
        3,
    )
    summations["ScenarioMIP"] = {"missingVars": True, "checkSummations": 0, "checkSummationsRegional": None}
    row = status_of(run)
    assert (row.missing_proj_vars, row.proj_summation_errors, row.proj_summation_errors_regional) == (
        True,
        0,
        None,
    )  # TRUE printed as is


def test_range_errors_file_follows_the_title(tmp_path: Path, off_cluster: FakeShell, clock: object) -> None:
    cfg = "model_name: REMIND\ntitle: a/b\ngms:\n  optimization: nash\n"
    run = make_run(tmp_path / "x", "r", {"config.yml": cfg})
    (run / "REMIND_generic_a").mkdir()
    (run / "REMIND_generic_a" / "b.mif").write_text("x")
    (run / "REMIND_generic_a" / "b_range_errors.txt").write_text("h\n1\n2\n")
    assert status_of(run).range_errors == 2  # paste0("REMIND_generic_", title, "_range_errors.txt")


def test_plausibility_not_for_magpie(tmp_path: Path, off_cluster: FakeShell, clock: object) -> None:
    run = make_run(tmp_path, "m", {"validation.mif": "x"}, config="magpie", stats="magpie")
    assert status_of(run).summation_errors is None


# --- to_dataframe -------------------------------------------------------------------------------


def test_to_dataframe_uses_r_names(tmp_path: Path, off_cluster: FakeShell, clock: object) -> None:
    rows = get_run_status(
        [make_run(tmp_path, "a", stats="remind"), make_run(tmp_path, "b")], newest_first=False
    )
    frame = to_dataframe(rows)
    assert list(frame.index) == ["a", "b"]
    assert list(frame.columns) == [
        "jobInSLURM",
        "RunType",
        "modelstat",
        "runInAppResults",
        "Mif",
        "Iter",
        "RunStatus",
        "Warnings",
        "Conv",
        "Runtime",
        "summationErrors",
        "rangeErrors",
        "fixErrors",
        "missingProjVars",
        "projSummationErrors",
        "projSummationErrorsRegional",
    ]
    assert frame.loc["a", "Runtime"] == 90030
    assert frame.loc["b", "modelstat"] == "NA"
    assert to_dataframe([]).empty


# --- quirks and fixes recorded in PORTING-BUGS.md -----------------------------------------------


def test_sparse_modelstat_vector_restores_zeros(
    tmp_path: Path, off_cluster: FakeShell, clock: object
) -> None:
    run = make_run(tmp_path, "r")
    write_gdx(
        run / "fulldata.gdx",
        sets={"all_regi": ["CAZ", "CHA", "EUR"]},
        parameters={"p80_modelstat": (["all_regi"], [("EUR", 2)])},
    )
    assert status_of(run).modelstat == "..2"  # BUG-048


def test_single_digit_iteration_max_is_not_recognised(
    tmp_path: Path, off_cluster: FakeShell, clock: object
) -> None:
    # BUG-047: R's pattern needs two characters before the semicolon, so the earlier two-digit line counts
    gms = "cm_iteration_max = 50;\ncm_iteration_max = 5;\n"
    run = make_run(tmp_path, "single", {"full.log": FULL_LOG_OK, "full.gms": gms}, config="remind")
    assert status_of(run).iter == "12/50"
    run = make_run(
        tmp_path, "only", {"full.log": FULL_LOG_OK, "full.gms": "cm_iteration_max = 5;\n"}, config="remind"
    )
    assert status_of(run).iter == "12"


def test_magpie_log_one_level_up(
    tmp_path: Path, shell: FakeShell, in_slurm: dict[str, str], clock: object
) -> None:
    nested = make_run(
        tmp_path / "remind" / "output",
        "C_Y-rem-1",
        {"full.log": "running\n", "log-mag.txt": "Preparing MAgPIE\n"},
    )
    make_run(tmp_path / "remind" / "magpie" / "output", "C_Y-mag-1", {"full.log": "*** LOOPS = y2100\n"})
    in_slurm["C_Y-rem-1"] = "standby"
    assert status_of(nested).run_status == "Run MAgPIE y2100"


def test_conv_without_iteration_number_and_with_na(
    tmp_path: Path, off_cluster: FakeShell, clock: object
) -> None:
    run = make_run(tmp_path, "r", {"full.log": FULL_LOG_OK}, config="remind")
    write_gdx(run / "fulldata.gdx", scalars={"o_modelstat": 2, "s80_bool": 1})
    assert status_of(run).conv == "converged"  # BUG-023: no iteration number needed
    write_gdx(run / "fulldata.gdx", scalars={"o_modelstat": 2, "s80_bool": 0})
    assert status_of(run).conv == ""  # s80_bool 0 without an iteration number: on to p80_repy (missing)
    write_gdx(run / "fulldata.gdx", scalars={"o_modelstat": 2, "s80_bool": GDX_NA})
    assert status_of(run).conv == "NA"  # an NA s80_bool stopped R
    cfg = "model_name: REMIND\ngms:\n  optimization: nash\n  cm_iteration_max: 5\n"
    (run / "config.yml").write_text(cfg)
    (run / "config.Rdata").unlink()
    nash_gdx(run / "fulldata.gdx", 0, iteration=GDX_NA, modelstats=[2, 2, 6])
    assert status_of(run).conv == "226"  # BUG-023: an NA iteration with a known maximum stopped R
    write_gdx(
        run / "fulldata.gdx",
        scalars={"o_modelstat": 2, "s80_bool": 0, "o_iterationNumber": 3},
        sets={"all_regi": ["CAZ", "EUR"], "solveinfo80": ["modelstat"]},
        parameters={
            "p80_repy": (
                ["all_regi", "solveinfo80"],
                [("CAZ", "modelstat", GDX_NA), ("EUR", "modelstat", 2)],
            )
        },
    )
    assert status_of(run).conv == "NA2"


def test_conv_with_iteration_max_from_full_gms(tmp_path: Path, off_cluster: FakeShell, clock: object) -> None:
    cfg = "model_name: REMIND\ngms:\n  optimization: nash\n  cm_nash_autoconverge: 1\n"
    run = make_run(
        tmp_path, "gms", {"full.log": FULL_LOG_OK, "full.gms": "cm_iteration_max = 50;\n", "config.yml": cfg}
    )
    nash_gdx(run / "fulldata.gdx", 0, iteration=50, modelstats=[2, 2, 6])
    assert status_of(run).conv == "not_converged"
    (run / "full.gms").write_text("cm_iteration_max = 2*25;\n")
    assert status_of(run).conv == "226"  # BUG-017: a maximum that is no number


def test_calibration_chunks_over_all_lines(tmp_path: Path, off_cluster: FakeShell, clock: object) -> None:
    cfg = "model_name: REMIND\ntitle: c\ngms:\n  optimization: nash\n  CES_parameters: calibrate\n"
    log = "CES calibration iteration 4\nCES calibration iteration finished\n"
    run = make_run(tmp_path, "c", {"full.log": FULL_LOG_OK, "config.yml": cfg, "log.txt": log})
    assert status_of(run).iter == "12 Clb: 4"
    (run / "log.txt").write_text("CES calibration iteration started\n")
    assert status_of(run).iter == "12"


def test_equal_mtimes_keep_the_given_order(tmp_path: Path, off_cluster: FakeShell, clock: object) -> None:
    runs = [make_run(tmp_path, name) for name in ("b", "a", "c")]
    for run in runs:
        os.utime(run, (3_000_000, 3_000_000))
    assert [row.name for row in get_run_status(runs)] == ["b", "a", "c"]


def test_latest_gdx_ignores_an_na_iteration(tmp_path: Path, off_cluster: FakeShell, clock: object) -> None:
    run = make_run(tmp_path, "r", stats="remind")
    write_gdx(run / "fulldata.gdx", scalars={"o_iterationNumber": GDX_NA, "o_modelstat": 4})
    write_gdx(run / "non_optimal.gdx", scalars={"o_iterationNumber": 3, "o_modelstat": 7})
    os.utime(run / "non_optimal.gdx", (3_000_000, 3_000_000))  # older, but which.max() skips NA
    assert status_of(run).modelstat == "7: Intermed Nonoptimal"
    write_gdx(run / "non_optimal.gdx", scalars={"o_iterationNumber": GDX_NA, "o_modelstat": 7})
    assert (
        status_of(run).modelstat == "2: Locally Optimal"
    )  # all NA: no latest gdx, runstatistics.rda decides


def test_stats_config_without_model_name(
    tmp_path: Path, off_cluster: FakeShell, clock: object, data_dir: Path
) -> None:
    # BUG-056: R stopped on `if (NULL == "MAgPIE")`; the REMIND rules apply
    run = make_run(
        tmp_path, "r", {"runstatistics.rda": (data_dir / "runstatistics_nomodel.rda").read_bytes()}
    )
    assert status_of(run).modelstat == "2: Locally Optimal"


def test_magpie_recursion_is_one_level_deep(
    tmp_path: Path, shell: FakeShell, in_slurm: dict[str, str], clock: object
) -> None:
    # BUG-025: a MAgPIE folder whose own log.txt ends in "Starting MAgPIE..." made R recurse until its stack overflowed
    cfg = "model_name: REMIND\ngms:\n  optimization: nash\npath_magpie: {where}\ncfg_mag:\n  results_folder: output/C_X-mag-3\n"
    run = make_run(
        tmp_path / "output",
        "C_X-mag-3",
        {"full.log": "*** LOOPS = y2050\n", "log.txt": "Starting MAgPIE...\n"},
    )
    (run / "config.yml").write_text(cfg.format(where=tmp_path))
    in_slurm["C_X-mag-3"] = "standby"
    assert status_of(run).run_status == "mag-3 y2050"


def test_latest_gdx_equal_iterations_take_the_first(
    tmp_path: Path, off_cluster: FakeShell, clock: object
) -> None:
    run = make_run(tmp_path, "r")
    write_gdx(run / "fulldata.gdx", scalars={"o_iterationNumber": 7, "o_modelstat": 2})
    write_gdx(run / "non_optimal.gdx", scalars={"o_iterationNumber": 7, "o_modelstat": 4})  # newer
    os.utime(run / "fulldata.gdx", (3_000_000, 3_000_000))
    assert status_of(run).modelstat == "2: Locally Optimal"  # which.max() takes the first of equals


def test_conv_unreadable_p80_repy(
    tmp_path: Path, off_cluster: FakeShell, clock: object, monkeypatch: pytest.MonkeyPatch
) -> None:
    run = make_run(tmp_path, "r", {"full.log": FULL_LOG_OK}, config="remind")
    nash_gdx(run / "fulldata.gdx", 0, iteration=20, modelstats=[2, 7, 2])
    assert status_of(run).conv == "272"
    monkeypatch.setattr(
        run_status, "read_records", lambda path, name: (_ for _ in ()).throw(GdxReadError("x"))
    )
    assert status_of(run).conv == "NA"  # R's try() failed after s80_bool and the iteration were read
