"""``modelstats.run``: the public ``Run`` object on synthetic run folders.

Three groups (``migration/06-user-requests.md`` 5.2-5.3, decision D-25): the table from every R ``RunStatus``
string of ``03`` 3.1 to ``RunState``; one test per deliberate difference listed in the module docstring of
``run.py`` ("R says / Run says"), each asserting the parity record and the clean view side by side on one
synthetic case; and the object contract (lazy, cached, nothing read twice, errors as ``RunReadError``,
the snapshot, ``is_run_folder``, ``iter_runs``). The folders come from the builders of
``tests/unit/test_facts.py`` and ``tests/unit/test_run_status.py``.
"""

from __future__ import annotations

import json
from collections import Counter
from datetime import timedelta
from pathlib import Path

import pytest

from modelstats.env import FakeEffects
from modelstats.errors import ModelstatsError, RunReadError
from modelstats.facts import ARGUMENT_OF_LENGTH_ZERO, INVALID_DESCRIPTION, MISSING_VALUE
from modelstats.facts import RunReadError as FactsReadError
from modelstats.rdata_io import write_rds
from modelstats.run import (
    Convergence,
    Iterations,
    MifState,
    ModelStat,
    ProjectSummation,
    Run,
    RunState,
    iter_runs,
    run_state,
    state_of_detail,
)
from modelstats.run_status import INCORRECT_DIMENSIONS, REPLACEMENT_LENGTH_ZERO, RunStatus, get_run_status
from modelstats.slurm import SlurmJob
from tests.unit.test_facts import (
    ME,
    NOW_EPOCH,
    effects,
    library_reads,
    make_run,
    remind_cfg,
    remind_stats,
    write_config,
    write_gdx,
    write_stats,
)
from tests.unit.test_run_status import (
    LOOPS,
    STATUS_ERROR,
    STATUS_OK,
    WARNINGS_27,
    magpie_run,
    remind_run,
    running_job,
)

CONVERGED = {"o_iterationNumber": 26, "o_modelstat": 2, "s80_bool": 1}
HOUR = 3600.0


def record_of(run: Run, detailed: bool = True) -> RunStatus:
    return run.to_status_record(detailed)


def parity_error(run: Run, message: str, detailed: bool = True) -> None:
    """The parity record aborts with R's message; the clean view is read in the test instead."""
    with pytest.raises(RunReadError, match=message):
        run.to_status_record(detailed)


def pending_job(run: Path, qos: str = "standby", user: str = ME) -> dict[str, str]:
    return {"squeue_all.txt": f"{user} {run} {run.name} 0:00 PENDING {qos}\n"}


# ---------------------------------------------------------------------------------------------
# The table: every R RunStatus string of 03 section 3.1 (and the parameterised forms)
# ---------------------------------------------------------------------------------------------

STATE_TABLE = [
    ("Normal completion", RunState.COMPLETED),
    ("Execution error", RunState.ERROR),
    ("Compilation error", RunState.ERROR),
    ("Terminated by use", RunState.INTERRUPTED),
    ("Terminated due to", RunState.ERROR),
    ("NA", RunState.UNKNOWN),
    ("Timeout interrupt", RunState.INTERRUPTED),
    ("Memory interrupt", RunState.INTERRUPTED),
    ("Preempt interrupt", RunState.INTERRUPTED),
    ("Run requeued", RunState.INTERRUPTED),
    ("Run cancelled", RunState.INTERRUPTED),
    ("Run interrupted", RunState.INTERRUPTED),
    ("Run restarted", RunState.PENDING),
    ("Run in progress", RunState.RUNNING),
    ("conoptspy >2h", RunState.RUNNING),
    ("conoptspy >0.4h", RunState.RUNNING),
    ("mag-3 y2050", RunState.RUNNING),
    ("mag-output/:title::date: NA", RunState.RUNNING),
    ("Running reporting", RunState.RUNNING),
    ("Abort EUR 2*Infes", RunState.ERROR),
    ("Abort 2R*2*Infes", RunState.ERROR),
    ("Run MAgPIE y2050", RunState.RUNNING),
    ("Run MAgPIE report", RunState.RUNNING),
    ("Run MAgPIE ", RunState.RUNNING),
    ("Wait MAgPIE lock", RunState.RUNNING),
    ("Wait REMIND lock", RunState.RUNNING),
    ("full.log missing", RunState.UNKNOWN),
]


@pytest.mark.parametrize(("detail", "state"), STATE_TABLE)
def test_state_of_every_r_run_status_string(detail: str, state: RunState) -> None:
    assert state_of_detail(detail) is state
    assert run_state(detail) is state  # off the cluster: no jobs, the text decides


def test_the_table_covers_every_fixed_string_of_the_plan() -> None:
    """``03`` 3.1 lists these fixed values; a new one in ``run.py`` must be added to the table above."""
    from modelstats.run import _STATE_BY_DETAIL  # noqa: PLC0415 - the table under test

    assert set(_STATE_BY_DETAIL) <= {detail for detail, _ in STATE_TABLE}


def test_unknown_gams_status_lines_are_errors() -> None:
    assert state_of_detail("System error") is RunState.ERROR
    assert state_of_detail("File error") is RunState.ERROR


def test_the_queue_refines_the_text() -> None:
    pending = SlurmJob(ME, "/d", "run", "0:00", "PENDING", "standby", pending=True, startup=False)
    running = SlurmJob(ME, "/d", "run", "1:23:45", "RUNNING", "standby", pending=False, startup=False)
    assert run_state("Normal completion", [pending]) is RunState.PENDING
    assert run_state("Run in progress", [pending, pending]) is RunState.PENDING
    assert run_state("Run in progress", [pending, running]) is RunState.RUNNING
    assert run_state("full.log missing", [running]) is RunState.RUNNING
    assert run_state("NA", [running]) is RunState.RUNNING
    assert run_state("Normal completion", [running]) is RunState.COMPLETED
    assert run_state("Execution error", [running]) is RunState.ERROR
    assert run_state("full.log missing", []) is RunState.UNKNOWN


# ---------------------------------------------------------------------------------------------
# The differences of the module docstring (D-25), parity record and clean view side by side
# ---------------------------------------------------------------------------------------------


def test_difference_last_status_line_wins(tmp_path: Path) -> None:
    """BUG-036: R's cell assignment fails on two lines; Run takes the last one."""
    run_dir = remind_run(tmp_path, full_log=LOOPS + STATUS_ERROR + STATUS_OK)
    run = Run(run_dir, effects(tmp_path))
    assert record_of(run).RunStatus == "Run interrupted"  # "NA" then the off-cluster fallback
    assert run.status_detail == "Normal completion" and run.status is RunState.COMPLETED
    on_cluster = Run(run_dir, effects(tmp_path, on_cluster=True))
    assert record_of(on_cluster).RunStatus == "NA"
    assert on_cluster.status_detail == "Normal completion" and on_cluster.status is RunState.COMPLETED
    # the overrides still apply to the last line (R/getRunStatus.R:210-220)
    aborted = remind_run(tmp_path, "aborted", full_log=STATUS_OK + STATUS_ERROR)
    write_gdx(aborted / "abort.gdx", scalars={"cm_abortOnConsecFail": 2}, track={"EUR": {3: 2}})
    run = Run(aborted, effects(tmp_path))
    assert run.status_detail == "Abort EUR 2*Infes" and run.status is RunState.ERROR
    assert record_of(run).RunStatus == "Run interrupted"


def test_difference_modelstat_is_structured(tmp_path: Path) -> None:
    run = Run(remind_run(tmp_path, "a", gdx=CONVERGED), effects(tmp_path))
    assert record_of(run).modelstat == "2: Locally Optimal"
    assert run.modelstat == ModelStat(2, "Locally Optimal", "2")
    vector = Run(remind_run(tmp_path, "b", stats=remind_stats(modelstat=(2.0, 2.0))), effects(tmp_path))
    assert record_of(vector).modelstat == "NA"
    assert vector.modelstat == ModelStat(None, None, None)
    mag = magpie_run(tmp_path, files={"full.log": STATUS_OK})
    assert record_of(Run(mag, effects(tmp_path))).modelstat == "222"
    assert Run(mag, effects(tmp_path)).modelstat == ModelStat(None, None, "222")
    write_gdx(mag / "fulldata.gdx", magpie_modelstat=(2, 2, 10))
    assert Run(mag, effects(tmp_path)).modelstat == ModelStat(None, None, "221.")


def test_difference_modelstat_fallback_without_a_model_name(tmp_path: Path) -> None:
    """``R/getRunStatus.R:91`` aborts on ``stats$config`` without ``model_name`` (the BUG-005 input)."""
    run = Run(remind_run(tmp_path, stats=remind_stats(model_name=None)), effects(tmp_path))
    parity_error(run, ARGUMENT_OF_LENGTH_ZERO)
    assert run.modelstat == ModelStat(2, "Locally Optimal", "2")
    assert run.model == "REMIND" and run.status is RunState.COMPLETED  # cfg$model_name as the fallback


def test_difference_run_type_of_a_magpie_config_without_optimization(tmp_path: Path) -> None:
    """BUG-046 / D-28: ``colRunType()`` returns ``NULL`` and R fails the cell assignment."""
    run_dir = make_run(tmp_path, "run", {"config.yml": "title: t\nmodel_name: MAgPIE\ngms:\n  foo: 1\n"})
    run = Run(run_dir, effects(tmp_path))
    parity_error(run, REPLACEMENT_LENGTH_ZERO)
    assert run.run_type is None and run.title == "t" and run.model == "MAgPIE"
    assert run.status is RunState.UNKNOWN and run.status_detail == "full.log missing"


def test_difference_runtime_is_a_timedelta_and_live_only_with_a_job(tmp_path: Path) -> None:
    finished = Run(remind_run(tmp_path, "done"), effects(tmp_path))
    assert record_of(finished).Runtime == 8304  # round(8304.5, 0)
    assert finished.runtime == timedelta(seconds=8304.5)
    unfinished = remind_run(tmp_path, "live", full_log=LOOPS, stats=remind_stats(gams_end=None))
    off_cluster = Run(unfinished, effects(tmp_path))
    assert record_of(off_cluster).Runtime == round(NOW_EPOCH - 500)  # live off the cluster ("NA" != "no")
    assert off_cluster.runtime is None
    no_job = Run(unfinished, effects(tmp_path, on_cluster=True))
    assert record_of(no_job).Runtime is None and no_job.runtime is None
    with_job = Run(unfinished, effects(tmp_path, on_cluster=True, slurm=running_job(unfinished)))
    assert record_of(with_job).Runtime == round(NOW_EPOCH - 500)
    assert with_job.runtime == timedelta(seconds=NOW_EPOCH - 500)
    pending = Run(unfinished, effects(tmp_path, on_cluster=True, slurm=pending_job(unfinished)))
    assert pending.runtime == timedelta(seconds=NOW_EPOCH - 500)  # listed, so live, as in R
    assert Run(remind_run(tmp_path, "nostats", stats={}), effects(tmp_path)).runtime is None


def test_difference_convergence_is_an_enum(tmp_path: Path) -> None:
    converged = remind_run(tmp_path, "a", gdx=CONVERGED)
    assert record_of(Run(converged, effects(tmp_path))).Conv == "converged"
    assert Run(converged, effects(tmp_path)).convergence is Convergence.CONVERGED
    write_gdx(converged / "non_optimal.gdx", scalars={"o_iterationNumber": 3, "o_modelstat": 5})
    run = Run(converged, effects(tmp_path))
    assert run.convergence is Convergence.CONVERGED_HAD_INFES and run.feasibility is run.convergence
    assert run.convergence.is_converged
    at_max = remind_run(tmp_path, "b", gdx={"o_iterationNumber": 50, "o_modelstat": 2, "s80_bool": 0})
    assert Run(at_max, effects(tmp_path)).convergence is Convergence.NOT_CONVERGED
    assert not Run(at_max, effects(tmp_path)).convergence.is_converged
    below = remind_run(
        tmp_path, "c", gdx={"o_iterationNumber": 12, "o_modelstat": 2, "s80_bool": 0}, repy={"EUR": 2}
    )
    run = Run(below, effects(tmp_path))
    assert record_of(run).Conv == "002000000000"  # R: the p80_repy digits below the maximum
    assert run.convergence is Convergence.NOT_CONVERGED
    assert Run(remind_run(tmp_path, "d"), effects(tmp_path)).convergence is Convergence.UNKNOWN  # no GDX
    one_region = remind_run(tmp_path, "e", cfg=remind_cfg(optimization="testOneRegi"), gdx=CONVERGED)
    assert Run(one_region, effects(tmp_path)).convergence is Convergence.UNKNOWN


def test_difference_convergence_degrades_where_r_aborts(tmp_path: Path) -> None:
    """``R/getRunStatus.R:281-290``: missing ``s80_bool``, ``o_iterationNumber`` (BUG-038), ``p80_repy``."""
    no_flag = Run(
        remind_run(tmp_path, "a", gdx={"o_iterationNumber": 26, "o_modelstat": 2}), effects(tmp_path)
    )
    parity_error(no_flag, ARGUMENT_OF_LENGTH_ZERO)
    assert no_flag.convergence is Convergence.UNKNOWN
    no_iteration = Run(remind_run(tmp_path, "b", gdx={"o_modelstat": 2, "s80_bool": 0}), effects(tmp_path))
    parity_error(no_iteration, MISSING_VALUE)
    assert no_iteration.convergence is Convergence.NOT_CONVERGED
    no_report = remind_run(tmp_path, "c", gdx={"o_iterationNumber": 2, "o_modelstat": 2, "s80_bool": 0})
    parity_error(Run(no_report, effects(tmp_path)), INCORRECT_DIMENSIONS)
    assert Run(no_report, effects(tmp_path)).convergence is Convergence.NOT_CONVERGED
    assert Run(no_report, effects(tmp_path)).status is RunState.COMPLETED  # the rest of the view is intact


def test_calibration_convergence_keeps_the_above_ten_rule(tmp_path: Path) -> None:
    """BUG-001 / D-09 is a rule-level defect and stays (``06`` 4.2)."""
    calib = remind_run(
        tmp_path, cfg=remind_cfg(ces="calibrate"), gdx=CONVERGED, log_txt="CES calibration iteration 7\n"
    )
    for index in range(10):
        (calib / f"fulldata_{index}.gdx").write_bytes(b"")
    run = Run(calib, effects(tmp_path))
    assert run.run_type == "Calib_nash" and record_of(run).Conv == "converged"
    assert run.convergence is Convergence.CONVERGED
    assert run.iterations == Iterations(26, 50, 7, "26")
    (calib / "fulldata_10.gdx").write_bytes(b"")
    run = Run(calib, effects(tmp_path))
    assert record_of(run).Conv == "Clb_converged" and record_of(run).Iter == "26/50 Clb: 7"
    assert run.convergence is Convergence.CALIBRATION_CONVERGED and run.convergence.is_converged


def test_difference_pending_job_with_a_digit_qos(tmp_path: Path) -> None:
    """BUG-031: ``PENDING qos1`` does not match ``PENDING [A-Za-z]*$``, so R says startup and in progress."""
    run_dir = remind_run(tmp_path, full_log=LOOPS)
    env = effects(tmp_path, on_cluster=True, slurm=pending_job(run_dir, qos="qos1"))
    run = Run(run_dir, env)
    record = record_of(run)
    assert record.jobInSLURM == "qos1 startup" and record.RunStatus == "Run in progress"
    assert run.slurm is not None and run.slurm.pending and not run.slurm.startup
    assert run.status_detail == "Run in progress" and run.status is RunState.PENDING
    letters = Run(run_dir, effects(tmp_path, on_cluster=True, slurm=pending_job(run_dir)))
    assert record_of(letters).jobInSLURM == "standby pending" and record_of(letters).RunStatus == "NA"
    assert letters.status is RunState.PENDING and letters.status_detail == "NA"


def test_difference_several_hits_have_no_single_job(tmp_path: Path) -> None:
    """BUG-019 / BUG-035: R names the user or counts jobs; Run exposes every hit through the facts."""
    run_dir = remind_run(tmp_path, full_log=LOOPS)
    line = f"{ME} {run_dir} {run_dir.name} 0:00 PENDING standby\n"
    env = effects(tmp_path, on_cluster=True, slurm={"squeue_all.txt": line + line})
    run = Run(run_dir, env)
    assert record_of(run).jobInSLURM == ME and record_of(run).RunStatus == "Run in progress"
    assert run.slurm is None and run.facts.slurm is not None and len(run.facts.slurm.jobs) == 2
    assert run.status is RunState.PENDING
    other = f"alice {run_dir} {run_dir.name} 1:00:00 RUNNING standby\n"
    env = effects(tmp_path, on_cluster=True, slurm={"squeue_all.txt": line + other})
    run = Run(run_dir, env)
    assert record_of(run).jobInSLURM == "2 users" and run.slurm is None
    assert run.status is RunState.RUNNING


def test_difference_full_log_missing_with_a_live_job_is_running(tmp_path: Path) -> None:
    run_dir = remind_run(tmp_path, full_log=None)
    env = effects(tmp_path, on_cluster=True, slurm=running_job(run_dir))
    run = Run(run_dir, env)
    assert record_of(run).RunStatus == "full.log missing"
    assert run.status_detail == "full.log missing" and run.status is RunState.RUNNING
    idle = Run(run_dir, effects(tmp_path, on_cluster=True))
    assert idle.status is RunState.UNKNOWN


def test_difference_plausibility_counts_and_project_block(tmp_path: Path) -> None:
    errors = "variable,other\nX,1\nY,2\nX,3\n"
    run_dir = remind_run(
        tmp_path,
        extra={"REMIND_generic_t_summation_errors.csv": errors, "log_fixOnRef.csv": "variable\nZ\n"},
    )
    run = Run(run_dir, effects(tmp_path))
    record = record_of(run)
    assert record.sanity is not None and record.sanity.summationErrors == 2 and record.sanity.fixErrors == 1
    assert record.sanity.missingProjVars is None and run.project_summation is None
    assert (run.summation_errors, run.range_errors, run.fix_errors) == (2, 0, 1)
    assert run.mif is MifState.SUMMATION_ERRORS
    write_rds(run_dir / "projectSummations.rds", {"ScenarioMIP": {"missingVars": 2, "checkSummations": 32}})
    run = Run(run_dir, effects(tmp_path))
    parity_error(run, REPLACEMENT_LENGTH_ZERO)  # checkSummationsRegional is NULL: R/getRunStatus.R:357
    assert run.project_summation == ProjectSummation(2, 32, None)
    assert run.summation_errors == 2
    write_rds(run_dir / "projectSummations.rds", {"other": 1})
    run = Run(run_dir, effects(tmp_path))
    parity_error(run, REPLACEMENT_LENGTH_ZERO)
    assert run.project_summation == ProjectSummation(None, None, None)
    complete = {"missingVars": 5, "checkSummations": 36, "checkSummationsRegional": 53}
    write_rds(run_dir / "projectSummations.rds", {"ScenarioMIP": complete})
    run = Run(run_dir, effects(tmp_path))
    assert record_of(run).sanity is not None and run.project_summation == ProjectSummation(5, 36, 53)


def test_plausibility_counts_follow_the_mif_rule(tmp_path: Path) -> None:
    """``R/getRunStatus.R:316-322``: a configuration file, a REMIND run and the mif, else nothing."""
    no_mif = Run(remind_run(tmp_path, "nomif", mif=False), effects(tmp_path))
    assert record_of(no_mif).sanity is None and no_mif.summation_errors is None
    assert no_mif.mif is MifState.MISSING
    mag = Run(magpie_run(tmp_path, files={"full.log": STATUS_OK}), effects(tmp_path))
    assert mag.summation_errors is None and mag.range_errors is None and mag.fix_errors is None
    assert mag.mif is MifState.MISSING
    no_config = Run(make_run(tmp_path, "noconfig", {"full.log": STATUS_OK}), effects(tmp_path))
    assert no_config.summation_errors is None and no_config.mif is MifState.UNKNOWN
    assert record_of(no_config).Mif == "NA"
    assert Run(remind_run(tmp_path, "plain"), effects(tmp_path)).mif is MifState.PRESENT


def test_difference_in_app_results_is_a_boolean(tmp_path: Path) -> None:
    from modelstats.env import ClusterPaths  # noqa: PLC0415 - one test needs the archive location

    archive = tmp_path / "archive"
    archive.mkdir()
    paths = ClusterPaths(remind_results_archive=f"{archive}/")
    run_dir = remind_run(tmp_path, stats=remind_stats(run_id=42.0))
    assert Run(run_dir, effects(tmp_path)).in_app_results is None  # off the cluster
    env = effects(tmp_path, on_cluster=True, cluster_paths=paths)
    assert record_of(Run(run_dir, env)).runInAppResults == "no" and Run(run_dir, env).in_app_results is False
    (archive / "42.rds").write_text("")
    assert record_of(Run(run_dir, env)).runInAppResults == "yes" and Run(run_dir, env).in_app_results is True
    without_id = Run(remind_run(tmp_path, "noid", stats=remind_stats(run_id=None)), env)
    assert without_id.in_app_results is False
    null_name = Run(remind_run(tmp_path, "nullname", stats=remind_stats(run_id=42.0, model_name=None)), env)
    parity_error(null_name, ARGUMENT_OF_LENGTH_ZERO)  # R/getRunStatus.R:108
    assert null_name.in_app_results is True  # the REMIND archive, vacuous overview rule (BUG-010 kept)


def test_difference_iterations_and_warnings_are_numbers(tmp_path: Path) -> None:
    run = Run(remind_run(tmp_path, "a"), effects(tmp_path))
    assert record_of(run).Iter == "26/50" and record_of(run).Warnings == "27"
    assert run.iterations == Iterations(current=26, maximum=50, calibration=None, loop="26")
    assert run.warnings == 27
    no_log = Run(remind_run(tmp_path, "b", full_log=None, log_txt=None), effects(tmp_path))
    assert record_of(no_log).Iter == "NA" and record_of(no_log).Warnings == "NA"
    assert no_log.iterations == Iterations(None, 50, None, None) and no_log.warnings is None
    mag = magpie_run(tmp_path, files={"full.log": "--- LOOPS = y2100\n" + STATUS_OK, "slurm.log": "fine\n"})
    run = Run(mag, effects(tmp_path))
    assert record_of(run).Iter == "y2100" and run.iterations == Iterations(None, None, None, "y2100")
    assert run.warnings == 0
    block = Run(remind_run(tmp_path, "c", log_txt="Warning messages:\n1: a\n2: b\n"), effects(tmp_path))
    assert block.warnings == 2
    singular = Run(remind_run(tmp_path, "d", log_txt="Warning message:\nx\n"), effects(tmp_path))
    assert singular.warnings == 0  # BUG-029 kept: same count as R


def test_difference_a_missing_path_is_an_unknown_run(tmp_path: Path) -> None:
    missing = tmp_path / "does-not-exist"
    env = effects(tmp_path)
    rows = get_run_status([str(missing)], effects=env)
    assert rows[0].run == "NA" and rows[0].path == "NA"  # the data-frame artefact of R/getRunStatus.R:30-32
    run = Run(missing, env)
    assert run.name == "does-not-exist" and run.path == missing
    assert run.status is RunState.UNKNOWN and run.status_detail == "full.log missing"
    assert run.config is None and run.statistics is None and run.title is None and run.model is None
    assert run.run_type is None and run.convergence is Convergence.UNKNOWN
    assert run.modelstat == ModelStat(None, None, None) and run.mif is MifState.UNKNOWN
    assert run.iterations == Iterations(None, None, None, None) and run.warnings is None
    assert run.runtime is None and run.summation_errors is None and run.project_summation is None
    assert run.slurm is None and run.in_app_results is None
    record = record_of(run)
    assert record.run == "does-not-exist" and record.RunStatus == "full.log missing" and record.Mif == "NA"


# ---------------------------------------------------------------------------------------------
# The object contract
# ---------------------------------------------------------------------------------------------


def reads(env: FakeEffects) -> list[tuple[str, ...]]:
    """The file streams opened and the programs run so far."""
    return [call.args for call in env.calls if call.name in ("open_binary", "run")]


def test_construction_reads_nothing_and_the_views_share_one_reading(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    """Lazy, cached, one facts layer: from the first access on, both views read every file once."""
    run_dir = remind_run(tmp_path, gdx=CONVERGED)
    env = effects(tmp_path, on_cluster=True, slurm=running_job(run_dir))
    library = library_reads(monkeypatch)
    run = Run(run_dir, env)
    assert [call.name for call in env.calls] == ["normalize_path"]  # R/getRunStatus.R:25
    assert run.status is RunState.COMPLETED
    assert [args[0] for args in reads(env) if args[0] == "squeue"] == ["squeue"]  # one squeue for the status
    run.to_status_record()
    run.to_dict()
    run.to_status_record(detailed=False)
    counts = Counter([args[0] for args in reads(env)] + library)
    assert set(counts.values()) == {1}, counts  # streams, library reads and squeue: each once
    expected = {str(run_dir / name) for name in ("full.log", "log.txt", "fulldata.gdx", "config.Rdata")}
    assert expected <= set(counts)
    before = reads(env), list(library)
    assert run.status is RunState.COMPLETED and run.to_status_record() == run.to_status_record()
    run.to_dict()
    assert (reads(env), library) == before  # cached: a second pass over both views reads nothing
    assert run.facts.config is run.config and run.facts.files is run.files


def test_to_status_record_equals_get_run_status(tmp_path: Path) -> None:
    run_dir = remind_run(tmp_path, gdx=CONVERGED, log_txt=WARNINGS_27)
    for detailed in (True, False):
        env = effects(tmp_path)
        expected = get_run_status([str(run_dir)], detailed=detailed, effects=env)[0]
        assert Run(run_dir, effects(tmp_path)).to_status_record(detailed) == expected
    env = effects(tmp_path, on_cluster=True, slurm=running_job(run_dir))
    expected = get_run_status([str(run_dir)], effects=env)[0]
    env = effects(tmp_path, on_cluster=True, slurm=running_job(run_dir))
    assert Run(run_dir, env).to_status_record() == expected
    assert expected.jobInSLURM == "standby"


def test_user_is_the_slurm_user(tmp_path: Path) -> None:
    run_dir = remind_run(tmp_path)
    env = effects(tmp_path, on_cluster=True, slurm=running_job(run_dir, user="alice"))
    assert record_of(Run(run_dir, env)).jobInSLURM == "alice"
    env = effects(tmp_path, on_cluster=True, slurm=running_job(run_dir, user="alice"))
    run = Run(run_dir, env, user="alice")
    assert record_of(run).jobInSLURM == "standby" and run.slurm is not None and run.slurm.user == "alice"


def test_errors_are_run_read_errors_of_the_package(tmp_path: Path) -> None:
    run_dir = remind_run(tmp_path, extra={"config.Rdata.bak": ""})  # BUG-003: two matches
    run = Run(run_dir, effects(tmp_path))
    with pytest.raises(RunReadError, match=INVALID_DESCRIPTION) as excinfo:
        _ = run.config
    assert isinstance(excinfo.value, ModelstatsError)
    assert isinstance(excinfo.value.__cause__, FactsReadError)
    with pytest.raises(RunReadError, match=INVALID_DESCRIPTION):
        _ = run.title
    with pytest.raises(RunReadError, match=INVALID_DESCRIPTION):
        run.to_status_record()
    broken = Run(make_run(tmp_path, "broken", {"fulldata.gdx": "not a gdx"}), effects(tmp_path))
    with pytest.raises(RunReadError, match="fulldata.gdx"):  # BUG-037 / D-20: the port raises
        _ = broken.modelstat
    assert broken.status is RunState.UNKNOWN  # the properties that do not need the GDX still answer


def test_slurm_errors_are_run_read_errors_of_the_package(tmp_path: Path) -> None:
    """The deferred ``foundInSlurm()`` work (``R/foundInSlurm.R:31-39``) raises the package's error."""
    run_dir = remind_run(tmp_path)
    env = effects(tmp_path, on_cluster=True, slurm=running_job(run_dir), user="[")
    message = r"invalid regular expression '\^\[ ', reason 'Missing '\]''"  # BUG-008: the user is a regex
    with pytest.raises(RunReadError, match=message):
        get_run_status([str(run_dir)], user="[", effects=env)
    run = Run(run_dir, env, user="[")
    with pytest.raises(RunReadError, match=message) as excinfo:
        _ = run.status
    assert isinstance(excinfo.value, ModelstatsError)
    with pytest.raises(RunReadError, match=message):
        run.to_status_record()
    assert run.slurm is not None  # the typed job does not evaluate the user regex
    five_fields = {
        "01.txt.cmd": "squeue -h -o '%u %Z %j %M %T %q'\n",
        "01.txt": f"{ME} {run_dir} z 1:23 RUNNING\n",  # recorded: bypasses the six-field filter of the fake
    }
    run = Run(run_dir, effects(tmp_path, on_cluster=True, slurm=five_fields))
    with pytest.raises(RunReadError, match="fewer than six fields") as excinfo:
        _ = run.slurm
    assert isinstance(excinfo.value, ModelstatsError)


def test_model_title_and_repr(tmp_path: Path) -> None:
    run = Run(remind_run(tmp_path, "a"), effects(tmp_path))
    assert run.model == "REMIND" and run.title == "t" and run.name == "a"
    assert repr(run) == f"Run({str(tmp_path / 'a')!r})"
    mag = Run(magpie_run(tmp_path, "m"), effects(tmp_path))
    assert mag.model == "MAgPIE" and mag.run_type == "nlp_apr17"
    config_only = make_run(tmp_path, "c")
    write_config(config_only, remind_cfg())
    assert Run(config_only, effects(tmp_path)).model == "REMIND"  # cfg$model_name as the fallback
    stats_only = make_run(tmp_path, "s")
    write_stats(stats_only, remind_stats(model_name="MAgPIE"))
    assert (
        Run(stats_only, effects(tmp_path)).model == "MAgPIE"
        and Run(stats_only, effects(tmp_path)).title is None
    )


def test_to_dict_is_a_json_able_snapshot(tmp_path: Path) -> None:
    run_dir = remind_run(tmp_path, gdx=CONVERGED)
    env = effects(tmp_path, on_cluster=True, slurm=running_job(run_dir))
    snapshot = Run(run_dir, env).to_dict()
    json.dumps(snapshot)
    assert snapshot["status"] == "completed" and snapshot["status_detail"] == "Normal completion"
    assert snapshot["convergence"] == "converged" and snapshot["mif"] == "present"
    assert snapshot["modelstat"] == {"code": 2, "label": "Locally Optimal", "raw": "2"}
    assert snapshot["iterations"] == {"current": 26, "maximum": 50, "calibration": None, "loop": "26"}
    assert snapshot["runtime_seconds"] == 8304.5 and snapshot["warnings"] == 27
    assert snapshot["summation_errors"] == 0 and snapshot["project_summation"] is None
    assert snapshot["in_app_results"] is False
    assert snapshot["slurm"] == {
        "user": ME,
        "workdir": str(run_dir),
        "name": run_dir.name,
        "elapsed": "1:23:45",
        "state": "RUNNING",
        "qos": "standby",
        "pending": False,
        "startup": False,
    }
    assert snapshot["path"] == str(run_dir) and snapshot["name"] == run_dir.name


def test_status_detail_strings_map_to_states_on_synthetic_runs(tmp_path: Path) -> None:
    """A few R strings produced end to end, not only through the table."""
    error = Run(remind_run(tmp_path, "err", full_log=STATUS_ERROR), effects(tmp_path))
    assert error.status is RunState.ERROR and error.status_detail == "Execution error"
    interrupted = Run(remind_run(tmp_path, "int", full_log=LOOPS), effects(tmp_path))
    assert interrupted.status is RunState.INTERRUPTED and interrupted.status_detail == "Run interrupted"
    timeout_log = "slurmstepd: error: *** JOB 1 ON cs CANCELLED AT 2026 DUE TO TIME LIMIT ***\n"
    timeout = remind_run(tmp_path, "timeout", full_log=LOOPS, log_txt=timeout_log)
    run = Run(timeout, effects(tmp_path, on_cluster=True))
    assert run.status is RunState.INTERRUPTED and run.status_detail == "Timeout interrupt"
    running = remind_run(tmp_path, "running", full_log=LOOPS)
    run = Run(running, effects(tmp_path, on_cluster=True, slurm=running_job(running)))
    assert run.status is RunState.RUNNING and run.status_detail == "Run in progress"
    lock = Run(
        remind_run(tmp_path, "lock", full_log=None, log_txt="try to acquire model lock\n"), effects(tmp_path)
    )
    assert lock.status is RunState.RUNNING and lock.status_detail == "Wait REMIND lock"


def test_is_run_folder_and_iter_runs(tmp_path: Path) -> None:
    assert "output" not in str(tmp_path)
    main = tmp_path / "remind"
    for marker in ("output.R", "start.R", "main.gms"):
        (main / marker).parent.mkdir(parents=True, exist_ok=True)
        (main / marker).write_text("")
    markers = {"full.gms": "", "log.txt": "", "prepare_and_run.R": "", "prepareAndRun.R": ""}
    runs = [make_run(main / "output", name, markers) for name in ("r10", "r2", "C_x-rem-1")]
    make_run(main / "output", "archive")  # not a run folder
    (main / "output" / "gRS.rds").write_text("")
    env = effects(tmp_path)
    assert Run.is_run_folder(runs[0], env) and not Run.is_run_folder(main / "output" / "archive", env)
    assert not Run.is_run_folder(main, env)
    names = [run.name for run in iter_runs(main, env)]
    assert names == ["C_x-rem-1", "r2", "r10"]  # natural order (D-14), run folders only
    assert [run.name for run in iter_runs(main / "output", env)] == names
    assert [run.name for run in iter_runs(runs[1], env)] == ["r2"]
    assert list(iter_runs(tmp_path / "nowhere", env)) == []
    found = next(iter_runs(main / "output", env, user="alice"))
    assert found.facts.user == "alice" and found.path == runs[2]
