from __future__ import annotations

from pathlib import Path

import pytest

from modelstats.found_in_slurm import SQUEUE_FORMAT, SqueueJob, found_in_slurm
from tests.conftest import FakeShell


def line(
    user: str, workdir: str, name: str, time: str = "1:23:45", state: str = "RUNNING", qos: str = "standby"
) -> str:
    return f"{user} {workdir} {name} {time} {state} {qos}"


def test_squeue_job_fields() -> None:
    job = SqueueJob.parse(line("bob", "/p/out/run", "run", "0:45", "PENDING", "priority"))
    assert (job.user, job.time, job.qos) == ("bob", "0:45", "priority")
    assert job.pending and job.startup
    assert SqueueJob.parse(line("bob", "/p/out/run", "run", "5:59")).startup
    assert not SqueueJob.parse(line("bob", "/p/out/run", "run", "6:00")).startup
    assert not SqueueJob.parse(line("bob", "/p/out/run", "run", "12:00:00")).startup
    # a job name with a space
    assert SqueueJob.parse(line("bob", "/p/out/run", "my run", "0:10")).time == "0:10"
    short = SqueueJob.parse("bob")  # R stopped on such a line
    assert (short.user, short.time, short.qos) == ("bob", "", "bob") and not short.startup
    assert SqueueJob.parse("") == SqueueJob(line="", user="", time="", qos="")


def test_one_trailing_empty_field_is_dropped_like_r() -> None:
    # R's strsplit drops exactly one trailing empty string, so the fields shift by one
    job = SqueueJob.parse("alice /p/out/run run 1:00 RUNNING ")
    assert (job.time, job.qos) == ("run", "RUNNING")
    job = SqueueJob.parse("alice /p/out/run run 1:00 RUNNING  ")
    assert (job.time, job.qos) == ("1:00", "")


def test_not_found(tmp_path: Path, shell: FakeShell) -> None:
    run = tmp_path / "output" / "SSP2-NPi"
    run.mkdir(parents=True)
    shell.squeue_lines = [line("alice", str(tmp_path / "output" / "other"), "other")]
    assert found_in_slurm(run) == "no"
    assert shell.argvs == [["squeue", "-h", "-o", SQUEUE_FORMAT]]
    shell.squeue_lines = []
    assert found_in_slurm(run) == "no"


def test_own_run_states(tmp_path: Path, shell: FakeShell) -> None:
    run = tmp_path / "output" / "SSP2-NPi"
    run.mkdir(parents=True)
    shell.squeue_lines = [line("alice", str(run), "SSP2-NPi")]
    assert found_in_slurm(run, "alice") == "standby"
    shell.squeue_lines = [line("alice", str(run), "SSP2-NPi", "0:00", "PENDING", "priority")]
    assert found_in_slurm(run, "alice") == "priority pending"
    shell.squeue_lines = [line("alice", str(run), "SSP2-NPi", "0:45", "RUNNING", "short")]
    assert found_in_slurm(run, "alice") == "short startup"
    shell.squeue_lines = [line("alice", str(run), "SSP2-NPi", "0:45", "PENDING", "short")]
    assert found_in_slurm(run, "alice") == "short pending"


def test_pending_qos_with_digits_is_not_recognised(tmp_path: Path, shell: FakeShell) -> None:
    # R quirk kept (BUG-028): "PENDING [A-Za-z]*$" does not match a qos such as gpu_short or qos1
    run = tmp_path / "SSP2-NPi"
    run.mkdir()
    shell.squeue_lines = [line("alice", str(run), "SSP2-NPi", "0:00", "PENDING", "qos1")]
    assert found_in_slurm(run, "alice") == "qos1 startup"


def test_empty_qos_shifts_the_fields(tmp_path: Path, shell: FakeShell) -> None:
    # R quirk kept (BUG-030): an empty qos makes the state the last field
    run = tmp_path / "SSP2-NPi"
    run.mkdir()
    shell.squeue_lines = [line("alice", str(run), "SSP2-NPi", "1:00", "RUNNING", "")]
    assert found_in_slurm(run, "alice") == "RUNNING"


def test_default_user_is_the_current_user(tmp_path: Path, shell: FakeShell) -> None:
    run = tmp_path / "SSP2-NPi"
    run.mkdir()
    shell.squeue_lines = [line("alice", str(run), "SSP2-NPi")]
    assert found_in_slurm(run) == "standby"
    shell.user = "bob"
    assert found_in_slurm(run) == "alice"


def test_other_users_run(tmp_path: Path, shell: FakeShell) -> None:
    run = tmp_path / "SSP2-NPi"
    run.mkdir()
    shell.squeue_lines = [line("bob", str(run), "SSP2-NPi", "0:10")]
    assert found_in_slurm(run, "alice") == "bob startup"
    shell.squeue_lines = [line("bob", str(run), "SSP2-NPi", "0:00", "PENDING")]
    assert found_in_slurm(run, "alice") == "bob pending"
    shell.squeue_lines = [line("bob", str(run), "SSP2-NPi"), line("bob", str(run), "SSP2-NPi")]
    assert found_in_slurm(run, "alice") == "bob"
    shell.squeue_lines = [line("bob", str(run), "SSP2-NPi"), line("carol", str(run), "SSP2-NPi")]
    assert found_in_slurm(run, "alice") == "2 users"


def test_several_own_jobs_give_the_user_name(tmp_path: Path, shell: FakeShell) -> None:
    run = tmp_path / "SSP2-NPi"
    run.mkdir()
    shell.squeue_lines = [line("alice", str(run), "SSP2-NPi"), line("alice", str(run), "SSP2-NPi", "0:01")]
    assert found_in_slurm(run, "alice") == "alice"


def test_n_users_counts_jobs(tmp_path: Path, shell: FakeShell) -> None:
    # R quirk kept (BUG-029): three jobs of two users are "3 users"
    run = tmp_path / "SSP2-NPi"
    run.mkdir()
    shell.squeue_lines = [
        line("bob", str(run), "SSP2-NPi"),
        line("bob", str(run), "SSP2-NPi"),
        line("carol", str(run), "SSP2-NPi"),
    ]
    assert found_in_slurm(run, "alice") == "3 users"


def test_run_name_must_be_followed_by_a_space(tmp_path: Path, shell: FakeShell) -> None:
    main = tmp_path / "coupled"
    run = main / "output" / "C_SSP2-rem-3"
    run.mkdir(parents=True)
    shell.squeue_lines = [line("alice", str(main), "C_SSP2-rem-30")]
    assert found_in_slurm(run, "alice") == "no"
    shell.squeue_lines = [line("alice", str(main), "C_SSP2-rem-3"), line("bob", str(main), "C_SSP3-rem-3")]
    assert found_in_slurm(run, "alice") == "standby"


def test_workdir_prefix_matches(tmp_path: Path, shell: FakeShell) -> None:
    # R quirk kept (BUG-027): the folder test is a substring test, /coupled2 matches /coupled
    main = tmp_path / "coupled"
    coupled = main / "output" / "C_SSP2-rem-3"
    coupled.mkdir(parents=True)
    shell.squeue_lines = [line("bob", str(tmp_path / "coupled2"), "C_SSP2-rem-3")]
    assert found_in_slurm(coupled, "alice") == "bob"


def test_coupled_run_is_searched_in_the_grandparent(tmp_path: Path, shell: FakeShell) -> None:
    main = tmp_path / "coupled"
    run = main / "output" / "C_SSP2-rem-3"
    run.mkdir(parents=True)
    shell.squeue_lines = [line("alice", str(main), "C_SSP2-rem-3", "2:00:00", "RUNNING", "priority")]
    assert found_in_slurm(run, "alice") == "priority"


def test_remind_iteration_has_no_fallback(tmp_path: Path, shell: FakeShell) -> None:
    run = tmp_path / "coupled" / "output" / "C_SSP2-rem-3"
    run.mkdir(parents=True)
    shell.squeue_lines = [line("alice", str(tmp_path), "C_SSP2-rem-3")]
    assert found_in_slurm(run, "alice") == "no"


def test_magpie_iteration_falls_back_to_the_remind_job(tmp_path: Path, shell: FakeShell) -> None:
    main = tmp_path / "coupled"
    run = main / "magpie" / "output" / "C_SSP2-mag-3"
    run.mkdir(parents=True)
    shell.squeue_lines = [line("alice", str(main), "C_SSP2-rem-3")]
    assert found_in_slurm(run, "alice") == "standby"
    shell.squeue_lines = [line("alice", str(tmp_path / "elsewhere"), "C_SSP2-rem-3")]
    assert found_in_slurm(run, "alice") == "no"
    # a direct hit on the MAgPIE job (work dir: the magpie folder) wins over the fallback
    shell.squeue_lines = [
        line("bob", str(main / "magpie"), "C_SSP2-mag-3", "0:10"),
        line("alice", str(main), "C_SSP2-rem-3"),
    ]
    assert found_in_slurm(run, "alice") == "bob startup"


def test_relative_and_symlinked_run_dir(
    tmp_path: Path, shell: FakeShell, monkeypatch: pytest.MonkeyPatch
) -> None:
    run = tmp_path / "output" / "SSP2-NPi"
    run.mkdir(parents=True)
    link = tmp_path / "link"
    link.symlink_to(tmp_path / "output")
    shell.squeue_lines = [line("alice", str(run), "SSP2-NPi")]
    assert found_in_slurm(link / "SSP2-NPi", "alice") == "standby"
    monkeypatch.chdir(tmp_path)
    assert found_in_slurm(Path("output") / "SSP2-NPi", "alice") == "standby"


def test_user_list_never_matches_as_own_run(tmp_path: Path, shell: FakeShell) -> None:
    run = tmp_path / "SSP2-NPi"
    run.mkdir()
    shell.squeue_lines = [line("alice", str(run), "SSP2-NPi")]
    assert found_in_slurm(run, "alice,bob") == "alice"
