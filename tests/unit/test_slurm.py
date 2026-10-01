"""``modelstats.slurm`` reproduces ``R/foundInSlurm.R`` and the ``-C`` branch of ``R/commandLineInterface.R``.

The R string helpers are pinned against values probed in R 4.6.1 (``strsplit``, ``dirname``, TRE error
messages); the matching and answer rules run on hand-written squeue lines; the scheduler calls run through
``FakeEffects`` with a ``CannedSlurm`` built from the same files a synthetic case directory holds
(``migration/harness/fakebin/_slurm.py``), on temporary directories, without any fixture.
"""

from __future__ import annotations

from collections.abc import Mapping
from datetime import datetime, timedelta
from pathlib import Path
from zoneinfo import ZoneInfo

import pytest

from modelstats.env import CannedSlurm, FakeEffects
from modelstats.slurm import (
    INVALID_REGEX,
    NO_JOB,
    SUBSCRIPT_OUT_OF_BOUNDS,
    SlurmError,
    SlurmJob,
    current_runs,
    found_in_slurm,
    lookup_slurm,
    match_squeue,
    parse_elapsed,
    parse_squeue,
    r_dirname,
    r_grepl,
    r_strsplit_space,
    slurm_answer,
)

NOW = datetime(2026, 9, 30, 12, 0, 0, tzinfo=ZoneInfo("Europe/Berlin"))
ME = "pascalfu"


def effects(files: Mapping[str, str], cwd: Path, user: str = ME) -> FakeEffects:
    return FakeEffects(now=NOW, user=user, on_cluster=True, slurm=CannedSlurm(files), cwd=cwd)


def lines(*texts: str) -> list[object]:
    return [parse_squeue(text) for text in texts]


def six(
    user: str, workdir: str, name: str, elapsed: str = "2:15:00", state: str = "RUNNING", qos: str = "standby"
) -> str:
    return f"{user} {workdir} {name} {elapsed} {state} {qos}"


# ---------------------------------------------------------------------------------------------
# R string helpers (values probed in R 4.6.1)
# ---------------------------------------------------------------------------------------------


@pytest.mark.parametrize(
    ("text", "expected"),
    [
        ("a b", ("a", "b")),
        ("a b ", ("a", "b")),
        ("a  ", ("a", "")),
        (" a b", ("", "a", "b")),
        ("a  b", ("a", "", "b")),
        ("", ()),
        (" ", ("",)),
    ],
)
def test_r_strsplit_space(text: str, expected: tuple[str, ...]) -> None:
    assert r_strsplit_space(text) == expected


@pytest.mark.parametrize(
    ("path", "expected"),
    [
        ("/a/b/c", "/a/b"),
        ("/a/b/", "/a"),
        ("/a/b//", "/a"),
        ("/a//b", "/a"),
        ("/a/b/c/", "/a/b"),
        ("/a", "/"),
        ("//a", "/"),
        ("a", "."),
        ("a/", "."),
        ("/", "/"),
        ("", ""),
    ],
)
def test_r_dirname(path: str, expected: str) -> None:
    assert r_dirname(path) == expected


@pytest.mark.parametrize(
    ("pattern", "text", "expected"),
    [
        ("SSP2-NPi-AMT", "/p/x/SSP2-NPi-AMT_2026", True),
        ("C.SSP2.NDC", "/p/C_SSP2-NDC", True),  # BUG-008: '.' matches anything
        ("extern", "", False),
        ("", "/p/x", True),
        ("+foo", "x+foo", True),  # TRE: a leading repetition operator is literal
        ("*foo", "x*foo", True),
        ("a$b", "a$b", False),  # '$' is an anchor in both engines
    ],
)
def test_r_grepl(pattern: str, text: str, expected: bool) -> None:
    assert r_grepl(pattern, text) is expected


@pytest.mark.parametrize(
    ("pattern", "reason"),
    [
        ("C_SSP2[NDC", "Missing ']'"),
        ("a(b", "Missing ')'"),
        ("foo\\", "Trailing backslash"),
        ("a**b", "Invalid use of repetition operators"),
    ],
)
def test_r_grepl_invalid_pattern_raises_the_r_message(pattern: str, reason: str) -> None:
    with pytest.raises(SlurmError) as excinfo:
        r_grepl(pattern, "/p/x")
    assert str(excinfo.value) == INVALID_REGEX.format(pattern=pattern, reason=reason)


# ---------------------------------------------------------------------------------------------
# SqueueLine and SlurmJob
# ---------------------------------------------------------------------------------------------


def test_parse_squeue_reads_fields_from_the_ends() -> None:
    line = parse_squeue("alice /p/work/run my job name 3:45 RUNNING standby")
    assert line.user == "alice"
    assert line.first_field == "alice"
    assert line.workdir == "/p/work/run"
    assert line.name == "my job name"
    assert line.elapsed == "3:45"
    assert line.state == "RUNNING"
    assert line.qos == "standby"


def test_parse_squeue_short_line_is_subscript_out_of_bounds() -> None:
    line = parse_squeue("alice /p/work")
    assert line.user == "alice"
    assert line.qos == "/p/work"
    with pytest.raises(SlurmError, match=SUBSCRIPT_OUT_OF_BOUNDS):
        _ = line.elapsed


def test_parse_squeue_empty_line() -> None:
    line = parse_squeue("")
    assert line.fields == ()
    assert line.first_field is None
    with pytest.raises(SlurmError, match=SUBSCRIPT_OUT_OF_BOUNDS):
        _ = line.user


def test_to_job_clean_flags() -> None:
    assert parse_squeue(six("u", "/d", "j", "0:00", "PENDING", "qos1")).to_job() == SlurmJob(
        "u", "/d", "j", "0:00", "PENDING", "qos1", pending=True, startup=False
    )
    assert parse_squeue(six("u", "/d", "j", "3:45")).to_job().startup is True
    assert parse_squeue(six("u", "/d", "j", "6:00")).to_job().startup is False
    assert parse_squeue(six("u", "/d", "j", "2:15:00")).to_job().startup is False
    with pytest.raises(SlurmError, match="fewer than six fields"):
        parse_squeue("u /d j 0:00 RUNNING").to_job()


@pytest.mark.parametrize(
    ("text", "expected"),
    [
        ("3:45", timedelta(minutes=3, seconds=45)),
        ("2:15:00", timedelta(hours=2, minutes=15)),
        ("2-05:34:25", timedelta(days=2, hours=5, minutes=34, seconds=25)),
        ("0:00", timedelta()),
        ("INVALID", None),
        ("N/A", None),
        ("", None),
    ],
)
def test_parse_elapsed(text: str, expected: timedelta | None) -> None:
    assert parse_elapsed(text) == expected
    assert SlurmJob("u", "/d", "j", text, "RUNNING", "q", False, False).elapsed_time == expected


# ---------------------------------------------------------------------------------------------
# match_squeue (R/foundInSlurm.R:20-27)
# ---------------------------------------------------------------------------------------------

RUNNING = six(ME, "/p/out/running", "running")
EXTRA = six(ME, "/p/out/running-extra", "running")
OTHER = six("alice", "/p/out/nolog", "nolog")


def test_match_directory_substring_and_name_filter() -> None:
    # BUG-023: the job in <dir>-extra is kept for <dir> too, since both greps are substring matches
    matched = match_squeue(lines(RUNNING, EXTRA, OTHER), "/p/out/running", "running")  # type: ignore[arg-type]
    assert [line.text for line in matched] == [RUNNING, EXTRA]
    matched = match_squeue(lines(EXTRA, OTHER), "/p/out/running", "running")  # type: ignore[arg-type]
    assert [line.text for line in matched] == [EXTRA]
    # the name filter is "<runname> " as a substring: "running" does not pass for "run"
    assert match_squeue(lines(RUNNING), "/p/out/running", "run") == ()  # type: ignore[arg-type]


def test_match_magpie_iteration_falls_back_to_the_remind_job() -> None:
    rem = six(ME, "/p/runs/coupled/remind/output/C_Base-rem-2", "C_Base-rem-2")
    elsewhere = six("alice", "/p/other/remind/output/C_Base-rem-2", "C_Base-rem-2")
    # the grand-parent of the MAgPIE iteration is the model folder (lines 15-17), the retry uses its parent
    matched = match_squeue(lines(rem, elsewhere), "/p/runs/coupled/magpie", "C_Base-mag-2")  # type: ignore[arg-type]
    assert [line.text for line in matched] == [rem]
    # no retry for a REMIND iteration
    assert match_squeue(lines(rem), "/p/runs/coupled/remind", "C_Base-rem-3") == ()  # type: ignore[arg-type]
    # no retry when the first pass hit
    mag = six(ME, "/p/runs/coupled/magpie/output/C_Base-mag-2", "C_Base-mag-2")
    matched = match_squeue(lines(mag, rem), "/p/runs/coupled/magpie", "C_Base-mag-2")  # type: ignore[arg-type]
    assert [line.text for line in matched] == [mag]


# ---------------------------------------------------------------------------------------------
# slurm_answer (R/foundInSlurm.R:28-47)
# ---------------------------------------------------------------------------------------------


def answer(*texts: str, user: str = ME) -> str:
    return slurm_answer([parse_squeue(text) for text in texts], user)


def test_single_hit_own_job_is_the_qos() -> None:
    assert answer(six(ME, "/d", "j", "2:15:00", "RUNNING", "standby")) == "standby"
    assert answer(six(ME, "/d", "j", "3:45")) == "standby startup"
    assert answer(six(ME, "/d", "j", "0:00", "PENDING", "standby")) == "standby pending"
    assert answer(six(ME, "/d", "j", "6:00")) == "standby"
    assert answer(six(ME, "/d", "j", "5:59")) == "standby startup"


def test_single_hit_other_users_job_is_the_user() -> None:
    assert answer(six("alice", "/d", "j")) == "alice"
    assert answer(six("alice", "/d", "j", "2:30")) == "alice startup"
    assert answer(six("alice", "/d", "j", "0:00", "PENDING", "priority")) == "alice pending"


def test_pending_needs_a_qos_of_letters_bug_031() -> None:
    assert answer(six(ME, "/d", "j", "0:00", "PENDING", "qos1")) == "qos1 startup"
    assert answer(six(ME, "/d", "j", "0:00", "PENDING", "standby")) == "standby pending"


def test_user_is_a_regex_bug_009() -> None:
    line = six(ME, "/d", "j")
    assert answer(line, user="pascalfu,alice") == ME
    assert answer(line, user="pascal.u") == "standby"
    assert answer(line, user="pascal") == ME  # "^pascal " does not match "pascalfu "


def test_several_hits_of_one_user_no_suffix_bug_019() -> None:
    assert (
        answer(
            six(ME, "/d", "j", "0:00", "PENDING", "standby"), six(ME, "/d", "j", "0:00", "PENDING", "standby")
        )
        == ME
    )
    assert answer(six("alice", "/d", "j", "2:30"), six("alice", "/d", "j", "2:30")) == "alice"


def test_several_hits_count_jobs_bug_035() -> None:
    assert answer(six("alice", "/d", "j"), six("bob", "/d", "j")) == "2 users"
    assert answer(six("alice", "/d", "j"), six("alice", "/d", "j"), six("bob", "/d", "j")) == "3 users"


def test_no_hit() -> None:
    assert answer() == NO_JOB


def test_single_hit_short_line_errors_like_r() -> None:
    with pytest.raises(SlurmError, match=SUBSCRIPT_OUT_OF_BOUNDS):
        answer("pascalfu /d")


# ---------------------------------------------------------------------------------------------
# found_in_slurm through FakeEffects (R/foundInSlurm.R:10-19)
# ---------------------------------------------------------------------------------------------


def test_found_in_slurm_queries_every_user_and_defaults_to_the_effects_user(tmp_path: Path) -> None:
    run = tmp_path / "running"
    run.mkdir()
    env = effects({"squeue_all.txt": six(ME, str(run), "running", "3:45") + "\n"}, tmp_path)
    assert found_in_slurm(run, env) == "standby startup"
    assert found_in_slurm("running/", env) == "standby startup"  # normalizePath resolves the relative path
    assert found_in_slurm(run, env, user="alice") == "pascalfu startup"
    lookup = lookup_slurm(run, env)
    assert lookup.call.shell_text == "squeue -h -o '%u %Z %j %M %T %q'"
    assert lookup.call.argv == ("squeue", "-h", "-o", "%u %Z %j %M %T %q")
    assert lookup.call.warning is None
    assert lookup.directory == str(run)
    assert lookup.run_name == "running"
    assert lookup.job == SlurmJob(ME, str(run), "running", "3:45", "RUNNING", "standby", False, True)


def test_found_in_slurm_missing_directory_keeps_the_path_as_given(tmp_path: Path) -> None:
    env = effects({"squeue_all.txt": six(ME, "/p/none/C_x-mag-1", "C_x-mag-1") + "\n"}, tmp_path)
    assert found_in_slurm("/p/none/C_x-mag-1", env) == "standby"
    lookup = lookup_slurm("/p/none/C_x-rem-1/", env)
    assert lookup.run_name == "C_x-rem-1"
    assert lookup.directory == "/p/none/C_x-rem-1/"  # the trailing slash defeats the -rem-N rule, as in R
    assert lookup.text == NO_JOB


def test_found_in_slurm_grand_parent_rule(tmp_path: Path) -> None:
    rem = tmp_path / "remind" / "output" / "C_x-rem-1"
    rem.mkdir(parents=True)
    env = effects({"squeue_all.txt": six(ME, str(tmp_path / "remind"), "C_x-rem-1") + "\n"}, tmp_path)
    lookup = lookup_slurm(rem, env)
    assert lookup.directory == str(tmp_path / "remind")  # dirname(dirname(<remind>/output/C_x-rem-1))
    assert lookup.text == "standby"


def test_found_in_slurm_failing_squeue_is_no(tmp_path: Path) -> None:
    env = effects({"squeue_all.txt": six(ME, str(tmp_path), "x") + "\n", "squeue_exit": "1\n"}, tmp_path)
    lookup = lookup_slurm(tmp_path, env)
    assert lookup.text == NO_JOB
    assert lookup.matched == ()
    assert lookup.job is None
    assert lookup.call.warning == "running command 'squeue -h -o '%u %Z %j %M %T %q'' had status 1"


def test_found_in_slurm_several_jobs_expose_every_job(tmp_path: Path) -> None:
    dump = (
        six("alice", str(tmp_path), "run") + "\n" + six("bob", str(tmp_path), "run", "0:00", "PENDING") + "\n"
    )
    lookup = lookup_slurm(tmp_path, effects({"squeue_all.txt": dump}, tmp_path))
    assert lookup.text == "2 users"
    assert lookup.job is None
    assert [job.user for job in lookup.jobs] == ["alice", "bob"]
    assert lookup.jobs[1].pending is True


# ---------------------------------------------------------------------------------------------
# current_runs (R/commandLineInterface.R:139-176)
# ---------------------------------------------------------------------------------------------


def make_dirs(root: Path, *names: str) -> list[str]:
    paths = []
    for name in names:
        (root / name).mkdir(parents=True, exist_ok=True)
        paths.append(str(root / name))
    return paths


def test_current_runs_commands_and_order(tmp_path: Path) -> None:
    b, a_upper, a_lower = make_dirs(tmp_path, "b", "A", "a")
    dump = "".join(six(ME, path, Path(path).name) + "\n" for path in (b, a_upper, a_lower, b))
    env = effects({"squeue_all.txt": dump}, tmp_path)
    found = current_runs(ME, 0, env)
    assert found.runs == (a_lower, a_upper, b)  # sort(unique()) with R's collation
    assert [call.shell_text for call in found.calls] == [
        "squeue -u pascalfu -h -o '%Z'",
        "squeue -u pascalfu -h -o '%j'",
    ]
    assert found.calls[0].argv == ("squeue", "-u", "pascalfu", "-h", "-o", "%Z")
    assert found.calls[1].argv == ("squeue", "-u", "pascalfu", "-h", "-o", "%j")


def test_current_runs_sacct_with_daysback(tmp_path: Path) -> None:
    (done,) = make_dirs(tmp_path, "done")
    files = {
        "squeue_all.txt": "",
        "sacct_WorkDir_pascalfu.txt": f"{done}\n\n\n",
        "sacct_JobName_pascalfu.txt": "done\nbatch\nextern\n",
    }
    found = current_runs(ME, 5, effects(files, tmp_path))
    assert found.runs == (done,)
    assert [call.shell_text for call in found.calls[2:]] == [
        "sacct -u pascalfu -s cd,f,cancelled,timeout,oom -S 2026-09-25 -E now -P -n --format WorkDir",
        "sacct -u pascalfu -s cd,f,cancelled,timeout,oom -S 2026-09-25 -E now -P -n --format JobName",
    ]
    assert found.calls[2].argv[-2:] == ("--format", "WorkDir")
    assert len(current_runs(ME, 0, effects(files, tmp_path)).calls) == 2


def test_current_runs_drops_batch_and_default_only_with_mag_run(tmp_path: Path) -> None:
    coupled, magpie, default = make_dirs(tmp_path, "coupled", "coupled/magpie", "default")
    (tmp_path / "coupled" / "output" / "C_run").mkdir(parents=True)
    dump = (
        six(ME, coupled, "C_run")
        + "\n"
        + six(ME, coupled, "batch")
        + "\n"
        + six(ME, magpie, "mag-run")
        + "\n"
        + six(ME, default, "default")
        + "\n"
    )
    found = current_runs(ME, 0, effects({"squeue_all.txt": dump}, tmp_path))
    assert found.runs == (magpie, str(tmp_path / "coupled" / "output" / "C_run"))  # sorted: "m" before "o"
    no_magpie = six(ME, coupled, "batch") + "\n" + six(ME, default, "default") + "\n"
    assert current_runs(ME, 0, effects({"squeue_all.txt": no_magpie}, tmp_path)).runs == (default,)


def test_current_runs_drops_missing_paths_and_empty_workdirs(tmp_path: Path) -> None:
    (here,) = make_dirs(tmp_path, "here")
    files = {
        "squeue_all.txt": six(ME, here, "here") + "\n" + six(ME, "/p/tmp/nowhere/run1", "run1") + "\n",
        "sacct_WorkDir_pascalfu.txt": "\n\n",
        "sacct_JobName_pascalfu.txt": "extern\nextern\n",
    }
    env = effects(files, tmp_path)
    assert current_runs(ME, 1, env).runs == (here,)
    # file.exists("") is FALSE in R: the empty work directories never reach the probe
    assert all(call.args != ("",) for call in env.calls if call.name == "exists")


def test_current_runs_job_name_is_a_regex_bug_008(tmp_path: Path) -> None:
    (runs,) = make_dirs(tmp_path, "runs")
    (tmp_path / "runs" / "output" / "C_SSP2-NDC").mkdir(parents=True)
    dotted = six(ME, runs, "C.SSP2.NDC")
    assert current_runs(ME, 0, effects({"squeue_all.txt": dotted + "\n"}, tmp_path)).runs == ()
    plain = six(ME, runs, "C_SSP2-NDC")
    expected = str(tmp_path / "runs" / "output" / "C_SSP2-NDC")
    assert current_runs(ME, 0, effects({"squeue_all.txt": plain + "\n"}, tmp_path)).runs == (expected,)
    with pytest.raises(SlurmError) as excinfo:
        current_runs(ME, 0, effects({"squeue_all.txt": six(ME, runs, "C_SSP2[NDC") + "\n"}, tmp_path))
    assert str(excinfo.value) == "invalid regular expression 'C_SSP2[NDC', reason 'Missing ']''"


def test_current_runs_mismatched_answers_bug_028(tmp_path: Path) -> None:
    (here, there) = make_dirs(tmp_path, "here", "there")
    with pytest.raises(SlurmError, match=SUBSCRIPT_OUT_OF_BOUNDS):
        current_runs(
            ME, 0, effects({"squeue_Z_pascalfu.txt": f"{here}\n", "squeue_j_pascalfu.txt": ""}, tmp_path)
        )
    with pytest.raises(SlurmError, match=SUBSCRIPT_OUT_OF_BOUNDS):
        current_runs(
            ME,
            0,
            effects(
                {"squeue_Z_pascalfu.txt": f"{here}\n", "squeue_j_pascalfu.txt": "here\nthere\n"}, tmp_path
            ),
        )
    # more work directories than names: the extra ones are kept as they are
    files = {"squeue_Z_pascalfu.txt": f"{here}\n{there}\n", "squeue_j_pascalfu.txt": "here\n"}
    assert current_runs(ME, 0, effects(files, tmp_path)).runs == (here, there)
    # no work directories at all: nothing is looked at
    assert (
        current_runs(
            ME, 0, effects({"squeue_Z_pascalfu.txt": "", "squeue_j_pascalfu.txt": "x\n"}, tmp_path)
        ).runs
        == ()
    )


def test_current_runs_failing_commands_warn_like_r(tmp_path: Path) -> None:
    env = effects({"squeue_all.txt": "", "squeue_exit": "1\n", "sacct_exit": "1\n"}, tmp_path)
    found = current_runs(ME, 2, env)
    assert found.runs == ()
    sacct = "sacct -u pascalfu -s cd,f,cancelled,timeout,oom -S 2026-09-28 -E now -P -n --format"
    assert [call.warning for call in found.calls] == [
        "running command 'squeue -u pascalfu -h -o '%Z'' had status 1",
        "running command 'squeue -u pascalfu -h -o '%j'' had status 1",
        f"running command '{sacct} WorkDir' had status 1",
        f"running command '{sacct} JobName' had status 1",
    ]


def test_current_runs_empty_user_splits_like_the_shell(tmp_path: Path) -> None:
    # -A -C: opt$user <- NULL, paste0 yields "squeue -u  -h -o '%Z'", sh passes -h as the user
    found = current_runs("", 0, effects({"squeue_all.txt": six(ME, str(tmp_path), "x") + "\n"}, tmp_path))
    assert found.calls[0].shell_text == "squeue -u  -h -o '%Z'"
    assert found.calls[0].argv == ("squeue", "-u", "-h", "-o", "%Z")
    assert found.runs == ()


def test_current_runs_comma_users_are_one_argument(tmp_path: Path) -> None:
    found = current_runs("pascalfu,alice", 0, effects({"squeue_all.txt": ""}, tmp_path))
    assert found.calls[0].argv == ("squeue", "-u", "pascalfu,alice", "-h", "-o", "%Z")
