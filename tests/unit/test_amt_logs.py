"""``modelstats.amt.logs``: one log per cycle, the line format, the stderr mirror, the ``[<tool>]`` sink.

``migration/06-user-requests.md`` section 3.3: ``start`` creates ``<log-dir>/amt-<cycle id>.log`` and
``evaluate`` appends to it; every line carries the timestamp and the command; subprocess lines carry the
tool prefix; everything is mirrored to stderr. The tests inject a frozen clock so the lines are exact; the
subprocess capture is tested with ``RecordingEffects`` (canned answers) and once with ``ProductionEffects``
on a real ``sh`` child, so that both pipes are proven to reach the one log.
"""

from __future__ import annotations

import io
import logging
import re
from datetime import datetime, timedelta
from pathlib import Path
from zoneinfo import ZoneInfo

import pytest

from modelstats.amt.logs import (
    LOGGER_NAME,
    CycleFormatter,
    close_cycle_log,
    cycle_log,
    cycle_log_path,
    line_sink,
    run_logged,
    setup_cycle_log,
    tool_name,
)
from modelstats.env import CannedAmt, CommandFailedError, ProductionEffects, RecordingEffects, Stream

BERLIN = ZoneInfo("Europe/Berlin")
NOW = datetime(2026, 9, 30, 22, 4, 11, tzinfo=BERLIN)
LINE = re.compile(r"^\d{4}-\d{2}-\d{2} \d{2}:\d{2}:\d{2} (start|evaluate) {1,4}[A-Z]+ +.*$")
OUT = Stream.STDOUT
ERR = Stream.STDERR


class Clock:
    """A clock that advances by one second per read, from ``NOW``."""

    def __init__(self, moment: datetime = NOW) -> None:
        self.moment = moment

    def __call__(self) -> datetime:
        current = self.moment
        self.moment = current + timedelta(seconds=1)
        return current


@pytest.fixture(autouse=True)
def clean_logger() -> None:
    close_cycle_log(logging.getLogger(LOGGER_NAME))


def lines(path: Path) -> list[str]:
    return path.read_text(encoding="utf-8").splitlines()


def test_cycle_log_path_is_amt_cycle_id_log() -> None:
    assert cycle_log_path(Path("/p/amt/logs"), "2026-09-30") == Path("/p/amt/logs/amt-2026-09-30.log")


def test_lines_carry_timestamp_command_level_and_message(tmp_path: Path) -> None:
    log = tmp_path / "logs" / "amt-2026-09-30.log"
    stderr = io.StringIO()
    logger = setup_cycle_log(log, "start", clock=Clock(), stream=stderr)
    logger.info("Begin of AMT cycle %s", "2026-09-30")
    logger.warning("squeue failed (1 of 3)")
    logger.error("git reset failed with status %d", 128)
    expected = [
        "2026-09-30 22:04:11 start    INFO     Begin of AMT cycle 2026-09-30",
        "2026-09-30 22:04:12 start    WARNING  squeue failed (1 of 3)",
        "2026-09-30 22:04:13 start    ERROR    git reset failed with status 128",
    ]
    assert lines(log) == expected
    assert stderr.getvalue().splitlines() == expected


def test_the_directory_is_created_and_the_file_appended(tmp_path: Path) -> None:
    log = tmp_path / "missing" / "logs" / "amt-x.log"
    assert not log.parent.exists()
    logger = setup_cycle_log(log, "start", clock=Clock(), stream=io.StringIO())
    logger.info("one")
    close_cycle_log(logger)
    logger = setup_cycle_log(log, "start", clock=Clock(), stream=io.StringIO())
    logger.info("two")
    assert lines(log) == [
        "2026-09-30 22:04:11 start    INFO     one",
        "2026-09-30 22:04:11 start    INFO     two",
    ]


def test_two_commands_land_in_one_file_in_order(tmp_path: Path) -> None:
    """``start`` then ``evaluate`` on one cycle: one file, every line in order, the command on each."""
    log = cycle_log_path(tmp_path / "logs", "2026-09-30")
    clock = Clock()
    start_err = io.StringIO()
    with cycle_log(log, "start", clock=clock, stream=start_err) as logger:
        logger.info("Configuring and starting single testOneRegi-AMT")
        line_sink(logger, "Rscript")(OUT, "Run testOneRegi-AMT submitted\n")
        logger.info("Function 'startRuns' finished.")
    assert logger.handlers == []  # closed on exit
    evaluate_err = io.StringIO()
    with cycle_log(log, "evaluate", clock=clock, stream=evaluate_err) as logger:
        logger.info("waiting for all AMT runs to finish.")
        line_sink(logger, "squeue")(OUT, "")
        line_sink(logger, "git")(ERR, "commit 3f5e2a1b9c8d7e6f5a4b3c2d1e0f9a8b7c6d5e4f")
        logger.info("Function 'evaluateRuns' finished.")
    assert lines(log) == [
        "2026-09-30 22:04:11 start    INFO     Configuring and starting single testOneRegi-AMT",
        "2026-09-30 22:04:12 start    INFO     [Rscript] Run testOneRegi-AMT submitted",
        "2026-09-30 22:04:13 start    INFO     Function 'startRuns' finished.",
        "2026-09-30 22:04:14 evaluate INFO     waiting for all AMT runs to finish.",
        "2026-09-30 22:04:15 evaluate INFO     [squeue] ",
        "2026-09-30 22:04:16 evaluate INFO     [git] commit 3f5e2a1b9c8d7e6f5a4b3c2d1e0f9a8b7c6d5e4f",
        "2026-09-30 22:04:17 evaluate INFO     Function 'evaluateRuns' finished.",
    ]
    assert start_err.getvalue().splitlines() == lines(log)[:3]
    assert evaluate_err.getvalue().splitlines() == lines(log)[3:]


def test_line_sink_prefixes_the_tool_and_drops_one_line_break(tmp_path: Path) -> None:
    stderr = io.StringIO()
    logger = setup_cycle_log(None, "evaluate", clock=Clock(), stream=stderr)
    sink = line_sink(logger, "make")
    sink(OUT, "fake make: test-full-slurm\n")
    sink(ERR, "crlf line\r\n")
    sink(OUT, "no newline")
    sink(OUT, "kept: trailing spaces   ")
    sink(OUT, "\n")
    assert stderr.getvalue().splitlines() == [
        "2026-09-30 22:04:11 evaluate INFO     [make] fake make: test-full-slurm",
        "2026-09-30 22:04:12 evaluate INFO     [make] crlf line",
        "2026-09-30 22:04:13 evaluate INFO     [make] no newline",
        "2026-09-30 22:04:14 evaluate INFO     [make] kept: trailing spaces   ",
        "2026-09-30 22:04:15 evaluate INFO     [make] ",
    ]


def test_line_sink_level_can_be_raised() -> None:
    stderr = io.StringIO()
    logger = setup_cycle_log(None, "start", clock=Clock(), stream=stderr)
    line_sink(logger, "git", level=logging.WARNING)(ERR, "fatal: not a git repository")
    assert stderr.getvalue() == "2026-09-30 22:04:11 start    WARNING  [git] fatal: not a git repository\n"


def recording(tmp_path: Path, files: dict[str, str]) -> RecordingEffects:
    """``RecordingEffects`` with canned AMT answers, the checkout at ``tmp_path``."""
    return RecordingEffects(now=NOW, user="pascalfu", on_cluster=False, cwd=tmp_path, amt=CannedAmt(files))


def test_run_logged_announces_streams_and_reports_the_status(tmp_path: Path) -> None:
    """``make test-full-slurm`` answered from the case: the command, the lines, the status, in one log."""
    log = tmp_path / "amt.log"
    stderr = io.StringIO()
    logger = setup_cycle_log(log, "start", clock=Clock(), stream=stderr)
    effects = recording(tmp_path, {"make.txt": "fake make: one\ntwo\n", "make.exit": "2"})
    result = run_logged(effects, logger, ["make", "test-full-slurm"], cwd=tmp_path / "remind")
    assert result.returncode == 2 and result.stdout == "fake make: one\ntwo\n"
    assert lines(log) == [
        f"2026-09-30 22:04:11 start    INFO     running: make test-full-slurm in {tmp_path / 'remind'}",
        "2026-09-30 22:04:12 start    INFO     [make] fake make: one",
        "2026-09-30 22:04:13 start    INFO     [make] two",
        "2026-09-30 22:04:14 start    WARNING  make exited with status 2",
    ]
    assert stderr.getvalue().splitlines() == lines(log)
    assert [c.args for c in effects.runs] == [("make", "test-full-slurm")]


def test_run_logged_success_has_no_status_line(tmp_path: Path) -> None:
    stderr = io.StringIO()
    logger = setup_cycle_log(None, "evaluate", clock=Clock(), stream=stderr)
    effects = recording(tmp_path, {"git-pull.txt": "Already up to date.\n"})
    result = run_logged(effects, logger, ["git", "pull"])
    assert result.ok
    assert stderr.getvalue().splitlines() == [
        "2026-09-30 22:04:11 evaluate INFO     running: git pull",
        "2026-09-30 22:04:12 evaluate INFO     [git] Already up to date.",
    ]


def test_run_logged_check_raises_after_the_warning(tmp_path: Path) -> None:
    stderr = io.StringIO()
    logger = setup_cycle_log(None, "start", clock=Clock(), stream=stderr)
    effects = recording(tmp_path, {"git-reset.exit": "128"})
    with pytest.raises(CommandFailedError, match="git reset --hard origin/develop exited with status 128"):
        run_logged(effects, logger, ["git", "reset", "--hard", "origin/develop"], check=True)
    assert stderr.getvalue().splitlines()[-1] == (
        "2026-09-30 22:04:12 start    WARNING  git exited with status 128"
    )


def test_run_logged_quotes_the_command_like_a_shell(tmp_path: Path) -> None:
    stderr = io.StringIO()
    logger = setup_cycle_log(None, "start", clock=Clock(), stream=stderr)
    effects = recording(tmp_path, {})
    argv = ["Rscript", "start.R", "--testOneRegi", "titletag=AMT", "slurmConfig=--qos=priority --nodes=1"]
    run_logged(effects, logger, argv)
    assert (
        stderr.getvalue()
        .splitlines()[0]
        .endswith(
            "running: Rscript start.R --testOneRegi titletag=AMT 'slurmConfig=--qos=priority --nodes=1'"
        )
    )


def test_run_logged_captures_both_pipes_of_a_real_child(tmp_path: Path) -> None:
    """``ProductionEffects`` on ``sh``: stdout and stderr lines both arrive with the prefix."""
    log = tmp_path / "amt.log"
    logger = setup_cycle_log(log, "evaluate", stream=io.StringIO())
    script = "echo out one; echo err one 1>&2; echo out two; exit 3"
    result = run_logged(ProductionEffects(), logger, ["sh", "-c", script])
    assert result.returncode == 3
    messages = [line.split("evaluate ", 1)[1] for line in lines(log)]
    assert messages[0] == f"INFO     running: sh -c '{script}'"
    assert messages[-1] == "WARNING  sh exited with status 3"
    assert sorted(messages[1:-1]) == sorted(
        ["INFO     [sh] out one", "INFO     [sh] out two", "INFO     [sh] err one"]
    )
    assert messages.index("INFO     [sh] out one") < messages.index("INFO     [sh] out two")


def test_tool_name_is_the_basename_of_the_program() -> None:
    assert tool_name(["/usr/bin/Rscript", "start.R", "--testOneRegi"]) == "Rscript"
    assert tool_name(["make", "test-full-slurm"]) == "make"
    assert tool_name([]) == ""


def test_without_a_path_only_stderr_is_written(tmp_path: Path) -> None:
    stderr = io.StringIO()
    logger = setup_cycle_log(None, "start", clock=Clock(), stream=stderr)
    logger.info("dry run")
    assert stderr.getvalue() == "2026-09-30 22:04:11 start    INFO     dry run\n"
    assert [type(h).__name__ for h in logger.handlers] == ["StreamHandler"]
    assert list(tmp_path.iterdir()) == []


def test_setup_replaces_earlier_handlers_and_does_not_propagate(tmp_path: Path) -> None:
    first = io.StringIO()
    second = io.StringIO()
    setup_cycle_log(tmp_path / "a.log", "start", clock=Clock(), stream=first)
    logger = setup_cycle_log(tmp_path / "b.log", "evaluate", clock=Clock(), stream=second)
    logger.info("only once")
    assert first.getvalue() == ""
    assert second.getvalue() == "2026-09-30 22:04:11 evaluate INFO     only once\n"
    assert lines(tmp_path / "a.log") == []
    assert lines(tmp_path / "b.log") == ["2026-09-30 22:04:11 evaluate INFO     only once"]
    assert logger.propagate is False
    assert logger is logging.getLogger(LOGGER_NAME)


def test_exceptions_keep_their_traceback(tmp_path: Path) -> None:
    log = tmp_path / "amt.log"
    logger = setup_cycle_log(log, "evaluate", clock=Clock(), stream=io.StringIO())
    try:
        raise RuntimeError("squeue had exit status > 0 more than 3 times in a row.")
    except RuntimeError:
        logger.exception("evaluate failed")
    text = log.read_text()
    assert text.startswith(
        "2026-09-30 22:04:11 evaluate ERROR    evaluate failed\nTraceback (most recent call last):\n"
    )
    assert text.rstrip().endswith("RuntimeError: squeue had exit status > 0 more than 3 times in a row.")


def test_without_a_clock_the_wall_clock_is_formatted(tmp_path: Path) -> None:
    stderr = io.StringIO()
    logger = setup_cycle_log(None, "start", stream=stderr)
    logger.info("now")
    line = stderr.getvalue().rstrip("\n")
    assert LINE.match(line), line
    assert line.endswith(" start    INFO     now")


def test_formatter_alone(tmp_path: Path) -> None:
    formatter = CycleFormatter("evaluate", Clock())
    record = logging.LogRecord(LOGGER_NAME, logging.INFO, __file__, 1, "message %s", ("x",), None)
    assert formatter.format(record) == "2026-09-30 22:04:11 evaluate INFO     message x"


def test_undecodable_output_does_not_break_the_file(tmp_path: Path) -> None:
    log = tmp_path / "amt.log"
    logger = setup_cycle_log(log, "start", clock=Clock(), stream=io.StringIO())
    line_sink(logger, "git")(OUT, "caf\udce9 surrogate from a listdir")
    assert lines(log) == ["2026-09-30 22:04:11 start    INFO     [git] caf\\udce9 surrogate from a listdir"]
