"""The ``selectScenarios()`` bridge: ``modelstats.amt.bridges`` (``03`` 2.3, ``R/modeltests.R:127-137``).

Two kinds of tests. The contract tests answer ``Rscript`` and ``git`` from canned results (a
``FakeEffects`` subclass streaming the canned lines as ``Effects.run`` does) and pin the argv, cwd and
environment of the call, the ``[git]``/``[Rscript]`` log lines, the ``-AMT`` suffix and every payload
rejection. The end-to-end tests run the real ``Rscript`` through ``RecordingEffects`` (its canned ``git``
answers ``git log -1`` like the sandbox's fake, the ``ProductionEffects`` fallback runs ``Rscript``) on the
fake REMIND checkout of ``migration/harness/build_fake_remind.sh`` (the synthetic layer; skipped like the
golden tier when it is not built) and on a self-contained checkout written here, and compare the selection
with the ``runsToStart.rds`` the R oracle wrote for the ``remind-start`` golden. They need an ``Rscript`` on
the PATH and are skipped without one; the bridge itself never writes (the synthetic checkout is read in
place and its tree is checked afterwards), so copies only exist for the failure variants (a checkout
without ``selectScenarios()``, duplicate titles, a broken start script).
"""

from __future__ import annotations

import json
import logging
import os
import shutil
from collections.abc import Mapping, Sequence
from dataclasses import dataclass
from datetime import datetime
from pathlib import Path
from zoneinfo import ZoneInfo

import pytest

from modelstats.amt.bridges import (
    BRIDGE_ENVIRONMENT,
    BRIDGE_SCRIPT,
    PAYLOAD_PREFIX,
    PAYLOAD_SCHEMA,
    START_GROUP,
    BridgeError,
    ScenarioSelection,
    bridge_script,
    checkout_commit,
    is_payload_line,
    parse_payload,
    payload_line,
    select_scenarios,
    without_payload,
)
from modelstats.amt.logs import LOGGER_NAME
from modelstats.env import (
    Effects,
    FakeEffects,
    LineCallback,
    PathLike,
    ProductionEffects,
    RecordingEffects,
    RunResult,
    Stream,
    TraceRecord,
)
from modelstats.errors import ModelstatsError
from tests.conftest import GOLDENS, SYNTHETIC

NOW = datetime(2026, 9, 30, 12, 0, 0, tzinfo=ZoneInfo("Europe/Berlin"))
USER = "pascalfu"
FAKE_CHECKOUT = SYNTHETIC / "p" / "projects" / "remind" / "modeltests" / "remind"
"""The fake REMIND checkout of ``build_fake_remind.sh`` (``migration/harness/SYNTHETIC.md``)."""
GOLDEN_RUNS_TO_START = GOLDENS / "amt" / "remind-start" / "state" / "runsToStart_rds.json"
"""What the R oracle's ``selectScenarios()`` call saved for the ``remind-start`` golden."""

# The fake git of migration/harness/fakebin/git (and env.CannedAmt) answers `git log -1` with this text.
FAKE_GIT_LOG = (
    "commit 3f5e2a1b9c8d7e6f5a4b3c2d1e0f9a8b7c6d5e4f\n"
    "Author: AMT Bot <amt@example.org>\n"
    "Date:   Mon Sep 28 10:00:00 2026 +0200\n"
    "\n"
    "    Merge pull request #999 from example/feature\n"
)
FAKE_COMMIT = "3f5e2a1b9c8d7e6f5a4b3c2d1e0f9a8b7c6d5e4f"
GIT_LOG_LINES = [f"[git] {line}" for line in FAKE_GIT_LOG.splitlines()]

# The payload the script prints for the fake checkout (probe of 2026-10-02, pinned here).
FAKE_PAYLOAD = (
    '{"schema": 1, "startgroup": "AMT", "names": ["SSP2-NPi", "default", "SSP2-EU21-PkBudg650", '
    '"SSP2-NPi2025-calibrate", "SSP3-NPi2025", "SSP2-EcBudg500", "SSP2-EU21-NPi2025", "testOneRegi", '
    '"SSP2-never"], "columns": {"start": ["AMT", "AMT", "AMT", "AMT", "AMT", "AMT", "AMT", "AMT", "AMT"], '
    '"description": ["SSP2 with national policies", "default scenario", "peak budget 650", '
    '"CES calibration", "SSP3 with national policies 2025", "end-of-century budget 500", '
    '"EU21 national policies 2025", "single region test", "in the AMT group but never started"]}}'
)
FAKE_NAMES = (
    "SSP2-NPi-AMT",
    "default-AMT",
    "SSP2-EU21-PkBudg650-AMT",
    "SSP2-NPi2025-calibrate-AMT",
    "SSP3-NPi2025-AMT",
    "SSP2-EcBudg500-AMT",
    "SSP2-EU21-NPi2025-AMT",
    "testOneRegi-AMT",
    "SSP2-never-AMT",
)
SELECTED_NINE = 'selectScenarios(startgroup = "AMT") selected 9 scenarios: ' + ", ".join(FAKE_NAMES)

needs_rscript = pytest.mark.skipif(shutil.which("Rscript") is None, reason="the bridge runs the real Rscript")
needs_fake_checkout = pytest.mark.skipif(
    not (FAKE_CHECKOUT / "config" / "scenario_config.csv").is_file(),
    reason=f"synthetic layer not built: {FAKE_CHECKOUT} (migration/harness/build_fake_remind.sh)",
)


# ---------------------------------------------------------------------------------------------
# Doubles
# ---------------------------------------------------------------------------------------------


@dataclass(frozen=True)
class Call:
    argv: tuple[str, ...]
    cwd: str | None
    env: Mapping[str, str] | None


class CannedPrograms(FakeEffects):
    """A ``FakeEffects`` whose ``run`` answers programs from canned results and records every call.

    The canned lines are streamed to ``on_line`` as ``Effects.run`` streams a child's output (stdout first,
    then stderr, the order a canned ``RecordingEffects`` answer uses too).
    """

    def __init__(self, answers: Mapping[str, RunResult]) -> None:
        super().__init__(now=NOW, user=USER, on_cluster=False)
        self._answers = dict(answers)
        self.program_calls: list[Call] = []

    def run(
        self,
        argv: Sequence[str],
        *,
        cwd: PathLike | None = None,
        env: Mapping[str, str] | None = None,
        check: bool = False,
        capture: bool = True,
        on_line: LineCallback | None = None,
    ) -> RunResult:
        assert capture, "the bridge captures both streams"
        self.program_calls.append(Call(tuple(argv), None if cwd is None else os.fspath(cwd), env))
        canned = self._answers[os.path.basename(argv[0])]
        if on_line is not None:
            for line in canned.stdout.splitlines():
                on_line(Stream.STDOUT, line)
            for line in canned.stderr.splitlines():
                on_line(Stream.STDERR, line)
        return RunResult(tuple(argv), canned.returncode, canned.stdout, canned.stderr)


def canned(returncode: int = 0, stdout: str = "", stderr: str = "") -> RunResult:
    return RunResult(("canned",), returncode, stdout, stderr)


def git_ok() -> RunResult:
    return canned(0, FAKE_GIT_LOG)


def effects_answering(rscript: RunResult, git: RunResult | None = None) -> CannedPrograms:
    return CannedPrograms({"Rscript": rscript, "git": git if git is not None else git_ok()})


def real_rscript(cwd: Path) -> RecordingEffects:
    """The AMT double with the real ``Rscript``: ``git`` canned like the sandbox's fake, the rest real."""
    return RecordingEffects(now=NOW, user=USER, on_cluster=False, cwd=cwd, fallback=ProductionEffects())


def rscript_calls(effects: CannedPrograms) -> list[Call]:
    return [call for call in effects.program_calls if os.path.basename(call.argv[0]) == "Rscript"]


def rscript_trace(effects: RecordingEffects) -> list[TraceRecord]:
    return [record for record in effects.trace if record.tool == "Rscript"]


def messages(caplog: pytest.LogCaptureFixture, level: int = logging.INFO) -> list[str]:
    return [record.getMessage() for record in caplog.records if record.levelno >= level]


def checkout_dir(tmp_path: Path) -> Path:
    """An existing, empty directory standing in for the checkout of the canned tests."""
    path = tmp_path / "remind"
    path.mkdir(exist_ok=True)
    return path


def tree_snapshot(root: Path) -> dict[str, tuple[int, int]]:
    """Every entry below ``root`` with its mtime and size: the bridge must leave a checkout untouched."""
    snapshot: dict[str, tuple[int, int]] = {}
    for directory, _dirs, filenames in os.walk(root):
        for name in filenames:
            path = Path(directory) / name
            info = path.stat()
            snapshot[str(path.relative_to(root))] = (info.st_mtime_ns, info.st_size)
    return snapshot


# ---------------------------------------------------------------------------------------------
# Checkouts for the real Rscript
# ---------------------------------------------------------------------------------------------

SELECT_SCENARIOS_STUB = """\
# the selectScenarios() stub of build_fake_remind.sh, plus lines proving the bridge environment and that
# stdout noise from a sourced script is tolerated and logged
message("autoRenvFixDeps=", Sys.getenv("autoRenvFixDeps"),
        " LC_ALL=", Sys.getenv("LC_ALL"), " TZ=", Sys.getenv("TZ"))
cat("sourcing selectScenarios.R\\n")
selectScenarios <- function(settings, interactive = FALSE, startgroup = "1") {
  keep <- vapply(strsplit(as.character(settings$start), ",", fixed = TRUE),
                 function(s) startgroup %in% trimws(s), logical(1))
  keep[is.na(keep)] <- FALSE
  settings[keep, , drop = FALSE]
}
"""

# read.csv2(): ';' separated, ',' as the decimal mark, '#' comments, empty cells NA, quoted fields.
MINI_CSV = """\
# a self-contained scenario_config.csv for the bridge tests
title;start;description;weight;flag
SSP2-NPi;AMT;"quoted ""description"" with a \\ backslash and an umlaut: ä";1,5;TRUE
default;AMT,1;default scenario;2;FALSE
SSP2-manual;1;not in the AMT group;;TRUE
SSP2-tab;AMT;a tab\there and a slash / there;0,25;
"""


def write_checkout(root: Path, csv: str = MINI_CSV, stub: str = SELECT_SCENARIOS_STUB) -> Path:
    """A REMIND-shaped checkout: ``config/scenario_config.csv`` and ``scripts/start/selectScenarios.R``."""
    (root / "config").mkdir(parents=True)
    (root / "config" / "scenario_config.csv").write_text(csv, encoding="utf-8")
    (root / "scripts" / "start").mkdir(parents=True)
    (root / "scripts" / "start" / "selectScenarios.R").write_text(stub, encoding="utf-8")
    return root


@pytest.fixture
def fake_checkout_copy(tmp_path: Path) -> Path:
    """``config/`` and ``scripts/`` of the fake REMIND checkout, copied so a test may break them."""
    root = tmp_path / "remind"
    root.mkdir()
    for part in ("config", "scripts"):
        shutil.copytree(FAKE_CHECKOUT / part, root / part)
    return root


def golden_runs_to_start() -> tuple[list[str], dict[str, list[object]]]:
    """Row names and columns of the ``runsToStart.rds`` the R oracle saved (``remind-start`` golden)."""
    rows = json.loads(GOLDEN_RUNS_TO_START.read_text(encoding="utf-8"))["value"]["rows"]
    names = [row["_row"] for row in rows]
    columns = {column: [row[column] for row in rows] for column in rows[0] if column != "_row"}
    return names, columns


# ---------------------------------------------------------------------------------------------
# The contract (canned programs)
# ---------------------------------------------------------------------------------------------


def test_the_rscript_call_is_the_documented_contract(tmp_path: Path) -> None:
    effects = effects_answering(canned(0, FAKE_PAYLOAD + "\n"))
    checkout = checkout_dir(tmp_path)

    select_scenarios(effects, checkout)

    (call,) = rscript_calls(effects)
    assert call.argv[0] == "Rscript"
    assert call.argv[2:] == (START_GROUP,)
    assert Path(call.argv[1]).name == BRIDGE_SCRIPT
    assert Path(call.argv[1]).parent.name == "bridge_scripts"
    assert call.cwd == str(checkout)
    assert call.env == {"autoRenvFixDeps": "TRUE", "LC_ALL": "C.utf8", "TZ": "Europe/Berlin"}
    assert call.env == BRIDGE_ENVIRONMENT


def test_the_commit_is_read_before_the_script_runs_from_git_log_1(tmp_path: Path) -> None:
    effects = effects_answering(canned(0, FAKE_PAYLOAD + "\n"))
    checkout = checkout_dir(tmp_path)

    selection = select_scenarios(effects, checkout)

    git_call, rscript_call = effects.program_calls
    assert git_call.argv == ("git", "log", "-1")
    assert git_call.cwd == str(checkout)
    assert rscript_call.argv[0] == "Rscript"
    assert selection.commit == FAKE_COMMIT


def test_names_get_the_amt_suffix_and_the_columns_stay(tmp_path: Path) -> None:
    effects = effects_answering(canned(0, FAKE_PAYLOAD + "\n"))

    selection = select_scenarios(effects, checkout_dir(tmp_path))

    assert selection.names == FAKE_NAMES
    assert selection.titles == tuple(name.removesuffix("-AMT") for name in FAKE_NAMES)
    assert selection.startgroup == "AMT"
    assert list(selection.columns) == ["start", "description"]
    assert selection.columns["start"] == ("AMT",) * 9
    assert selection.columns["description"][0] == "SSP2 with national policies"
    assert len(selection) == 9
    assert selection.row("testOneRegi-AMT") == {"start": "AMT", "description": "single region test"}
    assert selection.row("testOneRegi") == selection.row("testOneRegi-AMT")


def test_the_startgroup_and_title_tag_are_parameters(tmp_path: Path) -> None:
    payload = '{"schema": 1, "startgroup": "nightly", "names": ["a"], "columns": {"start": ["nightly"]}}'
    effects = effects_answering(canned(0, payload + "\n"))

    selection = select_scenarios(effects, checkout_dir(tmp_path), startgroup="nightly", title_tag="NIGHT")

    (call,) = rscript_calls(effects)
    assert call.argv[2] == "nightly"
    assert selection.names == ("a-NIGHT",)
    assert selection.startgroup == "nightly"


def test_an_empty_selection_keeps_the_columns(tmp_path: Path, caplog: pytest.LogCaptureFixture) -> None:
    payload = '{"schema": 1, "startgroup": "AMT", "names": [], "columns": {"start": [], "description": []}}'
    effects = effects_answering(canned(0, payload + "\n"))
    caplog.set_level(logging.INFO, logger=LOGGER_NAME)

    selection = select_scenarios(effects, checkout_dir(tmp_path))

    assert selection.names == ()
    assert selection.columns == {"start": (), "description": ()}
    assert len(selection) == 0
    assert 'selectScenarios(startgroup = "AMT") selected 0 scenarios: (none)' in messages(caplog)


def test_the_trace_lines_name_the_commit_the_script_output_and_the_selection(
    tmp_path: Path, caplog: pytest.LogCaptureFixture
) -> None:
    effects = effects_answering(
        canned(0, "sourcing hint\n" + FAKE_PAYLOAD + "\n", "Loading required package: x\n")
    )
    caplog.set_level(logging.INFO, logger=LOGGER_NAME)
    checkout = checkout_dir(tmp_path)

    select_scenarios(effects, checkout)

    assert messages(caplog) == [
        *GIT_LOG_LINES,
        f"selectScenarios bridge: Rscript select_scenarios.R AMT in {checkout} (REMIND commit {FAKE_COMMIT})",
        "[Rscript] sourcing hint",
        "[Rscript] Loading required package: x",
        SELECTED_NINE,
    ]
    assert all(record.name == LOGGER_NAME for record in caplog.records)


def test_the_payload_line_stays_out_of_the_log(tmp_path: Path, caplog: pytest.LogCaptureFixture) -> None:
    effects = effects_answering(canned(0, FAKE_PAYLOAD + "\n"))
    caplog.set_level(logging.INFO, logger=LOGGER_NAME)

    select_scenarios(effects, checkout_dir(tmp_path))

    assert not any(PAYLOAD_PREFIX in line for line in messages(caplog))
    assert not any(line.startswith("[Rscript]") for line in messages(caplog))


def test_an_explicit_logger_is_used(tmp_path: Path, caplog: pytest.LogCaptureFixture) -> None:
    effects = effects_answering(canned(0, FAKE_PAYLOAD + "\n"))
    caplog.set_level(logging.INFO, logger="amt.cycle")

    select_scenarios(effects, checkout_dir(tmp_path), logger=logging.getLogger("amt.cycle"))

    assert any(record.name == "amt.cycle" for record in caplog.records)
    assert not any(record.name == LOGGER_NAME for record in caplog.records)


def test_a_non_zero_exit_fails_the_step_after_logging_the_output(
    tmp_path: Path, caplog: pytest.LogCaptureFixture
) -> None:
    stderr = (
        "Error in read.table(file = file, header = header, sep = sep, quote = quote,  : \n"
        "  boom\nExecution halted\n"
    )
    effects = effects_answering(canned(1, "", stderr))
    caplog.set_level(logging.INFO, logger=LOGGER_NAME)
    checkout = checkout_dir(tmp_path)

    with pytest.raises(BridgeError) as excinfo:
        select_scenarios(effects, checkout)

    assert str(excinfo.value) == f"Rscript select_scenarios.R exited with status 1 in {checkout}"
    assert excinfo.value.result is not None
    assert excinfo.value.result.returncode == 1
    assert excinfo.value.result.stderr == stderr
    assert isinstance(excinfo.value, ModelstatsError)
    assert messages(caplog)[len(GIT_LOG_LINES) + 1 :] == [
        "[Rscript] Error in read.table(file = file, header = header, sep = sep, quote = quote,  : ",
        "[Rscript]   boom",
        "[Rscript] Execution halted",
        f"selectScenarios bridge failed: Rscript select_scenarios.R exited with status 1 in {checkout}",
    ]
    assert messages(caplog, logging.ERROR) == [messages(caplog)[-1]]
    assert [call.name for call in effects.calls] == ["is_dir"]  # the bridge only looked, never wrote
    assert os.listdir(checkout) == []


def test_a_missing_rscript_is_a_bridge_error_with_status_127(tmp_path: Path) -> None:
    effects = effects_answering(canned(127, "", "Rscript: command not found\n"))

    with pytest.raises(BridgeError, match="exited with status 127"):
        select_scenarios(effects, checkout_dir(tmp_path))


def test_a_missing_checkout_fails_before_any_program_runs(tmp_path: Path) -> None:
    effects = effects_answering(canned(0, FAKE_PAYLOAD + "\n"))
    missing = tmp_path / "nowhere"

    with pytest.raises(BridgeError) as excinfo:
        select_scenarios(effects, missing)

    assert str(excinfo.value) == f"REMIND checkout {missing} is not a directory"
    assert excinfo.value.result is None
    assert effects.program_calls == []


def test_a_payload_problem_is_a_bridge_error_carrying_the_result(
    tmp_path: Path, caplog: pytest.LogCaptureFixture
) -> None:
    effects = effects_answering(canned(0, "no json here\n"))
    caplog.set_level(logging.INFO, logger=LOGGER_NAME)

    with pytest.raises(BridgeError, match="printed no JSON object on its last stdout line") as excinfo:
        select_scenarios(effects, checkout_dir(tmp_path))

    assert excinfo.value.result is not None
    assert excinfo.value.result.stdout == "no json here\n"
    assert "[Rscript] no json here" in messages(caplog)
    assert messages(caplog, logging.ERROR)[0].startswith(
        "selectScenarios bridge failed: select_scenarios.R printed no"
    )


def test_git_failure_logs_a_warning_and_leaves_the_commit_unknown(
    tmp_path: Path, caplog: pytest.LogCaptureFixture
) -> None:
    effects = effects_answering(
        canned(0, FAKE_PAYLOAD + "\n"), git=canned(128, "", "fatal: not a git repository\n")
    )
    caplog.set_level(logging.INFO, logger=LOGGER_NAME)
    checkout = checkout_dir(tmp_path)

    selection = select_scenarios(effects, checkout)

    assert selection.commit is None
    assert messages(caplog)[:3] == [
        "[git] fatal: not a git repository",
        f"could not determine the REMIND commit of {checkout}: git log -1 exited with status 128, no output",
        f"selectScenarios bridge: Rscript select_scenarios.R AMT in {checkout} (REMIND commit unknown)",
    ]
    assert messages(caplog, logging.WARNING) == [messages(caplog)[1]]


def test_checkout_commit_parses_the_first_line_like_r(tmp_path: Path) -> None:
    assert checkout_commit(effects_answering(canned(), git=git_ok()), tmp_path) == FAKE_COMMIT
    odd = canned(0, "HEAD detached\ncommit abc\n")
    assert checkout_commit(effects_answering(canned(), git=odd), tmp_path) is None
    assert checkout_commit(effects_answering(canned(), git=canned(0, "")), tmp_path) is None


def test_payload_line_is_the_last_non_blank_stdout_line() -> None:
    assert payload_line(canned(0, "a\n{...}\n\n  \n")) == "{...}"
    assert payload_line(canned(0, "unterminated")) == "unterminated"
    assert payload_line(canned(0, "")) == ""
    assert payload_line(canned(0, "\n\n")) == ""


def test_the_payload_filter_passes_everything_but_the_payload_line() -> None:
    seen: list[tuple[Stream, str]] = []
    sink = without_payload(lambda stream, line: seen.append((stream, line)))

    sink(Stream.STDOUT, "sourcing hint")
    sink(Stream.STDOUT, FAKE_PAYLOAD)
    sink(Stream.STDOUT, "  " + PAYLOAD_PREFIX + "2}")
    sink(Stream.STDERR, PAYLOAD_PREFIX + "1}")

    assert seen == [(Stream.STDOUT, "sourcing hint"), (Stream.STDERR, PAYLOAD_PREFIX + "1}")]
    assert is_payload_line(FAKE_PAYLOAD)
    assert not is_payload_line('{"startgroup": "AMT"}')


@pytest.mark.parametrize(
    ("line", "complaint"),
    [
        ("", "printed no JSON payload"),
        ("   ", "printed no JSON payload"),
        ("{not json", "printed no JSON object on its last stdout line"),
        ("[1, 2]", "printed a JSON list, not an object"),
        ('{"schema": 2, "startgroup": "AMT", "names": [], "columns": {}}', "payload schema 2 is not 1"),
        ('{"startgroup": "AMT", "names": [], "columns": {}}', "payload schema None is not 1"),
        ('{"schema": 1, "names": [], "columns": {}}', "'startgroup' is not a string"),
        (
            '{"schema": 1, "startgroup": "AMT", "names": "a", "columns": {}}',
            "'names' is not a list of strings",
        ),
        (
            '{"schema": 1, "startgroup": "AMT", "names": [1], "columns": {}}',
            "'names' is not a list of strings",
        ),
        ('{"schema": 1, "startgroup": "AMT", "names": ["a"], "columns": []}', "'columns' is not an object"),
        (
            '{"schema": 1, "startgroup": "AMT", "names": ["a"], "columns": {"start": "AMT"}}',
            "'start' is not a list",
        ),
        (
            '{"schema": 1, "startgroup": "AMT", "names": ["a", "b"], "columns": {"start": ["AMT"]}}',
            "column 'start' has 1 cells for 2 rows",
        ),
        (
            '{"schema": 1, "startgroup": "AMT", "names": ["a"], "columns": {"start": [{"x": 1}]}}',
            "column 'start' holds a dict",
        ),
    ],
)
def test_parse_payload_rejects_anything_but_the_documented_object(line: str, complaint: str) -> None:
    with pytest.raises(BridgeError) as excinfo:
        parse_payload(line)
    assert complaint in str(excinfo.value)
    assert excinfo.value.result is None


def test_parse_payload_keeps_the_json_cell_types() -> None:
    line = (
        '{"schema": 1, "startgroup": "AMT", "names": ["a", "b"], '
        '"columns": {"n": [1, 2.5], "flag": [true, null], "text": ["x", null]}}'
    )
    selection = parse_payload(line, commit="abc")
    assert selection.columns == {"n": (1, 2.5), "flag": (True, None), "text": ("x", None)}
    assert selection.commit == "abc"
    assert selection == ScenarioSelection("AMT", ("a", "b"), ("a-AMT", "b-AMT"), selection.columns, "abc")
    assert PAYLOAD_SCHEMA == 1


def test_the_bridge_script_ships_with_the_package() -> None:
    with bridge_script() as script:
        assert script.name == BRIDGE_SCRIPT
        text = script.read_text(encoding="utf-8")
    assert text.startswith("# select_scenarios.R")
    assert 'read.csv2("config/scenario_config.csv"' in text
    assert "stringsAsFactors = FALSE" in text
    assert "row.names = 1" in text
    assert 'comment.char = "#"' in text
    assert 'na.strings = ""' in text
    assert 'list.files("scripts/start", pattern = "\\\\.R$", full.names = TRUE)' in text
    assert "selectScenarios(settings = settings, interactive = FALSE, startgroup = startgroup)" in text
    assert 'cat("{\\"schema\\": 1, ' in text  # the payload line starts with PAYLOAD_PREFIX


def test_the_effects_protocol_is_what_the_bridge_needs(tmp_path: Path) -> None:
    effects: Effects = effects_answering(canned(0, FAKE_PAYLOAD + "\n"))
    assert select_scenarios(effects, checkout_dir(tmp_path)).names == FAKE_NAMES


# ---------------------------------------------------------------------------------------------
# The real Rscript on the fake REMIND checkout
# ---------------------------------------------------------------------------------------------


@needs_rscript
@needs_fake_checkout
def test_fake_checkout_selection_equals_the_r_oracles_runs_to_start(caplog: pytest.LogCaptureFixture) -> None:
    effects = real_rscript(FAKE_CHECKOUT)
    caplog.set_level(logging.INFO, logger=LOGGER_NAME)
    before = tree_snapshot(FAKE_CHECKOUT)

    selection = select_scenarios(effects, FAKE_CHECKOUT)

    golden_names, golden_columns = golden_runs_to_start()
    assert list(selection.names) == golden_names
    assert {column: list(cells) for column, cells in selection.columns.items()} == golden_columns
    assert selection.names == FAKE_NAMES
    assert selection.commit == FAKE_COMMIT
    (record,) = rscript_trace(effects)
    assert record.argv[1:] == ("AMT",)
    assert record.cwd == str(FAKE_CHECKOUT)
    assert {key: record.env[key] for key in BRIDGE_ENVIRONMENT} == dict(BRIDGE_ENVIRONMENT)
    assert messages(caplog) == [
        *GIT_LOG_LINES,
        f"selectScenarios bridge: Rscript select_scenarios.R AMT in {FAKE_CHECKOUT} "
        f"(REMIND commit {FAKE_COMMIT})",
        SELECTED_NINE,
    ]
    assert tree_snapshot(FAKE_CHECKOUT) == before  # the canonical synthetic layer is untouched


@needs_rscript
@needs_fake_checkout
def test_fake_checkout_without_select_scenarios_fails_the_step(
    fake_checkout_copy: Path, caplog: pytest.LogCaptureFixture
) -> None:
    (fake_checkout_copy / "scripts" / "start" / "selectScenarios.R").unlink()
    effects = real_rscript(fake_checkout_copy)
    caplog.set_level(logging.INFO, logger=LOGGER_NAME)
    before = tree_snapshot(fake_checkout_copy)

    with pytest.raises(BridgeError) as excinfo:
        select_scenarios(effects, fake_checkout_copy)

    assert str(excinfo.value) == f"Rscript select_scenarios.R exited with status 1 in {fake_checkout_copy}"
    assert excinfo.value.result is not None
    assert excinfo.value.result.returncode == 1
    logged = messages(caplog)
    assert logged[len(GIT_LOG_LINES)].endswith(f"(REMIND commit {FAKE_COMMIT})")
    assert any(
        line.startswith("[Rscript] Error: no selectScenarios() is defined by scripts/start/*.R of")
        for line in logged
    )
    assert logged[-1] == f"selectScenarios bridge failed: {excinfo.value}"
    assert tree_snapshot(fake_checkout_copy) == before  # nothing written
    assert sorted(os.listdir(fake_checkout_copy)) == ["config", "scripts"]


@needs_rscript
def test_mini_checkout_cells_cross_the_bridge_intact(
    tmp_path: Path, caplog: pytest.LogCaptureFixture
) -> None:
    checkout = write_checkout(tmp_path / "remind")
    caplog.set_level(logging.INFO, logger=LOGGER_NAME)

    selection = select_scenarios(real_rscript(checkout), checkout)

    assert selection.names == ("SSP2-NPi-AMT", "default-AMT", "SSP2-tab-AMT")
    assert selection.columns == {
        "start": ("AMT", "AMT,1", "AMT"),
        "description": (
            'quoted "description" with a \\ backslash and an umlaut: ä',
            "default scenario",
            "a tab\there and a slash / there",
        ),
        "weight": (1.5, 2, 0.25),
        "flag": (True, False, None),
    }
    logged = messages(caplog)
    assert "[Rscript] autoRenvFixDeps=TRUE LC_ALL=C.utf8 TZ=Europe/Berlin" in logged
    assert "[Rscript] sourcing selectScenarios.R" in logged  # stdout noise before the payload
    assert not any(PAYLOAD_PREFIX in line for line in logged)


@needs_rscript
def test_mini_checkout_another_startgroup(tmp_path: Path) -> None:
    checkout = write_checkout(tmp_path / "remind")

    selection = select_scenarios(real_rscript(checkout), checkout, startgroup="1")

    assert selection.names == ("default-AMT", "SSP2-manual-AMT")
    assert selection.startgroup == "1"
    assert selection.row("SSP2-manual-AMT")["weight"] is None


@needs_rscript
def test_mini_checkout_duplicate_titles_fail_inside_read_csv2(
    tmp_path: Path, caplog: pytest.LogCaptureFixture
) -> None:
    csv = "title;start;description\nSSP2-NPi;AMT;one\nSSP2-NPi;AMT;two\n"
    checkout = write_checkout(tmp_path / "remind", csv=csv)
    caplog.set_level(logging.INFO, logger=LOGGER_NAME)

    with pytest.raises(BridgeError, match="exited with status 1"):
        select_scenarios(real_rscript(checkout), checkout)

    assert any("duplicate 'row.names' are not allowed" in line for line in messages(caplog))


@needs_rscript
def test_mini_checkout_a_broken_start_script_fails_the_step(
    tmp_path: Path, caplog: pytest.LogCaptureFixture
) -> None:
    checkout = write_checkout(tmp_path / "remind")
    (checkout / "scripts" / "start" / "broken.R").write_text(
        'stop("this start script is broken")\n', encoding="utf-8"
    )
    caplog.set_level(logging.INFO, logger=LOGGER_NAME)

    with pytest.raises(BridgeError, match="exited with status 1"):
        select_scenarios(real_rscript(checkout), checkout)

    assert any("this start script is broken" in line for line in messages(caplog))


@needs_rscript
def test_mini_checkout_an_error_inside_select_scenarios_fails_the_step(
    tmp_path: Path, caplog: pytest.LogCaptureFixture
) -> None:
    stub = 'selectScenarios <- function(settings, interactive, startgroup) stop("no group ", startgroup)\n'
    checkout = write_checkout(tmp_path / "remind", stub=stub)
    caplog.set_level(logging.INFO, logger=LOGGER_NAME)

    with pytest.raises(BridgeError, match="exited with status 1"):
        select_scenarios(real_rscript(checkout), checkout)

    assert any("no group AMT" in line for line in messages(caplog))


@needs_rscript
def test_mini_checkout_a_non_frame_result_fails_the_step(
    tmp_path: Path, caplog: pytest.LogCaptureFixture
) -> None:
    stub = "selectScenarios <- function(settings, interactive, startgroup) row.names(settings)\n"
    checkout = write_checkout(tmp_path / "remind", stub=stub)
    caplog.set_level(logging.INFO, logger=LOGGER_NAME)

    with pytest.raises(BridgeError, match="exited with status 1"):
        select_scenarios(real_rscript(checkout), checkout)

    assert any(
        "selectScenarios() returned a character, not a data.frame" in line for line in messages(caplog)
    )


@needs_rscript
def test_mini_checkout_without_a_csv_fails_the_step(tmp_path: Path, caplog: pytest.LogCaptureFixture) -> None:
    checkout = write_checkout(tmp_path / "remind")
    (checkout / "config" / "scenario_config.csv").unlink()
    caplog.set_level(logging.INFO, logger=LOGGER_NAME)

    with pytest.raises(BridgeError, match="exited with status 1"):
        select_scenarios(real_rscript(checkout), checkout)

    assert any("cannot open file 'config/scenario_config.csv'" in line for line in messages(caplog))


@needs_rscript
def test_mini_checkout_with_an_empty_start_directory_fails_the_step(
    tmp_path: Path, caplog: pytest.LogCaptureFixture
) -> None:
    checkout = write_checkout(tmp_path / "remind")
    (checkout / "scripts" / "start" / "selectScenarios.R").unlink()
    caplog.set_level(logging.INFO, logger=LOGGER_NAME)

    with pytest.raises(BridgeError, match="exited with status 1"):
        select_scenarios(real_rscript(checkout), checkout)

    assert any("no selectScenarios() is defined by scripts/start/*.R of" in line for line in messages(caplog))
