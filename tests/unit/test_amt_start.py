"""``modelstats.amt.start`` and ``modelstats.amt.context``: the REMIND start of an AMT cycle (``06`` 4.1).

The tests run :func:`modelstats.amt.start.start_cycle` with ``RecordingEffects`` on a copy of the fake REMIND
checkout of ``migration/harness/build_fake_remind.sh`` written below ``tmp_path`` (its files are reproduced
here so the unit tier needs no gitignored tree; one test checks the copy against the synthetic layer when
it is built) with the R state files of the ``remind-start`` cases (``../.testsstatus`` = ``next:start``), the
canned answers of ``migration/cases/amt/<case>/`` and the ``selectScenarios()`` bridge answered from the
pinned payload of ``tests/unit/test_amt_bridges.py`` (one test runs the real ``Rscript`` instead).

The R goldens ``migration/goldens/amt/remind-start*`` are the reference (``06`` 4.4 item 3): the subprocess
records of ``trace.jsonl`` (tool, argv, cwd) must equal the recorded ``git``/``Rscript``/``make`` calls in R's
order, minus the bridge's own ``git log -1`` and ``Rscript select_scenarios.R`` (additions), the ``sed``
records must equal the ``replace_in_file`` calls, ``effects.json`` the deleted folder and the untouched
``default.cfg`` digest, ``state/runcode_rds.json`` the written pattern and ``state/runsToStart_rds.json`` the
``runs_to_start`` of the state file. The allowed differences: ``.testsstatus`` and ``runsToStart.rds`` are no
longer written (``06`` 3.4), and a failing ``git`` or ``make`` leaves the state ``failed`` where R went on
(``06`` 4.2, D-24: the goldens of the two failure cases record ``status: ok``).
"""

from __future__ import annotations

import hashlib
import io
import json
import logging
import os
import re
import shlex
import shutil
from collections.abc import Callable, Iterator, Mapping, Sequence
from contextlib import contextmanager
from dataclasses import dataclass, replace
from datetime import datetime
from pathlib import Path
from typing import Any
from zoneinfo import ZoneInfo

import pytest

from modelstats.amt.bridges import BRIDGE_SCRIPT, BridgeError, bridge_script
from modelstats.amt.context import (
    DEFAULT_LOG_DIR,
    OUTPUT_DIR,
    AmtContext,
    AmtOptions,
    CycleFailedError,
    record_failure,
)
from modelstats.amt.logs import LOGGER_NAME, close_cycle_log, cycle_log, cycle_log_path
from modelstats.amt.notify import MATTERMOST_URL_VARIABLE, NEXT_STEPS
from modelstats.amt.start import (
    BUNDLE_ARGV,
    DEFAULT_CFG,
    FORCE_DOWNLOAD_FALSE,
    FORCE_DOWNLOAD_TRUE,
    GIT_PULL_ARGV,
    GIT_RESET_ARGV,
    MAKE_ARGV,
    START_ENVIRONMENT,
    TEST_ONE_REGI_ARGV,
    delete_empty_realization_folders,
    empty_realizations,
    module_files,
    realization_name,
    set_force_download,
    start_cycle,
    update_checkout,
)
from modelstats.amt.state import (
    AMT_STATE_FILE,
    R_NEXT_START,
    RUNCODE_FILE,
    AmtState,
    AmtStateError,
    Command,
    Completed,
    Cycle,
    LastError,
    Phase,
    StateStore,
    cycle_id,
    next_command,
    runcode_pattern,
    target_phase,
)
from modelstats.env import (
    DRY_RUN_COMMIT,
    CannedAmt,
    CommandFailedError,
    DryRunEffects,
    Effects,
    PathLike,
    ProductionEffects,
    RecordingEffects,
    TraceRecord,
)
from modelstats.rdata_io import read_rds, scalar
from tests.conftest import CASES, GOLDENS, SYNTHETIC
from tests.unit.test_amt_bridges import FAKE_COMMIT, FAKE_NAMES, FAKE_PAYLOAD, golden_runs_to_start
from tests.unit.test_amt_state import R_COMMIT, R_RUNCODE, write_r_files
from tests.unit.test_env import tree_digest

BERLIN = ZoneInfo("Europe/Berlin")
NOW = datetime(2026, 9, 30, 12, 0, 0, tzinfo=BERLIN)
TODAY = NOW.date()
USER = "pascalfu"
R_MYDIR = "/p/projects/remind/modeltests/remind"
"""The checkout the R oracle ran in (``case.json`` ``mydir`` minus the trailing slash; the trace cwd)."""
SUBPROCESS_TOOLS = frozenset({"git", "Rscript", "make"})
"""The trace records that stay subprocesses in the port; ``sed`` became ``replace_in_file`` (test_env)."""
WEBHOOK = "https://mattermost.example.org/hooks/fake-amt-token"
FAKE_CHECKOUT = SYNTHETIC / "p" / "projects" / "remind" / "modeltests" / "remind"
LOG_LINE = re.compile(r"^\d{4}-\d{2}-\d{2} \d{2}:\d{2}:\d{2} start +([A-Z]+) +(.*)$")
ALL_STEPS = ["git", "git", "git", "Rscript", "Rscript", "Rscript", "git", "git", "make"]
"""The subprocesses of a complete start: the bridge's two calls included, in order."""

needs_rscript = pytest.mark.skipif(shutil.which("Rscript") is None, reason="the bridge runs the real Rscript")
needs_fake_checkout = pytest.mark.skipif(
    not (FAKE_CHECKOUT / "config" / "default.cfg").is_file(),
    reason=f"synthetic layer not built: {FAKE_CHECKOUT} (migration/harness/build_fake_remind.sh)",
)


# ---------------------------------------------------------------------------------------------
# The fake REMIND checkout of build_fake_remind.sh, reproduced (write_file: each line plus a newline)
# ---------------------------------------------------------------------------------------------

FAKE_FILES: Mapping[str, str] = {
    "config/default.cfg": (
        "# fake REMIND config/default.cfg (build_fake_remind.sh); startRuns() edits the next-but-one line"
        " with sed -i\n"
        "cfg <- list()\n"
        'cfg$model_name <- "REMIND"\n'
        "cfg$force_download <- FALSE\n"
        'cfg$gms$optimization <- "nash"\n'
    ),
    "config/scenario_config.csv": (
        "# fake REMIND config/scenario_config.csv (build_fake_remind.sh): read.csv2(row.names = 1,"
        ' comment.char = "#")\n'
        "title;start;description\n"
        "SSP2-NPi;AMT;SSP2 with national policies\n"
        "default;AMT;default scenario\n"
        "SSP2-EU21-PkBudg650;AMT;peak budget 650\n"
        "SSP2-NPi2025-calibrate;AMT;CES calibration\n"
        "SSP3-NPi2025;AMT;SSP3 with national policies 2025\n"
        "SSP2-EcBudg500;AMT;end-of-century budget 500\n"
        "SSP2-EU21-NPi2025;AMT;EU21 national policies 2025\n"
        "testOneRegi;AMT;single region test\n"
        "SSP2-never;AMT;in the AMT group but never started\n"
        "SSP2-manual;1;not in the AMT group\n"
    ),
    "scripts/start/selectScenarios.R": (
        "# fake REMIND scripts/start/selectScenarios.R (build_fake_remind.sh): sourced by"
        " modeltests::startRuns()\n"
        '# Returns the rows of settings whose "start" column (split on ",", trimmed) contains startgroup;'
        " row names kept.\n"
        'selectScenarios <- function(settings, interactive = FALSE, startgroup = "1") {\n'
        '  keep <- vapply(strsplit(as.character(settings$start), ",", fixed = TRUE),\n'
        "                 function(s) startgroup %in% trimws(s), logical(1))\n"
        "  keep[is.na(keep)] <- FALSE\n"
        "  settings[keep, , drop = FALSE]\n"
        "}\n"
    ),
    "scripts/cs2/run_compareScenarios2.R": (
        "# fake REMIND scripts/cs2/run_compareScenarios2.R (existence only; the fake sbatch never runs it)\n"
    ),
    "modules/01_macro/module.gms": (
        "*** fake module.gms (build_fake_remind.sh)\n"
        '$include "./modules/01_macro/singleSectorGr/realization.gms"\n'
    ),
    "modules/01_macro/singleSectorGr/realization.gms": "*** fake realization.gms\n",
    "modules/01_macro/input/.keep": "\n",
    "modules/02_welfare/module.gms": (
        "*** fake module.gms (build_fake_remind.sh)\n"
        '$include "./modules/02_welfare/utilitarian/realization.gms"\n'
    ),
    "modules/02_welfare/utilitarian/realization.gms": "*** fake realization.gms\n",
    "magpie/.keep": "\n",
    "tests/.keep": "\n",
    "Makefile": "# fake REMIND Makefile (build_fake_remind.sh); the fake make never runs targets\n",
}
FAKE_EMPTY_DIRS = ("modules/01_macro/emptyReal",)
"""The empty realization folder ``deleteEmptyRealizationFolders()`` must unlink (``SYNTHETIC.md``)."""
EMPTY_REALIZATION = "modules/01_macro/emptyReal"


def write_fake_checkout(root: Path) -> Path:
    """The fake REMIND checkout below ``root`` (``<tmp>/modeltests/remind``, the R ``mydir``)."""
    for relative, text in FAKE_FILES.items():
        path = root / relative
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(text, encoding="utf-8")
    for relative in FAKE_EMPTY_DIRS:
        (root / relative).mkdir(parents=True)
    return root


def sha256(path: Path) -> str:
    return hashlib.sha256(path.read_bytes()).hexdigest()


# ---------------------------------------------------------------------------------------------
# The goldens
# ---------------------------------------------------------------------------------------------


def golden_dir(case: str) -> Path:
    return GOLDENS / "amt" / case


def golden_trace(case: str) -> list[dict[str, Any]]:
    """The records of the fake binaries' ``trace.jsonl``."""
    lines = (golden_dir(case) / "trace.jsonl").read_text(encoding="utf-8").splitlines()
    return [json.loads(line) for line in lines]


def golden_effects(case: str) -> dict[str, Any]:
    """The clone diff ``effects.json``."""
    return json.loads((golden_dir(case) / "effects.json").read_text(encoding="utf-8"))  # type: ignore[no-any-return]


def golden_runcode(case: str) -> str:
    """The pattern R's ``saveRDS(runcode, ...)`` wrote (``state/runcode_rds.json``)."""
    record = json.loads((golden_dir(case) / "state" / "runcode_rds.json").read_text(encoding="utf-8"))
    value = record["value"]
    assert isinstance(value, list) and len(value) == 1 and isinstance(value[0], str)
    return value[0]


def in_checkout(golden_path: str, checkout: Path) -> str:
    """A path of the R oracle's run (``/p/projects/remind/modeltests/remind...``) inside the test checkout."""
    assert golden_path.startswith(R_MYDIR), golden_path
    return os.fspath(checkout) + golden_path[len(R_MYDIR) :]


def golden_steps(case: str, checkout: Path) -> list[tuple[str, tuple[str, ...], str]]:
    """The subprocess records of the golden trace as ``(tool, argv, cwd)`` with the cwd mapped."""
    return [
        (r["tool"], tuple(r["argv"]), in_checkout(r["cwd"], checkout))
        for r in golden_trace(case)
        if r["tool"] in SUBPROCESS_TOOLS
    ]


def golden_sed_edits(case: str, checkout: Path) -> list[tuple[str, str, str]]:
    """The ``sed -i 's/old/new/' file`` records as ``replace_in_file`` arguments."""
    edits: list[tuple[str, str, str]] = []
    for r in golden_trace(case):
        if r["tool"] != "sed":
            continue
        flag, expression, file = r["argv"]
        assert flag == "-i"
        command, old, new, rest = expression.split("/")
        assert command == "s" and rest == ""
        edits.append((os.fspath(checkout / file), old, new))
    return edits


def golden_deleted(case: str, checkout: Path) -> list[str]:
    """The ``deleted`` entries of ``effects.json`` inside the test checkout."""
    diff = golden_effects(case)
    root = diff["fixture_root"]
    return [in_checkout(f"{root}/{entry['path']}", checkout) for entry in diff["deleted"]]


# ---------------------------------------------------------------------------------------------
# The harness: RecordingEffects on the checkout, the cycle log at the path the CLI would open
# ---------------------------------------------------------------------------------------------


def case_environ(case: str) -> dict[str, str]:
    """The variables the sandbox gave the R oracle, as the trace records them (``TRACE_ENV_KEYS``)."""
    return {
        "FAKETIME": "2026-09-30 12:00:00",
        "LC_ALL": "C.utf8",
        "RS_AMT_CASE": case,
        "RS_SLURM_CASE": "recorded",
        "TZ": "Europe/Berlin",
        "USER": USER,
    }


def canned_case(case: str, **extra: str) -> CannedAmt:
    """The case directory's canned answers plus ``extra`` files (``Rscript.txt`` cans the bridge)."""
    directory = CASES / "amt" / case
    files = {p.name: p.read_text(encoding="utf-8") for p in directory.iterdir() if p.is_file()}
    return CannedAmt({**files, **extra})


def recording(
    tmp_path: Path,
    case: str,
    *,
    bridge: str | None = FAKE_PAYLOAD,
    environ: Mapping[str, str] | None = None,
    fallback: Effects | None = None,
) -> RecordingEffects:
    """The AMT double for ``case``; ``bridge`` is the payload the canned ``Rscript`` prints for the bridge."""
    extra = {"Rscript.txt": bridge + "\n"} if bridge is not None else {}
    return RecordingEffects(
        now=NOW,
        user=USER,
        on_cluster=False,
        amt=canned_case(case, **extra),
        cwd=tmp_path,
        environ={**case_environ(case), **(environ or {})},
        fallback=fallback,
    )


@dataclass
class Harness:
    checkout: Path
    effects: RecordingEffects
    ctx: AmtContext
    log_path: Path

    def state(self) -> AmtState:
        loaded = self.ctx.store.load()
        assert loaded is not None
        return loaded

    def log(self) -> list[tuple[str, str]]:
        """``(level, message)`` of every single-line record of the cycle log."""
        lines = self.log_path.read_text(encoding="utf-8").splitlines()
        matched = [LOG_LINE.match(line) for line in lines]
        return [(m.group(1), m.group(2)) for m in matched if m is not None]

    def messages(self, level: str = "INFO") -> list[str]:
        return [message for lvl, message in self.log() if lvl == level]

    def steps(self) -> list[tuple[str, tuple[str, ...], str]]:
        """The recorded subprocesses without the bridge's two calls, as the golden records them."""
        return [(t.tool, t.argv, t.cwd) for t in self.effects.trace if not is_bridge_call(t)]

    def calls(self, name: str) -> list[tuple[str, ...]]:
        return [c.args for c in self.effects.calls if c.name == name]

    def deleted(self) -> list[str]:
        return [args[0] for args in self.calls("delete_tree")]

    def written(self) -> list[str]:
        return [c.args[0] for c in self.effects.calls if c.name in ("write_text", "write_bytes")]


def is_bridge_call(record: TraceRecord) -> bool:
    """``git log -1`` and ``Rscript select_scenarios.R`` belong to the bridge (not in the R start trace)."""
    if record.tool == "git":
        return record.argv == ("log", "-1")
    return record.tool == "Rscript" and bool(record.argv) and record.argv[0].endswith(BRIDGE_SCRIPT)


@contextmanager
def harness(
    tmp_path: Path,
    case: str = "remind-start",
    *,
    wrap: Callable[[RecordingEffects], Effects] | None = None,
    options: AmtOptions | None = None,
    bridge: str | None = FAKE_PAYLOAD,
    environ: Mapping[str, str] | None = None,
    fallback: Effects | None = None,
) -> Iterator[Harness]:
    """A checkout with the R state (``next:start``), the effects, the loaded state and an open cycle log.

    What the CLI does for ``start``: open the log, load (here: migrate) the state through the effects,
    build the context. ``wrap`` replaces the recorder by something built around it (a dry run).
    """
    checkout = write_fake_checkout(tmp_path / "modeltests" / "remind")
    write_r_files(checkout, tests_status=R_NEXT_START)
    recorder = recording(tmp_path, case, bridge=bridge, environ=environ, fallback=fallback)
    effects = wrap(recorder) if wrap is not None else recorder
    opts = options if options is not None else AmtOptions()
    log_dir = opts.log_dir if opts.log_dir is not None else checkout / DEFAULT_LOG_DIR
    log_path = cycle_log_path(log_dir, cycle_id(TODAY))
    with cycle_log(log_path, "start", clock=recorder.now, stream=io.StringIO()) as logger:
        state = StateStore.at(effects, checkout).load_or_migrate(logger.info)
        ctx = AmtContext(
            dir=checkout,
            report_repo=None,
            user=USER,
            options=opts,
            effects=effects,
            logger=logger,
            state=state,
        )
        yield Harness(checkout, recorder, ctx, log_path)


@pytest.fixture(autouse=True)
def clean_logger() -> Iterator[None]:
    yield
    close_cycle_log(logging.getLogger(LOGGER_NAME))


def expected_cycle(h: Harness, **changes: Any) -> Cycle:
    """The cycle ``start`` records for ``NOW`` on the fake checkout with the canned bridge."""
    cycle = Cycle(
        id=cycle_id(TODAY),
        runcode=runcode_pattern(TODAY),
        started_at=NOW,
        started_commit=FAKE_COMMIT,
        runs_to_start=FAKE_NAMES,
        log=str(h.log_path),
    )
    return Cycle(**{**cycle.__dict__, **changes})


MIGRATED_COMPLETED = Completed(tested_commit=R_COMMIT, runcode=R_RUNCODE)
"""What ``load_or_migrate`` derives from ``next:start`` plus the R files (``06`` 3.2)."""


# ---------------------------------------------------------------------------------------------
# remind-start: the kept steps against the R golden
# ---------------------------------------------------------------------------------------------


def test_remind_start_runs_the_kept_subprocess_steps_in_the_order_of_the_r_trace(tmp_path: Path) -> None:
    with harness(tmp_path) as h:
        start_cycle(h.ctx)
    assert h.steps() == golden_steps("remind-start", h.checkout)
    assert [(t.tool, t.argv[:1]) for t in h.effects.trace] == [
        ("git", ("reset",)),
        ("git", ("pull",)),
        ("git", ("log",)),  # the bridge: the tested commit
        ("Rscript", (h.effects.trace[3].argv[0],)),  # the bridge: select_scenarios.R
        ("Rscript", ("start.R",)),
        ("Rscript", ("start.R",)),
        ("git", ("reset",)),
        ("git", ("pull",)),
        ("make", ("test-full-slurm",)),
    ]
    assert h.effects.trace[3].argv[0].endswith(BRIDGE_SCRIPT) and h.effects.trace[3].argv[1:] == ("AMT",)
    assert h.effects.trace[3].cwd == os.fspath(h.checkout)
    assert h.effects.posts == []


def test_remind_start_argv_are_the_r_command_lines_verbatim(tmp_path: Path) -> None:
    """``06`` 4.1 quotes the two ``Rscript start.R`` lines and ``make test-full-slurm``: pinned here."""
    with harness(tmp_path) as h:
        start_cycle(h.ctx)
    argv = [
        (t.tool, *t.argv) for t in h.effects.trace if t.tool in ("Rscript", "make") and not is_bridge_call(t)
    ]
    assert argv == [TEST_ONE_REGI_ARGV, BUNDLE_ARGV, MAKE_ARGV]
    assert [(t.tool, *t.argv) for t in h.effects.trace if t.tool == "git" and not is_bridge_call(t)] == [
        GIT_RESET_ARGV,
        GIT_PULL_ARGV,
        GIT_RESET_ARGV,
        GIT_PULL_ARGV,
    ]


def test_remind_start_environment_equals_the_r_trace_for_rscript_and_make(tmp_path: Path) -> None:
    """``autoRenvFixDeps=TRUE`` (``R/modeltests.R:109``) reaches ``Rscript start.R`` and ``make``.

    R's ``Sys.setenv`` applied to the whole process, so its later ``git`` calls in ``magpie/`` carried the
    variable too; the port sets it for the two programs that read it, so the ``git`` records are compared
    without it.
    """
    with harness(tmp_path) as h:
        start_cycle(h.ctx)
    recorded = [t for t in h.effects.trace if not is_bridge_call(t)]
    golden = [r for r in golden_trace("remind-start") if r["tool"] in SUBPROCESS_TOOLS]
    for record, expected in zip(recorded, golden, strict=True):
        if record.tool in ("Rscript", "make"):
            assert dict(record.env) == expected["env"], record
            assert record.env["autoRenvFixDeps"] == START_ENVIRONMENT["autoRenvFixDeps"]
        else:
            golden_env = {k: v for k, v in expected["env"].items() if k != "autoRenvFixDeps"}
            assert dict(record.env) == golden_env, record


def test_remind_start_file_edits_equal_the_golden_clone_diff(tmp_path: Path) -> None:
    """The ``sed`` records, the deleted folder and the untouched ``default.cfg`` of ``effects.json``."""
    with harness(tmp_path) as h:
        before = {name: sha256(h.checkout / name) for name in ("runsToStart.rds", "lastcommit.rds")}
        tests_status_before = (h.checkout.parent / ".testsstatus").read_bytes()
        start_cycle(h.ctx)
    case = "remind-start"
    assert h.calls("replace_in_file") == golden_sed_edits(case, h.checkout)
    assert h.calls("replace_in_file") == [
        (os.fspath(h.checkout / DEFAULT_CFG), FORCE_DOWNLOAD_FALSE, FORCE_DOWNLOAD_TRUE),
        (os.fspath(h.checkout / DEFAULT_CFG), FORCE_DOWNLOAD_TRUE, FORCE_DOWNLOAD_FALSE),
    ]
    assert h.deleted() == golden_deleted(case, h.checkout)
    assert not (h.checkout / EMPTY_REALIZATION).exists()
    assert (h.checkout / "modules/01_macro/singleSectorGr/realization.gms").is_file()
    assert (h.checkout / "modules/01_macro/input/.keep").is_file()
    assert (h.checkout / "modules/02_welfare/utilitarian/realization.gms").is_file()
    (touched,) = golden_effects(case)["touched"]
    assert touched["path"].endswith("config/default.cfg")
    cfg = h.checkout / DEFAULT_CFG
    assert sha256(cfg) == touched["sha256"] and cfg.stat().st_size == touched["size"]
    # 06 3.4: the R toggle file and runsToStart.rds are left alone (R rewrote both), lastcommit.rds too
    assert (h.checkout.parent / ".testsstatus").read_bytes() == tests_status_before
    assert {name: sha256(h.checkout / name) for name in before} == before
    assert sorted(set(h.written())) == sorted(
        {os.fspath(h.ctx.state_path), os.fspath(h.checkout / RUNCODE_FILE)}
    )


def test_remind_start_writes_the_r_pattern_and_the_started_state(tmp_path: Path) -> None:
    """``runcode.rds`` holds the golden's pattern; the state file the cycle with the golden's run names."""
    with harness(tmp_path) as h:
        returned = start_cycle(h.ctx)
    assert scalar(read_rds(h.checkout / RUNCODE_FILE)) == golden_runcode("remind-start")
    names, _columns = golden_runs_to_start()
    state = h.state()
    assert state == returned
    assert state.phase is Phase.STARTED
    assert state.cycle == expected_cycle(h)
    assert state.cycle is not None and list(state.cycle.runs_to_start) == names
    assert state.last_completed == MIGRATED_COMPLETED
    assert state.last_error is None
    assert state.cycle.log == str(h.ctx.cycle_log_path(state.cycle.id)) == str(h.log_path)
    assert target_phase(state, Command.EVALUATE) is Phase.IDLE
    assert next_command(state) is Command.EVALUATE


def test_remind_start_log_tells_each_step(tmp_path: Path) -> None:
    with harness(tmp_path) as h:
        start_cycle(h.ctx)
    info = h.messages()
    checkout = os.fspath(h.checkout)
    expected_in_order = [
        "amt-state.json is absent; deriving the state from"
        f" {h.checkout.parent / '.testsstatus'} ('next:start')",
        f"Starting AMT cycle 2026-09-30 in {checkout} (run pattern {runcode_pattern(TODAY)})",
        f"running: git reset --hard origin/develop in {checkout}",
        f"running: git pull in {checkout}",
        f"[git] commit {FAKE_COMMIT}",
        'selectScenarios(startgroup = "AMT") selected 9 scenarios: ' + ", ".join(FAKE_NAMES),
        f"set '{FORCE_DOWNLOAD_TRUE}' in {h.checkout / DEFAULT_CFG} (1 line)",
        f"deleted {h.checkout / EMPTY_REALIZATION} (not a realization named in"
        f" {h.checkout / 'modules/01_macro/module.gms'})",
        "Configuring and starting single testOneRegi-AMT",
        f"running: {' '.join(TEST_ONE_REGI_ARGV[:3])} titletag=AMT 'slurmConfig=--qos=priority --nodes=1"
        f" --tasks-per-node=1 --wait --time=2:00:00' in {checkout}",
        "[Rscript] fake Rscript: start.R --testOneRegi titletag=AMT slurmConfig=--qos=priority --nodes=1"
        " --tasks-per-node=1 --wait --time=2:00:00",
        "Configuring and starting bundle of AMT runs",
        f"set '{FORCE_DOWNLOAD_FALSE}' in {h.checkout / DEFAULT_CFG} (1 line)",
        f"running: git reset --hard origin/develop in {h.checkout / 'magpie'}",
        "Submitting make test-full-slurm",
        f"running: make test-full-slurm in {checkout}",
        "[make] fake make: test-full-slurm",
        f"Function 'startRuns' finished: cycle 2026-09-30 started with 9 scenarios, state in"
        f" {h.ctx.state_path}, pattern in {h.checkout / RUNCODE_FILE}",
    ]
    positions = [info.index(message) for message in expected_in_order]
    assert positions == sorted(positions)
    assert [m for lvl, m in h.log() if lvl in ("WARNING", "ERROR")] == []


@needs_rscript
def test_remind_start_with_the_real_bridge_selects_the_golden_runs_to_start(tmp_path: Path) -> None:
    """The real ``Rscript`` runs ``select_scenarios.R`` on the written checkout (``git`` stays canned)."""
    with harness(tmp_path, bridge=None, fallback=ProductionEffects()) as h:
        state = start_cycle(h.ctx)
    names, _columns = golden_runs_to_start()
    assert state.cycle is not None and list(state.cycle.runs_to_start) == names
    assert state.cycle.started_commit == FAKE_COMMIT
    assert h.steps() == golden_steps("remind-start", h.checkout)


@needs_fake_checkout
def test_the_written_checkout_mirrors_the_synthetic_layer() -> None:
    """``FAKE_FILES`` is ``build_fake_remind.sh``'s checkout: a drift there must show up here."""
    for relative, text in FAKE_FILES.items():
        assert (FAKE_CHECKOUT / relative).read_text(encoding="utf-8") == text, relative
    for relative in FAKE_EMPTY_DIRS:
        assert (FAKE_CHECKOUT / relative).is_dir() and not any((FAKE_CHECKOUT / relative).iterdir())
    synthetic_modules = sorted(
        str(p.relative_to(FAKE_CHECKOUT)) for p in (FAKE_CHECKOUT / "modules").rglob("*")
    )
    written = sorted({*(r for r in FAKE_FILES if r.startswith("modules/")), *FAKE_EMPTY_DIRS})
    written_dirs = sorted(
        {str(p) for r in written for p in Path(r).parents if str(p) not in (".", "modules")}
    )
    assert synthetic_modules == sorted({*written, *written_dirs})


# ---------------------------------------------------------------------------------------------
# remind-start-git-fail and remind-start-make-fail: a failed infrastructure command (06 3.2, D-24)
# ---------------------------------------------------------------------------------------------


def test_git_fail_stops_at_the_reset_and_leaves_the_state_failed(tmp_path: Path) -> None:
    """R skipped only ``git pull`` and went on (golden ``status: ok``); here nothing else runs."""
    with harness(tmp_path, "remind-start-git-fail") as h:
        runcode_before = (h.checkout / RUNCODE_FILE).read_bytes()
        with pytest.raises(CycleFailedError) as raised:
            start_cycle(h.ctx)
    error = raised.value
    assert isinstance(error.__cause__, CommandFailedError)
    assert error.command is Command.START and error.exit_status == 1
    assert str(error) == (
        f"'start' failed: git reset --hard origin/develop exited with status 128; log {h.log_path};"
        " the AMT state is now 'failed'"
    )
    assert h.steps() == golden_steps("remind-start-git-fail", h.checkout)[:1]
    assert h.steps() == [("git", ("reset", "--hard", "origin/develop"), os.fspath(h.checkout))]
    assert h.calls("replace_in_file") == [] and h.deleted() == []
    assert (h.checkout / EMPTY_REALIZATION).is_dir()
    state = h.state()
    assert state == error.state
    assert state.phase is Phase.FAILED
    assert state.last_error == LastError(
        command="start",
        message="git reset --hard origin/develop exited with status 128",
        at=NOW,
        log=str(h.log_path),
    )
    assert state.cycle == expected_cycle(h, started_commit=None, runs_to_start=())
    assert state.last_completed == MIGRATED_COMPLETED
    assert (h.checkout / RUNCODE_FILE).read_bytes() == runcode_before
    assert h.written() == [os.fspath(h.ctx.state_path)] * 2  # the migration, then the failed state
    assert [m for lvl, m in h.log() if lvl == "ERROR"] == [
        "'start' failed: git reset --hard origin/develop exited with status 128",
        f"the AMT state is now 'failed' ({h.ctx.state_path}); the cycle is kept for a retry",
    ]
    assert "[git] fatal: not a git repository" in h.messages()
    assert "git exited with status 128" in h.messages("WARNING")


def test_make_fail_runs_every_step_and_leaves_the_state_failed_with_the_cycle(tmp_path: Path) -> None:
    """``Rscript start.R`` exits 1 twice (a warning each, as R tolerated it); ``make`` exits 2 and aborts."""
    with harness(tmp_path, "remind-start-make-fail") as h:
        runcode_before = (h.checkout / RUNCODE_FILE).read_bytes()
        with pytest.raises(CycleFailedError) as raised:
            start_cycle(h.ctx)
    assert h.steps() == golden_steps("remind-start-make-fail", h.checkout)
    assert h.calls("replace_in_file") == golden_sed_edits("remind-start-make-fail", h.checkout)
    assert h.deleted() == golden_deleted("remind-start-make-fail", h.checkout)
    assert h.messages("WARNING") == [
        "Rscript exited with status 1",
        "Rscript exited with status 1",
        "make exited with status 2",
    ]
    assert "[make] make: *** [Makefile:12: test-full-slurm] Error 2" in h.messages()
    state = h.state()
    assert state.phase is Phase.FAILED
    assert state.cycle == expected_cycle(h)  # the submitted runs are known: evaluate can retry
    assert state.last_error == LastError(
        command="start", message="make test-full-slurm exited with status 2", at=NOW, log=str(h.log_path)
    )
    assert (h.checkout / RUNCODE_FILE).read_bytes() == runcode_before
    assert str(raised.value).startswith("'start' failed: make test-full-slurm exited with status 2")
    assert target_phase(state, Command.EVALUATE) is Phase.IDLE
    with pytest.raises(AmtStateError, match="cannot 'run'"):
        next_command(state)


def test_a_failed_start_notifies_when_a_webhook_is_configured(tmp_path: Path) -> None:
    environ = {MATTERMOST_URL_VARIABLE: WEBHOOK}
    with harness(tmp_path, "remind-start-git-fail", environ=environ) as h, pytest.raises(CycleFailedError):
        start_cycle(h.ctx)
    (post,) = h.effects.posts
    assert post.url == WEBHOOK
    text = post.payload["text"] if isinstance(post.payload, dict) else None
    assert isinstance(text, str)
    assert text.splitlines() == [
        "The REMIND automated model tests (AMT) failed: `modeltests start` stopped with an error at"
        f" 2026-09-30 12:00:00. Runs are here: `{h.checkout}`.",
        "```",
        "git reset --hard origin/develop exited with status 128",
        "```",
        f"Log: `{h.log_path}`",
        NEXT_STEPS,
    ]
    assert WEBHOOK not in h.log_path.read_text(encoding="utf-8")


def test_a_failed_start_without_a_webhook_sends_nothing(tmp_path: Path) -> None:
    with harness(tmp_path, "remind-start-git-fail") as h, pytest.raises(CycleFailedError):
        start_cycle(h.ctx)
    assert h.effects.posts == []
    assert f"No Mattermost webhook URL in ${MATTERMOST_URL_VARIABLE}: notification not sent" in h.messages()


def test_a_bridge_failure_aborts_before_any_run_is_submitted(tmp_path: Path) -> None:
    """The selection precedes the ``Rscript start.R`` calls (module docstring, "Order")."""
    with harness(tmp_path, bridge="not json at all") as h, pytest.raises(CycleFailedError) as raised:
        start_cycle(h.ctx)
    assert isinstance(raised.value.__cause__, BridgeError)
    assert [(t.tool, t.argv[:1]) for t in h.effects.trace] == [
        ("git", ("reset",)),
        ("git", ("pull",)),
        ("git", ("log",)),
        ("Rscript", (h.effects.trace[3].argv[0],)),
    ]
    assert h.calls("replace_in_file") == [] and h.deleted() == []
    state = h.state()
    assert state.phase is Phase.FAILED
    assert state.cycle == expected_cycle(h, started_commit=None, runs_to_start=())
    assert state.last_error is not None
    assert state.last_error.message.startswith(f"{BRIDGE_SCRIPT} printed no JSON object")


def test_an_unexpected_error_inside_a_step_is_recorded_the_same_way(tmp_path: Path) -> None:
    """``06`` 3.2: any crash inside ``start`` ends in ``failed`` with the message, not in a traceback only."""

    class Broken(RecordingEffects):
        def delete_tree(self, path: object) -> None:
            raise PermissionError(f"cannot remove {path}")

    broken = Broken(
        now=NOW,
        user=USER,
        on_cluster=False,
        amt=canned_case("remind-start", **{"Rscript.txt": FAKE_PAYLOAD}),
        cwd=tmp_path,
    )
    with harness(tmp_path, wrap=lambda _recorder: broken) as h:
        ctx = h.ctx
        with pytest.raises(CycleFailedError) as raised:
            start_cycle(ctx)
        state = ctx.store.load()
    assert isinstance(raised.value.__cause__, PermissionError)
    assert state is not None and state.phase is Phase.FAILED
    assert state.last_error is not None
    assert state.last_error.message == f"cannot remove {h.checkout / EMPTY_REALIZATION}"
    assert (h.checkout / EMPTY_REALIZATION).is_dir()
    assert [t.tool for t in broken.trace] == ["git", "git", "git", "Rscript"]


class FailingWrites(RecordingEffects):
    """``write_text``/``write_bytes`` that raise for the state file / ``runcode.rds`` once ``armed``.

    The errors are taken from the front of the given lists (one per refused write) unless
    ``persistent``, when the first one is raised every time. Arming after the harness entered keeps its
    migration save out of the fault.
    """

    def __init__(
        self,
        *,
        text_errors: Sequence[BaseException] = (),
        bytes_errors: Sequence[BaseException] = (),
        persistent: bool = False,
        **kwargs: Any,
    ) -> None:
        super().__init__(**kwargs)
        self.armed = False
        self._text_errors = list(text_errors)
        self._bytes_errors = list(bytes_errors)
        self._persistent = persistent
        self.refused: list[str] = []

    def _refuse(self, path: PathLike, errors: list[BaseException], suffix: str) -> None:
        if not self.armed or not errors or not os.fspath(path).endswith(suffix):
            return
        error = errors[0] if self._persistent else errors.pop(0)
        self.refused.append(os.fspath(path))
        raise error

    def write_text(
        self, path: PathLike, text: str, *, encoding: str = "utf-8", errors: str = "surrogateescape"
    ) -> None:
        self._refuse(path, self._text_errors, AMT_STATE_FILE)
        super().write_text(path, text, encoding=encoding, errors=errors)

    def write_bytes(self, path: PathLike, data: bytes) -> None:
        self._refuse(path, self._bytes_errors, RUNCODE_FILE)
        super().write_bytes(path, data)


def failing_writes(tmp_path: Path, **faults: Any) -> FailingWrites:
    return FailingWrites(
        now=NOW,
        user=USER,
        on_cluster=False,
        amt=canned_case("remind-start", **{"Rscript.txt": FAKE_PAYLOAD}),
        cwd=tmp_path,
        environ={**case_environ("remind-start"), MATTERMOST_URL_VARIABLE: WEBHOOK},
        **faults,
    )


def assert_failed_after_submission(h: Harness, effects: FailingWrites, raised: CycleFailedError) -> AmtState:
    """The persist step failed after every run was submitted: ``failed`` with the full cycle, notified."""
    assert [t.tool for t in effects.trace] == ALL_STEPS
    state = h.ctx.store.load()
    assert state is not None and state == raised.state
    assert state.phase is Phase.FAILED
    assert state.cycle == expected_cycle(h)  # the submitted runs are known: evaluate can retry
    assert state.last_error is not None and state.last_error.command == "start"
    assert state.last_error.at == NOW and state.last_error.log == str(h.log_path)
    with pytest.raises(AmtStateError, match="cannot 'start': the AMT state is 'failed'"):
        target_phase(state, Command.START)  # no second submission on top of the running one
    assert target_phase(state, Command.EVALUATE) is Phase.IDLE
    (post,) = effects.posts
    assert post.url == WEBHOOK
    assert (
        isinstance(post.payload, dict) and "`modeltests start` stopped with an error" in post.payload["text"]
    )
    return state


def test_a_failed_state_file_write_after_submission_leaves_the_state_failed(tmp_path: Path) -> None:
    """A transient ``OSError`` of the ``amt-state.json`` write: ``failed``, not ``idle`` with a traceback."""
    effects = failing_writes(tmp_path, text_errors=[OSError(28, "No space left on device")])
    with harness(tmp_path, wrap=lambda _recorder: effects) as h:
        effects.armed = True
        with pytest.raises(CycleFailedError) as raised:
            start_cycle(h.ctx)
    assert isinstance(raised.value.__cause__, OSError) and raised.value.__cause__.errno == 28
    state = assert_failed_after_submission(h, effects, raised.value)
    assert effects.refused == [os.fspath(h.ctx.state_path)]
    assert state.last_error is not None and state.last_error.message == "[Errno 28] No space left on device"
    assert str(raised.value).startswith("'start' failed: [Errno 28] No space left on device")
    assert scalar(read_rds(h.checkout / RUNCODE_FILE)) == R_RUNCODE  # the pattern was not written either


def test_a_failed_runcode_rds_write_leaves_the_state_failed_with_the_cycle(tmp_path: Path) -> None:
    """The JSON was written as ``started``; the ``runcode.rds`` failure rewrites it as ``failed``."""
    effects = failing_writes(tmp_path, bytes_errors=[PermissionError("runcode.rds: Permission denied")])
    with harness(tmp_path, wrap=lambda _recorder: effects) as h:
        runcode_before = (h.checkout / RUNCODE_FILE).read_bytes()
        effects.armed = True
        with pytest.raises(CycleFailedError) as raised:
            start_cycle(h.ctx)
    assert isinstance(raised.value.__cause__, PermissionError)
    state = assert_failed_after_submission(h, effects, raised.value)
    assert effects.refused == [os.fspath(h.checkout / RUNCODE_FILE)]
    assert state.last_error is not None and state.last_error.message == "runcode.rds: Permission denied"
    assert (h.checkout / RUNCODE_FILE).read_bytes() == runcode_before  # rs-r -A reads the stale pattern
    assert h.ctx.state_path.read_text(encoding="utf-8") == state.to_json()


def test_a_disk_that_refuses_every_state_write_still_notifies(tmp_path: Path) -> None:
    """When even the ``failed`` state cannot be saved, the notification goes out and the error escapes."""
    effects = failing_writes(tmp_path, text_errors=[OSError(30, "Read-only file system")], persistent=True)
    with harness(tmp_path, wrap=lambda _recorder: effects) as h:
        effects.armed = True
        with pytest.raises(OSError, match="Read-only file system"):
            start_cycle(h.ctx)
        state = h.ctx.store.load()
    assert [t.tool for t in effects.trace] == ALL_STEPS
    assert effects.refused == [os.fspath(h.ctx.state_path)] * 2  # save_started, then the failed state
    assert state is not None and state.phase is Phase.IDLE  # the migrated state is what the disk kept
    (post,) = effects.posts
    assert (
        isinstance(post.payload, dict) and "`modeltests start` stopped with an error" in post.payload["text"]
    )
    errors = [m for lvl, m in h.log() if lvl == "ERROR"]
    assert errors[0] == "'start' failed: [Errno 30] Read-only file system"
    assert errors[1].startswith(f"cannot save the 'failed' state to {h.ctx.state_path}: ")


# ---------------------------------------------------------------------------------------------
# The transitions start may and may not make (06 3.2)
# ---------------------------------------------------------------------------------------------


def started_state(h: Harness) -> AmtState:
    """A cycle of yesterday still ``started``: the previous evaluate never ran."""
    old = Cycle(
        id="2026-09-29",
        runcode=".*-AMT_2026-09-29|.*-AMT_2026-09-30",
        started_at=datetime(2026, 9, 29, 12, 0, 0, tzinfo=BERLIN),
        started_commit="0" * 40,
        runs_to_start=("SSP2-NPi-AMT",),
        log=str(h.ctx.cycle_log_path("2026-09-29")),
    )
    return AmtState(phase=Phase.STARTED, cycle=old, last_completed=MIGRATED_COMPLETED)


def test_started_without_force_is_refused_before_any_effect(tmp_path: Path) -> None:
    with harness(tmp_path) as h:
        before = started_state(h)
        h.ctx.store.save_started(before)
        digest = tree_digest(h.checkout.parent)
        h.effects.calls.clear()
        with pytest.raises(AmtStateError, match="cannot 'start': a cycle is 'started' \\(cycle 2026-09-29"):
            start_cycle(replace(h.ctx, state=before))
    assert h.effects.trace == []
    assert h.written() == []
    assert tree_digest(h.checkout.parent) == digest
    assert h.state() == before


def test_started_with_force_abandons_the_old_cycle_and_starts_a_new_one(tmp_path: Path) -> None:
    with harness(tmp_path, options=AmtOptions(force=True)) as h:
        before = started_state(h)
        h.ctx.store.save_started(before)
        state = start_cycle(replace(h.ctx, state=before))
    assert h.steps() == golden_steps("remind-start", h.checkout)
    assert state.phase is Phase.STARTED
    assert state.cycle == expected_cycle(h)
    assert state.last_error is not None
    assert state.last_error.command == "start" and state.last_error.at == NOW
    assert (
        "cycle 2026-09-29" in state.last_error.message
        and "abandoned by 'start --force'" in state.last_error.message
    )
    assert state.last_error.log == str(h.ctx.cycle_log_path("2026-09-29"))
    assert scalar(read_rds(h.checkout / RUNCODE_FILE)) == runcode_pattern(TODAY)


def test_failed_is_refused_even_with_force(tmp_path: Path) -> None:
    with harness(tmp_path, options=AmtOptions(force=True)) as h:
        failed = AmtState(
            phase=Phase.FAILED,
            cycle=started_state(h).cycle,
            last_error=LastError(command="evaluate", message="boom", at=NOW),
        )
        h.ctx.store.save(failed)
        with pytest.raises(AmtStateError, match="cannot 'start': the AMT state is 'failed'"):
            start_cycle(replace(h.ctx, state=failed))
    assert h.effects.trace == []
    assert h.state() == failed


def test_the_context_state_is_the_starting_point_and_the_store_is_not_read_again(tmp_path: Path) -> None:
    with harness(tmp_path) as h:
        h.effects.calls.clear()
        state = start_cycle(replace(h.ctx, state=AmtState()))
    reads = [c.args[0] for c in h.effects.calls if c.name == "read_text"]
    assert os.fspath(h.ctx.state_path) not in reads  # the module files are read, the state file is not
    assert state.last_completed is None  # the context's fresh state, not the migrated one of the store
    assert h.state() == state


# ---------------------------------------------------------------------------------------------
# --dry-run (06 4.4 item 1): nothing written, every mutation announced, no program run (the bridge included)
# ---------------------------------------------------------------------------------------------


def dry_run_options(tmp_path: Path) -> AmtOptions:
    """The cycle log is written by the logging handler, not by an effect: keep it out of the digested tree."""
    return AmtOptions(dry_run=True, log_dir=tmp_path / "dry-run-logs")


def bridge_command(checkout: Path) -> str:
    """The announced bridge call: ``Rscript <package path>/select_scenarios.R AMT in <checkout>``."""
    with bridge_script() as script:
        return f"would run {shlex.join(['Rscript', str(script), 'AMT'])} in {checkout}"


def test_dry_run_writes_nothing_and_announces_every_mutation(tmp_path: Path) -> None:
    messages: list[str] = []
    digests: list[str] = []
    checkout_dir = tmp_path / "modeltests" / "remind"

    def dry_run(recorder: RecordingEffects) -> Effects:
        digests.append(tree_digest(checkout_dir.parent))  # before the migration, the first dry mutation
        return DryRunEffects(recorder, log=messages.append)

    with harness(tmp_path, options=dry_run_options(tmp_path), wrap=dry_run) as h:
        ctx = h.ctx
        (before,) = digests
        state = start_cycle(ctx)
    assert tree_digest(h.checkout.parent) == before
    assert not ctx.state_path.exists()
    assert state.phase is Phase.STARTED
    assert state.cycle == expected_cycle(h, started_commit=DRY_RUN_COMMIT, runs_to_start=())
    checkout = os.fspath(h.checkout)
    assert messages == [
        f"would write {ctx.state_path}",  # the migrated state
        f"would run git reset --hard origin/develop in {checkout}",
        f"would run git pull in {checkout}",
        f"would run git log -1 in {checkout}",
        bridge_command(h.checkout),
        f"would replace '{FORCE_DOWNLOAD_FALSE}' by '{FORCE_DOWNLOAD_TRUE}' in {h.checkout / DEFAULT_CFG}",
        f"would delete {h.checkout / EMPTY_REALIZATION}",
        f"would run Rscript start.R --testOneRegi titletag=AMT 'slurmConfig=--qos=priority --nodes=1"
        f" --tasks-per-node=1 --wait --time=2:00:00' in {checkout}",
        f"would replace '{FORCE_DOWNLOAD_TRUE}' by '{FORCE_DOWNLOAD_FALSE}' in {h.checkout / DEFAULT_CFG}",
        f"would run Rscript start.R startgroup=AMT titletag=AMT 'slurmConfig=--qos=standby --nodes=1"
        f" --tasks-per-node=12 --time=36:00:00' config/scenario_config.csv in {checkout}",
        f"would run git reset --hard origin/develop in {h.checkout / 'magpie'}",
        f"would run git pull in {h.checkout / 'magpie'}",
        f"would run make test-full-slurm in {checkout}",
        f"would write {ctx.state_path}",
        f"would write {h.checkout / RUNCODE_FILE}",
    ]
    # no program ran, the bridge included: the dry run answers it with an empty selection
    assert h.effects.trace == []
    assert any(m.endswith(", dry run)") for m in h.messages())
    assert 'selectScenarios(startgroup = "AMT") selected 0 scenarios: (none)' in h.messages()
    assert (
        "dry run: the selectScenarios() bridge was not run (it executes the checkout's scripts/start/*.R);"
        " the cycle records no scenario names"
    ) in h.messages()


def test_dry_run_never_executes_the_checkout_s_r_code(tmp_path: Path) -> None:
    """The bridge sources ``.Rprofile`` and ``scripts/start/*.R`` of the checkout: code, not a read.

    A checkout whose profile and start script each write a file would do so under a real ``Rscript``
    (the recorder's production fallback would run it); under ``--dry-run`` neither file appears.
    """
    checkout_dir = tmp_path / "modeltests" / "remind"
    leaks = {
        ".Rprofile": 'writeLines("profile ran during dry run", "leak-from-Rprofile.txt")\n',
        "scripts/start/zz_leak.R": (
            'writeLines("start script ran during dry run", "leak-from-start-script.txt")\n'
        ),
    }
    messages: list[str] = []
    digests: list[str] = []

    def dry_run(recorder: RecordingEffects) -> Effects:
        for relative, code in leaks.items():
            (checkout_dir / relative).parent.mkdir(parents=True, exist_ok=True)
            (checkout_dir / relative).write_text(code, encoding="utf-8")
        digests.append(tree_digest(checkout_dir.parent))
        return DryRunEffects(recorder, log=messages.append)

    options = dry_run_options(tmp_path)
    with harness(tmp_path, options=options, wrap=dry_run, bridge=None, fallback=ProductionEffects()) as h:
        state = start_cycle(h.ctx)
    (before,) = digests
    assert tree_digest(h.checkout.parent) == before
    assert not (h.checkout / "leak-from-Rprofile.txt").exists()
    assert not (h.checkout / "leak-from-start-script.txt").exists()
    assert h.effects.trace == []  # neither the recorder nor its production fallback ran a program
    assert [m for m in messages if m.startswith("would run Rscript ")] == [
        bridge_command(h.checkout),
        f"would run Rscript start.R --testOneRegi titletag=AMT 'slurmConfig=--qos=priority --nodes=1"
        f" --tasks-per-node=1 --wait --time=2:00:00' in {h.checkout}",
        f"would run Rscript start.R startgroup=AMT titletag=AMT 'slurmConfig=--qos=standby --nodes=1"
        f" --tasks-per-node=12 --time=36:00:00' config/scenario_config.csv in {h.checkout}",
    ]
    assert state.cycle == expected_cycle(h, started_commit=DRY_RUN_COMMIT, runs_to_start=())


# ---------------------------------------------------------------------------------------------
# The steps on their own
# ---------------------------------------------------------------------------------------------


def test_update_checkout_runs_pull_only_after_a_successful_reset(tmp_path: Path) -> None:
    with harness(tmp_path, "remind-start-git-fail") as h, pytest.raises(CommandFailedError) as raised:
        update_checkout(h.ctx, h.checkout)
    assert raised.value.result.returncode == 128
    assert [t.argv for t in h.effects.trace] == [("reset", "--hard", "origin/develop")]
    with harness(tmp_path / "ok") as h:
        update_checkout(h.ctx, h.checkout / "magpie")
    assert [(t.argv, t.cwd) for t in h.effects.trace] == [
        (("reset", "--hard", "origin/develop"), os.fspath(h.checkout / "magpie")),
        (("pull",), os.fspath(h.checkout / "magpie")),
    ]


def test_set_force_download_edits_the_line_both_ways_and_warns_when_absent(tmp_path: Path) -> None:
    with harness(tmp_path) as h:
        cfg = h.checkout / DEFAULT_CFG
        assert set_force_download(h.ctx, enabled=True) == 1
        assert FORCE_DOWNLOAD_TRUE in cfg.read_text() and FORCE_DOWNLOAD_FALSE not in cfg.read_text()
        assert set_force_download(h.ctx, enabled=True) == 0
        assert set_force_download(h.ctx, enabled=False) == 1
        assert cfg.read_text(encoding="utf-8") == FAKE_FILES["config/default.cfg"]
    assert h.messages("WARNING") == [f"no line '{FORCE_DOWNLOAD_FALSE}' in {cfg}: nothing replaced"]


def test_set_force_download_fails_without_the_file(tmp_path: Path) -> None:
    with harness(tmp_path) as h:
        (h.checkout / DEFAULT_CFG).unlink()
        with pytest.raises(FileNotFoundError):
            set_force_download(h.ctx, enabled=True)


@pytest.mark.parametrize(
    ("line", "name"),
    [
        ('$include "./modules/01_macro/singleSectorGr/realization.gms"', "singleSectorGr"),
        ('$include "./modules/30_biomass/magpie_40/realization.gms"', "magpie_40"),
        ('$include "modules/02_welfare/utilitarian/realization.gms"', "utilitarian"),
        # no "modules/<name>/" of the expected shape: the first sub leaves the line, the second strips the end
        ('$include "./modules/01-macro/x/realization.gms"', '$include "./modules/01-macro/x'),
        ("*** see realization.gms", "*** see realization.gms"),
    ],
)
def test_realization_name_is_the_two_r_substitutions(line: str, name: str) -> None:
    assert realization_name(line) == name


def test_empty_realizations_is_the_setdiff_of_r(tmp_path: Path) -> None:
    """``dir()`` order and no dot names; ``module.gms``, ``input`` and the named realizations are kept."""
    module_dir = tmp_path / "modules" / "01_macro"
    for entry in ("singleSectorGr", "input", "emptyReal", "zz_other", "Another"):
        (module_dir / entry).mkdir(parents=True)
    (module_dir / "README.md").write_text("a stray file\n")
    (module_dir / ".hidden").mkdir()
    (module_dir / "module.gms").write_text(FAKE_FILES["modules/01_macro/module.gms"])
    effects = RecordingEffects(now=NOW, user=USER, on_cluster=False, cwd=tmp_path)
    assert empty_realizations(effects, module_dir / "module.gms") == [
        "Another",
        "emptyReal",
        "README.md",
        "zz_other",
    ]


def test_module_files_descends_like_find_and_tolerates_a_missing_modules_dir(tmp_path: Path) -> None:
    modules = tmp_path / "modules"
    for relative in ("a/module.gms", "a/sub/module.gms", "b/.dot/module.gms", "c/realization.gms"):
        path = modules / relative
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text("")
    (modules / "d" / "module.gms").mkdir(parents=True)  # a directory of that name is not a module file
    effects = RecordingEffects(now=NOW, user=USER, on_cluster=False, cwd=tmp_path)
    assert module_files(effects, modules) == [
        modules / "a" / "module.gms",
        modules / "a" / "sub" / "module.gms",
        modules / "b" / ".dot" / "module.gms",
    ]
    assert module_files(effects, tmp_path / "missing") == []


def test_module_files_does_not_follow_a_symbolic_link_like_find_p(tmp_path: Path) -> None:
    """``find modules/ -name module.gms`` (``-P``) never enters a link below ``modules/``; ``list.dirs`` does.

    The walk is depth first, so the link inside ``a/`` is seen before the one in ``modules/``.
    """
    modules = tmp_path / "modules"
    (modules / "a").mkdir(parents=True)
    (modules / "a" / "module.gms").write_text("")
    outside = tmp_path / "outside" / "module"
    outside.mkdir(parents=True)
    (outside / "module.gms").write_text("")
    (modules / "external").symlink_to(outside)
    (modules / "a" / "link").symlink_to(outside)
    effects = RecordingEffects(now=NOW, user=USER, on_cluster=False, cwd=tmp_path)
    assert effects.list_dirs_like_r(modules, full_names=False) == ["a", "external"]  # R stats through it
    assert module_files(effects, modules) == [modules / "a" / "module.gms"]
    assert [c.args[0] for c in effects.calls if c.name == "is_symlink"] == [
        os.fspath(modules / "a"),
        os.fspath(modules / "a" / "link"),
        os.fspath(modules / "external"),
    ]


def test_delete_empty_realization_folders_removes_files_too_and_logs_each(tmp_path: Path) -> None:
    """Parity with ``unlink(recursive = TRUE)`` on ``setdiff(dir(...))``: a stray file goes as well."""
    with harness(tmp_path) as h:
        stray = h.checkout / "modules" / "02_welfare" / "notes.txt"
        stray.write_text("left behind\n")
        nested = h.checkout / "modules" / "02_welfare" / "oldReal" / "deep"
        nested.mkdir(parents=True)
        removed = delete_empty_realization_folders(h.ctx)
    assert removed == [
        h.checkout / EMPTY_REALIZATION,
        h.checkout / "modules" / "02_welfare" / "notes.txt",
        h.checkout / "modules" / "02_welfare" / "oldReal",
    ]
    assert not stray.exists() and not nested.exists()
    assert (h.checkout / "modules" / "02_welfare" / "utilitarian" / "realization.gms").is_file()
    assert [m for m in h.messages() if m.startswith("deleted ")] == [
        f"deleted {path} (not a realization named in {path.parent / 'module.gms'})" for path in removed
    ]
    with harness(tmp_path / "clean") as h:
        shutil.rmtree(h.checkout / EMPTY_REALIZATION)
        assert delete_empty_realization_folders(h.ctx) == []
    assert f"no empty realization folders below {h.checkout / 'modules'}" in h.messages()


def test_delete_empty_realization_folders_never_reaches_behind_a_symbolic_link(tmp_path: Path) -> None:
    """A module directory reachable only through a link is not swept: nothing outside ``--dir`` is removed.

    A link inside a module directory that no ``realization.gms`` line names is unlinked as an entry
    (``unlink`` removes the link, not its target), as R did.
    """
    outside = tmp_path / "outside" / "module"
    (outside / "stray").mkdir(parents=True)
    (outside / "module.gms").write_text('$include "./modules/external/realA/realization.gms"\n')
    with harness(tmp_path) as h:
        (h.checkout / "modules" / "external").symlink_to(outside)
        removed = delete_empty_realization_folders(h.ctx)
    assert removed == [h.checkout / EMPTY_REALIZATION]
    assert (outside / "stray").is_dir() and (outside / "module.gms").is_file()
    assert (h.checkout / "modules" / "external").is_symlink()
    assert all("outside" not in path for path in h.deleted())
    target = tmp_path / "target"
    (target / "keep").mkdir(parents=True)
    with harness(tmp_path / "linked") as h:
        link = h.checkout / "modules" / "02_welfare" / "oldReal"
        link.symlink_to(target)
        removed = delete_empty_realization_folders(h.ctx)
    assert removed == [h.checkout / EMPTY_REALIZATION, link]
    assert not link.exists() and not link.is_symlink()
    assert (target / "keep").is_dir()


# ---------------------------------------------------------------------------------------------
# AmtContext and record_failure (context.py)
# ---------------------------------------------------------------------------------------------


def test_context_derives_the_store_the_state_file_the_output_and_the_log_location(tmp_path: Path) -> None:
    with harness(tmp_path) as h:
        ctx = h.ctx
        assert ctx.state_path == h.checkout / AMT_STATE_FILE
        assert os.fspath(ctx.store.path) == os.fspath(h.checkout / AMT_STATE_FILE)
        assert ctx.store.effects is h.effects
        assert ctx.output_dir == h.checkout / OUTPUT_DIR
        assert ctx.log_dir == h.checkout / DEFAULT_LOG_DIR
        assert ctx.cycle_log_path("2026-09-30") == h.checkout / DEFAULT_LOG_DIR / "amt-2026-09-30.log"
        elsewhere = AmtContext(
            dir=h.checkout,
            report_repo=tmp_path / "testing_suite",
            user=USER,
            options=AmtOptions(log_dir=tmp_path / "logs"),
            effects=h.effects,
            logger=h.ctx.logger,
            state=h.ctx.state,
        )
        assert elsewhere.log_dir == tmp_path / "logs"
        assert elsewhere.cycle_log_path("x") == tmp_path / "logs" / "amt-x.log"
        assert elsewhere.report_repo == tmp_path / "testing_suite"


def test_context_satisfies_the_evaluate_units_protocol(tmp_path: Path) -> None:
    """``modelstats.amt.evaluate.EvaluateContext`` reads ``checkout``, the flags, ``store`` and ``state``."""
    from modelstats.amt.evaluate import EvaluateContext

    with harness(tmp_path) as h:
        ctx: EvaluateContext = h.ctx  # structural: mypy checks every member of the protocol
        assert ctx.checkout == h.ctx.dir == h.checkout
        assert ctx.publish is True and ctx.compare_scenarios is True
        assert ctx.mattermost_variable == MATTERMOST_URL_VARIABLE
        assert ctx.state == h.ctx.state and ctx.state.phase is Phase.IDLE
        assert ctx.state.last_completed == MIGRATED_COMPLETED
        quiet = replace(
            h.ctx, options=AmtOptions(publish=False, compare_scenarios=False, mattermost_url_variable="HOOK")
        )
        assert (quiet.publish, quiet.compare_scenarios, quiet.mattermost_variable) == (False, False, "HOOK")


def test_options_default_to_06_3_1() -> None:
    options = AmtOptions()
    assert options == AmtOptions(
        publish=True,
        compare_scenarios=True,
        mattermost_url_variable=MATTERMOST_URL_VARIABLE,
        log_dir=None,
        dry_run=False,
        force=False,
    )


def test_record_failure_from_a_started_state_keeps_the_running_cycle(tmp_path: Path) -> None:
    """The ``evaluate`` side of the shared row: no cycle argument, the state's cycle and its log are kept."""
    with harness(tmp_path, environ={MATTERMOST_URL_VARIABLE: WEBHOOK}) as h:
        state = started_state(h)
        h.ctx.store.save_started(state)
        ctx = replace(h.ctx, state=state)
        error = record_failure(ctx, Command.EVALUATE, RuntimeError("squeue had exit status > 0"))
        saved = h.state()
    assert isinstance(error, CycleFailedError) and error.state == saved
    assert saved.phase is Phase.FAILED and saved.cycle == state.cycle
    assert saved.last_error == LastError(
        command="evaluate",
        message="squeue had exit status > 0",
        at=NOW,
        log=state.cycle.log if state.cycle else None,
    )
    assert str(error) == (
        f"'evaluate' failed: squeue had exit status > 0; log {saved.last_error.log};"
        " the AMT state is now 'failed'"
    )
    (post,) = h.effects.posts
    assert (
        isinstance(post.payload, dict)
        and "`modeltests evaluate` stopped with an error" in post.payload["text"]
    )
    assert [m for lvl, m in h.log() if lvl == "ERROR"][0] == "'evaluate' failed: squeue had exit status > 0"


def test_record_failure_without_a_message_uses_the_repr(tmp_path: Path) -> None:
    with harness(tmp_path) as h:
        error = record_failure(replace(h.ctx, state=AmtState()), Command.START, KeyError())
    assert error.state.last_error is not None and error.state.last_error.message == "KeyError()"
    assert error.state.last_error.log is None
    assert str(error) == "'start' failed: KeyError(); the AMT state is now 'failed'"
