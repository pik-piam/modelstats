"""amt.evaluate: ``evaluateRuns`` of ``R/modeltests.R`` against R 4.6.1 facts and the AMT goldens.

Two groups:

- synthetic tests (always run): a small AMT tree in ``tmp_path`` evaluated with :class:`RecordingEffects`, the
  status reader, ``config.Rdata`` and ``runstatistics.rda`` replaced by canned records through the module seams
  (``evaluate.get_run_status``, ``evaluate.load_config``, ``evaluate.read_rda``), checking every branch statement
  by statement: the squeue wait loop and its failure counter, the ``git log -1`` subscript error, the gRS ``try()``
  and its fallback, the error-list rules with R's NA logic, the converged branch (rsync, lastRun by collation, the
  archive fallback, BUG-014's ``NA`` run, the 1.25 runtime factor, the cs2 command line and its cwd), the
  runsNotStarted block (BUG-024), the test-full.log evaluation and rename, the 90-day archive sweep (BUG-033), the
  e-mail step and the notification, and that every failure leaves ``.testsstatus`` at the "running" text and
  ``lastcommit.rds`` untouched;
- golden-backed tests (skipped without ``migration/fixtures`` or ``Rscript``): the representative cases
  remind-evaluate, remind-evaluate-nocompscen, remind-evaluate-slow, remind-evaluate-archive,
  remind-evaluate-git-fail and magpie-evaluate-recent run against a copy of the sandbox's fixture view (the
  synthetic layer under the fixtures, the case's ``prepare.sh`` applied, the results archive linked read-only)
  with a RecordingEffects that answers the fake binaries' canned output, and compared with the R goldens the way
  the stage-3 comparator will: README bytes, ``.testsstatus``, the state files by value (gRS rows by run name,
  because the directory mtimes that order a fresh ``getRunStatus(dir())`` are not those of the sandbox overlay),
  the Mattermost payload text, the (tool, argv, cwd) multiset of the traced commands (curl replaced by the HTTP
  effect, grep dropped), the changed paths of the tree, the ``unchanged.json`` flags, stdout and the stderr lines
  of ``evaluateRuns``. The one normalisation: the temporary tree root stands for the sandbox's ``/p`` in every
  text (``mydir`` is pasted into the README and the message, ``normalizePath`` into cs2com and the messages).

R facts pinned here (``LC_ALL=C.utf8 Rscript``, 2026-10-01): see the module docstring of ``amt/evaluate.py``.
"""

from __future__ import annotations

import collections
import dataclasses
import datetime as dt
import hashlib
import json
import math
import os
import re
import shutil
import subprocess
import tempfile
import warnings
import zoneinfo
from collections.abc import Callable, Iterator, Mapping, Sequence
from pathlib import Path

import pandas as pd
import pytest
from _fake_effects import FakeEffects, RecordingEffects

from modelstats import colors, run_status, sanity
from modelstats.amt import evaluate as ev
from modelstats.amt import readme as rm
from modelstats.amt import state
from modelstats.amt.notify import payload_text
from modelstats.env import FileStat, PathLike, shell_segments, shell_tokens
from modelstats.errors import RParityError, RWarning
from modelstats.rdata_io import RTime, read_rds, scalar
from modelstats.run_status import StatusTable

REPO = Path(__file__).resolve().parents[2]
MIGRATION = REPO / "migration"
FIXTURES = MIGRATION / "fixtures" / "p" / "projects"
SYNTHETIC = MIGRATION / "synthetic" / "p" / "projects"
AMT_CASES = MIGRATION / "cases" / "amt"
SLURM_CASES = MIGRATION / "cases" / "slurm"
RECORDED_SLURM = MIGRATION / "fixtures" / "_meta" / "slurm" / "latest"
AMT_GOLDENS = MIGRATION / "goldens" / "amt"

BERLIN = zoneinfo.ZoneInfo("Europe/Berlin")
FROZEN = dt.datetime(2026, 9, 30, 12, 0, 0, tzinfo=BERLIN)
COMMIT = "3f5e2a1b9c8d7e6f5a4b3c2d1e0f9a8b7c6d5e4f"
LAST_COMMIT = "13f60fdd4fff366bebcc7dfe522897dedeb43cc7"
MERGE_LINES = (
    "3f5e2a1 Merge pull request #999 from example/feature\n1a2b3c4 Merge pull request #998 from example/bugfix\n"
)
GIT_LOG_1 = (
    f"commit {COMMIT}\nAuthor: AMT Bot <amt@example.org>\nDate:   Mon Sep 28 10:00:00 2026 +0200\n\n"
    "    Merge pull request #999 from example/feature\n"
)
TOKEN = "https://mattermost.example.org/hooks/fake-amt-token"
TEN = "%i %q %T %C %M %j %V %L %e %Z"
#: The tools of the semantic trace comparison (plan 4.5); curl is the HTTP effect, sed the file edit, grep a pipe.
TRACE_TOOLS = frozenset({"git", "sbatch", "rsync", "mv", "make", "Rscript", "squeue", "sacct"})

FULL_COLUMNS = (
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
)
BRIEF_COLUMNS = ("jobInSLURM", "RunType", "modelstat", "runInAppResults", "Mif", "Iter", "RunStatus")


# ---------------------------------------------------------------------------
# helpers shared by both groups
# ---------------------------------------------------------------------------


def _sha256(path: Path) -> str:
    return hashlib.sha256(path.read_bytes()).hexdigest()


def _record(**cells: object) -> dict[str, object]:
    """A full status record with the given cells over the usual AMT defaults."""
    base: dict[str, object] = {
        "jobInSLURM": "no",
        "RunType": "nash",
        "modelstat": "2: Locally Optimal",
        "runInAppResults": "yes",
        "Mif": "yes",
        "Iter": "26/100",
        "RunStatus": "Normal completion",
        "Warnings": "27",
        "Conv": "converged",
        "Runtime": 8304,
        "summationErrors": 0,
        "rangeErrors": 0,
        "fixErrors": 0,
        "missingProjVars": 2,
        "projSummationErrors": 32,
        "projSummationErrorsRegional": 0,
    }
    base.update(cells)
    return base


def _brief(**cells: object) -> dict[str, object]:
    base: dict[str, object] = {
        "jobInSLURM": "no",
        "RunType": "NA",
        "modelstat": "NA",
        "runInAppResults": "no",
        "Mif": "NA",
        "Iter": "NA",
        "RunStatus": "full.log missing",
    }
    base.update(cells)
    return base


def _table(records: Mapping[str, Mapping[str, object]], paths: Mapping[str, str] | None = None) -> StatusTable:
    table = StatusTable()
    for name, cells in records.items():
        for column, value in cells.items():
            table[name, column] = value
        if paths and name in paths:
            table.register_path(name, paths[name])
    return table


class _CannedStatus:
    """A stand-in for ``get_run_status``: canned records by basename, ``dir()`` filtering like R's ``sort = "nf"``."""

    def __init__(self, records: Mapping[str, Mapping[str, object]]) -> None:
        self.records = dict(records)
        self.calls: list[list[str]] = []

    def __call__(
        self,
        mydir: PathLike | Sequence[PathLike] | None = None,
        sort: str = "nf",
        user: str | None = None,
        detailed: bool = True,
        effects: object = None,
    ) -> StatusTable:
        if mydir is None:
            paths = sorted(os.listdir("."))
        elif isinstance(mydir, str | os.PathLike):
            paths = [os.fspath(mydir)]
        else:
            paths = [os.fspath(p) for p in mydir]
        self.calls.append(list(paths))
        table = StatusTable()
        for path in paths:
            if not os.path.isdir(path):
                continue  # a[a[, "isdir"] == TRUE, ]
            name = os.path.basename(os.path.normpath(path))
            cells = self.records.get(name, _brief())
            for column, value in cells.items():
                table[name, column] = value
            table.register_path(name, os.path.realpath(path))
        return table


def _canned_rda(stats_by_dir: Mapping[str, Mapping[str, object] | None]) -> Callable[..., dict[str, object]]:
    """A stand-in for ``read_rda`` keyed by the directory holding ``runstatistics.rda``."""

    def read(path: PathLike, effects: object = None) -> dict[str, object]:
        directory = os.path.basename(os.path.dirname(os.fspath(path)))
        if directory not in stats_by_dir or not os.path.exists(os.fspath(path)):
            cause = FileNotFoundError(2, "No such file or directory", os.fspath(path))
            raise RParityError("cannot open the connection") from cause
        stats = stats_by_dir[directory]
        return {} if stats is None else {"stats": dict(stats)}

    return read


def _rtime(seconds: float) -> RTime:
    return RTime(seconds, "Europe/Berlin")


# ---------------------------------------------------------------------------
# the synthetic tree
# ---------------------------------------------------------------------------

RUNS_TO_START = ["SSP2-NPi-AMT", "SSP2-NDC-AMT", "SSP2-NPi2025-calibrate-AMT", "testOneRegi-AMT"]


class SynthTree:
    """A minimal AMT tree: ``<root>/modeltests/{.testsstatus,remind/{output,...},testing_suite}``."""

    def __init__(self, root: Path) -> None:
        self.root = root
        self.modeltests = root / "modeltests"
        self.mydir = f"{self.modeltests}/remind/"
        self.output = self.modeltests / "remind" / "output"
        self.gitdir = self.modeltests / "testing_suite"
        self.output.mkdir(parents=True)
        (self.output / "archive").mkdir()
        self.gitdir.mkdir()
        (self.gitdir / "README.md").write_text("old\n")
        self.status_file = self.modeltests / ".testsstatus"
        self.status_file.write_text("next:evaluate\n")
        eff = FakeEffects()
        state.write_lastcommit(self.modeltests / "remind" / "lastcommit.rds", LAST_COMMIT, eff)
        state.write_runcode(self.modeltests / "remind" / "runcode.rds", ".*-AMT_2026-09-28|.*-AMT_2026-09-29", eff)
        frame = pd.DataFrame({"start": ["AMT"] * len(RUNS_TO_START)}, index=pd.Index(RUNS_TO_START, dtype=object))
        state.write_runs_to_start(self.modeltests / "remind" / "runsToStart.rds", frame, eff)

    def run_dir(self, name: str, *files: str) -> Path:
        path = self.output / name
        path.mkdir(exist_ok=True)
        for file in files:
            (path / file).write_text(f"{file}\n")
        return path

    def status_text(self) -> str:
        return self.status_file.read_text()

    def lastcommit(self) -> str:
        return state.read_lastcommit(self.modeltests / "remind" / "lastcommit.rds", FakeEffects())


class SynthHook:
    """The fake binaries of the harness for a synthetic run: canned git, sbatch, rsync, a real mv, the scheduler."""

    def __init__(self, effects_ref: list[RecordingEffects]) -> None:
        self.effects_ref = effects_ref
        self.git_log_1: tuple[str, int] = (GIT_LOG_1, 0)
        self.git_merges: tuple[str, int] = (MERGE_LINES, 0)
        self.squeue: list[tuple[str, int]] = [("", 0)]  # consumed per call, the last one repeats
        self.squeue_calls = 0
        self.mv_exit = 0
        self.sbatch_exit = 0

    def __call__(self, tokens: list[str], cwd: str) -> tuple[str, int] | None:
        segments = shell_segments(tokens)
        first = segments[0]
        tool = os.path.basename(first[0])
        if tool == "squeue":
            answer = self.squeue[min(self.squeue_calls, len(self.squeue) - 1)]
            self.squeue_calls += 1
            return answer
        if tool == "git":
            if first[1:3] == ["log", "-1"]:
                return self.git_log_1
            if "--merges" in first:
                text, status = self.git_merges
                if len(segments) > 1 and segments[1][0] == "grep":
                    kept = [line for line in text.splitlines() if segments[1][1] in line]
                    return ("".join(f"{line}\n" for line in kept), 0 if kept else 1)
                return text, status
            return "", 0
        if tool == "sbatch":
            return "Submitted batch job 4242\n", self.sbatch_exit
        if tool == "rsync":
            return "", 0
        if tool == "mv":
            if self.mv_exit:
                return "", self.mv_exit
            return "", subprocess.run(["mv", *first[1:]], cwd=cwd, check=False).returncode
        return "", 0


def _synth_effects(now: dt.datetime = FROZEN, **kwargs: object) -> tuple[RecordingEffects, SynthHook]:
    ref: list[RecordingEffects] = []
    hook = SynthHook(ref)
    eff = RecordingEffects(now=now, user="pascalfu", on_cluster=True, run_hook=hook, **kwargs)  # type: ignore[arg-type]
    ref.append(eff)
    return eff, hook


@pytest.fixture
def synth(tmp_path: Path, monkeypatch: pytest.MonkeyPatch) -> SynthTree:
    tree = SynthTree(tmp_path)
    monkeypatch.setattr(ev, "load_config", lambda path, effects=None: _cfg_for_cwd(tree))
    monkeypatch.setattr(ev, "read_rda", _canned_rda({}))
    monkeypatch.setattr(ev, "get_run_status", _CannedStatus({}))
    monkeypatch.setattr(ev, "send_notification", lambda *args, **kwargs: None)
    return tree


def _cfg_for_cwd(tree: SynthTree) -> dict[str, object]:
    name = os.path.basename(os.getcwd())
    title = rm.DATETIME_PATTERN.sub("", name)
    return {"title": title, "results_folder": f"output/{name}", "remind_folder": str(tree.modeltests / "remind")}


def _evaluate(
    tree: SynthTree,
    eff: RecordingEffects,
    *,
    model: str | None = "REMIND",
    comp_scen: bool = True,
    email: bool = False,
    token: str | None = TOKEN,
    gitdir: str | None = None,
) -> tuple[ev.EvaluateResult | None, BaseException | None, list[warnings.WarningMessage]]:
    with warnings.catch_warnings(record=True) as caught:
        warnings.simplefilter("always")
        with eff.chdir(tree.output):
            try:
                result = ev.evaluate_runs(
                    model,
                    tree.mydir,
                    comp_scen,
                    email,
                    token,
                    gitdir if gitdir is not None else str(tree.gitdir),
                    "pascalfu",
                    effects=eff,
                )
            except Exception as exc:  # noqa: BLE001 - the R error is the assertion subject
                return None, exc, [w for w in caught if isinstance(w.message, RWarning)]
    return result, None, [w for w in caught if isinstance(w.message, RWarning)]


def _rwarnings(caught: Sequence[warnings.WarningMessage]) -> list[tuple[str, str]]:
    return [(w.message.call, w.message.text) for w in caught if isinstance(w.message, RWarning)]


# ---------------------------------------------------------------------------
# R primitives
# ---------------------------------------------------------------------------


def test_r_print_character_matches_print_default() -> None:
    long_names = [f"SSP2-NPi-AMT_2026-05-{k:02d}_00.00.00" for k in range(1, 13)]
    assert ev.r_print_character(["bbb", "a"]) == '[1] "bbb" "a"  \n'  # R: trailing blanks of the last element
    assert ev.r_print_character(["SSP2-NPi-AMT_2026-05-01_00.00.00"]) == '[1] "SSP2-NPi-AMT_2026-05-01_00.00.00"\n'
    assert ev.r_print_character([*long_names[:2], "x"]) == (
        '[1] "SSP2-NPi-AMT_2026-05-01_00.00.00" "SSP2-NPi-AMT_2026-05-02_00.00.00"\n'
        '[3] "x"                               \n'
    )
    text = ev.r_print_character(long_names)
    lines = text.split("\n")
    assert lines[0].startswith(' [1] "SSP2-NPi-AMT_2026-05-01_00.00.00" "SSP2-NPi-AMT_2026-05-02_00.00.00"')
    assert lines[5].startswith('[11] "SSP2-NPi-AMT_2026-05-11_00.00.00" "SSP2-NPi-AMT_2026-05-12_00.00.00"')
    assert len(lines) == 7 and lines[6] == ""
    assert ev.r_print_character([]) == "character(0)\n"
    assert ev.r_print_character(['a"b']) == '[1] "a\\"b"\n'


def test_r_as_date_decides_the_format_on_the_first_element() -> None:
    assert ev.r_as_date(["2026-05-01", "testOneRegi-AMT", "2026-02-30"]) == [dt.date(2026, 5, 1), None, None]
    assert ev.r_as_date(["2026/05/01", "x"]) == [dt.date(2026, 5, 1), None]
    assert ev.r_as_date(["2026-5-1"]) == [dt.date(2026, 5, 1)]  # one-digit month and day
    assert ev.r_as_date(["2026-05-01-07"]) == [dt.date(2026, 5, 1)]  # trailing text is ignored
    assert ev.r_as_date(["26-05-01"]) == [dt.date(26, 5, 1)]  # %Y takes one to four digits
    assert ev.r_as_date([]) == []
    for first in ("testOneRegi-AMT", "2026-02-30xx", " 2026-05-01", "12026-05-01"):
        with pytest.raises(RParityError, match="character string is not in a standard unambiguous format") as info:
            ev.r_as_date([first, "2026-05-01"])
        assert info.value.call == "charToDate(x)"


def test_difftime_hours_follows_r_units_arithmetic() -> None:
    assert ev.difftime_hours(1500) == (1500 / 60) * (60 / 3600)  # mins, then units<- "hours"
    assert ev.difftime_hours(1500) == 0.4166666666666667
    assert ev.difftime_hours(45) == 45 * (1 / 3600) == 0.0125
    assert ev.difftime_hours(3600 * 2.3) == 2.3
    assert ev.difftime_hours(86400 * 1.5 + 7) == ((86400 * 1.5 + 7) / 86400) * (86400 / 3600)
    assert ev.difftime_hours(-7200) == -2.0


def test_r_read_lines_accepts_every_line_ending() -> None:
    assert ev.r_read_lines("a\r\nb\rc\nd") == ["a", "b", "c", "d"]
    assert ev.r_read_lines("a\n\n") == ["a", ""]
    assert ev.r_read_lines("") == []
    assert ev.r_read_lines("x\n") == ["x"]


def test_read_runtime_hours_variants(monkeypatch: pytest.MonkeyPatch, tmp_path: Path) -> None:
    start = _rtime(1790762400)
    canned: dict[str, Mapping[str, object] | None] = {
        "ok": {"timeGAMSStart": start, "timeGAMSEnd": _rtime(1790762400 + 8304)},
        "nostats": None,
        "noend": {"timeGAMSStart": start},
        "nostart": {"timeGAMSEnd": start},
        "nothing": {"id": "x"},
        "naend": {"timeGAMSStart": start, "timeGAMSEnd": RTime(None, "Europe/Berlin")},
    }
    for name in canned:
        (tmp_path / name).mkdir()
        (tmp_path / name / "runstatistics.rda").write_bytes(b"canned")
    monkeypatch.setattr(ev, "read_rda", _canned_rda(canned))
    eff = FakeEffects()
    assert ev.read_runtime_hours(str(tmp_path / "ok"), eff) == ev.difftime_hours(8304)
    assert ev.read_runtime_hours(str(tmp_path / "nostats"), eff) is None  # NULL - NULL: integer(0)
    assert ev.read_runtime_hours(str(tmp_path / "nothing"), eff) is None
    assert ev.read_runtime_hours(str(tmp_path / "nostart"), eff) is None  # POSIXct - NULL: length zero
    with pytest.raises(RParityError, match='can only subtract from "POSIXt" objects') as info:
        ev.read_runtime_hours(str(tmp_path / "noend"), eff)  # NULL - POSIXct
    assert info.value.call == "`-.POSIXt`(stats$timeGAMSEnd, stats$timeGAMSStart)"
    na = ev.read_runtime_hours(str(tmp_path / "naend"), eff)
    assert na is not None and math.isnan(na)
    with warnings.catch_warnings(record=True) as caught:
        warnings.simplefilter("always")
        with pytest.raises(RParityError, match="cannot open the connection") as info:
            ev.read_runtime_hours(str(tmp_path / "missing"), eff)
    assert info.value.call == "readChar(con, 5L, useBytes = TRUE)"
    assert _rwarnings(caught) == [
        (
            "readChar(con, 5L, useBytes = TRUE)",
            f"cannot open compressed file '{tmp_path}/missing/runstatistics.rda', probable reason "
            "'No such file or directory'",
        )
    ]


def test_cs2_command_is_the_r_string() -> None:
    this, last = "/p/out/SSP2-NPi-AMT_2026-09-28_10.30.27", "/p/out/SSP2-NPi-AMT_2026-09-21_10.30.27"
    assert ev.cs2_command("comp_with_SSP2-NPi-AMT_2026-09-21_10.30.27", this, last) == (
        "sbatch --qos=standby --job-name=comp_with_SSP2-NPi-AMT_2026-09-21_10.30.27 --comment=compareScenarios2"
        f" --output={this}/comp_with_SSP2-NPi-AMT_2026-09-21_10.30.27.out"
        f" --error={this}/comp_with_SSP2-NPi-AMT_2026-09-21_10.30.27.out"
        " --mail-type=END --time=200 --mem-per-cpu=8000"
        f' --wrap="Rscript scripts/cs2/run_compareScenarios2.R outputdirs={this},{last} profileName=default'
        " outFileName=comp_with_SSP2-NPi-AMT_2026-09-21_10.30.27;"
        f' mv comp_with_SSP2-NPi-AMT_2026-09-21_10.30.27.pdf {this}"'
    )


def test_archive_candidates(tmp_path: Path) -> None:
    for name in (
        "default-AMT_2026-09-28_13.23.51",
        "SSP2-NPi-AMT_2026-05-01_00.00.00",
        "SSP2-NPi-AMT_2026-07-02_00.00.00",
        "SSP2-NPi-AMT_2026-07-01_23.59.59",
        "testOneRegi-AMT",
        "archive",
    ):
        (tmp_path / name).mkdir()
    (tmp_path / "SSP3-AMT_2025-01-01_00.00.00.txt").write_text("not a directory\n")
    eff = FakeEffects()
    with eff.chdir(tmp_path):
        old = ev.archive_candidates(dt.date(2026, 9, 30), eff)  # cutoff 2026-07-02, strictly older
    assert old == ["SSP2-NPi-AMT_2026-05-01_00.00.00", "SSP2-NPi-AMT_2026-07-01_23.59.59"]
    # as.Date() decides the format on the first candidate: a name without a date first is R's error
    for name in list(tmp_path.iterdir()):
        if name.is_dir() and name.name != "testOneRegi-AMT":
            shutil.rmtree(name)
    (tmp_path / "zz-AMT_2026-01-01_00.00.00").mkdir()
    with eff.chdir(tmp_path), pytest.raises(RParityError, match="standard unambiguous format"):
        ev.archive_candidates(dt.date(2026, 9, 30), eff)


def test_system_intern_and_system_run_statuses(tmp_path: Path) -> None:
    eff = RecordingEffects(
        run_table={
            ("true",): ("a\nb\n", 0),
            ("false",): ("", 1),
            ("nope",): ("", 127),
        }
    )
    with warnings.catch_warnings(record=True) as caught:
        warnings.simplefilter("always")
        assert ev.system_intern("true", 'system("true", intern = TRUE)', eff) == ["a", "b"]
        assert ev.system_intern("false", "call1", eff) == []
        assert ev.system_run("false", "call2", eff) == 1
        assert ev.system_run("nope", "call3", eff) == 127
        with pytest.raises(RParityError, match="error in running command") as info:
            ev.system_intern("nope", "call4", eff)
    assert info.value.call == "call4"
    assert _rwarnings(caught) == [
        ("call1", "running command 'false' had status 1"),
        ("call3", "error in running command"),
    ]
    assert eff.shell_runs == ["true", "false", "false", "nope", "nope"]


# ---------------------------------------------------------------------------
# the wait loop (lines 180-194)
# ---------------------------------------------------------------------------


def test_wait_loop_counter_reset_and_sleep() -> None:
    eff, hook = _synth_effects()
    mydir = "/p/projects/remind/modeltests/remind/"
    hook.squeue = [
        ("", 1),
        ("", 1),
        ("", 1),
        (f"1 standby RUNNING 1 0:01 x 2026 N/A N/A {mydir}output/a\n", 0),
        ("", 0),
    ]
    with warnings.catch_warnings(record=True) as caught:
        warnings.simplefilter("always")
        ev.wait_for_runs(mydir, "pascalfu", eff)
    assert hook.squeue_calls == 5
    assert eff.sleeps == [
        600,
        600,
        600,
        600,
    ]  # after every failure and after the running job, never before the first poll
    assert [text for _, text in _rwarnings(caught)] == [
        f"running command 'squeue -u pascalfu -h -o '{TEN}'' had status 1"
    ] * 3
    assert all(
        call == 'system(paste0("squeue -u ", user, " -h -o \'%i %q %T %C %M %j %V %L %e %Z\'"), '
        for call, _ in _rwarnings(caught)
    )
    assert eff.tools[0] == ("squeue", ("-u", "pascalfu", "-h", "-o", TEN))


def test_wait_loop_stops_after_four_failures_and_user_null() -> None:
    eff, hook = _synth_effects()
    hook.squeue = [("", 1)]
    with warnings.catch_warnings(record=True):
        warnings.simplefilter("always")
        with pytest.raises(RParityError, match=re.escape(ev.SQUEUE_FAILED)) as info:
            ev.wait_for_runs("/p/x/", None, eff)
    assert info.value.call == "evaluateRuns(model = model, mydir = mydir, compScen = compScen, "
    assert hook.squeue_calls == 4 and eff.sleeps == [600, 600, 600]
    assert eff.shell_runs[0] == f"squeue -u  -h -o '{TEN}'"  # paste0 drops the NULL user
    hook.squeue = [("", 127)]
    with pytest.raises(RParityError, match="error in running command"):
        ev.wait_for_runs("/p/x/", "u", eff)


# ---------------------------------------------------------------------------
# failures before the README (lines 172-198): .testsstatus written, lastcommit.rds untouched
# ---------------------------------------------------------------------------


def test_missing_lastcommit_fails_like_readrds(synth: SynthTree) -> None:
    (synth.modeltests / "remind" / "lastcommit.rds").unlink()
    eff, _hook = _synth_effects()
    result, exc, caught = _evaluate(synth, eff)
    assert result is None and isinstance(exc, RParityError) and str(exc) == "cannot open the connection"
    assert exc.call == 'gzfile(file, "rb")'
    assert synth.status_text() == ev.RUNNING_STATUS + "\n"
    assert _rwarnings(caught) == [
        (
            'gzfile(file, "rb")',
            f"cannot open compressed file '{synth.mydir}/lastcommit.rds', probable reason 'No such file or directory'",
        )
    ]
    assert eff.sleeps == [] and eff.tools == []  # nothing ran after the failed read


def test_git_log_failure_is_the_subscript_error(synth: SynthTree, capsys: pytest.CaptureFixture[str]) -> None:
    eff, hook = _synth_effects()
    hook.git_log_1 = ("", 128)
    result, exc, caught = _evaluate(synth, eff)
    assert result is None and isinstance(exc, RParityError) and str(exc) == "subscript out of bounds"
    assert exc.call == 'system("git log -1", intern = TRUE)[[1]]'
    assert _rwarnings(caught) == [
        ('system("git log -1", intern = TRUE)', "running command 'git log -1' had status 128")
    ]
    assert synth.status_text() == ev.RUNNING_STATUS + "\n" and synth.lastcommit() == LAST_COMMIT
    assert eff.tools[-1] == ("git", ("log", "-1"))
    assert not Path(eff.tempdir(), "README.md").exists()
    err = capsys.readouterr().err
    assert err.splitlines()[-1] == "Compiling the README.md to be committed to testing_suite repo."
    assert "waiting for all AMT runs to finish." in err and "all AMT runs finished." in err


def test_model_null_fails_like_r(synth: SynthTree) -> None:
    eff, _hook = _synth_effects()
    _result, exc, _ = _evaluate(synth, eff, model=None, comp_scen=True)
    assert isinstance(exc, RParityError) and str(exc) == "missing value where TRUE/FALSE needed"
    assert Path(eff.tempdir(), "README.md").read_text().count("\n") == 4  # the four header lines were written
    _result, exc, _ = _evaluate(synth, eff, model=None, comp_scen=False)
    assert isinstance(exc, RParityError) and str(exc) == "argument is of length zero"  # if (NULL != "MAgPIE")
    assert synth.lastcommit() == LAST_COMMIT


# ---------------------------------------------------------------------------
# the REMIND flow (synthetic)
# ---------------------------------------------------------------------------

HIDDEN = ".hidden-AMT_2026-09-28_00.00.00"
NPI_NOW = "SSP2-NPi-AMT_2026-09-28_10.30.27"
NPI_LAST = "SSP2-NPi-AMT_2026-09-21_10.30.27"
NPI_OLD = "SSP2-NPi-AMT_2026-05-01_00.00.00"
START = 1790762400


def _remind_records() -> dict[str, dict[str, object]]:
    return {
        HIDDEN: _record(Conv="NA"),
        "default-AMT_2026-09-28_13.23.51": _record(Conv="converged (had INFES)", Runtime=12213, Iter="41/100"),
        "SSP2-NDC-AMT_2026-09-28_12.00.00": _record(Conv="not_converged", Mif="sumErr", Runtime=77679),
        NPI_NOW: _record(Runtime=8304),
        "SSP2-NPi2025-calibrate-AMT_2026-09-28_10.27.04": _record(
            RunType="Calib_nash", Conv="converged", Iter="26/100 Clb: 10"
        ),
        "SSP3-NPi2025-AMT_2026-09-28_17.12.58": _record(
            Conv="722552252275",
            modelstat="5: Locally Infes",
            Mif="no",
            runInAppResults="no",
            RunStatus="Execution error",
            Runtime=1223,
        ),
        "testOneRegi-AMT_2026-09-28_09.00.00": _record(RunType="testOneRegi", modelstat="5: Locally Infes", Conv="NA"),
        NPI_LAST: _record(Runtime=3600),
        NPI_OLD: _record(Conv="converged", Mif="yes"),
        "archive": _brief(),
        "export": _brief(),
    }


_REMIND_STATS: dict[str, Mapping[str, object] | None] = {
    NPI_NOW: {"timeGAMSStart": _rtime(START), "timeGAMSEnd": _rtime(START + 8304)},
    NPI_LAST: {"timeGAMSStart": _rtime(START), "timeGAMSEnd": _rtime(START + 3600)},
    NPI_OLD: {"timeGAMSStart": _rtime(START), "timeGAMSEnd": _rtime(START + 10)},
    "default-AMT_2026-09-28_13.23.51": {"timeGAMSStart": _rtime(START), "timeGAMSEnd": _rtime(START + 12213)},
    "SSP2-NPi2025-calibrate-AMT_2026-09-28_10.27.04": {
        "timeGAMSStart": _rtime(START),
        "timeGAMSEnd": _rtime(START + 52473),
    },
}

# the per-run error texts of _remind_records() in runsStarted order (lines 279-298), then the runtime check
REMIND_ERRORS = [
    ev.ERR_NOT_CONVERGED,  # .hidden: Conv "NA"
    ev.ERR_NOT_CONVERGED,  # SSP2-NDC not_converged
    ev.ERR_SUM_ERR,  # SSP2-NDC Mif sumErr
    ev.ERR_SLOWER,  # SSP2-NPi 2.3 h against 1 h of the previous run
    ev.ERR_NOT_CONVERGED,  # Calib_nash without Clb_converged
    ev.ERR_NOT_CONVERGED,  # SSP3 Conv of digits
    ev.ERR_NOT_REPORTED,  # SSP3 runInAppResults no
    ev.ERR_TEST_ONE_REGI,  # testOneRegi modelstat (Conv "NA" is skipped by the Calib_nash|testOneRegi exclusion)
]


@pytest.fixture
def remind_tree(synth: SynthTree, monkeypatch: pytest.MonkeyPatch) -> tuple[SynthTree, _CannedStatus]:
    records = _remind_records()
    for name in records:
        if name not in ("archive", "export"):
            title = rm.DATETIME_PATTERN.sub("", name)
            synth.run_dir(name, "config.Rdata", "runstatistics.rda", f"REMIND_generic_{title}.mif")
    (synth.output / NPI_NOW / "fulldata.gdx").write_bytes(b"gdx")
    (synth.output / "export").mkdir()
    status = _CannedStatus(records)
    monkeypatch.setattr(ev, "get_run_status", status)
    monkeypatch.setattr(ev, "read_rda", _canned_rda(_REMIND_STATS))
    return synth, status


def _write_testfull(tree: SynthTree, text: str, *, tests_dir: bool = True) -> Path:
    log = tree.modeltests / "remind" / "test-full.log"
    log.write_text(text)
    os.utime(log, (dt.datetime(2026, 9, 29, 16, 0).timestamp(),) * 2)
    if tests_dir:
        (tree.modeltests / "remind" / "tests").mkdir(exist_ok=True)
    return log


def _notification_recorder(monkeypatch: pytest.MonkeyPatch) -> list[dict[str, object]]:
    sent: list[dict[str, object]] = []

    def record(model: str, token: str | None, **kwargs: object) -> str | None:
        sent.append({"model": model, "token": token, **kwargs})
        return "message" if token is not None else None

    monkeypatch.setattr(ev, "send_notification", record)
    return sent


def test_remind_flow_end_to_end(
    remind_tree: tuple[SynthTree, _CannedStatus], monkeypatch: pytest.MonkeyPatch, capsys: pytest.CaptureFixture[str]
) -> None:
    tree, status = remind_tree
    sent = _notification_recorder(monkeypatch)
    log = _write_testfull(tree, "testthat results\n[ FAIL 0 | WARN 0 | SKIP 3 | PASS 120 ]\n")
    eff, hook = _synth_effects()
    hook.git_merges = ("", 1)  # grep found nothing: a deferred warning
    listed = eff.listdir_like_r(tree.output)  # dir() as the gRS block sees it (no dot entries, no gRS.rds yet)
    result, exc, caught = _evaluate(tree, eff)
    assert exc is None and result is not None
    captured = capsys.readouterr()
    this = str((tree.output / NPI_NOW).resolve())
    last = str((tree.output / NPI_LAST).resolve())

    # runsStarted: list.dirs() (dot-directories included) filtered by runcode.rds, in R's collation order
    assert result.runs_started == [
        HIDDEN,
        "default-AMT_2026-09-28_13.23.51",
        "SSP2-NDC-AMT_2026-09-28_12.00.00",
        NPI_NOW,
        "SSP2-NPi2025-calibrate-AMT_2026-09-28_10.27.04",
        "SSP3-NPi2025-AMT_2026-09-28_17.12.58",
        "testOneRegi-AMT_2026-09-28_09.00.00",
    ]
    # the gRS block without gRS.rds: rbind(NULL, getRunStatus(setdiff(dir(), NULL))) succeeds and is saved
    assert status.calls[0] == listed
    assert status.calls[1:] == [[run] for run in result.runs_started]  # then getRunStatus(i) per run
    grs = state.read_grs(tree.output / "gRS.rds", eff)
    assert grs.rownames == listed
    assert "Conv" in grs.columns and grs[NPI_OLD, "Conv"] == "converged"
    # the error list in R's order with duplicates, the summary with the distinct texts
    assert result.error_list == REMIND_ERRORS
    assert result.summary == "Summary: " + ". ".join(dict.fromkeys(REMIND_ERRORS))
    # the converged branch: rsync for the SSP2-NPi-AMT run only, compareScenarios2 against the collation-largest
    # earlier converged run of the same title, submitted from cfg$remind_folder
    assert eff.tools.count(("rsync", ("-e", "ssh", "-av", "fulldata.gdx", ev.GDX_ON_RSE_SERVER))) == 1
    rsync = [entry for entry in eff.trace if entry.tool == "rsync"]
    assert rsync[0].cwd == this
    sbatch = [entry for entry in eff.trace if entry.tool == "sbatch"]
    cs2 = ev.cs2_command(f"comp_with_{NPI_LAST}", this, last)
    assert len(sbatch) == 1 and sbatch[0].cwd == str(tree.modeltests / "remind")
    assert sbatch[0].argv == tuple(shell_tokens(cs2)[1:])
    assert captured.out == f"{cs2} \n" + f'[1] "{NPI_OLD}"\n'
    # the archive sweep: the run of 2026-05-01 is older than 90 days (cutoff 2026-07-02); the real mv moved it
    assert (tree.output / "archive" / NPI_OLD).is_dir() and not (tree.output / NPI_OLD).exists()
    assert eff.tools.count(("mv", (NPI_OLD, "archive"))) == 1
    # test-full.log: renamed with its mtime date and reported
    assert not log.exists() and (tree.modeltests / "remind" / "tests" / "test-full-2026-09-29.log").exists()
    assert result.testthat_result == "All tests pass in `make test-full`: [ FAIL 0 | WARN 0 | SKIP 3 | PASS 120 ]"
    # BUG-024: 7 runs started >= 4 + 1 scenarios -> no runsNotStarted block
    assert result.runs_not_started is None
    # the notification, then lastcommit.rds (the .testsstatus 'next:start' line belongs to modeltests())
    assert sent[0]["model"] == "REMIND" and sent[0]["token"] == TOKEN and sent[0]["summary"] == result.summary
    assert sent[0]["runs_not_started"] is None and sent[0]["git_info"] == rm.git_info(COMMIT, "2026-09-30", [])
    assert sent[0]["testthat_result"] == result.testthat_result and sent[0]["error_list"] == REMIND_ERRORS
    assert tree.lastcommit() == COMMIT and result.commit_tested == COMMIT and result.message == "message"
    assert tree.status_text() == ev.RUNNING_STATUS + "\n"
    # the grep warning was deferred to the end (the try() of the gRS block did not fail and did not print it)
    assert _rwarnings(caught) == [
        (
            'system(paste0("git log --merges --pretty=oneline ", lastCommit, ',
            f"running command 'git log --merges --pretty=oneline {LAST_COMMIT}..{COMMIT} --abbrev-commit"
            " | grep 'Merge pull request'' had status 1",
        )
    ]
    # the README
    readme = Path(result.readme_path).read_text()
    assert readme.startswith("```\nThis is the result of the automated model tests for REMIND on 2026-09-30.\n")
    assert f"Path to runs: {tree.mydir}output/\n" in readme
    assert "The test of 2026-09-30 contains these merges:\n" + rm.COLUMN_TITLE_LINE + "\n" in readme
    assert readme.endswith(result.summary + "\n```\n")
    assert "2.3 hours" in readme and "21.6 hours" in readme and " \nThese scenarios" not in readme
    # the messages
    err = captured.err.splitlines()
    assert err[0] == f"Current working directory {tree.output.resolve()}"
    assert err[1] == f"Writing '{ev.RUNNING_STATUS}' to {tree.status_file.resolve()}"
    assert err[2] == "2026-09-30 12:00:00 - waiting for all AMT runs to finish."
    assert err[3] == "2026-09-30 12:00:00 - all AMT runs finished."
    assert err[4] == "Compiling the README.md to be committed to testing_suite repo."
    assert err[5:7] == ["Starting analysis for the list of the following runs:", HIDDEN]
    assert f"Changed to {this}" in err and f"Calling compareScenarios2 with {this} and {last}" in err
    assert f"Finished analysis for {NPI_NOW} and changed back to {tree.output.resolve()}" in err
    assert f"{HIDDEN} does not seem to have converged. Skipping!" in err
    assert "Moving 1 runs with timestamp older than 90 days (2026-07-02) to 'archive':" in err
    assert err[-3:] == [
        "Finished compiling README.md",
        "Composing message and sending it to mattermost channel",
        "Function 'evaluateRuns' finished.",
    ]


def test_grs_try_fallback_prints_the_deferred_warnings(
    remind_tree: tuple[SynthTree, _CannedStatus], monkeypatch: pytest.MonkeyPatch, capsys: pytest.CaptureFixture[str]
) -> None:
    """Line 230: an old gRS.rds with 16 columns against brief new rows -> rbind fails inside try(); the fallback
    getRunStatus(dir()) is saved. try() prints the error and every warning deferred so far (the grep one)."""
    tree, status = remind_tree
    _notification_recorder(monkeypatch)
    old = _table({NPI_LAST: _record(Runtime=3600)})
    state.write_grs(tree.output / "gRS.rds", old, FakeEffects())
    eff, hook = _synth_effects()
    hook.git_merges = ("", 1)
    listed = eff.listdir_like_r(tree.output)  # dir(): gRS.rds included (a file; getRunStatus's sort drops it)
    with _canned_status_for_setdiff(status, brief_for_first_call=True):
        result, exc, caught = _evaluate(tree, eff)
    assert exc is None and result is not None
    err = capsys.readouterr().err
    assert (
        "Compiling the README.md to be committed to testing_suite repo.\n"
        "Error in rbind(deparse.level, ...) : \n"
        "  numbers of columns of arguments do not match\n"
        "In addition: Warning message:\n"
        'In system(paste0("git log --merges --pretty=oneline ", lastCommit,  :\n'
        f"  running command 'git log --merges --pretty=oneline {LAST_COMMIT}..{COMMIT} --abbrev-commit | grep "
        "'Merge pull request'' had status 1\n"
        "Starting analysis for the list of the following runs:\n"
    ) in err
    assert _rwarnings(caught) == []  # printed by try(), cleared
    assert status.calls[0] == [name for name in listed if name != NPI_LAST]  # setdiff(dir(), rownames(gRSold))
    assert status.calls[1] == listed  # the fallback getRunStatus(dir())
    grs = state.read_grs(tree.output / "gRS.rds", eff)
    assert grs.rownames == [name for name in listed if name != "gRS.rds"]


class _canned_status_for_setdiff:
    """Make the first getRunStatus call (the setdiff one) answer brief rows only, the later ones the full records."""

    def __init__(self, status: _CannedStatus, *, brief_for_first_call: bool) -> None:
        self.status = status
        self.brief = brief_for_first_call

    def __enter__(self) -> None:
        status = self.status
        original = _CannedStatus.__call__
        full = dict(status.records)

        def call(self_: _CannedStatus, *args: object, **kwargs: object) -> StatusTable:
            if self.brief and not self_.calls:
                self_.records = {name: _brief() for name in full}
                try:
                    return original(self_, *args, **kwargs)  # type: ignore[arg-type]
                finally:
                    self_.records = full
            return original(self_, *args, **kwargs)  # type: ignore[arg-type]

        self._original = original
        _CannedStatus.__call__ = call  # type: ignore[method-assign]

    def __exit__(self, *exc: object) -> None:
        _CannedStatus.__call__ = self._original  # type: ignore[method-assign]


def test_brief_record_among_the_started_runs_aborts_on_print_output(
    remind_tree: tuple[SynthTree, _CannedStatus], monkeypatch: pytest.MonkeyPatch
) -> None:
    tree, status = remind_tree
    _notification_recorder(monkeypatch)
    status.records[HIDDEN] = _brief()  # a run directory without a config: getRunStatus gives the brief columns
    eff, _hook = _synth_effects()
    result, exc, _ = _evaluate(tree, eff)
    assert result is None and isinstance(exc, RParityError) and str(exc) == "undefined columns selected"
    assert tree.lastcommit() == LAST_COMMIT and tree.status_text() == ev.RUNNING_STATUS + "\n"
    assert (tree.output / "gRS.rds").exists()  # saved before the loop, like R
    readme = Path(eff.tempdir(), "README.md").read_text()
    assert readme.endswith(rm.COLUMN_TITLE_LINE + "\n")  # the partial README of R


def test_bug014_no_earlier_run_takes_the_na_path(
    remind_tree: tuple[SynthTree, _CannedStatus], monkeypatch: pytest.MonkeyPatch, capsys: pytest.CaptureFixture[str]
) -> None:
    """Lines 320-328 for a converged run whose siblings are all collation-larger: max(character(0)) is NA with a
    warning, lastRun 'archive/NA', normalizePath warns, .readRuntime() fails -> evaluateRuns aborts (BUG-014)."""
    tree, status = remind_tree
    _notification_recorder(monkeypatch)
    status.records[NPI_LAST]["Conv"] = "NA"  # only the 2026-05-01 run... make the current run the smallest instead
    later = "SSP2-NPi-AMT_2026-09-29_10.30.27"
    tree.run_dir(later, "config.Rdata", "runstatistics.rda")
    status.records[later] = _record(Runtime=100)
    for name in (NPI_LAST, NPI_OLD):
        shutil.rmtree(tree.output / name)
        del status.records[name]
    eff, _hook = _synth_effects()
    result, exc, caught = _evaluate(tree, eff)
    assert result is None and isinstance(exc, RParityError) and str(exc) == "cannot open the connection"
    assert exc.call == "readChar(con, 5L, useBytes = TRUE)"
    assert _rwarnings(caught) == [
        ("max(sameRuns[sameRuns < basename(cfg$results_folder)])", "no non-missing arguments, returning NA"),
        ('normalizePath(file.path("..", lastRun))', 'path[1]="../archive/NA": No such file or directory'),
        (
            "readChar(con, 5L, useBytes = TRUE)",
            "cannot open compressed file '../archive/NA/runstatistics.rda', probable reason "
            "'No such file or directory'",
        ),
    ]
    assert tree.lastcommit() == LAST_COMMIT and tree.status_text() == ev.RUNNING_STATUS + "\n"
    err = capsys.readouterr().err.splitlines()
    assert err[-2] == f"Changed to {(tree.output / NPI_NOW).resolve()}"
    assert err[-1].endswith(f"with the fulldata.gdx of {NPI_NOW}")  # then .readRuntime() failed


def test_previous_run_in_archive_and_not_converged_runtime_skip(
    remind_tree: tuple[SynthTree, _CannedStatus], monkeypatch: pytest.MonkeyPatch, capsys: pytest.CaptureFixture[str]
) -> None:
    """Line 322: a previous run listed in gRS but moved to archive/ is compared from there; a not_converged current
    run (line 326) skips the runtime check but still gets compareScenarios2 (line 333)."""
    tree, status = remind_tree
    _notification_recorder(monkeypatch)
    shutil.move(tree.output / NPI_LAST, tree.output / "archive" / NPI_LAST)
    history = _table({NPI_LAST: status.records[NPI_LAST]})  # gRS.rds still lists the moved run
    state.write_grs(tree.output / "gRS.rds", history, FakeEffects())
    status.records[NPI_NOW]["Conv"] = "not_converged"
    status.records[NPI_NOW]["Runtime"] = 999999  # would be slower, but not_converged skips the check
    eff, _hook = _synth_effects()
    result, exc, _ = _evaluate(tree, eff)
    assert exc is None and result is not None
    grs = state.read_grs(tree.output / "gRS.rds", eff)
    assert grs.rownames[0] == NPI_LAST and len(grs.rownames) > 1  # rbind(gRSold, new) succeeded
    assert ev.ERR_SLOWER not in result.error_list and ev.ERR_NOT_CONVERGED in result.error_list
    this = str((tree.output / NPI_NOW).resolve())
    last = str((tree.output / "archive" / NPI_LAST).resolve())
    assert captured_out_starts(capsys, ev.cs2_command(f"comp_with_archive/{NPI_LAST}", this, last) + " \n")
    assert eff.tools.count(("rsync", ("-e", "ssh", "-av", "fulldata.gdx", ev.GDX_ON_RSE_SERVER))) == 0  # not converged


def captured_out_starts(capsys: pytest.CaptureFixture[str], prefix: str) -> bool:
    return capsys.readouterr().out.startswith(prefix)


def test_comp_scen_false_or_existing_pdf_skips_sbatch(
    remind_tree: tuple[SynthTree, _CannedStatus], monkeypatch: pytest.MonkeyPatch, capsys: pytest.CaptureFixture[str]
) -> None:
    tree, _status = remind_tree
    _notification_recorder(monkeypatch)
    eff, _hook = _synth_effects()
    result, exc, _ = _evaluate(tree, eff, comp_scen=False)
    assert exc is None and result is not None and ev.ERR_SLOWER in result.error_list
    assert not any(tool == "sbatch" for tool, _ in eff.tools)
    readme = Path(result.readme_path).read_text()
    assert "compareScenarios PDF" not in readme
    assert capsys.readouterr().out == f'[1] "{NPI_OLD}"\n'  # only the archive sweep printed
    # a comp_with_*.pdf already in the run directory (line 337)
    (tree.output / NPI_NOW / f"comp_with_{NPI_LAST}.pdf").write_bytes(b"%PDF")
    eff, _hook = _synth_effects()
    result, exc, _ = _evaluate(tree, eff, comp_scen=True)
    assert exc is None and result is not None
    assert not any(tool == "sbatch" for tool, _ in eff.tools) and capsys.readouterr().out == ""


def test_runs_not_started_block_and_missing_runs_to_start(
    remind_tree: tuple[SynthTree, _CannedStatus], monkeypatch: pytest.MonkeyPatch
) -> None:
    """Lines 366-378 (BUG-024 count check) and the readRDS of runsToStart.rds after the run loop."""
    tree, status = remind_tree
    sent = _notification_recorder(monkeypatch)
    for name in list(status.records):
        if name.startswith(("SSP2-NDC", "SSP3", "testOneRegi", ".hidden", "SSP2-NPi2025")):
            shutil.rmtree(tree.output / name)
            del status.records[name]
    eff, _hook = _synth_effects()
    result, exc, _ = _evaluate(tree, eff)
    assert exc is None and result is not None
    assert result.runs_started == ["default-AMT_2026-09-28_13.23.51", NPI_NOW]  # 2 < 4 + 1
    assert result.runs_not_started == ["SSP2-NDC-AMT", "SSP2-NPi2025-calibrate-AMT", "testOneRegi-AMT"]
    assert rm.not_started_text(result.runs_not_started) in Path(result.readme_path).read_text()
    assert sent[0]["runs_not_started"] == result.runs_not_started
    # runsToStart.rds missing: the error comes after the run loop, the README holds the run lines
    (tree.modeltests / "remind" / "runsToStart.rds").unlink()
    committed = tree.lastcommit()  # saved by the successful run above
    eff, _hook = _synth_effects()
    result, exc, caught = _evaluate(tree, eff)
    assert result is None and isinstance(exc, RParityError) and str(exc) == "cannot open the connection"
    assert any(
        text.startswith(f"cannot open compressed file '{tree.mydir}runsToStart.rds'") for _, text in _rwarnings(caught)
    )
    readme = Path(eff.tempdir(), "README.md").read_text()
    assert readme.count("\n") == 7 + 4 + 1 + 2  # header, gitInfo, titles, two runs: no summary, no fence
    assert tree.lastcommit() == committed and tree.status_text() == ev.RUNNING_STATUS + "\n"


@pytest.mark.parametrize(
    ("text", "tests_dir", "expected", "renamed"),
    [
        (
            "testthat results\n[ FAIL 0 | WARN 0 | SKIP 3 | PASS 120 ]\n",
            True,
            "All tests pass in `make test-full`: [ FAIL 0 | WARN 0 | SKIP 3 | PASS 120 ]",
            True,
        ),
        (
            "testthat results\n[ FAIL 2 | WARN 1 | SKIP 0 | PASS 10 ]\n",
            True,
            "Not all tests pass in `make test-full`: [ FAIL 2 | WARN 1 | SKIP 0 | PASS 10 ]. Check `{new}`",
            True,
        ),
        (
            "testthat results\n[ FAIL 0 | WARN 1 | SKIP 0 | PASS 10 ]\n",
            True,
            "Not all tests pass in `make test-full`: [ FAIL 0 | WARN 1 | SKIP 0 | PASS 10 ]. Check `{new}`",
            True,
        ),
        ("Error: could not start\n", True, "`make test-full` did not run properly. Check {new}", True),
        (
            "[ FAIL 0 | WARN 0 | SKIP 0 | PASS 1 ]\n[ FAIL 0 | WARN 0 | SKIP 0 | PASS 2 ]\n",
            True,
            "`make test-full` did not run properly. Check {new}",
            True,
        ),
        (
            "testthat results\r\n[ FAIL 0 | WARN 0 | SKIP 3 | PASS 120 ]",
            True,
            "All tests pass in `make test-full`: [ FAIL 0 | WARN 0 | SKIP 3 | PASS 120 ]",
            True,
        ),
        (
            "testthat results\n[ FAIL 0 | WARN 0 | SKIP 3 | PASS 120 ]\n",
            False,
            "All tests pass in `make test-full`: [ FAIL 0 | WARN 0 | SKIP 3 | PASS 120 ]",
            False,
        ),
    ],
    ids=["pass", "fail", "warn", "no-fail-line", "two-fail-lines", "crlf-no-final-newline", "no-tests-dir"],
)
def test_testthat_evaluation(
    remind_tree: tuple[SynthTree, _CannedStatus],
    monkeypatch: pytest.MonkeyPatch,
    text: str,
    tests_dir: bool,
    expected: str,
    renamed: bool,
) -> None:
    tree, _status = remind_tree
    _notification_recorder(monkeypatch)
    log = _write_testfull(tree, text, tests_dir=tests_dir)
    eff, _hook = _synth_effects()
    result, exc, caught = _evaluate(tree, eff)
    assert exc is None and result is not None
    target = tree.modeltests / "remind" / "tests" / "test-full-2026-09-29.log"
    new = (
        str(target.resolve()) if renamed else "../tests/test-full-2026-09-29.log"
    )  # normalizePath keeps a missing path
    assert result.testthat_result == expected.format(new=new)
    assert target.exists() is renamed and log.exists() is not renamed
    rename_warnings = [(c, t) for c, t in _rwarnings(caught) if c.startswith("file.rename")]
    normalize_warnings = [(c, t) for c, t in _rwarnings(caught) if c.startswith("normalizePath")]
    if renamed:
        assert rename_warnings == [] and normalize_warnings == []
    else:
        assert rename_warnings == [
            (
                'file.rename(from = currentName, to = paste0("../", newName))',
                "cannot rename file '../test-full.log' to '../tests/test-full-2026-09-29.log', reason "
                "'No such file or directory'",
            )
        ]
        assert normalize_warnings == []  # the "All tests pass" text carries no path: normalizePath is not called


def test_testthat_log_missing(remind_tree: tuple[SynthTree, _CannedStatus], monkeypatch: pytest.MonkeyPatch) -> None:
    tree, _status = remind_tree
    sent = _notification_recorder(monkeypatch)
    eff, _hook = _synth_effects()
    result, exc, _ = _evaluate(tree, eff)
    assert exc is None and result is not None and result.testthat_result == ev.TESTFULL_NOT_FOUND
    assert sent[0]["testthat_result"] == ev.TESTFULL_NOT_FOUND


def test_email_step_commits_readme_and_changelog(
    remind_tree: tuple[SynthTree, _CannedStatus], monkeypatch: pytest.MonkeyPatch
) -> None:
    tree, _status = remind_tree
    _notification_recorder(monkeypatch)
    eff, _hook = _synth_effects()
    result, exc, _ = _evaluate(tree, eff, email=True)
    assert exc is None and result is not None
    gitdir = str(tree.gitdir.resolve())
    git = [(entry.argv, entry.cwd) for entry in eff.trace if entry.tool == "git" and entry.cwd == gitdir]
    assert git == [
        (("reset", "--hard", "origin/master"), gitdir),
        (("pull",), gitdir),
        (("add", "README.md"), gitdir),
        (("commit", "-m", "Automated Test Results"), gitdir),
        (("push",), gitdir),
    ]
    assert (tree.gitdir / "README.md").read_bytes() == Path(result.readme_path).read_bytes()
    assert not (tree.gitdir / "data-changelog.csv").exists()
    # with a data-changelog.csv already in the clone the add follows (REMIND copies none: changelog is NULL)
    (tree.gitdir / "data-changelog.csv").write_text("version\n")
    eff, _hook = _synth_effects()
    _evaluate(tree, eff, email=True)
    argv = [entry.argv for entry in eff.trace if entry.tool == "git" and entry.cwd == gitdir]
    assert ("add", "data-changelog.csv") in argv
    # email=TRUE without a gitdir: setwd(NULL)
    eff, _hook = _synth_effects()
    with pytest.raises(RParityError, match="character argument expected"):
        with eff.chdir(tree.output):
            ev.evaluate_runs("REMIND", tree.mydir, True, True, TOKEN, None, "pascalfu", effects=eff)


def test_rsync_only_for_converged_ssp2_npi(
    remind_tree: tuple[SynthTree, _CannedStatus], monkeypatch: pytest.MonkeyPatch
) -> None:
    tree, status = remind_tree
    _notification_recorder(monkeypatch)
    status.records[NPI_NOW]["Conv"] = "converged (had INFES)"
    eff, _hook = _synth_effects()
    result, exc, _ = _evaluate(tree, eff)
    assert exc is None and result is not None
    assert eff.tools.count(("rsync", ("-e", "ssh", "-av", "fulldata.gdx", ev.GDX_ON_RSE_SERVER))) == 1
    # a 127 of rsync is a warning, the run goes on (system() without intern)
    eff, hook = _synth_effects()
    original = hook.__call__

    def failing(tokens: list[str], cwd: str) -> tuple[str, int] | None:
        if os.path.basename(tokens[0]) == "rsync":
            return "", 127
        return original(tokens, cwd)

    eff = RecordingEffects(now=FROZEN, user="pascalfu", on_cluster=True, run_hook=failing)
    result, exc, caught = _evaluate(tree, eff)
    assert exc is None and result is not None
    assert ('system(paste("rsync -e ssh -av fulldata.gdx", gdxOnRseServer))', "error in running command") in _rwarnings(
        caught
    )


def test_na_run_type_fails_in_the_error_rules(
    remind_tree: tuple[SynthTree, _CannedStatus], monkeypatch: pytest.MonkeyPatch
) -> None:
    """A real NA RunType: !grepl(..., NA) is NA; NA && TRUE is NA -> if() fails (R's NA logic)."""
    tree, status = remind_tree
    _notification_recorder(monkeypatch)
    status.records["SSP2-NDC-AMT_2026-09-28_12.00.00"]["RunType"] = None
    eff, _hook = _synth_effects()
    result, exc, _ = _evaluate(tree, eff)
    assert isinstance(exc, RParityError) and str(exc) == "missing value where TRUE/FALSE needed"
    # with a converged Conv the first rule is NA && FALSE = FALSE, the Calib_nash rule NA && TRUE fails
    status.records["SSP2-NDC-AMT_2026-09-28_12.00.00"].update({"Conv": "converged", "Mif": "yes"})
    eff, _hook = _synth_effects()
    result, exc, _ = _evaluate(tree, eff)
    assert isinstance(exc, RParityError) and str(exc) == "missing value where TRUE/FALSE needed"


# ---------------------------------------------------------------------------
# the MAgPIE flow (synthetic)
# ---------------------------------------------------------------------------


class _CtimeEffects(RecordingEffects):
    """RecordingEffects whose ``stat`` reports a chosen ctime for every path (the sandbox's ``ctime+Nd`` regime)."""

    def __init__(self, ctime_by_name: Mapping[str, float], default_ctime: float, **kwargs: object) -> None:
        super().__init__(**kwargs)  # type: ignore[arg-type]
        self._ctime_by_name = dict(ctime_by_name)
        self._default_ctime = default_ctime

    def stat(self, path: PathLike) -> FileStat:
        st = super().stat(path)
        name = os.path.basename(os.path.normpath(os.fspath(path)))
        return FileStat(st.mtime, self._ctime_by_name.get(name, self._default_ctime), st.size)


MAGPIE_FROZEN = dt.datetime(2026, 10, 1, 12, 0, 0, tzinfo=BERLIN)


def test_magpie_flow(synth: SynthTree, monkeypatch: pytest.MonkeyPatch, capsys: pytest.CaptureFixture[str]) -> None:
    tree = synth
    sent = _notification_recorder(monkeypatch)
    records = {
        "default_2026-09-19_04.05.47": _record(
            RunType="nlp_apr17", Conv="NA", modelstat="222222222222222222", Iter="y2100", Warnings="0", Runtime=2010
        ),
        "weeklyTests_SSP1-Ref": _record(
            RunType="nlp_apr17",
            Conv="NA",
            modelstat="222222222000000000",
            Iter="y2035",
            Runtime=852,
            RunStatus="Terminated due to",
        ),
        "v39k_FSECc_BAU": _record(
            RunType="nlp_apr17",
            RunStatus="full.log missing",
            Warnings="NA",
            Iter="NA",
            Conv="NA",
            modelstat="NA",
            Mif="no",
            runInAppResults="no",
            Runtime=None,
        ),
        "old_run": _record(RunType="nlp_apr17", Conv="NA"),
        "default_2026-01-01_00.00.00": _record(RunType="nlp_apr17", Conv="NA"),
    }
    for name in records:
        tree.run_dir(name, "config.yml")
    (tree.output / "default_2026-09-19_04.05.47" / "report.rds").write_bytes(b"rds")
    (tree.output / "default_2026-01-01_00.00.00" / "report.rds").write_bytes(b"rds")
    (tree.gitdir / "data-changelog.csv").write_text("version,x\nold,1\n")
    status = _CannedStatus(records)
    monkeypatch.setattr(ev, "get_run_status", status)
    bridge_calls: list[dict[str, object]] = []

    def fake_bridge(
        report: str, changelog: str, version_id: str, *, cwd: object = None, effects: object = None
    ) -> object:
        bridge_calls.append({"report": report, "changelog": changelog, "version_id": version_id, "cwd": cwd})
        Path(changelog).write_text(Path(changelog).read_text() + f"{version_id},2\n")
        return None

    monkeypatch.setattr(ev, "add_to_data_changelog", fake_bridge)
    recent = (MAGPIE_FROZEN - dt.timedelta(days=1)).timestamp()
    old = (MAGPIE_FROZEN - dt.timedelta(days=4)).timestamp()
    hook = SynthHook([])
    eff = _CtimeEffects(
        {"old_run": old, "default_2026-01-01_00.00.00": old, "archive": old},
        recent,
        now=MAGPIE_FROZEN,
        user="pascalfu",
        on_cluster=True,
        run_hook=hook,
    )
    with warnings.catch_warnings(record=True):
        warnings.simplefilter("always")
        with eff.chdir(tree.output):
            result = ev.evaluate_runs(
                "MAgPIE", tree.mydir, False, False, TOKEN, str(tree.gitdir), "pascalfu", effects=eff
            )
    captured = capsys.readouterr()
    # ctime within three days (today - 3 = 2026-09-28; the recent ctime date 2026-09-30 passes, the old 2026-09-27 not)
    assert result.runs_started == ["default_2026-09-19_04.05.47", "v39k_FSECc_BAU", "weeklyTests_SSP1-Ref"]
    # getRunStatus(dir()) once, then per run; no gRS.rds, no runcode read, no runsToStart, no archive sweep
    assert status.calls[0] == eff.listdir_like_r(tree.output) and status.calls[1:] == [[r] for r in result.runs_started]
    assert not (tree.output / "gRS.rds").exists()
    assert not any(tool in ("mv", "sbatch", "rsync") for tool, _ in eff.tools)
    assert result.testthat_result is None and result.runs_not_started is None
    # the changelog bridge for default_* runs that started: copied from gitdir, then the bridge
    changelog = os.path.join(eff.tempdir(), "data-changelog.csv")
    assert bridge_calls == [
        {
            "report": "default_2026-09-19_04.05.47/report.rds",
            "changelog": changelog,
            "version_id": "default_2026-09-19_04.05.47",
            "cwd": None,
        }
    ]
    assert Path(changelog).read_text() == "version,x\nold,1\ndefault_2026-09-19_04.05.47,2\n"
    copies = [call.args for call in eff.calls if call.method == "copy"]
    assert copies == [(f"{tree.gitdir}/data-changelog.csv", changelog, False)]
    # the error rules of lines 294 and 298: the distinct digits of modelstat, runInAppResults
    assert result.error_list == [
        ev.ERR_NOT_CONVERGED,  # v39k: modelstat "NA" -> "" != "2"
        ev.ERR_NOT_REPORTED,  # v39k: runInAppResults no
        ev.ERR_NOT_CONVERGED,  # weeklyTests_SSP1-Ref: 222222222000000000 -> "20"
    ]
    assert sent[0]["model"] == "MAgPIE" and sent[0]["error_list"] == result.error_list
    readme = Path(result.readme_path).read_text()
    assert "use 'weeklyTests' as keyword" in readme and "compareScenarios" not in readme
    assert result.summary == "Summary: Some run(s) did not converge. Some run(s) did not report correctly"
    assert captured.err.splitlines()[-1] == "Function 'evaluateRuns' finished."
    assert "does not seem to have converged" not in captured.err  # MAgPIE never prints the skip message
    assert tree.lastcommit() == COMMIT


def test_modelstat_digits_rule() -> None:
    assert ev._modelstat_digits("2: Locally Optimal") == "2"
    assert ev._modelstat_digits("222222222222222222") == "2"
    assert ev._modelstat_digits("222222222000000000") == "20"
    assert ev._modelstat_digits("NA") == ""
    assert ev._modelstat_digits(None) == "NA"
    assert ev._modelstat_digits("5: Locally Infes") == "5"


# ---------------------------------------------------------------------------
# the representative AMT cases against the R goldens (needs migration/fixtures and Rscript)
# ---------------------------------------------------------------------------

#: The representative cases named by the workflow; ``MODELSTATS_AMT_EVALUATE_CASES=all`` runs every evaluate case
#: of ``migration/cases/amt`` through the same harness (the full comparison belongs to the stage-3 golden tier).
REPRESENTATIVE_CASES = [
    "remind-evaluate",
    "remind-evaluate-nocompscen",
    "remind-evaluate-slow",
    "remind-evaluate-archive",
    "remind-evaluate-git-fail",
    "magpie-evaluate-recent",
]


def _all_evaluate_cases() -> list[str]:
    """Every evaluate case whose prepare script runs outside the sandbox.

    Two cases (``remind-evaluate-grs-existing``, ``remind-evaluate-previous-in-archive``) prepare their ``gRS.rds``
    with the R package's own ``getRunStatus()`` through ``devtools::load_all(Sys.getenv("MODELSTATS_REPO"))``:
    off the sandbox's ``/p`` view that call produces the local (15-column) table instead of the cluster one, so
    their starting state cannot be reproduced here; the stage-3 golden tier covers them in the sandbox.
    """
    cases = []
    for case in sorted(p.name for p in AMT_CASES.glob("*-evaluate*") if p.is_dir()):
        prepare = AMT_CASES / case / "prepare.sh"
        if prepare.is_file() and "MODELSTATS_REPO" in prepare.read_text():
            continue
        cases.append(case)
    return cases


GOLDEN_CASES = (
    _all_evaluate_cases()
    if os.environ.get("MODELSTATS_AMT_EVALUATE_CASES") == "all" and AMT_CASES.is_dir()
    else REPRESENTATIVE_CASES
)
_SUBTREE = {"REMIND": "remind/modeltests", "MAgPIE": "landuse/tests"}

golden_backed = pytest.mark.skipif(
    not FIXTURES.is_dir() or not SYNTHETIC.is_dir() or not AMT_GOLDENS.is_dir() or shutil.which("Rscript") is None,
    reason="needs migration/fixtures, migration/synthetic, the AMT goldens and Rscript (the prepare scripts use it)",
)


def _magpie4_available() -> bool:
    proc = subprocess.run(
        ["Rscript", "-e", "cat(requireNamespace('magpie4', quietly = TRUE))"],
        capture_output=True,
        text=True,
        check=False,
    )
    return proc.returncode == 0 and proc.stdout.strip().endswith("TRUE")


class CaseHook:
    """The fake binaries of ``migration/harness/fakebin`` for one AMT case: canned ``<name>.txt`` / ``<name>.exit``
    from the case directory, the scheduler fake of the double, a real ``mv``, the bridges to the real ``Rscript``."""

    def __init__(self, case_dir: Path) -> None:
        self.case_dir = case_dir
        self.effects: RecordingEffects | None = None

    def _canned(self, name: str, default: str) -> str:
        path = self.case_dir / f"{name}.txt"
        return path.read_text() if path.is_file() else default

    def _exit(self, name: str) -> int:
        path = self.case_dir / f"{name}.exit"
        digits = re.sub(r"[^0-9]", "", path.read_text()) if path.is_file() else ""
        return int(digits) if digits else 0

    def _git(self, args: list[str]) -> tuple[str, int]:
        sub = next((arg for arg in args if not arg.startswith("-")), "")
        name = f"git-{sub}"
        if sub == "log":
            name = "git-log-1" if "-1" in args else ("git-log-merges" if "--merges" in args else name)
        defaults = {"git-log-1": GIT_LOG_1, "git-log-merges": MERGE_LINES}
        return self._canned(name, defaults.get(name, "")), self._exit(name)

    def __call__(self, tokens: list[str], cwd: str) -> tuple[str, int] | None:
        assert self.effects is not None
        segments = shell_segments(tokens)
        first = segments[0]
        tool, args = os.path.basename(first[0]), first[1:]
        if tool in ("squeue", "sacct"):
            stdout, _stderr, status = self.effects._slurm.answer(tool, args)  # noqa: SLF001 - the double's fake
            return stdout, status
        if tool == "git":
            text, status = self._git(args)
            if len(segments) > 1 and segments[1][0] == "grep":  # ... | grep 'Merge pull request'
                kept = [line for line in text.splitlines() if segments[1][1] in line]
                return "".join(f"{line}\n" for line in kept), 0 if kept else 1
            return text, status
        if tool == "sbatch":
            return self._canned("sbatch", "Submitted batch job 4242\n"), self._exit("sbatch")
        if tool == "rsync":
            return self._canned("rsync", ""), self._exit("rsync")
        if tool == "make":
            return self._canned("make", f"fake make: {' '.join(args)}\n"), self._exit("make")
        if tool == "mv":
            status = self._exit("mv")
            if status:
                return "", status
            return "", subprocess.run(["mv", *args], cwd=cwd, check=False).returncode
        if tool == "Rscript":
            if any(arg == "start.R" or arg.endswith("/start.R") for arg in args):
                return self._canned("rscript-start", f"fake Rscript: {' '.join(args)}\n"), self._exit("rscript-start")
            return None  # a bridge script: the real Rscript (delegate_run)
        return None


class CaseEffects(RecordingEffects):
    """RecordingEffects with a per-case temporary directory and, for the MAgPIE ``ctime+Nd`` regime, a fixed ctime."""

    def __init__(self, *, case_tempdir: Path, ctime: float | None, **kwargs: object) -> None:
        super().__init__(**kwargs)  # type: ignore[arg-type]
        self._case_tempdir = case_tempdir
        self._ctime = ctime

    def tempdir(self) -> str:
        self._log("tempdir")
        return str(self._case_tempdir)

    def stat(self, path: PathLike) -> FileStat:
        st = super().stat(path)
        return st if self._ctime is None else FileStat(st.mtime, self._ctime, st.size)


def _snapshot(root: Path) -> dict[str, str]:
    """Every path below ``root`` (symlinks not followed): directories as ``"dir"``, files as their sha256."""
    out: dict[str, str] = {}
    for dirpath, dirnames, filenames in os.walk(root):
        rel = os.path.relpath(dirpath, root)
        for name in list(dirnames):
            full = Path(dirpath, name)
            if full.is_symlink():
                dirnames.remove(name)
                continue
            out[os.path.normpath(os.path.join(rel, name))] = "dir"
        for name in filenames:
            full = Path(dirpath, name)
            if full.is_symlink():
                continue
            out[os.path.normpath(os.path.join(rel, name))] = _sha256(full)
    return out


def _tree_diff(before: Mapping[str, str], after: Mapping[str, str]) -> dict[str, set[str]]:
    created = set(after) - set(before)
    deleted = set(before) - set(after)
    modified = {path for path in set(before) & set(after) if before[path] != after[path] and after[path] != "dir"}
    return {"created": created, "deleted": deleted, "modified": modified}


@dataclasses.dataclass
class CaseRun:
    case: str
    spec: dict[str, object]
    golden: Path
    root: Path
    mydir: str
    effects: CaseEffects
    result: ev.EvaluateResult | None
    error: BaseException | None
    stdout: str
    stderr: str
    diff: dict[str, set[str]]
    same: dict[str, bool]

    def norm(self, text: str) -> str:
        """The sandbox's ``/p`` for the temporary root (the only normalisation of the comparison)."""
        return text.replace(str(self.root), "/p")


def _copy_fixture_view(root: Path, subtree: str) -> None:
    dst = root / "projects" / subtree
    dst.mkdir(parents=True)
    for layer in (SYNTHETIC, FIXTURES):  # the synthetic layer below the fixtures: the fixtures win
        src = layer / subtree
        if src.is_dir():
            subprocess.run(["cp", "-a", "--reflink=auto", f"{src}/.", f"{dst}/"], check=True)
    (root / "projects" / "rd3mod").symlink_to(FIXTURES / "rd3mod")  # the results archives, read only


@pytest.fixture
def case_root() -> Iterator[Path]:
    """A scratch directory for one case's fixture view (about 700 MB), removed at teardown.

    It lives under the gitignored ``migration/_scratch`` when that is writable: on the same file system as the
    fixtures a ``cp --reflink=auto`` copy costs no space and no time (btrfs/xfs), whereas ``tmp_path`` on a tmpfs
    would hold a real copy per case for the whole session. Elsewhere ``tempfile.mkdtemp()`` is used.
    """
    scratch = MIGRATION / "_scratch"
    base = scratch if scratch.is_dir() and os.access(scratch, os.W_OK) else None
    root = Path(tempfile.mkdtemp(prefix="p5-evaluate-case-", dir=base)).resolve()
    try:
        yield root
    finally:
        shutil.rmtree(root, ignore_errors=True)


def _run_golden_case(
    case: str, case_root: Path, monkeypatch: pytest.MonkeyPatch, capsys: pytest.CaptureFixture[str]
) -> CaseRun:
    spec = json.loads((AMT_CASES / case / "case.json").read_text())
    golden = AMT_GOLDENS / case
    result_golden = json.loads((golden / "result.json").read_text())
    frozen = dt.datetime.strptime(result_golden["frozen"], "%Y-%m-%d %H:%M:%S").replace(tzinfo=BERLIN)
    ctime: float | None = None
    if str(spec.get("frozen", "")).startswith("ctime+"):
        days = int(str(spec["frozen"])[len("ctime+") : -1])
        ctime = (frozen - dt.timedelta(days=days)).timestamp()
    root = case_root / "p"
    _copy_fixture_view(root, _SUBTREE[str(spec["model"])])
    env = {**os.environ, "LC_ALL": "C.utf8", "TZ": "Europe/Berlin"}
    for script in spec.get("prepare", []):
        subprocess.run(["sh", str(AMT_CASES / case / script)], cwd=root, env=env, check=True)
    mydir = str(spec["mydir"]).replace("/p/", f"{root}/", 1)
    gitdir = None if spec.get("gitdir") is None else str(spec["gitdir"]).replace("/p/", f"{root}/", 1)
    unchanged = [os.path.join(mydir, p) for p in spec.get("expect_unchanged", ["../.testsstatus", "lastcommit.rds"])]
    before_sha = {p: _sha256(Path(p)) if Path(p).is_file() else "" for p in unchanged}
    before = _snapshot(root)
    slurm_case = str(spec.get("slurm_case", "recorded"))
    hook = CaseHook(AMT_CASES / case)
    curl_exit = hook._exit("curl")  # noqa: SLF001 - the fake curl's exit status becomes a failed POST (status 0)
    eff = CaseEffects(
        case_tempdir=case_root / "rtmp",
        ctime=ctime,
        now=frozen,
        user=str(spec["user"]),
        on_cluster=spec.get("mode", "oncluster") == "oncluster",
        slurm_case_dir=RECORDED_SLURM if slurm_case == "recorded" else SLURM_CASES / slurm_case,
        env={"MAGPIE_RESULTS_ARCHIVE_PATH": f"{root}/projects/rd3mod/models/results/magpie"},
        run_hook=hook,
        delegate_run=True,
        post_json_response=(0, f"fake curl failure (exit {curl_exit})") if curl_exit else (200, "ok"),
    )
    hook.effects = eff
    (case_root / "rtmp").mkdir(exist_ok=True)
    # the cluster paths the port carries as constants or reads from the fixtures' config.Rdata (cfg$remind_folder,
    # the cwd of the compareScenarios2 sbatch) exist in the sandbox at /p; here they are mapped to the copy's root
    monkeypatch.setattr(sanity, "AMT_PATH", f"{root}/projects/remind/modeltests/remind/output/")
    monkeypatch.setattr(sanity, "AMT_RUNCODE", f"{root}/projects/remind/modeltests/remind/runcode.rds")
    monkeypatch.setattr(run_status, "REMIND_RESULTS_ARCHIVE", f"{root}/projects/rd3mod/models/results/remind/")
    real_load_config = ev.load_config

    def rooted_load_config(path: PathLike, effects: object = None) -> dict[str, object]:
        cfg = real_load_config(path, effects)  # type: ignore[arg-type]
        return {
            key: value.replace("/p/", f"{root}/", 1) if isinstance(value, str) and value.startswith("/p/") else value
            for key, value in cfg.items()
        }

    monkeypatch.setattr(ev, "load_config", rooted_load_config)
    enabled, depth = colors.is_enabled(), colors.num_colors()
    colors.set_enabled(True, 256)  # the sandbox's R_CLI_NUM_COLORS=256
    result: ev.EvaluateResult | None = None
    error: BaseException | None = None
    try:
        with warnings.catch_warnings(record=True):
            warnings.simplefilter("always")
            with eff.chdir(f"{mydir}output"):
                try:
                    result = ev.evaluate_runs(
                        str(spec["model"]),
                        mydir,
                        bool(spec.get("compScen", True)),
                        bool(spec.get("email", False)),
                        spec.get("mattermostToken"),  # type: ignore[arg-type]
                        gitdir,
                        str(spec["user"]),
                        effects=eff,
                    )
                except Exception as exc:  # noqa: BLE001 - the R error is compared with the golden
                    error = exc
    finally:
        colors.set_enabled(enabled, depth)
    if error is None:
        Path(mydir, "../.testsstatus").write_text("next:start\n")  # modeltests() line 51
    captured = capsys.readouterr()
    after = _snapshot(root)
    same = {
        os.path.relpath(p, mydir) if not p.endswith("../.testsstatus") else "../.testsstatus": before_sha[p]
        == (_sha256(Path(p)) if Path(p).is_file() else "")
        for p in unchanged
    }
    return CaseRun(
        case, spec, golden, root, mydir, eff, result, error, captured.out, captured.err, _tree_diff(before, after), same
    )


def _golden_trace(path: Path) -> collections.Counter[tuple[str, tuple[str, ...], str]]:
    entries: collections.Counter[tuple[str, tuple[str, ...], str]] = collections.Counter()
    if not path.is_file():
        return entries
    for line in path.read_text().splitlines():
        if not line.strip():
            continue
        record = json.loads(line)
        if record["tool"] in TRACE_TOOLS:
            entries[(record["tool"], tuple(record["argv"]), record["cwd"])] += 1
    return entries


def _python_trace(run: CaseRun) -> collections.Counter[tuple[str, tuple[str, ...], str]]:
    """The double's trace as the sandbox would record it: the fake Rscript traces only ``start.R`` invocations, so
    the bridge scripts (in-process R code on the R side, see the p5-bridges contract) are dropped like there."""
    entries: collections.Counter[tuple[str, tuple[str, ...], str]] = collections.Counter()
    for entry in run.effects.trace:
        if entry.tool not in TRACE_TOOLS:
            continue
        if entry.tool == "Rscript" and not any(arg == "start.R" or arg.endswith("/start.R") for arg in entry.argv):
            continue
        entries[(entry.tool, tuple(run.norm(arg) for arg in entry.argv), run.norm(entry.cwd))] += 1
    return entries


def _golden_payloads(path: Path) -> list[str]:
    if not path.is_file():
        return []
    return [payload_text(json.loads(line)["payload"]) for line in path.read_text().splitlines() if line.strip()]


def _stderr_slice(lines: list[str]) -> list[str]:
    """The stderr lines of ``evaluateRuns`` in a golden: after ``Calling 'evaluateRuns'``, without the wrapper's
    ``Writing 'next:start'`` line."""
    start = lines.index("Calling 'evaluateRuns'") + 1
    body = lines[start:]
    if body and body[-1].startswith("Writing 'next:start' to "):
        body.pop()
    return body


@golden_backed
@pytest.mark.parametrize("case", GOLDEN_CASES)
def test_golden_case(
    case: str, case_root: Path, monkeypatch: pytest.MonkeyPatch, capsys: pytest.CaptureFixture[str]
) -> None:
    if case.startswith("magpie") and not _magpie4_available():
        pytest.skip("the changelog bridge needs magpie4 in the local R library")
    run = _run_golden_case(case, case_root, monkeypatch, capsys)
    golden = run.golden
    # result.json: status and R's error message
    result_golden = json.loads((golden / "result.json").read_text())
    if result_golden["status"] == "ok":
        assert run.error is None, f"{case}: Python failed with {run.error!r}, R succeeded"
    else:
        assert run.error is not None, f"{case}: R failed with {result_golden['error']!r}, Python succeeded"
        assert str(run.error) == result_golden["error"]
    # README bytes (the temporary root stands for /p)
    readme = Path(run.effects.tempdir(), "README.md")
    if (golden / "README.md").is_file():
        assert run.norm(readme.read_text()) == (golden / "README.md").read_text()
    else:
        assert not readme.exists()
    # .testsstatus after the run (the wrapper's next:start on success)
    assert Path(run.mydir, "../.testsstatus").read_text() == (golden / "testsstatus").read_text()
    # the state files by value
    for state_file in sorted((golden / "state").glob("*.json")):
        expected = json.loads(state_file.read_text())
        path = Path(run.mydir, str(expected["file"]))
        assert path.exists() == expected["exists"], f"{case}: {expected['file']} existence"
        if not expected["exists"]:
            continue
        if "read_error" in expected:  # R could not read the file (a corrupt state file): neither can the port
            with pytest.raises(RParityError, match=re.escape(str(expected["read_error"]))):
                read_rds(path, run.effects)
            continue
        if path.suffix == ".rds" and expected["class"] == "character":
            assert scalar(read_rds(path, run.effects)) == expected["value"][0], f"{case}: {expected['file']}"
        elif path.suffix == ".rds" and path.name == "gRS.rds":
            table = state.read_grs(path, run.effects)
            rows = {row["_row"]: row for row in table.to_json_rows()}
            golden_rows = {row["_row"]: row for row in expected["value"]["rows"]}
            assert set(rows) == set(golden_rows), f"{case}: gRS.rds run names"
            for name, golden_row in golden_rows.items():
                assert list(rows[name]) == list(golden_row), f"{case}: gRS.rds columns of {name}"
                assert rows[name] == golden_row, f"{case}: gRS.rds row {name}"
        elif path.suffix == ".rds":
            frame = state.read_runs_to_start(path, run.effects)
            assert state.run_names(frame) == [row["_row"] for row in expected["value"]["rows"]]
        else:
            assert ev.r_read_lines(run.norm(path.read_text())) == expected["value"], f"{case}: {expected['file']}"
    # unchanged.json flags
    unchanged = json.loads((golden / "unchanged.json").read_text())
    assert run.same == {key: value["same"] for key, value in unchanged.items()}
    # the Mattermost payload text (the HTTP effect for curl)
    assert [run.norm(payload_text(text)) for _url, text in run.effects.posts] == _golden_payloads(
        golden / "mattermost.json"
    )
    assert [url for url, _ in run.effects.posts] == [run.spec["mattermostToken"]] * len(run.effects.posts)
    # the traced commands as a multiset of (tool, argv, cwd); curl is the HTTP effect, grep a pipe
    assert _python_trace(run) == _golden_trace(golden / "trace.jsonl")
    # the changed paths of the tree (effects.json; RDS bytes differ between writers, so paths only)
    effects_golden = json.loads((golden / "effects.json").read_text())
    for key in ("created", "modified", "deleted"):
        assert run.diff[key] == {entry["path"] for entry in effects_golden[key]}, f"{case}: {key} paths"
    assert effects_golden["touched"] == []
    # stdout bytes and the stderr lines of evaluateRuns (stderr is not part of the stage-3 comparison; here it is
    # checked up to two documented differences: the changelog bridge names its own call, ``readRDS(report)``, where
    # R's in-process try() printed ``readRDS(file.path(i, "report.rds"))``, and a failed POST prints notify's
    # ``Mattermost notification failed: ...`` line where the real curl would have printed its own reason)
    assert run.norm(run.stdout) == (golden / "stdout.txt").read_text()
    stderr_lines = [
        line.replace("Error in readRDS(report) :", 'Error in readRDS(file.path(i, "report.rds")) :')
        for line in run.norm(run.stderr).splitlines()
        if not line.startswith("Mattermost notification failed: ")
    ]
    assert stderr_lines == _stderr_slice((golden / "stderr.txt").read_text().splitlines())
    # the changelog bridge output of the MAgPIE case
    if (golden / "data-changelog.csv").is_file():
        assert (
            Path(run.effects.tempdir(), "data-changelog.csv").read_bytes()
            == (golden / "data-changelog.csv").read_bytes()
        )
