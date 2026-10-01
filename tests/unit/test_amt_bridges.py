"""``modelstats.amt.bridges``: the two Rscript contracts of plan 03 section 2.3.

Two tiers:

- always: a scripted Effects double answers :meth:`Effects.run` with canned bridge output, so the argv,
  cwd and environment of each bridge, the last-line JSON protocol, the stdout/stderr pass-through and the
  error mapping (R error -> ``RParityError`` for ``select_scenarios``, ``ok=False`` without raising for
  ``add_to_data_changelog``, ``BridgeError`` for a bridge that cannot run) are checked without R; when
  ``Rscript`` is on the PATH the three R scripts are additionally parsed for syntax.
- sandbox: the real scripts against the fake checkout inside ``migration/harness/sandbox.sh`` (the real
  ``Rscript`` through the fake's pass-through, the overlay's writable copy of the fixtures). One sandbox
  run per bridge executes this file's driver mode (``--sandbox-driver select|changelog``), which runs the
  success and failure scenarios of the amt cases through a tracing ProductionEffects and writes a JSON
  report plus the produced files to ``/out``. Skipped (with the reason) when the fixtures, the synthetic
  fake checkout, ``bwrap``, ``Rscript`` or the worktree's ``.venv`` are absent; any other failure fails.

Oracle facts pinned here (R 4.6.1, magpie4 2.83.0, verified 2026-10-01): ``saveRDS()`` of the
selectScenarios data.frame with ``-AMT`` row names has the sha256 the ``remind-start`` golden records for
``runsToStart.rds``; the changelog CSVs equal the ``data-changelog.csv`` goldens of ``magpie-evaluate-recent``
(16 rows merged and cut to magpie4's 15), ``magpie-evaluate-changelog-missing`` (a fresh file with the new
row only, without the three ``lucEmis`` columns of the old file), ``magpie-evaluate-bridge-fail`` and
``magpie-evaluate-report-missing`` (untouched copies of the gitdir file).
"""

from __future__ import annotations

import contextlib
import hashlib
import io
import json
import os
import shutil
import subprocess
import sys
import tempfile
from collections.abc import Mapping, Sequence
from pathlib import Path
from typing import Any

import pytest

from modelstats.amt import bridges
from modelstats.amt.bridges import (
    ADD_TO_DATA_CHANGELOG_SCRIPT,
    BRIDGE_ENV,
    SELECT_SCENARIOS_SCRIPT,
    BridgeError,
    ChangelogResult,
    SelectScenariosResult,
    add_to_data_changelog,
    bridge_scripts_dir,
    select_scenarios,
)
from modelstats.env import PathLike, ProductionEffects
from modelstats.errors import RParityError

REPO = Path(__file__).resolve().parents[2]
MIGRATION = REPO / "migration"
SANDBOX = MIGRATION / "harness" / "sandbox.sh"
FIXTURES_PROBE = MIGRATION / "fixtures" / "p" / "projects"
FAKE_REMIND = MIGRATION / "synthetic" / "p" / "projects" / "remind" / "modeltests" / "remind"
FAKE_REMIND_PROBE = FAKE_REMIND / "scripts" / "start"
GOLDENS_AMT = MIGRATION / "goldens" / "amt"
VENV_PYTHON = REPO / ".venv" / "bin" / "python"

# inside the sandbox (oncluster mode)
REMIND_MYDIR = "/p/projects/remind/modeltests/remind"
MAGPIE_OUTPUT = "/p/projects/landuse/tests/magpie/output"
MAGPIE_GITDIR = "/p/projects/landuse/tests/testing_suite"
MAGPIE_DEFAULT_RUN = "default_2026-09-19_04.05.47"
OUT = "/out"

# the row names of the remind-start golden (state/runsToStart_rds.json), in selectScenarios order
REMIND_TITLES = [
    "SSP2-NPi",
    "default",
    "SSP2-EU21-PkBudg650",
    "SSP2-NPi2025-calibrate",
    "SSP3-NPi2025",
    "SSP2-EcBudg500",
    "SSP2-EU21-NPi2025",
    "testOneRegi",
    "SSP2-never",
]


def _sha256(path: PathLike) -> str:
    return hashlib.sha256(Path(path).read_bytes()).hexdigest()


def _digest(path: PathLike, algorithm: str) -> str:
    return hashlib.new(algorithm, Path(path).read_bytes()).hexdigest()


# ---------------------------------------------------------------------------
# tier 1: scripted bridge answers (no R)
# ---------------------------------------------------------------------------


class ScriptedEffects(ProductionEffects):
    """ProductionEffects whose ``run()`` records the call and answers from a queue of (stdout, stderr, status)."""

    def __init__(self, answers: Sequence[tuple[str, str, int]]) -> None:
        self.answers = list(answers)
        self.runs: list[dict[str, Any]] = []

    def run(
        self,
        argv: Sequence[str],
        cwd: PathLike | None = None,
        env: Mapping[str, str] | None = None,
        input: str | None = None,
    ) -> subprocess.CompletedProcess[str]:
        cwd_s = None if cwd is None else os.fspath(cwd)
        self.runs.append({"argv": list(argv), "cwd": cwd_s, "env": None if env is None else dict(env)})
        stdout, stderr, status = self.answers.pop(0)
        return subprocess.CompletedProcess(list(argv), status, stdout, stderr)


def _select_ok(row_names: list[str], out: str) -> str:
    return (
        json.dumps(
            {
                "bridge": "select_scenarios",
                "ok": True,
                "row_names": row_names,
                "columns": ["start", "description"],
                "nrow": len(row_names),
                "out": out,
                "sources": [{"file": "scripts/start/selectScenarios.R", "algorithm": "sha256", "digest": "ab" * 32}],
                "r_version": "R version 4.6.1 (2026-06-24)",
            }
        )
        + "\n"
    )


def _changelog_answer(ok: bool, **extra: Any) -> str:
    base: dict[str, Any] = {"bridge": "add_to_data_changelog", "ok": ok, "magpie4_version": "2.83.0", "r_version": "R"}
    return json.dumps({**base, **extra}) + "\n"


def test_scripts_are_package_data() -> None:
    with bridge_scripts_dir() as directory:
        assert directory.is_dir()
        for name in (SELECT_SCENARIOS_SCRIPT, ADD_TO_DATA_CHANGELOG_SCRIPT, "_json.R"):
            assert (directory / name).is_file(), name
        # the scripts source their sibling _json.R relative to their own --file= path
        for name in (SELECT_SCENARIOS_SCRIPT, ADD_TO_DATA_CHANGELOG_SCRIPT):
            assert '"_json.R"' in (directory / name).read_text(encoding="utf-8")


@pytest.mark.skipif(shutil.which("Rscript") is None, reason="Rscript not on PATH")
def test_scripts_parse_in_r() -> None:
    with bridge_scripts_dir() as directory:
        for name in (SELECT_SCENARIOS_SCRIPT, ADD_TO_DATA_CHANGELOG_SCRIPT, "_json.R"):
            cmd = ["Rscript", "-e", f"invisible(parse('{directory / name}'))"]
            proc = subprocess.run(cmd, capture_output=True, text=True, check=False)
            assert proc.returncode == 0, f"{name}: {proc.stderr}"


def test_select_scenarios_argv_cwd_env_and_result(capsys: pytest.CaptureFixture[str]) -> None:
    eff = ScriptedEffects([("sourced REMIND noise\n" + _select_ok(["a-AMT", "b-AMT"], "/m/runsToStart.rds"), "", 0)])
    res = select_scenarios("/m", "/m/runsToStart.rds", row_name_suffix="-AMT", effects=eff)
    assert isinstance(res, SelectScenariosResult)
    assert res.row_names == ["a-AMT", "b-AMT"]
    assert res.columns == ["start", "description"]
    assert res.nrow == 2
    assert res.rds_path == "/m/runsToStart.rds"
    assert res.sources[0].file == "scripts/start/selectScenarios.R"
    assert (res.sources[0].algorithm, res.sources[0].digest) == ("sha256", "ab" * 32)
    assert res.r_version.startswith("R version")
    [call] = eff.runs
    assert call["cwd"] == "/m"
    assert call["env"] == dict(BRIDGE_ENV) == {"autoRenvFixDeps": "TRUE", "LC_ALL": "C.utf8", "TZ": "Europe/Berlin"}
    argv = call["argv"]
    assert argv[0] == "Rscript"
    assert argv[1].endswith(f"/bridge_scripts/{SELECT_SCENARIOS_SCRIPT}")
    assert argv[2:] == [
        "--out", "/m/runsToStart.rds",
        "--config", "config/scenario_config.csv",
        "--startgroup", "AMT",
        "--scripts", "scripts/start",
        "--suffix", "-AMT",
    ]  # fmt: skip
    assert res.argv == argv
    # the fake Rscript of the harness intercepts only `start.R`: no bridge argument may look like one
    assert not any(a == "start.R" or a.endswith("/start.R") for a in argv)
    captured = capsys.readouterr()
    assert captured.out == "sourced REMIND noise\n"  # everything before the JSON line is passed through
    assert captured.err == ""


def test_select_scenarios_without_suffix_omits_the_option() -> None:
    eff = ScriptedEffects([(_select_ok(["a"], "raw.rds"), "", 0)])
    res = select_scenarios("/m", "raw.rds", startgroup="XY", config="c.csv", scripts_dir="s", effects=eff)
    assert res.row_names == ["a"]
    assert eff.runs[0]["argv"][2:] == ["--out", "raw.rds", "--config", "c.csv", "--startgroup", "XY", "--scripts", "s"]


def test_select_scenarios_r_error_is_rparity_error(capsys: pytest.CaptureFixture[str]) -> None:
    answer = json.dumps(
        {"bridge": "select_scenarios", "ok": False, "error": "cannot open the connection", "call": 'file(file, "rt")'}
    )
    eff = ScriptedEffects([(answer + "\n", 'Warning message:\nIn file(file, "rt") :\n  cannot open file\n', 1)])
    with pytest.raises(RParityError) as excinfo:
        select_scenarios("/m", "/m/runsToStart.rds", effects=eff)
    assert str(excinfo.value) == "cannot open the connection"
    assert excinfo.value.call == 'file(file, "rt")'
    captured = capsys.readouterr()
    assert captured.out == ""
    assert captured.err.startswith("Warning message:\n")  # R's stderr is passed through before raising


def test_select_scenarios_without_json_is_bridge_error(capsys: pytest.CaptureFixture[str]) -> None:
    eff = ScriptedEffects([("partial output\n", "Error: boom\nExecution halted\n", 1)])
    with pytest.raises(BridgeError) as excinfo:
        select_scenarios("/m", "x.rds", effects=eff)
    err = excinfo.value
    assert "no JSON answer" in str(err)
    assert str(err).startswith("bridge select_scenarios.R:")
    assert err.returncode == 1
    assert err.stdout == "partial output\n"
    assert err.stderr == "Error: boom\nExecution halted\n"
    captured = capsys.readouterr()
    assert captured.out == "partial output\n"  # passed through in full when there is no JSON line
    assert "Execution halted" in captured.err


def test_rscript_missing_is_bridge_error() -> None:
    eff = ScriptedEffects([("", "sh: Rscript: command not found\n", 127)])
    with pytest.raises(BridgeError, match="could not be run \\(status 127\\)"):
        add_to_data_changelog("r.rds", "c.csv", "v", effects=eff)


def test_contradicting_answer_is_bridge_error() -> None:
    eff = ScriptedEffects([(_changelog_answer(True, changelog="c.csv", version_id="v", nrow=1), "", 1)])
    with pytest.raises(BridgeError, match="contradicts exit status 1"):
        add_to_data_changelog("r.rds", "c.csv", "v", effects=eff)


def test_add_to_data_changelog_success_argv_cwd_env() -> None:
    answer = _changelog_answer(True, changelog="/t/data-changelog.csv", version_id="v1", nrow=15)
    eff = ScriptedEffects([(answer, "", 0)])
    res = add_to_data_changelog("v1/report.rds", "/t/data-changelog.csv", "v1", cwd="/p/out", effects=eff)
    assert isinstance(res, ChangelogResult)
    assert res.ok is True
    assert res.error is None and res.call is None
    assert res.nrow == 15
    assert res.changelog == "/t/data-changelog.csv"
    assert res.version_id == "v1"
    assert res.magpie4_version == "2.83.0"
    [call] = eff.runs
    assert call["cwd"] == "/p/out"
    assert call["env"] == dict(BRIDGE_ENV)
    argv = call["argv"]
    assert argv[0] == "Rscript"
    assert argv[1].endswith(f"/bridge_scripts/{ADD_TO_DATA_CHANGELOG_SCRIPT}")
    assert argv[2:] == ["--report", "v1/report.rds", "--changelog", "/t/data-changelog.csv", "--version-id", "v1"]
    assert res.argv == argv


def test_add_to_data_changelog_inherits_cwd_by_default() -> None:
    eff = ScriptedEffects([(_changelog_answer(True, changelog="c.csv", version_id="v", nrow=1), "", 0)])
    add_to_data_changelog("r.rds", "c.csv", "v", effects=eff)
    assert eff.runs[0]["cwd"] is None


def test_add_to_data_changelog_r_error_does_not_raise(capsys: pytest.CaptureFixture[str]) -> None:
    stderr = "Error in readRDS(report) : unknown input format\n"
    eff = ScriptedEffects([(_changelog_answer(False, error="unknown input format", call="readRDS(report)"), stderr, 1)])
    res = add_to_data_changelog("x/report.rds", "c.csv", "x", effects=eff)
    assert res.ok is False
    assert res.error == "unknown input format"
    assert res.call == "readRDS(report)"
    assert res.nrow is None
    assert res.magpie4_version == "2.83.0"
    captured = capsys.readouterr()
    assert captured.err == stderr  # R's try() text, printed by the script, reaches stderr
    assert captured.out == ""


def test_bridge_env_is_read_only() -> None:
    with pytest.raises(TypeError):
        BRIDGE_ENV["X"] = "1"  # type: ignore[index]


def test_module_exports() -> None:
    for name in bridges.__all__:
        assert hasattr(bridges, name), name


# ---------------------------------------------------------------------------
# tier 2: the real scripts against the fake checkout inside the sandbox
# ---------------------------------------------------------------------------


def _sandbox_skip_reason() -> str | None:
    if not SANDBOX.is_file():
        return f"{SANDBOX} is absent (no migration/ tree)"
    if not FIXTURES_PROBE.is_dir():
        return f"fixture tree {FIXTURES_PROBE} is absent"
    if not FAKE_REMIND_PROBE.is_dir():
        return f"fake REMIND checkout {FAKE_REMIND_PROBE} is absent (run migration/harness/build_fake_remind.sh)"
    if shutil.which("bwrap") is None:
        return "bwrap (bubblewrap) not installed"
    if shutil.which("Rscript") is None:
        return "Rscript not on PATH"
    if not VENV_PYTHON.exists():
        return f"{VENV_PYTHON} is absent (run uv sync)"
    return None


def _run_sandbox(case_dir: Path, scenario: str) -> dict[str, Any]:
    """One sandbox invocation of this file's driver; returns the report it wrote to /out/report.json."""
    case_dir.mkdir(parents=True, exist_ok=True)
    cmd = [
        str(SANDBOX), str(case_dir), "--mode", "oncluster", "--quiet", "--env", "PYTHONDONTWRITEBYTECODE=1",
        "--", str(VENV_PYTHON), str(Path(__file__).resolve()),
        "--sandbox-driver", scenario, "--out", f"{OUT}/report.json",
    ]  # fmt: skip
    log = case_dir / "sandbox.log"
    with log.open("wb") as handle:
        proc = subprocess.run(cmd, cwd=REPO, stdout=handle, stderr=subprocess.STDOUT, timeout=900, check=False)
    report_path = case_dir / "out" / "report.json"
    if proc.returncode != 0 or not report_path.is_file():
        tail = log.read_text(encoding="utf-8", errors="replace")[-4000:]
        pytest.fail(f"sandbox driver {scenario} failed with status {proc.returncode} (log {log}):\n{tail}")
    report: dict[str, Any] = json.loads(report_path.read_text(encoding="utf-8"))
    report["_case_dir"] = str(case_dir)
    return report


@pytest.fixture(scope="session")
def sandbox_reports(tmp_path_factory: pytest.TempPathFactory) -> dict[str, dict[str, Any]]:
    reason = _sandbox_skip_reason()
    if reason is not None:
        pytest.skip(f"bridge sandbox tests skipped: {reason}")
    root = tmp_path_factory.mktemp("p5-bridges")
    return {scenario: _run_sandbox(root / scenario, scenario) for scenario in ("select", "changelog")}


def _golden_bytes(case: str, name: str) -> bytes:
    path = GOLDENS_AMT / case / name
    assert path.is_file(), path
    return path.read_bytes()


def _golden_state(case: str, name: str) -> dict[str, Any]:
    data: dict[str, Any] = json.loads((GOLDENS_AMT / case / "state" / name).read_text(encoding="utf-8"))
    return data


def _check_bridge_trace(entry: Mapping[str, Any], script: str, cwd: str) -> list[str]:
    argv: list[str] = entry["argv"]
    assert argv[0] == "Rscript"
    assert argv[1].endswith(f"/bridge_scripts/{script}")
    assert entry["cwd"] == cwd
    assert entry["env"] == dict(BRIDGE_ENV)
    return argv


def test_sandbox_select_scenarios_rows(sandbox_reports: dict[str, dict[str, Any]]) -> None:
    rep = sandbox_reports["select"]
    golden_rows = [row["_row"] for row in _golden_state("remind-start", "runsToStart_rds.json")["value"]["rows"]]
    assert rep["raw"]["row_names"] == REMIND_TITLES
    assert rep["amt"]["row_names"] == [t + "-AMT" for t in REMIND_TITLES] == golden_rows
    for key in ("raw", "amt"):
        assert rep[key]["columns"] == ["start", "description"]
        assert rep[key]["nrow"] == 9
        [source] = rep[key]["sources"]
        assert source["file"] == "scripts/start/selectScenarios.R"
        # the pinned source: the digest of the fake selectScenarios.R (tools::sha256sum on R >= 4.5, else md5sum)
        assert (source["algorithm"], len(source["digest"])) in (("sha256", 64), ("md5", 32))
        assert source["digest"] == _digest(FAKE_REMIND / "scripts" / "start" / "selectScenarios.R", source["algorithm"])
        assert rep[key]["r_version"].startswith("R version 4.")
    assert rep["stdout"] == ""  # the fake checkout's scripts print nothing
    assert rep["stderr_select"] == ""


def test_sandbox_select_scenarios_rds_is_byte_compatible_with_r(sandbox_reports: dict[str, dict[str, Any]]) -> None:
    rep = sandbox_reports["select"]
    golden = _golden_state("remind-start", "runsToStart_rds.json")
    # what startRuns() saved in the R golden run, byte for byte
    assert rep["amt"]["sha256"] == golden["sha256"]
    # and identical() to R's literal lines 127-137 evaluated in the same sandbox
    assert rep["identical_to_r"] is True
    assert rep["amt"]["sha256"] == rep["reference_sha256"]
    assert rep["raw"]["sha256"] != rep["amt"]["sha256"]
    assert rep["raw"]["size"] > 0


def test_sandbox_select_scenarios_r_error(sandbox_reports: dict[str, dict[str, Any]]) -> None:
    rep = sandbox_reports["select"]
    assert rep["error"]["type"] == "RParityError"
    assert rep["error"]["message"] == "cannot open the connection"
    assert rep["error"]["call"] == 'file(file, "rt")'
    assert rep["error"]["rds_written"] is False
    assert "cannot open file 'config/missing.csv': No such file or directory" in rep["stderr_error"]


def test_sandbox_select_scenarios_trace(sandbox_reports: dict[str, dict[str, Any]]) -> None:
    rep = sandbox_reports["select"]
    trace = rep["trace"]
    assert [t["label"] for t in trace] == ["raw", "amt", "error"]
    for entry in trace:
        argv = _check_bridge_trace(entry, SELECT_SCENARIOS_SCRIPT, REMIND_MYDIR)
        assert argv[2:4] == ["--out", f"{OUT}/{entry['label']}.rds"]
        assert "--startgroup" in argv and argv[argv.index("--startgroup") + 1] == "AMT"
        assert argv[argv.index("--scripts") + 1] == "scripts/start"
    assert trace[0]["argv"][argv.index("--config") + 1] == "config/scenario_config.csv"
    assert "--suffix" not in trace[0]["argv"]
    assert trace[1]["argv"][-2:] == ["--suffix", "-AMT"]
    assert trace[2]["argv"][trace[2]["argv"].index("--config") + 1] == "config/missing.csv"
    assert rep["trace_count"] == 3  # the R reference run is a plain subprocess, not an effect


def test_sandbox_changelog_success_matches_golden(sandbox_reports: dict[str, dict[str, Any]]) -> None:
    rep = sandbox_reports["changelog"]
    case_dir = Path(rep["_case_dir"])
    res = rep["success"]
    assert res["ok"] is True
    assert res["error"] is None and res["call"] is None
    assert res["nrow"] == 15
    assert res["magpie4_version"]
    golden = _golden_bytes("magpie-evaluate-recent", "data-changelog.csv")
    assert (case_dir / "out" / "success.csv").read_bytes() == golden
    assert res["stderr"] == ""


def test_sandbox_changelog_missing_file_matches_golden(sandbox_reports: dict[str, dict[str, Any]]) -> None:
    rep = sandbox_reports["changelog"]
    case_dir = Path(rep["_case_dir"])
    res = rep["changelog_missing"]
    assert res["ok"] is True
    assert res["nrow"] == 1
    assert (case_dir / "out" / "changelog-missing.csv").read_bytes() == _golden_bytes(
        "magpie-evaluate-changelog-missing", "data-changelog.csv"
    )


def test_sandbox_changelog_corrupt_report(sandbox_reports: dict[str, dict[str, Any]]) -> None:
    rep = sandbox_reports["changelog"]
    case_dir = Path(rep["_case_dir"])
    res = rep["bridge_fail"]
    assert res["ok"] is False
    assert res["error"] == "unknown input format"
    assert res["call"] == "readRDS(report)"
    assert res["nrow"] is None
    assert res["stderr"] == "Error in readRDS(report) : unknown input format\n"
    gitdir_copy = _golden_bytes("magpie-evaluate-bridge-fail", "data-changelog.csv")
    assert (case_dir / "out" / "bridge-fail.csv").read_bytes() == gitdir_copy
    assert res["changelog_sha256_before"] == res["changelog_sha256_after"] == hashlib.sha256(gitdir_copy).hexdigest()


def test_sandbox_changelog_missing_report(sandbox_reports: dict[str, dict[str, Any]]) -> None:
    rep = sandbox_reports["changelog"]
    case_dir = Path(rep["_case_dir"])
    res = rep["report_missing"]
    assert res["ok"] is False
    assert res["error"] == "cannot open the connection"
    assert res["call"] == 'gzfile(file, "rb")'
    assert res["stderr"] == (
        'Error in gzfile(file, "rb") : cannot open the connection\n'
        "In addition: Warning message:\n"
        'In gzfile(file, "rb") :\n'
        f"  cannot open compressed file '{MAGPIE_DEFAULT_RUN}/report.rds', "
        "probable reason 'No such file or directory'\n"
    )
    assert (case_dir / "out" / "report-missing.csv").read_bytes() == _golden_bytes(
        "magpie-evaluate-report-missing", "data-changelog.csv"
    )
    assert res["changelog_sha256_before"] == res["changelog_sha256_after"]


def test_sandbox_changelog_trace(sandbox_reports: dict[str, dict[str, Any]]) -> None:
    rep = sandbox_reports["changelog"]
    trace = rep["trace"]
    assert [t["label"] for t in trace] == ["success", "changelog_missing", "bridge_fail", "report_missing"]
    for entry in trace:
        argv = _check_bridge_trace(entry, ADD_TO_DATA_CHANGELOG_SCRIPT, MAGPIE_OUTPUT)
        assert argv[2:4] == ["--report", f"{MAGPIE_DEFAULT_RUN}/report.rds"]
        assert argv[4] == "--changelog" and argv[5].endswith("/data-changelog.csv")
        assert argv[6:] == ["--version-id", MAGPIE_DEFAULT_RUN]
    assert rep["stdout"] == ""


# ---------------------------------------------------------------------------
# driver mode: runs inside the sandbox (python <this file> --sandbox-driver select|changelog --out FILE)
# ---------------------------------------------------------------------------


class TracingEffects(ProductionEffects):
    """ProductionEffects that records every ``run()`` (argv, cwd, env) and delegates to the real subprocess."""

    def __init__(self) -> None:
        self.trace: list[dict[str, Any]] = []
        self.label = ""

    def run(
        self,
        argv: Sequence[str],
        cwd: PathLike | None = None,
        env: Mapping[str, str] | None = None,
        input: str | None = None,
    ) -> subprocess.CompletedProcess[str]:
        self.trace.append(
            {
                "label": self.label,
                "tool": os.path.basename(argv[0]),
                "argv": list(argv),
                "cwd": None if cwd is None else os.fspath(cwd),
                "env": None if env is None else dict(env),
            }
        )
        return super().run(argv, cwd=cwd, env=env, input=input)


def _file_info(path: str) -> dict[str, Any]:
    p = Path(path)
    return {"path": path, "exists": p.is_file(), "size": p.stat().st_size if p.is_file() else None}


def _driver_select(out_dir: str) -> dict[str, Any]:
    eff = TracingEffects()
    report: dict[str, Any] = {"scenario": "select", "mydir": REMIND_MYDIR}
    stdout, stderr = io.StringIO(), io.StringIO()
    with contextlib.redirect_stdout(stdout), contextlib.redirect_stderr(stderr):
        eff.label = "raw"
        raw = select_scenarios(REMIND_MYDIR, f"{out_dir}/raw.rds", effects=eff)
        eff.label = "amt"
        amt = select_scenarios(REMIND_MYDIR, f"{out_dir}/amt.rds", row_name_suffix="-AMT", effects=eff)
    report["stdout"] = stdout.getvalue()
    report["stderr_select"] = stderr.getvalue()
    for label, res in (("raw", raw), ("amt", amt)):
        report[label] = {
            **dataclasses_dict(res),
            **_file_info(res.rds_path),
            "sha256": _sha256(res.rds_path),
        }
    # the failure path: an unreadable settings file (R: read.csv2 -> file(file, "rt") -> cannot open the connection)
    stderr = io.StringIO()
    error: dict[str, Any] = {"type": None}
    with contextlib.redirect_stderr(stderr):
        eff.label = "error"
        try:
            select_scenarios(REMIND_MYDIR, f"{out_dir}/error.rds", config="config/missing.csv", effects=eff)
        except RParityError as exc:
            error = {"type": "RParityError", "message": str(exc), "call": exc.call}
        except BridgeError as exc:
            error = {"type": "BridgeError", "message": str(exc), "stderr": exc.stderr}
    error["rds_written"] = Path(f"{out_dir}/error.rds").exists()
    report["error"] = error
    report["stderr_error"] = stderr.getvalue()
    # R's literal startRuns() lines 127-137 in the same sandbox, as the oracle for identical() and the bytes
    r_code = (
        'settings <- read.csv2("config/scenario_config.csv", stringsAsFactors = FALSE, row.names = 1,'
        ' comment.char = "#", na.strings = "");'
        ' invisible(sapply(list.files("scripts/start", pattern = "\\\\.R$", full.names = TRUE), source));'
        " f <- function() { selectScenarios <- NA;"
        ' runsToStart <- selectScenarios(settings = settings, interactive = FALSE, startgroup = "AMT");'
        ' row.names(runsToStart) <- paste0(row.names(runsToStart), "-AMT"); runsToStart };'
        f' r <- f(); saveRDS(r, "{out_dir}/reference.rds");'
        f' cat(identical(readRDS("{out_dir}/amt.rds"), r), "\\n")'
    )
    proc = subprocess.run(["Rscript", "-e", r_code], cwd=REMIND_MYDIR, capture_output=True, text=True, check=False)
    report["reference_rscript"] = {"status": proc.returncode, "stdout": proc.stdout, "stderr": proc.stderr}
    report["identical_to_r"] = proc.stdout.strip() == "TRUE"
    reference = Path(f"{out_dir}/reference.rds")
    report["reference_sha256"] = _sha256(reference) if reference.is_file() else None
    report["trace"] = eff.trace
    report["trace_count"] = len(eff.trace)
    return report


def _driver_changelog(out_dir: str) -> dict[str, Any]:
    eff = TracingEffects()
    report: dict[str, Any] = {"scenario": "changelog", "cwd": MAGPIE_OUTPUT, "run": MAGPIE_DEFAULT_RUN}
    gitdir_changelog = f"{MAGPIE_GITDIR}/data-changelog.csv"
    report_rds = f"{MAGPIE_OUTPUT}/{MAGPIE_DEFAULT_RUN}/report.rds"
    tmp = tempfile.mkdtemp(prefix="p5-bridges-")  # R: tempdir(), under the sandbox's /tmp
    stdout = io.StringIO()

    def run_case(label: str, *, copy_changelog: bool, out_name: str) -> dict[str, Any]:
        changelog = f"{tmp}/{label}/data-changelog.csv"
        Path(changelog).parent.mkdir(parents=True, exist_ok=True)
        if copy_changelog:  # R: file.copy(file.path(gitdir, "data-changelog.csv"), changelog)
            shutil.copyfile(gitdir_changelog, changelog)
        before = _sha256(changelog) if Path(changelog).is_file() else None
        stderr = io.StringIO()
        with contextlib.redirect_stdout(stdout), contextlib.redirect_stderr(stderr):
            eff.label = label
            res = add_to_data_changelog(
                f"{MAGPIE_DEFAULT_RUN}/report.rds", changelog, MAGPIE_DEFAULT_RUN, cwd=MAGPIE_OUTPUT, effects=eff
            )
        exists = Path(changelog).is_file()
        if exists:
            shutil.copyfile(changelog, f"{out_dir}/{out_name}")
        return {
            **dataclasses_dict(res),
            "stderr": stderr.getvalue(),
            "changelog_exists": exists,
            "changelog_sha256_before": before,
            "changelog_sha256_after": _sha256(changelog) if exists else None,
        }

    # magpie-evaluate-recent: the gitdir changelog copied to tempdir, the default run's report.rds merged in
    report["success"] = run_case("success", copy_changelog=True, out_name="success.csv")
    # magpie-evaluate-changelog-missing: gitdir has no data-changelog.csv (file.copy FALSE), the bridge creates it
    report["changelog_missing"] = run_case("changelog_missing", copy_changelog=False, out_name="changelog-missing.csv")
    # magpie-evaluate-bridge-fail: prepare.sh writes `garbage` into report.rds (the overlay copy is writable)
    Path(report_rds).write_bytes(b"garbage\n")
    report["bridge_fail"] = run_case("bridge_fail", copy_changelog=True, out_name="bridge-fail.csv")
    # magpie-evaluate-report-missing: prepare.sh removes report.rds
    os.remove(report_rds)
    report["report_missing"] = run_case("report_missing", copy_changelog=True, out_name="report-missing.csv")
    report["stdout"] = stdout.getvalue()
    report["trace"] = eff.trace
    report["trace_count"] = len(eff.trace)
    return report


def dataclasses_dict(obj: Any) -> dict[str, Any]:
    import dataclasses

    return dataclasses.asdict(obj)


def _driver_main(argv: list[str]) -> int:
    scenario = argv[argv.index("--sandbox-driver") + 1]
    out_file = argv[argv.index("--out") + 1]
    out_dir = str(Path(out_file).parent)
    report = _driver_select(out_dir) if scenario == "select" else _driver_changelog(out_dir)
    Path(out_file).write_text(json.dumps(report, indent=2, ensure_ascii=False), encoding="utf-8")
    return 0


if __name__ == "__main__":
    sys.exit(_driver_main(sys.argv[1:]))
