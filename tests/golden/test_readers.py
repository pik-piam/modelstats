"""The readers on every fixture, cross-checked against the R goldens and R's ``readRDS``.

* every ``config.Rdata``, ``config.yml``, ``runstatistics.rda``, ``projectSummations.rds``,
  ``gRS.rds`` and ``runcode.rds`` below the fixture snapshot and the synthetic layer loads (one
  test per file, so the pass count is the number of files read);
* for every row of every detailed status golden, ``Runtime`` equals ``round(difftime(timeGAMSEnd,
  timeGAMSStart, units = "secs"))`` on the ``RTime`` instants read from the same
  ``runstatistics.rda``, or ``round(difftime(<frozen clock>, timePrepareStart))`` while the job is
  not ``"no"`` (``R/getRunStatus.R:309-314``); the three project-summation columns equal what
  ``read_rds`` gives for ``projectSummations.rds`` (lines 351-361); and ``Mif`` follows
  ``REMIND_generic_<cfg$title>.mif`` for REMIND and the ``validation.mif`` size for MAgPIE, the
  model taken from ``stats$config$model_name`` (lines 120-138);
* the four AMT state files of the fixture (``runcode.rds``, ``lastcommit.rds``, ``runsToStart.rds``,
  ``output/gRS.rds``) survive ``read_rds`` -> ``write_rds`` with R's ``identical()``.
"""

from __future__ import annotations

import json
import shutil
import subprocess
from collections.abc import Iterator
from dataclasses import dataclass
from pathlib import Path
from typing import Any

import pandas as pd
import pytest

from modelstats.config import ConfigKind, find_config_file, load_config
from modelstats.env import Mode
from modelstats.rdata_io import RTime, r_round, read_rds, scalar, write_rds
from modelstats.runstats import RunStatistics, read_run_statistics
from tests.conftest import FIXTURES, GOLDENS, SYNTHETIC, GoldenCase, golden_cases

pytestmark = pytest.mark.golden

READER_FILES = (
    "config.Rdata",
    "config.yml",
    "runstatistics.rda",
    "projectSummations.rds",
    "gRS.rds",
    "runcode.rds",
)
AMT_STATE_FILES = ("runcode.rds", "lastcommit.rds", "runsToStart.rds", "output/gRS.rds")
AMT_DIR = Path("projects/remind/modeltests/remind")
RSCRIPT = shutil.which("Rscript")


def reader_files() -> list[Path]:
    """Every reader input below ``<fixtures>/p`` and ``<synthetic>/p`` (empty when the trees are absent)."""
    files: list[Path] = []
    for root in (FIXTURES, SYNTHETIC):
        if (root / "p").is_dir():
            for name in READER_FILES:
                files.extend(sorted((root / "p").rglob(name)))
    return files


def file_id(path: Path) -> str:
    """``fixtures/...`` or ``synthetic/...`` relative to the configured root (``tests/conftest.py``).

    ``MODELSTATS_FIXTURES`` and ``MODELSTATS_SYNTHETIC`` may point outside ``migration/``; the id is
    built against whichever root holds the file, so collection never fails on an external tree.
    """
    for label, root in (("fixtures", FIXTURES), ("synthetic", SYNTHETIC)):
        if path.is_relative_to(root):
            return f"{label}/{path.relative_to(root)}"
    return str(path)


def overlay(relative: str | Path) -> Path | None:
    """The file the sandbox overlay shows for a fixture-relative path: fixtures win over synthetic."""
    for root in (FIXTURES, SYNTHETIC):
        candidate = root / "p" / relative
        if candidate.exists():
            return candidate
    return None


# ---------------------------------------------------------------------------------------------
# Every file reads
# ---------------------------------------------------------------------------------------------


@pytest.mark.parametrize("path", reader_files(), ids=file_id)
def test_reader_reads_the_fixture_file(path: Path) -> None:
    if path.name in ("config.Rdata", "config.yml"):
        cfg = load_config(path)
        assert cfg.kind is (ConfigKind.YAML if path.name == "config.yml" else ConfigKind.RDATA)
        assert cfg.model_name in ("REMIND", "MAgPIE"), cfg.model_name
        assert isinstance(cfg.title, str) and cfg.title
        assert isinstance(cfg.optimization, str), cfg.optimization
    elif path.name == "runstatistics.rda":
        stats = read_run_statistics(path)
        assert stats is not None and stats.names
        assert stats.model_name in ("REMIND", "MAgPIE", None)
        for time in (stats.time_prepare_start, stats.time_gams_start, stats.time_gams_end):
            assert time is None or time.is_na or time.local() is not None
    elif path.name == "projectSummations.rds":
        summations = read_rds(path)
        assert isinstance(summations, dict) and "ScenarioMIP" in summations
        for key in ("missingVars", "checkSummations", "checkSummationsRegional"):
            value = scalar(summations["ScenarioMIP"].get(key))
            assert value is None or isinstance(value, int | float)
    elif path.name == "gRS.rds":
        frame = read_rds(path)
        assert isinstance(frame, pd.DataFrame) and len(frame) > 0 and "RunType" in frame.columns
    else:
        assert path.name == "runcode.rds"
        runcode = scalar(read_rds(path))
        assert isinstance(runcode, str) and "AMT" in runcode


# ---------------------------------------------------------------------------------------------
# Cross-checks against the status goldens
# ---------------------------------------------------------------------------------------------


@dataclass(frozen=True)
class StatusRow:
    case: GoldenCase
    mode: Mode
    run: str  # fixture-relative run directory
    row: dict[str, Any]

    @property
    def id(self) -> str:
        return f"{self.case.id}--{self.mode.value}--{Path(self.run).name}"


def status_rows() -> Iterator[StatusRow]:
    """Every (case, mode, run) of the detailed status goldens whose basenames are unique (BUG-022)."""
    for mode in Mode:
        for case in golden_cases("status", mode):
            golden = GOLDENS / "status" / mode.value / "detailed" / f"{case.id}.json"
            data = json.loads(golden.read_text(encoding="utf-8"))
            if "error" in data:
                continue
            rows = {row["_row"]: row for row in data["rows"]}
            runs = case.row["dirs"].split(",")
            if len({Path(run).name for run in runs}) != len(runs):
                continue
            for run in runs:
                if Path(run).name in rows:
                    yield StatusRow(case, mode, run, rows[Path(run).name])


STATUS_ROWS = list(status_rows()) if (FIXTURES / "p").is_dir() else []


def statistics_of(run: str) -> RunStatistics | None:
    path = overlay(Path(run) / "runstatistics.rda")
    return None if path is None else read_run_statistics(path)


def expected_runtime(status: StatusRow, stats: RunStatistics | None) -> int | None:
    """``R/getRunStatus.R:309-314`` on the instants of ``runstatistics.rda`` and the sandbox clock."""
    if stats is None:
        return None
    if stats.has_name("GAMSEnd"):
        secs = stats.gams_wall_seconds()
    elif stats.has_name("timePrepareStart") and status.row["jobInSLURM"] != "no":
        now = RTime.from_datetime(status.case.frozen)  # Sys.time() under the stopped clock
        secs = None if stats.time_prepare_start is None else now.elapsed_since(stats.time_prepare_start)
    else:
        return None
    return None if secs is None else r_round(secs)


@pytest.mark.parametrize("status", STATUS_ROWS, ids=lambda status: status.id)
def test_runtime_golden_equals_rounded_difftime(status: StatusRow) -> None:
    """``Runtime`` from ``timeGAMSEnd - timeGAMSStart``, or from the frozen clock while running."""
    assert status.row["Runtime"] == expected_runtime(status, statistics_of(status.run))


def is_magpie(stats: RunStatistics | None) -> bool:
    """``isTRUE(runstatistics$stats[["config"]][["model_name"]] == "MAgPIE")``: REMIND without the file."""
    return stats is not None and stats.is_magpie


@pytest.mark.parametrize("status", STATUS_ROWS, ids=lambda status: status.id)
def test_project_summation_columns_equal_the_rds(status: StatusRow) -> None:
    """``R/getRunStatus.R:316-323,351-361``: the three ``ScenarioMIP`` values, NA without the file.

    R assigns the columns only for a REMIND run with a config file and a mif (``Mif`` yes/sumErr);
    a golden row without them (single-directory call) must be such a run's complement.
    """
    columns = ("missingProjVars", "projSummationErrors", "projSummationErrorsRegional")
    remind_with_mif = status.row["Mif"] in ("yes", "sumErr") and not is_magpie(statistics_of(status.run))
    if "missingProjVars" not in status.row:
        assert not remind_with_mif
        return
    path = overlay(Path(status.run) / "projectSummations.rds")
    if path is None or not remind_with_mif:
        assert all(status.row[column] is None for column in columns)
        return
    scenario_mip = read_rds(path)["ScenarioMIP"]
    keys = ("missingVars", "checkSummations", "checkSummationsRegional")
    assert [status.row[column] for column in columns] == [scalar(scenario_mip.get(key)) for key in keys]


def expected_mif(status: StatusRow) -> str:
    """``R/getRunStatus.R:120-138``: the config title, the model of ``runstatistics.rda``, the files."""
    run_dir = overlay(status.run)
    assert run_dir is not None
    matches = find_config_file(run_dir)
    if len(matches.names) != 1:  # none: "NA"; two or more: the R call errors and has no golden row
        return "NA"
    if is_magpie(statistics_of(status.run)):
        validation = overlay(Path(status.run) / "validation.mif")
        return "yes" if validation is not None and validation.stat().st_size > 99999 else "no"
    cfg = load_config(matches.paths[0])
    if overlay(Path(status.run) / f"REMIND_generic_{cfg.title}.mif") is None:
        return "no"
    if overlay(Path(status.run) / f"REMIND_generic_{cfg.title}_summation_errors.csv") is not None:
        return "sumErr"
    return "yes"


@pytest.mark.parametrize("status", STATUS_ROWS, ids=lambda status: status.id)
def test_mif_column_follows_the_config_title_and_the_model(status: StatusRow) -> None:
    assert status.row["Mif"] == expected_mif(status)


# ---------------------------------------------------------------------------------------------
# AMT state files through R
# ---------------------------------------------------------------------------------------------


@pytest.mark.parametrize("name", AMT_STATE_FILES)
def test_amt_state_file_round_trips_through_r_identical(
    name: str, fixtures_dir: Path, tmp_path: Path
) -> None:
    """``read_rds`` -> ``write_rds`` -> R ``identical(readRDS(original), readRDS(copy))`` is TRUE."""
    if RSCRIPT is None:
        pytest.fail("Rscript is required for the readRDS round trip (migration/harness/README.md)")
    original = fixtures_dir / "p" / AMT_DIR / name
    copy = tmp_path / Path(name).name
    write_rds(copy, read_rds(original))
    code = f'cat(identical(readRDS("{original}"), readRDS("{copy}")))'
    result = subprocess.run([RSCRIPT, "-e", code], capture_output=True, text=True, check=False)
    assert result.returncode == 0, result.stderr
    assert result.stdout.strip() == "TRUE", f"{name}: {result.stdout} {result.stderr}"
