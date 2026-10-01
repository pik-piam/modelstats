"""``modelstats.runstats``: ``runstatistics.rda`` as ``R/getRunStatus.R:76-79,90-94,107,310-313`` read it."""

from __future__ import annotations

from pathlib import Path

import numpy as np
import pytest
import rdata

from modelstats.rdata_io import RDataError, RTime, r_round
from modelstats.runstats import RunStatistics, read_run_statistics, run_statistics_from


@pytest.fixture(scope="module")
def remind(r_testdata_dir: Path) -> RunStatistics:
    stats = read_run_statistics(r_testdata_dir / "runstatistics.rda")
    assert stats is not None
    return stats


@pytest.fixture(scope="module")
def magpie(r_testdata_dir: Path) -> RunStatistics:
    stats = read_run_statistics(r_testdata_dir / "runstatistics_magpie.rda")
    assert stats is not None
    return stats


def test_remind_fields(remind: RunStatistics, r_testdata_dir: Path) -> None:
    assert remind.path == r_testdata_dir / "runstatistics.rda"
    assert remind.names == (
        "id",
        "config",
        "timePrepareStart",
        "timeGAMSStart",
        "timeGAMSEnd",
        "modelstat",
        "runtime",
        "user",
    )
    assert remind.config == {"model_name": "REMIND", "title": "SSP2-NPi-AMT"} or list(
        remind.config or {}
    ) == [
        "model_name",
        "title",
    ]
    assert remind.model_name == "REMIND" and not remind.is_magpie
    assert remind.id == "2026-09-25_10.11.12_abcdef"
    assert remind.modelstat == (2.0,)
    assert remind.time_prepare_start == RTime(1790323872.0, "Europe/Berlin")  # 2026-09-25 10:11:12 CEST
    assert remind.time_gams_start == RTime(1790324400.0, "Europe/Berlin")
    assert remind.time_gams_end == RTime(1790387130.5, "Europe/Berlin")
    assert remind.raw["user"].tolist() == ["tobiasdi"]


def test_name_tests_are_unanchored_regexes(remind: RunStatistics) -> None:
    """``any(grepl("GAMSEnd", names(stats)))`` and friends (``R/getRunStatus.R:90,92,107,310,312``)."""
    assert remind.has_name("config") and remind.has_name("modelstat") and remind.has_name("id")
    assert remind.has_name("GAMSEnd") and remind.has_name("timePrepareStart")
    assert not remind.has_name("^GAMSEnd") and not remind.has_name("absent")


def test_gams_wall_seconds_equals_r_difftime(remind: RunStatistics) -> None:
    secs = remind.gams_wall_seconds()
    assert secs == 62730.5  # R: difftime(timeGAMSEnd, timeGAMSStart, units = "secs")
    assert r_round(secs) == 62730  # R: as.numeric(round(..., 0)) (R/getRunStatus.R:311)


def test_magpie_fields(magpie: RunStatistics) -> None:
    assert magpie.names == ("config", "modelstat", "id")
    assert magpie.model_name == "MAgPIE" and magpie.is_magpie
    assert magpie.modelstat == (2.0, 2.0, 2.0, 7.0, 2.0)
    assert magpie.id == "mag1"
    assert (
        magpie.time_gams_start is None and magpie.time_gams_end is None and magpie.time_prepare_start is None
    )
    assert not magpie.has_name("GAMSEnd")
    assert magpie.gams_wall_seconds() is None


def test_missing_file_is_none(tmp_path: Path) -> None:
    assert read_run_statistics(tmp_path / "runstatistics.rda") is None
    assert read_run_statistics(tmp_path) is None  # a directory is not a file


def test_file_without_a_stats_list_is_none(tmp_path: Path) -> None:
    path = tmp_path / "runstatistics.rda"
    rdata.write_rda(path, {"other": np.array([1.0])})
    assert read_run_statistics(path) is None
    rdata.write_rda(path, {"stats": np.array(["x"], dtype=object)})
    assert read_run_statistics(path) is None


def test_unreadable_file_raises(tmp_path: Path) -> None:
    path = tmp_path / "runstatistics.rda"
    path.write_bytes(b"garbage")
    with pytest.raises(RDataError):
        read_run_statistics(path)


def test_config_without_model_name(tmp_path: Path) -> None:
    """The ``noconfig-badstats`` shape: ``stats$config = list(title = "badstats")`` (BUG-005 case)."""
    stats = run_statistics_from(tmp_path, {"config": {"title": "badstats"}})
    assert stats.has_name("config") and stats.config == {"title": "badstats"}
    assert stats.model_name is None and not stats.is_magpie
    assert stats.id is None and stats.modelstat == ()


def test_config_that_is_not_a_list(tmp_path: Path) -> None:
    stats = run_statistics_from(tmp_path, {"config": "REMIND", "id": 179059272175832.0})
    assert stats.config is None and stats.model_name is None
    assert stats.id == "179059272175832"  # paste0() of a numeric id


def test_na_times_give_no_wall_seconds(tmp_path: Path) -> None:
    stats = run_statistics_from(
        tmp_path,
        {"timeGAMSStart": RTime(1790324400.0), "timeGAMSEnd": RTime(None), "timePrepareStart": RTime(None)},
    )
    assert stats.has_name("GAMSEnd") and stats.gams_wall_seconds() is None
    assert stats.time_prepare_start is not None and stats.time_prepare_start.is_na


def test_times_without_tzone_still_subtract(tmp_path: Path) -> None:
    """Cluster files store POSIXct without ``tzone`` (``01`` inventory); only the instants matter."""
    stats = run_statistics_from(
        tmp_path, {"timeGAMSStart": RTime(1792884600.0), "timeGAMSEnd": RTime(1792895400.0)}
    )  # 2026-10-25 01:30 CEST -> 03:30 CET across the fall-back transition
    assert stats.gams_wall_seconds() == 10800


def test_posixct_vector_is_rejected(tmp_path: Path) -> None:
    with pytest.raises(RDataError, match=r"stats\$timeGAMSEnd is not a single POSIXct"):
        run_statistics_from(tmp_path, {"timeGAMSEnd": (RTime(1.0), RTime(2.0))})
