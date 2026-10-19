from __future__ import annotations

from datetime import UTC, date, datetime
from pathlib import Path
from zoneinfo import ZoneInfo

import pandas as pd
import pytest

from modelstats._rdata import RDataReadError, read_rda, read_rds, write_rds_string

BERLIN = ZoneInfo("Europe/Berlin")


def test_config_rdata_is_a_nested_dict(data_dir: Path) -> None:
    cfg = read_rda(data_dir / "config_remind.Rdata")["cfg"]
    assert cfg["title"] == "SSP2-NPi-AMT"
    assert cfg["model_name"] == "REMIND"
    assert cfg["path_magpie"] is None
    assert cfg["gms"]["optimization"] == "nash"
    assert cfg["gms"]["cm_nash_mode"] == "parallel"
    assert cfg["gms"]["cm_iteration_max"] == 50.0
    assert cfg["gms"]["cm_MAgPIE_Nash"] == 0.0
    assert cfg["gms"]["regions"] == ["EUR", "USA"]
    assert cfg["gms"]["flag"] is True
    assert cfg["gms"]["unset"] is None
    assert cfg["gms"]["unsetstr"] is None


def test_runstatistics_times_are_aware_datetimes(data_dir: Path) -> None:
    stats = read_rda(data_dir / "runstatistics_remind.rda")["stats"]
    assert stats["id"] == "abc123"
    assert stats["config"] == {"model_name": "REMIND", "title": "SSP2-NPi-AMT"}
    assert stats["timeGAMSStart"] == datetime(2026, 3, 28, 10, 5, tzinfo=BERLIN)
    assert stats["timeGAMSEnd"] == datetime(2026, 3, 29, 12, 5, 30, tzinfo=BERLIN)
    assert stats["timeGAMSEnd"].tzinfo is UTC
    # the wall clock advanced 26 h across the DST switch of 2026-03-29, but only 25 h elapsed
    assert (stats["timeGAMSEnd"] - stats["timeGAMSStart"]).total_seconds() == 25 * 3600 + 30
    assert stats["runtime"] == 26.5 * 3600
    assert stats["modelstat"] == 2.0
    assert stats["pkg"]["Package"] == "base"


def test_magpie_modelstat_array_is_flattened(data_dir: Path) -> None:
    stats = read_rda(data_dir / "runstatistics_magpie.rda")["stats"]
    assert stats["config"]["model_name"] == "MAgPIE"
    assert stats["modelstat"] == [2.0, 2.0, 7.0]


def test_rds_scalars_lists_and_frames(data_dir: Path) -> None:
    assert read_rds(data_dir / "runcode.rds") == ".*-AMT_2026-10-18|.*-AMT_2026-10-19"
    assert read_rds(data_dir / "lastcommit.rds") == "abc1234"
    summations = read_rds(data_dir / "projectSummations.rds")
    assert summations == {
        "ScenarioMIP": {"missingVars": 3, "checkSummations": 0, "checkSummationsRegional": None}
    }
    runs = read_rds(data_dir / "runsToStart.rds")
    assert isinstance(runs, pd.DataFrame)
    assert list(runs.index) == ["SSP2-NPi-AMT", "SSP2-PkBudg1000-AMT"]
    grs = read_rds(data_dir / "gRS.rds")
    assert list(grs.columns) == ["Conv", "Mif"]
    assert list(grs["Conv"]) == ["converged", "not_converged"]


def test_value_types(data_dir: Path) -> None:
    types = read_rda(data_dir / "types.rda")["types"]
    assert types["nullvalue"] is None
    assert types["string"] == "x"
    assert types["strings"] == ["x", "y"]
    assert types["number"] == 2.0
    assert types["integer"] == 3
    assert types["flag"] is True
    assert types["na_number"] is None
    assert types["na_integer"] is None
    assert types["na_logical"] is None
    assert types["na_string"] is None
    assert types["named"] == {"a": 1.0, "b": None}
    assert types["named_one"] == 1.0  # a length-1 vector is a scalar, named or not
    assert types["named_chr"] == ["x", "y"]  # names of character vectors are lost
    assert types["matrix"] == [1, 2, 3, 4, 5, 6]  # R's column-major order
    assert types["dupnames"] == [1, 2, 3, 4]  # repeated dimnames names: values only (BUG-044)
    assert types["fac"] == ["b", "a", "b"]
    assert types["day"] == date(2026, 10, 18)
    assert types["nested"] == [1.0, "two"]
    assert types["secs"] == 5400.0
    assert types["stamp_notz"] == datetime(2026, 3, 29, 3, 30, tzinfo=BERLIN)
    assert types["stamp_utc"] == datetime(2026, 3, 29, 3, 30, tzinfo=ZoneInfo("UTC"))
    assert types["empty"] == []


def test_data_frame_cells_are_python_values(data_dir: Path) -> None:
    frame = read_rds(data_dir / "gRS_na.rds")
    assert list(frame.index) == ["run1", "run2"]
    assert frame.loc["run1", "Conv"] == "converged"
    assert frame.loc["run2", "Conv"] is None
    assert frame.loc["run2", "Runtime"] is None
    assert frame.loc["run1", "Runtime"] == 90030
    assert frame.loc["run1", "ok"] is True and frame.loc["run2", "ok"] is None
    assert frame.loc["run1", "when"] == datetime(2026, 3, 28, 10, 5, tzinfo=BERLIN)
    tibble = read_rds(data_dir / "tibble.rds")
    assert list(tibble.columns) == ["a", "b"] and list(tibble["b"]) == ["x", "y"]


def test_corrupt_files_raise_rdata_read_error(tmp_path: Path, data_dir: Path) -> None:
    path = tmp_path / "x.rda"
    path.write_bytes(b"garbage" * 10)
    with pytest.raises(RDataReadError):
        read_rda(path)
    path.write_bytes(b"")
    with pytest.raises(RDataReadError):
        read_rds(path)
    with pytest.raises(RDataReadError):
        read_rds(tmp_path / "missing.rds")
    data = (data_dir / "runstatistics_xz.rda").read_bytes()  # xz-compressed like the real files
    for cut in (len(data) // 3, len(data) - 8):
        path.write_bytes(data[:cut])
        with pytest.raises(RDataReadError):
            read_rda(path)
    flipped = bytearray(data)
    flipped[len(data) // 2] ^= 0xFF
    path.write_bytes(bytes(flipped))
    with pytest.raises(RDataReadError):
        read_rda(path)
    assert read_rda(data_dir / "runstatistics_xz.rda")["stats"]["id"] == "xz"
    import lzma

    payload = bytearray(lzma.decompress(data))  # corrupt the decompressed payload, not the stream
    payload[len(payload) // 2] ^= 0xFF
    path.write_bytes(lzma.compress(bytes(payload)))
    with pytest.raises(RDataReadError):
        read_rda(path)


def test_write_rds_string_round_trip(tmp_path: Path) -> None:
    path = tmp_path / "runcode.rds"
    write_rds_string(path, ".*-AMT_2026-10-18")
    assert read_rds(path) == ".*-AMT_2026-10-18"


def test_read_rds_string_and_frame(data_dir: Path) -> None:
    from modelstats._rdata import read_rds_frame, read_rds_string

    assert read_rds_string(data_dir / "runcode.rds") == ".*-AMT_2026-10-18|.*-AMT_2026-10-19"
    assert list(read_rds_frame(data_dir / "runsToStart.rds").index) == ["SSP2-NPi-AMT", "SSP2-PkBudg1000-AMT"]
    with pytest.raises(RDataReadError, match="expected one string, found DataFrame"):
        read_rds_string(data_dir / "runsToStart.rds")
    with pytest.raises(RDataReadError, match="expected a data frame, found '.*-AMT_2026-10-18"):
        read_rds_frame(data_dir / "runcode.rds")
