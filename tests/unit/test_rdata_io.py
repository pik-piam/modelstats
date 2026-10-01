"""``modelstats.rdata_io``: R data files with the semantics of ``02`` section 4.2, checked against R.

The inputs are the R-written files of ``tests/data/r`` (``migration/harness/checks/write_testdata.R``).
Where a value must equal what R computes, the R result is either pinned from an ``Rscript`` session
(the command is quoted next to the constant) or recomputed live when ``Rscript`` is on the PATH:
the DST table and every ``write_rds`` round trip (``identical()`` in R) run both ways.
"""

from __future__ import annotations

import datetime as dt
import math
import os
import shutil
import subprocess
from collections.abc import Mapping
from pathlib import Path
from typing import Any
from zoneinfo import ZoneInfo

import numpy as np
import pandas as pd
import pytest
import rdata

from modelstats.rdata_io import (
    RDataError,
    RTime,
    element,
    r_as_character,
    r_round,
    read_rda,
    read_rds,
    scalar,
    vector,
    write_rds,
)

BERLIN = ZoneInfo("Europe/Berlin")
RSCRIPT = shutil.which("Rscript")
requires_r = pytest.mark.skipif(RSCRIPT is None, reason="Rscript is not on the PATH")


def run_r(code: str, env: Mapping[str, str] | None = None) -> str:
    """Run R code with ``Rscript -e`` and return its stdout (the test fails on a non-zero status)."""
    assert RSCRIPT is not None
    result = subprocess.run(
        [RSCRIPT, "-e", code], capture_output=True, text=True, check=False, env={**os.environ, **(env or {})}
    )
    assert result.returncode == 0, result.stderr
    return result.stdout


def r_identical(path: Path, expression: str) -> bool:
    """``identical(readRDS(path), <expression>)`` in R."""
    return run_r(f'cat(identical(readRDS("{path}"), {expression}))').strip() == "TRUE"


# ---------------------------------------------------------------------------------------------
# Reading
# ---------------------------------------------------------------------------------------------


@pytest.fixture(scope="module")
def cfg(r_testdata_dir: Path) -> dict[str, Any]:
    objects = read_rda(r_testdata_dir / "config.Rdata")
    assert list(objects) == ["cfg"]
    loaded: dict[str, Any] = objects["cfg"]
    return loaded


@pytest.fixture(scope="module")
def stats(r_testdata_dir: Path) -> dict[str, Any]:
    loaded: dict[str, Any] = read_rda(r_testdata_dir / "runstatistics.rda")["stats"]
    return loaded


def test_named_list_is_a_dict_of_str_keys(cfg: dict[str, Any]) -> None:
    assert all(type(key) is str for key in cfg)
    assert all(type(key) is str for key in cfg["gms"])
    assert scalar(cfg["title"]) == "SSP2-NPi-AMT"
    assert scalar(cfg["gms"]["optimization"]) == "nash"


def test_scalar_unwraps_length_one_vectors_with_python_types(cfg: dict[str, Any]) -> None:
    gms = cfg["gms"]
    assert isinstance(gms["cm_iteration_max"], np.ndarray)  # R has no scalars
    assert scalar(gms["cm_iteration_max"]) == 50 and type(scalar(gms["cm_iteration_max"])) is int
    assert scalar(gms["cm_nash_autoconverge"]) == 1.0 and type(scalar(gms["cm_nash_autoconverge"])) is float
    assert scalar(gms["some_flag"]) is True
    assert scalar(cfg["slurmConfig"]) == "--qos=standby --nodes=1 --tasks-per-node=12"


def test_null_and_na_are_none(cfg: dict[str, Any], r_testdata_dir: Path) -> None:
    assert cfg["path_magpie"] is None  # NULL
    assert scalar(cfg["path_magpie"]) is None
    assert scalar(cfg["gms"]["some_na"]) is None  # NA in a logical vector: a masked element
    chars = read_rds(r_testdata_dir / "charNA.rds")
    assert vector(chars) == ("a", None, "c")  # NA_character_ is a plain None, not a mask
    assert vector(read_rds(r_testdata_dir / "lglNA.rds")) == (True, None, False)
    assert read_rds(r_testdata_dir / "null.rds") is None
    assert vector(None) == ()
    frame = read_rds(r_testdata_dir / "gRS.rds")
    assert vector(frame["Runtime"].to_numpy()) == (12345.0, None)  # NA_real_: a NaN with R's NA payload
    assert vector(np.array([1.5, math.nan])) == (1.5, None)  # a plain NaN is is.na() in R as well


def test_named_and_character_vectors(cfg: dict[str, Any]) -> None:
    assert vector(cfg["gms"]["vec"]) == (1.0, 2.0)  # named numeric vector (an xarray underneath)
    assert vector(cfg["gms"]["charvec"]) == ("x", "y")
    with pytest.raises(RDataError, match="got 2 elements"):
        scalar(cfg["gms"]["charvec"])


def test_element_rejects_non_atomic_values() -> None:
    assert element(np.float64(2.0)) == 2.0
    assert element(np.ma.masked) is None
    with pytest.raises(RDataError, match="not an element"):
        element(object())
    with pytest.raises(RDataError, match="not an R atomic vector"):
        vector({"a": 1})


def test_vector_flattens_arrays_in_column_major_order() -> None:
    array = np.array([[1, 2, 3], [4, 5, 6]])
    assert vector(array) == (1, 4, 2, 5, 3, 6)  # R's as.vector() on a matrix


def test_rds_scalars_and_nested_lists(r_testdata_dir: Path) -> None:
    assert scalar(read_rds(r_testdata_dir / "runcode.rds")) == ".*-AMT_2026-09-25|.*-AMT_2026-09-26"
    summations = read_rds(r_testdata_dir / "projectSummations.rds")
    assert scalar(summations["ScenarioMIP"]["missingVars"]) == 12
    assert scalar(summations["AR6"]["checkSummations"]) is None  # NA
    assert summations["AR6"]["checkSummationsRegional"] is None  # NULL


def test_rds_data_frame_keeps_row_names_and_na(r_testdata_dir: Path) -> None:
    frame = read_rds(r_testdata_dir / "gRS.rds")
    assert isinstance(frame, pd.DataFrame)
    assert frame.index.tolist() == ["SSP2-NPi-AMT_2026-09-25_10.11.12", "SSP2-Calib_2026-09-25_10.11.13"]
    assert frame["Runtime"].tolist()[0] == 12345
    assert frame["Runtime"].isna().tolist() == [False, True]
    assert frame["Conv"].isna().tolist() == [False, True]


def test_read_errors(tmp_path: Path, r_testdata_dir: Path) -> None:
    with pytest.raises(FileNotFoundError):
        read_rda(tmp_path / "absent.rda")
    with pytest.raises(FileNotFoundError):
        read_rds(tmp_path / "absent.rds")
    garbage = tmp_path / "garbage.rda"
    garbage.write_bytes(b"not an R file at all\n")
    with pytest.raises(RDataError, match="cannot read R data file"):
        read_rda(garbage)
    with pytest.raises(RDataError, match="not an R data file holding named objects"):
        read_rda(r_testdata_dir / "runcode.rds")


def test_reading_never_warns(r_testdata_dir: Path) -> None:
    """R's ``load()`` is silent; ``rdata`` would warn about POSIXct, difftime and unknown files."""
    import warnings

    with warnings.catch_warnings():
        warnings.simplefilter("error")
        read_rda(r_testdata_dir / "runstatistics.rda")
        read_rda(r_testdata_dir / "runstatistics_magpie.rda")
        read_rds(r_testdata_dir / "fixedtime.rds")


# ---------------------------------------------------------------------------------------------
# POSIXct, difftime and R's arithmetic
# ---------------------------------------------------------------------------------------------


def test_posixct_is_an_rtime_with_epoch_and_tzone(stats: dict[str, Any], r_testdata_dir: Path) -> None:
    start = stats["timeGAMSStart"]
    assert start == RTime(1790324400.0, "Europe/Berlin")  # 2026-09-25 10:20:00 CEST
    assert start.local() == dt.datetime(2026, 9, 25, 10, 20, tzinfo=BERLIN)
    assert start.as_datetime() == dt.datetime(2026, 9, 25, 8, 20, tzinfo=dt.UTC)
    assert not start.is_na
    fixed = read_rds(r_testdata_dir / "fixedtime.rds")
    assert fixed == RTime(1790758973.0, "Europe/Berlin")  # 2026-09-30 11:02:53 CEST


def test_gams_wall_time_and_whole_second_rounding(stats: dict[str, Any]) -> None:
    secs = stats["timeGAMSEnd"].elapsed_since(stats["timeGAMSStart"])
    assert secs == 62730.5  # R: difftime(timeGAMSEnd, timeGAMSStart, units = "secs")
    assert r_round(secs) == 62730  # R: as.numeric(round(difftime, 0)) rounds half to even


def test_difftime_arrives_in_seconds(stats: dict[str, Any]) -> None:
    assert stats["runtime"] == 3.5 * 3600  # as.difftime(3.5, units = "hours")


def test_r_round_is_half_to_even() -> None:
    """R 4.6.1: ``round(c(0.5, 1.5, 2.5, 62730.5, -0.5), 0)`` is ``0 2 2 62730 0``."""
    assert [r_round(x) for x in (0.5, 1.5, 2.5, 62730.5, -0.5, 7.4, 7.6)] == [0, 2, 2, 62730, 0, 7, 8]


# R 4.6.1 under TZ=Europe/Berlin (the cluster's zone):
#   as.POSIXct("2026-03-29 01:30:00", tz = "Europe/Berlin")   -> 1774744200 (CET, UTC+1)
#   as.POSIXct("2026-03-29 03:30:00", tz = "Europe/Berlin")   -> 1774747800 (CEST, UTC+2)  difftime: 3600 secs
#   as.POSIXct("2026-10-25 01:30:00", tz = "Europe/Berlin")   -> 1792884600 (CEST)
#   as.POSIXct("2026-10-25 03:30:00", tz = "Europe/Berlin")   -> 1792895400 (CET)      difftime: 10800 secs
#   as.POSIXct("2026-10-25 00:30:00", tz = "UTC")             -> 1792888200 (02:30 CEST)
#   as.POSIXct("2026-10-25 02:30:00", tz = "UTC")             -> 1792895400 (03:30 CET)    difftime: 7200 secs
# The same values with tz = "" (no tzone attribute, as the cluster's runstatistics.rda store them)
# give identical epochs and differences; difftime(<NA>, x) is NA.
DST_TABLE = (
    (
        "spring forward, tzone",
        RTime(1774744200.0, "Europe/Berlin"),
        RTime(1774747800.0, "Europe/Berlin"),
        3600,
    ),
    ("spring forward, no tzone", RTime(1774744200.0), RTime(1774747800.0), 3600),
    ("fall back, tzone", RTime(1792884600.0, "Europe/Berlin"), RTime(1792895400.0, "Europe/Berlin"), 10800),
    ("fall back, no tzone", RTime(1792884600.0), RTime(1792895400.0), 10800),
    ("fall back, UTC-defined", RTime(1792888200.0, "UTC"), RTime(1792895400.0, "UTC"), 7200),
    ("NA end", RTime(1792884600.0, "Europe/Berlin"), RTime(None), None),
    ("NA start, no tzone", RTime(None), RTime(1792895400.0), None),
)


@pytest.mark.parametrize(("label", "start", "end", "expected"), DST_TABLE, ids=[row[0] for row in DST_TABLE])
def test_elapsed_seconds_across_the_2026_dst_transitions(
    label: str, start: RTime, end: RTime, expected: float | None
) -> None:
    assert end.elapsed_since(start) == expected
    if expected is None:
        assert start.is_na or end.is_na
        assert (start.local() is None) == start.is_na
    else:
        assert r_round(expected) == expected
        # The trap of 02 section 4.2: two aware datetimes sharing one ZoneInfo subtract as wall-clock
        # values, and every pair here straddles a transition, so that difference is off by one hour.
        berlin_start = start.as_datetime().astimezone(BERLIN)  # type: ignore[union-attr]
        berlin_end = end.as_datetime().astimezone(BERLIN)  # type: ignore[union-attr]
        assert berlin_start.utcoffset() != berlin_end.utcoffset()
        assert abs((berlin_end - berlin_start).total_seconds() - expected) == 3600
        assert (berlin_end.astimezone(dt.UTC) - berlin_start.astimezone(dt.UTC)).total_seconds() == expected


def test_local_display_follows_the_tzone_or_the_cluster_default() -> None:
    assert RTime(1774744200.0, "Europe/Berlin").local() == dt.datetime(2026, 3, 29, 1, 30, tzinfo=BERLIN)
    assert RTime(1774747800.0).local() == dt.datetime(2026, 3, 29, 3, 30, tzinfo=BERLIN)
    assert RTime(1792888200.0, "UTC").local() == dt.datetime(2026, 10, 25, 0, 30, tzinfo=dt.UTC)
    assert RTime(1792888200.0, "UTC").local().strftime("%H:%M %Z") == "00:30 UTC"  # type: ignore[union-attr]
    assert RTime(1792888200.0).local().strftime("%H:%M %Z") == "02:30 CEST"  # type: ignore[union-attr]
    assert RTime(1792895400.0).local().strftime("%H:%M %Z") == "03:30 CET"  # type: ignore[union-attr]


def test_from_datetime_round_trips_an_instant() -> None:
    moment = dt.datetime(2026, 10, 25, 2, 30, tzinfo=dt.UTC)
    assert RTime.from_datetime(moment).epoch == 1792895400.0
    assert (
        RTime.from_datetime(dt.datetime(2026, 10, 25, 3, 30)).epoch == 1792895400.0
    )  # naive: Berlin wall time
    assert RTime.from_datetime(moment, "UTC").tzone == "UTC"


@requires_r
def test_dst_table_recomputed_by_r(tmp_path: Path) -> None:
    """R writes the DST pairs of ``DST_TABLE`` and prints its ``difftime``; the readers must agree."""
    rda = tmp_path / "dst.rda"
    code = f"""
    mk <- function(s, tz) as.POSIXct(s, tz = tz)
    berlin <- "Europe/Berlin"
    dst <- list(
      spring_tz   = list(start = mk("2026-03-29 01:30:00", berlin), end = mk("2026-03-29 03:30:00", berlin)),
      spring_notz = list(start = mk("2026-03-29 01:30:00", ""), end = mk("2026-03-29 03:30:00", "")),
      fall_tz     = list(start = mk("2026-10-25 01:30:00", berlin), end = mk("2026-10-25 03:30:00", berlin)),
      fall_notz   = list(start = mk("2026-10-25 01:30:00", ""), end = mk("2026-10-25 03:30:00", "")),
      fall_utc    = list(start = mk("2026-10-25 00:30:00", "UTC"), end = mk("2026-10-25 02:30:00", "UTC")),
      na_end      = list(start = mk("2026-10-25 01:30:00", berlin), end = as.POSIXct(NA)),
      na_start    = list(start = as.POSIXct(NA), end = mk("2026-10-25 03:30:00", ""))
    )
    save(dst, file = "{rda}")
    for (n in names(dst)) cat(n, as.numeric(dst[[n]]$start), as.numeric(dst[[n]]$end),
                              as.numeric(difftime(dst[[n]]$end, dst[[n]]$start, units = "secs")), "\\n")
    """
    reported = {}
    for line in run_r(code, env={"TZ": "Europe/Berlin"}).splitlines():
        name, start_epoch, end_epoch, secs = line.split()
        reported[name] = tuple(
            None if value == "NA" else float(value) for value in (start_epoch, end_epoch, secs)
        )
    pairs = read_rda(rda)["dst"]
    assert (
        set(pairs)
        == set(reported)
        == {
            "spring_tz",
            "spring_notz",
            "fall_tz",
            "fall_notz",
            "fall_utc",
            "na_end",
            "na_start",
        }
    )
    for name, pair in pairs.items():
        start, end = pair["start"], pair["end"]
        assert isinstance(start, RTime) and isinstance(end, RTime)
        assert (start.epoch, end.epoch, end.elapsed_since(start)) == reported[name], name
    assert pairs["spring_tz"]["start"].tzone == "Europe/Berlin"
    assert pairs["spring_notz"]["start"].tzone == ""
    assert pairs["fall_utc"]["end"].tzone == "UTC"
    assert (
        reported["spring_tz"][2] == 3600
        and reported["fall_utc"][2] == 7200
        and reported["fall_tz"][2] == 10800
    )


# ---------------------------------------------------------------------------------------------
# as.character()
# ---------------------------------------------------------------------------------------------

# R 4.6.1: as.character(v) for each double v (and paste0(v), which agrees).
AS_CHARACTER = (
    (2.0, "2"),
    (13.0, "13"),
    (7.0, "7"),
    (100.0, "100"),
    (1000.0, "1000"),
    (10000.0, "10000"),
    (100000.0, "1e+05"),
    (150000.0, "150000"),
    (123456.0, "123456"),
    (1e6, "1e+06"),
    (12345678.0, "12345678"),
    (1e15, "1e+15"),
    (123456789012345678.0, "123456789012345680"),
    (0.1 + 0.2, "0.3"),
    (0.5, "0.5"),
    (-2.5, "-2.5"),
    (1234.5678, "1234.5678"),
    (1 / 3, "0.333333333333333"),
    (0.0001, "1e-04"),
    (1e-5, "1e-05"),
    (0.00012345, "0.00012345"),
    (0.0, "0"),
    (math.inf, "Inf"),
    (-math.inf, "-Inf"),
    (math.nan, "NaN"),
)


@pytest.mark.parametrize(("value", "expected"), AS_CHARACTER, ids=[row[1] for row in AS_CHARACTER])
def test_as_character_of_doubles_matches_r(value: float, expected: str) -> None:
    assert r_as_character(value) == expected


def test_as_character_of_other_scalars() -> None:
    assert r_as_character(None) is None  # NA stays NA
    assert r_as_character(True) == "TRUE" and r_as_character(False) == "FALSE"
    assert r_as_character(3) == "3"
    assert r_as_character("x") == "x"


def test_magpie_modelstat_vector_concatenates_like_paste0(r_testdata_dir: Path) -> None:
    stats = read_rda(r_testdata_dir / "runstatistics_magpie.rda")["stats"]
    values = vector(stats["modelstat"])
    assert values == (2.0, 2.0, 2.0, 7.0, 2.0)
    assert "".join(r_as_character(v) or "NA" for v in values) == "22272"  # R/getRunStatus.R:92


# ---------------------------------------------------------------------------------------------
# Writing
# ---------------------------------------------------------------------------------------------


def python_frame() -> pd.DataFrame:
    """A frame with every nullable column kind the writer supports and character row names."""
    return pd.DataFrame(
        {
            "x": pd.array(["a", None], dtype="str"),
            "n": pd.array([1, None], dtype="Int64"),
            "v": pd.array([1.5, None], dtype="Float64"),
            "f": pd.array([True, None], dtype="boolean"),
        },
        index=["r1", "r2"],
    )


WRITE_CASES: tuple[tuple[str, object, str], ...] = (
    ("runcode", ".*-AMT_2026-09-25|.*-AMT_2026-09-26", '".*-AMT_2026-09-25|.*-AMT_2026-09-26"'),
    ("lastcommit", "13f60fdd4fff366bebcc7dfe522897dedeb43cc7", '"13f60fdd4fff366bebcc7dfe522897dedeb43cc7"'),
    ("logical", True, "TRUE"),
    ("integer", 3, "3L"),
    ("double", 2.5, "2.5"),
    ("null", None, "NULL"),
    ("character vector", ["a", None, "c"], 'c("a", NA, "c")'),
    ("integer vector", [1, None, 3], "c(1L, NA, 3L)"),
    ("double vector", (1.5, None), "c(1.5, NA)"),
    ("logical vector", [True, None, False], "c(TRUE, NA, FALSE)"),
    ("named list", {"a": 1, "b": "x", "c": None, "d": [2.5]}, 'list(a = 1L, b = "x", c = NULL, d = 2.5)'),
    (
        "data frame",
        python_frame(),
        'data.frame(x = c("a", NA), n = c(1L, NA), v = c(1.5, NA), f = c(TRUE, NA),'
        ' row.names = c("r1", "r2"))',
    ),
    (
        "data frame, default index",
        pd.DataFrame({"x": ["a", "b"], "n": [1, 2]}),
        'data.frame(x = c("a", "b"), n = 1:2)',
    ),
    ("empty data frame", pd.DataFrame({"x": pd.array([], dtype="str")}), "data.frame(x = character(0))"),
)


@pytest.mark.parametrize(("label", "value", "expression"), WRITE_CASES, ids=[case[0] for case in WRITE_CASES])
def test_write_rds_reads_back_in_python(label: str, value: object, expression: str, tmp_path: Path) -> None:
    path = tmp_path / "value.rds"
    write_rds(path, value)
    back = read_rds(path)
    if isinstance(value, pd.DataFrame):
        assert isinstance(back, pd.DataFrame)
        if isinstance(value.index, pd.RangeIndex):
            assert back.index.tolist() == list(range(1, len(value) + 1))  # R's automatic row names
        else:
            assert back.index.tolist() == [str(name) for name in value.index]
        assert back.columns.tolist() == value.columns.tolist()
        assert back.isna().to_numpy().tolist() == value.isna().to_numpy().tolist()
    elif isinstance(value, Mapping):
        assert list(back) == list(value)
        assert scalar(back["b"]) == "x" and back["c"] is None
    elif isinstance(value, list | tuple):
        assert vector(back) == tuple(value)
    else:
        assert scalar(back) == value


@requires_r
@pytest.mark.parametrize(("label", "value", "expression"), WRITE_CASES, ids=[case[0] for case in WRITE_CASES])
def test_write_rds_is_identical_in_r(label: str, value: object, expression: str, tmp_path: Path) -> None:
    """``identical(readRDS(<ours>), <the same object built in R>)`` is TRUE."""
    path = tmp_path / "value.rds"
    write_rds(path, value)
    assert r_identical(path, expression), f"{label}: R does not consider {expression} identical to our file"


@requires_r
def test_write_rds_round_trips_r_written_files(tmp_path: Path, r_testdata_dir: Path) -> None:
    """R file -> ``read_rds`` -> ``write_rds`` -> R ``readRDS`` is ``identical()`` to the original."""
    for name in ("runcode.rds", "gRS.rds", "projectSummations.rds", "charNA.rds", "lglNA.rds", "null.rds"):
        copy = tmp_path / name
        write_rds(copy, read_rds(r_testdata_dir / name))
        assert r_identical(copy, f'readRDS("{r_testdata_dir / name}")'), name


@requires_r
def test_float_na_is_na_not_nan_in_r(tmp_path: Path) -> None:
    path = tmp_path / "na.rds"
    write_rds(path, pd.DataFrame({"v": pd.array([1.5, None], dtype="Float64")}))
    out = run_r(f'x <- readRDS("{path}")$v; cat(is.na(x), is.nan(x))')
    assert out.split() == ["FALSE", "TRUE", "FALSE", "FALSE"]


def test_write_rds_rejects_unsupported_values(tmp_path: Path) -> None:
    path = tmp_path / "bad.rds"
    with pytest.raises(RDataError, match="cannot write a set"):
        write_rds(path, {1, 2})
    with pytest.raises(RDataError, match="all str, bool, int or float"):
        write_rds(path, ["a", 1])
    with pytest.raises(RDataError, match="inside a character vector"):
        write_rds(path, pd.DataFrame({"x": pd.array([1, "a"], dtype=object)}))
    with pytest.raises(RDataError, match="cannot write a data frame column"):
        write_rds(path, pd.DataFrame({"t": pd.to_datetime(["2026-01-01"])}))
    assert not path.exists()


def test_rda_written_by_rdata_reads_with_our_class_map(tmp_path: Path) -> None:
    """``rdata.write_rda`` output (other tests build odd files with it) round-trips through ``read_rda``."""
    path = tmp_path / "other.rda"
    rdata.write_rda(path, {"other": np.array([1.0, 2.0])})
    assert vector(read_rda(path)["other"]) == (1.0, 2.0)
