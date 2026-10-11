"""``rdata`` reads the R data files the package depends on, with the semantics the port relies on.

Converted from ``migration/harness/smoke/read_rdata.py`` and ``rdata_posix.py`` (the ``pyreadr``
comparison is gone: it was rejected, ``migration/02-python-libraries.md`` section 4.2). The files
are written by R (``migration/harness/checks/write_testdata.R``). Pinned here, before phase 1 builds
``rdata_io`` on top of them:

* named list -> ``dict`` (keys compare equal to ``str``), atomic vectors -> ``numpy.ndarray`` even
  for length 1, ``NULL`` -> ``None``, ``NA`` -> a masked element in logical and numeric vectors and
  ``None`` in character vectors, named vector -> ``xarray.DataArray``, data.frame ->
  ``pandas.DataFrame`` with the row names as index;
* POSIXct arrives as epoch seconds plus the ``tzone`` attribute when a constructor is supplied
  (a UserWarning without one), difftime as a float plus ``units``;
* elapsed time is computed on epoch seconds, which is DST-proof.
"""

from __future__ import annotations

import datetime as dt
import warnings
from collections.abc import Callable, Mapping
from pathlib import Path
from typing import Any
from zoneinfo import ZoneInfo

import numpy as np
import pandas as pd
import pytest
import rdata
import xarray as xr
from rdata.conversion import DEFAULT_CLASS_MAP


def posixct(obj: Any, attrs: Mapping[str, Any] | None) -> tuple[float | None, str]:
    """POSIXct -> (epoch seconds or None for NA, tzone attribute or '')."""
    tzone = str((attrs or {}).get("tzone", [""])[0] or "")
    values = np.ma.filled(np.ma.asarray(obj, dtype=float), np.nan).reshape(-1)
    assert values.size == 1
    epoch = float(values[0])
    return (None if np.isnan(epoch) else epoch, tzone)


def difftime(obj: Any, attrs: Mapping[str, Any] | None) -> tuple[float, str]:
    """R difftime -> (value, units)."""
    units = str((attrs or {}).get("units", ["secs"])[0])
    return (float(np.atleast_1d(obj)[0]), units)


Constructor = Callable[[Any, Mapping[str, Any]], Any]
RMAP: dict[str | bytes, Constructor] = {**DEFAULT_CLASS_MAP, "POSIXct": posixct, "difftime": difftime}


def scalar(value: Any) -> Any:
    """Unwrap a length-1 R vector; NA and NULL become None.

    NA is a masked element in logical and numeric vectors and a plain ``None`` inside the object
    array of a character vector; numpy scalars become Python scalars.
    """
    if value is None:
        return None
    array = np.ma.asarray(value).reshape(-1)
    assert array.size == 1
    element = array[0]
    if element is None or np.ma.is_masked(element):
        return None
    return element.item() if isinstance(element, np.generic) else element


@pytest.fixture(scope="module")
def cfg(r_testdata_dir: Path) -> dict[str, Any]:
    loaded: dict[str, Any] = rdata.read_rda(r_testdata_dir / "config.Rdata", constructor_dict=RMAP)["cfg"]
    return loaded


@pytest.fixture(scope="module")
def stats(r_testdata_dir: Path) -> dict[str, Any]:
    with warnings.catch_warnings():
        warnings.simplefilter("error")  # a missing POSIXct/difftime constructor would warn
        loaded: dict[str, Any] = rdata.read_rda(r_testdata_dir / "runstatistics.rda", constructor_dict=RMAP)[
            "stats"
        ]
    return loaded


def test_every_file_reads(r_testdata_dir: Path) -> None:
    for path in sorted(r_testdata_dir.iterdir()):
        if path.suffix == ".yml":
            continue
        if path.suffix in {".rda", ".Rdata"}:
            assert isinstance(rdata.read_rda(path, constructor_dict=RMAP), dict), path
        else:
            rdata.read_rds(path, constructor_dict=RMAP)


def test_config_named_list_is_a_dict_with_str_comparable_keys(cfg: dict[str, Any]) -> None:
    assert "gms" in cfg
    assert all(key == str(key) for key in cfg)
    assert scalar(cfg["title"]) == "SSP2-NPi-AMT"
    assert scalar(cfg["gms"]["optimization"]) == "nash"


def test_config_length_one_vectors_are_arrays_until_unwrapped(cfg: dict[str, Any]) -> None:
    assert isinstance(cfg["gms"]["cm_iteration_max"], np.ndarray)
    assert scalar(cfg["gms"]["cm_iteration_max"]) == 50
    assert isinstance(scalar(cfg["gms"]["cm_iteration_max"]), int)
    assert scalar(cfg["gms"]["cm_nash_autoconverge"]) == 1.0
    assert scalar(cfg["gms"]["some_flag"]) is True


def test_config_null_and_na(cfg: dict[str, Any]) -> None:
    assert cfg["path_magpie"] is None
    assert cfg["cfg_mag"] is None
    assert np.ma.is_masked(np.ma.asarray(cfg["gms"]["some_na"]).reshape(-1)[0])
    assert scalar(cfg["gms"]["some_na"]) is None


def test_config_named_vector_is_an_xarray_with_names(cfg: dict[str, Any]) -> None:
    vec = cfg["gms"]["vec"]
    assert isinstance(vec, xr.DataArray)
    assert dict(zip(vec.coords[vec.dims[0]].values.tolist(), vec.values.tolist(), strict=True)) == {
        "a": 1.0,
        "b": 2.0,
    }
    assert cfg["gms"]["charvec"].tolist() == ["x", "y"]


def test_posixct_carries_epoch_and_tzone(stats: dict[str, Any]) -> None:
    epoch, tzone = stats["timeGAMSStart"]
    assert epoch == 1790324400.0  # 2026-09-25 10:20:00 Europe/Berlin (CEST, UTC+2)
    assert tzone == "Europe/Berlin"
    assert dt.datetime.fromtimestamp(epoch, dt.UTC).astimezone(ZoneInfo(tzone)) == dt.datetime(
        2026, 9, 25, 10, 20, tzinfo=ZoneInfo("Europe/Berlin")
    )


def test_gams_wall_time_from_epoch_seconds(stats: dict[str, Any]) -> None:
    start, _ = stats["timeGAMSStart"]
    end, _ = stats["timeGAMSEnd"]
    assert end - start == 62730.5  # R: difftime(..., units = "secs") -> 62730.5 secs = 17.4 hours
    # R rounds the Runtime column to whole seconds; round() rounds half to even like R's round().
    assert round(end - start) == 62730


def test_difftime_keeps_its_units(stats: dict[str, Any]) -> None:
    assert stats["runtime"] == (3.5, "hours")


def test_stats_nested_config_and_scalars(stats: dict[str, Any]) -> None:
    assert scalar(stats["config"]["model_name"]) == "REMIND"
    assert scalar(stats["id"]) == "2026-09-25_10.11.12_abcdef"
    assert scalar(stats["modelstat"]) == 2.0


def test_posixct_without_constructor_warns(r_testdata_dir: Path) -> None:
    with warnings.catch_warnings(record=True) as caught:
        warnings.simplefilter("always")
        rdata.read_rda(r_testdata_dir / "runstatistics.rda")
    missing = {str(w.message).split('"')[1] for w in caught if "Missing constructor" in str(w.message)}
    assert missing == {"POSIXct", "POSIXt", "difftime"}


def test_magpie_modelstat_is_a_vector_over_years(r_testdata_dir: Path) -> None:
    stats_magpie = rdata.read_rda(r_testdata_dir / "runstatistics_magpie.rda", constructor_dict=RMAP)["stats"]
    assert stats_magpie["modelstat"].tolist() == [2.0, 2.0, 2.0, 7.0, 2.0]
    assert "".join(str(int(v)) for v in stats_magpie["modelstat"]) == "22272"  # R: paste0(..., collapse = "")


def test_runcode_rds_is_one_string(r_testdata_dir: Path) -> None:
    assert scalar(rdata.read_rds(r_testdata_dir / "runcode.rds")) == ".*-AMT_2026-09-25|.*-AMT_2026-09-26"


def test_fixed_time_rds(r_testdata_dir: Path) -> None:
    epoch, tzone = rdata.read_rds(r_testdata_dir / "fixedtime.rds", constructor_dict=RMAP)
    assert (epoch, tzone) == (1790758973.0, "Europe/Berlin")  # 2026-09-30 11:02:53 CEST


def test_project_summations_nested_list_with_na_and_null(r_testdata_dir: Path) -> None:
    summations = rdata.read_rds(r_testdata_dir / "projectSummations.rds")
    assert scalar(summations["ScenarioMIP"]["missingVars"]) == 12
    assert scalar(summations["AR6"]["checkSummations"]) is None  # NA
    assert summations["AR6"]["checkSummationsRegional"] is None  # NULL


def test_data_frame_with_row_names_and_na(r_testdata_dir: Path) -> None:
    frame = rdata.read_rds(r_testdata_dir / "gRS.rds")
    assert isinstance(frame, pd.DataFrame)
    assert frame.index.tolist() == ["SSP2-NPi-AMT_2026-09-25_10.11.12", "SSP2-Calib_2026-09-25_10.11.13"]
    assert frame.columns.tolist() == ["jobInSLURM", "RunType", "Conv", "Runtime"]
    assert frame["Runtime"].tolist()[0] == 12345
    assert frame["Runtime"].isna().tolist() == [False, True]
    assert frame["Conv"].tolist()[0] == "converged"
    assert frame["Conv"].isna().tolist() == [False, True]


def test_atomic_vectors_with_na_and_null(r_testdata_dir: Path) -> None:
    chars = rdata.read_rds(r_testdata_dir / "charNA.rds")
    assert isinstance(chars, np.ndarray) and chars.dtype == object
    assert chars.tolist() == ["a", None, "c"]  # NA_character_ is None, not a mask
    assert scalar(chars[1:2]) is None
    logical = np.ma.asarray(rdata.read_rds(r_testdata_dir / "lglNA.rds"))
    assert bool(logical[0]) is True and np.ma.is_masked(logical[1]) and bool(logical[2]) is False
    assert rdata.read_rds(r_testdata_dir / "null.rds") is None


def test_epoch_arithmetic_is_dst_proof() -> None:
    """Across the 2026-10-25 change 00:30Z -> 02:30Z is 2 h; two datetimes sharing one tzinfo say 1 h.

    The trap of ``migration/02-python-libraries.md`` section 4.2: Python subtracts aware datetimes
    with the *same* tzinfo as wall-clock values, ignoring the utcoffset change. Elapsed time must be
    computed on epoch seconds (or UTC instants), never on local datetimes.
    """
    berlin = ZoneInfo("Europe/Berlin")
    start = dt.datetime(2026, 10, 25, 0, 30, tzinfo=dt.UTC)
    end = dt.datetime(2026, 10, 25, 2, 30, tzinfo=dt.UTC)
    assert end.timestamp() - start.timestamp() == 7200
    local_start, local_end = start.astimezone(berlin), end.astimezone(berlin)
    assert (local_start.hour, local_end.hour) == (2, 3)  # 02:30 CEST -> 03:30 CET, 2 h apart
    assert (local_end - local_start).total_seconds() == 3600  # the trap: same tzinfo, wall-clock difference
    assert local_end.timestamp() - local_start.timestamp() == 7200  # instants: correct
    assert (local_end.astimezone(dt.UTC) - local_start.astimezone(dt.UTC)).total_seconds() == 7200
