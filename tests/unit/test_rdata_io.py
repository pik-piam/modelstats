"""rdata_io: R data files, the POSIXct adapter and the atomic RDS writer.

The inputs under ``tests/unit/data/`` were written by R (``make_data.R``), which
also recorded what it computed for them (``expected.json``), so these tests run
without R. The round trips through ``readRDS``/``identical()`` need ``Rscript``
and are skipped where it is absent; the Python-level round trips always run.
"""

from __future__ import annotations

import datetime as dt
import gzip
import json
import os
import shutil
import subprocess
from pathlib import Path
from typing import Any

import numpy as np
import pandas as pd
import pytest
import rdata

from modelstats.errors import RParityError
from modelstats.rdata_io import (
    DEFAULT_TZ,
    RTime,
    pythonize,
    rds_bytes,
    read_rda,
    read_rds,
    scalar,
    vector,
    write_rds,
)

DATA = Path(__file__).parent / "data"
EXPECTED: dict[str, Any] = json.loads((DATA / "expected.json").read_text(encoding="utf-8"))
HAS_R = shutil.which("Rscript") is not None
needs_r = pytest.mark.skipif(not HAS_R, reason="Rscript is not on PATH")
ROUND_TRIP_SHAPES = ["rt_runcode.rds", "rt_grs.rds", "rt_runstostart.rds", "rt_lastcommit.rds"]


def r_eval(code: str) -> str:
    result = subprocess.run(["Rscript", "-e", code], capture_output=True, text=True, check=True)
    return result.stdout.strip()


def r_identical(a: Path, b: Path) -> bool:
    return r_eval(f'cat(identical(readRDS("{a}"), readRDS("{b}")))') == "TRUE"


class BytesEffects:
    """The read-only Effects subset the readers use, answering from canned bytes."""

    def __init__(self, files: dict[str, bytes]) -> None:
        self.files = files
        self.calls: list[tuple[str, str]] = []

    def exists(self, path: str) -> bool:
        self.calls.append(("exists", path))
        return path in self.files

    def read_bytes(self, path: str) -> bytes:
        self.calls.append(("read_bytes", path))
        try:
            return self.files[path]
        except KeyError:
            raise FileNotFoundError(path) from None


# ---------------------------------------------------------------------------
# Reading the shapes
# ---------------------------------------------------------------------------
def test_rds_string_is_a_length_one_character_vector() -> None:
    value = read_rds(DATA / "rds_string.rds")
    assert isinstance(value, np.ndarray)
    assert scalar(value) == "hello world"
    assert pythonize(value) == "hello world"


def test_rds_character_vector_with_na() -> None:
    assert vector(read_rds(DATA / "rds_chr_na.rds")) == ["a", None, "c"]


def test_rds_logical_vector_with_na() -> None:
    assert vector(read_rds(DATA / "rds_lgl_na.rds")) == [True, None, False]


def test_rds_null() -> None:
    assert read_rds(DATA / "rds_null.rds") is None
    assert vector(None) == []
    assert scalar(None) is None


def test_rds_double_na_and_nan_both_read_as_none() -> None:
    # R keeps NA_real_ (a NaN payload) apart from NaN; is.na() is TRUE for both and so is None here.
    assert vector(read_rds(DATA / "rds_num_na_nan.rds")) == [1.0, None, None]


def test_rds_nested_list() -> None:
    value = pythonize(read_rds(DATA / "rds_nested.rds"))
    assert value == {
        "ScenarioMIP": {"missingVars": 2, "checkSummations": 32, "checkSummationsRegional": 0},
        "other": {"a": None, "b": None, "c": [1.0, 2.0]},
    }
    assert isinstance(value["ScenarioMIP"]["missingVars"], int)


def test_rds_systime_is_an_rtime_without_tzone() -> None:
    value = read_rds(DATA / "rds_systime.rds")
    assert isinstance(value, RTime)
    assert value.tzone == ""
    assert value.epoch is not None
    assert abs(value.epoch - float(EXPECTED["systime_epoch"])) < 1e-6


def test_rds_dataframe_keeps_r_types_and_na() -> None:
    frame = read_rds(DATA / "rds_df.rds")
    assert isinstance(frame, pd.DataFrame)
    assert list(frame.index) == ["r1", "r2", "r3"]
    assert list(frame.columns) == ["chr", "num", "int", "lgl"]
    assert str(frame["chr"].dtype) == "string"
    assert frame["num"].dtype == np.float64
    assert str(frame["int"].dtype) == "Int32"
    assert str(frame["lgl"].dtype) == "boolean"
    assert frame["chr"].tolist()[0] == "a" and pd.isna(frame["chr"].iloc[1])
    assert np.isnan(frame["num"].iloc[1]) and frame["num"].iloc[2] == 3.0
    assert pd.isna(frame["int"].iloc[1]) and frame["int"].iloc[2] == 3
    assert pd.isna(frame["lgl"].iloc[1]) and frame["lgl"].iloc[2] is np.False_


def test_rda_returns_every_object_by_name() -> None:
    objects = read_rda(DATA / "config.Rdata")
    assert list(objects) == ["cfg"]
    assert scalar(objects["cfg"]["title"]) == "testOneRegi"
    assert read_rda(DATA / "nocfg.Rdata") == {"other": pytest.approx([1.0])}


def test_posixct_with_and_without_tzone_and_na() -> None:
    stats = read_rda(DATA / "runstatistics.rda")["stats"]
    expect = EXPECTED["runstatistics"]
    assert stats["timePrepareStart"] == RTime(float(expect["timePrepareStart"]), "Europe/Berlin")
    assert stats["timeGAMSStart"] == RTime(float(expect["timeGAMSStart"]), "")
    assert stats["timeGAMSEnd"].epoch == pytest.approx(float(expect["timeGAMSEnd"]), abs=1e-6)
    assert stats["timeOutputStart"] == RTime(None, "")
    assert stats["timeOutputStart"].is_na


def test_difftime_applies_its_units() -> None:
    stats = read_rda(DATA / "runstatistics.rda")["stats"]
    assert stats["runtime"] == pytest.approx(EXPECTED["runstatistics"]["runtime_secs"])  # 2.306602 hours in seconds


def test_magpie_object_reads_as_vector_in_storage_order() -> None:
    path = DATA / "runstatistics_magpie.rda"
    if not path.exists():
        pytest.skip("make_data.R ran without magclass")
    stats = read_rda(path)["stats"]
    assert vector(stats["modelstat"]) == [2.0, 2.0, None, 13.0]
    assert EXPECTED["modelstat_magpie"] == "22NA13"


# ---------------------------------------------------------------------------
# scalar / vector / pythonize
# ---------------------------------------------------------------------------
def test_scalar_unwraps_length_one_vectors() -> None:
    assert scalar(np.array([5], dtype=np.int32)) == 5
    assert isinstance(scalar(np.array([5], dtype=np.int32)), int)
    assert scalar(np.array([2.5])) == 2.5
    assert scalar(np.array(["x"])) == "x"
    assert scalar(np.array([True])) is True
    assert scalar(np.ma.array([1], mask=[True])) is None
    assert scalar(np.array([None], dtype=object)) is None
    assert scalar(np.array([np.nan])) is None
    assert scalar("plain") == "plain"
    assert scalar(7) == 7
    assert scalar(["one"]) == "one"
    assert scalar({"a": 1}) == 1
    time = RTime(1.0, "")
    assert scalar(time) is time
    with pytest.raises(ValueError, match="length-1"):
        scalar(np.array([1, 2]))
    with pytest.raises(ValueError, match="length-1"):
        scalar(np.array([]))


def test_vector_keeps_masks_and_order() -> None:
    assert vector(np.ma.array([2.0, 2.0, 0.0, 13.0], mask=[False, False, True, False])) == [2.0, 2.0, None, 13.0]
    assert vector(np.array(["a", None], dtype=object)) == ["a", None]
    assert vector("one") == ["one"]
    assert vector(3) == [3]
    assert vector([1, None, "x"]) == [1, None, "x"]
    assert vector(np.reshape(np.array([1, 2, 3, 4]), (2, 2), order="F")) == [1, 2, 3, 4]  # R storage order


def test_pythonize_nested_structure() -> None:
    cfg = pythonize(read_rda(DATA / "config.Rdata")["cfg"])
    assert cfg["title"] == "testOneRegi"
    assert cfg["gms"]["cm_nash_mode"] == 1 and isinstance(cfg["gms"]["cm_nash_mode"], int)
    assert cfg["gms"]["cm_MAgPIE_Nash"] == 0.0 and isinstance(cfg["gms"]["cm_MAgPIE_Nash"], float)
    assert cfg["gms"]["a_named"] == {"a": 1.0, "b": 2.0}
    assert cfg["gms"]["a_chars"] == ["x", "y"]
    assert cfg["gms"]["a_chr_na"] == ["a", None]
    assert cfg["gms"]["a_empty"] == []
    assert cfg["gms"]["a_null"] is None and cfg["gms"]["a_na"] is None and cfg["gms"]["a_na_real"] is None
    assert cfg["gms"]["a_true"] is True
    assert cfg["gms"]["a_utf8"] == "Ärger"
    assert cfg["cfg_mag"] == {"results_folder": "output/:title::date:"}


# ---------------------------------------------------------------------------
# RTime
# ---------------------------------------------------------------------------
def dst_cases() -> list[dict[str, Any]]:
    return [{str(k): v for k, v in case.items()} for case in read_rda(DATA / "dst.rda")["dst"]]


@pytest.mark.parametrize("case", dst_cases(), ids=lambda case: str(scalar(case["id"])))
def test_elapsed_seconds_equal_r_difftime_across_dst(case: dict[str, Any]) -> None:
    """Both 2026 transitions (03-29, 10-25), with and without tzone, NA, zero, negative and .5 rounding."""
    start, end = case["start"], case["end"]
    assert isinstance(start, RTime) and isinstance(end, RTime)
    assert start.tzone == scalar(case["tz_start"]) and end.tzone == scalar(case["tz_end"])
    secs = end.elapsed_since(start)
    r_secs = scalar(case["secs"])
    r_rounded = scalar(case["rounded"])
    if r_secs is None:
        assert secs is None and r_rounded is None
    else:
        assert secs == r_secs
        assert round(secs) == r_rounded  # R round(x, 0) and Python round() both go to the even digit at .5


def test_rtime_display_and_from_datetime() -> None:
    noon = RTime(1790762400.0, "")  # 2026-09-30 12:00:00 Europe/Berlin
    shown = noon.display()
    assert shown is not None
    assert shown.isoformat() == "2026-09-30T12:00:00+02:00"
    assert noon.display("UTC").isoformat() == "2026-09-30T10:00:00+00:00"  # type: ignore[union-attr]
    assert RTime(1790762400.0, "UTC").display().isoformat() == "2026-09-30T10:00:00+00:00"  # type: ignore[union-attr]
    assert RTime(None).display() is None
    assert DEFAULT_TZ == "Europe/Berlin"
    now = dt.datetime(2026, 9, 30, 10, 0, tzinfo=dt.UTC)
    assert RTime.from_datetime(now) == noon
    assert RTime.from_datetime(now).elapsed_since(RTime(1790762400.0 - 90, "Europe/Berlin")) == 90.0
    with pytest.raises(ValueError, match="aware"):
        RTime.from_datetime(dt.datetime(2026, 9, 30, 10, 0))


# ---------------------------------------------------------------------------
# Writing
# ---------------------------------------------------------------------------
@pytest.mark.parametrize("name", [*ROUND_TRIP_SHAPES, "rds_df.rds"])
def test_write_rds_round_trips_in_python(name: str, tmp_path: Path) -> None:
    original = read_rds(DATA / name)
    out = tmp_path / name
    write_rds(out, original)
    copy = read_rds(out)
    if isinstance(original, pd.DataFrame):
        pd.testing.assert_frame_equal(copy, original)
    else:
        assert vector(copy) == vector(original)
    assert out.read_bytes()[:2] == b"\x1f\x8b"


@needs_r
@pytest.mark.parametrize("name", [*ROUND_TRIP_SHAPES, "rds_df.rds"])
def test_write_rds_round_trips_through_r_identical(name: str, tmp_path: Path) -> None:
    """``identical(readRDS(original), readRDS(copy))`` for the four AMT state shapes."""
    out = tmp_path / name
    write_rds(out, read_rds(DATA / name))
    assert r_identical(DATA / name, out)


@needs_r
def test_r_reads_a_frame_built_in_python(tmp_path: Path) -> None:
    frame = pd.DataFrame(
        {
            "RunStatus": ["Normal completion", None, "NA"],
            "Runtime": [77679.0, np.nan, 12.0],
            "summationErrors": pd.array([1, None, 0], dtype="Int32"),
            "flag": [True, None, False],
            "count": [1, 2, 3],
        },
        index=["a", "b", "c"],
    )
    out = tmp_path / "built.rds"
    write_rds(out, frame)
    report = r_eval(
        f'x <- readRDS("{out}"); cat(paste(sapply(x, class), collapse = ","), rownames(x)[2], '
        f"is.na(x$RunStatus[2]), x$RunStatus[3], is.na(x$Runtime[2]), is.nan(x$Runtime[2]), "
        f'is.na(x$summationErrors[2]), is.na(x$flag[2]), sum(x$count), sep = "|")'
    )
    assert report == "character,numeric,integer,logical,integer|b|TRUE|NA|TRUE|FALSE|TRUE|TRUE|6"


@needs_r
def test_r_reads_python_strings_and_null(tmp_path: Path) -> None:
    write_rds(tmp_path / "s.rds", ".*-AMT_2026-09-28|.*-AMT_2026-09-29")
    write_rds(tmp_path / "n.rds", None)
    assert r_eval(f'x <- readRDS("{tmp_path / "s.rds"}"); cat(class(x), length(x), x, sep = "|")') == (
        "character|1|.*-AMT_2026-09-28|.*-AMT_2026-09-29"
    )
    assert r_eval(f'cat(is.null(readRDS("{tmp_path / "n.rds"}")))') == "TRUE"


def test_write_rds_is_atomic(tmp_path: Path) -> None:
    out = tmp_path / "state.rds"
    write_rds(out, "first")
    write_rds(out, "second")
    assert scalar(read_rds(out)) == "second"
    with pytest.raises(NotImplementedError):
        write_rds(out, {"bad": object()})
    assert scalar(read_rds(out)) == "second"
    assert sorted(os.listdir(tmp_path)) == ["state.rds"]


def test_rds_bytes_is_deterministic_gzip() -> None:
    first, second = rds_bytes("abc"), rds_bytes("abc")
    assert first == second
    assert gzip.decompress(first)[:2] == b"X\n"


def test_write_converts_nan_and_none_to_na(tmp_path: Path) -> None:
    out = tmp_path / "na.rds"
    write_rds(out, np.array([1.0, np.nan]))
    assert vector(read_rds(out)) == [1.0, None]
    write_rds(out, {"a": None, "b": float("nan")})
    assert pythonize(read_rds(out)) == {"a": None, "b": None}


# ---------------------------------------------------------------------------
# Error texts and Effects
# ---------------------------------------------------------------------------
def test_missing_files_raise_r_connection_error(tmp_path: Path) -> None:
    with pytest.raises(RParityError, match="^cannot open the connection$"):
        read_rds(tmp_path / "missing.rds")
    with pytest.raises(RParityError, match="^cannot open the connection$"):
        read_rda(tmp_path / "missing.rda")


def test_corrupt_files_raise_r_format_errors(tmp_path: Path) -> None:
    text = tmp_path / "text.rds"
    text.write_text("not an rds file")
    with pytest.raises(RParityError, match="^unknown input format$"):
        read_rds(text)
    with pytest.raises(RParityError, match="^bad restore file magic number"):
        read_rda(text)
    with pytest.raises(RParityError, match="^unknown input format$"):
        read_rds(DATA / "config.Rdata")  # readRDS() on an RData stream
    with pytest.raises(RParityError, match="^bad restore file magic number"):
        read_rda(DATA / "rds_string.rds")  # load() on an RDS stream


def test_memory_errors_are_not_reported_as_format_errors(monkeypatch: pytest.MonkeyPatch) -> None:
    """Running out of memory (the 4.7 GB overview.rds) must surface as such, not as R's format error."""

    def out_of_memory(*args: Any, **kwargs: Any) -> Any:
        raise MemoryError("cannot allocate")

    monkeypatch.setattr(rdata.parser, "parse_data", out_of_memory)
    with pytest.raises(MemoryError):
        read_rds(DATA / "rds_string.rds")
    with pytest.raises(MemoryError):
        read_rda(DATA / "config.Rdata")


def test_readers_go_through_effects_when_given() -> None:
    effects = BytesEffects({"/fake/runstatistics.rda": (DATA / "runstatistics.rda").read_bytes()})
    stats = read_rda("/fake/runstatistics.rda", effects)["stats"]
    assert scalar(stats["id"]) == "179059272175832"
    assert effects.calls == [("read_bytes", "/fake/runstatistics.rda")]
    with pytest.raises(RParityError, match="cannot open the connection"):
        read_rds("/fake/none.rds", effects)


# ---------------------------------------------------------------------------
# Every real fixture file of each kind (only where the fixture tree is linked)
# ---------------------------------------------------------------------------
FIXTURE_KINDS = {
    "config.Rdata": "rda",
    "config.yml": "yml",
    "runstatistics.rda": "rda",
    "projectSummations.rds": "rds",
    "gRS.rds": "rds",
    "runcode.rds": "rds",
    "runsToStart.rds": "rds",
    "lastcommit.rds": "rds",
    "report.rds": "rds",
}


def test_readers_on_every_fixture_file(paths: Any, fixtures_available: bool) -> None:
    """Reads every fixture file of the kinds the port uses; overview.rds is excluded, see the note."""
    if not fixtures_available:
        pytest.skip("migration/fixtures is not linked")
    from modelstats.config import load_config

    root = paths.fixtures / "p"
    counts: dict[str, int] = {}
    failures: list[str] = []
    for name, kind in FIXTURE_KINDS.items():
        for path in sorted(root.rglob(name)):
            counts[name] = counts.get(name, 0) + 1
            try:
                value = (
                    load_config(path)
                    if kind in {"rda", "yml"} and name.startswith("config")
                    else (read_rda(path) if kind == "rda" else read_rds(path))
                )
                if name == "runstatistics.rda":
                    assert "stats" in value
            except Exception as exc:  # noqa: BLE001 - collected and reported together
                failures.append(f"{path}: {type(exc).__name__}: {exc}")
    assert not failures, "\n".join(failures)
    assert counts, "no fixture files found"
    # overview.rds (results archive, 159100 x 4141 data.table, 4.4-4.7 GiB serialised) is only stat()-ed by
    # R (getRunStatus.R:114, file.info()$mtime) and is never deserialised; rdata ran out of memory on it even
    # under a 20 GiB cap (peak RSS 20.1 GB after 175 s, 2026-10-01).
    assert len(list(root.rglob("overview.rds"))) >= 1
