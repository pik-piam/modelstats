"""runstats: the ``stats`` list of ``runstatistics.rda`` as getRunStatus reads it."""

from __future__ import annotations

import datetime as dt
import json
import os
from pathlib import Path
from typing import Any

import pytest

from modelstats.errors import RParityError
from modelstats.rdata_io import RTime
from modelstats.runstats import RunStatistics

DATA = Path(__file__).parent / "data"
EXPECTED: dict[str, Any] = json.loads((DATA / "expected.json").read_text(encoding="utf-8"))


class FsEffects:
    def __init__(self) -> None:
        self.calls: list[tuple[str, str]] = []

    def exists(self, path: str) -> bool:
        self.calls.append(("exists", path))
        return os.path.exists(path)

    def read_bytes(self, path: str) -> bytes:
        self.calls.append(("read_bytes", path))
        return Path(path).read_bytes()


def r_as_character_join(values: list[Any]) -> str:
    """``paste0(as.character(x), collapse = "")`` for the small numbers in modelstat vectors."""
    return "".join("NA" if v is None else str(int(v)) if float(v).is_integer() else str(v) for v in values)


def test_missing_file_gives_none_without_reading(tmp_path: Path) -> None:
    effects = FsEffects()
    assert RunStatistics.load(tmp_path / "runstatistics.rda", effects) is None
    assert effects.calls == [("exists", str(tmp_path / "runstatistics.rda"))]


def test_file_without_stats_object_gives_none() -> None:
    assert RunStatistics.load(DATA / "nostats.rda", FsEffects()) is None


def test_remind_statistics() -> None:
    stats = RunStatistics.load(DATA / "runstatistics.rda", FsEffects())
    assert stats is not None
    expect = EXPECTED["runstatistics"]
    assert stats.names == expect["names"]
    assert stats.id == expect["id"] and isinstance(stats.id, str)
    assert stats.model_name == "REMIND"
    assert stats.modelstat == [2.0]
    assert stats.config is not None and stats.config["title"] is not None
    assert stats.timePrepareStart == RTime(float(expect["timePrepareStart"]), "Europe/Berlin")
    assert stats.timeGAMSStart == RTime(float(expect["timeGAMSStart"]), "")
    assert stats.timeGAMSEnd is not None
    assert stats.timeGAMSEnd.epoch == pytest.approx(float(expect["timeGAMSEnd"]), abs=1e-6)
    assert stats.raw["timeOutputStart"] == RTime(None, "")


def test_has_is_any_grepl_over_names() -> None:
    stats = RunStatistics.load(DATA / "runstatistics.rda")
    assert stats is not None
    assert stats.has("GAMSEnd") and stats.has("config") and stats.has("id") and stats.has("timePrepareStart")
    assert stats.has("time")  # matches several names, like grepl
    assert stats.has("^id$") and stats.has("GAMS")
    assert not stats.has("Warnings") and not stats.has("^GAMSEnd$")
    assert stats.has("modelstat")


def test_runtime_like_r_round_difftime() -> None:
    stats = RunStatistics.load(DATA / "runstatistics.rda")
    assert stats is not None
    assert stats.timeGAMSEnd is not None and stats.timeGAMSStart is not None
    secs = stats.timeGAMSEnd.elapsed_since(stats.timeGAMSStart)
    assert secs == pytest.approx(EXPECTED["runstatistics"]["gams_secs"])
    assert secs is not None and round(secs) == EXPECTED["runstatistics"]["gams_rounded"]
    now = RTime.from_datetime(dt.datetime(2026, 9, 30, 12, 0, tzinfo=dt.timezone(dt.timedelta(hours=2))))
    assert stats.timePrepareStart is not None
    live = now.elapsed_since(stats.timePrepareStart)
    assert live == 1790762400.0 - 1790584390.0


def test_magpie_modelstat_vector_from_plain_vector_and_magpie_object() -> None:
    plain = RunStatistics.load(DATA / "runstatistics_vec.rda")
    assert plain is not None
    assert plain.model_name == "MAgPIE"
    assert plain.modelstat == [2.0, 2.0, None, 2.0, 13.0, 2.0]
    assert r_as_character_join(plain.modelstat) == EXPECTED["modelstat_vec"] == "22NA2132"
    path = DATA / "runstatistics_magpie.rda"
    if not path.exists():
        pytest.skip("make_data.R ran without magclass")
    magpie = RunStatistics.load(path)
    assert magpie is not None
    assert magpie.modelstat == [2.0, 2.0, None, 13.0]
    assert r_as_character_join(magpie.modelstat) == EXPECTED["modelstat_magpie"] == "22NA13"
    assert magpie.timePrepareStart is None and not magpie.has("timePrepareStart")


def test_config_without_model_name() -> None:
    stats = RunStatistics.load(DATA / "runstatistics_noname.rda")
    assert stats is not None
    assert stats.has("config") and stats.model_name is None
    assert stats.modelstat == [] and not stats.has("modelstat")
    assert stats.timeGAMSStart is None and stats.timeGAMSEnd is None
    assert stats.timePrepareStart is not None


def test_corrupt_file_raises_like_load(tmp_path: Path) -> None:
    bad = tmp_path / "runstatistics.rda"
    bad.write_text("garbage")
    with pytest.raises(RParityError, match="^bad restore file magic number"):
        RunStatistics.load(bad, FsEffects())


def test_load_through_canned_effects() -> None:
    class Canned:
        def __init__(self, files: dict[str, bytes]) -> None:
            self.files = files

        def exists(self, path: str) -> bool:
            return path in self.files

        def read_bytes(self, path: str) -> bytes:
            return self.files[path]

    effects = Canned({"/run/runstatistics.rda": (DATA / "runstatistics_vec.rda").read_bytes()})
    stats = RunStatistics.load("/run/runstatistics.rda", effects)
    assert stats is not None and stats.id == "178978767733374"
    assert RunStatistics.load("/other/runstatistics.rda", effects) is None
