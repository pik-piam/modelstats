"""amt.state: the four RDS state files of the AMT (plan 03 section 2.3, D-17).

The always-running tests use the small R-written shapes under ``tests/unit/data/`` (``rt_*.rds``,
``make_data.R``) and in-memory tables; the ``identical()`` round trips need ``Rscript`` and the real
fixtures (``migration/fixtures/p/projects/remind/modeltests/remind/``, ``landuse/tests/magpie/``), which
are gitignored, so those tests skip with a clear reason where either is absent. The R facts pinned
here were probed with R 4.6.1 on 2026-10-01 (``rbind.data.frame`` errors and promotion,
``make.unique(sep = "")``, ``readRDS`` conditions, ``data.frame()`` row names).

Writes go through ``Effects.write_rds`` (atomic in ``ProductionEffects``, logged by ``DryRunEffects``);
the doubles here are ``ProductionEffects`` with the calls recorded, and a recorder that never touches
the disk. The torn-write tests make the rename or the fsync of the atomic write fail.
"""

from __future__ import annotations

import hashlib
import os
import shutil
import subprocess
import warnings
from pathlib import Path

import numpy as np
import pandas as pd
import pytest

from modelstats.amt import state
from modelstats.amt.state import (
    GrsTable,
    column_r_types,
    grs_from_frame,
    grs_to_frame,
    infer_r_type,
    rbind_status,
    read_grs,
    read_lastcommit,
    read_runcode,
    read_runs_to_start,
    run_names,
    save_rds,
    with_amt_suffix,
    write_grs,
    write_lastcommit,
    write_runcode,
    write_runs_to_start,
)
from modelstats.env import DryRunEffects, PathLike, ProductionEffects
from modelstats.errors import RParityError, RWarning
from modelstats.rdata_io import rds_bytes, read_rds, vector
from modelstats.run_status import StatusTable

DATA = Path(__file__).resolve().parent / "data"
REPO = Path(__file__).resolve().parents[2]
REMIND = REPO / "migration/fixtures/p/projects/remind/modeltests/remind"
MAGPIE = REPO / "migration/fixtures/p/projects/landuse/tests/magpie"
HAS_R = shutil.which("Rscript") is not None
needs_r = pytest.mark.skipif(not HAS_R, reason="Rscript is not on PATH")
needs_fixtures = pytest.mark.skipif(
    not (REMIND / "output" / "gRS.rds").is_file(), reason="migration/fixtures (gitignored) is absent"
)


def r_eval(code: str) -> str:
    result = subprocess.run(["Rscript", "-e", code], capture_output=True, text=True, check=True)
    return result.stdout.strip()


def r_identical(a: Path, b: Path) -> bool:
    return r_eval(f'cat(identical(readRDS("{a}"), readRDS("{b}")))') == "TRUE"


def sha256(path: Path) -> str:
    return hashlib.sha256(path.read_bytes()).hexdigest()


# --------------------------------------------------------------------------- Effects doubles


class WritingEffects(ProductionEffects):
    """ProductionEffects with the state writes recorded (real, atomic filesystem writes)."""

    def __init__(self) -> None:
        self.calls: list[tuple[str, str]] = []

    def write_rds(self, path: PathLike, obj: object) -> None:
        self.calls.append(("write_rds", os.fspath(path)))
        super().write_rds(path, obj)


class RecordingOnlyEffects(ProductionEffects):
    """Reads for real, records every state write and performs none."""

    def __init__(self) -> None:
        self.calls: list[tuple[str, str]] = []

    def write_rds(self, path: PathLike, obj: object) -> None:
        self.calls.append(("write_rds", os.fspath(path)))


def no_space(*_args: object, **_kwargs: object) -> None:
    raise OSError(28, "No space left on device")


# --------------------------------------------------------------------------- builders


def table(rows: dict[str, dict[str, object]]) -> StatusTable:
    """A StatusTable filled row by row, column by column, like getRunStatus does."""
    out = StatusTable()
    for rowname, cells in rows.items():
        for column, value in cells.items():
            out[rowname, column] = value
    return out


GRS_ROW = {
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
GRS_TYPES: dict[str, state.RType] = {
    **dict.fromkeys(["jobInSLURM", "RunType", "modelstat", "runInAppResults", "Mif", "Iter"], "character"),
    **dict.fromkeys(["RunStatus", "Warnings", "Conv"], "character"),
    **dict.fromkeys(["Runtime", "summationErrors", "rangeErrors", "fixErrors"], "double"),
    **dict.fromkeys(["missingProjVars", "projSummationErrors", "projSummationErrorsRegional"], "integer"),
}


def cells(table: StatusTable, column: str) -> list[object]:
    return [row[column] for row in table.rows()]


# --------------------------------------------------------------------------- reading the shapes


def test_read_small_shapes_from_r() -> None:
    eff = WritingEffects()
    assert read_runcode(DATA / "rt_runcode.rds", eff) == scalar_of(DATA / "rt_runcode.rds")
    assert read_lastcommit(DATA / "rt_lastcommit.rds", eff) == scalar_of(DATA / "rt_lastcommit.rds")
    frame = read_runs_to_start(DATA / "rt_runstostart.rds", eff)
    assert isinstance(frame, pd.DataFrame)
    assert run_names(frame) == [str(name) for name in frame.index]
    grs = read_grs(DATA / "rt_grs.rds", eff)
    assert isinstance(grs, GrsTable)
    assert grs.rownames == [str(name) for name in read_rds(DATA / "rt_grs.rds").index]
    assert set(grs.r_types) == set(grs.columns)


def scalar_of(path: Path) -> str:
    values = vector(read_rds(path))
    assert len(values) == 1 and isinstance(values[0], str)
    return values[0]


def test_grs_from_frame_maps_r_types_and_na() -> None:
    frame = pd.DataFrame(
        {
            "chr": pd.array(["a", None, "NA"], dtype="string"),
            "dbl": np.array([1.5, np.nan, 3.0]),
            "int": pd.array([1, None, 3], dtype="Int32"),
            "lgl": pd.array([True, None, False], dtype="boolean"),
        },
        index=["r1", "r2", "r3"],
    )
    grs = grs_from_frame(frame)
    assert grs.r_types == {"chr": "character", "dbl": "double", "int": "integer", "lgl": "logical"}
    assert grs.rownames == ["r1", "r2", "r3"]
    assert cells(grs, "chr") == ["a", None, "NA"]  # NA_character_ is None, the string "NA" stays a string
    assert cells(grs, "dbl") == [1.5, None, 3]  # integral doubles are ints in the table (StatusTable convention)
    assert cells(grs, "int") == [1, None, 3]
    assert cells(grs, "lgl") == [True, None, False]
    assert isinstance(cells(grs, "dbl")[2], int)


def test_grs_from_frame_without_na_and_with_numpy_dtypes() -> None:
    frame = pd.DataFrame({"i": np.array([1, 2], dtype=np.int32), "b": np.array([True, False]), "s": ["x", "y"]})
    grs = grs_from_frame(frame)
    assert grs.r_types == {"i": "integer", "b": "logical", "s": "character"}
    assert grs.rownames == ["0", "1"]  # a RangeIndex has no R row names; str() of the labels
    with pytest.raises(TypeError, match="not an R data.frame column type"):
        grs_from_frame(pd.DataFrame({"t": pd.to_datetime(["2026-09-30"])}))


def test_grs_to_frame_uses_the_remembered_types() -> None:
    grs = GrsTable({"chr": "character", "dbl": "double", "int": "integer", "lgl": "logical"})
    for rowname, (c, d, i, lg) in {"r1": ("a", 1, 1, True), "r2": (None, None, None, None)}.items():
        grs[rowname, "chr"] = c
        grs[rowname, "dbl"] = d
        grs[rowname, "int"] = i
        grs[rowname, "lgl"] = lg
    frame = grs_to_frame(grs)
    assert list(frame.index) == ["r1", "r2"] and frame.index.dtype == object
    assert frame["chr"].dtype == object and frame["chr"].tolist() == ["a", None]
    assert frame["dbl"].dtype == np.float64 and frame["dbl"].iloc[0] == 1.0 and np.isnan(frame["dbl"].iloc[1])
    assert str(frame["int"].dtype) == "Int32" and frame["int"].iloc[0] == 1 and pd.isna(frame["int"].iloc[1])
    assert str(frame["lgl"].dtype) == "boolean" and frame["lgl"].iloc[0] is np.True_
    # a remembered type is promoted when later cells need it (NA then 3L is integer in R)
    grs2 = GrsTable({"x": "logical"})
    grs2["r1", "x"] = 3
    assert column_r_types(grs2) == {"x": "integer"}


def test_grs_to_frame_of_an_empty_table_has_integer_row_names() -> None:
    frame = grs_to_frame(StatusTable())
    assert frame.shape == (0, 0)
    assert isinstance(frame.index, pd.RangeIndex)  # written as data.frame(): integer(0) row names


# --------------------------------------------------------------------------- type inference (getRunStatus columns)


@pytest.mark.parametrize(
    ("column", "values", "expected"),
    [
        ("Runtime", [None, None], "logical"),  # out[i, "Runtime"] <- NA for every row
        ("Runtime", [None, 8304], "double"),  # as.numeric(round(difftime(...)))
        ("summationErrors", [0, 1], "double"),  # the literal 0 is double, length() integer -> double
        ("rangeErrors", [0], "double"),
        ("fixErrors", [2443], "double"),
        ("missingProjVars", [2, None], "integer"),  # projectSummations.rds holds integers
        ("projSummationErrors", [35], "integer"),
        ("projSummationErrorsRegional", [47], "integer"),
        ("RunType", ["nash", None], "character"),
        ("RunType", [None], "logical"),  # colRunType gave NA for every row
        ("Warnings", ["27", "0"], "character"),
        ("other", [1, 2], "integer"),
        ("other", [1.5], "double"),
        ("other", [1.0, 2], "double"),
        ("other", [True, None], "logical"),
        ("other", [True, 2], "integer"),
        ("other", ["a", 2], "character"),
    ],
)
def test_infer_r_type(column: str, values: list[object], expected: str) -> None:
    assert infer_r_type(column, values) == expected


def test_fresh_table_gets_the_fixture_column_types() -> None:
    fresh = table({"run-AMT_2026-09-28_10.30.27": dict(GRS_ROW), "archive": {**GRS_ROW, "Runtime": None}})
    assert column_r_types(fresh) == GRS_TYPES
    assert list(column_r_types(fresh)) == list(GRS_ROW)  # column order of first assignment


# --------------------------------------------------------------------------- rbind


def test_rbind_with_null_old_is_the_new_table() -> None:
    new = table({"a": dict(GRS_ROW)})
    out = rbind_status(None, new)
    assert isinstance(out, GrsTable)
    assert out.rownames == ["a"] and out.columns == new.columns
    assert out.r_types == GRS_TYPES
    assert cells(out, "Runtime") == [8304]


def test_rbind_keeps_column_order_and_appends_rows() -> None:
    old = GrsTable({"a": "character", "b": "integer"})
    old["r1", "a"] = "x"
    old["r1", "b"] = 1
    old["r2", "a"] = None
    old["r2", "b"] = None
    new = table({"r3": {"b": 9, "a": "w"}})  # columns in the other order: matched by name
    out = rbind_status(old, new)
    assert out.columns == ["a", "b"]
    assert out.rownames == ["r1", "r2", "r3"]
    assert cells(out, "a") == ["x", None, "w"]
    assert cells(out, "b") == [1, None, 9]
    assert out.r_types == {"a": "character", "b": "integer"}


def test_rbind_promotes_column_types_like_r() -> None:
    old = GrsTable({"c": "character", "d": "double", "i": "integer", "l": "logical"})
    old["r1", "c"] = "x"
    old["r1", "d"] = 1
    old["r1", "i"] = 1
    old["r1", "l"] = None
    new = GrsTable({"c": "logical", "d": "integer", "i": "double", "l": "character"})
    new["r2", "c"] = None  # logical NA into character: NA_character_
    new["r2", "d"] = 2  # integer into double: double
    new["r2", "i"] = 2.5  # double into integer: double
    new["r2", "l"] = "s"  # character into logical: character
    out = rbind_status(old, new)
    assert out.r_types == {"c": "character", "d": "double", "i": "double", "l": "character"}
    assert cells(out, "c") == ["x", None]
    assert cells(out, "d") == [1, 2]
    assert cells(out, "i") == [1, 2.5]
    assert cells(out, "l") == [None, "s"]


def test_rbind_coerces_numbers_to_character_as_r_does() -> None:
    old = GrsTable({"a": "character"})
    old["r1", "a"] = "x"
    new = GrsTable({"a": "double"})
    new["r2", "a"] = 100000
    new["r3", "a"] = 2.5
    out = rbind_status(old, new)
    assert cells(out, "a") == ["x", "1e+05", "2.5"]  # as.character(1e5) is "1e+05" for a double
    new_int = GrsTable({"a": "integer"})
    new_int["r2", "a"] = 100000
    assert cells(rbind_status(old, new_int), "a") == ["x", "100000"]  # but "100000" for an integer
    new_lgl = GrsTable({"a": "logical"})
    new_lgl["r2", "a"] = True
    assert cells(rbind_status(old, new_lgl), "a") == ["x", "TRUE"]


def test_rbind_makes_duplicate_row_names_unique_with_empty_sep() -> None:
    old = table({"r1": {"a": "x"}, "r2": {"a": "y"}})
    new = table({"r1": {"a": "z"}})
    assert rbind_status(old, new).rownames == ["r1", "r2", "r11"]
    # make.unique(c("r1", "r2", "r1", "r11"), sep = "") is r1 r2 r12 r11
    assert rbind_status(old, table({"r1": {"a": "z"}, "r11": {"a": "q"}})).rownames == ["r1", "r2", "r12", "r11"]
    # make.unique(c("b", "b", "b", "b1", "b2"), sep = "") is b b3 b4 b1 b2
    assert state._make_unique(["b", "b", "b", "b1", "b2"], sep="") == ["b", "b3", "b4", "b1", "b2"]
    assert state._make_unique(["a", "a", "a1", "a"], sep="") == ["a", "a2", "a1", "a3"]


def test_rbind_errors_carry_r_text_and_call() -> None:
    old = table({"r1": dict(GRS_ROW)})
    brief = table({"archive": {k: GRS_ROW[k] for k in list(GRS_ROW)[:10]}})  # a run without the plausibility columns
    with pytest.raises(RParityError, match=r"^numbers of columns of arguments do not match$") as info:
        rbind_status(old, brief)
    assert info.value.call == "rbind(deparse.level, ...)"
    renamed = table({"q": {**{k: GRS_ROW[k] for k in list(GRS_ROW)[:15]}, "other": 1}})
    with pytest.raises(RParityError, match=r"^names do not match previous names$") as info:
        rbind_status(old, renamed)
    assert info.value.call == "match.names(clabs, names(xi))"


def test_rbind_drops_zero_column_arguments() -> None:
    old = table({"r1": dict(GRS_ROW)})
    out = rbind_status(old, StatusTable())  # getRunStatus(character(0)) is data.frame()
    assert out.rownames == ["r1"] and out.columns == old.columns and out.r_types == GRS_TYPES
    out = rbind_status(StatusTable(), old)
    assert out.rownames == ["r1"] and out.columns == old.columns
    assert rbind_status(StatusTable(), StatusTable()).columns == []


def test_rbind_keeps_registered_paths() -> None:
    old = table({"r1": {"a": "x"}})
    old.register_path("r1", "/runs/r1")
    new = table({"r2": {"a": "y"}})
    new.register_path("r2", "/runs/r2")
    out = rbind_status(old, new)
    assert [row.path for row in out.rows()] == ["/runs/r1", "/runs/r2"]


@needs_r
def test_rbind_equals_r_rbind(tmp_path: Path) -> None:
    """``identical(rbind(readRDS(old), readRDS(new)), readRDS(python))`` with promotion and duplicate names."""
    eff = WritingEffects()
    old = GrsTable({"c": "character", "d": "double", "i": "integer", "l": "logical"})
    for rowname, (c, d, i) in {"r1": ("x", 1, 5), "r2": ("NA", None, None), "r11": ("y", 2.5, 7)}.items():
        old[rowname, "c"] = c
        old[rowname, "d"] = d
        old[rowname, "i"] = i
        old[rowname, "l"] = None
    new = table({"r1": {"c": None, "d": 3, "i": 1.5, "l": "s"}, "r3": {"c": "z", "d": 100000, "i": 0, "l": None}})
    write_grs(tmp_path / "old.rds", old, eff)
    write_grs(tmp_path / "new.rds", new, eff)
    write_grs(tmp_path / "python.rds", rbind_status(old, new), eff)
    report = r_eval(
        f'o <- readRDS("{tmp_path / "old.rds"}"); n <- readRDS("{tmp_path / "new.rds"}"); '
        f'p <- readRDS("{tmp_path / "python.rds"}"); r <- rbind(o, n); '
        f'cat(identical(r, p), paste(rownames(r), collapse = ","), paste(sapply(r, class), collapse = ","), sep = "|")'
    )
    assert report == "TRUE|r1,r2,r11,r12,r3|character,numeric,numeric,character"


# --------------------------------------------------------------------------- errors like R


def test_missing_file_fails_like_readrds(tmp_path: Path) -> None:
    missing = tmp_path / "lastcommit.rds"
    with warnings.catch_warnings(record=True) as caught:
        warnings.simplefilter("always")
        with pytest.raises(RParityError, match=r"^cannot open the connection$") as info:
            read_lastcommit(missing, WritingEffects())
    assert info.value.call == 'gzfile(file, "rb")'
    [warning] = [w.message for w in caught if isinstance(w.message, RWarning)]
    assert warning.call == 'gzfile(file, "rb")'
    assert warning.text == f"cannot open compressed file '{missing}', probable reason 'No such file or directory'"
    for reader in (read_runcode, read_runs_to_start, read_grs):
        with warnings.catch_warnings():
            warnings.simplefilter("ignore")
            with pytest.raises(RParityError, match=r"^cannot open the connection$"):
                reader(tmp_path / "nope.rds", WritingEffects())


def test_corrupt_file_fails_like_readrds(tmp_path: Path) -> None:
    corrupt = tmp_path / "state.rds"
    corrupt.write_text("garbage\n")  # the prepare script of remind-evaluate-state-corrupt: echo garbage > runcode.rds
    expected_calls = {
        read_lastcommit: 'readRDS(paste0(mydir, "/lastcommit.rds"))',
        read_runcode: 'readRDS(paste0(mydir, "/runcode.rds"))',
        read_runs_to_start: 'readRDS(paste0(mydir, "runsToStart.rds"))',
        read_grs: 'readRDS("gRS.rds")',
    }
    for reader, call in expected_calls.items():
        with pytest.raises(RParityError, match=r"^unknown input format$") as info:
            reader(corrupt, WritingEffects())
        assert info.value.call == call


def test_wrong_object_kind_is_a_type_error(tmp_path: Path) -> None:
    eff = WritingEffects()
    save_rds(tmp_path / "frame.rds", pd.DataFrame({"a": [1]}), eff)
    save_rds(tmp_path / "num.rds", 3.0, eff)
    save_rds(tmp_path / "null.rds", None, eff)
    with pytest.raises(TypeError, match="data.frame, not a character string"):
        read_lastcommit(tmp_path / "frame.rds", eff)
    with pytest.raises(TypeError, match="not a character string"):
        read_runcode(tmp_path / "num.rds", eff)
    with pytest.raises(TypeError, match="NULL, not a data.frame"):
        read_grs(tmp_path / "null.rds", eff)
    with pytest.raises(TypeError, match="not a data.frame"):
        read_runs_to_start(tmp_path / "num.rds", eff)


# --------------------------------------------------------------------------- atomic writes through Effects


def test_write_goes_through_effects_only(tmp_path: Path) -> None:
    eff = RecordingOnlyEffects()
    target = tmp_path / "gRS.rds"
    write_grs(target, table({"r1": dict(GRS_ROW)}), eff)
    assert eff.calls == [("write_rds", str(target))]
    assert list(tmp_path.iterdir()) == []  # nothing reached the disk


def test_dry_run_logs_the_state_write_and_writes_nothing(tmp_path: Path) -> None:
    dry = DryRunEffects()
    target = tmp_path / "gRS.rds"
    grs = table({"r1": dict(GRS_ROW)})
    write_grs(target, grs, dry)
    write_runcode(tmp_path / "runcode.rds", ".*-AMT_2026-09-30|.*-AMT_2026-10-01", dry)
    assert list(tmp_path.iterdir()) == []
    [first, second] = dry.events
    assert str(first) == f"would write {target} (RDS, {len(first.data or b'')} bytes)"
    assert first.data == rds_bytes(grs_to_frame(grs))  # what saveRDS would have written
    assert second.target == str(tmp_path / "runcode.rds")


def test_write_is_atomic_and_replaces_the_old_state(tmp_path: Path) -> None:
    eff = WritingEffects()
    target = tmp_path / "runcode.rds"
    write_runcode(target, "first", eff)
    write_runcode(target, "second", eff)
    assert read_runcode(target, eff) == "second"
    assert sorted(p.name for p in tmp_path.iterdir()) == ["runcode.rds"]  # no temporary file left behind
    assert eff.calls == [("write_rds", str(target))] * 2


@pytest.mark.parametrize("failing", ["replace", "fsync"])
def test_partial_write_leaves_the_old_state_intact(
    failing: str, tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    """A write that dies before (fsync) or at (rename) the final step leaves the old file byte for byte."""
    target = tmp_path / "lastcommit.rds"
    eff = WritingEffects()
    write_lastcommit(target, "13f60fdd4fff366bebcc7dfe522897dedeb43cc7", eff)
    before = sha256(target)
    monkeypatch.setattr(os, failing, no_space)
    with pytest.raises(OSError, match="No space left"):
        write_lastcommit(target, "0000000000000000000000000000000000000000", eff)
    monkeypatch.undo()
    assert sha256(target) == before
    assert read_lastcommit(target, eff) == "13f60fdd4fff366bebcc7dfe522897dedeb43cc7"
    assert sorted(p.name for p in tmp_path.iterdir()) == ["lastcommit.rds"]  # the torn temporary file was removed


def test_unwritable_object_touches_nothing(tmp_path: Path) -> None:
    eff = WritingEffects()
    with pytest.raises(NotImplementedError):
        save_rds(tmp_path / "x.rds", {"bad": object()}, eff)
    assert list(tmp_path.iterdir()) == []


def test_leftover_temp_file_does_not_disturb_reading_or_writing(tmp_path: Path) -> None:
    eff = WritingEffects()
    target = tmp_path / "gRS.rds"
    write_grs(target, table({"r1": dict(GRS_ROW)}), eff)
    leftover = tmp_path / ".gRS.rds.0123456789ab.tmp"  # a writer killed before its rename
    leftover.write_text("torn\n")
    assert read_grs(target, eff).rownames == ["r1"]
    assert eff.listdir_like_r(tmp_path) == ["gRS.rds"]  # R's dir() does not see the dot-file either
    write_grs(target, table({"r2": dict(GRS_ROW)}), eff)
    assert read_grs(target, eff).rownames == ["r2"]
    assert sorted(p.name for p in tmp_path.iterdir()) == [leftover.name, "gRS.rds"]


# --------------------------------------------------------------------------- runsToStart helpers


def test_with_amt_suffix_renames_rows_on_a_copy() -> None:
    frame = pd.DataFrame({"start": ["1,AMT,2"], "cm_startyear": pd.array([2030], dtype="Int32")}, index=["SSP2-NPi"])
    out = with_amt_suffix(frame)
    assert run_names(out) == ["SSP2-NPi-AMT"]
    assert run_names(frame) == ["SSP2-NPi"]
    assert out["cm_startyear"].dtype == frame["cm_startyear"].dtype


# --------------------------------------------------------------------------- R round trips (identical())


@needs_r
@pytest.mark.parametrize("name", ["rt_runcode.rds", "rt_lastcommit.rds", "rt_runstostart.rds", "rt_grs.rds"])
def test_small_shapes_round_trip_through_r_identical(name: str, tmp_path: Path) -> None:
    eff = WritingEffects()
    out = tmp_path / name
    if name == "rt_runcode.rds":
        write_runcode(out, read_runcode(DATA / name, eff), eff)
    elif name == "rt_lastcommit.rds":
        write_lastcommit(out, read_lastcommit(DATA / name, eff), eff)
    elif name == "rt_runstostart.rds":
        write_runs_to_start(out, read_runs_to_start(DATA / name, eff), eff)
    else:
        write_grs(out, read_grs(DATA / name, eff), eff)
    assert r_identical(DATA / name, out)


@needs_r
def test_empty_table_round_trips_as_data_frame(tmp_path: Path) -> None:
    write_grs(tmp_path / "empty.rds", StatusTable(), WritingEffects())
    assert r_eval(f'cat(identical(data.frame(), readRDS("{tmp_path / "empty.rds"}")))') == "TRUE"


@needs_r
@needs_fixtures
@pytest.mark.parametrize(
    "fixture",
    [
        REMIND / "lastcommit.rds",
        REMIND / "runcode.rds",
        REMIND / "runsToStart.rds",
        REMIND / "output" / "gRS.rds",
        MAGPIE / "lastcommit.rds",
        MAGPIE / "runcode.rds",
    ],
    ids=lambda p: f"{p.parent.parent.name}/{p.parent.name}/{p.name}",
)
def test_fixture_round_trips_through_r_identical(fixture: Path, tmp_path: Path) -> None:
    """The evidence of the unit: a Python write of each state fixture reads back identical() in R."""
    eff = WritingEffects()
    out = tmp_path / fixture.name
    if fixture.name == "lastcommit.rds":
        write_lastcommit(out, read_lastcommit(fixture, eff), eff)
    elif fixture.name == "runcode.rds":
        write_runcode(out, read_runcode(fixture, eff), eff)
    elif fixture.name == "runsToStart.rds":
        write_runs_to_start(out, read_runs_to_start(fixture, eff), eff)
    else:
        write_grs(out, read_grs(fixture, eff), eff)
    assert r_identical(fixture, out)


@needs_fixtures
def test_grs_fixture_shape_and_na_conventions() -> None:
    """``str()`` of the fixture: 349 runs, 16 columns, literal "NA" strings only in the character columns."""
    grs = read_grs(REMIND / "output" / "gRS.rds", WritingEffects())
    assert len(grs) == 349 and grs.columns == list(GRS_ROW)
    assert grs.r_types == GRS_TYPES
    assert sum(1 for v in cells(grs, "RunType") if v == "NA") == 5
    assert all(v is not None for column in list(GRS_ROW)[:9] for v in cells(grs, column))
    assert sum(1 for v in cells(grs, "Runtime") if v is None) == 12
    assert sum(1 for v in cells(grs, "summationErrors") if v is None) == 58
    assert sum(1 for v in cells(grs, "missingProjVars") if v is None) == 82
    assert grs.rownames[0] == "SSP2-EU21-EU-Ger-NZ-AMT_2026-09-19_01.28.17"
    assert grs["SSP2-NPi-AMT_2026-09-28_10.30.27", "Runtime"] == 8304
    assert isinstance(grs["SSP2-NPi-AMT_2026-09-28_10.30.27", "Runtime"], int)


@needs_r
@needs_fixtures
def test_fresh_table_writes_the_fixture_column_classes(tmp_path: Path) -> None:
    """A plain StatusTable with the fixture's cells (types inferred) writes the same column classes as R."""
    eff = WritingEffects()
    grs = read_grs(REMIND / "output" / "gRS.rds", eff)
    fresh = StatusTable()
    for row in grs.rows():
        for column in grs.columns:
            fresh[row.rowname, column] = row[column]
    write_grs(tmp_path / "gRS.rds", fresh, eff)
    assert r_identical(REMIND / "output" / "gRS.rds", tmp_path / "gRS.rds")


@needs_r
@needs_fixtures
def test_rbind_with_the_fixture_history_through_r(tmp_path: Path) -> None:
    """``rbind(gRSold, new)`` of the fixture history with a new run equals R's rbind (column order, row names)."""
    eff = WritingEffects()
    old = read_grs(REMIND / "output" / "gRS.rds", eff)
    new = table({"new-AMT_2026-09-30_12.00.00": dict(GRS_ROW)})
    out = rbind_status(old, new)
    assert out.columns == old.columns and out.rownames == [*old.rownames, "new-AMT_2026-09-30_12.00.00"]
    assert out.r_types == GRS_TYPES
    write_grs(tmp_path / "new.rds", new, eff)
    write_grs(tmp_path / "python.rds", out, eff)
    assert (
        r_eval(
            f'o <- readRDS("{REMIND / "output" / "gRS.rds"}"); n <- readRDS("{tmp_path / "new.rds"}"); '
            f'cat(identical(rbind(o, n), readRDS("{tmp_path / "python.rds"}")))'
        )
        == "TRUE"
    )


@needs_r
@needs_fixtures
def test_runs_to_start_fixture_with_amt_suffix_through_r(tmp_path: Path) -> None:
    """``row.names(runsToStart) <- paste0(row.names(runsToStart), "-AMT")`` then saveRDS, compared in R."""
    eff = WritingEffects()
    frame = read_runs_to_start(REMIND / "runsToStart.rds", eff)
    write_runs_to_start(tmp_path / "runsToStart.rds", with_amt_suffix(frame), eff)
    assert (
        r_eval(
            f'a <- readRDS("{REMIND / "runsToStart.rds"}"); row.names(a) <- paste0(row.names(a), "-AMT"); '
            f'cat(identical(a, readRDS("{tmp_path / "runsToStart.rds"}")))'
        )
        == "TRUE"
    )
