"""run_status: ``get_run_status`` and ``StatusTable`` against R/getRunStatus.R (R 4.6.1 facts pinned 2026-10-01).

Every scenario is built in ``tmp_path`` from the unit-test data (``data/config.Rdata``,
``data/config.yml.txt``, the ``runstatistics*.rda`` variants, the synthetic GDX matrix) and
observed through ``FakeEffects`` (frozen clock 2026-09-30 12:00 Europe/Berlin, canned squeue).
The golden tier (``tests/golden/test_status.py``) proves parity on the real fixtures; this
module pins the R semantics the port relies on and the branches no golden reaches (the
coupled ``mag-N`` recursion, ``Run MAgPIE `` without a loop, the sort of non-directories).
"""

from __future__ import annotations

import datetime as dt
import os
import shutil
import zoneinfo
from pathlib import Path
from typing import Any

import pytest
import yaml
from _fake_effects import DEFAULT_FROZEN, FakeEffects

from modelstats import found_in_slurm, get_run_status
from modelstats.errors import RParityError
from modelstats.formatting import print_output
from modelstats.gdx import GdxError
from modelstats.rdata_io import write_rds
from modelstats.run_status import (
    EXPLAIN_MODELSTAT,
    RunStatus,
    StatusTable,
    _as_numeric,
    _directory_rows,
    _is_true_gt0,
    _make_unique,
    _newest_first,
    _paste,
    _which_max,
)
from modelstats.slurm import RWarning
from modelstats.tables import to_dataframe

DATA = Path(__file__).resolve().parent / "data"
GDX = Path(__file__).resolve().parent / "gdx_matrix"
NOW = DEFAULT_FROZEN.timestamp()  # 1790762400
BERLIN = zoneinfo.ZoneInfo("Europe/Berlin")
REMIND_CONFIG_RUNTYPE = "nash debug"  # data/config.Rdata: optimization nash, cm_nash_mode 1
REMIND_CONFIG_TITLE = "testOneRegi"
MAGPIE_ID = "178978767733374"  # data/runstatistics_magpie.rda


# --------------------------------------------------------------------------- builders


def write(path: Path, text: str) -> Path:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(text, encoding="utf-8")
    return path


def touch(path: Path, mtime: float | None = None) -> Path:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.touch()
    if mtime is not None:
        os.utime(path, (mtime, mtime))
    return path


def remind_yaml(**fields: Any) -> str:
    """A REMIND-style config as YAML (``load_config`` reads any ``yml`` name; ``colRunType`` needs ``model_name``)."""
    gms = {"optimization": "nash", **fields.pop("gms", {})}
    cfg = {"title": "syn", "model_name": "REMIND", "gms": gms, **fields}
    return yaml.safe_dump(cfg, sort_keys=False)


class Run:
    """One run directory under ``root`` with the files a scenario needs."""

    def __init__(self, root: Path, name: str = "run") -> None:
        self.dir = root / name
        self.dir.mkdir(parents=True, exist_ok=True)

    @property
    def path(self) -> str:
        return str(self.dir)

    def remind_config(self) -> Run:
        shutil.copy(DATA / "config.Rdata", self.dir / "config.Rdata")
        return self

    def yaml_config(self, **fields: Any) -> Run:
        write(self.dir / "config.yml", remind_yaml(**fields))
        return self

    def magpie_config(self) -> Run:
        shutil.copy(DATA / "config.yml.txt", self.dir / "config.yml")
        return self

    def stats(self, variant: str = "runstatistics") -> Run:
        shutil.copy(DATA / f"{variant}.rda", self.dir / "runstatistics.rda")
        return self

    def gdx(self, matrix_name: str, as_name: str = "fulldata.gdx", mtime: float | None = None) -> Run:
        target = self.dir / as_name
        shutil.copy(GDX / matrix_name, target)
        if mtime is not None:
            os.utime(target, (mtime, mtime))
        return self

    def full_log(self, *lines: str, loops: str | None = "35", status: str | None = "Normal completion") -> Run:
        body = ["--- GAMS run"]
        if loops is not None:
            body.append(f"   LOOPS = {loops}")
        body.extend(lines)
        if status is not None:
            body.append(f"*** Status: {status}")
        write(self.dir / "full.log", "\n".join(body) + "\n")
        return self

    def log_txt(self, *lines: str) -> Run:
        write(self.dir / "log.txt", "\n".join(lines) + "\n")
        return self

    def file(self, name: str, text: str = "", mtime: float | None = None) -> Run:
        write(self.dir / name, text)
        if mtime is not None:
            os.utime(self.dir / name, (mtime, mtime))
        return self


def slurm_case(root: Path, *lines: str) -> Path:
    """A fresh FakeEffects squeue case directory: ``squeue_all.txt`` with the given six-field lines."""
    k = 0
    while (case := root / f"slurm-case-{k}").exists():
        k += 1
    case.mkdir()
    (case / "squeue_all.txt").write_text("".join(line + "\n" for line in lines), encoding="utf-8")
    return case


def running_job(run: Run, user: str = "pascalfu", elapsed: str = "1:23:45", state: str = "RUNNING") -> str:
    return f"{user} {os.path.realpath(run.path)} {run.dir.name} {elapsed} {state} standby"


def status_of(run: Run | str, effects: FakeEffects, **kwargs: Any) -> RunStatus:
    table = get_run_status(run.path if isinstance(run, Run) else run, effects=effects, **kwargs)
    rows = table.rows()
    assert len(rows) == 1, table
    return rows[0]


def local() -> FakeEffects:
    return FakeEffects(on_cluster=False)


def cluster(case_dir: Path | None = None, **kwargs: Any) -> FakeEffects:
    return FakeEffects(on_cluster=True, slurm_case_dir=case_dir, **kwargs)


# --------------------------------------------------------------------------- StatusTable


def test_status_table_fills_na_for_new_rows_and_columns() -> None:
    table = StatusTable()
    table["a", "x"] = "1"
    table["a", "y"] = None
    table["b", "x"] = "2"
    table["b", "z"] = 3
    assert table.columns == ["x", "y", "z"]
    assert table.rownames == ["a", "b"]
    assert table["a", "z"] is None  # created after row a: NA
    assert table["b", "y"] is None  # row b created after column y: NA
    assert table["nope", "x"] is None  # out["nope", "x"] is NA_character_
    with pytest.raises(RParityError, match="undefined columns selected"):
        table["a", "nope"]
    assert table.to_json_rows() == [
        {"_row": "a", "x": "1", "y": None, "z": None},
        {"_row": "b", "x": "2", "y": None, "z": 3},
    ]
    assert table.column("x") == ["1", "2"] and table.column("nope") is None
    assert len(table) == 2 and "a" in table and "c" not in table


def test_status_table_overwrites_only_assigned_cells_and_keeps_order() -> None:
    table = StatusTable()
    table["run", "RunType"] = "nash"
    table["run", "Mif"] = "yes"
    table["run", "RunType"] = "NA"  # a later directory with the same basename (BUG-022 / D-13)
    assert table.to_json_rows() == [{"_row": "run", "RunType": "NA", "Mif": "yes"}]


def test_status_table_rows_are_mappings_with_path_and_rowname() -> None:
    table = StatusTable()
    table.register_path("r", "/some/where/r")
    table["r", "Runtime"] = 8304.0
    (row,) = table.rows()
    assert isinstance(row, RunStatus)
    assert row.rowname == "r" and row.path == "/some/where/r"
    assert row["Runtime"] == 8304.0 and "Runtime" in row and row.get("Conv") is None and list(row) == ["Runtime"]
    with pytest.raises(KeyError):
        row["Conv"]
    assert table.to_json_rows() == [{"_row": "r", "Runtime": 8304}]  # integral floats serialise as ints
    assert list(table) == [row]
    frame = to_dataframe(table)
    assert list(frame.index) == ["r"] and list(frame.columns) == ["Runtime"]


# --------------------------------------------------------------------------- R helpers


def test_paste_keeps_the_separator_for_a_null_argument() -> None:
    """R 4.6.1: paste("a", NULL) is "a ", paste0("mag-", "1", " ", NULL) is "mag-1 "; only NULLs give character(0)."""
    assert _paste(["a", None], sep=" ") == ["a "]
    assert _paste(["mag-", "1", " ", None]) == ["mag-1 "]
    assert _paste(["mag-", [], " ", "NA"]) == ["mag- NA"]
    assert _paste([None, []]) == []
    assert _paste(["x", ["1", "2"]]) == ["x1", "x2"]
    assert _paste(["n=", 100000.0, 2, True, None]) == ["n=1e+052TRUE"]


def test_make_unique_like_r() -> None:
    assert _make_unique(["NA", "NA", "d2", "d2"]) == ["NA", "NA.1", "d2", "d2.1"]
    assert _make_unique(["a", "a", "a.1"]) == ["a", "a.2", "a.1"]


def test_directory_rows_name_na_rows_like_data_frame_subsetting() -> None:
    """R: rownames(a[a[, "isdir"] == TRUE, ]) for d2, d1, missing, file, missing, d2 is d2 d1 NA NA.1 d2.1."""
    names = ["d2", "d1", "missing1", "f1", "missing2", "d2"]
    info: list[tuple[bool | None, float | None]] = [
        (True, 5.0),
        (True, 5.0),
        (None, None),
        (False, 1.0),
        (None, None),
        (True, 5.0),
    ]
    rows = _directory_rows(names, info)
    assert [name for name, _ in rows] == ["d2", "d1", "NA", "NA.1", "d2.1"]
    assert _newest_first(rows) == ["d2", "d1", "d2.1", "NA", "NA.1"]  # stable, NA last
    assert _directory_rows(["d1", "missing"], [(True, 1.0), (None, None)]) == [("d1", 1.0), ("NA", None)]
    assert [n for n, _ in _directory_rows(["NA", "missing"], [(True, 1.0), (None, None)])] == ["NA", "NA.1"]
    assert _newest_first([("old", 1.0), ("new", 2.0), ("tie", 2.0)]) == ["new", "tie", "old"]


def test_is_true_gt0_compares_strings_by_collation() -> None:
    assert _is_true_gt0("1") and _is_true_gt0("a") and not _is_true_gt0("") and not _is_true_gt0("0")
    assert _is_true_gt0(1) and _is_true_gt0(0.5) and _is_true_gt0(True)
    assert (
        not _is_true_gt0(0) and not _is_true_gt0(None) and not _is_true_gt0(float("nan")) and not _is_true_gt0([1, 2])
    )


def test_which_max_and_as_numeric() -> None:
    assert (
        _which_max([5.0, 5.0, 3.0]) == 0 and _which_max([float("nan"), 3.0]) == 1 and _which_max([float("nan")]) is None
    )
    assert _as_numeric("100") == 100.0 and _as_numeric(" 1e2 ") == 100.0 and _as_numeric("0x10") == 16.0
    assert _as_numeric("abc") != _as_numeric("abc") and _as_numeric(None) != _as_numeric(None)  # NA
    assert _as_numeric(True) == 1.0 and _as_numeric(7) == 7.0


# --------------------------------------------------------------------------- directory list


def test_newest_directory_first_files_and_missing_paths(tmp_path: Path) -> None:
    older = Run(tmp_path, "older")
    newer = Run(tmp_path, "newer")
    os.utime(older.dir, (NOW - 7200, NOW - 7200))
    os.utime(newer.dir, (NOW - 3600, NOW - 3600))
    afile = touch(tmp_path / "afile.txt")
    missing = tmp_path / "does-not-exist"
    with pytest.warns(RWarning) as record:
        table = get_run_status([older.path, str(afile), newer.path, str(missing)], effects=local())
    assert [w.message.text for w in record if isinstance(w.message, RWarning)] == [
        f'path[4]="{missing}": No such file or directory'
    ]
    # sort = "nf": directories newest first, a file dropped, a missing path visited as the directory "NA"
    assert table.rownames == ["newer", "older", "NA"]
    na_row = table.rows()[2]
    assert na_row.path == "NA" and na_row["RunStatus"] == "full.log missing" and na_row["RunType"] == "NA"
    assert na_row["jobInSLURM"] == "NA" and na_row["Runtime"] is None
    # any other sort keeps the input order and visits the file and the missing path as given
    with pytest.warns(RWarning):
        table = get_run_status([older.path, str(afile), newer.path, str(missing)], sort="none", effects=local())
    assert table.rownames == ["older", "afile.txt", "newer", "does-not-exist"]
    assert table["afile.txt", "RunStatus"] == "full.log missing"


def test_a_single_path_and_no_rows(tmp_path: Path) -> None:
    run = Run(tmp_path)
    assert get_run_status(run.path, effects=local()).rownames == ["run"]
    assert get_run_status(run.dir, effects=local()).rownames == ["run"]
    empty = get_run_status([], effects=local())
    assert empty.rownames == [] and empty.columns == [] and empty.to_json_rows() == [] and empty.column("Iter") is None


def test_duplicate_basenames_collapse_into_one_row(tmp_path: Path) -> None:
    first = Run(tmp_path / "A", "samebase").remind_config()
    second = Run(tmp_path / "B", "samebase")  # no config: RunType "NA" overwrites "nash debug"
    os.utime(first.dir, (NOW - 10, NOW - 10))
    os.utime(second.dir, (NOW - 20, NOW - 20))
    table = get_run_status([first.path, second.path], effects=local())
    assert table.rownames == ["samebase"]
    assert table["samebase", "RunType"] == "NA" and table.rows()[0].path == second.path


def test_default_user_and_explicit_user_reach_found_in_slurm(tmp_path: Path) -> None:
    run = Run(tmp_path)
    effects = cluster(slurm_case(tmp_path, running_job(run, user="alice")), user="alice")
    assert status_of(run, effects)["jobInSLURM"] == "standby"  # Sys.info() user alice: own job -> QOS
    assert status_of(run, effects, user="bob")["jobInSLURM"] == "alice"  # explicit other user: the job's user
    assert found_in_slurm(run.path, "alice", effects) == "standby"


# --------------------------------------------------------------------------- the columns, off-cluster


def test_remind_finished_run_off_cluster(tmp_path: Path) -> None:
    run = (
        Run(tmp_path)
        .remind_config()
        .stats()
        .full_log(loops="35")
        .log_txt("some output", "There were 3 warnings (use warnings() to see them)")
        .file(f"REMIND_generic_{REMIND_CONFIG_TITLE}.mif", "mif")
        .file(f"REMIND_generic_{REMIND_CONFIG_TITLE}_summation_errors.csv", "variable,value\na,1\nb,2\na,3\n")
        .file(f"REMIND_generic_{REMIND_CONFIG_TITLE}_range_errors.txt", "variable;value\nx;1\ny;2\n")
    )
    write_rds(
        run.dir / "projectSummations.rds",
        {"ScenarioMIP": {"missingVars": 2, "checkSummations": 32, "checkSummationsRegional": 0}},
    )
    table = get_run_status(run.path, effects=local())
    assert table.to_json_rows() == [
        {
            "_row": "run",
            "jobInSLURM": "NA",
            "RunType": REMIND_CONFIG_RUNTYPE,
            "modelstat": "2: Locally Optimal",  # no GDX: stats$modelstat 2 assigned into the character column
            "Mif": "sumErr",
            "Iter": "35/100",  # cfg$gms$cm_iteration_max, no full.gms to override it
            "RunStatus": "Normal completion",
            "Warnings": "3",
            "Conv": "NA",
            "Runtime": 8304,  # round(timeGAMSEnd - timeGAMSStart) = round(8304.4)
            "summationErrors": 2,
            "rangeErrors": 2,
            "fixErrors": 0,
            "missingProjVars": 2,
            "projSummationErrors": 32,
            "projSummationErrorsRegional": 0,
        }
    ]
    (row,) = table.rows()
    line = print_output(row, rowname=row.rowname, cols=["RunType", "Runtime"])
    assert line.startswith("run") and "nash debug" in line


def test_brief_mode_stops_after_run_status(tmp_path: Path) -> None:
    run = Run(tmp_path).remind_config().stats().full_log().log_txt("There were 3 warnings")
    (row,) = get_run_status(run.path, detailed=False, effects=local()).to_json_rows()
    assert list(row) == ["_row", "jobInSLURM", "RunType", "modelstat", "Mif", "Iter", "RunStatus"]


def test_mif_rules_remind(tmp_path: Path) -> None:
    run = Run(tmp_path).remind_config()
    assert status_of(run, local())["Mif"] == "no"
    run.file(f"REMIND_generic_{REMIND_CONFIG_TITLE}.mif")
    assert status_of(run, local())["Mif"] == "yes"
    run.file(f"REMIND_generic_{REMIND_CONFIG_TITLE}_summation_errors.csv", "variable\n")
    assert status_of(run, local())["Mif"] == "sumErr"
    assert status_of(Run(tmp_path, "noconfig"), local())["Mif"] == "NA"


def test_mif_rules_magpie_and_missing_mif_skips_sanity(tmp_path: Path) -> None:
    run = Run(tmp_path).magpie_config().stats("runstatistics_magpie").full_log(loops="y2100")
    row = status_of(run, local())
    assert row["RunType"] == "nlp_apr17" and row["Mif"] == "no" and row["Iter"] == "y2100"
    assert row["modelstat"] == "22NA13"  # paste0(as.character(stats$modelstat), collapse = "") of the magclass vector
    assert row["Runtime"] == 2260 and row["Warnings"] == "NA" and row["Conv"] == "NA"
    assert "summationErrors" not in row  # MAgPIE: the sanity block is skipped entirely
    run.file("validation.mif", "x" * 99999)
    assert status_of(run, local())["Mif"] == "no"
    run.file("validation.mif", "x" * 100000)
    assert status_of(run, local())["Mif"] == "yes"
    # without runstatistics.rda a MAgPIE run is treated as REMIND: REMIND_generic_<title>.mif decides
    nostats = Run(tmp_path, "nostats").magpie_config().full_log(loops="y2100")
    row = status_of(nostats, local())
    assert row["Mif"] == "no" and row["modelstat"] == "NA" and row["Runtime"] is None
    nostats.file("REMIND_generic_default.mif")
    assert status_of(nostats, local())["Mif"] == "yes"
    assert "summationErrors" not in status_of(Run(tmp_path, "mifless").remind_config().stats(), local())


def test_warnings_magpie_from_slurm_log(tmp_path: Path) -> None:
    run = Run(tmp_path).magpie_config().stats("runstatistics_magpie")
    assert status_of(run, local())["Warnings"] == "NA"
    run.file("slurm.log", "all fine\n")
    assert status_of(run, local())["Warnings"] == "0"
    run.file("slurm.log", "Warning message:\nsomething\n")
    assert status_of(run, local())["Warnings"] == "1"
    run.file("slurm.log", "Warning messages:\n1: first\n  explanation\n2: second\n")
    assert status_of(run, local())["Warnings"] == "2"


def test_warnings_remind_block_count(tmp_path: Path) -> None:
    run = Run(tmp_path).remind_config().stats().full_log()
    run.log_txt("Warning messages:", "1: first", "2: second", "done")
    assert status_of(run, local())["Warnings"] == "2"
    run.log_txt("There were 50 or more warnings (use warnings() to see the first 50)")
    assert status_of(run, local())["Warnings"] == "0"  # BUG-029: no digits captured, block count 0


def test_modelstat_from_gdx_and_explanations(tmp_path: Path) -> None:
    run = Run(tmp_path).remind_config().stats().gdx("full.gdx")
    assert status_of(run, local())["modelstat"] == "2: Locally Optimal"
    run.gdx("first_found_second.gdx")  # p80_modelstat(t) over four years, one sparse zero: 0 -> "."
    assert status_of(run, local(), detailed=False)["modelstat"] == "22.2"  # (no s80_bool: Conv would fail, as in R)
    run.gdx("missing.gdx")
    assert status_of(run, local(), detailed=False)["modelstat"] == "5: Locally Infes"
    assert EXPLAIN_MODELSTAT["13"] == "Error No Solution"
    # a vector stats$modelstat of a REMIND run fails silently inside try(): "NA" stays
    vec = Run(tmp_path, "vec").remind_config().stats("runstatistics_vec")
    shutil.copy(DATA / "runstatistics.rda", vec.dir / "unused.rda")
    # runstatistics_vec has model_name MAgPIE: as.character over the years
    assert status_of(vec, local())["modelstat"] == "22NA2132"


def test_stats_config_without_model_name_aborts_the_modelstat_fallback(tmp_path: Path) -> None:
    run = Run(tmp_path).stats("runstatistics_noname")  # no config file, no GDX: if (NULL == "MAgPIE") at line 91
    with pytest.raises(RParityError, match="^argument is of length zero$"):
        get_run_status(run.path, effects=local())
    # with a GDX the fallback is not reached off-cluster; on the cluster line 108 fails the same way
    run.gdx("full.gdx")
    assert status_of(run, local())["modelstat"] == "2: Locally Optimal"
    with pytest.raises(RParityError, match="^argument is of length zero$"):
        get_run_status(run.path, effects=cluster(slurm_case(tmp_path)))


def test_two_config_matches_fail_like_r(tmp_path: Path) -> None:
    run = Run(tmp_path).remind_config()
    shutil.copy(run.dir / "config.Rdata", run.dir / "config.Rdata.bak")
    with pytest.raises(RParityError, match="^invalid 'description' argument$"):
        get_run_status(run.path, effects=local())


def test_runtime_live_off_cluster_and_rounding(tmp_path: Path) -> None:
    run = Run(tmp_path).stats("runstatistics_noname").gdx("full.gdx")  # timePrepareStart, no GAMSEnd
    assert status_of(run, local())["Runtime"] == int(NOW - 1789784930)  # jobInSLURM "NA" is not "no"
    later = FakeEffects(on_cluster=False, now=dt.datetime.fromtimestamp(1789784930 + 2.5, tz=BERLIN))
    assert status_of(run, later)["Runtime"] == 2  # round half to even


def test_iter_and_run_status_from_full_log(tmp_path: Path) -> None:
    run = Run(tmp_path).remind_config().full_log(loops="12", status="Terminated by user(s) at 10:00")
    row = status_of(run, local())
    assert row["Iter"] == "12/100" and row["RunStatus"] == "Terminated by use"  # (s) removed, cut to 17 characters
    run.full_log(loops=None, status="Execution error(s)")
    row = status_of(run, local())
    assert row["Iter"] == "NA/100" and row["RunStatus"] == "Execution error"
    # two status lines: the vector assignment fails inside try(), "NA" stays and off-cluster means interrupted (BUG-036)
    run.full_log("*** Status: Normal completion", status="Normal completion")
    assert status_of(run, local())["RunStatus"] == "Run interrupted"
    run.full_log(status=None)
    assert status_of(run, local())["RunStatus"] == "Run interrupted"
    # cm_iteration_max from full.gms for nash runs with autoconverge: the last line wins, nothing found drops the suffix
    run.file("full.gms", "cm_iteration_max = 100;\n cm_iteration_max = 50;   \n")
    assert status_of(run, local())["Iter"] == "35/50"
    run.file("full.gms", "nothing here\n")
    assert status_of(run, local())["Iter"] == "35"


def test_wait_remind_lock_and_full_log_missing(tmp_path: Path) -> None:
    run = Run(tmp_path).remind_config()
    assert status_of(run, local())["RunStatus"] == "full.log missing"
    run.log_txt("prepare", "try to acquire model lock")
    assert status_of(run, local())["RunStatus"] == "Wait REMIND lock"
    run.log_txt("try to acquire model lock", "got it")
    assert status_of(run, local())["RunStatus"] == "full.log missing"


# --------------------------------------------------------------------------- Conv and calibration


def test_conv_converged_and_gdx_selection(tmp_path: Path) -> None:
    run = Run(tmp_path).remind_config().gdx("full.gdx")  # s80_bool 1, iteration 5
    assert status_of(run, local())["Conv"] == "converged"
    run.gdx("tie_b.gdx", "non_optimal.gdx", mtime=NOW - 10)  # same iteration: the first candidate (fulldata) wins
    row = status_of(run, local())
    assert row["Conv"] == "converged (had INFES)" and row["modelstat"] == "2: Locally Optimal"
    run.gdx("noiter.gdx", "non_optimal.gdx", mtime=NOW - 10)  # no o_iterationNumber: newest mtime wins
    os.utime(run.dir / "fulldata.gdx", (NOW - 3600, NOW - 3600))
    with pytest.raises(RParityError, match="^missing value where TRUE/FALSE needed$"):
        get_run_status(run.path, effects=local())  # s80_bool 0 && 100 == numeric(0) is NA (BUG-038 / D-23)
    assert status_of(run, local(), detailed=False)["modelstat"] == "5: Locally Infes"


def test_conv_digits_not_converged_and_errors(tmp_path: Path) -> None:
    run = Run(tmp_path).yaml_config(gms={"cm_iteration_max": 100}).gdx("sparse_zeros.gdx")  # s80_bool 0, iteration 3
    row = status_of(run, local())
    assert (
        row["RunType"] == "nash" and row["Conv"] == "000200004000"
    )  # p80_repy modelstat over the 12 regions in set order
    run.yaml_config(gms={"cm_iteration_max": "3"})  # as.numeric("3") == 3
    assert status_of(run, local())["Conv"] == "not_converged"
    run.yaml_config()  # no cm_iteration_max: TRUE && logical(0) is NA
    with pytest.raises(RParityError, match="^missing value where TRUE/FALSE needed$"):
        get_run_status(run.path, effects=local())
    run.yaml_config(gms={"cm_iteration_max": [3, 4]})
    with pytest.raises(RParityError, match="^'length = 2' in coercion to 'logical\\(1\\)'$"):
        get_run_status(run.path, effects=local())
    run.yaml_config(gms={"cm_iteration_max": 100}).gdx("missing.gdx")  # no s80_bool: if (numeric(0) == 1)
    with pytest.raises(RParityError, match="^argument is of length zero$"):
        get_run_status(run.path, effects=local())
    assert status_of(run, local(), detailed=False)["modelstat"] == "5: Locally Infes"
    run.gdx("not_a_gdx.gdx")
    with pytest.raises(GdxError):  # D-20: R aborts the process here
        get_run_status(run.path, effects=local())


def test_conv_is_na_without_nash_or_gdx(tmp_path: Path) -> None:
    run = Run(tmp_path).magpie_config().gdx("full.gdx")
    assert status_of(run, local())["Conv"] == "NA"
    assert status_of(Run(tmp_path, "nogdx").remind_config(), local())["Conv"] == "NA"


def test_calibration_suffix_and_clb_converged(tmp_path: Path) -> None:
    run = (
        Run(tmp_path)
        .yaml_config(gms={"CES_parameters": "calibrate", "cm_iteration_max": 100})
        .gdx("full.gdx")
        .full_log(loops="26")
        .log_txt("CES calibration iteration 3", "CES calibration iteration 10 done", "end")
    )
    row = status_of(run, local())
    assert row["RunType"] == "Calib_nash" and row["Iter"] == "26/100 Clb: 10" and row["Conv"] == "converged"
    for k in range(1, 11):
        touch(run.dir / f"fulldata_{k}.gdx")
    assert status_of(run, local())["Conv"] == "converged"  # exactly 10: not more than 10 (BUG-001 / D-09)
    touch(run.dir / "sub" / "fulldata_11.gdx")  # find is recursive
    assert status_of(run, local())["Conv"] == "Clb_converged"
    assert status_of(run, local(), detailed=False)["Iter"] == "26/100"  # brief: no Clb suffix
    run.log_txt("CES calibration iteration 0")
    assert status_of(run, local())["Iter"] == "26/100"
    run.log_txt("no calibration line")
    assert status_of(run, local())["Iter"] == "26/100"


# --------------------------------------------------------------------------- on the cluster


def test_interrupt_labels_without_a_job(tmp_path: Path) -> None:
    run = Run(tmp_path).remind_config().full_log(status=None)
    effects = cluster(slurm_case(tmp_path))
    assert status_of(run, effects)["RunStatus"] == "Run interrupted"  # no log.txt
    for text, label in [
        (
            "slurmstepd: error: *** JOB 1 ON cs-x CANCELLED AT 2026-09-19T01:00:00 DUE TO TIME LIMIT ***",
            "Timeout interrupt",
        ),
        ("slurmstepd: error: Detected 1 oom-kill event(s) in StepId=2.batch.", "Memory interrupt"),
        (
            "slurmstepd: error: *** JOB 3 ON cs-x CANCELLED AT 2026-09-19T01:00:00 DUE TO PREEMPTION ***",
            "Preempt interrupt",
        ),
        (
            "slurmstepd: error: *** JOB 4 ON cs-x CANCELLED AT 2026-09-19T01:00:00 DUE TO JOB REQUEUE ***",
            "Run requeued",
        ),
        ("slurmstepd: error: *** JOB 5 ON cs-x CANCELLED AT 2026-09-19T01:00:00 ***", "Run cancelled"),
        ("slurmstepd: error: execve(): something unexpected", "NA"),  # BUG-030
        ("no slurm error at all", "NA"),
    ]:
        run.log_txt("output", text)
        assert status_of(run, effects)["RunStatus"] == label, text
    # a pending job without log.txt: restarted
    (run.dir / "log.txt").unlink()
    pending = cluster(slurm_case(tmp_path, running_job(run, elapsed="0:00", state="PENDING")))
    row = status_of(run, pending)
    assert row["jobInSLURM"] == "standby pending" and row["RunStatus"] == "Run restarted"


def test_run_in_progress_conoptspy_and_live_runtime(tmp_path: Path) -> None:
    run = Run(tmp_path).remind_config().stats("runstatistics_noname").full_log(status=None).log_txt("running")
    effects = cluster(slurm_case(tmp_path, running_job(run)))
    with pytest.raises(RParityError, match="argument is of length zero"):  # noname stats: line 108 on the cluster
        get_run_status(run.path, effects=effects)
    run.stats()
    row = status_of(run, effects)
    assert row["jobInSLURM"] == "standby" and row["RunStatus"] == "Run in progress"
    assert row["runInAppResults"] == "no" and row["Runtime"] == 8304
    touch(run.dir / "225a" / "grid1" / "gmsgrid.log", NOW - 0.2 * 3600)
    assert status_of(run, effects)["RunStatus"] == "conoptspy >0.2h"  # gdxdelay defaults to 1
    run.gdx("full.gdx", mtime=NOW - 0.25 * 3600)
    assert status_of(run, effects)["RunStatus"] == "Run in progress"  # gdxdelay > 0.25 is FALSE at equality
    run.gdx("full.gdx", mtime=NOW - 0.3 * 3600)
    assert status_of(run, effects)["RunStatus"] == "conoptspy >0.2h"
    touch(run.dir / "225a" / "grid1" / "gmsgrid.log", NOW - 0.149 * 3600)
    assert status_of(run, effects)["RunStatus"] == "Run in progress"  # round(0.149 - 0.049, 1) = 0.1
    touch(run.dir / "225a" / "grid1" / "gmsgrid.log", NOW - 12.3 * 3600)
    assert status_of(run, effects)["RunStatus"] == "conoptspy >12h"


def test_coupled_magpie_step_recurses_into_the_magpie_run(tmp_path: Path) -> None:
    magpie = Run(tmp_path / "magpie" / "output", "foo-mag-3").full_log(loops="y2050", status=None)
    run = (
        Run(tmp_path, "foo-rem-3")
        .yaml_config(path_magpie=str(tmp_path / "magpie"), cfg_mag={"results_folder": "output/foo-mag-3"})
        .full_log(status=None)
        .log_txt("output", "Starting MAgPIE...", "   ")
    )
    effects = cluster(slurm_case(tmp_path, running_job(run)))
    assert status_of(run, effects)["RunStatus"] == "mag-3 y2050"
    assert magpie.dir.exists()
    # BUG-021: the unexpanded template is not a directory: the recursive call reports the "NA" row
    run.yaml_config(path_magpie=str(tmp_path / "magpie"), cfg_mag={"results_folder": "output/:title::date:"})
    with pytest.warns(RWarning):
        assert status_of(run, effects)["RunStatus"] == "mag-output/:title::date: NA"
    # without cfg$path_magpie file.path() is character(0): no row, paste0 keeps the trailing space
    run.yaml_config(cfg_mag={"results_folder": "output/foo-mag-3"})
    assert status_of(run, effects)["RunStatus"] == "mag-3 "
    run.log_txt("Starting MAgPIE...", "and more")
    assert status_of(run, effects)["RunStatus"] == "Run in progress"


def test_running_reporting(tmp_path: Path) -> None:
    run = Run(tmp_path).remind_config().full_log().log_txt("Starting output generation for run")
    effects = cluster(slurm_case(tmp_path, running_job(run)))
    assert status_of(run, effects)["RunStatus"] == "Running reporting"
    run.log_txt("Starting output generation for run", "Finished output generation for run")
    assert status_of(run, effects)["RunStatus"] == "Normal completion"
    run.log_txt("Starting output generation for run")
    assert status_of(run, cluster(slurm_case(tmp_path)))["RunStatus"] == "Normal completion"  # no job
    assert status_of(run, local())["RunStatus"] == "Running reporting"  # off-cluster "NA" != "no"


def test_abort_infes_from_abort_gdx(tmp_path: Path) -> None:
    run = Run(tmp_path).remind_config().full_log(status="Execution error(s)").gdx("full.gdx", "abort.gdx")
    assert status_of(run, local())["RunStatus"] == "Abort EUR 2*Infes"  # EUR == 2 == cm_abortOnConsecFail, REF 1
    run.gdx("missing.gdx", "abort.gdx")
    assert status_of(run, local())["RunStatus"] == "Execution error"
    run.gdx("sparse_zeros.gdx")  # the abort label only applies to Execution error
    run.full_log(status="Normal completion")
    assert status_of(run, local())["RunStatus"] == "Normal completion"


def test_run_magpie_phase_and_locks(tmp_path: Path) -> None:
    root = tmp_path / "coupled"
    run = Run(root / "output", "C_X-rem-2").remind_config().full_log(status=None)
    effects = cluster(slurm_case(tmp_path, running_job(run)))
    run.file("log-mag.txt", "### COUPLING ### Preparing MAgPIE\nStarting MAgPIE run\n")
    assert status_of(run, effects)["RunStatus"] == "Run MAgPIE "  # no magpie full.log: paste("Run MAgPIE", NULL)
    write(root / "magpie" / "output" / "C_X-mag-2" / "full.log", "   LOOPS = y2050\n")
    assert status_of(run, effects)["RunStatus"] == "Run MAgPIE y2050"
    run.file("log-mag.txt", "### COUPLING ### Preparing MAgPIE\nStart getReport(gdx)...\n")
    assert status_of(run, effects)["RunStatus"] == "Run MAgPIE report"
    run.file("log-mag.txt", "### COUPLING ### Preparing MAgPIE\ntry to acquire model lock\n")
    assert status_of(run, effects)["RunStatus"] == "Wait MAgPIE lock"
    # a stored MAgPIE output after the last preparation: both tac|grep -m 1 hits exist, lengths are equal
    run.file("log-mag.txt", "Preparing MAgPIE\nMAgPIE output was stored\nPreparing MAgPIE\nMAgPIE output was stored\n")
    assert status_of(run, effects)["RunStatus"] == "Run in progress"
    run.file("log-mag.txt", "### COUPLING ### Preparing MAgPIE\nStarting MAgPIE run\n")
    assert status_of(run, effects)["RunStatus"] == "Run MAgPIE y2050"
    assert status_of(run, cluster(slurm_case(tmp_path)))["RunStatus"] == "Run interrupted"  # no job: not checked
    # log.txt stands in for log-mag.txt only after Normal completion
    (run.dir / "log-mag.txt").unlink()
    run.log_txt("Preparing MAgPIE")
    assert status_of(run, effects)["RunStatus"] == "Run in progress"
    run.full_log()
    assert status_of(run, effects)["RunStatus"] == "Run MAgPIE y2050"


def test_run_in_app_results_magpie_archive(tmp_path: Path) -> None:
    run = Run(tmp_path).magpie_config().stats("runstatistics_magpie")
    archive = tmp_path / "archive"
    effects = cluster(slurm_case(tmp_path), env={"MAGPIE_RESULTS_ARCHIVE_PATH": str(archive)})
    assert status_of(run, effects)["runInAppResults"] == "no"
    touch(archive / f"{MAGPIE_ID}.rds", NOW - 1000)
    assert status_of(run, effects)["runInAppResults"] == "yes"  # no overview.rds: all() over nothing (BUG-010)
    touch(archive / "overview.rds", NOW - 1601)
    assert status_of(run, effects)["runInAppResults"] == "no"
    touch(archive / "overview.rds", NOW - 1599)
    assert status_of(run, effects)["runInAppResults"] == "yes"
    assert "runInAppResults" not in status_of(run, local())
    assert status_of(Run(tmp_path, "nostats"), effects)["runInAppResults"] == "no"


def test_runtime_na_on_the_cluster_without_a_job(tmp_path: Path) -> None:
    run = Run(tmp_path).remind_config().stats().full_log()
    assert status_of(run, cluster(slurm_case(tmp_path)))["Runtime"] == 8304  # GAMSEnd wins over the job state
    assert status_of(Run(tmp_path, "nostats").remind_config(), cluster(slurm_case(tmp_path)))["Runtime"] is None


def test_mixed_models_share_the_sanity_columns(tmp_path: Path) -> None:
    remind = (
        Run(tmp_path, "remind").remind_config().stats().full_log().file(f"REMIND_generic_{REMIND_CONFIG_TITLE}.mif")
    )
    magpie = Run(tmp_path, "magpie").magpie_config().stats("runstatistics_magpie").full_log(loops="y2100")
    os.utime(remind.dir, (NOW - 1, NOW - 1))
    os.utime(magpie.dir, (NOW - 2, NOW - 2))
    rows = get_run_status([magpie.path, remind.path], effects=local()).to_json_rows()
    assert [row["_row"] for row in rows] == ["remind", "magpie"]
    assert rows[0]["summationErrors"] == 0 and rows[0]["missingProjVars"] is None
    assert rows[1]["summationErrors"] is None and rows[1]["Iter"] == "y2100"
