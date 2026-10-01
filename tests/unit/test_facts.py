"""``modelstats.facts``: the reading layer of a run folder, fact by fact, on synthetic folders.

Every fact is checked against what ``R/getRunStatus.R`` reads at the cited lines: the composed paths
(``:40-51``), the configuration discovery and its two-match aborts (``:41, 69-74``), ``runstatistics.rda``
(``:76-79``), the GDX selection (``:53-64``) and the symbol reads, the text scans of ``full.gms``,
``full.log``, ``log.txt``, ``slurm.log`` and the MAgPIE log, the grid-log mtimes (``:179-181``), the mif and
sanity inputs (``:124-136, 326-360``), the runtime inputs (``:310-313``), the results archive rule
(``:112-113``, BUG-010) and the SLURM lookup (``:38``). The helpers at the top build run folders, R data
files and small GDX files for this module and for ``test_run_status.py``.
"""

from __future__ import annotations

import os
from collections import Counter
from collections.abc import Callable, Iterator, Mapping, Sequence
from contextlib import contextmanager
from datetime import datetime
from pathlib import Path, PurePosixPath
from typing import Any
from zoneinfo import ZoneInfo

import gams.transfer as gt
import gamspy_base
import numpy as np
import pandas as pd
import pytest
import rdata
from rdata.conversion.to_r import DEFAULT_CLASS_MAP, build_r_object
from rdata.parser import RObjectType

from modelstats import facts as facts_module
from modelstats import gdx as gdx_module
from modelstats.config import load_config
from modelstats.env import CannedSlurm, ClusterPaths, FakeEffects, OverlayPaths, ProductionEffects
from modelstats.facts import (
    ARGUMENT_OF_LENGTH_ZERO,
    CONDITION_LENGTH_ABOVE_ONE,
    INVALID_DESCRIPTION,
    NO_LINES_IN_INPUT,
    RunFacts,
    RunFiles,
    RunReadError,
    physical_path,
)
from modelstats.gdx import GdxFile
from modelstats.rdata_io import RTime, read_rds, write_rds
from modelstats.runstats import read_run_statistics

ZONE = ZoneInfo("Europe/Berlin")
NOW = datetime(2026, 9, 30, 12, 0, 0, tzinfo=ZONE)
NOW_EPOCH = NOW.timestamp()
ME = "me"
REGIONS = ("CAZ", "CHA", "EUR", "IND", "JPN", "LAM", "MEA", "NEU", "OAS", "REF", "SSA", "USA")
SOLVEINFO = ("modelstat", "solvestat", "resusd", "objval")
NOT_ROOT = pytest.mark.skipif(os.geteuid() == 0, reason="root ignores file modes")

# ---------------------------------------------------------------------------------------------
# Builders shared with test_run_status.py
# ---------------------------------------------------------------------------------------------


def effects(
    cwd: Path,
    *,
    on_cluster: bool = False,
    slurm: Mapping[str, str] | None = None,
    user: str = ME,
    now: datetime = NOW,
    cluster_paths: ClusterPaths | None = None,
    environ: Mapping[str, str] | None = None,
) -> FakeEffects:
    """A ``FakeEffects`` on the real filesystem (no overlay) with the frozen clock and a canned squeue."""
    return FakeEffects(
        now=now,
        user=user,
        on_cluster=on_cluster,
        slurm=CannedSlurm(slurm) if slurm is not None else None,
        cwd=cwd,
        cluster_paths=cluster_paths,
        environ=environ,
    )


def make_run(root: Path, name: str, files: Mapping[str, str | bytes] | None = None) -> Path:
    """A run folder with the given text or byte files (sub-directories created as needed)."""
    directory = root / name
    directory.mkdir(parents=True, exist_ok=True)
    for relative, content in (files or {}).items():
        path = directory / relative
        path.parent.mkdir(parents=True, exist_ok=True)
        if isinstance(content, bytes):
            path.write_bytes(content)
        else:
            path.write_text(content, encoding="utf-8")
    return directory


def _rtime_to_r(value: RTime, converter: Any) -> Any:
    """Write an ``RTime`` as a ``POSIXct`` double with the ``tzone`` attribute."""
    attributes = converter.convert_to_r_attributes(
        {"class": np.array(["POSIXct", "POSIXt"], dtype="U"), "tzone": np.array([value.tzone], dtype="U")}
    )
    epoch = np.array([np.nan if value.epoch is None else value.epoch])
    return build_r_object(RObjectType.REAL, value=epoch, is_object=True, attributes=attributes)


def write_stats(directory: Path, stats: Mapping[str, Any]) -> Path:
    """``runstatistics.rda`` holding ``stats`` (``RTime`` values become ``POSIXct``)."""
    path = directory / "runstatistics.rda"
    rdata.write_rda(path, {"stats": dict(stats)}, constructor_dict={**DEFAULT_CLASS_MAP, RTime: _rtime_to_r})
    return path


def remind_stats(
    *,
    gams_start: float | None = 1000.0,
    gams_end: float | None = 9304.5,
    prepare_start: float | None = 500.0,
    modelstat: Sequence[float] = (2.0,),
    run_id: float | None = 123456.0,
    model_name: str | None = "REMIND",
    with_config: bool = True,
) -> dict[str, Any]:
    """A REMIND-shaped ``stats`` list; ``None`` leaves the element out (R's ``NULL``)."""
    stats: dict[str, Any] = {"user": ME, "modelstat": np.array(list(modelstat))}
    if with_config:
        stats["config"] = {"title": "t"} if model_name is None else {"title": "t", "model_name": model_name}
    if run_id is not None:
        stats["id"] = np.array([run_id])
    if prepare_start is not None:
        stats["timePrepareStart"] = RTime(prepare_start)
    if gams_start is not None:
        stats["timeGAMSStart"] = RTime(gams_start)
    if gams_end is not None:
        stats["timeGAMSEnd"] = RTime(gams_end)
    return stats


def write_config(directory: Path, cfg: Mapping[str, Any], name: str = "config.Rdata") -> Path:
    """``config.Rdata`` holding the ``cfg`` list."""
    path = directory / name
    rdata.write_rda(path, {"cfg": dict(cfg)})
    return path


def remind_cfg(
    *,
    title: str = "t",
    optimization: str = "nash",
    iteration_max: Any = 100,
    autoconverge: Any = 1,
    ces: str = "load",
    path_magpie: str | None = None,
    results_folder: str | None = None,
) -> dict[str, Any]:
    """A REMIND ``cfg`` list with the switches ``colRunType`` and ``getRunStatus`` read."""
    gms: dict[str, Any] = {"optimization": optimization, "CES_parameters": ces}
    if iteration_max is not None:
        gms["cm_iteration_max"] = iteration_max
    if autoconverge is not None:
        gms["cm_nash_autoconverge"] = autoconverge
    cfg: dict[str, Any] = {"title": title, "model_name": "REMIND", "gms": gms}
    if path_magpie is not None:
        cfg["path_magpie"] = path_magpie
    if results_folder is not None:
        cfg["cfg_mag"] = {"results_folder": results_folder}
    return cfg


def write_gdx(
    path: Path,
    *,
    scalars: Mapping[str, float] | None = None,
    repy: Mapping[str, float] | None = None,
    track: Mapping[str, Mapping[int, float]] | None = None,
    magpie_modelstat: Sequence[float] | None = None,
    vector_threshold: Sequence[float] | None = None,
) -> Path:
    """A small GDX: scalars, ``p80_repy``, ``p80_trackConsecFail`` and ``p80_modelstat``.

    ``p80_repy`` is over ``(all_regi, solveinfo80)``, ``p80_trackConsecFail`` over ``(all_regi,
    iteration)`` and ``p80_modelstat`` over ``(t)``. ``repy`` maps a region to its modelstat (other
    regions get no record, so ``0`` after dense reconstruction); ``track`` maps a region to
    ``{iteration: value}``; ``magpie_modelstat`` writes one value per year; ``vector_threshold`` writes
    ``cm_abortOnConsecFail`` as a one-dimensional parameter over a ``dummy`` set, one record per value
    (R reads it as a numeric vector of that length).
    """
    m = gt.Container(system_directory=gamspy_base.directory)
    for name, value in (scalars or {}).items():
        gt.Parameter(m, name, records=value)
    if vector_threshold is not None:
        dummy = gt.Set(m, "dummy", records=[f"d{i}" for i in range(len(vector_threshold))])
        pairs = [(f"d{i}", value) for i, value in enumerate(vector_threshold)]
        gt.Parameter(m, "cm_abortOnConsecFail", [dummy], records=pd.DataFrame(pairs, columns=["d", "value"]))
    if repy is not None or track is not None:
        regi = gt.Set(m, "all_regi", records=list(REGIONS))
    if repy is not None:
        info = gt.Set(m, "solveinfo80", records=list(SOLVEINFO))
        rows = [(region, "modelstat", value) for region, value in repy.items()]
        rows += [(region, "solvestat", 1.0) for region in repy]
        gt.Parameter(m, "p80_repy", [regi, info], records=pd.DataFrame(rows, columns=["r", "s", "value"]))
    if track is not None:
        iteration = gt.Set(m, "iteration", records=[str(i) for i in range(1, 6)])
        rows = [(region, str(it), value) for region, its in track.items() for it, value in its.items()]
        frame = pd.DataFrame(rows, columns=["r", "i", "value"]) if rows else None
        gt.Parameter(m, "p80_trackConsecFail", [regi, iteration], records=frame)
    if magpie_modelstat is not None:
        years = gt.Set(m, "t", records=[f"y{2000 + 5 * i}" for i in range(len(magpie_modelstat))])
        yearly = [(f"y{2000 + 5 * i}", value) for i, value in enumerate(magpie_modelstat)]
        gt.Parameter(m, "p80_modelstat", [years], records=pd.DataFrame(yearly, columns=["t", "value"]))
    m.write(str(path))
    return path


def set_mtime(path: Path, epoch: float) -> None:
    os.utime(path, (epoch, epoch))


def facts_of(directory: Path, env: FakeEffects, user: str | None = None) -> RunFacts:
    return RunFacts(RunFiles(str(directory), env), env, user)


@contextmanager
def unreadable(*paths: Path) -> Iterator[None]:
    """``paths`` with mode ``000`` inside the block; the modes come back so ``tmp_path`` can be removed."""
    modes = [path.stat().st_mode for path in paths]
    for path in paths:
        path.chmod(0)
    try:
        yield
    finally:
        for path, mode in zip(paths, modes, strict=True):
            path.chmod(mode)


def library_reads(monkeypatch: pytest.MonkeyPatch) -> list[str]:
    """Every file the facts layer hands to a library, in order.

    GDX symbol tables, ``runstatistics.rda``, ``.rds`` files and the configuration: the readers are
    patched to record the path before reading as usual, and the returned list is the record.
    """
    opened: list[str] = []

    def recording[T](read: Callable[[Path], T]) -> Callable[[Path], T]:
        def recorded(path: Path) -> T:
            opened.append(str(path))
            return read(path)

        return recorded

    monkeypatch.setattr(gdx_module, "read_symbol_table", recording(gdx_module.read_symbol_table))
    monkeypatch.setattr(facts_module, "read_run_statistics", recording(read_run_statistics))
    monkeypatch.setattr(facts_module, "read_rds", recording(read_rds))
    monkeypatch.setattr(facts_module, "load_config", recording(load_config))
    return opened


def opened_streams(env: FakeEffects) -> Counter[str]:
    """How often each path was streamed through ``open_binary`` so far."""
    return Counter(call.args[0] for call in env.calls if call.name == "open_binary")


# ---------------------------------------------------------------------------------------------
# RunFiles (R/getRunStatus.R:40-51, 114, 126-129, 226-228, 319-351)
# ---------------------------------------------------------------------------------------------


def test_run_files_paste_the_r_paths(tmp_path: Path) -> None:
    env = effects(tmp_path)
    files = RunFiles("/p/x/output/run-rem-2", env)
    assert files.name == "run-rem-2"
    assert files.statistics == "/p/x/output/run-rem-2/runstatistics.rda"
    assert files.fulldata_gdx.endswith("/fulldata.gdx") and files.non_optimal_gdx.endswith("/non_optimal.gdx")
    assert files.full_gms.endswith("/full.gms") and files.full_log.endswith("/full.log")
    assert files.slurm_log.endswith("/slurm.log") and files.log_txt.endswith("/log.txt")
    assert files.abort_gdx.endswith("/abort.gdx") and files.validation_mif.endswith("/validation.mif")
    assert files.remind_mif("T") == "/p/x/output/run-rem-2/REMIND_generic_T.mif"
    assert files.remind_mif(None) == "/p/x/output/run-rem-2/REMIND_generic_.mif"  # paste0 of NULL
    assert files.summation_errors_csv("T").endswith("/REMIND_generic_T_summation_errors.csv")
    assert files.range_errors_txt("T").endswith("/REMIND_generic_T_range_errors.txt")
    assert files.fix_on_ref_csv.endswith("/log_fixOnRef.csv")
    assert files.project_summations_rds.endswith("/projectSummations.rds")


def test_magpie_log_falls_back_to_log_txt(tmp_path: Path) -> None:
    run = make_run(tmp_path, "run", {"log.txt": "x\n"})
    files = RunFiles(str(run), effects(tmp_path))
    assert files.magpie_log == f"{run}/log.txt"
    coupled = make_run(tmp_path, "coupled", {"log.txt": "x\n", "log-mag.txt": "y\n"})
    assert RunFiles(str(coupled), effects(tmp_path)).magpie_log == f"{coupled}/log-mag.txt"


def test_coupled_magpie_full_log_replaces_every_output_and_rem(tmp_path: Path) -> None:
    """``gsub("output", "magpie/output", ...)`` and ``gsub("-rem-", "-mag-", ...)`` over the whole path."""
    assert "output" not in str(tmp_path)  # the replacement would otherwise hit the temporary root too
    root = tmp_path / "model"
    remind = make_run(root / "remind" / "output", "C_x-rem-2", {"full.log": "a\n"})
    env = effects(tmp_path)
    # neither candidate exists: the ../magpie/output form is returned (R/getRunStatus.R:227)
    second = f"{root}/remind/../magpie/output/C_x-mag-2/full.log"
    assert RunFiles(str(remind), env).coupled_magpie_full_log == second
    make_run(root / "remind" / "magpie" / "output", "C_x-mag-2", {"full.log": "LOOPS = y2050\n"})
    first = f"{root}/remind/magpie/output/C_x-mag-2/full.log"
    assert RunFiles(str(remind), env).coupled_magpie_full_log == first


def test_config_matches_are_the_unanchored_regex_in_r_order(tmp_path: Path) -> None:
    run = make_run(
        tmp_path, "run", {"config.Rdata.bak": "", "config.Rdata": "", "zconfig.yml": "", "cfg.txt": ""}
    )
    files = RunFiles(str(run), effects(tmp_path))
    assert files.config_matches == ("config.Rdata", "config.Rdata.bak", "zconfig.yml")


def test_grid_logs_are_globbed(tmp_path: Path) -> None:
    run = make_run(
        tmp_path,
        "run",
        {"225a/grid1/gmsgrid.log": "", "225a/grid2/gmsgrid.log": "", "225a/x/gmsgrid.log": ""},
    )
    files = RunFiles(str(run), effects(tmp_path))
    assert files.grid_logs == (f"{run}/225a/grid1/gmsgrid.log", f"{run}/225a/grid2/gmsgrid.log")


# ---------------------------------------------------------------------------------------------
# physical_path
# ---------------------------------------------------------------------------------------------


def test_physical_path_is_identity_without_an_overlay(tmp_path: Path) -> None:
    assert physical_path(f"{tmp_path}/a", effects(tmp_path)) == Path(f"{tmp_path}/a")
    assert physical_path("relative/a", effects(tmp_path)) == tmp_path / "relative" / "a"  # the fake cwd
    assert physical_path("relative/a", ProductionEffects()) == Path("relative/a")  # the process cwd


def test_physical_path_maps_a_logical_path_through_the_overlay(tmp_path: Path) -> None:
    layer = tmp_path / "layer"
    (layer / "projects" / "run").mkdir(parents=True)
    (layer / "projects" / "run" / "full.log").write_text("x")
    env = FakeEffects(
        now=NOW,
        user=ME,
        on_cluster=True,
        paths=OverlayPaths(PurePosixPath("/p"), (layer,)),
        cwd="/p/projects",
    )
    assert physical_path("/p/projects/run/full.log", env) == layer / "projects" / "run" / "full.log"
    assert physical_path("run/full.log", env) == layer / "projects" / "run" / "full.log"
    assert physical_path("/p/projects/run/missing", env) == Path("/p/projects/run/missing")


# ---------------------------------------------------------------------------------------------
# Configuration (R/getRunStatus.R:41, 69-74)
# ---------------------------------------------------------------------------------------------


def test_config_and_run_type_without_a_file(tmp_path: Path) -> None:
    run = make_run(tmp_path, "run", {"full.log": ""})
    facts = facts_of(run, effects(tmp_path))
    assert facts.config is None and facts.run_type is None


def test_config_loads_once_and_run_type_follows(tmp_path: Path) -> None:
    run = make_run(tmp_path, "run")
    write_config(run, remind_cfg(ces="calibrate"))
    env = effects(tmp_path)
    facts = facts_of(run, env)
    config = facts.config
    assert config is not None and config.title == "t"
    assert facts.run_type == "Calib_nash"
    assert facts.config is config  # cached


def test_two_rdata_matches_abort_like_load_on_a_vector(tmp_path: Path) -> None:
    run = make_run(tmp_path, "run", {"config.Rdata.bak": ""})
    write_config(run, remind_cfg())
    with pytest.raises(RunReadError, match=INVALID_DESCRIPTION):
        _ = facts_of(run, effects(tmp_path)).config


def test_two_yml_matches_abort_in_col_run_type(tmp_path: Path) -> None:
    run = make_run(tmp_path, "run", {"config.yml": "title: t\n", "old_config.yml": "title: t\n"})
    with pytest.raises(RunReadError, match=CONDITION_LENGTH_ABOVE_ONE):
        _ = facts_of(run, effects(tmp_path)).config


def test_mixed_matches_abort_like_load(tmp_path: Path) -> None:
    run = make_run(tmp_path, "run", {"config.yml": "title: t\n"})
    write_config(run, remind_cfg())
    with pytest.raises(RunReadError, match=INVALID_DESCRIPTION):
        _ = facts_of(run, effects(tmp_path)).config


def test_unreadable_config_aborts(tmp_path: Path) -> None:
    run = make_run(tmp_path, "run", {"config.Rdata": "garbage"})
    with pytest.raises(RunReadError):
        _ = facts_of(run, effects(tmp_path)).config


def test_run_type_error_of_col_run_type_aborts(tmp_path: Path) -> None:
    run = make_run(tmp_path, "run")
    write_config(run, {"title": "t", "gms": {"optimization": "nash"}})  # no model_name: R/colRunType.R:20
    with pytest.raises(RunReadError, match=ARGUMENT_OF_LENGTH_ZERO):
        _ = facts_of(run, effects(tmp_path)).run_type


# ---------------------------------------------------------------------------------------------
# runstatistics.rda (R/getRunStatus.R:76-79)
# ---------------------------------------------------------------------------------------------


def test_statistics_absent_present_and_magpie(tmp_path: Path) -> None:
    run = make_run(tmp_path, "run")
    facts = facts_of(run, effects(tmp_path))
    assert facts.statistics is None and facts.is_magpie is False
    write_stats(run, remind_stats(model_name="MAgPIE"))
    facts = facts_of(run, effects(tmp_path))
    stats = facts.statistics
    assert stats is not None and stats.model_name == "MAgPIE" and facts.is_magpie


def test_unreadable_statistics_abort(tmp_path: Path) -> None:
    run = make_run(tmp_path, "run", {"runstatistics.rda": "garbage"})
    with pytest.raises(RunReadError):
        _ = facts_of(run, effects(tmp_path)).statistics


# ---------------------------------------------------------------------------------------------
# GDX selection and reads (R/getRunStatus.R:52-64, 84, 212-213, 280-288)
# ---------------------------------------------------------------------------------------------


def test_no_gdx_means_no_values(tmp_path: Path) -> None:
    facts = facts_of(make_run(tmp_path, "run"), effects(tmp_path))
    assert facts.gdx_candidates == () and facts.selected_gdx is None and facts.selected_gdx_mtime is None
    assert facts.modelstat_values is None and facts.iteration_number is None
    assert facts.convergence_flag is None and facts.solve_report is None
    assert facts.abort_threshold is None and facts.abort_track is None


def test_single_candidate_is_taken_unread(tmp_path: Path) -> None:
    run = make_run(tmp_path, "run", {"fulldata.gdx": "not a gdx"})
    facts = facts_of(run, effects(tmp_path))
    selected = facts.selected_gdx
    assert isinstance(selected, GdxFile) and selected.path == run / "fulldata.gdx"
    with pytest.raises(RunReadError, match="fulldata.gdx"):  # the read of :84 aborts in effect (D-20)
        _ = facts.modelstat_values


def test_two_candidates_pick_the_higher_iteration(tmp_path: Path) -> None:
    run = make_run(tmp_path, "run")
    write_gdx(run / "fulldata.gdx", scalars={"o_iterationNumber": 5, "o_modelstat": 2, "s80_bool": 1})
    write_gdx(run / "non_optimal.gdx", scalars={"o_iterationNumber": 7, "o_modelstat": 5, "s80_bool": 0})
    facts = facts_of(run, effects(tmp_path))
    selected = facts.selected_gdx
    assert selected is not None and selected.path.name == "non_optimal.gdx"
    assert facts.modelstat_values == [5.0] and facts.iteration_number == 7.0 and facts.convergence_flag == 0.0


def test_tie_goes_to_fulldata_and_a_missing_number_to_the_newest(tmp_path: Path) -> None:
    run = make_run(tmp_path, "run")
    write_gdx(run / "fulldata.gdx", scalars={"o_iterationNumber": 5, "o_modelstat": 2})
    write_gdx(run / "non_optimal.gdx", scalars={"o_iterationNumber": 5, "o_modelstat": 5})
    set_mtime(run / "fulldata.gdx", NOW_EPOCH - 5 * 3600)
    set_mtime(run / "non_optimal.gdx", NOW_EPOCH - 3600)
    facts = facts_of(run, effects(tmp_path))
    assert facts.selected_gdx is not None and facts.selected_gdx.path.name == "fulldata.gdx"
    assert facts.selected_gdx_mtime == pytest.approx(NOW_EPOCH - 5 * 3600)
    write_gdx(run / "non_optimal.gdx", scalars={"o_modelstat": 5})
    set_mtime(run / "non_optimal.gdx", NOW_EPOCH - 3600)
    facts = facts_of(run, effects(tmp_path))
    assert facts.selected_gdx is not None and facts.selected_gdx.path.name == "non_optimal.gdx"
    assert facts.iteration_number is None  # numeric(0): the BUG-038 input


def test_damaged_candidate_in_a_pair_aborts_the_selection(tmp_path: Path) -> None:
    run = make_run(tmp_path, "run", {"fulldata.gdx": "not a gdx"})
    write_gdx(run / "non_optimal.gdx", scalars={"o_iterationNumber": 2})
    with pytest.raises(RunReadError, match="fulldata.gdx"):
        _ = facts_of(run, effects(tmp_path)).selected_gdx


def test_solve_report_and_abort_symbols(tmp_path: Path) -> None:
    run = make_run(tmp_path, "run")
    write_gdx(run / "fulldata.gdx", scalars={"o_iterationNumber": 3}, repy={"EUR": 2, "CHA": 4})
    write_gdx(run / "abort.gdx", scalars={"cm_abortOnConsecFail": 2}, track={"EUR": {3: 2}, "REF": {3: 1}})
    facts = facts_of(run, effects(tmp_path))
    report = facts.solve_report
    assert report is not None and len(report) == len(REGIONS) * len(SOLVEINFO)
    assert facts.abort_threshold == [2.0]  # as.numeric() of the scalar: a one-element vector
    track = facts.abort_track
    assert track is not None and len(track) == len(REGIONS) * 5
    write_gdx(run / "abort.gdx", vector_threshold=[2, 2], track={"EUR": {3: 2}})
    assert facts_of(run, effects(tmp_path)).abort_threshold == [2.0, 2.0]  # a 1-d symbol is a vector
    write_gdx(run / "abort.gdx", track={"EUR": {3: 2}})
    assert facts_of(run, effects(tmp_path)).abort_threshold is None  # numeric(0) for the absent symbol


def test_abort_symbols_share_one_gdx_reader(tmp_path: Path, monkeypatch: pytest.MonkeyPatch) -> None:
    """``06`` 5.2: both symbols of ``R/getRunStatus.R:212-213`` come from one symbol-table read."""
    run = make_run(tmp_path, "run")
    write_gdx(run / "abort.gdx", scalars={"cm_abortOnConsecFail": 2}, track={"EUR": {3: 2}})
    library = library_reads(monkeypatch)
    facts = facts_of(run, effects(tmp_path))
    assert facts.abort_threshold == [2.0] and facts.abort_track is not None
    assert facts.abort_gdx is not None and facts.abort_gdx.path == run / "abort.gdx"
    assert library == [str(run / "abort.gdx")]


# ---------------------------------------------------------------------------------------------
# Text scans (R/getRunStatus.R:144-157, 162, 189, 204-205, 223-240, 258-273, 299)
# ---------------------------------------------------------------------------------------------


def test_text_facts_without_files(tmp_path: Path) -> None:
    facts = facts_of(make_run(tmp_path, "run"), effects(tmp_path))
    assert facts.iteration_max_lines == [] and facts.last_loops == [] and facts.status_lines == []
    assert facts.slurmstepd_errors == [] and facts.last_logtxt_line is None and facts.logtxt_tail == []
    assert facts.output_generation_started is False and facts.output_generation_finished is False
    assert facts.warning_summary == [] and facts.numbered_warning_count == 0
    assert facts.calibration_iteration == []
    assert facts.magpie_warning_block == [] and facts.slurm_log_has_warning_message is False
    assert facts.magpie_preparing is False and facts.magpie_output_stored is False
    assert facts.magpie_report_started is False and facts.magpie_log_tail == []
    assert facts.coupled_magpie_loops == []


def test_text_facts_read_the_r_pipelines(tmp_path: Path) -> None:
    run = make_run(
        tmp_path,
        "run",
        {
            "full.gms": "cm_iteration_max = 1;\n$setglobal x\ncm_iteration_max = 50;  \nother\n",
            "full.log": "--- LOOPS iteration = 1\n*** Status: Execution error(s)\n--- LOOPS iteration = 26\n"
            "*** Status: Normal completion\n",
            "log.txt": "slurmstepd: error: *** JOB 1 ON cs CANCELLED AT 2026 DUE TO TIME LIMIT ***\n"
            "There were 27 warnings (use warnings() to see them)\nCES calibration iteration: 7\n"
            "CES calibration iteration: 10\nStarting output generation for t\n\n   \nStarting MAgPIE...\n\n",
            "slurm.log": "Warning messages:\n1: first\n  more\n2: second\nWarning message:\nlone\n",
            "log-mag.txt": "Preparing MAgPIE\nMAgPIE output was stored\nPreparing MAgPIE\nStart getReport\n"
            "try to acquire model lock\n",
        },
    )
    facts = facts_of(run, effects(tmp_path))
    assert facts.iteration_max_lines == ["50"]
    assert facts.last_loops == ["26"]
    assert facts.status_lines == ["Execution error", "Normal completion"]
    assert facts.slurmstepd_errors == [
        "slurmstepd: error: *** JOB 1 ON cs CANCELLED AT 2026 DUE TO TIME LIMIT ***"
    ]
    assert facts.last_logtxt_line == "Starting MAgPIE..."
    assert facts.logtxt_tail == [""]  # tail -1 of a file ending in a blank line
    assert facts.output_generation_started is True and facts.output_generation_finished is False
    assert facts.warning_summary == ["27"]
    assert facts.calibration_iteration == ["10"]
    assert facts.magpie_warning_block == ["2"] and facts.slurm_log_has_warning_message is True
    assert facts.magpie_preparing and facts.magpie_output_stored and facts.magpie_report_started
    assert facts.magpie_log_tail == ["try to acquire model lock"]


@NOT_ROOT
def test_unreadable_logs_scan_as_empty_like_a_failing_grep(tmp_path: Path) -> None:
    """``system(..., intern = TRUE)`` is ``character(0)`` for a grep, tac or tail that cannot read its file.

    ``R/getRunStatus.R:144, 154, 157, 162, 204-205, 240, 267-273, 299`` all take their no-match path;
    only the ``awk`` result of ``:189`` is compared outside ``try()`` (``:190``) and aborts.
    """
    run = make_run(
        tmp_path,
        "run",
        {
            "full.gms": "cm_iteration_max = 50;\n",
            "full.log": "--- LOOPS iteration = 26\n*** Status: Normal completion\n",
            "log.txt": "slurmstepd: error: CANCELLED DUE TO TIME LIMIT\nThere were 27 warnings\n"
            "Warning messages:\n1: one\nCES calibration iteration: 7\nStarting output generation for t\n",
        },
    )
    facts = facts_of(run, effects(tmp_path))
    with unreadable(run / "full.gms", run / "full.log", run / "log.txt"):
        assert facts.iteration_max_lines == [] and facts.last_loops == [] and facts.status_lines == []
        assert facts.slurmstepd_errors == [] and facts.logtxt_tail == []
        assert facts.output_generation_started is False and facts.warning_summary == []
        assert facts.numbered_warning_count == 0 and facts.calibration_iteration == []
        with pytest.raises(RunReadError, match=ARGUMENT_OF_LENGTH_ZERO):
            _ = facts.last_logtxt_line


def test_the_facts_of_one_log_share_one_stream(tmp_path: Path) -> None:
    """``06`` 5.2: ``full.log`` is streamed once for its two facts, ``log.txt`` once for all of its facts."""
    run = make_run(
        tmp_path,
        "run",
        {
            "full.log": "--- LOOPS iteration = 26\n*** Status: Normal completion\n",
            "log.txt": "Starting output generation for t\nThere were 27 warnings\n",
        },
    )
    env = effects(tmp_path)
    facts = facts_of(run, env)
    assert facts.last_loops == ["26"] and facts.status_lines == ["Normal completion"]
    assert facts.slurmstepd_errors == [] and facts.last_logtxt_line == "There were 27 warnings"
    assert facts.logtxt_tail == ["There were 27 warnings"] and facts.warning_summary == ["27"]
    assert facts.output_generation_started is True and facts.output_generation_finished is False
    assert facts.numbered_warning_count == 0 and facts.calibration_iteration == []
    assert facts.magpie_log_tail == ["There were 27 warnings"]  # logmagtxt is log.txt: the same snapshot
    assert facts.magpie_preparing is False
    assert opened_streams(env) == {str(run / "full.log"): 1, str(run / "log.txt"): 1}


def test_numbered_warning_count_counts_the_blocks(tmp_path: Path) -> None:
    """The block pattern pairs lines greedily: ``1: a`` takes ``2: b`` as its explanation line.

    ``([0-9]+:(.*NL)?.*NL)*`` (PCRE and ``re`` alike) then finds the explanation line of ``2: b`` where a
    number should be and ends the block before ``3: c``.
    """
    run = make_run(tmp_path, "run", {"log.txt": "x\nWarning messages:\n1: a\n2: b\n  detail\n3: c\nend\n"})
    assert facts_of(run, effects(tmp_path)).numbered_warning_count == 2
    plain = make_run(tmp_path, "plain", {"log.txt": "Warning messages:\n1: a\n2: b\n3: c\n"})
    assert facts_of(plain, effects(tmp_path)).numbered_warning_count == 3


def test_coupled_magpie_loops_read_the_sibling_full_log(tmp_path: Path) -> None:
    root = tmp_path / "model"
    remind = make_run(root / "remind" / "output", "C_x-rem-2", {"full.log": "a\n"})
    make_run(root / "remind" / "magpie" / "output", "C_x-mag-2", {"full.log": "--- LOOPS = y2050\n"})
    assert facts_of(remind, effects(tmp_path)).coupled_magpie_loops == ["y2050"]


# ---------------------------------------------------------------------------------------------
# Grid logs, clock (R/getRunStatus.R:179-183, 313)
# ---------------------------------------------------------------------------------------------


def test_grid_log_newest_mtime_and_hours_since(tmp_path: Path) -> None:
    run = make_run(tmp_path, "run", {"225a/grid1/gmsgrid.log": "", "225a/grid2/gmsgrid.log": ""})
    set_mtime(run / "225a" / "grid1" / "gmsgrid.log", NOW_EPOCH - 7200)
    set_mtime(run / "225a" / "grid2" / "gmsgrid.log", NOW_EPOCH - 720)
    facts = facts_of(run, effects(tmp_path))
    assert facts.now == NOW_EPOCH
    assert facts.grid_log_newest_mtime == pytest.approx(NOW_EPOCH - 720)
    assert facts.hours_since(NOW_EPOCH - 720) == pytest.approx(0.2)
    assert facts_of(make_run(tmp_path, "bare"), effects(tmp_path)).grid_log_newest_mtime is None


# ---------------------------------------------------------------------------------------------
# Mif and sanity inputs (R/getRunStatus.R:124-136, 326-360)
# ---------------------------------------------------------------------------------------------


def test_mif_inputs(tmp_path: Path) -> None:
    run = make_run(tmp_path, "run", {"validation.mif": "x" * 10, "REMIND_generic_t.mif": "m"})
    facts = facts_of(run, effects(tmp_path))
    assert facts.validation_mif_size == 10
    assert (
        facts.remind_mif_exists("t")
        and not facts.remind_mif_exists("u")
        and not facts.remind_mif_exists(None)
    )
    assert facts.summation_errors_exist("t") is False
    assert facts_of(make_run(tmp_path, "bare"), effects(tmp_path)).validation_mif_size is None


def test_sanity_counts_read_the_csv_files(tmp_path: Path) -> None:
    run = make_run(
        tmp_path,
        "run",
        {
            "REMIND_generic_t_summation_errors.csv": "model,variable,value\na,X,1\na,Y,2\na,X,3\n",
            "REMIND_generic_t_range_errors.txt": "variable;min;max\nA;0;1\nB;0;1\n",
            "log_fixOnRef.csv": "variable,region\nV1,EUR\nV1,CHA\nV2,EUR\nV3,EUR\n",
        },
    )
    facts = facts_of(run, effects(tmp_path))
    assert facts.summation_errors_exist("t") and facts.summation_error_count("t") == 2
    assert facts.range_errors_exist("t") and facts.range_error_count("t") == 2
    assert facts.fix_errors_exist and facts.fix_error_count == 3


def test_empty_csv_aborts_like_read_csv2(tmp_path: Path) -> None:
    run = make_run(tmp_path, "run", {"log_fixOnRef.csv": ""})
    with pytest.raises(RunReadError, match=NO_LINES_IN_INPUT):
        _ = facts_of(run, effects(tmp_path)).fix_error_count


def test_project_summations_rds(tmp_path: Path) -> None:
    run = make_run(tmp_path, "run")
    facts = facts_of(run, effects(tmp_path))
    assert facts.project_summations is None
    write_rds(
        run / "projectSummations.rds",
        {"ScenarioMIP": {"missingVars": 2, "checkSummations": 32, "checkSummationsRegional": 0}},
    )
    facts = facts_of(run, effects(tmp_path))
    project = facts.project_summations
    assert project is not None and "ScenarioMIP" in project


def test_iteration_file_counts_are_recursive_like_find(tmp_path: Path) -> None:
    run = make_run(tmp_path, "run", {f"fulldata_{i}.gdx": "" for i in range(1, 11)})
    make_run(run, "sub", {"fulldata_11.gdx": "", "input_1.gdx": ""})
    facts = facts_of(run, effects(tmp_path))
    assert facts.fulldata_iteration_count == 11 and facts.input_iteration_count == 1
    assert facts_of(tmp_path / "missing", effects(tmp_path)).fulldata_iteration_count == 0


# ---------------------------------------------------------------------------------------------
# Runtime inputs (R/getRunStatus.R:310-313)
# ---------------------------------------------------------------------------------------------


def test_runtime_inputs(tmp_path: Path) -> None:
    run = make_run(tmp_path, "run")
    facts = facts_of(run, effects(tmp_path))
    assert facts.gams_wall_seconds is None and facts.seconds_since_prepare_start is None
    write_stats(run, remind_stats(gams_start=1000.0, gams_end=9304.5, prepare_start=NOW_EPOCH - 42.4))
    facts = facts_of(run, effects(tmp_path))
    assert facts.gams_wall_seconds == 8304.5
    assert facts.seconds_since_prepare_start == pytest.approx(42.4)
    write_stats(run, remind_stats(gams_start=None, gams_end=None, prepare_start=None))
    facts = facts_of(run, effects(tmp_path))
    assert facts.gams_wall_seconds is None and facts.seconds_since_prepare_start is None


# ---------------------------------------------------------------------------------------------
# Results archive (R/getRunStatus.R:112-113, BUG-010)
# ---------------------------------------------------------------------------------------------


def test_archived_result_rule(tmp_path: Path) -> None:
    archive = tmp_path / "archive"
    archive.mkdir()
    run = make_run(tmp_path, "run")
    write_stats(run, remind_stats(run_id=42.0))
    facts = facts_of(run, effects(tmp_path))
    prefix = f"{archive}/"
    assert facts.archived_result_is_current(prefix) is False  # no <id>.rds
    (archive / "42.rds").write_text("")
    set_mtime(archive / "42.rds", NOW_EPOCH - 1000)
    assert facts.archived_result_is_current(prefix) is True  # vacuous without overview.rds (BUG-010)
    (archive / "overview.rds").write_text("")
    set_mtime(archive / "overview.rds", NOW_EPOCH - 1500)  # 500 s older: still within the 600 s grace
    assert facts.archived_result_is_current(prefix) is True
    set_mtime(archive / "overview.rds", NOW_EPOCH - 1700)  # 700 s older than the result
    assert facts.archived_result_is_current(prefix) is False


def test_archived_result_with_a_null_id_pastes_nothing(tmp_path: Path) -> None:
    archive = tmp_path / "archive"
    archive.mkdir()
    (archive / ".rds").write_text("")
    run = make_run(tmp_path, "run")
    write_stats(run, remind_stats(run_id=None))
    assert facts_of(run, effects(tmp_path)).archived_result_is_current(f"{archive}/") is True


# ---------------------------------------------------------------------------------------------
# SLURM (R/getRunStatus.R:38)
# ---------------------------------------------------------------------------------------------


def test_slurm_lookup_only_on_the_cluster(tmp_path: Path) -> None:
    run = make_run(tmp_path, "run")
    assert facts_of(run, effects(tmp_path)).slurm is None
    line = f"{ME} {run} run 1:23:45 RUNNING standby\n"
    on_cluster = effects(tmp_path, on_cluster=True, slurm={"squeue_all.txt": line})
    lookup = facts_of(run, on_cluster).slurm
    assert lookup is not None and lookup.text == "standby" and lookup.job is not None
    assert len(on_cluster.runs) == 1  # one squeue call, cached
    assert facts_of(run, effects(tmp_path, on_cluster=True, slurm={})).slurm is not None


def test_slurm_lookup_for_another_user(tmp_path: Path) -> None:
    run = make_run(tmp_path, "run")
    line = f"alice {run} run 0:45 RUNNING standby\n"
    lookup = facts_of(run, effects(tmp_path, on_cluster=True, slurm={"squeue_all.txt": line}), "bob").slurm
    assert lookup is not None and lookup.text == "alice startup"


# ---------------------------------------------------------------------------------------------
# Laziness
# ---------------------------------------------------------------------------------------------


def test_nothing_is_read_before_a_fact_is_asked(tmp_path: Path) -> None:
    run = make_run(tmp_path, "run", {"full.log": "*** Status: Normal completion\n"})
    env = effects(tmp_path)
    facts = facts_of(run, env)
    assert env.calls == []
    assert facts.status_lines == ["Normal completion"]
    assert facts.status_lines == ["Normal completion"]
    assert [c.name for c in env.calls] == ["exists", "open_binary"]  # read once
