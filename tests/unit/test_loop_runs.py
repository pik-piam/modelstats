"""``modelstats.loop_runs``: every rule of ``loopRuns()`` (``R/loopRuns.R``) on synthetic run folders.

The goldens (``tests/golden/test_looprun.py``) pin the bytes on the fixture and synthetic cases; these
tests name the R lines they reproduce and cover the mechanics and the branches no golden reaches: crayon
disabled, a file among the directories, a repeated path (``make.unique`` row names), two missing paths,
the ``<i>/NA`` probe of line 58, a config-less folder with a running job, the ``exit`` sentinel, the
default output streams and the user default. The stderr side of line 57 (base R's ``try()`` prints the
caught condition and the deferred warnings) is pinned here to the lines of the ``rs`` goldens
``migration/goldens/rs/synthetic-remind.err:5-10`` and ``synthetic-bug005.err:5-6`` (quoted verbatim, the
golden tier reads the files), and :func:`try_error_message` is exercised on every shape of ``try()``'s and
``PrintWarnings``'s output (checked in R 4.6.1).
"""

from __future__ import annotations

import io
import re
from collections.abc import Sequence
from pathlib import Path

import pytest

from modelstats.colors import Crayon, colour_legend
from modelstats.env import FakeEffects
from modelstats.errors import DeferredWarning, RunReadError
from modelstats.formatting import CellValue
from modelstats.loop_runs import (
    COLUMN_SEPARATOR,
    EXIT,
    FOLDER_WIDTH_MAX,
    FOLDER_WIDTH_MIN,
    NO_RUNS_FOUND,
    SKIPPED,
    SKIPPED_BECAUSE_OF_ERROR,
    SUBSCRIPT_OUT_OF_BOUNDS,
    DirectoryRow,
    LoopRunsError,
    deferred_warnings_message,
    directory_rows,
    display_cells,
    folder_width,
    format_row,
    has_config_or_log,
    loop_runs,
    make_unique,
    ordered_names,
    table_layout,
    try_error_message,
)
from modelstats.run_status import NA, RunStatus, get_run_status
from tests.unit.test_facts import ME, NOW_EPOCH, effects, make_run, remind_stats, set_mtime, write_stats
from tests.unit.test_run_status import STATUS_OK, magpie_run, remind_run, running_job

ESC = "\x1b["
GREEN = f"{ESC}32m"
YELLOW = f"{ESC}33m"
CLOSE = f"{ESC}39m"
UNDERLINE = f"{ESC}4m"
UNDERLINE_CLOSE = f"{ESC}24m"
ANSI = re.compile(r"\x1b\[[0-9;]*m")
CONVERGED_GDX = {"o_iterationNumber": 26, "o_modelstat": 2, "s80_bool": 1}
# The headers R prints for a 15-wide folder column (goldens errors--oncluster and errors--local).
ONCLUSTER_HEADER = (
    "Folder           Runtime      inSlurm   RunType      RunStatus          Warnings   Iter              "
    "Conv                   modelstat              Mif     AppResults"
)
LOCAL_HEADER = (
    "Folder           Runtime      RunType      RunStatus          Warnings   Iter              "
    "Conv                   modelstat            Mif   "
)
# What R's try() printed for the synthetic runs config-bak (migration/goldens/rs/synthetic-remind.err:5),
# gdx-noiter (synthetic-remind.err:6-10) and noconfig-badstats (synthetic-bug005.err:5-6).
CONFIG_BAK_STDERR = "Error in gzfile(file) : invalid 'description' argument\n"
GDX_NOITER_STDERR = (
    "Error in if (s80_bool == 0 && as.numeric(cm_iteration_max) == iter_no) { : \n"
    "  missing value where TRUE/FALSE needed\n"
    "In addition: Warning message:\n"
    'In readGDX(gdx = latest_gdx, "o_iterationNumber", format = "simplest") :\n'
    "  Error : User specified to read symbol o_iterationNumber, but it does not exist in the source file\n"
)
BUG005_STDERR = (
    'Error in if (runstatistics$stats[["config"]][["model_name"]] == "MAgPIE") { : \n'
    "  argument is of length zero\n"
)


def run_table(dirs: Sequence[str] | str, env: FakeEffects, **kwargs: object) -> tuple[str | None, str, str]:
    """``loop_runs`` into buffers: the return value, what was written to ``out`` and to ``err``."""
    out = io.StringIO()
    err = io.StringIO()
    result = loop_runs(dirs, effects=env, out=out, err=err, **kwargs)  # type: ignore[arg-type]
    return result, out.getvalue(), err.getvalue()


def table(dirs: Sequence[str] | str, env: FakeEffects, **kwargs: object) -> tuple[str | None, str]:
    """``run_table`` for a call that must leave stderr empty (no ``getRunStatus`` error)."""
    result, out, err = run_table(dirs, env, **kwargs)
    assert err == "", err
    return result, out


def plain_rows(text: str) -> list[str]:
    """The lines after the header, escape sequences removed."""
    lines = ANSI.sub("", text).split("\n")
    start = next(i for i, line in enumerate(lines) if line.startswith("Folder")) + 1
    return [line for line in lines[start:] if line]


def cells_of(line: str) -> list[str]:
    return [cell for cell in re.split(r" {2,}", line) if cell]


# ---------------------------------------------------------------------------------------------
# Early returns, legend, header (R/loopRuns.R:22-55)
# ---------------------------------------------------------------------------------------------


def test_no_directories_return_no_runs_found_and_exit_returns_none(tmp_path: Path) -> None:
    env = effects(tmp_path)
    assert table([], env) == (NO_RUNS_FOUND, "")
    assert table([EXIT, str(tmp_path)], env) == (None, "")
    assert table(EXIT, env) == (None, "")
    assert loop_runs([]) == NO_RUNS_FOUND  # production effects: nothing is touched before the return


def test_legend_only_with_colors_and_header_underlined_regardless(tmp_path: Path) -> None:
    run = remind_run(tmp_path, "run")
    env = effects(tmp_path)
    _, coloured = table([str(run)], env)
    legend = colour_legend(Crayon())
    assert coloured.startswith(legend)
    assert coloured[len(legend) :].split("\n")[0] == f"{UNDERLINE}{LOCAL_HEADER}{UNDERLINE_CLOSE} "
    _, uncoloured = table([str(run)], env, colors=False)
    assert "# Color code" not in uncoloured
    assert uncoloured.startswith(f"{UNDERLINE}Folder")
    _, disabled = table([str(run)], env, crayon=Crayon.disabled())
    assert ESC not in disabled
    assert disabled.startswith(
        "# Color code: pending/startup, running, converged/finished, no mif, conopt stalled?, error.\n\n"
        f"{LOCAL_HEADER} \n"
    )


def test_header_on_the_cluster_has_the_inslurm_and_appresults_columns(tmp_path: Path) -> None:
    run = remind_run(tmp_path, "run")
    _, text = table([str(run)], effects(tmp_path, on_cluster=True), colors=False, crayon=Crayon.disabled())
    assert text.split("\n")[0] == f"{ONCLUSTER_HEADER} "


def test_table_layout_widths_are_the_title_widths_and_three() -> None:
    on_cluster = table_layout(20, True)
    assert on_cluster.titles[0] == "Folder" + " " * 14
    assert on_cluster.widths == (20, 11, 8, 11, 17, 9, 16, 21, 21, 6, 3)
    assert on_cluster.header == COLUMN_SEPARATOR.join(on_cluster.titles)
    assert table_layout(15, True).header == ONCLUSTER_HEADER
    local = table_layout(15, False)
    assert local.widths == (15, 11, 11, 17, 9, 16, 21, 19, 6)
    assert local.header == LOCAL_HEADER


def test_folder_width_bounds() -> None:
    assert folder_width([]) == FOLDER_WIDTH_MIN
    assert folder_width(["a" * 3, "b" * 30]) == 30
    assert folder_width(["a" * 80]) == FOLDER_WIDTH_MAX


def test_folder_column_follows_the_longest_basename_and_cuts_at_67(tmp_path: Path) -> None:
    long_name = "x" * 70
    run = remind_run(tmp_path, long_name)
    _, text = table([str(run)], effects(tmp_path), colors=False, crayon=Crayon.disabled())
    header, row = text.split("\n")[:2]
    assert header.startswith("Folder" + " " * 61 + COLUMN_SEPARATOR + "Runtime")
    assert row.startswith("x" * 67 + COLUMN_SEPARATOR)
    assert "x" * 68 not in row


# ---------------------------------------------------------------------------------------------
# file.info rows and their order (R/loopRuns.R:33-39)
# ---------------------------------------------------------------------------------------------


def test_make_unique_like_r() -> None:
    assert make_unique(["a", "a", "a.1", "a"]) == ["a", "a.2", "a.1", "a.3"]  # R 4.6.1
    assert make_unique(["NA", "NA"]) == ["NA", "NA.1"]
    assert make_unique([]) == []
    assert make_unique(["b", "a", "b"], sep="_") == ["b", "a", "b_1"]


def test_directory_rows_keep_directories_name_missing_paths_na_and_suffix_repeats(tmp_path: Path) -> None:
    a, b, c = (make_run(tmp_path, name) for name in "abc")
    a_file = tmp_path / "f.txt"
    a_file.write_text("")
    set_mtime(a, NOW_EPOCH - 30)
    set_mtime(b, NOW_EPOCH - 10)
    set_mtime(c, NOW_EPOCH - 20)
    paths = [str(a), str(a_file), str(tmp_path / "missing"), str(b), str(a), str(c), str(tmp_path / "gone")]
    rows = directory_rows(paths, effects(tmp_path))
    assert [row.name for row in rows] == [str(a), NA, str(b), f"{a}.1", str(c), "NA.1"]
    assert [row.mtime is None for row in rows] == [False, True, False, False, False, True]
    assert ordered_names(rows, sortbytime=False) == [row.name for row in rows]
    # newest first, the repeated path tied with its original in input order, the NA rows last
    assert ordered_names(rows, sortbytime=True) == [str(b), str(c), str(a), f"{a}.1", NA, "NA.1"]


def test_ordered_names_is_stable_for_equal_mtimes() -> None:
    rows = [DirectoryRow("x", 5.0), DirectoryRow("y", 5.0), DirectoryRow("z", 7.0)]
    assert ordered_names(rows, sortbytime=True) == ["z", "x", "y"]


def test_rows_are_printed_newest_first_or_in_input_order(tmp_path: Path) -> None:
    old = remind_run(tmp_path, "old")
    new = remind_run(tmp_path, "new")
    set_mtime(old, NOW_EPOCH - 100)
    set_mtime(new, NOW_EPOCH - 1)
    env = effects(tmp_path)
    _, sorted_text = table([str(old), str(new)], env, colors=False, crayon=Crayon.disabled())
    assert [cells_of(row)[0] for row in plain_rows(sorted_text)] == ["new", "old"]
    _, unsorted = table([str(old), str(new)], env, colors=False, sortbytime=False, crayon=Crayon.disabled())
    assert [cells_of(row)[0] for row in plain_rows(unsorted)] == ["old", "new"]


# ---------------------------------------------------------------------------------------------
# The skipped lines and the error lines (R/loopRuns.R:56-65)
# ---------------------------------------------------------------------------------------------


def test_missing_paths_are_looked_up_as_na_rows_and_skipped_on_the_cluster(tmp_path: Path) -> None:
    """A missing path is the row ``NA`` (``:33-39``); ``getRunStatus("NA")`` finds no job: ``NA skipped.``."""
    run = remind_run(tmp_path, "run")
    missing = [str(tmp_path / "missing"), str(tmp_path / "gone")]
    env = effects(tmp_path, on_cluster=True)
    _, text = table([missing[0], str(run), missing[1]], env, colors=False, crayon=Crayon.disabled())
    rows = plain_rows(text)
    assert rows[0].startswith("run  ") and rows[1:] == [f"NA {SKIPPED}", f"NA.1 {SKIPPED}"]
    _, unsorted = table(
        [missing[0], str(run), missing[1]], env, colors=False, sortbytime=False, crayon=Crayon.disabled()
    )
    rows = plain_rows(unsorted)
    assert rows[0] == f"NA {SKIPPED}" and rows[1].startswith("run  ") and rows[2] == f"NA.1 {SKIPPED}"


def test_off_the_cluster_nothing_is_ever_skipped(tmp_path: Path) -> None:
    """``jobInSLURM`` is the string ``"NA"`` off the cluster, never ``"no"``: line 58 cannot be true."""
    bare = make_run(tmp_path, "bare", {"full.log": STATUS_OK})
    env = effects(tmp_path)
    _, text = table([str(bare), str(tmp_path / "missing")], env, colors=False, crayon=Crayon.disabled())
    rows = [cells_of(row) for row in plain_rows(text)]
    assert rows[0][:4] == ["bare", "NA", "NA", "Normal completion"]
    assert rows[1][:4] == ["NA", "NA", "NA", "full.log missing"]
    assert SKIPPED not in text


def test_a_repeated_path_is_looked_up_under_its_make_unique_name(tmp_path: Path) -> None:
    """``file.info`` names the second ``/x`` row ``/x.1`` and the loop reads that path (an R quirk)."""
    run = remind_run(tmp_path, "run")
    env = effects(tmp_path, on_cluster=True)
    _, text = table([str(run), str(run)], env, colors=False, crayon=Crayon.disabled())
    rows = plain_rows(text)
    assert rows[0].startswith("run  ")
    assert rows[1] == f"{run}.1 {SKIPPED}"


def test_folder_without_config_or_log_is_skipped_with_its_path_as_given(tmp_path: Path) -> None:
    bare = make_run(tmp_path, "bare", {"full.log": STATUS_OK})
    env = effects(tmp_path, on_cluster=True)
    _, text = table([f"{bare}/"], env, colors=False, crayon=Crayon.disabled())
    assert plain_rows(text) == [f"{bare}/ {SKIPPED}"]


def test_folder_without_config_but_with_a_job_is_printed(tmp_path: Path) -> None:
    bare = make_run(tmp_path, "bare", {"full.log": STATUS_OK})
    env = effects(tmp_path, on_cluster=True, slurm=running_job(bare))
    _, text = table([str(bare)], env, colors=False, crayon=Crayon.disabled())
    row = cells_of(plain_rows(text)[0])
    assert row[:3] == ["bare", "NA", "standby"]  # Runtime NA (no stats), inSlurm without a suffix


def test_the_config_probe_follows_r_literally(tmp_path: Path) -> None:
    """``paste0(i, "/", NA)`` is ``<i>/NA``: an entry named ``NA`` passes the probe (``:58``)."""
    env = effects(tmp_path, on_cluster=True)
    with_log = make_run(tmp_path, "withlog", {"log.txt": ""})
    with_config_dir = make_run(tmp_path, "configdir", {"config/x": ""})
    with_na = make_run(tmp_path, "quirk", {"full.log": STATUS_OK, "NA": ""})
    bare = make_run(tmp_path, "bare", {"full.log": STATUS_OK, "notconfig.txt": ""})
    assert has_config_or_log(str(with_log), env)
    assert has_config_or_log(str(with_config_dir), env)
    assert has_config_or_log(str(with_na), env)
    assert not has_config_or_log(str(bare), env)
    assert not has_config_or_log(str(tmp_path / "missing"), env)
    _, text = table([str(with_na), str(bare)], env, colors=False, sortbytime=False, crayon=Crayon.disabled())
    rows = plain_rows(text)
    assert cells_of(rows[0])[:3] == ["quirk", "NA", "no"]
    assert rows[1] == f"{bare} {SKIPPED}"


def test_a_failing_run_with_a_config_is_skipped_because_of_error(tmp_path: Path) -> None:
    """The ``try()`` diagnostic goes to stderr (``:57``), the ``skipped because of error`` line to stdout."""
    broken = remind_run(tmp_path, "config-bak", extra={"config.Rdata.bak": ""})  # two config matches
    good = remind_run(tmp_path, "good")
    _, text, err = run_table(
        [f"{broken}/", str(good)], effects(tmp_path), colors=False, sortbytime=False, crayon=Crayon.disabled()
    )
    rows = plain_rows(text)
    assert rows[0] == f"config-bak {SKIPPED_BECAUSE_OF_ERROR}"
    assert rows[1].startswith("good  ")
    assert err == CONFIG_BAK_STDERR  # migration/goldens/rs/synthetic-remind.err:5


def test_gdx_without_iteration_number_prints_the_deferred_warning_after_the_error(tmp_path: Path) -> None:
    """BUG-038 / D-23: the ``if (NA)`` error (``R/getRunStatus.R:285``) and ``readGDX``'s deferred warning."""
    noiter = remind_run(tmp_path, "gdx-noiter", gdx={"o_modelstat": 2, "s80_bool": 0})
    _, text, err = run_table([str(noiter)], effects(tmp_path), colors=False, crayon=Crayon.disabled())
    assert plain_rows(text) == [f"gdx-noiter {SKIPPED_BECAUSE_OF_ERROR}"]
    assert err == GDX_NOITER_STDERR  # migration/goldens/rs/synthetic-remind.err:6-10


def test_gdx_without_s80_bool_prints_the_expand_pattern_warning(tmp_path: Path) -> None:
    """``if (s80_bool == 1)`` on ``numeric(0)`` (``R/getRunStatus.R:283``) after the typed read's warning."""
    no_flag = remind_run(tmp_path, "noflag", gdx={"o_iterationNumber": 26, "o_modelstat": 2})
    _, text, err = run_table([str(no_flag)], effects(tmp_path), colors=False, crayon=Crayon.disabled())
    assert plain_rows(text) == [f"noflag {SKIPPED_BECAUSE_OF_ERROR}"]
    assert err == (
        "Error in if (s80_bool == 1) { : argument is of length zero\n"
        "In addition: Warning message:\n"
        "In .expandPattern(c(...), gdx, format, type, react) :\n"
        "  No element of s80_bool found in GDX! NULL returned\n"
    )


def test_bug005_a_failing_run_without_a_config_aborts_the_table(tmp_path: Path) -> None:
    """``status[["jobInSLURM"]]`` on the ``try-error`` before ``inherits()`` (``:58``, BUG-005, D-03)."""
    good = remind_run(tmp_path, "good")
    bad = make_run(tmp_path, "noconfig-badstats", {"full.log": STATUS_OK})
    write_stats(bad, remind_stats(model_name=None))  # getRunStatus: argument is of length zero (:91)
    out = io.StringIO()
    err = io.StringIO()
    with pytest.raises(LoopRunsError, match=SUBSCRIPT_OUT_OF_BOUNDS):
        loop_runs(
            [str(good), str(bad)],
            colors=False,
            sortbytime=False,
            effects=effects(tmp_path),
            out=out,
            err=err,
            crayon=Crayon.disabled(),
        )
    rows = plain_rows(out.getvalue())
    assert len(rows) == 1 and rows[0].startswith("good  ")  # the partial table stays, as R's stdout does
    assert out.getvalue().startswith("Folder" + " " * 11 + COLUMN_SEPARATOR + "Runtime")  # width 17
    assert err.getvalue() == BUG005_STDERR  # printed by try() before the subscript aborts (bug005.err:5-6)


# ---------------------------------------------------------------------------------------------
# try()'s stderr text (base::try, PrintWarnings; R 4.6.1)
# ---------------------------------------------------------------------------------------------


def test_try_error_message_breaks_the_line_as_r_does() -> None:
    assert try_error_message(RunReadError("boom", call="f()")) == "Error in f() : boom\n"
    assert try_error_message(RunReadError("boom")) == "Error : boom\n"
    fits = "x" * (75 - 14 - len("boom"))  # 14 + nchar(call) + nchar(message) == 75: one line
    assert try_error_message(RunReadError("boom", call=fits)) == f"Error in {fits} : boom\n"
    assert try_error_message(RunReadError("boom", call=fits + "x")) == f"Error in {fits}x : \n  boom\n"
    long_call = 'if (runstatistics$stats[["config"]][["model_name"]] == "MAgPIE") {'
    assert try_error_message(RunReadError("argument is of length zero", call=long_call)) == BUG005_STDERR
    two_lines = RunReadError("a\n" + "b" * 80, call="f()")  # only the first line counts for the break
    assert try_error_message(two_lines) == "Error in f() : a\n" + "b" * 80 + "\n"


def test_deferred_warnings_message_shapes() -> None:
    short = DeferredWarning("f()", "w1")
    assert deferred_warnings_message([]) == ""
    assert deferred_warnings_message([short]) == "In addition: Warning message:\nIn f() : w1\n"
    assert deferred_warnings_message([short, DeferredWarning("f()", "w2")]) == (
        "In addition: Warning messages:\n1: In f() : w1\n2: In f() : w2\n"
    )
    assert deferred_warnings_message([DeferredWarning(None, "no call here")]) == (
        "In addition: Warning message:\nno call here \n"
    )
    long = DeferredWarning("long()", "User specified to read symbol o_iterationNumber, but it does not exist")
    assert deferred_warnings_message([long]) == (
        "In addition: Warning message:\nIn long() :\n  User specified to read symbol o_iterationNumber, "
        "but it does not exist\n"
    )
    fits = DeferredWarning("g()", "x" * (75 - 6 - len("g()")))  # 6 + nchar(call) + nchar(message) == 75
    assert deferred_warnings_message([fits]) == f"In addition: Warning message:\nIn g() : {fits.message}\n"
    numbered = DeferredWarning("g()", "x" * (75 - 10 - len("g()") + 1))  # offset 10 in the numbered form
    assert deferred_warnings_message([numbered, short]) == (
        f"In addition: Warning messages:\n1: In g() :\n  {numbered.message}\n2: In f() : w1\n"
    )
    assert deferred_warnings_message([short] * 11) == (
        "In addition: There were 11 warnings (use warnings() to see them)\n"
    )
    error = RunReadError("boom", call="f()", warnings=[short])
    assert try_error_message(error) == "Error in f() : boom\nIn addition: Warning message:\nIn f() : w1\n"


# ---------------------------------------------------------------------------------------------
# The displayed cells (R/loopRuns.R:66-84)
# ---------------------------------------------------------------------------------------------


def record(**overrides: object) -> RunStatus:
    base: dict[str, object] = {
        "run": "x",
        "path": "/x",
        "jobInSLURM": "no",
        "RunType": "nash",
        "modelstat": "2: Locally Optimal",
        "runInAppResults": None,
        "Mif": "yes",
        "Iter": "26/100",
        "RunStatus": "Normal completion",
        "Warnings": "0",
        "Conv": "converged",
        "Runtime": 8304,
    }
    return RunStatus(**{**base, **overrides})  # type: ignore[arg-type]


def test_display_cells_runtime_precedence_suffix_and_1regi() -> None:
    finished = display_cells(record())
    assert (finished["Runtime"], finished["jobInSLURM"]) == ("2.3 hours", "no")
    live = display_cells(record(jobInSLURM="standby startup", Runtime=1001))
    assert (live["Runtime"], live["jobInSLURM"]) == ("> 16.7 mins", "standby")  # numeric beats startup
    startup = display_cells(record(jobInSLURM="qos1 startup", Runtime=None))
    assert (startup["Runtime"], startup["jobInSLURM"]) == ("startup", "qos1")
    pending = display_cells(record(jobInSLURM="standby pending", Runtime=1001))
    assert (pending["Runtime"], pending["jobInSLURM"]) == ("pending", "standby")  # pending beats numeric
    users = display_cells(record(jobInSLURM="2 users", Runtime=None))
    assert (users["Runtime"], users["jobInSLURM"]) == ("NA", "2 users")
    local = display_cells(record(jobInSLURM="NA", Runtime=8304))  # BUG-042: "NA" != "no"
    assert (local["Runtime"], local["jobInSLURM"]) == ("> 2.3 hours", "NA")
    assert display_cells(record(RunType="testOneRegi EUR"))["RunType"] == "1Regi EUR"
    assert display_cells(record(RunType="testOneRegi-testOneRegi"))["RunType"] == "1Regi-1Regi"
    assert display_cells(record(Iter="26/100"))["Iter"] == "26/100"  # untouched cells stay


def test_display_cells_through_get_run_status(tmp_path: Path) -> None:
    live = remind_run(tmp_path, "live", stats=remind_stats(gams_end=None, prepare_start=NOW_EPOCH - 1000.6))
    env = effects(tmp_path, on_cluster=True, slurm=running_job(live, elapsed="0:45"))
    cells = display_cells(get_run_status([str(live)], effects=env)[0])
    assert (cells["Runtime"], cells["jobInSLURM"]) == ("> 16.7 mins", "standby")
    env = effects(tmp_path, on_cluster=True, slurm=running_job(live, state="PENDING"))
    cells = display_cells(get_run_status([str(live)], effects=env)[0])
    assert (cells["Runtime"], cells["jobInSLURM"]) == ("pending", "standby")


def test_format_row_trims_the_blanks_before_the_newline() -> None:
    cells: dict[str, CellValue] = {
        "jobInSLURM": "no",
        "RunType": "nash",
        "modelstat": "NA",
        "Mif": "NA",
        "Iter": "NA",
        "RunStatus": "full.log missing",
        "Warnings": "0",
        "Conv": "NA",
        "Runtime": "NA",
    }
    line = format_row("nofulllog", cells, table_layout(15, False), False)
    assert line == (
        "nofulllog        NA           nash         full.log missing   0          NA                "
        "NA                     NA                   NA\n"
    )
    cells["runInAppResults"] = "no"
    on_cluster = format_row("nofulllog", cells, table_layout(15, True), True)
    assert on_cluster.endswith("NA                     NA      no\n")
    assert on_cluster.startswith("nofulllog        NA           no        nash")


# ---------------------------------------------------------------------------------------------
# Colours (R/loopRuns.R:87-132)
# ---------------------------------------------------------------------------------------------


def test_colour_wraps_the_line_with_its_newline_and_b_suppresses_rows_only(tmp_path: Path) -> None:
    converged = remind_run(tmp_path, "conv", gdx=CONVERGED_GDX)
    env = effects(tmp_path)
    _, coloured = table([str(converged)], env)
    line = coloured.split(UNDERLINE_CLOSE + " \n", 1)[1]
    assert line.startswith(f"{GREEN}conv  ") and line.endswith(f"yes\n{CLOSE}")
    _, uncoloured = table([str(converged)], env, colors=False)
    assert uncoloured.startswith(UNDERLINE) and uncoloured.count(ESC) == 2  # the underline pair only
    assert uncoloured.endswith("yes\n")


def test_magpie_rows_take_the_magpie_branch(tmp_path: Path) -> None:
    mag = magpie_run(tmp_path, "mag", files={"full.log": STATUS_OK})
    _, text = table([str(mag)], effects(tmp_path))
    line = text.split(UNDERLINE_CLOSE + " \n", 1)[1]
    assert line.startswith(f"{GREEN}mag  ") and "nlp_apr17" in line and "222" in line


def test_pending_rows_are_yellow_on_the_cluster(tmp_path: Path) -> None:
    run = remind_run(tmp_path, "run")
    env = effects(tmp_path, on_cluster=True, slurm=running_job(run, state="PENDING"))
    _, text = table([str(run)], env)
    line = text.split(UNDERLINE_CLOSE + " \n", 1)[1]
    assert line.startswith(f"{YELLOW}run  ")
    assert cells_of(ANSI.sub("", line))[1:3] == ["pending", "standby"]


# ---------------------------------------------------------------------------------------------
# User, streams
# ---------------------------------------------------------------------------------------------


def test_user_defaults_to_the_effects_user(tmp_path: Path) -> None:
    """``user`` reaches ``getRunStatus`` (``:22, 57``): one's own job shows its QOS, another's its owner."""
    run = remind_run(tmp_path, "run")
    env = effects(tmp_path, on_cluster=True, slurm=running_job(run))
    _, mine = table([str(run)], env, colors=False, crayon=Crayon.disabled())
    assert cells_of(plain_rows(mine)[0])[2] == "standby"
    _, alices = table([str(run)], env, colors=False, user="alice", crayon=Crayon.disabled())
    assert cells_of(plain_rows(alices)[0])[2] == ME


def test_writes_to_stdout_and_stderr_by_default(tmp_path: Path, capsys: pytest.CaptureFixture[str]) -> None:
    run = remind_run(tmp_path, "run")
    assert loop_runs([str(run)], effects=effects(tmp_path), colors=False, crayon=Crayon.disabled()) is None
    captured = capsys.readouterr()
    assert captured.out.startswith(LOCAL_HEADER) and captured.err == ""
    broken = remind_run(tmp_path, "config-bak", extra={"config.Rdata.bak": ""})
    assert loop_runs([str(broken)], effects=effects(tmp_path), colors=False, crayon=Crayon.disabled()) is None
    captured = capsys.readouterr()
    assert plain_rows(captured.out) == [f"config-bak {SKIPPED_BECAUSE_OF_ERROR}"]
    assert captured.err == CONFIG_BAK_STDERR
