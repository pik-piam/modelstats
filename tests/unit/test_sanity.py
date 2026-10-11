"""``modelstats.sanity``: every rule of ``getSanityChecks()`` (``R/getSanityChecks.R``) on synthetic runs.

The run folders come from the builders of ``test_facts.py`` and ``test_run_status.py``; the AMT lookup is
pointed at a temporary checkout through an injected ``ClusterPaths``. Every test names the R lines it
reproduces; the goldens (``tests/golden/test_sanity.py``) pin the same bytes on the fixture runs.
"""

from __future__ import annotations

import io
import json
from pathlib import Path

import pytest

from modelstats.colors import Crayon
from modelstats.env import ClusterPaths, FakeEffects
from modelstats.errors import RunReadError
from modelstats.facts import INVALID_DESCRIPTION
from modelstats.formatting import Row
from modelstats.rdata_io import write_rds
from modelstats.sanity import (
    CANNOT_OPEN_CONNECTION,
    EXPLANATION_URL,
    INVALID_PATTERN,
    SANITY_COLUMNS,
    SanityError,
    amt_runcode,
    column_titles,
    folder_width,
    get_sanity_checks,
    header,
    normalized_or_error,
    sanity_row,
)
from tests.unit.test_facts import effects, make_run
from tests.unit.test_run_status import STATUS_OK, magpie_run, remind_run

CSV_FILES = {
    "REMIND_generic_t_summation_errors.csv": "model,variable,value\na,X,1\na,Y,2\na,X,3\n",
    "REMIND_generic_t_range_errors.txt": "variable;min;max\nA;0;1\n",
    "log_fixOnRef.csv": "variable,region\nV1,EUR\nV2,EUR\n",
}
EXPLANATION = f"For column explanations see: {EXPLANATION_URL}"
TITLES_15 = "Folder           SumErr  RangeErr  FixingErr  MissingVar  ProjSumErr  ProjSumErrReg"
HEADER_15 = f"\n\x1b[36m{EXPLANATION}\n\x1b[39m\x1b[4m{TITLES_15}\x1b[24m \n\n"
PLAIN_HEADER_15 = f"\n{EXPLANATION}\n{TITLES_15} \n\n"
# The row of a 15-wide folder column: ``printOutput`` cells joined with three spaces (BUG-034).
ROW_REMIND = "remind" + " " * 9 + "   " + "2     " + "   " + "1       " + "   " + "2        "
ROW_REMIND += "   " + " " * 10 + "   " + " " * 10 + "   " + " " * 13 + "\n"


def capture(dirs: list[str] | str | None, env: FakeEffects, crayon: Crayon | None = None) -> str:
    out = io.StringIO()
    get_sanity_checks(dirs, env, out, crayon=crayon)
    return out.getvalue()


def amt_checkout(tmp_path: Path) -> tuple[Path, ClusterPaths]:
    """A temporary AMT checkout with an ``output`` directory, as ``ClusterPaths`` names the real one."""
    checkout = tmp_path / "amt"
    (checkout / "output").mkdir(parents=True)
    paths = ClusterPaths(
        amt_dir=str(checkout),
        amt_output=f"{checkout}/output/",
        amt_archive=f"{checkout}/output/archive",
        amt_runcode=f"{checkout}/runcode.rds",
    )
    return checkout, paths


# ---------------------------------------------------------------------------------------------
# Folder width and normalizePath(mustWork = TRUE) (R/getSanityChecks.R:29-30)
# ---------------------------------------------------------------------------------------------


def test_folder_width_is_bounded_by_15_and_67(tmp_path: Path) -> None:
    env = effects(tmp_path)
    short = make_run(tmp_path, "r")
    middle = make_run(tmp_path, "m" * 30)
    long = make_run(tmp_path, "l" * 80)
    assert folder_width([], env) == 15
    assert folder_width([str(short)], env) == 15
    assert folder_width([str(short), str(middle)], env) == 30
    assert folder_width([str(middle), str(long)], env) == 67


def test_folder_width_uses_the_basename_of_the_resolved_path(tmp_path: Path) -> None:
    run = make_run(tmp_path, "a" * 20)
    (tmp_path / "link").symlink_to(run)
    env = effects(tmp_path)
    assert folder_width([str(tmp_path / "link")], env) == 20
    assert folder_width([f"{run}/"], env) == 20
    assert folder_width([run.name], env) == 20  # relative to the effects' cwd


def test_missing_path_is_the_r_normalize_path_error(tmp_path: Path) -> None:
    env = effects(tmp_path)
    run = make_run(tmp_path, "run")
    missing = tmp_path / "gone"
    with pytest.raises(SanityError) as excinfo:
        folder_width([str(run), str(missing)], env)
    assert str(excinfo.value) == f'path[2]="{missing}": No such file or directory'
    (tmp_path / "file").write_text("x", encoding="utf-8")
    with pytest.raises(SanityError, match=r'path\[1\]=".*/file/below": Not a directory'):
        normalized_or_error(str(tmp_path / "file" / "below"), 1, env)
    (tmp_path / "dangling").symlink_to(tmp_path / "nowhere")
    with pytest.raises(SanityError, match="No such file or directory"):
        normalized_or_error(str(tmp_path / "dangling"), 1, env)
    with pytest.raises(SanityError, match=r'path\[3\]="": No such file or directory'):
        normalized_or_error("", 3, env)


def test_nothing_is_printed_before_a_path_error(tmp_path: Path) -> None:
    env = effects(tmp_path)
    out = io.StringIO()
    with pytest.raises(SanityError):
        get_sanity_checks([str(tmp_path / "gone")], env, out)
    assert out.getvalue() == ""


# ---------------------------------------------------------------------------------------------
# Header (R/getSanityChecks.R:33-49)
# ---------------------------------------------------------------------------------------------


def test_column_titles_and_their_widths() -> None:
    titles = column_titles(20)
    assert titles[0] == "Folder" + " " * 14
    assert titles[1:] == ("SumErr", "RangeErr", "FixingErr", "MissingVar", "ProjSumErr", "ProjSumErrReg")
    assert [len(t) for t in titles] == [20, 6, 8, 9, 10, 10, 13]


def test_header_bytes_with_crayon_on_and_off() -> None:
    titles = column_titles(15)
    assert header(titles, Crayon()) == HEADER_15
    assert header(titles, Crayon.disabled()) == PLAIN_HEADER_15
    assert header(titles, Crayon(colours=8)) == HEADER_15  # cyan and underline are basic codes


def test_empty_dirs_print_the_header_only(tmp_path: Path) -> None:
    assert capture([], effects(tmp_path)) == HEADER_15


# ---------------------------------------------------------------------------------------------
# Rows (R/getSanityChecks.R:52-57)
# ---------------------------------------------------------------------------------------------


def test_rows_use_three_spaces_under_the_two_space_header(tmp_path: Path) -> None:
    run = remind_run(tmp_path, "remind", extra=CSV_FILES)
    text = capture([str(run)], effects(tmp_path))
    assert text == HEADER_15 + ROW_REMIND
    assert "  SumErr  RangeErr" in text and "   2        1          2" in text


def test_runs_without_every_sanity_column_are_omitted(tmp_path: Path) -> None:
    env = effects(tmp_path)
    with_mif = remind_run(tmp_path, "remind", extra=CSV_FILES)
    without_mif = remind_run(tmp_path, "nomif", mif=False)
    magpie = magpie_run(tmp_path, "magpie", files={"full.log": STATUS_OK})
    a_file = tmp_path / "plain.txt"
    a_file.write_text("x", encoding="utf-8")
    text = capture([str(without_mif), str(magpie), str(a_file), str(with_mif)], env)
    assert text == HEADER_15 + ROW_REMIND


def test_sanity_row_requires_every_column() -> None:
    full = Row("r", dict.fromkeys(SANITY_COLUMNS, 0))
    assert (
        sanity_row(full, [15, 6, 8, 9, 10, 10, 13])
        == "r" + " " * 14 + "   " + "   ".join("0".ljust(w) for w in (6, 8, 9, 10, 10, 13)) + "\n"
    )
    partial = Row("r", {name: 0 for name in SANITY_COLUMNS[:-1]})
    assert sanity_row(partial, [15, 6, 8, 9, 10, 10, 13]) is None


def test_real_na_cells_print_as_blanks_and_numbers_as_text(tmp_path: Path) -> None:
    run = remind_run(tmp_path, "remind", extra=CSV_FILES)
    write_rds(
        run / "projectSummations.rds",
        {"ScenarioMIP": {"missingVars": 2, "checkSummations": 32, "checkSummationsRegional": 0}},
    )
    text = capture([str(run)], effects(tmp_path))
    row = text.removeprefix(HEADER_15)
    counts = "   2        1          2           2            32           0            \n"
    assert row == "remind" + " " * 9 + counts


def test_a_single_path_string_is_accepted(tmp_path: Path) -> None:
    run = remind_run(tmp_path, "remind", extra=CSV_FILES)
    assert capture(str(run), effects(tmp_path)) == HEADER_15 + ROW_REMIND


def test_crayon_disabled_prints_no_escapes(tmp_path: Path) -> None:
    run = remind_run(tmp_path, "remind", extra=CSV_FILES)
    text = capture([str(run)], effects(tmp_path), Crayon.disabled())
    assert "\x1b" not in text
    assert text == PLAIN_HEADER_15 + ROW_REMIND


def test_a_run_read_error_propagates_after_the_header(tmp_path: Path) -> None:
    # A second match of the unanchored configuration regex (BUG-003) aborts getRunStatus.
    run = remind_run(tmp_path, "remind", extra={**CSV_FILES, "config.Rdata.bak": ""})
    out = io.StringIO()
    with pytest.raises(RunReadError, match=INVALID_DESCRIPTION):
        get_sanity_checks([str(run)], effects(tmp_path), out)
    assert out.getvalue() == HEADER_15


def test_default_out_is_the_process_stdout(tmp_path: Path, capsys: pytest.CaptureFixture[str]) -> None:
    get_sanity_checks([], effects(tmp_path), crayon=Crayon.disabled())
    assert capsys.readouterr().out == PLAIN_HEADER_15


# ---------------------------------------------------------------------------------------------
# The default AMT lookup (R/getSanityChecks.R:22-26; 06 section 3.4, D-26)
# ---------------------------------------------------------------------------------------------


def test_default_lookup_reads_runcode_rds_and_lists_the_matching_runs(tmp_path: Path) -> None:
    checkout, paths = amt_checkout(tmp_path)
    write_rds(checkout / "runcode.rds", ".*-AMT_2026-09-30")
    remind_run(checkout / "output", "b-AMT_2026-09-30", extra=CSV_FILES)
    remind_run(checkout / "output", "a-AMT_2026-09-30", extra=CSV_FILES)
    remind_run(checkout / "output", "c-AMT_2026-09-23", extra=CSV_FILES)
    env = effects(tmp_path, cluster_paths=paths)
    text = capture(None, env)
    lines = text.splitlines()
    assert lines[0] == f"Results from {checkout}/output/ "
    assert [line.split()[0] for line in lines[5:]] == ["a-AMT_2026-09-30", "b-AMT_2026-09-30"]
    assert lines[3].startswith("\x1b[39m\x1b[4mFolder" + " " * 10 + "  SumErr")  # width 16


def test_amt_runcode_prefers_the_state_file(tmp_path: Path) -> None:
    checkout, paths = amt_checkout(tmp_path)
    write_rds(checkout / "runcode.rds", "from-rds")
    env = effects(tmp_path, cluster_paths=paths)
    assert amt_runcode(env) == "from-rds"
    state = checkout / "amt-state.json"
    state.write_text(json.dumps({"phase": "started", "cycle": {"runcode": "from-cycle"}}), encoding="utf-8")
    assert amt_runcode(env) == "from-cycle"
    state.write_text(
        json.dumps({"phase": "idle", "cycle": None, "last_completed": {"runcode": "from-last"}}),
        encoding="utf-8",
    )
    assert amt_runcode(env) == "from-last"
    state.write_text(json.dumps({"phase": "idle"}), encoding="utf-8")
    assert amt_runcode(env) == "from-rds"
    state.write_text("{not json", encoding="utf-8")
    with pytest.raises(SanityError, match="amt-state.json"):
        amt_runcode(env)


def test_missing_runcode_rds_is_the_readrds_error_after_the_results_line(tmp_path: Path) -> None:
    checkout, paths = amt_checkout(tmp_path)
    env = effects(tmp_path, cluster_paths=paths)
    out = io.StringIO()
    with pytest.raises(SanityError) as excinfo:
        get_sanity_checks(None, env, out)
    assert str(excinfo.value) == CANNOT_OPEN_CONNECTION
    assert out.getvalue() == f"Results from {checkout}/output/ \n"


def test_a_runcode_that_is_not_one_string_is_an_invalid_pattern(tmp_path: Path) -> None:
    checkout, paths = amt_checkout(tmp_path)
    write_rds(checkout / "runcode.rds", 5)
    with pytest.raises(SanityError, match=INVALID_PATTERN):
        amt_runcode(effects(tmp_path, cluster_paths=paths))


def test_default_lookup_without_matching_runs_prints_the_header_only(tmp_path: Path) -> None:
    checkout, paths = amt_checkout(tmp_path)
    write_rds(checkout / "runcode.rds", "nothing-matches")
    text = capture(None, effects(tmp_path, cluster_paths=paths))
    assert text == f"Results from {checkout}/output/ \n" + HEADER_15
