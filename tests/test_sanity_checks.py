from __future__ import annotations

import dataclasses
import os
from pathlib import Path
from typing import Any

import numpy as np
import pytest
import rdata

from modelstats import _console, _shell, sanity_checks
from modelstats._rdata import write_rds_string
from modelstats.errors import ModelstatsError
from modelstats.run_status import R_COLUMNS, RunStatus
from modelstats.sanity_checks import (
    EXPLANATION_URL,
    SANITY_COLUMNS,
    SANITY_TITLES,
    latest_amt_dirs,
    print_sanity_checks,
    read_runcode,
)
from tests.conftest import capture_console, make_run

HEADER = "Folder           SumErr  RangeErr  FixingErr  MissingVar  ProjSumErr  ProjSumErrReg "
WIDTHS = (15, 6, 8, 9, 10, 10, 13)


def sanity_row(*cells: str) -> str:
    return "   ".join(cell.ljust(width) for cell, width in zip(cells, WIDTHS, strict=True))


def checked(**cells: int | float | str | None) -> RunStatus:
    """A status whose plausibility block ran: every sanity column 0 unless given."""
    assert set(cells) <= set(SANITY_COLUMNS)
    status = RunStatus(name="", path=Path())
    for column in SANITY_COLUMNS:
        setattr(status, column, cells.get(column, 0))
    return status


class StatusTable(dict[str, RunStatus]):
    """Statuses by folder name, the folders whose status fails, and the keyword arguments used."""

    def __init__(self) -> None:
        super().__init__()
        self.calls: list[dict[str, object]] = []
        self.failing: set[str] = set()


@pytest.fixture
def statuses(monkeypatch: pytest.MonkeyPatch) -> StatusTable:
    """Replace get_run_status by a lookup on the folder name (a row without sanity columns by default)."""
    table = StatusTable()

    def fake(dirs: Path, **kwargs: Any) -> list[RunStatus]:
        table.calls.append(kwargs)
        if dirs.name in table.failing:
            raise ModelstatsError("broken runstatistics")
        status = table.get(dirs.name, RunStatus(name=dirs.name, path=dirs))
        return [dataclasses.replace(status, name=dirs.name, path=dirs)]

    monkeypatch.setattr(sanity_checks, "get_run_status", fake)
    return table


@pytest.fixture
def amt_output(tmp_path: Path, monkeypatch: pytest.MonkeyPatch, data_dir: Path) -> Path:
    """A cluster AMT output folder with the runcode fixture (``.*-AMT_2026-10-18|.*-AMT_2026-10-19``)."""
    output = tmp_path / "output"
    for name in [
        "SSP2-NPi-AMT_2026-10-18_10.00.00",
        "SSP2-NPi-AMT_2026-10-11_10.00.00",
        "SSP2-PkBudg1000-AMT_2026-10-18_10.00.00",
        "SSP2-PkBudg650-AMT_2026-10-18_10.00.00",
        "default-AMT_2026-10-19_01.00.00",
        ".hidden-AMT_2026-10-18_00.00.00",
        "archive",
    ]:
        (output / name).mkdir(parents=True)
    (output / "note-AMT_2026-10-18.txt").write_text("")
    monkeypatch.setattr(_shell, "AMT_OUTPUT", output)
    monkeypatch.setattr(_shell, "AMT_RUNCODE", data_dir / "runcode.rds")
    return output


def test_table_layout(tmp_path: Path, statuses: StatusTable) -> None:
    run = make_run(tmp_path, "SSP2-NPi-AMT")
    statuses["SSP2-NPi-AMT"] = checked(
        summation_errors=3,
        fix_errors=1,
        missing_proj_vars=None,
        proj_summation_errors=2,
        proj_summation_errors_regional=None,
    )
    skipped = make_run(tmp_path, "magpie")  # no plausibility columns: not printed
    out = capture_console()
    print_sanity_checks([run, skipped], out=out.console)
    # rows use three-space separators, the header two (BUG-036, open)
    assert out.lines == [
        "",
        f"For column explanations see: {EXPLANATION_URL}",
        HEADER,
        "",
        "SSP2-NPi-AMT      3        0          1                        2                         ",
    ]
    assert out.lines[4] == sanity_row("SSP2-NPi-AMT", "3", "0", "1", "", "2", "")
    assert statuses.calls == [{}, {}]  # the default user, detailed, newest first


def test_rows_keep_the_given_order_and_values_as_stored(tmp_path: Path, statuses: StatusTable) -> None:
    second = make_run(tmp_path, "b-run")
    first = make_run(tmp_path, "a-run")
    statuses["b-run"] = checked(
        missing_proj_vars=2.5, proj_summation_errors="3", proj_summation_errors_regional=100000.0
    )
    statuses["a-run"] = checked(summation_errors=1)
    out = capture_console()
    print_sanity_checks([second, first], out=out.console)
    assert out.lines[4] == sanity_row("b-run", "0", "0", "0", "2.5", "3", "1e+05")
    assert out.lines[5] == sanity_row("a-run", "1", "0", "0", "0", "0", "0")


def test_run_names_are_not_markup(tmp_path: Path, statuses: StatusTable) -> None:
    run = make_run(tmp_path, "run[bold]x")
    statuses["run[bold]x"] = checked()
    out = capture_console()
    print_sanity_checks([run], out=out.console)
    assert out.lines[4] == sanity_row("run[bold]x", "0", "0", "0", "0", "0", "0")


def test_styles(tmp_path: Path, statuses: StatusTable) -> None:
    out = capture_console(color=True)
    print_sanity_checks([], out=out.console)
    assert f"\x1b[36mFor column explanations see: {EXPLANATION_URL}\x1b[0m" in out.text  # cyan
    assert "\x1b[4m" + HEADER.rstrip(" ") + "\x1b[0m \n" in out.text  # underlined, the trailing space not


def test_default_console_is_stdout(statuses: StatusTable, monkeypatch: pytest.MonkeyPatch) -> None:
    out = capture_console()
    monkeypatch.setattr(_console, "out", out.console)
    print_sanity_checks([])
    assert out.lines[2] == HEADER


def test_empty_list_prints_the_header_only(statuses: StatusTable) -> None:
    out = capture_console()
    print_sanity_checks([], out=out.console)
    assert out.lines == ["", f"For column explanations see: {EXPLANATION_URL}", HEADER, ""]


def test_first_column_width_is_capped(tmp_path: Path, statuses: StatusTable) -> None:
    long_run = make_run(tmp_path, "x" * 70)
    link = tmp_path / "short"
    link.symlink_to(long_run)  # the width follows the resolved name
    out = capture_console()
    print_sanity_checks([link], out=out.console)
    assert out.lines[2].startswith("Folder" + " " * 61 + "  SumErr")


def test_missing_folder_is_an_error(tmp_path: Path, statuses: StatusTable) -> None:
    with pytest.raises(ModelstatsError, match="No such file"):
        print_sanity_checks([tmp_path / "missing"], out=capture_console().console)


def test_failing_run_aborts_after_the_rows_before_it(tmp_path: Path, statuses: StatusTable) -> None:
    # BUG-055: R did not wrap getRunStatus() in try(), the table stops at the failing run
    good = make_run(tmp_path, "good")
    bad = make_run(tmp_path, "bad")
    statuses["good"] = checked()
    statuses.failing.add("bad")
    out = capture_console()
    with pytest.raises(ModelstatsError, match="broken"):
        print_sanity_checks([good, bad], out=out.console)
    assert out.lines[4] == sanity_row("good", "0", "0", "0", "0", "0", "0")


def test_latest_amt_dirs_sorted(amt_output: Path) -> None:
    assert [p.name for p in latest_amt_dirs()] == [  # files too, dot-entries not, run order (BUG-054)
        "default-AMT_2026-10-19_01.00.00",
        "note-AMT_2026-10-18.txt",
        "SSP2-NPi-AMT_2026-10-18_10.00.00",
        "SSP2-PkBudg650-AMT_2026-10-18_10.00.00",  # numbers numerically: 650 before 1000
        "SSP2-PkBudg1000-AMT_2026-10-18_10.00.00",
    ]


def test_latest_amt_dirs_with_a_pattern(amt_output: Path) -> None:
    assert [p.name for p in latest_amt_dirs("10-11")] == ["SSP2-NPi-AMT_2026-10-11_10.00.00"]
    with pytest.raises(ModelstatsError, match="^rs2 -t: invalid pattern"):
        latest_amt_dirs("AMT_[", source="rs2 -t")


def test_latest_amts_table_prints_the_results_line(amt_output: Path, statuses: StatusTable) -> None:
    out = capture_console()
    print_sanity_checks(out=out.console)
    assert out.lines[0] == f"Results from {amt_output}/ "
    assert out.lines[3] == HEADER.replace("Folder" + " " * 9, "Folder" + " " * 33)


def test_latest_amt_dirs_of_a_missing_folder(
    tmp_path: Path, statuses: StatusTable, monkeypatch: pytest.MonkeyPatch, data_dir: Path
) -> None:
    monkeypatch.setattr(_shell, "AMT_OUTPUT", tmp_path / "gone")
    monkeypatch.setattr(_shell, "AMT_RUNCODE", data_dir / "runcode.rds")
    assert latest_amt_dirs() == []
    out = capture_console()
    print_sanity_checks(out=out.console)
    assert out.lines[3] == HEADER


@pytest.mark.skipif(os.geteuid() == 0, reason="root reads every folder")
def test_latest_amt_dirs_of_an_unreadable_folder(tmp_path: Path, monkeypatch: pytest.MonkeyPatch) -> None:
    folder = tmp_path / "amt"
    folder.mkdir()
    (folder / "x-run").mkdir()
    folder.chmod(0o311)
    monkeypatch.setattr(_shell, "AMT_OUTPUT", folder)
    try:
        assert latest_amt_dirs("x") == []  # dir() gave character(0)
    finally:
        folder.chmod(0o755)


def test_read_runcode_rejects_vectors(tmp_path: Path, data_dir: Path) -> None:
    assert read_runcode(data_dir / "runcode.rds") == ".*-AMT_2026-10-18|.*-AMT_2026-10-19"
    vector = tmp_path / "vector.rds"
    rdata.write_rds(vector, np.array(["a", "b"], dtype=object))  # BUG-072: dir() used the first one
    with pytest.raises(ModelstatsError, match="expected one string, found list"):
        read_runcode(vector)
    with pytest.raises(ModelstatsError, match="one string"):
        read_runcode(data_dir / "projectSummations.rds")
    with pytest.raises(ModelstatsError):
        read_runcode(tmp_path / "missing.rds")
    bad = tmp_path / "bad.rds"
    write_rds_string(bad, "AMT_[")
    with pytest.raises(ModelstatsError, match="invalid pattern"):
        read_runcode(bad)
    with pytest.raises(ModelstatsError, match="invalid pattern"):
        latest_amt_dirs("AMT_[")


def test_sanity_columns_match_run_status_fields() -> None:
    assert len(SANITY_TITLES) == len(SANITY_COLUMNS)
    assert all(hasattr(RunStatus("n", Path("p")), column) for column in SANITY_COLUMNS)
    assert [R_COLUMNS[column] for column in SANITY_COLUMNS] == [
        "summationErrors",
        "rangeErrors",
        "fixErrors",
        "missingProjVars",
        "projSummationErrors",
        "projSummationErrorsRegional",
    ]


@pytest.mark.usefixtures("shell", "clock")
def test_end_to_end_with_real_statuses(tmp_path: Path) -> None:
    run = make_run(
        tmp_path,
        "SSP2-NPi-AMT",
        {"REMIND_generic_SSP2-NPi-AMT.mif": "x", "log_fixOnRef.csv": "variable\nA\nB\n"},
        config="remind",
    )
    no_mif = make_run(tmp_path, "nomif", config="remind")
    magpie = make_run(tmp_path, "magpie", {"validation.mif": "x"}, config="magpie", stats="magpie")
    bare = make_run(tmp_path, "bare")
    out = capture_console()
    print_sanity_checks([no_mif, magpie, run, bare], out=out.console)
    assert out.lines[4:] == [sanity_row("SSP2-NPi-AMT", "0", "0", "2", "", "", "")]
