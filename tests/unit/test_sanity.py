"""sanity: ``get_sanity_checks`` against R/getSanityChecks.R (R 4.6.1 facts pinned 2026-10-01).

The golden tier (``tests/golden/test_sanity.py``) proves byte parity on the real fixtures; this module
pins the R semantics the port relies on without ``migration/``: the header bytes, the folder width
bounds, the two-space header over three-space rows (BUG-034), the omitted rows (BUG-012), the
``normalizePath(mustWork = TRUE)`` error, the ``dir(pattern = )`` listing of the AMT path and the
order of the ``Results from`` line and the ``readRDS`` failure. R facts verified with
``LC_ALL=C.utf8 Rscript`` on 2026-10-01:

- ``dir("d/", full.names = TRUE)`` gives ``d//a-AMT_2026-09-28`` (an unconditional ``/`` after the path),
  dot-files are skipped, a file or a missing directory lists ``character(0)``, an invalid pattern stops with
  ``invalid 'pattern' regular expression``;
- ``normalizePath(c("d", "/does/not/exist", "/also/missing"), mustWork = TRUE)`` stops with
  ``path[2]="/does/not/exist": No such file or directory``; ``normalizePath("~/nope-xyz", mustWork = TRUE)``
  reports the expanded path; ``basename(normalizePath("d/"))`` is ``d``; ``character(0)`` gives width 15;
- ``cat("Results from", "/p/x/", "\\n")`` prints ``Results from /p/x/ \\n``;
- with ``R_CLI_NUM_COLORS`` unset, ``options(crayon.enabled = FALSE)`` makes ``cyan("x")`` print ``x``.
"""

from __future__ import annotations

import io
import shutil
from collections.abc import Iterator
from pathlib import Path

import pytest
from _fake_effects import FakeEffects

from modelstats import colors, sanity
from modelstats.errors import RParityError
from modelstats.rdata_io import write_rds
from modelstats.run_status import StatusTable
from modelstats.sanity import AMT_PATH, EXPLANATION, SANITY_COLUMNS, get_sanity_checks, r_dir_pattern

DATA = Path(__file__).resolve().parent / "data"
CYAN, UNDERLINE = "\x1b[36m", "\x1b[4m"
CLOSE_COLOR, CLOSE_UNDERLINE = "\x1b[39m", "\x1b[24m"
TITLES = "SumErr  RangeErr  FixingErr  MissingVar  ProjSumErr  ProjSumErrReg"


# --------------------------------------------------------------------------- helpers


@pytest.fixture(autouse=True)
def colours_on() -> Iterator[None]:
    """The harness regime: colours forced on at 256 colours; restored afterwards."""
    was, depth = colors.is_enabled(), colors.num_colors()
    colors.set_enabled(True, 256)
    yield
    colors.set_enabled(was, depth)


@pytest.fixture
def eff() -> FakeEffects:
    return FakeEffects(on_cluster=False)


def capture(dirs: object, eff: FakeEffects) -> str:
    buf = io.StringIO()
    get_sanity_checks(dirs, effects=eff, out=buf)  # type: ignore[arg-type]
    return buf.getvalue()


def header(width: int) -> str:
    """The bytes R prints before the rows for a folder column of ``width`` characters (colours on)."""
    folder = "Folder" + " " * (width - 6)
    return f"\n{CYAN}{EXPLANATION}{CLOSE_COLOR}{UNDERLINE}{folder}  {TITLES}{CLOSE_UNDERLINE} \n\n"


def remind_run(root: Path, name: str) -> Path:
    """A minimal REMIND run folder that reaches the sanity columns: config.Rdata (title testOneRegi) and its mif."""
    run = root / name
    run.mkdir(parents=True)
    shutil.copy(DATA / "config.Rdata", run / "config.Rdata")
    (run / "REMIND_generic_testOneRegi.mif").touch()
    (run / "full.log").write_text("*** Status: Normal completion\n", encoding="utf-8")
    return run


def table(rowname: str, **cells: object) -> StatusTable:
    out = StatusTable()
    for column, value in cells.items():
        out[rowname, column] = value
    return out


def sanity_cells(**overrides: object) -> dict[str, object]:
    cells: dict[str, object] = dict.fromkeys(SANITY_COLUMNS, 0)
    cells.update(overrides)
    return cells


class StubStatus:
    """A stand-in for ``get_run_status`` that records its calls and answers from a path -> table map."""

    def __init__(self, answers: dict[str, StatusTable] | None = None, default: StatusTable | None = None) -> None:
        self.answers = answers or {}
        self.default = default if default is not None else StatusTable()
        self.calls: list[tuple[object, dict[str, object]]] = []

    def __call__(self, path: object, **kwargs: object) -> StatusTable:
        self.calls.append((path, kwargs))
        return self.answers.get(str(path), self.default)


# --------------------------------------------------------------------------- end to end


def test_header_and_row_bytes_for_a_real_run(tmp_path: Path, eff: FakeEffects) -> None:
    """A run with a mif prints the cyan line, the underlined two-space header, a blank line and a three-space row."""
    run = remind_run(tmp_path, "SSP2-NPi-AMT_2026-09-28_10.30.27")
    out = capture([str(run)], eff)
    # the three project columns are real NA without projectSummations.rds and print as blanks: 8 (pad of the
    # 9-wide FixingErr) + 3 + 10 + 3 + 10 + 3 + 13 = 50 spaces after the last digit, as in the R golden
    row = "SSP2-NPi-AMT_2026-09-28_10.30.27   0        0          0" + " " * 50 + "\n"
    assert out == header(32) + row
    assert "   0        0          0" in out  # three spaces after the folder, BUG-034 (D-18 pending)


def test_runs_without_sanity_columns_are_omitted(tmp_path: Path, eff: FakeEffects) -> None:
    """BUG-012 (D-04 pending): a folder whose status lacks a sanity column prints nothing, not even its name."""
    bare = tmp_path / "bare-run"
    bare.mkdir()
    run = remind_run(tmp_path, "with-mif")
    out = capture([str(bare), str(run)], eff)
    assert out == header(15) + "with-mif" + " " * 10 + "0" + " " * 8 + "0" + " " * 10 + "0" + " " * 50 + "\n"
    assert "bare-run" not in out


def test_accepts_a_single_path(tmp_path: Path, eff: FakeEffects) -> None:
    run = remind_run(tmp_path, "single")
    assert capture(str(run), eff) == capture([run], eff) == capture(run, eff)


def test_output_goes_to_stdout_by_default(tmp_path: Path, eff: FakeEffects, capsys: pytest.CaptureFixture[str]) -> None:
    bare = tmp_path / "x"
    bare.mkdir()
    get_sanity_checks([str(bare)], effects=eff)
    assert capsys.readouterr().out == header(15)


# --------------------------------------------------------------------------- the printed rows (stubbed status)


def test_row_values_na_string_and_real_na(tmp_path: Path, eff: FakeEffects, monkeypatch: pytest.MonkeyPatch) -> None:
    """Numbers print as R pastes them, the string "NA" as NA, a real NA as blanks; widths follow the titles."""
    run = tmp_path / "run"
    run.mkdir()
    cells = sanity_cells(
        summationErrors=7,
        rangeErrors="NA",
        fixErrors=2443,
        missingProjVars=None,
        projSummationErrors=31,
        projSummationErrorsRegional=0,
    )
    stub = StubStatus({str(run): table("run", jobInSLURM="no", **cells)})
    monkeypatch.setattr(sanity, "get_run_status", stub)
    out = capture([str(run)], eff)
    expected_row = "run            " + "   " + "7     " + "   " + "NA      " + "   " + "2443     " + "   "
    expected_row += " " * 10 + "   " + "31        " + "   " + "0            " + "\n"
    assert out == header(15) + expected_row
    assert stub.calls == [(str(run), {"effects": eff})]  # getRunStatus(i): R's defaults for sort, user, detailed


def test_row_from_the_r_golden(tmp_path: Path, eff: FakeEffects, monkeypatch: pytest.MonkeyPatch) -> None:
    """The testOneRegi row of migration/goldens/sanity/amt-explicit--local.out, verbatim (folder width 46)."""
    widest = tmp_path / "SSP2-NPi2025-calibrate-AMT_2026-09-28_10.27.04"
    widest.mkdir()
    cells = sanity_cells(
        summationErrors=7,
        rangeErrors=0,
        fixErrors=0,
        missingProjVars=None,
        projSummationErrors=None,
        projSummationErrorsRegional=None,
    )
    monkeypatch.setattr(sanity, "get_run_status", StubStatus(default=table("testOneRegi", **cells)))
    out = capture([str(widest)], eff)
    golden_row = (
        "testOneRegi                                      7        0          0"
        "                                                  \n"  # 50 blanks: three real-NA cells and their separators
    )
    assert len(golden_row) == 121
    assert out == header(46) + golden_row


def test_all_six_columns_are_required(tmp_path: Path, eff: FakeEffects, monkeypatch: pytest.MonkeyPatch) -> None:
    run = tmp_path / "run"
    run.mkdir()
    cells = sanity_cells()
    del cells["projSummationErrorsRegional"]
    monkeypatch.setattr(sanity, "get_run_status", StubStatus({str(run): table("run", **cells)}))
    assert capture([str(run)], eff) == header(15)


def test_folder_width_is_clamped_between_15_and_67(
    tmp_path: Path, eff: FakeEffects, monkeypatch: pytest.MonkeyPatch
) -> None:
    """len <- min(67, max(15, nchar(basename(...)))): the Folder title is padded to it and long names are cut."""
    long_name = "x" * 80
    short, long_run = tmp_path / "ab", tmp_path / long_name
    short.mkdir()
    long_run.mkdir()
    stub = StubStatus(default=table("ignored"))
    monkeypatch.setattr(sanity, "get_run_status", stub)
    assert capture([str(short)], eff) == header(15)
    assert capture([str(short), str(long_run)], eff) == header(67)
    # a row for the long name is cut to 67 characters by printOutput
    rows = {str(long_run): table(long_name, **sanity_cells())}
    monkeypatch.setattr(sanity, "get_run_status", StubStatus(rows))
    out = capture([str(long_run)], eff)
    zeros = "   0" + " " * 8 + "0" + " " * 10 + "0" + " " * 11 + "0" + " " * 12 + "0" + " " * 12 + "0" + " " * 12
    assert out == header(67) + "x" * 67 + zeros + "\n"


def test_basename_of_the_real_path_decides_the_width(
    tmp_path: Path, eff: FakeEffects, monkeypatch: pytest.MonkeyPatch
) -> None:
    """normalizePath() resolves symlinks and drops trailing slashes before basename()."""
    target = tmp_path / "a-rather-long-directory-name"
    target.mkdir()
    link = tmp_path / "ln"
    link.symlink_to(target)
    monkeypatch.setattr(sanity, "get_run_status", StubStatus())
    assert capture([f"{link}/"], eff) == header(len("a-rather-long-directory-name"))
    assert capture([f"{target}/"], eff) == header(len("a-rather-long-directory-name"))


# --------------------------------------------------------------------------- errors


def test_missing_directory_is_the_normalize_path_error(tmp_path: Path, eff: FakeEffects) -> None:
    """normalizePath(mustWork = TRUE) stops at the first missing element, before anything is printed."""
    present = tmp_path / "present"
    present.mkdir()
    buf = io.StringIO()
    with pytest.raises(RParityError) as info:
        get_sanity_checks([str(present), str(tmp_path / "does/not/exist"), "/also/missing"], effects=eff, out=buf)
    assert str(info.value) == f'path[2]="{tmp_path}/does/not/exist": No such file or directory'
    assert buf.getvalue() == ""


def test_missing_directory_error_reports_the_expanded_path(
    eff: FakeEffects, monkeypatch: pytest.MonkeyPatch, tmp_path: Path
) -> None:
    monkeypatch.setenv("HOME", str(tmp_path))
    with pytest.raises(RParityError) as info:
        capture(["~/nope-xyz"], eff)
    assert str(info.value) == f'path[1]="{tmp_path}/nope-xyz": No such file or directory'


# --------------------------------------------------------------------------- dirs = NULL: the AMT path


def test_default_dirs_are_the_amt_runs_matching_the_runcode(
    tmp_path: Path, eff: FakeEffects, monkeypatch: pytest.MonkeyPatch
) -> None:
    """Results from <amtPath> first, then dir(amtPath, pattern = readRDS(runcode.rds), full.names = TRUE)."""
    amt = tmp_path / "amt"
    output = amt / "output"
    for name in (
        "SSP2-NPi2025-calibrate-AMT_2026-09-28_10.27.04",
        "SSP2-NPi-AMT_2026-09-28_10.30.27",
        "default-AMT_2026-09-28_13.23.51",
        "SSP2-NPi-AMT_2026-08-28_22.06.57",  # another date: not matched
        "archive",
        ".hidden-AMT_2026-09-28",  # dot-file: never listed
    ):
        (output / name).mkdir(parents=True)
    (output / "notes-AMT_2026-09-29.txt").touch()  # a file: listed by dir() like a directory
    write_rds(amt / "runcode.rds", ".*-AMT_2026-09-28|.*-AMT_2026-09-29")
    amt_path = f"{output}/"  # the R constant ends with a slash
    monkeypatch.setattr(sanity, "AMT_PATH", amt_path)
    monkeypatch.setattr(sanity, "AMT_RUNCODE", str(amt / "runcode.rds"))
    stub = StubStatus()
    monkeypatch.setattr(sanity, "get_run_status", stub)

    out = capture(None, eff)

    assert out == f"Results from {amt_path} \n" + header(46)
    # dir() order is R's collation (punctuation before digits: SSP2-NPi- before SSP2-NPi2), names joined with "/"
    assert [path for path, _ in stub.calls] == [
        f"{amt_path}/default-AMT_2026-09-28_13.23.51",
        f"{amt_path}/notes-AMT_2026-09-29.txt",
        f"{amt_path}/SSP2-NPi-AMT_2026-09-28_10.30.27",
        f"{amt_path}/SSP2-NPi2025-calibrate-AMT_2026-09-28_10.27.04",
    ]


def test_default_dirs_without_a_runcode_file(tmp_path: Path, eff: FakeEffects, monkeypatch: pytest.MonkeyPatch) -> None:
    """Off the cluster readRDS fails after the Results line was printed (golden sanity/default--local)."""
    monkeypatch.setattr(sanity, "AMT_RUNCODE", str(tmp_path / "runcode.rds"))
    buf = io.StringIO()
    with pytest.raises(RParityError) as info:
        get_sanity_checks(None, effects=eff, out=buf)
    assert str(info.value) == "cannot open the connection"
    assert buf.getvalue() == f"Results from {AMT_PATH} \n"


def test_default_dirs_with_no_match_prints_the_header_only(
    tmp_path: Path, eff: FakeEffects, monkeypatch: pytest.MonkeyPatch
) -> None:
    output = tmp_path / "output"
    (output / "unrelated").mkdir(parents=True)
    write_rds(tmp_path / "runcode.rds", ".*-AMT_2099-01-01")
    monkeypatch.setattr(sanity, "AMT_PATH", f"{output}/")
    monkeypatch.setattr(sanity, "AMT_RUNCODE", str(tmp_path / "runcode.rds"))
    assert capture(None, eff) == f"Results from {output}/ \n" + header(15)


def test_r_dir_pattern(tmp_path: Path, eff: FakeEffects) -> None:
    d = tmp_path / "d"
    d.mkdir()
    (d / "a-AMT_2026-09-28").touch()
    (d / ".hidden-AMT_2026-09-28").touch()
    (d / "b-AMT_2026-09-29_x").mkdir()
    (d / "other").mkdir()
    pattern = ".*-AMT_2026-09-28|.*-AMT_2026-09-29"
    assert r_dir_pattern(f"{d}/", pattern, eff) == [f"{d}//a-AMT_2026-09-28", f"{d}//b-AMT_2026-09-29_x"]
    assert r_dir_pattern(str(d), pattern, eff) == [f"{d}/a-AMT_2026-09-28", f"{d}/b-AMT_2026-09-29_x"]
    assert r_dir_pattern(str(d), None, eff) == [f"{d}/a-AMT_2026-09-28", f"{d}/b-AMT_2026-09-29_x", f"{d}/other"]
    assert r_dir_pattern(str(d / "a-AMT_2026-09-28"), "x", eff) == []
    assert r_dir_pattern(str(tmp_path / "nope"), "x", eff) == []
    with pytest.raises(RParityError, match=r"^invalid 'pattern' regular expression$"):
        r_dir_pattern(str(d), "[", eff)


# --------------------------------------------------------------------------- colours


def test_without_colours_the_header_has_no_escapes(tmp_path: Path, eff: FakeEffects) -> None:
    """crayon on a pipe (no R_CLI_NUM_COLORS): the same text without sequences, the trailing space kept."""
    bare = tmp_path / "x"
    bare.mkdir()
    colors.set_enabled(False)
    assert capture([str(bare)], eff) == "\n" + EXPLANATION + "Folder           " + TITLES + " \n\n"
