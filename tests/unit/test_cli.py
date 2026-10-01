"""``modelstats.cli``: the ``rs`` command line against ``R/commandLineInterface.R``.

The parser is exercised through typer's ``CliRunner`` (bundled flags, one positional, usage errors, help); the
decision tree through :func:`modelstats.cli.run_cli` with ``FakeEffects`` on a temporary tree, with ``loop_runs``
and ``get_sanity_checks`` replaced by recorders (their own behaviour is pinned in their own tests). What the R
function prints is embedded verbatim from the rs goldens' ``.err`` files where a golden pins it (``migration/
goldens/rs/*.err``: ``current--failing``, ``amt--local``, ``current--regex-bracket``, ``synthetic-bug005``).

R facts pinned here (R 4.6.1, 2026-10-01): ``list.dirs(recursive = FALSE)`` includes dot-directories and symbolic
links to directories, pastes ``<dir>/<name>`` (``x/`` -> ``x//a``) and gives ``character(0)`` for a file or a missing
path; ``strsplit("", ",")[[1]]`` is ``character(0)``; ``gms::chooseFromList`` returns ``character(0)`` for an empty
selection, which ``loopRuns`` turns into the visible ``"No runs found"`` (printed by Rscript as ``[1] "No runs
found"``); the top-level error layout (``Error in <call> : <msg>``, ``Calls:``, ``In addition:``,
``Execution halted``).
"""

from __future__ import annotations

import io
import os
import sys
import warnings
from collections.abc import Iterator
from pathlib import Path
from typing import TextIO

import pytest
from typer.testing import CliRunner

import modelstats.cli as cli
from modelstats import colors
from modelstats.cli import HINTS, OPTION_FLAGS, Options, _Alert, app, list_dirs, main, run_cli
from modelstats.errors import RParityError, RWarning
from modelstats.rdata_io import write_rds
from modelstats.slurm import SQUEUE_ALL_ARGV
from unit._fake_effects import FakeEffects

UPDATE_LINES = [
    "Update 1: The underline has been removed for converged runs (green).",
    "Update 2: Runs that showed INFES but finally converged are now displayed in the same way "
    "(now green, previously blue).",
]
NO_CURRENT = "No currently running runs found. To include recent runs please expand the time horizon by adding -d DAYS."


# ---------------------------------------------------------------------------
# fixtures
# ---------------------------------------------------------------------------


@pytest.fixture(autouse=True)
def _plain_environment(monkeypatch: pytest.MonkeyPatch) -> Iterator[None]:
    """No colours unless a test asks for them; the process-wide flag is reset afterwards."""
    monkeypatch.delenv("R_CLI_NUM_COLORS", raising=False)
    monkeypatch.delenv("NO_COLOR", raising=False)
    monkeypatch.delenv("COLORTERM", raising=False)
    yield
    colors.set_enabled(False, 256)


@pytest.fixture
def effects() -> FakeEffects:
    return FakeEffects(user="pascalfu", on_cluster=False)


class Recorder:
    """A stand-in for ``loop_runs`` / ``get_sanity_checks`` that records its arguments."""

    def __init__(self) -> None:
        self.calls: list[dict[str, object]] = []

    def loop_runs(
        self,
        mydir: list[str],
        user: str | None = None,
        colors: bool = True,
        sortbytime: bool = True,
        effects: object = None,
        out: TextIO | None = None,
    ) -> str | None:
        self.calls.append({"mydir": list(mydir), "user": user, "colors": colors, "sortbytime": sortbytime})
        if len(mydir) == 0:
            return "No runs found"
        if mydir[0] == "exit":
            return None
        assert out is not None
        out.write("TABLE\n")
        return None

    def sanity(self, dirs: list[str] | None, effects: object = None, out: TextIO | None = None) -> None:
        self.calls.append({"dirs": None if dirs is None else list(dirs)})
        assert out is not None
        out.write("SANITY\n")


@pytest.fixture
def recorder(monkeypatch: pytest.MonkeyPatch) -> Recorder:
    rec = Recorder()
    monkeypatch.setattr(cli, "loop_runs", rec.loop_runs)
    monkeypatch.setattr(cli, "get_sanity_checks", rec.sanity)
    return rec


def make_run(path: Path, *, magpie: bool = False) -> Path:
    """A folder ``is.runfolder`` accepts (four REMIND markers, or the four MAgPIE ones)."""
    path.mkdir(parents=True, exist_ok=True)
    names = (
        ("full.gms", "submit.R", "config.yml", "magpie_y1995.gdx")
        if magpie
        else ("full.gms", "log.txt", "config.Rdata", "prepare_and_run.R")
    )
    for name in names:
        (path / name).touch()
    return path


def make_main(path: Path, runs: tuple[str, ...]) -> Path:
    """A folder ``is.mainfolder`` accepts, with run folders under ``output/``."""
    path.mkdir(parents=True, exist_ok=True)
    for name in ("output.R", "start.R", "main.gms"):
        (path / name).touch()
    (path / "output").mkdir(exist_ok=True)
    for run in runs:
        make_run(path / "output" / run)
    return path


def alerts(captured: str) -> list[str]:
    """The stderr lines after the two update lines and the hint (which are asserted here once)."""
    lines = captured.splitlines()
    assert lines[:2] == [f"ℹ {text}" for text in UPDATE_LINES]
    assert lines[2].startswith("ℹ Did you know? ")
    assert lines[2].removeprefix("ℹ Did you know? ") in HINTS
    return lines[3:]


def run(opt: Options, effects: FakeEffects, capsys: pytest.CaptureFixture[str]) -> tuple[int, str, list[str]]:
    """``run_cli`` with captured streams: (status, stdout, stderr lines after the three alerts)."""
    status = run_cli(opt, effects=effects)
    captured = capsys.readouterr()
    return status, captured.out, alerts(captured.err)


# ---------------------------------------------------------------------------
# the parser (typer): bundling, positional, usage errors, help
# ---------------------------------------------------------------------------


def invoke_parse(monkeypatch: pytest.MonkeyPatch, argv: list[str]) -> tuple[int, Options | None, str, str]:
    seen: list[Options] = []

    def capture(opt: Options, effects: object = None) -> int:
        seen.append(opt)
        return 0

    monkeypatch.setattr(cli, "run_cli", capture)
    result = CliRunner().invoke(app, argv)
    return result.exit_code, (seen[0] if seen else None), result.stdout, result.stderr


@pytest.mark.parametrize(
    ("argv", "expected"),
    [
        ([], Options()),
        (["-mltpbf", "PkBudg"], Options(magpie=True, last=True, time=True, prompt=True, nocolor=True, filter="PkBudg")),
        (["-mltpbfPkBudg"], Options(magpie=True, last=True, time=True, prompt=True, nocolor=True, filter="PkBudg")),
        (
            ["/p/x/remind", "-mltpbf", "PkBudg"],
            Options(paths="/p/x/remind", magpie=True, last=True, time=True, prompt=True, nocolor=True, filter="PkBudg"),
        ),
        (["-As"], Options(amt=True, sanity=True)),
        (["-Ab"], Options(amt=True, nocolor=True)),
        (["-C", "-u", "alf", "-d", "5"], Options(current=True, user="alf", daysback=5)),
        (["-A", "-f", "SSP2EU-Base"], Options(amt=True, filter="SSP2EU-Base")),
        (["folder1,folder2", "-f", "PkBudg500,EU21"], Options(paths="folder1,folder2", filter="PkBudg500,EU21")),
        (["-bt", "."], Options(nocolor=True, time=True, paths=".")),
        (["-fPkBudg"], Options(filter="PkBudg")),
        (["--filter=abc", "--user", "bob", "--daysback", "3"], Options(filter="abc", user="bob", daysback=3)),
        (["-d", "-1", "-C"], Options(daysback=-1, current=True)),
        (["--found-in-slurm", "/p/run"], Options(found_in_slurm="/p/run")),
    ],
)
def test_parser_matches_optparse(monkeypatch: pytest.MonkeyPatch, argv: list[str], expected: Options) -> None:
    status, opt, out, err = invoke_parse(monkeypatch, argv)
    assert status == 0, err
    assert opt == expected


@pytest.mark.parametrize(
    ("argv", "message"),
    [
        (["-z"], "No such option: -z"),
        (["--bogus"], "No such option: --bogus"),
        (["a", "b"], "Got unexpected extra argument(s) (b)"),
        (["-C", "-d", "x"], "Invalid value for '-d' / '--daysback': 'x' is not a valid int."),
        (["-f"], "Option '-f' requires an argument."),
    ],
)
def test_usage_errors_exit_2_in_typer_and_print_nothing_on_stdout(
    monkeypatch: pytest.MonkeyPatch, argv: list[str], message: str
) -> None:
    status, opt, out, err = invoke_parse(monkeypatch, argv)
    assert status == 2
    assert opt is None  # the command body never ran
    assert out == ""
    assert f"Error: {message}" in err


def test_main_maps_typer_usage_errors_to_exit_status_1(
    monkeypatch: pytest.MonkeyPatch, capsys: pytest.CaptureFixture[str]
) -> None:
    """D-12 pending: R's optparse error is an Rscript error (status 1); typer's 2 is mapped to that."""
    monkeypatch.setattr(sys, "argv", ["rs", "-z"])
    with pytest.raises(SystemExit) as info:
        main()
    assert info.value.code == 1
    captured = capsys.readouterr()
    assert captured.out == ""
    assert "Error: No such option: -z" in captured.err


def test_main_keeps_exit_status_0_of_help(monkeypatch: pytest.MonkeyPatch, capsys: pytest.CaptureFixture[str]) -> None:
    monkeypatch.setattr(sys, "argv", ["rs", "-h"])
    with pytest.raises(SystemExit) as info:
        main()
    assert info.value.code == 0
    assert "Usage: rs [OPTION] [PATH]" in capsys.readouterr().out


def test_main_passes_r_errors_as_exit_status_1(
    monkeypatch: pytest.MonkeyPatch, capsys: pytest.CaptureFixture[str]
) -> None:
    monkeypatch.setattr(sys, "argv", ["rs", "-x"])
    monkeypatch.setattr(cli, "run_cli", lambda opt, effects=None: 1)
    with pytest.raises(SystemExit) as info:
        main()
    assert info.value.code == 1


@pytest.mark.parametrize("flag", ["-h", "--help"])
def test_help_lists_every_option_of_the_contract(flag: str) -> None:
    """D-11: the layout is typer's; every option of plan 03 section 3.4 (and the help alias) is listed."""
    result = CliRunner().invoke(app, [flag])
    assert result.exit_code == 0
    assert result.stderr == ""
    for short, long in OPTION_FLAGS:
        assert short in result.stdout and long in result.stdout, (short, long)
    assert "Usage: rs [OPTION] [PATH]" in result.stdout
    assert "Bugs and feedback: https://github.com/pik-piam/modelstats/issues" in result.stdout
    # the preformatted blocks keep their line breaks
    assert "    -u, -d only with -C\n    -l     only with -m\n" in result.stdout
    assert "  2. rs -As\n" in result.stdout


# ---------------------------------------------------------------------------
# list.dirs(recursive = FALSE)
# ---------------------------------------------------------------------------


def test_list_dirs_like_r(tmp_path: Path, effects: FakeEffects) -> None:
    x = tmp_path / "x"
    for name in (".hidden", "b", "a", "A"):
        (x / name).mkdir(parents=True)
    (x / "file.txt").touch()
    (tmp_path / "y").mkdir()
    os.symlink("../y", x / "link")
    os.symlink("../nowhere", x / "dangling")
    expected = [f"{x}/{name}" for name in (".hidden", "a", "A", "b", "link")]
    assert list_dirs(str(x), effects) == expected
    assert list_dirs(f"{x}/", effects) == [f"{x}//{name}" for name in (".hidden", "a", "A", "b", "link")]
    assert list_dirs(f"{x}/file.txt", effects) == []
    assert list_dirs(f"{tmp_path}/nope", effects) == []
    assert list_dirs("", effects) == []


def test_list_dirs_with_glob_characters_in_the_path(tmp_path: Path, effects: FakeEffects) -> None:
    x = tmp_path / "a[1]*"
    (x / "run").mkdir(parents=True)
    assert list_dirs(str(x), effects) == [f"{x}/run"]


# ---------------------------------------------------------------------------
# cli_alert_info / cli_alert_warning
# ---------------------------------------------------------------------------


def test_alert_bytes_with_colours() -> None:
    stream = io.StringIO()
    alert = _Alert(stream, colour=True)
    alert.info("Runs found: 5")
    alert.warning("No runs found")
    assert stream.getvalue() == "\x1b[36mℹ\x1b[39m Runs found: 5\n\x1b[33m!\x1b[39m No runs found\n"
    paths = ["/p/projects/remind/modeltests/remind/output/", "/p/projects/remind/modeltests/remind/output/archive"]
    assert alert.files(paths[:1]) == "\x1b[34m\x1b[34m/p/projects/remind/modeltests/remind/output/\x1b[34m\x1b[39m"
    assert alert.files(paths) == (
        "\x1b[34m\x1b[34m/p/projects/remind/modeltests/remind/output/\x1b[34m\x1b[39m and "
        "\x1b[34m\x1b[34m/p/projects/remind/modeltests/remind/output/archive\x1b[34m\x1b[39m"
    )


def test_alert_bytes_without_colours() -> None:
    stream = io.StringIO()
    alert = _Alert(stream, colour=False)
    alert.info("Runs found: 5")
    alert.warning("No runs found")
    assert stream.getvalue() == "ℹ Runs found: 5\n! No runs found\n"
    assert alert.files(["/p/a/", "/p/b"]) == "'/p/a/' and '/p/b'"
    assert alert.files(["/a", "/b", "/c"]) == "'/a', '/b', and '/c'"
    assert alert.files([]) == ""


def test_alert_symbol_outside_utf8() -> None:
    stream = io.TextIOWrapper(io.BytesIO(), encoding="latin-1")
    alert = _Alert(stream, colour=False)
    alert.info("x")
    stream.flush()
    assert stream.buffer.getvalue() == b"i x\n"  # type: ignore[attr-defined]


# ---------------------------------------------------------------------------
# option B: run folders from the paths
# ---------------------------------------------------------------------------


def test_cwd_container_lists_its_subdirectories(
    tmp_path: Path,
    monkeypatch: pytest.MonkeyPatch,
    effects: FakeEffects,
    recorder: Recorder,
    capsys: pytest.CaptureFixture[str],
) -> None:
    for name in ("b", "a", "run-rem-10", "run-rem-2", ".git"):
        (tmp_path / name).mkdir()
    (tmp_path / "file").touch()
    monkeypatch.chdir(tmp_path)
    status, out, err = run(Options(), effects, capsys)
    assert status == 0
    assert out == "TABLE\n"
    assert err == ["ℹ Runs found: 5"]
    assert recorder.calls == [
        {
            "mydir": ["./.git", "./a", "./b", "./run-rem-2", "./run-rem-10"],
            "user": "pascalfu",
            "colors": True,
            "sortbytime": False,
        }
    ]


def test_main_folder_lists_output(
    tmp_path: Path,
    monkeypatch: pytest.MonkeyPatch,
    effects: FakeEffects,
    recorder: Recorder,
    capsys: pytest.CaptureFixture[str],
) -> None:
    main_dir = make_main(tmp_path / "remind", ("x", "y"))
    status, out, err = run(Options(paths=str(main_dir), time=True, nocolor=True), effects, capsys)
    assert status == 0
    assert recorder.calls[0]["mydir"] == [f"{main_dir}/output/x", f"{main_dir}/output/y"]
    assert recorder.calls[0]["colors"] is False
    assert recorder.calls[0]["sortbytime"] is True
    assert err == ["ℹ Runs found: 2"]


def test_run_folder_is_taken_as_is_and_paths_are_comma_separated(
    tmp_path: Path, effects: FakeEffects, recorder: Recorder, capsys: pytest.CaptureFixture[str]
) -> None:
    a = make_run(tmp_path / "a")
    b = make_run(tmp_path / "b", magpie=True)
    container = tmp_path / "c"
    (container / "z").mkdir(parents=True)
    status, out, err = run(Options(paths=f"{a},{b},{container}"), effects, capsys)
    assert status == 0
    assert recorder.calls[0]["mydir"] == [str(a), str(b), f"{container}/z"]


def test_early_failed_run_is_a_container_and_yields_no_runs(
    tmp_path: Path, effects: FakeEffects, recorder: Recorder, capsys: pytest.CaptureFixture[str]
) -> None:
    """BUG-027: a run without full.gms is not a run folder; its subdirectories (none) are listed."""
    run_dir = tmp_path / "early"
    run_dir.mkdir()
    for name in ("log.txt", "config.Rdata", "prepare_and_run.R"):
        (run_dir / name).touch()
    status, out, err = run(Options(paths=str(run_dir)), effects, capsys)
    assert status == 0
    assert out == ""
    assert err == ["! No runs found"]
    assert recorder.calls == []


def test_nonexistent_path_yields_no_runs(
    effects: FakeEffects, recorder: Recorder, capsys: pytest.CaptureFixture[str]
) -> None:
    status, out, err = run(Options(paths="/p/projects/does/not/exist"), effects, capsys)
    assert (status, out, err) == (0, "", ["! No runs found"])


def test_empty_path_argument_has_no_paths_at_all(
    tmp_path: Path,
    monkeypatch: pytest.MonkeyPatch,
    effects: FakeEffects,
    recorder: Recorder,
    capsys: pytest.CaptureFixture[str],
) -> None:
    """strsplit("", ",")[[1]] is character(0): runfolders stays NULL, -ml would still say 'No runs found'."""
    (tmp_path / "C_x-rem-1").mkdir()
    monkeypatch.chdir(tmp_path)
    status, out, err = run(Options(paths="", magpie=True, last=True), effects, capsys)
    assert (status, out, err) == (0, "", ["! No runs found"])


# ---------------------------------------------------------------------------
# -f, ordering, the > 40 hint
# ---------------------------------------------------------------------------


def test_filter_is_joined_with_a_bar_and_applied_to_the_whole_path(
    tmp_path: Path,
    monkeypatch: pytest.MonkeyPatch,
    effects: FakeEffects,
    recorder: Recorder,
    capsys: pytest.CaptureFixture[str],
) -> None:
    for name in ("NPi", "PkBudg650", "calibrate", "other"):
        (tmp_path / name).mkdir()
    monkeypatch.chdir(tmp_path)
    status, out, err = run(Options(filter="PkBudg650,calibrate"), effects, capsys)
    assert recorder.calls[0]["mydir"] == ["./PkBudg650", "./calibrate"]
    assert err == ["ℹ Runs found: 2"]
    recorder.calls.clear()
    status, out, err = run(Options(filter="zzz"), effects, capsys)
    assert (status, out, err) == (0, "", ["! No runs found"])
    assert recorder.calls == []


def test_invalid_filter_is_an_r_error(
    tmp_path: Path,
    monkeypatch: pytest.MonkeyPatch,
    effects: FakeEffects,
    recorder: Recorder,
    capsys: pytest.CaptureFixture[str],
) -> None:
    (tmp_path / "a").mkdir()
    monkeypatch.chdir(tmp_path)
    status, out, err = run(Options(filter="x["), effects, capsys)
    assert status == 1
    assert out == ""
    assert err == [
        "Error in grep(opt$filter, runfolders, value = TRUE) : ",
        "  invalid regular expression 'x[', reason 'Missing ']''",
        "Calls: commandLineInterface -> grep",
        "In addition: Warning message:",
        "In grep(opt$filter, runfolders, value = TRUE) :",
        "  TRE pattern compilation error 'Missing ']''",
        "Execution halted",
    ]


def test_natural_order_of_the_basenames(
    tmp_path: Path, effects: FakeEffects, recorder: Recorder, capsys: pytest.CaptureFixture[str]
) -> None:
    names = ("x-10", "x-9", "x-1", "a2", "a10", "B", "b")
    for name in names:
        make_run(tmp_path / name)
    status, out, err = run(Options(paths=",".join(str(tmp_path / n) for n in names)), effects, capsys)
    # ICU en_US_POSIX (D-14): digits, A-Z, then a-z at the primary level, embedded numbers numerically
    assert [os.path.basename(p) for p in recorder.calls[0]["mydir"]] == [  # type: ignore[union-attr]
        "B",
        "a2",
        "a10",
        "b",
        "x-1",
        "x-9",
        "x-10",
    ]


def test_more_than_forty_runs_print_the_filter_hint(
    tmp_path: Path,
    monkeypatch: pytest.MonkeyPatch,
    effects: FakeEffects,
    recorder: Recorder,
    capsys: pytest.CaptureFixture[str],
) -> None:
    for i in range(41):
        (tmp_path / f"run{i:02d}").mkdir()
    monkeypatch.chdir(tmp_path)
    status, out, err = run(Options(), effects, capsys)
    assert err == [
        "ℹ To reduce the number of runs, filter the runs with -f REGEX or select manually from the list with -p.",
        "ℹ Runs found: 41",
    ]
    status, out, err = run(Options(filter="run"), effects, capsys)
    assert err == ["ℹ Runs found: 41"]  # only with the default filter and without -p


# ---------------------------------------------------------------------------
# -m / -l
# ---------------------------------------------------------------------------


def test_magpie_keeps_coupling_iterations_and_last_keeps_the_last_in_listing_order(
    tmp_path: Path,
    monkeypatch: pytest.MonkeyPatch,
    effects: FakeEffects,
    recorder: Recorder,
    capsys: pytest.CaptureFixture[str],
) -> None:
    for name in ("C_a", "x-rem-1", "x-rem-10", "x-rem-2", "plain"):
        (tmp_path / name).mkdir()
    monkeypatch.chdir(tmp_path)
    run(Options(magpie=True), effects, capsys)
    assert recorder.calls[-1]["mydir"] == ["./C_a", "./x-rem-1", "./x-rem-2", "./x-rem-10"]
    run(Options(magpie=True, last=True), effects, capsys)
    # BUG-025: the entry at the largest listing position per prefix (x-rem-2 in ICU order 1, 10, 2), then sorted
    assert recorder.calls[-1]["mydir"] == ["./C_a", "./x-rem-2"]


def test_magpie_without_coupled_runs(
    tmp_path: Path,
    monkeypatch: pytest.MonkeyPatch,
    effects: FakeEffects,
    recorder: Recorder,
    capsys: pytest.CaptureFixture[str],
) -> None:
    (tmp_path / "plain").mkdir()
    monkeypatch.chdir(tmp_path)
    status, out, err = run(Options(magpie=True), effects, capsys)
    assert (status, out, err) == (0, "", ["! No runs found"])  # character(0) survives the -m block
    status, out, err = run(Options(magpie=True, last=True), effects, capsys)
    assert (status, out, err) == (0, "", ["! No coupled runs found"])  # lastdirs stays NULL
    assert recorder.calls == []


# ---------------------------------------------------------------------------
# -p
# ---------------------------------------------------------------------------


def test_prompt_reads_the_selection_from_stdin(
    tmp_path: Path,
    monkeypatch: pytest.MonkeyPatch,
    effects: FakeEffects,
    recorder: Recorder,
    capsys: pytest.CaptureFixture[str],
) -> None:
    for name in ("a", "b", "c"):
        (tmp_path / name).mkdir()
    monkeypatch.chdir(tmp_path)
    monkeypatch.setattr(sys, "stdin", io.StringIO("2,4\n"))
    status, out, err = run(Options(prompt=True), effects, capsys)
    assert status == 0
    assert out == "TABLE\n"
    assert err[:3] == ["", "", "Please choose folders:"]
    assert err[-2:] == ["Selected: ./a, ./c", "ℹ Runs found: 2"]
    assert recorder.calls[0]["mydir"] == ["./a", "./c"]


def test_prompt_with_an_empty_selection_prints_the_visible_no_runs_found(
    tmp_path: Path,
    monkeypatch: pytest.MonkeyPatch,
    effects: FakeEffects,
    recorder: Recorder,
    capsys: pytest.CaptureFixture[str],
) -> None:
    """migration/goldens/rs/prompt-empty: Rscript prints loopRuns' return value ``[1] "No runs found"``."""
    (tmp_path / "a").mkdir()
    monkeypatch.chdir(tmp_path)
    monkeypatch.setattr(sys, "stdin", io.StringIO(""))
    status, out, err = run(Options(prompt=True), effects, capsys)
    assert status == 0
    assert out == '[1] "No runs found"\n'
    assert err[-2:] == ["Selected: ", "ℹ Runs found: 0"]
    assert recorder.calls == [{"mydir": [], "user": "pascalfu", "colors": True, "sortbytime": False}]


def test_prompt_with_sanity_passes_the_empty_selection_not_null(
    tmp_path: Path,
    monkeypatch: pytest.MonkeyPatch,
    effects: FakeEffects,
    recorder: Recorder,
    capsys: pytest.CaptureFixture[str],
) -> None:
    """chooseFromList returns character(0), not NULL: getSanityChecks must not fall back to the AMT lookup."""
    (tmp_path / "a").mkdir()
    monkeypatch.chdir(tmp_path)
    monkeypatch.setattr(sys, "stdin", io.StringIO("\n"))
    status, out, err = run(Options(prompt=True, sanity=True), effects, capsys)
    assert status == 0
    assert out == "SANITY\n"
    assert recorder.calls == [{"dirs": []}]


def test_first_folder_named_exit_prints_null(
    tmp_path: Path,
    monkeypatch: pytest.MonkeyPatch,
    effects: FakeEffects,
    recorder: Recorder,
    capsys: pytest.CaptureFixture[str],
) -> None:
    """``mydir[[1]] == "exit"`` compares the path as given: ``rs exit`` on a run folder of that name."""
    make_run(tmp_path / "exit")
    monkeypatch.chdir(tmp_path)
    status, out, err = run(Options(paths="exit"), effects, capsys)
    assert (status, out) == (0, "NULL\n")  # loopRuns' visible return(NULL)
    run_dir = make_run(tmp_path / "other" / "exit")
    status, out, err = run(Options(paths=str(run_dir)), effects, capsys)
    assert out == "TABLE\n"  # the full path is not "exit"


# ---------------------------------------------------------------------------
# -s
# ---------------------------------------------------------------------------


def test_sanity_gets_the_folders(
    tmp_path: Path, effects: FakeEffects, recorder: Recorder, capsys: pytest.CaptureFixture[str]
) -> None:
    a = make_run(tmp_path / "a")
    status, out, err = run(Options(paths=str(a), sanity=True), effects, capsys)
    assert (status, out, err) == (0, "SANITY\n", ["ℹ Runs found: 1"])
    assert recorder.calls == [{"dirs": [str(a)]}]


def test_sanity_normalize_path_error(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch, effects: FakeEffects, capsys: pytest.CaptureFixture[str]
) -> None:
    def failing(dirs: list[str], effects: object = None, out: TextIO | None = None) -> None:
        raise RParityError('path[1]="/nope": No such file or directory')

    monkeypatch.setattr(cli, "get_sanity_checks", failing)
    a = make_run(tmp_path / "a")
    status, out, err = run(Options(paths=str(a), sanity=True), effects, capsys)
    assert status == 1
    assert err == [
        "ℹ Runs found: 1",
        "Error in normalizePath(dirs, mustWork = TRUE) : ",  # 14 + 37 + 44 > 75: the message on its own line
        '  path[1]="/nope": No such file or directory',
        "Calls: commandLineInterface -> <Anonymous> -> normalizePath",
        "Execution halted",
    ]


# ---------------------------------------------------------------------------
# loopRuns errors and warnings
# ---------------------------------------------------------------------------


def test_bug005_abort_of_loop_runs(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch, effects: FakeEffects, capsys: pytest.CaptureFixture[str]
) -> None:
    """migration/goldens/rs/synthetic-bug005.err: the try() text, then Rscript's error block, exit 1."""

    def aborting(mydir: list[str], **kwargs: object) -> None:
        out = kwargs["out"]
        assert isinstance(out, io.TextIOBase)
        out.write("HEADER\n")
        sys.stderr.write('Error in if (runstatistics$stats[["config"]][["model_name"]] == "MAgPIE") { : \n')
        sys.stderr.write("  argument is of length zero\n")
        raise RParityError("subscript out of bounds")

    monkeypatch.setattr(cli, "loop_runs", aborting)
    a = make_run(tmp_path / "a")
    status, out, err = run(Options(paths=str(a)), effects, capsys)
    assert status == 1
    assert out == "HEADER\n"
    assert err == [
        "ℹ Runs found: 1",
        'Error in if (runstatistics$stats[["config"]][["model_name"]] == "MAgPIE") { : ',
        "  argument is of length zero",
        'Error in status[["jobInSLURM"]] : subscript out of bounds',
        "Calls: commandLineInterface -> <Anonymous>",
        "Execution halted",
    ]


def test_deferred_warnings_are_printed_at_the_end_like_rscript(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch, effects: FakeEffects, capsys: pytest.CaptureFixture[str]
) -> None:
    def warning_loop(mydir: list[str], **kwargs: object) -> None:
        warnings.warn(RWarning("normalizePath(mydir)", 'path[1]="x": No such file or directory'), stacklevel=1)
        warnings.warn(UserWarning("a Python warning is not an R warning"), stacklevel=1)

    monkeypatch.setattr(cli, "loop_runs", warning_loop)
    a = make_run(tmp_path / "a")
    status, out, err = run(Options(paths=str(a)), effects, capsys)
    assert status == 0
    assert err == [
        "ℹ Runs found: 1",
        "Warning message:",
        'In normalizePath(mydir) : path[1]="x": No such file or directory',
    ]


# ---------------------------------------------------------------------------
# -C
# ---------------------------------------------------------------------------


def squeue_tables(user: str, workdirs: str, names: str) -> dict[tuple[str, ...], tuple[str, int]]:
    return {
        ("squeue", "-u", user, "-h", "-o", "%Z"): (workdirs, 0),
        ("squeue", "-u", user, "-h", "-o", "%j"): (names, 0),
    }


def test_current_lists_the_existing_job_directories(
    tmp_path: Path, recorder: Recorder, capsys: pytest.CaptureFixture[str]
) -> None:
    b = make_run(tmp_path / "b")
    a = make_run(tmp_path / "a")
    eff = FakeEffects(run_table=squeue_tables("pascalfu", f"{b}\n{a}\n{tmp_path}/gone\n", "b\na\ngone\n"))
    status, out, err = run(Options(current=True), eff, capsys)
    assert status == 0
    assert err == ["ℹ Runs found: 2"]
    assert recorder.calls[0]["mydir"] == [str(a), str(b)]
    assert recorder.calls[0]["user"] == "pascalfu"


def test_current_with_another_user(tmp_path: Path, recorder: Recorder, capsys: pytest.CaptureFixture[str]) -> None:
    a = make_run(tmp_path / "a")
    eff = FakeEffects(run_table=squeue_tables("alice", f"{a}\n", "a\n"))
    run(Options(current=True, user="alice"), eff, capsys)
    assert recorder.calls[0] == {"mydir": [str(a)], "user": "alice", "colors": True, "sortbytime": False}


def test_current_without_jobs(effects: FakeEffects, recorder: Recorder, capsys: pytest.CaptureFixture[str]) -> None:
    status, out, err = run(Options(current=True), effects, capsys)
    assert (status, out, err) == (0, "", [f"! {NO_CURRENT}"])
    status, out, err = run(Options(current=True, daysback=2), effects, capsys)
    assert (status, out, err) == (0, "", ["! No runs found in the past 2 days. Try to expand the time horizon."])
    assert recorder.calls == []


def test_current_with_a_failing_squeue_warns_like_rscript(
    recorder: Recorder, capsys: pytest.CaptureFixture[str]
) -> None:
    """migration/goldens/rs/current--failing.err: the child's stderr at call time, the alert, then the warnings."""
    eff = FakeEffects(
        run_table={
            ("squeue", "-u", "pascalfu", "-h", "-o", "%Z"): ("", 1),
            ("squeue", "-u", "pascalfu", "-h", "-o", "%j"): ("", 1),
        }
    )
    status, out, err = run(Options(current=True), eff, capsys)
    assert status == 0
    assert err == [
        f"! {NO_CURRENT}",
        "Warning messages:",
        '1: In system(paste0("squeue -u ", opt$user, " -h -o \'%Z\'"), intern = TRUE) :',
        "  running command 'squeue -u pascalfu -h -o '%Z'' had status 1",
        '2: In system(paste0("squeue -u ", opt$user, " -h -o \'%j\'"), intern = TRUE) :',
        "  running command 'squeue -u pascalfu -h -o '%j'' had status 1",
    ]


def test_current_with_an_invalid_job_name_regex_is_an_r_error(
    tmp_path: Path, recorder: Recorder, capsys: pytest.CaptureFixture[str]
) -> None:
    """migration/goldens/rs/current--regex-bracket.err (BUG-008): Rscript's error block, exit 1."""
    eff = FakeEffects(run_table=squeue_tables("pascalfu", f"{tmp_path}\n", "C_SSP2[NDC\n"))
    status, out, err = run(Options(current=True), eff, capsys)
    assert status == 1
    assert out == ""
    assert err == [
        "Error in grepl(runnames[[i]], myruns[[i]]) : ",
        "  invalid regular expression 'C_SSP2[NDC', reason 'Missing ']''",
        "Calls: commandLineInterface -> grepl",
        "In addition: Warning message:",
        "In grepl(runnames[[i]], myruns[[i]]) :",
        "  TRE pattern compilation error 'Missing ']''",
        "Execution halted",
    ]


def test_current_with_amt_queries_without_a_user(
    amt_tree: Path, recorder: Recorder, capsys: pytest.CaptureFixture[str]
) -> None:
    """-A -C (rs golden amt--current): the runcode is read first, then opt$user is NULL so paste0 drops it from
    the command (``squeue -u  -h ...``; the shell drops the empty word, current_runs gets ""); -C wins."""
    eff = FakeEffects(
        run_table={("squeue", "-u", "", "-h", "-o", "%Z"): ("", 0), ("squeue", "-u", "", "-h", "-o", "%j"): ("", 0)}
    )
    status, out, err = run(Options(amt=True, current=True), eff, capsys)
    assert status == 0
    assert err == [f"ℹ Results from '{amt_tree}/'", f"! {NO_CURRENT}"]
    assert recorder.calls == []
    assert eff.runs == [("squeue", "-u", "", "-h", "-o", "%Z"), ("squeue", "-u", "", "-h", "-o", "%j")]


# ---------------------------------------------------------------------------
# -A
# ---------------------------------------------------------------------------


def test_amt_without_the_runcode_file_fails_like_readrds(
    effects: FakeEffects, recorder: Recorder, capsys: pytest.CaptureFixture[str]
) -> None:
    """migration/goldens/rs/amt--local.err: readRDS on a missing file; gzfile's warning in the In addition block."""
    status, out, err = run(Options(amt=True), effects, capsys)
    assert status == 1
    assert out == ""
    assert err == [
        'Error in gzfile(file, "rb") : cannot open the connection',
        "Calls: commandLineInterface -> readRDS -> gzfile",
        "In addition: Warning message:",
        'In gzfile(file, "rb") :',
        "  cannot open compressed file '/p/projects/remind/modeltests/remind/runcode.rds', "
        "probable reason 'No such file or directory'",
        "Execution halted",
    ]
    assert recorder.calls == []


@pytest.fixture
def amt_tree(tmp_path: Path, monkeypatch: pytest.MonkeyPatch) -> Path:
    """The AMT output tree relocated to a temporary directory (the R paths are constants)."""
    output = tmp_path / "output"
    for name in ("SSP2-NPi-AMT_2026-09-28_10.30.27", "SSP2-NPi-AMT_2026-08-28_22.06.57", "testOneRegi", "archive"):
        (output / name).mkdir(parents=True)
    (output / "archive" / "SSP2-PkBudg-AMT_2025-07-01_10.00.00").mkdir()
    write_rds(tmp_path / "runcode.rds", ".*-AMT_2026-09-28|.*-AMT_2026-09-29")
    monkeypatch.setattr(cli, "AMT_PATH", f"{output}/")
    monkeypatch.setattr(cli, "AMT_ARCHIVE", f"{output}/archive")
    monkeypatch.setattr(cli, "AMT_RUNCODE", str(tmp_path / "runcode.rds"))
    return output


def test_amt_uses_the_runcode_as_filter(
    amt_tree: Path, effects: FakeEffects, recorder: Recorder, capsys: pytest.CaptureFixture[str]
) -> None:
    status, out, err = run(Options(amt=True), effects, capsys)
    assert status == 0
    assert err == [f"ℹ Results from '{amt_tree}/'", "ℹ Runs found: 1"]
    assert recorder.calls[0]["mydir"] == [f"{amt_tree}//SSP2-NPi-AMT_2026-09-28_10.30.27"]
    assert recorder.calls[0]["user"] is None  # opt$user <- NULL: loopRuns falls back to Sys.info()


def test_amt_with_a_filter_includes_the_archive(
    amt_tree: Path, effects: FakeEffects, recorder: Recorder, capsys: pytest.CaptureFixture[str]
) -> None:
    status, out, err = run(Options(amt=True, filter="PkBudg,testOne"), effects, capsys)
    assert status == 0
    assert err == [f"ℹ Results from '{amt_tree}/' and '{amt_tree}/archive'", "ℹ Runs found: 2"]
    assert recorder.calls[0]["mydir"] == [
        f"{amt_tree}/archive/SSP2-PkBudg-AMT_2025-07-01_10.00.00",
        f"{amt_tree}//testOneRegi",
    ]


def test_amt_results_line_is_blue_with_colours(
    amt_tree: Path,
    monkeypatch: pytest.MonkeyPatch,
    effects: FakeEffects,
    recorder: Recorder,
    capsys: pytest.CaptureFixture[str],
) -> None:
    monkeypatch.setenv("R_CLI_NUM_COLORS", "256")
    status = run_cli(Options(amt=True), effects=effects)
    captured = capsys.readouterr()
    assert status == 0
    lines = captured.err.splitlines()
    assert lines[0] == f"\x1b[36mℹ\x1b[39m {UPDATE_LINES[0]}"
    assert lines[3] == f"\x1b[36mℹ\x1b[39m Results from \x1b[34m\x1b[34m{amt_tree}/\x1b[34m\x1b[39m"
    assert colors.is_enabled()  # the tables get colours too


# ---------------------------------------------------------------------------
# rs --found-in-slurm DIR
# ---------------------------------------------------------------------------


def test_found_in_slurm_contract(tmp_path: Path, capsys: pytest.CaptureFixture[str]) -> None:
    run_dir = make_run(tmp_path / "running")
    eff = FakeEffects(run_table={SQUEUE_ALL_ARGV: (f"pascalfu {run_dir} running 1:23:45 RUNNING short\n", 0)})
    status = run_cli(Options(found_in_slurm=str(run_dir)), effects=eff)
    captured = capsys.readouterr()
    assert status == 0
    assert captured.out == "short\n"
    assert captured.err == ""
    eff = FakeEffects(run_table={SQUEUE_ALL_ARGV: ("", 0)})
    status = run_cli(Options(found_in_slurm=str(run_dir)), effects=eff)
    captured = capsys.readouterr()
    assert (status, captured.out, captured.err) == (0, "no\n", "")


def test_found_in_slurm_error_path(tmp_path: Path, capsys: pytest.CaptureFixture[str]) -> None:
    eff = FakeEffects(run_table={SQUEUE_ALL_ARGV: ("", 127)})  # squeue cannot be run: "error in running command"
    status = run_cli(Options(found_in_slurm=str(tmp_path)), effects=eff)
    captured = capsys.readouterr()
    assert status == 1
    assert captured.out == ""
    assert captured.err == "Error: error in running command\n"
    # a matching squeue line with two tokens: rev(strsplit(...))[[3]] is subscript out of bounds
    eff = FakeEffects(run_table={SQUEUE_ALL_ARGV: (f"{tmp_path} {os.path.basename(tmp_path)} \n", 0)})
    status = run_cli(Options(found_in_slurm=str(tmp_path)), effects=eff)
    captured = capsys.readouterr()
    assert (status, captured.out, captured.err) == (1, "", "Error: subscript out of bounds\n")


def test_found_in_slurm_through_the_parser(monkeypatch: pytest.MonkeyPatch, tmp_path: Path) -> None:
    eff = FakeEffects(run_table={SQUEUE_ALL_ARGV: ("", 0)})
    monkeypatch.setattr(cli, "default_effects", lambda: eff)
    result = CliRunner().invoke(app, ["--found-in-slurm", str(tmp_path)])
    assert result.exit_code == 0
    assert result.stdout == "no\n"
    assert result.stderr == ""


# ---------------------------------------------------------------------------
# the three alert lines
# ---------------------------------------------------------------------------


def test_alerts_precede_everything_and_the_hint_is_from_the_list(
    tmp_path: Path, effects: FakeEffects, recorder: Recorder, capsys: pytest.CaptureFixture[str]
) -> None:
    seen: set[str] = set()
    a = make_run(tmp_path / "a")
    for _ in range(40):
        run_cli(Options(paths=str(a)), effects=effects)
        lines = capsys.readouterr().err.splitlines()
        assert lines[:2] == [f"ℹ {text}" for text in UPDATE_LINES]
        seen.add(lines[2].removeprefix("ℹ Did you know? "))
    assert seen <= set(HINTS)
    assert len(seen) > 1
