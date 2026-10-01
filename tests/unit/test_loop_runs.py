"""loop_runs: ``loopRuns`` against ``R/loopRuns.R`` (R 4.6.1 facts pinned 2026-10-01).

The listing is observed through ``FakeEffects`` (frozen clock, injected user and cluster probe) with the status
records injected by monkeypatching ``modelstats.loop_runs.get_run_status`` (the real one is exercised end to end
in ``test_end_to_end_remind_run_off_cluster`` on a run directory built from ``tests/unit/data`` and, on the real
fixtures, by the golden tier ``tests/golden/test_looprun.py``). Byte expectations are copied verbatim from the R
goldens ``migration/goldens/looprun/dup-basenames--oncluster.out`` and ``errors--local.out``.

R facts verified with Rscript (R 4.6.1, 2026-10-01):

- ``as.character(list(NA_character_))`` is ``NA`` (a real ``NA`` RunType survives the ``gsub`` and prints as
  blanks); ``format(data.frame(Runtime = NA)["Runtime"])`` is the string ``"NA"``.
- ``nchar(status["Runtime"])`` of the 1x1 data.frame holding ``"3.3 hours"`` is 9, so the prefix is ``"> "``;
  ``"10.5 hours"`` is 10 and gets ``">"`` only.
- ``trimws("abc    NA   \\n", which = "right", whitespace = " ")`` is ``"abc    NA\\n"``.
- ``r[["jobInSLURM"]]`` on a try-error fails with *subscript out of bounds*; ``TRUE && logical(0)`` in an ``if``
  fails with *missing value where TRUE/FALSE needed*; ``logical(0) || TRUE`` is ``TRUE``.
- ``order(c(2, 1, 2, 1), decreasing = TRUE)`` is ``1 3 2 4`` (stable).
- ``try(f())`` with ``f`` warning then stopping prints (stderr) ``Error in f1() : boom`` / ``In addition: Warning
  message:`` / ``In f1() : w1``; a long warning text moves to its own line after ``In f2() :``; two warnings are
  numbered ``1: In f3() : w1``; a ``call. = FALSE`` warning prints as ``nocall `` and the error as ``Error : boom``;
  11 warnings print ``There were 11 warnings (use warnings() to see them)``, 60 print ``There were 50 or more
  warnings (use warnings() to see the first 50)``; a multi-line message keeps its newlines; ``stop("")`` prints
  ``Error in f8() : `` (trailing space).
"""

from __future__ import annotations

import importlib
import io
import os
import shutil
import warnings
from collections.abc import Callable, Iterator, Sequence
from pathlib import Path
from typing import Any

import pytest
from _fake_effects import FakeEffects

import modelstats
from modelstats import colors
from modelstats.errors import RParityError, RWarning
from modelstats.formatting import format_runtime
from modelstats.loop_runs import (
    COL_SEP,
    COLTITLES_LOCAL,
    COLTITLES_ON_CLUSTER,
    loop_runs,
    r_try_message,
    r_warnings_text,
)
from modelstats.run_status import StatusTable

# the package exports the function under the module's name (``modelstats.loop_runs`` is the function), so the
# module itself is fetched from the import system for monkeypatching
loop_runs_module = importlib.import_module("modelstats.loop_runs")

DATA = Path(__file__).resolve().parent / "data"

# R goldens, byte for byte (migration/goldens/looprun/dup-basenames--oncluster.out, errors--local.out)
LEGEND = (
    "# Color code: \x1b[33mpending\x1b[39m/\x1b[33mstartup\x1b[39m, \x1b[36mrunning\x1b[39m, "
    "\x1b[32mconverged\x1b[39m/\x1b[32mfinished\x1b[39m, \x1b[38;5;214mno mif\x1b[39m, "
    "\x1b[35mconopt stalled?\x1b[39m, \x1b[38;5;202merror\x1b[39m.\n\n"
)
LEGEND_PLAIN = "# Color code: pending/startup, running, converged/finished, no mif, conopt stalled?, error.\n\n"
HEADER_CLUSTER_15 = (
    "\x1b[4mFolder           Runtime      inSlurm   RunType      RunStatus          Warnings   "
    "Iter              Conv                   modelstat              Mif     AppResults\x1b[24m \n"
)
HEADER_LOCAL_15 = (
    "\x1b[4mFolder           Runtime      RunType      RunStatus          Warnings   Iter              "
    "Conv                   modelstat            Mif   \x1b[24m \n"
)
HEADER_LOCAL_15_PLAIN = HEADER_LOCAL_15.replace("\x1b[4m", "").replace("\x1b[24m", "")
DUP_ROW_MAGPIE = (
    "samebase         33.5 mins    no        nlp_apr17    Normal completion  0          y2100             "
    "NA                     222222222222222222     yes     yes\n"
)
DUP_ROW_REMIND = (
    "samebase         NA           no        nash         full.log missing   0          NA                "
    "NA                     NA                     no      no\n"
)
ERRORS_LOCAL_NOCONFIG = (
    "noconfig         > 3.3 hours  NA           Normal completion  5          35                "
    "NA                     2: Locally Optimal   NA\n"
)

# offsets of the display columns in a width-15 listing
RUNTIME = slice(17, 28)
INSLURM = slice(30, 38)
RUNTYPE_CLUSTER = slice(40, 51)
RUNTYPE_LOCAL = slice(30, 41)

ABSENT = object()

REMIND_CELLS: dict[str, object] = {
    "jobInSLURM": "no",
    "RunType": "nash",
    "modelstat": "2: Locally Optimal",
    "runInAppResults": "yes",
    "Mif": "yes",
    "Iter": "35/100",
    "RunStatus": "Normal completion",
    "Warnings": "5",
    "Conv": "converged",
    "Runtime": 11880,
}
MAGPIE_CELLS: dict[str, object] = {
    "jobInSLURM": "no",
    "RunType": "nlp_apr17",
    "modelstat": "222222222222222222",
    "runInAppResults": "yes",
    "Mif": "yes",
    "Iter": "y2100",
    "RunStatus": "Normal completion",
    "Warnings": "0",
    "Conv": "NA",
    "Runtime": 2010,
}


class CallError(RParityError):
    """An R error that knows its call (what phase 4 adds to the raise sites the rs goldens pin)."""

    def __init__(self, message: str, call: str) -> None:
        super().__init__(message)
        self.call = call


# --------------------------------------------------------------------------- helpers


@pytest.fixture(autouse=True)
def plain_colours() -> Iterator[None]:
    """Colours off (crayon disabled) unless a test switches them on; the process-wide state is restored."""
    enabled, depth = colors.is_enabled(), colors.num_colors()
    colors.set_enabled(False, 256)
    yield
    colors.set_enabled(enabled, depth)


def coloured() -> None:
    colors.set_enabled(True, 256)


def table(rowname: str = "run", base: dict[str, object] = REMIND_CELLS, **overrides: object) -> StatusTable:
    """A one-row StatusTable with the detailed REMIND (or MAgPIE) record; ``ABSENT`` drops a column."""
    out = StatusTable()
    for column, value in {**base, **overrides}.items():
        if value is not ABSENT:
            out[rowname, column] = value
    return out


Answer = StatusTable | Exception | Callable[[], StatusTable]


class FakeStatus:
    """``get_run_status`` replaced: answers per basename (a table, an exception to raise or a callable)."""

    def __init__(self, **answers: Answer) -> None:
        self.answers = answers
        self.calls: list[tuple[str, str | None]] = []

    def __call__(
        self, mydir: str, sort: str = "nf", user: str | None = None, detailed: bool = True, effects: Any = None
    ) -> StatusTable:
        self.calls.append((mydir, user))
        answer = self.answers[os.path.basename(mydir)]
        if isinstance(answer, Exception):
            raise answer
        if callable(answer):
            return answer()
        return answer


def patch(monkeypatch: pytest.MonkeyPatch, **answers: Answer) -> FakeStatus:
    fake = FakeStatus(**answers)
    monkeypatch.setattr(loop_runs_module, "get_run_status", fake)
    return fake


def run_dir(root: Path, name: str, *files: str, mtime: float | None = None) -> str:
    """A run directory with the named (empty) files; returns its path as a string."""
    directory = root / name
    directory.mkdir(parents=True, exist_ok=True)
    for file in files:
        (directory / file).write_text("", encoding="utf-8")
    if mtime is not None:
        os.utime(directory, (mtime, mtime))
    return str(directory)


def listing(dirs: Sequence[str] | str, effects: FakeEffects, **kwargs: Any) -> str:
    out = io.StringIO()
    result = loop_runs(dirs, effects=effects, out=out, **kwargs)
    assert result is None
    return out.getvalue()


def local() -> FakeEffects:
    return FakeEffects(on_cluster=False)


def cluster() -> FakeEffects:
    return FakeEffects(on_cluster=True)


def rows_of(text: str) -> list[str]:
    """The lines after the header (escape sequences removed)."""
    plain = text
    for open_seq, close_seq in colors.STYLES.values():
        plain = plain.replace(open_seq, "").replace(close_seq, "")
    lines = plain.split("\n")
    start = next(k for k, line in enumerate(lines) if line.startswith("Folder")) + 1
    return [line for line in lines[start:] if line != ""]


def recorded(*messages: Warning | str) -> list[warnings.WarningMessage]:
    with warnings.catch_warnings(record=True) as caught:
        warnings.simplefilter("always")
        for message in messages:
            warnings.warn(message, stacklevel=1)
    return list(caught)


# --------------------------------------------------------------------------- the frame


def test_loop_runs_is_exported() -> None:
    assert modelstats.loop_runs is loop_runs


def test_titles_match_r_verbatim() -> None:
    assert COL_SEP == "  "
    assert [len(t) for t in COLTITLES_ON_CLUSTER] == [11, 8, 11, 17, 9, 16, 21, 21, 6, 10]
    assert [len(t) for t in COLTITLES_LOCAL] == [11, 11, 17, 9, 16, 21, 19, 6]
    assert COLTITLES_ON_CLUSTER[-1] == "AppResults" and COLTITLES_LOCAL[-1] == "Mif   "


def test_empty_list_and_exit_print_nothing() -> None:
    coloured()
    out = io.StringIO()
    assert loop_runs([], effects=cluster(), out=out) == "No runs found"
    assert loop_runs(["exit", "/some/dir"], effects=cluster(), out=out) is None
    assert loop_runs("exit", effects=cluster(), out=out) is None
    assert out.getvalue() == ""


def test_default_out_is_the_current_stdout(
    capsys: pytest.CaptureFixture[str], tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    a = run_dir(tmp_path, "a", "config.yml")
    patch(monkeypatch, a=table("a"))
    expected = listing([a], local(), colors=False)
    assert loop_runs([a], effects=local(), colors=False) is None
    assert capsys.readouterr().out == expected
    assert expected == HEADER_LOCAL_15_PLAIN + (
        "a                3.3 hours    nash         Normal completion  5          35/100            "
        "converged              2: Locally Optimal   yes\n"
    )


def test_only_files_print_just_the_header(tmp_path: Path) -> None:
    file = tmp_path / "notes.txt"
    file.write_text("x", encoding="utf-8")
    assert listing([str(file)], local(), colors=False) == HEADER_LOCAL_15_PLAIN


def test_dup_basenames_golden_bytes(tmp_path: Path, monkeypatch: pytest.MonkeyPatch) -> None:
    """``dup-basenames--oncluster``: two directories with the same basename, one MAgPIE and one REMIND row."""
    coloured()
    a = run_dir(tmp_path / "dupA" / "output", "samebase", "config.yml", mtime=2_000_000_000)
    b = run_dir(tmp_path / "dupB" / "output", "samebase", "config.yml", mtime=1_000_000_000)
    answers = iter(
        [
            table("samebase", MAGPIE_CELLS),
            table(
                "samebase",
                Runtime=None,
                RunStatus="full.log missing",
                Iter="NA",
                Conv="NA",
                modelstat="NA",
                Mif="no",
                runInAppResults="no",
                Warnings="0",
            ),
        ]
    )
    patch(monkeypatch, samebase=lambda: next(answers))
    text = listing([b, a], cluster())
    green, orangered = "\x1b[32m", "\x1b[38;5;202m"
    assert (
        text
        == LEGEND + HEADER_CLUSTER_15 + green + DUP_ROW_MAGPIE + "\x1b[39m" + orangered + DUP_ROW_REMIND + "\x1b[39m"
    )
    assert format_runtime(2010) == "33.5 mins"


def test_legend_only_with_colors_and_header_regardless(tmp_path: Path, monkeypatch: pytest.MonkeyPatch) -> None:
    patch(monkeypatch, run=table())
    d = run_dir(tmp_path, "run", "config.yml")
    coloured()
    with_colors = listing([d], local())
    assert with_colors.startswith(LEGEND + HEADER_LOCAL_15)
    without = listing([d], local(), colors=False)
    assert without.startswith(HEADER_LOCAL_15)  # the underline does not depend on `colors`
    colors.set_enabled(False)
    plain = listing([d], local())
    assert plain.startswith(LEGEND_PLAIN + HEADER_LOCAL_15_PLAIN)


def test_folder_width_bounds(tmp_path: Path, monkeypatch: pytest.MonkeyPatch) -> None:
    short = run_dir(tmp_path, "abc", "config.yml")
    long_name = "x" * 70
    long = run_dir(tmp_path, long_name, "config.yml")
    patch(monkeypatch, abc=table("abc"), **{long_name: table(long_name)})
    text = listing([short], local(), colors=False)
    assert text.startswith("Folder" + " " * 9 + "  Runtime    ")
    assert rows_of(text)[0].startswith("abc" + " " * 12 + "  ")
    text = listing([long], local(), colors=False)
    assert text.startswith("Folder" + " " * 61 + "  Runtime    ")
    assert rows_of(text)[0].startswith("x" * 67 + "  3.3 hours")
    assert "x" * 68 not in text


def test_sort_by_mtime_stable_and_input_order_otherwise(tmp_path: Path, monkeypatch: pytest.MonkeyPatch) -> None:
    a = run_dir(tmp_path, "a", "config.yml", mtime=1_000)
    b = run_dir(tmp_path, "b", "config.yml", mtime=3_000)
    c = run_dir(tmp_path, "c", "config.yml", mtime=2_000)
    d = run_dir(tmp_path, "d", "config.yml", mtime=3_000)
    fake = patch(monkeypatch, a=table("a"), b=table("b"), c=table("c"), d=table("d"))
    listing([a, b, c, d], local(), colors=False)
    assert [os.path.basename(p) for p, _ in fake.calls] == ["b", "d", "c", "a"]  # ties keep the given order
    fake.calls.clear()
    listing([c, a, d, b], local(), colors=False, sortbytime=False)
    assert [os.path.basename(p) for p, _ in fake.calls] == ["c", "a", "d", "b"]
    fake.calls.clear()
    listing([c, a, d, b], local(), colors=False, sortbytime=1)  # type: ignore[arg-type]  # isTRUE(1) is FALSE
    assert [os.path.basename(p) for p, _ in fake.calls] == ["c", "a", "d", "b"]


def test_files_are_dropped_and_a_missing_path_is_the_directory_na(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    monkeypatch.chdir(tmp_path)  # "NA" resolves relative to the working directory, as in R
    a = run_dir(tmp_path, "a", "config.yml", mtime=1_000)
    file = tmp_path / "notes.txt"
    file.write_text("x", encoding="utf-8")
    fake = patch(monkeypatch, a=table("a"), NA=table("NA", jobInSLURM="no"))
    text = listing([str(tmp_path / "does/not/exist"), str(file), a], local(), colors=False)
    assert [p for p, _ in fake.calls] == [a, "NA"]  # the NA row sorts last
    assert rows_of(text)[0].startswith("a" + " " * 14 + "  3.3 hours")
    assert rows_of(text)[1:] == ["NA skipped."]


def test_user_default_and_explicit(tmp_path: Path, monkeypatch: pytest.MonkeyPatch) -> None:
    a = run_dir(tmp_path, "a", "config.yml")
    fake = patch(monkeypatch, a=table("a"))
    listing([a], FakeEffects(on_cluster=False, user="bob"), colors=False)
    listing([a], FakeEffects(on_cluster=False, user="bob"), colors=False, user="alice")
    assert [u for _, u in fake.calls] == ["bob", "alice"]


# --------------------------------------------------------------------------- the skip rules (line 58)


def test_skip_rules(tmp_path: Path, monkeypatch: pytest.MonkeyPatch) -> None:
    noconfig = run_dir(tmp_path, "noconfig", "full.log")
    bak = run_dir(tmp_path, "bak", "config.Rdata.bak")
    logonly = run_dir(tmp_path, "logonly", "log.txt")
    other = run_dir(tmp_path, "other", "myconfig.yml", "log.txt.old")
    patch(
        monkeypatch,
        noconfig=table("noconfig"),
        bak=table("bak"),
        logonly=table("logonly"),
        other=table("other"),
    )
    text = listing([noconfig, bak, logonly, other], local(), colors=False, sortbytime=False)
    assert rows_of(text)[0] == f"{noconfig} skipped."  # the path as given, not the basename
    assert rows_of(text)[1].startswith("bak" + " " * 12)  # ^config.* matches config.Rdata.bak
    assert rows_of(text)[2].startswith("logonly")
    assert rows_of(text)[3] == f"{other} skipped."  # myconfig.yml and log.txt.old do not match


def test_no_config_but_a_job_is_not_skipped(tmp_path: Path, monkeypatch: pytest.MonkeyPatch) -> None:
    noconfig = run_dir(tmp_path, "noconfig")
    patch(monkeypatch, noconfig=table("noconfig", jobInSLURM="standby"))
    text = listing([noconfig], cluster(), colors=False)
    assert rows_of(text)[0].startswith("noconfig" + " " * 7 + "  > 3.3 hours  standby ")


def test_no_config_with_na_job_fails_in_the_if(tmp_path: Path, monkeypatch: pytest.MonkeyPatch) -> None:
    noconfig = run_dir(tmp_path, "noconfig")
    patch(monkeypatch, noconfig=table("noconfig", jobInSLURM=None))
    with pytest.raises(RParityError, match="^missing value where TRUE/FALSE needed$"):
        listing([noconfig], cluster(), colors=False)
    patch(monkeypatch, noconfig=table("noconfig", jobInSLURM=ABSENT))
    with pytest.raises(RParityError, match="^missing value where TRUE/FALSE needed$"):
        listing([noconfig], cluster(), colors=False)
    patch(monkeypatch, noconfig=StatusTable())
    with pytest.raises(RParityError, match="^missing value where TRUE/FALSE needed$"):
        listing([noconfig], cluster(), colors=False)


def test_bug005_error_without_config_aborts_the_listing(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch, capsys: pytest.CaptureFixture[str]
) -> None:
    """``errors-bug005--local``: the try-error subscript at line 58 aborts after the header (BUG-005, D-03)."""
    coloured()
    bad = run_dir(tmp_path, "noconfig-badstats", "runstatistics.rda")
    good = run_dir(tmp_path, "nofulllog", "config.yml")
    patch(
        monkeypatch,
        **{
            "noconfig-badstats": CallError(
                "argument is of length zero", 'if (runstatistics$stats[["config"]][["model_name"]] == "MAgPIE") {'
            ),
            "nofulllog": table("nofulllog"),
        },
    )
    out = io.StringIO()
    with pytest.raises(RParityError, match="^subscript out of bounds$"):
        loop_runs([bad, good], effects=local(), colors=False, sortbytime=False, out=out)
    header_17 = HEADER_LOCAL_15.replace("Folder" + " " * 9 + "  ", "Folder" + " " * 11 + "  ", 1)
    assert out.getvalue() == header_17  # the partial output: no legend (colors = FALSE), the header, nothing else
    assert capsys.readouterr().err == (
        'Error in if (runstatistics$stats[["config"]][["model_name"]] == "MAgPIE") { : \n  argument is of length zero\n'
    )


def test_error_with_config_is_reported_and_the_loop_continues(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch, capsys: pytest.CaptureFixture[str]
) -> None:
    """``errors--local``: ``config-bak skipped because of error`` on stdout, R's try() text on stderr."""
    bak = run_dir(tmp_path, "config-bak", "config.Rdata", "config.Rdata.bak", mtime=3_000)
    noconfig = run_dir(tmp_path, "noconfig", "log.txt", mtime=2_000)

    def fail() -> StatusTable:
        warnings.warn(
            RWarning(
                'readGDX(gdx = latest_gdx, "o_iterationNumber", format = "simplest")',
                "Error : User specified to read symbol o_iterationNumber, but it does not exist in the source file",
            ),
            stacklevel=1,
        )
        raise CallError("invalid 'description' argument", "gzfile(file)")

    patch(
        monkeypatch,
        **{
            "config-bak": fail,
            "noconfig": table(
                "noconfig",
                jobInSLURM="NA",
                RunType="NA",
                Iter="35",
                Conv="NA",
                Mif="NA",
                runInAppResults=ABSENT,
            ),
        },
    )
    text = listing([noconfig, bak], local(), colors=False)
    assert text == HEADER_LOCAL_15_PLAIN + "config-bak skipped because of error\n" + ERRORS_LOCAL_NOCONFIG
    assert capsys.readouterr().err == (
        "Error in gzfile(file) : invalid 'description' argument\n"
        "In addition: Warning message:\n"
        'In readGDX(gdx = latest_gdx, "o_iterationNumber", format = "simplest") :\n'
        "  Error : User specified to read symbol o_iterationNumber, but it does not exist in the source file\n"
    )


def test_error_without_a_call_and_non_r_errors(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch, capsys: pytest.CaptureFixture[str]
) -> None:
    a = run_dir(tmp_path, "a", "config.yml", mtime=2_000)
    b = run_dir(tmp_path, "b", "config.yml", mtime=1_000)
    patch(monkeypatch, a=RParityError("the condition has length > 1"), b=ValueError("not a GDX"))
    text = listing([a, b], local(), colors=False)
    assert rows_of(text) == ["a skipped because of error", "b skipped because of error"]
    assert capsys.readouterr().err == "Error : the condition has length > 1\nError : not a GDX\n"


def test_warnings_of_a_successful_call_stay_deferred(tmp_path: Path, monkeypatch: pytest.MonkeyPatch) -> None:
    a = run_dir(tmp_path, "a", "config.yml")

    def warn_then_answer() -> StatusTable:
        warnings.warn(RWarning("normalizePath(mydir)", 'path[1]="x": No such file or directory'), stacklevel=1)
        return table("a")

    patch(monkeypatch, a=warn_then_answer)
    with warnings.catch_warnings(record=True) as caught:
        warnings.simplefilter("always")
        text = listing([a], local(), colors=False)
    assert rows_of(text)[0].startswith("a ")
    assert [type(w.message) for w in caught] == [RWarning]
    message = caught[0].message
    assert isinstance(message, RWarning)
    assert (message.call, message.text) == ("normalizePath(mydir)", 'path[1]="x": No such file or directory')


# --------------------------------------------------------------------------- the display rewrite (lines 66-82)


@pytest.mark.parametrize(
    ("job", "runtime", "expected_runtime", "expected_job"),
    [
        ("standby pending", 100, "pending", "standby"),
        ("alice pending", None, "pending", "alice"),
        ("standby", 11880, "> 3.3 hours", "standby"),
        ("standby startup", 11880, "> 3.3 hours", "standby"),  # a numeric runtime wins over startup
        ("standby", 37800, ">10.5 hours", "standby"),  # ten characters: no space after >
        ("standby", 45, "> 45 secs", "standby"),
        ("no", 11880, "3.3 hours", "no"),
        ("NA", 11880, "> 3.3 hours", "NA"),  # off the cluster "NA" is not "no"
        ("standby startup", None, "startup", "standby"),
        ("alice  startup", None, "startup", "alice"),
        ("no", None, "NA", "no"),
        ("standby", None, "NA", "standby"),
        ("standby", float("nan"), "NA", "standby"),
    ],
)
def test_runtime_display_precedence(
    tmp_path: Path,
    monkeypatch: pytest.MonkeyPatch,
    job: str,
    runtime: object,
    expected_runtime: str,
    expected_job: str,
) -> None:
    a = run_dir(tmp_path, "a", "config.yml")
    patch(monkeypatch, a=table("a", jobInSLURM=job, Runtime=runtime))
    line = rows_of(listing([a], cluster(), colors=False))[0]
    assert line[RUNTIME].rstrip() == expected_runtime
    assert line[INSLURM].rstrip() == expected_job


def test_runtime_text_is_an_r_error(tmp_path: Path, monkeypatch: pytest.MonkeyPatch) -> None:
    a = run_dir(tmp_path, "a", "config.yml")
    patch(monkeypatch, a=table("a", Runtime="3.3 hours"))
    with pytest.raises(RParityError, match="^non-numeric argument to binary operator$"):
        listing([a], cluster(), colors=False)


def test_run_type_rewrite_and_na(tmp_path: Path, monkeypatch: pytest.MonkeyPatch) -> None:
    a = run_dir(tmp_path, "a", "config.yml")
    patch(monkeypatch, a=table("a", RunType="testOneRegi EUR"))
    assert rows_of(listing([a], cluster(), colors=False))[0][RUNTYPE_CLUSTER].rstrip() == "1Regi EUR"
    patch(monkeypatch, a=table("a", RunType="NA"))
    assert rows_of(listing([a], cluster(), colors=False))[0][RUNTYPE_CLUSTER].rstrip() == "NA"
    patch(monkeypatch, a=table("a", RunType=None, Conv="converged", Mif="yes"))
    assert rows_of(listing([a], cluster(), colors=False))[0][RUNTYPE_CLUSTER] == " " * 11  # a real NA prints blank


def test_absent_columns_fail_like_r(tmp_path: Path, monkeypatch: pytest.MonkeyPatch) -> None:
    a = run_dir(tmp_path, "a", "config.yml")
    patch(monkeypatch, a=table("a", Runtime=ABSENT))
    with pytest.raises(RParityError, match="^argument is of length zero$"):
        listing([a], cluster(), colors=False)
    patch(monkeypatch, a=table("a", RunType=ABSENT))
    with pytest.raises(RParityError, match="^undefined columns selected$"):
        listing([a], cluster(), colors=False)
    patch(monkeypatch, a=table("a", Conv=ABSENT))  # printOutput's string[, cols]
    with pytest.raises(RParityError, match="^undefined columns selected$"):
        listing([a], cluster(), colors=False)
    patch(monkeypatch, a=table("a", runInAppResults=ABSENT))  # only selected on the cluster
    assert rows_of(listing([a], local(), colors=False))[0].startswith("a ")
    with pytest.raises(RParityError, match="^undefined columns selected$"):
        listing([a], cluster(), colors=False)


def test_trailing_spaces_are_trimmed_but_the_newline_kept(tmp_path: Path, monkeypatch: pytest.MonkeyPatch) -> None:
    a = run_dir(tmp_path, "a", "config.yml")
    patch(monkeypatch, a=table("a", Mif="NA"))
    text = listing([a], local(), colors=False)
    assert text.endswith("2: Locally Optimal   NA\n")
    patch(monkeypatch, a=table("a", runInAppResults="no"))
    text = listing([a], cluster(), colors=False)
    assert text.endswith("yes     no\n")
    coloured()
    text = listing([a], cluster())
    assert text.endswith("yes     no\n\x1b[39m")  # the close sequence follows the newline


def test_numbers_in_cells_print_like_r(tmp_path: Path, monkeypatch: pytest.MonkeyPatch) -> None:
    a = run_dir(tmp_path, "a", "config.yml")
    patch(monkeypatch, a=table("a", Warnings=5, Iter=35.0))
    line = rows_of(listing([a], local(), colors=False))[0]
    assert "  5          35                " in line


# --------------------------------------------------------------------------- colours (lines 87-132)


def test_rows_are_coloured_by_the_trees(tmp_path: Path, monkeypatch: pytest.MonkeyPatch) -> None:
    coloured()
    a = run_dir(tmp_path, "a", "config.yml")
    patch(monkeypatch, a=table("a"))  # REMIND, converged with a mif, no job -> green
    text = listing([a], cluster())
    row = text[len(LEGEND) + len(HEADER_CLUSTER_15) :]
    assert row.startswith("\x1b[32ma ") and row.endswith("\n\x1b[39m")
    patch(monkeypatch, a=table("a", jobInSLURM="standby pending"))
    assert "\x1b[33ma " in listing([a], cluster())  # pending -> yellow
    patch(monkeypatch, a=table("a", jobInSLURM="standby", RunStatus="conoptspy >0.2h"))
    assert "\x1b[35ma " in listing([a], cluster())  # conoptspy -> magenta
    patch(monkeypatch, a=table("a", jobInSLURM="standby"))
    assert "\x1b[36ma " in listing([a], cluster())  # running -> cyan
    patch(monkeypatch, a=table("a", Mif="no"))
    assert "\x1b[38;5;214ma " in listing([a], cluster())  # converged without mif -> orange
    patch(monkeypatch, a=table("a", Conv="NA", RunStatus="Execution error"))
    assert "\x1b[38;5;202ma " in listing([a], cluster())  # failed -> orangered
    patch(monkeypatch, a=table("a", MAGPIE_CELLS))
    assert "\x1b[32ma " in listing([a], cluster())  # MAgPIE all-2 modelstat -> green
    patch(monkeypatch, a=table("a", MAGPIE_CELLS, modelstat="22.2", RunStatus="Normal completion"))
    text = listing([a], cluster())
    assert "\x1b[" not in text[len(LEGEND) + len(HEADER_CLUSTER_15) :]  # the MAgPIE tree's plain fall-through


def test_colors_false_prints_plain_even_with_crayon_on(tmp_path: Path, monkeypatch: pytest.MonkeyPatch) -> None:
    coloured()
    a = run_dir(tmp_path, "a", "config.yml")
    patch(monkeypatch, a=table("a"))
    text = listing([a], cluster(), colors=False)
    assert text.startswith("\x1b[4mFolder") and "\x1b[32m" not in text


def test_na_in_a_colour_condition_aborts_only_when_colouring(tmp_path: Path, monkeypatch: pytest.MonkeyPatch) -> None:
    """PORT-003: a real NA in Conv makes the REMIND tree's ``if`` NA; with ``colors = FALSE`` the row prints."""
    a = run_dir(tmp_path, "a", "config.yml")
    patch(monkeypatch, a=table("a", Conv=None))
    with pytest.raises(RParityError, match="^missing value where TRUE/FALSE needed$"):
        listing([a], cluster())
    assert rows_of(listing([a], cluster(), colors=False))[0].startswith("a ")


def test_magpie_branch_test_needs_iter_and_run_type(tmp_path: Path, monkeypatch: pytest.MonkeyPatch) -> None:
    a = run_dir(tmp_path, "a", "config.yml")
    patch(monkeypatch, a=table("a", Iter=ABSENT))
    with pytest.raises(RParityError, match="^undefined columns selected$"):  # printOutput fails first
        listing([a], cluster(), colors=False)


# --------------------------------------------------------------------------- stderr text helpers


def test_r_try_message_formats() -> None:
    assert r_try_message(CallError("boom", "f1()")) == "Error in f1() : boom\n"
    assert r_try_message(RParityError("boom")) == "Error : boom\n"
    assert r_try_message(ValueError("boom")) == "Error : boom\n"
    long_call = "if (s80_bool == 0 && as.numeric(cm_iteration_max) == iter_no) {"
    assert r_try_message(CallError("missing value where TRUE/FALSE needed", long_call)) == (
        "Error in if (s80_bool == 0 && as.numeric(cm_iteration_max) == iter_no) { : \n"
        "  missing value where TRUE/FALSE needed\n"
    )
    assert r_try_message(CallError("multi\nline", "f7()")) == "Error in f7() : multi\nline\n"
    assert r_try_message(CallError("", "f8()")) == "Error in f8() : \n"
    short_call = "f()"
    assert r_try_message(CallError("x" * 58, short_call)) == "Error in f() : " + "x" * 58 + "\n"  # 14 + 3 + 58 = 75
    assert r_try_message(CallError("x" * 59, short_call)) == "Error in f() : \n  " + "x" * 59 + "\n"  # 76 > 75


def test_r_warnings_text_formats() -> None:
    assert r_warnings_text([]) == ""
    assert r_warnings_text(recorded(RWarning("f1()", "w1")), in_addition=True) == (
        "In addition: Warning message:\nIn f1() : w1\n"
    )
    long_text = "a long warning text that pushes the line well past the seventy-five character limit"
    assert r_warnings_text(recorded(RWarning("f2()", long_text))) == f"Warning message:\nIn f2() :\n  {long_text}\n"
    two = recorded(
        RWarning("f3()", "w1"),
        RWarning("f3()", "w2 is also rather long so that it needs the break too, definitely yes it does"),
    )
    assert r_warnings_text(two, in_addition=True) == (
        "In addition: Warning messages:\n"
        "1: In f3() : w1\n"
        "2: In f3() :\n"
        "  w2 is also rather long so that it needs the break too, definitely yes it does\n"
    )
    assert r_warnings_text(recorded("nocall")) == "Warning message:\nnocall \n"
    assert r_warnings_text(recorded(RWarning("", "nocall"))) == "Warning message:\nnocall \n"
    assert r_warnings_text(recorded(*[RWarning("f5()", f"w {k}") for k in range(1, 12)]), in_addition=True) == (
        "In addition: There were 11 warnings (use warnings() to see them)\n"
    )
    assert r_warnings_text(recorded(*[RWarning("f6()", f"w {k}") for k in range(1, 61)])) == (
        "There were 50 or more warnings (use warnings() to see the first 50)\n"
    )
    assert r_warnings_text(recorded(RWarning("f7()", "first\nsecond line"))) == (
        "Warning message:\nIn f7() : first\nsecond line\n"
    )
    # the break rule counts the numbering: 6 (or 10) + nchar(call) + nchar(first line) > 75
    assert r_warnings_text(recorded(RWarning("f()", "x" * 66))) == "Warning message:\nIn f() : " + "x" * 66 + "\n"
    assert r_warnings_text(recorded(RWarning("f()", "x" * 67))) == "Warning message:\nIn f() :\n  " + "x" * 67 + "\n"
    assert r_warnings_text(recorded(RWarning("f()", "x" * 62), RWarning("f()", "x" * 63))) == (
        "Warning messages:\n1: In f() : " + "x" * 62 + "\n2: In f() :\n  " + "x" * 63 + "\n"
    )


# --------------------------------------------------------------------------- end to end


def test_end_to_end_remind_run_off_cluster(tmp_path: Path, capsys: pytest.CaptureFixture[str]) -> None:
    """The real ``get_run_status`` on a finished REMIND run built from ``tests/unit/data`` (no monkeypatching)."""
    run = tmp_path / "run"
    run.mkdir()
    shutil.copy(DATA / "config.Rdata", run / "config.Rdata")  # title testOneRegi, nash debug, cm_iteration_max 100
    shutil.copy(DATA / "runstatistics.rda", run / "runstatistics.rda")  # GAMS runtime 8304 s, modelstat 2
    (run / "full.log").write_text("--- GAMS run\n   LOOPS = 35\n*** Status: Normal completion\n", encoding="utf-8")
    (run / "log.txt").write_text("some output\nThere were 3 warnings (use warnings() to see them)\n", encoding="utf-8")
    (run / "REMIND_generic_testOneRegi.mif").write_text("mif", encoding="utf-8")
    row = (
        "run              > 2.3 hours  nash debug   Normal completion  3          35/100            "
        "NA                     2: Locally Optimal   yes\n"
    )
    with warnings.catch_warnings(record=True) as caught:
        warnings.simplefilter("always")
        text = listing([str(run)], local(), colors=False)
    assert not [w for w in caught if isinstance(w.message, RWarning)]  # a successful call defers no R warning
    assert text == HEADER_LOCAL_15_PLAIN + row
    assert format_runtime(8304) == "2.3 hours"
    coloured()
    text = listing([str(run)], local())
    # jobInSLURM "NA" off the cluster: not "no", so the REMIND tree falls through to its final cyan
    assert text == LEGEND + HEADER_LOCAL_15 + "\x1b[36m" + row + "\x1b[39m"
    assert capsys.readouterr().err == ""
