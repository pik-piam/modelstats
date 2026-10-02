"""``modelstats.cli``: optparse's reading of argv, the alerts, R's error printing and the decision tree.

The argv table below was probed against optparse 1.8.2 (``parse_args(..., positional_arguments = c(0, 1))``
with the option list of ``R/commandLineInterface.R:35-47``, R 4.6.1) on 2026-10-02, the negative-number
forms included; the alert bytes come from ``migration/harness/checks/cli_alerts.R``; the error and warning
layouts from ``Rscript`` probes of ``R/src/main/errors.c`` (``verrorcall_dflt``, ``PrintWarnings``) and from
the ``rs`` goldens named in the tests. The decision tree runs on ``FakeEffects`` over temporary directories
(no fixtures): the "nothing found" branches, the ``> 40`` hint, ``-C`` on an empty and on a failing scheduler,
``-A`` with the D-26 state file and off the cluster, the autoprinted ``[1] "No runs found"`` of an empty
``-p`` selection, the usage errors and ``--found-in-slurm``.
"""

from __future__ import annotations

import io
import json
import math
from collections.abc import Callable, Iterator
from contextlib import redirect_stderr, redirect_stdout
from dataclasses import dataclass
from datetime import datetime
from pathlib import Path
from zoneinfo import ZoneInfo

import pytest

from modelstats.cli import (
    HINTS,
    NO_CURRENT_RUNS,
    AlertKind,
    Options,
    OptparseError,
    OptparseInteger,
    RError,
    Session,
    alert_line,
    error_line,
    file_span,
    halted_text,
    make_app,
    normalize_argv,
    optparse_message,
    r_as_numeric,
    run_rs,
    top_level_warnings,
    usage_error_text,
    utf8_output,
)
from modelstats.colors import Crayon
from modelstats.env import CannedSlurm, ClusterPaths, FakeEffects
from modelstats.errors import DeferredWarning
from modelstats.runfolders import MAIN_FOLDER_MARKERS, REMIND_RUN_MARKERS

NOW = datetime(2026, 9, 30, 12, 0, tzinfo=ZoneInfo("Europe/Berlin"))
USER = "pascalfu"
UTF8_ENVIRON = {"LC_ALL": "C.utf8", "LANG": "C.utf8"}
PLAIN = Crayon.disabled()
COLOURS = Crayon(enabled=True, colours=256)
INFO = "ℹ"
BANNER_LINES = 3


# ---------------------------------------------------------------------------------------------
# normalize_argv: optparse's forms and messages
# ---------------------------------------------------------------------------------------------

ACCEPTED: list[tuple[list[str], list[str]]] = [
    (["X", "-mltpbf", "PkBudg"], ["X", "-m", "-l", "-t", "-p", "-b", "-f", "PkBudg"]),
    (["-As"], ["-A", "-s"]),
    (["-bt", "."], ["-b", "-t", "."]),
    (["-C", "-u", "alf", "-d", "5"], ["-C", "-u", "alf", "-d", "5"]),
    (["--daysback=3"], ["--daysback=3"]),
    (["--daysback", "3"], ["--daysback", "3"]),
    (["--da", "3"], ["--daysback", "3"]),
    (["--f", "x"], ["--filter", "x"]),
    (["--filter=a=b"], ["--filter=a=b"]),
    (["-Ad", "3"], ["-A", "-d", "3"]),
    (["-A", "--", "-f"], ["-A", "--", "-f"]),
    (["-A", "-"], ["-A", "-"]),
    (["-f", "-"], ["-f", "-"]),
    (["-u", ""], ["-u", ""]),
    (["-d", "3", "-d", "4"], ["-d", "3", "-d", "4"]),
    (["--found-in-slurm", "/p/x"], ["--found-in-slurm", "/p/x"]),
    (["--found-in-slurm=/p/x"], ["--found-in-slurm=/p/x"]),
    (["a,b", "-f", "x,y"], ["a,b", "-f", "x,y"]),
    ([], []),
    (["-"], ["-"]),
    (["--"], ["--"]),
    # negative numbers are values (optparse's is_negative_number in getopt and normalize_opt)
    (["-d", "-1"], ["-d", "-1"]),
    (["-d", "-15"], ["-d", "-15"]),
    (["-d", "-1.5e2"], ["-d", "-1.5e2"]),
    (["-d", "-.5"], ["-d", "-.5"]),
    (["-d", "-0"], ["-d", "-0"]),
    (["-Cd", "-15"], ["-C", "-d", "-15"]),
    (["--daysback", "-1"], ["--daysback", "-1"]),
    (["--daysback=-15"], ["--daysback=-15"]),
    (["--da", "-15"], ["--daysback", "-15"]),
    (["-f", "-1"], ["-f", "-1"]),
    (["-u", "-15"], ["-u", "-15"]),
    (["-d", "-1", "-A"], ["-d", "-1", "-A"]),
    (["-d", "-1b"], ["-d", "-1", "-b"]),  # not a number: expanded first, -1 is the value, -b a flag
    (["-d", "-1", "--", "-15"], ["-d", "-1", "--", "-15"]),
    (["-d", "-9999999999"], ["-d", "-9999999999"]),  # accepted here, kept as a string by OptparseInteger
]
REJECTED: list[tuple[list[str], str]] = [
    (["-z"], 'short flag "z" is invalid'),
    (["--bogus"], 'long flag "bogus" is invalid'),
    (["--z"], 'long flag "z" is invalid'),
    (["---A"], 'long flag "-A" is invalid'),
    (["-A=1"], 'short flag "=" is invalid'),
    (["--amt=1"], 'long flag "amt" accepts no arguments'),
    (["--amt=FALSE"], 'long flag "amt" accepts no arguments'),
    (["-f"], 'short flag "f" requires an argument'),
    (["-fA"], 'short flag "f" requires an argument'),
    (["-fPkBudg"], 'short flag "f" requires an argument'),
    (["-dA"], 'short flag "d" requires an argument'),
    (["-u", "-d"], 'short flag "u" requires an argument'),
    (["-u", "-A"], 'short flag "u" requires an argument'),
    (["-f", "--"], 'short flag "f" requires an argument'),
    (["--filter"], 'long flag "filter" requires an argument'),
    (["--daysback"], 'long flag "daysback" requires an argument'),
    (["--found-in-slurm"], 'long flag "found-in-slurm" requires an argument'),
    (["--found-in"], 'long flag "found-in" is invalid'),  # the Python-only option is not abbreviated
    (["-z", "-h"], 'short flag "z" is invalid'),
    # a negative number after a flag without a value is a bundle of digits
    (["-A", "-15"], 'short flag "1" is invalid'),
    (["-A", "-1"], 'short flag "1" is invalid'),
    (["-1"], 'short flag "1" is invalid'),
    ([".", "-1"], 'short flag "1" is invalid'),
    (["--daysback=3", "-15"], 'short flag "1" is invalid'),
    (["-d", "-15", "-15"], 'short flag "1" is invalid'),
    (["-dC", "-15"], 'short flag "d" requires an argument'),
    # not a negative number: a flag, so no value
    (["-d", "-x"], 'short flag "d" requires an argument'),
    (["-d", "-."], 'short flag "d" requires an argument'),
    (["-d", "--1"], 'short flag "d" requires an argument'),
    (["-d", "--"], 'short flag "d" requires an argument'),
    # digit-led bundles that are not numbers are expanded before the value is taken
    (["-d", "-1x"], 'short flag "x" is invalid'),
    (["-d", "-12x"], 'short flag "2" is invalid'),
    (["-d", "-1e"], 'short flag "e" is invalid'),
    (["-d", "-1."], 'short flag "." is invalid'),
    (["-d", "-1a5"], 'short flag "a" is invalid'),
    (["-d", "-1", "-2"], 'short flag "2" is invalid'),
    (["--daysback", "-15x"], 'short flag "5" is invalid'),
]


@pytest.mark.parametrize(("argv", "expected"), ACCEPTED, ids=lambda value: " ".join(value) or "<empty>")
def test_normalize_argv_accepts_the_optparse_forms(argv: list[str], expected: list[str]) -> None:
    assert normalize_argv(argv) == expected


@pytest.mark.parametrize(
    ("argv", "message"), REJECTED, ids=lambda value: " ".join(value) if isinstance(value, list) else value
)
def test_normalize_argv_rejects_with_optparse_messages(argv: list[str], message: str) -> None:
    with pytest.raises(OptparseError) as excinfo:
        normalize_argv(argv)
    assert str(excinfo.value) == message


def test_usage_error_text_is_the_unknown_flag_golden() -> None:
    """``migration/goldens/rs/unknown-flag.err``."""
    assert usage_error_text('short flag "z" is invalid') == (
        'Usage: \n rs [PATH] [OPTION]\n\n\nNA: error: short flag "z" is invalid\n'
    )


def test_optparse_message_maps_typers_wording() -> None:
    from typer._click.exceptions import BadOptionUsage, NoSuchOption, UsageError

    assert optparse_message(NoSuchOption("-z")) == 'short flag "z" is invalid'
    assert optparse_message(NoSuchOption("--bogus")) == 'long flag "bogus" is invalid'
    assert optparse_message(BadOptionUsage("-f", "Option '-f' requires an argument.")) == (
        'short flag "f" requires an argument'
    )
    assert optparse_message(BadOptionUsage("--amt", "Option '--amt' does not take a value.")) == (
        'long flag "amt" accepts no arguments'
    )
    assert optparse_message(UsageError("something else")) == "something else"


# ---------------------------------------------------------------------------------------------
# OptparseInteger: -d
# ---------------------------------------------------------------------------------------------

INTEGERS: list[tuple[str, int]] = [
    ("5", 5),
    ("+5", 5),
    ("-3", -3),
    ("5.5", 5),
    (".5", 0),
    ("1.", 1),
    ("1e1", 10),
    ("0x10", 16),
    ("0x1p3", 8),  # a hexadecimal with a binary exponent
    ("0X1P-1", 0),
    ("0x1.8p1", 3),
    ("1e-400", 0),  # underflows to 0
    (" 7 ", 7),
    ("2147483647", 2147483647),
]
NUMERICS: list[tuple[str, float | None]] = [
    ("x", None),
    ("NA", None),
    ("TRUE", None),
    ("1e", None),
    ("1d5", None),
    (".", None),
    ("0x", None),
    ("1e999 x", None),
    ("99999999999", 99999999999.0),
    ("-9999999999", -9999999999.0),
    ("2147483648", 2147483648.0),
    ("0x80000000", 2147483648.0),
    ("0x1p3", 8.0),
    ("0x1.8p1", 3.0),
    ("1E5", 1e5),
    ("+.5", 0.5),
    ("1e-400", 0.0),
    ("1e999", math.inf),
    ("-1e999", -math.inf),
    ("1e400", math.inf),
    (" 1e999 ", math.inf),
    ("0x" + "F" * 300, math.inf),
    ("Inf", math.inf),
    ("+Inf", math.inf),
    ("-Inf", -math.inf),
    ("inf", math.inf),
    ("INF", math.inf),
    ("Infinity", math.inf),
    ("-infinity", -math.inf),
]
"""``as.numeric()`` of the strings, probed in R 4.6.1 on 2026-10-02 (``NaN`` and ``nan`` are tested apart)."""
KEPT_STRINGS = [
    "x",
    "NA",
    "99999999999",
    "-9999999999",
    "Inf",
    "TRUE",
    "1_0",
    "1e",
    "3=4",
    "1e999",  # as.integer(Inf) is NA
    "-1e999",
    "1e400",
    "0x80000000",  # one past the integer range
    "0x" + "F" * 300,
]


@pytest.mark.parametrize(("text", "value"), INTEGERS)
def test_optparse_integer_reads_what_as_integer_reads(text: str, value: int) -> None:
    parsed = OptparseInteger.parse(text, fancy_quotes=True)
    assert parsed.value == value
    assert parsed.warnings == ()
    assert parsed.shown == str(value)


@pytest.mark.parametrize("text", KEPT_STRINGS)
def test_optparse_integer_keeps_the_string_as_integer_warns_about(text: str) -> None:
    parsed = OptparseInteger.parse(text, fancy_quotes=True)
    assert parsed.value == text
    assert parsed.warnings == (DeferredWarning("value[[3L]](cond)", f"integer expected, got “{text}”"),)
    assert parsed.shown == text


@pytest.mark.parametrize(
    ("text", "expected"), NUMERICS, ids=lambda value: repr(value) if isinstance(value, str) else ""
)
def test_r_as_numeric_reads_what_as_numeric_reads(text: str, expected: float | None) -> None:
    assert r_as_numeric(text) == expected


@pytest.mark.parametrize("text", ["NaN", "nan", "-NaN", "+nan"])
def test_r_as_numeric_nan(text: str) -> None:
    value = r_as_numeric(text)
    assert value is not None and math.isnan(value)


@pytest.mark.parametrize("text", ["NaN", "nan", "-NaN"])
def test_optparse_integer_nan_is_a_silent_na_like_the_blank(text: str) -> None:
    """``as.integer("NaN")`` is ``NA`` without a warning, so getopt's own check warns, as for ``""``."""
    parsed = OptparseInteger.parse(text, fancy_quotes=True)
    assert parsed.value is None
    assert parsed.warnings == (
        DeferredWarning(
            "getopt(spec = spec, opt = args, operand = operand, defaults = defaults, ",
            f"integer expected, got “{text}”",
        ),
    )


def test_optparse_integer_uses_plain_quotes_outside_utf8() -> None:
    parsed = OptparseInteger.parse("x", fancy_quotes=False)
    assert parsed.warnings[0].message == 'integer expected, got "x"'


def test_optparse_integer_blank_is_na_with_getopts_warning() -> None:
    parsed = OptparseInteger.parse("", fancy_quotes=True)
    assert parsed.value is None
    assert (
        parsed.warnings[0].call == "getopt(spec = spec, opt = args, operand = operand, defaults = defaults, "
    )
    assert parsed.warnings[0].message == "integer expected, got “”"
    with pytest.raises(RError) as excinfo:
        parsed.greater_than(0)
    assert excinfo.value.message == "missing value where TRUE/FALSE needed"
    assert excinfo.value.call == "if (opt$daysback > 0) {"
    assert excinfo.value.traceback == ("commandLineInterface",)


def test_optparse_integer_compares_a_kept_string_as_r_collates() -> None:
    """``"x" > 0`` is TRUE and ``"x" < 1`` is FALSE in R (golden ``daysback-nonint``)."""
    kept = OptparseInteger.parse("x", fancy_quotes=True)
    assert kept.greater_than(0) and not kept.less_than(1)
    five = OptparseInteger.parse("5", fancy_quotes=True)
    assert five.greater_than(0) and not five.less_than(1)
    zero = OptparseInteger.parse("0", fancy_quotes=True)
    assert not zero.greater_than(0) and zero.less_than(1)


# ---------------------------------------------------------------------------------------------
# Alerts (migration/harness/checks/cli_alerts.R)
# ---------------------------------------------------------------------------------------------

PATHS = (
    "/p/projects/remind/modeltests/remind/output/",
    "/p/projects/remind/modeltests/remind/output/archive",
)


def test_alert_lines_ascii_without_colour() -> None:
    assert alert_line(AlertKind.INFO, "Runs found: 2", PLAIN, utf8=False) == "i Runs found: 2\n"
    assert alert_line(AlertKind.WARNING, "No runs found", PLAIN, utf8=False) == "! No runs found\n"
    assert file_span(PATHS, PLAIN) == f"'{PATHS[0]}' and '{PATHS[1]}'"


def test_alert_lines_utf8_without_colour() -> None:
    assert alert_line(AlertKind.INFO, "Runs found: 2", PLAIN, utf8=True) == f"{INFO} Runs found: 2\n"
    assert alert_line(AlertKind.WARNING, "No runs found", PLAIN, utf8=True) == "! No runs found\n"


def test_alert_lines_utf8_with_256_colours() -> None:
    assert (
        alert_line(AlertKind.INFO, "Runs found: 2", COLOURS, utf8=True)
        == f"\x1b[36m{INFO}\x1b[39m Runs found: 2\n"
    )
    assert (
        alert_line(AlertKind.WARNING, "No runs found", COLOURS, utf8=True)
        == "\x1b[33m!\x1b[39m No runs found\n"
    )
    blue = [f"\x1b[34m\x1b[34m{path}\x1b[34m\x1b[39m" for path in PATHS]
    assert file_span(PATHS, COLOURS) == f"{blue[0]} and {blue[1]}"
    assert file_span(PATHS[:1], COLOURS) == blue[0]


def test_alert_line_drops_the_trailing_newline_of_the_glue_string() -> None:
    """``cli_alert_info()`` of a glue string ending in a newline prints one line (golden ``amt.err``)."""
    assert alert_line(AlertKind.INFO, "Results from x\n", PLAIN, utf8=True) == f"{INFO} Results from x\n"


def test_file_span_joins_three_paths_with_an_oxford_comma() -> None:
    assert file_span(("a", "b", "c"), PLAIN) == "'a', 'b', and 'c'"


@pytest.mark.parametrize(
    ("environ", "expected"),
    [
        ({"LC_ALL": "C.utf8"}, True),
        ({"LC_ALL": "C", "LANG": "en_US.UTF-8"}, False),
        ({"LC_CTYPE": "de_DE.UTF-8"}, True),
        ({"LANG": "en_US.utf8"}, True),
        ({"LANG": "POSIX"}, False),
        ({}, False),
    ],
)
def test_utf8_output_follows_the_locale_categories(environ: dict[str, str], expected: bool) -> None:
    assert utf8_output(FakeEffects(now=NOW, user=USER, on_cluster=False, environ=environ)) is expected


# ---------------------------------------------------------------------------------------------
# R's top-level error and warning printing
# ---------------------------------------------------------------------------------------------


def test_error_line_short_long_and_without_call() -> None:
    assert error_line('gzfile(file, "rb")', "cannot open the connection") == (
        'Error in gzfile(file, "rb") : cannot open the connection\n'
    )
    assert error_line("f()", "m" * 70) == "Error in f() : \n  " + "m" * 70 + "\n"
    assert error_line("f()", "ab\ncd") == "Error in f() : ab\ncd\n"
    assert error_line(None, "no call") == "Error: no call\n"


def test_halted_text_is_the_amt_local_golden() -> None:
    """``migration/goldens/rs/amt--local.err`` from line 4 on."""
    path = "/p/projects/remind/modeltests/remind/runcode.rds"
    warning = DeferredWarning(
        'gzfile(file, "rb")',
        f"cannot open compressed file '{path}', probable reason 'No such file or directory'",
    )
    abort = RError(
        "cannot open the connection",
        call='gzfile(file, "rb")',
        traceback=("commandLineInterface", "readRDS", "gzfile"),
        warnings=(warning,),
    )
    assert halted_text(abort, []) == (
        'Error in gzfile(file, "rb") : cannot open the connection\n'
        "Calls: commandLineInterface -> readRDS -> gzfile\n"
        "In addition: Warning message:\n"
        'In gzfile(file, "rb") :\n'
        f"  cannot open compressed file '{path}', probable reason 'No such file or directory'\n"
        "Execution halted\n"
    )


def test_halted_text_without_traceback_or_warnings() -> None:
    assert halted_text(RError("end of input at the confirmation prompt"), []) == (
        "Error: end of input at the confirmation prompt\nExecution halted\n"
    )


def test_top_level_warnings_is_the_current_failing_golden_block() -> None:
    """``migration/goldens/rs/current--failing.err`` from the ``Warning messages:`` line on."""
    calls = (
        'system(paste0("squeue -u ", opt$user, " -h -o \'%Z\'"), intern = TRUE)',
        'system(paste0("squeue -u ", opt$user, " -h -o \'%j\'"), intern = TRUE)',
    )
    warnings = [
        DeferredWarning(calls[0], "running command 'squeue -u pascalfu -h -o '%Z'' had status 1"),
        DeferredWarning(calls[1], "running command 'squeue -u pascalfu -h -o '%j'' had status 1"),
    ]
    assert top_level_warnings(warnings) == (
        "Warning messages:\n"
        f"1: In {calls[0]} :\n  running command 'squeue -u pascalfu -h -o '%Z'' had status 1\n"
        f"2: In {calls[1]} :\n  running command 'squeue -u pascalfu -h -o '%j'' had status 1\n"
    )
    assert top_level_warnings([]) == ""
    assert top_level_warnings(warnings[:1]).startswith("Warning message:\n")


# ---------------------------------------------------------------------------------------------
# The decision tree on temporary directories
# ---------------------------------------------------------------------------------------------


@dataclass(frozen=True)
class Run:
    """What one in-process invocation produced."""

    stdout: str
    stderr: str
    status: int

    @property
    def alerts(self) -> list[str]:
        """The stderr lines after the three announcement lines."""
        return self.stderr.splitlines()[BANNER_LINES:]


def make_run_folder(directory: Path) -> Path:
    directory.mkdir(parents=True, exist_ok=True)
    for name in REMIND_RUN_MARKERS[:4]:
        (directory / name).write_text("", encoding="utf-8")
    return directory


def make_main_folder(directory: Path) -> Path:
    directory.mkdir(parents=True, exist_ok=True)
    for name in MAIN_FOLDER_MARKERS:
        target = directory / name
        if name == "output":
            target.mkdir(exist_ok=True)
        else:
            target.write_text("", encoding="utf-8")
    return directory


def scripted(lines: list[str]) -> Callable[[], str | None]:
    pending: Iterator[str] = iter(lines)
    return lambda: next(pending, None)


def effects_in(
    cwd: Path,
    *,
    on_cluster: bool = False,
    slurm: CannedSlurm | None = None,
    cluster_paths: ClusterPaths | None = None,
    user: str = USER,
) -> FakeEffects:
    return FakeEffects(
        now=NOW,
        user=user,
        on_cluster=on_cluster,
        slurm=slurm,
        cwd=cwd,
        environ=UTF8_ENVIRON,
        cluster_paths=cluster_paths,
    )


def invoke(argv: list[str], effects: FakeEffects, stdin: list[str] | None = None) -> Run:
    """``rs argv`` through the typer application on a session over ``effects`` without colours."""
    out, err = io.StringIO(), io.StringIO()
    session = Session(
        effects=effects,
        out=out,
        err=err,
        table_crayon=PLAIN,
        alert_crayon=PLAIN,
        read_line=scripted(stdin or []),
        pick_hint=lambda hints: hints[0],
    )
    with redirect_stdout(out), redirect_stderr(err):
        status = make_app(lambda: session)(args=argv, prog_name="rs")
    return Run(out.getvalue(), err.getvalue(), status)


def test_the_banner_is_three_info_lines_with_a_hint(tmp_path: Path) -> None:
    run = invoke([], effects_in(tmp_path))
    lines = run.stderr.splitlines()
    assert lines[0] == f"{INFO} Update 1: The underline has been removed for converged runs (green)."
    assert lines[1].startswith(f"{INFO} Update 2: ")
    assert lines[2] == f"{INFO} Did you know? {HINTS[0]}"


def test_nothing_below_the_working_directory_is_no_runs_found(tmp_path: Path) -> None:
    run = invoke([], effects_in(tmp_path))
    assert run == Run("", run.stderr, 0)
    assert run.alerts == ["! No runs found"]


def test_a_filter_that_removes_everything_is_no_runs_found(tmp_path: Path) -> None:
    make_run_folder(tmp_path / "SSP2-NPi")
    run = invoke(["-f", "zzz,yyy"], effects_in(tmp_path))
    assert run.alerts == ["! No runs found"] and run.status == 0


def test_an_invalid_filter_is_greps_error(tmp_path: Path) -> None:
    make_run_folder(tmp_path / "SSP2-NPi")
    run = invoke(["-f", "a["], effects_in(tmp_path))
    assert run.status == 1 and run.stdout == ""
    assert run.alerts == [
        "Error in grep(opt$filter, runfolders, value = TRUE) : ",
        "  invalid regular expression 'a[', reason 'Missing ']''",
        "Calls: commandLineInterface -> grep",
        "In addition: Warning message:",
        "In grep(opt$filter, runfolders, value = TRUE) :",
        "  TRE pattern compilation error 'Missing ']''",
        "Execution halted",
    ]


def test_magpie_flag_without_coupled_runs(tmp_path: Path) -> None:
    """``-m`` falls through to "No runs found"; only ``-ml`` says "No coupled runs found" (BUG-026 family)."""
    make_run_folder(tmp_path / "plain")
    assert invoke(["-m"], effects_in(tmp_path)).alerts == ["! No runs found"]
    assert invoke(["-ml"], effects_in(tmp_path)).alerts == ["! No coupled runs found"]


def test_an_empty_positional_is_rs_null_paths(tmp_path: Path) -> None:
    """``rs ""``: ``strsplit("", ",")`` is empty, ``runfolders`` stays ``NULL`` and ``-ml`` is skipped."""
    assert invoke(["", "-ml"], effects_in(tmp_path)).alerts == ["! No runs found"]


def test_more_than_forty_runs_prints_the_filter_hint_unless_filtered_or_prompted(tmp_path: Path) -> None:
    main = make_main_folder(tmp_path / "remind")
    for index in range(41):
        make_run_folder(main / "output" / f"run{index:02d}")
    effects = effects_in(main)
    hint = (
        f"{INFO} To reduce the number of runs, filter the runs with -f REGEX "
        "or select manually from the list with -p."
    )
    listed = invoke(["-b"], effects)
    assert listed.alerts[0] == hint and listed.alerts[1] == f"{INFO} Runs found: 41" and listed.status == 0
    assert listed.stdout.count("\n") == 42  # the header and 41 rows (no legend with -b)
    filtered = invoke(["-f", "run"], effects)
    assert hint not in filtered.alerts and f"{INFO} Runs found: 41" in filtered.alerts
    prompted = invoke(["-p"], effects, stdin=["2"])
    assert hint not in prompted.alerts and f"{INFO} Runs found: 1" in prompted.alerts


def test_an_empty_prompt_selection_autoprints_no_runs_found(tmp_path: Path) -> None:
    """``loopRuns(character(0))`` returns its string visibly (golden ``prompt-empty``)."""
    make_run_folder(tmp_path / "SSP2-NPi")
    run = invoke(["-p"], effects_in(tmp_path), stdin=[])
    assert run.stdout == '[1] "No runs found"\n' and run.status == 0
    assert run.alerts[-2:] == ["Selected: ", f"{INFO} Runs found: 0"]


def test_prompt_end_of_input_at_a_search_prompt_is_an_error(tmp_path: Path) -> None:
    """D-21: where R recurses to a stack overflow the port stops with an error line and exit 1."""
    make_run_folder(tmp_path / "SSP2-NPi")
    run = invoke(["-p"], effects_in(tmp_path), stdin=["p"])
    assert run.status == 1 and run.stdout == ""
    assert run.alerts[-2:] == ["Error: end of input at the search prompt", "Execution halted"]


def test_current_runs_on_an_empty_scheduler(tmp_path: Path) -> None:
    effects = effects_in(tmp_path, on_cluster=True, slurm=CannedSlurm({"squeue_all.txt": ""}))
    none = invoke(["-C"], effects)
    assert none.alerts == [f"! {NO_CURRENT_RUNS}"]
    assert none.status == 0
    recent = invoke(["-C", "-d", "2"], effects)
    assert recent.alerts == ["! No runs found in the past 2 days. Try to expand the time horizon."]


def test_current_runs_on_a_failing_scheduler_passes_stderr_through_and_defers_the_warnings(
    tmp_path: Path,
) -> None:
    """Golden ``current--failing``: the tool's stderr at once, R's warnings at the end."""
    effects = effects_in(
        tmp_path, on_cluster=True, slurm=CannedSlurm({"squeue_all.txt": "", "squeue_exit": "1"})
    )
    run = invoke(["-C"], effects)
    assert run.status == 0
    assert run.alerts == [
        "squeue: error: fake squeue failure (exit 1)",
        "squeue: error: fake squeue failure (exit 1)",
        f"! {NO_CURRENT_RUNS}",
        "Warning messages:",
        '1: In system(paste0("squeue -u ", opt$user, " -h -o \'%Z\'"), intern = TRUE) :',
        "  running command 'squeue -u pascalfu -h -o '%Z'' had status 1",
        '2: In system(paste0("squeue -u ", opt$user, " -h -o \'%j\'"), intern = TRUE) :',
        "  running command 'squeue -u pascalfu -h -o '%j'' had status 1",
    ]


def test_current_runs_error_keeps_the_scheduler_diagnostics(tmp_path: Path) -> None:
    """``%Z`` answers one directory, ``%j`` fails: the child's stderr, then R's error with the warning.

    R runs both ``system()`` calls before reading their answers (``R/commandLineInterface.R:141-165``), so the
    scheduler's message precedes ``Error in runnames[[i]]`` and the status warning follows under ``In
    addition:`` (probed with ``Rscript`` on 2026-10-02).
    """
    slurm = CannedSlurm(
        {"squeue_all.txt": "", "squeue_Z_pascalfu.txt": f"{tmp_path}/run\n", "squeue_sequence": "0\n1\n"}
    )
    run = invoke(["-C"], effects_in(tmp_path, on_cluster=True, slurm=slurm))
    assert run.status == 1 and run.stdout == ""
    assert run.alerts == [
        "squeue: error: fake squeue failure (exit 1)",
        "Error in runnames[[i]] : subscript out of bounds",
        "Calls: commandLineInterface -> grepl",
        "In addition: Warning message:",
        'In system(paste0("squeue -u ", opt$user, " -h -o \'%j\'"), intern = TRUE) :',
        "  running command 'squeue -u pascalfu -h -o '%j'' had status 1",
        "Execution halted",
    ]


def test_current_runs_with_more_names_than_directories_is_the_myruns_error(tmp_path: Path) -> None:
    """One work directory, two names: ``grepl`` forces ``myruns[[i]]`` first, through ``is.factor``."""
    slurm = CannedSlurm(
        {"squeue_all.txt": "", "squeue_Z_pascalfu.txt": f"{tmp_path}\n", "squeue_j_pascalfu.txt": "a\nb\n"}
    )
    run = invoke(["-C"], effects_in(tmp_path, on_cluster=True, slurm=slurm))
    assert run.status == 1
    assert run.alerts == [
        "Error in myruns[[i]] : subscript out of bounds",
        "Calls: commandLineInterface -> grepl -> is.factor",
        "Execution halted",
    ]


def test_current_runs_with_a_negative_daysback_is_a_value_and_skips_sacct(tmp_path: Path) -> None:
    """``rs -C -d -1``: ``-1`` is the value; ``-1 > 0`` is FALSE (no ``sacct``) and ``-1 < 1`` TRUE."""
    effects = effects_in(tmp_path, on_cluster=True, slurm=CannedSlurm({"squeue_all.txt": ""}))
    run = invoke(["-C", "-d", "-1"], effects)
    assert run.status == 0
    assert run.alerts == [f"! {NO_CURRENT_RUNS}"]
    assert [call.args[0] for call in effects.runs] == ["squeue", "squeue"]
    bundled = invoke(
        ["-Cd", "-1"], effects_in(tmp_path, on_cluster=True, slurm=CannedSlurm({"squeue_all.txt": ""}))
    )
    assert bundled.alerts == [f"! {NO_CURRENT_RUNS}"]


def sacct_since(effects: FakeEffects) -> list[str]:
    """The ``-S`` value of every ``sacct`` call the effects recorded."""
    return [call.args[call.args.index("-S") + 1] for call in effects.runs if call.args[0] == "sacct"]


@pytest.mark.parametrize(
    ("text", "since", "coercion_warning", "recent"),
    [
        ("x", "NA", True, True),  # golden daysback-nonint
        ("NA", "NA", True, True),
        ("1e999", "-Inf", False, True),  # golden daysback-overflow
        ("1e999x", "NA", True, True),
        ("Inf", "-Inf", False, True),
        ("99999999999", "-273788674-01-19", False, True),
        ("0x80000000", "-5877584-03-21", False, False),  # "0x80000000" < "1" by collation: the other message
    ],
)
def test_current_runs_with_a_kept_daysback_string_pastes_as_numeric_into_sacct(
    tmp_path: Path, text: str, since: str, coercion_warning: bool, recent: bool
) -> None:
    """``as.numeric(opt$daysback)`` (R ``:145``): ``NA`` with a coercion warning, or a number without one.

    The strings compare with ``0`` and ``1`` by collation (probed in R 4.6.1): all of these are ``> 0``, so
    ``sacct`` runs with the pasted text, and ``recent`` says whether ``< 1`` is FALSE (the "past N days"
    message) or TRUE.
    """
    effects = effects_in(tmp_path, on_cluster=True, slurm=CannedSlurm({"squeue_all.txt": ""}))
    run = invoke(["-C", "-d", text], effects)
    assert run.status == 0
    assert sacct_since(effects) == [since, since]
    if recent:
        expected = [f"! No runs found in the past {text} days. Try to expand the time horizon."]
    else:
        expected = [f"! {NO_CURRENT_RUNS}"]
    kept = f"In value[[3L]](cond) : integer expected, got “{text}”"
    if coercion_warning:
        expected += [
            "Warning messages:",
            f"1: {kept}",
            '2: In paste0("sacct -u ", opt$user, " -s cd,f,cancelled,timeout,oom -S ",  :',
            "  NAs introduced by coercion",
        ]
    else:
        expected += ["Warning message:", kept]
    assert run.alerts == expected


def test_an_overflowing_daysback_is_kept_as_a_string_with_the_warning(tmp_path: Path) -> None:
    """``rs -d 1e999`` without ``-C``: the announcements, ``No runs found``, the warning last, exit 0."""
    run = invoke(["-d", "1e999"], effects_in(tmp_path))
    assert run.status == 0 and run.stdout == ""
    assert run.alerts == [
        "! No runs found",
        "Warning message:",
        "In value[[3L]](cond) : integer expected, got “1e999”",
    ]


def test_current_runs_with_a_blank_daysback_is_rs_if_error(tmp_path: Path) -> None:
    effects = effects_in(tmp_path, on_cluster=True, slurm=CannedSlurm({"squeue_all.txt": ""}))
    run = invoke(["-C", "-d", ""], effects)
    assert run.status == 1
    assert run.alerts == [
        "Error in if (opt$daysback > 0) { : missing value where TRUE/FALSE needed",
        "Calls: commandLineInterface",
        "In addition: Warning message:",
        "In getopt(spec = spec, opt = args, operand = operand, defaults = defaults,  :",
        "  integer expected, got “”",
        "Execution halted",
    ]


def amt_paths(tmp_path: Path) -> ClusterPaths:
    """A temporary AMT checkout in place of ``/p/projects/remind/modeltests/remind``."""
    amt = tmp_path / "modeltests" / "remind"
    (amt / "output").mkdir(parents=True)
    (amt / "output" / "archive").mkdir()
    return ClusterPaths(
        amt_dir=str(amt),
        amt_output=f"{amt}/output/",
        amt_archive=f"{amt}/output/archive",
        amt_runcode=f"{amt}/runcode.rds",
    )


def test_amt_reads_the_state_file_before_runcode_rds(tmp_path: Path) -> None:
    """D-26: ``amt-state.json`` names the cycle; the two matching runs are listed, the archive is not."""
    paths = amt_paths(tmp_path)
    output = Path(paths.amt_output)
    for name in ("SSP2-NPi-AMT_2026-09-28", "SSP2-EU21-AMT_2026-09-28", "SSP2-NPi-AMT_2026-09-01"):
        make_run_folder(output / name)
    make_run_folder(output / "archive" / "SSP2-NPi-AMT_2026-08-01")
    state = {"cycle": {"runcode": "2026-09-28"}}
    (Path(paths.amt_dir) / "amt-state.json").write_text(json.dumps(state), encoding="utf-8")
    run = invoke(["-A", "-b"], effects_in(tmp_path, on_cluster=True, cluster_paths=paths))
    assert run.status == 0
    assert run.alerts[0] == f"{INFO} Results from '{paths.amt_output}'"
    assert run.alerts[1] == f"{INFO} Runs found: 2"
    assert "SSP2-NPi-AMT_2026-09-28" in run.stdout and "SSP2-EU21-AMT_2026-09-28" in run.stdout
    assert "2026-09-01" not in run.stdout and "2026-08-01" not in run.stdout


def test_amt_with_a_filter_adds_the_archive_and_skips_the_runcode(tmp_path: Path) -> None:
    paths = amt_paths(tmp_path)
    make_run_folder(Path(paths.amt_archive) / "SSP2-PkBudg-AMT_2026-07-01")
    run = invoke(["-A", "-f", "PkBudg", "-b"], effects_in(tmp_path, on_cluster=True, cluster_paths=paths))
    assert run.status == 0
    assert run.alerts[0] == f"{INFO} Results from '{paths.amt_output}' and '{paths.amt_archive}'"
    assert run.alerts[1] == f"{INFO} Runs found: 1"
    assert "SSP2-PkBudg-AMT_2026-07-01" in run.stdout


def test_amt_without_runcode_is_the_readrds_error(tmp_path: Path) -> None:
    """Golden ``amt--local``: ``readRDS`` on a missing ``runcode.rds``."""
    paths = amt_paths(tmp_path)
    run = invoke(["-A"], effects_in(tmp_path, on_cluster=False, cluster_paths=paths))
    assert run.status == 1 and run.stdout == ""
    assert run.alerts == [
        'Error in gzfile(file, "rb") : cannot open the connection',
        "Calls: commandLineInterface -> readRDS -> gzfile",
        "In addition: Warning message:",
        'In gzfile(file, "rb") :',
        f"  cannot open compressed file '{paths.amt_runcode}', probable reason 'No such file or directory'",
        "Execution halted",
    ]


def test_help_and_usage_errors_through_the_application(tmp_path: Path) -> None:
    effects = effects_in(tmp_path)
    for flag in ("-h", "--help"):
        page = invoke([flag], effects)
        assert page.status == 0 and page.stderr == ""
        assert page.stdout.startswith("Usage: rs [OPTIONS] [PATH]")
        assert "-A, --amt" in page.stdout and "--found-in-slurm DIR" in page.stdout
    unknown = invoke(["-z"], effects)
    assert unknown == Run("", usage_error_text('short flag "z" is invalid'), 1)
    two = invoke(["a", "b"], effects)
    assert two == Run("", usage_error_text("required at most 1 positional arguments, got 2"), 1)
    three = invoke(["a", "-A", "b", "c"], effects)
    assert three.stderr.endswith("got 3\n")


def test_found_in_slurm_prints_the_value_and_nothing_else(tmp_path: Path) -> None:
    run_dir = make_run_folder(tmp_path / "output" / "myrun")
    effects = effects_in(tmp_path, on_cluster=True, slurm=CannedSlurm({"squeue_all.txt": ""}))
    run = invoke(["--found-in-slurm", str(run_dir)], effects)
    assert run == Run("no\n", "", 0)
    line = f"{USER} {run_dir} myrun 0:10 PENDING standby\n"
    pending = invoke(
        ["--found-in-slurm", str(run_dir)],
        effects_in(tmp_path, on_cluster=True, slurm=CannedSlurm({"squeue_all.txt": line})),
    )
    assert pending == Run("standby pending\n", "", 0)


def test_run_rs_returns_zero_for_the_quit_branches(tmp_path: Path) -> None:
    out, err = io.StringIO(), io.StringIO()
    session = Session(effects=effects_in(tmp_path), out=out, err=err, table_crayon=PLAIN, alert_crayon=PLAIN)
    assert run_rs(Options(), session) == 0
    assert out.getvalue() == ""
    assert err.getvalue().endswith("! No runs found\n")
