"""Console script ``rs``: the port of ``R/commandLineInterface.R`` (plan 03 sections 3.4 and 5).

The command line is parsed by typer (02 section 4.6: bundled short flags, one optional positional, ``-h``); everything
after the parse mirrors the R function statement by statement in :func:`command_line_interface`, which the typer
command hands its options as an :class:`Options` record. The surrounding :func:`run_cli` is what ``Rscript -e
'commandLineInterface(commandArgs(TRUE))'`` adds around the function: the auto-print of a visible return value
(``[1] "No runs found"`` when ``loopRuns`` gets an empty selection), the deferred ``Warning message(s):`` block at
the end, and for an R error ``Error in <call> : <message>`` / ``Calls: ...`` / ``In addition: ...`` /
``Execution halted`` with exit status 1.

What R prints with ``cli::cli_alert_info`` / ``cli_alert_warning`` goes to stderr (02 section 4.7): ``ℹ <text>``
(``i`` outside a UTF-8 locale) and ``! <text>``, the symbol in cyan (``ESC[36m``) or yellow (``ESC[33m``) closed
with ``ESC[39m`` when crayon's detection says stderr takes colours; ``{.file {paths}}`` renders each path in blue
with the doubled open sequence cli emits (``ESC[34mESC[34m<path>ESC[34mESC[39m``) or as ``'<path>'`` without colours,
several paths joined with `` and `` (two) or ``, `` and ``, and `` (three or more). The hint line is a random pick
of R's list (the comparison drops it, D-08).

Exit status: 0 wherever R quits normally (every "nothing found" branch included), 1 for an R error. typer reports
a usage error (unknown flag, two positionals, ``-d x``) with its own wording and status 2; :func:`main` maps that
status to R's 1 (D-12 pending, parity of the status; the wording is an expected-py file). ``-h`` / ``--help``
prints typer's layout (D-11).

R facts reproduced here (R 4.6.1, verified 2026-10-01, pinned in ``tests/unit/test_cli.py``):

- ``list.dirs(dir, recursive = FALSE)`` lists the sub-directories of ``dir`` (dot-directories and symbolic links to
  directories included, dangling links and files not) as ``<dir>/<name>`` with an unconditional ``/`` (``x/`` gives
  ``x//a``), in R's collation order; a missing path or a file gives ``character(0)``.
- ``strsplit("", ",")[[1]]`` is ``character(0)``, so ``rs ""`` has no paths at all and ``runfolders`` stays NULL,
  which skips the ``-m`` block; a path that lists nothing gives ``character(0)`` instead, which runs it and makes
  ``-l`` (``lastdirs`` NULL) print ``No coupled runs found``. The ``No runs found`` branch is reached from both.
- ``paste0("squeue -u ", NULL, " -h -o '%Z'")`` after ``-A`` (which sets ``opt$user <- NULL``) is the command with
  two spaces and no user word; the shell drops the empty word.
- ``grep(pattern, x)`` compiles the pattern (TRE) even for an empty ``x`` and fails with ``invalid regular
  expression '<p>', reason '<r>'`` after the ``TRE pattern compilation error`` warning.
- ``Rscript`` auto-prints the visible value of the top-level call: ``loopRuns(character(0))`` returns the visible
  string ``"No runs found"`` (printed as ``[1] "No runs found"``), ``loopRuns(c("exit", ...))`` the visible ``NULL``;
  every other branch ends in an invisible value, a ``for`` loop or ``quit()``.
- The top-level error handler prints ``Error in <call> : <message>``, with the message on its own line indented
  by two spaces when ``14 + nchar(call) + nchar(first line)`` exceeds 75 (the same rule as ``try()``,
  :func:`modelstats.loop_runs.r_try_message`), then ``Calls: <traceback>`` (omitted when it would only repeat
  the call), then the deferred warnings as ``In addition: Warning message(s):`` and finally ``Execution halted``;
  a call-less error prints ``Error: <message>``. The ``Calls:`` traceback is known for the paths the rs goldens
  pin (``readRDS`` of the runcode, the ``-C`` job-name regex, the filter regex, ``loopRuns``'s BUG-005 abort,
  ``getSanityChecks``'s ``normalizePath``); for an error raised deeper in the callees the port prints the
  traceback down to the function it can name, which R would continue.

Two things are not R: ``rs --found-in-slurm DIR`` (plan 5, the contract for REMIND's ``readcoupled.R``) prints
exactly the :func:`modelstats.slurm.found_in_slurm` string plus a newline on stdout and nothing else (exit 0; on an
error nothing on stdout, the message on stderr, exit 1), and the chooser reads one stdin line per prompt (D-21).
"""

from __future__ import annotations

import dataclasses
import glob
import io
import os
import random
import sys
import warnings
from collections.abc import Sequence
from typing import Annotated, TextIO

import typer

from modelstats import colors
from modelstats.choose import choose_from_list
from modelstats.env import Effects, default_effects, r_sort
from modelstats.errors import RParityError, RWarning
from modelstats.gdx import GdxError
from modelstats.loop_runs import loop_runs, r_try_message, r_warnings_text
from modelstats.rdata_io import read_rds, scalar
from modelstats.runfolders import (
    expand_paths,
    filter_coupled,
    is_main_folder,
    is_run_folder,
    last_iterations,
    natural_order_indices,
    r_basename,
    split_comma,
)
from modelstats.sanity import AMT_PATH, AMT_RUNCODE, get_sanity_checks
from modelstats.slurm import current_runs, found_in_slurm, normalize_path, r_regex

__all__ = [
    "HINTS",
    "OPTION_FLAGS",
    "Options",
    "app",
    "command_line_interface",
    "list_dirs",
    "main",
    "run_cli",
]

# ---------------------------------------------------------------------------
# the texts of R/commandLineInterface.R
# ---------------------------------------------------------------------------

#: The ``make_option`` help texts, verbatim.
HELP_AMT = (
    "print most recent REMIND automated model test runs. With -f: of all AMTs, show only those that match the "
    "regular expression REGEX -f REGEX"
)
HELP_NOCOLOR = "black&white: print table without colors"
HELP_CURRENT = (
    "print currently running runs no matter where they are, i.e. you don't need to be in or right above a run to "
    "see it. Add user with -u USER and include recent runs with -d N"
)
HELP_DAYSBACK = "only with -C: show recent runs of the last N days"
HELP_FILTER = "print runs that match the regular expression REGEX. Specify multiple REGEX separated by comma."
HELP_LAST = "only with -m: print the last coupling iterations only"
HELP_MAGPIE = "print all coupling iterations. Add -l to print the last iterations only"
HELP_PROMPT = "let the user choose individual runs from a list before printing the status table"
HELP_SANITY = "show overview of sanity check"
HELP_TIME = "sort runs chronologically"
HELP_USER = "only with -C: show runs of user USER"
HELP_PATH = (
    "optional; any comma separated combination of individual run folders, the main folder, or the output folder."
)
HELP_FOUND_IN_SLURM = (
    "print only the SLURM state of the run folder DIR (the value of foundInSlurm, one line, nothing else) and exit"
)

# The description and the epilogue of the R parser; click keeps the line breaks of a paragraph that starts with \b.
DESCRIPTION = """\b
Print run status or sanity of model runs.
Flags in capital letters list runs from special locations and ignore any path provided.
Short flags may be bundled together, sharing a single leading -, but only the final short flag is able to have a
corresponding argument.
The following flags only take effect if used together with other flag:
  -u, -d only with -C
  -l     only with -m
The following flags can be combined with all other flags:
  -b, -f, -p, -t
"""

EPILOG = """\b
Examples:
  1. rs /p/projects/remind/runs/REMIND-MAgPIE-2025-04-24/remind/ -mltpbf PkBudg
     From the given path list coupled runs (m) and only last iteration (l), order by time (t), prompt me to
     select the runs (p), print in black&white (b), filter (f) by 'PkBudg'.
  2. rs -As
     Show sanity checks (s) for lastest AMTs (A).
  3. rs -C -u alf -d 5
     Show run status of current runs (-C) from the user alf (-u) of the last 5 days (-d 5).

Bugs and feedback: https://github.com/pik-piam/modelstats/issues"""

#: The ``Did you know?`` hints, verbatim; one is picked at random per run (D-08: the comparison drops the line).
HINTS: tuple[str, ...] = (
    "Show (l)ast iterations of (m)agpie-coupled runs with: rs -ml",
    "Show all (m)agpie-coupled runs with: rs -m",
    "Show your runs (C)urrently running with: rs -C",
    "Show your (C)urrent runs from the last 5 (d)ays with: rs -C -d 5",
    "Sort runs by (t)ime of last change with: rs -t",
    "List runs found and (p)rompt me to select for which the status should be printed with: rs -p",
    "Show results from specific folders with: rs folder1,folder2",
    "(f)ilter runs by regular expression: rs -f PkBudg500,EU21",
    "Remove the coloring and print the table in (b)lack and white with -b.",
    "Show (C)urrent runs for one or more (u)sers with: rs -C -u user1,user2",
    "Show a (h)elp text with: rs -h",
    "To understand why your pending runs don't start, run: sq -s",
    "To get info about the current run output folder, simply run: rs",
    "List most recent (A)utomated model tests with: rs -A",
    "To get info about a specific AMT scenario, run: rs -A -f SSP2EU-Base",
    "Get a more detailed assessment of a specific run: remindstatus folder",
)

UPDATE_1 = "Update 1: The underline has been removed for converged runs (green)."
UPDATE_2 = (
    "Update 2: Runs that showed INFES but finally converged are now displayed in the same way "
    "(now green, previously blue)."
)
NO_CURRENT_RUNS = (
    "No currently running runs found. To include recent runs please expand the time horizon by adding -d DAYS."
)
TOO_MANY_RUNS = "To reduce the number of runs, filter the runs with -f REGEX or select manually from the list with -p."

#: The option flags of plan 03 section 3.4 (plus the help alias), what ``rs -h`` must list (D-11).
OPTION_FLAGS: tuple[tuple[str, str], ...] = (
    ("-A", "--amt"),
    ("-b", "--nocolor"),
    ("-C", "--current"),
    ("-d", "--daysback"),
    ("-f", "--filter"),
    ("-l", "--last"),
    ("-m", "--magpie"),
    ("-p", "--prompt"),
    ("-s", "--sanity"),
    ("-t", "--time"),
    ("-u", "--user"),
    ("-h", "--help"),
)

AMT_ARCHIVE = AMT_PATH + "archive"

# The deparsed R calls of the error sites the goldens pin (``Error in <call> : ...``) and their ``Calls:`` lines.
_GREP_FILTER_CALL = "grep(opt$filter, runfolders, value = TRUE)"
_GREPL_JOBNAME_CALL = "grepl(runnames[[i]], myruns[[i]])"
_GZFILE_CALL = 'gzfile(file, "rb")'
_READRDS_CALL = f'readRDS("{AMT_RUNCODE}")'
_LOOPRUNS_SUBSCRIPT_CALL = 'status[["jobInSLURM"]]'
_NORMALIZE_CALL = "normalizePath(dirs, mustWork = TRUE)"
_CONFIRM_CALL = 'if (!getLine() %in% c("y", "Y")) {'
_TRACE_CHOOSE = "commandLineInterface -> <Anonymous> -> choosePatternFromList"
_TRACE_LOOPRUNS = "commandLineInterface -> <Anonymous>"
_TRACE_SANITY = "commandLineInterface -> <Anonymous>"
_INVALID_REGEX = "invalid regular expression "


# ---------------------------------------------------------------------------
# options
# ---------------------------------------------------------------------------


@dataclasses.dataclass(frozen=True)
class Options:
    """The parsed command line, named like ``arguments$options`` / ``arguments$args`` in R."""

    paths: str | None = None
    amt: bool = False
    nocolor: bool = False
    current: bool = False
    daysback: int = 0
    filter: str = ".*"
    last: bool = False
    magpie: bool = False
    prompt: bool = False
    sanity: bool = False
    time: bool = False
    user: str = "you"
    found_in_slurm: str | None = None


# ---------------------------------------------------------------------------
# cli::cli_alert_info / cli_alert_warning on stderr
# ---------------------------------------------------------------------------


def _is_utf8(stream: TextIO) -> bool:
    """cli's ``is_utf8_output()``: the UTF-8 symbols unless the stream declares another encoding."""
    encoding = str(getattr(stream, "encoding", None) or "")
    if not encoding:
        return True  # an in-memory text stream: Python's default encoding is UTF-8
    return encoding.lower().replace("-", "").replace("_", "") == "utf8"


class _Alert:
    """``cli_alert_info`` / ``cli_alert_warning`` as cli 3.6 prints them (02 section 4.7)."""

    def __init__(self, stream: TextIO, colour: bool) -> None:
        self.stream = stream
        self.colour = colour
        self.info_symbol = "ℹ" if _is_utf8(stream) else "i"

    def _line(self, symbol: str, code: str, text: str) -> None:
        prefix = f"\x1b[{code}m{symbol}\x1b[39m" if self.colour else symbol
        self.stream.write(f"{prefix} {text}\n")

    def info(self, text: str) -> None:
        self._line(self.info_symbol, "36", text)

    def warning(self, text: str) -> None:
        self._line("!", "33", text)

    def files(self, paths: Sequence[str]) -> str:
        """``{.file {paths}}``: blue (doubled open sequence) or quoted, collapsed like a cli vector."""
        items = [f"\x1b[34m\x1b[34m{path}\x1b[34m\x1b[39m" if self.colour else f"'{path}'" for path in paths]
        if len(items) <= 1:
            return "".join(items)
        if len(items) == 2:
            return f"{items[0]} and {items[1]}"
        return ", ".join(items[:-1]) + f", and {items[-1]}"


# ---------------------------------------------------------------------------
# R errors escaping commandLineInterface
# ---------------------------------------------------------------------------


class _Halt(Exception):
    """An R error condition reaching the top level: ``Rscript`` prints it and exits with status 1.

    ``message`` is the condition message, ``call`` the deparsed call (``None`` prints ``Error: <message>``),
    ``trace`` the ``Calls:`` line's text (``None`` prints no such line).
    """

    def __init__(self, message: str, call: str | None, trace: str | None) -> None:
        super().__init__(message)
        self.message = message
        self.call = call
        self.trace = trace

    def text(self) -> str:
        if self.call:
            error = r_try_message(RParityError(self.message, call=self.call))
        else:
            error = f"Error: {self.message}\n"
        if self.trace:
            error += f"Calls: {self.trace}\n"
        return error


def _halt(exc: BaseException, *, call: str | None, trace: str | None) -> _Halt:
    """The :class:`_Halt` for an exception raised by a callee; a ``call`` the exception carries wins."""
    own = getattr(exc, "call", None)
    return _Halt(str(exc), own if isinstance(own, str) and own else call, trace)


# ---------------------------------------------------------------------------
# R helpers
# ---------------------------------------------------------------------------


def list_dirs(path: str, effects: Effects) -> list[str]:
    """``list.dirs(path, recursive = FALSE)``: the sub-directories of ``path`` as ``<path>/<name>``.

    Dot-directories and symbolic links to directories are included (``dir()`` would skip the former), dangling
    links and files are not; the names are in R's collation order (:func:`modelstats.env.r_sort`) and joined
    with an unconditional ``/``. A path that is not a directory lists nothing.
    """
    if path == "" or not effects.is_dir(path):
        return []
    escaped = glob.escape(path)
    names = {os.path.basename(entry) for entry in effects.glob(f"{escaped}/*")}
    names |= {os.path.basename(entry) for entry in effects.glob(f"{escaped}/.*")}
    names.discard(".")
    names.discard("..")
    return [f"{path}/{name}" for name in r_sort(names) if effects.is_dir(f"{path}/{name}")]


def _read_runcode(effects: Effects) -> str:
    """``readRDS("/p/projects/remind/modeltests/remind/runcode.rds")`` with R's error and warning texts."""
    try:
        value = read_rds(AMT_RUNCODE, effects)
    except RParityError as exc:
        if str(exc) == "cannot open the connection":
            cause = exc.__cause__
            reason = cause.strerror if isinstance(cause, OSError) and cause.strerror else "No such file or directory"
            warnings.warn(
                RWarning(_GZFILE_CALL, f"cannot open compressed file '{AMT_RUNCODE}', probable reason '{reason}'"),
                stacklevel=2,
            )
            raise _Halt(str(exc), _GZFILE_CALL, "commandLineInterface -> readRDS -> gzfile") from exc
        raise _Halt(str(exc), _READRDS_CALL, "commandLineInterface -> readRDS") from exc
    runcode = scalar(value)
    if not isinstance(runcode, str):
        # opt$filter <- NULL / a non-character value: grep() rejects the pattern
        raise _Halt("invalid 'pattern' argument", _GREP_FILTER_CALL, "commandLineInterface -> grep")
    return runcode


def _r_warnings(caught: Sequence[warnings.WarningMessage]) -> list[warnings.WarningMessage]:
    """The recorded warnings R would have deferred (the port's :class:`RWarning` ones); Python's own are dropped."""
    return [item for item in caught if isinstance(item.message, RWarning)]


# ---------------------------------------------------------------------------
# commandLineInterface, statement by statement
# ---------------------------------------------------------------------------


def command_line_interface(opt: Options, effects: Effects, stdin: TextIO, stdout: TextIO, alert: _Alert) -> None:
    """The body of ``commandLineInterface(argv)`` after ``parse_args``; raises :class:`_Halt` for an R error.

    Writes the table to ``stdout``, the alerts through ``alert`` (stderr) and reads the ``-p`` answers from
    ``stdin``; a visible return value of the R function is printed to ``stdout`` as ``Rscript`` would.
    """
    # print hint
    alert.info(UPDATE_1)
    alert.info(UPDATE_2)
    alert.info(f"Did you know? {random.choice(HINTS)}")  # noqa: S311 - not security related

    # set default for user; set default for paths; split comma separated parameters into vectors
    user: str | None = effects.user if opt.user == "you" else opt.user
    paths = expand_paths(opt.paths)
    filt = "|".join(split_comma(opt.filter))

    # AMT runs: hardcode AMT path and use regular expression from 'runcode.rds' for filtering the latest AMTs
    if opt.amt:
        user = None
        paths = [AMT_PATH]
        if filt == ".*":
            filt = _read_runcode(effects)
        else:
            paths = [AMT_PATH, AMT_ARCHIVE]
        alert.info(f"Results from {alert.files(paths)}")

    runfolders: list[str] | None
    if opt.current:
        # OPTION A: get current runs. After -A the user is NULL: paste0() drops it from the command, the shell
        # drops the empty word; current_runs() takes the empty string for that (its command text is R's).
        try:
            myruns = current_runs("" if user is None else user, opt.daysback, effects)
        except RParityError as exc:
            if str(exc).startswith(_INVALID_REGEX):
                raise _halt(exc, call=_GREPL_JOBNAME_CALL, trace="commandLineInterface -> grepl") from exc
            if str(exc) == "error in running command":
                raise _halt(exc, call=None, trace="commandLineInterface -> system") from exc
            raise _halt(exc, call="runnames[[i]]", trace="commandLineInterface") from exc
        # exit with the proper message
        if len(myruns) == 0:
            if opt.daysback < 1:
                alert.warning(NO_CURRENT_RUNS)
            else:
                alert.warning(f"No runs found in the past {opt.daysback} days. Try to expand the time horizon.")
            return
        runfolders = myruns
    else:
        # OPTION B: create list with run folders from path supplied by user or AMTs
        runfolders = None
        for directory in paths:
            if runfolders is None:
                runfolders = []
            if is_run_folder(directory, effects):
                runfolders.append(directory)
            elif is_main_folder(directory, effects):
                runfolders.extend(list_dirs(f"{directory}/output", effects))  # file.path(dir, "output")
            else:
                runfolders.extend(list_dirs(directory, effects))

    # filter coupling iterations (only when runfolders is not NULL; character(0) runs the block)
    if opt.magpie and runfolders is not None:
        runfolders = filter_coupled(runfolders, os.getcwd(), effects)
        if opt.last:
            runfolders = last_iterations(runfolders) or None  # lastdirs stays NULL for an empty input
        if runfolders is None:
            alert.warning("No coupled runs found")
            return

    # filter runs. If not changed by the user the default pattern '.*' filters all
    try:
        regex = r_regex(filt, call=_GREP_FILTER_CALL)
    except RParityError as exc:
        raise _halt(exc, call=_GREP_FILTER_CALL, trace="commandLineInterface -> grep") from exc
    runfolders = [folder for folder in (runfolders or []) if regex.search(folder) is not None]

    # sort strings containing embedded numbers so that the numbers are numerically sorted
    keys = [r_basename(normalize_path(folder, effects)) for folder in runfolders]
    runfolders = [runfolders[i - 1] for i in natural_order_indices(keys)]

    # proceed if there are runs left after filtering
    if not runfolders:
        # this can only happen if the filtering removed all runs
        alert.warning("No runs found")
        return

    # print hint how to reduce number of runs
    if len(runfolders) > 40 and filt == ".*" and not opt.prompt:
        alert.info(TOO_MANY_RUNS)

    # list all runs found and prompt the user to select (gms::chooseFromList prints with message(): stderr)
    if opt.prompt:
        try:
            runfolders = choose_from_list(runfolders, type="folders", stdin=stdin, stdout=alert.stream)
        except RParityError as exc:
            # EOF at the confirmation prompt: R's `if (!getLine() %in% c("y", "Y"))` on character(0)
            raise _halt(exc, call=_CONFIRM_CALL, trace=_TRACE_CHOOSE) from exc
        except EOFError as exc:
            # EOF at the pattern prompt: R re-prompts until the node stack overflows (BUG-040); the port stops
            raise _Halt(str(exc), None, _TRACE_CHOOSE) from exc

    alert.info(f"Runs found: {len(runfolders)}")

    # decide whether to display sanity checks or runstatus
    if opt.sanity:
        try:
            get_sanity_checks(runfolders, effects=effects, out=stdout)
        except RParityError as exc:
            if str(exc).startswith("path[") and ": No such file or directory" in str(exc):
                raise _halt(exc, call=_NORMALIZE_CALL, trace=f"{_TRACE_SANITY} -> normalizePath") from exc
            raise _halt(exc, call=None, trace=f"{_TRACE_SANITY} -> getRunStatus") from exc
        except GdxError as exc:  # D-20: a corrupt GDX, where R aborts the whole process
            raise _halt(exc, call=None, trace=f"{_TRACE_SANITY} -> getRunStatus") from exc
        return
    try:
        result = loop_runs(
            runfolders, user=user, colors=not opt.nocolor, sortbytime=opt.time, effects=effects, out=stdout
        )
    except RParityError as exc:
        if str(exc) == "subscript out of bounds":  # BUG-005: status[["jobInSLURM"]] on the try-error
            raise _halt(exc, call=_LOOPRUNS_SUBSCRIPT_CALL, trace=_TRACE_LOOPRUNS) from exc
        raise _halt(exc, call=None, trace=_TRACE_LOOPRUNS) from exc
    # Rscript prints the visible value of the top-level call: the "No runs found" string of an empty selection,
    # NULL for a first folder named "exit"; every other path returns invisibly
    if result is not None:
        stdout.write(f'[1] "{result}"\n')
    elif runfolders[0] == "exit":
        stdout.write("NULL\n")


# ---------------------------------------------------------------------------
# the process around it (Rscript)
# ---------------------------------------------------------------------------


def _found_in_slurm_contract(opt: Options, effects: Effects, stdout: TextIO, stderr: TextIO) -> int:
    """``rs --found-in-slurm DIR``: the foundInSlurm string plus newline, nothing else; an error gives 1."""
    assert opt.found_in_slurm is not None
    user = None if opt.user == "you" else opt.user
    with warnings.catch_warnings(record=True) as caught:
        warnings.simplefilter("always")
        try:
            value = found_in_slurm(opt.found_in_slurm, user=user, effects=effects)
        except Exception as exc:  # noqa: BLE001 - every failure is reported on stderr with status 1
            stderr.write(f"Error: {exc}\n")
            stderr.write(r_warnings_text(_r_warnings(caught)))
            return 1
    stdout.write(f"{value}\n")
    stderr.write(r_warnings_text(_r_warnings(caught)))
    return 0


def run_cli(opt: Options, effects: Effects | None = None) -> int:
    """Run ``rs`` with parsed options; returns the exit status (0, or 1 for an R error).

    Streams are ``sys.stdin`` / ``sys.stdout`` / ``sys.stderr`` as they are at call time. Colours: crayon's
    detection on stdout switches the process-wide flag of :mod:`modelstats.colors` (the tables), the same
    detection on stderr decides the colour of the alert symbols. The deferred R warnings of the run are printed
    at the end exactly as ``Rscript`` prints them (or in the ``In addition:`` block of an error).
    """
    eff = default_effects() if effects is None else effects
    stdout, stderr = sys.stdout, sys.stderr
    stdin: TextIO = sys.stdin if sys.stdin is not None else io.StringIO()
    if opt.found_in_slurm is not None:
        return _found_in_slurm_contract(opt, eff, stdout, stderr)
    colors.enable_from_environment(stdout)
    alert = _Alert(stderr, colors.detect_enabled(stderr))
    with warnings.catch_warnings(record=True) as caught:
        warnings.simplefilter("always")
        try:
            command_line_interface(opt, eff, stdin, stdout, alert)
        except _Halt as halt:
            stdout.flush()
            stderr.write(halt.text())
            stderr.write(r_warnings_text(_r_warnings(caught), in_addition=True))
            stderr.write("Execution halted\n")
            return 1
    stdout.flush()
    stderr.write(r_warnings_text(_r_warnings(caught)))
    return 0


# ---------------------------------------------------------------------------
# typer
# ---------------------------------------------------------------------------

app = typer.Typer(
    add_completion=False,
    rich_markup_mode=None,
    pretty_exceptions_enable=False,
    context_settings={"help_option_names": ["-h", "--help"], "terminal_width": 100, "max_content_width": 100},
)


@app.command(name="rs", help=DESCRIPTION, epilog=EPILOG, options_metavar="[OPTION]")
def rs(
    paths: Annotated[str | None, typer.Argument(metavar="[PATH]", help=HELP_PATH, show_default=False)] = None,
    amt: Annotated[bool, typer.Option("-A", "--amt", help=HELP_AMT)] = False,
    nocolor: Annotated[bool, typer.Option("-b", "--nocolor", help=HELP_NOCOLOR)] = False,
    current: Annotated[bool, typer.Option("-C", "--current", help=HELP_CURRENT)] = False,
    daysback: Annotated[int, typer.Option("-d", "--daysback", metavar="N", help=HELP_DAYSBACK)] = 0,
    filter: Annotated[str, typer.Option("-f", "--filter", metavar="REGEX", help=HELP_FILTER)] = ".*",
    last: Annotated[bool, typer.Option("-l", "--last", help=HELP_LAST)] = False,
    magpie: Annotated[bool, typer.Option("-m", "--magpie", help=HELP_MAGPIE)] = False,
    prompt: Annotated[bool, typer.Option("-p", "--prompt", help=HELP_PROMPT)] = False,
    sanity: Annotated[bool, typer.Option("-s", "--sanity", help=HELP_SANITY)] = False,
    time: Annotated[bool, typer.Option("-t", "--time", help=HELP_TIME)] = False,
    user: Annotated[str, typer.Option("-u", "--user", metavar="USER", help=HELP_USER)] = "you",
    found_in_slurm: Annotated[
        str | None, typer.Option("--found-in-slurm", metavar="DIR", help=HELP_FOUND_IN_SLURM, show_default=False)
    ] = None,
) -> None:
    opt = Options(
        paths=paths,
        amt=amt,
        nocolor=nocolor,
        current=current,
        daysback=daysback,
        filter=filter,
        last=last,
        magpie=magpie,
        prompt=prompt,
        sanity=sanity,
        time=time,
        user=user,
        found_in_slurm=found_in_slurm,
    )
    status = run_cli(opt)
    if status != 0:
        raise typer.Exit(code=status)


def _reconfigure(stream: object) -> None:
    """Write (and read) file names byte for byte, like R: undecodable bytes survive through surrogateescape."""
    reconfigure = getattr(stream, "reconfigure", None)
    if reconfigure is None:
        return
    try:
        reconfigure(errors="surrogateescape")
    except ValueError, OSError:
        pass


def main() -> None:
    """Entry point of the ``rs`` console script."""
    for stream in (sys.stdin, sys.stdout, sys.stderr):
        _reconfigure(stream)
    try:
        app(prog_name="rs")
    except SystemExit as exc:
        if exc.code == 2:
            # typer's usage error; R's optparse stops with an error, i.e. Rscript status 1 (D-12 pending)
            raise SystemExit(1) from None
        raise
