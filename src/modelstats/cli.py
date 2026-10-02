"""Console script ``rs``: the status and sanity tables of REMIND and MAgPIE runs.

A drop-in for ``R/commandLineInterface.R`` (modelstats 0.31.0, ``migration/03-migration-plan.md`` section
3.4 and 5 "CLI drop-in", ``migration/06-user-requests.md`` section 7 phase 4). The command line is parsed by
a typer application with one command whose options are those of the R ``option_list``
(``R/commandLineInterface.R:35-47``); :func:`normalize_argv` first rewrites ``argv`` the way optparse reads
it (bundled short flags, abbreviated long flags, no attached short values, a value starting with ``-`` only
when it is a negative number) so that the two parsers accept and reject the same forms and a usage error
prints optparse's text (:func:`usage_error_text`, decision D-12 pending: parity, exit status 1). The help
page is typer's (D-11).

The decision tree (``:95-266``) is :func:`run_rs` over a :class:`Session`: the announcement lines and every
``cli_alert_*`` go to stderr in cli's format (``migration/02-python-libraries.md`` 4.7, :func:`alert_line`),
the table to stdout, the random hint is one of :data:`HINTS` (D-08: excluded from the golden comparison).
``rs -A`` reads the AMT cycle's regular expression through :func:`modelstats.sanity.amt_runcode`
(``amt-state.json`` before ``runcode.rds``, D-26). Where ``commandLineInterface`` stops with an R error the
port prints what ``Rscript`` prints (:func:`halted_text`: ``Error in <call> : <message>``, the ``Calls:``
traceback where it is known, the deferred warnings, ``Execution halted``) and exits 1; the deferred
warnings of a successful run are printed at the end as R's top level does (``current--failing``,
``daysback-nonint`` goldens).

``rs --found-in-slurm DIR`` prints exactly ``foundInSlurm(DIR)`` plus a newline on stdout and nothing else
(the contract REMIND's ``readcoupled.R`` relies on, ``03`` section 5), exit 0; on error nothing on stdout, the
message on stderr, exit 1.

Differences from R, all under pending or approved decisions: the help layout (D-11); the usage-error
program name is R's ``NA`` (``basename(get_Rscript_filename())`` under ``Rscript -e``) until D-12 decides;
``-p`` reads one line per prompt (D-21, :mod:`modelstats.choose`) and ends with an error where R recurses
to a stack overflow; a damaged GDX is a ``skipped because of error`` row (D-20) where R aborts; the
``Calls:`` line is printed only where the R traceback is known (the goldens' cases and the direct calls of
``commandLineInterface``), never guessed.
"""

from __future__ import annotations

import io
import math
import os
import random
import re
import sys
from collections.abc import Callable, Sequence
from dataclasses import dataclass, field
from enum import Enum
from typing import Annotated, Any, Final, TextIO

import typer
from typer._click.exceptions import BadOptionUsage, NoSuchOption, UsageError

from modelstats.choose import LineReader, NoMoreInputError, choose_from_list, read_stdin_line
from modelstats.colors import MISSING_VALUE_IN_IF, ColourRuleError, Crayon
from modelstats.env import Effects, ProductionEffects, r_collation_key
from modelstats.errors import DeferredWarning, ModelstatsError, RunReadError
from modelstats.formatting import PrintOutputError
from modelstats.loop_runs import (
    NO_RUNS_FOUND,
    LoopRunsError,
    deferred_warnings_message,
    loop_runs,
    warnings_block,
)
from modelstats.runfolders import (
    DEFAULT_PATH,
    expand_paths,
    filter_coupled,
    last_iterations,
    natural_order,
    split_comma_list,
)
from modelstats.sanity import (
    CANNOT_OPEN_CONNECTION,
    INVALID_PATTERN,
    SanityError,
    amt_runcode,
    get_sanity_checks,
)
from modelstats.slurm import (
    MYRUNS_CALL,
    RUNNAMES_CALL,
    SUBSCRIPT_OUT_OF_BOUNDS,
    SlurmCall,
    SlurmError,
    found_in_slurm,
    r_grepl,
    run_folders_of,
    sacct_since_text,
    scheduler_calls,
)

__all__ = [
    "HINTS",
    "AlertKind",
    "OptparseError",
    "OptparseInteger",
    "Options",
    "RError",
    "RsApp",
    "Session",
    "alert_line",
    "app",
    "error_line",
    "file_span",
    "found_in_slurm_command",
    "halted_text",
    "make_app",
    "normalize_argv",
    "r_as_numeric",
    "run_rs",
    "usage_error_text",
    "utf8_output",
]

# ---------------------------------------------------------------------------------------------
# The texts of R/commandLineInterface.R:35-93
# ---------------------------------------------------------------------------------------------

USAGE: Final = "\n rs [PATH] [OPTION]\n"
"""``usage`` (``R/commandLineInterface.R:49-50``), printed by optparse ahead of a usage error."""

USAGE_ERROR_PROGRAM: Final = "NA"
"""optparse prefixes its errors with ``basename(get_Rscript_filename())``: ``NA`` under ``Rscript -e``."""

HELP_DESCRIPTION: Final = (
    "[PATH] is optional and can be any comma separated combination of individual run folders, the main "
    "folder, or the output folder. [OPTION] see below.\n"
    "\n"
    "Print run status or sanity of model runs. Flags in capital letters list runs from special locations "
    "and ignore any path provided. Short flags may be bundled together, sharing a single leading -, but "
    "only the final short flag is able to have a corresponding argument.\n"
    "\n"
    "\b\n"
    "The following flags only take effect if used together with other flag:\n"
    "  -u, -d only with -C\n"
    "  -l     only with -m\n"
    "The following flags can be combined with all other flags:\n"
    "  -b, -f, -p, -t\n"
    "\n"
    "\b\n"
    "Examples:\n"
    "  1. rs /p/projects/remind/runs/REMIND-MAgPIE-2025-04-24/remind/ -mltpbf PkBudg\n"
    "     From the given path list coupled runs (m) and only last iteration (l), order by time (t),\n"
    "     prompt me to select the runs (p), print in black&white (b), filter (f) by 'PkBudg'.\n"
    "  2. rs -As\n"
    "     Show sanity checks (s) for lastest AMTs (A).\n"
    "  3. rs -C -u alf -d 5\n"
    "     Show run status of current runs (-C) from the user alf (-u) of the last 5 days (-d 5).\n"
)
"""``description`` (``R/commandLineInterface.R:52-67``) in typer's help layout (D-11)."""

HELP_EPILOGUE: Final = "Bugs and feedback: https://github.com/pik-piam/modelstats/issues"
"""``epilogue`` (``R/commandLineInterface.R:69-70``)."""

OPTION_HELP: Final[dict[str, str]] = {
    "amt": (
        "print most recent REMIND automated model test runs. With -f: of all AMTs, show only those that "
        "match the regular expression REGEX -f REGEX"
    ),
    "nocolor": "black&white: print table without colors",
    "current": (
        "print currently running runs no matter where they are, i.e. you don't need to be in or right above "
        "a run to see it. Add user with -u USER and include recent runs with -d N"
    ),
    "daysback": "only with -C: show recent runs of the last N days",
    "filter": (
        "print runs that match the regular expression REGEX. Specify multiple REGEX separated by comma."
    ),
    "last": "only with -m: print the last coupling iterations only",
    "magpie": "print all coupling iterations. Add -l to print the last iterations only",
    "prompt": "let the user choose individual runs from a list before printing the status table",
    "sanity": "show overview of sanity check",
    "time": "sort runs chronologically",
    "user": "only with -C: show runs of user USER",
    "found-in-slurm": (
        "print only the inSlurm value of the run folder DIR (foundInSlurm), for scripts such as REMIND's "
        "readcoupled.R"
    ),
}
"""The ``help`` strings of ``make_option`` (``R/commandLineInterface.R:36-46``) and the Python-only option."""

HINTS: Final[tuple[str, ...]] = (
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
"""``hints`` (``R/commandLineInterface.R:76-93``); ``sample(hints, 1)`` picks the ``Did you know?`` line."""

UPDATE_LINES: Final = (
    "Update 1: The underline has been removed for converged runs (green).",
    "Update 2: Runs that showed INFES but finally converged are now displayed in the same way "
    "(now green, previously blue).",
)
"""The two ``cli_alert_info`` announcements (``R/commandLineInterface.R:102-103``)."""

DID_YOU_KNOW: Final = "Did you know? "
NO_COUPLED_RUNS: Final = "No coupled runs found"
NO_CURRENT_RUNS: Final = (
    "No currently running runs found. "
    "To include recent runs please expand the time horizon by adding -d DAYS."
)
MANY_RUNS_HINT: Final = (
    "To reduce the number of runs, filter the runs with -f REGEX or select manually from the list with -p."
)
MANY_RUNS: Final = 40
"""``length(runfolders) > 40`` (``R/commandLineInterface.R:244``)."""

USER_DEFAULT: Final = "you"
"""The ``-u`` default that ``:109`` replaces by ``Sys.info()[["user"]]``."""

FILTER_DEFAULT: Final = ".*"
AUTOPRINTED_NO_RUNS: Final = '[1] "No runs found"\n'
"""``loopRuns(character(0))`` returns the string visibly and ``Rscript`` autoprints it (``prompt-empty``)."""

# ---------------------------------------------------------------------------------------------
# optparse's reading of argv (R/commandLineInterface.R:35-47, 99)
# ---------------------------------------------------------------------------------------------

_SHORT_FLAGS: Final = frozenset("AbClmpsth")
_SHORT_VALUED: Final = frozenset("dfu")
_LONG_FLAGS: Final = ("amt", "nocolor", "current", "last", "magpie", "prompt", "sanity", "time", "help")
_LONG_VALUED: Final = ("daysback", "filter", "user")
_PYTHON_LONG_VALUED: Final = ("found-in-slurm",)
_R_LONG: Final = (*_LONG_FLAGS, *_LONG_VALUED)
_VALUED: Final = (*_LONG_VALUED, *_PYTHON_LONG_VALUED)
_ARGV_END: Final = "--"
_NEGATIVE_NUMBER: Final = re.compile(r"^-[0-9]*\.?[0-9]+([eE][-+]?[0-9]+)?$")
"""optparse's ``is_negative_number``: the only tokens starting with ``-`` that a flag takes as its value."""


class OptparseError(Exception):
    """A usage error as optparse raises it; ``str(error)`` is its message (``short flag "z" is invalid``)."""


def normalize_argv(argv: Sequence[str]) -> list[str]:
    """Rewrite ``argv`` into the one-flag-per-token form that typer and optparse read identically.

    optparse 1.8.2 (``getopt``, probed on 2026-10-02 with the option list of ``R/commandLineInterface.R``
    lines 35-47): the first ``--`` ends the options; ``normalize_opt`` then expands every short bundle into
    one flag per character (``-mltpbf``), except a negative number right after a flag that takes a value
    (``-d -15`` keeps ``-15``, ``-A -15`` becomes ``-1 -5`` and fails on ``1``); a flag's value is the next
    token, which must exist and must not be a flag: ``-`` and a negative number pass (``-d -1``,
    ``-d -1.5e2``, ``-f -1``), ``-fA``, ``-u -d`` and ``-d -x`` do not; a digit-led bundle that is not a
    number was expanded first, so ``-d -1b`` is ``-d -1 -b`` and ``-d -1x`` fails on ``x``; long flags take
    ``--name value`` or ``--name=value`` and may be abbreviated to a unique prefix. Errors carry optparse's
    messages: ``short flag "z" is invalid``, ``long flag "bogus" is invalid``, ``short flag "f" requires an
    argument``, ``long flag "amt" accepts no arguments``. ``--found-in-slurm`` exists only here and is
    accepted spelled out.
    """
    tokens = list(argv)
    end = tokens.index(_ARGV_END) if _ARGV_END in tokens else len(tokens)
    options = _expand_bundles(tokens[:end])
    normalized: list[str] = []
    index = 0
    while index < len(options):
        token = options[index]
        index += 1
        if _is_long_flag(token):
            index = _long_option(token, options, index, normalized)
        elif _is_short_flag(token):
            index = _short_option(token, options, index, normalized)
        else:
            normalized.append(token)
    normalized.extend(tokens[end:])
    return normalized


def _is_long_flag(token: str) -> bool:
    """``is_long_flag`` of optparse: ``--`` and at least one more character."""
    return token.startswith("--") and len(token) > 2


def _is_short_flag(token: str) -> bool:
    """``is_short_flag`` of optparse: ``-`` and exactly one more character (``-A``, ``-1``, ``-=``)."""
    return token.startswith("-") and not token.startswith("--") and len(token) == 2


def _is_bundle(token: str) -> bool:
    """``is_bundle`` of optparse: ``-`` and at least two more characters (``-mltpbf``, ``-15``, ``-1b``)."""
    return token.startswith("-") and not token.startswith("--") and len(token) > 2


def _expand_bundles(tokens: Sequence[str]) -> list[str]:
    """``normalize_opt`` of optparse: one token per short flag; a negative number stays after a valued one."""
    expanded: list[str] = []
    for token in tokens:
        number = _NEGATIVE_NUMBER.match(token) is not None
        if _is_bundle(token) and not (number and expanded and _takes_value(expanded[-1])):
            expanded.extend(f"-{letter}" for letter in token[1:])
        else:
            expanded.append(token)
    return expanded


def _takes_value(flag: str) -> bool:
    """``prev_takes_argument`` of optparse: a known flag with a ``store`` action (``-d``, ``--da``)."""
    if _is_long_flag(flag) and "=" not in flag:
        try:
            return _long_name(flag[2:]) in _VALUED
        except OptparseError:
            return False
    return _is_short_flag(flag) and flag[1] in _SHORT_VALUED


def _long_option(token: str, tokens: Sequence[str], index: int, normalized: list[str]) -> int:
    """``--name``, ``--name=value`` or ``--name value``; returns the index after the consumed value."""
    name, has_value, value = token[2:].partition("=")
    full = _long_name(name)
    if full in _VALUED:
        if has_value:
            normalized.append(f"--{full}={value}")
            return index
        normalized.extend((f"--{full}", _value_token(tokens, index, f'long flag "{full}"')))
        return index + 1
    if has_value:
        raise OptparseError(f'long flag "{full}" accepts no arguments')
    normalized.append(f"--{full}")
    return index


def _long_name(name: str) -> str:
    """The option a long flag names: exact, else a unique prefix of an R option (``--da``: ``daysback``)."""
    if name in _R_LONG or name in _PYTHON_LONG_VALUED:
        return name
    candidates = [option for option in _R_LONG if name and option.startswith(name)]
    if len(candidates) == 1:
        return candidates[0]
    raise OptparseError(f'long flag "{name}" is invalid')


def _short_option(token: str, tokens: Sequence[str], index: int, normalized: list[str]) -> int:
    """``-m`` or ``-f value`` (the bundles are expanded already); returns the index after a consumed value."""
    letter = token[1]
    if letter in _SHORT_VALUED:
        normalized.extend((token, _value_token(tokens, index, f'short flag "{letter}"')))
        return index + 1
    if letter not in _SHORT_FLAGS:
        raise OptparseError(f'short flag "{letter}" is invalid')
    normalized.append(token)
    return index


def _value_token(tokens: Sequence[str], index: int, flag: str) -> str:
    """The next token as an option value, as ``getopt`` peeks at it: it must exist and must not be a flag.

    Not a long flag, and a negative number or not a short flag: ``-``, ``-1``, ``-15``, ``-.5``, ``x`` and
    ``""`` pass; ``-A``, ``-x``, ``--1`` and the end of the options do not.
    """
    if index < len(tokens):
        candidate = tokens[index]
        if not _is_long_flag(candidate) and (
            _NEGATIVE_NUMBER.match(candidate) is not None or not _is_short_flag(candidate)
        ):
            return candidate
    raise OptparseError(f"{flag} requires an argument")


def usage_error_text(message: str) -> str:
    """What optparse prints for a usage error: the usage block, two blank lines, ``NA: error: <message>``."""
    return f"Usage: {USAGE}\n\n{USAGE_ERROR_PROGRAM}: error: {message}\n"


def optparse_message(error: UsageError) -> str:
    """The optparse wording of a usage error typer raised after :func:`normalize_argv` (a fallback)."""
    name = getattr(error, "option_name", None) or ""
    kind = "long flag" if name.startswith("--") else "short flag"
    bare = name.lstrip("-")
    if isinstance(error, NoSuchOption):
        return f'{kind} "{bare}" is invalid'
    if isinstance(error, BadOptionUsage):
        if "does not take a value" in error.message:
            return f'{kind} "{bare}" accepts no arguments'
        return f'{kind} "{bare}" requires an argument'
    return error.message


# ---------------------------------------------------------------------------------------------
# The -d value: optparse's type = "integer" (R/commandLineInterface.R:39)
# ---------------------------------------------------------------------------------------------

_R_DECIMAL: Final = r"(?:\d+\.?\d*|\.\d+)(?:[eE][+-]?\d+)?"
_R_HEX: Final = r"0[xX](?:[0-9a-fA-F]+\.?[0-9a-fA-F]*|\.[0-9a-fA-F]+)(?:[pP][+-]?\d+)?"
_R_NUMBER: Final = re.compile(rf"^[+-]?(?:{_R_HEX}|{_R_DECIMAL}|(?i:inf|infinity|nan))$")
"""What ``R_strtod`` reads whole: a decimal, a hexadecimal with a binary exponent, ``Inf``, ``Infinity``,
``NaN``."""
_R_NAN: Final = re.compile(r"^[+-]?(?i:nan)$")
_R_SPACE: Final = " \t\n\r\f\v"
_INT_MAX: Final = 2_147_483_647
_INTEGER_EXPECTED_CALL: Final = "value[[3L]](cond)"
_INTEGER_EXPECTED_BLANK_CALL: Final = (
    "getopt(spec = spec, opt = args, operand = operand, defaults = defaults, "
)
_COERCION_CALL: Final = 'paste0("sacct -u ", opt$user, " -s cd,f,cancelled,timeout,oom -S ", '
_NAS_INTRODUCED: Final = "NAs introduced by coercion"
_DAYSBACK_IF_CALL: Final = "if (opt$daysback > 0) {"


@dataclass(frozen=True)
class OptparseInteger:
    """What optparse makes of a ``type = "integer"`` value (``-d N``, ``R/commandLineInterface.R:39``).

    optparse converts with ``storage.mode(value) <- "integer"`` inside a ``tryCatch(warning = ...)``: a value
    ``as.integer()`` reads (``5``, ``5.5``, ``1e1``, ``0x10``, blanks around) is that integer; a value it
    warns about (``x``, ``NA``, ``99999999999``, ``1e999``) is left as the *string*, with the warning
    ``integer expected, got “x”``; a value that is silently ``NA`` (an empty string, ``NaN``) stays ``NA``
    with getopt's own warning. ``value`` is the integer, the kept string or ``None`` for ``NA``; ``warnings``
    are what optparse raised (``R/commandLineInterface.R`` then compares the string with ``0`` and ``1`` as
    R does, pastes ``as.numeric()`` of it into the ``sacct`` date and prints the string in the "No runs found
    in the past x days" message: goldens ``daysback-nonint`` and ``daysback-overflow``).
    """

    text: str
    value: int | str | None
    warnings: tuple[DeferredWarning, ...] = ()

    @classmethod
    def parse(cls, text: str, *, fancy_quotes: bool) -> OptparseInteger:
        """``storage.mode(text) <- "integer"`` under optparse's handler; ``fancy_quotes`` is ``dQuote``'s."""
        stripped = text.strip(_R_SPACE)
        if stripped == "" or _R_NAN.match(stripped):
            return cls(
                text, None, (DeferredWarning(_INTEGER_EXPECTED_BLANK_CALL, _expected(text, fancy_quotes)),)
            )
        value = _r_integer(stripped)
        if value is None:
            return cls(text, text, (DeferredWarning(_INTEGER_EXPECTED_CALL, _expected(text, fancy_quotes)),))
        return cls(text, value)

    def greater_than(self, other: int) -> bool:
        """``opt$daysback > other`` (``:144``, ``:179``): numeric, or R's string collation for a kept string.

        Raises:
            RError: ``if (NA > 0)`` is R's ``missing value where TRUE/FALSE needed``.
        """
        if self.value is None:
            raise RError(MISSING_VALUE_IN_IF, call=_DAYSBACK_IF_CALL, traceback=("commandLineInterface",))
        if isinstance(self.value, str):
            return r_collation_key(self.value) > r_collation_key(str(other))
        return self.value > other

    def less_than(self, other: int) -> bool:
        """``opt$daysback < other`` with the same rules as :meth:`greater_than`."""
        if self.value is None:
            raise RError(MISSING_VALUE_IN_IF, call=_DAYSBACK_IF_CALL, traceback=("commandLineInterface",))
        if isinstance(self.value, str):
            return r_collation_key(self.value) < r_collation_key(str(other))
        return self.value < other

    @property
    def shown(self) -> str:
        """``{opt$daysback}`` in the glue string: the integer, or the kept text."""
        return self.text if isinstance(self.value, str) else str(self.value)


def r_as_numeric(text: str) -> float | None:
    """``as.numeric()`` of a string: a double, ``inf`` and ``nan`` included, or ``None`` for R's ``NA``.

    Blanks around are ignored; the syntax is a decimal with an optional exponent, a hexadecimal with an
    optional fraction and binary exponent (``0x1.8p1`` is 3), or ``Inf``, ``Infinity`` and ``NaN`` in any
    case, each with one optional sign; ``1e999`` and a hexadecimal too large for a double are ``Inf``;
    ``1d5``, ``.``, ``0x``, ``NA`` and ``TRUE`` are ``NA`` (probed in R 4.6.1 on 2026-10-02).
    """
    stripped = text.strip(_R_SPACE)
    if not _R_NUMBER.match(stripped):
        return None
    sign = -1.0 if stripped.startswith("-") else 1.0
    body = stripped.lstrip("+-")
    lower = body.lower()
    if lower == "nan":
        return math.nan
    if lower in ("inf", "infinity"):
        return sign * math.inf
    if lower.startswith("0x"):
        try:
            return sign * float.fromhex(body)
        except OverflowError:
            return sign * math.inf
    return sign * float(body)


def _r_integer(text: str) -> int | None:
    """``as.integer()`` of a trimmed string: R's number truncated toward zero; ``None`` is R's ``NA``.

    ``NA`` with the coercion warning, so that optparse keeps the string, for a non-number, for ``Inf`` and
    for a value outside the integer range, ``1e999`` and a hexadecimal literal of any length included
    (probed in R 4.6.1 on 2026-10-02); ``NaN`` is a silent ``NA`` like the empty string, which
    :meth:`OptparseInteger.parse` handles first.
    """
    number = r_as_numeric(text)
    if number is None or not math.isfinite(number):
        return None
    value = int(number)
    return value if abs(value) <= _INT_MAX else None


def _expected(text: str, fancy_quotes: bool) -> str:
    """``paste(mode, "expected, got", dQuote(val))``: curly quotes in a UTF-8 locale."""
    quoted = f"“{text}”" if fancy_quotes else f'"{text}"'
    return f"integer expected, got {quoted}"


# ---------------------------------------------------------------------------------------------
# The parsed command line and the session it runs in
# ---------------------------------------------------------------------------------------------


@dataclass(frozen=True)
class Options:
    """``arguments$options`` and the one positional argument (``R/commandLineInterface.R:99, 106-107``)."""

    path: str | None = None
    amt: bool = False
    nocolor: bool = False
    current: bool = False
    daysback: OptparseInteger = field(default_factory=lambda: OptparseInteger("0", 0))
    filter: str = FILTER_DEFAULT
    last: bool = False
    magpie: bool = False
    prompt: bool = False
    sanity: bool = False
    time: bool = False
    user: str = USER_DEFAULT


@dataclass(frozen=True)
class Session:
    """Where one ``rs`` invocation runs: the environment, the streams, stdin, the colour states, the dice.

    ``table_crayon`` is crayon's state for stdout (``loopRuns`` and ``getSanityChecks`` paint with it,
    ``migration/02-python-libraries.md`` 4.5), ``alert_crayon`` cli's for stderr, where the alerts go;
    ``read_line`` feeds ``-p``; ``pick_hint`` is ``sample(hints, 1)``.
    """

    effects: Effects
    out: TextIO
    err: TextIO
    table_crayon: Crayon
    alert_crayon: Crayon
    read_line: LineReader = read_stdin_line
    pick_hint: Callable[[Sequence[str]], str] = random.choice

    @classmethod
    def production(cls) -> Session:
        """The real environment and streams; stdout and stderr pass undecodable names through."""
        out, err = sys.stdout, sys.stderr
        for stream in (out, err):
            if isinstance(stream, io.TextIOWrapper):
                stream.reconfigure(errors="surrogateescape")
        return cls(
            effects=ProductionEffects(),
            out=out,
            err=err,
            table_crayon=Crayon.from_terminal(is_tty=out.isatty(), environ=os.environ),
            alert_crayon=Crayon.from_terminal(is_tty=err.isatty(), environ=os.environ),
        )


# ---------------------------------------------------------------------------------------------
# cli_alert_info / cli_alert_warning (migration/02-python-libraries.md 4.7)
# ---------------------------------------------------------------------------------------------


class AlertKind(Enum):
    """The two cli alerts the command line uses, with their ASCII and UTF-8 bullets."""

    INFO = ("i", "ℹ")
    WARNING = ("!", "!")

    def bullet(self, utf8: bool) -> str:
        """The symbol cli prints (``ℹ`` only when ``is_utf8_output()``)."""
        return self.value[1] if utf8 else self.value[0]


def utf8_output(effects: Effects) -> bool:
    """``is_utf8_output()`` of cli: ``l10n_info()$`UTF-8```, the C locale's ``LC_CTYPE`` category.

    glibc takes ``LC_ALL``, else ``LC_CTYPE``, else ``LANG``; the category is UTF-8 when its name says so
    (``C.utf8``, ``en_US.UTF-8``); ``C``, ``POSIX`` and no setting are ASCII. R's ``cli.unicode`` option is
    not reproduced.
    """
    for name in ("LC_ALL", "LC_CTYPE", "LANG"):
        value = effects.getenv(name)
        if value:
            return "utf-8" in value.lower() or "utf8" in value.lower()
    return False


def alert_line(kind: AlertKind, text: str, crayon: Crayon, utf8: bool) -> str:
    """One ``cli_alert_info`` / ``cli_alert_warning`` line as cli 3.6.6 writes it to stderr.

    The bullet is cyan for info and yellow for a warning when colours are on (``migration/harness/checks/
    cli_alerts.R``); the text follows a single space, trailing newlines of the glue string are not kept.
    """
    bullet = kind.bullet(utf8)
    painted = crayon.cyan(bullet) if kind is AlertKind.INFO else crayon.yellow(bullet)
    return f"{painted} {text.rstrip(chr(10))}\n"


def file_span(paths: Sequence[str], crayon: Crayon) -> str:
    """``{.file {paths}}``: each path blue (cli doubles the open code) or quoted, joined with ``and``."""
    if crayon.enabled:
        pieces = [f"\x1b[34m\x1b[34m{path}\x1b[34m\x1b[39m" for path in paths]
    else:
        pieces = [f"'{path}'" for path in paths]
    if len(pieces) <= 2:
        return " and ".join(pieces)
    return ", ".join(pieces[:-1]) + ", and " + pieces[-1]


# ---------------------------------------------------------------------------------------------
# R's top-level error printing (src/main/errors.c verrorcall_dflt, R_ConciseTraceback, PrintWarnings)
# ---------------------------------------------------------------------------------------------

_LONG: Final = 75
_ERROR_WIDTH_OFFSET: Final = 14
_EXECUTION_HALTED: Final = "Execution halted\n"
_CALLS_SEPARATOR: Final = " -> "
_NO_CALL_TRACEBACK: Final[tuple[str, ...]] = ()
_TRE_REASON: Final = re.compile(r"reason '(.*)'$")


class RError(Exception):
    """``commandLineInterface`` stopped with an R error: what ``Rscript`` prints and exit status 1.

    ``call`` is the error's call as ``deparse()`` prints it (``None`` for ``Error: <message>``), ``traceback``
    the functions of the ``Calls:`` line (empty when R would omit it or the port does not know it),
    ``warnings`` the deferred warnings raised by the failing step itself (the global ones are added by
    :func:`run_rs`).
    """

    def __init__(
        self,
        message: str,
        *,
        call: str | None = None,
        traceback: Sequence[str] = _NO_CALL_TRACEBACK,
        warnings: Sequence[DeferredWarning] = (),
    ) -> None:
        """``message`` is R's condition message."""
        super().__init__(message)
        self.message = message
        self.call = call
        self.traceback = tuple(traceback)
        self.warnings = tuple(warnings)


def error_line(call: str | None, message: str) -> str:
    r"""``Error in <call> : <message>\n`` as R's default error handler prints it, or ``Error: <message>\n``.

    The message moves to its own line, indented by two blanks, when ``14 + width(call) + width(first line)
    > 75`` (``verrorcall_dflt``, the rule of ``try()`` in :func:`modelstats.loop_runs.try_error_message`).
    """
    if call is None:
        return f"Error: {message}\n"
    head = f"Error in {call} : "
    if _ERROR_WIDTH_OFFSET + len(call) + len(message.split("\n", 1)[0]) > _LONG:
        head += "\n  "
    return f"{head}{message}\n"


def halted_text(abort: RError, warnings: Sequence[DeferredWarning]) -> str:
    """Everything ``Rscript`` writes to stderr for an uncaught error, ``Execution halted`` included."""
    text = error_line(abort.call, abort.message)
    if abort.traceback:
        text += f"Calls: {_CALLS_SEPARATOR.join(abort.traceback)}\n"
    return text + deferred_warnings_message([*warnings, *abort.warnings]) + _EXECUTION_HALTED


def top_level_warnings(warnings: Sequence[DeferredWarning]) -> str:
    """``PrintWarnings`` at the top level: the ``Warning message(s):`` block without ``In addition:``."""
    return warnings_block(warnings, in_addition=False)


def _tre_warning(call: str, message: str) -> tuple[DeferredWarning, ...]:
    """The ``TRE pattern compilation error`` warning R raises under ``call`` beside an invalid-regex error."""
    reason = _TRE_REASON.search(message)
    if reason is None:
        return ()
    return (DeferredWarning(call, f"TRE pattern compilation error '{reason.group(1)}'"),)


# ---------------------------------------------------------------------------------------------
# The decision tree (R/commandLineInterface.R:95-266)
# ---------------------------------------------------------------------------------------------

_SLURM_CALLS: Final = (
    'system(paste0("squeue -u ", opt$user, " -h -o \'%Z\'"), intern = TRUE)',
    'system(paste0("squeue -u ", opt$user, " -h -o \'%j\'"), intern = TRUE)',
    'system(paste(sacctcode, "--format WorkDir"), intern = TRUE)',
    'system(paste(sacctcode, "--format JobName"), intern = TRUE)',
)
"""The four ``system()`` calls of ``-C`` (``:141-147``) as R deparses them in a warning, in call order."""

_GREP_CALL: Final = "grep(opt$filter, runfolders, value = TRUE)"
_GREPL_CALL: Final = "grepl(runnames[[i]], myruns[[i]])"
_GREPL_CALL_TRACEBACK: Final = ("commandLineInterface", "grepl")
_SUBSCRIPT_TRACEBACKS: Final[dict[str, tuple[str, ...]]] = {
    RUNNAMES_CALL: ("commandLineInterface", "grepl"),
    MYRUNS_CALL: ("commandLineInterface", "grepl", "is.factor"),
}
"""The ``Calls:`` line of ``subscript out of bounds`` in ``grepl(runnames[[i]], myruns[[i]])`` (``:165``,
BUG-028): ``grepl`` forces ``x`` first, through ``is.factor``, so a missing work directory has the longer
traceback (probed in R 4.6.1 on 2026-10-02)."""
_GREP_TRACEBACK: Final = ("commandLineInterface", "grep")
_READRDS_CALL: Final = 'gzfile(file, "rb")'
_READRDS_TRACEBACK: Final = ("commandLineInterface", "readRDS", "gzfile")
_ANONYMOUS_TRACEBACK: Final = ("commandLineInterface", "<Anonymous>")
_NORMALIZE_PATH_CALL: Final = "normalizePath(dirs, mustWork = TRUE)"
_NORMALIZE_PATH_TRACEBACK: Final = ("commandLineInterface", "<Anonymous>", "normalizePath")
_JOB_IN_SLURM_CALL: Final = 'status[["jobInSLURM"]]'
_NO_SUCH_FILE: Final = "No such file or directory"


def run_rs(options: Options, session: Session) -> int:
    """``commandLineInterface(argv)`` after parsing: run, print, and return the process exit status.

    An R error (:class:`RError`) is printed with :func:`halted_text` and gives 1; otherwise the deferred
    warnings are printed last (R's top level) and the status is 0, the ``quit(status = 0)`` branches
    included.
    """
    warnings: list[DeferredWarning] = []
    try:
        status = _command_line_interface(options, session, warnings)
    except RError as abort:
        session.err.write(halted_text(abort, warnings))
        return 1
    session.err.write(top_level_warnings(warnings))
    return status


def _command_line_interface(options: Options, session: Session, warnings: list[DeferredWarning]) -> int:
    """The decision tree in R's order; ``warnings`` collects what R would defer to the end."""
    effects = session.effects
    _announce(session)
    user: str | None = effects.user if options.user == USER_DEFAULT else options.user  # :109
    paths = split_comma_list(DEFAULT_PATH if options.path is None else options.path)  # :112-115
    pattern = "|".join(split_comma_list(options.filter))  # :116
    warnings.extend(options.daysback.warnings)
    if options.amt:  # :125-136
        user = None
        paths, pattern = _amt_selection(pattern, session)
    if options.current:  # :139-186
        runfolders = _current_run_folders("" if user is None else user, options.daysback, session, warnings)
        if runfolders is None:
            return 0
    else:  # :190-202
        runfolders = _run_folders_of(paths, effects)
    if options.magpie and runfolders is not None:  # :207-228
        runfolders = filter_coupled(runfolders, effects)
        if options.last:
            runfolders = last_iterations(runfolders) or None
        if runfolders is None:
            _warn(NO_COUPLED_RUNS, session)
            return 0
    runfolders = natural_order(_r_grep(pattern, runfolders or [], warnings), effects)  # :235-238
    if not runfolders:  # :263-265
        _warn(NO_RUNS_FOUND, session)
        return 0
    if len(runfolders) > MANY_RUNS and pattern == FILTER_DEFAULT and not options.prompt:  # :244-246
        _info(MANY_RUNS_HINT, session)
    if options.prompt:  # :249-251
        runfolders = _choose(runfolders, session)
    _info(f"Runs found: {len(runfolders)}", session)  # :253
    if options.sanity:  # :256-260
        _sanity_table(runfolders, session)
    else:
        _status_table(runfolders, user, options, session)
    return 0


def _announce(session: Session) -> None:
    """``:102-104``: the two update lines and the random hint."""
    for line in UPDATE_LINES:
        _info(line, session)
    _info(f"{DID_YOU_KNOW}{session.pick_hint(HINTS)}", session)


def _info(text: str, session: Session) -> None:
    session.err.write(alert_line(AlertKind.INFO, text, session.alert_crayon, utf8_output(session.effects)))


def _warn(text: str, session: Session) -> None:
    session.err.write(alert_line(AlertKind.WARNING, text, session.alert_crayon, utf8_output(session.effects)))


def _amt_selection(pattern: str, session: Session) -> tuple[list[str], str]:
    """``-A`` (``:125-136``): the AMT output directory, the cycle's runcode as the filter, or the archive too.

    Raises:
        RError: ``readRDS`` on a missing ``runcode.rds`` (``cannot open the connection`` with the ``gzfile``
            warning, golden ``amt--local``); a runcode that is not one string is ``grep()``'s
            ``invalid 'pattern' argument``.
    """
    cluster = session.effects.cluster_paths
    paths = [cluster.amt_output]
    if pattern == FILTER_DEFAULT:
        try:
            pattern = amt_runcode(session.effects)
        except SanityError as error:
            raise _amt_abort(error, cluster.amt_runcode) from error
    else:
        paths.append(cluster.amt_archive)
    _info(f"Results from {file_span(paths, session.alert_crayon)}", session)
    return paths, pattern


def _amt_abort(error: SanityError, runcode_path: str) -> RError:
    """The R error behind a failed runcode lookup."""
    message = str(error)
    if message == CANNOT_OPEN_CONNECTION:
        warning = DeferredWarning(
            _READRDS_CALL, f"cannot open compressed file '{runcode_path}', probable reason '{_NO_SUCH_FILE}'"
        )
        return RError(message, call=_READRDS_CALL, traceback=_READRDS_TRACEBACK, warnings=(warning,))
    if message == INVALID_PATTERN:
        return RError(message, call=_GREP_CALL, traceback=_GREP_TRACEBACK)
    return RError(message)


def _current_run_folders(
    user: str, daysback: OptparseInteger, session: Session, warnings: list[DeferredWarning]
) -> list[str] | None:
    """``-C`` (``:139-186``): the run folders of the scheduler, or ``None`` after the "no runs" warning.

    The scheduler calls come first (``:141-147``) and their diagnostics are kept before the answers are
    processed (``:150-176``), as in R: the scheduler's own stderr is passed through at once, as ``system()``
    does, and the warnings of failed calls are deferred, so that an error of the processing prints them
    under ``In addition:``. ``-A -C`` queries with the empty user (``paste0`` of R's ``NULL``).

    Raises:
        RError: an invalid regular expression in a job name (BUG-008) with the ``grepl`` traceback and TRE's
            warning (golden ``current--regex-bracket``); ``subscript out of bounds`` for answers of different
            lengths (BUG-028) with the call and traceback ``Rscript`` prints; R's other errors of this branch
            by message.
    """
    positive = daysback.greater_than(0)  # :144
    since: str | None = None
    if positive and isinstance(daysback.value, str):
        # as.numeric(opt$daysback) (:145) of a string optparse kept: NA with a coercion warning for "x"
        # (golden daysback-nonint), Inf for "1e999" (golden daysback-overflow), a double for "99999999999";
        # R pastes the date minus that number into the sacct command (BUG-066) and the sacct calls happen.
        number = r_as_numeric(daysback.text)
        if number is None:
            warnings.append(DeferredWarning(_COERCION_CALL, _NAS_INTRODUCED))
        since = sacct_since_text(session.effects.now().date(), number)
    days = daysback.value if positive and isinstance(daysback.value, int) else int(since is not None)
    try:
        calls = scheduler_calls(user, days, session.effects, since=since)
        _forward_scheduler_diagnostics(calls, session, warnings)
        runs = run_folders_of(calls, session.effects)
    except SlurmError as error:
        raise _current_runs_error(error) from error
    if runs:
        return list(runs)
    if daysback.less_than(1):  # :179-183
        _warn(NO_CURRENT_RUNS, session)
    else:
        _warn(f"No runs found in the past {daysback.shown} days. Try to expand the time horizon.", session)
    return None


def _forward_scheduler_diagnostics(
    calls: Sequence[SlurmCall], session: Session, warnings: list[DeferredWarning]
) -> None:
    """What ``system(intern = TRUE)`` leaves behind: the child's stderr now, the status warning deferred."""
    for call, r_call in zip(calls, _SLURM_CALLS, strict=False):
        session.err.write(call.result.stderr)
        if call.warning is not None:
            warnings.append(DeferredWarning(r_call, call.warning))


def _current_runs_error(error: SlurmError) -> RError:
    """The R error behind a failed ``-C``, with the call and the ``Calls:`` line ``Rscript`` prints.

    ``grepl(runnames[[i]], myruns[[i]])`` (``:165``) for an invalid job name (BUG-008, with TRE's warning);
    ``runnames[[i]]`` or ``myruns[[i]]`` for ``subscript out of bounds`` (BUG-028, the tracebacks of
    :data:`_SUBSCRIPT_TRACEBACKS`); any other error by message alone.
    """
    message = str(error)
    if _TRE_REASON.search(message) is not None:
        warnings = _tre_warning(_GREPL_CALL, message)
        return RError(message, call=_GREPL_CALL, traceback=_GREPL_CALL_TRACEBACK, warnings=warnings)
    traceback = _SUBSCRIPT_TRACEBACKS.get(error.call or "", _NO_CALL_TRACEBACK)
    return RError(message, call=error.call, traceback=traceback)


def _run_folders_of(paths: Sequence[str], effects: Effects) -> list[str] | None:
    """``:192-202``: the run folders below the given paths; ``None`` is R's ``NULL`` for no path at all."""
    if not paths:
        return None
    return expand_paths(",".join(paths), effects)


def _r_grep(pattern: str, runfolders: Sequence[str], warnings: list[DeferredWarning]) -> list[str]:
    """``grep(opt$filter, runfolders, value = TRUE)`` (``:235``); the pattern is compiled even for no folders.

    Raises:
        RError: an invalid regular expression, with ``grep``'s traceback and TRE's warning.
    """
    try:
        if not runfolders:
            r_grepl(pattern, "", call=_GREP_CALL)
            return []
        return [folder for folder in runfolders if r_grepl(pattern, folder, call=_GREP_CALL)]
    except SlurmError as error:
        tre = _tre_warning(_GREP_CALL, str(error))
        raise RError(str(error), call=_GREP_CALL, traceback=_GREP_TRACEBACK, warnings=tre) from error


def _choose(runfolders: Sequence[str], session: Session) -> list[str]:
    """``gms::chooseFromList(runfolders, type = "folders")`` (``:250``) on the session's input (D-21)."""
    try:
        return choose_from_list(runfolders, kind="folders", read_line=session.read_line, err=session.err)
    except NoMoreInputError as error:
        raise RError(str(error)) from error


def _sanity_table(runfolders: Sequence[str], session: Session) -> None:
    """``modelstats::getSanityChecks(runfolders)`` (``:258``) with R's errors mapped to :class:`RError`."""
    try:
        get_sanity_checks(runfolders, session.effects, session.out, crayon=session.table_crayon)
    except SanityError as error:
        if str(error).startswith("path["):
            raise RError(
                str(error), call=_NORMALIZE_PATH_CALL, traceback=_NORMALIZE_PATH_TRACEBACK
            ) from error
        raise RError(str(error)) from error
    except RunReadError as error:
        raise RError(str(error), call=error.call, warnings=error.warnings) from error
    except (PrintOutputError, ColourRuleError) as error:
        raise RError(str(error)) from error


def _status_table(runfolders: Sequence[str], user: str | None, options: Options, session: Session) -> None:
    """``modelstats::loopRuns(runfolders, user, colors, sortbytime)`` (``:260``), autoprinting its value."""
    try:
        result = loop_runs(
            runfolders,
            user=user,
            colors=not options.nocolor,
            sortbytime=options.time,
            effects=session.effects,
            out=session.out,
            err=session.err,
            crayon=session.table_crayon,
        )
    except LoopRunsError as error:
        if str(error) == SUBSCRIPT_OUT_OF_BOUNDS:  # BUG-005 (golden synthetic-bug005)
            raise RError(str(error), call=_JOB_IN_SLURM_CALL, traceback=_ANONYMOUS_TRACEBACK) from error
        raise RError(str(error)) from error
    except (PrintOutputError, ColourRuleError) as error:
        raise RError(str(error)) from error
    if result == NO_RUNS_FOUND:
        session.out.write(AUTOPRINTED_NO_RUNS)


# ---------------------------------------------------------------------------------------------
# rs --found-in-slurm DIR (migration/03-migration-plan.md section 5)
# ---------------------------------------------------------------------------------------------


def found_in_slurm_command(directory: str, session: Session) -> int:
    """Print ``foundInSlurm(directory)`` plus a newline, nothing else; on error the message on stderr, 1."""
    try:
        value = found_in_slurm(directory, session.effects)
    except (ModelstatsError, SlurmError, OSError) as error:
        session.err.write(f"rs --found-in-slurm: {error}\n")
        return 1
    session.out.write(f"{value}\n")
    return 0


# ---------------------------------------------------------------------------------------------
# The typer application
# ---------------------------------------------------------------------------------------------


class RsApp(typer.Typer):
    """The ``rs`` application: optparse's reading of ``argv`` in front of typer, usage errors in its words.

    ``app()`` returns the exit status the console script passes to ``sys.exit`` (never ``sys.exit`` itself),
    so the command runs in typer's non-standalone mode: the help option returns 0, a usage error prints
    :func:`usage_error_text` on stderr and returns 1 (D-12 pending: R's exit status).
    """

    def __call__(self, *args: Any, **kwargs: Any) -> int:
        """Parse ``kwargs["args"]`` (default ``sys.argv[1:]``) and run; returns the exit status."""
        argv = kwargs.pop("args", None)
        tokens = list(sys.argv[1:] if argv is None else argv)
        try:
            status = super().__call__(*args, args=normalize_argv(tokens), standalone_mode=False, **kwargs)
        except OptparseError as error:
            sys.stderr.write(usage_error_text(str(error)))
            return 1
        except UsageError as error:
            sys.stderr.write(usage_error_text(optparse_message(error)))
            return 1
        return int(status or 0)


def make_app(session_factory: Callable[[], Session]) -> RsApp:
    """The typer application whose single command runs :func:`run_rs` in ``session_factory()``.

    The module-level :data:`app` uses :meth:`Session.production`; tests pass a session over
    :class:`modelstats.env.FakeEffects` and in-memory streams.
    """
    application = RsApp(
        name="rs",
        add_completion=False,
        rich_markup_mode=None,
        pretty_exceptions_enable=False,
        context_settings={"help_option_names": ["-h", "--help"], "allow_extra_args": True},
    )

    @application.command(name="rs", help=HELP_DESCRIPTION, epilog=HELP_EPILOGUE)
    def rs(
        ctx: typer.Context,
        path: Annotated[str | None, typer.Argument(metavar="PATH", show_default=False)] = None,
        amt: Annotated[bool, typer.Option("-A", "--amt", help=OPTION_HELP["amt"])] = False,
        nocolor: Annotated[bool, typer.Option("-b", "--nocolor", help=OPTION_HELP["nocolor"])] = False,
        current: Annotated[bool, typer.Option("-C", "--current", help=OPTION_HELP["current"])] = False,
        daysback: Annotated[
            str, typer.Option("-d", "--daysback", metavar="N", help=OPTION_HELP["daysback"])
        ] = "0",
        filter_: Annotated[
            str, typer.Option("-f", "--filter", metavar="REGEX", help=OPTION_HELP["filter"])
        ] = FILTER_DEFAULT,
        last: Annotated[bool, typer.Option("-l", "--last", help=OPTION_HELP["last"])] = False,
        magpie: Annotated[bool, typer.Option("-m", "--magpie", help=OPTION_HELP["magpie"])] = False,
        prompt: Annotated[bool, typer.Option("-p", "--prompt", help=OPTION_HELP["prompt"])] = False,
        sanity: Annotated[bool, typer.Option("-s", "--sanity", help=OPTION_HELP["sanity"])] = False,
        time: Annotated[bool, typer.Option("-t", "--time", help=OPTION_HELP["time"])] = False,
        user: Annotated[
            str, typer.Option("-u", "--user", metavar="USER", help=OPTION_HELP["user"])
        ] = USER_DEFAULT,
        found_in_slurm: Annotated[
            str | None,
            typer.Option(
                "--found-in-slurm", metavar="DIR", help=OPTION_HELP["found-in-slurm"], show_default=False
            ),
        ] = None,
    ) -> int:
        """Print run status or sanity of model runs."""
        if ctx.args:  # positional_arguments = c(0, 1) (R/commandLineInterface.R:99)
            raise OptparseError(f"required at most 1 positional arguments, got {1 + len(ctx.args)}")
        session = session_factory()
        if found_in_slurm is not None:
            return found_in_slurm_command(found_in_slurm, session)
        options = Options(
            path=path,
            amt=amt,
            nocolor=nocolor,
            current=current,
            daysback=OptparseInteger.parse(daysback, fancy_quotes=utf8_output(session.effects)),
            filter=filter_,
            last=last,
            magpie=magpie,
            prompt=prompt,
            sanity=sanity,
            time=time,
            user=user,
        )
        return run_rs(options, session)

    return application


app = make_app(Session.production)
"""The console script ``rs`` (``pyproject.toml`` ``[project.scripts]``)."""
