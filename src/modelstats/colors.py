"""ANSI styles of modelstats, byte-exact with crayon, and the row colour decision trees of ``R/loopRuns.R``.

The seven styles the R package uses (``make_style("orangered")`` as ``red``, ``make_style("orange")``, ``yellow``,
``cyan``, ``green``, ``magenta``, ``underline``) are a hand-rolled table: crayon closes a colour with ``ESC[39m``
and underline with ``ESC[24m``, which neither click nor rich reproduce. Nesting is literal concatenation, as in
crayon. Colours are emitted only when explicitly enabled (``set_enabled``), never through tty detection on the
golden path; ``detect_enabled`` / ``detect_num_colors`` implement crayon's rules (crayon 1.5.3's
``num_ansi_colors`` for a Unix terminal: ``R_CLI_NUM_COLORS``, ``NO_COLOR``, tty, ``COLORTERM``, ``tput colors``,
the ``TERM`` guess) for the CLI to call once on a real terminal.
"""

from __future__ import annotations

import math
import re
import subprocess
from collections.abc import Callable, Collection, Mapping, Sequence
from typing import IO, Any

from modelstats.errors import RParityError
from modelstats.formatting import r_num_str

__all__ = [
    "STYLES",
    "STYLES_16",
    "STYLE_NAMES",
    "colour_for_magpie",
    "colour_for_remind",
    "colour_for_row",
    "detect_enabled",
    "detect_num_colors",
    "enable_from_environment",
    "is_enabled",
    "is_magpie_row",
    "num_colors",
    "set_enabled",
    "set_num_colors",
    "style",
]

_CLOSE_COLOR = "\x1b[39m"
_CLOSE_UNDERLINE = "\x1b[24m"

#: (open, close) sequences at 256 colours and truecolor, the depth the R goldens were recorded at
#: (``crayon.colors = 256``; ``02-python-libraries.md`` section 4.5).
STYLES: dict[str, tuple[str, str]] = {
    "orangered": ("\x1b[38;5;202m", _CLOSE_COLOR),
    "orange": ("\x1b[38;5;214m", _CLOSE_COLOR),
    "yellow": ("\x1b[33m", _CLOSE_COLOR),
    "cyan": ("\x1b[36m", _CLOSE_COLOR),
    "green": ("\x1b[32m", _CLOSE_COLOR),
    "magenta": ("\x1b[35m", _CLOSE_COLOR),
    "underline": ("\x1b[4m", _CLOSE_UNDERLINE),
}

#: The same styles on a 16-colour (or 8-colour) terminal: crayon maps the two named colours to red and yellow.
STYLES_16: dict[str, tuple[str, str]] = {
    "orangered": ("\x1b[31m", _CLOSE_COLOR),
    "orange": ("\x1b[33m", _CLOSE_COLOR),
    "yellow": ("\x1b[33m", _CLOSE_COLOR),
    "cyan": ("\x1b[36m", _CLOSE_COLOR),
    "green": ("\x1b[32m", _CLOSE_COLOR),
    "magenta": ("\x1b[35m", _CLOSE_COLOR),
    "underline": ("\x1b[4m", _CLOSE_UNDERLINE),
}

STYLE_NAMES: tuple[str, ...] = tuple(STYLES)

_enabled = False
_num_colors = 256


# ---------------------------------------------------------------------------
# process-wide state
# ---------------------------------------------------------------------------


def set_enabled(enabled: bool, num_colors: int | None = None) -> None:
    """Switch colour output on or off for the process; optionally set the colour depth (256 or 16)."""
    global _enabled
    _enabled = bool(enabled)
    if num_colors is not None:
        set_num_colors(num_colors)


def is_enabled() -> bool:
    """Whether ``style`` currently emits escape sequences."""
    return _enabled


def num_colors() -> int:
    """The colour depth ``style`` formats for; 256 and more use the 256-colour table, less the 16-colour one."""
    return _num_colors


def set_num_colors(num_colors: int) -> None:
    global _num_colors
    _num_colors = int(num_colors)


def style(name: str, text: str) -> str:
    """``text`` wrapped in the named crayon style when colours are enabled, unchanged otherwise.

    Nesting is literal: ``style("yellow", "a" + style("underline", "b") + "c")`` gives
    ``ESC[33m a ESC[4m b ESC[24m c ESC[39m`` like crayon.
    """
    table = STYLES if _num_colors >= 256 else STYLES_16
    try:
        open_sequence, close_sequence = table[name]
    except KeyError:
        raise ValueError(f"unknown style {name!r}; expected one of {', '.join(STYLE_NAMES)}") from None
    if not _enabled:
        return text
    return f"{open_sequence}{text}{close_sequence}"


# ---------------------------------------------------------------------------
# crayon's detection rules
# ---------------------------------------------------------------------------


def _as_int(value: str) -> int | None:
    """R's ``as.integer`` of an environment value: ``None`` for what R would make ``NA``."""
    try:
        return int(value.strip())
    except ValueError:
        try:
            number = float(value.strip())
        except ValueError:
            return None
        return int(number) if math.isfinite(number) else None


def _is_tty(stream: IO[Any] | None) -> bool:
    if stream is None:
        return False
    try:
        return bool(stream.isatty())
    except AttributeError, ValueError, OSError:
        return False


#: ``run(argv) -> stdout text`` for the ``tput colors`` probe; it raises when the command cannot run.
type TputRunner = Callable[[Sequence[str]], str]

_TRUECOLOR = 16777216
_GUESS_TERM = re.compile("^screen|^xterm|^vt100|color|ansi|cygwin|linux", re.IGNORECASE)


def _run_tput(argv: Sequence[str]) -> str:
    """The default ``run``: ``system("tput colors 2>/dev/null", intern = TRUE)`` (stdout only, any exit status)."""
    return subprocess.run(list(argv), capture_output=True, text=True, check=False).stdout


def _as_number(value: str) -> float | None:
    """R's ``as.numeric`` of one line: ``None`` for what R makes ``NA`` (a non-finite result counts as NA too)."""
    try:
        number = float(value.strip())
    except ValueError:
        return None
    return number if math.isfinite(number) else None


def _tput_colors(run: TputRunner | None) -> float | None:
    """``as.numeric(system("tput colors 2>/dev/null", intern = TRUE))[1]`` inside crayon's ``try``: ``None`` is NA."""
    try:
        output = (run or _run_tput)(["tput", "colors"])
    except OSError, subprocess.SubprocessError, ValueError:
        return None
    lines = output.splitlines()
    if not lines:
        return None
    return _as_number(lines[0])


def _guess_tty_colors(term: str) -> int:
    """crayon's ``guess_tty_colors()``.

    ``dumb`` -> 1; ``TERM`` starting with screen / xterm / vt100 or containing color / ansi / cygwin / linux
    (case-insensitive) -> 8; else 1.
    """
    if term == "dumb":
        return 1
    return 8 if _GUESS_TERM.search(term) else 1


def detect_num_colors(
    stream: IO[Any] | None, environ: Mapping[str, str] | None = None, run: TputRunner | None = None
) -> int:
    """crayon's ``num_colors()`` (crayon 1.5.3 ``num_ansi_colors`` + ``detect_tty_colors``) for a Unix terminal.

    In this order: ``R_CLI_NUM_COLORS`` non-empty -> its ``as.integer`` value (a non-numeric value, R's ``NA``,
    counts as 1); ``NO_COLOR`` present -> 1; ``stream`` not a tty -> 1; ``COLORTERM`` present -> 16777216 for
    ``truecolor`` / ``24bit``, else 8; ``tput colors`` through ``run`` (default: a subprocess, stderr discarded;
    a failure, no output or a non-numeric first line is R's ``NA``) -> NA gives ``_guess_tty_colors(TERM)``,
    -1 / 0 / 1 give 1, 8 with ``TERM`` exactly ``xterm`` gives 256, anything else its number. Deliberately not
    ported: the ``cli.num_colors`` / ``crayon.enabled`` / ``crayon.colors`` / ``cli.default_num_colors`` options,
    knitr, sinks, RStudio, Windows and Emacs detection.
    """
    env = _environ(environ)
    forced = env.get("R_CLI_NUM_COLORS", "")
    if forced != "":
        number = _as_int(forced)
        return number if number is not None else 1
    if "NO_COLOR" in env:
        return 1
    if not _is_tty(stream):
        return 1
    if "COLORTERM" in env:
        return _TRUECOLOR if env["COLORTERM"] in ("truecolor", "24bit") else 8
    cols = _tput_colors(run)
    if cols is None:
        return _guess_tty_colors(env.get("TERM", ""))
    if cols in (-1, 0, 1):
        return 1
    if cols == 8 and env.get("TERM", "") == "xterm":
        return 256  # xterm compatible terminals tend to support 256 colours (r-lib/crayon#17)
    return int(cols)


def detect_enabled(
    stream: IO[Any] | None, environ: Mapping[str, str] | None = None, run: TputRunner | None = None
) -> bool:
    """crayon's ``has_color()``: ``num_ansi_colors() > 1`` (see :func:`detect_num_colors`)."""
    return detect_num_colors(stream, environ, run) > 1


def enable_from_environment(
    stream: IO[Any] | None, environ: Mapping[str, str] | None = None, run: TputRunner | None = None
) -> bool:
    """Apply crayon's detection to the process-wide state and return whether colours are on."""
    depth = detect_num_colors(stream, environ, run)
    set_enabled(depth > 1, depth)
    return depth > 1


def _environ(environ: Mapping[str, str] | None) -> Mapping[str, str]:
    if environ is not None:
        return environ
    import os

    return os.environ


# ---------------------------------------------------------------------------
# the two row colour decision trees of loopRuns (R/loopRuns.R)
# ---------------------------------------------------------------------------

# The trees see `status <- unlist(status)`: a named character vector of the status row AFTER loopRuns rewrote
# Runtime (the display string: "pending", "startup", "> 1.5 hours", "NA"), stripped " startup"/" pending" from
# jobInSLURM and shortened testOneRegi to 1Regi. A real NA cell is None here; the row mapping may still hold
# numbers, which unlist() would have coerced to character (r_num_str). `out` is the trimmed printOutput line,
# which the trees grep for "conoptspy >", "Run in progress" and " NA " / "FALSE".

_ERR_NA_CONDITION = "missing value where TRUE/FALSE needed"
_ERR_SUBSCRIPT = "subscript out of bounds"

_MAGPIE_FAIL = re.compile("not_converged|Execution erro|Compilation er|missing|interrupted|Abort")
_MAGPIE_OK = re.compile("converged|Clb_converged")
_REMIND_FAIL = re.compile("not_converged|Execution erro|Compilation er|interrupted|Intermed Infes")
_ITER_MAGPIE = re.compile("^y[12]")
_RUNTYPE_MAGPIE = re.compile("^nlp_")

type _Logical = bool | None  # R's three-valued logical


def _cell(row: Mapping[str, object], name: str) -> str | None:
    """``status[[name]]`` of the unlisted row: a missing name is R's subscript error, ``NA`` is ``None``."""
    if name not in row:
        raise RParityError(_ERR_SUBSCRIPT)
    value = row[name]
    if value is None:
        return None
    if isinstance(value, str):
        return value
    if isinstance(value, float) and math.isnan(value):
        return None
    return r_num_str(value)


def _eq(value: str | None, other: str) -> _Logical:
    """``value == other``: ``NA`` when the value is missing."""
    return None if value is None else value == other


def _not(value: _Logical) -> _Logical:
    return None if value is None else not value


def _grepl(pattern: re.Pattern[str], value: str | None) -> bool:
    """``grepl(pattern, value)``: ``FALSE`` for ``NA``."""
    return value is not None and pattern.search(value) is not None


def _in(value: str | None, options: Collection[str]) -> bool:
    """``value %in% options``: never ``NA``."""
    return value is not None and value in options


def _and(left: _Logical, right: Callable[[], _Logical]) -> _Logical:
    """R's ``&&`` with its short circuit and ``NA`` rules."""
    if left is False:
        return False
    value = right()
    if left is True:
        return value
    return False if value is False else None


def _or(left: _Logical, right: Callable[[], _Logical]) -> _Logical:
    """R's ``||`` with its short circuit and ``NA`` rules."""
    if left is True:
        return True
    value = right()
    if left is False:
        return value
    return True if value is True else None


def _condition(value: _Logical) -> bool:
    """``if (value)``: an ``NA`` condition is an R error."""
    if value is None:
        raise RParityError(_ERR_NA_CONDITION)
    return value


def is_magpie_row(row: Mapping[str, object]) -> bool:
    """The branch test of ``loopRuns``: ``grepl("^y[12]", Iter) || grepl("^nlp_", RunType)``."""
    return _grepl(_ITER_MAGPIE, _cell(row, "Iter")) or _grepl(_RUNTYPE_MAGPIE, _cell(row, "RunType"))


def colour_for_magpie(row: Mapping[str, object], out: str) -> str | None:
    """The MAgPIE decision tree of ``loopRuns``: the style name for the row, or ``None`` for plain output."""
    if _in(_cell(row, "Runtime"), ("pending",)):
        return "yellow"
    if _grepl(_MAGPIE_FAIL, _cell(row, "RunStatus")):
        return "orangered"
    if _grepl(_MAGPIE_OK, _cell(row, "RunStatus")):
        return "green"
    modelstat = _cell(row, "modelstat")
    all_optimal = _grepl(re.compile("222"), modelstat) and "." not in (modelstat or "")
    if _condition(_or(all_optimal, lambda: _eq(_cell(row, "modelstat"), "2: Locally Optimal"))):
        return "green"
    if "conoptspy >" in out:
        return "magenta"
    if "Run in progress" in out:
        return "cyan"
    if " NA " in out and "FALSE" in out:
        return "orangered"
    return None


def colour_for_remind(row: Mapping[str, object], out: str) -> str | None:
    """The REMIND decision tree of ``loopRuns``: the style name for the row (its last branch is ``cyan``)."""
    if _in(_cell(row, "Runtime"), ("pending", "startup")):
        return "yellow"
    if "conoptspy >" in out:
        return "magenta"
    if _condition(_and(_not(_eq(_cell(row, "jobInSLURM"), "no")), lambda: _not(_eq(_cell(row, "jobInSLURM"), "NA")))):
        return "cyan"
    if _condition(_and(_eq(_cell(row, "Conv"), "converged (had INFES)"), lambda: _not(_eq(_cell(row, "Mif"), "no")))):
        return "green"
    if _grepl(_REMIND_FAIL, _cell(row, "RunStatus")):
        return "orangered"
    if _condition(
        _and(_in(_cell(row, "Conv"), ("converged", "Clb_converged")), lambda: _not(_eq(_cell(row, "Mif"), "no")))
    ):
        return "green"
    if _grepl(re.compile("2: Locally Optimal"), _cell(row, "modelstat")) and not _grepl(
        re.compile("nash"), _cell(row, "RunType")
    ):
        return "green"
    if _condition(
        _and(
            _eq(_cell(row, "Mif"), "no"),
            lambda: _in(_cell(row, "Conv"), ("converged", "Clb_converged", "converged (had INFES)")),
        )
    ):
        return "orange"
    if _condition(_eq(_cell(row, "jobInSLURM"), "no")):
        return "orangered"
    return "cyan"


def colour_for_row(row: Mapping[str, object], out: str) -> str | None:
    """The style name ``loopRuns`` would print the row in: the MAgPIE tree for MAgPIE rows, else the REMIND one."""
    if is_magpie_row(row):
        return colour_for_magpie(row, out)
    return colour_for_remind(row, out)
