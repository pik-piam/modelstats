"""crayon's escape sequences and the row colour rules of ``R/loopRuns.R``.

Parity port (``migration/03-migration-plan.md`` section 3.3; ``migration/02-python-libraries.md``
section 4.5). crayon closes a colour with ``ESC[39m`` and underline with ``ESC[24m``, replaces a
nested style's own closing sequence by the enclosing style's opening one (``crayon:::crayon_template``:
an inner colour hands back to the outer colour, underline inside a colour stays literal), uses
256-index codes for the two named colours (``orangered``, ``orange``) whenever the terminal reports
at least 256 colours and the basic codes otherwise, and emits nothing at all when it is disabled. The
goldens were recorded with ``options(crayon.enabled = TRUE, crayon.colors = 256)`` ("crayon on") or
``crayon.enabled = FALSE, cli.num_colors = 1L`` ("crayon off", no escape at all); the port never probes a
terminal here, the CLI decides
(``Crayon.from_terminal`` reproduces crayon's ``num_ansi_colors`` from the values the CLI passes,
:func:`num_ansi_colours`; only :func:`terminfo_colours` touches the terminfo database).

The row colour is the decision tree of ``R/loopRuns.R:94-135``: the MAgPIE branch when ``Iter``
starts with ``y1``/``y2`` or ``RunType`` with ``nlp_``, the REMIND branch otherwise, both read from
the formatted line and the status fields exactly as coded. R evaluates the fields of the
``unlist()``-ed status row with its three-valued logic: a missing value (``None`` here) makes
``grepl`` and ``%in%`` false but turns ``==`` into ``NA``, and an ``if (NA)`` is the R error
``missing value where TRUE/FALSE needed``; a field the row lacks is ``subscript out of bounds``.
Both are raised as ``ColourRuleError`` so that ``loop_runs`` fails where ``loopRuns`` fails.
"""

from __future__ import annotations

import curses
import re
from collections.abc import Callable, Mapping
from dataclasses import dataclass
from enum import StrEnum
from typing import Final

from modelstats.errors import MISSING_VALUE_IN_IF, SUBSCRIPT_OUT_OF_BOUNDS, ModelstatsError
from modelstats.formatting import CellValue, is_na, r_as_character

__all__ = [
    "BASIC_COLOURS",
    "EXTENDED_COLOURS",
    "MISSING_VALUE_IN_IF",
    "PLAIN",
    "SUBSCRIPT_OUT_OF_BOUNDS",
    "TRUECOLOR",
    "ColourRuleError",
    "Crayon",
    "Style",
    "StyleName",
    "TerminfoColours",
    "colour_for_magpie",
    "colour_for_remind",
    "colour_for_row",
    "colour_legend",
    "is_magpie_row",
    "num_ansi_colours",
    "terminfo_colours",
]

_ESC = "\x1b["
_CLOSE_COLOUR = f"{_ESC}39m"
_CLOSE_UNDERLINE = f"{_ESC}24m"
#: crayon reports 256 colours (or truecolor, same codes) from this depth on.
_EXTENDED_FROM = 256
#: crayon's ``truecolor`` (cli: ``0x1000000``), what ``COLORTERM=truecolor`` reports.
TRUECOLOR: Final = 256**3
#: The descriptor ``setupterm`` probes (window size only); the capability lookup needs no terminal on it.
_STDOUT_FD: Final = 1
#: ``crayon:::guess_tty_colors()``: names that look like a colour terminal when ``tput`` fails.
_GUESS_COLOUR_TERM: Final = re.compile(r"^screen|^xterm|^vt100|color|ansi|cygwin|linux", re.IGNORECASE)
#: ``crayon:::emacs_version()`` keeps the digits and dots of ``INSIDE_EMACS``.
_EMACS_VERSION_NOISE: Final = re.compile(r"[^0-9.]+")


class StyleName(StrEnum):
    """The seven crayon styles ``R/loopRuns.R`` uses (``red`` is ``make_style("orangered")``)."""

    ORANGERED = "orangered"
    ORANGE = "orange"
    YELLOW = "yellow"
    CYAN = "cyan"
    GREEN = "green"
    MAGENTA = "magenta"
    UNDERLINE = "underline"


@dataclass(frozen=True)
class Style:
    """One crayon style: the bytes that open it and the bytes that close it."""

    open: str
    close: str

    def __call__(self, text: str) -> str:
        """Wrap ``text`` as ``crayon:::crayon_template`` does (crayon 1.5.3, ``R/machinery.R``).

        ``st$open %+% gsub_(st$close, st$open, text, fixed = TRUE) %+% st$close``: every occurrence of
        this style's own closing sequence inside ``text`` becomes its opening sequence, so a nested
        colour restores the enclosing colour (``yellow(paste0("a", green("b"), "c"))`` is
        ``ESC[33ma ESC[32mb ESC[33mc ESC[39m``); underline inside a colour stays literal because its
        closing sequence differs. ``PLAIN`` (empty sequences) returns ``text`` unchanged.
        """
        inner = text.replace(self.close, self.open) if self.close else text
        return f"{self.open}{inner}{self.close}"


#: crayon disabled (``options(crayon.enabled = FALSE)``, or no terminal): the text itself.
PLAIN = Style("", "")

#: ``crayon.colors`` of 256 or more (``02`` section 4.5, ``migration/harness/checks/crayon_codes.R``).
EXTENDED_COLOURS: Mapping[StyleName, Style] = {
    StyleName.ORANGERED: Style(f"{_ESC}38;5;202m", _CLOSE_COLOUR),
    StyleName.ORANGE: Style(f"{_ESC}38;5;214m", _CLOSE_COLOUR),
    StyleName.YELLOW: Style(f"{_ESC}33m", _CLOSE_COLOUR),
    StyleName.CYAN: Style(f"{_ESC}36m", _CLOSE_COLOUR),
    StyleName.GREEN: Style(f"{_ESC}32m", _CLOSE_COLOUR),
    StyleName.MAGENTA: Style(f"{_ESC}35m", _CLOSE_COLOUR),
    StyleName.UNDERLINE: Style(f"{_ESC}4m", _CLOSE_UNDERLINE),
}
#: Below 256 colours the two named colours fall back to the basic red and yellow.
BASIC_COLOURS: Mapping[StyleName, Style] = {
    **EXTENDED_COLOURS,
    StyleName.ORANGERED: Style(f"{_ESC}31m", _CLOSE_COLOUR),
    StyleName.ORANGE: Style(f"{_ESC}33m", _CLOSE_COLOUR),
}


# --- crayon's colour detection -----------------------------------------------------------------------

type TerminfoColours = Callable[[str], int | None]
"""``tput colors`` of a ``TERM``: the ``max_colors`` capability, ``None`` where ``tput`` prints nothing."""


def terminfo_colours(term: str) -> int | None:
    """``tput colors`` through the terminfo database (``curses``), ``None`` where ``tput`` prints nothing.

    ``tput`` prints nothing for an empty or unknown ``TERM`` (exit 2 or 3) and ``-1`` for a terminal
    without the capability (``dumb``, ``vt100``); ``curses.tigetnum("colors")`` after ``setupterm`` for
    that ``TERM`` gives the same numbers (checked for dumb, xterm, xterm-256color, screen, vt100, linux
    and an unknown name). CPython's ``setupterm`` keeps the first terminal it loaded for the rest of
    the process, so this answers for one ``TERM`` per process, which is all the CLI asks.
    """
    if not term:
        return None
    try:
        curses.setupterm(term, _STDOUT_FD)
        return int(curses.tigetnum("colors"))
    except curses.error:
        return None


def _r_as_integer(text: str) -> int:
    """``as.integer()`` of the ``R_CLI_NUM_COLORS`` value.

    R parses a decimal or exponent form and truncates (``"256.7"`` is 256); a value it cannot parse
    is ``NA``, and crayon's ``if (nc > 1)`` then stops with ``missing value where TRUE/FALSE needed``,
    raised here as ``ValueError`` carrying that message.
    """
    try:
        return int(float(text))
    except (ValueError, OverflowError) as error:
        raise ValueError(f"R_CLI_NUM_COLORS={text!r}: {MISSING_VALUE_IN_IF}") from error


def _emacs_major_version(environ: Mapping[str, str]) -> float | None:
    """``crayon:::emacs_version()[1]``: ``INSIDE_EMACS`` reduced to digits and dots, up to the first dot."""
    digits = _EMACS_VERSION_NOISE.sub("", environ.get("INSIDE_EMACS", ""))
    if not digits:
        return None
    first = digits.split(".")[0]
    return float(first) if first else None


def _is_emacs_with_colour(environ: Mapping[str, str]) -> bool:
    """``crayon:::is_emacs_with_color()``: ``EMACS`` or ``INSIDE_EMACS`` set, Emacs version 23 or newer."""
    if not (environ.get("EMACS", "") or environ.get("INSIDE_EMACS", "")):
        return False
    major = _emacs_major_version(environ)
    return major is not None and major >= 23


def _guess_tty_colours(term: str) -> int:
    """``crayon:::guess_tty_colors()``: ``dumb`` is 1, a colour-terminal-looking name 8, anything else 1."""
    if term == "dumb":
        return 1
    return 8 if _GUESS_COLOUR_TERM.search(term) else 1


def _detect_tty_colours(environ: Mapping[str, str], terminfo: TerminfoColours) -> int:
    """``crayon:::detect_tty_colors()`` on Unix with a terminal: ``COLORTERM``, Emacs, ``tput colors``."""
    if "COLORTERM" in environ:
        return TRUECOLOR if environ["COLORTERM"] in ("truecolor", "24bit") else 8
    if _is_emacs_with_colour(environ):
        return 8
    term = environ.get("TERM", "")
    colours = terminfo(term)
    if colours is None:
        return _guess_tty_colours(term)
    if colours in (-1, 0, 1):
        return 1
    if colours == 8 and term == "xterm":
        return 256
    return colours


def num_ansi_colours(
    *, is_tty: bool, environ: Mapping[str, str], terminfo: TerminfoColours = terminfo_colours
) -> int:
    """``crayon:::num_ansi_colors()`` (crayon 1.5.3, vendored from cli 3.6.6) as a Linux CLI reaches it.

    In order: a non-empty ``R_CLI_NUM_COLORS`` wins outright (even piped, even next to ``NO_COLOR``);
    ``NO_COLOR`` set to anything, ``""`` included, is 1; stdout not a terminal is 1; then the terminal
    detection: ``COLORTERM`` set is 16777216 for ``truecolor``/``24bit`` and 8 for any other value;
    Emacs 23 or newer (``EMACS``/``INSIDE_EMACS``) is 8; otherwise ``tput colors`` for ``TERM`` decides,
    with crayon's guess from the name when it fails (``dumb`` 1; ``^screen|^xterm|^vt100|color|ansi|
    cygwin|linux``, case-insensitively, 8; else 1), ``-1``/``0``/``1`` meaning 1 and 8 on a ``TERM`` of
    exactly ``xterm`` raised to 256. There is no ``FORCE_COLOR`` rule and no ``256color``-in-``TERM``
    rule. Every row of ``tests/unit/test_colors.py`` was checked against ``crayon::num_colors()`` in R.

    Not reproduced, being reachable from R only: the options ``cli.num_colors``, ``crayon.enabled``,
    ``crayon.colors`` and ``cli.default_num_colors`` (the goldens set the crayon ones), knitr, sinks,
    RStudio, R.app/RGui and the Windows branches.
    """
    value = environ.get("R_CLI_NUM_COLORS", "")
    if value:
        return _r_as_integer(value)
    if "NO_COLOR" in environ:
        return 1
    if not is_tty:
        return 1
    return _detect_tty_colours(environ, terminfo)


@dataclass(frozen=True)
class Crayon:
    """The crayon state a ``loopRuns`` call runs under: enabled or not, and the colour depth.

    The styles are the ``make_style`` / ``yellow`` ... closures of ``R/loopRuns.R:26-27`` and the
    ``crayon`` imports; ``paint`` applies one by name, ``style`` returns it.
    """

    enabled: bool = True
    colours: int = _EXTENDED_FROM

    @classmethod
    def disabled(cls) -> Crayon:
        """``options(crayon.enabled = FALSE)``: every style is ``PLAIN``."""
        return cls(enabled=False)

    @classmethod
    def from_terminal(
        cls, *, is_tty: bool, environ: Mapping[str, str], terminfo: TerminfoColours = terminfo_colours
    ) -> Crayon:
        """What crayon decides for this process: :func:`num_ansi_colours` from the values the CLI passes.

        Enabled when more than one colour is detected (``crayon::has_color()`` is
        ``num_ansi_colors() > 1``); ``terminfo`` is the ``tput colors`` lookup, injectable for tests.
        """
        colours = num_ansi_colours(is_tty=is_tty, environ=environ, terminfo=terminfo)
        return cls(enabled=colours > 1, colours=colours)

    def style(self, name: StyleName) -> Style:
        """The escape sequences of ``name`` at this depth, or ``PLAIN`` when disabled."""
        if not self.enabled:
            return PLAIN
        table = EXTENDED_COLOURS if self.colours >= _EXTENDED_FROM else BASIC_COLOURS
        return table[name]

    def paint(self, name: StyleName | None, text: str) -> str:
        """``text`` in style ``name``; ``None`` (no colour decided) leaves it as it is."""
        return text if name is None else self.style(name)(text)

    def red(self, text: str) -> str:
        """``make_style("orangered")`` of ``R/loopRuns.R:26``."""
        return self.paint(StyleName.ORANGERED, text)

    def orange(self, text: str) -> str:
        """``make_style("orange")`` of ``R/loopRuns.R:27``."""
        return self.paint(StyleName.ORANGE, text)

    def yellow(self, text: str) -> str:
        """``crayon::yellow``."""
        return self.paint(StyleName.YELLOW, text)

    def cyan(self, text: str) -> str:
        """``crayon::cyan``."""
        return self.paint(StyleName.CYAN, text)

    def green(self, text: str) -> str:
        """``crayon::green``."""
        return self.paint(StyleName.GREEN, text)

    def magenta(self, text: str) -> str:
        """``crayon::magenta``."""
        return self.paint(StyleName.MAGENTA, text)

    def underline(self, text: str) -> str:
        """``crayon::underline``."""
        return self.paint(StyleName.UNDERLINE, text)


def colour_legend(crayon: Crayon) -> str:
    """The ``# Color code:`` line of ``R/loopRuns.R:29-31`` (printed only with ``colors = TRUE``)."""
    return (
        f"# Color code: {crayon.yellow('pending')}/{crayon.yellow('startup')}, {crayon.cyan('running')}, "
        f"{crayon.green('converged')}/{crayon.green('finished')}, {crayon.orange('no mif')}, "
        f"{crayon.magenta('conopt stalled?')}, {crayon.red('error')}.\n\n"
    )


# --- the decision trees -------------------------------------------------------------------------------


class ColourRuleError(ModelstatsError):
    """An R error inside the colour decision; ``str(error)`` is the R condition message."""


#: The ``unlist(status)`` vector of ``R/loopRuns.R:83``: every field is text (numbers pasted by
#: ``as.character``), a real ``NA`` is ``None``.
type StatusFields = Mapping[str, CellValue]
#: R's logical with ``NA``.
type RBool = bool | None

_MAGPIE_ITER = re.compile(r"^y[12]")
_MAGPIE_RUN_TYPE = re.compile(r"^nlp_")
_MAGPIE_FAILED = re.compile("not_converged|Execution erro|Compilation er|missing|interrupted|Abort")
_MAGPIE_CONVERGED = re.compile("converged|Clb_converged")
_MAGPIE_ALL_222 = re.compile("222")
_LOCALLY_OPTIMAL = "2: Locally Optimal"
_RUN_IN_PROGRESS = re.compile("Run in progress")
_NA_CELL = re.compile(" NA ")
_FALSE_CELL = re.compile("FALSE")
_REMIND_FAILED = re.compile("not_converged|Execution erro|Compilation er|interrupted|Intermed Infes")
_REMIND_LOCALLY_OPTIMAL = re.compile("2: Locally Optimal")
_NASH = re.compile("nash")
_CONOPT_STALLED = "conoptspy >"
_CONVERGED_HAD_INFES = "converged (had INFES)"
_CONVERGED = ("converged", "Clb_converged")
_CONVERGED_ANY = ("converged", "Clb_converged", _CONVERGED_HAD_INFES)


def _field(status: StatusFields, name: str) -> str | None:
    """``status[[name]]`` of the unlisted row: text, ``None`` for ``NA``, an R error when absent."""
    if name not in status:
        raise ColourRuleError(SUBSCRIPT_OUT_OF_BOUNDS)
    value = status[name]
    return None if is_na(value) else r_as_character(value)


def _grepl(pattern: re.Pattern[str], value: str | None) -> bool:
    """``grepl(pattern, x)``: ``FALSE`` for ``NA``."""
    return value is not None and pattern.search(value) is not None


def _is_in(value: str | None, choices: tuple[str, ...]) -> bool:
    """``x %in% choices``: ``FALSE`` for ``NA``."""
    return value is not None and value in choices


def _eq(value: str | None, other: str) -> RBool:
    """``x == other``: ``NA`` for ``NA``."""
    return None if value is None else value == other


def _not(value: RBool) -> RBool:
    """``!x`` with ``NA``."""
    return None if value is None else not value


def _and(left: RBool, right: Callable[[], RBool]) -> RBool:
    """``left && right()``: the right side is not evaluated when the left is ``FALSE``."""
    if left is False:
        return False
    other = right()
    if other is False:
        return False
    return None if (left is None or other is None) else True


def _or(left: RBool, right: Callable[[], RBool]) -> RBool:
    """``left || right()``: the right side is not evaluated when the left is ``TRUE``."""
    if left is True:
        return True
    other = right()
    if other is True:
        return True
    return None if (left is None or other is None) else False


def _if(condition: RBool) -> bool:
    """``if (condition)``: an ``NA`` condition is an R error."""
    if condition is None:
        raise ColourRuleError(MISSING_VALUE_IN_IF)
    return condition


def is_magpie_row(status: StatusFields) -> bool:
    """``R/loopRuns.R:94``: ``grepl("^y[12]", Iter) || grepl("^nlp_", RunType)``."""
    return _grepl(_MAGPIE_ITER, _field(status, "Iter")) or _grepl(_MAGPIE_RUN_TYPE, _field(status, "RunType"))


def colour_for_magpie(status: StatusFields, line: str) -> StyleName | None:
    """The MAgPIE branch of ``R/loopRuns.R:95-114`` for a formatted ``line``; ``None`` is plain."""
    if _is_in(_field(status, "Runtime"), ("pending",)):
        return StyleName.YELLOW
    if _grepl(_MAGPIE_FAILED, _field(status, "RunStatus")):
        return StyleName.ORANGERED
    if _grepl(_MAGPIE_CONVERGED, _field(status, "RunStatus")):
        return StyleName.GREEN
    modelstat = _field(status, "modelstat")
    all_optimal = _grepl(_MAGPIE_ALL_222, modelstat) and "." not in (modelstat or "")
    if _if(_or(all_optimal, lambda: _eq(modelstat, _LOCALLY_OPTIMAL))):
        return StyleName.GREEN
    if _CONOPT_STALLED in line:
        return StyleName.MAGENTA
    if _grepl(_RUN_IN_PROGRESS, line):
        return StyleName.CYAN
    if _grepl(_NA_CELL, line) and _grepl(_FALSE_CELL, line):
        return StyleName.ORANGERED
    return None


def colour_for_remind(status: StatusFields, line: str) -> StyleName | None:
    """The REMIND branch of ``R/loopRuns.R:115-135`` for a formatted ``line``; ``None`` is plain."""
    if _is_in(_field(status, "Runtime"), ("pending", "startup")):
        return StyleName.YELLOW
    if _CONOPT_STALLED in line:
        return StyleName.MAGENTA
    job = _field(status, "jobInSLURM")
    if _if(_and(_not(_eq(job, "no")), lambda: _not(_eq(job, "NA")))):
        return StyleName.CYAN
    conv = _field(status, "Conv")
    if _if(_and(_eq(conv, _CONVERGED_HAD_INFES), lambda: _not(_eq(_field(status, "Mif"), "no")))):
        return StyleName.GREEN
    if _grepl(_REMIND_FAILED, _field(status, "RunStatus")):
        return StyleName.ORANGERED
    if _if(_and(_is_in(conv, _CONVERGED), lambda: _not(_eq(_field(status, "Mif"), "no")))):
        return StyleName.GREEN
    if _grepl(_REMIND_LOCALLY_OPTIMAL, _field(status, "modelstat")) and not _grepl(
        _NASH, _field(status, "RunType")
    ):
        return StyleName.GREEN
    if _if(_and(_eq(_field(status, "Mif"), "no"), lambda: _is_in(conv, _CONVERGED_ANY))):
        return StyleName.ORANGE
    if _if(_eq(job, "no")):
        return StyleName.ORANGERED
    return StyleName.CYAN


def colour_for_row(status: StatusFields, line: str, colors: bool = True) -> StyleName | None:
    """The colour of one table row: ``R/loopRuns.R:94-135``.

    The model branch is decided first (``is_magpie_row``, line 94, also when colours are off);
    with ``colors = FALSE`` both branches print the line as it is (lines 95-96, 116-117).
    """
    magpie = is_magpie_row(status)
    if not colors:
        return None
    return colour_for_magpie(status, line) if magpie else colour_for_remind(status, line)
