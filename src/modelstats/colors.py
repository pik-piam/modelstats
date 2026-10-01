"""crayon's escape sequences and the row colour rules of ``R/loopRuns.R``.

Parity port (``migration/03-migration-plan.md`` section 3.3; ``migration/02-python-libraries.md``
section 4.5). crayon closes a colour with ``ESC[39m`` and underline with ``ESC[24m``, nests
literally, uses 256-index codes for the two named colours (``orangered``, ``orange``) whenever the
terminal reports at least 256 colours and the basic codes otherwise, and emits nothing at all when
it is disabled. The goldens were recorded with ``options(crayon.enabled = TRUE, crayon.colors = 256)``
("crayon on") or ``crayon.enabled = FALSE`` ("crayon off"); the port never probes a terminal here,
the CLI decides (``Crayon.from_terminal`` is the pure part of that decision).

The row colour is the decision tree of ``R/loopRuns.R:94-135``: the MAgPIE branch when ``Iter``
starts with ``y1``/``y2`` or ``RunType`` with ``nlp_``, the REMIND branch otherwise, both read from
the formatted line and the status fields exactly as coded. R evaluates the fields of the
``unlist()``-ed status row with its three-valued logic: a missing value (``None`` here) makes
``grepl`` and ``%in%`` false but turns ``==`` into ``NA``, and an ``if (NA)`` is the R error
``missing value where TRUE/FALSE needed``; a field the row lacks is ``subscript out of bounds``.
Both are raised as ``ColourRuleError`` so that ``loop_runs`` fails where ``loopRuns`` fails.
"""

from __future__ import annotations

import re
from collections.abc import Callable, Mapping
from dataclasses import dataclass
from enum import StrEnum

from modelstats.formatting import CellValue, is_na, r_as_character

__all__ = [
    "BASIC_COLOURS",
    "EXTENDED_COLOURS",
    "MISSING_VALUE_IN_IF",
    "PLAIN",
    "SUBSCRIPT_OUT_OF_BOUNDS",
    "ColourRuleError",
    "Crayon",
    "Style",
    "StyleName",
    "colour_for_magpie",
    "colour_for_remind",
    "colour_for_row",
    "colour_legend",
    "is_magpie_row",
]

#: R's message for ``if (NA)``.
MISSING_VALUE_IN_IF = "missing value where TRUE/FALSE needed"
#: R's message for ``status[["name"]]`` on a vector without that name.
SUBSCRIPT_OUT_OF_BOUNDS = "subscript out of bounds"

_ESC = "\x1b["
_CLOSE_COLOUR = f"{_ESC}39m"
_CLOSE_UNDERLINE = f"{_ESC}24m"
#: crayon reports 256 colours (or truecolor, same codes) from this depth on.
_EXTENDED_FROM = 256


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
        """Wrap ``text`` the way crayon does: open, text, close; nested styles stay literal."""
        return f"{self.open}{text}{self.close}"


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
    def from_terminal(cls, *, is_tty: bool, environ: Mapping[str, str]) -> Crayon:
        """The enable logic of crayon, as ``02`` section 4.5 records it, from values the CLI passes.

        Colours only when stdout is a terminal and ``NO_COLOR`` is unset, or when ``FORCE_COLOR``
        is set; 256 colours when ``TERM`` contains ``256color`` or ``COLORTERM`` is set, 16
        otherwise.
        """
        forced = bool(environ.get("FORCE_COLOR"))
        enabled = forced or (is_tty and "NO_COLOR" not in environ)
        extended = "256color" in environ.get("TERM", "") or bool(environ.get("COLORTERM"))
        return cls(enabled=enabled, colours=_EXTENDED_FROM if extended else 16)

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


class ColourRuleError(RuntimeError):
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
