"""The numbered selection menu of ``rs -p``: a port of ``gms::chooseFromList`` (gms 0.35.0).

``R/commandLineInterface.R:250`` calls ``gms::chooseFromList(runfolders, type = "folders")`` with the
defaults ``addAllPattern = TRUE``, ``multiple = TRUE`` and an unnamed list (no groups). The port
reproduces that path: the menu written to stderr (R's ``message()``), the input syntax (numbers, ranges
``4:6`` or ``4-6``, comma lists, ``a`` for all, ``p`` for a regular-expression search, ``f`` for a
fixed-string search, each search confirmed with ``y``), the retry messages, the ``Selected:`` line with
its 666-character cut and the selection order of the result (``2,4:6`` picks in that order, ``6:4``
counts down, duplicates collapse to the first occurrence; verified against gms 0.35.0).

Differences, all recorded (``migration/03-migration-plan.md`` 3.5, decision D-21, BUG-040):

* one line is read per prompt (``read_line``), where R's ``gms::getLine()`` reopens ``stdin`` for every
  read and loses the rest of a piped input; end of input at the main prompt is the empty selection, as
  in R, and at a search prompt it raises :class:`NoMoreInputError` where R recurses until the stack overflows;
* R evaluates the input as R code (``c(2,4:6)``); the port accepts the documented grammar and reproduces
  R's diagnostic for an unknown name (``object 'x' not found``, pinned by the golden
  ``rs/prompt-invalid-then-ok``) but not R's parser messages for other malformed input;
* the search uses Python's ``re`` where R uses TRE extended regular expressions, and the message for an
  invalid pattern carries Python's reason.
"""

from __future__ import annotations

import re
import sys
from collections.abc import Callable, Sequence
from dataclasses import dataclass
from typing import Final, TextIO

__all__ = [
    "ALL_ENTRY",
    "FIXED_ENTRY",
    "PATTERN_ENTRY",
    "SELECTED_LIMIT",
    "LineReader",
    "NoMoreInputError",
    "ParseFailure",
    "choose_from_list",
    "choose_pattern_from_list",
    "menu_text",
    "parse_selection",
    "read_stdin_line",
]

ALL_ENTRY: Final = "all"
PATTERN_ENTRY: Final = "Search pattern by regular expression..."
FIXED_ENTRY: Final = "Search by fixed pattern..."
"""The entries ``chooseFromList`` adds around the items (``a``, ``p`` and ``f`` in the menu)."""

SELECTED_LIMIT: Final = 666
"""``getOption("chooseFromListLimit", 666)``: characters of selected names shown in the ``Selected:`` line."""

type LineReader = Callable[[], str | None]
"""Reads one line without its line ending; ``None`` at end of input."""

_NAME: Final = re.compile(r"^[A-Za-z][A-Za-z0-9._]*$")
_ATOM: Final = re.compile(r"^(\d+|[apf])$")
_RANGE: Final = re.compile(r"^(\d+|[apf]):(\d+|[apf])$")
_LETTERS: Final = ("a", "p", "f")
_LISTED_LIMIT: Final = 240


class NoMoreInputError(EOFError):
    """Standard input ended at a prompt that R would repeat forever (a search or its confirmation)."""


@dataclass(frozen=True)
class ParseFailure:
    """Why an input line was rejected; ``detail`` is the text R appends to ``Try again, ...``."""

    detail: str


def read_stdin_line() -> str | None:
    """One line of ``sys.stdin`` without its line ending, ``None`` at end of input (D-21)."""
    line = sys.stdin.readline()
    if line == "":
        return None
    return line.rstrip("\r\n")


def choose_from_list(
    items: Sequence[str],
    *,
    kind: str = "items",
    read_line: LineReader = read_stdin_line,
    err: TextIO | None = None,
    limit: int = SELECTED_LIMIT,
) -> list[str]:
    """Let the user pick from ``items``; returns the picked items in selection order.

    Args:
        items: the choices (``theList``), unnamed.
        kind: the noun in the prompts (``type``; ``rs`` passes ``"folders"``).
        read_line: the input source, one line per prompt.
        err: where the prompts go (R's ``message()``); ``sys.stderr`` by default.
        limit: the ``chooseFromListLimit`` option.
    """
    out = err if err is not None else sys.stderr
    if not items:
        _say(out, f"No {kind} found that might be selected, returning the empty list.")
        return []
    menu = [ALL_ENTRY, *items, PATTERN_ENTRY, FIXED_ENTRY]
    error_message = ""
    while True:
        _say(out, menu_text(menu, kind, error_message))
        line = read_line()
        parsed = parse_selection("" if line is None else line, len(menu))
        if isinstance(parsed, ParseFailure):
            error_message = f"Try again, you have to choose some numbers. {parsed.detail}\n"
            continue
        if any(number < 1 or number > len(menu) for number in parsed):
            listed = ", ".join(str(number) for number in parsed)[:_LISTED_LIMIT]
            error_message = f"Try again, not all in list: {listed}...\n"
            continue
        break
    chosen = _resolve_selection(parsed, items, kind, read_line, out)
    _say(out, _selected_line(chosen, limit))
    return chosen


def menu_text(menu: Sequence[str], kind: str, error_message: str = "") -> str:
    """The prompt R builds (``m`` in ``chooseFromList``) for the extended list ``menu``.

    Numbers are right-aligned to the width of the count; the first entry carries ``,a``, the last two
    ``,p`` and ``,f``; the error message of the previous attempt sits between the list and the prompt.
    """
    width = len(str(len(menu)))
    suffixes = [",a", *[""] * (len(menu) - 3), ",p", ",f"]
    lines = [
        f"{str(number).rjust(width)}{suffix}: {entry}"
        for number, (suffix, entry) in enumerate(zip(suffixes, menu, strict=True), start=1)
    ]
    return (
        f"\n\nPlease choose {kind}:\n\n"
        + "\n".join(lines)
        + "\n"
        + error_message
        + "\nNumbers entered as 2,4:6,9 or leave empty:"
    )


def parse_selection(line: str, menu_length: int) -> list[int] | ParseFailure:
    """Menu numbers for an input line, or why it is rejected.

    R first rewrites the line (``-`` to ``:``, spaces removed, ``,,`` collapsed once) and then evaluates
    ``c(<line>)`` with ``a``, ``p`` and ``f`` bound to the first, second-last and last menu number.
    Accepted here: a comma-separated list of numbers, the letters ``a``/``p``/``f`` and ranges between
    two of those (a descending range counts down, as ``6:4`` does in R); the empty line is no selection.
    """
    normalized = line.replace("-", ":").replace(" ", "").replace(",,", ",")
    if normalized == "":
        return []
    bound = {"a": 1, "p": menu_length - 1, "f": menu_length}

    def atom(text: str) -> int:
        return bound[text] if text in bound else int(text)

    numbers: list[int] = []
    for token in normalized.split(","):
        if _ATOM.match(token):
            numbers.append(atom(token))
        elif (bounds := _RANGE.match(token)) is not None:
            numbers.extend(_r_colon(atom(bounds.group(1)), atom(bounds.group(2))))
        else:
            return ParseFailure(_parse_detail(normalized))
    return numbers


def choose_pattern_from_list(
    items: Sequence[str],
    kind: str,
    *,
    fixed: bool,
    read_line: LineReader,
    err: TextIO,
) -> list[int]:
    """``gms:::choosePatternFromList(theList, type, fixed = fixed)``: 1-based positions of the matches.

    Asks for a pattern, lists the matches, asks for confirmation (``y``/``Y``) and repeats otherwise.

    Raises:
        NoMoreInputError: when the input ends at the pattern or confirmation prompt (D-21).
    """
    while True:
        _say(err, "\nInsert the " + ("search pattern with fixed=TRUE: " if fixed else "regular expression: "))
        pattern = read_line()
        if pattern is None:
            raise NoMoreInputError("end of input at the search prompt")
        try:
            matches = _grep(pattern, items, fixed)
        except re.error as exc:  # R: try() prints the error, then the message below
            err.write(
                "Error in grep(pattern = pattern, theList, fixed = fixed) : \n"
                f"  invalid regular expression '{pattern}', reason '{exc.msg}'\n"
            )
            _say(err, "\n\nMatching created an error. Try again!")
            continue
        if matches:
            _say(err, f"\n\nThe search pattern matches the following {kind}:")
            _say(
                err,
                "\n".join(f"{number}: {items[index - 1]}" for number, index in enumerate(matches, start=1)),
            )
        else:
            _say(err, "Oops. You didn't select anything.")
        _say(err, f"\nAre you sure these are the right {kind}? (y/n): ")
        answer = read_line()
        if answer is None:
            raise NoMoreInputError("end of input at the confirmation prompt")
        if answer in ("y", "Y"):
            return matches


def _resolve_selection(
    numbers: Sequence[int], items: Sequence[str], kind: str, read_line: LineReader, err: TextIO
) -> list[str]:
    """Menu numbers to items: the ``multiple`` branch of ``chooseFromList`` without groups.

    ``all`` wins over everything else; a search appends its matches (menu numbers are item positions
    plus one); duplicates keep their first position; the ``p`` and ``f`` entries themselves are dropped.
    """
    menu_length = len(items) + 3
    if 1 in numbers:
        return list(items)
    identifier = _unique(numbers)
    if menu_length - 1 in numbers:
        found = choose_pattern_from_list(items, kind, fixed=False, read_line=read_line, err=err)
        identifier = _unique([*identifier, *(index + 1 for index in found)])
    if menu_length in numbers:
        found = choose_pattern_from_list(items, kind, fixed=True, read_line=read_line, err=err)
        identifier = _unique([*identifier, *(index + 1 for index in found)])
    return [items[number - 2] for number in identifier if number < len(items) + 2]


def _grep(pattern: str, items: Sequence[str], fixed: bool) -> list[int]:
    """``grep(pattern, items, fixed = fixed)``: 1-based positions."""
    if fixed:
        return [index for index, item in enumerate(items, start=1) if pattern in item]
    compiled = re.compile(pattern)
    return [index for index, item in enumerate(items, start=1) if compiled.search(item)]


def _selected_line(chosen: Sequence[str], limit: int) -> str:
    """``Selected: a, b, ...``, cut after the name that takes the character count past ``limit``."""
    stop = len(chosen)
    total = 0
    for position, name in enumerate(chosen, start=1):
        total += len(name)
        if total > limit:
            stop = position
            break
    suffix = ", ..." if stop < len(chosen) else ""
    return "Selected: " + ", ".join(chosen[:stop]) + suffix


def _parse_detail(normalized: str) -> str:
    """R's diagnostic for the rejected line, where it can be reproduced: an unknown name.

    ``eval(parse(text = "c( x )"))`` fails with ``object 'x' not found``; the condition's text, as
    ``paste0()`` renders it, ends with a newline (golden ``rs/prompt-invalid-then-ok``). Any other
    rejection (a stray character R's parser trips over) gets no detail.
    """
    if _NAME.match(normalized) and normalized not in _LETTERS:
        return (
            f'Error in eval(parse(text = paste("c(", userinput, ")"))): object \'{normalized}\' not found\n'
        )
    return ""


def _r_colon(start: int, end: int) -> list[int]:
    """``start:end`` in R: inclusive, counting down when ``start > end``."""
    step = 1 if start <= end else -1
    return list(range(start, end + step, step))


def _unique(numbers: Sequence[int]) -> list[int]:
    """``unique()``: first occurrences in order."""
    return list(dict.fromkeys(numbers))


def _say(err: TextIO, text: str) -> None:
    """R's ``message(text)``: the text plus a newline on stderr."""
    err.write(text + "\n")
