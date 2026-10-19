"""Interactive selection from a numbered list.

R source: ``gms::chooseFromList(theList, type = "folders")`` as used by ``rs -p`` and ``rs2``.
A simplified grammar (PORTING.md §3.8): the folders are numbered from 1 and come back in list
order; a number, a range ``1-3`` or ``1:3``, a comma list ``2,5``, ``all`` (or R's shortcut ``a``,
alone or in a list such as ``2,a``), a lone ``p``/``f`` for a second prompt asking for a regular
expression / a fixed string (an empty pattern selects nothing), or any other text as a regular
expression over the items selects, empty selects nothing. An answer of digits, ``,``, ``:``, ``-``
and blanks only is read as numbers, never as a pattern (``2026-10-18`` asks again;
``2026-10-18_`` is a pattern).
``gms::chooseFromList`` numbered the folders from 2 (1 was ``all``, the last two entries
``p``/``f``), listed the folders as given (``./name``), accepted ``2,4:6``, had its second prompt
list the matches and ask ``(y/n)`` until confirmed, returned the typed order (BUG-076), and cut
its ``Selected:`` line after 666 characters (BUG-104). Each prompt reads one line of the
process's stdin (``gms::getLine()`` lost every line of a pipe after the first, BUG-106). A
typer/rich prompt is a later task (PORTING-TASKS.md).
"""

from __future__ import annotations

import re
from collections.abc import Callable, Sequence

from rich.console import Console
from rich.text import Text

from modelstats import _console
from modelstats._textscan import compile_pattern
from modelstats.errors import ModelstatsError

__all__ = ["choose_from_list"]

_NUMBERS = re.compile(r"[0-9,:\-\s]+")
_PART = re.compile(r"\s*(?P<low>[0-9]+)\s*(?:[-:]\s*(?P<high>[0-9]+)\s*)?")


def _parse_numbers(answer: str, count: int) -> list[int] | None:
    """Indices (1-based) from ``2,4-6,9``; ``None`` when a number is out of range, malformed, or none.

    A part is a number or a range ``a-b``/``a:b`` with optional spaces around the numbers; ``1 2``
    is malformed (not ``12``), and so is a lone ``,``.
    """
    chosen: list[int] = []
    for part in answer.split(","):
        if not part.strip():
            continue
        match = _PART.fullmatch(part)
        if match is None:
            return None
        low = match["low"].lstrip("0") or "0"
        high = (match["high"] or match["low"]).lstrip("0") or "0"
        if max(len(low), len(high)) > len(str(count)):
            return None  # out of range anyway (and int() refuses more than 4300 digits)
        start, stop = int(low), int(high)
        if start < 1 or stop > count or start > stop:
            return None
        chosen.extend(range(start, stop + 1))
    return chosen or None


def _selects_all(answer: str, count: int) -> bool | None:
    """R's ``a`` entry (``1,a: all``), alone or inside a comma list of numbers such as ``2,a``.

    ``None`` when a number of the list is out of range or malformed (R asked again).
    """
    parts = [part.strip() for part in answer.split(",")]
    if "a" not in parts or not all(part in ("a", "") or _NUMBERS.fullmatch(part) for part in parts):
        return False
    numbers = [part for part in parts if part not in ("a", "")]
    if numbers and _parse_numbers(",".join(numbers), count) is None:
        return None
    return True


def _ask_pattern(kind: str, read: Callable[[str], str], console: Console) -> re.Pattern[str] | None:
    """R's second prompt for ``p`` (a regular expression) or ``f`` (a fixed string).

    Asked again until the expression compiles (as R did); ``None`` at the end of the input or for an
    empty pattern, which selects nothing (R matched every item with an empty pattern, but only
    after its ``(y/n)`` confirmation, which the port has not). Blanks are part of the pattern.
    """
    what = "regular expression" if kind == "p" else "search pattern with fixed=TRUE"
    while True:
        console.print(Text(f"\nInsert the {what}: "), end="")
        try:
            pattern = read("")
        except EOFError:
            return None
        if pattern == "":
            return None
        if kind == "f":
            return re.compile(re.escape(pattern))
        try:
            return compile_pattern(pattern, source="pattern")
        except ModelstatsError as error:
            console.print(Text(f"Try again, not a valid regular expression: {error}"))


def choose_from_list(
    items: Sequence[str],
    *,
    kind: str = "folders",
    read: Callable[[str], str] = input,
    out: Console | None = None,
) -> list[str]:
    """Print ``1: item`` lines and read answers until one is valid.

    Returns the chosen items in list order.
    """
    console = out if out is not None else _console.err
    if not items:
        console.print(Text(f"No {kind} found that might be selected, returning the empty list."))
        return []
    width = len(str(len(items)))
    console.print()
    console.print(Text(f"Please choose {kind}:"))
    console.print()
    for number, item in enumerate(items, 1):
        console.print(Text(f"{number:>{width}}: {item}"))
    prompt = Text(
        f"\nChoose {kind} (numbers, ranges 1-3, comma lists, 'all'/'a', 'p'/'f' for a pattern prompt, "
        "a regex; empty = none): "
    )
    while True:
        console.print(prompt, end="")
        try:
            answer = read("").strip()
        except EOFError:
            answer = ""
        if answer == "":
            chosen: list[int] = []
        elif answer == "all":
            chosen = list(range(1, len(items) + 1))
        elif (selects_all := _selects_all(answer, len(items))) is not False:
            if selects_all is None:
                console.print(Text("Try again, not all in list."))
                continue
            chosen = list(range(1, len(items) + 1))
        elif answer in ("p", "f"):  # gms::chooseFromList's pattern entries: a second prompt
            regex = _ask_pattern(answer, read, console)
            chosen = [] if regex is None else [n for n, item in enumerate(items, 1) if regex.search(item)]
        elif _NUMBERS.fullmatch(answer):
            parsed = _parse_numbers(answer, len(items))
            if parsed is None:
                console.print(Text("Try again, not all in list."))
                continue
            chosen = parsed
        else:
            try:
                regex = compile_pattern(answer, source="answer")
            except ModelstatsError as error:
                console.print(Text(f"Try again, not a valid regular expression: {error}"))
                continue
            chosen = [number for number, item in enumerate(items, 1) if regex.search(item)]
        selected = [items[number - 1] for number in sorted(set(chosen))]
        console.print(Text("Selected: " + ", ".join(selected)))
        return selected
