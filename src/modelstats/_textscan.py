"""Text scanning of log files (replaces grep, tac, awk, tail and find shell-outs).

R source: ``system("grep ...", intern = TRUE)`` and friends in getRunStatus.R, colRunType.R and
modeltests.R. Pure Python with bounded memory: ``last_match`` reads a file backwards in 1 MB
chunks (``full.gms`` is hundreds of MB and only its last ``cm_iteration_max`` line matters).
Lines come back without their newline; files are decoded as UTF-8 with replacement characters.
A file that cannot be read behaves like grep on a missing file: no match.
"""

from __future__ import annotations

import csv
import os
import re
import warnings
from collections.abc import Iterator
from pathlib import Path

from modelstats.errors import ModelstatsError

__all__ = [
    "all_matches",
    "compile_pattern",
    "count_files",
    "csv_line_count",
    "csv_rows",
    "csv_unique_values",
    "find_all_text",
    "first_match",
    "last_line",
    "last_match",
    "last_nonempty_line",
    "r_strsplit",
]

_CHUNK = 1 << 20
_ENCODING = "utf-8"


def _lines(path: Path) -> Iterator[str]:
    """The lines of a file, forwards; nothing when the file cannot be read."""
    try:
        with path.open(encoding=_ENCODING, errors="replace", newline="\n") as handle:
            for line in handle:
                yield line.rstrip("\n")
    except OSError:
        return


def _lines_backwards(path: Path) -> Iterator[str]:
    """The lines of a file, last line first, read in chunks from the end."""
    try:
        with path.open("rb") as handle:
            position = handle.seek(0, os.SEEK_END)
            if position == 0:
                return
            carry: list[bytes] = []  # the start of a line that continues in the previous chunk
            first = True
            while position > 0:
                size = min(_CHUNK, position)
                position -= size
                handle.seek(position)
                chunk = handle.read(size)
                if first:
                    first = False
                    chunk = chunk.removesuffix(b"\n")  # a final newline ends the last line, it is no line
                if b"\n" not in chunk:
                    carry.append(chunk)
                    continue
                pieces = chunk.split(b"\n")
                pieces[-1] = b"".join([pieces[-1], *reversed(carry)])
                carry = [pieces[0]]
                for piece in reversed(pieces[1:]):
                    yield piece.decode(_ENCODING, errors="replace")
            yield b"".join(reversed(carry)).decode(_ENCODING, errors="replace")
    except OSError:
        return


def last_match(path: Path, pattern: str) -> str | None:
    """``tac FILE | grep -m 1 PATTERN`` / ``grep PATTERN FILE | tail -1``: the last matching line."""
    regex = re.compile(pattern)
    for line in _lines_backwards(path):
        if regex.search(line):
            return line
    return None


def first_match(path: Path, pattern: str) -> str | None:
    """The first line matching PATTERN."""
    regex = re.compile(pattern)
    for line in _lines(path):
        if regex.search(line):
            return line
    return None


def all_matches(path: Path, pattern: str) -> list[str]:
    """``grep PATTERN FILE``: every matching line."""
    regex = re.compile(pattern)
    return [line for line in _lines(path) if regex.search(line)]


def last_line(path: Path) -> str | None:
    """``tail -1 FILE``; ``None`` for an empty or unreadable file."""
    return next(_lines_backwards(path), None)


def last_nonempty_line(path: Path) -> str | None:
    """``awk 'NF{s=$0}END{print s}' FILE``: the last line with a field (awk's blanks: space and tab)."""
    for line in _lines_backwards(path):
        if line.strip(" \t"):
            return line
    return None


def find_all_text(path: Path, pattern: str) -> list[re.Match[str]]:
    """``grep -zoP PATTERN FILE``: every match of a (possibly multi-line) pattern over the whole file."""
    try:
        text = path.read_text(encoding=_ENCODING, errors="replace")
    except OSError:
        return []
    return list(re.finditer(pattern, text))


def count_files(root: Path, glob: str) -> int:
    """``find ROOT -name GLOB | wc -l``: matching entries at any depth below ROOT."""
    try:
        return sum(1 for _ in root.rglob(glob))
    except OSError:
        return 0


def csv_rows(path: Path, delimiter: str) -> list[dict[str, str]]:
    """``read.csv2(path, sep = delimiter)``: the data rows by column name, blank rows skipped.

    Extra fields of a row (more than the header has) are ignored; ``read.csv2``'s own quirks
    (BUG-045) are not reproduced.

    Raises:
        ModelstatsError: The file cannot be read or parsed.
    """
    try:
        with path.open(encoding="utf-8", errors="replace", newline="") as handle:
            rows = list(csv.DictReader(handle, delimiter=delimiter))
    except (OSError, csv.Error) as error:
        raise ModelstatsError(f"{path}: {error}") from error
    kept = []
    for row in rows:
        cells = {key: value for key, value in row.items() if key is not None and isinstance(value, str)}
        if any(value.strip() for value in cells.values()):
            kept.append(cells)
    return kept


def csv_line_count(path: Path, delimiter: str) -> int:
    """``nrow(read.csv2(path))``: the non-blank lines after the header.

    Raises:
        ModelstatsError: The file cannot be read or parsed.
    """
    try:
        with path.open(encoding="utf-8", errors="replace", newline="") as handle:
            rows = [
                row for row in csv.reader(handle, delimiter=delimiter) if any(cell.strip() for cell in row)
            ]
    except (OSError, csv.Error) as error:
        raise ModelstatsError(f"{path}: {error}") from error
    return max(0, len(rows) - 1)


def csv_unique_values(path: Path, column: str, delimiter: str = ",") -> int:
    """``length(unique(read.csv2(path, sep = ",")$column))``: 0 when the column is absent."""
    return len({row[column] for row in csv_rows(path, delimiter) if column in row})


def r_strsplit(text: str, separator: str) -> list[str]:
    """``strsplit(text, separator, fixed = TRUE)[[1]]``: one trailing empty field dropped, "" gives []."""
    if text == "":
        return []
    parts = text.split(separator)
    if len(parts) > 1 and parts[-1] == "":
        parts.pop()
    return parts


def compile_pattern(pattern: str, *, source: str) -> re.Pattern[str]:
    """A user-given regular expression, or ``ModelstatsError`` naming where it came from (BUG-072).

    A pattern Python only warns about (``[[:digit:]]``, a nested set) is an error too: it would not
    mean what the user meant.
    """
    try:
        with warnings.catch_warnings():
            warnings.simplefilter("error", FutureWarning)
            return re.compile(pattern)
    except (re.error, FutureWarning, OverflowError, RecursionError, ValueError) as error:
        # a{4294967296}, 5000 parentheses, a repeat count of more than 4300 digits
        raise ModelstatsError(f"{source}: invalid pattern {pattern!r}: {error}") from error
