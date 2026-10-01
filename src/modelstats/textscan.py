"""Bounded-memory text scanners replacing the shell-outs of ``R/getRunStatus.R`` (02 section 4.9).

Every function reproduces what the R code gets back from ``system(cmd, intern = TRUE)`` for
one shell pipeline, including the quirks of the tools involved:

- ``tac FILE | grep -m 1 PATTERN`` (:func:`last_match`): ``tac`` attaches the newline as a
  suffix of each record, so when the file does not end with a newline its last partial line
  is glued in front of the previous line (``a\\nb\\nc`` becomes ``cb``, ``a``). The file is
  read backwards in 1 MB chunks; a 500 MB ``full.gms`` never sits in memory.
- ``grep PATTERN FILE`` / ``... | tail -1`` (:func:`all_matches`, :func:`last_match_forward`,
  :func:`count_lines_matching`): one line per match, final partial line included.
- ``awk 'NF{s=$0}END{print s}'`` (:func:`last_nonempty_line`): the last line holding a
  character other than space or tab (``\\r`` counts as content); ``""`` when there is none,
  because awk still prints ``s``.
- ``tail -1`` (:func:`last_line`): nothing for an empty file, else the last line.
- ``grep -zoP PATTERN FILE`` (:func:`grep_z_only_matching`, :func:`magpie_warnings`,
  :func:`remind_warnings`): the file is split into NUL-terminated records, every match of the
  Perl regex is printed followed by a NUL; ``tail -1`` then keeps the last ``\\n``-line of that
  stream and R's ``system(intern = TRUE)`` cuts every line at its first NUL (BUG-011, D-06
  parity). ``grep -P`` in a UTF-8 locale never lets ``.`` match an invalid byte, which the
  patterns reproduce on text decoded with ``surrogateescape``.

Patterns are Python regular expressions applied with ``re.search`` to one line at a time
(without its newline). The R code uses basic ``grep`` syntax, so callers translate: ``*** Status:``
becomes ``\\*\\*\\* Status: ``, everything else used by modelstats is literal or POSIX-compatible.
Lines are decoded as UTF-8 with ``surrogateescape`` so that invalid bytes round-trip; GNU grep's
"binary file matches" suppression for such lines is not reproduced (no fixture log triggers it).
"""

from __future__ import annotations

import os
import re
from collections.abc import Iterator
from pathlib import Path
from typing import BinaryIO

__all__ = [
    "CHUNK_SIZE",
    "MAGPIE_WARNINGS_PATTERN",
    "REMIND_THERE_WERE_PATTERN",
    "REMIND_WARNINGS_BLOCK_PATTERN",
    "Pattern",
    "all_matches",
    "count_lines_matching",
    "grep_z_only_matching",
    "iter_lines",
    "last_line",
    "last_match",
    "last_match_forward",
    "last_nonempty_line",
    "magpie_warnings",
    "r_intern_lines",
    "remind_there_were_warnings",
    "remind_warnings",
    "remind_warnings_block_count",
    "tac_lines",
    "tail_1",
]

type Pattern = str | re.Pattern[str]
type PathLike = str | os.PathLike[str]

CHUNK_SIZE = 1 << 20

# grep -P in a UTF-8 locale: `.` matches any character but the newline and never an
# invalid byte (which surrogateescape decoding turns into U+DC80..U+DCFF).
_DOT = "[^\n\udc80-\udcff]"
MAGPIE_WARNINGS_PATTERN = re.compile(f"Warning messages:\n([0-9]+:({_DOT}*\n)?{_DOT}*\n)*([0-9]+)")
"""``R/getRunStatus.R:258``: the MAgPIE block whose trailing number counts the warnings."""
REMIND_THERE_WERE_PATTERN = re.compile("There were ([0-9]+) warnings")
"""``R/getRunStatus.R:267``."""
REMIND_WARNINGS_BLOCK_PATTERN = re.compile(f"Warning messages:\n([0-9]+:({_DOT}*\n)?{_DOT}*\n)*")
"""``R/getRunStatus.R:272``: the block whose ``N:`` lines are counted."""


def _compile(regex: Pattern) -> re.Pattern[str]:
    return regex if isinstance(regex, re.Pattern) else re.compile(regex)


def _decode(data: bytes) -> str:
    return data.decode("utf-8", "surrogateescape")


# ---------------------------------------------------------------------------
# forward reading
# ---------------------------------------------------------------------------


def iter_lines(path: PathLike) -> Iterator[str]:
    """The lines of a file as grep sees them: split on ``\\n`` only, final partial line included."""
    with open(path, "rb") as fh:
        for raw in fh:
            yield _decode(raw[:-1] if raw.endswith(b"\n") else raw)


def all_matches(path: PathLike, regex: Pattern) -> list[str]:
    """``grep PATTERN FILE``: every line matching ``regex``, in file order."""
    pattern = _compile(regex)
    return [line for line in iter_lines(path) if pattern.search(line)]


def last_match_forward(path: PathLike, regex: Pattern) -> str | None:
    """``grep PATTERN FILE | tail -1``: the last matching line, ``None`` when nothing matches."""
    pattern = _compile(regex)
    found: str | None = None
    for line in iter_lines(path):
        if pattern.search(line):
            found = line
    return found


def count_lines_matching(path: PathLike, regex: Pattern) -> int:
    """``length(system("grep PATTERN FILE", intern = TRUE))``."""
    pattern = _compile(regex)
    return sum(1 for line in iter_lines(path) if pattern.search(line))


# ---------------------------------------------------------------------------
# backward reading
# ---------------------------------------------------------------------------


def _partial_tail(fh: BinaryIO, size: int, chunk_size: int) -> tuple[bytes, int]:
    """The bytes after the last newline of the file and the offset just past that newline."""
    pos = size
    buf = b""
    while pos > 0:
        n = min(chunk_size, pos)
        pos -= n
        fh.seek(pos)
        data = fh.read(n)
        i = data.rfind(b"\n")
        if i >= 0:
            return data[i + 1 :] + buf, pos + i + 1
        buf = data + buf
    return buf, 0


def _reverse_blocks(fh: BinaryIO, end: int, chunk_size: int) -> Iterator[bytes]:
    """Blocks of consecutive newline-terminated lines of ``fh[:end]``, last block first.

    Each block is ``b"\\n".join(lines)`` of complete lines in file order (no trailing
    newline); a line is never split across two blocks. ``end`` must point just past a
    newline (or be 0).
    """
    pos = end
    carry = b""  # the head of the first line of the later block, which continues in this one
    first = True
    while pos > 0:
        n = min(chunk_size, pos)
        pos -= n
        fh.seek(pos)
        data = fh.read(n)
        if first:
            first = False
            data = data[:-1]  # drop the terminating newline of the last complete line
        data += carry
        i = data.find(b"\n")
        if i < 0:
            carry = data
            continue
        carry = data[:i]
        yield data[i + 1 :]
    if end > 0:
        yield carry


def _reverse_lines(fh: BinaryIO, end: int, chunk_size: int) -> Iterator[bytes]:
    for block in _reverse_blocks(fh, end, chunk_size):
        yield from reversed(block.split(b"\n"))


def tac_lines(path: PathLike, chunk_size: int = CHUNK_SIZE) -> Iterator[str]:
    """The lines ``tac FILE`` emits, in that order, with its glue quirk for a missing final newline."""
    with open(path, "rb") as fh:
        size = fh.seek(0, os.SEEK_END)
        partial, end = _partial_tail(fh, size, chunk_size)
        lines = _reverse_lines(fh, end, chunk_size)
        if partial:
            yield _decode(partial + next(lines, b""))
        for line in lines:
            yield _decode(line)


def last_match(path: PathLike, regex: Pattern, chunk_size: int = CHUNK_SIZE) -> str | None:
    """``tac FILE | grep -m 1 PATTERN``: the last line matching ``regex`` (with tac's glue quirk).

    Reads the file backwards in ``chunk_size`` bytes; a block of lines is only split and
    searched line by line when a multi-line search of the block finds a candidate, so the
    cost per non-matching megabyte is one regex scan.
    """
    pattern = _compile(regex)
    block_pattern = re.compile(pattern.pattern, pattern.flags | re.MULTILINE)
    with open(path, "rb") as fh:
        size = fh.seek(0, os.SEEK_END)
        partial, end = _partial_tail(fh, size, chunk_size)
        blocks = _reverse_blocks(fh, end, chunk_size)
        if partial:
            glued: bytes | None = None
            for block in blocks:
                i = block.rfind(b"\n")
                glued = partial + block[i + 1 :]
                rest = block[:i] if i >= 0 else None
                if rest is not None:
                    blocks = _prepend(rest, blocks)
                break
            line = _decode(partial if glued is None else glued)
            if pattern.search(line):
                return line
        for block in blocks:
            text = _decode(block)
            if block_pattern.search(text) is None:
                continue
            for line in reversed(text.split("\n")):
                if pattern.search(line):
                    return line
    return None


def _prepend(block: bytes, blocks: Iterator[bytes]) -> Iterator[bytes]:
    yield block
    yield from blocks


def last_nonempty_line(path: PathLike, chunk_size: int = CHUNK_SIZE) -> str:
    """``awk 'NF{s=$0}END{print s}' FILE``: the last line with a non-blank character, else ``""``."""
    with open(path, "rb") as fh:
        size = fh.seek(0, os.SEEK_END)
        partial, end = _partial_tail(fh, size, chunk_size)
        if partial and partial.strip(b" \t"):
            return _decode(partial)
        for line in _reverse_lines(fh, end, chunk_size):
            if line.strip(b" \t"):
                return _decode(line)
    return ""


def last_line(path: PathLike, chunk_size: int = CHUNK_SIZE) -> str | None:
    """``tail -1 FILE``: ``None`` for an empty file, else the last line (``""`` for a trailing blank line)."""
    with open(path, "rb") as fh:
        size = fh.seek(0, os.SEEK_END)
        if size == 0:
            return None
        partial, end = _partial_tail(fh, size, chunk_size)
        if partial:
            return _decode(partial)
        return _decode(next(_reverse_lines(fh, end, chunk_size), b""))


# ---------------------------------------------------------------------------
# grep -zoP and R's system(intern = TRUE)
# ---------------------------------------------------------------------------


def grep_z_only_matching(path: PathLike, pattern: re.Pattern[str]) -> bytes:
    """The byte stream ``grep -zoP PATTERN FILE`` writes: each match followed by NUL, record by record.

    The whole file is read (the slurm.log / log.txt files this serves are a few hundred KB).
    """
    data = Path(path).read_bytes()
    out = bytearray()
    for record in data.split(b"\0"):
        for match in pattern.finditer(_decode(record)):
            out += match.group(0).encode("utf-8", "surrogateescape")
            out += b"\0"
    return bytes(out)


def tail_1(stream: bytes) -> bytes | None:
    """``tail -1`` of a byte stream: ``None`` for an empty stream, else the last line as written."""
    if not stream:
        return None
    body = stream[:-1] if stream.endswith(b"\n") else stream
    return stream[body.rfind(b"\n") + 1 :]


def r_intern_lines(stream: bytes | str) -> list[str]:
    """What ``system(cmd, intern = TRUE)`` makes of a command's stdout (R 4.6.1, verified).

    The stream is split on ``\\n``; a trailing newline adds no element, a final partial line
    does (even when it is empty after the cut); every line is cut at its first NUL byte;
    ``\\r`` is kept; lines are never split by length.
    """
    data = stream.encode("utf-8", "surrogateescape") if isinstance(stream, str) else stream
    if not data:
        return []
    parts = data.split(b"\n")
    if parts[-1] == b"":
        parts.pop()
    return [_decode(part.split(b"\0", 1)[0]) for part in parts]


def magpie_warnings(path: PathLike) -> str:
    """``R/getRunStatus.R:257-265`` for an existing slurm.log of a MAgPIE run.

    The trailing number of the last ``Warning messages:`` block (what ``grep -zoP ... | tail -1``
    leaves after R's NUL cut), else ``"1"`` when a line contains ``Warning message:``, else ``"0"``.
    """
    last = tail_1(grep_z_only_matching(path, MAGPIE_WARNINGS_PATTERN))
    if last is not None:
        lines = r_intern_lines(last)
        if lines:
            return lines[0]
    if last_match_forward(path, "Warning message:") is not None:
        return "1"
    return "0"


def remind_there_were_warnings(path: PathLike) -> list[str]:
    """``system("grep -zoP \\"There were ([0-9]+) warnings\\" log.txt", intern = TRUE)`` as R sees it.

    All matches share one NUL-separated line, so the cut keeps the first match only (BUG-011).
    """
    return r_intern_lines(grep_z_only_matching(path, REMIND_THERE_WERE_PATTERN))


def remind_warnings_block_count(path: PathLike) -> int:
    """``length(grep("^[0-9]+:", system("grep -zoP \\"Warning messages:...\\" log.txt", intern = TRUE)))``."""
    lines = r_intern_lines(grep_z_only_matching(path, REMIND_WARNINGS_BLOCK_PATTERN))
    return sum(1 for line in lines if re.match("[0-9]+:", line))


def remind_warnings(path: PathLike) -> str:
    """``R/getRunStatus.R:266-274`` for an existing log.txt of a REMIND run.

    The digits of the first ``There were N warnings`` line (``gsub("^[^0-9]*([0-9]+)[^0-9]*$", "\\\\1", .)``),
    else the number of ``N:`` lines of the ``Warning messages:`` blocks as a string (R assigns the
    integer into the character column).
    """
    there_were = remind_there_were_warnings(path)
    if there_were:
        return re.sub("^[^0-9]*([0-9]+)[^0-9]*$", r"\1", there_were[0])
    return str(remind_warnings_block_count(path))
