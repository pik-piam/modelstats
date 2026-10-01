r"""Bounded-memory text scanners: the shell pipelines of ``R/getRunStatus.R`` in Python.

``getRunStatus()`` reads its log files through ``system(<pipeline>, intern = TRUE)``: ``tac f | grep
-m 1 P``, ``grep P f``, ``grep P f | tail -1``, ``tail -1 f``, ``awk 'NF{s=$0}END{print s}' f``,
``grep -zoP P f`` (with and without ``| tail -1``), ``grep P f | grep -Eo '[0-9]{1,2}'`` and ``find
DIR -name 'X'``. This module replaces each pipeline by a pure Python scanner that produces the same
bytes the pipeline would have printed and then converts them into the character vector R received
(:func:`intern_lines`), so the string post-processing the R code applies next (``sub``, ``substr``,
``gsub``, ``length()``) sees identical input. Memory is bounded: the reverse scanners read the file
from its end in :data:`CHUNK_SIZE` blocks and stop at the first hit, the forward scanners stream
line by line; only the ``grep -z`` scanners load the whole file, because their patterns span lines
(they run on ``log.txt`` and ``slurm.log``, a few megabytes at most). The real ``full.gms`` is
300-500 MB and only its last ``cm_iteration_max = N;`` line matters (``migration/03-migration-plan.md``
section 5, "No shell-outs for text scanning").

Pipeline semantics reproduced (GNU coreutils 9 / grep 3, as on the cluster):

* ``tac``: records are ``\n``-terminated; an unterminated last line is printed first *without* a
  separator, so it runs into the previous line (``printf 'a\nb' | tac`` gives ``ba``). The port joins
  them the same way (:func:`tac_records`), which only matters for a file without a final newline.
* ``grep P`` (basic regular expressions): every line containing a match, in file order; the last line
  counts even without a newline. ``-m 1`` stops at the first hit, ``-c`` counts lines, ``| tail -1``
  keeps the last line. The BRE ``'*** Status: '`` (``R/getRunStatus.R:157``) is *not* three literal
  stars: a leading ``*`` is literal and the next two quantify it, so it matches every line containing
  ``" Status: "`` (:data:`STATUS_LINE`, verified with GNU grep 3.12).
* ``grep -zoP P``: ``-z`` makes the whole file one NUL-separated record, ``-o`` prints each match
  followed by a NUL. The pattern runs over the file text with Python's ``re``, whose semantics equal
  PCRE's for these patterns (greedy, backtracking, ``.`` excluding ``\n``).
* ``tail -1``: the last ``\n``-delimited line, with its newline when it has one; nothing for an
  empty input. ``awk 'NF{s=$0}END{print s}'``: the last line with at least one whitespace-separated
  field, and an empty line when there is none (so R always receives one element, ``""`` at worst).
* ``system(intern = TRUE)`` (``src/unix/sys-unix.c``): the output is read with ``fgets`` into an
  8096-byte buffer and each piece becomes one element after ``strlen``, i.e. a line longer than
  8095 bytes is split into several elements and a line is cut at its first NUL byte. The NUL cut is
  what makes ``grep -zoP ... | tail -1`` return the digits of the last block (register BUG-011,
  decision D-06): only the bytes after the last newline survive, up to the NUL.

Not reproduced (documented, no golden exercises them): GNU grep's binary-file detection (a NUL byte
in the first buffer, or an encoding error in a matching line under a UTF-8 locale, suppresses the
output of the matching lines; where that would change a value is version-dependent, grep 3.5
moved the notice to stderr), and the unquoted paths of ``R/getRunStatus.R:162, 258, 261, 267, 272,
302-303`` (a run directory with a space or a shell metacharacter breaks the R commands). Both are
recorded as suspected R bugs by this unit.

Every scanner takes a path or an open seekable binary stream (``Effects.open_binary`` hands the
latter out for the overlay of the golden harness) and returns the character vector R received as a
``list[str]`` (``surrogateescape`` decoding keeps undecodable bytes), or a convenience form of it.
"""

from __future__ import annotations

import fnmatch
import io
import os
import re
from collections.abc import Callable, Iterator
from contextlib import contextmanager
from typing import BinaryIO, Final

__all__ = [
    "CALIBRATION_LINE",
    "CHUNK_SIZE",
    "INTERN_LINE_LIMIT",
    "ITERATION_MAX_LINE",
    "LOOPS_LINE",
    "MAGPIE_OUTPUT_STORED",
    "MAGPIE_PREPARING",
    "MAGPIE_REPORT_START",
    "MAGPIE_WARNING_BLOCK",
    "NUMBERED_WARNING_LINE",
    "ONE_OR_TWO_DIGITS",
    "OUTPUT_GENERATION_END",
    "OUTPUT_GENERATION_START",
    "REMIND_WARNING_BLOCK",
    "SLURMSTEPD_ERROR",
    "STATUS_LINE",
    "STATUS_PLURAL",
    "STATUS_PREFIX",
    "STRIP_SEMICOLON",
    "STRIP_TO_ASSIGNMENT",
    "WARNING_MESSAGE_LINE",
    "WARNING_SUMMARY",
    "WARNING_SUMMARY_DIGITS",
    "all_matches",
    "count_files_named",
    "count_matches",
    "count_numbered_warning_lines",
    "grep_z_only_matching",
    "has_warning_message",
    "intern_lines",
    "iter_lines",
    "last_block_match",
    "last_line",
    "last_match",
    "last_matching_line",
    "last_nonempty_line",
    "scan_calibration_iteration",
    "scan_iteration_max",
    "scan_last_line",
    "scan_last_loops",
    "scan_last_match",
    "scan_last_matching_line",
    "scan_last_nonempty_line",
    "scan_magpie_warnings",
    "scan_slurmstepd_errors",
    "scan_status_lines",
    "scan_warning_summary",
    "tac_records",
    "tail_last_line",
]

type PathLike = str | os.PathLike[str]
type Source = PathLike | BinaryIO
type Pattern = re.Pattern[bytes] | bytes
"""A compiled bytes regular expression, or a bytes literal matched as a fixed string."""

CHUNK_SIZE: Final = 1 << 20
"""Bytes read per step by the reverse scanners (plan section 5: 1 MB)."""

INTERN_LINE_LIMIT: Final = 8095
"""``INTERN_BUFSIZE - 1`` of ``src/unix/sys-unix.c``: the longest ``system(intern = TRUE)`` element."""

_ENCODING: Final = "utf-8"
_ERRORS: Final = "surrogateescape"


def _literal(text: bytes) -> re.Pattern[bytes]:
    """A fixed-string grep pattern (no BRE metacharacter in the R source) as a compiled regex."""
    return re.compile(re.escape(text))


# ---------------------------------------------------------------------------------------------
# The regular expressions of R/getRunStatus.R, one constant per shell-out
# ---------------------------------------------------------------------------------------------

ITERATION_MAX_LINE: Final = re.compile(rb"cm_iteration_max = [1-9].*.;[ ]*$")
"""``tac full.gms | grep -m 1 'cm_iteration_max = [1-9].*.;[ ]*$'`` (``R/getRunStatus.R:144``)."""

STRIP_TO_ASSIGNMENT: Final = re.compile(r"^.*.= ")
"""``sub("^.*.= ", "", x)`` (``R/getRunStatus.R:145, 154, 230``): everything through the last ``= ``."""

STRIP_SEMICOLON: Final = re.compile(r";[ ]*")
"""``sub(";[ ]*", "", x)`` (``R/getRunStatus.R:145``): the first semicolon and the blanks after it."""

LOOPS_LINE: Final = _literal(b"LOOPS")
"""``grep 'LOOPS' full.log | tail -1`` (``R/getRunStatus.R:154``, and ``:230`` for the MAgPIE log)."""

STATUS_LINE: Final = re.compile(rb"\** Status: ")
"""``grep '*** Status: ' full.log`` (``R/getRunStatus.R:157``): every line containing ``" Status: "``.

A leading ``*`` is literal in a BRE and the next two quantify it, so GNU grep treats the stars as optional.
"""

STATUS_PREFIX: Final = re.compile(r"\*\*\* Status: ")
r"""``sub("\\*\\*\\* Status: ", "", x)`` (``R/getRunStatus.R:157``)."""

STATUS_PLURAL: Final = re.compile(r"\(s\)")
r"""``sub("\\(s\\)", "", x)`` (``R/getRunStatus.R:157``): the first ``(s)`` of ``Execution error(s)``."""

SLURMSTEPD_ERROR: Final = re.compile(rb"slurmstepd.*error")
"""``grep 'slurmstepd.*error' log.txt`` (``R/getRunStatus.R:162``)."""

OUTPUT_GENERATION_START: Final = _literal(b"Starting output generation for")
"""``tac log.txt | grep -m 1 'Starting output generation for'`` (``R/getRunStatus.R:204``)."""

OUTPUT_GENERATION_END: Final = _literal(b"Finished output generation for")
"""``tac log.txt | grep -m 1 'Finished output generation for'`` (``R/getRunStatus.R:205``)."""

MAGPIE_PREPARING: Final = _literal(b"Preparing MAgPIE")
"""``tac log-mag.txt | grep -m 1 'Preparing MAgPIE'`` (``R/getRunStatus.R:223``)."""

MAGPIE_OUTPUT_STORED: Final = _literal(b"MAgPIE output was stored")
"""``tac log-mag.txt | grep -m 1 'MAgPIE output was stored'`` (``R/getRunStatus.R:224``)."""

MAGPIE_REPORT_START: Final = _literal(b"Start getReport")
"""``tac log-mag.txt | grep -m 1 'Start getReport'`` (``R/getRunStatus.R:232``)."""

MAGPIE_WARNING_BLOCK: Final = re.compile(rb"Warning messages:\n([0-9]+:(.*\n)?.*\n)*([0-9]+)")
r"""``grep -zoP "Warning messages:\n([0-9]+:(.*\n)?.*\n)*([0-9]+)" f | tail -1`` (``getRunStatus.R:258``)."""

WARNING_MESSAGE_LINE: Final = _literal(b"Warning message:")
"""``grep "Warning message:" slurm.log | tail -1`` (``R/getRunStatus.R:261``)."""

WARNING_SUMMARY: Final = re.compile(rb"There were ([0-9]+) warnings")
"""``grep -zoP "There were ([0-9]+) warnings" log.txt`` (``R/getRunStatus.R:267``)."""

WARNING_SUMMARY_DIGITS: Final = re.compile(r"^[^0-9]*([0-9]+)[^0-9]*$")
r"""``gsub("^[^0-9]*([0-9]+)[^0-9]*$", "\\1", warnings)`` (``R/getRunStatus.R:269``)."""

REMIND_WARNING_BLOCK: Final = re.compile(rb"Warning messages:\n([0-9]+:(.*\n)?.*\n)*")
r"""``grep -zoP "Warning messages:\n([0-9]+:(.*\n)?.*\n)*" log.txt`` (``R/getRunStatus.R:272``)."""

NUMBERED_WARNING_LINE: Final = re.compile(r"^[0-9]+:")
"""``grep("^[0-9]+:", warnings)`` (``R/getRunStatus.R:273``), R's own ``grep`` over the elements."""

CALIBRATION_LINE: Final = _literal(b"CES calibration iteration")
"""``grep 'CES calibration iteration' log.txt`` (``R/getRunStatus.R:299``)."""

ONE_OR_TWO_DIGITS: Final = re.compile(rb"[0-9]{1,2}")
"""``grep -Eo '[0-9]{1,2}'`` (``R/getRunStatus.R:299``): every run of one or two digits, leftmost first."""


# ---------------------------------------------------------------------------------------------
# Streams and lines
# ---------------------------------------------------------------------------------------------


@contextmanager
def _opened(source: Source) -> Iterator[BinaryIO]:
    """A binary stream for ``source``: a path is opened and closed here, a stream is used as is."""
    if isinstance(source, str | os.PathLike):
        with open(source, "rb") as stream:
            yield stream
    else:
        yield source


def _compiled(pattern: Pattern) -> re.Pattern[bytes]:
    """The pattern as a compiled regex (a bytes literal is a fixed string)."""
    return _literal(pattern) if isinstance(pattern, bytes) else pattern


def _decode(data: bytes) -> str:
    """Bytes as R would hold them in a character string (undecodable bytes are kept, not dropped)."""
    return data.decode(_ENCODING, _ERRORS)


def _strip_newline(line: bytes) -> bytes:
    r"""A line without its single trailing ``\n`` (``\r`` stays part of the line, as for grep)."""
    return line[:-1] if line.endswith(b"\n") else line


def iter_lines(source: Source) -> Iterator[bytes]:
    r"""The lines of a file as ``grep`` sees them: split at ``\n``, no newline, a partial last line included.

    Streams through the file with the buffered reader; memory is bounded by the longest line.
    """
    with _opened(source) as stream:
        stream.seek(0)
        for raw in stream:
            yield _strip_newline(raw)


type BlockFilter = Callable[[bytearray, int, int, bytes], bool]
"""Decides whether a block of whole lines is wanted before it is copied out of the read buffer.

Called as ``keep(buffer, start, end, tail)``: ``buffer[start:end]`` holds the block's newline-terminated
lines that lie inside the current chunk and ``tail`` its last line without newline (the line that runs
on into the chunk after it, or the file's unterminated last line); ``start == end`` when the block has
no other line.
"""


def _fill(stream: BinaryIO, target: memoryview) -> None:
    """Read exactly ``len(target)`` bytes into ``target``, in place where the stream supports it."""
    filled = 0
    while filled < len(target):
        if isinstance(stream, io.BufferedIOBase | io.RawIOBase):
            count = stream.readinto(target[filled:]) or 0
        else:
            data = stream.read(len(target) - filled)
            count = len(data)
            target[filled : filled + count] = data
        if count == 0:
            raise OSError("the stream ended before the expected byte count")
        filled += count


def _without_newline(fragment: bytes) -> bytes:
    r"""A carried-over line fragment without the ``\n`` it may end with."""
    return fragment[:-1] if fragment.endswith(b"\n") else fragment


def _reverse_blocks(stream: BinaryIO, chunk_size: int, keep: BlockFilter | None = None) -> Iterator[bytes]:
    r"""Runs of whole lines from the end of ``stream``: ``\n``-terminated blocks of ~``chunk_size`` bytes.

    One ``chunk_size`` buffer is reused for every read; a line cut by a chunk boundary is carried over
    (``pending``) until its start is read, so a block never splits a line and the memory bound is one
    chunk plus the longest line. ``keep`` (:data:`BlockFilter`) is asked before a block is copied out
    of the buffer, and an unwanted block costs no copy at all. The first block yielded lacks the final
    newline when the file does; the last block is the file's first line.
    """
    stream.seek(0, os.SEEK_END)
    position = stream.tell()
    buffer = bytearray(chunk_size)
    view = memoryview(buffer)
    pending = b""
    while position > 0:
        size = min(chunk_size, position)
        position -= size
        stream.seek(position)
        _fill(stream, view[:size])
        cut = buffer.find(b"\n", 0, size) + 1
        if cut == 0:
            pending = bytes(view[:size]) + pending
            continue
        last_newline = buffer.rfind(b"\n", 0, size)
        wanted = keep is None or keep(
            buffer, cut, last_newline + 1, bytes(view[last_newline + 1 : size]) + _without_newline(pending)
        )
        if wanted and (cut < size or pending):
            yield bytes(view[cut:size]) + pending
        pending = bytes(view[:cut])
    if pending and (keep is None or keep(buffer, 0, 0, _without_newline(pending))):
        yield pending


def _block_lines(block: bytes) -> list[bytes]:
    r"""The lines of a block in file order, without newlines; a block ending in ``\n`` has no empty tail."""
    lines = block.split(b"\n")
    if lines[-1] == b"":
        lines.pop()
    return lines


def _block_filter(pattern: re.Pattern[bytes]) -> BlockFilter:
    r"""The :data:`BlockFilter` that keeps a block only if some line of it may match ``pattern``.

    Runs the pattern once over the lines inside the read buffer in multi-line mode, where ``^``/``$``
    are line anchors and ``.`` stops at ``\n``, plus once over the block's tail line, so a per-line
    match is always a block match: the filter has no false negatives for a pattern without
    look-behind or ``\A``/``\Z`` (none of this module's patterns has one), and a false positive only
    costs the per-line scan of that block. This is what makes the reverse scan of a 500 MB file run
    at regex speed instead of per-line call overhead, without copying the chunks it rejects.
    """
    multiline = re.compile(pattern.pattern, pattern.flags | re.MULTILINE)

    def keep(buffer: bytearray, start: int, end: int, tail: bytes) -> bool:
        return multiline.search(buffer, start, end) is not None or multiline.search(tail) is not None

    return keep


def tac_records(
    source: Source, *, chunk_size: int = CHUNK_SIZE, keep_block: BlockFilter | None = None
) -> Iterator[bytes]:
    r"""The records ``tac`` prints, last first, each without its ``\n``.

    ``tac`` attaches the separator to the end of a record; a last line without one is printed as is
    and the previous record follows on the same output line, so the two are joined here. A block of
    whole lines that ``keep_block`` rejects is skipped without being copied or split (the prefilter
    of :func:`last_match`); the blocks taking part in the join are always read.
    """
    with _opened(source) as stream:
        joining = True

        def keep(buffer: bytearray, start: int, end: int, tail: bytes) -> bool:
            return joining or keep_block is None or keep_block(buffer, start, end, tail)

        blocks = _reverse_blocks(stream, chunk_size, keep)
        first = next(blocks, None)
        if first is None:
            return
        partial: bytes | None = None
        lines = _block_lines(first)
        if not first.endswith(b"\n"):
            partial = lines.pop()
            if lines:
                yield partial + lines.pop()
                partial = None
        joining = partial is not None
        yield from reversed(lines)
        for block in blocks:
            lines = _block_lines(block)
            if partial is not None:
                yield partial + lines.pop()
                partial = None
                joining = False
            yield from reversed(lines)
        if partial is not None:
            yield partial


# ---------------------------------------------------------------------------------------------
# What system(intern = TRUE) makes of a pipeline's output
# ---------------------------------------------------------------------------------------------


def intern_lines(output: bytes) -> list[str]:
    """``system(cmd, intern = TRUE)`` applied to the bytes ``cmd`` printed (``src/unix/sys-unix.c``).

    Each ``fgets(buf, 8096)`` piece (through a newline or 8095 bytes) becomes one element after
    ``strlen``: the piece is cut at its first NUL byte, then a trailing newline is dropped. An empty
    output is ``character(0)``.
    """
    elements: list[str] = []
    position = 0
    size = len(output)
    while position < size:
        newline = output.find(b"\n", position, position + INTERN_LINE_LIMIT)
        end = newline + 1 if newline >= 0 else min(position + INTERN_LINE_LIMIT, size)
        piece = output[position:end].split(b"\0", 1)[0]
        position = end
        elements.append(_decode(_strip_newline(piece)))
    return elements


def tail_last_line(output: bytes) -> bytes:
    r"""``| tail -1``: the last ``\n``-delimited line of ``output`` (with its newline if it has one)."""
    if not output:
        return b""
    body = output[:-1] if output.endswith(b"\n") else output
    return output[body.rfind(b"\n") + 1 :]


def _as_output(lines: list[bytes]) -> bytes:
    """Lines as a program prints them: each terminated by a newline."""
    return b"".join(line + b"\n" for line in lines)


# ---------------------------------------------------------------------------------------------
# The pipelines, each returning the character vector R received
# ---------------------------------------------------------------------------------------------


def scan_last_match(source: Source, pattern: Pattern, *, chunk_size: int = CHUNK_SIZE) -> list[str]:
    """``tac f | grep -m 1 P``: the last line matching ``pattern``, as R received it.

    ``character(0)`` (an empty list) without a match; otherwise one element, or several when the
    line exceeds 8095 bytes (:func:`intern_lines`). Reads the file backwards in ``chunk_size`` blocks
    and stops at the first hit. Used by ``R/getRunStatus.R:144, 204-205, 223-224, 232``.
    """
    regex = _compiled(pattern)
    for record in tac_records(source, chunk_size=chunk_size, keep_block=_block_filter(regex)):
        if regex.search(record):
            return intern_lines(record + b"\n")
    return []


def last_match(source: Source, pattern: Pattern, *, chunk_size: int = CHUNK_SIZE) -> str | None:
    """The first element of :func:`scan_last_match`, or ``None`` for no match."""
    found = scan_last_match(source, pattern, chunk_size=chunk_size)
    return found[0] if found else None


def all_matches(source: Source, pattern: Pattern) -> list[str]:
    """``grep P f``: every line matching ``pattern``, in file order, as R received it.

    ``R/getRunStatus.R:157`` (the ``*** Status:`` lines), ``:162`` (``slurmstepd`` errors).
    """
    regex = _compiled(pattern)
    return intern_lines(_as_output([line for line in iter_lines(source) if regex.search(line)]))


def scan_last_matching_line(source: Source, pattern: Pattern) -> list[str]:
    """``grep P f | tail -1``: the last matching line in file order, as R received it.

    Unlike :func:`scan_last_match` there is no ``tac`` join of a partial last line; ``character(0)``
    (an empty list) without a match. ``R/getRunStatus.R:154, 230, 261``.
    """
    regex = _compiled(pattern)
    last: bytes | None = None
    for line in iter_lines(source):
        if regex.search(line):
            last = line
    return [] if last is None else intern_lines(last + b"\n")


def last_matching_line(source: Source, pattern: Pattern) -> str | None:
    """The first element of :func:`scan_last_matching_line`, ``None`` without a match."""
    found = scan_last_matching_line(source, pattern)
    return found[0] if found else None


def count_matches(source: Source, pattern: Pattern) -> int:
    """``grep -c P f``: the number of lines matching ``pattern``."""
    regex = _compiled(pattern)
    return sum(1 for line in iter_lines(source) if regex.search(line))


def scan_last_line(source: Source, *, chunk_size: int = CHUNK_SIZE) -> list[str]:
    """``tail -1 f``: the last line as R received it (``character(0)`` for an empty file).

    A file ending in a blank line gives ``[""]``. ``R/getRunStatus.R:235, 240`` test the result with
    ``grepl("try to acquire model lock", ...)``.
    """
    with _opened(source) as stream:
        for block in _reverse_blocks(stream, chunk_size):
            return intern_lines(tail_last_line(block))
    return []


def last_line(source: Source, *, chunk_size: int = CHUNK_SIZE) -> str | None:
    """The first element of :func:`scan_last_line`, ``None`` for an empty file."""
    found = scan_last_line(source, chunk_size=chunk_size)
    return found[0] if found else None


def _has_field(line: bytes) -> bool:
    """Awk's ``NF > 0`` with the default field separator: a character other than blank, tab or newline."""
    return line.strip(b" \t\n") != b""


def scan_last_nonempty_line(source: Source, *, chunk_size: int = CHUNK_SIZE) -> list[str]:
    """``awk 'NF{s=$0}END{print s}' f``: the last line with a field, as R received it.

    awk prints an empty line when no line had a field, so the result always has one element
    (``R/getRunStatus.R:189`` compares it with ``"Starting MAgPIE..."``).
    """
    with _opened(source) as stream:
        for block in _reverse_blocks(stream, chunk_size):
            for line in reversed(_block_lines(block)):
                if _has_field(line):
                    return intern_lines(line + b"\n")
    return intern_lines(b"\n")


def last_nonempty_line(source: Source, *, chunk_size: int = CHUNK_SIZE) -> str:
    """The one element of :func:`scan_last_nonempty_line` (``""`` when every line is blank)."""
    return scan_last_nonempty_line(source, chunk_size=chunk_size)[0]


def grep_z_only_matching(source: Source, pattern: re.Pattern[bytes]) -> bytes:
    """``grep -zoP P f``: every match over the whole file, each followed by a NUL byte.

    Loads the file (the patterns span lines); ``pattern`` must be a compiled regex whose semantics
    agree between PCRE and ``re`` (no PCRE-only syntax). ``R/getRunStatus.R:258, 267, 272``.
    """
    with _opened(source) as stream:
        stream.seek(0)
        text = stream.read()
    return b"".join(match.group(0) + b"\0" for match in pattern.finditer(text))


def last_block_match(source: Source, pattern: re.Pattern[bytes]) -> list[str]:
    """``grep -zoP P f | tail -1``: what survives of the last match, as R received it.

    ``tail -1`` keeps the bytes after the last newline of the NUL-separated output and
    ``system(intern = TRUE)`` cuts at the NUL, so the result is the last line of the last match
    (register BUG-011, decision D-06); ``character(0)`` without a match.
    """
    return intern_lines(tail_last_line(grep_z_only_matching(source, pattern)))


def count_files_named(directory: PathLike, name_pattern: str) -> int:
    """``length(system("find DIR -name 'PATTERN'", intern = TRUE))`` (``R/getRunStatus.R:302-303``).

    ``find`` tests the directory itself and everything below it against the shell pattern, does not
    follow symbolic links and silently skips what it cannot read; a missing directory gives 0.
    """
    root = os.fspath(directory)
    count = 1 if os.path.lexists(root) and fnmatch.fnmatchcase(os.path.basename(root), name_pattern) else 0
    for _root, dirs, files in os.walk(root, followlinks=False):
        count += sum(1 for name in dirs + files if fnmatch.fnmatchcase(name, name_pattern))
    return count


# ---------------------------------------------------------------------------------------------
# R/getRunStatus.R, line by line: pipeline plus the R string handling applied to its output
# ---------------------------------------------------------------------------------------------


def scan_iteration_max(full_gms: Source, *, chunk_size: int = CHUNK_SIZE) -> list[str]:
    """``cm_iteration_max`` from ``full.gms`` (``R/getRunStatus.R:144-145``).

    ``tac full.gms | grep -m 1 'cm_iteration_max = [1-9].*.;[ ]*$'`` then ``sub(";[ ]*", "",
    sub("^.*.= ", "", x))``: ``cm_iteration_max = 100;`` becomes ``100``. An empty list means the
    R vector was ``character(0)`` and the ``Iter`` cell gets no ``/max`` suffix.
    """
    found = scan_last_match(full_gms, ITERATION_MAX_LINE, chunk_size=chunk_size)
    return [STRIP_SEMICOLON.sub("", STRIP_TO_ASSIGNMENT.sub("", line, count=1), count=1) for line in found]


def scan_last_loops(full_log: Source) -> list[str]:
    """The last ``LOOPS`` value of ``full.log`` (``R/getRunStatus.R:154``, ``:230`` for the MAgPIE log).

    ``grep 'LOOPS' full.log | tail -1`` then ``sub("^.*.= ", "", x)``: ``--- LOOPS iteration = 26``
    becomes ``26``; ``character(0)`` (an empty list) without a ``LOOPS`` line.
    """
    found = scan_last_matching_line(full_log, LOOPS_LINE)
    return [STRIP_TO_ASSIGNMENT.sub("", line, count=1) for line in found]


def scan_status_lines(full_log: Source) -> list[str]:
    r"""The ``*** Status:`` lines of ``full.log`` (``R/getRunStatus.R:157``), cut to 17 characters.

    ``grep '*** Status: ' full.log`` then ``substr(sub("\\(s\\)", "", sub("\\*\\*\\* Status: ", "",
    x)), 1, 17)`` for every line. The caller applies the cell-assignment rule of register BUG-036: one
    element is the ``RunStatus``, zero or several leave it ``"NA"``.
    """
    lines = all_matches(full_log, STATUS_LINE)
    return [STATUS_PLURAL.sub("", STATUS_PREFIX.sub("", line, count=1), count=1)[:17] for line in lines]


def scan_slurmstepd_errors(log_txt: Source) -> list[str]:
    """``grep 'slurmstepd.*error' log.txt`` (``R/getRunStatus.R:162``): the lines the interrupt rules read."""
    return all_matches(log_txt, SLURMSTEPD_ERROR)


def scan_magpie_warnings(slurm_log: Source) -> list[str]:
    r"""The MAgPIE warning count from ``slurm.log`` (``R/getRunStatus.R:258``).

    ``grep -zoP "Warning messages:\n([0-9]+:(.*\n)?.*\n)*([0-9]+)" slurm.log | tail -1``: the number
    of the last numbered warning of the last block, e.g. ``["2"]``; ``[]`` without a block. The
    callers fall back to :func:`has_warning_message` and then to ``"0"`` (``:259-265``).
    """
    return last_block_match(slurm_log, MAGPIE_WARNING_BLOCK)


def has_warning_message(log: Source) -> bool:
    """``length(grep "Warning message:" f | tail -1) > 0`` (``R/getRunStatus.R:261``)."""
    return last_matching_line(log, WARNING_MESSAGE_LINE) is not None


def scan_warning_summary(log_txt: Source) -> list[str]:
    r"""The REMIND warning count from the ``There were N warnings`` line (``R/getRunStatus.R:267-269``).

    ``grep -zoP "There were ([0-9]+) warnings" log.txt`` gives every match on one NUL-separated line,
    which ``system(intern = TRUE)`` cuts at the first NUL, so only the first summary survives
    (register BUG-011); ``gsub("^[^0-9]*([0-9]+)[^0-9]*$", "\\1", x)`` keeps its digits. ``[]`` when
    there is no such line (``There were 50 or more warnings`` does not match, register BUG-029).
    """
    found = intern_lines(grep_z_only_matching(log_txt, WARNING_SUMMARY))
    return [WARNING_SUMMARY_DIGITS.sub(r"\1", line) for line in found]


def count_numbered_warning_lines(log_txt: Source) -> int:
    r"""The REMIND fallback warning count (``R/getRunStatus.R:272-273``).

    ``grep -zoP "Warning messages:\n([0-9]+:(.*\n)?.*\n)*" log.txt`` as R received it, then
    ``length(grep("^[0-9]+:", warnings))``: the numbered lines of every ``Warning messages:`` block.
    """
    elements = intern_lines(grep_z_only_matching(log_txt, REMIND_WARNING_BLOCK))
    return sum(1 for element in elements if NUMBERED_WARNING_LINE.search(element))


def scan_calibration_iteration(log_txt: Source) -> list[str]:
    """The last CES calibration iteration number (``R/getRunStatus.R:299``).

    ``tail(system("grep 'CES calibration iteration' log.txt | grep -Eo '[0-9]{1,2}'", intern = TRUE),
    n = 1)``: every one-or-two-digit run of every matching line, last one kept; ``[]`` without one.
    """
    digits = [
        group
        for line in iter_lines(log_txt)
        if CALIBRATION_LINE.search(line)
        for group in ONE_OR_TWO_DIGITS.findall(line)
    ]
    found = intern_lines(_as_output(digits))
    return found[-1:]
