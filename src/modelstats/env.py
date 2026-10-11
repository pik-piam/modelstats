"""``Effects``: the only door to the environment (``migration/03-migration-plan.md`` sections 2.3 and 5).

Everything the package reads from or does to the outside of its own process goes through one injectable
object: the filesystem below the run folders and the AMT checkout, the scheduler (``squeue``, ``sacct``,
``sbatch``), every other subprocess (``git``, ``Rscript``, ``make``, ``rsync``), HTTP (the Mattermost
webhook), the clock, sleeping, the user name, the ``/p`` probe, the environment variables and the lock that
serialises AMT invocations. Four implementations:

* ``ProductionEffects`` maps every call to the real system.
* ``FakeEffects`` is the test double of the ``rs`` goldens: an overlay of the fixture layers, a canned
  scheduler, a frozen clock and an injected identity (below). Mutations are real outside the overlay root
  (temporary trees in tests) and refused inside it, because the layers are the canonical snapshot.
* ``RecordingEffects`` is the AMT test double (``06`` section 4.4): the filesystem is real (a sandbox clone
  or a temporary checkout), subprocesses are answered from a case directory with the exact rules of the
  fake binaries (``CannedAmt`` ports ``migration/harness/fakebin/_lib.sh`` and the ``git``, ``sbatch``,
  ``rsync``, ``make``, ``Rscript`` and ``curl`` fakes; ``CannedSlurm`` the ``squeue``/``sacct`` fake), the
  webhook is answered from the same ``curl.txt``, ``sleep`` returns at once (the R oracle ran with a no-op
  ``Sys.sleep``), and every call is recorded: ``calls`` for all of them, ``trace`` in the schema of the
  fakes' ``trace.jsonl`` and ``posts`` in the schema of their ``mattermost.jsonl``, so that R and Python
  see the same answers and leave comparable records.
* ``DryRunEffects`` (``--dry-run``, ``03`` 2.3): reads execute for real through the wrapped effects, every
  mutation is logged (``would write <path>``) and answered with a synthetic result (a fake commit hash for
  ``git log -1``, an empty ``squeue``, ``Submitted batch job 0``), notifications are suppressed, nothing is
  written, run, posted or locked.

The R behaviours reproduced here, with the lines they come from:

* ``Sys.info()[["user"]]`` (``R/getRunStatus.R:24``, ``R/foundInSlurm.R:12``,
  ``R/commandLineInterface.R:109``): the passwd name of the real uid, never ``$USER``.
* ``file.exists("/p")`` (``R/getRunStatus.R:27``, ``R/loopRuns.R:44``, ``R/printOutput.R:28``): the
  cluster probe.
* ``dir(path)`` (``R/getRunStatus.R:41``, ``R/colRunType.R:14``, ``R/loopRuns.R:58``,
  ``R/getSanityChecks.R:25``): files and directories, dot-names skipped, sorted the way base R sorts
  character vectors, which is the ICU root collation even under ``LC_ALL=C.utf8`` (R 4.6.1, probed
  2026-10-01 and pinned in ``tests/unit/test_env.py``: ``_x ~t 1 10 2 ä "a b" a_b a-b A-B a.b a1 A1 a10 a2
  ab b B Dir1 dir2 e E é Run1 x_1 x-1 x.1 Zed``); ``full.names = TRUE`` pastes ``path``, ``/`` and the
  name, so a trailing slash yields ``output//run``; ``list.dirs(recursive = FALSE)``
  (``R/commandLineInterface.R:198-200``) lists directories including dot-names, joined the same way;
  ``Sys.glob`` (``R/getRunStatus.R:114, 179``) is glob(3): byte order, dot-names not matched by wildcards.
* ``file.info()`` (``R/getRunStatus.R:30, 61, 114, 126, 181-183``, ``R/modeltests.R:382, 405``): size,
  isdir, mtime and ctime, a missing path giving ``NA``.
* ``normalizePath()`` (``R/getRunStatus.R:25``, ``R/foundInSlurm.R:13``): the real absolute path of an
  existing file, the input unchanged otherwise.
* ``system(cmd, intern = TRUE)`` and ``system(cmd)`` (``R/foundInSlurm.R:19``,
  ``R/commandLineInterface.R:141-147``, ``R/modeltests.R:98-146, 182-201, 310, 355, 435-443``): stdout
  as lines, the exit status kept, no exception; the AMT streams the lines into its cycle log instead of
  letting the child print (``06`` 3.3).
* ``writeLines()`` / ``saveRDS()`` of the AMT state (``R/modeltests.R:42, 51, 138, 164, 172, 232, 489``):
  written atomically here (temp file and ``os.replace`` in the same directory, ``06`` 3.2); ``sed -i``
  on ``config/default.cfg`` (``:100, 117``) is ``replace_in_file``; ``unlink(recursive = TRUE)`` (``:83``)
  is ``delete_tree``; ``file.copy()`` (``:263, 437``) is ``copy``; ``file.rename()`` (``:389``) is
  ``rename``; ``mv ... archive`` (``:417``) is ``move`` (``06`` 4.2); ``Sys.sleep()`` (``:153, 193``) is
  ``sleep``; the ``curl`` webhook call (``:58-61``) is ``post_json`` (``06`` 4.2, decision D-15).
* ``Sys.time()`` / ``Sys.Date()`` (``R/getRunStatus.R:181, 183, 313``, ``R/commandLineInterface.R:145``,
  ``R/modeltests.R:34, 93, 176``): an aware datetime in the session's time zone (``TZ``).
* ``Sys.getenv("MAGPIE_RESULTS_ARCHIVE_PATH")`` (``R/getRunStatus.R:109``) and the hard-coded cluster
  paths (``R/commandLineInterface.R:127-133``, ``R/getSanityChecks.R:22-24``, ``R/getRunStatus.R:111``,
  ``R/modeltests.R:456``) in ``ClusterPaths``.

``FakeEffects`` reproduces the golden harness (``migration/harness/README.md``): a logical fixture root
(``/p`` on the cluster, ``/fixtures/p`` in local mode, ``Mode``) is mapped onto physical layers with the
overlay semantics of ``migration/harness/sandbox.sh``: the canonical fixtures over the synthetic layer, so
the fixtures win where both have a path, directory listings are the union, and displayed paths keep the
logical form. ``squeue`` and ``sacct`` answer from a recorded capture or a synthetic case directory with
the selection rules of ``migration/harness/fakebin/_slurm.py`` (``CannedSlurm``). The clock, the user and
the ``/p`` probe are injected; every call is logged. The in-process goldens of the ``rs`` families run on
``FakeEffects``; the end-to-end ``rs`` goldens additionally run the console script inside
``migration/harness/sandbox.sh`` (phase 4), where ``ProductionEffects`` meets the same overlay, fake
binaries and stopped clock as the R oracle.

Path-valued results are ``str``: they are the R character values (``dir(full.names = TRUE)`` joins with
``/`` even after a trailing slash, giving ``output//run``) that later phases may print verbatim. Inputs
accept ``str`` or any ``os.PathLike``.
"""

from __future__ import annotations

import contextlib
import fcntl
import glob as globmodule
import http.client
import json
import os
import pwd
import re
import shlex
import shutil
import stat as statmodule
import subprocess
import sys
import tempfile
import threading
import time
import unicodedata
import urllib.error
import urllib.parse
import urllib.request
from collections.abc import Callable, Iterable, Iterator, Mapping, Sequence
from contextlib import AbstractContextManager
from dataclasses import dataclass
from datetime import datetime, timedelta, tzinfo
from enum import Enum
from pathlib import Path, PurePosixPath
from typing import BinaryIO, Protocol
from zoneinfo import ZoneInfo, ZoneInfoNotFoundError

from modelstats.errors import ModelstatsError

type PathLike = str | os.PathLike[str]

# ---------------------------------------------------------------------------------------------
# Values crossing the boundary
# ---------------------------------------------------------------------------------------------


@dataclass(frozen=True)
class FileInfo:
    """One row of ``file.info()``: size, isdir, mtime and ctime (``R/getRunStatus.R:30, 61, 114, 126``).

    ``mtime_epoch`` and ``ctime_epoch`` are the stat seconds as R stores a POSIXct (a double carrying the
    nanosecond fraction), for the arithmetic against the epoch values of ``runstatistics.rda``; ``mtime``
    and ``ctime`` are the same instants as aware datetimes in the session's time zone (microsecond
    resolution), as R prints them. ``ctime`` is what the MAgPIE AMT selected runs by
    (``R/modeltests.R:247``) and what ``file.info()$ctime`` reports for the archive rules.
    """

    size: int
    is_dir: bool
    mtime_epoch: float
    ctime_epoch: float
    mtime: datetime
    ctime: datetime


class Stream(Enum):
    """Which pipe of a subprocess a streamed line came from (``Effects.run``'s ``on_line``)."""

    STDOUT = "stdout"
    STDERR = "stderr"


type LineCallback = Callable[[Stream, str], None]
"""Receives every complete line of a subprocess as it arrives, without its trailing newline.

The AMT's cycle log writes each line with a ``[<tool>]`` prefix and mirrors it to stderr
(``migration/06-user-requests.md`` section 3.3).
"""


@dataclass(frozen=True)
class RunResult:
    """What ``system(cmd, intern = TRUE)`` observes: the exit status and both streams."""

    argv: tuple[str, ...]
    returncode: int
    stdout: str
    stderr: str

    @property
    def ok(self) -> bool:
        """True for exit status 0."""
        return self.returncode == 0

    def lines(self) -> list[str]:
        r"""The stdout lines as ``intern = TRUE`` returns them (``src/unix/sys-unix.c``, ``getline``).

        Split on ``\n`` only (``\r`` and the other ``str.splitlines`` breaks stay inside a line, as R keeps
        them: ``printf 'a\rb\n'`` is one element), an unterminated last line is one element, each element
        is cut at its first NUL (BUG-011) and empty output is ``character(0)``. The byte-level twin is
        :func:`modelstats.textscan.intern_lines`.
        """
        return [line.split("\0", 1)[0] for line in _split_lines(self.stdout)]


def _split_lines(text: str) -> list[str]:
    r"""Split on ``\n`` only, an unterminated last line kept, nothing for empty text."""
    if not text:
        return []
    lines = text.split("\n")
    if text.endswith("\n"):
        lines.pop()
    return lines


class CommandFailedError(RuntimeError):
    """A subprocess run with ``check=True`` exited with a non-zero status."""

    def __init__(self, result: RunResult) -> None:
        """Carry the failed ``RunResult`` in ``result``."""
        super().__init__(f"{shlex.join(result.argv)} exited with status {result.returncode}")
        self.result = result


@dataclass(frozen=True)
class HttpResponse:
    """What ``post_json`` observes: the HTTP status and the response body.

    ``status`` is ``0`` when no HTTP exchange happened (connection refused, name resolution, timeout) and
    ``body`` then carries the reason, the way ``RunResult`` reports a missing program as status 127
    instead of raising: the R version's ``system(curl, intern = TRUE)`` (``R/modeltests.R:61``) warned
    and went on, and the notification layer decides what a failure means.
    """

    url: str
    status: int
    body: str

    @property
    def ok(self) -> bool:
        """True for a 2xx status."""
        return 200 <= self.status < 300


class LockHeldError(ModelstatsError, BlockingIOError):
    """``lock(path, wait=False)`` found the lock held by another process (``06`` 3.2, ``--no-wait``).

    It is the package's error (a ``ModelstatsError`` with a message naming the lock file) and at the same
    time the ``BlockingIOError`` that ``fcntl.flock(LOCK_NB)`` raises, so a caller may catch either.
    """

    def __init__(self, path: str) -> None:
        """Name the lock file in the message."""
        super().__init__(f"lock held by another process: {path}")
        self.path = path


@dataclass(frozen=True)
class ClusterPaths:
    """The cluster locations hard-coded in the R sources, verbatim (logical ``/p`` paths in both modes).

    ``R/commandLineInterface.R:127-133`` and ``R/getSanityChecks.R:22-24`` (the AMT output directory with
    its trailing slash, the archive, ``runcode.rds``), ``R/getRunStatus.R:111`` (the REMIND results
    archive, trailing slash included because the R code pastes the file name onto it) and
    ``R/modeltests.R:456`` (the AMT checkout). The MAgPIE archive is not a constant: it is
    ``Sys.getenv("MAGPIE_RESULTS_ARCHIVE_PATH")`` (``R/getRunStatus.R:109``), read through
    ``Effects.getenv``.
    """

    amt_dir: str = "/p/projects/remind/modeltests/remind"
    amt_output: str = "/p/projects/remind/modeltests/remind/output/"
    amt_archive: str = "/p/projects/remind/modeltests/remind/output/archive"
    amt_runcode: str = "/p/projects/remind/modeltests/remind/runcode.rds"
    remind_results_archive: str = "/p/projects/rd3mod/models/results/remind/"


MAGPIE_RESULTS_ARCHIVE_VARIABLE = "MAGPIE_RESULTS_ARCHIVE_PATH"
"""The environment variable naming the MAgPIE results archive (``R/getRunStatus.R:109``)."""

TRACE_ENV_KEYS: tuple[str, ...] = (
    "RS_AMT_CASE",
    "RS_SLURM_CASE",
    "USER",
    "autoRenvFixDeps",
    "FAKETIME",
    "TZ",
    "LC_ALL",
)
"""The variables a fake binary records with each call (``harness/fakebin/_trace.py``, ``ENV_KEYS``)."""


@dataclass(frozen=True)
class TraceRecord:
    """One subprocess call in the schema of the fake binaries' ``trace.jsonl`` (``_trace.py``).

    ``tool`` is the program's basename, ``argv`` its arguments without the program, ``cwd`` the absolute
    working directory, ``env`` the ``TRACE_ENV_KEYS`` subset of the child's environment and ``exit`` the
    status, present for ``squeue``/``sacct`` only (``_slurm.py`` adds it, the shell fakes do not).
    """

    tool: str
    argv: tuple[str, ...]
    cwd: str
    env: Mapping[str, str]
    exit: int | None = None

    def to_json(self) -> dict[str, object]:
        """The record as the fake writes it (``exit`` only when set)."""
        record: dict[str, object] = {
            "tool": self.tool,
            "argv": list(self.argv),
            "cwd": self.cwd,
            "env": dict(self.env),
        }
        if self.exit is not None:
            record["exit"] = self.exit
        return record

    def json_line(self) -> str:
        """The ``trace.jsonl`` line: ``json.dumps(ensure_ascii=False, sort_keys=True)``."""
        return json.dumps(self.to_json(), ensure_ascii=False, sort_keys=True)


@dataclass(frozen=True)
class PostRecord:
    """One ``post_json`` call in the schema of the fake ``curl``'s ``mattermost.jsonl``.

    The fake records ``url``, the raw ``-d`` payload and its parse (``payload_json``, or
    ``payload_json_error`` when the R-built string is not JSON, BUG-015); the Python payload is built with
    ``json.dumps`` (``06`` 4.2), so it always parses.
    """

    url: str
    payload: object

    @property
    def raw(self) -> str:
        """The payload as sent: ``json.dumps(payload, ensure_ascii=False)``."""
        return json.dumps(self.payload, ensure_ascii=False)

    def to_json(self) -> dict[str, object]:
        """The record as the fake writes it."""
        return {"url": self.url, "payload": self.raw, "payload_json": self.payload}

    def json_line(self) -> str:
        """The ``mattermost.jsonl`` line."""
        return json.dumps(self.to_json(), ensure_ascii=False, sort_keys=True)


# ---------------------------------------------------------------------------------------------
# The protocol
# ---------------------------------------------------------------------------------------------


class Effects(Protocol):
    """The whole environment (``03`` sections 2.3 and 5, "Effects").

    ``rs`` uses the read-only part plus ``run`` for ``squeue``/``sacct``; the AMT uses all of it.
    Implementations: ``ProductionEffects``, ``FakeEffects``, ``RecordingEffects``, ``DryRunEffects``.
    """

    @property
    def user(self) -> str:
        """``Sys.info()[["user"]]``: the passwd name of the real uid."""
        ...

    @property
    def on_cluster(self) -> bool:
        """``file.exists("/p")``."""
        ...

    @property
    def cluster_paths(self) -> ClusterPaths:
        """The hard-coded cluster locations."""
        ...

    @property
    def cwd(self) -> str:
        """``getwd()``: the directory relative paths are resolved against."""
        ...

    def getenv(self, name: str) -> str:
        """``Sys.getenv(name)``: the value, or ``""`` when unset."""
        ...

    @property
    def magpie_results_archive(self) -> str:
        """``Sys.getenv("MAGPIE_RESULTS_ARCHIVE_PATH")`` (``R/getRunStatus.R:109``): ``""`` when unset."""
        ...

    def now(self) -> datetime:
        """``Sys.time()``: an aware datetime in the session's time zone."""
        ...

    # -- reading ---------------------------------------------------------------------------------

    def exists(self, path: PathLike) -> bool:
        """``file.exists(path)`` (symlinks followed)."""
        ...

    def is_dir(self, path: PathLike) -> bool:
        """``dir.exists(path)`` / ``file.info(path)$isdir`` for an existing path."""
        ...

    def is_symlink(self, path: PathLike) -> bool:
        """``os.path.islink``: true for a symbolic link itself, never followed (``find -P`` semantics)."""
        ...

    def stat(self, path: PathLike) -> FileInfo | None:
        """``file.info(path)`` with mtime and ctime; ``None`` where R reports an ``NA`` row (missing path)."""
        ...

    def listdir_like_r(
        self, path: PathLike, *, pattern: str | None = None, full_names: bool = False
    ) -> list[str]:
        """``dir(path, pattern, full.names)``: sorted like base R, dot-names skipped, directories included.

        A missing path or a file gives ``character(0)``. ``pattern`` is an unanchored regular expression
        matched against the names; ``full_names`` joins ``path`` and name with ``/`` exactly as R does.
        """
        ...

    def list_dirs_like_r(self, path: PathLike, *, full_names: bool = True) -> list[str]:
        """``list.dirs(path, recursive = FALSE, full.names)``: the subdirectories, dot-names included."""
        ...

    def glob(self, pattern: PathLike) -> list[str]:
        """Expand like ``Sys.glob``: the C library glob, byte order, dot-names not matched by wildcards."""
        ...

    def normalize_path(self, path: PathLike) -> str:
        """``normalizePath(path)``: the real absolute path when it exists, the input unchanged otherwise."""
        ...

    def read_text(self, path: PathLike, *, encoding: str = "utf-8", errors: str = "surrogateescape") -> str:
        """The whole file as text (``surrogateescape`` keeps undecodable bytes round-trippable)."""
        ...

    def read_bytes(self, path: PathLike) -> bytes:
        """The whole file as bytes."""
        ...

    def open_binary(self, path: PathLike) -> BinaryIO:
        """A seekable binary stream for the bounded-memory scanners of ``textscan``."""
        ...

    # -- writing (``06`` 3.2, 4.1, 4.2) ---------------------------------------------------------

    def write_text(
        self, path: PathLike, text: str, *, encoding: str = "utf-8", errors: str = "surrogateescape"
    ) -> None:
        """Write the whole file atomically: a temp file in the same directory, then ``os.replace``.

        A crash before the rename leaves the old file untouched and no temp file behind; the mode of an
        existing file is kept. Text is written as is (no newline translation).
        """
        ...

    def write_bytes(self, path: PathLike, data: bytes) -> None:
        """``write_text`` for bytes (``runcode.rds`` and other serialised files)."""
        ...

    def replace_in_file(self, path: PathLike, old: str, new: str) -> int:
        """``sed -i 's/old/new/' path`` (``R/modeltests.R:100, 117``): literal, first occurrence per line.

        The file is rewritten atomically even when nothing matches (``sed -i`` does the same, which is why
        the R golden shows ``default.cfg`` as touched). Returns the number of lines changed.
        """
        ...

    def copy(self, src: PathLike, dst: PathLike) -> None:
        """``file.copy(src, dst, overwrite = TRUE)`` (``R/modeltests.R:437``); ``dst`` may be a directory."""
        ...

    def move(self, src: PathLike, dst: PathLike) -> None:
        """``mv src dst`` (``R/modeltests.R:417``, ``06`` 4.2); ``dst`` may be a directory."""
        ...

    def rename(self, src: PathLike, dst: PathLike) -> None:
        """``file.rename(src, dst)`` (``R/modeltests.R:389``): an existing ``dst`` file is replaced."""
        ...

    def delete_tree(self, path: PathLike) -> None:
        """``unlink(path, recursive = TRUE)`` (``R/modeltests.R:83``): a file or a tree; missing is fine."""
        ...

    def mkdir(self, path: PathLike) -> None:
        """``dir.create(path, recursive = TRUE)``: parents created, an existing directory is fine."""
        ...

    # -- processes, network, time, locking ------------------------------------------------------

    def run(
        self,
        argv: Sequence[str],
        *,
        cwd: PathLike | None = None,
        env: Mapping[str, str] | None = None,
        check: bool = False,
        capture: bool = True,
        on_line: LineCallback | None = None,
    ) -> RunResult:
        r"""Run a program and observe it the way ``system()`` does; ``check`` raises on failure.

        ``env`` overrides single variables on top of the inherited environment. With ``capture`` (the
        default) both streams are read through pipes, every complete line (split on ``\n`` only) goes to
        ``on_line`` as it arrives, and the whole text lands in the result; without ``capture`` the child
        inherits the parent's stdout and stderr (R's ``system(cmd)`` without ``intern``), the result's
        streams are empty and ``on_line`` must be ``None``. A missing program answers with status 127 and a
        ``command not found`` line on stderr, as ``sh`` does for ``system()``; a missing ``cwd`` raises.
        """
        ...

    def post_json(self, url: str, payload: object) -> HttpResponse:
        """POST ``payload`` as ``application/json`` (``R/modeltests.R:58-61`` through ``json.dumps``, D-15).

        Never raises for a failed exchange: an HTTP error status is returned as such, a transport failure
        as status 0 with the reason in the body.
        """
        ...

    def sleep(self, seconds: float) -> None:
        """``Sys.sleep(seconds)`` (``R/modeltests.R:193``, the 600 s between ``squeue`` polls)."""
        ...

    def lock(self, path: PathLike, *, wait: bool = True) -> AbstractContextManager[None]:
        """An exclusive ``fcntl.flock`` on ``path`` for the ``with`` block (``06`` 3.2).

        The file is created when missing and never deleted (deleting it would let a later invocation lock
        a different inode). ``wait=False`` raises ``LockHeldError`` at once when another process holds it.
        """
        ...


# ---------------------------------------------------------------------------------------------
# Base R collation of character vectors (dir(), list.dirs(), sort())
# ---------------------------------------------------------------------------------------------

_R_PUNCTUATION_ORDER = " _-,;:!?.'\"()[]{}@*/\\&#%`^+<=>|~$"
"""``sort()`` of the printable ASCII characters in R 4.6.1 under ``LC_ALL=C.utf8`` (probe 2026-10-01).

The order is the CLDR root collation: whitespace, then punctuation and symbols in this sequence, then
digits, then letters (case-insensitive at the primary level, lowercase first at the tertiary level).
"""
_PRIMARY_DIGIT = 100
_PRIMARY_LETTER = 200
_PRIMARY_OTHER = 1_000_000


def _collation_weights(char: str) -> tuple[int, int, int]:
    """Primary, secondary and tertiary weight of one character under the root collation."""
    index = _R_PUNCTUATION_ORDER.find(char)
    if index >= 0:
        return (1 + index, 0, 0)
    if char.isascii() and char.isdigit():
        return (_PRIMARY_DIGIT + ord(char) - ord("0"), 0, 0)
    decomposed = unicodedata.normalize("NFD", char)
    base = decomposed[0]
    if base.isascii() and base.isalpha():
        accented = 1 if len(decomposed) > 1 else 0
        upper = 1 if base.isupper() else 0
        return (_PRIMARY_LETTER + ord(base.lower()) - ord("a"), accented, upper)
    if ord(char) < 32:
        return (ord(char), 0, 0)
    return (_PRIMARY_OTHER + ord(char), 0, 0)


def r_collation_key(text: str) -> tuple[tuple[int, ...], tuple[int, ...], tuple[int, ...]]:
    """Sort key reproducing base R's character collation (ICU root) for ASCII names.

    Levels are compared one after the other over the whole string: primary (letters case-insensitively,
    digits, punctuation in ``_R_PUNCTUATION_ORDER``), then secondary (accents), then tertiary (lowercase
    before uppercase), so ``a1 < A1 < a10 < a2 < b < B`` and ``x_1 < x-1 < x.1``. Non-ASCII letters with a
    decomposable accent (``ä``, ``é``) fall in with their base letter; anything else sorts after the
    letters by code point, which is an approximation outside the fixture repertoire.
    """
    weights = [_collation_weights(char) for char in text]
    return (
        tuple(w[0] for w in weights),
        tuple(w[1] for w in weights),
        tuple(w[2] for w in weights),
    )


def sort_like_r(names: Iterable[str]) -> list[str]:
    """``sort(names)`` as base R orders a character vector (see ``r_collation_key``)."""
    return sorted(names, key=r_collation_key)


def _is_hidden(name: str) -> bool:
    """``all.files = FALSE``: names starting with a dot are skipped by ``dir()``."""
    return name.startswith(".")


def _join_like_r(path: str, name: str) -> str:
    """``full.names = TRUE``: R pastes path, ``/`` and name without cleaning a trailing slash."""
    return f"{path}/{name}"


def _select_names(names: Iterable[str], pattern: str | None) -> list[str]:
    """The ``dir()`` filter: dot-names out, ``pattern`` as an unanchored regular expression, R order."""
    regex = re.compile(pattern) if pattern is not None else None
    kept = [n for n in names if not _is_hidden(n) and (regex is None or regex.search(n))]
    return sort_like_r(kept)


def _local_timezone(environ: Mapping[str, str]) -> tzinfo:
    """The session's time zone: ``TZ`` when it names a zone, else the system's local zone."""
    name = environ.get("TZ", "").lstrip(":")
    if name:
        try:
            return ZoneInfo(name)
        except ZoneInfoNotFoundError, ValueError:
            pass
    local = datetime.now().astimezone().tzinfo
    if local is None:  # pragma: no cover - astimezone() always attaches a zone
        raise RuntimeError("the local time zone could not be determined")
    return local


def _file_info(result: os.stat_result, zone: tzinfo) -> FileInfo:
    """``file.info()`` fields from a stat result, times in the session's zone."""
    return FileInfo(
        size=result.st_size,
        is_dir=statmodule.S_ISDIR(result.st_mode),
        mtime_epoch=result.st_mtime,
        ctime_epoch=result.st_ctime,
        mtime=datetime.fromtimestamp(result.st_mtime, tz=zone),
        ctime=datetime.fromtimestamp(result.st_ctime, tz=zone),
    )


def _stat_or_none(physical: PathLike, zone: tzinfo) -> FileInfo | None:
    """``file.info`` of a physical path, ``None`` for a missing one."""
    try:
        return _file_info(os.stat(physical), zone)
    except OSError:
        return None


def _listdir_or_none(physical: PathLike) -> list[str] | None:
    """The entries of a physical directory, ``None`` when it is missing or not a directory."""
    try:
        return os.listdir(physical)
    except OSError:
        return None


# ---------------------------------------------------------------------------------------------
# The real calls, shared by the implementations that touch the filesystem or a process
# ---------------------------------------------------------------------------------------------


def _not_found_result(argv: Sequence[str]) -> RunResult:
    """What ``sh -c`` reports for a program that is not on the PATH (status 127)."""
    return RunResult(tuple(argv), 127, "", f"{argv[0]}: command not found\n")


def _target_mode(path: str) -> int:
    """The permission bits a written file gets: those of the file it replaces, else ``0666 & ~umask``."""
    try:
        return statmodule.S_IMODE(os.stat(path).st_mode)
    except OSError:
        mask = os.umask(0)
        os.umask(mask)
        return 0o666 & ~mask


def _write_bytes_atomically(path: str, data: bytes) -> None:
    """Temp file next to ``path``, data flushed to disk, then ``os.replace`` (``06`` 3.2).

    Any failure before the rename removes the temp file and leaves the old file as it was.
    """
    directory = os.path.dirname(path) or "."
    descriptor, temp = tempfile.mkstemp(prefix=f".{os.path.basename(path)}.", suffix=".tmp", dir=directory)
    try:
        with os.fdopen(descriptor, "wb") as stream:
            stream.write(data)
            stream.flush()
            os.fsync(stream.fileno())
        os.chmod(temp, _target_mode(path))
        os.replace(temp, path)
    except BaseException:
        with contextlib.suppress(OSError):
            os.unlink(temp)
        raise


_LINE_PIECES = re.compile(r"(?<=\n)")
"""Splits text into its lines with the newline kept on each piece (``sed`` works line by line)."""


def _replace_in_file(path: str, old: str, new: str) -> int:
    """``sed -i 's/old/new/'``: the first occurrence per line, literal; the count of changed lines."""
    text = Path(path).read_bytes().decode("utf-8", "surrogateescape")
    changed = 0
    pieces: list[str] = []
    for piece in _LINE_PIECES.split(text):
        if old and old in piece:
            changed += 1
            piece = piece.replace(old, new, 1)
        pieces.append(piece)
    _write_bytes_atomically(path, "".join(pieces).encode("utf-8", "surrogateescape"))
    return changed


def _delete_tree(path: str) -> None:
    """``unlink(recursive = TRUE)``: a tree, a file or a symlink; nothing to do for a missing path."""
    if os.path.isdir(path) and not os.path.islink(path):
        shutil.rmtree(path)
        return
    with contextlib.suppress(FileNotFoundError):
        os.remove(path)


@contextlib.contextmanager
def _flock(path: str, wait: bool) -> Iterator[None]:
    """An exclusive ``flock`` held for the block; the descriptor's close releases it."""
    descriptor = os.open(path, os.O_RDWR | os.O_CREAT | os.O_CLOEXEC, 0o644)
    try:
        try:
            fcntl.flock(descriptor, fcntl.LOCK_EX | (0 if wait else fcntl.LOCK_NB))
        except BlockingIOError:
            raise LockHeldError(path) from None
        yield
    finally:
        os.close(descriptor)


def _pump(
    stream: BinaryIO,
    which: Stream,
    chunks: list[str],
    on_line: LineCallback | None,
    guard: threading.Lock,
    failures: list[BaseException],
) -> None:
    r"""Read one pipe to its end, line by line (``\n`` only), collecting text and calling ``on_line``.

    A callback failure is kept for the caller; the pipe is still drained so the child never blocks.
    """
    try:
        for raw in stream:
            text = raw.decode("utf-8", "replace")
            chunks.append(text)
            if on_line is not None:
                with guard:
                    on_line(which, text.removesuffix("\n"))
    except BaseException as error:  # noqa: BLE001 - re-raised by the caller after the join
        failures.append(error)
        for _ in stream:
            pass


def _run_process(
    argv: Sequence[str],
    cwd: PathLike | None,
    env: Mapping[str, str],
    capture: bool,
    on_line: LineCallback | None,
) -> RunResult:
    """``subprocess.Popen`` with the semantics of ``Effects.run``."""
    pipe = subprocess.PIPE if capture else None
    try:
        process = subprocess.Popen(list(argv), cwd=cwd, env=env, stdout=pipe, stderr=pipe)
    except FileNotFoundError as error:
        if cwd is not None and os.fspath(error.filename) == os.fspath(cwd):
            raise
        return _not_found_result(argv)
    with process:
        if not capture:
            return RunResult(tuple(argv), process.wait(), "", "")
        assert process.stdout is not None and process.stderr is not None  # pipes were requested
        out: list[str] = []
        err: list[str] = []
        failures: list[BaseException] = []
        guard = threading.Lock()
        readers = [
            threading.Thread(target=_pump, args=(stream, which, chunks, on_line, guard, failures))
            for stream, which, chunks in (
                (process.stdout, Stream.STDOUT, out),
                (process.stderr, Stream.STDERR, err),
            )
        ]
        for reader in readers:
            reader.start()
        for reader in readers:
            reader.join()
        returncode = process.wait()
    if failures:
        raise failures[0]
    return RunResult(tuple(argv), returncode, "".join(out), "".join(err))


def _deliver(result: RunResult, capture: bool, on_line: LineCallback | None) -> RunResult:
    """Hand a canned or synthetic result over the way a real child would have produced it.

    With ``capture`` the lines go to ``on_line`` and the text stays in the result; without it the text is
    printed on the process's own streams (what an inheriting child would have done) and the result's
    streams are empty.
    """
    if not capture:
        sys.stdout.write(result.stdout)
        sys.stderr.write(result.stderr)
        return RunResult(result.argv, result.returncode, "", "")
    if on_line is not None:
        for line in _split_lines(result.stdout):
            on_line(Stream.STDOUT, line)
        for line in _split_lines(result.stderr):
            on_line(Stream.STDERR, line)
    return result


def _check_run_arguments(argv: Sequence[str], capture: bool, on_line: LineCallback | None) -> None:
    """The argument rules every ``run`` shares."""
    if not argv:
        raise ValueError("run() needs a program")
    if on_line is not None and not capture:
        raise ValueError("on_line needs capture=True (an inheriting child cannot be streamed)")


_WEBHOOK_SCHEMES = frozenset({"http", "https"})
NOT_A_WEBHOOK_URL = "webhook URL is not an http(s) URL"
"""The body of the status-0 answer for a URL that is not ``http://`` or ``https://``."""
INVALID_WEBHOOK_URL = "webhook URL rejected before any exchange"
"""The body of the status-0 answer for a URL that urllib or http.client refuse to use (``no host``, a
space or control character, an unbalanced IPv6 bracket, ...). Neither body echoes the URL or the
library's message, which quotes it: the URL carries the webhook secret."""


def _post_json(url: str, payload: object, timeout: float) -> HttpResponse:
    """``urllib`` POST of ``json.dumps(payload)`` with ``Content-Type: application/json``.

    Never raises for a bad ``url``: a scheme other than ``http`` or ``https`` (which keeps urllib's
    ``file:`` and ``data:`` handlers out) answers :data:`NOT_A_WEBHOOK_URL`, and every ``ValueError`` or
    ``http.client.InvalidURL`` of the libraries' own validation (``unknown url type``, ``Invalid IPv6
    URL``, control characters in the host or path) answers :data:`INVALID_WEBHOOK_URL`, both with status
    ``0`` as a transport failure does. R's ``system("curl ...")`` only warned about a failing call
    (``R/modeltests.R:61``), and a misconfigured webhook must not fail a finished evaluation.
    """
    data = json.dumps(payload, ensure_ascii=False).encode("utf-8")
    try:
        if urllib.parse.urlsplit(url).scheme not in _WEBHOOK_SCHEMES:
            return HttpResponse(url, 0, NOT_A_WEBHOOK_URL)
        request = urllib.request.Request(
            url, data=data, headers={"Content-Type": "application/json"}, method="POST"
        )
        with urllib.request.urlopen(request, timeout=timeout) as response:
            return HttpResponse(url, int(response.status), response.read().decode("utf-8", "replace"))
    except urllib.error.HTTPError as error:
        return HttpResponse(url, error.code, error.read().decode("utf-8", "replace"))
    except urllib.error.URLError as error:
        return HttpResponse(url, 0, str(error.reason))
    except (http.client.InvalidURL, ValueError) as error:
        return HttpResponse(url, 0, f"{INVALID_WEBHOOK_URL} ({type(error).__name__})")
    except (OSError, http.client.HTTPException) as error:
        return HttpResponse(url, 0, str(error))


def _trace_env(environ: Mapping[str, str]) -> dict[str, str]:
    """The ``TRACE_ENV_KEYS`` subset of a child's environment, in the fake's key order."""
    return {key: environ[key] for key in TRACE_ENV_KEYS if key in environ}


# ---------------------------------------------------------------------------------------------
# Production
# ---------------------------------------------------------------------------------------------


class ProductionEffects:
    """The real environment: the process's filesystem, PATH, clock, uid, variables and network."""

    def __init__(self, environ: Mapping[str, str] | None = None, *, http_timeout: float = 60.0) -> None:
        """Bind to ``os.environ`` or to an explicit variable mapping (used for the time zone and ``run``).

        ``http_timeout`` bounds a ``post_json`` exchange in seconds.
        """
        self._environ: Mapping[str, str] = os.environ if environ is None else environ
        self._zone = _local_timezone(self._environ)
        self._cluster_paths = ClusterPaths()
        self._http_timeout = http_timeout

    @property
    def user(self) -> str:
        """``Sys.info()[["user"]]``: getpwuid of the real uid, ``unknown`` when there is no entry."""
        try:
            return pwd.getpwuid(os.getuid()).pw_name
        except KeyError:
            return "unknown"

    @property
    def on_cluster(self) -> bool:
        """``file.exists("/p")``."""
        return os.path.exists("/p")

    @property
    def cluster_paths(self) -> ClusterPaths:
        """The hard-coded cluster locations."""
        return self._cluster_paths

    @property
    def cwd(self) -> str:
        """``getwd()``."""
        return os.getcwd()

    def getenv(self, name: str) -> str:
        """``Sys.getenv(name)``."""
        return self._environ.get(name, "")

    @property
    def magpie_results_archive(self) -> str:
        """The MAgPIE results archive named by ``MAGPIE_RESULTS_ARCHIVE_PATH``."""
        return self.getenv(MAGPIE_RESULTS_ARCHIVE_VARIABLE)

    def now(self) -> datetime:
        """``Sys.time()`` in the session's zone (libfaketime applies inside the sandbox)."""
        return datetime.now(tz=self._zone)

    # -- reading ---------------------------------------------------------------------------------

    def exists(self, path: PathLike) -> bool:
        """``file.exists(path)``."""
        return os.path.exists(path)

    def is_dir(self, path: PathLike) -> bool:
        """``dir.exists(path)``."""
        return os.path.isdir(path)

    def is_symlink(self, path: PathLike) -> bool:
        """``os.path.islink(path)``: the link itself, not followed."""
        return os.path.islink(path)

    def stat(self, path: PathLike) -> FileInfo | None:
        """``file.info(path)``, ``None`` for a missing path."""
        return _stat_or_none(path, self._zone)

    def listdir_like_r(
        self, path: PathLike, *, pattern: str | None = None, full_names: bool = False
    ) -> list[str]:
        """``dir(path, pattern, full.names)``."""
        names = _listdir_or_none(path)
        if names is None:
            return []
        selected = _select_names(names, pattern)
        return [_join_like_r(os.fspath(path), n) for n in selected] if full_names else selected

    def list_dirs_like_r(self, path: PathLike, *, full_names: bool = True) -> list[str]:
        """``list.dirs(path, recursive = FALSE, full.names)``."""
        names = _listdir_or_none(path)
        if names is None:
            return []
        dirs = sort_like_r(n for n in names if os.path.isdir(os.path.join(path, n)))
        return [_join_like_r(os.fspath(path), n) for n in dirs] if full_names else dirs

    def glob(self, pattern: PathLike) -> list[str]:
        """Expand like ``Sys.glob``: the C library glob, results in byte order."""
        return sorted(globmodule.glob(os.fspath(pattern)))

    def normalize_path(self, path: PathLike) -> str:
        """``normalizePath(path)``: realpath of an existing path, the input otherwise."""
        text = os.fspath(path)
        return os.path.realpath(text) if os.path.exists(text) else text

    def read_text(self, path: PathLike, *, encoding: str = "utf-8", errors: str = "surrogateescape") -> str:
        """The whole file as text."""
        return Path(path).read_text(encoding=encoding, errors=errors)

    def read_bytes(self, path: PathLike) -> bytes:
        """The whole file as bytes."""
        return Path(path).read_bytes()

    def open_binary(self, path: PathLike) -> BinaryIO:
        """A binary stream; the caller closes it."""
        return open(path, "rb")  # noqa: SIM115 - the stream is handed to the caller

    # -- writing ---------------------------------------------------------------------------------

    def write_text(
        self, path: PathLike, text: str, *, encoding: str = "utf-8", errors: str = "surrogateescape"
    ) -> None:
        """Atomic whole-file write; see ``Effects.write_text``."""
        _write_bytes_atomically(os.fspath(path), text.encode(encoding, errors))

    def write_bytes(self, path: PathLike, data: bytes) -> None:
        """Atomic whole-file write of bytes."""
        _write_bytes_atomically(os.fspath(path), data)

    def replace_in_file(self, path: PathLike, old: str, new: str) -> int:
        """``sed -i 's/old/new/'``; see ``Effects.replace_in_file``."""
        return _replace_in_file(os.fspath(path), old, new)

    def copy(self, src: PathLike, dst: PathLike) -> None:
        """``file.copy(overwrite = TRUE)``: content and mode, into ``dst`` when it is a directory."""
        shutil.copy(src, dst)

    def move(self, src: PathLike, dst: PathLike) -> None:
        """``mv src dst``: into ``dst`` when it is a directory, across filesystems if need be."""
        shutil.move(os.fspath(src), os.fspath(dst))

    def rename(self, src: PathLike, dst: PathLike) -> None:
        """``file.rename``: ``os.replace`` (an existing ``dst`` file is overwritten, as on POSIX)."""
        os.replace(src, dst)

    def delete_tree(self, path: PathLike) -> None:
        """``unlink(recursive = TRUE)``."""
        _delete_tree(os.fspath(path))

    def mkdir(self, path: PathLike) -> None:
        """``dir.create(recursive = TRUE)``."""
        os.makedirs(path, exist_ok=True)

    # -- processes, network, time, locking ------------------------------------------------------

    def run(
        self,
        argv: Sequence[str],
        *,
        cwd: PathLike | None = None,
        env: Mapping[str, str] | None = None,
        check: bool = False,
        capture: bool = True,
        on_line: LineCallback | None = None,
    ) -> RunResult:
        """``subprocess.Popen`` with both streams read line by line; see ``Effects.run``."""
        _check_run_arguments(argv, capture, on_line)
        merged = {**self._environ, **(env or {})}
        result = _run_process(argv, cwd, merged, capture, on_line)
        if check and not result.ok:
            raise CommandFailedError(result)
        return result

    def post_json(self, url: str, payload: object) -> HttpResponse:
        """``urllib`` POST; see ``Effects.post_json``."""
        return _post_json(url, payload, self._http_timeout)

    def sleep(self, seconds: float) -> None:
        """``time.sleep`` (the monotonic clock: it works under the sandbox's stopped clock)."""
        time.sleep(seconds)

    def lock(self, path: PathLike, *, wait: bool = True) -> AbstractContextManager[None]:
        """An exclusive ``flock`` on ``path``; see ``Effects.lock``."""
        return _flock(os.fspath(path), wait)


# ---------------------------------------------------------------------------------------------
# The test double: overlay paths, canned scheduler, injected clock and identity
# ---------------------------------------------------------------------------------------------


class Mode(Enum):
    """The two sandbox modes of ``migration/harness/sandbox.sh --mode``."""

    ONCLUSTER = "oncluster"
    LOCAL = "local"

    @property
    def fixture_root(self) -> PurePosixPath:
        """Where the fixture tree is mounted: ``/p`` on the cluster, ``/fixtures/p`` locally."""
        return PurePosixPath("/p") if self is Mode.ONCLUSTER else PurePosixPath("/fixtures/p")

    @property
    def on_cluster(self) -> bool:
        """``file.exists("/p")`` under this mode."""
        return self is Mode.ONCLUSTER


@dataclass(frozen=True)
class OverlayPaths:
    """A logical root mapped onto physical layers, the first layer winning where several have a path.

    Mirrors the overlayfs of ``sandbox.sh``: the canonical fixtures over the synthetic layer
    (``migration/harness/README.md``, "Fixture view": the fixtures win, the synthetic layer only adds
    paths). ``lookup`` walks a path component by component the way overlayfs does, so a file in a higher
    layer hides a same-named directory, and everything below it, in the layers underneath. Paths outside
    the logical root are passed through to the real filesystem unchanged, which keeps ``/p/...`` constants
    invisible in local mode, as on a machine without ``/p``.
    """

    logical_root: PurePosixPath
    layers: tuple[Path, ...]

    @classmethod
    def sandbox(cls, fixtures_p: Path, synthetic_p: Path | None, mode: Mode) -> OverlayPaths:
        """The sandbox's view: ``fixtures/p`` over ``synthetic/p`` (when built) at the mode's root."""
        layers = (
            (fixtures_p,) if synthetic_p is None or not synthetic_p.is_dir() else (fixtures_p, synthetic_p)
        )
        return cls(mode.fixture_root, layers)

    def candidates(self, logical: PurePosixPath) -> tuple[Path, ...]:
        """The same relative path in every layer, in priority order (one entry when outside the root).

        This is the raw correspondence, not the overlay's answer: ``lookup`` applies the shadowing rules.
        """
        if not logical.is_relative_to(self.logical_root):
            return (Path(logical),)
        relative = logical.relative_to(self.logical_root)
        return tuple(layer / relative for layer in self.layers)

    def lookup(self, logical: PurePosixPath) -> tuple[Path | None, tuple[Path, ...]]:
        """The overlay's answer for ``logical``: the physical entry shown, and the directories merged into it.

        Component by component, the highest layer holding the name decides. A directory is merged with the
        same-named directories of the layers below it, down to the first layer in which the name is not a
        directory (overlayfs stops merging there, so what those layers hold underneath stays hidden); a
        non-directory ends the walk, hiding everything below. A ``..`` component returns to the merged
        parent: the kernel resolves ``..`` to the parent dentry of the overlay, with every layer merged
        there again, so ``a/b/../c`` shows the same ``c`` as ``a/c`` once ``b`` exists, while a missing
        ``b`` stays ``ENOENT`` and a file ``b`` stays ``ENOTDIR``; a ``..`` at the overlay root climbs out
        onto the real filesystem (``/p/..`` is the host's ``/``). ``(None, ())`` when nothing is shown,
        ``(entry, ())`` for a file, ``(entry, merged directories)`` for a directory. Outside the logical
        root the real filesystem answers.
        """
        if not logical.is_relative_to(self.logical_root):
            physical = Path(logical)
            if not os.path.exists(physical):
                return (None, ())
            return (physical, (physical,) if os.path.isdir(physical) else ())
        parts = logical.relative_to(self.logical_root).parts
        root: tuple[Path | None, tuple[Path, ...]] = (self.layers[0] if self.layers else None, self.layers)
        states = [root]
        for index, part in enumerate(parts):
            if part == "..":
                if len(states) == 1:
                    return self.lookup(self.logical_root.parent.joinpath(*parts[index + 1 :]))
                states.pop()
                continue
            holders = [directory / part for directory in states[-1][1] if os.path.lexists(directory / part)]
            if not holders:
                return (None, ())
            shown = holders[0]
            if not os.path.isdir(shown):
                return (shown, ()) if index == len(parts) - 1 else (None, ())
            states.append((shown, _merged_run(holders)))
        return states[-1]

    def first_existing(self, logical: PurePosixPath) -> Path | None:
        """The physical path that the overlay shows for ``logical``, or ``None``."""
        return self.lookup(logical)[0]

    def merged_names(self, logical: PurePosixPath) -> set[str] | None:
        """The union of the entries of the directories merged at ``logical``.

        ``None`` when the overlay shows nothing; an empty set for a file (``dir()`` on a file is empty).
        """
        shown, merged = self.lookup(logical)
        if shown is None:
            return None
        names: set[str] = set()
        for directory in merged:
            names |= set(os.listdir(directory))
        return names

    def to_logical(self, physical: Path) -> str | None:
        """The logical form of a physical path inside one of the layers, or ``None``."""
        for layer in self.layers:
            if physical.is_relative_to(layer):
                return str(self.logical_root / physical.relative_to(layer))
        return None


def _merged_run(holders: Sequence[Path]) -> tuple[Path, ...]:
    """The directories overlayfs merges: the leading run of ``holders`` that are directories."""
    run: list[Path] = []
    for candidate in holders:
        if not os.path.isdir(candidate):
            break
        run.append(candidate)
    return tuple(run)


@dataclass(frozen=True)
class EffectCall:
    """One logged call: the method, its stringified arguments and, for ``run``/``post_json``, the status."""

    name: str
    args: tuple[str, ...]
    status: int | None = None

    @property
    def tool(self) -> str:
        """For a ``run`` call: the program's basename."""
        return os.path.basename(self.args[0]) if self.args else ""


_SQUEUE_SIX = "%u %Z %j %M %T %q"
_SQUEUE_TEN = "%i %q %T %C %M %j %V %L %e %Z"


@dataclass(frozen=True)
class _SlurmRequest:
    """The parts of a ``squeue``/``sacct`` argument list the fake reads (``_slurm.py`` ``parse_args``)."""

    user: str | None
    fmt: str | None


def _parse_slurm_args(argv: Sequence[str]) -> _SlurmRequest:
    """``-u U`` / ``--user=U`` and ``-o F`` / ``-oF`` / ``--format F`` / ``--format=F``."""
    user: str | None = None
    fmt: str | None = None
    i = 0
    while i < len(argv):
        arg = argv[i]
        if arg == "-u" and i + 1 < len(argv):
            user = argv[i + 1]
            i += 2
        elif arg.startswith("--user="):
            user = arg.split("=", 1)[1]
            i += 1
        elif arg == "-o" and i + 1 < len(argv):
            fmt = argv[i + 1]
            i += 2
        elif arg.startswith("--format=") or arg.startswith("-o"):
            fmt = arg.split("=", 1)[1] if "=" in arg else arg[2:]
            i += 1
        elif arg == "--format" and i + 1 < len(argv):
            fmt = argv[i + 1]
            i += 2
        else:
            i += 1
    return _SlurmRequest(user, fmt)


@dataclass(frozen=True)
class _SixRow:
    """A line of the six-field dump ``%u %Z %j %M %T %q`` (the job name may contain spaces)."""

    user: str
    workdir: str
    jobname: str
    elapsed: str
    state: str
    qos: str

    def line(self) -> str:
        """The line as the fake prints it (single spaces)."""
        return f"{self.user} {self.workdir} {self.jobname} {self.elapsed} {self.state} {self.qos}"


@dataclass(frozen=True)
class _TenRow:
    """A line of the ten-field dump ``%i %q %T %C %M %j %V %L %e %Z`` with its original text."""

    jobname: str
    workdir: str
    line: str


def _six_rows(lines: Iterable[str]) -> list[_SixRow]:
    """Parse the six-field dump; lines with fewer than six fields are skipped."""
    rows: list[_SixRow] = []
    for line in lines:
        fields = line.split(" ")
        if len(fields) < 6:
            continue
        rows.append(_SixRow(fields[0], fields[1], " ".join(fields[2:-3]), fields[-3], fields[-2], fields[-1]))
    return rows


def _ten_rows(lines: Iterable[str]) -> list[_TenRow]:
    """Parse the ten-field dump; lines with fewer than ten fields are skipped."""
    rows: list[_TenRow] = []
    for line in lines:
        fields = line.split(" ")
        if len(fields) < 10:
            continue
        rows.append(_TenRow(" ".join(fields[5:-4]), fields[-1], line))
    return rows


def _printed(lines: Iterable[str]) -> str:
    """What ``print()`` per line produces: every line newline-terminated, nothing for no lines."""
    return "".join(f"{line}\n" for line in lines)


def _case_files(case_dir: Path) -> dict[str, str]:
    """Every regular file of a case directory as ``name -> text`` (UTF-8, undecodable bytes replaced)."""
    if not case_dir.is_dir():
        raise FileNotFoundError(f"case directory not found: {case_dir}")
    return {
        entry.name: entry.read_text(encoding="utf-8", errors="replace")
        for entry in sorted(case_dir.iterdir())
        if entry.is_file()
    }


class CannedSlurm:
    """``squeue`` and ``sacct`` answers from a case directory (port of ``harness/fakebin/_slurm.py``).

    The directory is either the recorded capture (``fixtures/_meta/slurm/latest``: ``NN.txt`` with the
    producing command in ``NN.txt.cmd``) or a synthetic case (``migration/cases/slurm/<name>``:
    ``squeue_all.txt`` in the six-field format, optional per-user overrides ``squeue_Z_<user>.txt``,
    ``squeue_j_<user>.txt``, ``squeue_ten_<user>.txt``, ``squeue_six_<user>.txt``,
    ``sacct_<field>_<user>.txt``, and the exit controls ``squeue_exit``, ``sacct_exit``,
    ``squeue_sequence``, ``sacct_sequence``). A request is first matched against the recorded ``.cmd``
    lines (the ``sacct -S`` date wildcarded), then derived from the all-users dumps (``squeue_all.txt``
    or ``01.txt``; ``squeue_ten_all.txt`` or ``02.txt``). Without a case directory (``empty()``, the fake
    with ``RS_SLURM_CASE_DIR`` unset) every call succeeds with no output. The per-call counters of the
    ``_sequence`` files live on the instance.
    """

    def __init__(self, files: Mapping[str, str], *, present: bool = True) -> None:
        """Take the case directory's regular files as ``name -> text``; ``present=False`` is no directory."""
        self._files = dict(files)
        self._present = present
        self._calls: dict[str, int] = {}

    @classmethod
    def from_case_dir(cls, case_dir: Path) -> CannedSlurm:
        """Load every regular file of ``case_dir`` (UTF-8, undecodable bytes replaced, like the fake)."""
        return cls(_case_files(case_dir))

    @classmethod
    def empty(cls) -> CannedSlurm:
        """No case directory: every call succeeds with no output."""
        return cls({}, present=False)

    def answer(self, tool: str, argv: Sequence[str]) -> RunResult:
        """The fake's stdout, stderr and exit status for ``tool argv...``."""
        full_argv = (tool, *argv)
        status = self._exit_status(tool)
        if status != 0:
            return RunResult(full_argv, status, "", f"{tool}: error: fake {tool} failure (exit {status})\n")
        if not self._present:
            return RunResult(full_argv, 0, "", "")
        recorded = self._recorded_match(tool, argv)
        if recorded is not None:
            return RunResult(full_argv, 0, recorded, "")
        request = _parse_slurm_args(argv)
        if tool == "sacct":
            return RunResult(full_argv, 0, self._sacct(request), "")
        stdout = self._squeue(request)
        if stdout is None:
            return RunResult(
                full_argv, 1, "", f"fake squeue: unsupported format {request.fmt!r} (args {list(argv)})\n"
            )
        return RunResult(full_argv, 0, stdout, "")

    # -- selection rules ------------------------------------------------------------------------

    def _text(self, *names: str) -> str | None:
        """The content of the first of ``names`` that exists."""
        return next((self._files[n] for n in names if n in self._files), None)

    def _exit_status(self, tool: str) -> int:
        """``<tool>_sequence`` (one status per call, the last one repeated) or ``<tool>_exit``, else 0."""
        sequence = self._text(f"{tool}_sequence")
        if sequence is not None:
            statuses = [int(x) for x in sequence.splitlines() if x.strip()]
            call = self._calls.get(tool, 0)
            self._calls[tool] = call + 1
            if statuses:
                return statuses[min(call, len(statuses) - 1)]
        fixed = self._text(f"{tool}_exit")
        if fixed is not None:
            return int(fixed.strip() or 0)
        return 0

    def _recorded_match(self, tool: str, argv: Sequence[str]) -> str | None:
        """The ``NN.txt`` whose ``NN.txt.cmd`` first line equals the request (``sacct -S`` wildcarded)."""
        want = [tool, *argv]
        for name in sorted(self._files):
            if not name.endswith(".cmd"):
                continue
            first = self._files[name].splitlines()
            if not first:
                continue
            command = re.split(r" {3,}", first[0], maxsplit=1)[0].strip()
            try:
                have = shlex.split(command)
            except ValueError:
                continue
            if _same_request(tool, have, want) and name[:-4] in self._files:
                return self._files[name[:-4]]
        return None

    def _sacct(self, request: _SlurmRequest) -> str:
        """``sacct_<field>_<user>.txt`` for ``--format <field>``, empty otherwise."""
        field_name = (request.fmt or "").strip()
        return self._text(f"sacct_{field_name}_{request.user}.txt") or ""

    def _squeue(self, request: _SlurmRequest) -> str | None:
        """The six-field, ``%Z``, ``%j`` and ten-field answers; ``None`` for an unsupported format."""
        all_six = self._text("squeue_all.txt", "01.txt")
        six = _six_rows(all_six.splitlines()) if all_six is not None else []
        mine = [r for r in six if request.user is None or r.user == request.user]
        user = request.user
        if request.fmt == _SQUEUE_SIX:
            override = self._text(f"squeue_six_{user}.txt") if user is not None else None
            return override if override is not None else _printed(r.line() for r in mine)
        if request.fmt == "%Z":
            return self._override_or(f"squeue_Z_{user}.txt", (r.workdir for r in mine))
        if request.fmt == "%j":
            return self._override_or(f"squeue_j_{user}.txt", (r.jobname for r in mine))
        if request.fmt == _SQUEUE_TEN:
            override = self._text(f"squeue_ten_{user}.txt")
            if override is not None:
                return override
            return self._ten_field(mine, user)
        return None

    def _override_or(self, name: str, derived: Iterable[str]) -> str:
        """A per-user override file verbatim, else the derived lines printed one per line."""
        override = self._text(name)
        return override if override is not None else _printed(derived)

    def _ten_field(self, mine: Sequence[_SixRow], user: str | None) -> str:
        """The ten-field dump filtered to the user's (job name, work dir) pairs, or synthesised lines."""
        ten_all = self._text("squeue_ten_all.txt", "02.txt")
        if ten_all is not None:
            keys = {(r.jobname, r.workdir) for r in mine}
            rows = _ten_rows(ten_all.splitlines())
            return _printed(r.line for r in rows if user is None or (r.jobname, r.workdir) in keys)
        return _printed(
            f"{100000 + i} {r.qos} {r.state} 1 {r.elapsed} {r.jobname} 2026-09-30T00:00:00 N/A N/A"
            f" {r.workdir}"
            for i, r in enumerate(mine)
        )


def _same_request(tool: str, have: Sequence[str], want: Sequence[str]) -> bool:
    """Recorded command equals the request word for word, except the value after ``sacct -S``."""
    if len(have) != len(want) or have[0] != want[0]:
        return False
    for i, (h, w) in enumerate(zip(have, want, strict=True)):
        if h != w and not (tool == "sacct" and i > 0 and have[i - 1] == "-S"):
            return False
    return True


_CANNED_TOOLS = frozenset({"squeue", "sacct"})


class FakeEffects:
    """The test double: overlay paths, canned scheduler, frozen clock, injected user and ``/p`` probe.

    ``paths`` maps the logical fixture root onto physical layers (``OverlayPaths.sandbox``); without it
    every path is taken as is. ``slurm`` answers ``squeue`` and ``sacct``; any other program raises
    ``NotImplementedError`` so that a stray shell-out fails loudly (the AMT's programs are
    ``RecordingEffects``' business). ``cwd`` is a logical directory (the fixture root by default).
    ``environ`` backs ``getenv``. Every call is appended to ``calls``. Mutations are performed for real on
    paths outside the overlay root (temporary trees) and refused inside it, where the layers are the
    canonical, read-only snapshot; ``post_json`` answers ``200 ok`` without a network, ``sleep`` returns at
    once, both logged.
    """

    def __init__(
        self,
        *,
        now: datetime,
        user: str,
        on_cluster: bool,
        paths: OverlayPaths | None = None,
        slurm: CannedSlurm | None = None,
        cwd: PathLike | None = None,
        environ: Mapping[str, str] | None = None,
        cluster_paths: ClusterPaths | None = None,
    ) -> None:
        """Inject the frozen clock (aware), the user name, the cluster probe and the collaborators."""
        if now.tzinfo is None:
            raise ValueError("FakeEffects needs an aware datetime for now()")
        self._now = now
        self._user = user
        self._on_cluster = on_cluster
        self._paths = paths
        self._slurm = slurm if slurm is not None else CannedSlurm.empty()
        default_cwd = paths.logical_root if paths is not None else PurePosixPath(os.getcwd())
        self._cwd = PurePosixPath(os.fspath(cwd)) if cwd is not None else default_cwd
        self._environ: dict[str, str] = dict(environ or {})
        self._cluster_paths = cluster_paths if cluster_paths is not None else ClusterPaths()
        self.calls: list[EffectCall] = []

    # -- identity, clock, variables ------------------------------------------------------------

    @property
    def user(self) -> str:
        """The injected ``Sys.info()[["user"]]``."""
        self._log("user")
        return self._user

    @property
    def on_cluster(self) -> bool:
        """The injected ``file.exists("/p")``."""
        self._log("on_cluster")
        return self._on_cluster

    @property
    def cluster_paths(self) -> ClusterPaths:
        """The hard-coded cluster locations (logical paths)."""
        return self._cluster_paths

    @property
    def cwd(self) -> str:
        """The logical working directory."""
        return str(self._cwd)

    @property
    def paths(self) -> OverlayPaths | None:
        """The overlay mapping, if any."""
        return self._paths

    @property
    def runs(self) -> list[EffectCall]:
        """The logged ``run`` calls."""
        return [c for c in self.calls if c.name == "run"]

    def getenv(self, name: str) -> str:
        """``Sys.getenv(name)`` on the injected variables."""
        self._log("getenv", name)
        return self._environ.get(name, "")

    @property
    def magpie_results_archive(self) -> str:
        """The injected ``MAGPIE_RESULTS_ARCHIVE_PATH``."""
        return self.getenv(MAGPIE_RESULTS_ARCHIVE_VARIABLE)

    def now(self) -> datetime:
        """The frozen clock."""
        self._log("now")
        return self._now

    # -- filesystem ------------------------------------------------------------------------------

    def exists(self, path: PathLike) -> bool:
        """``file.exists`` through the overlay."""
        self._log("exists", path)
        return self._physical(path) is not None

    def is_dir(self, path: PathLike) -> bool:
        """``dir.exists`` through the overlay (the highest layer's entry decides)."""
        self._log("is_dir", path)
        physical = self._physical(path)
        return physical is not None and os.path.isdir(physical)

    def is_symlink(self, path: PathLike) -> bool:
        """``os.path.islink`` of the highest layer's entry (the link itself, not followed)."""
        self._log("is_symlink", path)
        physical = self._physical(path)
        return physical is not None and os.path.islink(physical)

    def stat(self, path: PathLike) -> FileInfo | None:
        """``file.info`` of the highest layer's entry, times in the frozen clock's zone."""
        self._log("stat", path)
        physical = self._physical(path)
        return _stat_or_none(physical, self._zone()) if physical is not None else None

    def listdir_like_r(
        self, path: PathLike, *, pattern: str | None = None, full_names: bool = False
    ) -> list[str]:
        """``dir(path, pattern, full.names)`` on the merged listing."""
        self._log("listdir_like_r", path)
        names = self._merged(path)
        if names is None:
            return []
        selected = _select_names(names, pattern)
        return [_join_like_r(os.fspath(path), n) for n in selected] if full_names else selected

    def list_dirs_like_r(self, path: PathLike, *, full_names: bool = True) -> list[str]:
        """``list.dirs(path, recursive = FALSE, full.names)`` on the merged listing."""
        self._log("list_dirs_like_r", path)
        names = self._merged(path)
        if names is None:
            return []
        logical = self._absolute(path)
        dirs = sort_like_r(n for n in names if self._is_dir_logical(logical / n))
        return [_join_like_r(os.fspath(path), n) for n in dirs] if full_names else dirs

    def glob(self, pattern: PathLike) -> list[str]:
        """``Sys.glob`` over every layer, mapped back to logical paths, dot-names kept out, byte order."""
        self._log("glob", pattern)
        logical = self._absolute(pattern)
        if self._paths is None or not logical.is_relative_to(self._paths.logical_root):
            return sorted(globmodule.glob(str(logical)))
        relative = str(logical.relative_to(self._paths.logical_root))
        found: set[str] = set()
        for layer in self._paths.layers:
            for match in globmodule.glob(relative, root_dir=layer):
                candidate = self._paths.logical_root / match
                if self._paths.first_existing(candidate) is not None:
                    found.add(str(candidate))
        return sorted(found)

    def normalize_path(self, path: PathLike) -> str:
        """``normalizePath``: the resolved logical path when it exists, the input unchanged otherwise."""
        self._log("normalize_path", path)
        logical = self._absolute(path)
        physical = self._physical(logical)
        if physical is None:
            return os.fspath(path)
        real = Path(os.path.realpath(physical))
        mapped = self._paths.to_logical(real) if self._paths is not None else None
        return mapped if mapped is not None else str(real)

    def read_text(self, path: PathLike, *, encoding: str = "utf-8", errors: str = "surrogateescape") -> str:
        """The highest layer's file as text."""
        self._log("read_text", path)
        return self._require(path).read_text(encoding=encoding, errors=errors)

    def read_bytes(self, path: PathLike) -> bytes:
        """The highest layer's file as bytes."""
        self._log("read_bytes", path)
        return self._require(path).read_bytes()

    def open_binary(self, path: PathLike) -> BinaryIO:
        """A binary stream on the highest layer's file; the caller closes it."""
        self._log("open_binary", path)
        return open(self._require(path), "rb")  # noqa: SIM115 - the stream is handed to the caller

    # -- mutations (real outside the overlay root, refused inside it) ----------------------------

    def write_text(
        self, path: PathLike, text: str, *, encoding: str = "utf-8", errors: str = "surrogateescape"
    ) -> None:
        """Atomic write on a path outside the overlay."""
        self._log("write_text", path, text)
        _write_bytes_atomically(self._writable(path), text.encode(encoding, errors))

    def write_bytes(self, path: PathLike, data: bytes) -> None:
        """Atomic write on a path outside the overlay."""
        self._log("write_bytes", path, f"{len(data)} bytes")
        _write_bytes_atomically(self._writable(path), data)

    def replace_in_file(self, path: PathLike, old: str, new: str) -> int:
        """``sed -i`` on a path outside the overlay."""
        self._log("replace_in_file", path, old, new)
        return _replace_in_file(self._writable(path), old, new)

    def copy(self, src: PathLike, dst: PathLike) -> None:
        """Copy the overlay's (or real) ``src`` to a ``dst`` outside the overlay."""
        self._log("copy", src, dst)
        shutil.copy(self._require(src), self._writable(dst))

    def move(self, src: PathLike, dst: PathLike) -> None:
        """Move between paths outside the overlay."""
        self._log("move", src, dst)
        shutil.move(self._writable(src), self._writable(dst))

    def rename(self, src: PathLike, dst: PathLike) -> None:
        """Rename between paths outside the overlay."""
        self._log("rename", src, dst)
        os.replace(self._writable(src), self._writable(dst))

    def delete_tree(self, path: PathLike) -> None:
        """Delete a path outside the overlay."""
        self._log("delete_tree", path)
        _delete_tree(self._writable(path))

    def mkdir(self, path: PathLike) -> None:
        """Create a directory outside the overlay."""
        self._log("mkdir", path)
        os.makedirs(self._writable(path), exist_ok=True)

    # -- programs, network, time, locking --------------------------------------------------------

    def run(
        self,
        argv: Sequence[str],
        *,
        cwd: PathLike | None = None,
        env: Mapping[str, str] | None = None,
        check: bool = False,
        capture: bool = True,
        on_line: LineCallback | None = None,
    ) -> RunResult:
        """Answer ``squeue``/``sacct`` from the canned case; any other program is an error."""
        _check_run_arguments(argv, capture, on_line)
        tool = os.path.basename(argv[0])
        if tool not in _CANNED_TOOLS:
            self._log("run", *argv)
            raise NotImplementedError(f"FakeEffects has no canned answer for {tool!r} (argv {list(argv)})")
        result = self._slurm.answer(tool, argv[1:])
        self.calls.append(EffectCall("run", tuple(argv), result.returncode))
        if check and not result.ok:
            raise CommandFailedError(result)
        return _deliver(result, capture, on_line)

    def post_json(self, url: str, payload: object) -> HttpResponse:
        """No network: the fake ``curl``'s default answer (``200``, body ``ok`` plus newline), logged."""
        response = CannedAmt.empty().http_answer(url)
        self.calls.append(EffectCall("post_json", (url, PostRecord(url, payload).raw), response.status))
        return response

    def sleep(self, seconds: float) -> None:
        """Logged, never waits (the oracle's ``Sys.sleep`` was a no-op)."""
        self._log("sleep", str(seconds))

    def lock(self, path: PathLike, *, wait: bool = True) -> AbstractContextManager[None]:
        """A real ``flock`` on a path outside the overlay."""
        self._log("lock", path)
        return _flock(self._writable(path), wait)

    # -- internals -------------------------------------------------------------------------------

    def _log(self, name: str, *args: PathLike) -> None:
        self.calls.append(EffectCall(name, tuple(os.fspath(a) for a in args)))

    def _zone(self) -> tzinfo:
        zone = self._now.tzinfo
        assert zone is not None  # checked in __init__
        return zone

    def _absolute(self, path: PathLike) -> PurePosixPath:
        """Resolve a relative path against the logical cwd.

        No symlink processing; ``..`` is kept, ``OverlayPaths.lookup`` walks it the way the kernel does.
        """
        return self._cwd / PurePosixPath(os.fspath(path))

    def _physical(self, path: PathLike) -> Path | None:
        """The physical path the overlay shows, or ``None``; ``""`` is nothing, as ``file.exists("")`` is."""
        if os.fspath(path) == "":
            return None
        logical = self._absolute(path)
        if self._paths is None:
            physical = Path(logical)
            return physical if os.path.exists(physical) else None
        return self._paths.first_existing(logical)

    def _writable(self, path: PathLike) -> str:
        """The real path for a mutation; refused inside the overlay root (the layers are canonical)."""
        logical = self._absolute(path)
        if self._paths is not None and logical.is_relative_to(self._paths.logical_root):
            raise NotImplementedError(
                f"FakeEffects cannot modify {os.fspath(path)}: the overlay layers are read-only"
                " (use RecordingEffects on a clone)"
            )
        return str(logical)

    def _is_dir_logical(self, logical: PurePosixPath) -> bool:
        physical = self._paths.first_existing(logical) if self._paths is not None else Path(logical)
        return physical is not None and os.path.isdir(physical)

    def _merged(self, path: PathLike) -> set[str] | None:
        """The directory entries the overlay shows, ``None`` for a missing path."""
        logical = self._absolute(path)
        if self._paths is None:
            try:
                return set(os.listdir(logical))
            except OSError:
                return set() if os.path.exists(logical) else None
        return self._paths.merged_names(logical)

    def _require(self, path: PathLike) -> Path:
        physical = self._physical(path)
        if physical is None:
            raise FileNotFoundError(f"no such file in the fake environment: {os.fspath(path)}")
        return physical


# ---------------------------------------------------------------------------------------------
# The AMT fakes: git, sbatch, rsync, make, Rscript start.R and curl (harness/fakebin)
# ---------------------------------------------------------------------------------------------

_GIT_LOG_1_DEFAULT = (
    "commit 3f5e2a1b9c8d7e6f5a4b3c2d1e0f9a8b7c6d5e4f\n"
    "Author: AMT Bot <amt@example.org>\n"
    "Date:   Mon Sep 28 10:00:00 2026 +0200\n"
    "\n"
    "    Merge pull request #999 from example/feature"
)
_GIT_LOG_MERGES_DEFAULT = (
    "3f5e2a1 Merge pull request #999 from example/feature\n"
    "1a2b3c4 Merge pull request #998 from example/bugfix"
)
_SBATCH_DEFAULT = "Submitted batch job 4242"
_CURL_DEFAULT = "HTTP/1.1 200 OK\nContent-Type: text/plain\n\nok"
_HTTP_STATUS_LINE = re.compile(r"^HTTP/\d(?:\.\d)? (\d{3})")


def _git_canned_name(argv: Sequence[str]) -> str:
    """The fake ``git``'s file stem: ``git-<sub>``, with ``log -1`` and ``log --merges`` told apart."""
    sub = next((a for a in argv if not a.startswith("-")), "")
    if sub == "log":
        joined = f" {' '.join(argv)} "
        if " -1 " in joined:
            return "git-log-1"
        if " --merges " in joined:
            return "git-log-merges"
    return f"git-{sub}"


def _is_start_script_call(argv: Sequence[str]) -> bool:
    """The fake ``Rscript`` intercepts ``start.R`` (or ``*/start.R``) unless a ``-e`` comes first."""
    for arg in argv:
        if arg == "start.R" or arg.endswith("/start.R"):
            return True
        if arg == "-e":
            return False
    return False


class CannedAmt:
    """Canned answers of the AMT fakes from ``migration/cases/amt/<case>/`` (``harness/fakebin/_lib.sh``).

    ``canned NAME DEFAULT`` prints ``<name>.txt`` verbatim when the file exists, else ``DEFAULT`` plus a
    newline (nothing for an empty default); ``canned_exit NAME`` is the digits of ``<name>.exit``, else 0.
    The names: ``git-<sub>`` (``git-log-1``, ``git-log-merges``, ``git-reset``, ``git-pull``, ...),
    ``sbatch``, ``rsync``, ``make``, ``rscript-start`` (``Rscript start.R ...`` only) and ``curl`` (the
    webhook; ``http_answer``). A program without a fake answers from a generic ``<tool>.txt`` /
    ``<tool>.exit`` pair when the case has one, and ``None`` otherwise (the sandbox would have run the
    real program). Every canned program writes to stdout only, as the shell fakes do.
    """

    def __init__(self, files: Mapping[str, str]) -> None:
        """Take the case directory's regular files as ``name -> text``."""
        self._files = dict(files)

    @classmethod
    def from_case_dir(cls, case_dir: Path) -> CannedAmt:
        """Load every regular file of ``case_dir`` (``RS_AMT_CASE_DIR``)."""
        return cls(_case_files(case_dir))

    @classmethod
    def empty(cls) -> CannedAmt:
        """No case directory: the defaults only (``RS_AMT_CASE_DIR`` unset)."""
        return cls({})

    def canned(self, name: str, default: str) -> str:
        """``canned NAME DEFAULT``."""
        text = self._files.get(f"{name}.txt")
        if text is not None:
            return text
        return f"{default}\n" if default else ""

    def exit_status(self, name: str) -> int:
        """``canned_exit NAME``: the digits of ``<name>.exit`` (``tr -dc '0-9'``), 0 without the file."""
        text = self._files.get(f"{name}.exit")
        if text is None:
            return 0
        digits = re.sub(r"[^0-9]", "", text)
        return int(digits) if digits else 0

    def answer(self, tool: str, argv: Sequence[str]) -> RunResult | None:
        """The fake's result for ``tool argv...``, or ``None`` when no fake handles the program."""
        selection = self._selection(tool, argv)
        if selection is None:
            return None
        name, default = selection
        return RunResult((tool, *argv), self.exit_status(name), self.canned(name, default), "")

    def http_answer(self, url: str) -> HttpResponse:
        """The fake ``curl``'s ``curl.txt`` as an HTTP exchange: the status line's code and the body.

        Without a status line (``curl.txt`` empty) the exchange counts as a transport failure: status 0,
        the ``curl.exit`` status in the body.
        """
        text = self.canned("curl", _CURL_DEFAULT)
        head, _, body = text.partition("\n\n")
        status_line = _HTTP_STATUS_LINE.match(head)
        if status_line:
            return HttpResponse(url, int(status_line.group(1)), body)
        return HttpResponse(url, 0, f"fake curl failure (exit {self.exit_status('curl')})")

    def _selection(self, tool: str, argv: Sequence[str]) -> tuple[str, str] | None:
        """The canned name and default text for a program, as the shell fakes choose them."""
        if tool == "git":
            name = _git_canned_name(argv)
            defaults = {"git-log-1": _GIT_LOG_1_DEFAULT, "git-log-merges": _GIT_LOG_MERGES_DEFAULT}
            return (name, defaults.get(name, ""))
        if tool == "sbatch":
            return ("sbatch", _SBATCH_DEFAULT)
        if tool == "rsync":
            return ("rsync", "")
        if tool == "make":
            return ("make", f"fake make: {' '.join(argv)}")
        if tool == "Rscript" and _is_start_script_call(argv):
            return ("rscript-start", f"fake Rscript: {' '.join(argv)}")
        if f"{tool}.txt" in self._files or f"{tool}.exit" in self._files:
            return (tool, "")
        return None


# ---------------------------------------------------------------------------------------------
# RecordingEffects: real files, canned programs, frozen clock, every call recorded
# ---------------------------------------------------------------------------------------------


class RecordingEffects:
    """The AMT test double (``06`` 4.4): real filesystem, canned programs and webhook, recorded calls.

    Paths are real; relative ones resolve against the injected ``cwd`` (the process's directory by
    default), so a test can point the effects at a temporary checkout without ``chdir``. ``amt`` answers
    ``git``, ``sbatch``, ``rsync``, ``make`` and ``Rscript start.R`` (``CannedAmt``), ``slurm`` answers
    ``squeue`` and ``sacct`` (``CannedSlurm``); a program neither knows goes to ``fallback`` when one is
    given and raises ``NotImplementedError`` otherwise. ``post_json`` answers from ``curl.txt``. ``now()``
    is frozen; ``sleep`` returns at once and, with ``advance_on_sleep``, moves the clock forward by the
    requested seconds instead (for wait loops with a deadline). ``lock`` is a real ``flock``. Records:
    ``calls`` (every call, ``EffectCall``), ``trace`` (every ``run``, ``TraceRecord``, the fake binaries'
    ``trace.jsonl`` schema) and ``posts`` (every ``post_json``, ``PostRecord``, the ``mattermost.jsonl``
    schema).
    """

    def __init__(
        self,
        *,
        now: datetime,
        user: str,
        on_cluster: bool,
        amt: CannedAmt | None = None,
        slurm: CannedSlurm | None = None,
        cwd: PathLike | None = None,
        environ: Mapping[str, str] | None = None,
        cluster_paths: ClusterPaths | None = None,
        fallback: Effects | None = None,
        advance_on_sleep: bool = False,
    ) -> None:
        """Inject the frozen clock (aware), the identity, the canned answers and the working directory."""
        if now.tzinfo is None:
            raise ValueError("RecordingEffects needs an aware datetime for now()")
        self._now = now
        self._user = user
        self._on_cluster = on_cluster
        self._amt = amt if amt is not None else CannedAmt.empty()
        self._slurm = slurm if slurm is not None else CannedSlurm.empty()
        self._cwd = os.path.abspath(os.fspath(cwd)) if cwd is not None else os.getcwd()
        self._environ: dict[str, str] = dict(environ or {})
        self._cluster_paths = cluster_paths if cluster_paths is not None else ClusterPaths()
        self._fallback = fallback
        self._advance_on_sleep = advance_on_sleep
        self.calls: list[EffectCall] = []
        self.trace: list[TraceRecord] = []
        self.posts: list[PostRecord] = []

    # -- identity, clock, variables ------------------------------------------------------------

    @property
    def user(self) -> str:
        """The injected ``Sys.info()[["user"]]``."""
        self._log("user")
        return self._user

    @property
    def on_cluster(self) -> bool:
        """The injected ``file.exists("/p")``."""
        self._log("on_cluster")
        return self._on_cluster

    @property
    def cluster_paths(self) -> ClusterPaths:
        """The injected cluster locations (a test points ``amt_dir`` at its checkout)."""
        return self._cluster_paths

    @property
    def cwd(self) -> str:
        """The injected working directory (absolute)."""
        return self._cwd

    @property
    def paths(self) -> OverlayPaths | None:
        """No overlay: paths are physical (``modelstats.facts.physical_path`` resolves against ``cwd``)."""
        return None

    @property
    def runs(self) -> list[EffectCall]:
        """The logged ``run`` calls."""
        return [c for c in self.calls if c.name == "run"]

    def getenv(self, name: str) -> str:
        """``Sys.getenv(name)`` on the injected variables."""
        self._log("getenv", name)
        return self._environ.get(name, "")

    @property
    def magpie_results_archive(self) -> str:
        """The injected ``MAGPIE_RESULTS_ARCHIVE_PATH``."""
        return self.getenv(MAGPIE_RESULTS_ARCHIVE_VARIABLE)

    def now(self) -> datetime:
        """The frozen clock (advanced by ``sleep`` only with ``advance_on_sleep``)."""
        self._log("now")
        return self._now

    # -- reading ---------------------------------------------------------------------------------

    def exists(self, path: PathLike) -> bool:
        """``file.exists`` on the real filesystem."""
        self._log("exists", path)
        return os.path.exists(self._resolve(path))

    def is_dir(self, path: PathLike) -> bool:
        """``dir.exists`` on the real filesystem."""
        self._log("is_dir", path)
        return os.path.isdir(self._resolve(path))

    def is_symlink(self, path: PathLike) -> bool:
        """``os.path.islink`` on the real filesystem (the link itself, not followed)."""
        self._log("is_symlink", path)
        return os.path.islink(self._resolve(path))

    def stat(self, path: PathLike) -> FileInfo | None:
        """``file.info``, times in the frozen clock's zone."""
        self._log("stat", path)
        return _stat_or_none(self._resolve(path), self._zone())

    def listdir_like_r(
        self, path: PathLike, *, pattern: str | None = None, full_names: bool = False
    ) -> list[str]:
        """``dir(path, pattern, full.names)``; the pasted path keeps the caller's spelling."""
        self._log("listdir_like_r", path)
        names = _listdir_or_none(self._resolve(path))
        if names is None:
            return []
        selected = _select_names(names, pattern)
        return [_join_like_r(os.fspath(path), n) for n in selected] if full_names else selected

    def list_dirs_like_r(self, path: PathLike, *, full_names: bool = True) -> list[str]:
        """``list.dirs(path, recursive = FALSE, full.names)``."""
        self._log("list_dirs_like_r", path)
        resolved = self._resolve(path)
        names = _listdir_or_none(resolved)
        if names is None:
            return []
        dirs = sort_like_r(n for n in names if os.path.isdir(os.path.join(resolved, n)))
        return [_join_like_r(os.fspath(path), n) for n in dirs] if full_names else dirs

    def glob(self, pattern: PathLike) -> list[str]:
        """``Sys.glob`` relative to the injected cwd (a relative pattern gives relative matches)."""
        self._log("glob", pattern)
        return sorted(globmodule.glob(os.fspath(pattern), root_dir=self._cwd))

    def normalize_path(self, path: PathLike) -> str:
        """``normalizePath``: realpath of an existing path, the input unchanged otherwise."""
        self._log("normalize_path", path)
        resolved = self._resolve(path)
        return os.path.realpath(resolved) if os.path.exists(resolved) else os.fspath(path)

    def read_text(self, path: PathLike, *, encoding: str = "utf-8", errors: str = "surrogateescape") -> str:
        """The whole file as text."""
        self._log("read_text", path)
        return Path(self._resolve(path)).read_text(encoding=encoding, errors=errors)

    def read_bytes(self, path: PathLike) -> bytes:
        """The whole file as bytes."""
        self._log("read_bytes", path)
        return Path(self._resolve(path)).read_bytes()

    def open_binary(self, path: PathLike) -> BinaryIO:
        """A binary stream; the caller closes it."""
        self._log("open_binary", path)
        return open(self._resolve(path), "rb")  # noqa: SIM115 - the stream is handed to the caller

    # -- writing (real, recorded) ----------------------------------------------------------------

    def write_text(
        self, path: PathLike, text: str, *, encoding: str = "utf-8", errors: str = "surrogateescape"
    ) -> None:
        """Atomic write; see ``Effects.write_text``."""
        self._log("write_text", path, text)
        _write_bytes_atomically(self._resolve(path), text.encode(encoding, errors))

    def write_bytes(self, path: PathLike, data: bytes) -> None:
        """Atomic write of bytes."""
        self._log("write_bytes", path, f"{len(data)} bytes")
        _write_bytes_atomically(self._resolve(path), data)

    def replace_in_file(self, path: PathLike, old: str, new: str) -> int:
        """``sed -i 's/old/new/'``."""
        self._log("replace_in_file", path, old, new)
        return _replace_in_file(self._resolve(path), old, new)

    def copy(self, src: PathLike, dst: PathLike) -> None:
        """``file.copy(overwrite = TRUE)``."""
        self._log("copy", src, dst)
        shutil.copy(self._resolve(src), self._resolve(dst))

    def move(self, src: PathLike, dst: PathLike) -> None:
        """``mv src dst``."""
        self._log("move", src, dst)
        shutil.move(self._resolve(src), self._resolve(dst))

    def rename(self, src: PathLike, dst: PathLike) -> None:
        """``file.rename``."""
        self._log("rename", src, dst)
        os.replace(self._resolve(src), self._resolve(dst))

    def delete_tree(self, path: PathLike) -> None:
        """``unlink(recursive = TRUE)``."""
        self._log("delete_tree", path)
        _delete_tree(self._resolve(path))

    def mkdir(self, path: PathLike) -> None:
        """``dir.create(recursive = TRUE)``."""
        self._log("mkdir", path)
        os.makedirs(self._resolve(path), exist_ok=True)

    # -- programs, network, time, locking --------------------------------------------------------

    def run(
        self,
        argv: Sequence[str],
        *,
        cwd: PathLike | None = None,
        env: Mapping[str, str] | None = None,
        check: bool = False,
        capture: bool = True,
        on_line: LineCallback | None = None,
    ) -> RunResult:
        """Answer from the canned AMT or scheduler case, trace the call, stream the canned lines."""
        _check_run_arguments(argv, capture, on_line)
        tool = os.path.basename(argv[0])
        where = self._resolve(cwd) if cwd is not None else self._cwd
        merged = {**self._environ, **(env or {})}
        canned = self._canned_answer(tool, argv)
        self.trace.append(
            TraceRecord(
                tool,
                tuple(argv[1:]),
                where,
                _trace_env(merged),
                canned.returncode if canned is not None and tool in _CANNED_TOOLS else None,
            )
        )
        if canned is None:
            result = self._fallback_run(tool, argv, where, env, capture, on_line)
        else:
            result = _deliver(canned, capture, on_line)
        self.calls.append(EffectCall("run", tuple(argv), result.returncode))
        if check and not result.ok:
            raise CommandFailedError(result)
        return result

    def post_json(self, url: str, payload: object) -> HttpResponse:
        """The fake ``curl``'s answer for the case; the payload recorded like ``mattermost.jsonl``."""
        record = PostRecord(url, payload)
        self.posts.append(record)
        response = self._amt.http_answer(url)
        self.calls.append(EffectCall("post_json", (url, record.raw), response.status))
        return response

    def sleep(self, seconds: float) -> None:
        """Recorded, never waits; advances the frozen clock when ``advance_on_sleep`` is set."""
        self._log("sleep", str(seconds))
        if self._advance_on_sleep:
            self._now = self._now + timedelta(seconds=seconds)

    def lock(self, path: PathLike, *, wait: bool = True) -> AbstractContextManager[None]:
        """A real ``flock``; see ``Effects.lock``."""
        self._log("lock", path)
        return _flock(self._resolve(path), wait)

    # -- records ---------------------------------------------------------------------------------

    def trace_lines(self) -> list[str]:
        """The ``trace.jsonl`` lines of every ``run`` so far."""
        return [record.json_line() for record in self.trace]

    def write_trace(self, path: PathLike) -> None:
        """Write ``trace.jsonl`` (one record per line, as ``_trace.py`` appends them)."""
        Path(path).write_text("".join(f"{line}\n" for line in self.trace_lines()), encoding="utf-8")

    def post_lines(self) -> list[str]:
        """The ``mattermost.jsonl`` lines of every ``post_json`` so far."""
        return [record.json_line() for record in self.posts]

    # -- internals -------------------------------------------------------------------------------

    def _log(self, name: str, *args: PathLike) -> None:
        self.calls.append(EffectCall(name, tuple(os.fspath(a) for a in args)))

    def _zone(self) -> tzinfo:
        zone = self._now.tzinfo
        assert zone is not None  # checked in __init__
        return zone

    def _resolve(self, path: PathLike) -> str:
        """A relative path against the injected cwd; ``""`` stays ``""`` (``file.exists("")`` is FALSE)."""
        text = os.fspath(path)
        return os.path.join(self._cwd, text) if text else text

    def _canned_answer(self, tool: str, argv: Sequence[str]) -> RunResult | None:
        if tool in _CANNED_TOOLS:
            return self._slurm.answer(tool, argv[1:])
        answer = self._amt.answer(tool, argv[1:])
        return RunResult(tuple(argv), answer.returncode, answer.stdout, answer.stderr) if answer else None

    def _fallback_run(
        self,
        tool: str,
        argv: Sequence[str],
        where: str,
        env: Mapping[str, str] | None,
        capture: bool,
        on_line: LineCallback | None,
    ) -> RunResult:
        if self._fallback is None:
            raise NotImplementedError(
                f"RecordingEffects has no canned answer for {tool!r} (argv {list(argv)}) and no fallback"
            )
        return self._fallback.run(argv, cwd=where, env=env, check=False, capture=capture, on_line=on_line)


# ---------------------------------------------------------------------------------------------
# DryRunEffects: reads for real, mutations logged and answered synthetically
# ---------------------------------------------------------------------------------------------

DRY_RUN_COMMIT = "deadbeef" * 5
"""The commit hash a dry run reports for ``git log -1`` / ``git rev-parse`` (40 hex digits, plainly fake)."""

DRY_RUN_RESPONSE_BODY = "dry run: nothing was sent"
"""The body of the ``200`` a dry run answers for ``post_json``."""

SELECT_SCENARIOS_SCRIPT = "select_scenarios.R"
"""The ``selectScenarios()`` bridge script of ``modelstats.amt.bridges`` (its ``BRIDGE_SCRIPT``): a dry run
recognises ``Rscript <...>/select_scenarios.R <group>`` and answers an empty selection instead of running
it, because the script sources the checkout's ``scripts/start/*.R`` (REMIND code, not a read)."""


def _dry_run_answer(argv: Sequence[str], now: datetime) -> RunResult:
    """The synthetic result of a program that was not run (``03`` 2.3)."""
    tool = os.path.basename(argv[0])
    rest = argv[1:]
    stdout = ""
    if tool == "git":
        sub = next((a for a in rest if not a.startswith("-")), "")
        if sub == "log" and "-1" in rest:
            stdout = (
                f"commit {DRY_RUN_COMMIT}\nAuthor: dry run <dry-run@localhost>\n"
                f"Date:   {now:%a %b %d %H:%M:%S %Y %z}\n\n    dry run: no repository was read\n"
            )
        elif sub == "rev-parse":
            stdout = f"{DRY_RUN_COMMIT}\n"
    elif tool == "sbatch":
        stdout = "Submitted batch job 0\n"
    elif tool == "Rscript" and rest and os.path.basename(rest[0]) == SELECT_SCENARIOS_SCRIPT:
        stdout = _dry_run_selection(rest[1] if len(rest) > 1 else "AMT")
    return RunResult(tuple(argv), 0, stdout, "")


def _dry_run_selection(startgroup: str) -> str:
    """The bridge payload of an empty selection (``modelstats.amt.bridges.parse_payload`` reads it)."""
    return json.dumps({"schema": 1, "startgroup": startgroup, "names": [], "columns": {}}) + "\n"


class DryRunEffects:
    """``--dry-run``: reads go to the wrapped effects, mutations are logged and answered synthetically.

    ``messages`` collects every ``would ...`` line (and ``log`` receives each one as it happens, for the
    cycle log); ``calls`` records the mutating calls as ``EffectCall``. ``run`` executes nothing, not even
    a query (``squeue`` answers empty, ``git log -1`` the fake commit ``DRY_RUN_COMMIT``, ``sbatch`` job
    ``0``, ``Rscript select_scenarios.R`` an empty selection), so a wait loop ends at once and later steps
    still have inputs; ``post_json`` is suppressed and answers ``200`` (its announcement names the webhook
    host only: the URL path carries the secret); ``sleep`` and ``lock`` do nothing (a dry run writes
    nothing, so it cannot interleave).
    """

    def __init__(self, inner: Effects | None = None, *, log: Callable[[str], None] | None = None) -> None:
        """Wrap ``inner`` (production by default); ``log`` receives every ``would ...`` message."""
        self._inner: Effects = inner if inner is not None else ProductionEffects()
        self._log_sink = log
        self.messages: list[str] = []
        self.calls: list[EffectCall] = []

    @property
    def inner(self) -> Effects:
        """The wrapped effects that answer the reads."""
        return self._inner

    # -- identity, clock, variables: the wrapped effects' --------------------------------------

    @property
    def user(self) -> str:
        """The wrapped effects' user."""
        return self._inner.user

    @property
    def on_cluster(self) -> bool:
        """The wrapped effects' cluster probe."""
        return self._inner.on_cluster

    @property
    def cluster_paths(self) -> ClusterPaths:
        """The wrapped effects' cluster locations."""
        return self._inner.cluster_paths

    @property
    def cwd(self) -> str:
        """The wrapped effects' working directory."""
        return self._inner.cwd

    @property
    def paths(self) -> OverlayPaths | None:
        """The wrapped effects' overlay, if it has one (so ``physical_path`` resolves as it would)."""
        paths = getattr(self._inner, "paths", None)
        return paths if isinstance(paths, OverlayPaths) else None

    def getenv(self, name: str) -> str:
        """The wrapped effects' variables."""
        return self._inner.getenv(name)

    @property
    def magpie_results_archive(self) -> str:
        """The wrapped effects' ``MAGPIE_RESULTS_ARCHIVE_PATH``."""
        return self._inner.magpie_results_archive

    def now(self) -> datetime:
        """The wrapped effects' clock."""
        return self._inner.now()

    # -- reading: delegated ----------------------------------------------------------------------

    def exists(self, path: PathLike) -> bool:
        """Delegated."""
        return self._inner.exists(path)

    def is_dir(self, path: PathLike) -> bool:
        """Delegated."""
        return self._inner.is_dir(path)

    def is_symlink(self, path: PathLike) -> bool:
        """Delegated."""
        return self._inner.is_symlink(path)

    def stat(self, path: PathLike) -> FileInfo | None:
        """Delegated."""
        return self._inner.stat(path)

    def listdir_like_r(
        self, path: PathLike, *, pattern: str | None = None, full_names: bool = False
    ) -> list[str]:
        """Delegated."""
        return self._inner.listdir_like_r(path, pattern=pattern, full_names=full_names)

    def list_dirs_like_r(self, path: PathLike, *, full_names: bool = True) -> list[str]:
        """Delegated."""
        return self._inner.list_dirs_like_r(path, full_names=full_names)

    def glob(self, pattern: PathLike) -> list[str]:
        """Delegated."""
        return self._inner.glob(pattern)

    def normalize_path(self, path: PathLike) -> str:
        """Delegated."""
        return self._inner.normalize_path(path)

    def read_text(self, path: PathLike, *, encoding: str = "utf-8", errors: str = "surrogateescape") -> str:
        """Delegated."""
        return self._inner.read_text(path, encoding=encoding, errors=errors)

    def read_bytes(self, path: PathLike) -> bytes:
        """Delegated."""
        return self._inner.read_bytes(path)

    def open_binary(self, path: PathLike) -> BinaryIO:
        """Delegated."""
        return self._inner.open_binary(path)

    # -- mutations: logged, nothing happens ------------------------------------------------------

    def write_text(
        self, path: PathLike, text: str, *, encoding: str = "utf-8", errors: str = "surrogateescape"
    ) -> None:
        """``would write <path>``."""
        self._would(f"write {os.fspath(path)}", "write_text", path, text)

    def write_bytes(self, path: PathLike, data: bytes) -> None:
        """``would write <path>``."""
        self._would(f"write {os.fspath(path)}", "write_bytes", path, f"{len(data)} bytes")

    def replace_in_file(self, path: PathLike, old: str, new: str) -> int:
        """``would replace ... in <path>``; reports the lines that would change."""
        self._would(f"replace {old!r} by {new!r} in {os.fspath(path)}", "replace_in_file", path, old, new)
        text = self._inner.read_text(path)
        return sum(1 for piece in _LINE_PIECES.split(text) if old and old in piece)

    def copy(self, src: PathLike, dst: PathLike) -> None:
        """``would copy <src> to <dst>``."""
        self._would(f"copy {os.fspath(src)} to {os.fspath(dst)}", "copy", src, dst)

    def move(self, src: PathLike, dst: PathLike) -> None:
        """``would move <src> to <dst>``."""
        self._would(f"move {os.fspath(src)} to {os.fspath(dst)}", "move", src, dst)

    def rename(self, src: PathLike, dst: PathLike) -> None:
        """``would rename <src> to <dst>``."""
        self._would(f"rename {os.fspath(src)} to {os.fspath(dst)}", "rename", src, dst)

    def delete_tree(self, path: PathLike) -> None:
        """``would delete <path>``."""
        self._would(f"delete {os.fspath(path)}", "delete_tree", path)

    def mkdir(self, path: PathLike) -> None:
        """``would create directory <path>``."""
        self._would(f"create directory {os.fspath(path)}", "mkdir", path)

    # -- programs, network, time, locking: synthetic ---------------------------------------------

    def run(
        self,
        argv: Sequence[str],
        *,
        cwd: PathLike | None = None,
        env: Mapping[str, str] | None = None,
        check: bool = False,
        capture: bool = True,
        on_line: LineCallback | None = None,
    ) -> RunResult:
        """``would run <command>``; the synthetic result of ``_dry_run_answer`` (always status 0)."""
        _check_run_arguments(argv, capture, on_line)
        where = f" in {os.fspath(cwd)}" if cwd is not None else ""
        message = f"run {shlex.join(argv)}{where}"
        result = _dry_run_answer(argv, self._inner.now())
        self._would(message, "run", *argv)
        return _deliver(result, capture, on_line)

    def post_json(self, url: str, payload: object) -> HttpResponse:
        """``would post JSON to the webhook at <scheme>://<host> (...)``; answers ``200`` without sending.

        The path of the URL, which carries the webhook secret, is redacted from the announcement (the
        in-memory ``calls`` record keeps the full URL, as ``FakeEffects.posts`` does; neither is logged).
        """
        raw = PostRecord(url, payload).raw
        self._would(
            f"post JSON to {_webhook_location(url)} (path redacted, {len(raw)} characters)",
            "post_json",
            url,
            raw,
        )
        return HttpResponse(url, 200, DRY_RUN_RESPONSE_BODY)

    def sleep(self, seconds: float) -> None:
        """``would sleep <n> s``; returns at once."""
        self._would(f"sleep {seconds} s", "sleep", str(seconds))

    def lock(self, path: PathLike, *, wait: bool = True) -> AbstractContextManager[None]:
        """``would lock <path>``; no lock file is created."""
        self._would(f"lock {os.fspath(path)}", "lock", path)
        return contextlib.nullcontext()

    # -- internals -------------------------------------------------------------------------------

    def _would(self, what: str, name: str, *args: PathLike) -> None:
        message = f"would {what}"
        self.messages.append(message)
        self.calls.append(EffectCall(name, tuple(os.fspath(a) for a in args), 0 if name == "run" else None))
        if self._log_sink is not None:
            self._log_sink(message)


def _webhook_location(url: str) -> str:
    """``the webhook at <scheme>://<host>`` of a URL, or ``the webhook (URL redacted)`` when it has none."""
    parts = urllib.parse.urlsplit(url)
    if parts.scheme and parts.netloc:
        return f"the webhook at {parts.scheme}://{parts.netloc}"
    return "the webhook (URL redacted)"


__all__ = [
    "DRY_RUN_COMMIT",
    "DRY_RUN_RESPONSE_BODY",
    "INVALID_WEBHOOK_URL",
    "MAGPIE_RESULTS_ARCHIVE_VARIABLE",
    "NOT_A_WEBHOOK_URL",
    "SELECT_SCENARIOS_SCRIPT",
    "TRACE_ENV_KEYS",
    "CannedAmt",
    "CannedSlurm",
    "ClusterPaths",
    "CommandFailedError",
    "DryRunEffects",
    "EffectCall",
    "Effects",
    "FakeEffects",
    "FileInfo",
    "HttpResponse",
    "LineCallback",
    "LockHeldError",
    "Mode",
    "OverlayPaths",
    "PathLike",
    "PostRecord",
    "ProductionEffects",
    "RecordingEffects",
    "RunResult",
    "Stream",
    "TraceRecord",
    "r_collation_key",
    "sort_like_r",
]
