"""Effects: the only path from modelstats to its environment (plan 03 sections 2.3 and 5).

Every public function of the package takes ``effects: Effects | None = None`` and
falls back to :func:`default_effects`. ``Effects`` is the interface, ``ProductionEffects``
does the real thing, ``DryRunEffects`` (``modeltests --dry-run``) executes reads for real
and only logs mutations, and the test double ``RecordingEffects`` / ``FakeEffects``
(``tests/unit/_fake_effects.py``, not part of the package) records every call and answers
the clock, the user, the cluster probe, ``squeue`` / ``sacct``, HTTP and sleeping from
canned data while filesystem reads and writes go to the real path.

Two parts:

- the read-only subset used by ``rs`` (phases 1 to 4): abstract, unchanged since phase 1;
- the mutating part used by the AMT (phase 5, plan 03 section 2.3): ``write_text``,
  ``write_bytes``, ``write_rds``, ``copy``, ``rename``, ``delete``, ``mkdir``, ``chdir``,
  ``setenv``, ``run`` with ``shell`` and ``capture``, ``run_shell``, ``post_json``, ``sleep``,
  ``tempdir``, ``getcwd``, ``listdir_all`` and ``walk``. These are NOT abstract: a read-only
  double written for phases 1 to 4 stays instantiable, and each of them raises
  ``NotImplementedError`` until a subclass implements it (ProductionEffects implements all).

R facts reproduced here (verified with R 4.6.1 on 2026-10-01, see the unit tests):

- ``dir()`` lists files and directories alike, skips dot-files, returns ``character(0)``
  for a missing path or a file, and sorts with the collation of the session. R uses ICU
  (``icuGetCollate()`` is ``"root"``) in every locale except plain ``C``/``POSIX``,
  including the sandbox's ``LC_ALL=C.utf8``, so the goldens carry ICU root order:
  ``_x | a | ä | A1 | a10 | a2 | b | B`` rather than code-point order. :func:`r_sort`
  reproduces that order (see its docstring for the extent of the emulation).
  ``dir(all.files = TRUE, no.. = TRUE)`` and ``list.dirs(recursive = FALSE)`` include the
  dot entries (``.hid`` before ``archive``) but never ``.`` and ``..``.
- ``file.exists()`` follows symlinks (a dangling one is ``FALSE``), ``Sys.glob()`` returns
  the matches sorted by code point (``glob(3)`` under ``C.utf8``) and never matches a
  leading dot with ``*``.
- ``dir()``, ``file.exists()``, ``file.info()``, ``Sys.glob()`` (which returns the expanded
  paths) and file connections apply ``path.expand()``: a leading ``~`` or ``~user`` becomes
  the home directory, a tilde anywhere else is kept (``a~/b``); ``system()`` runs through a
  shell, which expands ``~`` the same way. :func:`_expand` does that for every path here.
- ``Sys.Date()`` is the date in the local time zone; ``Sys.time()`` is time-zone aware.
- ``Sys.info()[["user"]]`` is the passwd name of the uid (``"unknown"`` when the uid has
  no passwd entry); ``Sys.getenv("X")`` is ``""`` when unset; ``Sys.setenv()`` is seen by
  every later child process.
- ``system(cmd, intern = TRUE)`` returns the stdout lines (see
  :func:`modelstats.textscan.r_intern_lines` for the exact line semantics) with the exit
  status as an attribute; a command that cannot be run (status 127) raises
  ``error in running command``. ``system(cmd)`` without ``intern`` lets the child write to
  the process's own stdout and stderr (file descriptors 1 and 2, which ``sink()`` does not
  intercept), returns the status and only warns ``error in running command`` on 127.
  :meth:`Effects.run` returns the status in both cases and leaves the error or warning to
  the caller. Both forms run the command line through ``/bin/sh``.
- ``writeLines(x, con)`` writes ``x`` plus ``\\n`` over the file in place (same inode and
  mode); ``write(x, file, append = TRUE)`` appends one line per element (nothing for a
  zero-length vector); a file in a missing directory fails with ``cannot open the
  connection``. ``saveRDS()`` creates files with the umask mode (``644``), not ``600``.
- ``file.copy(from, to, overwrite)`` returns ``FALSE`` silently for a missing ``from`` or
  an existing ``to`` without ``overwrite``; ``to`` may be a directory; the mode is copied
  (``copy.mode = TRUE``). ``file.rename()`` returns ``FALSE`` with a warning ``cannot rename
  file 'a' to 'b', reason '...'``. ``unlink()`` is silent: a missing path is a success, a
  directory without ``recursive = TRUE`` is left alone. ``dir.create()`` warns
  ``'x' already exists`` / ``cannot create dir 'x', reason '...'`` and returns ``FALSE``.
  ``setwd()`` (``withr::with_dir``, ``local_dir``) fails with
  ``cannot change working directory``. ``tempdir()`` is one per-session directory.
"""

from __future__ import annotations

import abc
import atexit
import contextlib
import datetime as dt
import glob as _glob
import os
import pwd
import secrets
import shlex
import shutil
import stat as _stat
import subprocess
import sys
import tempfile
import time
import unicodedata
import urllib.error
import urllib.request
from collections.abc import Callable, Iterable, Iterator, Mapping, Sequence
from dataclasses import dataclass, field
from email.message import Message
from pathlib import Path
from typing import IO, NamedTuple, NoReturn

from modelstats.errors import RParityError

__all__ = [
    "DRY_RUN_COMMIT",
    "HTTP_TIMEOUT",
    "DryRunEffects",
    "DryRunEvent",
    "Effects",
    "FileStat",
    "PathLike",
    "ProductionEffects",
    "default_effects",
    "dry_run_answer",
    "r_collate_key",
    "r_sort",
    "shell_segments",
    "shell_tokens",
]

type PathLike = str | os.PathLike[str]

#: Seconds :meth:`ProductionEffects.post_json` waits for the webhook. R's ``curl`` call has no
#: timeout; a cron job that hangs on a dead webhook is worse than a late notification.
HTTP_TIMEOUT = 60.0

#: The commit hash :class:`DryRunEffects` answers ``git log -1`` with.
DRY_RUN_COMMIT = "0000000000000000000000000000000000000000"


class _NoRedirectHandler(urllib.request.HTTPRedirectHandler):
    """``curl`` without ``--location`` (``R/modeltests.R`` lines 58-61): a 3xx answer is the response, not a hop.

    urllib would otherwise follow a 301/302/303 with a second, body-less GET (and a 307/308 with the POST
    repeated), a request R's AMT never makes; returning ``None`` makes the 3xx an ``HTTPError`` that
    :meth:`ProductionEffects.post_json` reports like any other status.
    """

    def redirect_request(
        self, req: urllib.request.Request, fp: IO[bytes], code: int, msg: str, headers: Message, newurl: str
    ) -> urllib.request.Request | None:
        return None


_OPENER = urllib.request.build_opener(_NoRedirectHandler)

_SHELL_OPERATORS = frozenset({"&&", "||", ";", "|", "&"})


class FileStat(NamedTuple):
    """The ``file.info()`` triple the port needs: times as POSIX seconds (float), size in bytes."""

    mtime: float
    ctime: float
    size: int


# ---------------------------------------------------------------------------
# R string collation (ICU root), used by dir() and sort()
# ---------------------------------------------------------------------------

# Primary order of the ASCII repertoire under ICU root collation (CLDR, variable weighting
# "non-ignorable"): whitespace, then punctuation and symbols in this fixed order, then the
# digits, then the letters. Taken from R 4.6.1 / ICU 78.3: sort() of every printable ASCII
# character gives `_-,;:!?.'"()[]{}@*/\&#%`^+<=>|~$0123456789aAbB...zZ` and TAB < SPACE.
_PRIMARY_ASCII = "\t\n\v\f\r _-,;:!?.'\"()[]{}@*/\\&#%`^+<=>|~$0123456789abcdefghijklmnopqrstuvwxyz"
_PRIMARY: dict[str, int] = {ch: i + 1 for i, ch in enumerate(_PRIMARY_ASCII)}
_PRIMARY_FALLBACK = 0x20000  # anything else sorts after the letters, by code point
_SECONDARY_COMMON = 1
_TERTIARY_LOWER = 1
_TERTIARY_UPPER = 2
_TERTIARY_LIGATURE = 3
# Letters without a canonical decomposition that ICU root treats as a base letter plus a
# difference below the primary level: ligatures (tertiary) and stroked letters (secondary).
_LIGATURES: dict[str, str] = {"æ": "ae", "œ": "oe", "ß": "ss", "ĳ": "ij", "ﬁ": "fi", "ﬂ": "fl"}
_STROKED: dict[str, str] = {"ø": "o", "đ": "d", "ł": "l", "ħ": "h", "ŧ": "t", "ð": "d", "þ": "th"}
_SECONDARY_STROKE = 0x400
# Secondary weights of the common combining marks in DUCET order (acute before grave:
# R sorts `é | É | è`); any other mark sorts after these by code point.
_SECONDARY_MARKS: dict[str, int] = {
    mark: _SECONDARY_COMMON + 1 + rank for rank, mark in enumerate(["́", "̀", "̆", "̂", "̌", "̊", "̈", "̋", "̃", "̇", "̧", "̨", "̄"])
}
_SECONDARY_MARK_FALLBACK = 0x100


def r_collate_key(text: str) -> tuple[tuple[int, ...], tuple[int, ...], tuple[int, ...]]:
    """Sort key reproducing R's ICU root collation for the names that occur in model runs.

    Three levels compared one after the other over the whole string, as the Unicode
    Collation Algorithm does: primary (base letters, digits, punctuation in the CLDR
    order, case-insensitive, no numeric collation: ``a10 < a2``), secondary (accents,
    forward), tertiary (``lower < upper``). Exact for ASCII; Latin letters with
    canonical decompositions follow through ``unicodedata`` NFD, a handful of ligatures
    and stroked letters are tabled, everything else sorts after the letters by code
    point. Verified against R's ``sort()`` and ``dir()`` on every fixture directory and on
    2626 random ASCII names (see ``migration/_scratch/p1-tables-textscan-env``).
    """
    primary: list[int] = []
    secondary: list[int] = []
    tertiary: list[int] = []
    for ch in text:
        lower = ch.lower()
        upper = lower != ch
        for base in unicodedata.normalize("NFD", lower):
            if unicodedata.combining(base):
                secondary.append(_SECONDARY_MARKS.get(base, _SECONDARY_MARK_FALLBACK + ord(base)))
                continue
            if base in _LIGATURES:
                for letter in _LIGATURES[base]:
                    primary.append(_PRIMARY[letter])
                    secondary.append(_SECONDARY_COMMON)
                    tertiary.append(_TERTIARY_LIGATURE + (_TERTIARY_UPPER if upper else 0))
                continue
            if base in _STROKED:
                for letter in _STROKED[base]:
                    primary.append(_PRIMARY[letter])
                    secondary.append(_SECONDARY_STROKE)
                    tertiary.append(_TERTIARY_UPPER if upper else _TERTIARY_LOWER)
                continue
            primary.append(_PRIMARY.get(base, _PRIMARY_FALLBACK + ord(base)))
            secondary.append(_SECONDARY_COMMON)
            tertiary.append(_TERTIARY_UPPER if upper else _TERTIARY_LOWER)
    return tuple(primary), tuple(secondary), tuple(tertiary)


def r_sort(names: Iterable[str]) -> list[str]:
    """``sort()`` / ``dir()`` order of R in the sandbox and on the cluster (ICU root collation)."""
    return sorted(names, key=r_collate_key)


# ---------------------------------------------------------------------------
# Shell command lines (what /bin/sh makes of R's system() strings)
# ---------------------------------------------------------------------------


def shell_tokens(command: str) -> list[str]:
    """The words of a POSIX shell command line, the control operators as tokens of their own.

    ``shlex`` with ``punctuation_chars``: ``git log a..b | grep 'Merge pull request'`` gives
    ``git log a..b`` ``|`` ``grep`` ``Merge pull request``; a quoted argument that contains
    an operator (sbatch's ``--wrap="Rscript ...; mv ..."``) stays one word. No expansion of
    variables or globs takes place. This is what the fake binaries of the harness see as
    their ``argv`` and what the doubles record.
    """
    lexer = shlex.shlex(command, posix=True, punctuation_chars=True)
    lexer.whitespace_split = True
    return list(lexer)


def shell_segments(tokens: Sequence[str]) -> list[list[str]]:
    """The simple commands of a token list, split at ``&&``, ``||``, ``;``, ``|`` and ``&``.

    Good enough to name the tools of ``git reset --hard origin/develop && git pull``; not a
    shell parser (redirections stay in place as words).
    """
    segments: list[list[str]] = []
    current: list[str] = []
    for token in tokens:
        if token in _SHELL_OPERATORS:
            if current:
                segments.append(current)
            current = []
        else:
            current.append(token)
    if current:
        segments.append(current)
    return segments


# ---------------------------------------------------------------------------
# Effects
# ---------------------------------------------------------------------------


class Effects(abc.ABC):
    """The environment interface: filesystem, subprocesses, HTTP, clock, identity.

    Everything here mirrors one R call named in the method docstring; see the module
    docstring for the verified semantics. The read-only subset (phases 1 to 4) is abstract;
    the mutating part (phase 5) raises ``NotImplementedError`` until a subclass implements
    it, so that read-only doubles stay instantiable.
    """

    # -- filesystem (read-only) -------------------------------------------------

    @abc.abstractmethod
    def listdir_like_r(self, directory: PathLike) -> list[str]:
        """``dir(directory)``: entries without dot-files, in R's collation order; ``[]`` when not a directory."""

    @abc.abstractmethod
    def exists(self, path: PathLike) -> bool:
        """``file.exists(path)``: true for files and directories, false for a dangling symlink."""

    @abc.abstractmethod
    def is_dir(self, path: PathLike) -> bool:
        """``file.info(path)$isdir`` (false, not NA, for a missing path)."""

    @abc.abstractmethod
    def stat(self, path: PathLike) -> FileStat:
        """``file.info(path)`` mtime, ctime and size; raises ``FileNotFoundError`` for a missing path."""

    @abc.abstractmethod
    def read_text(self, path: PathLike) -> str:
        """The file's bytes decoded as UTF-8 with ``surrogateescape`` (R keeps invalid bytes as they are)."""

    @abc.abstractmethod
    def read_bytes(self, path: PathLike) -> bytes:
        """The raw bytes of a file."""

    @abc.abstractmethod
    def glob(self, pattern: PathLike) -> list[str]:
        """``Sys.glob(pattern)``: matches sorted by code point, ``*`` never matching a leading dot."""

    def listdir_all(self, directory: PathLike) -> list[str]:
        """``dir(directory, all.files = TRUE, no.. = TRUE)``: dot entries included, ``.`` and ``..`` not.

        ``list.dirs(recursive = FALSE, full.names = FALSE)`` is this list filtered by
        :meth:`is_dir`. ``[]`` when ``directory`` is not a directory.
        """
        self._unsupported("listdir_all")

    def walk(self, top: PathLike) -> Iterator[tuple[str, list[str], list[str]]]:
        """``find top -name ...``: ``os.walk`` over ``top`` (directory order, as ``find`` lists it).

        Yields ``(dirpath, dirnames, filenames)`` with ``dirpath`` starting with ``top`` as
        given (``find modules/ -name module.gms`` prints ``modules/01_x/module.gms``, which is
        ``os.path.join(dirpath, name)`` here). The order of entries is the directory's, as
        with ``find``; sort it when the order matters.
        """
        self._unsupported("walk")

    # -- filesystem (mutating) --------------------------------------------------

    def write_text(self, path: PathLike, text: str, *, append: bool = False, atomic: bool = False) -> None:
        """``writeLines(text, con = path)`` / ``cat(text, file = path)``: ``text`` as UTF-8 (``surrogateescape``).

        No newline is added: the caller composes the exact bytes (``writeLines`` adds ``\\n``
        after every element, ``write(..., append = TRUE)`` one line per element). ``append``
        opens the file like R's ``append = TRUE``; ``atomic`` writes a temporary file next to
        ``path`` and renames it over ``path`` (the mode of an existing file is kept, a new file
        gets the umask mode like ``saveRDS``). A missing directory raises ``FileNotFoundError``
        (R: ``cannot open the connection``).
        """
        self._unsupported("write_text")

    def write_bytes(self, path: PathLike, data: bytes, *, append: bool = False, atomic: bool = False) -> None:
        """``writeBin(data, path)``: the raw bytes; ``append`` and ``atomic`` as in :meth:`write_text`."""
        self._unsupported("write_bytes")

    def write_rds(self, path: PathLike, obj: object) -> None:
        """``saveRDS(obj, path)`` through :func:`modelstats.rdata_io.rds_bytes`, always atomic (plan 03 section 2.3)."""
        from modelstats.rdata_io import rds_bytes

        self.write_bytes(path, rds_bytes(obj), atomic=True)

    def copy(self, src: PathLike, dst: PathLike, overwrite: bool = False) -> bool:
        """``file.copy(src, dst, overwrite)`` for one file: ``dst`` may be a directory; the mode is copied.

        ``False`` (no error, no warning, like R) for a missing or non-file ``src``, for an
        existing ``dst`` without ``overwrite`` and for a failed copy.
        """
        self._unsupported("copy")

    def rename(self, src: PathLike, dst: PathLike) -> None:
        """``file.rename(src, dst)`` as ``rename(2)``: an existing ``dst`` file is replaced.

        Raises ``OSError`` (``FileNotFoundError`` for a missing ``src``) where R returns
        ``FALSE`` with the warning ``cannot rename file 'src' to 'dst', reason '<strerror>'``;
        the caller emits that warning with its own deparsed call.
        """
        self._unsupported("rename")

    def delete(self, path: PathLike, recursive: bool = False) -> None:
        """``unlink(path, recursive)``: a file or symlink is removed, a directory only with ``recursive``.

        Silent like R: a missing path is a success, a directory without ``recursive`` is
        left alone (R returns status 1 there, nobody reads it), and an entry that cannot be
        removed (a file in a read-only directory, a read-only subdirectory of a recursive
        delete) is left in place without a condition while every other entry is still
        attempted (R's ``R_unlink``: partial deletion, status 1, no warning or error; the
        AMT's only caller ``deleteEmptyRealizationFolders`` ignores the status, so the
        status is swallowed here). R's wildcard expansion (``expand = TRUE``) is not
        reproduced: ``path`` is taken literally.
        """
        self._unsupported("delete")

    def mkdir(self, path: PathLike, parents: bool = False) -> None:
        """``dir.create(path, recursive = parents)``.

        Raises ``FileExistsError`` / ``OSError`` where R warns (``'x' already exists``,
        ``cannot create dir 'x', reason '...'``) and returns ``FALSE``.
        """
        self._unsupported("mkdir")

    def chdir(self, path: PathLike) -> contextlib.AbstractContextManager[None]:
        """``withr::with_dir(path, ...)``: the process working directory for the ``with`` block, restored afterwards.

        A directory that cannot be entered raises ``RParityError('cannot change working
        directory')`` (``setwd``'s message), before the block runs.
        """
        self._unsupported("chdir")

    def getcwd(self) -> str:
        """``getwd()``: the current working directory."""
        self._unsupported("getcwd")

    def tempdir(self) -> str:
        """``tempdir()``: one per-process temporary directory, created on first use and removed at exit."""
        self._unsupported("tempdir")

    # -- subprocesses -----------------------------------------------------------

    @abc.abstractmethod
    def run(
        self,
        argv: Sequence[str] | str,
        cwd: PathLike | None = None,
        env: Mapping[str, str] | None = None,
        input: str | None = None,
        *,
        shell: bool = False,
        capture: bool = True,
    ) -> subprocess.CompletedProcess[str]:
        """Run a command and return its status, with the output captured (``system(cmd, intern = TRUE)``) or not.

        ``argv`` is the command and its arguments, run without a shell; with ``shell=True``
        it is a command line run through ``/bin/sh -c`` like every R ``system()`` call (see
        :meth:`run_shell`): a ``str`` as written in R, or a sequence whose elements become one
        quoted word each (``shlex.join``; an ``&&`` element is a literal argument, not an
        operator). With
        ``capture`` (``intern = TRUE``) ``stdout`` and ``stderr`` are text (UTF-8,
        ``surrogateescape``); without it (``system(cmd)``) the child inherits the process's
        own file descriptors 1 and 2 and both strings are empty. ``returncode`` is the exit
        status; an executable that cannot be found gives status 127 like ``sh``. ``env``
        holds overrides merged over the current environment; ``input`` feeds stdin.
        """

    def run_shell(
        self,
        command: str,
        cwd: PathLike | None = None,
        env: Mapping[str, str] | None = None,
        input: str | None = None,
        *,
        capture: bool = True,
    ) -> subprocess.CompletedProcess[str]:
        """``system(command)`` / ``system(command, intern = TRUE)``: the command line through ``/bin/sh -c``."""
        return self.run(command, cwd, env, input, shell=True, capture=capture)

    # -- network and time -------------------------------------------------------

    def post_json(self, url: str, payload: str | bytes) -> tuple[int, str]:
        """``curl -X POST -H 'Content-Type: application/json' -d <payload> <url>``: ``(status, body)``.

        ``payload`` is the JSON text (``json.dumps`` output; ``str`` is sent as UTF-8). The
        HTTP status and the decoded body come back for any response, a 4xx/5xx and a 3xx
        included (``curl`` without ``--location`` does not follow a redirect, so neither
        does this: one request, the original response); when no response arrives (no
        network, refused, invalid URL, timeout) the status is ``0`` and the body names the
        error. Never raises for a network condition: R's ``curl`` call only warns through
        ``system(intern = TRUE)`` and the AMT continues.
        """
        self._unsupported("post_json")

    def sleep(self, seconds: float) -> None:
        """``Sys.sleep(seconds)``."""
        self._unsupported("sleep")

    # -- clock and identity -----------------------------------------------------

    @abc.abstractmethod
    def now(self) -> dt.datetime:
        """``Sys.time()``: an aware datetime in the local time zone."""

    @abc.abstractmethod
    def today(self) -> dt.date:
        """``Sys.Date()``: the local date."""

    @property
    @abc.abstractmethod
    def user(self) -> str:
        """``Sys.info()[["user"]]``: the passwd name of the uid."""

    @property
    @abc.abstractmethod
    def on_cluster(self) -> bool:
        """``file.exists("/p")``: the cluster probe that switches getRunStatus into cluster mode."""

    @abc.abstractmethod
    def getenv(self, name: str, default: str = "") -> str:
        """``Sys.getenv(name)``: ``""`` (or ``default``) when unset."""

    def setenv(self, name: str, value: str) -> None:
        """``Sys.setenv(name = value)``: seen by :meth:`getenv` and by every later child process."""
        self._unsupported("setenv")

    def _unsupported(self, name: str) -> NoReturn:
        raise NotImplementedError(f"{type(self).__name__} does not implement {name}()")


def _expand(path: PathLike) -> str:
    """R's ``path.expand()``: a leading ``~`` or ``~user`` becomes the home directory; nothing else changes."""
    return os.path.expanduser(os.fspath(path))


def _replace_atomically(target: str, data: bytes) -> None:
    """Write ``data`` to a temporary file beside ``target`` and rename it over ``target``.

    A new file gets the umask mode (``0666 & ~umask``, what ``saveRDS`` and ``writeLines``
    create), an existing one keeps its mode; the temporary file is removed on failure.
    """
    directory = os.path.dirname(target) or "."
    base = os.path.basename(target)
    try:
        mode: int | None = _stat.S_IMODE(os.stat(target).st_mode)
    except FileNotFoundError:
        mode = None
    for _ in range(100):
        tmp = os.path.join(directory, f".{base}.{secrets.token_hex(6)}.tmp")
        try:
            fd = os.open(tmp, os.O_WRONLY | os.O_CREAT | os.O_EXCL, 0o666)
        except FileExistsError:
            continue
        break
    else:  # pragma: no cover - 100 collisions of a 48-bit random name
        raise FileExistsError(f"cannot create a temporary file beside {target}")
    try:
        with os.fdopen(fd, "wb") as handle:
            handle.write(data)
            handle.flush()
            os.fsync(handle.fileno())
        if mode is not None:
            os.chmod(tmp, mode)
        os.replace(tmp, target)
    except BaseException:
        with contextlib.suppress(OSError):
            os.unlink(tmp)
        raise


_session_tempdir: str | None = None


def _process_tempdir() -> str:
    """R's ``tempdir()``: one directory per process (under ``TMPDIR``), removed at interpreter exit."""
    global _session_tempdir
    if _session_tempdir is None or not os.path.isdir(_session_tempdir):
        _session_tempdir = tempfile.mkdtemp(prefix="modelstats-")
        atexit.register(shutil.rmtree, _session_tempdir, ignore_errors=True)
    return _session_tempdir


class ProductionEffects(Effects):
    """The real environment (every path goes through :func:`_expand` first, as R's file functions do)."""

    # -- filesystem (read-only) -------------------------------------------------

    def listdir_like_r(self, directory: PathLike) -> list[str]:
        try:
            names = os.listdir(_expand(directory))
        except FileNotFoundError, NotADirectoryError, PermissionError:
            return []
        return r_sort(name for name in names if not name.startswith("."))

    def listdir_all(self, directory: PathLike) -> list[str]:
        try:
            names = os.listdir(_expand(directory))
        except FileNotFoundError, NotADirectoryError, PermissionError:
            return []
        return r_sort(name for name in names if name not in (".", ".."))

    def walk(self, top: PathLike) -> Iterator[tuple[str, list[str], list[str]]]:
        return os.walk(_expand(top))

    def exists(self, path: PathLike) -> bool:
        return os.path.exists(_expand(path))

    def is_dir(self, path: PathLike) -> bool:
        return os.path.isdir(_expand(path))

    def stat(self, path: PathLike) -> FileStat:
        st = os.stat(_expand(path))
        return FileStat(mtime=st.st_mtime, ctime=st.st_ctime, size=st.st_size)

    def read_text(self, path: PathLike) -> str:
        return Path(_expand(path)).read_bytes().decode("utf-8", "surrogateescape")

    def read_bytes(self, path: PathLike) -> bytes:
        return Path(_expand(path)).read_bytes()

    def glob(self, pattern: PathLike) -> list[str]:
        # Sys.glob() returns the expanded paths
        return sorted(_glob.glob(_expand(pattern)))

    # -- filesystem (mutating) --------------------------------------------------

    def _write(self, path: PathLike, data: bytes, append: bool, atomic: bool) -> None:
        if append and atomic:
            raise ValueError("append and atomic are mutually exclusive")
        target = _expand(path)
        if atomic:
            _replace_atomically(target, data)
            return
        with open(target, "ab" if append else "wb") as handle:
            handle.write(data)

    def write_text(self, path: PathLike, text: str, *, append: bool = False, atomic: bool = False) -> None:
        self._write(path, text.encode("utf-8", "surrogateescape"), append, atomic)

    def write_bytes(self, path: PathLike, data: bytes, *, append: bool = False, atomic: bool = False) -> None:
        self._write(path, bytes(data), append, atomic)

    def copy(self, src: PathLike, dst: PathLike, overwrite: bool = False) -> bool:
        source = _expand(src)
        target = _expand(dst)
        if not os.path.isfile(source):
            return False
        if os.path.isdir(target):
            target = os.path.join(target, os.path.basename(source))
        if os.path.exists(target) and not overwrite:
            return False
        try:
            shutil.copy(source, target)  # copyfile + copymode, R's copy.mode = TRUE
        except OSError:
            return False
        return True

    def rename(self, src: PathLike, dst: PathLike) -> None:
        os.replace(_expand(src), _expand(dst))

    def delete(self, path: PathLike, recursive: bool = False) -> None:
        # R's unlink() never signals a failure (status 1, swallowed: see the Effects docstring); rmtree with
        # ignore_errors keeps removing the removable entries like R_unlink does
        target = _expand(path)
        if os.path.islink(target) or os.path.isfile(target):
            with contextlib.suppress(OSError):
                os.unlink(target)
        elif os.path.isdir(target) and recursive:
            shutil.rmtree(target, ignore_errors=True)

    def mkdir(self, path: PathLike, parents: bool = False) -> None:
        target = _expand(path)
        if parents:
            os.makedirs(target)
        else:
            os.mkdir(target)

    @contextlib.contextmanager
    def chdir(self, path: PathLike) -> Iterator[None]:
        previous = os.getcwd()
        try:
            os.chdir(_expand(path))
        except OSError as exc:
            raise RParityError("cannot change working directory", call="setwd(dir)") from exc
        try:
            yield
        finally:
            os.chdir(previous)

    def getcwd(self) -> str:
        return os.getcwd()

    def tempdir(self) -> str:
        return _process_tempdir()

    # -- subprocesses -----------------------------------------------------------

    def run(
        self,
        argv: Sequence[str] | str,
        cwd: PathLike | None = None,
        env: Mapping[str, str] | None = None,
        input: str | None = None,
        *,
        shell: bool = False,
        capture: bool = True,
    ) -> subprocess.CompletedProcess[str]:
        args: list[str] | str
        if shell:
            args = argv if isinstance(argv, str) else shlex.join(argv)
        elif isinstance(argv, str):
            raise TypeError("run() takes an argv sequence; use shell=True (or run_shell) for a command line")
        else:
            args = list(argv)
            if not args:
                raise ValueError("run() needs a command")
        full_env = None if env is None else {**os.environ, **env}
        if not capture:
            # system(cmd) without intern: the child writes to the process's own stdout and stderr,
            # which neither R's sink() nor a Python-level sys.stdout redirection intercept
            for stream in (sys.stdout, sys.stderr):
                with contextlib.suppress(Exception):
                    stream.flush()
        try:
            proc = subprocess.run(
                args,
                shell=shell,
                cwd=None if cwd is None else _expand(cwd),  # system() runs through a shell, which expands ~
                env=full_env,
                input=input,
                stdout=subprocess.PIPE if capture else None,
                stderr=subprocess.PIPE if capture else None,
                text=True,
                encoding="utf-8",
                errors="surrogateescape",
                check=False,
            )
        except FileNotFoundError:
            if cwd is not None and not os.path.isdir(_expand(cwd)):
                raise  # the working directory is missing, not the executable
            command = args if isinstance(args, str) else args[0]
            return subprocess.CompletedProcess(args, 127, "", f"sh: {command}: command not found\n")
        if not capture:
            return subprocess.CompletedProcess(proc.args, proc.returncode, "", "")
        return proc

    # -- network and time -------------------------------------------------------

    def post_json(self, url: str, payload: str | bytes) -> tuple[int, str]:
        data = payload.encode("utf-8") if isinstance(payload, str) else bytes(payload)
        try:
            request = urllib.request.Request(
                url, data=data, headers={"Content-Type": "application/json"}, method="POST"
            )
            with _OPENER.open(request, timeout=HTTP_TIMEOUT) as response:
                return int(response.status), response.read().decode("utf-8", "replace")
        except urllib.error.HTTPError as exc:
            try:
                body = exc.read().decode("utf-8", "replace")
            except OSError:
                body = ""
            return int(exc.code), body
        except (urllib.error.URLError, OSError, ValueError) as exc:
            return 0, f"{type(exc).__name__}: {exc}"

    def sleep(self, seconds: float) -> None:
        time.sleep(seconds)

    # -- clock and identity -----------------------------------------------------

    def now(self) -> dt.datetime:
        return dt.datetime.now().astimezone()

    def today(self) -> dt.date:
        return self.now().date()

    @property
    def user(self) -> str:
        try:
            return pwd.getpwuid(os.getuid()).pw_name
        except KeyError:
            return "unknown"

    @property
    def on_cluster(self) -> bool:
        return Path("/p").exists()

    def getenv(self, name: str, default: str = "") -> str:
        return os.environ.get(name, default)

    def setenv(self, name: str, value: str) -> None:
        os.environ[name] = value


# ---------------------------------------------------------------------------
# Dry run
# ---------------------------------------------------------------------------


@dataclass(frozen=True)
class DryRunEvent:
    """One mutation a dry run did not perform: ``would <action> <target> (<detail>)``.

    ``data`` keeps the bytes a write would have produced (the README, a state file), so
    that a dry run can still show what it would have written.
    """

    action: str
    target: str
    detail: str = ""
    data: bytes | None = field(default=None, repr=False, compare=False)

    def __str__(self) -> str:
        text = f"would {self.action} {self.target}"
        return f"{text} ({self.detail})" if self.detail else text


def dry_run_answer(tokens: Sequence[str]) -> tuple[str, int]:
    """The synthetic ``(stdout, status)`` a dry run gives a command it does not execute.

    ``git log -1`` (any segment of a shell command line) answers a fixed commit so that
    the commit-dependent steps of evaluateRuns still run; everything else, ``squeue``
    (an empty scheduler), ``sacct`` and ``git log --merges`` included, answers nothing
    with status 0.
    """
    for segment in shell_segments(tokens):
        if len(segment) >= 2 and os.path.basename(segment[0]) == "git" and segment[1] == "log" and "-1" in segment:
            return (
                f"commit {DRY_RUN_COMMIT}\nAuthor: dry run <modelstats@localhost>\n\n    dry run: no commit was read\n",
                0,
            )
    return "", 0


class DryRunEffects(ProductionEffects):
    """``modeltests --dry-run``: reads execute for real, every mutation is logged and answered synthetically.

    Nothing below the model directory, the state directory or the temporary directory's
    files changes, no subprocess runs (``run`` answers through :func:`dry_run_answer` or
    the ``answer`` callable, which gets the shell tokens and may return ``(stdout,
    status)`` or ``None`` for the default), no notification is sent (``post_json`` answers
    ``200``), no time passes in ``sleep``. ``chdir`` is performed for real (reads need it);
    ``setenv`` is applied for real (it only affects this process). Every event is kept in
    :attr:`events` (``log`` gives the lines) and handed to ``report`` as it happens.
    """

    def __init__(
        self,
        *,
        report: Callable[[str], None] | None = None,
        answer: Callable[[Sequence[str]], tuple[str, int] | None] | None = None,
    ) -> None:
        self.events: list[DryRunEvent] = []
        self._report = report
        self._answer = answer

    @property
    def log(self) -> list[str]:
        """The ``would ...`` lines, in order."""
        return [str(event) for event in self.events]

    def _would(self, action: str, target: str, detail: str = "", data: bytes | None = None) -> None:
        event = DryRunEvent(action, target, detail, data)
        self.events.append(event)
        if self._report is not None:
            self._report(str(event))

    # -- mutations: logged, not performed ----------------------------------------

    def write_text(self, path: PathLike, text: str, *, append: bool = False, atomic: bool = False) -> None:
        self.write_bytes(path, text.encode("utf-8", "surrogateescape"), append=append, atomic=atomic)

    def write_bytes(self, path: PathLike, data: bytes, *, append: bool = False, atomic: bool = False) -> None:
        if append and atomic:
            raise ValueError("append and atomic are mutually exclusive")
        detail = f"{len(data)} bytes" + (", append" if append else "") + (", atomic" if atomic else "")
        self._would("write", os.fspath(path), detail, bytes(data))

    def write_rds(self, path: PathLike, obj: object) -> None:
        from modelstats.rdata_io import rds_bytes

        data = rds_bytes(obj)
        self._would("write", os.fspath(path), f"RDS, {len(data)} bytes", data)

    def copy(self, src: PathLike, dst: PathLike, overwrite: bool = False) -> bool:
        self._would("copy", f"{os.fspath(src)} -> {os.fspath(dst)}", "overwrite" if overwrite else "")
        return True

    def rename(self, src: PathLike, dst: PathLike) -> None:
        self._would("rename", f"{os.fspath(src)} -> {os.fspath(dst)}")

    def delete(self, path: PathLike, recursive: bool = False) -> None:
        self._would("delete", os.fspath(path), "recursive" if recursive else "")

    def mkdir(self, path: PathLike, parents: bool = False) -> None:
        self._would("create directory", os.fspath(path), "parents" if parents else "")

    def setenv(self, name: str, value: str) -> None:
        self._would("set", f"{name}={value}")
        super().setenv(name, value)

    def run(
        self,
        argv: Sequence[str] | str,
        cwd: PathLike | None = None,
        env: Mapping[str, str] | None = None,
        input: str | None = None,
        *,
        shell: bool = False,
        capture: bool = True,
    ) -> subprocess.CompletedProcess[str]:
        args: list[str] | str
        if shell:
            args = argv if isinstance(argv, str) else shlex.join(argv)
            tokens = shell_tokens(args)
        elif isinstance(argv, str):
            raise TypeError("run() takes an argv sequence; use shell=True (or run_shell) for a command line")
        else:
            args = list(argv)
            tokens = list(argv)
        where = os.getcwd() if cwd is None else os.path.abspath(_expand(cwd))
        answer = self._answer(tokens) if self._answer is not None else None
        stdout, status = dry_run_answer(tokens) if answer is None else answer
        command = args if isinstance(args, str) else shlex.join(args)
        self._would("run", command, f"cwd {where}, answered status {status}")
        return subprocess.CompletedProcess(args, status, stdout if capture else "", "")

    def post_json(self, url: str, payload: str | bytes) -> tuple[int, str]:
        data = payload.encode("utf-8") if isinstance(payload, str) else bytes(payload)
        self._would("POST", url, f"{len(data)} bytes", data)
        return 200, "dry run: nothing was sent"

    def sleep(self, seconds: float) -> None:
        self._would("sleep", f"{seconds:g} s")


# ---------------------------------------------------------------------------
# Default
# ---------------------------------------------------------------------------

_default: ProductionEffects | None = None


def default_effects() -> ProductionEffects:
    """The process-wide ProductionEffects every public function falls back to."""
    global _default
    if _default is None:
        _default = ProductionEffects()
    return _default
