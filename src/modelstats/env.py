"""Effects: the only path from modelstats to its environment (plan 03 sections 2.3 and 5).

Every public function of the package takes ``effects: Effects | None = None`` and
falls back to :func:`default_effects`. ``Effects`` is the interface, ``ProductionEffects``
does the real thing, and the test double ``FakeEffects`` (``tests/unit/_fake_effects.py``,
not part of the package) answers the clock, the user, the cluster probe and ``squeue`` /
``sacct`` from canned data while filesystem reads go to the real path.

This module holds the read-only subset used by ``rs`` (phases 1 to 4). Phase 5 adds the
mutating part (``write_text``, ``copy``, ``rename``, ``delete``, ``mkdir``, ``chdir``,
``post_json``, ``sleep``) and the recording / dry-run doubles; the class is deliberately
open (no slots, no finality) for that.

R facts reproduced here (verified with R 4.6.1 on 2026-10-01, see the unit tests):

- ``dir()`` lists files and directories alike, skips dot-files, returns ``character(0)``
  for a missing path or a file, and sorts with the collation of the session. R uses ICU
  (``icuGetCollate()`` is ``"root"``) in every locale except plain ``C``/``POSIX``,
  including the sandbox's ``LC_ALL=C.utf8``, so the goldens carry ICU root order:
  ``_x | a | ä | A1 | a10 | a2 | b | B`` rather than code-point order. :func:`r_sort`
  reproduces that order (see its docstring for the extent of the emulation).
- ``file.exists()`` follows symlinks (a dangling one is ``FALSE``), ``Sys.glob()`` returns
  the matches sorted by code point (``glob(3)`` under ``C.utf8``) and never matches a
  leading dot with ``*``.
- ``Sys.Date()`` is the date in the local time zone; ``Sys.time()`` is time-zone aware.
- ``Sys.info()[["user"]]`` is the passwd name of the uid (``"unknown"`` when the uid has
  no passwd entry); ``Sys.getenv("X")`` is ``""`` when unset.
- ``system(cmd, intern = TRUE)`` returns the stdout lines (see
  :func:`modelstats.textscan.r_intern_lines` for the exact line semantics) with the exit
  status as an attribute; a command that cannot be run (status 127) raises
  ``error in running command``. :meth:`Effects.run` returns the status instead and leaves
  that error to the caller.
"""

from __future__ import annotations

import abc
import datetime as dt
import glob as _glob
import os
import pwd
import subprocess
import unicodedata
from collections.abc import Iterable, Mapping, Sequence
from pathlib import Path
from typing import NamedTuple

__all__ = [
    "Effects",
    "FileStat",
    "PathLike",
    "ProductionEffects",
    "default_effects",
    "r_collate_key",
    "r_sort",
]

type PathLike = str | os.PathLike[str]


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
# Effects
# ---------------------------------------------------------------------------


class Effects(abc.ABC):
    """The environment interface: filesystem reads, subprocesses, clock, identity.

    Read-only subset (phases 1 to 4). Everything here mirrors one R call named in the
    method docstring; see the module docstring for the verified semantics.
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

    # -- subprocesses -----------------------------------------------------------

    @abc.abstractmethod
    def run(
        self,
        argv: Sequence[str],
        cwd: PathLike | None = None,
        env: Mapping[str, str] | None = None,
        input: str | None = None,
    ) -> subprocess.CompletedProcess[str]:
        """Run ``argv`` without a shell and capture its output (``system(cmd, intern = TRUE)``).

        ``stdout`` and ``stderr`` are text (UTF-8, ``surrogateescape``), ``returncode`` the
        exit status; an executable that cannot be found gives status 127 like ``sh``.
        ``env`` holds overrides merged over the current environment.
        """

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


class ProductionEffects(Effects):
    """The real environment."""

    def listdir_like_r(self, directory: PathLike) -> list[str]:
        try:
            names = os.listdir(directory)
        except FileNotFoundError, NotADirectoryError, PermissionError:
            return []
        return r_sort(name for name in names if not name.startswith("."))

    def exists(self, path: PathLike) -> bool:
        return os.path.exists(path)

    def is_dir(self, path: PathLike) -> bool:
        return os.path.isdir(path)

    def stat(self, path: PathLike) -> FileStat:
        st = os.stat(path)
        return FileStat(mtime=st.st_mtime, ctime=st.st_ctime, size=st.st_size)

    def read_text(self, path: PathLike) -> str:
        return Path(path).read_bytes().decode("utf-8", "surrogateescape")

    def read_bytes(self, path: PathLike) -> bytes:
        return Path(path).read_bytes()

    def glob(self, pattern: PathLike) -> list[str]:
        return sorted(_glob.glob(os.fspath(pattern)))

    def run(
        self,
        argv: Sequence[str],
        cwd: PathLike | None = None,
        env: Mapping[str, str] | None = None,
        input: str | None = None,
    ) -> subprocess.CompletedProcess[str]:
        argv = list(argv)
        full_env = None if env is None else {**os.environ, **env}
        try:
            return subprocess.run(
                argv,
                cwd=cwd,
                env=full_env,
                input=input,
                capture_output=True,
                text=True,
                encoding="utf-8",
                errors="surrogateescape",
                check=False,
            )
        except FileNotFoundError:
            command = argv[0] if argv else ""
            return subprocess.CompletedProcess(argv, 127, "", f"sh: {command}: command not found\n")

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


_default: ProductionEffects | None = None


def default_effects() -> ProductionEffects:
    """The process-wide ProductionEffects every public function falls back to."""
    global _default
    if _default is None:
        _default = ProductionEffects()
    return _default
