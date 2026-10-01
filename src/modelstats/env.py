"""``Effects``: the only door to the environment (``migration/03-migration-plan.md`` sections 2.3 and 5).

Everything the R package reads from outside its own process goes through one injectable object: the
filesystem below the run folders, ``squeue`` and ``sacct``, the clock, the user name, the ``/p`` probe
and the environment variables. ``ProductionEffects`` maps the calls to the real system; ``FakeEffects``
is the test double. This phase defines the read-only subset ``rs`` needs; phase 5 (the AMT rewrite,
``migration/06-user-requests.md`` section 7) adds the mutating part (write, copy, rename, delete,
``post_json``, ``sleep``) and the recording and dry-run variants in this same module.

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
* ``file.info()`` (``R/getRunStatus.R:30, 61, 114, 126, 181-183``): size, isdir, mtime and ctime, a
  missing path giving ``NA``.
* ``normalizePath()`` (``R/getRunStatus.R:25``, ``R/foundInSlurm.R:13``): the real absolute path of an
  existing file, the input unchanged otherwise.
* ``system("squeue ...", intern = TRUE)`` and ``sacct`` (``R/foundInSlurm.R:19``,
  ``R/commandLineInterface.R:141-147``): stdout as lines, the exit status kept, no exception.
* ``Sys.time()`` / ``Sys.Date()`` (``R/getRunStatus.R:181, 183, 313``, ``R/commandLineInterface.R:145``):
  an aware datetime in the session's time zone (``TZ``).
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

import glob as globmodule
import os
import pwd
import re
import shlex
import stat as statmodule
import subprocess
import unicodedata
from collections.abc import Iterable, Mapping, Sequence
from dataclasses import dataclass
from datetime import datetime, tzinfo
from enum import Enum
from pathlib import Path, PurePosixPath
from typing import BinaryIO, Protocol
from zoneinfo import ZoneInfo, ZoneInfoNotFoundError

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
    resolution), as R prints them.
    """

    size: int
    is_dir: bool
    mtime_epoch: float
    ctime_epoch: float
    mtime: datetime
    ctime: datetime


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
        """The stdout lines, as ``intern = TRUE`` returns them: no newline, ``character(0)`` when empty."""
        return self.stdout.splitlines()


class CommandFailedError(RuntimeError):
    """A subprocess run with ``check=True`` exited with a non-zero status."""

    def __init__(self, result: RunResult) -> None:
        """Carry the failed ``RunResult`` in ``result``."""
        super().__init__(f"{shlex.join(result.argv)} exited with status {result.returncode}")
        self.result = result


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


# ---------------------------------------------------------------------------------------------
# The protocol
# ---------------------------------------------------------------------------------------------


class Effects(Protocol):
    """The read-only subset of the environment that ``rs`` needs (``03`` section 5, "Effects").

    Phase 5 extends this protocol with the mutating calls of the AMT. Implementations: ``ProductionEffects``
    and ``FakeEffects``.
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

    def exists(self, path: PathLike) -> bool:
        """``file.exists(path)`` (symlinks followed)."""
        ...

    def is_dir(self, path: PathLike) -> bool:
        """``dir.exists(path)`` / ``file.info(path)$isdir`` for an existing path."""
        ...

    def stat(self, path: PathLike) -> FileInfo | None:
        """``file.info(path)``; ``None`` where R reports an ``NA`` row (missing path)."""
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

    def run(
        self,
        argv: Sequence[str],
        *,
        cwd: PathLike | None = None,
        env: Mapping[str, str] | None = None,
        check: bool = False,
    ) -> RunResult:
        """Run a program (``squeue``, ``sacct``) and capture both streams; ``check`` raises on failure.

        ``env`` overrides single variables on top of the inherited environment. A missing program answers
        with status 127 and a ``command not found`` line on stderr, as ``sh`` does for ``system()``.
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


def _not_found_result(argv: Sequence[str]) -> RunResult:
    """What ``sh -c`` reports for a program that is not on the PATH (status 127)."""
    return RunResult(tuple(argv), 127, "", f"{argv[0]}: command not found\n")


# ---------------------------------------------------------------------------------------------
# Production
# ---------------------------------------------------------------------------------------------


class ProductionEffects:
    """The real environment: the process's filesystem, PATH, clock, uid and variables."""

    def __init__(self, environ: Mapping[str, str] | None = None) -> None:
        """Bind to ``os.environ`` or to an explicit variable mapping (used for the time zone and ``run``)."""
        self._environ: Mapping[str, str] = os.environ if environ is None else environ
        self._zone = _local_timezone(self._environ)
        self._cluster_paths = ClusterPaths()

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

    def exists(self, path: PathLike) -> bool:
        """``file.exists(path)``."""
        return os.path.exists(path)

    def is_dir(self, path: PathLike) -> bool:
        """``dir.exists(path)``."""
        return os.path.isdir(path)

    def stat(self, path: PathLike) -> FileInfo | None:
        """``file.info(path)``, ``None`` for a missing path."""
        try:
            return _file_info(os.stat(path), self._zone)
        except OSError:
            return None

    def listdir_like_r(
        self, path: PathLike, *, pattern: str | None = None, full_names: bool = False
    ) -> list[str]:
        """``dir(path, pattern, full.names)``."""
        try:
            names = os.listdir(path)
        except OSError:
            return []
        selected = _select_names(names, pattern)
        return [_join_like_r(os.fspath(path), n) for n in selected] if full_names else selected

    def list_dirs_like_r(self, path: PathLike, *, full_names: bool = True) -> list[str]:
        """``list.dirs(path, recursive = FALSE, full.names)``."""
        try:
            names = os.listdir(path)
        except OSError:
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

    def run(
        self,
        argv: Sequence[str],
        *,
        cwd: PathLike | None = None,
        env: Mapping[str, str] | None = None,
        check: bool = False,
    ) -> RunResult:
        """``subprocess.run`` with both streams captured as text; see ``Effects.run``."""
        merged = {**self._environ, **(env or {})}
        try:
            completed = subprocess.run(
                list(argv),
                cwd=cwd,
                env=merged,
                capture_output=True,
                text=True,
                encoding="utf-8",
                errors="replace",
                check=False,
            )
        except FileNotFoundError:
            result = _not_found_result(argv)
        else:
            result = RunResult(tuple(argv), completed.returncode, completed.stdout, completed.stderr)
        if check and not result.ok:
            raise CommandFailedError(result)
        return result


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
        non-directory ends the walk, hiding everything below. ``(None, ())`` when nothing is shown, ``(entry,
        ())`` for a file, ``(entry, merged directories)`` for a directory. Outside the logical root the real
        filesystem answers.
        """
        if not logical.is_relative_to(self.logical_root):
            physical = Path(logical)
            if not os.path.exists(physical):
                return (None, ())
            return (physical, (physical,) if os.path.isdir(physical) else ())
        parts = logical.relative_to(self.logical_root).parts
        shown: Path | None = self.layers[0] if self.layers else None
        merged: tuple[Path, ...] = self.layers
        for index, part in enumerate(parts):
            holders = [directory / part for directory in merged if os.path.lexists(directory / part)]
            if not holders:
                return (None, ())
            shown = holders[0]
            if not os.path.isdir(shown):
                return (shown, ()) if index == len(parts) - 1 else (None, ())
            merged = _merged_run(holders)
        return (shown, merged)

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
    """One logged ``FakeEffects`` call: the method, its stringified arguments and, for ``run``, the status."""

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
        if not case_dir.is_dir():
            raise FileNotFoundError(f"slurm case directory not found: {case_dir}")
        files = {
            entry.name: entry.read_text(encoding="utf-8", errors="replace")
            for entry in sorted(case_dir.iterdir())
            if entry.is_file()
        }
        return cls(files)

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
    ``NotImplementedError`` so that a stray shell-out fails loudly (phase 5 adds the AMT's programs).
    ``cwd`` is a logical directory (the fixture root by default). ``environ`` backs ``getenv``. Every
    call is appended to ``calls``.
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

    def stat(self, path: PathLike) -> FileInfo | None:
        """``file.info`` of the highest layer's entry, times in the frozen clock's zone."""
        self._log("stat", path)
        physical = self._physical(path)
        if physical is None:
            return None
        try:
            return _file_info(os.stat(physical), self._zone())
        except OSError:
            return None

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

    # -- programs --------------------------------------------------------------------------------

    def run(
        self,
        argv: Sequence[str],
        *,
        cwd: PathLike | None = None,
        env: Mapping[str, str] | None = None,
        check: bool = False,
    ) -> RunResult:
        """Answer ``squeue``/``sacct`` from the canned case; any other program is an error."""
        if not argv:
            raise ValueError("run() needs a program")
        tool = os.path.basename(argv[0])
        if tool not in _CANNED_TOOLS:
            self._log("run", *argv)
            raise NotImplementedError(f"FakeEffects has no canned answer for {tool!r} (argv {list(argv)})")
        result = self._slurm.answer(tool, argv[1:])
        self.calls.append(EffectCall("run", tuple(argv), result.returncode))
        if check and not result.ok:
            raise CommandFailedError(result)
        return result

    # -- internals -------------------------------------------------------------------------------

    def _log(self, name: str, *args: PathLike) -> None:
        self.calls.append(EffectCall(name, tuple(os.fspath(a) for a in args)))

    def _zone(self) -> tzinfo:
        zone = self._now.tzinfo
        assert zone is not None  # checked in __init__
        return zone

    def _absolute(self, path: PathLike) -> PurePosixPath:
        """Resolve a relative path against the logical cwd (no symlink or ``..`` processing)."""
        return self._cwd / PurePosixPath(os.fspath(path))

    def _physical(self, path: PathLike) -> Path | None:
        """The physical path the overlay shows, or ``None``."""
        logical = self._absolute(path)
        if self._paths is None:
            physical = Path(logical)
            return physical if os.path.exists(physical) else None
        return self._paths.first_existing(logical)

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


__all__ = [
    "MAGPIE_RESULTS_ARCHIVE_VARIABLE",
    "CannedSlurm",
    "ClusterPaths",
    "CommandFailedError",
    "EffectCall",
    "Effects",
    "FakeEffects",
    "FileInfo",
    "Mode",
    "OverlayPaths",
    "PathLike",
    "ProductionEffects",
    "RunResult",
    "r_collation_key",
    "sort_like_r",
]
