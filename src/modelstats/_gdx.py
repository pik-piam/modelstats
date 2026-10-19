"""GDX files: scalars, vectors and dense parameter records.

R source: ``gdx2::readGDX(..., react = "silent")``, ``quitte::as.quitte()`` and the magclass
subsetting ``p80_repy[, , "modelstat"]`` in getRunStatus.R.

Semantics kept from the R side: an absent symbol is ``None`` (never an exception); symbol names
are case-insensitive; ``read_first_found`` returns *all* values of the first symbol that exists,
dense over its domain sets like ``read_records``, which is a long frame over the cartesian product
of the domain sets (``0`` where the GDX stores no record), its columns named after them; special
values are ``EPS -> 0.0``, ``INF -> inf``, ``NA``/``UNDEF -> nan``. A corrupt, truncated or
non-GDX file raises ``GdxReadError``.

The GAMS libraries are not safe on damaged files: ``gams.transfer`` segfaults on some, and the
low-level ``gdxOpenRead`` reads a garbage record count from a truncated file and allocates memory
until the machine is out of it (BUG-042). Every read therefore runs in one worker process
(``_gdx_worker``) whose address space is capped (``WORKER_MEMORY``) and which is killed and
replaced when it dies or hangs (``WORKER_TIMEOUT``); the parent never loads the GAMS libraries
itself, and a file that failed once is not read again until it changes.
"""

from __future__ import annotations

import itertools
import os
import pickle
import select
import subprocess
import sys
import time
from collections.abc import Generator, Sequence
from contextlib import contextmanager
from pathlib import Path
from typing import IO, Any

import pandas as pd

from modelstats import _gdx_worker
from modelstats._gdx_worker import write_frame
from modelstats.errors import GdxReadError

__all__ = [
    "WORKER_MEMORY",
    "WORKER_TIMEOUT",
    "GdxReadError",
    "read_first_found",
    "read_records",
    "read_scalar",
]

WORKER_MEMORY = 2 * 1024**3
"""Bytes of address space the GDX worker process may use (a damaged file cannot take more)."""
WORKER_TIMEOUT = 300.0
"""Seconds one read may take before the worker is replaced."""

_UNIVERSE = "*"
_GDX_MAGIC = (17, bytes([123, 7]) + b"GAMSGDX")  # offset of the header byte 123 and the id string
_WORKER_BOOTSTRAP = (
    "import resource, sys\n"
    "limit = int(sys.argv[1])\n"
    "soft, hard = resource.getrlimit(resource.RLIMIT_AS)\n"
    "cap = limit if hard == resource.RLIM_INFINITY else min(limit, hard)\n"
    "resource.setrlimit(resource.RLIMIT_AS, (cap, hard))\n"
    "from modelstats._gdx_worker import main\n"
    "main([])\n"
)


# --- functions that run inside the worker process ---------------------------------------------


@contextmanager
def _open(path: Path) -> Generator[Any]:
    """A read handle on a GDX file, or ``GdxReadError``."""
    import gamspy_base
    from gams.core import gdx as gdxcc

    handle = gdxcc.new_gdxHandle_tp()
    created, message = gdxcc.gdxCreateD(handle, gamspy_base.directory, 256)
    if not created:
        raise GdxReadError(f"GDX library not available: {message}")
    try:
        opened, error = gdxcc.gdxOpenRead(handle, str(path))
        if not opened:
            raise GdxReadError(f"{path}: {gdxcc.gdxErrorStr(handle, error)[1]}")
        try:
            yield handle
        finally:
            gdxcc.gdxClose(handle)
    finally:
        gdxcc.gdxFree(handle)


def _symbol_names(path: Path, names: Sequence[str]) -> dict[str, str]:
    """The symbols among ``names`` that exist in the file: requested name -> name as stored."""
    from gams.core import gdx as gdxcc

    with _open(path) as handle:
        found: dict[str, str] = {}
        for name in names:
            exists, number = gdxcc.gdxFindSymbol(handle, name)
            if exists:
                found[name] = gdxcc.gdxSymbolInfo(handle, number)[1]
        return found


def _read_symbols(path: Path, names: Sequence[str]) -> Any:
    """A ``gams.transfer`` container holding the named symbols."""
    import gams.transfer as gt
    import gamspy_base

    container = gt.Container(system_directory=gamspy_base.directory)
    try:
        container.read(str(path), symbols=list(names))
    except (ValueError, RuntimeError, OSError) as error:
        raise GdxReadError(f"{path}: {error}") from error
    return container


def _values(records: pd.DataFrame | None) -> list[float]:
    if records is None or "value" not in records:
        return []
    return [float(value) + 0.0 for value in records["value"]]  # + 0.0 turns EPS (-0.0) into 0.0


def _scalar_in_worker(path: Path, name: str) -> float | None:
    stored = _symbol_names(path, [name]).get(name)
    if stored is None:
        return None
    values = _values(_read_symbols(path, [stored])[stored].records)
    return values[0] if values else 0.0


def _first_found_in_worker(path: Path, names: Sequence[str]) -> list[float] | None:
    found = _symbol_names(path, names)
    for name in names:
        if name in found:
            return _values(_dense_records(path, found[name]))
    return None


def _records_in_worker(path: Path, name: str) -> pd.DataFrame | None:
    stored = _symbol_names(path, [name]).get(name)
    if stored is None:
        return None
    return _dense_records(path, stored)


def _dense_records(path: Path, stored: str) -> pd.DataFrame:
    """The records of a symbol over the cartesian product of its domain sets (``restore_zeros``)."""
    container = _read_symbols(path, [stored])
    symbol = container[stored]
    domains = [str(d) for d in symbol.domain_names]
    records = symbol.records
    columns = [*domains, "value"]
    if records is None and not domains:
        records = pd.DataFrame({"value": [0.0]})  # a scalar without record is 0 in GAMS
    elif records is None:
        records = pd.DataFrame(columns=columns)
    else:
        records = records.copy()
        records.columns = columns[: len(records.columns)]
    records["value"] = [float(v) + 0.0 for v in records["value"]]
    for column in domains:
        records[column] = records[column].astype(str)
    if not domains or _UNIVERSE in domains:
        return records
    sets = _symbol_names(path, domains)
    if len(sets) != len(domains):
        return records
    set_container = _read_symbols(path, [sets[d] for d in domains])
    elements = []
    for domain in domains:
        set_records = set_container[sets[domain]].records
        elements.append([] if set_records is None else [str(e) for e in set_records.iloc[:, 0]])
    dense = pd.DataFrame(list(itertools.product(*elements)), columns=domains)
    merged = dense.merge(records, how="left", on=domains, indicator=True)
    merged.loc[merged["_merge"] == "left_only", "value"] = 0.0  # no record: 0 (an NA record stays nan)
    return merged.drop(columns="_merge")


# --- the parent side ----------------------------------------------------------------------------


def _file_key(path: Path) -> tuple[str, int, int] | None:
    try:
        stat = path.stat()
    except OSError:
        return None
    return (str(path), stat.st_size, stat.st_mtime_ns)


class _Worker:
    """One ``_gdx_worker`` process that holds the GAMS libraries; replaced when it dies or hangs.

    Files that could not be read are remembered by path, size and modification time, so that a
    damaged file costs one failed read, not one per symbol.
    """

    def __init__(self) -> None:
        self._process: subprocess.Popen[bytes] | None = None
        self._failed: dict[tuple[str, int, int], str] = {}

    def _start(self) -> subprocess.Popen[bytes]:
        env = {**os.environ, "OPENBLAS_NUM_THREADS": "1", "OMP_NUM_THREADS": "1", "MKL_NUM_THREADS": "1"}
        # -P keeps the working directory (a run folder of another user) off the worker's import path;
        # the bootstrap caps the address space before the package and its libraries are imported
        self._process = subprocess.Popen(
            [sys.executable, "-P", "-c", _WORKER_BOOTSTRAP, str(WORKER_MEMORY)],
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=subprocess.DEVNULL,
            env=env,
            cwd=Path("/"),
        )
        return self._process

    def call(self, function: str, path: Path, *args: Any) -> Any:
        key = _file_key(path)
        if key in self._failed:
            raise GdxReadError(self._failed[key])
        process = (
            self._process if self._process is not None and self._process.poll() is None else self._start()
        )
        assert process.stdin is not None and process.stdout is not None
        try:
            write_frame(process.stdin, pickle.dumps((function, (path, *args))))
            frame = _read_frame_within(process.stdout, WORKER_TIMEOUT)
        except (OSError, EOFError, TimeoutError) as error:
            what = "hung" if isinstance(error, TimeoutError) else "crashed"
            self.shutdown()
            message = f"{path}: the GDX reader {what}"
            if key is not None:
                self._failed[key] = message
            raise GdxReadError(message) from error
        status, payload = pickle.loads(frame)  # written by our worker
        if status == "error":
            if key is not None:
                self._failed[key] = str(payload)
            raise payload
        return payload

    def shutdown(self) -> None:
        """Kill the worker (if any); the next read starts a fresh one."""
        if self._process is not None:
            self._process.kill()
            self._process.wait()
            self._process = None

    def forget_failures(self) -> None:
        self._failed.clear()


def _read_frame_within(stream: IO[bytes], timeout: float) -> bytes:
    """One frame from the worker, or ``TimeoutError``/``EOFError``."""
    deadline = time.monotonic() + timeout
    header = _read_exact(stream, _gdx_worker._HEADER.size, deadline)
    (length,) = _gdx_worker._HEADER.unpack(header)
    return _read_exact(stream, length, deadline)


def _read_exact(stream: IO[bytes], count: int, deadline: float) -> bytes:
    chunks = bytearray()
    while len(chunks) < count:
        remaining = deadline - time.monotonic()
        if remaining <= 0 or not select.select([stream], [], [], remaining)[0]:
            raise TimeoutError
        chunk = os.read(stream.fileno(), count - len(chunks))
        if not chunk:
            raise EOFError
        chunks += chunk
    return bytes(chunks)


_worker = _Worker()


def _check_magic(path: Path) -> None:
    """Reject missing or non-GDX files before a worker is spawned for them."""
    offset, magic = _GDX_MAGIC
    try:
        with path.open("rb") as handle:
            head = handle.read(offset + len(magic))
    except OSError as error:
        raise GdxReadError(f"{path}: {error.strerror}") from error
    if head[offset:] != magic:
        raise GdxReadError(f"{path}: not a GDX file")


def read_scalar(path: Path, name: str) -> float | None:
    """The value of a scalar symbol (``o_iterationNumber``, ``s80_bool``); ``None`` when absent.

    A multi-valued symbol yields its first record; a scalar without record is ``0``.
    """
    _check_magic(path)
    return _worker.call(_scalar_in_worker.__name__, path, name)


def read_first_found(path: Path, names: Sequence[str]) -> list[float] | None:
    """All values of the first symbol of ``names`` that exists (``format = "first_found"``)."""
    _check_magic(path)
    return _worker.call(_first_found_in_worker.__name__, path, list(names))


def read_records(path: Path, name: str) -> pd.DataFrame | None:
    """The records of a parameter as a long frame, dense over its domain sets; ``None`` when absent."""
    _check_magic(path)
    return _worker.call(_records_in_worker.__name__, path, name)
