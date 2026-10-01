"""GDX reading with the semantics of ``gdx2::readGDX`` as used by ``R/getRunStatus.R``.

The R package reads seven status symbols from a run's ``fulldata.gdx``, ``non_optimal.gdx``
or ``abort.gdx`` (``getRunStatus.R`` lines 57, 84, 212-213, 280-288): ``o_iterationNumber``,
``s80_bool``, ``o_modelstat`` / ``p80_modelstat`` (first found), ``p80_repy``,
``cm_abortOnConsecFail`` and ``p80_trackConsecFail``. ``gdx2::readGDX`` returns magclass arrays
that are *dense*: GDX files store non-zero records only and ``readGDX`` re-indexes them onto
the cartesian product of the declared domain sets, in the order of the sets inside the file,
with ``0`` for absent records (``restoreZeros = TRUE``). This module reproduces that
(``02-python-libraries.md`` section 4.1):

* an absent symbol is ``None`` (R ``react = "silent"`` returns ``NULL``);
* a present symbol without records is dense zeros (a 0-d scalar reads as ``0.0``);
* elements are ordered as the domain set lists them in the GDX file, never sorted (magclass
  keeps the order, also for year-like elements, verified against R);
* a domain given as ``*`` (or naming no set of the file) disables the dense reconstruction, as
  in R: the frame then holds the observed elements in record order and missing combinations
  are GAMS NA (R ``NA``, see :func:`is_na`);
* GAMS special values map as gamstransfer maps them for R: ``EPS`` -> ``0.0``, ``+INF`` ->
  ``inf``, ``-INF`` -> ``-inf``, ``UNDEF`` -> a plain ``nan`` (R ``NaN``) and ``NA`` ->
  ``gams.transfer.SpecialValues.NA`` (R ``NA``: a NaN with the payload ``0xfffffffffffffffe``
  that survives ``float()`` and a float64 Series; ``math.isnan`` is true for both, :func:`is_na`
  tells them apart, so a status string built from such values reads ``NaN`` / ``NA`` like R);
* a file that is not a GDX file, is empty or truncated raises :class:`GdxError` from every
  reader. R aborts the whole process there (BUG-037); the port raises instead (D-20) and the
  callers let it propagate like the R code paths outside ``try()`` do.

Reading goes through ``gams.transfer`` (``Container(system_directory=gamspy_base.directory)``
with selective ``read(symbols=[...])``). ``gams.transfer`` does not validate the file: a text
file or an empty file reads as a GDX with no symbols and a truncated file either yields wrong
values or crashes the interpreter (verified with gamsapi 54.5.0). Every file is therefore first
opened with the low-level ``gdxcc`` API of the same wheel, which rejects all of those with a
message (``File not recognized as a GDX file``, ``Expected data marker (...) not found``).

:class:`GdxFile` is the per-file cache the plan asks for: one object per candidate file per
``get_run_status`` call, created by the caller, never shared across calls. Every module-level
function accepts either a path (opened for that one call) or an open :class:`GdxFile`.
"""

from __future__ import annotations

import itertools
import os
from collections.abc import Sequence
from dataclasses import dataclass
from typing import Any

import pandas as pd

__all__ = [
    "GdxError",
    "GdxFile",
    "SymbolInfo",
    "is_na",
    "open_gdx",
    "read_first_found",
    "read_param",
    "read_scalar",
    "read_scalar_strict",
]


class GdxError(Exception):
    """A GDX file that cannot be read, or a symbol that cannot be read the way the caller asks."""


_UNIVERSE = "*"
_system_directory: str | None = None


def _gams_system_directory() -> str:
    """The GAMS system directory shipped by gamspy_base (02 section 4.1, route A)."""
    global _system_directory
    if _system_directory is None:
        import gamspy_base

        _system_directory = str(gamspy_base.directory)
    return _system_directory


def _check_gdx_file(path: str) -> None:
    """Open ``path`` with the low-level GDX library and raise :class:`GdxError` if it refuses.

    ``gdxOpenRead`` checks the file header and the data markers; it rejects missing, empty,
    non-GDX and truncated files, all of which ``gams.transfer`` would silently misread.
    """
    from gams.core.gdx import gdxcc

    handle = gdxcc.new_gdxHandle_tp()
    created, message = gdxcc.gdxCreateD(handle, _gams_system_directory(), gdxcc.GMS_SSSIZE)
    if not created:
        raise GdxError(f"cannot load the GDX library from {_gams_system_directory()}: {message}")
    try:
        opened, error_number = gdxcc.gdxOpenRead(handle, path)
        if not opened:
            _, text = gdxcc.gdxErrorStr(handle, error_number)
            raise GdxError(f"{path}: {text}")
        gdxcc.gdxClose(handle)
    finally:
        gdxcc.gdxFree(handle)


def _r_value(value: float) -> float:
    """Map a gams.transfer value to what gamstransfer hands R: EPS (-0.0) becomes 0.0."""
    if value == 0.0:
        return 0.0
    return float(value)


def is_na(value: float) -> bool:
    """Whether a value read from a GDX file is the GAMS special value NA (R's ``NA``, not ``NaN``).

    gams.transfer hands GAMS NA over as a NaN carrying the payload ``0xfffffffffffffffe``
    (``gams.transfer.SpecialValues.NA``); UNDEF is a plain NaN (R's ``NaN``). The payload
    survives ``float()`` and a float64 Series, so the readers of this module keep it.
    """
    import gams.transfer as gt

    return bool(gt.SpecialValues.isNA(value))


@dataclass(frozen=True, slots=True)
class SymbolInfo:
    """What the symbol table of a GDX file says about one symbol."""

    name: str
    kind: str
    """``Set``, ``Parameter``, ``Variable``, ``Equation``, ``Alias`` or ``UniverseAlias``."""
    dimension: int
    domain_names: tuple[str, ...]
    """Domain names as stored in the file; ``*`` for the universe."""
    alias_with: str | None
    """For an alias: the name of the set (or ``*``) it stands for."""


@dataclass(frozen=True, slots=True)
class _Dense:
    """A parameter re-indexed onto its full domain, as ``readGDX`` returns it."""

    columns: tuple[str, ...]
    elements: tuple[tuple[str, ...], ...]
    lookup: dict[tuple[str, ...], float]
    fill: float

    def value(self, key: tuple[str, ...]) -> float:
        return self.lookup.get(key, self.fill)

    def values_c_order(self) -> list[float]:
        """All values with the first domain varying slowest (the row order of :meth:`frame`)."""
        return [self.value(key) for key in itertools.product(*self.elements)]

    def values_fortran_order(self) -> list[float]:
        """All values with the first domain varying fastest: R ``c(<magpie>)`` order.

        This equals R for 0-d and 1-d symbols and for 2-d symbols whose first domain is the
        spatial one (the only shapes the status symbols have).
        """
        return [self.value(tuple(reversed(key))) for key in itertools.product(*reversed(self.elements))]

    def frame(self) -> pd.DataFrame:
        keys = list(itertools.product(*self.elements))
        data: dict[str, Any] = {
            column: pd.Series([key[i] for key in keys], dtype="str") for i, column in enumerate(self.columns)
        }
        data["value"] = pd.Series([self.value(key) for key in keys], dtype="float64")
        return pd.DataFrame(data)


class GdxFile:
    """One GDX file: validated on open, symbol table read once, records read on demand and cached.

    ``path`` is the file as given. ``symbols`` lists every symbol of the file in file order.
    Lookups by name are case-insensitive like GDX itself.
    """

    def __init__(self, path: str | os.PathLike[str]) -> None:
        self.path = os.fspath(path)
        _check_gdx_file(self.path)
        import gams.transfer as gt

        meta = gt.Container(system_directory=_gams_system_directory())
        try:
            meta.read(self.path, records=False)
        except Exception as err:
            raise GdxError(f"{self.path}: cannot read the symbol table: {err}") from err
        infos: list[SymbolInfo] = []
        for name, symbol in meta.data.items():
            kind = type(symbol).__name__
            alias_with: str | None = None
            if kind == "Alias":
                alias_with = str(symbol.alias_with.name)
            elif kind == "UniverseAlias":
                alias_with = _UNIVERSE
            infos.append(
                SymbolInfo(
                    name=str(name),
                    kind=kind,
                    dimension=int(symbol.dimension),
                    domain_names=tuple(str(d) for d in symbol.domain_names),
                    alias_with=alias_with,
                )
            )
        self.symbols: tuple[SymbolInfo, ...] = tuple(infos)
        self._by_name: dict[str, SymbolInfo] = {info.name.casefold(): info for info in infos}
        self._container: Any = gt.Container(system_directory=_gams_system_directory())
        self._dense_cache: dict[str, _Dense] = {}

    def __repr__(self) -> str:
        return f"GdxFile({self.path!r}, {len(self.symbols)} symbols)"

    def has(self, name: str) -> bool:
        return name.casefold() in self._by_name

    def info(self, name: str) -> SymbolInfo | None:
        return self._by_name.get(name.casefold())

    def _resolve_set(self, name: str) -> str | None:
        """The set a domain name stands for (following aliases), or None for the universe or an unknown name."""
        seen: set[str] = set()
        current = name
        while current != _UNIVERSE:
            info = self.info(current)
            if info is None or info.name in seen:
                return None
            if info.kind == "Set":
                return info.name
            if info.alias_with is None:
                return None
            seen.add(info.name)
            current = info.alias_with
        return None

    def _load(self, names: Sequence[str]) -> None:
        missing = [name for name in dict.fromkeys(names) if name not in self._container]
        if not missing:
            return
        try:
            self._container.read(self.path, symbols=missing)
        except Exception as err:
            raise GdxError(f"{self.path}: cannot read {', '.join(missing)}: {err}") from err

    def _set_elements(self, set_name: str) -> tuple[str, ...]:
        records = self._container[set_name].records
        if records is None:
            return ()
        return tuple(records.iloc[:, 0].astype(str).tolist())

    def _dense(self, name: str) -> _Dense:
        """Re-index a parameter onto its domain like ``readGDX`` (``restoreZeros = TRUE``)."""
        info = self.info(name)
        if info is None:
            raise GdxError(f"{self.path}: no symbol {name}")
        if info.name in self._dense_cache:
            return self._dense_cache[info.name]
        if info.kind != "Parameter":
            raise GdxError(f"{self.path}: {info.name} is a {info.kind}, not a Parameter")
        import gams.transfer as gt

        resolved = [self._resolve_set(domain) for domain in info.domain_names]
        self._load([info.name, *(set_name for set_name in resolved if set_name is not None)])
        records = self._container[info.name].records

        lookup: dict[tuple[str, ...], float] = {}
        if records is not None:
            # to_numpy().tolist() keeps one (empty) key per row for a 0-d symbol, itertuples would not
            keys = records.iloc[:, : info.dimension].astype(str).to_numpy().tolist()
            for key, value in zip(keys, records["value"].tolist(), strict=True):
                lookup[tuple(str(k) for k in key)] = _r_value(value)

        restore_zeros = all(set_name is not None for set_name in resolved)
        elements: list[tuple[str, ...]] = []
        for position, set_name in enumerate(resolved):
            if restore_zeros and set_name is not None:
                elements.append(self._set_elements(set_name))
            elif records is None:
                elements.append(())
            else:
                elements.append(tuple(dict.fromkeys(records.iloc[:, position].astype(str))))

        columns: list[str] = []
        for position, domain in enumerate(info.domain_names):
            column = f"uni_{position}" if domain == _UNIVERSE else domain
            if column in columns:
                suffix = 1
                while f"{column}_{suffix}" in columns:
                    suffix += 1
                column = f"{column}_{suffix}"
            columns.append(column)

        dense = _Dense(
            columns=tuple(columns),
            elements=tuple(elements),
            lookup=lookup,
            # readGDX fills missing combinations with 0 (restoreZeros) or, under a universe domain, with R NA
            fill=0.0 if restore_zeros else float(gt.SpecialValues.NA),
        )
        self._dense_cache[info.name] = dense
        return dense

    def scalar(self, name: str) -> float | None:
        """A 0-d parameter as a float (``format = "simplest"``); None when the symbol is absent."""
        info = self.info(name)
        if info is None:
            return None
        if info.kind != "Parameter" or info.dimension != 0:
            raise GdxError(f"{self.path}: {info.name} is not a scalar parameter ({info.kind}, {info.dimension}-d)")
        return self._dense(info.name).values_c_order()[0]

    def values(self, name: str) -> list[float] | None:
        """All values of a parameter in R ``c(<magpie>)`` order, dense; None when absent."""
        if not self.has(name):
            return None
        return self._dense(name).values_fortran_order()

    def frame(self, name: str) -> pd.DataFrame | None:
        """The dense parameter as a frame: one str column per domain plus ``value``; None when absent.

        Rows follow the cartesian product of the domains with the first domain varying slowest,
        so filtering on the last key column (``frame[frame.iloc[:, -2] == "modelstat"]``) keeps
        the first domain's set order, which is what ``paste(p80_repy[, , "modelstat"], collapse
        = "")`` concatenates in R.
        """
        if not self.has(name):
            return None
        return self._dense(name).frame()


GdxSource = str | os.PathLike[str] | GdxFile


def open_gdx(gdx: GdxSource) -> GdxFile:
    """Return ``gdx`` itself when it is an open :class:`GdxFile`, otherwise open the path."""
    if isinstance(gdx, GdxFile):
        return gdx
    return GdxFile(gdx)


def read_scalar(gdx: GdxSource, name: str) -> float | None:
    """``as.numeric(readGDX(gdx, name, format = "simplest", react = "silent"))``.

    None when the symbol is absent. :class:`GdxError` when the file is unreadable (R aborts
    there even inside ``try()``, BUG-037 / D-20) or the symbol is not a scalar parameter.
    """
    return open_gdx(gdx).scalar(name)


def read_scalar_strict(gdx: GdxSource, name: str) -> float:
    """Like :func:`read_scalar` but an absent symbol is a :class:`GdxError` too."""
    value = open_gdx(gdx).scalar(name)
    if value is None:
        raise GdxError(f"{open_gdx(gdx).path}: no symbol {name}")
    return value


def read_first_found(gdx: GdxSource, names: Sequence[str]) -> list[float] | None:
    """``c(readGDX(gdx, names, format = "first_found", react = "silent"))``.

    The values of the first symbol of ``names`` that exists in the file, dense and in R's
    ``c()`` order (see :meth:`GdxFile.values`); None when none of them exists.
    """
    if not names:
        raise ValueError("first_found needs at least one symbol name")
    file = open_gdx(gdx)
    for name in names:
        if file.has(name):
            return file.values(name)
    return None


def read_param(gdx: GdxSource, name: str) -> pd.DataFrame | None:
    """``readGDX(gdx, name)`` as a dense frame (see :meth:`GdxFile.frame`); None when absent."""
    return open_gdx(gdx).frame(name)
