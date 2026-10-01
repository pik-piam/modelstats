"""GDX reading with the semantics of ``gdx2::readGDX`` as ``getRunStatus`` uses it.

``R/getRunStatus.R`` reads seven symbols from a run's ``fulldata.gdx``, ``non_optimal.gdx`` and
``abort.gdx``: ``o_iterationNumber`` (lines 57 and 280), ``o_modelstat``/``p80_modelstat`` (84),
``s80_bool`` (281), ``p80_repy`` (288), ``cm_abortOnConsecFail`` and ``p80_trackConsecFail``
(212-213). Every read goes through ``gdx2::readGDX`` (gdx2 0.6.2 over gamstransfer 3.0.9 and
magclass 7.5.0, the versions pinned by the goldens), whose behaviour this module reproduces
(``migration/02-python-libraries.md`` section 4.1, ``migration/03-migration-plan.md`` section 3.1):

* *absent symbol*: ``react = "silent"`` returns ``NULL`` → :func:`read_param` returns ``None``, and
  ``format = "first_found"`` tries the next name (:func:`read_first_found`);
* *dense reconstruction* (``restoreZeros = TRUE``): a parameter over sets comes back as a full array
  over the cartesian product of its domain sets, in set order, with ``0`` where the GDX stores no
  record; aliases are followed to their parent set. A present parameter without records is therefore
  all zeros, a scalar is one value, and a parameter over the universe ``*`` cannot be restored (R
  warns and keeps the stored records);
* *order*: ``magclass::as.magpie`` puts the spatial dimension first (every label ``^[A-Z]{3}$`` or
  ``glob``, or every label ``^[A-Z]+[._][0-9]+$``), the temporal one second (every label
  ``^[a-z]?[0-9]{4}$``) and the rest after it, so ``c(x)`` runs through the spatial labels fastest;
  labels keep the order of the set records (years are not sorted). :func:`read_param` returns the
  dense frame with its rows in that order and ``flat_values`` is ``c(x)``;
* *special values*: ``EPS`` is ``-0.0`` (R prints it as ``0``), ``INF``/``-INF`` are infinities,
  ``NA`` is R's ``NA`` and ``UNDEF`` is R's ``NaN``; both are ``nan`` here, told apart by
  :func:`is_na` (gams.transfer's payload), which :func:`number_text` prints as ``NA`` and ``NaN``;
* *errors*: a file that is not a GDX, truncated or missing aborts the whole R process (BUG-037,
  decision D-20) and is a :class:`GdxReadError` here, raised and never swallowed, so that the caller
  reproduces the ``try()`` structure of ``getRunStatus`` on the Python side: a ``None`` result is R's
  ``NULL``, an exception is R's abort.

The ``o_iterationNumber`` selection rule of ``R/getRunStatus.R:53-64`` is :func:`select_latest_gdx`;
the ``modelstat`` string of lines 84-87 and 98-102 is :func:`modelstat_text` and
:func:`explain_modelstat`; the ``Conv`` digit string of line 290 is :func:`repy_modelstat_text`.

Reading uses the two layers of the ``gamsapi`` wheel with ``gamspy_base`` as the GAMS system
directory (route A of ``02`` section 4.1): the symbol table comes from the low-level ``gdxcc`` API,
because ``gams.transfer``'s selective read crashes the interpreter (SIGSEGV) on a corrupt file while
``gdxOpenRead`` reports it cleanly; the records then come from one ``gams.transfer.Container`` per
:class:`GdxFile`, read selectively and cached per symbol. A :class:`GdxFile` is meant to live for one
``get_run_status`` call, never across calls.
"""

from __future__ import annotations

import math
import re
from collections.abc import Iterable, Mapping, Sequence
from dataclasses import dataclass
from enum import Enum
from itertools import product
from pathlib import Path
from typing import Any, Final

import gams.transfer as gt
import gamspy_base
import pandas as pd
from gams.core.gdx import gdxcc

from modelstats.rdata_io import r_as_character

VALUE: Final = "value"
"""The value column of a records frame (gams.transfer's name, also gamstransfer's in R)."""

UNIVERSE: Final = "*"
"""The universe domain, over which ``gdx2::readGDX`` cannot restore zeros."""

ITERATION_SYMBOL: Final = "o_iterationNumber"
MODELSTAT_SYMBOLS: Final = ("o_modelstat", "p80_modelstat")
"""``R/getRunStatus.R:84``: the ``first_found`` list for ``modelstat``."""

MODELSTAT_EXPLANATIONS: Final[Mapping[str, str]] = {
    "1": "Optimal",
    "2": "Locally Optimal",
    "3": "Unbounded",
    "4": "Infeasible",
    "5": "Locally Infes",
    "6": "Intermed Infes",
    "7": "Intermed Nonoptimal",
    "13": "Error No Solution",
}
"""``explain_modelstat`` of ``R/getRunStatus.R:98-99``."""

_REGIONAL: Final = re.compile(r"^(([A-Z]{3})|(glob))$")
_REGIOSPATIAL: Final = re.compile(r"^[A-Z]+[._][0-9]+$")
_TEMPORAL: Final = re.compile(r"^[a-z]?[0-9]{4}$")


class GdxReadError(Exception):
    """A GDX file could not be read: not a GDX, truncated, missing, or a symbol of the wrong kind.

    R aborts in these cases (BUG-037) or errors outside ``try()``; the port raises (D-20).
    """


class SymbolKind(Enum):
    """GDX symbol types (``GMS_DT_*`` of the gdx library)."""

    SET = 0
    PARAMETER = 1
    VARIABLE = 2
    EQUATION = 3
    ALIAS = 4


class DomainKind(Enum):
    """What ``gdxSymbolGetDomainX`` knows about a symbol's domain."""

    NONE = 1
    """No domain information: a scalar, or every dimension is the universe ``*``."""
    RELAXED = 2
    """Set names recorded without a link to the sets."""
    REGULAR = 3
    """Declared over sets (or aliases) that exist in the file."""


@dataclass(frozen=True)
class SymbolInfo:
    """One row of a GDX file's symbol table."""

    name: str
    kind: SymbolKind
    dimension: int
    domain: tuple[str, ...]
    domain_kind: DomainKind
    record_count: int
    alias_of: str | None = None
    """The parent set of an alias (``gdx2::readGDX(followAlias = TRUE)``)."""


# ---------------------------------------------------------------------------------------------
# Symbol table (gdxcc)
# ---------------------------------------------------------------------------------------------


def _open_for_reading(path: Path) -> Any:
    """A gdxcc handle on ``path``, or :class:`GdxReadError` with the library's message."""
    handle = gdxcc.new_gdxHandle_tp()
    created, message = gdxcc.gdxCreateD(handle, gamspy_base.directory, gdxcc.GMS_SSSIZE)
    if not created:
        gdxcc.gdxFree(handle)
        raise GdxReadError(f"GDX library not available in {gamspy_base.directory}: {message}")
    opened, error_number = gdxcc.gdxOpenRead(handle, str(path))
    if not opened:
        _, text = gdxcc.gdxErrorStr(handle, error_number)
        gdxcc.gdxFree(handle)
        raise GdxReadError(f"{path}: {text}")
    return handle


def _domain_kind(code: int) -> DomainKind:
    """``gdxSymbolGetDomainX`` return codes; anything else is a library error."""
    try:
        return DomainKind(code)
    except ValueError as error:
        raise GdxReadError(f"gdxSymbolGetDomainX failed with code {code}") from error


def read_symbol_table(path: Path) -> dict[str, SymbolInfo]:
    """The symbol table of a GDX file in file order; raises :class:`GdxReadError` when unreadable."""
    handle = _open_for_reading(path)
    try:
        _, count, _ = gdxcc.gdxSystemInfo(handle)
        names: dict[int, str] = {}
        rows: list[tuple[SymbolInfo, int]] = []
        for number in range(1, count + 1):
            _, name, dimension, kind = gdxcc.gdxSymbolInfo(handle, number)
            _, record_count, user_info, _ = gdxcc.gdxSymbolInfoX(handle, number)
            code, domain = gdxcc.gdxSymbolGetDomainX(handle, number)
            info = SymbolInfo(
                name=str(name),
                kind=SymbolKind(int(kind)),
                dimension=int(dimension),
                domain=tuple(str(d) for d in domain),
                domain_kind=_domain_kind(int(code)),
                record_count=int(record_count),
            )
            names[number] = info.name
            rows.append((info, int(user_info)))
    finally:
        gdxcc.gdxClose(handle)
        gdxcc.gdxFree(handle)
    table: dict[str, SymbolInfo] = {}
    for info, user_info in rows:
        parent = names.get(user_info) if info.kind is SymbolKind.ALIAS else None
        table[info.name] = SymbolInfo(**{**info.__dict__, "alias_of": parent})
    return table


# ---------------------------------------------------------------------------------------------
# Dense reconstruction in magclass order
# ---------------------------------------------------------------------------------------------


def domain_labels(domain: Sequence[str]) -> tuple[str, ...]:
    """Column names of a records frame as gams.transfer labels them.

    ``*`` becomes ``uni``; when a name repeats, every name gets its 0-based position as a suffix
    (``all_regi_0``, ``solveinfo80_1``, ``all_regi_2``). R's gamstransfer uses 1-based suffixes and
    ``uni_1``; no consumer in ``getRunStatus`` reads such a column by name.
    """
    names = ["uni" if name == UNIVERSE else name for name in domain]
    if len(set(names)) == len(names):
        return tuple(names)
    return tuple(f"{name}_{position}" for position, name in enumerate(names))


def _all_match(pattern: re.Pattern[str], labels: Sequence[str]) -> bool:
    return all(pattern.match(label) for label in labels)


def _spatial_dimension(levels: Sequence[Sequence[str]]) -> int | None:
    """``as.magpie``'s spatial dimension: the first regiospatial one, else the first regional one."""
    for pattern in (_REGIOSPATIAL, _REGIONAL):
        for index, labels in enumerate(levels):
            if _all_match(pattern, labels):
                return index
    return None


def _temporal_dimension(levels: Sequence[Sequence[str]], spatial: int | None) -> int | None:
    """``as.magpie``'s temporal dimension: the first one whose labels are all years.

    A dimension without labels matches every pattern in R, which then fails on a duplicated map;
    the spatial dimension is excluded here instead so that an empty set yields an empty frame.
    """
    for index, labels in enumerate(levels):
        if index != spatial and _all_match(_TEMPORAL, labels):
            return index
    return None


def magpie_roles(levels: Sequence[Sequence[str]]) -> tuple[int | None, int | None]:
    """``as.magpie``'s (spatial, temporal) dimension indices, ``None`` for a role it does not detect."""
    spatial = _spatial_dimension(levels)
    return spatial, _temporal_dimension(levels, spatial)


def magpie_order(levels: Sequence[Sequence[str]]) -> tuple[int, ...]:
    """Dimension indices from the slowest to the fastest varying in ``c(as.magpie(x))``.

    ``wrap(x, list(spatial, temporal, NA))`` (``magclass::as.magpie`` for arrays) makes the spatial
    dimension the first (fastest in column-major order), the temporal one the second, and folds the
    remaining dimensions in their original order into the third.
    """
    leading = [index for index in magpie_roles(levels) if index is not None]
    rest = [index for index in range(len(levels)) if index not in leading]
    return tuple(reversed(leading + rest))


def _record_values(records: pd.DataFrame | None) -> dict[tuple[str, ...], float]:
    """The stored records as ``{labels: value}``; values stay gams.transfer's floats (payloads kept)."""
    if records is None:
        return {}
    keys = [tuple(row) for row in records.iloc[:, :-1].astype(str).to_numpy().tolist()]
    values = [float(v) for v in records[VALUE].tolist()]
    return dict(zip(keys, values, strict=True))


def dense_frame(
    labels: Sequence[str], levels: Sequence[Sequence[str]], records: pd.DataFrame | None
) -> pd.DataFrame:
    """The ``restoreZeros`` array as a frame: every label combination, 0 where no record is stored.

    Rows follow :func:`magpie_order`, so ``frame[VALUE]`` is ``c(x)`` of the R object.
    """
    stored = _record_values(records)
    order = magpie_order(levels)
    rows = []
    for combination in product(*(levels[index] for index in order)):
        key = tuple(combination[order.index(index)] for index in range(len(levels)))
        rows.append((*key, stored.get(key, 0.0)))
    return pd.DataFrame(rows, columns=[*labels, VALUE], dtype=object).astype({VALUE: float})


def stored_frame(labels: Sequence[str], records: pd.DataFrame | None) -> pd.DataFrame:
    """The stored records as read (universe domains): file order, labels as plain strings."""
    if records is None:
        return pd.DataFrame(columns=[*labels, VALUE]).astype({VALUE: float})
    frame = records.iloc[:, :-1].astype(str)
    frame.columns = pd.Index(list(labels))
    frame[VALUE] = [float(v) for v in records[VALUE].tolist()]
    return frame.reset_index(drop=True)


# ---------------------------------------------------------------------------------------------
# One file
# ---------------------------------------------------------------------------------------------


class GdxFile:
    """One GDX file: the symbol table read once, parameters read on demand and cached.

    Nothing is opened before the first lookup, so a candidate that ``select_latest_gdx`` never needs
    to read (a single existing GDX, ``R/getRunStatus.R:54``) is never opened, as in R.
    """

    def __init__(self, path: Path | str) -> None:
        """Wrap ``path`` without touching it."""
        self.path = Path(path)
        self._table: dict[str, SymbolInfo] | None = None
        self._container: Any = None
        self._parameters: dict[str, pd.DataFrame | None] = {}

    def __repr__(self) -> str:
        """``GdxFile('/path/fulldata.gdx')``."""
        return f"GdxFile({str(self.path)!r})"

    @property
    def symbols(self) -> Mapping[str, SymbolInfo]:
        """The symbol table; opening the file for the first time raises on a damaged one."""
        if self._table is None:
            self._table = read_symbol_table(self.path)
            self._container = gt.Container(system_directory=gamspy_base.directory)
        return self._table

    def info(self, name: str) -> SymbolInfo | None:
        """The table row of ``name``, ``None`` when the file has no such symbol."""
        return self.symbols.get(name)

    def parameter(self, name: str) -> pd.DataFrame | None:
        """``readGDX(gdx, name, format = "simplest", react = "silent")`` as a dense frame.

        ``None`` for an absent symbol; :class:`GdxReadError` for a symbol that is not a parameter
        (R would return a set's elements or a variable's five fields, which no caller can use) and
        for a domain set missing from the file.
        """
        if name not in self._parameters:
            info = self.info(name)
            self._parameters[name] = None if info is None else self._read_parameter(info)
        return self._parameters[name]

    def _read_parameter(self, info: SymbolInfo) -> pd.DataFrame:
        if info.kind is not SymbolKind.PARAMETER:
            kind = info.kind.name.lower()
            raise GdxReadError(f"{self.path}: {info.name} is not a parameter (kind: {kind})")
        sets = [self._domain_set(info, name) for name in info.domain]
        self._load([info.name, *(name for name in sets if name is not None)])
        records = self._container[info.name].records
        labels = domain_labels(info.domain)
        if any(name is None for name in sets):
            return stored_frame(labels, records)
        levels = [self._set_elements(name) for name in sets if name is not None]
        return dense_frame(labels, levels, records)

    def _domain_set(self, info: SymbolInfo, name: str) -> str | None:
        """The set (alias resolved) behind one domain position, ``None`` for the universe."""
        if name == UNIVERSE:
            return None
        domain = self.info(name)
        if domain is None or domain.kind not in (SymbolKind.SET, SymbolKind.ALIAS):
            raise GdxReadError(f"{self.path}: domain {name} of {info.name} is not a set in the file")
        return domain.alias_of if domain.kind is SymbolKind.ALIAS and domain.alias_of else name

    def _load(self, names: Iterable[str]) -> None:
        """Selective read of the symbols not yet in the container (the table lookup opened the file)."""
        missing = [name for name in dict.fromkeys(names) if name not in self._container]
        if not missing:
            return
        try:
            self._container.read(str(self.path), symbols=missing)
        except Exception as error:
            raise GdxReadError(f"{self.path}: reading {', '.join(missing)} failed: {error}") from error

    def _set_elements(self, name: str) -> list[str]:
        records = self._container[name].records
        return [] if records is None else [str(e) for e in records.iloc[:, 0].tolist()]


type GdxSource = GdxFile | Path | str


def _as_file(gdx: GdxSource) -> GdxFile:
    return gdx if isinstance(gdx, GdxFile) else GdxFile(gdx)


# ---------------------------------------------------------------------------------------------
# readGDX call shapes used by getRunStatus
# ---------------------------------------------------------------------------------------------


def read_param(gdx: GdxSource, name: str) -> pd.DataFrame | None:
    """``readGDX(gdx, name, react = "silent")``: the dense frame in R order, ``None`` when absent."""
    return _as_file(gdx).parameter(name)


def read_scalar(gdx: GdxSource, name: str) -> float | None:
    """``as.numeric(readGDX(gdx, name, format = "simplest", react = "silent"))`` of a 0-d parameter.

    ``None`` when the symbol is absent; :class:`GdxReadError` when it has dimensions (R returns a
    vector there, which every call site in ``getRunStatus`` then mishandles).
    """
    frame = read_param(gdx, name)
    if frame is None:
        return None
    if len(frame.columns) != 1 or len(frame) != 1:
        dimensions, count = len(frame.columns) - 1, len(frame)
        raise GdxReadError(f"{name} is not a scalar: {dimensions} dimension(s), {count} value(s)")
    return float(frame[VALUE].iloc[0])


def read_first_found(gdx: GdxSource, names: Sequence[str]) -> pd.DataFrame | None:
    """``readGDX(gdx, names, format = "first_found", react = "silent")``: the first present symbol."""
    file = _as_file(gdx)
    for name in names:
        frame = file.parameter(name)
        if frame is not None:
            return frame
    return None


def flat_values(frame: pd.DataFrame) -> list[float]:
    """``c(x)`` / ``as.vector(x)`` of the R object: the values in magclass order."""
    return [float(v) for v in frame[VALUE].tolist()]


def select_element(frame: pd.DataFrame, element: str) -> list[float]:
    """``x[, , element]``: the values whose label in the one dimension containing ``element`` is it.

    Exactly one domain column must carry the label; otherwise R's subscript fails as well.
    """
    columns = [column for column in frame.columns[:-1] if (frame[column] == element).any()]
    if len(columns) != 1:
        raise GdxReadError(f"{element!r} is a label of {len(columns)} dimension(s), not of exactly one")
    return flat_values(frame[frame[columns[0]] == element])


# ---------------------------------------------------------------------------------------------
# Special values and R text
# ---------------------------------------------------------------------------------------------


def is_na(value: float) -> bool:
    """GAMS ``NA`` (R's ``NA``), as opposed to ``UNDEF`` (R's ``NaN``); both are ``nan`` floats."""
    return math.isnan(value) and bool(gt.SpecialValues.isNA(value))


def number_text(value: float) -> str:
    """``paste0(x)`` of one GDX value: ``NA``, ``NaN``, ``Inf``, ``-Inf``, ``0`` for EPS, else R's digits."""
    if is_na(value):
        return "NA"
    return r_as_character(value) or "NA"


def modelstat_text(values: Iterable[float]) -> str:
    """``gsub("0", ".", paste0(o_modelstat, collapse = ""))`` (``R/getRunStatus.R:86``)."""
    return "".join(number_text(value) for value in values).replace("0", ".")


def explain_modelstat(text: str) -> str:
    """``R/getRunStatus.R:98-102``: ``"2"`` becomes ``"2: Locally Optimal"``; anything else is kept."""
    explanation = MODELSTAT_EXPLANATIONS.get(text)
    return f"{text}: {explanation}" if explanation is not None else text


def repy_modelstat_text(repy: pd.DataFrame) -> str:
    """``paste(p80_repy[, , "modelstat"], collapse = "")`` (``R/getRunStatus.R:290``)."""
    return "".join(number_text(value) for value in select_element(repy, "modelstat"))


# ---------------------------------------------------------------------------------------------
# GDX selection (R/getRunStatus.R:52-64)
# ---------------------------------------------------------------------------------------------


@dataclass(frozen=True)
class GdxCandidate:
    """An existing candidate GDX with its ``file.info()$mtime`` (seconds since the epoch)."""

    file: GdxFile
    mtime: float


def which_max(values: Sequence[float]) -> int | None:
    """R ``which.max``: the first index of the maximum, ``NA``/``NaN`` ignored, ``None`` when none remain."""
    best: int | None = None
    for index, value in enumerate(values):
        if math.isnan(value):
            continue
        if best is None or value > values[best]:
            best = index
    return best


def select_latest_gdx(candidates: Sequence[GdxCandidate]) -> GdxFile | None:
    """``R/getRunStatus.R:53-64``: the candidate with the highest ``o_iterationNumber``.

    ``candidates`` are the existing ones of ``fulldata.gdx``, ``non_optimal.gdx`` in that order. One
    candidate is taken without reading it. With several, every one is read (a damaged file raises,
    which R has no ``try()`` for either); if together they yield one value per file, ``which.max``
    decides (first maximum on ties, ``NA`` skipped, no file at all when every value is ``NA``),
    otherwise the newest mtime does (first on ties).
    """
    if not candidates:
        return None
    if len(candidates) == 1:
        return candidates[0].file
    iterations = [
        value
        for candidate in candidates
        for value in _values_or_none(candidate.file.parameter(ITERATION_SYMBOL))
    ]
    if len(iterations) == len(candidates):
        index = which_max(iterations)
        return None if index is None else candidates[index].file
    newest = which_max([candidate.mtime for candidate in candidates])
    return None if newest is None else candidates[newest].file


def _values_or_none(frame: pd.DataFrame | None) -> list[float]:
    """``unlist()`` drops a ``NULL`` element and flattens the others."""
    return [] if frame is None else flat_values(frame)
