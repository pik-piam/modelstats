"""``get_run_status``: the status record of model runs (``R/getRunStatus.R``, line by line).

The function builds a :class:`StatusTable`, a literal model of the R ``data.frame`` that
``getRunStatus`` fills cell by cell with ``out[i, "col"] <- value``: rows are keyed by the
directory's basename in order of first assignment, columns in order of first assignment, a
new row gets NA in every existing column, a new column NA in every existing row, and a later
directory with the same basename overwrites only the cells it assigns (BUG-022, decision D-13
pending, so parity). Brief mode (``detailed = FALSE``) stops after ``RunStatus``; the record
then has no ``Warnings``, ``Conv``, ``Runtime`` or sanity columns at all.

Everything R does not wrap in ``try()`` propagates. Where an R golden pins the message the
port raises :class:`~modelstats.errors.RParityError` with R's text; a corrupt GDX file raises
:class:`~modelstats.gdx.GdxError` where R aborts the whole process (BUG-037, D-20). The R
semantics reproduced here were verified with R 4.6.1 on 2026-10-01 (``tests/unit/test_run_status.py``):

- ``normalizePath()`` keeps a non-existent path as given and warns ``path[k]="...": No such
  file or directory`` (not suppressed on this path: an :class:`~modelstats.errors.RWarning`);
  ``file.info()`` gives an NA row for it, and ``a[a[, "isdir"] == TRUE, ]`` turns that NA row
  into a row named ``"NA"`` (``"NA.1"`` for a second one, ``make.unique`` of ``[.data.frame``),
  which the loop then visits as the directory ``NA``; ``order(mtime, decreasing = TRUE)`` is
  stable and puts NA last. Without ``sort == "nf"`` files and missing paths are visited as given.
- ``paste("a", NULL)`` is ``"a "``: a zero-length argument contributes ``""`` and the separator
  stays (``"Run MAgPIE "`` when no MAgPIE loop is known, ``"mag-1 "`` without a MAgPIE status).
- ``TRUE && logical(0)`` is ``NA`` and ``if (NA)`` fails with ``missing value where TRUE/FALSE
  needed`` (the ``Conv`` check when the selected GDX lacks ``o_iterationNumber``, BUG-038 / D-23);
  ``if (logical(0))`` fails with ``argument is of length zero`` (``s80_bool`` absent, and
  ``if (NULL == "MAgPIE")`` for a ``stats$config`` without ``model_name``, lines 91 and 108).
- ``out[i, col] <- value`` with a length-0 value fails with ``replacement has length zero`` and
  with a longer vector with ``replacement has <n> rows, data has 1``; inside ``try()`` (the
  ``*** Status:`` lines, BUG-036, and the REMIND ``stats$modelstat`` fallback) that leaves the
  cell at ``"NA"``, elsewhere it aborts the call. A number assigned into a character column is
  ``as.character``-ed (``"2"``), ``NA`` stays a real NA (``None``).
- ``grep '*** Status: '`` is a basic regex whose leading ``*`` is literal and whose ``**`` repeat
  it: ``\\** Status: `` (every fixture log only has ``*** Status:`` lines).
- ``"1" > 0`` compares as strings under the session collation (``cm_nash_autoconverge`` is the
  string ``"1"`` in some configs); ``cfg$gms$x`` partially matches a unique prefix.
"""

from __future__ import annotations

import fnmatch
import math
import os
import re
import warnings
from collections.abc import Iterator, Mapping, Sequence
from typing import TYPE_CHECKING, Any

from modelstats import textscan
from modelstats.config import config_matches, find_config_file, load_config
from modelstats.env import r_collate_key
from modelstats.errors import RParityError, RWarning
from modelstats.formatting import niceround, r_num_str, r_round
from modelstats.gdx import GdxFile, is_na, read_first_found, read_param, read_scalar
from modelstats.rdata_io import RTime, pythonize, read_rds, vector
from modelstats.run_type import col_run_type
from modelstats.runfolders import r_basename
from modelstats.runstats import RunStatistics
from modelstats.slurm import found_in_slurm, normalize_path
from modelstats.tables import count_rows, count_unique_variable

if TYPE_CHECKING:
    import pandas as pd

    from modelstats.env import Effects

__all__ = ["EXPLAIN_MODELSTAT", "REMIND_RESULTS_ARCHIVE", "RunStatus", "StatusTable", "get_run_status"]

#: ``explain_modelstat`` of ``R/getRunStatus.R:98``.
EXPLAIN_MODELSTAT: Mapping[str, str] = {
    "1": "Optimal",
    "2": "Locally Optimal",
    "3": "Unbounded",
    "4": "Infeasible",
    "5": "Locally Infes",
    "6": "Intermed Infes",
    "7": "Intermed Nonoptimal",
    "13": "Error No Solution",
}
#: The REMIND results archive of ``runInAppResults`` (``R/getRunStatus.R:111``).
REMIND_RESULTS_ARCHIVE = "/p/projects/rd3mod/models/results/remind/"

# grep '*** Status: ' (basic regex: a leading * is literal, the following ones repeat it)
_STATUS_LINE = re.compile(r"\** Status: ")
# grep -m 1 'cm_iteration_max = [1-9].*.;[ ]*$' on tac full.gms
_ITERMAX_LINE = re.compile(r"cm_iteration_max = [1-9].*.;[ ]*$")
_AFTER_EQUALS = re.compile(r"^.*.= ")
_SEMICOLON = re.compile(r";[ ]*")
_STATUS_PREFIX = re.compile(r"\*\*\* Status: ")
_PLURAL_S = re.compile(r"\(s\)")
_COUPLING_ITER = re.compile(r".*-mag-([0-9]{1,2})$")
_DIGITS_1_2 = re.compile(r"[0-9]{1,2}")

type PathLike = str | os.PathLike[str]


def _effects(effects: Effects | None) -> Effects:
    if effects is not None:
        return effects
    from modelstats.env import default_effects

    return default_effects()


# ---------------------------------------------------------------------------
# StatusTable: R's data.frame cell assignment, literally
# ---------------------------------------------------------------------------


class RunStatus(Mapping[str, object]):
    """One row of a :class:`StatusTable`: a read-only mapping column -> cell plus ``rowname`` and ``path``.

    ``None`` is a real NA, the string ``"NA"`` is the literal R assigns; ``row[col]`` raises
    ``KeyError`` for a column the table does not have (brief records have no ``Runtime``).
    ``path`` is the resolved directory the row was last filled from (``None`` for a row that
    was never tied to a directory).
    """

    __slots__ = ("_cells", "path", "rowname")

    def __init__(self, rowname: str, path: str | None, cells: Mapping[str, object]) -> None:
        self.rowname = rowname
        self.path = path
        self._cells: dict[str, object] = dict(cells)

    def __getitem__(self, column: str) -> object:
        return self._cells[column]

    def __iter__(self) -> Iterator[str]:
        return iter(self._cells)

    def __len__(self) -> int:
        return len(self._cells)

    def __repr__(self) -> str:
        return f"RunStatus({self.rowname!r}, {self._cells!r})"


class StatusTable:
    """The ``out`` data.frame of ``getRunStatus`` with R's cell-assignment semantics.

    ``table[rowname, col] = value`` creates the row (NA in every existing column) and/or the
    column (NA in every existing row) on first use and sets the one cell; ``table[rowname, col]``
    reads a cell (NA for a row that does not exist, like ``out["nope", "col"]``). Reading a column
    the table never got raises ``RParityError('undefined columns selected')`` as a guard against
    port mistakes (R 4.6.1 quietly returns ``NULL`` there; ``getRunStatus`` never reads a column
    before assigning it). Row and column order is the order of first assignment. Cells hold
    ``None`` (a real NA), ``str`` (including the literal ``"NA"``), ``int`` or ``float``.
    """

    def __init__(self) -> None:
        self._columns: list[str] = []
        self._cells: dict[str, dict[str, object]] = {}
        self._paths: dict[str, str] = {}

    def __repr__(self) -> str:
        return f"StatusTable({len(self._cells)} rows, columns {self._columns!r})"

    def __len__(self) -> int:
        return len(self._cells)

    def __iter__(self) -> Iterator[RunStatus]:
        return iter(self.rows())

    def __contains__(self, rowname: object) -> bool:
        return rowname in self._cells

    @property
    def columns(self) -> list[str]:
        """The column names in order of first assignment (``names(out)``)."""
        return list(self._columns)

    @property
    def rownames(self) -> list[str]:
        """The row names in order of first assignment (``rownames(out)``)."""
        return list(self._cells)

    def __setitem__(self, key: tuple[str, str], value: object) -> None:
        rowname, column = key
        if column not in self._columns:
            self._columns.append(column)
            for cells in self._cells.values():
                cells[column] = None
        if rowname not in self._cells:
            self._cells[rowname] = dict.fromkeys(self._columns)
        self._cells[rowname][column] = value

    def __getitem__(self, key: tuple[str, str]) -> object:
        rowname, column = key
        if column not in self._columns:
            raise RParityError("undefined columns selected")
        cells = self._cells.get(rowname)
        return None if cells is None else cells[column]

    def column(self, column: str) -> list[object] | None:
        """``out[[column]]``: the cells of a column in row order, ``None`` (R NULL) for an unknown column."""
        if column not in self._columns:
            return None
        return [cells[column] for cells in self._cells.values()]

    def register_path(self, rowname: str, path: str) -> None:
        """Remember the directory a row is being filled from (``ii`` of the R loop)."""
        self._paths[rowname] = path

    def rows(self) -> list[RunStatus]:
        """A snapshot of every row, in row order."""
        return [RunStatus(name, self._paths.get(name), cells) for name, cells in self._cells.items()]

    def to_json_rows(self) -> list[dict[str, object]]:
        """The rows as the golden JSON records them: ``_row`` first, then every column of the table.

        A real NA is ``None``, the literal ``"NA"`` stays a string, and an integral float is
        written as an ``int`` (``jsonlite`` writes the R double ``8304`` as ``8304``).
        """
        return [
            {"_row": name, **{col: _json_cell(v) for col, v in cells.items()}} for name, cells in self._cells.items()
        ]


def _json_cell(value: object) -> object:
    if isinstance(value, float) and not isinstance(value, bool) and math.isfinite(value) and value.is_integer():
        return int(value)
    return value


# ---------------------------------------------------------------------------
# R value helpers
# ---------------------------------------------------------------------------


def _r_vec(x: object) -> list[object]:
    """The elements of an R value: ``None`` (NULL) is ``[]``, a scalar is ``[x]``, a vector its elements."""
    if x is None:
        return []
    if isinstance(x, str | bytes | bool | int | float | RTime):
        return [x]
    if isinstance(x, Mapping):
        return list(x.values())
    if isinstance(x, list | tuple):
        return list(x)
    return vector(x)


def _as_character(v: object) -> str:
    """``as.character()`` of one element (``NA`` is ``"NA"``, numbers as R's ``paste0`` prints them)."""
    if v is None:
        return "NA"
    if isinstance(v, bool):
        return "TRUE" if v else "FALSE"
    if isinstance(v, int | float):
        return r_num_str(v)
    if isinstance(v, bytes):
        return v.decode("utf-8", "surrogateescape")
    return str(v)


def _gdx_text(v: float) -> str:
    """``as.character()`` of a value read from a GDX file (GAMS NA is R's ``NA``, UNDEF is ``NaN``)."""
    return "NA" if is_na(v) else r_num_str(v)


def _paste(parts: Sequence[object], sep: str = "") -> list[str]:
    """``paste(..., sep = sep)``: term by term over recycled vectors, a zero-length argument as ``""``.

    All arguments zero-length gives ``character(0)`` (``[]``); otherwise the separator is
    emitted between every pair of arguments, so ``paste("a", NULL)`` is ``"a "``.
    """
    vectors = [_r_vec(part) for part in parts]
    n = max((len(v) for v in vectors), default=0)
    if n == 0:
        return []
    return [sep.join("" if not v else _as_character(v[k % len(v)]) for v in vectors) for k in range(n)]


def _paste1(parts: Sequence[object]) -> str:
    """``paste0(...)`` of scalar-or-NULL arguments feeding an ``if (file.exists(...))``: one string.

    A longer result would make that ``if`` fail with ``the condition has length > 1``.
    """
    values = _paste(parts)
    if len(values) > 1:
        raise RParityError("the condition has length > 1")
    return values[0]


def _cell(values: Sequence[object]) -> object:
    """The value ``out[i, col] <- values`` stores: exactly one element, else R's replacement errors."""
    if len(values) == 0:
        raise RParityError("replacement has length zero")
    if len(values) > 1:
        raise RParityError(f"replacement has {len(values)} rows, data has 1")
    value = values[0]
    if isinstance(value, float) and not isinstance(value, bool) and math.isfinite(value) and value.is_integer():
        return int(value)
    return value


def _r_dollar(obj: object, name: str) -> object:
    """``obj$name`` on an R list: exact name, else a unique partial match, else NULL (``None``)."""
    if obj is None:
        return None
    if not isinstance(obj, Mapping):
        raise RParityError("$ operator is invalid for atomic vectors")
    if name in obj:
        return obj[name]
    partial = [key for key in obj if str(key).startswith(name)]
    return obj[partial[0]] if len(partial) == 1 else None


def _r_index2(obj: object, name: str) -> object:
    """``obj[[name]]`` on an R list: exact name or NULL; NULL stays NULL; anything else is out of bounds."""
    if obj is None:
        return None
    if isinstance(obj, Mapping):
        return obj.get(name)
    raise RParityError("subscript out of bounds")


def _as_numeric(v: object) -> float:
    """``as.numeric()`` of one element: NA (``nan``) for text that is not a number."""
    if v is None:
        return math.nan
    if isinstance(v, bool):
        return 1.0 if v else 0.0
    if isinstance(v, int | float):
        return float(v)
    text = _as_character(v).strip()
    try:
        if text.lower().lstrip("+-").startswith("0x"):
            return float.fromhex(text)
        return float(text)
    except ValueError:
        return math.nan


def _grepl(pattern: str, x: object) -> bool:
    """``grepl(pattern, x)`` for one element: FALSE for NA."""
    return x is not None and re.search(pattern, _as_character(x)) is not None


def _is_true_eq(x: object, target: str) -> bool:
    """``isTRUE(x == target)``: only a length-1, non-NA element equal to ``target`` as text."""
    values = _r_vec(x)
    return len(values) == 1 and values[0] is not None and _as_character(values[0]) == target


def _is_true_gt0(x: object) -> bool:
    """``isTRUE(x > 0)``: numbers numerically, strings by the session collation (``"1" > "0"``)."""
    values = _r_vec(x)
    if len(values) != 1 or values[0] is None:
        return False
    value = values[0]
    if isinstance(value, bool):
        return value
    if isinstance(value, int | float):
        return not math.isnan(value) and value > 0
    return r_collate_key(_as_character(value)) > r_collate_key("0")


def _which_max(values: Sequence[float]) -> int | None:
    """``which.max()``: the index of the first maximum, NA ignored, ``None`` when there is none."""
    best: int | None = None
    for k, v in enumerate(values):
        if math.isnan(v):
            continue
        if best is None or v > values[best]:
            best = k
    return best


def _intern_first(text: str) -> str:
    """One output line as ``system(intern = TRUE)`` hands it over (cut at a NUL byte)."""
    lines = textscan.r_intern_lines(text + "\n")
    return lines[0] if lines else ""


def _make_unique(names: Sequence[str]) -> list[str]:
    """``make.unique(names)``: duplicates get ``.1``, ``.2``, ... avoiding names already present."""
    taken = set(names)
    seen: dict[str, int] = {}
    out: list[str] = []
    for name in names:
        if name in seen:
            k = seen[name]
            candidate = f"{name}.{k}"
            while candidate in taken:
                k += 1
                candidate = f"{name}.{k}"
            seen[name] = k + 1
            taken.add(candidate)
            out.append(candidate)
        else:
            seen[name] = 1
            out.append(name)
    return out


def _find_count(directory: str, pattern: str) -> int:
    """``length(system(paste0("find ", dir, " -name '", pattern, "'"), intern = TRUE))``.

    Every entry below ``directory`` (files and directories, any depth, symlinks not followed)
    whose name matches the shell pattern; 0 when the directory does not exist.
    """
    count = 0
    for _root, dirs, files in os.walk(os.path.expanduser(directory)):
        count += sum(1 for name in dirs + files if fnmatch.fnmatchcase(name, pattern))
    return count


# The deparsed R calls that try() / Rscript print as ``Error in <call> : <message>`` (PORT-033). R shows only the
# first deparse line of a long call, hence the line-108 text ending in ``== `` (Rscript 4.6.1, 2026-10-01); the
# line-91 text is pinned by the rs goldens ``synthetic-bug005*.err``, the Conv texts by ``synthetic-remind*.err``.
_MAGPIE_CHECK_LINE_91 = 'if (runstatistics$stats[["config"]][["model_name"]] == "MAgPIE") {'
_MAGPIE_CHECK_LINE_108 = (
    'if (any(grepl("config", names(runstatistics$stats))) && runstatistics$stats[["config"]][["model_name"]] == '
)
_S80_BOOL_CHECK = "if (s80_bool == 1) {"
_ITERATION_MAX_CHECK = "if (s80_bool == 0 && as.numeric(cm_iteration_max) == iter_no) {"
_ITERATION_MAX_AND = "s80_bool == 0 && as.numeric(cm_iteration_max) == iter_no"


def _model_name_is_magpie(stats: RunStatistics, call: str) -> bool:
    """``if (stats[["config"]][["model_name"]] == "MAgPIE")`` (lines 91 and 108): R's errors for NULL and NA.

    ``call`` is the caller's deparsed ``if`` (``_MAGPIE_CHECK_LINE_91`` / ``_MAGPIE_CHECK_LINE_108``), carried by the
    :class:`RParityError` so that R's ``try()`` text can be reproduced.
    """
    config = stats.config
    if config is None or "model_name" not in config:
        raise RParityError("argument is of length zero", call=call)
    values = vector(config["model_name"])
    if not values:
        raise RParityError("argument is of length zero", call=call)
    if len(values) > 1:
        raise RParityError("the condition has length > 1", call=call)
    if values[0] is None:
        raise RParityError("missing value where TRUE/FALSE needed", call=call)
    return _as_character(values[0]) == "MAgPIE"


def _warn_absent_gdx_symbol(call: str, name: str) -> None:
    """The warning ``gdx2::readGDX`` (``react = "warning"``) raises for an absent symbol, deferred like R's.

    gdx2 wraps a ``try()`` message, so the text ends with a newline that R's warning printer keeps (the blank
    line after the ``In addition:`` block of the rs goldens ``synthetic-remind*.err``).
    """
    warnings.warn(
        RWarning(call, f"Error : User specified to read symbol {name}, but it does not exist in the source file\n"),
        stacklevel=3,
    )


def _is_true_magpie(stats: RunStatistics | None) -> bool:
    """``isTRUE(runstatistics$stats[["config"]][["model_name"]] == "MAgPIE")`` (lines 124, 257, 266, 318)."""
    if stats is None:
        return False
    config = stats.config
    if config is None:
        return False
    return _is_true_eq(config.get("model_name"), "MAgPIE")


def _runtime_seconds(end: RTime | None, start: RTime | None) -> int | None:
    """``as.numeric(round(difftime(end, start, units = "secs"), 0))`` as the cell value.

    ``difftime()`` of a NULL is ``numeric(0)``, whose assignment fails with ``replacement has length zero``.
    """
    if end is None or start is None:
        raise RParityError("replacement has length zero")
    seconds = end.elapsed_since(start)
    return None if seconds is None else int(r_round(seconds, 0))


def _file_path(*parts: object) -> list[str]:
    """``file.path(...)``: ``character(0)`` as soon as one argument is zero-length."""
    vectors = [_r_vec(part) for part in parts]
    if any(not v for v in vectors):
        return []
    return _paste(vectors, sep="/")


# ---------------------------------------------------------------------------
# the directory list (lines 24-32)
# ---------------------------------------------------------------------------


def _normalize_paths(paths: Sequence[str], effects: Effects) -> list[str]:
    """``mydir <- normalizePath(mydir)`` (line 25): the warning for a missing path is not suppressed."""
    out: list[str] = []
    for k, path in enumerate(paths, start=1):
        normalized = normalize_path(path, effects)
        if normalized != "" and not effects.exists(normalized):
            warnings.warn(
                RWarning("normalizePath(mydir)", f'path[{k}]="{normalized}": No such file or directory'),
                stacklevel=3,
            )
        out.append(normalized)
    return out


def _file_info(path: str, effects: Effects) -> tuple[bool | None, float | None]:
    """``file.info(path)[, c("isdir", "mtime")]``: an NA row (``None, None``) when the path cannot be stat-ed."""
    try:
        stat = effects.stat(path)
    except OSError:
        return None, None
    return effects.is_dir(path), stat.mtime


def _directory_rows(
    names: Sequence[str], info: Sequence[tuple[bool | None, float | None]]
) -> list[tuple[str, float | None]]:
    """``a <- a[a[, "isdir"] == TRUE, ]`` (line 31): the rows kept, with the row names ``[.data.frame`` gives them.

    A FALSE drops the row, TRUE keeps it, NA keeps an NA row whose name becomes ``"NA"``; duplicate
    names (two NA rows, the same directory twice, or a directory called ``NA``) go through ``make.unique``.
    """
    kept: list[tuple[str | None, float | None]] = []
    for name, (isdir, mtime) in zip(names, info, strict=True):
        if isdir is None:
            kept.append((None, None))
        elif isdir:
            kept.append((name, mtime))
    raw_labels = [name for name, _ in kept]
    has_na = any(label is None for label in raw_labels)
    duplicated = len(set(raw_labels)) != len(raw_labels)
    if not duplicated:
        duplicated = "NA" in raw_labels
    labels = (
        ["NA" if label is None else label for label in raw_labels] if has_na else [str(label) for label in raw_labels]
    )
    if duplicated:
        labels = _make_unique(labels)
    return [(label, mtime) for label, (_, mtime) in zip(labels, kept, strict=True)]


def _newest_first(rows: Sequence[tuple[str, float | None]]) -> list[str]:
    """``rownames(a[order(a[, "mtime"], decreasing = TRUE), ])`` (line 32): stable, NA last."""
    ordered = sorted(rows, key=lambda row: (row[1] is None, -(row[1] if row[1] is not None else 0.0)))
    return [name for name, _ in ordered]


# ---------------------------------------------------------------------------
# one directory (lines 34-362)
# ---------------------------------------------------------------------------


class _RunScan:
    """The body of the ``for (i in mydir)`` loop for one directory ``ii`` (row ``i = basename(ii)``)."""

    def __init__(
        self, out: StatusTable, ii: str, user: str, on_cluster: bool, detailed: bool, effects: Effects
    ) -> None:
        self.out = out
        self.ii = ii
        self.i = r_basename(ii)
        self.user = user
        self.on_cluster = on_cluster
        self.detailed = detailed
        self.eff = effects
        self._gdx_files: dict[str, GdxFile] = {}
        self.cfg: Any = None
        self.cfgf: str | None = None
        self.stats: RunStatistics | None = None
        self.latest_gdx: str | None = None
        self.cm_iteration_max: list[object] = []

    # -- helpers ----------------------------------------------------------------

    def _gdx(self, path: str) -> GdxFile:
        """One :class:`GdxFile` per candidate file per call (validated on open, records cached)."""
        if path not in self._gdx_files:
            self._gdx_files[path] = GdxFile(path)
        return self._gdx_files[path]

    def _text(self, column: str) -> str:
        """A cell that R only ever holds as a string (``jobInSLURM``, ``RunStatus``, ``Iter``)."""
        value = self.out[self.i, column]
        if not isinstance(value, str):
            raise TypeError(f"{column} is not a string: {value!r}")
        return value

    def _path(self, name: str) -> str:
        return f"{self.ii}/{name}"

    # -- the loop body --------------------------------------------------------------

    def run(self) -> None:
        out, i, ii, eff = self.out, self.i, self.ii, self.eff
        out.register_path(i, ii)
        # line 38
        out[i, "jobInSLURM"] = found_in_slurm(ii, self.user, eff) if self.on_cluster else "NA"
        # lines 41-51
        cfg_matches = config_matches(ii, eff)
        fle = self._path("runstatistics.rda")
        gdx = self._path("fulldata.gdx")
        gdx_non_optimal = self._path("non_optimal.gdx")
        self.fullgms = self._path("full.gms")
        self.fulllog = self._path("full.log")
        self.slurmlog = self._path("slurm.log")
        self.logtxt = self._path("log.txt")
        self.logmagtxt = self._path("log-mag.txt")
        self.abortgdx = self._path("abort.gdx")
        if not eff.exists(self.logmagtxt):
            self.logmagtxt = self.logtxt
        self.gdx_non_optimal = gdx_non_optimal
        # lines 53-64: the latest GDX by o_iterationNumber (outside try), else by mtime
        gdxfiles = [path for path in (gdx, gdx_non_optimal) if eff.exists(path)]
        self.latest_gdx = gdxfiles[0] if gdxfiles else None
        if len(gdxfiles) > 1:
            itergdx = [
                value for path in gdxfiles if (value := read_scalar(self._gdx(path), "o_iterationNumber")) is not None
            ]
            if len(itergdx) == len(gdxfiles):
                best = _which_max(itergdx)
                self.latest_gdx = None if best is None else gdxfiles[best]
            else:
                best = _which_max([eff.stat(path).mtime for path in gdxfiles])
                self.latest_gdx = None if best is None else gdxfiles[best]
        # lines 67-74: cfg and RunType only with a config file
        self.cfg = None
        if not cfg_matches:
            out[i, "RunType"] = "NA"
        else:
            # two or more matches fail here like R's ifelse(grepl("yml$", cfgf), loadConfig(...), load(...))
            self.cfgf = find_config_file(ii, eff)
            if self.cfgf is None:
                raise RParityError("argument is of length zero")
            self.cfg = load_config(self._path(self.cfgf), eff)
            out[i, "RunType"] = col_run_type(ii, eff)
        # lines 76-79
        self.stats = RunStatistics.load(fle, eff)
        self._modelstat()
        self._run_in_app_results()
        self._mif()
        self._iteration_max()
        self._run_status()
        if not self.detailed:  # lines 247-249
            return
        self._warnings()
        self._conv()
        self._calibration()
        self._runtime()
        self._sanity()

    # -- lines 82-102 --------------------------------------------------------------

    def _modelstat(self) -> None:
        out, i, stats = self.out, self.i, self.stats
        out[i, "modelstat"] = "NA"
        if self.latest_gdx is not None:
            o_modelstat = read_first_found(self._gdx(self.latest_gdx), ["o_modelstat", "p80_modelstat"])
            if o_modelstat is not None:
                out[i, "modelstat"] = "".join(_gdx_text(v) for v in o_modelstat).replace("0", ".")
        if out[i, "modelstat"] == "NA" and stats is not None and stats.has("config"):
            if _model_name_is_magpie(stats, _MAGPIE_CHECK_LINE_91):
                if stats.has("modelstat"):
                    out[i, "modelstat"] = "".join(_as_character(v) for v in vector(stats.raw.get("modelstat")))
            elif stats.has("modelstat"):
                # try(out[i, "modelstat"] <- stats[["modelstat"]]): a vector or an empty value fails silently
                values = vector(stats.raw.get("modelstat"))
                if len(values) == 1:
                    out[i, "modelstat"] = None if values[0] is None else _as_character(values[0])
        value = out[i, "modelstat"]
        if isinstance(value, str) and value in EXPLAIN_MODELSTAT:
            out[i, "modelstat"] = f"{value}: {EXPLAIN_MODELSTAT[value]}"

    # -- lines 105-118 ----------------------------------------------------------------

    def _run_in_app_results(self) -> None:
        out, i, eff, stats = self.out, self.i, self.eff, self.stats
        if not self.on_cluster:
            return
        out[i, "runInAppResults"] = "no"
        if stats is None or not stats.has("id"):
            return
        if stats.has("config") and _model_name_is_magpie(stats, _MAGPIE_CHECK_LINE_108):
            ovdir = eff.getenv("MAGPIE_RESULTS_ARCHIVE_PATH") + "/"
        else:
            ovdir = REMIND_RESULTS_ARCHIVE
        ids = _paste([ovdir, stats.raw.get("id"), ".rds"])
        if len(ids) != 1:
            raise RParityError(f"'length = {len(ids)}' in coercion to 'logical(1)'")
        id_file = ids[0]
        if not eff.exists(id_file):
            return
        # all() over no overview.rds is TRUE (BUG-010)
        overviews = eff.glob(f"{ovdir}overview.rds")
        id_mtime = eff.stat(id_file).mtime
        if all(eff.stat(overview).mtime + 600 > id_mtime for overview in overviews):
            out[i, "runInAppResults"] = "yes"

    # -- lines 121-138 ----------------------------------------------------------------

    def _config_file_exists(self) -> bool:
        """``length(cfgf) != 0 && file.exists(paste0(ii, "/", cfgf))``."""
        return self.cfgf is not None and self.eff.exists(self._path(self.cfgf))

    def _remind_file(self, suffix: str) -> str:
        """``paste0(ii, "/REMIND_generic_", cfg[["title"]], suffix)``."""
        return _paste1([self.ii, "/REMIND_generic_", _r_index2(self.cfg, "title"), suffix])

    def _mif(self) -> None:
        out, i, eff = self.out, self.i, self.eff
        out[i, "Mif"] = "NA"
        if not self._config_file_exists():
            return
        if _is_true_magpie(self.stats):
            miffile = self._path("validation.mif")
            out[i, "Mif"] = "yes" if eff.exists(miffile) and eff.stat(miffile).size > 99999 else "no"
            return
        miffile = self._remind_file(".mif")
        sum_err_file = self._remind_file("_summation_errors.csv")
        if not eff.exists(miffile):
            out[i, "Mif"] = "no"
        elif eff.exists(sum_err_file):
            out[i, "Mif"] = "sumErr"
        else:
            out[i, "Mif"] = "yes"

    # -- lines 141-147 ----------------------------------------------------------------

    def _iteration_max(self) -> None:
        gms = _r_dollar(self.cfg, "gms")
        self.cm_iteration_max = _r_vec(_r_dollar(gms, "cm_iteration_max"))
        if _is_true_gt0(_r_dollar(gms, "cm_nash_autoconverge")) and _grepl("nash", self.out[self.i, "RunType"]):
            if self.eff.exists(self.fullgms):
                line = textscan.last_match(self.fullgms, _ITERMAX_LINE)
                self.cm_iteration_max = [] if line is None else [_SEMICOLON.sub("", _AFTER_EQUALS.sub("", line, 1), 1)]

    # -- lines 151-244 ----------------------------------------------------------------

    def _run_status(self) -> None:
        out, i, eff = self.out, self.i, self.eff
        out[i, "Iter"] = "NA"
        out[i, "RunStatus"] = "NA"
        if eff.exists(self.fulllog):
            self._status_from_full_log()
            if self.on_cluster and self._text("RunStatus") == "NA":
                self._cluster_fallbacks()
            elif self._text("RunStatus") == "NA":
                out[i, "RunStatus"] = "Run interrupted"
            self._running_reporting()
            self._abort_infes()
            self._magpie_phase()
        elif eff.exists(self.logtxt) and self._last_line_matches(self.logtxt, "try to acquire model lock"):
            out[i, "RunStatus"] = "Wait REMIND lock"
        else:
            out[i, "RunStatus"] = "full.log missing"

    def _status_from_full_log(self) -> None:
        """Lines 154-157: ``Iter`` from the last ``LOOPS`` line, ``RunStatus`` from the ``*** Status:`` lines."""
        out, i = self.out, self.i
        loop_line = textscan.last_match_forward(self.fulllog, "LOOPS")
        loop = [] if loop_line is None else [_AFTER_EQUALS.sub("", loop_line, 1)]
        if loop:
            out[i, "Iter"] = loop[0]
        if self.cm_iteration_max:
            out[i, "Iter"] = _cell(_paste([out[i, "Iter"], "/", self.cm_iteration_max]))
        statuses = [
            _PLURAL_S.sub("", _STATUS_PREFIX.sub("", line, 1), 1)[:17]
            for line in textscan.all_matches(self.fulllog, _STATUS_LINE)
        ]
        # try(): no line or two or more lines fail the assignment silently (BUG-036 / D-19)
        if len(statuses) == 1:
            out[i, "RunStatus"] = statuses[0]

    def _cluster_fallbacks(self) -> None:
        """Lines 158-199: the interrupt labels, ``Run in progress``, conoptspy and the coupled MAgPIE step."""
        out, i, eff = self.out, self.i, self.eff
        job = self._text("jobInSLURM")
        if job == "no" or re.search("pending$", job):
            if eff.exists(self.logtxt):
                slurmerror = textscan.all_matches(self.logtxt, "slurmstepd.*error")
                for pattern, label in (
                    ("DUE TO TIME LIMIT", "Timeout interrupt"),
                    ("memory|oom-kill", "Memory interrupt"),
                    ("DUE TO PREEMPTION", "Preempt interrupt"),
                    ("DUE TO JOB REQUEUE", "Run requeued"),
                    ("CANCELLED", "Run cancelled"),
                ):
                    if any(re.search(pattern, line) for line in slurmerror):
                        out[i, "RunStatus"] = label
                        break
            else:
                out[i, "RunStatus"] = "Run interrupted" if job == "no" else "Run restarted"
            return
        out[i, "RunStatus"] = "Run in progress"
        gridfiles = eff.glob(self._path("225*/grid*/gmsgrid.log"))
        if gridfiles:
            newest = max(eff.stat(path).mtime for path in gridfiles)
            conoptdelay = r_round((eff.now().timestamp() - newest) / 3600 - 0.049, 1)
            gdxdelay = 1.0
            if self.latest_gdx is not None and eff.exists(self.latest_gdx):
                gdxdelay = (eff.now().timestamp() - eff.stat(self.latest_gdx).mtime) / 3600
            if conoptdelay > 0.1 and gdxdelay > 0.25:
                out[i, "RunStatus"] = f"conoptspy >{niceround(conoptdelay, 1)}h"
        # for coupled REMIND-MAgPIE runs: MAgPIE currently running?
        if eff.exists(self.logtxt):
            last_line = _intern_first(textscan.last_nonempty_line(self.logtxt))
            if last_line == "Starting MAgPIE...":
                cfg_mag = _r_dollar(self.cfg, "cfg_mag")
                results_folder = _r_dollar(cfg_mag, "results_folder")
                # getRunStatus(file.path(cfg$path_magpie, cfg$cfg_mag$results_folder))[["Iter"]] with R's defaults
                magpie_dirs = _file_path(_r_dollar(self.cfg, "path_magpie"), results_folder)
                status_magpie = get_run_status(magpie_dirs, effects=eff).column("Iter")
                coupling_iter = [_COUPLING_ITER.sub(r"\1", _as_character(v)) for v in _r_vec(results_folder)]
                out[i, "RunStatus"] = _cell(_paste(["mag-", coupling_iter, " ", status_magpie]))

    def _running_reporting(self) -> None:
        """Lines 203-209."""
        out, i, eff = self.out, self.i, self.eff
        if self._text("RunStatus") != "Normal completion" or not eff.exists(self.logtxt):
            return
        if self._text("jobInSLURM") == "no":
            return
        startrep = textscan.last_match(self.logtxt, "Starting output generation for")
        endrep = textscan.last_match(self.logtxt, "Finished output generation for")
        if startrep is not None and endrep is None:
            out[i, "RunStatus"] = "Running reporting"

    def _abort_infes(self) -> None:
        """Lines 210-221: ``Abort <regi> <n>*Infes`` from abort.gdx (equality with the threshold, BUG-002 / D-10)."""
        out, i, eff = self.out, self.i, self.eff
        if self._text("RunStatus") != "Execution error" or not eff.exists(self.abortgdx):
            return
        abort = self._gdx(self.abortgdx)
        maxinfes = read_scalar(abort, "cm_abortOnConsecFail")
        cf: pd.DataFrame | None = read_param(abort, "p80_trackConsecFail")
        if maxinfes is None or math.isnan(maxinfes) or not maxinfes > 0 or cf is None:
            return
        regions = list(dict.fromkeys(cf[cf["value"] == maxinfes].iloc[:, 0].tolist()))
        if not regions:
            return
        label = f"{regions[0]} " if len(regions) == 1 else f"{len(regions)}R*"
        out[i, "RunStatus"] = f"Abort {label}{r_num_str(maxinfes)}*Infes"

    def _magpie_phase(self) -> None:
        """Lines 222-239: ``Run MAgPIE <loop|report>`` and ``Wait MAgPIE lock`` from log-mag.txt (or log.txt)."""
        out, i, eff = self.out, self.i, self.eff
        logmagtxt = self.logmagtxt
        if not eff.exists(logmagtxt) or self._text("jobInSLURM") == "no":
            return
        if self._text("RunStatus") != "Normal completion" and re.search("log-mag.txt", logmagtxt) is None:
            return
        startmag = textscan.last_match(logmagtxt, "Preparing MAgPIE")
        endmag = textscan.last_match(logmagtxt, "MAgPIE output was stored")
        if startmag is not None and endmag is None:
            fulllogmag = self.fulllog.replace("output", "magpie/output").replace("-rem-", "-mag-")
            if not eff.exists(fulllogmag):
                fulllogmag = self.fulllog.replace("output", "../magpie/output").replace("-rem-", "-mag-")
            loopmag: list[object] = []
            if eff.exists(fulllogmag):
                loop_line = textscan.last_match_forward(fulllogmag, "LOOPS")
                loopmag = [] if loop_line is None else [_AFTER_EQUALS.sub("", loop_line, 1)]
            if textscan.last_match(logmagtxt, "Start getReport") is not None:
                loopmag = ["report"]
            out[i, "RunStatus"] = _cell(_paste(["Run MAgPIE", loopmag], sep=" "))
        if self._last_line_matches(logmagtxt, "try to acquire model lock"):
            out[i, "RunStatus"] = "Wait MAgPIE lock"

    def _last_line_matches(self, path: str, pattern: str) -> bool:
        """``isTRUE(grepl(pattern, try(system(paste("tail -1", path), intern = TRUE), silent = TRUE)))``."""
        last = textscan.last_line(path)
        return last is not None and re.search(pattern, _intern_first(last)) is not None

    # -- lines 256-275 ----------------------------------------------------------------

    def _warnings(self) -> None:
        out, i, eff = self.out, self.i, self.eff
        out[i, "Warnings"] = "NA"
        magpie = _is_true_magpie(self.stats)
        if eff.exists(self.slurmlog) and magpie:
            out[i, "Warnings"] = textscan.magpie_warnings(self.slurmlog)
        elif eff.exists(self.logtxt) and not magpie:
            out[i, "Warnings"] = textscan.remind_warnings(self.logtxt)

    # -- lines 278-294 ----------------------------------------------------------------

    def _conv(self) -> None:
        out, i, eff = self.out, self.i, self.eff
        out[i, "Conv"] = "NA"
        if not _grepl("nash", out[i, "RunType"]) or self.latest_gdx is None:
            return
        gdx = self._gdx(self.latest_gdx)
        # lines 280-281: readGDX (no react = "silent" here) warns about an absent symbol inside the silent
        # try() and returns NULL; as.numeric(NULL) is numeric(0), not a try-error (PORT-033)
        iter_no = read_scalar(gdx, "o_iterationNumber")
        if iter_no is None:
            _warn_absent_gdx_symbol(
                'readGDX(gdx = latest_gdx, "o_iterationNumber", format = "simplest")', "o_iterationNumber"
            )
        s80_bool = read_scalar(gdx, "s80_bool")
        if s80_bool is None:
            _warn_absent_gdx_symbol(
                'readGDX(gdx = latest_gdx, "s80_bool", type = "Parameter", format = "simplest")', "s80_bool"
            )
            raise RParityError("argument is of length zero", call=_S80_BOOL_CHECK)  # if (numeric(0) == 1)
        if math.isnan(s80_bool):
            raise RParityError("missing value where TRUE/FALSE needed", call=_S80_BOOL_CHECK)
        if s80_bool == 1:
            out[i, "Conv"] = "converged (had INFES)" if eff.exists(self.gdx_non_optimal) else "converged"
        elif s80_bool == 0 and self._iteration_max_reached(iter_no):
            out[i, "Conv"] = "not_converged"
        else:
            p80_repy = read_param(gdx, "p80_repy")
            out[i, "Conv"] = "" if p80_repy is None else _modelstat_digits(p80_repy)

    def _iteration_max_reached(self, iter_no: float | None) -> bool:
        """``as.numeric(cm_iteration_max) == iter_no`` as the right operand of ``&&`` inside ``if ()``.

        A zero-length side makes ``&&`` yield NA and the ``if`` fail with ``missing value where
        TRUE/FALSE needed`` (BUG-038 / D-23); a longer ``cm_iteration_max`` fails in the coercion.
        """
        values = [_as_numeric(v) for v in self.cm_iteration_max]
        if len(values) > 1:  # R blames the && expression, not the if (Rscript 4.6.1 probe, 2026-10-01)
            raise RParityError(f"'length = {len(values)}' in coercion to 'logical(1)'", call=_ITERATION_MAX_AND)
        if not values or iter_no is None or math.isnan(values[0]) or math.isnan(iter_no):
            raise RParityError("missing value where TRUE/FALSE needed", call=_ITERATION_MAX_CHECK)
        return values[0] == iter_no

    # -- lines 298-306 ----------------------------------------------------------------

    def _calibration(self) -> None:
        out, i, eff = self.out, self.i, self.eff
        gms = _r_dollar(self.cfg, "gms")
        calib = _grepl("Calib", out[i, "RunType"]) or _is_true_eq(_r_dollar(gms, "CES_parameters"), "calibrate")
        if not calib or not eff.exists(self.logtxt):
            return
        # grep 'CES calibration iteration' log.txt | grep -Eo '[0-9]{1,2}' | tail -1
        numbers = [
            m
            for line in textscan.all_matches(self.logtxt, "CES calibration iteration")
            for m in _DIGITS_1_2.findall(line)
        ]
        calibiter = numbers[-1] if numbers else None
        if calibiter is not None and _as_numeric(calibiter) > 0:
            out[i, "Iter"] = f"{self._text('Iter')} Clb: {calibiter}"
        if out[i, "Conv"] in ("converged", "converged (had INFES)") and (
            _find_count(self.ii, "fulldata_*.gdx") > 10 or _find_count(self.ii, "input_*.gdx") > 10
        ):
            out[i, "Conv"] = "Clb_converged"  # more than 10 files (BUG-001 / D-09)

    # -- lines 309-314 ----------------------------------------------------------------

    def _runtime(self) -> None:
        out, i, eff, stats = self.out, self.i, self.eff, self.stats
        out[i, "Runtime"] = None
        if stats is None:
            return
        if stats.has("GAMSEnd"):
            out[i, "Runtime"] = _runtime_seconds(stats.timeGAMSEnd, stats.timeGAMSStart)
        elif stats.has("timePrepareStart") and self._text("jobInSLURM") != "no":
            out[i, "Runtime"] = _runtime_seconds(RTime.from_datetime(eff.now()), stats.timePrepareStart)

    # -- lines 317-362 ----------------------------------------------------------------

    def _sanity(self) -> None:
        out, i, eff = self.out, self.i, self.eff
        if not self._config_file_exists() or _is_true_magpie(self.stats):
            return
        miffile = self._remind_file(".mif")
        sum_err_file = self._remind_file("_summation_errors.csv")
        if not eff.exists(miffile):
            return  # next
        out[i, "summationErrors"] = count_unique_variable(sum_err_file) if eff.exists(sum_err_file) else 0
        range_err_file = self._remind_file("_range_errors.txt")
        out[i, "rangeErrors"] = count_rows(range_err_file) if eff.exists(range_err_file) else 0
        fix_err_file = self._path("log_fixOnRef.csv")
        out[i, "fixErrors"] = count_unique_variable(fix_err_file) if eff.exists(fix_err_file) else 0
        project_sum_file = self._path("projectSummations.rds")
        if eff.exists(project_sum_file):
            tmp = pythonize(read_rds(project_sum_file, eff))
            scenario_mip = _r_index2(tmp, "ScenarioMIP")
            out[i, "missingProjVars"] = _cell(_r_vec(_r_index2(scenario_mip, "missingVars")))
            out[i, "projSummationErrors"] = _cell(_r_vec(_r_index2(scenario_mip, "checkSummations")))
            out[i, "projSummationErrorsRegional"] = _cell(_r_vec(_r_index2(scenario_mip, "checkSummationsRegional")))
        else:
            out[i, "missingProjVars"] = None
            out[i, "projSummationErrors"] = None
            out[i, "projSummationErrorsRegional"] = None


def _modelstat_digits(p80_repy: pd.DataFrame) -> str:
    """``paste(p80_repy[, , "modelstat"], collapse = "")``: the modelstat values over the regions in set order."""
    if len(p80_repy.columns) < 2:
        raise RParityError("subscript out of bounds")
    key = p80_repy.columns[-2]
    rows = p80_repy[p80_repy[key] == "modelstat"]
    if rows.empty:
        raise RParityError("subscript out of bounds")
    return "".join(_gdx_text(float(v)) for v in rows["value"].tolist())


# ---------------------------------------------------------------------------
# public entry point
# ---------------------------------------------------------------------------


def get_run_status(
    mydir: PathLike | Sequence[PathLike] | None = None,
    sort: str = "nf",
    user: str | None = None,
    detailed: bool = True,
    effects: Effects | None = None,
) -> StatusTable:
    """``getRunStatus(mydir, sort, user, detailed)``: the status record of one or several run directories.

    ``mydir`` is a path or a sequence of paths (``dir()`` of the working directory when omitted, as
    in R); ``sort = "nf"`` keeps directories only and visits the newest (directory mtime) first,
    any other value visits the paths as given; ``user`` defaults to the process user
    (``Sys.info()[["user"]]``); ``detailed = False`` stops after ``RunStatus``. Returns the
    :class:`StatusTable` (``to_json_rows()`` for the golden records, ``rows()`` for row views).
    """
    eff = _effects(effects)
    if mydir is None:
        paths = eff.listdir_like_r(".")
    elif isinstance(mydir, str | os.PathLike):
        paths = [os.fspath(mydir)]
    else:
        paths = [os.fspath(path) for path in mydir]
    if user is None:  # line 24
        user = eff.user
    paths = _normalize_paths(paths, eff)  # line 25
    on_cluster = eff.on_cluster  # line 27
    out = StatusTable()  # line 28
    info = [_file_info(path, eff) for path in paths]  # line 30
    rows = _directory_rows(paths, info)  # line 31
    if sort == "nf":  # line 32
        paths = _newest_first(rows)
    for ii in paths:  # lines 34-364
        _RunScan(out, ii, user, on_cluster, detailed, eff).run()
    return out
