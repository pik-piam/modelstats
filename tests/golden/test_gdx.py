"""``modelstats.gdx`` against ``gdx2::readGDX`` run live through ``Rscript`` (the GDX matrix of ``02`` 4.1).

Two comparisons, both with the R package versions the goldens were made with (gdx2 0.6.2,
gamstransfer 3.0.9, magclass 7.5.0):

* the *synthetic matrix*: GDX files written with gams.transfer into a temporary directory (missing,
  empty and zero-valued symbols, sparse records, reordered, swapped, renamed, aliased, duplicated and
  universe domains, unsorted years, non-region labels, special values, ``first_found`` lists, the
  ``o_iterationNumber`` selection groups of ``R/getRunStatus.R:53-64`` and the damaged files of
  BUG-037) are read by one ``Rscript`` process with ``readGDX(format = "simplest", react = "silent")``
  and by :mod:`modelstats.gdx`; values in ``c(x)`` order and the labels of every named dimension must
  agree. The damaged files abort the R process (exit 134) while the port raises (decision D-20);
* the *fixture files*: every non-empty ``*.gdx`` below ``migration/fixtures/p`` is read for the seven
  status symbols by both sides, and the test prints the table of values (``pytest -rP``).

The tests need ``Rscript`` with gdx2 on the PATH and are skipped without it, like the fixture-bound
goldens are skipped without the fixture tree; they never relax a comparison.
"""

from __future__ import annotations

import json
import math
import os
import re
import shutil
import subprocess
from collections.abc import Mapping, Sequence
from dataclasses import dataclass
from pathlib import Path
from typing import Any

import gams.transfer as gt
import pandas as pd
import pytest

from modelstats import gdx
from modelstats.gdx import (
    GdxCandidate,
    GdxFile,
    GdxReadError,
    flat_values,
    is_na,
    magpie_order,
    magpie_roles,
    read_first_found,
    read_param,
    select_latest_gdx,
)
from tests.unit.test_gdx import REGIONS, SOLVEINFO, YEARS, repy_rows, write_gdx

pytestmark = pytest.mark.golden

RSCRIPT = shutil.which("Rscript")
STATUS_SYMBOLS = (
    "o_iterationNumber",
    "o_modelstat",
    "p80_modelstat",
    "s80_bool",
    "p80_repy",
    "cm_abortOnConsecFail",
    "p80_trackConsecFail",
)
"""The seven symbols ``getRunStatus`` reads (``migration/01-test-data-inventory.md`` section 2)."""

R_ENV = {**os.environ, "LC_ALL": "C.utf8", "LANG": "C.utf8", "TZ": "Europe/Berlin"}

# One R process answers every request: reads (file, symbol), first_found (file, names) and the
# selection groups, which run R/getRunStatus.R:54-64 verbatim on the given existing files.
R_DUMP = r"""
suppressMessages({library(gdx2); library(magclass); library(jsonlite)})
args <- commandArgs(trailingOnly = TRUE)
request <- jsonlite::fromJSON(args[1], simplifyVector = FALSE)
fmt <- function(v) ifelse(is.nan(v), "NaN", ifelse(is.na(v), "NA", sprintf("%.17g", v)))
describe <- function(x) {
  if (is.null(x)) return(list(null = TRUE))
  sets <- getSets(x, fulldim = TRUE)
  items <- list()
  for (s in sets[!is.na(sets)]) items[[s]] <- as.list(getItems(x, dim = s))
  list(values = as.list(fmt(as.vector(x))), dim = as.list(dim(x)), items = items)
}
reads <- lapply(request$reads, function(r) {
  x <- suppressWarnings(readGDX(r$file, r$symbol, format = "simplest", react = "silent"))
  list(file = r$file, symbol = r$symbol, result = describe(x))
})
first <- lapply(request$first_found, function(r) {
  x <- suppressWarnings(readGDX(r$file, unlist(r$names), format = "first_found", react = "silent"))
  list(file = r$file, names = r$names, result = describe(x))
})
selections <- lapply(request$selections, function(r) {
  gdxfiles <- unlist(r$files)
  latest_gdx <- head(gdxfiles, 1)
  if (length(gdxfiles) > 1) {
    itergdx <- as.numeric(unlist(lapply(gdxfiles, readGDX, "o_iterationNumber",
                                        format = "simplest", react = "silent")))
    if (length(itergdx) == length(gdxfiles)) {
      latest_gdx <- gdxfiles[which.max(itergdx)]
    } else {
      fileInfo <- file.info(gdxfiles)
      latest_gdx <- rownames(fileInfo)[which.max(fileInfo$mtime)]
    }
  }
  list(id = r$id, latest = if (length(latest_gdx) == 0) NULL else latest_gdx)
})
answer <- list(reads = reads, first_found = first, selections = selections)
cat(toJSON(answer, auto_unbox = TRUE, null = "null"))
"""


@pytest.fixture(scope="module")
def rscript() -> str:
    if RSCRIPT is None:
        pytest.skip("Rscript is not on the PATH")
    probe = subprocess.run(
        [
            RSCRIPT,
            "-e",
            "cat(requireNamespace('gdx2', quietly = TRUE), requireNamespace('jsonlite', quietly = TRUE))",
        ],
        capture_output=True,
        text=True,
        check=False,
        env=R_ENV,
    )
    if probe.returncode != 0 or "FALSE" in probe.stdout:
        pytest.skip(f"R lacks gdx2 or jsonlite: {probe.stdout} {probe.stderr}")
    return RSCRIPT


def r_dump(rscript: str, workdir: Path, request: Mapping[str, Any]) -> dict[str, Any]:
    """Run ``R_DUMP`` on ``request`` and return its JSON answer (the test fails on an R error)."""
    request_file = workdir / "request.json"
    request_file.write_text(json.dumps(request), encoding="utf-8")
    script = workdir / "dump.R"
    script.write_text(R_DUMP, encoding="utf-8")
    result = subprocess.run(
        [rscript, str(script), str(request_file)], capture_output=True, text=True, check=False, env=R_ENV
    )
    assert result.returncode == 0, result.stderr
    answer: dict[str, Any] = json.loads(result.stdout)
    return answer


# ---------------------------------------------------------------------------------------------
# Comparison
# ---------------------------------------------------------------------------------------------


def same_number(r_text: str, value: float) -> bool:
    """R's ``%.17g`` text (``NA``/``NaN``/``Inf``) against a Python float, sign of zero included."""
    if r_text == "NA":
        return is_na(value)
    if r_text == "NaN":
        return math.isnan(value) and not is_na(value)
    if r_text in ("Inf", "-Inf"):
        return value == (math.inf if r_text == "Inf" else -math.inf)
    expected = float(r_text)
    return expected == value and math.copysign(1.0, expected) == math.copysign(1.0, value)


def r_dimensions(frame: pd.DataFrame) -> list[tuple[str, list[str]]]:
    """The frame's dimensions as magclass orders and labels them: spatial, temporal, the rest.

    ``as.magpie`` renames ``glob`` to ``GLO`` in the spatial dimension and prefixes bare years with
    ``y`` in the temporal one; the frame keeps the GDX labels, so the comparison applies the renames.
    """
    columns = list(frame.columns[:-1])
    levels = [list(dict.fromkeys(frame[column].astype(str).tolist())) for column in columns]
    spatial, temporal = magpie_roles(levels)
    dimensions = []
    for index in reversed(magpie_order(levels)):
        labels = levels[index]
        if index == spatial:
            labels = ["GLO" if label == "glob" else label for label in labels]
        if index == temporal:
            labels = [re.sub(r"^[a-z]?([0-9]{4})$", r"y\1", label) for label in labels]
        dimensions.append((columns[index], labels))
    return dimensions


def base_name(name: str) -> str:
    """A domain label without the duplicate suffix (R counts from 1, gams.transfer from 0)."""
    return re.sub(r"_[0-9]+$", "", name)


def assert_frame_matches(frame: pd.DataFrame | None, result: Mapping[str, Any], what: str) -> None:
    """The Python frame equals the R object: ``NULL``-ness, ``c(x)`` and every labelled dimension."""
    if result.get("null"):
        assert frame is None, f"{what}: R returned NULL, Python {frame}"
        return
    assert frame is not None, f"{what}: R returned an object, Python None"
    values = flat_values(frame)
    r_values: list[str] = list(result["values"])
    assert len(values) == len(r_values), f"{what}: {len(values)} values, R has {len(r_values)}"
    for position, (r_text, value) in enumerate(zip(r_values, values, strict=True)):
        assert same_number(r_text, value), f"{what}[{position}]: R {r_text}, Python {value!r}"
    if len(frame.columns) == 1:
        return  # a scalar: R's region/year/data placeholders have no counterpart
    r_dims = [(name, list(items)) for name, items in result["items"].items() if items]
    py_dims = r_dimensions(frame)
    assert len(r_dims) == len(py_dims), f"{what}: dimensions {py_dims}, R has {r_dims}"
    for (r_name, r_items), (column, labels) in zip(r_dims, py_dims, strict=True):
        assert base_name(r_name) == base_name(column), f"{what}: dimension {column}, R has {r_name}"
        assert labels == r_items, f"{what}: dimension {column} is {labels}, R has {r_items}"


# ---------------------------------------------------------------------------------------------
# The synthetic matrix
# ---------------------------------------------------------------------------------------------


@dataclass(frozen=True)
class Matrix:
    directory: Path
    reads: list[tuple[str, str]]
    first_found: list[tuple[str, tuple[str, ...]]]
    selections: dict[str, list[str]]
    damaged: dict[str, Path]


def build_matrix(directory: Path) -> Matrix:
    """Write the matrix files and list what R and Python must both read."""
    iteration = [str(i) for i in range(1, 6)]
    remind_sets = {"all_regi": REGIONS, "solveinfo80": SOLVEINFO, "iteration": iteration}
    modelstat = dict.fromkeys(REGIONS, 2.0) | {"SSA": 7.0, "NEU": 5.0, "USA": 13.0}
    alphabetical = sorted(REGIONS)
    special = gt.SpecialValues
    files: dict[str, Path] = {}
    files["complete"] = write_gdx(
        directory / "complete.gdx",
        sets=remind_sets,
        scalars={"o_iterationNumber": 3, "o_modelstat": 2, "s80_bool": 1, "cm_abortOnConsecFail": 2},
        parameters={
            "p80_repy": (["all_regi", "solveinfo80"], repy_rows(modelstat)),
            "p80_trackConsecFail": (["all_regi"], [("EUR", 2.0), ("REF", 1.0), ("CAZ", 0.0)]),
        },
    )
    files["magpie"] = write_gdx(
        directory / "magpie.gdx",
        sets={"t": YEARS},
        parameters={"p80_modelstat": (["t"], [(y, {"y2010": 0.0, "y2030": 4.0}.get(y, 2.0)) for y in YEARS])},
    )
    files["empty"] = write_gdx(
        directory / "empty.gdx",
        sets=remind_sets,
        scalars={"s80_bool": 0},
        parameters={"p80_repy": (["all_regi", "solveinfo80"], []), "p80_trackConsecFail": (["all_regi"], [])},
    )
    files["sparse"] = write_gdx(
        directory / "sparse.gdx",
        sets=remind_sets,
        scalars={"o_iterationNumber": 3, "o_modelstat": 0, "s80_bool": 0},
        parameters={
            "p80_repy": (["all_regi", "solveinfo80"], repy_rows({"EUR": 2.0, "CHA": 4.0})),
            "p80_trackConsecFail": (["all_regi"], [("EUR", 2.0), ("REF", 1.0)]),
        },
    )
    files["setorder"] = write_gdx(
        directory / "setorder.gdx",
        sets={"all_regi": alphabetical, "solveinfo80": SOLVEINFO},
        parameters={
            "p80_repy": (
                ["all_regi", "solveinfo80"],
                list(
                    reversed(
                        list(repy_rows({r: float(i) for i, r in enumerate(alphabetical, 1)}, alphabetical))
                    )
                ),
            )
        },
    )
    files["swapped"] = write_gdx(
        directory / "swapped.gdx",
        sets={"all_regi": ["LAM", "OAS", "EUR"], "solveinfo80": ["solvestat", "modelstat"]},
        parameters={
            "p80_repy": (
                ["solveinfo80", "all_regi"],
                [("modelstat", "EUR", 7.0), ("solvestat", "LAM", 1.0), ("modelstat", "LAM", 2.0)],
            )
        },
    )
    files["renamed"] = write_gdx(
        directory / "renamed.gdx",
        sets={"regi": ["EUR", "CHA", "IND"], "info": ["modelstat", "solvestat"], "it": iteration},
        parameters={
            "p80_repy": (["regi", "info"], [("CHA", "modelstat", 4.0), ("EUR", "solvestat", 1.0)]),
            "p80_trackConsecFail": (["regi", "it"], [("CHA", "3", 2.0), ("IND", "1", 1.0)]),
        },
    )
    files["alias"] = write_gdx(
        directory / "alias.gdx",
        sets={"all_regi": ["LAM", "OAS", "EUR"], "solveinfo80": SOLVEINFO},
        aliases={"regi2": "all_regi"},
        parameters={
            "p80_repy": (["regi2", "solveinfo80"], [("EUR", "modelstat", 5.0), ("OAS", "modelstat", 2.0)]),
            "p80_trackConsecFail": (["regi2"], [("EUR", 1.0)]),
        },
    )
    files["universe"] = write_gdx(
        directory / "universe.gdx",
        sets={"all_regi": ["LAM", "OAS", "EUR"]},
        parameters={"p80_repy": (["*", "*"], [("EUR", "modelstat", 5.0), ("LAM", "modelstat", 2.0)])},
    )
    files["special"] = write_gdx(
        directory / "special.gdx",
        sets={"all_regi": ["LAM", "OAS", "EUR", "CHA"]},
        scalars={
            "s_eps": special.EPS,
            "s_inf": special.POSINF,
            "s_ninf": special.NEGINF,
            "s_undef": special.UNDEF,
            "s_na": special.NA,
            "o_modelstat": special.NA,
            "s80_bool": special.EPS,
        },
        parameters={
            "p_special": (
                ["all_regi"],
                [("LAM", special.EPS), ("OAS", special.NA), ("EUR", special.UNDEF), ("CHA", special.NEGINF)],
            )
        },
        eps_to_zero=False,
    )
    files["shapes"] = write_gdx(
        directory / "shapes.gdx",
        sets={
            "all_regi": ["LAM", "OAS", "EUR"],
            "solveinfo80": ["solvestat", "modelstat"],
            "t": ["y2005", "y1995", "y2000"],
            "big": ["LAM", "EU27", "World"],
            "cell": ["AFR_1", "EUR_2", "LAM_3"],
            "digits": ["2005", "1995"],
        },
        parameters={
            "p_years": (["t"], [("y1995", 1.0), ("y2000", 2.0), ("y2005", 3.0)]),
            "p_big": (
                ["big", "solveinfo80"],
                [("EU27", "modelstat", 2.0), ("World", "modelstat", 3.0), ("LAM", "solvestat", 1.0)],
            ),
            "p_dup": (["all_regi", "all_regi"], [("LAM", "EUR", 1.0)]),
            "p_3d": (
                ["all_regi", "solveinfo80", "t"],
                [("EUR", "modelstat", "y1995", 4.0), ("LAM", "solvestat", "y2005", 1.0)],
            ),
            "p_cells": (
                ["solveinfo80", "cell", "all_regi"],
                [("modelstat", "EUR_2", "LAM", 6.0), ("solvestat", "AFR_1", "EUR", 1.0)],
            ),
            "p_digits": (["digits", "all_regi"], [("1995", "OAS", 9.0)]),
            "p_glob": (["big"], [("LAM", 1.0)]),
        },
    )
    older, newer = 1_790_700_000.0, 1_790_762_400.0
    for name, iteration_number, mtime in (
        ("tie-a", 5, older),
        ("tie-b", 5, newer),
        ("low", 2, newer),
        ("high", 7, older),
        ("noiter", None, newer),
        ("na-a", special.NA, older),
        ("na-b", special.NA, newer),
    ):
        scalars: dict[str, float] = {"o_modelstat": 2.0}
        if iteration_number is not None:
            scalars["o_iterationNumber"] = iteration_number
        files[name] = write_gdx(directory / f"{name}.gdx", scalars=scalars)
        os.utime(files[name], (mtime, mtime))

    reads = [
        *((files["complete"].name, s) for s in STATUS_SYMBOLS),
        *((files["magpie"].name, s) for s in STATUS_SYMBOLS),
        *((files["empty"].name, s) for s in STATUS_SYMBOLS),
        *((files["sparse"].name, s) for s in STATUS_SYMBOLS),
        (files["setorder"].name, "p80_repy"),
        (files["swapped"].name, "p80_repy"),
        (files["renamed"].name, "p80_repy"),
        (files["renamed"].name, "p80_trackConsecFail"),
        (files["alias"].name, "p80_repy"),
        (files["alias"].name, "p80_trackConsecFail"),
        (files["universe"].name, "p80_repy"),
        *(
            (files["special"].name, s)
            for s in ("s_eps", "s_inf", "s_ninf", "s_undef", "s_na", "p_special", "s80_bool")
        ),
        *(
            (files["shapes"].name, s)
            for s in ("p_years", "p_big", "p_dup", "p_3d", "p_cells", "p_digits", "p_glob")
        ),
    ]
    first_found = [
        (files["complete"].name, gdx.MODELSTAT_SYMBOLS),
        (files["magpie"].name, gdx.MODELSTAT_SYMBOLS),
        (files["empty"].name, gdx.MODELSTAT_SYMBOLS),
        (files["sparse"].name, gdx.MODELSTAT_SYMBOLS),
        (files["special"].name, gdx.MODELSTAT_SYMBOLS),
        (files["shapes"].name, ("nope", "p_years", "p_big")),
    ]
    selections = {
        "single": [files["complete"].name],
        "tie": [files["tie-a"].name, files["tie-b"].name],
        "tie-reversed": [files["tie-b"].name, files["tie-a"].name],
        "higher-second": [files["low"].name, files["high"].name],
        "noiter-newest": [files["high"].name, files["noiter"].name],
        "noiter-first": [files["noiter"].name, files["high"].name],
        "na-skipped": [files["na-a"].name, files["high"].name],
        "all-na": [files["na-a"].name, files["na-b"].name],
    }
    damaged = {
        "text": directory / "text.gdx",
        "empty-file": directory / "empty-file.gdx",
        "truncated": directory / "truncated.gdx",
    }
    damaged["text"].write_text("This is not a GDX file.\n" * 4, encoding="utf-8")
    damaged["empty-file"].write_bytes(b"")
    data = files["complete"].read_bytes()
    damaged["truncated"].write_bytes(data[: len(data) // 2])
    return Matrix(directory, reads, first_found, selections, damaged)


@pytest.fixture(scope="module")
def matrix(tmp_path_factory: pytest.TempPathFactory) -> Matrix:
    return build_matrix(tmp_path_factory.mktemp("gdx-matrix"))


@pytest.fixture(scope="module")
def r_matrix(rscript: str, matrix: Matrix) -> dict[str, Any]:
    request = {
        "reads": [{"file": str(matrix.directory / f), "symbol": s} for f, s in matrix.reads],
        "first_found": [{"file": str(matrix.directory / f), "names": list(n)} for f, n in matrix.first_found],
        "selections": [
            {"id": i, "files": [str(matrix.directory / f) for f in fs]} for i, fs in matrix.selections.items()
        ],
    }
    return r_dump(rscript, matrix.directory, request)


def test_matrix_reads_equal_r(matrix: Matrix, r_matrix: Mapping[str, Any]) -> None:
    answers: Sequence[Mapping[str, Any]] = r_matrix["reads"]
    assert len(answers) == len(matrix.reads)
    files = {name: GdxFile(matrix.directory / name) for name, _ in matrix.reads}
    for (name, symbol), answer in zip(matrix.reads, answers, strict=True):
        assert Path(answer["file"]).name == name and answer["symbol"] == symbol
        assert_frame_matches(read_param(files[name], symbol), answer["result"], f"{name}:{symbol}")
    print(f"synthetic matrix: {len(answers)} (file, symbol) reads equal R readGDX")


def test_matrix_first_found_equals_r(matrix: Matrix, r_matrix: Mapping[str, Any]) -> None:
    answers: Sequence[Mapping[str, Any]] = r_matrix["first_found"]
    assert len(answers) == len(matrix.first_found)
    for (name, names), answer in zip(matrix.first_found, answers, strict=True):
        frame = read_first_found(matrix.directory / name, names)
        assert_frame_matches(frame, answer["result"], f"{name}:first_found{names}")


def test_matrix_selection_equals_r(matrix: Matrix, r_matrix: Mapping[str, Any]) -> None:
    answers: Sequence[Mapping[str, Any]] = r_matrix["selections"]
    assert [a["id"] for a in answers] == list(matrix.selections)
    for answer in answers:
        names = matrix.selections[answer["id"]]
        candidates = [
            GdxCandidate(GdxFile(p), p.stat().st_mtime) for p in (matrix.directory / n for n in names)
        ]
        chosen = select_latest_gdx(candidates)
        expected = None if answer["latest"] is None else Path(answer["latest"]).name
        assert (None if chosen is None else chosen.path.name) == expected, answer["id"]


def test_damaged_files_abort_r_and_raise_here(rscript: str, matrix: Matrix) -> None:
    for kind, path in matrix.damaged.items():
        with pytest.raises(GdxReadError):
            read_param(path, "o_iterationNumber")
        code = (
            f'x <- try(gdx2::readGDX("{path}", "o_iterationNumber", format = "simplest", react = "silent"))'
            "; cat(class(x))"
        )
        result = subprocess.run([rscript, "-e", code], capture_output=True, text=True, check=False, env=R_ENV)
        assert result.returncode != 0, f"{kind}: R survived with {result.stdout!r}"
        print(f"damaged {kind}: R exit {result.returncode}, Python GdxReadError")


# ---------------------------------------------------------------------------------------------
# The fixture files
# ---------------------------------------------------------------------------------------------


def fixture_gdx_files(fixtures_dir: Path) -> list[Path]:
    return sorted(p for p in (fixtures_dir / "p").rglob("*.gdx") if p.stat().st_size > 0)


def summary(frame: pd.DataFrame | None) -> str:
    if frame is None:
        return "-"
    values = flat_values(frame)
    if len(values) == 1:
        return gdx.number_text(values[0])
    if "modelstat" in set(frame.iloc[:, -2].astype(str)):
        return f"modelstat={gdx.repy_modelstat_text(frame)}"
    return "".join(gdx.number_text(v) for v in values)


def test_fixture_gdx_files_equal_r(rscript: str, fixtures_dir: Path, tmp_path: Path) -> None:
    files = fixture_gdx_files(fixtures_dir)
    # 01-test-data-inventory.md section 8 counts 19; the snapshot holds 18 non-empty files (2026-10-01)
    assert len(files) == 18, [str(p) for p in files]
    request = {
        "reads": [{"file": str(p), "symbol": s} for p in files for s in STATUS_SYMBOLS],
        "first_found": [],
        "selections": [],
    }
    answers: Sequence[Mapping[str, Any]] = r_dump(rscript, tmp_path, request)["reads"]
    assert len(answers) == len(files) * len(STATUS_SYMBOLS)
    rows = []
    position = 0
    for path in files:
        file = GdxFile(path)
        cells = []
        for symbol in STATUS_SYMBOLS:
            answer = answers[position]
            position += 1
            assert answer["file"] == str(path) and answer["symbol"] == symbol
            frame = read_param(file, symbol)
            assert_frame_matches(frame, answer["result"], f"{path.relative_to(fixtures_dir)}:{symbol}")
            cells.append(summary(frame))
        rows.append((str(path.relative_to(fixtures_dir / "p" / "projects")), cells))
    width = max(len(r[0]) for r in rows)
    print(f"{'file':<{width}}  " + "  ".join(f"{s[:12]:>12}" for s in STATUS_SYMBOLS))
    for name, cells in rows:
        print(f"{name:<{width}}  " + "  ".join(f"{c[:12]:>12}" for c in cells))
    print(f"fixture GDX: {len(files)} files x {len(STATUS_SYMBOLS)} symbols equal R readGDX")
