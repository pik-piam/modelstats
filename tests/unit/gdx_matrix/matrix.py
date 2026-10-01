"""The synthetic GDX test matrix of ``02-python-libraries.md`` section 4.1 and its R oracle.

``python tests/unit/gdx_matrix/matrix.py`` rewrites every file of the matrix next to this
script (gams.transfer for the regular cases, the low-level gdxcc API for symbols declared with
zero records, plain bytes for the corrupt files) and regenerates ``r_expected.json`` from
``oracle.R``. The committed JSON lets ``test_gdx.py`` compare the Python readers with R on a
machine without R; with R installed the test also checks that the JSON is current.

Every case is a dict of symbols; sets are written in the order given (the GDX set order that
``readGDX`` keeps), parameters as records (sparse: a missing record is a GDX zero).
"""

from __future__ import annotations

import json
import shutil
import subprocess
import sys
from pathlib import Path
from typing import Any

HERE = Path(__file__).resolve().parent
ORACLE = HERE / "oracle.R"
EXPECTED = HERE / "r_expected.json"

REGIONS = ["LAM", "OAS", "SSA", "EUR", "NEU", "MEA", "REF", "CAZ", "CHA", "IND", "JPN", "USA"]
SOLVEINFO = ["solvestat", "modelstat", "resusd", "objval"]
ITERATIONS = [str(i) for i in range(1, 6)]
STATUS_SYMBOLS = (
    "o_iterationNumber",
    "s80_bool",
    "o_modelstat",
    "p80_modelstat",
    "p80_repy",
    "cm_abortOnConsecFail",
    "p80_trackConsecFail",
)

#: The corrupt files: R aborts the process on them (BUG-037), Python raises GdxError (D-20).
CORRUPT = ("not_a_gdx.gdx", "empty_file.gdx", "truncated.gdx")


def repy(regions: list[str], modelstat: dict[str, float], info: list[str] = SOLVEINFO) -> list[list[Any]]:
    """p80_repy records for the regions in ``modelstat`` (solvestat 1, resusd 10.5, objval 100.25)."""
    values = {"solvestat": 1.0, "resusd": 10.5, "objval": 100.25}
    rows: list[list[Any]] = []
    for region in regions:
        if region in modelstat:
            rows.extend([region, item, modelstat[region] if item == "modelstat" else values[item]] for item in info)
    return rows


def regular_cases() -> dict[str, dict[str, Any]]:
    """Cases written with gams.transfer: name -> {symbol: spec}.

    Spec forms: ``("set", [elements])``, ``("alias", set_name)``, ``("scalar", value)``,
    ``("param", [domain names], [[key..., value], ...])``.
    """
    sv = _special_values()
    dense_repy = {r: 2.0 for r in REGIONS}
    dense_repy["JPN"] = 7.0
    return {
        # every status symbol as in a real REMIND file; p80_trackConsecFail sparse (EUR 2, REF 1)
        "full.gdx": {
            "all_regi": ("set", REGIONS),
            "solveinfo80": ("set", SOLVEINFO),
            "iteration": ("set", ITERATIONS),
            "o_iterationNumber": ("scalar", 5.0),
            "s80_bool": ("scalar", 1.0),
            "o_modelstat": ("scalar", 2.0),
            "cm_abortOnConsecFail": ("scalar", 2.0),
            "cm_iteration_max": ("scalar", 50.0),
            "p80_repy": ("param", ["all_regi", "solveinfo80"], repy(REGIONS, dense_repy)),
            "p80_trackConsecFail": ("param", ["all_regi"], [["EUR", 2.0], ["REF", 1.0]]),
        },
        # only the sets and o_modelstat: every other status symbol is absent
        "missing.gdx": {
            "all_regi": ("set", REGIONS),
            "solveinfo80": ("set", SOLVEINFO),
            "o_modelstat": ("scalar", 5.0),
        },
        # no o_modelstat: first_found falls through to p80_modelstat(t); y2005 has no record (sparse zero)
        "first_found_second.gdx": {
            "t": ("set", ["y1995", "y2000", "y2005", "y2010"]),
            "p80_modelstat": ("param", ["t"], [["y1995", 2.0], ["y2000", 2.0], ["y2010", 2.0]]),
        },
        # p80_repy with records for EUR and CHA only: dense reconstruction over all 12 regions
        "sparse_zeros.gdx": {
            "all_regi": ("set", REGIONS),
            "solveinfo80": ("set", SOLVEINFO),
            "o_iterationNumber": ("scalar", 3.0),
            "s80_bool": ("scalar", 0.0),
            "o_modelstat": ("scalar", 2.0),
            "p80_repy": ("param", ["all_regi", "solveinfo80"], repy(REGIONS, {"EUR": 2.0, "CHA": 4.0})),
        },
        # domain sets in another order than the real files: the digit string follows the file
        "reordered.gdx": {
            "all_regi": ("set", sorted(REGIONS)),
            "solveinfo80": ("set", ["modelstat", "solvestat", "objval", "resusd"]),
            "o_modelstat": ("scalar", 2.0),
            "p80_repy": (
                "param",
                ["all_regi", "solveinfo80"],
                repy(
                    sorted(REGIONS),
                    {r: float(i + 1) for i, r in enumerate(sorted(REGIONS))},
                    ["modelstat", "solvestat", "objval", "resusd"],
                ),
            ),
        },
        # domains named differently (regi, info); B2 has no record
        "altnames.gdx": {
            "regi": ("set", ["A1", "B2", "C3"]),
            "info": ("set", SOLVEINFO),
            "p80_repy": ("param", ["regi", "info"], repy(["A1", "B2", "C3"], {"A1": 2.0, "C3": 4.0})),
        },
        # p80_repy declared over an alias of all_regi
        "alias.gdx": {
            "all_regi": ("set", REGIONS),
            "regi": ("alias", "all_regi"),
            "solveinfo80": ("set", SOLVEINFO),
            "p80_repy": ("param", ["regi", "solveinfo80"], repy(REGIONS, {"EUR": 2.0, "CHA": 4.0})),
        },
        # universe domains: R cannot restore zeros, missing combinations are NA
        "universe.gdx": {
            "all_regi": ("set", REGIONS),
            "solveinfo80": ("set", SOLVEINFO),
            "p80_repy": ("param", ["*", "*"], [["EUR", "modelstat", 2.0], ["CHA", "objval", 7.0]]),
        },
        # iteration tie: both candidates at iteration 5 (selection picks the first, p2)
        "tie_a.gdx": {
            "o_iterationNumber": ("scalar", 5.0),
            "s80_bool": ("scalar", 1.0),
            "o_modelstat": ("scalar", 2.0),
        },
        "tie_b.gdx": {
            "o_iterationNumber": ("scalar", 5.0),
            "s80_bool": ("scalar", 0.0),
            "o_modelstat": ("scalar", 5.0),
        },
        # no o_iterationNumber at all
        "noiter.gdx": {
            "s80_bool": ("scalar", 0.0),
            "o_modelstat": ("scalar", 5.0),
        },
        # zero-valued scalars stored as explicit records
        "zero_scalars.gdx": {
            "o_iterationNumber": ("scalar", 0.0),
            "s80_bool": ("scalar", 0.0),
            "o_modelstat": ("scalar", 0.0),
        },
        # GAMS special values as scalars and inside p80_repy
        "special.gdx": {
            "sv_eps": ("scalar", sv["EPS"]),
            "sv_inf": ("scalar", sv["POSINF"]),
            "sv_neginf": ("scalar", sv["NEGINF"]),
            "sv_undef": ("scalar", sv["UNDEF"]),
            "sv_na": ("scalar", sv["NA"]),
            "all_regi": ("set", REGIONS[:5]),
            "solveinfo80": ("set", SOLVEINFO),
            "p80_repy": (
                "param",
                ["all_regi", "solveinfo80"],
                [
                    ["LAM", "modelstat", sv["EPS"]],
                    ["OAS", "modelstat", sv["POSINF"]],
                    ["SSA", "modelstat", sv["NEGINF"]],
                    ["EUR", "modelstat", sv["UNDEF"]],
                    ["NEU", "modelstat", sv["NA"]],
                    ["LAM", "objval", 1.5],
                ],
            ),
        },
        # year-like elements out of chronological order: magclass keeps the file order
        "years.gdx": {
            "t": ("set", ["y2010", "y1995", "y2000"]),
            "p80_modelstat": ("param", ["t"], [["y2010", 3.0], ["y1995", 2.0], ["y2000", 7.0]]),
        },
        # a 2-d parameter: pins the flattening order of c(<magpie>) (first domain fastest)
        "twod.gdx": {
            "all_regi": ("set", REGIONS[:3]),
            "iteration": ("set", ["1", "2"]),
            "p2": (
                "param",
                ["all_regi", "iteration"],
                [["LAM", "1", 11.0], ["LAM", "2", 12.0], ["OAS", "1", 21.0], ["SSA", "2", 32.0]],
            ),
        },
    }


def _special_values() -> dict[str, float]:
    import gams.transfer as gt

    return {
        "EPS": gt.SpecialValues.EPS,
        "POSINF": gt.SpecialValues.POSINF,
        "NEGINF": gt.SpecialValues.NEGINF,
        "UNDEF": gt.SpecialValues.UNDEF,
        "NA": gt.SpecialValues.NA,
    }


def write_regular(path: Path, spec: dict[str, Any]) -> None:
    import gams.transfer as gt
    import gamspy_base
    import pandas as pd

    container = gt.Container(system_directory=gamspy_base.directory)
    for name, entry in spec.items():
        kind = entry[0]
        if kind == "set":
            gt.Set(container, name, records=entry[1], description=name)
        elif kind == "alias":
            gt.Alias(container, name, container[entry[1]])
        elif kind == "scalar":
            gt.Parameter(container, name, records=float(entry[1]), description=name)
        elif kind == "param":
            domains = [d if d == "*" else container[d] for d in entry[1]]
            columns = [f"d{i}" for i in range(len(domains))] + ["value"]
            gt.Parameter(container, name, domains, records=pd.DataFrame(entry[2], columns=columns), description=name)
        else:
            raise ValueError(kind)
    container.write(str(path))


def write_empty_records(path: Path) -> None:
    """Symbols declared with zero records (as GAMS writes a zero scalar), only possible with gdxcc."""
    import gamspy_base
    from gams.core.gdx import gdxcc

    def values(level: float) -> Any:
        array = gdxcc.doubleArray(gdxcc.GMS_VAL_MAX)
        for i in range(gdxcc.GMS_VAL_MAX):
            array[i] = 0.0
        array[gdxcc.GMS_VAL_LEVEL] = level
        return array

    handle = gdxcc.new_gdxHandle_tp()
    created, message = gdxcc.gdxCreateD(handle, gamspy_base.directory, gdxcc.GMS_SSSIZE)
    assert created, message
    try:
        opened, error = gdxcc.gdxOpenWrite(handle, str(path), "modelstats gdx matrix")
        assert opened, error
        for set_name, elements in (("all_regi", REGIONS), ("solveinfo80", SOLVEINFO)):
            assert gdxcc.gdxDataWriteStrStart(handle, set_name, set_name, 1, gdxcc.GMS_DT_SET, 0)
            for element in elements:
                assert gdxcc.gdxDataWriteStr(handle, [element], values(0.0))
            assert gdxcc.gdxDataWriteDone(handle)
        for scalar in ("o_iterationNumber", "s80_bool", "o_modelstat"):
            assert gdxcc.gdxDataWriteStrStart(handle, scalar, scalar, 0, gdxcc.GMS_DT_PAR, 0)
            assert gdxcc.gdxDataWriteDone(handle)
        for name, domains in (("p80_repy", ["all_regi", "solveinfo80"]), ("p80_trackConsecFail", ["all_regi"])):
            assert gdxcc.gdxDataWriteStrStart(handle, name, name, len(domains), gdxcc.GMS_DT_PAR, 0)
            assert gdxcc.gdxDataWriteDone(handle)
            found, number = gdxcc.gdxFindSymbol(handle, name)
            assert found
            assert gdxcc.gdxSymbolSetDomainX(handle, number, domains)
        assert gdxcc.gdxDataErrorCount(handle) == 0
        gdxcc.gdxClose(handle)
    finally:
        gdxcc.gdxFree(handle)


def write_matrix(directory: Path) -> list[Path]:
    """Write every file of the matrix into ``directory`` and return the paths (corrupt ones last)."""
    directory.mkdir(parents=True, exist_ok=True)
    paths: list[Path] = []
    for name, spec in regular_cases().items():
        path = directory / name
        write_regular(path, spec)
        paths.append(path)
    empty = directory / "empty_records.gdx"
    write_empty_records(empty)
    paths.append(empty)
    full = (directory / "full.gdx").read_bytes()
    (directory / "truncated.gdx").write_bytes(full[: len(full) * 9 // 10])
    (directory / "not_a_gdx.gdx").write_bytes(b"This is not a GDX file; it stands in for a corrupt fulldata.gdx.\n")
    (directory / "empty_file.gdx").write_bytes(b"")
    paths.extend(directory / name for name in CORRUPT)
    return paths


def run_oracle(files: list[Path], rscript: str = "Rscript") -> dict[str, Any]:
    """Run ``oracle.R`` on ``files`` and return its JSON keyed by file name.

    Valid files go through one R process. Each corrupt file (``CORRUPT``) runs alone, because
    gamstransfer aborts the R process on it (BUG-037); the result records the exit status.
    """
    valid = [f for f in files if f.name not in CORRUPT]
    corrupt = [f for f in files if f.name in CORRUPT]
    result: dict[str, Any] = {}
    if valid:
        result.update(oracle_process(valid, rscript))
    for file in corrupt:
        result.update(oracle_process([file], rscript))
    return {Path(key).name: value for key, value in result.items()}


def oracle_process(files: list[Path], rscript: str) -> dict[str, Any]:
    """One R process over ``files``; the result is keyed by ``str(file)`` exactly as passed.

    When the process dies (BUG-037) every file maps to ``{"r_process": {returncode, stderr_tail}}``.
    """
    import tempfile

    with tempfile.TemporaryDirectory() as tmp:
        out = Path(tmp) / "oracle.json"
        proc = subprocess.run(
            [rscript, str(ORACLE), str(out), *map(str, files)],
            capture_output=True,
            text=True,
            errors="replace",
        )
        if proc.returncode != 0 or not out.exists():
            tail = (proc.stderr or proc.stdout).strip().splitlines()[-3:]
            return {str(f): {"r_process": {"returncode": proc.returncode, "stderr_tail": tail}} for f in files}
        data = json.loads(out.read_text(encoding="utf-8"))
    return {str(key): value for key, value in data.items()}


def main(argv: list[str]) -> int:
    rscript = shutil.which("Rscript")
    files = write_matrix(HERE)
    print(f"wrote {len(files)} files to {HERE}")
    if rscript is None:
        print("Rscript not found: r_expected.json left unchanged", file=sys.stderr)
        return 1
    expected = run_oracle(files, rscript)
    EXPECTED.write_text(json.dumps(expected, indent=1, sort_keys=True) + "\n", encoding="utf-8")
    print(f"wrote {EXPECTED}")
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
