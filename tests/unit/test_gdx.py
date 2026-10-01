"""``modelstats.gdx`` on synthetic GDX files written with gams.transfer (no fixtures, no R).

The expected values are the ``gdx2::readGDX`` results recorded while the module was written (gdx2 0.6.2,
gamstransfer 3.0.9, magclass 7.5.0, R 4.6.1, 2026-10-01); each is quoted next to its assertion. The
golden tier (``tests/golden/test_gdx.py``) recomputes the whole matrix with ``Rscript`` and adds the
fixture files; this tier pins the semantics so that they stay checked without R.
"""

from __future__ import annotations

import math
import os
from collections.abc import Mapping, Sequence
from pathlib import Path

import gams.transfer as gt
import gamspy_base
import pandas as pd
import pytest
from gams.core.gdx import gdxcc

from modelstats import gdx
from modelstats.gdx import (
    VALUE,
    DomainKind,
    GdxCandidate,
    GdxFile,
    GdxReadError,
    SymbolKind,
    dense_frame,
    domain_labels,
    explain_modelstat,
    flat_values,
    is_na,
    magpie_order,
    modelstat_text,
    number_text,
    read_first_found,
    read_param,
    read_scalar,
    read_symbol_table,
    read_typed_scalar,
    repy_modelstat_text,
    select_element,
    select_latest_gdx,
    which_max,
)

REGIONS = ["LAM", "OAS", "SSA", "EUR", "NEU", "MEA", "REF", "CAZ", "CHA", "IND", "JPN", "USA"]
"""``all_regi`` in the order of the real REMIND files (``migration/harness/synthetic_gdx.py``)."""
SOLVEINFO = ["solvestat", "modelstat", "resusd", "objval"]
YEARS = [f"y{year}" for year in (1995, 2000, 2005, 2010, 2015, 2020, 2025, 2030, 2035, 2040)]

type Rows = Sequence[tuple[object, ...]]
type Parameters = Mapping[str, tuple[Sequence[str], Rows]]


def write_gdx(
    path: Path,
    *,
    sets: Mapping[str, Sequence[str]] | None = None,
    aliases: Mapping[str, str] | None = None,
    scalars: Mapping[str, float] | None = None,
    parameters: Parameters | None = None,
    eps_to_zero: bool = True,
) -> Path:
    """Write a GDX with gams.transfer: ``parameters`` maps a name to its domain names and record rows.

    A domain name is a set, an alias or ``*``; a parameter with no rows is declared without records.
    Rows are ``(label, ..., value)`` tuples. ``eps_to_zero=False`` keeps a real GAMS ``EPS``.
    """
    m = gt.Container(system_directory=gamspy_base.directory)
    for name, elements in (sets or {}).items():
        gt.Set(m, name, records=list(elements))
    for alias, parent in (aliases or {}).items():
        gt.Alias(m, alias, m[parent])
    for name, value in (scalars or {}).items():
        gt.Parameter(m, name, records=value)
    for name, (domain, rows) in (parameters or {}).items():
        domain_objects = [m[d] if d != "*" else "*" for d in domain]
        columns = [*(f"d{i}" for i in range(len(domain))), VALUE]
        records = pd.DataFrame(list(rows), columns=columns) if rows else None
        gt.Parameter(m, name, domain_objects, records=records)
    m.write(str(path), eps_to_zero=eps_to_zero)
    return path


def repy_rows(modelstat: Mapping[str, float], regions: Sequence[str] = REGIONS) -> Rows:
    """``p80_repy`` records for the regions in ``modelstat`` (the other solveinfo fields fixed)."""
    rows: list[tuple[object, ...]] = []
    for region in regions:
        if region in modelstat:
            rows += [
                (region, "solvestat", 1.0),
                (region, "modelstat", modelstat[region]),
                (region, "resusd", 10.5),
                (region, "objval", 100.25),
            ]
    return rows


@pytest.fixture(scope="module")
def remind_gdx(tmp_path_factory: pytest.TempPathFactory) -> Path:
    """A REMIND-shaped file with all seven status symbols, ``p80_repy`` dense over the 12 regions."""
    modelstat = dict.fromkeys(REGIONS, 2.0) | {"SSA": 7.0, "NEU": 5.0}
    return write_gdx(
        tmp_path_factory.mktemp("gdx") / "fulldata.gdx",
        sets={"all_regi": REGIONS, "solveinfo80": SOLVEINFO, "iteration": [str(i) for i in range(1, 6)]},
        scalars={"o_iterationNumber": 3, "o_modelstat": 2, "s80_bool": 1, "cm_abortOnConsecFail": 2},
        parameters={
            "p80_repy": (["all_regi", "solveinfo80"], repy_rows(modelstat)),
            "p80_trackConsecFail": (["all_regi"], [("EUR", 2.0), ("REF", 1.0), ("CAZ", 0.0)]),
        },
    )


# ---------------------------------------------------------------------------------------------
# Symbol table
# ---------------------------------------------------------------------------------------------


def test_symbol_kinds_are_the_gdx_library_constants() -> None:
    assert SymbolKind.SET.value == gdxcc.GMS_DT_SET
    assert SymbolKind.PARAMETER.value == gdxcc.GMS_DT_PAR
    assert SymbolKind.VARIABLE.value == gdxcc.GMS_DT_VAR
    assert SymbolKind.EQUATION.value == gdxcc.GMS_DT_EQU
    assert SymbolKind.ALIAS.value == gdxcc.GMS_DT_ALIAS


def test_symbol_table_describes_domains_and_aliases(tmp_path: Path) -> None:
    path = write_gdx(
        tmp_path / "t.gdx",
        sets={"all_regi": ["LAM", "EUR"], "solveinfo80": ["modelstat"]},
        aliases={"regi2": "all_regi"},
        scalars={"o_iterationNumber": 3},
        parameters={
            "p80_repy": (["all_regi", "solveinfo80"], [("LAM", "modelstat", 2.0)]),
            "p_alias": (["regi2"], [("EUR", 1.0)]),
            "p_univ": (["*", "*"], [("x", "y", 1.0)]),
            "p_empty": (["all_regi"], []),
        },
    )
    table = read_symbol_table(path)
    assert list(table) == [
        "all_regi",
        "solveinfo80",
        "regi2",
        "o_iterationNumber",
        "p80_repy",
        "p_alias",
        "p_univ",
        "p_empty",
    ]
    assert table["o_iterationNumber"].kind is SymbolKind.PARAMETER
    assert table["o_iterationNumber"].dimension == 0
    assert table["o_iterationNumber"].domain == ()
    assert table["o_iterationNumber"].domain_kind is DomainKind.NONE
    assert table["p80_repy"].domain == ("all_regi", "solveinfo80")
    assert table["p80_repy"].domain_kind is DomainKind.REGULAR
    assert table["p80_repy"].record_count == 1
    assert table["regi2"].kind is SymbolKind.ALIAS
    assert table["regi2"].alias_of == "all_regi"
    assert table["p_alias"].domain == ("regi2",)
    assert table["p_univ"].domain == ("*", "*")
    assert table["p_univ"].domain_kind is DomainKind.NONE
    assert table["p_empty"].record_count == 0
    assert table["all_regi"].kind is SymbolKind.SET


def test_domain_labels_follow_the_gams_transfer_convention(tmp_path: Path) -> None:
    assert domain_labels(["all_regi", "solveinfo80"]) == ("all_regi", "solveinfo80")
    assert domain_labels(["*", "all_regi"]) == ("uni", "all_regi")
    assert domain_labels(["*", "*"]) == ("uni_0", "uni_1")
    assert domain_labels(["all_regi", "solveinfo80", "all_regi"]) == (
        "all_regi_0",
        "solveinfo80_1",
        "all_regi_2",
    )
    path = write_gdx(
        tmp_path / "dup.gdx",
        sets={"all_regi": ["LAM", "EUR"], "solveinfo80": ["a"]},
        parameters={"p3": (["all_regi", "solveinfo80", "all_regi"], [("LAM", "a", "EUR", 1.0)])},
    )
    m = gt.Container(system_directory=gamspy_base.directory)
    m.read(str(path))
    assert tuple(m["p3"].records.columns[:-1]) == domain_labels(["all_regi", "solveinfo80", "all_regi"])


# ---------------------------------------------------------------------------------------------
# Absent, empty, zero, scalar
# ---------------------------------------------------------------------------------------------


def test_absent_symbol_is_none_in_every_read_shape(remind_gdx: Path) -> None:
    file = GdxFile(remind_gdx)
    assert read_param(file, "p80_modelstat") is None
    assert read_scalar(file, "nope") is None
    assert read_first_found(file, ["nope", "p80_modelstat"]) is None
    assert file.info("nope") is None


def test_scalar_is_one_value_in_a_one_column_frame(remind_gdx: Path) -> None:
    frame = read_param(remind_gdx, "o_iterationNumber")
    assert frame is not None
    assert frame.columns.tolist() == [VALUE]
    assert flat_values(frame) == [3.0]
    assert read_scalar(remind_gdx, "o_iterationNumber") == 3.0
    assert isinstance(read_scalar(remind_gdx, "s80_bool"), float)


def test_read_scalar_rejects_a_parameter_with_dimensions(remind_gdx: Path) -> None:
    with pytest.raises(GdxReadError, match="not a scalar: 2 dimension"):
        read_scalar(remind_gdx, "p80_repy")


def test_present_parameter_without_records_is_dense_zeros(tmp_path: Path) -> None:
    # R: readGDX(f, "p_empty") -> 3x1x1 magpie of zeros ("values: 0 0 0")
    path = write_gdx(
        tmp_path / "empty.gdx",
        sets={"all_regi": ["LAM", "OAS", "EUR"], "solveinfo80": SOLVEINFO},
        parameters={"p_empty": (["all_regi"], []), "p80_repy": (["all_regi", "solveinfo80"], [])},
    )
    frame = read_param(path, "p_empty")
    assert frame is not None
    assert frame[("all_regi")].tolist() == ["LAM", "OAS", "EUR"]
    assert flat_values(frame) == [0.0, 0.0, 0.0]
    repy = read_param(path, "p80_repy")
    assert repy is not None
    assert len(repy) == 12
    assert repy_modelstat_text(repy) == "000"


def test_dense_frame_of_a_full_parameter_has_the_product_shape(remind_gdx: Path) -> None:
    frame = read_param(remind_gdx, "p80_repy")
    assert frame is not None
    assert frame.columns.tolist() == ["all_regi", "solveinfo80", VALUE]
    assert len(frame) == 12 * 4
    assert pd.api.types.is_float_dtype(frame[VALUE])
    # magclass order: regions (spatial) vary fastest, solveinfo80 is the data dimension
    assert frame["all_regi"].tolist()[:13] == [*REGIONS, "LAM"]
    assert frame["solveinfo80"].tolist()[::12] == SOLVEINFO
    assert repy_modelstat_text(frame) == "227252222222"
    assert frame.set_index(["all_regi", "solveinfo80"])[VALUE]["SSA", "modelstat"] == 7.0


def test_zero_records_and_missing_records_both_read_as_zero(remind_gdx: Path) -> None:
    # p80_trackConsecFail stores EUR 2, REF 1 (CAZ 0 is not written by GDX); the rest is restored
    frame = read_param(remind_gdx, "p80_trackConsecFail")
    assert frame is not None
    assert frame["all_regi"].tolist() == REGIONS
    expected = [2.0 if r == "EUR" else 1.0 if r == "REF" else 0.0 for r in REGIONS]
    assert flat_values(frame) == expected


# ---------------------------------------------------------------------------------------------
# Dense reconstruction and order
# ---------------------------------------------------------------------------------------------


def test_sparse_records_land_at_their_set_positions(tmp_path: Path) -> None:
    # synthetic case gdx-empty-repy (records for EUR 2 and CHA 4 only): golden Conv "000200004000"
    path = write_gdx(
        tmp_path / "sparse.gdx",
        sets={"all_regi": REGIONS, "solveinfo80": SOLVEINFO},
        parameters={"p80_repy": (["all_regi", "solveinfo80"], repy_rows({"EUR": 2.0, "CHA": 4.0}))},
    )
    repy = read_param(path, "p80_repy")
    assert repy is not None
    assert repy_modelstat_text(repy) == "000200004000"


def test_set_order_decides_not_record_order_or_alphabet(tmp_path: Path) -> None:
    alphabetical = sorted(REGIONS)
    modelstat = {region: float(i) for i, region in enumerate(alphabetical, start=1)}
    reversed_rows = list(reversed(list(repy_rows(modelstat, alphabetical))))
    path = write_gdx(
        tmp_path / "order.gdx",
        sets={"all_regi": alphabetical, "solveinfo80": SOLVEINFO},
        parameters={"p80_repy": (["all_regi", "solveinfo80"], reversed_rows)},
    )
    repy = read_param(path, "p80_repy")
    assert repy is not None
    assert repy_modelstat_text(repy) == "123456789101112"
    assert select_element(repy, "modelstat") == [float(i) for i in range(1, 13)]


def test_swapped_domains_follow_magclass_order(tmp_path: Path) -> None:
    # R: readGDX(f, "p_swapped") -> 3x1x2, values "1 0 0 2 0 7"; paste(x[, , "modelstat"]) -> "207"
    path = write_gdx(
        tmp_path / "swapped.gdx",
        sets={"all_regi": ["LAM", "OAS", "EUR"], "solveinfo80": ["solvestat", "modelstat"]},
        parameters={
            "p_swapped": (
                ["solveinfo80", "all_regi"],
                [("modelstat", "EUR", 7.0), ("solvestat", "LAM", 1.0), ("modelstat", "LAM", 2.0)],
            )
        },
    )
    frame = read_param(path, "p_swapped")
    assert frame is not None
    assert frame.columns.tolist() == ["solveinfo80", "all_regi", VALUE]
    assert flat_values(frame) == [1.0, 0.0, 0.0, 2.0, 0.0, 7.0]
    assert repy_modelstat_text(frame) == "207"


def test_three_dimensions_and_unsorted_years_keep_set_order(tmp_path: Path) -> None:
    # R: p_years -> "3 1 2" (years not sorted); p_3d -> 3x3x2 with values at positions 1 and 15
    path = write_gdx(
        tmp_path / "shapes.gdx",
        sets={
            "all_regi": ["LAM", "OAS", "EUR"],
            "solveinfo80": ["solvestat", "modelstat"],
            "t": ["y2005", "y1995", "y2000"],
        },
        parameters={
            "p_years": (["t"], [("y1995", 1.0), ("y2000", 2.0), ("y2005", 3.0)]),
            "p_3d": (
                ["all_regi", "solveinfo80", "t"],
                [("EUR", "modelstat", "y1995", 4.0), ("LAM", "solvestat", "y2005", 1.0)],
            ),
        },
    )
    years = read_param(path, "p_years")
    assert years is not None
    assert years["t"].tolist() == ["y2005", "y1995", "y2000"]
    assert flat_values(years) == [3.0, 1.0, 2.0]
    cube = read_param(path, "p_3d")
    assert cube is not None
    values = flat_values(cube)
    assert len(values) == 18
    assert values[0] == 1.0 and values[14] == 4.0
    assert sum(values) == 5.0


def test_labels_that_are_not_region_codes_have_no_spatial_dimension(tmp_path: Path) -> None:
    # R: p_big -> 1x1x6 (GLO), values "1 0 0 0 2 3"; x[, , "modelstat"] -> "023"
    path = write_gdx(
        tmp_path / "big.gdx",
        sets={"big": ["LAM", "EU27", "World"], "solveinfo80": ["solvestat", "modelstat"]},
        parameters={
            "p_big": (
                ["big", "solveinfo80"],
                [("EU27", "modelstat", 2.0), ("World", "modelstat", 3.0), ("LAM", "solvestat", 1.0)],
            )
        },
    )
    frame = read_param(path, "p_big")
    assert frame is not None
    assert flat_values(frame) == [1.0, 0.0, 0.0, 0.0, 2.0, 3.0]
    assert repy_modelstat_text(frame) == "023"


def test_duplicated_domain_uses_the_first_as_spatial(tmp_path: Path) -> None:
    # R: p_dup(all_regi, all_regi) -> 3x1x3, values "0 0 0 0 0 0 1 0 0" (LAM.EUR = 1)
    path = write_gdx(
        tmp_path / "dup.gdx",
        sets={"all_regi": ["LAM", "OAS", "EUR"]},
        parameters={"p_dup": (["all_regi", "all_regi"], [("LAM", "EUR", 1.0)])},
    )
    frame = read_param(path, "p_dup")
    assert frame is not None
    assert frame.columns.tolist() == ["all_regi_0", "all_regi_1", VALUE]
    assert flat_values(frame) == [0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0]


def test_renamed_domain_sets_are_discovered_from_the_file(tmp_path: Path) -> None:
    path = write_gdx(
        tmp_path / "renamed.gdx",
        sets={"regi": ["EUR", "CHA"], "info": ["modelstat", "solvestat"]},
        parameters={"p80_repy": (["regi", "info"], [("CHA", "modelstat", 4.0), ("EUR", "solvestat", 1.0)])},
    )
    frame = read_param(path, "p80_repy")
    assert frame is not None
    assert frame.columns.tolist() == ["regi", "info", VALUE]
    assert repy_modelstat_text(frame) == "04"


def test_alias_domain_is_restored_over_the_parent_set(tmp_path: Path) -> None:
    # R: p_alias(regi2) -> values "0 0 5" over LAM, OAS, EUR
    path = write_gdx(
        tmp_path / "alias.gdx",
        sets={"all_regi": ["LAM", "OAS", "EUR"]},
        aliases={"regi2": "all_regi"},
        parameters={"p_alias": (["regi2"], [("EUR", 5.0)])},
    )
    frame = read_param(path, "p_alias")
    assert frame is not None
    assert frame.columns.tolist() == ["regi2", VALUE]
    assert frame["regi2"].tolist() == ["LAM", "OAS", "EUR"]
    assert flat_values(frame) == [0.0, 0.0, 5.0]


def test_universe_domain_keeps_the_stored_records(tmp_path: Path) -> None:
    # R warns "Cannot restore zeros for p_univ as set dependency is not defined!" and keeps the records:
    # values "2 5" (LAM, EUR). R additionally pads unobserved label combinations with NA (tidy2magpie);
    # none of the status symbols is declared over the universe, so that padding is not reproduced.
    path = write_gdx(
        tmp_path / "universe.gdx",
        sets={"all_regi": ["LAM", "OAS", "EUR"]},
        parameters={"p_univ": (["*", "*"], [("EUR", "modelstat", 5.0), ("LAM", "modelstat", 2.0)])},
    )
    frame = read_param(path, "p_univ")
    assert frame is not None
    assert frame.columns.tolist() == ["uni_0", "uni_1", VALUE]
    assert frame["uni_0"].tolist() == ["LAM", "EUR"]
    assert flat_values(frame) == [2.0, 5.0]


def test_magpie_order_roles() -> None:
    regions, years, info = ["LAM", "EUR"], ["y2005", "y1995"], ["a", "b"]
    assert magpie_order([regions, info]) == (1, 0)
    assert magpie_order([info, regions]) == (0, 1)
    assert magpie_order([regions, info, years]) == (1, 2, 0)
    assert magpie_order([info, years, regions]) == (0, 1, 2)
    assert magpie_order([["EU27", "World"], info]) == (1, 0)
    # a regiospatial dimension wins; the regional one then joins the rest in original order
    assert magpie_order([info, ["AFR_1", "EUR_2"], regions]) == (2, 0, 1)
    assert magpie_order([regions, ["glob"]]) == (1, 0)
    assert magpie_order([["2005", "1995"], regions]) == (0, 1)
    assert magpie_order([]) == ()


def test_dense_frame_pads_the_product_in_r_order() -> None:
    records = pd.DataFrame([["b", "y2000", 1.5]], columns=["d", "t", VALUE])
    frame = dense_frame(["d", "t"], [["a", "b"], ["y2000", "y1995"]], records)
    assert frame.columns.tolist() == ["d", "t", VALUE]
    # no spatial dimension: the temporal one is the magpie's second and varies faster than the rest
    assert frame.to_dict("records") == [
        {"d": "a", "t": "y2000", VALUE: 0.0},
        {"d": "a", "t": "y1995", VALUE: 0.0},
        {"d": "b", "t": "y2000", VALUE: 1.5},
        {"d": "b", "t": "y1995", VALUE: 0.0},
    ]
    assert dense_frame([], [], None).to_dict("records") == [{VALUE: 0.0}]


# ---------------------------------------------------------------------------------------------
# Special values and R text
# ---------------------------------------------------------------------------------------------


def test_special_values_map_as_in_r(tmp_path: Path) -> None:
    # R: s_eps -> -0 (printed "0"), s_inf Inf, s_ninf -Inf, s_undef NaN, s_na NA; p_special "0 NA NaN"
    path = write_gdx(
        tmp_path / "special.gdx",
        sets={"all_regi": ["LAM", "OAS", "EUR"]},
        scalars={
            "s_eps": gt.SpecialValues.EPS,
            "s_inf": gt.SpecialValues.POSINF,
            "s_ninf": gt.SpecialValues.NEGINF,
            "s_undef": gt.SpecialValues.UNDEF,
            "s_na": gt.SpecialValues.NA,
        },
        parameters={
            "p_special": (
                ["all_regi"],
                [
                    ("LAM", gt.SpecialValues.EPS),
                    ("OAS", gt.SpecialValues.NA),
                    ("EUR", gt.SpecialValues.UNDEF),
                ],
            )
        },
        eps_to_zero=False,
    )
    eps = read_scalar(path, "s_eps")
    assert eps == 0.0 and eps is not None and math.copysign(1.0, eps) == -1.0
    assert read_scalar(path, "s_inf") == math.inf
    assert read_scalar(path, "s_ninf") == -math.inf
    undef = read_scalar(path, "s_undef")
    assert undef is not None and math.isnan(undef) and not is_na(undef)
    na = read_scalar(path, "s_na")
    assert na is not None and math.isnan(na) and is_na(na)
    frame = read_param(path, "p_special")
    assert frame is not None
    assert [number_text(v) for v in flat_values(frame)] == ["0", "NA", "NaN"]
    assert modelstat_text(flat_values(frame)) == ".NANaN"


def test_number_text_is_paste0_of_one_element() -> None:
    assert number_text(2.0) == "2"
    assert number_text(13.0) == "13"
    assert number_text(2.5) == "2.5"
    assert number_text(-0.0) == "0"
    assert number_text(100000.0) == "1e+05"
    assert number_text(math.inf) == "Inf"
    assert number_text(-math.inf) == "-Inf"
    assert number_text(float("nan")) == "NaN"
    assert number_text(float(gt.SpecialValues.NA)) == "NA"


def test_modelstat_text_replaces_zeros_with_dots() -> None:
    # R: gsub("0", ".", paste0(c(2, 0, 2), collapse = "")) -> "2.2"; paste0(20) -> "20" -> "2."
    assert modelstat_text([2.0]) == "2"
    assert modelstat_text([2.0, 0.0, 2.0]) == "2.2"
    assert modelstat_text([20.0]) == "2."
    assert modelstat_text([2.0] * 18) == "2" * 18
    assert modelstat_text([]) == ""


def test_explain_modelstat_matches_the_r_table() -> None:
    assert explain_modelstat("1") == "1: Optimal"
    assert explain_modelstat("2") == "2: Locally Optimal"
    assert explain_modelstat("3") == "3: Unbounded"
    assert explain_modelstat("4") == "4: Infeasible"
    assert explain_modelstat("5") == "5: Locally Infes"
    assert explain_modelstat("6") == "6: Intermed Infes"
    assert explain_modelstat("7") == "7: Intermed Nonoptimal"
    assert explain_modelstat("13") == "13: Error No Solution"
    assert explain_modelstat("8") == "8"
    assert explain_modelstat("222222") == "222222"
    assert explain_modelstat("NA") == "NA"
    assert explain_modelstat("2.") == "2."


def test_select_element_needs_exactly_one_dimension_with_the_label(remind_gdx: Path) -> None:
    repy = read_param(remind_gdx, "p80_repy")
    assert repy is not None
    assert select_element(repy, "solvestat") == [1.0] * 12
    with pytest.raises(GdxReadError, match="0 dimension"):
        select_element(repy, "nope")
    both = pd.DataFrame([["a", "a", 1.0]], columns=["x", "y", VALUE])
    with pytest.raises(GdxReadError, match="2 dimension"):
        select_element(both, "a")


# ---------------------------------------------------------------------------------------------
# first_found
# ---------------------------------------------------------------------------------------------


def test_first_found_tries_the_names_in_order(tmp_path: Path, remind_gdx: Path) -> None:
    magpie = write_gdx(
        tmp_path / "magpie.gdx",
        sets={"t": YEARS},
        parameters={"p80_modelstat": (["t"], [(year, 2.0 if year != "y2010" else 0.0) for year in YEARS])},
    )
    remind = read_first_found(remind_gdx, gdx.MODELSTAT_SYMBOLS)
    assert remind is not None and flat_values(remind) == [2.0]
    found = read_first_found(magpie, gdx.MODELSTAT_SYMBOLS)
    assert found is not None
    assert found.columns.tolist() == ["t", VALUE]
    assert modelstat_text(flat_values(found)) == "222.222222"
    assert read_first_found(magpie, ["o_modelstat"]) is None
    assert read_first_found(magpie, []) is None


# ---------------------------------------------------------------------------------------------
# Symbol names: the GDX library folds case, gdx2's typed path does not
# ---------------------------------------------------------------------------------------------


def test_plain_names_resolve_case_insensitively_like_gdxfindsymbol(tmp_path: Path) -> None:
    """R reads ``o_iterationNumber`` from a file storing ``O_ITERATIONNUMBER`` (gdx2 0.6.2, 2026-10-01).

    ``as.numeric(readGDX(f, "o_iterationNumber", format = "simplest", react = "silent"))`` is 12,
    ``c(readGDX(f, c("o_modelstat", "p80_modelstat"), format = "first_found", react = "silent"))`` is 2
    and ``names(gamstransfer::readGDX(f, "o_iterationNumber"))`` is ``"O_ITERATIONNUMBER"``.
    """
    upper = write_gdx(
        tmp_path / "upper.gdx", scalars={"O_ITERATIONNUMBER": 12, "S80_BOOL": 1, "P80_MODELSTAT": 2}
    )
    file = GdxFile(upper)
    assert read_scalar(file, "o_iterationNumber") == 12.0
    found = read_first_found(file, gdx.MODELSTAT_SYMBOLS)
    assert found is not None and flat_values(found) == [2.0]
    info = file.info("o_iterationnumber")
    assert info is not None and info.name == "O_ITERATIONNUMBER"
    assert read_param(file, "o_iterationNumber") is read_param(file, "O_ITERATIONNUMBER")  # one cache slot
    assert file.info("o_iteration") is None and read_scalar(file, "o_iteration") is None


def test_typed_read_of_line_281_is_exact_case_and_parameters_only(tmp_path: Path) -> None:
    """R: ``as.numeric(readGDX(f, "s80_bool", type = "Parameter", format = "simplest"))``.

    ``numeric(0)`` plus the warning ``No element of s80_bool found in GDX! NULL returned`` against the
    stored ``S80_BOOL`` and against a *set* named ``s80_bool``; ``1`` for the exact ``S80_BOOL``.
    """
    upper = write_gdx(tmp_path / "upper.gdx", scalars={"S80_BOOL": 1})
    assert read_typed_scalar(upper, "s80_bool") is None
    assert read_typed_scalar(upper, "S80_BOOL") == 1.0
    assert read_scalar(upper, "s80_bool") == 1.0  # the plain shape folds case
    m = gt.Container(system_directory=gamspy_base.directory)
    gt.Set(m, "s80_bool", records=["a"])
    gt.Parameter(m, "o_iterationNumber", records=2)
    set_named = tmp_path / "set-named.gdx"
    m.write(str(set_named))
    assert read_typed_scalar(set_named, "s80_bool") is None
    assert read_typed_scalar(set_named, "o_iterationNumber") == 2.0
    assert read_typed_scalar(set_named, "absent") is None


def test_select_latest_gdx_reads_an_uppercase_iteration_symbol(tmp_path: Path) -> None:
    older, newer = 1_790_700_000.0, 1_790_762_400.0
    high = write_gdx(tmp_path / "fulldata.gdx", scalars={"O_ITERATIONNUMBER": 7})
    low = write_gdx(tmp_path / "non_optimal.gdx", scalars={"o_iterationNumber": 2})
    os.utime(high, (older, older))
    os.utime(low, (newer, newer))
    candidates = [GdxCandidate(GdxFile(high), older), GdxCandidate(GdxFile(low), newer)]
    assert select_latest_gdx(candidates) is candidates[0].file  # by iteration (7 > 2), not by the newer mtime


# ---------------------------------------------------------------------------------------------
# Errors: raise, never abort
# ---------------------------------------------------------------------------------------------


@pytest.fixture
def damaged(tmp_path: Path, remind_gdx: Path) -> dict[str, Path]:
    data = remind_gdx.read_bytes()
    files = {
        "text": tmp_path / "text.gdx",
        "empty": tmp_path / "empty.gdx",
        "truncated": tmp_path / "truncated.gdx",
        "missing": tmp_path / "missing.gdx",
    }
    files["text"].write_text("This is not a GDX file.\n" * 4)
    files["empty"].write_bytes(b"")
    files["truncated"].write_bytes(data[: len(data) // 2])
    return files


def test_damaged_files_raise_instead_of_crashing(damaged: Mapping[str, Path]) -> None:
    with pytest.raises(GdxReadError, match="not recognized as a GDX file"):
        read_scalar(damaged["text"], "o_iterationNumber")
    with pytest.raises(GdxReadError, match="not recognized as a GDX file"):
        read_param(damaged["empty"], "o_iterationNumber")
    with pytest.raises(GdxReadError, match="truncated.gdx"):
        read_first_found(damaged["truncated"], gdx.MODELSTAT_SYMBOLS)
    with pytest.raises(GdxReadError, match="No such file"):
        GdxFile(damaged["missing"]).info("o_iterationNumber")


def test_symbols_of_another_kind_raise(tmp_path: Path) -> None:
    m = gt.Container(system_directory=gamspy_base.directory)
    regi = gt.Set(m, "all_regi", records=["LAM", "EUR"])
    gt.Set(m, "s80_bool", records=["a"])
    gt.Alias(m, "o_modelstat", regi)
    gt.Variable(
        m,
        "p80_repy",
        "free",
        [regi],
        records=pd.DataFrame([["LAM", 1.0, 0.5]], columns=["all_regi", "level", "marginal"]),
    )
    gt.Parameter(m, "o_iterationNumber", records=2)
    path = tmp_path / "kinds.gdx"
    m.write(str(path))
    file = GdxFile(path)
    assert read_scalar(file, "o_iterationNumber") == 2.0
    with pytest.raises(GdxReadError, match=r"s80_bool is not a parameter \(kind: set\)"):
        read_scalar(file, "s80_bool")
    with pytest.raises(GdxReadError, match=r"o_modelstat is not a parameter \(kind: alias\)"):
        read_first_found(file, gdx.MODELSTAT_SYMBOLS)
    with pytest.raises(GdxReadError, match=r"p80_repy is not a parameter \(kind: variable\)"):
        read_param(file, "p80_repy")


def test_parameter_frames_are_cached_per_file(remind_gdx: Path, tmp_path: Path) -> None:
    copy = tmp_path / "copy.gdx"
    copy.write_bytes(remind_gdx.read_bytes())
    file = GdxFile(copy)
    first = read_param(file, "p80_repy")
    assert first is not None
    assert read_param(file, "p80_repy") is first
    assert read_param(file, "nope") is None
    copy.unlink()
    assert read_param(file, "p80_repy") is first  # cached symbols survive the file
    assert read_param(file, "nope") is None
    with pytest.raises(GdxReadError, match="reading o_iterationNumber failed"):
        read_scalar(file, "o_iterationNumber")  # a symbol not read yet needs the file again
    with pytest.raises(GdxReadError, match="No such file"):
        GdxFile(copy).info("p80_repy")


# ---------------------------------------------------------------------------------------------
# GDX selection
# ---------------------------------------------------------------------------------------------


def test_which_max_is_first_maximum_ignoring_na() -> None:
    assert which_max([5.0, 5.0]) == 0
    assert which_max([2.0, 7.0, 7.0]) == 1
    assert which_max([float("nan"), 3.0]) == 1
    assert which_max([float("nan"), float("nan")]) is None
    assert which_max([]) is None


def iteration_file(directory: Path, name: str, iteration: float | None, mtime: float) -> GdxCandidate:
    scalars = {"o_modelstat": 2.0} | ({"o_iterationNumber": iteration} if iteration is not None else {})
    path = write_gdx(directory / name, scalars=scalars)
    os.utime(path, (mtime, mtime))
    return GdxCandidate(GdxFile(path), mtime)


def test_select_latest_gdx_reproduces_lines_53_to_64(tmp_path: Path) -> None:
    older, newer = 1_790_700_000.0, 1_790_762_400.0
    tie_a = iteration_file(tmp_path, "tie-a.gdx", 5, older)
    tie_b = iteration_file(tmp_path, "tie-b.gdx", 5, newer)
    assert select_latest_gdx([tie_a, tie_b]) is tie_a.file  # which.max: first maximum (synthetic gdx-tie)
    assert select_latest_gdx([tie_b, tie_a]) is tie_b.file
    low = iteration_file(tmp_path, "low.gdx", 2, newer)
    high = iteration_file(tmp_path, "high.gdx", 7, older)
    assert select_latest_gdx([low, high]) is high.file
    noiter = iteration_file(tmp_path, "noiter.gdx", None, newer)
    assert select_latest_gdx([high, noiter]) is noiter.file  # length mismatch -> newest mtime (gdx-noiter)
    assert select_latest_gdx([noiter, high]) is noiter.file
    same_time = iteration_file(tmp_path, "same-time.gdx", None, older)
    assert select_latest_gdx([high, same_time]) is high.file  # mtime tie -> first
    na = iteration_file(tmp_path, "na.gdx", gt.SpecialValues.NA, newer)
    assert select_latest_gdx([na, high]) is high.file  # NA skipped by which.max
    assert select_latest_gdx([na, na]) is None  # which.max(c(NA, NA)) -> integer(0): no GDX at all
    assert select_latest_gdx([]) is None


def test_single_candidate_is_taken_without_reading(tmp_path: Path) -> None:
    broken = tmp_path / "fulldata.gdx"
    broken.write_text("not a gdx")
    candidate = GdxCandidate(GdxFile(broken), 0.0)
    assert select_latest_gdx([candidate]) is candidate.file


def test_damaged_candidate_among_several_raises(tmp_path: Path) -> None:
    good = iteration_file(tmp_path, "non_optimal.gdx", 2, 1.0)
    broken = tmp_path / "fulldata.gdx"
    broken.write_text("This is not a GDX file.")
    with pytest.raises(GdxReadError):
        select_latest_gdx([GdxCandidate(GdxFile(broken), 2.0), good])
