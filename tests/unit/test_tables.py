"""``tables`` builds the R ``data.frame`` of ``getRunStatus()`` and counts like ``read.csv2``.

The frame semantics pinned here follow ``R/getRunStatus.R``: columns in order of first assignment,
row names from ``basename(dir)``, a column assigned for one run and not another is ``NA`` in the
other row, a repeated row name updates the existing row in place (register BUG-022, decision D-13),
the string ``"NA"`` stays a string while a real ``NA`` is missing. The CSV counting follows
``read.csv2`` as verified for register BUG-007 (withdrawn): quote ``"`` only, no comment character,
``NA`` is the missing value, blank lines are skipped. The golden-marked tests at the end check the
three sanity counts of every fixture run that has the files against ``migration/goldens``.
"""

from __future__ import annotations

import json
from pathlib import Path

import pytest

from modelstats import tables
from modelstats.formatting import CellValue, Row
from modelstats.tables import column_order, read_error_csv, to_dataframe

# --- column order --------------------------------------------------------------------------------


def test_status_columns_are_the_r_assignment_order() -> None:
    assert tables.COLUMN_ORDER == (
        "jobInSLURM",
        "RunType",
        "modelstat",
        "runInAppResults",
        "Mif",
        "Iter",
        "RunStatus",
        "Warnings",
        "Conv",
        "Runtime",
        "summationErrors",
        "rangeErrors",
        "fixErrors",
        "missingProjVars",
        "projSummationErrors",
        "projSummationErrorsRegional",
    )


def test_column_order_keeps_r_order_and_appends_unknown_names_as_seen() -> None:
    assert column_order(["RunStatus", "jobInSLURM", "Iter"]) == ["jobInSLURM", "Iter", "RunStatus"]
    assert column_order(["extra", "Conv", "other", "Conv"]) == ["Conv", "extra", "other"]
    assert column_order([]) == []


# --- to_dataframe --------------------------------------------------------------------------------

BRIEF = {
    "jobInSLURM": "NA",
    "RunType": "nash",
    "modelstat": "2: Locally Optimal",
    "Mif": "yes",
    "Iter": "26/100",
}
DETAILED = {
    **BRIEF,
    "RunStatus": "Normal completion",
    "Warnings": "27",
    "Conv": "converged",
    "Runtime": 77679,
    "summationErrors": 1,
    "rangeErrors": 0,
    "fixErrors": 2443,
    "missingProjVars": None,
    "projSummationErrors": 35,
    "projSummationErrorsRegional": 47,
}


def test_frame_has_run_names_as_index_and_r_column_order() -> None:
    frame = to_dataframe([Row("b", DETAILED), Row("a", BRIEF | {"RunStatus": "NA"})])
    assert frame.index.name == "run"
    assert list(frame.index) == ["b", "a"]
    assert list(frame.columns) == list(tables.COLUMN_ORDER[:3]) + list(tables.COLUMN_ORDER[4:])


def test_frame_dtypes_follow_the_r_storage_modes() -> None:
    frame = to_dataframe([Row("r", DETAILED)])
    assert str(frame["RunType"].dtype) == "str"
    assert str(frame["Runtime"].dtype) == "Float64"
    assert str(frame["summationErrors"].dtype) == "Int64"
    assert frame.loc["r", "Runtime"] == 77679
    assert frame.loc["r", "fixErrors"] == 2443


def test_the_string_na_stays_a_string_and_a_real_na_is_missing() -> None:
    frame = to_dataframe([Row("r", DETAILED)])
    assert frame.loc["r", "jobInSLURM"] == "NA"
    assert not frame["jobInSLURM"].isna().any()
    assert frame["missingProjVars"].isna().all()


def test_a_column_one_run_lacks_is_na_in_that_row() -> None:
    magpie = {"jobInSLURM": "no", "RunType": "nlp_par", "modelstat": "222", "Mif": "yes", "Iter": "y2150"}
    frame = to_dataframe([Row("mag", magpie), Row("rem", DETAILED)])
    assert list(frame.columns) == list(tables.COLUMN_ORDER[:3]) + list(tables.COLUMN_ORDER[4:])
    assert frame["summationErrors"].isna().tolist() == [True, False]
    assert frame["summationErrors"].tolist()[1] == 1
    assert frame["RunStatus"].isna().tolist() == [True, False]


def test_a_repeated_run_name_updates_the_earlier_row_in_place() -> None:
    first = Row("same", {"jobInSLURM": "no", "RunType": "nash", "Mif": "yes"})
    other = Row("other", {"jobInSLURM": "no", "RunType": "nash", "Mif": "no"})
    second = Row("same", {"jobInSLURM": "NA", "RunType": "debug EUR"})
    frame = to_dataframe([first, other, second])
    assert list(frame.index) == ["same", "other"]
    assert frame.loc["same", "RunType"] == "debug EUR"
    assert frame.loc["same", "jobInSLURM"] == "NA"
    assert frame.loc["same", "Mif"] == "yes"


def test_no_rows_give_an_empty_frame() -> None:
    frame = to_dataframe([])
    assert frame.empty
    assert frame.index.name == "run"
    assert list(frame.columns) == []


def test_unknown_columns_keep_their_objects() -> None:
    frame = to_dataframe([Row("r", {"RunType": "nash", "note": "free text", "flag": True})])
    assert list(frame.columns) == ["RunType", "note", "flag"]
    assert frame.loc["r", "flag"] is True


def test_any_object_with_run_and_cells_is_accepted() -> None:
    class Record:
        run = "rec"

        @property
        def cells(self) -> dict[str, CellValue]:
            return {"RunType": "nash"}

    assert list(to_dataframe([Record()]).index) == ["rec"]


# --- read_error_csv ------------------------------------------------------------------------------


def write(path: Path, text: str) -> Path:
    path.write_text(text, encoding="utf-8")
    return path


SUMMATION = """model,scenario,region,period,variable,unit,value,checkSum,diff,reldiff,details
REMIND,s,NEN,2110,Emi|CO2|Cumulated,Mt CO2,46.3,46.3,-0.01,-0.02,Emi|CO2|Cumulated|CDR|BECCS (-2420.9) + x (1)
REMIND,s,GLO,2110,Emi|CO2|Cumulated,Mt CO2,46.3,46.3,-0.01,-0.02,Emi|CO2|Cumulated|CDR|BECCS (-2420.9) + x (1)
REMIND,s,GLO,2005,FE|Industry|Steel|++|Primary,EJ/yr,1,1,0,0,FE|Industry|Steel|+|Gases (0.38)
"""


def test_summation_errors_count_distinct_variables(tmp_path: Path) -> None:
    path = write(tmp_path / "s.csv", SUMMATION)
    assert tables.summation_error_count(path) == 2
    assert tables.fix_error_count(path) == 2
    table = read_error_csv(path, sep=",")
    assert table.nrow == 3
    assert table.header[4] == "variable"
    assert table.distinct == {"variable": 2}


def test_range_errors_count_rows_of_a_text_file_with_no_separator(tmp_path: Path) -> None:
    text = "variables exceeding upper limit 100\nNEN   2020   SSP2   REMIND   Carbon (%)   100\n"
    path = write(tmp_path / "r.txt", text)
    assert tables.range_error_count(path) == 1
    assert read_error_csv(path).header == ("variables exceeding upper limit 100",)
    assert tables.range_error_count(write(tmp_path / "h.txt", "header only\n")) == 0


def test_blank_lines_are_skipped_but_whitespace_lines_are_rows(tmp_path: Path) -> None:
    path = write(tmp_path / "r.txt", "\nheader\n\nrow1\n  \nrow2\n\n")
    assert read_error_csv(path).nrow == 3
    assert read_error_csv(path).header == ("header",)


def test_quoted_fields_with_separators_newlines_and_doubled_quotes(tmp_path: Path) -> None:
    text = 'variable,details\n"a,b","x"\n"line\nbreak","y"\n"say ""hi""","z"\n"a,b","w"\n'
    table = read_error_csv(write(tmp_path / "q.csv", text), sep=",")
    assert table.nrow == 4
    assert table.distinct == {"variable": 3}


def test_na_is_one_distinct_value_and_padded_fields_are_blank(tmp_path: Path) -> None:
    text = "variable,value\nNA,1\nNA,2\n,3\n\n"
    table = read_error_csv(write(tmp_path / "n.csv", text), sep=",")
    assert table.nrow == 3
    assert table.distinct == {"variable": 2}
    short = "variable,value\nx,1\ny\n"
    assert read_error_csv(write(tmp_path / "p.csv", short), sep=",", distinct=("value",)).distinct == {
        "value": 2
    }


def test_a_missing_variable_column_counts_zero(tmp_path: Path) -> None:
    path = write(tmp_path / "m.csv", "model,region\nREMIND,GLO\n")
    assert read_error_csv(path, sep=",").distinct == {}
    assert tables.summation_error_count(path) == 0


def test_dollar_partial_matching_of_the_column_name(tmp_path: Path) -> None:
    assert tables.dollar_column(["variable", "variables"], "variable") == 0
    assert tables.dollar_column(["x", "variables"], "variable") == 1
    assert tables.dollar_column(["variableA", "variableB"], "variable") is None
    assert tables.dollar_column(["x"], "variable") is None
    path = write(tmp_path / "p.csv", "variableName,v\na,1\nb,2\na,3\n")
    assert tables.summation_error_count(path) == 2


def test_crlf_files_count_the_same(tmp_path: Path) -> None:
    path = tmp_path / "crlf.csv"
    path.write_bytes(b"variable,v\r\na,1\r\nb,2\r\n")
    assert tables.summation_error_count(path) == 2
    assert read_error_csv(path, sep=",").nrow == 2


def test_an_input_without_lines_is_the_r_error(tmp_path: Path) -> None:
    with pytest.raises(tables.EmptyCsvError, match="no lines available in input"):
        read_error_csv(write(tmp_path / "e.csv", ""))
    with pytest.raises(tables.EmptyCsvError):
        read_error_csv(write(tmp_path / "b.csv", "\n\n"))


def test_undecodable_bytes_stay_distinct(tmp_path: Path) -> None:
    path = tmp_path / "bytes.csv"
    path.write_bytes(b"variable\ncaf\xe9\ncaf\xc3\xa9\n")
    assert tables.summation_error_count(path) == 2


# --- goldens: the sanity counts of the fixture runs ----------------------------------------------

AMT = "remind/modeltests/remind/output"
COUPLED = "remind/runs/REMIND-MAgPIE-2026-07-07/output"
FIXTURE_RUNS = {
    "remind__modeltests__remind__output__SSP2-EU21-PkBudg650-AMT_2026-09-28_13.54.59": (
        f"{AMT}/SSP2-EU21-PkBudg650-AMT_2026-09-28_13.54.59",
        "SSP2-EU21-PkBudg650-AMT",
    ),
    "remind__modeltests__remind__output__testOneRegi": (f"{AMT}/testOneRegi", "testOneRegi"),
    "remind__modeltests__remind__output__archive__SSP2-EU21-NPi-AMT_2025-07-25_22.18.56": (
        f"{AMT}/archive/SSP2-EU21-NPi-AMT_2025-07-25_22.18.56",
        "SSP2-EU21-NPi-AMT",
    ),
    "remind__runs__REMIND-MAgPIE-2026-07-07__output__C_SSP2-NDC-LTS_2026-07-08_05.05.25": (
        f"{COUPLED}/C_SSP2-NDC-LTS_2026-07-08_05.05.25",
        "C_SSP2-NDC-LTS",
    ),
}


def sanity_counts_like_r(run: Path, title: str) -> tuple[int, int, int]:
    """``R/getRunStatus.R:325-348`` on top of the counters (a missing file counts 0)."""
    summation = run / f"REMIND_generic_{title}_summation_errors.csv"
    ranges = run / f"REMIND_generic_{title}_range_errors.txt"
    fixes = run / "log_fixOnRef.csv"
    return (
        tables.summation_error_count(summation) if summation.exists() else 0,
        tables.range_error_count(ranges) if ranges.exists() else 0,
        tables.fix_error_count(fixes) if fixes.exists() else 0,
    )


@pytest.mark.golden
@pytest.mark.parametrize("case_id", sorted(FIXTURE_RUNS))
def test_sanity_counts_equal_the_status_golden(case_id: str, goldens_dir: Path, fixtures_dir: Path) -> None:
    relative, title = FIXTURE_RUNS[case_id]
    golden = goldens_dir / "status" / "oncluster" / "detailed" / f"{case_id}.json"
    row = json.loads(golden.read_text(encoding="utf-8"))["rows"][0]
    expected = (row["summationErrors"], row["rangeErrors"], row["fixErrors"])
    assert sanity_counts_like_r(fixtures_dir / "p" / "projects" / relative, title) == expected


@pytest.mark.golden
def test_every_fixture_run_with_error_files_is_listed_here(fixtures_dir: Path) -> None:
    runs = {
        path.parent.relative_to(fixtures_dir / "p" / "projects").as_posix()
        for pattern in ("*_summation_errors.csv", "*_range_errors.txt", "log_fixOnRef.csv")
        for path in fixtures_dir.glob(f"p/projects/**/{pattern}")
    }
    assert runs == {relative for relative, _ in FIXTURE_RUNS.values()}
