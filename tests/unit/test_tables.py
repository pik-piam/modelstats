"""tables: read.csv2 counting semantics and the pandas view.

Every expected count marked "R" was obtained with R 4.6.1 (``read.csv2(text = ..., sep = ",")``
and ``read.csv2(file)``) on 2026-10-01; the fixture files were checked separately
(``migration/_scratch/p1-tables-textscan-env``): 7 files, all counts equal.
"""

from __future__ import annotations

from pathlib import Path

import pandas as pd
import pytest

from modelstats.errors import RParityError
from modelstats.tables import count_rows, count_unique_variable, to_dataframe


def uniq(tmp_path: Path, data: bytes | str) -> int:
    path = tmp_path / "t.csv"
    path.write_bytes(data.encode() if isinstance(data, str) else data)
    return count_unique_variable(path)


def nrow(tmp_path: Path, data: bytes | str) -> int:
    path = tmp_path / "t.txt"
    path.write_bytes(data.encode() if isinstance(data, str) else data)
    return count_rows(path)


UNIQUE_CASES: list[tuple[str, str | bytes, int]] = [
    ("basic", "a,variable,b\n1,x,2\n3,y,4\n5,x,6\n", 2),
    ("no trailing newline", "a,variable,b\n1,x,2\n3,y,4", 2),
    ("blank lines skipped", "a,variable,b\n1,x,2\n\n3,y,4\n\n", 2),
    ("whitespace-only line is a row", "a,variable,b\n1,x,2\n   \n3,y,4\n", 3),
    ("tab-only line is a row", "a,variable,b\n1,x,2\n\t\n3,y,4\n", 3),
    ("NA and empty are distinct", "a,variable,b\n1,NA,2\n3,,4\n5,x,6\n", 3),
    ("quoted NA is NA", 'a,variable,b\n1,NA,2\n3,"NA",4\n5,x,6\n', 2),
    ("dec=, keeps 1.0 as text", "a,variable,b\n1,1,2\n3,1.0,4\n5,2,6\n", 3),
    ("numeric column: blank and NA collapse", "a,variable,b\n1,1,2\n3,,4\n5,NA,6\n", 2),
    ("logical column", "a,variable,b\n1,T,2\n3,TRUE,4\n5,F,6\n", 2),
    ("decimal comma", 'a,variable,b\n1,"1,5",2\n3,"1.5",4\n', 2),
    ("missing column", "a,b\n1,2\n3,4\n", 0),
    ("partial match", "a,variablex,b\n1,x,2\n3,y,4\n", 2),
    ("ambiguous partial match", "a,variablex,variabley\n1,x,2\n3,y,4\n", 0),
    ("exact beats partial", "a,variable,variablex\n1,x,2\n3,y,4\n", 2),
    ("duplicate column names", "a,variable,variable\n1,x,2\n3,y,4\n", 2),
    ("quoted newline", 'a,variable,b\n1,"x\ny",2\n3,y,4\n', 2),
    ("quoted separator", 'a,variable,b\n1,"x,y",2\n3,"x,y",4\n', 1),
    ("quote in the middle of a field", 'a,variable,b\n1,x"y,z"w,2\n3,q,4\n', 2),
    ("every row one field longer: row names", "a,variable,b\n1,x,2,9\n3,y,4,9\n", 2),
    ("header one short: first field is the row name", "a,variable\n1,x,2\n3,y,4\n", 2),
    ("short rows padded with empty fields", "a,variable,b\n1,x\n3\n5,y,6\n", 3),
    ("short row and explicit empty field are the same value", "a,variable,b\n1,x,2\n2,,3\n3\n", 2),
    ("short row pads with empty, not NA", "a,variable,b\n1,x,2\n2,NA,3\n3\n", 3),
    ("surplus fields spill into a new row", "a,variable,b\n1,x,2\n1,x,2\n1,x,2\n1,x,2\n1,x,2\n1,x,2,extra\n", 2),
    ("header only", "a,variable,b\n", 0),
    ("hash is not a comment", "a,variable,b\n#1,x,2\n3,y,4\n", 2),
    ("trailing space kept", "a,variable,b\n1,x ,2\n3,x,4\n", 2),
    ("crlf", "a,variable,b\r\n1,x,2\r\n3,y,4\r\n", 2),
    ("single quotes are literal", "a,variable,b\n1,'x,y',2\n3,x,4\n", 2),
    ("quoted header", '"a","variable","b"\n1,x,2\n3,y,4\n', 2),
    ("header name with a space (make.names)", "a,variable ,b\n1,x,2\n3,y,4\n", 2),
    ("width from the first five lines, spill later", "a,variable,b\n1,x\n1,y\n1,z\n1,w\n1,v\n1,u,2,3,4\n", 7),
    (
        "unclosed quote beyond line five runs to EOF",
        'a,variable,b\n1,x,2\n1,x,2\n1,x,2\n1,x,2\n1,x,2\n1,x,2\n1,"y,2\n3,z,4\n',
        2,
    ),
    ("unclosed quote within the first five lines drops every row", 'a,variable\n1,"x\n2,3\n', 0),
    ("numeric: 1 1,0 01 are one value", 'a,variable\n1,1\n2,"1,0"\n3,01\n', 1),
    ("numeric: exponent and hex", "a,variable\n1,1e3\n2,1000\n3,0x10\n4,16\n", 2),
    ("numeric: inf and nan spellings", "a,variable\n1,Inf\n2,inf\n3,NaN\n4,nan\n5,-Inf\n", 3),
    ("numeric: NaN and NA are distinct", "a,variable\n1,NaN\n2,NA\n3,\n", 2),
    ("logical with blank", "a,variable\n1,T\n2,  \n3,F\n", 3),
    ("not logical: true", "a,variable\n1,TRUE\n2,true\n", 2),
    ("trailing blank in a number", "a,variable\n1,1 \n2,1\n", 1),
    ("NA with a space is text", "a,variable\n1, NA\n2,NA\n", 2),
    ("yes/no is text", "a,variable\n1,yes\n2,no\n", 2),
    ("utf-8 names", "a,variable\n1,Emi|CO2|+|Land-Use Change\n2,Emi|CO2|+|Land-Use Change\n3,ä\n", 2),
    ("invalid utf-8 byte kept", b"a,variable\n1,\xff\n2,\xff\n3,x\n", 2),
]


@pytest.mark.parametrize(("data", "expected"), [(d, e) for _, d, e in UNIQUE_CASES], ids=[c[0] for c in UNIQUE_CASES])
def test_count_unique_variable(tmp_path: Path, data: str | bytes, expected: int) -> None:
    assert uniq(tmp_path, data) == expected  # R


ROW_CASES: list[tuple[str, str, int]] = [
    ("range errors file", "variables exceeding upper limit 100\nNEN   2020   X   100\nABC 2030 Y 200\n", 2),
    ("no trailing newline", "h\nl1\nl2", 2),
    ("blank lines skipped", "h\nl1\n\nl2\n\n", 2),
    ("whitespace line is a row", "h\nl1\n   \nl2\n", 3),
    ("semicolons: header one short", "h;i\n1;2\n3;4;5\n", 2),
    ("semicolons: late spill", "h;i\n1;2\n1;2\n1;2\n1;2\n1;2\n3;4;5\n", 7),
    ("header only", "h\n", 0),
    ("quoted newline", 'h\n"a\nb"\nz\n', 2),
    ("hash is not a comment", "h\n#x\ny\n", 2),
    ("crlf", "h\r\na\r\nb\r\n", 2),
    ("only blank data lines", "h\n\n\n", 0),
    ("unclosed quote in the first five lines", "h\nit's\nx\"y\nz\n", 0),
    ("comma is not the separator", "h\na,b\nc,d\n", 2),
]


@pytest.mark.parametrize(("data", "expected"), [(d, e) for _, d, e in ROW_CASES], ids=[c[0] for c in ROW_CASES])
def test_count_rows(tmp_path: Path, data: str, expected: int) -> None:
    assert nrow(tmp_path, data) == expected  # R


@pytest.mark.parametrize(
    ("data", "message"),
    [
        ("", "no lines available in input"),
        ("\n\n", "no lines available in input"),
        ("   \n", "first five rows are empty: giving up"),
        ("a,variable\n1,x,2\n1,y,3\n", "duplicate 'row.names' are not allowed"),
        ("a,variable\n1,x,2,9\n", "more columns than column names"),
    ],
)
def test_r_errors(tmp_path: Path, data: str, message: str) -> None:
    with pytest.raises(RParityError, match=f"^{message}$"):
        uniq(tmp_path, data)  # R
    with pytest.raises(RParityError):
        nrow(tmp_path, data.replace(",", ";"))


def test_fixture_shaped_files(tmp_path: Path) -> None:
    summation = (
        "model,scenario,region,period,variable,unit,value,checkSum,diff,reldiff,details\n"
        "REMIND,S,MEA,2045,Emi|CO2|+|Land-Use Change,Mt CO2/yr,0.146,0.183,0.0366,25.0,Emi|CO2|a (-4.07) + b (4.25)\n"
        "REMIND,S,MEA,2150,Emi|CO2|+|Land-Use Change,Mt CO2/yr,-0.916,-0.953,-0.0366,4.0,Emi|CO2|a (-5.9) + b (4.95)\n"
        "REMIND,S,GLO,2005,FE|Industry|Steel|++|Primary,EJ/yr,21.95,21.97,0.0158,0.0719,FE|a (2.55) + FE|b (0)\n"
    )
    assert uniq(tmp_path, summation) == 2
    assert nrow(tmp_path, summation) == 3
    range_errors = (
        "variables exceeding upper limit 100\n"
        "NEN   2020   SSP2-EU21-NPi-AMT   REMIND   Carbon Management|Share of Stored CO2 from Captured CO2 (%)   100\n"
    )
    assert nrow(tmp_path, range_errors) == 1
    fix_on_ref = (
        "model,scenario,region,variable,unit,period,value,ref,reldiff\n"
        "R,S,CAZ,Inv|Transport,b$,2005,290.4,292.0,0.5\n"
        "R,S,CAZ,Inv|Transport,b$,2010,334.9,336.6,0.5\n"
        "R,S,CAZ,Other,b$,2010,1,2,0.5\n"
    )
    assert uniq(tmp_path, fix_on_ref) == 2


# ---------------------------------------------------------------------------
# to_dataframe
# ---------------------------------------------------------------------------


class _Table:
    columns = ["jobInSLURM", "RunType", "Runtime", "summationErrors"]

    def to_json_rows(self) -> list[dict[str, object]]:
        return [
            {"_row": "run-a", "jobInSLURM": "NA", "RunType": "nash", "Runtime": 120, "summationErrors": 0},
            {"_row": "run-b", "jobInSLURM": "no", "RunType": "NA", "Runtime": None},
        ]


def cell(df: pd.DataFrame, row: str, col: str) -> object:
    return df.at[row, col]


def test_to_dataframe_from_table() -> None:
    df = to_dataframe(_Table())
    assert isinstance(df, pd.DataFrame)
    assert list(df.columns) == ["jobInSLURM", "RunType", "Runtime", "summationErrors"]
    assert list(df.index) == ["run-a", "run-b"]
    assert df.index.name == "run"
    assert cell(df, "run-a", "Runtime") == 120
    assert cell(df, "run-a", "jobInSLURM") == "NA"
    assert cell(df, "run-b", "Runtime") is None
    assert cell(df, "run-b", "summationErrors") is None
    assert all(dtype is object or str(dtype) == "object" for dtype in df.dtypes)


def test_to_dataframe_from_rows() -> None:
    df = to_dataframe([{"_row": "x", "a": 1}, {"_row": "y", "b": "2", "a": None}])
    assert list(df.columns) == ["a", "b"]
    assert list(df.index) == ["x", "y"]
    assert cell(df, "y", "a") is None
    assert cell(df, "x", "b") is None
    assert to_dataframe([]).shape == (0, 0)
