from __future__ import annotations

from pathlib import Path

import pytest

from modelstats import _textscan


def test_last_match_finds_the_last_line_across_chunk_boundaries(tmp_path: Path) -> None:
    path = tmp_path / "full.gms"
    filler = "x" * 1000 + "\n"
    text = "cm_iteration_max = 30;\n" + filler * 3000 + "$setglobal cm_iteration_max = 50;  \n" + filler * 10
    path.write_text(text)
    assert path.stat().st_size > 2 * _textscan._CHUNK
    assert (
        _textscan.last_match(path, r"cm_iteration_max = [1-9].*;[ ]*$")
        == "$setglobal cm_iteration_max = 50;  "
    )


def test_last_match_without_trailing_newline_and_single_line(tmp_path: Path) -> None:
    path = tmp_path / "f"
    path.write_text("LOOPS = 3")
    assert _textscan.last_match(path, "LOOPS") == "LOOPS = 3"
    path.write_text("a\nLOOPS = 1\nLOOPS = 2\n")
    assert _textscan.last_match(path, "LOOPS") == "LOOPS = 2"
    assert _textscan.last_match(path, "nothing") is None


def test_first_and_all_matches(tmp_path: Path) -> None:
    path = tmp_path / "full.log"
    path.write_text("*** Status: Execution error(s)\nfoo\n*** Status: Normal completion\n")
    assert _textscan.first_match(path, r"\*\*\* Status: ") == "*** Status: Execution error(s)"
    assert _textscan.all_matches(path, r"\*\*\* Status: ") == [
        "*** Status: Execution error(s)",
        "*** Status: Normal completion",
    ]
    assert _textscan.all_matches(path, "absent") == []


def test_last_line_and_last_nonempty_line(tmp_path: Path) -> None:
    path = tmp_path / "log.txt"
    path.write_text("one\nStarting MAgPIE...\n   \n\n")
    assert _textscan.last_line(path) == ""
    assert _textscan.last_nonempty_line(path) == "Starting MAgPIE..."
    path.write_text("")
    assert _textscan.last_line(path) is None
    assert _textscan.last_nonempty_line(path) is None
    path.write_text("\n")
    assert _textscan.last_line(path) == ""


def test_missing_file_is_no_match(tmp_path: Path) -> None:
    path = tmp_path / "missing"
    assert _textscan.last_match(path, "x") is None
    assert _textscan.first_match(path, "x") is None
    assert _textscan.all_matches(path, "x") == []
    assert _textscan.last_line(path) is None
    assert _textscan.last_nonempty_line(path) is None
    assert _textscan.find_all_text(path, "x") == []


def test_invalid_utf8_is_replaced(tmp_path: Path) -> None:
    path = tmp_path / "f"
    path.write_bytes(b"ok\n\xff\xfe LOOPS = 2\n")
    assert _textscan.last_match(path, "LOOPS") == "�� LOOPS = 2"
    assert _textscan.last_line(path) == "�� LOOPS = 2"


def test_find_all_text_warning_blocks(tmp_path: Path) -> None:
    # the grep -zoP patterns of getRunStatus.R:258 and :272, checked against GNU grep output
    slurm = tmp_path / "slurm.log"
    slurm.write_text("x\nWarning messages:\n1: a\n2: b\n3\nWarning messages:\n1: c\n4\n")
    matches = _textscan.find_all_text(slurm, r"Warning messages:\n([0-9]+:(.*\n)?.*\n)*([0-9]+)")
    assert [m.group(3) for m in matches] == ["3", "4"]
    log = tmp_path / "log.txt"
    log.write_text(
        "start\nWarning messages:\n1: first warning\n  explanation line\n2: second\nsome other line\n"
        "There were 3 warnings\nmore\nWarning messages:\n1: third\n2: fourth\n3: fifth\n"
        "There were 50 or more warnings\nThere were 5 warnings\nend\n"
    )
    blocks = _textscan.find_all_text(log, r"Warning messages:\n([0-9]+:(.*\n)?.*\n)*")
    assert len(blocks) == 2
    counted = sum(
        len([ln for ln in m.group(0).splitlines() if ln[:1].isdigit() and ":" in ln]) for m in blocks
    )
    assert counted == 5
    totals = _textscan.find_all_text(log, r"There were ([0-9]+)( or more)? warnings")
    assert [m.group(1) for m in totals] == ["3", "50", "5"]


def test_count_files_is_recursive(tmp_path: Path) -> None:
    (tmp_path / "fulldata_1.gdx").write_text("")
    (tmp_path / "sub").mkdir()
    (tmp_path / "sub" / "fulldata_2.gdx").write_text("")
    (tmp_path / "input_1.gdx").write_text("")
    assert _textscan.count_files(tmp_path, "fulldata_*.gdx") == 2
    assert _textscan.count_files(tmp_path, "input_*.gdx") == 1
    assert _textscan.count_files(tmp_path / "missing", "*.gdx") == 0


def test_csv_helpers(tmp_path: Path) -> None:
    import pytest

    from modelstats._textscan import csv_line_count, csv_rows, csv_unique_values
    from modelstats.errors import ModelstatsError

    path = tmp_path / "x.csv"
    path.write_text("variable,value\nA,1\n,,extra\nB,2\n\n  , \nA,3\n")
    assert csv_rows(path, ",") == [
        {"variable": "A", "value": "1"},
        {"variable": "B", "value": "2"},
        {"variable": "A", "value": "3"},
    ]
    assert csv_unique_values(path, "variable") == 2
    assert csv_unique_values(path, "absent") == 0
    assert csv_line_count(path, ",") == 4
    path.write_text("")
    assert csv_rows(path, ",") == [] and csv_line_count(path, ";") == 0
    path.write_text("variable\n" + "x" * 200000 + "\n")
    with pytest.raises(ModelstatsError):
        csv_rows(path, ",")
    with pytest.raises(ModelstatsError):
        csv_line_count(tmp_path / "missing.csv", ";")


def test_r_strsplit() -> None:
    from modelstats._textscan import r_strsplit

    assert r_strsplit("a,b", ",") == ["a", "b"]
    assert r_strsplit("a,b,", ",") == ["a", "b"]
    assert r_strsplit("a,,b", ",") == ["a", "", "b"]
    assert r_strsplit(",a", ",") == ["", "a"]
    assert r_strsplit("a,,", ",") == ["a", ""]
    assert r_strsplit("", ",") == []
    assert r_strsplit("x y  ", " ") == ["x", "y", ""]


def test_compile_pattern() -> None:
    from modelstats._textscan import compile_pattern
    from modelstats.errors import ModelstatsError

    assert compile_pattern("a|b", source="-f").search("xb")
    with pytest.raises(ModelstatsError, match=r"^-f: invalid pattern 'a\[': "):
        compile_pattern("a[", source="-f")
    huge = "a{" + "1" * 5000 + "}"  # a repeat count beyond int()'s digit limit
    for pattern in ("a{4294967296}", "(" * 5000 + ")" * 5000, "[[:digit:]]", huge):
        with pytest.raises(ModelstatsError, match="invalid pattern"):
            compile_pattern(pattern, source="x")


def test_last_nonempty_line_counts_only_spaces_and_tabs(tmp_path: Path) -> None:
    from modelstats._textscan import last_nonempty_line

    log = tmp_path / "log.txt"
    log.write_bytes(b"Starting MAgPIE...\n\r\n \t\n")
    assert last_nonempty_line(log) == "\r"  # awk's NF saw a field in "\r"
