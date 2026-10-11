"""``choose_from_list`` reproduces ``gms::chooseFromList`` (gms 0.35.0) on scripted input.

The menu texts below are copied from the stderr of the R goldens ``migration/goldens/rs/prompt-*``
(the thirteen AMT run folders of the fixture output directory); the selection results for direct
inputs (``2,4:5``, ``6:4``, ``1:3``, ``3-4``, ``4,4,2``, ``2, 3``, the empty line, ``02``) were pinned
with ``gms::chooseFromList(items, userinput = ...)`` in R. The search prompts and the end-of-input
behaviour follow decision D-21 (one line per prompt).
"""

from __future__ import annotations

import io
from collections.abc import Callable, Sequence

import pytest

from modelstats.choose import (
    NoMoreInputError,
    ParseFailure,
    choose_from_list,
    choose_pattern_from_list,
    menu_text,
    parse_selection,
    read_stdin_line,
)

FOLDERS = [
    "./SSP2-EU21-NPi2025-AMT_2026-09-18_22.11.36",
    "./SSP2-EU21-PkBudg650-AMT_2026-09-28_13.54.59",
    "./SSP2-EcBudg500-AMT_2026-09-19_01.25.14",
    "./SSP2-NPi2025-calibrate-AMT_2026-09-28_10.27.04",
    "./SSP2-NPi-AMT_2026-08-28_22.06.57",
    "./SSP2-NPi-AMT_2026-09-28_10.30.27",
    "./SSP3-NPi2025-AMT_2026-09-28_17.12.58",
    "./archive",
    "./default-AMT_2026-09-28_13.23.51",
    "./export",
    "./gamscompile",
    "./testOneRegi",
    "./testOneRegi-AMT",
]

# migration/goldens/rs/prompt-ranges.err after the three alert lines, up to "Runs found".
MENU = """

Please choose folders:

 1,a: all
 2: ./SSP2-EU21-NPi2025-AMT_2026-09-18_22.11.36
 3: ./SSP2-EU21-PkBudg650-AMT_2026-09-28_13.54.59
 4: ./SSP2-EcBudg500-AMT_2026-09-19_01.25.14
 5: ./SSP2-NPi2025-calibrate-AMT_2026-09-28_10.27.04
 6: ./SSP2-NPi-AMT_2026-08-28_22.06.57
 7: ./SSP2-NPi-AMT_2026-09-28_10.30.27
 8: ./SSP3-NPi2025-AMT_2026-09-28_17.12.58
 9: ./archive
10: ./default-AMT_2026-09-28_13.23.51
11: ./export
12: ./gamscompile
13: ./testOneRegi
14: ./testOneRegi-AMT
15,p: Search pattern by regular expression...
16,f: Search by fixed pattern...
"""
PROMPT = "\nNumbers entered as 2,4:6,9 or leave empty:\n"
INVALID_X = (
    "Try again, you have to choose some numbers. "
    'Error in eval(parse(text = paste("c(", userinput, ")"))): object \'x\' not found\n\n'
)


def scripted(*lines: str) -> tuple[list[str | None], io.StringIO]:
    return [*lines], io.StringIO()


def reader(lines: list[str | None]) -> Callable[[], str | None]:
    """One line per call, ``None`` once the script is exhausted (end of input)."""

    def read() -> str | None:
        return lines.pop(0) if lines else None

    return read


def run(lines: Sequence[str], items: Sequence[str] = FOLDERS) -> tuple[list[str], str]:
    script, err = scripted(*lines)
    chosen = choose_from_list(items, kind="folders", read_line=reader(script), err=err)
    return chosen, err.getvalue()


# --- the goldens' stderr ------------------------------------------------------------------------


def test_ranges_golden() -> None:
    chosen, err = run(["2,4:6"])
    assert chosen == [FOLDERS[0], FOLDERS[2], FOLDERS[3], FOLDERS[4]]
    assert err == MENU + PROMPT + "Selected: " + ", ".join(chosen) + "\n"


def test_all_golden() -> None:
    chosen, err = run(["a"])
    assert chosen == FOLDERS
    assert err == MENU + PROMPT + "Selected: " + ", ".join(FOLDERS) + "\n"


def test_empty_golden() -> None:
    chosen, err = run([""])
    assert chosen == []
    assert err == MENU + PROMPT + "Selected: \n"


def test_end_of_input_at_the_main_prompt_is_the_empty_selection() -> None:
    chosen, err = run([])
    assert chosen == []
    assert err == MENU + PROMPT + "Selected: \n"


def test_invalid_then_ok_golden() -> None:
    chosen, err = run(["x", "3"])
    assert chosen == [FOLDERS[1]]
    assert err == MENU + PROMPT + MENU + INVALID_X + PROMPT + "Selected: " + FOLDERS[1] + "\n"


def test_prompt_bw_golden_selects_one() -> None:
    chosen, _ = run(["2"])
    assert chosen == [FOLDERS[0]]


# --- the input grammar, pinned against gms 0.35.0 ------------------------------------------------


@pytest.mark.parametrize(
    ("line", "expected"),
    [
        ("2,4:5", ["./a-1", "./c", "./d"]),
        ("6:4", ["./e", "./d", "./c"]),
        ("1:3", ["./a-1", "./b", "./c", "./d", "./e"]),
        ("3-4", ["./b", "./c"]),
        ("4,4,2", ["./c", "./a-1"]),
        ("2, 3", ["./a-1", "./b"]),
        ("", []),
        ("02", ["./a-1"]),
        ("2:2", ["./a-1"]),
        ("a", ["./a-1", "./b", "./c", "./d", "./e"]),
        ("6,a", ["./a-1", "./b", "./c", "./d", "./e"]),
    ],
)
def test_selection_grammar(line: str, expected: list[str]) -> None:
    chosen, _ = run([line], ["./a-1", "./b", "./c", "./d", "./e"])
    assert chosen == expected


def test_parse_selection_binds_the_letters_to_menu_numbers() -> None:
    assert parse_selection("a,p,f", 16) == [1, 15, 16]
    assert parse_selection("a:3", 16) == [1, 2, 3]
    assert parse_selection("2-4,9", 16) == [2, 3, 4, 9]
    assert parse_selection(" 2 ,, 3 ", 16) == [2, 3]
    assert parse_selection("", 16) == []


@pytest.mark.parametrize(
    ("line", "detail"),
    [
        ("x", 'Error in eval(parse(text = paste("c(", userinput, ")"))): object \'x\' not found\n'),
        ("aa", 'Error in eval(parse(text = paste("c(", userinput, ")"))): object \'aa\' not found\n'),
        ("p1", 'Error in eval(parse(text = paste("c(", userinput, ")"))): object \'p1\' not found\n'),
        ("+2", ""),
        ("2.5", ""),
        ("2,", ""),
        ("1a", ""),
    ],
)
def test_parse_selection_rejections(line: str, detail: str) -> None:
    assert parse_selection(line, 16) == ParseFailure(detail)


def test_number_outside_the_menu_is_retried_with_the_list() -> None:
    chosen, err = run(["0,17,3", "2"])
    assert chosen == [FOLDERS[0]]
    assert "Try again, not all in list: 0, 17, 3...\n" in err
    assert err.count("Please choose folders:") == 2


def test_rejected_input_without_detail_prints_the_bare_retry_line() -> None:
    _, err = run(["+2", ""])
    assert "Try again, you have to choose some numbers. \n" in err


# --- the searches: gms:::choosePatternFromList -----------------------------------------------------


def test_regular_expression_search_adds_its_matches_after_the_numbers() -> None:
    chosen, err = run(["2,p", "PkBudg|Ec", "y"])
    assert chosen == [FOLDERS[0], FOLDERS[1], FOLDERS[2]]
    assert "\nInsert the regular expression: \n" in err
    assert (
        "\n\nThe search pattern matches the following folders:\n"
        f"1: {FOLDERS[1]}\n2: {FOLDERS[2]}\n"
        "\nAre you sure these are the right folders? (y/n): \n"
    ) in err


def test_fixed_search_takes_the_pattern_literally_and_repeats_until_confirmed() -> None:
    chosen, err = run(["f", "testOneRegi", "n", "testOneRegi-", "Y"])
    assert chosen == [FOLDERS[12]]
    assert err.count("\nInsert the search pattern with fixed=TRUE: \n") == 2
    assert f"1: {FOLDERS[11]}\n2: {FOLDERS[12]}\n" in err


def test_search_without_matches_says_oops_and_confirms_nothing() -> None:
    chosen, err = run(["p", "nomatch", "y"])
    assert chosen == []
    assert "Oops. You didn't select anything.\n" in err


def test_invalid_regular_expression_is_retried() -> None:
    chosen, err = run(["p", "(", "archive$", "y"])
    assert chosen == [FOLDERS[7]]
    assert (
        "Error in grep(pattern = pattern, theList, fixed = fixed) : \n  invalid regular expression '('" in err
    )
    assert "\n\nMatching created an error. Try again!\n" in err


def test_all_wins_over_a_search() -> None:
    chosen, err = run(["a,p"])
    assert chosen == FOLDERS
    assert "Insert the" not in err


def test_end_of_input_at_a_search_prompt_raises() -> None:
    with pytest.raises(NoMoreInputError):
        run(["p"])
    with pytest.raises(NoMoreInputError):
        run(["f", "archive"])


def test_choose_pattern_from_list_returns_one_based_positions() -> None:
    script, err = scripted("rem-", "y")
    items = ["C_a-rem-1", "C_a-mag-1", "C_b-rem-2"]
    found = choose_pattern_from_list(items, "folders", fixed=True, read_line=reader(script), err=err)
    assert found == [1, 3]


# --- the Selected: line ------------------------------------------------------------------------------


def test_selected_line_is_cut_after_the_limit() -> None:
    items = ["a" * 300, "b" * 300, "c" * 100, "d"]
    script, err = scripted("a")
    choose_from_list(items, read_line=reader(script), err=err)
    assert err.getvalue().endswith(f"Selected: {items[0]}, {items[1]}, {items[2]}, ...\n")
    script, err = scripted("a")
    choose_from_list(items, read_line=reader(script), err=err, limit=1000)
    assert err.getvalue().endswith("Selected: " + ", ".join(items) + "\n")


def test_empty_list_returns_without_a_prompt() -> None:
    script, err = scripted("1")
    assert choose_from_list([], kind="folders", read_line=reader(script), err=err) == []
    assert err.getvalue() == "No folders found that might be selected, returning the empty list.\n"
    assert script == ["1"]


def test_menu_text_width_follows_the_count() -> None:
    assert menu_text(["all", "x", "p", "f"], "items") == (
        "\n\nPlease choose items:\n\n1,a: all\n2: x\n3,p: p\n4,f: f\n"
        "\nNumbers entered as 2,4:6,9 or leave empty:"
    )


def test_read_stdin_line(monkeypatch: pytest.MonkeyPatch) -> None:
    monkeypatch.setattr("sys.stdin", io.StringIO("2,4\r\n\nlast"))
    assert read_stdin_line() == "2,4"
    assert read_stdin_line() == ""
    assert read_stdin_line() == "last"
    assert read_stdin_line() is None
