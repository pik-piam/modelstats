from __future__ import annotations

import warnings

from modelstats._choose import choose_from_list
from tests.conftest import CapturedConsole, capture_console

ITEMS = ["out/C_A-rem-1", "out/C_A-rem-2", "out/SSP2-NPi", "out/SSP2-PkBudg500"]


def choose(*lines: str, items: list[str] = ITEMS) -> tuple[list[str], CapturedConsole]:
    captured = capture_console()
    replies = iter(lines)
    chosen = choose_from_list(items, kind="folders", read=lambda _prompt: next(replies), out=captured.console)
    return chosen, captured


def test_lists_items_and_prompts() -> None:
    chosen, captured = choose("")
    assert chosen == []
    assert captured.lines[1:3] == ["Please choose folders:", ""]
    assert captured.lines[3:7] == [f"{i}: {item}" for i, item in enumerate(ITEMS, 1)]
    assert (
        "Choose folders (numbers, ranges 1-3, comma lists, 'all'/'a', 'p'/'f' for a pattern prompt, a regex; "
        "empty = none): " in captured.text
    )
    assert captured.lines[-1].endswith("Selected: ")


def test_single_number_range_and_comma_list() -> None:
    assert choose("2")[0] == ["out/C_A-rem-2"]
    assert choose("1-3")[0] == ITEMS[:3]
    assert choose("1:3")[0] == ITEMS[:3]
    assert choose("4, 1")[0] == ["out/C_A-rem-1", "out/SSP2-PkBudg500"]
    assert choose("1,1,2")[0] == ITEMS[:2]


def test_all_and_regex() -> None:
    assert choose("all")[0] == ITEMS
    assert choose("a")[0] == ITEMS  # R's "1,a: all" shortcut (BUG-076), not the pattern "a"
    assert choose("2,a")[0] == ITEMS  # inside a list as well
    assert choose("aa")[0] == []  # a pattern
    for answer in ("9,a", "0,a", "a,3-9", "a,1 2", "a,1-"):  # out of range or malformed: asked again
        chosen, captured = choose(answer, "2")
        assert chosen == ["out/C_A-rem-2"] and "Try again, not all in list." in captured.text
    # R's other ways to "all" or to the pattern prompt are regular expressions over the items (BUG-076)
    assert choose("a:3", items=["out/a:3", "out/x"])[0] == ["out/a:3"]
    assert choose("a-3")[0] == [] and choose("2-a")[0] == []
    assert choose("2,p")[0] == [] and choose("a,p")[0] == [] and choose("p,f")[0] == []
    assert choose("2,")[0] == ["out/C_A-rem-2"]  # an empty element is ignored (R: asked again)
    assert choose("a,")[0] == ITEMS and choose(",a")[0] == ITEMS
    assert choose("SSP2")[0] == ITEMS[2:]
    assert choose("rem-[0-9]+$")[0] == ITEMS[:2]
    assert choose("nomatch")[0] == []


def test_pattern_prompts() -> None:
    chosen, captured = choose("p", "SSP2")  # BUG-076: R's "p" entry opened a second prompt
    assert chosen == ITEMS[2:] and "Insert the regular expression: " in captured.text
    chosen, captured = choose("f", "rem-")
    assert chosen == ITEMS[:2] and "Insert the search pattern with fixed=TRUE: " in captured.text
    assert choose("f", ".")[0] == []  # fixed: no metacharacters
    chosen, captured = choose("p", "a(", "SSP2")  # an invalid expression asks for the pattern again (R too)
    assert chosen == ITEMS[2:] and "Try again, not a valid regular expression" in captured.text
    assert captured.text.count("Insert the regular expression") == 2
    assert captured.text.count("Choose folders") == 1
    assert choose("f", " rem")[0] == []  # blanks are part of the pattern
    assert choose("p", "")[0] == [] and choose("f", "")[0] == []  # an empty pattern selects nothing
    replies = iter(["p"])

    def read(_prompt: str) -> str:  # end of input at the second prompt selects nothing as well
        try:
            return next(replies)
        except StopIteration:
            raise EOFError from None

    assert choose_from_list(ITEMS, read=read, out=capture_console().console) == []


def test_invalid_answers_reprompt() -> None:
    with warnings.catch_warnings():
        warnings.simplefilter("error")  # a POSIX class must not warn, it is rejected (BUG-072)
        chosen, captured = choose(
            "9",
            "0",
            "3-2",
            "-2",
            "1-",
            ",",
            "1 2",
            "1:3:4",
            "1" * 5000,
            "2026-10-18",
            "a(",
            "[[:digit:]]",
            "a{4294967296}",
            " 2 ,	4 - 4 ",
        )
    assert chosen == ["out/C_A-rem-2", "out/SSP2-PkBudg500"]
    assert captured.text.count("Choose folders") == 14
    assert captured.text.count("Try again, not all in list.") == 10  # 5000 digits and a date are numbers
    assert captured.text.count("Try again, not a valid regular expression: answer: invalid pattern") == 3
    assert "Try again, not a valid regular expression" in captured.text


def test_end_of_input_selects_nothing() -> None:
    captured = capture_console()

    def eof(_prompt: str) -> str:
        raise EOFError

    assert choose_from_list(ITEMS, read=eof, out=captured.console) == []


def test_empty_list() -> None:
    chosen, captured = choose(items=[])
    assert chosen == []
    assert captured.text == "No folders found that might be selected, returning the empty list.\n"


def test_selected_line_lists_items_in_list_order() -> None:
    _, captured = choose("4,2")
    assert captured.lines[-1].endswith("Selected: out/C_A-rem-2, out/SSP2-PkBudg500")


def test_items_are_printed_verbatim() -> None:
    items = ["out/run[x]", "out/a[/b]", "out/run:cd:"]
    chosen, captured = choose("all", items=items)
    assert chosen == items
    assert captured.lines[3:6] == [f"{i}: {item}" for i, item in enumerate(items, 1)]
    assert captured.lines[-1].endswith("Selected: out/run[x], out/a[/b], out/run:cd:")


def test_numbers_are_right_aligned_and_spaces_do_not_join_digits() -> None:
    items = [f"out/run{n:02d}" for n in range(1, 13)]
    chosen, captured = choose("1 2", "12", items=items)
    assert chosen == ["out/run12"]  # "1 2" is not 12: it was rejected first
    assert captured.lines[3] == " 1: out/run01" and captured.lines[14] == "12: out/run12"


def test_leading_zeros_are_numbers() -> None:
    assert choose("02")[0] == ["out/C_A-rem-2"]
    assert choose("01-03")[0] == ITEMS[:3]
    assert choose("0" * 5000 + "1")[0] == ITEMS[:1]


def test_selected_line_is_not_cut() -> None:
    items = [f"out/{letter * 300}" for letter in "abc"]  # gms cut the line after 666 characters (BUG-104)
    chosen, captured = choose("all", items=items)
    assert chosen == items
    assert captured.lines[-1].endswith(items[-1]) and ", ..." not in captured.text
