"""``modelstats.choose.choose_from_list`` against ``gms::chooseFromList`` as the rs goldens show it.

The menu text R printed for the 13 AMT folders is embedded verbatim from
``migration/goldens/rs/prompt-*.err``; a second test re-reads those files when the golden
tree is present. Everything R printed before ``Selected:`` must match byte for byte; the
selection itself differs only where D-21 says so (R read every piped line at once).
"""

from __future__ import annotations

import io
from pathlib import Path

import pytest

from modelstats.choose import _r_eval_c, _REvalError, choose_from_list
from modelstats.errors import RParityError

REPO = Path(__file__).resolve().parents[2]
RS_GOLDENS = REPO / "migration" / "goldens" / "rs"

ITEMS = [
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

# migration/goldens/rs/prompt-ranges.err, between the hint line and "Runs found"
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
RANGES_SELECTED = (
    "Selected: ./SSP2-EU21-NPi2025-AMT_2026-09-18_22.11.36, ./SSP2-EcBudg500-AMT_2026-09-19_01.25.14, "
    "./SSP2-NPi2025-calibrate-AMT_2026-09-28_10.27.04, ./SSP2-NPi-AMT_2026-08-28_22.06.57\n"
)
INVALID_X = (
    "Try again, you have to choose some numbers. "
    'Error in eval(parse(text = paste("c(", userinput, ")"))): object \'x\' not found\n\n'
)


def run(items: list[str], stdin_text: str) -> tuple[str, list[str]]:
    out = io.StringIO()
    selected = choose_from_list(items, stdin=io.StringIO(stdin_text), stdout=out)
    return out.getvalue(), selected


def golden_span(case: str) -> str | None:
    """What R printed between the ``Did you know?`` hint and ``Runs found``."""
    path = RS_GOLDENS / f"{case}.err"
    if not path.is_file():
        return None
    lines = path.read_text(encoding="utf-8").split("\n")
    start = next(i for i, line in enumerate(lines) if "Did you know?" in line) + 1
    end = next(i for i, line in enumerate(lines) if "Runs found:" in line)
    return "\n".join(lines[start:end]) + "\n"


# --- the goldens ---------------------------------------------------------------------------------


def test_ranges() -> None:
    out, selected = run(ITEMS, "2,4:6\n")
    assert out == MENU + PROMPT + RANGES_SELECTED
    assert selected == [ITEMS[0], ITEMS[2], ITEMS[3], ITEMS[4]]


def test_all() -> None:
    out, selected = run(ITEMS, "a\n")
    assert out == MENU + PROMPT + "Selected: " + ", ".join(ITEMS) + "\n"
    assert selected == ITEMS


def test_one_number() -> None:
    out, selected = run(ITEMS, "2\n")
    assert out == MENU + PROMPT + f"Selected: {ITEMS[0]}\n"
    assert selected == [ITEMS[0]]


def test_empty_line_selects_nothing() -> None:
    out, selected = run(ITEMS, "\n")
    assert out == MENU + PROMPT + "Selected: \n"
    assert selected == []


def test_eof_at_the_main_prompt_is_r_character0() -> None:
    out, selected = run(ITEMS, "")
    assert out == MENU + PROMPT + "Selected: \n"
    assert selected == []


def test_invalid_then_ok() -> None:
    out, selected = run(ITEMS, "x\n3\n")
    assert out == MENU + PROMPT + MENU + INVALID_X + PROMPT + f"Selected: {ITEMS[1]}\n"
    assert selected == [ITEMS[1]]  # D-21: R read "3" together with "x" and selected nothing


@pytest.mark.skipif(not RS_GOLDENS.is_dir(), reason="migration/goldens is absent")
@pytest.mark.parametrize(
    ("case", "stdin_text", "before_selected"),
    [
        ("prompt-ranges", "2,4:6\n", False),
        ("prompt-all", "a\n", False),
        ("prompt-bw", "2\n", False),
        ("prompt-empty", "\n", False),
        ("prompt-invalid-then-ok", "x\n3\n", True),
    ],
)
def test_against_the_rs_goldens(case: str, stdin_text: str, before_selected: bool) -> None:
    golden = golden_span(case)
    assert golden is not None
    out, _ = run(ITEMS, stdin_text)
    if before_selected:  # D-21: the selection differs, everything printed before it must not
        cut = golden.index("Selected: ")
        assert out[:cut] == golden[:cut]
    else:
        assert out == golden


@pytest.mark.skipif(not RS_GOLDENS.is_dir(), reason="migration/goldens is absent")
def test_sortroot_prompt_golden() -> None:
    names = ["A1", "B", "C_SSP2-rem-1", "Run1", "SSP2-NPi-AMT_2026-09-25_9.11.12", "SSP2-NPi-AMT_2026-09-25_10.11.12"]
    names += ["_x", "a2", "a10", "b", "run-rem-1", "run-rem-2", "run-rem-10", "x-1", "x.1", "x_1"]
    out, selected = run([f"./output/{name}" for name in names], "2,4:6\n")
    assert out == golden_span("sortroot--prompt")
    assert selected == [
        "./output/A1",
        "./output/C_SSP2-rem-1",
        "./output/Run1",
        "./output/SSP2-NPi-AMT_2026-09-25_9.11.12",
    ]


# --- input syntax --------------------------------------------------------------------------------


@pytest.mark.parametrize(
    ("text", "expected"),
    [
        ("2,4:6", [0, 2, 3, 4]),
        ("6:4", [4, 3, 2]),
        ("2, 4 - 6", [0, 2, 3, 4]),  # spaces vanish, "-" is ":"
        ("2,,3", [0, 1]),  # ",," collapses
        ("2,2,3", [0, 1]),  # unique
        ("007", [5]),
        ("14:12", [12, 11, 10]),  # selection order is kept
    ],
)
def test_selection_syntax(text: str, expected: list[int]) -> None:
    _, selected = run(ITEMS, text + "\n")
    assert selected == [ITEMS[i] for i in expected]


def test_a_range_touching_one_selects_everything() -> None:
    _, selected = run(ITEMS, "1:2:3\n")  # 1:3 contains 1 == "all"
    assert selected == ITEMS
    _, selected = run(ITEMS, "a:3\n")
    assert selected == ITEMS


def test_f_colon_p_is_a_descending_range() -> None:
    # f:p is 16:15; the pattern prompt still comes first because R tests `pattern` before `fixed`
    out, selected = run(ITEMS, "f:p\nPkBudg\ny\ntestOneRegi\ny\n")
    assert "Insert the regular expression: " in out
    assert out.index("Insert the regular expression: ") < out.index("Insert the search pattern with fixed=TRUE: ")
    assert selected == [ITEMS[1], ITEMS[11], ITEMS[12]]


@pytest.mark.parametrize(
    ("text", "message"),
    [
        ("17", "Try again, not all in list: 17...\n"),
        ("0", "Try again, not all in list: 0...\n"),
        ("1,99", "Try again, not all in list: 1, 99...\n"),
        ("1:0", "Try again, not all in list: 1, 0...\n"),
        ("100000", "Try again, not all in list: 1e+05...\n"),
        ("T", "Try again, you have to choose some numbers. \n"),
        ("1.5", "Try again, you have to choose some numbers. \n"),
        ("x", INVALID_X),
        ("A", INVALID_X.replace("'x'", "'A'")),
        (",2", "Try again, you have to choose some numbers. Error in c(, 2): argument 1 is empty\n\n"),
        ("2,", "Try again, you have to choose some numbers. Error in c(2, ): argument 2 is empty\n\n"),
        (
            "a:",
            "Try again, you have to choose some numbers. "
            'Error in parse(text = paste("c(", userinput, ")")): <text>:1:7: unexpected \')\'\n'
            "1: c( a: )\n          ^\n\n",
        ),
    ],
)
def test_reprompt_messages(text: str, message: str) -> None:
    out, selected = run(ITEMS, text + "\n2\n")
    assert out == MENU + PROMPT + MENU + message + PROMPT + f"Selected: {ITEMS[0]}\n"
    assert selected == [ITEMS[0]]


def test_not_in_list_message_is_cut_at_240_characters() -> None:
    out, _ = run(ITEMS, "1:99999\n2\n")
    line = next(line for line in out.split("\n") if line.startswith("Try again, not all in list: "))
    pasted = line[len("Try again, not all in list: ") : -len("...")]
    assert len(pasted) == 240
    assert pasted.startswith("1, 2, 3, 4, 5, ")


def test_huge_range_is_rs_error() -> None:
    out, selected = run(ITEMS, "1:99999999999999999999\n2\n")
    assert "Error in 1:1e+20: result would be too long a vector\n" in out
    assert selected == [ITEMS[0]]


# --- the pattern prompts -------------------------------------------------------------------------

PATTERN_DIALOGUE = (
    "\nInsert the regular expression: \n"
    "\n\nThe search pattern matches the following folders:\n"
    "1: ./SSP2-EU21-PkBudg650-AMT_2026-09-28_13.54.59\n"
    "\nAre you sure these are the right folders? (y/n): \n"
)


def test_pattern_search() -> None:
    out, selected = run(ITEMS, "p\nPkBudg\ny\n")
    assert out == MENU + PROMPT + PATTERN_DIALOGUE + f"Selected: {ITEMS[1]}\n"
    assert selected == [ITEMS[1]]


def test_pattern_search_by_number() -> None:
    _, selected = run(ITEMS, "15\n^./SSP3\nY\n")
    assert selected == [ITEMS[6]]


def test_fixed_search_and_decline_then_retry() -> None:
    out, selected = run(ITEMS, "f\ntestOneRegi\nn\ntestOneRegi-AMT\ny\n")
    assert out == MENU + PROMPT + (
        "\nInsert the search pattern with fixed=TRUE: \n"
        "\n\nThe search pattern matches the following folders:\n"
        "1: ./testOneRegi\n2: ./testOneRegi-AMT\n"
        "\nAre you sure these are the right folders? (y/n): \n"
        "\nInsert the search pattern with fixed=TRUE: \n"
        "\n\nThe search pattern matches the following folders:\n"
        "1: ./testOneRegi-AMT\n"
        "\nAre you sure these are the right folders? (y/n): \n"
        f"Selected: {ITEMS[12]}\n"
    )
    assert selected == [ITEMS[12]]


def test_fixed_search_is_not_a_regex() -> None:
    _, selected = run(ITEMS, "f\n.\ny\n")  # every item starts with "./"
    assert selected == ITEMS
    _, selected = run(ITEMS, "f\nSSP[23]\ny\n")
    assert selected == []


def test_no_match_says_oops() -> None:
    out, selected = run(ITEMS, "p\nnothing-like-this\ny\n")
    assert "\nInsert the regular expression: \nOops. You didn't select anything.\n\nAre you sure" in out
    assert selected == []


def test_invalid_regex_reprompts() -> None:
    out, selected = run(ITEMS, "p\n(\nPkBudg\ny\n")
    assert (
        "Error in grep(pattern = pattern, theList, fixed = fixed) : \n  invalid regular expression '(', reason '" in out
    )
    assert "\n\nMatching created an error. Try again!\n\nInsert the regular expression: \n" in out
    assert selected == [ITEMS[1]]


def test_numbers_then_pattern_then_fixed_in_selection_order() -> None:
    _, selected = run(ITEMS, "14,15,16\nEU21\ny\nNPi\ny\n")
    assert selected == [ITEMS[12], ITEMS[0], ITEMS[1], ITEMS[3], ITEMS[4], ITEMS[5], ITEMS[6]]


def test_eof_at_the_pattern_prompt() -> None:
    with pytest.raises(EOFError):
        run(ITEMS, "p\n")


def test_eof_at_the_confirmation_is_r_argument_of_length_zero() -> None:
    with pytest.raises(RParityError, match="^argument is of length zero$"):
        run(ITEMS, "p\nPkBudg\n")


# --- odds and ends -------------------------------------------------------------------------------


def test_empty_list() -> None:
    out, selected = run([], "a\n")
    assert out == "No folders found that might be selected, returning the empty list.\n"
    assert selected == []


def test_type_in_messages() -> None:
    out = io.StringIO()
    choose_from_list(["x"], type="items", stdin=io.StringIO("1\n"), stdout=out)
    assert out.getvalue().startswith("\n\nPlease choose items:\n\n1,a: all\n2: x\n3,p: ")


def test_selected_message_stops_after_666_characters() -> None:
    items = [f"item-{i}-" + "x" * 60 for i in range(20)]  # 67 characters each
    out, selected = run(items, "a\n")
    line = out[out.index("Selected: ") :]
    assert line == "Selected: " + ", ".join(items[:10]) + ", ...\n"
    assert selected == items


def test_crlf_input() -> None:
    _, selected = run(ITEMS, "2,3\r\n")
    assert selected == ITEMS[:2]


def test_menu_width_grows_with_the_list() -> None:
    out, _ = run([f"r{i}" for i in range(98)], "\n")
    assert "\n  1,a: all\n  2: r0\n" in out
    assert "\n100,p: Search pattern by regular expression...\n101,f: Search by fixed pattern...\n" in out


# --- the R expression emulation (texts verified with Rscript, R 4.6.1) ---------------------------

ENV = {"a": 1.0, "p": 15.0, "f": 16.0}
PARSE = 'Error in parse(text = paste("c(", userinput, ")")): '
EVAL = 'Error in eval(parse(text = paste("c(", userinput, ")"))): '


@pytest.mark.parametrize(
    ("text", "values"),
    [
        ("", []),
        ("2,3", [2, 3]),
        ("1:2:3", [1, 2, 3]),
        ("1:2:3:4", [1, 2, 3, 4]),
        ("3:1", [3, 2, 1]),
        ("3:1:2", [3, 2]),
        ("a:3", [1, 2, 3]),
        ("f:p", [16, 15]),
        ("2:a", [2, 1]),
        ("a:a", [1]),
        ("007", [7]),
        ("0", [0]),
        ("1:0", [1, 0]),
        ("2,2,2", [2, 2, 2]),
        ("99999999999999999999", [1e20]),
        ("T", [1.0]),
        ("1.5", [1.5]),
        ("1e2", [100.0]),
        ("0x10", [16.0]),
    ],
)
def test_r_eval_values(text: str, values: list[float]) -> None:
    assert _r_eval_c(text, ENV) == (values, False)


@pytest.mark.parametrize(
    ("text", "message"),
    [
        ("x", EVAL + "object 'x' not found\n"),
        ("x1", EVAL + "object 'x1' not found\n"),
        ("ab", EVAL + "object 'ab' not found\n"),
        ("x:1", EVAL + "object 'x' not found\n"),
        ("a:x", EVAL + "object 'x' not found\n"),
        ("2:3:x", EVAL + "object 'x' not found\n"),
        ("1,x", EVAL + "object 'x' not found\n"),
        ("x,", EVAL + "object 'x' not found\n"),
        (",x", "Error in c(, x): argument 1 is empty\n"),
        (",2", "Error in c(, 2): argument 1 is empty\n"),
        ("2,", "Error in c(2, ): argument 2 is empty\n"),
        ("1,,2", "Error in c(1, , 2): argument 2 is empty\n"),
        (",", "Error in c(, ): argument 1 is empty\n"),
        ("2,3,", "Error in c(2, 3, ): argument 3 is empty\n"),
        ("a:", PARSE + "<text>:1:7: unexpected ')'\n1: c( a: )\n          ^\n"),
        ("1:", PARSE + "<text>:1:7: unexpected ')'\n1: c( 1: )\n          ^\n"),
        (":", PARSE + "<text>:1:4: unexpected ':'\n1: c( :\n       ^\n"),
        ("1;2", PARSE + "<text>:1:5: unexpected ';'\n1: c( 1;\n        ^\n"),
        ("1,2;3", PARSE + "<text>:1:7: unexpected ';'\n1: c( 1,2;\n          ^\n"),
        ("1x", PARSE + "<text>:1:5: unexpected symbol\n1: c( 1x\n        ^\n"),
        ("1xy", PARSE + "<text>:1:5: unexpected symbol\n1: c( 1xy\n        ^\n"),
        ("12x", PARSE + "<text>:1:6: unexpected symbol\n1: c( 12x\n         ^\n"),
        ("1:2x", PARSE + "<text>:1:7: unexpected symbol\n1: c( 1:2x\n          ^\n"),
        ("$", PARSE + "<text>:1:4: unexpected '$'\n1: c( $\n       ^\n"),
        ("a::3", PARSE + "<text>:1:7: unexpected numeric constant\n1: c( a::3\n          ^\n"),
        ("a::33", PARSE + "<text>:1:7: unexpected numeric constant\n1: c( a::33\n          ^\n"),
        ("1::2", PARSE + "<text>:1:5: unexpected '::'\n1: c( 1::\n        ^\n"),
        ("a::a", "Error in loadNamespace(x): there is no package called ‘a’\n"),
        ("a:::a", "Error in loadNamespace(x): there is no package called ‘a’\n"),
        ("1:99999999999999999999", "Error in 1:1e+20: result would be too long a vector\n"),
    ],
)
def test_r_eval_errors(text: str, message: str) -> None:
    with pytest.raises(_REvalError) as excinfo:
        _r_eval_c(text, ENV)
    assert excinfo.value.text == message


def test_long_ranges_are_cut_but_flagged() -> None:
    values, truncated = _r_eval_c("1:5000", ENV)
    assert truncated is True
    assert values == [float(i) for i in range(1, 1001)]


# --- the range cap follows the menu size (Codex diff finding 9) -----------------------------------


def test_ranges_are_validated_against_the_menu_not_a_fixed_cap() -> None:
    # R's only check is `any(!identifier %in% seq_along(theList))`: 2:1100 on a 1200-entry menu is valid
    items = [f"./run{i:04d}" for i in range(1, 1201)]
    out, selected = run(items, "2:1100\n")
    assert "Try again" not in out
    assert selected == items[:1099]
    # a range longer than the menu is still rejected, with the pasted numbers cut at 240 characters
    out, selected = run(items, "2:5000\n\n")
    assert "Try again, not all in list: " in out
    pasted = out.split("Try again, not all in list: ", 1)[1].split("...", 1)[0]
    assert len(pasted) == 240
    assert selected == []


# --- POSIX character classes in `p` patterns (Codex diff finding 11; R semantics verified with TRE) ---


@pytest.mark.filterwarnings("error::FutureWarning")
def test_posix_class_in_a_pattern() -> None:
    out, selected = run(ITEMS, "p\n[[:digit:]]{4}-\ny\n")
    assert "FutureWarning" not in out
    assert selected == [ITEMS[i] for i in (0, 1, 2, 3, 4, 5, 6, 8)]


@pytest.mark.filterwarnings("error::FutureWarning")
def test_negated_posix_class_in_a_pattern() -> None:
    _, selected = run(ITEMS, "p\n[^[:digit:]]$\ny\n")
    assert selected == [ITEMS[i] for i in (7, 9, 10, 11, 12)]


@pytest.mark.filterwarnings("error::FutureWarning")
def test_mixed_set_with_a_posix_class() -> None:
    _, selected = run(ITEMS, "p\n^\\./[de[:digit:]]\ny\n")
    assert selected == [ITEMS[8], ITEMS[9]]


@pytest.mark.filterwarnings("error::FutureWarning")
def test_unknown_posix_class_reprompts_like_r() -> None:
    out, selected = run(ITEMS, "p\n[[:foo:]]\nPkBudg\ny\n")
    assert "invalid regular expression '[[:foo:]]', reason 'Unknown character class name'" in out
    assert "Matching created an error. Try again!" in out
    assert selected == [ITEMS[1]]


@pytest.mark.filterwarnings("error::FutureWarning")
def test_class_token_outside_a_bracket_is_a_plain_set() -> None:
    # TRE and re agree: grep("[:digit:]", ...) matches the characters : d i g t
    items = ["d", "1", ":", "run1", "run:", "xyz"]
    _, selected = run(items, "p\n[:digit:]\ny\n")
    assert selected == ["d", ":", "run:"]
