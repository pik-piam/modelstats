"""modelstats.colors: crayon byte sequences, the enable rules and the loopRuns colour trees.

The byte sequences are the ones 02-python-libraries.md section 4.5 captured from crayon 1.5.x and were re-captured
on 2026-10-01 with crayon 1.5.3 (``options(crayon.enabled = TRUE, crayon.colors = 256)`` and ``16``). The colour
trees were checked against R functions carrying the literal ``if``/``else`` chains of ``R/loopRuns.R`` on 6000
random rows including ``NA`` cells and missing columns (0 differences); the cases below pin every branch.
"""

from __future__ import annotations

import io
from collections.abc import Iterator, Sequence

import pytest

from modelstats.colors import (
    STYLE_NAMES,
    STYLES,
    STYLES_16,
    _run_tput,
    colour_for_magpie,
    colour_for_remind,
    colour_for_row,
    detect_enabled,
    detect_num_colors,
    enable_from_environment,
    is_enabled,
    is_magpie_row,
    num_colors,
    set_enabled,
    set_num_colors,
    style,
)
from modelstats.errors import RParityError

ESC = "\x1b"


@pytest.fixture(autouse=True)
def _reset_colour_state() -> Iterator[None]:
    enabled, depth = is_enabled(), num_colors()
    try:
        yield
    finally:
        set_enabled(enabled, depth)


# ---------------------------------------------------------------------------
# the seven styles, byte-exact
# ---------------------------------------------------------------------------

CRAYON_256: dict[str, tuple[bytes, bytes]] = {
    "orangered": (b"\x1b[38;5;202m", b"\x1b[39m"),
    "orange": (b"\x1b[38;5;214m", b"\x1b[39m"),
    "yellow": (b"\x1b[33m", b"\x1b[39m"),
    "cyan": (b"\x1b[36m", b"\x1b[39m"),
    "green": (b"\x1b[32m", b"\x1b[39m"),
    "magenta": (b"\x1b[35m", b"\x1b[39m"),
    "underline": (b"\x1b[4m", b"\x1b[24m"),
}
CRAYON_16: dict[str, tuple[bytes, bytes]] = {
    "orangered": (b"\x1b[31m", b"\x1b[39m"),
    "orange": (b"\x1b[33m", b"\x1b[39m"),
    "yellow": (b"\x1b[33m", b"\x1b[39m"),
    "cyan": (b"\x1b[36m", b"\x1b[39m"),
    "green": (b"\x1b[32m", b"\x1b[39m"),
    "magenta": (b"\x1b[35m", b"\x1b[39m"),
    "underline": (b"\x1b[4m", b"\x1b[24m"),
}


def test_style_tables_are_the_crayon_sequences() -> None:
    assert STYLE_NAMES == ("orangered", "orange", "yellow", "cyan", "green", "magenta", "underline")
    assert {name: (o.encode(), c.encode()) for name, (o, c) in STYLES.items()} == CRAYON_256
    assert {name: (o.encode(), c.encode()) for name, (o, c) in STYLES_16.items()} == CRAYON_16


@pytest.mark.parametrize("name", STYLE_NAMES)
def test_style_wraps_text_at_256_colours(name: str) -> None:
    set_enabled(True, 256)
    open_sequence, close_sequence = CRAYON_256[name]
    assert style(name, "text").encode() == open_sequence + b"text" + close_sequence


@pytest.mark.parametrize("name", STYLE_NAMES)
def test_style_wraps_text_at_16_colours(name: str) -> None:
    set_enabled(True, 16)
    open_sequence, close_sequence = CRAYON_16[name]
    assert style(name, "text").encode() == open_sequence + b"text" + close_sequence


def test_nested_styling_is_literal() -> None:
    set_enabled(True, 256)
    # yellow(paste0("a", underline("b"), "c")) in crayon
    assert style("yellow", "a" + style("underline", "b") + "c") == f"{ESC}[33ma{ESC}[4mb{ESC}[24mc{ESC}[39m"
    assert style("orangered", style("green", "x")) == f"{ESC}[38;5;202m{ESC}[32mx{ESC}[39m{ESC}[39m"


def test_style_is_plain_when_disabled_and_rejects_unknown_names() -> None:
    set_enabled(False)
    assert style("cyan", "text") == "text"
    assert style("underline", "") == ""
    with pytest.raises(ValueError, match="unknown style"):
        style("red", "text")  # the R alias `red` is make_style("orangered")
    set_enabled(True)
    with pytest.raises(ValueError, match="unknown style"):
        style("bold", "text")


def test_enabled_flag_and_depth_are_process_wide() -> None:
    set_enabled(False)
    assert not is_enabled()
    set_enabled(True)
    assert is_enabled()
    set_num_colors(8)
    assert num_colors() == 8
    assert style("orangered", "x") == f"{ESC}[31mx{ESC}[39m"  # below 256 the 16-colour codes
    set_num_colors(16777216)
    assert style("orangered", "x") == f"{ESC}[38;5;202mx{ESC}[39m"  # truecolor uses the 256 codes
    set_enabled(True, 256)
    assert (is_enabled(), num_colors()) == (True, 256)


# ---------------------------------------------------------------------------
# crayon's detection rules
# ---------------------------------------------------------------------------


class _Tty(io.StringIO):
    def isatty(self) -> bool:
        return True


class _Tput:
    """A canned ``tput colors``: returns ``output``, or raises like a missing command when it is None."""

    def __init__(self, output: str | None) -> None:
        self.output = output
        self.calls = 0

    def __call__(self, argv: Sequence[str]) -> str:
        assert list(argv) == ["tput", "colors"]
        self.calls += 1
        if self.output is None:
            raise FileNotFoundError("tput")
        return self.output


# crayon 1.5.3 on a real pty (`script -qec "Rscript ..."`, COLORTERM / NO_COLOR / R_CLI_NUM_COLORS unset), see the
# verification of Codex finding 7 of the phase 1 review: (TERM, tput output, num_colors)
TTY_CASES: list[tuple[str, str | None, int]] = [
    ("dumb", "-1\n", 1),
    ("dumb", None, 1),
    ("xterm", "8\n", 256),  # tput says 8, crayon lifts TERM == "xterm" to 256
    ("xterm-256color", "256\n", 256),
    ("screen", "8\n", 8),
    ("rxvt", "88\n", 88),
    ("xterm", "0\n", 1),
    ("xterm", "1\n", 1),
    ("foo", None, 1),  # tput fails: guess_tty_colors
    ("screen", None, 8),
    ("xterm", None, 8),  # the guess, not the tput 8 -> 256 rule
    ("linux", "", 8),  # no output: as.numeric(character(0))[1] is NA -> guess
    ("xterm", "abc\n", 8),  # non-numeric: NA -> guess
    ("vt100", "nan\n", 8),
    ("Eterm-color", None, 8),  # "color" anywhere, case-insensitive
    ("", None, 1),
]


@pytest.mark.parametrize(("term", "tput", "expected"), TTY_CASES)
def test_detect_num_colors_follows_crayon_on_a_tty(term: str, tput: str | None, expected: int) -> None:
    assert detect_num_colors(_Tty(), {"TERM": term}, _Tput(tput)) == expected
    assert detect_enabled(_Tty(), {"TERM": term}, _Tput(tput)) is (expected > 1)


def test_colorterm_decides_before_tput() -> None:
    for value, expected in (("yes", 8), ("", 8), ("truecolor", 16777216), ("24bit", 16777216)):
        tput = _Tput("256\n")
        assert detect_num_colors(_Tty(), {"TERM": "xterm", "COLORTERM": value}, tput) == expected
        assert tput.calls == 0


def test_forced_value_no_color_and_non_tty_decide_first() -> None:
    tty, pipe = _Tty(), io.StringIO()
    cases: list[tuple[io.StringIO | None, dict[str, str], int]] = [
        (pipe, {"R_CLI_NUM_COLORS": "256"}, 256),
        (tty, {"R_CLI_NUM_COLORS": "8"}, 8),
        (tty, {"R_CLI_NUM_COLORS": "1"}, 1),
        (tty, {"R_CLI_NUM_COLORS": "abc"}, 1),  # as.integer gives NA
        (pipe, {"R_CLI_NUM_COLORS": "2", "NO_COLOR": ""}, 2),  # the forced value wins
        (tty, {"NO_COLOR": ""}, 1),
        (tty, {"NO_COLOR": "1", "TERM": "xterm-256color", "COLORTERM": "truecolor"}, 1),
        (pipe, {"TERM": "xterm-256color", "COLORTERM": "truecolor"}, 1),
        (None, {"TERM": "xterm"}, 1),
    ]
    for stream, env, expected in cases:
        tput = _Tput("256\n")
        assert detect_num_colors(stream, env, tput) == expected, env
        assert detect_enabled(stream, env, tput) is (expected > 1), env
        assert tput.calls == 0, env


def test_enable_from_environment_applies_detection() -> None:
    assert enable_from_environment(io.StringIO(), {"R_CLI_NUM_COLORS": "256"}) is True
    assert (is_enabled(), num_colors()) == (True, 256)
    assert enable_from_environment(_Tty(), {"TERM": "xterm"}, _Tput("8\n")) is True
    assert (is_enabled(), num_colors()) == (True, 256)
    assert style("orangered", "x") == f"{ESC}[38;5;202mx{ESC}[39m"
    assert enable_from_environment(_Tty(), {"TERM": "screen"}, _Tput("8\n")) is True
    assert (is_enabled(), num_colors()) == (True, 8)
    assert style("orangered", "x") == f"{ESC}[31mx{ESC}[39m"  # crayon's 8-colour bytes equal its 16-colour bytes
    assert enable_from_environment(_Tty(), {"TERM": "xterm", "COLORTERM": "truecolor"}) is True
    assert (is_enabled(), num_colors()) == (True, 16777216)
    assert style("orangered", "x") == f"{ESC}[38;5;202mx{ESC}[39m"  # and its truecolor bytes equal its 256 bytes
    assert enable_from_environment(io.StringIO(), {}) is False
    assert (is_enabled(), num_colors()) == (False, 1)


def test_detect_reads_the_process_environment_by_default(monkeypatch: pytest.MonkeyPatch) -> None:
    monkeypatch.setenv("R_CLI_NUM_COLORS", "256")
    assert detect_enabled(io.StringIO()) is True
    assert detect_num_colors(io.StringIO()) == 256
    monkeypatch.delenv("R_CLI_NUM_COLORS")
    monkeypatch.setenv("NO_COLOR", "1")
    assert detect_enabled(_Tty()) is False
    monkeypatch.delenv("NO_COLOR")
    monkeypatch.delenv("COLORTERM", raising=False)
    monkeypatch.setenv("TERM", "dumb")
    assert detect_num_colors(_Tty(), run=_Tput("-1\n")) == 1


def test_default_tput_runner_reads_stdout_only_and_raises_when_missing() -> None:
    # system("tput colors 2>/dev/null", intern = TRUE): stdout regardless of the exit status, stderr dropped
    assert _run_tput(["sh", "-c", "echo 7; echo noise >&2; exit 3"]) == "7\n"
    with pytest.raises(OSError):
        _run_tput(["modelstats-no-such-tput-xyz", "colors"])
    # the real probe on a tty: whatever tput says, the result is crayon's and at least 1
    assert detect_num_colors(_Tty(), {"TERM": "xterm-256color"}) >= 1


# ---------------------------------------------------------------------------
# the colour decision trees of loopRuns
# ---------------------------------------------------------------------------


def _remind(**overrides: object) -> dict[str, object]:
    row: dict[str, object] = {
        "Runtime": "2.3 hours",
        "jobInSLURM": "no",
        "RunType": "nash",
        "RunStatus": "Normal completion",
        "Warnings": "27",
        "Iter": "26/100",
        "Conv": "converged",
        "modelstat": "2: Locally Optimal",
        "Mif": "yes",
        "runInAppResults": "yes",
    }
    row.update(overrides)
    return row


def _magpie(**overrides: object) -> dict[str, object]:
    row: dict[str, object] = {
        "Runtime": "33.6 mins",
        "jobInSLURM": "no",
        "RunType": "nlp_apr17",
        "RunStatus": "Normal completion",
        "Warnings": "0",
        "Iter": "y2100",
        "Conv": "NA",
        "modelstat": "222222222222222222",
        "Mif": "yes",
        "runInAppResults": "yes",
    }
    row.update(overrides)
    return row


PLAIN = "run  2.3 hours  no  nash  Normal completion  27  26/100  converged  2: Locally Optimal  yes"
CONOPT = "run  > 1.5 hours  12345  nash  conoptspy > 1.2 hours  27  26/100  NA  NA  no"


def test_is_magpie_row_dispatch() -> None:
    assert is_magpie_row(_magpie()) is True
    assert is_magpie_row(_remind(Iter="y1995", RunType="nash")) is True
    assert is_magpie_row(_remind(Iter="26/100", RunType="nlp_apr17")) is True
    assert is_magpie_row(_remind(Iter="26/100", RunType="nash")) is False
    assert is_magpie_row(_remind(Iter=None, RunType=None)) is False  # grepl(NA) is FALSE
    with pytest.raises(RParityError, match=r"^subscript out of bounds$"):
        is_magpie_row({"RunType": "nash"})


@pytest.mark.parametrize(
    ("overrides", "out", "expected"),
    [
        ({"Runtime": "pending"}, PLAIN, "yellow"),
        ({"Runtime": "startup"}, PLAIN, "yellow"),
        ({"Runtime": "> 1.5 hours", "jobInSLURM": "12345"}, CONOPT, "magenta"),
        ({"Runtime": "> 1.5 hours", "jobInSLURM": "12345"}, PLAIN, "cyan"),
        ({"Conv": "converged (had INFES)"}, PLAIN, "green"),
        ({"Conv": "converged (had INFES)", "Mif": "no", "RunStatus": "Execution error"}, PLAIN, "orangered"),
        ({"RunStatus": "not_converged"}, PLAIN, "orangered"),
        ({"RunStatus": "Compilation error"}, PLAIN, "orangered"),
        ({"RunStatus": "interrupted"}, PLAIN, "orangered"),
        ({"RunStatus": "Intermed Infes"}, PLAIN, "orangered"),
        ({}, PLAIN, "green"),
        ({"Conv": "Clb_converged"}, PLAIN, "green"),
        ({"Conv": "not_converged", "RunType": "negishi"}, PLAIN, "green"),
        ({"Conv": "not_converged", "RunType": "nash"}, PLAIN, "orangered"),
        ({"Conv": "converged", "Mif": "no", "RunType": "negishi"}, PLAIN, "green"),
        ({"Conv": "converged", "Mif": "no"}, PLAIN, "orange"),
        ({"Conv": "converged (had INFES)", "Mif": "no"}, PLAIN, "orange"),
        ({"Conv": "not_converged", "Mif": "no", "modelstat": "4: Infeasible"}, PLAIN, "orangered"),
        ({"Conv": "NA", "Mif": "NA", "modelstat": "NA", "jobInSLURM": "NA"}, PLAIN, "cyan"),
        ({"Conv": None, "Mif": "no", "RunStatus": "x", "modelstat": "NA", "jobInSLURM": "NA"}, PLAIN, "cyan"),
    ],
)
def test_colour_for_remind_branches(overrides: dict[str, object], out: str, expected: str) -> None:
    assert colour_for_remind(_remind(**overrides), out) == expected


@pytest.mark.parametrize(
    ("overrides", "out", "expected"),
    [
        ({"Runtime": "pending"}, PLAIN, "yellow"),
        ({"Runtime": "startup", "modelstat": "4: Infeasible"}, PLAIN, None),  # only pending is yellow here
        ({"RunStatus": "not_converged"}, PLAIN, "orangered"),
        ({"RunStatus": "Execution error"}, PLAIN, "orangered"),
        ({"RunStatus": "missing"}, PLAIN, "orangered"),
        ({"RunStatus": "Abort"}, PLAIN, "orangered"),
        ({"RunStatus": "converged"}, PLAIN, "green"),
        ({"RunStatus": "Clb_converged"}, PLAIN, "green"),
        ({}, PLAIN, "green"),  # 222... without a dot
        ({"modelstat": "22.2222"}, PLAIN, None),  # the dot vetoes
        ({"modelstat": "2: Locally Optimal"}, PLAIN, "green"),
        ({"modelstat": "4: Infeasible"}, CONOPT, "magenta"),
        ({"modelstat": "4: Infeasible"}, "run  Run in progress  ", "cyan"),
        ({"modelstat": "4: Infeasible"}, "run  NA  FALSE ", "orangered"),
        ({"modelstat": "4: Infeasible"}, "run FALSE  NA", None),  # " NA " needs the blanks
        ({"modelstat": "4: Infeasible"}, PLAIN, None),
    ],
)
def test_colour_for_magpie_branches(overrides: dict[str, object], out: str, expected: str | None) -> None:
    assert colour_for_magpie(_magpie(**overrides), out) == expected


def test_na_cells_follow_r_three_valued_logic() -> None:
    # NA %in% "pending" and grepl(pattern, NA) are FALSE; NA == "x" inside if() is an R error
    assert colour_for_magpie(_magpie(Runtime=None, RunStatus=None, modelstat="222"), PLAIN) == "green"
    with pytest.raises(RParityError, match=r"^missing value where TRUE/FALSE needed$"):
        colour_for_magpie(_magpie(modelstat=None), PLAIN)
    assert colour_for_remind(_remind(Runtime=None, jobInSLURM="12345"), PLAIN) == "cyan"
    with pytest.raises(RParityError, match=r"^missing value where TRUE/FALSE needed$"):
        colour_for_remind(_remind(jobInSLURM=None), PLAIN)
    # NA && FALSE is FALSE (Mif == "no" vetoes the INFES branch) but NA && TRUE is NA
    assert colour_for_remind(_remind(Conv=None, Mif="no", RunStatus="x"), PLAIN) == "orangered"
    with pytest.raises(RParityError, match=r"^missing value where TRUE/FALSE needed$"):
        colour_for_remind(_remind(Conv=None, Mif="yes"), PLAIN)
    with pytest.raises(RParityError, match=r"^missing value where TRUE/FALSE needed$"):
        colour_for_remind(_remind(Conv="converged (had INFES)", Mif=None), PLAIN)
    assert colour_for_remind(_remind(Conv="x", Mif=None, RunStatus="x", RunType="negishi"), PLAIN) == "green"
    # Mif == "no" is NA, but NA && FALSE is FALSE: the orange branch is skipped without an error
    assert colour_for_remind(_remind(Conv="x", Mif=None, RunStatus="x", modelstat="x"), PLAIN) == "orangered"


def test_missing_columns_and_numeric_cells() -> None:
    with pytest.raises(RParityError, match=r"^subscript out of bounds$"):
        colour_for_remind({"Runtime": "pending2"}, PLAIN)
    with pytest.raises(RParityError, match=r"^subscript out of bounds$"):
        colour_for_magpie({"Runtime": "x", "RunStatus": "x"}, PLAIN)
    # unlist() turns numbers into their paste0 text
    assert colour_for_magpie(_magpie(modelstat=222222), PLAIN) == "green"
    assert colour_for_magpie(_magpie(modelstat=2.5), PLAIN) is None
    assert colour_for_remind(_remind(Mif=False), PLAIN) == "green"


def test_colour_for_row_dispatches_on_the_magpie_test() -> None:
    assert colour_for_row(_magpie(), PLAIN) == "green"
    assert colour_for_row(_magpie(Runtime="startup", modelstat="4: Infeasible"), PLAIN) is None
    assert colour_for_row(_remind(Runtime="startup"), PLAIN) == "yellow"
    assert colour_for_row(_remind(Iter="y2100", Runtime="startup", modelstat="4: Infeasible"), PLAIN) is None
