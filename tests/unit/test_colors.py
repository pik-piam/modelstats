"""``modelstats.colors`` reproduces crayon's bytes and the row colour trees of ``R/loopRuns.R``.

The escape sequences were captured from crayon 1.5.3 with ``options(crayon.enabled = TRUE,
crayon.colors = 256)`` and ``16`` (``migration/harness/checks/crayon_codes.R``; hexdumps compared on
2026-10-01). The decision-tree cases follow ``R/loopRuns.R:94-135`` branch by branch, with R's
three-valued logic for a real ``NA`` (``None``) where the R code would stop with an error.
"""

from __future__ import annotations

import os
import shutil
import subprocess
import sys
from collections.abc import Callable

import pytest

from modelstats.colors import (
    BASIC_COLOURS,
    EXTENDED_COLOURS,
    MISSING_VALUE_IN_IF,
    PLAIN,
    SUBSCRIPT_OUT_OF_BOUNDS,
    ColourRuleError,
    Crayon,
    Style,
    StyleName,
    colour_for_magpie,
    colour_for_remind,
    colour_for_row,
    colour_legend,
    is_magpie_row,
    num_ansi_colours,
)
from modelstats.formatting import CellValue

ESC = "\x1b["

# --- bytes -----------------------------------------------------------------------------------------


@pytest.mark.parametrize(
    ("name", "expected"),
    [
        (StyleName.ORANGERED, f"{ESC}38;5;202mx{ESC}39m"),
        (StyleName.ORANGE, f"{ESC}38;5;214mx{ESC}39m"),
        (StyleName.YELLOW, f"{ESC}33mx{ESC}39m"),
        (StyleName.CYAN, f"{ESC}36mx{ESC}39m"),
        (StyleName.GREEN, f"{ESC}32mx{ESC}39m"),
        (StyleName.MAGENTA, f"{ESC}35mx{ESC}39m"),
        (StyleName.UNDERLINE, f"{ESC}4mx{ESC}24m"),
    ],
)
def test_256_colour_sequences_equal_crayon(name: StyleName, expected: str) -> None:
    assert Crayon().style(name)("x") == expected
    assert EXTENDED_COLOURS[name]("x") == expected


@pytest.mark.parametrize(
    ("name", "expected"),
    [(StyleName.ORANGERED, f"{ESC}31mx{ESC}39m"), (StyleName.ORANGE, f"{ESC}33mx{ESC}39m")],
)
def test_16_colour_fallback_of_the_named_colours(name: StyleName, expected: str) -> None:
    assert Crayon(colours=16).style(name)("x") == expected
    assert Crayon(colours=8).style(name)("x") == expected
    assert BASIC_COLOURS[name]("x") == expected


def test_the_other_styles_do_not_depend_on_the_depth() -> None:
    for name in StyleName:
        if name not in (StyleName.ORANGERED, StyleName.ORANGE):
            assert BASIC_COLOURS[name] == EXTENDED_COLOURS[name]


def test_truecolor_uses_the_256_index_codes() -> None:
    assert Crayon(colours=256**3).style(StyleName.ORANGERED) == EXTENDED_COLOURS[StyleName.ORANGERED]


def test_nested_colour_restores_the_enclosing_colour_like_crayon_template() -> None:
    """R with crayon 1.5.3 at 256 colours: ``yellow(paste0("a", green("b"), "c"))`` and green in green."""
    crayon = Crayon()
    assert crayon.yellow("a" + crayon.green("b") + "c") == f"{ESC}33ma{ESC}32mb{ESC}33mc{ESC}39m"
    assert crayon.green("a" + crayon.green("b") + "c") == f"{ESC}32ma{ESC}32mb{ESC}32mc{ESC}39m"


def test_underline_inside_a_colour_stays_literal_like_crayon() -> None:
    """``ESC[24m`` is not the colour's closing sequence, so nothing is replaced (same R capture)."""
    crayon = Crayon()
    assert crayon.yellow("a" + crayon.underline("b") + "c") == f"{ESC}33ma{ESC}4mb{ESC}24mc{ESC}39m"


def test_every_colour_closes_with_39_and_underline_with_24() -> None:
    for name, style in EXTENDED_COLOURS.items():
        assert style.close == (f"{ESC}24m" if name is StyleName.UNDERLINE else f"{ESC}39m")


def test_disabled_crayon_emits_nothing() -> None:
    crayon = Crayon.disabled()
    assert crayon.red("x") == "x"
    assert crayon.underline("x") == "x"
    assert crayon.style(StyleName.CYAN) is PLAIN
    assert crayon.paint(StyleName.GREEN, "x") == "x"


def test_paint_without_a_decision_leaves_the_text() -> None:
    assert Crayon().paint(None, "x") == "x"


def test_named_accessors_map_to_the_styles() -> None:
    crayon = Crayon()
    accessors: dict[StyleName, Callable[[str], str]] = {
        StyleName.ORANGERED: crayon.red,
        StyleName.ORANGE: crayon.orange,
        StyleName.YELLOW: crayon.yellow,
        StyleName.CYAN: crayon.cyan,
        StyleName.GREEN: crayon.green,
        StyleName.MAGENTA: crayon.magenta,
        StyleName.UNDERLINE: crayon.underline,
    }
    for name, accessor in accessors.items():
        assert accessor("x") == EXTENDED_COLOURS[name]("x")


def test_style_is_an_open_close_pair_that_reopens_after_its_own_close() -> None:
    assert Style("<", ">")("x") == "<x>"
    assert Style("<", ">")("a>b") == "<a<b>"
    assert PLAIN("a>b") == "a>b"


TPUT_COLORS: dict[str, int] = {
    "xterm": 8,
    "xterm-256color": 256,
    "screen": 8,
    "linux": 8,
    "dumb": -1,
    "vt100": -1,
}
"""``tput colors`` per TERM (ncurses terminfo, 2026-10-01); an unknown or empty TERM prints nothing."""


def fake_terminfo(term: str) -> int | None:
    return TPUT_COLORS.get(term)


# Every row was checked against crayon 1.5.3 in R 4.6.1 on 2026-10-01: the number is crayon::num_colors()
# and colours are emitted exactly when it exceeds 1. Command (pty rows; the piped rows drop `script`):
#   env -u COLORTERM -u NO_COLOR -u R_CLI_NUM_COLORS -u FORCE_COLOR -u EMACS -u INSIDE_EMACS -u TERM VAR=... \
#     script -qec "Rscript -e 'cat(crayon::num_colors(), crayon::make_style(\"orangered\")(\"x\"))'" \
#     /dev/null | od -c
@pytest.mark.parametrize(
    ("is_tty", "environ", "colours"),
    [
        (False, {"TERM": "xterm-256color", "R_CLI_NUM_COLORS": "256"}, 256),
        (False, {"TERM": "xterm-256color", "R_CLI_NUM_COLORS": "8"}, 8),
        (False, {"TERM": "xterm", "NO_COLOR": "1", "R_CLI_NUM_COLORS": "256"}, 256),
        (False, {"TERM": "xterm-256color", "FORCE_COLOR": "1"}, 1),
        (False, {"TERM": "xterm-256color"}, 1),
        (True, {"TERM": "xterm-256color", "NO_COLOR": ""}, 1),
        (True, {"TERM": "xterm", "COLORTERM": "truecolor"}, 256**3),
        (True, {"TERM": "xterm", "COLORTERM": "foo"}, 8),
        (True, {"TERM": "xterm", "COLORTERM": ""}, 8),
        (True, {"TERM": "dumb", "INSIDE_EMACS": "28.2,comint"}, 8),
        (True, {"TERM": "dumb", "EMACS": "t"}, 1),
        (True, {"TERM": "xterm", "INSIDE_EMACS": "22.1,comint"}, 256),
        (True, {"TERM": "xterm"}, 256),
        (True, {"TERM": "xterm-256color"}, 256),
        (True, {"TERM": "screen"}, 8),
        (True, {"TERM": "dumb"}, 1),
        (True, {"TERM": "vt100"}, 1),
        (True, {"TERM": "nonsense-term"}, 1),
        (True, {"TERM": "mycolorterm"}, 8),
        (True, {}, 1),
    ],
)
def test_from_terminal_follows_crayon(is_tty: bool, environ: dict[str, str], colours: int) -> None:
    assert num_ansi_colours(is_tty=is_tty, environ=environ, terminfo=fake_terminfo) == colours
    assert Crayon.from_terminal(is_tty=is_tty, environ=environ, terminfo=fake_terminfo) == Crayon(
        enabled=colours > 1, colours=colours
    )


def test_truecolor_from_the_terminal_emits_the_256_index_codes() -> None:
    crayon = Crayon.from_terminal(is_tty=True, environ={"COLORTERM": "truecolor"}, terminfo=fake_terminfo)
    assert crayon.red("x") == f"{ESC}38;5;202mx{ESC}39m"  # R: 16777216 colours, same bytes as 256


def test_unparseable_r_cli_num_colors_fails_like_crayons_if_on_na() -> None:
    with pytest.raises(ValueError, match=MISSING_VALUE_IN_IF):
        num_ansi_colours(is_tty=True, environ={"R_CLI_NUM_COLORS": "lots"}, terminfo=fake_terminfo)
    assert (
        num_ansi_colours(is_tty=False, environ={"R_CLI_NUM_COLORS": "256.7"}, terminfo=fake_terminfo) == 256
    )


TPUT = shutil.which("tput")


@pytest.mark.skipif(TPUT is None, reason="tput (ncurses) is the oracle for terminfo_colours")
@pytest.mark.parametrize("term", ["dumb", "xterm", "xterm-256color", "nonsense-term", ""])
def test_terminfo_colours_equals_tput_colors(term: str) -> None:
    """One fresh interpreter per TERM: CPython's ``setupterm`` keeps the first terminal of a process."""
    code = "import sys; from modelstats.colors import terminfo_colours; print(terminfo_colours(sys.argv[1]))"
    ours = subprocess.run([sys.executable, "-c", code, term], capture_output=True, text=True, check=True)
    environ = {name: value for name, value in os.environ.items() if name != "TERM"}
    if term:
        environ["TERM"] = term
    assert TPUT is not None
    tput = subprocess.run([TPUT, "colors"], capture_output=True, text=True, env=environ, check=False)
    assert ours.stdout.strip() == (tput.stdout.strip() or "None")


def test_colour_legend_equals_the_looprun_golden_header() -> None:
    expected = (
        f"# Color code: {ESC}33mpending{ESC}39m/{ESC}33mstartup{ESC}39m, {ESC}36mrunning{ESC}39m, "
        f"{ESC}32mconverged{ESC}39m/{ESC}32mfinished{ESC}39m, {ESC}38;5;214mno mif{ESC}39m, "
        f"{ESC}35mconopt stalled?{ESC}39m, {ESC}38;5;202merror{ESC}39m.\n\n"
    )
    assert colour_legend(Crayon()) == expected
    assert colour_legend(Crayon.disabled()) == (
        "# Color code: pending/startup, running, converged/finished, no mif, conopt stalled?, error.\n\n"
    )


# --- the decision trees: R/loopRuns.R:94-135 --------------------------------------------------------

REMIND_DONE: dict[str, CellValue] = {
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
MAGPIE_DONE: dict[str, CellValue] = {
    "Runtime": "33.5 mins",
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
LINE = "run   2.3 hours   no   nash   Normal completion   27   26/100   converged   2: Locally Optimal   yes"


def remind(**changes: CellValue) -> dict[str, CellValue]:
    return {**REMIND_DONE, **changes}


def magpie(**changes: CellValue) -> dict[str, CellValue]:
    return {**MAGPIE_DONE, **changes}


def test_is_magpie_row_reads_iter_then_run_type() -> None:
    assert is_magpie_row(MAGPIE_DONE)
    assert is_magpie_row(remind(Iter="y1995"))
    assert is_magpie_row(remind(Iter="y2100 Clb: 3"))
    assert is_magpie_row(remind(RunType="nlp_apr17"))
    assert not is_magpie_row(REMIND_DONE)
    assert not is_magpie_row(remind(Iter="26/100", RunType="nash debug"))
    assert not is_magpie_row(remind(Iter=None, RunType=None))
    with pytest.raises(ColourRuleError, match=SUBSCRIPT_OUT_OF_BOUNDS):
        is_magpie_row({"RunType": "nash"})


# REMIND branch, lines 115-135, in the order of the if/else chain.
REMIND_CASES: list[tuple[str, dict[str, CellValue], str, StyleName | None]] = [
    ("pending", remind(Runtime="pending", jobInSLURM="pascalfu priority"), LINE, StyleName.YELLOW),
    ("startup", remind(Runtime="startup", jobInSLURM="pascalfu priority"), LINE, StyleName.YELLOW),
    (
        "conoptspy from the line",
        remind(Runtime="> 1.5 hours", jobInSLURM="pascalfu"),
        "x conoptspy >0.3h",
        StyleName.MAGENTA,
    ),
    (
        "in slurm",
        remind(Runtime="> 1.5 hours", jobInSLURM="pascalfu priority", RunStatus="Run in progress"),
        LINE,
        StyleName.CYAN,
    ),
    ("in slurm as other user", remind(jobInSLURM="otheruser"), LINE, StyleName.CYAN),
    ("converged had infes with mif", remind(Conv="converged (had INFES)", Mif="yes"), LINE, StyleName.GREEN),
    (
        "converged had infes with sumErr",
        remind(Conv="converged (had INFES)", Mif="sumErr"),
        LINE,
        StyleName.GREEN,
    ),
    (
        "not_converged",
        remind(Conv="not_converged", RunStatus="Normal completion", Mif="no"),
        LINE,
        StyleName.ORANGERED,
    ),
    ("execution error", remind(RunStatus="Execution error", Conv="NA"), LINE, StyleName.ORANGERED),
    ("compilation error", remind(RunStatus="Compilation error", Conv="NA"), LINE, StyleName.ORANGERED),
    ("interrupted", remind(RunStatus="Run interrupted", Conv="NA", Mif="no"), LINE, StyleName.ORANGERED),
    ("intermed infes", remind(RunStatus="Intermed Infes", Conv="NA"), LINE, StyleName.ORANGERED),
    ("converged with mif", REMIND_DONE, LINE, StyleName.GREEN),
    ("calibration converged with sumErr", remind(Conv="Clb_converged", Mif="sumErr"), LINE, StyleName.GREEN),
    (
        "locally optimal non-nash",
        remind(Conv="NA", Mif="no", RunType="testOneRegi EUR"),
        LINE,
        StyleName.GREEN,
    ),
    ("locally optimal nash without mif", remind(Mif="no"), LINE, StyleName.ORANGE),
    (
        "calibration converged without mif",
        remind(Conv="Clb_converged", Mif="no", RunType="Calib_nash"),
        LINE,
        StyleName.ORANGE,
    ),
    ("had infes without mif", remind(Conv="converged (had INFES)", Mif="no"), LINE, StyleName.ORANGE),
    (
        "nothing converged and not in slurm",
        remind(Conv="NA", Mif="NA", modelstat="NA", RunStatus="NA"),
        LINE,
        StyleName.ORANGERED,
    ),
    ("locally optimal nash, conv NA, mif NA", remind(Conv="NA", Mif="NA"), LINE, StyleName.ORANGERED),
    (
        "local mode: jobInSLURM NA",
        remind(jobInSLURM="NA", Conv="NA", Mif="NA", modelstat="NA"),
        LINE,
        StyleName.CYAN,
    ),
    ("local mode: converged with mif", remind(jobInSLURM="NA"), LINE, StyleName.GREEN),
    ("numeric runtime cell is pasted", remind(Runtime=8304), LINE, StyleName.GREEN),
]


@pytest.mark.parametrize(
    ("label", "status", "line", "expected"), REMIND_CASES, ids=[c[0] for c in REMIND_CASES]
)
def test_remind_tree(label: str, status: dict[str, CellValue], line: str, expected: StyleName | None) -> None:
    assert colour_for_remind(status, line) is expected
    assert colour_for_row(status, line) is expected


# MAgPIE branch, lines 95-114.
MAGPIE_LINE = (
    "default_2026-09-19_04.05.47   33.5 mins   no   nlp_apr17   Normal completion   0   y2100   NA   "
    "222222222222222222   yes"
)
MAGPIE_CASES: list[tuple[str, dict[str, CellValue], str, StyleName | None]] = [
    ("pending", magpie(Runtime="pending"), MAGPIE_LINE, StyleName.YELLOW),
    ("startup is not yellow for MAgPIE", magpie(Runtime="startup"), MAGPIE_LINE, StyleName.GREEN),
    ("not_converged", magpie(RunStatus="not_converged"), MAGPIE_LINE, StyleName.ORANGERED),
    ("execution error", magpie(RunStatus="Execution error"), MAGPIE_LINE, StyleName.ORANGERED),
    ("full.log missing", magpie(RunStatus="full.log missing"), MAGPIE_LINE, StyleName.ORANGERED),
    ("interrupted", magpie(RunStatus="Run interrupted"), MAGPIE_LINE, StyleName.ORANGERED),
    ("abort", magpie(RunStatus="Abort 3 2*Infes"), MAGPIE_LINE, StyleName.ORANGERED),
    ("converged status", magpie(RunStatus="converged", modelstat="NA"), MAGPIE_LINE, StyleName.GREEN),
    ("all 2s", MAGPIE_DONE, MAGPIE_LINE, StyleName.GREEN),
    ("2s with a dot", magpie(modelstat="2222.2222"), MAGPIE_LINE, None),
    ("locally optimal", magpie(modelstat="2: Locally Optimal"), MAGPIE_LINE, StyleName.GREEN),
    ("conoptspy from the line", magpie(modelstat="NA"), "x conoptspy >0.3h", StyleName.MAGENTA),
    ("run in progress from the line", magpie(modelstat="NA"), "x Run in progress y", StyleName.CYAN),
    ("NA and FALSE in the line", magpie(modelstat="NA"), "x NA y FALSE", StyleName.ORANGERED),
    ("NA alone in the line", magpie(modelstat="NA"), "x NA y", None),
    ("nothing matches", magpie(modelstat="NA"), MAGPIE_LINE, None),
]


@pytest.mark.parametrize(
    ("label", "status", "line", "expected"), MAGPIE_CASES, ids=[c[0] for c in MAGPIE_CASES]
)
def test_magpie_tree(label: str, status: dict[str, CellValue], line: str, expected: StyleName | None) -> None:
    assert colour_for_magpie(status, line) is expected
    assert colour_for_row(status, line) is expected


def test_colors_false_decides_the_model_first_and_then_nothing() -> None:
    assert colour_for_row(REMIND_DONE, LINE, colors=False) is None
    assert colour_for_row(MAGPIE_DONE, MAGPIE_LINE, colors=False) is None
    with pytest.raises(ColourRuleError, match=SUBSCRIPT_OUT_OF_BOUNDS):
        colour_for_row({"RunType": "nash"}, LINE, colors=False)


# NA semantics: grepl and %in% are FALSE for NA, == is NA, and if (NA) is an error.


def test_na_job_in_slurm_is_an_r_error_in_the_remind_tree() -> None:
    with pytest.raises(ColourRuleError, match=MISSING_VALUE_IN_IF):
        colour_for_remind(remind(jobInSLURM=None), LINE)


def test_na_conv_errors_only_when_mif_is_not_no() -> None:
    with pytest.raises(ColourRuleError, match=MISSING_VALUE_IN_IF):
        colour_for_remind(remind(Conv=None, Mif="yes"), LINE)
    # Conv == "..." is NA, but NA && FALSE is FALSE: the chain goes on and ends in red.
    assert colour_for_remind(remind(Conv=None, Mif="no", RunType="nash"), LINE) is StyleName.ORANGERED


def test_na_mif_errors_only_when_conv_is_converged() -> None:
    with pytest.raises(ColourRuleError, match=MISSING_VALUE_IN_IF):
        colour_for_remind(remind(Conv="converged", Mif=None), LINE)
    assert colour_for_remind(remind(Conv="NA", Mif=None, RunType="testOneRegi EUR"), LINE) is StyleName.GREEN


def test_na_modelstat_is_an_r_error_in_the_magpie_tree() -> None:
    with pytest.raises(ColourRuleError, match=MISSING_VALUE_IN_IF):
        colour_for_magpie(magpie(modelstat=None), MAGPIE_LINE)


def test_na_run_status_and_runtime_are_simply_false() -> None:
    assert colour_for_remind(remind(RunStatus=None, Runtime=None), LINE) is StyleName.GREEN
    assert colour_for_magpie(magpie(RunStatus=None, Runtime=None), MAGPIE_LINE) is StyleName.GREEN


def test_missing_fields_error_where_r_reads_them() -> None:
    status = remind()
    del status["Conv"]
    with pytest.raises(ColourRuleError, match=SUBSCRIPT_OUT_OF_BOUNDS):
        colour_for_remind(status, LINE)
    status = remind(Conv="converged (had INFES)")
    del status["Mif"]
    with pytest.raises(ColourRuleError, match=SUBSCRIPT_OUT_OF_BOUNDS):
        colour_for_remind(status, LINE)
    # Pending is decided before any other field is read.
    assert colour_for_remind({"Runtime": "pending"}, LINE) is StyleName.YELLOW
