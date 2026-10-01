"""The hand-rolled ANSI table reproduces crayon's bytes.

Converted from the colour part of ``migration/harness/smoke/py_misc.py``. crayon closes colours with
``ESC[39m`` and underline with ``ESC[24m`` and, when nesting, swaps a nested style's own closing
sequence for the enclosing style's opening one (``crayon:::crayon_template``); ``click.style`` and
``rich`` close with ``ESC[0m`` and cannot reproduce the goldens, so the port carries this seven-entry table
(``migration/02-python-libraries.md`` section 4.5). The R side of the contract is pinned by
``migration/harness/checks/crayon_codes.R``; the literals below are the same bytes.
"""

from __future__ import annotations

import pytest

CRAYON_256: dict[str, tuple[str, str]] = {
    "orangered": ("\x1b[38;5;202m", "\x1b[39m"),
    "orange": ("\x1b[38;5;214m", "\x1b[39m"),
    "yellow": ("\x1b[33m", "\x1b[39m"),
    "cyan": ("\x1b[36m", "\x1b[39m"),
    "green": ("\x1b[32m", "\x1b[39m"),
    "magenta": ("\x1b[35m", "\x1b[39m"),
    "underline": ("\x1b[4m", "\x1b[24m"),
}
# At 16 colours the two named colours fall back to the basic red and yellow codes.
CRAYON_16: dict[str, tuple[str, str]] = {
    **CRAYON_256,
    "orangered": ("\x1b[31m", "\x1b[39m"),
    "orange": ("\x1b[33m", "\x1b[39m"),
}


def style(text: str, name: str, table: dict[str, tuple[str, str]] = CRAYON_256) -> str:
    opening, closing = table[name]
    return f"{opening}{text}{closing}"


@pytest.mark.parametrize(
    ("name", "expected"),
    [
        ("orangered", "\x1b[38;5;202mx\x1b[39m"),
        ("orange", "\x1b[38;5;214mx\x1b[39m"),
        ("yellow", "\x1b[33mx\x1b[39m"),
        ("cyan", "\x1b[36mx\x1b[39m"),
        ("green", "\x1b[32mx\x1b[39m"),
        ("magenta", "\x1b[35mx\x1b[39m"),
        ("underline", "\x1b[4mx\x1b[24m"),
    ],
)
def test_256_colour_sequences_match_crayon(name: str, expected: str) -> None:
    assert style("x", name) == expected


@pytest.mark.parametrize(
    ("name", "expected"), [("orangered", "\x1b[31mx\x1b[39m"), ("orange", "\x1b[33mx\x1b[39m")]
)
def test_16_colour_fallback_of_the_named_colours(name: str, expected: str) -> None:
    assert style("x", name, CRAYON_16) == expected


def test_nesting_is_literal_like_crayon() -> None:
    # crayon: yellow(paste0("a", underline("b"), "c"))
    assert style("a" + style("b", "underline") + "c", "yellow") == "\x1b[33ma\x1b[4mb\x1b[24mc\x1b[39m"


def test_every_colour_closes_with_39_and_underline_with_24() -> None:
    for name, (_, closing) in CRAYON_256.items():
        assert closing == ("\x1b[24m" if name == "underline" else "\x1b[39m")
