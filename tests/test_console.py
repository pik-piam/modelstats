from __future__ import annotations

import pytest
from rich.text import Text

from modelstats import _console
from tests.conftest import CapturedConsole, capture_console


def test_no_color_environment_is_honoured(monkeypatch: pytest.MonkeyPatch) -> None:
    monkeypatch.setenv("NO_COLOR", "1")
    assert _console.make_console().no_color
    monkeypatch.delenv("NO_COLOR")
    assert not _console.make_console().no_color
    assert _console.make_console(color=False).no_color


def test_info_glyph_is_cyan() -> None:
    captured = capture_console(color=True)
    _console.info("x", console=captured.console)
    assert "\x1b[36mℹ" in captured.text


def test_info_and_warn_glyphs(err: CapturedConsole) -> None:
    _console.info("Runs found: 3")
    _console.warn("No runs found")
    assert err.lines == ["ℹ Runs found: 3", "! No runs found"]


def test_info_can_target_another_console(out: CapturedConsole) -> None:
    _console.info("hello", console=out.console)
    assert out.text == "ℹ hello\n"


def test_styled_maps_crayon_names() -> None:
    assert _console.styled("x", "orangered").style == "orange_red1"
    assert _console.styled("x", "orange").style == "orange1"
    assert _console.styled("x", "cyan").style == "cyan"
    assert _console.styled("x", None).style == ""
    assert isinstance(_console.styled("[not markup]", "green"), Text)


def test_styled_text_is_not_markup() -> None:
    captured = capture_console()
    captured.console.print(_console.styled("C_[rem]-1 [bold]", "green"))
    assert captured.text == "C_[rem]-1 [bold]\n"


def test_colour_codes_only_when_enabled() -> None:
    coloured = capture_console(color=True)
    coloured.console.print(_console.styled("row", "green"))
    assert "\x1b[" in coloured.text and "row" in coloured.text
    plain = capture_console(color=False)
    plain.console.print(_console.styled("row", "green"))
    assert plain.text == "row\n"


def test_make_console_flags() -> None:
    console = _console.make_console(color=False)
    assert console.no_color is True
    assert console.soft_wrap is True
    assert _console.make_console(stderr=True).stderr is True
    assert _console.out.stderr is False and _console.err.stderr is True


def test_capture_is_plain_text(monkeypatch: pytest.MonkeyPatch) -> None:
    monkeypatch.setenv("FORCE_COLOR", "1")
    console, buffer = _console.capture()
    console.print(_console.styled("x" * 200, "green"))
    console.print(Text.assemble(("header", "underline"), " "))
    assert buffer.getvalue() == "x" * 200 + "\n" + "header \n"


def test_info_accepts_text(err: CapturedConsole) -> None:
    _console.info(Text.assemble("Results from ", ("x", "blue")))
    assert err.lines == ["ℹ Results from x"]


def test_error_line(err: CapturedConsole) -> None:
    _console.error(ValueError("[bold] is not markup"))
    assert err.lines == ["Error: [bold] is not markup"]


def test_run_app_maps_system_exit() -> None:
    def app(*, args: list[str] | None, prog_name: str) -> None:
        if args == ["none"]:
            raise SystemExit
        if args == ["text"]:
            raise SystemExit("usage")
        if args == ["three"]:
            raise SystemExit(3)

    assert _console.run_app(app, ["none"], prog_name="x") == 0
    assert _console.run_app(app, ["text"], prog_name="x") == 1
    assert _console.run_app(app, ["three"], prog_name="x") == 3
    assert _console.run_app(app, [], prog_name="x") == 0
