"""Console output: the table stream (stdout), alerts and hints (stderr), colours.

R source: ``cat()``, ``crayon`` styles, ``cli::cli_alert_info``/``cli_alert_warning``,
``message()``. Rows are ``rich.text.Text`` objects, never markup, so that ``[`` in a run name is
printed verbatim; rich honours ``NO_COLOR`` and non-TTY output by itself.
"""

from __future__ import annotations

import io
import sys
from collections.abc import Callable, Sequence

from rich.console import Console
from rich.text import Text

__all__ = [
    "STYLES",
    "capture",
    "err",
    "error",
    "info",
    "run_app",
    "make_console",
    "out",
    "styled",
    "tolerate_undecodable_names",
    "warn",
]

STYLES = {
    "yellow": "yellow",
    "cyan": "cyan",
    "green": "green",
    "magenta": "magenta",
    "orangered": "orange_red1",
    "orange": "orange1",
    "underline": "underline",
}


def make_console(*, stderr: bool = False, color: bool = True) -> Console:
    """A console that never wraps or highlights; ``color=False`` is ``-b``/``--nocolor``.

    With ``color=True`` rich still decides by itself: no colour when ``NO_COLOR`` is set or the
    stream is not a terminal (use ``capture()`` for a plain-text buffer). Tabs in a row are
    expanded by rich (R's ``cat`` kept them).
    """
    return Console(stderr=stderr, highlight=False, soft_wrap=True, no_color=None if color else True)


def run_app(app: Callable[..., object], argv: Sequence[str] | None, *, prog_name: str) -> int:
    """Run a typer app as a console script would and return its exit status (``SystemExit(None)`` is 0)."""
    try:
        app(args=None if argv is None else list(argv), prog_name=prog_name)
    except SystemExit as done:
        code = done.code
        if code is None:
            return 0
        return code if isinstance(code, int) else 1
    return 0


def tolerate_undecodable_names() -> None:
    """Let stdout/stderr pass folder names holding bytes that are not UTF-8, as R's ``cat`` did.

    Called by the console scripts; Python decodes such names with surrogate escapes, which a
    strict stream refuses to encode.
    """
    for stream in (sys.stdout, sys.stderr):
        if isinstance(stream, io.TextIOWrapper):
            stream.reconfigure(errors="surrogateescape")


out = make_console()
err = make_console(stderr=True)


def capture() -> tuple[Console, io.StringIO]:
    """A console writing plain text (no colour, no underline, no wrapping) into a buffer.

    R's ``capture.output()`` of a table gave the same: crayon emits nothing outside a terminal.
    """
    buffer = io.StringIO()
    console = Console(file=buffer, force_terminal=False, no_color=True, highlight=False, soft_wrap=True)
    return console, buffer


def info(message: str | Text, *, console: Console | None = None) -> None:
    """``cli_alert_info``: a cyan information glyph and the message on stderr."""
    (console or err).print(Text.assemble(("ℹ", "cyan"), " ", message))


def error(error: object) -> None:
    """The ``Error: ...`` line of a failed command on stderr (R's ``Error in ...: ...``)."""
    err.print(Text(f"Error: {error}"))


def warn(message: str, *, console: Console | None = None) -> None:
    """``cli_alert_warning``: an exclamation mark and the message on stderr."""
    (console or err).print(Text.assemble(("!", "yellow"), " ", message))


def styled(text: str, style: str | None) -> Text:
    """A row with one of the crayon colours of ``STYLES`` (``None``: no style)."""
    return Text(text, style=STYLES.get(style, style) if style else "")
