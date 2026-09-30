"""Console script ``rs`` (placeholder until the CLI is ported in phase 4)."""

import sys

import typer

app = typer.Typer(
    add_completion=False,
    rich_markup_mode=None,
    context_settings={"help_option_names": ["-h", "--help"]},
)

NOT_IMPLEMENTED = "not implemented yet (phase 4/5)"


@app.command()
def rs() -> None:
    """Run statistics for REMIND and MAgPIE runs (Python port, in progress)."""
    print(NOT_IMPLEMENTED, file=sys.stderr)
    raise typer.Exit(code=2)


def main() -> None:
    """Entry point of the ``rs`` console script."""
    app()
