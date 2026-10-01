"""Console script ``rs``: the status table of REMIND and MAgPIE runs.

Placeholder until phase 4 (``migration/06-user-requests.md`` section 7): the typer application
exists so that the wheel builds and ``rs --help`` runs, and its single command raises
``NotImplementedError``. The real command is a drop-in for ``R/commandLineInterface.R``
(``migration/03-migration-plan.md`` section 5, "CLI drop-in").
"""

import typer

PHASE = "phase 4 (rs CLI)"

app = typer.Typer(
    add_completion=False,
    rich_markup_mode=None,
    pretty_exceptions_enable=False,
    context_settings={"help_option_names": ["-h", "--help"]},
)


@app.command()
def rs() -> None:
    """Show the status of REMIND and MAgPIE runs (not ported yet)."""
    raise NotImplementedError(f"rs is ported in {PHASE} of migration/06-user-requests.md section 7")
