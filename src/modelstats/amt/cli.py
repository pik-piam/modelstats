"""Console script ``modeltests``: the REMIND automated model tests.

Placeholder until phase 5 (``migration/06-user-requests.md`` section 7): the typer application
exists so that the wheel builds and ``modeltests --help`` runs, and its single command raises
``NotImplementedError``. The commands of section 3.1 (``start``, ``evaluate``, ``run``, ``status``,
``reset``) replace it in phase 5.
"""

import typer

PHASE = "phase 5 (AMT rewrite)"

app = typer.Typer(
    add_completion=False,
    rich_markup_mode=None,
    pretty_exceptions_enable=False,
    context_settings={"help_option_names": ["-h", "--help"]},
)


@app.command()
def modeltests() -> None:
    """Start and evaluate the REMIND automated model tests (not ported yet)."""
    raise NotImplementedError(f"modeltests is ported in {PHASE} of migration/06-user-requests.md section 7")
