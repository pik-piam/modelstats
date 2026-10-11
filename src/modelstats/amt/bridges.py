"""The ``selectScenarios()`` bridge: REMIND's own R code, called through one ``Rscript`` contract.

``R/modeltests.R:127-137`` (modelstats 0.31.0) builds the list of runs the AMT cycle should have started:
it reads ``config/scenario_config.csv`` with ``read.csv2()``, sources every ``scripts/start/*.R`` of the
REMIND checkout so that REMIND's ``selectScenarios()`` is defined, calls it with ``interactive = FALSE``
and ``startgroup = "AMT"`` and pastes ``-AMT`` onto the row names. That function belongs to REMIND, not to
modelstats, so the port does not re-implement it (``migration/03-migration-plan.md`` section 2.3,
"Bridges to R code that is not modelstats'"): :func:`select_scenarios` runs
``bridge_scripts/select_scenarios.R`` through ``Effects.run`` with a fixed contract and parses its JSON.
The changelog bridge of the same section went with the MAgPIE AMT (decision D-27).

The contract:

* program ``Rscript``, script :data:`BRIDGE_SCRIPT` (shipped inside this package), one argument: the
  start group;
* working directory: the REMIND checkout (the R ``mydir``), so that the script's relative paths are the
  R ones; a checkout that is not a directory fails before anything runs;
* environment: :data:`BRIDGE_ENVIRONMENT` on top of the inherited variables (``autoRenvFixDeps=TRUE`` as
  ``R/modeltests.R:109`` sets it, ``LC_ALL=C.utf8``, ``TZ=Europe/Berlin``);
* pinned source: the checkout's commit (``git log -1``, parsed as ``R/modeltests.R:198`` does) is written
  to the cycle log before the script runs;
* output: one JSON object on the last non-blank line of stdout, ``{"schema": 1, "startgroup": ...,
  "names": [...], "columns": {<column>: [<cells>]}}``; the ``-AMT`` suffix is added here, on the Python
  side;
* failure: a non-zero exit status, or output that is not the expected object, raises
  :class:`BridgeError`; the bridge writes nothing itself, so a failed call leaves no partial state (the
  caller's state transition happens after the bridge returned).

Every line the two programs print goes to the cycle log as it arrives, with the ``[<tool>]`` prefix of
``migration/06-user-requests.md`` section 3.3 (``modelstats.amt.logs.line_sink``); the payload line
itself is left out of the log (it can hold every column of the scenario table) and is summarised instead.
"""

from __future__ import annotations

import json
import logging
import os
from collections.abc import Mapping
from contextlib import AbstractContextManager
from dataclasses import dataclass
from importlib.resources import as_file, files
from pathlib import Path

from modelstats.amt.logs import LOGGER_NAME, line_sink
from modelstats.env import SELECT_SCENARIOS_SCRIPT, Effects, LineCallback, PathLike, RunResult, Stream
from modelstats.errors import ModelstatsError

__all__ = [
    "BRIDGE_ENVIRONMENT",
    "BRIDGE_SCRIPT",
    "PAYLOAD_PREFIX",
    "PAYLOAD_SCHEMA",
    "START_GROUP",
    "TITLE_TAG",
    "BridgeError",
    "Cell",
    "ScenarioSelection",
    "bridge_script",
    "checkout_commit",
    "is_payload_line",
    "parse_payload",
    "payload_line",
    "select_scenarios",
    "without_payload",
]

BRIDGE_SCRIPT = SELECT_SCENARIOS_SCRIPT
"""The script under ``bridge_scripts/`` that implements the R side of the contract (the name is shared with
``modelstats.env`` so that ``DryRunEffects`` recognises the call and answers an empty selection instead
of running REMIND's ``scripts/start/*.R``)."""

START_GROUP = "AMT"
"""``startgroup = "AMT"`` of ``R/modeltests.R:136``."""

TITLE_TAG = "AMT"
"""``titletag=AMT`` of the ``Rscript start.R`` calls (``R/modeltests.R:111, 120``): the run titles end in
``-AMT``, which ``R/modeltests.R:137`` pastes onto the selected row names."""

BRIDGE_ENVIRONMENT: Mapping[str, str] = {
    "autoRenvFixDeps": "TRUE",
    "LC_ALL": "C.utf8",
    "TZ": "Europe/Berlin",
}
"""Variables set for the script on top of the inherited environment (``03`` section 2.3)."""

PAYLOAD_SCHEMA = 1
"""The ``schema`` value the script prints; a mismatch is a packaging error, not a REMIND change."""

PAYLOAD_PREFIX = '{"schema": '
"""How the script starts its payload line (``select_scenarios.R``, the final ``cat``): the log filter
recognises the payload by it, the parser does not depend on it."""

RSCRIPT = "Rscript"
GIT = "git"
COMMIT_PREFIX = "commit "

type Cell = str | int | float | bool | None
"""One cell of the selected table as JSON carries it: R character, integer or double, logical, or NA."""


class BridgeError(ModelstatsError):
    """The ``selectScenarios()`` bridge failed: the script exited non-zero or printed no usable payload.

    ``result`` is the ``Rscript`` call (argv, exit status, both streams) when the script ran at all.
    """

    def __init__(self, message: str, *, result: RunResult | None = None) -> None:
        """``message`` says what failed; ``result`` carries the subprocess outcome when there is one."""
        super().__init__(message)
        self.result = result


@dataclass(frozen=True)
class ScenarioSelection:
    """What ``selectScenarios()`` returned, in the row order R returned it.

    ``titles`` are the row names as REMIND returned them (the ``title`` column of the CSV), ``names`` the
    same with the ``-<title tag>`` suffix (``R/modeltests.R:137``): the names of the runs the cycle should
    start, as :mod:`modelstats.amt.state` records them. ``columns`` are the remaining CSV columns of the
    selected rows, keyed by column name in CSV order; every column has one cell per row. ``commit`` is the
    checkout's HEAD when ``git`` could report it.
    """

    startgroup: str
    titles: tuple[str, ...]
    names: tuple[str, ...]
    columns: Mapping[str, tuple[Cell, ...]]
    commit: str | None

    def __len__(self) -> int:
        """The number of selected scenarios."""
        return len(self.names)

    def row(self, name: str) -> Mapping[str, Cell]:
        """The cells of one selected scenario by its suffixed ``name`` (or its bare title)."""
        try:
            index = self.names.index(name)
        except ValueError:
            index = self.titles.index(name)
        return {column: cells[index] for column, cells in self.columns.items()}


# ---------------------------------------------------------------------------------------------
# The bridge
# ---------------------------------------------------------------------------------------------


def select_scenarios(
    effects: Effects,
    checkout: PathLike,
    *,
    startgroup: str = START_GROUP,
    title_tag: str = TITLE_TAG,
    logger: logging.Logger | None = None,
) -> ScenarioSelection:
    """Run ``selectScenarios(settings, interactive = FALSE, startgroup)`` inside the REMIND ``checkout``.

    ``R/modeltests.R:127-137``: the CSV is read by the script with the exact ``read.csv2()`` call, the
    function comes from ``scripts/start/*.R`` of the checkout, and the ``-<title_tag>`` suffix is applied
    here. The checkout's commit is logged first, every line the script prints while it runs; a failure
    raises :class:`BridgeError` after those lines, with nothing written anywhere.
    """
    log = logger if logger is not None else logging.getLogger(LOGGER_NAME)
    checkout_dir = Path(os.fspath(checkout))
    if not effects.is_dir(checkout_dir):
        raise BridgeError(f"REMIND checkout {checkout_dir} is not a directory")
    commit = checkout_commit(effects, checkout_dir, logger=log)
    log.info(
        "selectScenarios bridge: %s %s %s in %s (REMIND commit %s)",
        RSCRIPT,
        BRIDGE_SCRIPT,
        startgroup,
        checkout_dir,
        commit if commit is not None else "unknown",
    )
    with bridge_script() as script:
        result = effects.run(
            [RSCRIPT, str(script), startgroup],
            cwd=checkout_dir,
            env=BRIDGE_ENVIRONMENT,
            on_line=without_payload(line_sink(log, RSCRIPT)),
        )
    if not result.ok:
        message = f"{RSCRIPT} {BRIDGE_SCRIPT} exited with status {result.returncode} in {checkout_dir}"
        log.error("selectScenarios bridge failed: %s", message)
        raise BridgeError(message, result=result)
    try:
        selection = parse_payload(payload_line(result), title_tag=title_tag, commit=commit)
    except BridgeError as error:
        log.error("selectScenarios bridge failed: %s", error)
        raise BridgeError(str(error), result=result) from None
    log.info(
        'selectScenarios(startgroup = "%s") selected %d scenario%s: %s',
        selection.startgroup,
        len(selection),
        "" if len(selection) == 1 else "s",
        ", ".join(selection.names) if selection.names else "(none)",
    )
    return selection


def bridge_script() -> AbstractContextManager[Path]:
    """A context manager yielding the filesystem path of :data:`BRIDGE_SCRIPT` inside this package.

    ``importlib.resources.as_file`` hands the real file for a directory install and a temporary copy for
    a zipped one; the path is valid inside the ``with`` block only.
    """
    return as_file(files("modelstats.amt") / "bridge_scripts" / BRIDGE_SCRIPT)


def checkout_commit(
    effects: Effects, checkout: PathLike, *, logger: logging.Logger | None = None
) -> str | None:
    """The checkout's HEAD as ``R/modeltests.R:198`` reads it: ``git log -1``, first line minus ``commit``.

    The lines ``git`` prints go to the log with the ``[git]`` prefix. ``None`` when ``git`` fails or
    prints nothing usable; that is logged as a warning and does not stop the caller (the commit is a
    record for the log, not an input of the step).
    """
    log = logger if logger is not None else logging.getLogger(LOGGER_NAME)
    result = effects.run([GIT, "log", "-1"], cwd=checkout, on_line=line_sink(log, GIT))
    lines = result.lines()
    first = lines[0] if lines else ""
    if result.ok and first.startswith(COMMIT_PREFIX):
        return first.removeprefix(COMMIT_PREFIX).strip()
    log.warning(
        "could not determine the REMIND commit of %s: git log -1 exited with status %d%s",
        os.fspath(checkout),
        result.returncode,
        f", first line {first!r}" if first else ", no output",
    )
    return None


def without_payload(sink: LineCallback) -> LineCallback:
    """``sink`` for every line except the payload line on stdout (:func:`is_payload_line`)."""

    def filtered(stream: Stream, line: str) -> None:
        if stream is Stream.STDOUT and is_payload_line(line):
            return
        sink(stream, line)

    return filtered


def is_payload_line(line: str) -> bool:
    """Whether a stdout line is the script's payload: it starts with :data:`PAYLOAD_PREFIX`."""
    return line.lstrip().startswith(PAYLOAD_PREFIX)


def payload_line(result: RunResult) -> str:
    """The last non-blank stdout line of the script (``""`` when it printed none)."""
    for line in reversed(result.lines()):
        if line.strip():
            return line
    return ""


# ---------------------------------------------------------------------------------------------
# The payload
# ---------------------------------------------------------------------------------------------


def parse_payload(line: str, *, title_tag: str = TITLE_TAG, commit: str | None = None) -> ScenarioSelection:
    """Turn the script's JSON line into a :class:`ScenarioSelection`, suffixing the names.

    Raises :class:`BridgeError` for anything that is not the documented object: no JSON at all, another
    ``schema``, missing or mistyped members, or a column whose length differs from the number of rows.
    """
    if not line.strip():
        raise BridgeError(f"{BRIDGE_SCRIPT} printed no JSON payload on stdout")
    try:
        payload = json.loads(line)
    except json.JSONDecodeError as error:
        raise BridgeError(
            f"{BRIDGE_SCRIPT} printed no JSON object on its last stdout line: {error}"
        ) from None
    if not isinstance(payload, dict):
        raise BridgeError(f"{BRIDGE_SCRIPT} printed a JSON {type(payload).__name__}, not an object")
    schema = payload.get("schema")
    if schema != PAYLOAD_SCHEMA:
        raise BridgeError(f"{BRIDGE_SCRIPT} payload schema {schema!r} is not {PAYLOAD_SCHEMA}")
    startgroup = payload.get("startgroup")
    if not isinstance(startgroup, str):
        raise BridgeError(f"{BRIDGE_SCRIPT} payload member 'startgroup' is not a string")
    titles = _string_list(payload.get("names"), "names")
    columns = _columns(payload.get("columns"), len(titles))
    return ScenarioSelection(
        startgroup=startgroup,
        titles=titles,
        names=tuple(f"{title}-{title_tag}" for title in titles),
        columns=columns,
        commit=commit,
    )


def _string_list(value: object, member: str) -> tuple[str, ...]:
    if not isinstance(value, list) or not all(isinstance(item, str) for item in value):
        raise BridgeError(f"{BRIDGE_SCRIPT} payload member {member!r} is not a list of strings")
    return tuple(str(item) for item in value)


def _columns(value: object, rows: int) -> Mapping[str, tuple[Cell, ...]]:
    if not isinstance(value, dict):
        raise BridgeError(f"{BRIDGE_SCRIPT} payload member 'columns' is not an object")
    columns: dict[str, tuple[Cell, ...]] = {}
    for name, cells in value.items():
        if not isinstance(name, str) or not isinstance(cells, list):
            raise BridgeError(f"{BRIDGE_SCRIPT} payload column {name!r} is not a list")
        if len(cells) != rows:
            raise BridgeError(
                f"{BRIDGE_SCRIPT} payload column {name!r} has {len(cells)} cells for {rows} rows"
            )
        columns[name] = tuple(_cell(item, name) for item in cells)
    return columns


def _cell(item: object, column: str) -> Cell:
    if item is None or isinstance(item, str | bool | int | float):
        return item
    raise BridgeError(f"{BRIDGE_SCRIPT} payload column {column!r} holds a {type(item).__name__}")
