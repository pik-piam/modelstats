"""Bridges to R code that is not modelstats' (plan 03 section 2.3, "Bridges to R code that is not modelstats'").

``startRuns()`` sources REMIND's ``scripts/start/*.R`` and calls ``selectScenarios()`` in-process
(``R/modeltests.R`` lines 127-138); ``evaluateRuns()`` calls ``magpie4::addToDataChangelog()`` inside
``try()`` (lines 262-269). Neither is modelstats' code, so the port runs each through an ``Rscript``
subprocess with a fixed contract: the script ships as package data under ``bridge_scripts/``, takes
explicit inputs on its command line, runs with the cwd R has at that point and the environment
``autoRenvFixDeps=TRUE``, ``LC_ALL=C.utf8``, ``TZ=Europe/Berlin`` (:data:`BRIDGE_ENV`, merged over the
process environment by :meth:`Effects.run`), and answers with one JSON object as the last line of its
stdout: ``{"bridge": <name>, "ok": true, ...}`` with exit status 0, or ``{"bridge": <name>, "ok": false,
"error": <R conditionMessage>, "call": <deparsed call or null>, ...}`` with exit status 1. Every
subprocess goes through :meth:`Effects.run`, so a recording or dry-run Effects sees the exact argv, cwd
and environment; the pinned source of the R code is part of each answer (the digest of every sourced
REMIND file, the installed magpie4 version).

Failure semantics follow the R code each bridge replaces:

- :func:`select_scenarios`: an R error (an unreadable ``config/scenario_config.csv``, a failing
  ``selectScenarios()``) raises :class:`RParityError` with R's ``conditionMessage`` and call, as the
  in-process error aborts ``startRuns()``; nothing is written to the RDS path then (no partial state).
- :func:`add_to_data_changelog`: R wraps the call in ``try()``, so a failure (a corrupt or missing
  ``report.rds``: the amt cases ``magpie-evaluate-bridge-fail`` and ``magpie-evaluate-report-missing``)
  comes back as :class:`ChangelogResult` with ``ok=False`` and never raises; the script's own ``try()``
  prints R's ``Error in <call> : <message>`` and the ``In addition: Warning message:`` block on stderr,
  which is passed through, as the in-process ``try()`` prints it. The deparsed call in that line is
  ``readRDS(report)`` here against ``readRDS(file.path(i, "report.rds"))`` in R, because the report path
  is an explicit input of the bridge; stderr is not among the compared AMT artefacts (plan 4.5).
- A bridge that cannot run at all (``Rscript`` not found, status 127, or a script that ended without its
  JSON line) raises :class:`BridgeError`. R has no counterpart (the code ran in-process); the AMT run
  stops there like any other uncaught error.

The subprocess' stdout before the JSON line (whatever the sourced REMIND code prints; nothing in the
fake checkout) is passed through to ``sys.stdout`` and its stderr to ``sys.stderr``, both resolved at
call time so that captures and redirects work.

Verified against the R oracle (R 4.6.1, magpie4 2.83.0, the fake checkout of ``build_fake_remind.sh``,
see ``tests/unit/test_amt_bridges.py``): the RDS written by ``select_scenarios.R --suffix -AMT`` has the
sha256 of the ``remind-start`` golden's ``runsToStart.rds`` and is ``identical()`` to R's literal
``startRuns()`` lines; the changelog written by ``add_to_data_changelog.R`` equals the
``data-changelog.csv`` goldens of ``magpie-evaluate-recent``, ``magpie-evaluate-changelog-missing``,
``magpie-evaluate-bridge-fail`` and ``magpie-evaluate-report-missing`` byte for byte.
"""

from __future__ import annotations

import contextlib
import dataclasses
import json
import os
import subprocess
import sys
from collections.abc import Iterator, Mapping, Sequence
from importlib.resources import as_file, files
from pathlib import Path
from types import MappingProxyType
from typing import Any

from modelstats.env import Effects, PathLike, default_effects
from modelstats.errors import RParityError

__all__ = [
    "ADD_TO_DATA_CHANGELOG_SCRIPT",
    "BRIDGE_ENV",
    "DEFAULT_STARTGROUP",
    "RSCRIPT",
    "SCENARIO_CONFIG",
    "SELECT_SCENARIOS_SCRIPT",
    "START_SCRIPTS_DIR",
    "BridgeError",
    "BridgeSource",
    "ChangelogResult",
    "SelectScenariosResult",
    "add_to_data_changelog",
    "bridge_scripts_dir",
    "select_scenarios",
]

#: The environment of every bridge subprocess (plan 03 section 2.3): ``Sys.setenv(autoRenvFixDeps = "TRUE")``
#: of ``R/modeltests.R`` line 109 and the sandbox's locale and time zone, merged over the process environment.
BRIDGE_ENV: Mapping[str, str] = MappingProxyType({"autoRenvFixDeps": "TRUE", "LC_ALL": "C.utf8", "TZ": "Europe/Berlin"})
#: The interpreter, resolved through ``PATH`` by :meth:`Effects.run` (the sandbox's fake ``Rscript`` passes every
#: command that is not ``start.R`` through to the real one).
RSCRIPT = "Rscript"
SELECT_SCENARIOS_SCRIPT = "select_scenarios.R"
ADD_TO_DATA_CHANGELOG_SCRIPT = "add_to_data_changelog.R"
#: ``read.csv2("config/scenario_config.csv", ...)`` of ``R/modeltests.R`` line 127, relative to mydir.
SCENARIO_CONFIG = "config/scenario_config.csv"
#: ``list.files("scripts/start", pattern = "\\.R$", full.names = TRUE)`` of line 134, relative to mydir.
START_SCRIPTS_DIR = "scripts/start"
#: ``selectScenarios(..., startgroup = "AMT")`` of line 136.
DEFAULT_STARTGROUP = "AMT"


class BridgeError(Exception):
    """A bridge subprocess that could not deliver its answer (no R error of the bridged code).

    Raised when ``Rscript`` cannot be run (status 127), when the script ends without its JSON line
    (a crash before the protocol line) or when the JSON line and the exit status contradict each
    other. ``argv``, ``returncode``, ``stdout`` and ``stderr`` carry what happened.
    """

    def __init__(self, reason: str, *, argv: Sequence[str], returncode: int, stdout: str, stderr: str) -> None:
        super().__init__(f"bridge {os.path.basename(argv[1]) if len(argv) > 1 else argv[0]}: {reason}")
        self.reason = reason
        self.argv = list(argv)
        self.returncode = returncode
        self.stdout = stdout
        self.stderr = stderr


@dataclasses.dataclass(frozen=True)
class BridgeSource:
    """One R file the ``select_scenarios`` bridge sourced: its path (as listed) and digest, the pinned source.

    ``algorithm`` is ``sha256`` (``tools::sha256sum``, R 4.5 and later) or ``md5`` (``tools::md5sum``
    on an older R).
    """

    file: str
    digest: str
    algorithm: str


@dataclasses.dataclass(frozen=True)
class SelectScenariosResult:
    """The answer of :func:`select_scenarios`.

    ``row_names`` are ``rownames(runsToStart)`` (with ``row_name_suffix`` applied when one was given),
    ``columns`` the data.frame's columns, ``rds_path`` where the script saved the data.frame with
    ``saveRDS()``, ``sources`` the sourced ``scripts/start/*.R`` files with their digests, ``argv`` the
    exact command that ran.
    """

    row_names: list[str]
    columns: list[str]
    nrow: int
    rds_path: str
    sources: list[BridgeSource]
    r_version: str
    argv: list[str]


@dataclasses.dataclass(frozen=True)
class ChangelogResult:
    """The answer of :func:`add_to_data_changelog`; ``ok=False`` is R's ``try()`` having caught an error.

    ``error`` and ``call`` are R's ``conditionMessage`` and deparsed call then (``unknown input format``
    / ``readRDS(report)`` for a corrupt report, ``cannot open the connection`` / ``gzfile(file, "rb")``
    for a missing one); ``nrow`` is the number of changelog rows written on success.
    """

    ok: bool
    changelog: str
    version_id: str
    error: str | None
    call: str | None
    nrow: int | None
    magpie4_version: str | None
    r_version: str | None
    argv: list[str]


@contextlib.contextmanager
def bridge_scripts_dir() -> Iterator[Path]:
    """The directory holding the R bridge scripts (package data ``modelstats/amt/bridge_scripts``) as a path.

    The scripts ``source()`` their sibling ``_json.R``, so the whole directory is materialised (a no-op
    for the installed wheel or an editable install, where it already is a directory on disk).
    """
    with as_file(files("modelstats.amt").joinpath("bridge_scripts")) as directory:
        yield Path(directory)


def _effects(effects: Effects | None) -> Effects:
    return default_effects() if effects is None else effects


def _run_bridge(
    script: str,
    args: Sequence[str],
    *,
    cwd: PathLike | None,
    effects: Effects,
    rscript: str,
) -> tuple[dict[str, Any], subprocess.CompletedProcess[str]]:
    """Run one bridge script and return its JSON answer (the last non-empty stdout line) and the process.

    The stdout before the JSON line goes to ``sys.stdout``, the stderr to ``sys.stderr``.
    """
    with bridge_scripts_dir() as directory:
        argv = [rscript, str(directory / script), *args]
        proc = effects.run(argv, cwd=cwd, env=dict(BRIDGE_ENV))
    stdout = proc.stdout or ""
    stderr = proc.stderr or ""
    body = stdout.rstrip("\n")
    head, _sep, last = body.rpartition("\n")
    payload: dict[str, Any] | None = None
    if last.strip():
        try:
            parsed = json.loads(last)
        except ValueError:
            parsed = None
        if isinstance(parsed, dict) and "bridge" in parsed:
            payload = parsed
    passthrough = stdout if payload is None else (head + "\n" if head else "")
    if passthrough:
        sys.stdout.write(passthrough)
        sys.stdout.flush()
    if stderr:
        sys.stderr.write(stderr)
        sys.stderr.flush()
    if proc.returncode == 127:
        raise BridgeError(
            f"{rscript} could not be run (status 127)", argv=argv, returncode=127, stdout=stdout, stderr=stderr
        )
    if payload is None:
        raise BridgeError(
            f"no JSON answer on stdout (exit status {proc.returncode})",
            argv=argv,
            returncode=proc.returncode,
            stdout=stdout,
            stderr=stderr,
        )
    ok = payload.get("ok")
    if ok is True and proc.returncode != 0 or ok is False and proc.returncode == 0 or ok not in (True, False):
        raise BridgeError(
            f"answer ok={ok!r} contradicts exit status {proc.returncode}",
            argv=argv,
            returncode=proc.returncode,
            stdout=stdout,
            stderr=stderr,
        )
    payload["_argv"] = argv
    return payload, proc


def _opt_str(payload: Mapping[str, Any], key: str) -> str | None:
    value = payload.get(key)
    return None if value is None else str(value)


def select_scenarios(
    mydir: PathLike,
    rds_out: PathLike,
    *,
    startgroup: str = DEFAULT_STARTGROUP,
    config: str = SCENARIO_CONFIG,
    scripts_dir: str = START_SCRIPTS_DIR,
    row_name_suffix: str = "",
    effects: Effects | None = None,
    rscript: str = RSCRIPT,
) -> SelectScenariosResult:
    """``R/modeltests.R`` lines 127-138 of ``startRuns()`` through ``bridge_scripts/select_scenarios.R``.

    Runs ``Rscript select_scenarios.R --out <rds_out> --config <config> --startgroup <startgroup>
    --scripts <scripts_dir> [--suffix <row_name_suffix>]`` with ``cwd=mydir`` and :data:`BRIDGE_ENV`. The
    script reads the settings with R's exact ``read.csv2()`` call, sources ``<scripts_dir>/*.R``, calls
    ``selectScenarios(settings = settings, interactive = FALSE, startgroup = startgroup)``, appends
    ``row_name_suffix`` to the row names when it is not empty (line 137's ``paste0(row.names(...), "-AMT")``,
    so that the saved file is byte-compatible with what ``startRuns()`` saves) and writes the data.frame
    with ``saveRDS()`` to ``rds_out`` (relative paths resolve against ``mydir``; written to a temporary
    file next to it and renamed). The returned row names are what ``evaluateRuns()`` later reads as
    ``rownames(readRDS("runsToStart.rds"))``.

    An R error raises :class:`RParityError` with R's ``conditionMessage`` (``cannot open the connection``
    for a missing settings file, with the deparsed call); a bridge that cannot run raises :class:`BridgeError`.
    """
    args = ["--out", os.fspath(rds_out), "--config", config, "--startgroup", startgroup, "--scripts", scripts_dir]
    if row_name_suffix:
        args += ["--suffix", row_name_suffix]
    payload, _proc = _run_bridge(SELECT_SCENARIOS_SCRIPT, args, cwd=mydir, effects=_effects(effects), rscript=rscript)
    if payload["ok"] is not True:
        raise RParityError(str(payload.get("error", "")), call=_opt_str(payload, "call"))
    sources = [
        BridgeSource(
            file=str(item.get("file", "")), digest=str(item.get("digest", "")), algorithm=str(item.get("algorithm", ""))
        )
        for item in payload.get("sources", [])
        if isinstance(item, Mapping)
    ]
    return SelectScenariosResult(
        row_names=[str(name) for name in payload.get("row_names", [])],
        columns=[str(name) for name in payload.get("columns", [])],
        nrow=int(payload.get("nrow", 0)),
        rds_path=str(payload.get("out", os.fspath(rds_out))),
        sources=sources,
        r_version=str(payload.get("r_version", "")),
        argv=list(payload["_argv"]),
    )


def add_to_data_changelog(
    report: PathLike,
    changelog: PathLike,
    version_id: str,
    *,
    cwd: PathLike | None = None,
    effects: Effects | None = None,
    rscript: str = RSCRIPT,
) -> ChangelogResult:
    """``R/modeltests.R`` lines 264-268 of ``evaluateRuns()`` through ``bridge_scripts/add_to_data_changelog.R``.

    Runs ``Rscript add_to_data_changelog.R --report <report> --changelog <changelog> --version-id
    <version_id>`` with ``cwd`` (``None``: the process' working directory, which ``evaluateRuns()`` has
    set to the output directory; ``report`` may then be relative, like R's ``file.path(i, "report.rds")``)
    and :data:`BRIDGE_ENV`. The script evaluates ``try(magpie4::addToDataChangelog(report = readRDS(report),
    changelog = changelog, versionId = version_id))``: on success the changelog file holds the new row
    (``write.csv`` by magpie4, a missing changelog is created with the new row alone, an existing one is
    merged and cut to magpie4's ``maxEntries``), on an R error the file is left as it was and the result
    says ``ok=False`` with R's message; nothing is raised in that case, like R's ``try()``. A bridge that
    cannot run raises :class:`BridgeError`.
    """
    args = ["--report", os.fspath(report), "--changelog", os.fspath(changelog), "--version-id", version_id]
    payload, _proc = _run_bridge(
        ADD_TO_DATA_CHANGELOG_SCRIPT, args, cwd=cwd, effects=_effects(effects), rscript=rscript
    )
    ok = payload["ok"] is True
    nrow = payload.get("nrow")
    return ChangelogResult(
        ok=ok,
        changelog=str(payload.get("changelog", os.fspath(changelog))),
        version_id=str(payload.get("version_id", version_id)),
        error=None if ok else str(payload.get("error", "")),
        call=None if ok else _opt_str(payload, "call"),
        nrow=int(nrow) if ok and nrow is not None else None,
        magpie4_version=_opt_str(payload, "magpie4_version"),
        r_version=_opt_str(payload, "r_version"),
        argv=list(payload["_argv"]),
    )
