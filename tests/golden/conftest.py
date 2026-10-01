"""Golden tier (``-m golden``): the Python port, run through the sandbox driver, against the R goldens.

``py_goldens`` is a session-scoped factory: ``py_goldens(family)`` runs
``migration/harness/make_goldens.sh --candidate python`` once per family into
``migration/_scratch/py-goldens`` (or uses the pre-generated tree named by the
environment variable ``MODELSTATS_PY_GOLDENS``, which the gate sets to avoid
regenerating) and returns the family's directory. ``r_golden_ids(family)``
enumerates the R golden ids at collection time so that every R case is a test
and a missing Python output fails.

The whole tier is skipped by ``tests/conftest.py`` when the fixture tree is
absent. Nothing here skips: a case that cannot be compared fails.
"""

from __future__ import annotations

import os
import shutil
import subprocess
from collections.abc import Callable
from pathlib import Path

import pytest

REPO = Path(__file__).resolve().parents[2]
MIGRATION = REPO / "migration"
GOLDENS = MIGRATION / "goldens"
EXPECTED_PY = GOLDENS / "expected-py"
DRIVER = MIGRATION / "harness" / "make_goldens.sh"
PY_GOLDENS_DIR = MIGRATION / "_scratch" / "py-goldens"
PREGENERATED_ENV = "MODELSTATS_PY_GOLDENS"
# one family through the sandbox: the biggest (rs, status) take minutes, not an hour
DRIVER_TIMEOUT_SECONDS = 3600

# The file suffixes that identify one case of a family (``<id><suffix>``); the other
# files of a case (``rs``: .argv .out .err) are looked up by the comparator.
_CASE_SUFFIXES: dict[str, tuple[str, ...]] = {
    "status": (".json",),
    "runtype": (".json",),
    "slurm": (".json",),
    "sort": (".json",),
    "printoutput": (".out", ".error.json"),
    "looprun": (".out", ".error.json"),
    "sanity": (".out", ".error.json"),
    "rs": (".status",),
}


def r_golden_ids(family: str) -> list[str]:
    """The case ids of a family's R goldens, relative to the family directory.

    ``status`` ids carry their subdirectory (``oncluster/detailed/<id>``), ``amt``
    ids are the case directories; an absent family directory gives ``[]``.
    """
    root = GOLDENS / family
    if not root.is_dir():
        return []
    if family == "amt":
        return sorted(p.parent.relative_to(root).as_posix() for p in root.rglob("result.json"))
    suffixes = _CASE_SUFFIXES[family]
    ids: set[str] = set()
    for path in root.rglob("*"):
        if not path.is_file():
            continue
        rel = path.relative_to(root).as_posix()
        for suffix in suffixes:
            if rel.endswith(suffix):
                ids.add(rel[: -len(suffix)])
                break
    return sorted(ids)


def _tail(log: Path, lines: int = 30) -> str:
    try:
        text = log.read_text(encoding="utf-8", errors="replace")
    except OSError as exc:
        return f"<{log}: {exc}>"
    return "\n".join(text.splitlines()[-lines:])


def _generate(family: str) -> Path:
    """Run the driver for one family into ``PY_GOLDENS_DIR`` and return the family directory."""
    if not DRIVER.is_file():
        pytest.fail(f"golden driver {DRIVER} is missing")
    PY_GOLDENS_DIR.mkdir(parents=True, exist_ok=True)
    family_dir = PY_GOLDENS_DIR / family
    # stale output of an earlier run must never pass for a case the generator failed to write this time
    shutil.rmtree(family_dir, ignore_errors=True)
    log = PY_GOLDENS_DIR / f"{family}.driver.log"
    cmd = [str(DRIVER), "--candidate", "python", "--out", str(PY_GOLDENS_DIR), family]
    with log.open("wb") as handle:
        try:
            proc = subprocess.run(
                cmd,
                cwd=REPO,
                stdout=handle,
                stderr=subprocess.STDOUT,
                timeout=DRIVER_TIMEOUT_SECONDS,
                check=False,
            )
        except subprocess.TimeoutExpired:
            pytest.fail(f"{' '.join(cmd)} exceeded {DRIVER_TIMEOUT_SECONDS} s (log: {log})")
    if proc.returncode != 0:
        pytest.fail(f"{' '.join(cmd)} failed with status {proc.returncode} (log: {log}); tail:\n{_tail(log)}")
    if not family_dir.is_dir():
        pytest.fail(f"{' '.join(cmd)} succeeded but wrote no {family_dir} (log: {log}); tail:\n{_tail(log)}")
    return family_dir


@pytest.fixture(scope="session")
def py_goldens() -> Callable[[str], Path]:
    """``py_goldens(family) -> Path``: the Python goldens of a family, generated once per session."""
    ready: dict[str, Path] = {}
    pregenerated = os.environ.get(PREGENERATED_ENV)

    def get(family: str) -> Path:
        if family in ready:
            return ready[family]
        if pregenerated:
            family_dir = Path(pregenerated).expanduser().resolve() / family
            if not family_dir.is_dir():
                pytest.fail(
                    f"{PREGENERATED_ENV}={pregenerated}: no pre-generated Python goldens for family "
                    f"{family!r} at {family_dir}"
                )
        else:
            family_dir = _generate(family)
        ready[family] = family_dir
        return family_dir

    return get
