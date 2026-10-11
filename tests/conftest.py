"""Shared pytest configuration: the data trees, the test tiers and the golden-case environment.

Tiers (``pyproject.toml`` ``[tool.pytest.ini_options]``):

* the unit tier (default) depends on nothing outside the repository and the locked environment;
* ``-m golden`` compares against the R goldens under ``migration/goldens``; a golden test that
  reads a run also needs the fixture snapshot or the synthetic layer below ``migration/``;
* ``-m sandbox`` runs commands inside ``migration/harness/sandbox.sh``.

``migration/`` is the migration record (the plan, the R goldens, the case lists, the fixture snapshot of
PIK-internal runs, the synthetic layer and the sandbox harness). It is kept outside the repository
(gitignored), so the golden and sandbox tiers run on a checkout that has it and skip as a whole where
it is absent (CI included); the unit tier never touches it. The paths mirror
``migration/harness/lib.sh``: ``MODELSTATS_FIXTURES`` overrides the canonical fixture snapshot,
``MODELSTATS_SYNTHETIC`` the synthetic layer and ``MODELSTATS_GOLDENS`` the R reference outputs; the
case lists live in ``migration/cases``. A golden test is skipped only when a tree it requests through
its fixtures (``cases_dir``, ``goldens_dir``, ``fixtures_dir``, ``synthetic_dir``) is absent; a golden
test that *runs* is never skipped or relaxed (the migration plan, ``03`` section 4.6).

Golden cases (phase 1): ``golden_cases(family, mode)`` reads ``migration/cases/<family>.tsv`` the way
``migration/harness/make_goldens.R`` does and resolves the sandbox-level settings of
``migration/harness/sandbox.sh`` (mode, user, slurm case, frozen clock, cwd) into ``GoldenCase``
records; the ``case_effects`` fixture turns one record into a ``modelstats.env.FakeEffects`` whose
overlay, canned scheduler, clock and user equal what the R oracle saw for that row. The end-to-end
``rs`` goldens of phase 4 additionally run inside ``sandbox.sh`` itself.
"""

from __future__ import annotations

import os
import re
from collections.abc import Callable, Mapping
from dataclasses import dataclass
from datetime import datetime, timedelta
from pathlib import Path
from zoneinfo import ZoneInfo

import pytest

from modelstats.env import CannedSlurm, FakeEffects, Mode, OverlayPaths

REPO = Path(__file__).resolve().parent.parent
MIGRATION = REPO / "migration"
FIXTURES = Path(os.environ.get("MODELSTATS_FIXTURES", MIGRATION / "fixtures"))
SYNTHETIC = Path(os.environ.get("MODELSTATS_SYNTHETIC", MIGRATION / "synthetic"))
GOLDENS = Path(os.environ.get("MODELSTATS_GOLDENS", MIGRATION / "goldens"))
CASES = MIGRATION / "cases"
RECORDED_SLURM = FIXTURES / "_meta" / "slurm" / "latest"

# Sandbox defaults (migration/harness/lib.sh and sandbox.sh): the fake user, the stopped clock, the
# zone and the variables the R oracle saw, R_CLI_NUM_COLORS=256 included (crayon and cli report 256
# colours without a tty; it precedes options(crayon.enabled), migration/02-python-libraries.md 4.5, and
# the crayon-off rows override it with options(cli.num_colors = 1L), make_goldens.R set_crayon()).
# Every golden was generated under these unless its row says otherwise.
SANDBOX_USER = "pascalfu"
SANDBOX_FROZEN = "2026-09-30 12:00:00"
SANDBOX_TZ = ZoneInfo("Europe/Berlin")
SANDBOX_ENVIRON: Mapping[str, str] = {
    "TZ": "Europe/Berlin",
    "LC_ALL": "C.utf8",
    "LANG": "C.utf8",
    "MAGPIE_RESULTS_ARCHIVE_PATH": "/p/projects/rd3mod/models/results/magpie",
    "COLUMNS": "200",
    "TERM": "xterm-256color",
    "R_CLI_NUM_COLORS": "256",
}
_FROZEN_FORMAT = "%Y-%m-%d %H:%M:%S"
_CTIME_PRESET = re.compile(r"^ctime\+(\d+)d$")


def read_case_rows(path: Path) -> list[dict[str, str]]:
    """Read a case list as ``make_goldens.R`` does: tab-separated, header row, ``#`` comments, no quoting.

    Blank lines are skipped and missing cells are ``""``; every value stays a string.
    """
    rows: list[dict[str, str]] = []
    header: list[str] | None = None
    for raw in path.read_text(encoding="utf-8").splitlines():
        if not raw.strip() or raw.startswith("#"):
            continue
        cells = raw.split("\t")
        if header is None:
            header = cells
            continue
        cells += [""] * (len(header) - len(cells))
        rows.append(dict(zip(header, cells[: len(header)], strict=True)))
    if header is None:
        raise ValueError(f"case file without a header row: {path}")
    return rows


def resolve_frozen(spec: str) -> datetime:
    """``--frozen`` of ``sandbox.sh``: ``'YYYY-MM-DD hh:mm:ss'`` in the sandbox zone, or ``ctime+Nd``.

    ``ctime+Nd`` is the ctime of ``<fixtures>/p/projects`` on this machine plus N days at 12:00:00
    (``lib.sh`` ``resolve_frozen``), the MAgPIE evaluate regimes.
    """
    preset = _CTIME_PRESET.match(spec)
    if preset:
        stamp = os.stat(FIXTURES / "p" / "projects").st_ctime
        ctime = datetime.fromtimestamp(stamp, tz=SANDBOX_TZ)
        day = (ctime + timedelta(days=int(preset.group(1)))).date()
        return datetime.combine(day, datetime.min.time().replace(hour=12), tzinfo=SANDBOX_TZ)
    return datetime.strptime(spec, _FROZEN_FORMAT).replace(tzinfo=SANDBOX_TZ)


def slurm_case_dir(name: str) -> Path:
    """``--slurm-case``: ``recorded`` is the capture under the fixtures, else ``cases/slurm/<name>``."""
    return RECORDED_SLURM if name == "recorded" else CASES / "slurm" / name


@dataclass(frozen=True)
class GoldenCase:
    """One row of ``migration/cases/<family>.tsv`` with the sandbox-level settings resolved.

    ``row`` keeps every column verbatim for the family-specific ones (``dirs``, ``argv``, ``sort``,
    ``colors`` ...); fixture-relative paths in them are turned into logical paths with
    ``logical_path`` / ``logical_paths`` (``fix_path`` of ``make_goldens.R``).
    """

    family: str
    id: str
    row: Mapping[str, str]
    mode: Mode
    user: str
    slurm_case: str
    frozen: datetime
    cwd: str

    @property
    def fixture_root(self) -> str:
        """``RS_FIXTURE_ROOT``: ``/p`` or ``/fixtures/p``."""
        return str(self.mode.fixture_root)

    def logical_path(self, relative: str) -> str:
        """A fixture-relative path (``projects/...``) as the oracle saw it; absolute paths pass through."""
        if not relative or relative.startswith("/"):
            return relative
        return f"{self.fixture_root}/{relative}"

    def logical_paths(self, column: str) -> list[str]:
        """A comma-separated path column as logical paths (empty column: no paths)."""
        value = self.row.get(column, "")
        return [self.logical_path(p) for p in value.split(",")] if value else []


def case_list(family: str) -> Path:
    """``migration/cases/<family>.tsv``, read at collection time by the golden modules.

    Without the migration record (a checkout that lacks it, CI, the unpacked sdist) the golden module
    is skipped as a whole instead of failing the collection of the unit tier (pytest imports every
    module before it applies the marker deselection).
    """
    path = CASES / f"{family}.tsv"
    if not path.is_file():
        pytest.skip(
            f"case list absent: {path} (the migration record is not in the repository)",
            allow_module_level=True,
        )
    return path


def golden_cases(family: str, mode: Mode | None = None) -> list[GoldenCase]:
    """The cases of one family; ``mode`` applies where the row has no ``mode`` column (status: both modes)."""
    cases: list[GoldenCase] = []
    for row in read_case_rows(case_list(family)):
        row_mode = Mode(row["mode"]) if row.get("mode") else (mode or Mode.ONCLUSTER)
        cwd = row.get("cwd", "")
        cases.append(
            GoldenCase(
                family=family,
                id=row["id"],
                row=row,
                mode=row_mode,
                user=row.get("user") or SANDBOX_USER,
                slurm_case=row.get("slurm_case") or "recorded",
                frozen=resolve_frozen(row.get("frozen") or SANDBOX_FROZEN),
                cwd=f"{row_mode.fixture_root}/{cwd}" if cwd else str(row_mode.fixture_root),
            )
        )
    return cases


def make_case_effects(
    case: GoldenCase, fixtures: Path = FIXTURES, synthetic: Path = SYNTHETIC
) -> FakeEffects:
    """The ``FakeEffects`` of ``sandbox.sh`` for ``case``: overlay, canned scheduler, clock, user, cwd."""
    environ = {**SANDBOX_ENVIRON, "USER": case.user, "LOGNAME": case.user}
    return FakeEffects(
        now=case.frozen,
        user=case.user,
        on_cluster=case.mode.on_cluster,
        paths=OverlayPaths.sandbox(fixtures / "p", synthetic / "p", case.mode),
        slurm=CannedSlurm.from_case_dir(slurm_case_dir(case.slurm_case)),
        cwd=case.cwd,
        environ=environ,
    )


@pytest.fixture(scope="session")
def repo_root() -> Path:
    return REPO


@pytest.fixture(scope="session")
def fixtures_dir() -> Path:
    """The canonical fixture snapshot (``<fixtures>/p`` is the cluster layout).

    Skips the test when the snapshot is absent; only tests that read a fixture run request it.
    """
    if not (FIXTURES / "p").is_dir():
        pytest.skip(f"fixture snapshot absent: {FIXTURES}/p (migration/01-test-data-inventory.md)")
    return FIXTURES


@pytest.fixture(scope="session")
def synthetic_dir() -> Path:
    """The synthetic layer; skips the test when it has not been built."""
    if not (SYNTHETIC / "p").is_dir():
        pytest.skip(f"synthetic layer not built: {SYNTHETIC} (migration/harness/build_synthetic.sh)")
    return SYNTHETIC


def record_path(root: Path, *parts: str) -> Path:
    """``root/parts`` inside the migration record, or a skip when that part of the record is absent.

    For helpers of the unit tier that read a case directory or an R golden directly instead of
    requesting ``cases_dir`` or ``goldens_dir``: the test skips like those fixtures do.
    """
    if not root.is_dir():
        pytest.skip(f"migration record absent: {root} (the record is not in the repository)")
    return root.joinpath(*parts)


@pytest.fixture(scope="session")
def goldens_dir() -> Path:
    """The R reference outputs; skips the test when the migration record is absent."""
    if not GOLDENS.is_dir():
        pytest.skip(f"R goldens absent: {GOLDENS} (the migration record is not in the repository)")
    return GOLDENS


@pytest.fixture(scope="session")
def cases_dir() -> Path:
    """The case lists the goldens were made from; skips the test when the record is absent."""
    if not CASES.is_dir():
        pytest.skip(f"case lists absent: {CASES} (the migration record is not in the repository)")
    return CASES


@pytest.fixture(scope="session")
def case_effects(fixtures_dir: Path) -> Callable[[GoldenCase], FakeEffects]:
    """Build a golden case's ``FakeEffects`` (fixture snapshot required, synthetic layer when built)."""

    def build(case: GoldenCase) -> FakeEffects:
        return make_case_effects(case, fixtures_dir, SYNTHETIC)

    return build
