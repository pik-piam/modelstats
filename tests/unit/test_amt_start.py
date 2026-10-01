"""amt.start: ``startRuns()`` against ``R/modeltests.R`` lines 71-166 (R 4.6.1 / GNU sed 4.10 facts pinned 2026-10-01).

Three tiers:

- self-contained (no ``migration/``): a fake checkout built here with the exact bytes of
  ``migration/harness/build_fake_remind.sh`` (the sha256 of ``config/default.cfg`` equals the one the R goldens
  recorded, which pins the copy), a :class:`FakeHarness` run hook that plays the sandbox's ``/bin/sh`` (the ``&&``
  short-circuit), the fake binaries' canned exit statuses and the ``select_scenarios`` bridge, and
  :class:`RecordingEffects` for everything else: the command strings, the executed commands, the files written,
  the sleeps, the wait loop (BUG-032), the warnings of a failing or missing command, the sed edit (compared
  with the real GNU sed when it is on the PATH), ``deleteEmptyRealizationFolders()``, the dry run;
- golden (skipped without ``migration/goldens/amt``): the five start cases of the workflow, each run with
  RecordingEffects against the fake checkout: the executed commands equal the R trace (``trace.jsonl`` minus the
  ``sed`` calls: the port edits the file instead, the documented exception of the AMT comparison; the
  ``select_scenarios`` bridge call is invisible to the fake ``Rscript`` and asserted separately), the written
  files equal ``state/*.json`` by value and ``effects.json`` by path set and sha256 (RDS by value);
- sandbox (skipped without the sandbox prerequisites): three of the cases run inside
  ``migration/harness/sandbox.sh`` with the real fake binaries, the real bridge Rscript and the overlay's clone
  diff, compared with the same goldens.

Documented differences to the R trace, both exact by construction: no ``sed`` process (a file edit with the same
resulting bytes, atomic like ``sed -i``), and the ``find modules/ -name module.gms`` of
``deleteEmptyRealizationFolders()`` as :meth:`Effects.walk` (``find`` is not a fake binary, so the R trace
never showed it either).

R facts (``Rscript`` 4.6.1, 2026-10-01): the deparsed calls of the ``system()`` sites as ``Rscript`` prints them in
a warning are the first deparse line (``system(paste0("Rscript start.R ", "startgroup=AMT titletag=AMT ", `` and
``system(paste0("squeue -u ", user, " -h -o '%i %q %T %C %M %j %V %L %e %Z'"), `` keep the trailing comma, the
``remind-evaluate-squeue-3fail`` golden shows the latter); ``paste0("squeue -u ", NULL, " -h")`` is ``squeue -u  -h``;
``any(grepl(p, character(0)))`` is ``FALSE``; a plain ``system()`` returns the status and warns only on 127;
``stop("Model cannot be NULL")`` reports the call ``startRuns(model = model, user = user, mydir = mydir)``.
"""

from __future__ import annotations

import contextlib
import hashlib
import io
import json
import os
import shutil
import subprocess
import sys
import warnings
from collections.abc import Mapping, Sequence
from pathlib import Path
from typing import Any

import pandas as pd
import pytest
from _fake_effects import TEN, FakeCall, RecordingEffects

from modelstats.amt import start as st
from modelstats.amt.bridges import BRIDGE_ENV, SELECT_SCENARIOS_SCRIPT
from modelstats.amt.state import read_lastcommit, read_runcode, read_runs_to_start, run_names
from modelstats.env import DryRunEffects
from modelstats.errors import RParityError, RWarning
from modelstats.rdata_io import read_rds, write_rds

REPO = Path(__file__).resolve().parents[2]
MIGRATION = REPO / "migration"
AMT_CASES = MIGRATION / "cases" / "amt"
AMT_GOLDENS = MIGRATION / "goldens" / "amt"
SLURM_CASES = MIGRATION / "cases" / "slurm"
SLURM_RECORDED = MIGRATION / "fixtures" / "_meta" / "slurm" / "latest"
SANDBOX = MIGRATION / "harness" / "sandbox.sh"
FIXTURES_PROBE = MIGRATION / "fixtures" / "p" / "projects"
FAKE_REMIND_PROBE = MIGRATION / "synthetic/p/projects/remind/modeltests/remind/scripts/start"
VENV_PYTHON = REPO / ".venv" / "bin" / "python"

START_CASES = [
    "remind-start",
    "remind-start-git-fail",
    "remind-start-make-fail",
    "magpie-start",
    "magpie-start-job-in-mydir",
]
SANDBOX_CASES = ["remind-start", "remind-start-git-fail", "magpie-start-job-in-mydir"]
USER = "pascalfu"
FROZEN_DATE = "2026-09-30"  # the harness' stopped clock (RecordingEffects.DEFAULT_FROZEN)
RUNCODE_FROZEN = ".*-AMT_2026-09-30|.*-AMT_2026-10-01"
OLD_RUNCODE = ".*-AMT_2026-09-28|.*-AMT_2026-09-29"
LASTCOMMIT = "13f60fdd4fff366bebcc7dfe522897dedeb43cc7"
GIT_RESET = ("git", ("reset", "--hard", "origin/develop"))
GIT_PULL = ("git", ("pull",))

# --------------------------------------------------------------------------- the fake checkouts, byte for byte

# migration/harness/build_fake_remind.sh: `printf '%s\n' lines...`; the sha256 values are what the R goldens recorded
# (effects.json of remind-start: touched, same bytes; magpie-start: modified FALSE -> TRUE).
REMIND_DEFAULT_CFG = [
    "# fake REMIND config/default.cfg (build_fake_remind.sh); startRuns() edits the next-but-one line with sed -i",
    "cfg <- list()",
    'cfg$model_name <- "REMIND"',
    "cfg$force_download <- FALSE",
    'cfg$gms$optimization <- "nash"',
]
REMIND_DEFAULT_CFG_SHA256 = "1108dfd8f16c5c50ac0736e0a85b8a8d1f72c82862dcddb3da6592718c26117e"
MAGPIE_DEFAULT_CFG = [
    "# fake MAgPIE config/default.cfg (build_fake_remind.sh)",
    "cfg <- list()",
    'cfg$model_name <- "MAgPIE"',
    "cfg$force_download <- FALSE",
    'cfg$gms$optimization <- "nlp_apr17"',
]
MAGPIE_DEFAULT_CFG_SHA256_BEFORE = "244655ba5510178090b54ae2baf45a33f38a6ae2f2532a0d716f75d9250dc01c"
MAGPIE_DEFAULT_CFG_SHA256_AFTER = "dc3df11103bec5d5713703afa42639d0118d5650fffb85e3441f73c67f47f776"
# config/scenario_config.csv rows (title, start, description); selectScenarios(startgroup = "AMT") keeps the AMT rows
SCENARIOS = [
    ("SSP2-NPi", "AMT", "SSP2 with national policies"),
    ("default", "AMT", "default scenario"),
    ("SSP2-EU21-PkBudg650", "AMT", "peak budget 650"),
    ("SSP2-NPi2025-calibrate", "AMT", "CES calibration"),
    ("SSP3-NPi2025", "AMT", "SSP3 with national policies 2025"),
    ("SSP2-EcBudg500", "AMT", "end-of-century budget 500"),
    ("SSP2-EU21-NPi2025", "AMT", "EU21 national policies 2025"),
    ("testOneRegi", "AMT", "single region test"),
    ("SSP2-never", "AMT", "in the AMT group but never started"),
    ("SSP2-manual", "1", "not in the AMT group"),
]
AMT_TITLES = [title for title, group, _ in SCENARIOS if group == "AMT"]


def _lines(lines: Sequence[str]) -> str:
    return "".join(line + "\n" for line in lines)


def _sha256(path: Path) -> str:
    return hashlib.sha256(path.read_bytes()).hexdigest()


def selected_frame() -> pd.DataFrame:
    """What ``selectScenarios(settings, interactive = FALSE, startgroup = "AMT")`` returns for the fake checkout."""
    rows = [(t, g, d) for t, g, d in SCENARIOS if g == "AMT"]
    return pd.DataFrame(
        {"start": [g for _, g, _ in rows], "description": [d for _, _, d in rows]},
        index=pd.Index([t for t, _, _ in rows], dtype=object),
    )


def make_checkout(root: Path, model: str) -> str:
    """The fake checkout of build_fake_remind.sh for ``model`` plus the state files; returns ``mydir`` with the
    trailing slash of the cron job's argument (``/p/projects/remind/modeltests/remind/``)."""
    if model == "REMIND":
        amt_root = root / "modeltests"
        mydir = amt_root / "remind"
        (mydir / "config").mkdir(parents=True)
        (mydir / "config" / "default.cfg").write_text(_lines(REMIND_DEFAULT_CFG), encoding="utf-8")
        (mydir / "config" / "scenario_config.csv").write_text(
            _lines(
                [
                    "# fake REMIND config/scenario_config.csv (build_fake_remind.sh)",
                    "title;start;description",
                    *(f"{t};{g};{d}" for t, g, d in SCENARIOS),
                ]
            ),
            encoding="utf-8",
        )
        (mydir / "scripts" / "start").mkdir(parents=True)
        (mydir / "scripts" / "start" / "selectScenarios.R").write_text("selectScenarios <- function(...) NULL\n")
        (mydir / "scripts" / "cs2").mkdir()
        (mydir / "scripts" / "cs2" / "run_compareScenarios2.R").write_text("# existence only\n")
        for module, realization in (("01_macro", "singleSectorGr"), ("02_welfare", "utilitarian")):
            (mydir / "modules" / module / realization).mkdir(parents=True)
            (mydir / "modules" / module / "module.gms").write_text(
                _lines(["*** fake module.gms", f'$include "./modules/{module}/{realization}/realization.gms"'])
            )
            (mydir / "modules" / module / realization / "realization.gms").write_text("*** fake realization.gms\n")
        (mydir / "modules" / "01_macro" / "input").mkdir()
        (mydir / "modules" / "01_macro" / "input" / ".keep").write_text("\n")
        (mydir / "modules" / "01_macro" / "emptyReal").mkdir()
        (mydir / "magpie").mkdir()
        (mydir / "magpie" / ".keep").write_text("\n")
        (mydir / "tests").mkdir()
        (mydir / "Makefile").write_text("# fake REMIND Makefile\n")
        (amt_root / "test-full.log").write_text("[ FAIL 0 | WARN 0 | SKIP 3 | PASS 120 ]\n")
        write_rds(
            mydir / "runsToStart.rds", pd.DataFrame({"start": ["1,AMT"]}, index=pd.Index(["old-AMT"], dtype=object))
        )
    elif model == "MAgPIE":
        amt_root = root / "tests"
        mydir = amt_root / "magpie"
        (mydir / "config").mkdir(parents=True)
        (mydir / "config" / "default.cfg").write_text(_lines(MAGPIE_DEFAULT_CFG), encoding="utf-8")
        (mydir / "modules" / "10_land" / "feb15").mkdir(parents=True)
        (mydir / "modules" / "10_land" / "module.gms").write_text(
            _lines(["*** fake module.gms", '$include "./modules/10_land/feb15/realization.gms"'])
        )
        (mydir / "modules" / "10_land" / "feb15" / "realization.gms").write_text("*** fake realization.gms\n")
        (mydir / "modules" / "10_land" / "emptyReal").mkdir()
        (mydir / "scripts" / "start").mkdir(parents=True)
        (mydir / "scripts" / "start" / ".keep").write_text("\n")
        (mydir / "Makefile").write_text("# fake MAgPIE Makefile\n")
    else:
        raise ValueError(model)
    (amt_root / ".testsstatus").write_text("next:start\n")
    write_rds(mydir / "runcode.rds", OLD_RUNCODE)
    write_rds(mydir / "lastcommit.rds", LASTCOMMIT)
    return f"{mydir}/"


def snapshot(root: Path) -> dict[str, tuple[str, str]]:
    """Every file below ``root``: relative path -> (sha256, inode and mtime); directories as ``("dir", "")``."""
    out: dict[str, tuple[str, str]] = {}
    for path in sorted(root.rglob("*")):
        rel = path.relative_to(root).as_posix()
        if path.is_dir():
            out[rel] = ("dir", "")
        else:
            lst = path.lstat()
            out[rel] = (_sha256(path), f"{lst.st_ino}:{lst.st_mtime_ns}")
    return out


def tree_hash(root: Path) -> str:
    digest = hashlib.sha256()
    for path in [root, *sorted(root.rglob("*"))]:
        lst = path.lstat()
        digest.update(
            f"{path.relative_to(root).as_posix()}|{oct(lst.st_mode)}|{lst.st_size}|{lst.st_mtime_ns}\n".encode()
        )
        if path.is_file() and not path.is_symlink():
            digest.update(path.read_bytes())
    return digest.hexdigest()


# --------------------------------------------------------------------------- the sandbox around RecordingEffects.run

_OPERATORS = ("&&", "||", ";", "|", "&")


class FakeHarness:
    """What the sandbox puts around a command, for ``RecordingEffects(run_hook=...)``.

    ``/bin/sh`` runs the simple commands of a shell line with the ``&&`` short-circuit; the fake binaries
    (``migration/harness/fakebin/_lib.sh``) exit with ``<case>/<name>.exit`` (``git-reset``, ``git-pull``,
    ``make``, ``rscript-start``; 0 otherwise); the ``select_scenarios`` bridge answers by writing ``frame`` to
    its ``--out`` path (with the ``--suffix`` applied) and printing its JSON line; ``squeue`` answers from
    ``squeue`` (one ``(stdout, status)`` per call) and falls back to RecordingEffects' scheduler fake (``None``).
    ``executed`` lists what ran as ``(tool, argv, cwd)``, the bridge included; ``bridge_calls`` the bridge argv.
    """

    def __init__(
        self,
        *,
        frame: pd.DataFrame | None = None,
        exits: Mapping[str, int] | None = None,
        squeue: Sequence[tuple[str, int]] | None = None,
    ) -> None:
        self.frame = frame
        self.exits = dict(exits or {})
        self.squeue = list(squeue or [])
        self.executed: list[tuple[str, tuple[str, ...], str]] = []
        self.bridge_calls: list[tuple[list[str], str]] = []

    def __call__(self, tokens: list[str], cwd: str) -> tuple[str, int] | None:
        if tokens and os.path.basename(tokens[0]) in ("squeue", "sacct"):
            self.executed.append((os.path.basename(tokens[0]), tuple(tokens[1:]), cwd))
            return self.squeue.pop(0) if self.squeue else None
        groups: list[tuple[str | None, list[str]]] = []
        operator: str | None = None
        current: list[str] = []
        for token in tokens:
            if token in _OPERATORS:
                groups.append((operator, current))
                operator, current = token, []
            else:
                current.append(token)
        groups.append((operator, current))
        stdout, status = "", 0
        for operator, segment in groups:
            if not segment or (operator == "&&" and status != 0) or (operator == "||" and status == 0):
                continue
            out, status = self._run(segment, cwd)
            stdout += out
        return stdout, status

    def _run(self, segment: list[str], cwd: str) -> tuple[str, int]:
        tool, args = os.path.basename(segment[0]), segment[1:]
        if tool == "Rscript" and args and args[0].endswith("/" + SELECT_SCENARIOS_SCRIPT):
            self.executed.append((tool, tuple(args), cwd))
            self.bridge_calls.append((list(segment), cwd))
            return self._bridge(args)
        if tool == "git":
            sub = next((a for a in args if not a.startswith("-")), "")
            name = f"git-{sub}"
        elif tool == "Rscript" and any(a == "start.R" or a.endswith("/start.R") for a in args):
            name = "rscript-start"
        else:
            name = tool
        self.executed.append((tool, tuple(args), cwd))
        return "", self.exits.get(name, 0)

    def _bridge(self, args: list[str]) -> tuple[str, int]:
        assert self.frame is not None, "the bridge was called without a frame to answer with"
        out = args[args.index("--out") + 1]
        frame = self.frame.copy()
        if "--suffix" in args:
            suffix = args[args.index("--suffix") + 1]
            frame.index = pd.Index([f"{name}{suffix}" for name in frame.index], dtype=object)
        write_rds(out, frame)
        answer = {
            "bridge": "select_scenarios",
            "ok": True,
            "row_names": [str(n) for n in frame.index],
            "columns": [str(c) for c in frame.columns],
            "nrow": len(frame),
            "out": out,
            "sources": [{"file": "scripts/start/selectScenarios.R", "algorithm": "sha256", "digest": "ab" * 32}],
            "r_version": "R version 4.6.1 (2026-06-24)",
        }
        return json.dumps(answer) + "\n", 0


def make_effects(
    harness: FakeHarness, *, user: str | None = USER, slurm_case_dir: Path | None = None
) -> RecordingEffects:
    return RecordingEffects(
        user=user or "unknown", on_cluster=True, slurm_case_dir=slurm_case_dir, run_hook=harness, env={}
    )


def run_start(model: str | None, mydir: str, user: str | None, eff: RecordingEffects) -> list[warnings.WarningMessage]:
    """``modeltests()``'s ``withr::local_dir(mydir)`` then ``startRuns()``; the deferred warnings come back."""
    with warnings.catch_warnings(record=True) as caught:
        warnings.simplefilter("always")
        with eff.chdir(mydir):
            st.start_runs(model, mydir, user, effects=eff)
    return list(caught)


def executed_relative(harness: FakeHarness, mydir: str) -> list[tuple[str, tuple[str, ...], str]]:
    """The executed commands with the cwd relative to ``mydir`` (``""`` for mydir itself), the bridge excluded."""
    base = Path(mydir).resolve()
    out = []
    for tool, argv, cwd in harness.executed:
        if tool == "Rscript" and argv and argv[0].endswith("/" + SELECT_SCENARIOS_SCRIPT):
            continue
        rel = Path(cwd).resolve().relative_to(base).as_posix()
        out.append((tool, argv, "" if rel == "." else rel))
    return out


# what the fake binaries see as argv (R: system() through /bin/sh; the sandbox's trace.jsonl)
RSCRIPT_TEST_ONE_REGI = (
    "start.R", "--testOneRegi", "titletag=AMT",
    "slurmConfig=--qos=priority --nodes=1 --tasks-per-node=1 --wait --time=2:00:00",
)  # fmt: skip
RSCRIPT_BUNDLE = (
    "start.R", "startgroup=AMT", "titletag=AMT",
    "slurmConfig=--qos=standby --nodes=1 --tasks-per-node=12 --time=36:00:00", "config/scenario_config.csv",
)  # fmt: skip
REMIND_TRACE = [
    (*GIT_RESET, ""),
    (*GIT_PULL, ""),
    ("Rscript", RSCRIPT_TEST_ONE_REGI, ""),
    ("Rscript", RSCRIPT_BUNDLE, ""),
    (*GIT_RESET, "magpie"),
    (*GIT_PULL, "magpie"),
    ("make", ("test-full-slurm",), ""),
]
MAGPIE_TRACE = [
    (*GIT_RESET, ""),
    (*GIT_PULL, ""),
    ("Rscript", ("start.R", "runscripts=default", "submit=SLURM priority"), ""),
    ("squeue", ("-u", USER, "-h", "-o", TEN), ""),
    ("Rscript", ("start.R", "runscripts=test_runs", "submit=SLURM standby"), ""),
]
REMIND_SHELL_RUNS = [st.GIT_RESET_PULL, st.REMIND_TEST_ONE_REGI, st.REMIND_BUNDLE, st.GIT_RESET_PULL, st.MAKE_TEST_FULL]
MAGPIE_SHELL_RUNS = [st.GIT_RESET_PULL, st.MAGPIE_DEFAULT_RUN, st.MAGPIE_TEST_RUNS]


# --------------------------------------------------------------------------- the pieces


def test_runcode_for() -> None:
    import datetime as dt

    assert st.runcode_for(dt.date(2026, 9, 30)) == RUNCODE_FROZEN
    assert st.runcode_for(dt.date(2026, 12, 31)) == ".*-AMT_2026-12-31|.*-AMT_2027-01-01"
    assert st.runcode_for(dt.date(2028, 2, 28)) == ".*-AMT_2028-02-28|.*-AMT_2028-02-29"


def test_the_r_command_strings_are_the_literal_lines_of_modeltests_r() -> None:
    # lines 98, 112-113, 120-123, 146, 150, 154-155, 161: what R hands to /bin/sh
    assert st.GIT_RESET_PULL == "git reset --hard origin/develop && git pull"
    assert st.REMIND_TEST_ONE_REGI == (
        "Rscript start.R --testOneRegi titletag=AMT "
        'slurmConfig="--qos=priority --nodes=1 --tasks-per-node=1 --wait --time=2:00:00"'
    )
    assert st.REMIND_BUNDLE == (
        "Rscript start.R startgroup=AMT titletag=AMT "
        'slurmConfig="--qos=standby --nodes=1 --tasks-per-node=12 --time=36:00:00" config/scenario_config.csv'
    )
    assert st.MAKE_TEST_FULL == "make test-full-slurm"
    assert st.MAGPIE_DEFAULT_RUN == "Rscript start.R runscripts=default submit='SLURM priority'"
    assert st.MAGPIE_TEST_RUNS == "Rscript start.R runscripts=test_runs submit='SLURM standby'"
    assert st.squeue_wait_command(USER) == f"squeue -u {USER} -h -o '%i %q %T %C %M %j %V %L %e %Z'"
    assert st.squeue_wait_command(None) == "squeue -u  -h -o '%i %q %T %C %M %j %V %L %e %Z'"  # paste0 with NULL
    assert (
        st.SED_FORCE_DOWNLOAD_ON
        == "sed -i 's/cfg$force_download <- FALSE/cfg$force_download <- TRUE/' config/default.cfg"
    )
    assert (
        st.SED_FORCE_DOWNLOAD_OFF
        == "sed -i 's/cfg$force_download <- TRUE/cfg$force_download <- FALSE/' config/default.cfg"
    )
    # the deparsed calls as Rscript prints them (probe of 2026-10-01; the squeue one is in remind-evaluate-squeue-3fail)
    assert st.SQUEUE_WAIT_CALL == 'system(paste0("squeue -u ", user, " -h -o \'%i %q %T %C %M %j %V %L %e %Z\'"), '
    assert st.REMIND_BUNDLE_CALL == 'system(paste0("Rscript start.R ", "startgroup=AMT titletag=AMT ", '
    assert st.REMIND_TEST_ONE_REGI_CALL == (
        'system(paste("Rscript start.R --testOneRegi titletag=AMT", '
        '"slurmConfig=\\"--qos=priority --nodes=1 --tasks-per-node=1 --wait --time=2:00:00\\""))'
    )
    assert st.START_RUNS_CALL == "startRuns(model = model, user = user, mydir = mydir)"


# --- sed -i as a file edit


@pytest.mark.parametrize(
    "content",
    [
        "cfg$force_download <- FALSE\n",
        _lines(REMIND_DEFAULT_CFG),
        "x cfg$force_download <- FALSE and cfg$force_download <- FALSE\nnone",  # two per line, no final newline
        "a\r\ncfg$force_download <- FALSE\r\n",  # CRLF kept
        "nothing to replace\n\n",
        "",
    ],
)
def test_sed_replace_produces_the_bytes_of_gnu_sed(tmp_path: Path, content: str) -> None:
    target = tmp_path / "default.cfg"
    target.write_bytes(content.encode())
    eff = RecordingEffects()
    assert st.sed_replace(target, st.FORCE_DOWNLOAD_OFF, st.FORCE_DOWNLOAD_ON, eff) == 0
    expected = "\n".join(line.replace(st.FORCE_DOWNLOAD_OFF, st.FORCE_DOWNLOAD_ON, 1) for line in content.split("\n"))
    assert target.read_bytes() == expected.encode()
    if shutil.which("sed") is not None:
        reference = tmp_path / "reference.cfg"
        reference.write_bytes(content.encode())
        subprocess.run(["sed", "-i", f"s/{st.FORCE_DOWNLOAD_OFF}/{st.FORCE_DOWNLOAD_ON}/", str(reference)], check=True)
        assert target.read_bytes() == reference.read_bytes()
    [call] = [c for c in eff.calls if c.method == "write_text"]
    assert call.args == (str(target), expected, False, True)  # atomic, like sed's temporary file + rename


def test_sed_replace_keeps_the_mode_and_rewrites_an_unchanged_file(tmp_path: Path) -> None:
    target = tmp_path / "default.cfg"
    target.write_text("nothing\n")
    target.chmod(0o600)
    before = target.stat()
    eff = RecordingEffects()
    assert st.sed_replace(target, st.FORCE_DOWNLOAD_OFF, st.FORCE_DOWNLOAD_ON, eff) == 0
    after = target.stat()
    assert target.read_text() == "nothing\n"
    assert after.st_mode == before.st_mode
    assert (after.st_ino, after.st_mtime_ns) != (before.st_ino, before.st_mtime_ns)  # the golden's "touched" entry


def test_sed_replace_of_a_missing_file_prints_seds_message_and_returns_2(
    tmp_path: Path, capsys: pytest.CaptureFixture[str]
) -> None:
    eff = RecordingEffects()
    with eff.chdir(tmp_path):
        assert st.sed_replace("config/default.cfg", "a", "b", eff) == 2
    assert capsys.readouterr().err == "sed: can't read config/default.cfg: No such file or directory\n"
    assert not any(c.method == "write_text" for c in eff.calls)


# --- deleteEmptyRealizationFolders


def _module_tree(root: Path) -> None:
    """The probe tree of 2026-10-01 (R: find/sub/dir/setdiff/unlink verified on it) plus a nested module."""
    m = root / "modules"
    (m / "01_macro" / "singleSectorGr").mkdir(parents=True)
    (m / "01_macro" / "input").mkdir()
    (m / "01_macro" / "input" / ".keep").write_text("")
    (m / "01_macro" / "emptyReal").mkdir()
    (m / "01_macro" / "emptyReal" / "leftover.gms").write_text("")  # a non-empty "empty" realization is unlinked too
    (m / "01_macro" / "module.gms").write_text(
        '*** fake module.gms\n$include "./modules/01_macro/singleSectorGr/realization.gms"\n'
    )
    (m / "01_macro" / "singleSectorGr" / "realization.gms").write_text("*** fake\n")
    (m / "02_welfare" / "utilitarian").mkdir(parents=True)
    (m / "02_welfare" / "emptyB").mkdir()
    (m / "02_welfare" / ".hidden").mkdir()  # dir() skips dot entries: never unlinked
    (m / "02_welfare" / "strayfile.txt").write_text("")  # a file is in dir() too: unlinked
    (m / "02_welfare" / "module.gms").write_bytes(
        b'*** fake\r\n$include "./modules/02_welfare/utilitarian/realization.gms"\r\nsome text realization.gms here'
    )  # CRLF and an incomplete last line, as readLines() accepts them
    (m / "02_welfare" / "utilitarian" / "realization.gms").write_text("")
    (m / "03_nested" / "sub" / "keepme").mkdir(parents=True)  # find descends: modules/03_nested/sub/module.gms
    (m / "03_nested" / "sub" / "gone").mkdir()
    (m / "03_nested" / "sub" / "module.gms").write_text('$include "./modules/sub/keepme/realization.gms"\n')
    (m / "03_nested" / "unlisted").mkdir()  # no module.gms directly in 03_nested: nothing happens there


def test_delete_empty_realization_folders(tmp_path: Path) -> None:
    _module_tree(tmp_path)
    eff = RecordingEffects()
    with eff.chdir(tmp_path):
        deleted = st.delete_empty_realization_folders(eff)
    assert deleted == [
        "modules/01_macro/emptyReal",
        "modules/02_welfare/emptyB",
        "modules/02_welfare/strayfile.txt",
        "modules/03_nested/sub/gone",
    ]
    m = tmp_path / "modules"
    assert not (m / "01_macro" / "emptyReal").exists()
    assert not (m / "02_welfare" / "emptyB").exists() and not (m / "02_welfare" / "strayfile.txt").exists()
    assert not (m / "03_nested" / "sub" / "gone").exists()
    kept_paths = (
        "01_macro/singleSectorGr", "01_macro/input/.keep", "01_macro/module.gms", "02_welfare/utilitarian",
        "02_welfare/.hidden", "02_welfare/module.gms", "03_nested/sub/keepme", "03_nested/unlisted",
    )  # fmt: skip
    for kept in kept_paths:
        assert (m / kept).exists(), kept
    deletes = [c for c in eff.calls if c.method == "delete"]
    assert deletes == [FakeCall("delete", (path, True)) for path in deleted]  # unlink(recursive = TRUE)
    assert [c.args[0] for c in eff.calls if c.method == "walk"] == ["modules/"]  # find modules/ -name 'module.gms'


def test_delete_empty_realization_folders_without_modules(tmp_path: Path) -> None:
    eff = RecordingEffects()
    with eff.chdir(tmp_path):
        assert st.delete_empty_realization_folders(eff) == []
    assert not any(c.method == "delete" for c in eff.calls)


# --- system() without intern


def test_r_system_returns_the_status_and_warns_only_on_127() -> None:
    harness = FakeHarness(exits={"make": 2, "git-reset": 128})
    eff = make_effects(harness)
    with warnings.catch_warnings():
        warnings.simplefilter("error")
        assert st.r_system(st.MAKE_TEST_FULL, st.MAKE_TEST_FULL_CALL, eff) == 2
        assert st.r_system(st.GIT_RESET_PULL, st.GIT_RESET_PULL_CALL, eff) == 128
    assert harness.executed == [("make", ("test-full-slurm",), os.getcwd()), (*GIT_RESET, os.getcwd())]  # no git pull
    [run_make, run_git] = [c for c in eff.calls if c.method == "run_shell"]
    assert run_make.args[0] == st.MAKE_TEST_FULL and run_git.args[0] == st.GIT_RESET_PULL
    eff127 = RecordingEffects()  # no hook, no table: every command is "not found"
    with pytest.warns(RWarning) as record:
        assert st.r_system(st.MAKE_TEST_FULL, st.MAKE_TEST_FULL_CALL, eff127) == 127
    [w] = record
    assert (w.message.call, w.message.text) == (st.MAKE_TEST_FULL_CALL, "error in running command")  # type: ignore[union-attr]


# --------------------------------------------------------------------------- startRuns with RecordingEffects


def test_remind_start_sequence(tmp_path: Path, capsys: pytest.CaptureFixture[str]) -> None:
    mydir = make_checkout(tmp_path, "REMIND")
    root = tmp_path / "modeltests"
    before = snapshot(root)
    assert before["remind/config/default.cfg"][0] == REMIND_DEFAULT_CFG_SHA256
    harness = FakeHarness(frame=selected_frame())
    eff = make_effects(harness)
    caught = run_start("REMIND", mydir, USER, eff)
    assert caught == []
    # the commands, in R's order, with R's command strings
    assert executed_relative(harness, mydir) == REMIND_TRACE
    assert eff.shell_runs == REMIND_SHELL_RUNS
    assert all(
        c.args[1] is None for c in eff.calls if c.method == "run_shell"
    )  # cwd: the process' (chdir), never passed
    # the bridge: after the bundle start, cwd mydir, R's inputs, no suffix (the suffix is applied through amt.state)
    [(bridge_argv, bridge_cwd)] = harness.bridge_calls
    assert bridge_cwd == str(Path(mydir).resolve())
    assert bridge_argv[0] == "Rscript" and bridge_argv[1].endswith(f"/bridge_scripts/{SELECT_SCENARIOS_SCRIPT}")
    assert bridge_argv[2:] == [
        "--out", os.path.join(eff.tempdir(), st.BRIDGE_RDS_NAME),
        "--config", "config/scenario_config.csv", "--startgroup", "AMT", "--scripts", "scripts/start",
    ]  # fmt: skip
    bridge_index = next(
        i for i, (tool, argv, _) in enumerate(harness.executed) if tool == "Rscript" and argv[0] == bridge_argv[1]
    )
    assert (
        bridge_index == 4
    )  # git reset, git pull, Rscript testOneRegi, Rscript bundle, <bridge>, git reset (magpie), ...
    [bridge_run] = [c for c in eff.calls if c.method == "run" and c.args[0][0] == "Rscript"]
    assert bridge_run.args[1] == mydir and bridge_run.args[2] == dict(BRIDGE_ENV)
    # Sys.setenv(autoRenvFixDeps = "TRUE") before the first Rscript start.R
    methods = [c.method for c in eff.calls]
    assert FakeCall("setenv", ("autoRenvFixDeps", "TRUE")) in eff.calls
    assert methods.index("setenv") < methods.index("run_shell", methods.index("run_shell") + 1)
    assert eff.sleeps == []
    # the files: runsToStart.rds with -AMT row names, runcode.rds for the frozen clock, the rest untouched
    after = snapshot(root)
    assert read_runcode(f"{mydir}/runcode.rds", eff) == RUNCODE_FROZEN
    frame = read_runs_to_start(f"{mydir}/runsToStart.rds", eff)
    assert run_names(frame) == [f"{t}-AMT" for t in AMT_TITLES]
    assert list(frame.columns) == ["start", "description"]
    assert frame["description"].tolist() == [d for _, g, d in SCENARIOS if g == "AMT"]
    assert read_lastcommit(f"{mydir}/lastcommit.rds", eff) == LASTCOMMIT
    changed = {p for p in before if p in after and before[p] != after[p]}
    assert {p for p in before if p not in after} == {"remind/modules/01_macro/emptyReal"}
    assert {p for p in after if p not in before} == set()
    assert changed == {"remind/config/default.cfg", "remind/runcode.rds", "remind/runsToStart.rds"}
    # default.cfg: edited twice (FALSE -> TRUE -> FALSE), same bytes as before, "touched" in the golden
    assert after["remind/config/default.cfg"][0] == before["remind/config/default.cfg"][0] == REMIND_DEFAULT_CFG_SHA256
    assert after["remind/lastcommit.rds"] == before["remind/lastcommit.rds"]
    assert after[".testsstatus"] == before[".testsstatus"] and (root / ".testsstatus").read_text() == "next:start\n"
    writes = [c for c in eff.calls if c.method == "write_text"]
    assert [c.args[1].split("\n")[3] for c in writes] == [st.FORCE_DOWNLOAD_ON, st.FORCE_DOWNLOAD_OFF]
    # message() lines on stderr, nothing on stdout
    captured = capsys.readouterr()
    assert captured.out == ""
    assert captured.err == f"{st.MSG_TEST_ONE_REGI}\n{st.MSG_BUNDLE}\n{st.MSG_FINISHED}\n"


def test_magpie_start_sequence(tmp_path: Path, capsys: pytest.CaptureFixture[str]) -> None:
    mydir = make_checkout(tmp_path, "MAgPIE")
    root = tmp_path / "tests"
    before = snapshot(root)
    assert before["magpie/config/default.cfg"][0] == MAGPIE_DEFAULT_CFG_SHA256_BEFORE
    harness = FakeHarness()
    eff = make_effects(harness)
    assert run_start("MAgPIE", mydir, USER, eff) == []
    assert executed_relative(harness, mydir) == MAGPIE_TRACE
    assert eff.shell_runs == MAGPIE_SHELL_RUNS
    assert harness.bridge_calls == []
    assert not any(c.method == "setenv" for c in eff.calls)
    assert eff.sleeps == [300]  # Sys.sleep(300) before the one squeue check (an empty scheduler ends the loop)
    [squeue] = [c for c in eff.calls if c.method == "run"]
    assert squeue.args == (("squeue", "-u", USER, "-h", "-o", TEN), None, None, None)
    after = snapshot(root)
    assert read_runcode(f"{mydir}/runcode.rds", eff) == RUNCODE_FROZEN
    assert {p for p in before if p not in after} == {"magpie/modules/10_land/emptyReal"}
    assert {p for p in after if p not in before} == set()
    assert {p for p in before if p in after and before[p] != after[p]} == {
        "magpie/config/default.cfg",
        "magpie/runcode.rds",
    }
    assert after["magpie/config/default.cfg"][0] == MAGPIE_DEFAULT_CFG_SHA256_AFTER  # FALSE -> TRUE stays
    assert (root / "magpie" / "config" / "default.cfg").read_text() == _lines(MAGPIE_DEFAULT_CFG).replace(
        st.FORCE_DOWNLOAD_OFF, st.FORCE_DOWNLOAD_ON
    )
    captured = capsys.readouterr()
    assert (captured.out, captured.err) == ("", f"{st.MSG_FINISHED}\n")


def test_model_null_stops_before_anything_runs(tmp_path: Path) -> None:
    mydir = make_checkout(tmp_path, "REMIND")
    before = tree_hash(tmp_path)
    harness = FakeHarness(frame=selected_frame())
    eff = make_effects(harness)
    with pytest.raises(RParityError) as excinfo:
        run_start(None, mydir, USER, eff)
    assert str(excinfo.value) == "Model cannot be NULL"
    assert excinfo.value.call == st.START_RUNS_CALL
    assert harness.executed == [] and eff.shell_runs == []
    assert tree_hash(tmp_path) == before


def test_another_model_runs_the_common_steps_only(tmp_path: Path, capsys: pytest.CaptureFixture[str]) -> None:
    mydir = make_checkout(tmp_path, "REMIND")
    harness = FakeHarness(frame=selected_frame())
    eff = make_effects(harness)
    assert run_start("remind", mydir, USER, eff) == []  # neither "REMIND" nor "MAgPIE": R's if chain does nothing
    assert executed_relative(harness, mydir) == REMIND_TRACE[:2]
    assert harness.bridge_calls == [] and eff.sleeps == []
    assert read_runcode(f"{mydir}/runcode.rds", eff) == RUNCODE_FROZEN
    assert run_names(read_runs_to_start(f"{mydir}/runsToStart.rds", eff)) == ["old-AMT"]  # not rewritten
    assert (Path(mydir) / "config" / "default.cfg").read_text() == _lines(REMIND_DEFAULT_CFG).replace(
        st.FORCE_DOWNLOAD_OFF, st.FORCE_DOWNLOAD_ON
    )  # only the first sed edit
    assert not (Path(mydir) / "modules" / "01_macro" / "emptyReal").exists()
    assert capsys.readouterr().err == f"{st.MSG_FINISHED}\n"


def test_git_failure_short_circuits_the_pull_and_the_run_continues(tmp_path: Path) -> None:
    # remind-start-git-fail: git reset exits 128, sh skips `git pull`, startRuns() goes on without a warning
    mydir = make_checkout(tmp_path, "REMIND")
    harness = FakeHarness(frame=selected_frame(), exits={"git-reset": 128})
    eff = make_effects(harness)
    assert run_start("REMIND", mydir, USER, eff) == []
    assert executed_relative(harness, mydir) == [e for e in REMIND_TRACE if e[:2] != GIT_PULL]
    assert eff.shell_runs == REMIND_SHELL_RUNS  # the whole line is still handed to the shell
    assert read_runcode(f"{mydir}/runcode.rds", eff) == RUNCODE_FROZEN


def test_make_and_rscript_failures_are_ignored(tmp_path: Path) -> None:
    # remind-start-make-fail: make exits 2 and Rscript start.R exits 1; system() ignores both
    mydir = make_checkout(tmp_path, "REMIND")
    harness = FakeHarness(frame=selected_frame(), exits={"make": 2, "rscript-start": 1})
    eff = make_effects(harness)
    assert run_start("REMIND", mydir, USER, eff) == []
    assert executed_relative(harness, mydir) == REMIND_TRACE
    assert read_runcode(f"{mydir}/runcode.rds", eff) == RUNCODE_FROZEN


def test_a_missing_command_warns_like_r_and_the_run_continues(tmp_path: Path) -> None:
    mydir = make_checkout(tmp_path, "REMIND")
    harness = FakeHarness(frame=selected_frame(), exits={"make": 127, "git-pull": 127})
    eff = make_effects(harness)
    caught = run_start("REMIND", mydir, USER, eff)
    assert [(w.message.call, w.message.text) for w in caught] == [  # type: ignore[union-attr]
        (st.GIT_RESET_PULL_CALL, "error in running command"),
        (st.GIT_RESET_PULL_CALL, "error in running command"),
        (st.MAKE_TEST_FULL_CALL, "error in running command"),
    ]
    assert all(isinstance(w.message, RWarning) for w in caught)
    assert read_runcode(f"{mydir}/runcode.rds", eff) == RUNCODE_FROZEN


def test_bridge_error_aborts_before_the_state_is_written(tmp_path: Path) -> None:
    mydir = make_checkout(tmp_path, "REMIND")
    harness = FakeHarness(frame=None)  # the bridge cannot answer
    eff = make_effects(harness)
    with pytest.raises(AssertionError, match="without a frame"):
        run_start("REMIND", mydir, USER, eff)
    assert run_names(read_runs_to_start(f"{mydir}/runsToStart.rds", eff)) == ["old-AMT"]
    assert read_runcode(f"{mydir}/runcode.rds", eff) == OLD_RUNCODE
    assert not any(c.method == "write_rds" for c in eff.calls)


# --- the MAgPIE wait loop


def test_wait_loop_continues_while_a_job_runs_in_mydir(tmp_path: Path) -> None:
    mydir = make_checkout(tmp_path, "MAgPIE")
    running = f"100001 priority RUNNING 1 0:05 default 2026-09-30T00:00:00 N/A N/A {mydir}\n"
    harness = FakeHarness(
        squeue=[
            (running + "100002 x RUNNING 1 0:01 other 2026-09-30T00:00:00 N/A N/A /home/x\n", 0),
            (running, 0),
            ("", 0),
        ]
    )
    eff = make_effects(harness)
    assert run_start("MAgPIE", mydir, USER, eff) == []
    assert eff.sleeps == [300, 300, 300]  # sleep BEFORE every check, the third check sees an empty scheduler
    assert [e for e in harness.executed if e[0] == "squeue"] == [
        ("squeue", ("-u", USER, "-h", "-o", TEN), str(Path(mydir).resolve()))
    ] * 3
    assert harness.squeue == []
    assert executed_relative(harness, mydir)[-1] == MAGPIE_TRACE[-1]  # test_runs started after the wait


def test_wait_loop_bug_032_a_workdir_without_the_trailing_slash_never_matches(tmp_path: Path) -> None:
    # magpie-start-job-in-mydir: squeue's %Z has no trailing slash, the regex paste0(mydir, "$") has one
    mydir = make_checkout(tmp_path, "MAgPIE")
    workdir = mydir.rstrip("/")
    line = f"100001 priority RUNNING 1 0:05 default 2026-09-30T00:00:00 N/A N/A {workdir}\n"
    harness = FakeHarness(squeue=[(line, 0), (line, 0)])
    eff = make_effects(harness)
    assert run_start("MAgPIE", mydir, USER, eff) == []
    assert eff.sleeps == [300] and len(harness.squeue) == 1  # one check, the job is still "running"
    # without the trailing slash the same job keeps the loop waiting (parity: the regex is mydir as given)
    harness2 = FakeHarness(squeue=[(line, 0), (line, 0), ("", 0)])
    eff2 = make_effects(harness2)
    with eff2.chdir(workdir):
        assert st.wait_for_default_run(workdir, USER, eff2) == 3
    assert eff2.sleeps == [300, 300, 300]


def test_wait_loop_regex_is_r_s(tmp_path: Path) -> None:
    # mydir is a TRE regex: "." matches any character; a pattern TRE rejects is R's error
    eff = make_effects(FakeHarness(squeue=[("1 q R 1 0:01 j t n n /p/x/a.b/\n", 0), ("", 0)]))
    assert st.wait_for_default_run("/p/x/a.b/", USER, eff) == 2  # "." matched "."
    eff = make_effects(FakeHarness(squeue=[("1 q R 1 0:01 j t n n /p/x/aXb/\n", 0), ("", 0)]))
    assert st.wait_for_default_run("/p/x/a.b/", USER, eff) == 2  # "." matched "X" as well
    eff = make_effects(FakeHarness(squeue=[("1 q R 1 0:01 j t n n /p/x/\n", 0)]))
    with warnings.catch_warnings(record=True), pytest.raises(RParityError) as excinfo:
        warnings.simplefilter("always")
        st.wait_for_default_run("/p/x(", USER, eff)
    assert str(excinfo.value).startswith("invalid regular expression '/p/x($', reason ")


def test_squeue_failure_warns_and_ends_the_wait() -> None:
    eff = make_effects(FakeHarness(squeue=[("", 1)]))
    with pytest.warns(RWarning) as record:
        assert st.wait_for_default_run("/p/x/", USER, eff) == 1
    [w] = record
    assert w.message.call == st.SQUEUE_WAIT_CALL  # type: ignore[union-attr]
    assert w.message.text == f"running command 'squeue -u {USER} -h -o '{TEN}'' had status 1"  # type: ignore[union-attr]
    assert eff.sleeps == [300]


def test_squeue_that_cannot_run_is_r_s_error() -> None:
    eff = make_effects(FakeHarness(squeue=[("", 127)]))
    with pytest.raises(RParityError, match="^error in running command$"):
        st.wait_for_default_run("/p/x/", USER, eff)


def test_wait_loop_with_user_none_pastes_nothing() -> None:
    harness = FakeHarness(squeue=[("", 0)])
    eff = make_effects(harness, user=None)
    assert st.wait_for_default_run("/p/x/", None, eff) == 1
    assert harness.executed == [("squeue", ("-u", "-h", "-o", TEN), os.getcwd())]  # what sh makes of "squeue -u  -h"


# --- dry run


def test_dry_run_writes_nothing_and_logs_every_mutation(tmp_path: Path, monkeypatch: pytest.MonkeyPatch) -> None:
    # the dry run applies Sys.setenv for real (p5-effects): a recorded setenv + delenv restores the absence at teardown
    monkeypatch.setenv("autoRenvFixDeps", "placeholder")
    monkeypatch.delenv("autoRenvFixDeps")
    mydir = make_checkout(tmp_path, "REMIND")
    before = tree_hash(tmp_path)
    harness = FakeHarness(frame=selected_frame())
    lines: list[str] = []
    eff = DryRunEffects(report=lines.append, answer=lambda tokens: harness(list(tokens), os.getcwd()))
    with warnings.catch_warnings(record=True) as caught, eff.chdir(mydir):
        warnings.simplefilter("always")
        st.start_runs("REMIND", mydir, USER, effects=eff)
    assert caught == []
    assert tree_hash(tmp_path) == before  # the bridge wrote into tempdir(), the state writes were only logged
    assert (Path(mydir) / "modules" / "01_macro" / "emptyReal").is_dir()
    resolved = str(Path(mydir).resolve())
    cfg_bytes = len(_lines(REMIND_DEFAULT_CFG).encode())
    assert lines == [
        f"would run {st.GIT_RESET_PULL} (cwd {resolved}, answered status 0)",
        f"would write config/default.cfg ({cfg_bytes - 1} bytes, atomic)",  # TRUE is one byte shorter than FALSE
        "would delete modules/01_macro/emptyReal (recursive)",
        "would set autoRenvFixDeps=TRUE",
        f"would run {st.REMIND_TEST_ONE_REGI} (cwd {resolved}, answered status 0)",
        f"would write config/default.cfg ({cfg_bytes} bytes, atomic)",  # the file still holds FALSE: nothing to edit
        f"would run {st.REMIND_BUNDLE} (cwd {resolved}, answered status 0)",
        lines[7],
        lines[8],
        f"would run {st.GIT_RESET_PULL} (cwd {resolved}/magpie, answered status 0)",
        f"would run {st.MAKE_TEST_FULL} (cwd {resolved}, answered status 0)",
        lines[11],
    ]
    assert lines[7].startswith("would run Rscript ") and f"{SELECT_SCENARIOS_SCRIPT} --out " in lines[7]
    assert lines[8].startswith(f"would write {mydir}/runsToStart.rds (RDS, ")
    assert lines[11].startswith(f"would write {mydir}/runcode.rds (RDS, ")
    runcode_event = eff.events[-1]
    assert runcode_event.data is not None and len(runcode_event.data) > 0
    assert os.environ.get("autoRenvFixDeps") == "TRUE"  # Sys.setenv is applied for real in a dry run (p5-effects)


def test_module_exports() -> None:
    for name in st.__all__:
        assert hasattr(st, name), name


# --------------------------------------------------------------------------- the five start cases against the R goldens


def _golden_dir(case: str) -> Path:
    path = AMT_GOLDENS / case
    if not (path / "trace.jsonl").is_file() or not (AMT_CASES / case / "case.json").is_file():
        pytest.skip(f"R golden {path} or its case directory is absent (no migration/ tree)")
    return path


def golden_trace(case: str) -> list[dict[str, Any]]:
    with (AMT_GOLDENS / case / "trace.jsonl").open(encoding="utf-8") as handle:
        return [json.loads(line) for line in handle if line.strip()]


def golden_trace_relative(case: str, case_mydir: str) -> list[tuple[str, tuple[str, ...], str]]:
    """``(tool, argv, cwd relative to mydir)`` of the R trace without the ``sed`` calls (the documented exception)."""
    base = case_mydir.rstrip("/")
    out = []
    for entry in golden_trace(case):
        if entry["tool"] == "sed":
            continue
        cwd = entry["cwd"]
        assert cwd == base or cwd.startswith(base + "/"), cwd
        out.append((entry["tool"], tuple(entry["argv"]), cwd[len(base) :].lstrip("/")))
    return out


def _trace_key(entry: Mapping[str, Any]) -> tuple[str, tuple[str, ...], str]:
    return entry["tool"], tuple(entry["argv"]), entry["cwd"]


def golden_state(case: str, name: str) -> dict[str, Any]:
    data: dict[str, Any] = json.loads((AMT_GOLDENS / case / "state" / name).read_text(encoding="utf-8"))
    return data


def case_exits(case: str) -> dict[str, int]:
    """``<case>/<name>.exit`` of migration/cases/amt/<case>: what the fake binaries exit with."""
    return {p.stem: int(p.read_text().strip() or 0) for p in (AMT_CASES / case).glob("*.exit")}


def slurm_case_dir(name: str) -> Path | None:
    if name == "recorded":
        return SLURM_RECORDED if SLURM_RECORDED.is_dir() else None  # the capture never lists a job in mydir
    path = SLURM_CASES / name
    if not path.is_dir():
        pytest.skip(f"slurm case {path} is absent")
    return path


def _sed_entries(case: str) -> list[list[str]]:
    return [e["argv"] for e in golden_trace(case) if e["tool"] == "sed"]


@pytest.mark.parametrize("case", START_CASES)
def test_start_case_against_the_r_golden(case: str, tmp_path: Path, capsys: pytest.CaptureFixture[str]) -> None:
    golden = _golden_dir(case)
    spec = json.loads((AMT_CASES / case / "case.json").read_text(encoding="utf-8"))
    model, case_mydir, user = spec["model"], spec["mydir"], spec["user"]
    mydir = make_checkout(tmp_path, model)
    root = Path(mydir).resolve().parent
    before = snapshot(root)
    harness = FakeHarness(frame=selected_frame() if model == "REMIND" else None, exits=case_exits(case))
    eff = make_effects(harness, user=user, slurm_case_dir=slurm_case_dir(spec["slurm_case"]))
    caught = run_start(model, mydir, user, eff)
    assert caught == [] and json.loads((golden / "result.json").read_text())["status"] == "ok"
    # 1. the commands: the R trace minus sed (which the port replaces by the file edit) equals what ran, in order
    assert executed_relative(harness, mydir) == golden_trace_relative(case, case_mydir)
    assert _sed_entries(case) == [
        ["-i", f"s/{st.FORCE_DOWNLOAD_OFF}/{st.FORCE_DOWNLOAD_ON}/", st.DEFAULT_CFG],
        *([["-i", f"s/{st.FORCE_DOWNLOAD_ON}/{st.FORCE_DOWNLOAD_OFF}/", st.DEFAULT_CFG]] if model == "REMIND" else []),
    ]
    assert eff.shell_runs == (REMIND_SHELL_RUNS if model == "REMIND" else MAGPIE_SHELL_RUNS)
    # 2. the bridge call (invisible to the fake Rscript, hence absent from the R trace) replaces lines 127-136
    if model == "REMIND":
        [(bridge_argv, bridge_cwd)] = harness.bridge_calls
        assert bridge_cwd == str(Path(mydir).resolve()) and bridge_argv[1].endswith("/" + SELECT_SCENARIOS_SCRIPT)
        assert bridge_argv[2:] == [
            "--out", os.path.join(eff.tempdir(), st.BRIDGE_RDS_NAME), "--config", "config/scenario_config.csv",
            "--startgroup", "AMT", "--scripts", "scripts/start",
        ]  # fmt: skip
    else:
        assert harness.bridge_calls == []
    # 3. the state files by value (state/*.json of the golden)
    assert (
        read_runcode(f"{mydir}/runcode.rds", eff)
        == golden_state(case, "runcode_rds.json")["value"][0]
        == RUNCODE_FROZEN
    )
    assert read_lastcommit(f"{mydir}/lastcommit.rds", eff) == LASTCOMMIT
    assert golden_state(case, "lastcommit_rds.json")["exists"] is True
    if model == "REMIND":
        frame = read_runs_to_start(f"{mydir}/runsToStart.rds", eff)
        rows = [{"_row": name, **{c: str(frame.at[name, c]) for c in frame.columns}} for name in run_names(frame)]
        assert rows == golden_state(case, "runsToStart_rds.json")["value"]["rows"]
    # 4. effects.json: the clone diff relative to mydir (the .testsstatus write is modeltests()'s, not startRuns()'s)
    effects_json = json.loads((golden / "effects.json").read_text(encoding="utf-8"))
    prefix = case_mydir.rstrip("/").removeprefix(effects_json["fixture_root"] + "/") + "/"

    def rel(path: str) -> str:
        assert path.startswith(prefix), path
        return path[len(prefix) :]

    after = snapshot(root)
    local = Path(mydir).resolve().relative_to(root).as_posix() + "/"
    deleted = {p[len(local) :] for p in before if p not in after and p.startswith(local)}
    created = {p[len(local) :] for p in after if p not in before and p.startswith(local)}
    modified = {p[len(local) :] for p in before if p in after and before[p][0] != after[p][0] and p.startswith(local)}
    touched = {
        p[len(local) :]
        for p in before
        if p in after and before[p] != after[p] and before[p][0] == after[p][0] and p.startswith(local)
    }
    outside = {p: before.get(p) for p in set(before) | set(after) if not p.startswith(local)}
    assert all(before.get(p) == after.get(p) for p in outside), "nothing above mydir may change"
    assert deleted == {rel(e["path"]) for e in effects_json["deleted"]}
    assert all(e["kind"] == "dir" for e in effects_json["deleted"])
    assert created == set() == set(effects_json["created"])
    golden_modified = {rel(e["path"]): e for e in effects_json["modified"] if not e["path"].endswith("/.testsstatus")}
    assert modified == set(golden_modified)
    for path, entry in golden_modified.items():
        if not path.endswith(".rds"):  # RDS bytes differ between writers by design; compared by value above
            assert after[local + path][0] == entry["sha256"], path
    golden_touched = {rel(e["path"]): e for e in effects_json["touched"]}
    assert touched == set(golden_touched)
    for path, entry in golden_touched.items():
        assert after[local + path][0] == entry["sha256"] == before[local + path][0], path
    assert (root / ".testsstatus").read_text() == "next:start\n"
    assert json.loads((golden / "unchanged.json").read_text())["lastcommit.rds"]["same"] is True
    # 5. the message() lines are a contiguous part of the golden's stderr; nothing on stdout
    captured = capsys.readouterr()
    assert captured.out == "" and (golden / "stdout.txt").read_bytes() == b""
    assert captured.err in (golden / "stderr.txt").read_text(encoding="utf-8")
    assert eff.sleeps == ([] if model == "REMIND" else [300])


def test_the_fake_checkout_equals_the_synthetic_layer() -> None:
    """The embedded default.cfg lines are the synthetic layer's (sha256 pinned by the goldens' effects.json)."""
    assert hashlib.sha256(_lines(REMIND_DEFAULT_CFG).encode()).hexdigest() == REMIND_DEFAULT_CFG_SHA256
    assert hashlib.sha256(_lines(MAGPIE_DEFAULT_CFG).encode()).hexdigest() == MAGPIE_DEFAULT_CFG_SHA256_BEFORE
    edited = _lines(MAGPIE_DEFAULT_CFG).replace(st.FORCE_DOWNLOAD_OFF, st.FORCE_DOWNLOAD_ON)
    assert hashlib.sha256(edited.encode()).hexdigest() == MAGPIE_DEFAULT_CFG_SHA256_AFTER
    synthetic = MIGRATION / "synthetic/p/projects/remind/modeltests/remind/config/default.cfg"
    if synthetic.is_file():
        assert synthetic.read_bytes() == _lines(REMIND_DEFAULT_CFG).encode()
    for case in ("remind-start", "magpie-start"):
        golden = AMT_GOLDENS / case / "effects.json"
        if not golden.is_file():
            continue
        entries = json.loads(golden.read_text())
        [cfg] = [e for e in entries["touched"] + entries["modified"] if e["path"].endswith("config/default.cfg")]
        assert cfg["sha256"] == (
            REMIND_DEFAULT_CFG_SHA256 if case == "remind-start" else MAGPIE_DEFAULT_CFG_SHA256_AFTER
        )


# --------------------------------------------------------------------------- inside the sandbox (real fake binaries)


def _sandbox_skip_reason() -> str | None:
    if not SANDBOX.is_file():
        return f"{SANDBOX} is absent (no migration/ tree)"
    if not FIXTURES_PROBE.is_dir():
        return f"fixture tree {FIXTURES_PROBE} is absent"
    if not FAKE_REMIND_PROBE.is_dir():
        return f"fake REMIND checkout {FAKE_REMIND_PROBE} is absent (run migration/harness/build_fake_remind.sh)"
    if shutil.which("bwrap") is None:
        return "bwrap (bubblewrap) not installed"
    if shutil.which("Rscript") is None:
        return "Rscript not on PATH"
    if not VENV_PYTHON.exists():
        return f"{VENV_PYTHON} is absent (run uv sync)"
    return None


def _run_sandbox(case_dir: Path, case: str) -> dict[str, Any]:
    spec = json.loads((AMT_CASES / case / "case.json").read_text(encoding="utf-8"))
    case_dir.mkdir(parents=True, exist_ok=True)
    cmd = [
        str(SANDBOX), str(case_dir), "--mode", spec["mode"], "--quiet", "--amt-case", case,
        "--slurm-case", spec["slurm_case"], "--user", spec["user"], "--env", "PYTHONDONTWRITEBYTECODE=1",
        "--", str(VENV_PYTHON), str(Path(__file__).resolve()), "--sandbox-driver", case, "--out", "/out/report.json",
    ]  # fmt: skip
    log = case_dir / "sandbox.log"
    with log.open("wb") as handle:
        proc = subprocess.run(cmd, cwd=REPO, stdout=handle, stderr=subprocess.STDOUT, timeout=900, check=False)
    report_path = case_dir / "out" / "report.json"
    if proc.returncode != 0 or not report_path.is_file():
        tail = log.read_text(encoding="utf-8", errors="replace")[-4000:]
        pytest.fail(f"sandbox driver {case} failed with status {proc.returncode} (log {log}):\n{tail}")
    report: dict[str, Any] = json.loads(report_path.read_text(encoding="utf-8"))
    report["_case_dir"] = str(case_dir)
    return report


@pytest.fixture(scope="session")
def sandbox_reports(tmp_path_factory: pytest.TempPathFactory) -> dict[str, dict[str, Any]]:
    reason = _sandbox_skip_reason()
    if reason is not None:
        pytest.skip(f"start sandbox tests skipped: {reason}")
    root = tmp_path_factory.mktemp("p5-start")
    return {case: _run_sandbox(root / case, case) for case in SANDBOX_CASES}


@pytest.mark.parametrize("case", SANDBOX_CASES)
def test_sandbox_start_case(case: str, sandbox_reports: dict[str, dict[str, Any]]) -> None:
    rep = sandbox_reports[case]
    golden = _golden_dir(case)
    case_dir = Path(rep["_case_dir"])
    assert rep["error"] is None, rep["error"]
    assert rep["warnings"] == []
    # the fake binaries' trace: R's minus sed, with the same argv and cwd; no sed line on the Python side
    with (case_dir / "out" / "trace.jsonl").open(encoding="utf-8") as handle:
        py_trace = [json.loads(line) for line in handle if line.strip()]
    assert [e["tool"] for e in py_trace if e["tool"] == "sed"] == []
    assert [_trace_key(e) for e in py_trace] == [_trace_key(e) for e in golden_trace(case) if e["tool"] != "sed"]
    golden_entries = [e for e in golden_trace(case) if e["tool"] != "sed"]
    assert ["autoRenvFixDeps" in e["env"] for e in py_trace] == ["autoRenvFixDeps" in e["env"] for e in golden_entries]
    # the clone diff against effects.json (.testsstatus is modeltests()'s write, not startRuns()'s)
    diff = json.loads((case_dir / "clone-diff.json").read_text(encoding="utf-8"))
    effects_json = json.loads((golden / "effects.json").read_text(encoding="utf-8"))
    assert [e["path"] for e in diff["deleted"]] == [e["path"] for e in effects_json["deleted"]]
    assert diff["created"] == [] == effects_json["created"]
    golden_modified = {e["path"]: e for e in effects_json["modified"] if not e["path"].endswith("/.testsstatus")}
    assert {e["path"] for e in diff["modified"]} == set(golden_modified)
    for e in diff["modified"]:
        if not e["path"].endswith(".rds"):
            assert e["sha256"] == golden_modified[e["path"]]["sha256"], e["path"]
    assert {e["path"]: e["sha256"] for e in diff["touched"]} == {
        e["path"]: e["sha256"] for e in effects_json["touched"]
    }
    # the state by value
    assert rep["runcode"] == golden_state(case, "runcode_rds.json")["value"]
    assert rep["lastcommit_before"] == rep["lastcommit_after"] == golden_state(case, "lastcommit_rds.json")["sha256"]
    if rep["model"] == "REMIND":
        assert rep["runs_to_start"] == golden_state(case, "runsToStart_rds.json")["value"]["rows"]
    assert rep["testsstatus"] == "next:start\n"
    assert rep["messages"] in (golden / "stderr.txt").read_text(encoding="utf-8")
    assert rep["sleeps"] == ([] if rep["model"] == "REMIND" else [300])


# --------------------------------------------------------------------------- driver mode (runs inside the sandbox)


def _driver(case: str, out_file: str) -> int:
    spec = json.loads(Path(f"/opt/cases/amt/{case}/case.json").read_text(encoding="utf-8"))
    mydir, model, user = spec["mydir"], spec["model"], spec["user"]
    eff = RecordingEffects(user=user, on_cluster=True, delegate_run=True)
    lastcommit = Path(mydir) / "lastcommit.rds"
    report: dict[str, Any] = {"case": case, "model": model, "mydir": mydir, "lastcommit_before": _sha256(lastcommit)}
    stderr = io.StringIO()
    error: str | None = None
    with warnings.catch_warnings(record=True) as caught:
        warnings.simplefilter("always")
        with contextlib.redirect_stderr(stderr), eff.chdir(mydir):
            try:
                st.start_runs(model, mydir, user, effects=eff)
            except Exception as exc:  # noqa: BLE001 - reported, the test fails on it
                error = repr(exc)
    report["error"] = error
    report["warnings"] = [[getattr(w.message, "call", None), str(w.message)] for w in caught]
    report["messages"] = stderr.getvalue()
    report["sleeps"] = eff.sleeps
    report["shell_runs"] = eff.shell_runs
    report["lastcommit_after"] = _sha256(lastcommit)
    report["runcode"] = [str(v) for v in read_rds(f"{mydir}/runcode.rds")]
    if model == "REMIND":
        frame = read_runs_to_start(f"{mydir}/runsToStart.rds", eff)
        report["runs_to_start"] = [
            {"_row": name, **{c: str(frame.at[name, c]) for c in frame.columns}} for name in run_names(frame)
        ]
    report["testsstatus"] = (Path(mydir) / ".." / ".testsstatus").read_text(encoding="utf-8")
    Path(out_file).write_text(json.dumps(report, indent=2, ensure_ascii=False), encoding="utf-8")
    return 0


if __name__ == "__main__":
    _argv = sys.argv[1:]
    sys.exit(_driver(_argv[_argv.index("--sandbox-driver") + 1], _argv[_argv.index("--out") + 1]))
