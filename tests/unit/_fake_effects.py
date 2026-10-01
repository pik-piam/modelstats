"""FakeEffects: the test double of ``modelstats.env.Effects`` (unit tests only, not in the package).

Filesystem reads go to the real path (the fixtures are real files); the clock is frozen, the
user and the cluster probe are injected, ``run()`` answers ``squeue`` / ``sacct`` from a case
directory exactly like the harness' fake binaries (``migration/harness/fakebin/_slurm.py``:
a synthetic case ``migration/cases/slurm/<name>/`` or the recorded capture
``fixtures/_meta/slurm/latest/``), and every call is logged in ``calls``.

Case directory layout (all optional except ``squeue_all.txt``):

- ``squeue_all.txt``: ``squeue -h -o '%u %Z %j %M %T %q'`` for all users; the per-user ``%Z``,
  ``%j`` and ten-field answers are derived from it unless ``squeue_Z_<user>.txt``,
  ``squeue_j_<user>.txt``, ``squeue_six_<user>.txt`` or ``squeue_ten_<user>.txt`` override them;
  ``squeue_ten_all.txt`` (or the capture's ``02.txt``) feeds the ten-field format;
- ``sacct_WorkDir_<user>.txt``, ``sacct_JobName_<user>.txt``: sacct answers (empty otherwise);
- ``squeue_exit`` / ``sacct_exit``: exit status of every call (nothing on stdout, a message
  on stderr); ``squeue_sequence`` / ``sacct_sequence``: one status per line consumed per call
  (the last line repeats);
- a recorded capture: ``NN.txt.cmd`` holding the command, ``NN.txt`` its output; a request is
  matched token by token (``sacct -S <date>`` wildcarded).
"""

from __future__ import annotations

import datetime as dt
import os
import re
import shlex
import subprocess
import zoneinfo
from collections.abc import Mapping, Sequence
from dataclasses import dataclass, field
from pathlib import Path

from modelstats.env import PathLike, ProductionEffects

__all__ = ["DEFAULT_FROZEN", "FakeCall", "FakeEffects", "SIX", "TEN"]

SIX = "%u %Z %j %M %T %q"
TEN = "%i %q %T %C %M %j %V %L %e %Z"

#: The harness' default stopped clock (epoch 1790762400): 2026-09-30 12:00:00 Europe/Berlin.
DEFAULT_FROZEN = dt.datetime.fromtimestamp(1790762400, tz=zoneinfo.ZoneInfo("Europe/Berlin"))


@dataclass(frozen=True)
class FakeCall:
    """One logged call: the method name and its positional arguments."""

    method: str
    args: tuple[object, ...] = ()


@dataclass
class _SlurmFake:
    """Port of ``migration/harness/fakebin/_slurm.py`` with the per-call counter kept in memory."""

    case_dir: Path | None
    counters: dict[str, int] = field(default_factory=dict)

    def answer(self, tool: str, argv: Sequence[str]) -> tuple[str, str, int]:
        user, fmt, _rest = _parse_args(argv)
        status = self._exit_control(tool) if self.case_dir is not None else 0
        if status != 0:
            return "", f"{tool}: error: fake {tool} failure (exit {status})\n", status
        if self.case_dir is None or not self.case_dir.is_dir():
            return "", "", 0  # no case: empty scheduler
        recorded = self._recorded_match(tool, argv)
        if recorded is not None:
            return _read(recorded), "", 0
        if tool == "sacct":
            path = self._find(f"sacct_{(fmt or '').strip()}_{user}.txt")
            return (_read(path) if path else ""), "", 0
        all_six = self._find("squeue_all.txt", "01.txt")
        six = _six_rows(_read_lines(all_six)) if all_six else []
        mine = [row for row in six if user is None or row["user"] == user]
        if fmt == SIX:
            if user is not None and (path := self._find(f"squeue_six_{user}.txt")):
                return _read(path), "", 0
            lines = [f"{r['user']} {r['workdir']} {r['jobname']} {r['elapsed']} {r['state']} {r['qos']}" for r in mine]
            return _joined(lines), "", 0
        if fmt == "%Z":
            if path := self._find(f"squeue_Z_{user}.txt"):
                return _read(path), "", 0
            return _joined([r["workdir"] for r in mine]), "", 0
        if fmt == "%j":
            if path := self._find(f"squeue_j_{user}.txt"):
                return _read(path), "", 0
            return _joined([r["jobname"] for r in mine]), "", 0
        if fmt == TEN:
            if path := self._find(f"squeue_ten_{user}.txt"):
                return _read(path), "", 0
            ten_all = self._find("squeue_ten_all.txt", "02.txt")
            if ten_all:
                keys = {(r["jobname"], r["workdir"]) for r in mine}
                rows = _ten_rows(_read_lines(ten_all))
                return _joined([r["line"] for r in rows if user is None or (r["jobname"], r["workdir"]) in keys]), "", 0
            lines = [
                f"{100000 + i} {r['qos']} {r['state']} 1 {r['elapsed']} {r['jobname']} "
                f"2026-09-30T00:00:00 N/A N/A {r['workdir']}"
                for i, r in enumerate(mine)
            ]
            return _joined(lines), "", 0
        return "", f"fake squeue: unsupported format {fmt!r} (args {list(argv)})\n", 1

    def _find(self, *names: str) -> Path | None:
        assert self.case_dir is not None
        for name in names:
            path = self.case_dir / name
            if path.is_file():
                return path
        return None

    def _exit_control(self, tool: str) -> int:
        if sequence := self._find(f"{tool}_sequence"):
            statuses = [int(x) for x in _read_lines(sequence) if x.strip()]
            n = self.counters.get(tool, 0)
            self.counters[tool] = n + 1
            if statuses:
                return statuses[min(n, len(statuses) - 1)]
        if exit_file := self._find(f"{tool}_exit"):
            return int(_read(exit_file).strip() or 0)
        return 0

    def _recorded_match(self, tool: str, argv: Sequence[str]) -> Path | None:
        assert self.case_dir is not None
        want = [tool, *argv]
        for name in sorted(os.listdir(self.case_dir)):
            if not name.endswith(".cmd"):
                continue
            text = _read_lines(self.case_dir / name)
            if not text:
                continue
            cmd = re.split(r" {3,}", text[0], maxsplit=1)[0].strip()
            try:
                have = shlex.split(cmd)
            except ValueError:
                continue
            if len(have) != len(want) or have[0] != want[0]:
                continue
            ok = True
            for i, (h, w) in enumerate(zip(have, want, strict=True)):
                if h != w and not (tool == "sacct" and i > 0 and have[i - 1] == "-S"):
                    ok = False
                    break
            if ok:
                data = self.case_dir / name[:-4]
                if data.is_file():
                    return data
        return None


def _parse_args(argv: Sequence[str]) -> tuple[str | None, str | None, list[str]]:
    user, fmt, rest = None, None, []
    i = 0
    while i < len(argv):
        a = argv[i]
        if a == "-u" and i + 1 < len(argv):
            user = argv[i + 1]
            i += 2
        elif a.startswith("--user="):
            user = a.split("=", 1)[1]
            i += 1
        elif a == "-o" and i + 1 < len(argv):
            fmt = argv[i + 1]
            i += 2
        elif a.startswith("--format=") or a.startswith("-o"):
            fmt = a.split("=", 1)[1] if "=" in a else a[2:]
            i += 1
        elif a == "--format" and i + 1 < len(argv):
            fmt = argv[i + 1]
            i += 2
        else:
            rest.append(a)
            i += 1
    return user, fmt, rest


def _read(path: Path) -> str:
    return path.read_bytes().decode("utf-8", "replace")


def _read_lines(path: Path) -> list[str]:
    return _read(path).splitlines()


def _joined(lines: list[str]) -> str:
    return "".join(line + "\n" for line in lines)


def _six_rows(lines: list[str]) -> list[dict[str, str]]:
    rows = []
    for ln in lines:
        f = ln.split(" ")
        if len(f) < 6:
            continue
        rows.append(
            {
                "user": f[0],
                "workdir": f[1],
                "jobname": " ".join(f[2:-3]),
                "elapsed": f[-3],
                "state": f[-2],
                "qos": f[-1],
            }
        )
    return rows


def _ten_rows(lines: list[str]) -> list[dict[str, str]]:
    rows = []
    for ln in lines:
        f = ln.split(" ")
        if len(f) < 10:
            continue
        rows.append({"jobname": " ".join(f[5:-4]), "workdir": f[-1], "line": ln})
    return rows


class FakeEffects(ProductionEffects):
    """Effects with a frozen clock, an injected identity and canned scheduler answers.

    ``run_table`` maps an exact argv tuple to ``(stdout, returncode)`` and wins over the
    scheduler fake; any other command is answered like a missing executable (status 127).
    ``env`` replaces the process environment for ``getenv`` when given.
    """

    def __init__(
        self,
        *,
        now: dt.datetime = DEFAULT_FROZEN,
        user: str = "pascalfu",
        on_cluster: bool = False,
        slurm_case_dir: PathLike | None = None,
        env: Mapping[str, str] | None = None,
        run_table: Mapping[tuple[str, ...], tuple[str, int]] | None = None,
    ) -> None:
        if now.tzinfo is None:
            raise ValueError("now must be an aware datetime")
        self._now = now
        self._user = user
        self._on_cluster = on_cluster
        self._env = None if env is None else dict(env)
        self._run_table = dict(run_table or {})
        self._slurm = _SlurmFake(None if slurm_case_dir is None else Path(slurm_case_dir))
        self.calls: list[FakeCall] = []

    # -- logging ------------------------------------------------------------------

    def _log(self, method: str, *args: object) -> None:
        self.calls.append(FakeCall(method, args))

    @property
    def runs(self) -> list[tuple[str, ...]]:
        """The argv of every ``run()`` call, in order."""
        return [tuple(call.args[0]) for call in self.calls if call.method == "run"]  # type: ignore[arg-type]

    # -- filesystem: real reads, logged -----------------------------------------------

    def listdir_like_r(self, directory: PathLike) -> list[str]:
        self._log("listdir_like_r", os.fspath(directory))
        return super().listdir_like_r(directory)

    def exists(self, path: PathLike) -> bool:
        self._log("exists", os.fspath(path))
        return super().exists(path)

    def is_dir(self, path: PathLike) -> bool:
        self._log("is_dir", os.fspath(path))
        return super().is_dir(path)

    def stat(self, path: PathLike):  # type: ignore[no-untyped-def]
        self._log("stat", os.fspath(path))
        return super().stat(path)

    def read_text(self, path: PathLike) -> str:
        self._log("read_text", os.fspath(path))
        return super().read_text(path)

    def read_bytes(self, path: PathLike) -> bytes:
        self._log("read_bytes", os.fspath(path))
        return super().read_bytes(path)

    def glob(self, pattern: PathLike) -> list[str]:
        self._log("glob", os.fspath(pattern))
        return super().glob(pattern)

    # -- canned environment ---------------------------------------------------------

    def run(
        self,
        argv: Sequence[str],
        cwd: PathLike | None = None,
        env: Mapping[str, str] | None = None,
        input: str | None = None,
    ) -> subprocess.CompletedProcess[str]:
        argv = list(argv)
        self._log(
            "run", tuple(argv), None if cwd is None else os.fspath(cwd), None if env is None else dict(env), input
        )
        key = tuple(argv)
        if key in self._run_table:
            stdout, status = self._run_table[key]
            return subprocess.CompletedProcess(argv, status, stdout, "")
        tool = os.path.basename(argv[0]) if argv else ""
        if tool in ("squeue", "sacct"):
            stdout, stderr, status = self._slurm.answer(tool, argv[1:])
            return subprocess.CompletedProcess(argv, status, stdout, stderr)
        return subprocess.CompletedProcess(argv, 127, "", f"sh: {tool}: command not found\n")

    def now(self) -> dt.datetime:
        self._log("now")
        return self._now

    def today(self) -> dt.date:
        self._log("today")
        return self._now.date()

    @property
    def user(self) -> str:
        self._log("user")
        return self._user

    @property
    def on_cluster(self) -> bool:
        self._log("on_cluster")
        return self._on_cluster

    def getenv(self, name: str, default: str = "") -> str:
        self._log("getenv", name)
        if self._env is None:
            return super().getenv(name, default)
        return self._env.get(name, default)
