"""RecordingEffects (alias FakeEffects): the test double of ``modelstats.env.Effects`` (unit tests only).

Filesystem reads and writes go to the real path (the fixtures are real files; tests copy what
they mutate into ``tmp_path``); the clock is frozen, the user and the cluster probe are
injected, ``run()`` answers from a table, a hook, the real subprocess (``delegate_run``) or the
built-in scheduler fake, ``post_json()`` never opens a connection, ``sleep()`` never waits, and
every call is logged in ``calls`` (``FakeCall(method, args)``). ``trace`` lists every command
the way the harness' fake binaries trace it (``tool``, ``argv``, ``cwd``, one entry per simple
command of a shell line), ``posts`` the webhook payloads, ``sleeps`` the requested seconds.

The scheduler fake mirrors ``migration/harness/fakebin/_slurm.py``: a synthetic case
``migration/cases/slurm/<name>/`` or the recorded capture ``fixtures/_meta/slurm/latest/``.
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

``run()`` answer order: ``run_table`` (keyed by the argv tuple, or for a shell line by
``(command,)`` and by its token tuple), then ``run_hook(tokens, cwd)``, then the real
subprocess when ``delegate_run`` is set (so the sandbox's fake binaries see the call), then the
scheduler fake for ``squeue`` / ``sacct``, else status 127 like a missing executable.
"""

from __future__ import annotations

import contextlib
import datetime as dt
import os
import re
import shlex
import subprocess
import zoneinfo
from collections.abc import Callable, Iterator, Mapping, Sequence
from dataclasses import dataclass, field
from pathlib import Path

from modelstats.env import FileStat, PathLike, ProductionEffects, shell_segments, shell_tokens

__all__ = ["DEFAULT_FROZEN", "FakeCall", "FakeEffects", "RecordingEffects", "SIX", "TEN", "TraceEntry"]

SIX = "%u %Z %j %M %T %q"
TEN = "%i %q %T %C %M %j %V %L %e %Z"

#: The harness' default stopped clock (epoch 1790762400): 2026-09-30 12:00:00 Europe/Berlin.
DEFAULT_FROZEN = dt.datetime.fromtimestamp(1790762400, tz=zoneinfo.ZoneInfo("Europe/Berlin"))

type RunAnswer = tuple[str, int]
type RunHook = Callable[[list[str], str], RunAnswer | None]


@dataclass(frozen=True)
class FakeCall:
    """One logged call: the method name and its positional arguments."""

    method: str
    args: tuple[object, ...] = ()


@dataclass(frozen=True)
class TraceEntry:
    """One command as the harness' fake binaries trace it: the tool, its arguments and the working directory."""

    tool: str
    argv: tuple[str, ...]
    cwd: str


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


def _fspath(path: PathLike | None) -> str | None:
    return None if path is None else os.fspath(path)


class RecordingEffects(ProductionEffects):
    """Effects with a frozen clock, an injected identity, canned answers and a log of every call.

    ``run_table`` maps an exact argv tuple (or ``(command,)`` for a shell line) to
    ``(stdout, returncode)`` and wins over everything else; ``run_hook(tokens, cwd)`` may answer
    dynamically; ``delegate_run`` hands unanswered commands to the real subprocess; otherwise
    ``squeue`` / ``sacct`` come from the scheduler fake and any other command is answered like
    a missing executable (status 127). ``env`` replaces the process environment for
    ``getenv`` / ``setenv`` when given. ``post_json_response`` is what every POST returns.
    """

    def __init__(
        self,
        *,
        now: dt.datetime = DEFAULT_FROZEN,
        user: str = "pascalfu",
        on_cluster: bool = False,
        slurm_case_dir: PathLike | None = None,
        env: Mapping[str, str] | None = None,
        run_table: Mapping[tuple[str, ...], RunAnswer] | None = None,
        run_hook: RunHook | None = None,
        delegate_run: bool = False,
        post_json_response: tuple[int, str] = (200, "ok"),
    ) -> None:
        if now.tzinfo is None:
            raise ValueError("now must be an aware datetime")
        self._now = now
        self._user = user
        self._on_cluster = on_cluster
        self._env = None if env is None else dict(env)
        self._run_table = dict(run_table or {})
        self._run_hook = run_hook
        self._delegate_run = delegate_run
        self._post_json_response = post_json_response
        self._slurm = _SlurmFake(None if slurm_case_dir is None else Path(slurm_case_dir))
        self.calls: list[FakeCall] = []
        self.trace: list[TraceEntry] = []
        self.posts: list[tuple[str, str]] = []
        self.sleeps: list[float] = []

    # -- logging ------------------------------------------------------------------

    def _log(self, method: str, *args: object) -> None:
        self.calls.append(FakeCall(method, args))

    @property
    def runs(self) -> list[tuple[str, ...]]:
        """The argv of every ``run()`` call without a shell, in order."""
        return [tuple(call.args[0]) for call in self.calls if call.method == "run"]  # type: ignore[arg-type]

    @property
    def shell_runs(self) -> list[str]:
        """The command line of every ``run_shell()`` / ``run(..., shell=True)`` call, in order."""
        return [str(call.args[0]) for call in self.calls if call.method == "run_shell"]

    @property
    def tools(self) -> list[tuple[str, tuple[str, ...]]]:
        """``(tool, argv)`` of every traced command: the multiset the AMT golden comparison uses."""
        return [(entry.tool, entry.argv) for entry in self.trace]

    # -- filesystem: real reads, logged -----------------------------------------------

    def listdir_like_r(self, directory: PathLike) -> list[str]:
        self._log("listdir_like_r", os.fspath(directory))
        return super().listdir_like_r(directory)

    def listdir_all(self, directory: PathLike) -> list[str]:
        self._log("listdir_all", os.fspath(directory))
        return super().listdir_all(directory)

    def walk(self, top: PathLike) -> Iterator[tuple[str, list[str], list[str]]]:
        self._log("walk", os.fspath(top))
        return super().walk(top)

    def exists(self, path: PathLike) -> bool:
        self._log("exists", os.fspath(path))
        return super().exists(path)

    def is_dir(self, path: PathLike) -> bool:
        self._log("is_dir", os.fspath(path))
        return super().is_dir(path)

    def stat(self, path: PathLike) -> FileStat:
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

    # -- filesystem: real writes, logged ------------------------------------------------

    def write_text(self, path: PathLike, text: str, *, append: bool = False, atomic: bool = False) -> None:
        self._log("write_text", os.fspath(path), text, append, atomic)
        super().write_text(path, text, append=append, atomic=atomic)

    def write_bytes(self, path: PathLike, data: bytes, *, append: bool = False, atomic: bool = False) -> None:
        self._log("write_bytes", os.fspath(path), bytes(data), append, atomic)
        super().write_bytes(path, data, append=append, atomic=atomic)

    def write_rds(self, path: PathLike, obj: object) -> None:
        from modelstats.rdata_io import rds_bytes

        self._log("write_rds", os.fspath(path), obj)
        ProductionEffects.write_bytes(self, path, rds_bytes(obj), atomic=True)

    def copy(self, src: PathLike, dst: PathLike, overwrite: bool = False) -> bool:
        self._log("copy", os.fspath(src), os.fspath(dst), overwrite)
        return super().copy(src, dst, overwrite)

    def rename(self, src: PathLike, dst: PathLike) -> None:
        self._log("rename", os.fspath(src), os.fspath(dst))
        super().rename(src, dst)

    def delete(self, path: PathLike, recursive: bool = False) -> None:
        self._log("delete", os.fspath(path), recursive)
        super().delete(path, recursive)

    def mkdir(self, path: PathLike, parents: bool = False) -> None:
        self._log("mkdir", os.fspath(path), parents)
        super().mkdir(path, parents)

    @contextlib.contextmanager
    def chdir(self, path: PathLike) -> Iterator[None]:
        self._log("chdir", os.fspath(path))
        with super().chdir(path):
            yield

    def getcwd(self) -> str:
        self._log("getcwd")
        return super().getcwd()

    def tempdir(self) -> str:
        self._log("tempdir")
        return super().tempdir()

    # -- canned environment ---------------------------------------------------------

    def run(
        self,
        argv: Sequence[str] | str,
        cwd: PathLike | None = None,
        env: Mapping[str, str] | None = None,
        input: str | None = None,
        *,
        shell: bool = False,
        capture: bool = True,
    ) -> subprocess.CompletedProcess[str]:
        env_copy = None if env is None else dict(env)
        keys: list[tuple[str, ...]]
        if shell:
            command = argv if isinstance(argv, str) else shlex.join(argv)
            tokens = shell_tokens(command)
            self._log("run_shell", command, _fspath(cwd), env_copy, input)
            keys = [(command,), tuple(tokens)]
        elif isinstance(argv, str):
            raise TypeError("run() takes an argv sequence; use shell=True (or run_shell) for a command line")
        else:
            tokens = list(argv)
            self._log("run", tuple(tokens), _fspath(cwd), env_copy, input)
            keys = [tuple(tokens)]
        where = os.getcwd() if cwd is None else os.path.abspath(os.path.expanduser(os.fspath(cwd)))
        for segment in shell_segments(tokens):
            self.trace.append(TraceEntry(os.path.basename(segment[0]), tuple(segment[1:]), where))
        for key in keys:
            if key in self._run_table:
                stdout, status = self._run_table[key]
                return subprocess.CompletedProcess(tokens, status, stdout if capture else "", "")
        if self._run_hook is not None:
            hooked = self._run_hook(tokens, where)
            if hooked is not None:
                stdout, status = hooked
                return subprocess.CompletedProcess(tokens, status, stdout if capture else "", "")
        if self._delegate_run:
            return super().run(argv, cwd, env, input, shell=shell, capture=capture)
        tool = os.path.basename(tokens[0]) if tokens else ""
        if tool in ("squeue", "sacct"):
            stdout, stderr, status = self._slurm.answer(tool, tokens[1:])
            return subprocess.CompletedProcess(tokens, status, stdout if capture else "", stderr)
        return subprocess.CompletedProcess(tokens, 127, "", f"sh: {tool}: command not found\n")

    def post_json(self, url: str, payload: str | bytes) -> tuple[int, str]:
        text = payload if isinstance(payload, str) else bytes(payload).decode("utf-8", "surrogateescape")
        self._log("post_json", url, text)
        self.posts.append((url, text))
        return self._post_json_response

    def sleep(self, seconds: float) -> None:
        self._log("sleep", seconds)
        self.sleeps.append(seconds)

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

    def setenv(self, name: str, value: str) -> None:
        self._log("setenv", name, value)
        if self._env is None:
            super().setenv(name, value)
        else:
            self._env[name] = value


#: The phase-1 name of the double; the same class.
FakeEffects = RecordingEffects
