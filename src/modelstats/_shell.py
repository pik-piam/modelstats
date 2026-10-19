"""External programs, cluster facts and the cluster paths.

R source: ``system(..., intern = TRUE)``, ``Sys.info()[["user"]]``, ``file.exists("/p")``,
``Sys.sleep``, ``curl -X POST``. Every subprocess takes an argument list (no shell strings) and an
explicit ``cwd``; its output is decoded as UTF-8 with replacement characters, so a stray byte in a
job name never raises. Tests monkeypatch the functions of this module.
"""

from __future__ import annotations

import getpass
import http.client
import json
import logging
import os
import pwd
import subprocess
import time
import urllib.request
from collections.abc import Mapping, Sequence
from datetime import date
from pathlib import Path
from typing import Any

__all__ = [
    "AMT_OUTPUT",
    "AMT_RUNCODE",
    "MAGPIE_RESULTS_ARCHIVE_ENV",
    "REMIND_RESULTS_ARCHIVE",
    "current_user",
    "lines",
    "on_cluster",
    "post_json",
    "rscript_argv",
    "run",
    "sacct",
    "sacct_argv",
    "sleep",
    "squeue",
    "squeue_argv",
]

AMT_OUTPUT = Path("/p/projects/remind/modeltests/remind/output")
AMT_RUNCODE = Path("/p/projects/remind/modeltests/remind/runcode.rds")
REMIND_RESULTS_ARCHIVE = Path("/p/projects/rd3mod/models/results/remind")
MAGPIE_RESULTS_ARCHIVE_ENV = "MAGPIE_RESULTS_ARCHIVE_PATH"

sleep = time.sleep
_log = logging.getLogger(__name__)


def run(
    argv: Sequence[str],
    *,
    cwd: Path | None = None,
    check: bool = False,
    input: str | None = None,
    env: Mapping[str, str] | None = None,
) -> subprocess.CompletedProcess[str]:
    """Run a program with captured text output.

    A missing executable yields exit status 127 like a shell would (``CalledProcessError`` with
    ``check=True``) instead of ``FileNotFoundError``; a missing ``cwd`` or a non-executable file
    raises the ``OSError`` as it is.
    """
    args = list(argv)
    try:
        return subprocess.run(
            args,
            cwd=cwd,
            check=check,
            input=input,
            env=env,
            encoding="utf-8",
            errors="replace",
            capture_output=True,
        )
    except FileNotFoundError as error:
        if str(error.filename) != args[0]:
            raise
        completed = subprocess.CompletedProcess(args, 127, "", f"{args[0]}: {error.strerror}")
        if check:
            raise subprocess.CalledProcessError(127, args, "", completed.stderr) from error
        return completed


def lines(argv: Sequence[str], *, cwd: Path | None = None) -> list[str]:
    """The stdout lines of a program; ``[]`` when it fails or does not exist.

    Unlike R's ``system(intern = TRUE)``, output of a failing program is discarded.
    """
    completed = run(argv, cwd=cwd)
    if completed.returncode != 0:
        return []
    text = completed.stdout
    if not text:
        return []  # character(0)
    return text.split("\n")[:-1] if text.endswith("\n") else text.split("\n")  # system(): "\n" only


def squeue_argv(fmt: str, *, user: str | None = None) -> list[str]:
    """``squeue -h -o FMT [-u USER]`` (USER may be a comma separated list)."""
    argv = ["squeue", "-h", "-o", fmt]
    if user:
        argv += ["-u", user]
    return argv


def squeue(fmt: str, *, user: str | None = None) -> list[str]:
    """The lines of ``squeue -h -o FMT [-u USER]``."""
    return lines(squeue_argv(fmt, user=user))


def sacct_argv(user: str, since: date, fmt: str) -> list[str]:
    """``sacct -u USER -s cd,f,cancelled,timeout,oom -S SINCE -E now -P -n --format FMT``."""
    return [
        "sacct",
        "-u",
        user,
        "-s",
        "cd,f,cancelled,timeout,oom",
        "-S",
        since.isoformat(),
        "-E",
        "now",
        "-P",
        "-n",
        "--format",
        fmt,
    ]


def sacct(user: str, since: date, fmt: str) -> list[str]:
    """The lines of ``sacct`` for the finished jobs of USER since SINCE."""
    return lines(sacct_argv(user, since, fmt))


def rscript_argv(expression: str, *args: str) -> list[str]:
    """``Rscript -e EXPRESSION [ARGS...]``: the arguments reach R as ``commandArgs(trailingOnly = TRUE)``."""
    return ["Rscript", "-e", expression, *args]


def post_json(url: str, payload: Mapping[str, Any]) -> None:
    """One HTTP POST of a JSON document (the Mattermost webhook).

    A failed post is logged as a warning, never raised, and the URL is never echoed.
    """
    try:
        request = urllib.request.Request(
            url, data=json.dumps(payload).encode("utf-8"), headers={"Content-Type": "application/json"}
        )
        with urllib.request.urlopen(request, timeout=60):
            pass
    except ValueError, http.client.InvalidURL:  # their messages quote the URL
        _log.warning("The notification could not be posted: the webhook URL is not a URL")
    except (OSError, http.client.HTTPException) as error:
        _log.warning("The notification could not be posted: %s", error)


def on_cluster() -> bool:
    """Are we on the PIK cluster (is ``/p`` there)?"""
    return Path("/p").exists()


def current_user() -> str:
    """The name of the real user (from the password database, like ``Sys.info()``)."""
    try:
        return pwd.getpwuid(os.getuid()).pw_name
    except KeyError:
        return getpass.getuser()
