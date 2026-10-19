"""Is a run found in SLURM, and in which state?

R source: R/foundInSlurm.R.

Deviations and fixes (details in PORTING-BUGS.md):

- ``squeue`` is called with an argument list; when it is missing or fails the run counts as not
  found, where R aborted the whole status table (BUG-026); its output is decoded with
  replacement characters (BUG-034); the default user comes from the password database (BUG-032);
- matching stays a substring test on the raw squeue line, so that a job is recognised exactly
  when R recognised it, folder-prefix matches included (BUG-027);
- quirks of the result are kept: the pending test misses a qos with digits (BUG-028), ``N users``
  counts jobs (BUG-029), one trailing empty field is dropped so an empty qos shifts the fields
  (BUG-030); a line with fewer than three fields gives empty fields where R stopped;
- "is this my run" tests ``line.startswith(user + " ")`` where R used ``user`` as a regular
  expression (BUG-004);
- the run folder is resolved to an absolute path; R left a non-existing path as given (BUG-031).
"""

from __future__ import annotations

import re
from dataclasses import dataclass
from pathlib import Path

from modelstats import _shell
from modelstats._sorting import ITERATION
from modelstats._textscan import r_strsplit

__all__ = ["SqueueJob", "found_in_slurm"]

SQUEUE_FORMAT = "%u %Z %j %M %T %q"
_PENDING = re.compile(r"PENDING [A-Za-z]*\Z")
_STARTUP_TIME = re.compile(r"[0-5]:[0-9]{2}")


@dataclass(frozen=True)
class SqueueJob:
    """One line of ``squeue -h -o '%u %Z %j %M %T %q'``: user, work dir, job name, time, state, qos."""

    line: str
    user: str
    time: str
    qos: str

    @classmethod
    def parse(cls, line: str) -> SqueueJob:
        """Read the user (first field), the elapsed time (third from the end) and the qos (last).

        A line with fewer than three fields gives empty fields (R stopped with a subscript error).
        """
        fields = r_strsplit(line, " ")
        return cls(
            line=line,
            user=fields[0] if fields else "",
            time=fields[-3] if len(fields) >= 3 else "",
            qos=fields[-1] if fields else "",
        )

    def is_owned_by(self, user: str) -> bool:
        """``grepl(paste0("^", user, " "), line)`` with the user taken as text (BUG-004)."""
        return self.line.startswith(f"{user} ")

    @property
    def pending(self) -> bool:
        """``grepl("PENDING [A-Za-z]*$", line)`` (BUG-028)."""
        return _PENDING.search(self.line) is not None

    @property
    def startup(self) -> bool:
        """Running for less than six minutes (an elapsed time like ``0:45``)."""
        return _STARTUP_TIME.fullmatch(self.time) is not None


def _matching_jobs(run_dir: Path, lines: list[str]) -> list[SqueueJob]:
    """The squeue lines mentioning the folder and the run name (followed by a space)."""
    run_name = run_dir.name
    iteration = ITERATION.match(run_name)
    search_dir = str(run_dir.parent.parent if iteration else run_dir)
    matched = [line for line in lines if search_dir in line and f"{run_name} " in line]
    if not matched and iteration and iteration["kind"] == "mag":
        # try to find REMIND slurm job corresponding to coupled MAgPIE run
        remind_name = run_name.replace("-mag-", "-rem-")
        parent = str(run_dir.parent.parent.parent)
        matched = [line for line in lines if f"{remind_name} " in line and parent in line]
    return [SqueueJob.parse(line) for line in matched]


def found_in_slurm(run_dir: Path, user: str | None = None) -> str:
    """Is the run found in SLURM?

    Args:
        run_dir: Path to the folder where the run is performed.
        user: The user whose own job shows its qos instead of its owner (default: the current
            user); the jobs of every user are searched.

    Returns:
        ``"no"`` when no job matches. For exactly one job: its qos when it is yours, else its
        user, each followed by ``" pending"`` or ``" startup"`` when applicable
        (``"priority pending"``, ``"bob startup"``). For several jobs: the user when all belong
        to one user (also when it is you), otherwise ``"N users"`` with N the number of jobs.
    """
    if user is None:
        user = _shell.current_user()
    run_dir = run_dir.resolve()
    jobs = _matching_jobs(run_dir, _shell.squeue(SQUEUE_FORMAT))
    if len(jobs) == 1:
        job = jobs[0]
        suffix = " pending" if job.pending else " startup" if job.startup else ""
        label = job.qos if job.is_owned_by(user) else job.user
        return f"{label}{suffix}"
    if len({job.user for job in jobs}) == 1:
        return jobs[0].user
    if len(jobs) > 1:
        return f"{len(jobs)} users"
    return "no"
