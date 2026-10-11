"""SLURM queries of ``rs``: ``R/foundInSlurm.R`` and the ``-C`` branch of ``R/commandLineInterface.R``.

Parity port (``migration/03-migration-plan.md`` sections 3.2 and 3.4; ``06`` section 1). ``foundInSlurm(mydir,
user)`` lists every job of every user with ``squeue`` and decides from the lines that mention the directory
whether the run is in SLURM; ``rs -C`` asks ``squeue`` (and ``sacct`` with ``-d N``) for one user's work
directories and job names and turns them into run folders. Both reach the scheduler, the clock, the user
name and the filesystem only through :class:`modelstats.env.Effects`, so the goldens under
``migration/goldens/slurm`` and the ``rs -C`` goldens replay the canned scheduler of the harness.

Reproduced on purpose (``migration/04-bug-register.md``; the decision table ``03`` 3.5 keeps D-05 pending, so
parity holds everywhere):

* the directory is a *substring* of the line (``grep(fixed = TRUE)``, ``R/foundInSlurm.R:20-21``), so a job in
  ``<dir>-extra`` counts for ``<dir>`` (BUG-023);
* ``" pending"`` needs ``PENDING`` followed by letters only up to the end of the line (line 32), so a QOS
  with a digit is reported as ``" startup"`` instead (BUG-031);
* with several hits the suffixes are never added (BUG-019) and ``"<n> users"`` counts jobs (BUG-035);
* the user is a regular expression (line 31), so ``-u a,b`` never matches its own jobs (BUG-009);
* ``-C`` pairs the ``%Z`` and ``%j`` answers of two separate ``squeue`` calls by position (BUG-028): answers
  of different lengths raise ``subscript out of bounds`` as R does; and it uses every job name as an unescaped
  TRE regular expression against its work directory (BUG-008): an invalid name raises :class:`SlurmError`
  with R's message.

Two views of a ``squeue`` line (``06`` 5.2): :class:`SqueueLine` is the R side, the text and its
``strsplit(" ")`` pieces read positionally from the ends the way lines 29-30 and 37 do; :class:`SlurmJob` is
the typed view for the ``Run`` object (``06`` 5.1), whose ``pending`` and ``startup`` come from the state and
the elapsed field, not from the R regexes. Where the two disagree, ``found_in_slurm`` is right for ``rs``:

======================================== ===================== =============================== =======
line                                     ``found_in_slurm``    ``SlurmJob``                    why
======================================== ===================== =============================== =======
``u /d j 0:00 PENDING qos1`` (own job)   ``"qos1 startup"``    ``pending=True, startup=False`` BUG-031
``u /d j 0:00 PENDING standby`` (own)    ``"standby pending"`` ``pending=True, startup=False`` same
two lines of ``u`` for one directory     ``"u"``               two jobs, each with its flags   BUG-019
======================================== ===================== =============================== =======

The command lines are built as the R ``paste0`` calls build them and split into words as ``sh`` would
(``shlex``), without a shell: no expansion, no metacharacters (the spirit of D-29), while ``-u ""`` still
yields ``squeue -u -h -o '%Z'`` exactly as R's ``opt$user <- NULL`` of ``-A -C`` does.
"""

from __future__ import annotations

import math
import re
import shlex
from collections.abc import Iterable, Sequence
from dataclasses import dataclass
from datetime import date, timedelta
from typing import Final

from modelstats.env import Effects, PathLike, RunResult, sort_like_r
from modelstats.errors import SUBSCRIPT_OUT_OF_BOUNDS, ModelstatsError
from modelstats.runfolders import r_basename, r_dirname
from modelstats.textscan import intern_lines

__all__ = [
    "INVALID_REGEX",
    "MYRUNS_CALL",
    "NO_JOB",
    "PENDING_SUFFIX",
    "RUNNAMES_CALL",
    "USER_GREPL_CALL",
    "SQUEUE_FORMAT",
    "STARTUP_SUFFIX",
    "SUBSCRIPT_OUT_OF_BOUNDS",
    "CurrentRuns",
    "SlurmCall",
    "SlurmError",
    "SlurmJob",
    "SlurmLookup",
    "SqueueLine",
    "current_runs",
    "found_in_slurm",
    "lookup_slurm",
    "match_squeue",
    "parse_elapsed",
    "parse_squeue",
    "query_squeue",
    "r_grepl",
    "r_strsplit_space",
    "run_folders_of",
    "sacct_since_text",
    "scheduler_calls",
    "slurm_answer",
]

SQUEUE_FORMAT: Final = "%u %Z %j %M %T %q"
"""``R/foundInSlurm.R:19``: user, work directory, job name, elapsed time, state and QOS, space-separated."""

NO_JOB: Final = "no"
"""``R/foundInSlurm.R:46``: no line kept."""

PENDING_SUFFIX: Final = " pending"
"""``R/foundInSlurm.R:32``."""

STARTUP_SUFFIX: Final = " startup"
"""``R/foundInSlurm.R:33``."""

INVALID_REGEX: Final = "invalid regular expression '{pattern}', reason '{reason}'"
"""R's message for a pattern TRE rejects (``R/commandLineInterface.R:165``, BUG-008)."""

_COUPLED_NAME: Final = re.compile(r"-(rem|mag)-[0-9]+\Z")
"""``R/foundInSlurm.R:15``: a coupling iteration, whose job runs two directories up."""

_MAGPIE_NAME: Final = re.compile(r"-mag-[0-9]+\Z")
"""``R/foundInSlurm.R:23``: a MAgPIE iteration, looked up under its REMIND name when nothing matched."""

_PENDING_LINE: Final = re.compile(r"PENDING [A-Za-z]*\Z")
"""``R/foundInSlurm.R:32``: ``PENDING`` and a QOS of letters only at the end of the line (BUG-031)."""

_STARTUP_ELAPSED: Final = re.compile(r"[0-5]:[0-9]{2}")
"""``R/foundInSlurm.R:33`` (``fullmatch``): an elapsed time under six minutes, ``M:SS``."""

_ELAPSED: Final = re.compile(r"(?:(?P<days>\d+)-)?(?:(?P<hours>\d+):)?(?P<minutes>\d+):(?P<seconds>\d+)")
"""``squeue %M``: ``[[days-]hours:]minutes:seconds``."""

_PENDING_STATE: Final = "PENDING"
_MAGPIE_JOB: Final = "mag-run"
_STEP_JOBS: Final = frozenset({"batch"})
_STEP_JOBS_WITH_MAGPIE: Final = frozenset({"default", "batch"})
_SACCT_STATES: Final = "cd,f,cancelled,timeout,oom"

_TRE_REASONS: Final[dict[str, str]] = {
    "unterminated character set": "Missing ']'",
    "missing ), unterminated subpattern": "Missing ')'",
    "bad escape (end of pattern)": "Trailing backslash",
    "multiple repeat": "Invalid use of repetition operators",
}
"""``re.error`` messages and the TRE reason R prints for the same pattern (verified in R 4.6.1)."""


class SlurmError(ModelstatsError):
    """An R error of ``foundInSlurm()`` or ``rs -C``; ``str(error)`` is the R condition message.

    ``call`` is the failing R call as ``deparse(conditionCall(e), nlines = 1L)`` prints it, what ``try()``
    puts after ``Error in``; set where the port knows it (the user regex of ``R/foundInSlurm.R:31``).
    """

    def __init__(self, message: str, *, call: str | None = None) -> None:
        """``message`` is R's condition message, ``call`` the deparsed call it is reported under."""
        super().__init__(message)
        self.call = call


USER_GREPL_CALL: Final = 'grepl(paste0("^", user, " "), squeuefiltered)'
"""``R/foundInSlurm.R:31`` as R deparses it: the call of the invalid-regex error for a bad ``user``."""

RUNNAMES_CALL: Final = "runnames[[i]]"
"""``R/commandLineInterface.R:165``: the call of ``subscript out of bounds`` for a job name past the end."""

MYRUNS_CALL: Final = "myruns[[i]]"
"""``R/commandLineInterface.R:165``: the call of ``subscript out of bounds`` for a missing work directory."""


# ---------------------------------------------------------------------------------------------
# R string helpers
# ---------------------------------------------------------------------------------------------


def r_strsplit_space(text: str) -> tuple[str, ...]:
    """``strsplit(text, " ")[[1]]``: pieces between single spaces, as R cuts them.

    Leading and inner empty pieces are kept (``" a"`` is ``"", "a"``; ``"a  b"`` is ``"a", "", "b"``), one
    trailing empty piece is dropped (``"a b "`` is ``"a", "b"``; ``"a  "`` is ``"a", ""``) and ``""`` is
    ``character(0)``. Verified in R 4.6.1.
    """
    if text == "":
        return ()
    pieces = text.split(" ")
    if pieces[-1] == "":
        pieces.pop()
    return tuple(pieces)


def r_grepl(pattern: str, text: str, call: str | None = None) -> bool:
    """``grepl(pattern, text)`` with R's default engine (TRE, extended syntax) where ``re`` agrees with it.

    ``call`` is the R call the error of an invalid ``pattern`` is reported under (:class:`SlurmError`).

    The characters of run and job names (letters, digits, ``.``, ``-``, ``_``) mean the same in both engines.
    Differences, all inside BUG-008 (D-05 pending): TRE takes a repetition operator at the start of the
    pattern literally (``"+foo"`` matches ``"x+foo"``), which is reproduced for the first character; it also
    takes an unmatched ``)`` literally and rejects a ``{`` that does not open a bound, where ``re`` does the
    opposite, and those are not reproduced. An invalid pattern raises :class:`SlurmError` with R's message,
    the reason being TRE's where ``re``'s error has a counterpart and ``re``'s own text otherwise.
    """
    tre_pattern = f"\\{pattern}" if pattern.startswith(("*", "+", "?")) else pattern
    try:
        compiled = re.compile(tre_pattern)
    except re.error as error:
        reason = _TRE_REASONS.get(error.msg, error.msg)
        raise SlurmError(INVALID_REGEX.format(pattern=pattern, reason=reason), call=call) from error
    return compiled.search(text) is not None


def _r_at(vector: Sequence[str], index: int, call: str | None = None) -> str:
    """``vector[[index + 1]]``: the element, or R's ``subscript out of bounds`` reported under ``call``."""
    if 0 <= index < len(vector):
        return vector[index]
    raise SlurmError(SUBSCRIPT_OUT_OF_BOUNDS, call=call)


def _without(vector: Sequence[str], dropped: Iterable[int]) -> list[str]:
    """``vector[-dropped]`` for a non-empty ``dropped``."""
    gone = set(dropped)
    return [value for index, value in enumerate(vector) if index not in gone]


# ---------------------------------------------------------------------------------------------
# The scheduler calls
# ---------------------------------------------------------------------------------------------


@dataclass(frozen=True)
class SlurmCall:
    r"""One ``system(cmd, intern = TRUE)`` call: the shell text R built and what came back.

    ``lines`` is the character vector R received (``textscan.intern_lines``: one element per ``\n``, nothing
    for a command that printed nothing, whatever its exit status); ``warning`` is the text of the warning R
    attaches to a non-zero status (``running command '...' had status N``), which ``rs`` lets through to
    stderr.
    """

    shell_text: str
    result: RunResult

    @property
    def argv(self) -> tuple[str, ...]:
        """The words ``run`` received."""
        return self.result.argv

    @property
    def lines(self) -> list[str]:
        """The stdout lines as ``intern = TRUE`` returns them."""
        return intern_lines(self.result.stdout.encode("utf-8", "surrogateescape"))

    @property
    def warning(self) -> str | None:
        """R's warning for a non-zero exit status, ``None`` on success."""
        if self.result.ok:
            return None
        return f"running command '{self.shell_text}' had status {self.result.returncode}"


def _call(shell_text: str, env: Effects) -> SlurmCall:
    """``system(shell_text, intern = TRUE)`` without a shell: the words ``sh`` would pass go to ``run``."""
    try:
        argv = shlex.split(shell_text)
    except ValueError as error:
        raise SlurmError(f"cannot split the command line {shell_text!r}: {error}") from error
    return SlurmCall(shell_text, env.run(argv))


def query_squeue(env: Effects) -> SlurmCall:
    """``R/foundInSlurm.R:19``: ``squeue -h -o '%u %Z %j %M %T %q'``, every job of every user."""
    return _call(f"squeue -h -o '{SQUEUE_FORMAT}'", env)


def _squeue_of(user: str, fmt: str, env: Effects) -> SlurmCall:
    """``R/commandLineInterface.R:141-142``: ``squeue -u USER -h -o '%Z'`` or ``'%j'``."""
    return _call(f"squeue -u {user} -h -o '{fmt}'", env)


def _sacct_of(user: str, since: str, field: str, env: Effects) -> SlurmCall:
    """``R/commandLineInterface.R:145-147``: jobs finished since ``since``, as ``WorkDir`` or ``JobName``."""
    return _call(f"sacct -u {user} -s {_SACCT_STATES} -S {since} -E now -P -n --format {field}", env)


def _sacct_since(daysback: int, env: Effects) -> str:
    """``R/commandLineInterface.R:145``: ``as.Date(format(Sys.Date(), "%Y-%m-%d")) - daysback`` as text."""
    return sacct_since_text(env.now().date(), daysback)


_EPOCH: Final = date(1970, 1, 1)
"""R's ``Date`` origin."""
_DAYS_PER_ERA: Final = 146097
_DAYS_TO_ERA_START: Final = 719468
"""Days from 1970-01-01 back to 0000-03-01, the start of an era of 400 years (H. Hinnant)."""


def sacct_since_text(today: date, offset: float | None) -> str:
    """``paste0(as.Date(format(Sys.Date(), "%Y-%m-%d")) - offset)`` (``R/commandLineInterface.R:145``).

    The civil date ``floor(days - offset)`` days after 1970-01-01 in the proleptic Gregorian calendar as R
    formats it, ``%Y-%m-%d`` with the year a plain signed integer (``1752-12-15``, ``99-04-05``, ``0-09-11``,
    ``-273788674-01-19``); a non-finite offset prints ``-Inf``, ``Inf`` or ``NaN`` and ``None`` (R's ``NA``,
    ``as.numeric()`` of a non-number) prints ``NA`` (probed in R 4.6.1 on 2026-10-02).
    """
    if offset is None:
        return "NA"
    if math.isnan(offset):
        return "NaN"
    if math.isinf(offset):
        return "Inf" if offset < 0 else "-Inf"
    year, month, day = _civil_from_days(math.floor((today - _EPOCH).days - offset))
    return f"{year}-{month:02d}-{day:02d}"


def _civil_from_days(days: int) -> tuple[int, int, int]:
    """Days since 1970-01-01 to ``(year, month, day)``, proleptic Gregorian, for any integer (H. Hinnant)."""
    z = days + _DAYS_TO_ERA_START
    era = z // _DAYS_PER_ERA
    day_of_era = z - era * _DAYS_PER_ERA
    year_of_era = (day_of_era - day_of_era // 1460 + day_of_era // 36524 - day_of_era // 146096) // 365
    day_of_year = day_of_era - (365 * year_of_era + year_of_era // 4 - year_of_era // 100)
    shifted_month = (5 * day_of_year + 2) // 153
    day = day_of_year - (153 * shifted_month + 2) // 5 + 1
    month = shifted_month + 3 if shifted_month < 10 else shifted_month - 9
    return year_of_era + era * 400 + (month <= 2), month, day


# ---------------------------------------------------------------------------------------------
# A squeue line, R's reading and the typed reading
# ---------------------------------------------------------------------------------------------


@dataclass(frozen=True)
class SlurmJob:
    """The typed view of one ``squeue`` line for the ``Run`` object (``06`` 5.1).

    ``pending`` is the state being ``PENDING``; ``startup`` is a job that is not pending and has run for less
    than six minutes (``elapsed`` of the form ``M:SS``). ``found_in_slurm`` derives its suffixes from the R
    regexes instead (module docstring).
    """

    user: str
    workdir: str
    name: str
    elapsed: str
    state: str
    qos: str
    pending: bool
    startup: bool

    @property
    def elapsed_time(self) -> timedelta | None:
        """``elapsed`` as a duration, ``None`` when it is not a time (``INVALID``, ``N/A``)."""
        return parse_elapsed(self.elapsed)


def parse_elapsed(text: str) -> timedelta | None:
    """``squeue %M`` as a duration: ``M:SS``, ``H:MM:SS`` or ``D-HH:MM:SS``; ``None`` for anything else."""
    match = _ELAPSED.fullmatch(text)
    if match is None:
        return None
    parts = {name: int(match.group(name) or 0) for name in ("days", "hours", "minutes", "seconds")}
    return timedelta(**parts)


@dataclass(frozen=True)
class SqueueLine:
    """One line of ``squeue -h -o '%u %Z %j %M %T %q'`` as R reads it.

    ``fields`` is ``strsplit(text, " ")[[1]]``; the properties read it the way ``R/foundInSlurm.R`` does, from
    the ends, so a job name with spaces does not shift the elapsed time, the state or the QOS. A field R
    would index past the end raises :class:`SlurmError` ``subscript out of bounds`` (line 29 on a short line).
    """

    text: str
    fields: tuple[str, ...]

    @property
    def user(self) -> str:
        """``strsplit(line, " ")[[1]][[1]]`` (``R/foundInSlurm.R:37, 41``)."""
        return self._field(0)

    @property
    def first_field(self) -> str | None:
        """``sapply(strsplit(lines, " "), `[`, 1)`` (``R/foundInSlurm.R:40``): ``NA`` for an empty line."""
        return self.fields[0] if self.fields else None

    @property
    def workdir(self) -> str:
        """The second field (``%Z``)."""
        return self._field(1)

    @property
    def name(self) -> str:
        """The job name (``%j``): everything between the work directory and the last three fields."""
        return " ".join(self.fields[2:-3])

    @property
    def elapsed(self) -> str:
        """``rev(strsplit(line, " ")[[1]])[[3]]`` (``R/foundInSlurm.R:29``): the elapsed time (``%M``)."""
        return self._field(len(self.fields) - 3)

    @property
    def state(self) -> str:
        """The second-to-last field (``%T``)."""
        return self._field(len(self.fields) - 2)

    @property
    def qos(self) -> str:
        """``rev(strsplit(line, " ")[[1]])[[1]]`` (``R/foundInSlurm.R:30``): the QOS (``%q``)."""
        return self._field(len(self.fields) - 1)

    def to_job(self) -> SlurmJob:
        """The typed view; needs the six fields of the format."""
        if len(self.fields) < 6:
            raise SlurmError(f"squeue line has fewer than six fields: {self.text!r}")
        pending = self.state == _PENDING_STATE
        startup = not pending and _STARTUP_ELAPSED.fullmatch(self.elapsed) is not None
        return SlurmJob(
            self.user, self.workdir, self.name, self.elapsed, self.state, self.qos, pending, startup
        )

    def _field(self, index: int) -> str:
        return _r_at(self.fields, index)


def parse_squeue(text: str) -> SqueueLine:
    """A ``squeue`` line with its ``strsplit(" ")`` pieces."""
    return SqueueLine(text, r_strsplit_space(text))


# ---------------------------------------------------------------------------------------------
# foundInSlurm
# ---------------------------------------------------------------------------------------------


@dataclass(frozen=True)
class SlurmLookup:
    """What ``foundInSlurm(mydir, user)`` computed for one directory.

    ``directory`` is ``mydir`` after ``normalizePath`` and the grand-parent rule of lines 15-17, ``run_name``
    its basename (line 14), ``matched`` the lines kept by :func:`match_squeue`; ``text`` is the R return value
    and ``job`` the typed view of the one line R reads in its single-hit branch (``None`` for no hit or
    several).
    """

    directory: str
    run_name: str
    user: str
    call: SlurmCall
    matched: tuple[SqueueLine, ...]

    @property
    def text(self) -> str:
        """The string ``foundInSlurm()`` returns."""
        return slurm_answer(self.matched, self.user)

    @property
    def jobs(self) -> tuple[SlurmJob, ...]:
        """Every matched line as a :class:`SlurmJob`."""
        return tuple(line.to_job() for line in self.matched)

    @property
    def job(self) -> SlurmJob | None:
        """The single matched job, ``None`` when there is none or more than one."""
        return self.matched[0].to_job() if len(self.matched) == 1 else None


def found_in_slurm(mydir: PathLike, env: Effects, user: str | None = None) -> str:
    """``foundInSlurm(mydir, user)`` (``R/foundInSlurm.R:10-48``): the string the ``inSlurm`` column shows.

    ``user`` defaults to ``Sys.info()[["user"]]`` (line 12). The answer is the QOS for the caller's own
    single job, the job's user for someone else's, with ``" startup"`` or ``" pending"`` appended; the user
    for several jobs of one user; ``"<n> users"`` for several jobs of several users; ``"no"`` otherwise.
    """
    return lookup_slurm(mydir, env, user).text


def lookup_slurm(mydir: PathLike, env: Effects, user: str | None = None) -> SlurmLookup:
    """``foundInSlurm`` with its working state kept: the query, the kept lines and both views of them."""
    who = env.user if user is None else user  # line 12
    directory, run_name = _target(mydir, env)  # lines 13-17
    call = query_squeue(env)  # line 19
    lines = [parse_squeue(text) for text in call.lines]
    return SlurmLookup(directory, run_name, who, call, match_squeue(lines, directory, run_name))


def _target(mydir: PathLike, env: Effects) -> tuple[str, str]:
    """``R/foundInSlurm.R:13-17``: the directory to look for and the run name.

    ``normalizePath`` resolves an existing path and leaves a missing one as given; the run name is its
    basename; a ``-rem-N``/``-mag-N`` name is looked for by its grand-parent (the coupled job's work directory
    is the model folder, not the output folder).
    """
    normalized = env.normalize_path(mydir)
    run_name = r_basename(normalized)
    directory = r_dirname(r_dirname(normalized)) if _COUPLED_NAME.search(normalized) else normalized
    return directory, run_name


def match_squeue(lines: Sequence[SqueueLine], directory: str, run_name: str) -> tuple[SqueueLine, ...]:
    """``R/foundInSlurm.R:20-27``: the lines kept for ``directory`` and ``run_name``.

    Lines containing ``directory`` as a substring (BUG-023), then those containing ``"<run_name> "``; when
    nothing is left and the name is a MAgPIE iteration, the REMIND job of the same iteration instead: the
    lines containing ``"<name with -rem->"`` and the parent of ``directory``.
    """
    kept = [line for line in lines if directory in line.text]
    kept = [line for line in kept if f"{run_name} " in line.text]
    if not kept and _MAGPIE_NAME.search(run_name):
        rem_name = run_name.replace("-mag-", "-rem-")
        parent = r_dirname(directory)
        kept = [line for line in lines if f"{rem_name} " in line.text and parent in line.text]
    return tuple(kept)


def slurm_answer(matched: Sequence[SqueueLine], user: str) -> str:
    """``R/foundInSlurm.R:28-47``: the return value for the kept lines.

    One line: :func:`_single_hit`. Several lines of one user (by first field): that user, no suffix
    (BUG-019). Several lines otherwise: ``"<n> users"`` with n the number of lines (BUG-035). None: ``"no"``.
    """
    if len(matched) == 1:
        return _single_hit(matched[0], user)
    if len({line.first_field for line in matched}) == 1:
        return matched[0].user
    if len(matched) > 1:
        return f"{len(matched)} users"
    return NO_JOB


def _single_hit(line: SqueueLine, user: str) -> str:
    """``R/foundInSlurm.R:29-39``: QOS for the user's own job, else the job's user, plus one suffix at most.

    ``user`` is matched as a regular expression anchored at the start of the line (BUG-009); ``" pending"``
    needs ``PENDING`` and a QOS of letters at the end of the line (BUG-031); ``" startup"`` needs an elapsed
    time ``M:SS`` and no pending suffix.
    """
    elapsed = line.elapsed
    qos = line.qos
    yourrun = r_grepl(f"^{user} ", line.text, call=USER_GREPL_CALL)
    pending = PENDING_SUFFIX if _PENDING_LINE.search(line.text) else ""
    startup = STARTUP_SUFFIX if not pending and _STARTUP_ELAPSED.fullmatch(elapsed) else ""
    head = qos if yourrun else line.user
    return f"{head}{startup}{pending}"


# ---------------------------------------------------------------------------------------------
# rs -C
# ---------------------------------------------------------------------------------------------


@dataclass(frozen=True)
class CurrentRuns:
    """``rs -C`` (``R/commandLineInterface.R:139-176``): the run folders and the scheduler calls behind them.

    ``runs`` is ``sort(unique(...))`` of the existing paths (R's collation, ``env.sort_like_r``); ``calls``
    are the two ``squeue`` calls and, with ``daysback > 0``, the two ``sacct`` calls, in R's order, for the
    warnings and the stderr the command line reproduces.
    """

    runs: tuple[str, ...]
    calls: tuple[SlurmCall, ...]


def current_runs(user: str, daysback: int, env: Effects, *, since: str | None = None) -> CurrentRuns:
    """The ``-C`` branch of ``commandLineInterface`` (``R/commandLineInterface.R:139-176``) in one step.

    :func:`scheduler_calls` then :func:`run_folders_of`; ``rs`` runs the two steps itself so that the
    diagnostics of the calls are kept when the second step raises. The caller prints the "no runs"
    warnings and continues with ``runs``.
    """
    calls = scheduler_calls(user, daysback, env, since=since)
    return CurrentRuns(run_folders_of(calls, env), calls)


def scheduler_calls(
    user: str, daysback: int, env: Effects, *, since: str | None = None
) -> tuple[SlurmCall, ...]:
    """``R/commandLineInterface.R:141-147``: ``squeue`` twice, then ``sacct`` twice for ``daysback > 0``.

    Work directories (``%Z``, ``WorkDir``) and job names (``%j``, ``JobName``) are asked for separately, in
    R's order. ``since`` replaces the ``-S`` date text of the ``sacct`` calls: line 145 pastes the date minus
    ``as.numeric(opt$daysback)`` into it, which for a ``-d`` value that optparse kept as a string is ``NA``
    (``x``, golden ``rs/daysback-nonint``), ``-Inf`` (``1e999``, golden ``rs/daysback-overflow``) or a date
    with an absurd year (BUG-066), so ``rs`` passes that text through :func:`sacct_since_text`.
    """
    calls = [_squeue_of(user, "%Z", env), _squeue_of(user, "%j", env)]
    if daysback > 0:
        date_text = _sacct_since(daysback, env) if since is None else since
        calls += [_sacct_of(user, date_text, "WorkDir", env), _sacct_of(user, date_text, "JobName", env)]
    return tuple(calls)


def run_folders_of(calls: Sequence[SlurmCall], env: Effects) -> tuple[str, ...]:
    """``R/commandLineInterface.R:150-176``: the run folders behind the answers of :func:`scheduler_calls`.

    The work directories are the lines of the first and third call, the names those of the second and
    fourth, paired by position (BUG-028); ``batch`` jobs are dropped, ``default`` ones too when a
    ``mag-run`` job is listed; a job whose name is not found in its work directory (as a regular
    expression, BUG-008) and is not ``mag-run`` is a coupled run at ``<workdir>/output/<name>``; missing
    paths are dropped; the rest is ``sort(unique())``.

    Raises:
        SlurmError: ``subscript out of bounds`` under :data:`RUNNAMES_CALL` or :data:`MYRUNS_CALL` for
            answers of different lengths; R's message for an invalid job name.
    """
    workdirs = [line for call in calls[0::2] for line in call.lines]
    names = [line for call in calls[1::2] for line in call.lines]
    workdirs, names = _drop_step_jobs(workdirs, names)
    return tuple(_run_folders(workdirs, names, env)) if workdirs else ()


def _drop_step_jobs(workdirs: Sequence[str], names: Sequence[str]) -> tuple[list[str], list[str]]:
    """``R/commandLineInterface.R:150-157``: drop ``batch`` jobs, and ``default`` ones next to ``mag-run``."""
    dropped = _STEP_JOBS_WITH_MAGPIE if any(_MAGPIE_JOB in name for name in names) else _STEP_JOBS
    delete = [index for index, name in enumerate(names) if name in dropped]
    if not delete:
        return list(workdirs), list(names)
    return _without(workdirs, delete), _without(names, delete)


def _run_folders(workdirs: Sequence[str], names: Sequence[str], env: Effects) -> list[str]:
    """``R/commandLineInterface.R:161-176``: re-map coupled jobs, keep existing paths, ``sort(unique())``.

    The loop runs over ``1:length(runnames)``, which is ``c(1, 0)`` for no names, so with work directories but
    no names ``runnames[[1]]`` fails with ``subscript out of bounds``; a name without a work directory fails
    the same way in ``myruns[[i]]`` (BUG-028). ``grepl`` forces its ``x`` before its ``pattern``, so the
    work directory is read first.
    """
    if not names:
        raise SlurmError(SUBSCRIPT_OUT_OF_BOUNDS, call=RUNNAMES_CALL)
    coupled: list[str] = []
    remapped: list[int] = []
    for index, name in enumerate(names):
        workdir = _r_at(workdirs, index, call=MYRUNS_CALL)
        if not (r_grepl(name, workdir) or _MAGPIE_JOB in name):
            coupled.append(f"{workdir}/output/{name}")
            remapped.append(index)
    candidates = _without(workdirs, remapped) + coupled
    existing = [path for path in candidates if _r_file_exists(path, env)]
    return sort_like_r(dict.fromkeys(existing))


def _r_file_exists(path: str, env: Effects) -> bool:
    """``file.exists(path)``: ``FALSE`` for ``""`` (the work directory of a ``sacct`` step), else probed."""
    return path != "" and env.exists(path)
