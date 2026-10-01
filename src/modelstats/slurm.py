"""SLURM queries: ``foundInSlurm`` and the ``rs -C`` run list.

Ports ``R/foundInSlurm.R`` (:func:`found_in_slurm`) and the ``opt$current`` branch of
``R/commandLineInterface.R`` lines 139 to 177 (:func:`current_runs`) line by line, with the
R bugs the register lists for them kept for parity: substring matching of the directory
and the run name (BUG-023), ``"<n> users"`` counting jobs (BUG-035), no ``pending`` /
``startup`` suffix on the multi-hit paths (BUG-019), ``PENDING [A-Za-z]*$`` missing a QOS
with digits (BUG-031), a comma-separated user list never matching ``^user `` (BUG-009),
two separate ``squeue`` queries for ``-C`` (BUG-028) and the SLURM job name used as an
unescaped regular expression (BUG-008).

R facts reproduced here (R 4.6.1, verified 2026-10-01; the unit tests pin them):

- ``system(cmd, intern = TRUE)`` returns the stdout lines (:func:`modelstats.textscan.r_intern_lines`)
  and leaves the child's stderr on R's stderr; a non-zero exit status keeps the lines and
  raises the warning ``running command '<cmd>' had status <n>``; status 127 (the shell
  could not run the command) is the error ``error in running command``; a child killed by
  a signal gives the lines without a warning. :func:`system_intern` does the same, the
  warning being an :class:`RWarning` (``warnings.warn``) that carries R's deparsed call so
  the CLI can print it the way ``Rscript`` does.
- ``normalizePath(p)`` expands a leading ``~``, then resolves the path with ``realpath()``
  when it exists (symlinks, ``..`` and trailing slashes go) and otherwise keeps it as
  given, a partially existing path included (:func:`normalize_path`).
- ``basename`` / ``dirname`` drop trailing slashes first (``runfolders.r_basename``,
  :func:`r_dirname`); ``strsplit(x, " ")`` drops exactly one trailing empty piece and gives
  ``character(0)`` for ``""`` (:func:`r_strsplit_space`); ``[[k]]`` beyond the length is
  ``subscript out of bounds``.
- ``grepl(pattern, x)`` uses TRE's extended syntax. :func:`r_regex` translates the
  constructs that TRE and :mod:`re` read differently (POSIX classes, an unmatched ``)``
  and a dangling ``*`` ``+`` ``?`` or bound, which TRE accepts; ``{}`` bounds, which TRE
  validates; lookarounds and collating elements, which TRE rejects) and raises R's
  ``invalid regular expression '<p>', reason '<TRE reason>'`` after the
  ``TRE pattern compilation error '<reason>'`` warning. Approximate-matching bounds such
  as ``{-1}`` and the ``{,}`` quirk are not emulated (documented in the tests).
"""

from __future__ import annotations

import datetime as dt
import os
import re
import sys
import warnings
from collections.abc import Sequence

from modelstats.choose import _tre_to_re
from modelstats.env import Effects, r_sort
from modelstats.errors import RParityError, RWarning
from modelstats.runfolders import r_basename
from modelstats.textscan import r_intern_lines

__all__ = [
    "RWarning",
    "SQUEUE_ALL_ARGV",
    "SQUEUE_ALL_COMMAND",
    "current_runs",
    "found_in_slurm",
    "normalize_path",
    "r_dirname",
    "r_grepl",
    "r_regex",
    "r_strsplit_space",
    "system_intern",
]

#: ``squeue -h -o '%u %Z %j %M %T %q'`` as R writes it (the shell command string) and as the shell runs it.
SQUEUE_ALL_COMMAND = "squeue -h -o '%u %Z %j %M %T %q'"
SQUEUE_ALL_ARGV: tuple[str, ...] = ("squeue", "-h", "-o", "%u %Z %j %M %T %q")
SQUEUE_ALL_CALL = "system(\"squeue -h -o '%u %Z %j %M %T %q'\", intern = TRUE)"

_SQUEUE_Z_CALL = 'system(paste0("squeue -u ", opt$user, " -h -o \'%Z\'"), intern = TRUE)'
_SQUEUE_J_CALL = 'system(paste0("squeue -u ", opt$user, " -h -o \'%j\'"), intern = TRUE)'
_SACCT_WORKDIR_CALL = 'system(paste(sacctcode, "--format WorkDir"), intern = TRUE)'
_SACCT_JOBNAME_CALL = 'system(paste(sacctcode, "--format JobName"), intern = TRUE)'
_COUPLED_GREPL_CALL = "grepl(runnames[[i]], myruns[[i]])"
_YOURRUN_GREPL_CALL = 'grepl(paste0("^", user, " "), squeuefiltered)'

_COUPLED_NAME = re.compile(r"-(rem|mag)-[0-9]+$")
_MAG_NAME = re.compile(r"-mag-[0-9]+$")
_PENDING = re.compile(r"PENDING [A-Za-z]*$")
_STARTUP = re.compile(r"[0-5]:[0-9]{2}")


def _effects(effects: Effects | None) -> Effects:
    if effects is not None:
        return effects
    from modelstats.env import default_effects

    return default_effects()


# --- R primitives ------------------------------------------------------------------------------


def system_intern(command: str, argv: Sequence[str], call: str, effects: Effects | None = None) -> list[str]:
    """``system(command, intern = TRUE)``: the stdout lines, the child's stderr passed through.

    ``command`` is the shell command string R builds (it only appears in the warning text),
    ``argv`` is what the shell makes of it (what :meth:`Effects.run` executes) and ``call``
    the deparsed R call for the warning. Exit status 127 raises
    ``RParityError('error in running command')``; any other non-zero status warns with
    ``running command '<command>' had status <n>`` and still returns the lines.
    """
    proc = _effects(effects).run(list(argv))
    if proc.stderr:
        sys.stderr.write(proc.stderr)
        sys.stderr.flush()
    status = proc.returncode
    if status == 127:
        raise RParityError("error in running command")
    if status > 0:
        warnings.warn(RWarning(call, f"running command '{command}' had status {status}"), stacklevel=2)
    return r_intern_lines(proc.stdout)


def normalize_path(path: str, effects: Effects | None = None) -> str:
    """``suppressWarnings(normalizePath(path))``: tilde expansion, then ``realpath()`` for an existing path.

    A path that does not exist (a dangling symlink or a missing last component included)
    comes back as given, after the tilde expansion; ``""`` stays ``""``.
    """
    expanded = os.path.expanduser(path) if path.startswith("~") else path
    if expanded == "" or not _effects(effects).exists(expanded):
        return expanded
    return os.path.realpath(expanded)


def r_dirname(path: str) -> str:
    """R's ``dirname()``: tilde expansion, trailing slashes dropped, the part before the last ``/``.

    ``dirname("a/b/")`` is ``"a"``, ``dirname("a")`` is ``"."``, ``dirname("/a")`` and
    ``dirname("//a")`` are ``"/"``, ``dirname("/")`` is ``"/"`` and ``dirname("")`` is ``""``.
    """
    if path == "":
        return ""
    expanded = os.path.expanduser(path) if path.startswith("~") else path
    stripped = expanded.rstrip("/")
    if stripped == "":
        return "/"
    if "/" not in stripped:
        return "."
    head = stripped.rsplit("/", 1)[0].rstrip("/")
    return head if head else "/"


def r_strsplit_space(text: str) -> list[str]:
    """``strsplit(text, " ")[[1]]``: ``""`` gives ``[]``; one trailing empty piece is dropped, leading ones kept."""
    pieces = text.split(" ")
    if pieces and pieces[-1] == "":
        pieces.pop()
    return pieces


def _r_element(values: Sequence[str], k: int) -> str:
    """``values[[k]]`` (1-based): ``subscript out of bounds`` beyond the length."""
    if k < 1 or k > len(values):
        raise RParityError("subscript out of bounds")
    return values[k - 1]


# --- TRE regular expressions (R's grepl) ---------------------------------------------------------

_BOUND_BODY = re.compile(r"(\d*)(?:(,)(\d*))?")
_RE_DUP_MAX = 255
_INLINE_FLAGS = re.compile(r"\(\?[a-zA-Z-]+\)")
# Python's re.error messages and the TRE reason R prints for the same defect.
_TRE_REASONS: tuple[tuple[str, str], ...] = (
    ("unterminated character set", "Missing ']'"),
    ("missing ), unterminated subpattern", "Missing ')'"),
    ("nothing to repeat", "Invalid use of repetition operators"),
    ("multiple repeat", "Invalid use of repetition operators"),
    ("bad escape (end of pattern)", "Trailing backslash"),
    ("bad character range", "Invalid character range"),
    ("invalid group reference", "Invalid back reference"),
    ("min repeat greater than max repeat", "Invalid contents of {}"),
)
_TRE_OWN_REASONS = frozenset(
    {
        "Missing ']'",
        "Missing ')'",
        "Missing '}'",
        "Invalid contents of {}",
        "Invalid use of repetition operators",
        "Invalid regexp",
        "Unknown character class name",
        "Unknown collating element",
        "Trailing backslash",
    }
)


def _bracket_end(source: str, start: int) -> int:
    """The index of the ``]`` closing the bracket expression that opens at ``start``."""
    i = start + 1
    if i < len(source) and source[i] == "^":
        i += 1
    if i < len(source) and source[i] == "]":
        i += 1
    while i < len(source):
        if source[i] == "]":
            return i
        if source[i] == "[" and i + 1 < len(source) and source[i + 1] in ".=":
            raise re.error("Unknown collating element")
        i += 1
    raise re.error("Missing ']'")


def _bound(source: str, start: int) -> tuple[str, int]:
    """Validate the ``{...}`` bound at ``start`` like TRE; returns the bound text and the index after it."""
    close = source.find("}", start)
    if close < 0:
        raise re.error("Missing '}'")
    body = source[start + 1 : close]
    match = _BOUND_BODY.fullmatch(body)
    if match is None:
        raise re.error("Invalid contents of {}")
    low, comma, high = match.group(1), match.group(2), match.group(3)
    if low == "" and comma is None:
        raise re.error("Invalid contents of {}")
    minimum = int(low) if low else -1
    maximum = (int(high) if high else -1) if comma else minimum
    if (maximum >= 0 and minimum > maximum) or maximum > _RE_DUP_MAX:
        raise re.error("Invalid contents of {}")
    return source[start : close + 1], close + 1


def _tre_to_python(pattern: str) -> str:
    """Rewrite a TRE pattern so that :mod:`re` reads it the way TRE does (see the module docstring)."""
    source = _tre_to_re(pattern)  # POSIX classes; raises re.error("Unknown character class name")
    out: list[str] = []
    atom: int | None = None  # index in ``out`` where the atom a quantifier would apply to starts
    quant: str | None = None  # "op": *, + or ? applied; "bound": {..} applied; "lazy": ? after one; "dangling"
    groups: list[int] = []
    i, n = 0, len(source)
    while i < n:
        ch = source[i]
        if ch == "\\":
            if i + 1 >= n:
                raise re.error("Trailing backslash")
            atom, quant = len(out), None
            out.append(source[i : i + 2])
            i += 2
        elif ch == "[":
            end = _bracket_end(source, i)
            atom, quant = len(out), None
            out.append(source[i : end + 1])
            i = end + 1
        elif ch == "(":
            if source.startswith("(?", i):
                flags = _INLINE_FLAGS.match(source, i)
                if flags is not None:  # (?i): a flag group, not a group
                    out.append(flags.group(0))
                    atom, quant = None, None
                    i = flags.end()
                    continue
                if not source.startswith("(?:", i):  # lookarounds, named groups: TRE rejects them
                    raise re.error("Invalid regexp")
                ch = "(?:"
            groups.append(len(out))
            out.append(ch)
            atom, quant = None, None
            i += len(ch)
        elif ch == ")":
            if groups:
                atom, quant = groups.pop(), None
                out.append(ch)
            else:  # an unmatched ) is a literal in TRE
                atom, quant = len(out), None
                out.append("\\)")
            i += 1
        elif ch in "|^$":
            out.append(ch)
            atom, quant = None, None
            i += 1
        elif ch in "*+?":
            if atom is None:  # nothing to repeat: TRE ignores the operator, twice in a row it rejects it
                if quant == "dangling" and ch != "?":
                    raise re.error("Invalid use of repetition operators")
                quant = "dangling"
            elif quant is None:
                quant = "op"
                out.append(ch)
            elif ch == "?" and quant != "lazy":
                quant = "lazy"
                out.append(ch)
            else:
                raise re.error("Invalid use of repetition operators")
            i += 1
        elif ch == "{":
            text, i = _bound(source, i)
            if atom is None:
                quant = "dangling"
                continue
            if quant is not None:  # TRE applies a bound to an already quantified atom; re needs a group
                out.insert(atom, "(?:")
                out.append(")")
            out.append(text)
            quant = "bound"
        else:
            atom, quant = len(out), None
            out.append(ch)
            i += 1
    if groups:
        raise re.error("Missing ')'")
    return "".join(out)


def _tre_reason(exc: re.error) -> str:
    message = exc.msg
    if message in _TRE_OWN_REASONS:
        return message
    for prefix, reason in _TRE_REASONS:
        if message.startswith(prefix):
            return reason
    return "Invalid regexp"


def r_regex(pattern: str, *, call: str) -> re.Pattern[str]:
    """Compile ``pattern`` as R's ``grepl(pattern, x)`` reads it (TRE extended syntax).

    A pattern TRE rejects raises ``RParityError("invalid regular expression '<pattern>',
    reason '<reason>'")`` right after the ``TRE pattern compilation error '<reason>'``
    :class:`RWarning` with the deparsed ``call``, like R.
    """
    try:
        translated = _tre_to_python(pattern)
        with warnings.catch_warnings():
            warnings.simplefilter("ignore", FutureWarning)  # re's "possible nested set" hint; R prints nothing
            return re.compile(translated)
    except re.error as exc:
        reason = _tre_reason(exc)
        warnings.warn(RWarning(call, f"TRE pattern compilation error '{reason}'"), stacklevel=2)
        raise RParityError(f"invalid regular expression '{pattern}', reason '{reason}'") from None


def r_grepl(pattern: str, text: str, *, call: str) -> bool:
    """``grepl(pattern, text)`` for one string (see :func:`r_regex`)."""
    return r_regex(pattern, call=call).search(text) is not None


# --- foundInSlurm ---------------------------------------------------------------------------------


def found_in_slurm(mydir: str = ".", user: str | None = None, effects: Effects | None = None) -> str:
    """``foundInSlurm(mydir, user)``: the run's SLURM state from ``squeue -h -o '%u %Z %j %M %T %q'``.

    Lines containing the normalised directory (its grand-parent for a ``-rem-N`` /
    ``-mag-N`` path) and then ``"<runname> "`` as substrings are the hits; a ``-mag-N``
    run without a hit is looked up again under its ``-rem-N`` name and ``dirname(mydir)``.
    One hit gives the QOS for the user's own job (``^<user> ``) or the job's user
    otherwise, plus ``" startup"`` (elapsed ``[0-5]:[0-9]{2}`` and not pending) or
    ``" pending"`` (line ending in ``PENDING <letters>``); several hits of one user give
    that user; several hits give ``"<n> users"``; no hit gives ``"no"``.
    """
    eff = _effects(effects)
    if user is None:
        user = eff.user
    mydir = normalize_path(mydir, eff)
    runname = r_basename(mydir)
    if _COUPLED_NAME.search(mydir):
        mydir = r_dirname(r_dirname(mydir))

    squeueresult = system_intern(SQUEUE_ALL_COMMAND, SQUEUE_ALL_ARGV, SQUEUE_ALL_CALL, eff)
    squeuefiltered = [line for line in squeueresult if mydir in line]
    squeuefiltered = [line for line in squeuefiltered if runname + " " in line]
    # try to find REMIND slurm job corresponding to coupled MAgPIE run
    if not squeuefiltered and _MAG_NAME.search(runname):
        runrem = runname.replace("-mag-", "-rem-")
        squeuefiltered = [line for line in squeueresult if runrem + " " in line]
        parent = r_dirname(mydir)
        squeuefiltered = [line for line in squeuefiltered if parent in line]
    if len(squeuefiltered) == 1:
        line = squeuefiltered[0]
        tokens = r_strsplit_space(line)
        reversed_tokens = tokens[::-1]
        time = _r_element(reversed_tokens, 3)
        qos = _r_element(reversed_tokens, 1)
        yourrun = r_grepl("^" + user + " ", line, call=_YOURRUN_GREPL_CALL)
        pending = " pending" if _PENDING.search(line) else None
        startup = " startup" if _STARTUP.fullmatch(time) and pending is None else None
        if yourrun:
            return qos + (startup or "") + (pending or "")
        runuser = _r_element(tokens, 1)
        return runuser + (startup or "") + (pending or "")
    # sapply(strsplit(squeuefiltered, " "), `[`, 1): the first token, NA for an empty line
    first_tokens = [(pieces[0] if (pieces := r_strsplit_space(line)) else None) for line in squeuefiltered]
    if len(set(first_tokens)) == 1:
        return _r_element(r_strsplit_space(squeuefiltered[0]), 1)
    if len(squeuefiltered) > 1:
        return f"{len(squeuefiltered)} users"
    return "no"


# --- rs -C ----------------------------------------------------------------------------------------


def current_runs(user: str, daysback: int = 0, effects: Effects | None = None) -> list[str]:
    """The ``-C`` run list of ``R/commandLineInterface.R`` lines 139 to 177.

    ``squeue -u <user> -h -o '%Z'`` and ``... '%j'`` (two queries, BUG-028), with
    ``daysback > 0`` the two ``sacct -u <user> -s cd,f,cancelled,timeout,oom -S <today - N>
    -E now -P -n --format WorkDir|JobName`` queries appended; ``batch`` jobs are dropped
    (``default`` too when any job name contains ``mag-run``); a job whose name, read as a
    regular expression (BUG-008), does not match its work directory and is not a
    ``mag-run`` is re-mapped to ``<workdir>/output/<name>``; paths that do not exist are
    dropped and the rest is ``sort(unique(...))`` in R's collation. The list may be empty;
    the CLI prints the "No ... runs found" message. A job name that is not a valid TRE
    regular expression raises ``RParityError`` like R.
    """
    eff = _effects(effects)
    myruns = system_intern(
        f"squeue -u {user} -h -o '%Z'", ["squeue", "-u", user, "-h", "-o", "%Z"], _SQUEUE_Z_CALL, eff
    )
    runnames = system_intern(
        f"squeue -u {user} -h -o '%j'", ["squeue", "-u", user, "-h", "-o", "%j"], _SQUEUE_J_CALL, eff
    )

    if daysback > 0:
        since = (eff.today() - dt.timedelta(days=daysback)).isoformat()
        sacctcode = f"sacct -u {user} -s cd,f,cancelled,timeout,oom -S {since} -E now -P -n"
        sacct_argv = ["sacct", "-u", user, "-s", "cd,f,cancelled,timeout,oom", "-S", since, "-E", "now", "-P", "-n"]
        myruns += system_intern(
            f"{sacctcode} --format WorkDir", [*sacct_argv, "--format", "WorkDir"], _SACCT_WORKDIR_CALL, eff
        )
        runnames += system_intern(
            f"{sacctcode} --format JobName", [*sacct_argv, "--format", "JobName"], _SACCT_JOBNAME_CALL, eff
        )

    if any("mag-run" in name for name in runnames):
        deleteruns = {i for i, name in enumerate(runnames) if name in ("default", "batch")}
    else:
        deleteruns = {i for i, name in enumerate(runnames) if name == "batch"}
    if deleteruns:
        myruns = [run for i, run in enumerate(myruns) if i not in deleteruns]
        runnames = [name for i, name in enumerate(runnames) if i not in deleteruns]

    # add REMIND-MAgPIE coupled runs where run directory is not the output directory
    # these lines also drop all other slurm jobs such as remind preprocessing etc.
    if len(myruns) > 0:
        coupled: list[str] = []
        rem: set[int] = set()
        for i in range(1, len(runnames) + 1) if runnames else (1, 0):  # 1:length(runnames) is c(1, 0) when empty
            name = _r_element(runnames, i)
            run = _r_element(myruns, i)
            if not (r_grepl(name, run, call=_COUPLED_GREPL_CALL) or "mag-run" in name):
                coupled.append(f"{run}/output/{name}")  # for coupled runs in parallel mode
                rem.add(i - 1)
        if rem:
            myruns = [run for i, run in enumerate(myruns) if i not in rem]  # remove coupled parent-job and the rest
            myruns = myruns + coupled  # add coupled paths
        myruns = [run for run in myruns if eff.exists(run)]  # keep only existing paths
        myruns = r_sort(dict.fromkeys(myruns))
    return myruns
