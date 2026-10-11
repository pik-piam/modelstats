r"""Mattermost notifications of the AMT: the REMIND status message and the failure message.

``R/modeltests.R:449-486`` composes, once the README is written, one chat message per evaluation and
posts it to a Mattermost webhook when a token (the webhook URL) was given. The rewrite
(``migration/06-user-requests.md`` sections 3.2, 4.1 and 4.2, decisions D-24 and D-15) keeps the REMIND
text and replaces the transport:

* :func:`status_message` is the REMIND message of ``:453-466``, line for line: the status line with
  ``today`` and the runs path, the summary, the ``make test-full`` result, the git information inside a
  code fence, the ``rs -A`` table (``capture.output(loopRuns(runsStarted, user = NULL, colors = FALSE,
  sortbytime = FALSE))``) and the sanity table (``capture.output(getSanityChecks())``) inside code fences,
  and the scenarios that did not start. The elements are joined with newlines as ``paste0(message,
  collapse = "\n")`` does (``:483``); :func:`captured_lines` splits a captured stream the way
  ``capture.output`` returns its lines.
* :func:`failure_message` is new (``06`` section 3.2): the message of the ``failed`` state, with the
  command, the error and the log path. The R version stopped silently.
* :func:`send` posts ``{"text": message}`` through ``Effects.post_json`` (``json.dumps`` instead of the
  string-built body of ``.mattermostBotMessage``, ``:57-61``, register BUG-015, D-15 closed). The URL is
  read from the environment variable named on the command line (``--mattermost-url-env``, default
  :data:`MATTERMOST_URL_VARIABLE`), never from a file this module reads; an unset variable means no call,
  as ``is.null(mattermostToken)`` did (``:449``). ``post_json`` never raises for a failed exchange
  (``modelstats.env.HttpResponse``): a status outside 2xx, or status ``0`` when no exchange happened, is
  logged as an error and the evaluation goes on, as it did after the failed ``curl`` of the R version
  (``system(cmd, intern = TRUE)`` only warned; golden ``amt/remind-evaluate-curlfail`` records status
  ``ok``). The URL carries the webhook secret and is never written to the log.

The logger is ``modelstats.amt.notify``, a child of the cycle logger ``modelstats.amt``
(``modelstats.amt.logs.LOGGER_NAME``), so the records land in the cycle log (``06`` 3.3).

Differences to the R goldens under ``migration/goldens/amt/remind-*`` (``06`` section 4.4, item 3):

* The tables are captured without colour unless the caller injects a :class:`modelstats.colors.Crayon`:
  R left the escapes to crayon's terminal detection, and the sandbox forced 256 colours
  (``R_CLI_NUM_COLORS``), which is why the recorded ``mattermost.json`` carries ``\x1b[4m`` inside the
  fences; the cron job of the real AMT has no terminal and sent none. The golden comparison injects the
  sandbox's crayon state (implementation artefact of the recording, D-24).
* The "did not start" block is appended whenever the list is not empty. R appended it only when the
  count-based check of ``:370`` had fired (``exists("runsNotStarted")``, ``:464``), so a scenario hidden
  by another run that started twice got no block (BUG-024); the rewrite's set difference (``06`` 4.2)
  has no count.
* The runs path is a parameter (the checkout of ``--dir``), where R printed the cluster constant
  ``/p/projects/remind/modeltests/remind`` (``:456``).
* The sanity table lists the cycle's started runs in the checkout's ``output/`` (``capture_tables`` takes
  the run paths and the output directory), where R's ``getSanityChecks()`` without arguments read the
  hard-coded cluster ``output/`` and its ``runcode.rds`` (``R/getSanityChecks.R:22-24``): a
  path-arithmetic artefact of the kind BUG-013 names (D-24), which described another checkout for any
  ``--dir`` but the cluster one. The text is the same for the cluster checkout (the golden comparison).
"""

from __future__ import annotations

import io
import json
import logging
import os
from collections.abc import Sequence
from dataclasses import dataclass
from datetime import date, datetime
from enum import Enum
from typing import Final, Protocol, TextIO

from modelstats.amt.readme import CODE_FENCE, NOT_STARTED_TITLE
from modelstats.colors import Crayon
from modelstats.env import Effects, HttpResponse, PathLike
from modelstats.loop_runs import loop_runs
from modelstats.sanity import get_sanity_checks

__all__ = [
    "MATTERMOST_URL_VARIABLE",
    "NEXT_STEPS",
    "RS_A_HEADER",
    "SANITY_HEADER",
    "CapturedTables",
    "Delivery",
    "FailureReport",
    "NotifyingEffects",
    "ReadsVariables",
    "StatusReport",
    "capture_tables",
    "captured_lines",
    "failure_message",
    "failure_reason",
    "payload",
    "payload_json",
    "send",
    "send_to",
    "status_message",
    "webhook_url",
]

MATTERMOST_URL_VARIABLE: Final = "AMT_MATTERMOST_URL"
"""The default environment variable holding the webhook URL (``06`` section 3.1, ``--mattermost-url-env``)."""

RS_A_HEADER: Final = "`rs -A` returns:"
"""``R/modeltests.R:460``."""
SANITY_HEADER: Final = "Sanity checks (`rs -s`) return:"
"""``R/modeltests.R:462``."""
NEXT_STEPS: Final = (
    "Next steps: `modeltests status` shows the state, `modeltests evaluate` retries the evaluation, "
    "`modeltests reset` clears the failed state."
)
"""The last line of the failure message: the ``06`` section 3.1 commands that leave the ``failed`` state."""
_STATUS_LINE: Final = (
    "Please find below the status of the REMIND automated model tests (AMT) of {today}. "
    "Runs are here: `{runs}`."
)
"""``R/modeltests.R:455-456``, the cluster constant replaced by the runs path."""
_FAILURE_LINE: Final = (
    "The REMIND automated model tests (AMT) failed: `modeltests {command}` stopped with an error{when}. "
    "Runs are here: `{runs}`."
)
_TIMESTAMP: Final = "%Y-%m-%d %H:%M:%S"
"""The clock format of R's ``message()`` lines (``R/modeltests.R:178``)."""
_TEXT_KEY: Final = "text"
_BODY_EXCERPT: Final = 200
"""How much of a failed response's body the log keeps (an HTML error page is not worth more)."""


# --- the environment the sender needs ------------------------------------------------------------------


class ReadsVariables(Protocol):
    """The one door of :class:`modelstats.env.Effects` the URL lookup uses: the environment variables."""

    def getenv(self, name: str) -> str:
        """``Sys.getenv(name)``: the value, or ``""`` when unset."""
        ...


class NotifyingEffects(ReadsVariables, Protocol):
    """The two doors of :class:`modelstats.env.Effects` the sender uses: the variables and HTTP.

    ``post_json`` is the mutating call of the phase-5 ``Effects`` (``03`` section 2.3): one POST of the
    JSON-encoded ``payload`` to ``url``, answered with the :class:`modelstats.env.HttpResponse` that
    ``Effects.post_json`` specifies (never raised for a failed exchange).
    """

    def post_json(self, url: str, payload: object) -> HttpResponse:
        """POST ``payload`` as JSON to ``url``; the response, status ``0`` when no exchange happened."""
        ...


# --- the tables (R/modeltests.R:453-454) --------------------------------------------------------------


@dataclass(frozen=True)
class CapturedTables:
    """What ``capture.output`` saw: the ``rs -A`` table and the sanity table, as the streams were written."""

    rs: str
    sanity: str


def capture_tables(
    runs_started: Sequence[PathLike],
    effects: Effects,
    *,
    output_dir: PathLike,
    user: str | None = None,
    err: TextIO | None = None,
    crayon: Crayon | None = None,
) -> CapturedTables:
    """``R/modeltests.R:453-454``: the two tables of the message, captured from their output streams.

    ``loopRuns(runsStarted, user = NULL, colors = FALSE, sortbytime = FALSE)`` prints the started runs in
    the order given, without the legend and the row colours (the header underline is crayon's, see
    ``crayon``). The sanity table is ``getSanityChecks()``'s text for the cycle's started runs: the
    ``Results from <output_dir>/ `` line of ``R/getSanityChecks.R:23`` (``cat`` separates its arguments
    with a space, hence the blank before the newline, BUG-065) followed by ``getSanityChecks(runs_started)``
    over the explicit run paths; R's call without arguments read the fixed cluster ``output/`` and its
    ``runcode.rds`` (``:22-24``), which is the same text for the cluster checkout and another checkout's
    runs for any other ``--dir`` (D-24, see the module docstring). ``user`` is the SLURM user of the
    ``inSlurm`` column (``None``: the process user, R's ``NULL``); ``err`` receives the ``try()``
    diagnostics of unreadable runs, which ``capture.output`` never caught (default: the process's stderr);
    ``crayon`` defaults to :meth:`modelstats.colors.Crayon.disabled`, no escape sequence at all, the state
    crayon reports without a terminal.
    """
    paint = crayon if crayon is not None else Crayon.disabled()
    rs = io.StringIO()
    loop_runs(
        list(runs_started),
        user,
        colors=False,
        sortbytime=False,
        effects=effects,
        out=rs,
        err=err,
        crayon=paint,
    )
    sanity = io.StringIO()
    sanity.write(f"Results from {os.fspath(output_dir)}/ \n")
    get_sanity_checks(list(runs_started), effects, sanity, crayon=paint)
    return CapturedTables(rs=rs.getvalue(), sanity=sanity.getvalue())


def captured_lines(text: str) -> list[str]:
    """The character vector ``capture.output`` returns for a stream that wrote ``text``.

    One element per line, a final newline closes the last line (no empty element after it), an
    incomplete last line is kept, an empty stream is ``character(0)``.
    """
    if not text:
        return []
    lines = text.split("\n")
    if lines[-1] == "":
        lines.pop()
    return lines


# --- the status message (R/modeltests.R:453-466) ------------------------------------------------------


@dataclass(frozen=True)
class StatusReport:
    """The inputs of the REMIND status message, in the vocabulary of ``evaluateRuns``.

    ``today`` is R's ``today`` (``:176``, the evaluation date), ``runs_path`` the checkout whose
    ``output/`` holds the runs (R's constant, ``:456``), ``summary`` the ``Summary: ...`` line (``:423-427``,
    ``modelstats.amt.readme.summary_line``), ``testthat_result`` the ``make test-full`` sentence
    (``:381-400``), ``git_info`` the lines of R's ``gitInfo`` vector (``:219-221``,
    ``modelstats.amt.readme.git_info_lines``: ``Tested commit: ...``, ``The test of ... contains these
    merges:`` and one line per merge), ``tables`` the captured tables, ``runs_not_started`` the scenario
    names that did not start (``:373``).
    """

    today: date
    runs_path: str
    summary: str
    testthat_result: str
    git_info: Sequence[str]
    tables: CapturedTables
    runs_not_started: Sequence[str] = ()


def _fenced(lines: Sequence[str]) -> list[str]:
    """``c("```", lines, "```")``."""
    return [CODE_FENCE, *lines, CODE_FENCE]


def status_message(report: StatusReport) -> str:
    """The REMIND message of ``R/modeltests.R:453-466``, joined with newlines (``:483``).

    The elements, in R's order: the status line (``:455-456``), the summary (``:457``), the testthat
    result (``:458``), the git information in a code fence (``:459``), the ``rs -A`` header and the
    captured table in a code fence (``:460-461``), the sanity header and table in a code fence
    (``:462-463``), and, when there are any, the scenarios that did not start, one per line after the
    title (``:464-466``, the title of the README block, ``:375``).
    """
    lines = [
        _STATUS_LINE.format(today=report.today.isoformat(), runs=report.runs_path),
        report.summary,
        report.testthat_result,
        *_fenced(report.git_info),
        RS_A_HEADER,
        *_fenced(captured_lines(report.tables.rs)),
        SANITY_HEADER,
        *_fenced(captured_lines(report.tables.sanity)),
    ]
    if report.runs_not_started:
        lines += [NOT_STARTED_TITLE, *report.runs_not_started]
    return "\n".join(lines)


# --- the failure message (06 section 3.2, the failed state) -------------------------------------------


@dataclass(frozen=True)
class FailureReport:
    """What the ``failed`` state records (``06`` section 3.2, ``last_error``) plus the runs path.

    The fields mirror ``modelstats.amt.state.LastError``: ``command`` is the subcommand that crashed
    (``start`` or ``evaluate``), ``message`` the error's message, ``at`` when (``None``: not recorded),
    ``log`` the cycle log that holds the traceback (``None``: no log file). ``runs_path`` is the checkout.
    """

    command: str
    message: str
    runs_path: str
    at: datetime | None = None
    log: PathLike | None = None


def failure_message(report: FailureReport) -> str:
    """The message of the ``failed`` state: command and time, the error in a code fence, the log, next steps.

    The error's lines are fenced as :func:`captured_lines` splits them (a multi-line message keeps its
    lines); the ``Log:`` line is left out when no log path is known.
    """
    when = f" at {report.at.strftime(_TIMESTAMP)}" if report.at is not None else ""
    lines = [
        _FAILURE_LINE.format(command=report.command, when=when, runs=report.runs_path),
        *_fenced(captured_lines(report.message)),
    ]
    if report.log is not None:
        lines.append(f"Log: `{os.fspath(report.log)}`")
    lines.append(NEXT_STEPS)
    return "\n".join(lines)


# --- the transport (R/modeltests.R:57-61 and :449, :482-485) -------------------------------------------


class Delivery(Enum):
    """What :func:`send` did with a message."""

    SENT = "sent"
    NOT_CONFIGURED = "not configured"
    FAILED = "failed"


def webhook_url(effects: ReadsVariables, variable: str = MATTERMOST_URL_VARIABLE) -> str | None:
    """The webhook URL from the environment variable ``variable``, ``None`` when it is unset or blank.

    The cron script exports the variable from a file only its user can read; this code reads no file.
    Surrounding whitespace (a trailing newline from ``export X=$(cat file)`` variants) is dropped.
    """
    value = effects.getenv(variable).strip()
    return value or None


def payload(message: str) -> dict[str, str]:
    """The Mattermost incoming-webhook body: ``{"text": message}``."""
    return {_TEXT_KEY: message}


def payload_json(message: str) -> str:
    """The body as the bytes on the wire: ``json.dumps`` of :func:`payload` (quotes and newlines escaped).

    ``Effects.post_json`` encodes the payload the same way (``modelstats.env.PostRecord.raw``). The R
    version pasted the message into ``'{"text": "<message>"}'`` unescaped (``R/modeltests.R:58-59``,
    BUG-015), which is invalid JSON for every message with a newline; the recorded goldens show that.
    """
    return json.dumps(payload(message), ensure_ascii=False)


def failure_reason(response: HttpResponse) -> str:
    """One line describing a failed exchange for the log: the HTTP status or the transport error.

    The URL is not part of it (the webhook secret stays out of the log); the body is cut to
    :data:`_BODY_EXCERPT` characters.
    """
    body = response.body.strip().replace("\n", " ")
    excerpt = body if len(body) <= _BODY_EXCERPT else body[:_BODY_EXCERPT] + "..."
    if response.status == 0:
        return f"no HTTP exchange ({excerpt})"
    return f"HTTP {response.status}: {excerpt}" if excerpt else f"HTTP {response.status}"


def send(
    effects: NotifyingEffects,
    message: str,
    *,
    variable: str = MATTERMOST_URL_VARIABLE,
    log: logging.Logger | None = None,
) -> Delivery:
    """Post ``message`` to the webhook named by ``variable``; a failed exchange is logged, not raised.

    Returns :attr:`Delivery.NOT_CONFIGURED` without any call when the variable is unset (``:449``),
    :attr:`Delivery.SENT` after a 2xx answer and :attr:`Delivery.FAILED` for any other response: the
    reason (:func:`failure_reason`) is logged as an error and the caller goes on, because a chat
    notification is best effort and must not turn a finished evaluation into a failed cycle (``06``
    section 3.2; R's failed ``curl`` only warned, ``:61``). ``log`` overrides the module logger.
    """
    logger = log if log is not None else logging.getLogger(__name__)
    url = webhook_url(effects, variable)
    if url is None:
        logger.info("No Mattermost webhook URL in $%s: notification not sent", variable)
        return Delivery.NOT_CONFIGURED
    return send_to(effects, url, message, variable=variable, log=logger)


def send_to(
    effects: NotifyingEffects,
    url: str,
    message: str,
    *,
    variable: str = MATTERMOST_URL_VARIABLE,
    log: logging.Logger | None = None,
) -> Delivery:
    """Post ``message`` to ``url`` (from :func:`webhook_url`): the posting half of :func:`send`.

    ``variable`` only names the source of the URL in the log line of a failed exchange; the URL itself
    is never logged. Returns :attr:`Delivery.SENT` or :attr:`Delivery.FAILED` as :func:`send` does.
    """
    logger = log if log is not None else logging.getLogger(__name__)
    response = effects.post_json(url, payload(message))
    if not response.ok:
        logger.error(
            "Mattermost notification failed (webhook from $%s): %s", variable, failure_reason(response)
        )
        return Delivery.FAILED
    logger.info("Mattermost notification sent (%d characters, HTTP %d)", len(message), response.status)
    return Delivery.SENT
