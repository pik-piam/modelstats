"""Automated model tests (AMT) for REMIND: the ``modeltests`` command and its steps.

The REMIND-only rewrite of ``R/modeltests.R`` (``migration/06-user-requests.md`` sections 2, 3 and 4,
decisions D-24 to D-27): :mod:`modelstats.amt.cli` is the console script with the commands ``start``,
``evaluate``, ``run``, ``status`` and ``reset``; :mod:`modelstats.amt.state` the JSON state machine,
the lock and the migration from the R files; :mod:`modelstats.amt.logs` the one log per cycle;
:mod:`modelstats.amt.start` and :mod:`modelstats.amt.evaluate` the two steps on one
:class:`modelstats.amt.context.AmtContext`; :mod:`modelstats.amt.previous`, :mod:`modelstats.amt.readme`,
:mod:`modelstats.amt.bridges` and :mod:`modelstats.amt.notify` their helpers. ``docs/amt.md`` is the
operator's guide (cron lines, the state file, recovery, the log, the differences from the R version).
The MAgPIE weekly tests stay with the R package (D-27).
"""

from modelstats.amt.cli import app, main

__all__ = ["app", "main"]
