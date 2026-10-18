# PORTING-BUGS.md — suspected bugs and quirks of the R code, and what the port does with them

One row per suspicion. Line numbers refer to the R files of this branch (modelstats 0.31.0, commit `eaf4151`).

**Port decision** vocabulary:

- `fixed` — the port deliberately behaves differently.
- `fixed (flagged)` — a deliberate fix whose intended semantics is a judgement call; the user may veto it
  before the final golden check.
- `vanishes` — the defect is an artefact of R evaluation, shell strings or the data-frame representation
  and has no counterpart in the Python design; the port behaves "correctly" without a decision.
- `kept` — the port reproduces the R behaviour on purpose (model semantics, or not obvious enough).
- `n/a` — withdrawn, not a bug, or a harness limitation.
- `open` — found while porting, not yet judged; the porter keeps the R behaviour until it is.

Severity: `high` (wrong status shown to users or silent data loss), `medium` (wrong value in an edge
case), `low` (cosmetic, dead code, robustness).

**Adding a row**: next free `BUG-` id, the R location, one sentence of symptom, severity, `open`, and the
module/test that pins the R behaviour in the port. Never fix silently.


| Id | Where (R) | Symptom | Severity | Port decision | Pinned by |
|---|---|---|---|---|---|
