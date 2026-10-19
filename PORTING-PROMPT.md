Migrate the project from R to Python. For every R-file, read the instructions in PORTING.md and then proceed as follows:

1. Read `R/<file>.R` top to bottom; list the R functions, their callers.
2. Create `src/modelstats/<module>.py` with the module docstring (`R source: R/<file>.R`, listing deviations and fixes applied).
3. Translate function by function, top-down, with §2.1 names and §4 idioms; keep comment blocks as
   private helper names; copy user-visible strings verbatim.
4. Route every file read, process call, clock read and console write through the §3 modules; add a
   missing helper **there**, with its own test.
5. Write `tests/test_<module>.py`: one test per branch of the R code you can reach with synthetic inputs,
   one per applied fix, one per recorded quirk you kept.
6. Run two different subagents using Opus 5.5 xhigh:
   - One verifying the port against the R behaviour.
   - One verifying the Python code and suggesting improvements. This also runs `uv run ruff check . && uv check && uv run pytest`.
7. Implement suggested improvements from the review subagent. Run the implementation-review loop until both subagents are satisfied.
