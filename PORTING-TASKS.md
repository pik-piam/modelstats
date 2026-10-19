## Later tasks

1. **rich.Table** for the status and sanity tables (replaces the hand-padded `format_row`; changes every
   output line, so only after the final golden check).
2. **typer/rich prompt** replacing the `chooseFromList` grammar in `_choose.py`.
3. Retire `runcode.rds` (write the runcode only in `amt-state.json`) once `rs-r -A` and `remindstatus`
   no longer read it.
4. **Final golden check** against `/home/tobias/code/modelstats/migration/goldens` — done 2026-10-19, see
   "Golden check" below; rerun it after every change that touches an output line.
5. Cluster wrappers: `/p/projects/rd3mod/tools/rs` → the Python entry point (keep the usage log line and
   the R version as `rs-r`); the two AMT cron scripts call `modeltests`; REMIND's `readcoupled.R` and
   `remindstatus` switch from the R functions to the CLI.
6. Vignettes `rs2.Rmd`, `testingSuite.Rmd` → `docs/`.
7. Drop the R sources, `DESCRIPTION`, `NAMESPACE`, `man/`.

## Golden check (2026-10-19)

The R goldens (`/home/tobias/code/modelstats/migration/goldens`, 1079 cases of the families `status`, `runtype`,
`slurm`, `sort`, `printoutput`, `looprun`, `sanity`, `rs`, `amt`) were regenerated with this port inside the
harness sandbox (`migration/harness/sandbox.sh`: read-only fixture overlay, stopped clock, fake slurm/git/curl)
by a Python twin of `make_goldens.R` that maps the R calls onto this API, and compared case by case. The tools
live next to the R harness in `/home/tobias/code/modelstats/migration/harness/`: `make_py_goldens_structured.py`
(the generator, run inside the sandbox), `dump_state.R`, `run_py_goldens_structured.sh` (the driver) and
`compare_structured.py` (the classifier). To rerun, from that folder and with the worktree synced (`uv sync`):

```
WORKTREE=/home/tobias/orca/workspaces/modelstats/Python-structured \
  bash -c 'ulimit -v 8000000; ./run_py_goldens_structured.sh status runtype slurm sort printoutput looprun sanity rs --jobs 2'
WORKTREE=/home/tobias/orca/workspaces/modelstats/Python-structured \
  bash -c 'ulimit -v 8000000; ./run_py_goldens_structured.sh amt --jobs 2 --only-ids "$(ls ../cases/amt | grep -vx magpie-start-job-in-mydir | paste -sd, -)"'
python3 -I ./compare_structured.py status runtype slurm sort printoutput looprun sanity rs amt > report.txt
```

- `status`: `get_run_status(dirs, newest_first=sort == "nf", user=, detailed=)`, rows serialised with R's
  column-presence rule; whole floats written as integers (jsonlite); `runtype`: `run_type`; `slurm`:
  `found_in_slurm`; `sort`: `_sorting.run_sort_key`; `printoutput`: `format_row` + `"\n"` (BUG-006);
  `looprun`/`sanity`: `print_run_table`/`print_sanity_checks` on a rich console with `force_terminal=True,
  color_system="256"` (crayon on) or no terminal (crayon off); `rs`: one `rs` process per case with
  `FORCE_COLOR=1` (R: `R_CLI_NUM_COLORS=256`); `amt`: `modeltests()` in-process with `_shell.post_json`
  recording the payload like the fake curl, `_shell.sleep` a no-op (R: `Sys.sleep` no-op), the README from
  `evaluate_runs()`'s return value, the state files dumped by R's own reader plus `amt-state.json`.
- Compared: JSON by key order, type and value; text after removing the SGR escapes plus the set of non-reset
  SGR codes per line (BUG-105: rich and crayon encode the same colours differently); the run order
  separately (BUG-054); `rs` stderr without the random hint line (PORTING.md §1.1); `amt` per artefact
  (trace without the `cwd` of squeue, `curl`/`sed` excluded, state files without their sha256).
- Every remaining difference is a register row (BUG-001, 002, 010, 012, 014, 017, 022, 040, 041, 049, 050,
  051, 052, 054, 058, 065, 067, 069, 070, 076, 082, 083, 084, 087, 095, 102, 105, 106) or an error message
  worded differently where both sides fail. Two trace differences are accepted without a row: `git log`
  runs in the model folder (R: in `output/`, the same repository), and the port reads each folder of
  `output/` once per evaluation where R read the started runs again (BUG-070), so it calls `squeue` less
  often.
- Not comparable: `amt/magpie-start-job-in-mydir` — the fixed wait for the default run (BUG-084) never ends
  against the harness's static fake squeue; the two `looprun` `*--crayon-off` goldens carry colour codes
  (R ignored `crayon.enabled = FALSE` under `R_CLI_NUM_COLORS=256`), the port's plain output is right.

## Further tasks found while porting

