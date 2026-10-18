## Later tasks

1. **rich.Table** for the status and sanity tables (replaces the hand-padded `format_row`; changes every
   output line, so only after the final golden check).
2. **typer/rich prompt** replacing the `chooseFromList` grammar in `_choose.py`.
3. Retire `runcode.rds` (write the runcode only in `amt-state.json`) once `rs-r -A` and `remindstatus`
   no longer read it.
4. **Final golden check** against `/home/tobias/code/modelstats/migration/goldens` (R oracle, families
   `status`, `runtype`, `slurm`, `sort`, `printoutput`, `looprun`, `sanity`, `rs`, `amt`), normalising ANSI
   codes, the random hint line and the differences listed in §1.4; the harness is in
   `/home/tobias/code/modelstats/migration/harness`.
5. Cluster wrappers: `/p/projects/rd3mod/tools/rs` → the Python entry point (keep the usage log line and
   the R version as `rs-r`); the two AMT cron scripts call `modeltests`; REMIND's `readcoupled.R` and
   `remindstatus` switch from the R functions to the CLI.
6. Vignettes `rs2.Rmd`, `testingSuite.Rmd` → `docs/`.
7. Drop the R sources, `DESCRIPTION`, `NAMESPACE`, `man/`.

## Further tasks found while porting

