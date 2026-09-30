# Known bugs and quirks (not fixed by the refactoring)

Suspected bugs found while refactoring the package. They were deliberately **not** fixed, because the
`rs` tool has to behave exactly as before; each entry is a candidate for a separate, reviewed change.
Line numbers refer to the refactored code on this branch. "Preserved" means the refactored code
reproduces the behaviour on purpose (verified against recorded outputs of the old code).

## Run inspection (`modelRun()`, `getRunStatus()`, `colRunType()`, `foundInSlurm()`)

| # | Where | What |
|---|-------|------|
| 1 | `R/colRunType.R:19` | `file.exists(paste0(mydir, "/", configName))` is `TRUE` for a folder *without* a config file (`paste0` with `character(0)` gives `"<dir>/"`, which exists), so the config branch is entered with `cfg = NULL` and `runTypeFromConfig()` fails with "argument is of length zero". The `full.lst` fallback below it is therefore unreachable. Preserved. |
| 2 | `R/modelRun.R` (`runFiles`), `R/colRunType.R:15` | The config file is found with the unanchored pattern `config.Rdata\|config.yml`, so `config.Rdata.bak` or `old_config.yml` match too. Two matches make `getRunStatus()` fail in `load()` with "invalid 'description' argument", and `colRunType()` fail earlier with "the condition has length > 1" (`file.exists()` of two files in `if`). `colRunType()` additionally loads `config.yml` by its literal name whatever matched. Preserved. |
| 3 | `R/modelRun.R` (`readRunProgress`) | A `full.log` with several `*** Status:` lines (restarted run) yields `runStatus = "NA"` because the old code could not store a vector in one table cell; the run is then reported as interrupted although the last status may be "Normal completion". Preserved (`length(statusLines) == 1`). |
| 4 | `R/modelRun.R` (`clusterRunStatus`) | On the cluster a stopped run whose `log.txt` contains no recognised `slurmstepd` error keeps `runStatus = "NA"`; "Run interrupted" is only used when `log.txt` is absent. Preserved. |
| 5 | `R/modelRun.R` (`readWarnings`) | REMIND warnings: the singular "Warning message:" and "There were 50 or more warnings" are not recognised (reported as 0); `grep -zoP` output is cut at the first NUL by `system(intern = TRUE)`, so only the first summary counts. Preserved. |
| 6 | `R/modelRun.R` (`readConvergence`) | A gdx without `o_iterationNumber` (e.g. `non_optimal.gdx` chosen by the mtime fallback) gives `numeric(0)`, and `if (converged == 0 && ... == iteration)` aborts the whole `getRunStatus()` call ("missing value where TRUE/FALSE needed" / "argument is of length zero") outside any `try()`. Preserved. |
| 7 | `R/modelRun.R` (`inAppResults`) | Without an `overview.rds` in the results archive `all(logical(0))` is `TRUE`, so `inAppResults` is "yes" whenever `<id>.rds` exists, whatever the state of the app. Preserved. |
| 8 | `R/modelRun.R` (`calibrationInfo`) | `Clb_converged` needs more than 10 `fulldata_*.gdx`/`input_*.gdx` files, but a current calibration run writes exactly 10, so it is not reported for them (it is for a folder with 11 such files). The AMT rule "Calib_nash must be Clb_converged" therefore always reports "did not converge" for the calibration run. Preserved. |
| 9 | `R/modelRun.R` (`abortStatus`) | The "Abort <region> N*Infes" label keeps regions with `p80_trackConsecFail == cm_abortOnConsecFail`; recorded abort gdx files show values one above the threshold, so the label does not fire. Preserved. |
| 10 | `R/modelRun.R` (`readModelstat`, `inAppResults`) | `stats$config$model_name == "MAgPIE"` is evaluated with `if()` although `model_name` may be missing (`NULL == "MAgPIE"` is `logical(0)`): a `runstatistics.rda` without `config$model_name` aborts the call. A REMIND `stats$modelstat` is taken over with the coercion rules of a table cell (a factor gives its integer code). Preserved (`setField()`, the scratch cell in `readModelstat()`). |
| 11 | `R/modelRun.R` (`clusterRunStatus`) | The coupled-run branch (`Starting MAgPIE...`) derives the MAgPIE folder from `cfg$cfg_mag$results_folder`, which in the Nash coupling layout is the template `output/:title::date:`, so `getRunStatus()` is called on a non-existent path. Preserved. |
| 12 | `R/getRunStatus.R:34`, `R/modelRun.R` (`addRunStatusRow`) | Rows are keyed by the folder name: two runs with the same basename under different parents overwrite each other and the table ends up with one row of mixed fields. Preserved. |
| 13 | `R/modelRun.R` (`sanityChecks`) | A `projectSummations.rds` whose `ScenarioMIP` entry lacks one of the three fields aborts the call ("replacement has length zero") instead of giving `NA`. Preserved (the `NULL` is kept in the object and fails in `as.data.frame()`). |
| 14 | `R/modelRun.R` (`latestGdxFile`, `readModelstat`) | A corrupt or non-GDX `fulldata.gdx` makes the GDX library abort the whole R process (SIGABRT); nothing in R can catch it, so one damaged file kills `rs` for every run in the table. Not fixable in this package. |
| 15 | `R/modelRun.R` (`clusterRunStatus`, `calibrationInfo`), `R/utils.R` (`lastLineMatches`) | `grep`, `tail`, `awk` and `find` are called with unquoted paths in some places; a folder name with spaces or shell metacharacters breaks them. Preserved. |
| 16 | `R/foundInSlurm.R:27-28` | Jobs are matched by *substring* of the folder path and by `"<name> "`, so a job in `/work/run-x` named `run` is attributed to `/work/run`. Preserved. |
| 17 | `R/foundInSlurm.R:44-47` | With several matching jobs the `" pending"`/`" startup"` suffixes are never added, and `"<n> users"` counts jobs, not distinct users. The pending check `PENDING [A-Za-z]*$` misses QOS names with digits. Preserved. |
| 18 | `R/commandLineInterface.R:46` + `R/foundInSlurm.R:38` | `rs -C -u user1,user2` passes the comma list to `foundInSlurm()`, whose `^user1,user2 ` pattern never matches, so own runs show the user name instead of the QOS. Preserved. |
| 19 | `R/commandLineInterface.R` (`slurmRunFolders`) | SLURM job names are used as unescaped regular expressions (`grepl(runnames[[i]], myruns[[i]])`); a name with `+`, `(`, `[` errors or mis-matches. Two separate `squeue` calls (`%Z`, `%j`) can pair a working directory with the wrong job name if a job finishes in between, and when the second call returns nothing the loop `1:length(runnames)` fails with "subscript out of bounds". Preserved. |
| 20 | `R/commandLineInterface.R` (`coupledRunFolders`) | `-l` keeps the *last listed* iteration, not the highest number (`-rem-9` beats `-rem-10` in listing order, the numeric sort happens later); `-m` appends the folder `magpie/output` itself, whose basename never matches the coupling pattern, so MAgPIE iterations are never added by it. Preserved. |
| 21 | `R/commandLineInterface.R` (`isRunFolder`, `runFoldersBelow`) | A run that failed early (only `config.Rdata` and `log.txt`) does not pass the "4 of 5 marker files" test and is treated as a folder of runs, so `rs <that folder>` reports "No runs found". Preserved. |

## Table printing (`printOutput()`, `loopRuns()`, `getSanityChecks()`)

| # | Where | What |
|---|-------|------|
| 22 | `R/printOutput.R:26-30` | Without `lenCols` the columns get the widths 13, 14, ... counted from the *last* column, without separators. With fewer widths than columns the last columns get those fallback widths and the ones in between an `NA` width, on which `rep(" ", NA)` errors. A single-column `cols` errors because `string[, cols]` drops to a vector without row names. Preserved. |
| 23 | `R/loopRuns.R:35-40` | The "skipped" check reads `status[["jobInSLURM"]]` before the `try-error` check; for a folder without `config*`/`log.txt` whose status computation failed this is "subscript out of bounds" and the whole table aborts instead of printing "skipped because of error". Preserved. |
| 24 | `R/loopRuns.R` (`statusLineStyle`) | The MAgPIE branch `all(grepl(" NA ", line) & grepl("FALSE", line))` can only be true for a folder *name* containing `FALSE`: dead in practice. Preserved. |
| 25 | `R/getSanityChecks.R:27,49` | The header is joined with two spaces, the rows use `printOutput()`'s default of three: every column after the first drifts by one character per column. Runs without sanity results (MAgPIE, no mif) are silently omitted. Preserved. |

## Accepted deviations of the refactored `rs`

- Uncaught errors and warnings print R's traceback, which quotes the code that failed. Where internal
  variable names changed (`opt$user` → `user`, `latest_gdx` → `latestGdx`, `s80_bool` → `converged`) or a
  helper was introduced (`commandLineInterface -> slurmRunFolders -> grepl`), that text differs. The
  messages and exit statuses are unchanged (checked for all 98 recorded `rs` invocations).

## Automated model tests (`modeltests()`)

The AMT code was rewritten (REMIND only, cycle record instead of `.testsstatus`, one log per cycle),
so the following defects of the old `modeltests.R` no longer exist. They are listed for the record:

- `.testsstatus` stayed at "evaluateRuns() is running or stopped due to an error" after any error in
  the evaluation, and every later call did nothing until someone edited the file by hand; an error in
  `startRuns()` left "next:start", so the next call started the runs a second time.
- `mydir` needed a trailing slash for some paths (`paste0(mydir, "../.testsstatus")`) and none for
  others (`paste0(mydir, "/runcode.rds")`).
- Runs that did not start were detected by a count (`length(runsStarted) < length(runsToStart) + 1`),
  so a scenario started twice hid a missing one, and the list never reached the summary line.
- The previous run of a scenario was taken from `gRS.rds`, a history of every run ever seen; a run
  that had left `output/` and `archive/` made `.readRuntime()` fail and aborted the whole evaluation
  (also `max(character(0))` when no earlier run existed).
- The Mattermost payload was built by string concatenation inside single quotes; a `'` in the message
  broke it, and the webhook token was passed on the `curl` command line.
- `mv <runs> archive` was unquoted and assumed `archive/` to exist.
- The `make test-full` result used `tail(grep(...))`, i.e. up to six lines, so with more than one
  result line the check said "did not run properly".
- MAgPIE: the wait for the default run compared `mydir/` (trailing slash) with `squeue %Z` output
  (no slash) and never matched. Removed together with the MAgPIE support.
