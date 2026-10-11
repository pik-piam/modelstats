# The `modeltests` command: REMIND automated model tests (AMT)

`modeltests` replaces the `Rscript -e 'modelstats::modeltests(...)'` line of the REMIND AMT cron job
(`R/modeltests.R`, modelstats 0.31.0). It is REMIND only (`migration/06-user-requests.md` section 2,
decision D-27): the MAgPIE weekly tests keep running on the R package, see the last section. The design is
`06` sections 3 and 4; this page is the operator's guide: the cron lines, the state file and its
transitions, recovery from a failed cycle, the log, the webhook variable, `--dry-run`, and what changed
compared with the R version.

```
modeltests [GLOBAL OPTIONS] start      # start the AMT cycle (REMIND checkout in --dir)
modeltests [GLOBAL OPTIONS] evaluate   # wait for the runs, write the README, publish, notify
modeltests [GLOBAL OPTIONS] run        # 'start' when idle, 'evaluate' when started; the drop-in for the old toggle
modeltests [GLOBAL OPTIONS] status     # print the state file (stdout) and the log paths (stderr), exit 0
modeltests [GLOBAL OPTIONS] reset      # clear a 'failed' state after a human looked at it (asks unless --yes)

GLOBAL OPTIONS
  --dir DIR                 the REMIND checkout (the R 'mydir'); default '.'
  --report-repo DIR         the git clone that receives README.md (the R 'gitdir'); required for evaluate unless --no-publish
  --user USER               SLURM user whose jobs are awaited; default: the current user
  --mattermost-url-env NAME environment variable holding the webhook URL (default AMT_MATTERMOST_URL); unset = no notification
  --publish / --no-publish  commit and push README.md to --report-repo (the R 'email' flag; default publish)
  --compare-scenarios / --no-compare-scenarios   run compareScenarios2 for converged runs (the R 'compScen'; default on)
  --log-dir DIR             where the cycle logs go; default <dir>/logs
  --dry-run                 reads execute, every mutation is logged and answered synthetically; no file is written
  --force                   'start' although a cycle is still 'started' (a previous evaluate never ran); also 'start --force'
  --no-wait                 fail with status 2 when another modeltests command holds the lock instead of waiting
```

`modeltests --help` and `modeltests <command> --help` print the same text. The global options go in
front of the command (`modeltests --dir /p/... --force start`); `start --force` and `reset --yes` are
also accepted after the command.

## Installation

```
uv tool install modelstats     # the commands rs and modeltests (Python 3.14)
modeltests --help
```

The command needs `git`, `Rscript` (for REMIND's `start.R` and the `selectScenarios()` bridge), `make`,
`squeue`, `sbatch` and `rsync` on `PATH` of the cron user, exactly as the R version did.

## Cron lines

The cron job redirects nothing: every command writes its own log (below) and mirrors it to stderr, so cron
mail still shows what happened. The webhook URL comes from an environment variable that the cron script
loads from a file only the AMT user can read; the command never reads that file itself.

One schedule with `run` (the drop-in for the old `.testsstatus` toggle; `run` starts a cycle when the state
is `idle` and evaluates it when it is `started`):

```
# crontab of the AMT user: Monday and Thursday 22:00, one cycle per week
0 22 * * 1,4  . "$HOME/.amt-env" && modeltests --dir /p/projects/remind/modeltests/remind --report-repo /p/projects/remind/modeltests/testing_suite --user "$USER" run
```

or `start` and `evaluate` on their own schedules (the same state file keeps them in order: an `evaluate`
without a started cycle, or a `start` on top of one, is refused with status 2 and changes nothing):

```
0 22 * * 1    . "$HOME/.amt-env" && modeltests --dir /p/projects/remind/modeltests/remind --report-repo /p/projects/remind/modeltests/testing_suite start
0 22 * * 4    . "$HOME/.amt-env" && modeltests --dir /p/projects/remind/modeltests/remind --report-repo /p/projects/remind/modeltests/testing_suite evaluate
```

where `~/.amt-env` (mode 0600) holds `export AMT_MATTERMOST_URL='https://mattermost.../hooks/...'`. A
relative `--dir`, `--report-repo` or `--log-dir` is made absolute against the working directory when the
command starts, so the state file always records paths that a later invocation can open from anywhere.

`evaluate` blocks until no job of `--user` has the checkout in its work directory (`squeue` every 600 s,
as the R version), so an `evaluate` scheduled right after `start` simply waits for the runs.

## The state file and its transitions

One JSON file `<dir>/amt-state.json` holds the state of the cycle (`06` 3.2). Every command holds an
exclusive `flock` on `<dir>/amt-state.lock` from start to end, so a second invocation waits for the first
(or fails fast with `--no-wait`) instead of interleaving with it; the state is written atomically (temp
file and rename in the same directory).

```json
{
  "schema": 1,
  "phase": "idle | started | failed",
  "cycle": {
    "id": "2026-09-30",
    "runcode": ".*-AMT_2026-09-30|.*-AMT_2026-10-01",
    "started_at": "2026-09-30T22:04:11+02:00",
    "started_commit": "abc1234...",
    "runs_to_start": ["SSP2-NPi-AMT", "default-AMT", "..."],
    "log": "/p/projects/remind/modeltests/remind/logs/amt-2026-09-30.log"
  },
  "last_completed": {"id": "2026-09-23", "evaluated_at": "...", "tested_commit": "...", "summary": "Summary: AMT runs look good.", "runcode": "..."},
  "last_error": {"command": "evaluate", "at": "...", "message": "...", "log": "..."}
}
```

| from      | command         | to        | notes |
|-----------|-----------------|-----------|-------|
| `idle`    | `start`         | `started` | `cycle` filled; `runcode.rds` also written (for `rs-r -A`) |
| `started` | `evaluate`      | `idle`    | `last_completed` filled, `cycle` cleared; the tested commit becomes the base of the next merge list |
| `started` | `start --force` | `started` | the abandoned cycle is recorded in `last_error` |
| `failed`  | `evaluate`      | `idle`    | retry after a failed evaluate (the `cycle` is still there) |
| `failed`  | `reset`         | `idle`    | the failed cycle is dropped |
| any       | crash inside `start` or `evaluate` | `failed` | `last_error` filled, the cycle kept, a Mattermost message sent when a URL is configured |

Anything else is refused with exit status 2 and no change (`cannot 'evaluate': the AMT state is 'idle';
run 'modeltests start' first`). `run` maps `idle` to `start`, `started` to `evaluate` and `failed` to
that refusal: after a failure nothing runs until a human decides.

Exit statuses: `0` the command did what it says; `1` `start` or `evaluate` crashed and the state is now
`failed` (or a `reset` was declined at the prompt); `2` refused transition, held lock under `--no-wait`,
damaged state file, missing `--report-repo`, or a usage error.

**First use on a checkout the R version ran.** When `amt-state.json` is absent and `../.testsstatus`
exists, the first command derives the state once and logs each fact (`amt-state.json is absent; deriving
the state from <path> (<content>)`, one line per missing or unreadable R file, `derived state: ...`, and
`the derived state is kept in <path>; the R files are left untouched`): `next:start` is `idle`;
`next:evaluate` is `started` with the pattern from `runcode.rds`, the scenario names from
`runsToStart.rds` and the previously tested commit from `lastcommit.rds`; the error sentence of a crashed
R evaluate (or any other content) is `failed`, with the cycle derived the same way so that `evaluate` can
retry it. The R files are only read; `.testsstatus` is left as it is (the R package stays the rollback)
and `gRS.rds` is never read. `runcode.rds` keeps being written on every `start` (the same single string
R wrote) so that `rs-r -A`, `remindstatus` and the Python `rs -A` agree during the transition (`06`
3.4, D-26); `runsToStart.rds`, `lastcommit.rds` and `gRS.rds` are no longer written.

## The log: one file per cycle

`start` creates `<log-dir>/amt-<cycle id>.log` (the cycle id is the start date), `evaluate` appends to
it and `run` follows; the default `<log-dir>` is `<dir>/logs`. Every line carries the timestamp, the
command and the level:

```
2026-09-30 22:04:11 start    INFO     Starting AMT cycle 2026-09-30 in /p/projects/remind/modeltests/remind (run pattern .*-AMT_2026-09-30|.*-AMT_2026-10-01)
2026-09-30 22:04:11 start    INFO     running: git reset --hard origin/develop in /p/projects/remind/modeltests/remind
2026-09-30 22:04:12 start    INFO     [git] HEAD is now at 3f5e2a1 Merge pull request #2041
```

Subprocess output (`Rscript start.R`, `make test-full-slurm`, `git`, `sbatch`, `rsync`, `squeue`) is
captured line by line with a `[<tool>]` prefix, after a `running: <argv>` line (shell-quoted, with
`in <dir>` when the program runs in a given directory); a non-zero exit status is the `WARNING` line
`<tool> exited with status N`; a crash is an `ERROR` record with the traceback. Everything is mirrored to stderr. The README of the cycle is kept next
to the log as `<log-dir>/amt-<cycle id>-README.md` (the R version wrote it to `tempdir()`), whether or not
it is published. `modeltests status` prints the state file on stdout and the log paths on stderr.

## Recovery from `failed`

1. `modeltests --dir ... status` says what failed, when, and which log holds the traceback
   (`last_error`); `rs -A` keeps showing the cycle's runs meanwhile (it reads `cycle.runcode` from the
   state file).
2. Fix the cause (a stale checkout, a broken `scenario_config.csv`, SLURM, the report repository, ...).
3. `modeltests --dir ... evaluate` retries the evaluation of the kept cycle (`failed -> idle`), including
   after a failed `start` whose runs were submitted (the state then carries the scenario list), or
   `modeltests --dir ... reset` drops the failed cycle (`failed -> idle`) so that the next `run` starts a
   new one. `reset` asks for confirmation on the terminal; `--yes` skips the question (scripts).
4. A cycle that was started but whose `evaluate` never ran (for example a cron job that was switched off)
   blocks `start` with status 2; `modeltests --dir ... start --force` abandons it (recorded in
   `last_error`) and starts a new one.

The failure is also posted to the Mattermost channel (the R version stopped silently and nothing ran
until someone edited `.testsstatus`). The lock needs no cleaning: a `flock` disappears with the process
that held it, and `amt-state.lock` is an empty file.

## The webhook variable

`--mattermost-url-env NAME` names the environment variable holding the Mattermost incoming-webhook URL
(default `AMT_MATTERMOST_URL`). The command reads only that variable: no file, no option on the command
line (which would show in `ps`), and the URL is never written to the log. An unset or empty variable
means no notification, as a missing `mattermostToken` did in R: the message is then not even composed
(no `rs -A` or sanity table is captured). The payload is `{"text": message}` built with `json.dumps` and
posted with `urllib`; a failed post is logged as an error and the evaluation goes on (a chat message is
best effort and must not fail the cycle), and so is a malformed URL in the variable (`not-a-url`, a
`file:` or `mailto:` scheme): it is reported as `no HTTP exchange (webhook URL is not an http(s) URL)`
without echoing the value. The sanity table of the message lists the cycle's started runs in
`<dir>/output/` (the R `getSanityChecks()` read the cluster constant and its `runcode.rds`, so for any
other `--dir` it described the cluster checkout).

## `--dry-run`

`modeltests --dry-run run` (or `start`, `evaluate`) performs every read for real and answers every
mutation synthetically: no file is written (not even the state, the lock or the log: the log goes to
stderr only), no program is run (`git log -1` answers a fake commit, `squeue` answers an empty queue so the
wait loop ends at once, `sbatch` job 0), nothing is posted. Every mutation is announced as a
`dry run: would ...` line (`would write ...`, `would run make test-full-slurm in ...`, `would post JSON to
the webhook at https://<host> (path redacted, ...)`: the path of the URL carries the secret and never
reaches stderr or the cron mail). The `selectScenarios()` bridge is not run either: `Rscript
select_scenarios.R` sources the checkout's `.Rprofile` and `scripts/start/*.R`, which is REMIND code, not
a read (on a fresh clone the profile bootstraps `renv`, with downloads and writes). It is announced as
`would run Rscript .../select_scenarios.R AMT in <dir>` and answered with an empty selection, so the
cycle a dry run reports records no scenario names (`selected 0 scenarios: (none)`, with a line saying
why). Use the dry run to check a checkout, the options and the environment before switching the cron job
over; `Rscript` stays a requirement of the real run.

## What changed compared with the R version

Behaviour kept from `R/modeltests.R` (`06` 4.1): `git reset --hard origin/develop && git pull`; the
`cfg$force_download` edit of `config/default.cfg`; the deletion of empty realization folders; the two
`Rscript start.R` command lines (`--testOneRegi` with `--wait`, then the `AMT` bundle); the `magpie/`
submodule update and `make test-full-slurm`; the run pattern `.*-AMT_<today>|.*-AMT_<tomorrow>`; the
`squeue` wait loop with its 600 s polls and the stop after four consecutive failures; the merge list from
`git log --merges`; the README block byte for byte (header lines, column titles and widths, one row per
run, the "did not start" block, the summary, the code fences); the five error rules (`Calib_nash` must be
`Clb_converged`, other runs converged, `testOneRegi` optimal, `sumErr`, not reported), the 1.25 x runtime
rule and the 90-day archive; the `rsync` of `fulldata.gdx` to the RSE server for a converged
`SSP2-NPi-AMT`; the compareScenarios2 `sbatch` command line byte for byte; the `test-full.log`
evaluation and rename; the publication to the report repository (`reset`, `pull`, copy, `add`,
`commit -m 'Automated Test Results'`, `push`); the Mattermost message text with the `rs -A` and sanity
tables.

Deliberate changes (decision D-24; implementation artefacts are not reproduced, rule-level defects are
kept, `06` 4.2):

| Change | Replaces |
|---|---|
| JSON state machine with a lock; `runcode.rds` kept for `rs-r -A` | `.testsstatus`, `runsToStart.rds`, `lastcommit.rds`, `gRS.rds` (BUG-013 gone) |
| previous run found on disk: the newest directory in `output/` or `output/archive/` named `<title>_<date>_<time>` that sorts before the current run and is converged with a mif | the `gRS.rds` history and `max(sameRuns[...])` (BUG-014, BUG-039 gone) |
| scenarios that did not start by set difference on the names, listed in the README **and** counted as an error (`Some scenario(s) did not start at all`) | the count-based check, README only (BUG-024 gone) |
| one `shutil.move` per run into `output/archive/`, created when missing | `system("mv ... archive")` (BUG-016, BUG-033 gone) |
| `{"text": ...}` with `json.dumps`, the URL from an environment variable, `urllib` | the string-built JSON on a `curl` command line (BUG-015 gone, D-15) |
| failure notification and the `failed` state | a silent stop with the error sentence in `.testsstatus` |
| one log per cycle, written by the command | bash redirects per stage in the cron script |
| a failed `git` (checkout or `magpie/`), a failed `make test-full-slurm` or a failed scenario selection stops `start` with the state `failed` and a notification; `Rscript start.R` keeps R's tolerance (its status reflects the runs under test) | `system()` ignored every exit status |
| the scenario selection runs right after `git pull`, before the first run is submitted, so a broken `scenario_config.csv` aborts with nothing started | selection after both `Rscript start.R` calls |
| the `squeue` wait loop reads the work-directory field (`%Z`), so a job started in the checkout itself counts | `grepl(mydir, line)` over the whole line |
| `test-full.log` is read from the checkout (`<dir>/test-full.log`, where `make test-full-slurm` writes it) and the last `[ FAIL` line decides | `../test-full.log` relative to `output/` (the same file) and `isTRUE()` over up to six lines |
| the README is kept next to the cycle log | `tempdir()` |
| REMIND only | the `model` argument (BUG-032 `wont-fix`) |

Kept as they are, because they are rules of the model owners: the `> 10` calibration file threshold of
`Clb_converged` (BUG-001), the `Abort ... *Infes` equality test (BUG-002), the 1.25 x runtime rule, the
90 days, the `Calib_nash` and `testOneRegi` rules, the `sumErr` rule. The Python `rs` command is
unchanged by all of this (it is a parity drop-in).

The evidence: unit tests of every step and transition (`tests/unit/test_amt_*.py`), the replay of every
`remind-evaluate*` R case (`tests/unit/test_amt_evaluate.py`, `pytest -m golden`), and the end-to-end runs
of the command inside the sandbox on every `remind-*` case (`tests/golden/test_amt_e2e.py`, `pytest -m
sandbox`, driven by `migration/harness/run_amt_case.sh`); every difference to an R golden is classified in
`migration/reviews/2026-10-11-phase5-amt-differences.md`.

## The MAgPIE weekly tests stay on R

`modeltests` has no `--model` option and no MAgPIE branch (`06` section 2, D-27). The MAgPIE weekly tests
(`/p/projects/landuse/tests/magpie`, the `default` run, the 300 s wait, `test_runs`, the ctime-based run
selection, `magpie4::addToDataChangelog`, the MAgPIE Mattermost texts) keep running through the R package:

```
Rscript -e 'modelstats::modeltests(mydir = "/p/projects/landuse/tests/magpie/", gitdir = "...", model = "MAgPIE", user = "...", mattermostToken = ...)'
```

with its `.testsstatus` toggle, until the landuse group decides otherwise. The R goldens
`migration/goldens/amt/magpie-*` document that behaviour.
