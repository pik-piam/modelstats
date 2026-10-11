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
modeltests [GLOBAL OPTIONS] status     # print the state file (stdout) and where the logs are (stderr), exit 0
modeltests [GLOBAL OPTIONS] reset      # clear a 'failed' state after a human looked at it (asks unless --yes)

GLOBAL OPTIONS
  --dir DIR                 the REMIND checkout (the R 'mydir'); default '.'
  --report-repo DIR         the git clone that receives README.md (the R 'gitdir'); required for evaluate unless --no-publish
  --user USER               SLURM user whose jobs are awaited; default: the current user
  --mattermost-url-env NAME environment variable holding the webhook URL (default AMT_MATTERMOST_URL); unset = no notification
  --publish / --no-publish  commit and push README.md to --report-repo (the R 'email' flag; default publish)
  --compare-scenarios / --no-compare-scenarios   run compareScenarios2 for converged runs (the R 'compScen'; default on)
  --log-dir DIR             where the cycle logs and the README copies go; default <dir>/logs
  --dry-run                 reads execute, every mutation is logged and answered synthetically; no file is written
  --force                   'start' although a cycle is still 'started' (a previous evaluate never ran); also 'start --force'
  --no-wait                 fail with status 2 when another modeltests command holds the lock instead of waiting
```

`modeltests --help` (or `-h`) lists the global options and the five commands; the global options go in
front of the command (`modeltests --dir /p/... --force start`). Two commands take an option of their own
after the command, shown by `modeltests start --help` and `modeltests reset --help`: `start --force` (the
same as the global `--force`) and `reset --yes` (`-y`). Without a command the help page is printed on
stderr with exit status 2.

## Installation

```
uv tool install ./dist/modelstats-0.31.0-py3-none-any.whl   # the commands rs and modeltests (Python 3.14); README.md "Installation with uv"
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

`evaluate` blocks until no job of `--user` has the checkout (or a directory below it) as its work directory
(`squeue` every 600 s, as the R version), so an `evaluate` scheduled right after `start` simply waits for
the runs.

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

The blocks are `null` while they have nothing to say (a fresh checkout: `"cycle": null`, `"last_completed":
null`, `"last_error": null`).

| from      | command         | to        | notes |
|-----------|-----------------|-----------|-------|
| `idle`    | `start`         | `started` | `cycle` filled; `runcode.rds` also written (for `rs-r -A`); both persisted right after the scenario selection and before the first `Rscript start.R`, so a `start` killed during the submissions (hours, with `--wait`) leaves `started`, never `idle`, and is never repeated: `run` then evaluates it (`No runs started` and the missing scenarios when nothing ran) or a human abandons it with `start --force` |
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
the state from <path> ('<content>')`, one line per missing or unreadable R file, `derived state: ...`, and
`the derived state is kept in <path>; the R files are left untouched`): `next:start` is `idle`;
`next:evaluate` is `started` with the pattern from `runcode.rds`, the scenario names from
`runsToStart.rds` and the previously tested commit from `lastcommit.rds`; the error sentence of a crashed
R evaluate (or any other content) is `failed`, with the cycle derived the same way so that `evaluate` can
retry it. The R files are only read; `.testsstatus` is left as it is (the R package stays the rollback,
after setting `.testsstatus` by hand) and `gRS.rds` is never read. `runcode.rds` keeps being written on
every `start` (the same single string R wrote) so that `rs-r -A`, `remindstatus` and the Python `rs -A`
agree during the transition (`06` 3.4, D-26); `runsToStart.rds`, `lastcommit.rds` and `gRS.rds` are no
longer written.

## The log: one file per cycle

`start` creates `<log-dir>/amt-<cycle id>.log` (the cycle id is the start date), `evaluate` appends to
it and `run` follows; the default `<log-dir>` is `<dir>/logs`. Every line carries the timestamp, the
command and the level. The first lines of a cycle started on a checkout the R version ran (recorded in
the sandbox, where the clock is stopped at the frozen time and the fake `git` answers one commit):

```
2026-09-30 12:00:00 start    INFO     amt-state.json is absent; deriving the state from /p/projects/remind/modeltests/.testsstatus ('next:start')
2026-09-30 12:00:00 start    INFO     derived state: the AMT state is 'idle'
2026-09-30 12:00:00 start    INFO     the derived state is kept in /p/projects/remind/modeltests/remind/amt-state.json; the R files are left untouched
2026-09-30 12:00:00 start    INFO     Starting AMT cycle 2026-09-30 in /p/projects/remind/modeltests/remind (run pattern .*-AMT_2026-09-30|.*-AMT_2026-10-01)
2026-09-30 12:00:00 start    INFO     running: git reset --hard origin/develop in /p/projects/remind/modeltests/remind
2026-09-30 12:00:00 start    INFO     running: git pull in /p/projects/remind/modeltests/remind
2026-09-30 12:00:00 start    INFO     [git] commit 3f5e2a1b9c8d7e6f5a4b3c2d1e0f9a8b7c6d5e4f
...
2026-09-30 12:00:00 evaluate INFO     Evaluating AMT cycle 2026-09-30 (runs matching .*-AMT_2026-09-30|.*-AMT_2026-10-01) in /p/projects/remind/modeltests/remind
2026-09-30 12:00:00 evaluate INFO     running: squeue -u pascalfu -h -o '%i %q %T %C %M %j %V %L %e %Z'
2026-09-30 12:00:00 evaluate INFO     [squeue] 2366122 short RUNNING 6 5:57 piam-runmerge 2026-09-30T20:27:01 1:54:03 2026-09-30T22:27:31 /home/pascalfu
...
2026-09-30 12:00:00 evaluate INFO     Mattermost notification sent (1083 characters, HTTP 200)
2026-09-30 12:00:00 evaluate INFO     Evaluation of cycle 2026-09-30 finished: Summary: Some scenario(s) did not start at all. No runs started
```

Subprocess output (`Rscript start.R`, `make test-full-slurm`, `git`, `sbatch`, `rsync`, `squeue`) is
captured line by line with a `[<tool>]` prefix, after a `running: <argv>` line (shell-quoted, with
`in <dir>` when the program runs in a given directory); a non-zero exit status is the `WARNING` line
`<tool> exited with status N`; a crash is an `ERROR` record with the traceback. Everything is mirrored to
stderr. The README of the cycle is kept next to the log as `<log-dir>/amt-<cycle id>-README.md` (the R
version wrote it to `tempdir()`), whether or not it is published. `modeltests status` prints the state
file on stdout and, on stderr, one line saying where the AMT stands, the state file path and, when they
exist, the cycle log and the last error's log.

## Recovery from `failed`

1. `modeltests --dir ... status` says what failed, when, and which log holds the traceback
   (`last_error`); `rs -A` keeps showing the cycle's runs meanwhile (it reads `cycle.runcode` from the
   state file).
2. Fix the cause (a stale checkout, a broken `scenario_config.csv`, SLURM, the report repository, ...).
3. `modeltests --dir ... evaluate` retries the evaluation of the kept cycle (`failed -> idle`), including
   after a failed `start` whose runs were submitted (the state then carries the scenario list) and after
   a failed publication (`git reset`, `pull`, `add`, `commit` or `push` in the report repository exited
   non-zero: the README was compiled, the retry rebuilds and pushes it; when the push had gone through
   and the crash came later, the retry finds `README.md` unchanged and commits nothing), or
   `modeltests --dir ... reset` drops the failed cycle (`failed -> idle`) so that the next `run` starts a
   new one. `reset` asks for confirmation on the terminal; `--yes` (`-y`) skips the question (scripts).
   The `make test-full` verdict survives a retry: `test-full.log` is renamed into `tests/` only at the very
   end of a successful evaluation.
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
(no `rs -A` or sanity table is captured; the log says `No Mattermost webhook URL in $AMT_MATTERMOST_URL:
notification not sent`). The payload is `{"text": message}` built with `json.dumps` and posted with
`urllib`; a failed post is logged as an error and the evaluation goes on (a chat message is best effort
and must not fail the cycle), and so is a malformed URL in the variable (`not-a-url`, a `file:` or
`mailto:` scheme): it is reported as `no HTTP exchange (webhook URL is not an http(s) URL)` without
echoing the value. The sanity table of the message lists the cycle's started runs in `<dir>/output/`
(the R `getSanityChecks()` read the cluster constant and its `runcode.rds`, so for any other `--dir` it
described the cluster checkout).

## `--dry-run`

`modeltests --dry-run run` (or `start`, `evaluate`) performs every read for real and answers every
mutation synthetically: no file is written (not even the state, the lock or the log: the log goes to
stderr only), no program is run (`git log -1` answers a fake commit, `squeue` answers an empty queue so the
wait loop ends at once, `sbatch` job 0), nothing is posted. Every mutation is announced as a
`dry run: would ...` line, for example (from a dry run on the sandbox's fake checkout):

```
dry run: would lock /p/projects/remind/modeltests/remind/amt-state.lock
dry run: would write /p/projects/remind/modeltests/remind/amt-state.json
dry run: would run git reset --hard origin/develop in /p/projects/remind/modeltests/remind
dry run: would run Rscript .../modelstats/amt/bridge_scripts/select_scenarios.R AMT in /p/projects/remind/modeltests/remind
dry run: the selectScenarios() bridge was not run (it executes the checkout's scripts/start/*.R); the cycle records no scenario names
dry run: would replace 'cfg$force_download <- FALSE' by 'cfg$force_download <- TRUE' in /p/projects/remind/modeltests/remind/config/default.cfg
dry run: would run make test-full-slurm in /p/projects/remind/modeltests/remind
dry run: would post JSON to the webhook at https://mattermost.example.org (path redacted, 3054 characters)
```

The path of the webhook URL carries the secret and never reaches stderr or the cron mail. The
`selectScenarios()` bridge is not run either: `Rscript select_scenarios.R` sources the checkout's
`.Rprofile` and `scripts/start/*.R`, which is REMIND code, not a read (on a fresh clone the profile
bootstraps `renv`, with downloads and writes). It is announced and answered with an empty selection
(`selected 0 scenarios: (none)`, with the line saying why), so the cycle a dry run reports records no
scenario names. Use the dry run to check a checkout, the options and the environment before switching the
cron job over; `Rscript` stays a requirement of the real run. A dry run on a checkout the R version ran
announces the derived state as a `would write` of `amt-state.json` and keeps nothing.

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
| a failed `git` (checkout or `magpie/`), a failed `make test-full-slurm` or a failed scenario selection stops `start` with the state `failed` and a notification; `Rscript start.R` keeps R's tolerance (its status reflects the runs under test); a failed publication `git` (`reset`, `pull`, `add`, `commit`, `push` in the report repository) stops `evaluate` the same way, the cycle kept for a retry, and a `README.md` already equal to the new one is not committed | `system()` ignored every exit status (BUG-075 at start; at evaluate a failed push left the README unpublished and nobody told) |
| the scenario selection runs right after `git pull`, before the first run is submitted, so a broken `scenario_config.csv` aborts with nothing started | selection after both `Rscript start.R` calls |
| the cycle (`started` in `amt-state.json`, the pattern in `runcode.rds`) is persisted right after the selection and before the first `Rscript start.R`, so a `start` killed during the submissions or `make` is never repeated by the next `run` | `.testsstatus` set to `next:evaluate` only after `startRuns` returned (`R/modeltests.R:42`): a killed start was submitted again by the next cron invocation under the same day's runcode |
| the `squeue` wait loop reads the work-directory field (`%Z`) and compares canonical paths, so a job started in the checkout itself, or through a symlink or `..` alias of it, counts | `grepl(mydir, line)` over the whole line, with `mydir` as spelled |
| `test-full.log` is read from the checkout (`<dir>/test-full.log`, where `make test-full-slurm` writes it), the last `[ FAIL` line decides, `tests/` is created before the rename, and the rename happens last, after the notification, so that a retried `evaluate` (`failed -> idle`) reads the same log | `../test-full.log` relative to `output/` (the same file), `isTRUE()` over up to six lines, a rename into a possibly missing `tests/`, done before the README and the publication |
| the README is kept next to the cycle log | `tempdir()` |
| the message's first line names the `--dir` checkout and its sanity table lists the cycle's runs of that checkout | the hard-coded cluster path in both |
| REMIND only | the `model` argument (BUG-032 `wont-fix`) |

Kept as they are, because they are rules of the model owners: the `> 10` calibration file threshold of
`Clb_converged` (BUG-001), the `Abort ... *Infes` equality test (BUG-002), the 1.25 x runtime rule, the
90 days, the `Calib_nash` and `testOneRegi` rules, the `sumErr` rule. The Python `rs` command is
unchanged by all of this (it is a parity drop-in).

The evidence: unit tests of every step and transition (`tests/unit/test_amt_*.py`, the lock and the dry
run also in `tests/unit/test_env.py`), the replay of every `remind-evaluate*` R case
(`tests/unit/test_amt_evaluate.py`, `pytest -m golden`), the README block against the R goldens
(`tests/golden/test_amt_readme.py`, `pytest -m golden`) and the end-to-end runs of the command inside the
sandbox on every `remind-*` case plus the round trips (`tests/golden/test_amt_e2e.py`, `pytest -m
sandbox`, driven by `migration/harness/run_amt_case.sh`); every difference to an R golden is classified in
`migration/reviews/2026-10-11-phase5-amt-differences.md`. `migration/README.md` says how to run the tiers.

## The MAgPIE weekly tests stay on R

`modeltests` has no `--model` option and no MAgPIE branch (`06` section 2, D-27). The MAgPIE weekly tests
(`/p/projects/landuse/tests/magpie`, the `default` run, the 300 s wait, `test_runs`, the ctime-based run
selection, `magpie4::addToDataChangelog`, the MAgPIE Mattermost texts) keep running through the R package:

```
Rscript -e 'modelstats::modeltests(mydir = "/p/projects/landuse/tests/magpie/", gitdir = "...", model = "MAgPIE", user = "...", mattermostToken = ...)'
```

with its `.testsstatus` toggle, until the landuse group decides otherwise. The R goldens
`migration/goldens/amt/magpie-*` document that behaviour.
