# The `Run` object

`modelstats.Run` reads one REMIND or MAgPIE run folder the way `rs` (`R/getRunStatus.R`) reads it and
presents the result with Python types, so that other repositories do not re-implement `getRunStatus.R`
(`migration/06-user-requests.md` section 5, decision D-25). It sits on the same reading layer as the
parity record `rs` prints (`modelstats.facts.RunFacts`): every file is read at most once, lazily, and the
R-compatible record is one method away.

## Example

```python
from modelstats import Run, RunState

run = Run("/p/projects/remind/modeltests/remind/output/SSP2-NPi-AMT_2026-09-28_10.30.27")
print(run.status, run.convergence, run.iterations.current, run.warnings, run.runtime)
if run.status is RunState.COMPLETED and run.convergence.is_converged:
    print(run.summation_errors, run.mif, run.to_status_record())
```

On that run of the fixture snapshot this prints `RunState.COMPLETED Convergence.CONVERGED 26 27 2:18:23.765632`
and `0 MifState.PRESENT RunStatus(run='SSP2-NPi-AMT_2026-09-28_10.30.27', ..., RunType='nash',
modelstat='2: Locally Optimal', ..., Iter='26/100', RunStatus='Normal completion', ..., Warnings='27',
Conv='converged', Runtime=8304, sanity=SanityCounts(summationErrors=0, ...))`.

## The API

```python
from modelstats import Convergence, Run, RunState, iter_runs

run = Run(path)  # Run(path, effects=None, *, user=None); nothing is read here
run.path, run.name, run.title  # Path, basename, cfg$title (None without a configuration file)
run.model  # "REMIND" | "MAgPIE" | None (runstatistics.rda model_name, the configuration as fallback)
run.run_type  # str | None: "nash", "nash debug", "Calib_nash", "testOneRegi EUR", "empty model", a MAgPIE optimization
run.status  # RunState: PENDING, RUNNING, COMPLETED, ERROR, INTERRUPTED, UNKNOWN
run.status_detail  # the R RunStatus string ("Normal completion", "conoptspy >2h", "Timeout interrupt", ...)
run.convergence  # Convergence: CONVERGED, CONVERGED_HAD_INFES, NOT_CONVERGED, CALIBRATION_CONVERGED, UNKNOWN
run.convergence.is_converged  # True for CONVERGED, CONVERGED_HAD_INFES and CALIBRATION_CONVERGED
run.feasibility  # alias of convergence (the user's word)
run.modelstat  # ModelStat(code: int | None, label: str | None, raw: str | None), e.g. (2, "Locally Optimal", "2")
run.iterations  # Iterations(current: int | None, maximum: int | None, calibration: int | None, loop: str | None)
run.warnings  # int | None
run.runtime  # timedelta | None (GAMS wall time, or elapsed since the prepare start while a job of the run is listed)
run.mif  # MifState: MISSING, PRESENT, SUMMATION_ERRORS, UNKNOWN
run.summation_errors, run.range_errors, run.fix_errors  # int | None (None without a REMIND mif)
run.project_summation  # ProjectSummation(missing_vars, errors, errors_regional) | None (projectSummations.rds)
run.in_app_results  # bool | None (cluster only)
run.slurm  # SlurmJob | None: user, workdir, name, elapsed, state, qos, pending, startup, elapsed_time (cluster only)
run.config, run.statistics  # RunConfig | None, RunStatistics | None (the parsed files)
run.files  # RunFiles: the resolved paths (configuration, GDX files, full.log, log.txt, ...)
run.facts  # RunFacts: the reading layer itself (every fact, cached)
run.to_status_record(detailed=True)  # RunStatus: the R-compatible parity record (03 section 5)
run.to_dict()  # a JSON-able snapshot of the properties above (enums as values, the runtime in seconds)
Run.is_run_folder(path)  # the rs detection rule (03 section 3.4)
iter_runs(directory)  # a Run for every run folder below directory, in rs order (effects=None, *, user=None)
```

`Run(path, effects=None, *, user=None)`: `path` is normalised as `getRunStatus()` normalises it (the real
path when it exists, the input otherwise); `effects` is the environment door (`modelstats.env.Effects`,
production by default, `FakeEffects` in tests and off the cluster); `user` is the SLURM user whose jobs
count as the run's own (`Sys.info()[["user"]]` by default). Construction touches nothing but the path
normalisation; `repr(run)` is `Run('/path/to/run')`.

Every property is lazy and cached. GDX files are opened only when a property needs them (`modelstat`,
`convergence`, the `Abort ... *Infes` status); `squeue` runs once per `Run`, and only on the cluster (a
coupled run whose `log.txt` ends in `Starting MAgPIE...` reads its MAgPIE child once more, as
`R/getRunStatus.R:188-196` does, and both views share that one reading).
Errors while reading raise `modelstats.RunReadError` (a `ModelstatsError`) from the property, with R's
message where R has one, R's deparsed call in `error.call` (`None` where R has none) and the R warnings
deferred during the read in `error.warnings`, and never abort the process; an error is not cached, so the
next access reads again. `status` and `run_type` degrade to `UNKNOWN` / `None` on missing inputs instead of
raising (a damaged file still raises). `iter_runs(directory)` expands `directory` as the `rs` positional
argument is expanded (a run folder is itself, a model main folder contributes the directories below its
`output`, anything else its own sub-directories), keeps the run folders and yields them in `rs` order.

## `RunState`: from the R `RunStatus` text and the queue

`run.status_detail` is the R `RunStatus` string (`migration/03-migration-plan.md` 3.1) with one change:
when `full.log` holds several `*** Status:` lines, the last one wins (R leaves `"NA"`, BUG-036).
`run.status` maps that text as follows (`modelstats.run.state_of_detail`), then lets the queue refine it
(`modelstats.run.run_state`): every job of the run pending means `PENDING`, and a listed job turns
`UNKNOWN` (`full.log missing`: GAMS has not started yet) into `RUNNING`.

| R `RunStatus` | `RunState` |
|---|---|
| `Normal completion` | `COMPLETED` |
| `Execution error`, `Compilation error`, `Abort <regi> <n>*Infes`, `Abort <k>R*<n>*Infes`, any other GAMS status line (`Terminated due to ...`, `System error`) | `ERROR` |
| `Terminated by use` (the 17-character cut of `Terminated by user interrupt`), `Timeout interrupt`, `Memory interrupt`, `Preempt interrupt`, `Run requeued`, `Run cancelled`, `Run interrupted` | `INTERRUPTED` |
| `Run restarted` | `PENDING` |
| `Run in progress`, `conoptspy >Nh`, `mag-<i> <Iter>`, `Running reporting`, `Run MAgPIE <loop\|report\|>`, `Wait MAgPIE lock`, `Wait REMIND lock` | `RUNNING` |
| `NA`, `full.log missing` | `UNKNOWN` |

## R says / Run says (decision D-25)

`rs` prints the parity view (`get_run_status`, pinned by the status goldens) and is never affected. Where
the `Run` object says something else on purpose, the difference is listed here and in the module
docstring of `src/modelstats/run.py`, and `tests/unit/test_run.py` asserts each one on a synthetic case
with the parity record and the clean view side by side.

| Topic | R says | Run says | Why |
|---|---|---|---|
| Several `*** Status:` lines in `full.log` | `RunStatus` `"NA"`, then the interrupt fallback (`R/getRunStatus.R:157`, BUG-036) | the last line is `status_detail` | GAMS appends one status per restart; the last is the current state |
| `modelstat` | `"2: Locally Optimal"`, `"222222"`, the string `"NA"` or a real `NA` | `ModelStat(2, "Locally Optimal", "2")`, `ModelStat(None, None, "222222")`, `ModelStat(None, None, None)` | real `None` and numbers, not string conventions |
| `modelstat` fallback when `stats$config` has no `model_name` | the call aborts (`argument is of length zero`, `:91`, the BUG-005 input) | the REMIND fallback (`stats$modelstat` when it is one value) | a missing field is a missing input |
| `RunType` of a config without `cfg$model_name` or (REMIND) without `cfg$gms$optimization`; of a MAgPIE config without `cfg$gms$optimization` | the call aborts (`argument is of length zero`, `R/colRunType.R:20` / `:24`, the BUG-004 mechanism); the call aborts (`replacement has length zero`, `:73`, BUG-046, D-28) | `run_type` is `None`, `convergence` is `UNKNOWN` | a missing field is a missing input |
| `Runtime` | whole seconds; `now - timePrepareStart` whenever `jobInSLURM != "no"`, which off the cluster (`"NA"`) is every unfinished run (`:309-313`) | a `timedelta` with its fraction; live only while a job of the run is listed by `squeue`, else `None` | a live runtime without a job is the age of an abandoned run |
| `Conv` for a Nash run with `s80_bool` 0 below `cm_iteration_max` | the `p80_repy` modelstat digits over all regions (`:288-290`) | `Convergence.NOT_CONVERGED` | the flag is the model's verdict; the digits are a diagnostic |
| `Conv` when `s80_bool`, `o_iterationNumber` or `p80_repy` is missing from the GDX | the call aborts (`argument is of length zero`; `missing value where TRUE/FALSE needed`, BUG-038 / D-23; `incorrect number of dimensions`) | `UNKNOWN` without the flag, `NOT_CONVERGED` for a 0 flag whatever else is missing | missing inputs degrade |
| Pending job with a QOS containing a digit (`R/foundInSlurm.R:32`, BUG-031) | `"qos1 startup"` and `"Run in progress"` | `slurm.pending` is `True`, `status` is `PENDING` | `SlurmJob` reads the state field, not a regex on the line |
| Two or more `squeue` hits for one run (BUG-019, BUG-035) | `"<user>"` or `"<n> users"`, never a suffix | `slurm` is `None` (`facts.slurm.jobs` has every hit); `status` is `PENDING` when every hit is pending | one typed job cannot stand for several |
| `full.log missing` while a job of the run is listed | `"full.log missing"` | `status_detail` keeps the text, `status` is `RUNNING` | the job is alive, GAMS has not started (input preparation) |
| Plausibility block | counts only with a mif; the call aborts (`replacement has length zero`) when `projectSummations.rds` lacks `ScenarioMIP` or one of its elements (`:351-357`) | the three counts as `int` under the same mif rule; `project_summation` whenever the file exists, a missing element being `None` | one damaged element does not hide the counts |
| `runInAppResults` | `"yes"` / `"no"`, tested when any `stats` name contains `id` (`:107`); aborts on a `NULL`/`NA` `stats$config$model_name` (`:108`) | `True` / `False` when `stats$id` exists, `None` off the cluster; the MAgPIE archive by `isTRUE(model_name == "MAgPIE")` | the vacuous `overview.rds` rule (BUG-010) is kept |
| `Warnings`, `Iter`, `Mif`, `Conv` | strings (`"27"`, `"26/50 Clb: 3"`, `"sumErr"`, `"converged (had INFES)"`) | `int | None`, `Iterations(26, 50, 3, "26")`, `MifState`, `Convergence`; the `"NA"` string is `None` / `UNKNOWN` | types instead of string conventions |
| A path that does not exist | the row `"NA"` under `sort = "nf"` (`:30-32`) | a `Run` named after the path whose properties are `None` / `UNKNOWN` | a data-frame artefact is not a run |

Kept as R has them (rule-level, `06` 4.2): `Clb_converged` only above ten calibration files (BUG-001, D-09),
the `Abort ... *Infes` equality test (BUG-002, D-10), the REMIND warning count that misses `Warning message:`
and `50 or more` (BUG-029), the `overview.rds` rule (BUG-010), the unexpanded `cfg_mag$results_folder` in the
`mag-<i>` status text (BUG-021).

## Evidence

- `uv run pytest -q -m golden tests/golden/test_run.py` (`708 passed` on 2026-10-11): for every row of
  `migration/cases/status.tsv`, in both modes and both detail levels, `Run(dir).to_status_record(detailed)`
  equals the R golden exactly as `tests/golden/test_status.py` demands (key order, type, value, or the exact
  R error); on every passing row the clean view is read too, agrees with the record where the vocabulary
  allows, and reading both views again streams no file and runs no `squeue` a second time.
- `uv run pytest -q tests/unit/test_run.py` (`58 passed`): the `RunState` table above (every string of
  `03` 3.1), one test per difference of the table, the laziness and caching contract, `to_dict`,
  `is_run_folder`, `iter_runs`.
