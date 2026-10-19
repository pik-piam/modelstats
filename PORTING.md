# PORTING.md — translating modelstats from R to Python, one file at a time

You are translating one R file to Python. Read this whole document before writing any code. The goal of Phase A is a draft .py file next to the R file that captures the logic faithfully — it does not need to be able to pass all tests or align perfectly with the old R code. Phase B will focus on verifying the port against the R behaviour using the goldens and recorded quirks.

Two earlier ports exist on the branches `Python` (PR #154) and `Python-with-migration` (PR #155).
They are OUT OF SCOPE for this porting effort, DO NOT refer to them at all.

## 1. Ground rules

### 1.1 Fidelity

- **Idiomatic Python, same behaviour.** The visible behaviour of `rs`, `rs2` and `modeltests` (table
  contents, column widths, status strings, README text, exit codes) is preserved. Internals are free:
  dataclasses instead of data frames, `Path` instead of pasted strings, functions split where the R loop
  body is many lines long.
- **Very obvious bugs are fixed**, everything else is **recorded**. "Very obvious" means one of:
  the code cannot do what its own name or comment says;
  a quoting or robustness defect with exactly one correct fix; an accident of R evaluation that nobody intended.
- **Model semantics are never changed**
- **Goldens are not consulted while porting.** The R oracle outputs of the earlier migration record
  (`/home/tobias/code/modelstats/migration/goldens`, families `status`, `runtype`, `slurm`, `sort`,
  `printoutput`, `looprun`, `sanity`, `rs`, `amt`; gitignored, local only) are a check **at the very end**
  (see PORTING-TASKS.md). Consequence now: keep every output *diffable* — same column widths, same strings, same split
  between stdout and stderr — so that the final comparison needs only trivial normalisation (ANSI codes,
  the random hint line).
- **Later tasks** are documented in PORTING-TASKS.md. If you encounter a situation that might require a later task, record it there.

### 1.2 Output and environment contract

- **stdout** carries only the table or data the user asked for. **stderr** carries alerts, hints, the
  "Runs found: N" line, log messages.
- Colour only when enabled (`-b`/`--nocolor` off, `NO_COLOR` unset, stdout a TTY — rich handles the last two).
- **No shell strings, ever.** External programs are called with argument lists through `_shell` (§3.5);
  text is scanned by `_textscan` (§3.4), never by `grep`/`tac`/`awk`.
- **No process-global state.** No `os.chdir`; every path is a `Path`, every subprocess gets `cwd=`.
- **Duplicated R code is ported once.** The "current runs" logic exists twice
  (`R/commandLineInterface.R:141-176` and `R/promptAndRun.R:96-132`): port it once in `cli.py` as
  `current_run_dirs()` and import it in `prompt_and_run.py`. Same for the run-folder/main-folder tests.
- **Cluster constants** live in one place (`_shell.AMT_OUTPUT`, `_shell.AMT_RUNCODE`,
  `_shell.REMIND_RESULTS_ARCHIVE`, `_shell.MAGPIE_RESULTS_ARCHIVE_ENV`) instead of being pasted in four files.

## 2. File map and name map

| R file | Python module | Public names | Depends on (§3) |
|---|---|---|---|
| `R/colRunType.R` | `src/modelstats/run_type.py` | `run_type(run_dir: Path, cfg: dict \| None = None) -> str` | `_config`, `_textscan` |
| `R/foundInSlurm.R` | `src/modelstats/found_in_slurm.py` | `found_in_slurm(run_dir: Path, user: str \| None = None) -> str`, `SqueueJob` | `_shell`, `_sorting` |
| `R/printOutput.R` | `src/modelstats/print_output.py` | `format_row(row: TableRow, col_widths: Sequence[int], col_sep: str = "   ", cols: Sequence[str] \| None = None, *, cells: Mapping[str, object] \| None = None) -> str` (`cells`: display values replacing attributes), `TableRow` (Protocol: `name` plus the column attributes), `default_columns(on_cluster: bool) -> list[str]` | `_shell.on_cluster` |
| `R/getSanityChecks.R` | `src/modelstats/sanity_checks.py` | `print_sanity_checks(dirs: Sequence[Path] \| None = None, *, out: Console \| None = None) -> None`, `SANITY_COLUMNS` | `run_status`, `print_output`, `_console`, `_shell` |
| `R/loopRuns.R` | `src/modelstats/loop_runs.py` | `print_run_table(dirs: Sequence[Path], user: str \| None = None, color: bool = True, sort_by_time: bool = True, *, out: Console \| None = None) -> None`, `runtime_cell(row) -> str` | `run_status`, `print_output`, `_console`, `_timefmt` |
| `R/getRunStatus.R` | `src/modelstats/run_status.py` | `RunStatus` (dataclass), `get_run_status(dirs: Path \| Sequence[Path], *, newest_first: bool = True, user: str \| None = None, detailed: bool = True) -> list[RunStatus]`, `to_dataframe(rows) -> pd.DataFrame`, `EXPLAIN_MODELSTAT` | `_config`, `_gdx`, `_rdata`, `_textscan`, `_shell`, `_timefmt`, `run_type`, `found_in_slurm` |
| `R/commandLineInterface.R` | `src/modelstats/cli.py` | typer app `app` (console script `rs`), `main(argv)`, `is_run_folder`, `is_main_folder`, `current_run_dirs(user, days_back)`, `expand_paths(paths)`, `HINTS`; shared with rs2: `current_jobs`, `run_dirs_of_jobs`, `output_folder`, `user_path`, `MAGPIE_OUTPUT` | `loop_runs`, `sanity_checks`, `_choose`, `_console`, `_shell`, `_sorting`, `_textscan`, `_timefmt` |
| `R/promptAndRun.R` | `src/modelstats/prompt_and_run.py` | `prompt_and_run(paths: str = ".", user: str \| None = None, days_back: int = 3) -> None`, console script `rs2` (`main(argv)`) | `loop_runs`, `cli` (shared helpers), `_choose`, `_sorting`, `_rdata` |
| `R/modeltests.R` | `src/modelstats/modeltests.py` | `modeltests(...)`, `start_runs`, `evaluate_runs`, `delete_empty_realization_folders`, `notify_mattermost`, `AmtPaths`, `AmtState`, console script `modeltests` | `run_status`, `loop_runs`, `sanity_checks`, `print_output`, `_rdata`, `_shell`, `_timefmt`, `_sorting`, `_config`, `_textscan`, `_console` |
| `R/modelstats-package.R` | `src/modelstats/__init__.py` | `__all__`, `__version__ = "0.31.0"` (mirror `DESCRIPTION`) | — |

Porting order (each row only needs the rows above it): `_timefmt`, `_sorting`, `_shell`, `_console`,
`_textscan`, `_rdata`, `_config`, `_gdx`, `_choose` → `run_type`, `found_in_slurm`, `print_output` →
`run_status` → `loop_runs`, `sanity_checks` → `cli`, `prompt_and_run` → `modeltests`.

### 2.1 Name map

Functions (R → Python): `getRunStatus → get_run_status`, `colRunType → run_type`, `foundInSlurm →
found_in_slurm`, `printOutput → format_row`, `loopRuns → print_run_table`, `getSanityChecks →
print_sanity_checks`, `commandLineInterface → cli.main`, `promptAndRun → prompt_and_run`, `modeltests →
modeltests`, `startRuns → start_runs`, `evaluateRuns → evaluate_runs`, `deleteEmptyRealizationFolders →
delete_empty_realization_folders`, `.mattermostBotMessage → notify_mattermost`, `.readRuntime →
_read_runtime`, `is.runfolder → is_run_folder`, `is.mainfolder → is_main_folder`, `formatstr →
_format_cell`.

Arguments: `mydir → run_dir` (one folder) / `dirs` (several) / `model_dir` (the AMT checkout), `sort =
"nf" → newest_first: bool`, `user → user`, `detailed → detailed`, `colors → color`, `sortbytime →
sort_by_time`, `lenCols → col_widths`, `colSep → col_sep`, `cols → cols`,
`string → row`, `daysback → days_back`, `gitdir → report_repo`, `email → publish`, `compScen →
compare_scenarios`, `mattermostToken → mattermost_url`, `model → model`. (`len1stcol` has no counterpart: it is `col_widths[0]`.)

Columns of the status table (R name → `RunStatus` field → display title): `jobInSLURM → job_in_slurm →
inSlurm`, `RunType → run_type → RunType`, `modelstat → modelstat`, `runInAppResults → run_in_app_results →
AppResults`, `Mif → mif`, `Iter → iter`, `RunStatus → run_status`, `Warnings → warnings`, `Conv → conv`,
`Runtime → runtime`, `summationErrors → summation_errors → SumErr`, `rangeErrors → range_errors → RangeErr`,
`fixErrors → fix_errors → FixingErr`, `missingProjVars → missing_proj_vars → MissingVar`,
`projSummationErrors → proj_summation_errors → ProjSumErr`, `projSummationErrorsRegional →
proj_summation_errors_regional → ProjSumErrReg`. `to_dataframe()` uses the **R** column names so that
existing R-side consumers of the table find what they expect.

Console scripts: `rs` (`modelstats.cli:app`), `rs2` (`modelstats.prompt_and_run:main`), `modeltests`
(`modelstats.modeltests:app`).

## 3. Support layer

Private modules in `src/modelstats/`, one per R dependency or tool family. Each exists exactly once; a
porter who needs something not listed here adds it to the owning module, not to the file being ported.

### 3.1 `_gdx.py` — `gdx2::readGDX`, `quitte::as.quitte`, magclass subsetting

Decision: **`gamsapi[transfer]` + `gamspy_base`** (`gams.transfer.Container(system_directory=gamspy_base.directory)`),
verified on Python 3.14 without a GAMS installation. Licence is not a concern.

```python
class GdxReadError(ModelstatsError): ...
def read_scalar(path: Path, name: str) -> float | None         # o_iterationNumber, s80_bool, cm_abortOnConsecFail
def read_first_found(path: Path, names: Sequence[str]) -> list[float] | None   # c("o_modelstat", "p80_modelstat")
def read_records(path: Path, name: str) -> pd.DataFrame | None # p80_repy, p80_trackConsecFail (long format)
```

Semantics to keep (they are what `readGDX(..., react = "silent")` gave the R code):

- an absent symbol is `None`, never an exception (`react = "silent"`); `read_first_found` returns the
  values of the first name that exists;
- symbol lookup is **case-insensitive** (GAMS identifiers are); `type = "Parameter"` at
  `R/getRunStatus.R:281` only restricts the symbol class;
- `read_first_found` returns *all* values of the symbol, dense over its domain sets like `read_records`
  (gdx2's `restore_zeros`) — `o_modelstat` is a scalar for REMIND but a vector over years for MAgPIE,
  and the R code pastes the values together (`paste0(o_modelstat, collapse = "")`);
- `read_records` is a long frame with one column per domain set plus `value`, **densely reconstructed**:
  one row for every element of the cartesian product of the domain sets, in set order, `0` where the
  GDX stores no record (this is what `gdx2`/`magclass` return; `p80_repy[, , "modelstat"]` concatenates
  the modelstat of *every* region, zeros included). The region column is named after the domain set
  (`all_regi`), not `region` as `quitte` renamed it;
- special values: `EPS` → `0.0`, `INF`/`-INF` → `±inf`, `NA`/`UNDEF` → `nan`;
- a corrupt, truncated or non-GDX file raises `GdxReadError`. The GAMS libraries are not safe on damaged
  files (`gams.transfer` segfaults, `gdxcc.gdxOpenRead` allocates without bound on a file truncated behind
  its index block, BUG-042): every read runs in one worker process (`_gdx_worker`, started with
  `python -P -c <bootstrap>` that sets `RLIMIT_AS = _gdx.WORKER_MEMORY` before importing anything, cwd `/`,
  single-threaded BLAS, stderr discarded; pickled frames over pipes; killed and replaced when it dies or
  hangs for `_gdx.WORKER_TIMEOUT`; a file that failed is not read again until it changes). The parent
  process never loads the GAMS libraries;
- numbers that end up in text go through `_timefmt.r_num_str` (`2`, not `2.0`).

### 3.2 `_rdata.py` — `load()`, `readRDS()`, `saveRDS()`

Decision: **`rdata`** (pure Python, MIT) for reading; writing only for `runcode.rds`.

```python
def read_rda(path: Path) -> dict[str, Any]   # objects by name: {"cfg": {...}} / {"stats": {...}}
def read_rds(path: Path) -> Any
def read_rds_string(path: Path) -> str                 # readRDS of a file that must hold one string (runcode.rds, lastcommit.rds); else RDataReadError
def read_rds_frame(path: Path) -> pd.DataFrame         # readRDS of a file that must hold a data frame (runsToStart.rds, gRS.rds); else RDataReadError
def write_rds_string(path: Path, value: str) -> None   # runcode.rds only (read by R: rs-r -A, remindstatus)
```

Conversion rules (an adapter of ~100 lines over `rdata.read_rda(path, constructor_dict=...)`):

- named list → `dict[str, Any]`; `NULL` → `None`; `NA` in any representation → `None`;
- a length-1 atomic vector → the scalar (`str`/`int`/`float`/`bool`); longer → `list`;
- `POSIXct` → aware `datetime` in **UTC** (a POSIXct is an instant; the `tzone` attribute only affects
  printing). Not the zone, because Python subtracts two datetimes sharing one `ZoneInfo` object without
  consulting it and would lose an hour across a DST switch; call `.astimezone()` before printing a time.
  Elapsed times are differences of aware datetimes, never of wall-clock strings (DST-safe);
- a file that cannot be parsed raises `RDataReadError`;
- `difftime` → `float` seconds (its `units` attribute applied);
- `data.frame` → `pd.DataFrame` with the row names as index (`runsToStart.rds`, `gRS.rds` of an R checkout);
- objects of classes the R code never inspects (`packageDescription` inside every REMIND
  `runstatistics.rda`, the `magpie` array of a MAgPIE `stats$modelstat`) come back as their underlying
  value, without warnings.

The files the package reads: `config.Rdata` (object `cfg`), `runstatistics.rda` (object `stats`),
`projectSummations.rds` (nested list), `runcode.rds` (one string), `runsToStart.rds` (data frame),
`lastcommit.rds` (one string), `gRS.rds` (data frame), `report.rds` (MAgPIE, passed straight to R).

### 3.3 `_config.py` — `gms::loadConfig()` and `load("config.Rdata")`

Decision: **PyYAML `SafeLoader` + two tag constructors** (`!namedVector` → `dict`, `!character` → `str`
or `list[str]`). PyYAML speaks YAML 1.1 like R's `yaml` package: `yes`/`no` are booleans in both.

```python
CONFIG_NAMES = ("config.Rdata", "config.yml")
def find_config(run_dir: Path) -> Path | None     # exact names; config.Rdata wins if both exist
def load_config(path: Path) -> dict[str, Any]     # .Rdata → read_rda(path)["cfg"]; .yml → yaml.load(GmsLoader)
def cfg_get(cfg: Mapping, *keys: str) -> Any      # cfg$gms$cm_nash_mode → cfg_get(cfg, "gms", "cm_nash_mode"); None when absent
def r_equals(value, text: str) -> bool            # isTRUE(value == "text"): numbers/logicals coerced to character
def r_equals_number(value, number: float) -> bool # isTRUE(value == 1): numbers/logicals compared numerically
def r_as_number(value: Any) -> float | None   # as.numeric(): a number or a numeric string, else None (no "_" separators)
def r_greater_than(value, number: float) -> bool  # isTRUE(value > 0) for a number or a string holding one
class ConfigReadError(ModelstatsError): ...        # raised by load_config for an unreadable file
```

`gms::saveConfig` writes the tags in the verbatim form `!<namedVector>`; both forms are accepted, unknown tags
are ignored, mapping keys become text as R names them (`on` → `"TRUE"`), and `1_000`/`1:30`/timestamps stay
text as in R's `yaml` package.

Fields the R code reads: `title`, `model_name`, `results_folder`, `remind_folder`, `path_magpie`,
`cfg_mag$results_folder`, `gms$optimization`, `gms$cm_nash_mode`, `gms$cm_quick_mode`,
`gms$c_testOneRegi_region`, `gms$CES_parameters`, `gms$cm_MAgPIE_coupling`, `gms$cm_MAgPIE_Nash`,
`gms$c_empty_model`, `gms$cm_iteration_max`, `gms$cm_nash_autoconverge`. Switch values arrive as `str`
*or* `int` (`cm_nash_mode` is `"debug"` or `1`): compare with `r_equals` / `r_equals_number`, which coerce as
R's `==` does.

### 3.4 `_textscan.py` — `grep`, `tac`, `awk`, `tail`, `find`

Decision: **pure-Python scanners**, bounded memory. `full.gms` is 300–500 MB and only its last
`cm_iteration_max = N;` line matters: `last_match` reads the file backwards in 1 MB chunks and stops at
the first hit. The other logs are a few MB and may be read whole.

```python
def last_match(path: Path, pattern: str) -> str | None        # tac f | grep -m 1 P ; grep P f | tail -1
def first_match(path: Path, pattern: str) -> str | None       # grep P f (single use)
def all_matches(path: Path, pattern: str) -> list[str]        # grep P f
def last_line(path: Path) -> str | None                       # tail -1 f
def last_nonempty_line(path: Path) -> str | None              # awk 'NF{s=$0}END{print s}' f
def find_all_text(path: Path, pattern: str) -> list[re.Match[str]]   # grep -zoP P f (multi-line patterns)
def count_files(root: Path, glob: str) -> int                 # find ROOT -name 'glob' | wc -l (recursive)
def csv_rows(path: Path, delimiter: str) -> list[dict[str, str]]   # read.csv2(path, sep = delimiter): data rows by column
def csv_line_count(path: Path, delimiter: str) -> int         # nrow(read.csv2(path))
def csv_unique_values(path: Path, column: str, delimiter: str = ",") -> int   # length(unique(read.csv2(...)$column))
def compile_pattern(pattern: str, *, source: str) -> re.Pattern[str]   # a user-given regex; ModelstatsError "<source>: invalid pattern ..." (BUG-072)
def r_strsplit(text: str, separator: str) -> list[str]   # strsplit(text, sep, fixed = TRUE)[[1]]: one trailing empty field dropped, "" gives []
```

Lines are returned without their newline; files are decoded as UTF-8 with `errors="replace"`. Patterns
are Python `re` patterns.

| R shell-out | Python |
|---|---|
| `tac 'full.gms' \| grep -m 1 'cm_iteration_max = [1-9].*.;[ ]*$'` (`getRunStatus.R:144`) | `last_match(full_gms, r"cm_iteration_max = [1-9].+;[ ]*$")` (the BRE needs two characters before `;`, BUG-047) |
| `grep 'LOOPS' 'full.log' \| tail -1` (`:154`, `:230`) | `last_match(full_log, "LOOPS")` |
| `grep '*** Status: ' 'full.log'` (`:157`) | `all_matches(full_log, r"\*\*\* Status: ")` — literal stars; last line wins |
| `grep 'slurmstepd.*error' log.txt` (`:162`) | `all_matches(log_txt, r"slurmstepd.*error")` |
| `awk 'NF{s=$0}END{print s}' log.txt` (`:189`) | `last_nonempty_line(log_txt)` |
| `tac 'log.txt' \| grep -m 1 'Starting output generation for'` (`:204-205`, `:223-224`, `:232`) | `last_match(log_txt, "Starting output generation for")` (presence test) |
| `tail -1 log-mag.txt` (`:235`, `:240`) | `last_line(log_mag_txt)` |
| `grep -zoP "Warning messages:\n([0-9]+:(.*\n)?.*\n)*([0-9]+)" slurm.log \| tail -1` (`:258`) | `find_all_text(slurm_log, r"Warning messages:\n([0-9]+:(.*\n)?.*\n)*([0-9]+)")[-1].group(3)` |
| `grep "Warning message:" slurm.log \| tail -1` (`:261`) | `last_match(slurm_log, "Warning message:") is not None` |
| `grep -zoP "There were ([0-9]+) warnings" log.txt` (`:267`) | `find_all_text(log_txt, r"There were ([0-9]+)( or more)? warnings")[-1].group(1)` |
| `grep -zoP "Warning messages:\n([0-9]+:(.*\n)?.*\n)*" log.txt` (`:272`) | `find_all_text(log_txt, r"Warning messages:\n([0-9]+:(.*\n)?.*\n)*")`, then count the lines matching `^[0-9]+:` in *all* matches (R counted all blocks, see BUG-015) |
| `grep 'CES calibration iteration' log.txt \| grep -Eo '[0-9]{1,2}'` + `tail(n = 1)` (`:299`) | the last `[0-9]{1,2}` chunk over `all_matches(log_txt, "CES calibration iteration")` |
| `find <run> -name 'fulldata_*.gdx'` (`:302-303`) | `count_files(run_dir, "fulldata_*.gdx")` |
| `grep 'setGlobal optimization  ' full.lst`, `grep 'setglobal CES_parameters  ' full.lst` (`colRunType.R:37`, `:40`) | `first_match(full_lst, "setGlobal optimization  ")` |
| `find modules/ -name 'module.gms'` (`modeltests.R:72`) | `sorted(model_dir.glob("modules/**/module.gms"))` |
| `git log … \| grep 'Merge pull request'` (`modeltests.R:199-201`) | `[l for l in _shell.lines([...]) if "Merge pull request" in l]` |

### 3.5 `_shell.py` — `system()`, SLURM, git, Rscript, curl

Decision: **one thin module of typed wrappers over `subprocess.run`**; argument lists only; tests
monkeypatch its functions (`monkeypatch.setattr(_shell, "lines", fake)`).

```python
def run(argv: Sequence[str], *, cwd: Path | None = None, check: bool = False,
        input: str | None = None) -> subprocess.CompletedProcess[str]       # text=True, capture_output=True
def lines(argv: Sequence[str], *, cwd: Path | None = None) -> list[str]    # stdout.splitlines(); [] on failure
def squeue_argv(fmt: str, *, user: str | None = None) -> list[str]         # ["squeue", ("-u", user,) "-h", "-o", fmt] — R's order
def squeue(fmt: str, *, user: str | None = None) -> list[str]              # lines(squeue_argv(...))
def sacct_argv(user: str, since: date, fmt: str) -> list[str]              # -s cd,f,cancelled,timeout,oom -S <since> -E now -P -n --format <fmt>
def sacct(user: str, since: date, fmt: str) -> list[str]                   # lines(sacct_argv(...))
def rscript_argv(expression: str, *args: str) -> list[str]                # ["Rscript", "-e", expression, *args]; args = commandArgs(trailingOnly = TRUE)
def post_json(url: str, payload: Mapping[str, Any]) -> None                # urllib.request, Content-Type: application/json; a failure (OSError, HTTPException) is logged, not raised
def on_cluster() -> bool                                                   # Path("/p").exists()
def current_user() -> str                                                  # pwd.getpwuid(os.getuid()).pw_name  (R: Sys.info()[["user"]]; getpass prefers $USER)
sleep = time.sleep                                                         # module attribute; tests replace it
AMT_OUTPUT = Path("/p/projects/remind/modeltests/remind/output")
AMT_RUNCODE = Path("/p/projects/remind/modeltests/remind/runcode.rds")
REMIND_RESULTS_ARCHIVE = Path("/p/projects/rd3mod/models/results/remind")
MAGPIE_RESULTS_ARCHIVE_ENV = "MAGPIE_RESULTS_ARCHIVE_PATH"
```

Output is decoded as UTF-8 with replacement characters (a stray byte in a job name never raises).
`system(cmd, intern = TRUE)` returned a character vector plus a `status` attribute: use `lines()` where
the R code only consumed the lines (output of a failing program is discarded) and `run()` where it tested the status (`evaluateRuns`' squeue
failure counter, `R/modeltests.R:183`). The AMT's programs run through `modeltests._run` (`run(..., check=True)`,
output logged): the model's git update, the `Rscript start.R` calls and the `selectScenarios()` bridge let
`CalledProcessError` abort the step (a program that cannot start, e.g. a missing folder, is a `ModelstatsError`),
the auxiliary ones (`make`, `rsync`, `sbatch`, the `addToDataChangelog()` bridge, the report repository's git)
log the failure and go on (BUG-067). Values for R expressions are passed as `Rscript` arguments
(`rscript_argv`), never interpolated into the R code.

HTTP: `urllib.request` (stdlib), one POST, `json.dumps({"text": message})`. The webhook URL is
a parameter (`--mattermost-url`, or the environment variable `AMT_MATTERMOST_URL`), never echoed.

### 3.6 `_console.py` — `crayon`, `cli::cli_alert_*`, `cat`

Decision: **rich** (already a dependency of typer 0.27).

```python
def make_console(*, stderr: bool = False, color: bool = True) -> Console   # no_color=None if color else True (rich honours NO_COLOR / non-TTY)
out = make_console()                                           # stdout, the table stream
err = make_console(stderr=True)                                # stderr, alerts and hints
def info(message: str | Text) -> None     # cli_alert_info:    "ℹ <message>"   (cyan glyph, as cli's theme)
def error(error: object) -> None          # "Error: <error>" on stderr, the line of a failed command (exit status 1)
def run_app(app, argv, *, prog_name) -> int   # a typer app as a console script: SystemExit -> exit status (None is 0)
def warn(message: str) -> None     # cli_alert_warning: "! <message>"   (yellow glyph)
def styled(text: str, style: str | None) -> Text               # a row with a colour, or none
def capture() -> tuple[Console, io.StringIO]                   # plain-text capture (capture.output of a table)
def tolerate_undecodable_names() -> None                       # stdout/stderr with surrogateescape (console scripts)
STYLES = {"yellow": "yellow", "cyan": "cyan", "green": "green", "magenta": "magenta",
          "orangered": "orange_red1", "orange": "orange1", "underline": "underline"}
```

Rows are printed as `rich.text.Text` objects (never through markup — run names may contain `[`). `-b` /
`--nocolor` constructs the consoles with `no_color=True`; rich also honours `NO_COLOR` and non-TTY
output by itself. Closing codes differ from crayon's (`ESC[0m` vs `ESC[39m`): the final golden check
compares after stripping ANSI sequences.

### 3.7 `_sorting.py` — `stringi::stri_order`, `gtools::mixedsort/mixedorder`

Decision: **own sort key, no dependency.** R used three sorts that disagree with each other
(`stri_order(numeric = TRUE)` in `rs`, `mixedsort` and `mixedorder(gsub("-", "", x))` in `rs2`); the
only numbers that matter are coupling iterations (`-rem-10` after `-rem-9`) and numbers inside scenario
names (`PkBudg500` before `PkBudg1000`). Timestamps in folder names are zero-padded and sort as strings.

```python
ITERATION = re.compile(r"^(?P<stem>.*)-(?P<kind>rem|mag)-(?P<iteration>[0-9]+)$")
TIMESTAMP = re.compile(r"^(?P<stem>.*)_(?P<ts>[0-9]{4}-[0-9]{2}-[0-9]{2}_[0-9]{2}\.[0-9]{2}\.[0-9]{2})$")
@dataclass(frozen=True)
class RunName: name: str; stem: str; kind: str | None; iteration: int | None; timestamp: str | None
def parse_run_name(name: str) -> RunName
def natural_key(s: str) -> tuple[Any, ...]        # re.split(r"(\d+)", s) → (text.lower(), int) pairs
def run_sort_key(path: Path) -> tuple[Any, ...]   # (natural_key(stem), timestamp or "", iteration or 0, name)
def is_coupled(name: str) -> bool                 # grepl("-(rem|mag)-[0-9]+$", name)
def last_iterations(paths: Sequence[Path]) -> list[Path]   # one path per (parent, stem, kind): the highest iteration
def last_timestamps(paths: Sequence[Path]) -> list[Path]   # one path per (parent, stem): the latest timestamp (rs2 -s)
def existing_dirs(paths: Sequence[Path], *, newest_first: bool) -> list[Path]   # file.info() once per path; directories only; mtime order
def subdirs(folder: Path) -> list[Path]   # list.dirs(folder, recursive = FALSE): direct subfolders by code point; [] when unreadable
```

Everything that sorts run folders calls `sorted(paths, key=run_sort_key)`; nothing else sorts.

### 3.8 `_choose.py` — `gms::chooseFromList()`

Decision: **port the input grammar** (later task: a typer/rich prompt, see PORTING-TASKS.md).

An invalid answer (a number out of range, a descending range `3-2`, `1 2`, a lone `,`, a pattern that does not compile) asks again; an
empty answer selects nothing. An answer of digits, `,`, `:`, `-` and blanks only is read as numbers, never as a
pattern (`2026-10-18` asks again, `2026-10-18_` is a pattern).

```python
def choose_from_list(items: Sequence[str], *, kind: str = "folders",
                     read: Callable[[str], str] = input, out: Console | None = None) -> list[str]   # None: _console.err at call time
```

Prints `1: item` lines, prompts `Choose <kind> (numbers, ranges 1-3, comma lists, 'all'/'a', 'p'/'f' for a pattern prompt, a regex; empty = none): `,
reads **one line per prompt**, accepts a number, a range `1-4` or `1:4` (spaces allowed around the numbers),
a comma list `2,5`, `all` (or `a`, the shortcut `gms::chooseFromList` printed as `1,a: all`, alone or in a list
such as `2,a`; a number out of range asks again), a lone `p`/`f` (a second prompt asks for a regular expression /
a fixed string until it compiles, as in R, without R's match list and confirmation; an empty pattern or the end
of the input selects nothing), or any other text as a regular expression over the items. Returns the chosen items
in list order.

### 3.9 `_timefmt.py` — `lubridate::make_difftime`, `piamutils::niceround`, R number formatting

```python
def now() -> datetime                       # Sys.time(): aware, local zone; the only clock read (tests replace it)
def today() -> date                         # Sys.Date()
def format_runtime(seconds: float) -> str   # make_difftime(second = s) → format(round(., 1))
def niceround(x: float, digits: int = 3) -> str   # piamutils' default; the status passes 1
def r_num_str(x: float) -> str              # as.character()/paste0() of a number
def r_str(value: object) -> str             # as.character() of a scalar (TRUE/FALSE, 2 for 2.0)
def r_paste0(values: object) -> str         # paste0(as.character(x), collapse = "") of a scalar/vector/named vector
def r_round(x: float, digits: int = 0) -> float   # R's round(): decimal ties resolved in double arithmetic (63 s → "1 mins")
def r_signif(x: float, digits: int) -> float      # R's signif()
```

Call the clock through the module (`_timefmt.now()`), never `from _timefmt import now`, so that the test
fixture can replace it.

`format_runtime`: unit by magnitude — `secs` if `s < 60`, `mins` if `< 3600`, `hours` if `< 86400`, else
`days`; value `r_round(s / div, 1)` printed with up to 7 significant digits (trailing zeros dropped, like R). Reference (lubridate 1.9.5):

| seconds | output | seconds | output |
|---|---|---|---|
| 0 | `0 secs` | 3600 | `1 hours` |
| 3 | `3 secs` | 3661 | `1 hours` |
| 45.6 | `45.6 secs` | 5400 | `1.5 hours` |
| 59.99 | `60 secs` | 12345 | `3.4 hours` |
| 60 | `1 mins` | 62730.5 | `17.4 hours` |
| 125.4 | `2.1 mins` | 86399 | `24 hours` |
| 3599 | `60 mins` | 86400 | `1 days` |
| | | 2592000 | `30 days` |

`niceround(x, digits)`: `digits = max(digits, ceil(log10(abs(x))))`, then `signif(x, digits)` printed
without scientific notation: `0.35 → 0.4`, `1.25 → 1`, `9.99 → 10`, `12.3 → 12`, `120.4 → 120` (so the
status reads `conoptspy >1h`, never `>1.2h`).

`r_num_str`: integers without decimals (`2`), otherwise up to 15 significant digits (`0.5`), scientific
notation for `>= 1e5` or `< 1e-4` as R prints them (`1e+05`).

### 3.10 Dropped R dependencies

| R | Why / replacement |
|---|---|
| `lucode2::sendmail` | imported, never called |
| `dplyr`, `rlang::.data`, `%>%` | comprehensions over `RunStatus` rows |
| `withr::local_dir/with_dir` | explicit `Path`s and `cwd=` (§4.7) |
| `utils::read.csv2(sep = ",")` | `csv.DictReader`; only `len({row["variable"]})` and row counts are used, the decimal-comma setting is irrelevant |
| `gtools`, `stringi` | `_sorting` |
| `magpie4::addToDataChangelog`, REMIND's `selectScenarios()` | `Rscript -e` bridges; R stays installed on the cluster |

## 4. Concept translation

Rules, not a dictionary.

### 4.1 Values, `NULL`, `NA` and the string `"NA"`

| R | Python | Note |
|---|---|---|
| `NULL` | `None` | |
| `NA` (any type) | `None` | `_rdata` converts every representation |
| the *string* `"NA"` in a status column | the string `"NA"` | R shows real `NA` as blanks and `"NA"` as `NA` — they differ in the table; keep both |
| `isTRUE(x == "v")` / `isTRUE(x == 1)` | `r_equals(x, "v")` / `r_equals_number(x, 1)` | R's `==` coerces to character against a string (`1 == "1"`) and to numeric against a number (`TRUE == 1`) |
| `is.null(x)` / `length(x) == 0` | `x is None` / `not x` | a zero-length vector in `if ()` is an R error — the Python guard is explicit |
| `x %in% c(a, b)` | `x in (a, b)` | |
| `any(grepl("id", names(stats)))` | `"id" in stats` | the R test is unanchored; exact membership is the intent |
| `ifelse(cond, a <- f(), b <- g())` as a statement | `if cond: a = f() else: b = g()` | `colRunType.R:19`, `getRunStatus.R:72` |
| `exists("cfg")` | `cfg is not None` | |
| `try(expr, silent = TRUE)`; `inherits(x, "try-error")` | `try: … except <NarrowError>: x = None` | catch the specific error (`GdxReadError`, `OSError`), never `Exception` — with one exception: the support modules wrap a third-party parser of arbitrary bytes (rdata, PyYAML, the GDX worker) in a *total* boundary that turns any failure into the module's `...ReadError`, so that callers can catch narrowly |
| `suppressWarnings(...)` | nothing | |
| `invisible(x)` / `return(NULL)` | `return None` | |
| `c(x, y)` growing a vector | `list.append` / `list.extend` | |
| `paste0(x, character(0))` | — | yields `"x"` in R (recycling); never port literally |
| `stop("msg")` | `raise ModelstatsError("msg")` | the CLI maps it to exit status 1 |

### 4.2 Strings and regular expressions

R's default regex engine (TRE, `perl = FALSE`) is POSIX ERE; GNU `grep` without `-E` is BRE; `grep -P` is
PCRE. Python `re` is PCRE-like. Translate as follows:

| R / grep | Python | Note |
|---|---|---|
| `grepl(p, x)` | `re.search(p, x) is not None` | |
| `grepl(p, x, fixed = TRUE)` | `p in x` | |
| `sub(p, r, x)` / `gsub(p, r, x)` | `re.sub(p, r, x, count=1)` / `re.sub(p, r, x)` | |
| `"\\1"` in a replacement, `"\\("` in a pattern | `r"\1"`, `r"\("` | raw strings everywhere |
| `"^.*.= "` | `r"^.*= "` | `.*.` is `.+` in effect; the lines matched always have text before `= ` |
| BRE `'*** Status: '` | `r"\*\*\* Status: "` | in BRE the leading `*` is literal and the next two quantify it, so grep matched any ` Status: ` line ; the literal is the intent |
| `grep -zoP P` (NUL-separated, multi-line) | `re.finditer(P, text)` over the whole file | `.` excludes `\n` in both; the R side only ever saw the *first* match because `system()` cut at the NUL |
| `substr(x, 1, 17)` | `x[:17]` | |
| `nchar(x)` | `len(x)` | |
| `paste0(v, collapse = "")` | `"".join(v)` | elements through `r_num_str` when numeric |
| `paste(a, b)` | `f"{a} {b}"` | `paste("x", NULL)` is `"x "` in R; in Python join only the non-`None` parts |
| `strsplit(x, ",")[[1]]` | `_textscan.r_strsplit(x, ",")` | `x.split(",")` keeps a trailing empty field and turns `""` into `[""]`; R drops the one and gives `character(0)` for the other |
| `trimws(x, which = "right", whitespace = " ")` | `x.rstrip(" ")` | |
| `rev(strsplit(s, " ")[[1]])[[3]]` | `s.split(" ")[-3]` | better: parse the fields once |
| `startsWith(x, p)` | `x.startswith(p)` | |
| `format(Sys.time(), "%Y-%m-%d %H:%M:%S")` | `now.strftime("%Y-%m-%d %H:%M:%S")` | |
| `sprintf` / `format(x, …)` | f-strings | numbers through `r_num_str` / `format_runtime` |

### 4.3 Vectors, lists and the status table

| R | Python |
|---|---|
| `sapply(x, f)` / `lapply` / `unlist(lapply(...))` | `[f(v) for v in x]` |
| `which.max(v)` | `max(range(len(v)), key=v.__getitem__)` |
| `unique(v)` (order kept) | `list(dict.fromkeys(v))` |
| `setdiff(a, b)` (order of `a`) | `[x for x in a if x not in set(b)]` |
| `head(v, 1)` / `tail(v, 1)` | `v[:1]` / `v[-1:]` (lists, so that emptiness survives) |
| `seq_along(v)` | `enumerate(v)` |
| `rev(v)` | `v[::-1]` |
| `data.frame()` + `out[i, "col"] <- v` | `RunStatus` dataclass per run; `row.col = v` |
| `rownames(df)` / `names(df)` | `row.name` / dataclass fields |
| `df[i, "col"]` in a caller | `row.col` |
| `rbind(a, b)` | `a + b` (lists) |
| `dplyr::filter(df, cond)`, `mutate`, `pull` | comprehensions over the list of rows |
| `try(df[i, "col"] <- <vector>)` | the R assignment *fails silently* when the vector has length ≠ 1: never port it literally — write the single-value rule out |

`get_run_status()` returns `list[RunStatus]` in the requested order; `to_dataframe(rows)` builds the
pandas frame (`index = name`, R column names) for library users. pandas appears nowhere else in the
`rs` path.

### 4.4 Files and paths

| R | Python |
|---|---|
| `file.path(a, b)` / `paste0(dir, "/", f)` | `a / b` (`Path`) |
| `normalizePath(p)` | `p.resolve()` (R returns a non-existing path unchanged, `resolve()` makes it absolute; only existing run folders reach the status code, BUG-031) |
| `basename(p)` / `dirname(p)` | `p.name` / `p.parent` |
| `file.exists(p)` / `dir.exists(p)` | `p.exists()` / `p.is_dir()` |
| `file.info(p)$mtime` / `$size` / `$isdir` / `$ctime` | `p.stat().st_mtime` / `.st_size` / `p.is_dir()` / `.st_ctime` (aware `datetime` via `datetime.fromtimestamp(ts, tz)` where compared with `Sys.time()`) |
| `dir(p)` | `sorted(e.name for e in p.iterdir() if not e.name.startswith("."))` |
| `list.dirs(p, recursive = FALSE)` | `sorted(e for e in p.iterdir() if e.is_dir())` (dot-directories included, as in R) |
| `list.files(pattern = re, include.dirs = TRUE)` | `[e for e in p.iterdir() if re.search(re_, e.name)]` |
| `Sys.glob(file.path(d, "225*", "grid*", "gmsgrid.log"))` | `d.glob("225*/grid*/gmsgrid.log")` |
| `readLines(p)` / `writeLines(s, p)` | `p.read_text().splitlines()` / `p.write_text(s + "\n")` |
| `write(x, f, append = TRUE)` | collect lines in a list, `write_text("\n".join(lines) + "\n")` once |
| `file.copy`, `file.rename`, `unlink(recursive = TRUE)` | `shutil.copy`, `Path.rename` (or `shutil.move` across devices), `shutil.rmtree` |
| `tempdir()` | `tempfile.TemporaryDirectory()` |
| `read.csv2(f, sep = ",")` | `csv.DictReader(open(f, newline=""))` |
| `sed -i 's/a/b/' f` | `f.write_text(f.read_text().replace("a", "b"))` |
| `mv a b c archive` | `shutil.move(a, archive / a.name)` per item |
| `find d -name 'x'` | `_textscan.count_files` / `d.rglob("x")` |

### 4.5 Time

| R | Python |
|---|---|
| `Sys.time()` | `_timefmt.now()` (aware, local zone) |
| `Sys.Date()` | `_timefmt.today()` |
| `as.Date(format(Sys.Date(), "%Y-%m-%d")) + 1` | `_timefmt.today() + timedelta(days=1)` |
| `Sys.Date() - daysback` | `_timefmt.today() - timedelta(days=days_back)` |
| `difftime(a, b, units = "secs")` / `"hours"` | `(a - b).total_seconds()` / `/ 3600` |
| `as.numeric(round(difftime(...), 0))` | `round(...)` → `int` |
| `make_difftime(second = s)` + `format(round(., 1))` | `_timefmt.format_runtime(s)` |
| `format(x$mtime, "%Y-%m-%d")` | `datetime.fromtimestamp(st_mtime).strftime("%Y-%m-%d")` |
| `as.Date(gsub(..., runname))` | `date.fromisoformat(m.group(1))` when the name matches, else skip |

### 4.6 Processes, environment, output

| R | Python |
|---|---|
| `system(cmd, intern = TRUE)` | `_shell.lines(argv)` |
| `attributes(x)$status` | `_shell.run(argv).returncode` |
| `system(cmd)` (side effect) | `_shell.run(argv, check=True, cwd=...)` |
| `Sys.info()[["user"]]` | `_shell.current_user()` |
| `Sys.getenv("X")` / `Sys.setenv(X = "v")` | `os.environ.get("X")` / `env={**os.environ, "X": "v"}` passed to `run` (never mutate the process environment) |
| `file.exists("/p")` | `_shell.on_cluster()` |
| `cat(...)` (table stream) | `_console.out.print(Text(...))` |
| `message(...)`, `cli_alert_info`, `cli_alert_warning` | `_console.info/warn` in `rs`/`rs2`; `logging` in `modeltests` |
| `print(x)` | `_console.err.print(...)` (R's `print` of a vector was diagnostic output) |
| `quit(save = "no", status = 0)` | `raise typer.Exit(0)` |
| `Sys.sleep(n)` | `_shell.sleep(n)` |
| `curl -X POST …` | `_shell.post_json(url, {"text": message})` |

### 4.7 Control flow and structure

- `for (i in x) { … next }` → `for … continue`; `repeat { Sys.sleep(); if (…) break }` → `while True`.
- `withr::local_dir(d)` / `with_dir(d, …)` → no `chdir`. `modeltests.py` carries an `AmtPaths` dataclass
  with every directory it touches (`model_dir`, `output_dir`, `state_file`, `runcode_rds`, `report_repo`,
  `magpie_dir`, `test_full_log`) and passes `cwd=` to each subprocess.
- A 300-line R function body becomes one public function that calls private helpers named after the R
  comment blocks (`# modelstat`, `# MIF`, `# Iter`, `# RunStatus`, …), all in the same module.
- Recursion (`getRunStatus()` on the coupled MAgPIE folder) stays recursion.
- Mutual exclusion that R got from cron timing (`modeltests`) gets a `fcntl.flock` on a lock file next to the
  state file (`.modeltests.lock`; the state file itself is replaced by rename on every save, so a lock on it would
  not keep two runs apart).

### 4.8 Documentation, types, packaging

- roxygen `#' @param` → Google-style docstrings. Every module starts with a docstring: a summary line, then
  `R source: R/<file>.R`, then the deviations and fixes with their `BUG-` ids. Don't include any other
  references to the R source.
- `@export` → `__all__` in the module and re-export from `__init__.py`.
- `DESCRIPTION`/`NAMESPACE` → `pyproject.toml`; `man/` has no Python counterpart; vignettes become `docs/`
  later (see PORTING-TASKS.md).
- Full type hints, `uv check`. `Path` for paths, `str` for display cells (may hold `"NA"`), `int | None`
  for counts, `float | None` for seconds, `datetime` always aware.

## 5. Package layout and tooling

```
pyproject.toml                 # uv, src layout, requires-python >= 3.14, uv_build backend
src/modelstats/
  __init__.py  run_type.py  found_in_slurm.py  print_output.py  sanity_checks.py  loop_runs.py
  run_status.py  cli.py  prompt_and_run.py  modeltests.py
  _gdx.py  _rdata.py  _config.py  _textscan.py  _shell.py  _console.py  _sorting.py  _choose.py  _timefmt.py
  errors.py                    # ModelstatsError and the subclasses the support modules raise
  py.typed
tests/
  conftest.py                  # make_run(tmp_path, name, files=...) builders, fake _shell answers, tiny GDX/.rda data
  data/                        # a few small real config.Rdata / runstatistics.rda / config.yml files
  test_<module>.py             # one per module, synthetic inputs only
```

- Dependencies: `gamsapi[transfer]>=54.4,<55`, `gamspy_base>=54.4,<55`, `rdata>=1.1,<2`, `pyyaml>=6,<7`,
  `typer>=0.27,<1` (brings `rich`). Dev: `ruff`, `ty`, `pytest`, `pandas-stubs`, `types-PyYAML`,
  `pre-commit`.
- `ruff`: line length 110, `select = ["E", "F", "I", "UP", "B", "SIM", "N", "D"]`, `target-version = "py314"`.
  `ty` on `src` and `tests`; `ignore_missing_imports` for `gams.*` and `gamspy_base`.
- Console scripts: `rs = "modelstats.cli:app"`, `rs2 = "modelstats.prompt_and_run:main"`,
  `modeltests = "modelstats.modeltests:app"`.
- `__init__.py` re-exports `run_type`, `found_in_slurm`, ... under the names of their modules (not `cli.main`:
  R's `commandLineInterface()` is the console script `rs`, and a re-export would shadow the `cli` module), so
  `modelstats.run_type` is the function; tests patch a function where it is used
  (`monkeypatch.setattr(run_status, "found_in_slurm", fake)`).
- Tests never run programs: an autouse fixture makes every `_shell` call fail unless the test uses the
  `shell` fixture (`FakeShell`) or is marked `real_shell` (`test_shell.py` only).
- Tests never touch the cluster, the network or R: `_shell` is monkeypatched; GDX fixtures are written
  with `gams.transfer.Container.write` in `conftest.py`; R data files come from `tests/data/` (small real
  files.
- The R package (`R/`, `DESCRIPTION`, `NAMESPACE`, `man/`, `tests/testthat`, `vignettes/`) stays
  untouched until the switch-over.



