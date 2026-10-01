# Run Analysis Tools

## Python port

This branch carries a Python port of the package (Python 3.14, [uv](https://docs.astral.sh/uv/),
`src/modelstats/`) next to the unchanged R package. The port reproduces the R behaviour
byte for byte where the R output is pinned, including the R package's known quirks; nothing
in the R sources was changed. Two console scripts are provided:

- `rs`: the command line of `commandLineInterface()`, same options (`-A -b -C -d N -f REGEX
  -l -m -p -s -t -u USER`, bundled short flags, at most one comma-separated path argument,
  `.` by default), the status table on stdout, the information lines on stderr, exit status
  0 wherever the R version quits normally and 1 for an R error.
- `modeltests`: the automated model tests (`modeltests()`):
  `modeltests --mydir DIR --gitdir DIR --model REMIND|MAgPIE --user USER [--email/--no-email]
  [--comp-scen/--no-comp-scen] [--mattermost-token-env NAME] [--dry-run]`; reads
  `<mydir>/../.testsstatus` and starts (`next:start`) or evaluates (`next:evaluate`) the test
  runs, the messages on stderr; the Mattermost webhook is read from the environment variable
  named with `--mattermost-token-env` (unset: no notification); `--dry-run` executes the reads
  and logs every mutation as `would ...` on stderr instead of performing it (the two Rscript
  bridges still run; they only read the checkout and write under the temporary directory);
  exit status 0, or 1 for an R error (`Error in <call> : ...` / `Execution halted` as Rscript
  prints it).

### Installation

`uv` fetches Python 3.14 itself, so no system Python or module is needed:

```sh
uv tool install modelstats-0.31.0-py3-none-any.whl   # from a built wheel (uv build), or:
uv tool install --from git+https://github.com/pik-piam/modelstats modelstats
rs -h
```

For development, inside a checkout:

```sh
uv sync --locked                                    # the environment with the dev tools
uv run rs -h
uv run pytest -q                                    # unit tier (no fixtures needed)
uv run ruff check src tests && uv run mypy --strict src
```

### Transition from the R command line

The R package stays installed and usable during the transition. When the cluster's `rs`
wrapper is switched to the Python entry point, the R implementation remains available as
`rs-r`, so the two can be compared on the same folders (`rs-r -b folder` versus
`rs -b folder`) until the R command line is retired. Known differences are limited to the
help page layout, the wording of usage errors, and cases where R aborts (a corrupt GDX file,
`-p` with piped input), which the port reports as an error row or a selection instead.

### `rs --found-in-slurm DIR` (the contract for REMIND's `readcoupled.R`)

REMIND's `scripts/utils/readcoupled.R` calls `modelstats::foundInSlurm(folder)` and compares
the result with `"no"`. The same value is available without R:

```sh
rs --found-in-slurm /p/projects/remind/runs/.../output/SSP2-NPi
```

prints exactly the `foundInSlurm()` string plus a newline on stdout and nothing else (no
information lines, no hint), exit status 0; the values are `no`, the QOS of your job (for
example `priority`), the user name of someone else's job, or `N users`, with ` startup` or
` pending` appended as in R. The directory is not validated, exactly as in R: `foundInSlurm()`
keeps a path that does not exist (its `normalizePath` warning is suppressed) and reports `no`
for it. Only when the value cannot be computed (`squeue` cannot be run or fails, or
`foundInSlurm` itself would error in R) nothing is printed on stdout, the message goes to
stderr and the exit status is 1.

### Test tiers

| Tier | Command | Needs |
|---|---|---|
| unit | `uv run pytest -q` | nothing beyond `uv sync` |
| golden | `uv run pytest -q -m golden tests/golden` | the maintainers' `migration/` tree (harness, cases, R goldens, 1.3 GB of fixtures; not in this repository); skipped with a message when it is absent |
| packaging | `uv run pytest -q -m packaging tests/packaging` | `uv`: builds the wheel, installs it into a fresh venv and runs the installed `rs` and `modeltests` |

The golden tier runs the port inside the same sandbox (bubblewrap, frozen clock, fake
`squeue`/`sacct`) that produced the R reference output and compares the two byte for byte;
its driver is `migration/harness/make_goldens.sh --candidate python`. The migration plan with
the behavioural contract (`migration/03-migration-plan.md`), the library decisions
(`migration/02-python-libraries.md`), the bug register (`migration/04-bug-register.md`) and
the findings of the port (`migration/06-port-findings.md`) live in that `migration/`
directory, which is kept out of git on purpose.

R package **modelstats**, version **0.31.0**

   [![R build status](https://github.com/pik-piam/modelstats/workflows/check/badge.svg)](https://github.com/pik-piam/modelstats/actions) [![codecov](https://codecov.io/gh/pik-piam/modelstats/branch/master/graph/badge.svg)](https://app.codecov.io/gh/pik-piam/modelstats) [![r-universe](https://pik-piam.r-universe.dev/badges/modelstats)](https://pik-piam.r-universe.dev/builds)

## Purpose and Functionality

A collection of tools to analyze model runs.


## Installation

For installation of the most recent package version an additional repository has to be added in R:

```r
options(repos = c(CRAN = "@CRAN@", pik = "https://rse.pik-potsdam.de/r/packages"))
```
The additional repository can be made available permanently by adding the line above to a file called `.Rprofile` stored in the home folder of your system (`Sys.glob("~")` in R returns the home directory).

After that the most recent version of the package can be installed using `install.packages`:

```r
install.packages("modelstats")
```

Package updates can be installed using `update.packages` (make sure that the additional repository has been added before running that command):

```r
update.packages()
```

## Tutorial

The package comes with vignettes describing the basic functionality of the package and how to use it. You can load them with the following command (the package needs to be installed):

```r
vignette("rs2")          # Run statistics 2 (rs2)
vignette("testingSuite") # Testing Suite
```

## Questions / Problems

In case of questions / problems please contact Anastasis Giannousakis <giannou@pik-potsdam.de>.

## Citation

To cite package **modelstats** in publications use:

Giannousakis A, Richters O, Krogmann S (2026). "modelstats: Run Analysis Tools." Version: 0.31.0, <https://github.com/pik-piam/modelstats>.

A BibTeX entry for LaTeX users is

 ```latex
@Misc{,
  title = {modelstats: Run Analysis Tools},
  author = {Anastasis Giannousakis and Oliver Richters and Simon Krogmann},
  date = {2026-09-25},
  year = {2026},
  url = {https://github.com/pik-piam/modelstats},
  note = {Version: 0.31.0},
}
```
