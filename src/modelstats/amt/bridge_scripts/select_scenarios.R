#!/usr/bin/env Rscript
# select_scenarios.R - modelstats bridge (modelstats.amt.bridges.select_scenarios): R/modeltests.R lines 127-138 of
# startRuns() as a subprocess, because selectScenarios() is REMIND's code (scripts/start/*.R), not modelstats'.
#
#   Rscript select_scenarios.R --out PATH [--config config/scenario_config.csv] [--startgroup AMT]
#                              [--scripts scripts/start] [--suffix STR]
#
# Run with cwd = the model directory (mydir), as startRuns() runs there. Exactly as R/modeltests.R does:
#   settings <- read.csv2(config, stringsAsFactors = FALSE, row.names = 1, comment.char = "#", na.strings = "")
#   invisible(sapply(list.files(scripts, pattern = "\\.R$", full.names = TRUE), source))
#   runsToStart <- selectScenarios(settings = settings, interactive = FALSE, startgroup = startgroup)
#   row.names(runsToStart) <- paste0(row.names(runsToStart), suffix)      # only when --suffix is given (line 137)
#   saveRDS(runsToStart, file = out)                                      # line 138, via a temporary file + rename
# then prints ONE JSON line as the last line of stdout (bridges.py parses the last non-empty line):
#   {"bridge":"select_scenarios","ok":true,"row_names":[...],"columns":[...],"nrow":N,"out":PATH,
#    "sources":[{"file":...,"algorithm":"sha256"|"md5","digest":...}],"r_version":"..."}
# The "sources" entries pin the REMIND code that was sourced (file name and digest: tools::sha256sum where R has it,
# R >= 4.5, else tools::md5sum), the plan's "pinned source".
# On an R error (unreadable config, a failing selectScenarios, ...) the line is
#   {"bridge":"select_scenarios","ok":false,"error":<conditionMessage>,"call":<deparsed call or null>}
# and the exit status is 1; nothing is written to --out in that case (no partial state). The R error text is what the
# in-process R code would have raised, so bridges.py can re-raise it as RParityError.
source(file.path(dirname(sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1])), "_json.R"))

opt <- bridge_args(c(out = "", config = "config/scenario_config.csv", startgroup = "AMT", scripts = "scripts/start",
                     suffix = ""))
if (!nzchar(opt[["out"]])) stop("select_scenarios.R: --out PATH is required")

# the body runs in a function, as startRuns() does: the `selectScenarios <- NA` binding is local and R's call lookup
# skips it in favour of the function source() put into the global environment (R/modeltests.R lines 134-136)
if (exists("sha256sum", envir = asNamespace("tools"))) {
  digest_name <- "sha256"; digest_fun <- get("sha256sum", envir = asNamespace("tools"))
} else {
  digest_name <- "md5"; digest_fun <- tools::md5sum
}
select_scenarios <- function(opt) {
  settings <- read.csv2(opt[["config"]], stringsAsFactors = FALSE, row.names = 1, comment.char = "#", na.strings = "")
  sourced <- list.files(opt[["scripts"]], pattern = "\\.R$", full.names = TRUE)
  invisible(sapply(sourced, source))
  selectScenarios <- NA # avoid buildLibrary to fail with "no visible global function" (R/modeltests.R line 135)
  runsToStart <- selectScenarios(settings = settings, interactive = FALSE, startgroup = opt[["startgroup"]])
  if (nzchar(opt[["suffix"]])) row.names(runsToStart) <- paste0(row.names(runsToStart), opt[["suffix"]])
  tmp <- file.path(dirname(opt[["out"]]), paste0(".", basename(opt[["out"]]), ".bridge-", Sys.getpid(), ".tmp"))
  saveRDS(runsToStart, file = tmp)
  if (!file.rename(tmp, opt[["out"]])) { unlink(tmp); stop("cannot rename ", tmp, " to ", opt[["out"]]) }
  list(bridge = json_scalar("select_scenarios"), ok = TRUE,
       row_names = as.character(row.names(runsToStart)), columns = as.character(names(runsToStart)),
       nrow = nrow(runsToStart), out = json_scalar(opt[["out"]]),
       sources = unname(lapply(sourced, function(f) list(file = json_scalar(f), algorithm = json_scalar(digest_name),
                                                          digest = json_scalar(unname(digest_fun(f)))))),
       r_version = json_scalar(R.version.string))
}
result <- tryCatch(select_scenarios(opt), error = function(e) {
  list(bridge = json_scalar("select_scenarios"), ok = FALSE, error = json_scalar(conditionMessage(e)),
       call = cond_call(e))
})
json_line(result)
if (!isTRUE(result$ok)) quit(save = "no", status = 1L)
