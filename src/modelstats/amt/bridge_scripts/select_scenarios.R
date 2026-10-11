# select_scenarios.R - the selectScenarios() bridge of modelstats (migration/03-migration-plan.md section 2.3).
#
# Run by modelstats.amt.bridges.select_scenarios() as
#     Rscript select_scenarios.R [STARTGROUP]
# with the working directory set to the REMIND checkout. It reproduces R/modeltests.R:127-136 of modelstats
# 0.31.0: read config/scenario_config.csv with the exact read.csv2() call, source every scripts/start/*.R so
# that REMIND's selectScenarios() is defined, call it with interactive = FALSE and the start group (default
# "AMT"), and print the selection as one JSON object on the last line of stdout:
#
#     {"schema": 1, "startgroup": "AMT", "names": [<row names>], "columns": {<column>: [<cells>], ...}}
#
# The row names are REMIND's scenario titles without the "-AMT" suffix (R/modeltests.R:137 pastes it; the
# Python side does the same). A cell is a string, a number, true/false or null (NA, NaN, Inf). Whatever the
# sourced scripts print before the payload is passed through (the Python side logs it). Any failure (missing
# file, duplicate titles, no selectScenarios() in scripts/start, an error inside it) is an R error: Rscript
# then exits with status 1 and the message on stderr, and the Python side fails the AMT step.
#
# No package is loaded: REMIND's checkout has its own renv library, which the sourced scripts may use, but this
# script must not depend on anything outside base R, so the JSON writer below is its own. The script writes
# no file.

jsonString <- function(x) {
  # The JSON string literal for one character value: backslash, double quote and the control characters
  # escaped (RFC 8259 section 7); everything else, UTF-8 included, is passed through.
  x <- enc2utf8(x)
  x <- gsub("\\", "\\\\", x, fixed = TRUE)
  x <- gsub("\"", "\\\"", x, fixed = TRUE)
  x <- gsub("\n", "\\n", x, fixed = TRUE)
  x <- gsub("\r", "\\r", x, fixed = TRUE)
  x <- gsub("\t", "\\t", x, fixed = TRUE)
  bytes <- utf8ToInt(x)
  if (anyNA(bytes)) {
    stop("a cell of config/scenario_config.csv is not valid UTF-8: ", x)
  }
  control <- bytes < 32
  if (any(control)) {
    pieces <- intToUtf8(bytes, multiple = TRUE)
    pieces[control] <- sprintf("\\u%04x", bytes[control])
    x <- paste(pieces, collapse = "")
  }
  paste0("\"", x, "\"")
}

jsonScalar <- function(x) {
  # One cell of a data.frame column as read.csv2() produces it: NA (and NaN, Inf) -> null, logical ->
  # true/false, numbers with 15 significant digits (as.character), anything else as a string.
  if (length(x) != 1 || is.na(x)) {
    return("null")
  }
  if (is.logical(x)) {
    return(if (x) "true" else "false")
  }
  if (is.numeric(x)) {
    if (!is.finite(x)) {
      return("null")
    }
    return(as.character(x))
  }
  jsonString(as.character(x))
}

jsonArray <- function(values) {
  paste0("[", paste(vapply(values, jsonScalar, character(1)), collapse = ", "), "]")
}

jsonColumns <- function(frame) {
  # {"column": [cells...], ...} in the column order of the data.frame; an empty selection keeps the columns.
  members <- vapply(names(frame), function(column) {
    paste0(jsonString(column), ": ", jsonArray(as.list(frame[[column]])))
  }, character(1))
  paste0("{", paste(members, collapse = ", "), "}")
}

startgroup <- commandArgs(trailingOnly = TRUE)
startgroup <- if (length(startgroup) >= 1) startgroup[[1]] else "AMT"

# R/modeltests.R:127-131
settings <- read.csv2("config/scenario_config.csv",
                      stringsAsFactors = FALSE,
                      row.names = 1,
                      comment.char = "#",
                      na.strings = "")

# R/modeltests.R:133-134: source everything from scripts/start so that all functions are available
invisible(sapply(list.files("scripts/start", pattern = "\\.R$", full.names = TRUE), source))
if (!exists("selectScenarios", mode = "function")) {
  stop("no selectScenarios() is defined by scripts/start/*.R of ", normalizePath("."))
}

# R/modeltests.R:136
runsToStart <- selectScenarios(settings = settings, interactive = FALSE, startgroup = startgroup)
if (!is.data.frame(runsToStart)) {
  stop("selectScenarios() returned a ", class(runsToStart)[[1]], ", not a data.frame")
}

# The payload: modelstats.amt.bridges.PAYLOAD_PREFIX is the start of this line.
cat("{\"schema\": 1, \"startgroup\": ", jsonString(startgroup),
    ", \"names\": ", jsonArray(as.list(row.names(runsToStart))),
    ", \"columns\": ", jsonColumns(runsToStart), "}\n", sep = "")
