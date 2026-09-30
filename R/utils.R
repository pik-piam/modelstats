# Small internal helpers shared by the run inspection and the AMT code.

#' Run a shell pipeline and return its standard output
#'
#' Thin wrapper around `system(intern = TRUE)` that silences the warning R emits
#' for a non-zero exit status (e.g. `grep` without a match) and never errors.
#'
#' @param command a shell command line (a pipeline is fine)
#' @return character vector of output lines, `character(0)` when there is none
#' @noRd
shellLines <- function(command) {
  out <- suppressWarnings(try(system(command, intern = TRUE), silent = TRUE))
  if (inherits(out, "try-error")) character(0) else out
}

#' Last line of a file matching a pattern (via `tac | grep -m 1`)
#'
#' @param file path to the file, quoted for the shell
#' @param pattern a grep pattern (basic regular expression)
#' @noRd
lastMatchingLine <- function(file, pattern) {
  shellLines(paste0("tac '", file, "' | grep -m 1 '", pattern, "'"))
}

#' Does the last (non-empty) line of a file match a pattern?
#'
#' @param file path to the file (passed to `tail` unquoted, as the legacy code did)
#' @param pattern a regular expression
#' @noRd
lastLineMatches <- function(file, pattern) {
  isTRUE(grepl(pattern, try(system(paste("tail -1", file), intern = TRUE), silent = TRUE)))
}

#' Load an `.rda`/`.Rdata` file into a fresh environment and return it as a list
#'
#' @param file path to the file, or a vector of paths (which `load()` rejects)
#' @noRd
loadRdata <- function(file) {
  env <- new.env()
  load(file, envir = env)
  as.list(env)
}

#' Format a time stamp for log lines
#' @noRd
timeStamp <- function(time = Sys.time()) {
  format(time, "%Y-%m-%d %H:%M:%S")
}
