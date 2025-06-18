#' getSanityChecks
#'
#' Get overview of sanity check results for a list of REMIND runs
#' @details
#' Currently the following columns are printed:
#' * SumErr Summation Errors found when running `remind2::convGDX2MIF`
#' * RangeErr Range Errors found when running `remind2::convGDX2MIF`
#' * FixingErr Fixing Errors found when running `piamInterfaces::fixOnRef`
#' * MissingVar Missing Variables found when running `piamInterfaces::checkMissingVars` for ScenarioMIP
#' * ProjSumErr Summation Errors found when running `piamInterfaces::checkSummations` for ScenarioMIP
#' * ProjSumErrReg Regional Summation Errors found when running `piamInterfaces::checkSummationsRegional` for ScenarioMIP
#'
#' @param dirs a vector of paths to REMIND runs.
#' When NULL, the latest AMTs are used (only works on PIK cluster)
#'
#' @author Falk Benke
#' @export
#' @md
getSanityChecks <- function(dirs = NULL) {

  if (is.null(dirs)) {
    amtPath <- "/p/projects/remind/modeltests/remind/output/"
    cat("Results from", amtPath, "\n")
    amtPattern <- readRDS("/p/projects/remind/modeltests/remind/runcode.rds")
    chosendirs <- dir(path = amtPath, pattern = amtPattern, full.names = TRUE)
  } else {
    dirs <- list.dirs(dirs, recursive = FALSE)
    chosendirs <- gms::chooseFromList(dirs, type = "folders")
  }

  colSep <- "  "
  len <- max(c(15, nchar(basename(normalizePath(dirs, mustWork = TRUE)))))
  len <- min(67, len)

  coltitles <- c(
    paste0("Folder", paste(rep(" ", len - 6), collapse = "")),
    "SumErr", "RangeErr", "FixingErr", "MissingVar",
    "ProjSumErr", "ProjSumErrReg"
  )

  lenCols <- nchar(coltitles)

  cols <- c(
    "summationErrors", "rangeErrors", "fixErrors",
    "missingProjVars", "projSummationErrors", "projSummationErrorsRegional"
  )

  cat("\n")
  cat("For column explanations see: https://github.com/remind/remind/blob/develop/tutorials/05_AnalysingModelOutputs.md#7-visualizing-run-status-und-summation-checks-for-runs\n")
  cat(underline(paste(coltitles, collapse = colSep)), "\n")

  cat("\n")

  for (i in dirs) {
    status <- getRunStatus(i)
    if (all(cols %in% names(status))) {
      cat(printOutput(status, lenCols = lenCols, cols = cols))
    }
  }
}
