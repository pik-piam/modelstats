#' getSanityChecks
#'
#' Get overview of sanity check results for a list of REMIND runs
#' @details
#' Currently the following columns are printed:
#' * SumErr Summation Errors found when running `remind2::convGDX2MIF`
#' * RangeErr Range Errors found when running `remind2::convGDX2MIF`
#' * FixingErr Fixing Errors found when running `piamInterfaces::fixOnRef]
#' * MissingVar Missing Variables found when running `piamInterfaces::checkMissingVars`
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
    dirs <- dir(path = amtPath, pattern = amtPattern, full.names = TRUE)
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
  cat(underline(paste(coltitles, collapse = colSep)), "\n")

  for (i in dirs) {
    status <- getRunStatus(i)
    if (all(cols %in% names(status))) {
      cat(printOutput(status, lenCols = lenCols, cols = cols))
    }
  }
}
