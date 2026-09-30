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
#' * ProjSumErrReg Regional Summation Errors found when running `piamInterfaces::checkSummationsRegional`
#'   for ScenarioMIP
#'
#' @param dirs a vector of paths to REMIND runs.
#' When NULL, the latest AMTs are used (only works on PIK cluster)
#'
#' @author Falk Benke
#' @export
#' @md
getSanityChecks <- function(dirs = NULL) {
  if (is.null(dirs)) {
    cat("Results from", amtOutputDir, "\n")
    dirs <- dir(path = amtOutputDir, pattern = readRDS(amtRuncodeFile), full.names = TRUE)
  }
  layout <- sanityTableLayout(min(67, max(c(15, nchar(basename(normalizePath(dirs, mustWork = TRUE)))))))

  cat("\n")
  cat(cyan(paste0("For column explanations see: https://github.com/remindmodel/remind/blob/develop/tutorials/",
                  "05_AnalysingModelOutputs.md#7-visualizing-run-status-and-summation-checks-for-runs\n")))
  cat(underline(paste(layout$titles, collapse = "  ")), "\n")
  cat("\n")
  for (path in dirs) {
    cat(formatSanityLine(getRunStatus(path), layout))
  }
}

# The columns of getRunStatus() shown in the sanity table.
sanityColumns <- c("summationErrors", "rangeErrors", "fixErrors",
                   "missingProjVars", "projSummationErrors", "projSummationErrorsRegional")

sanityTableLayout <- function(folderWidth) {
  titles <- c(paste0("Folder", paste(rep(" ", folderWidth - 6), collapse = "")),
              "SumErr", "RangeErr", "FixingErr", "MissingVar", "ProjSumErr", "ProjSumErrReg")
  list(titles = titles, lenCols = nchar(titles))
}

# One line of the sanity table; empty for a run without sanity check results (e.g. no mif).
formatSanityLine <- function(status, layout) {
  if (!all(sanityColumns %in% names(status))) return("")
  printOutput(status, lenCols = layout$lenCols, cols = sanityColumns)
}
