#' getRunStatus
#'
#' Returns the current status of a run or a vector of runs as a table with one
#' row per run. This is the tabular view of [modelRun()]; see there for the
#' meaning of the columns.
#'
#' @param mydir Path to the folder(s) where the run(s) is(are) performed
#' @param sort how to sort (nf=newest first)
#' @param user the user whose runs will be shown
#' @param detailed (boolean, default: TRUE). If FALSE, avoids costly information gathering and returns less info
#' @return a data.frame with the runs as rows (named by the folder name) and the
#'   columns `jobInSLURM`, `RunType`, `modelstat`, `runInAppResults` (cluster only),
#'   `Mif`, `Iter`, `RunStatus` and, with `detailed = TRUE`, `Warnings`, `Conv`,
#'   `Runtime` and the sanity check counts of REMIND runs with a mif
#'
#' @author Anastasis Giannousakis
#' @seealso [modelRun()]
#' @examples
#' \dontrun{
#'
#' a <- getRunStatus(dir())
#' }
#'
#' @export
getRunStatus <- function(mydir = dir(), sort = "nf", user = NULL, detailed = TRUE) {
  if (is.null(user)) user <- Sys.info()[["user"]]
  mydir <- normalizePath(mydir)
  onCluster <- file.exists("/p")

  info <- file.info(mydir)
  info <- info[info[, "isdir"] == TRUE, ]
  if (sort == "nf") mydir <- rownames(info[order(info[, "mtime"], decreasing = TRUE), ])

  out <- data.frame()
  for (path in mydir) {
    out <- addRunStatusRow(out, modelRun(path, user = user, detailed = detailed, onCluster = onCluster))
  }
  out
}
