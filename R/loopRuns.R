#' loopRuns
#'
#' Returns the output for getRunStatus nicely
#'
#' @param mydir a dir or vector of dirs
#' @param user the user whose runs will be shown
#'
#' @author Anastasis Giannousakis
#' @import crayon
#' @export
#' @examples
#' \dontrun{
#'
#' loopRuns(dir())
#' }
#'
loopRuns <- function(mydir, user = NULL) {

  if (is.null(user)) user <- Sys.info()[["user"]]
  if (length(mydir) == 0) return("No runs found")
  if (mydir[[1]] == "exit") return(NULL)

  red <- make_style("orangered")

  a <- file.info(mydir)
  a <- a[a[, "isdir"] == TRUE, ]
  mydir <- rownames(a[order(a[, "mtime"], decreasing = TRUE), ])
  len <- max(c(15, nchar(basename(normalizePath(mydir, mustWork = FALSE)))))
  len <- min(67, len)

  colSep <- "  "
  if (file.exists("/p")) {
    coltitles <- c(paste0("Folder", paste(rep(" ", len - 6), collapse = "")),
      "Runtime    ", "inSlurm", "RunType    ", "RunStatus        ", "Iter            ",
      "Conv                 ", "modelstat          ", "Mif     ", "inAppResults")
    lenCols <- c(nchar(coltitles)[-length(coltitles)], 5)
  } else {
    coltitles <- c(paste0("Folder", paste(rep(" ", len - 6), collapse = "")),
      "Runtime    ", "RunType    ", "RunStatus        ", "Mif     ",
      "Conv                 ", "Iter            ", "modelstat          ")
    lenCols <- nchar(coltitles)
  }
  message(paste(coltitles, collapse = colSep))

  for (i in mydir) {

    if (!file.exists(paste0(i, "/", grep("^config.*", dir(i), value = TRUE)[1]))) next # do not report on folders that do not contain runs
    try(out <- printOutput(getRunStatus(i, user = user), lenCols = lenCols, colSep = colSep))
    if (grepl(" y2| nlp_", out)) {
      if (grepl("not_converged|Execution erro|Compilation er|missing|interrupted", out)) {
        cat(red(out))
      } else if (grepl(" converged|Clb_converged", out)) {
        cat(underline(green(out)))
      } else if (grepl("222", out)) {
        cat(green(out))
      } else if (grepl("Run in progress", out)) {
        cat(cyan(out))
      } else if (all(grepl(" NA ", out) & grepl("FALSE", out))) {
        cat(red(out))
      } else {
        cat(cyan(out))
      }
    } else {
      if (grepl("Run in progress", out)) {
        cat(cyan(out))
      } else if (grepl("  PD ", out)) {
        cat(yellow(out))
      } else if (grepl("not_converged|Execution erro|Compilation er|missing|interrupted|Intermed Infes", out)) {
        cat(red(out))
      } else if (grepl(" converged|Clb_converged", out)) {
        cat(underline(green(out)))
      } else if (all(grepl("testOneRegi", out) & grepl("2: Locally Optimal", out))) {
        cat(green(out))
      } else if (grepl("2: Locally Optimal", out) && !grepl("nash ", out)) {
        cat(green(out))
      } else if (all(grepl(" NA ", out) & grepl("FALSE", out))) {
        cat(red(out))
      } else {
        cat(cyan(out))
      }
    }

  }

}
