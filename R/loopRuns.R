#' loopRuns
#'
#' Returns the output for getRunStatus nicely
#'
#' @param mydir a dir or vector of dirs
#' @param user the user whose runs will be shown
#' @param colors boolean whether the output is colored dependent on runstatus
#' @param sortbytime boolean whether the output is sorted by timestamp
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
loopRuns <- function(mydir, user = NULL, colors = TRUE, sortbytime = TRUE) {

  if (is.null(user)) user <- Sys.info()[["user"]]
  if (length(mydir) == 0) return("No runs found")
  if (mydir[[1]] == "exit") return(NULL)

  red <- make_style("orangered")
  if (colors) {
    cat("# Color code: ", yellow("pending"), "/", yellow("startup"), ", ", cyan("running"), ", ",
        underline(green("converged")), ", ", blue("converged (had INFES)"), ", ",
        green("finished"), ", ", red("error"), ".\n\n", sep = "")
  }
  if (isTRUE(sortbytime)) {
    a <- file.info(mydir)
    a <- a[a[, "isdir"] == TRUE, ]
    mydir <- rownames(a[order(a[, "mtime"], decreasing = TRUE), ])
  }
  len <- max(c(15, nchar(basename(normalizePath(mydir, mustWork = FALSE)))))
  len <- min(67, len)

  colSep <- "  "
  if (file.exists("/p")) {
    coltitles <- c(paste0("Folder", paste(rep(" ", len - 6), collapse = "")),
      "Runtime    ", "inSlurm ", "RunType    ", "RunStatus        ", "Iter            ",
      "Conv                 ", "modelstat            ", "Mif", "AppResults")
    lenCols <- c(nchar(coltitles)[-length(coltitles)], 3)
  } else {
    coltitles <- c(paste0("Folder", paste(rep(" ", len - 6), collapse = "")),
      "Runtime    ", "RunType    ", "RunStatus        ", "Mif",
      "Conv                 ", "Iter            ", "modelstat          ")
    lenCols <- nchar(coltitles)
  }
  cat(underline(paste(coltitles, collapse = colSep)), "\n")
  for (i in mydir) {
    status <- try(getRunStatus(i, user = user))
    if (! file.exists(paste0(i, "/", grep("^config.*", dir(i), value = TRUE)[1])) && status[["jobInSLURM"]] == "no") next
    if (inherits(status, "try-error")) {
      cat(basename(i), "skipped because of error\n")
      next
    }
    out <- trimws(printOutput(status, lenCols = lenCols, colSep = colSep), which = "right", whitespace = " ")
    status <- unlist(status)
    if (grepl("^y[12]", status[["Iter"]]) || grepl("^nlp_", status[["RunType"]])) {
      if (isFALSE(colors)) {
        cat(out)
      } else if (status[["Runtime"]] %in% "pending") {
        cat(yellow(out))
      } else if (grepl("not_converged|Execution erro|Compilation er|missing|interrupted|Abort", status[["RunStatus"]])) {
        cat(red(out))
      } else if (grepl("converged|Clb_converged", status[["RunStatus"]])) {
        cat(underline(green(out)))
      } else if ((grepl("222", status[["modelstat"]]) && ! grepl(".", status[["modelstat"]], fixed = TRUE))
                 || status[["modelstat"]] == "2: Locally Optimal") {
        cat(green(out))
      } else if (grepl("Run in progress", out)) {
        cat(cyan(out))
      } else if (all(grepl(" NA ", out) & grepl("FALSE", out))) {
        cat(red(out))
      } else {
        cat(out)
      }
    } else {
      if (isFALSE(colors)) {
        cat(out)
      } else if (status[["Runtime"]] %in% c("pending", "startup")) {
        cat(yellow(out))
      } else if (! status[["jobInSLURM"]] == "no") {
        cat(cyan(out))
      } else if (status[["Conv"]] == "converged (had INFES)") {
        cat(blue(out))
      } else if (grepl("not_converged|Execution erro|Compilation er|interrupted|Intermed Infes", status[["RunStatus"]])) {
        cat(red(out))
      } else if (status[["Conv"]] %in% c("converged", "Clb_converged")) {
        cat(underline(green(out)))
      } else if (grepl("2: Locally Optimal", status[["modelstat"]]) && ! grepl("nash", status[["RunType"]])) {
        cat(green(out))
      } else if (status[["jobInSLURM"]] == "no") {
        cat(red(out))
      } else {
        cat(cyan(out))
      }
    }

  }

}
