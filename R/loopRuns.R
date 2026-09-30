#' loopRuns
#'
#' Prints the run status table of [getRunStatus()] for a set of run folders,
#' one line per run, coloured by the state of the run.
#'
#' @param mydir a dir or vector of dirs
#' @param user the user whose runs will be shown
#' @param colors boolean whether the output is colored dependent on runstatus
#' @param sortbytime boolean whether the output is sorted by timestamp
#'
#' @author Anastasis Giannousakis
#' @import crayon
#' @importFrom lubridate make_difftime
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

  if (colors) cat(statusColorLegend(), "\n\n", sep = "")
  info <- file.info(mydir)
  info <- info[info[, "isdir"] == TRUE, ]
  mydir <- if (isTRUE(sortbytime)) rownames(info[order(info[, "mtime"], decreasing = TRUE), ]) else rownames(info)

  onCluster <- file.exists("/p")
  layout <- statusTableLayout(folderWidth(mydir), onCluster)
  cat(underline(paste(layout$titles, collapse = layout$colSep)), "\n")
  for (path in mydir) {
    status <- try(getRunStatus(path, user = user))
    if (!file.exists(paste0(path, "/", grep("^config.*|^log.txt$", dir(path), value = TRUE)[1])) &&
          status[["jobInSLURM"]] == "no") {
      cat(paste(path, "skipped.\n"))
      next
    }
    if (inherits(status, "try-error")) {
      cat(basename(path), "skipped because of error\n")
      next
    }
    cat(formatStatusLine(status, layout, colors))
  }
}

# The width of the folder column: the longest folder name, between 15 and 67 characters.
folderWidth <- function(dirs) {
  min(67, max(c(15, nchar(basename(normalizePath(dirs, mustWork = FALSE))))))
}

# Column titles and widths of the run status table.
statusTableLayout <- function(folderWidth, onCluster, colSep = "  ") {
  folder <- paste0("Folder", paste(rep(" ", folderWidth - 6), collapse = ""))
  if (onCluster) {
    titles <- c(folder, "Runtime    ", "inSlurm ", "RunType    ", "RunStatus        ", "Warnings ", "Iter            ",
                "Conv                 ", "modelstat            ", "Mif   ", "AppResults")
    lenCols <- c(nchar(titles)[-length(titles)], 3)
  } else {
    titles <- c(folder, "Runtime    ", "RunType    ", "RunStatus        ", "Warnings ", "Iter            ",
                "Conv                 ", "modelstat          ", "Mif   ")
    lenCols <- nchar(titles)
  }
  list(titles = titles, lenCols = lenCols, colSep = colSep)
}

statusColorLegend <- function() {
  red <- make_style("orangered")
  orange <- make_style("orange")
  paste0("# Color code: ", yellow("pending"), "/", yellow("startup"), ", ", cyan("running"), ", ",
         green("converged"), "/", green("finished"), ", ", orange("no mif"), ", ",
         magenta("conopt stalled?"), ", ", red("error"), ".")
}

# One line of the run status table: the formatted row in the colour of its state.
formatStatusLine <- function(status, layout, colors = TRUE) {
  status <- formatRuntime(status)
  status["RunType"] <- gsub("testOneRegi", "1Regi", status["RunType"])
  line <- trimws(printOutput(status, lenCols = layout$lenCols, colSep = layout$colSep), which = "right",
                 whitespace = " ")
  style <- if (colors) statusLineStyle(unlist(status), line) else identity
  style(line)
}

# Runtime is given in seconds: format it as a duration, prepend ">" while the run is
# still active, or show the SLURM state (pending / startup) instead.
formatRuntime <- function(status) {
  if (grepl("pending$", status[["jobInSLURM"]])) {
    status["Runtime"] <- "pending"
  } else if (!is.na(status[["Runtime"]])) {
    status["Runtime"] <- format(round(make_difftime(second = status[["Runtime"]]), 1))
    if (!status["jobInSLURM"] == "no") {
      status["Runtime"] <- paste0(">", if (nchar(status["Runtime"]) < 10) " ", status["Runtime"])
    }
  } else if (grepl("startup$", status[["jobInSLURM"]])) {
    status["Runtime"] <- "startup"
  } else {
    status["Runtime"] <- format(status["Runtime"])
  }
  status["jobInSLURM"] <- gsub(" *startup$| *pending$", "", status["jobInSLURM"])
  status
}

# The crayon style for a status line: yellow = pending/startup, cyan = running,
# green = converged/finished, orange = no mif, magenta = conopt stalled, red = error.
statusLineStyle <- function(status, line) {
  isMagpie <- grepl("^y[12]", status[["Iter"]]) || grepl("^nlp_", status[["RunType"]])
  if (isMagpie) magpieLineStyle(status, line) else remindLineStyle(status, line)
}

magpieLineStyle <- function(status, line) {
  red <- make_style("orangered")
  if (status[["Runtime"]] %in% "pending") return(yellow)
  if (grepl("not_converged|Execution erro|Compilation er|missing|interrupted|Abort", status[["RunStatus"]])) return(red)
  if (grepl("converged|Clb_converged", status[["RunStatus"]])) return(green)
  optimal <- (grepl("222", status[["modelstat"]]) && !grepl(".", status[["modelstat"]], fixed = TRUE)) ||
    status[["modelstat"]] == "2: Locally Optimal"
  if (optimal) return(green)
  if (grepl("conoptspy >", line, fixed = TRUE)) return(magenta)
  if (grepl("Run in progress", line)) return(cyan)
  if (all(grepl(" NA ", line) & grepl("FALSE", line))) return(red)
  identity
}

remindLineStyle <- function(status, line) {
  if (status[["Runtime"]] %in% c("pending", "startup")) return(yellow)
  if (grepl("conoptspy >", line, fixed = TRUE)) return(magenta)
  if (!status[["jobInSLURM"]] == "no" && !status[["jobInSLURM"]] == "NA") return(cyan)
  finishedRemindLineStyle(status)
}

finishedRemindLineStyle <- function(status) {
  red <- make_style("orangered")
  orange <- make_style("orange")
  converged <- c("converged", "Clb_converged")
  hasMif <- !status[["Mif"]] == "no"
  if (status[["Conv"]] == "converged (had INFES)" && hasMif) return(green)
  if (grepl("not_converged|Execution erro|Compilation er|interrupted|Intermed Infes", status[["RunStatus"]])) {
    return(red)
  }
  if (status[["Conv"]] %in% converged && hasMif) return(green)
  if (grepl("2: Locally Optimal", status[["modelstat"]]) && !grepl("nash", status[["RunType"]])) return(green)
  if (!hasMif && status[["Conv"]] %in% c(converged, "converged (had INFES)")) return(orange)
  if (status[["jobInSLURM"]] == "no") return(red)
  cyan
}
