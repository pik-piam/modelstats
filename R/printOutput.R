#' printOutput
#'
#' Formats one row of [getRunStatus()] as a fixed-width text line: the folder
#' name followed by the selected columns, each padded or truncated to its width.
#'
#' @param string one-row data.frame as returned by [getRunStatus()]
#' @param len1stcol length of first column (the folder name)
#' @param lenCols vector with length information for all columns, the first
#'   entry being the folder column (it overrides `len1stcol`). Without it the
#'   columns get the widths 13, 14, ... counted from the last column and are
#'   printed without separator.
#' @param colSep string separating the columns
#' @param cols optional vector of elements to retrieve from the passed list.
#' Default NULL corresponds to a predefined set of elements.
#' @return the formatted line including the trailing newline
#'
#' @author Anastasis Giannousakis, Falk Benke
#' @export
printOutput <- function(string, len1stcol = 67, lenCols = NULL, colSep = "   ", cols = NULL) {
  if (length(lenCols) > 0) len1stcol <- lenCols[1]
  if (length(string) == 0) return("")
  if (is.null(cols)) cols <- defaultStatusColumns(onCluster = file.exists("/p"))
  string <- string[, cols]

  n <- length(string)
  cells <- vapply(seq_len(n), function(k) {
    # without a width for this column: 13 for the last column, 14 for the one before, ...
    width <- if (n + 1 - k >= length(lenCols)) n + 13 - k else lenCols[k + 1]
    separator <- if (is.null(lenCols) || k == n) "" else colSep
    paste0(fixedWidth(unname(string)[[k]], width), separator)
  }, "")
  paste0(fixedWidth(rownames(string), len1stcol), colSep, paste0(cells, collapse = ""), "\n")
}

# Pad or truncate a value to `len` characters; NA becomes blanks.
fixedWidth <- function(x, len) {
  if (is.na(x)) return(paste0(rep(" ", len), collapse = ""))
  substr(paste0(c(x, rep(" ", len)), collapse = ""), 1, len)
}

# The columns of the run status table printed by rs (loopRuns / printOutput).
defaultStatusColumns <- function(onCluster) {
  if (onCluster) {
    c("Runtime", "jobInSLURM", "RunType", "RunStatus", "Warnings", "Iter", "Conv", "modelstat", "Mif",
      "runInAppResults")
  } else {
    c("Runtime", "RunType", "RunStatus", "Warnings", "Iter", "Conv", "modelstat", "Mif")
  }
}
