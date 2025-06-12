#' printOutput
#'
#' Returns the output for getRunStatus nicely
#'
#' @param string String to print
#' @param len1stcol length of first column
#' @param lenCols vector with length information for all columns
#' @param colSep string separating the columns
#'
#' @author Anastasis Giannousakis
#' @export
printOutput <- function(string, len1stcol = 67, lenCols = NULL, colSep = "   ") {

  if (length(lenCols) > 0) len1stcol <- lenCols[1]

  formatstr <- function(x, len) {
    if (is.na(x)) return(paste0(rep(" ", len), collapse = ""))
    else return(substr(paste0(c(x, rep(" ", len)), collapse = ""), 1, len))
  }

  if (length(string) == 0) return("")

  out <- ""

  if (file.exists("/p")) {
    cols <- rev(c("Runtime", "jobInSLURM", "RunType", "RunStatus", "Iter", "Conv",
                  "modelstat", "Mif", "runInAppResults"))
  } else {
    cols <- rev(c("Runtime", "RunType", "RunStatus", "Iter", "Conv", "modelstat", "Mif"))
  }

  string <- string[, cols]

  for (i in seq_along(string)) {
    out <- paste0(formatstr(unname(string)[[i]], ifelse(i >= length(lenCols), i + 12, lenCols[length(string) + 2 - i])),
                  ifelse(is.null(lenCols) || i == 1, "", colSep), out)
  }
  out <- paste0(formatstr(rownames(string), len1stcol), colSep, out, "\n")

  return(out)
}
