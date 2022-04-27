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
  templ <- data.frame("modelstat" = "NA",   "Iter" = "NA",      "Conv" = "NA", "Mif" = "NA", "RunStatus" = "NA", "RunType" = "NA", stringsAsFactors = FALSE)
  if (file.exists("/p")) templ <- data.frame("runInAppResults" = "NA", "Mif" = "NA", "modelstat" = "NA",   "Conv" = "NA",   "Iter" = "NA", "RunStatus" = "NA", "RunType" = "NA", "jobInSLURM" = "NA", stringsAsFactors = FALSE)
  templ[, names(string)] <- string
  rownames(templ) <- rownames(string)
  string <- templ
  for (i in 1:length(string)) {
    out <- paste0(formatstr(unname(string)[[i]],
           ifelse(i >= length(lenCols), i + 12, lenCols[length(string) + 2 - i])), colSep, out)
  }
  return(paste0(formatstr(rownames(string), len1stcol), colSep, out, "\n"))
#  if(is.data.frame(string)) print(unname(string))
}
