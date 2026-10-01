# _json.R - minimal JSON writer for the modelstats bridge scripts (sourced by select_scenarios.R and
# add_to_data_changelog.R). Base R only: the bridges must not depend on jsonlite being installed next to the
# model's R library. Strings are escaped per RFC 8259 (backslash, quote, control characters); NULL -> null;
# character vectors -> arrays of strings; unnamed lists -> arrays; named lists -> objects; logical -> true/false;
# a single number -> its 15-significant-digit text.
json_str <- function(x) {
  if (length(x) == 0 || is.na(x)) return("null")
  x <- enc2utf8(as.character(x))
  x <- gsub("\\", "\\\\", x, fixed = TRUE)
  x <- gsub("\"", "\\\"", x, fixed = TRUE)
  x <- gsub("\n", "\\n", x, fixed = TRUE)
  x <- gsub("\r", "\\r", x, fixed = TRUE)
  x <- gsub("\t", "\\t", x, fixed = TRUE)
  for (i in c(1:8, 11, 12, 14:31)) x <- gsub(intToUtf8(i), sprintf("\\u%04x", i), x, fixed = TRUE)
  paste0("\"", x, "\"")
}
json_value <- function(x) {
  if (is.null(x)) return("null")
  if (is.list(x)) {
    if (!is.null(names(x))) {
      return(paste0("{", paste0(vapply(names(x), json_str, ""), ":", vapply(x, json_value, ""), collapse = ","), "}"))
    }
    return(paste0("[", paste(vapply(x, json_value, ""), collapse = ","), "]"))
  }
  if (is.logical(x) && length(x) == 1) return(if (is.na(x)) "null" else if (x) "true" else "false")
  if (is.numeric(x) && length(x) == 1) return(if (is.na(x)) "null" else format(x, digits = 15))
  if (is.character(x) && length(x) == 1 && !is.null(attr(x, "json_scalar"))) return(json_str(x))
  # character vectors are arrays (a scalar string is marked with attr json_scalar by json_scalar())
  paste0("[", paste(vapply(as.character(x), json_str, ""), collapse = ","), "]")
}
json_scalar <- function(x) structure(as.character(x)[1], json_scalar = TRUE)
# print one JSON object as the last line of stdout (the bridge protocol: bridges.py parses the last non-empty line)
json_line <- function(obj) cat(json_value(obj), "\n", sep = "")
# the deparsed call of a condition, or NULL
cond_call <- function(cond) {
  cl <- conditionCall(cond)
  if (is.null(cl)) NULL else json_scalar(paste(deparse(cl, width.cutoff = 500L), collapse = " "))
}
# --key value argument parsing: a named character vector of the given defaults overridden by the command line
bridge_args <- function(defaults) {
  args <- commandArgs(trailingOnly = TRUE)
  out <- defaults
  i <- 1L
  while (i <= length(args)) {
    key <- args[[i]]
    if (!startsWith(key, "--") || i == length(args)) stop("bridge: unexpected argument '", key, "'")
    name <- substring(key, 3L)
    if (!name %in% names(defaults)) stop("bridge: unknown option '", key, "'")
    out[[name]] <- args[[i + 1L]]
    i <- i + 2L
  }
  out
}
