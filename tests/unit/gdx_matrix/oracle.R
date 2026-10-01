#!/usr/bin/env Rscript
# oracle.R OUT.json FILE.gdx...
#
# The R oracle of tests/unit/test_gdx.py: reads the status symbols of every GDX file exactly as
# R/getRunStatus.R does (gdx2::readGDX with the same format/type/react arguments, lines 57, 84,
# 212-213, 280-288) and writes one JSON object per file. Numbers are written as R's
# as.character() text ("2", "10.5", "Inf", "-Inf", "NaN"); an R NA value is the string "NA"; an
# absent symbol (NULL in R) is null; an R error is {"error": <conditionMessage>}.
#
# gamstransfer aborts the whole R process on a file that is not a GDX file (BUG-037), so the
# caller runs such files in a process of their own.
suppressMessages(library(gdx2))

args <- commandArgs(TRUE)
out <- args[1]
files <- args[-1]

# as.character() text of numbers; an R NA value is the string "NA" so that it stays distinct
# from an absent symbol (NULL -> JSON null)
chr_num <- function(v) {
  out <- as.character(v)
  out[is.na(v) & !is.nan(v)] <- "NA"
  out
}
num <- function(x) if (is.null(x)) NULL else chr_num(as.numeric(x))
chr0 <- function(d) if (is.null(d)) character(0) else as.character(d)
tryval <- function(expr) {
  v <- try(expr, silent = TRUE)
  if (inherits(v, "try-error")) list(error = conditionMessage(attr(v, "condition"))) else v
}
param <- function(x, modelstat = FALSE) {
  if (is.null(x)) return(NULL)
  r <- list(
    dimnames = lapply(dimnames(x), chr0),
    dimnames_names = chr0(names(dimnames(x))),
    dim = dim(x),
    values = chr_num(c(x))
  )
  if (modelstat) r$modelstat_string <- tryval(paste(x[, , "modelstat"], collapse = ""))
  r
}

res <- list()
for (f in files) {
  r <- list()
  # getRunStatus.R:57 (GDX selection) and :280
  r$o_iterationNumber <- num(readGDX(f, "o_iterationNumber", format = "simplest", react = "silent"))
  # getRunStatus.R:281
  r$s80_bool <- num(suppressWarnings(readGDX(f, "s80_bool", type = "Parameter", format = "simplest")))
  # getRunStatus.R:212
  r$cm_abortOnConsecFail <- num(readGDX(f, "cm_abortOnConsecFail", format = "simplest", react = "silent"))
  for (n in c("sv_eps", "sv_inf", "sv_neginf", "sv_undef", "sv_na")) {
    v <- num(readGDX(f, n, format = "simplest", react = "silent"))
    if (!is.null(v)) r[[n]] <- v
  }
  # getRunStatus.R:84-86
  ff <- tryval(c(readGDX(gdx = f, c("o_modelstat", "p80_modelstat"), format = "first_found", react = "silent")))
  r$first_found <- if (is.list(ff) || is.null(ff)) ff else chr_num(ff)
  if (!is.list(ff)) r$modelstat_string <- gsub("0", ".", paste0(ff, collapse = ""))
  # getRunStatus.R:288-290
  r$p80_repy <- param(suppressWarnings(readGDX(gdx = f, "p80_repy")), modelstat = TRUE)
  # getRunStatus.R:213-215
  tc <- readGDX(gdx = f, "p80_trackConsecFail", react = "silent")
  r$p80_trackConsecFail <- param(tc)
  if (!is.null(tc)) {
    q <- quitte::as.quitte(tc)
    r$p80_trackConsecFail$quitte <- list(region = as.character(q$region), value = chr_num(q$value))
  }
  for (n in c("p80_modelstat", "p2")) {
    v <- param(readGDX(gdx = f, n, react = "silent"))
    if (!is.null(v)) r[[n]] <- v
  }
  res[[f]] <- r
}
# A small JSON writer (no jsonlite: the deps-in-desc pre-commit hook checks every .R file of the
# repository against DESCRIPTION). Same shape as jsonlite::toJSON(auto_unbox = TRUE, na = "null"):
# NULL -> null, a length-1 atomic -> scalar, other atomics -> arrays, lists -> arrays or objects.
json_str <- function(s) {
  if (is.na(s)) return("null")
  s <- gsub("\\\\", "\\\\\\\\", s)
  s <- gsub("\"", "\\\\\"", s)
  s <- gsub("\n", "\\\\n", s)
  s <- gsub("\t", "\\\\t", s)
  s <- gsub("\r", "\\\\r", s)
  paste0("\"", s, "\"")
}
to_json <- function(x) {
  if (is.null(x)) return("null")
  if (is.list(x)) {
    if (length(x) == 0) return(if (is.null(names(x))) "[]" else "{}")
    if (is.null(names(x))) return(paste0("[", paste(vapply(x, to_json, ""), collapse = ", "), "]"))
    keys <- names(x)
    keys[keys == ""] <- as.character(seq_along(keys))[keys == ""]
    keys <- vapply(keys, json_str, "", USE.NAMES = FALSE)
    return(paste0("{", paste(paste0(keys, ": ", vapply(x, to_json, "")), collapse = ", "), "}"))
  }
  items <- if (is.character(x)) vapply(x, json_str, "", USE.NAMES = FALSE) else ifelse(is.na(x), "null", as.character(x))
  if (length(items) == 1) return(items)
  paste0("[", paste(items, collapse = ", "), "]")
}
writeLines(to_json(res), out)
