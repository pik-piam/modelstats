#!/usr/bin/env Rscript
# add_to_data_changelog.R - modelstats bridge (modelstats.amt.bridges.add_to_data_changelog): R/modeltests.R lines
# 264-268 of evaluateRuns() as a subprocess, because magpie4::addToDataChangelog() is magpie4's code, not modelstats'.
#
#   Rscript add_to_data_changelog.R --report PATH --changelog PATH --version-id ID
#
# Run with the cwd evaluateRuns() has (the output directory; --report may be relative to it, as R's
# file.path(i, "report.rds") is). Exactly as R/modeltests.R does, inside try():
#   try(magpie4::addToDataChangelog(report = readRDS(report), changelog = changelog, versionId = id))
# try() prints R's own "Error in <call> : <message>" (and the "In addition: Warning message:" block) on stderr,
# as the in-process call does; the only difference to R/modeltests.R is the deparsed call, readRDS(report) here
# against readRDS(file.path(i, "report.rds")) there, because the report path is an explicit input of the bridge.
# Then ONE JSON line is printed as the last line of stdout (bridges.py parses the last non-empty line):
#   {"bridge":"add_to_data_changelog","ok":true,"changelog":PATH,"version_id":ID,"nrow":N,
#    "magpie4_version":"...","r_version":"..."}
# or, when the try() failed, {"bridge":...,"ok":false,"error":<conditionMessage>,"call":<deparsed call or null>,
# "magpie4_version":...} with exit status 1: the changelog file is then whatever addToDataChangelog left (it writes
# only at its end, so an error before that leaves the file untouched), exactly like R's try().
source(file.path(dirname(sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1])), "_json.R"))

opt <- bridge_args(c(report = "", changelog = "", `version-id` = ""))
for (k in names(opt)) if (!nzchar(opt[[k]])) stop("add_to_data_changelog.R: --", k, " is required")
report <- opt[["report"]]
changelog <- opt[["changelog"]]
versionId <- opt[["version-id"]]

res <- try(magpie4::addToDataChangelog(report = readRDS(report), changelog = changelog, versionId = versionId))

magpie4_version <- tryCatch(as.character(utils::packageVersion("magpie4")), error = function(e) NA_character_)
if (inherits(res, "try-error")) {
  cond <- attr(res, "condition")
  json_line(list(bridge = json_scalar("add_to_data_changelog"), ok = FALSE,
                 error = json_scalar(conditionMessage(cond)), call = cond_call(cond),
                 magpie4_version = json_scalar(magpie4_version), r_version = json_scalar(R.version.string)))
  quit(save = "no", status = 1L)
}
json_line(list(bridge = json_scalar("add_to_data_changelog"), ok = TRUE, changelog = json_scalar(changelog),
               version_id = json_scalar(versionId), nrow = nrow(res),
               magpie4_version = json_scalar(magpie4_version), r_version = json_scalar(R.version.string)))
