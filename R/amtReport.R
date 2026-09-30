# Report of the automated model tests (AMT): the README for the testing_suite
# repository and the Mattermost message. Pure functions of the evaluated runs.

amtRunNamePattern <- "_[0-9]{4}-[0-9]{2}-[0-9]{2}_[0-9]{2}\\.[0-9]{2}\\.[0-9]{2}$"
amtConverged <- c("converged", "converged (had INFES)")

# The problems a run reveals (each string is a line of the summary).
runProblems <- function(run) {
  problems <- character(0)
  isSpecial <- grepl("Calib_nash|testOneRegi", run$runType)
  if (!isSpecial && !run$convergence %in% amtConverged) problems <- c(problems, "Some run(s) did not converge")
  if (run$runType == "Calib_nash" && run$convergence != "Clb_converged") {
    problems <- c(problems, "Some run(s) did not converge")
  }
  if (grepl("testOneRegi", run$runType) && run$modelstat != "2: Locally Optimal") {
    problems <- c(problems, "testOneRegi does not return an optimal solution")
  }
  if (run$mif == "sumErr") problems <- c(problems, "Summation checks for some run(s) revealed some gaps")
  if (!identical(run$inAppResults, "yes")) problems <- c(problems, "Some run(s) did not report correctly")
  problems
}

# The planned scenarios that have no run folder.
scenariosNotStarted <- function(planned, runNames) {
  setdiff(c("default-AMT", planned), sub(amtRunNamePattern, "", runNames))
}

amtSummary <- function(problems) {
  if (length(problems) == 0) {
    "Summary: AMT runs look good."
  } else {
    paste0("Summary: ", paste(unique(problems), collapse = ". "))
  }
}

# The status table of the runs as printed by `rs -A` (without colours).
statusTableText <- function(runs, onCluster = file.exists("/p")) {
  withr::local_options(crayon.enabled = FALSE)
  layout <- statusTableLayout(folderWidth(vapply(runs, `[[`, "", "path")), onCluster)
  lines <- vapply(runs, function(run) formatStatusLine(as.data.frame(run), layout, colors = FALSE), "")
  c(paste(underline(paste(layout$titles, collapse = layout$colSep)), ""), sub("\n$", "", lines))
}

# The sanity check table of the runs as printed by `rs -s`.
sanityTableText <- function(runs) {
  withr::local_options(crayon.enabled = FALSE)
  layout <- sanityTableLayout(folderWidth(vapply(runs, `[[`, "", "path")))
  lines <- vapply(runs, function(run) formatSanityLine(as.data.frame(run), layout), "")
  explanation <- paste0("For column explanations see: https://github.com/remindmodel/remind/blob/develop/tutorials/",
                        "05_AnalysingModelOutputs.md#7-visualizing-run-status-and-summation-checks-for-runs")
  header <- paste(underline(paste(layout$titles, collapse = "  ")), "")
  c("", explanation, header, "", sub("\n$", "", lines[nzchar(lines)]))
}

# The run table of the README (jobInSLURM is left blank, the runs are finished).
readmeTable <- function(runs, onCluster = file.exists("/p")) {
  titles <- c("Run                                           ", "Runtime    ", "", "RunType    ", "RunStatus         ",
              "Warnings ", "Iter            ", "Conv                 ", "modelstat          ", "Mif   ", "AppResults")
  lenCols <- c(nchar(titles)[-length(titles)], 3)
  if (!onCluster) {
    keep <- !titles %in% c("", "AppResults")
    titles <- titles[keep]
    lenCols <- lenCols[keep]
  }
  rows <- vapply(runs, function(run) {
    status <- as.data.frame(run)
    if ("Runtime" %in% names(status) && is.numeric(status[["Runtime"]])) {
      status["Runtime"] <- format(round(make_difftime(second = status[["Runtime"]]), 1))
    }
    sub("\n$", "", printOutput(status, lenCols = lenCols, colSep = "  ", cols = defaultStatusColumns(onCluster)))
  }, "")
  c(paste(titles, collapse = "  "), rows)
}

# The README.md committed to the testing_suite repository.
buildReadme <- function(runs, evaluation, cycle, paths, compScen) {
  today <- format(Sys.time(), "%Y-%m-%d")
  c("```",
    paste0("This is the result of the automated model tests for REMIND on ", today, "."),
    paste0("Path to runs: ", paths$output, "/"),
    paste("Direct and interactive access to plots: open shinyResults::appResults,",
          "then use 'AMT' as keyword in the title search"),
    if (compScen) {
      paste0("Each run folder below should contain a compareScenarios PDF comparing the output of the ",
             "current and the last successful tests (comp_with_RUN-DATE.pdf)")
    },
    "Note: 'Mif' = 'no' indicates a possible error in output generation, please check!",
    paste("If you are currently viewing the email: Overview of the last test is in red,",
          "and of the current test in green"),
    evaluation$gitInfo,
    readmeTable(runs),
    if (length(evaluation$notStarted) > 0) c(" ", "These scenarios did not start at all:", evaluation$notStarted, " "),
    evaluation$summary,
    "```")
}

# The Mattermost message: summary, test results, git info and the rs tables.
buildMattermostMessage <- function(runs, evaluation, cycle, paths) {
  today <- format(Sys.time(), "%Y-%m-%d")
  intro <- paste0("Please find below the status of the REMIND automated model tests (AMT) of ", today,
                  ". Runs are here: `", paths$model, "`.")
  message <- c(intro, evaluation$summary, evaluation$testFullResult,
               "```", evaluation$gitInfo, "```",
               "`rs -A` returns:",
               "```", statusTableText(runs), "```",
               "Sanity checks (`rs -s`) return:",
               "```", sanityTableText(runs), "```")
  if (length(evaluation$notStarted) > 0) {
    message <- c(message, "These scenarios did not start at all:", evaluation$notStarted)
  }
  message <- c(message, paste0("Log: ", file.path(paths$root, cycle$log)))
  paste0(message, collapse = "\n")
}

# Post a message to a Mattermost incoming webhook. The payload and the URL are passed
# through a curl config file, so that neither shows up on the command line.
sendMattermostMessage <- function(message, token) {
  payload <- tempfile("mattermost-", fileext = ".json")
  writeLines(jsonlite::toJSON(list(text = message), auto_unbox = TRUE), payload)
  config <- tempfile("curl-", fileext = ".cfg")
  writeLines(c(paste0("url = ", jsonlite::toJSON(token, auto_unbox = TRUE)),
               "request = POST",
               "header = \"Content-Type: application/json\"",
               paste0("data-binary = \"@", payload, "\"")), config)
  withr::defer(unlink(c(payload, config)))
  result <- runCommand("curl", c("--silent", "--show-error", "--include", "--config", shQuote(config)))
  if (result$status != 0) amtLog("WARNING: sending the Mattermost message failed (exit status ", result$status, ")")
  invisible(result$status == 0)
}
