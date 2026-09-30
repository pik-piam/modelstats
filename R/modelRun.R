#' Inspect a model run folder
#'
#' Collects everything `modelstats` knows about one REMIND or MAgPIE run folder
#' (configuration, GAMS status, convergence, reporting, sanity checks, SLURM job)
#' into a single object that can be used from other packages and scripts.
#' [getRunStatus()] and the `rs` command line tool are built on top of it and
#' expose the same values in a table.
#'
#' The values are read from the files GAMS and the model scripts leave in the run
#' folder (`config.Rdata`/`config.yml`, `fulldata.gdx`, `non_optimal.gdx`,
#' `full.log`, `full.gms`, `log.txt`, `runstatistics.rda`, the mif and the
#' summation/range check files) and, on the cluster, from `squeue`.
#'
#' @details The fields of the returned object:
#' * `path`, `name`: the normalized folder and its basename
#' * `model`: `"REMIND"`, `"MAgPIE"` or `NA` (from `runstatistics.rda`)
#' * `config`: the run configuration (`cfg`) or `NULL`
#' * `status`: coarse status, one of `"pending"`, `"running"`, `"completed"`,
#'   `"error"`, `"unknown"` (derived from `runStatus` and `jobInSlurm`)
#' * `jobInSlurm`: SLURM state as reported by [foundInSlurm()], `"NA"` off the cluster
#' * `runType`: as reported by [colRunType()], e.g. `"nash"`, `"Calib_nash"`, `"testOneRegi EUR"`
#' * `modelstat`: GAMS model status, e.g. `"2: Locally Optimal"` (MAgPIE: one digit per year)
#' * `inAppResults` (cluster only): `"yes"` if the run was uploaded to `shinyResults::appResults`
#' * `mif`: `"yes"`, `"no"` (no report written), `"sumErr"` (report with summation errors)
#' * `iterations`: Nash iterations as `"<current>/<max>"`, with `" Clb: <n>"` for calibration runs
#' * `runStatus`: detailed status, e.g. `"Normal completion"`, `"Execution error"`,
#'   `"Run in progress"`, `"Timeout interrupt"`, `"full.log missing"`
#' * with `detailed = TRUE` also `warnings` (number of R warnings during the run),
#'   `convergence` (`"converged"`, `"converged (had INFES)"`, `"not_converged"`,
#'   `"Clb_converged"` or the regional modelstat string), `runtime` (GAMS run time in
#'   seconds, or the time since the run was prepared while it is still active) and,
#'   for REMIND runs with a mif, the sanity check counts `summationErrors`,
#'   `rangeErrors`, `fixErrors`, `missingProjVars`, `projSummationErrors`,
#'   `projSummationErrorsRegional`
#'
#' Fields that were not computed (brief mode, MAgPIE runs, no mif) are absent
#' from the object; a computed but unavailable value is `NA` or the string `"NA"`,
#' exactly as in the `rs` table.
#'
#' @param path path to the run folder
#' @param user the user whose SLURM jobs are inspected, default: the current user
#' @param detailed if `FALSE`, skip the costly checks (warnings, convergence,
#'   runtime, sanity checks)
#' @param onCluster whether the PIK cluster specific checks (SLURM, appResults) are done
#' @return an object of class `modelRun` (a named list, see details)
#' @author Anastasis Giannousakis, Tobias Diez
#' @examples
#' \dontrun{
#' run <- modelRun("/p/projects/remind/modeltests/remind/output/SSP2-NPi-AMT_2026-09-28_10.30.27")
#' run$status
#' run$convergence
#' as.data.frame(run)
#' }
#' @importFrom gdx2 readGDX
#' @importFrom utils head tail
#' @importFrom gms loadConfig
#' @importFrom piamutils niceround
#' @export
modelRun <- function(path, user = NULL, detailed = TRUE, onCluster = file.exists("/p")) {
  if (is.null(user)) user <- Sys.info()[["user"]]
  path <- normalizePath(path, mustWork = FALSE)
  files <- runFiles(path)
  run <- list(path = path, name = basename(path))
  run <- setField(run, "jobInSlurm", if (onCluster) foundInSlurm(path, user) else "NA")

  latestGdx <- latestGdxFile(files)
  run["config"] <- list(readRunConfig(path, files$configName))
  run <- setField(run, "runType", if (is.null(files$configName)) "NA" else colRunType(path))
  stats <- readRunStatistics(files$runstatistics)
  run$model <- runModelName(stats)

  run <- setField(run, "modelstat", readModelstat(latestGdx, stats))
  if (onCluster) run <- setField(run, "inAppResults", inAppResults(stats))
  run <- setField(run, "mif", mifStatus(path, files, run$config, stats))

  iterationMax <- maxIterations(run$config, run$runType, files$fullGms)
  progress <- readRunProgress(path, files, run, latestGdx, iterationMax, onCluster)
  run <- setField(run, "iterations", progress$iterations)
  run <- setField(run, "runStatus", progress$runStatus)

  if (detailed) {
    run <- setField(run, "warnings", readWarnings(files, stats))
    run <- setField(run, "convergence", readConvergence(files, run$config, run$runType, latestGdx, iterationMax))
    calib <- calibrationInfo(path, files, run)
    run <- setField(run, "iterations", calib$iterations)
    run <- setField(run, "convergence", calib$convergence)
    run <- setField(run, "runtime", readRuntime(stats, run$jobInSlurm))
    checks <- sanityChecks(path, files, run$config, stats)
    for (check in names(checks)) run <- setField(run, check, checks[[check]])
  }

  run$status <- coarseStatus(run$runStatus, run$jobInSlurm, onCluster)
  class(run) <- "modelRun"
  run
}

# Store one value in the run. The values are the cells of the getRunStatus() table,
# so a value that does not fit into one cell fails as the table assignment did.
setField <- function(run, name, value) {
  if (length(value) == 0) stop("replacement has length zero")
  if (length(value) > 1) stop("replacement has ", length(value), " rows, data has 1")
  run[[name]] <- value
  run
}

#' @export
print.modelRun <- function(x, ...) {
  cat("Model run", x$name, "\n")
  cat("  path:", x$path, "\n")
  fields <- setdiff(names(x), c("path", "name", "config"))
  for (field in fields) {
    value <- x[[field]]
    if (is.null(value)) value <- "NULL"
    cat(sprintf("  %-28s %s\n", paste0(field, ":"), paste(format(value), collapse = " ")))
  }
  invisible(x)
}

#' Convert a run into the one-row table of [getRunStatus()]
#'
#' @param x a `modelRun` object
#' @param ... ignored
#' @return a data.frame with one row named after the run folder and the columns
#'   `jobInSLURM`, `RunType`, `modelstat`, `runInAppResults`, `Mif`, `Iter`,
#'   `RunStatus`, `Warnings`, `Conv`, `Runtime` and the sanity check counts (the
#'   last ones only when they were computed)
#' @export
as.data.frame.modelRun <- function(x, ...) {
  addRunStatusRow(data.frame(), x)
}

# The columns of getRunStatus() and the run fields they come from, in the order in
# which they are assigned (which is the column order of the table).
runStatusColumns <- c(
  jobInSLURM = "jobInSlurm", RunType = "runType", modelstat = "modelstat",
  runInAppResults = "inAppResults", Mif = "mif", Iter = "iterations", RunStatus = "runStatus",
  Warnings = "warnings", Conv = "convergence", Runtime = "runtime",
  summationErrors = "summationErrors", rangeErrors = "rangeErrors", fixErrors = "fixErrors",
  missingProjVars = "missingProjVars", projSummationErrors = "projSummationErrors",
  projSummationErrorsRegional = "projSummationErrorsRegional"
)

# Append (or, for a run of the same name, overwrite) the row of a run in a status table.
addRunStatusRow <- function(table, run) {
  for (column in names(runStatusColumns)) {
    field <- runStatusColumns[[column]]
    if (field %in% names(run)) table[run$name, column] <- run[[field]]
  }
  table
}

# ---------------------------------------------------------------------------
# Files of a run folder

runFiles <- function(path) {
  configName <- grep("config.Rdata|config.yml", dir(path), value = TRUE)
  logTxt <- file.path(path, "log.txt")
  logMagTxt <- file.path(path, "log-mag.txt")
  list(
    configName = if (length(configName) == 0) NULL else configName,
    runstatistics = file.path(path, "runstatistics.rda"),
    gdx = file.path(path, "fulldata.gdx"),
    gdxNonOptimal = file.path(path, "non_optimal.gdx"),
    abortGdx = file.path(path, "abort.gdx"),
    fullGms = file.path(path, "full.gms"),
    fullLog = file.path(path, "full.log"),
    slurmLog = file.path(path, "slurm.log"),
    logTxt = logTxt,
    logMagTxt = if (file.exists(logMagTxt)) logMagTxt else logTxt
  )
}

# The gdx to read results from: fulldata.gdx or non_optimal.gdx, whichever has the
# higher iteration number (or is newer, if the iteration number cannot be read).
latestGdxFile <- function(files) {
  candidates <- c(files$gdx, files$gdxNonOptimal)
  candidates <- candidates[file.exists(candidates)]
  if (length(candidates) < 2) return(head(candidates, 1))
  iterations <- as.numeric(unlist(lapply(candidates, readGDX, "o_iterationNumber",
                                         format = "simplest", react = "silent")))
  if (length(iterations) == length(candidates)) return(candidates[which.max(iterations)])
  info <- file.info(candidates)
  rownames(info)[which.max(info$mtime)]
}

# The run configuration (cfg) from config.yml or config.Rdata, NULL without a config file.
readRunConfig <- function(path, configName) {
  if (is.null(configName)) return(NULL)
  configFile <- paste0(path, "/", configName)
  isYaml <- grepl("yml$", configName)
  cfg <- NULL
  if (any(isYaml)) cfg <- loadConfig(configFile)
  if (any(!isYaml)) cfg <- loadRdata(configFile)$cfg
  cfg
}

# The `stats` list of runstatistics.rda, NULL if the file is missing.
readRunStatistics <- function(file) {
  if (!file.exists(file)) return(NULL)
  suppressWarnings(loadRdata(file))$stats
}

runModelName <- function(stats) {
  name <- stats[["config"]][["model_name"]]
  if (is.null(name)) NA_character_ else name
}

isMagpieRun <- function(stats) {
  isTRUE(stats[["config"]][["model_name"]] == "MAgPIE")
}

# ---------------------------------------------------------------------------
# Single fields

modelstatLabels <- c("1" = "Optimal", "2" = "Locally Optimal", "3" = "Unbounded", "4" = "Infeasible",
                     "5" = "Locally Infes", "6" = "Intermed Infes", "7" = "Intermed Nonoptimal",
                     "13" = "Error No Solution")

readModelstat <- function(latestGdx, stats) {
  modelstat <- "NA"
  if (length(latestGdx) > 0) {
    value <- try(c(readGDX(gdx = latestGdx, c("o_modelstat", "p80_modelstat"), format = "first_found",
                           react = "silent")), silent = TRUE)
    if (!is.null(value) && !inherits(value, "try-error")) {
      modelstat <- gsub("0", ".", paste0(value, collapse = ""))
    }
  }
  if (modelstat == "NA" && !is.null(stats) && any(grepl("config", names(stats)))) {
    if (stats[["config"]][["model_name"]] == "MAgPIE") {
      if (any(grepl("modelstat", names(stats)))) modelstat <- paste0(as.character(stats[["modelstat"]]), collapse = "")
    } else if (any(grepl("modelstat", names(stats)))) {
      # the value is taken over only if it fits into a table cell, coerced as the table would
      cell <- data.frame(modelstat = "NA")
      stored <- try(cell[1, "modelstat"] <- stats[["modelstat"]], silent = TRUE)
      if (!inherits(stored, "try-error")) modelstat <- cell[1, "modelstat"]
    }
  }
  if (modelstat %in% names(modelstatLabels)) {
    modelstat <- paste0(modelstat, ": ", modelstatLabels[[modelstat]])
  }
  modelstat
}

# Has the run been uploaded to shinyResults::appResults? (cluster only)
inAppResults <- function(stats) {
  if (!any(grepl("id", names(stats)))) return("no")
  resultsDir <- if (any(grepl("config", names(stats))) && stats[["config"]][["model_name"]] == "MAgPIE") {
    paste0(Sys.getenv("MAGPIE_RESULTS_ARCHIVE_PATH"), "/")
  } else {
    "/p/projects/rd3mod/models/results/remind/"
  }
  idFile <- paste0(resultsDir, stats[["id"]], ".rds")
  overview <- file.info(Sys.glob(paste0(resultsDir, "overview.rds")))
  if (file.exists(idFile) && all((overview$mtime + 600) > file.info(idFile)$mtime)) "yes" else "no"
}

remindMifFile <- function(path, cfg) {
  paste0(path, "/REMIND_generic_", cfg[["title"]], ".mif")
}

remindCheckFile <- function(path, cfg, suffix) {
  paste0(path, "/REMIND_generic_", cfg[["title"]], suffix)
}

mifStatus <- function(path, files, cfg, stats) {
  if (is.null(files$configName) || !file.exists(paste0(path, "/", files$configName))) return("NA")
  if (isMagpieRun(stats)) {
    mifFile <- paste0(path, "/validation.mif")
    return(if (file.exists(mifFile) && file.info(mifFile)[["size"]] > 99999) "yes" else "no")
  }
  if (!file.exists(remindMifFile(path, cfg))) {
    "no"
  } else if (file.exists(remindCheckFile(path, cfg, "_summation_errors.csv"))) {
    "sumErr"
  } else {
    "yes"
  }
}

# The maximum number of Nash iterations: cfg$gms$cm_iteration_max, or the value set
# at the end of full.gms when autoconvergence may have changed it.
maxIterations <- function(cfg, runType, fullGms) {
  iterationMax <- cfg$gms$cm_iteration_max
  if (isTRUE(cfg$gms$cm_nash_autoconverge > 0) && grepl("nash", runType) && file.exists(fullGms)) {
    iterationMax <- lastMatchingLine(fullGms, "cm_iteration_max = [1-9].*.;[ ]*$")
    iterationMax <- sub(";[ ]*", "", sub("^.*.= ", "", iterationMax))
  }
  iterationMax
}

# Iterations and run status from full.log, with the cluster specific interpretation
# of a run that has no status line yet (or any more).
readRunProgress <- function(path, files, run, latestGdx, iterationMax, onCluster) {
  iterations <- "NA"
  runStatus <- "NA"
  if (!file.exists(files$fullLog)) {
    if (file.exists(files$logTxt) && lastLineMatches(files$logTxt, "try to acquire model lock")) {
      runStatus <- "Wait REMIND lock"
    } else {
      runStatus <- "full.log missing"
    }
    return(list(iterations = iterations, runStatus = runStatus))
  }

  loops <- sub("^.*.= ", "", shellLines(paste0("grep 'LOOPS' '", files$fullLog, "' | tail -1")))
  if (length(loops) > 0) iterations <- loops
  if (length(iterationMax) > 0) iterations <- paste0(iterations, "/", iterationMax)

  statusLines <- shellLines(paste0("grep '*** Status: ' '", files$fullLog, "'"))
  statusLines <- substr(sub("\\(s\\)", "", sub("\\*\\*\\* Status: ", "", statusLines)), start = 1, stop = 17)
  # several status lines (a restarted run) cannot be stored in one cell: the status stays unknown
  if (length(statusLines) == 1) runStatus <- statusLines

  if (onCluster && runStatus == "NA") {
    runStatus <- clusterRunStatus(path, files, run, latestGdx)
  } else if (runStatus == "NA") {
    runStatus <- "Run interrupted"
  }
  list(iterations = iterations, runStatus = refineRunStatus(runStatus, files, run$jobInSlurm))
}

# Later stages of a run that has a status: the reporting after a normal completion,
# an abort because of infeasibilities, the MAgPIE part of a coupled run.
refineRunStatus <- function(runStatus, files, jobInSlurm) {
  if (runStatus == "Normal completion" && file.exists(files$logTxt) && jobInSlurm != "no") {
    started <- lastMatchingLine(files$logTxt, "Starting output generation for")
    finished <- lastMatchingLine(files$logTxt, "Finished output generation for")
    if (length(started) > length(finished)) runStatus <- "Running reporting"
  }
  if (runStatus == "Execution error" && file.exists(files$abortGdx)) {
    runStatus <- abortStatus(files$abortGdx, runStatus)
  }
  if (file.exists(files$logMagTxt) && jobInSlurm != "no" &&
        (runStatus == "Normal completion" || grepl("log-mag.txt", files$logMagTxt))) {
    runStatus <- coupledMagpieStatus(files, runStatus)
  }
  runStatus
}

# Status of a run without a GAMS status line on the cluster: interrupted by SLURM,
# or still running (possibly stalled in conopt or busy with a coupled MAgPIE run).
clusterRunStatus <- function(path, files, run, latestGdx) {
  jobInSlurm <- run$jobInSlurm
  if (jobInSlurm == "no" || grepl("pending$", jobInSlurm)) {
    if (!file.exists(files$logTxt)) {
      return(if (jobInSlurm == "no") "Run interrupted" else "Run restarted")
    }
    slurmError <- shellLines(paste0("grep 'slurmstepd.*error' ", files$logTxt))
    interrupts <- c("DUE TO TIME LIMIT" = "Timeout interrupt", "memory|oom-kill" = "Memory interrupt",
                    "DUE TO PREEMPTION" = "Preempt interrupt", "DUE TO JOB REQUEUE" = "Run requeued",
                    "CANCELLED" = "Run cancelled")
    for (pattern in names(interrupts)) {
      if (isTRUE(any(grepl(pattern, slurmError)))) return(interrupts[[pattern]])
    }
    return("NA")
  }
  runStatus <- "Run in progress"
  gridFiles <- Sys.glob(file.path(path, "225*", "grid*", "gmsgrid.log"))
  if (length(gridFiles) > 0) {
    conoptDelay <- round(difftime(Sys.time(), max(file.info(gridFiles)$mtime), units = "hours") - 0.049, 1)
    gdxDelay <- 1
    if (length(latestGdx) > 0 && file.exists(latestGdx)) {
      gdxDelay <- difftime(Sys.time(), file.info(latestGdx)$mtime, units = "hours")
    }
    if (conoptDelay > 0.1 && gdxDelay > 0.25) runStatus <- paste0("conoptspy >", niceround(conoptDelay, 1), "h")
  }
  # coupled REMIND-MAgPIE run with MAgPIE currently running: report its iteration and year
  if (file.exists(files$logTxt)) {
    lastLine <- suppressWarnings(system(paste("awk 'NF{s=$0}END{print s}'", files$logTxt), intern = TRUE))
    if (lastLine == "Starting MAgPIE...") {
      cfg <- run$config
      magpieIterations <- getRunStatus(file.path(cfg$path_magpie, cfg$cfg_mag$results_folder))[["Iter"]]
      couplingIteration <- gsub(".*-mag-([0-9]{1,2})$", "\\1", cfg$cfg_mag$results_folder)
      runStatus <- paste0("mag-", couplingIteration, " ", magpieIterations)
    }
  }
  runStatus
}

# "Abort <region> <n>*Infes" when the run was aborted because of consecutive infeasibilities.
abortStatus <- function(abortGdx, runStatus) {
  maxInfes <- try(as.numeric(readGDX(gdx = abortGdx, "cm_abortOnConsecFail", format = "simplest", react = "silent")),
                  silent = TRUE)
  consecFail <- try(quitte::as.quitte(readGDX(gdx = abortGdx, "p80_trackConsecFail", react = "silent")), silent = TRUE)
  if (inherits(maxInfes, "try-error") || !isTRUE(maxInfes > 0) || inherits(consecFail, "try-error") ||
        is.null(consecFail)) {
    return(runStatus)
  }
  regions <- unique(consecFail[consecFail$value == maxInfes, ]$region)
  if (length(regions) == 0) return(runStatus)
  regions <- if (length(regions) == 1) paste0(regions, " ") else paste0(length(regions), "R*")
  paste0("Abort ", regions, maxInfes, "*Infes")
}

# Status of the MAgPIE part of a coupled run, read from log-mag.txt (or log.txt).
coupledMagpieStatus <- function(files, runStatus) {
  started <- lastMatchingLine(files$logMagTxt, "Preparing MAgPIE")
  stored <- lastMatchingLine(files$logMagTxt, "MAgPIE output was stored")
  if (length(started) > length(stored)) {
    magpieLog <- gsub("-rem-", "-mag-", gsub("output", file.path("magpie", "output"), files$fullLog))
    if (!file.exists(magpieLog)) {
      magpieLog <- gsub("-rem-", "-mag-", gsub("output", file.path("..", "magpie", "output"), files$fullLog))
    }
    loops <- NULL
    if (file.exists(magpieLog)) {
      loops <- sub("^.*.= ", "", shellLines(paste0("grep 'LOOPS' '", magpieLog, "' | tail -1")))
    }
    if (length(lastMatchingLine(files$logMagTxt, "Start getReport")) > 0) loops <- "report"
    runStatus <- paste("Run MAgPIE", loops)
  }
  if (lastLineMatches(files$logMagTxt, "try to acquire model lock")) runStatus <- "Wait MAgPIE lock"
  runStatus
}

# Number of R warnings: for MAgPIE from slurm.log, for REMIND from log.txt.
readWarnings <- function(files, stats) {
  if (file.exists(files$slurmLog) && isMagpieRun(stats)) {
    warnings <- shellLines(paste0("grep -zoP \"Warning messages:\\n([0-9]+:(.*\\n)?.*\\n)*([0-9]+)\" ",
                                  files$slurmLog, " | tail -1"))
    if (length(warnings) > 0) return(warnings)
    if (length(shellLines(paste0("grep \"Warning message:\" ", files$slurmLog, " | tail -1"))) > 0) return("1")
    return("0")
  }
  if (file.exists(files$logTxt) && !isMagpieRun(stats)) {
    warnings <- shellLines(paste0("grep -zoP \"There were ([0-9]+) warnings\" ", files$logTxt))
    if (length(warnings) > 0) {
      return(gsub("^[^0-9]*([0-9]+)[^0-9]*$", "\\1", warnings))
    }
    warnings <- shellLines(paste0("grep -zoP \"Warning messages:\\n([0-9]+:(.*\\n)?.*\\n)*\" ", files$logTxt))
    return(as.character(length(grep("^[0-9]+:", warnings))))
  }
  "NA"
}

# Convergence of a Nash run from the gdx: s80_bool, the iteration count, or the
# regional modelstats of the last iteration.
readConvergence <- function(files, cfg, runType, latestGdx, iterationMax) {
  if (!isTRUE(grepl("nash", runType)) || length(latestGdx) == 0) return("NA")
  iteration <- try(as.numeric(readGDX(gdx = latestGdx, "o_iterationNumber", format = "simplest")), silent = TRUE)
  converged <- try(as.numeric(readGDX(gdx = latestGdx, "s80_bool", type = "Parameter", format = "simplest")),
                   silent = TRUE)
  if (inherits(converged, "try-error") || inherits(iteration, "try-error")) return("NA")
  if (converged == 1) {
    return(if (file.exists(files$gdxNonOptimal)) "converged (had INFES)" else "converged")
  }
  if (converged == 0 && as.numeric(iterationMax) == iteration) return("not_converged")
  repy <- try(readGDX(gdx = latestGdx, "p80_repy"), silent = TRUE)
  if (inherits(repy, "try-error")) return("NA")
  paste(repy[, , "modelstat"], collapse = "")
}

# CES calibration runs: append the calibration iteration and detect "Clb_converged".
calibrationInfo <- function(path, files, run) {
  iterations <- run$iterations
  convergence <- run$convergence
  isCalibration <- isTRUE(grepl("Calib", run$runType)) || isTRUE(run$config$gms$CES_parameters == "calibrate")
  if (isCalibration && file.exists(files$logTxt)) {
    calibIteration <- tail(shellLines(paste0("grep 'CES calibration iteration' '", files$logTxt,
                                             "' |  grep -Eo  '[0-9]{1,2}'")), n = 1)
    if (isTRUE(as.numeric(calibIteration) > 0)) iterations <- paste0(iterations, " ", "Clb: ", calibIteration)
    if (isTRUE(convergence %in% c("converged", "converged (had INFES)")) &&
          (length(system(paste0("find ", path, " -name 'fulldata_*.gdx'"), intern = TRUE)) > 10 ||
             length(system(paste0("find ", path, " -name 'input_*.gdx'"), intern = TRUE)) > 10)) {
      convergence <- "Clb_converged"
    }
  }
  list(iterations = iterations, convergence = convergence)
}

# GAMS run time in seconds, or the time since preparation started for an active run.
readRuntime <- function(stats, jobInSlurm) {
  if (any(grepl("GAMSEnd", names(stats)))) {
    return(as.numeric(round(difftime(stats[["timeGAMSEnd"]], stats[["timeGAMSStart"]], units = "secs"), 0)))
  }
  if (any(grepl("timePrepareStart", names(stats))) && !jobInSlurm %in% "no") {
    return(as.numeric(round(difftime(Sys.time(), stats[["timePrepareStart"]], units = "secs"), 0)))
  }
  NA
}

# Results of the plausibility checks of a REMIND run with a mif (empty list otherwise).
sanityChecks <- function(path, files, cfg, stats) {
  if (is.null(files$configName) || !file.exists(file.path(path, files$configName)) || isMagpieRun(stats)) {
    return(list())
  }
  if (!file.exists(remindMifFile(path, cfg))) return(list())
  countVariables <- function(file) {
    if (file.exists(file)) length(unique(read.csv2(file, sep = ",")$variable)) else 0
  }
  rangeErrFile <- remindCheckFile(path, cfg, "_range_errors.txt")
  checks <- list(
    summationErrors = countVariables(remindCheckFile(path, cfg, "_summation_errors.csv")),
    rangeErrors = if (file.exists(rangeErrFile)) nrow(read.csv2(rangeErrFile)) else 0,
    fixErrors = countVariables(file.path(path, "log_fixOnRef.csv"))
  )
  projectFile <- paste0(path, "/projectSummations.rds")
  if (file.exists(projectFile)) {
    project <- readRDS(projectFile)
    # `[<-` with list() keeps NULL entries, which then fail in setField() like they did before
    checks["missingProjVars"] <- list(project[["ScenarioMIP"]][["missingVars"]])
    checks["projSummationErrors"] <- list(project[["ScenarioMIP"]][["checkSummations"]])
    checks["projSummationErrorsRegional"] <- list(project[["ScenarioMIP"]][["checkSummationsRegional"]])
  } else {
    checks[c("missingProjVars", "projSummationErrors", "projSummationErrorsRegional")] <- NA
  }
  checks
}

# A coarse classification of the detailed run status.
coarseStatus <- function(runStatus, jobInSlurm, onCluster) {
  if (grepl("pending$", jobInSlurm)) return("pending")
  if (onCluster && !jobInSlurm %in% c("no", "NA")) return("running")
  if (grepl("^(Run in progress|Running reporting|conoptspy|mag-|Run MAgPIE|Wait )", runStatus)) return("running")
  if (runStatus == "Normal completion") return("completed")
  if (runStatus == "NA") return("unknown")
  "error"
}
