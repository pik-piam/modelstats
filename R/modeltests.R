#' modeltests
#'
#' Runs the automated model tests (AMT) of REMIND. A test cycle has two phases
#' that are started by separate calls, typically from two cron jobs:
#' * `start`: update the REMIND checkout in `mydir` to `origin/develop`, start the
#'   `testOneRegi` run (which downloads the input data), the scenarios of the
#'   `AMT` group in `config/scenario_config.csv` and the coupled test
#'   (`make test-full-slurm`);
#' * `evaluate`: wait until the runs are finished, inspect them with [modelRun()],
#'   compare each with the previous run of the same scenario (runtime, and a
#'   `compareScenarios2` PDF in the run folder), move runs older than 90 days to
#'   `output/archive`, write the report and post it to Mattermost (and, with
#'   `email = TRUE`, commit it to the `testing_suite` repository in `gitdir`).
#'
#' With `action = "auto"` the phase is chosen from the record of the current cycle
#' (`amt-state.json` in the parent folder of `mydir`): a new cycle is started after
#' an evaluated one, a started cycle is evaluated. A cycle whose start or
#' evaluation failed with an error is recorded as `failed` and the tests continue
#' with the next phase; a cycle recorded as `starting` or `evaluating` (a process
#' that was killed) is not touched, the call fails with an explanation and the
#' phase has to be given explicitly once.
#'
#' Every cycle writes one log file, `logs/amt-<date>.log` in the parent folder of
#' `mydir`, covering both phases including the output of all commands. A lock
#' (`amt.lock` in the same folder) keeps two calls from working on the same folder
#' at the same time; a lock left behind by a process that died on the same host is
#' removed automatically, one from another host has to be removed by hand.
#' A second start on the same day continues the cycle of that day.
#'
#' @param mydir path to the REMIND folder in which the tests are run
#' @param gitdir path to the clone of the `testing_suite` repository that receives
#'   the report as `README.md` (only used with `email = TRUE`)
#' @param model kept for compatibility, must be `"REMIND"`
#' @param user the cluster user who starts the runs (default: the current user)
#' @param email whether to commit the report to `gitdir` (which triggers the email
#'   notification of the `testing_suite` repository)
#' @param compScen whether to start `compareScenarios2` for every finished run
#' @param mattermostToken URL of the Mattermost webhook receiving the report,
#'   `NULL` for no message
#' @param action `"auto"` (default), `"start"` or `"evaluate"`, see details
#' @param pollInterval seconds between two checks whether the runs are finished
#' @return invisibly the record of the cycle (a list)
#'
#' @author Anastasis Giannousakis, David Klein, Tobias Diez
#' @seealso [modelRun()]
#' @importFrom utils read.csv2
#' @export
modeltests <- function(mydir = ".", gitdir = NULL, model = "REMIND", user = NULL, email = TRUE, compScen = TRUE,
                       mattermostToken = NULL, action = c("auto", "start", "evaluate"), pollInterval = 600) {
  action <- match.arg(action)
  if (!identical(model, "REMIND")) stop("modeltests() runs the REMIND tests only (MAgPIE support was removed)")
  if (email && is.null(gitdir)) stop("email = TRUE needs the gitdir with the clone of the testing_suite repository")
  if (is.null(user)) user <- Sys.info()[["user"]]
  settings <- list(gitdir = gitdir, user = user, email = email, compScen = compScen,
                   mattermostToken = mattermostToken, pollInterval = pollInterval)
  paths <- amtPaths(mydir)
  lock <- acquireAmtLock(paths)
  withr::defer(releaseAmtLock(lock))

  state <- readAmtState(paths, action)
  phase <- tryCatch(nextAmtPhase(state, action), error = function(e) {
    notifyAmtFailure(state, paths, "auto", conditionMessage(e), mattermostToken)
    stop(e)
  })
  cycle <- if (phase == "start") newAmtCycle(state) else state
  if (is.null(cycle)) stop("nothing to evaluate: no test cycle has been started yet")
  cycle$phase <- if (phase == "start") "starting" else "evaluating"
  logFile <- file.path(paths$root, cycle$log)
  message("modeltests: ", phase, " of cycle ", cycle$cycle, ", logging to ", logFile)

  withAmtLog(logFile, {
    amtLog("==== ", phase, " of AMT cycle ", cycle$cycle, " in ", paths$model, " (modelstats ",
           as.character(utils::packageVersion("modelstats")), ") ====")
    if (file.exists(paths$legacyStatus)) {
      amtLog("NOTE: ", paths$legacyStatus, " is not used any more and can be deleted; the state is in ", paths$state)
    }
    cycle <- writeAmtState(paths, cycle)
    # on an error, the latest checkpoint of the cycle (amtEnv$cycle) is marked as failed
    recordFailure <- function(e) {
      failed <- amtEnv$cycle
      failed$phase <- "failed"
      failed$failedPhase <- phase
      failed$error <- conditionMessage(e)
      writeAmtState(paths, failed)
      notifyAmtFailure(failed, paths, phase, conditionMessage(e), mattermostToken)
      stop(e)
    }
    cycle <- tryCatch(if (phase == "start") amtStart(paths, cycle) else amtEvaluate(paths, cycle, settings),
                      error = recordFailure)
    amtLog("==== ", phase, " of AMT cycle ", cycle$cycle, " finished ====")
  })
  message("modeltests: ", phase, " finished, see ", logFile)
  invisible(cycle)
}

# Tell the Mattermost channel that a phase failed (best effort).
notifyAmtFailure <- function(cycle, paths, phase, error, token) {
  if (is.null(token)) return(invisible(FALSE))
  log <- if (is.null(cycle$log)) "(no log file)" else file.path(paths$root, cycle$log)
  text <- paste0("The REMIND automated model tests failed in phase '", phase, "'",
                 if (!is.null(cycle$cycle)) paste0(" of cycle ", cycle$cycle), ": ", error,
                 "\nLog: ", log, "\nState: ", paths$state)
  sendMattermostMessage(text, token)
}
