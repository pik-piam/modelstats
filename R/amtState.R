# State of the automated model tests (AMT): the cycle record and the lock.
#
# A cycle is one round of tests: the runs are started (phase "start") and, once they
# are done, evaluated (phase "evaluate"). The record of the current cycle is kept in
# <AMT root>/amt-state.json, where the AMT root is the parent of the REMIND folder
# (e.g. /p/projects/remind/modeltests). It replaces the .testsstatus toggle file.
#
# Record fields: cycle (its id, the start date), phase ("starting", "started",
# "evaluating", "evaluated" or "failed"), failedPhase and error (when failed),
# startedAt, evaluatedAt, updatedAt, runcode (regular expression matching the run
# folders of this cycle), scenarios (the scenarios that should have been started),
# commit (the REMIND commit tested), lastCommit (the commit tested by the previous
# evaluated cycle), log (the cycle's log file, relative to the AMT root), host, pid.

amtPhases <- c("starting", "started", "evaluating", "evaluated", "failed")

# The folders and files the AMT works with, derived from the REMIND folder.
amtPaths <- function(mydir) {
  model <- normalizePath(mydir, mustWork = TRUE)
  root <- dirname(model)
  list(
    model = model,
    root = root,
    output = file.path(model, "output"),
    archive = file.path(model, "output", "archive"),
    state = file.path(root, "amt-state.json"),
    lock = file.path(root, "amt.lock"),
    logs = file.path(root, "logs"),
    testFullLog = file.path(model, "test-full.log"),
    legacyStatus = file.path(root, ".testsstatus"),
    runcode = file.path(model, "runcode.rds"),
    legacyLastCommit = file.path(model, "lastcommit.rds"),
    legacyRunsToStart = file.path(model, "runsToStart.rds")
  )
}

# The current cycle record, migrated from the files of the old modeltests() if
# there is none yet, or NULL when no test has been run.
readAmtState <- function(paths, action = "auto") {
  if (file.exists(paths$state)) {
    state <- jsonlite::fromJSON(paths$state, simplifyVector = TRUE)
    if (!isTRUE(state$phase %in% amtPhases)) stop("unknown phase '", state$phase, "' in ", paths$state)
    return(state)
  }
  legacyAmtState(paths, action)
}

# Write the cycle record (atomically: written to a temporary file, then renamed) and
# remember it as the latest checkpoint.
writeAmtState <- function(paths, state) {
  state$updatedAt <- timeStamp()
  state <- Filter(Negate(is.null), state)
  tmp <- tempfile("amt-state-", tmpdir = dirname(paths$state), fileext = ".json")
  jsonlite::write_json(state, tmp, auto_unbox = TRUE, pretty = TRUE, null = "null", na = "null")
  if (!file.rename(tmp, paths$state)) stop("cannot write ", paths$state)
  amtEnv$cycle <- state
  amtLog("state: cycle ", state$cycle, " is now '", state$phase, "' (", paths$state, ")")
  invisible(state)
}

# The record of a new cycle starting today, following `previous` (NULL for the first one).
# A second start on the same day continues the same cycle (same id, log and run selection).
newAmtCycle <- function(previous) {
  today <- Sys.Date()
  cycle <- format(today, "%Y-%m-%d")
  lastCommit <- if (isTRUE(previous$phase == "evaluated") && !is.null(previous$commit)) {
    previous$commit
  } else {
    previous$lastCommit
  }
  list(
    cycle = cycle,
    phase = "starting",
    startedAt = timeStamp(),
    runcode = paste0(".*-AMT_", cycle, "|.*-AMT_", today + 1),
    lastCommit = lastCommit,
    log = file.path("logs", paste0("amt-", cycle, ".log")),
    host = Sys.info()[["nodename"]],
    pid = Sys.getpid()
  )
}

# The phase to run next for `action`, or an error explaining why nothing can be done.
#   "auto": start after an evaluated (or failed) evaluation, evaluate after a (failed)
#   start; refuse while a start or evaluation is recorded as running.
nextAmtPhase <- function(state, action) {
  if (action != "auto") return(action)
  if (is.null(state)) return("start")
  running <- paste0("cycle ", state$cycle, " is recorded as '", state$phase, "' since ", state$updatedAt,
                    " (host ", state$host, ", pid ", state$pid, "). If that process is not running any more, call ",
                    "modeltests(action = 'evaluate') to evaluate the runs of this cycle or ",
                    "modeltests(action = 'start') to begin a new one.")
  switch(state$phase,
         starting = stop(running),
         evaluating = stop(running),
         started = "evaluate",
         evaluated = "start",
         failed = if (state$failedPhase == "start") "evaluate" else "start")
}

# Migration from the files of the old modeltests(): .testsstatus decided between
# starting and evaluating; runcode.rds, runsToStart.rds and lastcommit.rds held the state.
# An explicit action resolves an unknown .testsstatus ("evaluate" evaluates the runs of
# runcode.rds, "start" begins a new cycle).
legacyAmtState <- function(paths, action = "auto") {
  lastCommit <- if (file.exists(paths$legacyLastCommit)) readRDS(paths$legacyLastCommit) else NULL
  evaluated <- list(cycle = "legacy", phase = "evaluated", commit = lastCommit, lastCommit = lastCommit)
  status <- if (file.exists(paths$legacyStatus)) readLines(paths$legacyStatus, warn = FALSE)[1] else NA
  if (is.na(status)) {
    return(if (is.null(lastCommit)) NULL else evaluated)
  }
  if (identical(status, "next:start") || (action == "start" && !identical(status, "next:evaluate"))) {
    return(evaluated)
  }
  if (!identical(status, "next:evaluate") && action != "evaluate") {
    stop("found '", status, "' in ", paths$legacyStatus, " and no ", basename(paths$state), ". Check the runs, ",
         "then call modeltests(action = 'evaluate') or modeltests(action = 'start') explicitly.")
  }
  if (!file.exists(paths$runcode)) stop(paths$legacyStatus, " says '", status, "' but ", paths$runcode, " is missing")
  runcode <- readRDS(paths$runcode)
  cycle <- regmatches(runcode, regexpr("[0-9]{4}-[0-9]{2}-[0-9]{2}", runcode))
  cycle <- if (length(cycle) == 1) cycle else "legacy"
  scenarios <- if (file.exists(paths$legacyRunsToStart)) rownames(readRDS(paths$legacyRunsToStart)) else character(0)
  list(cycle = cycle, phase = "started", runcode = runcode, scenarios = scenarios, lastCommit = lastCommit,
       log = file.path("logs", paste0("amt-", cycle, ".log")))
}

# Take the AMT lock (a directory, created atomically), so that two modeltests()
# processes never work on the same test folder. A lock left behind by a process that
# died on this host is removed. Returns the lock path; release it with releaseAmtLock().
acquireAmtLock <- function(paths) {
  for (attempt in 1:2) {
    if (dir.create(paths$lock, showWarnings = FALSE)) {
      writeLines(c(Sys.info()[["nodename"]], Sys.getpid(), timeStamp()), file.path(paths$lock, "owner"))
      return(paths$lock)
    }
    owner <- tryCatch(readLines(file.path(paths$lock, "owner"), warn = FALSE), error = function(e) character(0))
    stale <- length(owner) >= 2 && identical(owner[1], Sys.info()[["nodename"]]) &&
      dir.exists("/proc") && !dir.exists(file.path("/proc", owner[2]))
    if (!stale) {
      stop("another modeltests() is running on this test folder (lock ", paths$lock, " held by host ", owner[1],
           ", pid ", owner[2], " since ", owner[3], "). Remove the lock if that process does not exist any more.")
    }
    message("removing the lock left behind by the dead process ", owner[2], " on ", owner[1])
    unlink(paths$lock, recursive = TRUE)
  }
  stop("cannot acquire the lock ", paths$lock)
}

releaseAmtLock <- function(lock) {
  unlink(lock, recursive = TRUE)
}
