# Start phase of the automated model tests (AMT): update the REMIND checkout and
# submit the test runs.

amtSlurmConfigTestOneRegi <- "--qos=priority --nodes=1 --tasks-per-node=1 --wait --time=2:00:00"
amtSlurmConfigRuns <- "--qos=standby --nodes=1 --tasks-per-node=12 --time=36:00:00"

amtStart <- function(paths, cycle) {
  model <- paths$model
  updateCheckout(model)
  cycle$commit <- gitHead(model)
  amtLog("testing commit ", cycle$commit)
  cycle$scenarios <- plannedScenarios(model)
  amtLog("scenarios that should be started: ", paste(cycle$scenarios, collapse = ", "))
  cycle <- writeAmtState(paths, cycle)
  # rs -A finds the runs of the latest test through this file
  saveRDS(cycle$runcode, file = paths$runcode)

  # empty realization folders break the model, but can be left over because git does not
  # delete empty folders if all files in the folder are deleted.
  deleteEmptyRealizationFolders(model)

  # do not ask when installing dependencies in piamenv::fixDeps
  withr::local_envvar(autoRenvFixDeps = "TRUE")

  amtLog("starting the testOneRegi run (downloads the input data first)")
  setForceDownload(model, TRUE)
  runCommand("Rscript", c("start.R", "--testOneRegi", "titletag=AMT",
                          shellArg("slurmConfig", amtSlurmConfigTestOneRegi)), cwd = model)
  setForceDownload(model, FALSE)

  amtLog("starting the AMT scenarios")
  runCommand("Rscript", c("start.R", "startgroup=AMT", "titletag=AMT", shellArg("slurmConfig", amtSlurmConfigRuns),
                          "config/scenario_config.csv"), cwd = model)

  amtLog("starting the coupled test (make test-full-slurm)")
  updateCheckout(file.path(model, "magpie"))
  runCommand("make", "test-full-slurm", cwd = model)

  cycle$phase <- "started"
  writeAmtState(paths, cycle)
}

# git reset --hard origin/develop && git pull
updateCheckout <- function(dir) {
  if (!dir.exists(dir)) stop("the checkout ", dir, " does not exist")
  runCommandOrStop("git", c("reset", "--hard", "origin/develop"), cwd = dir, what = paste("git reset in", dir))
  runCommandOrStop("git", "pull", cwd = dir, what = paste("git pull in", dir))
}

gitHead <- function(dir) {
  head <- runCommandOrStop("git", c("log", "-1", "--format=%H"), cwd = dir, what = "git log", capture = TRUE)
  hash <- regmatches(head$output, regexpr("[0-9a-f]{40}", head$output))
  if (length(hash) == 0) stop("no commit hash in the output of git log: ", paste(head$output, collapse = " / "))
  hash[1]
}

# Set cfg$force_download in config/default.cfg (the first run of a test downloads the input data).
setForceDownload <- function(model, on) {
  file <- file.path(model, "config", "default.cfg")
  lines <- readLines(file, warn = FALSE)
  value <- if (on) "TRUE" else "FALSE"
  lines <- sub("^cfg\\$force_download <- (TRUE|FALSE)", paste0("cfg$force_download <- ", value), lines)
  writeLines(lines, file)
  amtLog("set cfg$force_download <- ", if (on) "TRUE" else "FALSE", " in ", file)
}

deleteEmptyRealizationFolders <- function(model) {
  for (moduleFile in list.files(file.path(model, "modules"), pattern = "^module\\.gms$", recursive = TRUE,
                                full.names = TRUE)) {
    moduleDir <- dirname(moduleFile)
    realizations <- grep("realization.gms", readLines(moduleFile, warn = FALSE), value = TRUE)
    realizations <- sub("/realization.gms\"$", "", sub("^.*.modules/[0-9a-zA-Z_]{1,}/", "", realizations))
    empty <- setdiff(dir(moduleDir), c("module.gms", "input", realizations))
    if (length(empty) > 0) {
      amtLog("removing unused realization folders of ", basename(moduleDir), ": ", paste(empty, collapse = ", "))
      unlink(file.path(moduleDir, empty), recursive = TRUE)
    }
  }
}

# The names of the runs that start.R starts for the AMT group, from
# config/scenario_config.csv and REMIND's own selectScenarios().
plannedScenarios <- function(model) {
  settings <- read.csv2(file.path(model, "config", "scenario_config.csv"), stringsAsFactors = FALSE,
                        row.names = 1, comment.char = "#", na.strings = "")
  scripts <- new.env(parent = globalenv())
  for (script in list.files(file.path(model, "scripts", "start"), pattern = "\\.R$", full.names = TRUE)) {
    sys.source(script, envir = scripts)
  }
  if (!exists("selectScenarios", envir = scripts)) {
    stop("selectScenarios() not found in ", file.path(model, "scripts", "start"))
  }
  selected <- scripts$selectScenarios(settings = settings, interactive = FALSE, startgroup = "AMT")
  paste0(rownames(selected), "-AMT")
}
