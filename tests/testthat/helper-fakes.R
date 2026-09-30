# Helpers shared by the tests: fake run folders and fake command line tools.

# A REMIND run folder with the files modelRun() reads (no gdx: modelstat stays "NA").
fakeRemindRun <- function(dir, title = "SSP2-NPi-AMT", status = "Normal completion", loops = 26,
                          warnings = 27, summationErrors = TRUE, mif = TRUE, runtimeSeconds = 3600) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  cfg <- list(title = title, model_name = "REMIND", results_folder = dir, remind_folder = dirname(dirname(dir)),
              gms = list(optimization = "nash", cm_iteration_max = 100, cm_nash_autoconverge = 0))
  save(cfg, file = file.path(dir, "config.Rdata"))
  start <- as.POSIXct("2026-09-28 10:00:00", tz = "UTC")
  stats <- list(config = list(model_name = "REMIND"), id = "run-id", timePrepareStart = start - 60,
                timeGAMSStart = start, timeGAMSEnd = start + runtimeSeconds)
  save(stats, file = file.path(dir, "runstatistics.rda"))
  writeLines(c("some output", paste0("--- LOOPS iteration = ", loops),
               if (!is.null(status)) paste0("*** Status: ", status)), file.path(dir, "full.log"))
  writeLines(c("Starting output generation for x", "Finished output generation for x",
               if (!is.null(warnings)) paste0("There were ", warnings, " warnings (use warnings() to see them)")),
             file.path(dir, "log.txt"))
  writeLines(c("full.gms", "cm_iteration_max = 100;"), file.path(dir, "full.gms"))
  if (mif) writeLines("Model;Scenario;Region;Variable;Unit;2005;", file.path(dir, remindMifName(title)))
  if (summationErrors) {
    writeLines(c("variable,value", "a,1", "b,2", "a,3"), file.path(dir, remindMifName(title, "_summation_errors.csv")))
  }
  invisible(dir)
}

remindMifName <- function(title, suffix = ".mif") paste0("REMIND_generic_", title, suffix)

# A directory of fake command line tools; each `name = "shell body"` becomes an executable.
# Every fake appends its name and arguments to <dir>/calls.log.
fakeBinDir <- function(..., envir = parent.frame()) {
  fakes <- list(...)
  dir <- withr::local_tempdir(.local_envir = envir)
  for (name in names(fakes)) {
    script <- file.path(dir, name)
    writeLines(c("#!/bin/sh", paste0("echo \"", name, " $*\" >> \"", dir, "/calls.log\""), fakes[[name]]), script)
    Sys.chmod(script, "755")
  }
  withr::local_path(dir, .local_envir = envir)
  dir
}

fakeCalls <- function(binDir) {
  if (file.exists(file.path(binDir, "calls.log"))) readLines(file.path(binDir, "calls.log")) else character(0)
}
