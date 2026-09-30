# Logging and shell commands of the automated model tests (AMT).
#
# Every AMT cycle (one start and the following evaluation) writes to a single log file.
# amtLog() appends time-stamped lines to it, runCommand() runs a program with its output
# streamed into the same file, and withAmtLog() routes R's console output, messages and
# warnings raised anywhere in the code into it, so that the log is complete and ordered.

amtEnv <- new.env(parent = emptyenv())

# Append one line to the current AMT log (or print it, when no log is open).
amtLog <- function(...) {
  line <- paste0("[", timeStamp(), "] ", paste0(..., collapse = ""))
  if (is.null(amtEnv$log)) {
    message(line)
  } else {
    cat(line, "\n", sep = "", file = amtEnv$log)
    flush(amtEnv$log)
  }
  invisible(line)
}

# Evaluate `expr` with `file` as the AMT log: console output, messages and warnings
# become log lines, an error is logged and re-raised.
withAmtLog <- function(file, expr) {
  dir.create(dirname(file), showWarnings = FALSE, recursive = TRUE)
  if (!is.null(amtEnv$log)) stop("an AMT log is already open")
  amtEnv$logFile <- file
  amtEnv$log <- file(file, open = "at")
  withr::defer({
    close(amtEnv$log)
    amtEnv$log <- NULL
    amtEnv$logFile <- NULL
  })
  withr::local_output_sink(amtEnv$log)
  withCallingHandlers(
    tryCatch(expr, error = function(e) {
      amtLog("ERROR: ", conditionMessage(e))
      stop(e)
    }),
    message = function(m) {
      amtLog(sub("\n$", "", conditionMessage(m)))
      invokeRestart("muffleMessage")
    },
    warning = function(w) {
      amtLog("WARNING: ", conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
}

# Run a program and log its command line, output and exit status.
# `args` are passed to the shell as they are: quote them with shQuote() where needed.
# By default the output is appended to the log file by the program itself (so that it is
# there even if R dies while the program runs); with `capture = TRUE` it is returned
# instead (and logged only when the program fails), with `quiet = TRUE` nothing is logged
# unless the program fails.
runCommand <- function(command, args = character(0), cwd = NULL, capture = FALSE, quiet = FALSE) {
  if (!quiet) amtLog("$ ", paste(c(command, args), collapse = " "), if (!is.null(cwd)) paste0("   (in ", cwd, ")"))
  if (!is.null(amtEnv$log)) flush(amtEnv$log)
  run <- function() {
    if (capture || is.null(amtEnv$logFile)) {
      suppressWarnings(system2(command, args, stdout = TRUE, stderr = TRUE))
    } else {
      status <- system2("sh", c("-c", shQuote(paste(c(command, args, ">>", shQuote(amtEnv$logFile), "2>&1"),
                                                     collapse = " "))))
      structure(character(0), status = if (status == 0) NULL else status)
    }
  }
  output <- if (is.null(cwd)) run() else withr::with_dir(cwd, run())
  status <- attr(output, "status")
  if (is.null(status)) status <- 0L
  if (status != 0) {
    if (quiet) amtLog("$ ", paste(c(command, args), collapse = " "))
    for (line in output) amtLog("  | ", line)
    amtLog("  => exit status ", status)
  } else if (!quiet && !capture) {
    amtLog("  => done")
  }
  list(status = status, output = as.character(output))
}

# Run a program, return its output, and stop with `what` when it fails.
runCommandOrStop <- function(command, args = character(0), cwd = NULL, what = paste(command, args[1]),
                             capture = FALSE) {
  result <- runCommand(command, args, cwd = cwd, capture = capture)
  if (result$status != 0) {
    stop(what, " failed with exit status ", result$status,
         if (length(result$output) > 0) paste0(": ", paste(tail(result$output, 3), collapse = " / ")))
  }
  invisible(result)
}

# A "name=value" command line argument with the value quoted for the shell.
shellArg <- function(name, value) {
  paste0(name, "=", shQuote(value))
}
