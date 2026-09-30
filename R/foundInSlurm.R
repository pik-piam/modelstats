#' foundInSlurm
#'
#' Is the run found in SLURM? Looks for a job whose working directory and name
#' match the run folder (for coupled `-rem-N`/`-mag-N` iterations: the coupled
#' run's main folder and, for MAgPIE iterations, the REMIND job of the same
#' iteration).
#'
#' @param mydir Path to the folder(s) where the run(s) is(are) performed
#' @param user the user whose runs will be sought for
#' @return `"no"` if no job is found; for one job of `user` its QOS, for one job
#'   of someone else that user name, both suffixed with `" pending"` or
#'   `" startup"` (running for less than six minutes); for several jobs the
#'   common user name or `"<n> users"`
#'
#' @author Anastasis Giannousakis, Oliver Richters
#' @export
foundInSlurm <- function(mydir = ".", user = NULL) {
  if (is.null(user)) user <- Sys.info()[["user"]]
  suppressWarnings(mydir <- normalizePath(mydir))
  runName <- basename(mydir)
  if (grepl("-(rem|mag)-[0-9]+$", mydir)) mydir <- dirname(dirname(mydir))

  jobs <- system("squeue -h -o '%u %Z %j %M %T %q'", intern = TRUE)
  matching <- grep(mydir, jobs, value = TRUE, fixed = TRUE)
  matching <- grep(paste0(runName, " "), matching, value = TRUE, fixed = TRUE)
  # the REMIND job of a coupled MAgPIE iteration
  if (length(matching) == 0 && grepl("-mag-[0-9]+$", runName)) {
    remindName <- gsub("-mag-", "-rem-", runName)
    matching <- grep(paste0(remindName, " "), jobs, value = TRUE, fixed = TRUE)
    matching <- grep(dirname(mydir), matching, value = TRUE, fixed = TRUE)
  }

  if (length(matching) == 1) {
    fields <- strsplit(matching, " ")[[1]]
    time <- rev(fields)[[3]]
    qos <- rev(fields)[[1]]
    pending <- if (grepl("PENDING [A-Za-z]*$", matching)) " pending" else NULL
    startup <- if (grepl("^[0-5]:[0-9]{2}$", time) && is.null(pending)) " startup" else NULL
    owner <- if (grepl(paste0("^", user, " "), matching)) qos else fields[[1]]
    return(paste0(owner, startup, pending))
  }
  users <- vapply(strsplit(matching, " "), `[`, "", 1)
  if (length(unique(users)) == 1) return(users[[1]])
  if (length(matching) > 1) return(paste(length(matching), "users"))
  "no"
}
