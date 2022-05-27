#' getRunStatus
#'
#' Returns the current status of a run or a vector of runs
#'
#' @param mydir Path to the folder(s) where the run(s) is(are) performed
#' @param sort how to sort (nf=newest first)
#' @param user the user whose runs will be shown
#'
#' @author Anastasis Giannousakis
#' @examples
#' \dontrun{
#'
#' a <- getRunStatus(dir())
#' }
#'
#' @importFrom gdx readGDX
#' @importFrom utils tail
#' @importFrom gms loadConfig
#' @export
getRunStatus <- function(mydir = dir(), sort = "nf", user = NULL) {

  substrRight <- function(x, n) {
    substr(x, nchar(x) - n + 1, nchar(x))
  }

  rem <- function(x) return(x[-which(x == "")])

  if (is.null(user)) user <- Sys.info()[["user"]]
  mydir <- normalizePath(mydir)

  onCluster <- file.exists("/p")
  out <- data.frame()

  a <- file.info(mydir)
  a <- a[a[, "isdir"] == TRUE, ]
  if (sort == "nf") mydir <- rownames(a[order(a[, "mtime"], decreasing = TRUE), ])

  for (i in mydir) {

    ii <- i
    i <- sub(paste0(dirname(i), "/"), "", i)

    if (onCluster) out[i, "jobInSLURM"] <- foundInSlurm(i, user)

#    if (onCluster) if (!out[i,"jobInSLURM"] & onlyrunning) {
#     out <- out[setdiff(rownames(out),i),]
#     next
#    }

    # Define files

    cfgf <- grep("config.Rdata|config.yml", dir(ii), value = TRUE)
#    ifelse(grepl("yml$", cfgf), cfgf <- "config.yml", cfgf <- "config.Rdata")
    fle <- paste0(ii, "/runstatistics.rda")
    gdx <- paste0(ii, "/fulldata.gdx")
    gdx_non_optimal <- paste0(ii, "/non_optimal.gdx")
    fulllst <- paste0(ii, "/full.lst")
    fulllog <- paste0(ii, "/full.log")
    logtxt <- paste0(ii, "/log.txt")

    # Initialize objects
    stats <- runtype <- cfg <- NULL
    ifelse(grepl("yml$", cfgf), cfg <- loadConfig(paste0(ii, "/", cfgf)), load(paste0(ii, "/", cfgf)))
    # RunType
    ifelse(length(cfgf) == 0, out[i, "RunType"] <- "NA",  out[i, "RunType"] <- colRunType(ii))

    # modelstat
    out[i, "modelstat"] <- "NA"
    if (file.exists(fle)) {
      load(fle)
      if (exists("stats")) if (any(grepl("config", names(stats)))) if (stats[["config"]][["model_name"]] == "MAgPIE") {
        if (any(grepl("modelstat", names(stats)))) out[i, "modelstat"] <- paste0(as.character(stats[["modelstat"]]), collapse = "")
        if (is.na(out[i, "modelstat"])) out[i, "modelstat"] <- "NA"
      } else {
        if (any(grepl("modelstat", names(stats)))) out[i, "modelstat"] <- stats[["modelstat"]]
        if (is.na(out[i, "modelstat"])) out[i, "modelstat"] <- "NA"
      }
    }

    modelstatFiles <- c(gdx, gdx_non_optimal)
    if (out[i, "modelstat"] == "NA" && any(file.exists(modelstatFiles))) {
      modelstat <- NULL
      modelstat_iter <- NULL
      for (filename in modelstatFiles[file.exists(modelstatFiles)]) {
        modelstat <- c(modelstat, as.numeric(readGDX(gdx = filename, "o_modelstat", format = "simplest")))
        modelstat_iter <- c(modelstat_iter, as.numeric(readGDX(gdx = filename, "o_iterationNumber", format = "simplest")))
      }
      out[i, "modelstat"] <- modelstat[which.max(modelstat_iter)]
    }
    explain_modelstat <- c("1" = "Optimal", "2" = "Locally Optimal", "3" = "Unbounded", "4" = "Infeasible",
                           "5" = "Locally Infes", "6" = "Intermed Infes", "7" = "Intermed Nonoptimal")
    if (out[i, "modelstat"] %in% names(explain_modelstat)) {
      out[i, "modelstat"] <- paste0(out[i, "modelstat"], ": ", explain_modelstat[out[i, "modelstat"]])
    }

    # runInAppResults
    if (onCluster) out[i, "runInAppResults"] <- "NA"
    if (file.exists(fle)) {
      load(fle)
      if (onCluster && any(grepl("id", names(stats)))) {
        if (exists("stats")) if (any(grepl("config", names(stats)))) if (stats[["config"]][["model_name"]] == "MAgPIE") {
          ovdir <- "/p/projects/rd3mod/models/results/magpie/"
        } else {
          ovdir <- "/p/projects/rd3mod/models/results/remind/"
        }
        try(id <- paste0(ovdir, stats[["id"]], ".rds"))
        if (exists("ovdir")) if (file.exists(id) && all((file.info(Sys.glob(paste0(ovdir, "overview.rds")))$mtime + 600) > file.info(id)$mtime))
          out[i, "runInAppResults"] <- TRUE
      } else {
        if (onCluster) out[i, "runInAppResults"] <- FALSE
      }
    }

    # Iter
    if (exists("cfg")) totNoOfIter <- cfg[["gms"]][["cm_iteration_max"]]
    out[i, "Iter"] <- "NA"
    out[i, "RunStatus"] <- "NA"
    if (file.exists(fulllog)) {
      suppressWarnings(try(loop <- sub("^.*.= ", "", system(paste0("grep 'LOOPS' ", fulllog, " | tail -1"), intern = TRUE)), silent = TRUE))
      if (length(loop) > 0) out[i, "Iter"] <- loop
      if (!out[i, "RunType"] %in% c("nash", "Calib_nash") & length(totNoOfIter) > 0) out[i, "Iter"] <- paste0(out[i, "Iter"], "/", sub(";", "", sub("^.*.= ", "", totNoOfIter)))
      suppressWarnings(try(out[i, "RunStatus"] <- substr(sub("\\(s\\)", "", sub("\\*\\*\\* Status: ", "", system(paste0("grep '*** Status: ' ", fulllog), intern = TRUE))), start = 1, stop = 17), silent = TRUE))
      if (onCluster & out[i, "RunStatus"] == "NA") {
        if (out[i, "jobInSLURM"] == FALSE) {
          out[i, "RunStatus"] <- "Run interrupted"
        } else {
          out[i, "RunStatus"] <- "Run in progress"
          }
      } else {
        if (out[i, "RunStatus"] == "NA") out[i, "RunStatus"] <- "Run interrupted"
      }
    } else {
      out[i, "RunStatus"] <- "full.log missing"
    }

    # Conv
    out[i, "Conv"] <- "NA"
    if (exists("cfg"))
    if (file.exists(fulllst)) {
      if (length(out[i, "RunType"]) > 0)

      if (grepl("nash", out[i, "RunType"]) & !is.na(out[i, "RunType"])) {

        if (isTRUE(cfg[["gms"]][["cm_nash_autoconverge"]] > 0)) {
          totNoOfIter <- tail(suppressWarnings(system(paste0("grep 'cm_iteration_max = [1-9].*.;$' ", fulllst), intern = TRUE)), n = 1)
        } else {
          totNoOfIter <- cfg[["gms"]][["cm_iteration_max"]]
        }
        if (exists("totNoOfIter") && any(grepl("cm_iteration_max", cfg))) if (length(totNoOfIter) > 0 & !cfg[["gms"]][["cm_iteration_max"]] > out[i, "Iter"]) out[i, "Iter"] <- paste0(out[i, "Iter"], "/", sub(";", "", sub("^.*.= ", "", totNoOfIter)))

        if (length(suppressWarnings(system(paste0("grep 'Convergence threshold' ", fulllst), intern = TRUE))) > 1) {
          out[i, "Conv"] <- "converged"
        } else if (length(suppressWarnings(system(paste0("grep 'Nash did NOT' ", fulllst), intern = TRUE))) > 1) {
          out[i, "Conv"] <- "not_converged"
        } else {
          iters <- suppressWarnings(system(paste0("grep -A 30 'PARAMETER p80_repy  sum' ", fulllst), intern = TRUE))
          if (length(iters) > 0) {
            iters <- grep("^$|--|modelstat", iters, invert = TRUE, value = TRUE)
            iters <- grep("^[A-Z][A-Z][A-Z]  ", iters,             value = TRUE)
            iters <- tail(sapply(iters, strsplit, split = " "), n = 120)
            regions <- unique(sapply(iters, rem)[1, ])
            b <- paste0(sapply(iters, rem)[3, ], collapse = "")
            iters <- gsub(" |0|\\.", "", b[[1]])
            out[i, "Conv"] <- substr(iters, nchar(iters) - length(regions) + 1, nchar(iters)) # a function is needed that extracts a summary of each iteration, not just the last one
          }
        }
      } else {
        out[i, "Conv"] <- "NA"
      }
    } else {
      out[i,"Conv"] <- "NA" 
    }
    # END Conv

    # Calib Iter
    if (!is.null(out[i, "RunType"])) if (file.exists(logtxt) & grepl("Calib", out[i, "RunType"])) {
      calibiter <- length(suppressWarnings(system(paste0("grep 'CES calibration iteration' ", logtxt), intern = TRUE)))
      if (calibiter > 0) out[i, "Iter"] <- paste0(out[i, "Iter"], " ", "Clb: ", calibiter)
      if (!is.null(out[i, "Conv"])) if (out[i, "Conv"] == "converged" & length(system(paste0("find ", ii, " -name 'input_*.gdx'"), intern = TRUE)) > 10) out[i, "Conv"] <- "Clb_converged"
    }

    # MIF
    out[i,"Mif"] <- "NA"
    if (length(cfgf) != 0) if (file.exists(paste0(ii, "/", cfgf))) {
      if (exists("stats")) if (any(grepl("config", names(stats)))) if (stats[["config"]][["model_name"]] == "MAgPIE") {
        miffile <- paste0(ii, "/validation.mif")
        out[i, "Mif"] <- FALSE
        if (file.exists(miffile)) {
          if (file.info(miffile)[["size"]] > 99999) out[i, "Mif"] <- TRUE
        }
      } else {
        miffile <- paste0(ii, "/REMIND_generic_", cfg[["title"]], ".mif")
        out[i, "Mif"] <- FALSE
        if (file.exists(miffile)) {
          if (file.info(miffile)[["size"]] > 3899999) out[i, "Mif"] <- TRUE
        }
      }
    }

    # Runtime
    out[i, "Runtime"] <- "NA"
    if (file.exists(fle)) {
      load(fle)
      if (exists("stats")) {
        if (any(grepl("GAMSEnd", names(stats)))) {
          out[i, "Runtime"] <- format(round(stats[["timeGAMSEnd"]] - stats[["timeGAMSStart"]], 1))
        } else if (any(grepl("timePrepareStart", names(stats))) & out[i, "RunStatus"] %in% c("Run in progress")) {
          out[i, "Runtime"] <- paste0("> ", format(round(Sys.time() - stats[["timePrepareStart"]], 1)))
        }
      }
    }


  } # END DIR LOOP

  return(out)

}
