#' Provide a command line interface to loopRuns and getSanityChecks
#' 
#' This function is made to be called by the rs tool. It takes the command line arguments
#' supplied by the user when calling rs, evaluates them, and according to them prepares the 
#' arguments finally passed to loopRuns or getSanityChecks.
#' 
#' @param argv A character vector containing the command line arguments.
#' @returns A string containing the status or sanity table.
#' @importFrom optparse make_option OptionParser parse_args
#' @author David Klein
#' @export

commandLineInterface <- function(argv) {

  # =============================================
  #   Define internal functions
  # =============================================

  # return TRUE if dir is a REMIND or MAgPIE run folder
  is.runfolder <- function(dir) {
    return(sum(file.exists(paste0(dir, "/", c("full.gms", "log.txt", "config.Rdata", "prepare_and_run.R", "prepareAndRun.R")))) >= 4 ||
           sum(file.exists(paste0(dir, "/", c("full.gms", "submit.R", "config.yml", "magpie_y1995.gdx")))) == 4
    )
  }

  # return TRUE if dir is the output folder
  is.outputfolder <- function(dir) {
    return(basename(normalizePath(dir)) == "output")
  }

  # return TRUE if dir is the REMIND or MAgPIE main folder
  is.mainfolder <- function(dir) {
    return(sum(file.exists(paste0(dir, "/", c("output", "output.R", "start.R", "main.gms")))) == 4)
  }

  # ================================================
  #  Define command line arguments, help, and hints
  # ================================================

  option_list <- list(
    make_option(c("-A", "--amt"     ), type = "logical", action = "store_true", default = FALSE, help = "print most recent REMIND automated model test runs. With -f: of all AMTs, show only those that match the regular expression REGEX -f REGEX"),
    make_option(c("-b", "--nocolor" ), type = "logical", action = "store_true", default = FALSE, help = "black&white: print table without colors"),
    make_option(c("-C", "--current" ), type = "logical", action = "store_true", default = FALSE, help = "print currently running runs no matter where they are, i.e. you don't need to be in or right above a run to see it. Add user with -u USER and include recent runs with -d N"),
    make_option(c("-d", "--daysback"), type = "integer",                        default = 0    , help = "with -c: show recent runs of the last N days", metavar = "N"),
    make_option(c("-f", "--filter"  ), type = "character",                      default = ".*" , help = "print runs that match the regular expression REGEX. Specify multiple REGEX separated by comma.", metavar = "REGEX"),
    make_option(c("-l", "--last"    ), type = "logical", action = "store_true", default = FALSE, help = "with -m: print the last coupling iterations only"),
    make_option(c("-m", "--magpie"  ), type = "logical", action = "store_true", default = FALSE, help = "print all coupling iterations. Add -l to print the last iterations only"),
    make_option(c("-p", "--prompt"  ), type = "logical", action = "store_true", default = FALSE, help = "let the user choose individual runs from a list before printing the status table" ),
    make_option(c("-s", "--sanity"  ), type = "logical", action = "store_true", default = FALSE, help = "show overview of sanity check" ),
    make_option(c("-t", "--time"    ), type = "logical", action = "store_true", default = FALSE, help = "sort runs chronologically" ),
    make_option(c("-u", "--user"    ), type = "character",                      default = "you", help = "with -c: show runs of user USER", metavar = "USER")
  )

  usage <- c(
    "\n rs [PATH] [OPTION]\n")

  description <- c(
    "  [PATH] is optional and can be any comma separated combination of individual run folders, the main folder, or the output folder.",
    "  [OPTION] see below.\n\nDescription:\n",
    " Print run status or sanity of model runs.",
    "  Flags in capital letters list runs from special locations and ignore any path provided.",
    "  Short flags may be bundled together, sharing a single leading -, but only the final short flag is able to have a corresponding argument.",
    "  The following flags only take effect if used together with other flag:",
    "    -u, -d only with -C\n",
    "   -l     only with -m",
    "  The following flags can be combined with all other flags:",
    "    -b, -f, -p, -t\n\nExamples:",
    "  1. rs /p/projects/remind/runs/REMIND-MAgPIE-2025-04-24/remind/ -mltpbf PkBudg",
    "     From the given path list coupled runs (m) and only last iteration (l), order by time (t), prompt me to select the runs (p), print in black&white (b), filter (f) by 'PkBudg'.",
    "  2. rs -As\n",
    "    Show sanity checks (s) for lastest AMTs (A).",
    "  3. rs -C -u alf -d 5",
    "     Show run status of current runs (-C) from the user alf (-u) of the last 5 days (-d 5).")

  epilogue <- c(
    "Bugs and feedback: https://github.com/pik-piam/modelstats/issues")
    
  hints <- c(
    "Show (l)ast iterations of (m)agpie-coupled runs with: rs -ml",
    "Show all (m)agpie-coupled runs with: rs -m",
    "Show your runs (C)urrently running with: rs -C",
    "Show your (C)urrent runs from the last 5 (d)ays with: rs -C -d 5",
    "Sort runs by (t)ime of last change with: rs -t",
    "List runs found and (p)rompt me to select for which the status should be printed with: rs -p",
    "Show results from specific folders with: rs folder1,folder2",
    "(f)ilter runs by regular expression: rs -f PkBudg500,EU21",
    "Remove the coloring and print the table in (b)lack and white with -b.",
    "Show (C)urrent runs for one or more (u)sers with: rs -C -u user1,user2",
    "Show a (h)elp text with: rs -h",
    "To understand why your pending runs don't start, run: sq -s",
    "To get info about the current run output folder, simply run: rs",
    "List most recent (A)utomated model tests with: rs -A",
    "To get info about a specific AMT scenario, run: rs -A -f SSP2EU-Base",
    "Get a more detailed assessment of a specific run: remindstatus folder")

  # create the parser object
  opt_parser <- OptionParser(usage = usage, option_list = option_list, description = description, epilogue = epilogue) # , formatter = TitledHelpFormatter

  # parse the arguments, allow for one optional positional argument that takes the paths
  arguments <- parse_args(opt_parser, args = argv, positional_arguments = c(0,1))

  # print hint
  message("Did you know? ", sample(hints, 1))

  # retrieve options (flags) and positional arguments (paths)
  opt  <- arguments$options
  paths <- arguments$args

  # set default for user
  if (opt$user == "you") opt$user <- Sys.info()[["user"]]

  # set default for paths
  if (length(paths) < 1) paths <- "."

  # split comma separated parameters into vectors
  paths <- strsplit(paths, ',')[[1]]
  opt$filter <- paste0(strsplit(opt$filter, ",")[[1]], collapse = "|")

  # =============================================
  #            Main decision tree:
  #        Evaluate command line arguments
  #           and decide what to do
  # =============================================

  # AMT runs: hardcode AMT path and use regular expression from 'runode.rds' for filtering the latest AMTs
  if (opt$amt) {
    paths <- "/p/projects/remind/modeltests/remind/output/"
    cat("Results from", paths, "\n")
    if (opt$filter == ".*") opt$filter <- readRDS("/p/projects/remind/modeltests/remind/runcode.rds")
    opt$user <- NULL
  }

  if (opt$current) {
    # OPTION A: get current runs (code recycled from promptAndRun)
    myruns   <- system(paste0("squeue -u ", opt$user, " -h -o '%Z'"), intern = TRUE)
    runnames <- system(paste0("squeue -u ", opt$user, " -h -o '%j'"), intern = TRUE)

    if (opt$daysback > 0) {
      sacctcode <- paste0("sacct -u ", opt$user, " -s cd,f,cancelled,timeout,oom -S ", as.Date(format(Sys.Date(), "%Y-%m-%d")) - as.numeric(opt$daysback), " -E now -P -n")
      myruns   <- c(myruns,   system(paste(sacctcode, "--format WorkDir"), intern = TRUE))
      runnames <- c(runnames, system(paste(sacctcode, "--format JobName"), intern = TRUE))
    }

    if (any(grepl("mag-run", runnames))) {
      deleteruns <- which(runnames %in% c("default", "batch"))
    } else {
      deleteruns <- which(runnames %in% c("batch"))
    }
    if (length(deleteruns) > 0) {
      myruns <- myruns[-deleteruns]
      runnames <- runnames[-deleteruns]
    }

    if (length(myruns) == 0) {
      message(paste0("No runs found for this user and time horizon of ", opt$daysback, " days"))
      quit(save = 'no', status = 0)
    }
    # add REMIND-MAgPIE coupled runs where run directory is not the output directory
    # these lines also drop all other slurm jobs such as remind preprocessing etc.
    coupled <- rem <- NULL
    for (i in 1:length(runnames)) {
      if (! any(grepl(runnames[[i]], myruns[[i]]), grepl("mag-run", runnames[[i]]))) {
        coupled <- c(coupled, paste0(myruns[[i]], "/output/", runnames[[i]])) # for coupled runs in parallel mode
        rem <- c(rem, i)
      }
    }
    if (!is.null(rem)) {
      myruns <- myruns[-rem] # remove coupled parent-job and all other slurm jobs
      myruns <- c(myruns, coupled) # add coupled paths
    }
    myruns <- myruns[file.exists(myruns)] # keep only existing paths
    myruns <- sort(unique(myruns[!is.na(myruns)]))

    if (length(myruns) == 0) {
      message("No runs found for this user. To change the reporting period (days) you need to specify also a user, e.g. rs2 -c USER 1")
      quit(save = 'no', status = 0)
    }
    runfolders <- myruns
    
  } else {
    # OPTION B: create list with run folders from path supplied by user or AMTs
    runfolders <- NULL
    unidentified <- NULL

    for (dir in paths) {
      if(is.runfolder(dir)) {
        runfolders <- c(runfolders, dir)
      } else if (is.outputfolder(dir)) {
        runfolders <- c(runfolders, list.dirs(dir, recursive = FALSE))
      } else if (is.mainfolder(dir)) {
        runfolders <- c(runfolders, list.dirs(file.path(dir, "output"), recursive = FALSE))
      } else {
        unidentified <- c(unidentified, dir)
      }
    }
    
    # report unidentified folders
    if(!is.null(unidentified)) {
      message("No runs found in the following folders:")
      print(unidentified)
    }
  }

  # filter coupling iterations
  if (opt$magpie & !is.null(runfolders)) {
    # add magpie run folders if 'magpie/output' exists inside current folder
    if (dir.exists(file.path("magpie", "output"))) runfolders <- c(runfolders, file.path("magpie", "output"))
    
    # find iterations using only the basename (to ignore rem|mag if they exist in the path before the basename)
    IndexOfcoupledRuns <- grepl("-(rem|mag)-[0-9]+$", basename(runfolders))
    runfolders <- runfolders[IndexOfcoupledRuns]
    
    # keep last iteration only
    if (opt$last) {
      lastdirs <- NULL
      for (r in unique(gsub("-(rem|mag)-[0-9]+$", "", runfolders))) {
        lastdirs <- c(lastdirs, runfolders[max(which(gsub("-(rem|mag)-[0-9]+$", "", runfolders) == r))])
      }
    }
    runfolders <- lastdirs
  }
   
  # =============================================
  #   print table (runstatus or sanity)
  # =============================================

  if(!is.null(runfolders)) {

    # print hint how to reduce number of runs
    if (length(runfolders) > 40) message("To reduce the number of runs, filter the runs with -f REGEX or select manually from the list with -p.")
    
    # filter runs. If not changed by the user the default pattern '.*' filters all
    runfolders <- grep(opt$filter, runfolders, value = TRUE)

    # list all runs found and prompt the user to select
    if (opt$prompt) {
      runfolders <- gms::chooseFromList(runfolders, type = "folders") 
    }

    # decide whether to display sanity cheks or runstatus  
    if (opt$sanity) {
      modelstats::getSanityChecks(runfolders, prompt = FALSE)
    } else {
      modelstats::loopRuns(runfolders, user = opt$user, colors = !opt$nocolor, sortbytime = opt$time)
    }
  } else {
    message("No runs found")
  }
}