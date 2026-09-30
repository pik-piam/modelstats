#' colRunType
#'
#' What is the type of this run? For REMIND the optimization mode
#' (`nash`, `negishi`, `testOneRegi <region>`) decorated with the calibration,
#' debug, quick and coupling settings; for MAgPIE the optimization setting.
#'
#' @param mydir Path to the folder where the run is performed
#' @return a string, `"NA"` if the type cannot be determined
#'
#' @author Anastasis Giannousakis
#' @importFrom gms loadConfig
#' @export
colRunType <- function(mydir = ".") {
  configName <- grep("config.Rdata|config.yml", dir(mydir), value = TRUE)
  fullLst <- paste0(mydir, "/full.lst")

  runType <- "NA"
  if (file.exists(paste0(mydir, "/", configName))) {
    cfg <- NULL
    isYaml <- grepl("yml", configName)
    if (any(isYaml)) cfg <- loadConfig(file.path(mydir, "config.yml"))
    if (any(!isYaml)) cfg <- loadRdata(paste0(mydir, "/", configName))$cfg
    runType <- runTypeFromConfig(cfg)
  } else if (file.exists(fullLst)) {
    # fallback for a run without a config file; see known-bugs.md for why it is unreachable
    runType <- system(paste0("grep 'setGlobal optimization  ' ", fullLst), intern = TRUE)
    runType <- sub("         !! def = nash", "", sub("^ .*.ion  ", "", runType))
    cesParameters <- system(paste0("grep 'setglobal CES_parameters  ' ", fullLst), intern = TRUE)
    cesParameters <- sub("       !! def = load", "", sub("^ .*.ers  ", "", cesParameters))
    if (cesParameters == "calibrate") runType <- paste0("Calib_", runType)
  }
  runType
}

# The run type from a run configuration (cfg) of REMIND or MAgPIE.
runTypeFromConfig <- function(cfg) {
  if (cfg[["model_name"]] == "MAgPIE") return(cfg$gms$optimization)
  gms <- cfg$gms
  runType <- gms$optimization
  isDebug <- isTRUE(gms$cm_nash_mode == "debug" | gms$cm_nash_mode == 1)
  if (grepl("^testOneRegi", runType)) {
    mode <- if (isDebug) "debug" else if (isTRUE(gms$cm_quick_mode == "on")) "quick" else "testOneRegi"
    runType <- paste(mode, gms$c_testOneRegi_region)
  } else {
    if (isDebug) runType <- paste0(runType, " debug")
    if (isTRUE(gms$CES_parameters == "calibrate")) runType <- paste0("Calib_", runType)
    if (isTRUE(gms$cm_MAgPIE_coupling == "on") || isTRUE(gms$cm_MAgPIE_Nash == 1)) runType <- paste0(runType, " + mag")
  }
  if (isTRUE(gms$c_empty_model == "on")) runType <- "empty model"
  runType
}
