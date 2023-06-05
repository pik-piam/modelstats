#' colRunType
#'
#' What is the type of this run?
#'
#' @param mydir Path to the folder where the run is performed
#'
#'
#' @author Anastasis Giannousakis
#' @importFrom gms loadConfig
#' @export
colRunType <- function(mydir = ".") {

  cfg <- cfgf <- NULL
  cfgf <- grep("config.Rdata|config.yml", dir(mydir), value = TRUE)
  fulllst <- paste0(mydir, "/full.lst")

  out <- "NA"
  if (file.exists(paste0(mydir, "/", cfgf))) {
    ifelse(grepl("yml",cfgf), cfg <- loadConfig(file.path(mydir, "config.yml")), load(paste0(mydir, "/", cfgf)))
    if (cfg[["model_name"]] == "MAgPIE") {
        out <- cfg$gms$optimization
    } else {
        out <- cfg$gms$optimization
        if (grepl("^testOneRegi", out)) {
          out <- paste(if (isTRUE(cfg$gms$cm_nash_mode == "debug")) "debug" else
                       if (isTRUE(cfg$gms$cm_quick_mode == "on")) "quick" else "1Regi", cfg$gms$c_testOneRegi_region)
        } else {
          if (isTRUE(cfg$gms$cm_nash_mode == "debug")) out <- paste0(out, " debug")
          if (isTRUE(cfg$gms$CES_parameters == "calibrate")) out <- paste0("Calib_", out)
          if (isTRUE(cfg$gms$cm_MAgPIE_coupling == "on")) out <- paste0(out, " + mag")
        }
    }
  } else if (file.exists(fulllst)) {
        out <- sub("         !! def = nash", "", sub("^ .*.ion  ", "", system(paste0("grep 'setGlobal optimization  ' ", fulllst), intern = TRUE)))
        chck <- sub("       !! def = load", "", sub("^ .*.ers  ", "", system(paste0("grep 'setglobal CES_parameters  ' ", fulllst), intern = TRUE)))
        if (chck == "calibrate") out <- paste0("Calib_", out)
  }



  return(out)

}
