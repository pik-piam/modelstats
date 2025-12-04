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
  print(paste0(mydir, "/", cfgf))
    ifelse(grepl("yml", cfgf), cfg <- loadConfig(file.path(mydir, "config.yml")), load(paste0(mydir, "/", cfgf)))
    if (cfg[["model_name"]] == "MAgPIE") {
        out <- cfg$gms$optimization
    } else {
      out <- cfg$gms$optimization
      if (grepl("^testOneRegi", out)) {
        out <- paste(if (isTRUE(cfg$gms$cm_nash_mode == "debug" | cfg$gms$cm_nash_mode == 1)) "debug" else
                     if (isTRUE(cfg$gms$cm_quick_mode == "on")) "quick" else "testOneRegi", cfg$gms$c_testOneRegi_region)
      } else {
        if (isTRUE(cfg$gms$cm_nash_mode == "debug" | cfg$gms$cm_nash_mode == 1)) out <- paste0(out, " debug")
        if (isTRUE(cfg$gms$CES_parameters == "calibrate")) out <- paste0("Calib_", out)
        browser()
        if (isTRUE(cfg$gms$cm_MAgPIE_coupling == "on") | isTRUE(cfg$gms$cm_MAgPIE_Nash == 1)) out <- paste0(out, " + mag")
      }
      if (isTRUE(cfg$gms$c_empty_model == "on")) {
        out <- "empty model"
      }
    }
  } else if (file.exists(fulllst)) {
    out <- system(paste0("grep 'setGlobal optimization  ' ", fulllst), intern = TRUE)
    out <- sub("         !! def = nash", "", sub("^ .*.ion  ", "", out))
    chck <- system(paste0("grep 'setglobal CES_parameters  ' ", fulllst), intern = TRUE)
    chck <- sub("       !! def = load", "", sub("^ .*.ers  ", "", chck))
    if (chck == "calibrate") out <- paste0("Calib_", out)
  }

  return(out)
}
