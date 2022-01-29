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
        out <- cfg[["gms"]][["optimization"]]
    } else {
        out <- cfg[["gms"]][["optimization"]]
        if (!is.null(cfg$gms$CES_parameters)) if (cfg[["gms"]][["CES_parameters"]] == "calibrate") out <- paste0("Calib_", out)
    }
  } else if (file.exists(fulllst)) {
        out <- sub("         !! def = nash", "", sub("^ .*.ion  ", "", system(paste0("grep 'setGlobal optimization  ' ", fulllst), intern = TRUE)))
        chck <- sub("       !! def = load", "", sub("^ .*.ers  ", "", system(paste0("grep 'setglobal CES_parameters  ' ", fulllst), intern = TRUE)))
        if (chck == "calibrate") out <- paste0("Calib_", out)
  }



  return(out)

}
