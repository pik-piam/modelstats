# Generates the small R data files of this directory. Run from the repository root:
#   Rscript tests/data/make_data.R
# Only base R is needed. Re-run after changing anything here and commit the results.
outdir <- "tests/data"
Sys.setenv(TZ = "Europe/Berlin")

# config.Rdata of a REMIND nash run (object 'cfg', the fields getRunStatus/colRunType read)
cfg <- list(
  title = "SSP2-NPi-AMT", model_name = "REMIND",
  results_folder = "output/SSP2-NPi-AMT_2026-10-18_10.00.00",
  remind_folder = "/p/projects/remind/modeltests/remind",
  path_magpie = NULL,
  gms = list(optimization = "nash", cm_nash_mode = "parallel", cm_quick_mode = "off",
             CES_parameters = "load", cm_MAgPIE_coupling = "off", cm_MAgPIE_Nash = 0,
             c_empty_model = "off", cm_iteration_max = 50, cm_nash_autoconverge = 1,
             c_testOneRegi_region = "EUR", regions = c("EUR", "USA"), flag = TRUE,
             unset = NA, unsetstr = NA_character_)
)
save(cfg, file = file.path(outdir, "config_remind.Rdata"))

# runstatistics.rda of a finished REMIND run (object 'stats')
stats <- list(
  id = "abc123",
  config = list(model_name = "REMIND", title = "SSP2-NPi-AMT"),
  timePrepareStart = as.POSIXct("2026-03-28 10:00:00", tz = "Europe/Berlin"),
  timeGAMSStart = as.POSIXct("2026-03-28 10:05:00", tz = "Europe/Berlin"),
  timeGAMSEnd = as.POSIXct("2026-03-29 12:05:30", tz = "Europe/Berlin"),   # across the DST switch
  runtime = as.difftime(26.5, units = "hours"),
  modelstat = 2,
  versionGMS = "r",
  pkg = utils::packageDescription("base")
)
save(stats, file = file.path(outdir, "runstatistics_remind.rda"))

# runstatistics.rda of a MAgPIE run: the modelstat is a magpie array (class 'magpie')
stats <- list(
  id = "mag42",
  config = list(model_name = "MAgPIE"),
  timePrepareStart = as.POSIXct("2026-03-28 10:00:00", tz = "Europe/Berlin"),
  modelstat = structure(array(c(2, 2, 7), dim = c(1, 1, 3),
                              dimnames = list("GLO", "NA", c("y1995", "y2000", "y2005"))),
                        class = "magpie")
)
# real magclass objects name their dimnames c("", "t", "") - rdata cannot turn that into coordinates (BUG-044)
names(dimnames(stats$modelstat)) <- c("", "t", "")
save(stats, file = file.path(outdir, "runstatistics_magpie.rda"))

# an xz-compressed runstatistics.rda, as the real ones are written
stats <- list(id = "xz", config = list(model_name = "REMIND"), modelstat = 2)
save(stats, file = file.path(outdir, "runstatistics_xz.rda"), compress = "xz")

# a run that took 3 hours (for the AMT runtime comparison) and one whose config has no model_name (BUG-056)
stats <- list(id = "short", config = list(model_name = "REMIND", title = "SSP2-NPi-AMT"),
              timeGAMSStart = as.POSIXct("2026-03-21 10:00:00", tz = "Europe/Berlin"),
              timeGAMSEnd = as.POSIXct("2026-03-21 13:00:00", tz = "Europe/Berlin"), modelstat = 2)
save(stats, file = file.path(outdir, "runstatistics_short.rda"))
stats <- list(id = "nomodel", config = list(title = "SSP2-NPi-AMT"), modelstat = 2)
save(stats, file = file.path(outdir, "runstatistics_nomodel.rda"))

# runstatistics.rda with timeGAMSEnd but without timeGAMSStart (BUG-016)
stats <- list(id = "nostart", config = list(model_name = "REMIND"),
              timeGAMSEnd = as.POSIXct("2026-03-29 12:05:30", tz = "Europe/Berlin"))
save(stats, file = file.path(outdir, "runstatistics_nostart.rda"))

saveRDS(".*-AMT_2026-10-18|.*-AMT_2026-10-19", file.path(outdir, "runcode.rds"))
saveRDS("abc1234", file.path(outdir, "lastcommit.rds"))
saveRDS(list(ScenarioMIP = list(missingVars = 3L, checkSummations = 0L, checkSummationsRegional = NA)),
        file.path(outdir, "projectSummations.rds"))
# entries missing altogether (NULL in R, BUG-019)
saveRDS(list(ScenarioMIP = list(checkSummations = 1L)), file.path(outdir, "projectSummations_null.rds"))
runsToStart <- data.frame(start = c(1, 1), row.names = c("SSP2-NPi-AMT", "SSP2-PkBudg1000-AMT"))
saveRDS(runsToStart, file.path(outdir, "runsToStart.rds"))
gRS <- data.frame(Conv = c("converged", "not_converged"), Mif = c("yes", "no"),
                  row.names = c("SSP2-NPi-AMT_2026-09-01_10.00.00", "SSP2-NPi-AMT_2026-09-08_10.00.00"))
saveRDS(gRS, file.path(outdir, "gRS.rds"))

# a data frame with NA cells of every type, as gRS.rds of a running AMT has them (BUG: none, _rdata rule)
gRS_na <- data.frame(Conv = c("converged", NA), Runtime = c(90030, NA), ok = c(TRUE, NA),
                     when = as.POSIXct(c("2026-03-28 10:05:00", NA), tz = "Europe/Berlin"),
                     row.names = c("run1", "run2"), stringsAsFactors = FALSE)
saveRDS(gRS_na, file.path(outdir, "gRS_na.rds"))
# a data.frame subclass (tibble-like class vector) must still become a data frame
tib <- structure(list(a = c(1, 2), b = c("x", "y")), class = c("tbl_df", "tbl", "data.frame"), row.names = c(NA, -2L))
saveRDS(tib, file.path(outdir, "tibble.rds"))

# a grab-bag of R value types for the _rdata conversion tests
named <- c(a = 1, b = NA)
types <- list(
  nullvalue = NULL, string = "x", strings = c("x", "y"), number = 2, integer = 3L, flag = TRUE,
  na_number = NA_real_, na_integer = NA_integer_, na_logical = NA, na_string = NA_character_,
  named = named, named_one = c(a = 1), named_chr = c(a = "x", b = "y"),
  matrix = array(1:6, dim = c(2, 3)), fac = factor(c("b", "a", "b")),
  dupnames = array(1:4, dim = c(2, 2), dimnames = structure(list(c("a", "b"), c("x", "y")), names = c("r", "r"))),
  day = as.Date("2026-10-18"), nested = list(1, "two"), secs = as.difftime(90, units = "mins"),
  stamp_notz = as.POSIXct("2026-03-29 03:30:00"), stamp_utc = as.POSIXct("2026-03-29 03:30:00", tz = "UTC"),
  empty = character(0)
)
save(types, file = file.path(outdir, "types.rda"))
