#!/usr/bin/env Rscript
# Generates the tiny R data files used by tests/unit/test_config.py, test_rdata_io.py and
# test_runstats.py, together with JSON files holding what R computed for them (so that the
# unit tests never need R).  Run from anywhere:  Rscript tests/unit/data/make_data.R
# Needs R >= 4.3 with gms, yaml and jsonlite; magclass is optional (MAgPIE modelstat object).
suppressPackageStartupMessages({ library(jsonlite) })
Sys.setenv(TZ = "Europe/Berlin")
here <- normalizePath(dirname(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))))
out <- function(name) file.path(here, name)
berlin <- function(s) as.POSIXct(s, tz = "Europe/Berlin")
notz <- function(x) { attr(x, "tzone") <- NULL; x }

# --- 1. config.Rdata: a nested cfg like REMIND writes it -----------------------------------
cfg <- list(
  title = "testOneRegi", model_name = "REMIND", description = "unit test config",
  gms = list(optimization = "nash", cm_nash_mode = 1L, cm_quick_mode = "off", c_testOneRegi_region = "EUR",
             CES_parameters = "load", cm_MAgPIE_coupling = "off", cm_MAgPIE_Nash = 0, c_empty_model = "off",
             cm_iteration_max = 100L, cm_nash_autoconverge = "1", cm_abortOnConsecFail = "2",
             a_null = NULL, a_na = NA, a_true = TRUE, a_named = c(a = 1, b = 2), a_chars = c("x", "y"),
             a_int = 5L, a_dbl = 2.5, a_chr_na = c("a", NA), a_empty = character(0), a_na_real = NA_real_,
             a_utf8 = "Ärger"),
  path_magpie = "/p/magpie", cfg_mag = list(results_folder = "output/:title::date:"),
  results_folder = "output/:title::date:", remind_folder = "/p/remind")
save(cfg, file = out("config.Rdata"))
other <- 1
save(other, file = out("nocfg.Rdata"))

# --- 2. config.yml written by gms::saveConfig (namedVector and character tags) --------------
# Stored with a .txt suffix: the repository's pre-commit check-yaml hook (a plain YAML 1.1 safe
# loader) rejects the gms tags.  The tests copy it to <tmp>/config.yml before loading it.
cfgm <- list(
  title = "default", model_name = "MAgPIE", model = "main.gms",
  input = c(regional = "rev4.135_h12_magpie.tgz", cellular = "rev4.135_cellularmagpie.tgz"),
  repositories = list("https://example.org/public" = NULL, "/p/projects/landuse/data/input/archive" = NULL),
  force_download = TRUE, recalibrate = FALSE, calib_accuracy = 0.05, calib_maxiter = 20.0, qos = "standby",
  gms = list(optimization = "nlp_apr17", c_timesteps = "coup2110", s15_elastic_demand = 0L,
             nothing = character(0), nv = c(a = 1, b = 2), mixed = list(1L, "a"), big = 1000000.0,
             policy_countries = c("DEU", "FRA")),
  results_folder = "output/:title::date:")
gms::saveConfig(cfgm, out("config.yml.txt"))

# --- 3. runstatistics.rda variants -------------------------------------------------------------
stats <- list(
  user = "tester", date = berlin("2026-09-28 10:33:10"),
  config = list(model_name = "REMIND", title = "testOneRegi", gms = list(optimization = "nash")),
  id = "179059272175832", modelstat = 2,
  timePrepareStart = berlin("2026-09-28 10:33:10"),            # tzone Europe/Berlin
  timeGAMSStart = notz(berlin("2026-09-28 10:33:35")),         # no tzone attribute
  timeGAMSEnd = berlin("2026-09-28 12:51:59.4"),               # fractional seconds
  timeOutputStart = as.POSIXct(NA),                            # POSIXct NA
  runtime = as.difftime(2.306602, units = "hours"),
  setup_info = NULL, revision = "abc")
save(stats, file = out("runstatistics.rda"))

stats <- list(user = "tester", config = list(model_name = "MAgPIE", title = "default", gms = list(optimization = "nlp_apr17")),
              id = "178978767733374", modelstat = c(2, 2, NA, 2, 13, 2),
              timePrepareStart = berlin("2026-09-19 04:28:50"), timeGAMSStart = berlin("2026-09-19 04:36:56"),
              timeGAMSEnd = berlin("2026-09-19 05:14:36"), runtime = as.difftime(37.67155, units = "mins"))
save(stats, file = out("runstatistics_vec.rda"))

if (requireNamespace("magclass", quietly = TRUE)) {
  # getExportedValue() rather than magclass::new.magpie: magclass is not in DESCRIPTION and the
  # repository's deps-in-desc pre-commit hook checks every pkg::fun token in .R files.
  newMagpie <- getExportedValue("magclass", "new.magpie")
  m <- newMagpie("GLO", paste0("y", c(1995, 2000, 2005, 2010)), "main", c(2, 2, NA, 13))
  attr(m, "description") <- "modelstat indicator (1)"
  stats <- list(user = "tester", config = list(model_name = "MAgPIE", title = "default"), id = "178978767733374",
                modelstat = m, timeGAMSStart = berlin("2026-09-19 04:36:56"), timeGAMSEnd = berlin("2026-09-19 05:14:36"))
  save(stats, file = out("runstatistics_magpie.rda"))
  cat("magpie modelstat as.character:", paste0(as.character(m), collapse = ","), "\n")
}

stats <- list(user = "tester", config = list(title = "noname"), id = "1", timePrepareStart = berlin("2026-09-19 04:28:50"))
save(stats, file = out("runstatistics_noname.rda"))
foo <- "no stats object here"
save(foo, file = out("nostats.rda"))

# --- 4. rds files of the shapes the port reads -------------------------------------------------
saveRDS("hello world", out("rds_string.rds"))
df <- data.frame(chr = c("a", NA, "c"), num = c(1.5, NA, 3), int = c(1L, NA, 3L), lgl = c(TRUE, NA, FALSE),
                 row.names = c("r1", "r2", "r3"), stringsAsFactors = FALSE)
saveRDS(df, out("rds_df.rds"))
saveRDS(list(ScenarioMIP = list(missingVars = 2L, checkSummations = 32L, checkSummationsRegional = 0L),
             other = list(a = NA, b = NULL, c = c(1, 2))), out("rds_nested.rds"))
saveRDS(NULL, out("rds_null.rds"))
saveRDS(c("a", NA, "c"), out("rds_chr_na.rds"))
saveRDS(c(TRUE, NA, FALSE), out("rds_lgl_na.rds"))
systime <- Sys.time()
saveRDS(systime, out("rds_systime.rds"))
saveRDS(c(1, NA, NaN), out("rds_num_na_nan.rds"))

# --- 5. originals for the write_rds round trip (the four AMT state shapes) ---------------------
saveRDS(".*-AMT_2026-09-28|.*-AMT_2026-09-29", out("rt_runcode.rds"))
saveRDS("3f5e2a1b9c8d7e6f5a4b3c2d1e0f9a8b7c6d5e4f", out("rt_lastcommit.rds"))
grs <- data.frame(
  jobInSLURM = c("no", "no", "no"), RunType = c("NA", "nash", "Calib_nash"),
  modelstat = c("NA", "2: Locally Optimal", "2: Locally Optimal"), runInAppResults = c("no", "yes", "no"),
  Mif = c("NA", "sumErr", "yes"), Iter = c("NA", "100/100", "28/100 Clb: 10"),
  RunStatus = c("full.log missing", "Normal completion", "Normal completion"), Warnings = c("NA", "27", "3"),
  Conv = c("NA", "not_converged", "converged"), Runtime = c(NA, 77679, 59943),
  summationErrors = c(NA, 1, 0), rangeErrors = c(NA, 0, 0), fixErrors = c(NA, 5489, 0),
  missingProjVars = c(NA, 5L, 72L), projSummationErrors = c(NA, 37L, 14L), projSummationErrorsRegional = c(NA, 61L, 0L),
  row.names = c("archive", "SSP2-EU21-PkBudg650-AMT_2026-09-28_13.54.59", "SSP2-NPi-AMT_2026-08-28_22.06.57"),
  stringsAsFactors = FALSE)
saveRDS(grs, out("rt_grs.rds"))
rts <- data.frame(
  start = c("calibrate,AMT,compileInTests,calibrateSSP2", "1,AMT,2", "1,AMT,2"),
  CES_parameters = c("calibrate", NA, NA), slurmConfig = c(14L, NA, NA), regionmapping = c(NA, NA, NA_character_),
  cm_rcp_scen = c(NA, "rcp26", "rcp37"), cm_startyear = c(NA, 2030L, 2030L), path_gdx = c(NA, NA, NA),
  regipol = c(NA, NA, NA), cm_so2tax_scen = c(NA, 4L, 4L), c_changeProdCost = c(NA, "1", "1"),
  description = c("SSP2-NPi2025-calibrate-AMT: calibration run", "SSP2-NDC-LTS-pf-AMT: a run", "SSP2-NDC-LTS-my-AMT: another run"),
  row.names = c("SSP2-NPi2025-calibrate-AMT", "SSP2-NDC-LTS-pf-AMT", "SSP2-NDC-LTS-my-AMT"), stringsAsFactors = FALSE)
stopifnot(identical(sapply(rts, class)[["path_gdx"]], "logical"), identical(sapply(rts, class)[["regipol"]], "logical"))
saveRDS(rts, out("rt_runstostart.rds"))

# --- 6. DST table: what R's difftime(end, start, units = "secs") gives ---------------------------
utc <- function(s) as.POSIXct(s, tz = "UTC")
retz <- function(x, tz) { attr(x, "tzone") <- tz; x }
base <- berlin("2026-09-30 12:00:00")
cases <- list(
  list(id = "spring_forward_berlin", start = berlin("2026-03-29 01:30:00"), end = berlin("2026-03-29 03:30:00")),
  list(id = "spring_forward_wide", start = berlin("2026-03-29 00:30:00"), end = berlin("2026-03-29 04:30:00")),
  list(id = "spring_forward_notz", start = notz(berlin("2026-03-29 01:30:00")), end = notz(berlin("2026-03-29 03:30:00"))),
  list(id = "spring_forward_utc_tz", start = utc("2026-03-29 01:30:00"), end = utc("2026-03-29 03:30:00")),
  list(id = "fall_back_berlin", start = retz(utc("2026-10-24 23:30:00"), "Europe/Berlin"), end = retz(utc("2026-10-25 01:30:00"), "Europe/Berlin")),
  list(id = "fall_back_wide", start = berlin("2026-10-24 22:00:00"), end = berlin("2026-10-25 04:00:00")),
  list(id = "fall_back_notz", start = notz(utc("2026-10-24 23:30:00")), end = notz(utc("2026-10-25 01:30:00"))),
  list(id = "mixed_tz", start = utc("2026-10-24 23:30:00"), end = berlin("2026-10-25 02:30:00")),
  list(id = "across_both", start = berlin("2026-03-01 12:00:00"), end = berlin("2026-11-01 12:00:00")),
  list(id = "na_start", start = as.POSIXct(NA), end = base),
  list(id = "na_end", start = base, end = as.POSIXct(NA)),
  list(id = "na_both", start = as.POSIXct(NA), end = as.POSIXct(NA)),
  list(id = "zero", start = base, end = base),
  list(id = "negative", start = base, end = base - 90),
  list(id = "half_0.5", start = base, end = base + 0.5),
  list(id = "half_1.5", start = base, end = base + 1.5),
  list(id = "half_2.5", start = base, end = base + 2.5),
  list(id = "half_3.5", start = base, end = base + 3.5),
  list(id = "frac_0.49", start = base, end = base + 0.49),
  list(id = "frac_0.51", start = base, end = base + 0.51),
  list(id = "neg_half_2.5", start = base, end = base - 2.5),
  list(id = "long_run", start = berlin("2026-09-28 10:33:35"), end = berlin("2026-09-28 12:51:59.4")))
dst <- lapply(cases, function(k) {
  d <- difftime(k$end, k$start, units = "secs")
  c(k, list(secs = as.numeric(d), rounded = as.numeric(round(d, 0)),
            tz_start = if (is.null(attr(k$start, "tzone"))) "" else attr(k$start, "tzone"),
            tz_end = if (is.null(attr(k$end, "tzone"))) "" else attr(k$end, "tzone")))
})
save(dst, file = out("dst.rda"))

# --- 7. YAML scalar typing of R's yaml package (through gms::loadConfig) -------------------------
scalars <- c("100", "-3", "1.5", "1e5", "1.0e+5", ".5", "0x1A", "017", "1_000", "yes", "Yes", "YES", "y", "Y", "n", "N",
             "no", "NO", "on", "On", "off", "OFF", "true", "True", "TRUE", "false", "False", "FALSE", "~", "null",
             "Null", "NULL", ".inf", "+.inf", "-.Inf", ".INF", "inf", "Inf", "Infinity", ".nan", ".NaN", ".NAN", "nan",
             "NaN", "NA", "na", "1:30", "1:30:00", "12:30:00", "2026-09-30", "2026-09-30T12:00:00Z",
             "2026-09-30 12:00:00", "nash", "12345678901234567890", "1e400", "1.0e400", "1e+5", "1E+5", "1e-5",
             "1.5e3", "1.5E-3", "1.5e+3", "1.", "0.", "3.", "00", "01", "007", "0777", "08", "09", "-08", "0b101",
             "+12", "-0", "+0", "0", "-1", "12_3", "1,5", "0x", "0xZ", "0o17", "-0x1A", "+0x1A", "-017", "1.5e",
             "1e", ".", "+", "1.0E5", "1.0e5", ".5e3", "+.5", "-.5", "1.5.5", "1_0.5", "0x1_A", "1e3.5", "-.",
             "1.5f", "0.5.", ".e5", "1.e5", "1.E+5", "12345678901", "2147483647", "2147483648", "-2147483648",
             "-2147483647", "1.7976931348623157e308", "1.0e309", "0.0", "-0.0", "0x7FFFFFFF", "0x80000000",
             "0177777777777", "1,5.0", "1.5,0", "5e", "e5", "a b", "''", "\"1\"", "", "  ", "0.1e+1000", "1.0e-5")
keys <- sprintf("s%03d", seq_along(scalars))
lines <- c(paste0(keys, ": ", scalars),
           "a_list: [1, 2, 3]", "a_mixed: [1, a]", "a_floats: [1.5, 2]", "a_strs: [a, b]", "a_bools: [yes, no]",
           "a_nv: !<namedVector> {x: 1, y: 2}", "a_nvmix: !<namedVector> {x: 1, y: a}",
           "a_nvbool: !<namedVector> {x: yes, y: 2}", "a_nvnull: !<namedVector> {x: ~, y: 2}",
           "a_nvfloat: !<namedVector> {x: 1, y: 2.5}", "a_nvstr: !<namedVector> {regional: rev4.tgz, cellular: cell.tgz}",
           "a_chr: !<character> []", "a_chr2: !<character> [a, b]", "a_chr3: !<character> 5", "a_chr4: !<character> [1, 2.5, yes]",
           "a_nested: {b: {c: 1}}", "a_empty_map: {}", "a_empty_seq: []", "a_base: &base {x: 1, y: 2}",
           "a_merge: {<<: *base, y: 3, z: 4}", "a_seq_of_maps: [{a: 1}, {b: 2}]", "a_int_seq_na: [1, 2, 99999999999]", "a_multi: |", "  line one", "  line two", "a_folded: >-", "  folded", "  text")
writeLines(lines, out("yaml_scalars.yml.txt"))
describe <- function(x) {
  if (is.null(x)) return(list(class = "NULL"))
  if (is.list(x)) return(list(class = "list", names = if (is.null(names(x))) NULL else as.list(names(x)),
                              value = lapply(unname(x), describe)))
  val <- if (is.numeric(x)) lapply(x, function(v) if (is.na(v) && !is.nan(v)) NULL else if (is.nan(v)) "NaN" else if (is.infinite(v)) (if (v > 0) "Inf" else "-Inf") else v)
         else lapply(x, function(v) if (is.na(v)) NULL else v)
  list(class = class(x)[1], names = if (is.null(names(x))) NULL else as.list(names(x)), value = val)
}
y <- suppressWarnings(gms::loadConfig(out("yaml_scalars.yml.txt")))
expect <- lapply(y, describe)
expect[["__scalars__"]] <- as.list(setNames(scalars, keys))
writeLines(toJSON(expect, auto_unbox = TRUE, null = "null", na = "null", digits = NA, pretty = TRUE), out("yaml_scalars.json"))

# --- 8. what R says about the files above (for the Python tests) ----------------------------------
rd <- function(p) { e <- new.env(); load(p, envir = e); e$stats }
rs <- rd(out("runstatistics.rda"))
expected <- list(
  systime_epoch = sprintf("%.6f", as.numeric(systime)),
  runstatistics = list(names = names(rs), id = rs$id, model_name = rs$config$model_name,
                       timePrepareStart = sprintf("%.6f", as.numeric(rs$timePrepareStart)),
                       timeGAMSStart = sprintf("%.6f", as.numeric(rs$timeGAMSStart)),
                       timeGAMSEnd = sprintf("%.6f", as.numeric(rs$timeGAMSEnd)),
                       runtime_secs = as.numeric(rs$runtime, units = "secs"),
                       gams_secs = as.numeric(difftime(rs$timeGAMSEnd, rs$timeGAMSStart, units = "secs")),
                       gams_rounded = as.numeric(round(difftime(rs$timeGAMSEnd, rs$timeGAMSStart, units = "secs"), 0))),
  modelstat_vec = paste0(as.character(rd(out("runstatistics_vec.rda"))$modelstat), collapse = ""),
  r_version = R.version.string)
if (file.exists(out("runstatistics_magpie.rda")))
  expected$modelstat_magpie <- paste0(as.character(rd(out("runstatistics_magpie.rda"))$modelstat), collapse = "")
writeLines(toJSON(expected, auto_unbox = TRUE, null = "null", digits = NA, pretty = TRUE), out("expected.json"))
cat("done:", R.version.string, "\n")
