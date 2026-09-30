remindConfig <- function(...) {
  list(model_name = "REMIND", gms = utils::modifyList(list(optimization = "nash"), list(...)))
}

test_that("runTypeFromConfig composes the REMIND run type", {
  expect_equal(runTypeFromConfig(remindConfig()), "nash")
  expect_equal(runTypeFromConfig(remindConfig(cm_nash_mode = "debug")), "nash debug")
  expect_equal(runTypeFromConfig(remindConfig(cm_nash_mode = 1)), "nash debug")
  expect_equal(runTypeFromConfig(remindConfig(CES_parameters = "calibrate")), "Calib_nash")
  expect_equal(runTypeFromConfig(remindConfig(cm_MAgPIE_coupling = "on")), "nash + mag")
  expect_equal(runTypeFromConfig(remindConfig(cm_MAgPIE_Nash = 1, CES_parameters = "calibrate")), "Calib_nash + mag")
  oneRegi <- function(...) remindConfig(optimization = "testOneRegi", c_testOneRegi_region = "EUR", ...)
  expect_equal(runTypeFromConfig(oneRegi()), "testOneRegi EUR")
  expect_equal(runTypeFromConfig(oneRegi(cm_quick_mode = "on")), "quick EUR")
  expect_equal(runTypeFromConfig(oneRegi(cm_nash_mode = "debug")), "debug EUR")
  expect_equal(runTypeFromConfig(remindConfig(c_empty_model = "on")), "empty model")
})

test_that("runTypeFromConfig returns the optimization of MAgPIE", {
  expect_equal(runTypeFromConfig(list(model_name = "MAgPIE", gms = list(optimization = "nlp_apr17"))), "nlp_apr17")
})

test_that("colRunType reads config.Rdata", {
  dir <- withr::local_tempdir()
  cfg <- remindConfig(CES_parameters = "calibrate")
  save(cfg, file = file.path(dir, "config.Rdata"))
  expect_equal(colRunType(dir), "Calib_nash")
})
