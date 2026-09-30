test_that("printOutput pads and truncates the columns to their widths", {
  row <- data.frame(RunStatus = "Normal completion", Conv = "converged", Runtime = 12, row.names = "run-1")
  out <- printOutput(row, lenCols = c(10, 5, 8, 4), colSep = "|", cols = c("RunStatus", "Conv", "Runtime"))
  expect_equal(out, "run-1     |Norma|converge|12  \n")
})

test_that("printOutput blanks real NA and keeps the string NA", {
  row <- data.frame(RunStatus = NA, Conv = "NA", row.names = "r")
  expect_equal(printOutput(row, lenCols = c(3, 4, 4), colSep = " ", cols = c("RunStatus", "Conv")), "r        NA  \n")
})

test_that("printOutput without widths uses 13, 14, ... from the last column and no separator", {
  row <- data.frame(RunStatus = "a", Conv = "b", row.names = "r")
  out <- printOutput(row, len1stcol = 5, colSep = "-", cols = c("RunStatus", "Conv"))
  expect_equal(out, paste0("r    -", "a", strrep(" ", 13), "b", strrep(" ", 12), "\n"))
})

test_that("printOutput fails for columns that are not in the table", {
  row <- data.frame(RunStatus = "a", row.names = "r")
  expect_error(printOutput(row, cols = c("RunStatus", "Conv")), "undefined columns")
})

test_that("an empty table gives an empty string", {
  expect_equal(printOutput(data.frame()), "")
})
