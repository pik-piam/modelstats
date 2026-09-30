test_that("foundInSlurm reports the state of the matching job", {
  root <- withr::local_tempdir()
  run <- file.path(root, "output", "myrun")
  dir.create(run, recursive = TRUE)
  squeue <- paste0("echo 'me ", run, " myrun 0:42 RUNNING priority'; ",
                   "echo 'you /elsewhere/other other 5:00:00 RUNNING short'")
  fakeBinDir(squeue = squeue)
  expect_equal(foundInSlurm(run, user = "me"), "priority startup")
  expect_equal(foundInSlurm(run, user = "you"), "me startup")
  expect_equal(foundInSlurm(file.path(root, "output", "nothing"), user = "me"), "no")
})

test_that("foundInSlurm recognises pending jobs and several jobs", {
  root <- withr::local_tempdir()
  run <- file.path(root, "output", "myrun")
  dir.create(run, recursive = TRUE)
  fakeBinDir(squeue = paste0("echo 'me ", run, " myrun 0:00 PENDING priority'"))
  expect_equal(foundInSlurm(run, user = "me"), "priority pending")

  fakeBinDir(squeue = paste0("echo 'a ", run, " myrun 1:00 RUNNING x'; echo 'b ", run, " myrun 1:00 RUNNING x'"))
  expect_equal(foundInSlurm(run, user = "me"), "2 users")
})
