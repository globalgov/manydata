test_that("Report data returns the correct output at the database level", {
  expect_length(report_data(pkg = "qStates", dbase = "states"), 24)
})

test_that("Report data returns the correct output at the dataset level", {
  expect_length(report_data(pkg = "qStates", dbase = "states", dset = "COW"), 8)
})

test_that("Report data returns the correct output at the package level", {
  expect_length(report_data(pkg = "qStates"), 24)
})
