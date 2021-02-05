test_that("Report data returns the correct output at the database level", {
  expect_length(report_data(pkg = "qStates", dbase = "states"), 8)
  expect_named(report_data(pkg = "qStates"), c("Unique ID", "Missing data", "Rows", "Columns", "Beg", "End", "Link", "Bibliography"))
})

test_that("Report data retirns the correct output at the dataset level", {
  expect_length(report_data(pkg = "qStates", dbase = "states", dset = "COW"), 8)
  expect_named(report_data(pkg = "qStates"), c("Unique ID", "Missing data", "Rows", "Columns", "Beg", "End", "Link", "Bibliography"))
})

test_that("Report data retirns the correct output at the package level", {
  expect_length(report_data(pkg = "qStates"), 8)
  expect_named(report_data(pkg = "qStates"), c("Unique ID", "Missing data", "Rows", "Columns", "Beg", "End", "Link", "Bibliography"))
})
