test_that("Report data returns the correct output at the database level", {
  expect_length(report_data(pkg = "qStates", database = "states"), 21)
})

test_that("Report data returns the correct output at the dataset level", {
  expect_length(report_data(pkg = "qStates", database = "states", dataset = "COW"), 6)
})

test_that("Report data returns the correct output at the package level", {
  expect_length(report_data(pkg = "qStates"), 21)
})
