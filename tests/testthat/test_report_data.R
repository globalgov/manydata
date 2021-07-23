test_that("data_source() returns the correct output format", {
  expect_type(data_source("qData"), "list")
  expect_type(data_source("qData", "emperors"), "list")
  expect_type(data_source("qData", "emperors", "wikipedia"), "list")
})

test_that("data_contrast() returns the correct output format", {
  expect_type(data_contrast("qData"), "list")
  expect_type(data_contrast("qData", "emperors"), "list")
  expect_type(data_contrast("qData", "emperors", "wikipedia"), "list")
})
