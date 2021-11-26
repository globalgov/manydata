test_that("data_source() returns the correct output format", {
  expect_type(data_source("manydata"), "list")
  expect_type(data_source("manydata", "emperors"), "list")
  expect_type(data_source("manydata", "emperors", "wikipedia"), "list")
})

test_that("data_contrast() returns the correct output format", {
  expect_type(data_contrast("manydata"), "list")
  expect_type(data_contrast("manydata", "emperors"), "list")
  expect_type(data_contrast("manydata", "emperors", "wikipedia"), "list")
})
