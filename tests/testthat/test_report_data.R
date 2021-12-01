test_that("data_source() returns the correct output format", {
  expect_type(data_source("manydata"), "list")
  expect_length(data_source("manydata"), 1)
  expect_named(data_source("manydata"), NULL)
  expect_type(data_source("manydata", "emperors"), "list")
  expect_length(data_source("manydata", "emperors"), 1)
  expect_type(data_source("manydata", "emperors", "wikipedia"), "list")
  expect_length(data_source("manydata", "emperors", "wikipedia"), 1)
})

test_that("data_contrast() returns the correct output format", {
  expect_type(data_contrast("manydata"), "list")
  expect_length(data_contrast("manydata"), 1)
  expect_named(data_contrast("manydata"), "emperors")
  expect_type(data_contrast("manydata", "emperors"), "list")
  expect_length(data_contrast("manydata", "emperors"), 1)
  expect_type(data_contrast("manydata", "emperors", "wikipedia"), "list")
  expect_length(data_contrast("manydata", "emperors", "wikipedia"), 1)
})

test_that("open_codebook() works", {
  expect_error(open_codebook("manydata"), "argument \"database\" is missing, with no default")
  expect_error(open_codebook("manydata", "emperors"), "argument \"dataset\" is missing, with no default")
})
