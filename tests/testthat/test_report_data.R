test_that("data_source() returns the correct output format", {
  expect_type(data_source("qData"), "list")
  expect_type(data_source("qData", "states"), "list")
  expect_type(data_source("qData", "states", "COW"), "list")
})

test_that("data_contrast() returns the correct output format", {
  expect_type(data_contrast("qData", "states"), "list")
  expect_type(data_contrast("qData", "states", "COW"), "list")
  expect_invisible(contrast <- data_contrast("qData", "states", "COW"))
})
