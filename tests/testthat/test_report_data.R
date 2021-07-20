test_that("data_source() returns the correct output format", {
  get_packages("qStates")
  pkg <- "qStates"
  database <- "states"
  dataset <- "COW"
  expect_type(data_source(pkg), "list")
  expect_type(data_source(pkg, database), "list")
  expect_type(data_source(pkg, database, dataset), "list")
})

test_that("data_contrast() returns the correct output format", {
  get_packages("qStates")
  pkg <- "qStates"
  database <- "states"
  dataset <- "COW"
  expect_type(data_contrast(pkg), "list")
  expect_type(data_contrast(pkg, database), "list")
  expect_type(data_contrast(pkg, database, dataset), "list")
})
