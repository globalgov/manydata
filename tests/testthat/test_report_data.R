# Set up "qStates" for testing
get_packages("qStates")
pkg <- "qStates"
database <- "states"
dataset <- "COW"

test_that("data_source() returns the correct output format at the package
          level", {
    expect_type(data_source(pkg), "list")
          })

test_that("data_source() returns the correct output format at the database
          level", {
  expect_type(data_source(pkg, database), "list")
          })

test_that("data_source() returns the correct output format at the dataset
          level", {
  expect_type(data_source(pkg, database, dataset), "list")
          })

test_that("data_contrast() returns the correct output format at the package
          level", {
  expect_type(data_contrast(pkg), "list")
})

test_that("data_contrast() returns the correct output format at the database
          level", {
  expect_type(data_contrast(pkg, database), "list")
})

test_that("data_contrast() returns the correct output format at the dataset
          level", {
  expect_type(data_contrast(pkg, database, dataset), "list")
})
