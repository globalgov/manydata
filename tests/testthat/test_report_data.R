test_that("data_source() returns the correct output format at the package
          level", {
    expect_type(data_source(pkg = "qStates"), "list")
          })

test_that("data_source() returns the correct output format at the database
          level", {
  expect_type(data_source(pkg = "qStates",
                          database = "states"), "list")
          })

test_that("data_source() returns the correct output format at the dataset
          level", {
  expect_type(data_source(pkg = "qStates",
                          database = "states",
                          dataset = "GW"), "list")
          })

test_that("data_contrast() returns the correct output format at the package
          level", {
  expect_type(data_contrast(pkg = "qStates"), "list")
})

test_that("data_contrast() returns the correct output format at the database
          level", {
  expect_type(data_contrast(pkg = "qStates",
                            database = "states"), "list")
})

test_that("data_contrast() returns the correct output format at the dataset
          level", {
  expect_type(data_contrast(pkg = "qStates",
                            database = "states",
                            dataset = "GW"), "list")
})
