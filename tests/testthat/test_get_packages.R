test_that("get_packages() works properly", {
  expect_error(get_packages(9),
               "Package number not found, please type package name")
  expect_equivalent(get_packages("qStates"), get_packages("globalgov/qStates"))
  expect_type(get_packages(), "NULL")
})
