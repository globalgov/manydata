a <- get_packages()
test_that("get_packages() works properly", {
  expect_type(a, "NULL")
  expect_length(a, 0)
})
