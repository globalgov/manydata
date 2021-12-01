test_that("get_packages() works properly", {
  skip_on_os("mac")
  a <- get_packages()
  expect_type(a, "NULL")
  expect_length(a, 0)
})
