test_that("agreements data is properly formatted", {
  expect_type(agreements, "list")
  expect_length(agreements, 3)
})