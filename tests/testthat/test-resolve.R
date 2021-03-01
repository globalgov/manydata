test_that("resolve works", {
  test <- tibble(var = list("1990-01-01",c("1990-01-01","1990-01-02","1990-01-02","1990-01-03")))
  expect_equal(resolve_min(test$var), c("1990-01-01","1990-01-01"))
  expect_equal(resolve_max(test$var), c("1990-01-01","1990-01-03"))
  expect_equal(resolve_mean(test$var), c("1990-01-01","1990-01-02"))
  expect_equal(resolve_median(test$var), c("1990-01-01","1990-01-02"))
  expect_equal(resolve_mode(test$var), c("1990-01-01","1990-01-02"))
})
