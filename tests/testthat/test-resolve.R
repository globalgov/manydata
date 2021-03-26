test_that("resolve works", {
  test <- tibble(var = list("1990-01-01",c("1990-01-01","1990-01-02","1990-01-02","1990-01-03")))
  expect_equal(resolve_min(test$var), c("1990-01-01","1990-01-01"))
  expect_equal(resolve_max(test$var), c("1990-01-01","1990-01-03"))
  expect_equal(resolve_mean(test$var), c("1990-01-01","1990-01-02"))
  expect_equal(resolve_median(test$var), c("1990-01-01","1990-01-02"))
  expect_equal(resolve_mode(test$var), c("1990-01-01","1990-01-02"))
})

test_that("resolve_dates() returns date output correctly", {
  dates6 <- data.frame(date = c("30/4/1990", "NA", "2010-12-30", "Obsolete?", "2010-00-00", "2599-01-01"))
  sdate <- standardise_dates(dates6$date)
  expect_equal(as.character(resolve_dates(sdate, type = "min")), c("1990-04-30", NA, "2010-12-30", NA, "2010-01-01", "9999-12-31"))
  expect_equal(as.character(resolve_dates(sdate, type = "max")), c("1990-04-30", NA, "2010-12-30", NA, "2010-12-31", "9999-12-31"))
  expect_equal(as.character(resolve_dates(sdate, type = "mean")), c("1990-04-30", NA, "2010-12-30", NA, "2010-07-02", "9999-12-31"))
  sdate <- resolve_dates(sdate, "max")
  expect_true(lubridate::is.Date(sdate))
})

test_that("resolve treats date ranges correctly", {
  test <- tibble(var = c("2010-01-01:2010-12-31", "1995-10-30", "1816-01-01", "1816-01-01:1916-01-01", NA))
  expect_equal(resolve_min(test$var), c("2010-01-01", "1995-10-30", "1816-01-01", "1816-01-01", NA))
  expect_equal(resolve_max(test$var), c("2010-12-31", "1995-10-30", "1816-01-01", "1916-01-01", NA))
  expect_equal(resolve_mean(test$var), c("2010-02-07", "1995-10-30", "1816-01-01", "1866-01-01", NA))
  expect_equal(resolve_median(test$var), c("2010-02-07", "1995-10-30", "1816-01-01", "1866-01-01", NA))
  expect_equal(resolve_mode(test$var), c("2010-02-07", "1995-10-30", "1816-01-01", "1866-01-01", NA))
})