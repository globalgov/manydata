test_that("collapse works", {
  data1 <- data.frame(ID = c("NZL", "BRA", "CHF"), date = c("1990-01-01","1990-01-02","1990-01-01:1990-01-31"))
  data2 <- data.frame(ID = c("NZL", "BRA"), date = c("1990-01-01","1990-01-02"))
  data3 <- data.frame(ID = c("NZL", "BRA", "CHF", "OTH"), date = c("1990-01-01","1990-01-02","1990-01-01:1990-01-31", NA))
  test <- tibble::lst(a = data1, b = data2, c = data3)
  expect_equal(collapse_select(test, "a"), data1)
  test1 <- data.frame(ID = c("NZL", "BRA", "CHF", "OTH"), date.x = c("1990-01-01","1990-01-02","1990-01-01:1990-01-31", NA),
                      date.y = c("1990-01-01","1990-01-02", NA, NA), date = c("1990-01-01","1990-01-02","1990-01-01:1990-01-31", NA))
  expect_equal(collapse_full(test, "ID"), test1)
  test2 <- data.frame(ID = c("NZL", "BRA"), date.x = c("1990-01-01","1990-01-02"),
                      date.y = c("1990-01-01","1990-01-02"), date = c("1990-01-01","1990-01-02"))
  expect_equal(collapse_consensus(test, "ID"), test2)
  test3 <- data.frame(ID.x = c("NZL", "BRA"), date = c("1990-01-01","1990-01-02"),
                      ID.y = c("NZL", "BRA"), ID = c("NZL", "BRA"))
  expect_equal(collapse_consensus(test, "date"), test3)
  test4 <- data.frame(ID.x = c("NZL", "BRA", "CHF", NA), date = c("1990-01-01","1990-01-02","1990-01-01", NA),
                             ID.y = c("NZL", "BRA", NA, NA), ID = c("NZL", "BRA", "CHF", "OTH"))
  expect_equal(collapse_full(test, "date", resolve = "min"), test4)
})
