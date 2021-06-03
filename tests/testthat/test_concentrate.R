# input data
data1 <- dplyr::tibble(qID = c("NZL", "BRA", "CHF"), 
                       date = c("1990-01-01","1990-01-02","1990-01-01:1990-01-31"),
                       number = c(100, 1000, 10000))
data2 <- dplyr::tibble(qID = c("NZL", "BRA"), 
                       date = c("1990-01-01","1990-01-03"))
data3 <- dplyr::tibble(qID = c("NZL", "BRA", "CHF", "OTH"), 
                       date = c("1990-01-01","1990-01-02","1990-01-01:1990-01-31", NA),
                       number = c(100, 1200, 12222, 21))
test <- tibble::lst(a = data1, b = data2, c = data3)

test_that("pluck works", {
  expect_equal(pluck(test, "a"), data1)
  expect_equal(pluck(test, "b"), data2)
  expect_equal(pluck(test, "c"), data3)
})

