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

# expected output data
data.con.con <- dplyr::tibble(qID = c("NZL", "BRA"), 
                                date = c("1990-01-01","1990-01-02"))
data.con.any <- dplyr::tibble(qID = c("NZL","BRA"), 
                              date = c("1990-01-01","1990-01-02"),
                              number = c(100,1000))
data.13.any <- dplyr::tibble(qID = c("NZL","BRA","CHF","OTH"), 
                                date = c("1990-01-01", "1990-01-02","1990-01-01:1990-01-31",NA),
                                number = c(100,1000,10000,21))

test_that("pluck works", {
  expect_equal(pluck(test, "a"), data1)
  expect_equal(pluck(test, "b"), data2)
  expect_equal(pluck(test, "c"), data3)
})

test_that("consolidate works", {
  expect_equal(consolidate(test, "every", "every"), data.con.con)
  expect_equal(consolidate(test, "every", "any"), data.con.any)
  expect_equal(consolidate(test[c(1,3)], "any", "any"), data.13.any)
  expect_equal(consolidate(test, "any"), data.13.any)
})