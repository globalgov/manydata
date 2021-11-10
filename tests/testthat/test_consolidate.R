# input data
data1 <- dplyr::tibble(qID = c("NZL", "BRA", "CHF"),
                       date = c("1990-01-01", "1990-01-02",
                                "1990-01-01:1990-01-31"),
                       number = c(100, 1000, 10000))
data2 <- dplyr::tibble(qID = c("NZL", "BRA"),
                       date = c("1990-01-01", "1990-01-03"))
data3 <- dplyr::tibble(qID = c("NZL", "BRA", "CHF", "OTH"),
                       date = c("1990-01-01", "1990-01-02",
                                "1990-01-01:1990-01-31", NA),
                       number = c(100, 1200, 12222, 21))
test <- tibble::lst(a = data1, b = data2, c = data3)

# expected output data
data.con.con <- dplyr::tibble(qID = c("NZL", "BRA"),
                                date = c("1990-01-01",
                                         "1990-01-02"))
data.con.any <- dplyr::tibble(qID = c("NZL", "BRA"),
                              date = c("1990-01-01",
                                       "1990-01-02"),
                              number = c(100, 1000))
data.13.any <- dplyr::tibble(qID = c("NZL", "BRA", "CHF", "OTH"),
                                date = c("1990-01-01", "1990-01-02",
                                         "1990-01-01:1990-01-31", NA),
                                number = c(100, 1000, 10000, 21))
data.dup <- dplyr::tibble(qID = c("NZL", "BRA", "CHF", "OTH",
                                  "NZL", "BRA", "CHF", "OTH"),
                          date = c("1990-01-01", "1990-01-02",
                                   "1990-01-01:1990-01-31", NA,
                                   "1990-01-01", "1990-01-02",
                                   "1990-01-01:1990-01-31", NA),
                          number = c(100, 1000, 10000, 21,
                                     100, 1000, 10000, 21))
data.con.min <- dplyr::tibble(qID = c("NZL", "BRA"),
                              date = c("1990-01-01",
                                       "1990-01-02"),
                              number = c("100", "1000"))
data.con.max <- dplyr::tibble(qID = c("NZL", "BRA"),
                              date = c("1990-01-01",
                                       "1990-01-03"),
                              number = c("100", "1200"))
data.con.median <- dplyr::tibble(qID = c("NZL", "BRA"),
                              date = c("1990-01-01",
                                       "1990-01-02"),
                              number = c("100", "1100"))

test_that("pluck works", {
  expect_equal(pluck(test, "a"), data1)
  expect_equal(pluck(test, "b"), data2)
  expect_equal(pluck(test, "c"), data3)
})

test_that("coalesce_compatible works", {
  expect_equal(coalesce_compatible(data.13.any), data.13.any)
  expect_equal(coalesce_compatible(data.dup), data.13.any)
})

test_that("consolidate methods", {
  expect_equal(consolidate(test, "every", "every", resolve = "coalesce"), data.con.con)
  expect_equal(consolidate(test, "every", "any", resolve = "coalesce"), data.con.any)
  expect_equal(consolidate(test[c(1, 3)], "any", "any", resolve = "coalesce"), data.13.any)
  expect_equal(consolidate(test, "any", resolve = "coalesce"), data.13.any)
  expect_equal(consolidate(test, "every", "any", resolve = "min"), data.con.min)
  expect_equal(consolidate(test, "every", "any", resolve = "max"), data.con.max)
  expect_equal(consolidate(test, "every", "any", resolve = "median"), data.con.median)
  expect_equal(consolidate(test, "every", "any", resolve = "mean"), data.con.median)
})
