# input data
data1 <- dplyr::tibble(many_ID = c("NZL", "BRA", "CHF"),
                       date = c("1990-01-01", "1990-01-02",
                                "1990-01-01:1990-01-31"),
                       number = c(100, 1000, 10000))
data2 <- dplyr::tibble(many_ID = c("NZL", "BRA"),
                       date = c("1990-01-01", "1990-01-03"))
data3 <- dplyr::tibble(many_ID = c("NZL", "BRA", "CHF", "OTH"),
                       date = c("1990-01-01", "1990-01-02",
                                "1990-01-01:1990-01-31", NA),
                       number = c(100, 1200, 12222, 21))
test <- tibble::lst(a = data1, b = data2, c = data3)

# expected output data
data.con.con <- dplyr::tibble(many_ID = c("NZL", "BRA"),
                                date = c("1990-01-01",
                                         "1990-01-02"))
data.con.any <- dplyr::tibble(many_ID = c("NZL", "BRA"),
                              date = c("1990-01-01",
                                       "1990-01-02"),
                              number = c(100, 1000))
data.13.any <- dplyr::tibble(many_ID = c("NZL", "BRA", "CHF", "OTH"),
                                date = c("1990-01-01", "1990-01-02",
                                         "1990-01-01:1990-01-31", NA),
                                number = c(100, 1000, 10000, 21))
data.dup <- dplyr::tibble(many_ID = c("NZL", "BRA", "CHF", "OTH",
                                  "NZL", "BRA", "CHF", "OTH"),
                          date = c("1990-01-01", "1990-01-02",
                                   "1990-01-01:1990-01-31", NA,
                                   "1990-01-01", "1990-01-02",
                                   "1990-01-01:1990-01-31", NA),
                          number = c(100, 1000, 10000, 21,
                                     100, 1000, 10000, 21))
data.con.min <- dplyr::tibble(many_ID = c("NZL", "BRA"),
                              date = c("1990-01-01",
                                       "1990-01-02"),
                              number = c("100", "1000"))
data.con.max <- dplyr::tibble(many_ID = c("NZL", "BRA"),
                              date = c("1990-01-01",
                                       "1990-01-03"),
                              number = c("100", "1200"))
data.con.median <- dplyr::tibble(many_ID = c("NZL", "BRA"),
                              date = c("1990-01-01",
                                       "1990-01-02"),
                              number = c("100", "1100"))

# input data
dat1 <- dplyr::tibble(many_ID = c("NZL", "BRA", "CHF"),
                      date = messydates::as_messydate(c("1990-01-01", "1990-01-02",
                                                        "1990-01-01:1990-01-31")),
                      number = c(100, 1000, 10000))
dat2 <- dplyr::tibble(many_ID = c("NZL", "BRA"),
                      date = messydates::as_messydate(c("1990-01-01", "1990-01-03")))
dat3 <- dplyr::tibble(many_ID = c("NZL", "BRA", "CHF", "OTH"),
                      date = messydates::as_messydate(c("1990-01-01", "1990-01-02",
                                                        "1990-01-01:1990-01-31", NA)),
                       number = c(100, 1200, 12222, 21))
test2 <- tibble::lst(a = dat1, b = dat2, c = dat3)



data.multi <- dplyr::tibble(many_ID = c("NZL", "BRA", "CHF", "OTH"),
                            date = c("1990-01-01", "1990-01-02", "1990-01-01", NA),
                            number = c("100", "1200", "12222", "21"))

data.many <- dplyr::tibble(many_ID = c("NZL", "BRA", "CHF", "OTH"),
                           number = c("100", "1100", "11111", "21"),
                           date = c("1990-01-01", "1990-01-02", "1990-01-01..1990-01-31", NA))

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
  expect_length(consolidate(test, "every", "any", resolve = "random"), 3)
  expect_equal(consolidate(test2, "every", "any", resolve = "min"), data.con.min)
  expect_equal(consolidate(test2, "every", "any", resolve = "max"), data.con.max)
  expect_equal(consolidate(test2, "every", "any", resolve = "median"), data.con.median)
  expect_equal(consolidate(test2, "every", "any", resolve = "mean"), data.con.median)
  expect_length(consolidate(test2, "every", "any", resolve = "random"), 3)
  expect_equal(consolidate(test2, "any", "any", resolve = c(date = "min", number = "max")), data.multi)
  expect_equal(consolidate(test2, "any", "any", resolve = c(date = "mean", number = "median")), data.many)
  expect_length(consolidate(test2, "any", "any", resolve = c(date = "coalesce", number = "random")), 3)
})
