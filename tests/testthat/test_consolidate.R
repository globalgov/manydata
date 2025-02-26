# input data
data1 <- dplyr::tibble(manyID = c("NZL", "BRA", "CHF"),
                       date = c("1990-01-01", "1990-01-02",
                                "1990-01-01:1990-01-31"),
                       number = c(100, 1000, 10000))
data2 <- dplyr::tibble(manyID = c("NZL", "BRA"),
                       date = c("1990-01-01", "1990-01-03"))
data3 <- dplyr::tibble(manyID = c("NZL", "BRA", "CHF", "OTH"),
                       date = c("1990-01-01", "1990-01-02",
                                "1990-01-01:1990-01-31", NA),
                       number = c(100, 1200, 12222, 21))
test <- list(a = data1, b = data2, c = data3)

dat1 <- dplyr::tibble(manyID = c("NZL", "BRA", "CHF"),
                      date = messydates::as_messydate(c("1990-01-01",
                                                        "1990-01-02",
                                                        "1990-01-01:1990-01-31")),
                      number = c(100, 1000, 10000))
dat2 <- dplyr::tibble(manyID = c("NZL", "BRA"),
                      date = messydates::as_messydate(c("1990-01-01",
                                                        "1990-01-03")))
dat3 <- dplyr::tibble(manyID = c("NZL", "BRA", "CHF", "OTH"),
                      date = messydates::as_messydate(c("1990-01-01",
                                                        "1990-01-02",
                                                        "1990-01-01:1990-01-31",
                                                        NA)),
                      number = c(100, 1200, 12222, 21))
test2 <- list(a = dat1, b = dat2, c = dat3)

# expected output data
data.con.con <- dplyr::tibble(manyID = c("NZL", "BRA"),
                                date = c("1990-01-01",
                                         "1990-01-02"))
data.favour <- dplyr::tibble(manyID = c("NZL", "BRA"),
                             date = c("1990-01-01",
                                      "1990-01-03"))
data.con.any <- dplyr::tibble(manyID = c("NZL", "BRA"),
                              date = c("1990-01-01",
                                       "1990-01-02"),
                              number = c(100, 1000))
data.13.any <- dplyr::tibble(manyID = c("NZL", "BRA", "CHF", "OTH"),
                                date = c("1990-01-01", "1990-01-02",
                                         "1990-01-01:1990-01-31", NA),
                                number = c(100, 1000, 10000, 21))
data.dup <- dplyr::tibble(manyID = c("NZL", "BRA", "CHF", "OTH",
                                  "NZL", "BRA", "CHF", "OTH"),
                          date = c("1990-01-01", "1990-01-02",
                                   "1990-01-01:1990-01-31", NA,
                                   "1990-01-01", "1990-01-02",
                                   "1990-01-01:1990-01-31", NA),
                          number = c(100, 1000, 10000, 21,
                                     100, 1000, 10000, 21))
data.con.min <- dplyr::tibble(manyID = c("NZL", "BRA"),
                              date = c("1990-01-01",
                                       "1990-01-02"),
                              number = c(100, 1000))
data.con.max <- dplyr::tibble(manyID = c("NZL", "BRA"),
                              date = c("1990-01-01",
                                       "1990-01-03"),
                              number = c(100, 1200))
data.con.median <- dplyr::tibble(manyID = c("NZL", "BRA"),
                              date = c("1990-01-01",
                                       "1990-01-02"),
                              number = c(100, 1100))
data.multi <- dplyr::tibble(manyID = c("NZL", "BRA", "CHF", "OTH"),
                            date = c("1990-01-01", "1990-01-02",
                                     "1990-01-01", NA),
                            number = c(100, 1200, 12222, 21))
data.many <- dplyr::tibble(manyID = c("NZL", "BRA", "CHF", "OTH"),
                           number = c(100, 1100, 11111, 21),
                           date = c("1990-01-01", "1990-01-02",
                                    "1990-01-16", NA))

test_that("pluck works", {
  expect_equal(pluck(test, "a"), data1)
  expect_equal(pluck(test, "b"), data2)
  expect_equal(pluck(test, "c"), data3)
})

test_that("consolidate methods work", {
  expect_equal(consolidate(test, "inner", "every",
                           resolve = "coalesce"),
               data.con.con[order(data.con.con$manyID),])
  expect_equal(consolidate(test, "inner", "any",
                           resolve = "coalesce"),
               data.con.any[order(data.con.any$manyID),])
  expect_equal(consolidate(test, "full", "any",
                           resolve = "coalesce"),
               data.13.any[order(data.13.any$manyID),])
  expect_equal(consolidate(test, "full", resolve = "coalesce"),
               data.13.any[order(data.13.any$manyID),])
  expect_equal(consolidate(datacube = test, rows = "inner", cols = "any",
                           resolve = "min"),
               data.con.min[order(data.con.min$manyID),])
  expect_equal(consolidate(test, "inner", "any",
                           resolve = "max"),
               data.con.max[order(data.con.max$manyID),])
  expect_equal(consolidate(test, "inner", "any",
                           resolve = "median"),
               data.con.median[order(data.con.median$manyID),])
  expect_equal(consolidate(datacube = test, join = "inner", cols = "any",
                           resolve = "mean"),
                 data.con.median[order(data.con.median$manyID),])
  expect_length(consolidate(test, "inner", "any",
                            resolve = "random"), 3)
  expect_equal(unname(unlist(lapply(consolidate(
    datacube = test2, join = "inner", cols = "any", resolve = "min"), class))),
               c("character", "Date", "numeric"))
  expect_equal(unname(unlist(lapply(consolidate(
    datacube = test2, join = "inner", cols = "any", resolve = "max"), class))),
    c("character", "Date", "numeric"))
  expect_equal(dim(consolidate(test2, "inner", "any",
                           resolve = "median")), c(2,3))
  expect_equal(dim(consolidate(datacube = test2, join = "inner", cols = "any",
                           resolve = "mean")), c(2,3))
  expect_length(consolidate(test2, "inner", "any",
                            resolve = "random"), 3)
  expect_equal(consolidate(datacube = test2, join = "full", cols = "any",
                           resolve = c(date = "min", number = "max")),
               data.multi[order(data.multi$manyID),])
  expect_equal(consolidate(test2, "full", "any",
                           resolve = c(date = "mean", number = "median")),
               data.many[order(data.many$manyID),])
  expect_length(consolidate(test2, "full", "any",
                            resolve = c(date = "coalesce",
                                        number = "random")), 3)
  expect_length(consolidate(test2, "full", "any",
                            resolve = "coalesce", key = c("manyID", "date")), 3)
})

test_that("favouring a dataset works", {
  expect_equal(consolidate(favour(test, "b"), "inner", "every",
                           resolve = "coalesce"),
               data.favour[order(data.favour$manyID),])
  expect_equal(consolidate(favor(test, "b"), "inner", "every",
                           resolve = "coalesce"),
               data.favour[order(data.favour$manyID),])
  expect_equal(consolidate(favor(test, c("c", "a", "b")), "inner", "every",
                           resolve = "coalesce"),
               data.favour[order(data.favour$manyID),])
})
