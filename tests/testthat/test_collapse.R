test_that("collapse works", {
  data1 <- dplyr::tibble(ID = c("NZL", "BRA", "CHF"), 
                         date = c("1990-01-01","1990-01-02","1990-01-01:1990-01-31"))
  data2 <- dplyr::tibble(ID = c("NZL", "BRA"), 
                         date = c("1990-01-02","1990-01-03"))
  data3 <- dplyr::tibble(ID = c("NZL", "BRA", "CHF", "OTH"), 
                         date = c("1990-01-01","1990-01-02","1990-01-01:1990-01-31", NA))
  test <- tibble::lst(a = data1, b = data2, c = data3)
  # Passes :)
  expect_equal(collapse_select(test, "a"), data1)
  test1 <- dplyr::tibble(ID = c("NZL", "BRA", "CHF", "OTH"), 
                         date.x = c("1990-01-01","1990-01-02","1990-01-01:1990-01-31", NA),
                         date.y = c("1990-01-02","1990-01-03", NA, NA),
                         date = c("1990-01-01","1990-01-02","1990-01-01:1990-01-31", NA))
  expect_equal(collapse_full(test, "ID"), test1)
  # Passes :)
  test2 <- dplyr::tibble(ID = c("NZL", "BRA"), 
                         date.x = c("1990-01-01","1990-01-02"),
                         date.y = c("1990-01-02","1990-01-03"),
                         date = c("1990-01-01","1990-01-02"))
  expect_equal(collapse_consensus(test, "ID"), test2)
  # Passes :)
  test3 <- dplyr::tibble(ID.x = c("BRA"), date = c("1990-01-02"),
                      ID.y = c("NZL"), ID = c("BRA"))
  expect_equal(collapse_consensus(test, "date"), test3)
  # We join on date to test the resolve implementation for uncertain keys. 
  # This will not often be used in practice though.
  test4 <- dplyr::tibble(ID.x = c("NZL", "NZL", "BRA", "CHF", "CHF", NA, NA),
                          date = c("1990-01-01","1990-01-01","1990-01-02", "1990-01-01","1990-01-01","1990-01-03",NA),
                          ID.y = c(NA, NA, "NZL", NA, NA, "BRA", NA), 
                         ID = c("NZL", "CHF", "BRA", "NZL", "CHF", NA, "OTH"))
  expect_equal(collapse_full(test, "date", resolve = "min"), test4)
  # Does it make sense to join on date?
  # Passes :)
})

# For now the resolve option only resolves on the key variable by design. We might want to expand this.