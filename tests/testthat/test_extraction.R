membs <- tibble::tibble(CountryID = c("ROU", "RUS", "DNK"),
                        manyID = c("ROU-RUS[RFP]_1901A", "ROU-RUS[RFP]_1901A", "GD16FI_1901A"),
                        Title = c("Convention Between Roumania And Russia Concerning Fishing",
                                  "Convention Between Roumania And Russia Concerning Fishing",
                                  "Convention Between The Governments Of Denmark And ..."),
                        Beg = c("1901-02-22", "1901-02-22", "1901-06-24"),
                        End = c(NA, NA, NA))
bilats <- tibble::tibble(CountryID1 = "RUS", CountryID2 = "ROU",
                          Title = "Convention Between Roumania And Russia Concerning Fishing",
                          Beg = "1901-02-22", End = NA)
multi <- tibble::tibble(manyID = "GD16FI_1901A",
                        Title = "Convention Between The Governments Of Denmark And ...",
                        Beg = "1901-06-24", End = NA)

test_that("extract functions work", {
  expect_equal(extract_bilaterals(membs), bilats)
  expect_equal(extract_multilaterals(membs), multi)
})
