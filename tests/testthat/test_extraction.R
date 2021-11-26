membs <- data.frame(qID_ref = c("ROU-RUS[RFP]_1901A","ROU-RUS[RFP]_1901A", "GD16FI_1901A", "GD16FI_1901A"),
                    CountryID = c("ROU", "RUS", "DNK", "GBR"),
                    Title = c("Convention Between Roumania And Russia Concerning Fishing In The Danube And The Pruth",
                              "Convention Between Roumania And Russia Concerning Fishing In The Danube And The Pruth",
                              "Convention Between The Governments Of Denmark And The United Kingdom Of Great Britain And Northern Ireland For Regulating The Fisheries Of Their Respective Subjects Outside Territorial Waters In The Ocean Surrounding The Faroe Islands",
                              "Convention Between The Governments Of Denmark And The United Kingdom Of Great Britain And Northern Ireland For Regulating The Fisheries Of Their Respective Subjects Outside Territorial Waters In The Ocean Surrounding The Faroe Islands"),
                    Beg = c("1901-02-22", "1901-02-22", "1901-06-24", "1901-06-24"),
                    End = c(NA, NA, NA, NA))

bilats <- data.frame(CountryID1 = "RUS",
                     CountryID2 = "ROU",
                     Title = "Convention Between Roumania And Russia Concerning Fishing In The Danube And The Pruth",
                     Beg = "1901-02-22",
                     End = NA)

test_that("extraction function works", {
  expect_equal(extract_bilaterals(membs), bilats)
})
