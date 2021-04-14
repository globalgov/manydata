data <- data.frame(title = c("Agreement Between Cape Verde And Portugal On Fisheries Development", 
                              "Traité De Délimitation Maritime, Signé À Paris Le 30 Janvier 1981",
                              "Convention On Wetlands Of International Importance Especially As Waterfowl Habitat",
                              "Protocol To Amend The Convention On Wetlands Of International Importance Especially As Waterfowl Habitat"),
                    date = c("1980-05-08", "1981-01-30", "1971-02-02", "1982-12-03"))

test_that("Code_agreements() properly returns qIDs", {
  expect_equal(code_agreements(data$title, data$date), c("19800508A_CPV-PRT", "19810130A",   "RAMSA_19710202A", "RAMSA_19821203P_A19710202"))
})

test_that("Code_agreements helper functions work properly", {
  expect_equal(code_parties(data$title), c("CPV-PRT", NA, NA, NA))
  expect_equal(code_type(data$title), c("A", "A", "A", "P"))
  expect_equal(code_dates(data$date), c("19800508", "19810130", "19710202", "19821203"))
  expect_equal(code_known_agreements(data$title), c(NA, NA, "RAMSA", "RAMSA"))
  expect_equal(code_linkage(data$title, data$date), c("", "", "A19710202", "A19710202"))
  # Should we return NAs here?
})
    
