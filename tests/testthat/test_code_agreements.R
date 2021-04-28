data <- data.frame(title = c("Agreement Between Cape Verde And Portugal On Fisheries Development", 
                             "Protocol to the Agreement Between Cape Verde And Portugal On Fisheries Development",
                              "Traité De Délimitation Maritime, Signé À Paris Le 30 Janvier 1981",
                              "Convention On Wetlands Of International Importance Especially As Waterfowl Habitat",
                              "Protocol To Amend The Convention On Wetlands Of International Importance Especially As Waterfowl Habitat", 
                              "Convention On The Protection Of The Rhine Against Pollution By Chlorides",
                              "Amendment 1 to the Convention On The Protection Of The Rhine Against Pollution By Chlorides"),
                    date = c("1980-05-08", "1990-12-31", "1981-01-30", "1971-02-02", "1982-12-03", "1976-12-03", "1983-04-29"))

test_that("Code_agreements() properly returns qIDs", {
  expect_equal(code_agreements(data$title, data$date), c("19800508_CPV-PRT", "19901231P_19800508_CPV-PRT", "19810130A",   "RAMSA19710202", "19821203P_RAMSA19710202", "19761203A", "19830429E1_19761203A"))
})

test_that("Code_agreements helper functions work properly", {
  expect_equal(code_parties(data$title), c("CPV-PRT", "CPV-PRT", NA, NA, NA, NA, NA))
  expect_equal(code_type(data$title), c("A", "P", "A", "A", "P", "A", "E1"))
  expect_equal(code_dates(data$title, data$date), c("19800508", "19901231", "19810130", "19710202", "19821203", "19761203", "19830429"))
  expect_equal(code_known_agreements(data$title), c(NA, NA, NA, "RAMSA19710202", "RAMSA19710202", NA, NA))
  expect_equal(code_linkage(data$title, data$date), c("19800508_CPV-PRT", "19800508_CPV-PRT", "", "19710202A", "19710202A", "RAMSA119761203", "RAMSA119761203"))
})
    
# Add one test for dataset that have range as dates
data2 <- data.frame(title = c("Agreement Between Cape Verde And Portugal On Fisheries Development", 
                             "Traité De Délimitation Maritime, Signé À Paris Le 30 Janvier 1981",
                             "Convention On Wetlands Of International Importance Especially As Waterfowl Habitat",
                             "Protocol To Amend The Convention On Wetlands Of International Importance Especially As Waterfowl Habitat", 
                             "Convention On The Protection Of The Rhine Against Pollution By Chlorides",
                             "Amendment 1 to the Convention On The Protection Of The Rhine Against Pollution By Chlorides"),
                   date = c("1980-01-01:1980-12-31", "1981-01-01:1981-12-31", "1971-01-01:1971-12-31", 
                            "1982-01-01:1982-12-31", "1976-01-01:1976-12-31", "1983-01-01:1983-12-31"))
test_that("code_dates() helper function treats date range correctly", {
  # Add title to the code_dates function arguments 
  expect_equal(code_dates(data2$title, data2$date), c("1980ACT01", "1981TTO01", "1971CAT01", "1982PTT01", 
                                                      "1976CPS01", "1983ALS01"))
})




