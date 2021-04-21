data <- data.frame(title = c("Agreement Between Cape Verde And Portugal On Fisheries Development", 
                              "Traité De Délimitation Maritime, Signé À Paris Le 30 Janvier 1981",
                              "Convention On Wetlands Of International Importance Especially As Waterfowl Habitat",
                              "Protocol To Amend The Convention On Wetlands Of International Importance Especially As Waterfowl Habitat", 
                              "Convention On The Protection Of The Rhine Against Pollution By Chlorides",
                              "Amendment 1 to the Convention On The Protection Of The Rhine Against Pollution By Chlorides"),
                    date = c("1980-05-08", "1981-01-30", "1971-02-02", "1982-12-03", "1976-12-03", "1983-04-29"))

test_that("Code_agreements() properly returns qIDs", {
  expect_equal(code_agreements(data$title, data$date), c("19800508A_CPV-PRT", "19810130A",   "RAMSA_19710202A", "RAMSA_19821203P_A19710202", "19761203A", "19830429E1_A19761203"))
})

test_that("Code_agreements helper functions work properly", {
  expect_equal(code_parties(data$title), c("CPV-PRT", NA, NA, NA, NA, NA))
  expect_equal(code_type(data$title), c("A", "A", "A", "P", "A", "E1"))
  expect_equal(code_dates(data$date), c("19800508", "19810130", "19710202", "19821203", "19761203", "19830429"))
  expect_equal(code_known_agreements(data$title), c(NA, NA, "RAMSA", "RAMSA", NA, NA))
  expect_equal(code_linkage(data$title, data$date), c("", "", "A19710202", "A19710202", "A19761203", "A19761203"))
})
    
# Add one test for dataset that have range as dates
data2 <- data.frame(title = c("Agreement Between Cape Verde And Portugal On Fisheries Development", 
                             "Traité De Délimitation Maritime, Signé À Paris Le 30 Janvier 1981",
                             "Convention On Wetlands Of International Importance Especially As Waterfowl Habitat",
                             "Protocol To Amend The Convention On Wetlands Of International Importance Especially As Waterfowl Habitat", 
                             "Convention On The Protection Of The Rhine Against Pollution By Chlorides",
                             "Amendment 1 to the Convention On The Protection Of The Rhine Against Pollution By Chlorides"),
                   date = c("1980", "1981", "1971", "1982", "1976", "1983"))
test_that("code_dates() helper function treats date range correctly", {
  # Add title to the code_dates function arguments 
  expect_equal(code_dates(data2$title, data2$date), c("1980AT", "1981TR", "1971CT", "1982PS", "1976CS", "1983AS"))
})




