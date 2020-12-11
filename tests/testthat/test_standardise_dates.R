test_that("standardise_dates() treats typical dates correctly",{
  expect_equal(standardise_dates("2010-01-01"), anytime::anydate("2010-01-01"))
})    

test_that("standardise_dates() treats reverse ordered dates correctly",{
  expect_equal(standardise_dates("30.1.1874"), anytime::anydate("1874-01-30"))
  expect_equal(standardise_dates("31.12.1879"), anytime::anydate("1879-12-31"))
  expect_match(standardise_dates("6.12.2012"), anytime::anydate("2012-12-06"))
  expect_match(standardise_dates("9.4.1939"), anytime::anydate("1939-04-09"))
  expect_match(standardise_dates("12.5.1993"), anytime::anydate("1993-05-12"))
})    

test_that("standardise_dates() treats incomplete dates correctly",{
  expect_match(as.character(min(standardise_dates("2010"))), "2010-01-01")
  expect_match(as.character(max(standardise_dates("2010"))), "2010-12-31")
  expect_match(as.character(min(standardise_dates("2010-01"))), "2010-01-01")
  expect_match(as.character(max(standardise_dates("2010-01"))), "2010-01-31")
}) 

test_that("standardise_dates() treats missing month components correctly",{
  expect_match(as.character(min(standardise_dates("2010-00-00"))), "2010-01-01")
  expect_match(as.character(max(standardise_dates("2010-00-00"))), "2010-12-31")
  expect_match(as.character(min(standardise_dates("2010-??-??"))), "2010-01-01")
  expect_match(as.character(max(standardise_dates("2010-??-??"))), "2010-12-31")
  expect_match(as.character(min(standardise_dates("2010-NA-NA"))), "2010-01-01")
  expect_match(as.character(max(standardise_dates("2010-NA-NA"))), "2010-12-31")
})

test_that("standardise_dates() treats missing day components correctly",{
  expect_match(as.character(min(standardise_dates("2010-01-00"))), "2010-01-01")
  expect_match(as.character(max(standardise_dates("2010-01-00"))), "2010-01-31")
  expect_match(as.character(min(standardise_dates("2010-01-??"))), "2010-01-01")
  expect_match(as.character(max(standardise_dates("2010-01-??"))), "2010-01-31")
  expect_match(as.character(min(standardise_dates("2010-01-NA"))), "2010-01-01")
  expect_match(as.character(max(standardise_dates("2010-01-NA"))), "2010-01-31")
})

test_that("standardise_dates() treats ranged dates correctly",{
  expect_match(as.character(min(standardise_dates("2010:2011"))), "2010-01-01")
  expect_match(as.character(max(standardise_dates("2010:2011"))), "2011-12-31")
  expect_match(as.character(min(standardise_dates("2010-01:03"))), "2010-01-01")
  expect_match(as.character(max(standardise_dates("2010-01:03"))), "2010-03-31")
  expect_match(as.character(min(standardise_dates("2010-01-01:03"))), "2010-01-01")
  expect_match(as.character(max(standardise_dates("2010-01-01:03"))), "2010-01-03")
  expect_match(as.character(min(standardise_dates("2010_2011"))), "2010-01-01")
  expect_match(as.character(max(standardise_dates("2010_2011"))), "2011-12-31")
  expect_match(as.character(min(standardise_dates("2010-01_03"))), "2010-01-01")
  expect_match(as.character(max(standardise_dates("2010-01_03"))), "2010-03-31")
  expect_match(as.character(min(standardise_dates("2010-01-01_03"))), "2010-01-01")
  expect_match(as.character(max(standardise_dates("2010-01-01_03"))), "2010-01-03")
})

test_that("standardise_dates() treats future dates correctly",{
  expect_match(as.character(standardise_dates("9999-12-31")), "9999-12-31")
  expect_match(as.character(standardise_dates("2599-12-31")), "9999-12-31")
})

# test_that("standardise_dates() treats historical dates correctly",{
#   expect_match(as.character(standardise_dates("1712-01-01")[[1]]), "1712-01-01")
#   expect_match(as.character(standardise_dates("712-01-01")[[1]]), "0712-01-01")
#   expect_match(as.character(standardise_dates("0712-01-01")[[1]]), "0712-01-01")
#   expect_match(as.character(min(standardise_dates("712")[[1]])), "0712-01-01")
#   expect_match(as.character(min(standardise_dates("712 AD")[[1]])), "0712-01-01")
#   expect_match(as.character(min(standardise_dates("712 BC")[[1]])), "-0712-01-01")
#   expect_match(as.character(min(standardise_dates("-712")[[1]])), "-0712-01-01")
#   expect_match(as.character(min(standardise_dates("-1712")[[1]])), "-1712-01-01")
# })
