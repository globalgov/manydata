test_that("standardise_dates() treats typical dates correctly",{
  expect_equal(standardise_dates("2010-01-01"), lubridate::as_date("2010-01-01"))
})    

test_that("standardise_dates() takes three variables as input",{
  expect_equal(standardise_dates("2010", "01", "01"), lubridate::as_date("2010-01-01"))
  expect_equal(standardise_dates("2010", "1", "1"), lubridate::as_date("2010-01-01"))
  expect_error(standardise_dates("2010", "1"), "you need to pass standardise_dates")
})    

test_that("standardise_dates() treats reverse ordered dates correctly", {
  expect_match(as.character(standardise_dates("30.1.1874")), "1874-01-30")
  expect_match(as.character(standardise_dates("31.12.1879")), "1879-12-31")
  expect_match(as.character(standardise_dates("6/10/2012")), "2012-10-06")
  expect_match(as.character(standardise_dates("9.4.1939")), "1939-04-09")
  expect_match(as.character(standardise_dates("12/5/1993")), "1993-05-12")
  expect_match(as.character(standardise_dates("10.30.93")), "1993-10-30")
  expect_match(as.character(standardise_dates("20/06/04")), "2004-06-20")
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

test_that("standardise_dates() treats historical dates correctly",{
  expect_warning(recent(), "is deprecated")
  expect_match(as.character(standardise_dates("1712-01-01")[[1]]), "1712-01-01")
  expect_match(as.character(standardise_dates("712-01-01")[[1]]), "0712-01-01")
  expect_match(as.character(standardise_dates("0712-01-01")[[1]]), "0712-01-01")
  expect_match(as.character(min(standardise_dates("712")[[1]])), "0712-01-01")
  expect_match(as.character(standardise_dates("712-01-01")[[1]]), "0712-01-01")
  expect_match(as.character(min(standardise_dates("712 AD")[[1]])), "0712-01-01")
  expect_match(as.character(min(standardise_dates("1712 AD")[[1]])), "1712-01-01")
  expect_match(as.character(min(standardise_dates("12 AD")[[1]])), "0012-01-01")
  expect_match(as.character(min(standardise_dates("12")[[1]])), "0012-01-01")
  expect_match(as.character(min(standardise_dates("1712 AD")[[1]])), "1712-01-01")
  expect_match(as.character(standardise_dates("712 BC")), "-712-01-01")
  expect_match(as.character(standardise_dates("1712 BC")), "-1712-01-01")
  expect_match(as.character(standardise_dates("44 BC")), "-044-01-01")
  expect_match(as.character(standardise_dates("-712")), "-712-01-01")
  expect_match(as.character(standardise_dates("-1712")), "-1712-01-01")
  expect_match(as.character(standardise_dates("-1712-12-10")), "-1712-12-10")
  expect_match(as.character(standardise_dates("-12-10-1712")), "-1712-12-10")
})

dat <- data.frame(date = as.character(c("2010.10.12","2010/03/30", "10/12/93", "12-10-1993", "NA")))
dat1 <- data.frame(date = c("NA", "2010.10.12"," ", "10/12/93", "12-10-1993"))
dat2 <- data.frame(date = c("2010.10.12", "2010/11/13", "2010-12-14"))
dat3 <- data.frame(date = c("2010-10-12", "2010-11-13", "2010-12-14"))
datr <- data.frame(date = as.Date(c("2010-10-12", "2010-11-13", "2010-12-14")))

test_that("standardise_dates() treats multiple inconsistent dates correctly",{
  expect_match(as.character(standardise_dates(c("2010-10-12", "2010-11-13", "2010-12-14"))), c("2010-10-12", "2010-11-13", "2010-12-14"))
  expect_match(as.character(standardise_dates(dat3)), datr)
  expect_match(as.character(standardise_dates(dat2)), c("2010-10-12", "2010-11-13", "2010-12-14"))
  expect_match(as.character(standardise_dates(dat)), c("2010-10-12", "2010-03-30", "1993-10-12", "1993-10-12", "NA"))
  expect_match(as.character(standardise_dates(dat1)), c("NA", "2010-10-12", "NA", "1993-10-12", "1993-10-12"))
})
