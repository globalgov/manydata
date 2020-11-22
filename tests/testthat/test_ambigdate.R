test_that("ambig_date() treats typical dates correctly",{
  expect_equal(ambig_date("2010-01-01")[[1]], anytime::anydate("2010-01-01"))
})    

test_that("ambig_date() treats incomplete dates correctly",{
  expect_match(as.character(min(ambig_date("2010")[[1]])), "2010-01-01")
  expect_match(as.character(max(ambig_date("2010")[[1]])), "2010-12-31")
  expect_match(as.character(min(ambig_date("2010-01")[[1]])), "2010-01-01")
  expect_match(as.character(max(ambig_date("2010-01")[[1]])), "2010-01-31")
}) 

test_that("ambig_date() treats missing month components correctly",{
  expect_match(as.character(min(ambig_date("2010-00-00")[[1]])), "2010-01-01")
  expect_match(as.character(max(ambig_date("2010-00-00")[[1]])), "2010-12-31")
  expect_match(as.character(min(ambig_date("2010-??-??")[[1]])), "2010-01-01")
  expect_match(as.character(max(ambig_date("2010-??-??")[[1]])), "2010-12-31")
  expect_match(as.character(min(ambig_date("2010-NA-NA")[[1]])), "2010-01-01")
  expect_match(as.character(max(ambig_date("2010-NA-NA")[[1]])), "2010-12-31")
})

test_that("ambig_date() treats missing day components correctly",{
  expect_match(as.character(min(ambig_date("2010-01-00")[[1]])), "2010-01-01")
  expect_match(as.character(max(ambig_date("2010-01-00")[[1]])), "2010-01-31")
  expect_match(as.character(min(ambig_date("2010-01-??")[[1]])), "2010-01-01")
  expect_match(as.character(max(ambig_date("2010-01-??")[[1]])), "2010-01-31")
  expect_match(as.character(min(ambig_date("2010-01-NA")[[1]])), "2010-01-01")
  expect_match(as.character(max(ambig_date("2010-01-NA")[[1]])), "2010-01-31")
})

test_that("ambig_date() treats ranged dates correctly",{
  expect_match(as.character(min(ambig_date("2010:2011")[[1]])), "2010-01-01")
  expect_match(as.character(max(ambig_date("2010:2011")[[1]])), "2011-12-31")
  expect_match(as.character(min(ambig_date("2010-01:03")[[1]])), "2010-01-01")
  expect_match(as.character(max(ambig_date("2010-01:03")[[1]])), "2010-03-31")
  expect_match(as.character(min(ambig_date("2010-01-01:03")[[1]])), "2010-01-01")
  expect_match(as.character(max(ambig_date("2010-01-01:03")[[1]])), "2010-01-03")
  expect_match(as.character(min(ambig_date("2010_2011")[[1]])), "2010-01-01")
  expect_match(as.character(max(ambig_date("2010_2011")[[1]])), "2011-12-31")
  expect_match(as.character(min(ambig_date("2010-01_03")[[1]])), "2010-01-01")
  expect_match(as.character(max(ambig_date("2010-01_03")[[1]])), "2010-03-31")
  expect_match(as.character(min(ambig_date("2010-01-01_03")[[1]])), "2010-01-01")
  expect_match(as.character(max(ambig_date("2010-01-01_03")[[1]])), "2010-01-03")
})

test_that("ambig_date() treats future dates correctly",{
  expect_match(as.character(ambig_date("9999-12-31")[[1]]), "9999-12-31")
  expect_match(as.character(ambig_date("2599-12-31")[[1]]), "9999-12-31")
})

test_that("ambig_date() treats historical dates correctly",{
  expect_match(as.character(ambig_date("1712-01-01")[[1]]), "1712-01-01")
  expect_match(as.character(ambig_date("712-01-01")[[1]]), "0712-01-01")
  expect_match(as.character(ambig_date("0712-01-01")[[1]]), "0712-01-01")
  expect_match(as.character(min(ambig_date("712")[[1]])), "0712-01-01")
  expect_match(as.character(min(ambig_date("712 AD")[[1]])), "0712-01-01")
  expect_match(as.character(min(ambig_date("712 BC")[[1]])), "-0712-01-01")
  expect_match(as.character(min(ambig_date("-712")[[1]])), "-0712-01-01")
  expect_match(as.character(min(ambig_date("-1712")[[1]])), "-1712-01-01")
})
