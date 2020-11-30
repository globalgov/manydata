test_that("dates are parsed correctly",{
  expect_equal(recent("12/12/24"),"1924-12-12")
  expect_equal(recent("6/23/24"),"1924-06-23")
  expect_equal(recent("23/6/24"),"1924-06-23")
})

test_that("separators are parsed correctly",{
  expect_equal(recent("12/12/04"),"2004-12-12")
  expect_equal(recent("12.12.04"),"2004-12-12")
})

test_that("centuries are parsed correctly",{
  expect_equal(recent("100-00-00"), "0100-01-01")
  expect_equal(recent("1874-07-20"), "1874-07-20")
  expect_equal(recent("2004-12-12"), "2004-12-12")
  expect_equal(recent("2067-08-20"), "1967-08-20")
})

test_that("vectors are parsed correctly",{
  expect_equal(recent(c("1874-07-20","2004-12-12","2067-08-20")),
               c("1874-07-20","2004-12-12","1967-08-20"))
  expect_equal(recent(c("1874-07-20","2004-12-12","2067-08-20", NA)),
               c("1874-07-20","2004-12-12","1967-08-20", NA))
  expect_equal(recent(c("11/30/98","9/29/1864","11/4/1866","5/29/68")),
               c("1998-11-30","1864-09-29","1866-11-04","1968-05-29"))
  expect_equal(recent(c("1549-01-01","920-01-01","1368-01-01")),
               c("1549-01-01","0920-01-01","1368-01-01"))
  expect_equal(recent(c("100-00-00","9999-12-31")),
               c("0100-01-01","9999-12-31"))
})