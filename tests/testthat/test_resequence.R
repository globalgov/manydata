data <- data.frame(Sign = c("2000-01-01", "2001-01-01", "2001-01-01_2000-01-01", "2000-01-01", NA),
                   Force = c("2001-01-01", "2000-01-01", "2001-01-01", NA, "2001-01-01"))
result <- as.matrix(unname(data.frame(c("2000-01-01", "2000-01-01", "2000-01-01", "2000-01-01", NA),
                                      c("2001-01-01", "2001-01-01", "2001-01-01", NA, "2001-01-01"))))
attr(result, "dimnames") <- NULL

test_that("dates are parsed correctly",{
  expect_equal(resequence(data, c("Sign","Force")), result)
})