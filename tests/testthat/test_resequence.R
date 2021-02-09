data <- data.frame(Sign = c("2000-01-01", "2001-01-01", "2001-01-01_2000-01-01", "2000-01-01", NA, "-01-01"),
                   Force = c("2001-01-01", "2000-01-01", "2001-01-01", NA, "2001-01-01", "9999"))
result <- as.matrix(unname(data.frame(c("2000-01-01", "2000-01-01", "2000-01-01", "2000-01-01", NA, "-01-01"),
                                      c("2001-01-01", "2001-01-01", "2001-01-01", NA, "2001-01-01", "9999"))))
attr(result, "dimnames") <- NULL

test_that("dates are parsed correctly",{
  expect_equal(resequence(data, c("Sign","Force")), result)
})
