names <- c(NA, "JH", NA, "HS", "HS", NA, NA)
group <- data.frame(group = c("A", "A", "A", "B", "B", "B", "B"))
dat <- data.frame(cbind(names, group))

test_that("first non NA value is gotten", {
  expect_equal(coalesce_rows(dat), group)
  expect_equal(coalesce_rows(dat$names), "JH")
  expect_equal(coalesce_rows(dat$group), "A")
})
