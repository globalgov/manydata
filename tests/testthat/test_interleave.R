dat <- c("1", NA,  "2", NA, "3",  "4", "5")

test_that("missing values dropped",{
  expect_equal(as.character(interleave(1:5, c(2,4))), dat)
  expect_length(interleave(1:5, c(2,4)), 7)
})

