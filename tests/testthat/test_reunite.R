data <- data.frame(fir=c(NA, "two", "three", NA),
                   sec=c("one", NA, "three", NA), stringsAsFactors = F)
dat2 <-  data.frame(single=c("one", "two", "three", NA), stringsAsFactors = F)

test_that("missing values dropped",{
  expect_equal(transmutate(data, single = reunite(fir, sec)),
               dat2)
})
