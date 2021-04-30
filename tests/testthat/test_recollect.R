data1 <- data.frame(ID = c(1, 2, 3, 3, 2, 1),
                    One = c(1, NA, 3, NA, 2, NA),
                    Two = c(NA, "B", NA, "C", NA, "A"))

test_that("observations recollected correctly", {
  expect_equal(recollect(data1$One), "1_3_2")
  expect_equal(recollect(data1$Two), "B_C_A")
})
