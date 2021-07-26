data1 <- data.frame(ID = c(1, 2, 3, 3, 2, 1),
                    One = c(1, NA, 3, NA, 2, NA),
                    Two = c(NA, "B", NA, "C", NA, "A"))
data2 <- data.frame(ID = c(1, 2, 3, 3, 2, 1),
                    One = c(1, 2, 3, 3, 2, 1),
                    Two = c("A", "B", "C", "C", "B", "A"))

test_that("observations painted correctly", {
  expect_equal(repaint(data1, "ID", c("One", "Two")), data2)
})
