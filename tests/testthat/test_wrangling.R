# Test wrangling functions

names <- c(NA, "JH", NA, "HS", "HS", NA, NA)
group <- data.frame(group = c("A", "A", "A", "B", "B", "B", "B"))
dat <- data.frame(cbind(names, group))

test_that("first non NA value is gotten", {
  expect_equal(coalesce_rows(dat), group)
  expect_equal(coalesce_rows(dat$names), "JH")
  expect_equal(coalesce_rows(dat$group), "A")
})

data1 <- data.frame(ID = c(1, 2, 3, 3, 2, 1),
                    One = c(1, NA, 3, NA, 2, NA),
                    Two = c(NA, "B", NA, "C", NA, "A"))

test_that("observations recollected correctly", {
  expect_equal(recollect(data1$One), "1_3_2")
  expect_equal(recollect(data1$Two), "B_C_A")
})

data1 <- data.frame(ID = c(1, 2, 3, 3, 2, 1),
                    One = c(1, NA, 3, NA, 2, NA),
                    Two = c(NA, "B", NA, "C", NA, "A"))
data2 <- data.frame(ID = c(1, 2, 3, 3, 2, 1),
                    One = c(1, 2, 3, 3, 2, 1),
                    Two = c("A", "B", "C", "C", "B", "A"))

test_that("observations painted correctly", {
  expect_equal(repaint(data1, "ID", c("One", "Two")), data2)
})

data <- data.frame(fir = c(NA, "two", "three", NA),
                   sec = c("one", NA, "three", NA), stringsAsFactors = F)
dat2 <-  data.frame(single = c("one", "two", "three", NA), stringsAsFactors = F)

test_that("missing values dropped", {
  expect_equal(transmutate(data, single = reunite(fir, sec)),
               dat2)
})

transp <- data.frame(bikes = 5, skates = 4)

test_that("transmutate function works for category", {
  expect_named(transmutate(transp, tmode = bikes + skates), "tmode")
})

test_that("transmutate drops variables", {
  expect_output(str(transmutate(transp, tmode = bikes + skates)), "1 variable")
})
