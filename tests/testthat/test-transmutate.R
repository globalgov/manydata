transp <- data.frame (bikes = c(5), skates = c(4))

test_that("transmutate function works for category",{
 expect_named(transmutate(transp, tmode=bikes+skates), "tmode")
})

test_that("transmutate function works for droping variables",{
  expect_output(str(tmode), "1 variable")
})

