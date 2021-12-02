transp <- data.frame(bikes = 5, skates = 4)

test_that("transmutate function works for category", {
 expect_named(transmutate(transp, tmode = bikes + skates), "tmode")
})

test_that("transmutate drops variables", {
  expect_output(str(transmutate(transp, tmode = bikes + skates)), "1 variable")
})
