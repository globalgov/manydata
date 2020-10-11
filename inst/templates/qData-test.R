library(pointblank)

test_that("object is correct", {
  expect_col_exists(object, vars(Beg))
  expect_col_exists(object, vars(End))
})
