library(pointblank)

test_that("object is correct", {
  expect_col_exists({{{dat}}}, vars(endsWith("ID")))
  expect_col_exists({{{dat}}}, vars(Beg))
  expect_col_exists({{{dat}}}, vars(End))
})
