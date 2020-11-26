library(pointblank)

test_that("object is correct", {
  expect_col_exists({{{dat}}}, vars(endsWith("ID")))
  expect_col_exists({{{dat}}}, vars(Beg))
  expect_col_exists({{{dat}}}, vars(End))
})

test_that("missing obsevarsions are reported correctly", {
  expect_length(grepl("-", {{{dat}}}), 0)
  expect_length(grepl("n/a", {{{dat}}}), 0)
  expect_length(grepl("N/A", {{{dat}}}), 0)
})
