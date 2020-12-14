library(pointblank)

test_that("object is correct", {
  expect_col_exists({{{dab}}}[["{{{dat}}}"]], vars(endsWith("ID")))
  expect_col_exists({{{dab}}}[["{{{dat}}}"]], vars(Beg))
  expect_col_exists({{{dab}}}[["{{{dat}}}"]], vars(End))
})

test_that("missing obsevarsions are reported correctly", {
  expect_false(any(grepl("-", {{{dab}}}[["{{{dat}}}"]])))
  expect_false(any(grepl("n/a", {{{dab}}}[["{{{dat}}}"]])))
  expect_false(any(grepl("N/A", {{{dab}}}[["{{{dat}}}"]])))
})

test_that("dates are standardised", {
  expect_col_is_date({{{dab}}}[["{{{dat}}}"]], vars(Beg))
  expect_col_is_date({{{dab}}}[["{{{dat}}}"]], vars(End))
  expect_false(any(grepl("/", {{{dab}}}[["{{{dat}}}"]])))
})

test_that("labels are standardised", {
  expect_false(any(grepl("U.S.", {{{dab}}}[["{{{dat}}}"]])))
  expect_false(any(grepl("U.K.", {{{dab}}}[["{{{dat}}}"]])))
  expect_false(any(grepl("?", {{{dab}}}[["{{{dat}}}"]])))
  expect_false(any(grepl("NANA.", {{{dab}}}[["{{{dat}}}"]])))
})
