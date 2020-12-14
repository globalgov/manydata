library(pointblank)

test_that("object is correct", {
  expect_col_exists({{{dab}}}[[{{{dat}}}]], vars(endsWith("ID")))
  expect_col_exists({{{dab}}}[[{{{dat}}}]], vars(Beg))
  expect_col_exists({{{dab}}}[[{{{dat}}}]], vars(End))
})

test_that("missing obsevarsions are reported correctly", {
  expect_length(grepl("-", {{{dab}}}[[{{{dat}}}]]), 0)
  expect_length(grepl("n/a", {{{dab}}}[[{{{dat}}}]]), 0)
  expect_length(grepl("N/A", {{{dab}}}[[{{{dat}}}]]), 0)
})

test_that("dates are standardised", {
  expect_col_is_date({{{dab}}}[[{{{dat}}}]], vars(Beg))
  expect_col_is_date({{{dab}}}[[{{{dat}}}]], vars(End))
  expect_length(grepl("/", {{{dab}}}[[{{{dat}}}]]), 0)
})

test_that("labels are standardised", {
  expect_length(grepl("U.S.", {{{dab}}}[[{{{dat}}}]]), 0)
  expect_length(grepl("U.K.", {{{dab}}}[[{{{dat}}}]]), 0)
  expect_length(grepl("?", {{{dab}}}[[{{{dat}}}]]), 0)
  expect_length(grepl("NANA.", {{{dab}}}[[{{{dat}}}]]), 0)
})
