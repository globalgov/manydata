# Test if {{{dataset}}} meets the q ecosystem requirements

# Report missing values
test_that("missing observations are reported correctly", {
  expect_false(any(grepl("^.$", {{{dab}}}[["{{{dat}}}"]])))
  expect_false(any(grepl("^n/a$", {{{dab}}}[["{{{dat}}}"]])))
  expect_false(any(grepl("^N/A$", {{{dab}}}[["{{{dat}}}"]])))
})

# Labels are standardized
test_that("labels are standardised", {
  # expect_false(any(grepl("U.S.", {{{dab}}}[["{{{dat}}}"]])))
  # expect_false(any(grepl("U.K.", {{{dab}}}[["{{{dat}}}"]])))
  expect_false(any(grepl("!", {{{dab}}}[["{{{dat}}}"]])))
  expect_false(any(grepl("NANA.", {{{dab}}}[["{{{dat}}}"]])))
})
