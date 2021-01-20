# Test if {{{dataset}}} meets the q ecosystem requirements

# Requires the following package
library(pointblank)

# # Ensure the dataset is in tibble format
# test_that("exported data is in tibble format", {
#   expect_message(tibble::is_tibble("{{{dat}}}"), "TRUE")
# })

# Report missing values 
test_that("missing observations are reported correctly", {
  expect_false(any(grepl("^.$", {{{dab}}}[["{{{dat}}}"]])))
  expect_false(any(grepl("^n/a$", {{{dab}}}[["{{{dat}}}"]])))
  expect_false(any(grepl("^N/A$", {{{dab}}}[["{{{dat}}}"]])))
})

# Uniformity tests (agreements have a source ID, a string title, a signature and entry into force date)
test_that("datasets have the correct variables", {
  expect_col_exists({{{dab}}}[["{{{dat}}}"]], vars(Title))
  expect_col_exists({{{dab}}}[["{{{dat}}}"]], vars(Signature))
  expect_col_exists({{{dab}}}[["{{{dat}}}"]], vars(Force))
})

# Dates are standardized
test_that("dates are standardised", {
  expect_col_is_date({{{dab}}}[["{{{dat}}}"]], vars(Signature))
  expect_col_is_date({{{dab}}}[["{{{dat}}}"]], vars(Force))
  expect_false(any(grepl("/", {{{dab}}}[["{{{dat}}}"]]$Signature)))
  expect_false(any(grepl("/", {{{dab}}}[["{{{dat}}}"]]$Force)))
})
