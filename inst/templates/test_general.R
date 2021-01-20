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
