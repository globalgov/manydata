# Test if {{{dataset}}} meets the q ecosystem requirements

# Report missing values
test_that("missing observations are reported correctly", {
  expect_false(any(grepl("\\?", {{{dab}}}[["{{{dat}}}"]])))
  expect_false(any(grepl("^n/a$", {{{dab}}}[["{{{dat}}}"]])))
  expect_false(any(grepl("^N/A$", {{{dab}}}[["{{{dat}}}"]])))
  expect_false(any(grepl("^\\s$", {{{dab}}}[["{{{dat}}}"]])))
  expect_false(any(grepl("^\\.$", {{{dab}}}[["{{{dat}}}"]])))
  expect_false(any(grepl("N\\.A\\.$", {{{dab}}}[["{{{dat}}}"]])))
  expect_false(any(grepl("n\\.a\\.$", {{{dab}}}[["{{{dat}}}"]])))
})

# Uniformity tests (agreements have a source ID, a string title, a signature and
# entry into force date)
test_that("datasets have the correct variables", {
  expect_col_exists({{{dab}}}[["{{{dat}}}"]], vars(Title))
  expect_col_exists({{{dab}}}[["{{{dat}}}"]], vars(Beg))
  expect_col_exists({{{dab}}}[["{{{dat}}}"]], vars(ID))
})

# Dates are standardized for mandatory column
test_that("dates are standardised", {
  expect_col_is_date({{{dab}}}[["{{{dat}}}"]], vars(Beg))
  expect_false(any(grepl("/", {{{dab}}}[["{{{dat}}}"]]$Beg)))
  expect_false(any(grepl("^[:digit:]{2}-[:digit:]{2}-[:digit:]{4}$", {{{dab}}}[["{{{dat}}}"]]$Beg)))
  expect_false(any(grepl("^[:digit:]{1}-[:digit:]{1}-[:digit:]{4}$", {{{dab}}}[["{{{dat}}}"]]$Beg)))
  expect_false(any(grepl("^[:digit:]{2}-[:digit:]{1}-[:digit:]{4}$", {{{dab}}}[["{{{dat}}}"]]$Beg)))
  expect_false(any(grepl("^[:digit:]{1}-[:digit:]{2}-[:digit:]{4}$", {{{dab}}}[["{{{dat}}}"]]$Beg)))
  expect_false(any(grepl("^[:digit:]{2}-[:digit:]{4}$", {{{dab}}}[["{{{dat}}}"]]$Beg)))
  expect_false(any(grepl("^[:alpha:]$", {{{dab}}}[["{{{dat}}}"]]$Beg)))
})

# Dates are standardized for optional columns
test_that("Columns with dates are standardized", {
  if(!is.null({{{dab}}}[["{{{dat}}}"]]$End)) {
    expect_false(any(grepl("/", {{{dab}}}[["{{{dat}}}"]]$End)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{2}-[:digit:]{4}$", {{{dab}}}[["{{{dat}}}"]]$End)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{1}-[:digit:]{4}$", {{{dab}}}[["{{{dat}}}"]]$End)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{1}-[:digit:]{4}$", {{{dab}}}[["{{{dat}}}"]]$End)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{2}-[:digit:]{4}$", {{{dab}}}[["{{{dat}}}"]]$End)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{4}$", {{{dab}}}[["{{{dat}}}"]]$End)))
    expect_false(any(grepl("^[:alpha:]$", {{{dab}}}[["{{{dat}}}"]]$End)))
  }
  if (!is.null({{{dab}}}[["{{{dat}}}"]]$Force)) {
    expect_false(any(grepl("/", {{{dab}}}[["{{{dat}}}"]]$Force)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{2}-[:digit:]{4}$", {{{dab}}}[["{{{dat}}}"]]$Force)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{1}-[:digit:]{4}$", {{{dab}}}[["{{{dat}}}"]]$Force)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{1}-[:digit:]{4}$", {{{dab}}}[["{{{dat}}}"]]$Force)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{2}-[:digit:]{4}$", {{{dab}}}[["{{{dat}}}"]]$Force)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{4}$", {{{dab}}}[["{{{dat}}}"]]$Force)))
    expect_false(any(grepl("^[:alpha:]$", {{{dab}}}[["{{{dat}}}"]]$Force)))
  }
  if (!is.null({{{dab}}}[["{{{dat}}}"]]$Rat)) {
    expect_false(any(grepl("/", {{{dab}}}[["{{{dat}}}"]]$Rat)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{2}-[:digit:]{4}$", {{{dab}}}[["{{{dat}}}"]]$Rat)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{1}-[:digit:]{4}$", {{{dab}}}[["{{{dat}}}"]]$Rat)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{1}-[:digit:]{4}$", {{{dab}}}[["{{{dat}}}"]]$Rat)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{2}-[:digit:]{4}$", {{{dab}}}[["{{{dat}}}"]]$Rat)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{4}$", {{{dab}}}[["{{{dat}}}"]]$Rat)))
    expect_false(any(grepl("^[:alpha:]$", {{{dab}}}[["{{{dat}}}"]]$Rat)))
  }
})


