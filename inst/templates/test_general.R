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

# At least one column named ID
test_that("a column indicating an ID source exists", {
  expect_true(any(grepl("_ID$", colnames({{{dab}}}[["{{{dat}}}"]]))))
})

# Labels are standardized
test_that("labels are standardised", {
  if (!is.null({{{dab}}}[["{{{dat}}}"]]$Label)) {
  expect_false(any(grepl("U.S.", {{{dab}}}[["{{{dat}}}"]])))
  expect_false(any(grepl("U.K.", {{{dab}}}[["{{{dat}}}"]])))
  expect_false(any(grepl("!", {{{dab}}}[["{{{dat}}}"]])))
  expect_false(any(grepl("NANA.", {{{dab}}}[["{{{dat}}}"]])))
  }
})

# Dates are standardized
test_that("Columns with dates are standardized", {
  if (!is.null({{{dab}}}[["{{{dat}}}"]]$Beg)) {
    expect_false(any(grepl("/", {{{dab}}}[["{{{dat}}}"]]$Beg)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{2}-[:digit:]{4}$",
                           {{{dab}}}[["{{{dat}}}"]]$Beg)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{1}-[:digit:]{4}$",
                           {{{dab}}}[["{{{dat}}}"]]$Beg)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{1}-[:digit:]{4}$",
                           {{{dab}}}[["{{{dat}}}"]]$Beg)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{2}-[:digit:]{4}$",
                           {{{dab}}}[["{{{dat}}}"]]$Beg)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{4}$",
                           {{{dab}}}[["{{{dat}}}"]]$Beg)))
    expect_false(any(grepl("^[:alpha:]$",
                           {{{dab}}}[["{{{dat}}}"]]$Beg)))
  } 
  if (!is.null({{{dab}}}[["{{{dat}}}"]]$End)) {
    expect_false(any(grepl("/", {{{dab}}}[["{{{dat}}}"]]$End)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{2}-[:digit:]{4}$",
                           {{{dab}}}[["{{{dat}}}"]]$End)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{1}-[:digit:]{4}$",
                           {{{dab}}}[["{{{dat}}}"]]$End)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{1}-[:digit:]{4}$",
                           {{{dab}}}[["{{{dat}}}"]]$End)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{2}-[:digit:]{4}$",
                           {{{dab}}}[["{{{dat}}}"]]$End)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{4}$",
                           {{{dab}}}[["{{{dat}}}"]]$End)))
    expect_false(any(grepl("^[:alpha:]$",
                           {{{dab}}}[["{{{dat}}}"]]$End)))
  }
  if (!is.null({{{dab}}}[["{{{dat}}}"]]$Force)) {
    expect_false(any(grepl("/", {{{dab}}}[["{{{dat}}}"]]$Force)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{2}-[:digit:]{4}$",
                           {{{dab}}}[["{{{dat}}}"]]$Force)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{1}-[:digit:]{4}$",
                           {{{dab}}}[["{{{dat}}}"]]$Force)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{1}-[:digit:]{4}$",
                           {{{dab}}}[["{{{dat}}}"]]$Force)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{2}-[:digit:]{4}$",
                           {{{dab}}}[["{{{dat}}}"]]$Force)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{4}$",
                           {{{dab}}}[["{{{dat}}}"]]$Force)))
    expect_false(any(grepl("^[:alpha:]$",
                           {{{dab}}}[["{{{dat}}}"]]$Force)))
  }
  if (!is.null({{{dab}}}[["{{{dat}}}"]]$Rat)) {
    expect_false(any(grepl("/", {{{dab}}}[["{{{dat}}}"]]$Rat)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{2}-[:digit:]{4}$",
                           {{{dab}}}[["{{{dat}}}"]]$Rat)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{1}-[:digit:]{4}$",
                           {{{dab}}}[["{{{dat}}}"]]$Rat)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{1}-[:digit:]{4}$",
                           {{{dab}}}[["{{{dat}}}"]]$Rat)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{2}-[:digit:]{4}$",
                           {{{dab}}}[["{{{dat}}}"]]$Rat)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{4}$",
                           {{{dab}}}[["{{{dat}}}"]]$Rat)))
    expect_false(any(grepl("^[:alpha:]$",
                           {{{dab}}}[["{{{dat}}}"]]$Rat)))
  }
  if (!is.null({{{dab}}}[["{{{dat}}}"]]$Signature)) {
    expect_false(any(grepl("/", {{{dab}}}[["{{{dat}}}"]]$Signature)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{2}-[:digit:]{4}$",
                           {{{dab}}}[["{{{dat}}}"]]$Signature)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{1}-[:digit:]{4}$",
                           {{{dab}}}[["{{{dat}}}"]]$Signature)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{1}-[:digit:]{4}$",
                           {{{dab}}}[["{{{dat}}}"]]$Signature)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{2}-[:digit:]{4}$",
                           {{{dab}}}[["{{{dat}}}"]]$Signature)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{4}$",
                           {{{dab}}}[["{{{dat}}}"]]$Signature)))
    expect_false(any(grepl("^[:alpha:]$",
                           {{{dab}}}[["{{{dat}}}"]]$Signature)))
  }
  if (!is.null({{{dab}}}[["{{{dat}}}"]]$Term)) {
    expect_false(any(grepl("/", {{{dab}}}[["{{{dat}}}"]]$Term)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{2}-[:digit:]{4}$",
                           {{{dab}}}[["{{{dat}}}"]]$Term)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{1}-[:digit:]{4}$",
                           {{{dab}}}[["{{{dat}}}"]]$Term)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{1}-[:digit:]{4}$",
                           {{{dab}}}[["{{{dat}}}"]]$Term)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{2}-[:digit:]{4}$",
                           {{{dab}}}[["{{{dat}}}"]]$Term)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{4}$",
                           {{{dab}}}[["{{{dat}}}"]]$Term)))
    expect_false(any(grepl("^[:alpha:]$",
                           {{{dab}}}[["{{{dat}}}"]]$Term)))
  }
  if (!is.null({{{dab}}}[["{{{dat}}}"]]$Withdrawal)) {
    expect_false(any(grepl("/", {{{dab}}}[["{{{dat}}}"]]$Withdrawal)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{2}-[:digit:]{4}$",
                           {{{dab}}}[["{{{dat}}}"]]$Withdrawal)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{1}-[:digit:]{4}$",
                           {{{dab}}}[["{{{dat}}}"]]$Withdrawal)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{1}-[:digit:]{4}$",
                           {{{dab}}}[["{{{dat}}}"]]$Withdrawal)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{2}-[:digit:]{4}$",
                           {{{dab}}}[["{{{dat}}}"]]$Withdrawal)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{4}$",
                           {{{dab}}}[["{{{dat}}}"]]$Withdrawal)))
    expect_false(any(grepl("^[:alpha:]$",
                           {{{dab}}}[["{{{dat}}}"]]$Withdrawal)))
  }
})
