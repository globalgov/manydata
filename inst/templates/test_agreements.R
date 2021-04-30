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
test_that("datasets have the required variables", {
  expect_col_exists({{{dab}}}[["{{{dat}}}"]], vars(Title))
  expect_col_exists({{{dab}}}[["{{{dat}}}"]], vars(Beg))
  expect_true(any(grepl("_ID$", colnames({{{dab}}}[["{{{dat}}}"]]))))
  expect_col_exists({{{dab}}}[["{{{dat}}}"]], vars(Signature))
  expect_col_exists({{{dab}}}[["{{{dat}}}"]], vars(Force))
})

# Dates are standardized for mandatory column
test_that("Column `Beg` has standardised dates", {
  expect_col_is_date({{{dab}}}[["{{{dat}}}"]], vars(Beg))
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
})

test_that("Column `Signature` has standardised dates", {
  expect_col_is_date({{{dab}}}[["{{{dat}}}"]], vars(Signature))
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
})

test_that("Column `Force` has standardised dates", {
  expect_col_is_date({{{dab}}}[["{{{dat}}}"]], vars(Force))
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
})

# Dates are standardized for optional columns
test_that("Columns with dates are standardized", {
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
})
