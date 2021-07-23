# Test if  meets the q ecosystem requirements

# Report missing values
test_that("missing observations are reported correctly", {
  expect_false(any(grepl("\\?", emperors[["britannica"]])))
  expect_false(any(grepl("^n/a$", emperors[["britannica"]])))
  expect_false(any(grepl("^N/A$", emperors[["britannica"]])))
  expect_false(any(grepl("^\\s$", emperors[["britannica"]])))
  expect_false(any(grepl("^\\.$", emperors[["britannica"]])))
  expect_false(any(grepl("N\\.A\\.$", emperors[["britannica"]])))
  expect_false(any(grepl("n\\.a\\.$", emperors[["britannica"]])))
})

# # At least one column named ID
# test_that("a column indicating an ID source exists", {
#   expect_true(any(grepl("_ID$", colnames(emperors[["britannica"]]))))
# })

# Labels are standardized
test_that("labels are standardised", {
  if (!is.null(emperors[["britannica"]]$Label)) {
  expect_false(any(grepl("U.S.", emperors[["britannica"]])))
  expect_false(any(grepl("U.K.", emperors[["britannica"]])))
  expect_false(any(grepl("!", emperors[["britannica"]])))
  expect_false(any(grepl("NANA.", emperors[["britannica"]])))
  }
})

# Dates are standardized
test_that("Columns with dates are standardized", {
  if (!is.null(emperors[["britannica"]]$Beg)) {
    expect_equal(class(emperors[["britannica"]]$Beg), "messydt")
    expect_false(any(grepl("/", emperors[["britannica"]]$Beg)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{2}-[:digit:]{4}$",
                           emperors[["britannica"]]$Beg)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{1}-[:digit:]{4}$",
                           emperors[["britannica"]]$Beg)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{1}-[:digit:]{4}$",
                           emperors[["britannica"]]$Beg)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{2}-[:digit:]{4}$",
                           emperors[["britannica"]]$Beg)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{4}$",
                           emperors[["britannica"]]$Beg)))
    expect_false(any(grepl("^[:alpha:]$",
                           emperors[["britannica"]]$Beg)))
  }
  if (!is.null(emperors[["britannica"]]$End)) {
    expect_equal(class(emperors[["britannica"]]$End), "messydt")
    expect_false(any(grepl("/", emperors[["britannica"]]$End)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{2}-[:digit:]{4}$",
                           emperors[["britannica"]]$End)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{1}-[:digit:]{4}$",
                           emperors[["britannica"]]$End)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{1}-[:digit:]{4}$",
                           emperors[["britannica"]]$End)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{2}-[:digit:]{4}$",
                           emperors[["britannica"]]$End)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{4}$",
                           emperors[["britannica"]]$End)))
    expect_false(any(grepl("^[:alpha:]$",
                           emperors[["britannica"]]$End)))
  }
  if (!is.null(emperors[["britannica"]]$Force)) {
    expect_equal(class(emperors[["britannica"]]$Force), "messydt")
    expect_false(any(grepl("/", emperors[["britannica"]]$Force)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{2}-[:digit:]{4}$",
                           emperors[["britannica"]]$Force)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{1}-[:digit:]{4}$",
                           emperors[["britannica"]]$Force)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{1}-[:digit:]{4}$",
                           emperors[["britannica"]]$Force)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{2}-[:digit:]{4}$",
                           emperors[["britannica"]]$Force)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{4}$",
                           emperors[["britannica"]]$Force)))
    expect_false(any(grepl("^[:alpha:]$",
                           emperors[["britannica"]]$Force)))
  }
  if (!is.null(emperors[["britannica"]]$Rat)) {
    expect_equal(class(emperors[["britannica"]]$Rat), "messydt")
    expect_false(any(grepl("/", emperors[["britannica"]]$Rat)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{2}-[:digit:]{4}$",
                           emperors[["britannica"]]$Rat)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{1}-[:digit:]{4}$",
                           emperors[["britannica"]]$Rat)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{1}-[:digit:]{4}$",
                           emperors[["britannica"]]$Rat)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{2}-[:digit:]{4}$",
                           emperors[["britannica"]]$Rat)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{4}$",
                           emperors[["britannica"]]$Rat)))
    expect_false(any(grepl("^[:alpha:]$",
                           emperors[["britannica"]]$Rat)))
  }
  if (!is.null(emperors[["britannica"]]$Signature)) {
    expect_equal(class(emperors[["britannica"]]$Signature), "messydt")
    expect_false(any(grepl("/", emperors[["britannica"]]$Signature)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{2}-[:digit:]{4}$",
                           emperors[["britannica"]]$Signature)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{1}-[:digit:]{4}$",
                           emperors[["britannica"]]$Signature)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{1}-[:digit:]{4}$",
                           emperors[["britannica"]]$Signature)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{2}-[:digit:]{4}$",
                           emperors[["britannica"]]$Signature)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{4}$",
                           emperors[["britannica"]]$Signature)))
    expect_false(any(grepl("^[:alpha:]$",
                           emperors[["britannica"]]$Signature)))
  }
  if (!is.null(emperors[["britannica"]]$Term)) {
    expect_equal(class(emperors[["britannica"]]$Term), "messydt")
    expect_false(any(grepl("/", emperors[["britannica"]]$Term)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{2}-[:digit:]{4}$",
                           emperors[["britannica"]]$Term)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{1}-[:digit:]{4}$",
                           emperors[["britannica"]]$Term)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{1}-[:digit:]{4}$",
                           emperors[["britannica"]]$Term)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{2}-[:digit:]{4}$",
                           emperors[["britannica"]]$Term)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{4}$",
                           emperors[["britannica"]]$Term)))
    expect_false(any(grepl("^[:alpha:]$",
                           emperors[["britannica"]]$Term)))
  }
  if (!is.null(emperors[["britannica"]]$Withdrawal)) {
    expect_equal(class(emperors[["britannica"]]$Withdrawal), "messydt")
    expect_false(any(grepl("/", emperors[["britannica"]]$Withdrawal)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{2}-[:digit:]{4}$",
                           emperors[["britannica"]]$Withdrawal)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{1}-[:digit:]{4}$",
                           emperors[["britannica"]]$Withdrawal)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{1}-[:digit:]{4}$",
                           emperors[["britannica"]]$Withdrawal)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{2}-[:digit:]{4}$",
                           emperors[["britannica"]]$Withdrawal)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{4}$",
                           emperors[["britannica"]]$Withdrawal)))
    expect_false(any(grepl("^[:alpha:]$",
                           emperors[["britannica"]]$Withdrawal)))
  }
})

# # Dataset should be ordered according to the "Beg" column
# # if the column exists
#   test_that("dataset is arranged by date variable", {
#     if (!is.null(emperors[["britannica"]]$Beg)) {
#   expect_true(emperors[["britannica"]]$Beg[1] < emperors[["britannica"]]$Beg[10])
#   expect_true(emperors[["britannica"]]$Beg[50] < emperors[["britannica"]]$Beg[75])
#   expect_true(emperors[["britannica"]]$Beg[100] < emperors[["britannica"]]$Beg[120])
#     }
# })
