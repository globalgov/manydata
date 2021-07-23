# Test if  meets the q ecosystem requirements

# Report missing values
test_that("missing observations are reported correctly", {
  # expect_false(any(grepl("\\?", emperors[["UNRV"]])))
  expect_false(any(grepl("^n/a$", emperors[["UNRV"]])))
  expect_false(any(grepl("^N/A$", emperors[["UNRV"]])))
  expect_false(any(grepl("^\\s$", emperors[["UNRV"]])))
  expect_false(any(grepl("^\\.$", emperors[["UNRV"]])))
  expect_false(any(grepl("N\\.A\\.$", emperors[["UNRV"]])))
  expect_false(any(grepl("n\\.a\\.$", emperors[["UNRV"]])))
})

# # At least one column named ID
# test_that("a column indicating an ID source exists", {
#   expect_true(any(grepl("_ID$", colnames(emperors[["UNRV"]]))))
# })

# Labels are standardized
test_that("labels are standardised", {
  if (!is.null(emperors[["UNRV"]]$Label)) {
  expect_false(any(grepl("U.S.", emperors[["UNRV"]])))
  expect_false(any(grepl("U.K.", emperors[["UNRV"]])))
  expect_false(any(grepl("!", emperors[["UNRV"]])))
  expect_false(any(grepl("NANA.", emperors[["UNRV"]])))
  }
})

# Dates are standardized
test_that("Columns with dates are standardized", {
  if (!is.null(emperors[["UNRV"]]$Beg)) {
    expect_equal(class(emperors[["UNRV"]]$Beg), "messydt")
    expect_false(any(grepl("/", emperors[["UNRV"]]$Beg)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{2}-[:digit:]{4}$",
                           emperors[["UNRV"]]$Beg)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{1}-[:digit:]{4}$",
                           emperors[["UNRV"]]$Beg)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{1}-[:digit:]{4}$",
                           emperors[["UNRV"]]$Beg)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{2}-[:digit:]{4}$",
                           emperors[["UNRV"]]$Beg)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{4}$",
                           emperors[["UNRV"]]$Beg)))
    expect_false(any(grepl("^[:alpha:]$",
                           emperors[["UNRV"]]$Beg)))
  }
  if (!is.null(emperors[["UNRV"]]$End)) {
    expect_equal(class(emperors[["UNRV"]]$End), "messydt")
    expect_false(any(grepl("/", emperors[["UNRV"]]$End)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{2}-[:digit:]{4}$",
                           emperors[["UNRV"]]$End)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{1}-[:digit:]{4}$",
                           emperors[["UNRV"]]$End)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{1}-[:digit:]{4}$",
                           emperors[["UNRV"]]$End)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{2}-[:digit:]{4}$",
                           emperors[["UNRV"]]$End)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{4}$",
                           emperors[["UNRV"]]$End)))
    expect_false(any(grepl("^[:alpha:]$",
                           emperors[["UNRV"]]$End)))
  }
  if (!is.null(emperors[["UNRV"]]$Force)) {
    expect_equal(class(emperors[["UNRV"]]$Force), "messydt")
    expect_false(any(grepl("/", emperors[["UNRV"]]$Force)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{2}-[:digit:]{4}$",
                           emperors[["UNRV"]]$Force)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{1}-[:digit:]{4}$",
                           emperors[["UNRV"]]$Force)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{1}-[:digit:]{4}$",
                           emperors[["UNRV"]]$Force)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{2}-[:digit:]{4}$",
                           emperors[["UNRV"]]$Force)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{4}$",
                           emperors[["UNRV"]]$Force)))
    expect_false(any(grepl("^[:alpha:]$",
                           emperors[["UNRV"]]$Force)))
  }
  if (!is.null(emperors[["UNRV"]]$Rat)) {
    expect_equal(class(emperors[["UNRV"]]$Rat), "messydt")
    expect_false(any(grepl("/", emperors[["UNRV"]]$Rat)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{2}-[:digit:]{4}$",
                           emperors[["UNRV"]]$Rat)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{1}-[:digit:]{4}$",
                           emperors[["UNRV"]]$Rat)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{1}-[:digit:]{4}$",
                           emperors[["UNRV"]]$Rat)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{2}-[:digit:]{4}$",
                           emperors[["UNRV"]]$Rat)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{4}$",
                           emperors[["UNRV"]]$Rat)))
    expect_false(any(grepl("^[:alpha:]$",
                           emperors[["UNRV"]]$Rat)))
  }
  if (!is.null(emperors[["UNRV"]]$Signature)) {
    expect_equal(class(emperors[["UNRV"]]$Signature), "messydt")
    expect_false(any(grepl("/", emperors[["UNRV"]]$Signature)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{2}-[:digit:]{4}$",
                           emperors[["UNRV"]]$Signature)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{1}-[:digit:]{4}$",
                           emperors[["UNRV"]]$Signature)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{1}-[:digit:]{4}$",
                           emperors[["UNRV"]]$Signature)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{2}-[:digit:]{4}$",
                           emperors[["UNRV"]]$Signature)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{4}$",
                           emperors[["UNRV"]]$Signature)))
    expect_false(any(grepl("^[:alpha:]$",
                           emperors[["UNRV"]]$Signature)))
  }
  if (!is.null(emperors[["UNRV"]]$Term)) {
    expect_equal(class(emperors[["UNRV"]]$Term), "messydt")
    expect_false(any(grepl("/", emperors[["UNRV"]]$Term)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{2}-[:digit:]{4}$",
                           emperors[["UNRV"]]$Term)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{1}-[:digit:]{4}$",
                           emperors[["UNRV"]]$Term)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{1}-[:digit:]{4}$",
                           emperors[["UNRV"]]$Term)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{2}-[:digit:]{4}$",
                           emperors[["UNRV"]]$Term)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{4}$",
                           emperors[["UNRV"]]$Term)))
    expect_false(any(grepl("^[:alpha:]$",
                           emperors[["UNRV"]]$Term)))
  }
  if (!is.null(emperors[["UNRV"]]$Withdrawal)) {
    expect_equal(class(emperors[["UNRV"]]$Withdrawal), "messydt")
    expect_false(any(grepl("/", emperors[["UNRV"]]$Withdrawal)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{2}-[:digit:]{4}$",
                           emperors[["UNRV"]]$Withdrawal)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{1}-[:digit:]{4}$",
                           emperors[["UNRV"]]$Withdrawal)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{1}-[:digit:]{4}$",
                           emperors[["UNRV"]]$Withdrawal)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{2}-[:digit:]{4}$",
                           emperors[["UNRV"]]$Withdrawal)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{4}$",
                           emperors[["UNRV"]]$Withdrawal)))
    expect_false(any(grepl("^[:alpha:]$",
                           emperors[["UNRV"]]$Withdrawal)))
  }
})

# # Dataset should be ordered according to the "Beg" column
# # if the column exists
#   test_that("dataset is arranged by date variable", {
#     if (!is.null(emperors[["UNRV"]]$Beg)) {
#   expect_true(emperors[["UNRV"]]$Beg[1] < emperors[["UNRV"]]$Beg[10])
#   expect_true(emperors[["UNRV"]]$Beg[50] < emperors[["UNRV"]]$Beg[75])
#   expect_true(emperors[["UNRV"]]$Beg[100] < emperors[["UNRV"]]$Beg[120])
#     }
# })
