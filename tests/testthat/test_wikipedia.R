# Test if  meets the q ecosystem requirements

# Report missing values
test_that("missing observations are reported correctly", {
  expect_false(any(grepl("\\?", emperors[["wikipedia"]])))
  expect_false(any(grepl("^n/a$", emperors[["wikipedia"]])))
  expect_false(any(grepl("^N/A$", emperors[["wikipedia"]])))
  expect_false(any(grepl("^\\s$", emperors[["wikipedia"]])))
  expect_false(any(grepl("^\\.$", emperors[["wikipedia"]])))
  expect_false(any(grepl("N\\.A\\.$", emperors[["wikipedia"]])))
  expect_false(any(grepl("n\\.a\\.$", emperors[["wikipedia"]])))
})

# At least one column named ID
test_that("a column indicating an ID source exists", {
  expect_true(any(grepl("_ID$", colnames(emperors[["wikipedia"]]))))
})

# Labels are standardized
test_that("labels are standardised", {
  if (!is.null(emperors[["wikipedia"]]$Label)) {
  expect_false(any(grepl("U.S.", emperors[["wikipedia"]])))
  expect_false(any(grepl("U.K.", emperors[["wikipedia"]])))
  expect_false(any(grepl("!", emperors[["wikipedia"]])))
  expect_false(any(grepl("NANA.", emperors[["wikipedia"]])))
  }
})

# Dates are standardized
test_that("Columns with dates are standardized", {
  if (!is.null(emperors[["wikipedia"]]$Beg)) {
    expect_equal(class(emperors[["wikipedia"]]$Beg), "messydt")
    expect_false(any(grepl("/", emperors[["wikipedia"]]$Beg)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{2}-[:digit:]{4}$",
                           emperors[["wikipedia"]]$Beg)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{1}-[:digit:]{4}$",
                           emperors[["wikipedia"]]$Beg)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{1}-[:digit:]{4}$",
                           emperors[["wikipedia"]]$Beg)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{2}-[:digit:]{4}$",
                           emperors[["wikipedia"]]$Beg)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{4}$",
                           emperors[["wikipedia"]]$Beg)))
    expect_false(any(grepl("^[:alpha:]$",
                           emperors[["wikipedia"]]$Beg)))
  }
  if (!is.null(emperors[["wikipedia"]]$End)) {
    expect_equal(class(emperors[["wikipedia"]]$End), "messydt")
    expect_false(any(grepl("/", emperors[["wikipedia"]]$End)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{2}-[:digit:]{4}$",
                           emperors[["wikipedia"]]$End)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{1}-[:digit:]{4}$",
                           emperors[["wikipedia"]]$End)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{1}-[:digit:]{4}$",
                           emperors[["wikipedia"]]$End)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{2}-[:digit:]{4}$",
                           emperors[["wikipedia"]]$End)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{4}$",
                           emperors[["wikipedia"]]$End)))
    expect_false(any(grepl("^[:alpha:]$",
                           emperors[["wikipedia"]]$End)))
  }
  if (!is.null(emperors[["wikipedia"]]$Force)) {
    expect_equal(class(emperors[["wikipedia"]]$Force), "messydt")
    expect_false(any(grepl("/", emperors[["wikipedia"]]$Force)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{2}-[:digit:]{4}$",
                           emperors[["wikipedia"]]$Force)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{1}-[:digit:]{4}$",
                           emperors[["wikipedia"]]$Force)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{1}-[:digit:]{4}$",
                           emperors[["wikipedia"]]$Force)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{2}-[:digit:]{4}$",
                           emperors[["wikipedia"]]$Force)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{4}$",
                           emperors[["wikipedia"]]$Force)))
    expect_false(any(grepl("^[:alpha:]$",
                           emperors[["wikipedia"]]$Force)))
  }
  if (!is.null(emperors[["wikipedia"]]$Rat)) {
    expect_equal(class(emperors[["wikipedia"]]$Rat), "messydt")
    expect_false(any(grepl("/", emperors[["wikipedia"]]$Rat)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{2}-[:digit:]{4}$",
                           emperors[["wikipedia"]]$Rat)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{1}-[:digit:]{4}$",
                           emperors[["wikipedia"]]$Rat)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{1}-[:digit:]{4}$",
                           emperors[["wikipedia"]]$Rat)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{2}-[:digit:]{4}$",
                           emperors[["wikipedia"]]$Rat)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{4}$",
                           emperors[["wikipedia"]]$Rat)))
    expect_false(any(grepl("^[:alpha:]$",
                           emperors[["wikipedia"]]$Rat)))
  }
  if (!is.null(emperors[["wikipedia"]]$Signature)) {
    expect_equal(class(emperors[["wikipedia"]]$Signature), "messydt")
    expect_false(any(grepl("/", emperors[["wikipedia"]]$Signature)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{2}-[:digit:]{4}$",
                           emperors[["wikipedia"]]$Signature)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{1}-[:digit:]{4}$",
                           emperors[["wikipedia"]]$Signature)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{1}-[:digit:]{4}$",
                           emperors[["wikipedia"]]$Signature)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{2}-[:digit:]{4}$",
                           emperors[["wikipedia"]]$Signature)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{4}$",
                           emperors[["wikipedia"]]$Signature)))
    expect_false(any(grepl("^[:alpha:]$",
                           emperors[["wikipedia"]]$Signature)))
  }
  if (!is.null(emperors[["wikipedia"]]$Term)) {
    expect_equal(class(emperors[["wikipedia"]]$Term), "messydt")
    expect_false(any(grepl("/", emperors[["wikipedia"]]$Term)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{2}-[:digit:]{4}$",
                           emperors[["wikipedia"]]$Term)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{1}-[:digit:]{4}$",
                           emperors[["wikipedia"]]$Term)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{1}-[:digit:]{4}$",
                           emperors[["wikipedia"]]$Term)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{2}-[:digit:]{4}$",
                           emperors[["wikipedia"]]$Term)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{4}$",
                           emperors[["wikipedia"]]$Term)))
    expect_false(any(grepl("^[:alpha:]$",
                           emperors[["wikipedia"]]$Term)))
  }
  if (!is.null(emperors[["wikipedia"]]$Withdrawal)) {
    expect_equal(class(emperors[["wikipedia"]]$Withdrawal), "messydt")
    expect_false(any(grepl("/", emperors[["wikipedia"]]$Withdrawal)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{2}-[:digit:]{4}$",
                           emperors[["wikipedia"]]$Withdrawal)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{1}-[:digit:]{4}$",
                           emperors[["wikipedia"]]$Withdrawal)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{1}-[:digit:]{4}$",
                           emperors[["wikipedia"]]$Withdrawal)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{2}-[:digit:]{4}$",
                           emperors[["wikipedia"]]$Withdrawal)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{4}$",
                           emperors[["wikipedia"]]$Withdrawal)))
    expect_false(any(grepl("^[:alpha:]$",
                           emperors[["wikipedia"]]$Withdrawal)))
  }
})

# Dataset should be ordered according to the "Beg" column
# if the column exists
  test_that("dataset is arranged by date variable", {
    if (!is.null(emperors[["wikipedia"]]$Beg)) {
  expect_true(emperors[["wikipedia"]]$Beg[1] < emperors[["wikipedia"]]$Beg[10])
  expect_true(emperors[["wikipedia"]]$Beg[50] < emperors[["wikipedia"]]$Beg[75])
  expect_true(emperors[["wikipedia"]]$Beg[100] < emperors[["wikipedia"]]$Beg[120])
    }
})
