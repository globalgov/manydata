# Test if  meets the q ecosystem requirements

test_that("missing observations are reported correctly", {
  expect_false(any(grepl("^n/a$", emperors[["wikipedia"]])))
  expect_false(any(grepl("^N/A$", emperors[["wikipedia"]])))
  expect_false(any(grepl("^\\s$", emperors[["wikipedia"]])))
  expect_false(any(grepl("^\\.$", emperors[["wikipedia"]])))
  expect_false(any(grepl("N\\.A\\.$", emperors[["wikipedia"]])))
  expect_false(any(grepl("n\\.a\\.$", emperors[["wikipedia"]])))
})

test_that("a column indicating an ID source exists", {
  expect_true(any(grepl("ID", colnames(emperors[["wikipedia"]]))))
})

test_that("Columns with dates are standardized", {
  if (!is.null(emperors[["wikipedia"]]$Beg)) {
    expect_equal(class(emperors[["wikipedia"]]$Beg), "messydt")
    expect_false(any(grepl("/", emperors[["wikipedia"]]$Beg)))
    expect_false(any(grepl("^[:alpha:]$",
                           emperors[["wikipedia"]]$Beg)))
  }
  if (!is.null(emperors[["wikipedia"]]$End)) {
    expect_equal(class(emperors[["wikipedia"]]$End), "messydt")
    expect_false(any(grepl("/", emperors[["wikipedia"]]$End)))
    expect_false(any(grepl("^[:alpha:]$",
                           emperors[["wikipedia"]]$End)))
  }
})

test_that("dataset is arranged by date variable", {
    if (!is.null(emperors[["wikipedia"]]$Beg)) {
  expect_true(emperors[["wikipedia"]]$Beg[10] < emperors[["wikipedia"]]$Beg[20])
    }
})
