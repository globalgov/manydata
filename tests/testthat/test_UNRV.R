# Test if  meets the many packages universe requirements

test_that("missing observations are reported correctly", {
  expect_false(any(grepl("^n/a$", emperors[["UNRV"]])))
  expect_false(any(grepl("^N/A$", emperors[["UNRV"]])))
  expect_false(any(grepl("^\\s$", emperors[["UNRV"]])))
  expect_false(any(grepl("^\\.$", emperors[["UNRV"]])))
  expect_false(any(grepl("N\\.A\\.$", emperors[["UNRV"]])))
  expect_false(any(grepl("n\\.a\\.$", emperors[["UNRV"]])))
})

test_that("a column indicating an ID source exists", {
  expect_true(any(grepl("ID", colnames(emperors[["UNRV"]]))))
})

test_that("Columns with dates are standardized", {
  if (!is.null(emperors[["UNRV"]]$Beg)) {
    expect_equal(class(emperors[["UNRV"]]$Beg), "mdate")
    expect_false(any(grepl("/", emperors[["UNRV"]]$Beg)))
    expect_false(any(grepl("^[:alpha:]$",
                           emperors[["UNRV"]]$Beg)))
  }
  if (!is.null(emperors[["UNRV"]]$End)) {
    expect_equal(class(emperors[["UNRV"]]$End), "mdate")
    expect_false(any(grepl("/", emperors[["UNRV"]]$End)))
    expect_false(any(grepl("^[:alpha:]$",
                           emperors[["UNRV"]]$End)))
  }
})

test_that("dataset is arranged by date variable", {
  if (!is.null(emperors[["UNRV"]]$Beg)) {
    expect_true(emperors[["UNRV"]]$Beg[10] < emperors[["UNRV"]]$Beg[20])
  }
})
