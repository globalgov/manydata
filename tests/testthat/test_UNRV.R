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
  if (!is.null(emperors[["UNRV"]]$Begin)) {
    expect_equal(class(emperors[["UNRV"]]$Begin), "mdate")
    expect_false(any(grepl("/", emperors[["UNRV"]]$Begin)))
    expect_false(any(grepl("^[:alpha:]$",
                           emperors[["UNRV"]]$Begin)))
  }
  if (!is.null(emperors[["UNRV"]]$End)) {
    expect_equal(class(emperors[["UNRV"]]$End), "mdate")
    expect_false(any(grepl("/", emperors[["UNRV"]]$End)))
    expect_false(any(grepl("^[:alpha:]$",
                           emperors[["UNRV"]]$End)))
  }
})

test_that("dataset is arranged by date variable", {
  if (!is.null(emperors[["UNRV"]]$Begin)) {
    expect_true(emperors[["UNRV"]]$Begin[10] < emperors[["UNRV"]]$Begin[20])
  }
})
