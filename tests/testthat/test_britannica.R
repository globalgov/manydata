# Test if  meets the many packages universe requirements

test_that("missing observations are reported correctly", {
  expect_false(any(grepl("^n/a$", emperors[["britannica"]])))
  expect_false(any(grepl("^N/A$", emperors[["britannica"]])))
  expect_false(any(grepl("^\\s$", emperors[["britannica"]])))
  expect_false(any(grepl("^\\.$", emperors[["britannica"]])))
  expect_false(any(grepl("N\\.A\\.$", emperors[["britannica"]])))
  expect_false(any(grepl("n\\.a\\.$", emperors[["britannica"]])))
})

test_that("a column indicating an ID source exists", {
  expect_true(any(grepl("ID", colnames(emperors[["britannica"]]))))
})

test_that("Columns with dates are standardized", {
  if (!is.null(emperors[["britannica"]]$Begin)) {
    expect_equal(class(emperors[["britannica"]]$Begin), "mdate")
    expect_false(any(grepl("/", emperors[["britannica"]]$Begin)))
    expect_false(any(grepl("^[:alpha:]$",
                           emperors[["britannica"]]$Begin)))
  }
  if (!is.null(emperors[["britannica"]]$End)) {
    expect_equal(class(emperors[["britannica"]]$End), "mdate")
    expect_false(any(grepl("/", emperors[["britannica"]]$End)))
    expect_false(any(grepl("^[:alpha:]$",
                           emperors[["britannica"]]$End)))
  }
})

test_that("dataset is arranged by date variable", {
  skip_on_cran()
  if (!is.null(emperors[["britannica"]]$Begin)) {
    expect_true(emperors[["britannica"]]$Begin[10] <
                  emperors[["britannica"]]$Begin[20])
  }
})
