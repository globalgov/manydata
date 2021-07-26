# Test if  meets the q ecosystem requirements

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
  if (!is.null(emperors[["britannica"]]$Beg)) {
    expect_equal(class(emperors[["britannica"]]$Beg), "messydt")
    expect_false(any(grepl("/", emperors[["britannica"]]$Beg)))
    expect_false(any(grepl("^[:alpha:]$",
                           emperors[["britannica"]]$Beg)))
  }
  if (!is.null(emperors[["britannica"]]$End)) {
    expect_equal(class(emperors[["britannica"]]$End), "messydt")
    expect_false(any(grepl("/", emperors[["britannica"]]$End)))
    expect_false(any(grepl("^[:alpha:]$",
                           emperors[["britannica"]]$End)))
  }
})

test_that("dataset is arranged by date variable", {
  if (!is.null(emperors[["britannica"]]$Beg)) {
    expect_true(emperors[["britannica"]]$Beg[10] <
                  emperors[["britannica"]]$Beg[20])
  }
})
