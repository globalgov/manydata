# Test for compare_ family of functions

test_that("plot for compare_categories returns the correct output format", {
  db <- plot(compare_categories(datacube = emperors, key = "ID"))
  expect_type(db, "list")
  expect_length(db, 11)
  expect_true(ggplot2::is_ggplot(db))
  expect_named(db, names(ggplot2::ggplot()))
})

test_that("compare_categories() returns the correct output format", {
  db <- compare_categories(datacube = emperors, key = "ID")
  db1 <- compare_categories(datacube = emperors, key = "ID", variable = "Begin")
  db2 <- compare_categories(datacube = emperors, key = "ID",
                            variable = c("Begin", "End"), category = "conflict")
  expect_type(db, "list")
  expect_type(db1, "list")
  expect_type(db2, "list")
  expect_s3_class(db, "tbl")
  expect_s3_class(db1, "tbl")
  expect_s3_class(db2, "tbl")
  expect_length(db, 35)
  expect_length(db1, 5)
  expect_length(db2, 9)
  expect_equal(nrow(db), nrow(db1))
  expect_named(db1, c("ID", "Wikipedia$Begin", "UNRV$Begin",
                      "Britannica$Begin", "Begin (3)"))
  expect_true(db2[1,5] == db2[10,5])
})

test_that("compare_dimensions() returns the correct output format", {
  db <- compare_dimensions(emperors)
  expect_type(db, "list")
  expect_length(db, 5)
  expect_s3_class(db, "tbl_df")
  expect_equal(db$Earliest_Date,
               c("-0062-09-23", "-0063-01-01", "-0031-01-01"))
  expect_equal(db$Latest_Date,
               c("0421-09-02", "0518-12-31", "0491-12-31"))
})

test_that("compare_overlap() and return the correct output format", {
  db <- compare_overlap(emperors, key = "ID")
  expect_type(db, "list")
  expect_length(db, 2)
  expect_s3_class(db, "tbl_df")
  pl <- plot(db)
  expect_type(pl, "list")
  expect_length(pl, length(ggplot2::ggplot()))
  expect_true(ggplot2::is_ggplot(pl))
  expect_named(pl, names(ggplot2::ggplot()))
})

test_that("compare_missing() and plot_missing() returns the correct output format", {
  db <- compare_missing(emperors)
  expect_type(db, "list")
  expect_length(db, 6)
  expect_s3_class(db, "tbl_df")
  pl <- plot(db)
  expect_type(pl, "list")
  expect_length(pl, length(ggplot2::ggplot()))
  expect_true(ggplot2::is_ggplot(pl))
  expect_named(pl, names(ggplot2::ggplot()))
})
