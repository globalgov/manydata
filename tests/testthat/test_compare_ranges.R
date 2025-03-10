# test_that("compare_ranges() returns the correct output format", {
#   # expect_error(compare_ranges(emperors),
#   #              "Please declare one or more variables.")
#   db <- compare_ranges(emperors, variable = c("Begin", "End"))
#   expect_type(db, "list")
#   expect_length(db, 6)
#   expect_s3_class(db, "tbl_df")
#   expect_equal(db$Min[1], "-0026-01-16")
#   expect_equal(db$Max[4], "0518-12-31")
#   expect_equal(db$Mean[5], "0275-04-23")
#   expect_equal(db$Median[6], "0276-09-16")
# })

