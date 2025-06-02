test <- data.frame(preferred_dataset = c(1,6,NA), 
                    more_comprehensive = c(1,3,3), 
                    precise_where_available = c(NA,3.3,4.1))

test_that("unite works", {
  expect_equal(resolve_unite(test), c("{1}","{6,3,3.3}","{3,4.1}"))
  expect_equal(resolve_unite(test, na.rm = FALSE), c("{1,NA}","{6,3,3.3}","{NA,3,4.1}"))
})

test_that("mean works", {
  expect_equal(resolve_mean(test), c(1,4.1,3.55))
  expect_equal(resolve_mean(test, na.rm = FALSE), c(NA,4.1,NA))
})

test_that("median works", {
  expect_equal(resolve_median(test), c(1,3.3,3.55))
  expect_equal(resolve_median(test, na.rm = FALSE), c(NA,3.3,NA))
})

test_that("mode works", {
  expect_equal(resolve_mode(test), c(1,3,3))
  expect_equal(resolve_mode(test, na.rm = FALSE), c(1,3,3))
})

test_that("consensus works", {
  expect_equal(resolve_consensus(test), c(1,NA,NA))
  expect_equal(resolve_consensus(test, na.rm = FALSE), c(NA,NA,NA))
})

test_that("coalesce works", {
  expect_equal(resolve_coalesce(test), c(1,6,3))
  # expect_equal(resolve_coalesce(test, na.rm = FALSE), c("{1,NA}","{6,3,3.3}","{NA,3,4.1}"))
})

test_that("random works", {
  expect_length(resolve_random(test), 3)
  # expect_equal(resolve_random(test, na.rm = FALSE), c("{1,NA}","{6,3,3.3}","{NA,3,4.1}"))
})

test_that("precision works", {
  expect_equal(resolve_precision(test), c(1,3.3,4.1))
  # expect_equal(resolve_precision(test, na.rm = FALSE), c("{1,NA}","{6,3,3.3}","{NA,3,4.1}"))
})

test_that("min works", {
  expect_equal(resolve_min(test), c(1,3,3))
  expect_equal(resolve_min(test, na.rm = FALSE), c(NA,3,NA))
})

test_that("max works", {
  expect_equal(resolve_max(test), c(1,6,4.1))
  expect_equal(resolve_max(test, na.rm = FALSE), c(NA,6,NA))
})
