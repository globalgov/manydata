# Test call_ family of functions
test_that("call_sources() returns the correct output format", {
  so <- call_sources("emperors")
  expect_type(so, "list")
  # expect_length(so, 4)
  # expect_named(so, c('Dataset', 'Source', 'URL', 'Mapping'))
  expect_s3_class(so, "tbl")
})