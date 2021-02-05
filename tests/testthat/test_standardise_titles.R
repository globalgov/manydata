test_that("capitalisation works",{
  expect_match(standardise_titles("A treaty to do things"), "A Treaty To Do Things")
})

test_that("white space is stripped",{
  expect_match(standardise_titles("A treaty to do things  "), "A Treaty To Do Things")
})

test_that("end of a sentence points are taken off",{
  expect_match(standardise_titles("A treaty to do things."), "A Treaty To Do Things")
})