test_that("capitalisation works",{
  expect_match(entitle("A treaty to do things"), "A Treaty To Do Things")
})

test_that("white space is stripped",{
  expect_match(entitle("A treaty to do things  "), "A Treaty To Do Things")
})

test_that("end of a sentence points are taken off",{
  expect_match(entitle("A treaty to do things."), "A Treaty To Do Things")
})
