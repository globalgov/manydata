data <- data.frame(letters = letters[1:10],
                   numbers = 1:10,
                   colours = rainbow(10))

test_that("columns ordered correctly",{
  expect_equal(rearrange(data, "letters", "last"), data[,c(2,3,1)])
  expect_equal(rearrange(data, "letters", "after", "numbers"), data[,c(2,1,3)])
  expect_equal(rearrange(data, "letters", "before", "colours"), data[,c(2,1,3)])
  expect_equal(rearrange(data, "colours", "first"), data[,c(3,1,2)])
})

test_that("refva specified when necessary",{
  expect_error(rearrange(data, "letters", "after"), "must specify refva column")
  expect_error(rearrange(data, "letters", "before"), "must specify refva column")
})

test_that("refva specified correctly",{
  expect_error(rearrange(data, "letters", "after", c("numbers", "colours")),
               "refva must be a single character string")
  expect_error(rearrange(data, "letters", "before", c("numbers", "colours")),
               "refva must be a single character string")
})
