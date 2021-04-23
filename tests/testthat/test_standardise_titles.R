test_that("capitalisation works",{
  expect_match(standardise_titles("A treaty to do things"), "A Treaty To Do Things")
})

test_that("white space is stripped",{
  expect_match(standardise_titles("A treaty to do things  "), "A Treaty To Do Things")
})

test_that("end of a sentence points are taken off",{
  expect_match(standardise_titles("A treaty to do things."), "A Treaty To Do Things")
})

test_that("titles in other languages than english are translated",{
  expect_match(standardise_titles("La Convention Pour La Protection De La Flore, De La Faune Et Des Beautés Panoramiques Naturelles Des Pays De l'Amérique"), "The Convention for the Protection of Flora, Fauna and Panoramic Natural Beauty of the Countries of America")
})
