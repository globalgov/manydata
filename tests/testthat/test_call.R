# Test call_ family of functions

test_that("call_sources() returns the correct output format", {
  so <- call_sources("manydata", "emperors")
  expect_type(so, "list")
  expect_length(so, 4)
  expect_named(so, c('Dataset', 'Source', 'URL', 'Mapping'))
  expect_s3_class(so, "tbl")
})

membs <- dplyr::tibble(StateID = c("ROU", "RUS", "DNK"),
                        manyID = c("ROU-RUS[RFP]_1901A", "ROU-RUS[RFP]_1901A", "GD16FI_1901A"),
                        Title = c("Convention Between Roumania And Russia Concerning Fishing",
                                  "Convention Between Roumania And Russia Concerning Fishing",
                                  "Convention Between The Governments Of Denmark And ..."),
                        Beg = c("1901-02-22", "1901-02-22", "1901-06-24"),
                        End = c(NA, NA, NA),
                        Text = c("treaty 1", "treaty 2", "treaty 3"))
bilats <- dplyr::tibble(manyID = "ROU-RUS[RFP]_1901A",
                         Title = "Convention Between Roumania And Russia Concerning Fishing",
                         Beg = "1901-02-22")
multi <- dplyr::tibble(manyID = "GD16FI_1901A",
                        Title = "Convention Between The Governments Of Denmark And ...",
                        Beg = "1901-06-24")
mlist <- dplyr::tibble(manyID = c("GD16FI_1901A", "ROU-RUS[RFP]_1901A"),
                        Memberships = c("DNK", "ROU, RUS"))
manyid <- dplyr::tibble(manyID = c("ROU-RUS[RFP]_1901A", "GD16FI_1901A"))

test_that("retrieve functions work", {
  expect_equal(call_treaties(membs), manyid)
  expect_equal(call_treaties(membs, treaty_type = "bilateral",
                             variable = c("Title", "Beg")), bilats)
  expect_equal(call_treaties(membs, treaty_type = "multilateral",
                             variable = c("Title", "Beg")), multi)
  expect_equal(call_treaties(membs, actor = "StateID"), mlist)
})
