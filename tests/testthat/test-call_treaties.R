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

test_that("call treaties works", {
  expect_equal(call_treaties(membs), manyid)
  expect_equal(call_treaties(membs, treaty_type = "bilateral",
                             variable = c("Title", "Beg")), bilats)
  expect_equal(call_treaties(membs, treaty_type = "multilateral",
                             variable = c("Title", "Beg")), multi)
  expect_equal(call_treaties(membs, actor = "StateID"), mlist)
})

test_that("call releases visualises historical
          milestones/releases of a repository", {
            testdf <- data.frame(tag_name = c("v0.1.0", "v0.1.1"),
                                 date = c("2021-04-01", "2021-05-01"),
                                 milestone = c("Minor", "Patch"))
            testplot <- call_releases(testdf)
            expect_true(is.list(testplot))
            expect_length(testplot, length(ggplot2::ggplot()))
            expect_named(testplot[1:3], c("data", "layers", "scales"))
})
