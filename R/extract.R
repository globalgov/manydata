#' Extracting international agreements
#' 
#' @param membs A memberships dataset from one of the many packages
#' @description This set of functions help extracting bilateral agreements or
#' multilateral agreements from a membership database
#' in the many packages universe.
#' @name extraction
NULL

#' @name extraction
#' @details The function helps extracting bilateral agreements
#' from a membership database in the many packages universe.
#' @import dplyr
#' @return A tibble of bilateral agreements
#' @examples
#' membs <- tibble::tibble(CountryID = c("ROU", "RUS", "DNK"),
#' manyID = c("ROU-RUS[RFP]_1901A", "ROU-RUS[RFP]_1901A", "GD16FI_1901A"),
#' Title = c("Convention Between Roumania And Russia Concerning Fishing
#' In The Danube And The Pruth",
#' "Convention Between Roumania And Russia Concerning Fishing
#' In The Danube And The Pruth",
#' "Convention Between The Governments Of Denmark And
#' The United Kingdom Of Great Britain
#' And Northern Ireland For Regulating The Fisheries
#' Of Their Respective Subjects Outside
#' Territorial Waters In The Ocean Surrounding The Faroe Islands"),
#' Beg = c("1901-02-22", "1901-02-22", "1901-06-24"),
#' End = c(NA, NA, NA))
#' extract_bilaterals(membs)
#' @export
extract_bilaterals <- function(membs) {
  Beg <- CountryID <- CountryID1 <- CountryID2 <- End <- Title <- manyID <- NULL
  if (!any(colnames(membs) == "manyID")) {
    stop("manyID column not found, please declare a many packages dataset.")
  }
  bilats <- subset(membs, grepl("[A-Z]{3}-", manyID)) %>%
    dplyr::arrange(manyID, CountryID) %>%
    dplyr::select(CountryID, manyID, Title, Beg, End)
  bilats <- bilats %>%
    dplyr::filter(manyID %in%
                    names(table(bilats$manyID)[table(bilats$manyID) == 2]))
  bilats1 <- bilats %>%
    dplyr::filter(row_number() %% 2 == 0) %>%
    dplyr::rename(CountryID1 = "CountryID")
  bilats2 <- bilats %>%
    dplyr::filter(row_number() %% 2 == 1) %>%
    dplyr::rename(CountryID2 = "CountryID")
  bilats <- dplyr::full_join(bilats2, bilats1,
                             by = c("manyID", "Title", "Beg", "End")) %>%
    dplyr::select(CountryID1, CountryID2, Title, Beg, End)
  bilats
}

#' @name extraction
#' @details The function helps extracting multilateral agreements
#' from a membership database in the many packages universe.
#' @importFrom dplyr filter
#' @importFrom stringr str_detect
#' @return A tibble of multilateral agreements
#' @examples
#' membs <- tibble::tibble(CountryID = c("ROU", "RUS", "DNK"),
#' manyID = c("ROU-RUS[RFP]_1901A", "ROU-RUS[RFP]_1901A", "GD16FI_1901A"),
#' Title = c("Convention Between Roumania And Russia Concerning Fishing
#' In The Danube And The Pruth",
#' "Convention Between Roumania And Russia Concerning Fishing
#' In The Danube And The Pruth",
#' "Convention Between The Governments Of Denmark And
#' The United Kingdom Of Great Britain
#' And Northern Ireland For Regulating The Fisheries
#' Of Their Respective Subjects Outside
#' Territorial Waters In The Ocean Surrounding The Faroe Islands"),
#' Beg = c("1901-02-22", "1901-02-22", "1901-06-24"),
#' End = c(NA, NA, NA))
#' extract_multilaterals(membs)
#' @export
extract_multilaterals <- function(membs) {
  manyID <- Title <- Beg <- End <- NULL
  if (!any(colnames(membs) == "manyID")) {
    stop("manyID column not found, please declare a many packages dataset.")
  }
  multi <- subset(membs, stringr::str_detect(manyID, "\\-", negate = TRUE)) %>%
    dplyr::filter(manyID %in% names(table(manyID)[table(manyID) != 2])) %>%
    dplyr::select(manyID, Title, Beg, End)
  multi
}
