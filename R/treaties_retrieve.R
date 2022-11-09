#' Retrieve international treaties
#'
#' @description Some databases and datasets across the
#' 'many* packages' (e.g. manyenviron) contain a myriad of information on
#' international treaties governing an international domain.
#' Researchers can, for example, use `retrieve_bilaterals()` to retrieve
#' which countries have signed a specific international agreement, or
#' several international agreements signed in a respective year.
#' As well, researchers can use `retrieve_multilaterals()` to retrieve
#' the titles of all multilateral agreements signed in the past 10 years,
#' for instance.
#' Alternatively, researchers can retrieve treaties that modify,
#' amend, or expand other treaties with `retrieve_links()`.
#' Or, even, researchers can retrieve membership lists of
#' treaty IDs and countries part to a certain treaty with
#' `retrieve_membership_list()`.
#' To retrieve information from several datasets in a database,
#' researchers can also `consolidate()` a database into one dataset
#' with some combination of the rows, columns, and observations of the datasets
#' before getting the desired information.
#' @param dataset A dataset within an agreements or memberships database
#' from one of the many packages.
#' @param database An agreements or memberships database
#' from one of the many packages.
#' @param treaty_type The type of treaties to be returned.
#' Null, by default.
#' @param actor An actor variable.
#' "CountryID", by default.
#' @import dplyr
#' @importFrom stringr str_detect
#' @importFrom purrr map map_chr
#' @importFrom tibble tibble
#' @name retrieve_treaty
NULL

#' @rdname retrieve_treaty
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
#' retrieve_bilaterals(membs)
#' @export
retrieve_bilaterals <- function(dataset, actor = "CountryID") {
  Beg <- Actor <- CountryID1 <- CountryID2 <- Title <- manyID <- NULL
  if (!any(colnames(dataset) == "manyID")) {
    stop("manyID column not found, please declare a many packages dataset.")
  }
  bilats <- subset(dataset, grepl("[A-Z]{3}-", manyID)) %>%
    dplyr::arrange(manyID, actor) %>%
    dplyr::relocate(actor) %>%
    rename(Actor = 1) %>%
    dplyr::select(Actor, manyID, Title, Beg)
  bilats <- dplyr::filter(bilats, manyID %in%
                            names(table(bilats$manyID)[table(bilats$manyID) == 2]))
  bilats1 <- bilats %>%
    dplyr::filter(row_number() %% 2 == 0) %>%
    dplyr::rename(CountryID1 = "Actor")
  bilats2 <- bilats %>%
    dplyr::filter(row_number() %% 2 == 1) %>%
    dplyr::rename(CountryID2 = "Actor")
  bilats <- dplyr::full_join(bilats2, bilats1,
                             by = c("manyID", "Title", "Beg")) %>%
      dplyr::select(CountryID1, CountryID2, Title, Beg)
  bilats
}

#' @rdname retrieve_treaty
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
#' retrieve_multilaterals(membs)
#' @export
retrieve_multilaterals <- function(dataset) {
  manyID <- Title <- Beg <- End <- NULL
  if (!any(colnames(dataset) == "manyID")) {
    stop("manyID column not found, please declare a many packages dataset.")
  }
  multi <- subset(dataset, stringr::str_detect(manyID, "\\-", negate = TRUE)) %>%
    dplyr::filter(manyID %in% names(table(manyID)[table(manyID) != 2]))
  if (!any(colnames(multi) == "End")) {
    multi <- dplyr::select(multi, manyID, Title, Beg, End)
  } else {
    multi <- dplyr::select(multi, manyID, Title, Beg)
  }
  multi
}

#' @name retrieve_treaty
#' @return A tibble of treaty IDs and countries part of the treaty
#' @examples
#' membs <- tibble::tibble(CountryID = c("ROU", "RUS", "DNK"),
#' manyID = c("ROU-RUS[RFP]_1901A", "ROU-RUS[RFP]_1901A", "GD16FI_1901A"))
#' retrieve_membership_list(dataset = membs)
#' @export
retrieve_membership_list <- function(dataset, actor = "CountryID",
                                     treaty_type = NULL) {
  Actor <- Memberships <- manyID <- NULL
  membs_list <- dplyr::select(dataset, manyID, actor) %>%
    rename(Actor = 2)
  if (!is.null(treaty_type)) {
    if (treaty_type == "bilateral") {
      membs_list <- subset(membs_list, stringr::str_detect(manyID, "\\-"))
    }
    if (treaty_type == "multilateral") {
      membs_list <- subset(membs_list, stringr::str_detect(manyID, "\\-",
                                                           negate = TRUE))
      }
  }
  ml <- membs_list %>%
    dplyr::group_by(manyID) %>%
    dplyr::summarise(Memberships = toString(Actor)) %>%
    dplyr::ungroup()
  membs_list <- dplyr::left_join(membs_list, ml, by = "manyID") %>%
    dplyr::select(manyID, Memberships) %>%
    unique()
  membs_list
}

#' @rdname retrieve_treaty
#' @return A tibble of agreements' ID and their links.
#' @examples
#' membs <- tibble::tibble(manyID = c("ROU-RUS[RFP]_1901A",
#' "ROU-RUS[RFP]_1901A:ROU-RUS[RFP]_1901A",
#' "GD16FI_1901A"))
#' retrieve_links(dataset = membs)
#' @export
retrieve_links <- function(database, dataset, treaty_type = NULL) {
  # Get manyID
  if (!missing(database)) {
    treatyID <- unname(unlist(purrr::map(database, "manyID")))
  }
  if (!missing(dataset)) {
    treatyID <- dataset$manyID
  }
  # Filter by links
  treatyID <- grep(":", treatyID, value = TRUE)
  # Filter by treaty_type
  if (!is.null(treaty_type)) {
    if (treaty_type == "bilateral") {
    treatyID <- grep("-", treatyID, value = TRUE)
    }
    if (treaty_type == "multilateral") {
    treatyID <- grep("-", treatyID, value = TRUE, invert = TRUE)
    }
  }
  # Split treatyID
  Link <- purrr::map_chr(strsplit(treatyID, ":"), 2)
  Agreement <- purrr::map_chr(strsplit(treatyID, ":"), 1)
  # Return tibble
  out <- tibble::tibble(Agreement, Link)
  out
}
