#' Call treaties from 'many' datasets
#' 
#' @details Certain datasets, or consolidated datacubes, in 'many' packages
#' contains information on treaties which can be retrieved
#' with `call_treaties()`.
#' @family call_
#' @param dataset A dataset in a datacube from one of the many packages.
#' NULL by default.
#' That is, all datasets in the datacube are used.
#' For multiple datasets, please declare datasets as a vector
#' (e.g. c("dataset1", "dataset2")).
#' @param treaty_type The type of treaties to be returned.
#' NULL, by default.
#' Other options are "bilateral" or "multilateral".
#' @param variable Would you like to get one, or more, specific variables
#' present in one or more datasets in the 'many' datacube?
#' NULL by default.
#' For multiple variables, please declare variable names as a vector.
#' @param actor An actor variable in dataset.
#' NULL by default.
#' If declared, a tibble of the treaties and their member actors is returned.
#' @param key A variable key to join datasets.
#' 'manyID' by default.
#' @examples
#' \donttest{
#' membs <- dplyr::tibble(manyID = c("ROU-RUS[RFP]_1901A",
#' "ROU-RUS[RFP]_1901A", "GD16FI_1901A"),
#' stateID = c("ROU", "RUS", "DNK"),
#' Title = c("Convention Between Roumania And Russia Concerning Fishing
#' In The Danube And The Pruth",
#' "Convention Between Roumania And Russia Concerning Fishing
#' In The Danube And The Pruth",
#' "Convention Between The Governments Of Denmark And
#' The United Kingdom Of Great Britain
#' And Northern Ireland For Regulating The Fisheries
#' Of Their Respective Subjects Outside
#' Territorial Waters In The Ocean Surrounding The Faroe Islands"),
#' Begin = c("1901-02-22", "1901-02-22", "1901-06-24"))
#' call_treaties(membs)
#' call_treaties(membs, treaty_type = "bilateral",
#' variable = c("Title", "Begin"))
#' call_treaties(membs, variable = c("Title", "Begin"), actor = "stateID")
#' }
#' @return
#' `call_treaties()` returns a tibble with a list of the agreements.
#' @export
call_treaties <- function(dataset, treaty_type = NULL, variable = NULL,
                          actor = NULL, key = "manyID") {
  Memberships <- manyID <- NULL
  # check if key is valid
  if (key != "manyID" & key != "treatyID") {
    stop("Please declare either 'manyID' or 'treatyID'.")
  }
  # get variables, if declared
  if (!is.null(variable)) {
    out <- dataset[,c(key, variable)] %>% dplyr::distinct()
  } else {
    out <- dataset[,key] %>% dplyr::distinct()
  }
  # subset treaty types
  if (!is.null(treaty_type)) {
    if (treaty_type == "bilateral") {
      out <- subset(out, stringr::str_detect(manyID, "\\-"))
    }
    if (treaty_type == "multilateral") {
      out <- subset(out, stringr::str_detect(manyID, "\\-", negate = TRUE))
    }
  }
  # get memebership lists, if actor is declared
  if (!is.null(actor)) {
    actors <- dataset[,c(key, actor)] %>% dplyr::distinct()
    names(actors)[names(actors) == actor] <- "Memberships"
    out <- actors %>%
      dplyr::group_by(manyID) %>%
      dplyr::summarise(Memberships = toString(Memberships)) %>%
      dplyr::ungroup() %>%
      dplyr::right_join(out, by = key) %>%
      dplyr::distinct()
  }
  out
}

