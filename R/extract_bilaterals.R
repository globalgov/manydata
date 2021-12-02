#' Extracting adjacency edgelist for international agreements
#' @param membs A memberships dataset from one of the many packages 
#' @description The set of functions help
#' extracting adjacency edgelist for bilaterals agreements in a mixed edgelist,
#' and extracting adjacency edgelist for multilateral agreements
#' in a mixed edgelist.
#' These membership edgelists can be found in membership databases across
#' the many package universe.
#' @name extraction
NULL

#' @name extraction
#' @details The function helps extracting adjacency edgelist
#' for bilaterals agreements in a mixed edgelist.
#' @import dplyr
#' @return An edgelist for bilaterals agreements
#' @examples 
#' \dontrun{
#' membs <- manyenviron::memberships$IEADB_MEM
#' extract_bilaterals(membs)
#' }
#' @export
extract_bilaterals <- function(membs){
  Beg <- CountryID <- CountryID1 <- CountryID2 <- End <- Title <- many_ID <- NULL
  bilats <- subset(membs, grepl("[A-Z]{3}-", many_ID)) %>% 
    dplyr::arrange(many_ID, CountryID) %>% 
    dplyr::select(CountryID, many_ID, Title, Beg, End)
  bilats <- bilats %>% dplyr::filter(many_ID %in% names(table(bilats$many_ID)[table(bilats$many_ID)==2]))
  bilats1 <- bilats %>% dplyr::filter(row_number() %% 2 == 0) %>% 
    dplyr::rename(CountryID1 = "CountryID")
  bilats2 <- bilats %>% dplyr::filter(row_number() %% 2 == 1) %>% 
    dplyr::rename(CountryID2 = "CountryID")
  bilats <- dplyr::full_join(bilats2, bilats1, 
                             by = c("many_ID","Title","Beg", "End")) %>% 
    dplyr::select(CountryID1, CountryID2, Title, Beg, End)
  bilats
}

#' @name extraction
#' @details The function helps extracting adjacency edgelist
#' for multilateral agreements in a mixed edgelist.
#' @importFrom dplyr filter
#' @return An edgelist for multilateral agreements
#' @examples
#' \dontrun{
#' membs <- manyenviron::memberships$IEADB_MEM
#' extract_multilaterals(membs)
#' }
#' @export
extract_multilaterals <- function(membs){
  many_ID <- NULLref <- NULL
  subset(membs, !grepl("[A-Z]{3}-", NULLref)) %>% 
    dplyr::filter(many_ID %in% names(table(many_ID)[table(many_ID)!=2]))
}
