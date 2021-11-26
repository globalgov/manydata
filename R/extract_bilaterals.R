#' Extracting adjacency edgelist for international agreements
#' 
#' @param membs A memberships dataset from one of the many packages 
#' @description The set of functions help
#' extracting adjacency edgelist for bilaterals agreements in a mixed edgelist,
#' and extracting adjacency edgelist for multilateral agreements
#' in a mixed edgelist.
#' These membership edgelist can be found in memebership databases across
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
  Beg <- CountryID <- CountryID1 <- CountryID2 <- End <- Title <- qID_ref <- NULL
  bilats <- subset(membs, grepl("[A-Z]{3}-", qID_ref)) %>% 
    dplyr::arrange(qID_ref, CountryID) %>% 
    dplyr::select(CountryID, qID_ref, Title, Beg, End)
  bilats <- bilats %>% dplyr::filter(qID_ref %in% names(table(bilats$qID_ref)[table(bilats$qID_ref)==2]))
  bilats1 <- bilats %>% dplyr::filter(row_number() %% 2 == 0) %>% 
    dplyr::rename(CountryID1 = "CountryID")
  bilats2 <- bilats %>% dplyr::filter(row_number() %% 2 == 1) %>% 
    dplyr::rename(CountryID2 = "CountryID")
  bilats <- dplyr::full_join(bilats2, bilats1, 
                             by = c("qID_ref","Title","Beg", "End")) %>% 
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
  qID_ref <- NULLref <- NULL
  subset(membs, !grepl("[A-Z]{3}-", NULLref)) %>% 
    dplyr::filter(qID_ref %in% names(table(qID_ref)[table(qID_ref)!=2]))
}
