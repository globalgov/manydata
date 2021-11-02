#' Extracting adjacency edgelist from bilaterals in a mixed edgelist
#' @name extraction
#' @examples 
#' \dontrun{
#' membs <- qEnviron::memberships$IEADB_MEM
#' extract_bilaterals(membs)
#' extract_multilaterals(membs)
#' }
#' @export
extract_bilaterals <- function(membs){
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

#' @rdname extraction
#' @export
extract_multilaterals <- function(membs){
  subset(membs, !grepl("[A-Z]{3}-", qID_ref)) %>% 
    dplyr::filter(qID_ref %in% names(table(qID_ref)[table(qID_ref)!=2]))
}
