
#' @rdname find
#' @param id_col The name of the column containing IDs. 
#'   Default is "ID".
#' @examples
#' find_duplicates(emperors$Wikipedia)
#' @export
find_duplicates <- function(df, id_col = "ID"){
  
  # Check if the data frame has a '..ID' column
  if (!id_col %in% names(df)) {
    id_col <- names(df)[endsWith(names(df), "ID")][1]
    if(is.null(id_col)) {
    stop(paste("The data frame does not contain a", id_col, "column."))
  }}
  
  # Find the duplicates for the specified ID column
  out <- df[(duplicated(df[[id_col]][!is.na(df[[id_col]])], 
                 fromLast = TRUE) | 
        duplicated(df[[id_col]][!is.na(df[[id_col]])], 
                   fromLast = FALSE)),]
  if(nrow(out)==0){
    message(paste("There are no duplicate IDs in the", id_col, "column."))
    return(NULL)
  } else out
}
