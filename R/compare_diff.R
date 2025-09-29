#' Compare two datasets for differences
#' @name compare_diff
#' @param .data1 First dataset to compare
#' @param .data2 Second dataset to compare
#' @param by Column name to join on (default is "ID")
#' @return A data frame with the differences found
#' @details This function uses `dplyr::anti_join` to find rows in `.data1` 
#'   that are not present in `.data2`.
#'   If no differences are found, a message is printed and `NULL` is returned.
#'   If differences are found, they are returned as a data frame.
#' @importFrom dplyr anti_join semi_join

#' @rdname compare_diff
#' @examples
#' \dontrun{
#' df1 <- data.frame(ID = 1:5, Value = letters[1:5])
#' df2 <- data.frame(ID = 3:7, Value = letters[3:7])
#' compare_new(df1, df2)
#' compare_new(df1, df1)
#' }
#' @export
compare_new <- function(.data1, .data2, by = "ID"){
  
  if(by == "ID" && !("ID" %in% names(.data1) && "ID" %in% names(.data2))){
    id1 <- find_ID(.data1)
    id2 <- find_ID(.data2)
    if(id1 == id2){
      by <- id1
    } else stop("Column 'ID' not found in both datasets. Please specify the 'by' parameter.")
  }
  
  # Find differences
  diff <- dplyr::anti_join(.data1, .data2, by = by)
  
  if (nrow(diff) == 0) {
    message("No differences found.")
    return(NULL)
  } else {
    return(diff)
  }
}

#' @rdname compare_diff
#' @param exclude Character vector of column names to exclude from comparison.
#'   By default, "Title", "Coder", and "Comments" are excluded.
#' @param diff_threshold Integer specifying the minimum number of differing 
#'   columns for a row to be included in the output. 
#'   Default is 0, meaning any difference will be included.
#'   Set to 3 to only show rows with at least 3 differing columns.
#' @examples
#' compare_diff(emperors$Wikipedia, emperors$Britannica)
#' @export
compare_diff <- function(.data1, .data2, by = "ID",
                         exclude = c("Title","Coder","Comments"),
                         diff_threshold = 0){
  
  if(by == "ID" && !("ID" %in% names(.data1) && "ID" %in% names(.data2))){
    by <- find_common_ID(.data1, .data2)
  }
  
  mat1 <- as.matrix(.data1)
  rownames(mat1) <- .data1[[by]]
  mat2 <- as.matrix(.data2)
  rownames(mat2) <- .data2[[by]]
  
  mat1 <- mat1[rownames(mat1) %in% rownames(mat2), ]
  mat2 <- mat2[rownames(mat2) %in% rownames(mat1), ]
  mat1 <- mat1[!duplicated(rownames(mat1)), ]
  mat2 <- mat2[!duplicated(rownames(mat1)), ]
  
  mat1 <- mat1[,colnames(mat1) %in% colnames(mat2)]
  mat2 <- mat2[,colnames(mat2) %in% colnames(mat1)]
  
  allDiffs <- mat1 != mat2
  allDiffs[is.na(allDiffs)] <- FALSE
  allDiffs[,colnames(allDiffs) %in% c("Title","StateName")] <- TRUE
  out <- mat1
  out[!allDiffs] <- NA
  # exclude <- c("Title","Coder","Comments")
  out <- out[rowSums(!is.na(out[ , !(colnames(out) %in% exclude)])) > diff_threshold, ]
  out <- out[,colnames(out) != by]
  out

  # Find differences
  # diff <- dplyr::semi_join(.data1, .data2, by = by)
  # 
  # if (nrow(diff) == 0) {
  #   message("No differences found.")
  #   return(NULL)
  # } else {
  #   return(diff)
  # }
}