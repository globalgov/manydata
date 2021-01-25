#' @param pkg character string of the qPackage to report data on
#' @export
report_data <- function(pkg){
  pkg_path <- find.package(pkg)
  data_path <- file.path(pkg_path, "data")
  pkg_dbs <- unname(unlist(readRDS(file.path(data_path, "Rdata.rds"))))
  tmp_env <- new.env()
  lazyLoad(file.path(data_path, "Rdata"), envir = tmp_env)
  dbs <- get(pkg_dbs, envir = tmp_env)
  tabl <- bind_rows(purrr::map(dbs, function(x) length(unique(x$ID))),
            purrr::map(dbs, function(x) sum(is.na(x))),
            purrr::map(dbs, function(x) nrow(x)),
            purrr::map(dbs, function(x) ncol(x)))
            # purrr::map(dbs, function(x) min(x$Beg)),
            # purrr::map(dbs, function(x) max(x$End)))
   
  tabl3 <- tabl %>% 
    t()
  
  colnames(tabl3) <- c("Unique ID", "Missing data", "Rows", "Columns")
  tabl3
  
  
  # tibble: for each database
  
  # columns/variables should be:
  #  - name of dataset => ALready showing? 
  #  - source of dataset AT THE END
  #  - number of unique IDs DONE
  #  - number of observations (rows)
  #  - number of variables (cols)
  #  - internally missing data (%)
  #  - earliest begin date
  #  - most recent (non-future) end date
  
  # attr(states[["COW"]], "source") <- "bloobloo"
  
}
