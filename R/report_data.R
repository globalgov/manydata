#' Reports on qPackage data
#' 
#' To quickly report metadata on the databases and datasets within a specific qPackage
#' @param pkg character string of the qPackage to report data on. Mandatory input.
#' @param database character string of the qPackage to report data a specific database in a
#' qPackage. If Null, report_data returns a summary of all databases in the qPackage.
#' Null by default.
#' @param dataset character string of the qPackage to report data on a specific dataset
#' in a specific database of a qPackage. If Null and database is specified, returns database
#' level metadata. Null by default.
#' @return A dataframe with the data report
#' @examples
#' report_data(pkg = "qStates", database = "states", dataset = "COW")
#' @export
report_data <- function(pkg, database = NULL, dataset = NULL){
  pkg_path <- find.package(pkg)
  data_path <- file.path(pkg_path, "data")
  #selcts all dbs
  pkg_dbs <- unname(unlist(readRDS(file.path(data_path, "Rdata.rds"))))
  pkg_dbs
  if(!is.null(database)){
    if(is.null(dataset)){
      #report_data("pkg", "database")
      tmp_env <- new.env()
      lazyLoad(file.path(data_path, "Rdata"), envir = tmp_env)
      db <- get(database, envir = tmp_env)
      tabl <- rbind(purrr::map(db, function(x) length(unique(x$ID))),
                        #purrr::map(db, function(x) paste0(sum(is.na(x))/prod(dim(x)), " %")),
                        purrr::map(db, function(x) nrow(x)),
                        purrr::map(db, function(x) ncol(x)),
                        purrr::map(db, function(x) as.character(min(x$Beg))),
                        purrr::map(db, function(x) as.character(max(x$Beg))),
                        purrr::map(db, function(x) attr(x, which = "source_link")),
                        purrr::map(db, function(x) paste0(utils::capture.output(print(attr(x, which = "source_bib"))), sep = "", collapse = "")))
      tabl1 <- tabl %>% 
        t()
      colnames(tabl1) <- c("Unique ID", "Rows", "Columns", "Beg", "End", "URL", "Reference")
      tabl1
    }else{
      #report_data("pkg", "database", "dataset")
      tmp_env <- new.env()
      lazyLoad(file.path(data_path, "Rdata"), envir = tmp_env)
      db <- get(database, envir = tmp_env)
      ds <- db[[dataset]]
      tabl <- data.frame(UniqueID = length(unique(ds$ID)),
                        #MissingValues = paste0(sum(is.na(ds))/prod(dim(ds)), " %"),
                        NObs = nrow(ds),
                        NVar = ncol(ds),
                        MinDate = min(ds$Beg),
                        MaxDate = max(ds$End),
                        URL = attr(ds, which = "source_link"),
                        Reference = paste0(utils::capture.output(print(attr(ds, which = "source_bib"))), sep = "", collapse = "")
      )
      tabl2 <- tabl %>%
        t()
      colnames(tabl2) <- dataset
      tabl2
    }
  }else{
    #report_data("pkg")
    tmp_env <- new.env()
    lazyLoad(file.path(data_path, "Rdata"), envir = tmp_env)
    dbs <- get(pkg_dbs, envir = tmp_env)
    tabl <- rbind(purrr::map(dbs, function(x) length(unique(x$ID))),
                      #purrr::map(dbs, function(x) paste0(sum(is.na(x))/prod(dim(x)), " %")),
                      purrr::map(dbs, function(x) nrow(x)),
                      purrr::map(dbs, function(x) ncol(x)),
                      purrr::map(dbs, function(x) as.character(min(x$Beg))),
                      purrr::map(dbs, function(x) as.character(max(x$Beg))),
                      purrr::map(dbs, function(x) attr(x, which = "source_link")),
                      purrr::map(dbs, function(x) paste0(utils::capture.output(print(attr(x, which = "source_bib"))), sep = "", collapse = "")))
    
    tabl3 <- tabl %>%
      t()
    
    colnames(tabl3) <- c("Unique ID", "Rows", "Columns", "Beg", "End", "URL", "Reference")
    tabl3
    
  }
}
