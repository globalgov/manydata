#' To quickly report on the databases and datasets within a qPackage
#' 
#' @param pkg character string of the qPackage to report data on various levels of data.
#' @param dbase character string of the qPackage to report data on various levels of data.Null by default.
#' @param dset character string of the qPackage to report data on various levels of data.Null by default.
#' @export
report_data <- function(pkg, dbase=NULL, dset=NULL){
  pkg_path <- find.package(pkg)
  data_path <- file.path(pkg_path, "data")
  dbase_path <- file.path(pkg_path, "data", dbase)
  #selcts all dbs
  pkg_dbs <- unname(unlist(readRDS(file.path(data_path, "Rdata.rds"))))
  pkg_dbs
  if(!is.null(dbase)){
    if(is.null(dset)){
      tmp_env <- new.env()
      lazyLoad(file.path(data_path, "Rdata"), envir = tmp_env)
      db <- get(dbase, envir = tmp_env)
      tabl <- bind_rows(purrr::map(db, function(x) as.character(length(unique(x$ID)))),
                        purrr::map(db, function(x) as.character(sum(is.na(x)))),
                        purrr::map(db, function(x) as.character(nrow(x))),
                        purrr::map(db, function(x) as.character(ncol(x))),
                        purrr::map(db, function(x) as.character(min(x$Beg))),
                        purrr::map(db, function(x) as.character(max(x$End))),
                        purrr::map(db, function(x) attr(x, which = "source_link")),
                        purrr::map(db, function(x) paste0(capture.output(print(attr(x, which = "source_bib"))), sep = "", collapse = "")))
      tabl1 <- tabl %>% 
        t()
      tabl1
    }
    else{
      tmp_env <- new.env()
      lazyLoad(file.path(data_path, "Rdata"), envir = tmp_env)
      db <- get(dbase, envir = tmp_env)
      ds <- db[[dset]]
      attr(ds, which = "source_link")
      tabl <- data.frame(UniqueID = length(unique(ds$ID)),
                        MissingValues = paste0(sum(is.na(ds))/prod(dim(ds)), " %"),
                        NObs = nrow(ds),
                        NVar = ncol(ds),
                        MinDate = min(ds$Beg),
                        MaxDate = max(ds$End),
                        SourceLink = attr(ds, which = "source_link"),
                        SourceBib = paste0(capture.output(print(attr(ds, which = "source_bib"))), sep = "", collapse = "")
      ) 
      tabl2 <- tabl %>% 
        t()
      colnames(tabl2) <- dset
      tabl2
      colnames(tabl2) <- c("Unique ID", "Missing data", "Rows", "Columns", "Beg", "End", "Link", "Bibliography")
    }
  }
  else{
    tmp_env <- new.env()
    lazyLoad(file.path(data_path, "Rdata"), envir = tmp_env)
    dbs <- get(pkg_dbs, envir = tmp_env)
    tabl <- bind_rows(purrr::map(dbs, function(x) as.character(length(unique(x$ID)))),
                      purrr::map(dbs, function(x) as.character(sum(is.na(x)))),
                      purrr::map(dbs, function(x) as.character(nrow(x))),
                      purrr::map(dbs, function(x) as.character(ncol(x))),
                      purrr::map(dbs, function(x) as.character(min(x$Beg))),
                      purrr::map(dbs, function(x) as.character(max(x$End))),
                      purrr::map(dbs, function(x) attr(x, which = "source_link")),
                      purrr::map(dbs, function(x) paste0(capture.output(print(attr(x, which = "source_bib"))), sep = "", collapse = "")))
    
    tabl3 <- tabl %>% 
      t()
    
    colnames(tabl3) <- c("Unique ID", "Missing data", "Rows", "Columns", "Beg", "End", "Link", "Bibliography")
    tabl3
    
  }
  
  
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
