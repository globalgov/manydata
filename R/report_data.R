# Set of data structure exploration functions for users.

#' Displays names of the database/datasets and source material of qPackage data
#' 
#' To quickly report the names of the data structures in a package as well as their source.
#' @param pkg character string of the qPackage to report data on. Mandatory input.
#' @param database vector of character strings of the qPackage to report data a 
#' specific database in a qPackage. If Null, report_data returns a summary 
#' of all databases in the qPackage. Null by default.
#' @param dataset character string of the qPackage to report data on a specific dataset
#' in a specific database of a qPackage. If Null and database is specified, returns database
#' level metadata. Null by default.
#' @return A dataframe with the data sources.
#' @examples
#' data_source(pkg = "qStates", database = "states", dataset = "COW")
#' @export
data_source <- function(pkg, database = NULL, dataset = NULL){
  pkg_path <- find.package(pkg)
  data_path <- file.path(pkg_path, "data")
  #selcts all dbs
  pkg_dbs <- unname(unlist(readRDS(file.path(data_path, "Rdata.rds"))))
  if(!is.null(database)){
    if(is.null(dataset)){
      #data_source("pkg", "database")
      tmp_env <- new.env()
      lazyLoad(file.path(data_path, "Rdata"), envir = tmp_env)
      dbs <-  mget(ls(tmp_env), tmp_env)
      dbs <- dbs[database]
      for (i in c(1:length(dbs))) {
        assign(paste0("tabl", i), rbind(purrr::map(dbs[[i]], function(x) paste0(utils::capture.output(print(attr(x, which = "source_bib"))), sep = "", collapse = ""))))
        assign(paste0("tabl", i), t(get(paste0("tabl", i))))
        tmp <- get(paste0("tabl", i))
        colnames(tmp) <- c("Reference")
        assign(paste0("tabl", i), tmp)
        print(get(paste0("tabl", i)))
      }
    } else {
      #data_source("pkg", "database", "dataset")
      tmp_env <- new.env()
      lazyLoad(file.path(data_path, "Rdata"), envir = tmp_env)
      db <- get(database, envir = tmp_env)
      ds <- db[[dataset]]
      tabl <- data.frame(Reference = paste0(utils::capture.output(print(attr(ds, which = "source_bib"))), sep = "", collapse = "")
      )
      tabl2 <- tabl %>%
        t()
      colnames(tabl2) <- dataset
      tabl2
    }
  } else {
    #data_source("pkg")
    tmp_env <- new.env()
    lazyLoad(file.path(data_path, "Rdata"), envir = tmp_env)
    dbs <-  mget(ls(tmp_env), tmp_env)
    for (i in c(1:length(dbs))) {
      assign(paste0("tabl", i), rbind(purrr::map(dbs[[i]], function(x) paste0(utils::capture.output(print(attr(x, which = "source_bib"))), sep = "", collapse = ""))))
      assign(paste0("tabl", i), t(get(paste0("tabl", i))))
      tmp <- get(paste0("tabl", i))
      colnames(tmp) <- c("Reference")
      assign(paste0("tabl", i), tmp)
      print(get(paste0("tabl", i)))
    }
  }
}


# This second function will contain information about missing values, number of observations, number of variables, etc.
# It will allow users to compare processed datasets. E.g. which dataset in database A has more observations?

#' Contrasts data in qPackages
#' 
#' Allows users to quickly compare processed datasets.
#' @param pkg character string of the qPackage to report data on. Mandatory input.
#' @param database vector of character strings of the qPackage to report data a 
#' specific database in a qPackage. If Null, report_data returns a summary 
#' of all databases in the qPackage. Null by default.
#' @param dataset character string of the qPackage to report data on a specific dataset
#' in a specific database of a qPackage. If Null and database is specified, returns database
#' level metadata. Null by default.
#' @return A dataframe with the data report
#' @examples
#' data_contrast(pkg = "qStates", database = "states", dataset = "COW")
#' @export
data_contrast <- function(pkg, database = NULL, dataset = NULL){
  pkg_path <- find.package(pkg)
  data_path <- file.path(pkg_path, "data")
  #selcts all dbs
  pkg_dbs <- unname(unlist(readRDS(file.path(data_path, "Rdata.rds"))))
  if(!is.null(database)){
    if(is.null(dataset)){
      #contrast_data("pkg", "database")
      tmp_env <- new.env()
      lazyLoad(file.path(data_path, "Rdata"), envir = tmp_env)
      dbs <-  mget(ls(tmp_env), tmp_env)
      dbs <- dbs[database]
      for (i in c(1:length(dbs))) {
        assign(paste0("tabl", i), 
               rbind(purrr::map(dbs[[i]], function(x) length(unique(x$ID))),
                     purrr::map(dbs[[i]], function(x) paste0(round(sum(is.na(x))/prod(dim(x)), digits = 2), " %")),
                     purrr::map(dbs[[i]], function(x) nrow(x)),
                     purrr::map(dbs[[i]], function(x) ncol(x)),
                     purrr::map(dbs[[i]], function(x) as.character(as.Date(ifelse(!all(is.na(x$Beg)), min(x$Beg, na.rm=T), NA), origin='1970-01-01'))),
                     purrr::map(dbs[[i]], function(x) as.character(as.Date(ifelse(!all(is.na(x$End)), max(x$End, na.rm=T), NA), origin='1970-01-01'))),
                     purrr::map(dbs[[i]], function(x) attr(x, which = "source_link"))))
        assign(paste0("tabl", i), t(get(paste0("tabl", i))))
        tmp <- get(paste0("tabl", i))
        colnames(tmp) <- c("Unique ID", "Missing Data", "Rows", "Columns", "Beg", "End", "URL")
        assign(paste0("tabl", i), tmp)
        print(get(paste0("tabl", i)))
      }
    } else {
      #data_contrast("pkg", "database", "dataset")
      tmp_env <- new.env()
      lazyLoad(file.path(data_path, "Rdata"), envir = tmp_env)
      db <- get(database, envir = tmp_env)
      ds <- db[[dataset]]
      tabl <- data.frame(UniqueID = length(unique(ds$ID)),
                         Missing_Data = paste0(round(sum(is.na(ds))/prod(dim(ds)), digits = 2), " %"),
                         NObs = nrow(ds),
                         NVar = ncol(ds),
                         MinDate = as.character(as.Date(ifelse(!all(is.na(ds$Beg)), min(ds$Beg, na.rm=T), NA), origin = '1970-01-01')),
                         MaxDate = as.character(as.Date(ifelse(!all(is.na(ds$End)), max(ds$End, na.rm=T), NA)), origin = '1970-01-01'))
      tabl2 <- tabl %>%
        t()
      colnames(tabl2) <- dataset
      tabl2
    }
  } else {
    #data_contrast("pkg")
    tmp_env <- new.env()
    lazyLoad(file.path(data_path, "Rdata"), envir = tmp_env)
    dbs <-  mget(ls(tmp_env), tmp_env)
    for (i in c(1:length(dbs))) {
      assign(paste0("tabl", i),
             rbind(purrr::map(dbs[[i]], function(x) length(unique(x$ID))),
                   purrr::map(dbs, function(x) paste0(sum(is.na(x))/prod(dim(x)), " %")),
                   purrr::map(dbs[[i]], function(x) nrow(x)),
                   purrr::map(dbs[[i]], function(x) ncol(x)),
                   purrr::map(dbs[[i]], function(x) as.character(as.Date(ifelse(!all(is.na(x$Beg)), min(x$Beg, na.rm=T), NA), origin='1970-01-01'))),
                   purrr::map(dbs[[i]], function(x) as.character(as.Date(ifelse(!all(is.na(x$End)), max(x$End, na.rm=T), NA), origin='1970-01-01'))),
                   purrr::map(dbs[[i]], function(x) attr(x, which = "source_link"))))
      assign(paste0("tabl", i), t(get(paste0("tabl", i))))
      tmp <- get(paste0("tabl", i))
      colnames(tmp) <- c("Unique ID", "Missing Data", "Rows", "Columns", "Beg", "End", "URL")
      assign(paste0("tabl", i), tmp)
      print(get(paste0("tabl", i)))
    }
  }
 }

#' # This third function will display information about the process that the data 
#' # went through to be refined and included in our qPackage.
#' 
#' #' Reports on qPackage data
#' #' 
#' #' Allows users to see the changes to the original coding that was performed by
#' #' the preparation script.
#' #' @param pkg character string of the qPackage to report data on. Mandatory input.
#' #' @param database vector of character strings of the qPackage to report data a 
#' #' specific database in a qPackage. If Null, report_data returns a summary 
#' #' of all databases in the qPackage. Null by default.
#' #' @param dataset character string of the qPackage to report data on a specific dataset
#' #' in a specific database of a qPackage. 
#' #' @return A dataframe with the data report
#' @examples
#' #' evolution_data(pkg = "qStates", database = "states", dataset = "COW")
#' #' @export
#' evolution_data <- function(pkg, database, dataset){
#'   pkg_path <- find.package(pkg)
#'   data_path <- file.path(pkg_path, "data")
#'   #selcts all dbs
#'   pkg_dbs <- unname(unlist(readRDS(file.path(data_path, "Rdata.rds"))))
#'   
#' }
