#' @name report
#' @title Set of data structure exploration functions for users
#' @description The report family of functions allows users
#' to quickly get information about and compare several
#' aspects of a q Packages, databases and datasets.
#' @param pkg character string of the qPackage to report data on.
#' Required input.
#' @param database vector of character strings of the qPackage to
#' report data a specific database in a qPackage.
#' If NULL, report_data returns a summary of all databases in the qPackage.
#' NULL by default.
#' @param dataset character string of the qPackage to report data on a specific
#' dataset in a specific database of a qPackage.
#' If NULL and database is specified, returns database level metadata.
#' NULL by default.
#' @param print should dataframe returned be printed to console?
#' TRUE by default.
NULL

#' @name report
#' @details `data_source()` displays names of the database/datasets and
#' source material of qPackage data.
#' @importFrom purrr map
#' @importFrom stringr str_to_title
#' @return A dataframe with the data sources
#' @examples
#' data_source(pkg = "qStates", database = "states", dataset = "COW")
#' @export
data_source <- function(pkg, database = NULL, dataset = NULL, print = TRUE) {
  pkg_path <- find.package(pkg)
  data_path <- file.path(pkg_path, "data")
  #selcts all dbs
  pkg_dbs <- unname(unlist(readRDS(file.path(data_path, "Rdata.rds"))))
  if (!is.null(database)) {
    # Database specified, dataset unspecified
    if (is.null(dataset)) {
      tmp_env <- new.env()
      lazyLoad(file.path(data_path, "Rdata"), envir = tmp_env)
      dbs <-  mget(ls(tmp_env), tmp_env)
      dbs <- dbs[database]
      outlist <- list()
      for (i in c(1:length(dbs))) {
        assign(paste0("tabl", i), rbind(purrr::map(dbs[[i]], function(x)
          paste0(utils::capture.output(
            print(attr(x, which = "source_bib"))), sep = "", collapse = "")))
          )
        assign(paste0("tabl", i), t(get(paste0("tabl", i))))
        tmp <- get(paste0("tabl", i))
        colnames(tmp) <- "Reference"
        assign(paste0("tabl", i), tmp)
        # Console print
        if (print == TRUE){
          print(paste0("References for the ", stringr::str_to_title(database),
                     " database", sep = ""))
          print(get(paste0("tabl", i)))
        }
        #List output
        outlist[i] <- list(get(paste0("tabl", i)))
      }
      names(outlist) <- names(dbs)
      invisible(outlist)
    } else {
      # Database and dataset specified
      tmp_env <- new.env()
      lazyLoad(file.path(data_path, "Rdata"), envir = tmp_env)
      db <- get(database, envir = tmp_env)
      ds <- db[[dataset]]
      tabl <- data.frame(Reference = paste0(utils::capture.output(
        print(attr(ds, which = "source_bib"))), sep = "", collapse = "")
      )
      tabl2 <- tabl %>%
        t()
      colnames(tabl2) <- dataset
      # Console print
      if (print == TRUE){
        print(paste0("Reference for the ", dataset,
                   " dataset in the ", stringr::str_to_title(database),
                   " database", sep = ""))
      }
      tabl2
    }
  } else {
    tmp_env <- new.env()
    lazyLoad(file.path(data_path, "Rdata"), envir = tmp_env)
    dbs <-  mget(ls(tmp_env), tmp_env)
    outlist <- list()
    for (i in c(1:length(dbs))) {
      assign(paste0("tabl", i), rbind(purrr::map(dbs[[i]], function(x)
        paste0(utils::capture.output(
          print(attr(x, which = "source_bib"))), sep = "", collapse = ""
          ))))
      assign(paste0("tabl", i), t(get(paste0("tabl", i))))
      tmp <- get(paste0("tabl", i))
      colnames(tmp) <- "Reference"
      names(tmp) <- names(dbs[[i]])
      #Clear attr from object for a prettier print to console
      attr(tmp, "names") <- NULL
      assign(paste0("tabl", i), tmp)
      # Console print
      if (print == TRUE){
      print(paste0("References for the ",
                   stringr::str_to_title(ls(tmp_env)[i]),
                   " database", sep = ""))
      print(get(paste0("tabl", i)))
      }
      #Append to list output
      outlist[i] <- list(get(paste0("tabl", i)))
    }
    # Return list
    names(outlist) <- names(dbs)
    invisible(outlist)
  }
}

#' @name report
#' @details `data_contrast()` displays information about databases
#' and datasets contained in them.
#' Namely the number of unique ID's, the percentage of
#' missing data, the number of observations, the number of variables,
#' the minimum beginning date and the maximum ending date as well as
#' the most direct URL to the original dataset.
#' @importFrom purrr map
#' @importFrom stringr str_to_title
#' @return A dataframe with the data report
#' @examples
#' data_contrast(pkg = "qStates", database = "states", dataset = "COW")
#' @export
data_contrast <- function(pkg, database = NULL, dataset = NULL, print = TRUE) {
  pkg_path <- find.package(pkg)
  data_path <- file.path(pkg_path, "data")
  pkg_dbs <- unname(unlist(readRDS(file.path(data_path, "Rdata.rds"))))
  if (!is.null(database)) {
    if (is.null(dataset)) {
      # Database specified but not dataset
      tmp_env <- new.env()
      lazyLoad(file.path(data_path, "Rdata"), envir = tmp_env)
      dbs <-  mget(ls(tmp_env), tmp_env)
      dbs <- dbs[database]
      outlist <- list()
      for (i in c(1:length(dbs))) {
        assign(paste0("tabl", i),
               rbind(purrr::map(dbs[[i]], function(x) length(unique(x$ID))),
                     purrr::map(dbs[[i]], function(x)
                       paste0(
                         round(sum(is.na(x)) * 100 / prod(dim(x)), digits = 2), " %")
                       ),
                     purrr::map(dbs[[i]], function(x) nrow(x)),
                     purrr::map(dbs[[i]], function(x) ncol(x)),
                     purrr::map(dbs[[i]], function(x)
                       as.character(as.Date(ifelse(!all(is.na(x$Beg)),
                                                   min(x$Beg, na.rm = TRUE), NA),
                                                   origin = "1970-01-01"))),
                     purrr::map(dbs[[i]], function(x)
                       as.character(as.Date(ifelse(!all(is.na(x$End)),
                                                   max(x$End, na.rm = TRUE), NA),
                                            origin = "1970-01-01"))),
                     purrr::map(dbs[[i]], function(x)
                       attr(x, which = "source_URL"))))
        assign(paste0("tabl", i), t(get(paste0("tabl", i))))
        tmp <- as.data.frame(get(paste0("tabl", i)))
        colnames(tmp) <- c("Unique ID", "Missing Data", "Rows",
                           "Columns", "Beg", "End", "URL")
        assign(paste0("tabl", i), tmp)
        #Print to console
        if (print == TRUE){
          print(paste0(stringr::str_to_title(database[i]),
                       " database", sep = ""))
          print(get(paste0("tabl", i))) 
        }
        # Append objects to outlist
        outlist[i] <- list(get(paste0("tabl", i)))
      }
      # Name elements in list
      names(outlist) <- database
      # Quiet return
      invisible(outlist)
    } else {
      # Both dataset and database specified
      tmp_env <- new.env()
      lazyLoad(file.path(data_path, "Rdata"), envir = tmp_env)
      db <- get(database, envir = tmp_env)
      ds <- db[[dataset]]
      tabl <- data.frame(UniqueID = length(unique(ds$ID)),
                         Missing_Data = paste0(
                           round(sum(is.na(ds)) * 100 / prod(dim(ds)),
                                 digits = 2), " %"),
                         NObs = nrow(ds),
                         NVar = ncol(ds),
                         MinDate = as.character(as.Date(
                           ifelse(!all(is.na(ds$Beg)),
                                  min(ds$Beg, na.rm = TRUE), NA),
                           origin = "1970-01-01")),
                         MaxDate = as.character(as.Date(
                           ifelse(!all(is.na(ds$End)),
                                  max(ds$End, na.rm = TRUE), NA),
                           origin = "1970-01-01")),
                         URL = attr(ds, which = "source_URL"))
      tabl2 <- as.data.frame(t(tabl))
      colnames(tabl2) <- dataset
      # Print to console
      if (print == TRUE){
        print(paste0(dataset, " dataset from the ",
                     stringr::str_to_title(database), " database", sep = ""))
        print(tabl2)
      }
      tabl2
    }
  } else {
    # Only package specified, returns package level info
    tmp_env <- new.env()
    lazyLoad(file.path(data_path, "Rdata"), envir = tmp_env)
    dbs <-  mget(ls(tmp_env), tmp_env)
    outlist <- list()
    for (i in c(1:length(dbs))) {
      assign(paste0("tabl", i),
             rbind(purrr::map(dbs[[i]], function(x) length(unique(x$ID))),
                   purrr::map(dbs[[i]], function(x)
                     paste0(round(sum(is.na(x)) * 100 / prod(dim(x)), digits = 2), " %")),
                   purrr::map(dbs[[i]], function(x) nrow(x)),
                   purrr::map(dbs[[i]], function(x) ncol(x)),
                   purrr::map(dbs[[i]], function(x)
                     as.character(as.Date(ifelse(!all(is.na(x$Beg)),
                                                 min(x$Beg, na.rm = TRUE), NA),
                                          origin = "1970-01-01"))),
                   purrr::map(dbs[[i]], function(x)
                     as.character(as.Date(ifelse(!all(is.na(x$End)),
                                                 max(x$End, na.rm = TRUE), NA),
                                          origin = "1970-01-01"))),
                   purrr::map(dbs[[i]], function(x)
                     attr(x, which = "source_URL"))))
      assign(paste0("tabl", i), t(get(paste0("tabl", i))))
      tmp <- as.data.frame(get(paste0("tabl", i)))
      colnames(tmp) <- c("Unique ID", "Missing Data", "Rows",
                         "Columns", "Beg", "End", "URL")
      assign(paste0("tabl", i), tmp)
      # Print to console
      if (print == TRUE){
        print(paste0(stringr::str_to_title(ls(tmp_env)[i]),
                     " database", sep = ""))
        print(get(paste0("tabl", i))) 
      }
      #Append to outlist
      outlist[i] <- list(get(paste0("tabl", i)))
    }
    # Name elements in list
    names(outlist) <- pkg_dbs
    #Quiet return
    invisible(outlist)
  }
 }

#'  This third function will display information about the process that the data
#' # went through to be refined and included in our qPackage.
#'
#' #' Reports on qPackage data
#' #'
#' #' Allows users to see the changes to the original coding that was performed
#' #' by the preparation script.
#' #' @return A dataframe with the data report
#' #' @examples
#' #' data_evolution(pkg = "qStates", database = "states", dataset = "COW")
#' #' @export
#' data_evolution <- function(pkg, database, dataset) {
#'   pkg_path <- find.package(pkg)
#'   data_path <- file.path(pkg_path, "data")
#'   #selcts all dbs
#'   pkg_dbs <- unname(unlist(readRDS(file.path(data_path, "Rdata.rds"))))
#'
#' }
