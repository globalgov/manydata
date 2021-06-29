#' @name report
#' @title Set of data structure exploration functions for users
#' @description The report family of functions allows users
#' to quickly get information about and compare several
#' aspects of a q Packages, databases and datasets.
#' @param pkg character string of the qPackage to report data on.
#' Required input.
#' @param database vector of character strings of the qPackage to
#' report data a specific database in a qPackage.
#' If NULL, the function returns a summary of all databases in the qPackage.
#' NULL by default for `data_source()` and `data_contrast()`. Required for
#' `data_evolution()`.
#' @param dataset character string of the qPackage to report data on a specific
#' dataset in a specific database of a qPackage.
#' If NULL and database is specified, returns database level metadata.
#' NULL by default for `data_source()` and `data_contrast()`. Required for
#' `data_evolution()`.
NULL

#' @name report
#' @details `data_source()` displays names of the database/datasets and
#' source material of qPackage data.
#' @importFrom purrr map
#' @importFrom stringr str_to_title
#' @return A dataframe with the data sources
#' @examples
#' \dontrun{
#' get_packages("qStates")
#' data_source(pkg = "qStates", database = "states", dataset = "COW")
#' }
#' @export
data_source <- function(pkg, database = NULL, dataset = NULL) {
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
        #List output
        outlist[i] <- list(get(paste0("tabl", i)))
      }
      names(outlist) <- names(dbs)
      # Redefine outlist class to list
      class(outlist) <- "listof"
      return(outlist)
    } else {
      # Database and dataset specified
      tmp_env <- new.env()
      lazyLoad(file.path(data_path, "Rdata"), envir = tmp_env)
      db <- get(database, envir = tmp_env)
      ds <- db[[dataset]]
      tabl <- data.frame(Reference = paste0(utils::capture.output(
        print(attr(ds, which = "source_bib"))), sep = "", collapse = "")
      )
      tmp <- as.data.frame(tabl)
      colnames(tmp) <- c("Reference")
      outlist <- list(tmp)
      names(outlist) <- dataset
      # Redefine outlist class to list
      class(outlist) <- "listof"
      return(outlist)
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
      #Append to list output
      outlist[i] <- list(get(paste0("tabl", i)))
    }
    # Redefine outlist class to list
    class(outlist) <- "listof"
    return(outlist)
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
#' @return A list with the desired metadata to compare various datasets in the
#' qVerse.
#' @examples
#' \dontrun{
#' get_packages("qStates")
#' data_contrast(pkg = "qStates", database = "states")
#' }
#' @export
data_contrast <- function(pkg, database = NULL, dataset = NULL) {
  
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
        # Append objects to outlist
        outlist[i] <- list(get(paste0("tabl", i)))
      }
      # Name elements in list
      names(outlist) <- database
      # Redefine outlist class to list
      class(outlist) <- "listof"
      return(outlist)
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
      tmp <- as.data.frame(tabl)
      colnames(tmp) <- c("Unique ID", "Missing Data", "Rows",
                         "Columns", "Beg", "End", "URL")
      outlist <- list(tmp)
      names(outlist) <- dataset
      # Redefine outlist class to list
      class (outlist) <- "listof"
      return(outlist)
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
      #Append to outlist
      outlist[i] <- list(get(paste0("tabl", i)))
    }
    # Name elements in list
    names(outlist) <- pkg_dbs
    # Redefine outlist class to list
    class(outlist) <- "listof"
    return(outlist)
  }
}

pkg <- "qStates"
database <- "states"
dataset <- "COW"

#' @name report
#' @details `data_evolution()` Allows users to see the changes to the original
#' coding that was performed by the preparation script.
#' Requires an individual dataset to be specified.
#' @return A list of elements highlighting metrics tracking the changes between
#' the original object and the processed object.
#' @examples
#' \dontrun{
#' get_packages("qStates")
#' data_evolution(pkg = "qStates", database = "states", dataset = "COW")
#' }
#' @export
data_evolution <- function(pkg, database, dataset) {
  # Loading things in
  pkg_path <- find.package(pkg)
  data_path <- file.path(pkg_path, "data")
  pkg_dbs <- unname(unlist(readRDS(file.path(data_path, "Rdata.rds"))))
  tmp_env <- new.env()
  lazyLoad(file.path(data_path, "Rdata"), envir = tmp_env)
  db <- get(database, envir = tmp_env)
  ds <- db[[dataset]]
  procnames <- colnames(ds)
  # Getting the original metadata
  orig <- attr(ds, which = "metadata_orig" , exact = TRUE)
  names(orig) <- c("ColNames", "Rows", "Columns", "Missing Data")
  # Getting the processed metadata
  proc <- as.list(qData::data_contrast(pkg = pkg,
                               database = database,
                               dataset = dataset)[[1]])
  proc[["ColNames"]] <- procnames
  proc <- list(proc[["ColNames"]], proc[["Rows"]],
               proc[["Columns"]], proc[["Missing Data"]])
  names(proc) <- c("ColNames", "Rows", "Columns", "Missing Data")
  # Building the output
  outlist <- list("Original" = orig, "Processed" = proc)
  class(outlist) <- "listof"
  # ToDo: Pretty print method.
  # Not a great solution, but works and does not print if assigned.
  # Some adjustments might be warranted.
  outlist <- purrr::map(outlist, as.character)
  outlist <- as.data.frame(outlist)
  colnames(outlist) <- c(paste("Original", dataset), paste("Corrected", dataset))
  row.names(outlist) <- c("ColNames", "Rows", "Columns", "Missing Data")
  outlist <- t(outlist)
  outlist <- data.frame(outlist)
  outlist$ColNames <- paste0(outlist$ColNames)
  outlist
}
