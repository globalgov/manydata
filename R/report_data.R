#' @name report
#' @title Set of data structure exploration functions for users
#' @description The report family of functions allows users
#' to quickly get information about and compare several
#' aspects of  a package in the many packages universe,
#' and its' databases and datasets.
#' @param pkg character string of the many package to report data on.
#' Required input.
#' @param database vector of character strings of the many package to
#' report data a specific database in a many package
#' If NULL, the function returns a summary of all databases in the many package
#' NULL by default for `data_source()` and `data_contrast()`.
#' @param dataset character string of the many package to report data on a specific
#' dataset in a specific database of a many package
#' If NULL and database is specified, returns database level metadata.
#' NULL by default for `data_source()` and `data_contrast()`.
NULL

#' @name report
#' @details `data_source()` displays names of the database/datasets and
#' source material of data in a many package.
#' @importFrom purrr map
#' @importFrom stringr str_to_title
#' @return A dataframe with the data sources
#' @examples
#' data_source(pkg = "manydata")
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
      for (i in c(seq_len(length(dbs)))) {
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
      colnames(tmp) <- "Reference"
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
    for (i in c(seq_len(length(dbs)))) {
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
#' @return A list with the desired metadata to compare various datasets in a many package.
#' @examples
#' data_contrast(pkg = "manydata")
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
      for (i in c(seq_len(length(dbs)))) {
        assign(paste0("tabl", i),
               rbind(purrr::map(dbs[[i]], function(x) length(unique(x$ID))),
                     purrr::map(dbs[[i]], function(x)
                       paste0(
                         round(sum(is.na(x)) * 100 / prod(dim(x)),
                               digits = 2), " %")
                       ),
                     purrr::map(dbs[[i]], function(x) nrow(x)),
                     purrr::map(dbs[[i]], function(x) ncol(x)),
                     purrr::map(dbs[[i]], function(x)
                       as.character(ifelse(!all(is.na(x$Beg)),
                                                   min(x$Beg,
                                                       na.rm = TRUE),
                                                   NA))),
                     purrr::map(dbs[[i]], function(x)
                       as.character(ifelse(!all(is.na(x$End)),
                                                   max(x$End,
                                                       na.rm = TRUE),
                                                   NA))),
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
                         MinDate = as.character(
                           ifelse(!all(is.na(ds$Beg)),
                                  min(ds$Beg, na.rm = TRUE), NA)),
                         MaxDate = as.character(
                           ifelse(!all(is.na(ds$End)),
                                  max(ds$End, na.rm = TRUE), NA)),
                         URL = attr(ds, which = "source_URL"))
      tmp <- as.data.frame(tabl)
      colnames(tmp) <- c("Unique ID", "Missing Data", "Rows",
                         "Columns", "Beg", "End", "URL")
      outlist <- list(tmp)
      names(outlist) <- dataset
      # Redefine outlist class to list
      class(outlist) <- "listof"
      return(outlist)
    }
  } else {
    # Only package specified, returns package level info
    tmp_env <- new.env()
    lazyLoad(file.path(data_path, "Rdata"), envir = tmp_env)
    dbs <-  mget(ls(tmp_env), tmp_env)
    outlist <- list()
    for (i in 1:length(dbs)) {
      assign(paste0("tabl", i),
             rbind(purrr::map(dbs[[i]], function(x) length(unique(x$ID))),
                   purrr::map(dbs[[i]], function(x)
                     paste0(round(sum(is.na(x)) * 100 / prod(dim(x)),
                                  digits = 2), " %")),
                   purrr::map(dbs[[i]], function(x) nrow(x)),
                   purrr::map(dbs[[i]], function(x) ncol(x)),
                   purrr::map(dbs[[i]], function(x)
                     as.character(ifelse(!all(is.na(x$Beg)),
                                                 min(x$Beg,
                                                     na.rm = TRUE),
                                                 NA))),
                   purrr::map(dbs[[i]], function(x)
                     as.character(ifelse(!all(is.na(x$End)),
                                                 max(x$End,
                                                     na.rm = TRUE),
                                                 NA))),
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

#' @name report
#' @details `open_codebook()` opens the original codebook of the specified
#' dataset to allow users to look up the original coding rules.
#' Note that there is no original codebook for the genevar database
#' and that a codebook might not exist for certain datasets.
#' In the latter case, please refer to the
#' source URL provided with each dataset by running `manydata::data_contrast()`
#' as further information on coding rules available online.
#' @return Opens a pdf version of the original codebook of the specified
#' dataset, if available.
#' @examples
#' \dontrun{
#' open_codebook(pkg = "qStates", database = "states", dataset = "COW")
#' }
#' @export
open_codebook <- function(pkg, database, dataset) {
  # Check if input is null
  if (is.null(pkg) | is.null(database) | is.null(dataset)) {
    stop("Please specify a pkg, a database and a dataset for which you would
         like to open the original codebook.")
  }
  # Check if package exists
  repo <- paste0("https://api.github.com/users/globalgov/repos")
  repo <- httr::GET(repo, query = list(state = "all",
                                       per_page = 100, page = 1))
  repo <- suppressMessages(httr::content(repo, type = "text"))
  repo <- jsonlite::fromJSON(repo, flatten = TRUE)
  reponames <- repo[["name"]]
  if (!(pkg %in% reponames)) {
    stop("Please enter a valid ")
  }
  # Find the PDF on GitHub
  url <- paste0("https://github.com/globalgov/",
                 pkg,
                 "/raw/develop/data-raw/",
                 database,
                 "/",
                 dataset,
                 "/",
                 dataset,
                 "OriginalCodebook.pdf")
  # Open the PDF
  utils::browseURL(url, browser = getOption("browser"),
            encodeIfNeeded = FALSE)
}
