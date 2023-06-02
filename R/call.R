#' Call 'many' packages, sources, and data
#' 
#' @description The `call_` functions in `{manydata}` allows users to call,
#' install, and update different 'many' packages, as well as additional
#' information on databases and datasets across 'many packages'.
#' @name call
#' @param pkg A character vector of package name.
#' For multiple packages,
#' please declare package names as a vector (e.g. c("pkg1", "pkg2)).
#' @param database A database from one of the many packages.
#' @param dataset A dataset in a database from one of the many packages.
#' NULL by default.
#' That is, all datasets in the database are used.
#' For multiple datasets, please declare datasets as a vector
#' (e.g. c("dataset1", "dataset2)).
#' @param treaty_type The type of treaties to be returned.
#' NULL, by default.
#' Other options are "bilateral" or "multilateral".
#' @param develop Would you like to download the develop
#' version of the package?
#' FALSE by default.
#' If TRUE, the function downloads the develop version of package from GitHub.
#' @param variable Would you like to get one, or more, specific variables
#' present in one or more datasets in the 'many' database?
#' NULL by default.
#' For multiple variables, please declare variable names as a vector.
#' @param actor An actor variable in dataset.
#' NULL by default.
#' If declared, a tibble of the traties and their member actors is returned.
#' @importFrom dplyr as_tibble %>%
#' @examples
#' \donttest{
#' #call_packages()
#' #call_packages("manyenviron")
#' call_sources(emperors)
#' membs <- tibble::tibble( manyID = c("ROU-RUS[RFP]_1901A",
#' "ROU-RUS[RFP]_1901A", "GD16FI_1901A"),
#' StateID = c("ROU", "RUS", "DNK"),
#' Title = c("Convention Between Roumania And Russia Concerning Fishing
#' In The Danube And The Pruth",
#' "Convention Between Roumania And Russia Concerning Fishing
#' In The Danube And The Pruth",
#' "Convention Between The Governments Of Denmark And
#' The United Kingdom Of Great Britain
#' And Northern Ireland For Regulating The Fisheries
#' Of Their Respective Subjects Outside
#' Territorial Waters In The Ocean Surrounding The Faroe Islands"),
#' Beg = c("1901-02-22", "1901-02-22", "1901-06-24"))
#' call_treaties(membs)
#' call_treaties(membs, treaty_type = "bilaterals",
#' variable = c("Title", "Beg"))
#' call_treaties(membs, variable = c("Title", "Beg"), actor = "StateID")
#' }
#' @return
#' The `call_` functions return tibbles with the respective information.
NULL

#' @describeIn call Call, download, and update 'many' packages
#' @details `call_packages()` finds and download other packages
#' that belong to the many universe of packages.
#' It allows users to rapidly access the names and other
#' descriptive information of these packages.
#' If users intend to download and install a package listed,
#' they can type the package name within the function.
#' @importFrom dplyr bind_rows rename relocate %>%
#' @importFrom stringr str_detect str_remove
#' @importFrom jsonlite fromJSON
#' @importFrom httr GET content
#' @importFrom remotes install_github
#' @importFrom utils packageVersion askYesNo
#' @return `call_packages()` returns a tibble with the 'many packages'
#' currently available.
#' If one or more package names are provided,
#' these will be installed from Github.
#' @export
call_packages <- function(pkg, develop = FALSE) {
  # introduce variables to avoid check notes
  Description <- Installed <- Latest <- Name <- updated <-
    description <- full_name <- name <- NULL
  # get info from GitHub if pkg is missing
  if (missing(pkg)) {
    # get package releases, versions, and bind information
    repo <- httr::GET("https://api.github.com/users/globalgov/repos",
                      query = list(state = "all", per_page = 100, page = 1))
    repo <- suppressMessages(httr::content(repo, type = "text"))
    repo <- jsonlite::fromJSON(repo, flatten = TRUE)
    repo <- repo[c("name", "full_name", "description")]
    if (length(repo) < 3 | nrow(repo) < 5) {
      stop(
        "The download limit from GitHub has been reached.
      To see all the available 'many packages' packages,
      please go to the following link: https://github.com/globalgov")
    }
    # search for locally installed packages
    repo$Installed <- sapply(repo$name, function(x) {
      tryCatch({
        as.character(utils::packageVersion(x))
        }, error = function(e) {
          NA_character_
          })
      })
    # get latest release
    repo$Latest <- get_latest_release(repo$full_name)
    repo <- subset(repo, !grepl("Unreleased", repo$Latest))
    repo <- repo %>%
      dplyr::bind_rows() %>%
      dplyr::select(-full_name) %>%
      dplyr::rename(Name = name, Description = description) %>%
      dplyr::relocate(Name, Description, Installed, Latest) %>%
      dplyr::as_tibble()
    # prints tibble before asking about updates
    print(repo, justify = "right")
    # ask users if they want to install/update packages
    if (any(repo$Installed != repo$Latest)) {
      not_up_to_date <- repo[repo$Installed != repo$Latest,]
      install <- not_up_to_date[is.na(not_up_to_date$Installed), "Name"]
      update <- not_up_to_date[!is.na(not_up_to_date$Installed), "Name"]
      if (nrow(install) > 0) {
        if(utils::askYesNo(msg = paste0("Would you like to install ",
                                        paste(install$Name, collapse = ", "),
                                        " ?"))) {
          tryCatch({
            for (i in install$Name) {
              remotes::install_github(paste0("globalgov/", i))
            }
          }, error = function(e) {
            stop(paste0("The download limit from GitHub has been reached.
       Please download the package using:
              remotes::install_github(globalgov/", pkg, ")"))
          })
        }
      } 
      if (nrow(update) > 0) {
        if(utils::askYesNo(msg = paste0("Would you like to update ",
                                        paste(update$Name, collapse = ", "),
                                        " ?"))) {
          tryCatch({
            for (i in update$Name) {
              remotes::install_github(paste0("globalgov/", i))
            }
          }, error = function(e) {
            stop(paste0("The download limit from GitHub has been reached.
       Please download the package using:
              remotes::install_github(globalgov/", pkg, ")"))
          })
        }
      }
    }
  } else {
    # download package if declared
    tryCatch({
      if (stringr::str_detect(pkg, "/")) {
        remotes::install_github(pkg)
        pkg <- strsplit(pkg, "/")[[1]][2]
      }
      if (develop == FALSE) {
        remotes::install_github(paste0("globalgov/", pkg))
      } else {
        remotes::install_github(paste0("globalgov/", pkg), ref = "develop")
      }
      library(pkg, character.only = TRUE)
    }, error = function(e) {
      stop(paste0("The download limit from GitHub has been reached.
       Please download the package using:
              remotes::install_github(globalgov/", pkg, ")"))
    })
  }
}

# Helper function to get package releases
get_latest_release <- function(full_name) {
  latest <- paste0("https://api.github.com/repos/",
                   full_name, "/releases/latest")
  if (length(latest) == 1) {
    latest <- httr::GET(latest)
    latest <- suppressMessages(httr::content(latest, type = "text"))
    latest <- jsonlite::fromJSON(latest, flatten = TRUE)$tag_name
  } else {
    latest <- sapply(latest, function(x) {
      x <- httr::GET(x)
      x <- suppressMessages(httr::content(x, type = "text"))
      x <- jsonlite::fromJSON(x, flatten = TRUE)$tag_name
      if (is.null(x)) {
        x <- "Unreleased"
        x
      } else {
        x <- stringr::str_remove(x, "v")
        x
      }
    })
  }
  unlist(latest)
}

#' @describeIn call Call sources for databases and datasets in 'many' packages
#' @details `call_sources()` displays sources of the databases and datasets
#' in 'many' packages.
#' @importFrom purrr map
#' @importFrom stringr str_to_title
#' @export
call_sources <- function(database, dataset = NULL) {
  #selcts all dbs
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

#' @describeIn call Call treaties from 'many' datasets
#' @details Certain datasets, or consolidated databases, in 'many' packages
#' contains information on treaties which can be retrieved
#' with `call_treaties()`.
#' 
#' @export
call_treaties <- function(dataset, treaty_type = NULL, key = "manyID",
                          variable = NULL, actor = NULL) {
  Key <- Memberships <- NULL
  # check if key is valid
  if (!any(colnames(dataset) == key)) {
    stop("Please declare a valid key variable.")
  }
  # get variables, if declared
  if (!is.null(variable)) {
    out <- dataset[,c(key, variable)] %>% dplyr::distinct()
  } else {
    out <- dataset[,key] %>% dplyr::distinct()
  }
  names(out)[names(out) == key] <- "Key"
  # subset treaty types
  if (!is.null(treaty_type)) {
    if (treaty_type == "bilateral") {
      out <- subset(out, stringr::str_detect(Key, "\\-"))
    }
    if (treaty_type == "multilateral") {
      out <- subset(out, stringr::str_detect(Key, "\\-", negate = TRUE))
    }
  }
  if (!is.null(actor)) {
    actors <- dataset[,c(key, actor)] %>% dplyr::distinct()
    names(actors)[names(actors) == key] <- "Key"
    names(actors)[names(actors) == actor] <- "Memberships"
    out <- actors %>%
      dplyr::group_by(Key) %>%
      dplyr::summarise(Memberships = toString(Memberships)) %>%
      dplyr::ungroup() %>%
      dplyr::right_join(out, by = "Key") %>%
      distinct()
  }
  out
}

#' #' @describeIn call Call 'many' texts
#' #' @return A tibble of treaties and their texts.
#' #' @export
#' call_texts <- function(dataset, treaty_type = NULL, key = "manyID") {
#'   # check if key is valid
#'   if (!any(colnames(dataset) == key)) {
#'     stop("Please declare a valid key variable.")
#'   }
#'   # get variables, if declared
#'   out <- dataset[, c(key, grep("text", names(dataset),
#'                                ignore.case = TRUE, value = TRUE))]
#'   names(out)[names(out) == key] <- "Key"
#'   # subset treaty types
#'   if (!is.null(treaty_type)) {
#'     if (treaty_type == "bilateral") {
#'       out <- subset(out, stringr::str_detect(Key, "\\-"))
#'     }
#'     if (treaty_type == "multilateral") {
#'       out <- subset(out, stringr::str_detect(Key, "\\-", negate = TRUE))
#'     }
#'   }
#'   out %>%
#'     dplyr::filter(!dplyr::if_all(-Key, is.na)) %>%
#'     dplyr::distinct()
#' }
