#' Call 'many' packages, sources, and data
#' 
#' @description The `call_` functions in `{manydata}` allows users to call,
#' install, and update different 'many' packages, as well as additional
#' information on databases and datasets across 'many packages'.
#' @name call_ 
#' @param package A character vector of package name.
#' For multiple packages,
#' please declare package names as a vector (e.g. c("package1", "package2")).
#' @param database A database from one of the many packages.
#' @param dataset A dataset in a database from one of the many packages.
#' NULL by default.
#' That is, all datasets in the database are used.
#' For multiple datasets, please declare datasets as a vector
#' (e.g. c("dataset1", "dataset2")).
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
#' If declared, a tibble of the treaties and their member actors is returned.
#' @param open_script Would you like to open the preparation script
#' for the dataset?
#' By default false.
#' @param open_codebook Would you like to open the preparation script
#' for the dataset? By default false.
#' @importFrom dplyr as_tibble %>%
#' @examples
#' \donttest{
#' #call_packages()
#' #call_packages("manyenviron")
#' call_sources("manydata", "emperors")
#' membs <- dplyr::tibble(manyID = c("ROU-RUS[RFP]_1901A",
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
#' Begin = c("1901-02-22", "1901-02-22", "1901-06-24"))
#' call_treaties(membs)
#' call_treaties(membs, treaty_type = "bilaterals",
#' variable = c("Title", "Begin"))
#' call_treaties(membs, variable = c("Title", "Begin"), actor = "StateID")
#' }
#' @return
#' The `call_` functions return tibbles with the respective information.
NULL

#' @describeIn call_ Call, download, and update 'many' packages
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
call_packages <- function(package, develop = FALSE) {
  # introduce variables to avoid check notes
  Description <- Installed <- Latest <- Name <- updated <-
    description <- full_name <- name <- NULL
  # get info from GitHub if package is missing
  if (missing(package)) {
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
              remotes::install_github(globalgov/", package, ")"))
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
              remotes::install_github(globalgov/", package, ")"))
          })
        }
      }
    }
  } else {
    # download package if declared
    tryCatch({
      if (stringr::str_detect(package, "/")) {
        remotes::install_github(package)
        package <- strsplit(package, "/")[[1]][2]
      }
      if (develop == FALSE) {
        remotes::install_github(paste0("globalgov/", package))
      } else {
        remotes::install_github(paste0("globalgov/", package), ref = "develop")
      }
      library(package, character.only = TRUE)
    }, error = function(e) {
      stop(paste0("The download limit from GitHub has been reached.
       Please download the package using:
              remotes::install_github(globalgov/", package, ")"))
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

#' @describeIn call_ Call sources for databases and datasets in 'many' packages
#' @details `call_sources()` displays sources of the databases and datasets
#' in 'many' packages.
#' Please declare package, database, and dataset
#' @importFrom utils help browseURL
#' @importFrom dplyr relocate
#' @importFrom stringr str_extract_all str_remove_all str_trim
#' @export
call_sources <- function(package, database, dataset = NULL,
                         open_script = FALSE, open_codebook = FALSE) {
  Dataset <- Source <- URL <- Mapping <- NULL
  # get path
  helptext <- utils::help(topic = as.character(database),
                          package = as.character(package))
  # get help file as text
  helptext <- as.character(get_help_file(helptext))
  # clean text
  helptext <- stringr::str_remove_all(helptext,
                                      "\\\n|\\{|\\}|\\\\tab$|\\\\cr$|^cc$")
  helptext <- paste(stringr::str_trim(helptext[nzchar(helptext)]),
                    collapse = " ")
  # keep only portions we are interested in
  helptext <- paste0(sub('.*</div>', '', helptext), " \\item")
  # get names and sections
  names <- unique(unlist(stringr::str_extract_all(helptext, "\\w*:")))
  names <- names[!grepl("https:", names)]
  sections <- c(unlist(stringr::str_extract_all(helptext,
                                                "section \\w*")), "Source")
  sections <- stringr::str_trim(gsub("section", "", sections))
  # organize information into lists of list
  out <- list()
  for (i in names) {
    out[i] <- stringr::str_extract_all(helptext,
                                       paste0(i, "\\s*(.*?)\\s*\\\\item"))
  }
  # if one or more datasets are declared
  if(!is.null(dataset)) {
    out <- out[grepl(dataset, names(out))]
  }
  # bind list
  out <- data.frame(do.call(rbind, out))
  # clean observations
  out <- data.frame(t(apply(out, 1, function(x) {
    stringr::str_squish(gsub(paste0(paste(names, collapse = "|"),
                                    "|\\\\item|\\\\tabular|\\\\url|\\\\emph|\\\\section|\\\\source|Variable Mapping"),
                             "", x))
    })))
  # add names to data frame
  colnames(out) <- sections
  rownames(out) <- gsub(":", "", names)
  # clean variable mapping
  out$Mapping <- unlist(lapply(out$Mapping, function(x) {
    gsub("\\|", " | ",
         gsub("\\_", " ", 
              gsub("\\(|\\)", "",
                   gsub(" ", " - ",
                        gsub("(\\S* \\S*) ","\\1|",
                             gsub("\\s+(?=[^()]*\\))", "_",
                                  gsub("('.*?')", "(\\1)", x), perl=TRUE))))))
  }))
  # open preparation script if declared
  if (open_script == TRUE & !is.null(dataset)) {
    url <- paste0("https://github.com/globalgov/", package, "/blob/main/data-raw/",
                  database, "/", dataset, "/", "prepare-", dataset, ".R")
    tryCatch({
        utils::browseURL(url, browser = getOption("browser"), encodeIfNeeded = FALSE)
      }, error = function(e) {
        message(paste0("Unable to open preparation script, please visit: ", url))
        })
  } else if (open_script == TRUE & is.null(dataset)) {
    message("Please declare a dataset to open a preparation script.")
  }
  # open codebook if declared
  if (open_codebook == TRUE & !is.null(dataset)) {
    url <- paste0("https://github.com/globalgov/", package, "/raw/develop/data-raw/",
                  database, "/", dataset,"/", dataset)
    tryCatch({
      utils::browseURL(paste0(url, "/", "OriginalCodebook.pdf"),
                       browser = getOption("browser"), encodeIfNeeded = FALSE)
    }, error = function(e) {
      message(paste0("Unable to open codebook, please visit: ", url))
      })
    } else if (open_codebook == TRUE & is.null(dataset)) {
    message("Please declare a dataset to open codebook.")
    }
  # out a with a tibble
  dplyr::as_tibble(out, rownames = "Dataset") %>%
    dplyr::relocate(Dataset, Source, URL, Mapping)
}

# Helper function to get help file into text
get_help_file <- function(file) {
  path <- dirname(file)
  dirpath <- dirname(path)
  if (!file.exists(dirpath)) 
    stop(gettextf("invalid %s argument", sQuote("file")), 
         domain = NA)
  pkgname <- basename(dirpath)
  RdDB <- file.path(path, pkgname)
  fetchRdDB <- function(db) {
    vals <- db$vals
    vars <- db$vars
    datafile <- db$datafile
    compressed <- db$compressed
    envhook <- db$envhook
    key <- basename(file)
    fetch <- function(key) lazyLoadDBfetch(vals[key][[1L]],
                                           datafile, compressed, envhook)
    fetch(key)
  }
  lazyLoadDBexec(RdDB, fetchRdDB)
}

#' @describeIn call_ Call treaties from 'many' datasets
#' @details Certain datasets, or consolidated databases, in 'many' packages
#' contains information on treaties which can be retrieved
#' with `call_treaties()`.
#' @export
call_treaties <- function(dataset, treaty_type = NULL,
                          variable = NULL, actor = NULL) {
  Memberships <- manyID <- NULL
  # check if key is valid
  if (!any(colnames(dataset) == "manyID")) {
    stop("Please declare a many dataset")
  }
  # get variables, if declared
  if (!is.null(variable)) {
    out <- dataset[,c("manyID", variable)] %>% dplyr::distinct()
  } else {
    out <- dataset[,"manyID"] %>% dplyr::distinct()
  }
  # subset treaty types
  if (!is.null(treaty_type)) {
    if (treaty_type == "bilateral") {
      out <- subset(out, stringr::str_detect(manyID, "\\-"))
    }
    if (treaty_type == "multilateral") {
      out <- subset(out, stringr::str_detect(manyID, "\\-", negate = TRUE))
    }
  }
  if (!is.null(actor)) {
    actors <- dataset[,c("manyID", actor)] %>% dplyr::distinct()
    names(actors)[names(actors) == actor] <- "Memberships"
    out <- actors %>%
      dplyr::group_by(manyID) %>%
      dplyr::summarise(Memberships = toString(Memberships)) %>%
      dplyr::ungroup() %>%
      dplyr::right_join(out, by = "manyID") %>%
      distinct()
  }
  out
}

#' #' @describeIn call_ Call 'many' texts
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

thisRequires <- function(pkgname){
  if (!requireNamespace(pkgname, quietly = TRUE)) {
    if(utils::askYesNo(msg = paste("The", pkgname, 
    "package is required to run this function. Would you like to install", pkgname, "from CRAN?"))) {
      utils::install.packages(pkgname)
    } else {
      stop(paste("Please install", pkgname, "from CRAN to run this function."))
    }
  }
}
