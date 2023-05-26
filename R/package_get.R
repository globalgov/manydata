#' Find and download packages in the many packages universe
#'
#' Find and download packages in the many packages universe.
#' @param pkg A character vector of package name.
#' To download multiple packages at once,
#' please declare package names as a vector (e.g. c("pkg1", "pkg2)).
#' @param develop Would you like to download the develop
#' version of the package?
#' FALSE by default.
#' If TRUE, the function downloads the develop version of package from GitHub.
#' @param update Would you like to update many packages
#' available but not up to date?
#' FALSE by default.
#' If TRUE, the function updates all many packages not up to date.
#' @details The function finds and download other packages
#' that belong to the many universe of packages.
#' It allows users to rapidly access the names and other
#' descriptive information of these
#' packages by simply calling the function.
#' If users intend to download and install
#' a package from the universe,
#' they can type the package name within the function.
#' @return If no package name is provided,
#' this function prints a table (tibble)
#' to the console with details on 'many packages'
#' that are currently available within
#' the many universe.
#' If one or more package names are provided,
#' these will be installed from Github.
#' @importFrom tibble as_tibble
#' @importFrom dplyr bind_rows rename relocate %>%
#' @importFrom stringr str_detect str_remove
#' @importFrom jsonlite fromJSON
#' @importFrom httr GET content
#' @importFrom remotes install_github
#' @importFrom utils packageVersion
#' @examples 
#' \donttest{
#' #get_packages()
#' #get_packages("manyenviron")
#' #get_packages(update = TRUE)
#' }
#' @export
get_packages <- function(pkg, develop = FALSE, update = FALSE) {
  # introduce variables to avoid check notes
  Description <- Installed <- Latest <- Name <- Updated <- Repository <-
    description <- full_name <- name <- NULL
  # check for possible issues
  if (missing(pkg)) {
    # get info from GitHub if pkg is missing
    orgs <- "globalgov" # add more users/orgs as they 'register'
    # get package releases, versions, and bind information
    repos <- lapply(orgs, function(x) {
      repo <- paste0("https://api.github.com/users/", x, "/repos")
      repo <- httr::GET(repo, query = list(state = "all",
                                           per_page = 100, page = 1))
      repo <- suppressMessages(httr::content(repo, type = "text"))
      repo <- jsonlite::fromJSON(repo, flatten = TRUE)
      repo <- repo[c("Name", "full_name", "Description")]
      repo$Installed <- get_installed_release(repo$Name)
      repo$Latest <- get_latest_release(repo$full_name)
      repo <- subset(repo, !grepl("Unreleased", repo$Latest))
    })
    repos <- repos %>%
      dplyr::bind_rows() %>%
      dplyr::select(-full_name) %>%
      dplyr::relocate(Name, Installed, Latest, Description) %>%
      tibble::as_tibble()
    if (length(repos) < 5) {
      stop(
      "The download limit from GitHub has been reached.
      To see all the available 'many packages' packages,
      please go to the following link: https://github.com/globalgov")
    } else {
      print(repos, justify = "center")
    }
  } else {
    # download package if pkg is declared
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
  if (update == TRUE) {
    if (exists("repos")) {
      lookup_updates(repos)
    } else {
      repos <- NULL
      lookup_updates(repos)
    }
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

# Helper function to get package versions locally installed
get_installed_release <- function(name) {
  installed_v <- sapply(name, function(x) {
    tryCatch({
      as.character(utils::packageVersion(x))
    }, error = function(e) {
      NA_character_
    })
  })
  installed_v
}

# Helper function to look for package updates
lookup_updates <- function(repos) {
  # introduce variables to avoid check notes
  Description <- Installed <- Latest <- Name <- Updated <- Repository <-
    description <- full_name <- name <- NULL
  tryCatch({
  if(is_null(repos)) {
    orgs <- "globalgov" # add more users/orgs as they 'register'
    # get package releases, versions, and bind information
    repos <- lapply(orgs, function(x) {
      repo <- paste0("https://api.github.com/users/", x, "/repos")
      repo <- httr::GET(repo, query = list(state = "all",
                                           per_page = 100, page = 1))
      repo <- suppressMessages(httr::content(repo, type = "text"))
      repo <- jsonlite::fromJSON(repo, flatten = TRUE)
      repo <- repo[c("name", "full_name", "description")]
      repo$Installed <- get_installed_release(repo$name)
      repo$Latest <- get_latest_release(repo$full_name)
      repo <- subset(repo, !grepl("Unreleased", repo$Latest))
    })
    repos <- repos %>%
      dplyr::bind_rows() %>%
      dplyr::rename(Name = name, Repository = full_name,
                    Description = description) %>%
      dplyr::relocate(Name, Repository, Installed, Latest, Description) %>%
      tibble::as_tibble()
  }
  up <- ifelse(repos$Installed != repos$Latest | is.na(repos$Installed),
               repos$Name, "")
  up <- up[up != ""]
  if (length(up) == 0) {
    cat("All many packages are up to date!")
  } else {
    cat(paste0(paste(up, collapse = " and "), " will be updated."))
    remotes::install_github(c(paste0("globalgov/", up)))
    }   
  }, error = function(e) {
  stop("The download limit from GitHub has been reached.
       To see and download all the available packages in the many universe,
       please go to the following link: https://github.com/globalgov")
    })
}
