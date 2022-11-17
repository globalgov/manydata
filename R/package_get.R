#' Find and download packages in the many packages universe
#'
#' Find and download packages in the many packages universe.
#' @param pkg A character vector of package names or number of a package.
#' To download multiple packages at once,
#' please declare package names as a vector (e.g. c("pkg1", "pkg2)).
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
#' to the console with details on packages
#' that are currently available within
#' the many universe.
#' This includes the name and description of the package,
#' the latest installed and release version number,
#' and the latest release date.
#' It also include a list of numbers which orders
#' the package and can be used to load the respective
#' package instead of the name.
#' If one or more package names are provided,
#' these will be installed from Github.
#' @importFrom tibble as_tibble
#' @importFrom dplyr bind_rows rename relocate %>%
#' @importFrom stringr str_detect str_remove
#' @importFrom jsonlite fromJSON
#' @importFrom httr GET content
#' @importFrom remotes install_github
#' @importFrom utils packageVersion askYesNo
#' @importFrom messydates as_messydate
#' @examples 
#' \donttest{
#' #get_packages()
#' }
#' @export
get_packages <- function(pkg) {
  # get info from GitHub if pkg is missing
  if (missing(pkg)) {
    orgs <- "globalgov" # add more users/orgs as they 'register'
    # get releases
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
    # get release dates
    get_latest_date <- function(full_name) {
      latest <- paste0("https://api.github.com/repos/",
                       full_name, "/releases/latest")
      if (length(latest) == 1) {
        latest <- httr::GET(latest)
        latest <- suppressMessages(httr::content(latest, type = "text"))
        latest <- jsonlite::fromJSON(latest, flatten = TRUE)$published_at
      } else {
        latest <- sapply(latest, function(x) {
          x <- httr::GET(x)
          x <- suppressMessages(httr::content(x, type = "text"))
          x <- jsonlite::fromJSON(x, flatten = TRUE)$published_at
          if (is.null(x)) {
            x <- "Unreleased"
            x
          } else {
            x <- as.character(x)
            x
          }
        })
      }
      unlist(latest)
    }
    # get version locally installed
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
    # bind information
    repos <- lapply(orgs, function(x) {
      repo <- paste0("https://api.github.com/users/", x, "/repos")
      repo <- httr::GET(repo, query = list(state = "all",
                                           per_page = 100, page = 1))
      repo <- suppressMessages(httr::content(repo, type = "text"))
      repo <- jsonlite::fromJSON(repo, flatten = TRUE)
      repo <- repo[c("name", "full_name", "description")]
      repo$Installed <- get_installed_release(repo$name)
      repo$Latest <- get_latest_release(repo$full_name)
      repo$Updated <- messydates::as_messydate(get_latest_date(repo$full_name))
      repo <- subset(repo, !grepl("Unreleased", repo$Latest))
      repo <- as.data.frame(repo)
    })
    Description <- Installed <- Latest <- Name <- Updated <- Repository <-
      description <- full_name <- name <- NULL
    repos <- dplyr::bind_rows(repos) %>%
      dplyr::rename(Name = name, Repository = full_name, Description = description) %>%
      dplyr::relocate(Name, Repository, Installed, Latest, Updated, Description) %>%
      tibble::as_tibble()
    # check for possible issues
    if (length(repos) < 2) {
      stop(
      "The download limit from GitHub has been reached.
      To see all the packages in the many universe,
      please go to the following link: https://github.com/globalgov")
    } else {
    print(repos, justify = "center")
    }
  }
  # download package if pkg is declared
  tryCatch({
  if (!missing(pkg)) {
    if (askYesNo(paste0("Would you like to install the main or develop version of ",
                        pkg, "?"), prompts = c("main", "develop", "cancel")) == TRUE) {
      if (stringr::str_detect(pkg, "/")) {
        remotes::install_github(pkg)
        pkg <- strsplit(pkg, "/")[[1]][2]
      } else if (stringr::str_detect(pkg, "^[:digit:]{1}$")) {
        if (pkg == 3) {
          pkg <- "manypkgs"
          remotes::install_github("globalgov/manypkgs")
        } else if (pkg == 2) {
          pkg <- "manyenviron"
          remotes::install_github("globalgov/manyenviron")
        } else if (pkg == 4) {
          pkg <- "manystates"
          remotes::install_github("globalgov/manystates")
        } else if (pkg == 5) {
          pkg <- "manytrade"
          remotes::install_github("globalgov/manytrade")
        }
      } else {
        remotes::install_github(paste0("globalgov/", pkg))
      }
    } else {
      if (stringr::str_detect(pkg, "/")) {
        remotes::install_github(pkg, ref = "develop")
        pkg <- strsplit(pkg, "/")[[1]][2]
      } else if (stringr::str_detect(pkg, "^[:digit:]{1}$")) {
        if (pkg == 3) {
          pkg <- "manypkgs"
          remotes::install_github("globalgov/manypkgs", ref = "develop")
        } else if (pkg == 2) {
          pkg <- "manyenviron"
          remotes::install_github("globalgov/manyenviron", ref = "develop")
        } else if (pkg == 4) {
          pkg <- "manystates"
          remotes::install_github("globalgov/manystates", ref = "develop")
        } else if (pkg == 5) {
          pkg <- "manytrade"
          remotes::install_github("globalgov/manytrade", ref = "develop")
        }
      } else {
        remotes::install_github(paste0("globalgov/", pkg), ref = "develop")
      }
    }
    library(pkg, character.only = TRUE)
  }
  }, error = function(e) {
  stop(paste0("The download limit from GitHub has been reached.
       Please download the package using:
              remotes::github(globalgov/", pkg, ")"))
  })
}

# Helper function from usethis:::create_directory()
create_directory <- function(path) {
  if (dir.exists(path)) {
    return(invisible(FALSE))
  }
  else if (file.exists(path)) {
    usethis::ui_stop("{ui_path(path)} exists but is not a directory.")
  }
  dir.create(path, recursive = TRUE)
  usethis::ui_done("Creating {ui_path(path)}")
  invisible(TRUE)
}
