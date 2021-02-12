#' Find and download packages in the qData ecosystem
#'
#' Find and download packages in the qData ecosystem
#' @param pkg A character vector of package names
#' @details The function finds and download other packages that belong to
#' the qData ecosystem of data packages. It allows for users to rapidly access
#' the names and other descriptive information of these packages by simply
#' calling the function. If users intend to download a package from the
#' ecosystem, they can to type the package name within the function.
#' @return If no package name is provided, this function prints a table (tibble)
#' to the console with details on packages that are currently available within
#' the qData ecosystem.
#' This includes the name and description of the package,
#' the latest installed and release version number, and the latest release date,
#' and a string of contributors.
#' 
#' If one or more package names are provided, these will be installed from Github.
#' @importFrom pointblank %>%
#' @importFrom stringr str_detect
#' @importFrom tibble as_tibble
#' @importFrom jsonlite fromJSON
#' @importFrom httr GET
#' @importFrom httr content
#' @importFrom remotes install_github
#' @importFrom utils packageVersion
#' @examples
#' \dontrun{
#' get_packages() # This prints a table (tibble)
#' # to the console with details on currently available qPackages
#' get_packages("qStates") # This downloads and installs from Github
#' # the latest version of one or more qPackages
#' }
#' @export
get_packages <- function(pkg) {
  
  if (missing(pkg)) {
    orgs <- "globalgov" # add more users/orgs as they 'register'
    
    get_latest_release <- function(full_name){
      latest <- paste0("https://api.github.com/repos/", full_name, "/releases/latest")
      if(length(latest)==1){
        latest <- httr::GET(latest)
        latest <- suppressMessages(httr::content(latest, type = "text"))
        latest <- jsonlite::fromJSON(latest, flatten = TRUE)$tag_name
      } else {
        latest <- sapply(latest, function(x){
          x <- httr::GET(x)
          x <- suppressMessages(httr::content(x, type = "text"))
          x <- jsonlite::fromJSON(x, flatten = TRUE)$tag_name
          if(is.null(x)){
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
    
    get_latest_date <- function(full_name){
      latest <- paste0("https://api.github.com/repos/", full_name, "/releases/latest")
      if(length(latest)==1){
        latest <- httr::GET(latest)
        latest <- suppressMessages(httr::content(latest, type = "text"))
        latest <- jsonlite::fromJSON(latest, flatten = TRUE)$published_at
      } else {
        latest <- sapply(latest, function(x){
          x <- httr::GET(x)
          x <- suppressMessages(httr::content(x, type = "text"))
          x <- jsonlite::fromJSON(x, flatten = TRUE)$published_at
          if(is.null(x)){
            x <- "Unreleased"
            x
          } else {
            x <- as.character(anytime::anydate(x))
            x
          }
        })
      }
      unlist(latest)
    }
    
    get_installed_release <- function(name){
      installed <- sapply(name, function(x) as.character(utils::packageVersion(x)))
    }
    
    get_contributors <- function(full_name){
      contribs <- paste0("https://api.github.com/repos/", full_name, "/contributors")
      if(length(contribs)==1){
        contribs <- httr::GET(contribs)
        contribs <- suppressMessages(httr::content(contribs, type = "text"))
        contribs <- jsonlite::fromJSON(contribs, flatten = TRUE)$login
        contribs <- paste(contribs, collapse = "\n")
      } else {
        contribs <- sapply(contribs, function(x){
          x <- httr::GET(x)
          x <- suppressMessages(httr::content(x, type = "text"))
          x <- jsonlite::fromJSON(x, flatten = TRUE)$login
          x <- paste(x, collapse = ", ")
        })
      }
      unlist(contribs)
    }
    
    repos <- lapply(orgs, function(x){
      repo <- paste0("https://api.github.com/users/", x, "/repos")
      repo <- httr::GET(repo, query = list(state = "all", per_page = 100, page = 1))
      repo <- suppressMessages(httr::content(repo, type = "text"))
      repo <- jsonlite::fromJSON(repo, flatten = TRUE)
      repo <- subset(repo, grepl("q[[:upper:]]", repo$name))
      repo <- repo[c("name","full_name","description")]
      repo$installed <- get_installed_release(repo$name)
      repo$latest <- get_latest_release(repo$full_name)
      repo$updated <- anytime::anydate(get_latest_date(repo$full_name))
      repo$contributors <- get_contributors(repo$full_name)
      repo <- tibble::as_tibble(repo)
    })
    
    repos <- dplyr::bind_rows(repos)
    print(repos)
  }
  
  if (!missing(pkg)) {
    if(stringr::str_detect(pkg, "/")) {
      remotes::install_github(pkg) 
    } else {
      pkg <- paste0("globalgov/", pkg)
      remotes::install_github(pkg)  
    }
  }
  
}

# Helper function from usethis:::create_directory()
create_directory <- function(path){
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
