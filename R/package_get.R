#' Find and download packages in the qData ecosystem
#'
#' Find and download packages in the qData ecosystem
#' @param pkg A character vector of package names
#' @details The function finds and download other packages that belong to the qData ecosystem
#' of data packages. It allows for users to rapidly access the names and other descriptive information
#' of these packages by simply calling the function. If users intend to download a package from
#' the ecosystem, they can to type the package name within the function.
#' @return If no package name is provided, this function prints a table (tibble) to the console
#' with details on packages that are currently available within the qData ecosystem.
#' If one or more package names are provided, these will be installed from
#' @importFrom pointblank %>%
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom dplyr rename
#' @importFrom stringr str_detect
#' @importFrom tibble as_tibble
#' @importFrom jsonlite fromJSON
#' @importFrom httr GET
#' @importFrom httr content
#' @importFrom remotes install_github
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
    orgs <- c("globalgov") # add more users/orgs as they 'register'
    
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
            x
          } 
        })
      }
      unlist(latest)
    }
    
    get_installed_release <- function(name){
      installed <- sapply(name, function(x) as.character(packageVersion(x)))
    }
    
    repos <- lapply(orgs, function(x){
      repo <- paste0("https://api.github.com/users/", x, "/repos")
      repo <- httr::GET(repo, query = list(state = "all", per_page = 100, page = 1))
      repo <- suppressMessages(httr::content(repo, type = "text"))
      repo <- jsonlite::fromJSON(repo, flatten = TRUE)
      repo <- tibble::as_tibble(repo) %>%
        dplyr::select(.data$name, .data$full_name, .data$description, .data$updated_at, .data$stargazers_count, .data$open_issues_count) %>%
        dplyr::rename(stargazers = .data$stargazers_count, open_issues = .data$open_issues_count) %>%
        dplyr::filter(stringr::str_detect(.data$name, "q[[:upper:]]")) %>%
        dplyr::mutate(latest = get_latest_release(.data$full_name)) %>%
        dplyr::select(.data$name, .data$full_name, .data$description, .data$latest, .data$updated_at, .data$stargazers, .data$open_issues)
    })
    
    repos <- dplyr::bind_rows(repos)
    print(repos)
    
    # TODO: check potential packages for dependency on qData
    # TODO: expand this report by adding information on which packages, if any, are already installed
    # TODO: expand this report by adding information on whether all checks/tests are passing
    # TODO: expand this report by adding information on number of datacubes, datasets, and observations available
    # TODO: expand this report by adding information on sources
    # TODO: add a list of contributors
  }
  
  # TODO: make it possible to select (say, by number) which datasets to install from github
  
  if (!missing(pkg)) {
    remotes::install_github(pkg)
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
