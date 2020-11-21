#' Create a new package in the qDatr ecosystem
#'
#' Creates a new package in, and consistent with, the qDatr ecosystem
#' @param packageName A string giving the desired name of the package,
#' must start with "q"
#' @param packageAuthor A string, list or vector giving the package
#' author(s), required
#' @param update A logical indicating whether existing files should be
#' overwritten, by default TRUE.
#' TODO: do these checks internally and smartly...
#' @details The function establishes many of the required files and
#' folder structures required for a qDatr-consistent data package.
#' @return A new package structure
#' @import usethis
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_split
#' @examples
#' \dontrun{
#' setup_package("qStates", c("James Hollway", "Henrique Sposito"))
#' }
#' @export
setup_package <- function(packageName = NULL,
                          packageAuthor = NULL,
                          update = TRUE) {
  
  # Checks to see whether inputs are correct
  wd <- getwd()
  if (is.null(packageName)){
    if (file.exists(paste0(wd, "/DESCRIPTION"))){
      packageName <- read.dcf(paste0(wd, "/DESCRIPTION"))[[1]]
      usethis::ui_done("Obtained package name from existing DESCRIPTION file.")
      if (!startsWith(packageName, "q")) stop("Package name must start with a 'q'")
    } else {
      stop("Please declare a package name")
    }
  }
    
  if (is.null(packageAuthor)){
    if (file.exists(paste0(wd, "/DESCRIPTION"))){
      packageAuthor <- read.dcf(paste0(wd, "/DESCRIPTION"))[[4]]
      packageAuthor <- stringr::str_replace_all(packageAuthor, "\",\nfamily = \"", " ")
      packageAuthor <- stringr::str_replace_all(packageAuthor, "c\\(", "")
      packageAuthor <- stringr::str_replace_all(packageAuthor, "person\\(given = \"", "")
      packageAuthor <- stringr::str_replace_all(packageAuthor, "\\n", "")
      packageAuthor <- stringr::str_replace_all(packageAuthor, "\".*", "")
      usethis::ui_done("Obtained lead author name from existing DESCRIPTION file.")
    } else {
      stop("Please declare at least one author")
    }
  }
    
  # Step one: ensure/create package/project structure
  # Add DESCRIPTION
  given <- stringr::str_split(packageAuthor, " ")[1]
  family <- stringr::str_split(packageAuthor, " ")[2]
  qtemplate("qPackage-DESC.dcf",
            "DESCRIPTION", 
            data = list(package = packageName,
                        given = given,
                        family = family))
  usethis::ui_done("Added DESCRIPTION file. Modify if necessary.")
  # Add R folder
  usethis::use_directory("R")
  usethis::ui_done("Created R/ folder. Here is where any scripts go.")
  # Add LICENSE
  usethis::use_ccby_license(name = packageAuthor)
  usethis::ui_done("Added CCBY license. Modify if necessary.")
  # Add NAMESPACE
  usethis::use_namespace()
  usethis::ui_done("Created NAMESPACE file. Don't modify it.")
  # Add NEWS
  if (!file.exists(paste0(wd, "/NEWS.md"))){
    qtemplate("qPackage-NEWS.md",
              "NEWS.md", 
              data = list(package = packageName))
    usethis::ui_done("Added starter NEWS file. Update for every release.")
  }
  # Add README
  qtemplate("qPackage-README.Rmd",
            "README.Rmd", 
            data = list(package = packageName,
                        author = packageAuthor))
  usethis::ui_done("Added standard README.")
  # TODO: Add badges to qPackage README
  
  # Step two: ensure/create core package files
  usethis::use_testthat()
  
  # Step three: ensure/create Github files
  usethis::use_directory(".github")
  usethis::ui_done("Created .github folder.")
  qtemplate("qPackage-COC.md",
            fs::path(".github", "CODE_OF_CONDUCT", ext = "md"),
            data = list(package = packageName,
                        author = packageAuthor),
            open = FALSE)
  usethis::ui_done("Created CODE_OF_CONDUCT file. Modify if necessary.")
  qtemplate("qPackage-CONTRIB.md",
            fs::path(".github", "CONTRIBUTING.md"),
            data = list(package = packageName,
                        author = packageAuthor),
            open = FALSE)
  usethis::ui_done("Created CONTRIBUTING file. Modify if necessary.")
  qtemplate("qPackage-PR.md",
            fs::path(".github", "pull_request_template.md"),
            data = list(package = packageName,
                        author = packageAuthor),
            open = FALSE)
  usethis::ui_done("Created PR template. Modify if necessary.")
  
  usethis::use_directory(".github/ISSUE_TEMPLATE")
  usethis::ui_done("Created ISSUE_TEMPLATE folder.")
  qtemplate("qPackage-Bugs.md",
            fs::path(".github", "ISSUE_TEMPLATE", "bug_report.md"),
            data = list(package = packageName,
                        author = packageAuthor),
            open = FALSE)
  usethis::ui_done("Created bug report issue template. Modify if necessary.")
  qtemplate("qPackage-Features.md",
            fs::path(".github", "ISSUE_TEMPLATE", "feature_request.md"),
            data = list(package = packageName,
                        author = packageAuthor),
            open = FALSE)
  usethis::ui_done("Created feature request issue template. Modify if necessary.")
  
  usethis::use_directory(".github/workflows")
  usethis::ui_done("Created workflows folder.")
  file.copy(fs::path_package(package = "qDatr", "templates", "qPackage-Check.yml"), 
            fs::path(".github", "workflows", "prchecks.yml"))
  usethis::ui_done("Added checks workflow upon opening a push release.")
  file.copy(fs::path_package(package = "qDatr", "templates", "qPackage-Commands.yml"), 
            fs::path(".github", "workflows", "prcommands.yml"))
  usethis::ui_done("Added commands workflow upon labelling a push release.")
  file.copy(fs::path_package(package = "qDatr", "templates", "qPackage-Release.yml"), 
            fs::path(".github", "workflows", "pushrelease.yml"))
  usethis::ui_done("Added release workflow upon merging a push release.")

  usethis::ui_todo("Remember to set up your project together with Github for visibility etc.")
  #usethis::ui_todo("{ui_code('use_pkgdown()')}")
  
  # Step five: checks package (?) 
  # usethis::use_spell_check()
  # usethis::use_tibble()
  
  # Step 6: create GitHub repository (?)
  # usethis::use_git() # The usethis::use_github() may also be an interesting option to explore here. 
  # usethis::proj_activate()
}

#' Find and download packages in the qDatr ecosystem
#'
#' Find and download packages in the qDatr ecosystem
#' @param pkg A character vector of package names 
#' @details Need to write up these details...
#' @return If no package name is provided, this function prints a table (tibble) to the console 
#' with details on packages that are currently available within the qDatr ecosystem.
#' If one or more package names are provided, these will be installed from 
#' @importFrom pointblank %>%
#' @importFrom dplyr filter
#' @importFrom stringr str_detect
#' @examples
#' \dontrun{
#' get_packages() # This prints a table (tibble) to the console with details on the current
#' get_packages("qStates") # This downloads and installs from github the latest version of a qPackage
#' }
#' @export
get_packages <- function(pkg){
  
  if (missing(pkg)){
    res <- tibble::as_tibble(jsonlite::fromJSON("http://rpkg-api.gepuro.net/rpkg?q=q"))
    res <- res %>% dplyr::filter(stringr::str_detect(pkg_name, "/q[[:upper:]]")) %>%
      dplyr::filter(stringr::str_detect(title, "read-only mirror")) %>%
      dplyr::filter(stringr::str_detect(pkg_name, "globalgov")) 
    # At the moment, just our packages, but we can either expand the list of recognised contributors
    # or remove the condition entirely in the future.
    # TODO: check potential packages for dependency on qDatr
    res
    # TODO: expand this report by adding information on current release version available
    # TODO: expand this report by adding information on which packages, if any, are already installed
    # TODO: expand this report by adding information on whether all checks/tests are passing
    # TODO: expand this report by adding information on number of datacubes, datasets, and observations available
    # TODO: expand this report by adding information on sources
  }
  
  # TODO: make it possible to select (say, by number) which datasets to install from github
  # TODO: consider reexporting e.g. magrittr's pipe (%>%) within qDatr
  
  if (!missing(pkg)){
    remotes::install_github(pkg)
  }
  
}

