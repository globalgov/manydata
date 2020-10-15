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
#' qpackage_create("qStates",
#' c("James Hollway", "Henrique Sposito"))
#' }
#' @export
create_qPackage <- function(packageName = NULL,
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
  given <- stringr::str_split(packageAuthor, " ")[1]
  family <- stringr::str_split(packageAuthor, " ")[2]
  qtemplate("qPackage-DESC.dcf",
            "DESCRIPTION", 
            data = list(package = packageName,
                        given = given,
                        family = family))
  usethis::ui_done("Added DESCRIPTION file. Modify if necessary.")
  usethis::use_directory("R")
  usethis::ui_done("Created R/ folder. Here is where any scripts go.")
  usethis::use_namespace()
  usethis::ui_done("Created NAMESPACE file. Don't modify it.")
  usethis::use_news_md()
  usethis::ui_done("Added starter NEWS file. Update for every release.")
  usethis::use_ccby_license(name = packageAuthor)
  usethis::ui_done("Added CCBY license. Modify if necessary.")
  # Add README
  qtemplate("qPackage-README.Rmd",
            "README.Rmd", 
            data = list(package = packageName,
                        author = packageAuthor))
  usethis::ui_done("Added standard README.")
  # TODO: Add badges to qPackage README
  
  # Step two: ensure/create core package files
  
  # Step three: ensure/create Github files
  usethis::use_directory(".github")
  qtemplate("qPackage-COC.md",
            fs::path(".github", "CODE_OF_CONDUCT", ext = "md"),
            data = list(package = packageName,
                        author = packageAuthor))
  qtemplate("qPackage-CONTRIB.md",
            fs::path(".github", "CONTRIBUTING.md"),
            data = list(package = packageName,
                        author = packageAuthor))
  qtemplate("qPackage-PR.md",
            fs::path(".github", "pull_request_template.md"),
            data = list(package = packageName,
                        author = packageAuthor))

  usethis::use_directory(".github/ISSUE_TEMPLATE")
  qtemplate("qPackage-Bugs.md",
            fs::path(".github", "ISSUE_TEMPLATE", "bug_report.md"),
            data = list(package = packageName,
                        author = packageAuthor))
  qtemplate("qPackage-Features.md",
            fs::path(".github", "ISSUE_TEMPLATE", "feature_request.md"),
            data = list(package = packageName,
                        author = packageAuthor))
  
  usethis::use_directory(".github/workflows")
  # TODO: qtemplate() not working with Github workflows because whisker reads workflow variables to be rendered
  # qtemplate("qPackage-Check.yml",
  #           fs::path(".github", "workflows", "prchecks.yml"),
  #           data = list(package = packageName,
  #                       author = packageAuthor))
  # qtemplate("qPackage-Commands.yml",
  #           fs::path(".github", "workflows", "prcommands.yml"),
  #           data = list(package = packageName,
  #                       author = packageAuthor))
  # qtemplate("qPackage-Release.yml",
  #           fs::path(".github", "workflows", "pushrelease.yml"),
  #           data = list(package = packageName,
  #                       author = packageAuthor))
  # TODO: Consider ways to replace "package_" with package name for qPackage-Release.yml template

  usethis::ui_todo("Remember to set up your project together with Github for visibility etc.")
  #usethis::ui_todo("{ui_code('use_pkgdown()')}")
  
  # Step five: checks package (?) 
  # usethis::use_spell_check()
  # usethis::use_tibble()
  
  # Step 6: create GitHub repository (?)
  # usethis::use_git() # The usethis::use_github() may also be an interesting option to explore here. 
  # usethis::proj_activate()
}
