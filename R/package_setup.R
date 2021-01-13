#' Create a new package in the qData ecosystem
#'
#' Creates a new package in, and consistent with, the qData ecosystem
#' @param packageName A string giving the desired name of the package,
#' must start with "q"
#' @param packageAuthor A string, list or vector giving the package
#' author(s), required
#' @param update A logical indicating whether existing files should be
#' overwritten, by default TRUE.
#' @param path A string, if missing default is path to the working directory
#' @details The function establishes many of the required files and
#' folder structures required for a qData-consistent data package.
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
                          update = TRUE,
                          path = getwd()) {
  
  # Checks to see whether inputs are correct
  # usethis:::check_path_is_directory(fs::path_dir(path))
  name <- fs::path_file(fs::path_abs(path))
  # usethis:::check_not_nested(fs::path_dir(path), name)
  # usethis:::create_directory(path)
  
  # Step zero: get details from existing files, if present
  if (is.null(packageName)){
    if (file.exists(paste0(path, "/DESCRIPTION"))){
      packageName <- read.dcf(paste0(path, "/DESCRIPTION"))[[1]]
      usethis::ui_done("Obtained package name from existing DESCRIPTION file.")
      if (!startsWith(packageName, "q")) stop("Package name must start with a 'q'")
    } else {
      stop("Please declare a package name")
    }
  }
  
  if (is.null(packageAuthor)){
    if (file.exists(paste0(path, "/DESCRIPTION"))){
      packageAuthor <- read.dcf(paste0(path, "/DESCRIPTION"))[[4]]
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
  # TODO: add package authors from ORCID numbers
  
  # Step one: ensure/create package/project structure
  # Add DESCRIPTION
  given <- stringr::str_split(packageAuthor, " ")[1]
  family <- stringr::str_split(packageAuthor, " ")[2]
  qtemplate("qPackage-DESC.dcf",
            "DESCRIPTION",
            data = list(package = packageName,
                        given = given,
                        family = family),
            path = path)
  usethis::ui_done("Added DESCRIPTION file. Modify if necessary.")
  # Add R folder
  usethis::ui_done("Created R/ folder. Here is where any scripts go.")
  # Add LICENSE
  usethis::use_ccby_license()
  usethis::ui_done("Added CCBY license. Modify if necessary.")
  # Add NAMESPACE
  usethis::use_namespace()
  usethis::ui_done("Created NAMESPACE file. Don't modify it.")
  # Add NEWS
  if (!file.exists(paste0(path, "/NEWS.md"))){
    qtemplate("qPackage-NEWS.md",
              "NEWS.md",
              data = list(package = packageName),
              path = path)
    usethis::ui_done("Added starter NEWS file. Update for every release.")
  }
  # Add README
  qtemplate("qPackage-README.Rmd",
            "README.Rmd",
            data = list(package = packageName,
                        author = packageAuthor),
            path = path)
  usethis::ui_done("Added standard README.")
  # TODO: Add badges to qPackage README
  
  # Step two: ensure/create core package files
  usethis::use_testthat()
  
  # Step three: ensure/create Github files
  create_directory(paste0(path, "/.github"))
  usethis::ui_done("Created .github folder.")
  qtemplate("qPackage-COC.md",
            fs::path(".github", "CODE_OF_CONDUCT", ext = "md"),
            data = list(package = packageName,
                        author = packageAuthor),
            path = path,
            open = FALSE)
  usethis::ui_done("Created CODE_OF_CONDUCT file. Modify if necessary.")
  qtemplate("qPackage-CONTRIB.md",
            fs::path(".github", "CONTRIBUTING.md"),
            data = list(package = packageName,
                        author = packageAuthor),
            path = path,
            open = FALSE)
  usethis::ui_done("Created CONTRIBUTING file. Modify if necessary.")
  qtemplate("qPackage-PR.md",
            fs::path(".github", "pull_request_template.md"),
            data = list(package = packageName,
                        author = packageAuthor),
            path = path,
            open = FALSE)
  usethis::ui_done("Created PR template. Modify if necessary.")
  
  create_directory(paste0(path, "/.github/ISSUE_TEMPLATE"))
  usethis::ui_done("Created ISSUE_TEMPLATE folder.")
  qtemplate("qPackage-Bugs.md",
            fs::path(".github", "ISSUE_TEMPLATE", "bug_report.md"),
            data = list(package = packageName,
                        author = packageAuthor),
            path = path,
            open = FALSE)
  usethis::ui_done("Created bug report issue template. Modify if necessary.")
  qtemplate("qPackage-Features.md",
            fs::path(".github", "ISSUE_TEMPLATE", "feature_request.md"),
            data = list(package = packageName,
                        author = packageAuthor),
            path = path,
            open = FALSE)
  usethis::ui_done("Created feature request issue template. Modify if necessary.")
  
  create_directory(paste0(path, "/.github/workflows"))
  usethis::ui_done("Created workflows folder.")
  if(interactive()) {
    file.copy(fs::path_package(package = "qData", "templates", "qPackage-Check.yml"),
              fs::path(".github", "workflows", "prchecks.yml"))
    usethis::ui_done("Added checks workflow upon opening a push release.")
    file.copy(fs::path_package(package = "qData", "templates", "qPackage-Commands.yml"),
              fs::path(".github", "workflows", "prcommands.yml"))
    usethis::ui_done("Added commands workflow upon labelling a push release.")
    file.copy(fs::path_package(package = "qData", "templates", "qPackage-Release.yml"),
              fs::path(".github", "workflows", "pushrelease.yml"))
    usethis::ui_done("Added release workflow upon merging a push release.")
  }
  
  usethis::ui_todo("Remember to set up your project together with Github for visibility etc.")
  #usethis::ui_todo("{ui_code('use_pkgdown()')}")
  
  # Step five: checks package (?)
  # usethis::use_spell_check()
  # usethis::use_tibble()
  
  # Step 6: create GitHub repository (?)
  # usethis::use_git() # The usethis::use_github() may also be an interesting option to explore here.
  # usethis::proj_activate()
}
