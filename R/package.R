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
#' @importFrom usethis create_package
#' @examples
#' \dontrun{
#' qpackage_create("qStates",
#' c("James Hollway", "Henrique Sposito"))
#' }
#' @export
create_qPackage <- function(packageName,
                            packageAuthor,
                            update = TRUE) {
  
  # Checks to see whether inputs are correct
  if (is.null(packageName)) stop("Please declare a package name")
  if (!startsWith(packageName, "q")) stop("Package name must start with a 'q'")
  if (is.null(packageAuthor)) stop("Please declare at least one author")
  
  # Checks to see whether path already contains files or is empty
  # path<- usethis::create_package(path, rstudio = TRUE, open = FALSE)
  # old_project <- usethis::proj_set(path)
  # on.exit(usethis::proj_set(old_project), add = TRUE)
  # The lines above are folded into create_package. 
  
  # Step one: ensure/create package/project structure
  # usethis::create_package()
  
  # Add README
  qtemplate("qPackage-README.Rmd",
            "README.Rmd", 
            data = list(package = packageName,
                        author = packageAuthor))
  # TODO: Make sure workflow creates README.md from README.Rmd
  # TODO: Add badges to qPackage README
  
  # Step two: ensure/create core package files
  # usethis::use_ccby_license(name = packageAuthor)
  
  # usethis::use_tidy_description()
  # usethis::use_namespace()
  # usethis::use_news_md()
  
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

  usethis::ui_todo("Remember to set up your project together with Github for visibility etc.")
  # usethis::ui_todo("{ui_code('use_pkgdown()')}")
  
  # Step five: checks package (?) 
  # usethis::use_spell_check()
  # usethis::use_tibble()
  
  # Step 6: create GitHub repository (?)
  # usethis::use_git() # The usethis::use_github() may also be an interesting option to explore here. 
  # usethis::proj_activate()
  }