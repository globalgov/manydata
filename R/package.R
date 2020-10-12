#' Create a new package in the qDatr ecosystem
#'
#' Creates a new package in and consistent with the qDatr ecosystem
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
  

  usethis::ui_todo("Remember to set up your project together with Github for visibility etc.")
  # usethis::ui_todo("{ui_code('use_pkgdown()')}")
  
  # Step five: checks package (?) 
  # usethis::use_spell_check()
  # usethis::use_tibble()
  
  # Step 6: create GitHub repository (?)
  # usethis::use_git() # The usethis::use_github() may also be an interesting option to explore here. 
  # usethis::proj_activate()
  }


## Functions to add our GitHub actions checks templates to qpackage.

qchecks <- function() {
  usethis::use_directory(fs::path(".github", "workflows"))
  usethis::use_git_ignore("*.html", directory = ".github")
}

qprchecks <- function() {
  qchecks()
  qtemplate(
    "prchecks.yml", fs::path("workflows", "prchecks.yml"),
    data = usethis:::project_data()
  )
}

qprcommands <- function() {
  qchecks()
  qtemplate(
    "prcommands.yml",
    fs::path("workflows", "prcommands.yml"),
    data = usethis:::project_data()
  )
}


# With pushrelease it is a little more complicated as it requires the package names to 
# be changed, but I am trying to work around it. 
# qpushrelease <- function() {
#qchecks()
#qtemplate("prcommands.yml",
#  fs::path("workflows", "prcommands.yml"),
#  data = usethis:::project_data()) {
# out <- textclean::mgsub("qdatr", basename(packageName), out)
#}
#out
#}

## Funtions to add our own README, COC, contributing and issue PR templates. 

qreadme <- function(open = rlang::is_interactive()) {
  librarian::check_installed("rmarkdown")
  data <- usethis:::project_data()
  data$Rmd <- TRUE
  new <- qtemplate("README.Rmd",
                   data = data,
                   open = open)
  invisible(TRUE)
}

qgithub <- function () {
  usethis:::use_dot_github()
  qcoc()
  qprtemplate()
  qcontributing()
}

# We do not have a REAMDME.md file template yet.

qcoc <- function() {
  usethis:::use_dot_github()
  usethis::use_directory(fs::path(".github"))
  qtemplate("CODE_OF_CONDUCT.md",
            fs::path(".github", "CODE_OF_CONDUCT.md"))
}

qprtemplate <- function() {
  usethis:::use_dot_github()
  usethis::use_directory(fs::path(".github"))
  qtemplate("pull_request_template.md",
            fs::path(".github", "pull_request_template.md"))
}

qcontributing <- function() {
  usethis:::use_dot_github()
  usethis::use_directory(fs::path(".github"))
  qtemplate("CONTRIBUTING_GGO.md",
            fs::path(".github", "CONTRIBUTING_GGO.md"))
}

# Helpers ...

user_path_prep <- function(path) {
  fs::path_expand(fs::path)
}
