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
#' @importFrom usethis create_tidy_package
#' @examples
#' \dontrun{
#' qpackage_create("qStates",
#' c("James Hollway", "Henrique Sposito"))
#' }
#' @export
create_qpackage <- function(packageName,
                            packageAuthor,
                            update = TRUE) {
  if (is.null(packageName)) stop("Please declare a package name")
  if (!startsWith(packageName, "q")) stop("Package name must start with a 'q'")
  if (is.null(packageAuthor)) stop("Please declare at least one author")
  
  qpackage() 
}
  
# Similar to usethis::create_tidy_package(getwd(), name = packageName) but set up to specific parameters. 
# First we need to create a package and projetc in R Studio. This was taken/modified from usethis::create_package

#' @export 
create_package <- function(path,
                           fields = list(),
                           rstudio = rstudioapi::isAvailable(),
                           roxygen = TRUE,
                           check_name = TRUE,
                           open = rlang::is_interactive()) {
  path <- user_path_prep(path)
  check_path_is_directory(path_dir(path))
  
  name <- path_file(path_abs(path))
  if (check_name) {
    check_package_name(name)
  }
  check_not_nested(path_dir(path), name)
  
  create_directory(path)
  old_project <- proj_set(path, force = TRUE)
  on.exit(proj_set(old_project), add = TRUE)
  
  use_directory("R")
  use_description(fields, check_name = FALSE, roxygen = roxygen)
  use_namespace(roxygen = roxygen)
  
  if (rstudio) {
    use_rstudio()
  }
  
  if (open) {
    if (proj_activate(proj_get())) {
      # working directory/active project already set; clear the on.exit()'s
      on.exit()
    }
  }
  
  invisible(proj_get())
}

#' @export
#' @rdname create_package
create_project <- function(path,
                           rstudio = rstudioapi::isAvailable(),
                           open = rlang::is_interactive()) {
  path <- user_path_prep(path)
  name <- path_file(path_abs(path))
  check_not_nested(path_dir(path), name)
  
  create_directory(path)
  old_project <- proj_set(path, force = TRUE)
  on.exit(proj_set(old_project), add = TRUE)
  
  use_directory("R")
  
  if (rstudio) {
    use_rstudio()
  } else {
    ui_done("Writing a sentinel file {ui_path('.here')}")
    ui_todo("Build robust paths within your project via {ui_code('here::here()')}")
    ui_todo("Learn more at <https://here.r-lib.org>")
    file_create(proj_path(".here"))
  }
  
  if (open) {
    if (proj_activate(proj_get())) {
      # working directory/active project already set; clear the on.exit()'s
      on.exit()
    }
  }
  
  invisible(proj_get())
}  

#' @export
qpackage <- function(path, copyright_holder = NULL) {
    path <- create_package(path, rstudio = TRUE, open = FALSE)
    old_project <- proj_set(path)
    on.exit(proj_set(old_project), add = TRUE)
    
    use_testthat()
    use_ccby_license()
    use_tidy_description()
    use_namespace()
    use_news_md()
    
    use_readme_rmd(open = FALSE)
    use_read_md (open = FALSE)
    use_github_action_prcommands()
    use_github_action_prchecks()
    use_github_action_pushrelease()
    
    use_github()
    ui_todo("In the new package, remember to do:")
    ui_todo("{ui_code('use_git()')}")
    ui_todo("{ui_code('use_github()')}")
    ui_todo("{ui_code('use_tidy_github_actions()')}")
    ui_todo("{ui_code('use_pkgdown()')}")
    
    proj_activate(path)
}  

# Create a basic testthat folder, taken from usethis. 

#' @export
use_testthat <- function(edition = NULL) {
  use_testthat_impl(edition)
  
  ui_todo(
    "Call {ui_code('use_test()')} to initialize a basic test file and open it \\
    for editing."
  )
}

use_testthat_impl <- function(edition = NULL) {
  check_installed("testthat")
  if (utils::packageVersion("testthat") < "2.1.0") {
    ui_stop("testthat 2.1.0 or greater needed. Please install before re-trying")
  }
  
  if (is_package()) {
    use_dependency("testthat", "Suggests")
    
    edition <- check_edition(edition)
    use_description_field("Config/testthat/edition", edition)
  }
  
  use_directory(path("tests", "testthat"))
  use_template(
    "testthat.R",
    save_as = path("tests", "testthat.R"),
    data = list(name = project_name())
  )
}

# License, taken from usethis. 

#' @export
use_ccby_license <- function() {
  if (is_package()) {
    use_description_field("License", "CC BY 4.0", overwrite = TRUE)
  }
  use_license_template("ccby-4")
}

use_license_template <- function(license, data = list()) {
  license_template <- glue("license-{license}.md")
  
  use_template(license_template,
               save_as = "LICENSE.md",
               data = data,
               ignore = TRUE
  )
}  


use_template <- function(template,
                         save_as = template,
                         data = list(),
                         ignore = FALSE,
                         open = FALSE,
                         package = "usethis") {
  template_contents <- render_template(template, data, package = package)
  new <- write_over(proj_path(save_as), template_contents)
  
  if (ignore) {
    use_build_ignore(save_as)
  }
  
  if (open && new) {
    edit_file(proj_path(save_as))
  }
  
  invisible(new)
}

# Taken from usethis::use_tidy_description 

#' @export
use_tidy_description <- function() {
  desc <- desc::description$new(file = proj_get())
  tidy_desc(desc)
  desc$write()
  invisible(TRUE)
  
}

# Set contributing, issue template and github modified from usethis.  

#' @export
use_github <- function() {
  use_dot_github()
  use_coc()
  use_contributing()
  use_issue_template()
}

# Path to our COC template in qDatr

#' 
use_coc <- function(path = NULL) {
  if (!is.null(path)) {
    use_directory(path, ignore = is_package())
  }
  save_as <- pkgdown::path_join(c(path, "CODE_OF_CONDUCT.md"))
  
  new <- use_template(
    "CODE_OF_CONDUCT.md",
    save_as = save_as,
    ignore = is_package() && is.null(path)
  )
  
  href <- pkgdown_url(pedantic = TRUE) %||%
    "https://raw.githubusercontent.com/globalgov/qDatr/main/.github/CODE_OF_CONDUCT.md"
  href <- paste0(href, "/CODE_OF_CONDUCT.txt")
  
  ui_todo("Don't forget to describe the code of conduct in your README:")
  ui_code_block("
    ## Code of Conduct
    Please note that the {project_name()} project is released with a \\
    [Contributor Code of Conduct]({href}). By contributing to this project, \\
    you agree to abide by its terms."
  )
  
  invisible(new)
}


use_contributing <- function(path = NULL) {
  if (!is.null(path)) {
    use_directory(path, ignore = is_package())
  }
  save_as <- pkgdown::path_join(c(path, "Contributing.md"))
  
  newc <- use_template(
    "Contributing.md",
    save_as = save_as,
    ignore = is_package() && is.null(path)
  )
  
  cref <- pkgdown_url(pedantic = TRUE) %||%
    "https://raw.githubusercontent.com/globalgov/qDatr/main/.github/CONTRIBUTING.md"
  cref <- paste0(href, "/Contributing.txt")
  invisible(newc)
}


use_dot_github <- function(ignore = TRUE) {
  use_directory(".github", ignore = ignore)
  use_git_ignore("*.html", directory = ".github")
}


use_pr_template <-  function(path = NULL) {
  if (!is.null(path)) {
    use_directory(path, ignore = is_package())
  }
  save_as <- pkgdown::path_join(c(path, "pull_request_template.md"))
  
  newpr <- use_template(
    "pull_request_template.md",
    save_as = save_as,
    ignore = is_package() && is.null(path)
  )
  
  cref <- pkgdown_url(pedantic = TRUE) %||%
    "https://raw.githubusercontent.com/globalgov/qDatr/main/.github/pull_request_template.md"
  cref <- paste0(href, "/pull_request_template.txt")
  invisible(newpr)
}
    
# Adapting readme.rmd     
#' @export
use_readme_rmd <- function(open = rlang::is_interactive()) {
  check_installed("rmarkdown")
  
  data <- project_data()
  data$Rmd <- TRUE
  data$on_github <- origin_is_on_github()
  
  new <- use_template(
    if (is_package()) "package-README" else "project-README",
    "README.Rmd",
    data = data,
    ignore = is_package(),
    open = open
  )
  if (!new) {
    return(invisible(FALSE))
  }
  
  if (uses_git()) {
    use_git_hook(
      "pre-commit",
      render_template("readme-rmd-pre-commit.sh")
    )
  }
  
  invisible(TRUE)
}

#' @export
use_readme_md <- function(open = rlang::is_interactive()) {
  use_template(
    if (is_package()) "package-README" else "project-README",
    "README.md",
    data = project_data(),
    open = open
  )
}

# Adding news.md from usethis. 
#' @export
use_news_md <- function(open = rlang::is_interactive()) {
  check_no_uncommitted_changes()
  
  use_template(
    "NEWS.md",
    data = package_data(),
    open = open
  )
  
  git_ask_commit("Add NEWS.md", untracked = TRUE, paths = "NEWS.md")
}

package_data <- function(base_path = proj_get()) {
  desc <- desc::description$new(base_path)
  as.list(desc$get(desc$fields()))
}

# Adding a path for actions adapted from usethis. 
#' @export
use_github_action_prchecks <- function(name,
                                  url = NULL,
                                  save_as = NULL,
                                  ignore = TRUE,
                                  open = FALSE) {
  
  # Check if a custom URL is being used.
  if (is.null(url)) {
    stopifnot(is_string(name))
    
    # Append a `.yaml` extension if needed
    if (!grepl("[.]yaml$", name)) {
      name <- paste0(name, ".yaml")
    }
    
    url <- glue(
      "https://raw.githubusercontent.com/globalgov/qDatr/main/.github/workflows/prchecks.yml"
    )
  } else {
    stopifnot(is_string(url))
  }
  
  if (is.null(save_as)) {
    save_as <- basename(url)
  }
  
  contents <- read_utf8(url)
  
  use_dot_github(ignore = ignore)
  
  save_as <- path(".github", "workflows", save_as)
  create_directory(dirname(proj_path(save_as)))
  
  new <- write_over(proj_path(save_as), contents)
  
  if (open && new) {
    edit_file(proj_path(save_as))
  }
  
  invisible(new)
}

#' @export
use_github_action_pushrelease <- function(name,
                                       url = NULL,
                                       save_as = NULL,
                                       ignore = TRUE,
                                       open = FALSE) {
  
  # Check if a custom URL is being used.
  if (is.null(url)) {
    stopifnot(is_string(name))
    
    # Append a `.yaml` extension if needed
    if (!grepl("[.]yaml$", name)) {
      name <- paste0(name, ".yaml")
    }
    
    url <- glue(
      "https://raw.githubusercontent.com/globalgov/qDatr/main/.github/workflows/pushrelease.yml"
    )
  } else {
    stopifnot(is_string(url))
  }
  
  if (is.null(save_as)) {
    save_as <- basename(url)
  }
  
  contents <- read_utf8(url)
  
  use_dot_github(ignore = ignore)
  
  save_as <- path(".github", "workflows", save_as)
  create_directory(dirname(proj_path(save_as)))
  
  new <- write_over(proj_path(save_as), contents)
  
  if (open && new) {
    edit_file(proj_path(save_as))
  }
  
  invisible(new)
}

#' @export
use_github_action_prcommands <- function(name,
                                          url = NULL,
                                          save_as = NULL,
                                          ignore = TRUE,
                                          open = FALSE) {
  
  # Check if a custom URL is being used.
  if (is.null(url)) {
    stopifnot(is_string(name))
    
    # Append a `.yaml` extension if needed
    if (!grepl("[.]yaml$", name)) {
      name <- paste0(name, ".yaml")
    }
    
    url <- glue(
      "https://raw.githubusercontent.com/globalgov/qDatr/main/.github/workflows/prcommands.yml"
    )
  } else {
    stopifnot(is_string(url))
  }
  
  if (is.null(save_as)) {
    save_as <- basename(url)
  }
  
  contents <- read_utf8(url)
  
  use_dot_github(ignore = ignore)
  
  save_as <- path(".github", "workflows", save_as)
  create_directory(dirname(proj_path(save_as)))
  
  new <- write_over(proj_path(save_as), contents)
  
  if (open && new) {
    edit_file(proj_path(save_as))
  }
  
  invisible(new)
}

# Namespace function from usethis
#' @export
use_namespace <- function(roxygen = TRUE) {
  check_is_package("use_namespace()")
  
  path <- proj_path("NAMESPACE")
  if (roxygen) {
    write_over(path, c("# Generated by roxygen2: do not edit by hand", ""))
  } else {
    write_over(path, 'exportPattern("^[^\\\\.]")')
  }
}

# Still not addressed:
# usethis::use_github_actions_badge()
# usethis::use_spell_check()
# usethis::use_github_action_check_standard()
# usethis::use_git()
# usethis::use_tidy_github_actions()
# usethis::use_tibble()


