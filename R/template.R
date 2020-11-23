#' Helper function for finding and rendering templates
#'
#' Helper function for finding and rendering templates from the qDatr package
#' @param template Template called
#' @param save_as Path to where the rendered template should be saved
#' @param data Any elements to be entered into the template via Whisker
#' @param ignore For use with usethis::use_build_ignore()
#' @param path Path to where template was gathered from
#' @param open Whether the resulting template will be opened
#' @param package Package called
#' @details This function is an adaptation of the usethis variant
#' for use in the qDatr ecosystem.
#' @return A rendered template, saved into the correct folder
#' @importFrom whisker whisker.render
#' @examples
#' \dontrun{
#' TODO
#' }
#' @export
qtemplate <- function(template,
                      save_as = template,
                      data = list(),
                      ignore = FALSE,
                      path,
                      open = rlang::is_interactive(),
                      package = "qDatr") {
  
  # Set up find_template() helper function
  find_template <- function(template_name, package = "qDatr") {
    path <- tryCatch(fs::path_package(package = package, "templates", template_name),
                     error = function(e) ""
    )
    if (identical(path, "")) {
      usethis::ui_stop(
        "Could not find template {usethis::ui_value(template_name)} \\
      in package {usethis::ui_value(package)}."
      )
    }
    path
  }
  
  # Set up render_template() helper function
  render_template <- function(template, data = list(), package = "qDatr") {
    template_path <- find_template(template, package = package)
    strsplit(whisker::whisker.render(xfun::read_utf8(template_path), data), "\n")[[1]]
  }
  
  # Render and save the template as correct file
  template_contents <- render_template(template, data, package = package)
  new <- usethis::write_over(paste0(path, "/", save_as), template_contents)
  if (ignore) {
    usethis::use_build_ignore(save_as)
  }
  if (open && new) {
    usethis::edit_file(usethis::proj_path(save_as))
  }
  invisible(new)
}
