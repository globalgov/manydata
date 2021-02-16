#' Create a new package in the qData ecosystem
#'
#' Creates a new package in, and consistent with, the qData ecosystem
#' @param packageName A string giving the desired name of the package,
#' must start with "q"
#' @param ORCID A vector of strings of all the ORCID numbers of the authors.
#' Needs `{rorcid}` package to be installed. Takes precedence over manual
#' entries if specified.
#' @param packageAuthorName A vector giving the package
#' author(s)' name(s)
#' @param packageAuthorSurname A vector giving the package
#' author(s)' surname(s)
#' @param packageAuthorInstitution A vector giving the package
#' author(s)' institution (only one per author)
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
#' setup_package("qStates", "James Hollway, Henrique Sposito")
#' }
#' @export
setup_package <- function(packageName = NULL,
                          packageAuthorName = NULL,
                          packageAuthorSurname = NULL,
                          packageAuthorInstitution = NULL,
                          ORCID = NULL,
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
  
  if (is.null(packageAuthorName) || is.null(packageAuthorSurname)){
    if (file.exists(paste0(path, "/DESCRIPTION"))){
      packageAuthor <- read.dcf(paste0(path, "/DESCRIPTION"))[[4]]
      packageAuthor <- stringr::str_replace_all(packageAuthor, "\",\nfamily = \"", " ")
      packageAuthor <- stringr::str_replace_all(packageAuthor, "c\\(", "")
      packageAuthor <- stringr::str_replace_all(packageAuthor, "person\\(given = \"", "")
      packageAuthor <- stringr::str_replace_all(packageAuthor, "\\n", "")
      packageAuthor <- stringr::str_replace_all(packageAuthor, "\".*", "")
      given1 <- stringr::str_split(packageAuthor, " ")[[1]][[1]]
      family1 <- stringr::str_split(packageAuthor, " ")[[1]][[2]]
      comment1 <- NULL
      usethis::ui_done("Obtained lead author name from existing DESCRIPTION file.")
    } else {
      stop("Please declare at least one author")
    }
  }
  # Step 0.1 See if there are any ORCID numbers
  if(!is.null(ORCID)){
    # Check if rorcid package is installed.
    if("rorcid" %in% rownames(installed.packages()) == FALSE){
      stop("Please install the rorcid package before proceeding.")
    }
    if(length(ORCID)>5){
      stop("Please specify a maximum of 5 authors. Add the rest by using our
            add_author() function.")
    }
    # Authenticate the user, might be useful to add a stop here.
    rorcid::orcid_auth()
    # Get the data from the ORCID API
    personal <- rorcid::orcid_person(ORCID)
    employments <- rorcid::orcid_employments(ORCID)
    # Initializing vectors
    givenv <- c()
    familyv <- c()
    emailv <- c()
    commentv <- c()
    # Disentangle the data and get get them into vectors
    for (i in c(1:length(ORCID))) {
      givenv <- append(givenv, as.character(personal[[i]][["name"]]
                                         [["given-names"]][["value"]]))
      familyv <- append(familyv, as.character(personal[[i]][["name"]]
                                 [["family-name"]][["value"]]))
      # if(length(personal[[i]][["emails"]][["email"]]) != 0){
      #   append(emailv, as.character(personal[[i]][["emails"]][["email"]][[1]]))
      # }else{
      #   append(emailv, email)
      # } Wise to add it ? Or let authors modify by hand ?
      commentv <- append(commentv, as.character(employments[[i]]
                                [["affiliation-group"]][["summaries"]]
                                [[1]]
                                [["employment-summary.organization.name"]]))
      assign(paste0("given", i), givenv[[i]])
      assign(paste0("family", i), familyv[[i]])
      assign(paste0("comment", i), commentv[[i]])
    }
    # Use correct template
    if(length(ORCID) == 1){
      qtemplate("qPackage-DESC1.dcf",
                "DESCRIPTION",
                data = list(package = packageName,
                            given1 = given1,
                            family1 = family1,
                            comment1 = comment1),
                path = path)
    }
    if(length(ORCID) == 2){
      qtemplate("qPackage-DESC2.dcf",
                "DESCRIPTION",
                data = list(package = packageName,
                            given1 = given1,
                            family1 = family1,
                            comment1 = comment1,
                            given2 = given2,
                            family2 = family2,
                            comment2 = comment2),
                path = path)
    }
    if(length(ORCID) == 3){
      qtemplate("qPackage-DESC3.dcf",
                "DESCRIPTION",
                data = list(package = packageName,
                            given1 = given1,
                            family1 = family1,
                            comment1 = comment1,
                            given2 = given2,
                            family2 = family2,
                            comment2 = comment2,
                            given3 = given3,
                            family3 = family3,
                            comment3 = comment3),
                path = path)
    }
    if(length(ORCID) == 4){
      qtemplate("qPackage-DESC4.dcf",
                "DESCRIPTION",
                data = list(package = packageName,
                            given1 = given1,
                            family1 = family1,
                            comment1 = comment1,
                            given2 = given2,
                            family2 = family2,
                            comment2 = comment2,
                            given3 = given3,
                            family3 = family3,
                            comment3 = comment3,
                            given4 = given4,
                            family4 = family4,
                            comment4 = comment4),
                path = path)
    }
    if(length(ORCID) == 5){
      qtemplate("qPackage-DESC5.dcf",
                "DESCRIPTION",
                data = list(package = packageName,
                            given1 = given1,
                            family1 = family1,
                            comment1 = comment1,
                            given2 = given2,
                            family2 = family2,
                            comment2 = comment2,
                            given3 = given3,
                            family3 = family3,
                            comment3 = comment3,
                            given4 = given4,
                            family4 = family4,
                            comment4 = comment4,
                            given5 = given5,
                            family5 = family5,
                            comment5 = comment5),
                path = path)
    }
  } else {
    # Step one: ensure/create package/project structure
    # Add DESCRIPTION
    # Test that lengths are equal for name and surname vector
    if(length(packageAuthorSurname) != length(packageAuthorSurname)){
      stop("The number of author names you entered does not match 
           the number of surnames")
    }
    if(length(packageAuthorName)>5){
      stop("Please specify a maximum of 5 authors. Add the rest by using our
            add_author() function.")
    }
    if(length(packageAuthorName) == 1){
      qtemplate("qPackage-DESC1.dcf",
                "DESCRIPTION",
                data = list(package = packageName,
                            given1 = packageAuthorName[[1]],
                            family1 = packageAuthorSurname[[1]],
                            comment1 = packageAuthorInstitution[[1]]),
                path = path)
    }
    if(length(packageAuthorName) == 2){
      qtemplate("qPackage-DESC2.dcf",
                "DESCRIPTION",
                data = list(package = packageName,
                            given1 = packageAuthorName[[1]],
                            family1 = packageAuthorSurname[[1]],
                            comment1 = packageAuthorInstitution[[1]],
                            given2 = packageAuthorName[[2]],
                            family2 = packageAuthorSurname[[2]],
                            comment2 = packageAuthorInstitution[[2]]),
                path = path)
    }
    if(length(packageAuthorName) == 3){
      qtemplate("qPackage-DESC3.dcf",
                "DESCRIPTION",
                data = list(package = packageName,
                            given1 = packageAuthorName[[1]],
                            family1 = packageAuthorSurname[[1]],
                            comment1 = packageAuthorInstitution[[1]],
                            given2 = packageAuthorName[[2]],
                            family2 = packageAuthorSurname[[2]],
                            comment2 = packageAuthorInstitution[[2]],
                            given3 = packageAuthorName[[3]],
                            family3 = packageAuthorSurname[[3]],
                            comment3 = packageAuthorInstitution[[3]]),
                path = path)
    }
    if(length(packageAuthorName) == 4){
      qtemplate("qPackage-DESC4.dcf",
                "DESCRIPTION",
                data = list(package = packageName,
                            given1 = packageAuthorName[[1]],
                            family1 = packageAuthorSurname[[1]],
                            comment1 = packageAuthorInstitution[[1]],
                            given2 = packageAuthorName[[2]],
                            family2 = packageAuthorSurname[[2]],
                            comment2 = packageAuthorInstitution[[2]],
                            given3 = packageAuthorName[[3]],
                            family3 = packageAuthorSurname[[3]],
                            comment3 = packageAuthorInstitution[[3]],
                            given4 = packageAuthorName[[4]],
                            family4 = packageAuthorSurname[[4]],
                            comment4 = packageAuthorInstitution[[4]]),
                path = path)
    }
    if(length(packageAuthorName) == 5){
      qtemplate("qPackage-DESC5.dcf",
                "DESCRIPTION",
                data = list(package = packageName,
                            given1 = packageAuthorName[[1]],
                            family1 = packageAuthorSurname[[1]],
                            comment1 = packageAuthorInstitution[[1]],
                            given2 = packageAuthorName[[2]],
                            family2 = packageAuthorSurname[[2]],
                            comment2 = packageAuthorInstitution[[2]],
                            given3 = packageAuthorName[[3]],
                            family3 = packageAuthorSurname[[3]],
                            comment3 = packageAuthorInstitution[[3]],
                            given4 = packageAuthorName[[4]],
                            family4 = packageAuthorSurname[[4]],
                            comment4 = packageAuthorInstitution[[4]],
                            given5 = packageAuthorName[[5]],
                            family5 = packageAuthorSurname[[5]],
                            comment5 = packageAuthorInstitution[[5]]),
                path = path)
    }
  }
  
  usethis::ui_done("Added DESCRIPTION file. Modify if necessary.")
  usethis::ui_done("Check out our new_author function if you need to add
                   authors down the line")
  # Add R folder
  create_directory(paste0(path, "/R"))
  usethis::ui_done("Created R/ folder. Here is where any scripts go.")
  # Add NAMESPACE
  usethis::use_namespace()
  usethis::ui_done("Created NAMESPACE file. Don't modify it.")
  # Add LICENSE
  qtemplate("LICENSE.md",
            ignore = TRUE,
            path = path,
            open = FALSE)
  usethis::ui_done("Added CC BY 4.0 license.")
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
