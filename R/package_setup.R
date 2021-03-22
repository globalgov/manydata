#' Create a new package in the qData ecosystem
#'
#' Creates a new package in, and consistent with, the qData ecosystem
#' @param package A string giving the desired name of the package,
#' must start with "q"
#' @param orcid A vector of strings of all the ORCID numbers of the authors.
#' Needs `{rorcid}` package to be installed. Takes precedence over manual
#' entries if specified.
#' @param name A list of vectors giving the package
#' author(s)' name(s). Authors(s)last name(s) and first 
#' name(s) are separated by a comma.
#' @param role A list of vectors of the roles the package authors have
#' in the project. If there are no roles declared,
#' roles are set contributor.
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
#' setup_package("qStates", name = "Hollway, James"))
#' }
#' \dontrun{
#' setup_package("qStates", 
#' orcid = c("0000-0002-8361-9647"))
#' }
#' @export
setup_package <- function(package = NULL,
                          name = NULL,
                          role = NULL,
                          orcid = NULL,
                          update = TRUE,
                          path = getwd()) {
  
  # Checks to see whether inputs are correct
  # usethis:::check_path_is_directory(fs::path_dir(path))
  pname <- fs::path_file(fs::path_abs(path))
  # usethis:::check_not_nested(fs::path_dir(path), name)
  # usethis:::create_directory(path)
  
  # Step zero: get details from existing files, if present
  if (is.null(package)){
    if (file.exists(paste0(path, "/DESCRIPTION"))){
      package <- read.dcf(paste0(path, "/DESCRIPTION"))[[1]]
      usethis::ui_done("Obtained package name from existing DESCRIPTION file.")
      if (!startsWith(package, "q")) stop("Package name must start with a 'q'")
    } else {
      stop("Please declare a package name")
    }
  }
  
  ifelse (!startsWith(package, "q"), stop("Package name must start with a 'q'"), package)
  
  if (is.null(name)){
    if (file.exists(paste0(path, "/DESCRIPTION"))){
      author <- read.dcf(paste0(path, "/DESCRIPTION"))[[4]]
      author <- stringr::str_replace_all(author, "\",\nfamily = \"", " ")
      author <- stringr::str_replace_all(author, "c\\(", "")
      author <- stringr::str_replace_all(author, "person\\(given = \"", "")
      author <- stringr::str_replace_all(author, "\\n", "")
      author <- stringr::str_replace_all(author, "\".*", "")
      given <- stringr::str_split(author, " ")[[1]][[1]]
      family <- stringr::str_split(author, " ")[[1]][[2]]
      comment <- NULL
      usethis::ui_done("Obtained lead author name from existing DESCRIPTION file.")
    } else {
      stop("Please declare one author")
    }
  }
  # Small check to see if roles are defined. If there are
  # no roles declared it sets all roles, but first author declared, 
  # to contributor.
  
  rolefirst <- 'c("aut", "cre", "ctb")'
  
  # Step 0.1 See if there are any ORCID numbers
  if(!is.null(orcid)){
    # Check if rorcid package is installed.
    if("rorcid" %in% rownames(utils::installed.packages()) == FALSE){
      depends("rorcid")
    }
    if(length(orcid)>2){
      stop("Please specify one author. Add the rest by using our
            add_author() function.")
    }
    # Authenticate the user, might be useful to add a stop here.
    rorcid::orcid_auth()
    # orcid <- c("0000-0001-5943-9059", "0000-0003-3420-6085")
    # orcid <- "0000-0001-5943-9059"
    # Get the data from the ORCID API
    personal <- rorcid::orcid_person(orcid)
    employments <- rorcid::orcid_employments(orcid)
    
    # Disentangle the data and get get them into vectors
    given <- as.character(personal[[orcid]][["name"]]
                          [["given-names"]][["value"]])
    family <- as.character(personal[[orcid]][["name"]]
                           [["family-name"]][["value"]])
    comment <- as.character(orcid)
    # Use correct template
    if(length(orcid) == 1 & is.null(role)) {
      qtemplate("qPackage-DESC.dcf",
                "DESCRIPTION",
                data = list(package = package,
                            given = given,
                            family = family,
                            comment = comment,
                            role = rolefirst),
                path = path)
    } else if(length(orcid) == 1 & !is.null(role)) {
      qtemplate("qPackage-DESC.dcf",
                "DESCRIPTION",
                data = list(package = package,
                            given = given,
                            family = family,
                            comment = comment,
                            role = role),
                path = path)
    }
  }
    
  if(!is.null(name)) {
    
    # Treat author names
    fullname <- stringr::str_split(name, ",")
    given <- stringr::str_trim(paste0(fullname[[1]][2]))
    family <- paste0(fullname[[1]][1])

    if(length(fullname)>2){
      stop("Please specify author. Add the rest by using our add_author() function.")
    }
    
    if(length(fullname) == 1 & is.null(role)) {
      qtemplate("qPackage-DESC.dcf",
                "DESCRIPTION",
                data = list(package = package,
                            given = given,
                            family = family,
                            role = rolefirst),
                path = path)
    } else if(length(fullname) == 1 & !is.null(role)) {
      qtemplate("qPackage-DESC.dcf",
                "DESCRIPTION",
                data = list(package = package,
                            given = given,
                            family = family,
                            role = role),
                path = path)
    }
  }
  
  usethis::ui_done("Added DESCRIPTION file. Modify if necessary.")
  usethis::ui_done("Check out our new_author function if you need to add
                   authors down the line")
  
  authors <- as.vector(mapply(stringr::str_c, given, family, sep = " "))
  author <- paste0(authors, collapse = ", ")

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
              data = list(package = package),
              path = path)
    usethis::ui_done("Added starter NEWS file. Update for every release.")
  }
  
  # Add README
  qtemplate("qPackage-README.Rmd",
            "README.Rmd",
            data = list(package = package,
                        author = author),
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
            data = list(package = package,
                        author = author),
            path = path,
            open = FALSE)
  usethis::ui_done("Created CODE_OF_CONDUCT file. Modify if necessary.")
  
  qtemplate("qPackage-CONTRIB.md",
            fs::path(".github", "CONTRIBUTING.md"),
            data = list(package = package,
                        author = author),
            path = path,
            open = FALSE)
  usethis::ui_done("Created CONTRIBUTING file. Modify if necessary.")
  
  qtemplate("qPackage-PR.md",
            fs::path(".github", "pull_request_template.md"),
            data = list(package = package,
                        author = author),
            path = path,
            open = FALSE)
  usethis::ui_done("Created PR template. Modify if necessary.")
  
  create_directory(paste0(path, "/.github/ISSUE_TEMPLATE"))
  usethis::ui_done("Created ISSUE_TEMPLATE folder.")
  
  qtemplate("qPackage-Bugs.md",
            fs::path(".github", "ISSUE_TEMPLATE", "bug_report.md"),
            data = list(package = package,
                        author = author),
            path = path,
            open = FALSE)
  usethis::ui_done("Created bug report issue template. Modify if necessary.")
  
  qtemplate("qPackage-Features.md",
            fs::path(".github", "ISSUE_TEMPLATE", "feature_request.md"),
            data = list(package = package,
                        author = author),
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

}

#' Helper function for adding an author to the current package
#' 
#' Helper function for adding an author to the description file of the current
#' package.
#' @param orcid Character string of the author's ORCID number. If this is null,
#' then the function switches to manual entry.
#' @param  name A vector giving the package
#' author(s)' name(s). Authors(s)last name(s) and first 
#' name(s) are separated by a comma.
#' @param role Character vector of role(s) the author has in the project. 
#' Contributor by default. For example "c(aut, cre, ctb)".
#' @param email Character string of the author's email
#' @param comment Character vector of the author's miscellaneous information 
#' such as his/her institution. 
#' @return Adds a new author to the description file of the package
#' @details This function adds an author to the description file of the current
#' package. This can be done in two ways. First you can specify the ORCID number
#' of the author you want to add. This will leverage the excellent `rorcid` 
#' package and scrape the information from the ORCID API and fill out the 
#' description file automatically. Second, you can specify the arguments 
#' manually if the author does not have an ORCID number. Finally, note that by
#' default the role of the new author is set to contributor.
#' @examples
#' \dontrun{
#' add_author(orcid = "0000-0002-8361-9647", role = list(c("aut", "cre", "ctb"))
#' add_author(name = "Smith, John",
#' comment = "University of Somewhere")
#' }
#' @export
add_author <- function(orcid = NULL,
                       name = NULL,
                       role = NULL,
                       email = NULL,
                       affiliation = NULL){
  
  # Check for correct input
  if (is.null(orcid) & is.null(name)) stop("Either a correct ORCID number or name in the format 'Surname, Given Names' must be provided.")
  
  # Use ORCID data if available
  if(!is.null(orcid)){
    if(!stringr::str_detect(orcid, "^[0-9]")){
      name <- orcid # in case user accidentally puts name in first argument
    } else {
      # Check whether rorcid is installed, if not install it
      if(!"rorcid" %in% rownames(utils::installed.packages())){
        depends("rorcid")
      }
      #Step 1: Authenticate with ORCID
      rorcid::orcid_auth()
      #Step 2: Extract the information from ORCID API (lists)
      author <- rorcid::orcid_person(orcid)
      employment <- rorcid::orcid_employments(orcid)
      if(is.null(affiliation)){
        affiliation <- c(as.character(employment[[orcid]]
                                      [["affiliation-group"]][["summaries"]]
                                      [[1]][["employment-summary.organization.name"]]))
      }
      given <- as.character(author[[orcid]][["name"]]
                            [["given-names"]][["value"]])
      family <- as.character(author[[orcid]][["name"]]
                             [["family-name"]][["value"]])
      if(is.null(email) & length(author[[orcid]][["emails"]][["email"]]) != 0){
        email <- as.character(author[[orcid]][["emails"]][["email"]][[1]])
      }
    }
  }
  
  if (!is.null(name)) {
    name <- stringr::str_split(name, ", ")
    family <- name[[1]][1]
    given <- name[[1]][2]
  }
  
  # Unless otherwise provided, new authors added are listed as 'contributors'
  if(is.null(role)) role <- "ctb"
  
  if(!is.null(email) && !grepl("@", email, fixed = TRUE)){
    stop("Please specify a correct email adress.")
  }
  
  if(!"desc" %in% rownames(utils::installed.packages())){
    depends("desc")
  }
  
  # Write the new author to the description
  # TODO: check whether author already exists and update details instead
  desc::desc_add_author(given = given,
                        family = family,
                        role = role,
                        email = email,
                        comment = c(affiliation = affiliation, ORCID = orcid))
}
