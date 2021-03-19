#' Create a new package in the qData ecosystem
#'
#' Creates a new package in, and consistent with, the qData ecosystem
#' @param packageName A string giving the desired name of the package,
#' must start with "q"
#' @param ORCID A vector of strings of all the ORCID numbers of the authors.
#' Needs `{rorcid}` package to be installed. Takes precedence over manual
#' entries if specified.
#' @param AuthorName A vector giving the package
#' author(s)' name(s). Authors(s)last name(s) and first 
#' name(s) are separated by a comma.
#' @param Role A list of vectors of the roles the package authors have
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
#' setup_package("qStates", AuthorName = c("Hollway, James", "Sposito, Henrique"),
#' Role = list(c("aut", "cre", "ctb"), "ctb")
#' }
#' \dontrun{
#' setup_package("qStates", 
#' ORCID = c("0000-0002-8361-9647", "0000-0003-3420-6085"), Role = c("aut", "ctb"))
#' }
#' @export
setup_package <- function(packageName = NULL,
                          AuthorName = NULL,
                          Role = NULL,
                          ORCID = NULL,
                          update = TRUE,
                          path = getwd()) {
  
  # Initialize variables to suppress an annoying note when running 
  # devtools_check
  given1 <- given2 <- given3 <- given4 <- given5 <- NULL
  family1 <- family2 <- family3 <- family4 <- family5 <- NULL
  role1 <- role2 <- role3 <- role4 <- role5 <- NULL
  comment1 <- comment2 <- comment3 <- comment4 <- comment5 <- NULL
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
  
  ifelse (!startsWith(packageName, "q"), stop("Package name must start with a 'q'"), packageName())
  
  if (is.null(AuthorName)){
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
  # Small check to see if roles are defined. If there are
  # no roles declared sets roles to it sets roles to contributor.
  if(is.null(Role) || length(Role) != length(AuthorName)){
    if(length(AuthorName)==1) {
      Role = "ctb"
    } else if(length(AuthorName)==2) {
      Role = c("ctb", "ctb")
    } else if(length(AuthorName)==3) {
      Role = c("ctb", "ctb", "ctb")
    } else if(length(AuthorName)==4) {
      Role = c("ctb", "ctb", "ctb", "ctb")
    } else if(length(AuthorName)==5) {
      Role = c("ctb", "ctb", "ctb", "ctb", "ctb")
    }
  }
  # Step 0.1 See if there are any ORCID numbers
  if(!is.null(ORCID)){
    # Check if rorcid package is installed.
    if("rorcid" %in% rownames(utils::installed.packages()) == FALSE){
      stop("Please install the rorcid package before proceeding.")
    }
    if(length(ORCID)>5){
      stop("Please specify a maximum of 5 authors. Add the rest by using our
            add_author() function.")
    }
    # Authenticate the user, might be useful to add a stop here.
    rorcid::orcid_auth()
    # ORCID <- c("0000-0001-5943-9059", "0000-0003-3420-6085")
    # ORCID <- "0000-0001-5943-9059"
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
      commentv <- append(commentv, ORCID[i]) 
      assign(paste0("given", i), givenv[[i]])
      assign(paste0("family", i), familyv[[i]])
      assign(paste0("comment", i), commentv[[i]])
      assign(paste0("role", i), dput(Role[[i]]))
      if(length(get(paste0("role", i))) == 1){
        assign(paste0("role", i), paste('"', Role[i], '"', sep = ""))
    }}
    # Use correct template
    if(length(ORCID) == 1){
      qtemplate("qPackage-DESC1.dcf",
                "DESCRIPTION",
                data = list(package = packageName,
                            given1 = given1,
                            family1 = family1,
                            comment1 = comment1,
                            role1 = role1),
                path = path)
    }
    if(length(ORCID) == 2){
      qtemplate("qPackage-DESC2.dcf",
                "DESCRIPTION",
                data = list(package = packageName,
                            given1 = given1,
                            family1 = family1,
                            comment1 = comment1,
                            role1 = role1,
                            given2 = given2,
                            family2 = family2,
                            comment2 = comment2,
                            role2 = role2),
                path = path)
    }
    if(length(ORCID) == 3){
      qtemplate("qPackage-DESC3.dcf",
                "DESCRIPTION",
                data = list(package = packageName,
                            given1 = given1,
                            family1 = family1,
                            comment1 = comment1,
                            role1 = role1,
                            given2 = given2,
                            family2 = family2,
                            comment2 = comment2,
                            role2 = role2,
                            given3 = given3,
                            family3 = family3,
                            comment3 = comment3,
                            role3 = role3),
                path = path)
    }
    if(length(ORCID) == 4){
      qtemplate("qPackage-DESC4.dcf",
                "DESCRIPTION",
                data = list(package = packageName,
                            given1 = given1,
                            family1 = family1,
                            comment1 = comment1,
                            role1 = role1,
                            given2 = given2,
                            family2 = family2,
                            comment2 = comment2,
                            role2 = role2,
                            given3 = given3,
                            family3 = family3,
                            comment3 = comment3,
                            role3 = role3,
                            given4 = given4,
                            family4 = family4,
                            comment4 = comment4,
                            role4 = role4),
                path = path)
    }
    if(length(ORCID) == 5){
      qtemplate("qPackage-DESC5.dcf",
                "DESCRIPTION",
                data = list(package = packageName,
                            given1 = given1,
                            family1 = family1,
                            comment1 = comment1,
                            role1 = role1,
                            given2 = given2,
                            family2 = family2,
                            comment2 = comment2,
                            role2 = role2,
                            given3 = given3,
                            family3 = family3,
                            comment3 = comment3,
                            role3 = role3,
                            given4 = given4,
                            family4 = family4,
                            comment4 = comment4,
                            role4 = role4,
                            given5 = given5,
                            family5 = family5,
                            comment5 = comment5,
                            role5 = role5),
                path = path)
    }
  } else {
    # Step one: ensure/create package/project structure
    # Add DESCRIPTION
    # Treat author names
    fullname <- stringr::str_split(AuthorName, ",")

    if(length(fullname)>5){
      stop("Please specify a maximum of 5 authors. Add the rest by using our add_author() function.")
    }
    for (i in c(1:length(fullname))){
      assign(paste0("role", i), dput(Role[i]))
      if(length(get(paste0("role", i))) == 1){
        assign(paste0("role", i), paste('"', Role[i], '"', sep = ""))
      }
    }
    
    if(length(fullname) == 1) {
      given1 <- paste0(fullname[[1]][2])
      family1 <- paste0(fullname[[1]][1])
      qtemplate("qPackage-DESC1.dcf",
                "DESCRIPTION",
                data = list(package = packageName,
                            given1 = given1,
                            family1 = family1,
                            role1 = role1),
                path = path)
      
    } 
    
    if(length(fullname) == 2) {
      given1 <- paste0(fullname[[1]][2])
      family1 <- paste0(fullname[[1]][1])
      given2 <-  paste0(fullname[[2]][2])
      family2 <- paste0(fullname[[2]][1])
      qtemplate("qPackage-DESC2.dcf",
                "DESCRIPTION",
                data = list(package = packageName,
                            given1 = given1,
                            family1 = family1,
                            role1 = role1,
                            given2 = given2,
                            family2 = family2,
                            role2 = role2),
                path = path)
    } 
    
    if (length(fullname) == 3) {
      given1 <- paste0(fullname[[1]][2])
      family1 <- paste0(fullname[[1]][1])
      given2 <-  paste0(fullname[[2]][2])
      family2 <- paste0(fullname[[2]][1])
      given3 <- paste0(fullname[[3]][2])
      family3 <- paste0(fullname[[3]][1])
      qtemplate("qPackage-DESC3.dcf",
                "DESCRIPTION",
                data = list(package = packageName,
                            given1 = given1,
                            family1 = family1,
                            role1 = role1,
                            given2 = given2,
                            family2 = family2,
                            role2 = role2,
                            given3 = given3,
                            family3 = family3,
                            role3 = role3),
                path = path)
    } 
    
 
    if (length(fullname) == 4) {
      given1 <- paste0(fullname[[1]][2])
      family1 <- paste0(fullname[[1]][1])
      given2 <-  paste0(fullname[[2]][2])
      family2 <- paste0(fullname[[2]][1])
      given3 <- paste0(fullname[[3]][2])
      family3 <- paste0(fullname[[3]][1])
      given4 <- paste0(fullname[[4]][2])
      family4 <- paste0(fullname[[4]][1])
      qtemplate("qPackage-DESC4.dcf",
                "DESCRIPTION",
                data = list(package = packageName,
                            given1 = given1,
                            family1 = family1,
                            role1 = role1,
                            given2 = given2,
                            family2 = family2,
                            role2 = role2,
                            given3 = given3,
                            family3 = family3,
                            role3 = role3,
                            given4 = given4,
                            family4 = given4,
                            role4 = role4),
                path = path)
    }
    if(length(AuthorName) == 5) {
      given1 <- paste0(fullname[[1]][2])
      family1 <- paste0(fullname[[1]][1])
      given2 <-  paste0(fullname[[2]][2])
      family2 <- paste0(fullname[[2]][1])
      given3 <- paste0(fullname[[3]][2])
      family3 <- paste0(fullname[[3]][1])
      given4 <- paste0(fullname[[4]][2])
      family4 <- paste0(fullname[[4]][1])
      given5 <- paste0(fullname[[5]][2])
      family5 <- paste0(fullname[[5]][1])
      qtemplate("qPackage-DESC5.dcf",
                "DESCRIPTION",
                data = list(package = packageName,
                            given1 = given1,
                            family1 = family1,
                            role1 = role1,
                            given2 = given2,
                            family2 = family2,
                            role2 = role2,
                            given3 = given3,
                            family3 = family3,
                            role3 = role3,
                            given4 = given4,
                            family4 = given4,
                            role4 = role4,
                            given5 = given5,
                            family5 = family5,
                            role5 = role5),
                path = path)
    }
  }
  
  # Little helper combining author name and surname to form a packageAuthor 
  # variable for the rest of the functions.
  if(!is.null(ORCID)){
    authors <- as.vector(mapply(stringr::str_c, givenv, familyv, sep = " "))
    packageAuthor <- paste0(authors, collapse = ", ")
  } else {
    authors <- as.vector(mapply(stringr::str_c, AuthorName, sep = " "))
    packageAuthor <- paste0(authors, collapse = ", ")
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
