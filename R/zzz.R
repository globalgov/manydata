.onAttach <- function(lib, pkg) {
  msg <- c(paste0("manydata ", utils::packageVersion("manydata")),
           "\nPlease see ",
           cli::style_hyperlink("manydata.ch", "http://manydata.ch"),
           " for more information.",
           "\nType 'citation(\"manydata\")' for citing this R package in publications.")
  packageStartupMessage(msg)      
  invisible()
}

# defining global variables more centrally
utils::globalVariables(c("Dataset","Source","URL","Mapping",":="))

#' @importFrom dplyr %>%
#' @export
dplyr::`%>%`

# Helper function for checking and downloading packages
thisRequires <- function(pkgname){
  if (!requireNamespace(pkgname, quietly = TRUE)) {
    if(utils::askYesNo(msg = paste("The", pkgname, 
                                   "package is required to run this function. Would you like to install", pkgname, "from CRAN?"))) {
      utils::install.packages(pkgname)
    } else {
      stop(paste("Please install", pkgname, "from CRAN to run this function."))
    }
  }
}


