.onAttach <- function(lib, pkg) {
  msg <- c(paste0("manydata ", utils::packageVersion("manydata")),
           "\nFor more information about the package please visit https://manydata.ch",
           "\nType 'citation(\"manydata\")' for citing this R package in publications.")
  packageStartupMessage(msg)      
  invisible()
}
