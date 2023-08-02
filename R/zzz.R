globalCallingHandlers(message = function(condition) {
  msg <- trimws(conditionMessage(condition))
  writeLines(msg)
  invokeRestart("muffleMessage")
  })

.onAttach <- function(lib, pkg) {
  msg <- c(paste0("manydata ", utils::packageVersion("manydata")),
           "\nPlease see ",
           cli::style_hyperlink("manydata.ch", "http://manydata.ch"),
           " for more information.",
           "\nType 'citation(\"manydata\")' for citing this R package in publications.")
  packageStartupMessage(msg)      
  invisible()
}
