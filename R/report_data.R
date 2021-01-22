#' @param pkg character string of the qPackage to report data on
#' @export
report_data <- function(pkg){
  pkg_path <- find.package(pkg)
  data_path <- file.path(pkg_path, "data")
  pkg_dbs <- unname(unlist(readRDS(file.path(data_path, "Rdata.rds"))))
  tmp_env <- new.env()
  lazyLoad(file.path(data_path, "Rdata"), envir = tmp_env)
  dbs <- get(pkg_dbs, envir = tmp_env)
  bind_cols(purrr::map(dbs, function(x) length(unique(x$ID))))
}
