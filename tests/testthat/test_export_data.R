# transp <- data.frame (bikes = c(5), skates = c(4))
# 
# create_local_package <- function(dir = fs::file_temp(), env = parent.frame()) {
#   old_project <- usethis:::proj_get_()
#   
#   # create new folder and package
#   setup_package(dir, "hs") # A
#   # TODO: address error that user input is required, but session is not interactive...
#   withr::defer(fs::dir_delete(dir), envir = env) # -A
#   usethis::ui_silence(open = FALSE, check_name = FALSE)
#   
#   # change working directory
#   setwd(dir) # B
#   withr::defer(setwd(old_project), envir = env) # -B
#   
#   # switch to new usethis project
#   usethis::proj_set(dir) # C
#   withr::defer(usethis::proj_set(old_project, force = TRUE), envir = env) # -C
#   
#   invisible(dir)
# }
# 
# test_that("export_data() stores create data folder and tests folder", {
#    create_local_package()
#    export_data("transp")
#    usethis:::expect_proj_file(path("data"))
#    usethis:::expect_proj_file(path("tests", "testhat"))
# })
# 
# # Here the issue appears to be related to the fact that the create_local_package() function
# # is not currently working. 
# 
# # TODO: test that missing obsevartions are standard
# 
# # TODO: test that data exported is in tibble format
# # test_that("exported data is in tibble format") {
# #   export_data("transp")
# #   expect_message(tibble::is.tibble("transp"), "TRUE")
# # }