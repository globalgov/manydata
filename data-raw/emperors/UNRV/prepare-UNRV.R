# UNRV Preparation Script

# This is a template for importing, cleaning, and exporting data
# ready for the qPackage.
# The dataset was improted from:
# https://www.unrv.com/government/emperor.php
# The dataset was impoted to the package with the following line:
# qCreate::import_data("UNRV", "emperors")
library(qCreate)

# Stage one: Collecting data
UNRV <- readxl::read_excel("data-raw/emperors/UNRV/UNRV.xlsx")

# Stage two: Correcting data
# In this stage you will want to correct the variable names and
# formats of the 'UNRV' object until the object created
# below (in stage three) passes all the tests.
# First, let's just complete year variables so that Beg and End variables
# have at least have 4 digits for years.
for(i in c(1:13)) {
  UNRV$Beg[i] <- paste0("00", UNRV$Beg[i])
}
for(i in c(14:99)) {
  UNRV$Beg[i] <- paste0("0", UNRV$Beg[i])
}
for(i in c(1:12)) {
  UNRV$End[i] <- paste0("00", UNRV$End[i])
}
for(i in c(13:99)) {
  UNRV$End[i] <- paste0("0", UNRV$End[i])
}
# standardise_dates() is a wrapper function for
# messydates::as_messydates() and messydates::make_messydates(). 
# Let's standardise dates and variable names
UNRV <- as_tibble(UNRV) %>%
  dplyr::mutate(Beg = qCreate::standardise_dates(Beg),
                End = qCreate::standardise_dates(End)) %>% 
  dplyr::rename(ID = "Common Name",
         FullName = "Full Name/Imperial Name",
         Dynasty = "Dynasty/Class/Notes")
# qData includes several functions that should help cleaning
# and standardising your data.
# Please see the vignettes or website for more details.

# Stage three: Connecting data
# Next run the following line to make UNRV available
# within the qPackage.
export_data(UNRV, database = "emperors", URL = "https://www.unrv.com/government/emperor.php")
# This function also does two additional things.
# First, it creates a set of tests for this object to ensure adherence
# to certain standards.You can hit Cmd-Shift-T (Mac) or Ctrl-Shift-T (Windows)
# to run these tests locally at any point.
# Any test failures should be pretty self-explanatory and may require
# you to return to stage two and further clean, standardise, or wrangle
# your data into the expected format.
# Second, it also creates a documentation file for you to fill in.
# Please make sure that you cite any sources appropriately and fill in as
# much detail about the variables etc as possible.
