# UNRV Preparation Script

# The dataset was imported from:
# https://www.unrv.com/government/emperor.php

# Stage one: Collecting data
UNRV <- readxl::read_excel("data-raw/emperors/UNRV/UNRV.xlsx")

# Stage two: Correcting data
# In this stage you will want to correct the variable names and
# formats of the 'UNRV' object until the object created
# below (in stage three) passes all the tests.
# Remove non-ASCII characters
UNRV <- apply(UNRV, 2, stringi::stri_enc_toascii)
# Let's standardise dates and variable names
UNRV <- tibble::as_tibble(UNRV) %>%
  dplyr::mutate(Begin = messydates::as_messydate(Beg),
                End = messydates::as_messydate(End)) %>%
  dplyr::rename(ID = "Common Name",
         FullName = "Full Name/Imperial Name",
         Dynasty = "Dynasty/Class/Notes") %>%
  dplyr::relocate(ID, Begin, End)
# manydata includes several functions that should help cleaning
# and standardising your data.
# Please see the vignettes or website for more details.

# Stage three: Connecting data
# Next run the following line to make UNRV available
# within the qPackage.
manypkgs::export_data(UNRV, database = "emperors", URL = "https://www.unrv.com/government/emperor.php")
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
