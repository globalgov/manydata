# britannica Preparation Script

# This is a template for importing, cleaning, and exporting data
# ready for the qPackage.
# The dataset was improted from:
# https://www.britannica.com/topic/list-of-Roman-emperors-2043294
# The dataset was impoted to the package with the following line:
# qCreate::import_data("britannica", "emperors")

library(qCreate)

# Stage one: Collecting data
britannica <- readxl::read_excel("data-raw/emperors/britannica/britannica.xlsx")

# Stage two: Correcting data
# In this stage you will want to correct the variable names and
# formats of the 'britannica' object until the object created
# below (in stage three) passes all the tests.
britannica <- as_tibble(britannica) %>%
  transmutate(ID = {id_variable_name_here},
              Beg = standardise_dates({date_variable_name_here})) %>%
  dplyr::arrange(Beg)
# qData includes several functions that should help cleaning
# and standardising your data.
# Please see the vignettes or website for more details.

# Stage three: Connecting data
# Next run the following line to make britannica available
# within the qPackage.
export_data(britannica, database = "emperors")
# This function also does two additional things.
# First, it creates a set of tests for this object to ensure adherence
# to certain standards.You can hit Cmd-Shift-T (Mac) or Ctrl-Shift-T (Windows)
# to run these tests locally at any point.
# Any test failures should be pretty self-explanatory and may require
# you to return to stage two and further clean, standardise, or wrangle
# your data into the expected format.
# Second, it also creates a documentation file for you to fill in.
# Please make sure that you cite any sources appropriately and fill in as
# much detail about the variables etc as possible.
