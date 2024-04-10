# britannica Preparation Script

# The dataset was imported from:
# https://www.britannica.com/topic/list-of-Roman-emperors-2043294
# Stage one: Collecting data
britannica <- readxl::read_excel("data-raw/emperors/britannica/britannica.xlsx")
# Stage two: Correcting data
# In this stage you will want to correct the variable names and
# formats of the 'britannica' object until the object created
# below (in stage three) passes all the tests.
# The main issue with the dataset lies on the inconsistencies
# reign_start and reign_end date variables.
# Some dates have months, while other dates only have year.
# Let's just fix a few of these issues.
# Now, some months are and years
# are in the inverse order.
# While some other dates are precise to the day.
# Let's address these issues
britannica$reign_start[7] <- "0069-01"
britannica$reign_start[8] <- "0069-07"
britannica$reign_start[19] <- "0193-01"
britannica$reign_start[20] <- "0193-03"
britannica$reign_start[28] <- "0238-03"
britannica$reign_start[29] <- "0238-03"
britannica$reign_start[30] <- "0238-04-22"
britannica$reign_start[31] <- "0238-04-22"
britannica$reign_start[44] <- "0276-06"
britannica$reign_start[76] <- "0455-03-17"
britannica$reign_start[81] <- "0472-04"
britannica$reign_end[7] <- "0069-04"
britannica$reign_end[8] <- "0069-12"
britannica$reign_end[19] <- "0193-03"
britannica$reign_end[20] <- "0193-06"
britannica$reign_end[28] <- "0238-04"
britannica$reign_end[29] <- "0238-04"
britannica$reign_end[30] <- "0238-07-29"
britannica$reign_end[31] <- "0238-07-29"
britannica$reign_end[44] <- "0276-09"
britannica$reign_end[76] <- "0455-05-31"
britannica$reign_end[81] <- "0472-11"
# Replace some unicode characters
britannica$reign_end[1] <- "14 CE"
# Let's standardise dates and variable names
britannica <- as_tibble(britannica) %>%
  manydata::transmutate(ID = Name,
                        Begin = messydates::as_messydate(reign_start),
                        End = messydates::as_messydate(reign_end)) %>%
  dplyr::relocate(ID, Begin, End)
# manydata includes several functions that should help cleaning
# and standardising your data.
# Please see the vignettes or website for more details.
# Stage three: Connecting data
# Next run the following line to make britannica available
# within the qPackage.
manypkgs::export_data(britannica, datacube = "emperors",
                      URL = "https://www.britannica.com/topic/list-of-Roman-emperors-2043294")
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
