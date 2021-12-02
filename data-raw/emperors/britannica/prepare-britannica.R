# britannica Preparation Script

# This is a template for importing, cleaning, and exporting data
# ready for the qPackage.
# The dataset was improted from:
# https://www.britannica.com/topic/list-of-Roman-emperors-2043294
# The dataset was impoted to the package with the following line:
# manypkgs::import_data("britannica", "emperors")
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
# First, let's just complete year variables so that they
# at least have 4 digits.
for(i in c(1:6, 9:13)) {
  britannica$reign_start[i] <- paste0("00", britannica$reign_start[i])
}
for(i in c(14:18, 21:27,32:43, 45:75, 77:80, 82:87)) {
  britannica$reign_start[i] <- paste0("0", britannica$reign_start[i])
}
for(i in c(1:6, 9:12)) {
  britannica$reign_end[i] <- paste0("00", britannica$reign_end[i])
}
for(i in c(13:18, 21:27, 32:43, 45:75, 77:80, 82:87)) {
  britannica$reign_end[i] <- paste0("0", britannica$reign_end[i])
}
# Now, some months are speeled thrghout and years
# are in the inverse order.
# While some other dates are precise to the day.
# Let's adress these issues
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
britannica$reign_end[76] <- "0455-05-32"
britannica$reign_end[81] <- "0472-11"
# Let's also just make sure BC and CE dates are reposrted consistently.
britannica$reign_start <- gsub("BCE", "BC", britannica$reign_start)
britannica$reign_end <- gsub("CE", "", britannica$reign_end)
# standardise_dates() is a wrapper function for
# messydates::as_messydates() and messydates::make_messydates(). 
# Let's standardise dates and variable names
britannica <- as_tibble(britannica) %>%
  manydata::transmutate(ID = Name,
                     Beg = manypkgs::standardise_dates(reign_start),
                     End = manypkgs::standardise_dates(reign_end)) %>%
  dplyr::relocate(ID, Beg, End)
# manydata includes several functions that should help cleaning
# and standardising your data.
# Please see the vignettes or website for more details.

# Stage three: Connecting data
# Next run the following line to make britannica available
# within the qPackage.
manypkgs::export_data(britannica, database = "emperors",
                      URL = "https://www.britannica.com/topic/list-of-Roman-emperors-2043294")
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
