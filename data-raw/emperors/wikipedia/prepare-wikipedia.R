# wikipedia Preparation Script

# This is a template for importing, cleaning, and exporting data
# ready for the qPackage.
# The dataset was improted from:
# https://github.com/zonination/emperors
# It was assembled from the following wikipedia page:
# https://en.wikipedia.org/wiki/List_of_Roman_emperors
# The dataset was imported to the package with the following line:
# qCreate::import_data(dataset = "wikipedia", database = "emperors")
# This also created and opened this preparation script.
library(qCreate)

# Stage one: Collecting data
wikipedia <- readr::read_csv("data-raw/emperors/wikipedia/emperors.csv")

# Stage two: Correcting data
# In this stage you will want to correct the variable names and
# formats of the 'Wikipedia' object until the object created
# below (in stage three) passes all the tests.

# First, the dataset contains a notes column about some errors in dates.
wikipedia$notes
# Some of the first few dates are supposed to be negative (BC).
# While for other dates, only the year is accurate...
# However, due to issues in dealing with dates with most R packages,
# these dates became positive or were completed arbitrarly.
# Let's change these back and treat dates with `{messydates}`.

# Get these columns into character to be able to modify them.
wikipedia$birth <- as.character(wikipedia$birth)
wikipedia$death <- as.character(wikipedia$death)
wikipedia$reign.start <- as.character(wikipedia$reign.start)
wikipedia$reign.end <- as.character(wikipedia$reign.end)
# Now let's correct ngative columns, according to the notes,
# by adding a BC to them.
for(i in c(1, 2, 4, 6)) {
  wikipedia$birth[i] <- paste0(wikipedia$birth[i], " BC")
}
wikipedia$reign.start[1] <- paste0(wikipedia$reign.start[1], " BC")
# Some dates are estimates, while others only the year is correct,
# according to the notes.
# How could we be more specific about that?
# Let's look into how `{messydates}` indicates uncertainty
library(messydates)
?messydates::class
# We should assign a "~" to dates which are estimates.
for(i in c(25, 28, 29, 30, 31, 51, 55, 63, 65, 67)) {
  # birth is estimate
  wikipedia$birth[i] <-  paste0(wikipedia$birth[i], "~") 
}
# death is estimate
wikipedia$death[61] <-  paste0(wikipedia$death[61], "~") 
# reign end is estimate
wikipedia$reign.end[33] <-  paste0(wikipedia$reign.end[33], "~")
for(i in c(50, 57, 58)) { # birth and death are estimates
  wikipedia$birth[i] <-  paste0(wikipedia$birth[i], "~")
  wikipedia$death[i] <-  paste0(wikipedia$death[i], "~")
}
for(i in c(17, 56)) { # death and reign end are estimates
  wikipedia$death[i] <-  paste0(wikipedia$death[i], "~")
  wikipedia$reign.end[i] <-  paste0(wikipedia$reign.end[i], "~") 
}
for(i in 62){ 
  # birth and reign start are estimates
  wikipedia$birth[i] <-  paste0(wikipedia$birth[i], "~")
  wikipedia$reign.start[i] <-  paste0(wikipedia$reign.start[i], "~")
}
for(i in c(27, 43, 52)) {
  # birth, death and reign end estimates
  wikipedia$birth[i] <-  paste0(wikipedia$birth[i], "~")
  wikipedia$death[i] <-  paste0(wikipedia$death[i], "~")
  wikipedia$reign.end[i] <-  paste0(wikipedia$reign.end[i], "~")
}
for(i in c(44, 47, 48)){ 
  # death and reign start and reign end estimates
  wikipedia$death[i] <-  paste0(wikipedia$death[i], "~")
  wikipedia$reign.end[i] <-  paste0(wikipedia$reign.end[i], "~")
  wikipedia$reign.start[i] <-  paste0(wikipedia$reign.start[i], "~")
}
for(i in c(34, 35, 36, 37, 38, 39, 40, 41, 45, 46, 60)){ 
  # birth, death and reign start and reign end estimates
  wikipedia$birth[i] <-  paste0(wikipedia$birth[i], "~")
  wikipedia$death[i] <-  paste0(wikipedia$death[i], "~")
  wikipedia$reign.end[i] <-  paste0(wikipedia$reign.end[i], "~")
  wikipedia$reign.start[i] <-  paste0(wikipedia$reign.start[i], "~")
}

# Let's also keep the year only for those dates which
# the notes detail only year in certain.
for(i in c(18, 22, 23)){ # reign start year only
  wikipedia$reign.start[i] <-  stringr::str_extract(wikipedia$reign.start[i], "^[0-9]{4}")
}
# birth year only
wikipedia$birth[24] <-  stringr::str_extract(wikipedia$birth[24], "^[0-9]{4}")

# Finally, some dates appear to be ranges.
# `{messydates}` deals with ranges with a ".." separator.
wikipedia$birth[20] <-  paste0(wikipedia$birth[20], "..", "0137-02-02")
wikipedia$birth[66] <-  paste0(wikipedia$birth[66], "..", "0359-05-23")

# standardise_dates() is a wrapper function for
# messydates::as_messydates() and messydates::make_messydates(). 
# Let's standardise dates and variable names
wikipedia <- as_tibble(wikipedia) %>%
  qData::transmutate(ID = name,
                     Beg = qCreate::standardise_dates(reign.start),
                     End = qCreate::standardise_dates(reign.end)
  ) %>%
  dplyr::rename()
  dplyr::arrange(Beg)

# qData includes several functions that should help cleaning
# and standardising your data.
# Please see the vignettes or website for more details.

# Stage three: Connecting data
# Next run the following line to make Wikipedia available
# within the qPackage.
qCreate::export_data(wikipedia,  database = "emperors", URL = "https://github.com/zonination/emperors")
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
