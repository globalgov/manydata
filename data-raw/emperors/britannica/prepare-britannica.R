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
britannica <- britannica %>% 
  dplyr::rename(Beg = Birth,
                End = Death)

for(i in c(1:6, 9:13)) {
  britannica$Beg[i] <- paste0("00", britannica$Beg[i])
}

for(i in c(14:18, 21:27,32:43, 45:75, 77:80, 82:87)) {
  britannica$Beg[i] <- paste0("0", britannica$Beg[i])
}

for(i in c(1:6, 9:12)) {
  britannica$End[i] <- paste0("00", britannica$End[i])
}

for(i in c(13:18, 21:27, 32:43, 45:75, 77:80, 82:87)) {
  britannica$End[i] <- paste0("0", britannica$End[i])
}

# Correct dates format 
britannica$Beg[7] <- "0069-01"
britannica$Beg[8] <- "0069-07"
britannica$Beg[19] <- "0193-01"
britannica$Beg[20] <- "0193-03"
britannica$Beg[28] <- "0238-03"
britannica$Beg[29] <- "0238-03"
britannica$Beg[30] <- "0238-04-22"
britannica$Beg[31] <- "0238-04-22"
britannica$Beg[44] <- "0276-06"
britannica$Beg[76] <- "0455-03-17"
britannica$Beg[81] <- "0472-04"


britannica$End[7] <- "0069-04"
britannica$End[8] <- "0069-12"
britannica$End[19] <- "0193-03"
britannica$End[20] <- "0193-06"
britannica$End[28] <- "0238-04"
britannica$End[29] <- "0238-04"
britannica$End[30] <- "0238-07-29"
britannica$End[31] <- "0238-07-29"
britannica$End[44] <- "0276-09"
britannica$End[76] <- "0455-05-32"
britannica$End[81] <- "0472-11"

britannica <- as_tibble(britannica) %>%
  dplyr::mutate(Beg = as_messydate(Beg)) %>% 
  dplyr::mutate(End = as_messydate(End)) %>% 
  transmutate(ID = Name)
# qData includes several functions that should help cleaning
# and standardising your data.
# Please see the vignettes or website for more details.

# Stage three: Connecting data
# Next run the following line to make britannica available
# within the qPackage.
export_data(britannica, database = "emperors", URL = "https://www.britannica.com/topic/list-of-Roman-emperors-2043294")
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
