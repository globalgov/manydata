# Qdataraw Template
# This is a template for how to Collect, Correct and Connect Data.
# All references to 'dataset' should be replaced with the name of the dataset.

# Stage one: collecting data
library(qDatr)
# For example, you may wish to import a CSV file
dataset <- read_csv("data-raw/data/dataset/dataset.csv")

# Stage two: correcting data
# In this stage you will want to correct the variable names and formats of the 'dataset' object
# until the object created below (in stage three) passes all the tests.

# Stage three: connecting data
# At this stage, all you need to do
use_qData(dataset)
