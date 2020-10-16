# {{{name}}} Preparation Script

# This is a template for how to Collect, Correct and Connect data 
# for the qDatr system.
library(qDatr)

# Stage one: collecting data
# For example, you may wish to import a CSV file
{{{name}}} <- read_csv("data-raw/data/dataset/{{{name}}}.csv")
# For excel files you may wish to import with the read_excel() function and 
# for dta files you may wish to import with read_stata () function.

# Stage two: correcting data
# In this stage you will want to correct the variable names and 
# formats of the '{{{name}}}' object until the object created 
# below (in stage three) passes all the tests.

# Stage three: connecting data
# At this stage, all you need to do is run the next line to 
# make {{{name}}} available within the package.
export_data({{{name}}})
