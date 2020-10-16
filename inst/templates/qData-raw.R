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
# below (in stage three) passes all the tests. Several of the functions in 
# the qDatr package fit the purpose of wrangling and standardising data. 
# The function transmutate(), for example, returns mutated variables
# while dropping the original varibles used in the mutation. 
# Please refer to package manual further details and examples.

# Stage three: connecting data
# At this stage, all you need to do is run the next line to 
# make {{{name}}} available within the package.
export_data({{{name}}})
# Please note that export_data() also creates documentation and test files for data.
# Please make sure to cite source dataset on documentation file created. 
# Please make sure data complies with qDatr requirements by running test script created 
# and, if any of the tests fail, please go back to step two and correct the data. 