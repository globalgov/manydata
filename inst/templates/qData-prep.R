# {{{dataset}}} Preparation Script

# This is a template for importing, cleaning, and exporting data
# ready for the qPackage.
library(qData)

# Stage one: Collecting data
{{{dataset}}} <- {{{import_type}}}("{{{path}}}")

# Stage two: Correcting data
# In this stage you will want to correct the variable names and
# formats of the '{{{dataset}}}' object until the object created
# below (in stage three) passes all the tests. Several of the functions in
# the qData package fit the purpose of wrangling and standardising data.
# The function transmutate(), for example, returns mutated variables
# while dropping the original variables used in the mutation.
# Please refer to package manual further details and examples.
# TODO: Get users to check whether missing observations are
# correctly stated as NA.

# Stage three: connecting data
# At this stage, all you need to do is run the next line to
# make {{{dataset}}} available within the package.
export_data({{{dataset}}})
# Please note that export_data() also creates documentation and test
# files for data.
# Please make sure to cite source dataset on documentation file created.
# Please make sure data complies with qData requirements by running
# test script created and, if any of the tests fail, please go
# back to step two and correct the data.
