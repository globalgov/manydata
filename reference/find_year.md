# Creates Numerical IDs from Signature Dates

Agreements should have a unique identification number that is
meaningful, we condense their signature dates to produce this number.

## Usage

``` r
find_year(date)
```

## Arguments

- date:

  A date variable

## Value

A character vector with condensed dates

## Examples

``` r
if (FALSE) { # \dontrun{
IEADB <- dplyr::slice_sample(manyenviron::agreements$IEADB, n = 10)
code_dates(IEADB$Title)
} # }
```
