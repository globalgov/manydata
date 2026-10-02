# Resolving multiple observations of the same variable into one

This family of functions provides row-wise summarization for data frames
or tibbles, returning a single value per row based on specified columns.
They are useful for tasks like extracting typical or summary values from
multiple variables, simplifying wide data structures, and imputing
representative values.

## Usage

``` r
resolve_unite(.data, vars, na.rm = TRUE)

resolve_coalesce(.data, vars)

resolve_min(.data, vars, na.rm = TRUE)

resolve_max(.data, vars, na.rm = TRUE)

resolve_random(.data, vars, na.rm = TRUE)

resolve_precision(.data, vars)

resolve_mean(.data, vars, na.rm = TRUE)

resolve_mode(.data, vars, na.rm = TRUE)

resolve_median(.data, vars, na.rm = TRUE)

resolve_consensus(.data, vars, na.rm = TRUE)
```

## Arguments

- .data:

  A data frame or tibble containing the variables.

- vars:

  A vector of variables from `.data` to be resolved or converged. If
  this argument is left unspecified, then all variables will be merged
  together.

- na.rm:

  Logical whether missing values (NAs) should be removed before
  operation of the function. Note that unlike how the `na.rm` argument
  operates in functions in base R, e.g.
  [`max()`](https://rdrr.io/r/base/Extremes.html), here the default is
  TRUE.

## Unite

Uniting returns all the unique values as a set, separated by commas and
contained within braces. Note that uniting always returns a
character/string vector, which enables it to accommodate different
classes of variables. The order of the values reflects their first
appearance; that is, they are not ordered by increasing value.

## Coalesce

Coalescing returns a vector of the first non-missing values found when
reading the variables from left to right. That is, missing values in the
first vector may be filled by observations in the second vector, or
later vectors if the second vector also misses an observation for that
cell. Variables can be reordered manually.

## Min and Max

These functions return a vector containing each row's minimum or maximum
value. Note that these functions work not only on numeric and date
vectors, but also on character string vectors. For character data, these
functions will return the shortest or longest strings, respectively, in
each row.

## Random

This function returns a vector of values selected randomly from among
the values contained in each row. Note that by default `na.rm = TRUE`,
which means that missing data will not be selected at random by default,
which can also change the probability distribution by each row. Where
`na.rm = FALSE`, the probability of each value being selected is
uniform.

## Precision

This function returns a vector that maximises the precision of the
values in each row. For numeric vectors, precision is expressed in
significant digits, such that 1.01 would be more precise than 1. For
character vectors, precision is expressed in terms of the character
length proportional to the max character length in the row. This applies
also to messydates, meaning precision is expressed in the lowest level
date component specified, such that 2008-10 would be more precise than
2008, and 2008-10-10 would be more precise still.

## Mean and median

These functions return a vector of the means or medians, respectively,
of the values in each row.

## Consensus

This function returns a vector of consensus values, i.e. where there is
no variation in values by each row. If the values (excluding missing
values by default) are not equivalent, then an NA is returned for that
row.

## Examples

``` r
test <- data.frame(preferred_dataset = c(1,6,NA), 
                   more_comprehensive = c(1,3,3), 
                   precise_where_available = c(NA,3.3,4.1))
test
#>   preferred_dataset more_comprehensive precise_where_available
#> 1                 1                  1                      NA
#> 2                 6                  3                     3.3
#> 3                NA                  3                     4.1
resolve_unite(test)
#> [1] "{1}"       "{6,3,3.3}" "{3,4.1}"  
resolve_coalesce(test)
#> [1] 1 6 3
resolve_min(test)
#> [1] 1 3 3
resolve_max(test)
#> [1] 1.0 6.0 4.1
resolve_random(test)
#> [1] 1 3 3
resolve_precision(test)
#> [1] 1.0 3.3 4.1
resolve_mean(test)
#> [1] 1.00 4.10 3.55
resolve_mode(test)
#> [1] 1 3 3
resolve_median(test)
#> [1] 1.00 3.30 3.55
resolve_consensus(test)
#> [1]  1 NA NA
```
