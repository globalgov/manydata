# Pastes unique string vectors

For use with dplyr::summarise, for example

## Usage

``` r
recollect(x, collapse = "_")
```

## Arguments

- x:

  A vector

- collapse:

  String indicating how elements separated

## Value

A single value

## Details

This function operates similarly to reunite, but instead of operating on
columns/observations, it pastes together unique rows/observations.

## Examples

``` r
# \donttest{
data <- data.frame(ID = c(1,2,3,3,2,1))
data1 <- data.frame(ID = c(1,2,3,3,2,1), One = c(1,NA,3,NA,2,NA))
recollect(data$ID)
#> [1] "1_2_3"
recollect(data1$One)
#> [1] "1_3_2"
# }
```
