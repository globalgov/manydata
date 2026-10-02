# Pastes unique string vectors

A vectorised function for use with dplyr's mutate, etc

## Usage

``` r
reunite(..., sep = "_")
```

## Arguments

- ...:

  Variables to pass to the function, currently only two at a time

- sep:

  Separator when vectors reunited, by default "\_"

## Value

A single vector with unique non-missing information

## Examples

``` r
# \donttest{
data <- data.frame(fir=c(NA, "two", "three", NA),
                   sec=c("one", NA, "three", NA), stringsAsFactors = FALSE)
transmutate(data, single = reunite(fir, sec))
#>   single
#> 1    one
#> 2    two
#> 3  three
#> 4   <NA>
# }
```
