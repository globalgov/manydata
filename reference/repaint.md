# Fills missing data by lookup

Fills missing data where known by other observations with the same
id/index

## Usage

``` r
repaint(df, id, var)
```

## Arguments

- df:

  a dataframe

- id:

  a string identifying a column in the dataframe for indexing

- var:

  a string identifying a column or columns in the dataframe to be filled

## Value

A dataframe

## Examples

``` r
# \donttest{
data <- data.frame(ID = c(1,2,3,3,2,1),
                    One = c(1,NA,3,NA,2,NA),
                    Two = c(NA,"B",NA,"C",NA,"A"))
repaint(data, "ID", c("One","Two"))
#>   ID One Two
#> 1  1   1   A
#> 2  2   2   B
#> 3  3   3   C
#> 4  3   3   C
#> 5  2   2   B
#> 6  1   1   A
# }
```
