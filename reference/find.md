# Find elements within manydata

Find elements within manydata

## Usage

``` r
find_ID(df, id_col = "ID")

find_common_ID(..., id_col = "ID")

find_duplicates(df, id_col = "ID")
```

## Arguments

- df:

  A data frame to be scored.

- id_col:

  The name of the column containing IDs. Default is "ID".

- ...:

  Data frames to compare

## Examples

``` r
find_duplicates(emperors$Wikipedia)
#> There are no duplicate IDs in the ID column.
#> NULL
```
