# Compare dimensions for 'many' data

Compare dimensions for 'many' data

## Usage

``` r
compare_dimensions(datacube, dataset = "all")
```

## Arguments

- datacube:

  A datacube from one of the many packages.

- dataset:

  A dataset in a datacube from one of the many packages. By default,
  "all". That is, all datasets in the datacube are used. To select two
  or more datasets, please declare them as a vector.

## Value

`compare_dimensions()` returns a tibble with information about each
dataset including the number of observations, the number of variables,
the earliest date, and the latest date in all observations.

## Details

`compare_dimensions()` compares the number of observations, variables,
the earliest date, and the latest date in all observations for datasets
in a 'many' datacube.

## See also

Other compare\_:
[`compare_categories()`](https://www.manydata.ch/reference/compare_categories.md),
[`compare_missing()`](https://www.manydata.ch/reference/compare_missing.md),
[`compare_overlap()`](https://www.manydata.ch/reference/compare_overlap.md)

## Examples

``` r
# \donttest{
compare_dimensions(emperors)
#> # A tibble: 3 × 5
#>   Dataset    Observations Variables                    Earliest_Date Latest_Date
#>   <chr>      <chr>        <chr>                        <chr>         <chr>      
#> 1 Wikipedia  69           ID, Begin, End, FullName, B… -0062-09-23   0421-09-02 
#> 2 UNRV       98           ID, Begin, End, Birth, Deat… -0063-01-01   0518-12-31 
#> 3 Britannica 87           ID, Begin, End               -0031-01-01   0491-12-31 
# }
```
