# Compare missing observations for 'many' data

Compare missing observations for 'many' data

## Usage

``` r
compare_missing(datacube, dataset = "all", variable = "all")
```

## Arguments

- datacube:

  A datacube from one of the many packages.

- dataset:

  A dataset in a datacube from one of the many packages. NULL by
  default. That is, all datasets in the datacube are used. To select two
  or more datasets, please declare them as a vector.

- variable:

  Would you like to focus on one, or more, specific variables present in
  one or more datasets in the 'many' datacube? By default "all". For
  multiple variables, please declare variable names as a vector.

## Value

`compare_missing()` returns a tibble with information about each dataset
including the number of observations, the number of variables, the
earliest date, and the latest date in all observations.

## Details

`compare_missing()` compares the missing observations for variables in
each dataset in a 'many' datacube.

## See also

Other compare\_:
[`compare_categories()`](https://www.manydata.ch/reference/compare_categories.md),
[`compare_dimensions()`](https://www.manydata.ch/reference/compare_dimensions.md),
[`compare_overlap()`](https://www.manydata.ch/reference/compare_overlap.md)

## Examples

``` r
# \donttest{
compare_missing(emperors)
#> # A tibble: 24 × 6
#>    Variable  Dataset    Class     Count Missing `Percent Missing`
#>    <chr>     <chr>      <chr>     <dbl>   <dbl>             <dbl>
#>  1 Begin     Wikipedia  mdate        69       0              0   
#>  2 Begin     UNRV       mdate        98       0              0   
#>  3 Begin     Britannica mdate        87       0              0   
#>  4 Birth     Wikipedia  mdate        69       6              8.7 
#>  5 Birth     UNRV       mdate        98      24             24.5 
#>  6 Cause     Wikipedia  character    69       1              1.45
#>  7 CityBirth Wikipedia  character    69      18             26.1 
#>  8 Dataset   Wikipedia  character    69       0              0   
#>  9 Dataset   UNRV       character    98       0              0   
#> 10 Dataset   Britannica character    87       0              0   
#> # ℹ 14 more rows
plot(compare_missing(emperors))

# }
```
