# Selects a single dataset from a datacube

This function is reexported/wrapped from the `{purrr}` package. It
allows users to select a single dataset from one of the datacubes
available across the 'many\* packages'. It additionally invites users to
cite the selected dataset.

## Usage

``` r
pluck(.x, ..., .default = NULL)
```

## Arguments

- .x:

  The datacube

- ...:

  The name of the dataset in the datacube

- .default:

  Value to use if target is `NULL` or absent.

## Value

The selected dataset

## Examples

``` r
# \donttest{
pluck(emperors, "UNRV")
#> Please cite the dataset: 
#> • United Nations of Roma Victrix, 'Roman Emperor list', https://www.unrv.com/government/emperor.php, Accessed on 2021-07-22.
#> # A tibble: 98 × 7
#>    ID        Begin   End     Birth   Death   FullName                    Dynasty
#>    <chr>     <mdate> <mdate> <mdate> <mdate> <chr>                       <chr>  
#>  1 Augustus  -0027   -0014   -0063   0014    Gaius Julius Caesar Octavi… Julio-…
#>  2 Tiberius  0014    0037    -0042   0037    Tiberius Claudius Nero Cae… Julio-…
#>  3 Caligula  0037    0041    0012    0014    Gaius Caesar Germanicus / … Julio-…
#>  4 Claudius  0041    0054    -0010   0041    Tiberius Claudius Nero Ger… Julio-…
#>  5 Nero      0054    0068    0037    0068    Claudius Nero Caesar (born… Julio-…
#>  6 Galba     0068    0069    -0003   0069    Servius Sulpicius Galba / … Year o…
#>  7 Otho      0069    0069    0032    0069    Marcus Salvius Otho / Impe… Year o…
#>  8 Vitellius 0069    0069    0015    0069    Aulus Vitellius / Aulus Vi… Year o…
#>  9 Vespasian 0069    0079    0009    0079    Titus Flavius Vespasianus … Year o…
#> 10 Titus     0079    0081    0039    0079    Titus Flavius Vespasianus … Flavian
#> # ℹ 88 more rows
# }
```
