# Data reports for datacubes and datasets with 'mdate' variables

These functions provide meta level descriptions of datacubes or
datasets. `mreport()` creates a properly formatted data report for
datasets which contain 'mdate' class objects, alongside other object
classes. `describe_datacube()` prints a text description of the datasets
in a datacube.

## Usage

``` r
mreport(data)

describe_datacube(datacube)
```

## Arguments

- data:

  A `{tibble}` or a `{data.frame}`.

- datacube:

  A datacube

## Value

A data report of class 'mreport'.

## Details

'mreport' displays the variable's name, the variable type, the number of
observations per variable, the number of missing observations for
variable, and the percentage of missing observations in variable.

## Examples

``` r
mreport(emperors)
#> $Wikipedia
#> --------------------------------------------------------
#> |  Variable   |  Class  |  Obs  |  Missing  |  Miss %  |
#> --------------------------------------------------------
#> |ID           |character|     69|          0|         0|
#> |Begin        |mdate    |     69|          0|         0|
#> |End          |mdate    |     69|          0|         0|
#> |FullName     |character|     68|          1|      1.45|
#> |Birth        |mdate    |     63|          6|       8.7|
#> |Death        |mdate    |     68|          1|      1.45|
#> |CityBirth    |character|     51|         18|     26.09|
#> |ProvinceBirth|character|     68|          1|      1.45|
#> |Rise         |character|     68|          1|      1.45|
#> |Cause        |character|     68|          1|      1.45|
#> |Killer       |character|     68|          1|      1.45|
#> |Dynasty      |character|     68|          1|      1.45|
#> |Era          |character|     68|          1|      1.45|
#> |Notes        |character|     46|         23|     33.33|
#> --------------------------------------------------------
#> 
#> 
#> $UNRV
#> -------------------------------------------------------
#> |  Variable  |  Class  |  Obs  |  Missing  |  Miss %  |
#> -------------------------------------------------------
#> |ID          |character|     98|          0|         0|
#> |Begin       |mdate    |     98|          0|         0|
#> |End         |mdate    |     98|          0|         0|
#> |Birth       |mdate    |     74|         24|     24.49|
#> |Death       |mdate    |     97|          1|      1.02|
#> |FullName    |character|     93|          5|       5.1|
#> |Dynasty     |character|     61|         37|     37.76|
#> -------------------------------------------------------
#> 
#> 
#> $Britannica
#> -------------------------------------------------------
#> |  Variable  |  Class  |  Obs  |  Missing  |  Miss %  |
#> -------------------------------------------------------
#> |ID          |character|     87|          0|         0|
#> |Begin       |mdate    |     87|          0|         0|
#> |End         |mdate    |     87|          0|         0|
#> -------------------------------------------------------
#> 
#> 
```
