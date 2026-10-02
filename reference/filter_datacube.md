# Filtering datacube datasets to a certain date

Filtering datacube datasets to a certain date

## Usage

``` r
filter_datacube(datacube, date = Sys.Date())
```

## Arguments

- datacube:

  A datacube, i.e. a list of data frames with Begin and End date
  variables.

- date:

  A date (of class Date or character) at which to filter the datacube.

## Examples

``` r
filter_datacube(emperors, date = "0100")
#> $Wikipedia
#> # A tibble: 1 × 14
#>   ID     Begin    End   FullName Birth Death CityBirth ProvinceBirth Rise  Cause
#>   <chr>  <mdate>  <mda> <chr>    <mda> <mda> <chr>     <chr>         <chr> <chr>
#> 1 Trajan 0098-01… 0117… Caesar … 0053… 0117… Italica   Hispania Bae… Birt… Natu…
#> # ℹ 4 more variables: Killer <chr>, Dynasty <chr>, Era <chr>, Notes <chr>
#> 
#> $UNRV
#> # A tibble: 1 × 7
#>   ID     Begin   End     Birth   Death   FullName                        Dynasty
#>   <chr>  <mdate> <mdate> <mdate> <mdate> <chr>                           <chr>  
#> 1 Trajan 0098    0117    0053    0117    Marcus Ulpius Nerva Traianus /… Adopti…
#> 
#> $Britannica
#> # A tibble: 1 × 3
#>   ID     Begin   End    
#>   <chr>  <mdate> <mdate>
#> 1 Trajan 0098    0117   
#> 
```
