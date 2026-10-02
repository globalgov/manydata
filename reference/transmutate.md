# Drop only columns used in formula

A function between dplyr's transmute and mutate

## Usage

``` r
transmutate(.data, ...)
```

## Source

https://stackoverflow.com/questions/51428156/dplyr-mutate-transmute-drop-only-the-columns-used-in-the-formula

## Arguments

- .data:

  Data frame to pass to the function

- ...:

  Variables to pass to the function

## Value

Data frame with mutated variables and none of the variables used in the
mutations, but, unlike
[`dplyr::transmute()`](https://dplyr.tidyverse.org/reference/transmute.html),
all other unnamed variables.

## Examples

``` r
# \donttest{
pluck(emperors, "Wikipedia")
#> Please cite the dataset: 
#> • Wikipedia, 'List_of_Roman_emperors',  https://en.wikipedia.org/wiki/List_of_Roman_emperors, Accessed on 2021-07-22.
#> # A tibble: 69 × 14
#>    ID       Begin End   FullName Birth Death CityBirth ProvinceBirth Rise  Cause
#>    <chr>    <mda> <mda> <chr>    <mda> <mda> <chr>     <chr>         <chr> <chr>
#>  1 Augustus -002… 0014… Imperat… -006… 0014… Rome      Italia        Birt… Assa…
#>  2 Tiberius 0014… 0037… Tiberiv… -004… 0037… Rome      Italia        Birt… Assa…
#>  3 Caligula 0037… 0041… Gaivs I… 0012… 0041… Antitum   Italia        Birt… Assa…
#>  4 Claudius 0041… 0054… Tiberiv… -000… 0054… Lugdunum  Gallia Lugdu… Birt… Assa…
#>  5 Nero     0054… 0068… Nero Cl… 0037… 0068… Antitum   Italia        Birt… Suic…
#>  6 Galba    0068… 0069… Servivs… -000… 0069… Terracina Italia        Seiz… Assa…
#>  7 Otho     0069… 0069… Marcvs … 0032… 0069… Terentin… Italia        Appo… Suic…
#>  8 Vitelli… 0069… 0069… Avlvs V… 0015… 0069… Rome      Italia        Seiz… Assa…
#>  9 Vespasi… 0069… 0079… Titvs F… 0009… 0079… Falacrine Italia        Seiz… Natu…
#> 10 Titus    0079… 0081… Titvs F… 0039… 0081… Rome      Italia        Birt… Natu…
#> # ℹ 59 more rows
#> # ℹ 4 more variables: Killer <chr>, Dynasty <chr>, Era <chr>, Notes <chr>
transmutate(emperors$Wikipedia, Beginning = Begin)
#> # A tibble: 69 × 14
#>    ID      End   FullName Birth Death CityBirth ProvinceBirth Rise  Cause Killer
#>    <chr>   <mda> <chr>    <mda> <mda> <chr>     <chr>         <chr> <chr> <chr> 
#>  1 August… 0014… Imperat… -006… 0014… Rome      Italia        Birt… Assa… Wife  
#>  2 Tiberi… 0037… Tiberiv… -004… 0037… Rome      Italia        Birt… Assa… Other…
#>  3 Caligu… 0041… Gaivs I… 0012… 0041… Antitum   Italia        Birt… Assa… Senate
#>  4 Claudi… 0054… Tiberiv… -000… 0054… Lugdunum  Gallia Lugdu… Birt… Assa… Wife  
#>  5 Nero    0068… Nero Cl… 0037… 0068… Antitum   Italia        Birt… Suic… Senate
#>  6 Galba   0069… Servivs… -000… 0069… Terracina Italia        Seiz… Assa… Other…
#>  7 Otho    0069… Marcvs … 0032… 0069… Terentin… Italia        Appo… Suic… Other…
#>  8 Vitell… 0069… Avlvs V… 0015… 0069… Rome      Italia        Seiz… Assa… Other…
#>  9 Vespas… 0079… Titvs F… 0009… 0079… Falacrine Italia        Seiz… Natu… Disea…
#> 10 Titus   0081… Titvs F… 0039… 0081… Rome      Italia        Birt… Natu… Disea…
#> # ℹ 59 more rows
#> # ℹ 4 more variables: Dynasty <chr>, Era <chr>, Notes <chr>, Beginning <mdate>
# }
```
