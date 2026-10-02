# Call treaties from 'many' datasets

Call treaties from 'many' datasets

## Usage

``` r
call_treaties(
  dataset,
  treaty_type = NULL,
  variable = NULL,
  actor = NULL,
  key = "manyID"
)
```

## Arguments

- dataset:

  A dataset in a datacube from one of the many packages. NULL by
  default. That is, all datasets in the datacube are used. For multiple
  datasets, please declare datasets as a vector (e.g. c("dataset1",
  "dataset2")).

- treaty_type:

  The type of treaties to be returned. NULL, by default. Other options
  are "bilateral" or "multilateral".

- variable:

  Would you like to get one, or more, specific variables present in one
  or more datasets in the 'many' datacube? NULL by default. For multiple
  variables, please declare variable names as a vector.

- actor:

  An actor variable in dataset. NULL by default. If declared, a tibble
  of the treaties and their member actors is returned.

- key:

  A variable key to join datasets. 'manyID' by default.

## Value

`call_treaties()` returns a tibble with a list of the agreements.

## Details

Certain datasets, or consolidated datacubes, in 'many' packages contains
information on treaties which can be retrieved with `call_treaties()`.

## See also

Other call\_:
[`call_packages()`](https://www.manydata.ch/reference/call_packages.md),
[`call_releases()`](https://www.manydata.ch/reference/call_releases.md)

## Examples

``` r
# \donttest{
membs <- dplyr::tibble(manyID = c("ROU-RUS[RFP]_1901A",
"ROU-RUS[RFP]_1901A", "GD16FI_1901A"),
stateID = c("ROU", "RUS", "DNK"),
Title = c("Convention Between Roumania And Russia Concerning Fishing
In The Danube And The Pruth",
"Convention Between Roumania And Russia Concerning Fishing
In The Danube And The Pruth",
"Convention Between The Governments Of Denmark And
The United Kingdom Of Great Britain
And Northern Ireland For Regulating The Fisheries
Of Their Respective Subjects Outside
Territorial Waters In The Ocean Surrounding The Faroe Islands"),
Begin = c("1901-02-22", "1901-02-22", "1901-06-24"))
call_treaties(membs)
#> # A tibble: 2 × 1
#>   manyID            
#>   <chr>             
#> 1 ROU-RUS[RFP]_1901A
#> 2 GD16FI_1901A      
call_treaties(membs, treaty_type = "bilateral",
variable = c("Title", "Begin"))
#> # A tibble: 1 × 3
#>   manyID             Title                                                 Begin
#>   <chr>              <chr>                                                 <chr>
#> 1 ROU-RUS[RFP]_1901A "Convention Between Roumania And Russia Concerning F… 1901…
call_treaties(membs, variable = c("Title", "Begin"), actor = "stateID")
#> # A tibble: 2 × 4
#>   manyID             Memberships Title                                     Begin
#>   <chr>              <chr>       <chr>                                     <chr>
#> 1 GD16FI_1901A       DNK         "Convention Between The Governments Of D… 1901…
#> 2 ROU-RUS[RFP]_1901A ROU, RUS    "Convention Between Roumania And Russia … 1901…
# }
```
