
# manydata <img src="man/figures/manydataLogo.png" align="right" width="220"/>

<!-- badges: start -->

[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
![GitHub release (latest by
date)](https://img.shields.io/github/v/release/globalgov/manydata)
![GitHub Release
Date](https://img.shields.io/github/release-date/globalgov/manydata)
![GitHub
issues](https://img.shields.io/github/issues-raw/globalgov/manydata)
<!-- [![HitCount](http://hits.dwyl.com/globalgov/manydata.svg)](http://hits.dwyl.com/globalgov/manydata) -->
[![Codecov test
coverage](https://codecov.io/gh/globalgov/manydata/branch/main/graph/badge.svg)](https://app.codecov.io/gh/globalgov/manydata?branch=main)
[![CodeFactor](https://www.codefactor.io/repository/github/globalgov/manydata/badge)](https://www.codefactor.io/repository/github/globalgov/manydata)
[![CII Best
Practices](https://bestpractices.coreinfrastructure.org/projects/4562/badge)](https://bestpractices.coreinfrastructure.org/projects/4562)
<!-- ![GitHub All Releases](https://img.shields.io/github/downloads/jhollway/roctopus/total) -->
<!-- badges: end -->

`{manydata}` is a portal to other ‘many packages’ that helps researchers
download and manipulate related data across various issue-domains of
global governance, and beyond.

## Why manydata?

- {manydata} offers users convenient access to various ‘many packages’
  that gather well-regarded global governance datasets

- {manydata} helps users to easily compare databases (i.e. collections
  of related datasets)

- {manydata} allows users to rapidly consolidate databases and datasets
  in different ways to check the robustness of their results

`{manydata}` contains several functions to help global governance
researchers. For a quick overview, please also check the package cheat
sheet.

<a href="https://github.com/globalgov/manydata/blob/develop/man/figures/cheatsheet.pdf"><img src="https://raw.githubusercontent.com/globalgov/manydata/develop/man/figures/cheatsheet.png" width="525" height="378"/></a>

For more details, please see the
[vignette](https://globalgov.github.io/manydata/articles/user.html).

## Downloading and installing manydata

The easiest way to install `{manydata}` is directly from CRAN.

``` r
install.packages("manydata")
```

The development version of the package `{manydata}` can also be
downloaded from GitHub.

``` r
# install.packages("remotes")
remotes::install_github("globalgov/manydata")
```

## Call ‘many’ packages

`{manydata}` connects users to other packages that help fill global
governance researchers’ data needs. The `get_packages()` function can be
used to discover the ‘many packages’ currently available.

``` r
library(manydata)
get_packages()
```

Please see [the website](https://globalgov.github.io/manydata/) for more
information about how to use `{manydata}`.

## Comparing ‘many’ data

Once ‘many’ data packages are downloaded, `{manydata}` helps users
visualize the relationship between matched observations across datasets
within a database for a ‘many’ package database.

``` r
plot_categories(database = emperors, key = "ID", variable = "all", category = "all")
```

    #> There were 116 matched observations by ID variable across datasets in database.

<img src="man/figures/README-unnamed-chunk-2-1.png" width="100%" />

## Consolidating ‘many’ data

`{manydata}` also contains flexible methods for consolidating ‘many’
package database into a single dataset with some combination of the
rows, columns, as well as for how to resolve conflicts for observations
across datasets.

``` r
consolidate(database = emperors, rows = "every", cols = "every",
            resolve = "coalesce", key = "ID")
```

    #> There were 116 matched observations by ID variable across datasets in database.

    #> # A tibble: 41 × 3
    #>    ID             Beg         End        
    #>    <chr>          <mdate>     <mdate>    
    #>  1 Aemilian       0253-08-15~ 0253-10-15~
    #>  2 Augustus       -0026-01-16 0014-08-19 
    #>  3 Aurelian       0270-09-15  0275-09-15 
    #>  4 Balbinus       0238-04-22  0238-07-29 
    #>  5 Caracalla      0198        0217-04-08 
    #>  6 Carinus        0283-08-01~ 0285-08-01~
    #>  7 Carus          0282-10-01~ 0283-08-01~
    #>  8 Claudius       0041-01-25  0054-10-13 
    #>  9 Commodus       0177        0192-12-31 
    #> 10 Constantine II 0337-05-22  0340-01-01 
    #> # ℹ 31 more rows

## Contributing to the many packages universe

For more information for developers and data contributors to ‘many
packages’, please see `{manypkgs}` [the
website](https://globalgov.github.io/manypkgs/).
