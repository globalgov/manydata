
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

`{manydata}` is the central package in the many packages universe aimed
at collecting, connecting, and correcting network data across
issue-domains of global governance. To assist users in doing so,
`{manydata}` contains functions that enable users to download and
manipulate data easily.

## Why manydata?

`{manydata}` offers users access to all of the tested data in the
various ‘many packages’ available, for use in analyses of global
governance and beyond. A special feature of the ‘many packages’ is that
it is not ‘opinionated’ - instead of offering a single, supposedly
authoritative version of global governance events, the packages in the
many packages universe gather well-regarded datasets in each
issue-domain into three-dimensional ‘datacubes’. The chief advantage of
this for global governance researchers is that it enables a quick and
easy way to check the robustness of their results using different
formulations of the study population or concept specification. The
‘datacube’ structure has a specific coding system for the variables
across the datasets. For more details, please see the
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

## Available ‘many packages’

`{manydata}` connects users to other packages that help fill global
governance researchers’ data needs. The `get_packages()` function can be
used to discover the ‘many packages’ currently available.

``` r
library(manydata)
get_packages()
```

Please see [the website](https://globalgov.github.io/manydata/) for more
information about how to use `{manydata}`.

## Cheat Sheet

<a href="https://github.com/globalgov/manydata/blob/main/man/figures/cheatsheet.pdf"><img src="https://raw.githubusercontent.com/globalgov/manydata/main/man/figures/cheatsheet.png" width="525" height="378"/></a>

## Contributing to the many packages universe

For more information for developers and data contributors to ‘many
packages’, please see `{manypkgs}` [the
website](https://globalgov.github.io/manypkgs/).
