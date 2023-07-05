
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

`{manydata}` is a portal to ‘many’ packages containing many datacubes,
each containing many related datasets on many issue-domains, actors and
institutions of global governance. These ‘many’ packages are: -
`{manyenviron}`: contains data on international environmental
agreements - `{manytrade}`: contains data on international trade
agreements - `{manyhealth}`: contains data on international health
agreements - `{manystates}`: contains data on states throughout history

Datasets are related to one another within a datacube through a
particular coding system which follows the same principles across the
different packages.

For instance, in the data packages on international agreements
(including `{manyenviron}`, `{manytrade}`, and `{manyhealth}`), the
`agreements` and `memberships` datacubes have standardised IDs
(`manyID`), and date variables such as `Begin` and `End` that denote the
beginning and end dates of treaties respectively. The beginning date is
derived from the signature or entry into force date, whichever is the
earliest available date for the treaty. Standardised IDs across datasets
allow the same observations to be matched across datasets so that the
values can be compared or expanded where relevant. These specific
variable names allows the comparison of information across datasets that
have different sources. It enables users to point out the recurrence,
difference or absence of observations between the datasets and extract
more robust data when researching on a particular governance domain.

The memberships datacube contains additional date variables on each
state member’s ratification, signature, entry into force, and end dates
for each treaty. Data in the memberships datacube is comparable across
datasets through standardised state names and stateIDs, made possible
with the `manypkgs::code_states()` function. More information on each
state, including its `Begin` and `End` date, can be found in the
`{manystates}` package.

To enable users to work with the data in these packages, `{manydata}`
contains tools for:

- *calling* data packages,
- *comparing* individual datasets, and
- *consolidating* datacubes in different ways.

We intend for `{manydata}` to be useful:

- at the **start** of a research project, to access and gather recent
  versions of well-regarded datasets, see what is available, describe,
  and explore the data,
- in the **middle** of a project, to facilitate analysis, comparison and
  modelling, and
- at the **end** of the project, to help with conducting robustness
  checks, preparing replication scripts, and writing the next grant
  application.

## Call ‘many’ packages

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

Once `{manydata}` is installed, the `call_` functions can be used to
discover the ‘many packages’ currently available and/or download or
update these packages when needed. For this, the `call_packages()` can
be used.

``` r
library(manydata)
call_packages() # lists all packages currently available
call_packages("manytrade") # downloads and installs this package
```

The `call_sources()` function obtains information about the sources and
original locations of the desired datasets.

``` r
call_sources(package = "manydata", datacube = "emperors")
```

    #> # A tibble: 3 × 4
    #>   Dataset    Source                                                URL   Mapping
    #>   <chr>      <chr>                                                 <chr> <chr>  
    #> 1 wikipedia  Wikipedia, List_of_Roman_emperors, https://en.wikipe… http… from -…
    #> 2 UNRV       UNRV, Roman Emperor list, https://www.unrv.com/gover… http… from -…
    #> 3 britannica Britannica, List of Roman emperors, https://www.brit… http… from -…

## Comparing ‘many’ data

The first thing users of the data packages may want to do is to identify
datasets that might contribute to their research goals. One major
advantage of storing datasets in datacubes is that it facilitates the
comparison and analysis of multiple datasets in a specific domain of
global governance. To aid in the selection of datasets and the use of
data within datacubes, the `compare_` functions in `{manydata}` allows
users to quickly compare different information on datacubes and/or
datasets across ‘many packages’. These include comparison for data
observations, variables, and ranges, overlap among observations, missing
observations, and conflicts among observations.

For now, let’s work with the Roman Emperors datacube included in
manydata. We can get a quick summary of the datasets included in this
package with the following command:

``` r
data(package = "manydata")
data(emperors, package = "manydata")
emperors
```

We can see that there are three named datasets relating to emperors
here: `wikipedia` (dataset assembled from Wikipedia pages), `UNVR`
(United Nations of Roman Vitrix), and `britannica` (Britannica
Encyclopedia List of Roman Emperors). Each of these datasets has their
advantages and so we may wish to understand their similarities and
differences, summarise variables across them, and perhaps also rerun
models across them.

The `compare_dimensions()` function returns a tibble with the
observations and variables of each dataset within the specified datacube
of a many package.

``` r
compare_dimensions(emperors)
```

    #> # A tibble: 3 × 5
    #>   Dataset    Observations Variables                    Earliest_Date Latest_Date
    #>   <chr>      <chr>        <chr>                        <mdate>       <mdate>    
    #> 1 wikipedia  68           ID, Begin, End, FullName, B… -0026-01-16   0014-08-19 
    #> 2 UNRV       99           ID, Begin, End, Birth, Deat… -0014-01-01   -0027-12-31
    #> 3 britannica 87           ID, Begin, End               -0031-01-01   0014-12-31

The `compare_ranges()` function returns a tibble with the date range
using the earliest and latest dates of each dataset within the specified
datacube of a many package.

``` r
compare_ranges(emperors, variable = c("Begin", "End"))
```

    #> # A tibble: 6 × 6
    #>   Dataset    Variable Min        Max        Mean       Median    
    #>   <chr>      <chr>    <chr>      <chr>      <chr>      <chr>     
    #> 1 wikipedia  Begin    -026-01-16 -026-01-16 -026-01-16 -026-01-16
    #> 2 wikipedia  End      0014-08-19 0014-08-19 0014-08-19 0014-08-19
    #> 3 UNRV       Begin    -027-01-01 -027-12-31 -027-07-02 -027-07-02
    #> 4 UNRV       End      -014-01-01 -014-12-31 -014-07-02 -014-07-02
    #> 5 britannica Begin    -031-01-01 -031-12-31 -031-07-02 -031-07-02
    #> 6 britannica End      0014-01-01 0014-12-31 0014-07-02 0014-07-02

The `compare_overlap()` function returns a tibble with the number of
overlapping observations for a specified variable (specify using the
`key` argument) across datasets within the datacube.

``` r
plot(compare_overlap(emperors, key = "ID"))
```

<img src="man/figures/README-overlap-1.png" width="100%" />

The `compare_missing()` function returns a tibble with the number and
percentage of missing observations in datasets within datacube.

``` r
plot(compare_missing(emperors))
```

<img src="man/figures/README-missing-1.png" width="100%" />

Finally, the `compare_categories()` function help researchers identify
how variables across datasets within a datacube relate to one another in
five categories. Observations are matched by an “ID” variable to
facilitate comparison. The categories here include ‘confirmed’,
‘majority’, ‘unique’, ‘missing’, and ‘conflict’. Observations are
‘confirmed’ if all non-NA values are the same across all datasets, and
‘majority’ if the non-NA values are the same across most datasets.
‘Unique’ observations are present in only one dataset and ‘missing’
observations indicate there are no non-NA values across all datasets for
that variable. Observations are in ‘conflict’ if datasets have different
non-NA values.

``` r
plot(compare_categories(emperors, key = "ID"))
```

    #> There were 116 matched observations by ID variable across datasets in datacube.

<img src="man/figures/README-categories-1.png" width="100%" />

## Consolidating ‘many’ data

To retrieve an individual dataset from this datacube, we can use the
`pluck()` function.

``` r
pluck(emperors, "wikipedia")
```

However, the real value of the various ‘many packages’ is that multiple
datasets relating to the same phenomenon are presented together.
`{manydata}` contains flexible methods for consolidating the different
datasets in a datacube into a single dataset. For example, you could
have the rows (observations) from one dataset, but add on some columns
(variables) from another dataset. Where there are conflicts in the
values across the different datasets, there are several ways that these
may be resolved.

The `consolidate()` function facilitates consolidating a set of
datasets, or a datacube, from a ‘many’ package into a single dataset
with some combination of the rows and columns. The function includes
separate arguments for rows and columns, as well as for how to resolve
conflicts in observations across datasets. The key argument indicates
the column to collapse datasets by. This provides users with
considerable flexibility in how they combine data.

For example, users may wish to see units and variables coded in “any”
dataset (i.e. units or variables present in at least one of the datasets
in the datacube) or units and variables coded in “every” dataset
(i.e. units or variables present in all of the datasets in the
datacube).

``` r
consolidate(datacube = emperors, rows = "any", cols = "any",
            resolve = "coalesce", key = "ID")
```

    #> There were 116 matched observations by ID variable across datasets in datacube.

    #> # A tibble: 138 × 15
    #>    ID         CityBirth ProvinceBirth Rise  Cause Killer Era   Notes Verif Birth
    #>    <chr>      <chr>     <chr>         <chr> <chr> <chr>  <chr> <chr> <chr> <chr>
    #>  1 Aemilian   <NA>      Africa        Appo… Assa… Other… Prin… birt… <NA>  0207…
    #>  2 Allectus   <NA>      <NA>          <NA>  <NA>  <NA>   <NA>  <NA>  <NA>  ?    
    #>  3 Anastasius <NA>      <NA>          <NA>  <NA>  <NA>   <NA>  <NA>  <NA>  430  
    #>  4 Anthemius  <NA>      <NA>          <NA>  <NA>  <NA>   <NA>  <NA>  <NA>  420  
    #>  5 Antoninus… <NA>      <NA>          <NA>  <NA>  <NA>   <NA>  <NA>  <NA>  86   
    #>  6 Antonius … Lanuvium  Italia        Birt… Natu… Disea… Prin… <NA>  <NA>  0086…
    #>  7 Arcadius   <NA>      <NA>          <NA>  <NA>  <NA>   <NA>  <NA>  <NA>  377  
    #>  8 Augustus   Rome      Italia        Birt… Assa… Wife   Prin… birt… Redd… 0062…
    #>  9 Aulus Vit… <NA>      <NA>          <NA>  <NA>  <NA>   <NA>  <NA>  <NA>  <NA> 
    #> 10 Aurelian   Sirmium   Pannonia      Appo… Assa… Praet… Prin… <NA>  <NA>  0214…
    #> # ℹ 128 more rows
    #> # ℹ 5 more variables: Death <chr>, FullName <chr>, Dynasty <chr>,
    #> #   Begin <mdate>, End <mdate>

``` r
consolidate(datacube = emperors, rows = "every", cols = "every",
            resolve = "coalesce", key = "ID")
```

    #> There were 116 matched observations by ID variable across datasets in datacube.

    #> # A tibble: 41 × 3
    #>    ID             Begin       End        
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

Users can also choose how they want to resolve conflicts between
observations in `consolidate()` with several ‘resolve’ methods:

- coalesce: the first non-NA value
- max: the largest value
- min: the smallest value
- mean: the average value
- median: the median value
- random: a random value

``` r
consolidate(datacube = emperors, rows = "any", cols = "every", resolve = "max", key = "ID")
```

    #> There were 116 matched observations by ID variable across datasets in datacube.

    #> # A tibble: 138 × 3
    #>    ID              Begin      End       
    #>    <chr>           <chr>      <chr>     
    #>  1 Aemilian        0253-12-31 0253-12-31
    #>  2 Allectus        0293       0297      
    #>  3 Anastasius      0491       0518      
    #>  4 Anthemius       0467       0472      
    #>  5 Antoninus Pius  0138       0161      
    #>  6 Antonius Pius   0138-07-10 0161-03-07
    #>  7 Arcadius        0395       0408      
    #>  8 Augustus        -031-12-31 0014-12-31
    #>  9 Aulus Vitellius 0069-07    0069-12   
    #> 10 Aurelian        0270-12-31 0275-12-31
    #> # ℹ 128 more rows

``` r
consolidate(datacube = emperors, rows = "every", cols = "any", resolve = "min", key = "ID")
```

    #> There were 116 matched observations by ID variable across datasets in datacube.

    #> # A tibble: 41 × 15
    #>    ID         CityBirth ProvinceBirth Rise  Cause Killer Era   Notes Verif Birth
    #>    <chr>      <chr>     <chr>         <chr> <chr> <chr>  <chr> <chr> <chr> <chr>
    #>  1 Aemilian   <NA>      Africa        Appo… Assa… Other… Prin… birt… <NA>  0207…
    #>  2 Augustus   Rome      Italia        Birt… Assa… Wife   Prin… birt… Redd… 0062…
    #>  3 Aurelian   Sirmium   Pannonia      Appo… Assa… Praet… Prin… <NA>  <NA>  0214…
    #>  4 Balbinus   <NA>      Unknown       Appo… Assa… Praet… Prin… birt… <NA>  0178…
    #>  5 Caracalla  Lugdunum  Gallia Lugdu… Birt… Assa… Other… Prin… reig… <NA>  0188…
    #>  6 Carinus    <NA>      Unknown       Birt… Died… Oppos… Prin… deat… <NA>  ?    
    #>  7 Carus      Narbo     Gallia Narbo… Seiz… Natu… Light… Prin… birt… <NA>  0230…
    #>  8 Claudius   Lugdunum  Gallia Lugdu… Birt… Assa… Wife   Prin… birt… Redd… 0009…
    #>  9 Commodus   Lanuvium  Italia        Birt… Assa… Praet… Prin… reig… <NA>  0161…
    #> 10 Constanti… Arelate   Gallia Narbo… Birt… Exec… Other… Domi… birt… <NA>  0316…
    #> # ℹ 31 more rows
    #> # ℹ 5 more variables: Death <chr>, FullName <chr>, Dynasty <chr>, Begin <chr>,
    #> #   End <chr>

``` r
consolidate(datacube = emperors, rows = "every", cols = "every", resolve = "mean", key = "ID")
```

    #> There were 116 matched observations by ID variable across datasets in datacube.

    #> # A tibble: 41 × 3
    #>    ID             Begin       End        
    #>    <chr>          <chr>       <chr>      
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

``` r
consolidate(datacube = emperors, rows = "any", cols = "any", resolve = "median", key = "ID")
```

    #> There were 116 matched observations by ID variable across datasets in datacube.

    #> # A tibble: 138 × 15
    #>    ID         CityBirth ProvinceBirth Rise  Cause Killer Era   Notes Verif Birth
    #>    <chr>      <chr>     <chr>         <chr> <chr> <chr>  <chr> <chr> <chr> <chr>
    #>  1 Aemilian   <NA>      Africa        Appo… Assa… Other… Prin… birt… <NA>  0207…
    #>  2 Allectus   <NA>      <NA>          <NA>  <NA>  <NA>   <NA>  <NA>  <NA>  ?    
    #>  3 Anastasius <NA>      <NA>          <NA>  <NA>  <NA>   <NA>  <NA>  <NA>  430  
    #>  4 Anthemius  <NA>      <NA>          <NA>  <NA>  <NA>   <NA>  <NA>  <NA>  420  
    #>  5 Antoninus… <NA>      <NA>          <NA>  <NA>  <NA>   <NA>  <NA>  <NA>  86   
    #>  6 Antonius … Lanuvium  Italia        Birt… Natu… Disea… Prin… <NA>  <NA>  0086…
    #>  7 Arcadius   <NA>      <NA>          <NA>  <NA>  <NA>   <NA>  <NA>  <NA>  377  
    #>  8 Augustus   Rome      Italia        Birt… Assa… Wife   Prin… birt… Redd… 0062…
    #>  9 Aulus Vit… <NA>      <NA>          <NA>  <NA>  <NA>   <NA>  <NA>  <NA>  <NA> 
    #> 10 Aurelian   Sirmium   Pannonia      Appo… Assa… Praet… Prin… <NA>  <NA>  0214…
    #> # ℹ 128 more rows
    #> # ℹ 5 more variables: Death <chr>, FullName <chr>, Dynasty <chr>, Begin <chr>,
    #> #   End <chr>

``` r
consolidate(datacube = emperors, rows = "every", cols = "every", resolve = "random", key = "ID")
```

    #> There were 116 matched observations by ID variable across datasets in datacube.

    #> # A tibble: 41 × 3
    #>    ID             Begin      End       
    #>    <chr>          <chr>      <chr>     
    #>  1 Aemilian       0253-12-31 0253-12-31
    #>  2 Augustus       -031-12-31 -014-12-31
    #>  3 Aurelian       0270-12-31 0275-09-15
    #>  4 Balbinus       0238-04-22 0238-07-29
    #>  5 Caracalla      0198-12-31 0217-12-31
    #>  6 Carinus        0283-12-31 0285-12-31
    #>  7 Carus          0282-12-31 0283-12-31
    #>  8 Claudius       0041-12-31 0054-10-13
    #>  9 Commodus       0177-12-31 0192-12-31
    #> 10 Constantine II 0337-05-22 0340-01-01
    #> # ℹ 31 more rows

Users can even specify how conflicts for different variables should be
‘resolved’:

``` r
consolidate(datacube = emperors, rows = "any", cols = "every", resolve = c(Begin = "min", End = "max"), key = "ID")
```

    #> There were 116 matched observations by ID variable across datasets in datacube.

    #> # A tibble: 138 × 3
    #>    ID              Begin      End       
    #>    <chr>           <chr>      <chr>     
    #>  1 Aemilian        0253-01-01 0253-12-31
    #>  2 Allectus        0293       0297      
    #>  3 Anastasius      0491       0518      
    #>  4 Anthemius       0467       0472      
    #>  5 Antoninus Pius  0138       0161      
    #>  6 Antonius Pius   0138-07-10 0161-03-07
    #>  7 Arcadius        0395       0408      
    #>  8 Augustus        -026-01-16 0014-12-31
    #>  9 Aulus Vitellius 0069-07    0069-12   
    #> 10 Aurelian        0270-01-01 0275-12-31
    #> # ℹ 128 more rows

Alternatively, users can “favour” a dataset in a datacube over others:

``` r
consolidate(datacube = favour(emperors, "UNRV"), rows = "every", cols = "any", resolve = "coalesce", key = "ID")
```

    #> There were 116 matched observations by ID variable across datasets in datacube.

    #> # A tibble: 41 × 15
    #>    ID    FullName Birth Death CityBirth ProvinceBirth Rise  Cause Killer Dynasty
    #>    <chr> <chr>    <chr> <chr> <chr>     <chr>         <chr> <chr> <chr>  <chr>  
    #>  1 Aemi… "Marcus… 207?  253   <NA>      Africa        Appo… Assa… Other… Gordian
    #>  2 Augu… "Gaius … 63 BC 14    Rome      Italia        Birt… Assa… Wife   Julio-…
    #>  3 Aure… "Lucius… 214   275   Sirmium   Pannonia      Appo… Assa… Praet… Gordian
    #>  4 Balb… "Decimu… 170?  238   <NA>      Unknown       Appo… Assa… Praet… Gordian
    #>  5 Cara… "born L… 188   217   Lugdunum  Gallia Lugdu… Birt… Assa… Other… Severan
    #>  6 Cari… "Marcus… ?     285   <NA>      Unknown       Birt… Died… Oppos… co-emp…
    #>  7 Carus "Marcus… 230?  283   Narbo     Gallia Narbo… Seiz… Natu… Light… .      
    #>  8 Clau… "Tiberi… 10 BC 41    Lugdunum  Gallia Lugdu… Birt… Assa… Wife   Julio-…
    #>  9 Comm… "Marcus… 161   192   Lanuvium  Italia        Birt… Assa… Praet… Adopti…
    #> 10 Cons… "Flaviu… 317   340   Arelate   Gallia Narbo… Birt… Exec… Other… House …
    #> # ℹ 31 more rows
    #> # ℹ 5 more variables: Era <chr>, Notes <chr>, Verif <chr>, Begin <mdate>,
    #> #   End <mdate>

Users can, even, declare multiple key ID columns to consolidate a
datacube or multiple datasets:

``` r
consolidate(datacube = emperors, rows = "any", cols = "any", resolve = c(Death = "max", Cause = "coalesce"),
            key = c("ID", "Begin"))
```

    #> # A tibble: 202 × 4
    #>    ID             Begin       Cause          Death      
    #>    <chr>          <mdate>     <chr>          <chr>      
    #>  1 Aemilian       0253        <NA>           253        
    #>  2 Aemilian       0253-08-15~ Assassination  0253-10-15~
    #>  3 Allectus       0293        <NA>           297        
    #>  4 Anastasius     0491        <NA>           518        
    #>  5 Anthemius      0467        <NA>           472        
    #>  6 Antoninus Pius 0138        <NA>           161        
    #>  7 Antonius Pius  0138-07-10  Natural Causes 0161-03-07 
    #>  8 Arcadius       0383        <NA>           <NA>       
    #>  9 Arcadius       0395        <NA>           408        
    #> 10 Augustus       -0026-01-16 Assassination  0014-08-19 
    #> # ℹ 192 more rows

## Cheatsheet

Please see the cheat sheet below for a quick overview:

<a href="https://github.com/globalgov/manydata/blob/develop/man/figures/cheatsheet.pdf"><img src="https://raw.githubusercontent.com/globalgov/manydata/develop/man/figures/cheatsheet.png" width="525" height="378"/></a>

## Contributing to the many packages universe

For more information for developers and data contributors to ‘many
packages’, please see `{manypkgs}` [the
website](https://globalgov.github.io/manypkgs/).

## Funding details

Development on this package has been funded by the Swiss National
Science Foundation (SNSF) [Grant Number
188976](https://data.snf.ch/grants/grant/188976): “Power and Networks
and the Rate of Change in Institutional Complexes” (PANARCHIC).
