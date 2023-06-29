
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

`{manydata}` is a portal to many packages containing many databases
(each containing many related datasets) on many issue-domains, actors
and institutions of global governance. This package contains tools for:

- *calling* data packages and databases,
- *comparing* individual datasets, and
- *consolidating* databases in different ways.

`{manydata}` connects users to other packages that help fill global
governance researchers’ data needs. The easiest way to install
`{manydata}` is directly from CRAN.

``` r
install.packages("manydata")
```

The development version of the package `{manydata}` can also be
downloaded from GitHub.

``` r
# install.packages("remotes")
remotes::install_github("globalgov/manydata")
```

We intend for `{manydata}` to be useful:

- at the **start** of a research project, to access and gather recent
  versions of well-regarded datasets, see what is available, describe,
  and explore the data,
- in the **middle** of a project, to facilitate analysis, comparison and
  modelling, and
- at the **end** of the project, to help with conducting robustness
  checks, preparing replication scripts, and writing the next grant
  application.

Please see the vignette below for a quick overview:

<a href="https://github.com/globalgov/manydata/blob/develop/man/figures/cheatsheet.pdf"><img src="https://raw.githubusercontent.com/globalgov/manydata/develop/man/figures/cheatsheet.png" width="525" height="378"/></a>

## Call ‘many’ packages

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
call_sources(package = "manydata", database = "emperors")
```

    #> # A tibble: 3 × 4
    #>   Dataset    Source                                                URL   Mapping
    #>   <chr>      <chr>                                                 <chr> <chr>  
    #> 1 wikipedia  Wikipedia, List_of_Roman_emperors, https://en.wikipe… http… from -…
    #> 2 UNRV       UNRV, Roman Emperor list, https://www.unrv.com/gover… http… from -…
    #> 3 britannica Britannica, List of Roman emperors, https://www.brit… http… from -…

## Comparing ‘many’ datasets

Packages in the many packages universe have the advantage to facilitate
comparison and analysis of multiple datasets in a specific domain of
global governance.

For now, let’s work with the Roman Emperors database included in
manydata. We can get a quick summary of the datasets included in this
package with the following command:

``` r
data(package = "manydata")
data(emperors, package = "manydata")
emperors
```

    #> $wikipedia
    #> # A tibble: 68 × 15
    #>    ID       Begin End   FullName Birth Death CityBirth ProvinceBirth Rise  Cause
    #>    <chr>    <mda> <mda> <chr>    <chr> <chr> <chr>     <chr>         <chr> <chr>
    #>  1 Augustus -002… 0014… IMPERAT… 0062… 0014… Rome      Italia        Birt… Assa…
    #>  2 Tiberius 0014… 0037… TIBERIV… 0041… 0037… Rome      Italia        Birt… Assa…
    #>  3 Caligula 0037… 0041… GAIVS I… 0012… 0041… Antitum   Italia        Birt… Assa…
    #>  4 Claudius 0041… 0054… TIBERIV… 0009… 0054… Lugdunum  Gallia Lugdu… Birt… Assa…
    #>  5 Nero     0054… 0068… NERO CL… 0037… 0068… Antitum   Italia        Birt… Suic…
    #>  6 Galba    0068… 0069… SERVIVS… 0002… 0069… Terracina Italia        Seiz… Assa…
    #>  7 Otho     0069… 0069… MARCVS … 0032… 0069… Terentin… Italia        Appo… Suic…
    #>  8 Vitelli… 0069… 0069… AVLVS V… 0015… 0069… Rome      Italia        Seiz… Assa…
    #>  9 Vespasi… 0069… 0079… TITVS F… 0009… 0079… Falacrine Italia        Seiz… Natu…
    #> 10 Titus    0079… 0081… TITVS F… 0039… 0081… Rome      Italia        Birt… Natu…
    #> # ℹ 58 more rows
    #> # ℹ 5 more variables: Killer <chr>, Dynasty <chr>, Era <chr>, Notes <chr>,
    #> #   Verif <chr>
    #> 
    #> $UNRV
    #> # A tibble: 99 × 7
    #>    ID               Begin   End     Birth Death FullName                 Dynasty
    #>    <chr>            <mdate> <mdate> <chr> <chr> <chr>                    <chr>  
    #>  1 Augustus         -0027   -0014   63 BC 14    Gaius Julius Caesar Oct… Julio-…
    #>  2 Tiberius         -0014   0037    42 BC 37    Tiberius Claudius Nero … Julio-…
    #>  3 Gaius (Caligula) 0037    0041    12    14    Gaius Caesar Germanicus… Julio-…
    #>  4 Claudius         0041    0054    10 BC 41    Tiberius Claudius Nero … Julio-…
    #>  5 Nero             0054    0068    37    68    Claudius Nero Caesar (b… Julio-…
    #>  6 Galba            0068    0069    3 BC  69    Servius Sulpicius Galba… Year o…
    #>  7 Otho             0069    0069    32    69    Marcus Salvius Otho / I… Year o…
    #>  8 Vitellius        0069    0069    15    69    Aulus Vitellius / Aulus… Year o…
    #>  9 Vespasian        0069    0079    9     79    Titus Flavius Vespasian… Year o…
    #> 10 Titus            0079    0081    39    79    Titus Flavius Vespasian… Flavian
    #> # ℹ 89 more rows
    #> 
    #> $britannica
    #> # A tibble: 87 × 3
    #>    ID              Begin   End    
    #>    <chr>           <mdate> <mdate>
    #>  1 Augustus        -0031   0014   
    #>  2 Tiberius        0014    0037   
    #>  3 Caligula        0037    0041   
    #>  4 Claudius        0041    0054   
    #>  5 Nero            0054    0068   
    #>  6 Galba           0068    0069   
    #>  7 Otho            0069-01 0069-04
    #>  8 Aulus Vitellius 0069-07 0069-12
    #>  9 Vespasian       0069    0079   
    #> 10 Titus           0079    0081   
    #> # ℹ 77 more rows

We can see that there are three named datasets relating to emperors
here: `wikipedia` (dataset assembled from Wikipedia pages), `UNVR`
(United Nations of Roman Vitrix), and `britannica` (Britannica
Encyclopedia List of Roman Emperors). Each of these datasets has their
advantages and so we may wish to understand their differences, summarise
variables across them, and perhaps also rerun models across them.

To retrieve an individual dataset from this database, we can use the
`pluck()` function.

``` r
wikipedia <- pluck(emperors, "wikipedia")
```

However, the real value of the various ‘many packages’ is that multiple
datasets relating to the same phenomenon are presented together.

The `compare_` functions in `{manydata}` allows users to quickly compare
different information on databases and/or datasets across ‘many
packages’. These include comparison for data summaries, missing
observations, overlap, and categories.

The `compare_data()` function returns a tibble with the key metadata of
each dataset within the specified database of a many package.

``` r
compare_data(emperors)
```

    #> # A tibble: 3 × 5
    #>   Dataset    Observations Variables Earliest_Date Latest_Date
    #>   <chr>      <chr>        <chr>     <chr>         <chr>      
    #> 1 wikipedia  68           15        -0026-01-16   0014-08-19 
    #> 2 UNRV       99           7         -0014-01-01   -0027-12-31
    #> 3 britannica 87           3         -0031-01-01   0014-12-31

The `compare_overlap()` function returns a tibble with the number of
overlapping observations for a specified variable (specify using the
`key` argument) across datasets within the database. Most of the
`compare_` functions are usually accompanied by an appropriate plotting
method that allows users to visualize the comparisons.

``` r
compare_overlap(emperors, key = "ID")
```

    #> # A tibble: 7 × 2
    #>   `Datasets from emperors`    `Overlapping Observations by ID`
    #>   <chr>                                                  <int>
    #> 1 wikipedia                                                 13
    #> 2 UNRV                                                      31
    #> 3 britannica                                                20
    #> 4 wikipedia..UNRV                                            7
    #> 5 wikipedia..britannica                                      7
    #> 6 UNRV..britannica                                          19
    #> 7 wikipedia..UNRV..britannica                               41

``` r
plot(compare_overlap(emperors, key = "ID"))
```

<img src="man/figures/README-overlap-1.png" width="100%" />

The `compare_missing()` function returns a tibble with the number and
percentage of missing observations in datasets within database.

``` r
compare_missing(emperors)
```

    #> # A tibble: 25 × 6
    #>    Variable  Dataset    Class     Count Missing `Percent Missing`
    #>    <chr>     <chr>      <chr>     <dbl>   <dbl>             <dbl>
    #>  1 Begin     wikipedia  mdate        68       0              0   
    #>  2 Begin     UNRV       mdate        99       0              0   
    #>  3 Begin     britannica mdate        87       0              0   
    #>  4 Birth     wikipedia  character    68       5              7.35
    #>  5 Birth     UNRV       character    99       0              0   
    #>  6 Cause     wikipedia  character    68       0              0   
    #>  7 CityBirth wikipedia  character    68      17             25   
    #>  8 Dataset   wikipedia  character    68       0              0   
    #>  9 Dataset   UNRV       character    99       0              0   
    #> 10 Dataset   britannica character    87       0              0   
    #> # ℹ 15 more rows

``` r
plot(compare_missing(emperors))
```

<img src="man/figures/README-missing-1.png" width="100%" />

Finally, the `compare_categories()` function help researchers identify
how variables across datasets within a database relate to one another.
Observations are matched by an “ID” variable to facilitate comparison.
The categories here include ‘confirmed’, ‘majority’, ‘unique’,
‘missing’, and ‘conflict’. Observations are ‘confirmed’ if all non-NA
values are the same across all datasets, and ‘majority’ if the non-NA
values are the same across most datasets. ‘Unique’ observations are
present in only one dataset and ‘missing’ observations indicate there
are no non-NA values across all datasets for that variable. Observations
are in ‘conflict’ if datasets have different non-NA values.

``` r
compare_categories(emperors, key = "ID")
```

    #> There were 116 matched observations by ID variable across datasets in database.

    #> # A tibble: 139 × 37
    #>    ID        `wikipedia$Begin` `UNRV$Begin` `britannica$Begin` `Begin (3)`
    #>    <chr>     <mdate>           <mdate>      <mdate>            <chr>      
    #>  1 Augustus  -0026-01-16       -0027        -0031              conflict   
    #>  2 Tiberius  0014-09-18        -0014        0014               conflict   
    #>  3 Caligula  0037-03-18        NA           0037               conflict   
    #>  4 Claudius  0041-01-25        0041         0041               majority   
    #>  5 Nero      0054-10-13        0054         0054               majority   
    #>  6 Galba     0068-06-08        0068         0068               majority   
    #>  7 Otho      0069-01-15        0069         0069-01            conflict   
    #>  8 Vitellius 0069-04-17        0069         NA                 conflict   
    #>  9 Vespasian 0069-12-21        0069         0069               majority   
    #> 10 Titus     0079-06-24        0079         0079               majority   
    #> # ℹ 129 more rows
    #> # ℹ 32 more variables: `wikipedia$End` <mdate>, `UNRV$End` <mdate>,
    #> #   `britannica$End` <mdate>, `End (3)` <chr>, `wikipedia$FullName` <chr>,
    #> #   `UNRV$FullName` <chr>, `FullName (2)` <chr>, `wikipedia$Birth` <chr>,
    #> #   `UNRV$Birth` <chr>, `Birth (2)` <chr>, `wikipedia$Death` <chr>,
    #> #   `UNRV$Death` <chr>, `Death (2)` <chr>, `wikipedia$CityBirth` <chr>,
    #> #   `CityBirth (1)` <chr>, `wikipedia$ProvinceBirth` <chr>, …

``` r
plot(compare_categories(emperors, key = "ID"))
```

    #> There were 116 matched observations by ID variable across datasets in database.

<img src="man/figures/README-categories-1.png" width="100%" />

## Consolidating ‘many’ databases

`{manydata}` also contains flexible methods for consolidating the
different datasets in a database into a single dataset. For example, you
could have the rows (observations) from one dataset, but add on some
columns (variables) from another dataset. Where there are conflicts in
the values across the different datasets, there are several ways that
these may be resolved.

The `consolidate()` function facilitates consolidating a set of
datasets, or a database, from a ‘many’ package into a single dataset
with some combination of the rows and columns. The function includes
separate arguments for rows and columns, as well as for how to resolve
conflicts in observations across datasets. The key argument indicates
the column to collapse datasets by. This provides users with
considerable flexibility in how they combine data.

For example, users may wish to see units and variables coded in “any”
dataset (i.e. units or variables present in at least one of the datasets
in the database) or units and variables coded in “every” dataset
(i.e. units or variables present in all of the datasets in the
database).

``` r
consolidate(database = emperors, rows = "any", cols = "any",
            resolve = "coalesce", key = "ID")
```

    #> There were 116 matched observations by ID variable across datasets in database.

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
consolidate(database = emperors, rows = "every", cols = "every",
            resolve = "coalesce", key = "ID")
```

    #> There were 116 matched observations by ID variable across datasets in database.

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
consolidate(database = emperors, rows = "any", cols = "every", resolve = "max", key = "ID")
```

    #> There were 116 matched observations by ID variable across datasets in database.

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
consolidate(database = emperors, rows = "every", cols = "any", resolve = "min", key = "ID")
```

    #> There were 116 matched observations by ID variable across datasets in database.

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
consolidate(database = emperors, rows = "every", cols = "every", resolve = "mean", key = "ID")
```

    #> There were 116 matched observations by ID variable across datasets in database.

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
consolidate(database = emperors, rows = "any", cols = "any", resolve = "median", key = "ID")
```

    #> There were 116 matched observations by ID variable across datasets in database.

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
consolidate(database = emperors, rows = "every", cols = "every", resolve = "random", key = "ID")
```

    #> There were 116 matched observations by ID variable across datasets in database.

    #> # A tibble: 41 × 3
    #>    ID             Begin      End       
    #>    <chr>          <chr>      <chr>     
    #>  1 Aemilian       0253-12-31 0253-12-31
    #>  2 Augustus       -026-01-16 0014-12-31
    #>  3 Aurelian       0270-12-31 0275-09-15
    #>  4 Balbinus       0238-12-31 0238-07-29
    #>  5 Caracalla      0211-12-31 0217-04-08
    #>  6 Carinus        0283-08-01 0285-08-01
    #>  7 Carus          0282-12-31 0283-08-01
    #>  8 Claudius       0041-01-25 0054-12-31
    #>  9 Commodus       0180-12-31 0192-12-31
    #> 10 Constantine II 0337-12-31 0340-12-31
    #> # ℹ 31 more rows

Users can even specify how conflicts for different variables should be
‘resolved’:

``` r
consolidate(database = emperors, rows = "any", cols = "every", resolve = c(Begin = "min", End = "max"), key = "ID")
```

    #> There were 116 matched observations by ID variable across datasets in database.

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

Alternatively, users can “favour” a dataset in a database over others:

``` r
consolidate(database = favour(emperors, "UNRV"), rows = "every", cols = "any", resolve = "coalesce", key = "ID")
```

    #> There were 116 matched observations by ID variable across datasets in database.

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

## Contributing to the many packages universe

For more information for developers and data contributors to ‘many
packages’, please see `{manypkgs}` [the
website](https://globalgov.github.io/manypkgs/).

## Funding details

Development on this package has been funded by the Swiss National
Science Foundation (SNSF) [Grant Number
188976](https://data.snf.ch/grants/grant/188976): “Power and Networks
and the Rate of Change in Institutional Complexes” (PANARCHIC).
