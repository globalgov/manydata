# Consolidate datacube into a single dataset

This function consolidates a set of datasets in a 'many\*' package
datacube into a single dataset with some combination of the rows,
columns, and observations of the datasets in the datacube.

## Usage

``` r
consolidate(
  datacube,
  join = c("full", "inner", "left"),
  resolve = "coalesce",
  key = NULL
)
```

## Arguments

- datacube:

  A datacube from one of the many packages

- join:

  Which join procedure to use. By default "full" so that all
  observations are retained, but other options include "left" for basing
  the consolidated dataset on observations present in the first dataset
  (reorder the datasets to favour another dataset), and "inner" for a
  consolidated dataset that includes only observations that are present
  in all datasets.

- resolve:

  Choice how (potentially conflicting) values from shared variables
  should be resolved. Options include:

  - "coalesce" (default): uses first non-NA value (if available) for
    each observation, essentially favouring the order the datasets are
    in in the datacube.

  - "unite": combines the unique values for each observation across
    datasets as a set (separated by commas and surrounded by braces),
    which can be useful for retaining information.

  - "random": selects values at random from among the observations from
    each dataset that observed that variable, of particular use for
    exploring the implications of dataset-related variation.

  - "precise": selects the value that has the highest precision from
    among the observations from each dataset (see
    `resolving_precision()`), which favours more precise data.

  - "min", "max": these options return the minimum or maximum values
    respectively, which can be useful for conservative temporal fixing.

  To resolve variables by different functions, pass the argument a
  vector (e.g. `resolve = c(var1 = "min", var2 = "max")`). Unnamed
  variables will be resolved according to the default ("coalesce").

- key:

  An ID column to collapse by. By default "manyID". Users can also
  specify multiple key variables in a list. For multiple key variables,
  the key variables must be present in all the datasets in the datacube
  (e.g. `key = c("key1", "key2")`). For equivalent key columns with
  different names across datasets, matching is possible if keys are
  declared (e.g. `key = c("key1" = "key2")`). Missing observations in
  the key variable are removed.

## Value

A single tibble/data frame.

## Details

The function includes separate arguments for the rows and columns, as
well as for how to resolve conflicts for observations across datasets.
This provides users with considerable flexibility in how they combine
data. For example, users may wish to stick to units that appear in every
dataset but include variables coded in any dataset, or units that appear
in any dataset but only those variables that appear in every dataset.
Even then there may be conflicts, as the actual unit-variable
observations may differ from dataset to dataset. We offer a number of
resolve methods that enable users to choose how conflicts between
observations are resolved.

Text variables are dropped for more efficient consolidation.

## Examples

``` r
# \donttest{
consolidate(emperors, join = "full", resolve = "coalesce", key = "ID")
#> Using `ID` for matching observations across datasets...
#> ✔ Matched 151 observations by `ID` variable in `emperors` datasets.
#> Using `ID` for matching observations across datasets...

#> Joining 3 datasets using a full join...
#> ✔ Joined 3 datasets using a full join.
#> Joining 3 datasets using a full join...

#> Found 12 conflicts to resolve by `coalesce()`...
#> ✔ Resolved 3 related variables into `Begin`.
#> Found 12 conflicts to resolve by `coalesce()`...

#> ✔ Resolved 3 related variables into `End`.
#> Found 12 conflicts to resolve by `coalesce()`...

#> ✔ Resolved 2 related variables into `FullName`.
#> Found 12 conflicts to resolve by `coalesce()`...

#> ✔ Resolved 2 related variables into `Birth`.
#> Found 12 conflicts to resolve by `coalesce()`...

#> ✔ Resolved 2 related variables into `Death`.
#> Found 12 conflicts to resolve by `coalesce()`...

#> ✔ Resolved 2 related variables into `Dynasty`.
#> Found 12 conflicts to resolve by `coalesce()`...

#> # A tibble: 103 × 14
#>    ID       Begin End   FullName Birth Death CityBirth ProvinceBirth Rise  Cause
#>    <chr>    <mda> <mda> <chr>    <mda> <mda> <chr>     <chr>         <chr> <chr>
#>  1 Aemilian 0253… 0253… Caesar … 0207… 0253… NA        Africa        Appo… Assa…
#>  2 Allectus 0293… 0297… ?        NA  … 0297… NA        NA            NA    NA   
#>  3 Anastas… 0491… 0518… Flavius… 0430… 0518… NA        NA            NA    NA   
#>  4 Anthemi… 0467… 0472… Procopi… 0420… 0472… NA        NA            NA    NA   
#>  5 Antonin… 0138… 0161… Caesar … 0086… 0161… Lanuvium  Italia        Birt… Natu…
#>  6 Arcadius 0395… 0408… Flavius… 0377… 0408… NA        NA            NA    NA   
#>  7 Augustus -002… 0014… Imperat… -006… 0014… Rome      Italia        Birt… Assa…
#>  8 Aurelian 0270… 0275… Caesar … 0214… 0275… Sirmium   Pannonia      Appo… Assa…
#>  9 Avitus   0455… 0456… Marcus … 0395… 0456… NA        NA            NA    NA   
#> 10 Balbinus 0238… 0238… Caesar … 0178… 0238… NA        Unknown       Appo… Assa…
#> # ℹ 93 more rows
#> # ℹ 4 more variables: Killer <chr>, Dynasty <chr>, Era <chr>, Notes <chr>
consolidate(emperors, join = "inner", resolve = "min", key = "ID")
#> Using `ID` for matching observations across datasets...
#> ✔ Matched 151 observations by `ID` variable in `emperors` datasets.
#> Using `ID` for matching observations across datasets...

#> Joining 3 datasets using a inner join...
#> ✔ Joined 3 datasets using a inner join.
#> Joining 3 datasets using a inner join...

#> Found 12 conflicts to resolve by `min()`...
#> ✔ Resolved 3 related variables into `Begin`.
#> Found 12 conflicts to resolve by `min()`...

#> ✔ Resolved 3 related variables into `End`.
#> Found 12 conflicts to resolve by `min()`...

#> ✔ Resolved 2 related variables into `FullName`.
#> Found 12 conflicts to resolve by `min()`...

#> Warning: There were 4 warnings in `mutate()`.
#> The first warning was:
#> ℹ In argument: `Birth = get(funny)(out, all_of(vars))`.
#> Caused by warning in `min()`:
#> ! no non-missing arguments, returning NA
#> ℹ Run `dplyr::last_dplyr_warnings()` to see the 3 remaining warnings.
#> ✔ Resolved 2 related variables into `Birth`.
#> Found 12 conflicts to resolve by `min()`...

#> ✔ Resolved 2 related variables into `Death`.
#> Found 12 conflicts to resolve by `min()`...

#> ✔ Resolved 2 related variables into `Dynasty`.
#> Found 12 conflicts to resolve by `min()`...

#> # A tibble: 65 × 14
#>    ID       Begin End   FullName Birth Death CityBirth ProvinceBirth Rise  Cause
#>    <chr>    <mda> <mda> <chr>    <mda> <mda> <chr>     <chr>         <chr> <chr>
#>  1 Augustus -002… -0014 Gaius J… -006… 0014  Rome      Italia        Birt… Assa…
#>  2 Tiberius 0014… 0037  Tiberiu… -004… 0037  Rome      Italia        Birt… Assa…
#>  3 Caligula 0037… 0041  Gaius C… 0012… 0014  Antitum   Italia        Birt… Assa…
#>  4 Claudius 0041… 0054  Tiberiu… -000… 0041  Lugdunum  Gallia Lugdu… Birt… Assa…
#>  5 Nero     0054… 0068  Claudiu… 0037… 0068  Antitum   Italia        Birt… Suic…
#>  6 Galba    0068… 0069  Servius… -000… 0069  Terracina Italia        Seiz… Assa…
#>  7 Otho     0069… 0069  Marcus … 0032… 0069  Terentin… Italia        Appo… Suic…
#>  8 Vitelli… 0069… 0069  Aulus V… 0015… 0069  Rome      Italia        Seiz… Assa…
#>  9 Vespasi… 0069… 0079  Titus F… 0009… 0079  Falacrine Italia        Seiz… Natu…
#> 10 Titus    0079… 0081  Titus F… 0039… 0079  Rome      Italia        Birt… Natu…
#> # ℹ 55 more rows
#> # ℹ 4 more variables: Killer <chr>, Dynasty <chr>, Era <chr>, Notes <chr>
consolidate(emperors, join = "left", resolve = "max", key = "ID")
#> Using `ID` for matching observations across datasets...
#> ✔ Matched 151 observations by `ID` variable in `emperors` datasets.
#> Using `ID` for matching observations across datasets...

#> Joining 3 datasets using a left join...
#> ✔ Joined 3 datasets using a left join.
#> Joining 3 datasets using a left join...

#> Found 12 conflicts to resolve by `max()`...
#> ✔ Resolved 3 related variables into `Begin`.
#> Found 12 conflicts to resolve by `max()`...

#> ✔ Resolved 3 related variables into `End`.
#> Found 12 conflicts to resolve by `max()`...

#> Warning: There was 1 warning in `mutate()`.
#> ℹ In argument: `FullName = get(funny)(out, all_of(vars))`.
#> Caused by warning in `max()`:
#> ! no non-missing arguments, returning NA
#> ✔ Resolved 2 related variables into `FullName`.
#> Found 12 conflicts to resolve by `max()`...

#> Warning: There were 6 warnings in `mutate()`.
#> The first warning was:
#> ℹ In argument: `Birth = get(funny)(out, all_of(vars))`.
#> Caused by warning in `max()`:
#> ! no non-missing arguments, returning NA
#> ℹ Run `dplyr::last_dplyr_warnings()` to see the 5 remaining warnings.
#> ✔ Resolved 2 related variables into `Birth`.
#> Found 12 conflicts to resolve by `max()`...

#> Warning: There was 1 warning in `mutate()`.
#> ℹ In argument: `Death = get(funny)(out, all_of(vars))`.
#> Caused by warning in `max()`:
#> ! no non-missing arguments, returning NA
#> ✔ Resolved 2 related variables into `Death`.
#> Found 12 conflicts to resolve by `max()`...

#> Warning: There was 1 warning in `mutate()`.
#> ℹ In argument: `Dynasty = get(funny)(out, all_of(vars))`.
#> Caused by warning in `max()`:
#> ! no non-missing arguments, returning NA
#> ✔ Resolved 2 related variables into `Dynasty`.
#> Found 12 conflicts to resolve by `max()`...

#> # A tibble: 69 × 14
#>    ID       Begin End   FullName Birth Death CityBirth ProvinceBirth Rise  Cause
#>    <chr>    <mda> <mda> <chr>    <mda> <mda> <chr>     <chr>         <chr> <chr>
#>  1 Augustus -003… 0014… Imperat… -006… 0014… Rome      Italia        Birt… Assa…
#>  2 Tiberius 0014… 0037… Tiberiv… -004… 0037… Rome      Italia        Birt… Assa…
#>  3 Caligula 0037… 0041… Gaivs I… 0012… 0041… Antitum   Italia        Birt… Assa…
#>  4 Claudius 0041… 0054… Tiberiv… -001… 0054… Lugdunum  Gallia Lugdu… Birt… Assa…
#>  5 Nero     0054… 0068… Nero Cl… 0037… 0068… Antitum   Italia        Birt… Suic…
#>  6 Galba    0068… 0069… Servivs… -000… 0069… Terracina Italia        Seiz… Assa…
#>  7 Otho     0069… 0069… Marcvs … 0032… 0069… Terentin… Italia        Appo… Suic…
#>  8 Vitelli… 0069… 0069… Avlvs V… 0015… 0069… Rome      Italia        Seiz… Assa…
#>  9 Vespasi… 0069… 0079… Titvs F… 0009… 0079… Falacrine Italia        Seiz… Natu…
#> 10 Titus    0079… 0081… Titvs F… 0039… 0081… Rome      Italia        Birt… Natu…
#> # ℹ 59 more rows
#> # ℹ 4 more variables: Killer <chr>, Dynasty <chr>, Era <chr>, Notes <chr>
# }
```
