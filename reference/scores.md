# Scoring Functions for Data Quality Checks

A set of functions to assess various aspects of data quality. including
a comprehensive dataset score as well as individual scores for specific
data quality dimensions such as date consistency, duplicates, recency,
frequency, time, coding, comments, sources, missing values, and
variables.

According to the literature, data quality can be assessed by checking
for consistency, completeness, accuracy, timeliness, and uniqueness of
the data. Consistency means that the data is logically coherent,
completeness means that all required data is present, accuracy means
that the data is correct and reliable, timeliness means that the data is
up-to-date, and uniqueness means that there are no duplicate records.

## Usage

``` r
score_dataset(df)

score_obs_no(df)

score_var_no(df)

score_completeness(df)

score_date_consistency(df)

score_date_scope(df)

score_obs_info(df, id_col = "ID")

score_coding(df)

score_comments(df)

score_var_info(df)
```

## Arguments

- df:

  A data frame to be scored.

- id_col:

  The name of the column containing IDs. Default is "ID".

## Details

These functions are designed to help assess the quality of data in a
data frame. Each function checks a specific aspect of the data and
returns a score or a message indicating the quality of that aspect. The
functions include:

- `score_date_consistency`: Proportion of invalid date pairs (End \<=
  Begin).

- `score_duplicates`: Proportion of duplicate IDs.

## References

Wang, R. Y., & Strong, D. M. (1996). Beyond accuracy: What data quality
means to data consumers. Journal of Management Information Systems,
12(4), 5-34.

## Examples

``` r
score_dataset(emperors)
#> Missing values per variable: FullName: 1, Birth: 6, Death: 1, CityBirth: 18, ProvinceBirth: 1, Rise: 1, Cause: 1, Killer: 1, Dynasty: 1, Era: 1, Notes: 23
#> Missing values per variable: Birth: 24, Death: 1, FullName: 5, Dynasty: 37
#> Missing values per variable: 
#>  Wikipedia       UNRV Britannica 
#>        842        521        174 
score_obs_no(emperors)
#>  Wikipedia       UNRV Britannica 
#>         69         98         87 
score_var_no(emperors)
#>  Wikipedia       UNRV Britannica 
#>         13          6          2 
score_completeness(emperors)
#> Missing values per variable: FullName: 1, Birth: 6, Death: 1, CityBirth: 18, ProvinceBirth: 1, Rise: 1, Cause: 1, Killer: 1, Dynasty: 1, Era: 1, Notes: 23
#> Missing values per variable: Birth: 24, Death: 1, FullName: 5, Dynasty: 37
#> Missing values per variable: 
#>  Wikipedia       UNRV Britannica 
#>  0.9386845  0.8860544  1.0000000 
score_date_consistency(emperors)
#> All date pairs are valid (End > Begin).
#> There are 17 potentially invalid date pairs (End <= Begin). This is 17.35% of the data. Please check the Begin and End dates for correctness.
#>                   ID      Begin        End
#> 7               Otho 0069-12-31 0069-01-01
#> 8          Vitellius 0069-12-31 0069-01-01
#> 19   Didius Julianus 0193-12-31 0193-01-01
#> 20          Pertinax 0193-12-31 0193-01-01
#> 24              Geta 0211-12-31 0211-01-01
#> 30          Balbinus 0238-12-31 0238-01-01
#> 31         Gordian I 0238-12-31 0238-01-01
#> 32        Gordian II 0238-12-31 0238-01-01
#> 33          Pupienus 0238-12-31 0238-01-01
#> 38          Aemilian 0253-12-31 0253-01-01
#> 43         Laelianus 0269-12-31 0269-01-01
#> 44            Marius 0269-12-31 0269-01-01
#> 46        Quintillus 0270-12-31 0270-01-01
#> 50           Florian 0276-12-31 0276-01-01
#> 85 Petronius Maximus 0455-12-31 0455-01-01
#> 91          Olybrius 0472-12-31 0472-01-01
#> 93            Leo II 0474-12-31 0474-01-01
#> There are 5 potentially invalid date pairs (End <= Begin). This is 5.75% of the data. Please check the Begin and End dates for correctness.
#>                 ID      Begin        End
#> 35       Hostilian 0251-12-31 0251-01-01
#> 37        Aemilian 0253-12-31 0253-01-01
#> 41      Quintillus 0270-12-31 0270-01-01
#> 73 Constantius III 0421-12-31 0421-01-01
#> 84          Leo II 0474-12-31 0474-01-01
#>  Wikipedia       UNRV Britannica 
#> 0.00000000 0.17346939 0.05747126 
score_date_scope(emperors)
#> The total time scope is 163493 days, from -26-01-16 to 421-09-02
#> The total time scope is 199421 days, from -27-01-01 to 518-12-31
#> The total time scope is 191021 days, from -31-01-01 to 491-12-31
#>  Wikipedia       UNRV Britannica 
#>     163493     199421     191021 
score_obs_info(emperors)
#> There are no duplicate IDs in the ID column.
#> There are no duplicate IDs in the ID column.
#> There are no duplicate IDs in the ID column.
#> There are no duplicate IDs in the ID column.
#> There are no duplicate IDs in the ID column.
#> There are no duplicate IDs in the ID column.
#>  Wikipedia       UNRV Britannica 
#>          0          0          0 
score_var_info(emperors)
#> The data frame has 14 variables.
#> The data frame has 7 variables.
#> The data frame has 3 variables.
#>  Wikipedia       UNRV Britannica 
#>         14          7          3 
```
