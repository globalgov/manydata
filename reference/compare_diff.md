# Compare two datasets for differences

Compare two datasets for differences

## Usage

``` r
compare_new(.data1, .data2, by = "ID")

compare_diff(
  .data1,
  .data2,
  by = "ID",
  exclude = c("Title", "Coder", "Comments"),
  diff_threshold = 0
)
```

## Arguments

- .data1:

  First dataset to compare

- .data2:

  Second dataset to compare

- by:

  Column name to join on (default is "ID")

- exclude:

  Character vector of column names to exclude from comparison. By
  default, "Title", "Coder", and "Comments" are excluded.

- diff_threshold:

  Integer specifying the minimum number of differing columns for a row
  to be included in the output. Default is 0, meaning any difference
  will be included. Set to 3 to only show rows with at least 3 differing
  columns.

## Value

A data frame with the differences found

## Details

This function uses
[`dplyr::anti_join`](https://dplyr.tidyverse.org/reference/filter-joins.html)
to find rows in `.data1` that are not present in `.data2`. If no
differences are found, a message is printed and `NULL` is returned. If
differences are found, they are returned as a data frame.

## Examples

``` r
if (FALSE) { # \dontrun{
df1 <- data.frame(ID = 1:5, Value = letters[1:5])
df2 <- data.frame(ID = 3:7, Value = letters[3:7])
compare_new(df1, df2)
compare_new(df1, df1)
} # }
compare_diff(emperors$Wikipedia, emperors$Britannica)
#>                    Begin         End          
#> Augustus           "-0026-01-16" "0014-08-19" 
#> Tiberius           "0014-09-18"  "0037-03-16" 
#> Caligula           "0037-03-18"  "0041-01-24" 
#> Claudius           "0041-01-25"  "0054-10-13" 
#> Nero               "0054-10-13"  "0068-06-09" 
#> Galba              "0068-06-08"  "0069-01-15" 
#> Otho               "0069-01-15"  "0069-04-16" 
#> Vitellius          "0069-04-17"  "0069-12-20" 
#> Vespasian          "0069-12-21"  "0079-06-24" 
#> Titus              "0079-06-24"  "0081-09-13" 
#> Domitian           "0081-09-14"  "0096-09-18" 
#> Nerva              "0096-09-18"  "0098-01-27" 
#> Trajan             "0098-01-28"  "0117-08-07" 
#> Hadrian            "0117-08-11"  "0138-07-10" 
#> Antoninus Pius     "0138-07-10"  "0161-03-07" 
#> Lucius Verus       "0161-03-07"  "0169-03-15~"
#> Marcus Aurelius    "0161-03-07"  "0180-03-17" 
#> Commodus           NA            "0192-12-31" 
#> Pertinax           "0193-01-01"  "0193-03-28" 
#> Didius Julianus    "0193-03-28"  "0193-07-01" 
#> Septimus Severus   "0193-04-09"  "0211-02-04" 
#> Caracalla          NA            "0217-04-08" 
#> Geta               NA            "0211-12-26" 
#> Macrinus           "0217-04-11"  "0218-06-08" 
#> Elagabalus         "0218-06-08"  "0222-03-11" 
#> Severus Alexander  "0222-03-13"  "0235-03-18" 
#> Maximinus I        "0235-03-20"  "0238-06-15~"
#> Gordian I          "0238-03-22"  "0238-04-12" 
#> Gordian II         "0238-03-22"  "0238-04-12" 
#> Balbinus           "0238-04-22"  "0238-07-29" 
#> Pupienus           NA            NA           
#> Gordian III        NA            "0244-02-11" 
#> Philip I           "0244-02-14"  "0249-10-01~"
#> Decius             "0249-10-01~" "0251-06-15~"
#> Hostilian          "0251-06-15~" "0251-10-01~"
#> Trebonianus Gallus "0251-06-15~" "0253-08-15~"
#> Aemilian           "0253-08-15~" "0253-10-15~"
#> Valerian           "0253-10-15~" "0260-01-01~"
#> Gallienus          "0253-10-15~" "0268-09-15~"
#> Claudius Gothicus  "0268-09-15~" "0270-01-15~"
#> Quintillus         "0270-01-15~" "0270-04-15~"
#> Aurelian           "0270-09-15"  "0275-09-15" 
#> Tacitus            "0275-09-25"  "0276-06-15~"
#> Florian            "0276-06-15~" "0276-09-15~"
#> Probus             "0276-09-15~" "0282-10-01~"
#> Carus              "0282-10-01~" "0283-08-01~"
#> Numerian           "0283-08-01~" "0284-11-20~"
#> Carinus            "0283-08-01~" "0285-08-01~"
#> Diocletian         "0284-11-20"  "0305-05-01" 
#> Maximian           "0286-04-01"  "0305-05-01" 
#> Constantius I      "0305-05-01"  "0306-07-25" 
#> Severus II         "0305-05-01"  "0307-09-16" 
#> Galerius           "0305-05-01"  "0311-05-01~"
#> Constantine I      "0306-07-25"  "0337-05-22" 
#> Maxentius          "0306-10-08"  "0312-10-08" 
#> Lucinius I         "0308-11-11"  "0324-09-18" 
#> Maximinus II       "0311-05-01"  "0313-08-01~"
#> Constantine II     "0337-05-22"  "0340-01-01" 
#> Constantius II     "0337-05-22"  "0361-11-03" 
#> Constans I         "0337-05-22~" "0350-02-14~"
#> Julian             "0360-02-14~" "0363-06-26" 
#> Jovian             "0363-06-26"  "0364-02-17" 
#> Valentinian I      "0364-02-26"  "0375-11-17" 
#> Valens             "0364-03-28"  "0378-08-09" 
#> Gratian            "0367-08-04"  "0383-08-25" 
#> Valentinian II     "0375-11-17"  "0392-05-15" 
#> Theodosius I       "0379-01-01"  "0395-01-17" 
#> Constantius III    "0421-02-08"  "0421-09-02" 
```
