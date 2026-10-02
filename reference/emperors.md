# Emperors datacube documentation

The `emperors` datacube is a list containing 3 datasets: Wikipedia,
UNRV, and Britannica

## Usage

``` r
emperors
```

## Format

- Wikipedia: :

  A dataset with 68 observations and the following 15 variables: ID,
  Begin, End, FullName, Birth, Death, CityBirth, ProvinceBirth, Rise,
  Cause, Killer, Dynasty, Era, Notes, Verif.

- UNRV: :

  A dataset with 99 observations and the following 7 variables: ID,
  Begin, End, Birth, Death, FullName, Dynasty.

- Britannica: :

  A dataset with 87 observations and the following 3 variables: ID,
  Begin, End.

## Source

- Wikipedia, 'List_of_Roman_emperors',
  https://en.wikipedia.org/wiki/List_of_Roman_emperors, Accessed on
  2021-07-22.

- United Nations of Roma Victrix, 'Roman Emperor list',
  https://www.unrv.com/government/emperor.php, Accessed on 2021-07-22.

- Britannica, 'List of Roman emperors',
  https://www.britannica.com/topic/list-of-Roman-emperors-2043294,
  Accessed on 2021-07-22.

## Details

    #> $Wikipedia
    #> --------------------------------------------------------
    #> |  Variable   |  Class  |  Obs  |  Missing  |  Miss %  |
    #> --------------------------------------------------------
    #> |ID           |character|     69|          0|         0|
    #> |Begin        |mdate    |     69|          0|         0|
    #> |End          |mdate    |     69|          0|         0|
    #> |FullName     |character|     68|          1|      1.45|
    #> |Birth        |mdate    |     63|          6|       8.7|
    #> |Death        |mdate    |     68|          1|      1.45|
    #> |CityBirth    |character|     51|         18|     26.09|
    #> |ProvinceBirth|character|     68|          1|      1.45|
    #> |Rise         |character|     68|          1|      1.45|
    #> |Cause        |character|     68|          1|      1.45|
    #> |Killer       |character|     68|          1|      1.45|
    #> |Dynasty      |character|     68|          1|      1.45|
    #> |Era          |character|     68|          1|      1.45|
    #> |Notes        |character|     46|         23|     33.33|
    #> --------------------------------------------------------
    #>
    #>
    #> $UNRV
    #> -------------------------------------------------------
    #> |  Variable  |  Class  |  Obs  |  Missing  |  Miss %  |
    #> -------------------------------------------------------
    #> |ID          |character|     98|          0|         0|
    #> |Begin       |mdate    |     98|          0|         0|
    #> |End         |mdate    |     98|          0|         0|
    #> |Birth       |mdate    |     74|         24|     24.49|
    #> |Death       |mdate    |     97|          1|      1.02|
    #> |FullName    |character|     93|          5|       5.1|
    #> |Dynasty     |character|     61|         37|     37.76|
    #> -------------------------------------------------------
    #>
    #>
    #> $Britannica
    #> -------------------------------------------------------
    #> |  Variable  |  Class  |  Obs  |  Missing  |  Miss %  |
    #> -------------------------------------------------------
    #> |ID          |character|     87|          0|         0|
    #> |Begin       |mdate    |     87|          0|         0|
    #> |End         |mdate    |     87|          0|         0|
    #> -------------------------------------------------------

## URL

- Wikipedia: <https://en.wikipedia.org/wiki/List_of_Roman_emperors>

- UNRV: <https://www.unrv.com/government/emperor.php>

- Britannica:
  <https://www.britannica.com/place/list-of-Roman-emperors-2043294>

## Mapping

- wikipedia: Variable Mapping

|             |               |
|-------------|---------------|
| *from*      | *to*          |
| name        | ID            |
| reign.start | Begin         |
| reign.end   | End           |
| name.full   | FullName      |
| birth       | Birth         |
| death       | Death         |
| birth.cty   | CityBirth     |
| birth.prv   | ProvinceBirth |
| rise        | Rise          |
| cause       | Cause         |
| killer      | Killer        |
| dynasty     | Dynasty       |
| era         | Era           |
| notes       | Notes         |
| verif.who   | Verif         |

- UNRV: Variable Mapping

|                           |          |
|---------------------------|----------|
| *from*                    | *to*     |
| 'Common Name'             | ID       |
| Beg                       | Begin    |
| 'Full Name/Imperial Name' | FullName |
| 'Dynasty/Class/Notes'     | Dynasty  |

- britannica: Variable Mapping

|             |       |
|-------------|-------|
| *from*      | *to*  |
| Name        | ID    |
| reign_start | Begin |
| reign_end   | End   |
