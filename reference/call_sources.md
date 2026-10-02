# Call sources and citations

These functions call any source or citation information that is
available for a datacube or dataset. The function can be used on its own
to the console, called during another function call such as
[`consolidate()`](https://www.manydata.ch/reference/consolidate.md) or
[`pluck()`](https://www.manydata.ch/reference/pluck.md), or is used to
automatically and consistently populate help files.

## Usage

``` r
call_sources(x)

call_citations(x, output = c("console", "help"))
```

## Arguments

- x:

  A datacube or dataset

- output:

  Whether the output should be formatted for "console" or the "help"
  page.
