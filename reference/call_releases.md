# Call releases historical milestones/releases

The function will take a data frame that details this information, or
more usefully, a Github repository listing.

## Usage

``` r
call_releases(repo, begin = NULL, end = NULL)
```

## Source

https://benalexkeen.com/creating-a-timeline-graphic-using-r-and-ggplot2/

## Arguments

- repo:

  the github repository to track, e.g. "globalgov/manydata"

- begin:

  When to begin tracking repository milestones. By default NULL, two
  months before the first release.

- end:

  When to end tracking repository milestones. By default NULL, two
  months after the latest release.

## Value

A ggplot graph object

## Details

The function creates a project timeline graphic using ggplot2 with
historical milestones and milestone statuses gathered from a specified
GitHub repository.

## See also

Other call\_:
[`call_packages()`](https://www.manydata.ch/reference/call_packages.md),
[`call_treaties()`](https://www.manydata.ch/reference/call_treaties.md)

## Examples

``` r
# \donttest{
#call_releases("globalgov/manydata")
#call_releases("manypkgs")
# }
```
