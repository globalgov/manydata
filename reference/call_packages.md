# Call, download, and update many\* packages

`call_packages()` finds and download other packages that belong to the
many universe of packages. It allows users to rapidly access the names
and other descriptive information of these packages. If users intend to
download and install a package listed, they can type the package name
within the function.

## Usage

``` r
call_packages(package, develop = FALSE)
```

## Arguments

- package:

  A character vector of package name. For multiple packages, please
  declare package names as a vector (e.g. c("package1", "package2")).

- develop:

  Would you like to download the develop version of the package? FALSE
  by default.

## Value

`call_packages()` returns a tibble with the 'many packages' currently
available. If one or more package names are provided, these will be
installed from Github.

## See also

Other call\_:
[`call_releases()`](https://www.manydata.ch/reference/call_releases.md),
[`call_treaties()`](https://www.manydata.ch/reference/call_treaties.md)

## Examples

``` r
# \donttest{
#call_packages()
#call_packages("manyenviron")
# }
```
