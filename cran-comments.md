## Test environments

* local R installation, x86_64-apple-darwin17.0, R 4.1.2
* Mac OS X 11.6.5 (on Github), R 4.1.3
* Microsoft Windows Server 2022 10.0.20348 (on Github), R 4.1.3
* Ubuntu 20.04.4 (on Github), R 4.1.3

## R CMD check results

0 errors | 0 warnings | 0 notes

## Resubmission

This is a resubmission. In this version I have:

* Updated how the `get_packages()` function identifies installed packages to avoid using `installed.packages()`
* Updated documentation for `coalesce_compatible()` function to include the returns
