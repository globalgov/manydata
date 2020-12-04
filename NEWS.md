# qData 0.2.1

## Package

* Fixes #61 by adding tests for `setup_package()` and `import_data()`
* Closes #50 by making sure workflow templates are created in the proper
folder with `setup_package()`

## Correction

* Closes #62 by creating a function which vectorises dates and cleans
and standardizes diverse date formats, `standardise_dates()`
* Closes #55 by deprecating `rearrange()` function in favor of `dplyr::relocate()`
* Closes #65 by correcting issues with the path call in `export_data()` function
  * Ensures consistency for path calls across qData functions

## Connection

* Closes #64 by creating the `get_packages()` function which displays other packages,
and some information about these packages, in the qData ecosystem. The function
serves as a wrapper for downloading qPackages from GitHub.

# qData 0.2.0

## Package

* Updated pushrelease.yml to upload all assets individually to release
* ((Tests for following functions on hold...))

## Collection

* Renamed `create_qPackage()` to `setup_package()`
  * Now establishes main testing framework
  * Opens fewer files but is more verbose
  * Closed #51 by trimming dependencies from `qPackage-DESC` template
  * Fixed bug in `qPackage-DESC` that names weren't strings
* Renamed `use_qData_raw()` to `import_data()`
  * Closed #49 with path argument or selector
  * Added delete_original argument (default = FALSE)
  * Closed #58 by extracting file type information from path and adding it and path to preparation template

## Connection

* Renamed `use_qData()` to `export_data()`
  * Avoided reliance on `usethis::use_data()`
  * Fixed bug where it wasn't selecting object correctly
  * Closed #57 by passing on object information into the documentation template (now `qData-doc.R`)
  * Adapted test template so that object name is passed forward

# qData 0.1.0

## Package

* Updated CONTRIBUTING
* Added README.Rmd render to pushrelease workflow

## Collection

* Closes #47 by adding `qtemplate()` function for finding and rendering templates
* Closes #44 by adding `create_qPackage()` that establishes a `{qData}` consistent package framework
  * Fixed #42 by drawing information from DESCRIPTION where possible
  * Added DESCRIPTION template
  * Added README template
  * Added COC template
  * Added CONTRIBUTING template
  * Added PR and issue templates for bugs and features
  * Added workflow templates for push-release, pr-checks, and pr-commands
* Closes #48 by adding `use_qData_raw()` for setting up data cleaning and wrangling
  * Added template for importing, cleaning and exporting raw data

## Correction

* Closes #24 by adding `repaint()` for filling in missing data by lookup
* Added `recent()` for sensible centuries for dates

## Connection

* Closes #45 by adding `use_qData()` for setting up tests, documentation, and lazy-loading of cleaned data
  * Added template for documenting cleaned data
  * Added template for testing cleaned data

# qData 0.0.4

## Collection

* Closes #25 by adding `create_qpackage()`
* Fixed 'pushrelease.yml' postfix bug

## Correction

* Closes #26 by adding `interleave()`
* Closes #23 by adding `resequence()`
* Closes #30 by adding `recollect()`

# qData 0.0.3

## Package

* Closes #18 by setting up `{lintr}`, `{goodpractice}`, and `{spelling}` in prchecks.yml

## Correction

* Closes #29 by adding `rearrange()` 
* Closes #31 by adding `reunite()`

# qData 0.0.2

## Package

* Added a `NEWS.md` file to track changes to the package.

## Correction

* Added `transmutate()` for merging variables

# qData 0.0.1

## Package

* Package setup

## Correction

* Added `entitle()` for standardising treaty titles, etc.
