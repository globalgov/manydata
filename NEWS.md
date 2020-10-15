# qDatr 0.1.0

## Package

* Updated CONTRIBUTING
* Added README.Rmd render to pushrelease workflow

## Collection

* Added `qtemplate()` function for finding and rendering templates
* Closes #44 by adding `create_qPackage()` that establishes a `{qDatr}` consistent package framework
  * Added DESCRIPTION template
  * Added README template
  * Added COC template
  * Added CONTRIBUTING template
  * Added PR and issue templates for bugs and features
  * Added workflow templates for push-release, pr-checks, and pr-commands
* Adds `use_qData_raw()` for setting up data cleaning and wrangling
  * Added template for importing, cleaning and exporting raw data

## Correction

* Added `repaint()` for filling in missing data by lookup
* Added `recent()` for sensible centuries for dates

## Connection

* Closes #45 by adding `use_qData()` for setting up tests, documentation, and lazy-loading of cleaned data
  * Added template for documenting cleaned data
  * Added template for testing cleaned data

# qDatr 0.0.4

## Collection

* Closes #25 by adding `create_qpackage()`
* Fixed 'pushrelease.yml' postfix bug

## Correction

* Closes #26 by adding `interleave()`
* Closes #23 by adding `resequence()`
* Closes #30 by adding `recollect()`

# qDatr 0.0.3

## Package

* Closes #18 by setting up `{lintr}`, `{goodpractice}`, and `{spelling}` in prchecks.yml

## Correction

* Closes #29 by adding `rearrange()` 
* Closes #31 by adding `reunite()`

# qDatr 0.0.2

## Package

* Added a `NEWS.md` file to track changes to the package.

## Correction

* Added `transmutate()` for merging variables

# qDatr 0.0.1

## Package

* Package setup

## Correction

* Added `entitle()` for standardising treaty titles, etc.
