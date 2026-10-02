# Changelog

## manydata 1.1.4

### Package

- Moved [text2vec](http://text2vec.org) to Suggests because of archival
  of [float](https://github.com/wrathematics/float)
  - [`code_extend_glove()`](https://www.manydata.ch/reference/code_extend.md)
    now checks whether [text2vec](http://text2vec.org) is installed
- Suggested packages now only offered for installation in interactive
  sessions
- Examples and tests requiring suggested packages are now skipped if not
  installed
- Now depends on R \>= 4.1.0 and uses the native pipe `|>` internally
  - `%>%` is still re-exported for users
- Updated GitHub actions and PR template
  - PR checks now include PR metadata and reverse dependency checks
  - Releases now draw their notes from the NEWS file

### Data

- Fixed invalid date in `emperors$UNRV` causing an error in
  [`consolidate()`](https://www.manydata.ch/reference/consolidate.md) eg

## manydata 1.1.3

CRAN release: 2025-09-30

### Package

- Updated website address
- Updated authorship

## manydata 1.1.0

### Package

- Updated GitHub actions to use code coverage secrets

### Wrangling

- Added
  [`filter_datacube()`](https://www.manydata.ch/reference/filter_datacube.md)
  for filtering datasets in a datacube by date
- Added [`find_ID()`](https://www.manydata.ch/reference/find.md) and
  [`find_common_ID()`](https://www.manydata.ch/reference/find.md) for
  identifying ID columns in datasets

### Evaluation

- Added [`find_year()`](https://www.manydata.ch/reference/find_year.md)
  for extracting just the year from a date (potentially unnecessary if
  [`messydates::year()`](https://lubridate.tidyverse.org/reference/year.html)
  available)
- Added
  [`compare_new()`](https://www.manydata.ch/reference/compare_diff.md)
  and
  [`compare_diff()`](https://www.manydata.ch/reference/compare_diff.md)
  for comparing what is new or different in one dataset over another
- Added a range of `score_*()` functions for scoring datasets on various
  criteria, including consistency, completeness, accuracy, timeliness,
  and uniqueness of the data

### Maintaining

- Added [`find_duplicates()`](https://www.manydata.ch/reference/find.md)
  for identifying duplicate observations in datasets
- Added
  [`code_extend_glove()`](https://www.manydata.ch/reference/code_extend.md)
  and
  [`code_extend_bert()`](https://www.manydata.ch/reference/code_extend.md)
  for extending existing coding to new or missing data

## manydata 1.0.3

CRAN release: 2025-06-18

### Connection

- Added new `getID()` helper that obtains the one or two ID columns that
  appear as the first one or two columns in a datacube
- [`compare_overlap()`](https://www.manydata.ch/reference/compare_overlap.md)
  now returns a list of each datasets IDs to avoid issues with
  [ggVennDiagram](https://github.com/gaospecial/ggVennDiagram)
- `plot.compare_overlap()` now always returns an upset plot (closes
  [\#292](https://github.com/globalgov/manydata/issues/292))
- Fixed testing of ggplot objects (closes
  [\#308](https://github.com/globalgov/manydata/issues/308))
- Fixed how `plot.compare_categories()` treated identifier variables
  (closes [\#291](https://github.com/globalgov/manydata/issues/291))

## manydata 1.0.2

CRAN release: 2025-06-03

### Connection

- Fixed global variables in several `resolve_*()` functions

## manydata 1.0.1

CRAN release: 2025-03-21

### Package

- Updated website

### Connection

- `resolve_*()` functions now have a parameter indicating whether
  missing values should be included; unlike base R, by default missing
  values are excluded
- Restored
  [`resolve_mean()`](https://www.manydata.ch/reference/resolving.md)
- Restored
  [`resolve_median()`](https://www.manydata.ch/reference/resolving.md)
- Added
  [`resolve_mode()`](https://www.manydata.ch/reference/resolving.md) for
  retaining the most common values
- Added
  [`resolve_consensus()`](https://www.manydata.ch/reference/resolving.md)
  for retaining only values where there are no conflicts

## manydata 1.0.0

### Package

- Updated GitHub checks and release actions
- Fixes to URLs
- Updated website
- Improved ease of operation by making [cli](https://cli.r-lib.org),
  [dplyr](https://dplyr.tidyverse.org), and
  [messydates](https://globalgov.github.io/messydates/) Depends
- Dropped [usethis](https://usethis.r-lib.org) Suggest

### Collection

- Updated `emperors` dataset
  - Using zero-padded messydates
  - Added citation prompts
  - Datasets capitalised:
    - `emperors$Wikipedia`
    - `emperors$UNRV`
    - `emperors$Britannica`
  - Fixed non-unique IDs bugs
  - Fixed inc

### Calling

- Added
  [`call_citations()`](https://www.manydata.ch/reference/call_sources.md)
  to print citations added as hidden information
- Fixed finicky
  [`call_sources()`](https://www.manydata.ch/reference/call_sources.md)
  bug related to calling help files
- Improved
  [`call_sources()`](https://www.manydata.ch/reference/call_sources.md)
  and
  [`call_citations()`](https://www.manydata.ch/reference/call_sources.md)
  to accept datacubes or datasets, as objects or characters
- Moved [`mreport()`](https://www.manydata.ch/reference/describe.md)
  from messydates
  - Added `mreport.list()` to make it easier to report on datacubes
- Added `describe_data()` for describing key aspects of datasets in
  datacubes
- Fixed
  [`call_releases()`](https://www.manydata.ch/reference/call_releases.md)
  to use
  [`messydates::vmin()`](https://globalgov.github.io/messydates/reference/resolve_extrema.html)

### Connection

- Improved [`pluck()`](https://www.manydata.ch/reference/pluck.md)
  - Function now wraps `dplyr::pluck()` but adds a citation prompt
- Improved
  [`consolidate()`](https://www.manydata.ch/reference/consolidate.md)
  - Improved useability with [cli](https://cli.r-lib.org) progress
    messages and success alerts
  - Improved speed using [dtplyr](https://dtplyr.tidyverse.org) in place
    of
    [`dplyr::full_join()`](https://dplyr.tidyverse.org/reference/mutate-joins.html)
    (closes [\#288](https://github.com/globalgov/manydata/issues/288))
    - [duckplyr](https://duckplyr.tidyverse.org) considered: faster, but
      couldn’t handle `mdate` class
    - [collapse](https://fastverse.org/collapse/) considered: even
      faster, but inconsistent output
  - Improved compatibility by converting ‘rows’ argument to ‘join’
    (breaking)
    - “all” becomes “inner”
    - “any” becomes “full”
    - “favour” becomes “left”
  - Fixed being passed a single dataset
  - Prompts users to cite datasets (closes
    [\#280](https://github.com/globalgov/manydata/issues/280))
  - Fixed bug in ‘resolve’ argument, named ‘resolve’ vector no longer
    has to be same length as variables
  - Dropped ‘cols’ argument
- Updated tests for
  [`consolidate()`](https://www.manydata.ch/reference/consolidate.md) to
  use new ‘join’ argument
  - testthat tests use [cli](https://cli.r-lib.org) on quiet mode
- Updated
  [`resolve_coalesce()`](https://www.manydata.ch/reference/resolving.md)
  for coalescing (taking first non-NA value)
- Updated
  [`resolve_random()`](https://www.manydata.ch/reference/resolving.md)
  for returning random values sampling from those available
- Updated
  [`resolve_min()`](https://www.manydata.ch/reference/resolving.md) and
  [`resolve_max()`](https://www.manydata.ch/reference/resolving.md) for
  returning min or max values
- Added
  [`resolve_unite()`](https://www.manydata.ch/reference/resolving.md)
  for returning all possible values as a set
- Added
  [`resolve_precision()`](https://www.manydata.ch/reference/resolving.md)
  for returning most precise values available (closes
  [\#265](https://github.com/globalgov/manydata/issues/265))
  - Added `precision.numeric()` to return most significant figures
  - Added `precision.character()` to return most characters
- Dropped
  [`resolve_median()`](https://www.manydata.ch/reference/resolving.md)
  and [`resolve_mean()`](https://www.manydata.ch/reference/resolving.md)
  as uncommon choices
- Dropped `resolve_multiple()` in favour of always using more flexible
  for loop
- Dropped `favour()` in favour of left joins and coalesces
- Dropped `coalesce_rows()` as no longer necessary

## manydata 0.9.3

CRAN release: 2024-05-06

### Connection

- Updated
  [`call_sources()`](https://www.manydata.ch/reference/call_sources.md)
  to be more flexible when gathering data from datacube documentation
- Closed [\#279](https://github.com/globalgov/manydata/issues/279) by
  updating documentation across many packages to be compatible with
  [`call_sources()`](https://www.manydata.ch/reference/call_sources.md)
- Updated
  [`compare_dimensions()`](https://www.manydata.ch/reference/compare_dimensions.md)
  by fixing bugs related to dates and NA observations

## manydata 0.9.2

CRAN release: 2024-02-22

### Package

- Fixed the `emperors` data documentation issues related to lost braces
  with CRAN submission

## manydata 0.9.1

### Package

- Updated test expectations to make package compatible with the new
  release of [ggplot2](https://ggplot2.tidyverse.org)

### Connection

- Closed [\#266](https://github.com/globalgov/manydata/issues/266) by
  adding startup messages to ‘many’ packages
- Closed [\#267](https://github.com/globalgov/manydata/issues/267) by
  adding links to package websites in console messages
- Closed [\#282](https://github.com/globalgov/manydata/issues/282) by
  updating all references from ‘database’ to ‘datacube’
- Closed [\#293](https://github.com/globalgov/manydata/issues/293) by
  fixing bugs related to missing dates when using
  [`consolidate()`](https://www.manydata.ch/reference/consolidate.md)
- Closed [\#294](https://github.com/globalgov/manydata/issues/294) by
  updating how
  [`call_sources()`](https://www.manydata.ch/reference/call_sources.md)
  identify datasets within datacubes

## manydata 0.9.0

### Package

- Closed [\#259](https://github.com/globalgov/manydata/issues/259) by
  revising CCC package structure and updating the package cheatsheet
- Updated documentation for ‘emperors’ data to new style to improve
  visibility and transparency
- Closed [\#264](https://github.com/globalgov/manydata/issues/264) by
  removing [tibble](https://tibble.tidyverse.org/)and
  [janitor](https://github.com/sfirke/janitor) package imports in
  DESCRIPTION file
- Closed [\#276](https://github.com/globalgov/manydata/issues/276) by
  reviewing package vignettes
- Closed [\#277](https://github.com/globalgov/manydata/issues/277) by
  updating ‘manydata-defunct’ file
- Closed [\#284](https://github.com/globalgov/manydata/issues/284) by
  removing vignette and updating README to include more information on
  how to use the package
- Updated all references and argument from ‘database’ to ‘datacube’

### Connection

- Renamed and updated ‘call\_’ family of functions
  - Closed [\#250](https://github.com/globalgov/manydata/issues/250),
    [\#251](https://github.com/globalgov/manydata/issues/251), and
    [\#262](https://github.com/globalgov/manydata/issues/262) by
    renaming
    [`get_packages()`](https://www.manydata.ch/reference/defunct.md) to
    [`call_packages()`](https://www.manydata.ch/reference/call_packages.md)
    and updating how the function works and look up packages, version
    updates, and availailabity
  - Closed [\#269](https://github.com/globalgov/manydata/issues/269) and
    \# by adding a
    [`call_sources()`](https://www.manydata.ch/reference/call_sources.md)
    function that displays sources and variable changes for datasets in
    datacubes
  - Closed [\#271](https://github.com/globalgov/manydata/issues/271) by
    updating the `retrieve_` family of functions to `call_` functions
  - Closed [\#283](https://github.com/globalgov/manydata/issues/283) by
    renaming
    [`plot_releases()`](https://www.manydata.ch/reference/defunct.md) to
    `call_releases`
- Renamed and updated ‘compare\_’ family of functions
  - Closed [\#243](https://github.com/globalgov/manydata/issues/243) and
    [\#257](https://github.com/globalgov/manydata/issues/257) by
    creating a
    [`compare_missing()`](https://www.manydata.ch/reference/compare_missing.md)
    function to compare missing values in datasets in a ‘many’ datacube
  - Closed [\#249](https://github.com/globalgov/manydata/issues/249) and
    [\#253](https://github.com/globalgov/manydata/issues/253) by
    renaming [`db_plot()`](https://www.manydata.ch/reference/defunct.md)
    function to
    [`compare_categories()`](https://www.manydata.ch/reference/compare_categories.md)
    and updating variable categories
  - Closed [\#261](https://github.com/globalgov/manydata/issues/261) by
    renaming and updating other `db_` functions to `compare_` functions
  - Closed [\#268](https://github.com/globalgov/manydata/issues/268) by
    adding
    [`compare_overlap()`](https://www.manydata.ch/reference/compare_overlap.md)
    to help users investigate overlap for datasets within datacubes
  - Closed [\#285](https://github.com/globalgov/manydata/issues/285) by
    adding
    [`compare_dimensions()`](https://www.manydata.ch/reference/compare_dimensions.md)
    and `compare_ranges()` to compare dimensions and ranges in datacubes

## manydata 0.8.3

CRAN release: 2023-06-15

### Connection

- Made ´network_map()´ function defunct

## manydata 0.8.2

CRAN release: 2022-11-19

### Connection

- Updated
  [`consolidate()`](https://www.manydata.ch/reference/consolidate.md) to
  require two keys when joining memberships’ databases
- Updated [`db_comp()`](https://www.manydata.ch/reference/defunct.md) to
  follow consolidation defaults for memberships’ databases
- Closed [\#231](https://github.com/globalgov/manydata/issues/231) by
  adding a `retrieve_texts()` function to retrieve treaty texts from
  other ‘many’ packages

## manydata 0.8.1

CRAN release: 2022-11-11

### Package

- Added ‘RDataTmp’ files to Rbuildignore and .gitignore
- Updated
  [`data_evolution()`](https://www.manydata.ch/reference/defunct.md) to
  use [`inherits()`](https://rdrr.io/r/base/class.html) instead of
  [`class()`](https://rdrr.io/r/base/class.html) for condition
  comparison

## manydata 0.8.0

### Package

- Closed [\#212](https://github.com/globalgov/manydata/issues/212) by
  implementing package caching in GitHub actions workflows
- Closed [\#218](https://github.com/globalgov/manydata/issues/218) by
  fixing bug with GitHub actions workflows
- Closed [\#225](https://github.com/globalgov/manydata/issues/225) by
  changing the structure of datasets in “many” data packages
- Closed [\#240](https://github.com/globalgov/manydata/issues/240) by
  updating the package cheatsheet

### Connection

- Closed [\#134](https://github.com/globalgov/manydata/issues/134) by
  adding a
  [`data_evolution()`](https://www.manydata.ch/reference/defunct.md)
  function to the report family of functions that gets original
  datasets, if available, or opens the preparation scripts, if not
  available
- Added ‘db_profile’ family of functions to visualise databases
  - Closed [\#214](https://github.com/globalgov/manydata/issues/214) by
    adding [`db_plot()`](https://www.manydata.ch/reference/defunct.md)
    function to plot a profile of the database to facilitate comparison
    of matched observations across datasets
  - Closed [\#224](https://github.com/globalgov/manydata/issues/224) by
    adding [`db_comp()`](https://www.manydata.ch/reference/defunct.md)
    function that creates a tibble of the database to facilitate
    comparison of matched observations across datasets
- Updated
  [`get_packages()`](https://www.manydata.ch/reference/defunct.md)
  function
  - Closed [\#215](https://github.com/globalgov/manydata/issues/215) by
    making
    [`get_packages()`](https://www.manydata.ch/reference/defunct.md)
    interactive so that users can chose which branch to download
  - Closed [\#219](https://github.com/globalgov/manydata/issues/219) by
    improving
    [`get_packages()`](https://www.manydata.ch/reference/defunct.md)
    printing
  - Updated
    [`get_packages()`](https://www.manydata.ch/reference/defunct.md) and
    [`plot_releases()`](https://www.manydata.ch/reference/defunct.md) to
    use [messydates](https://globalgov.github.io/messydates/), instead
    of [lubridate](https://lubridate.tidyverse.org), for dates coercion
- Closed [\#222](https://github.com/globalgov/manydata/issues/222) by
  adding [`network_map()`](https://www.manydata.ch/reference/defunct.md)
  function for plotting geographical networks
- Updated
  [`consolidate()`](https://www.manydata.ch/reference/consolidate.md)function
  to make function over 20 times faster
  - Closed [\#227](https://github.com/globalgov/manydata/issues/227) by
    making
    [`consolidate()`](https://www.manydata.ch/reference/consolidate.md)
    ignore text related variables due to their size
  - Closed [\#230](https://github.com/globalgov/manydata/issues/230) by
    making
    [`consolidate()`](https://www.manydata.ch/reference/consolidate.md)
    more concise to avoid running into memory limits
  - Closed [\#228](https://github.com/globalgov/manydata/issues/228) and
    [\#232](https://github.com/globalgov/manydata/issues/232) by
    replacing
    [`coalesce_compatible()`](https://www.manydata.ch/reference/defunct.md)
    for a faster approach to coalescing compatible missing observations
    that relies on `zoo::na.locf()`
  - Made
    [`coalesce_compatible()`](https://www.manydata.ch/reference/defunct.md)
    function defunct

## manydata 0.7.5

CRAN release: 2022-06-07

### Package

- Removed [skimr](https://docs.ropensci.org/skimr/) table from
  `emperors` database documentation
- Updated path for binaries in push release GitHub actions

## manydata 0.7.4

### Package

- Closed [\#187](https://github.com/globalgov/manydata/issues/187) by
  updating GitHub actions to implement package caching
- Closed [\#209](https://github.com/globalgov/manydata/issues/209) by
  removing all non-ASCII characters in package
- Closed [\#210](https://github.com/globalgov/manydata/issues/210) by
  removing [pkgdown](https://pkgdown.r-lib.org/) dependency
- Updated `emperors` data to contain correct date class name consistent
  with [messydates](https://globalgov.github.io/messydates/)

## manydata 0.7.3

CRAN release: 2022-04-01

### Connection

- Updated how the
  [`get_packages()`](https://www.manydata.ch/reference/defunct.md)
  function identifies installed packages to avoid using
  [`installed.packages()`](https://rdrr.io/r/utils/installed.packages.html)
- Updated documentation for
  [`coalesce_compatible()`](https://www.manydata.ch/reference/defunct.md)
  function to include the returns

## manydata 0.7.2

- Ignored CRAN-SUBMISSION and resubmitted.

## manydata 0.7.1

### Package

- Updated DESCRIPTION by removing ambiguous word from title
- Updated README by correcting the URL for life cycle badge

### Connection

- Updated helper functions for
  [`consolidate()`](https://www.manydata.ch/reference/consolidate.md) to
  use [`inherits()`](https://rdrr.io/r/base/class.html) to identify
  variable’s class

## manydata 0.7.0

### Package

- Closed [\#194](https://github.com/globalgov/manydata/issues/194) by
  updating all remaining references from “qID” to “manyID”
- Updated package website
  - Closed [\#196](https://github.com/globalgov/manydata/issues/196) by
    updating elements that configure website to work properly
  - Updated ’\_pkgdown.yml’ file to use bootstrap 5 template to build
    website

### Connection

- Updated
  [`consolidate()`](https://www.manydata.ch/reference/consolidate.md)
  function
  - Closed [\#191](https://github.com/globalgov/manydata/issues/191) by
    making
    [`consolidate()`](https://www.manydata.ch/reference/consolidate.md)
    function more concise and faster by removing redundant code lines
  - Fixed dates-related warnings by changing how
    [messydates](https://globalgov.github.io/messydates/) package is
    used to resolve dates
  - Updated how
    [`consolidate()`](https://www.manydata.ch/reference/consolidate.md)
    substitutes missing observations with first non-missing observation
    from other datasets
  - Closed [\#201](https://github.com/globalgov/manydata/issues/201) by
    fixing how
    [`consolidate()`](https://www.manydata.ch/reference/consolidate.md)
    detects variables to be resolved to avoid ambiguous variable
    matching
  - Closed [\#202](https://github.com/globalgov/manydata/issues/202) by
    allowing for multiple key vectors to be declared as arguments for
    [`consolidate()`](https://www.manydata.ch/reference/consolidate.md)
- Closed [\#199](https://github.com/globalgov/manydata/issues/199) by
  adding `favour()` (also `favor()`) function that re-orders datasets
  within a database

## manydata 0.6.0

### Package

- Closed [\#189](https://github.com/globalgov/manydata/issues/189) by
  renaming package from `{qData}` to
  [manydata](https://www.manydata.ch/)
- Updated user vignette to include more examples on working with
  [`consolidate()`](https://www.manydata.ch/reference/consolidate.md)
- Updated package website
- Closed [\#167](https://github.com/globalgov/manydata/issues/167) by
  adding a cheatsheet to README

### Connection

- Updated
  [`consolidate()`](https://www.manydata.ch/reference/consolidate.md)
  function
  - Closed [\#169](https://github.com/globalgov/manydata/issues/169) by
    making default key variable “many_ID” instead of “qID”
  - Closed [\#183](https://github.com/globalgov/manydata/issues/183) by
    adding further methods to resolve conflicts between observations:
    - Added “max” resolve argument which resolves conflicts in favor of
      the largest non NA value
    - Added “min” resolve argument which resolves conflicts in favor of
      the smallest non NA value
    - Added “mean” resolve argument which resolves conflicts in favor of
      the average non NA value
    - Added “median” resolve argument which resolves conflicts in favor
      of the median non NA value
    - Added “random” resolve argument which resolves conflicts in favor
      of a random non NA value
  - Closed [\#185](https://github.com/globalgov/manydata/issues/185) by
    making so that users can specify resolve argument differently for
    different variables
- Closed [\#188](https://github.com/globalgov/manydata/issues/188) by
  adding more informative warnings for GitHub download limits for
  [`get_packages()`](https://www.manydata.ch/reference/defunct.md)
  function
- Added extraction functions to generate edgelists from agreements
  membership datasets
  - Added `extract_bilaterals()` for extracting adjacency edgelist for
    bilateral agreements
  - Added `extract_multilaterals()` for extracting adjacency edgelist
    for multilateral agreements
