# Package index

## Calling ‘many’ packages

These functions assist researchers with downloading ‘many’ packages and
their data

- [`call_packages()`](https://www.manydata.ch/reference/call_packages.md)
  : Call, download, and update many\* packages
- [`call_releases()`](https://www.manydata.ch/reference/call_releases.md)
  : Call releases historical milestones/releases
- [`call_sources()`](https://www.manydata.ch/reference/call_sources.md)
  [`call_citations()`](https://www.manydata.ch/reference/call_sources.md)
  : Call sources and citations
- [`call_treaties()`](https://www.manydata.ch/reference/call_treaties.md)
  : Call treaties from 'many' datasets

## Comparing many data

These functions assist researchers with evaluating and comparing
datasets on global governance both visually and statistically.

- [`compare_categories()`](https://www.manydata.ch/reference/compare_categories.md)
  : Compare categories in 'many' datacubes
- [`compare_new()`](https://www.manydata.ch/reference/compare_diff.md)
  [`compare_diff()`](https://www.manydata.ch/reference/compare_diff.md)
  : Compare two datasets for differences
- [`compare_dimensions()`](https://www.manydata.ch/reference/compare_dimensions.md)
  : Compare dimensions for 'many' data
- [`compare_missing()`](https://www.manydata.ch/reference/compare_missing.md)
  : Compare missing observations for 'many' data
- [`compare_overlap()`](https://www.manydata.ch/reference/compare_overlap.md)
  : Compare the overlap between datasets in 'many' datacubes
- [`score_dataset()`](https://www.manydata.ch/reference/scores.md)
  [`score_obs_no()`](https://www.manydata.ch/reference/scores.md)
  [`score_var_no()`](https://www.manydata.ch/reference/scores.md)
  [`score_completeness()`](https://www.manydata.ch/reference/scores.md)
  [`score_date_consistency()`](https://www.manydata.ch/reference/scores.md)
  [`score_date_scope()`](https://www.manydata.ch/reference/scores.md)
  [`score_obs_info()`](https://www.manydata.ch/reference/scores.md)
  [`score_coding()`](https://www.manydata.ch/reference/scores.md)
  [`score_comments()`](https://www.manydata.ch/reference/scores.md)
  [`score_var_info()`](https://www.manydata.ch/reference/scores.md) :
  Scoring Functions for Data Quality Checks

## Consolidating many data

These functions assist researchers with working with multiple datasets
at once, including plucking individual datasets, consolidating multiple
datasets into a single dataset, and resolving conflicts between datasets
during this process.

- [`consolidate()`](https://www.manydata.ch/reference/consolidate.md) :
  Consolidate datacube into a single dataset
- [`resolve_unite()`](https://www.manydata.ch/reference/resolving.md)
  [`resolve_coalesce()`](https://www.manydata.ch/reference/resolving.md)
  [`resolve_min()`](https://www.manydata.ch/reference/resolving.md)
  [`resolve_max()`](https://www.manydata.ch/reference/resolving.md)
  [`resolve_random()`](https://www.manydata.ch/reference/resolving.md)
  [`resolve_precision()`](https://www.manydata.ch/reference/resolving.md)
  [`resolve_mean()`](https://www.manydata.ch/reference/resolving.md)
  [`resolve_mode()`](https://www.manydata.ch/reference/resolving.md)
  [`resolve_median()`](https://www.manydata.ch/reference/resolving.md)
  [`resolve_consensus()`](https://www.manydata.ch/reference/resolving.md)
  : Resolving multiple observations of the same variable into one
- [`pluck()`](https://www.manydata.ch/reference/pluck.md) : Selects a
  single dataset from a datacube
- [`filter_datacube()`](https://www.manydata.ch/reference/filter_datacube.md)
  : Filtering datacube datasets to a certain date

## Wrangling many data

These functions assist researchers with wrangling datasets and
datacubes.

- [`transmutate()`](https://www.manydata.ch/reference/transmutate.md) :
  Drop only columns used in formula
- [`recollect()`](https://www.manydata.ch/reference/recollect.md) :
  Pastes unique string vectors
- [`reunite()`](https://www.manydata.ch/reference/reunite.md) : Pastes
  unique string vectors
- [`repaint()`](https://www.manydata.ch/reference/repaint.md) : Fills
  missing data by lookup
- [`find_ID()`](https://www.manydata.ch/reference/find.md)
  [`find_common_ID()`](https://www.manydata.ch/reference/find.md)
  [`find_duplicates()`](https://www.manydata.ch/reference/find.md) :
  Find elements within manydata
- [`find_year()`](https://www.manydata.ch/reference/find_year.md) :
  Creates Numerical IDs from Signature Dates
- [`emperors`](https://www.manydata.ch/reference/emperors.md) : Emperors
  datacube documentation
- [`mreport()`](https://www.manydata.ch/reference/describe.md)
  [`describe_datacube()`](https://www.manydata.ch/reference/describe.md)
  : Data reports for datacubes and datasets with 'mdate' variables

## Maintaining many data

Datasets in ‘many’ packages are updated frequently. These functions
assist analysts with maintaining their data up to date.

- [`code_extend_glove()`](https://www.manydata.ch/reference/code_extend.md)
  [`code_extend_bert()`](https://www.manydata.ch/reference/code_extend.md)
  : Extending codes
