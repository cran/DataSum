# DataSum 1.0.0

## Breaking changes

- Replaces the prototype API (`Datum()`, `DataSumm()`, `getmode()`, and `shapiro_normality_test()`) with a clean research-facing API: `summarize_vector()`, `summarize_data()`, `profile_data()`, `datasum_report()`, and `run_datasum_app()`.
- Normality diagnostics now return the test name, statistic, p-value, alpha, and decision instead of a single label.

## New features

- Adds NA-aware summaries for numeric, character, factor, logical, date, datetime, and unsupported columns.
- Adds robust statistics: quartiles, IQR, MAD, outlier counts, missingness diagnostics, skewness, and excess kurtosis.
- Adds grouped summaries through `summarize_data(..., by = ...)`.
- Adds `profile_data()` for dataset-level diagnostics and warnings.
- Adds `datasum_report()` to create reproducible Quarto diagnostic reports.
- Adds `run_datasum_app()` as a Shiny interface for uploaded CSV files, summaries, warnings, plots, and report download.
- Adds testthat coverage, GitHub Actions, pkgdown configuration, citation metadata, and an introductory vignette.
