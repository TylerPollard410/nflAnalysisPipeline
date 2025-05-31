
<!-- README.md is generated from README.Rmd. Please edit that file -->

# nflAnalysisPipeline

<!-- badges: start -->

<!-- badges: end -->

This repository contains the private NFL analytics pipeline:
feature‐engineering functions, model‐fitting code, and nightly ETL
scripts. Outputs (Parquet & QS files) will be pushed to the public
`nflanalysisapp` repo via GitHub Actions.

## Installation

You can install the development version of nflAnalysisPipeline from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("TylerPollard410/nflAnalysisPipeline")
```
