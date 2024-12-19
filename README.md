
<!-- README.md is generated from README.Rmd. Please edit that file -->

# gtsummary4mice

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/jrob95/gtsummary4mice/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/jrob95/gtsummary4mice/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

Additional functions for using `tbl_uvsummary` with `mice::mids`
objects. Based on the `tbl_uvsummary` function from the `gtsummary`
package, which can be installed from CRAN

The goal of gtsummary4mice is to allow used of `mice` generating
multiply imputed data sets `mids` quickly and easily

## Installation

You can install the development version of `gtsummary4mice` like so:

``` r
# install.packages("pak")
pak::pak("jrob95/gtsummary4mice")
```

## Example

This how to use the `tbl_uvsummary` with objects generated from
`mice::mice`

``` r
library(gtsummary)
library(gtsummary4mice)
#> Registered S3 method overwritten by 'gtsummary4mice':
#>   method                from     
#>   tbl_uvregression.mids gtsummary
library(mice)
#> 
#> Attaching package: 'mice'
#> The following object is masked from 'package:stats':
#> 
#>     filter
#> The following objects are masked from 'package:base':
#> 
#>     cbind, rbind


  # Create a sample dataset with missing values
  set.seed(123)
  data <- data.frame(
    outcome = rbinom(100, 1, 0.5),
    predictor1 = rnorm(100),
    predictor2 = rnorm(100)
  )
  data$predictor2[sample(1:100, 20)] <- NA  # Introduce missing values

  # Create mids object using mice
  imputed_data <- mice::mice(data, m = 5, maxit = 5, seed = 123, printFlag = FALSE)

  # Run tbl_uvregression on mids object
  tbl <- tbl_uvregression(
    imputed_data,
    method = glm,
    y = outcome,
    exponentiate = TRUE
  )
```
