
<!-- README.md is generated from README.Rmd. Please edit that file -->

# gtsummary4mice

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

Additional functions for using `tbl_uvsummary` with `mice::mids`
objects.

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
library(gtsummary)

# an example...
```
