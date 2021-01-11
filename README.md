
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Antools: Analisys tools

<!-- badges: start -->
<!-- badges: end -->

The goal of antools to be a set of tools that serve as a complement to
the statistical analyses used in psychology.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("brianmsm/antools")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(antools)
## basic example code
```

``` r
library(lavaan)
#> This is lavaan 0.6-7
#> lavaan is BETA software! Please report any bugs.

HS.model <- " visual  =~ x1 + x2 + x3
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9 "

fit <- cfa(
  model = HS.model,
  data = HolzingerSwineford1939
)

fit_lavaan(fit)
#> # A tibble: 1 x 18
#>    nobs estimator ngroups converged chisq    df  pvalue  npar   CFI   TLI  RMSEA
#>   <int> <chr>       <int> <lgl>     <dbl> <dbl>   <dbl> <dbl> <dbl> <dbl>  <dbl>
#> 1   301 ML              1 TRUE       85.3    24 8.50e-9    21 0.931 0.896 0.0921
#> # … with 7 more variables: RMSEA.CI.LOWER <dbl>, RMSEA.CI.UPPER <dbl>,
#> #   SRMR <dbl>, WRMR <dbl>, AIC <dbl>, BIC <dbl>, missing_method <chr>
```
