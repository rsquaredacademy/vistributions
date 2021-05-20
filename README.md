
<!-- README.md is generated from README.Rmd. Please edit that file -->

# vistributions

> Visualize probability distributions

<!-- badges: start -->

[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/vistributions)](https://cran.r-project.org/package=vistributions)
[![cran
checks](https://cranchecks.info/badges/summary/vistributions)](https://cran.r-project.org/web/checks/check_results_vistributions.html)
[![R build
status](https://github.com/rsquaredacademy/vistributions/workflows/R-CMD-check/badge.svg)](https://github.com/rsquaredacademy/vistributions/actions)
[![Coverage
Status](https://img.shields.io/codecov/c/github/rsquaredacademy/vistributions/master.svg)](https://codecov.io/github/rsquaredacademy/vistributions?branch=master)
[![status](https://tinyverse.netlify.com/badge/vistributions)](https://CRAN.R-project.org/package=vistributions)
[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![](https://cranlogs.r-pkg.org/badges/grand-total/vistributions)](https://cran.r-project.org/package=vistributions)
<!-- badges: end -->

## Installation

``` r
# Install release version from CRAN
install.packages("vistributions")

# Install development version from GitHub
# install.packages("devtools")
devtools::install_github("rsquaredacademy/vistributions")
```

## Articles

-   [Explore
    Distributions](https://vistributions.rsquaredacademy.com/articles/introduction-to-vistributions.html)

## Usage

### Normal Distribution

``` r
# visualize normal distribution
vdist_normal_plot()
```

<img src="man/figures/README-normal-1.png" width="100%" />

``` r
# visualize quantiles out of given probability
vdist_normal_perc(0.95, mean = 2, sd = 1.36, type = 'both')
```

<img src="man/figures/README-normal-2.png" width="100%" />

``` r
# visualize probability from a given quantile
vdist_normal_prob(c(-1.74, 1.83), type = 'both')
```

<img src="man/figures/README-normal-3.png" width="100%" />

## Getting Help

If you encounter a bug, please file a minimal reproducible example using
[reprex](https://reprex.tidyverse.org/index.html) on github. For
questions and clarifications, use
[StackOverflow](https://stackoverflow.com/).
