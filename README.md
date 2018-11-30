
<!-- README.md is generated from README.Rmd. Please edit that file -->

# vistributions

> Visualize probability
distributions

[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/vistributions)](https://cran.r-project.org/package=vistributions)
[![cran
checks](https://cranchecks.info/badges/summary/vistributions)](https://cran.r-project.org/web/checks/check_results_vistributions.html)
[![Travis-CI Build
Status](https://travis-ci.org/rsquaredacademy/vistributions.svg?branch=master)](https://travis-ci.org/rsquaredacademy/vistributions)
[![AppVeyor Build
Status](https://ci.appveyor.com/api/projects/status/github/rsquaredacademy/vistributions?branch=master&svg=true)](https://ci.appveyor.com/project/aravindhebbali/vistributions)
[![lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)

## Installation

``` r
# Install release version from CRAN
install.packages("vistributions")

# Install development version from GitHub
# install.packages("devtools")
devtools::install_github("rsquaredacademy/vistributions")
```

## Shiny App

Use `vdist_launch_app()` to explore the package using a shiny app.

## Articles

  - [Explore
    Distributions](https://vistributions.rsquaredacademy.com/articles/introduction_to_vistributions.html)

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

## Community Guidelines

Please note that the ‘vistributions’ project is released with a
[Contributor Code of Conduct](CODE_OF_CONDUCT.md). By contributing to
this project, you agree to abide by its terms.
