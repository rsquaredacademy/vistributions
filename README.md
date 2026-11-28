
<!-- README.md is generated from README.Rmd. Please edit that file -->

# vistributions <img src="man/figures/logo.png" align="right" height="139" alt="vistributions logo" />

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/vistributions)](https://CRAN.R-project.org/package=vistributions)
[![R-CMD-check](https://github.com/rsquaredacademy/vistributions/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/rsquaredacademy/vistributions/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/rsquaredacademy/vistributions/branch/develop/graph/badge.svg)](https://app.codecov.io/gh/rsquaredacademy/vistributions?branch=develop)
<!-- badges: end -->

vistributions draws shaded normal, t, F, chi-square and binomial curves
for analysts who need probabilities without tables.

## Installation

``` r
# Release version from CRAN
install.packages("vistributions")

# Development version from GitHub
# install.packages("pak")
pak::pak("rsquaredacademy/vistributions")
```

## Quick example

### See the shape of daily orders

In this example, a store’s daily orders average 25.2 units, with a
standard deviation of 5.2 units. What shape does that describe?

``` r
vdist_normal_plot(mean = 25.2, sd = 5.2)
```

<img src="man/figures/README-normal-plot-1.png" alt="Normal curve for daily orders, centred on 25.2 units with a standard deviation of 5.2. Shaded bands mark one, two and three standard deviations around the mean." width="75%" />

The mean is 25.2 orders a day. The standard deviation is 5.2 orders, the
typical distance from the mean; most days sit near the dark centre.

### Find the chance of a busy day

How often would a day sell more than 35 orders?

``` r
vdist_normal_prob(35, mean = 25.2, sd = 5.2, type = "upper")
```

<img src="man/figures/README-normal-prob-1.png" alt="Normal curve with the area above 35 orders shaded. The caption reads P(X &gt; 35) = 3.0%." width="75%" />

The upper-tail probability is the share of days above the cutoff. Under
this curve, 3.0% of days sell more than 35 orders.

### Set a stock level for the busiest days

What order level do only the top 10% of days exceed?

``` r
vdist_normal_perc(0.10, mean = 25.2, sd = 5.2, type = "upper")
```

<img src="man/figures/README-normal-perc-1.png" alt="Normal curve with the area above 31.864 orders shaded. The caption reads P(X &gt; 31.864) = 10.0%." width="75%" />

A percentile turns a share into a level on the curve. Only the busiest
10% of days sell more than 31.864 orders.

The t, F, chi-square and binomial families follow the same `_plot`,
`_perc` and `_prob` pattern.

## Learn more

- [Documentation site](https://vistributions.rsquaredacademy.com)
- Vignettes:
  - [Exploring
    Distributions](https://vistributions.rsquaredacademy.com/articles/introduction-to-vistributions.html)
- [NEWS](https://vistributions.rsquaredacademy.com/news/index.html)

## Getting help and contributing

**Help:** open an
[issue](https://github.com/rsquaredacademy/vistributions/issues) with a
minimal reproducible example, built with
[reprex](https://reprex.tidyverse.org/).

**Contributing:** read the [contributing
guide](https://github.com/rsquaredacademy/vistributions/blob/develop/.github/CONTRIBUTING.md).

**Code of Conduct:** Please note that the vistributions project is
released with a [Contributor Code of
Conduct](https://vistributions.rsquaredacademy.com/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.

**Citation:** run `citation("vistributions")`.

<div data-rsa-signup="" data-site="vistributions.rsquaredacademy.com"
data-privacy-url="https://www.rsquaredacademy.com/privacy-policy.html">

</div>
