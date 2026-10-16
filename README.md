
<!-- README.md is generated from README.Rmd. Please edit that file -->

# vistributions <img src="man/figures/logo.png" align="right" height="139" />

> Visualize probability distributions

<!-- badges: start -->

[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/vistributions)](https://cran.r-project.org/package=vistributions)
[![R-CMD-check](https://github.com/rsquaredacademy/vistributions/actions/workflows/R-CMD-check.yaml/badge.svg?branch=develop)](https://github.com/rsquaredacademy/vistributions/actions/workflows/R-CMD-check.yaml)
[![Coverage
Status](https://img.shields.io/codecov/c/github/rsquaredacademy/vistributions/develop.svg)](https://app.codecov.io/github/rsquaredacademy/vistributions?branch=develop)
<!-- badges: end -->

`vistributions` visualizes the normal, t, F, chi-square and binomial
distributions with `ggplot2`. Each family exposes the same grammar:
`_plot` for the shape, `_perc` for quantiles from probabilities and
`_prob` for probabilities from quantiles, with shaded tails and
annotated cutoffs for teaching and publication.

## Installation

``` r
# Install release version from CRAN
install.packages("vistributions")

# Install development version from GitHub
# install.packages("devtools")
devtools::install_github("rsquaredacademy/vistributions")
```

## Supported distributions

| Family | Shape | Quantile from p | Probability from q |
|----|----|----|----|
| Normal | `vdist_normal_plot()` | `vdist_normal_perc()` | `vdist_normal_prob()` |
| t | `vdist_t_plot()` | `vdist_t_perc()` | `vdist_t_prob()` |
| F | `vdist_f_plot()` | `vdist_f_perc()` | `vdist_f_prob()` |
| Chi-square | `vdist_chisquare_plot()` | `vdist_chisquare_perc()` | `vdist_chisquare_prob()` |
| Binomial | `vdist_binom_plot()` | `vdist_binom_perc()` | `vdist_binom_prob()` |

All plotting functions print by default and return the `ggplot` object
invisibly. Pass `print_plot = FALSE` to get a composable plot object for
further customization with `+ labs()`, `+ theme_minimal()` or
`ggplot2::ggsave()`.

## Articles

- [Explore
  Distributions](https://vistributions.rsquaredacademy.com/articles/introduction-to-vistributions.html)

## Usage

### Normal distribution

``` r
# distribution shape
vdist_normal_plot()
```

<img src="man/figures/README-normal-plot-1.png" width="100%" />

``` r

# quantile from a probability (upper 10%)
vdist_normal_perc(0.10, mean = 60, sd = 3, type = "upper")
```

<img src="man/figures/README-normal-plot-2.png" width="100%" />

``` r

# probability from quantiles (middle 50%)
vdist_normal_prob(c(85, 100), mean = 90, sd = 4, type = "both")
```

<img src="man/figures/README-normal-plot-3.png" width="100%" />

### t distribution

``` r
vdist_t_plot(df = 8)
```

<img src="man/figures/README-t-plot-1.png" width="100%" />

``` r

# upper 15th percentile
vdist_t_perc(0.15, df = 8, type = "upper")
```

<img src="man/figures/README-t-plot-2.png" width="100%" />

``` r

# probability between -2 and 2
vdist_t_prob(2, df = 6, type = "interval")
```

<img src="man/figures/README-t-plot-3.png" width="100%" />

### Chi-square distribution

``` r
vdist_chisquare_plot(df = 5)
```

<img src="man/figures/README-chisquare-plot-1.png" width="100%" />

``` r

# tenth percentile
vdist_chisquare_perc(0.10, df = 8, type = "lower")
```

<img src="man/figures/README-chisquare-plot-2.png" width="100%" />

``` r

# P(X > 8.79) with 12 df
vdist_chisquare_prob(8.79, df = 12, type = "upper")
```

<img src="man/figures/README-chisquare-plot-3.png" width="100%" />

### F distribution

``` r
vdist_f_plot()
```

<img src="man/figures/README-f-plot-1.png" width="100%" />

``` r

# upper twentieth percentile, F(4, 5)
vdist_f_perc(0.20, num_df = 4, den_df = 5, type = "upper")
```

<img src="man/figures/README-f-plot-2.png" width="100%" />

``` r

# P(X > 3.89), F(4, 5)
vdist_f_prob(3.89, num_df = 4, den_df = 5, type = "upper")
```

<img src="man/figures/README-f-plot-3.png" width="100%" />

### Binomial distribution

``` r
vdist_binom_plot(12, 0.2)
```

<img src="man/figures/README-binom-plot-1.png" width="100%" />

``` r

# exactly 4 successes out of 12
vdist_binom_prob(12, 0.2, 4, type = "exact")
```

<img src="man/figures/README-binom-plot-2.png" width="100%" />

``` r

# at most 1 success
vdist_binom_prob(12, 0.2, 1, type = "lower")
```

<img src="man/figures/README-binom-plot-3.png" width="100%" />

### Customize the plot object

``` r
library(ggplot2)

p <- vdist_normal_plot(mean = 60, sd = 3, print_plot = FALSE)
p + theme_minimal() + labs(title = "Normal(60, 3)")
```

<img src="man/figures/README-composable-1.png" width="100%" />

## How it compares

- [visualize](https://cran.r-project.org/package=visualize) covers more
  families with base graphics; `vistributions` focuses on five teaching
  families with annotated `ggplot2` output.
- [mosaic](https://cran.r-project.org/package=mosaic) (`xpnorm()` and
  friends) is course-framework bound; `vistributions` works standalone.
- [distributional](https://pkg.mitchelloharawild.com/distributional/) +
  [ggdist](https://mjskay.github.io/ggdist/) is the modeling-grade
  stack; `vistributions` is the lightweight classroom companion. Native
  `distributional` dispatch is planned for v1.0 (see `roadmap/`).

## Getting Help

If you encounter a bug, please file a minimal reproducible example using
[reprex](https://reprex.tidyverse.org/index.html) on
[GitHub](https://github.com/rsquaredacademy/vistributions/issues). For
questions and clarifications, please use Stack Overflow (tagged
`vistributions` or `r`).
