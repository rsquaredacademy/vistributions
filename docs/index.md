# vistributions

> Visualize probability distributions

`vistributions` visualizes the normal, t, F, chi-square and binomial
distributions with `ggplot2`. Each family exposes the same grammar:
`_plot` for the shape, `_perc` for quantiles from probabilities and
`_prob` for probabilities from quantiles, with shaded tails and
annotated cutoffs for teaching and publication.

## Installation

`# Install release version from CRAN`` `[`install.packages`](https://rdrr.io/r/utils/install.packages.html)`(``"vistributions"``)`` `` ``# Install development version from GitHub`` ``# install.packages("devtools")`` ``devtools``::`[`install_github`](https://devtools.r-lib.org/reference/install-deprecated.html)`(``"rsquaredacademy/vistributions"``)`

## Supported distributions

| Family | Shape | Quantile from p | Probability from q |
|----|----|----|----|
| Normal | [`vdist_normal_plot()`](https://vistributions.rsquaredacademy.com/reference/vdist_normal_plot.md) | [`vdist_normal_perc()`](https://vistributions.rsquaredacademy.com/reference/vdist_normal_plot.md) | [`vdist_normal_prob()`](https://vistributions.rsquaredacademy.com/reference/vdist_normal_plot.md) |
| t | [`vdist_t_plot()`](https://vistributions.rsquaredacademy.com/reference/vdist_t_plot.md) | [`vdist_t_perc()`](https://vistributions.rsquaredacademy.com/reference/vdist_t_plot.md) | [`vdist_t_prob()`](https://vistributions.rsquaredacademy.com/reference/vdist_t_plot.md) |
| F | [`vdist_f_plot()`](https://vistributions.rsquaredacademy.com/reference/vdist_f_plot.md) | [`vdist_f_perc()`](https://vistributions.rsquaredacademy.com/reference/vdist_f_plot.md) | [`vdist_f_prob()`](https://vistributions.rsquaredacademy.com/reference/vdist_f_plot.md) |
| Chi-square | [`vdist_chisquare_plot()`](https://vistributions.rsquaredacademy.com/reference/vdist_chisquare_plot.md) | [`vdist_chisquare_perc()`](https://vistributions.rsquaredacademy.com/reference/vdist_chisquare_plot.md) | [`vdist_chisquare_prob()`](https://vistributions.rsquaredacademy.com/reference/vdist_chisquare_plot.md) |
| Binomial | [`vdist_binom_plot()`](https://vistributions.rsquaredacademy.com/reference/vdist_binom_plot.md) | [`vdist_binom_perc()`](https://vistributions.rsquaredacademy.com/reference/vdist_binom_plot.md) | [`vdist_binom_prob()`](https://vistributions.rsquaredacademy.com/reference/vdist_binom_plot.md) |

All plotting functions print by default and return the `ggplot` object
invisibly. Pass `print_plot = FALSE` to get a composable plot object for
further customization with `+ labs()`, `+ theme_minimal()` or
[`ggplot2::ggsave()`](https://ggplot2.tidyverse.org/reference/ggsave.html).

## Articles

- [Explore
  Distributions](https://vistributions.rsquaredacademy.com/articles/introduction-to-vistributions.html)

## Usage

### Normal distribution

`# distribution shape`` `[`vdist_normal_plot`](https://vistributions.rsquaredacademy.com/reference/vdist_normal_plot.md)`(``)`

![](reference/figures/README-normal-plot-1.png)

` ``# quantile from a probability (upper 10%)`` `[`vdist_normal_perc`](https://vistributions.rsquaredacademy.com/reference/vdist_normal_plot.md)`(``0.10``, mean ``=`` ``60``, sd ``=`` ``3``, type ``=`` ``"upper"``)`

![](reference/figures/README-normal-plot-2.png)

` ``# probability from quantiles (middle 50%)`` `[`vdist_normal_prob`](https://vistributions.rsquaredacademy.com/reference/vdist_normal_plot.md)`(`[`c`](https://rdrr.io/r/base/c.html)`(``85``, ``100``)``, mean ``=`` ``90``, sd ``=`` ``4``, type ``=`` ``"both"``)`

![](reference/figures/README-normal-plot-3.png)

### t distribution

[`vdist_t_plot`](https://vistributions.rsquaredacademy.com/reference/vdist_t_plot.md)`(``df ``=`` ``8``)`

![](reference/figures/README-t-plot-1.png)

` ``# upper 15th percentile`` `[`vdist_t_perc`](https://vistributions.rsquaredacademy.com/reference/vdist_t_plot.md)`(``0.15``, df ``=`` ``8``, type ``=`` ``"upper"``)`

![](reference/figures/README-t-plot-2.png)

` ``# probability between -2 and 2`` `[`vdist_t_prob`](https://vistributions.rsquaredacademy.com/reference/vdist_t_plot.md)`(``2``, df ``=`` ``6``, type ``=`` ``"interval"``)`

![](reference/figures/README-t-plot-3.png)

### Chi-square distribution

[`vdist_chisquare_plot`](https://vistributions.rsquaredacademy.com/reference/vdist_chisquare_plot.md)`(``df ``=`` ``5``)`

![](reference/figures/README-chisquare-plot-1.png)

` ``# tenth percentile`` `[`vdist_chisquare_perc`](https://vistributions.rsquaredacademy.com/reference/vdist_chisquare_plot.md)`(``0.10``, df ``=`` ``8``, type ``=`` ``"lower"``)`

![](reference/figures/README-chisquare-plot-2.png)

` ``# P(X > 8.79) with 12 df`` `[`vdist_chisquare_prob`](https://vistributions.rsquaredacademy.com/reference/vdist_chisquare_plot.md)`(``8.79``, df ``=`` ``12``, type ``=`` ``"upper"``)`

![](reference/figures/README-chisquare-plot-3.png)

### F distribution

[`vdist_f_plot`](https://vistributions.rsquaredacademy.com/reference/vdist_f_plot.md)`(``)`

![](reference/figures/README-f-plot-1.png)

` ``# upper twentieth percentile, F(4, 5)`` `[`vdist_f_perc`](https://vistributions.rsquaredacademy.com/reference/vdist_f_plot.md)`(``0.20``, num_df ``=`` ``4``, den_df ``=`` ``5``, type ``=`` ``"upper"``)`

![](reference/figures/README-f-plot-2.png)

` ``# P(X > 3.89), F(4, 5)`` `[`vdist_f_prob`](https://vistributions.rsquaredacademy.com/reference/vdist_f_plot.md)`(``3.89``, num_df ``=`` ``4``, den_df ``=`` ``5``, type ``=`` ``"upper"``)`

![](reference/figures/README-f-plot-3.png)

### Binomial distribution

[`vdist_binom_plot`](https://vistributions.rsquaredacademy.com/reference/vdist_binom_plot.md)`(``12``, ``0.2``)`

![](reference/figures/README-binom-plot-1.png)

` ``# exactly 4 successes out of 12`` `[`vdist_binom_prob`](https://vistributions.rsquaredacademy.com/reference/vdist_binom_plot.md)`(``12``, ``0.2``, ``4``, type ``=`` ``"exact"``)`

![](reference/figures/README-binom-plot-2.png)

` ``# at most 1 success`` `[`vdist_binom_prob`](https://vistributions.rsquaredacademy.com/reference/vdist_binom_plot.md)`(``12``, ``0.2``, ``1``, type ``=`` ``"lower"``)`

![](reference/figures/README-binom-plot-3.png)

### Customize the plot object

[`library`](https://rdrr.io/r/base/library.html)`(`[`ggplot2`](https://ggplot2.tidyverse.org)`)`` `` ``p`` ``<-`` `[`vdist_normal_plot`](https://vistributions.rsquaredacademy.com/reference/vdist_normal_plot.md)`(``mean ``=`` ``60``, sd ``=`` ``3``, print_plot ``=`` ``FALSE``)`` ``p`` ``+`` `[`theme_minimal`](https://ggplot2.tidyverse.org/reference/ggtheme.html)`(``)`` ``+`` `[`labs`](https://ggplot2.tidyverse.org/reference/labs.html)`(``title ``=`` ``"Normal(60, 3)"``)`

![](reference/figures/README-composable-1.png)

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
