# Visualize binomial distribution

Visualize how changes in number of trials and the probability of success
affect the shape of the binomial distribution. Compute & visualize
probability from a given quantile and quantiles out of given
probability.

## Usage

``` r
vdist_binom_plot(n = 10, p = 0.3, print_plot = TRUE)

vdist_binom_prob(
  n = 10,
  p = 0.3,
  s = 4,
  type = c("lower", "upper", "exact", "interval"),
  print_plot = TRUE
)

vdist_binom_perc(
  n = 10,
  p = 0.5,
  tp = 0.05,
  type = c("lower", "upper"),
  print_plot = TRUE
)
```

## Arguments

- n:

  Number of trials.

- p:

  Probability of success in a trial.

- print_plot:

  logical; if `TRUE`, prints the plot else returns a plot object.

- s:

  Number of success.

- type:

  Lower/upper/exact/interval.

- tp:

  Aggregate probability.

## Value

An object of class `ggplot` (invisibly if `print_plot = TRUE`).

## See also

[`Binomial`](https://rdrr.io/r/stats/Binomial.html)

## Examples

``` r
# visualize binomial distribution
vdist_binom_plot(10, 0.3)


# visualize probability from a given quantile
vdist_binom_prob(10, 0.3, 4, type = 'exact')

vdist_binom_prob(10, 0.3, 4, type = 'lower')

vdist_binom_prob(10, 0.3, 4, type = 'upper')

vdist_binom_prob(10, 0.3, c(4, 6), type = 'interval')



# visualize quantiles out of given probability
vdist_binom_perc(10, 0.5, 0.05)

vdist_binom_perc(10, 0.5, 0.05, "upper")

```
