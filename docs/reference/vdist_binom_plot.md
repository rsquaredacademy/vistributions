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

  Number of trials (a single non-negative whole number).

- p:

  Probability of success in each trial.

- print_plot:

  logical; if `TRUE`, prints the plot else returns a plot object.

- s:

  Number of successes: whole numbers in `[0, n]` only. Zero and `n` are
  counts of successes like any other; use
  [`pbinom()`](https://rdrr.io/r/stats/Binomial.html) for an arbitrary
  threshold outside that range. For `type = "interval"`, two values in
  increasing order.

- type:

  Lower/upper/exact/interval.

- tp:

  Tail probability for quantile calculation, strictly between 0 and 1
  and either equal to a cumulative-probability jump or more than 1e-12
  away from it. For `type = "lower"` the cutoff is the smallest `k` with
  `P(X <= k) >= tp` (as
  [`qbinom()`](https://rdrr.io/r/stats/Binomial.html)); for
  `type = "upper"` it is the smallest `k` with `P(X > k) <= tp`, and the
  bars with `x > k` are shaded.

## Value

An object of class `ggplot` (invisibly if `print_plot = TRUE`).

## Details

Probabilities below 1e-10% (including underflows) are shown as \<
1e-10%, with the corresponding upper display \> 99.9999999999%.

Every bar is one count while `n + 1` counts fit in the plotting grid
(2001 bars). A wider support is drawn in approximately 2001 bins of
counts; event boundaries can split up to two bins, each bar showing the
exact probability of its bin, with a caption that discloses the
grouping; the grid therefore has a bounded number of points whatever `n`
is. Probabilities, quantiles and shaded masses always come from
[`dbinom()`](https://rdrr.io/r/stats/Binomial.html),
[`pbinom()`](https://rdrr.io/r/stats/Binomial.html) and
[`qbinom()`](https://rdrr.io/r/stats/Binomial.html).

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
