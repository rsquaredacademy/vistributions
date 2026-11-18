# Visualize t distribution

Visualize how degrees of freedom affect the shape of t distribution,
visualize quantiles out of given probability and probability from a
given quantile.

## Usage

``` r
vdist_t_plot(df = 3, print_plot = TRUE)

vdist_t_perc(
  probs = 0.95,
  df = 4,
  type = c("lower", "upper", "both"),
  print_plot = TRUE
)

vdist_t_prob(
  perc = 1.6,
  df = 7,
  type = c("lower", "upper", "interval", "both"),
  print_plot = TRUE
)
```

## Arguments

- df:

  Degrees of freedom (positive; need not be a whole number).

- print_plot:

  logical; if `TRUE`, prints the plot else returns a plot object.

- probs:

  Probability value, strictly between 0 and 1 and within
  `(1e-12, 1 - 1e-12)`.

- type:

  Lower tail, upper tail, interval or both.

- perc:

  Quantile value. For `type = "interval"` and `type = "both"` the event
  is defined by `|perc|`, so a negative value gives the same result as
  its absolute value.

## Value

An object of class `ggplot` (invisibly if `print_plot = TRUE`).

## Details

Percentages below 1e-10% are displayed as \< 1e-10%, and probabilities
within 1e-10% of 100% as \> 99.9999999999%.

Drawing contract: every probability, cutoff and caption is computed from
the t density, distribution and quantile functions, never from the drawn
grid. The shaded polygon spans the whole event (clipped only by the
panel) and is sampled on a grid spaced in probability, with a fixed
number of points; its own area is checked against the exact event
probability and kept within 1e-6 relative error for events above the
display floor. Captions use one rounding rule and are true at the
precision they print. A very wide window - for example a heavy tail with
a small `df` - is drawn on a symmetric log axis so the shaded event
stays visible; the printed probability and the area check always use the
unclipped density. A shaded region too narrow to occupy a pixel at the
plotted scale is marked with a visible strip in the panel and disclosed
in the caption; its polygon and its checked area are unchanged. Every
plotting grid has a bounded number of points, whatever the degrees of
freedom or the cutoff.

## See also

[`TDist`](https://rdrr.io/r/stats/TDist.html)

## Examples

``` r
# visualize t distribution
vdist_t_plot()

vdist_t_plot(6)


# visualize quantiles out of given probability
vdist_t_perc(probs = 0.95, df = 4, type = 'lower')

vdist_t_perc(probs = 0.35, df = 4, type = 'upper')

vdist_t_perc(probs = 0.69, df = 7, type = 'both')


# visualize probability from a given quantile
vdist_t_prob(2.045, 7, 'lower')

vdist_t_prob(0.945, 7, 'upper')

vdist_t_prob(1.445, 7, 'interval')

vdist_t_prob(1.6, 7, 'both')

```
