# Visualize chi square distribution

Visualize how changes in degrees of freedom affect the shape of the chi
square distribution. Compute & visualize quantiles out of given
probability and probability from a given quantile.

## Usage

``` r
vdist_chisquare_plot(
  df = 3,
  normal = FALSE,
  xaxis_range = NULL,
  print_plot = TRUE
)

vdist_chisquare_perc(
  probs = 0.95,
  df = 3,
  type = c("lower", "upper"),
  print_plot = TRUE
)

vdist_chisquare_prob(
  perc = 13,
  df = 11,
  type = c("lower", "upper"),
  print_plot = TRUE
)
```

## Arguments

- df:

  Degrees of freedom (at least 0.1; need not be a whole number).

- normal:

  If `TRUE`, normal curve with same `mean` and `sd` as the chi square
  distribution is drawn.

- xaxis_range:

  Upper X axis limit; by default the view spans four standard deviations
  around df, clipped at zero.

- print_plot:

  logical; if `TRUE`, prints the plot else returns a plot object.

- probs:

  Probability value, strictly between 0 and 1 and within
  `(1e-12, 1 - 1e-12)`.

- type:

  Lower tail or upper tail.

- perc:

  Quantile value.

## Value

An object of class `ggplot` (invisibly if `print_plot = TRUE`).

## Details

Percentages below 1e-10% are displayed as \< 1e-10%, and probabilities
within 1e-10% of 100% as \> 99.9999999999%.

Drawing contract: every probability, cutoff and caption is computed from
the chi-square density, distribution and quantile functions, never from
the drawn grid. The shading polygon starts at the support (zero), not at
a quantile floor, so no probability is discarded for small `df`; it
spans the whole event and its own area is checked against the exact
event probability (within 1e-6 relative error above the display floor).
Where the density is unbounded at zero it is capped on the display axis
only: the source-coordinate area and the printed probability always use
the unclipped density. Captions use one rounding rule and are true at
the precision they print. A shaded region too narrow to occupy a pixel
at the plotted scale (for example the lower 5% tail of `df = 0.1`) is
marked with a visible strip in the panel and disclosed in the caption;
its polygon and its checked area are unchanged. Every plotting grid has
a bounded number of points, whatever the range or degrees of freedom.

## See also

[`Chisquare`](https://rdrr.io/r/stats/Chisquare.html)

## Examples

``` r
# visualize chi square distribution
vdist_chisquare_plot()

vdist_chisquare_plot(df = 5)

vdist_chisquare_plot(df = 5, normal = TRUE)


# visualize quantiles out of given probability
vdist_chisquare_perc(0.165, 8, 'lower')

vdist_chisquare_perc(0.22, 13, 'upper')


# visualize probability from a given quantile.
vdist_chisquare_prob(13.58, 11, 'lower')

vdist_chisquare_prob(15.72, 13, 'upper')

```
