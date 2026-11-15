# vistributions 0.2.2

## Bug Fixes

* `vdist_t_*()`, `vdist_chisquare_*()` and `vdist_f_*()` no longer truncate non-integer degrees of freedom to whole numbers; probabilities, quantiles, densities and plots now use the `df` that was supplied. Chi-square `df` must be at least 0.1 and F `num_df` must be at least 1; smaller values cannot be drawn from their support boundary at double precision and now give a documented error instead of a spike or an infinite density.
* `vdist_normal_perc()`, `vdist_t_perc()`, `vdist_chisquare_perc()` and `vdist_f_perc()` no longer round the quantile to 3 decimals before drawing it; the shaded areas, dashed lines and probabilities use the exact cutoff and only the label is formatted. Labels keep enough digits for the printed event to be numerically true, relative to both the cutoff magnitude and the distribution's scale (for example `vdist_normal_perc(.95, mean = 1, sd = 1e-13)` prints `P(X < 1.0000000000001645) = 95%`). If no printable double reproduces the requested probability, the function stops with a numerical-resolution error.
* Quantiles and probability limits are checked before plotting. Normal and t probabilities must lie in `[1e-10, 1-1e-10]`; chi-square and F probabilities must lie in `[1e-6, 1-1e-6]`. A `q*()` result that is non-finite or does not invert the requested tail is a package error, never a caption or an infinite polygon. A cutoff beyond the visible central mass is still drawn clipped to that view while the caption keeps its true value.
* Continuous curves and shaded polygons use a fixed number of points across the visible range, sampled in probability space and unioned with a fixed-count uniform grid; no step in data units (such as 0.01) is used. Ordinary inputs such as `vdist_normal_perc(.95, sd = .1)`, `vdist_normal_perc(.5, type = "both")`, `vdist_chisquare_plot(2.01)` and `vdist_chisquare_prob(0, 2.01)` now plot instead of failing an area check. A density whose central 99.9998% mass spans less than 0.001 is still rejected as too concentrated.
* Shaded regions are checked against the event the caption names: the drawn polygon plus the part of the event outside the visible range must equal the stated probability within 1e-3. Upper-tail chi-square and F events now extend the view far enough to include the mass, so `vdist_chisquare_perc(.05, 1, type = "upper")`, `vdist_f_perc(.05, 5, 10, type = "upper")` and the vignette's F upper-tail examples shade the stated area.
* Translated tiny-scale normals are rejected when `sd / max(1, |mean|)` is below the documented limit `1e-14`, because the cutoff cannot be represented as a double at that location. The error states the limit. Zero-centred tiny scales (for example `vdist_normal_plot(sd = 1e-300)`) remain valid.
* Captions print probabilities at the precision the area check guarantees: percentages at or above 0.1 keep one decimal, smaller ones use scientific notation. The printed cutoff is checked against that displayed probability, and interval/tail probabilities are kept unrounded until after the union arithmetic. For example `vdist_t_prob(.1, 7, "interval")` now prints 7.7% and `type = "both"` prints 92.3%.
* Normal interval annotations are placed at the midpoint of the visible cutoffs, so an interval far outside the curve no longer expands the panel and compresses the density.
* The chi-square `df >= 0.1` floor is now enforced by all three exports at that documented boundary; the F concentration limit is stated in the help page (`?vdist_f_plot`).
* `vdist_binom_plot()`, `vdist_binom_prob()` and `vdist_binom_perc()` now require `n` to be a non-negative whole number and `s` to be whole numbers, instead of truncating them and labelling the probability of a different event. A whole number stored as a double (for example `(.7 - .4) * 10`) is accepted and rounded once, so `n`, the plotted support and the success counts all describe the same distribution.
* `vdist_binom_prob(type = "interval")` gives an error when the endpoints are reversed, instead of a negative probability; `vdist_normal_prob()` does the same for reversed `perc` values.
* `vdist_binom_perc()` states the quantile definition correctly (`P(X <= k - 1) < tp` but `P(X <= k) >= tp`) and the upper tail now shades `x > k`, so the shaded mass is the reported tail probability. `tp` must now be strictly between 0 and 1.
* `vdist_t_prob(type = "interval")` and `type = "both"` with a negative `perc` now use `|perc|` for the caption, reference lines and annotations as well as the probability.
* `vdist_binom_plot()` and `vdist_binom_prob()` compute the displayed standard deviation from `sqrt(n * p * (1 - p))` rather than from the rounded mean.
* `probs = 0` or `probs = 1` in the `*_perc()` functions for the normal, t, F and chi-square distributions now give an informative error (the quantile is infinite and cannot be plotted).
* Missing, empty, infinite or vector arguments (for example `sd = NA`, `df = numeric()`, `p = c(.2, .3)`, `perc = numeric()` in `vdist_normal_prob()`) now give a clear package error instead of an internal one, and vector `s` is no longer silently recycled in `vdist_binom_prob()`.
* `vdist_f_prob()` and `vdist_chisquare_prob()` accept cutoffs below zero (probability 0 / 1) instead of failing.
* `vdist_chisquare_plot(normal = TRUE)`, `vdist_chisquare_perc()` and `vdist_chisquare_prob()` draw the chi-square distribution only on its non-negative support; for `df < 2`, where the density is unbounded at zero, the grid starts at the 1e-6 quantile instead of a negative value or an infinite density. The F distribution is drawn the same way for `num_df < 2`.
* The normal curve overlaid on `vdist_chisquare_plot(normal = TRUE)` uses the supplied degrees of freedom and `sqrt(2 * df)` rather than their three-decimal display values.
* The normal curve overlaid on `vdist_f_plot(normal = TRUE)` uses the unrounded F mean and standard deviation, so a valid large-df overlay no longer collapses to a zero-variance point mass. When the F variance is undefined (`den_df <= 4`), the overlay is omitted with a message instead of drawing `Std Dev. = NA`.
* Reference lines, markers and probability annotations are clamped to the plotted view. Extreme cutoffs therefore no longer extend the panel range (for example `vdist_t_prob(1e6, 7)` now shows its density instead of a vertical line) while the caption keeps the true event.

## Other

* Maintainer e-mail updated to aravindhebbali@rsquaredacademy.com.
* README: corrected the comment on the `vdist_normal_prob()` example.

# vistributions 0.2.1

## Bug Fixes

* `vdist_normal_plot()` now properly scales the x-axis grid to span `mean ± 4 * sd` (BUG-01).
* `vdist_f_perc()` adapts upper x-axis limit dynamically beyond 4.0 when quantiles exceed the default plotting range (BUG-02).
* `vdist_t_prob()` handles negative percentile values (`perc <= -5`) without "wrong sign in 'by' argument" sequence error (BUG-03).
* `vdist_chisquare_plot()` ensures shaded fill polygon coordinates match the full extent of `xaxis_range` (BUG-04).
* Guarded theoretical standard deviation calculation for the F distribution against division-by-zero when denominator degrees of freedom is less than or equal to 4 (`den_df <= 4`) (BUG-05).
* `vdist_t_prob(type = "both")` corrects color mapping and annotation placement so left/right shaded tails and central body match their respective regions (BUG-06).
* `check_positive()` now strictly enforces positive values (`val <= 0`), rejecting zero and negative inputs (BUG-07).
* `check_suggests()` formats informative error messages identifying the missing package prior to aborting execution (BUG-08).
* `vdist_binom_prob()` validates numeric types for `n`, `p`, and `s` before performing range checks (BUG-09).
* `check_range()` replaces bitwise `|` with logical `||` to eliminate scalar evaluation warnings in modern R (BUG-10).
* Visual test snapshot names for `vdist_t_prob()` interval and both types are properly aligned (BUG-11).
* Removed unprompted network polling (`available.packages()`) and interactive prompts during package startup in `.onAttach()`; retained rotating educational tips (BUG-12).
* `vdist_normal_perc()` scales x-axis bounds dynamically with standard deviation (BUG-13).
* `vdist_binom_prob(type = "upper")` strictly shades bars where `x >= s` (BUG-14).
* `vdist_binom_prob(type = "exact")` matches success counts via integer equality rather than floating-point density comparison (BUG-15).
* `vdist_t_prob(type = "upper")` aligns text annotation colors with polygon fill colors (BUG-16).
* Added missing positive degrees of freedom validations across t, F, and chi-square functions (BUG-17).
* F distribution normal approximation overlay is strictly clamped to non-negative support (`x >= 0`) (BUG-18).
* `vdist_binom_perc()` allows percentile input `tp` across the full valid range (0, 1) rather than capping at 0.5 (BUG-19).

## Documentation & Vignettes

* Fixed pedagogical and numerical discrepancies in `vignettes/introduction-to-vistributions.Rmd`:
  - Corrected Normal 15th percentile problem cutoff (`0.15` instead of `0.85`).
  - Aligned Binomial sample size ($n = 12$ consistently).
  - Synchronized Chi-square question text with lower-tail probability evaluation.
  - Aligned Student's t 95% interval problem (`0.95` instead of `0.8`).
  - Fixed inversion of Student's t `interval` vs `both` probability examples.
  - Standardized knitr chunk options (`fig.align = 'center'`).
  - Corrected typographic errors.
* Added explicit `@return` documentation across all exported plotting and calculation functions.
* Added `.github/CONTRIBUTING.md`, issue templates, PR template, and upgraded `CODE_OF_CONDUCT.md` to Contributor Covenant v2.1.

## Package Hygiene & Infrastructure

* Removed stale SVG files in `tests/figs/` and updated test suite to testthat 3rd edition (`Config/testthat/edition: 3`).
* Bumped dependency requirement to `R (>= 4.1.0)`.
* Removed unused `magrittr` dependency from `Imports:`.
* Configured automated `pkgdown` website deployment via Netlify.

# vistributions 0.2.0

This is a minor release for bug fixes.

# vistributions 0.1.2

This is a patch release to fix CRAN note about lazy data.

# vistributions 0.1.1

This is a patch release to fix bugs in the app.

# vistributions 0.1.0

* Added a `NEWS.md` file to track changes to the package.
