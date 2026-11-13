# vistributions 0.2.2

## Bug Fixes

* `vdist_t_*()`, `vdist_chisquare_*()` and `vdist_f_*()` no longer truncate non-integer degrees of freedom to whole numbers; probabilities, quantiles, densities and plots now use the `df` that was supplied, including `df` below 1.
* `vdist_normal_perc()`, `vdist_t_perc()`, `vdist_chisquare_perc()` and `vdist_f_perc()` no longer round the quantile to 3 decimals before drawing it; the shaded areas, dashed lines and probabilities use the exact cutoff and only the label is formatted (more decimals are shown for very small scales).
* Quantiles beyond the default plotting range (for example `vdist_t_perc(.95, df = 1)`, `vdist_chisquare_perc(.999, df = 1)`, `vdist_normal_perc(.9999999)`) no longer fail with "wrong sign in 'by' argument": the plotting grid extends to cover the cutoff, with a bounded number of points for extreme values.
* Shaded regions are drawn with at least 100 points, so plots of distributions with a very small scale (for example `vdist_normal_plot(sd = .001)`) no longer shade a triangle instead of the area under the curve.
* `vdist_binom_plot()`, `vdist_binom_prob()` and `vdist_binom_perc()` now require `n` to be a non-negative whole number and `s` to be whole numbers, instead of truncating them and labelling the probability of a different event.
* `vdist_binom_prob(type = "interval")` gives an error when the endpoints are reversed, instead of a negative probability; `vdist_normal_prob()` does the same for reversed `perc` values.
* `vdist_binom_perc()` states the quantile definition correctly (`P(X <= k - 1) < tp` but `P(X <= k) >= tp`) and the upper tail now shades `x > k`, so the shaded mass is the reported tail probability. `tp` must now be strictly between 0 and 1.
* `vdist_t_prob(type = "interval")` and `type = "both"` with a negative `perc` now use `|perc|` for the caption, reference lines and annotations as well as the probability.
* `vdist_binom_plot()` and `vdist_binom_prob()` compute the displayed standard deviation from `sqrt(n * p * (1 - p))` rather than from the rounded mean.
* `probs = 0` or `probs = 1` in the `*_perc()` functions for the normal, t, F and chi-square distributions now give an informative error (the quantile is infinite and cannot be plotted).
* Missing, empty, infinite or vector arguments (for example `sd = NA`, `df = numeric()`, `p = c(.2, .3)`) now give a clear package error instead of an internal one, and vector `s` is no longer silently recycled in `vdist_binom_prob()`.
* `vdist_f_prob()` and `vdist_chisquare_prob()` accept cutoffs below zero (probability 0 / 1) instead of failing.

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
