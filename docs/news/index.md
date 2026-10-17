# Changelog

## vistributions 0.2.1

CRAN release: 2026-10-16

### Bug Fixes

- [`vdist_normal_plot()`](https://vistributions.rsquaredacademy.com/reference/vdist_normal_plot.md)
  now properly scales the x-axis grid to span `mean ± 4 * sd` (BUG-01).
- [`vdist_f_perc()`](https://vistributions.rsquaredacademy.com/reference/vdist_f_plot.md)
  adapts upper x-axis limit dynamically beyond 4.0 when quantiles exceed
  the default plotting range (BUG-02).
- [`vdist_t_prob()`](https://vistributions.rsquaredacademy.com/reference/vdist_t_plot.md)
  handles negative percentile values (`perc <= -5`) without “wrong sign
  in ‘by’ argument” sequence error (BUG-03).
- [`vdist_chisquare_plot()`](https://vistributions.rsquaredacademy.com/reference/vdist_chisquare_plot.md)
  ensures shaded fill polygon coordinates match the full extent of
  `xaxis_range` (BUG-04).
- Guarded theoretical standard deviation calculation for the F
  distribution against division-by-zero when denominator degrees of
  freedom is less than or equal to 4 (`den_df <= 4`) (BUG-05).
- `vdist_t_prob(type = "both")` corrects color mapping and annotation
  placement so left/right shaded tails and central body match their
  respective regions (BUG-06).
- `check_positive()` now strictly enforces positive values (`val <= 0`),
  rejecting zero and negative inputs (BUG-07).
- `check_suggests()` formats informative error messages identifying the
  missing package prior to aborting execution (BUG-08).
- [`vdist_binom_prob()`](https://vistributions.rsquaredacademy.com/reference/vdist_binom_plot.md)
  validates numeric types for `n`, `p`, and `s` before performing range
  checks (BUG-09).
- `check_range()` replaces bitwise `|` with logical `||` to eliminate
  scalar evaluation warnings in modern R (BUG-10).
- Visual test snapshot names for
  [`vdist_t_prob()`](https://vistributions.rsquaredacademy.com/reference/vdist_t_plot.md)
  interval and both types are properly aligned (BUG-11).
- Removed unprompted network polling
  ([`available.packages()`](https://rdrr.io/r/utils/available.packages.html))
  and interactive prompts during package startup in `.onAttach()`;
  retained rotating educational tips (BUG-12).
- [`vdist_normal_perc()`](https://vistributions.rsquaredacademy.com/reference/vdist_normal_plot.md)
  scales x-axis bounds dynamically with standard deviation (BUG-13).
- `vdist_binom_prob(type = "upper")` strictly shades bars where `x >= s`
  (BUG-14).
- `vdist_binom_prob(type = "exact")` matches success counts via integer
  equality rather than floating-point density comparison (BUG-15).
- `vdist_t_prob(type = "upper")` aligns text annotation colors with
  polygon fill colors (BUG-16).
- Added missing positive degrees of freedom validations across t, F, and
  chi-square functions (BUG-17).
- F distribution normal approximation overlay is strictly clamped to
  non-negative support (`x >= 0`) (BUG-18).
- [`vdist_binom_perc()`](https://vistributions.rsquaredacademy.com/reference/vdist_binom_plot.md)
  allows percentile input `tp` across the full valid range (0, 1) rather
  than capping at 0.5 (BUG-19).

### Documentation & Vignettes

- Fixed pedagogical and numerical discrepancies in
  `vignettes/introduction-to-vistributions.Rmd`:
  - Corrected Normal 15th percentile problem cutoff (`0.15` instead of
    `0.85`).
  - Aligned Binomial sample size ($`n = 12`$ consistently).
  - Synchronized Chi-square question text with lower-tail probability
    evaluation.
  - Aligned Student’s t 95% interval problem (`0.95` instead of `0.8`).
  - Fixed inversion of Student’s t `interval` vs `both` probability
    examples.
  - Standardized knitr chunk options (`fig.align = 'center'`).
  - Corrected typographic errors.
- Added explicit `@return` documentation across all exported plotting
  and calculation functions.
- Added `.github/CONTRIBUTING.md`, issue templates, PR template, and
  upgraded `CODE_OF_CONDUCT.md` to Contributor Covenant v2.1.

### Package Hygiene & Infrastructure

- Removed stale SVG files in `tests/figs/` and updated test suite to
  testthat 3rd edition (`Config/testthat/edition: 3`).
- Bumped dependency requirement to `R (>= 4.1.0)`.
- Removed unused `magrittr` dependency from `Imports:`.
- Configured automated `pkgdown` website deployment via Netlify.

## vistributions 0.2.0

CRAN release: 2024-11-07

This is a minor release for bug fixes.

## vistributions 0.1.2

CRAN release: 2021-05-20

This is a patch release to fix CRAN note about lazy data.

## vistributions 0.1.1

CRAN release: 2019-03-07

This is a patch release to fix bugs in the app.

## vistributions 0.1.0

CRAN release: 2018-11-30

- Added a `NEWS.md` file to track changes to the package.
