# vistributions 0.2.2

## Bug Fixes

* Continuous percentile probabilities must be strictly between 1e-12 and 1 - 1e-12, with the narrower t range `[0.001, 0.999]` for `0.05 <= df < 1`; normal sd / |mean| must be at least 1e-10 for nonzero means. Binomial percentile cutoffs follow `qbinom()` including its fuzz at jumps.
* Display probabilities below 1e-10% with a floor, improve chi-square views, normal tick labels and singular density caps.

* Binomial bars are split at the event boundary. A bar is painted as a whole, so a bin that crossed the cutoff used to shade counts outside the event (19.26% for a 4.01% exact event at `n = 10000, p = .01, s = 100`) or miss counts inside it. Every event cutoff is now a bar edge, and the shaded bar heights sum to `dbinom()` / `pbinom()` for the event (to relative 1e-12).
* Grouped binomial bins are computed from the side of the mode that keeps their precision. The right-tail bin `[2261, 2262]` of `vdist_binom_plot(4000, .5)` used to be drawn as `0` because two lower-tail CDF values had both rounded to 1; it now carries `3.294e-17`, the exact `dbinom(2261) + dbinom(2262)`. Adjacent right-tail bins are no longer quantised.
* Binomial captions put the inequality inside `P()` (`P(X >= 4) = 0.35`, `P(X = 26) = 0.003`, `P(26 <= X <= 26) = 0.003`) and round exactly once from the exact probability. The double rounding `0.0034978 -> 0.0035 -> 0.004` no longer prints a value outside the correct rounding interval. Probabilities below 1e-3 use scientific notation, with `< 1e-10%` below the display floor.
* `vdist_binom_perc()` follows `qbinom()` at cumulative jumps and evaluates caption comparisons on the smaller tail.
* `vdist_binom_prob()` (and the interval type) require every success count, including both interval endpoints, to lie in `[0, n]`. `s = -1` used to be accepted and labelled `P(X) <= -1 = 0`; it is now the same clear error as `s = n + 1`.
* A probability caption prints enough cutoff digits for its own printed percentage. `vdist_normal_prob(1.00051)` used to print `P(X < 1.001) = 84.1%` although `pnorm(1.001)` is 84.16%; it now prints `P(X < 1.0005) = 84.1%`, and the same check covers the t, chi-square and F probability exports.
* Small tail probabilities use conditioned CDF or survival differences, with integration for narrow intervals. Percentages below 1e-10%, including underflowed probabilities, display `< 1e-10%`; near 100% the bound is `> 99.9999999999%`.
* A tail event beyond the default view keeps its polygon and is disclosed against the event's own probability. `vdist_normal_perc(1e-10)` used to draw no polygon and no caption; the region bound now follows the event's tail probability (an event of 1e-10 is not truncated at a fixed 1e-10 tail), and an event that lies outside the panel reports the correct fraction outside (100% when it is wholly outside) instead of dividing by the truncated mass.
* The normal resolution floor is `sd / abs(mean) >= 1e-10` when the mean is nonzero. The comparison uses the ratio directly and all three normal exports reject smaller ratios. Translated normal ticks retain distinct labels.
* A central normal percentile whose representable endpoints do not enclose `probs` to within 1e-6 relative error stops with a documented error naming `probs` and `sd`; choose a larger `sd` or `probs`.
* F percentiles use the F-beta quantile relation when `qf()` fails to invert the smaller F tail within 1e-6 relative error, including F(1,1) lower tail probabilities near 1e-9.
* The blue strip that marks an event too small to see is placed in normalized view coordinates. A normal event at `1e-200` used to be marked around zero (`[-1.6e-202, 1.6e-202]`), a completely different event; the strip now sits at the event at both `1e-200` and `1e200` (the latter used to have an infinite bound).
* A chi-square or F curve whose whole view lies in a far tail is drawn, not reduced to a two-point chord. `vdist_chisquare_plot(200, xaxis_range = 25)` on `[0, 25]` used to draw a straight line between `(0, 0)` and `(25, 7.8e-54)`; the grid now follows the probability below 1e-12, so the curve equals `dchisq(x, 200)` across the view. The tail floor is only used when the whole view is that far out, so a singular support edge keeps its documented refinement.
* The density axis is capped only at a genuine support singularity. A bounded density with a narrow body (F with `1e6` degrees of freedom) or a heavy-tailed t used to have its body cut off, or its whole axis chosen from an almost empty tail (`7.7e-98` for `vdist_f_perc(.95, 1e6, 1e6)`); the body is now drawn at its true height, and an axis just below the double maximum no longer overflows to an infinite range.
* The F moments use an overflow-free grouping, so `vdist_f_plot(200, 1e306)` reports `Std Dev. = 0.1` and draws its normal overlay at `dnorm(1, 1, .1) = 3.989`, instead of `Std Dev. = 0` and an infinite overlay.
* The chi-square mean marker is drawn at the exact `df`, not at its three-decimal label: `vdist_chisquare_plot(3.14159)` marks 3.14159 instead of 3.142.
* An overflow-safe midpoint is used when classifying shaded segments and refining the grid, so `vdist_normal_prob(1e308, 1e308, 1e306)` reports 50% instead of an area error, and `vdist_normal_perc(.5, 1e308, 1e306)` draws its central interval.
* A finite endpoint decades beyond the representable tail is clipped to that tail before the grid is built. `vdist_normal_prob(c(-1e300, 1e300))`, `vdist_t_prob(1e100, 7)`, `vdist_chisquare_prob(1e100, 4)` and `vdist_f_prob(1e100, 5, 10)` used to fail with a spurious polygon-area error; they now return the exact 100% event.
* Formatting no longer parses locale-dependent text: `options(OutDec = ",")` used to make every percentile and probability export fail with `NAs introduced by coercion`; the decimal mark is fixed to `.` for every printed and re-parsed number.
* `print_plot` is validated by all 15 plotting exports, so `print_plot = NA` gives `print_plot must be a single nonmissing logical.` instead of an internal `if` error, and a number is no longer silently accepted.
* The test entry points guard their suggested packages: `tests/testthat.R` skips when `testthat` is absent (instead of failing in `library(testthat)`), and the visual tests skip when `vdiffr` is absent without `skip_on_cran()`.
* The numerical-resolution error for an unrepresentable quantile reports the supported probability range, `1e-12 < probs < 1 - 1e-12`.

* Near-duplicate polygon vertices are suppressed consistently across platforms. The probability-spaced grid is built with `qlogis()`, `plogis()` and the distribution's `q*()`, which need not round-trip exactly: the first or last point can land one ulp inside the cutoff, and `unique()` then keeps a duplicate vertex on one platform only (the Linux CI snapshot of `vdist_t_prob(1.445, 7, "interval")` carried `265.66,407.84` twice). A grid point that is the same number as its neighbour to within 64 ulps is now dropped after the area refinement, keeping the region's exact endpoint, so every remaining vertex is unchanged. The comparison is relative to the coordinates and never to the region's width: the t region with `df = 0.1`, about `1e96` wide, keeps its full grid and its area.
* A normal distribution with a near-zero mean is drawn when its scale ratio is in range, even if the ratio overflows to infinity. The direct comparison preserves the result under scaling of mean and sd together.
* An extreme normal scale is now rejected with a true message instead of a NaN area error: at `sd = 3e307` the ends of `mean +/- 4 sd` are finite but the window itself (8 sd) exceeds the double range, so the drawing would rescale by an overflowing difference and show an empty panel. `vdist_normal_perc()`, `vdist_normal_prob()` and `vdist_normal_plot()` stop with a message naming the window limit (`sd` below about `2.2e307`), and just below it (`sd = 2e307`) the same plot draws with its full 95% shading. The polygon area sum also stays finite for a region wider than the double range: its zero-height closing edge is skipped instead of being computed as `Inf * 0 = NaN`.
* A shaded region too narrow to occupy a pixel is no longer invisible on the device. The lower 5% and 50% tails of `vdist_chisquare_perc()` with `df = 0.1` (and the matching `vdist_chisquare_prob()` case) are marked with a visible strip in the panel and disclosed in the caption ("the shaded region is narrower than one pixel at this scale"); the event polygon, its area check and the printed percentage are unchanged and exact. Ordinary regions are not marked.
* Every plotting grid now has a bounded number of points whatever the scale. The normal, t, chi-square and F curves and shaded regions are sampled on fixed-count probability and standardised grids, and a binomial support wider than 2001 counts is drawn as approximately 2001 bins whose bars carry the exact bin probability, with up to two splits at event boundaries and a caption disclosing the grouping. A scale that previously grew the grid with the data (for example `mean = 1e6, sd = 2e5`) keeps every layer's data small, and `vdist_normal_prob(1.2e6, 1e6, 2e5, "lower")` builds and prints immediately.

* `vdist_t_*()`, `vdist_chisquare_*()` and `vdist_f_*()` no longer truncate non-integer degrees of freedom to whole numbers; probabilities, quantiles, densities and plots now use the `df` that was supplied. Chi-square `df` must be at least 0.1 and F `num_df` must be at least 1; smaller values cannot be drawn from their support boundary at double precision and now give a documented error instead of a spike or an infinite density.
* Continuous percentile cutoffs and probabilities are computed without rounding, and cutoff labels retain enough digits to support the printed probabilities.
* Quantiles and probability limits are checked before plotting. Continuous percentile probabilities must lie in `(1e-12, 1-1e-12)`. A `q*()` result that is non-finite or does not invert the requested tail is a package error, never a caption or an infinite polygon. A cutoff beyond the visible central mass is still drawn clipped to that view while the caption keeps its true value.
* Continuous curves and shaded polygons are no longer sampled with a step in data units. The initial grid is a fixed number of points spaced in probability and, for the location-scale normal, in standardised units; it is then refined where the trapezoid rule disagrees with the exact CDF mass of the interval. The refinement is driven by the region's own probability scale, so a support edge where the density is unbounded, a far tail and a narrow translated normal are all resolved without allocating a huge grid. A density whose central 99.9998% mass spans less than 0.001 is still rejected as too concentrated.
* Shaded continuous regions use `p*()`, `q*()` and `d*()` and are checked to relative 1e-6 above the display floor. Small regions may be omitted with a disclosure.
* The normal scale limit is a ratio, so changing measurement units preserves the supported domain. Zero-centred tiny scales remain valid subject to finite density and plotting-range checks.
* Captions print probabilities at the precision the area check guarantees: percentages at or above 0.1 keep one decimal, smaller ones use scientific notation. The printed cutoff is checked against that displayed probability, and interval/tail probabilities are kept unrounded until after the union arithmetic. For example `vdist_t_prob(.1, 7, "interval")` now prints 7.7% and `type = "both"` prints 92.3%.
* Normal interval annotations are placed at the midpoint of the visible cutoffs, so an interval far outside the curve no longer expands the panel and compresses the density. Annotation heights and the density axis are relative to the density's own scale: a normal with large units (for example `mean = 1e6, sd = 1e5`), a chi-square with a singular support edge and an F with a heavy tail all keep their shaded region visible on the rendered device. Densities that are unbounded at a support edge are clipped on the display axis only, using a distribution-dependent cap; source-coordinate area checks always use the unclipped density, and very wide t/F windows are drawn on a symmetric log axis so no part of the event collapses into a line. Part of a region that lies outside the plotted window is disclosed in a plot caption when it exceeds 1% of the region; the polygon still covers it, so the area check is unaffected.
* Central percentile captions are checked as a pair, so both printed endpoints and the displayed interval percentage describe the same event (for example `vdist_normal_perc(.0001, mean = 1, type = "both")` no longer prints `P(0.9999 < X < 1) = 1e-02%`, and `vdist_normal_perc(.0505, type = "both")` prints a probability its endpoints actually attain). The pair is printed with enough digits for the displayed percentage, and that percentage is computed from the exact probability rather than from the printed endpoints.
* Quantiles are refined by root finding when the `stats` quantile function does not invert its own distribution function (for example `qf(.95, 1e8, 1e8)`), so high-degree-of-freedom F percentile plots work instead of failing the quantile check. The F area check now uses a probability-space grid without a coarse uniform filler, which removes the error in heavy-tail probability plots.
* An F tail that does not converge at double precision is reported as a tail-overflow error and no longer described as a too-concentrated density with advice to reduce the degrees of freedom.
* The chi-square `df >= 0.1` floor is now enforced by all three exports at that documented boundary; the F concentration limit is stated in the help page (`?vdist_f_plot`).
* `vdist_binom_plot()`, `vdist_binom_prob()` and `vdist_binom_perc()` now require `n` to be a non-negative whole number and `s` to be whole numbers, instead of truncating them and labelling the probability of a different event. A whole number stored as a double (for example `(.7 - .4) * 10`) is accepted and rounded once, so `n`, the plotted support and the success counts all describe the same distribution.
* `vdist_binom_prob(type = "interval")` gives an error when the endpoints are reversed, instead of a negative probability; `vdist_normal_prob()` does the same for reversed `perc` values.
* `vdist_binom_perc()` states the quantile definition correctly (`P(X <= k - 1) < tp` but `P(X <= k) >= tp`; upper tail `P(X >= k + 1) <= tp` but `P(X >= k) > tp`, subject to `qbinom()` numerical fuzz) and the upper tail now shades `x > k`, so the shaded mass is the reported tail probability. `tp` must now be strictly between 0 and 1.
* `vdist_t_prob(type = "interval")` and `type = "both"` with a negative `perc` now use `|perc|` for the caption, reference lines and annotations as well as the probability.
* `vdist_binom_plot()` and `vdist_binom_prob()` compute the displayed standard deviation from `sqrt(n * p * (1 - p))` rather than from the rounded mean.
* `probs = 0` or `probs = 1` in the `*_perc()` functions for the normal, t, F and chi-square distributions now give an informative error (the quantile is infinite and cannot be plotted).
* Missing, empty, infinite or vector arguments (for example `sd = NA`, `df = numeric()`, `p = c(.2, .3)`, `perc = numeric()` in `vdist_normal_prob()`) now give a clear package error instead of an internal one, and vector `s` is no longer silently recycled in `vdist_binom_prob()`.
* `vdist_f_prob()` and `vdist_chisquare_prob()` accept cutoffs below zero (probability 0 / 1) instead of failing.
* `vdist_chisquare_plot(normal = TRUE)`, `vdist_chisquare_perc()` and `vdist_chisquare_prob()` draw the chi-square distribution only on its non-negative support; for `df < 2`, where the density is unbounded at zero, the grid starts at the 1e-6 quantile instead of a negative value or an infinite density. The F distribution is drawn the same way for `num_df < 2`.
* The normal curve overlaid on `vdist_chisquare_plot(normal = TRUE)` uses the supplied degrees of freedom and `sqrt(2 * df)` rather than their three-decimal display values.
* The normal curve overlaid on `vdist_f_plot(normal = TRUE)` uses the unrounded F mean and standard deviation, so a valid large-df overlay no longer collapses to a zero-variance point mass. When the F variance is undefined (`den_df <= 4`), the overlay is omitted with a message instead of drawing `Std Dev. = NA`.
* Percentages are printed at a precision whose rounding interval contains the exact probability: a value exactly on a one-decimal boundary (for example `5.05%`) is printed with the digits that make it unambiguous instead of being rounded to a false value.
* The shading polygon starts at the distribution's support (zero for chi-square and F) rather than at a quantile floor, so no probability is discarded for small `df`: `vdist_chisquare_perc(.05, .1)` and the corresponding F cases now shade the full event. Where the density is unbounded at zero it is capped on the display axis only.
* The picture is invariant under a change of measurement units for the normal family: multiplying `mean` and `sd` by the same factor (tested with 1e-3, 1e3, 1e6 and 1e-6) changes only the axis labels. The shaded proportion, the probability caption and the standardised polygon are the same.
* The input contract is now defined once (`vistributions:::vdist_contract()`) and tested end to end. Ordinary teaching inputs (the defaults, `normal(100, 15)`, t with 5 df, chi-square with 1 or 2 df in an upper tail, `F(5, 10)`, a typical binomial) are inside it, and every documented limit stops with its own message.
* Reference lines, markers and probability annotations are clamped to the plotted view. Extreme cutoffs therefore no longer extend the panel range (for example `vdist_t_prob(1e6, 7)` now shows its density instead of a vertical line) while the caption keeps the true event.
* `library(vistributions)` no longer moves the random stream. A startup tip drew `runif(1)` (and sometimes `sample()`) on every interactive attach without restoring the seed; `.onAttach()` now takes its draw from a saved copy and puts `.Random.seed` back exactly as it was (or removes it when the session had none).
* The vignette describes the binomial arguments the way the help does: `p` is the probability of success in each trial and `tp` is the tail probability used for the quantile, instead of the two swapped.
* A binomial probability that rounds to `1.000` but is below one is printed as the bound `> 0.999`, the mirror of the `1e-10%` display floor. `vdist_binom_prob(1000, .01, 1, "upper")` used to assert certainty for an event whose probability is 99.99568%; an exact certainty still prints `= 1.000`.
* Continuous percentages at or above 0.1% retain at least one decimal, including `84.0%` and `80.0%`, with a tolerance of 0.05 percentage points for one decimal. Printed cutoffs retain enough digits to reproduce the same percentage under base R; whole-percent text no longer loosens the tolerance by a factor of ten. Percentages from 0.01% to 0.1% are printed in fixed point with three significant digits (`0.05%`, `0.1%`), so a value one floating-point step below the 0.1% boundary no longer reads `1e-01%`; below 0.01% the package prints three significant digits in scientific notation. The same event therefore prints the same percentage in the percentile and probability forms, for example `vdist_t_perc(.001, 5)` and `vdist_t_prob(-5.893, 5)` both print `0.1%`.
* Continuous probability and percentile labels are anchored to the cutoff line with a fixed absolute gap: the left label ends 2 mm before the line and the right label starts 2 mm after it, so the pair never overprints and the layout does not depend on the device size. A label that would leave the panel (a cutoff close to an edge) is stacked on a second line on the side that has room instead of being drawn across the cutoff. Every continuous builder uses this placement, including `vdist_f_prob()`, the chi-square and normal probability forms, and the three-label `both`/`interval` forms. The rendered glyph boxes are regression-tested at 4x3, 7x5 and 10x6 inches.
* A shaded region caption no longer repeats itself when the region is both below 0.4% of the density peak in height and narrower than a pixel: the strip sentence ("the shaded region is narrower than one pixel at this scale; it is marked by the blue strip") replaces the generic "too small to display" clause. `vdist_chisquare_prob(1, 200)` prints one sentence.
* The 0.4%-of-peak-height "too small to display" disclosure is documented for the normal, t, F and chi-square families, not only for chi-square.
* Chi-square degrees of freedom are limited to `2e20`: above that value `sqrt(2 / df)` is below 1e-10 of `df`, so the default `df +/- 4 sd` window collapses to one coordinate. `vdist_chisquare_plot(1e34)` now stops with an error naming `df` instead of returning a blank zero-area plot, and `vdist_chisquare_perc(.95, 1e30)` no longer blames `probs`.
* `vdist_fmt_interval()`, `vdist_fmt_cutoff()` and `vdist_fmt_value()` switch to scientific notation for `|x| >= 1e15`, as `fmt_num()` already did, instead of printing dozens of meaningless fixed-point integer digits (for example `vdist_t_perc(.998, df = .05, "both")`).
* The chi-square mean marker is drawn on the axis (`y = 0`), not at the minimum of the sampled density: for `df < 2` the density is unbounded at zero, so `vdist_chisquare_plot(df = 1)` marked the mean at `y = 0.104`.
* The t joint domain is `df >= 0.05`; for `0.05 <= df < 1`, percentiles require `0.001 <= probs <= 0.999` and probability plots require `abs(perc) <= qt(0.001, df, lower.tail = FALSE)`. For `df >= 1`, percentiles keep the full `(1e-12, 1 - 1e-12)` range. These conservative limits leave enough representable tail for the relative area check; errors name the argument and valid range. A representable-tail diagnostic no longer rounds deficient coverage to a contradictory "only 100%".
* Only an explicitly supplied `xaxis_range` is capped at `1e10` for `vdist_chisquare_plot()`. The default body-centred window remains valid at `df = 1e10`; an oversized explicit range gives an error naming `xaxis_range`.
* A chi-square probability view widens to the mean plus four standard deviations when its initial panel contains less than 20% of the distribution. This retains the body at `df = 40:72, perc = 13`, `df = 200, perc = 61:120` and `df = 500, perc = 280:350`; ordinary df 1--30 windows at the 5th--95th percentiles are unchanged. Shading less than 0.4% of the density peak in height is disclosed as too small to display.

* Binomial help now documents the `> 0.999` probability bound. The vignette lists all normal percentile types and describes successes in n trials.
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
