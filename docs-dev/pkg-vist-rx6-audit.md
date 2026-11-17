# vistributions 0.2.2: Gemini re-audit of round 6 (pkg-vist-rx6)

Date: 2026-11-16 IST.
Worktree: `F:/R/Packages/_worktrees/vist-fx6` (branch `task/vist-fx6`).
Audited HEAD: `0472be7` (incorporating `7c3cd44`, `44174a3`, `04d161f`, `0472be7`).
Comparison base: `f3cf926` (round-5 head).
Earlier audit findings: `F:/R/Packages/_worktrees/vist-fix-022/docs-dev/vfxx5-reaudit.md` (VFXX5-H1, VFXX5-M1..M4, VFXX5-L1).
Fix report under audit: `docs-dev/fx6-report.md`.

---

## Executive Summary

Round 6 redesigns the drawing layer to decouple mathematical events from graphical formatting:
1. **One numeric source**: Quantiles and probabilities are derived exclusively from `stats::d*`, `stats::p*`, and `stats::q*`. Coordinates are never derived from formatted labels or re-parsed strings.
2. **Adaptive probability/standardised grid**: Replaces fixed grids and manual fillers with an adaptive interval-refining grid driven by interval CDF mass.
3. **Display density cap**: The density cap for singular boundaries is now sampled on a uniform grid in x, preventing small-df chi-square and heavy-tailed F distributions from compressing the visible body to zero pixels on screen.
4. **Removal of unselected complementary polygons**: Percentile and probability plots no longer draw light complementary polygons, eliminating the unvalidated over-tolerance light polygons from round 5.
5. **Accurate disclosure caption**: The off-view caption now calculates omitted probability relative to the actual event mass rather than unconditional distribution mass.

Independent reproduction verifies that full test suite passes (`1120 pass, 0 fail, 0 warn, 0 skip`), clean archive `R CMD check --as-cran --no-manual` finishes with 0 errors, 0 warnings, and 1 NOTE (maintainer email update), and all 15 distribution functions × tail options render visibly to a real PNG device.

However, two Medium defects and two Low defects remain:
- **VFXX5-M3 guard bug**: `vdist_check_normal_resolution` fails on `mean = 1e-308, sd = 10` because `ratio = Inf` is erroneously treated by `!is.finite(ratio)` as too small, stopping with the false message `sd / |mean| = Inf is below the documented limit of 1e-14`.
- **VFXX5-H1 sub-pixel lower tails**: Chi-square lower tails for df < 1 (e.g. `vdist_chisquare_perc(.5, .1)` and `.05`) have exact numerical areas and captions, but their cutoffs (e.g. `1.11e-6` on a `0..2.335` view) collapse to sub-pixel width (< 0.0003 px), yielding 0 rendered shaded pixels on device.
- **Floating overflow on extreme scale**: `vdist_normal_perc(.95, sd = 3e307)` throws a NaN area error despite satisfying `vdist_check_normal_range`.
- **Tracked test artifact**: `tests/testthat/Rplots.pdf` remains committed in git.

---

## Earlier Findings Re-Run Table (VFXX5 Audit Findings)

| ID | Case Re-Run | Output / Independent Evidence | Status |
|---|---|---|---|
| **VFXX5-H1** | `vdist_chisquare_perc(.95, .1)`<br>`vdist_chisquare_perc(.95, .1, "upper")`<br>`vdist_chisquare_perc(.95, .2)`<br>`vdist_chisquare_plot(.1)`<br>`vdist_chisquare_perc(.5, .1)`<br>`vdist_chisquare_perc(.05, .1)` | • `.95, .1` (lower): Panel fraction `0.1071`, 40,573 dark pixels on 900x500 PNG.<br>• `.95, .1` (upper): Panel fraction `0.1076`, 39,906 dark pixels on PNG.<br>• `.95, .2`: Panel fraction `0.1206`, 45,489 dark pixels on PNG.<br>• `plot(.1)`: 33,543 light pixels on PNG.<br>• However, lower `.5, .1` (cutoff `1.11e-6`) has panel fraction `4.77e-7` (0 shading px on PNG); lower `.05, .1` has fraction `4.77e-27` (0 shading px). | **PARTLY FIXED**<br>(Body and upper tails visibly resolved on device; extreme lower tails on linear axis remain sub-pixel). |
| **VFXX5-M1** | `cutoff <- 124.675`<br>`vdist_normal_prob(cutoff, 100, 15)` vs `cutoff * .001, .1, .015`<br>`vdist_normal_perc(.0001, mean=1, sd=.1, "both")`<br>`options(digits=17); vdist_t_prob(.1, 7, "interval")` | • Cutoff geometry: max standardized x cutoff diff is `4.44e-16`. Polygon area difference is `0`.<br>• Subtitles: both print `P(X < ...) = 95%`.<br>• Central `.0001` percentiles print `1e-02%` across scaling factors 1, 1000, 0.001.<br>• `options(digits=17)` prints `P(-0.1 < X < 0.1) = 7.7%` without leaking binary rounding noise. | **FIXED**<br>(One numeric source; geometry is strictly unit-invariant; formatter does not move coordinates). |
| **VFXX5-M2** | `vdist_t_perc(1e-10, .1)`<br>`vdist_t_perc(1e-10, .05)`<br>`vdist_t_perc(1e-6, .05)`<br>(and upper-tail counterparts) | Continuous percentile/probability plots no longer draw the unselected complementary polygon. For `vdist_t_perc(1e-10, .1)`, dark area is 0 (outside view), no light polygon is drawn, and no over-tolerance polygon (> 1.001) is produced. | **FIXED**<br>(Redesign eliminated unselected complementary polygon rendering). |
| **VFXX5-M3** | `vdist_normal_*(mean=1e-308, sd=10)`<br>`vdist_normal_perc(.95, sd=3e307)`<br>`vdist_t_perc(.95, .02)`<br>`vdist_t_perc(1-1e-10, .1)`<br>`vdist_f_perc(.95, 1.8e8, 1.8e8)` (lower & upper) | • `mean=1e-308, sd=10`: **STILL FAILS**: `Error: sd / |mean| = Inf is below the documented limit of 1e-14`.<br>• `sd=3e307`: **STILL FAILS**: NaN polygon area error.<br>• `vdist_t_perc(.95, .02)`: **FIXED** (returns finite panel, subtitle `P(X < 7.12862e+48) = 95%`).<br>• `vdist_t_perc(1-1e-10, .1)`: **FIXED** (subtitle `P(X < 1.60444e+96) = 100%`).<br>• `vdist_f_perc(.95, 1.8e8, 1.8e8)`: **FIXED** (returns cleanly for both lower and upper tails). | **PARTLY FIXED**<br>(t and F domain extensions work; normal ratio overflow guard bug and extreme scale NaN remain). |
| **VFXX5-M4** | `vdist_chisquare_perc(.05, 1, "upper")`<br>`vdist_f_perc(.05, 5, 10, "upper")`<br>`vdist_f_perc(1e-6, 1, 1)`<br>`vdist_normal_perc(.95)`<br>`vdist_f_perc(1e-6, 5, .5, "upper")` | • `vdist_view_omitted()` now calculates `(total - inside) / total`, dividing by event mass.<br>• Sub-1% omissions do not show noisy captions.<br>• For genuinely truncated view `vdist_f_perc(1e-6, 5, .5, "upper")`, caption accurately reports: `94.6% of the shaded region lies outside the plotted range`. | **FIXED**<br>(Denominator is exact; percentage text truthfully describes the shaded region). |
| **VFXX5-L1** | Guarantees in NEWS/help, tracked detritus `tests/testthat/Rplots.pdf` | `tests/testthat/Rplots.pdf` is still tracked in git. NEWS and contract table describe unified rules, but `mean=1e-308, sd=10` guard bug and sub-pixel chi-square lower tail remain unaddressed in docs. | **PARTLY FIXED** |

---

## Detailed Findings

### VFXX6-M1 (Retained from VFXX5-M3) — Guard bug in `vdist_check_normal_resolution` rejects valid near-zero mean

- **Severity:** Medium
- **Location:** `R/vdist-utils.R:398-406`
- **Reproduction:**
  ```r
  vdist_normal_plot(mean = 1e-308, sd = 10, print_plot = FALSE)
  vdist_normal_perc(0.95, mean = 1e-308, sd = 10, print_plot = FALSE)
  vdist_normal_prob(16.45, mean = 1e-308, sd = 10, print_plot = FALSE)
  ```
- **Observed Result:**
  ```text
  Error: The normal cutoff cannot be represented numerically: sd / |mean| = Inf is below the documented limit of 1e-14. Use a larger sd relative to the mean (scaling mean and sd together is always allowed).
  ```
- **Evidence & Root Cause:**
  In `R/vdist-utils.R`:
  ```r
  vdist_check_normal_resolution <- function(mean, sd) {
    if (abs(mean) > 0) {
      ratio <- sd / abs(mean)
      if (!is.finite(ratio) || ratio < 1e-14) {
        stop(...)
      }
    }
  }
  ```
  When `abs(mean) = 1e-308` and `sd = 10`, `ratio` overflows to `+Inf`. A ratio of `+Inf` means the distribution has infinite relative resolution (it is effectively zero-centered). However, `!is.finite(ratio)` evaluates to `TRUE`, triggering the error handler and falsely asserting that `Inf is below the documented limit of 1e-14`.
- **Fix:** Check `!is.nan(ratio) && ratio < 1e-14`, or allow `is.infinite(ratio) && ratio > 0`:
  ```r
  if (!is.nan(ratio) && ratio < 1e-14)
  ```

---

### VFXX6-M2 (Retained from VFXX5-H1) — Chi-square lower tail for df < 1 collapses to 0 device pixels

- **Severity:** Medium (downgraded from High because upper tails and `prob=0.95` body are now visibly rendered)
- **Location:** `R/vdist-data-prep.R:105-135`, `R/vdist-utils.R:800-830`
- **Reproduction:**
  ```r
  p1 <- vdist_chisquare_perc(0.05, 0.1, print_plot = FALSE)
  p2 <- vdist_chisquare_perc(0.50, 0.1, print_plot = FALSE)
  ```
- **Observed Result:**
  - In `p1`, `cutoff = 1.114775e-26`. On the view `[0, 2.335]`, the built panel fraction is `4.77e-27`. Emitted dark shaded pixels on a 900x500 PNG is 0 (the only pixels detected are the vertical dashed reference line and red cross marker).
  - In `p2`, `cutoff = 1.114775e-06`. On the view `[0, 2.335]`, the built panel fraction is `4.77e-07`, and width on device is < 0.0003 pixels (0 shaded pixels on PNG).
- **Evidence & Root Cause:**
  While `fx6-report.md` resolved the upper tail and `.95` body by fixing the density axis cap (`0.45` instead of `1e18`), lower tail percentiles on extreme low-df distributions remain on a linear scale starting at 0, which makes the shaded region physically invisible. The fix report noted this in "Deferred #1", but the plot caption claims to show the lower 50% or 5% event without any visible shading.
- **Fix:** For events whose width relative to the view is below graphical resolution (e.g. `< 1e-3 * view span`), adjust the viewport or provide an inset view / log-transformed view.

---

### VFXX6-L1 (Retained from VFXX5-L1) — Tracked test artifact `tests/testthat/Rplots.pdf`

- **Severity:** Low
- **Location:** `tests/testthat/Rplots.pdf`
- **Evidence:** `git ls-files tests/testthat/Rplots.pdf` shows the 3,667-byte PDF file is tracked in git (originally committed in `5a35260`). It is generated test detritus.
- **Fix:** `git rm tests/testthat/Rplots.pdf`.

---

### VFXX6-L2 (Retained from VFXX5-M3) — Extreme scale `sd = 3e307` causes NaN polygon area error

- **Severity:** Low
- **Location:** `R/vdist-utils.R:353-380`
- **Reproduction:**
  ```r
  vdist_normal_perc(0.95, sd = 3e307, print_plot = FALSE)
  vdist_normal_prob(qnorm(0.95, sd = 3e307), sd = 3e307, print_plot = FALSE)
  ```
- **Observed Result:**
  ```text
  Error: The normal shaded region area (NaN) does not match the probability of its event (0.95) within 1e-3; the density is too concentrated for the documented plotting contract.
  ```
- **Evidence:** `mean +/- 4*sd` satisfies `vdist_check_normal_range` (`mean +/- 4*sd = +/- 1.2e308` is finite), but during polygon building, intermediate calculations overflow to `NaN`.

---

## Grid Sampling: Distribution Functions × Tail Options (Device PNG Inspection)

All 15 exported functions were sampled across tail options, rendered to PNG files via the `png` device at 720×480, and evaluated for polygon source area, ggplot built panel fraction, and real device pixel counts:

| Case | Truth | Area / Mass | Abs Error | Rendered Fraction | Dark Px | Light Px | Device Status |
|---|---:|---:|---:|---:|---:|---:|---|
| `normal_plot_default` | 0.997300 | 0.997295 | 5.02e-06 | 0.2404 | 44,942 | 21,004 | Visibly shaded |
| `normal_plot_sd_0.1` | 0.997300 | 0.997295 | 5.31e-06 | 0.2404 | 45,476 | 21,231 | Visibly shaded |
| `normal_perc_lower` | 0.950000 | 0.950008 | 8.40e-06 | 0.2290 | 64,053 | 454 | Visibly shaded |
| `normal_perc_upper` | 0.050000 | 0.050010 | 9.84e-06 | 0.0120 | 3,364 | 448 | Visibly shaded |
| `normal_perc_both` | 0.500000 | 0.499990 | 9.75e-06 | 0.1205 | 33,595 | 582 | Visibly shaded |
| `normal_prob_lower` | 0.950000 | 0.950008 | 8.39e-06 | 0.2290 | 64,053 | 454 | Visibly shaded |
| `normal_prob_upper` | 0.050000 | 0.050010 | 9.84e-06 | 0.0120 | 3,364 | 448 | Visibly shaded |
| `normal_prob_both` | 0.950000 | 0.949993 | 6.86e-06 | 0.2290 | 64,210 | 538 | Visibly shaded |
| `t_plot_default` | 0.989677 | 0.989677 | 5.12e-06 | 0.2543 | 385 | 70,137 | Visibly shaded |
| `t_perc_lower` | 0.950000 | 0.950008 | 8.14e-06 | 0.1921 | 54,229 | 324 | Visibly shaded |
| `t_perc_upper` | 0.050000 | 0.050010 | 9.64e-06 | 0.0097 | 3,583 | 626 | Visibly shaded |
| `t_perc_both` | 0.500000 | 0.499991 | 8.99e-06 | 0.1013 | 29,179 | 444 | Visibly shaded |
| `t_prob_lower` | 0.950000 | 0.950008 | 8.11e-06 | 0.1921 | 54,229 | 324 | Visibly shaded |
| `t_prob_upper` | 0.050000 | 0.050010 | 9.66e-06 | 0.0097 | 3,583 | 626 | Visibly shaded |
| `t_prob_interval` | 0.900000 | 0.900001 | 9.63e-07 | 0.1824 | 51,800 | 400 | Visibly shaded |
| `t_prob_both` | 0.050000 | 0.050018 | 1.81e-05 | 0.2018 | 3,008 | 52,547 | Visibly shaded |
| `chisq_plot_default` | 0.999985 | 0.999987 | 2.30e-06 | 0.1272 | 43 | 37,797 | Visibly shaded |
| `chisq_perc_lower` | 0.950000 | 0.950000 | 1.73e-08 | 0.1981 | 56,180 | 448 | Visibly shaded |
| `chisq_perc_upper` | 0.050000 | 0.050009 | 9.03e-06 | 0.0075 | 2,920 | 461 | Visibly shaded |
| `chisq_prob_lower` | 0.950000 | 0.950000 | 1.95e-08 | 0.1208 | 34,635 | 483 | Visibly shaded |
| `chisq_prob_upper` | 0.050000 | 0.050009 | 9.03e-06 | 0.0064 | 2,667 | 477 | Visibly shaded |
| `f_plot_default` | 0.970325 | 0.970325 | 4.88e-06 | 0.2668 | 604 | 76,005 | Visibly shaded |
| `f_perc_lower` | 0.950000 | 0.949990 | 9.94e-06 | 0.2657 | 74,870 | 365 | Visibly shaded |
| `f_perc_upper` | 0.050000 | 0.050009 | 9.07e-06 | 0.0031 | 1,866 | 368 | Visibly shaded |
| `f_prob_lower` | 0.950000 | 0.950005 | 4.72e-06 | 0.2657 | 74,869 | 350 | Visibly shaded |
| `f_prob_upper` | 0.050000 | 0.050010 | 9.93e-06 | 0.0031 | 1,866 | 374 | Visibly shaded |
| `binom_plot_default` | 1.000000 | 1.000000 | 0.00e+00 | NA | 70,402 | 506 | Visibly shaded |
| `binom_perc_lower` | 0.149308 | 0.149308 | 0.00e+00 | NA | 10,613 | 60,378 | Visibly shaded |
| `binom_perc_upper` | 0.150268 | 0.150268 | 0.00e+00 | NA | 3,425 | 67,574 | Visibly shaded |
| `binom_prob_lower` | 0.849732 | 0.849732 | 0.00e+00 | NA | 58,134 | 10,926 | Visibly shaded |
| `binom_prob_upper` | 0.350389 | 0.350389 | 0.00e+00 | NA | 24,114 | 44,941 | Visibly shaded |
| `binom_prob_exact` | 0.200121 | 0.200121 | 0.00e+00 | NA | 13,758 | 55,283 | Visibly shaded |
| `binom_prob_interval`| 0.803343 | 0.803343 | 0.00e+00 | NA | 54,974 | 14,110 | Visibly shaded |

All 33 sampled cases have absolute numerical area/mass error below `1.81e-05` (well within the package's 1e-3 contract), and every plot has substantial pixel coverage on the real device.

In addition, an independent subtitle grid test checked 190 subtitle cases across all continuous distributions against independent `stats::p*()` calculations: **190 passed, 0 failed**. Every subtitle percentage agrees with the event defined by the printed cutoffs to within the half-unit of the last displayed digit.

---

## Verification of Round-6 Tests on Old Code

To verify that the new tests in `tests/testthat/test-vfx6-redesign.R` assert meaningful values and actually catch previous defects, the test file was executed against the round-5 head (`f3cf926`):

- **Result on old code (`f3cf926`):**
  `[ FAIL 6 | WARN 0 | SKIP 0 | PASS 364 ]`
- **Failures caught on old code:**
  1. `rendered_dark_area(p) > 1e-04` failed on chi-square df 0.1 upper tail (`actual: 3e-19 <= 1e-4`).
  2. `subtitle(vdist_normal_perc(0.0505, type = "both"))` failed (`actual: "5.1%" != expected: "5.05%"`).
  3. `p$labels$caption` failed (old caption had unconditional off-view text "is added by the area check").
  4. `rendered_dark_area(p)` failed on low-df chi-square upper .05 and .95 cases.
  5. `vdist_contract` not found on old code.

- **Result on current HEAD (`0472be7`):**
  `[ FAIL 0 | WARN 0 | SKIP 0 | PASS 376 ]` (within `test-vfx6-redesign.R`), and `1120 PASS` across the entire suite.

---

## Clean Archive Build and Check Verification

A clean archive was created with `git archive HEAD`, built, and checked:
- Command: `R CMD build --no-manual check-src`
  - Output: `building 'vistributions_0.2.2.tar.gz'`, exit code 0.
- Command: `R CMD check --as-cran --no-manual vistributions_0.2.2.tar.gz`
  - Examples: `OK`
  - Testthat tests: `[84s] OK`
  - Vignettes: `[14s] OK`
  - Status: `1 NOTE` (Maintainer email change from gmail to rsquaredacademy.com, pre-existing from round 1).
  - 0 Errors, 0 Warnings, 1 Note.

---

## Count by Severity

| Severity | Count | Remaining Findings |
|---|---:|---|
| **High** | **0** | (VFXX5-H1 downgraded to Medium as body and upper tails now render visibly with 40k+ px) |
| **Medium** | **2** | VFXX6-M1 (ratio overflow guard bug rejects `mean=1e-308, sd=10`), VFXX6-M2 (sub-pixel low-df chi-square lower tail) |
| **Low** | **2** | VFXX6-L1 (tracked `Rplots.pdf` artifact), VFXX6-L2 (extreme scale `sd=3e307` NaN area error) |
| **Total** | **4** | |

Earlier findings re-run totals (6 findings from VFXX5):
- **3 FIXED** (VFXX5-M1, VFXX5-M2, VFXX5-M4)
- **3 PARTLY FIXED** (VFXX5-H1, VFXX5-M3, VFXX5-L1)
- **0 STILL WHOLLY WRONG**

---

## Verdict

Under the binding submission rule (*no CRAN submission with any known wrong result*), the presence of VFXX6-M1 (which rejects mathematically valid distributions with the mathematically false error message `sd / |mean| = Inf is below the documented limit of 1e-14`) and VFXX6-M2 (which renders 0 shaded pixels on the device for supported chi-square lower percentiles):

**any wrong result left? yes**
