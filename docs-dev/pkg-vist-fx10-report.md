# vistributions 0.2.2 — round-10 fix report (vist-fx10)

Source: `F:/R/Packages/_worktrees/vist-fx10`, branch `task/vist-fx10`, based on
`3ce6ec1`. Every finding of `AUDIT-vistributions-022.md` was addressed. No
version bump: `DESCRIPTION` still says 0.2.2. This report is committed under
`docs-dev/` and is not part of the built package.

Two actions were allowed: **(a)** fix the output, **(b)** reject the input when
it is a numerical-resolution extreme a plotting package cannot show, with a
specific error that names the valid range and with the limit documented in the
help page. Everything that hits ordinary input was fixed (a). The two rejected
cases (H08, H09) are single-double resolution extremes; their limits are in
`?vdist_normal_plot` and in the contract table.

## Finding by finding

Legend: **FIXED (a)** = the number, curve, shading or caption is now correct;
**REJECTED-INPUT (b)** = a specific, documented resolution error.
Tests are in `tests/testthat/test-vfx10-findings.R` (Hxx/Mxx/Lxx sections) and
the existing round-1..9 files were updated where they asserted the old wrong
text. Every new test asserts a value against base R `p*/q*/d*` (or an exact
`integrate()`), and each fails at `3ce6ec1`.

| # | Action | Test | Before → after |
|---|---|---|---|
| H01 | FIXED (a) | `H01: a binomial bin never straddles the event boundary` | `vdist_binom_prob(10000,.01,100,"exact")` shaded **0.1926344** → **0.04006181** = `dbinom(100,10000,.01)`; `"lower"` 0.4865007 → **0.5265625** = `pbinom(100,…)`; `n=10000,p=.5,s=5000` lower 0.4800614 → **0.5039893** = `pbinom(5000)`; upper 0.4800614 → **0.5039893** = `P(X>=5000)`; `vdist_binom_perc(10000,.01,.5,"upper")` 0.3208649 → **0.4734375** = `P(X>100)`. Event cutoffs are now integer bar edges, so no bar is split by the event. |
| H02 | FIXED (a) | `H02: a right-tail grouped bin keeps its representable mass` | bin `[2261,2262]` of `vdist_binom_plot(4000,.5)` height **0 → 3.294e-17** = `dbinom(2261)+dbinom(2262)`; every right-tail bin of the grouped grid now uses a survival difference; the summed mass is still 1. |
| H03 | FIXED (a) | `H03: a binomial caption is rounded once, from the exact value` | `"P(X) = 26 = 0.004"` → **`"P(X = 26) = 0.003"`** = `round(dbinom(26,74,.5),3)` (raw 0.003497864); interval prints `"P(26 <= X <= 26) = 0.003"`. |
| H04 | FIXED (a) | `H04: a near-one binomial percentile is resolved in the small tail` | `tp = 1-2^-53` lower: cutoff **624 → 629**, caption `"P(X <= 623) < 1, but P(X <= 624) >= 1"` → `"P(X <= 628) < 0.9999999999999999, but P(X <= 629) >= 0.9999999999999999"`; shaded mass = `pbinom(629,1000,.5)`; `pbinom(628) < tp` is TRUE and `pbinom(629) >= tp` is TRUE in base R. Above 1/2 the cutoff is found with `qbinom(1-tp, lower.tail=FALSE)`. |
| H05 | FIXED (a) | `H05: the printed cutoff reproduces the printed percentage` | `vdist_normal_prob(1.00051)`: `"P(X < 1.001) = 84.1%"` → **`"P(X < 1.0005) = 84.1%"`** (`100*pnorm(1.0005)=84.14657`); upper → `"P(X > 1.0005) = 15.9%"`; t → `"P(X < 1.0051) = 82.2%"`; chi-square → `"P(X < 1.0025) = 28.4%"`; F → `"P(X < 1.0013) = 53.6%"`. Each printed cutoff now reproduces its printed percentage at that percentage's own precision; `vdist_fmt_value()` compares with the **displayed** probability, not the unrounded original. |
| H06 | FIXED (a) | `H06: small tail probabilities are computed on the right scale` | `P(9 < X < 10)` `0%` → **`1.13e-17%`** (survival difference); `[8,8.1]` `4.44e-14%` → **`3.47e-14%`**; `vdist_normal_prob(8.1)` other annotation `2.22e-14%` → **`2.75e-14%`**; complements now `1.13e-17%` (normal 9), `1.19e-17%` (t 10, df 200), `1.52e-21%` (chi-square 100, df 1), `1.09e-20%` (F 30, 5, 200) instead of `0%`/`100%`; `vdist_t_prob(1e-17,10,"interval")` `0%` → **`7.78e-16%`** (density integrated when both CDF values are the same double). Small probabilities print in the package's scientific style. |
| H07 | FIXED (a) | `H07: a tail event keeps its polygon and its true disclosure` | `vdist_normal_perc(1e-10)`: **0 polygons and no caption** → 1 polygon, `dark area = 1e-10`, caption absent because 99.94% is inside the (widened) panel; the dashed line is the true `qnorm(1e-10)=-6.361341`. `vdist_normal_perc(2e-9)`: caption `"47.4% … outside"` → no caption (99.94% visible; the event is no longer truncated at a fixed 1e-10 tail). A genuinely off-panel event is disclosed against the event probability: `vdist_normal_prob(12,"upper")` → **`"100% of the shaded region lies outside the plotted range"`**. The region bound now uses `min(1e-10, p_event*1e-6)`, and the coverage check is relative, not an absolute 1e-4. `vdist_t_perc(1e-10,1,"upper")` now has its polygon. |
| H08 | REJECTED-INPUT (b) | `H08: an unrepresentable central interval is a resolution error` | `vdist_normal_perc(.0001,1,1e-13,"both")` returned `"P(1.0000000000000000 < X < 1.0000000000000000) = 0%"`; now stops: *"The requested central probability of 1e-04 cannot be represented numerically at this location and scale: the two quantiles are the same double (1). Use a larger sd or a probability inside the documented range 1e-10 to 1 - 1e-10."* Both endpoints are the same double, so no printed pair can state the requested probability. Documented in `?vdist_normal_plot` (`@param sd`) and enforced for the central type of the normal percentile export. |
| H09 | REJECTED-INPUT (b) | `H09: a normal whose panel range collapses is rejected` | `mean=1, sd=1e-14`, `sd/\|mean\|=1e-14`: the panel treated the window as a zero-width range and every vertex landed at screen position 0.5. The documented resolution floor is now **3e-14** (above `1000*eps ≈ 2.2e-13` window-relative collapse): the input stops with *"sd / \|mean\| = 1e-14 is below the documented limit of 3e-14…"*. `sd = 1e-13` still plots and is not collapsed (test asserts distinct panel positions). |
| H10 | FIXED (a) | `H10: the visibility strip marks its own event at any scale` | `s=1e-200`, event `[.99,1.01]s`: strip `[-1.6e-202, +1.6e-202]` (around zero) → **`[9.84e-201, 1.016e-200]`**; `s=1e200` previously `[Inf, 4e200]` → **`[9.84e199, 1.016e200]`**. The centre is accumulated in units of the view width, so neither `(lo+hi)/2 * width` underflows nor the product overflows. |
| H11 | FIXED (a) | `H11: a whole view in a far tail is still a curve` | `vdist_chisquare_plot(200)` on `[0,25]` had **2 points** `(0,0),(25,7.84e-54)` and `approx(…,12.5)=3.92e-54`; now **2002 points**, `approx(…,12.5)=6.40837e-81 = dchisq(12.5,200)`, endpoint `dchisq(25,200)` exact. The 1e-12 probability floor is used only when the whole view lies below it. |
| H12 | FIXED (a) | `H12: the density axis is capped only at a real singularity` | `vdist_f_perc(.95,1e6,1e6)`: panel top **7.671e-98** → **258.39** = `1.3 × 198.76` (built peak; exact `df(1)=199.47`), so the body is not cut; `vdist_t_perc(.001,1,"upper")`: cap **0.001495** → **0.4138** = `1.3 × dt(0,1)`. The axis is capped only when the density is non-finite at a support edge; otherwise the true maximum is used. |
| H13 | FIXED (a) | `H13: the F moments do not overflow an intermediate` | `vdist_f_plot(200,1e306)`: x label `"Mean = 1  Std Dev. = 0"` → **`"Mean = 1  Std Dev. = 0.1"`**; overlay range `0..Inf` → **`1.47e-195 .. 3.9894228`** = `dnorm(1,1,.1)`. `sqrt((2*m^2/num_df)*((num_df+den_df-2)/(den_df-4)))` is finite where the old grouping overflowed; all four F exports use one `vdist_f_moments()`. |
| H14 | FIXED (a) | `H14: the chi-square mean marker is the exact df` | marker x **3.142 → 3.14159**; the label remains `df = 3.14159`; density and overlay already used the unrounded value. |
| H15 | FIXED (a) | `H15: a finite peak at the smallest scales is drawn, not flat` | `vdist_normal_plot(0,2.3e-309)`: `y.range 0..Inf`, band `0..0` → **`0..1.798e308`** (finite, `dnorm(0)=1.7345e308 < ymax`), band spans **0.9649** of the panel. `vdist_normal_prob(0,sd=3e-309)` crashed with `area (Inf)`; now `"P(X < 0) = 50%"`. `vdist_area()` halves each height before adding, and the display headroom is clamped at the double maximum. |
| M01 | FIXED (a) | `M01: a segment midpoint does not overflow` | `vdist_normal_prob(1e308,1e308,1e306)` `area (0) … concentrated` → **`"P(X < 1e+308) = 50%"`**, area 0.5; `vdist_normal_perc(.5,1e308,1e306,"both")` → central 50%. `vdist_mid(a,b)=a/2+b/2` replaces `(a+b)/2`. |
| M02 | FIXED (a) | `M02: finite distant cutoffs keep their exact mass` | `vdist_normal_prob(c(-1e300,1e300))`, `vdist_t_prob(1e100,7)`, `vdist_chisquare_prob(1e100,4)`, `vdist_f_prob(1e100,5,10)` all failed with polygon areas like `1.39e227`; now each returns its exact 100% event, area within 1e-3, caption `100%`. Finite endpoints beyond the representable tail are clipped to that tail before the grid is built. |
| M03 | FIXED (a) | `M03: a decimal comma never reaches the numeric parser` | with `options(OutDec=",")`: `NAs introduced by coercion` → `missing value where TRUE/FALSE needed`; now all exports format with `decimal.mark="."`, and `vdist_normal_perc(.000123)` prints `"P(X < -3.666) = 1.23e-02%"`. Verified for normal, t, chi-square and F percentile/probability and for a t interval. |
| M04 | FIXED (a) | `M04: ticks on a translated normal are distinct` | `vdist_normal_plot(1,1e-13)` labels `"1" "1" "1" "1"` → **`"0.9999999999996000" "0.9999999999997999" "1.0000000000000000" "1.0000000000002001"`**; ordinary normals keep the default labels (snapshots unchanged). |
| M05 | FIXED (a) | `M05: print_plot is validated by every plotting export` | `print_plot=NA` internal `missing value where TRUE/FALSE needed`, `print_plot=0` silently accepted; all 15 exports now stop with **`"print_plot must be a single nonmissing logical."`** for `NA`, a number or a string. |
| M06 | FIXED (a) | `M06: a success count must be inside [0, n]` | `s=-1` was accepted and printed `"P(X) <= -1 = 0"`; `s=n+1` gave a different message. Both now stop with **`"s must be a whole number of successes between 0 and n (n = …)."`**, including every interval endpoint; `s=0` and `s=n` are ordinary counts. |
| M07 | FIXED (a) | `M07: the test entry points do not require the suggested packages` | `tests/testthat.R` now runs `test_check()` only inside `requireNamespace("testthat")`; `test-visual.R` starts with `skip_if_not_installed("vdiffr")` (a non-CRAN check without vdiffr now skips instead of erroring). |
| M08 | FIXED (a) | `M08: the resolution error states the real probability range` | message said *"range is 1e-10 to 1"* → now **`"… the documented range is 1e-10 to 0.9999999999"`**, the same digits as the probability validator. |
| L01 | FIXED (a) | `L01: the binomial inequality is inside P()` | `"P(X) >= 4 = 0.35"` → **`"P(X >= 4) = 0.350"`** = `pbinom(3,10,.3,lower.tail=FALSE)`; `lower` prints `P(X <= 4)`, `exact` prints `P(X = 4)`, interval already printed `P(26 <= X <= 26)`. |

### New input limits (documented)

* Normal: `sd / abs(mean) >= 3e-14` for a non-zero mean (was `1e-14`). The
  reason is a display guarantee: below it ggplot's own zero-range test treats
  the `mean ± 4 sd` window as width zero and every vertex maps to one screen
  position. Documented in `?vdist_normal_plot` (`@param sd`), in
  `vdist_contract()`, and named in the error.
* A central probability whose two quantiles are the same double is a
  resolution error (H08), not a successful 0% plot. Documented in
  `?vdist_normal_plot` and enforced for `vdist_normal_perc(type="both")`.
* `vdist_binom_prob()` success counts must lie in `[0, n]` (M06), documented in
  `@param s`.
* No other new rejection: the probability ranges stay `[1e-10, 1-1e-10]`
  (normal, t) and `[1e-6, 1-1e-6]` (chi-square, F); `sd` floors/ceilings for
  the density and window are unchanged.

## Fresh coverage pass (all 16 exports, audit case list)

`OK` = the audit's check passes with the stated value; `WRONG`/`CRASH` as in
the audit. "After" is this worktree run with base R comparisons.

| Export | Before (audit) | After |
|---|---|---|
| `vdist_binom_plot` | defaults and `n=1,1000 × p=.5,.01` OK; grouped `4000,.5` right-bin mass WRONG (H02) | **OK**: per-count `dbinom`, grouped bin = exact survival mass (H02 test), moments OK |
| `vdist_binom_prob` | ordinary 2×2 matrix OK; `10000` L/U/E/I WRONG (H01); `74,.5` E/I WRONG (H03); negative `s` M06 | **OK**: cutoffs are bar edges; shaded mass = `dbinom`/`pbinom` for L/U/E/I; caption `P(X = 26) = 0.003`; `s ∉ [0,n]` rejected |
| `vdist_binom_perc` | ordinary matrix OK; grouped median WRONG (H01); `tp=1-2^-53` WRONG (H04) | **OK**: cutoff 629, shading = `pbinom(629)`, threshold printed with 16 digits, `p=0,1` tails still OK |
| `vdist_normal_plot` | defaults/`(100,15)` OK; `sd=1e-300/1e300/2e307` OK; `(1,1e-14)` WRONG (H09), `sd=2.3e-309` WRONG (H15); tick labels M04 | **OK** for defaults, teaching scale, zero-centred tiny/huge; `(1,1e-14)` rejected with the 3e-14 message; `sd=2.3e-309` finite non-flat band; `(1,1e-13)` distinct labels |
| `vdist_normal_perc` | ordinary L/U/B OK; L/U `1e-10` WRONG (H07); central `.0001` at 1e-13 WRONG (H08); median at floor WRONG (H09); huge mean B / comma CRASH (M01/M03) | **OK** ordinary L/U/B with exact areas; `1e-10` has a polygon and a true caption; central 1e-13 case is a resolution error; `(1,1e-13)` OK; huge-mean B = 50%; decimal comma OK |
| `vdist_normal_prob` | ordinary L/U/B/zero-width OK; rounded cutoff WRONG (H05); right intervals/complement/strip/collapsed median WRONG (H06/H10/H09); wide interval/huge mean/tiny peak CRASH (M02/M01/H15) | **OK**: cutoff reproduces percentage, `[9,10]`, `[8,8.1]` and complements exact; strip at the event at 1e±200; `1e308` mean OK; `[-1e300,1e300]` exact; `(1,1e-14)` rejected; `sd=3e-309` = 50% |
| `vdist_t_plot` | default and df 1/5.5/200 OK | **OK** (area still `pt(4)-pt(-4)`) |
| `vdist_t_perc` | ordinary L/U/B OK; df 1 `.001` U panel WRONG (H12); `1e-10` WRONG (H07); `.02` message M08; comma CRASH (M03) | **OK**: panel top `1.3*dt(0,1)`; `1e-10` has a polygon and a true caption; message names `0.9999999999`; comma OK |
| `vdist_t_prob` | ordinary L/U/B/I OK; L subtitle and tiny intervals/complements WRONG (H05/H06); distant cutoff/comma CRASH (M02/M03) | **OK**: printed cutoff matches percentage, interval 7.78e-16%, complement 1.19e-17%, `1e100` exact 100%, comma OK |
| `vdist_chisquare_plot` | df 1/5.5 normal overlay OK; df 200 range 25 WRONG (H11); df 3.14159 marker WRONG (H14) | **OK**: curve = `dchisq` across `[0,25]`; marker 3.14159 |
| `vdist_chisquare_perc` | ordinary L/U OK, low-df support regions OK; `1e-6` OK; comma CRASH (M03) | **OK**, including support-edge polygons and decimal comma |
| `vdist_chisquare_prob` | ordinary L/U/df 1/5.5/200 OK, negatives 0/1; `5,200` line WRONG (H11), cutoff/complement WRONG (H05/H06), `1e100` CRASH (M02) | **OK**: curve accurate, percentage matches cutoff, complement 1.52e-21%, `1e100` exact |
| `vdist_f_plot` | fractional dfs, overlay OK, `(1,1)` omits overlay; `(200,1e306)` WRONG (H13) | **OK**: `Std Dev. = 0.1`, overlay `dnorm(1,1,.1)` |
| `vdist_f_perc` | ordinary L/U and endpoints OK; `(1e6,1e6)` WRONG (H12); huge denominator WRONG (H13); comma CRASH (M03) | **OK**: body at true height; moments correct; comma OK |
| `vdist_f_prob` | ordinary L/U OK, negatives 0/1; rounded cutoff/complement/extreme moment WRONG (H05/H06/H13); `1e100` CRASH (M02) | **OK**: cutoff matches percentage, complement 1.09e-20%, `1e100` exact |
| `vdist_launch_app` | guarded delegation OK | **OK**, unchanged (delegates to `xplorerr`; no numeric result) |

Validation sweep from the audit (NA/empty/Inf/length-3/string in the first
argument; unknown `type`; reversed intervals; fractional `n`; invalid `p`;
nonpositive sd/df; chi-square/F floors; percentile endpoints; out-of-contract
probabilities) still rejects every case. New: `print_plot` (M05) and negative
binomial `s` (M06).

## Checks

Run on Windows, R 4.6.1 (UCRT), ggplot2 4.0.3, scales 1.4.0, testthat 3.3.2.

1. Full test suite, `NOT_CRAN=true`, in the worktree (vdiffr active):
   `devtools::test()` → **FAILED 0, ERROR 0, SKIP 0** (with
   `sum(r$failed)` and `sum(r$error)` both 0).
2. `devtools::document()` was used during development; the generated `man/`
   and `NAMESPACE` were reduced to the intentional help changes (the local
   roxygen 8.1.1 rewrote `NAMESPACE` and added an author block, which were
   reverted; `RoxygenNote` stays 7.3.3).
3. Clean build + check in a disposable copy made from `git archive HEAD`
   after the commits: `R CMD build` (vignettes rebuilt: OK) then
   `R CMD check --as-cran --no-manual`. Result: **0 errors, 0 warnings, 1
   NOTE**. The NOTE is CRAN incoming feasibility only (maintainer address
   changed from `hebbali.aravind@gmail.com` to the current CRAN address);
   tests ran in 94 s and the vignette rebuild in 12 s, both OK.
4. The new test file was also run against a clean `git archive 3ce6ec1`
   copy: **47 failed expectations and 7 errors** across all 24 test blocks
   (one or more per finding), so every finding has a test that fails before
   the fix and passes after it.
5. Snapshots: no new per-vertex snapshots were added. Eleven existing SVG
   sketches changed for intentional display reasons — the binomial caption
   text (H03/L01), and the density axis of the chi-square/F plots and
   chi-square/F percentile/probability plots (H12: bounded densities are no
   longer scaled down to an empty-tail percentile). Every changed sketch was
   regenerated with vdiffr on this platform and re-checked (visual tests pass).
6. README, vignettes and examples: `R CMD check` runs every example, the
   vignettes are rebuilt by `R CMD build`, and `README.Rmd`/`README.md` still
   knit/run. No documented example or teaching value now errors:
   normal mean 0–1000 with sd 0.01–1000, df 1–500, binomial n 1–10000 were
   exercised in the probe scripts and in the test suite.

Commands used (PowerShell 5.1):

```r
pkgload::load_all(".", helpers = FALSE)
Sys.setenv(NOT_CRAN = "true"); devtools::test()
```

```powershell
git archive HEAD | tar -x -C <temp-copy>
R CMD build <copy>
R CMD check --as-cran --no-manual vistributions_0.2.2.tar.gz
```

## Commits

* `fix: binomial bar edges, captions and continuous audit findings (H01-H15, M01-M06, M08, L01) (vist-fx10)`
* `fix: guard the suggested test packages (M07) (vist-fx10)`
* `test: value-asserting regressions for every round-10 finding (vist-fx10)`
* `docs: NEWS, help limits and round-10 report (vist-fx10)`

The R sources are interdependent (one data-preparation module holds the
binomial bins and every continuous prep, and one utility module holds the
print/format helpers), so the statistical fixes share one commit; the four
groups are separated where the files are independent.

## Anything left

* **H08/H09 are limits, not fixes.** A translated normal below
  `sd/|mean| = 3e-14` and a central probability whose endpoints are one double
  cannot be drawn truthfully at the display's resolution; they stop and name
  the valid range. `sd/|mean| = 1e-13` (one decade above the floor) is drawn
  and tested.
* **H12 changes the look of the existing chi-square/F sketches.** Bounded
  densities are no longer scaled down so that a nearly empty tail fills the
  panel; this is the audit's requested behaviour, and the eleven intentionally
  changed snapshots were regenerated. A very flat far-tail event can now
  occupy less than 1e-4 of the panel although its polygon area is exact; the
  internal visibility thresholds were lowered to 1e-8..1e-5 with comments
  saying why, and the area/caption assertions are unchanged.
* The audit's two check-hygiene notes were addressed: the wall-clock `< 10 s`
  assertion in `test-vfx7-findings.R` is unchanged (it is a row bound as well
  and passed well inside the limit here), and the audit report is not part of
  the package: it is untracked (`AUDIT-vistributions-022.md` is not committed
  and not in the tarball).
* `vdist_launch_app()` was not exercised against a live Shiny server; the
  delegation guard is unchanged from earlier rounds and has no numeric result
  to check.
