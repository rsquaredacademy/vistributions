# vistributions 0.2.2 — fix round 4 (pkg-vfx4)

Worktree: `F:/R/Packages/_worktrees/vist-fix-022` (branch `task/vist-fix-022`,
head `f5f3e02` at the start, after vfx3). Findings: `docs-dev/vfxx3-reaudit.md`
(untracked and left in place), **VFXX3-H1, H2, M1, M2, M3, L1**. No push, no
GitHub operations, no branch switch, no other worktree touched. Scratch is
confined to `.vfx4-tmp/`; the pre-existing `.vfx3-tmp/`, `.vfxx*-tmp/`,
`Rplots.pdf` and earlier audit reports were left alone.

Environment: Windows 11 x64, R 4.6.1 ucrt (2026-06-24), ggplot2 4.0.3,
testthat 3.3.2, vdiffr 1.0.9, devtools 2.5.2. Heavy commands ran through
`bash ~/.claude/bin/heavy-slot.sh pkg-vfx4 …`.

Code commit: `1250673` (`fix: correct event areas, sampling grids and captions
(VFXX3-H1..M3) (vfx4)`). This report and the corrected round-3 report are a
later documentation-only commit; `docs-dev/` is `.Rbuildignore`d and is not in
the archive that was checked.

## Input contract enforced this round

A call outside a limit stops with a package error naming that limit. Every
continuous curve and polygon is built on a fixed number of points across the
visible range; the probability grid is unioned with a fixed-count uniform grid
so the far tail cannot form a spurious closing triangle. No fixed step in data
units (such as 0.01) is used anywhere. Shaded regions are checked against the
event the caption names: drawn area plus the probability outside the view must
match the stated probability within 1e-3.

| Function | Guaranteed parameters | Probability range | Sampling / convergence guarantee |
|---|---|---|---|
| `vdist_normal_plot()` | finite `mean`, finite `sd > 0`; when `mean != 0`, `sd / max(1, abs(mean)) >= 1e-14` | — | 1000-point curve on `mean ± 4 sd`; 1001-point probability+uniform polygons; polygon area checked to 1e-3 |
| `vdist_normal_perc()` | same normal resolution limit; requested cutoff representable | `probs` in `[1e-10, 1 - 1e-10]` | `qnorm` inversion checked; view covers the cutoff (or the central `1 - 1e-9` mass); polygon and event areas checked to 1e-3; caption cutoff printed to the displayed percentage precision |
| `vdist_normal_prob()` | same normal resolution limit; one or two finite `perc` values | event probability computed from the printed cutoff | view covers the cutoffs unless beyond the central mass; interval annotation uses the visible midpoint; polygon and event areas checked to 1e-3 |
| `vdist_t_plot()` | finite `df > 0` | — | 801-point curve on `[-4, 4]`, finite density |
| `vdist_t_perc()` | finite `df > 0`; very small `df` may hit the documented quantile/tail-bound error | `probs` in `[1e-10, 1 - 1e-10]` | `qt` inversion checked; view to the 1e-4 tail quantile (or the cutoff); 1001 grid points, 4001 when `df < 1`; polygon and event areas checked to 1e-3 |
| `vdist_t_prob()` | finite `df > 0`; same small-`df` representability limit | event probability computed from the printed cutoff | same view/grid; interval and both-tail captions use unrounded tail probabilities; polygon and event areas checked to 1e-3 |
| `vdist_chisquare_plot()` | finite `df >= 0.1`; finite positive `xaxis_range` | — | support starts at `qchisq(1e-6, df)`; 1001 grid points (10001 when `df < 2`); polygon area checked to 1e-3 |
| `vdist_chisquare_perc()` | finite `df >= 0.1` | `probs` in `[1e-6, 1 - 1e-6]` | `qchisq` inversion checked; upper-tail view reaches `qchisq(1 - 1e-4, df)`; polygon and event areas checked to 1e-3 |
| `vdist_chisquare_prob()` | finite `df >= 0.1`, finite `perc` | event probability computed from the printed cutoff | support start; upper-tail view reaches `qchisq(1 - 1e-4, df)`; polygon and event areas checked to 1e-3 |
| `vdist_f_plot()` | finite `num_df >= 1`, finite `den_df > 0`; central 99.9998% mass spans at least 0.001 | — | 1001 grid points (10001 when `num_df < 2`); polygon area checked to 1e-3; normal overlay only when `den_df > 4` |
| `vdist_f_perc()` | finite `num_df >= 1`, finite `den_df > 0`; same concentration limit | `probs` in `[1e-6, 1 - 1e-6]` | `qf` inversion checked; upper-tail view reaches `qf(1 - 1e-4, num_df, den_df)`; polygon and event areas checked to 1e-3 |
| `vdist_f_prob()` | finite `num_df >= 1`, finite `den_df > 0`; same concentration limit | event probability computed from the printed cutoff | upper-tail view reaches `qf(1 - 1e-4, num_df, den_df)`; polygon and event areas checked to 1e-3 |

Caption precision: percentages at or above 0.1 keep one decimal (trailing zeros
trimmed); smaller percentages use three significant digits in scientific
notation. The cutoff formatter accepts a printed value only when its `p*()`
value falls inside the rounding interval of that displayed percentage, so the
two always agree. Interval/tail probabilities stay unrounded until after the
union arithmetic.

The F concentration limit is `qf(1 - 1e-6) - qf(1e-6) >= 0.001`; the
chi-square floor is `df >= 0.1`; the F floor is `num_df >= 1`. The normal
translation limit is `sd / max(1, abs(mean)) >= 1e-14` when `mean != 0`.

## Files changed

| File | Change |
|---|---|
| `R/vdist-utils.R` | stable local-edge area; event check including omitted tail; normal-resolution limit; fixed-count probability+uniform grid; support-aware point counts; percentage/cutoff/value formatters tied to displayed precision; F grid simplified to fixed-count sampling |
| `R/vdist-data-prep.R` | normal/t/chi-square/F data preparation uses the new grids; upper-tail views reach the 1e-4 tail; omitted event mass computed with `p*()`; printed cutoffs and probabilities stored separately from full-precision values; interval/tail probabilities kept unrounded; chi-square and F shape polygons start at the support rather than adding a zero vertex |
| `R/vdist-plots.R` | captions use the verified cutoff/probability text; dark polygons are summed and checked against the event plus omitted mass; normal interval annotation uses the visible midpoint; polygon builders simplified to the fixed grid |
| `R/vdist-normal.R` | all three exports call the documented translation-resolution check; `sd` documentation states the 1e-14 limit |
| `R/vdist-chisquare.R` | all three exports enforce the documented `df >= 0.1` boundary |
| `R/vdist-f.R` | help states the 0.001 concentration limit |
| `NEWS.md` | corrects the old “uniform 0.01 grid” claim and records the round-4 user-visible fixes |
| `man/vdist_normal_plot.Rd`, `man/vdist_f_plot.Rd` | generated from the roxygen changes; unrelated roxygen-version changes reverted |
| `tests/testthat/test-vfx4-findings.R` | one value test per finding, every M1 input, every export default, the ordinary worked examples and the boundary tests |
| `tests/testthat/_snaps/visual/*.svg` | 29 intended snapshots (listed below) |
| `docs-dev/vfx3-report.md` | snapshot descriptions corrected to the actual round-3 SVG changes and marked as historical |

## Before / after evidence per finding

“Before” values are the audit’s independent measurements at `f5f3e02`; “after”
values are from `.vfx4-tmp/evidence.R` on the fixed tree. Areas are measured
from the actual polygon vertices with the independent local-edge formula.

### VFXX3-H1 — area guards validate the clipped polygon, not the requested event

| Call (`print_plot = FALSE`) | Before area | After area | Truth |
|---|---:|---:|---:|
| `vdist_chisquare_perc(.05, 1, type = "upper")` | .0454995887 | .0499000009 | .05 |
| `vdist_chisquare_perc(.95, .1, type = "upper")` | .9412027715 | .9499260647 | .95 |
| `vdist_chisquare_prob(0, 30, type = "upper")` | .2749681401 | .9998994340 | 1 |
| `vdist_f_perc(.05, 5, 10, type = "upper")` | .0202792620 | .0499001790 | .05 |
| `vdist_f_prob(0, 5, 10, type = "upper")` | .9703218700 | .9998998160 | 1 |
| `vdist_f_prob(1, 1, 1, type = "upper")` | .2048327659 | .4999003135 | .5 |
| `vdist_f_perc(.20, 4, 5, "upper")` (vignette) | .1196451841 | .1999017576 | .20 |
| `vdist_f_prob(3.89, 4, 5, "upper")` (vignette) | .0039966164 | .0842475135 | .0843468814 |

### VFXX3-H2 — translated tiny-scale normals

Before: `vdist_normal_perc(.5, mean = 1, sd = 1e-16)` printed
`P(X < 1e+00) = 50%` with a one-vertex, zero-area polygon;
`vdist_normal_prob(1, mean = 1, sd = 1e-16)` printed 50% with zero area; and
`vdist_normal_plot()` raised an internal `missing value where TRUE/FALSE
needed` error. After: all three exports reject the translated case before
plotting, with the documented limit in the message.

| Call | After |
|---|---|
| `vdist_normal_perc(.95, mean = 1, sd = 1e-13)` | `P(X < 1.0000000000001645) = 95%`; accepted |
| `vdist_normal_perc(.95, mean = 1, sd = 1e-14)` | `P(X < 1.00000000000001643) = 95%`; accepted |
| `vdist_normal_perc(.95, mean = 1, sd = 1e-15)` | error: `sd / max(1, \|mean\|) = 1e-15 is below the documented limit of 1e-14` |
| `vdist_normal_perc(.5, mean = 1, sd = 1e-16)` | same documented-limit error |
| `vdist_normal_prob(1, mean = 1, sd = 1e-16)` | same documented-limit error |
| `vdist_normal_plot(mean = 1, sd = 1e-16)` | same documented-limit error (not an internal error) |
| `vdist_normal_plot(sd = 1e-16 / 1e-100 / 1e-300)` | all accepted; zero-centred grids are evaluated on the standardized scale |

### VFXX3-M1 — ordinary documented inputs were rejected

| Call (`print_plot = FALSE`) | Before | After area | Truth |
|---|---|---:|---:|
| `vdist_normal_perc(.5, type = "both")` | error, area .49856969 | .4999999674 | .5 |
| `vdist_normal_perc(.95, sd = .1)` | error, area .94754011 | .9499991379 | .95 |
| `vdist_normal_prob(.1645, sd = .1)` | error, area .94751641 | .9499830227 | .9500150945 |
| `vdist_chisquare_plot(2.01)` | error, area .99757892 | 1.0000035207 | .9999962137 |
| `vdist_chisquare_prob(0, 2.01)` | error on remainder polygon | 0 | 0 |
| `vdist_t_perc(.95, .1)` | error, area 0 | .9499884444 | .95 |
| `vdist_t_perc(.95, .2)` | error | .9499233559 | .95 |

Very small `df` now gives a specific representability error rather than a NaN
or “too concentrated”: `df = .001` fails at the quantile
(`cannot be represented numerically`), and `df = .01` fails at the tail bound
(`the t distribution tail cannot be represented`).

### VFXX3-M2 — caption precision

Before, `probs = .123456789` printed 12.3456789% while the 3/4-decimal cutoff
only reproduced the probability to about 1e-4. After, the same calls print
12.3%, and the printed cutoff reproduces that displayed percentage:

| Function / parameters | Before printed cutoff (CDF) | After caption |
|---|---:|---|
| normal defaults | -1.1579 (.1234524) | `P(X < -1.158) = 12.3%` |
| t, df = 7 | -1.2633 (.1234645) | `P(X < -1.2633) = 12.3%` |
| chi-square, df = 1 | .02413 (.1234454) | `P(X < 0.02413) = 12.3%` |
| F, df = 5/10 | .3412 (.1234479) | `P(X < 0.3412) = 12.3%` |

For `probs = .012345`, all four print 1.2% with true cutoffs:
`-2.246`, `-2.85`, `0.0002394`, `0.1093`.

Double rounding is gone: `vdist_t_prob(.1, 7, "interval")` now prints
`P(-0.1 < X < 0.1) = 7.7%` (`pt(.1,7)-pt(-.1,7) = .07685194`), and
`type = "both"` prints 92.3% (`2*pt(-.1,7) = .9231481`).

### VFXX3-M3 — normal interval annotation

| Call | Before panel x-range | After panel x-range | After curve x-range |
|---|---|---|---|
| `vdist_normal_prob(c(1e6, 2e6))` | about -75006 … 1.6e6 | -6.5975877 … 6.5975877 | -5.997807 … 5.997807 |
| `vdist_normal_prob(c(-1e6, 2e6))` | about -25006 … 525000 | -6.5975877 … 6.5975877 | -5.997807 … 5.997807 |

The central annotation is now placed at the midpoint of the visible cutoffs, so
the panel is set by the curve; every text annotation is inside the built panel.

### VFXX3-L1 — contract, help, NEWS and snapshot descriptions

- Chi-square floor: before, all three exports tested `df < .05` while the
  message said 0.1, so `.075` and `.099999` were accepted. After, all three
  reject `.099999` with `df must be at least 0.1`; `.1` is accepted and passes
  its area check.
- F concentration limit: now documented in `?vdist_f_plot` (`0.001`), not only
  in NEWS. The `%` in that sentence was escaped as `\%`; without it Rd treated
  the rest of the line as a comment and the checked help silently lost the
  limit. This was found by the new installed-help test.
- NEWS no longer claims a uniform 0.01 grid, and states the fixed-count
  probability+uniform grid, event check, normal translation limit, caption
  precision, interval annotation clamp and chi-square floor.
- The round-3 report’s snapshot table was corrected to the actual SVG changes
  (for example `f-plot-1.svg` moved only the mean marker; it did not resample
  the curve or polygon) and marked historical.

## Must-pass table

All calls were run with `print_plot = FALSE`; areas are the dark shaded
polygons (or the shape polygon for shape plots). “Caption” is the printed
subtitle/probability.

| Call | Truth | Area | Difference | Caption |
|---|---:|---:|---:|---|
| `vdist_normal_perc(.5, type = "both")` | .5 | .4999999674 | 3.3e-8 | `P(-0.6745 < X < 0.6745) = 50%` |
| `vdist_normal_perc(.95, sd = .1)` | .95 | .9499991379 | 8.6e-7 | `P(X < 0.1645) = 95%` |
| `vdist_normal_prob(.1645, sd = .1)` | .9500150945 | .9499830227 | 3.2e-5 | `P(X < 0.1645) = 95%` |
| `vdist_chisquare_plot(2.01)` (shape) | .9999962137 | 1.0000035207 | 7.3e-6 | `df = 2.01` |
| `vdist_chisquare_prob(0, 2.01)` | 0 | 0 | 0 | `P(X < 0) = 0%` |
| `vdist_t_perc(.95, .1)` | .95 | .9499884444 | 1.2e-5 | `P(X < 1642931922.603) = 95%` |
| `vdist_chisquare_perc(.05, 1, type = "upper")` | .05 | .0499000009 | 1.0e-4 | `P(X > 3.841) = 5%` |
| `vdist_chisquare_perc(.95, .1, type = "upper")` | .95 | .9499260647 | 7.4e-5 | `P(X > 1.115e-26) = 95%` |
| `vdist_chisquare_prob(0, 30, type = "upper")` | 1 | .9998994340 | 1.0e-4 | `P(X > 0) = 100%` |
| `vdist_f_perc(.05, 5, 10, type = "upper")` | .05 | .0499001790 | 1.0e-4 | `P(X > 3.326) = 5%` |
| `vdist_f_prob(0, 5, 10, type = "upper")` | 1 | .9998998160 | 1.0e-4 | `P(X > 0) = 100%` |
| `vdist_f_prob(1, 1, 1, type = "upper")` | .5 | .4999003135 | 1.0e-4 | `P(X > 1) = 50%` |
| `vdist_f_perc(.20, 4, 5, "upper")` | .20 | .1999017576 | 9.8e-5 | `P(X > 2.24) = 20%` |
| `vdist_f_prob(3.89, 4, 5, "upper")` | .0843468814 | .0842475135 | 9.9e-5 | `P(X > 3.89) = 8.4%` |
| `vdist_normal_perc(.95, mean = 100, sd = 15)` | .95 | .9499991379 | 8.6e-7 | `P(X < 124.673) = 95%` |
| `vdist_t_perc(.95, df = 5)` | .95 | .9498999182 | 1.0e-4 | `P(X < 2.015) = 95%` |
| `vdist_chisquare_perc(.95, df = 1)` | .95 | .9499991678 | 8.3e-7 | `P(X < 3.841) = 95%` |
| `vdist_chisquare_perc(.05, df = 3, type = "upper")` | .05 | .0499000826 | 1.0e-4 | `P(X > 7.815) = 5%` |
| `vdist_f_perc(.95, 5, 10)` | .95 | .9499986779 | 1.3e-6 | `P(X < 3.326) = 95%` |

Every exported plotting default (`vdist_normal_plot/perc/prob`,
`vdist_t_plot/perc/prob`, `vdist_chisquare_plot/perc/prob`,
`vdist_f_plot/perc/prob`, `vdist_binom_plot/perc/prob`) was also called with
`print_plot = FALSE`; all 15 returned a `ggplot` without error.

## Changed snapshots (29)

All changes are intended and follow from the fixed-count probability+uniform
sampling (root cause 1) or from an event/caption fix. No snapshot was accepted
to hide a failing value assertion. Binomial snapshots and the three `t plot`
snapshots are unchanged.

| Snapshot | Why |
|---|---|
| `chisquare-plot-1.svg` | chi-square curve/polygon now use the fixed 1001-point probability+uniform grid |
| `chisquare-plot-1a.svg` | same grid change, `xaxis_range = 35` |
| `chisquare-plot-2.svg` | same grid change, `df = 5` |
| `chisquare-plot-3.svg` | same grid change plus the normal overlay on the same grid |
| `chisquare-perc-lower.svg` | polygon grid and sampled vertices change |
| `chisquare-perc-upper.svg` | event view now reaches the upper tail; polygon extent and vertices change |
| `chisquare-prob-lower.svg` | polygon grid and sampled vertices change |
| `chisquare-prob-upper.svg` | upper event view reaches the tail; polygon extent and vertices change |
| `f-plot-1.svg` | F shape curve/polygon now use the fixed grid |
| `f-plot-2.svg` | same F shape grid change; normal overlay follows the same x grid |
| `f-perc-lower.svg` | polygon grid and sampled vertices change |
| `f-perc-upper.svg` | upper event view reaches `qf(1 - 1e-4)`; polygon extent and vertices change |
| `f-prob-lower.svg` | polygon grid and sampled vertices change |
| `f-prob-upper.svg` | upper event view reaches the tail; polygon extent and vertices change |
| `normal-plot-1.svg` | central/shaded normal polygons now use the fixed probability+uniform grid; curve and text unchanged |
| `normal-plot-2.svg` | same polygon grid change; curve and text unchanged |
| `normal-perc-lower.svg` | polygon grid and sampled vertices change |
| `normal-perc-upper.svg` | polygon grid and sampled vertices change |
| `normal-perc-both.svg` | polygon grid and sampled vertices change |
| `normal-prob-lower.svg` | polygon grid and sampled vertices change |
| `normal-prob-upper.svg` | polygon grid and sampled vertices change |
| `normal-prob-both.svg` | polygon grid change; annotation stays at the visible midpoint |
| `t-perc-lower.svg` | t curve/polygon now use the fixed probability+uniform grid |
| `t-perc-upper.svg` | same t grid change |
| `t-perc-both.svg` | same t grid change |
| `t-prob-lower.svg` | same t grid change |
| `t-prob-upper.svg` | same t grid change |
| `t-prob-interval.svg` | same t grid change |
| `t-prob-both.svg` | same t grid change |

## Commands run (that changed something)

```text
Rscript .vfx4-tmp/probe*.R, evidence.R     # audit reproductions and after evidence
Rscript -e 'devtools::test()'              # NOT_CRAN=true
Rscript -e 'devtools::document()'          # kept only vdist_f_plot.Rd and vdist_normal_plot.Rd
git archive <code commit> | tar -x -C .vfx4-tmp/check-src
R CMD build --no-manual .
R CMD check --as-cran --no-manual vistributions_0.2.2.tar.gz
```

All heavy R commands used
`bash ~/.claude/bin/heavy-slot.sh pkg-vfx4 …`; build/check used
`LC_ALL=C LANG=C LANGUAGE=en`.

## Test / check output

Source tree, full suite including all 39 visual snapshots:

```text
NOT_CRAN=true devtools::test()
ℹ Testing vistributions
binomial: .........................
bug-regressions: ........................................................
chisquare: ............
correctness-022: ....................................................................................................
f: ...................
normal: .......................
plot-assembly: .......
t: ...........
vfx3-findings: ...........................................................................................................................
vfx4-findings: ............................................................................................
vfxx-findings: ..................................................................................................
visual: .......................................

[ FAIL 0 | WARN 0 | SKIP 0 | PASS 605 ]
```

Clean archive from code commit `1250673` (also built from the final HEAD;
the docs commit adds only `.Rbuildignore`d files, so the archive is identical):

```text
R CMD build --no-manual .                    exit 0 (vignette built)
R CMD check --as-cran --no-manual …          exit 0
* checking CRAN incoming feasibility ... OK
* checking examples ... [17s] OK
* checking tests ...
  Running 'testthat.R' [56s]
 [ FAIL 0 | WARN 0 | SKIP 0 | PASS 605 ]
* checking re-building of vignette outputs ... OK
Status: OK
```

That run used `NOT_CRAN=true` so the vdiffr snapshots actually run rather than
being skipped and reported as unused, and
`_R_CHECK_CRAN_INCOMING_REMOTE_=false` because the local Windows schannel stack
cannot validate the documentation host. With remote incoming checks left on,
the same archive gives one NOTE: the expected maintainer-address block plus

```text
Found the following (possibly) invalid URLs:
  URL: https://vistributions.rsquaredacademy.com
    From: DESCRIPTION, man/vistributions.Rd
    Status: Error
    Message: libcurl error code 60:
      schannel: SNI or certificate check failed ...
      (Status without verification: OK)
```

This is an environment TLS/principal-name failure (the unverified response is
OK), not a package error; the URL is the documented pkgdown site. No errors and
no warnings in either run.

## Follow-ups (written out in full)

1. **pkgdown website** — `docs/` still shows the older 0.2.1 site. Rebuild with
   `pkgdown::build_site()` after this round and commit the generated site; it
   is deliberately not part of this numerical fix.
2. **Shiny app** — `vdist_launch_app()` delegates to
   `xplorerr::app_vistributions()`; the live app was not run in this round.
3. **Very small t degrees of freedom** — the stated contract is `df > 0`, but
   at `df = .001` the 95% quantile itself is not representable and at `df = .01`
   the 1e-4 tail bound is not finite. Both now stop with specific messages.
   Decide whether to document a practical `df` floor or keep `df > 0` with the
   representability errors.
4. **Extremely heavy F upper tails** — `vdist_f_prob(1, 1, 1, "upper")` now has
   the correct area, but its view extends to about `4e7` so the density is
   visually compressed. A future rendering cap would need an explicit
   “probability beyond view” label rather than the current computed-tail check.
5. **Roxygen version** — installed roxygen2 8.1.1 vs the repository’s
   `RoxygenNote`; `devtools::document()` rewrote `DESCRIPTION`, `NAMESPACE` and
   `man/vistributions.Rd`. Those unrelated changes were reverted and only the
   two affected Rd files kept.
6. **Scratch directories** — `.vfx4-tmp/` contains this round’s scripts, logs
   and archive build/check products. It is untracked and excluded from the
   `git archive` check, but should be removed before a build made directly from
   the worktree. Pre-existing `.vfx3-tmp/`, `.vfxx*-tmp/`, `Rplots.pdf` and the
   earlier audit reports were left untouched.

## Deferred

None of the six remaining audit findings is deferred. The only deferred items
are the non-numerical follow-ups listed above.
