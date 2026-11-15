# vistributions 0.2.2 — fix round 3 (pkg-vfx3)

Worktree: `F:/R/Packages/_worktrees/vist-fix-022` (branch `task/vist-fix-022`,
head `0e48f1b` at the start, after vfx2). Findings: `docs-dev/vfxx2-reaudit.md`
(untracked and left in place), **VFXX2-H1–H4, M1, M2, L1**. No push, no GitHub
operations, no branch switch, no other worktree touched. Scratch is confined to
`.vfx3-tmp/`; the pre-existing `.vfxx-tmp/`, `.vfxx2-tmp/`, `Rplots.pdf` and
`docs-dev/vfxx-reaudit.md` were left alone.

Environment: Windows 11 x64, R 4.6.1 ucrt (2026-06-24), ggplot2 4.0.3,
testthat 3.3.2, vdiffr 1.0.9, devtools 2.5.2. Heavy commands ran through
`bash ~/.claude/bin/heavy-slot.sh pkg-vfx3 …`.

## Input contract enforced this round

The contract is enforced in R (validators and convergence checks in
`R/vdist-utils.R`), documented in roxygen/Rd and NEWS, and tested. A call
outside a limit stops with a package error naming that limit; nothing is
silently clipped to a different distribution.

| Function | Guaranteed parameters | Probability range | Sampling / convergence guarantee |
|---|---|---|---|
| `vdist_normal_plot()` | finite `mean`, finite `sd > 0`, density finite on `mean ± 4 sd` | — | 1000-point curve; polygons use the 0.01 grid (101 points when narrower than 20 steps) and are area-checked to 1e-3 |
| `vdist_normal_perc()` | finite `mean`, finite `sd > 0`; requested cutoff representable as a double relative to `mean` | `probs` in `[1e-10, 1 - 1e-10]` | `qnorm` result must invert its tail within `1e-3 × max(p, 1e-6)`; printed cutoff must reproduce the printed event within `1e-4 × max(p, 1e-4)`, otherwise numerical-resolution error |
| `vdist_normal_prob()` | finite `mean`, finite `sd > 0`, finite one or two `perc` values | event probabilities are computed, not supplied | view covers `perc` unless beyond the central `1 - 1e-9` mass; drawn lines/annotations clamped; polygon area checked to 1e-3 |
| `vdist_t_plot()` | finite `df > 0` | — | uniform `[-4, 4]` curve, finite density |
| `vdist_t_perc()` | finite `df > 0` | `probs` in `[1e-10, 1 - 1e-10]` | `qt` inversion checked; view extends to the 1e-4 tail quantile (or the cutoff when farther), so omitted tail mass is below 1e-3; polygon re-sampled until area error ≤ 1e-3 |
| `vdist_t_prob()` | finite `df > 0`, finite `perc` | event probability computed | same tail/view and area guarantee; lines/annotations clamped |
| `vdist_chisquare_plot()` | finite `df >= 0.1` | — | support begins at `qchisq(1e-6, df)` (no machine-epsilon floor); finite density; polygon area checked to 1e-3 |
| `vdist_chisquare_perc()` | finite `df >= 0.1` | `probs` in `[1e-6, 1 - 1e-6]` | `qchisq` inversion checked; support quantile; area checked to 1e-3; clipped drawn cutoff with true caption |
| `vdist_chisquare_prob()` | finite `df >= 0.1`, finite `perc` | event probability computed | support quantile; finite density; area checked; lines/annotations clamped |
| `vdist_f_plot()` | finite `num_df >= 1`, finite `den_df > 0`; central 99.9998% mass spans at least 0.001 | — | adaptive uniform step (`min(0.01, width/50)`, capped at 20,001 points) and re-sampling until polygon area error ≤ 1e-3; normal overlay only when `den_df > 4` |
| `vdist_f_perc()` | finite `num_df >= 1`, finite `den_df > 0`; same concentration limit | `probs` in `[1e-6, 1 - 1e-6]` | `qf` inversion checked; adaptive grid and per-polygon re-sampling to 1e-3; clipped drawn cutoff with true caption |
| `vdist_f_prob()` | finite `num_df >= 1`, finite `den_df > 0`; same concentration limit | event probability computed | adaptive grid and area re-sampling; lines/annotations clamped |

The F concentration limit is `qf(1 - 1e-6) - qf(1e-6) >= 0.001`; its error
message is “The F density is too concentrated to plot accurately…”. The
chi-square floor is `df >= 0.1`; the F floor is `num_df >= 1` (lower values
cannot be drawn from a representable support boundary). The normal/t
probability floor is 1e-10; the chi-square/F floor is 1e-6.

One redundant `vdist_t_plot(df = 8)` example call was removed (roxygen and Rd)
because the check reported its example page at 5.42 s elapsed; this is a
documentation/example-time change only, no code path.

## Files changed

| File | Change |
|---|---|
| `R/vdist-utils.R` | probability contracts, quantile inversion check, density/area checks, translated shoelace helper, clipped-view helper, tail/scale-aware cutoff formatter, adaptive F grid and concentration limit, no-epsilon support start, corrected `vdist_grid`/`vdist_seql` behaviour |
| `R/vdist-data-prep.R` | quantile validation and true captions for normal/t/chi-square/F; support starts in probability space; adaptive F grids and re-sampling; unrounded F moments; tail-aware t view; clipped cutoffs kept separate from captions; finite-density checks |
| `R/vdist-plots.R` | captions use the verified cutoff text; reference lines/markers/annotations use clipped `pc`; all annotation x positions clamped to the view; F overlay message; F moments formatted without losing small values; area checks on every polygon |
| `R/vdist-normal.R`, `R/vdist-t.R`, `R/vdist-chisquare.R`, `R/vdist-f.R` | exported validators call the documented probability limits and df floors |
| `NEWS.md`, `man/*.Rd` | release notes and generated documentation state the implemented limits (roxygen edited; only the four affected Rd files kept) |
| `docs-dev/vfx2-report.md` | corrected round-2 “all fixed”, F(5000) oracle, F3 representability, F4 zero-sd and F2/F-infinite-density claims |
| `tests/testthat/test-vfx3-findings.R` | one value test per finding, contract grid and limit-message tests |
| `tests/testthat/test-correctness-022.R`, `test-vfxx-findings.R` | updated the two round-2 cases that are now deliberately outside the documented contract; ordinary t/F area assertions kept |
| `tests/testthat/_snaps/visual/*.svg` | 15 intended snapshots (listed below) |

## Before / after evidence

The “before” rows are the audit reproductions or the worktree head before the
fix (`.vfx3-tmp/base.R`, `probe2.R`, `probe3.R`); “after” is
`.vfx3-tmp/evidence.R` on the fixed tree.

### VFXX2-H1 — chi-square mass discarded by the support floor

| Call (`print_plot = FALSE`) | Before area | After area | Truth |
|---|---:|---:|---:|
| `vdist_chisquare_perc(.05, .1)` | 0 | 0.0500044224 | .05 |
| `vdist_chisquare_perc(.5, .1)` | .3363500337 | 0.5000527280 | .5 |
| `vdist_chisquare_perc(.95, .1)` | .7864069192 | 0.9500894491 | .95 |
| `vdist_chisquare_perc(.95, .2)` | .9233664119 | 0.9500200194 | .95 |

The support now starts at `qchisq(1e-6, df)` itself (for df = .1 this is
1.168926e-120, density 4.277429e112), with no `.Machine$double.eps` floor.
`vdist_chisquare_perc(.95, .01)` is below the documented `df >= 0.1` floor and
now errors clearly instead of drawing 11.5% of a 95% event.

### VFXX2-H2 — unchecked quantiles / infinite F polygons

| Audit reproduction | Before | After |
|---|---|---|
| `vdist_f_perc(.5, .5, 3)` | `min(x) = 0`, one `Inf` density, NaN polygon | `num_df must be at least 1 for guaranteed numerical accuracy.` |
| `vdist_f_perc(1e-9, 1, 1)` | caption `P(X < 0) = 1e-07%` | `probs must be between 1e-06 and 0.999999 for guaranteed numerical accuracy.` |
| `vdist_chisquare_perc(1e-18, .1)` | positive probability for `X < 0` | same documented probability-limit error |

Every accepted `q*()` result is now checked for finiteness and for inverting
the requested tail; accepted F quantiles at `1e-6, .05, .5, .95, 1 - 1e-6`
have finite densities and true captions.

### VFXX2-H3 — scientific formatting lost a translated cutoff

Before: `vdist_normal_perc(.95, mean = 1, sd = 1e-13)` printed
`P(X < 1e+00) = 95%` (the mean). After:
`P(X < 1.0000000000001645) = 95%`; parsing that cutoff gives
`pnorm(..., 1, 1e-13) = 0.9500512` (the audit’s independently calculated
value) and it is distinct from the mean. The zero-centred `sd = 1e-16` case
still prints `P(X < 1.645e-16) = 95%`. With `mean = 1, sd = 1e-16` no printable
double reproduces 95%, so the function now raises a numerical-resolution error.

### VFXX2-H4 — concentrated valid F density

| Call (`print_plot = FALSE`) | Before area | After area | Truth |
|---|---:|---:|---:|
| `vdist_f_plot(1e6, 1e6)` | 1.994726 | 1.0000000000 (14,876 rows) | 1 |
| `vdist_f_plot(1e7, 1e7)` | 6.307831 | 1.0000000000 (20,001 rows) | 1 |
| `vdist_f_plot(1e8, 1e8)` | 19.94711 | 1.0000000053 (20,001 rows) | 1 |
| `vdist_f_plot(1e9, 1e9)` | wrong area | documented concentration-limit error | 1 |

The 1e9 case is beyond the documented central-mass width (0.000425 < 0.001);
it now stops. The accepted cases use a step refined to the central mass and a
polygon re-sampling loop.

### VFXX2-M1 — F normal overlay point mass

Before: `Mean = 1  Std Dev. = 0`, overlay ordinates `0 … Inf`. After:
`Mean = 1  Std Dev. = 0.0002`, max overlay 1994.711322, all finite, equal to
`dnorm(x, μ, σ)` with unrounded
`μ = 1e8/(1e8 - 2)`, `σ = sqrt(2 μ² (2e8 - 2)/(1e8 (1e8 - 4)))`.
For `den_df <= 4` (for example `vdist_f_plot(4, 3, normal = TRUE)`) the overlay
is omitted and a message says its standard deviation is undefined.

### VFXX2-M2 — clipped view defeated by reference lines

| Call | Before panel x-range | After panel x-range |
|---|---|---|
| `vdist_t_prob(1e6, 7)` | -50040.69 … 1050003 | -42.5765 … 42.5745 |
| `vdist_normal_prob(1e6)` | -50006.35 … 1050001 | -6.59759 … 6.59759 |
| `vdist_f_prob(1e5, 4, 30)` | -5000.04 … 105000.9 | -1.3945 … 29.2845 |

Reference lines, markers and text annotations now use the clipped `pc`; the
caption keeps the true event. Built-panel tests assert every reference line and
annotation lies inside the built range.

### VFXX2-L1 — release claims

NEWS now describes the actual ranges, df floors, concentration limit, adaptive
F sampling, quantile checks, overlay omission and clipping. Roxygen/Rd agree.
The round-2 report’s blanket “all eight fixed”, its F(5000) oracle (.9999644
was the area, `pf` is 1), its claim that the F(1,1) 1e-9 quantile is
unrepresentable (2.467401e-18 is representable; `qf` returns 0), and its claim
that a valid F `sd` cannot round to zero are corrected in place.

## Changed snapshots

All 15 changes are intentional; no snapshot was accepted to hide a failure.

| Snapshot | Why |
|---|---|
| `chisquare-perc-lower.svg` | caption cutoff `4.238` → `4.2384` so the printed event is true at printed precision |
| `f-perc-upper.svg` | caption cutoff `1.705` → `1.7047`, same reason |
| `f-plot-1.svg` | F plot grid re-sampled/re-stepped to meet the 1e-3 polygon-area guarantee; text unchanged |
| `f-plot-2.svg` | same grid change plus the unrounded-moment normal overlay (M1); text unchanged |
| `f-prob-lower.svg` | F shaded polygon/grid re-sampling and clamped annotation position; labels unchanged |
| `f-prob-upper.svg` | same F polygon/annotation change; labels unchanged |
| `normal-perc-both.svg` | caption cutoff `4.666` → `4.6656` (true printed event) |
| `normal-perc-upper.svg` | caption cutoff `2.713` → `2.7132` (true printed event) |
| `t-perc-lower.svg` | view extends to the 1e-4 tail quantile so the shaded area is correct; axis breaks/caption follow |
| `t-perc-upper.svg` | same tail-aware t view/area change |
| `t-perc-both.svg` | same tail-aware t view; caption cutoff `1.094` → `1.0944` |
| `t-prob-lower.svg` | tail-aware t view; axis breaks and shading extent change |
| `t-prob-upper.svg` | same |
| `t-prob-interval.svg` | same; interval mass now comes from the full drawn view |
| `t-prob-both.svg` | same; both-tail view/axis breaks |

All other visual snapshots are byte-identical.

## Tests added

`tests/testthat/test-vfx3-findings.R` (123 expectations; included in the
513-expectation suite):

- one test per finding using the audit’s reproduction (H1 areas, H2 rejected
  quantiles/probabilities, H3 translated cutoff, H4 concentrated F areas, M1
  unrounded overlay and undefined-variance message, M2 built panel ranges and
  annotation placement, L1 NEWS contract statements);
- a grid test over chi-square/t `df = .5, 1, 2, 5, 30` and F
  `num_df = 1, 2, 5, 30`, probabilities `.05, .5, .95`, asserting the shoelace
  area error is **< 1e-3 absolute**;
- a limit test asserting each documented limit errors with its message
  (chi-square df floor, F numerator floor, both probability floors, F
  concentration, normal coordinate precision).

The two round-2 tests that used now-out-of-contract inputs were updated:
F `num_df = .5` became `1.5`, and the chi-square overlay test now expects
`df = 1e-8` to error and checks the unrounded overlay at `df = 0.1`. No
correctness assertion was weakened.

## Commands run (that changed something)

```text
Rscript .vfx3-tmp/base.R / probe*.R / check*.R / evidence.R   # audit reproductions
Rscript -e 'devtools::test()'                                 # NOT_CRAN=true
Rscript -e 'devtools::document()'                             # kept only 4 affected Rd files
```

Scratch only in `.vfx3-tmp/`.

## Test / check output

```text
NOT_CRAN=true devtools::test()
ℹ Testing vistributions
[ FAIL 0 | WARN 0 | SKIP 0 | PASS 513 ]
```

## Clean build and `R CMD check`

From a clean `git archive HEAD` extraction (code commit `443d3d8`; this report
and all of `docs-dev/` are `.Rbuildignore`d and absent from the tarball):

```text
git archive HEAD | tar -x -C .vfx3-tmp/check-src
R CMD build --no-manual .
R CMD check --as-cran --no-manual vistributions_0.2.2.tar.gz
...
* checking examples ... [21s] OK
* checking tests ... [34s] OK
...
Status: 1 NOTE
```

The sole NOTE is the expected maintainer-address NOTE:

```text
* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Aravind Hebbali <aravindhebbali@rsquaredacademy.com>'
New maintainer:
  Aravind Hebbali <aravindhebbali@rsquaredacademy.com>
Old maintainer(s):
  Aravind Hebbali <hebbali.aravind@gmail.com>
```

No errors, no warnings. An earlier run had a second NOTE for
`vdist_t_plot` example elapsed time (5.42 s); one redundant example call was
removed (roxygen and Rd), after which examples completed OK. CRAN-mode tests
in the check skip the 15 vdiffr blocks and pass the remaining suite.

## Follow-ups (written out in full)

1. **pkgdown website** — `docs/` still shows 0.2.1. Rebuild with
   `pkgdown::build_site()` after this round and commit the generated site; it
   is deliberately not part of the numerical fix commit.
2. **Roxygen version** — installed roxygen2 8.1.1 vs `RoxygenNote: 7.3.3`.
   `devtools::document()` rewrote `DESCRIPTION`, `NAMESPACE` formatting and
   `man/vistributions.Rd`; those unrelated changes were reverted and only the
   four affected Rd files kept. Decide whether to adopt roxygen2 8.1.1 and
   regenerate everything, or pin 7.3.3.
3. **Very large df** — the F concentration limit covers df up to about 1e8.
   Larger df with a still-valid mathematical density is rejected by the
   documented 0.001 width limit rather than drawn inaccurately; relaxing it
   would need a better concentrated-density sampler.
4. **Shiny app** — `vdist_launch_app()` delegates to
   `xplorerr::app_vistributions()`; the app was not run in this round.
5. **Scratch directories** — `.vfx3-tmp/` contains this round’s scripts and
   logs. It is untracked and excluded from the clean `git archive` check, but
   should be removed before a build made directly from the worktree.
