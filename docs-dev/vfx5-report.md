# vistributions 0.2.2 — fix round 5 (pkg-vfx5)

Worktree: `F:/R/Packages/_worktrees/vist-fix-022` (branch `task/vist-fix-022`,
head `f698b87` after vfx4). Findings: `docs-dev/vfxx4-reaudit.md` (untracked and
left in place), **VFXX4-H1, H2, M1, M2, L1**. No push, no GitHub operations, no
branch switch, no other worktree touched. Scratch is confined to `.vfx5-tmp/`;
the pre-existing `.vfxx*-tmp/`, `.vfx3-tmp/`, `.vfx4-tmp/`, `Rplots.pdf` and all
earlier audit reports were left alone.

Environment: Windows 11 x64, R 4.6.1 ucrt (2026-06-24), ggplot2 4.0.3,
testthat 3.3.2, vdiffr 1.0.9, devtools 2.5.2. Heavy commands ran through
`bash ~/.claude/bin/heavy-slot.sh pkg-vfx5 …`.

Code commits: `5a35260` (`fix: standardize density display, event captions and
limits (VFXX4-H1/H2/M1/M2/L1) (vfx5)`) and `6b61940` (`fix: keep very large
cutoff labels to significant digits (vfx5)`). The second prints a very large
cutoff with significant digits (for example `P(X < -1.60443e+96) = 1e-08%`)
instead of a 90-digit fixed number; it affects no ordinary snapshot. This report
is a later documentation-only commit under `.Rbuildignore`d `docs-dev/`.

## Design of this round

1. **Standardized / probability-spaced geometry.** Every curve and polygon is
   sampled at a fixed number of points in probability space (with a fixed-count
   uniform grid for the far tail); no step in data units is used anywhere. The
   probability grid now also refines each quantile with `uniroot` when
   `stats::q*()` does not invert its own `p*()` (needed at very large F df).
2. **Density display scale.** The density panel is scaled to the density's own
   level: normal annotation offsets are a fraction of the peak, and densities
   unbounded at a support edge are capped at a high quantile of the density in
   the shaded event. The cap is a *display* limit only: all area checks use the
   unclipped density, so the printed probability is the true probability.
3. **Very wide t/F windows** (heavy tails, `df < 1`) are drawn on a symmetric
   log axis. The data, the area check and the caption are unchanged; the axis
   only prevents the event from collapsing into a line.
4. **The printed cutoff defines the shaded event.** Polygons, markers and the
   caption are computed from the printed cutoffs (full-precision cutoffs are
   kept for the geometry when the printed value is only a label); the caption's
   percentage is the probability those printed cutoffs actually attain, at the
   precision printed. Central intervals are formatted and checked as a pair.
5. **Off-view event mass is disclosed** in a plot caption whenever it exceeds
   `1e-7`, because the area check adds it to the drawn polygon.

## Updated contract table

| Function | Guaranteed parameters | Probability range | Sampling / convergence guarantee |
|---|---|---|---|
| `vdist_normal_plot()` | finite `mean`, finite `sd > 0`; when `mean != 0`, `sd / abs(mean) >= 1e-14`; finite density (`sd >= ~1e-308`) and finite `mean +/- 4 sd` | — | 1000-point curve on `mean ± 4 sd`; polygons on a 1001-point probability grid; density axis scaled to the peak; area checked against the event |
| `vdist_normal_perc()` | same normal limits | `probs` in `[1e-10, 1 - 1e-10]` | `qnorm` inversion checked; cutoff inside the view; polygon/event areas checked to 1e-3; printed cutoff and displayed percentage describe the same event; central pairs checked jointly |
| `vdist_normal_prob()` | same normal limits; one or two finite `perc` | — | event probability computed from the printed cutoff; visible midpoint for intervals; polygon/event areas checked to 1e-3 |
| `vdist_t_plot()` | finite `df > 0` | — | 801-point curve on `[-4, 4]`, finite density, density axis scaled to the curve |
| `vdist_t_perc()` | finite `df > 0`; very small `df` may hit the documented quantile/tail-bound error | `probs` in `[1e-10, 1 - 1e-10]` | `qt` inversion checked/refined; view to the 1e-4 tail quantile; 1001 grid points (4001 when `df < 1`); symmetric log axis for windows wider than 1e6; polygon/event areas checked to 1e-3 |
| `vdist_t_prob()` | finite `df > 0`; same small-`df` representability limit | event probability computed from the printed cutoff | same view/grid/axis; interval and both-tail captions use the printed cutoff's probability; polygon/event areas checked to 1e-3 |
| `vdist_chisquare_plot()` | finite `df >= 0.1`; finite positive `xaxis_range` | — | support starts at `qchisq(1e-6, df)`; 1001 grid points (10001 when `df < 2`); density axis capped for a singular support edge; polygon area checked to 1e-3 |
| `vdist_chisquare_perc()` | finite `df >= 0.1` | `probs` in `[1e-6, 1 - 1e-6]` | `qchisq` inversion checked; upper view reaches `qchisq(1 - 1e-4, df)`; polygon/event areas checked to 1e-3 |
| `vdist_chisquare_prob()` | finite `df >= 0.1`, finite `perc` | — | support start; upper view reaches `qchisq(1 - 1e-4, df)`; polygon/event areas checked to 1e-3 |
| `vdist_f_plot()` | finite `num_df >= 1`, finite `den_df > 0`; central 99.9998% mass finite and at least 0.001 | — | 1001 grid points (10001 when `num_df < 2` or `den_df < 1`); density axis capped for a singular support edge; polygon area checked to 1e-3 |
| `vdist_f_perc()` | finite `num_df >= 1`, finite `den_df > 0`; same concentration limit | `probs` in `[1e-6, 1 - 1e-6]` | `qf` inversion checked and root-refined; upper view reaches `qf(1 - 1e-4)`; symmetric log axis for windows wider than 1e6; polygon/event areas checked to 1e-3 |
| `vdist_f_prob()` | finite `num_df >= 1`, finite `den_df > 0`; same concentration limit | event probability computed from the printed cutoff | upper view reaches `qf(1 - 1e-4)`; symmetric log axis for wide windows; polygon/event areas checked to 1e-3 |

Caption precision: percentages at or above 0.1 keep one decimal (trailing zeros
trimmed); smaller percentages use three significant digits in scientific
notation. The printed cutoff is accepted only when its `p*()` value falls inside
the rounding interval of the displayed percentage, and the displayed percentage
is recomputed from the printed cutoffs, so the two always agree. Central
intervals are accepted only as a pair whose interval probability is inside that
rounding interval.

Additional validated limits stated in the help pages: normal density/range
limits above; F tail overflow (`den_df` so small that the 1e-6…1−1e-6 width is
not finite) is a separate error from concentration; quantiles are root-refined
where `stats::q*()` is inaccurate (equal F df up to at least `1e8` are tested).

## Files changed

| File | Change |
|---|---|
| `R/vdist-utils.R` | unit-ratio normal guard (`sd / abs(mean)`), normal density/range limits, quantile root refinement, event check unchanged but paired with disclosure, coupled interval formatter, density display cap, wide-window symmetric-log scale, off-view caption text, chi-square grid refinement and F grid/n refinements |
| `R/vdist-data-prep.R` | all percentile preps use refined quantiles, printed cutoffs for captions and full-precision geometry, paired central formatting, per-event density caps; event probability computed from printed cutoffs; F grid point count raised for `den_df < 1` |
| `R/vdist-plots.R` | density axis scaled to the cap for every continuous family, relative annotation heights, symmetric-log x axis for wide t/F windows, off-view caption, polygon builders use the probability grid only (no coarse uniform filler) |
| `R/vdist-normal.R`, `man/vdist_normal_plot.Rd` | `sd / abs(mean)` limit and finite density/range limits documented |
| `R/vdist-f.R`, `man/vdist_f_plot.Rd` | tail overflow distinguished from concentration; refinement and display-scale notes |
| `DESCRIPTION` | `scales` added to Imports (already a hard ggplot2 dependency; no new package is installed) |
| `NEWS.md` | one bullet per user-visible fix |
| `tests/testthat/test-vfx5-findings.R` | new: H1, H2 rendered visibility, M1 paired captions, M2 limits, unit invariance, must-pass table, defaults, docs |
| `tests/testthat/test-correctness-022.R` | restored exact-cutoff marker assertions after the geometry was returned to full precision |
| `tests/testthat/_snaps/visual/*.svg` | 32 intended snapshots (list below) |

## Before / after evidence per finding

Areas are measured from the polygon vertices with the independent local-edge
formula `abs(sum(diff(c(x, x[1])) * (y + c(y[-1], y[1])))) / 2`. “Rendered” is
the fraction of the panel covered by the clipped dark polygon after
`ggplot_build()` (coordinates mapped through the panel's own scale transform).

### VFXX4-H1 — resolvable scaled normals were rejected

| Call (`print_plot = FALSE`) | Before | After |
|---|---|---|
| `vdist_normal_plot(mean = 1e-16, sd = 1e-16)` | error: `sd / max(1, \|mean\|) = 1e-16 …` | plot; shape area `0.682689` (truth `0.682689`), rendered `0.165` |
| `vdist_normal_perc(.95, mean = 1e-16, sd = 1e-16)` | same error | `P(X < 2.6449e-16) = 95%`; area `0.950012`; rendered `0.183` |
| `vdist_normal_prob(2.645e-16, mean = 1e-16, sd = 1e-16)` | same error | area `0.949982` (truth `0.950015`); rendered `0.229` |
| `mean = 1, sd = 1e-15` | documented rejection | still rejected (ratio `1e-15 < 1e-14`) |

The guard is now the unit-free ratio `sd / |mean|`; scaling mean and sd by the
same factor never changes it.

### VFXX4-H2 — source areas became invisible shading

| Call (`print_plot = FALSE`) | Source area (after) | Rendered dark fraction (after) | Before: emitted SVG area |
|---|---:|---:|---:|
| `vdist_chisquare_perc(.05, 1, "upper")` | .049914 | 0.085 | 0 |
| `vdist_f_prob(1, 1, 1, "upper")` | .499900 | 0.026 | 0 |
| `vdist_t_perc(.95, .1)` | .949988 | 0.0042 | 0 |
| `vdist_chisquare_perc(.95, 1)` | .949986 | 0.026 | .0096 |
| `vdist_normal_perc(.95, mean = 1e8, sd = 1e7)` | .949996 | 0.183 | 0 |
| `vdist_normal_perc(.95, mean = 1e6, sd = 1e5)` | .949996 | 0.183 | invisible baseline |
| `vdist_normal_prob(1164500, mean = 1e6, sd = 1e5)` | .949982 | 0.229 | invisible baseline |

The measured rendered fraction is a panel-normalized area; a value above
`1e-4` is asserted for every must-pass input in the new test file. The
`normal-perc`/`normal-prob` snapshots and the money-unit cases are also checked
in the PNG evidence under `.vfx5-tmp/`.

### VFXX4-M1 — central percentile captions were false at printed precision

| Call (`print_plot = FALSE`) | Before caption | After caption (and the printed event's probability) |
|---|---|---|
| `vdist_normal_perc(.0001, mean = 1, type = "both")` | `P(0.9999 < X < 1) = 1e-02%` (3.99e-5) | `P(0.9999 < X < 1.0001) = 7.98e-03%` (7.98e-5) |
| `vdist_normal_perc(.0001, mean = 1, sd = .1, type = "both")` | `P(1 < X < 1) = 1e-02%` (0) | `P(0.99999 < X < 1.00001) = 7.98e-03%` (7.98e-5) |
| `vdist_normal_perc(1e-10, mean = 1, type = "both")` | `P(1 < X < 1) = 1e-08%` (0) | `P(0.9999999999 < X < 1.0000000001) = 7.98e-09%` (7.98e-11) |
| `vdist_normal_perc(.0505, type = "both")` | `P(-0.06333 < X < 0.06333) = 5.1%` (5.0496%) | `P(-0.063335 < X < 0.063335) = 5.1%` (5.050025%) |
| `vdist_normal_perc(.0505, mean = 100, type = "both")` | `P(99.937 < X < 100.063) = 5.1%` (5.0233%) | `P(99.936665 < X < 100.063335) = 5.1%` (5.050025%) |

Each “after” caption is true at the half-unit of its printed percentage
(asserted by the new test). Every central caption is now formatted as a pair.

### VFXX4-M2 — the advertised domain contained finite failures

| Input | Before | After |
|---|---|---|
| `vdist_t_perc(1e-10, .1)` | error: area `1.0011274` vs `1` | plot; `P(X < -1.60443e+96) = 1e-08%`; the 1e-10 event lies beyond the view and is counted by the area check (area `0`) |
| `vdist_f_prob(0, 2, .5, "upper")` | error: area `1.0015961` vs `.9999` | area `.999917`; caption `P(X > 0) = 100%` |
| `vdist_f_perc(.95, 2, .1)` | error: area `.96831008` vs `.95` | area `.950192` |
| `vdist_f_perc(1e-6, 5, .5, "upper")` | error: area `1.0021338` vs `.999999` | area `5.4e-8`; the 1e-4 off-view event mass is stated in the caption |
| `vdist_f_perc(.95, 1e8, 1e8)` | quantile-inversion error | plots; area `.950491`; `P(X < 1.00033) = 95.1%` (root-refined quantiles) |
| `vdist_f_plot(2, .01)` | says mass spans `Inf`, “less than” .001, advises smaller df | tail-overflow error: “does not span a finite interval … the tail does not converge at double precision. Use larger degrees of freedom.” |

The F probability grid no longer mixes a coarse uniform filler into the polygon
(that filler caused the `1.002` over-estimate), and `den_df < 1` uses 10001
probability points.

### VFXX4-L1 — documentation/report/test claims

- `man/vdist_normal_plot.Rd` now states `sd / abs(mean) >= 1e-14` and the
  finite-density/range limits; `man/vdist_f_plot.Rd` states finite width,
  the tail-overflow distinction, the display scale and the refinement.
- NEWS states the unit ratio and the rendered-visibility fix instead of the
  false “always agrees”/“any positive sd” guarantees.
- The new must-pass test asserts areas and rendered visibility, not only class.
- The contract table above matches the implementation, including the two
  documented normal double-precision limits and the F tail-overflow error.

## Must-pass table (all with `print_plot = FALSE`)

| Call | Truth | Area / selected mass | Rendered dark fraction |
|---|---:|---:|---:|
| `vdist_normal_plot()` | .9973002039 | .9973001 (shape) | — |
| `vdist_normal_perc()` | .95 | .950012 | 0.183 |
| `vdist_normal_prob()` | .9986501 | .998616 | 0.241 |
| `vdist_t_plot()` | .9719915 | .9719914 (shape) | — |
| `vdist_t_perc()` | .95 | .949909 | 0.075 |
| `vdist_t_prob()` | .9231841 | .923084 | 0.131 |
| `vdist_chisquare_plot()` | .9999846 | .9999819 (shape) | — |
| `vdist_chisquare_perc()` | .95 | .950012 | 0.198 |
| `vdist_chisquare_prob()` | .7066746 | .706664 | 0.236 |
| `vdist_f_plot()` | .9897892 | .9897875 (shape) | — |
| `vdist_f_perc()` | .95 | .949996 | 0.256 |
| `vdist_f_prob()` | .9367237 | .936725 | 0.244 |
| `vdist_normal_perc(.5, type = "both")` | .5 | .500006 | 0.096 |
| `vdist_normal_perc(.95, sd = .1)` | .95 | .950012 | 0.183 |
| `vdist_normal_prob(.1645, sd = .1)` | .950015 | .949982 | 0.229 |
| `vdist_chisquare_plot(2.01)` | .9999962 | 1.000004 (shape) | — |
| `vdist_chisquare_prob(0, 2.01)` | 0 | 0 | — |
| `vdist_t_perc(.95, .1)` | .95 | .949988 | 0.0042 |
| `vdist_t_perc(.95, .2)` | .95 | .949923 | 0.0099 |
| `vdist_normal_perc(.95, mean = 100, sd = 15)` | .95 | .949998 | 0.183 |
| `vdist_t_perc(.95, df = 5)` | .95 | .949897 | 0.099 |
| `vdist_chisquare_perc(.95, df = 1)` | .95 | .949986 | 0.026 |
| `vdist_chisquare_perc(.05, df = 3, type = "upper")` | .05 | .049894 | 0.081 |
| `vdist_f_perc(.95, 5, 10)` | .95 | .950021 | 0.266 |

(Binomial truths are the exact `dbinom`/`pbinom` masses used by the existing
tests; the cumulative discrete mass need not equal an unattainable exact
probability.)

Unit invariance test: mean and sd scaled by 1000 and by 0.001 give the same
shaded proportion (within 1e-4), the same printed probability text, and a dark
polygon whose standardized coordinates agree within 1e-6.

## Changed snapshots (32)

All changes follow from the density display scale, the relative annotation
heights, the paired central captions, or the off-view disclosure caption. No
snapshot was accepted to hide a failing value assertion. The seven binomial
snapshots are unchanged. Caption text is unchanged except where noted; the
extra text element in the upper-tail plots is the disclosure caption.

| Snapshot | Why |
|---|---|
| `chisquare-plot-1`, `chisquare-plot-1a`, `chisquare-plot-2`, `chisquare-plot-3` | density axis capped/scaled to the curve; no 5% axis expansion; axis tick geometry |
| `chisquare-perc-lower`, `chisquare-prob-lower` | density axis scaled; axis geometry |
| `chisquare-perc-upper`, `chisquare-prob-upper` | density axis scaled; disclosure caption added |
| `f-plot-1`, `f-plot-2` | density axis scaled to the curve; axis geometry |
| `f-perc-lower`, `f-perc-upper`, `f-prob-lower`, `f-prob-upper` | density axis scaled; upper plots gain the disclosure caption |
| `normal-plot-1`, `normal-plot-2` | density axis scaled to the peak and no axis expansion |
| `normal-perc-lower`, `normal-perc-upper`, `normal-prob-lower`, `normal-prob-upper` | density axis scaled; disclosure caption added |
| `normal-perc-both`, `normal-prob-both` | density axis scaled; paired central caption computed from the printed endpoints |
| `t-plot-1`, `t-plot-2`, `t-plot-3` | density axis scaled; no axis expansion |
| `t-perc-lower`, `t-perc-upper`, `t-perc-both`, `t-prob-lower`, `t-prob-upper`, `t-prob-interval`, `t-prob-both` | density axis scaled; upper/tail plots gain the disclosure caption |

## Commands run (that changed something)

```text
Rscript .vfx5-tmp/probe*.R, m2.R, df*.R, png.R    # audit reproductions and after evidence
Rscript -e 'devtools::document()'                  # kept only vdist_normal_plot.Rd and vdist_f_plot.Rd
git archive HEAD | tar -x -C .vfx5-tmp/check-src
R CMD build --no-manual .
R CMD check --as-cran --no-manual vistributions_0.2.2.tar.gz
```

All heavy R commands used `bash ~/.claude/bin/heavy-slot.sh pkg-vfx5 …`;
build/check used `LC_ALL=C LANG=C LANGUAGE=en`.

## Test / check output

Source tree, full suite including all snapshots:

```text
NOT_CRAN=true devtools::test()
[ FAIL 0 | WARN 0 | SKIP 0 | PASS 746 ]
```

Clean `git archive` from the final code commit (`6b61940`):

```text
R CMD build --no-manual .                    exit 0 (vignette built)
R CMD check --as-cran --no-manual …          exit 0
* checking examples ... [12s] OK
* checking tests ...
  Running 'testthat.R' [51s] OK
  [ FAIL 0 | WARN 0 | SKIP 15 | PASS 707 ]
* checking re-building of vignette outputs ... OK
Status: 1 NOTE
```

The sole NOTE is the expected maintainer-address block:

```text
* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Aravind Hebbali <aravindhebbali@rsquaredacademy.com>'
New maintainer:  Aravind Hebbali <aravindhebbali@rsquaredacademy.com>
Old maintainer(s): Aravind Hebbali <hebbali.aravind@gmail.com>
```

The check's 15 skips are the `skip_on_cran()` visual blocks; the separate
NOT_CRAN run executed them all.

## Follow-ups (written out in full)

1. **Chi-square with `df < 1`** — `vdist_chisquare_perc(.95, .1, "upper")`
   returns a correct area and a visible shaded plateau, but the density body is
   a sub-pixel support singularity, so the shape is not informative. Consider a
   support-edge inset or a documented display floor for these parameter values.
2. **F with a very small denominator df** — `vdist_f_perc(.95, 2, .1)` shades
   the correct 95%, but the density body is compressed against one edge because
   the event window necessarily reaches `~5e24`. The same inset/window-cap
   treatment as (1) would make it readable; the numerical result is correct.
3. **t with `df < 1`** — `vdist_t_perc(.95, .1)` is drawn on a symmetric log
   axis and its shaded region is visible, but the density is still a spike at
   zero. A dedicated robust-scale display would be a design change.
4. **pkgdown website** — `docs/` still shows the older site; rebuild with
   `pkgdown::build_site()` and commit separately.
5. **Shiny app** — `vdist_launch_app()` delegates to
   `xplorerr::app_vistributions()`; the live app was not run in this round.
6. **`scales` in Imports** — used for the symmetric-log axis. `scales` is
   already a hard dependency of ggplot2, so nothing new is installed for users;
   if a strict “no new Imports” rule applies, replace `scales::pseudo_log_trans`
   with a local transform in a future round.
7. **Scratch** — `.vfx5-tmp/` contains this round's scripts, logs, PNGs and the
   archive check products; it is untracked and excluded from `git archive`.

## Deferred

None of the five findings is deferred. The follow-ups above are readability or
infrastructure items, not known wrong results.
