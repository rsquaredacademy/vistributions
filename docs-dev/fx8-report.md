# pkg-vist-fx8 — one polygon vertex, one platform (vistributions)

**Task.** PR vistributions#70 → `develop` is red on Linux/macOS only: one vdiffr
snapshot, `tests/testthat/test-visual.R:172` "t prob interval"
(`vdist_t_prob(1.445, 7, 'interval')`). Everything else is identical. An earlier
attempt (commit `a6269b1`, reverted in `832f746`) snapped grid points within
`1e-12` of `lo`/`hi` onto the endpoints and made **6 tests error**.

**Result.** The near-duplicate is removed after the area refinement, using a
tolerance relative to the coordinates (64 ulps) rather than to the width of the
region. The drawn shape loses only the duplicate vertex: 13 snapshots change and
each change is a pure deletion of 1–4 duplicate vertices (all remaining
coordinates and all other SVG text identical, machine-checked). The failing
snapshot `t-prob-interval.svg` is **not** changed — the fix makes Linux/macOS
produce the file Windows already produces. Full suite: 122 tests, **0 failed, 0
errors**; `R CMD check --as-cran --no-manual`: **0 errors, 0 warnings**, 1
maintainer NOTE.

Worktree `vist-fx6`, branch `task/vist-fx6`, base `832f746` (tree == `5ff06db`).

---

## 1. What the CI saw

From the run-43860269110 artifact (`vist-snap/Linux-X64-rrelease-3-results/...`,
also `rdevel` and `roldrel`):

* `t-prob-interval.new.svg` and the committed `t-prob-interval.svg` have the
  same 90 lines; **one** line differs (line 51, the dark `geom_polygon`):
  152 points → 153 points.
* The only difference is one inserted token at index 1:
  `265.66,407.84`, textually identical to the vertex next to it.
* `macOS-ARM64` produced the same 153-point polygon for the same snapshot (its
  other failures are device/font differences: different line counts, e.g.
  `normal-plot-1` 83 vs 90 lines, and `dark_pixels()` = 0 on its png device).
* Windows produced 152 points, which is why the committed snapshot passes
  there.

So the polygon has one extra vertex on Linux/macOS, at the **start of the shaded
interval** (`x = lo`, then a second vertex one ulp to the right with the same
density to 2 decimals).

## 2. Root cause

`vdist_region_grid()` (`R/vdist-utils.R`) builds the probability-spaced points as

```r
xp <- qfun(plogis(seq(qlogis(plo), qlogis(phi), length.out = n)))
```

`qlogis()` → `plogis()` → `q*()` is not guaranteed to round-trip: the first
(last) value can land **one ulp inside** `lo` (`hi`) on one platform and exactly
on it on another. `x <- sort(unique(c(lo, xp, xu, hi)))` therefore keeps a
near-duplicate on one platform only. The pair is not dropped by refinement
either: the interval between them has zero width and zero error, so it is never
chosen for bisection. The extra vertex is invisible on the device (same pixel)
but it is an extra token in the SVG, and vdiffr compares text.

## 3. Why the reverted attempt broke six tests

`a6269b1` used

```r
eps <- 1e-12 * max(abs(hi - lo), .Machine$double.eps)
x[abs(x - lo) <= eps] <- lo
x[abs(x - hi) <= eps] <- hi
```

The epsilon is proportional to the **width of the region**. For a t with
`df = 0.1` the 95% interval is astronomically wide:

```
lo = -1.6044257056665724e+96
hi =  1.6429319226026354e+09
width = 1.6044257056665724e+96      eps = 1e-12 * width = 1.6e84
```

Every grid point within `1.6e84` of an endpoint was snapped onto it and then
removed by `unique()`: the grid collapsed from **3165 to 408 points**, the
polygon area became **4.0533063e+13** instead of 0.95, and
`vdist_check_region()` stopped with

```
The t shaded region area (4.0533063e+13) does not match the probability of its
event (0.95) within 1e-3; the density is too concentrated for the documented
plotting contract.
```

All six "errors" are that same call (`vdist_t_perc(.95, .1)`), reached from six
different tests (reproduced locally by installing the reverted grid function into
the namespace, `.vfx8-tmp/repro_old_counts.R`):

```
1. VFXX3-M1: ordinary documented inputs are plotted, with their area      (test-vfx4-findings.R:131)
2. VFXX4-H2: the shaded region is visible in the rendered panel            (test-vfx5-findings.R:131)
3. every must-pass ordinary input keeps its event area and visible shading (test-vfx5-findings.R:271)
4. area and caption use the exact distribution functions                   (test-vfx6-redesign.R:225)
5. shading is visible in the built panel for the must-pass inputs          (test-vfx6-redesign.R:262)
6. the grids stay bounded                                                  (test-vfx6-redesign.R:514)
```

Per-test counts (base = `832f746`, attempt = `a6269b1`'s grid function,
after = this fix; `p/f/e` = passed/failed/error):

| test | base | attempt | after |
|---|---|---|---|
| VFXX3-M1: ordinary documented inputs are plotted, with their area | 6/0/0 | 5/0/**1** | 6/0/0 |
| VFXX4-H2: the shaded region is visible in the rendered panel | 14/0/0 | 4/0/**1** | 14/0/0 |
| every must-pass ordinary input keeps its event area and visible shading | 66/0/0 | 45/0/**1** | 66/0/0 |
| area and caption use the exact distribution functions | 10/0/0 | 4/0/**1** | 10/0/0 |
| shading is visible in the built panel for the must-pass inputs | 103/0/0 | 68/0/**1** | 103/0/0 |
| the grids stay bounded | 14/0/0 | 10/0/**1** | 14/0/0 |

These are **errors**, not failures: `testthat`'s silent reporter reports
`failed = 0` for them, which is why the damage was easy to miss. Counted with

```r
r <- as.data.frame(testthat::test_dir(...)); sum(r$failed); sum(r$error)
```

the attempt gives `0 failed, 6 errors`; the `p` columns drop because the test
aborts at the offending `expect_*`.

## 4. The fix

`R/vdist-utils.R`, end of `vdist_region()` — **after** the refinement loop, on
the grid that is handed to the polygon:

```r
if (length(x) >= 3) {
  tol  <- 64 * .Machine$double.eps
  same <- diff(x) <= tol * pmax(abs(x[-1]), abs(x[-length(x)]))
  if (any(same)) {
    drop <- c(FALSE, same)            # drop the later point of a pair
    if (same[length(same)]) {         # ... unless the later one is zhi
      drop[length(drop)] <- FALSE
      drop[length(drop) - 1L] <- TRUE
    }
    x <- x[!drop]
    y <- y[!drop]
  }
}
```

Three design points, each one earned by a failure mode:

1. **Relative to the coordinates, never to the region width.** 64 ulps of
   `x` itself. A width-relative epsilon is what collapsed the `1e96`-wide t
   region. The endpoint of the region always survives, so the drawn polygon
   still closes on `lo`/`hi`.
2. **After the refinement.** Snapping in `vdist_region_grid()` (before `unique()`)
   fixes the grid, but it also changes the refinement path: the interval count
   `n` enters `k <- ceiling(frac * length(x))` and the error quantile, so a
   different set of intervals is bisected downstream. That version removed a
   legitimate vertex from 12 snapshots (e.g. `f-perc-lower` lost
   `59.54,161.48` between unchanged neighbours 2.7 px away) — the shape changed,
   not just the duplicate. Dropping after refinement leaves every other vertex,
   and therefore the whole refinement trajectory, untouched.
3. **The duplicate, not the endpoint, is dropped.** For the pair
   `(zlo, zlo + 1 ulp)` the first point (the exact endpoint) is kept; for
   `(zhi - 1 ulp, zhi)` the last point is kept. The polygon's two closing
   vertices on the x axis (`y = 0` at `lo` and at `hi`) are added *after* this,
   so the y=0 closure vertex and the first density vertex still share an x
   coordinate — that pair is intentional, not a duplicate.

Nothing else changed: `vdist_region_grid()` is back to its `832f746` text, the
area check still uses the pre-drop polygon (the removed sliver has width ≤ 64
ulps and carries no measurable area), and `n` reports the drawn vertex count.

## 5. Tests

Full `test_dir` with `NOT_CRAN=true` (`Rscript .vfx8-tmp/run_tests.R`):

| | files | tests | passed | failed | errors | skipped |
|---|---|---|---|---|---|---|
| before (`832f746`) | 15 | 120 | 1238 | 0 | 0 | 0 |
| after (this commit) | 16 | 122 | 1266 | 0 | **0** | 0 |

New regression file `tests/testthat/test-vfx8-findings.R` (28 expectations):

* **VFXX8-R1** — the drawn vertex list of four boundary cases has no consecutive
  points closer than `1e-9 × x range` *and* is strictly increasing (so an exact
  repeated coordinate fails too). Cases:
  `vdist_t_prob(1.445, 7, "interval")` (the CI case, measured ratio
  `1.08e-4`), `vdist_normal_prob(3)` (`1.11e-4`),
  `vdist_f_prob(1, 5, 10)` (`1.70e-6`), `vdist_normal_perc(.95)` (`9.08e-5`).
  Before the fix the CI case had a `~1 ulp` gap, ratio `7e-17` — the assertion
  catches exactly the reported bug.
* **VFXX8-R2** — the `t(df = 0.1)` 95% region (about `1e96` wide) still carries
  more than 1000 vertices per polygon, every gap is positive, and its area is
  still `0.95 ± 1e-3`. This is the guard against a width-relative epsilon.

The four R1 cases are deliberately chosen where the *refinement* keeps every
interval wider than `1e-9 × range`; near a density peak legitimate refinement
points can be far closer (e.g. `vdist_t_prob(1.6, 7, "interval")` has a
midpoint at `-2.9e-16` next to `0`, and `vdist_chisquare_perc(.95, 1)` reaches
`4e-25 × range`), and those are not duplicates — a test that demanded `1e-9`
there would be asserting a different property.

## 6. Changed snapshots (13, all vertex-only)

The 13 files below changed when the duplicate is dropped on Windows too (Windows
had the same duplicate in these regions; it just happened to have no duplicate
in `t-prob-interval`). Each was regenerated by running the suite and accepting
the `.new.svg` output, then verified mechanically with
`.vfx8-tmp/verify_svg.py 832f746`: the old and new SVG are compared line by
line; a changed line must have identical text outside the `points` attribute, the
new point list must be the old one with tokens deleted, and every deleted token
must be textually identical to a token next to it (same drawn coordinate at the
snapshot's 2-decimal precision).

```
OK   f-perc-lower:      1 element changed,  1 duplicate vertex lost
OK   f-plot-1:          1 element changed,  1 duplicate vertex lost
OK   f-prob-upper:      1 element changed,  1 duplicate vertex lost
OK   normal-perc-both:  1 element changed,  2 duplicate vertices lost
OK   normal-plot-1:     2 elements changed, 3 duplicate vertices lost
OK   normal-plot-2:     3 elements changed, 4 duplicate vertices lost
OK   normal-prob-both:  1 element changed,  2 duplicate vertices lost
OK   t-perc-lower:      1 element changed,  1 duplicate vertex lost
OK   t-plot-1:          1 element changed,  1 duplicate vertex lost
OK   t-plot-2:          1 element changed,  2 duplicate vertices lost
OK   t-prob-both:       1 element changed,  2 duplicate vertices lost
OK   t-prob-lower:      1 element changed,  1 duplicate vertex lost
OK   t-prob-upper:      1 element changed,  1 duplicate vertex lost

13 snapshots, 22 duplicate vertices lost, all changes are vertex-only
```

Example (the largest, `t-plot-2.svg`, 412 → 410 points in one polygon): the
polygon loses `18.07,540.98` and `714.52,540.98`; each is identical to the
vertex next to it (the first/last density vertex at the window edge), and all
remaining 410 points are byte-identical. The binomial snapshots are untouched (no continuous region).

`t-prob-interval.svg` is **not** in the list: Windows already drew 152 points
there, and after the fix every platform draws those same 152 points.

## 7. The Linux rounding, simulated locally

`.vfx8-tmp/perturb.R` reproduces the platform difference in the failing region
(`lo = -1.445`, `hi = 1.445`, `pt`/`qt`, `df = 7`) by nudging
`qfun(plo)` two ulps inward:

```
grid points, exact round trip: 127
side: low
  grid points, nudged round trip: 128   (nearest pair gap: 6.66e-16)
  x[1:3] exact : -1.4450000000000001 -1.4001560461899651 -1.3998437500000001
  x[1:3] nudged: -1.4450000000000001 -1.4449999999999994 -1.4001560461899651
  vertices exact/nudged: 152 / 152   identical x: TRUE  identical y: TRUE
  area: 0.80830092 0.80830092
  min gap / range of nudged vertices: 1.081e-04
side: high
  grid points, nudged round trip: 128   (nearest pair gap: 4.44e-16)
  vertices exact/nudged: 152 / 152   identical x: TRUE  identical y: TRUE
  area: 0.80830092 0.80830092
```

So a one-ulp platform difference in the grid produces an identical polygon, and
the drawn vertex list no longer depends on it. (Before the fix the nudged grid
contributes one extra entry to `c(zlo, x, zhi)`, i.e. the 153-point polygon the
CI saw.)

## 8. `R CMD check --as-cran --no-manual`

Clean `git archive` of the commit, `R CMD build`, then check of the tarball with
`NOT_CRAN=true`:

```
* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Aravind Hebbali <aravindhebbali@rsquaredacademy.com>'
New maintainer: ... Old maintainer(s): ...
* checking tests ...
  Running 'testthat.R' [76s]
 [76s] OK
* checking package vignettes ... OK
* checking re-building of vignette outputs ... OK
* DONE
Status: 1 NOTE
```

with `vistributions.Rcheck/tests/testthat.Rout`:

```
[ FAIL 0 | WARN 0 | SKIP 0 | PASS 1266 ]
```

One NOTE, the CRAN incoming-feasibility/maintainer note. (Checking the plain
*source directory* instead of a tarball fails in this R 4.6.1 with
`Required fields missing or empty: 'Author' 'Maintainer'` — the `Authors@R` field
is only expanded by `R CMD build`; CI also builds a tarball first, so the
tarball is the meaningful check. A first build was made with
`--no-build-vignettes` and reported two vignette WARNINGs; the reported check is
the full build.)

## 9. Files and commits

* `R/vdist-utils.R` — duplicate-vertex drop at the end of `vdist_region()`.
* `tests/testthat/test-vfx8-findings.R` — new regression test (VFXX8-R1/R2).
* `tests/testthat/_snaps/visual/*.svg` — 13 snapshots, duplicate vertices only.
* `NEWS.md` — bug-fix entry.
* `.gitignore` — `.vfx8-tmp/` (scratch; not committed).
* `6c1f00c` — fix + snapshots + test (vfx8).
* this report (vfx8).

Scratch (not committed): `.vfx8-tmp/` holds the test runner, the reproduction of
the reverted attempt, the vertex-gap scan, the Linux-rounding simulation, the
SVG proof script and the check logs.
