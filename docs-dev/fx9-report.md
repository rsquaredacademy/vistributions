# Round 9 (vfx9): make the suite pass on macOS arm64 without loosening a check

Worktree `_worktrees/vist-fx6`, branch `task/vist-fx6`, base `7061fd6`
(PR vistributions#70). macOS-latest (arm64, R 4.6.2) failed 12 tests /
1,254 passed there; Linux, Windows and coverage were green.

| Commit | Subject |
|---|---|
| `test: measure pixels and snapshots the same way on every platform (vfx9)` | groups A, B and C |
| `docs: round-9 report (vfx9)` | this file, plus `ragg` in `Suggests` |

The three failure groups in the brief are confirmed, with one correction to the
count: the 12 failures are group A **8** (`test-vfx7-findings.R:115` four
times, `:130` three times, `:155` once), group B 1 and group C 3.

## A. `dark_pixels(p) > 0` returned 0 on macOS (8 failures)

**Cause confirmed.** `dark_pixels()` rendered with `grDevices::png()` and
counted pixels within 0.02 of exactly `rgb(0, 0, 205)`. On macOS `png()`
defaults to `type = "quartz"`, which colour manages and antialiases, so the
fill never lands inside that box. The tell in the log is that `:130` failed too:
that test's regions are ordinary, 40,000-pixel areas, so a 0 count cannot mean
"invisible", only "not that exact colour".

A second, silent defect in the same helper: the panel's percentage annotations
and the *axis titles* (`Mean = 0.1  Std Dev. = 0.4472`) are also drawn in
`#0000CD`, and the density curve is drawn in `#0000CD`/`#0000FF`. With an exact
match those contributed about 2 pixels, which is why the round-7 "before" count
was 2 rather than 0, and with a hue-based match they contribute 33-3,000. A
`> 0` assertion would then pass for a plot whose region was not drawn at all.

**Fix** (`tests/testthat/test-vfx7-findings.R:28`):

* render with `grDevices::png(type = "cairo")` when `capabilities("cairo")`,
  else `ragg::agg_png()` (now in `Suggests`), else `skip()`. Cairo rasterises
  the same colours on every platform; the test then measures the same thing
  everywhere instead of reporting an invisible region;
* classify a fill pixel by hue dominance rather than an exact box: blue above
  red and green, red equal to green (a `#0000CD` blend into white keeps
  `r == g`, the light fills `#6495ED`/`#4682B4` have `g > r`), and at least half
  coverage (`#0000CD` over white at coverage `a` is `(1-a, 1-a, 1-0.196a)`, so
  `b - max(r, g) = 0.8a`; requiring `> 0.40` is `a > 0.5`);
* draw only the dark-filled polygons/rectangles — the layers the test is about —
  and drop the titles, so the count can only come from shading. The panel limits
  are set by `coord_cartesian()`, so removing layers does not move the pixels
  that remain.

**Proof it still catches an invisible region** (900 x 500, cairo, Windows;
`.vfx9-tmp/proof_a.R`):

| Case | dark fill pixels |
|---|---:|
| `vdist_normal_perc(.95)` | 92,901 |
| `vdist_t_perc(.95, 5)` | 77,923 |
| `vdist_f_perc(.95, 5, 10)` | 107,760 |
| `vdist_chisquare_perc(.95, .1)` | 43,549 |
| `vdist_chisquare_perc(.95, .1, "upper")` | 43,870 |
| `vdist_normal_perc(.95, sd = 2e307)` | 92,900 |
| 4 sub-pixel cases **with** the marker strip | 925, 925, 925, 925 |
| same 4 cases, **VFXX6-M2 fix broken** (marker layer removed) | 0, 0, 0, 0 |
| ordinary region drawn with `fill = "transparent"` | 0 |

So with the sub-pixel fix broken every one of the four cases measures 0, and the
assertion at `:115` was raised from `> 0` to `> 100` (the strip is ~3.6 device
pixels wide over most of the data height and paints ~925 pixels; the region
alone paints 0). This is stricter than before, not looser. The ordinary cases
keep `> 0`, which now really means "the dark fill is drawn".

## B. `VFXX2-H3` pnorm tolerance was finer than double arithmetic (1 failure)

**Cause confirmed.** The caption check of the exact string passed on macOS, so
the cutoff was right. Near 1 a double has spacing `eps * |x| = 2.22e-16`, which
is `2.22e-3` standard deviations at `sd = 1e-13`; `pnorm()` therefore moves in
steps of `dnorm(1.645)/1e-13 * 2.22e-16 = 2.29e-4`. Measured here:

```text
qnorm(.95, 1, 1e-13) = 1.0000000000001645
pnorm(q,                 1, 1e-13) = 0.95005122670075315   (Windows)
pnorm(q - eps,           1, 1e-13) = 0.94982198839170351   (macOS: 0.94982)
pnorm(q + eps,           1, 1e-13) = 0.9502796290361496
```

0.94982 is exactly the value one representation step below the requested
cutoff: the macOS `pnorm()` differs by one ulp of x, not by a wrong result, and
no tolerance below 2.3e-4 can see that difference.

**Fix** (`tests/testthat/test-vfx3-findings.R:79`). The cutoff is printed with
17 significant digits, so the real requirement is that it is the double nearest
the exact quantile. The test now checks, with the derivation in a comment:

```r
xres <- 4 * .Machine$double.eps * abs(cutoff)   # 8.9e-16, ~4 steps at 1.0
pres <- dnorm(cutoff, 1, 1e-13) * xres          # 9.2e-4, the same in probability
expect_lte(abs(cutoff - qnorm(.95, 1, 1e-13)), xres)
expect_lte(abs(pnorm(cutoff, 1, 1e-13) - .95), pres + 1e-12)
```

**What it still catches.** The original bug was the cutoff collapsing to 1
(`|cutoff - 1| = 1.65e-13` = 745 steps, so the new `xres` check fails) or losing
printed digits (`expect_equal(subtitle(p), "P(X < 1.0000000000001645) = 95%")`
still asserts every printed digit, and a value one step off is 5.5e-14 > xres).
The pnorm tolerance is now tied to the arithmetic's own spacing, so a cutoff
that was, say, 1e-13 too far out (≈ 1e-3 in probability) is still caught. The
remaining checks (`cutoff != 1`, the reference line equals the exact quantile)
are unchanged.

## C. Three vdiffr sketches with one refinement vertex fewer on macOS

**Cause confirmed.** In each failing sketch one dark polygon has exactly one
vertex fewer and every other SVG element is byte-identical (`diff` of the token
streams: one line per file, the missing vertex; neighbours 1-3 px away, so the
vertices are not duplicates). The polygon comes from `vdist_region()`
(`R/vdist-utils.R:315`), not from `vdist_region_grid()`: `vdist_region_grid()`
only lays out the initial grid. The refinement loop picks the `k` intervals with
the largest trapezoid error, where `thr` is itself one of those errors:

```r
thr <- quantile(ae, 1 - k / length(x), type = 1)
idx <- which(ae >= thr & ae > 0)
```

`type = 1` returns an existing `ae` value, so the boundary is a comparison
between two numbers that can be equal. Instrumented on Windows, the disputed
vertex in each case is the last interval admitted at one pass:

| sketch | region | pass | k | selected | disputed interval |
|---|---|---:|---:|---:|---|
| `normal plot 1` | `[-1, 1]`, N(0,1) | 3 | 5 | **6** | `ae == thr` exactly; the 5th and 6th errors are a mirrored pair that ties bit-for-bit here |
| `t perc both` | `[-1.0944, 1.0944]`, t(7) | 4 | 5 | 5 | `ae[6] / ae[5] = 0.9999999999` |
| `f prob upper` | `[1.5222, 18.1674]`, F(9,35) | 25 | 9 | 9 | disputed interval is **rank 9 of k = 9**, `ae == thr`, next candidate 0.9 % below |

On macOS the same comparison rounds the other way: the tie is not exact there,
so one interval drops out of the selected set and its midpoint is never drawn.
The error itself is a difference of two nearly equal masses (trapezoid mass
minus the CDF difference), so a few ulps in `p*()`/`q*()` are amplified — about
1e-11 relative where the loop refines — which is why the mirrored pair that is
bit-identical on one platform is not on another.

**Route taken: per-OS sketches for Darwin** (the fallback in the brief), with
the macOS SVGs from the artifact folder. This is the route I took after trying
the deterministic one and rejecting it for a concrete reason:

1. *Inclusive tie band* (`ae >= thr * (1 - 1e-6)`): selects every tied interval
   on every platform. It changes seven Windows sketches, and one of them
   (`t plot 3`, t(8)) grows from 155 to 396 vertices over 33 passes because one
   extra interval at pass 4 sends the trajectory into a thrash that ends at
   `|area - target| = 1.0e-5` instead of the original 5-pass convergence. The
   number of refined intervals is data-dependent, so the trajectory is chaotic;
   a band fixes one knife edge and creates others. Without a macOS machine I
   cannot verify that the regenerated sketches are what macOS then produces.
2. *Tie band plus a fixed cardinality* (`k` intervals, ties ranked on a
   7-significant-digit key and broken by position): no thrash (only 6 sketches
   change, by one removed or mirrored vertex each), and it does make
   `normal plot 1` and `t perc both` agree across platforms. But
   `f prob upper` is unchanged on Windows, and its disputed interval is rank
   `k` of `k` with the next candidate 0.9 % lower — a mechanism that moves a
   value by ~1 %, not by last-bit noise, and which I could not identify from the
   artifact. A band cannot be *proven* to fix that case, so adopting it would
   risk another red macOS run on a submission-blocking check.

Both attempts changed the Linux/Windows sketches for six plots elsewhere, i.e.
they changed the recorded pictures to fix three of them. The brief allows the
per-OS fallback exactly in that situation. Shape equivalence is shown below, so
the fallback records the same drawing twice, not a different drawing.

**Implementation.** `tests/testthat/test-visual.R`:

```r
mac_variant <- function() {
  if (identical(Sys.info()[["sysname"]], "Darwin")) "darwin" else NULL
}
vdiffr::expect_doppelganger("f prob upper", p19, variant = mac_variant())
```

vdiffr passes the variant to `expect_snapshot_file()`, so the macOS sketches
live in `tests/testthat/_snaps/darwin/visual/` (three files, copied from
`vist-mac-snap/macOS-ARM64-rrelease-0-results/.../visual/*.new.svg`) and every
other platform keeps comparing `_snaps/visual/*.svg`. Both directions were
checked on Windows: with `mac_variant()` forced to `"darwin"` the three
sketches fail against the Windows render (so the macOS files really are
compared, not silently accepted), and with the real helper all 39 sketches
pass. The macOS files survive a Windows/Linux run — testthat prunes snapshots a
run did not use, and the variant directory is kept because the same sketch name
is used inside it; the full suite was re-run twice afterwards and the three
files were still there.

**Proof the two drawings are the same shape** (`.vfx9-tmp/shape.py`: maximum
distance from every vertex of one polygon to the other's polyline):

| sketch | vertices (Windows -> macOS) | macOS vertices to Windows polyline | Windows vertices to macOS polyline |
|---|---|---:|---:|
| `f prob upper` | 291 -> 290 | 0.0000 px | 0.0002 px |
| `normal plot 1` | 143 -> 142 | 0.0000 px | 0.0183 px |
| `t perc both` | 168 -> 167 | 0.0000 px | 0.0046 px |

Every macOS vertex lies on the Windows polyline; the only Windows vertex off it
is the extra refinement point, 0.018 px from the chord it subdivides. A token
diff of each pair reports exactly one changed token (the extra vertex) and no
other element differs, so lines, axes, fills, text and styles are identical.

## Test counts (Windows, R 4.6.1, `test_dir(load_package = "source")`)

Before (fx8, on this base): 0 failed / 0 errors / 0 skipped / **1,266 passed**.
macOS CI at `7061fd6`: 12 failed / 1,254 passed.

After:

```text
[ FAIL 0 | WARN 0 | SKIP 0 | PASS 1267 ]
 failed  errors skipped  passed
      0       0       0    1267
```

`errors` and `skipped` are 0, not just `failed`. The +1 expectation is group B,
where one `expect_equal` became two representation-aware assertions. The 39
vdiffr sketches (36 unchanged, 3 macOS variants) all pass on Windows.

## `R CMD check`

`git archive HEAD` tarball, `R CMD check --as-cran --no-manual`:

```text
PLACEHOLDER_CHECK_OUTPUT
```

## Follow-ups

1. **Root cause of the refinement ties: compute the interval mass without
   cancellation.** `err` is `trapezoid - (pfun(x2) - pfun(x1))`, and in an upper
   tail `pfun` is near 1, so the difference of two CDF values loses about 14
   digits and the error inherits ~1e-11 relative platform noise. Pass a
   survival-CDF to `vdist_region()` (`pfun(x, lower.tail = FALSE)`) or evaluate
   the mass as `-expm1(log(p2) - log(p1)) * p1`, so the refinement decision is a
   well-conditioned function of the grid. Once that lands, the three
   `_snaps/darwin` sketches should be deleted and the `variant = mac_variant()`
   arguments removed.
2. **Then re-test the deterministic selection rule.** With the mass computed
   stably, the `signif(12)` quantisation added in vfx8 should be enough, and the
   tie at rank `k` should be bit-identical on every platform. Verify with a
   perturbation harness that moves `pfun()`/`qfun()` by ±2 ulps per input (the
   sketch of one is in `.vfx9-tmp/stability.R`), not by re-running CI.
3. **`f prob upper` still needs an explanation.** Its disputed interval is rank
   `k` of `k` with the next candidate 0.9 % below, which last-bit noise cannot
   explain; either `pf()` saturates earlier on macOS (making another interval's
   "error" the whole trapezoid mass) or the loop's area threshold is crossed one
   pass earlier there. Logging `ae`, `thr` and the pass count on a macOS runner
   once would settle it and might remove the third variant.
4. **Panel-only pixel measurement.** `dark_pixels()` currently enumerates the
   blue ink that is not the region (annotation text, axis titles, curve). A crop
   to the panel (or measuring the polygon grob's own viewport) would stop future
   plots from needing that list to be kept in sync; the current version is
   correct but knows too much about each plot's layers.
5. **Exercise the non-cairo fallback in CI.** `ragg` is only used when
   `capabilities("cairo")` is FALSE; no CI job covers that branch, so its first
   real test would be on a user's machine.
