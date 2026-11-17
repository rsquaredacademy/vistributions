# vistributions round 7 (pkg-vist-fx7): remaining audit findings + bounded grids

Date: 2026-11-16 IST.
Worktree: `F:/R/Packages/_worktrees/vist-fx6` (branch `task/vist-fx6`, round 6 head `0472be7`).
Spec: `docs-dev/pkg-vist-rx6-audit.md` (Gemini re-audit of round 6; committed with this round).
Earlier chain: `F:/R/Packages/_worktrees/vist-fix-022/docs-dev/vfxx-reaudit.md` … `vfxx5-reaudit.md`.
Common rules: `F:/pm/notes/2026-11-01-pkg-audits/common-pkgfix.md`.

Commits (`(vfx7)`):

* `75b8acf` fix: near-zero normal means, sub-pixel regions, extreme scales and bounded grids (VFXX6-M1/M2/L1/L2)
* the report commit that adds this file.

Fix scope is narrow: no redesign, no API change, no new hard dependency. `png` was added to
`Suggests` only because the new device test reads the rendered PNG; it is a recommended package.

---

## Finding table

| ID | Severity | Fix (file:line at HEAD) | Test added | Verified how | Status |
|---|---|---|---|---|---|
| **VFXX6-M1** | Medium | `R/vdist-utils.R:404-427` — `vdist_check_normal_resolution()` compares `log(sd) - log(|mean|) < log(1e-14)` instead of `sd / abs(mean)`, so a ratio that overflows to `Inf` (near-zero mean) is not a failure; the rejection message reports the true ratio, including `0 (underflow)`. | `tests/testthat/test-vfx7-findings.R:51` "a near-zero normal mean is drawn, not rejected (VFXX6-M1)"; `:81` "a genuinely unresolvable normal is still rejected (VFXX6-M1)" | `mean = 1e-308, sd = 10` now draws all three plots; the printed caption is true (`pnorm(16.449, 1e-308, 10) = 0.95`); ordinary rejections still stop with their own message | **FIXED** |
| **VFXX6-M2** | Medium | `R/vdist-utils.R:876-947` — `vdist_subpixel_region()` computes the dark width inside the view (as a fraction of the view span, 4e-3 threshold) and `vdist_subpixel_layer()` / `vdist_caption_text()` draw a 4e-3-span dark strip and append a caption note; wired through `R/vdist-data-prep.R:306` and the 8 continuous builders in `R/vdist-plots.R`. The event polygon, its area check and the printed percentage are untouched. | `tests/testthat/test-vfx7-findings.R:100` "a sub-pixel chi-square lower tail is visible on the device (VFXX6-M2)"; `:119` "an ordinary shaded region is not marked as sub-pixel (VFXX6-M2)" | Real 900x500 PNG: dark (#0000CD) pixels 2 → **411** for `vdist_chisquare_perc(.05,.1)`, `.50,.1`, `.05,.2` and `vdist_chisquare_prob(qchisq(.05,.1),.1)`; ordinary normal/t/F/chi-square cases stay unmarked and keep their pixels | **FIXED** |
| **VFXX6-L1** | Low | `tests/testthat/Rplots.pdf` removed from the index (`git rm --cached`), ignored in `.gitignore`; `tests/testthat/helper-device.R` opens a pdf device writing to `tempdir()` before the suite, so a test that prints without its own device cannot write `Rplots.pdf` into the source tree. | (checked by the suite itself; no dedicated test — a test cannot observe git) | `git ls-files tests/testthat/Rplots.pdf` is empty; after a full `devtools::test()` the file does not exist | **FIXED** |
| **VFXX6-L2** | Low | `R/vdist-utils.R:431-461` — `vdist_check_normal_range()` also rejects a window whose width `8 * sd` overflows (sd above about 2.2e307), and `R/vdist-data-prep.R:234-241` rejects any view wider than the double range. `R/vdist-utils.R:217-233` — `vdist_area()` skips the zero-height closing edge of a polygon whose ends lie on the axis, which is where the `NaN` came from. | `tests/testthat/test-vfx7-findings.R:137` "an extreme normal scale is rejected truthfully, not with a NaN area (VFXX6-L2)" | `sd = 3e307`: NaN area error → true message (below); `sd = 2e307` draws with 84,466 dark pixels and area 0.94999; `vdist_area()` finite for a polygon spanning 2.17e308 | **FIXED** (rejected with a true message, as the brief allows) |
| **grid blow-up (High, 2026-11-16)** | High | Continuous grids were already fixed-count in round 6 (`vdist_grid()` / `vdist_region()` / `vdist_density_cap()`); this round bounds the last unbounded grid, the binomial count support: `R/vdist-data-prep.R:82-108` (`vdist_count_grid()`, `vdist_count_prob()`), `R/vdist-plots.R:35-53` (`vdist_col_layer()`, `vdist_count_breaks()`), and stops a count axis materialising one break per count. | `tests/testthat/test-vfx7-findings.R:168` "every grid is bounded and fast at large scale"; `:207` "a grouped binomial grid still carries the exact total mass" | Every case builds, prints to a real png and has ≤ 2002 rows in every built layer, ≤ 0.60 s per case; the 0.2.1 grid for the audit reprex is 200,000,001 points (1,525.9 MB per copy) | **FIXED** |
| **NEWS / help** | — | `NEWS.md:5-8` (four bullets), `R/vdist-normal.R:11-27` and `:42-48`, `R/vdist-chisquare.R:18-31`, `R/vdist-t.R:17-31`, `R/vdist-f.R:8-28`, `R/vdist-binomial.R:15-24`; `man/*.Rd` regenerated. | `tests/testthat/test-vfx7-findings.R:224` "the documented guarantees match the fixed behaviour" (NEWS phrases + Rd text) | The help pages and NEWS now state the near-zero-mean limit, the sub-pixel disclosure, the window-width limit and the bounded/grouped grid | **FIXED** |

Round-5 must-pass rule (same data in different units → same picture and caption) still holds:
`test-vfx6-redesign.R:353` "scaling the data units changes nothing but the unit labels" passes, and the
new M1 test repeats it for the near-zero-mean case (percentage and checked area unchanged at 1e-3/1e3).

---

## Before / after evidence

### VFXX6-M1 — near-zero mean

Before:

```text
> vdist_normal_plot(mean = 1e-308, sd = 10)
Error: The normal cutoff cannot be represented numerically: sd / |mean| = Inf is
below the documented limit of 1e-14. Use a larger sd relative to the mean
(scaling mean and sd together is always allowed).
```

After:

```text
> vdist_normal_perc(0.95, mean = 1e-308, sd = 10)
subtitle: "P(X < 16.449) = 95%"
> vdist_normal_perc(0.95, mean = 1e-308, sd = 10, type = "upper")
subtitle: "P(X > -16.449) = 95%"
> vdist_normal_prob(16.45, mean = 1e-308, sd = 10)
subtitle: "P(X < 16.45) = 95%"
dark polygon area: 0.95000 (check tolerance 1e-3)
pnorm(16.449, 1e-308, 10) = 0.95
```

Still rejected (a genuinely tiny ratio):

```text
vdist_normal_plot(mean = 1, sd = 1e-15)
  sd / |mean| = 1e-15 is below the documented limit of 1e-14 ...
vdist_normal_plot(mean = 1e300, sd = 1e-10)
  sd / |mean| = 1e-310 is below the documented limit of 1e-14 ...
```

### VFXX6-M2 — sub-pixel chi-square lower tail

900x500 png device, number of exact `#0000CD` pixels (the measure the audit used; the audit
reported 0 shaded pixels for these cases):

| Case | Before (event polygon only) | After (marker + caption) |
|---|---:|---:|
| `vdist_chisquare_perc(.05, .1)` | 2 | **411** |
| `vdist_chisquare_perc(.50, .1)` | 2 | **411** |
| `vdist_chisquare_perc(.05, .2)` | 2 | **411** |
| `vdist_chisquare_prob(qchisq(.05, .1), .1)` | 2 | **411** |

("Before" is measured on the same plot with the marker layer removed, so it is the same picture the
audit counted; the 2 pixels are anti-aliasing of the dashed cutoff, not shading.)

Caption now printed:

```text
The shaded region is narrower than one pixel at this scale; it is marked by the blue strip
```

Ordinary cases are unchanged: `vdist_normal_perc(.95)` (84,433 dark px), `vdist_t_perc(.95, 5)`
(70,903), `vdist_f_perc(.95, 5, 10)` (98,063), `vdist_chisquare_perc(.95, .1)` (39,574) have no
marker and no note.

### VFXX6-L2 — extreme normal scale

Before:

```text
> vdist_normal_perc(0.95, sd = 3e307)
Error: The normal shaded region area (NaN) does not match the probability of its
event (0.95) within 1e-3; the density is too concentrated for the documented
plotting contract.
```

After (true message; the window width, not the ends, is the limit):

```text
> vdist_normal_perc(0.95, sd = 3e307)
Error: The normal plotting range (mean +/- 4 sd) spans more than the double range
for sd = 3e+307; sd must be below about 2.2e307 so the plotted window is
representable.
```

Just below the limit the same plot draws: `vdist_normal_perc(.95, sd = 2e307)` gives 84,466 dark
pixels and a checked area of 0.94999. The wide-polygon shoelace sum is finite:
`vdist_area(c(-1.68e308, -1e307, 4.93e307), c(0, 1.3e-308, 0))` is a finite positive number
(it was `NaN` when the zero-height closing edge was computed as `Inf * 0`).

### Grid blow-up — counted, not run

The 0.2.1 code used `seq(lmin, lmax, 0.01)` for the normal, so for
`vdist_normal_prob(1.2e6, 1e6, 2e5, "lower")` (`lmin = 0`, `lmax = 2e6`):

```text
length(seq(lmin, lmax, 0.01)) = 200000001
object.size(one copy)         = 1525.9 MB
```

The old prep function held that grid in `x` and again in `poly_data$y` / `poly_data$z` (and ggplot
copied it), which is the >18 GB growth the xplorerr re-audit saw. The 0.2.1 reprex itself was not
run.

Current code, same call: 168-row polygons, 2000 built curve points, 0.47 s including the png print. Across the must-pass set
(normal/t/chi-square/F at mean 1e6 / sd 2e5 and large cutoffs or df, plus binomial `n = 1e6`):

```text
normal_plot 1000 rows 0.60s   t_prob     2002 rows 0.36s   chisq_prob 2001 rows 0.27s
normal_perc 2000 rows 0.38s   t_big_df   2001 rows 0.36s   chisq_rng  2002 rows 0.27s
normal_prob 2000 rows 0.47s   f_plot     2002 rows 0.41s   f_perc     2001 rows 0.42s
normal_both 2000 rows 0.42s   f_prob     2001 rows 0.46s   binom_plot 2001 rows 0.18s
t_plot       801 rows 0.22s   t_perc     2000 rows 0.26s   binom_prob 2001 rows 0.18s
chisq_plot    67 rows 0.19s   chisq_perc 2001 rows 0.26s   binom_perc 2001 rows 0.17s
```

(rows = maximum rows in any built layer, measured with `ggplot_build()` after printing to a png.)
Binomial groups above 2001 counts: bar heights are the exact bin masses (their sum is 1 to 1e-8) and
the caption says the bars are grouped; `n + 1 <= 2001` keeps one bar per count, so every existing
teaching plot and snapshot is unchanged.

### VFXX6-L1 — tracked test artifact

```text
git ls-files tests/testthat/Rplots.pdf  ->  (empty)
file on disk after a full devtools::test()  ->  absent
```

`.gitignore` now lists `tests/testthat/Rplots.pdf` (plus the round's `.vfx7-tmp/` scratch directory),
and `tests/testthat/helper-device.R` opens `file.path(tempdir(), "vistributions-test-plots.pdf")`
before the tests run.

---

## Check output

`devtools::test()` with `NOT_CRAN=true`: all 15 test files green, no failures, no warnings.

Clean archive (`git archive HEAD` → `.vfx7-tmp/check-src`), then:

```text
R CMD build --no-manual check-src
  * building 'vistributions_0.2.2.tar.gz'   OK

R CMD check --as-cran --no-manual vistributions_0.2.2.tar.gz
  * checking examples ... OK
  * checking for unstated dependencies in 'tests' ... OK
  * checking tests ... Running 'testthat.R' [71s] OK
  * checking re-building of vignette outputs ... [11s] OK
  * DONE
  Status: 1 NOTE
  [ FAIL 0 | WARN 0 | SKIP 15 | PASS 1199 ]
```

The single NOTE is the pre-existing maintainer change:

```text
* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Aravind Hebbali <aravindhebbali@rsquaredacademy.com>'
New maintainer:  Aravind Hebbali <aravindhebbali@rsquaredacademy.com>
Old maintainer(s): Aravind Hebbali <hebbali.aravind@gmail.com>
```

0 errors, 0 warnings, maintainer NOTE only.

## Commands run that changed something

```text
Rscript -e "devtools::document()"        # regenerated man/*.Rd from roxygen (DESCRIPTION/NAMESPACE
                                         #   metadata churn from roxygen 8.1.1 reverted by hand)
git rm --cached tests/testthat/Rplots.pdf
Rscript .vfx7-tmp/count-old-grid.R       # counts the 0.2.1 grid, does not build it
git archive HEAD | tar -x -C .vfx7-tmp/check-src
R CMD build --no-manual check-src ; R CMD check --as-cran --no-manual vistributions_0.2.2.tar.gz
```

All heavy commands ran through `bash ~/.claude/bin/heavy-slot.sh pkg-vist-fx7 <cmd>`.

## Follow-ups (not done in this round, written out in full)

1. **Draw, rather than reject, normal windows wider than the double range.** `sd > ~2.2e307` is now
   rejected because the drawing rescales coordinates by `max - min`, which overflows. A real fix
   would draw such a normal in standardised units with rescaled axis labels (the same picture, the
   labels multiplied by `sd`). That is a drawing-layer redesign and was out of scope here.
2. **Extreme-tail polygons can still hold more than 5000 points.** `vdist_t_perc(.95, .02)` needs
   about 8,700 refinement points to reach the 1e-3 area contract (`vdist_region()` is capped at
   20,001). The cap is a bound, so memory is safe, but the "≤ 5000 rows" figure holds only for the
   large-scale cases in the must-pass set, not for every extreme tail. A follow-up could refine in
   probability space for heavy tails so fewer vertices suffice.
3. **Rebuild the committed pkgdown site.** `docs/` is committed but `.Rbuildignore`d; the help text
   changed in this round, so `pkgdown::build_site()` should be run before the site is republished.
   It is not part of `R CMD check` and was not run here.
4. **Binomial grouping is disclosed, not exact per count.** Above 2001 counts the bars are bins and
   a bar's colour is the bin's inclusion in the selected event; the printed probability `k` and the
   subtitles are still exact (`pbinom`/`qbinom`). A follow-up could add an inset showing the
   ungrouped count mass near the cutoff.
5. **Sub-pixel threshold is a fixed fraction of the panel.** 4e-3 of the view span is about 3 pixels
   at 900 px and 2.4 at 720 px. A device-size-aware threshold would be more precise but would need
   the device size at build time (it is not known until printing).
