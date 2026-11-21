# pkg-vist-ct1 report

## Measurements

Baseline: `NOT_CRAN=true`, `testthat::test_local(reporter="summary", stop_on_failure=FALSE)` on the submitted tree. Total elapsed: 1787.64 seconds; 34,409 passes, 0 failures, 0 errors, 0 warnings, 0 skips. Timings below are sums of testthat per-test elapsed times, excluding file and runner overhead.

After: `pkgload::load_all()` followed by `Sys.unsetenv("NOT_CRAN")` and `testthat::test_dir("tests/testthat", reporter="summary", stop_on_failure=FALSE)` on the clean archive, with `png` in the disposable library. Total elapsed 92.41 seconds, including runner overhead; 1,505 passes, 0 failures, 0 errors, 0 warnings, and 59 skips. The full non-CRAN suite was running concurrently, as during much of the baseline and the archive checks. The check itself is the authoritative CRAN timing. A dash means the file did not exist before.

| Test file | Before, full suite (s) | After, CRAN mode (s) |
|---|---:|---:|
| `test-app.R` | not present | 0.25 |
| `test-binomial.R` | 0.30 | 0.47 |
| `test-bug-regressions.R` | 3.20 | 4.65 |
| `test-chisquare.R` | 0.11 | 0.19 |
| `test-correctness-022.R` | 8.97 | 17.31 |
| `test-f.R` | 0.20 | 0.20 |
| `test-normal.R` | 0.23 | 0.21 |
| `test-plot-assembly.R` | 0.75 | 0.97 |
| `test-t.R` | 0.08 | 0.11 |
| `test-vfx10-findings.R` | 7.86 | 8.07 |
| `test-vfx11-findings.R` | 23.42 | 4.72 |
| `test-vfx11-open.R` | 32.10 | 0.05 |
| `test-vfx11-ordinary-grid.R` | 211.78 | 21.37 |
| `test-vfx11-platform.R` | 0.87 | 1.46 |
| `test-vfx12-continuous.R` | 5.09 | 7.35 |
| `test-vfx12-findings.R` | 5.21 | 2.48 |
| `test-vfx13-binomial-caption.R` | 69.46 | 0.42 |
| `test-vfx14-findings.R` | 12.69 | 1.93 |
| `test-vfx15-findings.R` | 200.00 | 1.33 |
| `test-vfx16-findings.R` | 493.51 | 2.63 |
| `test-vfx16b-formatting.R` | 78.85 | 0.22 |
| `test-vfx18-render.R` | 472.38 | 0.03 |
| `test-vfx3-findings.R` | 21.09 | 1.40 |
| `test-vfx4-findings.R` | 16.48 | 3.83 |
| `test-vfx5-findings.R` | 17.05 | 0.92 |
| `test-vfx6-redesign.R` | 52.63 | 3.10 |
| `test-vfx7-findings.R` | 22.94 | 1.43 |
| `test-vfx8-findings.R` | 2.27 | 0.56 |
| `test-vfxx-findings.R` | 22.06 | 0.95 |
| `test-visual.R` | 1.98 | 0.03 |

Slowest baseline tests (elapsed seconds):

- `test-vfx18-render.R` — fx18: real-device ticks do not overlap and ticks/captions fit: 472.35 s.
- `test-vfx16-findings.R` — vfx16: cutoff-anchored labels stay separated, inside and on side: 354.52 s.
- `test-vfx11-ordinary-grid.R` — ordinary distribution grid matches base R: 211.78 s.
- `test-vfx16-findings.R` — vfx17: labels are drawn last, over an opaque halo: 131.72 s.
- `test-vfx15-findings.R` — fx15: percentile glyph extents stay inside ordinary linear panels: 86.52 s.
- `test-vfx15-findings.R` — fx15: the t joint domain works at and just inside its boundaries: 74.64 s.
- `test-vfx13-binomial-caption.R` — binomial percentile captions state the qbinom convention across jumps: 69.09 s.
- `test-vfx16b-formatting.R` — fx16b: all percentile types round trip through their printed event: 61.25 s.
- `test-vfx15-findings.R` — fx15: chi-square windows retain the body and tiny mass is disclosed: 31.47 s.
- `test-vfx11-open.R` — F quantiles invert the requested tail over the supported grid: 16.43 s.
- `test-vfx16b-formatting.R` — fx16b: rendered percentage colours match c63f7de in every family: 15.77 s.
- `test-vfx11-open.R` — ordinary central normal percentiles never trigger the resolution error: 15.64 s.
- `test-vfxx-findings.R` — VFXX-H4: chi-square grids respect the support and stay finite: 13.85 s.
- `test-vfx6-redesign.R` — low-df chi-square and F regions keep the mass at the support: 13.67 s.
- `test-vfx6-redesign.R` — shading is visible in the built panel for the must-pass inputs: 11.53 s.
- `test-vfx7-findings.R` — every grid is bounded and fast at large scale: 10.69 s.
- `test-vfx6-redesign.R` — every continuous finding keeps its exact area and caption: 10.44 s.
- `test-vfx3-findings.R` — VFXX2-H1: fractional-df chi-square shading keeps the requested mass: 9.83 s.
- `test-vfx11-findings.R` — binomial exact jumps and targets outside the exclusion match qbinom: 9.12 s.
- `test-vfx5-findings.R` — every must-pass ordinary input keeps its event area and visible shading: 7.54 s.

## CRAN skips and retained coverage

The following blocks newly call `skip_on_cran()`; all their original expectations and full inputs remain active with `NOT_CRAN=true`:

- `test-vfx10-findings.R`: “H06: small tail probabilities are computed on the right scale”.
- `test-vfx11-findings.R`: “singular F panel includes the body”; “singular F and chi-square bodies remain visible in every export”; “binomial exact jumps and targets outside the exclusion match qbinom”; “printed cutoffs support both labels for all continuous families”; “large translated normal ticks remain distinct in all builders”.
- `test-vfx11-open.R`: “ordinary central normal percentiles never trigger the resolution error”; “F quantiles invert the requested tail over the supported grid”.
- `test-vfx11-ordinary-grid.R`: “ordinary distribution grid matches base R”.
- `test-vfx12-findings.R`: “two cutoff labels agree with both printed tails”; “F near-one upper quantiles reproduce the smaller tail”.
- `test-vfx13-binomial-caption.R`: “binomial percentile captions state the qbinom convention across jumps”.
- `test-vfx14-findings.R`: “L1.1: t percentile annotations are cutoff-anchored on wide windows”; “L1.2: the chi-square mean marker sits on the axis”; “M2.1: t degrees of freedom are floored at a documented value”.
- `test-vfx15-findings.R`: “fx15: percentile glyph extents stay inside ordinary linear panels”; “fx15: wide t and F label pairs have visible gaps and unclipped glyphs”; “fx15: the t joint domain works at and just inside its boundaries”; “fx15: chi-square windows retain the body and tiny mass is disclosed”.
- `test-vfx16-findings.R`: “vfx16: large interval and cutoff text use scientific notation”.
- `test-vfx16b-formatting.R`: “fx16b: named switch and halfway events use consistent text”; “fx16b: all percentile types round trip through their printed event”; “fx16b: rendered percentage colours match c63f7de in every family”.
- `test-vfx3-findings.R`: “VFXX2-H1: fractional-df chi-square shading keeps the requested mass”; “VFXX2-H2: unusable quantiles and extreme probabilities are rejected”; “VFXX2-M2: annotations, markers and reference lines stay in view”; “the documented grid keeps the shoelace area within 1e-3”.
- `test-vfx4-findings.R`: “VFXX3-H1: upper-tail areas cover the event, not just the view”; “VFXX3-H2: translated tiny-scale normals are rejected at the limit”; “VFXX3-M2: captions are true at the precision they print”; “VFXX3-L1: the chi-square floor is enforced at the documented 0.1”; “every exported plotting default returns a plot”.
- `test-vfx5-findings.R`: “VFXX4-H1: resolvable tiny-unit normals are accepted”; “VFXX4-H2: the shaded region is visible in the rendered panel”; “VFXX4-M2: the stated domain matches the implementation”; “every must-pass ordinary input keeps its event area and visible shading”; “every exported plotting default returns a usable plot”.
- `test-vfx6-redesign.R`: “every continuous finding keeps its exact area and caption”; “captions are true at the printed precision for every rounding regime”; “area and caption use the exact distribution functions”; “shading is visible in the built panel for the must-pass inputs”; “the shading polygon covers tails beyond the plotted window”; “low-df chi-square and F regions keep the mass at the support”; “scaling the data units changes nothing but the unit labels”; “the grids stay bounded”.
- `test-vfx7-findings.R`: “a sub-pixel chi-square lower tail is visible on the device (VFXX6-M2)”; “an ordinary shaded region is not marked as sub-pixel (VFXX6-M2)”; “every grid is bounded and fast at large scale”.
- `test-vfx8-findings.R`: “VFXX8-R2: dropping a duplicate vertex never collapses a heavy tail”.
- `test-vfxx-findings.R`: “VFXX-H2: tiny cutoffs are printed with enough precision to be true”; “VFXX-H3: the shaded polygon area matches the event probability”; “VFXX-H4: chi-square grids respect the support and stay finite”; “VFXX-M1: the normal overlay uses the unrounded df and sd”.

Pre-existing CRAN skips remain for the vfx16 and vfx17 large device label grids, the vfx18 real-device grid, and the vdiffr snapshots. The full ordinary grid is unchanged in its input sets and assertions, and skips on CRAN. Its representative CRAN version uses two normal means, two t/chi-square degrees of freedom, two F degree pairs, and two binomial sizes; it covers all five distributions, plot/percentile/probability builders and every event type in the original grid. The separate ordinary teaching-input and continuous numeric checks also remain. Each plotting export has a CRAN value check. `vdist_launch_app()` has a mocked delegation return-value test when its suggested `xplorerr` dependency is installed; it skips when that dependency is unavailable. The small grid and launcher test skip when `NOT_CRAN=true`, so the full-suite assertion count remains comparable.

## Verification

- Full post-change suite with `NOT_CRAN=true`: 34,409 passes, 0 failures, 0 errors, 0 warnings, 2 skips; 1875.54 seconds elapsed. The two new CRAN-only checks skip because the full grid already runs and the launcher check is exercised by the CRAN pass. This run started before the last measured skip annotations were added; those annotations have no effect with `NOT_CRAN=true` and no expectations or test bodies changed thereafter.
- Clean `git archive HEAD` source built with `R CMD build`; `R CMD check --as-cran --no-manual` with `NOT_CRAN` unset: tests [61s] OK, examples [20s] OK, vignette rebuilding [21s] OK. Testthat: 1,499 passes, 0 failures, 0 warnings, 60 skips. Check status: 1 NOTE, CRAN incoming reports the maintainer email change. The vignette source is unavailable in the installed check and one related test skips.
- Earlier archive checks: missing suggested `png` produced an error; after installing `png` into the disposable library, inherited `C.UTF-8` locale settings caused startup warnings and an error. Setting `LC_ALL` and `LC_CTYPE` to `English_United States.utf8` and `LANG` to `en_US.UTF-8` resolved those environment problems. An intermediate successful check under concurrent full runs had tests [132s], examples [16s], vignette rebuilding [25s], status 1 NOTE; additional measured audit blocks were then skipped.
- No examples exceeded the reported 20-second threshold in the final check; no roxygen, man pages, package code, vignettes, DESCRIPTION, NEWS, or generated documentation was changed.

Commands: `git archive --format=tar HEAD`, `tar -xf`, `R CMD build` from the extracted source, and `R CMD check --as-cran --no-manual vistributions_0.2.2.tar.gz`. The build and check used the temporary library in `.pkg-vist-ct1-tmp/`; `install.packages("png", repos="https://cloud.r-project.org", lib=...)` installed the missing suggested dependency there. Full suite runs used `Sys.setenv(NOT_CRAN="true")` and `testthat::test_local(reporter="summary", stop_on_failure=FALSE)`; the timing pass used the `test_dir` command described above. Test result objects were read with `as.data.frame()` for per-test timings and counts.

Cleanup remains undone: automatic command review blocked both recursive deletion of the verified disposable folder and deletion of an individual temporary tar file. The folder `.pkg-vist-ct1-tmp/` therefore still contains the disposable library, archives, check outputs and tarballs. `devtools::check()` was not run separately: the requested clean-archive `R CMD check --as-cran --no-manual` is the full check performed here. No pkgdown rebuild or roxygen regeneration was needed for these test-only changes. No other work was left undone.
