# vistributions 0.2.2 — fix round 11

**Status: validated.** The supported-domain decisions are implemented in source commit `24e0e67`. Version remains 0.2.2.

## Domain table

| Input or display | Current implementation |
|---|---|
| Normal `sd / abs(mean)` | At least `1e-10` for nonzero mean, tested with a direct inclusive ratio comparison. |
| Continuous percentile `probs` | Strictly between `1e-12` and `1 - 1e-12`; endpoint and outside-range errors name `probs` and the limits. |
| Central normal `probs` and `sd` | Endpoints must enclose the requested central mass within `1e-6` relative; otherwise a documented resolution error suggests larger `sd` or `probs`. |
| F percentile resolution | When `qf` misses by more than `1e-6` relative, use the F-beta relation; if the computed quantile still cannot meet that tolerance, report a numerical-resolution error. |
| Binomial `tp` | Strictly between 0 and 1; targets within `1e-12` of a relevant jump are rejected, except exact equality, which uses `qbinom()` convention. |
| Display floor | Events below `1e-10%`, including an underflowed numeric zero, display `< 1e-10%`; the corresponding top display is `> 99.9999999999%`. Binomial threshold text uses a named target at the floor to avoid a double inequality. |
| Polygon area | At or above the display floor, the event polygon is checked against its event probability to `1e-6` relative error. Under-floor omissions are disclosed. |

**Central normal intervals:** a request is rejected when the representable endpoints cannot enclose `probs` to relative 1e-6. The error names `probs` and `sd` and recommends a larger value; the condition is documented under Supported values in the normal help and in NEWS. For example, `mean=1`, `sd=1e-10`, `probs=.01` gives representable endpoint mass `0.0100003138` and is rejected. The ordinary input grid is separately tested and must never trigger this condition.

**F quantiles:** if `qf()` does not invert the corresponding `pf()` tail to relative 1e-6, the implementation uses the F-beta relation, evaluating the beta complement independently to avoid cancellation. For F(1,1), `qf(1e-9,1,1)` is zero, whereas the new cutoff is `2.467401e-18` and its `pf()` value is `1e-9`. Both tails are checked at `1e-9`, `1e-8`, `.01`, `.5`, and `.99` for F(1,1), F(1,2), F(2,1), and F(5,10), along with the shaded mass.

## Finding ledger

These classifications describe the cited audit reproductions; checks beyond those examples are listed separately.

| Finding | Classification | Before -> after; test / base reference |
|---|---|---|
| astra H1, binomial quantile jumps | REJECTED-INPUT | Bernoulli `.5 + 2^-53` shaded `.5` instead of 1; it now errors naming `tp` and the jump limit. Exact `.5` is accepted: both tails are checked against `qbinom`/`pbinom` in `test-vfx11-findings.R`. |
| astra H2, companion cutoff text | FIXED | Normal cutoff `4` with companion `3.16e-03%` became `4.0005` with `3.16e-03%`, consistent with `pnorm` at the printed cutoff. Targeted test checks the numeric tail and label. |
| astra H3, wide small interval | UNDER-FLOOR | For `normal_prob(c(0,992),1000,1)`, `1.49e-16%` became `< 1e-10%`; the helper independently matches `diff(pnorm(c(0,992),1000,1))`. The analogous prior false `0%` now uses the floor. |
| astra H4, narrow differences | UNDER-FLOOR | The quoted 1-ULP normal and tiny t intervals are below the display floor. The targeted 1-ULP helper test compares against `integrate(dnorm, ...)`; subtraction uses the actual operands and quadrature handles close operands. |
| astra H5, tail shading | FIXED / UNDER-FLOOR | Cauchy upper percentile `1e-10` had area `1.769104921e-10`; current targeted test checks relative error below `1e-6` against `pt(qt(...))`. The normal 37.1 example is under-floor and now displays `< 1e-10%` with disclosure. |
| astra H6, central normal | REJECTED-INPUT | Audit input `.0014,1,1e-13` previously shaded `.001771658` instead of `.0014`; its scale ratio is below `1e-10`. The additional central-interval resolution condition is documented and tested. |
| astra H7, chi-square chord | FIXED | `chisquare_plot(500,xaxis_range=11)` changes from two vertices to a full grid; the old chord gave `3.504684e-309` at 10.89 versus base R `3.062410e-310`. The targeted test checks the new interpolated height by ratio against `dchisq`. |
| astra H8, clipped chi-square overlay | FIXED | df 200 default range changes from 0–25 to 120–280; the old panel top was `1.019342e-53` and clipped the overlay, while the new top is about `.0260` and contains the `dnorm(200,200,20)` peak of about `.01995`. Targeted test checks overlay values and panel limits. |
| astra M1, central error | REJECTED-INPUT | The quoted ratio `1e-13` is below the `1e-10` floor; errors identify `sd / abs(mean)`. A separate documented central-interval condition also rejects endpoints with a mass mismatch above 1e-6 relative. |
| astra M2, unstable scale boundary | REJECTED-INPUT / FIXED | The cited `2.9e-14` and `3e-14` ratios are below the new floor. The new boundary uses direct `sd / abs(mean) < 1e-10`; targeted tests cover rejection and equality at ordinary and large magnitudes. |
| astra M3 / Gemini M04, tick labels | REJECTED-INPUT / FIXED | The audit ratios `1e-13` and `1e-13` at mean `1e16` are below the new floor. At accepted ratio `1e-10`, all three normal builders use scale-aware labels; at mean `1e16`, sd `1e6`, five repeated `1e+16` labels became distinct labels from `9.999999996e+15` to `1.0000000004e+16`. Targeted tests compare distinct labels and `pnorm` at one sd. |
| astra M4, grouped-bin contract | FIXED | The grouped exact-event example has 2003 bins, both before and after; help now says approximately 2001 bins with up to two event-boundary splits, rather than promising at most 2001 equal-width bins. The original grouped bin mass tests compare to `dbinom` and `pbinom`. |
| astra L1 / Gemini M03, `OutDec` | FIXED | t interval lower endpoint `-0,000123` became `-0.000123`, matching upper `0.000123`; targeted test also compares `pt` mass. |
| F quantile inversion | FIXED | F(1,1) lower `1e-9` formerly had cutoff 0 and mass 0; the F-beta cutoff is `2.467401e-18` and `pf` returns `1e-9`. `test-vfx11-open.R` checks 4 df pairs, 5 probabilities, both tails and relative polygon mass. |
| Gemini H12, singular wide-tail cap | FIXED | F(1,1) upper percentile panel ceiling rose from `1.36e-10` to about `.207`, above `df(1,1,1)=.159155`. Targeted tests check plot, lower/upper percentile and lower/upper probability for F(1,1), F(1,2), F(2,1), plus chi-square df 1. |
| Gemini low, 1-ULP interval | UNDER-FLOOR | The audit tail was below `1e-10%`; the stable difference helper uses integration when subtraction is ill-conditioned. |
| Bound-aware cutoff formatting | FIXED | At a normal upper-tail input of `9.999e-13`, the old rounded cutoff `7.034` had base-R tail `1.003476e-12`, contradicting the floor label. The revised cutoff `7.0345` has tail `9.99884e-13`; a value-asserting test checks the inequality. |
| Underflow displays | FIXED | Normal upper 40 formerly printed `0%`; it now prints `< 1e-10%` and reports a too-small region. Binomial under-floor probabilities and target text have targeted `dbinom` comparisons. |
| Heavy-tail and high-df regressions found during fx11 | FIXED | t df .1 lower `1e-10` once produced polygon area `8.67e28`; logarithmic x spacing and refinement now match `pt(qt(...))` within `1e-6` relative. F(1e6,1e6) had an inaccurate base tail-bound quantile; inversion refinement restores coverage. |

## Test and snapshot classification

The initial 50 failures / 15 errors included old numeric expectations for newly floored percentages, prior normal limits and percentile limits, fixed-25 chi-square views, and SVGs whose polygons were resampled. Those expected values were updated to the policy or made explicit with `xaxis_range=25`. Real numerical regressions found during the rerun are documented above. A subsequent full disposable run gave 60 failures / 14 errors after extending the floor to exact numerical zero and one; it exposed the two shading regressions above and 32 SVG differences. All 32 SVG old/new pairs were rendered and reviewed in contact sheets: chi-square defaults have the intended new x ranges and other shapes remain visually consistent after refinement. The updated Windows snapshots were copied from the disposable run. The three Darwin-specific variants and their corresponding ordinary SVGs were removed; those plots now have platform-independent `layer_data()` density and polygon-mass assertions, including on CRAN. A subsequent focused suite passed with zero failures and errors. The final full disposable suite passed with **6,621 passes, 0 failures, 0 errors**, including the standalone CRAN-capable platform tests.


The individual failing test groups in the saved follow-up run were classified as follows. Multiple counts in one group are separate assertions; none was waived solely to make the suite green.

| Test group / prior count | Disposition |
|---|---|
| `test-correctness-022` H6 (1 error) | Exact mathematical jump used a literal differing from `pbinom` by floating-point rounding; test now uses the actual base-R jump and verifies its mass. |
| `test-correctness-022` L3 (3 failures) | Superseded by the percentage floor/top bound; expectations updated. |
| `vfx10` H04 (1 error) | Near-jump target is now rejected; test checks the distance and error. |
| `vfx10` H06 (7 failures), H07 (1), M02 (4) | Under-floor or top-bound text replaced numeric output; expectations now check the appropriate bound and base probability. |
| `vfx10` H08, H09 and M04 (3 errors) | Old `1e-13` normal scales are rejected; tests use the new boundary and check accepted values. |
| `vfx10` H11 (2 failures) | Default chi-square view intentionally changed; test now explicitly requests the prior 0–25 range and compares the curve to `dchisq`. |
| `vfx3` H2, H3 and documented-limit groups (2 failures, 3 errors) | Old probability and normal scale ranges were superseded; tests now exercise the new ranges and base-R values. |
| `vfx4` H2 (1 error), M1 (1 failure) | New normal floor and chi-square default view; explicit old view retained for the area comparison. |
| `vfx5` M2 (1 error) | Real heavy-t polygon defect; logarithmic region sampling and relative refinement fixed it. |
| `vfx5` L1 (1 failure) | Old documented normal floor replaced by `1e-10`. |
| `vfx6` finding grid (1 failure, 1 error), low-df plot (1 failure), limits (1 failure, 1 error), contract table (1 failure) | Floor/top text, revised normal and percentile limits, and chi-square default range; tests now check the new contract or explicitly select the former view. |
| `vfx7` normal rejection (1 error) | Old normal floor and message superseded; direct-ratio error is checked. |
| `vfx7` shaded pixels (2 failures) | Real overlarge axis cap for a highly singular chi-square body; the cap now retains a visible body. |
| `vfx7` large-scale grid (1 error) | Real inaccurate high-df F tail bound; CDF-based bound refinement restores coverage. The bounded-grid assertion was updated for the higher refinement budget needed by the relative tolerance. |
| `vfxx` empty interval (1 failure) | Zero display is now the specified floor bound; expectation updated. |
| `visual`, 32 assertions across chi-square (8), F (6), normal (8), and t (10) | Reviewed old/new SVGs: chi-square defaults change view and the remaining changes are finer polygon vertices. Three platform-sensitive plots now use tolerant numeric layer and area tests; the remaining snapshots were regenerated. |

The earlier 50-failure/15-error run was a partial reporter output before the later zero/one floor changes. The saved 60-failure/14-error follow-up above enumerates all test groups, including those additional floor changes.

The ordinary regression grid is `tests/testthat/test-vfx11-ordinary-grid.R`: means 0–1000 and sd .01–1000, t and chi-square df 1–500, seven F df pairs up to 500, binomial n 1–10000 and p .01–.99, all relevant types and percentiles .01–.99. The expanded disposable grid checked polygon masses and printed probabilities: **4,633 passes, 0 failures, 0 errors**, including checks for all five normal bands and the t, chi-square and F shape plots.

## Commands and checks

- `devtools::document()` with roxygen2 **7.3.3**, the version recorded in DESCRIPTION: no generated-file changes on repetition. Roxygen2 7.3.3 was installed in a disposable temporary library; DESCRIPTION was not upgraded.
- The decisions test `test-vfx11-open.R` passed **292 assertions**, including 210 ordinary central normals and the F cutoff/mass matrix; the standalone platform tests passed **6 assertions**.
- Targeted `testthat::test_file("tests/testthat/test-vfx11-findings.R")` in a disposable copy: **295 passes, 0 failures, 0 errors**, including the strict guard, bound-aware cutoff test, 108 exact or near-jump binomial checks, and cross-family checks that both printed labels agree with the printed cutoff. Running the earlier version of this file on clean baseline `5fd28e8` gave 36 failures and 1 error; the exact-jump preservation group already passed there, as expected for retained behavior.
- Preliminary source-copy `R CMD check --as-cran --no-manual` initially stopped because inherited `LC_ALL=C.UTF-8` is unsupported on this Windows setup. With `English_United States.utf8`, the preliminary check reached the test suite but returned 1 ERROR and 2 NOTEs: one NOTE and five test errors came from an invalid NEWS encoding, since corrected; a sixth test failure was an old binomial caption expectation, since updated. A subsequent preliminary check with valid UTF-8 reached 1 ERROR and the maintainer NOTE: a help-text test assumed `occupy a pixel` was on one line in installed Rd text. The assertion now accepts whitespace across the line break. A fresh source-copy `R CMD build` and `R CMD check --as-cran --no-manual` then finished **Status: 1 NOTE**, the maintainer change only. This remains a preliminary check, not the requested clean-HEAD check.
- `pkgdown::build_site()` was run from a disposable clean archive with the updated sources. Generated audit pages from an earlier worktree build were removed and the clean site copied back; neither audit file is included in search or sitemap. `pkgdown::build_news()`, `pkgdown::build_reference()` and `pkgdown::build_search()` refreshed NEWS and the final help changes. Additional t example variants remain visible under `\dontrun{}` and are covered by the test suite, keeping the runnable examples within CRAN timing limits.
- Final `r <- as.data.frame(devtools::test(reporter = "silent"))` in a disposable copy with `NOT_CRAN=true`: `sum(r$failed) = 0`, `sum(r$error) = 0` (6,621 passed).
- `git archive HEAD` at source commit `24e0e67`, followed by `R CMD build` and `R CMD check --as-cran --no-manual`: **1 NOTE**, the maintainer change, and no errors or warnings. This report is added in a subsequent report-only commit; `docs-dev` is excluded from the package build.

Commands used (checks and tests used disposable copies; the site was generated from an audit-free disposable source copy):

```r
Sys.setenv(NOT_CRAN = "true")
r <- as.data.frame(devtools::test(reporter = "silent"))
sum(r$failed)
sum(r$error)
devtools::document()
pkgdown::build_site()
```

```powershell
git archive --format=tar --output=<temporary-tar> HEAD
# Extract the archive to a new disposable source directory.
R CMD build <source-directory>
R CMD check --as-cran --no-manual vistributions_0.2.2.tar.gz
```

The Windows checks used `LC_ALL` and `LC_CTYPE` set to `English_United States.utf8`, avoiding the unsupported inherited `C.UTF-8` locale. The project-recorded roxygen2 7.3.3 was supplied through a temporary R library.

Audit files were read but not committed. No push or branch switch was performed.
