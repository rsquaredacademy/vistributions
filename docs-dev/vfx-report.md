# vistributions 0.2.2 fix round 1 (job vfx)

Base: origin/develop 3836fe5, branch task/vist-fix-022. Findings: `F:/pm/notes/2026-11-04-codex-batch/cx-vistributions-audit.md`.
All new tests are in `tests/testthat/test-correctness-022.R`; each asserts values against stats d/p/q functions or a hand value.
Fails-before was verified by running that file against a `git archive` of the base commit.

| Finding | Test added | Fix | Verified how | Status |
|---|---|---|---|---|
| H1 version reuse | (check) | DESCRIPTION Version 0.2.2 | `R CMD check --as-cran`: incoming feasibility shows only the maintainer NOTE | Done |
| H2 fractional df | "H2: fractional df is kept ..." (9 expectations), "H2: densities use the real-valued df" (6) | `as.integer(df)` removed in `R/vdist-data-prep.R` (cplot/cperc, fplot/fperc/fprob, tplot/tperc/tprob) | labels equal `round(pt/pchisq/pf/qchisq/qf/qt, 3)`; densities equal `dt/dchisq/df` incl. df = .5; fails on base | Done |
| H3 rounded normal quantile | "H3: normal quantiles are not rounded" | `nperc_data_prep` keeps full precision; labels via `fmt_num()` (`R/vdist-utils.R`) used in `R/vdist-plots.R` | vline = `qnorm(.95, sd=.0001)`; subtitle "P(X < 0.0001645) = 95%"; the sd = 1e-5 case no longer errors; errors on base | Done |
| H4 binomial count coercion | "H4: binomial counts must be whole numbers", "H4: probabilities match stats", "H4: summary sd" | `check_count()` for n; whole-number check for s (`R/vdist-binomial.R`); `as.integer(s)` removed; sd from `sqrt(n p (1-p))` | errors for s = 2.9 / -.9, n = 3.9; mass matches pbinom. The "match stats" and "sd" tests are guards (pass on base too) | Done (API change: non-integer n or s is now an error) |
| H5 reversed intervals | "H5: reversed binomial intervals are rejected", "H5: reversed normal intervals are rejected" | error "increasing order" in `vdist_binom_prob`; same check for `vdist_normal_prob(c(2, -1), "both")` (same defect, found while testing) | errors asserted; fail on base | Done (API change: error) |
| H6 binomial quantile caption / shading | "H6: caption states the qbinom definition", "H6: shading matches the reported cutoff" (26 expectations over a tp grid plus the exact boundary) | `bperc_data_prep`: lower shades x <= k, upper shades x > k; caption `P(X <= k-1) < tp, but P(X <= k) >= tp` (`R/vdist-plots.R`) | shaded mass = pbinom(k) (lower) and upper-tail mass <= tp; audit case 0.0107 instead of 0.0547 | Done (convention in `@param tp`; tp must be in (0, 1)) |
| H7 negative symmetric t | "H7: negative symmetric t cutoffs ..." | `vdist_t_prob` uses `abs(perc)` for interval/both before prep and build | captions "P(-2 < X < 2) = 91.4%", "P(\|X\| > 2) = 8.6%", vlines at -2 and 2 | Done |
| M1 quantile outside fixed grid | "M1: quantiles beyond the default grid ...", "M1: very large cutoffs do not allocate huge grids" | grids extended to cover the cutoff (`vdist_seql`, `vdist_xax`, `vdist_chiseql`, t/F bounds); `vdist_seq()` caps at 20000 steps | vline = qt(.95,1), qchisq(.999,1), qnorm(.9999999), qf(.999,1,1); 1e6 cutoffs give < 30000 rows | Done |
| M2 polygon step | "M2: the shaded region keeps its area at small scales" | `vdist_seq()` uses at least 100 steps when the region spans fewer than 20 default steps | trapezoid area of the central polygon = pnorm(1) - pnorm(-1) (tol 1e-3) for sd .001, .05, 1, 250 | Done |
| M3 probs endpoints | "M3: probs of 0 or 1 are rejected ..." | `check_open_range()` in the four continuous `*_perc` and binomial `tp` | messages asserted | Done (documented in `@param probs`, `@param tp`) |
| M4 maintainer address | n/a | DESCRIPTION, man/vistributions.Rd | check shows the new-maintainer NOTE only | Done |
| L1 NEWS / cran-comments | n/a | NEWS.md 0.2.2 section; cran-comments.md rewritten for 0.2.2 | read | Done |
| L2 README | n/a | comment corrected in README.Rmd and README.md | audit arithmetic (0.888) | Done |
| L3 validation | "L3: missing, empty and vector inputs ...", "L3: cutoffs outside the support ..." | `check_scalar()`, stricter `check_logical()`, `s` length check, finite `perc`; chi-square/F prob clamp the cutoff to the support start | messages asserted; `vdist_f_prob(-1)` = "P(X < -1) = 0%" | Done |

## Snapshots
12 vdiffr SVGs updated, nothing else: `{normal,t,f,chisquare}-perc-*` (9 files) shift by sub-pixel amounts because the cutoff is no longer rounded to 3 decimals (intended, H3); `binomial-perc-lower` (caption text, H6) and `binomial-perc-upper` (bar 8 is no longer shaded, H6). The other 27 snapshots are unchanged, so ordinary plots look the same.

## Re-run of the audit reproducers (after the fix)
```
vdist_t_prob(2, df = 1.9)                    "P(X < 2) = 90.5%"        (pt(2, 1.9) = 0.9049)
vdist_chisquare_perc(.95, df = 1.9)          "P(X < 5.796) = 95%"      (qchisq = 5.796287)
vdist_f_prob(1, .5, 3)                       "P(X < 1) = 69.3%"        (no NaN)
vdist_normal_perc(.95, sd = .0001)           "P(X < 0.0001645) = 95%"
vdist_normal_perc(.95, .0006, 1e-5)          "P(X < 0.00061645) = 95%" (no error)
vdist_binom_prob(10, .5, s = 2.9, "exact")   Error: s must be whole numbers (counts of successes).
vdist_binom_plot(n = 3.9)                    Error: n must be a single non-negative whole number.
vdist_binom_prob(10, .5, c(6, 4), "interval") Error: The interval for s must be given in increasing order.
vdist_binom_perc(10, .5, .05)                "P(X <= 1) < 0.05, but P(X <= 2) >= 0.05"
vdist_binom_perc(10, .5, .05, "upper")       "P(X >= 9) <= 0.05, but P(X >= 8) > 0.05", shaded mass 0.0107
vdist_t_prob(-2, 7, "interval")              "P(-2 < X < 2) = 91.4%"
vdist_t_prob(-2, 7, "both")                  "P(|X| > 2) = 8.6%"
vdist_t_perc(.95, df = 1)                    "P(X < 6.314) = 95%"
vdist_chisquare_perc(.999, df = 1)           "P(X < 10.828) = 99.9%"
vdist_normal_perc(.9999999)                  "P(X < 5.199) = 99.99999%"
vdist_normal_plot(sd = .001)                 central polygon 103 rows (was 3)
vdist_*_perc(1)                              Error: probs must be greater than 0 and less than 1.
```

## Checks
- `devtools::test()` (NOT_CRAN=true): FAIL 0 | WARN 0 | SKIP 0 | PASS 291 (was 191; 100 new expectations).
- Clean `git archive HEAD` (commit 3c759a8), `R CMD build`, `R CMD check --as-cran --no-manual` (LC_ALL=C, R_CRAN_SRC/WEB = cloud.r-project.org, R 4.6.1, Windows 11): Status 1 NOTE, 0 errors, 0 warnings. The NOTE is CRAN incoming feasibility: new maintainer address (intended, M4). Examples, tests and vignette rebuild OK. The last commit only adds build-ignored files (cran-comments.md, docs-dev/).
- Fails-before: `test-correctness-022.R` run against the base commit: 15 of 17 test blocks fail or error; the two that pass on base are the H4 "match stats" and H4 "sd" guards.

## Deferred
- Chi-square overlay and axis text use `chisd = round(sqrt(2 df), 3)`, which rounds to 0 for df < 0.00025; changing it would shift ordinary snapshots.
- F density at 0 is infinite for num_df <= 2 (pre-existing; `geom_line` drops the point).
- `*_prob()` labels print `round(p, 3) * 100`, so probabilities below 0.0005 show as 0%.
- No cap on `n` beyond the integer range: `vdist_binom_plot(n = 1e8)` tries to draw 1e8 bars.
- win-builder / rhub / reverse-dependency checks and the actual CRAN submission were not run here.

## Commands that changed something
`git archive` + `tar -x` into scratch directories inside the worktree (removed afterwards); `devtools::document()` (roxygen 8.1.1 is installed, so its NAMESPACE, DESCRIPTION and vistributions.Rd changes were reverted by hand to keep the RoxygenNote 7.3.3 output; only the man files with new parameter text were kept); python edit scripts for the source edits; `git commit` x3.
