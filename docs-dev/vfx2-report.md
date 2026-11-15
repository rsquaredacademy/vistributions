# vistributions 0.2.2 — fix round 2 (pkg-vfx2)

Worktree: `F:/R/Packages/_worktrees/vist-fix-022` (branch `task/vist-fix-022`).
Base for this round: `6d26c7d` (the three `vfx` commits). Findings: Codex re-audit
`docs-dev/vfxx-reaudit.md` (untracked, left in place), VFXX-H1–H4, M1, L1–L3. That round fixed the
eight VFXX findings within its own scope; the subsequent VFXX2 re-audit
(`docs-dev/vfxx2-reaudit.md`) found four high, two medium and one low remaining result, corrected in
the vfx3 round (see `docs-dev/vfx3-report.md`). This report's "all fixed" statements describe the
VFXX round only and must not be read as a release-readiness claim.
Commits: `1e68fd3` (code + tests + snapshots), `dad7416` (roxygen/Rd + NEWS),
plus the commit that adds this report. Not pushed; no GitHub operations; no branch switch.

Environment: Windows 11 x64, R 4.6.1 ucrt (2026-06-24), ggplot2 4.0.3, testthat 3.3.2,
vdiffr 1.0.9, roxygen2 8.1.1 (see follow-up F5). Heavy commands ran through
`bash ~/.claude/bin/heavy-slot.sh pkg-vfx2 …`; scratch in `.vfx2-tmp/`.

## Files changed

| File | Change |
|---|---|
| `R/vdist-utils.R` | `check_count()` returns the rounded count (H1); `fmt_num()` uses significant digits in scientific notation for cutoffs too small for 15 decimals (H2); new `vdist_grid()` (central-mass sampling) and `vdist_support_start()` (H3/H4) |
| `R/vdist-binomial.R` | `n <- check_count(n)` in all three exports (H1) |
| `R/vdist-data-prep.R` | binomial prep uses `round(n)` instead of `as.integer(n)` (H1); chi-square/F grids use `vdist_grid()`, start on the support and are clipped to the central mass; drawn cutoffs clipped (`pc`) while captions keep the true value (H3/H4); chi-square overlay from unrounded `df` (M1) |
| `R/vdist-plots.R` | polygon helpers build grids with `vdist_grid()` and a new `vdist_pol_x()` (H3/H4); chi-square summary labels formatted from the unrounded `df` with `fmt_num()` (M1) |
| `R/vdist-normal.R` | `perc` length validated before preparation (L1); `perc` roxygen documents one/two-value interval semantics (L2) |
| `man/vdist_normal_plot.Rd` | regenerated from the roxygen change (L2) |
| `NEWS.md` | corrected the resolution wording (L3) and added the round-2 user-visible fixes |
| `tests/testthat/test-vfxx-findings.R` | new: 6 blocks / 97 expectations covering H1–H4, M1, L1 |
| `tests/testthat/test-normal.R` | message updated for the L1 check, empty-`perc` assertion added |
| `tests/testthat/_snaps/visual/chisquare-{plot-3,perc-lower,perc-upper}.svg` | updated (see “Snapshots”) |

## Findings

### VFXX-H1 — accepted near-integer binomial counts [Fixed]

**Test:** `test-vfxx-findings.R` → “VFXX-H1: accepted near-integer trial counts are rounded once”.

**Fix:** `R/vdist-utils.R:71` (`check_count()` returns `round(val)`); `R/vdist-binomial.R:46,68,115`
(`n <- check_count(n)`); `R/vdist-data-prep.R:12,25,54` (`round(n)` instead of `as.integer(n)`).
The rounded count reaches the title, subtitle, support (`seq(0, n)`), `dbinom()`/`qbinom()`,
`scale_x_continuous(breaks = seq(0, n))` and the `s > n` check.

**Before** (`n <- (.7 - .4) * 10`, HEAD `6d26c7d`):

```text
title   "Binomial Distribution: n = 3 , p = 0.5"      (from the raw n, not the plotted one)
data    0 1 2        mass 0.25 0.50 0.25             (truncated to two trials)
P(X = 2) = 0.25                                       (dbinom(2, n, .5) = .375)
vdist_binom_prob(n, .5, 3, "exact")  ->  Error: s must be less than or equal to n.
vdist_binom_perc(n, .5, .5)$data$n   ->  0 1 2
```

**After:**

```text
title   "Binomial Distribution: n = 3 , p = 0.5"
subtitle "Mean = 1.5 , Std. Dev. = 0.87"
data    0 1 2 3      mass 0.125 0.375 0.375 0.125    (== dbinom(0:3, 3, .5))
P(X = 2) = 0.375
P(X) = 3 = 0.125 ; P(X) <= 3 = 1                     (s = 3 accepted)
vdist_binom_perc(n, .5, .5)$data$n   ->  0 1 2 3      shaded mass == pbinom(qbinom(.5,3,.5), 3, .5)
```

3 ± 1e-9 round to 3; 2.9/3.1/2.9 (`s`) still error. Test asserts values, not the snapshot.

### VFXX-H2 — false tiny-cutoff captions [Fixed]

**Test:** “VFXX-H2: tiny cutoffs are printed with enough precision to be true”.

**Fix:** `R/vdist-utils.R:94` — when the required decimals exceed 15, print
`signif(x, 4)` in scientific notation instead of rounding to 15 decimals (which yields 0).
Ordinary values keep the previous three-decimal behaviour.

**Before → after:**

| Call | HEAD caption | fixed caption | caption parsed back |
|---|---|---|---|
| `vdist_normal_perc(.95, sd = 1e-16)` | `P(X < 0) = 95%` | `P(X < 1.645e-16) = 95%` | `pnorm(1.645e-16, sd = 1e-16)` = .950015 |
| `vdist_chisquare_perc(.05, df = .1)` | `P(X < 0) = 5%` | `P(X < 1.115e-26) = 5%` | `pchisq(1.115e-26, .1)` = .05 ± 1e-6 |
| `vdist_f_perc(1e-6, 1, 1)` | `P(X < 0.000000000002467) = 1e-04%` | same value, true | `pf(2.467e-12, 1, 1)` = 1e-6 |

Ordinary labels are byte-identical (e.g. `P(X < 0.0001645) = 95%`, `P(X < 5.796) = 95%`).

### VFXX-H3 — capped uniform grids drew wrong areas [Fixed]

**Test:** “VFXX-H3: the shaded polygon area matches the event probability” — shoelace area of the
first `GeomPolygon` layer against `p*()`/`pt()` for every row of the audit table plus
`vdist_f_prob(5000, 4, 30)`, tolerance 1e-3.

**Fix:** `R/vdist-utils.R:115` `vdist_grid()`: a uniform 0.01 grid when the span needs ≤ 20,000 steps
(this is what ordinary plots use, so they are unchanged); otherwise 2,000 points equally spaced in
log-odds probability between the 1e-6 and 1-1e-6 quantiles, so the central mass stays dense.
Endpoints beyond that range are dropped by the grid and `vdist_pol_x()` (`R/vdist-plots.R:285`)
closes the polygon at the sampled edge instead of bridging from an empty far tail. Each family
clips its view to the 1e-9/1-1e-9 quantiles and keeps the drawn cutoff (`pc`) separate from the
captioned one (`R/vdist-data-prep.R:119, 179, 268, 303, 404, 430, 570`).

**Shoelace area of the dark polygon, HEAD → fixed (truth):**

| Call | HEAD | fixed | truth |
|---|---:|---:|---:|
| `vdist_t_prob(1e6, 7)` | 33.457342 | 1.0000000 | 1 |
| `vdist_normal_prob(1e6)` | 35.20655 | 0.99999972 | 1 |
| `vdist_chisquare_prob(1e6, 5)` | 3.2647639e-08 | 0.99999997 | 1 |
| `vdist_f_prob(1e5, 4, 30)` | 0.018176127 | 0.99996444 | 1 |
| `vdist_t_perc(.99999, 1)` | 1.1322405 | 0.99999938 | .99999 |
| `vdist_f_prob(5000, 4, 30)` | 0.97814934 | 0.99996444 | 1 (area .99996444) |

Captions are unchanged (`P(X < 1e+06) = 100%`, `P(X < 10.828) = 99.9%`); each case still allocates
≤ 2,800 rows. An ordinary plot keeps its 1,001-row grid (asserted).

### VFXX-H4 — chi-square boundary spike [Fixed]

**Test:** “VFXX-H4: chi-square grids respect the support and stay finite”.

**Fix:** `R/vdist-data-prep.R:78,143,167` and `R/vdist-utils.R:155`: chi-square grids start at 0 for
`df >= 2`, and at `max(qchisq(1e-6, df), .Machine$double.eps)` for `df < 2` where the density is
unbounded; those ranges are sampled with the probability grid so the near-zero singularity is
resolved by area instead of by a `.01` step. The same rule is applied to F (`num_df < 2`,
`R/vdist-data-prep.R:217,262,291`), whose `df(0, 1, 1)` is `Inf`.

**Before** (`vdist_chisquare_perc(.999, 1)`): grid `xmin = -6.07`, dark area **133,863.8**,
`max(y) = 1.34e7`.
**After:** grid `xmin = 1.570796e-12`, dark area **0.9990134** (truth .999), all polygon `y` finite;
`vdist_chisquare_perc(.95, 1)` → area **0.9500081**. Asserted for `df` .1, .5, 1, 1.9, 2, 5, 13:
`min(x) >= 0`, finite densities, and F `(1,1)`/`(1,3)` polygon/line data finite.

### VFXX-M1 — zero-variance normal overlay [Fixed]

**Test:** “VFXX-M1: the normal overlay uses the unrounded df and sd”.

**Fix:** `R/vdist-data-prep.R:91` — `dnorm(x, df, sqrt(2 * df))`; the summary text is formatted
separately in `R/vdist-plots.R:105,138,213` from the unrounded `df` via `fmt_num()`.

**Before:** `vdist_chisquare_plot(1e-8, normal = TRUE)` → `xlab "Mean = 0  Std Dev. = 0"`,
overlay `y` range `0 … Inf` (point mass).
**After:** `xlab "Mean = 0.00000001  Std Dev. = 0.0001414"`, overlay finite everywhere and
`max(y) = 2820.948 = dnorm(0, 1e-8, sqrt(2e-8))`; the whole overlay equals
`dnorm(x, 1e-8, sqrt(2e-8))`. For `df = 5` the label is still `Mean = 5  Std Dev. = 3.162`.

### VFXX-L1 — empty normal `perc` leaked an internal error [Fixed]

**Test:** “VFXX-L1: an empty perc gives a package error, not an internal one”.

**Fix:** `R/vdist-normal.R:101-103` — one clear check for `length(perc) < 1 || length(perc) > 2`
before any preparation; equal endpoints remain a valid (empty) interval.

**Before:** `vdist_normal_prob(numeric())` → `max()` warning then
`Error: missing value where TRUE/FALSE needed`.
**After:** `Error: perc must have one or two values.` and
`vdist_normal_prob(c(1, 1))` → `P(1 < X < 1) = 0%`.
`test-normal.R:17` updated from the old “more than 2 percentile values” string to the new message.

### VFXX-L2 — normal interval semantics undocumented [Fixed, website deferred]

**Fix:** `R/vdist-normal.R:8-11` documents `perc` as one value, or two in nondecreasing order for an
interval (two values select `type = "both"`); `man/vdist_normal_plot.Rd` regenerated. Binomial
counts/discrete tails were already documented in the matching roxygen/Rd text and are unchanged.
The committed pkgdown site (`docs/`) still shows 0.2.1 — rebuilding it is follow-up F1.

### VFXX-L3 — NEWS overstated the polygon resolution [Fixed]

**Fix:** the shaded-region bullet in `NEWS.md` now says shaded regions **that span fewer than 20 grid
steps** are drawn with 101 points and wider regions use the standard 0.01 step. The behaviour itself is unchanged
(`vdist_normal_plot(sd = .1)$layers[[2]]$data` still has 23 rows); only the claim was wrong. The
empty-input bullet now names `perc = numeric()` as a covered case.

## Snapshots

Exactly 3 of 39 SVGs changed, each an intended consequence of a fix:

| Snapshot | Why |
|---|---|
| `chisquare-plot-3` (`vdist_chisquare_plot(df = 5, normal = TRUE)`) | overlay line only: `dnorm(x, 5, sqrt(10))` instead of `dnorm(x, 5, 3.162)`; max coordinate delta 0.01 px (M1) |
| `chisquare-perc-lower` (`vdist_chisquare_perc(.165, 8, "lower")`) | grid now starts at 0 instead of `mean - 5 sd = -11.45`, so the x scale and its break lines change; the shaded mass is identical (H4) |
| `chisquare-perc-upper` (`vdist_chisquare_perc(.22, 13, "upper")`) | same, `mean - 5 sd = -12.50` (H4) |

The other 36 SVGs are byte-identical (verified by the suite: 0 snapshot failures apart from these
three, which were regenerated by deleting them and re-running the visual tests as `AGENTS.md`
prescribes).

## Commands run

```text
bash ~/.claude/bin/heavy-slot.sh pkg-vfx2 Rscript .vfx2-tmp/repro.R        # before/after evidence
bash ~/.claude/bin/heavy-slot.sh pkg-vfx2 Rscript .vfx2-tmp/proto.R        # grid prototypes
bash ~/.claude/bin/heavy-slot.sh pkg-vfx2 Rscript .vfx2-tmp/svgdiff*.R     # per-snapshot diffs
bash ~/.claude/bin/heavy-slot.sh pkg-vfx2 Rscript .vfx2-tmp/count.R        # fails-before counts
bash ~/.claude/bin/heavy-slot.sh pkg-vfx2 Rscript -e 'Sys.setenv(NOT_CRAN="true"); devtools::test(reporter="check")'
bash ~/.claude/bin/heavy-slot.sh pkg-vfx2 Rscript -e 'devtools::document()'   # then reverted
   DESCRIPTION/NAMESPACE/man/vistributions.Rd churn from roxygen2 8.1.1 (follow-up F5)
bash ~/.claude/bin/heavy-slot.sh pkg-vfx2 Rscript -e 'devtools::test(filter="visual", ...)'  # x4
rm tests/testthat/_snaps/visual/chisquare-{plot-3,perc-lower,perc-upper}.svg   # regenerate
git archive HEAD --format=tar -o .vfx2-tmp/src.tar && tar -xf … -C .vfx2-tmp/src
bash ~/.claude/bin/heavy-slot.sh pkg-vfx2 R CMD build --no-manual .
bash ~/.claude/bin/heavy-slot.sh pkg-vfx2 R CMD check --as-cran --no-manual vistributions_0.2.2.tar.gz
```

## Test and check output

`devtools::test()` on the worktree with `NOT_CRAN=true` (all snapshots included):

```text
[ FAIL 0 | WARN 0 | SKIP 0 | PASS 389 ]
```

Fails-before, same new test file against `6d26c7d` code (`git stash push -- R/`):

```text
6 blocks, failed expectations: 38, blocks ending in an error: 2, passed: 0
failing/erroring blocks: VFXX-H1, VFXX-H2, VFXX-H3, VFXX-H4, VFXX-M1, VFXX-L1
```

With the fix the same file is `6 blocks, 97 passed, 0 failed, 0 errors`.

Clean `git archive HEAD` build + direct check of the tarball, run twice: once at `dad7416`
(the last code/docs commit) and again with this report committed — identical output; this commit
adds only the `.Rbuildignore`d report, so the tarball is otherwise unchanged:

```text
* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Aravind Hebbali <aravindhebbali@rsquaredacademy.com>'
New maintainer:
  Aravind Hebbali <aravindhebbali@rsquaredacademy.com>
Old maintainer(s):
  Aravind Hebbali <hebbali.aravind@gmail.com>
...
* checking examples ... [15s] OK
* checking tests ...
  Running 'testthat.R' [27s] [28s] OK
* checking re-building of vignette outputs ... [30s] OK
Status: 1 NOTE
tests: [ FAIL 0 | WARN 0 | SKIP 15 | PASS 350 ]
```

0 errors, 0 warnings, 1 NOTE (the already-known maintainer-address change; the audit's expected
NOTE). `git diff --check` clean. Nothing else in the tarball: `.vfx2-tmp/`, `.vfxx-tmp/`,
`docs-dev/` and `docs/` are not part of the `git archive` build.

## Follow-ups (not done in this round)

**F1 — rebuild the committed pkgdown site.** `docs/` still shows version 0.2.1, the old
parameter descriptions (including the new `perc` semantics) and no 0.2.2 NEWS. Run
`pkgdown::build_site()` from the final corrected candidate and commit `docs/`; it is
`.Rbuildignore`d, so CRAN check results are unaffected. Out of scope per the brief.

**F2 — view range for cutoffs beyond the sampled range.** When a cutoff lies beyond the
1e-9/1-1e-9 quantile range (e.g. `vdist_t_prob(1e6, 7)`), the sampled curve and the shaded polygon
are clipped to that range and the caption keeps the true cutoff, but the dashed reference line is
still drawn at the true cutoff, and `geom_vline()` therefore pulls the panel range out to it:
measured panel `-50041 … 1050003` against curve data `-38.7 … 38.7`. The shaded area and the caption
are correct (H3), but the rendered plot still shows the density as a spike at zero for such extreme
cutoffs. Re-anchoring the reference line and the percentage annotations at the clip edge would give
a usable view; that changes rendering (and possibly snapshots) beyond the audited numerical finding,
so it is a follow-up rather than part of this round. The same applies to `vdist_normal_prob(1e6)`,
`vdist_chisquare_prob(1e6, 5)` and `vdist_f_prob(1e5, 4, 30)`. The vfx3 round implements that
follow-up: reference lines, markers and annotations are clamped to the visible view while captions
keep the true event.

**F3 — `stats::qf()` underflow for very small probabilities.** `qf(1e-9, 1, 1)` returns exactly 0
in R 4.6.1, so `vdist_f_perc(1e-9, 1, 1)` captioned `P(X < 0) = 1e-07%`. The true quantile is
2.467401e-18 and *is* representable; only the `stats::qf()` result is wrong. The vfx3 round now
checks every `q*()` result against the requested tail and raises a package error when it does not
invert the CDF; F probabilities below 1e-6 are outside the documented contract.

**F4 — F normal overlay still uses the rounded mean/sd.** `fplot_data_prep()` built
`dnorm(nx, fm, fsd)` from `round(…, 3)` values. The VFXX2 re-audit showed that this *can* round a
valid large-df sd to zero (for example `vdist_f_plot(1e8, 1e8, normal = TRUE)`); the vfx3 round now
uses the unrounded moments and omits the overlay with a message when `den_df <= 4`.

**F5 — `RoxygenNote`/roxygen2 version mismatch.** The installed roxygen2 is 8.1.1 while
`DESCRIPTION` says `RoxygenNote: 7.3.3`; `devtools::document()` therefore rewrote `NAMESPACE`
formatting, `DESCRIPTION` and `man/vistributions.Rd` (adds an Authors block). Those unrelated
changes were reverted and only the intended `man/vdist_normal_plot.Rd` update kept. Decide whether
to adopt roxygen2 8.1.1 (regenerate everything, bump `RoxygenNote`) before release.

**F6 — scratch directories.** This round's scratch scripts, source copy, tarball and check
directory under `.vfx2-tmp/` were deleted after the report was written; the remaining
`.vfx2-tmp/verify/` subtree and the `.vfxx-tmp/` + `docs-dev/vfxx-reaudit.md` audit files are not
mine — they were created/left by other processes and were not touched. None of them is tracked or
part of any build.

## Independent re-verification (second runner, same worktree)

A second agent on this machine re-ran the checks after the two code commits, without changing package
code, R sources, tests or snapshots. Results:

- `devtools::test()` with `NOT_CRAN=true` on the final tree:
  `[ FAIL 0 | WARN 0 | SKIP 0 | PASS 389 ]`.
- The new `tests/testthat/test-vfxx-findings.R` was also run against a `git archive` of `6d26c7d`:
  **38 failed assertions + 2 erroring blocks** (VFXX-H1, H2, H3, H4, M1, L1), 44 guard expectations
  passing — the fails-before claim above is reproduced.
- `git diff --name-only 6d26c7d HEAD -- tests/testthat/_snaps/visual` lists exactly the three SVGs in
  the Snapshot table; the other 36 of 39 are byte-identical.
- The audited values were re-measured from the committed tree: H1 `n = 3`, data `0:3`,
  `P(X = 2) = 0.375`, `s = 3` accepted; H2 captions `P(X < 1.645e-16) = 95%` and
  `P(X < 1.115e-26) = 5%` with `pnorm`/`pchisq` reproducing the printed events; H3 shoelace areas
  1.000000 / 1.000000 / 1.000000 / 0.999964 / 0.999999 / 0.999964 for the six audit rows; H4
  `vdist_chisquare_perc(.999, 1)` area 0.999013 with `min(x) = 1.57e-12` and no negative grid point;
  M1 overlay 0 … 2820.948 = `dnorm(0, 1e-8, sqrt(2e-8))`; L1 `perc must have one or two values.`
- Clean `git archive HEAD` build + `R CMD check --as-cran --no-manual` again on this machine:
  **0 errors, 0 warnings**, the maintainer-address NOTE, and — on a run started while the shared
  heavy-slot queue had four other jobs running — one extra NOTE of the standard form
  “Examples with CPU (user + system) or elapsed time > 5s” (`vdist_t_plot` 4.55 s user / 5.89 s
  elapsed, `vdist_normal_plot` 3.78 s / 5.07 s). User CPU stays below the threshold; that NOTE is
  machine load, not a package change. A final clean-archive check of the report commit itself
  (`636f5ba`, `checking examples ... [19s] OK`) returned **`Status: 1 NOTE`**, the maintainer NOTE
  only, which matches the run recorded above.
- CRAN-mode tests inside that check: `[ FAIL 0 | WARN 0 | SKIP 15 | PASS 350 ]`, vignette rebuild OK.
- F3 was reproduced independently (`qf(1e-9, 1, 1)` returns 0, `pf(0, 1, 1) = 0`; likewise
  `qchisq(1e-18, .1)`), and is left as a follow-up for the reason already given: a guard that
  rejects only genuinely unrepresentable quantiles needs a precision decision across the four
  families (a naive relative-error test also rejects correct values such as
  `pf(2.467e-12, 1, 1)`, which differs from `1e-6` by 8e-6 from rounding alone).

This report commit touches only `docs-dev/`, which is `.Rbuildignore`d, so the checked tarball
content is unchanged by it.

## Deferred (not fixed, not wrong results in the audited scope)

- Enormous-`n` binomial allocation remains a resource limit (no huge-n experiment attempted).
- `vdist_binom_perc()`'s `tp` endpoints stay at the intentionally narrower documented contract.
- The infinite F density at zero is mathematically correct. The vfx3 round rejects `num_df < 1`,
  whose support boundary cannot be represented at double precision, and starts accepted
  `num_df < 2` grids at `qf(1e-6, …)`, so no `Inf` enters polygon data.
- pkgdown website prose/runtime is unaudited beyond F1.
