# vistributions 0.2.2: percentage consistency (vist-fx16b)

Base: `db2f7ec` (fx16), branch `task/vist-fx10`, worktree
`F:/R/Packages/_worktrees/vist-fx10`. Read the package audit stop rule and
`F:/pm/notes/2026-11-18-pkgs/pkg-vist-rx15n-audit.md`, including section 5
High 1 and High 2. The supported input domain and display bounds are unchanged.
No branch switch, push, `gh`, or GitHub operation was used.

## Changes

1. Fixed the fx16 colour regression first: `vdist_annotate_labels()` maps
   `colour = I(colour)`. This preserves each row's literal colour without
   adding a plot-wide scale. Extracted `c63f7de` with `git archive` into a
   temporary directory and read the actual percentage text grobs: shaded
   labels are `#0000CDFF`, other labels `#6495EDFF`. fx16 rendered them as
   `#F8766DFF` / `#00BFC4FF`. A committed CSV fixture records c63f7de's
   label/colour pairs for all 21 continuous family/form/type combinations.
   Tests compare rendered grobs, including with an independent layer using a
   manual colour scale.
2. Removed fx16's intermediate fixed-point branch below 0.1%. Scientific
   notation keeps three significant digits and trailing zeros (`5.00e-02%`,
   `2.00e-02%`, `1.00e-04%`). Its tolerance is half a unit at the documented
   three-digit precision, never inferred from a trimmed mantissa.
3. The notation switch uses the three-significant-digit rounded percentage.
   Values rounding to 0.1% on either side share `1.00e-01%`. This spelling
   makes the three-digit accuracy visible and prevents the near-boundary
   inverse-CDF roundoff from changing notation. It also prevents a 0.100%
   event from accepting a cutoff with probability 0.139%. Outside this narrow
   switch band, percentages above 0.1% retain the existing fixed-point rule
   with at least one decimal and extra digits at exact halfway boundaries.
4. The shared cutoff acceptance check now requires identical formatted
   percentage text as well as agreement within the rounding tolerance.
   This covers `vdist_fmt_cutoff()`, `vdist_fmt_value()`, both displayed
   labels of one-cutoff events, and the combined event and tails checked by
   `vdist_fmt_interval()`. Bounds still require the correct strict inequality.
   The default `vdist_fmt_value()` path derives the original displayed text
   when its caller does not explicitly supply it.
5. Updated roxygen details for all four continuous families, regenerated the
   four Rd files with `devtools::document()`, and updated NEWS.

No changes were made to `R/vdist-plots.R`, `R/vdist-data-prep.R`, or fx16's
render-time label placement. For all 13 named cases below, every built
non-text layer is exactly identical to fx16: densities, shading polygons,
reference lines, and marker data are unchanged. Only presentation precision
and label colours change.

## Named cases against base R

All function names below have prefix `vdist_` and return plots with
`print_plot = FALSE`. Base-R columns give percentages of the event at the
**printed** cutoff, using the appropriate lower/upper tail, not the unrounded
quantile. Before is fx16 (`db2f7ec`); after is this change.

| Percentile call | Before cutoff / text | Base R before (%) | After cutoff / text | Base R after (%) |
| --- | --- | ---: | --- | ---: |
| `f_perc(.001, 1e6, 1e6, "upper")` | `1.006 / 0.1%` | 0.139017785869 | `1.0061996 / 1.00e-01%` | 0.100001605456 |
| `f_perc(.0002, 1e6, 1e6, "upper")` | `1.007 / 0.02%` | 0.0243503872255 | `1.007105 / 2.00e-02%` | 0.0200113081635 |
| `f_perc(1e-6, 1e6, 1e6, "upper")` | `1.009552 / 1.00e-04%` | 0.000100049693332 | `1.009552 / 1.00e-04%` | 0.000100049693332 |
| `normal_perc(.0005)` | `-3.291 / 0.05%` | 0.0499159566382 | `-3.2905267315 / 5.00e-02%` | 0.0499999999986 |
| `f_perc(.0002, 500, 500, "upper")` | `1.374 / 0.02%` | 0.0197734008556 | `1.3736 / 2.00e-02%` | 0.0200172380029 |
| `t_perc(.001, 100)` | `-3.174 / 0.1%` | 0.0999185909913 | `-3.1737 / 1.00e-01%` | 0.100012347307 |
| `t_perc(.001, 5)` | `-5.893 / 0.1%` | 0.100032500025 | `-5.893 / 1.00e-01%` | 0.100032500025 |
| `normal_perc(.001, 100, 1)` | `96.91 / 0.1%` | 0.100078247661 | `96.91 / 1.00e-01%` | 0.100078247661 |
| `chisquare_perc(.001, 5)` | `0.2102 / 0.1%` | 0.0999854597174 | `0.2102 / 1.00e-01%` | 0.0999854597174 |
| `normal_perc(.0505, 10, 1)` | `8.36 / 5.05%` | 5.05025834741 | `8.3599751491 / 5.05%` | 5.05000000000 |
| `normal_perc(.9995, 10, 1)` | `13.291 / 99.95%` | 99.9500840434 | `13.2905267315 / 99.95%` | 99.9500000000 |
| `t_perc(.0505, 10)` | `-1.806 / 5.05%` | 5.05296551739 | `-1.80636010661 / 5.05%` | 5.05000000000 |
| `f_perc(.8005, 5, 10)` | `1.805 / 80.05%` | 80.0460532751 | `1.80519534127 / 80.05%` | 80.0500000000 |

The three switch pairs explicitly requested (`t_prob(-5.893, 5)`,
`normal_prob(96.91, 100, 1)`, `chisquare_prob(.2102, 5)`) now all show
`1.00e-01%`, identical to their percentile partners. The halfway cases'
old short cutoffs continue to describe their own events: respectively
`5.1%`, `100.0%`, `5.1%`, and `80.0%`. Percentile captions now print the
additional cutoff digits needed to round trip as `5.05%`, `99.95%`, `5.05%`,
and `80.05%` instead of assigning these texts to the different short-cutoff
events.

For comparison with the audit's older c63f7de reproductions, that archive
printed `1e-01%` at F cutoff `1.006`, `2e-02%` at `1.007`, and `1e-04%`
at `1.0096` (base R 0.0000889223859561%). fx16 had already corrected the
last of those; this change preserves its three-digit result.

## Regression coverage and validation

`test-vfx16b-formatting.R` adds four test blocks with **1,390 passing
expectations** in the focused run, zero failures, errors, warnings, or skips:

- Formatter text, three-digit tolerance, both sides of the notation switch,
  the named halfway cases, and concentrated F upper-tail cutoffs.
- **297 round trips** (594 exported plot calls): normal; t df 1, 5, 30, 100;
  chi-square df 1, 5, 30; F (5,10), (500,500), (1e6,1e6); all supported
  percentile types; probabilities `1e-6, 2e-4, 5e-4, .001, .0505, .05, .5,
  .8005, .95, .9995, .999`. The actual subtitle cutoff is fed to the matching
  probability export (t central `both` maps to probability `interval`).
  Each pair has identical percentage text. Base-R CDFs, or independent
  density quadrature for central intervals, verify the literal printed
  precision including trailing zeros. All percentage labels also match the
  formatter applied to their printed event's base-R probability.
- Rendered colours compared with the c63f7de fixture for normal/t/chi-square/F,
  percentile/probability, lower/upper/both/interval wherever supported.
- An additional scratch check directly exercises `vdist_fmt_cutoff()` and
  the default `vdist_fmt_value()` path on five scientific/halfway cases:
  **10 checks passed**.
- Measured glyph boxes for the 13 named scientific/halfway cases at all
  three device sizes: **39 renders, zero overlaps and zero clipped labels**.

Existing expected strings updated: **4** (two fx16 fixed-point formatter
expectations, the decimal-comma caption assertion, and the central normal
interval cutoff assertion at probability .0505). The latter now expects
`P(-0.063334680678 < X < 0.063334680678) = 5.05%` because the old endpoints
`+/-0.06333` do not retain identical percentage text under the new rule.
There were **0 positive
assertions expecting trimmed scientific strings** on the fx16 starting tree;
the two existing guards against `1e-01%` and `5e-02%` remain. Snapshot changes:
**0** (the seven existing vdiffr snapshots are binomial).

Final `devtools::test()` result: **28 files, 204 test blocks, 23,570 passing
expectations; zero failures, errors, warnings, and skips**. This includes the
full fx16 rendered-label regression grid (440 plots at three sizes, 1,320
renders) and all 1,390 new round-16b expectations.

Final `devtools::check()` result: **Status: OK; zero errors, warnings, or
notes**, duration **17m 51.6s** (`--as-cran --no-manual`). The installed-package
test pass reports **23,564 passes, zero failures and warnings, and one expected
skip** because the vignette source Rmd is not installed. The rendered-label
grid also ran in that check (`NOT_CRAN=true`). All package, documentation, and
test files in the clean check copy match the staged worktree files byte for
byte; this excluded report is the only later edit.

The focused rerun of `test-vfx6-redesign.R` after updating its central-interval
cutoff assertion passed **376 expectations**, with zero failures, errors,
warnings, or skips.

The prescribed contact-sheet script rendered all **33 examples at 3 sizes**
(99 images, 4x3 / 7x5 / 10x6 inches). Opened all five sheet pages: no label
overprint. Output:
`C:/Users/DELL/AppData/Local/Temp/vist-fx16b/contact-sheet/`.

## Commands and artifacts

All R package work ran through `Rscript <script-file>` and
`pkgload::load_all(".")`. Scratch scripts, baseline archives, CSV/RDS captures,
and logs are in `C:/Users/DELL/AppData/Local/Temp/vist-fx16b/`.

- `git status --short`, `git log`, `git branch --show-current`.
- `git archive --format=tar ... c63f7de` and `git archive ... HEAD`;
  extraction with base R `utils::untar()` into isolated temporary directories.
- `Rscript <scratch>/cases.R <source> <output-prefix>` for each baseline and
  the final source; `Rscript <scratch>/extra-checks.R` for geometry and direct
  formatter checks.
- `Rscript <scratch>/document.R`: `devtools::document()`.
- `Rscript <scratch>/run-targeted.R`: focused test file; results in
  `targeted-results.rds`.
- `Rscript <scratch>/scientific-layout.R`: rendered glyph boxes for the
  named scientific/halfway cases at all three sizes.
- `Rscript <scratch>/test-cutoff-update.R`: `devtools::test(filter =
  "vfx6-redesign")` after the expected-cutoff update.
- `Rscript <scratch>/test.R`: `devtools::test()`; `test.log` and
  `test-results.rds`. An initial suite run was stopped after identifying the
  stale decimal-comma expected string. The first complete suite and check
  identified the stale central-interval cutoff assertion; only that expected
  string needed updating. Final totals use the subsequent complete reruns.
- `Rscript <scratch>/check-final.R`: `devtools::check(document = FALSE,
  error_on = "never", check_dir = <scratch>/check-dir-final)` on a clean archive
  copy with the changed files and new tests overlaid. Existing unrelated,
  untracked audit Markdown files are excluded from this source copy.
  Final results in `check-final.log`, `check-final-results.rds`, and
  `check-dir-final/`; the earlier run is retained in `check.log`.
- `Rscript F:/pm/notes/2026-11-18-pkgs/vist-contact-sheet.R <worktree>
  <scratch>/contact-sheet`.
- `git diff --check` and snapshot diff verification before committing.

R is 4.6.1 on Windows. Final validation shells use `LC_ALL=C`, `LANG=C` to
avoid inherited unsupported `C.UTF-8` startup locale warnings. No supported
inputs were removed and no out-of-domain work was added.
