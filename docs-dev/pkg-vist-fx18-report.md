# vistributions 0.2.2 — fix round 18

Starting tree: `2d6d85ecf6b7ba5b76a29ef008a133c8da99ea8c`, branch
`task/vist-fx10`. Read the package audit stop rule, including its real-render
gate, and both supplied round-17 audits. This round addresses M1 (crowded
axes), the caption clipping finding, and M2 (chi-square limit rationale).

## Changes

- `R/vdist-plots.R`: all 15 builders now use bounded x-axis breaks, targeting
  five intervals and allowing at most seven ticks. This includes the chi-square
  fixed steps, the t plot's `-4:4`, normal plot/default/prepared breaks, F, and
  the binomial integer axes. Count axes retain integer ticks and support `n = 0`.
  Cutoff markers, source polygons, scale transformations and coordinate limits
  are unchanged. Continuous axes omit breaks in the last 5% of the view to
  keep a right-edge tick's half-width on the device; count axes retain their
  existing bar expansion. All continuous and binomial disclosure captions wrap at 48
  characters, changing whitespace only.
- `R/vdist-utils.R`: shared bounded break generation, including evenly spaced
  breaks in the existing pseudo-log coordinate system for wide t/F windows;
  shared caption wrapping. The chi-square cap's comment and rejection rationale
  now describe a conservative relative-resolution limit. The runtime cap remains
  `2e20` and the acceptance rule remains `sqrt(2 / df) >= 1e-10`.
- `R/vdist-chisquare.R`, regenerated `man/vdist_chisquare_plot.Rd`, and
  `NEWS.md`: remove the false double-precision-collapse rationale and describe
  the conservative limit. No label-placement, number-formatting or halo code
  from rounds 16, 16b or 17 was changed.
- `tests/testthat/helper-fx18-render.R` and
  `tests/testthat/test-vfx18-render.R`: reusable 334-call ordinary grid and
  device-space text-box checks, plus boundary-rationale arithmetic checks.
- Existing caption assertions in `test-vfx10-findings.R`,
  `test-vfx11-findings.R`, `test-vfx12-findings.R`, `test-vfx16-findings.R`, `test-vfx5-findings.R`,
  `test-vfx6-redesign.R`, and `test-vfx7-findings.R` normalise whitespace before
  checking the same disclosure wording. Their numerical assertions are intact.
- `docs-dev/vist-fx18-validate.R`: reproducible isolated baseline capture,
  exact comparison and named PNG render/measurement commands.
- Seven regenerated binomial SVG snapshots (listed below), and this report.

## Automated real-device gate

The new axis test prints each plot on a real `ragg::agg_png` device, forces the
grid display list, and reads all tick and caption text grobs in their actual
parent viewports. Widths and heights come from device glyph metrics;
`deviceLoc()` includes viewport offsets. It checks both x and y ticks, adjacent
tick boxes on each axis, and all four device edges for ticks and captions.
It also checks the seven-tick x-axis ceiling and requires actual tick grobs.

Coverage: all 15 exports with defaults; chi-square df `.5, 1, 2, 5, 30, 200,
1000`; F `(1,5), (5,10), (500,500)`; t df `1,5,30`; normal lower/upper/both
and asymmetric intervals; binomial n `10,100,1000` with every documented event
type; probabilities `.01,.05,.5,.99`; and named audit cases, including M1 and
both caption reprexes. Total: **334 calls at 4x3, 7x5 and 10x6 inches = 1,002
ragg renders**.

| Device size (inches) | Calls rendered | Adjacent tick-label overlaps | Tick/caption boxes beyond any device edge |
| --- | ---: | ---: | ---: |
| 4x3 | 334 | 0 | 0 |
| 7x5 | 334 | 0 | 0 |
| 10x6 | 334 | 0 | 0 |

The axis block passed **4,108 assertions**; the cap-rationale block passed six.

The existing fx16 glyph-box and fx17 halo/z-order/occlusion tests run unchanged
as part of the full suite. No anchor-only check substitutes for rendered boxes.

## PNG evidence and visual self-check

All PNGs are retained under the **untracked, uncommitted** directory
`docs-dev/fx18-render/`.

The supplied contact-sheet script produced **185 plots × three sizes = 555
PNGs**, `contact/index.txt`, `contact/contact-sheet.png`, and
`contact/sheet-01.png` through `contact/sheet-24.png`. I opened and inspected
**every page, 01–24**, covering every row and all three size columns. X-axis
labels are separated; captions stay inside the images; singular-density labels
remain visible on their existing light halos. The documented stacked fallback
is retained. After the right-edge tick repair, regenerated the full contact
sheet and opened all 24 final pages again.

The following nine 150-dpi PNGs were also opened individually at full plot size;
their exact device glyph boxes are saved in `named/named-boxes.rds`:

| Call | PNGs under `docs-dev/fx18-render/named/` | X ticks | Adjacent overlaps / clipped tick or caption boxes |
| --- | --- | ---: | ---: |
| `vdist_chisquare_prob(13, 200, "lower")` | `chi_axis-4x3.png`, `chi_axis-7x5.png`, `chi_axis-10x6.png` | 6 at each size | 0 / 0 at each size |
| `vdist_f_prob(2.35, 500, 500, "upper")` | `f_caption-4x3.png`, `f_caption-7x5.png`, `f_caption-10x6.png` | 4 at each size | 0 / 0 at each size |
| `vdist_t_perc(.01, 30, "both")` | `t_caption-4x3.png`, `t_caption-7x5.png`, `t_caption-10x6.png` | 5 at each size | 0 / 0 at each size |

M1 now uses `0,50,100,150,200,250`, retaining window `[0,280]` and the cutoff
marker at `13`. The F disclosure occupies three lines and the t strip disclosure
two lines, with identical wording at every size. No information was removed.
This is the fixer's Codex self-check; it does not replace the independently
reviewed cross-family final release gate required by the stop rule.

## Exact comparison with 2d6d85e

Extracted the starting commit with `git archive` and loaded the baseline and
current package in separate Rscript processes. Captured the same 334 ordinary
calls, including all 15 exports, and compared plain text values, full-precision
built layer data, source data, geometry classes/parameters and x/y panel windows
using `identical()`. Caption whitespace alone is normalised. S7 class-registry
identity is stripped from the label container before comparing its text values.

| Quantity | Differences out of 334 calls |
| --- | ---: |
| Errors / rejected calls, baseline and current combined | 0 |
| Titles, subtitles, x/y labels and normalised captions | 0 |
| X and Y plot windows | 0 |
| Every built layer, including shading, curves, vlines, markers and percentage-label data | 0 |
| Source plot data | 0 |
| Geometry classes and fixed aesthetic parameters | 0 |
| X break vectors | 327, intended |

No numerical tolerance or rounding was used for this comparison. Evidence:
`docs-dev/fx18-render/base.rds`, `current.rds`, and `compare.log`. The baseline
archive and extracted package are also retained there.

## Tests, check and snapshots

`devtools::test(reporter = "summary")`, unfiltered, `NOT_CRAN=true`:
**29 files, 209 test blocks, 34,409 passed, 0 failed, 0 errors, 0 warnings,
0 skipped**. Sum of recorded test elapsed times: **1,864.40 seconds**.

The fx16 cutoff-anchored glyph-box block passed **8,658 assertions**; the fx17
halo/z-order/occlusion block passed **6,678 assertions**. The new ragg axis
block passed **4,108 assertions** over 1,002 renders (377.91 seconds), and the
chi-square limit-rationale block passed six. All seven vdiffr snapshots passed.
Evidence: `full-test-final-clean.log`, `full-test-results.rds`, and `totals.log`
under `docs-dev/fx18-render/`.

`devtools::check()` with `--as-cran --no-manual`, tests/examples/vignettes
enabled, and `env_vars = c(NOT_CRAN = "false")`: **Status: OK — 0 errors,
0 warnings, 0 notes**, duration **16 minutes 21.3 seconds**. Installed-package
tests: **14,952 passed, 0 failed, 0 warnings, 7 expected skips** (the six
`skip_on_cran()` render/visual test blocks and the installed-package guard for
unavailable vignette source). Its test phase took 873.68 seconds. Evidence:
`check-cran.log`, `check-results.rds`,
`vistributions.Rcheck/00check.log`, and
`vistributions.Rcheck/tests/testthat.Rout` under the untracked render directory.

The check uses an isolated source copy made from tracked package files plus the
two new test files. `docs/` and `docs-dev/` are already build-ignored. The six
pre-existing untracked top-level audit files were neither moved nor included in
the package being checked. The initial check was invalidated by inherited
`C.UTF-8` locale startup warnings on Windows, then restarted with
`LC_ALL=English_United States.utf8`, `LC_CTYPE=English_United States.utf8`, and
`LANG=en_US.UTF-8`. No package workaround was added for the local environment.
An exploratory axis run caught right-edge tick clipping after the break-count
repair; this prompted the 5% tick reservation. Its pre-fix results are retained
as `axes-before-endpoint-fix.log` and `.rds`. The final full suite and check were
restarted after that repair and after preserving the existing NULL-caption
fallback in a whitespace-only test update. Their authoritative logs are
`full-test-final-clean.log` and `check-cran.log`. The glyph/occlusion blocks
passed before the final restart; the additional restart normalised the old
near-certain-caption assertion, where wrapping separates `>` from `99.9%`.
The final check runs with `NOT_CRAN=false`, exercising the package's intended
CRAN skips; the complete direct suite runs with `NOT_CRAN=true` and exercises
all local render grids. No `--no-tests` or test-file filter is used in either.

The seven committed vdiffr snapshots changed solely because n=10 now labels
`0,2,4,6,8,10` instead of every integer, with the corresponding axis ticks and
major/minor grid lines:

- `binomial-plot.svg`
- `binomial-prob-exact.svg`
- `binomial-prob-lower.svg`
- `binomial-prob-upper.svg`
- `binomial-prob-interval.svg`
- `binomial-perc-lower.svg`
- `binomial-perc-upper.svg`

All are under `tests/testthat/_snaps/visual/`. Bars, probabilities, titles,
subtitles and plot windows are unchanged; none of these seven cases has a
caption. Snapshots were deleted and regenerated by the visual tests as allowed
by AGENTS.md, then verified by the full suite.
An XML comparison against the baseline also confirms byte-identical rectangle
elements and byte-identical text elements after removing the five discontinued
odd-count x tick labels from consideration.

## Commands run

Every R command was run from a script file; package scripts begin with
`pkgload::load_all(".")` or load the explicitly selected isolated tree. No
`Rscript -e` was used.

```powershell
git status --short
git rev-parse HEAD
git branch --show-current
Rscript docs-dev/fx18-render/focused.R
Rscript docs-dev/fx18-render/document.R
Rscript docs-dev/fx18-render/snapshots.R
git archive 2d6d85e -o docs-dev/fx18-render/base.tar
tar -xf docs-dev/fx18-render/base.tar -C docs-dev/fx18-render/base
Rscript docs-dev/vist-fx18-validate.R capture docs-dev/fx18-render/base docs-dev/fx18-render/base.rds
Rscript docs-dev/vist-fx18-validate.R capture . docs-dev/fx18-render/current.rds
Rscript docs-dev/vist-fx18-validate.R compare docs-dev/fx18-render/base.rds docs-dev/fx18-render/current.rds
Rscript F:/pm/notes/2026-11-18-pkgs/vist-contact-sheet.R F:/R/Packages/_worktrees/vist-fx10 F:/R/Packages/_worktrees/vist-fx10/docs-dev/fx18-render/contact grid
Rscript docs-dev/vist-fx18-validate.R render docs-dev/fx18-render/named
Rscript docs-dev/fx18-render/full-test.R
$env:LC_ALL='English_United States.utf8'; $env:LC_CTYPE='English_United States.utf8'; $env:LANG='en_US.UTF-8'
Rscript docs-dev/fx18-render/check-cran.R
Rscript docs-dev/fx18-render/totals.R
git diff --check
git commit -m "Bounded axis breaks, captions fit at 4x3, chi-square limit wording (vist-fx18)"
```

The initial focused driver ran `devtools::document()` before the first test;
the retained `document.R` reproduces that command. It regenerated only
`man/vdist_chisquare_plot.Rd`. The focused follow-up also reran the existing
zero-count binomial regression after fixing the new break helper's `n = 0` path.
`full-test.R` calls `devtools::test(reporter = "summary")` with `NOT_CRAN=true`.
`check-cran.R` calls `devtools::check(document = FALSE, args = c("--as-cran",
"--no-manual"), env_vars = c(NOT_CRAN = "false"), error_on = "never")` on the
source copy, with tests, examples and vignettes enabled and the package's CRAN
skips in effect. Raw logs and result RDS files
are retained under the untracked render directory.

## Backlog and follow-ups — unchanged

1. **Sub-unit t degrees of freedom round-trip guard:** the displayed
   `vdist_t_perc(.999, .05, "both")` cutoff `±1.14044e59` and rounded
   `vdist_t_perc(.001, .1, "lower")` cutoff `-1.60443e26` can exceed
   `vdist_t_prob()`'s documented magnitude guard on re-entry. No guard or
   supported-domain limit changed in this round.
2. **Two spellings near 0.1%:** `1.00e-01%` and `0.1%` retain their documented
   notation switch near 0.1005%. Formatting and the extra digits required at
   exact halfway cutoffs remain unchanged.
3. **Exotic 5e-7 probability band:** for probabilities in
   `(0.9995, 0.9995005)`, percentile and probability forms can retain different,
   mathematically true bound spellings (`> 99.9%` versus `= 99.9%`). The
   formatter and cutoff-matching rules were not changed.
4. **Stacking help-page wording:** the continuous help pages still omit the
   explicit statement that the second-line label can move to the side with room,
   and retain the broad device-size-independence wording. The existing absolute
   gap, stacking algorithm, side selection, z-order and halo are unchanged.
5. **Final release gate:** obtain the independent cross-family audit and the
   Claude contact-sheet review required by the stop rule before CRAN submission.
   This report records a completed fixer self-check, not that external approval.

No push, GitHub/gh operation or branch switch was performed. Render PNGs, raw
logs and baseline/check scratch trees are not part of the commit.
