# vistributions 0.2.2 — fix round 16 (`pkg-vist-fx16`, DeepSeek)

Base: `c63f7de` on branch `task/vist-fx10` (PR #73), worktree
`F:/R/Packages/_worktrees/vist-fx10`. No branch switch, push, `gh` or GitHub
command was used. The supported-domain report is `F:/pm/notes/package-audit-stop-rule.md`;
the two gate audits read in full were `pkg-vist-rx15o-audit.md` and
`pkg-vist-rx15c-audit.md`.

## Summary

Items 1–7 are fixed. Ordinary-input label pairs are now anchored to their
cutoff with a fixed absolute gap rather than a data-unit offset; the
render-time placement is computed from the glyph widths in the panel viewport,
so it re-flows with the device. At 4x3, 7x5 and 10x6 in the full ordinary grid
(440 plots × 3 sizes, 1,320 renders) there are **0 label intersections, 0
clipped glyphs, 0 labels on the wrong side when they fit, and 0 labels that
cross their cutoff line**. A label that cannot fit on its own side is stacked
on a second line; this is the documented fallback and it is used only where a
cutoff is within roughly one label width of a panel edge.

All numbers, shaded masses, plot windows, subtitles and captions are unchanged
on a 596-call ordinary grid except the two intended text changes: the
percentage formatter near 0.1% (item 2) and one de-duplicated chi-square
caption (item 3). The comparison is below.

## Supported domain and display rules

The documented domain is unchanged except for the chi-square upper `df` bound
(item 5). Continuous `probs` remain in `(1e-12, 1 - 1e-12)`; t keeps its joint
small-`df` interval; normal keeps `sd / |mean| >= 1e-10`; F keeps
`num_df >= 1` and its concentration limit; chi-square is now `0.1 <= df <=
2e20`. The display rule is now stated consistently in all four continuous help
pages and NEWS:

- percentages at or above 0.1% keep one decimal (`84.0%`);
- percentages from 0.01% to 0.1% are printed in fixed point with three
  significant digits (`0.05%`, `0.011%`, `0.1%`);
- below 0.01% three significant digits are used in scientific notation
  (`5.00e-03%`, `3.16e-03%`);
- `< 1e-10%` and `> 99.9999999999%` remain the display bounds.

## 1. M1 + M2: cutoff-anchored label placement

### Design

Every continuous `_perc`/`_prob` builder now adds **one** label layer
(`vdist_annotate_labels()` in `R/vdist-utils.R`), not one `annotate("text")`
layer per label. The layer stores each label's cutoff coordinate in data units
and its intended side (`-1` left, `+1` right, `0` centre). Grid calls
`makeContent.vdist_labels()` while the panel viewport is current, so it can
measure each rendered glyph box with the device's own font metrics. Placement:

- left label: right edge at `cutoff - 2 mm`, `hjust = 1`;
- right label: left edge at `cutoff + 2 mm`, `hjust = 0`;
- centre label of `both`/`interval`: centred between the two cutoffs;
- if a label would leave the panel, it is moved to the other side of its
  cutoff at a lower line instead of being drawn across the cutoff. The line
  spacing is `max(6 mm, glyph height + 1.5 mm)`;
- labels on the same line are kept apart by an additional 1 mm clearance; a
  centre label that does not fit between its two cutoffs is moved down one
  line.

All builders were converted: normal, t, chi-square, F percentile and
probability forms, including `vdist_f_prob()` (which the audit found still on
the old fixed `± fsd` offset), the chi-square and normal probability forms, and
the three-label `both`/`interval` forms. The old `vdist_label_pair()` and
`vdist_annotate_text()` helpers are removed.

### Before/after rendered glyph boxes

Measured on a pdf device with the glyph boxes read in the drawn panel
viewport; gap is the minimum separation between the two (or three) glyph
boxes, so a negative number is an intersection. `stacked` is the number of
labels that could not fit on their own side.

| Case | Size | Labels | Before gap (mm) | After gap (mm) | After stacked |
| --- | --- | --- | ---: | ---: | ---: |
| t_perc default (0.95, 4) | 4x3 | 95.0% / 5.0% | -2.28 | 4.00 | 0 |
| t_perc default | 7x5 | 95.0% / 5.0% | 2.04 | 4.00 | 0 |
| t_perc default | 10x6 | 95.0% / 5.0% | 6.61 | 4.00 | 0 |
| t_perc .99 2 | 4x3 | 99.0% / 1.0% | -2.28 | 3.62 | 1 |
| t_perc .99 2 | 7x5 | 99.0% / 1.0% | -1.74 | 4.00 | 0 |
| t_perc .99 2 | 10x6 | 99.0% / 1.0% | 1.13 | 4.00 | 0 |
| f_perc .95 2 2 upper | 4x3 | 5.0% / 95.0% | -2.28 | 3.47 | 1 |
| f_perc .95 2 2 upper | 7x5 | 5.0% / 95.0% | -2.28 | 3.72 | 1 |
| f_perc .95 2 2 upper | 10x6 | 5.0% / 95.0% | -1.23 | 3.72 | 1 |
| f_perc .9 200 2 | 4x3 | 90.0% / 10.0% | -2.28 | 4.00 | 0 |
| f_perc .9 200 2 | 10x6 | 90.0% / 10.0% | -0.72 | 4.00 | 0 |
| f_perc .5 1 1 upper | 4x3 | 50.0% / 50.0% | 2.17 | 3.47 | 1 |
| f_perc .5 1 1 upper | 10x6 | 50.0% / 50.0% | 20.46 | 3.72 | 1 |
| t_perc .975 1 | 4x3 | 97.5% / 2.5% | 3.06 (wrong side) | 3.47 | 1 |
| t_perc .01 both | 4x3 | 49.5% / 1.0% / 49.5% | -2.28 | 3.47 | 0 |
| t_perc .01 both | 10x6 | 49.5% / 1.0% / 49.5% | -0.43 | 3.72 | 0 |
| chi_perc .95 1 upper | 4x3 | 5.0% / 95.0% | -2.28 | 3.62 | 1 |
| chi_prob .4549 1 | 4x3 | 50.0% / 50.0% | -2.28 | 3.62 | 1 |
| f_prob 4052 1 1 | 4x3 | 99.0% / 1.0% | -2.28 | 4.00 | 0 |
| f_prob 4052 1 1 | 10x6 | 99.0% / 1.0% | -2.28 | 4.00 | 0 |
| chi_prob 13 200 lower | 4x3 | < 1e-10% / > 99.9999999999% | -2.28 | 3.47 | 1 |
| chi_prob 13 200 lower | 10x6 | < 1e-10% / > 99.9999999999% | 0.65 | 3.72 | 1 |
| t_prob 0 5 both | 4x3 | 50.0% / < 1e-10% / 50.0% | -2.28 | 3.62 | 0 |
| t_prob 0 5 both | 10x6 | 50.0% / < 1e-10% / 50.0% | 12.81 | 3.72 | 0 |
| t_perc .05 5 both | 4x3 | 47.5% / 5.0% / 47.5% | -2.28 | 3.62 | 0 |
| t_perc .05 5 both | 10x6 | 47.5% / 5.0% / 47.5% | 0.87 | 3.72 | 0 |
| f_prob 2.35 500 500 | 4x3 | > 99.9999999999% / < 1e-10% | -2.28 | 3.47 | 1 |
| f_prob 2.35 500 500 | 10x6 | > 99.9999999999% / < 1e-10% | -2.28 | 3.72 | 1 |

Across the 28 named cases in the before/after table, the old tree had **49 of
84** (case, size) renders with a negative gap (24/28 at 4x3, 16/28 at 7x5,
9/28 at 10x6). The new tree has **0 of 84**, minimum gap **3.47 mm**, all
glyph boxes inside the panel, and no non-stacked label on the wrong side.
Seventeen of the 28 named cases need one stacked label at 4x3, 14 at 7x5 and 13
at 10x6; every one is a cutoff within about one label width of a panel edge
(for example F(2,2) upper at 0.0526, chi-square 13 at df 200, and t(2) .99).

The round's regression test (`tests/testthat/test-vfx16-findings.R`) covers the
requested ordinary grid: t df 1, 2, 4, 5, 30; F (1,1), (1,2), (2,2), (4,5),
(200,2) plus (500,500) for the pre-existing F(500,500) overprint; chi-square
df 1, 5, 30, 200; normal; all percentile and probability types; probabilities
.01/.05/.25/.5/.75/.95/.99; and every call in the two audits' M1/M2 tables. It
renders with `ggsave`-equivalent pdf devices at 4x3, 7x5 and 10x6 in and reads
the actual glyph boxes: 440 plots × 3 sizes, **0 failures, 0 overlaps, 0
clipped glyphs, 0 wrong-side labels, 0 centre labels off the midpoint**.

`vdist_t_perc()` at 4x3 now renders `95.0%` and `5.0%` with a 4.00 mm gap on
opposite sides of the line (the audit's `-2.53 mm` case), and
`vdist_f_prob(4052, 1, 1)` renders `99.0%` / `1.0%` with a 4.00 mm gap at every
size (the audit's fully superimposed pre-existing case).

## 2. L1: percentages near 0.1%

`vdist_fmt_pct()` now prints every percentage from 0.01% to 0.1% in fixed
point with three significant digits; below 0.01% it uses three significant
digits in scientific notation (previously `format(..., digits = 3)` trimmed
trailing zeros and produced one significant digit). The 0.1% boundary is no
longer decided by a one-ulp difference.

| Call | Before `c63f7de` | After |
| --- | --- | --- |
| `vdist_t_perc(.001, 5)` | `P(X < -5.893) = 1e-01%` | `P(X < -5.893) = 0.1%` |
| `vdist_t_prob(-5.893, 5)` | `P(X < -5.893) = 0.1%` | `P(X < -5.893) = 0.1%` |
| `vdist_chisquare_perc(.001, 5)` | `P(X < 0.2102) = 1e-01%` | `P(X < 0.2102) = 0.1%` |
| `vdist_chisquare_prob(0.2102, 5)` | `P(X < 0.2102) = 1e-01%` | `P(X < 0.2102) = 0.1%` |
| `vdist_fmt_pct(0.0005)` | `5e-02` | `0.05` |
| `vdist_fmt_pct(0.000123)` | `1.23e-02` | `0.0123` |
| `vdist_fmt_pct(0.00005)` | `5e-03` | `5.00e-03` |

The same event now prints the same text in the percentile and probability
forms. The new test asserts both documented pairs exactly.

## 3. L2: duplicated caption

`vdist_caption_text()` no longer adds the generic "the shaded region is too
small to display" clause when the blue sub-pixel strip is already present:
the strip sentence already says the region is too small to see and that it is
marked.

| Call | Before | After |
| --- | --- | --- |
| `vdist_chisquare_prob(1, 200)` | `The shaded region is too small to display; the shaded region is narrower than one pixel at this scale; it is marked by the blue strip` | `The shaded region is narrower than one pixel at this scale; it is marked by the blue strip` |
| `vdist_chisquare_prob(13, 200)` | `The shaded region is too small to display` | unchanged |

The comparison of the 596-call ordinary grid found exactly one caption change,
this case.

## 4. L3: documentation

The 0.4%-of-peak-height "too small to display" rule is now stated in the
normal, t, F and chi-square help pages (it was documented only for
chi-square). The label behaviour in NEWS is rewritten to describe what the
code does: cutoff-anchored, fixed 2 mm absolute gap, rendered glyph extents,
and the vertical-stack fallback at a panel edge. The help pages also state the
new percentage formatting. Files: `R/vdist-normal.R`, `R/vdist-t.R`,
`R/vdist-f.R`, `R/vdist-chisquare.R`, their generated `man/*.Rd`, and
`NEWS.md`.

## 5. Carbon Medium 2: chi-square `df` upper bound

`vdist_check_chisquare_df()` now rejects `df > 2e20` with a message naming
`df`, and `vdist_contract("chisquare")` and `?vdist_chisquare_plot` state
`0.1 <= df <= 2e20`. The bound is where `sqrt(2 / df) >= 1e-10` stops holding,
the same relative-resolution form as the normal `sd / |mean|` floor.

| Call | Before | After |
| --- | --- | --- |
| `vdist_chisquare_plot(df = 1e34)` | blank zero-area plot | `df must not exceed 2e+20 for the chi-square distribution: sqrt(2 / df) must be at least 1e-10 so the plotted range can distinguish df from its standard deviation.` |
| `vdist_chisquare_perc(.95, df = 1e30)` | failed blaming `probs` | same `df` error |
| `vdist_chisquare_prob(1, df = 1e30)` | same `df` error | same `df` error |

Ordinary `df` is unaffected: all chi-square grid calls (df 1, 5, 30, 200) and
their windows are identical to the baseline.

## 6. Carbon Low 3: scientific notation for huge formatted values

`vdist_fmt_interval()`, `vdist_fmt_cutoff()` and `vdist_fmt_value()` now skip
the fixed-point refinement loop when an endpoint has `|x| >= 1e15`, matching
`fmt_num()`.

| Call | Before | After |
| --- | --- | --- |
| `vdist_t_perc(.998, df = .05, "both")` | `P(-108760446760014833994088208200664482888242426286208264.000 < X < 108760446760074723680006022286424646860428046644448240.000) = 99.8%` | `P(-1.0876e+53 < X < 1.0876e+53) = 99.8%` |
| `vdist_t_prob(qt(.001, .05, lower.tail = FALSE), .05, "both")` | 50+ fixed digits | `P(|X| > 1.0876e+53) = 0.2%` |

## 7. Pre-existing overprints

The same placement covers the cases named in both audits:

| Call | Before | After |
| --- | --- | --- |
| `vdist_f_prob(4052, 1, 1)` | labels fully superimposed (`-2.28 mm` measured here; audit: complete overlap) | 4.00 mm gap, 4x3/7x5/10x6 |
| `vdist_chisquare_prob(13, 200, "lower")` | `< 1e-10%` and `> 99.9999999999%` overlap | vertical stack, 3.47–3.72 mm |
| `vdist_t_prob(0, 5, "interval"/"both")` | tail/centre overprint | centre on line 2, 3.62–3.72 mm |
| `vdist_f_prob(2.35, 500, 500)` | `> 99.9999999999%` on `< 1e-10%` | stacked, 3.47–3.72 mm |

## Numbers and shading are unchanged

A capture script built the same 596 continuous calls from an isolated
`git archive c63f7de` tree and from this worktree and compared:

| Quantity | Rows | Changed |
| --- | ---: | ---: |
| Subtitle numeric tokens | 596 | 0 |
| Subtitle text | 596 | 0 |
| Caption numeric tokens | 596 | 0 |
| Caption text | 596 | 1 (the item-3 de-duplication) |
| Label numeric tokens (multiset) | 596 | 0 |
| Dark-polygon area | 596 | 0 |
| x window (`xmin`, `xmax`) | 596 | 0 |

The 596 calls include all 440 regression-grid calls, the audit table cases, and
the F(500,500) and t `both` edge cases. The only text change is
`vdist_chisquare_prob(1, 200)` from item 3. This is the required proof that
this round touched placement and formatting only.

## Tests, snapshots and check

- `devtools::test()`: **27 files, 200 test blocks, 22,180 expectations
  passed; 0 failures, 0 errors, 0 warnings, 0 skips.**
- Focused round-16 file: 5 test blocks, **8,678** passing expectations
  (including 1,320 rendered plots), 0 failures.
- All seven vdiffr snapshots are binomial plots and are unchanged:
  `git diff --exit-code -- tests/testthat/_snaps/visual` exits 0. **Snapshots
  changed: 0.**
- `devtools::check()`: **0 errors, 0 warnings, 0 notes (`Status: OK`)**,
  duration 14m 28s, using `--as-cran --no-manual` on a clean source copy.
  The check's installed test pass reports 22,174 passes, 0 failures, 0
  warnings and 1 skip (the source-vignette assertion that skips when the Rmd
  source is not installed). The rendered-label grid also ran in the check,
  because `devtools::check()` sets `NOT_CRAN=true`.

Existing tests updated because the label layer is now a single
`GeomVdistLabels` layer with the labels in `layer$data` (instead of several
`annotate("text")` layers with labels in `aes_params`), and because labels are
anchored at the cutoff rather than offset in data units: `test-vfx14-findings.R`
(L1.1 placement), `test-bug-regressions.R` (TC-BUG-06/16 colour/position),
`test-vfx10-findings.R` (H06 label extraction, H08 axis labels),
`test-vfx11-findings.R` and `test-vfx12-findings.R` (label extraction), and
`test-vfx15-findings.R` (glyph measurement now unwraps `vdist_labels` and
measures 2D boxes). No expected percentage was changed except
`1.23e-02%` → `0.0123%` in the decimal-comma test (item 2).

## Contact sheet

Temp dir: `C:/Users/DELL/AppData/Local/Temp/vistfx16-contact`. The script
rendered all 33 help-page examples at 4x3, 7x5 and 10x6 in and wrote
`sheet-01.png` … `sheet-05.png` plus `index.txt`. I opened every page:
`sheet-01` (binomial 001–008), `sheet-02` (chi-square 009–016),
`sheet-03` (F/normal 017–024), `sheet-04` (normal/t 025–032), `sheet-05`
(t 033). No overprint and no non-stacked wrong-side label is visible. The two
cases that showed the round-15 bug on `c63f7de` are now correct: `#030`
(t_perc df 4) renders `95.0%` and `5.0%` on opposite sides with a 4 mm gap, and
`#031` (t_perc upper) likewise. Labels at the left/right edges of the normal
`both` and t `both` examples are fully inside their panels.

## Commands

All R work used `Rscript <script-file>` and `pkgload::load_all()`; no
`Rscript -e` with pipes or redirects was used for package work. Scratch
scripts, logs and captures are under
`C:/Users/DELL/AppData/Local/Temp/` and outside the repository.

- read stop rule, both audits, AGENTS.md and the implementation;
- `git rev-parse HEAD`, `git status`, `git archive c63f7de` into an isolated
  scratch tree;
- `devtools::document()` after the roxygen changes;
- focused `devtools::test(filter=...)` runs for vfx10–vfx16, then
  `devtools::test()`;
- `devtools::check()` on a clean source copy;
- `Rscript F:/pm/notes/2026-11-18-pkgs/vist-contact-sheet.R <worktree> <temp>`;
- capture/compare and glyph-box scripts described above.

## Backlog

No new in-domain finding. Out-of-domain backlog (unchanged, not release
blockers): t below `df = 0.05`; t with `0.05 <= df < 1` outside
`[.001, .999]`; explicit chi-square `xaxis_range > 1e10`; normal
`sd/|mean| < 1e-10`; continuous probabilities outside
`(1e-12, 1 - 1e-12)`; chi-square `df < 0.1`; F `num_df < 1` or the
concentration width below `.001`; normal scales outside the representable
window. `df > 2e20` for chi-square is now rejected in the documented domain and
moves from "silent blank" to a clear error, so it is not backlog.
