# vistributions 0.2.2 — fix round 17 (`pkg-vist-fx17`)

- **Worktree / branch:** `F:/R/Packages/_worktrees/vist-fx10`, branch `task/vist-fx10` (PR #73).
- **Start point:** `4333045` (fx16 `db2f7ec` + fx16b `4333045`).
- **Environment:** Windows, R 4.6.1, PowerShell 5.1; every probe is `Rscript <script-file>` with `pkgload::load_all()`.
- **Inputs:** `pkg-vist-rx16n-audit.md` (Neon: Medium 1 release blocker + Lows), `pkg-vist-rx16o-audit.md` (Opus: M1 + Lows), `package-audit-stop-rule.md`.
- **Scope:** only the items in the fx17 brief. fx16's placement algorithm (cutoff-anchored, 2 mm gap, stacked fallback) and fx16b's formatting rules are unchanged.

## What changed

### 1. Labels are drawn last, on a background patch (Neon Medium 1, Opus M1)

- `R/vdist-plots.R`: every continuous builder (`cperc`, `cprob`, `fperc`, `fprob`, `nperc`, `nprob`, `tperc`, `tprob`) now ends with `vdist_labels_last(plot)`. The helper moves the single `GeomVdistLabels` layer to the end of the layer list, so the density curve, the polygons, the cutoff lines, the markers and the sub-pixel strip are all below the text. No geometry, colour, window or anchor changed.
- `R/vdist-utils.R`: `GeomVdistLabels` gains a halo. `makeContent.vdist_labels()` inserts a `roundrectGrob` immediately before each text grob in the same gTree, sized to the measured glyph box plus 0.7 mm padding, filled `#EBEBEB` (the default `theme_grey` panel background, `vdist_label_halo_fill`), no border, full opacity. The text can no longer be crossed by a cutoff line or sit unreadably on `#0000CD` fill, and the label colours stay `#0000CD` / `#6495ED`.
- The centre label of the three-label forms keeps its own row (below the tail labels) when it is wider than the gap between the cutoffs; the patch, not a new row, is what makes it readable where the two dashed lines cross it. Moving it above the tails would put it outside the panel top (`ytext = 0.97 * ylim`), so the documented fallback is unchanged.

### 2. Near-certain continuous probabilities print a bound (Opus L3)

- `vdist_fmt_pct()`: a probability below 1 that would print `100.0%` now prints `> 99.9` (value 1, tolerance 0.0005), the mirror of `< 1e-10` and the continuous counterpart of the binomial `> 0.999`. A probability exactly 1 keeps `100.0`. Values in `(1 - 1e-12, 1)` keep the deeper `> 99.9999999999` bound, and exact halfway events still print their extra digit (`99.95%`).
- `vdist_pct_matches()` reads the printed bound and requires the probability to be strictly beyond it, so a cutoff is accepted for the text it actually states.
- Effects: `vdist_normal_prob(3.5)` → `P(X < 3.5) > 99.9%` (99.97673%); `vdist_normal_prob(13.291, 10, 1)` → `P(X < 13.291) > 99.9%`; `vdist_t_perc(.001, .5)` caption → `> 99.9% of the shaded region lies outside the plotted range`. The same event prints the same text in `_perc` and `_prob`.

### 3. Zero-width t interval (Neon L3)

- `R/vdist-data-prep.R`: `tprob_data_prep(..., "interval")` stores both printed ends from `vdist_fmt_interval()`; the builder prints them directly, so `vdist_t_prob(0, 5, "interval")` → `P(0 < X < 0) < 1e-10%` instead of `P(-0 < X < 0)`.

### 4. Chi-square df bound wording (Neon L5)

- `R/vdist-chisquare.R` (roxygen, both places), regenerated `man/vdist_chisquare_plot.Rd`, and `NEWS.md`: “`sqrt(2 / df)` is below `1e-10`” (it is `sd / df`; the old text said “1e-10 of `df`”).
- The Rd and NEWS also document the near-certainty bound and the label ordering/patch (`R/vdist-{normal,t,f,chisquare}.R` roxygen).

## Tests

### Extended rendered-glyph test (`tests/testthat/test-vfx16-findings.R`)

- `vfx16_label_grobs()` replaces the old extents helper: it now measures both the text grobs and the halo patches in the panel viewport of a real pdf device, and `vfx16_extents()` filters to the text grobs, so the round-16 placement test is unchanged in what it checks.
- New round-17 block, `vfx17: labels are drawn last, over an opaque halo` (`skip_on_cran()`): ordinary grid of 98 plots — chi-square `df` 0.5/1/1.5 (`probs` .05/.5/.95, lower/upper, plus `_prob`), F `num_df` 1/1.5 with `den_df` 2/5/10 (`probs` .05/.9/.95/.99, lower/upper, plus `_prob` at .1/.01), the named round-16 reprexes and the three-label `both`/`interval` forms — at 4x3, 7x5 and 10x6 in (294 renders). Assertions per render:
  1. the label layer is the **last** layer (`inherits(geom, "GeomVdistLabels")`);
  2. children alternate halo/text, the halo is opaque (alpha 1), not `NA`, covers the glyph box and is strictly larger than it;
  3. 0 clipped labels, 0 label-on-label overlaps, 0 edge labels crossing their own cutoff (round-16 geometry retained);
  4. **occlusion:** every label box that contains vertices/edge midpoints of the dark `#0000CD` polygon (transformed with the plot's own `coord$transform`) has its halo. The test also asserts `dark_hits > 0`, so the occlusion assertion is exercised (the singular-`df` reprexes put labels on the fill) rather than vacuous.
- New text tests: near-certain bound (formatter, `_perc`/`_prob` agreement, exact 1, caption) and the zero-width interval.

### Ordinary-input regression vs `4333045`

`git archive 4333045` into a scratch tree; the same 668-call grid was captured in separate R processes from both trees: errors, titles, subtitles, captions, panel x/y windows, every non-label layer's built data (numeric to 10 significant digits, fills and colours) and the rendered label texts/colours.

| Quantity (668 ordinary calls) | Differences |
| --- | ---: |
| Errors / rejected ordinary inputs | 0 |
| Titles | 0 |
| Captions | 0 |
| Panel x / y windows | 0 |
| Non-label layer data (shading, curves, lines, markers) | 0 |
| Label texts + colours | 0 |
| Subtitles | **10** — the intended round-17 text only: `vdist_t_prob(0, df, "interval")` `P(-0 < X < 0)` → `P(0 < X < 0)` and `vdist_t_prob(0, df, "both")` `> 99.9999999999%` → `= 100.0%` (exact certainty), for `df` 1/2/5/30/100 |

No number, shading, window or label changed on ordinary inputs.

### Visual check

- `Rscript F:/pm/notes/2026-11-18-pkgs/vist-contact-sheet.R F:/R/Packages/_worktrees/vist-fx10 C:/Users/DELL/AppData/Local/Temp/opencode/fx17/sheet grid` — 185 plots × 3 sizes (`4x3`, `7x5`, `10x6`) = 555 PNGs, 24 contact-sheet pages (`sheet-01.png` … `sheet-24.png`) plus `index.txt`. Pages inspected: 1, 2, 4, 6, 7, 8, 12, 20, 24 (and individual full-size renders of 051_7x5, 056_4x3, 061_10x6, 092_4x3/7x5, 044_10x6, 153_10x6). Every label is readable: the dark spikes no longer erase text, the pills blend into the panel and cover the fill, and the three-label centre labels are crossed by no visible line.
- Neon reprexes rendered with `ggsave()` at 7x5, 10x6 and 4x3 in `C:/Users/DELL/AppData/Local/Temp/opencode/fx17/png` (39 PNGs): `f_perc(.95,1,5)`, `f_perc(.95,1,2)`, `chi_perc(.95,1)`, `f_prob(.1,1,5)`, `f_prob(.01,1,5)`, `f_perc(.90/.95/.99,1.5,5)`, `chi_perc(.95,0.5)`, `chi_perc(.95,1.5)`, `t_perc(.05,5,"both")`, `t_perc(.01,30,"both")`, `normal_prob(3.5)`. All labels are fully legible at both sizes; no truncation such as `"5.0%"` for `"95.0%"`.
- Regression capture, scripts and logs: `C:/Users/DELL/AppData/Local/Temp/opencode/fx17/` (`base/`, `capture.R`, `base.rds`, `new.rds`, `compare.R`).

### Suite totals

- `devtools::test()` is run in two parts because the rendered-glyph file alone draws ~4,300 plots on pdf devices (measured at ~25 min on this machine; running it a second time inside the same call would double the wall clock):
  - `test-vfx16-findings.R` (the extended glyph test): **7 tests, 15,376 expectations passed, 0 failed, 0 errors, 0 warnings, 0 skipped** (`NOT_CRAN=true`; 294 new renders on top of the 3,960 from round 16).
  - the remaining **27 files: 199 tests, 14,912 passed, 0 failed, 0 errors, 0 warnings, 3 skipped**.
  - Totals across the suite: **28 files, 206 tests, 30,288 expectations passed, 0 failures, 0 errors, 0 warnings, 3 skips**.
- Expectations updated for the intended round-17 text only: `test-vfx10-findings.R` (H07 caption, M02 distant cutoffs now `= 100.0%`), `test-vfx11-findings.R` (exact-certainty subtitles; the printed-bound helper now checks the printed bound rather than `1 - 1e-12`), `test-vfx12-findings.R` (near-one F probabilities are a bound; the interval cutoff text is two-element), `test-vfx15-findings.R` (halo children filtered when measuring glyph extents), `test-vfx14-findings.R` (label layer located by class, not by position), `test-vfx10-findings.R` (sub-pixel strip layer located by class).
- `devtools::check(document = FALSE, args = c("--as-cran", "--no-manual", "--no-tests"))`: **Status: OK — 0 errors, 0 warnings, 0 notes**, 3 min 29 s. The check's own test phase was skipped because the same suite ran directly above; an earlier run with `--tests` was not repeated after the last test edits. (A first check with the untracked round-15/16 audit `.md` files present produced the expected “non-standard top-level files” NOTE; those files are not part of this commit. The check was re-run with them temporarily moved aside: clean.)

### vdiffr

All 7 committed SVG snapshots are binomial plots, which have no `vdist_annotate_labels()` layer, so none changed (`git diff -- tests/testthat/_snaps` is empty). No snapshot was regenerated.

## Left as backlog (unchanged, not fixed in this round)

1. Chi-square `[0, 280]` x-axis break smear (`vdist_chisquare_prob(13, 200, "lower")`, 57 breaks at 4x3).
2. Long single-line captions clip on the left at 4x3 (`vdist_f_prob(2.35, 500, 500, "upper")`, `vdist_t_perc(.01, 30, "both")`).
3. Small-`df` `t` boundary cutoffs (`-1.60443e+26`, `±1.14044e+59`) are not accepted back by `vdist_t_prob()`.
4. Two spellings of 0.1% (`1.00e-01%` below 0.1005%, `0.1%` above it) — consistent across forms, documented.
5. Halfway cutoffs keep many digits (`-0.987268511004`) — documented rule.
6. A label whose cutoff is within one label width of the panel edge is still clamped flush against the edge (now under a clipped halo pill); the side change of the stacked fallback remains documented behaviour.
