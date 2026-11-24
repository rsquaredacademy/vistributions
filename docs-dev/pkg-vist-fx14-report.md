# Round 14 fixes for the Carbon verify findings (vist-fx14)

Branch `task/vist-carbonx`, worktree `F:/R/Packages/_worktrees/vist-carbonx`, starting point `c3757f6` (= release candidate `84a58a6` + `docs-dev/pkg-vist-carbonx-report.md`), `vistributions` 0.2.2, R 4.6.1 (2026-06-24 ucrt). Every item below was read from `docs-dev/pkg-vist-carbonx-report.md` and reproduced before the change.

Domain unchanged: continuous `probs` strictly inside `(1e-12, 1 - 1e-12)`; normal `sd / |mean| >= 1e-10`; displayed probabilities below `1e-10%` floored and near 100% bounded; binomial quantiles follow `qbinom()`. **H5 was left alone** (whole-number percent tolerance is correct at the displayed precision). The t and chi-square *parameter* limits gained a documented floor/maximum (M2.1/M2.2); no probability limit moved.

## Status table

| ID | Before (`c3757f6`) | After | Test |
| :--- | :--- | :--- | :--- |
| **M4.2** | `.onAttach()` = `if (!interactive() \|\| runif(1) > 0.2) return()`; the draw and the 20% `sample()` advanced `.Random.seed` with no save/restore. `identical(before, .Random.seed)` was `FALSE`. | The seed (if any) is saved before any draw and restored in `on.exit()`; a session that had no `.Random.seed` has none afterwards. `identical(before, .Random.seed)` is `TRUE`, and the next `runif(1)` is the one the caller's seed prescribes. | `M4.2` (7 expectations) |
| **L1.3** | Vignette: `- p: a single aggregated probability of multiple trials` / `- tp: the probability of success in a trial` (swapped). | `- n:` / `- p: the probability of success in each trial` / `- tp: the tail probability used to compute the quantile` / `- type:`; no other `p`/`tp` text is swapped in `vignettes/` or `README.Rmd`. | `L1.3` (6) |
| **H4.3** | `vdist_binom_prob(1000, .01, 1, "upper")` → `P(X >= 1) = 1.000` for `pbinom(0, 1000, .01, lower.tail = FALSE) = 0.9999568`. | `P(X >= 1) > 0.999`. An exact `1` still prints `= 1.000`; ordinary values (`0.850`, `0.017`) unchanged; the `> 99.9999999999%` and `< 1e-10%` bounds keep their rules. | `H4.3` (9) |
| **L1.1** | `vdist_t_perc(.95, df = .1, "lower")`: both text layers at `x = 21.21975` (0% of the panel apart) - the two labels overprint. | Labels at `19.52217` and `21.21975` on the transformed axis, 4.0% of the panel apart (data coordinates `3.0e8` and `1.64e9`); ordinary windows keep `pc ± 0.3` exactly (`vdist_t_perc(.95, df = 7)`: `1.594579` / `2.194579`). | `L1.1` (7) |
| **L1.2** | `vdist_chisquare_plot(df = 1, xaxis_range = 2)`: marker at `y = 0.1037769` = `dchisq(2, 1)` (`min(y)`); `df = .5` → `0.02270277`. | Marker at `y = 0` for `df = .5, 1, 2, 3, 5`, at `x = df`. `df = 2` was also floating (`0.1839397`). | `L1.2` (12) |
| **M2.1** | `vdist_t_prob(1.5, df = .001)` → `only 0% of the event is inside the representable tails. Use a less extreme probability.`; `df = .01` → `only 99.81%`; `vdist_t_perc(.95, df = .001)` → `The t quantile for a probability of 0.95 cannot be represented ... use a less extreme probability`; `?vdist_t_plot` documented only `df > 0`. | `df must be at least 0.05 for the t distribution; smaller degrees of freedom cannot be plotted at double precision. Use df between 0.05 and Inf.` from all three exports. Help (`@param df`, details), `vdist_contract()` and NEWS document `df >= 0.05`; the floor itself plots `vdist_t_prob(1.5, .05)` (`56.1%`, `pt(1.5, .05) = 0.5605632`), `vdist_t_perc(.95, .05)` and `vdist_t_plot(.05)`. | `M2.1` (10) |
| **M2.2** | `vdist_chisquare_plot(3, xaxis_range = 1e100)` → `The chi-square plot area (3.2546708e+64) does not match the probability of its event (1) ... the density is too concentrated ...`. | `xaxis_range must not exceed 1e+10; a wider window cannot be drawn within the documented plotting contract. Use xaxis_range between 0 and 1e+10.` `1e10` still draws; `?vdist_chisquare_plot` documents `1e10`. | `M2.2` (5) |
| **H7.2b** | `vdist_chisquare_prob(13, df = 200, "lower")`: `x.range = [0, 33]`, `y.range = [0, 1.614214e-43]`, body (`dchisq(198, 200) = 0.02003074`) 41 orders of magnitude above the panel, `caption = NULL`. | **View widened** (option chosen): `x.range = [0, 280]`, `y.range = [0, 0.02603806]`, largest drawn density `0.0200307` (`0.999998` of `dchisq(198, 200)`), event mass `pchisq(13, 200) = 3.367343e-80` unchanged and inside the panel, so no "outside the plotted range" caption is needed; the subtitle keeps `< 1e-10%`. Views that already show the body are unchanged (`vdist_chisquare_prob(13, 5)` and `(13, 10)` stay `[0, 25]`). | `H7.2b` (7) |

## Details per item

### 1. M4.2 - startup tip is seed-neutral (`R/zzz.R`)

```r
if (!interactive()) return()

seed <- if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
  get(".Random.seed", envir = globalenv(), inherits = FALSE)
} else NULL
on.exit({
  if (is.null(seed)) {
    if (exists(".Random.seed", envir = globalenv(), inherits = FALSE))
      rm(".Random.seed", envir = globalenv())
  } else assign(".Random.seed", seed, envir = globalenv())
}, add = TRUE)

if (runif(1) > 0.2) return()
...
```

The `runif(1)` branch is unchanged in behaviour (20% of attaches show a tip), but the caller's stream is exactly as it was on every path, including the early `return()`. Verified with seeds 1, 2, 3, 42, 2024 (`identical()` `TRUE` each) and in a session with no `.Random.seed` (none created).

### 2. L1.3 - vignette `p`/`tp` (`vignettes/introduction-to-vistributions.Rmd`)

The Percentiles "Input" list now matches `?vdist_binom_plot`: `p` is the probability of success in each trial, `tp` the tail probability for the quantile. `grep` over `vignettes/` and `README.Rmd` found no other swapped description (the Probabilities list said "probability of success", which is correct and was left).

### 3. H4.3 - near-certain binomial tails (`vdist_fmt_binom_prob()`)

A three-decimal rounding of `0.9999568` is `1.000`, which asserts certainty for an event that can fail. The mirror of the low-tail floor is now applied at the high end: when the rounded text is exactly `1.000` while the value is below one, the caption prints the bound `> 0.999` (and `bprob_plot_build()` drops the `=` because the text starts with `>`). `P(X >= 1) > 0.999` is true at the printed precision; `p == 1` keeps `1.000`, and `> 99.9999999999%` / `< 1e-10%` are unchanged. Both tails gain the treatment (`P(X <= 999)` at the same inputs is exactly `1` in double precision and still prints `= 1.000`).

### 4. L1.1 / L1.2 - annotations (`vdist_label_pair()`, `cplot_data_prep()`)

* **L1.1.** A fixed 0.3 data-unit offset is 3% of an ordinary `df = 7` panel but 0.2% of the `df = 0.1` panel, which is drawn on the symmetric log axis (`pseudo_log_trans()`; raw window `[-1.64e9, 1.64e9]`, transformed `[-21.22, 21.22]`). `vdist_label_pair()` therefore places the two labels on the axis they are rendered on (transformed for a wide window) and, only when the clamped pair ends up closer than 2.5% of the view, widens it to 8% of the view. Ordinary windows return `pc ± 0.3` unchanged, so no ordinary t plot moved.
* **L1.2.** `point_data` in `cplot_data_prep()` is `data.frame(x = df, y = 0)` (the F plot already marks its mean that way). `min(y)` was `dchisq(max x)` for `df = 2` and the first finite density for `df < 2`, where the density is unbounded at zero.

### 5. M2.1 / M2.2 - documented parameter limits (`R/vdist-utils.R`, `R/vdist-t.R`, `R/vdist-chisquare.R`)

* **t `df` floor = 0.05.** Measured with a probe that runs `vdist_t_prob()` for `perc = -2, -1, -0.5, 0, 0.5, 1, 1.5, 2` and all four types and `vdist_t_perc()` for `probs = .05, .5, .95` and all three types:

  | df | failures |
  | :--- | :--- |
  | 0.03 | `perc .95` lower/upper: area `0.95000103` vs 1e-6 relative error |
  | 0.031 | `perc .95` lower/upper: area `0.95000097` |
  | 0.0315 | none |
  | 0.04 | none for .05/.5/.95 (still fails at `probs = .999`) |
  | 0.045 | none, including `probs = .999` |
  | 0.05 | none |

  The smallest `df` at which the ordinary probabilities `.05/.5/.95` plot is therefore about **0.0315** (0.031 fails by `1.02e-6` relative area error, 2% over the 1e-6 tolerance, so the empirical edge is not a safe contract boundary). The documented floor is **0.05**: it sits a factor 1.6 above the measured edge (well outside floating-point/platform variation) and is the smallest round value that also plots the `0.999` percentile. Values below it stop with a message naming `df` and the range, never the "less extreme probability" advice.
* **chi-square `xaxis_range` maximum = 1e10.** The area check still passes at `1e28` for `df = 0.1, 1, 200` and fails at `1e30` (`area 1.0000037` vs tolerance `1e-6`), so `1e10` leaves 18 orders of margin and is far above any chi-square body (its scale is `df`; the largest df in the test suite is `500` with `xaxis_range = 11`). Above it the error names `xaxis_range` and the limit instead of blaming a concentrated density.
* The contract table (`vdist_contract()`) now says `df >= 0.05` for `t`.

### 6. H7.2b - chi-square probability view (`cprob_data_prep()`)

**Choice: widen the view, not a caption.** The probability export already had a body-aware view rule for `perc >= 25`; the `perc < 25` branch hard-coded `hi = 25` (then `max(hi, perc + chisd)` = 33 at `df = 200`). A one-line rule reuses the rest of the machinery:

```r
if (hi < chim - 4 * chisd) hi <- chim + 4 * chisd   # view contains none of the body
```

then the existing cap (`hiq = qchisq(1 - 1e-9)`) applies. Widening is the simpler correct option because it needs no new caption type, keeps the existing area check (the event polygon is unchanged and wholly inside the new window, so `vdist_view_omitted()` is `0` and no disclosure is printed), and matches `vdist_chisquare_plot()` (H7.2a), which was fixed the same way. The alternative - keeping `[0, 33]` and adding "100% of the shaded region lies outside the plotted range" - would disclose a panel that still shows nothing but a far tail, so the reader would have to take the body on trust. Ordinary `df`s already contain the body and are untouched.

## Tests

New file `tests/testthat/test-vfx14-findings.R` (8 `test_that()` blocks, 63 expectations, helpers self-contained): seed identity for five seeds, the no-seed case and the next draw; vignette text; `> 0.999` plus its exact-1 and ordinary neighbours and the formatter's four branches; t label separation (lower and upper wide windows, ordinary placement unchanged); chi-square marker `y = 0` for five `df`s; t floor errors from all three exports, the message text and the floor itself; chi-square range message, `1e10` accepted, defaults accepted; H7.2b body visible at its true height, event mass preserved, ordinary `[0, 25]` views unchanged; and the help/NEWS checks for both limits.

One existing test was retargeted, not weakened: `test-vfx10-findings.R` `M08` used `vdist_t_perc(1e-10, .02)`, which is now rejected by the documented floor before the probability range is reached, so it checks the same probability-range message at a valid `df` (`vdist_t_perc(1e-13, 1)`) and additionally `probs`.

`devtools::test()`: **0 failures, 0 errors, 0 skips, 11525 expectations passed** (`visual: 7` vdiffr snapshots included).

**vdiffr snapshots changed: none.** The 7 committed SVGs (`binomial-plot`, `binomial-prob-exact/lower/upper/interval`, `binomial-perc-lower/upper`) use ordinary values and pass unchanged; the plots fixed in this round (t percentile labels, chi-square marker, chi-square probability view) are not snapshot cases, so `vdiffr::manage_cases()` was not needed.

## Check

```text
R CMD build F:/R/Packages/_worktrees/vist-carbonx          -> vistributions_0.2.2.tar.gz
_R_CHECK_FORCE_SUGGESTS_=false R CMD check --as-cran --no-manual vistributions_0.2.2.tar.gz
```

**0 errors, 0 warnings, 1 NOTE** - `checking CRAN incoming feasibility ... NOTE` with `New maintainer / Old maintainer(s)`, the CRAN-side record of the maintainer e-mail change already in 0.2.2; unrelated to this round and not reproducible locally in any other way.

`devtools::document()` was run with the installed roxygen2 8.1.1, which regenerated `DESCRIPTION` (`RoxygenNote` → `Config/roxygen2/version: 8.1.1`), reformatted the `importFrom(utils, ...)` block in `NAMESPACE` and added the new `Authors:` item to `man/vistributions.Rd`. Those three are the only incidental edits; `man/vdist_t_plot.Rd` and `man/vdist_chisquare_plot.Rd` carry the new limits.

## Commands run

Scripts under `C:\Users\DELL\AppData\Local\Temp\opencode\vist-fx14\`, from `F:\R\Packages\_worktrees\vist-carbonx`:

```text
Rscript 01_probe.R        # t df sweep, t perc/prob grid, chi-square xaxis_range sweep
Rscript 02_tfloor.R       # coarse t df search
Rscript 03_tfloor2.R      # t df search with the interval type fixed
Rscript 04_tfloor3.R      # df 0.030..0.100 for probs .05/.5/.95
Rscript 05_tfloor4.R      # df 0.0300..0.0320 and probs .999 (boundary and margin)
Rscript 06_limits.R       # chi-square range 1e20..1e35; t quantile messages at df >= 0.05
Rscript 07_after.R        # after-fix behaviour for all six items
Rscript 08_tlabel_dbg.R   # t percentile label/view coordinates
Rscript 09_evidence.R     # numbers quoted in this report
Rscript run_document.R    # devtools::document()
Rscript run_test.R        # devtools::test(), summary
```

## Wrong results left inside the domain

None known. The only deliberate restrictions are the new documented parameter limits (`t df >= 0.05`, chi-square `xaxis_range <= 1e10`), both stated in the help pages and NEWS, both above the smallest input that was found to work, and both rejected with a message naming the argument and its range.
