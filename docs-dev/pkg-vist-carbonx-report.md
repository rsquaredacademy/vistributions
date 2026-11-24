# Carbon `vistributions` findings verified on 0.2.2 RC (`84a58a6`)

Verification of every item in `docs-dev/carbon-vist-3ce6ec1.md` (Gemini audit of the old commit `3ce6ec1`) against this worktree: branch `task/vist-carbonx`, commit `84a58a6`, `vistributions` 0.2.2, R 4.6.1 (2026-06-24 ucrt), `pkgload::load_all(".")`.

Domain used for the classification (help pages + `NEWS.md`): continuous `probs` strictly inside `(1e-12, 1 - 1e-12)`; normal `sd / |mean| >= 1e-10` when `mean != 0`; displayed probabilities below `1e-10%` floored and within `1e-10%` of 100% bounded; binomial quantiles follow `qbinom()`; `t df > 0`, chi-square `df >= 0.1`, F `num_df >= 1`. The full test suite passes on this tree (`devtools::test()`, no failures).

## Status table

| ID | Status | Evidence on `84a58a6` |
| :--- | :--- | :--- |
| **H1** | **FIXED** | `n = 1e6, p = .5, s = 5e5`: dark bar mass `0.5003989422` = `pbinom(5e5,1e6,.5)`; `exact` mass `0.0007978844` = `dbinom(...)` (ratio 1, was 479x); `interval [4e5,6e5]` mass `1` = exact; `vdist_binom_perc(5000,.5,.5,"lower")` shaded `0.5056416137` = `pbinom(qbinom(.5,5000,.5)) >= .5`; upper shaded `0.4943583863` = `P(X >= 2501) <= .5`. Bins are now split at every event edge (`vdist_count_grid(n, brk)`). |
| **H2.1** | **FIXED** | `vdist_normal_prob(c(8.28,10),0,1,"both")` subtitle `P(8.28 < X < 10) < 1e-10%` (exact `6.158763e-15%`, below the documented floor); `c(8.5,10)` likewise `< 1e-10%`, no `0%`/false digits. `vdist_prob_between()` uses the conditioned CDF/survival difference. |
| **H2.2** | **FIXED** | Complement annotations evaluate the opposite tail directly and floor it: normal `perc = 8.28` lower → layer label `< 1e-10%` (exact `6.158764e-15%`); `vdist_f_prob(87.2698,5,30,"lower")` → `< 1e-10%` (exact `5.999965e-15%`); `vdist_t_prob(25.2615,20,"lower")` → `< 1e-10%` (exact `5.999953e-15%`); `vdist_chisquare_prob(85.47,5,"lower")` → `< 1e-10%` (exact `5.999531e-15%`). No `"0%"`. |
| **H3** | **FIXED** | The upper-tail closures now evaluate `lower.tail = FALSE` and the display value is passed to the formatter. `vdist_f_prob(1.001845,1e8,1e8,"upper")` → `P(X > 1.002) < 1e-10%`; the old caption attached `1.53e-18%` to `X > 1.002` (whose probability is `8.428028e-22%`), now the caption is a true bound. Above the floor digits are kept: `X > 1.00067238 = 3.89e-02%` (exact `0.03886295%`), `vdist_chisquare_prob(35.88819,5,"upper")` → `P(X > 35.888) = 1e-04%` (exact `1e-04%`). See note 1. |
| **H4.1** | **FIXED** | Single rounding from the exact value: `vdist_binom_prob(19,.4,3,"exact")` → `0.017` (`dbinom = 0.0174954`); `(10,.2,c(4,5),"interval")` → `0.115` (`0.1145045`). No double `round(...,5)` → `round(...,3)`. |
| **H4.2** | **FIXED** | `vdist_binom_prob(1000,.01,25,"upper")` → `P(X >= 25) = 4.2e-05` (base R `pbinom(24,1000,.01,lower.tail=FALSE) = 4.202921e-05`); no false `0`. Carbon's expected `3.272522e-05` is itself wrong: `sum(dbinom(25:1000,1000,.01))` is also `4.202921e-05`. |
| **H4.3** | **STILL PRESENT (cosmetic)** | `vdist_binom_prob(1000,.01,1,"upper")` → `P(X >= 1) = 1.000`, while base R gives `0.9999568`. `1.000` is the correct 3-decimal rounding (within half a unit of the last printed digit), but unlike the `< 1e-3` low tail this high tail has no scientific/bound display, so the caption asserts certainty for an event that can fail. |
| **H5** | **STILL PRESENT** | `vdist_f_perc(.80,200,200,"lower")` → `P(X < 1.127) = 80%`, while `vdist_f_prob(1.127,200,200,"lower")` → `P(X < 1.127) = 80.1%`; base R `pf(1.127,200,200)*100 = 80.06945`. Same at `df = 1e6`: `vdist_f_perc(.99,1e6,1e6)` → `P(X < 1.005) = 99%` vs `vdist_f_prob(1.005,1e6,1e6)` → `99.4%` (`pf(1.005)*100 = 99.36803`). Cause unchanged: `vdist_fmt_pct(0.80)` returns `text = "80", tol = 0.005` (0.5 percentage point) because the `.0` is stripped and `dec_shown = 0`, instead of `0.0005`. |
| **H6.1** | **FIXED** | `vdist_normal_perc(1e-10,0,1,"lower")`: 1 polygon, `xlim = [-7.348755, 7.348755]` includes `qnorm(1e-10) = -6.361341`, so the event is fully inside and no caption is needed (the old tree drew 0 polygons). `vdist_t_perc(1e-10,7,"lower")`: 1 polygon + `87.9% ... outside`. `vdist_normal_prob(-7)` / `vdist_t_prob(-53.9,7)` / `vdist_chisquare_prob(100,5,"upper")` / `vdist_f_prob(100,5,30,"upper")` each return 1 polygon with the correct disclosure (`7.8%`, `87.9%`, `> 99.9999999999%`, `> 99.9999999999%`). |
| **H6.2** | **FIXED** | `vdist_t_prob(-38.5,7,"lower")` caption `83.6% ... outside`; base R with the plotted `xlim = [-39.5,39.5]` gives `pt(-39.5,7)/pt(-38.5,7) = 0.8363017`. No division by the truncated mass. |
| **H7.1** | **FIXED** | Density cap only at a genuine support singularity: `vdist_t_perc(.999,1)` `y.range` top `0.4138029` = `dt(0,1)*1.3`; `vdist_t_perc(.95,.1)` top `0.1925198`; `vdist_chisquare_perc(.95,1e6)` top `0.0003667235` ≈ `dchisq(1e6-2,1e6)*1.3`; `vdist_f_perc(.95,1e6,1e6)` top `259.2646` ≈ `df(1,1e6,1e6)*1.3 = 259.31`. No 95–100% clipping. |
| **H7.2a** | **FIXED** | `vdist_chisquare_plot(200)` now spans `[120, 280]` (old fixed `[0,25]`), body at 200 visible; `vdist_chisquare_plot(1e6,1.01e6)` `y.range` top `3.667234e-4` against peak `dchisq(1e6-2,1e6) = 2.821e-4`. |
| **H7.2b** | **STILL PRESENT** | `vdist_chisquare_prob(13, df = 200, "lower")` still has `x.range = [0, 33]` and `y.range = [0, 1.614214e-43]`; the body is at 200 (`dchisq(198,200) = 0.02003`, 41 orders of magnitude above the panel top), fully off-screen, with `caption = NULL` and subtitle `P(X < 13) < 1e-10%`. The curve is now drawn correctly (2001 points) but the view still shows only a negligible far tail with no disclosure. |
| **M1.1** | **OUT OF DOMAIN** | The documented normal floor is now `sd / |mean| >= 1e-10` (`man/vdist_normal_plot.Rd:32`, `NEWS.md`). `mean = 1, sd = 1e-13` and `sd = 1e-14` are therefore outside the contract and are rejected with `sd / |mean| must be at least 1e-10 when mean is nonzero.`; `sd = 1e-10` plots. (In R 4.6.1 `qnorm(c(.025,.975),1,1e-13)` is `c(1,1)`, so Carbon's expected endpoints are not representable either.) |
| **M1.2** | **FIXED** | The old message blaming `"a probability of 0.05"` / `"the documented range is 1e-10 to 1"` is gone; the error now names the `sd / |mean|` ratio. |
| **M2.1** | **STILL PRESENT** | `t` has no documented `df` floor (`df > 0`, `?vdist_t_plot` says "whatever the degrees of freedom"), yet `vdist_t_prob(1.5, df = .001)` fails with `only 0% of the event is inside the representable tails. Use a less extreme probability.` (`pt(1.5,.001) = 0.5022709`). `vdist_t_prob(1.5, df = .01)` says `only 99.81% ...` and still stops; `vdist_t_perc(.95, df = .001)` says the `0.95` quantile "cannot be represented ... use a less extreme probability" although `0.95` is not extreme. Wording no longer says "density is too concentrated", but the valid-`df` crash and misleading advice remain. |
| **M2.2** | **STILL PRESENT** | `vdist_chisquare_plot(3, xaxis_range = 1e100)` still stops with `area (3.2546708e+64) does not match ... the density is too concentrated`; this is a trapezoid overflow, not a concentrated density. `xaxis_range = 1e10` now works, so the failure is confined to absurd-but-finite ranges. |
| **M2.3** | **FIXED** | `vdist_normal_prob(c(-1e308,1.5e308), mean = 1e308, sd = 1e307)` returns a plot; base R `pnorm(1.5e308,1e308,1e307) - pnorm(-1e308,1e308,1e307) = 0.9999997`. `vdist_normal_prob(1e308,1e308,1e306,"lower")` also works. |
| **M3** | **FIXED** | All 15 plotting exports return `print_plot must be a single nonmissing logical.` for `NA`, `c(TRUE,FALSE)`, `logical(0)`, `5`, `"yes"` and `NULL`; no raw `if` error, no silent numeric print. `vdist_binom_prob(10,.3,-1,...)`, `s = 11`, and interval `c(-1,2)` all return `s must be a whole number of successes between 0 and n (n = 10).`; `s = 0` still works. |
| **M4.1** | **FIXED** | `tests/testthat/test-visual.R:3` is a file-level `skip_if_not_installed("vdiffr")` before all 3 `test_that()` blocks; `tests/testthat.R` guards `testthat` with `requireNamespace()`. `devtools::test()` passes. |
| **M4.2** | **STILL PRESENT** | `.onAttach()` is `if (!interactive() || runif(1) > 0.2) return()`; when interactive it evaluates `runif(1)` before deciding, so `.Random.seed` advances on every `library(vistributions)` (and `sample(tips,1)` advances it further 20% of the time). It neither saves nor restores the seed. A copy of `.onAttach` with `interactive()` mocked to `TRUE` changes `.Random.seed` (`seed changed = TRUE`); `runif(1)` advanced the seed for seeds 1, 2 and 3 regardless of the 0.2 branch. |
| **L1.1** | **STILL PRESENT** | `vdist_t_perc(.95, df = .1, "lower")`: the two `GeomText` layers are both at `x = 21.21975` (`label = 95%` and `5%`), so they overprint on the wide/pseudo-log window. The F case (`vdist_f_perc(.95,3,30)`) is separated (`2.722277` vs `3.122277`). |
| **L1.2** | **STILL PRESENT** | `vdist_chisquare_plot(df = 1, xaxis_range = 2)`: the mean marker is at `y = 0.1037769 = dchisq(2,1)`, not `y = 0` (`cplot_data_prep()` still uses `point_data = data.frame(x = chim, y = min(y))`). Same for `df = .5` (`y = 0.02270277`). |
| **L1.3** | **STILL PRESENT** | `vignettes/introduction-to-vistributions.Rmd:125-127` still says `- p: a single aggregated probability of multiple trials` and `- tp: the probability of success in a trial`, the reverse of the help (`p` = probability of success in each trial; `tp` = tail probability). |
| **L1.4** | **FIXED** | Binomial captions put the inequality inside `P()`: `P(X <= 4) = 0.850`, `P(X >= 4) = 0.350`, `P(X = 4) = 0.200`, `P(4 <= X <= 6) = 0.340`. |

## Reprexes for STILL PRESENT items

All scripts call `pkgload::load_all(".")` from the worktree root and use `print_plot = FALSE`.

### H5 — whole-number percent tolerance is 10x too loose

```r
pkgload::load_all(".")

a <- vdist_f_perc(0.80, num_df = 200, den_df = 200, type = "lower", print_plot = FALSE)
b <- vdist_f_prob(1.127, num_df = 200, den_df = 200, type = "lower", print_plot = FALSE)
a$labels$subtitle   # "P(X < 1.127) = 80%"
b$labels$subtitle   # "P(X < 1.127) = 80.1%"

c <- vdist_f_perc(0.99, num_df = 1e6, den_df = 1e6, type = "lower", print_plot = FALSE)
d <- vdist_f_prob(1.005, num_df = 1e6, den_df = 1e6, type = "lower", print_plot = FALSE)
c$labels$subtitle   # "P(X < 1.005) = 99%"
d$labels$subtitle   # "P(X < 1.005) = 99.4%"

vistributions:::vdist_fmt_pct(0.80)   # text "80", tol 0.005
```

Base R: `pf(1.127, 200, 200) * 100 = 80.06945`, so the event printed by both captions (`X < 1.127`) has probability `80.1%`; the 80% cutoff is `1.126603`, which the percentile caption prints as `1.127`. `pf(1.005, 1e6, 1e6) * 100 = 99.36803` (`99.4%`); the 99% cutoff used by the package is `1.004664`, printed as `1.005`. `vdist_fmt_pct()` returns `tol = 0.5 * 10^-dec_shown / 100 = 0.005` when the formatted text drops `.0`, instead of `0.0005` for one displayed decimal. `vdist_fmt_cutoff()` therefore accepts a 3-decimal cutoff whose own probability rounds to a different displayed percentage.

### H7.2b — chi-square `df = 200` probability view is an empty far tail

```r
pkgload::load_all(".")
p <- vdist_chisquare_prob(13, df = 200, type = "lower", print_plot = FALSE)
ggplot2::ggplot_build(p)$layout$panel_params[[1]]$x.range   # 0 33
ggplot2::ggplot_build(p)$layout$panel_params[[1]]$y.range   # 0 1.614214e-43
p$labels$caption                                            # NULL
p$labels$subtitle                                           # "P(X < 13) < 1e-10%"
```

Base R: the distribution body is at `mean = df = 200` with `dchisq(198, 200) = 0.02003074`; the plotted window tops out at `dchisq(33, 200) = 1.241703e-43`, and `pchisq(33, 200) = 4.895933e-44`, so the whole panel is a flat 1e-43-high far tail and 100% of the body is off-screen. `vdist_chisquare_plot(df = 200)` was fixed and now spans `[120, 280]`; the probability export still hard-codes `hi = 25` for `perc < 25`.

### H4.3 — a 99.99568% binomial tail prints as certain

```r
pkgload::load_all(".")
vdist_binom_prob(1000, 0.01, 1, type = "upper", print_plot = FALSE)$labels$subtitle
# "P(X >= 1) = 1.000"
```

Base R: `pbinom(0, 1000, 0.01, lower.tail = FALSE) = 0.9999568`. `1.000` is the correct 3-decimal rounding (`round(0.9999568, 3) = 1`), so this is a display-policy issue rather than an off-by-a-digit error; the low-tail counterpart (`4.2e-05`) was moved to scientific notation, but the high tail still prints as certainty.

### M2.1 — valid `t` degrees of freedom with a misleading failure

```r
pkgload::load_all(".")
vdist_t_prob(1.5, df = 0.001, print_plot = FALSE)
vdist_t_prob(1.5, df = 0.01,  print_plot = FALSE)
vdist_t_perc(0.95, df = 0.001, type = "lower", print_plot = FALSE)
```

Actual:

```text
Error: The shaded part of the t distribution cannot be represented numerically:
only 0% of the event is inside the representable tails. Use a less extreme probability.
Error: ... only 99.81% of the event is inside the representable tails. Use a less extreme probability.
Error: The t quantile for a probability of 0.95 cannot be represented numerically;
use a less extreme probability (the documented range is 1e-12 to 0.999999999999).
```

Base R: `pt(1.5, 0.001) = 0.5022709`; `t` documents only `df > 0` (`?vdist_t_plot`), and the help promises a very wide window with a small `df` is drawn on a symmetric log axis. The message still gives probability advice to a function that takes a percentile, and describes `0.95` as extreme.

### M2.2 — absurd `xaxis_range` still blames a concentrated density

```r
pkgload::load_all(".")
vdist_chisquare_plot(3, xaxis_range = 1e100, print_plot = FALSE)
```

Actual:

```text
Error: The chi-square plot area (3.2546708e+64) does not match the probability of
its event (1) within 1e-6 relative error; the density is too concentrated for the
documented plotting contract.
```

`xaxis_range = 1e10` succeeds. The failure is trapezoid overflow on a window 1e100 wide, not a concentrated density; no documented `xaxis_range` maximum exists.

### M4.2 — `.onAttach()` advances `.Random.seed`

```r
pkgload::load_all(".")

# Exercise the real body with interactive() mocked only in a copied closure.
e <- new.env(parent = asNamespace("vistributions"))
assign("interactive", function() TRUE, envir = e)
onattach <- vistributions:::.onAttach
environment(onattach) <- e

set.seed(42)
before <- .Random.seed
invisible(capture.output(suppressMessages(onattach()), type = "message"))
identical(before, .Random.seed)   # FALSE
```

Base R/package: the first line is `if (!interactive() || runif(1) > 0.2) return()`; `runif(1)` is evaluated whenever `interactive()` is `TRUE`, before the 0.2 branch, and 20% of the time `sample(tips, 1)` advances the seed again. There is no `save`/restore. (Under `Rscript`, `interactive()` is `FALSE`, so the seed is untouched — the mutation is specific to interactive sessions, as Carbon stated.)

### L1.1 — overprinted `t` percentile annotations

```r
pkgload::load_all(".")
p <- vdist_t_perc(0.95, df = 0.1, type = "lower", print_plot = FALSE)
ggplot2::layer_data(p, 2)$x   # 21.21975
ggplot2::layer_data(p, 3)$x   # 21.21975
```

Both text layers (`95%` and `5%`) sit at the same data coordinate (`x = pc +/- 0.3` clamps to the same edge on this wide window), so they overprint.

### L1.2 — floating chi-square mean marker for `df < 2`

```r
pkgload::load_all(".")
p <- vdist_chisquare_plot(df = 1, xaxis_range = 2, print_plot = FALSE)
ggplot2::layer_data(p, 3)$y   # 0.1037769
```

Base R: `dchisq(2, 1) = 0.1037769`; the marker is at `min(y)`, not at the axis `y = 0`. `df = 0.5` gives `y = 0.02270277`.

### L1.3 — vignette swaps `p` and `tp`

```text
vignettes/introduction-to-vistributions.Rmd:125-127
- p: a single aggregated probability of multiple trials
- n: the number of trials
- tp: the probability of success in a trial
```

The help says `p` is "Probability of success in each trial" and `tp` is "Tail probability for quantile calculation". The vignette descriptions are swapped.

## Notes on other classifications

1. **H3 below the display floor.** At `1.001845` the exact upper tail is `1.534784e-18%`, below the documented `1e-10%` floor, so the caption now prints `< 1e-10%` for the rendered cutoff `1.002` (whose probability is `8.428028e-22%`). The bound is true at the printed precision; above the floor the cutoff keeps every digit it needs (`1.00067238`, `35.888`). The wrong number `1.53e-18%` attached to `X > 1.002` is gone.
2. **M1.** The audit was written against the old `sd / |mean| >= 1e-14` contract. This tree documents and enforces `1e-10`; the tested ratios (`1e-13`, `1e-14`) are outside it, and the error now names the ratio instead of blaming `probs`. In addition, `qnorm()` in R 4.6.1 returns `c(1, 1)` at `sd = 1e-13`, so the old "expected" endpoints cannot be produced by base R either.
3. **H4 base-R expectation in the audit.** Carbon's `pbinom(24,1000,.01,lower.tail=FALSE) = 3.272522e-05` is not reproducible: both it and `sum(dbinom(25:1000,1000,.01))` are `4.202921e-05`. The package prints `4.2e-05`.
4. **H7.2a vs H7.2b.** `vdist_chisquare_plot(df = 200)` is fixed, but the view rule in `cprob_data_prep()` for `perc < 25` was not changed, so the probability export still omits the body at `df = 200`.

## Commands run

All reprex scripts were files under `C:\Users\DELL\AppData\Local\Temp\opencode\vist-verify\`, run from `F:\R\Packages\_worktrees\vist-carbonx`:

```text
Rscript 01_h1_h4_binom.R
Rscript 02_h2_cancel.R
Rscript 03_h3_upper.R
Rscript 04_h5_perc.R
Rscript 04b_h5_probe.R
Rscript 05_h6_tails.R
Rscript 06_h7_density.R
Rscript 07_m1_m2.R
Rscript 08_m3_print_plot.R
Rscript 09_m4.R
Rscript 10_l1.R
Rscript 11_evidence.R
Rscript 12_h5_values.R
Rscript -e "devtools::test('.', reporter='summary')"
```

`devtools::test()`: all files pass (`visual: 7` expectations, no failures). Test guards inspected: `tests/testthat/test-visual.R:3` (`skip_if_not_installed("vdiffr")`), `tests/testthat.R` (`requireNamespace("testthat")`), `tests/testthat/test-vfx10-findings.R:343` (asserts the guard is present).

Any wrong result left inside the domain: yes
