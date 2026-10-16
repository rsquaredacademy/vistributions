# AGENTS.md

R package `vistributions`: visualizes normal, t, F, chi-square, and binomial distributions with ggplot2.

## Commands

Run inside an R session (no Makefile/scripts):

```r
devtools::test()        # run testthat suite
devtools::check()       # full R CMD check before finishing work
devtools::document()    # regenerate man/*.Rd and NAMESPACE from roxygen comments
pkgdown::build_site()   # rebuilds docs/ (committed to git)
```

## Branches / CI

- Default working branch is `develop`; CI (`R-CMD-check.yaml`) runs on both `master` and `develop`.
- CI also runs rhub and coverage workflows (`.github/workflows/`).

## Generated files — do not edit by hand

- `README.md` is generated from `README.Rmd` (knitr).
- `NAMESPACE` and `man/*.Rd` are roxygen-generated (`RoxygenNote 7.3.2`); edit roxygen comments in `R/` instead.
- `docs/` is committed pkgdown output but `.Rbuildignore`d; re-run `pkgdown::build_site()` rather than hand-editing HTML.

## Testing quirks

- Plot regression tests use **vdiffr** with committed SVG snapshots in `tests/testthat/_snaps/visual/`. All are `skip_on_cran()`. If a plot change is intentional, update snapshots via `vdiffr::manage_cases()` (interactive) or delete affected SVGs and let tests regenerate them.
- Every plotting function takes `print_plot = TRUE` by default: it prints the ggplot and returns it only via `print()`'s invisible value; pass `print_plot = FALSE` to get a clean plot object for testing.
- Error-message tests assert exact strings produced by the `check_numeric` / `check_positive` / `check_range` helpers in `R/vdist-utils.R`. Changing a validator message requires updating matching `expect_error(...)` calls.

## Code layout conventions

- Per distribution family: exported functions live in `R/vdist-<family>.R`; shared computation lives in `R/vdist-data-prep.R` as unexported `<x>plot_data_prep` / `<x>perc_data_prep` etc., paired with `<x>plot_plot_build` builders.
- Input validation uses the custom `check_*` helpers, not `stopifnot`.
- `vdist_launch_app()` only delegates to `xplorerr::app_vistributions()`; the shiny app code lives in the xplorerr package, not here.
