# Contributing to vistributions

Thank you for your interest in contributing to `vistributions`! This document provides guidelines for contributing code, documentation, and bug reports.

## Code of Conduct

Please note that this project is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree to abide by its terms.

## Reporting Bugs & Requesting Features

- Check existing [issues](https://github.com/rsquaredacademy/vistributions/issues) before opening a new one.
- For bug reports, include a minimal reproducible example (reprex) using the `reprex` package whenever possible.
- Include your R session information via `sessionInfo()`.

## Development Setup

1. Clone the repository and checkout the `develop` branch:
   ```sh
   git clone https://github.com/rsquaredacademy/vistributions.git
   cd vistributions
   git checkout develop
   ```

2. Open the project in R / RStudio and install required dependencies:
   ```r
   install.packages(c("devtools", "testthat", "vdiffr", "ggplot2", "pkgdown"))
   ```

3. Load the package interactively:
   ```r
   devtools::load_all()
   ```

## Development Workflow

- Default working branch is `develop`. Please submit pull requests targeting `develop`, not `master`.
- Run tests regularly:
  ```r
  devtools::test()
  ```
- Regenerate documentation and `NAMESPACE` from roxygen tags:
  ```r
  devtools::document()
  ```
  *Do not edit `NAMESPACE` or `man/*.Rd` files by hand.*
- Run full `R CMD check` before submitting:
  ```r
  devtools::check()
  ```
- Update `NEWS.md` with a concise description of your changes and reference issue numbers if applicable.

## Coding Standards

- Input validation: Use existing `check_*` helpers in `R/vdist-utils.R` (`check_numeric`, `check_positive`, `check_range`).
- Base R conventions: Use `call. = FALSE` for user-facing errors (`stop("message", call. = FALSE)`).
- Never introduce unprompted network calls in package code or startup hooks.
- Plotting functions: Support `print_plot = TRUE` by default, and return the `ggplot` object cleanly when `print_plot = FALSE`.
