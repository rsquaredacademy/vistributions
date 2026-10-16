## Test environments
* local Windows 10 x64, R 4.5.2 ucrt
* GitHub Actions: ubuntu-latest (release, devel, oldrel), windows-latest (release), macOS-latest (release)
* win-builder: Windows Server 2022 x64, R Under development (unstable) (2026-09-16 r90549 ucrt)

## R CMD check results

0 errors | 0 warnings | 2 notes

* win-builder (R-devel): Status OK (0 errors, 0 warnings, 0 notes).
* Local `rcmdcheck::rcmdcheck(args = c("--as-cran", "--no-manual"))` on Windows 10 x64 (R 4.5.2 ucrt): 0 errors, 0 warnings, 2 notes, both benign:
  * `checking for future file timestamps ... NOTE: unable to verify current time` — environmental (local Windows clock/NTP resolution), not a package issue.
  * `checking examples ... NOTE: Examples with CPU (user + system) or elapsed time > 5s` — the five distribution `*_plot()` examples take 7-17 s because they render ggplot2 graphics; expected, not a code defect.
* `urlchecker::url_check()`: all URLs are correct (12 checked).

## Submission Summary (v0.2.1)
This is a patch release fixing 19 identified correctness and edge-case bugs:
* Fixed normal distribution x-axis grid scaling.
* Fixed F-distribution adaptive bounds and variance division-by-zero guards.
* Fixed Student's t negative percentile sequence crash and annotation colors.
* Fixed chi-square polygon range bounds.
* Fixed binomial upper-tail and exact matching logic, and expanded quantile range.
* Added missing positive degrees of freedom validations.
* Removed unprompted network polling during package startup.
* Corrected pedagogical discrepancies in vignette.
* Updated test suite to testthat 3rd edition.
