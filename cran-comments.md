## Test environments
* local Windows 10 x64, R 4.5.2 ucrt
* GitHub Actions: ubuntu-latest (release, devel, oldrel), windows-latest (release), macOS-latest (release)

## R CMD check results

0 errors | 0 warnings | 1 note

* checking for future file timestamps ... NOTE
  unable to verify current time
  (Platform specific artifact on local Windows check)

## Resubmission / Patch Release Summary (v0.2.1)
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
