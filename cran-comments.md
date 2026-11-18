## Test environments

* local: Windows 11 x64, R 4.6.1 (ucrt)
* Linux (Ubuntu): R 4.6.2 and R-devel (4.7.0)
* win-builder: R-devel and R-release
* GitHub Actions: ubuntu-latest (devel, release, oldrel-1), windows-latest
  (release), macos-latest (release)

## R CMD check results

0 errors | 0 warnings | 1 note

* `checking CRAN incoming feasibility ... NOTE`: the maintainer e-mail address
  changes from hebbali.aravind@gmail.com to
  aravindhebbali@rsquaredacademy.com. This is the same maintainer, Aravind
  Hebbali; the change will be confirmed from the old address.

The check was run on a tarball built from a clean `git archive` copy of the
release commit.

## Submission summary (0.2.2)

This release fixes wrong results found in a pre-release audit of 0.2.1:

* Non-integer degrees of freedom are no longer truncated (t, chi-square, F).
* Quantile cutoffs are not rounded before shading and reference lines are
  drawn.
* Quantiles outside the default plotting range no longer fail.
* Binomial functions reject non-integer counts and reversed intervals; the
  quantile caption and shading follow `qbinom()`.
* `vdist_t_prob()` with negative symmetric cutoffs uses `|perc|`
  consistently.
* Clearer errors for missing/vector arguments and for `probs` of 0 or 1.

See NEWS.md for details.

## Reverse dependencies

We checked 1 reverse dependency (xplorerr), comparing R CMD check results
with the CRAN version and this release of vistributions.

* We saw 0 new problems
* We failed to check 0 packages
