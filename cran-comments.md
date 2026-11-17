## Test environments
* local Windows 11 x64, R 4.6.1 ucrt
* GitHub Actions / win-builder: to be added from the submission candidate before upload

## R CMD check results

`R CMD check --as-cran --no-manual` on the built 0.2.2 tarball (local Windows 11, R 4.6.1): 0 errors | 0 warnings | 1 note

* `checking CRAN incoming feasibility ... NOTE`: new maintainer e-mail address (aravindhebbali@rsquaredacademy.com; previous: hebbali.aravind@gmail.com). The change is intentional and will be confirmed from the old address.

## Submission Summary (v0.2.2)
This release fixes wrong results found in a pre-release audit of 0.2.1:
* Non-integer degrees of freedom are no longer truncated (t, chi-square, F).
* Quantile cutoffs are not rounded before shading and reference lines are drawn.
* Quantiles outside the default plotting range no longer fail.
* Binomial functions reject non-integer counts and reversed intervals; the quantile caption and shading follow `qbinom()`.
* `vdist_t_prob()` with negative symmetric cutoffs uses `|perc|` consistently.
* Clearer errors for missing/vector arguments and for `probs` of 0 or 1.
See NEWS.md for details.
