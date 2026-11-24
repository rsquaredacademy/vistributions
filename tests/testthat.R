# testthat is in Suggests: a check installation without it must skip the suite
# instead of failing at `library(testthat)`.
if (requireNamespace("testthat", quietly = TRUE)) {
  library(testthat)
  library(vistributions)

  test_check("vistributions")
}
