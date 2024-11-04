#' \code{vistributions} package
#'
#' Visualize probability distributions.
#'
#' @docType package
#' @keywords internal
#' @name vistributions
#' @aliases vistributions-package
"_PACKAGE"
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if (getRversion() >= "2.15.1") {
  globalVariables(c(
    ".", "df", "chi", "x", "y", "z", "x1", "x2"
  ))
}
