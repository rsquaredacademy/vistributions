#' \code{vistributions} package
#'
#' Visualize probability distributions.
#'
#' @docType package
#' @name vistributions
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    ".", "df"
  ))
}