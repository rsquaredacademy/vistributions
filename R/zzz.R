#' @import stats
.onAttach <- function(...) {

  if (!interactive() || runif(1) > 0.2) return()

  tips <- c(
    "Learn more about vistributions at https://github.com/rsquaredacademy/vistributions/.",
    "Use suppressPackageStartupMessages() to eliminate package startup messages.",
    "You might like our blog. Visit: https://blog.rsquaredacademy.com",
    "Check out all our R packages. Visit: https://pkgs.rsquaredacademy.com/."
  )

  tip <- sample(tips, 1)

  packageStartupMessage(paste(strwrap(tip), collapse = "\n"))

}
