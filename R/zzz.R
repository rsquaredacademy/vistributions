#' @import stats
.onAttach <- function(...) {

  if (!interactive()) return()

  # A startup tip owns no randomness of the caller: the draw below runs on a
  # copy of the seed and the caller's .Random.seed is put back exactly as it was
  # (or removed again when the session had none) when the function exits, so
  # `library(vistributions)` never moves the random stream.
  seed <- if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
    get(".Random.seed", envir = globalenv(), inherits = FALSE)
  } else {
    NULL
  }
  on.exit({
    if (is.null(seed)) {
      if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
        rm(".Random.seed", envir = globalenv())
      }
    } else {
      assign(".Random.seed", seed, envir = globalenv())
    }
  }, add = TRUE)

  if (runif(1) > 0.2) return()

  tips <- c(
    "Learn more about vistributions at https://github.com/rsquaredacademy/vistributions/",
    "Use suppressPackageStartupMessages() to eliminate package startup messages.",
    "You might like our blog. Visit: https://blog.rsquaredacademy.com",
    "Check out all our R packages. Visit: https://pkgs.rsquaredacademy.com/"
  )

  tip <- sample(tips, 1)

  packageStartupMessage(paste(strwrap(tip), collapse = "\n"))

}
