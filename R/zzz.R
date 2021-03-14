#' @import magrittr 
#' @import stats
.onAttach <- function(...) {

  if (!interactive() || runif(1) > 0.1) return()

  pkgs <- available.packages()
  
  cran_version <- 
    pkgs %>%
    extract("vistributions", "Version") %>%
    package_version()

  local_version <- packageVersion("vistributions")
  behind_cran <- cran_version > local_version

  tips <- c(
    "Learn more about vistributions at https://github.com/rsquaredacademy/vistributions/.",
    "Use suppressPackageStartupMessages() to eliminate package startup messages.",
    "You might like our blog. Visit: https://blog.rsquaredacademy.com",
    "Check out all our R packages. Visit: https://pkgs.rsquaredacademy.com/."
  )

  tip <- sample(tips, 1)

  if (interactive()) {
    if (behind_cran) {
      msg <- "A new version of vistributions is available with bug fixes and new features."
      packageStartupMessage(msg, "\nWould you like to install it?")
      if (menu(c("Yes", "No")) == 1) {
        update.packages("vistributions")
      } 
    } else {
      packageStartupMessage(paste(strwrap(tip), collapse = "\n"))
    }   
  }   

}
