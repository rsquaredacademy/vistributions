#' Visualize normal distribution
#'
#' Visualize how changes in mean and standard deviation affect the
#' shape of the normal distribution. Compute & visualize quantiles out of given
#' probability  and probability from a given quantile.
#'
#' @param mean Mean of the normal distribution.
#' @param perc Quantile value.
#' @param sd Standard deviation of the normal distribution.
#' @param probs Probability value.
#' @param type Lower tail, upper tail or both.
#' @param print_plot logical; if \code{TRUE}, prints the plot else returns a plot object.
#'
#' @examples
#' # visualize normal distribution
#' vdist_normal_plot()
#' vdist_normal_plot(mean = 2, sd = 0.6)
#'
#' # visualize quantiles out of given probability
#' vdist_normal_perc(0.95, mean = 2, sd = 1.36)
#' vdist_normal_perc(0.3, mean = 2, sd = 1.36, type = 'upper')
#' vdist_normal_perc(0.95, mean = 2, sd = 1.36, type = 'both')
#'
#' # visualize probability from a given quantile
#' vdist_normal_prob(3.78, mean = 2, sd = 1.36)
#' vdist_normal_prob(3.43, mean = 2, sd = 1.36, type = 'upper')
#' vdist_normal_prob(c(-1.74, 1.83), type = 'both')
#'
#' @seealso \code{\link[stats]{Normal}}
#'
#' @export
#'
vdist_normal_plot <- function(mean = 0, sd = 1, print_plot = TRUE) {

  check_numeric(mean, "mean")
  check_numeric(sd, "sd")
  check_positive(sd)

  data <- nplot_data_prep(mean, sd)
  plot <- nplot_plot_build(data, mean, sd)

  if (print_plot) {
    print(plot)
  } else {
    return(plot)
  }

}

#' @rdname vdist_normal_plot
#' @export
#'
vdist_normal_perc <- function(probs = 0.95, mean = 0, sd = 1, type = c("lower", "upper", "both"), print_plot = TRUE) {

  check_numeric(mean, "mean")
  check_numeric(sd, "sd")
  check_positive(sd)
  check_numeric(probs, "probs")
  check_range(probs, 0, 1, "probs")

  method <- match.arg(type)
  data   <- nperc_data_prep(probs, mean, sd, method)
  plot   <- nperc_plot_build(data, probs, mean, sd, method)

  if (print_plot) {
    print(plot)
  } else {
    return(plot)
  }

}

#' @rdname vdist_normal_plot
#' @export
#'
vdist_normal_prob <- function(perc = 3, mean = 0, sd = 1, type = c("lower", "upper", "both"), print_plot = TRUE) {

  method <- match.arg(type)

  if (length(perc) == 2) {
    method <- "both"
  }

  check_numeric(mean, "mean")
  check_numeric(sd, "sd")
  check_numeric(perc, "perc")
  check_positive(sd)

  if (length(perc) > 2) {
    stop("Please do not specify more than 2 percentile values.", call. = FALSE)
  }

  if ((method == "both") & (length(perc) != 2)) {
    stop("Specify two percentile values.", call. = FALSE)
  }

  data <- nprob_data_prep(perc, mean, sd, method)
  plot <- nprob_plot_build(data, perc, mean, sd, method)

  if (print_plot) {
    print(plot)
  } else {
    return(plot)
  }

}
