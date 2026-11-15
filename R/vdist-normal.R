#' Visualize normal distribution
#'
#' Visualize how changes in mean and standard deviation affect the
#' shape of the normal distribution. Compute & visualize quantiles out of given
#' probability  and probability from a given quantile.
#'
#' @param mean Mean of the normal distribution.
#' @param perc Quantile value(s): a single value for a lower or upper tail, or
#'   two values in nondecreasing order for the interval between them. Two
#'   values select \code{type = "both"} automatically.
#' @param sd Standard deviation of the normal distribution. It must be positive.
#'   When \code{mean} is nonzero, \code{sd / abs(mean)} must be at least
#'   \code{1e-14}; below that limit the cutoff cannot be represented as a double
#'   and all three normal functions stop with a numerical-resolution error.
#'   Zero-centred distributions have no translation and may use any positive
#'   \code{sd}. The comparison is a ratio, so multiplying \code{mean} and
#'   \code{sd} by the same factor (a change of units, for example from metres to
#'   millimetres) never changes the result. Independently, the density must be
#'   finite (\code{sd} at least about \code{1e-308}) and the plotted range
#'   \code{mean +/- 4 sd} must be representable (\code{sd} below about
#'   \code{4e307}); both limits come from double precision and are reported by a
#'   specific error.
#' @param probs Probability value, strictly between 0 and 1 and within
#'   \code{[1e-10, 1 - 1e-10]}.
#' @param type Lower tail, upper tail or both.
#' @param print_plot logical; if \code{TRUE}, prints the plot else returns a plot object.
#'
#' @return An object of class \code{ggplot} (invisibly if \code{print_plot = TRUE}).
#'
#' @family normal visualization
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
  check_scalar(mean, "mean")
  check_numeric(sd, "sd")
  check_scalar(sd, "sd")
  check_positive(sd)
  vdist_check_normal_resolution(mean, sd)
  vdist_check_normal_range(mean, sd)

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
  check_scalar(mean, "mean")
  check_numeric(sd, "sd")
  check_scalar(sd, "sd")
  check_positive(sd)
  check_numeric(probs, "probs")
  check_scalar(probs, "probs")
  check_range(probs, 0, 1, "probs")
  check_probability(probs, "probs", "normal")
  vdist_check_normal_resolution(mean, sd)
  vdist_check_normal_range(mean, sd)

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
  check_scalar(mean, "mean")
  check_numeric(sd, "sd")
  check_scalar(sd, "sd")
  check_numeric(perc, "perc")
  if (length(perc) < 1 || length(perc) > 2) {
    stop("perc must have one or two values.", call. = FALSE)
  }
  if (any(!is.finite(perc))) {
    stop("perc must be finite numbers.", call. = FALSE)
  }
  check_positive(sd)
  vdist_check_normal_resolution(mean, sd)
  vdist_check_normal_range(mean, sd)

  if ((method == "both") && (length(perc) != 2)) {
    stop("Specify two percentile values.", call. = FALSE)
  }

  if ((length(perc) == 2) && (perc[1] > perc[2])) {
    stop("perc[1] must be less than or equal to perc[2].", call. = FALSE)
  }

  data <- nprob_data_prep(perc, mean, sd, method)
  plot <- nprob_plot_build(data, perc, mean, sd, method)

  if (print_plot) {
    print(plot)
  } else {
    return(plot)
  }

}
