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
#'   \code{3e-14}: below that limit the whole plotted window (\code{mean +/-
#'   4 sd}) is indistinguishable from a zero-width range at the display's own
#'   tolerance, every vertex lands on the same screen position, and all three
#'   normal functions stop with a numerical-resolution error that names this
#'   limit.
#'   A near-zero mean is not a resolution failure: the ratio may overflow to
#'   infinity (\code{mean = 1e-308, sd = 10}) while the distribution is still
#'   perfectly resolvable, and it is drawn normally. Zero-centred
#'   distributions have no translation and may use any positive
#'   \code{sd}. The comparison is a ratio, so multiplying \code{mean} and
#'   \code{sd} by the same factor (a change of units, for example from metres to
#'   millimetres) never changes the result. Independently, the density must be
#'   finite (\code{sd} at least about \code{1e-308}) and the plotted window
#'   \code{mean +/- 4 sd} must be representable: both its ends and its width
#'   must fit in a double (\code{sd} below about \code{2.2e307}). Both limits
#'   come from double precision and are reported by a specific error.
#' @param probs Probability value, strictly between 0 and 1 and within
#'   \code{[1e-10, 1 - 1e-10]}.
#' @param type Lower tail, upper tail or both.
#' @param print_plot logical; if \code{TRUE}, prints the plot else returns a plot object.
#'
#' @details
#' Drawing contract: every probability, cutoff and caption is computed from the
#' normal density, distribution and quantile functions, never from the drawn
#' grid. The shaded polygon spans the whole event and is sampled on a grid
#' spaced in probability and in standardised units; its own area is checked
#' against the exact event probability and kept within 1e-3. A percentage is
#' printed at a precision whose rounding interval contains the exact
#' probability, and the printed cutoffs reproduce that displayed percentage.
#' Because the normal is a location-scale family, multiplying \code{mean} and
#' \code{sd} by the same factor changes only the axis labels: the shaded
#' proportion, the probability caption and the standardised picture are the
#' same. A part of the shaded region that lies outside the plotted window is
#' disclosed in a plot caption. A shaded region too narrow to occupy a pixel at
#' the plotted scale (for example an extreme percentile of a concentrated
#' distribution) is marked with a visible strip in the panel and disclosed in
#' the caption; its polygon and its checked area are unchanged. Every plotting
#' grid has a bounded number of points, whatever the mean and standard
#' deviation.
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
	check_print_plot(print_plot)

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
	check_print_plot(print_plot)

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
	check_print_plot(print_plot)

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
