#' Visualize t distribution
#'
#' Visualize how degrees of freedom affect the shape of t
#' distribution, visualize quantiles out of given probability and
#' probability from a given quantile.
#'
#' @param df Degrees of freedom, at least 0.05 (need not be a whole number).
#'   Very small degrees of freedom make the tails so heavy that an ordinary
#'   probability covers quantiles hundreds of orders of magnitude away, which
#'   cannot be plotted at double precision; values below the documented floor
#'   are rejected with an error naming \code{df}.
#' @param probs Probability value, strictly between 0 and 1 and within
#'   \code{(1e-12, 1 - 1e-12)}.
#' @param perc Quantile value. For \code{type = "interval"} and
#'   \code{type = "both"} the event is defined by \code{|perc|}, so a negative
#'   value gives the same result as its absolute value.
#' @param type Lower tail, upper tail, interval or both.
#' @param print_plot logical; if \code{TRUE}, prints the plot else returns a plot object.
#'
#' @details Percentages below 1e-10\% are displayed as < 1e-10\%, and
#' probabilities within 1e-10\% of 100\% as > 99.9999999999\%.
#'
#' @details
#' Drawing contract: every probability, cutoff and caption is computed from the
#' t density, distribution and quantile functions, never from the drawn grid.
#' The shaded polygon spans the whole event (clipped only by the panel) and is
#' sampled on a grid spaced in probability, with a fixed number of points; its
#' own area is checked against the exact event probability and kept within
#' 1e-6 relative error for events above the display floor. Captions use one rounding rule and are true at the precision they
#' print. A very wide window - for example a heavy tail with a small \code{df} -
#' is drawn on a symmetric log axis so the shaded event stays visible; the
#' printed probability and the area check always use the unclipped density.
#' A shaded region too narrow to occupy a pixel at the plotted scale is marked
#' with a visible strip in the panel and disclosed in the caption; its polygon
#' and its checked area are unchanged. Every plotting grid has a bounded number
#' of points, whatever the degrees of freedom or the cutoff.
#'
#' @return An object of class \code{ggplot} (invisibly if \code{print_plot = TRUE}).
#'
#' @family t visualization
#'
#' @examples
#' # Visualize a distribution, percentile and probability.
#' vdist_t_plot()
#' vdist_t_perc(probs = 0.95, df = 4, type = 'lower')
#' vdist_t_perc(probs = 0.35, df = 4, type = 'upper')
#' vdist_t_prob(2.045, 7, 'lower')
#' vdist_t_prob(1.6, 7, 'both')
#'
#' # Additional variants are exercised in the test suite.
#' \dontrun{
#' vdist_t_plot(6)
#' vdist_t_perc(probs = 0.69, df = 7, type = 'both')
#' vdist_t_prob(0.945, 7, 'upper')
#' vdist_t_prob(1.445, 7, 'interval')
#' }
#'
#' @seealso \code{\link[stats]{TDist}}
#'
#' @export
#'
vdist_t_plot <- function(df = 3, print_plot = TRUE) {
	check_print_plot(print_plot)

  check_numeric(df, "df")
  check_scalar(df, "df")
  check_positive(df, "df")
  vdist_check_t_df(df)

  data <- tplot_data_prep(df)
  plot <- tplot_plot_build(data, df)

  if (print_plot) {
    print(plot)
  } else {
    return(plot)
  }

}

#' @rdname vdist_t_plot
#' @export
#'
vdist_t_perc <- function(probs = 0.95, df = 4, type = c("lower", "upper", "both"), print_plot = TRUE) {
	check_print_plot(print_plot)

  check_numeric(probs, "probs")
  check_scalar(probs, "probs")
  check_numeric(df, "df")
  check_scalar(df, "df")
  check_positive(df, "df")
  vdist_check_t_df(df)
  check_probability(probs, "probs", "t")

  method  <- match.arg(type)
  data    <- tperc_data_prep(probs, df, method)
  plot    <- tperc_plot_build(data, probs, df, method)

  if (print_plot) {
    print(plot)
  } else {
    return(plot)
  }

}

#' @rdname vdist_t_plot
#' @export
#'
vdist_t_prob <- function(perc = 1.6, df = 7, type = c("lower", "upper", "interval", "both"), print_plot = TRUE) {
	check_print_plot(print_plot)

  check_numeric(perc, "perc")
  check_scalar(perc, "perc")
  check_numeric(df, "df")
  check_scalar(df, "df")
  check_positive(df, "df")
  vdist_check_t_df(df)
  method <- match.arg(type)

  # symmetric events (interval, both) are defined by |perc|: use it throughout
  if (method %in% c("interval", "both")) {
    perc <- abs(perc)
  }

  data <- tprob_data_prep(perc, df, method)
  plot <- tprob_plot_build(data, perc, df, method)

  if (print_plot) {
    print(plot)
  } else {
    return(plot)
  }

}
