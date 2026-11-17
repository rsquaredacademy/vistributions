#' Visualize chi square distribution
#'
#' Visualize how changes in degrees of freedom affect the shape of
#' the chi square distribution. Compute & visualize quantiles out of given
#' probability and probability from a given quantile.
#'
#' @param df Degrees of freedom (at least 0.1; need not be a whole number).
#' @param probs Probability value, strictly between 0 and 1 and within
#'   \code{[1e-6, 1 - 1e-6]}.
#' @param perc Quantile value.
#' @param type Lower tail or upper tail.
#' @param normal If \code{TRUE}, normal curve with same \code{mean} and
#' \code{sd} as the chi square distribution is drawn.
#' @param xaxis_range The upper range of the X axis.
#' @param print_plot logical; if \code{TRUE}, prints the plot else returns a plot object.
#'
#' @details
#' Drawing contract: every probability, cutoff and caption is computed from the
#' chi-square density, distribution and quantile functions, never from the
#' drawn grid. The shading polygon starts at the support (zero), not at a
#' quantile floor, so no probability is discarded for small \code{df}; it
#' spans the whole event and its own area is checked against the exact event
#' probability (within 1e-3). Where the density is unbounded at zero it is
#' capped on the display axis only: the source-coordinate area and the printed
#' probability always use the unclipped density. Captions use one rounding rule
#' and are true at the precision they print. A shaded region too narrow to
#' occupy a pixel at the plotted scale (for example the lower 5\% tail of
#' \code{df = 0.1}) is marked with a visible strip in the panel and disclosed in
#' the caption; its polygon and its checked area are unchanged. Every plotting
#' grid has a bounded number of points, whatever the range or degrees of
#' freedom.
#'
#' @return An object of class \code{ggplot} (invisibly if \code{print_plot = TRUE}).
#'
#' @family chisquare visualization
#'
#' @examples
#' # visualize chi square distribution
#' vdist_chisquare_plot()
#' vdist_chisquare_plot(df = 5)
#' vdist_chisquare_plot(df = 5, normal = TRUE)
#'
#' # visualize quantiles out of given probability
#' vdist_chisquare_perc(0.165, 8, 'lower')
#' vdist_chisquare_perc(0.22, 13, 'upper')
#'
#' # visualize probability from a given quantile.
#' vdist_chisquare_prob(13.58, 11, 'lower')
#' vdist_chisquare_prob(15.72, 13, 'upper')
#'
#' @seealso \code{\link[stats]{Chisquare}}
#'
#' @export
#'
vdist_chisquare_plot <- function(df = 3, normal = FALSE, xaxis_range = 25, print_plot = TRUE) {

  check_numeric(df, "df")
  check_scalar(df, "df")
  check_positive(df, "df")
  vdist_check_chisquare_df(df)
  check_numeric(xaxis_range, "xaxis_range")
  check_scalar(xaxis_range, "xaxis_range")
  check_positive(xaxis_range, "xaxis_range")
  check_logical(normal)

  data <- cplot_data_prep(df, xaxis_range)
  plot <- cplot_plot_build(data, df, xaxis_range, normal)

	if (print_plot) {
	  print(plot)
	} else {
	  return(plot)
	}

}

#' @rdname vdist_chisquare_plot
#' @export
#'
vdist_chisquare_perc <- function(probs = 0.95, df = 3, type = c("lower", "upper"), print_plot = TRUE) {

  check_numeric(probs, "probs")
  check_scalar(probs, "probs")
  check_numeric(df, "df")
  check_scalar(df, "df")
  check_positive(df, "df")
  vdist_check_chisquare_df(df)
  check_range(probs, 0, 1, "probs")
  check_probability(probs, "probs", "chisquare")

  method <- match.arg(type)
  data   <- cperc_data_prep(probs, df, method)
  plot   <- cperc_plot_build(data, method, probs, df)

	if (print_plot) {
	  print(plot)
	} else {
	  return(plot)
	}

}

#' @rdname vdist_chisquare_plot
#' @export
#'
vdist_chisquare_prob <- function(perc = 13, df = 11, type = c("lower", "upper"), print_plot = TRUE) {

  check_numeric(df, "df")
  check_scalar(df, "df")
  check_positive(df, "df")
  vdist_check_chisquare_df(df)
  check_numeric(perc, "perc")
  check_scalar(perc, "perc")

  method <- match.arg(type)
  data   <- cprob_data_prep(perc, df, method)
  plot   <- cprob_plot_build(data, method, perc, df)

	if (print_plot) {
	  print(plot)
	} else {
	  return(plot)
	}

}
