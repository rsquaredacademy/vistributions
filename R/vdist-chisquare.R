#' Visualize chi square distribution
#'
#' Visualize how changes in degrees of freedom affect the shape of
#' the chi square distribution. Compute & visualize quantiles out of given
#' probability and probability from a given quantile.
#'
#' @param df Degrees of freedom (at least 0.1 and at most 2e20; need not be a
#'   whole number). Above 2e20, \code{sqrt(2 / df)} is below 1e-10
#'   and the plotted range can no longer distinguish the mean from its standard
#'   deviation at double precision, so the value is rejected by name.
#' @param probs Probability value, strictly between 0 and 1 and within
#'   \code{(1e-12, 1 - 1e-12)}.
#' @param perc Quantile value.
#' @param type Lower tail or upper tail.
#' @param normal If \code{TRUE}, normal curve with same \code{mean} and
#' \code{sd} as the chi square distribution is drawn.
#' @param xaxis_range Upper X axis limit; by default the view spans four
#'   standard deviations around df, clipped at zero. An explicitly supplied
#'   value must not exceed \code{1e10}; this cap does not apply to the default
#'   body-centred window.
#' @param print_plot logical; if \code{TRUE}, prints the plot else returns a plot object.
#'
#' @details Percentages below 1e-10\% are displayed as < 1e-10\%, and
#' probabilities within 1e-10\% of 100\% as > 99.9999999999\%. A
#' probability below 1 that would print 100.0\% is displayed as the bound
#' > 99.9\%, the mirror of the floor; an exact 1 keeps 100.0\%.
#' Percentages at or above 0.1\% keep at least one decimal. Smaller
#' percentages use three significant digits in scientific notation, retaining
#' trailing zeros (for example \code{5.00e-02\%}). Values that round to
#' 0.1\% at three significant digits use \code{1.00e-01\%} on either side
#' of the notation switch. Cutoffs retain enough digits that the same event prints identical
#' percentage text in the percentile and probability forms, including at
#' halfway rounding boundaries. Extra percentage digits resolve exact halfway
#' values (for example \code{5.05\%}).
#'
#' @details
#' Drawing contract: every probability, cutoff and caption is computed from the
#' chi-square density, distribution and quantile functions, never from the
#' drawn grid. The shading polygon starts at the support (zero), not at a
#' quantile floor, so no probability is discarded for small \code{df}; it
#' spans the whole event and its own area is checked against the exact event
#' probability (within 1e-6 relative error above the display floor). Where the density is unbounded at zero it is
#' capped on the display axis only: the source-coordinate area and the printed
#' probability always use the unclipped density. Captions use one rounding rule
#' and are true at the precision they print. A shaded region too narrow to
#' occupy a pixel at the plotted scale (for example the lower 5\% tail of
#' \code{df = 0.1}) is marked with a visible strip in the panel and disclosed in
#' the caption; its polygon and its checked area are unchanged.
#' A shaded region whose height is less than 0.4\% of the density peak is
#' disclosed as too small to display.
#' Every plotting grid has a bounded number of points, whatever the range or degrees of
#' freedom.
#'
#' A probability view keeps the body of the distribution: when the requested
#' cutoff would leave less than 20\% of the distribution inside the panel (for example
#' \code{vdist_chisquare_prob(13, df = 200)}), the window is widened to the mean
#' and a few standard deviations instead of showing a flat far tail.
#' An explicitly supplied \code{xaxis_range} is limited to \code{1e10};
#' wider explicit windows are rejected with an error naming \code{xaxis_range}.
#' \code{df} itself is limited to \code{2e20}, the point at which
#' \code{sqrt(2 / df)} is below \code{1e-10} and the default body-centred
#' window collapses to a single coordinate.
#' Probability labels are anchored to the cutoff line with a fixed absolute
#' gap, so their layout does not depend on the device size. A label that would
#' leave the panel (a cutoff close to an edge) is stacked on a second line
#' instead of being drawn across the cutoff. Labels are drawn above the
#' density, the cutoff lines and the markers, each on a small light patch in the
#' panel background colour, so the shaded fill cannot erase the text and a
#' cutoff line cannot cross it.
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
vdist_chisquare_plot <- function(df = 3, normal = FALSE, xaxis_range = NULL, print_plot = TRUE) {
	check_print_plot(print_plot)

  check_numeric(df, "df")
  check_scalar(df, "df")
  check_positive(df, "df")
  vdist_check_chisquare_df(df)
  default_range <- is.null(xaxis_range)
  if (default_range) xaxis_range <- df + 4 * sqrt(2 * df)
  check_numeric(xaxis_range, "xaxis_range")
  check_scalar(xaxis_range, "xaxis_range")
  check_positive(xaxis_range, "xaxis_range")
  if (!default_range) vdist_check_chisquare_range(xaxis_range)
  check_logical(normal)

  data <- cplot_data_prep(df, xaxis_range, if (default_range) max(0, df - 4 * sqrt(2 * df)) else 0)
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
	check_print_plot(print_plot)

  check_numeric(probs, "probs")
  check_scalar(probs, "probs")
  check_numeric(df, "df")
  check_scalar(df, "df")
  check_positive(df, "df")
  vdist_check_chisquare_df(df)
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
	check_print_plot(print_plot)

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
