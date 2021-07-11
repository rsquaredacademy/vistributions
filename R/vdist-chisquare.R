#' Visualize chi square distribution
#'
#' Visualize how changes in degrees of freedom affect the shape of
#' the chi square distribution. Compute & visualize quantiles out of given
#' probability and probability from a given quantile.
#'
#' @param df Degrees of freedom.
#' @param probs Probability value.
#' @param perc Quantile value.
#' @param type Lower tail or upper tail.
#' @param normal If \code{TRUE}, normal curve with same \code{mean} and
#' \code{sd} as the chi square distribution is drawn.
#' @param xaxis_range The upper range of the X axis.
#' @param print_plot logical; if \code{TRUE}, prints the plot else returns a plot object.
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
  check_logical(normal)

  cplot_data <- cplot_data_prep(df, xaxis_range)
  plot_base  <- cplot_plot_build(cplot_data, df, xaxis_range)

	if (normal) {
	  plot_base <- cplot_plot_modify(plot_base, cplot_data)
	}

	if (print_plot) {
	  print(plot_base)
	} else {
	  return(plot_base)
	}

}

#' @rdname vdist_chisquare_plot
#' @export
#'
vdist_chisquare_perc <- function(probs = 0.95, df = 3, type = c("lower", "upper"), print_plot = TRUE) {

  check_numeric(probs, "probs")
  check_numeric(df, "df")
  check_range(probs, 0, 1, "probs")

  method     <- match.arg(type)
  cperc_data <- cperc_data_prep(probs, df, method)
  gplot      <- cperc_plot_build(cperc_data)
  gplot      <- cperc_plot_modify(gplot, cperc_data, method, probs, df)

	if (print_plot) {
	  print(gplot)
	} else {
	  return(gplot)
	}

}

#' @rdname vdist_chisquare_plot
#' @export
#'
vdist_chisquare_prob <- function(perc = 13, df = 11, type = c("lower", "upper"), print_plot = TRUE) {

  check_numeric(df, "df")
  check_numeric(perc, "perc")

  method     <- match.arg(type)
  cprob_data <- cprob_data_prep(perc, df, method)
  gplot      <- cprob_plot_build(cprob_data, method, perc, df)

	if (print_plot) {
	  print(gplot)
	} else {
	  return(gplot)
	}

}



