#' Visualize chi square distribution
#'
#' Visualize how changes in degrees of freedom affect the shape of
#' the chi square distribution. Compute & visualize quantiles out of given
#' probability and probability from a given quantile.
#'
#' @param df Degrees of freedom.
#' @param normal If \code{TRUE}, normal curve with same \code{mean} and
#' \code{sd} as the chi square distribution is drawn.
#'
#' @examples
#' # visualize chi square distribution
#' vdist_chisquare_plot()
#' vdist_chisquare_plot(df = 5)
#' vdist_chisquare_plot(df = 5, normal = TRUE)
#'
#' @seealso \code{\link[stats]{Chisquare}}
#'
#' @export
#'
vdist_chisquare_plot <- function(df = 3, normal = FALSE) {

	if (!is.numeric(df)) {
    stop("df must be numeric/integer")
  }

  if (!is.logical(normal)) {
    stop("normal must be logical")
  }

	df    <- as.integer(df)
	chim  <- round(df, 3)
	chisd <- round(sqrt(2 * df), 3)
	x     <- seq(0, 25, 0.01)
	data  <- stats::dchisq(x, df)

	plot_data  <- tibble::tibble(x = x, chi = data)
	poly_data  <- tibble::tibble(y = c(0, seq(0, 25, 0.01), 25),
	                            z = c(0, stats::dchisq(seq(0, 25, 0.01), df), 0))
	point_data <- tibble::tibble(x = chim, y = min(data))
	nline_data <- tibble::tibble(x = x, y = stats::dnorm(x, chim, chisd))


	pp <-
	  plot_data %>%
	  ggplot2::ggplot() +
	  ggplot2::geom_line(ggplot2::aes(x, chi), color = '#4682B4', size = 2) +
	  ggplot2::ggtitle(label = "Chi Square Distribution",
	                   subtitle = paste("df =", df)) + ggplot2::ylab('') +
	  ggplot2::xlab(paste("Mean =", chim, " Std Dev. =", chisd)) +
	  ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
	                 plot.subtitle = ggplot2::element_text(hjust = 0.5)) +
	  ggplot2::scale_x_continuous(breaks = seq(0, 25, 2)) +
	  ggplot2::geom_polygon(data = poly_data, mapping = ggplot2::aes(x = y, y = z),
	                        fill = '#4682B4') +
	  ggplot2::geom_point(data = point_data, mapping = ggplot2::aes(x = x, y = y),
	                      shape = 4, color = 'red', size = 3)


	if (normal) {
	  pp +
	    ggplot2::geom_line(data = nline_data, mapping = ggplot2::aes(x = x, y = y),
	      color = '#FF4500')
	}

}



