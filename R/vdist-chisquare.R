#' Visualize chi square distribution
#'
#' Visualize how changes in degrees of freedom affect the shape of
#' the chi square distribution. Compute & visualize quantiles out of given
#' probability and probability from a given quantile.
#'
#' @param df Degrees of freedom.
#' @param probs Probability value.
#' @param type Lower tail or upper tail.
#' @param normal If \code{TRUE}, normal curve with same \code{mean} and
#' \code{sd} as the chi square distribution is drawn.
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

#' @rdname vdist_chisquare_plot
#' @export
#'
vdist_chisquare_perc <- function(probs = 0.95, df = 3, type = c("lower", "upper")) {

  if (!is.numeric(probs)) {
    stop("probs must be numeric")
  }

  if (!is.numeric(df)) {
    stop("df must be numeric/integer")
  }

  if ((probs < 0) | (probs > 1)) {
    stop("probs must be between 0 and 1")
  }

  df     <- as.integer(df)
	method <- match.arg(type)
	chim   <- round(df, 3)
	chisd  <- round(sqrt(2 * df), 3)
	l      <- vdist_chiseql(chim, chisd)
	ln     <- length(l)

	if (method == "lower") {
	  pp  <- round(stats::qchisq(probs, df), 3)
	  lc  <- c(l[1], pp, l[ln])
	  col <- c("#0000CD", "#6495ED")
	  l1  <- c(1, 2)
	  l2  <- c(2, 3)
	} else {
	  pp  <- round(stats::qchisq(probs, df, lower.tail = F), 3)
	  lc  <- c(l[1], pp, l[ln])
	  col <- c("#6495ED", "#0000CD")
	  l1  <- c(1, 2)
	  l2  <- c(2, 3)
	}
	xm <- vdist_xmm(chim, chisd)

	plot_data <- tibble::tibble(x = l, y = stats::dchisq(l, df))
	gplot <- 
	  plot_data %>%
	  ggplot2::ggplot() +
	  ggplot2::geom_line(ggplot2::aes(x = x, y = y), color = "blue") +
	  ggplot2::xlab(paste("Mean =", chim, " Std Dev. =", chisd)) + 
	  ggplot2::ylab('') +
	  ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
	                 plot.subtitle = ggplot2::element_text(hjust = 0.5))


	if (method == "lower") {
	  gplot <-
	    gplot +
	    ggplot2::ggtitle(label = paste("Chi Square Distribution: df =", df),
	      subtitle = paste0("P(X < ", pp, ") = ", probs * 100, "%")) +
	    ggplot2::annotate("text", label = paste0(probs * 100, "%"), 
	      x = pp - chisd, y = max(stats::dchisq(l, df)) + 0.02, color = "#0000CD", 
	      size = 3) +
	    ggplot2::annotate("text", label = paste0((1 - probs) * 100, "%"),
	      x = pp + chisd, y = max(stats::dchisq(l, df)) + 0.02, color = "#6495ED", 
	      size = 3)
	    
	} else {
	  gplot +
	    ggplot2::ggtitle(label = paste("Chi Square Distribution: df =", df),
	      subtitle = paste0("P(X > ", pp, ") = ", probs * 100, "%")) +
	    ggplot2::annotate("text", label = paste0((1 - probs) * 100, "%"), 
	      x = pp - chisd, y = max(stats::dchisq(l, df)) + 0.02, color = "#6495ED", 
	      size = 3) +
	    ggplot2::annotate("text", label = paste0(probs * 100, "%"),
	      x = pp + chisd, y = max(stats::dchisq(l, df)) + 0.02, color = "#0000CD", 
	      size = 3) 
	}

	for (i in seq_len(length(l1))) {
	  pol_data <- vdist_pol_chi(lc[l1[i]], lc[l2[i]], df)
	  gplot <-
	    gplot +
	    ggplot2::geom_polygon(data = pol_data, mapping = ggplot2::aes(x = x, y = y),
	                          fill = col[i])
	}

	point_data <- tibble::tibble(x = pp, y = min(stats::dchisq(l, df)))

	gplot <- 
	  gplot +
	  ggplot2::geom_vline(xintercept = pp, linetype = 2, size = 1) +
	  ggplot2::geom_point(data = point_data, mapping = ggplot2::aes(x = x, y = y),
	    shape = 4, color = 'red', size = 3) +
	  ggplot2::scale_y_continuous(breaks = NULL) +
	  ggplot2::scale_x_continuous(breaks = seq(0, xm[2], by = 5))


}


vdist_chiseql <- function(mean, sd) {
  lmin <- mean - (5 * sd)
  lmax <- mean + (5 * sd)
  l <- seq(lmin, lmax, 0.01)
  return(l)
}


vdist_xmm <- function(mean, sd) {
  xmin <- mean - (5 * sd)
  xmax <- mean + (5 * sd)
  out <- c(xmin, xmax)
  return(out)
}

vdist_pol_chi <- function(l1, l2, df) {
  x <- c(l1, seq(l1, l2, 0.01), l2)
  y <- c(0, dchisq(seq(l1, l2, 0.01), df), 0)
  out <- tibble::tibble(x = x, y = y)
  return(out)
}
