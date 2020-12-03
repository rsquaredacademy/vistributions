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

  x   <- vdist_xax(mean)
  l   <- vdist_seql(mean, sd)
  col <- c("#0000CD", "#4682B4", "#6495ED", "#4682B4", "#6495ED")
  l1  <- c(3, 2, 1, 5, 6)
  l2  <- c(5, 3, 2, 6, 7)
  xm  <- vdist_xmm(mean, sd)
  
  plot_data <- data.frame(x = x, y = stats::dnorm(x, mean, sd))

  gplot <-
    ggplot2::ggplot(plot_data) +
    ggplot2::geom_line(ggplot2::aes(x = x, y = y)) +
    ggplot2::xlab('') + ggplot2::ylab('') +
    ggplot2::ggtitle(label = "Normal Distribution",
      subtitle = paste("Mean:", mean, "     Standard Deviation:", sd)) +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                   plot.subtitle = ggplot2::element_text(hjust = 0.5))

  ll <- l[3:9]

  for (i in seq_len(length(l1))) {
    poly_data <- vdist_pol_cord(ll[l1[i]], ll[l2[i]], mean, sd)
    gplot <-
      gplot +
      ggplot2::geom_polygon(data = poly_data, mapping = ggplot2::aes(x = x, y = y), fill = col[i])
  }

  if (print_plot) {
    print(gplot)
  } else {
    return(gplot)
  }

}

#' @rdname vdist_normal_plot
#' @export
#'
vdist_normal_perc <- function(probs = 0.95, mean = 0, sd = 1,
                              type = c("lower", "upper", "both"),
                              print_plot = TRUE) {

  check_numeric(mean, "mean")
  check_numeric(sd, "sd")
  check_positive(sd)
  check_numeric(probs, "probs")
  check_range(probs, 0, 1, "probs")

  x      <- vdist_xax(mean)
  method <- match.arg(type)
  l      <- vdist_seql(mean, sd)
  ln     <- length(l)

  if (method == "lower") {
    pp  <- round(stats::qnorm(probs, mean, sd), 3)
    lc  <- c(l[1], pp, l[ln])
    col <- c("#0000CD", "#6495ED")
    l1  <- c(1, 2)
    l2  <- c(2, 3)
  } else if (method == "upper") {
    pp  <- round(stats::qnorm(probs, mean, sd, lower.tail = F), 3)
    lc  <- c(l[1], pp, l[ln])
    col <- c("#6495ED", "#0000CD")
    l1  <- c(1, 2)
    l2  <- c(2, 3)
  } else {
    alpha <- (1 - probs) / 2
    pp1 <- round(stats::qnorm(alpha, mean, sd), 3)
    pp2 <- round(stats::qnorm(alpha, mean, sd, lower.tail = F), 3)
    pp  <- c(pp1, pp2)
    lc  <- c(l[1], pp1, pp2, l[ln])
    col <- c("#6495ED", "#0000CD", "#6495ED")
    l1  <- c(1, 2, 3)
    l2  <- c(2, 3, 4)
  }

  xm <- vdist_xmm(mean, sd)
  plot_data <- data.frame(x = x, y = stats::dnorm(x, mean, sd))

  gplot <-
    ggplot2::ggplot(plot_data) +
    ggplot2::geom_line(ggplot2::aes(x = x, y = y)) +
    ggplot2::xlab(paste("Mean:", mean, " Standard Deviation:", sd)) + ggplot2::ylab('') +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                   plot.subtitle = ggplot2::element_text(hjust = 0.5))

  if (method == "lower") {
	  gplot <-
	    gplot +
	    ggplot2::ggtitle(label = "Normal Distribution",
	      subtitle = paste0("P(X < ", pp, ") = ", probs * 100, "%")) +
	    ggplot2::annotate("text", label = paste0(probs * 100, "%"),
	      x = pp - sd, y = max(stats::dnorm(x, mean, sd)) + 0.025, color = "#0000CD",
	      size = 3) +
	    ggplot2::annotate("text", label = paste0((1 - probs) * 100, "%"),
	      x = pp + sd, y = max(stats::dnorm(x, mean, sd)) + 0.025, color = "#6495ED",
	      size = 3)

	} else if (method == "upper") {
	  gplot <-
	  	gplot +
	    ggplot2::ggtitle(label = "Normal Distribution",
	      subtitle = paste0("P(X > ", pp, ") = ", probs * 100, "%")) +
	    ggplot2::annotate("text", label = paste0((1 - probs) * 100, "%"),
	      x = pp - sd, y = max(stats::dnorm(x, mean, sd)) + 0.025, color = "#6495ED",
	      size = 3) +
	    ggplot2::annotate("text", label = paste0(probs * 100, "%"),
	      x = pp + sd, y = max(stats::dnorm(x, mean, sd)) + 0.025, color = "#0000CD",
	      size = 3)
	} else {
		gplot <-
	  	gplot +
	    ggplot2::ggtitle(label = "Normal Distribution",
	      subtitle = paste0("P(", pp[1], " < X < ", pp[2], ") = ", probs * 100, "%")) +
	    ggplot2::annotate("text", label = paste0(probs * 100, "%"),
	      x = mean, y = max(stats::dnorm(x, mean, sd)) + 0.025, color = "#0000CD",
	      size = 3) +
	    ggplot2::annotate("text", label = paste0(alpha * 100, "%"),
	      x = pp[1] - sd, y = max(stats::dnorm(x, mean, sd)) + 0.025, color = "#6495ED",
	      size = 3) +
	    ggplot2::annotate("text", label = paste0(alpha * 100, "%"),
	      x = pp[2] + sd, y = max(stats::dnorm(x, mean, sd)) + 0.025, color = "#6495ED",
	      size = 3)
	}

	for (i in seq_len(length(l1))) {
		poly_data <- vdist_pol_cord(lc[l1[i]], lc[l2[i]], mean, sd)
		gplot <-
		  gplot +
		  ggplot2::geom_polygon(data = poly_data, mapping = ggplot2::aes(x = x, y = y), fill = col[i])
  }

  pln <- length(pp)

  for (i in seq_len(pln)) {

  	point_data <- data.frame(x = pp[i], y = 0)

  	gplot <-
  	  gplot +
  	  ggplot2::geom_vline(xintercept = pp[i], linetype = 2, size = 1) +
  	  ggplot2::geom_point(data = point_data, mapping = ggplot2::aes(x = x, y = y),
	    shape = 4, color = 'red', size = 3)
  }

  gplot <-
    gplot +
	 	 	ggplot2::scale_y_continuous(breaks = NULL) +
	  	ggplot2::scale_x_continuous(breaks = l)

  if (print_plot) {
    print(gplot)
  } else {
    return(gplot)
  }

}

#' @rdname vdist_normal_plot
#' @export
#'
vdist_normal_prob <- function(perc, mean = 0, sd = 1,
                              type = c("lower", "upper", "both"),
                              print_plot = TRUE) {

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

  el <- max(abs(perc - mean)) / sd + 1
  x  <- vdist_xaxp(mean, el)
  l  <- vdist_seqlp(mean, sd, el)
  ln <- length(l)

  if (method == "lower") {
    pp  <- round(stats::pnorm(perc, mean, sd), 3)
    lc  <- c(l[1], perc, l[ln])
    col <- c("#0000CD", "#6495ED")
    l1  <- c(1, 2)
    l2  <- c(2, 3)
  } else if (method == "upper") {
    pp  <- round(stats::pnorm(perc, mean, sd, lower.tail = F), 3)
    lc  <- c(l[1], perc, l[ln])
    col <- c("#6495ED", "#0000CD")
    l1  <- c(1, 2)
    l2  <- c(2, 3)
  } else {
    pp1 <- round(stats::pnorm(perc[1], mean, sd), 3)
    pp2 <- round(stats::pnorm(perc[2], mean, sd, lower.tail = F), 3)
    pp  <- c(pp1, pp2)
    lc  <- c(l[1], perc[1], perc[2], l[ln])
    col <- c("#6495ED", "#0000CD", "#6495ED")
    l1  <- c(1, 2, 3)
    l2  <- c(2, 3, 4)
  }

  xm <- vdist_xmmp(mean, sd, el)
  plot_data <- data.frame(x = x, y = stats::dnorm(x, mean, sd))

  gplot <-
    ggplot2::ggplot(plot_data) +
    ggplot2::geom_line(ggplot2::aes(x = x, y = y)) +
    ggplot2::xlab(paste("Mean:", mean, " Standard Deviation:", sd)) + ggplot2::ylab('') +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                   plot.subtitle = ggplot2::element_text(hjust = 0.5))

  if (method == "lower") {
	  gplot <-
	    gplot +
	    ggplot2::ggtitle(label = "Normal Distribution",
	      subtitle = paste0("P(X < ", perc, ") = ", pp * 100, "%")) +
	    ggplot2::annotate("text", label = paste0(pp * 100, "%"),
	      x = perc - sd, y = max(stats::dnorm(x, mean, sd)) + 0.07, color = "#0000CD",
	      size = 3) +
	    ggplot2::annotate("text", label = paste0((1 - pp) * 100, "%"),
	      x = perc + sd, y = max(stats::dnorm(x, mean, sd)) + 0.07, color = "#6495ED",
	      size = 3)

	} else if (method == "upper") {
	  gplot <-
	  	gplot +
	    ggplot2::ggtitle(label = "Normal Distribution",
	      subtitle = paste0("P(X > ", perc, ") = ", pp * 100, "%")) +
	    ggplot2::annotate("text", label = paste0((1 - pp) * 100, "%"),
	      x = perc - sd, y = max(stats::dnorm(x, mean, sd)) + 0.07, color = "#6495ED",
	      size = 3) +
	    ggplot2::annotate("text", label = paste0(pp * 100, "%"),
	      x = perc + sd, y = max(stats::dnorm(x, mean, sd)) + 0.07, color = "#0000CD",
	      size = 3)
	} else {
		gplot <-
	  	gplot +
	    ggplot2::ggtitle(label = "Normal Distribution",
	      subtitle = paste0("P(", perc[1], " < X < ", perc[2], ") = ", (1 - (pp1 + pp2)) * 100, "%")) +
	    ggplot2::annotate("text", label = paste0((1 - (pp1 + pp2)) * 100, "%"),
	      x = mean(perc), y = max(stats::dnorm(x, mean, sd)) + 0.07, color = "#0000CD",
	      size = 3) +
	    ggplot2::annotate("text", label = paste0(pp[1] * 100, "%"),
	      x = perc[1] - sd, y = max(stats::dnorm(x, mean, sd)) + 0.07, color = "#6495ED",
	      size = 3) +
	    ggplot2::annotate("text", label = paste0(pp[2] * 100, "%"),
	      x = perc[2] + sd, y = max(stats::dnorm(x, mean, sd)) + 0.07, color = "#6495ED",
	      size = 3)
	}

  for (i in seq_len(length(l1))) {
		poly_data <- vdist_pol_cord(lc[l1[i]], lc[l2[i]], mean, sd)
		gplot <-
		  gplot +
		  ggplot2::geom_polygon(data = poly_data, mapping = ggplot2::aes(x = x, y = y), fill = col[i])
  }

  pln <- length(pp)

  for (i in seq_len(pln)) {

  	point_data <- data.frame(x = perc[i], y = 0)

  	gplot <-
  	  gplot +
  	  ggplot2::geom_vline(xintercept = perc[i], linetype = 2, size = 1) +
  	  ggplot2::geom_point(data = point_data, mapping = ggplot2::aes(x = x, y = y),
	    shape = 4, color = 'red', size = 3)
  }

  gplot <-
    gplot +
	 	 	ggplot2::scale_y_continuous(breaks = NULL) +
	  	ggplot2::scale_x_continuous(breaks = l)

  if (print_plot) {
    print(gplot)
  } else {
    return(gplot)
  }

}

vdist_xax <- function(mean) {
  xl <- mean - 3
  xu <- mean + 3
  x <- seq(xl, xu, 0.01)
  return(x)
}


vdist_seql <- function(mean, sd) {
  lmin <- mean - (5 * sd)
  lmax <- mean + (5 * sd)
  l <- seq(lmin, lmax, sd)
  return(l)
}

vdist_pol_cord <- function(l1, l2, mean, sd) {
  x <- c(l1, seq(l1, l2, 0.01), l2)
  y <- c(0, stats::dnorm(seq(l1, l2, 0.01), mean, sd), 0)
  data <- data.frame(x = x, y = y)
  return(data)
}

vdist_xaxp <- function(mean, el) {
  xl <- mean - el
  xu <- mean + el
  x <- seq(xl, xu, 0.01)
  return(x)
}


vdist_seqlp <- function(mean, sd, el) {
  if (el > 4) {
    lmin <- mean - (el * sd)
    lmax <- mean + (el * sd)
  } else {
    lmin <- mean - (4 * sd)
    lmax <- mean + (4 * sd)
  }

  l <- seq(lmin, lmax, sd)
  return(l)
}

vdist_xmmp <- function(mean, sd, el) {
  if (el > 4) {
    xmin <- mean - (el * sd)
    xmax <- mean + (el * sd)
  } else {
    xmin <- mean - (4 * sd)
    xmax <- mean + (4 * sd)
  }

  out <- c(xmin, xmax)
  return(out)
}
