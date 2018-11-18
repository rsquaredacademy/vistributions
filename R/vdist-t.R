#' Visualize t distribution
#'
#' Visualize how degrees of freedom affect the shape of t
#' distribution, visualize quantiles out of given probability and
#' probability from a given quantile.
#'
#' @param df Degrees of freedom.
#' @param probs Probability value.
#' @param type Lower tail, upper tail, interval or both.
#'
#' @examples
#' # visualize t distribution
#' vdist_t_plot()
#' vdist_t_plot(6)
#' vdist_t_plot(df = 8)
#'
#' # visualize quantiles out of given probability
#' vdist_t_perc(probs = 0.95, df = 4, type = 'lower')
#' vdist_t_perc(probs = 0.35, df = 4, type = 'upper')
#' vdist_t_perc(probs = 0.69, df = 7, type = 'both')
#'
#' @seealso \code{\link[stats]{TDist}}
#'
#' @name vdist_t
NULL

#' @export
#' @rdname vdist_t
#'
vdist_t_plot <- function(df = 3) {

  if (!is.numeric(df)) {
    stop("df must be numeric/integer")
  }

  df <- as.integer(df)

  x <- seq(-4, 4, 0.01)

  plot_data <- tibble::tibble(x = x, y = stats::dt(x, df))
  poly_data <- tibble::tibble(y = c(-4, seq(-4, 4, 0.01), 4),
    z = c(0, dt(seq(-4, 4, 0.01), df), 0))

  gplot <-
    plot_data %>%
    ggplot2::ggplot() +
    ggplot2::geom_line(ggplot2::aes(x = x, y = y), color = 'blue') +
    ggplot2::ggtitle(label = 't Distribution', subtitle = paste("df =", df)) +
    ggplot2::xlab('') + ggplot2::ylab('') + 
    ggplot2::geom_polygon(data = poly_data, mapping = ggplot2::aes(x = y, y = z),
      fill = '#4682B4') +
    ggplot2::scale_y_continuous(breaks = NULL) +
    ggplot2::scale_x_continuous(breaks = -4:4) +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                   plot.subtitle = ggplot2::element_text(hjust = 0.5))

  print(gplot)

}

#' @rdname vdist_t
#' @export
#'
vdist_t_perc <- function(probs = 0.95, df = 4, type = c("lower", "upper", "both")) {
 
  if (!is.numeric(probs)) {
    stop("probs must be numeric")
  }

  if (!is.numeric(df)) {
    stop("df must be numeric/integer")
  }

  if ((probs < 0) | (probs > 1)) {
    stop("probs must be between 0 and 1")
  }

  df      <- as.integer(df)
  method  <- match.arg(type)
  l       <- seq(-5, 5, 0.01)
  ln      <- length(l)

  if (method == "lower") {
    pp    <- round(stats::qt(probs, df), 3)
    lc    <- c(l[1], pp, l[ln])
    col   <- c("#0000CD", "#6495ED")
    l1    <- c(1, 2)
    l2    <- c(2, 3)
  } else if (method == "upper") {
    pp    <- round(stats::qt(probs, df, lower.tail = F), 3)
    lc    <- c(l[1], pp, l[ln])
    col   <- c("#6495ED", "#0000CD")
    l1    <- c(1, 2)
    l2    <- c(2, 3)
  } else {
    alpha <- (1 - probs) / 2
    pp1   <- round(stats::qt(alpha, df), 3)
    pp2   <- round(stats::qt(alpha, df, lower.tail = F), 3)
    pp    <- c(pp1, pp2)
    lc    <- c(l[1], pp1, pp2, l[ln])
    col   <- c("#6495ED", "#0000CD", "#6495ED")
    l1    <- c(1, 2, 3)
    l2    <- c(2, 3, 4)
  }

  plot_data <- tibble::tibble(x = l, y = stats::dt(l, df))

  gplot <-
    plot_data %>%
    ggplot2::ggplot() +
    ggplot2::geom_line(ggplot2::aes(x = x, y = y), color = 'blue') +
    ggplot2::xlab(paste("df =", df)) + ggplot2::ylab('') +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                   plot.subtitle = ggplot2::element_text(hjust = 0.5))

  if (method == "lower") {
    gplot <-
      gplot +
      ggplot2::ggtitle(label = "t Distribution",
        subtitle = paste0("P(X < ", pp, ") = ", probs * 100, "%")) +
      ggplot2::annotate("text", label = paste0(probs * 100, "%"), 
        x = pp - 0.3, y = max(stats::dt(l, df)) + 0.025, color = "#0000CD", 
        size = 3) +
      ggplot2::annotate("text", label = paste0((1 - probs) * 100, "%"),
        x = pp + 0.3, y = max(stats::dt(l, df)) + 0.025, color = "#6495ED", 
        size = 3)
      
  } else if (method == "upper") {
    gplot <- 
      gplot +
      ggplot2::ggtitle(label = "t Distribution",
        subtitle = paste0("P(X > ", pp, ") = ", probs * 100, "%")) +
      ggplot2::annotate("text", label = paste0((1 - probs) * 100, "%"), 
        x = pp - 0.3, y = max(stats::dt(l, df)) + 0.025, color = "#6495ED", 
        size = 3) +
      ggplot2::annotate("text", label = paste0(probs * 100, "%"),
        x = pp + 0.3, y = max(stats::dt(l, df)) + 0.025, color = "#0000CD", 
        size = 3) 
  } else {
    gplot <- 
      gplot +
      ggplot2::ggtitle(label = "t Distribution",
        subtitle = paste0("P(", pp[1], " < X < ", pp[2], ") = ", probs * 100, "%")) +
      ggplot2::annotate("text", label = paste0(probs * 100, "%"), 
        x = mean(l), y = max(stats::dt(l, df)) + 0.025, color = "#0000CD", 
        size = 3) +
      ggplot2::annotate("text", label = paste0(alpha * 100, "%"),
        x = pp[1] - 0.3, y = max(stats::dt(l, df)) + 0.025, color = "#6495ED", 
        size = 3) +
      ggplot2::annotate("text", label = paste0(alpha * 100, "%"),
        x = pp[2] + 0.3, y = max(stats::dt(l, df)) + 0.025, color = "#6495ED", 
        size = 3) 
  }

  for (i in seq_len(length(l1))) {
    poly_data <- vdist_pol_t(lc[l1[i]], lc[l2[i]], df)
    gplot <-
      gplot +
      ggplot2::geom_polygon(data = poly_data, mapping = ggplot2::aes(x = x, y = y), fill = col[i])
  }

  pln <- length(pp)

  for (i in seq_len(pln)) {

    point_data <- tibble::tibble(x = pp[i], y = 0)

    gplot <-
      gplot +
      ggplot2::geom_vline(xintercept = pp[i], linetype = 2, size = 1) +
      ggplot2::geom_point(data = point_data, mapping = ggplot2::aes(x = x, y = y),
      shape = 4, color = 'red', size = 3) 
  }

  gplot <-
    gplot +
      ggplot2::scale_y_continuous(breaks = NULL) +
      ggplot2::scale_x_continuous(breaks = -5:5)

  print(gplot)

}


vdist_pol_t <- function(l1, l2, df) {
  x <- c(l1, seq(l1, l2, 0.01), l2)
  y <- c(0, stats::dt(seq(l1, l2, 0.01), df), 0)
  data <- tibble::tibble(x = x, y = y)
  return(data)
}