#' Visualize f distribution
#'
#' @description Visualize how changes in degrees of freedom affect the
#' shape of the F distribution. Compute & visualize quantiles out of given
#' probability and probability from a given quantile.
#'
#' @param num_df Degrees of freedom associated with the numerator of f statistic.
#' @param den_df Degrees of freedom associated with the denominator of f statistic.
#' @param normal If \code{TRUE}, normal curve with same \code{mean} and
#' \code{sd} as the F distribution is drawn.
#' @param probs Probability value.
#' @param perc Quantile value.
#' @param type Lower tail or upper tail.
#' @param print_plot logical; if \code{TRUE}, prints the plot else returns a plot object.
#'
#' @examples
#' # visualize F distribution
#' vdist_f_plot()
#' vdist_f_plot(6, 10, normal = TRUE)
#'
#' # visualize probability from a given quantile
#' vdist_f_perc(0.95, 3, 30, 'lower')
#' vdist_f_perc(0.125, 9, 35, 'upper')
#'
#' # visualize quantiles out of given probability
#' vdist_f_prob(2.35, 5, 32)
#' vdist_f_prob(1.5222, 9, 35, type = "upper")
#'
#' @seealso \code{\link[stats]{FDist}}
#'
#' @export
#'
vdist_f_plot <- function(num_df = 4, den_df = 30, normal = FALSE,
                         print_plot = TRUE) {

  check_numeric(num_df, "num_df")
  check_numeric(den_df, "den_df")
  check_logical(normal)

  num_df <- as.integer(num_df)
  den_df <- as.integer(den_df)
  fm     <- round(den_df / (den_df - 2), 3)
  fsd    <- round(sqrt((2 * (fm ^ 2) * (num_df + den_df - 2)) / (num_df * (den_df - 4))), 3)
  x      <- seq(0, 4, 0.01)
  nx     <- seq(-2, 4, 0.01)

  plot_data  <- data.frame(x = x, y = stats::df(x, num_df, den_df))
  poly_data  <- data.frame(y = c(0, seq(0, 4, 0.01), 4),
    z = c(0, stats::df(seq(0, 4, 0.01), num_df, den_df), 0))
  point_data <- data.frame(x = fm, y = 0)
  nline_data <- data.frame(x = nx, y = stats::dnorm(nx, fm, fsd))

  gplot <-
    ggplot2::ggplot(plot_data) +
    ggplot2::geom_line(ggplot2::aes(x = x, y = y), color = "blue") +
    ggplot2::geom_polygon(data = poly_data, mapping = ggplot2::aes(x = y, y = z),
      fill = '#4682B4') +
    ggplot2::geom_point(data = point_data, mapping = ggplot2::aes(x = x, y = y),
      shape = 4, color = 'red', size = 3) +
    ggplot2::xlab(paste("Mean =", fm, " Std Dev. =", fsd)) + ggplot2::ylab('') +
    ggplot2::ggtitle(label = 'f Distribution',
      subtitle = paste("Num df =", num_df, "  Den df =", den_df)) +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                   plot.subtitle = ggplot2::element_text(hjust = 0.5)) +
    ggplot2::scale_x_continuous(breaks = c(-2:4)) +
    ggplot2::scale_y_continuous(breaks = NULL)

  if (normal) {
    gplot <-
      gplot +
      ggplot2::geom_line(data = nline_data, mapping = ggplot2::aes(x = x, y = y),
        color = '#FF4500')
  }

  if (print_plot) {
    print(gplot)
  } else {
    return(gplot)
  }

}

#' @rdname vdist_f_plot
#' @export
#'
vdist_f_perc <- function(probs = 0.95, num_df = 3, den_df = 30,
                         type = c("lower", "upper"), print_plot = TRUE) {

  check_numeric(num_df, "num_df")
  check_numeric(den_df, "den_df")
  check_numeric(probs, "probs")
  check_range(probs, 0, 1, "probs")

  num_df <- as.integer(num_df)
  den_df <- as.integer(den_df)
  method <- match.arg(type)
  fm     <- round(den_df / (den_df - 2), 3)
  fsd    <- round(sqrt((2 * (fm ^ 2) * (num_df + den_df - 2)) / (num_df * (den_df - 4))), 3)
  l      <- seq(0, 4, 0.01)
  ln     <- length(l)

  if (method == "lower") {
    pp  <- round(stats::qf(probs, num_df, den_df), 3)
    lc  <- c(l[1], pp, l[ln])
    col <- c("#0000CD", "#6495ED")
    l1  <- c(1, 2)
    l2  <- c(2, 3)
  } else {
    pp  <- round(stats::qf(probs, num_df, den_df, lower.tail = F), 3)
    lc  <- c(l[1], pp, l[ln])
    col <- c("#6495ED", "#0000CD")
    l1  <- c(1, 2)
    l2  <- c(2, 3)
  }

  plot_data <- data.frame(x = l, y = stats::df(l, num_df, den_df))

  gplot <-
    ggplot2::ggplot(plot_data) +
    ggplot2::geom_line(data = plot_data, mapping = ggplot2::aes(x = x, y = y),
      color = 'blue') + ggplot2::xlab(paste("Mean =", fm, " Std Dev. =", fsd)) +
    ggplot2::ylab('') +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                   plot.subtitle = ggplot2::element_text(hjust = 0.5))


  if (method == "lower") {
    gplot <-
      gplot +
      ggplot2::ggtitle(label = 'f Distribution',
        subtitle = paste0("P(X < ", pp, ") = ", probs * 100, "%")) +
      ggplot2::annotate("text", label = paste0(probs * 100, "%"),
        x = pp - 0.2, y = max(stats::df(l, num_df, den_df)) + 0.02, color = "#0000CD",
        size = 3) +
      ggplot2::annotate("text", label = paste0((1 - probs) * 100, "%"),
        x = pp + 0.2, y = max(stats::df(l, num_df, den_df)) + 0.02, color = "#6495ED",
        size = 3)

  } else {
    gplot <-
      gplot +
      ggplot2::ggtitle(label = 'f Distribution',
        subtitle = paste0("P(X > ", pp, ") = ", probs * 100, "%")) +
      ggplot2::annotate("text", label = paste0((1 - probs) * 100, "%"),
        x = pp - 0.2, y = max(stats::df(l, num_df, den_df)) + 0.02, color = "#6495ED",
        size = 3) +
      ggplot2::annotate("text", label = paste0(probs * 100, "%"),
        x = pp + 0.2, y = max(stats::df(l, num_df, den_df)) + 0.02, color = "#0000CD",
        size = 3)
  }

  for (i in seq_len(length(l1))) {
    poly_data <- vdist_pol_f(lc[l1[i]], lc[l2[i]], num_df, den_df)
    gplot <-
      gplot +
      ggplot2::geom_polygon(data = poly_data, mapping = ggplot2::aes(x = x, y = y),
        fill = col[i])
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
      ggplot2::scale_x_continuous(breaks = 0:5)

  if (print_plot) {
    print(gplot)
  } else {
    return(gplot)
  }

}

#' @rdname vdist_f_plot
#' @export
#'
vdist_f_prob <- function(perc, num_df, den_df, type = c("lower", "upper"),
                         print_plot = TRUE) {

  check_numeric(perc, "perc")
  check_numeric(num_df, "num_df")
  check_numeric(den_df, "den_df")

  num_df <- as.integer(num_df)
  den_df <- as.integer(den_df)
  method <- match.arg(type)
  fm     <- round(den_df / (den_df - 2), 3)
  fsd    <- round(sqrt((2 * (fm ^ 2) * (num_df + den_df - 2)) / (num_df * (den_df - 4))), 3)

  l <- if (perc < 4) {
    seq(0, 4, 0.01)
  } else {
    seq(0, (perc * 1.25), 0.01)
  }
  ln <- length(l)

  if (method == "lower") {
    pp  <- round(stats::pf(perc, num_df, den_df), 3)
    lc  <- c(l[1], perc, l[ln])
    col <- c("#0000CD", "#6495ED")
    l1  <- c(1, 2)
    l2  <- c(2, 3)
  } else {
    pp  <- round(stats::pf(perc, num_df, den_df, lower.tail = F), 3)
    lc  <- c(l[1], perc, l[ln])
    col <- c("#6495ED", "#0000CD")
    l1  <- c(1, 2)
    l2  <- c(2, 3)
  }

  plot_data <- data.frame(x = l, y = stats::df(l, num_df, den_df))

  gplot <-
    ggplot2::ggplot(plot_data) +
    ggplot2::geom_line(data = plot_data, mapping = ggplot2::aes(x = x, y = y),
      color = 'blue') + ggplot2::xlab(paste("Mean =", fm, " Std Dev. =", fsd)) +
    ggplot2::ylab('') +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                   plot.subtitle = ggplot2::element_text(hjust = 0.5))

  if (method == "lower") {
    gplot <-
      gplot +
      ggplot2::ggtitle(label = 'f Distribution',
        subtitle = paste0("P(X < ", perc, ") = ", pp * 100, "%")) +
      ggplot2::annotate("text", label = paste0(pp * 100, "%"),
        x = perc - fsd, y = max(stats::df(l, num_df, den_df)) + 0.04, color = "#0000CD",
        size = 3) +
      ggplot2::annotate("text", label = paste0(round((1 - pp) * 100, 2), "%"),
        x = perc + fsd, y = max(stats::df(l, num_df, den_df)) + 0.02, color = "#6495ED",
        size = 3)

  } else {
    gplot <-
      gplot +
      ggplot2::ggtitle(label = 'f Distribution',
        subtitle = paste0("P(X > ", perc, ") = ", pp * 100, "%")) +
      ggplot2::annotate("text", label = paste0(round((1 - pp) * 100, 2), "%"),
        x = perc - fsd, y = max(stats::df(l, num_df, den_df)) + 0.04, color = "#6495ED",
        size = 3) +
      ggplot2::annotate("text", label = paste0(pp * 100, "%"),
        x = perc + fsd, y = max(stats::df(l, num_df, den_df)) + 0.04, color = "#0000CD",
        size = 3)
  }

  for (i in seq_len(length(l1))) {
    poly_data <- vdist_pol_f(lc[l1[i]], lc[l2[i]], num_df, den_df)
    gplot <-
      gplot +
      ggplot2::geom_polygon(data = poly_data, mapping = ggplot2::aes(x = x, y = y),
        fill = col[i])
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
      ggplot2::scale_x_continuous(breaks = 0:max(l))

  if (print_plot) {
    print(gplot)
  } else {
    return(gplot)
  }

}


vdist_pol_f <- function(l1, l2, num_df, den_df) {
  x    <- c(l1, seq(l1, l2, 0.01), l2)
  y    <- c(0, df(seq(l1, l2, 0.01), num_df, den_df), 0)
  data <- data.frame(x = x, y = y)
  return(data)
}
