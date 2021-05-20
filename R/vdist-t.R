#' Visualize t distribution
#'
#' Visualize how degrees of freedom affect the shape of t
#' distribution, visualize quantiles out of given probability and
#' probability from a given quantile.
#'
#' @param df Degrees of freedom.
#' @param probs Probability value.
#' @param perc Quantile value.
#' @param type Lower tail, upper tail, interval or both.
#' @param print_plot logical; if \code{TRUE}, prints the plot else returns a plot object.
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
#' # visualize probability from a given quantile
#' vdist_t_prob(2.045, 7, 'lower')
#' vdist_t_prob(0.945, 7, 'upper')
#' vdist_t_prob(1.445, 7, 'interval')
#' vdist_t_prob(1.6, 7, 'both')
#'
#' @seealso \code{\link[stats]{TDist}}
#'
#' @name vdist_t
NULL

#' @export
#' @rdname vdist_t
#'
vdist_t_plot <- function(df = 3, print_plot = TRUE) {

  check_numeric(df, "df")

  df <- as.integer(df)
  x  <- seq(-4, 4, 0.01)

  plot_data <- data.frame(x = x, y = dt(x, df))
  poly_data <- data.frame(y = c(-4, seq(-4, 4, 0.01), 4),
    z = c(0, dt(seq(-4, 4, 0.01), df), 0))

  gplot <-
    ggplot(plot_data) +
    geom_line(aes(x = x, y = y), color = 'blue') +
    ggtitle(label = 't Distribution', subtitle = paste("df =", df)) +
    xlab('') + ylab('') +
    geom_polygon(data = poly_data, mapping = aes(x = y, y = z),
      fill = '#4682B4') +
    scale_y_continuous(breaks = NULL) +
    scale_x_continuous(breaks = -4:4) +
    theme(plot.title = element_text(hjust = 0.5),
                   plot.subtitle = element_text(hjust = 0.5))

  if (print_plot) {
    print(gplot)
  } else {
    return(gplot)
  }

}

#' @rdname vdist_t
#' @export
#'
vdist_t_perc <- function(probs = 0.95, df = 4,
                         type = c("lower", "upper", "both"),
                         print_plot = TRUE) {

  check_numeric(probs, "probs")
  check_numeric(df, "df")
  check_range(probs, 0, 1, "probs")

  df      <- as.integer(df)
  method  <- match.arg(type)
  l       <- seq(-5, 5, 0.01)
  ln      <- length(l)

  if (method == "lower") {
    pp    <- round(qt(probs, df), 3)
    lc    <- c(l[1], pp, l[ln])
    col   <- c("#0000CD", "#6495ED")
    l1    <- c(1, 2)
    l2    <- c(2, 3)
  } else if (method == "upper") {
    pp    <- round(qt(probs, df, lower.tail = F), 3)
    lc    <- c(l[1], pp, l[ln])
    col   <- c("#6495ED", "#0000CD")
    l1    <- c(1, 2)
    l2    <- c(2, 3)
  } else {
    alpha <- (1 - probs) / 2
    pp1   <- round(qt(alpha, df), 3)
    pp2   <- round(qt(alpha, df, lower.tail = F), 3)
    pp    <- c(pp1, pp2)
    lc    <- c(l[1], pp1, pp2, l[ln])
    col   <- c("#6495ED", "#0000CD", "#6495ED")
    l1    <- c(1, 2, 3)
    l2    <- c(2, 3, 4)
  }

  plot_data <- data.frame(x = l, y = dt(l, df))

  gplot <-
    ggplot(plot_data) +
    geom_line(aes(x = x, y = y), color = 'blue') +
    xlab(paste("df =", df)) + ylab('') +
    theme(plot.title = element_text(hjust = 0.5),
                   plot.subtitle = element_text(hjust = 0.5))

  if (method == "lower") {
    gplot <-
      gplot +
      ggtitle(label = "t Distribution",
        subtitle = paste0("P(X < ", pp, ") = ", probs * 100, "%")) +
      annotate("text", label = paste0(probs * 100, "%"),
        x = pp - 0.3, y = max(dt(l, df)) + 0.025, color = "#0000CD",
        size = 3) +
      annotate("text", label = paste0((1 - probs) * 100, "%"),
        x = pp + 0.3, y = max(dt(l, df)) + 0.025, color = "#6495ED",
        size = 3)

  } else if (method == "upper") {
    gplot <-
      gplot +
      ggtitle(label = "t Distribution",
        subtitle = paste0("P(X > ", pp, ") = ", probs * 100, "%")) +
      annotate("text", label = paste0((1 - probs) * 100, "%"),
        x = pp - 0.3, y = max(dt(l, df)) + 0.025, color = "#6495ED",
        size = 3) +
      annotate("text", label = paste0(probs * 100, "%"),
        x = pp + 0.3, y = max(dt(l, df)) + 0.025, color = "#0000CD",
        size = 3)
  } else {
    gplot <-
      gplot +
      ggtitle(label = "t Distribution",
        subtitle = paste0("P(", pp[1], " < X < ", pp[2], ") = ", probs * 100, "%")) +
      annotate("text", label = paste0(probs * 100, "%"),
        x = mean(l), y = max(dt(l, df)) + 0.025, color = "#0000CD",
        size = 3) +
      annotate("text", label = paste0(alpha * 100, "%"),
        x = pp[1] - 0.3, y = max(dt(l, df)) + 0.025, color = "#6495ED",
        size = 3) +
      annotate("text", label = paste0(alpha * 100, "%"),
        x = pp[2] + 0.3, y = max(dt(l, df)) + 0.025, color = "#6495ED",
        size = 3)
  }

  for (i in seq_len(length(l1))) {
    poly_data <- vdist_pol_t(lc[l1[i]], lc[l2[i]], df)
    gplot <-
      gplot +
      geom_polygon(data = poly_data, mapping = aes(x = x, y = y), fill = col[i])
  }

  pln <- length(pp)

  for (i in seq_len(pln)) {

    point_data <- data.frame(x = pp[i], y = 0)

    gplot <-
      gplot +
      geom_vline(xintercept = pp[i], linetype = 2, size = 1) +
      geom_point(data = point_data, mapping = aes(x = x, y = y),
      shape = 4, color = 'red', size = 3)
  }

  gplot <-
    gplot +
      scale_y_continuous(breaks = NULL) +
      scale_x_continuous(breaks = -5:5)

  if (print_plot) {
    print(gplot)
  } else {
    return(gplot)
  }

}

#' @rdname vdist_t
#' @export
#'
vdist_t_prob <- function(perc = 1.6, df = 7,
                         type = c("lower", "upper", "interval", "both"),
                         print_plot = TRUE) {

  check_numeric(perc, "perc")
  check_numeric(df, "df")

  df     <- as.integer(df)
  method <- match.arg(type)

  l <- if (abs(perc) < 5) {
    seq(-5, 5, 0.01)
  } else {
    seq(-(perc + 1), (perc + 1), 0.01)
  }

  ln <- length(l)

  if (method == "lower") {
    pp  <- round(pt(perc, df), 3)
    lc  <- c(l[1], perc, l[ln])
    col <- c("#0000CD", "#6495ED")
    l1  <- c(1, 2)
    l2  <- c(2, 3)
  } else if (method == "upper") {
    pp  <- round(pt(perc, df, lower.tail = F), 3)
    lc  <- c(l[1], perc, l[ln])
    col <- c("#6495ED", "#0000CD")
    l1  <- c(1, 2)
    l2  <- c(2, 3)
  } else if (method == "interval") {
    if (perc < 0) {
      perc <- -perc
    }

    pp1 <- round(pt(-perc, df), 3)
    pp2 <- round(pt(perc, df, lower.tail = F), 3)
    pp  <- c(pp1, pp2)
    lc  <- c(l[1], -perc, perc, l[ln])
    col <- c("#6495ED", "#0000CD", "#6495ED")
    l1  <- c(1, 2, 3)
    l2  <- c(2, 3, 4)
  } else {
    if (perc < 0) {
      perc <- -perc
    }

    pp1 <- round(pt(-perc, df), 3)
    pp2 <- round(pt(perc, df, lower.tail = F), 3)
    pp  <- c(pp1, pp2)
    lc  <- c(l[1], -perc, perc, l[ln])
    col <- c("#0000CD", "#6495ED", "#0000CD")
    l1  <- c(1, 2, 3)
    l2  <- c(2, 3, 4)
  }

  plot_data <- data.frame(x = l, y = dt(l, df))

  gplot <-
    ggplot(plot_data) +
    geom_line(aes(x = x, y = y), color = 'blue') +
    xlab(paste("df =", df)) + ylab('') +
    theme(plot.title = element_text(hjust = 0.5),
                   plot.subtitle = element_text(hjust = 0.5)) +
    scale_x_continuous(breaks = min(l):max(l)) +
    scale_y_continuous(breaks = NULL)

  for (i in seq_len(length(l1))) {
    poly_data <- vdist_pol_t(lc[l1[i]], lc[l2[i]], df)
    gplot <-
      gplot +
      geom_polygon(data = poly_data, mapping = aes(x = x, y = y), fill = col[i])
  }


  if (method == "lower") {

    point_data <- data.frame(x = perc, y = 0)

    gplot <-
      gplot +
      ggtitle(label = "t Distribution",
        subtitle = paste0("P(X < ", perc, ") = ", pp * 100, "%")) +
      annotate("text", label = paste0(pp * 100, "%"),
        x = perc - 1, y = max(dt(l, df)) + 0.07, color = "#0000CD",
        size = 3) +
      annotate("text", label = paste0((1 - pp) * 100, "%"),
        x = perc + 1, y = max(dt(l, df)) + 0.07, color = "#6495ED",
        size = 3) +
      geom_vline(xintercept = perc, linetype = 2, size = 1) +
      geom_point(data = point_data, mapping = aes(x = x, y = y),
      shape = 4, color = 'red', size = 3)

  } else if (method == "upper") {

    point_data <- data.frame(x = perc, y = 0)

    gplot <-
      gplot +
      ggtitle(label = "t Distribution",
        subtitle = paste0("P(X > ", perc, ") = ", pp * 100, "%")) +
      annotate("text", label = paste0((1 - pp) * 100, "%"),
        x = perc - 1, y = max(dt(l, df)) + 0.07, color = "#0000CD",
        size = 3) +
      annotate("text", label = paste0(pp * 100, "%"),
        x = perc + 1, y = max(dt(l, df)) + 0.07, color = "#6495ED",
        size = 3) +
      geom_vline(xintercept = perc, linetype = 2, size = 1) +
      geom_point(data = point_data, mapping = aes(x = x, y = y),
      shape = 4, color = 'red', size = 3)

  } else if (method == "interval") {

    point_data <- data.frame(x1 = perc, x2 = -perc, y = 0)

    gplot <-
      gplot +
      ggtitle(label = "t Distribution",
        subtitle = paste0("P(", -perc, " < X < ", perc, ") = ", (1 - (pp1 + pp2)) * 100, "%")) +
      annotate("text", label = paste0((1 - (pp1 + pp2)) * 100, "%"),
        x = 0, y = max(dt(l, df)) + 0.07, color = "#0000CD", size = 3) +
      annotate("text", label = paste0(pp[1] * 100, "%"),
        x = perc + 1, y = max(dt(l, df)) + 0.07, color = "#6495ED",
        size = 3) +
      annotate("text", label = paste0(pp[2] * 100, "%"),
        x = -perc - 1, y = max(dt(l, df)) + 0.07, color = "#6495ED",
        size = 3) +
      geom_vline(xintercept = perc, linetype = 2, size = 1) +
      geom_vline(xintercept = -perc, linetype = 2, size = 1) +
      geom_point(data = point_data, mapping = aes(x = x1, y = y),
      shape = 4, color = 'red', size = 3) +
      geom_point(data = point_data, mapping = aes(x = x2, y = y),
      shape = 4, color = 'red', size = 3)
  } else {

    point_data <- data.frame(x1 = perc, x2 = -perc, y = 0)

    gplot <-
      gplot +
      ggtitle(label = "t Distribution",
        subtitle = paste0("P(|X| > ", perc, ") = ", (pp1 + pp2) * 100, "%")) +
      annotate("text", label = paste0((1 - (pp1 + pp2)) * 100, "%"),
        x = 0, y = max(dt(l, df)) + 0.07, color = "#0000CD", size = 3) +
      annotate("text", label = paste0(pp[1] * 100, "%"),
        x = perc + 1, y = max(dt(l, df)) + 0.07, color = "#6495ED",
        size = 3) +
      annotate("text", label = paste0(pp[2] * 100, "%"),
        x = -perc - 1, y = max(dt(l, df)) + 0.07, color = "#6495ED",
        size = 3) +
      geom_vline(xintercept = perc, linetype = 2, size = 1) +
      geom_vline(xintercept = -perc, linetype = 2, size = 1) +
      geom_point(data = point_data, mapping = aes(x = x1, y = y),
      shape = 4, color = 'red', size = 3) +
      geom_point(data = point_data, mapping = aes(x = x2, y = y),
      shape = 4, color = 'red', size = 3)
  }

  if (print_plot) {
    print(gplot)
  } else {
    return(gplot)
  }

}


vdist_pol_t <- function(l1, l2, df) {
  x    <- c(l1, seq(l1, l2, 0.01), l2)
  y    <- c(0, dt(seq(l1, l2, 0.01), df), 0)
  data.frame(x = x, y = y)
}
