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

  plot_data  <- data.frame(x = x, y = df(x, num_df, den_df))
  point_data <- data.frame(x = fm, y = 0)
  nline_data <- data.frame(x = nx, y = dnorm(nx, fm, fsd))

  poly_data  <- data.frame(y = c(0, seq(0, 4, 0.01), 4),
                           z = c(0,
                                 df(seq(0, 4, 0.01),
                                    num_df,
                                    den_df),
                                 0))


  gplot <-
    ggplot(plot_data) +
    geom_line(aes(x = x, y = y),
              color = "blue") +
    geom_polygon(data    = poly_data,
                 mapping = aes(x = y, y = z),
                 fill    = '#4682B4') +
    geom_point(data    = point_data,
               mapping = aes(x = x, y = y),
               shape   = 4,
               color   = 'red',
               size    = 3) +
    xlab(paste("Mean =", fm, " Std Dev. =", fsd)) +
    ylab('') +
    ggtitle(label    = 'f Distribution',
            subtitle = paste("Num df =", num_df, "  Den df =", den_df)) +
    theme(plot.title    = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5)) +
    scale_x_continuous(breaks = c(-2:4)) +
    scale_y_continuous(breaks = NULL)

  if (normal) {
    gplot <-
      gplot +
      geom_line(data    = nline_data,
                mapping = aes(x = x, y = y),
                color   = '#FF4500')
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
    pp  <- round(qf(probs, num_df, den_df), 3)
    lc  <- c(l[1], pp, l[ln])
    col <- c("#0000CD", "#6495ED")
    l1  <- c(1, 2)
    l2  <- c(2, 3)
  } else {
    pp  <- round(qf(probs, num_df, den_df, lower.tail = F), 3)
    lc  <- c(l[1], pp, l[ln])
    col <- c("#6495ED", "#0000CD")
    l1  <- c(1, 2)
    l2  <- c(2, 3)
  }

  plot_data <- data.frame(x = l, y = df(l, num_df, den_df))

  gplot <-
    ggplot(plot_data) +
    geom_line(data    = plot_data,
              mapping = aes(x = x, y = y),
              color   = 'blue') +
    xlab(paste("Mean =", fm, " Std Dev. =", fsd)) +
    ylab('') +
    theme(plot.title    = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5))


  if (method == "lower") {
    gplot <-
      gplot +
      ggtitle(label    = 'f Distribution',
              subtitle = paste0("P(X < ", pp, ") = ", probs * 100, "%")) +
      annotate("text",
               label = paste0(probs * 100, "%"),
               x     = pp - 0.2,
               y     = max(df(l, num_df, den_df)) + 0.02,
               color = "#0000CD",
               size  = 3) +
      annotate("text",
               label = paste0((1 - probs) * 100, "%"),
               x     = pp + 0.2,
               y     = max(df(l, num_df, den_df)) + 0.02,
               color = "#6495ED",
               size  = 3)

  } else {
    gplot <-
      gplot +
      ggtitle(label    = 'f Distribution',
              subtitle = paste0("P(X > ", pp, ") = ", probs * 100, "%")) +
      annotate("text",
               label = paste0((1 - probs) * 100, "%"),
               x     = pp - 0.2,
               y     = max(df(l, num_df, den_df)) + 0.02,
               color = "#6495ED",
               size  = 3) +
      annotate("text",
               label = paste0(probs * 100, "%"),
               x     = pp + 0.2,
               y     = max(df(l, num_df, den_df)) + 0.02,
               color = "#0000CD",
               size  = 3)
  }

  for (i in seq_len(length(l1))) {
    poly_data <- vdist_pol_f(lc[l1[i]], lc[l2[i]], num_df, den_df)
    gplot <-
      gplot +
      geom_polygon(data    = poly_data,
                   mapping = aes(x = x, y = y),
                   fill    = col[i])
  }

  pln <- length(pp)

  for (i in seq_len(pln)) {

    point_data <- data.frame(x = pp[i], y = 0)

    gplot <-
      gplot +
      geom_vline(xintercept = pp[i],
                 linetype   = 2,
                 size       = 1) +
      geom_point(data    = point_data,
                 mapping = aes(x = x, y = y),
                 shape   = 4,
                 color   = 'red',
                 size    = 3)
  }

  gplot <-
    gplot +
      scale_y_continuous(breaks = NULL) +
      scale_x_continuous(breaks = 0:5)

  if (print_plot) {
    print(gplot)
  } else {
    return(gplot)
  }

}

#' @rdname vdist_f_plot
#' @export
#'
vdist_f_prob <- function(perc = 2.35, num_df = 5, den_df = 32, type = c("lower", "upper"),
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
    pp  <- round(pf(perc, num_df, den_df), 3)
    lc  <- c(l[1], perc, l[ln])
    col <- c("#0000CD", "#6495ED")
    l1  <- c(1, 2)
    l2  <- c(2, 3)
  } else {
    pp  <- round(pf(perc, num_df, den_df, lower.tail = F), 3)
    lc  <- c(l[1], perc, l[ln])
    col <- c("#6495ED", "#0000CD")
    l1  <- c(1, 2)
    l2  <- c(2, 3)
  }

  plot_data <- data.frame(x = l, y = df(l, num_df, den_df))

  gplot <-
    ggplot(plot_data) +
    geom_line(data    = plot_data,
              mapping = aes(x = x, y = y),
              color   = 'blue') +
    xlab(paste("Mean =", fm, " Std Dev. =", fsd)) +
    ylab('') +
    theme(plot.title    = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5))

  if (method == "lower") {
    gplot <-
      gplot +
      ggtitle(label    = 'f Distribution',
              subtitle = paste0("P(X < ", perc, ") = ", pp * 100, "%")) +
      annotate("text",
               label = paste0(pp * 100, "%"),
               x     = perc - fsd,
               y     = max(df(l, num_df, den_df)) + 0.04,
               color = "#0000CD",
               size  = 3) +
      annotate("text",
               label = paste0(round((1 - pp) * 100, 2), "%"),
               x     = perc + fsd,
               y     = max(df(l, num_df, den_df)) + 0.02,
               color = "#6495ED",
               size  = 3)

  } else {
    gplot <-
      gplot +
      ggtitle(label    = 'f Distribution',
              subtitle = paste0("P(X > ", perc, ") = ", pp * 100, "%")) +
      annotate("text",
               label = paste0(round((1 - pp) * 100, 2), "%"),
               x     = perc - fsd,
               y     = max(df(l, num_df, den_df)) + 0.04,
               color = "#6495ED",
               size  = 3) +
      annotate("text",
               label = paste0(pp * 100, "%"),
               x     = perc + fsd,
               y     = max(df(l, num_df, den_df)) + 0.04,
               color = "#0000CD",
               size  = 3)
  }

  for (i in seq_len(length(l1))) {
    poly_data <- vdist_pol_f(lc[l1[i]], lc[l2[i]], num_df, den_df)
    gplot <-
      gplot +
      geom_polygon(data    = poly_data,
                   mapping = aes(x = x, y = y),
                   fill    = col[i])
  }

  pln <- length(pp)

  for (i in seq_len(pln)) {

    point_data <- data.frame(x = perc[i], y = 0)

    gplot <-
      gplot +
      geom_vline(xintercept = perc[i],
                 linetype   = 2,
                 size       = 1) +
      geom_point(data    = point_data,
                 mapping = aes(x = x, y = y),
                 shape   = 4,
                 color   = 'red',
                 size    = 3)
  }

  gplot <-
    gplot +
      scale_y_continuous(breaks = NULL) +
      scale_x_continuous(breaks = 0:max(l))

  if (print_plot) {
    print(gplot)
  } else {
    return(gplot)
  }

}


vdist_pol_f <- function(l1, l2, num_df, den_df) {
  x    <- c(l1, seq(l1, l2, 0.01), l2)
  y    <- c(0, df(seq(l1, l2, 0.01), num_df, den_df), 0)
  data.frame(x = x, y = y)
}
