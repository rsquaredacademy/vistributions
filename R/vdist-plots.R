#' Binomial distribution plot
#'
#' Build plot for binomial distribution.
#'
#' @param data Output returned by `bplot_data_prep()`.
#' @param n Number of trials.
#' @param p Aggregate probability.
#'
#' @noRd
#'
bplot_plot_build <- function(data, n, p) {

  p <-
    ggplot(data$plot_data) +
    geom_col(aes(x = n, y = df), fill = "blue") +
    ylab("Probability") +
    xlab("No. of success") +
    ggtitle(label    = paste("Binomial Distribution: n =", n, ", p =", p),
            subtitle = paste("Mean =", data$bm, ", Std. Dev. =", data$bsd)) +
    theme(plot.title    = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5)) +
    scale_x_continuous(breaks = seq(0, n))

  return(p)

}

bprob_plot_build <- function(data, method, n, p, s) {

  plot <-
    ggplot(data$plot_data) +
    geom_col(aes(x = n, y = df),
             fill = data$cols) +
    ylab("Probability") +
    xlab(paste("No. of success\n", "Mean =", data$bm, ", Std. Dev. =", data$bsd)) +
    scale_x_continuous(breaks = seq(0, n)) +
    theme(plot.title    = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5))

  if (method == "lower") {
    plot <-
      plot +
      ggtitle(label    = paste("Binomial Distribution: n =", n, ", p =", p),
              subtitle = paste("P(X) <=", s, "=", round(data$k, 3)))
  } else if (method == "upper") {
    plot <-
      plot +
      ggtitle(label    = paste("Binomial Distribution: n =", n, ", p =", p),
              subtitle = paste("P(X) >=", s, "=", round(data$k, 3)))
  } else if (method == "exact") {
    plot <-
      plot +
      ggtitle(label    = paste("Binomial Distribution: n =", n, ", p =", p),
              subtitle = paste("P(X) =", s, "=", round(data$k, 3)))
  } else {
    plot <-
      plot +
      ggtitle(label    = paste("Binomial Distribution: n =", n, ", p =", p),
              subtitle = paste0("P(", s[1], " <= X <= ", s[2], ")", " = ", round(data$k, 3)))
  }

  return(plot)
}

bperc_plot_build <- function(data, method, n, p, tp) {

  plot <-
    ggplot(data$plot_data) +
    geom_col(aes(x = n, y = df),
             fill = data$cols) +
    ylab("Probability") +
    xlab("No. of success") +
    scale_x_continuous(breaks = seq(0, n)) +
    theme(plot.title    = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5))

  if (method == "lower") {
    plot <-
      plot +
      ggtitle(label    = paste("Binomial Distribution: n =", n, ", p =", p),
              subtitle = paste0("P(X <= ", data$k, ") <= ", tp, ", but P(X <= ", (data$k + 1), ") > ", tp)
      )
  } else {
    plot <-
      plot +
      ggtitle(label    = paste("Binomial Distribution: n =", n, ", p =", p),
              subtitle = paste0("P(X >= ", (data$k + 1), ") <= ", tp, ", but P(X >= ", data$k, ") > ", tp)
      )
  }

  return(plot)

}

cplot_plot_build <- function(data, df, range, normal) {

  pp <-
    ggplot(data$plot_data) +
    geom_line(aes(x, chi),
              color = '#4682B4',
              linewidth  = 2) +
    ggtitle(label    = "Chi Square Distribution",
            subtitle = paste("df =", df)) +
    ylab('') +
    xlab(paste("Mean =", data$chim, " Std Dev. =", data$chisd)) +
    theme(plot.title    = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5)) +
    scale_x_continuous(breaks = seq(0, range, 2)) +
    geom_polygon(data    = data$poly_data,
                 mapping = aes(x = y, y = z),
                 fill    = '#4682B4') +
    geom_point(data    = data$point_data,
               mapping = aes(x = x, y = y),
               shape   = 4,
               color   = 'red',
               size    = 3)

  if (normal) {

    pp <-
      pp +
      geom_line(data    = data$nline_data,
              mapping = aes(x = x, y = y),
              color   = '#FF4500')

  }

  return(pp)

}

cperc_plot_build <- function(data, method, probs, df) {

  plot <-
    ggplot(data$plot_data) +
    geom_line(aes(x = x, y = y),
              color = "blue") +
    xlab(paste("Mean =", data$chim, " Std Dev. =", data$chisd)) +
    ylab('') +
    theme(plot.title    = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5))

  if (method == "lower") {
    plot <-
      plot +
      ggtitle(label    = paste("Chi Square Distribution: df =", df),
              subtitle = paste0("P(X < ", data$pp, ") = ", probs * 100, "%")) +
      annotate("text",
               label   = paste0(probs * 100, "%"),
               x       = data$pp - data$chisd,
               y       = max(dchisq(data$l, df)) + 0.02,
               color   = "#0000CD",
               size    = 3) +
      annotate("text",
               label   = paste0((1 - probs) * 100, "%"),
               x       = data$pp + data$chisd,
               y       = max(dchisq(data$l, df)) + 0.02,
               color   = "#6495ED",
               size    = 3)

  } else {
    plot <-
      plot +
      ggtitle(label    = paste("Chi Square Distribution: df =", df),
              subtitle = paste0("P(X > ", data$pp, ") = ", probs * 100, "%")) +
      annotate("text",
               label   = paste0((1 - probs) * 100, "%"),
               x       = data$pp - data$chisd,
               y       = max(dchisq(data$l, df)) + 0.02,
               color   = "#6495ED",
               size    = 3) +
      annotate("text",
               label   = paste0(probs * 100, "%"),
               x       = data$pp + data$chisd,
               y       = max(dchisq(data$l, df)) + 0.02,
               color   = "#0000CD",
               size    = 3)
  }

  for (i in seq_len(length(data$l1))) {
    pol_data <- vdist_pol_chi(data$lc[data$l1[i]], data$lc[data$l2[i]], df)
    plot <-
      plot +
      geom_polygon(data    = pol_data,
                   mapping = aes(x = x, y = y),
                   fill    = data$col[i])
  }


  plot <-
    plot +
    geom_vline(xintercept = data$pp,
               linetype   = 2,
               linewidth  = 1) +
    geom_point(data       = data$point_data,
               mapping    = aes(x = x, y = y),
               shape      = 4,
               color      = 'red',
               size       = 3) +
    scale_y_continuous(breaks = NULL) +
    scale_x_continuous(breaks = seq(0, data$xm[2], by = 5))

  return(plot)

}

cprob_plot_build <- function(data, method, perc, df) {

  gplot <-
    ggplot(data$plot_data) +
    geom_line(aes(x = x, y = y),
              color = "blue") +
    xlab(paste("Mean =", data$chim, " Std Dev. =", data$chisd)) +
    ylab('') +
    theme(plot.title    = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5))


  if (method == "lower") {
    gplot <-
      gplot +
      ggtitle(label    = paste("Chi Square Distribution: df =", df),
              subtitle = paste0("P(X < ", perc, ") = ", data$pp * 100, "%")) +
      annotate("text",
               label = paste0(data$pp * 100, "%"),
               x     = perc - data$chisd,
               y     = max(dchisq(data$l, df)) + 0.02,
               color = "#0000CD",
               size  = 3) +
      annotate("text",
               label = paste0((1 - data$pp) * 100, "%"),
               x     = perc + data$chisd,
               y     = max(dchisq(data$l, df)) + 0.02,
               color = "#6495ED",
               size  = 3)

  } else {
    gplot <-
      gplot +
      ggtitle(label    = paste("Chi Square Distribution: df =", df),
              subtitle = paste0("P(X > ", perc, ") = ", data$pp * 100, "%")) +
      annotate("text",
               label = paste0((1 - data$pp) * 100, "%"),
               x     = perc - data$chisd,
               y     = max(dchisq(data$l, df)) + 0.02,
               color = "#6495ED",
               size  = 3) +
      annotate("text",
               label = paste0(data$pp * 100, "%"),
               x     = perc + data$chisd,
               y     = max(dchisq(data$l, df)) + 0.02,
               color = "#0000CD",
               size  = 3)
  }


  for (i in seq_len(length(data$l1))) {
    pol_data <- vdist_pol_chi(data$lc[data$l1[i]], data$lc[data$l2[i]], df)
    gplot <-
      gplot +
      geom_polygon(data    = pol_data,
                   mapping = aes(x = x, y = y),
                   fill    = data$col[i])
  }

  gplot <-
    gplot +
    geom_vline(xintercept = perc,
               linetype   = 2,
               linewidth = 1) +
    geom_point(data       = data$point_data,
               mapping    = aes(x = x, y = y),
               shape      = 4,
               color      = 'red',
               size       = 3) +
    scale_y_continuous(breaks = NULL) +
    scale_x_continuous(breaks = seq(0, data$l[data$ln], by = 5))

  return(gplot)
}

vdist_pol_chi <- function(l1, l2, df) {
  x   <- c(l1, seq(l1, l2, 0.01), l2)
  y   <- c(0, dchisq(seq(l1, l2, 0.01), df), 0)
  data.frame(x = x, y = y)
}

fplot_plot_build <- function(data, num_df, den_df, normal) {

  gplot <-
    ggplot(data$plot_data) +
    geom_line(aes(x = x, y = y),
              color = "blue") +
    geom_polygon(data    = data$poly_data,
                 mapping = aes(x = y, y = z),
                 fill    = '#4682B4') +
    geom_point(data    = data$point_data,
               mapping = aes(x = x, y = y),
               shape   = 4,
               color   = 'red',
               size    = 3) +
    xlab(paste("Mean =", data$fm, " Std Dev. =", data$fsd)) +
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
      geom_line(data    = data$nline_data,
                mapping = aes(x = x, y = y),
                color   = '#FF4500')
  }

  return(gplot)
}

fperc_plot_build <- function(data, probs, num_df, den_df, method) {

  gplot <-
    ggplot(data$plot_data) +
    geom_line(data    = data$plot_data,
              mapping = aes(x = x, y = y),
              color   = 'blue') +
    xlab(paste("Mean =", data$fm, " Std Dev. =", data$fsd)) +
    ylab('') +
    theme(plot.title    = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5))


  if (method == "lower") {
    gplot <-
      gplot +
      ggtitle(label    = 'f Distribution',
              subtitle = paste0("P(X < ", data$pp, ") = ", probs * 100, "%")) +
      annotate("text",
               label = paste0(probs * 100, "%"),
               x     = data$pp - 0.2,
               y     = max(df(data$l, num_df, den_df)) + 0.02,
               color = "#0000CD",
               size  = 3) +
      annotate("text",
               label = paste0((1 - probs) * 100, "%"),
               x     = data$pp + 0.2,
               y     = max(df(data$l, num_df, den_df)) + 0.02,
               color = "#6495ED",
               size  = 3)

  } else {
    gplot <-
      gplot +
      ggtitle(label    = 'f Distribution',
              subtitle = paste0("P(X > ", data$pp, ") = ", probs * 100, "%")) +
      annotate("text",
               label = paste0((1 - probs) * 100, "%"),
               x     = data$pp - 0.2,
               y     = max(df(data$l, num_df, den_df)) + 0.02,
               color = "#6495ED",
               size  = 3) +
      annotate("text",
               label = paste0(probs * 100, "%"),
               x     = data$pp + 0.2,
               y     = max(df(data$l, num_df, den_df)) + 0.02,
               color = "#0000CD",
               size  = 3)
  }

  for (i in seq_len(length(data$l1))) {
    poly_data <- vdist_pol_f(data$lc[data$l1[i]], data$lc[data$l2[i]], num_df, den_df)
    gplot <-
      gplot +
      geom_polygon(data    = poly_data,
                   mapping = aes(x = x, y = y),
                   fill    = data$col[i])
  }

  pln <- length(data$pp)

  for (i in seq_len(pln)) {

    point_data <- data.frame(x = data$pp[i], y = 0)

    gplot <-
      gplot +
      geom_vline(xintercept = data$pp[i],
                 linetype   = 2,
                 linewidth  = 1) +
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

  return(gplot)
}

fprob_plot_build <- function(data, perc, num_df, den_df, method) {

  gplot <-
    ggplot(data$plot_data) +
    geom_line(data    = data$plot_data,
              mapping = aes(x = x, y = y),
              color   = 'blue') +
    xlab(paste("Mean =", data$fm, " Std Dev. =", data$fsd)) +
    ylab('') +
    theme(plot.title    = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5))

  if (method == "lower") {
    gplot <-
      gplot +
      ggtitle(label    = 'f Distribution',
              subtitle = paste0("P(X < ", perc, ") = ", data$pp * 100, "%")) +
      annotate("text",
               label = paste0(data$pp * 100, "%"),
               x     = perc - data$fsd,
               y     = max(df(data$l, num_df, den_df)) + 0.04,
               color = "#0000CD",
               size  = 3) +
      annotate("text",
               label = paste0(round((1 - data$pp) * 100, 2), "%"),
               x     = perc + data$fsd,
               y     = max(df(data$l, num_df, den_df)) + 0.02,
               color = "#6495ED",
               size  = 3)

  } else {
    gplot <-
      gplot +
      ggtitle(label    = 'f Distribution',
              subtitle = paste0("P(X > ", perc, ") = ", data$pp * 100, "%")) +
      annotate("text",
               label = paste0(round((1 - data$pp) * 100, 2), "%"),
               x     = perc - data$fsd,
               y     = max(df(data$l, num_df, den_df)) + 0.04,
               color = "#6495ED",
               size  = 3) +
      annotate("text",
               label = paste0(data$pp * 100, "%"),
               x     = perc + data$fsd,
               y     = max(df(data$l, num_df, den_df)) + 0.04,
               color = "#0000CD",
               size  = 3)
  }

  for (i in seq_len(length(data$l1))) {
    poly_data <- vdist_pol_f(data$lc[data$l1[i]], data$lc[data$l2[i]], num_df, den_df)
    gplot <-
      gplot +
      geom_polygon(data    = poly_data,
                   mapping = aes(x = x, y = y),
                   fill    = data$col[i])
  }

  pln <- length(data$pp)

  for (i in seq_len(pln)) {

    point_data <- data.frame(x = perc[i], y = 0)

    gplot <-
      gplot +
      geom_vline(xintercept = perc[i],
                 linetype   = 2,
                 linewidth  = 1) +
      geom_point(data    = point_data,
                 mapping = aes(x = x, y = y),
                 shape   = 4,
                 color   = 'red',
                 size    = 3)
  }

  gplot <-
    gplot +
    scale_y_continuous(breaks = NULL) +
    scale_x_continuous(breaks = 0:max(data$l))

  return(gplot)

}

vdist_pol_f <- function(l1, l2, num_df, den_df) {
  x    <- c(l1, seq(l1, l2, 0.01), l2)
  y    <- c(0, df(seq(l1, l2, 0.01), num_df, den_df), 0)
  data.frame(x = x, y = y)
}

nplot_plot_build <- function(data, mean, sd) {

  gplot <-
    ggplot(data$plot_data) +
    geom_line(aes(x = x, y = y)) +
    xlab('') +
    ylab('') +
    ggtitle(label    = "Normal Distribution",
            subtitle = paste("Mean:", mean, "     Standard Deviation:", sd)) +
    theme(plot.title    = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5))

  for (i in seq_len(length(data$l1))) {
    poly_data <- vdist_pol_cord(data$ll[data$l1[i]], data$ll[data$l2[i]], mean, sd)
    gplot <-
      gplot +
      geom_polygon(data    = poly_data,
                   mapping = aes(x = x, y = y),
                   fill    = data$col[i])
  }

  return(gplot)

}

vdist_pol_cord <- function(l1, l2, mean, sd) {
  x <- c(l1, seq(l1, l2, 0.01), l2)
  y <- c(0, dnorm(seq(l1, l2, 0.01), mean, sd), 0)
  data.frame(x = x, y = y)
}

nperc_plot_build <- function(data, probs, mean, sd, method) {

  gplot <-
    ggplot(data$plot_data) +
    geom_line(aes(x = x, y = y)) +
    xlab(paste("Mean:", mean, " Standard Deviation:", sd)) +
    ylab('') +
    theme(plot.title    = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5))

  if (method == "lower") {
    gplot <-
      gplot +
      ggtitle(label    = "Normal Distribution",
              subtitle = paste0("P(X < ", data$pp, ") = ", probs * 100, "%")) +
      annotate("text",
               label = paste0(probs * 100, "%"),
               x     = data$pp - sd,
               y     = max(dnorm(data$x, mean, sd)) + 0.025,
               color = "#0000CD",
               size  = 3) +
      annotate("text",
               label = paste0((1 - probs) * 100, "%"),
               x     = data$pp + sd,
               y     = max(dnorm(data$x, mean, sd)) + 0.025,
               color = "#6495ED",
               size  = 3)

  } else if (method == "upper") {
    gplot <-
      gplot +
      ggtitle(label    = "Normal Distribution",
              subtitle = paste0("P(X > ", data$pp, ") = ", probs * 100, "%")) +
      annotate("text",
               label = paste0((1 - probs) * 100, "%"),
               x     = data$pp - sd,
               y     = max(dnorm(data$x, mean, sd)) + 0.025,
               color = "#6495ED",
               size  = 3) +
      annotate("text",
               label = paste0(probs * 100, "%"),
               x     = data$pp + sd,
               y     = max(dnorm(data$x, mean, sd)) + 0.025,
               color = "#0000CD",
               size  = 3)
  } else {
    gplot <-
      gplot +
      ggtitle(label    = "Normal Distribution",
              subtitle = paste0("P(", data$pp[1], " < X < ", data$pp[2], ") = ", probs * 100, "%")) +
      annotate("text",
               label = paste0(probs * 100, "%"),
               x     = mean,
               y     = max(dnorm(data$x, mean, sd)) + 0.025,
               color = "#0000CD",
               size  = 3) +
      annotate("text",
               label = paste0(data$alpha * 100, "%"),
               x     = data$pp[1] - sd,
               y     = max(dnorm(data$x, mean, sd)) + 0.025,
               color = "#6495ED",
               size  = 3) +
      annotate("text",
               label = paste0(data$alpha * 100, "%"),
               x     = data$pp[2] + sd,
               y     = max(dnorm(data$x, mean, sd)) + 0.025,
               color = "#6495ED",
               size  = 3)
  }

  for (i in seq_len(length(data$l1))) {
    poly_data <- vdist_pol_cord(data$lc[data$l1[i]], data$lc[data$l2[i]], mean, sd)
    gplot <-
      gplot +
      geom_polygon(data    = poly_data,
                   mapping = aes(x = x, y = y),
                   fill    = data$col[i])
  }

  pln <- length(data$pp)

  for (i in seq_len(pln)) {

    point_data <- data.frame(x = data$pp[i], y = 0)

    gplot <-
      gplot +
      geom_vline(xintercept = data$pp[i],
                 linetype   = 2,
                 linewidth  = 1) +
      geom_point(data    = point_data,
                 mapping = aes(x = x, y = y),
                 shape   = 4,
                 color   = 'red',
                 size    = 3)
  }

  gplot <-
    gplot +
    scale_y_continuous(breaks = NULL) +
    scale_x_continuous(breaks = data$l)

  return(gplot)
}

nprob_plot_build <- function(data, perc, mean, sd, method) {

  gplot <-
    ggplot(data$plot_data) +
    geom_line(aes(x = x, y = y)) +
    xlab(paste("Mean:", mean, " Standard Deviation:", sd)) +
    ylab('') +
    theme(plot.title    = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5))

  if (method == "lower") {
    gplot <-
      gplot +
      ggtitle(label    = "Normal Distribution",
              subtitle = paste0("P(X < ", perc, ") = ", data$pp * 100, "%")) +
      annotate("text",
               label = paste0(data$pp * 100, "%"),
               x     = perc - sd,
               y     = max(dnorm(data$x, mean, sd)) + 0.07,
               color = "#0000CD",
               size  = 3) +
      annotate("text",
               label = paste0((1 - data$pp) * 100, "%"),
               x     = perc + sd,
               y     = max(dnorm(data$x, mean, sd)) + 0.07,
               color = "#6495ED",
               size  = 3)

  } else if (method == "upper") {
    gplot <-
      gplot +
      ggtitle(label    = "Normal Distribution",
              subtitle = paste0("P(X > ", perc, ") = ", data$pp * 100, "%")) +
      annotate("text",
               label = paste0((1 - data$pp) * 100, "%"),
               x     = perc - sd,
               y     = max(dnorm(data$x, mean, sd)) + 0.07,
               color = "#6495ED",
               size  = 3) +
      annotate("text",
               label = paste0(data$pp * 100, "%"),
               x     = perc + sd,
               y     = max(dnorm(data$x, mean, sd)) + 0.07,
               color = "#0000CD",
               size  = 3)
  } else {
    gplot <-
      gplot +
      ggtitle(label    = "Normal Distribution",
              subtitle = paste0("P(", perc[1], " < X < ", perc[2], ") = ", (1 - (data$pp1 + data$pp2)) * 100, "%")) +
      annotate("text",
               label = paste0((1 - (data$pp1 + data$pp2)) * 100, "%"),
               x     = mean(perc),
               y     = max(dnorm(data$x, mean, sd)) + 0.07,
               color = "#0000CD",
               size  = 3) +
      annotate("text",
               label = paste0(data$pp[1] * 100, "%"),
               x     = perc[1] - sd,
               y     = max(dnorm(data$x, mean, sd)) + 0.07,
               color = "#6495ED",
               size  = 3) +
      annotate("text",
               label = paste0(data$pp[2] * 100, "%"),
               x     = perc[2] + sd,
               y     = max(dnorm(data$x, mean, sd)) + 0.07,
               color = "#6495ED",
               size  = 3)
  }

  for (i in seq_len(length(data$l1))) {
    poly_data <- vdist_pol_cord(data$lc[data$l1[i]], data$lc[data$l2[i]], mean, sd)
    gplot <-
      gplot +
      geom_polygon(data    = poly_data,
                   mapping = aes(x = x, y = y),
                   fill    = data$col[i])
  }

  pln <- length(data$pp)

  for (i in seq_len(pln)) {

    point_data <- data.frame(x = perc[i], y = 0)

    gplot <-
      gplot +
      geom_vline(xintercept = perc[i],
                 linetype   = 2,
                 linewidth  = 1) +
      geom_point(data    = point_data,
                 mapping = aes(x = x, y = y),
                 shape   = 4,
                 color   = 'red',
                 size    = 3)
  }

  gplot <-
    gplot +
    scale_y_continuous(breaks = NULL) +
    scale_x_continuous(breaks = data$l)

  return(gplot)

}

tplot_plot_build <- function(data, df) {

  gplot <-
    ggplot(data$plot_data) +
    geom_line(aes(x = x, y = y),
              color = 'blue') +
    ggtitle(label    = 't Distribution',
            subtitle = paste("df =", df)) +
    xlab('') +
    ylab('') +
    geom_polygon(data    = data$poly_data,
                 mapping = aes(x = y, y = z),
                 fill    = '#4682B4') +
    scale_y_continuous(breaks = NULL) +
    scale_x_continuous(breaks = -4:4) +
    theme(plot.title    = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5))

  return(gplot)

}

tperc_plot_build <- function(data, probs, df, method) {

  gplot <-
    ggplot(data$plot_data) +
    geom_line(aes(x = x, y = y),
              color = 'blue') +
    xlab(paste("df =", df)) +
    ylab('') +
    theme(plot.title    = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5))

  if (method == "lower") {
    gplot <-
      gplot +
      ggtitle(label    = "t Distribution",
              subtitle = paste0("P(X < ", data$pp, ") = ", probs * 100, "%")) +
      annotate("text",
               label = paste0(probs * 100, "%"),
               x     = data$pp - 0.3,
               y     = max(dt(data$l, df)) + 0.025,
               color = "#0000CD",
               size  = 3) +
      annotate("text",
               label = paste0((1 - probs) * 100, "%"),
               x     = data$pp + 0.3,
               y     = max(dt(data$l, df)) + 0.025,
               color = "#6495ED",
               size  = 3)

  } else if (method == "upper") {
    gplot <-
      gplot +
      ggtitle(label    = "t Distribution",
              subtitle = paste0("P(X > ", data$pp, ") = ", probs * 100, "%")) +
      annotate("text",
               label = paste0((1 - probs) * 100, "%"),
               x     = data$pp - 0.3,
               y     = max(dt(data$l, df)) + 0.025,
               color = "#6495ED",
               size  = 3) +
      annotate("text",
               label = paste0(probs * 100, "%"),
               x     = data$pp + 0.3,
               y     = max(dt(data$l, df)) + 0.025,
               color = "#0000CD",
               size  = 3)
  } else {
    gplot <-
      gplot +
      ggtitle(label    = "t Distribution",
              subtitle = paste0("P(", data$pp[1], " < X < ", data$pp[2], ") = ", probs * 100, "%")) +
      annotate("text",
               label = paste0(probs * 100, "%"),
               x     = mean(data$l),
               y     = max(dt(data$l, df)) + 0.025,
               color = "#0000CD",
               size  = 3) +
      annotate("text",
               label = paste0(data$alpha * 100, "%"),
               x     = data$pp[1] - 0.3,
               y     = max(dt(data$l, df)) + 0.025,
               color = "#6495ED",
               size  = 3) +
      annotate("text",
               label = paste0(data$alpha * 100, "%"),
               x     = data$pp[2] + 0.3,
               y     = max(dt(data$l, df)) + 0.025,
               color = "#6495ED",
               size  = 3)
  }

  for (i in seq_len(length(data$l1))) {
    poly_data <- vdist_pol_t(data$lc[data$l1[i]], data$lc[data$l2[i]], df)
    gplot <-
      gplot +
      geom_polygon(data    = poly_data,
                   mapping = aes(x = x, y = y),
                   fill    = data$col[i])
  }

  pln <- length(data$pp)

  for (i in seq_len(pln)) {

    point_data <- data.frame(x = data$pp[i], y = 0)

    gplot <-
      gplot +
      geom_vline(xintercept = data$pp[i],
                 linetype   = 2,
                 linewidth  = 1) +
      geom_point(data    = point_data,
                 mapping = aes(x = x, y = y),
                 shape   = 4,
                 color   = 'red',
                 size    = 3)
  }

  gplot <-
    gplot +
    scale_y_continuous(breaks = NULL) +
    scale_x_continuous(breaks = -5:5)

  return(gplot)
}

tprob_plot_build <- function(data, perc, df, method) {

  gplot <-
    ggplot(data$plot_data) +
    geom_line(aes(x = x, y = y),
              color = 'blue') +
    xlab(paste("df =", df)) +
    ylab('') +
    theme(plot.title    = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5)) +
    scale_x_continuous(breaks = min(data$l):max(data$l)) +
    scale_y_continuous(breaks = NULL)

  for (i in seq_len(length(data$l1))) {
    poly_data <- vdist_pol_t(data$lc[data$l1[i]], data$lc[data$l2[i]], df)
    gplot <-
      gplot +
      geom_polygon(data    = poly_data,
                   mapping = aes(x = x, y = y),
                   fill    = data$col[i])
  }


  if (method == "lower") {

    point_data <- data.frame(x = perc, y = 0)

    gplot <-
      gplot +
      ggtitle(label    = "t Distribution",
              subtitle = paste0("P(X < ", perc, ") = ", data$pp * 100, "%")) +
      annotate("text",
               label = paste0(data$pp * 100, "%"),
               x     = perc - 1,
               y     = max(dt(data$l, df)) + 0.07,
               color = "#0000CD",
               size  = 3) +
      annotate("text",
               label = paste0((1 - data$pp) * 100, "%"),
               x     = perc + 1,
               y     = max(dt(data$l, df)) + 0.07,
               color = "#6495ED",
               size  = 3) +
      geom_vline(xintercept = perc,
                 linetype   = 2,
                 linewidth  = 1) +
      geom_point(data    = point_data,
                 mapping = aes(x = x, y = y),
                 shape   = 4,
                 color   = 'red',
                 size    = 3)

  } else if (method == "upper") {

    point_data <- data.frame(x = perc, y = 0)

    gplot <-
      gplot +
      ggtitle(label    = "t Distribution",
              subtitle = paste0("P(X > ", perc, ") = ", data$pp * 100, "%")) +
      annotate("text",
               label = paste0((1 - data$pp) * 100, "%"),
               x     = perc - 1,
               y     = max(dt(data$l, df)) + 0.07,
               color = "#0000CD",
               size  = 3) +
      annotate("text",
               label = paste0(data$pp * 100, "%"),
               x     = perc + 1,
               y     = max(dt(data$l, df)) + 0.07,
               color = "#6495ED",
               size  = 3) +
      geom_vline(xintercept = perc,
                 linetype   = 2,
                 linewidth  = 1) +
      geom_point(data    = point_data,
                 mapping = aes(x = x, y = y),
                 shape   = 4,
                 color   = 'red',
                 size    = 3)

  } else if (method == "interval") {

    point_data <- data.frame(x1 = perc, x2 = -perc, y = 0)

    gplot <-
      gplot +
      ggtitle(label    = "t Distribution",
              subtitle = paste0("P(", -perc, " < X < ", perc, ") = ", (1 - (data$pp1 + data$pp2)) * 100, "%")) +
      annotate("text",
               label = paste0((1 - (data$pp1 + data$pp2)) * 100, "%"),
               x     = 0,
               y     = max(dt(data$l, df)) + 0.07,
               color = "#0000CD",
               size  = 3) +
      annotate("text",
               label = paste0(data$pp[1] * 100, "%"),
               x     = perc + 1,
               y     = max(dt(data$l, df)) + 0.07,
               color = "#6495ED",
               size  = 3) +
      annotate("text",
               label = paste0(data$pp[2] * 100, "%"),
               x     = -perc - 1,
               y     = max(dt(data$l, df)) + 0.07,
               color = "#6495ED",
               size  = 3) +
      geom_vline(xintercept = perc,
                 linetype   = 2,
                 linewidth  = 1) +
      geom_vline(xintercept = -perc,
                 linetype   = 2,
                 linewidth  = 1) +
      geom_point(data    = point_data,
                 mapping = aes(x = x1, y = y),
                 shape   = 4,
                 color   = 'red',
                 size    = 3) +
      geom_point(data    = point_data,
                 mapping = aes(x = x2, y = y),
                 shape   = 4,
                 color   = 'red',
                 size    = 3)
  } else {

    point_data <- data.frame(x1 = perc, x2 = -perc, y = 0)

    gplot <-
      gplot +
      ggtitle(label    = "t Distribution",
              subtitle = paste0("P(|X| > ", perc, ") = ", (data$pp1 + data$pp2) * 100, "%")) +
      annotate("text",
               label = paste0((1 - (data$pp1 + data$pp2)) * 100, "%"),
               x     = 0,
               y     = max(dt(data$l, df)) + 0.07,
               color = "#0000CD",
               size  = 3) +
      annotate("text",
               label = paste0(data$pp[1] * 100, "%"),
               x     = perc + 1,
               y     = max(dt(data$l, df)) + 0.07,
               color = "#6495ED",
               size  = 3) +
      annotate("text",
               label = paste0(data$pp[2] * 100, "%"),
               x     = -perc - 1,
               y     = max(dt(data$l, df)) + 0.07,
               color = "#6495ED",
               size  = 3) +
      geom_vline(xintercept = perc,
                 linetype   = 2,
                 linewidth  = 1) +
      geom_vline(xintercept = -perc,
                 linetype   = 2,
                 linewidth  = 1) +
      geom_point(data    = point_data,
                 mapping = aes(x = x1, y = y),
                 shape   = 4,
                 color   = 'red',
                 size    = 3) +
      geom_point(data    = point_data,
                 mapping = aes(x = x2, y = y),
                 shape   = 4,
                 color   = 'red',
                 size    = 3)
  }

  return(gplot)

}


vdist_pol_t <- function(l1, l2, df) {
  x    <- c(l1, seq(l1, l2, 0.01), l2)
  y    <- c(0, dt(seq(l1, l2, 0.01), df), 0)
  data.frame(x = x, y = y)
}
