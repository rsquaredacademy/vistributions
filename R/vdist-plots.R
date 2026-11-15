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
              subtitle = paste0("P(X <= ", (data$k - 1), ") < ", tp, ", but P(X <= ", data$k, ") >= ", tp)
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
    xlab(paste("Mean =", fmt_num(df), " Std Dev. =", fmt_num(sqrt(2 * df)))) +
    theme(plot.title    = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5)) +
    scale_x_continuous(breaks = if (range <= 100) seq(0, range, 2) else waiver()) +
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

  other_text <- vdist_pct_text(1 - data$event_prob)

  plot <-
    ggplot(data$plot_data) +
    geom_line(aes(x = x, y = y),
              color = "blue") +
    xlab(paste("Mean =", fmt_num(df), " Std Dev. =", fmt_num(sqrt(2 * df)))) +
    ylab('') +
    theme(plot.title    = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5))

  if (method == "lower") {
    plot <-
      plot +
      ggtitle(label    = paste("Chi Square Distribution: df =", df),
              subtitle = paste0("P(X < ", data$cut_text, ") = ", data$prob_text, "%")) +
      annotate("text",
               label   = paste0(data$prob_text, "%"),
               x       = vdist_clamp(data$pc - data$chisd, data$l),
               y       = max(dchisq(data$l, df)) + 0.02,
               color   = "#0000CD",
               size    = 3) +
      annotate("text",
               label   = paste0(other_text, "%"),
               x       = vdist_clamp(data$pc + data$chisd, data$l),
               y       = max(dchisq(data$l, df)) + 0.02,
               color   = "#6495ED",
               size    = 3)

  } else {
    plot <-
      plot +
      ggtitle(label    = paste("Chi Square Distribution: df =", df),
              subtitle = paste0("P(X > ", data$cut_text, ") = ", data$prob_text, "%")) +
      annotate("text",
               label   = paste0(other_text, "%"),
               x       = vdist_clamp(data$pc - data$chisd, data$l),
               y       = max(dchisq(data$l, df)) + 0.02,
               color   = "#6495ED",
               size    = 3) +
      annotate("text",
               label   = paste0(data$prob_text, "%"),
               x       = vdist_clamp(data$pc + data$chisd, data$l),
               y       = max(dchisq(data$l, df)) + 0.02,
               color   = "#0000CD",
               size    = 3)
  }

  dark <- 0
  for (i in seq_len(length(data$l1))) {
    pol_data <- vdist_pol_chi(data$lc[data$l1[i]], data$lc[data$l2[i]], df)
    if (identical(data$col[i], "#0000CD")) {
      dark <- dark + vdist_area(pol_data$x, pol_data$y)
    }
    plot <-
      plot +
      geom_polygon(data    = pol_data,
                   mapping = aes(x = x, y = y),
                   fill    = data$col[i])
  }
  vdist_check_event(dark, data$omitted, data$event_prob,
                    "chi-square shaded region")

  plot <-
    plot +
    geom_vline(xintercept = data$pc,
               linetype   = 2,
               linewidth  = 1) +
    geom_point(data       = data$point_data,
               mapping    = aes(x = x, y = y),
               shape      = 4,
               color      = 'red',
               size       = 3) +
    scale_y_continuous(breaks = NULL) +
    scale_x_continuous(breaks = if (data$xm[2] <= 300) seq(0, data$xm[2], by = 5) else waiver())

  return(plot)

}

cprob_plot_build <- function(data, method, perc, df) {

  other_text <- vdist_pct_text(1 - data$event_prob)

  gplot <-
    ggplot(data$plot_data) +
    geom_line(aes(x = x, y = y),
              color = "blue") +
    xlab(paste("Mean =", fmt_num(df), " Std Dev. =", fmt_num(sqrt(2 * df)))) +
    ylab('') +
    theme(plot.title    = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5))


  if (method == "lower") {
    gplot <-
      gplot +
      ggtitle(label    = paste("Chi Square Distribution: df =", df),
              subtitle = paste0("P(X < ", data$cut_text, ") = ", data$prob_text, "%")) +
      annotate("text",
               label = paste0(data$prob_text, "%"),
               x     = vdist_clamp(data$pc - data$chisd, data$l),
               y     = max(dchisq(data$l, df)) + 0.02,
               color = "#0000CD",
               size  = 3) +
      annotate("text",
               label = paste0(other_text, "%"),
               x     = vdist_clamp(data$pc + data$chisd, data$l),
               y     = max(dchisq(data$l, df)) + 0.02,
               color = "#6495ED",
               size  = 3)

  } else {
    gplot <-
      gplot +
      ggtitle(label    = paste("Chi Square Distribution: df =", df),
              subtitle = paste0("P(X > ", data$cut_text, ") = ", data$prob_text, "%")) +
      annotate("text",
               label = paste0(other_text, "%"),
               x     = vdist_clamp(data$pc - data$chisd, data$l),
               y     = max(dchisq(data$l, df)) + 0.02,
               color = "#6495ED",
               size  = 3) +
      annotate("text",
               label = paste0(data$prob_text, "%"),
               x     = vdist_clamp(data$pc + data$chisd, data$l),
               y     = max(dchisq(data$l, df)) + 0.02,
               color = "#0000CD",
               size  = 3)
  }

  dark <- 0
  for (i in seq_len(length(data$l1))) {
    pol_data <- vdist_pol_chi(data$lc[data$l1[i]], data$lc[data$l2[i]], df)
    if (identical(data$col[i], "#0000CD")) {
      dark <- dark + vdist_area(pol_data$x, pol_data$y)
    }
    gplot <-
      gplot +
      geom_polygon(data    = pol_data,
                   mapping = aes(x = x, y = y),
                   fill    = data$col[i])
  }
  vdist_check_event(dark, data$omitted, data$event_prob,
                    "chi-square shaded region")

  gplot <-
    gplot +
    geom_vline(xintercept = data$pc,
               linetype   = 2,
               linewidth = 1) +
    geom_point(data       = data$point_data,
               mapping    = aes(x = x, y = y),
               shape      = 4,
               color      = 'red',
               size       = 3) +
    scale_y_continuous(breaks = NULL) +
    scale_x_continuous(breaks = if (data$l[data$ln] <= 300) seq(0, data$l[data$ln], by = 5) else waiver())

  return(gplot)
}

# x coordinates of a filled polygon: the sampled grid, closed at the requested
# endpoints. An endpoint beyond the sampled central mass was clipped by
# vdist_grid, so the polygon stops at the grid edge instead.
vdist_pol_x <- function(g, l1, l2) {
  clipped <- attr(g, "clipped")
  if (is.null(clipped)) {
    clipped <- c(FALSE, FALSE)
  }
  c(if (clipped[1]) min(g) else l1, g, if (clipped[2]) max(g) else l2)
}

vdist_pol_chi <- function(l1, l2, df) {
  pfun <- function(x) pchisq(x, df)
  qfun <- function(p) qchisq(p, df)
  g    <- vdist_grid(l1, l2, pfun, qfun, quantile = TRUE,
                     n = vdist_grid_n(df < 2))
  x    <- vdist_pol_x(g, l1, l2)
  y    <- c(0, dchisq(g, df), 0)
  vdist_check_area(x, y, pfun(l2) - pfun(l1), "chi-square shaded region")
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
    xlab(paste("Mean =", fmt_moment(data$fm), " Std Dev. =", fmt_moment(data$fsd))) +
    ylab('') +
    ggtitle(label    = 'f Distribution',
            subtitle = paste("Num df =", num_df, "  Den df =", den_df)) +
    theme(plot.title    = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5)) +
    scale_x_continuous(breaks = c(-2:4)) +
    scale_y_continuous(breaks = NULL)

  if (normal && nrow(data$nline_data) > 0) {
    gplot <-
      gplot +
      geom_line(data    = data$nline_data,
                mapping = aes(x = x, y = y),
                color   = '#FF4500')
  } else if (normal) {
    message(paste("Normal overlay omitted: the F distribution standard",
                  "deviation is undefined when den_df <= 4."))
  }

  return(gplot)
}

fperc_plot_build <- function(data, probs, num_df, den_df, method) {

  gplot <-
    ggplot(data$plot_data) +
    geom_line(data    = data$plot_data,
              mapping = aes(x = x, y = y),
              color   = 'blue') +
    xlab(paste("Mean =", fmt_moment(data$fm), " Std Dev. =", fmt_moment(data$fsd))) +
    ylab('') +
    theme(plot.title    = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5))


  other_text <- vdist_pct_text(1 - data$event_prob)

  if (method == "lower") {
    gplot <-
      gplot +
      ggtitle(label    = 'f Distribution',
              subtitle = paste0("P(X < ", data$cut_text, ") = ", data$prob_text, "%")) +
      annotate("text",
               label = paste0(data$prob_text, "%"),
               x     = vdist_clamp(data$pc - 0.2, data$l),
               y     = max(df(data$l, num_df, den_df)) + 0.02,
               color = "#0000CD",
               size  = 3) +
      annotate("text",
               label = paste0(other_text, "%"),
               x     = vdist_clamp(data$pc + 0.2, data$l),
               y     = max(df(data$l, num_df, den_df)) + 0.02,
               color = "#6495ED",
               size  = 3)

  } else {
    gplot <-
      gplot +
      ggtitle(label    = 'f Distribution',
              subtitle = paste0("P(X > ", data$cut_text, ") = ", data$prob_text, "%")) +
      annotate("text",
               label = paste0(other_text, "%"),
               x     = vdist_clamp(data$pc - 0.2, data$l),
               y     = max(df(data$l, num_df, den_df)) + 0.02,
               color = "#6495ED",
               size  = 3) +
      annotate("text",
               label = paste0(data$prob_text, "%"),
               x     = vdist_clamp(data$pc + 0.2, data$l),
               y     = max(df(data$l, num_df, den_df)) + 0.02,
               color = "#0000CD",
               size  = 3)
  }

  dark <- 0
  for (i in seq_len(length(data$l1))) {
    poly_data <- vdist_pol_f(data$lc[data$l1[i]], data$lc[data$l2[i]], num_df, den_df)
    if (identical(data$col[i], "#0000CD")) {
      dark <- dark + vdist_area(poly_data$x, poly_data$y)
    }
    gplot <-
      gplot +
      geom_polygon(data    = poly_data,
                   mapping = aes(x = x, y = y),
                   fill    = data$col[i])
  }
  vdist_check_event(dark, data$omitted, data$event_prob, "F shaded region")

  pln <- length(data$pp)

  for (i in seq_len(pln)) {

    point_data <- data.frame(x = data$pc[i], y = 0)

    gplot <-
      gplot +
      geom_vline(xintercept = data$pc[i],
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
    xlab(paste("Mean =", fmt_moment(data$fm), " Std Dev. =", fmt_moment(data$fsd))) +
    ylab('') +
    theme(plot.title    = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5))

  fsd_offset <- if (!is.na(data$fsd)) data$fsd else 0.5
  other_text <- vdist_pct_text(1 - data$event_prob)

  if (method == "lower") {
    gplot <-
      gplot +
      ggtitle(label    = 'f Distribution',
              subtitle = paste0("P(X < ", data$cut_text, ") = ", data$prob_text, "%")) +
      annotate("text",
               label = paste0(data$prob_text, "%"),
               x     = vdist_clamp(data$pc - fsd_offset, data$l),
               y     = max(df(data$l, num_df, den_df)) + 0.04,
               color = "#0000CD",
               size  = 3) +
      annotate("text",
               label = paste0(other_text, "%"),
               x     = vdist_clamp(data$pc + fsd_offset, data$l),
               y     = max(df(data$l, num_df, den_df)) + 0.02,
               color = "#6495ED",
               size  = 3)

  } else {
    gplot <-
      gplot +
      ggtitle(label    = 'f Distribution',
              subtitle = paste0("P(X > ", data$cut_text, ") = ", data$prob_text, "%")) +
      annotate("text",
               label = paste0(other_text, "%"),
               x     = vdist_clamp(data$pc - fsd_offset, data$l),
               y     = max(df(data$l, num_df, den_df)) + 0.04,
               color = "#6495ED",
               size  = 3) +
      annotate("text",
               label = paste0(data$prob_text, "%"),
               x     = vdist_clamp(data$pc + fsd_offset, data$l),
               y     = max(df(data$l, num_df, den_df)) + 0.04,
               color = "#0000CD",
               size  = 3)
  }

  dark <- 0
  for (i in seq_len(length(data$l1))) {
    poly_data <- vdist_pol_f(data$lc[data$l1[i]], data$lc[data$l2[i]], num_df, den_df)
    if (identical(data$col[i], "#0000CD")) {
      dark <- dark + vdist_area(poly_data$x, poly_data$y)
    }
    gplot <-
      gplot +
      geom_polygon(data    = poly_data,
                   mapping = aes(x = x, y = y),
                   fill    = data$col[i])
  }
  vdist_check_event(dark, data$omitted, data$event_prob, "F shaded region")

  pln <- length(data$pp)

  for (i in seq_len(pln)) {

    point_data <- data.frame(x = data$pc[i], y = 0)

    gplot <-
      gplot +
      geom_vline(xintercept = data$pc[i],
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
    scale_x_continuous(breaks = if (max(data$l) <= 60) 0:max(data$l) else waiver())

  return(gplot)

}

vdist_pol_f <- function(l1, l2, num_df, den_df) {
  pfun <- function(x) pf(x, num_df, den_df)
  qfun <- function(p) qf(p, num_df, den_df)
  g <- vdist_f_grid(l1, l2, num_df, den_df, quantile = TRUE)
  x <- vdist_pol_x(g, l1, l2)
  y <- c(0, df(g, num_df, den_df), 0)
  vdist_check_area(x, y, pfun(l2) - pfun(l1), "F shaded region")
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

vdist_pol_cord <- function(l1, l2, mean, sd, n = 1001L) {
  pfun <- function(x) pnorm(x, mean, sd)
  qfun <- function(p) qnorm(p, mean, sd)
  g <- vdist_grid(l1, l2, pfun, qfun, quantile = TRUE, n = n)
  x <- vdist_pol_x(g, l1, l2)
  y <- c(0, dnorm(g, mean, sd), 0)
  vdist_check_area(x, y, pfun(l2) - pfun(l1), "normal shaded region")
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

  other_text <- vdist_pct_text(1 - data$event_prob)

  if (method == "lower") {
    gplot <-
      gplot +
      ggtitle(label    = "Normal Distribution",
              subtitle = paste0("P(X < ", data$cut_text, ") = ", data$prob_text, "%")) +
      annotate("text",
               label = paste0(data$prob_text, "%"),
               x     = vdist_clamp(data$pc - sd, data$l),
               y     = max(dnorm(data$x, mean, sd)) + 0.025,
               color = "#0000CD",
               size  = 3) +
      annotate("text",
               label = paste0(other_text, "%"),
               x     = vdist_clamp(data$pc + sd, data$l),
               y     = max(dnorm(data$x, mean, sd)) + 0.025,
               color = "#6495ED",
               size  = 3)

  } else if (method == "upper") {
    gplot <-
      gplot +
      ggtitle(label    = "Normal Distribution",
              subtitle = paste0("P(X > ", data$cut_text, ") = ", data$prob_text, "%")) +
      annotate("text",
               label = paste0(other_text, "%"),
               x     = vdist_clamp(data$pc - sd, data$l),
               y     = max(dnorm(data$x, mean, sd)) + 0.025,
               color = "#6495ED",
               size  = 3) +
      annotate("text",
               label = paste0(data$prob_text, "%"),
               x     = vdist_clamp(data$pc + sd, data$l),
               y     = max(dnorm(data$x, mean, sd)) + 0.025,
               color = "#0000CD",
               size  = 3)
  } else {
    tail_text <- vdist_pct_text(data$alpha)
    gplot <-
      gplot +
      ggtitle(label    = "Normal Distribution",
              subtitle = paste0("P(", data$cut_text[1], " < X < ", data$cut_text[2], ") = ", data$prob_text, "%")) +
      annotate("text",
               label = paste0(data$prob_text, "%"),
               x     = vdist_clamp(mean(data$pc), data$l),
               y     = max(dnorm(data$x, mean, sd)) + 0.025,
               color = "#0000CD",
               size  = 3) +
      annotate("text",
               label = paste0(tail_text, "%"),
               x     = vdist_clamp(data$pc[1] - sd, data$l),
               y     = max(dnorm(data$x, mean, sd)) + 0.025,
               color = "#6495ED",
               size  = 3) +
      annotate("text",
               label = paste0(tail_text, "%"),
               x     = vdist_clamp(data$pc[2] + sd, data$l),
               y     = max(dnorm(data$x, mean, sd)) + 0.025,
               color = "#6495ED",
               size  = 3)
  }

  dark <- 0
  for (i in seq_len(length(data$l1))) {
    poly_data <- vdist_pol_cord(data$lc[data$l1[i]], data$lc[data$l2[i]], mean, sd)
    if (identical(data$col[i], "#0000CD")) {
      dark <- dark + vdist_area(poly_data$x, poly_data$y)
    }
    gplot <-
      gplot +
      geom_polygon(data    = poly_data,
                   mapping = aes(x = x, y = y),
                   fill    = data$col[i])
  }
  vdist_check_event(dark, data$omitted, data$event_prob, "normal shaded region")

  pln <- length(data$pc)

  for (i in seq_len(pln)) {

    point_data <- data.frame(x = data$pc[i], y = 0)

    gplot <-
      gplot +
      geom_vline(xintercept = data$pc[i],
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
    scale_x_continuous(breaks = if (length(data$l) <= 60) data$l else waiver())

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

  other_text <- vdist_pct_text(1 - data$event_prob)

  if (method == "lower") {
    gplot <-
      gplot +
      ggtitle(label    = "Normal Distribution",
              subtitle = paste0("P(X < ", data$cut_text, ") = ", data$prob_text, "%")) +
      annotate("text",
               label = paste0(data$prob_text, "%"),
               x     = vdist_clamp(data$pc - sd, data$l),
               y     = max(dnorm(data$x, mean, sd)) + 0.07,
               color = "#0000CD",
               size  = 3) +
      annotate("text",
               label = paste0(other_text, "%"),
               x     = vdist_clamp(data$pc + sd, data$l),
               y     = max(dnorm(data$x, mean, sd)) + 0.07,
               color = "#6495ED",
               size  = 3)

  } else if (method == "upper") {
    gplot <-
      gplot +
      ggtitle(label    = "Normal Distribution",
              subtitle = paste0("P(X > ", data$cut_text, ") = ", data$prob_text, "%")) +
      annotate("text",
               label = paste0(other_text, "%"),
               x     = vdist_clamp(data$pc - sd, data$l),
               y     = max(dnorm(data$x, mean, sd)) + 0.07,
               color = "#6495ED",
               size  = 3) +
      annotate("text",
               label = paste0(data$prob_text, "%"),
               x     = vdist_clamp(data$pc + sd, data$l),
               y     = max(dnorm(data$x, mean, sd)) + 0.07,
               color = "#0000CD",
               size  = 3)
  } else {
    gplot <-
      gplot +
      ggtitle(label    = "Normal Distribution",
              subtitle = paste0("P(", data$cut_text[1], " < X < ", data$cut_text[2], ") = ", data$prob_text, "%")) +
      annotate("text",
               label = paste0(data$prob_text, "%"),
               x     = vdist_clamp(mean(data$pc), data$l),
               y     = max(dnorm(data$x, mean, sd)) + 0.07,
               color = "#0000CD",
               size  = 3) +
      annotate("text",
               label = paste0(vdist_pct_text(data$pp1), "%"),
               x     = vdist_clamp(data$pc[1] - sd, data$l),
               y     = max(dnorm(data$x, mean, sd)) + 0.07,
               color = "#6495ED",
               size  = 3) +
      annotate("text",
               label = paste0(vdist_pct_text(data$pp2), "%"),
               x     = vdist_clamp(data$pc[2] + sd, data$l),
               y     = max(dnorm(data$x, mean, sd)) + 0.07,
               color = "#6495ED",
               size  = 3)
  }

  dark <- 0
  for (i in seq_len(length(data$l1))) {
    poly_data <- vdist_pol_cord(data$lc[data$l1[i]], data$lc[data$l2[i]], mean, sd)
    if (identical(data$col[i], "#0000CD")) {
      dark <- dark + vdist_area(poly_data$x, poly_data$y)
    }
    gplot <-
      gplot +
      geom_polygon(data    = poly_data,
                   mapping = aes(x = x, y = y),
                   fill    = data$col[i])
  }
  vdist_check_event(dark, data$omitted, data$event_prob, "normal shaded region")

  pln <- length(data$pc)

  for (i in seq_len(pln)) {

    point_data <- data.frame(x = data$pc[i], y = 0)

    gplot <-
      gplot +
      geom_vline(xintercept = data$pc[i],
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
    scale_x_continuous(breaks = if (length(data$l) <= 60) data$l else waiver())

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

  other_text <- vdist_pct_text(1 - data$event_prob)

  if (method == "lower") {
    gplot <-
      gplot +
      ggtitle(label    = "t Distribution",
              subtitle = paste0("P(X < ", data$cut_text, ") = ", data$prob_text, "%")) +
      annotate("text",
               label = paste0(data$prob_text, "%"),
               x     = vdist_clamp(data$pc - 0.3, data$l),
               y     = max(dt(data$l, df)) + 0.025,
               color = "#0000CD",
               size  = 3) +
      annotate("text",
               label = paste0(other_text, "%"),
               x     = vdist_clamp(data$pc + 0.3, data$l),
               y     = max(dt(data$l, df)) + 0.025,
               color = "#6495ED",
               size  = 3)

  } else if (method == "upper") {
    gplot <-
      gplot +
      ggtitle(label    = "t Distribution",
              subtitle = paste0("P(X > ", data$cut_text, ") = ", data$prob_text, "%")) +
      annotate("text",
               label = paste0(other_text, "%"),
               x     = vdist_clamp(data$pc - 0.3, data$l),
               y     = max(dt(data$l, df)) + 0.025,
               color = "#6495ED",
               size  = 3) +
      annotate("text",
               label = paste0(data$prob_text, "%"),
               x     = vdist_clamp(data$pc + 0.3, data$l),
               y     = max(dt(data$l, df)) + 0.025,
               color = "#0000CD",
               size  = 3)
  } else {
    tail_text <- vdist_pct_text(data$alpha)
    gplot <-
      gplot +
      ggtitle(label    = "t Distribution",
              subtitle = paste0("P(", data$cut_text[1], " < X < ", data$cut_text[2], ") = ", data$prob_text, "%")) +
      annotate("text",
               label = paste0(data$prob_text, "%"),
               x     = vdist_clamp(mean(data$pc), data$l),
               y     = max(dt(data$l, df)) + 0.025,
               color = "#0000CD",
               size  = 3) +
      annotate("text",
               label = paste0(tail_text, "%"),
               x     = vdist_clamp(data$pc[1] - 0.3, data$l),
               y     = max(dt(data$l, df)) + 0.025,
               color = "#6495ED",
               size  = 3) +
      annotate("text",
               label = paste0(tail_text, "%"),
               x     = vdist_clamp(data$pc[2] + 0.3, data$l),
               y     = max(dt(data$l, df)) + 0.025,
               color = "#6495ED",
               size  = 3)
  }

  dark <- 0
  for (i in seq_len(length(data$l1))) {
    poly_data <- vdist_pol_t(data$lc[data$l1[i]], data$lc[data$l2[i]], df)
    if (identical(data$col[i], "#0000CD")) {
      dark <- dark + vdist_area(poly_data$x, poly_data$y)
    }
    gplot <-
      gplot +
      geom_polygon(data    = poly_data,
                   mapping = aes(x = x, y = y),
                   fill    = data$col[i])
  }
  vdist_check_event(dark, data$omitted, data$event_prob, "t shaded region")

  pln <- length(data$pc)

  for (i in seq_len(pln)) {

    point_data <- data.frame(x = data$pc[i], y = 0)

    gplot <-
      gplot +
      geom_vline(xintercept = data$pc[i],
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
    scale_x_continuous(breaks = if (max(data$l) <= 30) min(data$l):max(data$l) else waiver()) +
    scale_y_continuous(breaks = NULL)

  dark <- 0
  for (i in seq_len(length(data$l1))) {
    poly_data <- vdist_pol_t(data$lc[data$l1[i]], data$lc[data$l2[i]], df)
    if (identical(data$col[i], "#0000CD")) {
      dark <- dark + vdist_area(poly_data$x, poly_data$y)
    }
    gplot <-
      gplot +
      geom_polygon(data    = poly_data,
                   mapping = aes(x = x, y = y),
                   fill    = data$col[i])
  }
  vdist_check_event(dark, data$omitted, data$event_prob, "t shaded region")

  other_text <- vdist_pct_text(1 - data$event_prob)

  if (method == "lower") {

    point_data <- data.frame(x = data$pc, y = 0)

    gplot <-
      gplot +
      ggtitle(label    = "t Distribution",
              subtitle = paste0("P(X < ", data$cut_text, ") = ", data$prob_text, "%")) +
      annotate("text",
               label = paste0(data$prob_text, "%"),
               x     = vdist_clamp(data$pc - 1, data$l),
               y     = max(dt(data$l, df)) + 0.07,
               color = "#0000CD",
               size  = 3) +
      annotate("text",
               label = paste0(other_text, "%"),
               x     = vdist_clamp(data$pc + 1, data$l),
               y     = max(dt(data$l, df)) + 0.07,
               color = "#6495ED",
               size  = 3) +
      geom_vline(xintercept = data$pc,
                 linetype   = 2,
                 linewidth  = 1) +
      geom_point(data    = point_data,
                 mapping = aes(x = x, y = y),
                 shape   = 4,
                 color   = 'red',
                 size    = 3)

  } else if (method == "upper") {

    point_data <- data.frame(x = data$pc, y = 0)

    gplot <-
      gplot +
      ggtitle(label    = "t Distribution",
              subtitle = paste0("P(X > ", data$cut_text, ") = ", data$prob_text, "%")) +
      annotate("text",
               label = paste0(other_text, "%"),
               x     = vdist_clamp(data$pc - 1, data$l),
               y     = max(dt(data$l, df)) + 0.07,
               color = "#6495ED",
               size  = 3) +
      annotate("text",
               label = paste0(data$prob_text, "%"),
               x     = vdist_clamp(data$pc + 1, data$l),
               y     = max(dt(data$l, df)) + 0.07,
               color = "#0000CD",
               size  = 3) +
      geom_vline(xintercept = data$pc,
                 linetype   = 2,
                 linewidth  = 1) +
      geom_point(data    = point_data,
                 mapping = aes(x = x, y = y),
                 shape   = 4,
                 color   = 'red',
                 size    = 3)

  } else if (method == "interval") {

    point_data <- data.frame(x1 = data$pc, x2 = -data$pc, y = 0)

    gplot <-
      gplot +
      ggtitle(label    = "t Distribution",
              subtitle = paste0("P(", -as.numeric(data$cut_text), " < X < ", data$cut_text, ") = ", data$prob_text, "%")) +
      annotate("text",
               label = paste0(data$prob_text, "%"),
               x     = 0,
               y     = max(dt(data$l, df)) + 0.07,
               color = "#0000CD",
               size  = 3) +
      annotate("text",
               label = paste0(vdist_pct_text(data$pp1), "%"),
               x     = vdist_clamp(-data$pc - 1, data$l),
               y     = max(dt(data$l, df)) + 0.07,
               color = "#6495ED",
               size  = 3) +
      annotate("text",
               label = paste0(vdist_pct_text(data$pp2), "%"),
               x     = vdist_clamp(data$pc + 1, data$l),
               y     = max(dt(data$l, df)) + 0.07,
               color = "#6495ED",
               size  = 3) +
      geom_vline(xintercept = data$pc,
                 linetype   = 2,
                 linewidth  = 1) +
      geom_vline(xintercept = -data$pc,
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

    point_data <- data.frame(x1 = data$pc, x2 = -data$pc, y = 0)

    gplot <-
      gplot +
      ggtitle(label    = "t Distribution",
              subtitle = paste0("P(|X| > ", data$cut_text, ") = ", data$prob_text, "%")) +
      annotate("text",
               label = paste0(other_text, "%"),
               x     = 0,
               y     = max(dt(data$l, df)) + 0.07,
               color = "#6495ED",
               size  = 3) +
      annotate("text",
               label = paste0(vdist_pct_text(data$pp1), "%"),
               x     = vdist_clamp(-data$pc - 1, data$l),
               y     = max(dt(data$l, df)) + 0.07,
               color = "#0000CD",
               size  = 3) +
      annotate("text",
               label = paste0(vdist_pct_text(data$pp2), "%"),
               x     = vdist_clamp(data$pc + 1, data$l),
               y     = max(dt(data$l, df)) + 0.07,
               color = "#0000CD",
               size  = 3) +
      geom_vline(xintercept = data$pc,
                 linetype   = 2,
                 linewidth  = 1) +
      geom_vline(xintercept = -data$pc,
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
  pfun <- function(x) pt(x, df)
  qfun <- function(p) qt(p, df)
  g <- vdist_grid(l1, l2, pfun, qfun, quantile = TRUE,
                  n = vdist_grid_n_t(df))
  x <- vdist_pol_x(g, l1, l2)
  y <- c(0, dt(g, df), 0)
  vdist_check_area(x, y, pfun(l2) - pfun(l1), "t shaded region")
  data.frame(x = x, y = y)
}
