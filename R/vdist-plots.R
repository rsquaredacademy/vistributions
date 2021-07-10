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

bprob_plot_build <- function(data, n) {

  p <-
    ggplot(data$plot_data) +
    geom_col(aes(x = n, y = df),
             fill = data$cols) +
    ylab("Probability") +
    xlab(paste("No. of success\n", "Mean =", data$bm, ", Std. Dev. =", data$bsd)) +
    scale_x_continuous(breaks = seq(0, n)) +
    theme(plot.title    = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5))

  return(p)
}


bprob_plot_modify <- function(plot, method, n, p, s, data) {

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

bperc_plot_build <- function(data, n) {

  pp <-
    ggplot(data$plot_data) +
    geom_col(aes(x = n, y = df),
             fill = data$cols) +
    ylab("Probability") +
    xlab("No. of success") +
    scale_x_continuous(breaks = seq(0, n)) +
    theme(plot.title    = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5))

  return(pp)
}

bperc_plot_modify <- function(plot, method, n, p, tp, data) {

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

cplot_plot_build <- function(data, df, range) {

  pp <-
    ggplot(data$plot_data) +
    geom_line(aes(x, chi),
              color = '#4682B4',
              size  = 2) +
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

}

cplot_plot_modify <- function(plot, data) {

  plot <-
    plot +
    geom_line(data    = data$nline_data,
              mapping = aes(x = x, y = y),
              color   = '#FF4500')

  return(plot)

}
