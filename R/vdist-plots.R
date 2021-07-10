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
