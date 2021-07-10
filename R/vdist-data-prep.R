#' Binomial plot data
#'
#' Data preparation for Binomial plot.
#'
#' @param n Number of trials.
#' @param p Aggregate probability.
#'
#' @noRd
#'
bplot_data_prep <- function(n, p) {

  n    <- as.integer(n)
  x    <- seq(0, n, 1)
  bm   <- round(n * p, 2)
  bsd  <- round(sqrt((1 - p) * bm) , 2)
  data <- stats::dbinom(x, n, p)
  plot_data <- data.frame(n = seq(0, n), df = data)

  list(plot_data = plot_data, bm = bm, bsd = bsd)

}

bprob_data_prep <- function(n, p, s, method) {

  n   <- as.integer(n)
  s   <- as.integer(s)
  x   <- seq(0, n, 1)
  bm  <- round(n * p, 2)
  bsd <- round(sqrt((1 - p) * bm), 2)

  if (method == "lower") {
    k    <- round(pbinom(s, n, p), 3)
    cols <- ifelse(cumsum(round(dbinom(x, n, p), 3)) <= k, "#0000CD", "#6495ED")
  } else if (method == "upper") {
    k    <- round(1 - pbinom((s - 1), n, p), 3)
    cols <- ifelse(cumsum(round(dbinom(x, n, p), 3)) >= k, "#0000CD", "#6495ED")
  } else if (method == "exact") {
    k    <- pbinom(s, n, p) - pbinom((s - 1), n, p)
    cols <- ifelse(round(dbinom(x, n, p), 5) == round(k, 5), "#0000CD", "#6495ED")
  } else {
    k1   <- pbinom((s[1] - 1), n, p)
    k2   <- pbinom(s[2], n, p)
    k    <- pbinom(s[2], n, p) - pbinom((s[1] - 1), n, p)
    cols <- ifelse((round(cumsum(dbinom(x, n, p)), 6) > round(k1, 6) &
                      round(cumsum(dbinom(x, n, p)), 6) <= round(k2, 6)), "#0000CD", "#6495ED")
  }

  data      <- dbinom(x, n, p)
  plot_data <- data.frame(n = seq(0, n), df = data)

  list(plot_data = plot_data, bm = bm, bsd = bsd, k = k, cols = cols)

}


bperc_data_prep <- function(n, p, tp, method) {

  n      <- as.integer(n)
  x      <- seq(0, n, 1)

  if (method == "lower") {
    k    <- round(qbinom(tp, n, p), 3)
    cols <- ifelse(cumsum(dbinom(x, n, p)) <= pbinom(k, n, p), "#0000CD", "#6495ED")
  } else {
    k    <- round(qbinom(tp, n, p, lower.tail = F), 3)
    cols <- ifelse(cumsum(dbinom(x, n, p)) > pbinom((k + 1), n, p), "#0000CD", "#6495ED")
  }

  data      <- dbinom(x, n, p)
  plot_data <- data.frame(n = seq(0, n), df = data)

  list(plot_data = plot_data, k = k, cols = cols)

}

cplot_data_prep <- function(df, range) {

  df    <- as.integer(df)
  chim  <- round(df, 3)
  chisd <- round(sqrt(2 * df), 3)
  x     <- seq(0, range, 0.01)
  data  <- dchisq(x, df)

  plot_data  <- data.frame(x = x, chi = data)
  poly_data  <- data.frame(y = c(0, seq(0, 25, 0.01), 25),
                           z = c(0, dchisq(seq(0, 25, 0.01), df), 0))
  point_data <- data.frame(x = chim, y = min(data))
  nline_data <- data.frame(x = x, y = dnorm(x, chim, chisd))

  list(plot_data  = plot_data,
       poly_data  = poly_data,
       point_data = point_data,
       nline_data = nline_data,
       chim       = chim,
       chisd      = chisd)

}

