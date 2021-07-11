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

cperc_data_prep <- function(probs, df, method) {

  df     <- as.integer(df)
  chim   <- round(df, 3)
  chisd  <- round(sqrt(2 * df), 3)
  l      <- vdist_chiseql(chim, chisd)
  ln     <- length(l)

  if (method == "lower") {
    pp  <- round(qchisq(probs, df), 3)
    lc  <- c(l[1], pp, l[ln])
    col <- c("#0000CD", "#6495ED")
    l1  <- c(1, 2)
    l2  <- c(2, 3)
  } else {
    pp  <- round(qchisq(probs, df, lower.tail = F), 3)
    lc  <- c(l[1], pp, l[ln])
    col <- c("#6495ED", "#0000CD")
    l1  <- c(1, 2)
    l2  <- c(2, 3)
  }

  xm         <- vdist_xmm(chim, chisd)
  plot_data  <- data.frame(x = l, y = dchisq(l, df))
  point_data <- data.frame(x = pp, y = min(dchisq(l, df)))

  list(plot_data  = plot_data,
       point_data = point_data,
       chim       = chim,
       chisd      = chisd,
       xm         = xm,
       l          = l,
       pp         = pp,
       lc         = lc,
       l1         = l1,
       l2         = l2,
       col        = col)
}

vdist_chiseql <- function(mean, sd) {
  lmin <- mean - (5 * sd)
  lmax <- mean + (5 * sd)
  seq(lmin, lmax, 0.01)
}

vdist_xmm <- function(mean, sd) {
  xmin <- mean - (5 * sd)
  xmax <- mean + (5 * sd)
  c(xmin, xmax)
}

cprob_data_prep <- function(perc, df, method) {

  chim   <- round(df, 3)
  chisd  <- round(sqrt(2 * df), 3)

  l <- if (perc < 25) {
    seq(0, 25, 0.01)
  } else {
    seq(0, (perc + (3 * chisd)), 0.01)
  }

  ln <- length(l)

  if (method == "lower") {
    pp  <- round(pchisq(perc, df), 3)
    lc  <- c(l[1], perc, l[ln])
    col <- c("#0000CD", "#6495ED")
    l1  <- c(1, 2)
    l2  <- c(2, 3)
  } else {
    pp  <- round(pchisq(perc, df, lower.tail = F), 3)
    lc  <- c(l[1], perc, l[ln])
    col <- c("#6495ED", "#0000CD")
    l1  <- c(1, 2)
    l2  <- c(2, 3)
  }

  plot_data  <- data.frame(x = l, y = dchisq(l, df))
  point_data <- data.frame(x = perc, y = min(dchisq(l, df)))

  list(plot_data  = plot_data,
       point_data = point_data,
       chim       = chim,
       chisd      = chisd,
       l          = l,
       ln         = ln,
       pp         = pp,
       lc         = lc,
       l1         = l1,
       l2         = l2,
       col        = col)

}

fplot_data_prep <- function(num_df, den_df) {

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

  list(plot_data  = plot_data,
       poly_data  = poly_data,
       point_data = point_data,
       nline_data = nline_data,
       fm         = fm,
       fsd        = fsd)

}

fperc_data_prep <- function(probs, num_df, den_df, method) {

  num_df <- as.integer(num_df)
  den_df <- as.integer(den_df)
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

  list(plot_data = plot_data,
       fm        = fm,
       fsd       = fsd,
       l         = l,
       pp        = pp,
       lc        = lc,
       l1        = l1,
       l2        = l2,
       col       = col)

}

fprob_data_prep <- function(perc, num_df, den_df, method) {

  num_df <- as.integer(num_df)
  den_df <- as.integer(den_df)
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

  list(plot_data = plot_data,
       fm        = fm,
       fsd       = fsd,
       l         = l,
       pp        = pp,
       lc        = lc,
       l1        = l1,
       l2        = l2,
       col       = col)
}
