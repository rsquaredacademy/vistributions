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
    cols <- ifelse(x <= s, "#0000CD", "#6495ED")
  } else if (method == "upper") {
    k    <- round(1 - pbinom((s - 1), n, p), 3)
    cols <- ifelse(x >= s, "#0000CD", "#6495ED")
  } else if (method == "exact") {
    k    <- round(dbinom(s, n, p), 5)
    cols <- ifelse(x == s, "#0000CD", "#6495ED")
  } else {
    k    <- round(pbinom(s[2], n, p) - pbinom((s[1] - 1), n, p), 5)
    cols <- ifelse(x >= s[1] & x <= s[2], "#0000CD", "#6495ED")
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
    cols <- ifelse(x <= k, "#0000CD", "#6495ED")
  } else {
    k    <- round(qbinom(tp, n, p, lower.tail = F), 3)
    cols <- ifelse(x >= k, "#0000CD", "#6495ED")
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
  poly_data  <- data.frame(y = c(0, seq(0, range, 0.01), range),
                           z = c(0, dchisq(seq(0, range, 0.01), df), 0))
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
  fm     <- if (den_df > 2) round(den_df / (den_df - 2), 3) else NA_real_
  fsd    <- if (den_df > 4) round(sqrt((2 * (fm ^ 2) * (num_df + den_df - 2)) / (num_df * (den_df - 4))), 3) else NA_real_
  x      <- seq(0, 4, 0.01)
  nx     <- seq(0, 4, 0.01)

  plot_data  <- data.frame(x = x, y = df(x, num_df, den_df))
  point_data <- if (!is.na(fm)) data.frame(x = fm, y = 0) else data.frame(x = numeric(0), y = numeric(0))
  nline_data <- if (!is.na(fsd)) data.frame(x = nx, y = dnorm(nx, fm, fsd)) else data.frame(x = numeric(0), y = numeric(0))

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
  fm     <- if (den_df > 2) round(den_df / (den_df - 2), 3) else NA_real_
  fsd    <- if (den_df > 4) round(sqrt((2 * (fm ^ 2) * (num_df + den_df - 2)) / (num_df * (den_df - 4))), 3) else NA_real_

  if (method == "lower") {
    pp  <- round(qf(probs, num_df, den_df), 3)
    col <- c("#0000CD", "#6495ED")
    l1  <- c(1, 2)
    l2  <- c(2, 3)
  } else {
    pp  <- round(qf(probs, num_df, den_df, lower.tail = FALSE), 3)
    col <- c("#6495ED", "#0000CD")
    l1  <- c(1, 2)
    l2  <- c(2, 3)
  }

  xu <- if (pp < 4) 4 else pp * 1.25
  l  <- seq(0, xu, 0.01)
  ln <- length(l)
  lc <- c(l[1], pp, l[ln])

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
  fm     <- if (den_df > 2) round(den_df / (den_df - 2), 3) else NA_real_
  fsd    <- if (den_df > 4) round(sqrt((2 * (fm ^ 2) * (num_df + den_df - 2)) / (num_df * (den_df - 4))), 3) else NA_real_

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
    pp  <- round(pf(perc, num_df, den_df, lower.tail = FALSE), 3)
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

nplot_data_prep <- function(mean, sd) {

  x   <- vdist_xax(mean, sd)
  l   <- vdist_seql(mean, sd)
  col <- c("#0000CD", "#4682B4", "#6495ED", "#4682B4", "#6495ED")
  l1  <- c(3, 2, 1, 5, 6)
  l2  <- c(5, 3, 2, 6, 7)
  ll  <- l[3:9]

  plot_data <- data.frame(x = x, y = dnorm(x, mean, sd))

  list(plot_data = plot_data,
       l         = l,
       col       = col,
       l1        = l1,
       l2        = l2,
       ll        = ll)
}

vdist_xax <- function(mean, sd) {
  xl <- mean - (4 * sd)
  xu <- mean + (4 * sd)
  seq(xl, xu, length.out = 1000)
}

vdist_seql <- function(mean, sd) {
  lmin <- mean - (5 * sd)
  lmax <- mean + (5 * sd)
  seq(lmin, lmax, sd)
}

nperc_data_prep <- function(probs, mean, sd, method) {

  x      <- vdist_xax(mean, sd)
  l      <- vdist_seql(mean, sd)
  ln     <- length(l)

  if (method == "lower") {
    pp  <- round(qnorm(probs, mean, sd), 3)
    lc  <- c(l[1], pp, l[ln])
    col <- c("#0000CD", "#6495ED")
    l1  <- c(1, 2)
    l2  <- c(2, 3)
  } else if (method == "upper") {
    pp  <- round(qnorm(probs, mean, sd, lower.tail = F), 3)
    lc  <- c(l[1], pp, l[ln])
    col <- c("#6495ED", "#0000CD")
    l1  <- c(1, 2)
    l2  <- c(2, 3)
  } else {
    alpha <- (1 - probs) / 2
    pp1 <- round(qnorm(alpha, mean, sd), 3)
    pp2 <- round(qnorm(alpha, mean, sd, lower.tail = F), 3)
    pp  <- c(pp1, pp2)
    lc  <- c(l[1], pp1, pp2, l[ln])
    col <- c("#6495ED", "#0000CD", "#6495ED")
    l1  <- c(1, 2, 3)
    l2  <- c(2, 3, 4)
  }

  xm <- vdist_xmm(mean, sd)
  plot_data <- data.frame(x = x, y = dnorm(x, mean, sd))

  list(plot_data = plot_data,
       pp        = pp,
       l         = l,
       x         = x,
       alpha     = alpha,
       l1        = l1,
       l2        = l2,
       lc        = lc,
       col       = col)

}

nprob_data_prep <- function(perc, mean, sd, method) {

  el <- max(abs(perc - mean)) / sd + 1
  x  <- vdist_xaxp(mean, sd, el)
  l  <- vdist_seqlp(mean, sd, el)
  ln <- length(l)

  if (method == "lower") {
    pp1 <- NULL
    pp2 <- NULL
    pp  <- round(pnorm(perc, mean, sd), 3)
    lc  <- c(l[1], perc, l[ln])
    col <- c("#0000CD", "#6495ED")
    l1  <- c(1, 2)
    l2  <- c(2, 3)
  } else if (method == "upper") {
    pp1 <- NULL
    pp2 <- NULL
    pp  <- round(pnorm(perc, mean, sd, lower.tail = F), 3)
    lc  <- c(l[1], perc, l[ln])
    col <- c("#6495ED", "#0000CD")
    l1  <- c(1, 2)
    l2  <- c(2, 3)
  } else {
    pp1 <- round(pnorm(perc[1], mean, sd), 3)
    pp2 <- round(pnorm(perc[2], mean, sd, lower.tail = F), 3)
    pp  <- c(pp1, pp2)
    lc  <- c(l[1], perc[1], perc[2], l[ln])
    col <- c("#6495ED", "#0000CD", "#6495ED")
    l1  <- c(1, 2, 3)
    l2  <- c(2, 3, 4)
  }

  xm <- vdist_xmmp(mean, sd, el)
  plot_data <- data.frame(x = x, y = dnorm(x, mean, sd))

  list(plot_data = plot_data,
       pp        = pp,
       x         = x,
       l         = l,
       pp1       = pp1,
       pp2       = pp2,
       lc        = lc,
       l1        = l1,
       l2        = l2,
       col       = col)
}

vdist_xaxp <- function(mean, sd, el) {
  span <- if (el > 4) el * sd else 4 * sd
  xl <- mean - span
  xu <- mean + span
  seq(xl, xu, length.out = 1000)
}

vdist_seqlp <- function(mean, sd, el) {
  if (el > 4) {
    lmin <- mean - (el * sd)
    lmax <- mean + (el * sd)
  } else {
    lmin <- mean - (4 * sd)
    lmax <- mean + (4 * sd)
  }

  seq(lmin, lmax, sd)
}

vdist_xmmp <- function(mean, sd, el) {
  if (el > 4) {
    xmin <- mean - (el * sd)
    xmax <- mean + (el * sd)
  } else {
    xmin <- mean - (4 * sd)
    xmax <- mean + (4 * sd)
  }

  c(xmin, xmax)
}

tplot_data_prep <- function(df) {

  df <- as.integer(df)
  x  <- seq(-4, 4, 0.01)

  plot_data <- data.frame(x = x, y = dt(x, df))
  poly_data <- data.frame(y = c(-4, seq(-4, 4, 0.01), 4),
                          z = c(0, dt(seq(-4, 4, 0.01), df), 0))

  list(plot_data = plot_data,
       poly_data = poly_data)
}

tperc_data_prep <- function(probs, df, method) {

  df      <- as.integer(df)
  l       <- seq(-5, 5, 0.01)
  ln      <- length(l)

  if (method == "lower") {
    pp1   <- NULL
    pp2   <- NULL
    pp    <- round(qt(probs, df), 3)
    lc    <- c(l[1], pp, l[ln])
    col   <- c("#0000CD", "#6495ED")
    l1    <- c(1, 2)
    l2    <- c(2, 3)
  } else if (method == "upper") {
    pp1   <- NULL
    pp2   <- NULL
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

  list(plot_data = plot_data,
       l         = l,
       alpha     = alpha,
       pp        = pp,
       lc        = lc,
       l1        = l1,
       l2        = l2,
       col       = col)

}

tprob_data_prep <- function(perc, df, method) {

  df <- as.integer(df)

  bound <- max(5, abs(perc) + 1)
  l     <- seq(-bound, bound, 0.01)

  ln <- length(l)

  if (method == "lower") {
    pp1 <- NULL
    pp2 <- NULL
    pp  <- round(pt(perc, df), 3)
    lc  <- c(l[1], perc, l[ln])
    col <- c("#0000CD", "#6495ED")
    l1  <- c(1, 2)
    l2  <- c(2, 3)
  } else if (method == "upper") {
    pp1 <- NULL
    pp2 <- NULL
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

  list(plot_data = plot_data,
       l         = l,
       l1        = l1,
       l2        = l2,
       lc        = lc,
       col       = col,
       pp        =  pp,
       pp1       = pp1,
       pp2       = pp2)

}
