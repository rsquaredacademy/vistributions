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

  n    <- round(n)
  x    <- seq(0, n, 1)
  bm   <- round(n * p, 2)
  bsd  <- round(sqrt(n * p * (1 - p)), 2)
  data <- stats::dbinom(x, n, p)
  plot_data <- data.frame(n = seq(0, n), df = data)

  list(plot_data = plot_data, bm = bm, bsd = bsd)

}

bprob_data_prep <- function(n, p, s, method) {

  n   <- round(n)
  x   <- seq(0, n, 1)
  bm  <- round(n * p, 2)
  bsd <- round(sqrt(n * p * (1 - p)), 2)

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

  n      <- round(n)
  x      <- seq(0, n, 1)

  if (method == "lower") {
    # smallest k with P(X <= k) >= tp; shade x <= k
    k    <- qbinom(tp, n, p)
    cols <- ifelse(x <= k, "#0000CD", "#6495ED")
  } else {
    # smallest k with P(X > k) <= tp; shade the tail x > k (x >= k + 1)
    k    <- qbinom(tp, n, p, lower.tail = FALSE)
    cols <- ifelse(x > k, "#0000CD", "#6495ED")
  }

  data      <- dbinom(x, n, p)
  plot_data <- data.frame(n = seq(0, n), df = data)

  list(plot_data = plot_data, k = k, cols = cols)

}

cplot_data_prep <- function(df, range) {

  chim  <- round(df, 3)
  chisd <- round(sqrt(2 * df), 3)
  lo    <- vdist_support_start(df, qchisq(1e-6, df))
  x     <- vdist_grid(lo, range,
                      function(v) pchisq(v, df),
                      function(p) qchisq(p, df),
                      quantile = df < 2)
  data  <- dchisq(x, df)

  plot_data  <- data.frame(x = x, chi = data)
  poly_data  <- data.frame(y = c(0, x, range),
                           z = c(0, data, 0))
  point_data <- data.frame(x = chim, y = min(data))
  # the overlay uses the df that was supplied: rounding df (or its sd) to three
  # decimals turns a valid positive sd into a point mass for tiny df
  nline_data <- data.frame(x = x, y = dnorm(x, df, sqrt(2 * df)))

  list(plot_data  = plot_data,
       poly_data  = poly_data,
       point_data = point_data,
       nline_data = nline_data,
       chim       = chim,
       chisd      = chisd)

}

cperc_data_prep <- function(probs, df, method) {

  chim   <- round(df, 3)
  chisd  <- round(sqrt(2 * df), 3)

  if (method == "lower") {
    pp  <- qchisq(probs, df)
    col <- c("#0000CD", "#6495ED")
  } else {
    pp  <- qchisq(probs, df, lower.tail = FALSE)
    col <- c("#6495ED", "#0000CD")
  }

  # the grid must cover the cutoff, however far out it is
  l      <- vdist_chiseql(chim, chisd, pp, df)
  ln     <- length(l)
  # the drawn cutoff is clipped to the visible range; the caption keeps pp
  pc     <- min(max(pp, l[1]), l[ln])
  lc     <- c(l[1], pc, l[ln])
  l1     <- c(1, 2)
  l2     <- c(2, 3)

  xm         <- c(l[1], l[ln])
  plot_data  <- data.frame(x = l, y = dchisq(l, df))
  point_data <- data.frame(x = pc, y = min(dchisq(l, df)))

  list(plot_data  = plot_data,
       point_data = point_data,
       chim       = chim,
       chisd      = chisd,
       xm         = xm,
       l          = l,
       pp         = pp,
       pc         = pc,
       lc         = lc,
       l1         = l1,
       l2         = l2,
       col        = col)
}

vdist_chiseql <- function(mean, sd, cutoff = NULL, df) {
  lmin <- vdist_support_start(df, qchisq(1e-6, df))
  lmax <- mean + (5 * sd)
  if (!is.null(cutoff)) {
    lmax <- max(lmax, max(cutoff) + sd)
  }
  # never plot beyond the central mass: a cutoff further out is drawn clipped
  lmax <- max(min(lmax, qchisq(1 - 1e-9, df)), lmin)
  vdist_grid(lmin, lmax,
             function(x) pchisq(x, df),
             function(p) qchisq(p, df),
             quantile = df < 2)
}

vdist_xmm <- function(mean, sd) {
  xmin <- mean - (5 * sd)
  xmax <- mean + (5 * sd)
  c(xmin, xmax)
}

cprob_data_prep <- function(perc, df, method) {

  chim   <- round(df, 3)
  chisd  <- round(sqrt(2 * df), 3)

  lo <- vdist_support_start(df, qchisq(1e-6, df))
  hi <- if (perc < 25) 25 else (perc + (3 * chisd))
  # never plot beyond the central mass: a cutoff further out is drawn clipped
  hi <- max(min(hi, qchisq(1 - 1e-9, df)), lo + 1)

  l  <- vdist_grid(lo, hi,
                   function(x) pchisq(x, df),
                   function(p) qchisq(p, df),
                   quantile = df < 2)
  ln <- length(l)
  # the support starts at 0 and the view stops at the central mass: clamp the
  # cutoff used for the shaded regions (the caption keeps the true perc)
  pc <- min(max(perc, l[1]), l[ln])

  if (method == "lower") {
    pp  <- round(pchisq(perc, df), 3)
    lc  <- c(l[1], pc, l[ln])
    col <- c("#0000CD", "#6495ED")
    l1  <- c(1, 2)
    l2  <- c(2, 3)
  } else {
    pp  <- round(pchisq(perc, df, lower.tail = F), 3)
    lc  <- c(l[1], pc, l[ln])
    col <- c("#6495ED", "#0000CD")
    l1  <- c(1, 2)
    l2  <- c(2, 3)
  }

  plot_data  <- data.frame(x = l, y = dchisq(l, df))
  point_data <- data.frame(x = pc, y = min(dchisq(l, df)))

  list(plot_data  = plot_data,
       point_data = point_data,
       chim       = chim,
       chisd      = chisd,
       l          = l,
       ln         = ln,
       pp         = pp,
       pc         = pc,
       lc         = lc,
       l1         = l1,
       l2         = l2,
       col        = col)

}

fplot_data_prep <- function(num_df, den_df) {

  fm     <- if (den_df > 2) round(den_df / (den_df - 2), 3) else NA_real_
  fsd    <- if (den_df > 4) round(sqrt((2 * (fm ^ 2) * (num_df + den_df - 2)) / (num_df * (den_df - 4))), 3) else NA_real_
  lo     <- vdist_support_start(num_df, qf(1e-6, num_df, den_df))
  x      <- vdist_grid(lo, 4,
                       function(v) pf(v, num_df, den_df),
                       function(p) qf(p, num_df, den_df),
                       quantile = num_df < 2)
  nx     <- x

  plot_data  <- data.frame(x = x, y = df(x, num_df, den_df))
  point_data <- if (!is.na(fm)) data.frame(x = fm, y = 0) else data.frame(x = numeric(0), y = numeric(0))
  nline_data <- if (!is.na(fsd)) data.frame(x = nx, y = dnorm(nx, fm, fsd)) else data.frame(x = numeric(0), y = numeric(0))

  poly_data  <- data.frame(y = c(0, x, 4),
                           z = c(0,
                                 df(x, num_df, den_df),
                                 0))

  list(plot_data  = plot_data,
       poly_data  = poly_data,
       point_data = point_data,
       nline_data = nline_data,
       fm         = fm,
       fsd        = fsd)

}

fperc_data_prep <- function(probs, num_df, den_df, method) {

  fm     <- if (den_df > 2) round(den_df / (den_df - 2), 3) else NA_real_
  fsd    <- if (den_df > 4) round(sqrt((2 * (fm ^ 2) * (num_df + den_df - 2)) / (num_df * (den_df - 4))), 3) else NA_real_

  if (method == "lower") {
    pp  <- qf(probs, num_df, den_df)
    col <- c("#0000CD", "#6495ED")
    l1  <- c(1, 2)
    l2  <- c(2, 3)
  } else {
    pp  <- qf(probs, num_df, den_df, lower.tail = FALSE)
    col <- c("#6495ED", "#0000CD")
    l1  <- c(1, 2)
    l2  <- c(2, 3)
  }

  xu <- if (pp < 4) 4 else pp * 1.25
  # never plot beyond the central mass: a cutoff further out is drawn clipped
  xu <- max(min(xu, qf(1 - 1e-9, num_df, den_df)), qf(1e-6, num_df, den_df))
  lo <- vdist_support_start(num_df, qf(1e-6, num_df, den_df))
  l  <- vdist_grid(lo, xu,
                   function(x) pf(x, num_df, den_df),
                   function(p) qf(p, num_df, den_df),
                   quantile = num_df < 2)
  ln <- length(l)
  pc <- min(max(pp, l[1]), l[ln])
  lc <- c(l[1], pc, l[ln])

  plot_data <- data.frame(x = l, y = df(l, num_df, den_df))

  list(plot_data = plot_data,
       fm        = fm,
       fsd       = fsd,
       l         = l,
       pp        = pp,
       pc        = pc,
       lc        = lc,
       l1        = l1,
       l2        = l2,
       col       = col)

}

fprob_data_prep <- function(perc, num_df, den_df, method) {

  fm     <- if (den_df > 2) round(den_df / (den_df - 2), 3) else NA_real_
  fsd    <- if (den_df > 4) round(sqrt((2 * (fm ^ 2) * (num_df + den_df - 2)) / (num_df * (den_df - 4))), 3) else NA_real_

  lo <- vdist_support_start(num_df, qf(1e-6, num_df, den_df))
  hi <- if (perc < 4) 4 else (perc * 1.25)
  # never plot beyond the central mass: a cutoff further out is drawn clipped
  hi <- max(min(hi, qf(1 - 1e-9, num_df, den_df)), lo + 1)

  l  <- vdist_grid(lo, hi,
                   function(x) pf(x, num_df, den_df),
                   function(p) qf(p, num_df, den_df),
                   quantile = num_df < 2)
  ln <- length(l)
  # the support starts at 0 and the view stops at the central mass: clamp the
  # cutoff used for the shaded regions (the caption keeps the true perc)
  pc <- min(max(perc, l[1]), l[ln])

  if (method == "lower") {
    pp  <- round(pf(perc, num_df, den_df), 3)
    lc  <- c(l[1], pc, l[ln])
    col <- c("#0000CD", "#6495ED")
    l1  <- c(1, 2)
    l2  <- c(2, 3)
  } else {
    pp  <- round(pf(perc, num_df, den_df, lower.tail = FALSE), 3)
    lc  <- c(l[1], pc, l[ln])
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
       pc        = pc,
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

vdist_xax <- function(mean, sd, k = 4) {
  k  <- max(k, 4)
  xl <- mean - (k * sd)
  xu <- mean + (k * sd)
  seq(xl, xu, length.out = 1000)
}

# axis grid: mean +/- k sd in steps of sd (coarser when k is very large)
vdist_seql <- function(mean, sd, k = 5) {
  lmin <- mean - (k * sd)
  lmax <- mean + (k * sd)
  if (k > 50) {
    seq(lmin, lmax, length.out = 11)
  } else {
    seq(lmin, lmax, sd)
  }
}

# half-width (in sd units) needed to show every cutoff with a margin
vdist_halfwidth <- function(cutoff, mean, sd, min_k) {
  max(min_k, max(abs(cutoff - mean)) / sd + 1)
}

nperc_data_prep <- function(probs, mean, sd, method) {

  # cutoffs keep full precision; only the displayed text is formatted
  if (method == "lower") {
    pp  <- qnorm(probs, mean, sd)
    col <- c("#0000CD", "#6495ED")
    l1  <- c(1, 2)
    l2  <- c(2, 3)
  } else if (method == "upper") {
    pp  <- qnorm(probs, mean, sd, lower.tail = F)
    col <- c("#6495ED", "#0000CD")
    l1  <- c(1, 2)
    l2  <- c(2, 3)
  } else {
    alpha <- (1 - probs) / 2
    pp1 <- qnorm(alpha, mean, sd)
    pp2 <- qnorm(alpha, mean, sd, lower.tail = F)
    pp  <- c(pp1, pp2)
    col <- c("#6495ED", "#0000CD", "#6495ED")
    l1  <- c(1, 2, 3)
    l2  <- c(2, 3, 4)
  }

  # the grid must cover every cutoff, but never beyond the central mass
  k      <- min(vdist_halfwidth(pp, mean, sd, 5), qnorm(1 - 1e-9))
  x      <- vdist_xax(mean, sd, min(vdist_halfwidth(pp, mean, sd, 4), qnorm(1 - 1e-9)))
  l      <- vdist_seql(mean, sd, k)
  ln     <- length(l)
  # the drawn cutoff is clipped to the visible range; the caption keeps pp
  pc     <- pmin(pmax(pp, l[1]), l[ln])
  lc     <- if (method == "both") c(l[1], pc[1], pc[2], l[ln]) else c(l[1], pc, l[ln])

  plot_data <- data.frame(x = x, y = dnorm(x, mean, sd))

  list(plot_data = plot_data,
       pp        = pp,
       pc        = pc,
       l         = l,
       x         = x,
       alpha     = alpha,
       l1        = l1,
       l2        = l2,
       lc        = lc,
       col       = col)

}

nprob_data_prep <- function(perc, mean, sd, method) {

  # the view includes the cutoffs unless they lie beyond the central mass
  el <- min(max(abs(perc - mean)) / sd + 1, qnorm(1 - 1e-9))
  x  <- vdist_xaxp(mean, sd, el)
  l  <- vdist_seqlp(mean, sd, el)
  ln <- length(l)
  # the drawn cutoffs are clipped to the visible range; captions keep perc
  pc <- pmin(pmax(perc, l[1]), l[ln])

  if (method == "lower") {
    pp1 <- NULL
    pp2 <- NULL
    pp  <- round(pnorm(perc, mean, sd), 3)
    lc  <- c(l[1], pc, l[ln])
    col <- c("#0000CD", "#6495ED")
    l1  <- c(1, 2)
    l2  <- c(2, 3)
  } else if (method == "upper") {
    pp1 <- NULL
    pp2 <- NULL
    pp  <- round(pnorm(perc, mean, sd, lower.tail = F), 3)
    lc  <- c(l[1], pc, l[ln])
    col <- c("#6495ED", "#0000CD")
    l1  <- c(1, 2)
    l2  <- c(2, 3)
  } else {
    pp1 <- round(pnorm(perc[1], mean, sd), 3)
    pp2 <- round(pnorm(perc[2], mean, sd, lower.tail = F), 3)
    pp  <- c(pp1, pp2)
    lc  <- c(l[1], pc[1], pc[2], l[ln])
    col <- c("#6495ED", "#0000CD", "#6495ED")
    l1  <- c(1, 2, 3)
    l2  <- c(2, 3, 4)
  }

  xm <- vdist_xmmp(mean, sd, el)
  plot_data <- data.frame(x = x, y = dnorm(x, mean, sd))

  list(plot_data = plot_data,
       pp        = pp,
       pc        = pc,
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
  vdist_seql(mean, sd, if (el > 4) el else 4)
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

  x  <- seq(-4, 4, 0.01)

  plot_data <- data.frame(x = x, y = dt(x, df))
  poly_data <- data.frame(y = c(-4, x, 4),
                          z = c(0, dt(x, df), 0))

  list(plot_data = plot_data,
       poly_data = poly_data)
}

tperc_data_prep <- function(probs, df, method) {

  if (method == "lower") {
    pp1   <- NULL
    pp2   <- NULL
    pp    <- qt(probs, df)
    col   <- c("#0000CD", "#6495ED")
    l1    <- c(1, 2)
    l2    <- c(2, 3)
  } else if (method == "upper") {
    pp1   <- NULL
    pp2   <- NULL
    pp    <- qt(probs, df, lower.tail = F)
    col   <- c("#6495ED", "#0000CD")
    l1    <- c(1, 2)
    l2    <- c(2, 3)
  } else {
    alpha <- (1 - probs) / 2
    pp1   <- qt(alpha, df)
    pp2   <- qt(alpha, df, lower.tail = F)
    pp    <- c(pp1, pp2)
    col   <- c("#6495ED", "#0000CD", "#6495ED")
    l1    <- c(1, 2, 3)
    l2    <- c(2, 3, 4)
  }

  # the grid must cover every cutoff, but never beyond the central mass
  bound <- max(5, max(abs(pp)) + 1)
  bound <- min(bound, qt(1 - 1e-9, df))
  l     <- vdist_grid(-bound, bound,
                      function(x) pt(x, df),
                      function(p) qt(p, df))
  ln    <- length(l)
  # the drawn cutoffs are clipped to the visible range; captions keep pp
  pc    <- pmin(pmax(pp, l[1]), l[ln])
  lc    <- if (method == "both") c(l[1], pc[1], pc[2], l[ln]) else c(l[1], pc, l[ln])

  plot_data <- data.frame(x = l, y = dt(l, df))

  list(plot_data = plot_data,
       l         = l,
       alpha     = alpha,
       pp        = pp,
       pc        = pc,
       lc        = lc,
       l1        = l1,
       l2        = l2,
       col       = col)

}

tprob_data_prep <- function(perc, df, method) {

  bound <- max(5, abs(perc) + 1)
  bound <- min(bound, qt(1 - 1e-9, df))
  l     <- vdist_grid(-bound, bound,
                      function(x) pt(x, df),
                      function(p) qt(p, df))

  ln <- length(l)
  # the drawn cutoff is clipped to the visible range; the caption keeps perc
  pc <- min(max(perc, l[1]), l[ln])

  if (method == "lower") {
    pp1 <- NULL
    pp2 <- NULL
    pp  <- round(pt(perc, df), 3)
    lc  <- c(l[1], pc, l[ln])
    col <- c("#0000CD", "#6495ED")
    l1  <- c(1, 2)
    l2  <- c(2, 3)
  } else if (method == "upper") {
    pp1 <- NULL
    pp2 <- NULL
    pp  <- round(pt(perc, df, lower.tail = F), 3)
    lc  <- c(l[1], pc, l[ln])
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
    lc  <- c(l[1], -pc, pc, l[ln])
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
    lc  <- c(l[1], -pc, pc, l[ln])
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
       pp2       = pp2,
       pc        = pc)

}
