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
  lo    <- vdist_support_start(df, qchisq(1e-6, df), "chi-square", min_df = 0.1)
  if (!is.finite(range) || range <= lo) {
    stop("xaxis_range must be greater than the start of the chi-square support.",
         call. = FALSE)
  }
  x     <- vdist_grid(lo, range,
                      function(v) pchisq(v, df),
                      function(p) qchisq(p, df),
                      quantile = TRUE, n = vdist_grid_n(df < 2))
  data  <- dchisq(x, df)
  vdist_check_density(data, "chi-square density")

  plot_data  <- data.frame(x = x, chi = data)
  poly_data  <- data.frame(y = c(x[1], x, range),
                           z = c(0, data, 0))
  vdist_check_area(poly_data$y, poly_data$z,
                   pchisq(range, df) - pchisq(lo, df),
                   "chi-square plot")
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
    vdist_check_quantile(pp, function(v) pchisq(v, df), probs,
                         "chi-square quantile", "chisquare")
    cut_text <- vdist_fmt_cutoff(pp, Inf, function(v) pchisq(v, df), probs)
    col <- c("#0000CD", "#6495ED")
  } else {
    pp  <- qchisq(probs, df, lower.tail = FALSE)
    vdist_check_quantile(pp, function(v) pchisq(v, df, lower.tail = FALSE),
                         probs, "chi-square quantile", "chisquare")
    cut_text <- vdist_fmt_cutoff(pp, Inf,
                                 function(v) pchisq(v, df, lower.tail = FALSE),
                                 probs)
    col <- c("#6495ED", "#0000CD")
  }

  lmin <- vdist_support_start(df, qchisq(1e-6, df), "chi-square", min_df = 0.1)
  # An upper-tail event needs a view that reaches the tail: stop where only
  # 1e-4 of the total probability remains, so the omitted part of the event is
  # far below the 1e-3 area tolerance.
  tail_hi <- qchisq(1 - 1e-4, df)
  lmax <- max(chim + (5 * chisd), pp + chisd,
              if (method == "upper") tail_hi else -Inf)
  lmax <- max(min(lmax, qchisq(1 - 1e-9, df)), lmin, pp)

  l      <- vdist_grid(lmin, lmax,
                       function(x) pchisq(x, df),
                       function(p) qchisq(p, df),
                       quantile = TRUE, n = vdist_grid_n(df < 2))
  ln     <- length(l)
  pc     <- min(max(pp, l[1]), l[ln])
  lc     <- c(l[1], pc, l[ln])
  l1     <- c(1, 2)
  l2     <- c(2, 3)

  # the part of the named event that lies beyond the visible range
  omitted <- if (method == "lower") {
    min(probs, pchisq(l[1], df))
  } else {
    min(probs, pchisq(l[ln], df, lower.tail = FALSE))
  }

  xm         <- c(l[1], l[ln])
  plot_data  <- data.frame(x = l, y = dchisq(l, df))
  vdist_check_density(plot_data$y, "chi-square density")
  point_data <- data.frame(x = pc, y = min(dchisq(l, df)))

  list(plot_data  = plot_data,
       point_data = point_data,
       chim       = chim,
       chisd      = chisd,
       xm         = xm,
       l          = l,
       pp         = pp,
       cut_text   = cut_text,
       prob_text  = vdist_pct_text(probs),
       event_prob = probs,
       omitted    = omitted,
       pc         = pc,
       lc         = lc,
       l1         = l1,
       l2         = l2,
       col        = col)
}

vdist_chiseql <- function(mean, sd, cutoff = NULL, df) {
  lmin <- vdist_support_start(df, qchisq(1e-6, df), "chi-square", min_df = 0.1)
  lmax <- mean + (5 * sd)
  if (!is.null(cutoff)) {
    lmax <- max(lmax, max(cutoff) + sd)
  }
  lmax <- max(min(lmax, qchisq(1 - 1e-9, df)), lmin)
  vdist_grid(lmin, lmax,
             function(x) pchisq(x, df),
             function(p) qchisq(p, df),
             quantile = TRUE, n = vdist_grid_n(df < 2))
}

vdist_xmm <- function(mean, sd) {
  xmin <- mean - (5 * sd)
  xmax <- mean + (5 * sd)
  c(xmin, xmax)
}

cprob_data_prep <- function(perc, df, method) {

  chim   <- round(df, 3)
  chisd  <- round(sqrt(2 * df), 3)

  lo <- vdist_support_start(df, qchisq(1e-6, df), "chi-square", min_df = 0.1)
  pfun_ev <- if (method == "lower") {
    function(v) pchisq(v, df)
  } else {
    function(v) pchisq(v, df, lower.tail = FALSE)
  }

  # the printed cutoff defines the event, so it must keep the probability that
  # is printed beside it
  disp <- vdist_fmt_pct(pfun_ev(perc))
  cut_text <- vdist_fmt_value(perc, Inf, pfun_ev, tol = disp$tol)
  perc_printed <- as.numeric(cut_text)
  event_prob <- pfun_ev(perc_printed)

  tail_hi <- qchisq(1 - 1e-4, df)
  hi <- if (perc < 25) 25 else (perc + (3 * chisd))
  if (method == "upper") {
    hi <- max(hi, tail_hi)
  }
  hi <- max(min(hi, qchisq(1 - 1e-9, df)), lo + 1,
            if (method == "upper") perc_printed else lo + 1)

  l  <- vdist_grid(lo, hi,
                   function(x) pchisq(x, df),
                   function(p) qchisq(p, df),
                   quantile = TRUE, n = vdist_grid_n(df < 2))
  ln <- length(l)
  pc <- min(max(perc_printed, l[1]), l[ln])

  omitted <- if (method == "lower") {
    min(event_prob, pchisq(l[1], df))
  } else {
    min(event_prob, pchisq(l[ln], df, lower.tail = FALSE))
  }

  if (method == "lower") {
    lc  <- c(l[1], pc, l[ln])
    col <- c("#0000CD", "#6495ED")
  } else {
    lc  <- c(l[1], pc, l[ln])
    col <- c("#6495ED", "#0000CD")
  }
  l1  <- c(1, 2)
  l2  <- c(2, 3)

  plot_data  <- data.frame(x = l, y = dchisq(l, df))
  vdist_check_density(plot_data$y, "chi-square density")
  point_data <- data.frame(x = pc, y = min(dchisq(l, df)))

  list(plot_data  = plot_data,
       point_data = point_data,
       chim       = chim,
       chisd      = chisd,
       l          = l,
       ln         = ln,
       pp         = event_prob,
       cut_text   = cut_text,
       prob_text  = vdist_pct_text(event_prob),
       event_prob = event_prob,
       omitted    = omitted,
       pc         = pc,
       lc         = lc,
       l1         = l1,
       l2         = l2,
       col        = col)

}

fplot_data_prep <- function(num_df, den_df) {

  # moments stay unrounded for the overlay; only the displayed summary is
  # formatted. den_df <= 4 has no finite F variance.
  fm     <- if (den_df > 2) den_df / (den_df - 2) else NA_real_
  fsd    <- if (den_df > 4) sqrt((2 * (fm ^ 2) * (num_df + den_df - 2)) / (num_df * (den_df - 4))) else NA_real_
  lo     <- vdist_support_start(num_df, qf(1e-6, num_df, den_df), "F", min_df = 1)

  x <- vdist_f_grid(lo, 4, num_df, den_df, quantile = TRUE)
  nx <- x

  plot_data  <- data.frame(x = x, y = df(x, num_df, den_df))
  vdist_check_density(plot_data$y, "F density")
  point_data <- if (!is.na(fm)) data.frame(x = fm, y = 0) else data.frame(x = numeric(0), y = numeric(0))
  nline_data <- if (!is.na(fsd)) data.frame(x = nx, y = dnorm(nx, fm, fsd)) else data.frame(x = numeric(0), y = numeric(0))

  poly_data  <- data.frame(y = c(x[1], x, 4),
                           z = c(0,
                                 df(x, num_df, den_df),
                                 0))
  vdist_check_area(poly_data$y, poly_data$z,
                   pf(4, num_df, den_df) - pf(lo, num_df, den_df), "F plot")

  list(plot_data  = plot_data,
       poly_data  = poly_data,
       point_data = point_data,
       nline_data = nline_data,
       fm         = fm,
       fsd        = fsd)

}

fperc_data_prep <- function(probs, num_df, den_df, method) {

  fm     <- if (den_df > 2) den_df / (den_df - 2) else NA_real_
  fsd    <- if (den_df > 4) sqrt((2 * (fm ^ 2) * (num_df + den_df - 2)) / (num_df * (den_df - 4))) else NA_real_

  if (method == "lower") {
    pp  <- qf(probs, num_df, den_df)
    vdist_check_quantile(pp, function(v) pf(v, num_df, den_df), probs,
                         "F quantile", "f")
    cut_text <- vdist_fmt_cutoff(pp, Inf,
                                function(v) pf(v, num_df, den_df), probs)
    col <- c("#0000CD", "#6495ED")
  } else {
    pp  <- qf(probs, num_df, den_df, lower.tail = FALSE)
    vdist_check_quantile(pp, function(v) pf(v, num_df, den_df, lower.tail = FALSE),
                         probs, "F quantile", "f")
    cut_text <- vdist_fmt_cutoff(pp, Inf,
                                function(v) pf(v, num_df, den_df, lower.tail = FALSE),
                                probs)
    col <- c("#6495ED", "#0000CD")
  }
  l1  <- c(1, 2)
  l2  <- c(2, 3)

  lo <- vdist_support_start(num_df, qf(1e-6, num_df, den_df), "F", min_df = 1)
  tail_hi <- qf(1 - 1e-4, num_df, den_df)
  xu <- if (pp < 4) 4 else pp * 1.25
  if (method == "upper") {
    xu <- max(xu, tail_hi)
  }
  xu <- max(min(xu, qf(1 - 1e-9, num_df, den_df)), lo, pp)
  l  <- vdist_f_grid(lo, xu, num_df, den_df, quantile = TRUE)
  ln <- length(l)
  pc <- min(max(pp, l[1]), l[ln])
  lc <- c(l[1], pc, l[ln])

  omitted <- if (method == "lower") {
    min(probs, pf(l[1], num_df, den_df))
  } else {
    min(probs, pf(l[ln], num_df, den_df, lower.tail = FALSE))
  }

  plot_data <- data.frame(x = l, y = df(l, num_df, den_df))
  vdist_check_density(plot_data$y, "F density")

  list(plot_data  = plot_data,
       fm         = fm,
       fsd        = fsd,
       l          = l,
       pp         = pp,
       cut_text   = cut_text,
       prob_text  = vdist_pct_text(probs),
       event_prob = probs,
       omitted    = omitted,
       pc         = pc,
       lc         = lc,
       l1         = l1,
       l2         = l2,
       col        = col)

}

fprob_data_prep <- function(perc, num_df, den_df, method) {

  fm     <- if (den_df > 2) den_df / (den_df - 2) else NA_real_
  fsd    <- if (den_df > 4) sqrt((2 * (fm ^ 2) * (num_df + den_df - 2)) / (num_df * (den_df - 4))) else NA_real_

  lo <- vdist_support_start(num_df, qf(1e-6, num_df, den_df), "F", min_df = 1)
  pfun_ev <- if (method == "lower") {
    function(v) pf(v, num_df, den_df)
  } else {
    function(v) pf(v, num_df, den_df, lower.tail = FALSE)
  }

  disp <- vdist_fmt_pct(pfun_ev(perc))
  cut_text <- vdist_fmt_value(perc, Inf, pfun_ev, tol = disp$tol)
  perc_printed <- as.numeric(cut_text)
  event_prob <- pfun_ev(perc_printed)

  tail_hi <- qf(1 - 1e-4, num_df, den_df)
  hi <- if (perc < 4) 4 else (perc * 1.25)
  if (method == "upper") {
    hi <- max(hi, tail_hi)
  }
  hi <- max(min(hi, qf(1 - 1e-9, num_df, den_df)), lo + 1,
            if (method == "upper") perc_printed else lo + 1)

  l  <- vdist_f_grid(lo, hi, num_df, den_df, quantile = TRUE)
  ln <- length(l)
  pc <- min(max(perc_printed, l[1]), l[ln])

  omitted <- if (method == "lower") {
    min(event_prob, pf(l[1], num_df, den_df))
  } else {
    min(event_prob, pf(l[ln], num_df, den_df, lower.tail = FALSE))
  }

  if (method == "lower") {
    lc  <- c(l[1], pc, l[ln])
    col <- c("#0000CD", "#6495ED")
  } else {
    lc  <- c(l[1], pc, l[ln])
    col <- c("#6495ED", "#0000CD")
  }
  l1  <- c(1, 2)
  l2  <- c(2, 3)

  plot_data <- data.frame(x = l, y = df(l, num_df, den_df))
  vdist_check_density(plot_data$y, "F density")

  list(plot_data  = plot_data,
       fm         = fm,
       fsd        = fsd,
       l          = l,
       pp         = event_prob,
       cut_text   = cut_text,
       prob_text  = vdist_pct_text(event_prob),
       event_prob = event_prob,
       omitted    = omitted,
       pc         = pc,
       lc         = lc,
       l1         = l1,
       l2         = l2,
       col        = col)
}

nplot_data_prep <- function(mean, sd) {

  x   <- vdist_xax(mean, sd)
  l   <- vdist_seql(mean, sd)
  col <- c("#0000CD", "#4682B4", "#6495ED", "#4682B4", "#6495ED")
  l1  <- c(3, 2, 1, 5, 6)
  l2  <- c(5, 3, 2, 6, 7)
  ll  <- l[3:9]

  plot_data <- data.frame(x = x, y = dnorm(x, mean, sd))
  vdist_check_density(plot_data$y, "normal density")

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
    out <- seq(lmin, lmax, sd)
    # seq() stops before the upper limit when the span is not a whole number
    # of sd units; the drawn view (and the cutoff clamp) must reach it
    if (length(out) > 1 && out[length(out)] < lmax) {
      out <- c(out, lmax)
    }
    out
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
    vdist_check_quantile(pp, function(v) pnorm(v, mean, sd), probs,
                         "normal quantile", "normal")
    cut_text <- vdist_fmt_cutoff(pp, sd, function(v) pnorm(v, mean, sd), probs)
    col <- c("#0000CD", "#6495ED")
  } else if (method == "upper") {
    pp  <- qnorm(probs, mean, sd, lower.tail = F)
    vdist_check_quantile(pp, function(v) pnorm(v, mean, sd, lower.tail = F),
                         probs, "normal quantile", "normal")
    cut_text <- vdist_fmt_cutoff(pp, sd,
                                function(v) pnorm(v, mean, sd, lower.tail = F),
                                probs)
    col <- c("#6495ED", "#0000CD")
  } else {
    alpha <- (1 - probs) / 2
    pp1 <- qnorm(alpha, mean, sd)
    pp2 <- qnorm(alpha, mean, sd, lower.tail = F)
    vdist_check_quantile(pp1, function(v) pnorm(v, mean, sd), alpha,
                         "normal quantile", "normal")
    vdist_check_quantile(pp2, function(v) pnorm(v, mean, sd, lower.tail = F),
                         alpha, "normal quantile", "normal")
    pp  <- c(pp1, pp2)
    cut_text <- c(
      vdist_fmt_cutoff(pp1, sd, function(v) pnorm(v, mean, sd), alpha),
      vdist_fmt_cutoff(pp2, sd, function(v) pnorm(v, mean, sd, lower.tail = F), alpha)
    )
    col <- c("#6495ED", "#0000CD", "#6495ED")
  }
  l1  <- if (method == "both") c(1, 2, 3) else c(1, 2)
  l2  <- if (method == "both") c(2, 3, 4) else c(2, 3)

  # the grid must cover every cutoff, but never beyond the central mass
  k      <- min(vdist_halfwidth(pp, mean, sd, 5), qnorm(1 - 1e-9))
  x      <- vdist_xax(mean, sd, min(vdist_halfwidth(pp, mean, sd, 4), qnorm(1 - 1e-9)))
  l      <- vdist_seql(mean, sd, k)
  ln     <- length(l)
  pc     <- pmin(pmax(pp, l[1]), l[ln])
  lc     <- if (method == "both") c(l[1], pc[1], pc[2], l[ln]) else c(l[1], pc, l[ln])

  omitted <- if (method == "lower") {
    min(probs, pnorm(l[1], mean, sd))
  } else if (method == "upper") {
    min(probs, pnorm(l[ln], mean, sd, lower.tail = FALSE))
  } else {
    0
  }

  plot_data <- data.frame(x = x, y = dnorm(x, mean, sd))
  vdist_check_density(plot_data$y, "normal density")

  list(plot_data = plot_data,
       pp        = pp,
       cut_text  = cut_text,
       prob_text = vdist_pct_text(probs),
       event_prob = probs,
       omitted   = omitted,
       pc        = pc,
       l         = l,
       x         = x,
       alpha     = if (method == "both") alpha else NULL,
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

  if (method == "lower" || method == "upper") {
    pfun_ev <- if (method == "lower") {
      function(v) pnorm(v, mean, sd)
    } else {
      function(v) pnorm(v, mean, sd, lower.tail = FALSE)
    }
    disp <- vdist_fmt_pct(pfun_ev(perc))
    cut_text <- vdist_fmt_value(perc, sd, pfun_ev, tol = disp$tol)
    perc_printed <- as.numeric(cut_text)
    event_prob <- pfun_ev(perc_printed)
    pp1 <- NULL
    pp2 <- NULL
  } else {
    cut_text <- c(
      vdist_fmt_value(perc[1], sd, function(v) pnorm(v, mean, sd), tol = 1e-6),
      vdist_fmt_value(perc[2], sd,
                      function(v) pnorm(v, mean, sd, lower.tail = FALSE),
                      tol = 1e-6)
    )
    perc_printed <- as.numeric(cut_text)
    pp1 <- pnorm(perc_printed[1], mean, sd)
    pp2 <- pnorm(perc_printed[2], mean, sd, lower.tail = FALSE)
    event_prob <- 1 - (pp1 + pp2)
  }

  pc <- pmin(pmax(perc_printed, l[1]), l[ln])

  omitted <- if (method == "lower") {
    min(event_prob, pnorm(l[1], mean, sd))
  } else if (method == "upper") {
    min(event_prob, pnorm(l[ln], mean, sd, lower.tail = FALSE))
  } else {
    0
  }

  if (method == "lower") {
    lc  <- c(l[1], pc, l[ln])
    col <- c("#0000CD", "#6495ED")
    l1  <- c(1, 2)
    l2  <- c(2, 3)
  } else if (method == "upper") {
    lc  <- c(l[1], pc, l[ln])
    col <- c("#6495ED", "#0000CD")
    l1  <- c(1, 2)
    l2  <- c(2, 3)
  } else {
    lc  <- c(l[1], pc[1], pc[2], l[ln])
    col <- c("#6495ED", "#0000CD", "#6495ED")
    l1  <- c(1, 2, 3)
    l2  <- c(2, 3, 4)
  }

  xm <- vdist_xmmp(mean, sd, el)
  plot_data <- data.frame(x = x, y = dnorm(x, mean, sd))
  vdist_check_density(plot_data$y, "normal density")

  list(plot_data = plot_data,
       pp        = event_prob,
       cut_text  = cut_text,
       prob_text = vdist_pct_text(event_prob),
       event_prob = event_prob,
       omitted   = omitted,
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

  x  <- seq(-4, 4, length.out = 801)

  plot_data <- data.frame(x = x, y = dt(x, df))
  poly_data <- data.frame(y = c(-4, x, 4),
                          z = c(0, dt(x, df), 0))

  list(plot_data = plot_data,
       poly_data = poly_data)
}

tperc_data_prep <- function(probs, df, method) {

  if (method == "lower") {
    pp    <- qt(probs, df)
    vdist_check_quantile(pp, function(v) pt(v, df), probs,
                         "t quantile", "t")
    cut_text <- vdist_fmt_cutoff(pp, Inf, function(v) pt(v, df), probs)
    col   <- c("#0000CD", "#6495ED")
  } else if (method == "upper") {
    pp    <- qt(probs, df, lower.tail = F)
    vdist_check_quantile(pp, function(v) pt(v, df, lower.tail = F), probs,
                         "t quantile", "t")
    cut_text <- vdist_fmt_cutoff(pp, Inf,
                                function(v) pt(v, df, lower.tail = F), probs)
    col   <- c("#6495ED", "#0000CD")
  } else {
    alpha <- (1 - probs) / 2
    pp1   <- qt(alpha, df)
    pp2   <- qt(alpha, df, lower.tail = F)
    vdist_check_quantile(pp1, function(v) pt(v, df), alpha,
                         "t quantile", "t")
    vdist_check_quantile(pp2, function(v) pt(v, df, lower.tail = F), alpha,
                         "t quantile", "t")
    pp    <- c(pp1, pp2)
    cut_text <- c(
      vdist_fmt_cutoff(pp1, Inf, function(v) pt(v, df), alpha),
      vdist_fmt_cutoff(pp2, Inf, function(v) pt(v, df, lower.tail = F), alpha)
    )
    col   <- c("#6495ED", "#0000CD", "#6495ED")
  }
  l1 <- if (method == "both") c(1, 2, 3) else c(1, 2)
  l2 <- if (method == "both") c(2, 3, 4) else c(2, 3)

  # the grid must cover every cutoff, but never beyond the central mass
  bound <- vdist_t_bound(max(5, max(abs(pp)) + 1), df)
  bound <- min(bound, qt(1 - 1e-9, df))
  if (!is.finite(bound)) {
    stop(paste0("The t distribution tail cannot be represented for df = ",
                format(df, digits = 15),
                "; use a larger df inside the documented plotting contract."),
         call. = FALSE)
  }
  l     <- vdist_grid(-bound, bound,
                      function(x) pt(x, df),
                      function(p) qt(p, df),
                      quantile = TRUE, n = vdist_grid_n_t(df))
  ln    <- length(l)
  pc    <- pmin(pmax(pp, l[1]), l[ln])
  lc    <- if (method == "both") c(l[1], pc[1], pc[2], l[ln]) else c(l[1], pc, l[ln])

  omitted <- if (method == "lower") {
    min(probs, pt(l[1], df))
  } else if (method == "upper") {
    min(probs, pt(l[ln], df, lower.tail = FALSE))
  } else {
    0
  }

  plot_data <- data.frame(x = l, y = dt(l, df))
  vdist_check_density(plot_data$y, "t density")

  list(plot_data = plot_data,
       l         = l,
       alpha     = if (method == "both") alpha else NULL,
       pp        = pp,
       cut_text  = cut_text,
       prob_text = vdist_pct_text(probs),
       event_prob = probs,
       omitted   = omitted,
       pc        = pc,
       lc        = lc,
       l1        = l1,
       l2        = l2,
       col       = col)

}

tprob_data_prep <- function(perc, df, method) {

  bound <- vdist_t_bound(max(5, abs(perc) + 1), df)
  bound <- min(bound, qt(1 - 1e-9, df))
  if (!is.finite(bound)) {
    stop(paste0("The t distribution tail cannot be represented for df = ",
                format(df, digits = 15),
                "; use a larger df inside the documented plotting contract."),
         call. = FALSE)
  }
  l     <- vdist_grid(-bound, bound,
                      function(x) pt(x, df),
                      function(p) qt(p, df),
                      quantile = TRUE, n = vdist_grid_n_t(df))

  ln <- length(l)

  if (method == "lower" || method == "upper") {
    pfun_ev <- if (method == "lower") {
      function(v) pt(v, df)
    } else {
      function(v) pt(v, df, lower.tail = FALSE)
    }
    disp <- vdist_fmt_pct(pfun_ev(perc))
    cut_text <- vdist_fmt_value(perc, Inf, pfun_ev, tol = disp$tol)
    perc_printed <- as.numeric(cut_text)
    event_prob <- pfun_ev(perc_printed)
    pp1 <- NULL
    pp2 <- NULL
  } else {
    cut_text <- vdist_fmt_value(abs(perc), Inf,
                                function(v) pt(v, df), tol = 1e-6)
    perc_printed <- abs(as.numeric(cut_text))
    pp1 <- pt(-perc_printed, df)
    pp2 <- pt(perc_printed, df, lower.tail = FALSE)
    event_prob <- if (method == "interval") 1 - (pp1 + pp2) else pp1 + pp2
  }

  pc <- min(max(perc_printed, l[1]), l[ln])

  omitted <- if (method == "lower") {
    min(event_prob, pt(l[1], df))
  } else if (method == "upper") {
    min(event_prob, pt(l[ln], df, lower.tail = FALSE))
  } else if (method == "interval") {
    0
  } else {
    min(event_prob, pt(l[1], df) + pt(l[ln], df, lower.tail = FALSE))
  }

  if (method == "lower") {
    lc  <- c(l[1], pc, l[ln])
    col <- c("#0000CD", "#6495ED")
  } else if (method == "upper") {
    lc  <- c(l[1], pc, l[ln])
    col <- c("#6495ED", "#0000CD")
  } else if (method == "interval") {
    lc  <- c(l[1], -pc, pc, l[ln])
    col <- c("#6495ED", "#0000CD", "#6495ED")
  } else {
    lc  <- c(l[1], -pc, pc, l[ln])
    col <- c("#0000CD", "#6495ED", "#0000CD")
  }
  l1  <- if (method %in% c("interval", "both")) c(1, 2, 3) else c(1, 2)
  l2  <- if (method %in% c("interval", "both")) c(2, 3, 4) else c(2, 3)

  plot_data <- data.frame(x = l, y = dt(l, df))
  vdist_check_density(plot_data$y, "t density")

  list(plot_data = plot_data,
       l         = l,
       l1        = l1,
       l2        = l2,
       lc        = lc,
       col       = col,
       pp        = event_prob,
       cut_text  = cut_text,
       prob_text = vdist_pct_text(event_prob),
       event_prob = event_prob,
       omitted   = omitted,
       pp1       = pp1,
       pp2       = pp2,
       pc        = pc)

}
