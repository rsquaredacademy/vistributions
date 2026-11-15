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
# ---------------------------------------------------------------------------
# Continuous families
#
# One distribution spec, one event, one shaded region. Every probability and
# cutoff comes from the distribution's own d/p/q functions; nothing is read back
# from the drawn grid, and no caption is used as a coordinate. The drawing code
# (R/vdist-plots.R) only assembles the values returned here.
# ---------------------------------------------------------------------------

vdist_spec <- function(family, ...) {
  a <- list(...)
  switch(
    family,
    normal = list(
      family  = "normal",
      dfun    = function(x) stats::dnorm(x, a$mean, a$sd),
      pfun    = function(x, lower.tail = TRUE) {
        stats::pnorm(x, a$mean, a$sd, lower.tail = lower.tail)
      },
      qfun    = function(p, lower.tail = TRUE) {
        stats::qnorm(p, a$mean, a$sd, lower.tail = lower.tail)
      },
      support = c(-Inf, Inf),
      center  = a$mean,
      scale   = a$sd,
      standardise = TRUE,
      tail    = 1e-10,
      label   = "Normal Distribution"
    ),
    t = list(
      family  = "t",
      dfun    = function(x) stats::dt(x, a$df),
      pfun    = function(x, lower.tail = TRUE) {
        stats::pt(x, a$df, lower.tail = lower.tail)
      },
      qfun    = function(p, lower.tail = TRUE) {
        stats::qt(p, a$df, lower.tail = lower.tail)
      },
      support = c(-Inf, Inf),
      center  = 0,
      scale   = 1,
      standardise = TRUE,
      tail    = 1e-10,
      label   = "t Distribution"
    ),
    chisquare = list(
      family  = "chisquare",
      dfun    = function(x) stats::dchisq(x, a$df),
      pfun    = function(x, lower.tail = TRUE) {
        stats::pchisq(x, a$df, lower.tail = lower.tail)
      },
      qfun    = function(p, lower.tail = TRUE) {
        stats::qchisq(p, a$df, lower.tail = lower.tail)
      },
      support = c(0, Inf),
      center  = a$df,
      scale   = sqrt(2 * a$df),
      standardise = FALSE,
      tail    = 1e-10,
      label   = "Chi Square Distribution"
    ),
    f = list(
      family  = "f",
      dfun    = function(x) stats::df(x, a$num_df, a$den_df),
      pfun    = function(x, lower.tail = TRUE) {
        stats::pf(x, a$num_df, a$den_df, lower.tail = lower.tail)
      },
      qfun    = function(p, lower.tail = TRUE) {
        stats::qf(p, a$num_df, a$den_df, lower.tail = lower.tail)
      },
      support = c(0, Inf),
      center  = NA_real_,
      scale   = NA_real_,
      standardise = FALSE,
      tail    = 1e-10,
      label   = "f Distribution"
    )
  )
}

# Caption text for a one- or two-cutoff event. The percentage is computed from
# the exact event probability; the printed cutoff is a presentation of the exact
# cutoff chosen so that its own p*() value falls inside the printed
# percentage's rounding interval.
vdist_caption <- function(cut, prob, pfun_ev, scale = Inf) {
  disp <- vdist_fmt_pct(prob)
  cut_text <- if (length(cut) == 1L) {
    vdist_fmt_cutoff(cut, scale, pfun_ev, prob)
  } else {
    vdist_fmt_interval(cut[1], cut[2], scale, pfun_ev, prob)
  }
  list(cut_text  = cut_text,
       prob_text = disp$text,
       other_text = if (prob >= 1) "0" else vdist_pct_text(1 - prob))
}

# Probability of the part of the event that is outside the plotted window. Used
# only for the disclosure caption; the area check uses the polygon, which spans
# the whole event.
vdist_view_omitted <- function(spec, parts, view, rb) {
  inside <- 0
  total  <- 0
  for (p in parts) {
    lo <- max(p[1], rb$lo)
    hi <- min(p[2], rb$hi)
    if (is.finite(hi) && is.finite(lo) && hi > lo) {
      total <- total + (spec$pfun(hi) - spec$pfun(lo))
      vlo <- max(lo, view[1])
      vhi <- min(hi, view[2])
      if (is.finite(vhi) && is.finite(vlo) && vhi > vlo) {
        inside <- inside + (spec$pfun(vhi) - spec$pfun(vlo))
      }
    }
  }
  if (!is.finite(total) || total <= 0) return(0)
  max(0, (total - inside) / total)
}

# Region + caption + view data shared by every continuous prep. `parts` are the
# dark sub-intervals of the event; the drawn polygons are the segments between
# consecutive boundaries, filled dark where the segment is part of the event and
# light elsewhere.
vdist_cont_prep <- function(spec, parts, prob, view, method,
                            cut_draw = NULL, curve_n = 1001L,
                            tol = 2e-4) {
  ev_lo <- min(vapply(parts, function(p) p[1], numeric(1)))
  ev_hi <- max(vapply(parts, function(p) p[2], numeric(1)))
  rb <- vdist_region_bounds(spec, ev_lo, ev_hi)
  rb$lo <- max(rb$lo, spec$support[1])
  rb$hi <- min(rb$hi, spec$support[2])
  cover <- if (rb$hi > rb$lo) spec$pfun(rb$hi) - spec$pfun(rb$lo) else 0
  if (!is.finite(cover) || prob - cover > 1e-4) {
    stop(paste0("The shaded part of the ", spec$family, " distribution ",
                "cannot be represented numerically: only ",
                format(cover / max(prob, 1e-300) * 100, digits = 4),
                "% of the event is inside the representable tails. Use a ",
                "less extreme probability."), call. = FALSE)
  }

  cut <- sort(unique(unlist(lapply(parts, function(p) p[is.finite(p)]))))
  bounds <- if (rb$hi > rb$lo) {
    sort(unique(c(rb$lo, rb$hi,
                  vdist_clamp(unlist(parts, use.names = FALSE),
                              c(rb$lo, rb$hi)))))
  } else {
    numeric(0)
  }
  bounds <- bounds[is.finite(bounds)]
  nseg <- length(bounds) - 1L
  if (nseg < 1L) {
    lc <- numeric(0); l1 <- integer(0); l2 <- integer(0); col <- character(0)
  } else {
    mid  <- (bounds[-length(bounds)] + bounds[-1]) / 2
    dark <- vapply(mid, function(m) {
      any(vapply(parts, function(p) m >= p[1] && m <= p[2], logical(1)))
    }, logical(1))
    lc  <- bounds
    l1  <- seq_len(nseg)
    l2  <- l1 + 1L
    col <- ifelse(dark, "#0000CD", "#6495ED")
  }

  # curve over the view
  x <- vdist_grid(view[1], view[2], spec$pfun, spec$qfun,
                  n = curve_n, quantile = TRUE, uniform = TRUE)
  y <- spec$dfun(x)
  ok <- is.finite(y)
  x <- x[ok]; y <- y[ok]
  vdist_check_density(y, paste(spec$family, "density"))
  plot_data <- data.frame(x = x, y = y)

  # cutoffs drawn as reference lines are clamped to the view; the caption keeps
  # the exact value
  pc <- vdist_clamp(cut, view)
  if (!is.null(cut_draw)) pc <- vdist_clamp(cut_draw, view)

  list(spec       = spec,
       plot_data  = plot_data,
       x          = x,
       l          = view,
       ln         = length(view),
       xm         = view,
       view       = view,
       breaks     = vdist_axis_breaks(view, spec$scale, spec$center),
       cut        = cut,
       pc         = pc,
       lc         = lc,
       l1         = l1,
       l2         = l2,
       col        = col,
       event_prob = prob,
       omitted    = vdist_view_omitted(spec, parts, view, rb),
       ycap       = vdist_density_cap(spec$dfun, view[1], view[2]),
       point_data = data.frame(x = pc, y = 0))
}

# ---------------------------------------------------------------------------
# Chi-square
# ---------------------------------------------------------------------------

cplot_data_prep <- function(df, range) {

  chim  <- round(df, 3)
  chisd <- round(sqrt(2 * df), 3)
  spec  <- vdist_spec("chisquare", df = df)

  if (!is.finite(range) || range <= 0) {
    stop("xaxis_range must be greater than the start of the chi-square support.",
         call. = FALSE)
  }
  reg <- vdist_region(0, range, spec$dfun, spec$pfun, spec$qfun, n = 65L)
  vdist_check_region(reg$area, spec$pfun(range),

                     "chi-square plot")

  x <- vdist_grid(0, range, spec$pfun, spec$qfun, quantile = TRUE,
                  n = 1001L, uniform = TRUE)
  y <- spec$dfun(x)
  ok <- is.finite(y)
  x <- x[ok]; y <- y[ok]
  vdist_check_density(y, "chi-square density")

  list(plot_data  = data.frame(x = x, chi = y),
       poly_data  = data.frame(y = reg$x, z = reg$y),
       point_data = data.frame(x = chim, y = min(y)),
       nline_data = data.frame(x = x, y = dnorm(x, df, sqrt(2 * df))),
       chim       = chim,
       chisd      = chisd,
       view       = c(0, range),
       breaks     = if (range <= 100) seq(0, range, 2) else ggplot2::waiver(),
       ycap       = vdist_density_cap(spec$dfun, 0, range))
}

cperc_data_prep <- function(probs, df, method) {

  chim  <- round(df, 3)
  chisd <- round(sqrt(2 * df), 3)
  spec  <- vdist_spec("chisquare", df = df)

  if (method == "lower") {
    cut <- vdist_check_quantile(qchisq(probs, df), function(v) pchisq(v, df),
                                probs, "chi-square quantile", "chisquare")
    pfun_ev <- function(v, lower.tail = TRUE) pchisq(v, df, lower.tail = lower.tail)
    prob    <- pfun_ev(cut)
    parts   <- list(c(-Inf, cut))
    cap_ev  <- pfun_ev
  } else {
    cut <- vdist_check_quantile(qchisq(probs, df, lower.tail = FALSE),
                                function(v) pchisq(v, df, lower.tail = FALSE),
                                probs, "chi-square quantile", "chisquare")
    pfun_ev <- function(v, lower.tail = TRUE) pchisq(v, df, lower.tail = lower.tail)
    prob    <- pfun_ev(cut, lower.tail = FALSE)
    parts   <- list(c(cut, Inf))
    cap_ev  <- function(v, lower.tail = TRUE) pfun_ev(v, lower.tail = FALSE)
  }

  cap  <- vdist_caption(cut, prob, cap_ev)
  tail_hi <- vdist_tail_quantile(spec$qfun, 1e-4, lower.tail = FALSE)$q
  hi <- max(chim + 5 * chisd, cut + chisd,
            if (method == "upper") tail_hi else -Inf)
  hiq <- vdist_tail_quantile(spec$qfun, 1e-9, lower.tail = FALSE)$q
  hi <- min(max(chim + 5 * chisd, cut + chisd, hi), hiq)
  view <- c(0, max(hi, 1e-8))

  out <- vdist_cont_prep(spec, parts, prob, view, method)
  out$chim      <- chim
  out$chisd     <- chisd
  out$cut_text  <- cap$cut_text
  out$prob_text <- cap$prob_text
  out$other_text <- cap$other_text
  out$pp        <- cut
  out$pp_event  <- cut
  out
}

cprob_data_prep <- function(perc, df, method) {

  chim  <- round(df, 3)
  chisd <- round(sqrt(2 * df), 3)
  spec  <- vdist_spec("chisquare", df = df)

  if (method == "lower") {
    pfun_ev <- function(v, lower.tail = TRUE) pchisq(v, df, lower.tail = lower.tail)
    prob    <- pfun_ev(perc)
    parts   <- list(c(-Inf, perc))
  } else {
    pfun_ev <- function(v, lower.tail = TRUE) pchisq(v, df, lower.tail = lower.tail)
    prob    <- pfun_ev(perc, lower.tail = FALSE)
    parts   <- list(c(perc, Inf))
  }
  disp <- vdist_fmt_pct(prob)
  cut_text <- vdist_fmt_value(perc, Inf, pfun_ev, tol = disp$tol)

  tail_hi <- vdist_tail_quantile(spec$qfun, 1e-4, lower.tail = FALSE)$q
  hi <- if (perc < 25) 25 else (perc + (3 * chisd))
  if (method == "upper") hi <- max(hi, tail_hi)
  hiq <- vdist_tail_quantile(spec$qfun, 1e-9, lower.tail = FALSE)$q
  hi <- min(max(hi, perc + chisd, 1), max(hiq, 1))
  view <- c(0, hi)

  out <- vdist_cont_prep(spec, parts, prob, view, method)
  out$chim       <- chim
  out$chisd      <- chisd
  out$cut_text   <- cut_text
  out$prob_text  <- disp$text
  out$other_text <- if (prob >= 1) "0" else vdist_pct_text(1 - prob)
  out$pp         <- perc
  out$pp_event   <- perc
  out
}

# ---------------------------------------------------------------------------
# F
# ---------------------------------------------------------------------------

vdist_f_moments <- function(num_df, den_df) {
  fm  <- if (den_df > 2) den_df / (den_df - 2) else NA_real_
  fsd <- if (den_df > 4) {
    sqrt((2 * (fm ^ 2) * (num_df + den_df - 2)) / (num_df * (den_df - 4)))
  } else {
    NA_real_
  }
  list(fm = fm, fsd = fsd)
}

fplot_data_prep <- function(num_df, den_df) {

  fm  <- if (den_df > 2) den_df / (den_df - 2) else NA_real_
  fsd <- if (den_df > 4) {
    sqrt((2 * (fm ^ 2) * (num_df + den_df - 2)) / (num_df * (den_df - 4)))
  } else {
    NA_real_
  }
  spec <- vdist_spec("f", num_df = num_df, den_df = den_df,
                     mean = fm, sd = fsd)

  vdist_check_f_concentration(num_df, den_df)
  reg <- vdist_region(0, 4, spec$dfun, spec$pfun, spec$qfun, n = 65L)
  vdist_check_region(reg$area, pf(4, num_df, den_df), "F plot")

  x <- vdist_grid(0, 4, spec$pfun, spec$qfun, quantile = TRUE,
                  n = 1001L, uniform = TRUE)
  y <- spec$dfun(x)
  ok <- is.finite(y)
  x <- x[ok]; y <- y[ok]
  vdist_check_density(y, "F density")

  point_data <- if (!is.na(fm)) {
    data.frame(x = fm, y = 0)
  } else {
    data.frame(x = numeric(0), y = numeric(0))
  }
  nline_data <- if (!is.na(fsd)) {
    data.frame(x = x, y = dnorm(x, fm, fsd))
  } else {
    data.frame(x = numeric(0), y = numeric(0))
  }

  list(plot_data  = data.frame(x = x, y = y),
       poly_data  = data.frame(y = reg$x, z = reg$y),
       point_data = point_data,
       nline_data = nline_data,
       fm         = fm,
       fsd        = fsd,
       view       = c(0, 4),
       breaks     = pretty(c(0, 4), n = 4),
       ycap       = vdist_density_cap(spec$dfun, 0, 4))
}

fperc_data_prep <- function(probs, num_df, den_df, method) {

  fm  <- if (den_df > 2) den_df / (den_df - 2) else NA_real_
  fsd <- if (den_df > 4) {
    sqrt((2 * (fm ^ 2) * (num_df + den_df - 2)) / (num_df * (den_df - 4)))
  } else {
    NA_real_
  }
  spec <- vdist_spec("f", num_df = num_df, den_df = den_df,
                     mean = fm, sd = fsd)
  vdist_check_f_concentration(num_df, den_df)

  if (method == "lower") {
    cut <- vdist_check_quantile(qf(probs, num_df, den_df),
                                function(v) pf(v, num_df, den_df), probs,
                                "F quantile", "f")
    pfun_ev <- function(v, lower.tail = TRUE) {
      pf(v, num_df, den_df, lower.tail = lower.tail)
    }
    prob  <- pfun_ev(cut)
    parts <- list(c(-Inf, cut))
    cap_ev <- pfun_ev
  } else {
    cut <- vdist_check_quantile(qf(probs, num_df, den_df, lower.tail = FALSE),
                                function(v) pf(v, num_df, den_df, lower.tail = FALSE),
                                probs, "F quantile", "f")
    pfun_ev <- function(v, lower.tail = TRUE) {
      pf(v, num_df, den_df, lower.tail = lower.tail)
    }
    prob  <- pfun_ev(cut, lower.tail = FALSE)
    parts <- list(c(cut, Inf))
    cap_ev <- function(v, lower.tail = TRUE) pfun_ev(v, lower.tail = FALSE)
  }

  cap <- vdist_caption(cut, prob, cap_ev)
  tail_hi <- vdist_tail_quantile(spec$qfun, 1e-4, lower.tail = FALSE)$q
  hi <- if (max(cut) < 4) 4 else max(cut) * 1.25
  if (method == "upper") hi <- max(hi, tail_hi)
  hiq <- vdist_tail_quantile(spec$qfun, 1e-9, lower.tail = FALSE)$q
  hi <- min(max(hi, 4, max(cut) * 1.001), max(hiq, 1))
  view <- c(0, hi)

  out <- vdist_cont_prep(spec, parts, prob, view, method)
  out$fm        <- fm
  out$fsd       <- fsd
  out$breaks    <- pretty(out$view, n = 4)
  out$cut_text  <- cap$cut_text
  out$prob_text <- cap$prob_text
  out$other_text <- cap$other_text
  out$pp        <- cut
  out$pp_event  <- cut
  out
}

fprob_data_prep <- function(perc, num_df, den_df, method) {

  fm  <- if (den_df > 2) den_df / (den_df - 2) else NA_real_
  fsd <- if (den_df > 4) {
    sqrt((2 * (fm ^ 2) * (num_df + den_df - 2)) / (num_df * (den_df - 4)))
  } else {
    NA_real_
  }
  spec <- vdist_spec("f", num_df = num_df, den_df = den_df,
                     mean = fm, sd = fsd)
  vdist_check_f_concentration(num_df, den_df)

  pfun_ev <- function(v, lower.tail = TRUE) {
    pf(v, num_df, den_df, lower.tail = lower.tail)
  }
  if (method == "lower") {
    prob  <- pfun_ev(perc)
    parts <- list(c(-Inf, perc))
  } else {
    prob  <- pfun_ev(perc, lower.tail = FALSE)
    parts <- list(c(perc, Inf))
  }
  disp <- vdist_fmt_pct(prob)
  cut_text <- vdist_fmt_value(perc, Inf, pfun_ev, tol = disp$tol)

  tail_hi <- vdist_tail_quantile(spec$qfun, 1e-4, lower.tail = FALSE)$q
  hi <- if (perc < 4) 4 else (perc * 1.25)
  if (method == "upper") hi <- max(hi, tail_hi)
  hiq <- vdist_tail_quantile(spec$qfun, 1e-9, lower.tail = FALSE)$q
  hi <- min(max(hi, 4, perc * 1.001), max(hiq, 1))
  view <- c(0, hi)

  out <- vdist_cont_prep(spec, parts, prob, view, method)
  out$fm        <- fm
  out$fsd       <- fsd
  out$breaks    <- pretty(out$view, n = 4)
  out$cut_text  <- cut_text
  out$prob_text <- disp$text
  out$other_text <- if (prob >= 1) "0" else vdist_pct_text(1 - prob)
  out$pp        <- perc
  out$pp_event  <- perc
  out
}

# ---------------------------------------------------------------------------
# Normal
# ---------------------------------------------------------------------------

vdist_xax <- function(mean, sd, k = 4) {
  k <- max(k, 4)
  seq(mean - (k * sd), mean + (k * sd), length.out = 1000)
}

# half-width (in sd units) needed to show every cutoff with a margin
vdist_halfwidth <- function(cutoff, mean, sd, min_k) {
  max(min_k, max(abs(cutoff - mean)) / sd + 1)
}

# axis grid: mean +/- k sd in steps of sd (coarser when k is very large)
vdist_seql <- function(mean, sd, k = 5) {
  lmin <- mean - (k * sd)
  lmax <- mean + (k * sd)
  if (k > 50) {
    seq(lmin, lmax, length.out = 11)
  } else {
    out <- seq(lmin, lmax, sd)
    if (length(out) > 1 && out[length(out)] < lmax) {
      out <- c(out, lmax)
    }
    out
  }
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
       spec      = vdist_spec("normal", mean = mean, sd = sd),
       ycap      = vdist_density_cap(function(v) dnorm(v, mean, sd),
                                     mean - 4 * sd, mean + 4 * sd),
       l         = l,
       col       = col,
       l1        = l1,
       l2        = l2,
       ll        = ll,
       view      = c(mean - 4 * sd, mean + 4 * sd))
}

nperc_data_prep <- function(probs, mean, sd, method) {

  spec <- vdist_spec("normal", mean = mean, sd = sd)
  pfun_ev <- function(v, lower.tail = TRUE) pnorm(v, mean, sd, lower.tail = lower.tail)

  if (method == "lower") {
    cut  <- vdist_check_quantile(qnorm(probs, mean, sd), pfun_ev, probs,
                                 "normal quantile", "normal")
    prob <- pfun_ev(cut)
    parts <- list(c(-Inf, cut))
    cap <- vdist_caption(cut, prob, pfun_ev, sd)
    alpha <- NULL
  } else if (method == "upper") {
    cut  <- vdist_check_quantile(qnorm(probs, mean, sd, lower.tail = FALSE),
                                 function(v) pfun_ev(v, lower.tail = FALSE),
                                 probs, "normal quantile", "normal")
    prob <- pfun_ev(cut, lower.tail = FALSE)
    parts <- list(c(cut, Inf))
    cap <- vdist_caption(cut, prob,
                         function(v, lower.tail = TRUE) pfun_ev(v, lower.tail = FALSE),
                         sd)
    alpha <- NULL
  } else {
    alpha <- (1 - probs) / 2
    c1 <- vdist_check_quantile(qnorm(alpha, mean, sd), pfun_ev, alpha,
                               "normal quantile", "normal")
    c2 <- vdist_check_quantile(qnorm(alpha, mean, sd, lower.tail = FALSE),
                               function(v) pfun_ev(v, lower.tail = FALSE),
                               alpha, "normal quantile", "normal")
    cut  <- c(c1, c2)
    prob <- pfun_ev(c2) - pfun_ev(c1)
    parts <- list(c(c1, c2))
    cap <- vdist_caption(cut, prob, pfun_ev, sd)
  }

  k <- min(vdist_halfwidth(cut, mean, sd, 4), qnorm(1 - 1e-9))
  view <- c(mean - k * sd, mean + k * sd)

  out <- vdist_cont_prep(spec, parts, prob, view, method, cut_draw = cut)
  out$alpha     <- alpha
  out$pp1       <- pfun_ev(cut[1])
  out$cut_text  <- cap$cut_text
  out$prob_text <- cap$prob_text
  out$other_text <- cap$other_text
  out$pp        <- cut
  out$pp_event  <- cut
  out
}

nprob_data_prep <- function(perc, mean, sd, method) {

  spec <- vdist_spec("normal", mean = mean, sd = sd)
  pfun <- function(v, lower.tail = TRUE) pnorm(v, mean, sd, lower.tail = lower.tail)

  if (method == "lower" || method == "upper") {
    pfun_ev <- if (method == "lower") pfun else function(v, lower.tail = TRUE) {
      pfun(v, lower.tail = FALSE)
    }
    prob <- pfun_ev(perc)
    parts <- list(if (method == "lower") c(-Inf, perc) else c(perc, Inf))
    disp <- vdist_fmt_pct(prob)
    cut_text <- vdist_fmt_value(perc, sd, pfun_ev, tol = disp$tol)
    pp1 <- NULL; pp2 <- NULL
  } else {
    prob <- pfun(perc[2]) - pfun(perc[1])
    parts <- list(c(perc[1], perc[2]))
    disp <- vdist_fmt_pct(prob)
    cut_text <- vdist_fmt_interval(perc[1], perc[2], sd, pfun, prob)
    pp1 <- pfun(perc[1])
    pp2 <- pfun(perc[2], lower.tail = FALSE)
  }

  el <- min(max(abs(perc - mean)) / sd + 1, qnorm(1 - 1e-9))
  span <- max(4, el) * sd
  view <- c(mean - span, mean + span)

  out <- vdist_cont_prep(spec, parts, prob, view, method, cut_draw = perc)
  out$cut_text   <- cut_text
  out$prob_text  <- disp$text
  out$other_text <- if (prob >= 1) "0" else vdist_pct_text(1 - prob)
  out$pp1        <- pp1
  out$pp2        <- pp2
  out$pp         <- prob
  out$pp_event   <- prob
  out
}

# ---------------------------------------------------------------------------
# t
# ---------------------------------------------------------------------------

vdist_t_bound <- function(bound, df, tail = 1e-4) {
  max(bound, abs(qt(tail, df)), abs(qt(tail, df, lower.tail = FALSE)))
}

tplot_data_prep <- function(df) {

  spec <- vdist_spec("t", df = df)
  x <- seq(-4, 4, length.out = 801)
  reg <- vdist_region(-4, 4, spec$dfun, spec$pfun, spec$qfun, n = 65L)
  vdist_check_region(reg$area, pt(4, df) - pt(-4, df), "t plot")

  list(plot_data = data.frame(x = x, y = dt(x, df)),
       poly_data = data.frame(y = reg$x, z = reg$y),
       ycap      = vdist_density_cap(spec$dfun, -4, 4),
       view      = c(-4, 4))
}

tperc_data_prep <- function(probs, df, method) {

  spec <- vdist_spec("t", df = df)
  pfun <- function(v, lower.tail = TRUE) pt(v, df, lower.tail = lower.tail)

  if (method == "lower") {
    cut <- vdist_check_quantile(qt(probs, df), pfun, probs, "t quantile", "t")
    prob <- pfun(cut)
    parts <- list(c(-Inf, cut))
    cap <- vdist_caption(cut, prob, pfun)
    alpha <- NULL
  } else if (method == "upper") {
    cut <- vdist_check_quantile(qt(probs, df, lower.tail = FALSE),
                                function(v) pfun(v, lower.tail = FALSE), probs,
                                "t quantile", "t")
    prob <- pfun(cut, lower.tail = FALSE)
    parts <- list(c(cut, Inf))
    cap <- vdist_caption(cut, prob,
                         function(v, lower.tail = TRUE) pfun(v, lower.tail = FALSE))
    alpha <- NULL
  } else {
    alpha <- (1 - probs) / 2
    c1 <- vdist_check_quantile(qt(alpha, df), pfun, alpha, "t quantile", "t")
    c2 <- vdist_check_quantile(qt(alpha, df, lower.tail = FALSE),
                               function(v) pfun(v, lower.tail = FALSE), alpha,
                               "t quantile", "t")
    cut <- c(c1, c2)
    prob <- pfun(c2) - pfun(c1)
    parts <- list(c(c1, c2))
    cap <- vdist_caption(cut, prob, pfun)
  }

  bound <- max(5, max(abs(cut)) + 1)
  boundq <- vdist_tail_quantile(spec$qfun, 1e-9, lower.tail = FALSE)$q
  bound <- min(bound, boundq)
  if (!is.finite(bound)) {
    stop(paste0("The t distribution tail cannot be represented for df = ",
                format(df, digits = 15),
                "; use a larger df inside the documented plotting contract."),
         call. = FALSE)
  }
  view <- c(-bound, bound)

  out <- vdist_cont_prep(spec, parts, prob, view, method)
  out$alpha     <- alpha
  out$pp1       <- pfun(cut[1])
  out$cut_text  <- cap$cut_text
  out$prob_text <- cap$prob_text
  out$other_text <- cap$other_text
  out$pp        <- cut
  out$pp_event  <- cut
  out
}

tprob_data_prep <- function(perc, df, method) {

  spec <- vdist_spec("t", df = df)
  pfun <- function(v, lower.tail = TRUE) pt(v, df, lower.tail = lower.tail)

  if (method == "lower" || method == "upper") {
    pfun_ev <- if (method == "lower") pfun else function(v, lower.tail = TRUE) {
      pfun(v, lower.tail = FALSE)
    }
    prob  <- pfun_ev(perc)
    parts <- list(if (method == "lower") c(-Inf, perc) else c(perc, Inf))
    disp  <- vdist_fmt_pct(prob)
    cut_text <- vdist_fmt_value(perc, Inf, pfun_ev, tol = disp$tol)
    pp1 <- NULL; pp2 <- NULL
  } else if (method == "interval") {
    p <- abs(perc)
    prob <- pfun(p) - pfun(-p)
    parts <- list(c(-p, p))
    disp <- vdist_fmt_pct(prob)
    cut_text <- vdist_fmt_cutoff(p, Inf, function(v) pfun(v) - pfun(-v), prob)
    pp1 <- pfun(-p); pp2 <- pfun(p, lower.tail = FALSE)
  } else {
    p <- abs(perc)
    prob <- 2 * pfun(-p)
    parts <- list(c(-Inf, -p), c(p, Inf))
    disp <- vdist_fmt_pct(prob)
    cut_text <- vdist_fmt_cutoff(p, Inf, function(v) 2 * pfun(-abs(v)), prob)
    pp1 <- pfun(-p); pp2 <- pfun(p, lower.tail = FALSE)
  }

  bound <- max(5, abs(perc) + 1)
  boundq <- vdist_tail_quantile(spec$qfun, 1e-9, lower.tail = FALSE)$q
  bound <- min(bound, boundq)
  if (!is.finite(bound)) {
    stop(paste0("The t distribution tail cannot be represented for df = ",
                format(df, digits = 15),
                "; use a larger df inside the documented plotting contract."),
         call. = FALSE)
  }
  view <- c(-bound, bound)

  out <- vdist_cont_prep(spec, parts, prob, view, method,
                         cut_draw = if (method %in% c("interval", "both")) {
                           abs(perc)
                         } else {
                           perc
                         })
  out$cut_text   <- cut_text
  out$prob_text  <- disp$text
  out$other_text <- if (prob >= 1) "0" else vdist_pct_text(1 - prob)
  out$pp1        <- pp1
  out$pp2        <- pp2
  out$pp         <- prob
  out$pp_event   <- prob
  out
}
