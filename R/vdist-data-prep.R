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
  g    <- vdist_count_grid(n)
  data <- vdist_count_prob(g, n, p)
  bm   <- round(n * p, 2)
  bsd  <- round(sqrt(n * p * (1 - p)), 2)
  plot_data <- data.frame(n = g$x, df = data, w = g$w)

  list(plot_data = plot_data, bm = bm, bsd = bsd, binned = g$binned)

}

bprob_data_prep <- function(n, p, s, method) {

  n   <- round(n)
  # A bar is painted as a whole, so a bin that crosses the event cutoff would
  # shade counts outside the event (or miss counts inside it). The boundaries
  # below are integer count edges, so every bar still covers whole counts.
  brk <- switch(method,
                lower    = s + 1,
                upper    = s,
                exact    = c(s, s + 1),
                interval = c(s[1], s[2] + 1))
  g   <- vdist_count_grid(n, brk)
  bm  <- round(n * p, 2)
  bsd <- round(sqrt(n * p * (1 - p)), 2)
  last <- g$hi - 1

  if (method == "lower") {
    k    <- stats::pbinom(s, n, p)
    cols <- ifelse(last <= s, "#0000CD", "#6495ED")
  } else if (method == "upper") {
    k    <- stats::pbinom(s - 1, n, p, lower.tail = FALSE)
    cols <- ifelse(g$lo >= s, "#0000CD", "#6495ED")
  } else if (method == "exact") {
    k    <- stats::dbinom(s, n, p)
    cols <- ifelse(g$lo <= s & last >= s, "#0000CD", "#6495ED")
  } else {
    k    <- vdist_binom_between(s[1], s[2], n, p)
    cols <- ifelse(last >= s[1] & g$lo <= s[2], "#0000CD", "#6495ED")
  }

  data      <- vdist_count_prob(g, n, p)
  plot_data <- data.frame(n = g$x, df = data, w = g$w)

  list(plot_data = plot_data, bm = bm, bsd = bsd, k = k, cols = cols,
       binned = g$binned)
}


bperc_data_prep <- function(n, p, tp, method) {

  n <- round(n)

  # Use precisely the same tail and fuzz convention as qbinom().
  k <- stats::qbinom(tp, n, p, lower.tail = method == "lower")
  if (!is.finite(k)) stop("tp cannot be resolved by qbinom().", call. = FALSE)
  k <- min(max(k, 0), n)

  g      <- vdist_count_grid(n, k + 1)
  last   <- g$hi - 1

  if (method == "lower") {
    cols <- ifelse(last <= k, "#0000CD", "#6495ED")
  } else {
    cols <- ifelse(g$lo >= k + 1, "#0000CD", "#6495ED")
  }

  data      <- vdist_count_prob(g, n, p)
  plot_data <- data.frame(n = g$x, df = data, w = g$w)

  list(plot_data = plot_data, k = k, cols = cols, binned = g$binned)
}

# Bounded bar grid for a count distribution. Every bar is one count while the
# support fits in `max_bars`; a wider support is aggregated into equal-width
# bins of counts (each bar then shows the probability of its bin), because one
# bar per count would be narrower than a pixel and the grid would grow with n.
# The `binned` flag lets the plot disclose this.
#
# `breaks` are count boundaries at which a bin must be split (the event
# cutoffs). They are integers, so every bin still covers whole counts and the
# shaded mass can equal the exact event probability.
vdist_count_grid <- function(n, breaks = numeric(0), max_bars = 2001L) {
  brk <- unique(round(breaks[is.finite(breaks)]))
  brk <- brk[brk >= 0 & brk <= n + 1]
  if (n + 1 <= max_bars) {
    x <- seq(0, n, 1)
    # each bar is the half-open count interval [x, x + 1); every integer
    # boundary is already a bin edge
    return(list(x = x, lo = x, hi = x + 1, w = rep(1, length(x)),
                binned = FALSE))
  }
  edges <- unique(round(seq(0, n + 1, length.out = max_bars + 1)))
  edges <- sort(unique(c(edges, brk)))
  lo <- edges[-length(edges)]
  hi <- edges[-1]
  keep <- hi > lo
  lo <- lo[keep]
  hi <- hi[keep]
  list(x = (lo + hi - 1) / 2, lo = lo, hi = hi, w = hi - lo, binned = TRUE)
}

# Exact mass of the counts [a, b], evaluated on the side of the mode that keeps
# its precision: above the median the survival function is small and accurate,
# while the difference of two lower-tail values has saturated at 1.
vdist_binom_between <- function(a, b, n, p) {
  up  <- stats::pbinom(a - 1, n, p) > 0.5
  low <- stats::pbinom(b, n, p) - stats::pbinom(a - 1, n, p)
  hi  <- stats::pbinom(a - 1, n, p, lower.tail = FALSE) -
    stats::pbinom(b, n, p, lower.tail = FALSE)
  out <- ifelse(up, hi, low)
  out[!is.finite(out)] <- 0
  pmax(out, 0)
}

# Probability carried by each bar of the bounded grid: the exact probability of
# a single count, or the exact mass of a bin of counts. A bin [lo, hi) holds the
# counts lo ... hi - 1.
vdist_count_prob <- function(g, n, p) {
  vdist_binom_between(g$lo, g$hi - 1, n, p)
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
        vdist_f_quantile(p, a$num_df, a$den_df, lower.tail = lower.tail)
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
vdist_caption <- function(cut, prob, pfun_ev, scale = Inf, other = NULL,
                          dfun = NULL, otherfun = NULL) {
  disp <- vdist_fmt_pct(prob)
  cut_text <- if (length(cut) == 1L && !is.null(otherfun)) {
    vdist_fmt_value(cut, scale, pfun_ev, disp$tol, disp$value, otherfun, vdist_fmt_pct(other), disp$text)
  } else if (length(cut) == 1L) {
    vdist_fmt_cutoff(cut, scale, pfun_ev, prob)
  } else {
    vdist_fmt_interval(cut[1], cut[2], scale, pfun_ev, prob, dfun)
  }
  # `other` is the complement computed on its own scale. 1 - prob loses every
  # small tail probability (1 - 0.9999999999999 is 1e-13 at best), so the
  # caller passes the survival value instead.
  if (is.null(other)) {
    other <- if (prob >= 1) 0 else 1 - prob
  }
  list(cut_text  = cut_text,
       prob_text = disp$text,
       other_text = vdist_pct_text(other))
}

# Probability of the part of the event that is outside the plotted window. Used
# only for the disclosure caption; the area check uses the polygon, which spans
# the whole event.
vdist_view_omitted <- function(spec, parts, view, rb, prob) {
  inside <- 0
  for (p in parts) {
    lo <- max(p[1], rb$lo)
    hi <- min(p[2], rb$hi)
    if (is.finite(hi) && is.finite(lo) && hi > lo) {
      vlo <- max(lo, view[1])
      vhi <- min(hi, view[2])
      if (is.finite(vhi) && is.finite(vlo) && vhi > vlo) {
        inside <- inside + vdist_prob_between(spec$pfun, spec$dfun, vlo, vhi)
      }
    }
  }
  # The denominator is the event probability, not the truncated part that was
  # representable: dividing by the latter silently rescales the disclosure.
  if (!is.finite(prob) || prob <= 0) return(0)
  max(0, min(1, 1 - inside / prob))
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
  if (!is.finite(view[2] - view[1])) {
    stop(paste0("The plotted range spans more than the double range; ",
                "use a smaller scale so the window is representable."),
         call. = FALSE)
  }
  rb <- vdist_region_bounds(spec, ev_lo, ev_hi, prob)
  rb$lo <- max(rb$lo, spec$support[1])
  rb$hi <- min(rb$hi, spec$support[2])
  cover <- if (rb$hi > rb$lo) {
    vdist_prob_between(spec$pfun, spec$dfun, rb$lo, rb$hi)
  } else {
    0
  }
  # Relative, not an absolute milliprobability: an absolute bound would allow
  # an event of probability 1e-10 to lose all of its mass and still pass.
  if (!is.finite(cover) || !is.finite(prob) ||
      prob >= 1e-12 && cover < prob * (1 - 1e-6)) {
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
    mid  <- vdist_mid(bounds[-length(bounds)], bounds[-1])
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

  omitted <- vdist_view_omitted(spec, parts, view, rb, prob)
  cutoff_outside <- !is.null(cut_draw) && any(cut_draw < view[1] | cut_draw > view[2]) ||
    any(cut < view[1] | cut > view[2])

  list(cutoff_outside = cutoff_outside,
       spec       = spec,
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
       omitted    = omitted,
       underflow  = isTRUE(prob == 0),
       subpixel   = vdist_subpixel_region(parts, view, rb, prob, omitted),
       ycap       = vdist_density_cap(spec$dfun, view[1], view[2],
                                      spec$pfun, spec$qfun),
       point_data = data.frame(x = pc, y = 0))
}

# ---------------------------------------------------------------------------
# Chi-square
# ---------------------------------------------------------------------------

cplot_data_prep <- function(df, range, lower = 0) {

  chim  <- round(df, 3)
  chisd <- round(sqrt(2 * df), 3)
  spec  <- vdist_spec("chisquare", df = df)

  if (!is.finite(range) || range <= 0) {
    stop("xaxis_range must be greater than the start of the chi-square support.",
         call. = FALSE)
  }
  reg <- vdist_region(lower, range, spec$dfun, spec$pfun, spec$qfun, n = 65L)
  vdist_check_region(reg$area, spec$pfun(range) - spec$pfun(lower),

                     "chi-square plot")

  x <- vdist_grid(lower, range, spec$pfun, spec$qfun, quantile = TRUE,
                  n = 1001L, uniform = TRUE)
  y <- spec$dfun(x)
  ok <- is.finite(y)
  x <- x[ok]; y <- y[ok]
  vdist_check_density(y, "chi-square density")

  list(plot_data  = data.frame(x = x, chi = y),
       poly_data  = data.frame(y = reg$x, z = reg$y),
       point_data = data.frame(x = df, y = 0),
       nline_data = data.frame(x = x, y = dnorm(x, df, sqrt(2 * df))),
       chim       = chim,
       chisd      = chisd,
       view       = c(lower, range),
       breaks     = if (range <= 100) seq(0, range, 2) else ggplot2::waiver(),
       ycap       = vdist_density_cap(spec$dfun, lower, range,
                                      spec$pfun, spec$qfun))
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

  cap  <- vdist_caption(cut, prob, cap_ev, other = if (method == "lower") {
    pfun_ev(cut, lower.tail = FALSE)
  } else {
    pfun_ev(cut)
  }, otherfun = function(v) pfun_ev(v, lower.tail = method != "lower"))
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

  pfun <- function(v, lower.tail = TRUE) pchisq(v, df, lower.tail = lower.tail)
  if (method == "lower") {
    pfun_ev <- pfun
    prob    <- pfun_ev(perc)
    parts   <- list(c(-Inf, perc))
  } else {
    # The caption's cutoff must reproduce the event probability, so the closure
    # passed to the formatter evaluates the same (upper) tail as `prob`.
    pfun_ev <- function(v, lower.tail = TRUE) pfun(v, lower.tail = FALSE)
    prob    <- pfun_ev(perc)
    parts   <- list(c(perc, Inf))
  }
  disp <- vdist_fmt_pct(prob)
  otherfun <- function(v) pfun(v, lower.tail = method != "lower")
  cut_text <- vdist_fmt_value(perc, Inf, pfun_ev, tol = disp$tol,
                              displayed = disp$value, display_text = disp$text, otherfun = otherfun,
                              other = vdist_fmt_pct(otherfun(perc)))

  tail_hi <- vdist_tail_quantile(spec$qfun, 1e-4, lower.tail = FALSE)$q
  hi <- if (perc < 25) 25 else (perc + (3 * chisd))
  if (method == "upper") hi <- max(hi, tail_hi)
  # A cutoff far below the body of a high-df distribution would leave the whole
  # panel in a negligible left tail: df = 200 with perc = 13 on [0, 25] draws a
  # flat panel 40 orders of magnitude below the body at 200. The body is part of
  # the picture, so the view is widened to the mean and a few standard
  # deviations when it contains none of it. Windows that already show the body
  # (every ordinary df) are unchanged.
  if (hi < chim - 4 * chisd) hi <- chim + 4 * chisd
  hiq <- vdist_tail_quantile(spec$qfun, 1e-9, lower.tail = FALSE)$q
  hi <- min(max(hi, perc + chisd, 1), max(hiq, 1))
  view <- c(0, hi)

  out <- vdist_cont_prep(spec, parts, prob, view, method)
  out$chim       <- chim
  out$chisd      <- chisd
  out$cut_text   <- cut_text
  out$prob_text  <- disp$text
  out$other_text <- vdist_pct_text(if (method == "lower") {
    pfun(perc, lower.tail = FALSE)
  } else {
    pfun(perc)
  })
  out$pp         <- perc
  out$pp_event   <- perc
  out
}

# ---------------------------------------------------------------------------
# F
# ---------------------------------------------------------------------------

vdist_f_moments <- function(num_df, den_df) {
  fm <- if (den_df > 2) den_df / (den_df - 2) else NA_real_
  fsd <- if (den_df > 4) {
    # Every factor is evaluated on its own: (num_df + den_df - 2) and
    # (den_df - 4) are both about den_df, so grouping them first gives an
    # ordinary ratio. The original num_df * (den_df - 4) overflowed for
    # den_df = 1e306 although the standard deviation is 0.1.
    sqrt((2 * (fm ^ 2) / num_df) * ((num_df + den_df - 2) / (den_df - 4)))
  } else {
    NA_real_
  }
  list(fm = fm, fsd = fsd)
}

fplot_data_prep <- function(num_df, den_df) {

  fmo <- vdist_f_moments(num_df, den_df)
  fm  <- fmo$fm
  fsd <- fmo$fsd
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
       ycap       = vdist_density_cap(spec$dfun, 0, 4,
                                      spec$pfun, spec$qfun))
}

fperc_data_prep <- function(probs, num_df, den_df, method) {

  fmo <- vdist_f_moments(num_df, den_df)
  fm  <- fmo$fm
  fsd <- fmo$fsd
  spec <- vdist_spec("f", num_df = num_df, den_df = den_df,
                     mean = fm, sd = fsd)
  vdist_check_f_concentration(num_df, den_df)

  if (method == "lower") {
    cut <- vdist_check_quantile(vdist_f_quantile(probs, num_df, den_df),
                                function(v) pf(v, num_df, den_df), probs,
                                "F quantile", "f")
    pfun_ev <- function(v, lower.tail = TRUE) {
      pf(v, num_df, den_df, lower.tail = lower.tail)
    }
    prob  <- pfun_ev(cut)
    parts <- list(c(-Inf, cut))
    cap_ev <- pfun_ev
  } else {
    cut <- vdist_check_quantile(vdist_f_quantile(probs, num_df, den_df, lower.tail = FALSE),
                                function(v) pf(v, num_df, den_df, lower.tail = FALSE),
                                probs, "F quantile", "f")
    pfun_ev <- function(v, lower.tail = TRUE) {
      pf(v, num_df, den_df, lower.tail = lower.tail)
    }
    prob  <- pfun_ev(cut, lower.tail = FALSE)
    parts <- list(c(cut, Inf))
    cap_ev <- function(v, lower.tail = TRUE) pfun_ev(v, lower.tail = FALSE)
  }

  if (!is.finite(prob) || abs(prob - probs) > 1e-6 * probs) {
    stop("probs cannot be resolved by the F quantile at these degrees of freedom; choose a less extreme probs value.",
         call. = FALSE)
  }

  cap <- vdist_caption(cut, prob, cap_ev, other = if (method == "lower") {
    pfun_ev(cut, lower.tail = FALSE)
  } else {
    pfun_ev(cut)
  }, otherfun = function(v) pfun_ev(v, lower.tail = method != "lower"))
  tail_hi <- vdist_tail_quantile(spec$qfun, 1e-1, lower.tail = FALSE)$q
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

  fmo <- vdist_f_moments(num_df, den_df)
  fm  <- fmo$fm
  fsd <- fmo$fsd
  spec <- vdist_spec("f", num_df = num_df, den_df = den_df,
                     mean = fm, sd = fsd)
  vdist_check_f_concentration(num_df, den_df)

  pfun <- function(v, lower.tail = TRUE) {
    pf(v, num_df, den_df, lower.tail = lower.tail)
  }
  if (method == "lower") {
    pfun_ev <- pfun
    prob  <- pfun_ev(perc)
    parts <- list(c(-Inf, perc))
  } else {
    pfun_ev <- function(v, lower.tail = TRUE) pfun(v, lower.tail = FALSE)
    prob  <- pfun_ev(perc)
    parts <- list(c(perc, Inf))
  }
  disp <- vdist_fmt_pct(prob)
  otherfun <- function(v) pfun(v, lower.tail = method != "lower")
  cut_text <- vdist_fmt_value(perc, Inf, pfun_ev, tol = disp$tol,
                              displayed = disp$value, display_text = disp$text, otherfun = otherfun,
                              other = vdist_fmt_pct(otherfun(perc)))

  tail_hi <- vdist_tail_quantile(spec$qfun, 1e-1, lower.tail = FALSE)$q
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
  out$other_text <- vdist_pct_text(if (method == "lower") {
    pfun(perc, lower.tail = FALSE)
  } else {
    pfun(perc)
  })
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
                                     mean - 4 * sd, mean + 4 * sd,
                                     function(v) pnorm(v, mean, sd),
                                     function(p) qnorm(p, mean, sd)),
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
    cap <- vdist_caption(cut, prob, pfun_ev, sd,
                         other = pfun_ev(cut, lower.tail = FALSE),
                         otherfun = function(v) pfun_ev(v, lower.tail = FALSE))
    alpha <- NULL
  } else if (method == "upper") {
    cut  <- vdist_check_quantile(qnorm(probs, mean, sd, lower.tail = FALSE),
                                 function(v) pfun_ev(v, lower.tail = FALSE),
                                 probs, "normal quantile", "normal")
    prob <- pfun_ev(cut, lower.tail = FALSE)
    parts <- list(c(cut, Inf))
    cap <- vdist_caption(cut, prob,
                         function(v, lower.tail = TRUE) pfun_ev(v, lower.tail = FALSE),
                         sd, other = pfun_ev(cut), otherfun = function(v) pfun_ev(v))
    alpha <- NULL
  } else {
    alpha <- (1 - probs) / 2
    c1 <- vdist_check_quantile(qnorm(alpha, mean, sd), pfun_ev, alpha,
                               "normal quantile", "normal")
    c2 <- vdist_check_quantile(qnorm(alpha, mean, sd, lower.tail = FALSE),
                               function(v) pfun_ev(v, lower.tail = FALSE),
                               alpha, "normal quantile", "normal")
    cut  <- c(c1, c2)
    prob <- vdist_prob_between(pfun_ev, spec$dfun, c1, c2)
    vdist_check_central(c1, c2, prob, probs, "probability")
    parts <- list(c(c1, c2))
    cap <- vdist_caption(cut, prob, pfun_ev, sd, dfun = spec$dfun)
  }

  # An extreme cutoff must not stretch the panel (the density would collapse
  # to a line), but the documented lowest probability (1e-10) has to fit
  # inside it: the 1e-13 quantile is beyond every representable percentile.
  k <- min(vdist_halfwidth(cut, mean, sd, 4), qnorm(1 - 1e-13))
  view <- c(mean - k * sd, mean + k * sd)

  out <- vdist_cont_prep(spec, parts, prob, view, method, cut_draw = cut)
  out$alpha     <- alpha
  out$pp1       <- pfun_ev(cut[1])
  out$pp2       <- if (length(cut) == 2L) pfun_ev(cut[2], lower.tail = FALSE) else NULL
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
    other <- if (method == "lower") pfun(perc, lower.tail = FALSE) else pfun(perc)
    other_disp <- vdist_fmt_pct(other)
    cut_text <- vdist_fmt_value(perc, sd, pfun_ev, tol = disp$tol,
                                displayed = disp$value, display_text = disp$text, otherfun = function(v) if (method == "lower") pfun(v, lower.tail = FALSE) else pfun(v), other = other_disp)
    pp1 <- NULL; pp2 <- NULL
  } else {
    prob <- vdist_prob_between(pfun, spec$dfun, perc[1], perc[2])
    parts <- list(c(perc[1], perc[2]))
    disp <- vdist_fmt_pct(prob)
    cut_text <- vdist_fmt_interval(perc[1], perc[2], sd, pfun, prob, spec$dfun)
    other <- NULL
    pp1 <- pfun(perc[1])
    pp2 <- pfun(perc[2], lower.tail = FALSE)
  }

  el <- min(max(abs(perc - mean)) / sd + 1, qnorm(1 - 1e-13))
  span <- max(4, el) * sd
  view <- c(mean - span, mean + span)

  out <- vdist_cont_prep(spec, parts, prob, view, method, cut_draw = perc)
  out$cut_text   <- cut_text
  out$prob_text  <- disp$text
  out$other_text <- if (exists("other", inherits = FALSE) && !is.null(other)) vdist_pct_text(other) else "0"
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
       ycap      = vdist_density_cap(spec$dfun, -4, 4,
                                     spec$pfun, spec$qfun),
       view      = c(-4, 4))
}

tperc_data_prep <- function(probs, df, method) {

  spec <- vdist_spec("t", df = df)
  pfun <- function(v, lower.tail = TRUE) pt(v, df, lower.tail = lower.tail)

  if (method == "lower") {
    cut <- vdist_check_quantile(qt(probs, df), pfun, probs, "t quantile", "t")
    prob <- pfun(cut)
    parts <- list(c(-Inf, cut))
    cap <- vdist_caption(cut, prob, pfun, other = pfun(cut, lower.tail = FALSE),
                         otherfun = function(v) pfun(v, lower.tail = FALSE))
    alpha <- NULL
  } else if (method == "upper") {
    cut <- vdist_check_quantile(qt(probs, df, lower.tail = FALSE),
                                function(v) pfun(v, lower.tail = FALSE), probs,
                                "t quantile", "t")
    prob <- pfun(cut, lower.tail = FALSE)
    parts <- list(c(cut, Inf))
    cap <- vdist_caption(cut, prob,
                         function(v, lower.tail = TRUE) pfun(v, lower.tail = FALSE),
                         other = pfun(cut), otherfun = function(v) pfun(v))
    alpha <- NULL
  } else {
    alpha <- (1 - probs) / 2
    c1 <- vdist_check_quantile(qt(alpha, df), pfun, alpha, "t quantile", "t")
    c2 <- vdist_check_quantile(qt(alpha, df, lower.tail = FALSE),
                               function(v) pfun(v, lower.tail = FALSE), alpha,
                               "t quantile", "t")
    cut <- c(c1, c2)
    prob <- vdist_prob_between(pfun, spec$dfun, c1, c2)
    parts <- list(c(c1, c2))
    cap <- vdist_caption(cut, prob, pfun)
  }

  # Keep the documented 1e-10 percentile inside the panel, but do not let an
  # arbitrarily large cutoff stretch it: the view stops at the 1e-11 quantile.
  bound <- max(5, max(abs(cut)) + 1)
  boundq <- vdist_tail_quantile(spec$qfun, 1e-11, lower.tail = FALSE)$q
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
  out$pp2       <- if (length(cut) == 2L) pfun(cut[2], lower.tail = FALSE) else NULL
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
    other <- if (method == "lower") pfun(perc, lower.tail = FALSE) else pfun(perc)
    cut_text <- vdist_fmt_value(perc, Inf, pfun_ev, tol = disp$tol,
                                displayed = disp$value, display_text = disp$text,
                                otherfun = function(v) pfun(v, lower.tail = method != "lower"),
                                other = vdist_fmt_pct(other))
    pp1 <- NULL; pp2 <- NULL
  } else if (method == "interval") {
    p <- abs(perc)
    prob <- vdist_prob_between(pfun, spec$dfun, -p, p)
    parts <- list(c(-p, p))
    disp <- vdist_fmt_pct(prob)
    cut_text <- vdist_fmt_interval(-p, p, Inf, pfun, prob, spec$dfun)[2]
    pp1 <- pfun(-p); pp2 <- pfun(p, lower.tail = FALSE)
  } else {
    p <- abs(perc)
    prob <- 2 * pfun(-p)
    other <- vdist_prob_between(pfun, spec$dfun, -p, p)
    parts <- list(c(-Inf, -p), c(p, Inf))
    disp <- vdist_fmt_pct(prob)
    cut_text <- vdist_fmt_interval(-p, p, Inf, pfun, other, spec$dfun, total_tail = TRUE)[2]
    pp1 <- pfun(-p); pp2 <- pfun(p, lower.tail = FALSE)
  }

  bound <- max(5, abs(perc) + 1)
  boundq <- vdist_tail_quantile(spec$qfun, 1e-11, lower.tail = FALSE)$q
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
  out$other_text <- if (exists("other", inherits = FALSE) && !is.null(other)) {
    vdist_pct_text(other)
  } else {
    "0"
  }
  out$pp1        <- pp1
  out$pp2        <- pp2
  out$pp         <- prob
  out$pp_event   <- prob
  out
}
