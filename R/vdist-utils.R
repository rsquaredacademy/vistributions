#' @importFrom utils menu install.packages packageVersion
#' @import ggplot2
check_suggests <- function(pkg) {

  pkg_flag <- tryCatch(packageVersion(pkg), error = function(e) NA)

  if (is.na(pkg_flag)) {

    msg_text <- paste0('\n', pkg, ' must be installed for this functionality.')

    if (interactive()) {
      message(msg_text, "\nWould you like to install it?")
      if (menu(c("Yes", "No")) == 1) {
        install.packages(pkg)
      } else {
        stop(msg_text, call. = FALSE)
      }
    } else {
      stop(msg_text, call. = FALSE)
    }
  }

}

check_numeric <- function(val, var = "n") {
  if (!is.numeric(val)) {
    stop(paste(var, "must be numeric."), call. = FALSE)
  }
}

check_logical <- function(val, var = "normal") {
  if (!is.logical(val) || length(val) != 1 || is.na(val)) {
    stop(paste(var, "must be a logical value."), call. = FALSE)
  }
}

check_positive <- function(val, var = "sd") {
  if (val <= 0) {
    stop(paste(var, "must be a positive value."), call. = FALSE)
  }
}

check_range <- function(val, min = 0, max = 1, var = "p") {
  if ((val < min) || (val > max)) {
    stop(paste(var, "must be between", min, "and", max, "only."), call. = FALSE)
  }
}

check_scalar <- function(val, var = "n") {
  if (length(val) != 1 || is.na(val) || !is.finite(val)) {
    stop(paste(var, "must be a single finite number."), call. = FALSE)
  }
}

# strictly inside (0, 1): the quantile is finite and can be plotted
check_open_range <- function(val, var = "probs") {
  if (val <= 0 || val >= 1) {
    stop(paste(var, "must be greater than 0 and less than 1."), call. = FALSE)
  }
}

# Documented probability range in which a distribution's quantiles are
# guaranteed to be representable and the plotted area is accurate. The tails
# are excluded because their quantiles either overflow or cannot be recovered
# from the returned double.
vdist_prob_range <- function(dist) {
  switch(dist,
         normal    = c(1e-10, 1 - 1e-10),
         t         = c(1e-10, 1 - 1e-10),
         chisquare = c(1e-6, 1 - 1e-6),
         f         = c(1e-6, 1 - 1e-6))
}

check_probability <- function(val, var = "probs", dist = "normal") {
  lim <- vdist_prob_range(dist)
  check_open_range(val, var)
  if (val < lim[1] || val > lim[2]) {
    stop(paste0(var, " must be between ", format(lim[1], scientific = TRUE),
                " and ", format(lim[2], scientific = FALSE, digits = 15),
                " for guaranteed numerical accuracy."), call. = FALSE)
  }
}

# A quantile is usable only if it is finite and its distribution function
# returns the requested probability. `pfun` must evaluate the same tail that
# produced `q` (pass a lower/upper-tail closure).
vdist_check_quantile <- function(q, pfun, target, what = "quantile", dist = "normal") {
  ok <- length(q) == 1 && is.finite(q)
  if (ok) {
    got <- pfun(q)
    tol <- 1e-3 * max(target, 1e-6)
    ok  <- is.finite(got) && abs(got - target) <= tol
  }
  if (!ok) {
    lim <- vdist_prob_range(dist)
    stop(paste0("The ", what, " for a probability of ",
                format(target, digits = 15),
                " cannot be represented numerically; use a less extreme ",
                "probability (the documented range is ",
                format(lim[1], scientific = TRUE), " to ",
                format(lim[2], scientific = FALSE), ")."),
         call. = FALSE)
  }
  q
}

# Reject a density that cannot be drawn: a non-finite ordinate would make an
# infinite polygon or an invisible vertical spike.
vdist_check_density <- function(y, what = "density") {
  if (any(!is.finite(y))) {
    stop(paste0("The ", what, " is not finite at every plotted point; ",
                "choose parameters inside the documented plotting contract."),
         call. = FALSE)
  }
  invisible(y)
}

# Convergence check: the shoelace area of a drawn polygon must match the
# probability between its own endpoints within 1e-3.
vdist_area <- function(x, y) {
  # translate first: a polygon far from zero (e.g. mean = 1, sd = 1e-13) would
  # otherwise lose the small area to cancellation in the absolute coordinates
  xr <- x - x[1]
  abs(sum(xr * c(y[-1], y[1]) - y * c(xr[-1], xr[1]))) / 2
}

vdist_check_area <- function(x, y, target, what = "shaded region") {
  area <- vdist_area(x, y)
  if (!is.finite(area) || abs(area - target) > 1e-3) {
    stop(paste0("The ", what, " area (", format(area, digits = 8),
                ") does not match the probability between its endpoints (",
                format(target, digits = 8),
                ") within 1e-3; the density is too concentrated for the ",
                "documented plotting contract."), call. = FALSE)
  }
  invisible(area)
}

# Keep drawn markers, reference lines and annotation labels inside the plotted
# view. Captions keep the true, unclipped event.
vdist_clamp <- function(x, l) {
  pmin(pmax(x, l[1]), l[length(l)])
}

# a single non-negative whole number (number of trials). Values within 1e-8 of
# a whole number (e.g. (.7 - .4) * 10) are accepted and returned rounded, so
# that every later step uses the count that was actually validated instead of
# silently truncating a different one.
check_count <- function(val, var = "n") {
  check_scalar(val, var)
  if (val < 0 || abs(val - round(val)) > 1e-8 || val > .Machine$integer.max) {
    stop(paste(var, "must be a single non-negative whole number."), call. = FALSE)
  }
  round(val)
}

# Grid of plotting points from `from` to `to`: steps of `step`, but never fewer
# than ~20 steps (very narrow regions would collapse to a triangle) and never
# more than `max_n` steps (very wide regions would allocate huge vectors).
vdist_seq <- function(from, to, step = 0.01, min_n = 20, max_n = 20000) {
  n <- (to - from) / step
  if (n > max_n) {
    seq(from, to, length.out = max_n + 1)
  } else if (n < min_n) {
    seq(from, to, length.out = 101)
  } else {
    seq(from, to, step)
  }
}

# Format a number for display text. Calculations always use full precision;
# this keeps 3 decimals for ordinary values and adds decimals when the value
# (or the distribution's scale) is small so the label is not rounded to 0.
# Values too small for 15 decimal places are printed in scientific notation
# with significant digits, because rounding them to a fixed number of decimals
# would print zero for a nonzero cutoff.
fmt_num <- function(x, scale = Inf) {
  out <- rep(NA_character_, length(x))
  keep <- is.finite(x)
  if (!all(keep)) {
    out[!keep] <- format(x[!keep], trim = TRUE)
  }
  if (!any(keep)) {
    return(out)
  }
  xk  <- x[keep]
  sk  <- scale[if (length(scale) == 1) rep(TRUE, length(x)) else keep]
  ref <- pmin(sk, abs(xk))
  ref[!is.finite(ref) | ref == 0] <- 1
  dec <- pmax(3, 3 - floor(log10(ref)))
  out[keep] <- vapply(seq_along(xk), function(i) {
    if (dec[i] > 15) {
      format(signif(xk[i], 4), scientific = TRUE, trim = TRUE)
    } else {
      format(round(xk[i], dec[i]), digits = 15, scientific = FALSE, trim = TRUE)
    }
  }, "")
  out
}

# Plotting grid for a continuous density over [from, to]. A uniform step of
# 0.01 is used unless the caller asks for probability-space sampling
# (`quantile = TRUE`, used where the density is unbounded at the support edge)
# or supplies a refined step (concentrated F densities). Uniform grids are
# capped at `max_n` intervals; probability-space grids put most points in the
# central mass instead of spreading them over a far, essentially empty tail.
# Requested endpoints are only kept when they fall inside that sampled range,
# so a polygon never jumps from an empty far tail to the body of the density.
vdist_grid <- function(from, to, pfun, qfun, n = 8000, quantile = FALSE,
                       step = 0.01, max_n = 20000) {
  if (from > to) {
    tmp  <- from
    from <- to
    to   <- tmp
  }
  if (!quantile) {
    return(vdist_seq(from, to, step = step, max_n = max_n))
  }

  eps <- 1e-6
  p_from <- pfun(from)
  p_to   <- pfun(to)
  plo <- min(max(p_from, eps), 1 - eps)
  phi <- min(max(p_to, eps), 1 - eps)

  if (!is.finite(plo) || !is.finite(phi) || phi <= plo) {
    return(c(from, to))
  }

  p <- stats::plogis(seq(stats::qlogis(plo), stats::qlogis(phi), length.out = n))
  x <- sort(unique(qfun(p)))

  # the endpoints are part of the drawn range only if they were sampled
  lower_clipped <- p_from < eps
  upper_clipped <- p_to > 1 - eps
  if (!lower_clipped) {
    x <- sort(unique(c(from, x)))
  }
  if (!upper_clipped) {
    x <- sort(unique(c(x, to)))
  }
  attr(x, "clipped") <- c(lower_clipped, upper_clipped)

  x
}

# Lower end of a plotting grid for a distribution on [0, Inf): the support
# itself, or the requested quantile where the density is unbounded at zero.
# No absolute epsilon floor is used: a quantile that underflows to zero is a
# documented parameter-limit error rather than a silent mass loss.
vdist_support_start <- function(df, q, what = "distribution", min_df = 0) {
  if (df < 2) {
    if (!is.finite(q) || q <= 0) {
      stop(paste0("The ", what, " support boundary cannot be represented for ",
                  "df = ", format(df, digits = 15),
                  "; df must be at least ", format(min_df, digits = 15),
                  "."), call. = FALSE)
    }
    q
  } else {
    0
  }
}

# F density concentration: the central 99.9998% mass must span at least 1e-3,
# so that the capped 20,001-point grid resolves the body of the density.
vdist_f_width <- function(num_df, den_df) {
  qf(1 - 1e-6, num_df, den_df) - qf(1e-6, num_df, den_df)
}

vdist_check_f_concentration <- function(num_df, den_df) {
  width <- vdist_f_width(num_df, den_df)
  if (!is.finite(width) || width < 1e-3) {
    stop(paste0("The F density is too concentrated to plot accurately: its ",
                "central 99.9998% probability mass spans ",
                format(width, digits = 6),
                ", less than the documented minimum of 0.001. ",
                "Use smaller degrees of freedom."), call. = FALSE)
  }
  width
}

# F grid: refine the uniform step to the width of the central mass. Ordinary F
# densities keep the 0.01 step; very concentrated ones are sampled finely and
# capped at 20,001 points.
vdist_f_grid <- function(lo, hi, num_df, den_df, quantile = FALSE,
                         step = NULL) {
  width <- vdist_check_f_concentration(num_df, den_df)
  if (is.null(step)) {
    step <- min(0.01, width / 50)
  }
  vdist_grid(lo, hi,
             function(x) pf(x, num_df, den_df),
             function(p) qf(p, num_df, den_df),
             quantile = quantile,
             step = step)
}

# Keep enough of both t tails that the omitted mass is below 1e-4: the
# density is heavy-tailed, so a fixed +/-5 window loses more than 1e-3 of the
# probability for small df.
vdist_t_bound <- function(bound, df, tail = 1e-4) {
  max(bound, abs(qt(tail, df)), abs(qt(tail, df, lower.tail = FALSE)))
}

# Display a distribution moment: ordinary values keep the historical three
# decimals, while a genuinely small moment (for example sd = 2e-4 for a
# concentrated F overlay) keeps enough digits not to print as zero.
fmt_moment <- function(x) {
  if (length(x) != 1 || is.na(x)) {
    return("NA")
  }
  if (abs(x) >= 0.01) {
    format(round(x, 3), digits = 15, trim = TRUE)
  } else {
    fmt_num(x)
  }
}

# Format a cutoff so that the event in the caption is numerically true when
# the printed text is parsed again. The value is printed with more digits
# when the distribution's scale or the magnitude of the mean requires it; if
# no printable value reproduces the requested probability, that is a
# documented numerical-resolution error (the coordinate itself is unusable).
vdist_fmt_cutoff <- function(x, scale, pfun, target) {
  tol <- 1e-4 * max(target, 1e-4)
  ok <- function(s) {
    v <- suppressWarnings(as.numeric(s))
    is.finite(v) && abs(pfun(v) - target) <= tol
  }

  current <- fmt_num(x, scale)
  if (ok(current)) {
    return(current)
  }

  for (d in 3:17) {
    s <- sprintf(paste0("%.", d, "f"), x)
    if (ok(s)) {
      return(s)
    }
  }
  for (d in 4:17) {
    s <- format(x, scientific = TRUE, digits = d, trim = TRUE)
    if (ok(s)) {
      return(s)
    }
  }

  stop(paste0("The cutoff ", format(x, digits = 17),
              " cannot be printed with enough precision to state the ",
              "requested probability; the coordinate precision is ",
              "insufficient. Use a larger scale or a less extreme probability."),
       call. = FALSE)
}
