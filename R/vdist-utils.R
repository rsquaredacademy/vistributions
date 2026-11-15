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
  ref <- pmin(scale, abs(x))
  ref[!is.finite(ref) | ref == 0] <- 1
  dec <- pmax(3, 3 - floor(log10(ref)))
  vapply(seq_along(x), function(i) {
    if (dec[i] > 15) {
      format(signif(x[i], 4), scientific = TRUE, trim = TRUE)
    } else {
      format(round(x[i], dec[i]), digits = 15, scientific = FALSE, trim = TRUE)
    }
  }, "")
}

# Plotting grid for a continuous density over [from, to]. A uniform step of
# 0.01 is used when it is affordable: it resolves the density well and keeps
# ordinary plots unchanged. Wide ranges - or `quantile = TRUE`, used where the
# density is unbounded at the edge of the support - are instead sampled at
# points equally spaced in log-odds probability, which puts most points in the
# central mass instead of spreading them over a far, essentially empty tail.
# Requested endpoints are only kept when they fall inside that sampled range,
# so a polygon never jumps from an empty far tail to the body of the density.
vdist_grid <- function(from, to, pfun, qfun, n = 2000, quantile = FALSE) {
  if (from > to) {
    tmp  <- from
    from <- to
    to   <- tmp
  }
  if (!quantile && (to - from) / 0.01 <= 20000) {
    return(vdist_seq(from, to))
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
# itself, or a small positive quantile where the density is unbounded at zero.
# `q` is the 1e-6 quantile of the distribution.
vdist_support_start <- function(df, q) {
  if (df < 2) {
    max(q, .Machine$double.eps)
  } else {
    0
  }
}
