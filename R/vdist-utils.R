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
#
# At very large and very unequal degrees of freedom the stats q*() routines
# return a value that does not invert their own p*() (for example
# qf(.95, 1e8, 1e8) has pf about .8776). The p*() function is the accurate
# one, so refine a failed quantile by root finding before rejecting it.
vdist_check_quantile <- function(q, pfun, target, what = "quantile", dist = "normal") {
  ok <- length(q) == 1 && is.finite(q)
  if (ok) {
    got <- pfun(q)
    tol <- 1e-3 * max(target, 1e-6)
    ok  <- is.finite(got) && abs(got - target) <= tol
    if (!ok) {
      refined <- vdist_refine_quantile(q, pfun, target)
      if (!is.null(refined)) {
        q  <- refined
        ok <- TRUE
      }
    }
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

# Bracket and solve pfun(v) = target around a finite starting quantile. The
# distribution function is monotone, so a geometric widening of the bracket
# finds a sign change whenever one is representable. Returns NULL when no root
# can be found (the caller then reports the documented range error).
vdist_refine_quantile <- function(q, pfun, target) {
  if (length(q) != 1 || !is.finite(q)) {
    return(NULL)
  }
  f  <- function(v) pfun(v) - target
  step <- max(1, abs(q))
  lo <- q - step
  hi <- q + step
  flo <- f(lo)
  fhi <- f(hi)
  k <- 0L
  while (is.finite(flo) && is.finite(fhi) && sign(flo) == sign(fhi) && k < 80L) {
    step <- step * 2
    lo <- q - step
    hi <- q + step
    if (!is.finite(lo) || !is.finite(hi)) {
      return(NULL)
    }
    flo <- f(lo)
    fhi <- f(hi)
    k <- k + 1L
  }
  if (!is.finite(flo) || !is.finite(fhi) || sign(flo) == sign(fhi)) {
    return(NULL)
  }
  r <- tryCatch(stats::uniroot(f, c(lo, hi), tol = .Machine$double.eps^0.9),
                error = function(e) NULL)
  if (is.null(r) || !is.finite(r$root)) {
    return(NULL)
  }
  tol <- 1e-3 * max(target, 1e-6)
  if (abs(f(r$root)) > tol) {
    return(NULL)
  }
  r$root
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

# Convergence check: the shoelace area of a drawn polygon. The local-edge
# form uses only coordinate differences, so it stays accurate for polygons far
# from zero (e.g. mean = 1, sd = 1e-13) and for very wide t tails, where
# multiplying absolute coordinates or subtracting a distant origin loses the
# central differences.
vdist_area <- function(x, y) {
  n <- length(x)
  if (n < 3L) {
    return(0)
  }
  abs(sum((c(x[-1], x[1]) - x) * (y + c(y[-1], y[1])))) / 2
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

# Check the event the caption names, not just the polygon's own endpoints: the
# drawn area plus the part of the event that lies outside the visible window
# must equal the stated event probability. `omitted` is computed with the
# distribution's p*() function by the data preparation step.
vdist_check_event <- function(area, omitted, target, what = "shaded region") {
  total <- area + omitted
  if (!is.finite(total) || abs(total - target) > 1e-3) {
    stop(paste0("The ", what, " area (", format(area, digits = 8),
                ") plus the ", format(omitted, digits = 8),
                " probability outside the visible range does not match the ",
                "stated event probability (", format(target, digits = 8),
                ") within 1e-3; choose parameters inside the documented ",
                "plotting contract."), call. = FALSE)
  }
  invisible(total)
}

# A translated normal is only representable when the standard deviation is
# large enough, relative to the location, that the cutoff is more than a few
# double-precision units away from the mean. The comparison is a ratio, so it
# is unchanged by a change of measurement units (mean and sd scaled together);
# `mean = 1e-16, sd = 1e-16` is an ordinary, resolvable distribution.
# Zero-centred normals have no translation and are always evaluated on the
# standardized scale.
vdist_check_normal_resolution <- function(mean, sd) {
  if (abs(mean) > 0) {
    ratio <- sd / abs(mean)
    if (!is.finite(ratio) || ratio < 1e-14) {
      stop(paste0("The normal cutoff cannot be represented numerically: ",
                  "sd / |mean| = ",
                  format(ratio, scientific = TRUE, digits = 3),
                  " is below the documented limit of 1e-14. Use a larger sd ",
                  "relative to the mean (scaling mean and sd together is ",
                  "always allowed)."), call. = FALSE)
    }
  }
  invisible(TRUE)
}

# A normal curve is drawable only when its peak density and its plotting range
# are finite. These are limits of double precision, not a modelling choice, and
# both are stated in the help page.
vdist_check_normal_range <- function(mean, sd) {
  if (!is.finite(dnorm(0, 0, sd))) {
    stop(paste0("The normal density is not finite for sd = ",
                format(sd, digits = 15),
                "; sd must be at least about 1e-308."), call. = FALSE)
  }
  if (!is.finite(mean - 4 * sd) || !is.finite(mean + 4 * sd)) {
    stop(paste0("The normal plotting range (mean +/- 4 sd) is not finite for ",
                "mean = ", format(mean, digits = 15), " and sd = ",
                format(sd, digits = 15),
                "; mean and sd must be small enough that the range is ",
                "representable."), call. = FALSE)
  }
  invisible(TRUE)
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

# Fixed number of plotting points. The grid never uses a step in data units:
# a 0.01 step collapses when the distribution is narrow or far from zero, and
# wastes almost every point on a long, empty tail.
vdist_n_points <- 1001L

# A density that is singular at the support boundary needs more probability
# points close to that boundary; the fixed count keeps allocation bounded.
vdist_grid_n <- function(singular = FALSE) {
  if (isTRUE(singular)) 10001L else vdist_n_points
}

# Very small t degrees of freedom put almost all probability in a tail that
# reaches millions or further; the extra points keep the central mass resolved
# without slowing the ordinary examples.
vdist_grid_n_t <- function(df) {
  if (df < 1) 4001L else vdist_n_points
}

vdist_seq <- function(from, to, n = vdist_n_points) {
  seq(from, to, length.out = n)
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
    if (abs(xk[i]) >= 1e15) {
      # a very large cutoff keeps only significant digits; printing it as a
      # fixed number would emit meaningless integer digits
      format(signif(xk[i], 6), scientific = TRUE, trim = TRUE)
    } else if (dec[i] > 17) {
      format(signif(xk[i], 4), scientific = TRUE, trim = TRUE)
    } else if (dec[i] > 13) {
      # a translated value (for example mean = 1, sd = 1e-13) needs digits
      # to the right of the leading 1; print them fixed instead of letting
      # `format()` round the whole value to 15 significant digits
      sprintf(paste0("%.", dec[i], "f"), xk[i])
    } else {
      format(round(xk[i], dec[i]), digits = 15, scientific = FALSE, trim = TRUE)
    }
  }, "")
  out
}

# Plotting grid for a continuous density over [from, to]. A fixed number of
# points is used across the visible range. `quantile = TRUE` spaces those
# points in probability, which puts them where the mass is and makes the grid
# independent of the distribution's location and scale; it is also the only
# way to resolve a density that is singular at the support boundary. Both
# requested endpoints are always kept, so a polygon closes on the event it
# claims to shade.
vdist_grid <- function(from, to, pfun, qfun, n = vdist_n_points,
                       quantile = TRUE, uniform = TRUE) {
  if (from > to) {
    tmp  <- from
    from <- to
    to   <- tmp
  }
  if (!is.finite(from) || !is.finite(to)) {
    stop("The plotting range is not finite; choose parameters inside the ",
         "documented plotting contract.", call. = FALSE)
  }
  if (!quantile) {
    return(vdist_seq(from, to, n))
  }

  p_from <- pfun(from)
  p_to   <- pfun(to)

  if (!is.finite(p_from) || !is.finite(p_to) || p_to <= p_from) {
    return(sort(unique(c(from, to))))
  }

  eps <- 1e-12
  plo <- min(max(p_from, eps), 1 - eps)
  phi <- min(max(p_to, eps), 1 - eps)

  if (phi <= plo) {
    return(sort(unique(c(from, to))))
  }

  p <- stats::plogis(seq(stats::qlogis(plo), stats::qlogis(phi),
                         length.out = n))
  # Refine any quantile that does not invert the probability that produced it.
  # At very large degrees of freedom the stats q*() routines lose accuracy over
  # the whole grid (not only at the requested cutoff), which would distort the
  # drawn polygon; the p*() function is accurate, so solve it directly there.
  xq <- qfun(p)
  okq <- is.finite(xq) & is.finite(pfun(xq)) &
    abs(pfun(xq) - p) <= 1e-6 * pmax(p, 1e-6)
  if (any(!okq)) {
    for (i in which(!okq)) {
      xr <- vdist_refine_quantile(xq[i], pfun, p[i])
      if (!is.null(xr)) {
        xq[i] <- xr
      }
    }
  }
  # Union with a fixed-count uniform grid: the probability grid resolves the
  # body and a singular support edge, while the uniform grid closes the far
  # tail where the CDF has saturated and a single segment to the endpoint
  # would otherwise create a spurious triangle.
  x <- c(from, xq, if (uniform) seq(from, to, length.out = n), to)
  x <- x[is.finite(x)]
  sort(unique(x))
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
  if (!is.finite(width)) {
    stop(paste0("The central 99.9998% probability mass of the F distribution ",
                "does not span a finite interval for num_df = ",
                format(num_df, digits = 15), " and den_df = ",
                format(den_df, digits = 15),
                "; the tail does not converge at double precision. Use larger ",
                "degrees of freedom."), call. = FALSE)
  }
  if (width < 1e-3) {
    stop(paste0("The F density is too concentrated to plot accurately: its ",
                "central 99.9998% probability mass spans ",
                format(width, digits = 6),
                ", less than the documented minimum of 0.001. ",
                "Use smaller degrees of freedom."), call. = FALSE)
  }
  width
}

# F grid: probability-space sampling with a fixed point count resolves both
# ordinary densities and the concentrated ones whose central mass is narrow.
# The width limit is still checked, so a density that no finite grid can draw
# is rejected with its documented error.
vdist_f_grid <- function(lo, hi, num_df, den_df, quantile = TRUE,
                         step = NULL, n = NULL, uniform = TRUE) {
  vdist_check_f_concentration(num_df, den_df)
  if (is.null(n)) {
    n <- vdist_grid_n(num_df < 2 || den_df < 1)
  }
  vdist_grid(lo, hi,
             function(x) pf(x, num_df, den_df),
             function(p) qf(p, num_df, den_df),
             quantile = quantile,
             n = n,
             uniform = uniform)
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

# Format a probability for a caption together with the interval its printed
# digits guarantee. Percentages at or above 0.1 keep one decimal; smaller ones
# use three significant digits in scientific notation. The caption is true
# only when the probability of the printed cutoff falls inside this interval.
vdist_fmt_pct <- function(p) {
  if (length(p) != 1L || !is.finite(p)) {
    return(list(text = "NA", value = NA_real_, tol = Inf))
  }
  v <- p * 100
  if (v == 0) {
    return(list(text = "0", value = 0, tol = 0.005))
  }
  if (v >= 0.1) {
    r <- round(v, 1)
    s <- format(r, trim = TRUE, scientific = FALSE)
    dec <- if (grepl("\\.", s)) {
      nchar(sub("^[^.]*\\.", "", s))
    } else {
      0
    }
    list(text = s, value = r / 100, tol = 0.5 * 10 ^ (-dec) / 100)
  } else {
    s <- format(v, scientific = TRUE, digits = 3, trim = TRUE)
    mant <- sub("e.*$", "", s)
    dec <- if (grepl("\\.", mant)) nchar(sub("^[^.]*\\.", "", mant)) else 0
    ex <- suppressWarnings(as.numeric(sub("^.*e", "", s)))
    if (!is.finite(ex)) ex <- 0
    list(text = s, value = as.numeric(s) / 100,
         tol = 0.5 * 10 ^ (-dec) * 10 ^ ex / 100)
  }
}

vdist_pct_text <- function(p) {
  vdist_fmt_pct(p)$text
}

# Find a printable cutoff whose distribution function rounds to the displayed
# probability. The check is done at the precision the caption actually shows,
# not at an unrelated fixed tolerance.
vdist_fmt_cutoff <- function(x, scale, pfun, target) {
  disp <- vdist_fmt_pct(target)
  ok <- function(s) {
    v <- suppressWarnings(as.numeric(s))
    is.finite(v) && is.finite(pfun(v)) && abs(pfun(v) - disp$value) <= disp$tol
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

# Find a printable value whose distribution function differs from the value
# at the original coordinate by at most `tol`. Used for probability captions,
# where the printed cutoff defines the event and must therefore keep the
# probability that is printed beside it.
vdist_fmt_value <- function(x, scale, pfun, tol = 1e-6) {  target <- pfun(x)
  ok <- function(s) {
    v <- suppressWarnings(as.numeric(s))
    is.finite(v) && is.finite(pfun(v)) && abs(pfun(v) - target) <= tol
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
              " cannot be printed with enough precision to state its ",
              "probability; the coordinate precision is insufficient. Use a ",
              "larger scale or a less extreme probability."), call. = FALSE)
}

# Format both ends of a central interval together. Each endpoint formatted on
# its own can reproduce its own tail probability and still produce a pair whose
# interval probability rounds to a different displayed value; the pair must be
# checked as the event the caption names.
vdist_fmt_interval <- function(x1, x2, scale, pfun, target) {
  disp <- vdist_fmt_pct(target)
  ok <- function(a, b) {
    a <- suppressWarnings(as.numeric(a))
    b <- suppressWarnings(as.numeric(b))
    is.finite(a) && is.finite(b) && is.finite(pfun(b)) && is.finite(pfun(a)) &&
      abs((pfun(b) - pfun(a)) - disp$value) <= disp$tol
  }

  s1 <- fmt_num(x1, scale)
  s2 <- fmt_num(x2, scale)
  if (ok(s1, s2)) {
    return(c(s1, s2))
  }
  for (d in 3:17) {
    a <- sprintf(paste0("%.", d, "f"), x1)
    b <- sprintf(paste0("%.", d, "f"), x2)
    if (ok(a, b)) {
      return(c(a, b))
    }
  }
  for (d in 4:17) {
    a <- format(x1, scientific = TRUE, digits = d, trim = TRUE)
    b <- format(x2, scientific = TRUE, digits = d, trim = TRUE)
    if (ok(a, b)) {
      return(c(a, b))
    }
  }
  # No printable pair reproduces the requested displayed probability. Print
  # both ends at full precision; the caption's percentage is then taken from
  # the event these printed ends actually define, so the caption stays true.
  c(format(x1, digits = 17, scientific = TRUE, trim = TRUE),
    format(x2, digits = 17, scientific = TRUE, trim = TRUE))
}

# The upper limit of the density axis. A density that is unbounded at a support
# edge (chi-square and F with a small shape parameter) has a peak orders of
# magnitude above its body; drawing the true peak compresses the body and the
# shaded region into the baseline. Capping the axis at a high quantile of the
# plotted density keeps the body, and therefore the shading, visible. The
# source-coordinate area checks always use the unclipped density.
vdist_y_cap <- function(y) {
  y <- y[is.finite(y) & y >= 0]
  if (!length(y)) {
    return(1)
  }
  mx <- max(y)
  q  <- as.numeric(stats::quantile(y, 0.98, names = FALSE))
  if (is.finite(q) && q > 0 && mx > 20 * q) q else mx
}

# Axis limits for the density panel: headroom for a clipped peak and for the
# percentage labels above the curve.
vdist_ylim <- function(y, headroom = 1.3) {
  cap <- vdist_y_cap(y)
  c(0, cap * headroom)
}

# Display cap for the density axis, sampled uniformly in probability space so
# that the many nearly-empty points of a tail-resolving grid cannot decide it.
# A density that is unbounded at a support edge is capped at a value its body
# reaches, which keeps the body and the shaded region visible; ordinary
# densities are not clipped. Source-coordinate area checks always use the
# unclipped density.
vdist_density_cap <- function(dfun, pfun, qfun, lo, hi) {
  plo <- pfun(lo)
  phi <- pfun(hi)
  if (!is.finite(plo) || !is.finite(phi) || phi <= plo) {
    p <- seq(1e-3, 1 - 1e-3, length.out = 501)
  } else {
    p <- seq(max(plo, 0), min(phi, 1), length.out = 501)
  }
  y <- dfun(qfun(p))
  y <- y[is.finite(y) & y >= 0]
  if (!length(y)) {
    return(1)
  }
  mx  <- max(y)
  q90 <- as.numeric(stats::quantile(y, 0.9, names = FALSE))
  ref <- q90
  if (is.finite(ref) && ref > 0 && mx > 20 * ref) ref else mx
}

vdist_ylim_cap <- function(cap, headroom = 1.3) {
  c(0, cap * headroom)
}

# A very wide window (heavy t and F tails, singular support edges) cannot be
# read on a linear axis: the shaded event, or the whole body of the density,
# collapses into a line. Such windows are drawn on a symmetric log scale, which
# is still the same data (the area checks and captions are unchanged) but keeps
# every part of the window visible. Ordinary windows keep the linear axis.
vdist_wide_window <- function(x, limit = 1e6) {
  x <- x[is.finite(x)]
  if (length(x) < 2) {
    return(FALSE)
  }
  span <- max(x) - min(x)
  is.finite(span) && span > limit
}

# x scale for a continuous density plot. `breaks` is a numeric vector for the
# ordinary linear axis; the symmetric log axis uses its own breaks.
vdist_x_scale <- function(x, breaks = NULL, wide = NULL) {
  if (is.null(wide)) {
    wide <- vdist_wide_window(x)
  }
  if (wide) {
    return(ggplot2::scale_x_continuous(
      trans  = scales::pseudo_log_trans(),
      breaks = ggplot2::waiver(),
      labels = function(v) {
        out <- ifelse(v == 0, "0",
                      format(signif(v, 2), scientific = TRUE, trim = TRUE))
        out
      }
    ))
  }
  if (is.null(breaks)) {
    ggplot2::scale_x_continuous(breaks = ggplot2::waiver())
  } else {
    ggplot2::scale_x_continuous(breaks = breaks)
  }
}

# Caption that discloses the part of the named event outside the plotted range.
# The area check adds this probability to the drawn polygon, so it must be
# stated whenever it is not negligible.
vdist_beyond_text <- function(omitted) {
  if (!is.finite(omitted) || omitted <= 1e-7) {
    return(NULL)
  }
  paste0(vdist_pct_text(omitted),
         "% of the event lies outside the plotted range and is added by the area check")
}
