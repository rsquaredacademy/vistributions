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
  tab <- vdist_contract(dist)
  c(tab$prob_lo[1], tab$prob_hi[1])
}

# Parameter floors, defined once and used by every export of the family.
vdist_check_chisquare_df <- function(df) {
  if (df < 0.1) {
    stop("df must be at least 0.1 for guaranteed numerical accuracy.", call. = FALSE)
  }
  invisible(df)
}

vdist_check_f_num_df <- function(num_df) {
  if (num_df < 1) {
    stop("num_df must be at least 1 for guaranteed numerical accuracy.", call. = FALSE)
  }
  invisible(num_df)
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

# ---------------------------------------------------------------------------
# Input contract
#
# Every exported plotting function validates against the table below; it is the
# single definition used by the validators, by the help-page tests and by the
# report. The limits are the probability range in which a quantile is both
# representable and invertible at double precision, and the parameter ranges in
# which the density can be drawn. Ordinary teaching inputs (the function
# defaults, normal(100, 15), t with 5 df, chi-square with 1 or 2 df and an upper
# tail, F(5, 10), a typical binomial) are well inside every limit.
# ---------------------------------------------------------------------------
vdist_contract <- function(dist = NULL) {
  tab <- data.frame(
    dist          = c("normal", "t", "chisquare", "f", "binomial"),
    prob_lo       = c(1e-10, 1e-10, 1e-6, 1e-6, NA_real_),
    prob_hi       = c(1 - 1e-10, 1 - 1e-10, 1 - 1e-6, 1 - 1e-6, NA_real_),
    parameter     = c("sd / |mean| >= 1e-14 (mean != 0)",
                      "df > 0",
                      "df >= 0.1",
                      "num_df >= 1, den_df > 0",
                      "n a whole number in [0, 2^31 - 1], 0 <= p <= 1"),
    stringsAsFactors = FALSE
  )
  if (is.null(dist)) {
    return(tab)
  }
  tab[tab$dist == dist, , drop = FALSE]
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
  # local-edge trapezoid sum over consecutive vertices
  a <- sum(diff(x) * (y[-n] + y[-1L])) / 2
  # closing edge, unless both ends lie on the axis: then its height is zero and
  # computing the edge explicitly would overflow for a region wider than the
  # double range (for example sd = 3e307), turning 0 * Inf into NaN.
  if ((y[1] + y[n]) != 0) {
    a <- a + (x[1] - x[n]) * (y[1] + y[n]) / 2
  }
  abs(a)
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

# Check the polygon the plot actually draws: its own area must equal the exact
# probability of the event it represents, computed from the distribution
# function (never from the polygon). No "off-view" amount is added here: the
# shading polygon spans the whole event (the panel only clips the drawing), so
# the area is a direct, independent check of the sampling grid.
vdist_check_region <- function(area, target, what = "shaded region") {
  if (!is.finite(area) || abs(area - target) > 1e-3) {
    stop(paste0("The ", what, " area (", format(area, digits = 8),
                ") does not match the probability of its event (",
                format(target, digits = 8),
                ") within 1e-3; the density is too concentrated for the ",
                "documented plotting contract."), call. = FALSE)
  }
  invisible(area)
}

# ---------------------------------------------------------------------------
# Shading polygons
#
# A shaded region is built from three grids unioned together, so every scale of
# the region is resolved by construction:
#
#   1. points equally spaced in probability (logit-spaced: geometric in x in
#      both tails, which resolves a support edge where the density is
#      unbounded and a far tail that spans many orders of magnitude);
#   2. points equally spaced in standardised x (the body of the density).
#
# The polygon is then closed on the x axis at both ends. Its own shoelace area
# is compared with the exact probability p(hi) - p(lo) and the grid is refined
# (by doubling) until the area is within `tol`; the caller reports the
# documented error when the budget is exhausted. No step in data units is used
# and nothing is derived from the drawn grid.
#
# A density that is unbounded at a support edge cannot be evaluated at the
# edge. The edge vertex is then given the height that makes the first (last)
# trapezoid carry the exact CDF mass of that sliver, so the singular part of
# the region is included exactly instead of being dropped or drawn as a spike.
# ---------------------------------------------------------------------------


# log-spaced x values across [lo, hi], aware of the sign of the interval

vdist_region_grid <- function(lo, hi, pfun, qfun, n) {
  plo <- min(max(pfun(lo), 1e-12), 1 - 1e-12)
  phi <- min(max(pfun(hi), 1e-12), 1 - 1e-12)
  # probability-spaced points: geometric in x near a support edge and in a
  # heavy tail, so the singular part of the region is resolved
  xp  <- if (phi > plo) {
    qfun(stats::plogis(seq(stats::qlogis(plo), stats::qlogis(phi),
                           length.out = n)))
  } else {
    numeric(0)
  }
  # uniform points across the region (in standardised units, see vdist_region)
  xu  <- seq(lo, hi, length.out = n)
  x  <- sort(unique(c(lo, xp, xu, hi)))
  x  <- x[is.finite(x) & x >= lo & x <= hi]
  x
}


# Polygon of the region [lo, hi] under the density. Returns the closed polygon
# (both ends on the x axis), its area and the number of points used.
#
# The initial grid (probability + uniform + log, see above) is refined where
# the trapezoid rule is actually wrong: the local error of every interval is
# compared with the exact CDF mass p(x[i+1]) - p(x[i]), and the worst intervals
# are bisected until the polygon's own area is within `tol` of the exact
# p(hi) - p(lo). The refinement is therefore driven by the region's own scale
# and its exact probability, not by a step in data units.
vdist_region <- function(lo, hi, dfun, pfun, qfun,
                         n = 65L, tol = 1e-5, max_pts = 20001L,
                         max_pass = 200L, frac = 0.03,
                         center = 0, scale = 1) {
  if (!is.finite(lo) || !is.finite(hi)) {
    stop("The shading region is not finite; choose parameters inside the ",
         "documented plotting contract.", call. = FALSE)
  }
  if (hi <= lo) {
    return(list(x = c(lo, hi), y = c(0, 0), area = 0, n = 0L))
  }
  # Work in standardised units z = (x - center) / scale. The same region and
  # the same density under any change of measurement units therefore give the
  # same grid, the same refinement decisions and (mapped back) the same
  # standardised polygon.
  ctr <- if (is.finite(center)) center else 0
  scl <- if (is.finite(scale) && scale > 0) scale else 1
  zlo <- (lo - ctr) / scl
  zhi <- (hi - ctr) / scl
  if (!is.finite(zlo) || !is.finite(zhi) || zhi <= zlo) {
    return(list(x = c(lo, hi), y = c(0, 0), area = 0, n = 0L))
  }
  pfun_z <- function(z) pfun(ctr + scl * z)
  dfun_z <- function(z) scl * dfun(ctr + scl * z)
  qfun_z <- function(p) (qfun(p) - ctr) / scl

  p_lo   <- pfun_z(zlo)
  p_hi   <- pfun_z(zhi)
  target <- p_hi - p_lo
  x <- vdist_region_grid(zlo, zhi, pfun_z, qfun_z, n)
  if (length(x) < 2) {
    return(list(x = c(lo, hi), y = c(0, 0), area = 0, n = 0L))
  }
  fix <- function(x, y) {
    if (length(x) >= 2 && !is.finite(y[1])) {
      y[1] <- 2 * (pfun_z(x[2]) - p_lo) / (x[2] - zlo) - y[2]
    }
    if (length(x) >= 2 && !is.finite(y[length(y)])) {
      y[length(y)] <- 2 * (p_hi - pfun_z(x[length(x) - 1])) /
        (zhi - x[length(x) - 1]) - y[length(y) - 1]
    }
    y[!is.finite(y)] <- 0
    y[y < 0] <- 0
    y
  }
  y <- fix(x, dfun_z(x))
  poly_area <- function() vdist_area(c(zlo, x, zhi), c(0, y, 0))
  a <- poly_area()
  # Decisions are taken on values quantised to 12 significant digits: two
  # representations of the same region that differ only by a floating-point
  # rounding step then make exactly the same refinement decisions, so the
  # standardised polygon is the same picture in every unit system.
  qz <- function(v) signif(v, 12)
  pass <- 0L
  while (qz(abs(a - target)) > tol && length(x) < max_pts && pass < max_pass) {
    pass <- pass + 1L
    err <- ((y[-1] + y[-length(y)]) / 2) * diff(x) -
      (pfun_z(x[-1]) - pfun_z(x[-length(x)]))
    ae  <- abs(qz(err))
    k   <- max(4L, ceiling(frac * length(x)))
    thr <- max(tol * 1e-2, as.numeric(stats::quantile(
      ae, max(0, 1 - k / length(x)), names = FALSE, type = 1)))
    idx <- which(ae >= thr & ae > 0)
    if (!length(idx)) {
      idx <- which.max(ae)
    }
    mid <- (x[idx] + x[idx + 1]) / 2
    mid <- mid[is.finite(mid) & mid > x[idx] & mid < x[idx + 1]]
    if (!length(mid)) {
      break
    }
    x <- sort(c(x, mid))
    y <- fix(x, dfun_z(x))
    a <- poly_area()
  }
  zx <- c(zlo, x, zhi)
  list(x  = ctr + scl * zx,
       y  = c(0, y, 0) / scl,
       area = a,
       n  = length(zx))
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
    # Compare on the log scale. The ratio itself overflows to Inf for a
    # near-zero mean (mean = 1e-308, sd = 10) and underflows to 0 for a mean
    # close to the largest double; only a genuinely tiny ratio is rejected.
    # log(sd) - log(|mean|) is the same comparison without either overflow.
    lratio <- log(sd) - log(abs(mean))
    if (is.finite(lratio) && lratio < log(1e-14)) {
      shown <- if (is.finite(ratio)) {
        format(ratio, scientific = TRUE, digits = 3)
      } else {
        "0 (underflow)"
      }
      stop(paste0("The normal cutoff cannot be represented numerically: ",
                  "sd / |mean| = ", shown,
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
  # The window itself must fit in a double, not only its ends: the drawing
  # rescales coordinates by (max - min), and that difference overflows for
  # sd above about 2.2e307, which would emit an empty panel.
  if (!is.finite(8 * sd)) {
    stop(paste0("The normal plotting range (mean +/- 4 sd) spans more than the ",
                "double range for sd = ", format(sd, digits = 15),
                "; sd must be below about 2.2e307 so the plotted window is ",
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
  # a refined quantile can land a rounding step outside the requested view (or
  # just below a support boundary at 0); the curve is only drawn inside the view
  x <- x[is.finite(x) & x >= from & x <= to]
  sort(unique(x))
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
# use three significant digits in scientific notation.
#
# The displayed precision is increased whenever the exact value does not fall
# inside the rounding interval of the first candidate (a value exactly on a
# rounding boundary, such as 5.05%, cannot be stated truthfully with one
# decimal). The rule is a function of the probability alone, so it is unchanged
# by a change of measurement units and it guarantees that every printed
# percentage is true at the precision it shows.
vdist_fmt_pct <- function(p) {
  if (length(p) != 1L || !is.finite(p)) {
    return(list(text = "NA", value = NA_real_, tol = Inf))
  }
  if (p == 0) {
    return(list(text = "0", value = 0, tol = 0.005))
  }
  v <- p * 100
  if (v >= 0.1) {
    dec <- 1L
    repeat {
      shown <- round(v, dec)
      half  <- 0.5 * 10 ^ (-dec)
      if (abs(v - shown) <= half * (1 - 1e-9) || dec >= 12L) {
        break
      }
      dec <- dec + 1L
    }
    s  <- format(shown, trim = TRUE, scientific = FALSE, digits = 15)
    dec_shown <- if (grepl("\\.", s)) {
      nchar(sub("^[^.]*\\.", "", s))
    } else {
      0L
    }
    return(list(text = s, value = shown / 100, tol = 0.5 * 10 ^ (-dec_shown) / 100))
  }
  s <- format(v, scientific = TRUE, digits = 3, trim = TRUE)
  mant <- sub("e.*$", "", s)
  dec <- if (grepl("\\.", mant)) nchar(sub("^[^.]*\\.", "", mant)) else 0
  ex <- suppressWarnings(as.numeric(sub("^.*e", "", s)))
  if (!is.finite(ex)) ex <- 0
  list(text = s, value = as.numeric(s) / 100,
       tol = 0.5 * 10 ^ (-dec) * 10 ^ ex / 100)
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



# Display cap for the density axis. It is sampled uniformly in x over the
# plotted view, because that is the scale the eye sees: a density that is
# unbounded at a support edge has an integrable spike whose visible width is
# tiny, and a probability-spaced sample would let that spike decide the axis
# (and hide the body and the shaded region). When the maximum is orders of
# magnitude above the body, the axis is capped at the 95th percentile of the
# density over the view; ordinary densities are not clipped at all.
# Source-coordinate area checks always use the unclipped density.
vdist_density_cap <- function(dfun, lo, hi) {
  if (!is.finite(lo) || !is.finite(hi) || hi <= lo) {
    return(1)
  }
  x <- seq(lo, hi, length.out = 501)
  y <- dfun(x)
  y <- y[is.finite(y) & y >= 0]
  if (!length(y)) {
    return(1)
  }
  mx   <- max(y)
  body <- as.numeric(stats::quantile(y, 0.95, names = FALSE))
  if (is.finite(body) && body > 0 && mx > 20 * body) body else mx
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

# Caption that discloses the fraction of the shaded region that lies outside
# the plotted window. The polygon itself covers the whole event; only the panel
# is clipped, so the area check is unaffected. Disclosed only when the visible
# picture loses at least 1% of the region; small tail slivers are not worth a
# caption.
vdist_beyond_text <- function(omitted) {
  if (!is.finite(omitted) || omitted <= 1e-2) {
    return(NULL)
  }
  paste0(vdist_pct_text(omitted),
         "% of the shaded region lies outside the plotted range")
}

# A dark region narrower than about one device pixel cannot be seen, however
# exact its area and caption. Such a region is marked with a visible strip in
# the panel and named in the caption; the polygon itself, and therefore the
# area check and the drawn geometry, is unchanged. A region wholly outside the
# view is not marked: its omission is disclosed by vdist_beyond_text() instead.
vdist_subpixel_region <- function(parts, view, rb, prob, omitted,
                                  min_rel = 4e-3) {
  if (!is.finite(prob) || prob <= 0 ||
      !is.finite(omitted) || omitted > 1e-2) {
    return(list(show = FALSE))
  }
  span <- diff(view)
  if (!is.finite(span) || span <= 0) {
    return(list(show = FALSE))
  }
  w <- 0
  mid <- 0
  for (p in parts) {
    lo <- max(p[1], rb$lo, view[1])
    hi <- min(p[2], rb$hi, view[2])
    if (is.finite(lo) && is.finite(hi) && hi > lo) {
      w   <- w + (hi - lo)
      mid <- mid + ((lo + hi) / 2) * (hi - lo)
    }
  }
  if (!is.finite(w) || w <= 0) {
    return(list(show = FALSE))
  }
  rel <- w / span
  if (rel >= min_rel) {
    return(list(show = FALSE))
  }
  mid  <- mid / w
  half <- max(rel, min_rel) * span / 2
  list(show = TRUE,
       xmin = max(view[1], mid - half),
       xmax = min(view[2], mid + half))
}

vdist_subpixel_text <- function() {
  paste("the shaded region is narrower than one pixel at this scale;",
        "it is marked by the blue strip")
}

# The visible strip that marks a sub-pixel event. Added as its own layer so it
# is never mistaken for the event polygon: it is not part of any area check.
vdist_subpixel_layer <- function(data, ylim) {
  s <- data$subpixel
  if (!isTRUE(s$show)) {
    return(NULL)
  }
  list(ggplot2::annotate("rect", xmin = s$xmin, xmax = s$xmax,
                         ymin = 0, ymax = ylim[2], fill = "#0000CD"))
}

# Plot caption: the off-view disclosure and the sub-pixel note, when each
# applies, joined into one true sentence.
vdist_caption_text <- function(data) {
  bits <- character(0)
  b <- vdist_beyond_text(data$omitted)
  if (!is.null(b)) {
    bits <- c(bits, b)
  }
  if (isTRUE(data$subpixel$show)) {
    bits <- c(bits, vdist_subpixel_text())
  }
  if (!length(bits)) {
    return(NULL)
  }
  out <- paste(bits, collapse = "; ")
  paste0(toupper(substring(out, 1L, 1L)), substring(out, 2L))
}

# ---------------------------------------------------------------------------
# Distribution geometry helpers shared by the four continuous families
# ---------------------------------------------------------------------------

# A finite quantile for a tail probability. Heavy-tailed families have no
# representable quantile below some probability; walk the requested tail
# towards the centre until the quantile is finite and report the probability
# that is actually covered.
vdist_tail_quantile <- function(qfun, p, lower.tail = TRUE, max_p = 0.1) {
  p <- max(p, 1e-300)
  repeat {
    q <- qfun(p, lower.tail = lower.tail)
    if (is.finite(q) || p >= max_p) {
      return(list(q = q, p = p))
    }
    p <- min(max_p, p * 10)
  }
}

# Region bounds for an event: the event is clipped to its support, and an
# infinite side is replaced by the representable tail quantile [q(tail)]. A
# finite support bound is used as it is (the singular-endpoint correction in
# vdist_region includes the mass at that edge exactly).
vdist_region_bounds <- function(spec, ev_lo, ev_hi) {
  lo <- ev_lo
  hi <- ev_hi
  p_lo <- p_hi <- 0
  if (is.infinite(lo) && lo < 0) {
    if (is.finite(spec$support[1])) {
      lo <- spec$support[1]
    } else {
      b <- vdist_tail_quantile(spec$qfun, spec$tail, lower.tail = TRUE)
      lo <- b$q
      p_lo <- spec$pfun(lo)
    }
  }
  if (is.infinite(hi) && hi > 0) {
    if (is.finite(spec$support[2])) {
      hi <- spec$support[2]
    } else {
      b <- vdist_tail_quantile(spec$qfun, spec$tail, lower.tail = FALSE)
      hi <- b$q
      p_hi <- spec$pfun(hi, lower.tail = FALSE)
    }
  }
  list(lo = lo, hi = hi, tail_prob = p_lo + p_hi)
}

# Expand a plotting window so that every cutoff sits strictly inside it with a
# margin on both sides. A cutoff exactly on the panel edge is drawn as a line on
# the axis and its shaded region collapses to nothing.
vdist_view_margin <- function(view, cut, scale, qfun, pfun, tail = 1e-3) {
  cut <- cut[is.finite(cut)]
  if (!length(cut)) {
    return(view)
  }
  span <- view[2] - view[1]
  margin <- if (is.finite(scale) && scale > 0) 0.25 * scale else 0
  margin <- max(margin, 0.02 * span)
  lo <- min(view[1], min(cut) - margin)
  hi <- max(view[2], max(cut) + margin)
  # extend on the event side at least a little way into the tail, so the
  # reference line is not on the boundary of the panel
  for (c in cut) {
    if (c - lo < 0.02 * (hi - lo) && is.finite(pfun(c))) {
      b <- vdist_tail_quantile(qfun, max(pfun(c) - tail, 1e-12),
                               lower.tail = TRUE)
      if (is.finite(b$q)) lo <- min(lo, b$q)
    }
    if (hi - c < 0.02 * (hi - lo) && is.finite(pfun(c, lower.tail = FALSE))) {
      b <- vdist_tail_quantile(qfun, max(pfun(c, lower.tail = FALSE) - tail, 1e-12),
                               lower.tail = FALSE)
      if (is.finite(b$q)) hi <- max(hi, b$q)
    }
  }
  c(lo, hi)
}

# Nice axis breaks for a linear window: at most 11 values, anchored on the
# distribution's own centre and scale so the labels are round numbers in the
# data units. Falls back to pretty() when the window is not a small multiple of
# the scale.
vdist_axis_breaks <- function(view, scale = NULL, center = NULL, n = 6L) {
  if (!all(is.finite(view)) || diff(view) <= 0) {
    return(ggplot2::waiver())
  }
  if (length(scale) == 1L && is.finite(scale) && scale > 0 &&
      length(center) == 1L && is.finite(center)) {
    lo <- ceiling((view[1] - center) / scale)
    hi <- floor((view[2] - center) / scale)
    k  <- hi - lo
    if (is.finite(k) && k >= 1 && k <= 10) {
      return(center + seq(lo, hi) * scale)
    }
  }
  b <- pretty(view, n = n)
  b[b >= view[1] & b <= view[2]]
}
