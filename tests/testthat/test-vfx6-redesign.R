# Round-6 root-cause redesign of the drawing layer: regression tests for every
# finding id from the five audit rounds (docs-dev/vfx*-report.md), plus the
# numeric properties the redesign guarantees:
#
#   * one exact source (d/p/q functions) for every probability, cutoff and
#     caption -- never the drawn grid, never a printed label;
#   * the shaded polygon spans the whole event on a support/tail-aware grid and
#     its own area is within 1e-3 of the exact probability;
#   * captions are true at the precision they print, using one rounding rule;
#   * the picture is invariant under a change of measurement units;
#   * ordinary teaching inputs are inside the documented contract and the
#     documented limits stop with their own message.

subtitle <- function(p) p$labels$subtitle

poly_area <- function(x, y) {
  abs(sum((c(x[-1], x[1]) - x) * (y + c(y[-1], y[1])))) / 2
}

poly_layers <- function(p) {
  Filter(function(l) inherits(l$geom, "GeomPolygon"), p$layers)
}

layer_fill <- function(l) {
  fill <- l$aes_params$fill
  if (is.null(fill)) fill <- l$geom_params$fill
  fill
}

dark_layers <- function(p) {
  Filter(function(l) identical(layer_fill(l), "#0000CD"), poly_layers(p))
}

dark_area <- function(p) {
  sum(vapply(dark_layers(p), function(l) poly_area(l$data$x, l$data$y), numeric(1)))
}

shape_area <- function(p) {
  sum(vapply(poly_layers(p), function(l) {
    d <- l$data
    if (all(c("y", "z") %in% names(d))) poly_area(d$y, d$z) else poly_area(d$x, d$y)
  }, numeric(1)))
}

# Fraction of the built panel covered by the dark polygons, with the drawing
# clipped exactly as the device clips it (coordinates are mapped through the
# panel's own scale transform).
rendered_dark_area <- function(p) {
  built <- ggplot2::ggplot_build(p)
  pp    <- built$layout$panel_params[[1]]
  xr    <- pp$x.range
  yr    <- pp$y.range
  trx   <- pp$x$scale$trans$transform
  area  <- 0
  for (l in dark_layers(p)) {
    xx <- pmin(pmax(trx(l$data$x), xr[1]), xr[2])
    yy <- pmin(pmax(l$data$y, yr[1]), yr[2])
    area <- area + abs(sum((yy[-1] + yy[-length(yy)]) / 2 * diff(xx)))
  }
  area / (diff(xr) * diff(yr))
}

panel_x <- function(p) ggplot2::ggplot_build(p)$layout$panel_params[[1]]$x.range

# Probability of the event the caption prints, at the precision it prints.
caption_prob <- function(p, pfun) {
  s <- subtitle(p)
  m <- regmatches(s, regexec("P\\(X ([<>]) ([^)]+)\\)", s))[[1]]
  if (length(m) == 3) {
    v <- as.numeric(m[3])
    return(if (m[2] == "<") pfun(v) else pfun(v, lower.tail = FALSE))
  }
  m <- regmatches(s, regexec("P\\((?:\\|X\\| > )?([^ ]+) < X < ([^)]+)\\)", s))[[1]]
  if (length(m) == 3) {
    return(pfun(as.numeric(m[3])) - pfun(as.numeric(m[2])))
  }
  m <- regmatches(s, regexec("P\\(\\|X\\| > ([^)]+)\\)", s))[[1]]
  if (length(m) == 2) {
    return(2 * pfun(-abs(as.numeric(m[2]))))
  }
  stop("no recognised caption: ", s)
}

# Half a unit in the last printed digit of the caption percentage.
caption_half_unit <- function(s) {
  m <- regmatches(s, regexec("= ([0-9.eE+-]+)%", s))[[1]]
  stopifnot(length(m) == 2)
  shown <- as.numeric(m[2])
  sci   <- format(shown, scientific = TRUE, trim = TRUE)
  mant  <- sub("e.*$", "", sci)
  dec   <- if (grepl("\\.", mant)) nchar(sub("^[^.]*\\.", "", mant)) else 0
  ex    <- suppressWarnings(as.numeric(sub("^.*e", "", sci)))
  if (!is.finite(ex)) ex <- 0
  0.5 * 10 ^ (-dec) * 10 ^ ex
}

expect_caption_true <- function(p, pfun) {
  shown <- as.numeric(regmatches(subtitle(p), regexec("= ([0-9.eE+-]+)%", subtitle(p)))[[1]][2])
  got   <- caption_prob(p, pfun) * 100
  expect_lte(abs(got - shown), caption_half_unit(subtitle(p)) + 1e-9)
}

# ---------------------------------------------------------------------------
# Every continuous finding id, with its audit reproduction.
# ---------------------------------------------------------------------------
finding_cases <- list(
  H2         = list(f = function() vdist_t_prob(2, df = 1.9, print_plot = FALSE),
                    pf = function(v, lower.tail = TRUE) pt(v, 1.9, lower.tail = lower.tail)),
  H3         = list(f = function() vdist_normal_perc(.95, sd = .0001, print_plot = FALSE),
                    pf = function(v, lower.tail = TRUE) pnorm(v, sd = .0001, lower.tail = lower.tail)),
  H7         = list(f = function() vdist_t_prob(-2, 7, "interval", print_plot = FALSE),
                    pf = function(v, lower.tail = TRUE) pt(v, 7, lower.tail = lower.tail)),
  M1         = list(f = function() vdist_t_perc(.95, 1, print_plot = FALSE),
                    pf = function(v, lower.tail = TRUE) pt(v, 1, lower.tail = lower.tail)),
  M2         = list(f = function() vdist_normal_plot(sd = .001, print_plot = FALSE), shape = TRUE),
  VFXX_H2    = list(f = function() vdist_chisquare_perc(.05, .1, print_plot = FALSE),
                    pf = function(v, lower.tail = TRUE) pchisq(v, .1, lower.tail = lower.tail),
                    visible = FALSE),
  VFXX_H3    = list(f = function() vdist_f_prob(5000, 4, 30, print_plot = FALSE),
                    pf = function(v, lower.tail = TRUE) pf(v, 4, 30, lower.tail = lower.tail)),
  VFXX_H4    = list(f = function() vdist_chisquare_perc(.999, 1, print_plot = FALSE),
                    pf = function(v, lower.tail = TRUE) pchisq(v, 1, lower.tail = lower.tail)),
  VFXX2_H1   = list(f = function() vdist_chisquare_perc(.95, .1, print_plot = FALSE),
                    pf = function(v, lower.tail = TRUE) pchisq(v, .1, lower.tail = lower.tail)),
  VFXX2_H4   = list(f = function() vdist_f_plot(1e6, 1e6, print_plot = FALSE), shape = TRUE,
                    truth = pf(4, 1e6, 1e6)),
  VFXX3_H1   = list(f = function() vdist_chisquare_perc(.05, 1, "upper", print_plot = FALSE),
                    pf = function(v, lower.tail = TRUE) pchisq(v, 1, lower.tail = lower.tail)),
  VFXX3_H2   = list(f = function() vdist_normal_perc(.95, mean = 1, sd = 1e-14, print_plot = FALSE),
                    pf = function(v, lower.tail = TRUE) pnorm(v, 1, 1e-14, lower.tail = lower.tail)),
  VFXX3_M1   = list(f = function() vdist_chisquare_plot(2.01, print_plot = FALSE), shape = TRUE,
                    truth = pchisq(25, 2.01)),
  VFXX4_H1   = list(f = function() vdist_normal_perc(.95, mean = 1e-16, sd = 1e-16, print_plot = FALSE),
                    pf = function(v, lower.tail = TRUE) pnorm(v, 1e-16, 1e-16, lower.tail = lower.tail)),
  VFXX4_H2   = list(f = function() vdist_chisquare_perc(.95, 1, print_plot = FALSE),
                    pf = function(v, lower.tail = TRUE) pchisq(v, 1, lower.tail = lower.tail)),
  VFXX4_M2   = list(f = function() vdist_f_perc(.95, 2, .1, print_plot = FALSE),
                    pf = function(v, lower.tail = TRUE) pf(v, 2, .1, lower.tail = lower.tail))
)

test_that("every continuous finding keeps its exact area and caption", {
  for (id in names(finding_cases)) {
    cs <- finding_cases[[id]]
    p  <- cs$f()
    expect_s3_class(p, "ggplot")
    if (isTRUE(cs$shape)) {
      if (is.null(cs$truth)) {
        # a shape plot without a single event: the body must be drawn
        expect_gt(shape_area(p), 1e-4)
      } else {
        expect_lt(abs(shape_area(p) - cs$truth), 1e-3)
      }
      next
    }
    truth <- if (!is.null(cs$truth)) cs$truth else {
      shown <- as.numeric(regmatches(subtitle(p), regexec("= ([0-9.eE+-]+)%", subtitle(p)))[[1]][2])
      shown / 100
    }
    # the drawn polygon carries the exact event probability
    expect_lt(abs(dark_area(p) - truth), 1e-3)
    # and the caption is true at the precision it prints
    if (!is.null(cs$pf)) expect_caption_true(p, cs$pf)
    # shading is visible, not merely present in the layer data. A 5% lower
    # tail of a chi-square with df = 0.1 is 1e-26 wide: its area is exact but
    # its width is below a pixel on any linear axis (documented display limit).
    if (truth > 0 && !identical(cs$visible, FALSE)) {
      expect_gt(rendered_dark_area(p), 1e-4)
    }
  }
})

# ---------------------------------------------------------------------------
# Captions and percentages: one rounding rule, true at printed precision.
# ---------------------------------------------------------------------------
test_that("captions are true at the printed precision for every rounding regime", {
  cases <- list(
    list(f = function() vdist_normal_perc(.0505, type = "both", print_plot = FALSE),
         pf = function(v, lower.tail = TRUE) pnorm(v, lower.tail = lower.tail)),
    list(f = function() vdist_normal_perc(.0505, mean = 100, type = "both", print_plot = FALSE),
         pf = function(v, lower.tail = TRUE) pnorm(v, 100, lower.tail = lower.tail)),
    list(f = function() vdist_normal_perc(.0001, mean = 1, type = "both", print_plot = FALSE),
         pf = function(v, lower.tail = TRUE) pnorm(v, 1, lower.tail = lower.tail)),
    list(f = function() vdist_normal_perc(1e-10, mean = 1, type = "both", print_plot = FALSE),
         pf = function(v, lower.tail = TRUE) pnorm(v, 1, lower.tail = lower.tail)),
    list(f = function() vdist_t_prob(.1, 7, "interval", print_plot = FALSE),
         pf = function(v, lower.tail = TRUE) pt(v, 7, lower.tail = lower.tail)),
    list(f = function() vdist_t_prob(.1, 7, "both", print_plot = FALSE),
         pf = function(v, lower.tail = TRUE) pt(v, 7, lower.tail = lower.tail)),
    list(f = function() vdist_normal_perc(.123456789, print_plot = FALSE),
         pf = function(v, lower.tail = TRUE) pnorm(v, lower.tail = lower.tail)),
    list(f = function() vdist_t_perc(.123456789, 7, print_plot = FALSE),
         pf = function(v, lower.tail = TRUE) pt(v, 7, lower.tail = lower.tail)),
    list(f = function() vdist_chisquare_perc(.123456789, 1, print_plot = FALSE),
         pf = function(v, lower.tail = TRUE) pchisq(v, 1, lower.tail = lower.tail)),
    list(f = function() vdist_f_perc(.123456789, 5, 10, print_plot = FALSE),
         pf = function(v, lower.tail = TRUE) pf(v, 5, 10, lower.tail = lower.tail))
  )
  for (cs in cases) {
    p <- cs$f()
    expect_caption_true(p, cs$pf)
    expect_false(grepl("NA|NaN|Inf", subtitle(p)))
  }

  # a value exactly on a one-decimal rounding boundary is printed with the
  # digits that make it unambiguous, not rounded to a false value
  expect_equal(subtitle(vdist_normal_perc(.0505, type = "both", print_plot = FALSE)),
               "P(-0.06333 < X < 0.06333) = 5.05%")

  # the two-tailed t caption states the exact two-tail probability
  expect_equal(subtitle(vdist_t_prob(.1, 7, "both", print_plot = FALSE)),
               "P(|X| > 0.1) = 92.3%")
})

# ---------------------------------------------------------------------------
# One numeric source: the area and the caption come from p/q, not the grid.
# ---------------------------------------------------------------------------
test_that("area and caption use the exact distribution functions", {
  # a grid-free oracle: the event probability computed by p*() at the exact
  # quantile. The polygon area must match it, not the CDF at its own vertices.
  cases <- list(
    list(f = function() vdist_chisquare_perc(.05, 1, "upper", print_plot = FALSE),
         truth = .05),
    list(f = function() vdist_f_perc(.05, 5, 10, "upper", print_plot = FALSE),
         truth = .05),
    list(f = function() vdist_t_perc(.95, .1, print_plot = FALSE),
         truth = .95),
    list(f = function() vdist_normal_perc(.95, mean = 1e6, sd = 1e5, print_plot = FALSE),
         truth = .95),
    list(f = function() vdist_chisquare_perc(.95, .1, print_plot = FALSE),
         truth = .95)
  )
  for (cs in cases) {
    p <- cs$f()
    expect_lt(abs(dark_area(p) - cs$truth), 1e-3)
    # the shaded layer is not the axis grid: it must hold more vertices than a
    # coarse grid and its area must survive the built panel
    expect_gt(nrow(dark_layers(p)[[1]]$data), 50)
  }
})

# ---------------------------------------------------------------------------
# ggplot_build visibility: a source area is not enough.
# ---------------------------------------------------------------------------
test_that("shading is visible in the built panel for the must-pass inputs", {
  cases <- list(
    function() vdist_normal_plot(print_plot = FALSE),
    function() vdist_normal_perc(print_plot = FALSE),
    function() vdist_normal_prob(print_plot = FALSE),
    function() vdist_t_plot(print_plot = FALSE),
    function() vdist_t_perc(print_plot = FALSE),
    function() vdist_t_prob(print_plot = FALSE),
    function() vdist_chisquare_plot(print_plot = FALSE),
    function() vdist_chisquare_perc(print_plot = FALSE),
    function() vdist_chisquare_prob(print_plot = FALSE),
    function() vdist_f_plot(print_plot = FALSE),
    function() vdist_f_perc(print_plot = FALSE),
    function() vdist_f_prob(print_plot = FALSE),
    function() vdist_normal_perc(.5, type = "both", print_plot = FALSE),
    function() vdist_normal_perc(.95, sd = .1, print_plot = FALSE),
    function() vdist_normal_prob(.1645, sd = .1, print_plot = FALSE),
    function() vdist_chisquare_plot(2.01, print_plot = FALSE),
    function() vdist_t_perc(.95, .1, print_plot = FALSE),
    function() vdist_t_perc(.95, .2, print_plot = FALSE),
    function() vdist_normal_perc(.95, mean = 100, sd = 15, print_plot = FALSE),
    function() vdist_t_perc(.95, df = 5, print_plot = FALSE),
    function() vdist_chisquare_perc(.95, df = 1, print_plot = FALSE),
    function() vdist_chisquare_perc(.05, df = 3, type = "upper", print_plot = FALSE),
    function() vdist_f_perc(.95, 5, 10, print_plot = FALSE)
  )
  for (f in cases) {
    p <- f()
    expect_s3_class(p, "ggplot")
    if (!length(dark_layers(p))) {
      # a shape plot shades the whole body with the light fill
      expect_gt(shape_area(p), 1e-4)
      next
    }
    expect_gt(rendered_dark_area(p), 1e-4)
    # the built panel range is finite and the shading is inside it
    xr <- panel_x(p)
    expect_true(all(is.finite(xr)))
    lay <- dark_layers(p)[[1]]$data
    expect_lte(min(lay$x), xr[2])
    expect_gte(max(lay$x), xr[1])
  }
})

# ---------------------------------------------------------------------------
# Tails beyond the visible view.
# ---------------------------------------------------------------------------
test_that("the shading polygon covers tails beyond the plotted window", {
  # the event extends past the panel; the polygon still carries the probability
  p <- vdist_f_perc(1e-6, 5, .5, "upper", print_plot = FALSE)
  xr <- panel_x(p)
  lay <- dark_layers(p)[[1]]$data
  expect_gt(max(lay$x), xr[2])
  expect_lt(abs(dark_area(p) - 1e-6), 1e-3)
  expect_gt(rendered_dark_area(p), 1e-6)
  # and the caption discloses the clipped part of the region
  expect_match(p$labels$caption, "of the shaded region lies outside the plotted range")

  # a very far cutoff is clamped for the marker and in the panel, while the
  # caption keeps the true event
  p <- vdist_t_prob(1e6, 7, print_plot = FALSE)
  xr <- panel_x(p)
  expect_lt(max(abs(xr)), 1e4)
  for (v in unlist(lapply(ggplot2::ggplot_build(p)$data,
                          function(d) if ("xintercept" %in% names(d)) d$xintercept))) {
    expect_gte(v, xr[1] - 1e-8)
    expect_lte(v, xr[2] + 1e-8)
  }
  expect_lt(abs(dark_area(p) - 1), 1e-3)
})

# ---------------------------------------------------------------------------
# Low degrees of freedom near the support boundary.
# ---------------------------------------------------------------------------
test_that("low-df chi-square and F regions keep the mass at the support", {
  for (df in c(.1, .5, 1, 2, 5)) {
    for (pr in c(.05, .5, .95)) {
      p <- vdist_chisquare_perc(pr, df, print_plot = FALSE)
      expect_lt(abs(dark_area(p) - pr), 1e-3)
      lay <- dark_layers(p)[[1]]$data
      # the region starts at the support itself, not at a floor that discards
      # probability
      expect_equal(min(lay$x), 0)
      expect_true(all(is.finite(lay$y)))
      expect_true(all(p$data$y >= 0))
    }
  }
  # the whole shape is finite and its area is the plotted probability
  p <- vdist_chisquare_plot(df = .1, print_plot = FALSE)
  expect_lt(abs(shape_area(p) - pchisq(25, .1)), 1e-3)
  expect_true(all(is.finite(p$data$chi)))

  # the upper tail of the same singular distribution is visibly shaded
  for (pr in c(.05, .5, .95)) {
    p <- vdist_chisquare_perc(pr, .1, "upper", print_plot = FALSE)
    expect_lt(abs(dark_area(p) - pr), 1e-3)
    expect_gt(rendered_dark_area(p), 1e-4)
  }

  # F with a small numerator df has a singular density at zero as well
  p <- vdist_f_perc(.5, 1, 3, print_plot = FALSE)
  expect_equal(min(dark_layers(p)[[1]]$data$x), 0)
  expect_lt(abs(dark_area(p) - .5), 1e-3)
  expect_true(all(is.finite(vdist_f_prob(2, 1, 1, print_plot = FALSE)$data$y)))
})

# ---------------------------------------------------------------------------
# Unit invariance.
# ---------------------------------------------------------------------------
test_that("scaling the data units changes nothing but the unit labels", {
  for (k in c(1000, 0.001, 1e6, 1e-6)) {
    base   <- vdist_normal_perc(.95, mean = 100, sd = 15, print_plot = FALSE)
    scaled <- vdist_normal_perc(.95, mean = 100 * k, sd = 15 * k, print_plot = FALSE)
    expect_lt(abs(dark_area(scaled) - dark_area(base)), 1e-4)
    expect_equal(sub("^P\\([^)]*\\) = ", "", subtitle(scaled)),
                 sub("^P\\([^)]*\\) = ", "", subtitle(base)))
    expect_equal(rendered_dark_area(scaled), rendered_dark_area(base), tolerance = 1e-4)

    # The adaptive grid may hold one vertex more or less in a different unit
    # system; the picture is the density itself, so compare the standardised
    # polygon as a function (piecewise linear, with the closing baseline
    # vertices collapsed to the curve height).
    std <- function(p, kk) {
      lay <- dark_layers(p)[[1]]$data
      z <- (lay$x - 100 * kk) / (15 * kk)
      y <- lay$y * 15 * kk
      u <- sort(unique(z))
      list(z = u, y = vapply(u, function(v) max(y[z == v]), numeric(1)))
    }
    a <- std(base, 1)
    b <- std(scaled, k)
    grid <- seq(max(min(a$z), min(b$z)) + 1e-9,
                min(max(a$z), max(b$z)) - 1e-9, length.out = 300)
    ya <- stats::approx(a$z, a$y, grid, rule = 2)$y
    yb <- stats::approx(b$z, b$y, grid, rule = 2)$y
    expect_lt(max(abs(ya - yb)), 1e-3)
  }

  # probability captions and plots are invariant too
  for (k in c(1000, 0.001)) {
    p1 <- vdist_normal_prob(124.6728, mean = 100, sd = 15, print_plot = FALSE)
    p2 <- vdist_normal_prob(124.6728 * k, mean = 100 * k, sd = 15 * k, print_plot = FALSE)
    expect_equal(sub("^P\\([^)]*\\) = ", "", subtitle(p2)),
                 sub("^P\\([^)]*\\) = ", "", subtitle(p1)))
    expect_lt(abs(dark_area(p2) - dark_area(p1)), 1e-4)
  }
})

# ---------------------------------------------------------------------------
# The documented input contract.
# ---------------------------------------------------------------------------
test_that("ordinary teaching inputs are inside the contract and give their area", {
  ordinary <- list(
    list(f = function() vdist_normal_plot(print_plot = FALSE)),
    list(f = function() vdist_normal_plot(mean = 100, sd = 15, print_plot = FALSE)),
    list(f = function() vdist_normal_perc(.95, mean = 100, sd = 15, print_plot = FALSE), truth = .95),
    list(f = function() vdist_normal_prob(124.673, mean = 100, sd = 15, print_plot = FALSE),
         truth = pnorm(124.673, 100, 15)),
    list(f = function() vdist_t_plot(df = 5, print_plot = FALSE)),
    list(f = function() vdist_t_perc(.95, df = 5, print_plot = FALSE), truth = .95),
    list(f = function() vdist_t_prob(2.015, df = 5, print_plot = FALSE), truth = pt(2.015, 5)),
    list(f = function() vdist_chisquare_plot(df = 1, print_plot = FALSE)),
    list(f = function() vdist_chisquare_plot(df = 2, print_plot = FALSE)),
    list(f = function() vdist_chisquare_perc(.05, df = 1, type = "upper", print_plot = FALSE), truth = .05),
    list(f = function() vdist_chisquare_perc(.05, df = 2, type = "upper", print_plot = FALSE), truth = .05),
    list(f = function() vdist_chisquare_prob(3.841, df = 1, print_plot = FALSE),
         truth = pchisq(3.841, 1)),
    list(f = function() vdist_f_plot(5, 10, print_plot = FALSE)),
    list(f = function() vdist_f_perc(.95, 5, 10, print_plot = FALSE), truth = .95),
    list(f = function() vdist_f_prob(3.326, 5, 10, print_plot = FALSE), truth = pf(3.326, 5, 10)),
    list(f = function() vdist_binom_plot(10, .3, print_plot = FALSE)),
    list(f = function() vdist_binom_prob(10, .3, 4, "lower", print_plot = FALSE)),
    list(f = function() vdist_binom_perc(10, .5, .05, print_plot = FALSE))
  )
  for (cs in ordinary) {
    p <- cs$f()
    expect_s3_class(p, "ggplot")
    if (!is.null(cs$truth)) expect_lt(abs(dark_area(p) - cs$truth), 1e-3)
  }
})

test_that("the documented limits stop with their own message", {
  expect_error(vdist_chisquare_perc(.95, .05, print_plot = FALSE), "df must be at least 0.1")
  expect_error(vdist_chisquare_plot(.05, print_plot = FALSE), "df must be at least 0.1")
  expect_error(vdist_f_perc(.95, .5, 30, print_plot = FALSE), "num_df must be at least 1")
  expect_error(vdist_f_plot(1e9, 1e9, print_plot = FALSE), "too concentrated")
  expect_error(vdist_chisquare_perc(1e-8, 1, print_plot = FALSE),
               "probs must be between 1e-06 and 0.999999")
  expect_error(vdist_normal_perc(1e-12, print_plot = FALSE),
               "probs must be between 1e-10 and 0.9999999999")
  expect_error(vdist_t_perc(1e-12, 7, print_plot = FALSE),
               "probs must be between 1e-10 and 0.9999999999")
  expect_error(vdist_normal_perc(.95, mean = 1, sd = 1e-16, print_plot = FALSE),
               "documented limit of 1e-14")
  expect_error(vdist_binom_plot(3.1, .5), "whole number")
  expect_error(vdist_binom_prob(10, .5, c(6, 4), "interval"), "increasing order")
  expect_error(vdist_normal_prob(c(2, -1), type = "both"), "perc\\[1\\] must be less than")
})

test_that("the contract table is the one the implementation enforces", {
  tab <- vistributions:::vdist_contract()
  expect_equal(tab$dist, c("normal", "t", "chisquare", "f", "binomial"))
  expect_equal(tab$prob_lo[tab$dist == "normal"], 1e-10)
  expect_equal(tab$prob_lo[tab$dist == "chisquare"], 1e-6)
  expect_match(tab$parameter[tab$dist == "chisquare"], "0.1")
  expect_match(tab$parameter[tab$dist == "f"], "num_df >= 1")
  # NEWS states the redesign without the retracted claims of earlier rounds
  news <- paste(readLines(test_path("..", "..", "NEWS.md")), collapse = "\n")
  expect_match(news, "exact")
  expect_false(grepl("uniform 0.01 x-grid", news))
})

# ---------------------------------------------------------------------------
# API stability and the binomial drawing layer (exact d/p/q on the integers).
# ---------------------------------------------------------------------------
test_that("the exported plotting API is unchanged", {
  expected <- list(
    vdist_normal_plot = c("mean", "sd", "print_plot"),
    vdist_normal_perc = c("probs", "mean", "sd", "type", "print_plot"),
    vdist_normal_prob = c("perc", "mean", "sd", "type", "print_plot"),
    vdist_t_plot      = c("df", "print_plot"),
    vdist_t_perc      = c("probs", "df", "type", "print_plot"),
    vdist_t_prob      = c("perc", "df", "type", "print_plot"),
    vdist_chisquare_plot = c("df", "normal", "xaxis_range", "print_plot"),
    vdist_chisquare_perc = c("probs", "df", "type", "print_plot"),
    vdist_chisquare_prob = c("perc", "df", "type", "print_plot"),
    vdist_f_plot      = c("num_df", "den_df", "normal", "print_plot"),
    vdist_f_perc      = c("probs", "num_df", "den_df", "type", "print_plot"),
    vdist_f_prob      = c("perc", "num_df", "den_df", "type", "print_plot"),
    vdist_binom_plot  = c("n", "p", "print_plot"),
    vdist_binom_prob  = c("n", "p", "s", "type", "print_plot"),
    vdist_binom_perc  = c("n", "p", "tp", "type", "print_plot")
  )
  for (nm in names(expected)) {
    expect_equal(names(formals(getExportedValue("vistributions", nm))), expected[[nm]])
  }
})

test_that("binomial shading and captions come from dbinom/pbinom/qbinom", {
  shaded_mass <- function(p) {
    fill <- ggplot2::ggplot_build(p)$data[[1]]$fill
    p$data$df[fill == "#0000CD"]
  }
  expect_equal(sum(shaded_mass(vdist_binom_prob(10, .5, 3, "lower", print_plot = FALSE))),
               pbinom(3, 10, .5))
  expect_equal(sum(shaded_mass(vdist_binom_prob(10, .5, 3, "upper", print_plot = FALSE))),
               pbinom(2, 10, .5, lower.tail = FALSE))
  expect_equal(sum(shaded_mass(vdist_binom_prob(10, .5, 3, "exact", print_plot = FALSE))),
               dbinom(3, 10, .5))
  for (tp in c(.01, .05, .5, .8)) {
    lo <- vdist_binom_perc(10, .5, tp, "lower", print_plot = FALSE)
    expect_equal(sum(shaded_mass(lo)), pbinom(qbinom(tp, 10, .5), 10, .5))
    up <- vdist_binom_perc(10, .5, tp, "upper", print_plot = FALSE)
    expect_equal(sum(shaded_mass(up)),
                 pbinom(qbinom(tp, 10, .5, lower.tail = FALSE), 10, .5, lower.tail = FALSE))
  }
})

# ---------------------------------------------------------------------------
# The regression suite as a whole: no case allocates an unbounded grid.
# ---------------------------------------------------------------------------
test_that("the grids stay bounded", {
  calls <- list(
    function() vdist_t_prob(1e6, 7, print_plot = FALSE),
    function() vdist_normal_prob(1e6, print_plot = FALSE),
    function() vdist_chisquare_prob(1e6, 5, print_plot = FALSE),
    function() vdist_f_prob(1e5, 4, 30, print_plot = FALSE),
    function() vdist_f_plot(1e8, 1e8, print_plot = FALSE),
    function() vdist_t_perc(.95, .1, print_plot = FALSE),
    function() vdist_chisquare_perc(.5, .1, print_plot = FALSE)
  )
  for (f in calls) {
    p <- f()
    expect_lt(nrow(p$data), 30000)
    expect_lt(max(vapply(poly_layers(p), function(l) nrow(l$data), numeric(1))), 30000)
  }
})
