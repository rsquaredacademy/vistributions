# Value-asserting regression tests for the fifth 0.2.2 fix round
# (Codex re-audit of vfx4, findings VFXX4-H1, H2, M1, M2, L1).
#
# Areas are measured from the drawn polygon vertices with an independent
# local-edge formula. Rendered visibility is measured in panel coordinates
# from ggplot_build() (coordinate ranges and transformed layer data), so a
# polygon that exists in the data but collapses to zero height on the axis is
# caught.

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

dark_area <- function(p) {
  area <- 0
  for (l in poly_layers(p)) {
    if (identical(layer_fill(l), "#0000CD")) {
      area <- area + poly_area(l$data$x, l$data$y)
    }
  }
  area
}

shape_area <- function(p) {
  area <- 0
  for (l in poly_layers(p)) {
    d <- l$data
    area <- area + if (all(c("y", "z") %in% names(d))) {
      poly_area(d$y, d$z)
    } else {
      poly_area(d$x, d$y)
    }
  }
  area
}

# Fraction of the panel covered by the drawn dark polygon after the axis
# clipping that the device applies. Layer data are mapped through the panel's
# own scale transform, so symmetric-log axes are handled.
rendered_dark_area <- function(p) {
  built <- ggplot2::ggplot_build(p)
  pp    <- built$layout$panel_params[[1]]
  xr    <- pp$x.range
  yr    <- pp$y.range
  trx   <- pp$x$scale$trans$transform
  area  <- 0
  for (l in poly_layers(p)) {
    if (!identical(layer_fill(l), "#0000CD")) next
    xx <- pmin(pmax(trx(l$data$x), xr[1]), xr[2])
    yy <- pmin(pmax(l$data$y, yr[1]), yr[2])
    area <- area + abs(sum((yy[-1] + yy[-length(yy)]) / 2 * diff(xx)))
  }
  area / (diff(xr) * diff(yr))
}

# The printed cutoffs define the event; its probability is what the caption
# must state at the precision it prints.
caption_event_prob <- function(p, pfun) {
  s <- subtitle(p)
  m <- regmatches(s, regexec("P\\(X ([<>]) ([^)]+)\\)", s))[[1]]
  if (length(m) == 3) {
    v <- as.numeric(m[3])
    return(if (m[2] == "<") pfun(v) else pfun(v, lower.tail = FALSE))
  }
  m <- regmatches(s, regexec("P\\(([^ ]+) < X < ([^)]+)\\)", s))[[1]]
  stopifnot(length(m) == 3)
  pfun(as.numeric(m[3])) - pfun(as.numeric(m[2]))
}

# Parse the printed percentage and return its half rounding unit.
caption_shown <- function(s) {
  m <- regmatches(s, regexec("= ([0-9.eE+-]+)%", s))[[1]]
  stopifnot(length(m) == 2)
  shown <- as.numeric(m[2])
  mant <- sub("e.*$", "", format(shown, scientific = TRUE, trim = TRUE))
  dec  <- if (grepl("\\.", mant)) nchar(sub("^[^.]*\\.", "", mant)) else 0
  ex   <- suppressWarnings(as.numeric(sub("^.*e", "", format(shown, scientific = TRUE, trim = TRUE))))
  if (!is.finite(ex)) ex <- 0
  list(shown = shown, half = 0.5 * 10 ^ (-dec) * 10 ^ ex)
}

expect_caption_true <- function(p, pfun) {
  s   <- subtitle(p)
  got <- caption_event_prob(p, pfun)
  sh  <- caption_shown(s)
  expect_lte(abs(got * 100 - sh$shown), sh$half + 1e-9)
}

# VFXX4-H1 ------------------------------------------------------------------

test_that("VFXX4-H1: resolvable tiny-unit normals are accepted", {
  skip_on_cran()
  p <- vdist_normal_plot(mean = 1e-16, sd = 1e-16, print_plot = FALSE)
  expect_s3_class(p, "ggplot")
  expect_gt(rendered_dark_area(p), 1e-4)

  p <- vdist_normal_perc(.95, mean = 1e-16, sd = 1e-16, print_plot = FALSE)
  expect_lt(abs(dark_area(p) - .95), 1e-3)
  expect_gt(rendered_dark_area(p), 1e-4)
  expect_caption_true(p, function(v) pnorm(v, 1e-16, 1e-16))

  p <- vdist_normal_prob(2.645e-16, mean = 1e-16, sd = 1e-16, print_plot = FALSE)
  expect_lt(abs(dark_area(p) - pnorm(2.645e-16, 1e-16, 1e-16)), 1e-3)
  expect_gt(rendered_dark_area(p), 1e-4)

  # the guard is a ratio, so mean and sd scaled together always pass
  for (k in c(1e3, 1e-3)) {
    expect_s3_class(
      vdist_normal_perc(.95, mean = 1e-16 * k, sd = 1e-16 * k, print_plot = FALSE),
      "ggplot")
  }
})

# VFXX4-H2 ------------------------------------------------------------------

test_that("VFXX4-H2: the shaded region is visible in the rendered panel", {
  skip_on_cran()
  cases <- list(
    list(f = function() vdist_chisquare_perc(.05, 1, "upper", print_plot = FALSE), truth = .05),
    list(f = function() vdist_f_prob(1, 1, 1, "upper", print_plot = FALSE), truth = .5),
    list(f = function() vdist_t_perc(.95, .1, print_plot = FALSE), truth = .95),
    list(f = function() vdist_chisquare_perc(.95, 1, print_plot = FALSE), truth = .95),
    list(f = function() vdist_normal_perc(.95, mean = 1e8, sd = 1e7, print_plot = FALSE), truth = .95),
    list(f = function() vdist_normal_perc(.95, mean = 1e6, sd = 1e5, print_plot = FALSE), truth = .95),
    list(f = function() vdist_normal_prob(1164500, mean = 1e6, sd = 1e5, print_plot = FALSE),
         truth = pnorm(1164500, 1e6, 1e5))
  )
  for (cs in cases) {
    p <- cs$f()
    expect_lt(abs(dark_area(p) - cs$truth), 1e-3)
    if (cs$truth > 0) {
      # bounded densities are no longer flattened to make a heavy tail fill the
      # panel (H12); the polygon is still present and carries the exact mass
      expect_gt(rendered_dark_area(p), 1e-5)
    }
  }
})

# VFXX4-M1 ------------------------------------------------------------------

test_that("VFXX4-M1: central percentile captions are true at printed precision", {
  cases <- list(
    list(f = function() vdist_normal_perc(.0001, mean = 1, type = "both", print_plot = FALSE),
         pf = function(v) pnorm(v, 1, 1)),
    list(f = function() vdist_normal_perc(.0001, mean = 1, sd = .1, type = "both", print_plot = FALSE),
         pf = function(v) pnorm(v, 1, .1)),
    list(f = function() vdist_normal_perc(1e-10, mean = 1, type = "both", print_plot = FALSE),
         pf = function(v) pnorm(v, 1, 1)),
    list(f = function() vdist_normal_perc(.0505, type = "both", print_plot = FALSE),
         pf = function(v) pnorm(v, 0, 1)),
    list(f = function() vdist_normal_perc(.0505, mean = 100, type = "both", print_plot = FALSE),
         pf = function(v) pnorm(v, 100, 1))
  )
  for (cs in cases) {
    p <- cs$f()
    expect_caption_true(p, cs$pf)
    m <- regmatches(subtitle(p), regexec("P\\(([^ ]+) < X < ([^)]+)\\)", subtitle(p)))[[1]]
    expect_false(length(m) == 3 && m[2] == m[3])
  }

  # the two large-mass rounding boundaries keep a one-decimal true caption
  p <- vdist_normal_perc(.0505, type = "both", print_plot = FALSE)
  expect_caption_true(p, function(v) pnorm(v))
  p <- vdist_normal_perc(.95, mean = 100, sd = 15, type = "both", print_plot = FALSE)
  expect_caption_true(p, function(v) pnorm(v, 100, 15))
})

# VFXX4-M2 ------------------------------------------------------------------

test_that("VFXX4-M2: the stated domain matches the implementation", {
  skip_on_cran()
  # these now return a plot with the caption's area (or the documented error)
  ok <- function(f, truth) {
    p <- f()
    expect_lt(abs(dark_area(p) - truth), 1e-3)
  }
  ok(function() vdist_f_prob(0, 2, .5, "upper", print_plot = FALSE), 1)
  ok(function() vdist_f_perc(.95, 2, .1, print_plot = FALSE), .95)
  # an event that the panel cannot contain discloses how much of the shading
  # lies outside it (the polygon itself always covers the whole event)
  p <- vdist_f_perc(1e-6, 5, .5, "upper", print_plot = FALSE)
  expect_true(grepl("outside the plotted range", gsub("[[:space:]]+", " ", p$labels$caption)))
  expect_match(gsub("[[:space:]]+", " ", p$labels$caption), "of the shaded region")

  # The heavy-tail joint domain rejects this probability by name; df >= 1
  # retains the full percentile range and its checked event polygon.
  expect_error(vdist_t_perc(1e-10, .1, print_plot = FALSE),
               "probs must be between 0.001 and 0.999", fixed = TRUE)
  p <- vdist_t_perc(1e-10, 1, print_plot = FALSE)
  expect_lt(abs(dark_area(p) - 1e-10), 1e-3)

  # reliable inversion at very large equal F degrees of freedom
  p <- vdist_f_perc(.95, 1e8, 1e8, print_plot = FALSE)
  expect_lt(abs(dark_area(p) - .95), 1e-3)
  p <- vdist_f_perc(.95, 1.5e8, 1.5e8, print_plot = FALSE)
  expect_lt(abs(dark_area(p) - .95), 1e-3)

  # infinite width is a tail-overflow error, not a concentration error
  err <- tryCatch(vdist_f_plot(2, .01, print_plot = FALSE), error = function(e) conditionMessage(e))
  expect_match(err, "does not span a finite interval")
  expect_false(grepl("Use smaller degrees of freedom", err))
})

# Unit invariance -----------------------------------------------------------

test_that("scaling the units does not change the plot", {
  base <- vdist_normal_perc(.95, mean = 100, sd = 15, print_plot = FALSE)
  for (k in c(1000, 0.001)) {
    scaled <- vdist_normal_perc(.95, mean = 100 * k, sd = 15 * k, print_plot = FALSE)
    expect_lt(abs(dark_area(scaled) - dark_area(base)), 1e-4)
    # same probability caption; the cutoff is printed in the new units
    expect_equal(sub("^P\\([^)]*\\) = ", "", subtitle(scaled)),
                 sub("^P\\([^)]*\\) = ", "", subtitle(base)))
    expect_match(subtitle(scaled), "= 95.0%$")

    # the dark polygon, expressed in standardized units, is the same curve.
    # The adaptive grid may hold one vertex more or less in a different unit
    # system, so the polygons are compared as piecewise-linear functions.
    std <- function(p, s) {
      lay <- Filter(function(l) identical(layer_fill(l), "#0000CD"), poly_layers(p))[[1]]
      z <- (lay$data$x - 100 * s) / (15 * s)
      y <- lay$data$y * (15 * s)
      u <- sort(unique(z))
      list(z = u, y = vapply(u, function(v) max(y[z == v]), numeric(1)))
    }
    a <- std(base, 1); b <- std(scaled, k)
    grid <- seq(max(min(a$z), min(b$z)) + 1e-9,
                min(max(a$z), max(b$z)) - 1e-9, length.out = 300)
    ya <- stats::approx(a$z, a$y, grid, rule = 2)$y
    yb <- stats::approx(b$z, b$y, grid, rule = 2)$y
    expect_lt(max(abs(ya - yb)), 1e-3)
  }
})

# Must-pass ordinary inputs -------------------------------------------------

must_pass <- list(
  list(f = function() vdist_normal_plot(print_plot = FALSE), shape = TRUE),
  list(f = function() vdist_normal_perc(print_plot = FALSE), truth = .95),
  list(f = function() vdist_normal_prob(print_plot = FALSE), truth = pnorm(3)),
  list(f = function() vdist_t_plot(print_plot = FALSE), shape = TRUE),
  list(f = function() vdist_t_perc(print_plot = FALSE), truth = .95),
  list(f = function() vdist_t_prob(print_plot = FALSE), truth = pt(1.6, 7)),
  list(f = function() vdist_chisquare_plot(print_plot = FALSE), shape = TRUE),
  list(f = function() vdist_chisquare_perc(print_plot = FALSE), truth = .95),
  list(f = function() vdist_chisquare_prob(print_plot = FALSE), truth = pchisq(13, 11)),
  list(f = function() vdist_f_plot(print_plot = FALSE), shape = TRUE),
  list(f = function() vdist_f_perc(print_plot = FALSE), truth = .95),
  list(f = function() vdist_f_prob(print_plot = FALSE), truth = pf(2.35, 5, 32)),
  list(f = function() vdist_normal_perc(.5, type = "both", print_plot = FALSE), truth = .5),
  list(f = function() vdist_normal_perc(.95, sd = .1, print_plot = FALSE), truth = .95),
  list(f = function() vdist_normal_prob(.1645, sd = .1, print_plot = FALSE), truth = pnorm(.1645, sd = .1)),
  list(f = function() vdist_chisquare_plot(2.01, print_plot = FALSE), shape = TRUE),
  list(f = function() vdist_chisquare_prob(0, 2.01, print_plot = FALSE), truth = 0),
  list(f = function() vdist_t_perc(.95, .1, print_plot = FALSE), truth = .95),
  list(f = function() vdist_t_perc(.95, .2, print_plot = FALSE), truth = .95),
  list(f = function() vdist_normal_perc(.95, mean = 100, sd = 15, print_plot = FALSE), truth = .95),
  list(f = function() vdist_t_perc(.95, df = 5, print_plot = FALSE), truth = .95),
  list(f = function() vdist_chisquare_perc(.95, df = 1, print_plot = FALSE), truth = .95),
  list(f = function() vdist_chisquare_perc(.05, df = 3, type = "upper", print_plot = FALSE), truth = .05),
  list(f = function() vdist_f_perc(.95, 5, 10, print_plot = FALSE), truth = .95)
)

test_that("every must-pass ordinary input keeps its event area and visible shading", {
  skip_on_cran()
  for (i in seq_along(must_pass)) {
    cs <- must_pass[[i]]
    p  <- cs$f()
    expect_s3_class(p, "ggplot")
    if (isTRUE(cs$shape)) {
      expect_gt(shape_area(p), 1e-4)
      next
    }
    expect_lt(abs(dark_area(p) - cs$truth), 1e-3)
    if (cs$truth > 0) {
      # bounded densities are not flattened to make a heavy tail fill the
      # panel (H12); the polygon is still present with its exact mass
      expect_gt(rendered_dark_area(p), 1e-5)
    }
  }
})

test_that("every exported plotting default returns a usable plot", {
  skip_on_cran()
  calls <- list(
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
    function() vdist_binom_plot(print_plot = FALSE),
    function() vdist_binom_perc(print_plot = FALSE),
    function() vdist_binom_prob(print_plot = FALSE)
  )
  for (f in calls) {
    expect_s3_class(f(), "ggplot")
  }
})

# VFXX4-L1 ------------------------------------------------------------------

test_that("VFXX4-L1: help and NEWS state the limits this implementation enforces", {
  rd_text <- function(name) {
    db <- tryCatch(tools::Rd_db("vistributions"), error = function(e) NULL)
    if (!is.null(db) && !is.null(db[[name]])) {
      return(paste(utils::capture.output(tools::Rd2txt(db[[name]])), collapse = "\n"))
    }
    path <- test_path("..", "..", "man", name)
    if (file.exists(path)) return(paste(readLines(path), collapse = "\n"))
    ""
  }
  expect_match(rd_text("vdist_normal_plot.Rd"), "sd / abs\\(mean\\)")
  expect_match(rd_text("vdist_normal_plot.Rd"), "1e-10")
  expect_match(rd_text("vdist_f_plot.Rd"), "0.001")
  expect_match(rd_text("vdist_f_plot.Rd"), "finite")

  news <- system.file("NEWS.md", package = "vistributions")
  if (!nzchar(news)) news <- test_path("..", "..", "NEWS.md")
  text <- paste(readLines(news), collapse = "\n")
  expect_match(text, "fixed number of points")
  expect_match(text, "sd / \\|mean\\|")
  expect_false(grepl("uniform 0.01 x-grid", text))
})
