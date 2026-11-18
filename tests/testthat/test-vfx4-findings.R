# Value-asserting regression tests for the fourth 0.2.2 fix round
# (Codex re-audit 2026-11-15, findings VFXX3-H1, H2, M1, M2, M3, L1).
# Each test uses the audit's reproduction and compares the plotted area or
# printed caption with an independent p*() call.

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
    if (all(c("y", "z") %in% names(l$data))) {
      area <- area + poly_area(l$data$y, l$data$z)
    } else {
      area <- area + poly_area(l$data$x, l$data$y)
    }
  }
  area
}

# probability of the cutoff printed in a one-tail caption, at the precision
# the caption actually shows
caption_tail_check <- function(s, pfun) {
  m <- regmatches(s, regexec("P\\(X ([<>]) ([^)]+)\\) = ([0-9.eE+-]+)%", s))[[1]]
  expect_length(m, 4)
  cutoff <- as.numeric(m[3])
  shown  <- as.numeric(m[4])
  dec <- if (grepl("\\.", m[4])) nchar(sub("^[^.]*\\.", "", m[4])) else 0
  got <- if (m[2] == "<") pfun(cutoff) else pfun(cutoff, lower.tail = FALSE)
  expect_lte(abs(got * 100 - shown), 0.5 * 10 ^ (-dec) + 1e-9)
}

caption_interval_check <- function(s, pfun) {
  m <- regmatches(s, regexec("P\\(([^ ]+) < X < ([^)]+)\\) = ([0-9.eE+-]+)%", s))[[1]]
  expect_length(m, 4)
  lo <- as.numeric(m[2]); hi <- as.numeric(m[3]); shown <- as.numeric(m[4])
  dec <- if (grepl("\\.", m[4])) nchar(sub("^[^.]*\\.", "", m[4])) else 0
  got <- pfun(hi) - pfun(lo)
  expect_lte(abs(got * 100 - shown), 0.5 * 10 ^ (-dec) + 1e-9)
}

# VFXX3-H1 ------------------------------------------------------------------

test_that("VFXX3-H1: upper-tail areas cover the event, not just the view", {
  cases <- list(
    list(f = function() vdist_chisquare_perc(.05, 1, type = "upper", print_plot = FALSE),
         truth = .05),
    list(f = function() vdist_chisquare_perc(.95, .1, type = "upper", print_plot = FALSE),
         truth = .95),
    list(f = function() vdist_chisquare_prob(0, 30, type = "upper", print_plot = FALSE),
         truth = 1),
    list(f = function() vdist_f_perc(.05, 5, 10, type = "upper", print_plot = FALSE),
         truth = .05),
    list(f = function() vdist_f_prob(0, 5, 10, type = "upper", print_plot = FALSE),
         truth = 1),
    list(f = function() vdist_f_prob(1, 1, 1, type = "upper", print_plot = FALSE),
         truth = .5),
    # the vignette rows named by the audit
    list(f = function() vdist_f_perc(.20, 4, 5, "upper", print_plot = FALSE),
         truth = .20),
    list(f = function() vdist_f_prob(3.89, 4, 5, "upper", print_plot = FALSE),
         truth = pf(3.89, 4, 5, lower.tail = FALSE))
  )
  for (cs in cases) {
    p <- cs$f()
    expect_lt(abs(dark_area(p) - cs$truth), 1e-3)
  }
})

# VFXX3-H2 ------------------------------------------------------------------

test_that("VFXX3-H2: translated tiny-scale normals are rejected at the limit", {
  msg <- "sd / \\|mean\\| must be at least 1e-10"
  expect_error(vdist_normal_perc(.5, mean = 1, sd = 1e-16, print_plot = FALSE), msg)
  expect_error(vdist_normal_perc(.95, mean = 1, sd = 1e-15, print_plot = FALSE), msg)
  expect_error(vdist_normal_prob(1, mean = 1, sd = 1e-16, print_plot = FALSE), msg)
  expect_error(vdist_normal_plot(mean = 1, sd = 1e-16, print_plot = FALSE), msg)

  # zero-centred tiny scales have no translation and remain valid
  for (sd in c(1e-16, 1e-100, 1e-300)) {
    expect_s3_class(vdist_normal_plot(sd = sd, print_plot = FALSE), "ggplot")
    expect_s3_class(vdist_normal_perc(.95, sd = sd, print_plot = FALSE), "ggplot")
  }

  # the last accepted translated scale keeps a true caption
  p <- vdist_normal_perc(.95, mean = 1, sd = 1e-10, print_plot = FALSE)
  caption_tail_check(subtitle(p), function(v) pnorm(v, 1, 1e-10))

  p <- vdist_normal_prob(qnorm(.95, 1, 1e-10), mean = 1, sd = 1e-10,
                         print_plot = FALSE)
  caption_tail_check(subtitle(p), function(v) pnorm(v, 1, 1e-10))
})

# VFXX3-M1 ------------------------------------------------------------------

test_that("VFXX3-M1: ordinary documented inputs are plotted, with their area", {
  cases <- list(
    list(f = function() vdist_normal_perc(.5, type = "both", print_plot = FALSE),
         truth = .5, dark = TRUE),
    list(f = function() vdist_normal_perc(.95, sd = .1, print_plot = FALSE),
         truth = .95, dark = TRUE),
    list(f = function() vdist_normal_prob(.1645, sd = .1, print_plot = FALSE),
         truth = pnorm(.1645, sd = .1), dark = TRUE),
    list(f = function() vdist_chisquare_plot(2.01, xaxis_range = 25, print_plot = FALSE),
         truth = pchisq(25, 2.01), dark = FALSE),
    list(f = function() vdist_chisquare_prob(0, 2.01, print_plot = FALSE),
         truth = 0, dark = TRUE),
    list(f = function() vdist_t_perc(.95, .1, print_plot = FALSE),
         truth = .95, dark = TRUE)
  )
  for (cs in cases) {
    p <- cs$f()
    area <- if (cs$dark) dark_area(p) else shape_area(p)
    expect_lt(abs(area - cs$truth), 1e-3)
  }
})

# VFXX3-M2 ------------------------------------------------------------------

test_that("VFXX3-M2: captions are true at the precision they print", {
  for (pr in c(.123456789, .012345)) {
    caption_tail_check(subtitle(vdist_normal_perc(pr, print_plot = FALSE)),
                       function(v) pnorm(v))
    caption_tail_check(subtitle(vdist_t_perc(pr, 7, print_plot = FALSE)),
                       function(v) pt(v, 7))
    caption_tail_check(subtitle(vdist_chisquare_perc(pr, 1, print_plot = FALSE)),
                       function(v) pchisq(v, 1))
    caption_tail_check(subtitle(vdist_f_perc(pr, 5, 10, print_plot = FALSE)),
                       function(v) pf(v, 5, 10))
  }

  # tail probabilities stay unrounded until the interval arithmetic
  p <- vdist_t_prob(.1, 7, "interval", print_plot = FALSE)
  expect_equal(subtitle(p), "P(-0.1 < X < 0.1) = 7.7%")
  caption_interval_check(subtitle(p), function(v) pt(v, 7))

  p <- vdist_t_prob(.1, 7, "both", print_plot = FALSE)
  expect_equal(subtitle(p), "P(|X| > 0.1) = 92.3%")
  expect_equal(dark_area(p), 2 * pt(-.1, 7), tolerance = 1e-3)

  # probability captions use the printed cutoff, not a different number
  p <- vdist_normal_prob(3.78, mean = 2, sd = 1.36, print_plot = FALSE)
  caption_tail_check(subtitle(p), function(v) pnorm(v, 2, 1.36))
})

# VFXX3-M3 ------------------------------------------------------------------

test_that("VFXX3-M3: normal interval annotations stay inside the view", {
  for (pc in list(c(1e6, 2e6), c(-1e6, 2e6), c(-1, 1))) {
    p <- vdist_normal_prob(pc, print_plot = FALSE)
    built <- ggplot2::ggplot_build(p)
    xr <- built$layout$panel_params[[1]]$x.range
    expect_lte(max(abs(xr)), 1.5 * max(abs(p$data$x)))
    for (d in built$data) {
      if ("x" %in% names(d) && "label" %in% names(d)) {
        expect_true(all(d$x >= xr[1] - 1e-8 & d$x <= xr[2] + 1e-8))
      }
    }
  }
})

# VFXX3-L1 ------------------------------------------------------------------

test_that("VFXX3-L1: the chi-square floor is enforced at the documented 0.1", {
  for (f in list(function() vdist_chisquare_plot(.099999, print_plot = FALSE),
                 function() vdist_chisquare_perc(.95, .099999, print_plot = FALSE),
                 function() vdist_chisquare_prob(3, .099999, print_plot = FALSE))) {
    expect_error(f(), "df must be at least 0.1")
  }
  # the boundary itself is accepted and passes its area check
  expect_s3_class(vdist_chisquare_plot(.1, print_plot = FALSE), "ggplot")
  expect_lt(abs(dark_area(vdist_chisquare_perc(.95, .1, print_plot = FALSE)) - .95),
            1e-3)
})

rd_text <- function(name) {
  db <- tryCatch(tools::Rd_db("vistributions"), error = function(e) NULL)
  if (!is.null(db) && !is.null(db[[name]])) {
    return(paste(utils::capture.output(tools::Rd2txt(db[[name]])), collapse = "\n"))
  }
  path <- test_path("..", "..", "man", name)
  if (file.exists(path)) {
    return(paste(readLines(path), collapse = "\n"))
  }
  ""
}

test_that("VFXX3-L1: help and NEWS state the implemented sampling contract", {
  expect_match(rd_text("vdist_f_plot.Rd"), "0.001")
  expect_match(rd_text("vdist_chisquare_plot.Rd"), "at least 0.1")

  news <- system.file("NEWS.md", package = "vistributions")
  if (!nzchar(news)) {
    news <- test_path("..", "..", "NEWS.md")
  }
  text <- paste(readLines(news), collapse = "\n")
  expect_match(text, "fixed number of points")
  expect_false(grepl("uniform 0.01 x-grid", text))
})

# Must-pass ordinary inputs -------------------------------------------------

test_that("every exported plotting default returns a plot", {
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

test_that("ordinary worked examples keep their event area", {
  cases <- list(
    list(f = function() vdist_normal_perc(.95, mean = 100, sd = 15, print_plot = FALSE),
         truth = .95),
    list(f = function() vdist_t_perc(.95, df = 5, print_plot = FALSE),
         truth = .95),
    list(f = function() vdist_chisquare_perc(.95, df = 1, print_plot = FALSE),
         truth = .95),
    list(f = function() vdist_chisquare_perc(.05, df = 3, type = "upper", print_plot = FALSE),
         truth = .05),
    list(f = function() vdist_f_perc(.95, 5, 10, print_plot = FALSE),
         truth = .95)
  )
  for (cs in cases) {
    expect_lt(abs(dark_area(cs$f()) - cs$truth), 1e-3)
  }
})
