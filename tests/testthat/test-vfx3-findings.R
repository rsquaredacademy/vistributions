# Value-asserting regression tests for the third 0.2.2 fix round
# (Codex re-audit 2026-11-15, findings VFXX2-H1..H4, M1, M2, L1).
# Each test uses the audit's reproduction and compares the result with base R.

subtitle <- function(p) p$labels$subtitle

poly_area <- function(x, y) {
  abs(sum(x * c(y[-1], y[1]) - y * c(x[-1], x[1]))) / 2
}

dark_area <- function(p) {
  polys <- Filter(function(l) inherits(l$geom, "GeomPolygon"), p$layers)
  poly_area(polys[[1]]$data$x, polys[[1]]$data$y)
}

f_area <- function(p) {
  d <- p$layers[[2]]$data
  poly_area(d$y, d$z)
}

event_value <- function(s) {
  part <- strsplit(s, " < ", fixed = TRUE)[[1]][2]
  as.numeric(sub(").*$", "", part))
}

vlines <- function(p) {
  b <- ggplot2::ggplot_build(p)$data
  unlist(lapply(b, function(d) if ("xintercept" %in% names(d)) d$xintercept))
}

# VFXX2-H1 ------------------------------------------------------------------

test_that("VFXX2-H1: fractional-df chi-square shading keeps the requested mass", {
  cases <- list(
    list(probs = .05, df = .1,  truth = .05),
    list(probs = .5,  df = .1,  truth = .5),
    list(probs = .95, df = .1,  truth = .95),
    list(probs = .95, df = .2,  truth = .95),
    list(probs = .95, df = 1.9, truth = .95)
  )
  for (cs in cases) {
    p <- vdist_chisquare_perc(cs$probs, cs$df, print_plot = FALSE)
    expect_equal(dark_area(p), cs$truth, tolerance = 1e-3)
    expect_gte(min(p$data$x), 0)
    expect_true(all(is.finite(p$data$y)))
  }

  # the audit's 5% example: the caption and the reference line were already
  # right, but the shaded polygon must also carry the 5% mass
  p <- vdist_chisquare_perc(.05, .1, print_plot = FALSE)
  expect_equal(dark_area(p), .05, tolerance = 1e-3)
  expect_equal(event_value(subtitle(p)), 1.115e-26)
})

# VFXX2-H2 ------------------------------------------------------------------

test_that("VFXX2-H2: unusable quantiles and extreme probabilities are rejected", {
  # audit reproducer: qf(1e-6, .5, 3) is 0, so the old grid had an infinite
  # density; num_df below the documented floor is now a package error
  expect_error(vdist_f_perc(.5, .5, 3, print_plot = FALSE),
               "num_df must be at least 1")

  # qf(1e-9, 1, 1) returns 0 and does not invert the CDF
  expect_error(vdist_f_perc(1e-9, 1, 1, print_plot = FALSE),
               "probs must be between 1e-06 and 0.999999")
  expect_error(vdist_chisquare_perc(1e-18, .1, print_plot = FALSE),
               "probs must be between 1e-06 and 0.999999")

  # every accepted F quantile has a finite density and a true caption
  for (pr in c(1e-6, .05, .5, .95, 1 - 1e-6)) {
    p <- vdist_f_perc(pr, 1, 1, print_plot = FALSE)
    expect_true(all(is.finite(p$data$y)))
    expect_equal(pf(event_value(subtitle(p)), 1, 1), pr, tolerance = 1e-4)
  }
})

# VFXX2-H3 ------------------------------------------------------------------

test_that("VFXX2-H3: a translated small-scale normal cutoff keeps its precision", {
  p <- vdist_normal_perc(.95, mean = 1, sd = 1e-13, print_plot = FALSE)
  expect_equal(subtitle(p), "P(X < 1.0000000000001645) = 95%")
  cutoff <- event_value(subtitle(p))
  expect_false(cutoff == 1)
  expect_equal(pnorm(cutoff, 1, 1e-13), .95, tolerance = 1e-4)
  expect_equal(max(vlines(p)), qnorm(.95, 1, 1e-13))

  # the zero-centred case still prints its compact scientific value
  p <- vdist_normal_perc(.95, sd = 1e-16, print_plot = FALSE)
  expect_equal(subtitle(p), "P(X < 1.645e-16) = 95%")

  # the coordinate itself is not resolvable around mean = 1
  expect_error(vdist_normal_perc(.95, mean = 1, sd = 1e-16, print_plot = FALSE),
               "cannot be represented numerically")
})

# VFXX2-H4 ------------------------------------------------------------------

test_that("VFXX2-H4: concentrated valid F densities are sampled adaptively", {
  for (d in c(1e6, 1e7, 1e8)) {
    p <- vdist_f_plot(d, d, print_plot = FALSE)
    expect_equal(f_area(p), pf(4, d, d), tolerance = 1e-3)
    expect_true(all(is.finite(p$data$y)))
    expect_lt(nrow(p$data), 30000)
  }

  # beyond the documented concentration limit the function stops clearly
  expect_error(vdist_f_plot(1e9, 1e9, print_plot = FALSE),
               "too concentrated")
})

# VFXX2-M1 ------------------------------------------------------------------

test_that("VFXX2-M1: the F normal overlay uses unrounded moments", {
  p <- vdist_f_plot(1e8, 1e8, normal = TRUE, print_plot = FALSE)
  sd_text <- as.numeric(sub(".*Std Dev\\. = ", "", p$labels$x))
  expect_gt(sd_text, 0)
  expect_false(grepl("Std Dev. = NA", p$labels$x))

  nl <- p$layers[[4]]$data
  mu <- 1e8 / (1e8 - 2)
  sigma <- sqrt(2 * mu ^ 2 * (2e8 - 2) / (1e8 * (1e8 - 4)))
  expect_equal(nl$y, dnorm(nl$x, mu, sigma))
  expect_equal(max(nl$y), 1994.711, tolerance = 1e-5)
  expect_true(all(is.finite(nl$y)))

  # undefined variance: omit the overlay and say so
  expect_message(p <- vdist_f_plot(4, 3, normal = TRUE, print_plot = FALSE),
                 "Normal overlay omitted")
  expect_equal(length(Filter(function(l) inherits(l$geom, "GeomLine"), p$layers)), 1)
})

# VFXX2-M2 ------------------------------------------------------------------

test_that("VFXX2-M2: annotations, markers and reference lines stay in view", {
  cases <- list(
    vdist_t_prob(1e6, 7, print_plot = FALSE),
    vdist_normal_prob(1e6, print_plot = FALSE),
    vdist_f_prob(1e5, 4, 30, print_plot = FALSE)
  )
  for (p in cases) {
    built <- ggplot2::ggplot_build(p)
    xr <- built$layout$panel_params[[1]]$x.range
    expect_lt(max(abs(xr)), 1e4)
    # every reference line is inside the built panel view
    for (v in vlines(p)) {
      expect_gte(v, xr[1] - 1e-8)
      expect_lte(v, xr[2] + 1e-8)
    }
    # every text annotation is inside the built panel view
    for (d in built$data) {
      if ("x" %in% names(d) && "label" %in% names(d)) {
        expect_true(all(d$x >= xr[1] - 1e-8 & d$x <= xr[2] + 1e-8))
      }
    }
  }
})

# VFXX2-L1 ------------------------------------------------------------------

test_that("VFXX2-L1: NEWS states the implemented limits, not blanket claims", {
  news_file <- system.file("NEWS.md", package = "vistributions")
  if (!nzchar(news_file)) {
    news_file <- test_path("..", "..", "NEWS.md")
  }
  news <- readLines(news_file)
  text <- paste(news, collapse = "\n")
  expect_match(text, "at least 0.1")
  expect_match(text, "num_df")
  expect_match(text, "too concentrated")
  expect_match(text, "1e-6")
  expect_false(grepl("all eight findings are fixed", text, ignore.case = TRUE))
})

# Contract grid -------------------------------------------------------------

test_that("the documented grid keeps the shoelace area within 1e-3", {
  probs <- c(.05, .5, .95)

  for (df in c(.5, 1, 2, 5, 30)) {
    for (pr in probs) {
      expect_lt(abs(dark_area(vdist_chisquare_perc(pr, df, print_plot = FALSE)) - pr),
                1e-3)
      expect_lt(abs(dark_area(vdist_t_perc(pr, df, print_plot = FALSE)) - pr),
                1e-3)
    }
  }

  for (num_df in c(1, 2, 5, 30)) {
    for (pr in probs) {
      expect_lt(abs(dark_area(vdist_f_perc(pr, num_df, 30, print_plot = FALSE)) - pr),
                1e-3)
    }
  }
})

# Documented limits ---------------------------------------------------------

test_that("each documented limit stops with its own message", {
  expect_error(vdist_chisquare_plot(.01, print_plot = FALSE),
               "df must be at least 0.1")
  expect_error(vdist_f_plot(.5, 30, print_plot = FALSE),
               "num_df must be at least 1")
  expect_error(vdist_f_perc(1e-9, 1, 1, print_plot = FALSE),
               "probs must be between 1e-06 and 0.999999")
  expect_error(vdist_chisquare_perc(1e-18, .1, print_plot = FALSE),
               "probs must be between 1e-06 and 0.999999")
  expect_error(vdist_t_perc(1e-12, 7, print_plot = FALSE),
               "probs must be between 1e-10 and 0.9999999999")
  expect_error(vdist_normal_perc(1e-12, print_plot = FALSE),
               "probs must be between 1e-10 and 0.9999999999")
  expect_error(vdist_f_plot(1e9, 1e9, print_plot = FALSE),
               "too concentrated")
  expect_error(vdist_normal_perc(.95, mean = 1, sd = 1e-16, print_plot = FALSE),
               "cannot be represented numerically")
})
