# Value-asserting regression tests for the second 0.2.2 fix round
# (Codex re-audit 2026-11-14, findings VFXX-H1..H4, M1, L1..L3). Each test
# compares the plotted/printed result with base R's d/p/q functions or a hand
# value, and each one fails on the HEAD it was written for.

subtitle <- function(p) p$labels$subtitle

vlines <- function(p) {
  b <- ggplot2::ggplot_build(p)$data
  unlist(lapply(b, function(d) if ("xintercept" %in% names(d)) d$xintercept))
}

poly_area <- function(d) {
  abs(sum(d$x * c(d$y[-1], d$y[1]) - d$y * c(d$x[-1], d$x[1]))) / 2
}

dark_area <- function(p) {
  polys <- Filter(function(l) inherits(l$geom, "GeomPolygon"), p$layers)
  poly_area(polys[[1]]$data)
}

shaded_mass <- function(p) {
  fill <- ggplot2::ggplot_build(p)$data[[1]]$fill
  p$data$df[fill == "#0000CD"]
}

# the number printed as the event cutoff in "P(X < 1.23) = 95%"
event_value <- function(s) {
  part <- strsplit(s, " < ", fixed = TRUE)[[1]][2]
  as.numeric(sub(").*$", "", part))
}

# VFXX-H1 -------------------------------------------------------------------

test_that("VFXX-H1: accepted near-integer trial counts are rounded once", {
  n <- (.7 - .4) * 10
  expect_false(n == 3) # 2.9999999999999991
  expect_equal(round(n), 3)

  p <- vdist_binom_plot(n, .5, print_plot = FALSE)
  expect_equal(p$labels$title, "Binomial Distribution: n = 3 , p = 0.5")
  expect_equal(p$labels$subtitle, "Mean = 1.5 , Std. Dev. = 0.87")
  expect_equal(p$data$n, 0:3)
  expect_equal(p$data$df, dbinom(0:3, 3, .5))

  # exact probability of the rounded distribution, not the truncated one
  pe <- vdist_binom_prob(n, .5, 2, "exact", print_plot = FALSE)
  expect_equal(subtitle(pe), "P(X = 2) = 0.375")
  expect_equal(sum(shaded_mass(pe)), dbinom(2, 3, .5))
  expect_equal(sum(shaded_mass(pe)), .375)

  # s = n is now accepted instead of failing against the truncated n
  expect_equal(subtitle(vdist_binom_prob(n, .5, 3, "exact", print_plot = FALSE)),
               "P(X = 3) = 0.125")
  expect_equal(subtitle(vdist_binom_prob(n, .5, 3, "lower", print_plot = FALSE)),
               "P(X <= 3) = 1.000")
  expect_equal(sum(shaded_mass(vdist_binom_prob(n, .5, 3, "lower", print_plot = FALSE))),
               pbinom(3, 3, .5))

  # quantile plot: same three-trial distribution
  pq <- vdist_binom_perc(n, .5, .5, print_plot = FALSE)
  expect_equal(pq$data$n, 0:3)
  expect_equal(sum(shaded_mass(pq)), pbinom(qbinom(.5, 3, .5), 3, .5))

  # whole numbers stored as doubles on both sides of 3, still rounded to 3
  expect_equal(vdist_binom_plot(3 + 1e-9, .5, print_plot = FALSE)$data$n, 0:3)
  expect_equal(vdist_binom_plot(3 - 1e-9, .5, print_plot = FALSE)$data$n, 0:3)

  # genuinely fractional counts are still rejected
  expect_error(vdist_binom_plot(2.9, .5), "whole number")
  expect_error(vdist_binom_plot(3.1, .5), "whole number")
  expect_error(vdist_binom_perc(2.9, .5, .1), "whole number")
  expect_error(vdist_binom_prob(10, .5, s = 2.9), "whole number")
})

# VFXX-H2 -------------------------------------------------------------------

test_that("VFXX-H2: tiny cutoffs are printed with enough precision to be true", {
  # normal: sd = 1e-16, cutoff 1.64485362695147e-16 was printed as 0
  p <- vdist_normal_perc(.95, sd = 1e-16, print_plot = FALSE)
  expect_equal(subtitle(p), "P(X < 1.645e-16) = 95%")
  expect_equal(event_value(subtitle(p)), 1.645e-16)
  expect_equal(pnorm(event_value(subtitle(p)), sd = 1e-16), .95, tolerance = 1e-4)
  expect_equal(max(vlines(p)), qnorm(.95, sd = 1e-16))

  # chi-square with a fractional df: cutoff 1.114775e-26 was printed as 0
  p <- vdist_chisquare_perc(.05, df = .1, print_plot = FALSE)
  expect_equal(subtitle(p), "P(X < 1.115e-26) = 5%")
  expect_equal(pchisq(event_value(subtitle(p)), .1), .05, tolerance = 1e-4)
  expect_equal(max(vlines(p)), qchisq(.05, .1))

  # F with a small cutoff: the printed value is a fixed-decimal true value
  p <- vdist_f_perc(1e-6, 1, 1, print_plot = FALSE)
  expect_equal(pf(event_value(subtitle(p)), 1, 1), 1e-6, tolerance = 1e-4)
  expect_equal(max(vlines(p)), qf(1e-6, 1, 1))

  # ordinary scales keep the three-decimal text exactly as before
  expect_equal(subtitle(vdist_normal_perc(.95, sd = .0001, print_plot = FALSE)),
               "P(X < 0.0001645) = 95%")
  expect_equal(subtitle(vdist_chisquare_perc(.95, df = 1.9, print_plot = FALSE)),
               paste0("P(X < ", round(qchisq(.95, 1.9), 3), ") = 95%"))
})

# VFXX-H3 -------------------------------------------------------------------

test_that("VFXX-H3: the shaded polygon area matches the event probability", {
  cases <- list(
    list(p = vdist_t_prob(1e6, 7, print_plot = FALSE),           truth = 1),
    list(p = vdist_normal_prob(1e6, print_plot = FALSE),         truth = 1),
    list(p = vdist_chisquare_prob(1e6, 5, print_plot = FALSE),   truth = 1),
    list(p = vdist_f_prob(1e5, 4, 30, print_plot = FALSE),       truth = pf(1e5, 4, 30)),
    list(p = vdist_t_perc(.99999, 1, print_plot = FALSE),        truth = .99999),
    list(p = vdist_f_prob(5000, 4, 30, print_plot = FALSE),      truth = pf(5000, 4, 30))
  )
  for (cs in cases) {
    expect_equal(dark_area(cs$p), cs$truth, tolerance = 1e-3)
    # bounded allocation is still required
    expect_lt(nrow(cs$p$data), 30000)
  }

  # the base comparison from the audit: 625k uniform rows gave .9999644;
  # the central mass must now carry the area as well
  expect_gt(dark_area(vdist_f_prob(5000, 4, 30, print_plot = FALSE)), .999)
  expect_equal(dark_area(vdist_f_prob(1e6, 7, print_plot = FALSE)), 1, tolerance = 1e-3)

  # an ordinary plot keeps a bounded grid and its area is still right
  p <- vdist_t_prob(2.045, 7, print_plot = FALSE)
  expect_lt(nrow(p$data), 30000)
  expect_equal(dark_area(p), pt(2.045, 7) - pt(min(p$data$x), 7),
               tolerance = 1e-3)
})

# VFXX-H4 -------------------------------------------------------------------

test_that("VFXX-H4: chi-square grids respect the support and stay finite", {
  # the audited reproducer: was a spurious spike of ~133,864
  p <- vdist_chisquare_perc(.999, 1, print_plot = FALSE)
  expect_equal(subtitle(p), "P(X < 10.828) = 99.9%")
  expect_gte(min(p$data$x), 0)
  expect_true(all(is.finite(p$data$y)))
  expect_equal(dark_area(p), .999, tolerance = 1e-3)
  polys <- Filter(function(l) inherits(l$geom, "GeomPolygon"), p$layers)
  for (l in polys) expect_true(all(is.finite(l$data$y)))

  p <- vdist_chisquare_perc(.95, 1, print_plot = FALSE)
  expect_equal(dark_area(p), .95, tolerance = 1e-3)
  expect_gte(min(p$data$x), 0)

  # every chi-square view starts at or above zero, for df below and above 2
  for (df in c(.1, .5, 1, 1.9, 2, 5, 13)) {
    expect_gte(min(vdist_chisquare_plot(df, print_plot = FALSE)$data$x), 0)
    expect_gte(min(vdist_chisquare_perc(.5, df, print_plot = FALSE)$data$x), 0)
    expect_gte(min(vdist_chisquare_prob(3, df, print_plot = FALSE)$data$x), 0)
    expect_true(all(is.finite(vdist_chisquare_perc(.5, df, print_plot = FALSE)$data$y)))
  }

  # the F distribution is also on [0, Inf): no Inf in the density at zero
  p <- vdist_f_perc(.5, 1, 3, print_plot = FALSE)
  expect_gte(min(p$data$x), 0)
  expect_true(all(is.finite(p$data$y)))
  expect_true(all(is.finite(vdist_f_plot(1, 1, print_plot = FALSE)$layers[[2]]$data$z)))
  expect_true(all(is.finite(vdist_f_prob(2, 1, 1, print_plot = FALSE)$data$y)))
})

# VFXX-M1 -------------------------------------------------------------------

test_that("VFXX-M1: the normal overlay uses the unrounded df and sd", {
  # df below the documented chi-square floor is rejected, not drawn as a spike
  expect_error(vdist_chisquare_plot(1e-8, normal = TRUE, print_plot = FALSE),
               "df must be at least 0.1")

  p <- vdist_chisquare_plot(0.1, normal = TRUE, print_plot = FALSE)
  nl <- p$layers[[4]]$data
  expect_true(all(is.finite(nl$y)))
  expect_equal(nl$y, dnorm(nl$x, 0.1, sqrt(2 * 0.1)))
  expect_equal(max(nl$y), dnorm(0.1, 0.1, sqrt(2 * 0.1)), tolerance = 1e-6)
  # the summary text is formatted separately and no longer reads like a point mass
  expect_equal(p$labels$x, "Mean = 0.1  Std Dev. = 0.4472")

  # ordinary df unchanged
  p <- vdist_chisquare_plot(df = 5, normal = TRUE, print_plot = FALSE)
  nl <- p$layers[[4]]$data
  expect_equal(nl$y, dnorm(nl$x, 5, sqrt(10)))
  expect_equal(p$labels$x, "Mean = 5  Std Dev. = 3.162")
})

# VFXX-L1 -------------------------------------------------------------------

test_that("VFXX-L1: an empty perc gives a package error, not an internal one", {
  expect_error(vdist_normal_prob(numeric(), print_plot = FALSE),
               "perc must have one or two values.")
  expect_error(vdist_normal_prob(numeric(), type = "both", print_plot = FALSE),
               "perc must have one or two values.")
  expect_error(vdist_normal_prob(c(1, 2, 3), print_plot = FALSE),
               "perc must have one or two values.")
  # equal endpoints are still a valid (empty) interval
  expect_equal(subtitle(vdist_normal_prob(c(1, 1), print_plot = FALSE)),
               "P(1 < X < 1) < 1e-10%")
})
