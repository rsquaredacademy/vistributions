# Value-asserting regression tests for the 0.2.2 trust release (pre-CRAN audit
# 2026-11-04, findings H2-H7, M1-M3, L3). Each test compares the plotted/printed
# result with base R's d/p/q functions or a hand value.

subtitle <- function(p) p$labels$subtitle

vlines <- function(p) {
  b <- ggplot2::ggplot_build(p)$data
  unlist(lapply(b, function(d) if ("xintercept" %in% names(d)) d$xintercept))
}

poly_area <- function(d) {
  abs(sum(d$x * c(d$y[-1], d$y[1]) - d$y * c(d$x[-1], d$x[1]))) / 2
}

shaded_mass <- function(p) {
  fill <- ggplot2::ggplot_build(p)$data[[1]]$fill
  p$data$df[fill == "#0000CD"]
}

# H2 ------------------------------------------------------------------------

test_that("H2: fractional df is kept in t, chi-square and F helpers", {
  expect_equal(subtitle(vdist_t_prob(2, df = 1.9, print_plot = FALSE)),
               paste0("P(X < 2) = ", round(pt(2, 1.9), 3) * 100, "%"))
  expect_equal(subtitle(vdist_t_prob(2, df = 1.9, "upper", print_plot = FALSE)),
               paste0("P(X > 2) = ", round(pt(2, 1.9, lower.tail = FALSE), 3) * 100, "%"))
  expect_equal(subtitle(vdist_chisquare_perc(.95, df = 1.9, print_plot = FALSE)),
               paste0("P(X < ", round(qchisq(.95, 1.9), 3), ") = 95%"))
  expect_equal(subtitle(vdist_chisquare_prob(3, df = 2.5, print_plot = FALSE)),
               paste0("P(X < 3) = ", round(pchisq(3, 2.5), 3) * 100, "%"))
  expect_equal(subtitle(vdist_f_prob(1, num_df = 1.5, den_df = 3, print_plot = FALSE)),
               paste0("P(X < 1) = ", round(pf(1, 1.5, 3), 3) * 100, "%"))
  expect_equal(subtitle(vdist_f_perc(.9, num_df = 2.5, den_df = 7.5, print_plot = FALSE)),
               paste0("P(X < ", round(qf(.9, 2.5, 7.5), 3), ") = 90%"))
  expect_equal(subtitle(vdist_t_perc(.9, df = 2.5, print_plot = FALSE)),
               paste0("P(X < ", round(qt(.9, 2.5), 3), ") = 90%"))
})

test_that("H2: densities use the real-valued df (including df below one)", {
  p <- vdist_t_plot(df = .5, print_plot = FALSE)
  expect_equal(p$data$y, dt(p$data$x, .5))
  expect_false(anyNA(p$data$y))
  p <- vdist_chisquare_plot(df = 2.5, print_plot = FALSE)
  expect_equal(p$data$chi, dchisq(p$data$x, 2.5))
  p <- vdist_f_plot(num_df = 2.5, den_df = 7.5, print_plot = FALSE)
  expect_equal(p$data$y, df(p$data$x, 2.5, 7.5))
})

# H3 ------------------------------------------------------------------------

test_that("H3: normal quantiles are not rounded before plotting", {
  p <- vdist_normal_perc(.95, sd = .0001, print_plot = FALSE)
  expect_equal(max(vlines(p)), qnorm(.95, sd = .0001))
  expect_equal(subtitle(p), "P(X < 0.0001645) = 95%")

  p <- vdist_normal_perc(.95, mean = .0006, sd = .00001, print_plot = FALSE)
  expect_equal(max(vlines(p)), qnorm(.95, .0006, .00001))

  p <- vdist_normal_perc(.8, mean = 5, sd = .01, type = "both", print_plot = FALSE)
  expect_equal(sort(vlines(p)), qnorm(c(.1, .9), 5, .01))
})

test_that("H4: binomial counts must be whole numbers", {
  expect_error(vdist_binom_prob(10, .5, s = 2.9, type = "exact"), "s must be whole numbers")
  expect_error(vdist_binom_prob(10, .5, s = 2.9, type = "upper"), "s must be whole numbers")
  expect_error(vdist_binom_prob(10, .5, s = -.9, type = "exact"), "s must be whole numbers")
  expect_error(vdist_binom_plot(n = 3.9, p = .5), "n must be a single non-negative whole number")
  expect_error(vdist_binom_perc(n = 3.9, p = .5, tp = .1), "n must be a single non-negative whole number")
  expect_error(vdist_binom_plot(n = -1, p = .5), "n must be a single non-negative whole number")
  # whole numbers stored as doubles, and n = 0, still work
  p <- vdist_binom_prob(10, .5, s = 3, type = "exact", print_plot = FALSE)
  expect_equal(subtitle(p), paste0("P(X = 3) = ", sprintf("%.3f", dbinom(3, 10, .5))))
  expect_s3_class(vdist_binom_plot(n = 0, p = .5, print_plot = FALSE), "ggplot")
})

test_that("H4: binomial probabilities match stats for each event type", {
  expect_equal(sum(shaded_mass(vdist_binom_prob(10, .5, 3, "upper", print_plot = FALSE))),
               pbinom(2, 10, .5, lower.tail = FALSE))
  expect_equal(sum(shaded_mass(vdist_binom_prob(10, .5, 3, "lower", print_plot = FALSE))),
               pbinom(3, 10, .5))
  expect_equal(subtitle(vdist_binom_prob(10, .5, 3, "upper", print_plot = FALSE)),
               paste0("P(X >= 3) = ",
                      sprintf("%.3f", pbinom(2, 10, .5, lower.tail = FALSE))))
})

test_that("H4: binomial summary sd is computed from n, p exactly", {
  for (np in list(c(100, .5049), c(7, .333), c(25, .04))) {
    d <- vistributions:::bplot_data_prep(np[1], np[2])
    expect_equal(d$bsd, round(sqrt(np[1] * np[2] * (1 - np[2])), 2))
  }
})

# H5 ------------------------------------------------------------------------

test_that("H5: reversed binomial intervals are rejected", {
  expect_error(vdist_binom_prob(10, .5, s = c(6, 4), type = "interval"),
               "increasing order")
  p <- vdist_binom_prob(10, .5, s = c(4, 6), type = "interval", print_plot = FALSE)
  expect_equal(subtitle(p), paste0("P(4 <= X <= 6) = ", round(pbinom(6, 10, .5) - pbinom(3, 10, .5), 3)))
  expect_gte(as.numeric(sub(".*= ", "", subtitle(p))), 0)
})

test_that("H5: reversed normal intervals are rejected", {
  expect_error(vdist_normal_prob(c(2, -1), type = "both"), "perc\\[1\\] must be less than")
})

# H6 ------------------------------------------------------------------------

test_that("H6: binomial quantile caption states the qbinom definition", {
  p <- vdist_binom_perc(10, .5, .05, print_plot = FALSE)
  expect_equal(subtitle(p), "P(X <= 1) < 0.05, but P(X <= 2) >= 0.05")
  expect_lt(pbinom(1, 10, .5), .05)
  expect_gte(pbinom(2, 10, .5), .05)
})

test_that("H6: shading matches the reported cutoff (lower and upper)", {
  for (tp in c(.01, .05, .1, .3, .5, .8)) {
    k <- qbinom(tp, 10, .5)
    lo <- vdist_binom_perc(10, .5, tp, "lower", print_plot = FALSE)
    expect_equal(sum(shaded_mass(lo)), pbinom(k, 10, .5))
    expect_gte(sum(shaded_mass(lo)), tp)

    k <- qbinom(tp, 10, .5, lower.tail = FALSE)
    up <- vdist_binom_perc(10, .5, tp, "upper", print_plot = FALSE)
    expect_equal(sum(shaded_mass(up)), pbinom(k, 10, .5, lower.tail = FALSE))
    expect_lte(sum(shaded_mass(up)), tp)
  }
  # the case from the audit: tail mass 0.0107 (x = 9, 10), not 0.0547
  up <- vdist_binom_perc(10, .5, .05, "upper", print_plot = FALSE)
  expect_equal(sum(shaded_mass(up)), pbinom(8, 10, .5, lower.tail = FALSE))
  # exact equality boundary: P(X <= 0) = 0.5^10 exactly
  eq <- vdist_binom_perc(10, .5, .5^10, "lower", print_plot = FALSE)
  expect_equal(sum(shaded_mass(eq)), .5^10)
})

# H7 ------------------------------------------------------------------------

test_that("H7: negative symmetric t cutoffs give the same plot as |perc|", {
  p <- vdist_t_prob(-2, 7, "interval", print_plot = FALSE)
  expect_equal(subtitle(p),
               paste0("P(-2 < X < 2) = ", (1 - 2 * round(pt(-2, 7), 3)) * 100, "%"))
  expect_equal(sort(vlines(p)), c(-2, 2))
  p <- vdist_t_prob(-2, 7, "both", print_plot = FALSE)
  expect_equal(subtitle(p), paste0("P(|X| > 2) = ", 2 * round(pt(-2, 7), 3) * 100, "%"))
  expect_equal(sort(vlines(p)), c(-2, 2))
  expect_equal(subtitle(vdist_t_prob(-2, 7, "both", print_plot = FALSE)),
               subtitle(vdist_t_prob(2, 7, "both", print_plot = FALSE)))
})

# M1 ------------------------------------------------------------------------

test_that("M1: quantiles beyond the default grid still plot, at the true cutoff", {
  p <- vdist_t_perc(.95, df = 1, print_plot = FALSE)
  expect_equal(vlines(p), qt(.95, 1))
  expect_equal(subtitle(p), paste0("P(X < ", round(qt(.95, 1), 3), ") = 95%"))
  p <- vdist_chisquare_perc(.999, df = 1, print_plot = FALSE)
  expect_equal(vlines(p), qchisq(.999, 1))
  p <- vdist_normal_perc(.9999999, print_plot = FALSE)
  expect_equal(vlines(p), qnorm(.9999999))
  p <- vdist_f_perc(.999, 1, 1, print_plot = FALSE)
  expect_equal(vlines(p), qf(.999, 1, 1))
  p <- vdist_t_perc(.999, df = 1, "both", print_plot = FALSE)
  expect_equal(sort(vlines(p)), c(-1, 1) * qt(1 - .0005, 1))
})

test_that("M1: very large cutoffs do not allocate huge grids", {
  p <- vdist_t_prob(1e6, 7, print_plot = FALSE)
  expect_lt(nrow(p$data), 30000)
  p <- vdist_normal_prob(1e6, print_plot = FALSE)
  expect_lt(nrow(p$data), 30000)
  p <- vdist_chisquare_prob(1e6, 5, print_plot = FALSE)
  expect_lt(nrow(p$data), 30000)
})

# M2 ------------------------------------------------------------------------

test_that("M2: the shaded region keeps its area at small scales", {
  for (sd in c(.001, .05, 1, 250)) {
    p <- vdist_normal_plot(sd = sd, print_plot = FALSE)
    d <- p$layers[[2]]$data # central shaded polygon (mean +/- 1 sd)
    expect_gt(nrow(d), 90)
    area <- sum(diff(d$x[-c(1, nrow(d))]) * (head(d$y[-c(1, nrow(d))], -1) +
                  tail(d$y[-c(1, nrow(d))], -1)) / 2)
    expect_equal(area, pnorm(1) - pnorm(-1), tolerance = 1e-3)
  }
})

# M3 ------------------------------------------------------------------------

test_that("M3: probs of 0 or 1 are rejected with a clear message", {
  msg <- "probs must be greater than 0 and less than 1."
  expect_error(vdist_normal_perc(1), msg)
  expect_error(vdist_normal_perc(0), msg)
  expect_error(vdist_t_perc(1), msg)
  expect_error(vdist_f_perc(1), msg)
  expect_error(vdist_chisquare_perc(1), msg)
  expect_error(vdist_binom_perc(10, .5, 1), "tp must be greater than 0 and less than 1.")
})

# L3 ------------------------------------------------------------------------

test_that("L3: missing, empty and vector inputs get package errors", {
  expect_error(vdist_normal_plot(sd = NA_real_), "sd must be a single finite number.")
  expect_error(vdist_t_plot(df = numeric()), "df must be a single finite number.")
  expect_error(vdist_binom_plot(p = c(.2, .3)), "p must be a single finite number.")
  expect_error(vdist_binom_prob(10, .5, s = c(2, 4), type = "lower"), "s must be a single value")
  expect_error(vdist_f_plot(normal = NA), "normal must be a logical value.")
  expect_error(vdist_t_prob(Inf), "perc must be a single finite number.")
  expect_error(vdist_normal_prob(c(1, NA), type = "both"), "perc must be finite numbers.")
})

test_that("L3: cutoffs outside the support give probability 0, not an error", {
  expect_equal(subtitle(vdist_f_prob(-1, print_plot = FALSE)), "P(X < -1) = 0%")
  expect_equal(subtitle(vdist_chisquare_prob(-1, print_plot = FALSE)), "P(X < -1) = 0%")
  p <- vdist_chisquare_prob(-1, 5, "upper", print_plot = FALSE)
  expect_equal(subtitle(p), "P(X > -1) = 100%")
})
