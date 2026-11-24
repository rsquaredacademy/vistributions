# Numeric checks avoid platform-dependent adaptive-refinement SVG vertices.
check_continuous <- function(plot, density, cdf) {
  built <- ggplot2::ggplot_build(plot)
  inv <- built$layout$panel_scales_x[[1]]$get_transformation()$inverse
  d <- ggplot2::layer_data(built, 1)
  x <- inv(d$x)
  expect_equal(d$y, density(x), tolerance = 1e-8)
  for (i in seq_along(plot$layers)) {
    if (!inherits(plot$layers[[i]]$geom, "GeomPolygon")) next
    v <- ggplot2::layer_data(built, i)
    x <- inv(v$x)
    if (length(x) < 2) next
    actual <- vistributions:::vdist_area(x, v$y)
    expected <- vistributions:::vdist_prob_between(cdf, density, min(x), max(x))
    expect_lte(abs(actual - expected), 2e-6 * max(expected, 1e-10))
  }
}

test_that("output from vdist_chisquare_plot is as expected", {

  p7 <- vdist_chisquare_plot(print_plot = FALSE)
  check_continuous(p7, function(x) dchisq(x, 3), function(x, lower.tail = TRUE) pchisq(x, 3, lower.tail = lower.tail))

  p7a <- vdist_chisquare_plot(xaxis_range = 35, print_plot = FALSE)
  check_continuous(p7a, function(x) dchisq(x, 3), function(x, lower.tail = TRUE) pchisq(x, 3, lower.tail = lower.tail))

  p8 <- vdist_chisquare_plot(df = 5, print_plot = FALSE)
  check_continuous(p8, function(x) dchisq(x, 5), function(x, lower.tail = TRUE) pchisq(x, 5, lower.tail = lower.tail))

  p9 <- vdist_chisquare_plot(df = 5, normal = TRUE, print_plot = FALSE)
  check_continuous(p9, function(x) dchisq(x, 5), function(x, lower.tail = TRUE) pchisq(x, 5, lower.tail = lower.tail))
})

test_that("output from vdist_chisquare_perc is as expected", {

  p10 <- vdist_chisquare_perc(0.165, 8, 'lower', print_plot = FALSE)
  check_continuous(p10, function(x) dchisq(x, 8), function(x, lower.tail = TRUE) pchisq(x, 8, lower.tail = lower.tail))

  p11 <- vdist_chisquare_perc(0.22, 13, 'upper', print_plot = FALSE)
  check_continuous(p11, function(x) dchisq(x, 13), function(x, lower.tail = TRUE) pchisq(x, 13, lower.tail = lower.tail))
})

test_that("output from vdist_chisquare_prob is as expected", {

  p12 <-  vdist_chisquare_prob(13.58, 11, 'lower', print_plot = FALSE)
  check_continuous(p12, function(x) dchisq(x, 11), function(x, lower.tail = TRUE) pchisq(x, 11, lower.tail = lower.tail))

  p13 <- vdist_chisquare_prob(15.72, 13, 'upper', print_plot = FALSE)
  check_continuous(p13, function(x) dchisq(x, 13), function(x, lower.tail = TRUE) pchisq(x, 13, lower.tail = lower.tail))
})

test_that("output from vdist_f_plot is as expected", {

  p14 <- vdist_f_plot(print_plot = FALSE)
  check_continuous(p14, function(x) df(x, 4, 30), function(x, lower.tail = TRUE) pf(x, 4, 30, lower.tail = lower.tail))

  p15 <- vdist_f_plot(6, 10, normal = TRUE, print_plot = FALSE)
  check_continuous(p15, function(x) df(x, 6, 10), function(x, lower.tail = TRUE) pf(x, 6, 10, lower.tail = lower.tail))
})

test_that("output from vdist_f_perc is as expected", {

  p16 <- vdist_f_perc(0.95, 3, 30, 'lower', print_plot = FALSE)
  check_continuous(p16, function(x) df(x, 3, 30), function(x, lower.tail = TRUE) pf(x, 3, 30, lower.tail = lower.tail))

  p17 <- vdist_f_perc(0.125, 9, 35, 'upper', print_plot = FALSE)
  check_continuous(p17, function(x) df(x, 9, 35), function(x, lower.tail = TRUE) pf(x, 9, 35, lower.tail = lower.tail))
})

test_that("output from vdist_f_prob is as expected", {

  p18 <- vdist_f_prob(2.35, 5, 32, print_plot = FALSE)
  check_continuous(p18, function(x) df(x, 5, 32), function(x, lower.tail = TRUE) pf(x, 5, 32, lower.tail = lower.tail))

})

test_that("output from vdist_normal_plot is as expected", {

  p21 <- vdist_normal_plot(mean = 2, sd = 0.6, print_plot = FALSE)
  check_continuous(p21, function(x) dnorm(x, 2, .6), function(x, lower.tail = TRUE) pnorm(x, 2, .6, lower.tail = lower.tail))
})

test_that("output from vdist_normal_perc is as expected", {

  p22 <- vdist_normal_perc(0.95, mean = 2, sd = 1.36, print_plot = FALSE)
  check_continuous(p22, function(x) dnorm(x, 2, 1.36), function(x, lower.tail = TRUE) pnorm(x, 2, 1.36, lower.tail = lower.tail))

  p23 <- vdist_normal_perc(0.95, mean = 2, sd = 1.36, type = 'both', print_plot = FALSE)
  check_continuous(p23, function(x) dnorm(x, 2, 1.36), function(x, lower.tail = TRUE) pnorm(x, 2, 1.36, lower.tail = lower.tail))

  p24 <- vdist_normal_perc(0.3, mean = 2, sd = 1.36, type = 'upper', print_plot = FALSE)
  check_continuous(p24, function(x) dnorm(x, 2, 1.36), function(x, lower.tail = TRUE) pnorm(x, 2, 1.36, lower.tail = lower.tail))

})

test_that("output from vdist_normal_prob is as expected", {

  p25 <- vdist_normal_prob(3.78, mean = 2, sd = 1.36, print_plot = FALSE)
  check_continuous(p25, function(x) dnorm(x, 2, 1.36), function(x, lower.tail = TRUE) pnorm(x, 2, 1.36, lower.tail = lower.tail))

  p26 <- vdist_normal_prob(3.43, mean = 2, sd = 1.36, type = 'upper', print_plot = FALSE)
  check_continuous(p26, function(x) dnorm(x, 2, 1.36), function(x, lower.tail = TRUE) pnorm(x, 2, 1.36, lower.tail = lower.tail))

  p27 <- vdist_normal_prob(c(-1.74, 1.83), type = 'both', print_plot = FALSE)
  check_continuous(p27, function(x) dnorm(x, 0, 1), function(x, lower.tail = TRUE) pnorm(x, 0, 1, lower.tail = lower.tail))
})

test_that("output from vdist_t_plot is as expected", {

  p28 <-  vdist_t_plot(print_plot = FALSE)
  check_continuous(p28, function(x) dt(x, 3), function(x, lower.tail = TRUE) pt(x, 3, lower.tail = lower.tail))

  p29 <- vdist_t_plot(6, print_plot = FALSE)
  check_continuous(p29, function(x) dt(x, 6), function(x, lower.tail = TRUE) pt(x, 6, lower.tail = lower.tail))

  p30 <- vdist_t_plot(df = 8, print_plot = FALSE)
  check_continuous(p30, function(x) dt(x, 8), function(x, lower.tail = TRUE) pt(x, 8, lower.tail = lower.tail))
})

test_that("output from vdist_t_perc is as expected", {

  p31 <- vdist_t_perc(probs = 0.95, df = 4, type = 'lower', print_plot = FALSE)
  check_continuous(p31, function(x) dt(x, 4), function(x, lower.tail = TRUE) pt(x, 4, lower.tail = lower.tail))

  p32 <- vdist_t_perc(probs = 0.35, df = 4, type = 'upper', print_plot = FALSE)
  check_continuous(p32, function(x) dt(x, 4), function(x, lower.tail = TRUE) pt(x, 4, lower.tail = lower.tail))

})

test_that("output from vdist_t_prob is as expected", {

  p34 <- vdist_t_prob(2.045, 7, 'lower', print_plot = FALSE)
  check_continuous(p34, function(x) dt(x, 7), function(x, lower.tail = TRUE) pt(x, 7, lower.tail = lower.tail))

  p35 <- vdist_t_prob(0.945, 7, 'upper', print_plot = FALSE)
  check_continuous(p35, function(x) dt(x, 7), function(x, lower.tail = TRUE) pt(x, 7, lower.tail = lower.tail))

  p36 <- vdist_t_prob(1.445, 7, 'interval', print_plot = FALSE)
  check_continuous(p36, function(x) dt(x, 7), function(x, lower.tail = TRUE) pt(x, 7, lower.tail = lower.tail))

  p37 <- vdist_t_prob(1.6, 7, 'both', print_plot = FALSE)
  check_continuous(p37, function(x) dt(x, 7), function(x, lower.tail = TRUE) pt(x, 7, lower.tail = lower.tail))
})








