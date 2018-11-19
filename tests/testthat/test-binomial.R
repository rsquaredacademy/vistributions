context("test-binomial")

test_that("vdist_binom_plot throws the appropriate errors", {
  expect_error(vdist_binom_plot(10, -0.5), "p must be between 0 and 1")
  expect_error(vdist_binom_plot(10, 1.5), "p must be between 0 and 1")
  expect_error(vdist_binom_plot("10", 0.5), "n must be numeric/integer")
  expect_error(vdist_binom_plot(as.factor(10), 0.5), "n must be numeric/integer")
  expect_error(vdist_binom_plot(10, "0.5"), "p must be numeric")
  expect_error(vdist_binom_plot(10, as.factor(0.5)), "p must be numeric")
})

test_that("vdist_binom_prob throws the appropriate errors", {
  expect_error(vdist_binom_prob(10, -0.5, 4), "p must be between 0 and 1")
  expect_error(vdist_binom_prob(10, 1.5, 4), "p must be between 0 and 1")
  expect_error(vdist_binom_prob(10, "0.5", 4), "p must be numeric")
  expect_error(vdist_binom_prob("10", 0.5, 4), "n must be numeric/integer")
  expect_error(vdist_binom_prob(as.factor(10), 0.5, 4), "n must be numeric/integer")
  expect_error(vdist_binom_prob(10, 0.5, "4"), "s must be numeric/integer")
  expect_error(vdist_binom_prob(10, 0.5, as.factor(4)), "s must be numeric/integer")
})

test_that("vdist_binom_perc throws the appropriate errors", {
  expect_error(vdist_binom_perc(10, -0.5, 0.05), "p must be between 0 and 1")
  expect_error(vdist_binom_perc(10, 1.5, 0.05), "p must be between 0 and 1")
  expect_error(vdist_binom_perc(10, 0.5, -0.05), "tp must be between 0 and 0.5")
  expect_error(vdist_binom_perc(10, 0.5, 0.51), "tp must be between 0 and 0.5")
  expect_error(vdist_binom_perc("10", 0.5, 0.05), "n must be numeric/integer")
  expect_error(vdist_binom_perc(as.factor(10), 0.5, 0.05), "n must be numeric/integer")
  expect_error(vdist_binom_perc(10, "0.5", 0.05), "p must be numeric")
  expect_error(vdist_binom_perc(10, as.factor(0.5), 0.05), "p must be numeric")
  expect_error(vdist_binom_perc(10, 0.5, "0.05"), "tp must be numeric")
  expect_error(vdist_binom_perc(10, 0.5, as.factor(0.05)), "tp must be numeric")
})


test_that("binomial plot is as expected", {
  skip_on_cran()
  gplot <- vdist_binom_plot(10, 0.3)
  vdiffr::expect_doppelganger("binom_plot", gplot)
})

test_that("binomial prob exact plot is as expected", {
  skip_on_cran()
  gplot <- vdist_binom_prob(10, 0.3, 4, type = 'exact')
  vdiffr::expect_doppelganger("binom_prob_exact", gplot)
})

test_that("binomial prob lower plot is as expected", {
  skip_on_cran()
  gplot <- vdist_binom_prob(10, 0.3, 4, type = 'lower')
  vdiffr::expect_doppelganger("binom_prob_lower", gplot)
})

test_that("binomial prob upper plot is as expected", {
  skip_on_cran()
  gplot <- vdist_binom_prob(10, 0.3, 4, type = 'upper')
  vdiffr::expect_doppelganger("binom_prob_upper", gplot)
})

test_that("binomial prob interval plot is as expected", {
  skip_on_cran()
  gplot <- vdist_binom_prob(10, 0.3, c(4, 6), type = 'interval')
  vdiffr::expect_doppelganger("binom_prob_interval", gplot)
})

test_that("binomial perc lower plot is as expected", {
  skip_on_cran()
  gplot <- vdist_binom_perc(10, 0.4, 0.05)
  vdiffr::expect_doppelganger("binom_perc_lower", gplot)
})

test_that("binomial perc upper plot is as expected", {
  skip_on_cran()
  gplot <- vdist_binom_perc(10, 0.4, 0.05, 'upper')
  vdiffr::expect_doppelganger("binom_perc_upper", gplot)
})