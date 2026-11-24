# vdiffr is in Suggests as well: skip_on_cran() does not skip for a package that
# is simply absent, so a non-CRAN check without vdiffr must skip explicitly.
skip_if_not_installed("vdiffr")

test_that("output from vdist_binom_plot is as expected", {
  skip_on_cran()
  p <- vdist_binom_plot(10, 0.3, print_plot = FALSE)
  vdiffr::expect_doppelganger("binomial plot", p)
})

test_that("output from vdist_binom_prob is as expected", {
  skip_on_cran()

  p1 <- vdist_binom_prob(10, 0.3, 4, type = 'exact', print_plot = FALSE)
  vdiffr::expect_doppelganger("binomial prob exact", p1)

  p2 <- vdist_binom_prob(10, 0.3, 4, type = 'lower', print_plot = FALSE)
  vdiffr::expect_doppelganger("binomial prob lower", p2)

  p3 <- vdist_binom_prob(10, 0.3, 4, type = 'upper', print_plot = FALSE)
  vdiffr::expect_doppelganger("binomial prob upper", p3)

  p4 <- vdist_binom_prob(10, 0.3, c(4, 6), type = 'interval', print_plot = FALSE)
  vdiffr::expect_doppelganger("binomial prob interval", p4)
})

test_that("output from vdist_binom_perc is as expected", {
  skip_on_cran()

  p5 <- vdist_binom_perc(10, 0.5, 0.05, print_plot = FALSE)
  vdiffr::expect_doppelganger("binomial perc lower", p5)

  p6 <- vdist_binom_perc(10, 0.5, 0.05, "upper", print_plot = FALSE)
  vdiffr::expect_doppelganger("binomial perc upper", p6)
})

