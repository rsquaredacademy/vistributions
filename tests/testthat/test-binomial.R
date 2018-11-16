context("test-binomial")

test_that("binomial plot is as expected", {
  skip_on_cran()
  vdiffr::expect_doppelganger("binom_plot", vdist_binom_plot(10, 0.3))
})
