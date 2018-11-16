context("test-binomial")

test_that("vdist_binom_plot throws the appropriate errors", {
  expect_error(vdist_binom_plot(10, -0.5), "p must be between 0 and 1")
  expect_error(vdist_binom_plot(10, 1.5), "p must be between 0 and 1")
  expect_error(vdist_binom_plot("10", 0.5), "n must be numeric/integer")
  expect_error(vdist_binom_plot(as.factor(10), 0.5), "n must be numeric/integer")
  expect_error(vdist_binom_plot(10, "0.5"), "p must be numeric")
  expect_error(vdist_binom_plot(10, as.factor(0.5)), "p must be numeric")
})


test_that("binomial plot is as expected", {
  skip_on_cran()
  vdiffr::expect_doppelganger("binom_plot", vdist_binom_plot(10, 0.3))
})
