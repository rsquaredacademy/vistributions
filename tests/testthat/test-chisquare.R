context("test-chisquare")

test_that("vdist_chi_plot returns appropriate error messages", {
  expect_error(vdist_chisquare_plot("3"), "df must be numeric/integer")

  expect_error(vdist_chisquare_plot(as.factor(3)), "df must be numeric/integer")

  expect_error(vdist_chisquare_plot(normal = 3), "normal must be logical")

  expect_error(vdist_chisquare_plot(normal = "3"), "normal must be logical")
})

test_that("chisquare perc plot is as expected", {
  skip_on_cran()
  vdiffr::expect_doppelganger("chi_perc", vdist_chisquare_plot(df = 5))
})

test_that("chisquare perc normal plot is as expected", {
  skip_on_cran()
  vdiffr::expect_doppelganger("chi_perc_normal", vdist_chisquare_plot(df = 5, normal = TRUE))
})