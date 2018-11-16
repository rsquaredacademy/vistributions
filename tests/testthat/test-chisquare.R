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

test_that("vdist_chisquare_perc returns appropriate error messages", {
  expect_error(vdist_chisquare_perc("0.95"), "probs must be numeric")

  expect_error(vdist_chisquare_perc(as.factor(1)), "probs must be numeric")

  expect_error(vdist_chisquare_perc(df = "3"), "df must be numeric/integer")

  expect_error(vdist_chisquare_perc(df = as.factor(3)), "df must be numeric/integer")
})


test_that("chisquare perc lower plot is as expected", {
  skip_on_cran()
  vdiffr::expect_doppelganger("chi_perc_lower", vdist_chisquare_perc(0.92, 6, 'lower'))
})

test_that("chisquare perc upper normal plot is as expected", {
  skip_on_cran()
  vdiffr::expect_doppelganger("chi_perc_upper", vdist_chisquare_perc(0.165, 8, 'upper'))
})