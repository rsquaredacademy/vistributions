context("test-chisquare")

test_that("vdist_chi_plot returns appropriate error messages", {
  expect_error(vdist_chisquare_plot("3"), "df must be numeric/integer")

  expect_error(vdist_chisquare_plot(as.factor(3)), "df must be numeric/integer")

  expect_error(vdist_chisquare_plot(normal = 3), "normal must be logical")

  expect_error(vdist_chisquare_plot(normal = "3"), "normal must be logical")
})

test_that("vdist_chisquare_perc returns appropriate error messages", {
  expect_error(vdist_chisquare_perc("0.95"), "probs must be numeric")

  expect_error(vdist_chisquare_perc(as.factor(1)), "probs must be numeric")

  expect_error(vdist_chisquare_perc(df = "3"), "df must be numeric/integer")

  expect_error(vdist_chisquare_perc(df = as.factor(3)), "df must be numeric/integer")
})


test_that("chisquare perc plot is as expected", {
  skip_on_cran()
  gplot <- vdist_chisquare_plot(df = 5)
  vdiffr::expect_doppelganger("chi_perc", gplot)
})

test_that("chisquare perc normal plot is as expected", {
  skip_on_cran()
  gplot <- vdist_chisquare_plot(df = 5, normal = TRUE)
  vdiffr::expect_doppelganger("chi_perc_normal", gplot)
})