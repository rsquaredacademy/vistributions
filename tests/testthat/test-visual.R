context("test-visual")

test_that("output from vdist_binom_plot is as expected", {
  skip_on_cran()
  vdiffr::expect_doppelganger("binomial plot", vdist_binom_plot(10, 0.3))
})

test_that("output from vdist_binom_prob is as expected", {
  skip_on_cran()
  vdiffr::expect_doppelganger("binomial prob exact",
    vdist_binom_prob(10, 0.3, 4, type = 'exact'))
  vdiffr::expect_doppelganger("binomial prob lower",
    vdist_binom_prob(10, 0.3, 4, type = 'lower'))
  vdiffr::expect_doppelganger("binomial prob upper",
    vdist_binom_prob(10, 0.3, 4, type = 'upper'))
  vdiffr::expect_doppelganger("binomial prob interval",
    vdist_binom_prob(10, 0.3, c(4, 6), type = 'interval'))
})

test_that("output from vdist_binom_perc is as expected", {
  skip_on_cran()
  vdiffr::expect_doppelganger("binomial perc lower",
                              vdist_binom_perc(10, 0.5, 0.05))
  vdiffr::expect_doppelganger("binomial perc upper",
                              vdist_binom_perc(10, 0.5, 0.05, "upper"))
})

test_that("output from vdist_chisquare_plot is as expected", {
  skip_on_cran()
  vdiffr::expect_doppelganger("chisquare plot 1", vdist_chisquare_plot())
  vdiffr::expect_doppelganger("chisquare plot 2", vdist_chisquare_plot(df = 5))
  vdiffr::expect_doppelganger("chisquare plot 3",
                              vdist_chisquare_plot(df = 5, normal = TRUE))
})

test_that("output from vdist_chisquare_perc is as expected", {
  skip_on_cran()
  vdiffr::expect_doppelganger("chisquare perc lower",
                              vdist_chisquare_perc(0.165, 8, 'lower'))
  vdiffr::expect_doppelganger("chisquare perc upper",
                              vdist_chisquare_perc(0.22, 13, 'upper'))
})

test_that("output from vdist_chisquare_prob is as expected", {
  skip_on_cran()
  vdiffr::expect_doppelganger("chisquare prob lower",
                              vdist_chisquare_prob(13.58, 11, 'lower'))
  vdiffr::expect_doppelganger("chisquare prob upper",
                              vdist_chisquare_prob(15.72, 13, 'upper'))
})

test_that("output from vdist_f_plot is as expected", {
  skip_on_cran()
  vdiffr::expect_doppelganger("f plot 1", vdist_f_plot())
  vdiffr::expect_doppelganger("f plot 2", vdist_f_plot(6, 10, normal = TRUE))
})

test_that("output from vdist_f_perc is as expected", {
  skip_on_cran()
  vdiffr::expect_doppelganger("f perc lower", vdist_f_perc(0.95, 3, 30,
                                                           'lower'))
  vdiffr::expect_doppelganger("f perc upper", vdist_f_perc(0.125, 9, 35,
                                                           'upper'))
})

test_that("output from vdist_f_prob is as expected", {
  skip_on_cran()
  vdiffr::expect_doppelganger("f prob lower", vdist_f_prob(2.35, 5, 32))
  vdiffr::expect_doppelganger("f prob upper", vdist_f_prob(1.5222, 9, 35,
                                                           type = "upper"))
})

test_that("output from vdist_normal_plot is as expected", {
  skip_on_cran()
  vdiffr::expect_doppelganger("normal plot 1", vdist_normal_plot())
  vdiffr::expect_doppelganger("normal plot 2", vdist_normal_plot(mean = 2,
                                                                 sd = 0.6))
})

test_that("output from vdist_normal_perc is as expected", {
  skip_on_cran()
  vdiffr::expect_doppelganger("normal perc lower",
                              vdist_normal_perc(0.95, mean = 2, sd = 1.36))
  vdiffr::expect_doppelganger("normal perc both",
                              vdist_normal_perc(0.95, mean = 2, sd = 1.36,
                                                type = 'both'))
  vdiffr::expect_doppelganger("normal perc upper",
                              vdist_normal_perc(0.3, mean = 2, sd = 1.36,
                                                type = 'upper'))

})

test_that("output from vdist_normal_prob is as expected", {
  skip_on_cran()
  vdiffr::expect_doppelganger("normal prob lower",
                              vdist_normal_prob(3.78, mean = 2, sd = 1.36))
  vdiffr::expect_doppelganger("normal prob upper",
                              vdist_normal_prob(3.43, mean = 2, sd = 1.36,
                                                type = 'upper'))
  vdiffr::expect_doppelganger("normal prob both",
                              vdist_normal_prob(c(-1.74, 1.83), type = 'both'))
})

test_that("output from vdist_t_plot is as expected", {
  skip_on_cran()
  vdiffr::expect_doppelganger("t plot 1", vdist_t_plot())
  vdiffr::expect_doppelganger("t plot 2", vdist_t_plot(6))
  vdiffr::expect_doppelganger("t plot 3", vdist_t_plot(df = 8))
})

test_that("output from vdist_t_perc is as expected", {
  skip_on_cran()
  vdiffr::expect_doppelganger("t perc lower",
                              vdist_t_perc(probs = 0.95, df = 4, type = 'lower'))
  vdiffr::expect_doppelganger("t perc upper",
                              vdist_t_perc(probs = 0.35, df = 4, type = 'upper'))
  vdiffr::expect_doppelganger("t perc both",
                              vdist_t_perc(probs = 0.69, df = 7, type = 'both'))
})

test_that("output from vdist_t_prob is as expected", {
  skip_on_cran()
  vdiffr::expect_doppelganger("t prob lower", vdist_t_prob(2.045, 7, 'lower'))
  vdiffr::expect_doppelganger("t prob upper", vdist_t_prob(0.945, 7, 'upper'))
  vdiffr::expect_doppelganger("t prob both", vdist_t_prob(1.445, 7, 'interval'))
  vdiffr::expect_doppelganger("t prob interval", vdist_t_prob(1.6, 7, 'both'))
})








