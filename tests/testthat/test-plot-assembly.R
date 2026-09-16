test_that("vdist plotting functions return valid composable ggplot objects", {
  p_norm <- vdist_normal_plot(mean = 0, sd = 1, print_plot = FALSE)
  expect_s3_class(p_norm, "ggplot")
  expect_s3_class(p_norm, "gg")

  p_t <- vdist_t_plot(df = 10, print_plot = FALSE)
  expect_s3_class(p_t, "ggplot")

  p_f <- vdist_f_plot(num_df = 4, den_df = 10, print_plot = FALSE)
  expect_s3_class(p_f, "ggplot")

  p_chi <- vdist_chisquare_plot(df = 4, print_plot = FALSE)
  expect_s3_class(p_chi, "ggplot")

  p_binom <- vdist_binom_plot(n = 10, p = 0.5, print_plot = FALSE)
  expect_s3_class(p_binom, "ggplot")

  expect_no_error({
    p_comp <- p_norm + ggplot2::labs(title = "Custom Title")
    g <- ggplot2::ggplotGrob(p_comp)
  })
})
