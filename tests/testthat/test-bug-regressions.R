test_that("TC-BUG-01: vdist_normal_plot scales x-axis with sd (BUG-01)", {
  p <- vdist_normal_plot(mean = 100, sd = 10, print_plot = FALSE)
  x_range <- range(p$data$x)
  expect_lte(x_range[1], 100 - 3.5 * 10)
  expect_gte(x_range[2], 100 + 3.5 * 10)
})

test_that("TC-BUG-02: vdist_f_perc adapts x-axis bound when quantile > 4 (BUG-02)", {
  p <- vdist_f_perc(probs = 0.99, num_df = 3, den_df = 30, print_plot = FALSE)
  x_range <- range(p$data$x)
  q <- stats::qf(0.99, 3, 30)
  expect_gte(x_range[2], q)
  expect_gte(x_range[2], 5)
})

test_that("TC-BUG-03: vdist_t_prob handles perc <= -5 without wrong sign error (BUG-03)", {
  expect_no_error(p <- vdist_t_prob(perc = -6, df = 7, type = "lower", print_plot = FALSE))
  expect_s3_class(p, "ggplot")
})

test_that("TC-BUG-04: vdist_chisquare_plot polygon coordinates extend to xaxis_range (BUG-04)", {
  p <- vdist_chisquare_plot(df = 5, xaxis_range = 50, print_plot = FALSE)
  poly_layer <- p$layers[[2]]
  expect_equal(max(poly_layer$data$y), 50)
})

test_that("TC-BUG-05: F distribution functions evaluate cleanly when den_df <= 4 (BUG-05)", {
  expect_no_error(p1 <- vdist_f_plot(num_df = 5, den_df = 3, print_plot = FALSE))
  expect_no_error(p2 <- vdist_f_perc(probs = 0.95, num_df = 5, den_df = 3, print_plot = FALSE))
  expect_no_error(p3 <- vdist_f_prob(perc = 2, num_df = 5, den_df = 3, print_plot = FALSE))
  expect_s3_class(p1, "ggplot")
  expect_s3_class(p2, "ggplot")
  expect_s3_class(p3, "ggplot")
})

test_that("TC-BUG-06: vdist_t_prob with type = 'both' maps colors and positions correctly (BUG-06)", {
  p <- vdist_t_prob(perc = 2, df = 7, type = "both", print_plot = FALSE)
  text_dfs <- Filter(function(d) "label" %in% names(d), ggplot2::ggplot_build(p)$data)
  ann_df <- do.call(rbind, text_dfs)
  center_color <- ann_df[ann_df$x == 0, "colour"]
  expect_equal(center_color, "#6495ED")
  left_color <- ann_df[ann_df$x < -1, "colour"]
  expect_equal(left_color, "#0000CD")
  right_color <- ann_df[ann_df$x > 1, "colour"]
  expect_equal(right_color, "#0000CD")
})

test_that("TC-BUG-07: check_positive rejects zero and negative values (BUG-07)", {
  expect_error(vdist_normal_plot(mean = 0, sd = 0), "sd must be a positive value.")
  expect_error(vdist_normal_plot(mean = 0, sd = -1), "sd must be a positive value.")
})

test_that("TC-BUG-08: check_suggests formats error with package name before stopping (BUG-08)", {
  expect_error(
    vistributions:::check_suggests("nonexistent_package_xyz"),
    "nonexistent_package_xyz must be installed for this functionality."
  )
})

test_that("TC-BUG-09: vdist_binom_prob validates numeric types before range checks (BUG-09)", {
  expect_error(vdist_binom_prob(10, "invalid", 3), "p must be numeric.")
  expect_error(vdist_binom_prob("invalid", 0.5, 3), "n must be numeric.")
  expect_error(vdist_binom_prob(10, 0.5, "invalid"), "s must be numeric.")
})

test_that("TC-BUG-10: check_range handles scalar check without length > 1 warning (BUG-10)", {
  expect_error(vdist_binom_plot(10, -0.1), "p must be between 0 and 1 only.")
  expect_error(vdist_binom_plot(10, 1.1), "p must be between 0 and 1 only.")
})

test_that("TC-BUG-11: Snapshot names in test-visual.R are correctly matched (BUG-11)", {
  visual_file <- testthat::test_path("test-visual.R")
  skip_if_not(file.exists(visual_file), "test-visual.R not found")
  lines <- readLines(visual_file)
  interval_block <- any(grepl("vdist_t_prob\\(1\\.445.*interval", lines))
  both_block <- any(grepl("vdist_t_prob\\(1\\.6.*both", lines))
  interval_snap <- any(grepl('"t prob interval"', lines))
  both_snap <- any(grepl('"t prob both"', lines))
  expect_true(interval_block && interval_snap)
  expect_true(both_block && both_snap)
  expect_true(file.exists(testthat::test_path("_snaps", "visual", "t-prob-interval.svg")))
  expect_true(file.exists(testthat::test_path("_snaps", "visual", "t-prob-both.svg")))
})

test_that("TC-BUG-12: .onAttach runs without network access (BUG-12)", {
  expect_no_error(vistributions:::.onAttach())
})

test_that("TC-BUG-13: vdist_normal_perc scales x-axis with sd (BUG-13)", {
  p <- vdist_normal_perc(probs = 0.95, mean = 100, sd = 10, print_plot = FALSE)
  x_range <- range(p$data$x)
  expect_lte(x_range[1], 100 - 3.5 * 10)
  expect_gte(x_range[2], 100 + 3.5 * 10)
})

test_that("TC-BUG-14: vdist_binom_prob upper tail shades strictly x >= s (BUG-14)", {
  prep <- vistributions:::bprob_data_prep(n = 10, p = 0.3, s = 4, method = "upper")
  expect_equal(prep$cols[prep$plot_data$n == 3], "#6495ED")
  expect_equal(prep$cols[prep$plot_data$n == 4], "#0000CD")
  expected_prob <- round(stats::pbinom(3, 10, 0.3, lower.tail = FALSE), 3)
  expect_equal(prep$k, expected_prob)
})

test_that("TC-BUG-15: vdist_binom_prob exact shades only x == s (BUG-15)", {
  prep <- vistributions:::bprob_data_prep(n = 10, p = 0.5, s = 4, method = "exact")
  expect_equal(prep$cols[prep$plot_data$n == 4], "#0000CD")
  expect_equal(prep$cols[prep$plot_data$n == 6], "#6495ED")
  expect_equal(sum(prep$cols == "#0000CD"), 1)
})

test_that("TC-BUG-16: vdist_t_prob upper tail colors match shaded/unshaded regions (BUG-16)", {
  p <- vdist_t_prob(perc = 1.5, df = 7, type = "upper", print_plot = FALSE)
  text_dfs <- Filter(function(d) "label" %in% names(d), ggplot2::ggplot_build(p)$data)
  ann_df <- do.call(rbind, text_dfs)
  body_color <- ann_df[ann_df$x < 1.5, "colour"]
  expect_equal(body_color, "#6495ED")
  tail_color <- ann_df[ann_df$x > 1.5, "colour"]
  expect_equal(tail_color, "#0000CD")
})

test_that("TC-BUG-17: Degrees of freedom must be positive across t, f, chisquare (BUG-17)", {
  expect_error(vdist_t_plot(df = 0), "df must be a positive value.")
  expect_error(vdist_t_plot(df = -2), "df must be a positive value.")
  expect_error(vdist_t_perc(probs = 0.95, df = 0), "df must be a positive value.")
  expect_error(vdist_t_prob(perc = 1.5, df = 0), "df must be a positive value.")
  expect_error(vdist_f_plot(num_df = 0, den_df = 5), "num_df must be a positive value.")
  expect_error(vdist_f_plot(num_df = 5, den_df = 0), "den_df must be a positive value.")
  expect_error(vdist_f_perc(probs = 0.95, num_df = 0, den_df = 5), "num_df must be a positive value.")
  expect_error(vdist_f_perc(probs = 0.95, num_df = 5, den_df = 0), "den_df must be a positive value.")
  expect_error(vdist_f_prob(perc = 1.5, num_df = 0, den_df = 5), "num_df must be a positive value.")
  expect_error(vdist_f_prob(perc = 1.5, num_df = 5, den_df = 0), "den_df must be a positive value.")
  expect_error(vdist_chisquare_plot(df = 0), "df must be a positive value.")
  expect_error(vdist_chisquare_plot(df = -1), "df must be a positive value.")
  expect_error(vdist_chisquare_perc(probs = 0.95, df = 0), "df must be a positive value.")
  expect_error(vdist_chisquare_prob(perc = 2, df = 0), "df must be a positive value.")
})

test_that("TC-BUG-18: F distribution normal overlay does not extend below x = 0 (BUG-18)", {
  prep <- vistributions:::fplot_data_prep(num_df = 10, den_df = 10)
  if (nrow(prep$nline_data) > 0) {
    expect_gte(min(prep$nline_data$x), 0)
  }
})

test_that("TC-BUG-19: vdist_binom_perc accepts tp > 0.5 (BUG-19)", {
  expect_no_error(p <- vdist_binom_perc(n = 10, p = 0.5, tp = 0.8, print_plot = FALSE))
  expect_s3_class(p, "ggplot")
})
