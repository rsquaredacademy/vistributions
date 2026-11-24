test_that("fx18: real-device ticks do not overlap and ticks/captions fit", {
  skip_on_cran()
  skip_if_not_installed("ragg")
  cases <- fx18_calls()
  expect_gte(length(cases), 300L)
  plots <- lapply(cases, function(case) do.call(getExportedValue("vistributions", case$fun), case$args))
  for (sz in list(c(4, 3), c(7, 5), c(10, 6))) {
    path <- tempfile(fileext = ".png")
    ragg::agg_png(path, width = sz[1], height = sz[2], units = "in", res = 96)
    tryCatch({
      for (i in seq_along(plots)) {
        info <- paste(names(cases)[i], paste(sz, collapse = "x"))
        d <- fx18_text_boxes(plots[[i]])
        expect_true(any(d$kind == "tick"), info = info)
        expect_true(all(d$left >= -1e-6 & d$right <= sz[1] + 1e-6 &
          d$bottom >= -1e-6 & d$top <= sz[2] + 1e-6), info = paste(info, "device edge"))
        for (axis in unique(d$axis[d$kind == "tick"])) {
          ticks <- d[d$kind == "tick" & d$axis == axis, ]
          horizontal <- axis %in% c("axis-b", "axis-t")
          ticks <- ticks[order(if (horizontal) ticks$left else ticks$bottom), ]
          if (nrow(ticks) > 1L) {
            gap <- if (horizontal) ticks$left[-1L] - ticks$right[-nrow(ticks)] else
              ticks$bottom[-1L] - ticks$top[-nrow(ticks)]
            expect_true(all(gap >= -1e-6), info = paste(info, axis, "adjacent glyphs"))
          }
          if (horizontal) expect_true(nrow(ticks) <= 7L, info = info)
        }
      }
    }, finally = { grDevices::dev.off(); unlink(path) })
  }
})

test_that("fx18: chi-square cap is conservative, with distinct endpoints", {
  for (df in c(2e20, 1e21, 1e30)) {
    expect_length(unique(df + c(-4, 4) * sqrt(2 * df)), 2L)
  }
  expect_equal(sqrt(2 / 2e20), 1e-10)
  expect_silent(vistributions:::vdist_check_chisquare_df(2e20))
  expect_error(vistributions:::vdist_check_chisquare_df(2.001e20),
               "conservative relative-resolution limit", fixed = TRUE)
})
