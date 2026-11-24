test_that("binomial percentile captions state the qbinom convention across jumps", {
  skip_on_cran()
  prep <- vistributions:::bperc_data_prep
  build <- vistributions:::bperc_plot_build
  for (n in c(1, 2, 5, 9, 10, 20, 100, 1000)) {
    for (p in c(.01, .1, .3, .5, .9, .99)) {
      positions <- unique(round(seq(0, n, length.out = min(n + 1, 11))))
      for (lower in c(TRUE, FALSE)) {
        type <- if (lower) "lower" else "upper"
        jumps <- pbinom(positions, n, p, lower.tail = lower)
        sorted <- sort(unique(c(0, jumps, 1)))
        mids <- (head(sorted, -1) / 2) + (tail(sorted, -1) / 2)
        targets <- unique(c(jumps, mids, .5, 1 - 1e-11, 1 - 2^-53))
        targets <- targets[targets > 0 & targets < 1]
        for (tp in targets) {
          d <- prep(n, p, tp, type)
          k <- qbinom(tp, n, p, lower.tail = lower)
          expect_equal(d$k, k, info = paste(n, p, tp, type))
          expect_equal(sum(d$plot_data$df[d$cols == "#0000CD"]),
                       pbinom(k, n, p, lower.tail = lower), tolerance = 1e-12,
                       info = paste(n, p, tp, type))
          subtitle <- build(d, type, n, p, tp)$labels$subtitle
          threshold <- if (grepl("^[<>]", vistributions:::vdist_fmt_pct(tp)$text))
            "target" else vistributions:::vdist_fmt_number(tp)
          expected <- if (lower) {
            paste0("P(X <= ", k - 1, ") < ", threshold,
                   ", but P(X <= ", k, ") >= ", threshold)
          } else {
            paste0("P(X >= ", k + 1, ") <= ", threshold,
                   ", but P(X >= ", k, ") > ", threshold)
          }
          expect_match(subtitle, expected, fixed = TRUE, info = paste(n, p, tp, type))
        }
      }
    }
  }
})

test_that("audit binomial percentile examples retain the qbinom cutoff and event", {
  cases <- list(list(9, .5, .5, "lower"), list(1, .1, .1, "upper"),
                list(1, .1, .9, "lower"), list(2, .1, .01, "upper"))
  for (z in cases) {
    n <- z[[1]]; p <- z[[2]]; tp <- z[[3]]; type <- z[[4]]
    k <- qbinom(tp, n, p, lower.tail = type == "lower")
    g <- vdist_binom_perc(n, p, tp, type, FALSE)
    bars <- ggplot2::layer_data(g, 1)
    expect_equal(sum(bars$y[bars$fill == "#0000CD"]),
                 pbinom(k, n, p, lower.tail = type == "lower"), tolerance = 1e-12)
    expected <- if (type == "lower") paste0("P(X <= ", k - 1, ") < ", tp,
      ", but P(X <= ", k, ") >= ", tp) else paste0("P(X >= ", k + 1,
      ") <= ", tp, ", but P(X >= ", k, ") > ", tp)
    expect_match(g$labels$subtitle, expected, fixed = TRUE)
  }
})
