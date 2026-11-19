# Glyph bounds must be checked on the device, not by testing label anchors.
# The panel viewport here represents a compact plotting device; the same grid
# units are evaluated anew when the plot is drawn on a larger/smaller device.
fx15_text_extents <- function(p, width = 120) {
  gt <- ggplot2::ggplotGrob(p)
  panel <- gt$grobs[[which(gt$layout$name == "panel")]]
  texts <- Filter(function(g) inherits(g, "text"), panel$children)
  grid::pushViewport(grid::viewport(width = grid::unit(width, "mm"),
                                    xscale = c(0, 1)))
  on.exit(grid::popViewport())
  do.call(rbind, lapply(texts, function(g) {
    w <- grid::convertWidth(grid::grobWidth(g), "npc", valueOnly = TRUE)
    x <- grid::convertX(g$x, "npc", valueOnly = TRUE)
    data.frame(label = g$label, left = x - w * g$hjust,
               right = x + w * (1 - g$hjust))
  }))
}

fx15_mass <- function(p) {
  layers <- Filter(function(l) inherits(l$geom, "GeomPolygon") &&
    identical(l$aes_params$fill, "#0000CD"), p$layers)
  sum(vapply(layers, function(l) vistributions:::vdist_area(l$data$x, l$data$y),
             numeric(1)))
}

test_that("fx15: percentile glyph extents stay inside ordinary linear panels", {
  check <- function(p, id) {
    expect_false(vistributions:::vdist_wide_window(p$coordinates$limits$x), info = id)
    e <- fx15_text_extents(p)
    expect_true(nrow(e) >= 2, info = id)
    expect_true(all(e$left >= -1e-10 & e$right <= 1 + 1e-10), info = id)
  }
  for (prob in c(.05, .5, .95, .975)) {
    for (type in c("lower", "upper", "both")) {
      for (df in c(1, 2, 5, 30)) {
        check(vdist_t_perc(prob, df, type, FALSE), paste("t", df, prob, type))
      }
      check(vdist_normal_perc(prob, 0, 1, type, FALSE), paste("normal", prob, type))
    }
    for (df in 1:30) for (type in c("lower", "upper")) {
      check(vdist_chisquare_perc(prob, df, type, FALSE), paste("chi", df, prob, type))
    }
  }
  # Verify the harness detects the original ordinary-input clipping.
  old <- vdist_t_perc(.975, 1, "lower", FALSE)
  for (i in seq_along(old$layers)) {
    if (inherits(old$layers[[i]]$geom, "GeomText")) old$layers[[i]]$geom <- ggplot2::GeomText
  }
  old$layers[[3]]$data$x <- old$coordinates$limits$x[2]
  expect_gt(max(fx15_text_extents(old)$right), 1)
})

test_that("fx15: wide t and F label pairs have visible gaps and unclipped glyphs", {
  cases <- list(
    vdist_t_perc(.95, .1, "lower", FALSE),
    vdist_t_perc(.95, .1, "upper", FALSE),
    vdist_f_perc(.99, 1, 1, "lower", FALSE),
    vdist_f_perc(.99, 2, 1, "lower", FALSE),
    vdist_f_perc(.01, 1, 1, "upper", FALSE),
    vdist_t_prob(1e8, .1, "lower", FALSE),
    vdist_t_prob(1e8, .1, "upper", FALSE))
  for (p in cases) for (width in c(90, 150)) {
    e <- fx15_text_extents(p, width)
    e <- e[order(e$left), ]
    expect_true(all(e$left >= -1e-10 & e$right <= 1 + 1e-10))
    expect_gt(e$left[2] - e$right[1], 0)
  }
})

test_that("fx15: the t joint domain works at and just inside its boundaries", {
  for (df in c(.05, .1, .5, .999999)) {
    for (prob in c(.001, .00100001, .99899999, .999)) {
      for (type in c("lower", "upper", "both")) {
        p <- vdist_t_perc(prob, df, type, FALSE)
        expect_lt(abs(fx15_mass(p) / prob - 1), 1e-6,
                  label = paste(df, prob, type))
      }
    }
    for (prob in c(.00099999, .99900001, 1e-10, 1.1e-12)) {
      expect_error(vdist_t_perc(prob, df, print_plot = FALSE),
        "probs must be between 0.001 and 0.999 (inclusive)", fixed = TRUE)
    }
    limit <- qt(.001, df, lower.tail = FALSE)
    for (type in c("lower", "upper", "interval", "both")) {
      for (sign in c(-1, 1)) {
        x <- sign * limit
        p <- vdist_t_prob(x, df, type, FALSE)
        truth <- switch(type, lower = pt(x, df), upper = pt(x, df, lower.tail = FALSE),
          interval = 1 - 2 * pt(-abs(x), df), both = 2 * pt(-abs(x), df))
        expect_lt(abs(fx15_mass(p) / truth - 1), 1e-6)
        expect_error(vdist_t_prob(x * (1 + 1e-8), df, type, FALSE),
                     "perc must be between", fixed = TRUE)
      }
    }
  }
  # At df = 1 the full documented probability range is retained, all types.
  for (df in c(1, 2, 5, 30)) for (prob in c(1.0001e-12, 1 - 1.0001e-12)) {
    for (type in c("lower", "upper", "both")) {
      p <- vdist_t_perc(prob, df, type, FALSE)
      event <- vistributions:::tperc_data_prep(prob, df, type)$event_prob
      # Near the central quantile's floating-point limit, check the polygon
      # against the exact base-R event and the request at display precision.
      expect_lt(abs(fx15_mass(p) / event - 1), 1e-6)
      expect_identical(vistributions:::vdist_pct_text(event),
                        vistributions:::vdist_pct_text(prob))
    }
  }
  expect_error(vdist_t_prob(1e100, .1, "both", FALSE), "perc must be between", fixed = TRUE)
})

test_that("fx15: chi-square range cap applies only to explicit windows", {
  p <- vdist_chisquare_plot(1e10, print_plot = FALSE)
  expect_equal(p$coordinates$limits$x, 1e10 + c(-4, 4) * sqrt(2e10))
  layers <- Filter(function(l) inherits(l$geom, "GeomPolygon"), p$layers)
  mass <- sum(vapply(layers, function(l) vistributions:::vdist_area(l$data$y, l$data$z), numeric(1)))
  expect_lt(abs(mass / diff(pchisq(p$coordinates$limits$x, 1e10)) - 1), 1e-6)
  expect_s3_class(vdist_chisquare_plot(3, xaxis_range = 1e10, print_plot = FALSE), "ggplot")
  expect_error(vdist_chisquare_plot(3, xaxis_range = 1e10 * (1 + 1e-8), print_plot = FALSE),
               "xaxis_range must not exceed", fixed = TRUE)
})

test_that("fx15: chi-square windows retain the body and tiny mass is disclosed", {
  cases <- rbind(data.frame(df = 200, x = c(13, 59, 60, 65, 120)),
                 data.frame(df = 40:72, x = 13),
                 data.frame(df = 200, x = 61:120),
                 data.frame(df = 500, x = 280:350))
  for (i in seq_len(nrow(cases))) {
    df <- cases$df[i]; x <- cases$x[i]
    p <- vdist_chisquare_prob(x, df, "lower", FALSE)
    xr <- p$coordinates$limits$x
    expect_gte(pchisq(xr[2], df), .2)
    expect_true(xr[1] <= df - 2 && xr[2] >= df)
    expect_gt(max(p$data$y), .99 * dchisq(df - 2, df))
    # A ratio catches a missing polygon even for masses around 1e-80.
    expect_lt(abs(fx15_mass(p) / pchisq(x, df) - 1), 1e-6)
  }
  expect_match(vdist_chisquare_prob(13, 200, print_plot = FALSE)$labels$caption,
               "too small to display", fixed = TRUE)
  for (df in 1:30) for (prob in c(.05, .25, .5, .75, .95)) {
    x <- qchisq(prob, df)
    sd <- round(sqrt(2 * df), 3)
    old_hi <- if (x < 25) 25 else x + 3 * sd
    old_hi <- min(max(old_hi, x + sd, 1), max(qchisq(1e-9, df, lower.tail = FALSE), 1))
    p <- vdist_chisquare_prob(x, df, "lower", FALSE)
    expect_equal(p$coordinates$limits$x, c(0, old_hi), info = paste(df, prob))
  }
})

test_that("fx15: quantile-resolution diagnostics show both full endpoints", {
  # Deliberately nonfinite quantile reaches the diagnostic, bypassing neither
  # the real probability validator nor its separate M08 validation test.
  err <- tryCatch(vistributions:::vdist_check_quantile(Inf, stats::pt, .5,
                    "t quantile", "t"), error = conditionMessage)
  expect_match(err, "t quantile", fixed = TRUE)
  expect_match(err, "cannot be represented numerically", fixed = TRUE)
  expect_match(err, "1e-12 to 0.999999999999)", fixed = TRUE)
  # Controlled representable-tail failure cannot round its coverage to 100%.
  spec <- vistributions:::vdist_spec("t", df = .05)
  err <- tryCatch(vistributions:::vdist_cont_prep(spec, list(c(-Inf, qt(1e-10, .05))),
                     1e-10, c(-5, 5), "lower"), error = conditionMessage)
  expect_match(err, "representable tails do not cover the event", fixed = TRUE)
  expect_false(grepl("only 100%", err, fixed = TRUE))
})

test_that("fx15: one-decimal captions retain zeros and reproduce base R", {
  for (p in c(.84, .8, .99)) {
    f <- vistributions:::vdist_fmt_pct(p)
    expect_equal(f$text, sprintf("%.1f", 100 * p))
    expect_equal(f$tol, .0005)
  }
  cases <- list(
    list(vdist_normal_prob(10.99651, 10, 1, print_plot = FALSE), function(x) pnorm(x, 10, 1), "84.0%"),
    list(vdist_normal_prob(10.997, 10, 1, print_plot = FALSE), function(x) pnorm(x, 10, 1), "84.1%"),
    list(vdist_f_prob(1.0784, 500, 500, print_plot = FALSE), function(x) pf(x, 500, 500), "80.0%"),
    list(vdist_f_prob(1.078, 500, 500, print_plot = FALSE), function(x) pf(x, 500, 500), "79.9%"),
    list(vdist_f_perc(.8, 200, 200, print_plot = FALSE), function(x) pf(x, 200, 200), "80.0%"),
    list(vdist_f_perc(.8, 500, 500, print_plot = FALSE), function(x) pf(x, 500, 500), "80.0%"),
    list(vdist_f_perc(.99, 1e6, 1e6, print_plot = FALSE), function(x) pf(x, 1e6, 1e6), "99.0%"))
  for (case in cases) {
    s <- case[[1]]$labels$subtitle
    cut <- as.numeric(sub("P\\(X < ([^)]*)\\).*", "\\1", s))
    pct <- sub(".*= ", "", s)
    expect_equal(pct, case[[3]])
    expect_equal(pct, paste0(sprintf("%.1f", 100 * case[[2]](cut)), "%"))
    # A second call for the printed event must give the same percentage.
    expect_equal(vistributions:::vdist_pct_text(case[[2]](cut)), sub("%$", "", pct))
  }
})
