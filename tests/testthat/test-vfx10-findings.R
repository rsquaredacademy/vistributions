# Round-10 fixes: every finding of the astra audit (AUDIT-vistributions-022.md)
# that hits ordinary input is fixed and asserted against base R. Resolution
# extremes are rejected with a documented limit. Each test compares a number,
# a curve or a caption with p*/q*/d*, not merely "no error".

subtitle <- function(p) p$labels$subtitle
panel <- function(p) ggplot2::ggplot_build(p)$layout$panel_params[[1]]
panel_xy <- function(p, i) {
  ggplot2::ggplot_build(p)$layout$coord$transform(
    ggplot2::layer_data(p, i), panel(p)
  )
}
dark_polygons <- function(p) {
  Filter(function(l) identical(l$aes_params$fill, "#0000CD") ||
           identical(l$geom_params$fill, "#0000CD"), p$layers)
}
dark_area <- function(p) {
  sum(vapply(dark_polygons(p), function(l) {
    vistributions:::vdist_area(l$data$x, l$data$y)
  }, numeric(1)))
}
dark_bars <- function(p) {
  d <- ggplot2::layer_data(p, 1)
  sum(d$y[d$fill == "#0000CD"])
}
caption_pct <- function(s) {
  m <- regmatches(s, regexec("= ([0-9.eE+-]+)%", s))[[1]]
  if (length(m) >= 2) {
    return(as.numeric(m[2]))
  }
  as.numeric(regmatches(s, regexec("^([0-9.eE+-]+)%", s))[[1]][2])
}

# H01 -----------------------------------------------------------------------
test_that("H01: a binomial bin never straddles the event boundary", {
  g <- vdist_binom_prob(10000, .01, 100, "exact", print_plot = FALSE)
  expect_equal(dark_bars(g), dbinom(100, 10000, .01), tolerance = 1e-12)

  g <- vdist_binom_prob(10000, .01, 100, "lower", print_plot = FALSE)
  expect_equal(dark_bars(g), pbinom(100, 10000, .01), tolerance = 1e-12)

  g <- vdist_binom_prob(10000, .01, c(100, 100), "interval", print_plot = FALSE)
  expect_equal(dark_bars(g), dbinom(100, 10000, .01), tolerance = 1e-12)

  g <- vdist_binom_prob(10000, .5, 5000, "lower", print_plot = FALSE)
  expect_equal(dark_bars(g), pbinom(5000, 10000, .5), tolerance = 1e-12)

  g <- vdist_binom_prob(10000, .5, 5000, "upper", print_plot = FALSE)
  expect_equal(dark_bars(g), pbinom(4999, 10000, .5, lower.tail = FALSE),
               tolerance = 1e-12)

  tp <- .5
  k  <- qbinom(tp, 10000, .01, lower.tail = FALSE)
  g  <- vdist_binom_perc(10000, .01, tp, "upper", print_plot = FALSE)
  expect_equal(dark_bars(g), pbinom(k, 10000, .01, lower.tail = FALSE),
               tolerance = 1e-12)
})

# H02 -----------------------------------------------------------------------
test_that("H02: a right-tail grouped bin keeps its representable mass", {
  d <- ggplot2::layer_data(vdist_binom_plot(4000, .5, print_plot = FALSE), 1)
  row <- d[d$x == 2261.5, ]
  expect_equal(nrow(row), 1L)
  expect_equal(row$y, dbinom(2261, 4000, .5) + dbinom(2262, 4000, .5),
               tolerance = 1e-12)
  expect_gt(row$y, 0)

  # the upper tail from any bin is the survival difference, not 1 - 1
  i <- which(d$xmin <= 2300 & d$xmax > 2300)[1]
  tail <- sum(d$y[i:nrow(d)])
  expect_equal(tail, pbinom(d$xmin[i] - 1, 4000, .5, lower.tail = FALSE),
               tolerance = 1e-12)
})

# H03 -----------------------------------------------------------------------
test_that("H03: a binomial caption is rounded once, from the exact value", {
  p <- vdist_binom_prob(74, .5, 26, "exact", print_plot = FALSE)
  expect_equal(subtitle(p), "P(X = 26) = 0.003")
  expect_equal(round(dbinom(26, 74, .5), 3), 0.003)

  p <- vdist_binom_prob(74, .5, c(26, 26), "interval", print_plot = FALSE)
  expect_equal(subtitle(p), "P(26 <= X <= 26) = 0.003")
})

# H04 -----------------------------------------------------------------------
test_that("H04: a near-one binomial percentile is resolved in the small tail", {
  tp <- 1 - 2^-53
  p  <- vdist_binom_perc(1000, .5, tp, "lower", print_plot = FALSE)
  expect_match(subtitle(p), "P\\(X <= 629\\) >= 0\\.9999999999999999")
  expect_equal(dark_bars(p), pbinom(629, 1000, .5), tolerance = 1e-12)
  # the defining inequalities hold in the small tail
  expect_lt(pbinom(628, 1000, .5), tp)
  expect_gte(pbinom(629, 1000, .5), tp)
})

# H05 -----------------------------------------------------------------------
test_that("H05: the printed cutoff reproduces the printed percentage", {
  cases <- list(
    list(p = vdist_normal_prob(1.00051, print_plot = FALSE),
         pf = function(v) 100 * pnorm(v)),
    list(p = vdist_normal_prob(1.00051, type = "upper", print_plot = FALSE),
         pf = function(v) 100 * pnorm(v, lower.tail = FALSE)),
    list(p = vdist_t_prob(1.0051, 5.5, print_plot = FALSE),
         pf = function(v) 100 * pt(v, 5.5)),
    list(p = vdist_chisquare_prob(1.00251, 2.5, print_plot = FALSE),
         pf = function(v) 100 * pchisq(v, 2.5)),
    list(p = vdist_f_prob(1.00126, 5, 10, print_plot = FALSE),
         pf = function(v) 100 * pf(v, 5, 10))
  )
  for (cs in cases) {
    s    <- subtitle(cs$p)
    shown <- caption_pct(s)
    cut  <- as.numeric(regmatches(s, regexec("P\\(X [<>] ([^)]+)\\)", s))[[1]][2])
    # the printed cutoff, at the printed percentage's own precision
    expect_lt(abs(cs$pf(cut) - shown), 0.05 + 1e-9)
  }
})

# H06 -----------------------------------------------------------------------
test_that("H06: small tail probabilities are computed on the right scale", {
  s <- subtitle(vdist_normal_prob(c(9, 10), print_plot = FALSE))
  expect_equal(caption_pct(s),
               100 * (pnorm(9, lower.tail = FALSE) - pnorm(10, lower.tail = FALSE)),
               tolerance = 1e-2)

  s <- subtitle(vdist_normal_prob(c(8, 8.1), print_plot = FALSE))
  expect_equal(caption_pct(s),
               100 * (pnorm(8, lower.tail = FALSE) - pnorm(8.1, lower.tail = FALSE)),
               tolerance = 1e-2)

  s <- subtitle(vdist_t_prob(1e-17, 10, "interval", print_plot = FALSE))
  expect_equal(caption_pct(s),
               100 * integrate(function(x) dt(x, 10), -1e-17, 1e-17)$value,
               tolerance = 1e-2)

  labels <- function(p) {
    lapply(p$layers, function(l) l$aes_params$label)
  }
  # the complementary annotation is the opposite tail, never 1 - p
  g <- vdist_normal_prob(9, print_plot = FALSE)
  expect_true(any(vapply(labels(g), function(x) {
    !is.null(x) && grepl("^1\\.13e-17%$", x)
  }, logical(1))))

  g <- vdist_t_prob(10, 200, print_plot = FALSE)
  expect_true(any(vapply(labels(g), function(x) {
    !is.null(x) && grepl("^1\\.19e-17%$", x)
  }, logical(1))))

  g <- vdist_chisquare_prob(100, 1, print_plot = FALSE)
  expect_true(any(vapply(labels(g), function(x) {
    !is.null(x) && grepl("^1\\.52e-21%$", x)
  }, logical(1))))

  g <- vdist_f_prob(30, 5, 200, print_plot = FALSE)
  expect_true(any(vapply(labels(g), function(x) {
    !is.null(x) && grepl("^1\\.09e-20%$", x)
  }, logical(1))))
})

# H07 -----------------------------------------------------------------------
test_that("H07: a tail event keeps its polygon and its true disclosure", {
  # a finite event at 1e-10 must be drawn, not silently dropped
  p <- vdist_normal_perc(1e-10, print_plot = FALSE)
  expect_gte(length(dark_polygons(p)), 1L)
  expect_equal(dark_area(p), 1e-10, tolerance = 1e-4)

  p <- vdist_normal_prob(7, type = "upper", print_plot = FALSE)
  expect_gte(length(dark_polygons(p)), 1L)
  expect_equal(dark_area(p), pnorm(7, lower.tail = FALSE), tolerance = 1e-4)

  p <- vdist_t_perc(1e-10, 1, "upper", print_plot = FALSE)
  expect_gte(length(dark_polygons(p)), 1L)
  expect_equal(dark_area(p), 1e-10, tolerance = 1e-4)

  # a cutoff wholly outside the panel is disclosed against the event itself
  p <- vdist_normal_prob(12, type = "upper", print_plot = FALSE)
  expect_match(p$labels$caption, "of the shaded region lies outside")
  expect_equal(caption_pct(p$labels$caption), 100, tolerance = 1e-6)

  # the old formula divided by the truncated mass, not the event mass
  p <- vdist_normal_perc(2e-9, print_plot = FALSE)
  expect_true(is.null(p$labels$caption) ||
                caption_pct(p$labels$caption) < 1)
})

# H08 -----------------------------------------------------------------------
test_that("H08: an unrepresentable central interval is a resolution error", {
  expect_error(vdist_normal_perc(.0001, 1, 1e-13, "both", print_plot = FALSE),
               "cannot be represented numerically")
  expect_error(vdist_normal_perc(.0001, 1, 1e-13, "both", print_plot = FALSE),
               "1e-10 to 1 - 1e-10")
})

# H09 -----------------------------------------------------------------------
test_that("H09: a normal whose panel range collapses is rejected", {
  expect_error(vdist_normal_perc(.5, 1, 1e-14, print_plot = FALSE),
               "documented limit of 3e-14")
  expect_error(vdist_normal_prob(1, 1, 1e-14, print_plot = FALSE),
               "documented limit of 3e-14")

  # one step above the limit still draws distinct panel positions
  p <- vdist_normal_perc(.5, 1, 1e-13, print_plot = FALSE)
  expect_gt(diff(range(panel_xy(p, 4)$x)), 0)
})

# H10 -----------------------------------------------------------------------
test_that("H10: the visibility strip marks its own event at any scale", {
  for (s in c(1e-200, 1e200)) {
    p <- vdist_normal_prob(c(.99, 1.01) * s, sd = s, print_plot = FALSE)
    strip <- ggplot2::layer_data(p, length(p$layers))
    expect_true(strip$xmin[1] <= 1.01 * s && strip$xmax[1] >= .99 * s)
    expect_true(strip$xmin[1] >= .9 * s && strip$xmax[1] <= 1.1 * s)
  }
})

# H11 -----------------------------------------------------------------------
test_that("H11: a whole view in a far tail is still a curve", {
  p <- vdist_chisquare_plot(200, print_plot = FALSE)
  d <- ggplot2::layer_data(p, 1)
  expect_gt(nrow(d), 100)
  expect_equal(approx(d$x, d$y, 12.5)$y, dchisq(12.5, 200), tolerance = 1e-6)
  expect_equal(d$y[d$x == 25], dchisq(25, 200), tolerance = 1e-6)

  p <- vdist_chisquare_prob(5, 200, print_plot = FALSE)
  expect_gte(nrow(ggplot2::layer_data(p, 1)), 100)
})

# H12 -----------------------------------------------------------------------
test_that("H12: the density axis is capped only at a real singularity", {
  p <- vdist_f_perc(.95, 1e6, 1e6, print_plot = FALSE)
  expect_gte(panel(p)$y.range[2], 1.2 * df(1, 1e6, 1e6))
  expect_equal(max(ggplot2::layer_data(p, 1)$y), df(1, 1e6, 1e6),
               tolerance = 1e-2)

  p <- vdist_t_perc(.001, 1, "upper", print_plot = FALSE)
  expect_gte(panel(p)$y.range[2], 1.2 * dt(0, 1))
})

# H13 -----------------------------------------------------------------------
test_that("H13: the F moments do not overflow an intermediate", {
  p <- vdist_f_plot(200, 1e306, TRUE, print_plot = FALSE)
  expect_match(p$labels$x, "Std Dev\\. = 0\\.1")
  overlay <- ggplot2::layer_data(p, 4)
  expect_equal(max(overlay$y), dnorm(1, 1, 0.1), tolerance = 1e-6)

  a <- 200; b <- 1e306; m <- b / (b - 2)
  sd <- sqrt((2 * m^2 / a) * ((a + b - 2) / (b - 4)))
  expect_equal(sd, 0.1, tolerance = 1e-12)
})

# H14 -----------------------------------------------------------------------
test_that("H14: the chi-square mean marker is the exact df", {
  p <- vdist_chisquare_plot(3.14159, TRUE, print_plot = FALSE)
  expect_equal(ggplot2::layer_data(p, 3)$x, 3.14159)
  expect_equal(integrate(function(x) x * dchisq(x, 3.14159), 0, Inf)$value,
               3.14159, tolerance = 1e-5)
})

# H15 -----------------------------------------------------------------------
test_that("H15: a finite peak at the smallest scales is drawn, not flat", {
  p <- vdist_normal_plot(0, 2.3e-309, print_plot = FALSE)
  expect_true(all(is.finite(panel(p)$y.range)))
  expect_gt(diff(range(panel_xy(p, 2)$y)), 0.5)

  p <- vdist_normal_prob(0, sd = 3e-309, print_plot = FALSE)
  expect_equal(subtitle(p), "P(X < 0) = 50%")
})

# M01 -----------------------------------------------------------------------
test_that("M01: a segment midpoint does not overflow", {
  p <- vdist_normal_prob(1e308, 1e308, 1e306, print_plot = FALSE)
  expect_equal(subtitle(p), "P(X < 1e+308) = 50%")
  expect_equal(dark_area(p), pnorm(1e308, 1e308, 1e306), tolerance = 1e-3)

  p <- vdist_normal_perc(.5, 1e308, 1e306, "both", print_plot = FALSE)
  expect_equal(dark_area(p), .5, tolerance = 1e-3)
})

# M02 -----------------------------------------------------------------------
test_that("M02: finite distant cutoffs keep their exact mass", {
  cases <- list(
    list(p = vdist_normal_prob(c(-1e300, 1e300), print_plot = FALSE),
         truth = diff(pnorm(c(-1e300, 1e300)))),
    list(p = vdist_t_prob(1e100, 7, print_plot = FALSE),
         truth = pt(1e100, 7)),
    list(p = vdist_chisquare_prob(1e100, 4, print_plot = FALSE),
         truth = pchisq(1e100, 4)),
    list(p = vdist_f_prob(1e100, 5, 10, print_plot = FALSE),
         truth = pf(1e100, 5, 10))
  )
  for (cs in cases) {
    expect_equal(dark_area(cs$p), cs$truth, tolerance = 1e-3)
    expect_equal(caption_pct(subtitle(cs$p)), 100 * cs$truth,
                 tolerance = 1e-6)
  }
})

# M03 -----------------------------------------------------------------------
test_that("M03: a decimal comma never reaches the numeric parser", {
  old <- options(OutDec = ",")
  on.exit(options(old), add = TRUE)
  expect_equal(subtitle(vdist_normal_perc(.000123, print_plot = FALSE)),
               "P(X < -3.666) = 1.23e-02%")
  expect_s3_class(vdist_t_perc(.000123, 7, print_plot = FALSE), "ggplot")
  expect_s3_class(vdist_chisquare_perc(.000123, 7, print_plot = FALSE), "ggplot")
  expect_s3_class(vdist_f_perc(.000123, 5, 10, print_plot = FALSE), "ggplot")
  expect_s3_class(vdist_normal_prob(c(-3.5, -3.4), print_plot = FALSE), "ggplot")
  expect_s3_class(vdist_t_prob(.000123, 7, "interval", print_plot = FALSE),
                  "ggplot")
})

# M04 -----------------------------------------------------------------------
test_that("M04: ticks on a translated normal are distinct", {
  p <- vdist_normal_plot(1, 1e-13, print_plot = FALSE)
  labels <- panel(p)$x$get_labels()
  expect_equal(length(unique(labels)), length(labels))
  breaks <- panel(p)$x$get_breaks()
  expect_equal(length(unique(round(breaks / 1e-13))), length(breaks))
})

# M05 -----------------------------------------------------------------------
test_that("M05: print_plot is validated by every plotting export", {
  calls <- list(
    function(x) vdist_normal_plot(print_plot = x),
    function(x) vdist_normal_perc(print_plot = x),
    function(x) vdist_normal_prob(print_plot = x),
    function(x) vdist_t_plot(print_plot = x),
    function(x) vdist_t_perc(print_plot = x),
    function(x) vdist_t_prob(print_plot = x),
    function(x) vdist_chisquare_plot(print_plot = x),
    function(x) vdist_chisquare_perc(print_plot = x),
    function(x) vdist_chisquare_prob(print_plot = x),
    function(x) vdist_f_plot(print_plot = x),
    function(x) vdist_f_perc(print_plot = x),
    function(x) vdist_f_prob(print_plot = x),
    function(x) vdist_binom_plot(print_plot = x),
    function(x) vdist_binom_prob(print_plot = x),
    function(x) vdist_binom_perc(print_plot = x)
  )
  for (f in calls) {
    expect_error(f(NA), "print_plot must be a single nonmissing logical")
    expect_error(f(0), "print_plot must be a single nonmissing logical")
    expect_error(f("yes"), "print_plot must be a single nonmissing logical")
  }
})

# M06 -----------------------------------------------------------------------
test_that("M06: a success count must be inside [0, n]", {
  expect_error(vdist_binom_prob(4, .5, -1, print_plot = FALSE),
               "between 0 and n")
  expect_error(vdist_binom_prob(4, .5, 5, print_plot = FALSE),
               "between 0 and n")
  expect_error(vdist_binom_prob(4, .5, c(-1, 2), "interval", print_plot = FALSE),
               "between 0 and n")
  # both ends are ordinary counts
  expect_equal(subtitle(vdist_binom_prob(4, .5, 0, print_plot = FALSE)),
               "P(X <= 0) = 0.062")
  expect_equal(subtitle(vdist_binom_prob(4, .5, 4, print_plot = FALSE)),
               "P(X <= 4) = 1")
})

# M07 -----------------------------------------------------------------------
test_that("M07: the test entry points do not require the suggested packages", {
  runner <- readLines(testthat::test_path("..", "testthat.R"), warn = FALSE)
  expect_true(any(grepl("requireNamespace\\(\"testthat\"", runner)))
  visual <- readLines(testthat::test_path("test-visual.R"), warn = FALSE)
  expect_true(any(grepl("skip_if_not_installed\\(\"vdiffr\"", visual)))
})

# M08 -----------------------------------------------------------------------
test_that("M08: the resolution error states the real probability range", {
  err <- tryCatch(vdist_t_perc(1e-10, .02, print_plot = FALSE),
                  error = function(e) conditionMessage(e))
  expect_match(err, "0\\.9999999999")
  expect_match(err, "1e-10")
})

# L01 -----------------------------------------------------------------------
test_that("L01: the binomial inequality is inside P()", {
  p <- vdist_binom_prob(10, .3, 4, "upper", print_plot = FALSE)
  expect_equal(subtitle(p), "P(X >= 4) = 0.350")
  expect_equal(pbinom(3, 10, .3, lower.tail = FALSE), 0.3503893,
               tolerance = 1e-6)

  expect_equal(subtitle(vdist_binom_prob(10, .3, 4, "lower", print_plot = FALSE)),
               "P(X <= 4) = 0.850")
  expect_equal(subtitle(vdist_binom_prob(10, .3, 4, "exact", print_plot = FALSE)),
               "P(X = 4) = 0.200")
})
