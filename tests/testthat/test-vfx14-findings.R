# Round 14 findings (vist-fx14) -------------------------------------------------

subtitle14 <- function(p) p$labels$subtitle

rd_text14 <- function(name) {
  db <- tryCatch(tools::Rd_db("vistributions"), error = function(e) NULL)
  if (!is.null(db) && !is.null(db[[name]])) {
    return(paste(utils::capture.output(tools::Rd2txt(db[[name]])), collapse = "\n"))
  }
  path <- testthat::test_path("..", "..", "man", name)
  if (file.exists(path)) return(paste(readLines(path), collapse = "\n"))
  ""
}

# M4.2 ----------------------------------------------------------------------

test_that("M4.2: .onAttach() leaves .Random.seed untouched", {
  # Exercise the real body with interactive() mocked only in a copied closure.
  e <- new.env(parent = asNamespace("vistributions"))
  assign("interactive", function() TRUE, envir = e)
  onattach <- vistributions:::.onAttach
  environment(onattach) <- e

  for (seed in c(1, 2, 3, 42, 2024)) {
    set.seed(seed)
    before <- .Random.seed
    invisible(suppressMessages(onattach()))
    expect_identical(.Random.seed, before, info = paste("seed", seed))
  }

  # A session without a seed must not gain one either.
  if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
    rm(".Random.seed", envir = globalenv())
  }
  invisible(suppressMessages(onattach()))
  expect_false(exists(".Random.seed", envir = globalenv(), inherits = FALSE))

  # The next draw after attaching is the one the caller's seed prescribes.
  set.seed(99)
  expected <- runif(1)
  set.seed(99)
  invisible(suppressMessages(onattach()))
  expect_identical(runif(1), expected)
})

# L1.3 ----------------------------------------------------------------------

test_that("L1.3: the vignette describes p and tp the way the help does", {
  path <- testthat::test_path("..", "..", "vignettes",
                              "introduction-to-vistributions.Rmd")
  if (!file.exists(path)) skip("vignette source is not available")
  lines <- readLines(path, warn = FALSE)
  text <- paste(lines, collapse = "\n")

  expect_match(text, "- p: the probability of success in each trial", fixed = TRUE)
  expect_match(text, "- tp: the tail probability", fixed = TRUE)
  expect_false(grepl("aggregated probability of multiple trials", text,
                     fixed = TRUE))
  expect_false(grepl("tp: the probability of success in a trial", text,
                     fixed = TRUE))

  # the help page keeps the same definitions
  expect_match(rd_text14("vdist_binom_plot.Rd"),
               "Probability of success in each trial")
  expect_match(rd_text14("vdist_binom_plot.Rd"), "Tail probability")
})

# H4.3 ----------------------------------------------------------------------

test_that("H4.3: a near-certain binomial tail is a bound, not certainty", {
  p <- vdist_binom_prob(1000, .01, 1, "upper", print_plot = FALSE)
  expect_equal(pbinom(0, 1000, .01, lower.tail = FALSE), 0.9999568,
               tolerance = 1e-7)
  expect_equal(subtitle14(p), "P(X >= 1) > 0.999")

  # an event that really is certain keeps the equality
  expect_equal(subtitle14(vdist_binom_prob(10, .3, 10, "lower", print_plot = FALSE)),
               "P(X <= 10) = 1.000")

  # ordinary probabilities are unchanged
  expect_equal(subtitle14(vdist_binom_prob(10, .3, 4, "lower", print_plot = FALSE)),
               "P(X <= 4) = 0.850")
  expect_equal(subtitle14(vdist_binom_prob(19, .4, 3, "exact", print_plot = FALSE)),
               "P(X = 3) = 0.017")

  # the formatter itself: every bound it prints is true, and below 1
  expect_equal(vistributions:::vdist_fmt_binom_prob(0.9995), "> 0.999")
  expect_equal(vistributions:::vdist_fmt_binom_prob(0.9994), "0.999")
  expect_equal(vistributions:::vdist_fmt_binom_prob(1), "1.000")
  expect_equal(vistributions:::vdist_fmt_binom_prob(1 - 1e-13),
               "> 99.9999999999%")
})

# L1.1 ----------------------------------------------------------------------

test_that("L1.1: t percentile annotations do not overprint on a wide window", {
  p <- vdist_t_perc(.95, df = .1, "lower", print_plot = FALSE)
  built <- ggplot2::ggplot_build(p)
  xr <- built$layout$panel_params[[1]]$x.range
  text_x <- unlist(lapply(built$data, function(d) {
    if ("label" %in% names(d) && "x" %in% names(d)) d$x else NULL
  }))
  expect_length(text_x, 2)
  expect_gt(diff(range(text_x)) / diff(xr), 0.02)

  # both labels are inside the panel and name the two tails
  expect_true(all(text_x >= xr[1] & text_x <= xr[2]))
  labels <- unlist(lapply(built$data, function(d) {
    if ("label" %in% names(d)) as.character(d$label) else NULL
  }))
  expect_setequal(labels, c("95%", "5%"))

  # the upper tail is separated too
  pu <- vdist_t_perc(.02, df = .1, "upper", print_plot = FALSE)
  bu <- ggplot2::ggplot_build(pu)
  tur <- bu$layout$panel_params[[1]]$x.range
  tu <- unlist(lapply(bu$data, function(d) {
    if ("label" %in% names(d) && "x" %in% names(d)) d$x else NULL
  }))
  expect_gt(diff(range(tu)) / diff(tur), 0.02)

  # ordinary windows keep the historical +-0.3 placement
  p7 <- vdist_t_perc(.95, df = 7, "lower", print_plot = FALSE)
  pc <- vistributions:::tperc_data_prep(.95, 7, "lower")$pc
  expect_equal(ggplot2::layer_data(p7, 2)$x, pc - 0.3)
  expect_equal(ggplot2::layer_data(p7, 3)$x, pc + 0.3)
})

# L1.2 ----------------------------------------------------------------------

test_that("L1.2: the chi-square mean marker sits on the axis", {
  for (df in c(.5, 1, 2, 3, 5)) {
    p <- vdist_chisquare_plot(df, xaxis_range = 25, print_plot = FALSE)
    expect_equal(ggplot2::layer_data(p, 3)$y, 0, info = paste("df", df))
    expect_equal(ggplot2::layer_data(p, 3)$x, df, info = paste("df", df))
  }
  # df = 2 is the case where min(y) was small but nonzero
  expect_equal(dchisq(2, 2), 0.1839397, tolerance = 1e-6)
  p <- vdist_chisquare_plot(2, xaxis_range = 2, print_plot = FALSE)
  expect_equal(ggplot2::layer_data(p, 3)$y, 0)
})

# M2.1 ----------------------------------------------------------------------

test_that("M2.1: t degrees of freedom are floored at a documented value", {
  msg <- "df must be at least 0.05"
  for (f in list(function() vdist_t_plot(.01, print_plot = FALSE),
                 function() vdist_t_perc(.95, .01, print_plot = FALSE),
                 function() vdist_t_prob(1.5, .01, print_plot = FALSE))) {
    expect_error(f(), msg, fixed = TRUE)
  }
  # the message names df and the range rather than blaming the probability
  err <- tryCatch(vdist_t_prob(1.5, .001, print_plot = FALSE),
                  error = function(e) conditionMessage(e))
  expect_match(err, "df", fixed = TRUE)
  expect_match(err, "0.05", fixed = TRUE)
  expect_false(grepl("less extreme probability", err, fixed = TRUE))

  # the floor itself is ordinary: probabilities, percentiles and the density
  expect_s3_class(vdist_t_prob(1.5, 0.05, print_plot = FALSE), "ggplot")
  expect_s3_class(vdist_t_perc(.95, 0.05, print_plot = FALSE), "ggplot")
  expect_s3_class(vdist_t_plot(0.05, print_plot = FALSE), "ggplot")
  expect_lt(abs(vistributions:::tperc_data_prep(.95, .05, "lower")$event_prob - .95),
            1e-6)
})

# M2.2 ----------------------------------------------------------------------

test_that("M2.2: chi-square xaxis_range has a documented maximum", {
  expect_error(vdist_chisquare_plot(3, xaxis_range = 1e100, print_plot = FALSE),
               "xaxis_range must not exceed", fixed = TRUE)
  err <- tryCatch(vdist_chisquare_plot(3, xaxis_range = 1e40, print_plot = FALSE),
                  error = function(e) conditionMessage(e))
  expect_match(err, "1e\\+10")
  expect_false(grepl("density is too concentrated", err, fixed = TRUE))

  # the documented maximum itself is drawn
  expect_s3_class(vdist_chisquare_plot(3, xaxis_range = 1e10, print_plot = FALSE),
                  "ggplot")
  expect_s3_class(vdist_chisquare_plot(200, print_plot = FALSE), "ggplot")
})

# H7.2b ---------------------------------------------------------------------

test_that("H7.2b: a chi-square probability view keeps the body of the distribution", {
  p <- vdist_chisquare_prob(13, df = 200, "lower", print_plot = FALSE)
  built <- ggplot2::ggplot_build(p)
  xr <- built$layout$panel_params[[1]]$x.range
  # the body at 200 is inside the panel and drawn at its true height
  expect_true(xr[1] <= 198 && xr[2] >= 202)
  expect_gt(max(p$data$y), 0.5 * dchisq(198, 200))
  expect_equal(dchisq(198, 200), 0.02003074, tolerance = 1e-6)
  # the event itself is disclosed, not silently dropped
  expect_match(p$labels$subtitle, "< 1e-10%")

  # and the offset view is what the event needs: the shaded mass is unchanged
  dark <- 0
  for (i in seq_along(p$layers)) {
    l <- p$layers[[i]]
    d <- ggplot2::layer_data(p, i)
    if ((identical(l$aes_params$fill, "#0000CD") ||
         identical(l$geom_params$fill, "#0000CD")) &&
        all(c("x", "y") %in% names(d))) {
      dark <- dark + vistributions:::vdist_area(d$x, d$y)
    }
  }
  expect_equal(dark, pchisq(13, 200), tolerance = 1e-6)

  # ordinary windows are unchanged
  expect_equal(ggplot2::ggplot_build(
    vdist_chisquare_prob(13, df = 5, print_plot = FALSE)
  )$layout$panel_params[[1]]$x.range, c(0, 25))
  expect_equal(ggplot2::ggplot_build(
    vdist_chisquare_prob(13, df = 10, print_plot = FALSE)
  )$layout$panel_params[[1]]$x.range, c(0, 25))
})
