# Round-7 fixes: the remaining findings of the Gemini re-audit
# (docs-dev/pkg-vist-rx6-audit.md) and the large-scale grid audit.
#
#   VFXX6-M1  a near-zero were mean is not a resolution failure
#   VFXX6-M2  a sub-pixel shaded region is marked and disclosed
#   VFXX6-L2  a normal with sd = 3e307 draws without a NaN area
#   grid      every plotting grid is bounded at any scale

subtitle <- function(p) p$labels$subtitle

`%||%` <- function(x, y) if (is.null(x)) y else x

dark_polygons <- function(p) {
  Filter(function(l) identical(l$aes_params$fill, "#0000CD"), p$layers)
}

dark_area <- function(p) {
  sum(vapply(dark_polygons(p), function(l) vistributions:::vdist_area(l$data$x, l$data$y),
             numeric(1)))
}

marker_layers <- function(p) {
  Filter(function(l) inherits(l$geom, "GeomRect") &&
           !inherits(l$geom, "GeomCol") &&
           identical(l$aes_params$fill, "#0000CD"), p$layers)
}

# Dark-blue (#0000CD) pixels on a real png device, the measure the audit used.
#
# Two things made this measure platform-dependent without measuring anything
# different:
#
#  * the device. `png()` defaults to `type = "quartz"` on macOS, which colour
#    manages and antialiases, so the fill of #0000CD never lands within 0.02 of
#    that exact colour and even an ordinary region counted 0 pixels there.
#    Cairo rasterises the same colours on every platform (ragg is used when the
#    build has no cairo). If the platform offers neither, the device cannot be
#    compared at all, so the test skips instead of reporting an invisible
#    region.
#  * the classification. An antialiased #0000CD pixel keeps r == g and is blue
#    dominated; the light fills (#6495ED and #4682B4) have g > r, and white or
#    grey panel pixels are not blue dominated at all. Counting hue dominance
#    instead of an exact RGB box therefore measures the same thing on every
#    rasteriser. The thresholds are chosen so that a pixel counts only from
#    about half coverage: #0000CD over white at coverage a is (1-a, 1-a,
#    1-0.196a), so b - max(r, g) = 0.8a and requiring it above 0.40 is a > 0.5.
#    A region narrower than one pixel therefore still counts 0, where an exact
#    match would have been fooled by neither platform.
#
# The panel also contains other pure-blue ink: the density curve and the
# percentage annotations are drawn in #0000CD (or #0000FF). Those are not the
# shaded region, so they are removed before rendering and only the dark-filled
# polygons/rectangles are drawn. Otherwise a plot whose region was entirely
# invisible would still count curve and glyph pixels and the check would pass
# for the wrong reason. The panel limits are fixed by coord_cartesian() in the
# builders, so dropping layers does not move the pixels that remain.
dark_pixels <- function(p, width = 900, height = 500) {
  skip_if_not_installed("png")
  open_raster <- function(what) {
    # ragg first: it needs no X11. On macOS capabilities("cairo") can be TRUE
    # while the cairo module fails to load without XQuartz (CI 41c2473), and
    # png(type = "cairo") then writes no file at all.
    if (requireNamespace("ragg", quietly = TRUE)) {
      ragg::agg_png(what, width = width, height = height)
    } else if (isTRUE(capabilities("cairo"))) {
      suppressWarnings(
        grDevices::png(what, width = width, height = height, type = "cairo")
      )
    } else {
      skip("no ragg or cairo png device to measure pixels on")
    }
  }
  f <- tempfile(fileext = ".png")
  on.exit(unlink(f), add = TRUE)

  keep <- vapply(p$layers, function(l) {
    inherits(l$geom, c("GeomPolygon", "GeomRect")) &&
      identical(l$aes_params$fill, "#0000CD")
  }, logical(1))
  p$layers <- p$layers[keep]
  # the axis titles are printed in #0000CD as well ("Mean = ... Std Dev. = ...")
  p$labels[c("title", "subtitle", "caption", "x", "y")] <- NULL

  open_raster(f)
  print(p)
  grDevices::dev.off()
  # keep a temporary device open for any later plot that prints without one
  if (grDevices::dev.cur() == 1L) {
    grDevices::pdf(file.path(tempdir(), "vistributions-test-plots.pdf"))
  }
  if (!file.exists(f) || file.size(f) == 0) {
    skip("the png device could not write a file on this platform")
  }
  img <- png::readPNG(f)
  r <- img[, , 1]
  g <- img[, , 2]
  b <- img[, , 3]
  # a fill pixel: blue clearly above red and green, red and green equal (the
  # fill is #0000CD, r = g = 0), and covered by at least half a pixel
  sum(b - pmax(r, g) > 0.40 & abs(r - g) < 0.10)
}

# ---------------------------------------------------------------------------
# VFXX6-M1: sd / |mean| overflows for a near-zero mean; that is infinitely
# resolvable, not unresolvable.
# ---------------------------------------------------------------------------
test_that("a near-zero normal mean is drawn, not rejected (VFXX6-M1)", {
  p <- vdist_normal_plot(mean = 1e-308, sd = 10, print_plot = FALSE)
  expect_s3_class(p, "ggplot")

  pc <- vdist_normal_perc(.95, mean = 1e-308, sd = 10, print_plot = FALSE)
  expect_s3_class(pc, "ggplot")
  expect_equal(subtitle(pc), "P(X < 16.449) = 95.0%")

  pu <- vdist_normal_perc(.95, mean = 1e-308, sd = 10, type = "upper",
                          print_plot = FALSE)
  expect_equal(subtitle(pu), "P(X > -16.449) = 95.0%")

  pb <- vdist_normal_prob(16.45, mean = 1e-308, sd = 10, print_plot = FALSE)
  expect_s3_class(pb, "ggplot")
  expect_match(subtitle(pb), "= 95.0%")

  # the drawn probability is the probability the caption states
  v <- as.numeric(sub("^P\\(X < ([^)]+)\\).*$", "\\1", subtitle(pc)))
  expect_lt(abs(pnorm(v, 1e-308, 10) - .95), 1e-4)
  expect_lt(abs(dark_area(pc) - .95), 1e-3)

  # the same distribution in other units has the same picture and percentage
  for (k in c(1e3, 1e-3)) {
    pk <- vdist_normal_perc(.95, mean = 1e-308 * k, sd = 10 * k,
                            print_plot = FALSE)
    expect_match(subtitle(pk), "= 95.0%$")
    expect_lt(abs(dark_area(pk) - .95), 1e-3)
  }
})

test_that("a genuinely unresolvable normal is still rejected (VFXX6-M1)", {
  for (sd in c(1e-15, 1e-16)) {
    expect_error(vdist_normal_plot(mean = 1, sd = sd, print_plot = FALSE),
                 "sd / \\|mean\\| must be at least 1e-10")
    expect_error(vdist_normal_perc(.95, mean = 1, sd = sd, print_plot = FALSE),
                 "sd / \\|mean\\| must be at least 1e-10")
    expect_error(vdist_normal_prob(1, mean = 1, sd = sd, print_plot = FALSE),
                 "sd / \\|mean\\| must be at least 1e-10")
  }
  # the ratio underflows to zero for a huge mean: the message must stay true
  expect_error(vdist_normal_plot(mean = 1e300, sd = 1e-10, print_plot = FALSE),
               "sd / \\|mean\\| must be at least 1e-10")
})

# ---------------------------------------------------------------------------
# VFXX6-M2: lower tails of a low-df chi-square collapse to zero device pixels.
# ---------------------------------------------------------------------------
test_that("a sub-pixel chi-square lower tail is visible on the device (VFXX6-M2)", {
  cases <- list(
    function() vdist_chisquare_perc(.05, .1, print_plot = FALSE),
    function() vdist_chisquare_perc(.50, .1, print_plot = FALSE),
    function() vdist_chisquare_perc(.05, .2, print_plot = FALSE),
    function() vdist_chisquare_prob(qchisq(.05, .1), .1, print_plot = FALSE)
  )
  for (f in cases) {
    p <- f()
    # the exact region is still the event polygon ...
    expect_gt(dark_area(p), 0)
    # ... and a visible marker plus a true caption disclose the sub-pixel size
    expect_length(marker_layers(p), 1)
    expect_match(gsub("[[:space:]]+", " ", p$labels$caption), "narrower than one pixel")
    expect_match(gsub("[[:space:]]+", " ", p$labels$caption), "marked by the blue strip")
    # At least 100 pixels, not merely one: the marker strip is ~3.6 device
    # pixels wide over most of the data height (900 x 500 png), which paints
    # ~900 #0000CD pixels; the sub-pixel event polygon alone paints 0-3 even
    # at 50% coverage. So this still fails when the marker is not drawn.
    expect_gt(dark_pixels(p), 100)
  }
})

test_that("an ordinary shaded region is not marked as sub-pixel (VFXX6-M2)", {
  ordinary <- list(
    vdist_normal_perc(.95, print_plot = FALSE),
    vdist_t_perc(.95, 5, print_plot = FALSE),
    vdist_f_perc(.95, 5, 10, print_plot = FALSE),
    vdist_chisquare_perc(.95, .1, print_plot = FALSE),
    vdist_chisquare_perc(.95, .1, "upper", print_plot = FALSE)
  )
  for (p in ordinary) {
    expect_length(marker_layers(p), 0)
    expect_false(grepl("narrower than one pixel", p$labels$caption %||% ""))
    expect_gt(dark_pixels(p), 0)
  }
})

# ---------------------------------------------------------------------------
# VFXX6-L2: sd = 3e307 overflowed the shoelace closing edge to NaN.
# ---------------------------------------------------------------------------
test_that("an extreme normal scale is rejected truthfully, not with a NaN area (VFXX6-L2)", {
  # mean +/- 4 sd ends are finite at sd = 3e307, but the window (8 sd) is not:
  # the drawing would rescale by an overflowing difference and emit an empty
  # panel, so the input is rejected with a true message instead.
  expect_error(vdist_normal_perc(.95, sd = 3e307, print_plot = FALSE),
               "spans more than the double range")
  expect_error(vdist_normal_prob(qnorm(.95, sd = 3e307), sd = 3e307, print_plot = FALSE),
               "spans more than the double range")
  expect_error(vdist_normal_plot(sd = 3e307, print_plot = FALSE),
               "sd must be below about 2.2e307")
  expect_false(grepl("area", tryCatch(vdist_normal_perc(.95, sd = 3e307, print_plot = FALSE),
                                      error = function(e) conditionMessage(e))))

  # just below the limit the same distribution draws, with a finite area
  p <- vdist_normal_perc(.95, sd = 2e307, print_plot = FALSE)
  expect_s3_class(p, "ggplot")
  expect_true(is.finite(dark_area(p)))
  expect_lt(abs(dark_area(p) - .95), 1e-3)
  expect_gt(dark_pixels(p), 0)

  # the shoelace sum itself stays finite for a polygon wider than the double
  # range (this is what produced NaN before the closing edge was skipped)
  x <- c(-1.68e308, -1e307, 4.93e307)
  y <- c(0, 1.3e-308, 0)
  expect_true(is.finite(vistributions:::vdist_area(x, y)))
  expect_gt(vistributions:::vdist_area(x, y), 0)
})

# ---------------------------------------------------------------------------
# Large-scale grids: building and printing must stay bounded and fast.
# ---------------------------------------------------------------------------
test_that("every grid is bounded and fast at large scale", {
  cases <- list(
    normal_plot  = function() vdist_normal_plot(1e6, 2e5, print_plot = FALSE),
    normal_perc  = function() vdist_normal_perc(.95, 1e6, 2e5, print_plot = FALSE),
    normal_upper = function() vdist_normal_perc(.05, 1e6, 2e5, "upper", print_plot = FALSE),
    normal_prob  = function() vdist_normal_prob(1.2e6, 1e6, 2e5, print_plot = FALSE),
    normal_both  = function() vdist_normal_prob(c(0.8e6, 1.4e6), 1e6, 2e5, print_plot = FALSE),
    t_plot       = function() vdist_t_plot(7, print_plot = FALSE),
    t_perc       = function() vdist_t_perc(.95, 7, print_plot = FALSE),
    t_prob       = function() vdist_t_prob(1e6, 7, print_plot = FALSE),
    t_prob_up    = function() vdist_t_prob(1e6, 7, "upper", print_plot = FALSE),
    t_big_df     = function() vdist_t_perc(.95, 1e6, print_plot = FALSE),
    chisq_plot   = function() vdist_chisquare_plot(1e6, print_plot = FALSE),
    chisq_perc   = function() vdist_chisquare_perc(.95, 1e6, print_plot = FALSE),
    chisq_prob   = function() vdist_chisquare_prob(1e6, 1e6, print_plot = FALSE),
    chisq_range  = function() vdist_chisquare_plot(5, xaxis_range = 1e6, print_plot = FALSE),
    f_plot       = function() vdist_f_plot(1e6, 1e6, print_plot = FALSE),
    f_perc       = function() vdist_f_perc(.95, 1e6, 1e6, print_plot = FALSE),
    f_prob       = function() vdist_f_prob(1e6, 1e6, 1e6, print_plot = FALSE),
    binom_plot   = function() vdist_binom_plot(1e6, .5, print_plot = FALSE),
    binom_prob   = function() vdist_binom_prob(1e6, .5, 5e5, print_plot = FALSE),
    binom_perc   = function() vdist_binom_perc(1e6, .5, .05, print_plot = FALSE)
  )
  f <- tempfile(fileext = ".png")
  on.exit(unlink(f), add = TRUE)
  for (nm in names(cases)) {
    t0 <- proc.time()[["elapsed"]]
    p <- cases[[nm]]()
    grDevices::png(f, width = 720, height = 480)
    print(p)
    grDevices::dev.off()
    elapsed <- proc.time()[["elapsed"]] - t0
    built <- ggplot2::ggplot_build(p)$data
    rows <- vapply(built, nrow, integer(1))
    expect_lte(max(rows), 5000, label = nm)
    expect_lt(elapsed, 10, label = nm)
  }
})

test_that("a grouped binomial grid still carries the exact total mass", {
  p <- vdist_binom_plot(1e6, .5, print_plot = FALSE)
  expect_true(vistributions:::bplot_data_prep(1e6, .5)$binned)
  expect_equal(sum(p$data$df), 1, tolerance = 1e-8)
  expect_match(gsub("[[:space:]]+", " ", p$labels$caption), "Bars are grouped")

  # individual counts are unchanged while the support fits, so ordinary
  # teaching plots are untouched
  d <- vistributions:::bplot_data_prep(10, .3)
  expect_false(d$binned)
  expect_equal(d$plot_data$df, dbinom(0:10, 10, .3))
  expect_equal(d$plot_data$w, rep(1, 11))
})

# ---------------------------------------------------------------------------
# The guarantees in NEWS and the help pages match the behaviour.
# ---------------------------------------------------------------------------
test_that("the documented guarantees match the fixed behaviour", {
  news_file <- system.file("NEWS.md", package = "vistributions")
  if (!nzchar(news_file)) news_file <- test_path("..", "..", "NEWS.md")
  news <- paste(readLines(news_file), collapse = "\n")
  expect_match(news, "near-zero mean")
  expect_match(news, "narrower than one pixel")
  expect_match(news, "3e307")
  expect_match(news, "bounded")

  rd <- function(name) {
    path <- test_path("..", "..", "man", name)
    if (file.exists(path)) return(paste(readLines(path), collapse = "\n"))
    db <- tryCatch(tools::Rd_db("vistributions"), error = function(e) NULL)
    if (!is.null(db) && !is.null(db[[name]])) {
      return(paste(utils::capture.output(tools::Rd2txt(db[[name]])), collapse = "\n"))
    }
    ""
  }
  expect_match(rd("vdist_normal_plot.Rd"), "near-zero mean")
  expect_match(rd("vdist_chisquare_plot.Rd"), "occupy a\\s+pixel")
  expect_match(rd("vdist_binom_plot.Rd"), "bounded")
})
