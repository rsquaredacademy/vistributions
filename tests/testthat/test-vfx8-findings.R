# Round-8 regression (vfx8): the vertex list handed to the polygon must not
# contain two consecutive vertices closer than 1e-9 of the region's x range.
#
# The CI failure on Linux/macOS was exactly such a vertex: vdist_region_grid()
# builds the probability-spaced points with qfun(plogis(qlogis(p))), which is
# not required to round-trip exactly, so its first/last point can land one ulp
# inside the cutoff. unique() then keeps two vertices where Windows keeps one,
# and the snapshot of `vdist_t_prob(1.445, 7, 'interval')` gained an extra
# `265.66,407.84` (tests/testthat/test-visual.R, "t prob interval").
#
# vdist_region() now drops a grid point that is the same number as its
# neighbour to within 64 ulps, after the refinement, keeping the region's own
# endpoint. The comparison is relative to the coordinates, not to the width of
# the region, so a heavy tail (the t with df = 0.1 spans about 1e96) is not
# collapsed: that case is guarded below.

dark_layers <- function(p) {
  fill <- function(l) {
    f <- l$aes_params$fill
    if (is.null(f)) f <- l$geom_params$fill
    f
  }
  Filter(function(l) inherits(l$geom, "GeomPolygon") &&
           identical(fill(l), "#0000CD"), p$layers)
}

# The x values of the shaded density, without the two closing vertices that sit
# on the x axis at the ends of the region.
poly_x <- function(l) {
  x <- sort(l$data$x)
  x[-c(1, length(x))]
}

test_that("VFXX8-R1: shaded vertices are never near-duplicates of their neighbour", {
  cases <- list(
    # the CI case: an interval with two cutoffs at 1.445
    function() vdist_t_prob(1.445, 7, "interval", print_plot = FALSE),
    # a two-sided region with cutoffs at 3
    function() vdist_normal_prob(3, print_plot = FALSE),
    # a one-sided region whose cutoff is an F quantile of exactly 1
    function() vdist_f_prob(1, 5, 10, print_plot = FALSE),
    # a percentile cutoff on a normal with the default mean and sd
    function() vdist_normal_perc(.95, print_plot = FALSE)
  )
  for (f in cases) {
    p <- f()
    lays <- dark_layers(p)
    expect_gt(length(lays), 0)
    for (l in lays) {
      x   <- poly_x(l)
      rng <- diff(range(x))
      expect_gt(length(x), 50)
      expect_true(all(is.finite(x)))
      expect_gt(rng, 0)
      # strictly increasing: one vertex per x, no repeated coordinate
      expect_true(all(diff(x) > 0))
      # ... and no near-duplicate either: the drawn vertex list is separated by
      # more than 1e-9 of the region's own width
      expect_gt(min(diff(x)), 1e-9 * rng)
    }
  }
})

test_that("VFXX8-R2: dropping a duplicate vertex never collapses a heavy tail", {
  skip_on_cran()
  # The t with df = 0.1 has a region about 1e96 wide. A near-duplicate test
  # relative to the *width* of such a region would delete the whole grid (this
  # is what an earlier attempt did: the area became 4.05e13 instead of 0.95).
  p <- vdist_t_perc(.95, .1, print_plot = FALSE)
  lays <- dark_layers(p)
  expect_gt(length(lays), 0)
  for (l in lays) {
    expect_gt(nrow(l$data), 1000)
    x <- poly_x(l)
    expect_true(all(diff(x) > 0))
  }
  area <- sum(vapply(lays, function(l) {
    x <- l$data$x
    y <- l$data$y
    abs(sum((c(x[-1], x[1]) - x) * (y + c(y[-1], y[1])))) / 2
  }, numeric(1)))
  expect_lt(abs(area - .95), 1e-3)
})
