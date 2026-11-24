# These three plots formerly had separate macOS SVGs because individual
# refinement vertices moved by fractions of a pixel across platforms. Compare
# the actual layer values and the shaded event mass to base R instead.
check_density_layer <- function(plot, density) {
  d <- ggplot2::layer_data(plot, 1)
  expect_equal(d$y, density(d$x), tolerance = 1e-8)
}

check_dark_mass <- function(plot, probability) {
  index <- which(vapply(plot$layers, function(layer)
    inherits(layer$geom, "GeomPolygon") &&
      identical(layer$aes_params$fill, "#0000CD"), logical(1)))
  mass <- sum(vapply(index, function(i) {
    d <- ggplot2::layer_data(plot, i)
    vistributions:::vdist_area(d$x, d$y)
  }, numeric(1)))
  expect_lt(abs(mass / probability - 1), 1e-6)
}

test_that("normal density and central band are platform independent", {
  g <- vdist_normal_plot(print_plot = FALSE)
  check_density_layer(g, dnorm)
  check_dark_mass(g, diff(pnorm(c(-1,1))))
})

test_that("F upper probability is platform independent", {
  g <- vdist_f_prob(1.5222,9,35,type="upper",print_plot=FALSE)
  check_density_layer(g, function(x) df(x,9,35))
  check_dark_mass(g, pf(1.5222,9,35,lower.tail=FALSE))
})

test_that("t central percentile is platform independent", {
  g <- vdist_t_perc(.69,7,type="both",print_plot=FALSE)
  check_density_layer(g, function(x) dt(x,7))
  check_dark_mass(g, .69)
})
