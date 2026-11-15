#' Visualize f distribution
#'
#' @description Visualize how changes in degrees of freedom affect the
#' shape of the F distribution. Compute & visualize quantiles out of given
#' probability and probability from a given quantile.
#'
#' @details The F density must not be too concentrated to draw: the central
#' 99.9998\% probability mass must span a finite interval of at least 0.001.
#' More concentrated densities (for example \code{num_df = den_df = 1e9}) stop
#' with a clear error instead of returning an inaccurate shaded area. A tail
#' that does not converge at double precision (for example \code{den_df = 0.01})
#' is reported as a tail-overflow error, separately from concentration.
#' Every probability, cutoff and caption is computed from the F density,
#' distribution and quantile functions, never from the drawn grid: the shading
#' polygon starts at the support and spans the whole event, and its own area is
#' checked against the exact event probability (within 1e-3). Where the density
#' is unbounded at zero it is capped on the display axis only; the area check
#' and the printed probability always use the unclipped density.
#' Very wide heavy-tail windows are drawn on a symmetric log axis so the shaded
#' event stays visible. Captions use one rounding rule and are true at the
#' precision they print. Quantiles are refined by root finding when the
#' \code{stats} quantile does not invert its own distribution function, so equal
#' degrees of freedom up to at least \code{1e8} plot accurately.
#'
#' @param num_df Degrees of freedom associated with the numerator of f statistic
#'   (at least 1; need not be a whole number).
#' @param den_df Degrees of freedom associated with the denominator of f statistic
#'   (positive; need not be a whole number).
#' @param normal If \code{TRUE}, normal curve with same \code{mean} and
#' \code{sd} as the F distribution is drawn. The overlay is omitted with a
#' message when \code{den_df <= 4} (the variance is undefined).
#' @param probs Probability value, strictly between 0 and 1 and within
#' \code{[1e-6, 1 - 1e-6]}.
#' @param perc Quantile value.
#' @param type Lower tail or upper tail.
#' @param print_plot logical; if \code{TRUE}, prints the plot else returns a plot object.
#'
#' @return An object of class \code{ggplot} (invisibly if \code{print_plot = TRUE}).
#'
#' @family f visualization
#'
#' @examples
#' # visualize F distribution
#' vdist_f_plot()
#' vdist_f_plot(6, 10, normal = TRUE)
#'
#' # visualize quantiles out of given probability
#' vdist_f_perc(0.95, 3, 30, 'lower')
#' vdist_f_perc(0.125, 9, 35, 'upper')
#'
#' # visualize probability from a given quantile
#' vdist_f_prob(2.35, 5, 32)
#' vdist_f_prob(1.5222, 9, 35, type = "upper")
#'
#' @seealso \code{\link[stats]{FDist}}
#'
#' @export
#'
vdist_f_plot <- function(num_df = 4, den_df = 30, normal = FALSE, print_plot = TRUE) {

  check_numeric(num_df, "num_df")
  check_scalar(num_df, "num_df")
  check_numeric(den_df, "den_df")
  check_scalar(den_df, "den_df")
  check_positive(num_df, "num_df")
  check_positive(den_df, "den_df")
  vdist_check_f_num_df(num_df)
  check_logical(normal)

  data <- fplot_data_prep(num_df, den_df)
  plot <- fplot_plot_build(data, num_df, den_df, normal)

  if (print_plot) {
    print(plot)
  } else {
    return(plot)
  }

}

#' @rdname vdist_f_plot
#' @export
#'
vdist_f_perc <- function(probs = 0.95, num_df = 3, den_df = 30, type = c("lower", "upper"), print_plot = TRUE) {

  check_numeric(num_df, "num_df")
  check_scalar(num_df, "num_df")
  check_numeric(den_df, "den_df")
  check_scalar(den_df, "den_df")
  check_positive(num_df, "num_df")
  check_positive(den_df, "den_df")
  vdist_check_f_num_df(num_df)
  check_numeric(probs, "probs")
  check_scalar(probs, "probs")
  check_range(probs, 0, 1, "probs")
  check_probability(probs, "probs", "f")

  method <- match.arg(type)
  data   <- fperc_data_prep(probs, num_df, den_df, method)
  plot   <- fperc_plot_build(data, probs, num_df, den_df, method)

  if (print_plot) {
    print(plot)
  } else {
    return(plot)
  }

}

#' @rdname vdist_f_plot
#' @export
#'
vdist_f_prob <- function(perc = 2.35, num_df = 5, den_df = 32, type = c("lower", "upper"), print_plot = TRUE) {

  check_numeric(perc, "perc")
  check_scalar(perc, "perc")
  check_numeric(num_df, "num_df")
  check_scalar(num_df, "num_df")
  check_numeric(den_df, "den_df")
  check_scalar(den_df, "den_df")
  check_positive(num_df, "num_df")
  check_positive(den_df, "den_df")
  vdist_check_f_num_df(num_df)

  method <- match.arg(type)
  data   <- fprob_data_prep(perc, num_df, den_df, method)
  plot   <- fprob_plot_build(data, perc, num_df, den_df, method)

  if (print_plot) {
    print(plot)
  } else {
    return(plot)
  }

}
