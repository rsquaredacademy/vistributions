#' Visualize f distribution
#'
#' @description Visualize how changes in degrees of freedom affect the
#' shape of the F distribution. Compute & visualize quantiles out of given
#' probability and probability from a given quantile.
#'
#' @param num_df Degrees of freedom associated with the numerator of f statistic.
#' @param den_df Degrees of freedom associated with the denominator of f statistic.
#' @param normal If \code{TRUE}, normal curve with same \code{mean} and
#' \code{sd} as the F distribution is drawn.
#' @param probs Probability value.
#' @param perc Quantile value.
#' @param type Lower tail or upper tail.
#' @param print_plot logical; if \code{TRUE}, prints the plot else returns a plot object.
#'
#' @examples
#' # visualize F distribution
#' vdist_f_plot()
#' vdist_f_plot(6, 10, normal = TRUE)
#'
#' # visualize probability from a given quantile
#' vdist_f_perc(0.95, 3, 30, 'lower')
#' vdist_f_perc(0.125, 9, 35, 'upper')
#'
#' # visualize quantiles out of given probability
#' vdist_f_prob(2.35, 5, 32)
#' vdist_f_prob(1.5222, 9, 35, type = "upper")
#'
#' @seealso \code{\link[stats]{FDist}}
#'
#' @export
#'
vdist_f_plot <- function(num_df = 4, den_df = 30, normal = FALSE, print_plot = TRUE) {

  check_numeric(num_df, "num_df")
  check_numeric(den_df, "den_df")
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
  check_numeric(den_df, "den_df")
  check_numeric(probs, "probs")
  check_range(probs, 0, 1, "probs")

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
  check_numeric(num_df, "num_df")
  check_numeric(den_df, "den_df")

  method <- match.arg(type)
  data   <- fprob_data_prep(perc, num_df, den_df, method)
  plot   <- fprob_plot_build(data, perc, num_df, den_df, method)

  if (print_plot) {
    print(plot)
  } else {
    return(plot)
  }

}

