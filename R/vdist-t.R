#' Visualize t distribution
#'
#' Visualize how degrees of freedom affect the shape of t
#' distribution, visualize quantiles out of given probability and
#' probability from a given quantile.
#'
#' @param df Degrees of freedom.
#' @param probs Probability value.
#' @param perc Quantile value.
#' @param type Lower tail, upper tail, interval or both.
#' @param print_plot logical; if \code{TRUE}, prints the plot else returns a plot object.
#'
#' @examples
#' # visualize t distribution
#' vdist_t_plot()
#' vdist_t_plot(6)
#' vdist_t_plot(df = 8)
#'
#' # visualize quantiles out of given probability
#' vdist_t_perc(probs = 0.95, df = 4, type = 'lower')
#' vdist_t_perc(probs = 0.35, df = 4, type = 'upper')
#' vdist_t_perc(probs = 0.69, df = 7, type = 'both')
#'
#' # visualize probability from a given quantile
#' vdist_t_prob(2.045, 7, 'lower')
#' vdist_t_prob(0.945, 7, 'upper')
#' vdist_t_prob(1.445, 7, 'interval')
#' vdist_t_prob(1.6, 7, 'both')
#'
#' @seealso \code{\link[stats]{TDist}}
#'
#' @name vdist_t
NULL

#' @export
#' @rdname vdist_t
#'
vdist_t_plot <- function(df = 3, print_plot = TRUE) {

  check_numeric(df, "df")

  data <- tplot_data_prep(df)
  plot <- tplot_plot_build(data, df)

  if (print_plot) {
    print(plot)
  } else {
    return(plot)
  }

}

#' @rdname vdist_t
#' @export
#'
vdist_t_perc <- function(probs = 0.95, df = 4, type = c("lower", "upper", "both"), print_plot = TRUE) {

  check_numeric(probs, "probs")
  check_numeric(df, "df")
  check_range(probs, 0, 1, "probs")

  method  <- match.arg(type)
  data    <- tperc_data_prep(probs, df, method)
  plot    <- tperc_plot_build(data, probs, df, method)

  if (print_plot) {
    print(plot)
  } else {
    return(plot)
  }

}

#' @rdname vdist_t
#' @export
#'
vdist_t_prob <- function(perc = 1.6, df = 7, type = c("lower", "upper", "interval", "both"), print_plot = TRUE) {

  check_numeric(perc, "perc")
  check_numeric(df, "df")
  method <- match.arg(type)

  data <- tprob_data_prep(perc, df, method)
  plot <- tprob_plot_build(data, perc, df, method)

  if (print_plot) {
    print(plot)
  } else {
    return(plot)
  }

}

