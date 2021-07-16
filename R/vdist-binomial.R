#' Visualize binomial distribution
#'
#' Visualize how changes in number of trials and the probability of
#' success affect the shape of the binomial distribution. Compute & visualize
#' probability from a given quantile and quantiles out of given probability.
#'
#' @param n Number of trials.
#' @param p Aggregate probability.
#' @param s Number of success.
#' @param type Lower/upper/exact/interval.
#' @param tp Probability of success in a trial.
#' @param print_plot logical; if \code{TRUE}, prints the plot else returns a plot object.
#'
#' @examples
#' # visualize binomial distribution
#' vdist_binom_plot(10, 0.3)
#'
#' # visualize probability from a given quantile
#' vdist_binom_prob(10, 0.3, 4, type = 'exact')
#' vdist_binom_prob(10, 0.3, 4, type = 'lower')
#' vdist_binom_prob(10, 0.3, 4, type = 'upper')
#' vdist_binom_prob(10, 0.3, c(4, 6), type = 'interval')
#'
#'
#' # visualize quantiles out of given probability
#' vdist_binom_perc(10, 0.5, 0.05)
#' vdist_binom_perc(10, 0.5, 0.05, "upper")
#'
#' @seealso \code{\link[stats]{Binomial}}
#'
#'
#' @export
#'
vdist_binom_plot <- function(n = 10, p = 0.3, print_plot = TRUE) {

	check_numeric(n)
	check_numeric(p, "p")
	check_range(p)

	data <- bplot_data_prep(n, p)
	plot <- bplot_plot_build(data, n, p)

	if (print_plot) {
		print(plot)
	} else {
		return(plot)
	}

}

#' @rdname vdist_binom_plot
#' @export
#'
vdist_binom_prob <- function(n = 10, p = 0.3, s = 4, type = c("lower", "upper", "exact", "interval"), print_plot = TRUE) {

	check_range(p)
	check_numeric(n)
	check_numeric(p, "p")
	check_numeric(s, "s")

	method  <- match.arg(type)

	if (method == "interval") {
		if (length(s) != 2) {
			stop("Please specify an interval for s.", call. = FALSE)
		}
	}

	if (any(s > n)) {
		stop("s must be less than or equal to n.", call. = FALSE)
	}

	data <- bprob_data_prep(n, p, s, method)
	plot <- bprob_plot_build(data, method, n, p, s)

	if (print_plot) {
		print(plot)
	} else {
		return(plot)
	}

}

#' @rdname vdist_binom_plot
#' @export
#'
vdist_binom_perc <- function(n = 10, p = 0.5, tp = 0.05, type = c("lower", "upper"), print_plot = TRUE) {

	check_numeric(n)
	check_numeric(p, "p")
	check_numeric(tp, "tp")
	check_range(p)
	check_range(tp, 0, 0.5, "tp")

	method <- match.arg(type)
	data   <- bperc_data_prep(n, p, tp, method)
	plot   <- bperc_plot_build(data, method, n, p, tp)
 
	if (print_plot) {
		print(plot)
	} else {
		return(plot)
	}
}
