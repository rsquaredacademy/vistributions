#' Visualize binomial distribution
#'
#' Visualize how changes in number of trials and the probability of
#' success affect the shape of the binomial distribution. Compute & visualize
#' probability from a given quantile and quantiles out of given probability.
#'
#' @param n Number of trials (a single non-negative whole number).
#' @param p Probability of success in each trial.
#' @param s Number of successes: whole numbers in \code{[0, n]} only. Zero and
#'   \code{n} are counts of successes like any other; use \code{pbinom()} for
#'   an arbitrary threshold outside that range. For \code{type = "interval"},
#'   two values in increasing order.
#' @param type Lower/upper/exact/interval.
#' @param tp Tail probability for quantile calculation, strictly between 0 and
#'   1. The cutoff follows \code{qbinom()}, including its numerical fuzz
#'   at cumulative jumps. For \code{type = "upper"} the bars with
#'   \code{x > k} are shaded.
#' @param print_plot logical; if \code{TRUE}, prints the plot else returns a plot object.
#'
#' @details Probabilities below 1e-10\% (including underflows) are shown as
#' < 1e-10\%, with the corresponding upper display > 99.9999999999\%.
#'
#' Every bar is one count while \code{n + 1} counts fit in the plotting
#' grid (2001 bars). A wider support is drawn in approximately 2001 bins
#' of counts; event boundaries can split up to two bins, each bar showing the exact probability of its bin, with a caption
#' that discloses the grouping; the grid therefore has a bounded number of
#' points whatever \code{n} is. Probabilities, quantiles and shaded masses always
#' come from \code{dbinom()}, \code{pbinom()} and \code{qbinom()}.
#'
#' @return An object of class \code{ggplot} (invisibly if \code{print_plot = TRUE}).
#'
#' @family binomial visualization
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
	check_print_plot(print_plot)

	check_numeric(n)
	n <- check_count(n)
	check_numeric(p, "p")
	check_scalar(p, "p")
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
	check_print_plot(print_plot)

	check_numeric(n)
	n <- check_count(n)
	check_numeric(p, "p")
	check_scalar(p, "p")
	check_range(p)
	check_numeric(s, "s")

	method  <- match.arg(type)

	if (method == "interval") {
		if (length(s) != 2) {
			stop("Please specify an interval for s.", call. = FALSE)
		}
	} else if (length(s) != 1) {
		stop("s must be a single value unless type = \"interval\".", call. = FALSE)
	}

	if (any(is.na(s)) || any(!is.finite(s)) || any(abs(s - round(s)) > 1e-8)) {
		stop("s must be whole numbers (counts of successes).", call. = FALSE)
	}

	s <- round(s)

	if (any(s < 0) || any(s > n)) {
		stop(paste0("s must be a whole number of successes between 0 and n (n = ",
		            n, ")."), call. = FALSE)
	}

	if (method == "interval" && s[1] > s[2]) {
		stop("The interval for s must be given in increasing order.", call. = FALSE)
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
	check_print_plot(print_plot)

	check_numeric(n)
	n <- check_count(n)
	check_numeric(p, "p")
	check_scalar(p, "p")
	check_numeric(tp, "tp")
	check_scalar(tp, "tp")
	check_range(p)
	check_range(tp, 0, 1, "tp")
	check_open_range(tp, "tp")

	method <- match.arg(type)
	data   <- bperc_data_prep(n, p, tp, method)
	plot   <- bperc_plot_build(data, method, n, p, tp)
 
	if (print_plot) {
		print(plot)
	} else {
		return(plot)
	}
}
