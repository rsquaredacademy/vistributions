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

	n    <- as.integer(n)
	x    <- seq(0, n, 1)
	xn   <- n / 40
	bm   <- round(n * p, 2)
	bsd  <- round(sqrt((1 - p) * bm) , 2)
	data <- dbinom(x, n, p)

	plot_data <- data.frame(n = seq(0, n), df = data)

	pp <-
		ggplot(plot_data) +
		geom_col(aes(x = n, y = df), fill = "blue") +
		ylab("Probability") + xlab("No. of success") +
		ggtitle(label = paste("Binomial Distribution: n =", n, ", p =", p),
										 subtitle = paste("Mean =", bm, ", Std. Dev. =", bsd)) +
		theme(plot.title = element_text(hjust = 0.5),
									 plot.subtitle = element_text(hjust = 0.5)) +
		scale_x_continuous(breaks = seq(0, n))

	if (print_plot) {
		print(pp)
	} else {
		return(pp)
	}

}

#' @rdname vdist_binom_plot
#' @export
#'
vdist_binom_prob <- function(n = 10, p = 0.3, s = 4,
														 type = c("lower", "upper", "exact", "interval"),
														 print_plot = TRUE) {

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

	n   <- as.integer(n)
	s   <- as.integer(s)
	x   <- seq(0, n, 1)
	bm  <- round(n * p, 2)
	bsd <- round(sqrt((1 - p) * bm), 2)

	if (method == "lower") {
		k    <- round(pbinom(s, n, p), 3)
		cols <- ifelse(cumsum(round(dbinom(x, n, p), 3)) <= k, "#0000CD", "#6495ED")
	} else if (method == "upper") {
		k    <- round(1 - pbinom((s - 1), n, p), 3)
		cols <- ifelse(cumsum(round(dbinom(x, n, p), 3)) >= k, "#0000CD", "#6495ED")
	} else if (method == "exact") {
		k    <- pbinom(s, n, p) - pbinom((s - 1), n, p)
		cols <- ifelse(round(dbinom(x, n, p), 5) == round(k, 5), "#0000CD", "#6495ED")
	} else {
		k1   <- pbinom((s[1] - 1), n, p)
		k2   <- pbinom(s[2], n, p)
		k    <- pbinom(s[2], n, p) - pbinom((s[1] - 1), n, p)
		cols <- ifelse((round(cumsum(dbinom(x, n, p)), 6) > round(k1, 6) &
											round(cumsum(dbinom(x, n, p)), 6) <= round(k2, 6)), "#0000CD", "#6495ED")
	}

	data      <- dbinom(x, n, p)
	plot_data <- data.frame(n = seq(0, n), df = data)

	pp <-
		ggplot(plot_data) +
		geom_col(aes(x = n, y = df), fill = cols) +
		ylab("Probability") +
		xlab(paste("No. of success\n", "Mean =", bm, ", Std. Dev. =", bsd)) +
		scale_x_continuous(breaks = seq(0, n)) +
		theme(plot.title = element_text(hjust = 0.5),
									 plot.subtitle = element_text(hjust = 0.5))

	if (method == "lower") {
		pp +
			ggtitle(label = paste("Binomial Distribution: n =", n, ", p =", p),
											 subtitle = paste("P(X) <=", s, "=", round(k, 3)))
	} else if (method == "upper") {
		pp +
			ggtitle(label = paste("Binomial Distribution: n =", n, ", p =", p),
											 subtitle = paste("P(X) >=", s, "=", round(k, 3)))
	} else if (method == "exact") {
		pp +
			ggtitle(label = paste("Binomial Distribution: n =", n, ", p =", p),
											 subtitle = paste("P(X) =", s, "=", round(k, 3)))
	} else {
		pp +
			ggtitle(label = paste("Binomial Distribution: n =", n, ", p =", p),
											 subtitle = paste0("P(", s[1], " <= X <= ", s[2], ")", " = ", round(k, 3)))
	}

	if (print_plot) {
		print(pp)
	} else {
		return(pp)
	}

}

#' @rdname vdist_binom_plot
#' @export
#'
vdist_binom_perc <- function(n = 10, p = 0.5, tp = 0.05, type = c("lower", "upper"),
														 print_plot = TRUE) {

	check_numeric(n)
	check_numeric(p, "p")
	check_numeric(tp, "tp")
	check_range(p)
	check_range(tp, 0, 0.5, "tp")
	
	n      <- as.integer(n)
	method <- match.arg(type)
	x      <- seq(0, n, 1)

	if (method == "lower") {
		k    <- round(qbinom(tp, n, p), 3)
		cols <- ifelse(cumsum(dbinom(x, n, p)) <= pbinom(k, n, p), "#0000CD", "#6495ED")
	} else {
		k    <- round(qbinom(tp, n, p, lower.tail = F), 3)
		cols <- ifelse(cumsum(dbinom(x, n, p)) > pbinom((k + 1), n, p), "#0000CD", "#6495ED")
	}

	data      <- dbinom(x, n, p)
	plot_data <- data.frame(n = seq(0, n), df = data)

	pp <-
		ggplot(plot_data) +
		geom_col(aes(x = n, y = df), fill = cols) +
		ylab("Probability") + xlab("No. of success") +
		scale_x_continuous(breaks = seq(0, n)) +
		theme(plot.title = element_text(hjust = 0.5),
									 plot.subtitle = element_text(hjust = 0.5))


	if (method == "lower") {
		pp +
			ggtitle(label = paste("Binomial Distribution: n =", n, ", p =", p),
				subtitle = paste0("P(X <= ", k, ") <= ", tp, ", but P(X <= ", (k + 1),
				") > ", tp))
	} else {
		pp +
			ggtitle(label = paste("Binomial Distribution: n =", n, ", p =", p),
				subtitle = paste0("P(X >= ", (k + 1), ") <= ", tp, ", but P(X >= ", k,
				") > ", tp))
	}

	if (print_plot) {
		print(pp)
	} else {
		return(pp)
	}
}
