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
#' @seealso \code{\link[stats]{Binomial}}
#'
#' @importFrom magrittr %>%
#'
#' @export
#'
vdist_binom_plot <- function(n, p) {

  if (!is.numeric(n)) {
    stop("n must be numeric/integer")
  }

  if (!is.numeric(p)) {
    stop("p must be numeric")
  }

  if ((p < 0) | (p > 1)) {
    stop("p must be between 0 and 1")
  }

  n   <- as.integer(n)
  x   <- seq(0, n, 1)
  xn  <- n / 40

  bm <-
    n %>%
    magrittr::multiply_by(p) %>%
    round(2)

  bsd <-
    1 %>%
    magrittr::subtract(p) %>%
    magrittr::multiply_by(bm) %>%
    sqrt(.) %>%
    round(2)

  data <- stats::dbinom(x, n, p)
  plot_data <- tibble::tibble(n = seq(0, n), df = data)

  plot_data %>%
    ggplot2::ggplot() +
    ggplot2::geom_col(ggplot2::aes(x = n, y = df), fill = "blue") +
    ggplot2::ylab("Probability") + ggplot2::xlab("No. of success") +
    ggplot2::ggtitle(label = paste("Binomial Distribution: n =", n, ", p =", p),
                     subtitle = paste("Mean =", bm, ", Std. Dev. =", bsd)) +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                   plot.subtitle = ggplot2::element_text(hjust = 0.5)) +
    ggplot2::scale_x_continuous(breaks = seq(0, n))

}


#' @rdname vdist_binom_plot
#' @export
#'
vdist_binom_prob <- function(n, p, s, type = c("lower", "upper", "exact", "interval")) {

  method  <- match.arg(type)

  if ((p < 0) | (p > 1)) {
    stop("p must be between 0 and 1")
  }

  if (!is.numeric(n)) {
    stop("n must be numeric/integer")
  }

  if (!is.numeric(p)) {
    stop("p must be numeric")
  }

  if (!is.numeric(s)) {
    stop("s must be numeric/integer")
  }

  if (method == "interval") {
    if (length(s) != 2) {
      stop("Please specify an interval for s")
    }
  }

  if (any(s > n)) {
    stop("s must be less than or equal to n")
  }

  n   <- as.integer(n)
  s   <- as.integer(s)
  x   <- seq(0, n, 1)

  bm <-
    n %>%
    magrittr::multiply_by(p) %>%
    round(2)

  bsd <-
    1 %>%
    magrittr::subtract(p) %>%
    magrittr::multiply_by(bm) %>%
    sqrt(.) %>%
    round(2)


  if (method == "lower") {
    k <- round(pbinom(s, n, p), 3)
    cols <- ifelse(cumsum(round(dbinom(x, n, p), 3)) <= k, "#0000CD", "#6495ED")
  } else if (method == "upper") {
    k <- round(1 - pbinom((s - 1), n, p), 3)
    cols <- ifelse(cumsum(round(dbinom(x, n, p), 3)) >= k, "#0000CD", "#6495ED")
  } else if (method == "exact") {
    k <- pbinom(s, n, p) - pbinom((s - 1), n, p)
    cols <- ifelse(round(dbinom(x, n, p), 5) == round(k, 5), "#0000CD", "#6495ED")
  } else {
    k1 <- pbinom((s[1] - 1), n, p)
    k2 <- pbinom(s[2], n, p)
    k  <- pbinom(s[2], n, p) - pbinom((s[1] - 1), n, p)
    cols <- ifelse((round(cumsum(dbinom(x, n, p)), 6) > round(k1, 6) &
                      round(cumsum(dbinom(x, n, p)), 6) <= round(k2, 6)), "#0000CD", "#6495ED")
  }

  data <- stats::dbinom(x, n, p)
  plot_data <- tibble::tibble(n = seq(0, n), df = data)



  pp <-
    plot_data %>%
    ggplot2::ggplot() +
    ggplot2::geom_col(ggplot2::aes(x = n, y = df), fill = cols) +
    ggplot2::ylab("Probability") +
    ggplot2::xlab(paste("No. of success\n", "Mean =", bm, ", Std. Dev. =", bsd)) +
    ggplot2::scale_x_continuous(breaks = seq(0, n)) +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                   plot.subtitle = ggplot2::element_text(hjust = 0.5))

  if (method == "lower") {
    pp +
      ggplot2::ggtitle(label = paste("Binomial Distribution: n =", n, ", p =", p),
                       subtitle = paste("P(X) <=", s, "=", round(k, 3)))
  } else if (method == "upper") {
    pp +
      ggplot2::ggtitle(label = paste("Binomial Distribution: n =", n, ", p =", p),
                       subtitle = paste("P(X) >=", s, "=", round(k, 3)))
  } else if (method == "exact") {
    pp +
      ggplot2::ggtitle(label = paste("Binomial Distribution: n =", n, ", p =", p),
                       subtitle = paste("P(X) =", s, "=", round(k, 3)))
  } else {
    pp +
      ggplot2::ggtitle(label = paste("Binomial Distribution: n =", n, ", p =", p),
                       subtitle = paste0("P(", s[1], " <= X <= ", s[2], ")", " = ", round(k, 3)))
  }

}

