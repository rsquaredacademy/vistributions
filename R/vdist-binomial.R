#' Visualize binomial distribution
#'
#' Visualize how changes in number of trials and the probability of
#' success affect the shape of the binomial distribution. Compute & visualize
#' probability from a given quantile and quantiles out of given probability.
#'
#' @param n Number of trials.
#' @param p Aggregate probability.
#' @param lib The library to be use for visualization. \code{ggplot2} is the
#' default; the other option is \code{plotly}.
#'
#' @examples
#' # visualize binomial distribution
#' vdist_binom_plot(10, 0.3)
#'
#' @seealso \code{\link[stats]{Binomial}}
#'
#' @importFrom magrittr %>%
#'
#' @export
#'
vdist_binom_plot <- function(n, p, lib = c("ggplot2", "plotly")) {

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

  use_lib <- match.arg(lib)

  if (use_lib == "ggplot2") {
    plot_data %>%
      ggplot2::ggplot() +
      ggplot2::geom_col(ggplot2::aes(x = n, y = df), fill = "blue") +
      ggplot2::ylab("Probability") + ggplot2::xlab("No. of success") +
      ggplot2::ggtitle(label = paste("Binomial Distribution: n =", n, ", p =", p),
              subtitle = paste("Mean =", bm, ", Std. Dev. =", bsd)) +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
            plot.subtitle = ggplot2::element_text(hjust = 0.5)) +
      ggplot2::scale_x_continuous(breaks = seq(0, n))
  } else if (use_lib == "plotly") {
    plotly::plot_ly(plot_data, x = ~n, y = ~df) %>%
      plotly::add_bars(color = I("blue")) %>%
      plotly::layout(title = paste("Binomial Distribution: n =", n, ", p =", p),
             xaxis = list(title = "No. of success"),
             yaxis = list(title = "Probability"),
             annotations = list(
               list(x = xn, y = 1.05, showarrow = F,
                    text = paste("Mean =", bm, ", Std. Dev. =", bsd),
                    xref='paper', yref='paper')))

  } else {
    stop("Please specify a valid library.", call. = FALSE)
  }

}




