#' Visualize normal distribution
#'
#' Visualize how changes in mean and standard deviation affect the
#' shape of the normal distribution. Compute & visualize quantiles out of given
#' probability  and probability from a given quantile.
#'
#' @param mean Mean of the normal distribution.
#' @param sd Standard deviation of the normal distribution.
#'
#' @examples
#' # visualize normal distribution
#' vdist_normal_plot()
#' vdist_normal_plot(mean = 2, sd = 0.6)
#'
#' @seealso \code{\link[stats]{Normal}}
#'
#' @export
#'
vdist_normal_plot <- function(mean = 0, sd = 1) {
  
  if (!is.numeric(mean)) {
    stop("mean must be numeric/integer")
  }

  if (!is.numeric(sd)) {
    stop("sd must be numeric/integer")
  }

  if (sd < 0) {
    stop("sd must be positive")
  }

  x   <- vdist_xax(mean)
  l   <- vdist_seql(mean, sd)
  col <- c("#0000CD", "#4682B4", "#6495ED", "#4682B4", "#6495ED")
  l1  <- c(3, 2, 1, 5, 6)
  l2  <- c(5, 3, 2, 6, 7)

  xm <- vdist_xmm(mean, sd)

  plot_data <- tibble::tibble(x = x, y = stats::dnorm(x, mean, sd))
  
  gplot <-
    plot_data %>%
    ggplot2::ggplot() +
    ggplot2::geom_line(ggplot2::aes(x = x, y = y)) +
    ggplot2::xlab('') + ggplot2::ylab('') +
    ggplot2::ggtitle(label = "Normal Distribution",
      subtitle = paste("Mean:", mean, "     Standard Deviation:", sd)) +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                   plot.subtitle = ggplot2::element_text(hjust = 0.5))

  ll <- l[3:9]

  for (i in seq_len(length(l1))) {
    poly_data <- vdist_pol_cord(ll[l1[i]], ll[l2[i]], mean, sd)
    gplot <- 
      gplot +
      ggplot2::geom_polygon(data = poly_data, mapping = ggplot2::aes(x = x, y = y), fill = col[i])
  }

  print(gplot)
}


vdist_xax <- function(mean) {
  xl <- mean - 3
  xu <- mean + 3
  x <- seq(xl, xu, 0.01)
  return(x)
}


vdist_seql <- function(mean, sd) {
  lmin <- mean - (5 * sd)
  lmax <- mean + (5 * sd)
  l <- seq(lmin, lmax, sd)
  return(l)
}

vdist_pol_cord <- function(l1, l2, mean, sd) {
  x <- c(l1, seq(l1, l2, 0.01), l2)
  y <- c(0, stats::dnorm(seq(l1, l2, 0.01), mean, sd), 0)
  data <- tibble::tibble(x = x, y = y)
  return(data)
}