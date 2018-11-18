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
#'
#' @examples
#' # visualize F distribution
#' vdist_f_plot()
#' vdist_f_plot(6, 10, normal = TRUE)
#'
#' @seealso \code{\link[stats]{FDist}}
#'
#' @export
#'
vdist_f_plot <- function(num_df = 4, den_df = 30, normal = FALSE) {

  if (!is.numeric(num_df)) {
    stop("Numerator DF must be numeric/integer")
  }

  if (!is.numeric(den_df)) {
    stop("Denominator DF must be numeric/integer")
  }

  if (!is.logical(normal)) {
    stop("input for normal must be logical")
  }

  num_df <- as.integer(num_df)
  den_df <- as.integer(den_df)
  fm     <- round(den_df / (den_df - 2), 3)
  fsd    <- round(sqrt((2 * (fm ^ 2) * (num_df + den_df - 2)) / (num_df * (den_df - 4))), 3)
  x      <- seq(0, 4, 0.01)
  nx     <- seq(-2, 4, 0.01)
  
  plot_data  <- tibble::tibble(x = x, y = stats::df(x, num_df, den_df))
  poly_data  <- tibble::tibble(y = c(0, seq(0, 4, 0.01), 4),
    z = c(0, stats::df(seq(0, 4, 0.01), num_df, den_df), 0))
  point_data <- tibble::tibble(x = fm, y = 0)
  nline_data <- tibble::tibble(x = nx, y = stats::dnorm(nx, fm, fsd))

  gplot <-
    plot_data %>%
    ggplot2::ggplot() +
    ggplot2::geom_line(ggplot2::aes(x = x, y = y), color = "blue") +
    ggplot2::geom_polygon(data = poly_data, mapping = ggplot2::aes(x = y, y = z), 
      fill = '#4682B4') +
    ggplot2::geom_point(data = point_data, mapping = ggplot2::aes(x = x, y = y),
      shape = 4, color = 'red', size = 3) +
    ggplot2::xlab(paste("Mean =", fm, " Std Dev. =", fsd)) + ggplot2::ylab('') +
    ggplot2::ggtitle(label = 'f Distribution', 
      subtitle = paste("Num df =", num_df, "  Den df =", den_df)) + 
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                   plot.subtitle = ggplot2::element_text(hjust = 0.5)) +
    ggplot2::scale_x_continuous(breaks = c(-2:4)) +
    ggplot2::scale_y_continuous(breaks = NULL)

  if (normal) {
    gplot <- 
      gplot +
      ggplot2::geom_line(data = nline_data, mapping = ggplot2::aes(x = x, y = y),
        color = '#FF4500')
  }

  print(gplot)

}