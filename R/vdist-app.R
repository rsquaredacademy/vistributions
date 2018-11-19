#' @importFrom shiny runApp
#' @title Launch Shiny App
#' @description Launches shiny app
#' @examples
#' \dontrun{
#' vdist_launch_app()
#' }
#' @export
#'
vdist_launch_app <- function() {
  shiny::runApp(appDir = system.file("application", package = "vistributions"))
}

