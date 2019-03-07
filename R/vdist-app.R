#' @title Launch shiny app
#' @description Launches shiny app for visualizing distributions.
#' @examples
#' \dontrun{
#' vdist_launch_app ()
#' }
#' @export
#'
vdist_launch_app  <- function() {

	check_suggests('shinyBS')
	check_suggests('shinycssloaders')
	check_suggests('shinythemes')

	shiny::runApp(appDir = system.file("application", package = "vistributions"))
}
 