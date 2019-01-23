#' @title Launch shiny app
#' @description Launches shiny app for interactive model building.
#' @examples
#' \dontrun{
#' vdist_launch_app ()
#' }
#' @export
#'
vdist_launch_app  <- function() {

	message("`vdist_launch_app ()` has been soft-deprecated and will be removed in the next release. In future, to launch the app, run the below code:\n 
	- install.packages('xplorerr')\n - xplorerr::app_vistributions()\n")

	check_suggests('shinyBS')
	check_suggests('shinycssloaders')
	check_suggests('shinythemes')

	xplorerr::app_vistributions()
}
 