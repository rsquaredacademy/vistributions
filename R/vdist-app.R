#' @title Launch shiny app
#' @description Launches shiny app for visualizing distributions.
#' @examples
#' \dontrun{
#' vdist_launch_app ()
#' }
#' @return Called for its side effect of launching the app; returns the app object invisibly.
#'
#' @family shiny app
#'
#' @export
#'
vdist_launch_app  <- function() {

	check_suggests("xplorerr")
	xplorerr::app_vistributions()
}
 