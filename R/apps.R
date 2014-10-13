#' A Shiny app to experiment with item models
#'
#' @export
#' @examples
#' \dontrun{
#' itemModelExplorer()  # will launch a browser in RStudio
#' }
itemModelExplorer <- function() {
	shiny::runApp(system.file('itemModelExplorer', package='ifaTools'))
}
