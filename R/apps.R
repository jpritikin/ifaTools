#' A Shiny app to experiment with item models
#'
#' @export
#' @import shiny
#' @import rpf
#' @examples
#' \dontrun{
#' itemModelExplorer()  # will launch a browser in RStudio
#' }
itemModelExplorer <- function() {
	shiny::runApp(system.file('itemModelExplorer', package='ifaTools'))
}

#' A Shiny app for building IFA models
#'
#' @export
#' @import shiny
#' @examples
#' \dontrun{
#' modelBuilder()  # will launch a browser in RStudio
#' }
modelBuilder <- function() {
	shiny::runApp(system.file('modelBuilder', package='ifaTools'))
}

.onAttach <- function(libname, pkgname) {
	packageStartupMessage(paste(
		"Welcome to ifaTools!\n",
		"Use itemModelExplorer() to explore item models.\n",
		"Use modelBuilder() to build an IFA model."))
}
