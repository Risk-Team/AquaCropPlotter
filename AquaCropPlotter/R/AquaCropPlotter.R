#' Launching the app
#'
#' Load and visualize AquaCrop results
#' @export

#' @return launching the app
#' @examples
#' aquacropplotter()
#'

aquacropplotter <- function() {

runApp(appDir = system.file("shinyapp", package="AquaCropPlotter"))

}
