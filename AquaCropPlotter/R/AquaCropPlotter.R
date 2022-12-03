#' Launching the app
#'
#' Load and visualize AquaCrop results
#' @export

#' @return launching the app
#' @examples
#' aquacropvis()
#'

aquacropplotter <- function() {

runApp(appDir = system.file("shinyapp", package="AquaCropPlotter"))

}
