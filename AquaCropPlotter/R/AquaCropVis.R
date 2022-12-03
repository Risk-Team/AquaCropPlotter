#' Launching the app
#'
#' Load and visualize AquaCrop results
#' @export

#' @return launching the app
#' @examples
#' aquacropvis()
#'

aquacropvis <- function() {

runApp(appDir = system.file("shinyapp", package="AquaCropVis"))

}
