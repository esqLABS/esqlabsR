#' Start function visualizer
#' @details Starts a shiny app for visualizing of function values.
#' @export
startFunctionVisualizer <- function() {
  appDir <- system.file("FunctionVisualizer", package = "esqlabsR")
  shiny::runApp(appDir, display.mode = "normal")
}

#' Start unit converter
#' @details Starts a shiny app for computing unit conversions.
#' @export
startUnitConverter <- function() {
  appDir <- system.file("UnitConverter", package = "esqlabsR")
  shiny::runApp(appDir, display.mode = "normal")
}
