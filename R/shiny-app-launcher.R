#' Start function visualizer
#' @details Starts a shiny app for visualizing of function values.
#' @export
startFunctionVisualizer <- function() {
  appDir <- system.file("FunctionVisualizer", package = "esqlabsR")
  shiny::runApp(appDir, display.mode = "normal")
}
