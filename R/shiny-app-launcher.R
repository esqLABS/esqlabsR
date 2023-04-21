#' Start function visualizer
#' @details Starts a shiny app for visualizing of function values.

#' @export
startFunctionVisualizer <- function() {
  checkShinyPackages()
  appDir <- system.file("FunctionVisualizer", package = "esqlabsR")
  shiny::runApp(appDir, display.mode = "normal")
}

#' Start unit converter
#' @details Starts a shiny app for computing unit conversions.
#' @export
startUnitConverter <- function() {
  checkShinyPackages()
  appDir <- system.file("UnitConverter", package = "esqlabsR")
  shiny::runApp(appDir, display.mode = "normal")
}

#' Check if shiny, shinyjs packages are installed
#' @noRd
checkShinyDeps <- function() {
  rlang::check_installed("shiny", reason = "to launch this shiny App")
  rlang::check_installed("shinyjs", reason = "to launch this shiny App")
}
