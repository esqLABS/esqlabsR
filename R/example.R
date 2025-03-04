#' Get path to esqlabsR examples
#'
#' esqlabsR comes bundled with some example Project in its `inst/extdata`
#' directory. This function make them easy to access.
#'
#' @param name Name of example project. If `NULL`, the example names will be listed.
#' @keywords internal
exampleDirectory <- function(name = NULL) {
  if (is.null(name)) {
    dir(system.file("extdata", "examples", package = "esqlabsR"))
  } else {
    system.file("extdata", "examples", name, package = "esqlabsR", mustWork = TRUE)
  }
}
