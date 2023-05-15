#' Get path to esqlabsR examples
#'
#' esqlabsR comes bundled with some example Project in its `inst/extdata`
#' directory. This function make them easy to access.
#'
#' @param path Name of file. If `NULL`, the example files will be listed.
#' @keywords internal
esqlabsR_example <- function(path = NULL) {
  if (is.null(path)) {
    dir(system.file("extdata","examples", package = "esqlabsR"))
  } else {
    system.file("extdata","examples", path, package = "esqlabsR", mustWork = TRUE)
  }
}
