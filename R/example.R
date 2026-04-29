#' Get path to esqlabsR project templates
#'
#' esqlabsR comes bundled with project templates in its `inst/extdata`
#' directory. This function makes them easy to access.
#'
#' @param name Name of project directory. If `NULL`, the available names will be
#'   listed.
#' @keywords internal
.projectDirectory <- function(name = NULL) {
  directory <- system.file("extdata", "projects", package = "esqlabsR")
  if (!is.null(name)) {
    directory <- file.path(directory, name)
  }
  directory
}
