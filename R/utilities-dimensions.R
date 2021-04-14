#' Dimension existence
#'
#' @param dimension String representation of the dimension.
#' @details Deprecated, use ospsuite::ospDimensions instead. Check if the provided dimension is supported.
#' @export
existsDimension <- function(dimension) {
  ospsuite::hasDimension(dimension = dimension)
}
