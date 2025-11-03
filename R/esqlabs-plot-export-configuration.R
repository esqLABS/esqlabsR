#' @title ExportConfiguration
#' @description R6 class extending [tlf::ExportConfiguration] with row-aware
#'   height.
#'
#' @details Inherits fields and behavior from [tlf::ExportConfiguration].
#'
#' @section Inherited fields: See [tlf::ExportConfiguration] for `name`, `path`,
#'   `format`, `width`, `height`, `units`, and `dpi`.
#'
#' @family PlotConfiguration classes
#' @export
ExportConfiguration <- R6::R6Class(
  "ExportConfiguration",
  inherit = tlf::ExportConfiguration,
  active = list(
    #' @field heightPerRow The height of the plot dimensions for a row in a
    #'   multi panel plot. The final height of the figure will be 'heightPerRow'
    #'   times the number of rows. If `NULL` (default), value used in `height`
    #'   is used. If not `NULL`, this value always overrides the `height`
    #'   property.
    heightPerRow = function(value) {
      if (missing(value)) {
        private$.heightPerRow
      } else {
        validateIsNumeric(value)
        private$.heightPerRow <- value
      }
    }
  ),
  private = list(
    .heightPerRow = NULL,
    .height = NULL
  ),
  public = list(
    #' @description Save/Export a plot
    #' @param plotObject A `ggplot` object
    #' @param fileName character file name of the exported plot
    #' @returns The file name of the exported plot
    savePlot = function(plotObject, fileName = NULL) {
      # This function overrides the parent `savePlot()` function in order to
      # calculate the height of the output based on the number of rows.
      # Save the old value of the `height` property.
      oldHeight <- self$height

      # If `heightPerRow` is defined, calculate the height of the figure
      if (!is.null(self$heightPerRow)) {
        # Get the number of rows as defined in the plot layout
        nrOfRows <- ggplot2::wrap_dims(
          length(plotObject$patches$plots) + 1,
          nrow = plotObject$patches$layout$nrow,
          ncol = plotObject$patches$layout$ncol
        )[[1]]
        self$height <- self$heightPerRow * nrOfRows
      }
      super$savePlot(plotObject, fileName)
      # Restore the old value
      self$height <- oldHeight
    }
  )
)
