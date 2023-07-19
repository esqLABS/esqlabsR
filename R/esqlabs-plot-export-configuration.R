#' @title ExportConfiguration
#' @description R6 class defining properties for saving a `ggplot` object
#' @field name character defining the name of the file to be saved (without extension)
#' @field path Path of the directory to save plot to: path and filename are
#' combined to create the fully qualified file name. Defaults to the working directory.
#' @field format character defining the format of the file to be saved
#' @field width numeric values defining the width in `units` of the plot dimensions after saving
#' @field height numeric values defining the height in `units` of the plot dimensions after saving.
#' Only used if `heightPerRow` is `NULL`.
#' @field units character defining the unit of the saving dimension
#' @field dpi (dots per inch) numeric value defining plot resolution
#' @export
#' @import ggplot2
#' @family PlotConfiguration classes
ExportConfiguration <- R6::R6Class(
  "ExportConfiguration",
  inherit = tlf::ExportConfiguration,
  active = list(
    #' @field heightPerRow The height of the plot dimensions for a row in a multi
    #' pannel plot. The final height of the figure will be 'heightPerRow' times
    #' the number of rows.
    #' If `NULL` (default), value used in `height` is used. If not `NULL`, this
    #' value always overrides the `height` property.
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
    #' @return The file name of the exported plot
    savePlot = function(plotObject, fileName = NULL) {
      # This function overrides the parent `savePlot()` function in order to
      # calculate the height of the output based on the number of rows.
      # Save the old value of the `height` property.
      oldHeight <- self$height

      # If `heightPerRow` is defined, calculate the height of the figure
      if (!is.null(self$heightPerRow)) {
        # Get the number of rows as defined in the plot layout
        nrOfRows <- plotObject$patches$layout$nrow
        # If the value was not explicitely defined, it is `NULL`. In this case,
        # use the the same method to calculate the number of rows for a given
        # number of figures that is used in ggplot2::facet_wrap()
        if (is.null(nrOfRows)) {
          # number of plots + 1, as somehow the lenght of plotObject$patches$plots
          # is one too short?
          nrOfRows <- grDevices::n2mfrow(length(plotObject$patches$plots) + 1)[2]
        }
        self$height <- self$heightPerRow * nrOfRows
      }
      super$savePlot(plotObject, fileName)
      # Restore the old value
      self$height <- oldHeight
    }
  )
)
