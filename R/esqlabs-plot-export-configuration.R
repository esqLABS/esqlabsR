#' @title ExportConfiguration
#' @description R6 class defining and managing how to export/save ggplot objects
#' with row-aware height.
#' @field fileName character defining the name of the file to be saved
#' @field format character defining the format/device of the file to be saved
#' @field width numeric values defining the width in `units` of the plot dimensions after saving
#' @field height numeric values defining the height in `units` of the plot dimensions after saving
#' @field units character defining the unit of the saving dimension
#' @field dpi (dots per inch) numeric value defining plot resolution
#' @family PlotConfiguration classes
#' @export
ExportConfiguration <- R6::R6Class(
  "ExportConfiguration",
  cloneable = FALSE,
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
    fileName = NULL,
    format = NULL,
    width = NULL,
    height = NULL,
    units = NULL,
    dpi = NULL,

    #' @description Create a new `ExportConfiguration` object
    #' @param fileName character defining the name of the file to be saved
    #' @param format character defining the format of the file to be saved
    #' @param width numeric values defining the width in `units` of the plot dimensions after saving
    #' @param height numeric values defining the height in `units` of the plot dimensions after saving
    #' @param units character defining the unit of the saving dimension
    #' @param dpi numeric value defining plot resolution (dots per inch)
    #' @return A new `ExportConfiguration` object
    initialize = function(fileName = NULL,
                          format = NULL,
                          width = NULL,
                          height = NULL,
                          units = NULL,
                          dpi = NULL) {
      validateIsCharacter(fileName, nullAllowed = TRUE)
      validateIsCharacter(format, nullAllowed = TRUE)
      validateIsIncluded(units, c("cm", "in", "mm", "px"), nullAllowed = TRUE)
      validateIsNumeric(width, nullAllowed = TRUE)
      validateIsNumeric(height, nullAllowed = TRUE)
      validateIsNumeric(dpi, nullAllowed = TRUE)

      self$format <- format %||% ospsuite.plots::getOspsuite.plots.option(ospsuite.plots::OptionKeys$exportDevice)
      self$width <- width %||% ospsuite.plots::getOspsuite.plots.option(ospsuite.plots::OptionKeys$exportWidth)
      # Height is not defined by ospsuite.plots, using width to create square plots
      self$height <- height %||% ospsuite.plots::getOspsuite.plots.option(ospsuite.plots::OptionKeys$exportWidth)
      self$units <- units %||% ospsuite.plots::getOspsuite.plots.option(ospsuite.plots::OptionKeys$exportUnits)
      self$dpi <- dpi %||% ospsuite.plots::getOspsuite.plots.option(ospsuite.plots::OptionKeys$exportDpi)
      self$fileName <- fileName %||% "figure"
      return(invisible())
    },

    #' @description Print properties of export configuration
    #' @return Export configuration properties
    print = function() {
      ospsuite.utils::ospPrintItems(
        list(
          "Format/Device" = self$format,
          "Width" = paste(self$width, self$units),
          "Height Per Row" = paste(self$width, self$units),
          "Resolution" = paste(self$dpi, " dots per inch")
        ),
        title = "Export Configuration"
      )
      return(invisible())
    },

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
      ospsuite.plots::exportPlot(
        plotObject,
        filename = basename(fileName %||% self$path),
        filepath = dirname(fileName %||% self$path),
        width = self$width,
        height = self$height,
        device = self$format
      )
      # Restore the old value
      self$height <- oldHeight
    }
  )
)
