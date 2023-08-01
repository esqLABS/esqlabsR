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
    .height = NULL,
    .rescaleTextSizes = function(plotObject) {
      # Scaling of the maximal widths is currently determined by trial-and-error.
      # Better solution would be to get the margins/offsets of the respective
      # text elements and subtract them from 'widthPerPanel'.
      plotGridTitleFactor <- 0.95
      panelTitleFactor <- 0.17
      legendLabelFactor <- 0.15

      ### Plot grid title
      if (isOfType(plotObject, "patchwork")) {
        if (!isEmpty(plotObject$patches$annotation$theme$plot.title)) {
          plotObject$patches$annotation$theme$plot.title$size <-
            private$.calculateTextSize(
              string = plotObject$patches$annotation$title,
              stringPointSize = plotObject$patches$annotation$theme$plot.title$size,
              maxSize = self$width * plotGridTitleFactor
            )
        }
      }

      ### Process the the panels
      # Assuming that all panels have the same width, size of the single panel is
      # 'n colums / total width'. This will not work with custom sizes of the panels.
      nCols <- ggplot2::wrap_dims(length(plotObject$patches$plots) + 1,
        nrow = plotObject$patches$layout$nrow,
        ncol = plotObject$patches$layout$ncol
      )[[2]]
      widthPerPanel <- self$width / nCols
      # If the plot has only one panel, its properties are directly that of the
      # patchwork object.
      if (!isEmpty(plotObject$theme$plot.title)) {
        plotObject$theme$plot.title$size <-
          private$.calculateTextSize(
            # For whatever reason, it seems that the title is stored as a list
            # if string separated by a ",". Therefore, for calculation of the
            # width, collapse
            string = paste(plotObject$labels$title, collapse = ", "),
            stringPointSize = plotObject$theme$plot.title$size,
            maxSize = widthPerPanel * (1 - nCols * panelTitleFactor)
          )
      }

      # Legends
      legendLabels <- .getLegendLabel(plotObject)
      if (!is.null(legendLabels)) {
        plotObject$theme$legend.text$size <-
          min(sapply(legendLabels, function(label) {
            private$.calculateTextSize(
              string = label,
              stringPointSize = plotObject$theme$legend.text$size,
              maxSize = widthPerPanel * (1 - nCols * legendLabelFactor)
            )
          }))
      }
      # Also change height of the keys
      # plotObject$theme$legend.key.height <-
      #   grid::unit(1, "strheight", legendLabels)
      # plotObject$theme$legend.key.size <- grid::unit(plotObject$plotConfiguration$points$size, "points")

      # All following panels in a patchwork object are part of the 'patches'
      for (patchIdx in seq_along(plotObject$patches$plots)) {
        patch <- plotObject$patches$plots[[patchIdx]]
        # Title
        if (!isEmpty(plotObject$patches$plots[[patchIdx]]$theme$plot.title)) {
          plotObject$patches$plots[[patchIdx]]$theme$plot.title$size <-
            private$.calculateTextSize(
              string = paste(patch$labels$title, collapse = ", "),
              stringPointSize = patch$theme$plot.title$size,
              maxSize = widthPerPanel * (1 - nCols * panelTitleFactor)
            )
        }
        # Legends
        legendLabels <- .getLegendLabel(patch)
        if (!is.null(legendLabels)) {
          plotObject$patches$plots[[patchIdx]]$theme$legend.text$size <-
            min(sapply(legendLabels, function(label) {
              private$.calculateTextSize(
                string = label,
                stringPointSize = patch$theme$legend.text$size,
                maxSize = widthPerPanel * (1 - nCols * legendLabelFactor)
              )
            }))
        }
        # Also change height of the keys
        # plotObject$patches$plots[[patchIdx]]$theme$legend.key.height <-
        #   grid::unit(1, "strheight", legendLabels)
        # plotObject$theme$legend.key.size <- grid::unit(plotObject$plotConfiguration$points$size, "points")
      }

      return(plotObject)
    },
    .calculateTextSize = function(string, stringPointSize, maxSize) {
      # DPI used by the png graphics by default. It is used by
      # strwidth to calculate the width
      defaultDpi <- 72
      # Inch to cm conversion factor
      inchToCm <- 2.54
      # Default point size of the device that will be used by strwidth
      defaultPs <- par()$ps

      # Width of the string in cm. strwidth calculates it for defaultPs,
      # so it might be scaled by the actual text size
      stringWidth <- strwidth(string, units = "inches") * inchToCm * stringPointSize / defaultPs

      if (stringWidth > maxSize) {
        stringPointSize <- stringPointSize * (maxSize / stringWidth)
      }
      return(stringPointSize)
    }
  ),
  public = list(

    #' @description Save/Export a plot
    #'
    #' @param plotObject A `ggplot` object
    #' @param autoscaleText If `TRUE`, title, subtitle, and legend text size will
    #' be reduced if required to fit into figures size. If `FALSE`, specified
    #' text element size is always applied an text might be cropped. Default is
    #' `TRUE`.
    #' @param fileName character file name of the exported plot
    #'
    #' @return The file name of the exported plot
    savePlot = function(plotObject, fileName = NULL, autoscaleText = TRUE) {
      # This function overrides the parent `savePlot()` function in order to
      # calculate the height of the output based on the number of rows.
      # Save the old value of the `height` property.
      oldHeight <- self$height

      # If `heightPerRow` is defined, calculate the height of the figure
      if (!is.null(self$heightPerRow)) {
        # Get the number of rows as defined in the plot layout
        nrOfRows <- ggplot2::wrap_dims(length(plotObject$patches$plots) + 1,
          nrow = plotObject$patches$layout$nrow,
          ncol = plotObject$patches$layout$ncol
        )[[1]]
        self$height <- self$heightPerRow * nrOfRows
      }

      # Fit all text into the margins, which might require re-calculation
      # of text sizes
      if (autoscaleText) {
        plotObject <- private$.rescaleTextSizes(plotObject)
      }

      super$savePlot(plotObject, fileName)
      # Restore the old value
      self$height <- oldHeight
    }
  )
)
