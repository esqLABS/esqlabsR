#' @title PlotConfiguration
#' @docType class
#' @description An object storing configuration of a plot
#' @export
#' @format NULL
PlotConfiguration <- R6::R6Class(
  "PlotConfiguration",
  inherit = Printable,
  cloneable = FALSE,
  active = list(),
  private = list(),
  public = list(
    #' @description
    #' Initialize a new instance of the class
    #' @return A new `PlotConfiguration` object.
    initialize = function() {

    },
    #' @field outputDevice Output target of the plot. If NULL (default), the figure is created in the default "plot"
    #' output. Other values indicate output into a file. A list of supported outputs is provided in `GraphicsDevices`-enum.
    outputDevice = NULL,
    #' @field outputName A string used as the name of the .png file and as the title of the plot
    outputName = "",
    #' @field outputFolder Path to the directory where the outputs should be stored.
    outputFolder = NULL,
    #' @field width Width of the resulting plot in cm. If NULL (default), the width is automatically calculated
    #' using the value esqlabsEnv$widthPerPlotMapping
    width = NULL,
    #' @field height Height of the resulting plot in cm. If `NULL` (default), the
    #'   height is automatically calculated
    #' using the value esqlabsEnv$heightPerPlotMapping
    height = NULL,
    #' @field nrOfCols Number of column in a multi-panel plot. If `NULL`
    #'   (default), the number is calculated automatically to fit all panels
    #'   while trying to keep the number of columns and rows equal. If `nrOfCols`
    #'   is specified, the number of rows calculated to fit all the panels.
    nrOfCols = NULL,
    #' @field res Resolution of the .png output in dpi. Default is 600.
    res = 600,
    #' @field pointsize Size of the plotted text. Default is 8.
    pointsize = 8,
    #' @field addTitle Boolean flag if the title should be added. If TRUE,
    #'   `outputName` is added a title.
    addTitle = TRUE,

    #' @description
    #' Print the object to the console
    #' @param ... Rest arguments.
    print = function(...) {
      private$printClass()
      private$printLine("Output device", self$outputDevice)
      private$printLine("Output name", self$outputName)
      private$printLine("Output path", self$outputPath)
      private$printLine("Width", self$width)
      private$printLine("Height", self$height)
      private$printLine("Number of columns", self$nrOfCols)
      private$printLine("Resulution", self$res)
      private$printLine("Point size", self$pointsize)
      private$printLine("addTitle", self$addTitle)
      invisible(self)
    }
  )
)
