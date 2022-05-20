#' @title Plotable
#' @docType class
#' @description An object holding plotting information
#' @import ospsuite
#' @format NULL
Plotable <- R6::R6Class(
  "Plotable",
  inherit = Printable,
  cloneable = FALSE,
  active = list(),
  private = list(),
  public = list(
    #' @description
    #' Initialize a new instance of the class
    #' @param label A string that is used as a label (e.g. in the legend) for the data set
    #' @return A new `Plotable` object.
    initialize = function(label) {
      validateIsString(label)

      self$label <- label
    },
    #' @field label A string that is used as a label (e.g. in the legend) for the data set
    #' that will be plotted
    label = NULL,

    #' @field color Color that will be used when plotting the output. Represented as a array
    #' of three HSV values.
    #' @seealso col2hsv
    color = NULL,

    #' @field xFactor Numeric value the x-values will be multiplied by.
    xFactor = 1,

    #' @field yFactor Numeric value the y-values will be multiplied by.
    yFactor = 1,

    #' @field xOffset A value that is added to all x-values when plotting
    xOffset = 0,

    #' @field yOffset A value that is added to all y-values when plotting
    yOffset = 0,

    #' @field type "p" for points, "l" for line, or "pl" for both
    type = "p",

    #' @field pch Either an integer specifying a symbol or a single character to be used as the default in plotting points. See `par` for more information.
    pch = NULL,

    #' @field lty The line type. See `par` for more information.
    lty = NULL,

    #' @description
    #' Print the object to the console
    #' @param ... Rest arguments.
    print = function(...) {
      private$printClass()
      private$printLine("label", self$label)
      private$printLine("x factor", self$xFactor)
      private$printLine("y factor", self$yFactor)
      private$printLine("x offset", self$xOffset)
      private$printLine("y offset", self$yOffset)
      private$printLine("Color", self$color)
      private$printLine("Type", self$type)
      private$printLine("pch", self$pch)
      private$printLine("lty", self$lty)
      invisible(self)
    }
  )
)
