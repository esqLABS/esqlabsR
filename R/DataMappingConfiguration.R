#' @title DataMappingConfiguration
#' @docType class
#' @description An object storing configuration of a data mapping
#' @export
#' @format NULL
DataMappingConfiguration <- R6::R6Class(
  "DataMappingConfiguration",
  inherit = Printable,
  cloneable = FALSE,
  active = list(
    #' @field xFactors A named list listing numerical values that x-values are multiplied by. Keys are names of data sets,
    #' values are numerical factors.
    xFactors = function(value) {
      if (missing(value)) {
        private$.xFactors
      } else {
        stop(messages$errorPropertyReadOnly("xFactors", optionalMessage = "Use function 'setXFactors' to set the values."))
      }
    },
    #' @field yFactors A named list listing numerical values that y-values are multiplied by. Keys are names of data sets,
    #' values are numerical factors.
    yFactors = function(value) {
      if (missing(value)) {
        private$.yFactors
      } else {
        stop(messages$errorPropertyReadOnly("yFactors", optionalMessage = "Use function 'setYFactors' to set the values."))
      }
    },
    #' @field xOffsets A named list listing numerical values that will be added to the x-values. Keys are names of data sets,
    #' values are numerical values
    xOffsets = function(value) {
      if (missing(value)) {
        private$.xOffsets
      } else {
        stop(messages$errorPropertyReadOnly("xOffsets", optionalMessage = "Use function 'setXOffsets' to set the values."))
      }
    },
    #' @field yOffsets A named list listing numerical values that will be added to the y-values. Keys are names of data sets,
    #' values are numerical values
    yOffsets = function(value) {
      if (missing(value)) {
        private$.yOffsets
      } else {
        stop(messages$errorPropertyReadOnly("yOffsets", optionalMessage = "Use function 'setYOffsets' to set the values."))
      }
    },
    #' @field lineTypes A named list listing line types that will be used to plot a certain data set. Keys are names of data sets,
    #' values are numerical values recognized by the \code{lty} argument of the \code{\link{plot}} function
    lineTypes = function(value) {
      if (missing(value)) {
        private$.lineTypes
      } else {
        stop(messages$errorPropertyReadOnly("lineTypes", optionalMessage = "Use function 'setLineTypes' to set the values."))
      }
    },
    #' @field colors A named list listing color values that will be used to plot a certain data set. Keys are names of data sets,
    #' values are values recognized by the \code{col} argument of the \code{\link{plot}} function
    colors = function(value) {
      if (missing(value)) {
        private$.colors
      } else {
        stop(messages$errorPropertyReadOnly("colors", optionalMessage = "Use function 'setColors' to set the values."))
      }
    }
  ),
  private = list(
    .xFactors = NULL,
    .yFactors = NULL,
    .xOffsets = NULL,
    .yOffsets = NULL,
    .lineTypes = NULL,
    .colors = NULL
  ),
  public = list(
    #' @description
    #' Initialize a new instance of the class
    #' @return A new `DataMappingConfiguration` object.
    initialize = function() {
      private$.xFactors <- list()
      private$.yFactors <- list()
      private$.xOffsets <- list()
      private$.yOffsets <- list()
      private$.lineTypes <- list()
      private$.colors <- list()
    },

    #' @description Set the values of x-factors. An x-factor will be multiplied by the x-values of the data set.
    #' @param labels A string or a list of strings representing the names of the data sets
    #' @param xFactors A numerical value or a list of numerical values representing the x-factors. Both lists must be of same length
    #' and store the entries in the same order.
    setXFactors = function(labels, xFactors) {
      private$.xFactors <- mapPut(labels, xFactors, map = private$.xFactors, overwrite = TRUE)
    },
    #' @description Set the values of y-factors. A y-factor will be multiplied by the y-values of the data set.
    #' @param labels A string or a list of strings representing the names of the data sets
    #' @param yFactors A numerical value or a list of numerical values representing the y-factors. Both lists must be of same length
    #' and store the entries in the same order.
    setYFactors = function(labels, yFactors) {
      private$.yFactors <- mapPut(labels, yFactors, map = private$.yFactors, overwrite = TRUE)
    },
    #' @description Set the values of x-offsets. An x-offset will be added to the the x-values of the data set.
    #' @param labels A string or a list of strings representing the names of the data sets
    #' @param xOffsets A numerical value or a list of numerical values representing the x-offsets. Both lists must be of same length
    #' and store the entries in the same order.
    setXOffsets = function(labels, xOffsets) {
      private$.xOffsets <- mapPut(labels, xOffsets, map = private$.xOffsets, overwrite = TRUE)
    },
    #' @description Set the values of y-offsets. A y-offset will be added to the the y-values of the data set.
    #' @param labels A string or a list of strings representing the names of the data sets
    #' @param yOffsets A numerical value or a list of numerical values representing the y-offsets. Both lists must be of same length
    #' and store the entries in the same order.
    setYOffsets = function(labels, yOffsets) {
      private$.yOffsets <- mapPut(labels, yOffsets, map = private$.yOffsets, overwrite = TRUE)
    },
    #' @description Set the colors that will be used to plot a certain data set.
    #' @param labels A string or a list of strings representing the names of the data sets
    #' @param colors Values representing a color recognized by the \code{col} argument of the \code{\link{plot}} function
    setColors = function(labels, colors) {
      private$.colors <- mapPut(labels, colors, map = private$.colors, overwrite = TRUE)
    },
    #' @description Set the line types that will be used to plot a certain data set.
    #' @param labels A string or a list of strings representing the names of the data sets
    #' @param lineTypes Values recognized by the \code{col} argument of the \code{\link{plot}} function
    setLineTypes = function(labels, lineTypes) {
      private$.lineTypes <- mapPut(labels, lineTypes, map = private$.lineTypes, overwrite = TRUE)
    },

    #' @description
    #' Print the object to the console
    #' @param ... Rest arguments.
    print = function(...) {
      private$printClass()
      invisible(self)
    }
  )
)
