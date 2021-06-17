#' @title XYData
#' @docType class
#' @description A set of x and y value pairs
#' @export
#' @format NULL
XYData <- R6::R6Class(
  "XYData",
  inherit = Plotable,
  cloneable = FALSE,
  active = list(
    #' @field xValues An array of x-values. For time series, the values must be in minutes.
    xValues = function(value) {
      if (missing(value)) {
        private$.xVals
      } else {
        validateIsNumeric(value)
        validateIsSameLength(value, private$.yVals)
        private$.xVals <- value
      }
    },

    #' @field yValues An array of y-values.
    yValues = function(value) {
      if (missing(value)) {
        private$.yVals
      } else {
        validateIsNumeric(value)
        validateIsSameLength(value, private$.xVals)
        private$.yVals <- value
      }
    },

    #' @field yError An array of arithmetic error of the y-values. Only positive values are allowed
    yError = function(value) {
      if (missing(value)) {
        private$.yError
      } else {
        validateIsNumeric(value, nullAllowed = TRUE)
        if (!is.null(value)) {
          validateIsSameLength(private$.yVals, value)
          # Replace all NAs with 0
          value[is.na(value)] <- 0
          # Check if all values are positive
          if (!all(value >= 0)) {
            stop(messages$errorValuesAreNotPositive(values = value))
          }
          private$.yError <- value
        }
      }
    },

    #' @field xMax Maximal value of x values plus xOffset multiplied by the scaling factor
    xMax = function(value) {
      if (missing(value)) {
        max(private$.xVals + self$xOffset) * self$xFactor
      } else {
        stop(messages$errorPropertyReadOnly("xMax"))
      }
    },
    #' @field xMin Minimal value of x values plus xOffset multiplied by the scaling factor
    xMin = function(value) {
      if (missing(value)) {
        min(private$.xVals + self$xOffset) * self$xFactor
      } else {
        stop(messages$errorPropertyReadOnly("xMin"))
      }
    },
    #' @field yMax Maximal value (plus error, if specified) of y values plus yOffset multiplied by the scaling factor
    yMax = function(value) {
      if (missing(value)) {
        max(private$.yVals + self$yOffset + (private$.yError %||% 0)) * self$yFactor
      } else {
        stop(messages$errorPropertyReadOnly("yMax"))
      }
    },
    #' @field yMin Minimal value (minus error, if specified) of y values plus yOffset multiplied by the scaling factor
    yMin = function(value) {
      if (missing(value)) {
        min(private$.yVals + self$yOffset - (private$.yError %||% 0)) * self$yFactor
      } else {
        stop(messages$errorPropertyReadOnly("yMin"))
      }
    },
    #' @field dataType Type of the data. See enum \code{XYDataTypes} for the list of supported types.
    dataType = function(value) {
      if (missing(value)) {
        private$.dataType
      } else {
        validateEnumValue(enum = XYDataTypes, value)
        private$.dataType <- value
      }
    },
    #' @field xDimension Dimension of x values. See enum \code{Dimensions} for the list of supported dimensions.
    xDimension = function(value) {
      if (missing(value)) {
        private$.XDim
      } else {
        validateDimension(value)
        private$.XDim <- value
        private$.XUnit <- getBaseUnit(value)
      }
    },
    #' @field xUnit Unit of x values
    xUnit = function(value) {
      if (missing(value)) {
        private$.XUnit
      } else {
        validateUnit(value, self$xDimension)
        private$.XUnit <- value
      }
    },
    #' @field yDimension Dimension of y values. See enum \code{Dimensions} for the list of supported dimensions.
    yDimension = function(value) {
      if (missing(value)) {
        private$.YDim
      } else {
        validateDimension(value)
        private$.YDim <- value
        private$.YUnit <- getBaseUnit(value)
        private$.YErrorUnit <- getBaseUnit(value)
      }
    },
    #' @field yUnit Unit of y values
    yUnit = function(value) {
      if (missing(value)) {
        private$.YUnit
      } else {
        validateUnit(value, self$yDimension)
        private$.YUnit <- value
      }
    },
    #' @field yErrorUnit Unit of y error values
    yErrorUnit = function(value) {
      if (missing(value)) {
        private$.YErrorUnit
      } else {
        # Dimensionless is always supported as it represents the geometric error.
        if (value == "") {
          private$.YErrorUnit <- value
        } else {
          validateUnit(value, self$yDimension)
          private$.YErrorUnit <- value
        }
      }
    }
  ),
  private = list(
    .xVals = NULL,
    .yVals = NULL,
    .yError = NULL,
    .dataType = NULL,
    .XDim = NULL,
    .XUnit = NULL,
    .YDim = NULL,
    .YUnit = NULL,
    .YErrorUnit = NULL,
    .metaData = list()
  ),
  public = list(
    #' @param xVals An array of numeric x values.
    #' @param yVals An array of numeric y values,
    #' @param yError An array of numeric values of the arithmetic error. Optional
    #' @param label A string that is used as a label (e.g. in the legend) for the data set
    #' @description
    #' Initialize a new instance of the class. xVals, yVals, and yError (can be omitted) must be of the same length
    #' @return A new `XYData` object.
    initialize = function(xVals, yVals, label, yError = NULL) {
      validateIsNumeric(c(xVals, yVals))
      validateIsNumeric(yError, nullAllowed = TRUE)
      validateIsSameLength(xVals, yVals)

      super$initialize(label)
      private$.xVals <- xVals
      private$.yVals <- yVals
      if (!is.null(yError)) {
        validateIsSameLength(yVals, yError)
        # Replace all NAs with 0
        yError[is.na(yError)] <- 0
        private$.yError <- yError
      }
      private$.dataType <- XYDataTypes$Unspecified

      self$xDimension <- Dimensions$Time
      self$yDimension <- Dimensions$Dimensionless
    },

    #' @description
    #' Returns the minimal value (minus error, if specified) of the y series that is not negative or null
    #' @return The minimal non-negative value of the y series minus error, if specified, plus yOffset multiplied by the scaling factor
    #' If no such value exists, returns Inf.
    yMinPositive = function() {
      effectiveVals <- private$.yVals + self$yOffset - (private$.yError %||% 0)
      min(effectiveVals[effectiveVals > 0]) * self$yFactor
    },

    #' @description
    #' Numerical factor for converting the x-values to a specific unit of a specific dimension
    #'
    #' @param dimension Target dimension
    #' @param unit Target unit
    #'
    #'
    #' @return xFactor multiplied by the factor for conversion from dimensions multiplied by the factors for the conversion between units.
    xUnitDimensionFactor = function(dimension, unit) {
      getUnitConversionFactor(self$xUnit, getBaseUnit(self$xDimension), self$xDimension) * dimensionsConversionFactor(self$xDimension, dimension) * getUnitConversionFactor(getBaseUnit(dimension), unit, dimension)
    },

    #' @description
    #' Numerical factor for converting the y-values to a specific unit of a specific dimension
    #'
    #' @param dimension Target dimension
    #' @param unit Target unit
    #' @param mw Double value of the molecular weight of the molecule. Used
    #' for conversion between molar and mass concentrations. Can be \code{NULL}
    #' for other dimensions (default)
    #'
    #' @return yFactor multiplied by the factor for conversion from dimensions multiplied by the factors for the conversion between units.
    yUnitDimensionFactor = function(dimension, unit, mw = NULL) {
      getUnitConversionFactor(self$yUnit, getBaseUnit(self$yDimension), self$yDimension) * dimensionsConversionFactor(self$yDimension, dimension, mw) * getUnitConversionFactor(getBaseUnit(dimension), unit, dimension)
    },

    #' @description
    #' Numerical factor for converting the y error values to a specific unit of a specific dimension
    #'
    #' @param dimension Target dimension
    #' @param unit Target unit
    #' @param mw Double value of the molecular weight of the molecule. Used
    #' for conversion between molar and mass concentrations. Can be \code{NULL}
    #' for other dimensions (default)
    #'
    #' @return yFactor multiplied by the factor for conversion from dimensions multiplied by the factors for the conversion between units.
    yErrorUnitDimensionFactor = function(dimension, unit, mw = NULL) {
      getUnitConversionFactor(self$yErrorUnit, getBaseUnit(self$yDimension), self$yDimension) * dimensionsConversionFactor(self$yDimension, dimension, mw) * getUnitConversionFactor(getBaseUnit(dimension), unit, dimension)
    },

    #' @description
    #' x values with all conversions applied.
    #'
    #' @param dimension Target dimension. If \code{NULL} (default), the no conversion between dimensions is applied.
    #' @param unit Target unit. If \code{NULL} (default), the no conversion between units is applied. If one of \code{dimension}
    #' or \code{unit} is specified, the other must be, too.
    #'
    #' @return Raw xValues plus xOffset multiplied by xFactor and converted to a specified unit/dimension.
    #' It is assumed that raw xValues are in the xDimension and xUnit.
    xValuesProcessed = function(dimension = NULL, unit = NULL) {
      unitDimensionFactor <- 1
      if ((is.null(dimension) || is.null(unit)) && !(is.null(dimension) && is.null(unit))) {
        stop(messages$errorOneArgumentNullButNotBoth("dimension", "unit"))
      }

      # Calculate the unit and dimension conversion factor
      if (!is.null(dimension)) {
        unitDimensionFactor <- self$xUnitDimensionFactor(dimension, unit)
      }
      # Add offset, multiply by the factor, and then convert to unit/dimension
      return((private$.xVals + self$xOffset) * self$xFactor * unitDimensionFactor)
    },

    #' @description
    #' y values with all conversions applied.
    #'
    #' @param dimension Target dimension. If \code{NULL} (default), the no conversion between dimensions is applied.
    #' @param unit Target unit. If \code{NULL} (default), the no conversion between units is applied. If one of \code{dimension}
    #' or \code{unit} is specified, the other must be, too.
    #' @param mw Double value of the molecular weight of the molecule. Used
    #' for conversion between molar and mass concentrations. Can be \code{NULL}
    #' for other dimensions (default).
    #'
    #' @return Raw yValues plus yOffset multiplied by yFactor and converted to a specified unit/dimension.
    #' It is assumed that raw yValues are in the yDimension and yUnit.
    yValuesProcessed = function(dimension = NULL, unit = NULL, mw = NULL) {
      unitDimensionFactor <- 1
      if ((is.null(dimension) || is.null(unit)) && !(is.null(dimension) && is.null(unit))) {
        stop(messages$errorOneArgumentNullButNotBoth("dimension", "unit"))
      }

      # Calculate the unit and dimension conversion factor
      if (!is.null(dimension)) {
        unitDimensionFactor <- self$yUnitDimensionFactor(dimension, unit, mw)
      }
      # Add offset, multiply by the factor, and then convert to unit/dimension
      return((private$.yVals + self$yOffset) * self$yFactor * unitDimensionFactor)
    },

    #' @description
    #' y error values with all conversions applied.
    #'
    #' @param dimension Target dimension. If \code{NULL} (default), the no conversion between dimensions is applied.
    #' @param unit Target unit. If \code{NULL} (default), the no conversion between units is applied. If one of \code{dimension}
    #' or \code{unit} is specified, the other must be, too.
    #' @param mw Double value of the molecular weight of the molecule. Used
    #' for conversion between molar and mass concentrations. Can be \code{NULL}
    #' for other dimensions (default).
    #'
    #' @return Raw yError plus yOffset multiplied by yFactor and converted to a specified unit/dimension.
    #' It is assumed that raw yError are in the yDimension and yErrorUnit.
    yErrorProcessed = function(dimension = NULL, unit = NULL, mw = NULL) {
      unitDimensionFactor <- 1
      if ((is.null(dimension) || is.null(unit)) && !(is.null(dimension) && is.null(unit))) {
        stop(messages$errorOneArgumentNullButNotBoth("dimension", "unit"))
      }

      # Calculate the unit and dimension conversion factor
      if (!is.null(dimension)) {
        unitDimensionFactor <- self$yErrorUnitDimensionFactor(dimension, unit, mw)
      }
      # Add offset, multiply by the factor, and then convert to unit/dimension
      return((private$.yError + self$yOffset) * self$yFactor * unitDimensionFactor)
    },

    #' @description
    #' Meta data list of \code{XYData} object
    #' @return
    #' .metaData of \code{XYData} object
    getAllMetaData = function() {
      return(private$.metaData)
    },

    #' @description
    #' Adds a new entry to meta data list of \code{XYData} object or changes its value if name is already present in meta data.
    #' If \code{value} is NULL, entry with corresponding name is deleted from meta data
    #'
    #' @param name Name of new meta data list entry
    #' @param value Value of new meta data list entry
    setMetaData = function(name = NULL, value = NULL) {
      if (is.null(name)) {
        stop("Parameter 'name' can not be NULL")
      }

      if (length(name) != 1 || length(value) > 1) {
        stop("Can only set a single meta data entry at once")
      }

      private$.metaData[[name]] <- value
    },

    #' @description Print the object to the console
    #' @param ... Rest arguments.
    print = function(...) {
      super$print()
      private$printLine("Data type", c(private$.dataType))
      # private$printLine("X values", c(private$.xVals))
      # private$printLine("Y values", c(private$.yVals))
      private$printLine("X dimension", c(private$.XDim))
      private$printLine("X unit", c(private$.XUnit))
      private$printLine("Y dimension", c(private$.YDim))
      private$printLine("Y unit", c(private$.YUnit))
      private$printLine("Y error unit", c(private$.YErrorUnit))
      invisible(self)
    }
  )
)

#' Possible entries for the \code{dataType} field of a \code{XYData} object
#' @export
XYDataTypes <- enum(list("Simulated", "Observed", "Unspecified"))
