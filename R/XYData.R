#' @title XYData
#' @docType class
#' @description A set of x and y value pairs
#' @export
#' @format NULL
XYData <- R6::R6Class(
  "XYData",
  inherit = Plotable,
  active = list(
    #' @field xValues An array of x-values. For time series, the values must be in minutes.
    xValues = function(value) {
      if (missing(value)) {
        private$.xVals
      } else {
        ospsuite:::validateIsNumeric(value)
        ospsuite:::validateIsSameLength(value, private$.yVals)
        private$.xVals <- value
      }
    },

    #' @field yValues An array of y-values.
    yValues = function(value) {
      if (missing(value)) {
        private$.yVals
      } else {
        ospsuite:::validateIsNumeric(value)
        ospsuite:::validateIsSameLength(value, private$.xVals)
        private$.yVals <- value
      }
    },

    #' @field yError An array of arithmetic error of the y-values. Only positive values are allowed
    yError = function(value) {
      if (missing(value)) {
        private$.yError
      } else {
        ospsuite:::validateIsNumeric(value, nullAllowed = TRUE)
        if (!is.null(value)) {
          ospsuite:::validateIsSameLength(private$.yVals, value)
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
        ospsuite:::validateEnumValue(enum = XYDataTypes, value)
        private$.dataType <- value
      }
    },
    #' @field xDimension Dimension of x values. See enum \code{ospDimensions} for the list of supported dimensions.
    xDimension = function(value) {
      if (missing(value)) {
        private$.xDimension
      } else {
        validateDimension(value)
        private$.xDimension <- value
        private$.xUnit <- getBaseUnit(value)
      }
    },
    #' @field xUnit Unit of x values
    xUnit = function(value) {
      if (missing(value)) {
        private$.xUnit
      } else {
        validateUnit(value, self$xDimension)
        private$.xUnit <- value
      }
    },
    #' @field yDimension Dimension of y values. See enum \code{ospDimensions} for the list of supported dimensions.
    yDimension = function(value) {
      if (missing(value)) {
        private$.yDimension
      } else {
        validateDimension(value)
        private$.yDimension <- value
        private$.yUnit <- getBaseUnit(value)
        private$.yErrorUnit <- getBaseUnit(value)
      }
    },
    #' @field yUnit Unit of y values
    yUnit = function(value) {
      if (missing(value)) {
        private$.yUnit
      } else {
        validateUnit(value, self$yDimension)
        private$.yUnit <- value
      }
    },
    #' @field yErrorUnit Unit of y error values
    yErrorUnit = function(value) {
      if (missing(value)) {
        private$.yErrorUnit
      } else {
        # Dimensionless is always supported as it represents the geometric error.
        if (value == "") {
          private$.yErrorUnit <- value
        } else {
          validateUnit(value, self$yDimension)
          private$.yErrorUnit <- value
        }
      }
    },
    #' @field MW Molecular weight in g/mol. Required for conversion between molar and mass dimensions. Can be \code{NULL} (default)
    MW = function(value) {
      if (missing(value)) {
        private$.MW
      } else {
        ospsuite:::validateIsNumeric(value)
        private$.MW <- value
      }
    }
  ),
  private = list(
    .xVals = NULL,
    .yVals = NULL,
    .yError = NULL,
    .dataType = NULL,
    .xDimension = NULL,
    .xUnit = NULL,
    .yDimension = NULL,
    .yUnit = NULL,
    .yErrorUnit = NULL,
    .MW = NULL
  ),
  public = list(
    #' @param xVals An array of numeric x values.
    #' @param yVals An array of numeric y values,
    #' @param yError An array of numeric values of the arithmetic error. Optional
    #' @param label A string that is used as a label (e.g. in the legend) for the data set
    #' @description
    #' Initialize a new instance of the class. xVals, yVals, and yError (optional) must be of the same length
    #' @return A new `XYData` object.
    initialize = function(xVals, yVals, label, yError = NULL) {
      ospsuite:::validateIsNumeric(c(xVals, yVals))
      ospsuite:::validateIsNumeric(yError, nullAllowed = TRUE)
      ospsuite:::validateIsSameLength(xVals, yVals)

      super$initialize(label)
      private$.xVals <- xVals
      private$.yVals <- yVals
      if (!is.null(yError)) {
        ospsuite:::validateIsSameLength(yVals, yError)
        # Replace all NAs with 0
        yError[is.na(yError)] <- 0
        self$yError <- yError
      }
      private$.dataType <- XYDataTypes$Unspecified

      self$xDimension <- ospDimensions$Time
      self$yDimension <- ospDimensions$Dimensionless
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
    #' x values with all conversions applied.
    #'
    #' @param unit Target unit. If \code{NULL} (default), no conversion between units is applied.
    #'
    #' @return Raw xValues plus xOffset multiplied by xFactor and converted to a specified unit.
    #' It is assumed that raw xValues are in \code{xUnit}.
    xValuesProcessed = function(unit = NULL) {
      # Add offset and multiply by the factor. The values are in the unit of XYData
      valuesProcessed <- (private$.xVals + self$xOffset) * self$xFactor

      # If a unit is passed and is different from the current unit, convert to unit
      if (!is.null(unit) && unit != private$.xUnit) {
        return(toUnit(
          quantityOrDimension = private$.xDimension,
          values = valuesProcessed,
          targetUnit = unit,
          sourceUnit = private$.xUnit
        ))
      }
      # Otherwise return without conversion to unit
      return(valuesProcessed)
    },

    #' @description
    #' y values with all conversions applied.
    #'
    #' @param unit Target unit. If \code{NULL} (default), no conversion between units is applied.
    #'
    #' @return Raw yValues plus yOffset multiplied by yFactor and converted to a specified unit.
    #' It is assumed that raw yValues are in \code{yUnit}.
    yValuesProcessed = function(unit = NULL) {
      # Add offset and multiply by the factor. The values are in the unit of XYData
      valuesProcessed <- (private$.yVals + self$yOffset) * self$yFactor

      # If a unit is passed and is different from the current unit, convert to unit
      if (!is.null(unit) && unit != private$.yUnit) {
        return(toUnit(
          quantityOrDimension = private$.yDimension,
          values = valuesProcessed,
          targetUnit = unit,
          sourceUnit = private$.yUnit,
          molWeight = private$.MW,
          molWeightUnit = ospUnits$`Molecular weight`$`g/mol`
        ))
      }
      # Otherwise return without conversion to unit
      return(valuesProcessed)
    },

    #' @description
    #' y error values with all conversions applied.
    #'
    #' @param unit Target unit. If \code{NULL} (default), the no conversion between units is applied.
    #'
    #' @return Raw yError plus yOffset multiplied by yFactor and converted to a specified unit.
    #' It is assumed that raw yError are in \code{yUnit}.
    yErrorProcessed = function(unit = NULL) {
      # Add offset and multiply by the factor. The values are in the unit of XYData
      valuesProcessed <- (private$.yError + self$yOffset) * self$yFactor

      # If a unit is passed and is different from the current unit, convert to unit
      if (!is.null(unit) && unit != private$.yUnit) {
        return(toUnit(
          quantityOrDimension = private$.yDimension,
          values = valuesProcessed,
          targetUnit = unit,
          sourceUnit = private$.yErrorUnit,
          molWeight = private$.MW,
          molWeightUnit = ospUnits$`Molecular weight`$`g/mol`
        ))
      }
      # Otherwise return without conversion to unit
      return(valuesProcessed)
    },

    #' @description Print the object to the console
    #' @param ... Rest arguments.
    print = function(...) {
      super$print()
      private$printLine("Data type", c(private$.dataType))
      # private$printLine("X values", c(private$.xVals))
      # private$printLine("Y values", c(private$.yVals))
      private$printLine("X dimension", c(private$.xDimension))
      private$printLine("X unit", c(private$.xUnit))
      private$printLine("Y dimension", c(private$.yDimension))
      private$printLine("Y unit", c(private$.yUnit))
      private$printLine("Y error unit", c(private$.yErrorUnit))
      invisible(self)
    }
  )
)

#' Possible entries for the \code{dataType} field of a \code{XYData} object
#' @export
XYDataTypes <- enum(list("Simulated", "Observed", "Unspecified"))
