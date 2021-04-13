#' @title OSPSTimeValues
#' @docType class
#' @description A set of x and y value pairs with the properties as defined in the esqLABS "Time-Values Data"
#' @export
#' @format NULL
OSPSTimeValues <- R6::R6Class(
  "OSPSTimeValues",
  inherit = XYData,
  cloneable = FALSE,
  active = list(),
  private = list(),
  public = list(
    #' @description
    #' Numerical factor for converting the y-values to a specific unit of a specific dimension
    #'
    #' @param dimension Target dimension
    #' @param unit Target unit
    #'
    #' @return yFactor multiplied by the factor for conversion from dimensions multiplied by the factors for the conversion between units.
    yUnitDimensionFactor = function(dimension, unit, ...) {
      super$yUnitDimensionFactor(dimension, unit, self$MW)
    },

    #' @description
    #' Numerical factor for converting the y error values to a specific unit of a specific dimension
    #'
    #' @param dimension Target dimension
    #' @param unit Target unit
    #'
    #' @return yFactor multiplied by the factor for conversion from dimensions multiplied by the factors for the conversion between units.
    yErrorUnitDimensionFactor = function(dimension, unit, ...) {
      super$yErrorUnitDimensionFactor(dimension, unit, self$MW)
    },

    #' @description
    #' y values with all conversions applied.
    #'
    #' @param dimension Target dimension. If \code{NULL} (default), the no conversion between dimensions is applied.
    #' @param unit Target unit. If \code{NULL} (default), the no conversion between units is applied. If one of \code{dimension}
    #' or code{unit} is specified, the other must be, too.
    #'
    #' @return Raw yValues plus yOffset multiplied by yFactor and converted to a specified unit/dimension.
    #' It is assumed that raw yValues are in the yDimension and yUnit.
    yValuesProcessed = function(dimension = NULL, unit = NULL) {
      super$yValuesProcessed(dimension, unit, self$MW)
    },

    #' @description
    #' y error values with all conversions applied.
    #'
    #' @param dimension Target dimension. If \code{NULL} (default), the no conversion between dimensions is applied.
    #' @param unit Target unit. If \code{NULL} (default), the no conversion between units is applied. If one of \code{dimension}
    #' or code{unit} is specified, the other must be, too.
    #'
    #' @return Raw yError plus yOffset multiplied by yFactor and converted to a specified unit/dimension.
    #' It is assumed that raw yError are in the yDimension and yErrorUnit.
    yErrorProcessed = function(dimension = NULL, unit = NULL) {
      super$yErrorProcessed(dimension, unit, self$MW)
    },

    #' @param xVals An array of numeric x values.
    #'
    #' @param yVals An array of numeric y values,
    #' @param yError An array of numeric values of the arithmetic error. Optional
    #' @param label A string that is used as a label (e.g. in the legend) for the data set
    #'
    #' @description
    #' Initialize a new instance of the class. xVals, yVals, and yError (can be omitted) must be of the same length
    #' @return A new `OSPSTimeValues` object.
    initialize = function(xVals, yVals, label, yError = NULL) {
      super$initialize(xVals, yVals, label, yError)
    },

    #' @field StudyId Id of the study
    StudyId = NULL,
    #' @field PatientId Id of the patient
    PatientId = NULL,
    #' @field Organ Organ of the measurement
    Organ = NULL,
    #' @field Compartment Compartment of the measurement
    Compartment = NULL,
    #' @field Species Species
    Species = NULL,
    #' @field Gender Gender
    Gender = NULL,
    #' @field Molecule Measured molecule
    Molecule = NULL,
    #' @field MW Molecular weight of the measured molecule in g/mol
    MW = NULL,
    #' @field GroupId Id of the group
    GroupId = NULL,

    #' @description Print the object to the console
    #' @param ... Rest arguments.
    print = function(...) {
      super$print()
      private$printLine("Group Id", self$GroupId)
      private$printLine("Study Id", self$StudyId)
      private$printLine("Species", self$Species)
      private$printLine("Patient Id", self$PatientId)
      private$printLine("Organ", self$Organ)
      private$printLine("Compartment", self$Compartment)
      private$printLine("Gender", self$Gender)
      private$printLine("Molecule", self$Molecule)
      private$printLine("Molecular weight", self$MW)
      invisible(self)
    }
  )
)
