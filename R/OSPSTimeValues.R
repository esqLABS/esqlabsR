#' @title OSPSTimeValues
#' @docType class
#' @description A set of x and y value pairs with the properties as defined in the esqLABS "Time-Values Data"
#' @export
#' @format NULL
OSPSTimeValues <- R6::R6Class(
  "OSPSTimeValues",
  inherit = XYData,
  active = list(),
  private = list(),
  public = list(
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
