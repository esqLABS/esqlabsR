#' @title DataConfiguration
#' @docType class
#' @description An object storing configuration of for observed data import
#' @export
#' @format NULL
DataConfiguration <- R6::R6Class(
  "DataConfiguration",
  inherit = Printable,
  cloneable = FALSE,
  active = list(),
  private = list(),
  public = list(
    #' @param dataFolder Path to the directory where the data file is located
    #' @param dataFile Name of the data excel file
    #' @param dataSheets Name of excel sheets to read
    #' @param compoundPropertiesFile Name of the excel file with compound properties
    #' @description
    #' Initialize a new instance of the class
    #' @return A new `DataConfiguration` object.
    initialize = function(dataFolder, dataFile, compoundPropertiesFile, dataSheets) {
      self$dataFolder <- dataFolder
      self$dataFile <- dataFile
      self$compoundPropertiesFile <- compoundPropertiesFile
      self$dataSheets <- dataSheets
      self$columnsToSplitBy <- esqlabsEnv$columnsToSplitDataBy
      self$XValuesColumn <- esqlabsEnv$XValuesColumn
      self$YValuesColumn <- esqlabsEnv$YValuesColumn
      self$YErrorColumn <- esqlabsEnv$YErrorColumn
    },

    #' @field dataFolder Path to the directory where the data file is located
    dataFolder = "",
    #' @field dataFile Name of the data excel file
    dataFile = "",
    #' @field compoundPropertiesFile Name of the excel file with compound properties
    compoundPropertiesFile = "Compound Properties.xlsx",
    #' @field dataSheets Name of excel sheets to read
    dataSheets = c(),
    #' @field columnsToSplitBy Column names by which the data will be split into groups
    columnsToSplitBy = "",
    #' @field XValuesColumn Column index for x values in observed data files
    XValuesColumn = 0,
    #' @field YValuesColumn Column index for y values in observed data files
    YValuesColumn = 0,
    #' @field YErrorColumn Column index for y error values in observed data files
    YErrorColumn = 0,

    #' @description
    #' Print the object to the console
    #' @param ... Rest arguments.
    print = function(...) {
      private$printClass()
      private$printLine("Data file directory", self$dataFolder)
      private$printLine("Data file", self$dataFile)
      private$printLine("Sheets", self$dataSheets)
      private$printLine("CompoundProperties file", self$compoundPropertiesFile)
      private$printLine("Columns to split by", self$columnsToSplitBy)
      private$printLine("X values column", self$XValuesColumn)
      private$printLine("Y values column", self$YValuesColumn)
      private$printLine("Y error column", self$YErrorColumn)
      invisible(self)
    }
  )
)
