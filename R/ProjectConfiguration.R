#' @title ProjectConfiguration
#' @docType class
#' @description An object storing configuration used project-wide
#' @format NULL
#' @export
ProjectConfiguration <- R6::R6Class(
  "ProjectConfiguration",
  inherit = ospsuite.utils::Printable,
  cloneable = TRUE,
  public = list(
    #' @field modelFolder Path to the folder with pkml simulation files;
    #' relative to the "Code" folder.
    modelFolder = NULL,
    #' @field paramsFolder Path to the folder with excel files with
    #' parametrization; relative to the "Code" folder.
    paramsFolder = NULL,
    #' @field paramsFile Name of the excel file with global model
    #' parametrization. Must be located in the "paramsFolder".
    paramsFile = NULL,
    #' @field individualParamsFile Name of the excel file with
    #' individual-specific model parametrization. Must be located in the
    #' "paramsFolder"
    individualParamsFile = NULL,
    #' @field individualPhysiologyFile Name of the excel file with
    #' individual physiology information. Must be located in the "paramsFolder".
    individualPhysiologyFile = NULL,
    #' @field populationParamsFile Name of the excel file with population
    #' information. Must be located in the "paramsFolder".
    populationParamsFile = NULL,
    #' @field scenarioDefinitionFile Name of the excel file with scenario
    #' definitions. Must be located in the "paramsFolder".
    scenarioDefinitionFile = NULL,
    #' @field scenarioApplicationsFile Name of the excel file scenario-specific
    #'  parameters such as application protocol parameters. Must be located in
    #'   the "paramsFolder".
    scenarioApplicationsFile = NULL,
    #' @field dataFolder Path to the folder where experimental data files are located
    dataFolder = NULL,
    #' @field dataFile Name of the excel file with experimental data
    dataFile = NULL,
    #' @field dataImporterConfigurationFile Name of data importer configuration
    #' file in xml format used to load the data. Must be located in the "dataFolder"
    dataImporterConfigurationFile = NULL,
    #' @field compoundPropertiesFile Path to the excel file containing information
    #' about all compounds in the model. Must be located in the "dataFolder"
    compoundPropertiesFile = NULL,
    #' @field outputFolder Path to the folder where the results should be
    #' saved to; relative to the "Code" folder
    outputFolder = NULL,

    #' @description
    #' Print the object to the console
    #' @param ... Rest arguments.
    print = function(...) {
      private$printClass()
      private$printLine("Model folder", self$modelFolder)
      private$printLine("Parameters folder", self$paramsFolder)
      private$printLine("Parameters file name", self$paramsFile)
      private$printLine("Individual parameters file name", self$individualParamsFile)
      private$printLine("Individual physiology file name", self$individualPhysiologyFile)
      private$printLine("Population parameters file name", self$populationParamsFile)
      private$printLine("Scenario definitions file name", self$scenarioDefinitionFile)
      private$printLine("Scenario applications definitions file name", self$scenarioApplicationsFile)
      private$printLine("Experimental data folder", self$dataFolder)
      private$printLine("Experimental data file", self$dataFile)
      private$printLine("Data importer configuration", self$dataImporterConfigurationFile)
      private$printLine("Output folder", self$outputFolder)
      invisible(self)
    }
  )
)
