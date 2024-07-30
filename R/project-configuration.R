#' @title ProjectConfiguration
#' @docType class
#' @description An object storing configuration used project-wide
#' @format NULL
#' @import fs
#' @export
ProjectConfiguration <- R6::R6Class(
  "ProjectConfiguration",
  cloneable = TRUE,
  active = list(
    #' @field projectConfigurationFilePath Path to the file that serve as base
    #' path for other parameters. If NULL, then, other paths should be absolute
    #'  paths.
    projectConfigurationFilePath = function(value) {
      if (missing(value)) {
        private$.projectConfigurationFilePath
      } else {
        private$.read_config(value)
        self
      }
    },
    #' @field projectConfigurationDirPath Path to the folder that serve as base
    #' path for other paths. If NULL, then, other paths should be absolute
    #' paths.
    projectConfigurationDirPath = function(value) {
      if (missing(value)) {
        private$.projectConfigurationDirPath
      } else {
        stop("projectConfigurationDirPath is readonly")
      }
    },
    #' @field modelFolder Path to the folder containing pkml simulation files.
    modelFolder = function(value) {
      if (!missing(value)) {
        private$.projectConfigurationData$modelFolder$value <- value
      }
      private$.clean_path(
        private$.projectConfigurationData$modelFolder$value,
        self$projectConfigurationDirPath
      )
    },
    #' @field configurationsFolder Path to the folder containing excel files with model
    #' parameterization;
    configurationsFolder = function(value) {
      if (!missing(value)) {
        private$.projectConfigurationData$configurationsFolder$value <- value
      }
      private$.clean_path(
        private$.projectConfigurationData$configurationsFolder$value,
        self$projectConfigurationDirPath
      )
    },
    #' @field modelParamsFile Name of the excel file with global model
    #' parameterization.
    #' Must be located in the "configurationsFolder".
    modelParamsFile = function(value) {

      if (!missing(value)) {
        private$.projectConfigurationData$modelParamsFil$valuee <- value
      }
      private$.clean_path(
        private$.projectConfigurationData$modelParamsFile$value,
        self$configurationsFolder
      )
    },
    #' @field individualsFile Name of the excel file with
    #' individual-specific model parameterization.
    #' Must be located in the "configurationsFolder"
    individualsFile = function(value) {
      if (!missing(value)) {
        private$.projectConfigurationData$individualsFile$value <- value
      }
      private$.clean_path(
        private$.projectConfigurationData$individualsFile$value,
        self$configurationsFolder
      )

    },
    #' @field populationsFile Name of the excel file with population
    #' information.
    #' Must be located in the "configurationsFolder".
    populationsFile = function(value) {
      if (!missing(value)) {
        private$.projectConfigurationData$populationsFile$value <- value
      }

      private$.clean_path(
        private$.projectConfigurationData$populationsFile$value,
        self$configurationsFolder
      )
    },
    #' @field populationsFolder Name of the folder containing population defined through csv files
    #' Must be located in the "configurationsFolder".
    populationsFolder = function(value) {
      if (!missing(value)) {
        private$.projectConfigurationData$populationsFolder$value <- value
      }
      private$.clean_path(
        private$.projectConfigurationData$populationsFolder$value,
        self$configurationsFolder
      )
    },
    #' @field scenariosFile Name of the excel file with scenario
    #' definitions.
    #' Must be located in the "configurationsFolder".
    scenariosFile = function(value) {
      if (!missing(value)) {
        private$.projectConfigurationData$scenariosFile$value <- value
      }
      private$.clean_path(
        private$.projectConfigurationData$scenariosFile$value,
        self$configurationsFolder
      )
    },
    #' @field applicationsFile Name of the excel file scenario-specific
    #'  parameters such as application protocol parameters.
    #'  Must be located in the "configurationsFolder".
    applicationsFile = function(value) {
      if (!missing(value)) {
        private$.projectConfigurationData$applicationsFile$value <- value
      }
      private$.clean_path(
        private$.projectConfigurationData$applicationsFile$value,
        self$configurationsFolder
      )
    },
    #' @field plotsFile Name of the excel file with plot definitions.
    #' Must be located in the "configurationsFolder".
    plotsFile = function(value) {
      if (!missing(value)) {
        private$.projectConfigurationData$plotsFile$value <- value
      }
      private$.clean_path(
        private$.projectConfigurationData$plotsFile$value,
        self$configurationsFolder
      )
    },
    #' @field dataFolder Path to the folder where experimental data files are
    #' located.
    dataFolder = function(value) {
      if (!missing(value)) {
        private$.projectConfigurationData$dataFolder$value <- value
      }
      private$.clean_path(
        private$.projectConfigurationData$dataFolder$value,
        self$projectConfigurationDirPath
      )
    },
    #' @field dataFile Name of the excel file with experimental data.
    #' Must be located in the "dataFolder"
    dataFile = function(value) {
      if (!missing(value)) {
        private$.projectConfigurationData$dataFile$value <- value
      }
      private$.clean_path(
        private$.projectConfigurationData$dataFile$value,
        self$dataFolder
      )
    },
    #' @field dataImporterConfigurationFile Name of data importer configuration
    #' file in xml format used to load the data.
    #' Must be located in the "dataFolder"
    dataImporterConfigurationFile = function(value) {
      if (!missing(value)) {
        private$.projectConfigurationData$dataImporterConfigurationFile$value <- value
      }
      private$.clean_path(
        private$.projectConfigurationData$dataImporterConfigurationFile$value,
        self$dataFolder
      )
    },
    #' @field outputFolder Path to the folder where the results should be
    #' saved to; relative to the "Code" folder
    outputFolder = function(value) {
      if (!missing(value)) {
        private$.projectConfigurationData$outputFolder$value <- value
      }
      private$.clean_path(private$.projectConfigurationData$outputFolder$value,
        self$projectConfigurationDirPath,
        must_work = FALSE
      )
    }
  ),
  private = list(
    .projectConfigurationData = NULL,
    .projectConfigurationFilePath = NULL,
    .projectConfigurationDirPath = NULL,
    .checkProjectConfigurationFile = function() {
      data <- private$.projectConfigurationData

      # Check if read data is from V5, if yes, load and rename the objects to
      # make it compatible with v6.
      if (all(c(
        "paramsFolder",
        "paramsFile",
        "individualsFile",
        "populationParamsFile",
        "scenarioDefinitionFile",
        "scenarioApplicationsFile",
        "plotsFile",
        "dataFolder",
        "dataFile",
        "dataImporterConfigurationFile",
        "compoundPropertiesFile",
        "outputFolder"
      ) %in% names(data))) {
        cli::cli_warn(c(
          "!" = "The project configuration file layout used is from an older version of the package.",
          "i" = "This version is still supported and will be loaded but it is recommended to update the project configuration file.
                           To do so, use the {.code $save} method of the project configuration object."
        ))


        data$configurationsFolder <- data$paramsFolder
        data$modelParamsFile <- data$paramsFile
        data$populationsFile <- data$populationParamsFile
        data$scenariosFile <- data$scenarioDefinitionFile
        data$applicationsFile <- data$scenarioApplicationsFile
        data$populationsFolder <- list(value = NA, description = "Name of the folder containing population defined in files")

        data$paramsFolder <- NULL
        data$paramsFile <- NULL
        data$populationParamsFile <- NULL
        data$scenarioDefinitionFile <- NULL
        data$scenarioApplicationsFile <- NULL
        data$compoundPropertiesFile <- NULL
      }

      # If one of the excel configuration is not expected, return an error.
      for (property in names(data)) {
        if (!(property %in% names(self))) {
          cli::cli_abort(c("x" = "Property {property} is not a valid configuration property for {self$projectConfigurationFilePath}"))
        }
      }
      private$.projectConfigurationData <- data
    },
    .read_config = function(file_path) {
      path <- private$.clean_path(file_path)
      # Update private values
      private$.projectConfigurationFilePath <- path
      private$.projectConfigurationDirPath <- dirname(path)

      inputData <- readExcel(path = path)

      .projectConfigurationData <- list()

      for (property in inputData$Property) {
        private$.projectConfigurationData[[property]] <- list(
          value = inputData$Value[inputData$Property == property],
          description = inputData$Description[inputData$Property == property]
        )
      }

      private$.checkProjectConfigurationFile()

      for (property in colnames(private$.projectConfigurationData)) {
        # Update each private property
        self[[property]] <- private$.projectConfigurationData[[property]]$value
      }
    },
    .clean_path = function(path, parent = NULL, must_work = TRUE) {
      # In case project configuration is initialized empty
      if (is.null(path) || is.na(path)) {
        return(NULL)
      }
      if (is.null(parent) || is.na(parent) || fs::is_absolute_path(path)) {
        # When provided path is absolute or doesn't have parent directory, don't append parent
        abs_path <- fs::path_abs(path)
      } else {
        # When provided path is relative, append parent
        abs_path <- fs::path_abs(file.path(parent, path))
      }

      # Check wether the generated path exists
      if (!fs::file_exists(abs_path) && must_work == TRUE) {
        warning(abs_path, " does not exist")
      }

      return(abs_path)
    }
  ),
  public = list(
    #' Initialize
    #'
    #' @param projectConfigurationFilePath A string representing the path to the
    #' project configuration file.
    initialize = function(projectConfigurationFilePath = character()) {
      if (!missing(projectConfigurationFilePath)) {
        self$projectConfigurationFilePath <- projectConfigurationFilePath
      } else {
        private$.projectConfigurationDirPath <- NULL
      }
    },
    #' Print
    #' @description print prints a summary of the Project Configuration.
    print = function() {
      cli_h1("Project Configuration")
      cli_ul()
      cli_li("Project Configuration: {fs::path_rel(as.character(self$projectConfigurationFilePath))}")
      cli_li("Model folder: {fs::path_rel(as.character(self$modelFolder))}")
      cli_li("Configurations folder: {fs::path_rel(as.character(self$configurationsFolder))}")
      cli_li("Model Parameters: {fs::path_rel(as.character(self$modelParamsFile))}")
      cli_li("Individuals: {fs::path_rel(as.character(self$individualsFile))}")
      cli_li("Populations: {fs::path_rel(as.character(self$populationsFile))}")
      cli_li("PopulationsFolder: {fs::path_rel(as.character(self$populationsFolder))}")
      cli_li("Scenarios: {fs::path_rel(as.character(self$scenariosFile))}")
      cli_li("Applications: {fs::path_rel(as.character(self$applicationsFile))}")
      cli_li("Plots: {fs::path_rel(as.character(self$plotsFile))}")
      cli_li("Data folder: {fs::path_rel(as.character(self$dataFolder))}")
      cli_li("Data file: {fs::path_rel(as.character(self$dataFile))}")
      cli_li("Data importer configuration: {fs::path_rel(as.character(self$dataImporterConfigurationFile))}")
      cli_li("Output folder: {fs::path_rel(as.character(self$outputFolder))}")
      cli_end()
    },
    #' @description Export ProjectConfiguration object to ProjectConfiguration.xlsx
    #'
    #' @param path A string representing the path where to save the file.
    #' @export
    save = function() {
      excel_file <- readExcel(path = self$projectConfigurationFilePath)

      for (prop in excel_file$Property) {
        path <- ""

        if (!is.null(self[[prop]])) {
          if (fs::is_dir(self[[prop]])) {
            # if property is a directory, save relative path from ProjectConf dir
            path <- fs::path_rel(
              path = self[[prop]],
              start = self$projectConfigurationDirPath
            )
          } else if (fs::is_file(self[[prop]])) {
            # if property is a file, then save only its name
            path <- basename(self[[prop]])
          }
        }

        excel_file[excel_file$Property == prop, ]$Value <- path
      }
      .writeExcel(excel_file, path = self$projectConfigurationFilePath)
    }
  )
)

#' Create a default `ProjectConfiguration`
#'
#' @inheritParams readExcel
#'
#' @details Create a `ProjectConfiguration` based on the `"ProjectConfiguration.xlsx"`
#' located in the "Code" folder.
#'
#' @return Object of type `ProjectConfiguration`
#' @export
createDefaultProjectConfiguration <- function(path = file.path("ProjectConfiguration.xlsx")) {
  lifecycle::deprecate_soft(
    what = "createDefaultProjectConfiguration()",
    with = "createProjectConfiguration()",
    when = "5.1.4"
  )
  return(createProjectConfiguration(path))
}


#' #' Create a `ProjectConfiguration`
#'
#' @param path path to the `ProjectConfiguration.xlsx` file. default to the `ProjectConfiguration.xlsx` file located in the working directory.
#'
#' @details Create a `ProjectConfiguration` based on the `"ProjectConfiguration.xlsx"`
#'
#' @return Object of type `ProjectConfiguration`
#' @export
createProjectConfiguration <- function(path = file.path("ProjectConfiguration.xlsx")) {
  projectConfiguration <- ProjectConfiguration$new(projectConfigurationFilePath = path)
  return(projectConfiguration)
}
