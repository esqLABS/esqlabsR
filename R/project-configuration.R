#' @title ProjectConfiguration
#' @docType class
#' @description An object storing configuration used project-wide
#' @format NULL
#' @import fs
#' @export
ProjectConfiguration <- R6::R6Class(
  "ProjectConfiguration",
  inherit = ospsuite.utils::Printable,
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
      if (missing(value)) {
        private$.clean_path(private$.modelFolder, self$projectConfigurationDirPath)
      } else {
        private$.clean_path(value, self$projectConfigurationDirPath)
        private$.modelFolder <- value
        self
      }
    },
    #' @field paramsFolder Path to the folder containing excel files with model
    #' parameterization;
    paramsFolder = function(value) {
      if (missing(value)) {
        private$.clean_path(private$.paramsFolder, self$projectConfigurationDirPath)
      } else {
        private$.clean_path(value, self$projectConfigurationDirPath)
        private$.paramsFolder <- value
        self
      }
    },
    #' @field paramsFile Name of the excel file with global model
    #' parameterization.
    #' Must be located in the "paramsFolder".
    paramsFile = function(value) {
      if (missing(value)) {
        private$.clean_path(private$.paramsFile, self$paramsFolder)
      } else {
        private$.clean_path(value, self$paramsFolder)
        private$.paramsFile <- value
        self
      }
    },
    #' @field individualsFile Name of the excel file with
    #' individual-specific model parameterization.
    #' Must be located in the "paramsFolder"
    individualsFile = function(value) {
      if (missing(value)) {
        private$.clean_path(private$.individualsFile, self$paramsFolder)
      } else {
        private$.clean_path(value, self$paramsFolder)
        private$.individualsFile <- value
        self
      }
    },
    #' @field populationParamsFile Name of the excel file with population
    #' information.
    #' Must be located in the "paramsFolder".
    populationParamsFile = function(value) {
      if (missing(value)) {
        private$.clean_path(private$.populationParamsFile, self$paramsFolder)
      } else {
        private$.clean_path(value, self$paramsFolder)
        private$.populationParamsFile <- value
        self
      }
    },
    #' @field scenarioDefinitionFile Name of the excel file with scenario
    #' definitions.
    #' Must be located in the "paramsFolder".
    scenarioDefinitionFile = function(value) {
      if (missing(value)) {
        private$.clean_path(private$.scenarioDefinitionFile, self$paramsFolder)
      } else {
        private$.clean_path(value, self$paramsFolder)
        private$.scenarioDefinitionFile <- value
        self
      }
    },
    #' @field scenarioApplicationsFile Name of the excel file scenario-specific
    #'  parameters such as application protocol parameters.
    #'  Must be located in the "paramsFolder".
    scenarioApplicationsFile = function(value) {
      if (missing(value)) {
        private$.clean_path(private$.scenarioApplicationsFile, self$paramsFolder)
      } else {
        private$.clean_path(value, self$paramsFolder)
        private$.scenarioApplicationsFile <- value
        self
      }
    },
    #' @field plotsFile Name of the excel file with plot definitions.
    #' Must be located in the "paramsFolder".
    plotsFile = function(value) {
      if (missing(value)) {
        private$.clean_path(private$.plotsFile, self$paramsFolder)
      } else {
        private$.clean_path(value, self$paramsFolder)
        private$.plotsFile <- value
        self
      }
    },
    #' @field dataFolder Path to the folder where experimental data files are
    #' located.
    dataFolder = function(value) {
      if (missing(value)) {
        private$.clean_path(private$.dataFolder, self$projectConfigurationDirPath)
      } else {
        private$.clean_path(value, self$projectConfigurationDirPath)
        private$.dataFolder <- value
        self
      }
    },
    #' @field dataFile Name of the excel file with experimental data.
    #' Must be located in the "dataFolder"
    dataFile = function(value) {
      if (missing(value)) {
        private$.clean_path(private$.dataFile, self$dataFolder)
      } else {
        private$.clean_path(value, self$dataFolder)
        private$.dataFile <- value
        self
      }
    },
    #' @field dataImporterConfigurationFile Name of data importer configuration
    #' file in xml format used to load the data.
    #' Must be located in the "dataFolder"
    dataImporterConfigurationFile = function(value) {
      if (missing(value)) {
        private$.clean_path(private$.dataImporterConfigurationFile, self$dataFolder)
      } else {
        private$.clean_path(value, self$dataFolder)
        private$.dataImporterConfigurationFile <- value
        self
      }
    },
    #' @field compoundPropertiesFile Path to the excel file containing
    #' information about all compounds in the model.
    #' Must be located in the "dataFolder"
    compoundPropertiesFile = function(value, must_work = FALSE) {
      if (missing(value)) {
        private$.clean_path(private$.compoundPropertiesFile, self$dataFolder, must_work)
      } else {
        private$.clean_path(value, self$dataFolder, must_work)
        private$.compoundPropertiesFile <- value
        self
      }
    },
    #' @field outputFolder Path to the folder where the results should be
    #' saved to; relative to the "Code" folder
    outputFolder = function(value) {
      if (missing(value)) {
        private$.clean_path(private$.outputFolder, self$projectConfigurationDirPath)
      } else {
        private$.clean_path(value, self$projectConfigurationDirPath)
        private$.outputFolder <- value
        self
      }
    }
  ),
  private = list(
    .projectConfigurationFilePath = NULL,
    .projectConfigurationDirPath = NULL,
    .modelFolder = NULL,
    .paramsFolder = NULL,
    .paramsFile = NULL,
    .individualsFile = NULL,
    .populationParamsFile = NULL,
    .scenarioDefinitionFile = NULL,
    .scenarioApplicationsFile = NULL,
    .plotsFile = NULL,
    .dataFolder = NULL,
    .dataFile = NULL,
    .dataImporterConfigurationFile = NULL,
    .compoundPropertiesFile = NULL,
    .outputFolder = NULL,
    .read_config = function(file_path) {
      path <- private$.clean_path(file_path)
      # Update private values
      private$.projectConfigurationFilePath <- path
      private$.projectConfigurationDirPath <- dirname(path)

      data <- readExcel(path = path)
      for (property in data$Property) {
        # Update each private property
        self[[property]] <- data[data$Property == property, ]$Value
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
        stop(abs_path, " does not exist")
      }
      if (!fs::file_exists(abs_path) && must_work == FALSE) {
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
      private$printClass()
      private$printLine("Relative path from working directory", getwd())
      private$printLine("Project Configuration File", fs::path_rel(as.character(self$projectConfigurationFilePath)))
      private$printLine("Model folder", fs::path_rel(as.character(self$modelFolder)))
      private$printLine("Parameters folder", fs::path_rel(as.character(self$paramsFolder)))
      private$printLine("Parameters file name", fs::path_rel(as.character(self$paramsFile)))
      private$printLine("Individual parameters file name", fs::path_rel(as.character(self$individualsFile)))
      private$printLine("Population parameters file name", fs::path_rel(as.character(self$populationParamsFile)))
      private$printLine("Scenario definitions file name", fs::path_rel(as.character(self$scenarioDefinitionFile)))
      private$printLine("Scenario applications definitions file name", fs::path_rel(as.character(self$scenarioApplicationsFile)))
      private$printLine("Plot definitions file name", fs::path_rel(as.character(self$plotsFile)))
      private$printLine("Experimental data folder", fs::path_rel(as.character(self$dataFolder)))
      private$printLine("Experimental data file", fs::path_rel(as.character(self$dataFile)))
      private$printLine("Data importer configuration", fs::path_rel(as.character(self$dataImporterConfigurationFile)))
      private$printLine("Compound Properties File", fs::path_rel(as.character(self$compoundPropertiesFile)))
      private$printLine("Output folder", fs::path_rel(as.character(self$outputFolder)))
      invisible(self)
    },
    #' @description Export ProjectConfiguration object to ProjectConfiguration.xlsx
    #'
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
      writeExcel(excel_file, path = self$projectConfigurationFilePath)
    }
  )
)
