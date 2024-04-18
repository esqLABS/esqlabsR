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
        value <- private$.modelFolder
      }
      private$.modelFolder <- value
      private$.clean_path(value, self$projectConfigurationDirPath)
    },
    #' @field paramsFolder Path to the folder containing excel files with model
    #' parameterization;
    paramsFolder = function(value) {
      if (missing(value)) {
        value <- private$.paramsFolder
      }
      private$.paramsFolder <- value
      private$.clean_path(value, self$projectConfigurationDirPath)
    },
    #' @field modelParamsFile Name of the excel file with global model
    #' parameterization.
    #' Must be located in the "paramsFolder".
    modelParamsFile = function(value) {
      if (missing(value)) {
        value <- private$.modelParamsFile
      }
      private$.modelParamsFile <- value
      private$.clean_path(value, self$paramsFolder)
    },
    #' @field individualsFile Name of the excel file with
    #' individual-specific model parameterization.
    #' Must be located in the "paramsFolder"
    individualsFile = function(value) {
      if (missing(value)) {
        value <- private$.individualsFile
      }
      private$.individualsFile <- value
      private$.clean_path(value, self$paramsFolder)
    },
    #' @field populationsFile Name of the excel file with population
    #' information.
    #' Must be located in the "paramsFolder".
    populationsFile = function(value) {
      if (missing(value)) {
        value <- private$.populationsFile
      }
      private$.populationsFile <- value
      private$.clean_path(value, self$paramsFolder)
    },
    #' @field scenariosFile Name of the excel file with scenario
    #' definitions.
    #' Must be located in the "paramsFolder".
    scenariosFile = function(value) {
      if (missing(value)) {
        value <- private$.scenariosFile
      }
      private$.scenariosFile <- value
      private$.clean_path(value, self$paramsFolder)
    },
    #' @field applicationsFile Name of the excel file scenario-specific
    #'  parameters such as application protocol parameters.
    #'  Must be located in the "paramsFolder".
    applicationsFile = function(value) {
      if (missing(value)) {
        value <- private$.applicationsFile
      }
      private$.applicationsFile <- value
      private$.clean_path(value, self$paramsFolder)
    },
    #' @field plotsFile Name of the excel file with plot definitions.
    #' Must be located in the "paramsFolder".
    plotsFile = function(value) {
      if (missing(value)) {
        value <- private$.plotsFile
      }
      private$.plotsFile <- value
      private$.clean_path(value, self$paramsFolder)
    },
    #' @field dataFolder Path to the folder where experimental data files are
    #' located.
    dataFolder = function(value) {
      if (missing(value)) {
        value <- private$.dataFolder
      }
      private$.dataFolder <- value
      private$.clean_path(value, self$projectConfigurationDirPath)
    },
    #' @field dataFile Name of the excel file with experimental data.
    #' Must be located in the "dataFolder"
    dataFile = function(value) {
      if (missing(value)) {
        value <- private$.dataFile
      }
      private$.dataFile <- value
      private$.clean_path(value, self$dataFolder)
    },
    #' @field dataImporterConfigurationFile Name of data importer configuration
    #' file in xml format used to load the data.
    #' Must be located in the "dataFolder"
    dataImporterConfigurationFile = function(value) {
      if (missing(value)) {
        value <- private$.dataImporterConfigurationFile
      }
      private$.dataImporterConfigurationFile <- value
      private$.clean_path(value, self$dataFolder)
    },
    #' @field compoundPropertiesFile Path to the excel file containing
    #' information about all compounds in the model.
    #' Must be located in the "dataFolder"
    compoundPropertiesFile = function(value) {
      if (missing(value)) {
        value <- private$.compoundPropertiesFile
      }
      private$.compoundPropertiesFile <- value
      private$.clean_path(value, self$dataFolder, must_work = FALSE)
    },
    #' @field outputFolder Path to the folder where the results should be
    #' saved to; relative to the "Code" folder
    outputFolder = function(value) {
      if (missing(value)) {
        value <- private$.outputFolder
      }
      private$.outputFolder <- value
      private$.clean_path(value, self$projectConfigurationDirPath, must_work = FALSE)
    }
  ),
  private = list(
    .projectConfigurationFilePath = NULL,
    .projectConfigurationDirPath = NULL,
    .modelFolder = NULL,
    .paramsFolder = NULL,
    .modelParamsFile = NULL,
    .individualsFile = NULL,
    .populationsFile = NULL,
    .scenariosFile = NULL,
    .applicationsFile = NULL,
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
      private$printLine("Model Parameters", fs::path_rel(as.character(self$modelParamsFile)))
      private$printLine("Individuals", fs::path_rel(as.character(self$individualsFile)))
      private$printLine("Populations", fs::path_rel(as.character(self$populationsFile)))
      private$printLine("Scenarios", fs::path_rel(as.character(self$scenariosFile)))
      private$printLine("Applications", fs::path_rel(as.character(self$applicationsFile)))
      private$printLine("Plots", fs::path_rel(as.character(self$plotsFile)))
      private$printLine("Data folder", fs::path_rel(as.character(self$dataFolder)))
      private$printLine("Data file", fs::path_rel(as.character(self$dataFile)))
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
