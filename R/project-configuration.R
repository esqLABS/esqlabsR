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
        private$.projectConfigurationData$modelFolder$value <-
          value
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
        private$.projectConfigurationData$configurationsFolder$value <-
          value
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
        private$.projectConfigurationData$modelParamsFile$value <-
          value
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
        private$.projectConfigurationData$individualsFile$value <-
          value
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
        private$.projectConfigurationData$populationsFile$value <-
          value
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
        private$.projectConfigurationData$populationsFolder$value <-
          value
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
        private$.projectConfigurationData$scenariosFile$value <-
          value
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
        private$.projectConfigurationData$applicationsFile$value <-
          value
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
        private$.projectConfigurationData$plotsFile$value <-
          value
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
        private$.projectConfigurationData$dataFolder$value <-
          value
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
        private$.projectConfigurationData$dataFile$value <-
          value
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
        private$.projectConfigurationData$dataImporterConfigurationFile$value <-
          value
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
        private$.projectConfigurationData$outputFolder$value <-
          value
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

        # Delete previous properties
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
      path <- private$.clean_path(file_path, replace_env_var = FALSE)

      # Update private values
      private$.projectConfigurationFilePath <- path
      private$.projectConfigurationDirPath <- dirname(path)

      inputData <- readExcel(path = path)

      # Reset private variables
      private$.replaced_env_vars <- list()
      private$.projectConfigurationData <- list()

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
    .clean_path = function(path, parent = NULL, must_work = TRUE, replace_env_vars = TRUE) {
      # In case project configuration is initialized empty
      if (is.null(path) || is.na(path)) {
        return(NULL)
      }

      if (replace_env_vars) {
        path <- private$.replace_env_var(path)
      }

      if (is.null(parent) || is.na(parent) || fs::is_absolute_path(path)) {
        # When provided path is absolute or doesn't have parent directory, don't append parent
        abs_path <- fs::path_abs(path)
      } else {
        # When provided path is relative, append parent
        abs_path <- fs::path_abs(file.path(parent, path))
      }

      # Check whether the generated path exists
      if (!fs::file_exists(abs_path) && must_work == TRUE) {
        cli::cli_warn("{abs_path} does not exist.")
      }

      return(abs_path)
    },
    .replace_env_var = function(path) {
      # split path between each /
      path_split <- unlist(strsplit(path, "/"))
      for (i in seq_along(path_split)) {
        # Don't replace "path" in path_split[i] with PATH variable (windows)
        if (!stringr::str_detect(
          string = path_split[i],
          pattern = stringr::regex("path", ignore_case = T)
        )) {
          # check if path_split[i] is an environment variable
          if (Sys.getenv(path_split[i]) != "") {
            private$.replaced_env_vars[[path_split[i]]] <- Sys.getenv(path_split[i])
            path_split[i] <- Sys.getenv(path_split[i])
          }
        }
      }
      # reconstruct path with updated environment variables
      path <- paste(path_split, collapse = "/")
      return(path)
    },
    .replaced_env_vars = list()
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
    #' @param className Whether to print the name of the class at the beginning. default to TRUE.
    print = function(className = TRUE) {
      if (className) ospsuite.utils::ospPrintClass(self)
      ospsuite.utils::ospPrintItems(list(
        "Working Directory" = getwd(),
        "Project Configuration file stored at" = self$projectConfigurationFilePath
      ))

      ospsuite.utils::ospPrintHeader("Paths", level = 2)
      ospsuite.utils::ospPrintItems(list(
        "Configurations Folder" = self$configurationsFolder,
        "Model Folder" = self$modelFolder,
        "Data Folder" = self$dataFolder,
        "Output Folder" = self$outputFolder,
        "Populations Folder" = self$populationsFolder
      ), title = "Folders")

      ospsuite.utils::ospPrintItems(list(
        "Model Parameters File" = self$modelParamsFile,
        "Individuals File" = self$individualsFile,
        "Populations File" = self$populationsFile,
        "Scenarios File" = self$scenariosFile,
        "Applications File" = self$applicationsFile,
        "Plots File" = self$plotsFile,
        "Data File" = self$dataFile,
        "Data Importer Configuration File" = self$dataImporterConfigurationFile
      ), title = "Files")


      if (!isEmpty(private$.replaced_env_vars)) {
        cli::cli_h2("Environment Variables")
        cli::cli_inform("Environment variables were detected and replaced in paths:")
        purrr::iwalk(private$.replaced_env_vars, \(x, idx){
          cli::cli_li("{idx} to {x}")
        })
      }
      invisible(self)
    },
    #' @description Export ProjectConfiguration object to ProjectConfiguration.xlsx
    #' @param path a string representing the path or file name where to save the file. Can be absolute or relative (to working directory).
    #'
    #' @export
    save = function(path) {
      df <- data.frame(Property = character(), Value = character(), Description = character(), stringsAsFactors = FALSE)
      for (prop in c(
        "modelFolder",
        "configurationsFolder",
        "modelParamsFile",
        "individualsFile",
        "populationsFile",
        "populationsFolder",
        "scenariosFile",
        "applicationsFile",
        "plotsFile",
        "dataFolder",
        "dataFile",
        "dataImporterConfigurationFile",
        "outputFolder"
      )) {
        df <- rbind(
          df,
          data.frame(
            Property = prop,
            Value = private$.projectConfigurationData[[prop]]$value,
            Description = private$.projectConfigurationData[[prop]]$description
          )
        )
      }

      .writeExcel(df, path = path %||% self$projectConfigurationFilePath)
    }
  )
)
