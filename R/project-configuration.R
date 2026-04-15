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
        stop("projectConfigurationFilePath is readonly")
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
    #' @field modified Logical indicating whether any configuration properties
    #' have been modified since loading from file. Read-only.
    modified = function(value) {
      if (missing(value)) {
        private$.modified
      } else {
        stop("modified is readonly")
      }
    },
    #' @field modelFolder Path to the folder containing pkml simulation files.
    modelFolder = function(value) {
      if (!missing(value)) {
        private$.projectConfigurationData$modelFolder$value <-
          value
        private$.modified <- TRUE
      }
      private$.clean_path(
        private$.projectConfigurationData$modelFolder$value,
        self$projectConfigurationDirPath
      )
    },
    #' @field configurationsFolder Path to the folder containing configuration
    #'   files. Used by the Excel import/export bridge.
    configurationsFolder = function(value) {
      if (!missing(value)) {
        private$.projectConfigurationData$configurationsFolder$value <-
          value
        private$.modified <- TRUE
      }
      private$.clean_path(
        private$.projectConfigurationData$configurationsFolder$value,
        self$projectConfigurationDirPath
      )
    },
    #' @field modelParamsFile Path to the Excel file with global model
    #'   parameterization. Used by the Excel import/export bridge.
    modelParamsFile = function(value) {
      if (!missing(value)) {
        private$.projectConfigurationData$modelParamsFile$value <-
          value
        private$.modified <- TRUE
      }
      private$.clean_path(
        private$.projectConfigurationData$modelParamsFile$value,
        self$configurationsFolder
      )
    },
    #' @field individualsFile Path to the Excel file with individual-specific
    #'   model parameterization. Used by the Excel import/export bridge.
    individualsFile = function(value) {
      if (!missing(value)) {
        private$.projectConfigurationData$individualsFile$value <-
          value
        private$.modified <- TRUE
      }
      private$.clean_path(
        private$.projectConfigurationData$individualsFile$value,
        self$configurationsFolder
      )
    },
    #' @field populationsFile Path to the Excel file with population
    #'   information. Used by the Excel import/export bridge.
    populationsFile = function(value) {
      if (!missing(value)) {
        private$.projectConfigurationData$populationsFile$value <-
          value
        private$.modified <- TRUE
      }
      private$.clean_path(
        private$.projectConfigurationData$populationsFile$value,
        self$configurationsFolder
      )
    },
    #' @field populationsFolder Name of the folder containing population defined
    #'   through csv files.
    #' Must be located in the "configurationsFolder".
    populationsFolder = function(value) {
      if (!missing(value)) {
        private$.projectConfigurationData$populationsFolder$value <-
          value
        private$.modified <- TRUE
      }
      private$.clean_path(
        private$.projectConfigurationData$populationsFolder$value,
        self$configurationsFolder
      )
    },
    #' @field scenariosFile Path to the Excel file with scenario definitions.
    #'   Used by the Excel import/export bridge.
    scenariosFile = function(value) {
      if (!missing(value)) {
        private$.projectConfigurationData$scenariosFile$value <-
          value
        private$.modified <- TRUE
      }
      private$.clean_path(
        private$.projectConfigurationData$scenariosFile$value,
        self$configurationsFolder
      )
    },
    #' @field applicationsFile Path to the Excel file with scenario-specific
    #'   parameters such as application protocol parameters. Used by the
    #'   Excel import/export bridge.
    applicationsFile = function(value) {
      if (!missing(value)) {
        private$.projectConfigurationData$applicationsFile$value <-
          value
        private$.modified <- TRUE
      }
      private$.clean_path(
        private$.projectConfigurationData$applicationsFile$value,
        self$configurationsFolder
      )
    },
    #' @field plotsFile Path to the Excel file with plot definitions. Used by
    #'   the Excel import/export bridge.
    plotsFile = function(value) {
      if (!missing(value)) {
        private$.projectConfigurationData$plotsFile$value <-
          value
        private$.modified <- TRUE
      }
      private$.clean_path(
        private$.projectConfigurationData$plotsFile$value,
        self$configurationsFolder
      )
    },
    #' @field dataFolder Path to the folder where experimental data files are
    #'   located.
    dataFolder = function(value) {
      if (!missing(value)) {
        private$.projectConfigurationData$dataFolder$value <-
          value
        private$.modified <- TRUE
      }
      private$.clean_path(
        private$.projectConfigurationData$dataFolder$value,
        self$projectConfigurationDirPath
      )
    },
    #' @field dataFile Path to the Excel file with experimental (observed) data.
    #'   Must be located in the "dataFolder".
    dataFile = function(value) {
      if (!missing(value)) {
        private$.projectConfigurationData$dataFile$value <-
          value
        private$.modified <- TRUE
      }
      private$.clean_path(
        private$.projectConfigurationData$dataFile$value,
        self$dataFolder
      )
    },
    #' @field dataImporterConfigurationFile Name of data importer configuration
    #'   file in xml format used to load the data.
    #' Must be located in the "dataFolder"
    dataImporterConfigurationFile = function(value) {
      if (!missing(value)) {
        private$.projectConfigurationData$dataImporterConfigurationFile$value <-
          value
        private$.modified <- TRUE
      }
      private$.clean_path(
        private$.projectConfigurationData$dataImporterConfigurationFile$value,
        self$dataFolder
      )
    },
    #' @field outputFolder Path to the folder where the results should be saved
    #'   relative to the "Code" folder
    outputFolder = function(value) {
      if (!missing(value)) {
        private$.projectConfigurationData$outputFolder$value <-
          value
        private$.modified <- TRUE
      }
      private$.clean_path(
        private$.projectConfigurationData$outputFolder$value,
        self$projectConfigurationDirPath,
        must_work = FALSE
      )
    }
  ),
  private = list(
    .projectConfigurationData = NULL,
    .projectConfigurationFilePath = NULL,
    .projectConfigurationDirPath = NULL,
    .modified = FALSE,
    .warned_paths = character(),
    .clean_path = function(
      path,
      parent = NULL,
      must_work = TRUE,
      replace_env_vars = TRUE
    ) {
      # In case project configuration is initialized empty
      if (is.null(path) || is.na(path)) {
        return(NULL)
      }

      if (replace_env_vars) {
        path <- private$.replace_env_var(path)
      }

      if (is.null(parent) || is.na(parent) || fs::is_absolute_path(path)) {
        # When provided path is absolute or doesn't have parent directory, don't
        # append parent
        abs_path <- fs::path_abs(path)
      } else {
        # When provided path is relative, append parent
        abs_path <- fs::path_abs(file.path(parent, path))
      }

      # Check whether the generated path exists
      if (!fs::file_exists(abs_path) && must_work == TRUE) {
        # Only warn if we haven't already warned about this path
        if (!(abs_path %in% private$.warned_paths)) {
          warning(messages$fileNotFound(abs_path))
          private$.warned_paths <- c(private$.warned_paths, abs_path)
        }
      }

      return(abs_path)
    },
    .replace_env_var = function(path) {
      # split path between each /
      path_split <- unlist(strsplit(path, "/"))
      for (i in seq_along(path_split)) {
        # Don't replace "path" in path_split[i] with PATH variable (windows)
        if (
          !stringr::str_detect(
            string = path_split[i],
            pattern = stringr::regex("path", ignore_case = T)
          )
        ) {
          # check if path_split[i] is an environment variable
          if (Sys.getenv(path_split[i]) != "") {
            private$.replaced_env_vars[[path_split[
              i
            ]]] <- Sys.getenv(path_split[i])
            path_split[i] <- Sys.getenv(path_split[i])
          }
        }
      }
      # reconstruct path with updated environment variables
      path <- paste(path_split, collapse = "/")
      return(path)
    },
    .replaced_env_vars = list(),

    .read_json = function(jsonPath) {
      jsonPath <- fs::path_abs(jsonPath)
      if (!fs::file_exists(jsonPath)) {
        stop(messages$fileNotFound(jsonPath))
      }

      jsonData <- jsonlite::fromJSON(jsonPath, simplifyVector = FALSE)

      # Validate schema version
      if (is.null(jsonData$schemaVersion) || jsonData$schemaVersion != "2.0") {
        stop(paste0(
          "Unsupported or missing schemaVersion. Expected '2.0', got '",
          jsonData$schemaVersion %||% "NULL", "'."
        ))
      }

      # Check esqlabsRVersion for compatibility warning
      if (!is.null(jsonData$esqlabsRVersion)) {
        jsonMajor <- as.integer(strsplit(jsonData$esqlabsRVersion, "\\.")[[1]][[1]])
        pkgMajor <- as.integer(strsplit(
          as.character(utils::packageVersion("esqlabsR")), "\\."
        )[[1]][[1]])
        if (jsonMajor != pkgMajor) {
          warning(paste0(
            "JSON was created with esqlabsR v", jsonData$esqlabsRVersion,
            " but current version is v", utils::packageVersion("esqlabsR"),
            ". Some fields may not be compatible."
          ))
        }
      }

      self$jsonPath <- jsonPath
      private$.projectConfigurationFilePath <- jsonPath
      private$.projectConfigurationDirPath <- dirname(jsonPath)

      # Parse projectConfiguration paths
      pcData <- jsonData$projectConfiguration
      private$.projectConfigurationData <- list()
      for (prop in names(pcData)) {
        private$.projectConfigurationData[[prop]] <- list(
          value = pcData[[prop]],
          description = ""
        )
      }

      # Parse outputPaths
      if (!is.null(jsonData$outputPaths)) {
        self$outputPaths <- unlist(jsonData$outputPaths)
      }

      # Parse modelParameters
      self$modelParameters <- private$.parseParameterGroups(
        jsonData$modelParameters
      )

      # Parse individualParameterSets
      self$individualParameterSets <- private$.parseParameterGroups(
        jsonData$individualParameterSets
      )

      # Parse applications
      self$applications <- private$.parseParameterGroups(
        jsonData$applications
      )

      # Parse individuals
      self$individuals <- private$.parseIndividuals(jsonData$individuals)

      # Parse individual parameter set mapping
      self$individualParameterSetMapping <- private$.parseIndividualParameterSetMapping(
        jsonData$individuals
      )

      # Parse populations
      self$populations <- private$.parsePopulations(jsonData$populations)

      # Parse scenarios
      self$scenarios <- private$.parseScenarios(jsonData$scenarios)

      # Parse plots
      self$plots <- private$.parsePlots(jsonData$plots)

      private$.modified <- FALSE
    },

    .parseParameterGroups = function(groups) {
      if (is.null(groups)) return(list())
      result <- list()
      for (name in names(groups)) {
        entries <- groups[[name]]
        paths <- character(0)
        values <- numeric(0)
        units <- character(0)
        for (entry in entries) {
          paths <- c(paths, paste(
            entry$containerPath, entry$parameterName, sep = "|"
          ))
          values <- c(values, as.numeric(entry$value))
          units <- c(units, entry$units %||% "")
        }
        result[[name]] <- list(paths = paths, values = values, units = units)
      }
      result
    },

    .parseIndividuals = function(individualsData) {
      if (is.null(individualsData)) return(list())
      result <- list()
      for (entry in individualsData) {
        result[[entry$individualId]] <- list(
          species = entry$species,
          population = entry$population,
          gender = entry$gender,
          weight = as.double(entry$weight),
          height = as.double(entry$height),
          age = as.double(entry$age),
          proteinOntogenies = entry$proteinOntogenies
        )
      }
      result
    },

    .parseIndividualParameterSetMapping = function(individualsData) {
      if (is.null(individualsData)) return(list())
      result <- list()
      for (entry in individualsData) {
        setNames <- unlist(entry$parameterSets)
        result[[entry$individualId]] <- setNames %||% character(0)
      }
      result
    },

    .parsePopulations = function(populationsData) {
      if (is.null(populationsData)) return(list())
      result <- list()
      for (entry in populationsData) {
        popData <- list()
        for (field in names(entry)) {
          if (field == "populationId") next
          val <- entry[[field]]
          if (!is.null(val)) {
            numericFields <- c(
              "numberOfIndividuals", "proportionOfFemales",
              "weightMin", "weightMax", "heightMin", "heightMax",
              "ageMin", "ageMax", "BMIMin", "BMIMax"
            )
            if (field %in% numericFields) {
              val <- as.double(val)
            }
            popData[[field]] <- val
          }
        }
        result[[entry$populationId]] <- popData
      }
      result
    },

    .parseScenarios = function(scenariosData) {
      if (is.null(scenariosData)) return(list())
      result <- list()
      for (entry in scenariosData) {
        sc <- Scenario$new()
        sc$scenarioName <- entry$name
        sc$modelFile <- entry$modelFile
        sc$applicationProtocol <- entry$applicationProtocol %||% NA
        sc$individualId <- entry$individualId

        if (!is.null(entry$populationId)) {
          sc$populationId <- entry$populationId
          sc$simulationType <- "Population"
        }
        if (!is.null(entry$readPopulationFromCSV)) {
          sc$readPopulationFromCSV <- entry$readPopulationFromCSV
        }
        if (!is.null(entry$modelParameterGroups)) {
          sc$parameterGroups <- unlist(entry$modelParameterGroups)
        }
        if (!is.null(entry$simulationTime)) {
          sc$simulationTime <- .parseSimulationTimeIntervals(entry$simulationTime)
          sc$simulationTimeUnit <- entry$simulationTimeUnit
        }
        if (!is.null(entry$steadyState) && isTRUE(entry$steadyState)) {
          sc$simulateSteadyState <- TRUE
        }
        if (!is.null(entry$steadyStateTime)) {
          sc$steadyStateTime <- ospsuite::toBaseUnit(
            quantityOrDimension = ospDimensions$Time,
            values = entry$steadyStateTime,
            unit = entry$steadyStateTimeUnit
          )
        }
        if (!is.null(entry$overwriteFormulasInSS)) {
          sc$overwriteFormulasInSS <- entry$overwriteFormulasInSS
        }
        if (!is.null(entry$outputPathIds)) {
          pathIds <- unlist(entry$outputPathIds)
          sc$outputPaths <- unname(self$outputPaths[pathIds])
        }
        result[[entry$name]] <- sc
      }
      result
    },

    .parsePlots = function(plotsData) {
      if (is.null(plotsData)) return(NULL)
      list(
        dataCombined = private$.listOfListsToDataFrame(
          plotsData$dataCombined
        ),
        plotConfiguration = private$.listOfListsToDataFrame(
          plotsData$plotConfiguration
        ),
        plotGrids = private$.listOfListsToDataFrame(
          plotsData$plotGrids
        ),
        exportConfiguration = private$.listOfListsToDataFrame(
          plotsData$exportConfiguration
        )
      )
    },

    .listOfListsToDataFrame = function(data) {
      if (is.null(data) || length(data) == 0) {
        return(data.frame())
      }
      allCols <- unique(unlist(lapply(data, names)))
      rows <- lapply(data, function(entry) {
        row <- lapply(allCols, function(col) {
          val <- entry[[col]]
          if (is.null(val)) NA else val
        })
        names(row) <- allCols
        as.data.frame(row, stringsAsFactors = FALSE)
      })
      do.call(rbind, rows)
    }
  ),
  public = list(
    #' Initialize
    #'
    #' @param projectConfigurationFilePath A string representing the path to the
    #'   project configuration file.
    initialize = function(projectConfigurationFilePath = character()) {
      private$.modified <- FALSE
      if (!missing(projectConfigurationFilePath)) {
        private$.read_json(projectConfigurationFilePath)
      } else {
        private$.projectConfigurationDirPath <- NULL
      }
    },
    #' Print
    #' @description print prints a summary of the Project Configuration.
    #' @param className Whether to print the name of the class at the beginning.
    #'   default to TRUE.
    print = function(className = TRUE) {
      if (className) {
        ospsuite.utils::ospPrintClass(self)
      }
      ospsuite.utils::ospPrintItems(list(
        "Working Directory" = getwd(),
        "Project Configuration file stored at" = self$projectConfigurationFilePath
      ))

      ospsuite.utils::ospPrintHeader("Paths", level = 2)
      ospsuite.utils::ospPrintItems(
        list(
          "Configurations Folder" = self$configurationsFolder,
          "Model Folder" = self$modelFolder,
          "Data Folder" = self$dataFolder,
          "Output Folder" = self$outputFolder,
          "Populations Folder" = self$populationsFolder
        ),
        title = "Folders"
      )

      ospsuite.utils::ospPrintItems(
        list(
          "Model Parameters File" = self$modelParamsFile,
          "Individuals File" = self$individualsFile,
          "Populations File" = self$populationsFile,
          "Scenarios File" = self$scenariosFile,
          "Applications File" = self$applicationsFile,
          "Plots File" = self$plotsFile,
          "Data File" = self$dataFile,
          "Data Importer Configuration File" = self$dataImporterConfigurationFile
        ),
        title = "Files"
      )

      if (!isEmpty(private$.replaced_env_vars)) {
        cli::cli_h2("Environment Variables")
        message(
          "Environment variables were detected and replaced in paths:"
        )
        purrr::iwalk(private$.replaced_env_vars, \(x, idx) {
          cli::cli_li("{idx} to {x}")
        })
      }
      invisible(self)
    },
    #' @field scenarios Named list of `Scenario` objects, keyed by scenario
    #'   name. Populated by JSON loading.
    scenarios = NULL,
    #' @field modelParameters Named list of parameter structures, keyed by
    #'   sheet name. Each is a list with `paths`, `values`, `units` vectors.
    modelParameters = NULL,
    #' @field individuals Named list of `IndividualCharacteristics` objects,
    #'   keyed by individualId.
    individuals = NULL,
    #' @field individualParameterSets Named list of parameter structures,
    #'   keyed by set name. Each is a list with `paths`, `values`, `units`.
    individualParameterSets = NULL,
    #' @field populations Named list of `PopulationCharacteristics` objects,
    #'   keyed by populationId.
    populations = NULL,
    #' @field applications Named list of parameter structures, keyed by
    #'   protocol name. Each is a list with `paths`, `values`, `units`.
    applications = NULL,
    #' @field outputPaths Named character vector. Names are IDs, values are
    #'   output path strings.
    outputPaths = NULL,
    #' @field plots List with 4 data.frame elements: `dataCombined`,
    #'   `plotConfiguration`, `plotGrids`, `exportConfiguration`.
    plots = NULL,
    #' @field jsonPath Path to the source JSON file, or NULL if not loaded
    #'   from JSON.
    jsonPath = NULL,
    #' @field individualParameterSetMapping Named list mapping individualId
    #'   to a character vector of parameter set names.
    individualParameterSetMapping = NULL
  )
)

#' Parse simulation time intervals from a string
#' @param simulationTimeIntervalsString A string with format "start,end,res" or
#'   "start1,end1,res1;start2,end2,res2"
#' @returns A list of numeric vectors, or NULL if input is NULL.
#' @keywords internal
#' @noRd
.parseSimulationTimeIntervals <- function(simulationTimeIntervalsString) {
  if (is.null(simulationTimeIntervalsString)) {
    return(NULL)
  }
  simulationTimeIntervals <- strsplit(
    x = simulationTimeIntervalsString,
    split = ";",
    fixed = TRUE
  )[[1]]
  simulationTimeIntervals <- strsplit(
    x = simulationTimeIntervals,
    split = ",",
    fixed = TRUE
  )
  simulationTimeIntervals <- lapply(simulationTimeIntervals, as.numeric)
  validateIsNumeric(simulationTimeIntervals)
  if (any(unlist(simulationTimeIntervals) < 0)) {
    stop(messages$stopWrongTimeIntervalString(simulationTimeIntervalsString))
  }
  if (any(sapply(simulationTimeIntervals, length) != 3)) {
    stop(messages$stopWrongTimeIntervalString(simulationTimeIntervalsString))
  }
  if (any(sapply(simulationTimeIntervals, function(x) x[3] <= 0))) {
    stop(messages$stopWrongTimeIntervalString(simulationTimeIntervalsString))
  }
  if (any(sapply(simulationTimeIntervals, function(x) x[1] >= x[2]))) {
    stop(messages$stopWrongTimeIntervalString(simulationTimeIntervalsString))
  }
  return(simulationTimeIntervals)
}
