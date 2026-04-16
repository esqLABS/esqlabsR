#' @title Project
#' @docType class
#' @description An R6 class representing an esqlabsR project
#' @format NULL
#' @import fs
#' @export
Project <- R6::R6Class(
  "Project",
  cloneable = TRUE,
  active = list(
    #' @field projectFilePath Path to the file that serve as base
    #' path for other parameters. If NULL, then, other paths should be absolute
    #'  paths.
    projectFilePath = function(value) {
      if (missing(value)) {
        private$.projectFilePath
      } else {
        stop("projectFilePath is readonly")
      }
    },
    #' @field projectDirPath Path to the folder that serve as base
    #' path for other paths. If NULL, then, other paths should be absolute
    #' paths.
    projectDirPath = function(value) {
      if (missing(value)) {
        private$.projectDirPath
      } else {
        stop("projectDirPath is readonly")
      }
    },
    #' @field modified Logical indicating whether any configuration properties
    #' have been modified since loading from file.
    modified = function(value) {
      if (missing(value)) {
        private$.modified
      } else {
        stopifnot(is.logical(value), length(value) == 1L)
        private$.modified <- value
      }
    },
    #' @field modelFolder Path to the folder containing pkml simulation files.
    modelFolder = function(value) {
      if (!missing(value)) {
        private$.filePathsData$modelFolder$value <-
          value
        private$.modified <- TRUE
      }
      private$.clean_path(
        private$.filePathsData$modelFolder$value,
        self$projectDirPath
      )
    },
    #' @field configurationsFolder Path to the folder containing configuration
    #'   files. Used by the Excel import/export bridge.
    configurationsFolder = function(value) {
      if (!missing(value)) {
        private$.filePathsData$configurationsFolder$value <-
          value
        private$.modified <- TRUE
      }
      private$.clean_path(
        private$.filePathsData$configurationsFolder$value,
        self$projectDirPath
      )
    },
    #' @field modelParamsFile Path to the Excel file with global model
    #'   parameterization. Used by the Excel import/export bridge.
    modelParamsFile = function(value) {
      if (!missing(value)) {
        private$.filePathsData$modelParamsFile$value <-
          value
        private$.modified <- TRUE
      }
      private$.clean_path(
        private$.filePathsData$modelParamsFile$value,
        self$configurationsFolder
      )
    },
    #' @field individualsFile Path to the Excel file with individual-specific
    #'   model parameterization. Used by the Excel import/export bridge.
    individualsFile = function(value) {
      if (!missing(value)) {
        private$.filePathsData$individualsFile$value <-
          value
        private$.modified <- TRUE
      }
      private$.clean_path(
        private$.filePathsData$individualsFile$value,
        self$configurationsFolder
      )
    },
    #' @field populationsFile Path to the Excel file with population
    #'   information. Used by the Excel import/export bridge.
    populationsFile = function(value) {
      if (!missing(value)) {
        private$.filePathsData$populationsFile$value <-
          value
        private$.modified <- TRUE
      }
      private$.clean_path(
        private$.filePathsData$populationsFile$value,
        self$configurationsFolder
      )
    },
    #' @field populationsFolder Name of the folder containing population defined
    #'   through csv files.
    #' Must be located in the "configurationsFolder".
    populationsFolder = function(value) {
      if (!missing(value)) {
        private$.filePathsData$populationsFolder$value <-
          value
        private$.modified <- TRUE
      }
      private$.clean_path(
        private$.filePathsData$populationsFolder$value,
        self$configurationsFolder
      )
    },
    #' @field scenariosFile Path to the Excel file with scenario definitions.
    #'   Used by the Excel import/export bridge.
    scenariosFile = function(value) {
      if (!missing(value)) {
        private$.filePathsData$scenariosFile$value <-
          value
        private$.modified <- TRUE
      }
      private$.clean_path(
        private$.filePathsData$scenariosFile$value,
        self$configurationsFolder
      )
    },
    #' @field applicationsFile Path to the Excel file with scenario-specific
    #'   parameters such as application protocol parameters. Used by the
    #'   Excel import/export bridge.
    applicationsFile = function(value) {
      if (!missing(value)) {
        private$.filePathsData$applicationsFile$value <-
          value
        private$.modified <- TRUE
      }
      private$.clean_path(
        private$.filePathsData$applicationsFile$value,
        self$configurationsFolder
      )
    },
    #' @field plotsFile Path to the Excel file with plot definitions. Used by
    #'   the Excel import/export bridge.
    plotsFile = function(value) {
      if (!missing(value)) {
        private$.filePathsData$plotsFile$value <-
          value
        private$.modified <- TRUE
      }
      private$.clean_path(
        private$.filePathsData$plotsFile$value,
        self$configurationsFolder
      )
    },
    #' @field dataFolder Path to the folder where experimental data files are
    #'   located.
    dataFolder = function(value) {
      if (!missing(value)) {
        private$.filePathsData$dataFolder$value <-
          value
        private$.modified <- TRUE
      }
      private$.clean_path(
        private$.filePathsData$dataFolder$value,
        self$projectDirPath
      )
    },
    #' @field dataFile Path to the Excel file with experimental (observed) data.
    #'   Must be located in the "dataFolder".
    dataFile = function(value) {
      if (!missing(value)) {
        private$.filePathsData$dataFile$value <-
          value
        private$.modified <- TRUE
      }
      private$.clean_path(
        private$.filePathsData$dataFile$value,
        self$dataFolder
      )
    },
    #' @field dataImporterConfigurationFile Name of data importer configuration
    #'   file in xml format used to load the data.
    #' Must be located in the "dataFolder"
    dataImporterConfigurationFile = function(value) {
      if (!missing(value)) {
        private$.filePathsData$dataImporterConfigurationFile$value <-
          value
        private$.modified <- TRUE
      }
      private$.clean_path(
        private$.filePathsData$dataImporterConfigurationFile$value,
        self$dataFolder
      )
    },
    #' @field outputFolder Path to the folder where the results should be saved
    #'   relative to the "Code" folder
    outputFolder = function(value) {
      if (!missing(value)) {
        private$.filePathsData$outputFolder$value <-
          value
        private$.modified <- TRUE
      }
      private$.clean_path(
        private$.filePathsData$outputFolder$value,
        self$projectDirPath,
        must_work = FALSE
      )
    }
  ),
  private = list(
    .filePathsData = NULL,
    .projectFilePath = NULL,
    .projectDirPath = NULL,
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

      self$schemaVersion <- jsonData$schemaVersion
      self$esqlabsRVersion <- jsonData$esqlabsRVersion

      self$jsonPath <- jsonPath
      private$.projectFilePath <- jsonPath
      private$.projectDirPath <- dirname(jsonPath)

      # Parse filePaths
      pcData <- jsonData$filePaths
      private$.filePathsData <- list()
      for (prop in names(pcData)) {
        private$.filePathsData[[prop]] <- list(
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

      # Parse observedData
      self$observedData <- private$.parseObservedData(jsonData$observedData)

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
          if (is.null(entry$steadyStateTimeUnit)) {
            stop(
              "Scenario '", entry$name, "' has 'steadyStateTime' set but ",
              "'steadyStateTimeUnit' is null. Please specify a unit (e.g. \"min\")."
            )
          }
          sc$steadyStateTime <- ospsuite::toBaseUnit(
            quantityOrDimension = ospDimensions$Time,
            values = entry$steadyStateTime,
            unit = entry$steadyStateTimeUnit
          )
          sc$steadyStateTimeUnit <- entry$steadyStateTimeUnit
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

    .parseObservedData = function(observedDataConfig) {
      if (is.null(observedDataConfig)) return(list())
      for (entry in observedDataConfig) {
        if (is.null(entry$type) || !entry$type %in% c("excel", "pkml")) {
          stop("Each observedData entry must have a 'type' of 'excel' or 'pkml'.")
        }
        if (entry$type == "excel" && is.null(entry$sheets)) {
          stop("Excel observedData entries must have a 'sheets' field.")
        }
        if (entry$type == "pkml" && is.null(entry$file)) {
          stop("PKML observedData entries must have a 'file' field.")
        }
      }
      observedDataConfig
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
    #' @description Add a scenario programmatically.
    #' Delegates to the standalone [addScenario()] function.
    #' @param scenarioName Character. Name for the new scenario.
    #' @param modelFile Character. Name of the `.pkml` model file.
    #' @param ... Additional arguments passed to [addScenario()].
    addScenario = function(scenarioName, modelFile, ...) {
      addScenario(
        project = self,
        scenarioName = scenarioName,
        modelFile = modelFile,
        ...
      )
    },
    #' Initialize
    #'
    #' @param projectFilePath A string representing the path to the
    #'   project configuration file.
    initialize = function(projectFilePath = character()) {
      private$.modified <- FALSE
      if (!missing(projectFilePath)) {
        private$.read_json(projectFilePath)
      } else {
        private$.projectDirPath <- NULL
      }
    },
    #' Print
    #' @description print prints a summary of the Project.
    #' @param className Whether to print the name of the class at the beginning.
    #'   default to TRUE.
    print = function(className = TRUE) {
      if (className) {
        ospsuite.utils::ospPrintClass(self)
      }
      ospsuite.utils::ospPrintItems(list(
        "Schema version" = self$schemaVersion %||% "unknown",
        "Working Directory" = getwd(),
        "Configuration file" = self$projectFilePath,
        "Model folder" = self$modelFolder,
        "Data folder" = self$dataFolder,
        "Data file" = self$dataFile,
        "Data importer configuration" = self$dataImporterConfigurationFile,
        "Output folder" = self$outputFolder
      ))

      # Count plots breakdown
      plotCounts <- vapply(
        c("dataCombined", "plotConfiguration", "plotGrids", "exportConfiguration"),
        function(name) {
          df <- self$plots[[name]]
          if (is.null(df)) 0L else nrow(df)
        },
        integer(1)
      )
      # Only include non-zero plot sub-sections
      nonZero <- plotCounts[plotCounts > 0]
      if (length(nonZero) > 0) {
        plotsLabel <- paste(
          paste(nonZero, names(nonZero)),
          collapse = ", "
        )
      } else {
        plotsLabel <- "0"
      }

      ospsuite.utils::ospPrintItems(
        list(
          "Scenarios" = length(self$scenarios),
          "Individuals" = length(self$individuals),
          "Populations" = length(self$populations),
          "Model Parameters" = paste(length(self$modelParameters), "groups"),
          "Applications" = length(self$applications),
          "Output Paths" = length(self$outputPaths),
          "Plots" = plotsLabel
        ),
        title = "Contents"
      )
      invisible(self)
    },
    #' @field schemaVersion Project structure schema version (e.g. "2.0").
    #'   Shared between JSON and Excel representations.
    schemaVersion = NULL,
    #' @field esqlabsRVersion The esqlabsR version that created the JSON file.
    esqlabsRVersion = NULL,
    #' @field scenarios Named list of `Scenario` objects, keyed by scenario
    #'   name. Populated by JSON loading.
    scenarios = NULL,
    #' @field modelParameters Named list of parameter structures, keyed by
    #'   sheet name. Each is a list with `paths`, `values`, `units` vectors.
    modelParameters = NULL,
    #' @field individuals Named list of plain lists, keyed by individualId.
    #'   Each entry contains `species`, `population`, `gender`, `weight`,
    #'   `height`, `age`, and optionally `proteinOntogenies`.
    individuals = NULL,
    #' @field individualParameterSets Named list of parameter structures,
    #'   keyed by set name. Each is a list with `paths`, `values`, `units`.
    individualParameterSets = NULL,
    #' @field populations Named list of plain lists, keyed by populationId.
    #'   Each entry contains population creation arguments such as `species`,
    #'   `population`, `numberOfIndividuals`, etc.
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
    individualParameterSetMapping = NULL,
    #' @field observedData List of observed data source declarations parsed from
    #'   JSON. Each entry is a list with `type` ("excel" or "pkml") and
    #'   source-specific fields. See the JSON schema documentation for details.
    observedData = NULL
  )
)

#' @rdname Project
#' @usage NULL
#' @export
ProjectConfiguration <- R6::R6Class(
  "ProjectConfiguration",
  inherit = Project,
  public = list(
    #' @description Deprecated. Use `Project$new()` instead.
    #' @param projectConfigurationFilePath Path to the project file.
    #' @param ... Additional arguments passed to `Project$new()`.
    initialize = function(projectConfigurationFilePath = character(), ...) {
      lifecycle::deprecate_soft(
        what = "ProjectConfiguration()",
        with = "Project()",
        when = "7.0.0"
      )
      super$initialize(projectFilePath = projectConfigurationFilePath, ...)
    }
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
