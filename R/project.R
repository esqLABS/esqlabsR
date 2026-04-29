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
    #' @field projectFilePath Read-only. Absolute path to the JSON
    #'   configuration file the project was loaded from. All other relative
    #'   paths in the project are resolved against the file's directory
    #'   (see `projectDirPath`). `NULL` for an empty in-memory project; in
    #'   that case all path fields must be absolute.
    projectFilePath = function(value) {
      if (missing(value)) {
        private$.projectFilePath
      } else {
        stop("projectFilePath is readonly")
      }
    },
    #' @field projectDirPath Read-only. Directory containing the JSON
    #'   configuration file (i.e. `dirname(projectFilePath)`). Used as the
    #'   base for resolving relative paths. `NULL` if the project was not
    #'   loaded from a file.
    projectDirPath = function(value) {
      if (missing(value)) {
        private$.projectDirPath
      } else {
        stop("projectDirPath is readonly")
      }
    },
    #' @field modified Read-only logical. `TRUE` if any configuration property
    #' has been modified since the project was loaded or saved. Cleared
    #' internally by [saveProject()].
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
    },
    #' @field asList Returns the current project as a list matching the JSON
    #'   schema. Reflects any in-memory modifications. Read-only.
    asList = function(value) {
      if (!missing(value)) {
        stop("asList is readonly")
      }
      .projectToJson(self)
    }
  ),
  private = list(
    .filePathsData = NULL,
    .projectFilePath = NULL,
    .projectDirPath = NULL,
    .modified = FALSE,
    .rawData = NULL,
    .warned_paths = character(),
    .programmaticDataSets = list(),
    .observedDataNamesCache = NULL,
    .clean_path = function(
      path,
      parent = NULL,
      must_work = TRUE,
      replace_env_vars = TRUE
    ) {
      # In case project configuration is initialized empty
      if (
        is.null(path) ||
          length(path) == 0L ||
          (length(path) == 1L && is.na(path))
      ) {
        return(NULL)
      }

      if (replace_env_vars) {
        path <- private$.replace_env_var(path)
      }

      if (
        is.null(parent) ||
          (length(parent) == 1L && is.na(parent)) ||
          fs::is_absolute_path(path)
      ) {
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
        # Skip the system PATH variable (named "PATH" or "Path" depending on OS)
        # to avoid clobbering it. Other env vars whose name happens to contain
        # "path" (e.g. MY_DATA_PATH) are still expanded.
        if (toupper(path_split[i]) == "PATH") {
          next
        }
        if (Sys.getenv(path_split[i]) != "") {
          private$.replaced_env_vars[[path_split[i]]] <-
            Sys.getenv(path_split[i])
          path_split[i] <- Sys.getenv(path_split[i])
        }
      }
      # reconstruct path with updated environment variables
      path <- paste(path_split, collapse = "/")
      return(path)
    },
    .replaced_env_vars = list(),

    .to_relative_path = function(path) {
      if (is.null(path) || is.null(self$projectDirPath)) {
        return(path)
      }
      fs::path_rel(path, self$projectDirPath)
    },

    .read_json = function(jsonPath) {
      jsonPath <- fs::path_abs(jsonPath)
      if (!fs::file_exists(jsonPath)) {
        stop(messages$fileNotFound(jsonPath))
      }

      jsonData <- jsonlite::fromJSON(jsonPath, simplifyVector = FALSE)
      private$.rawData <- jsonData

      # Validate schema version. The JSON `schemaVersion` is the loading
      # contract; `esqlabsRVersion` is informational metadata (preserved on
      # round-trip) and is not used to gate or warn on load.
      if (is.null(jsonData$schemaVersion) || jsonData$schemaVersion != "2.0") {
        stop(paste0(
          "Unsupported or missing schemaVersion. Expected '2.0', got '",
          jsonData$schemaVersion %||% "NULL",
          "'."
        ))
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

      # Parse applications
      self$applications <- private$.parseApplications(
        jsonData$applications
      )

      # Parse individuals
      self$individuals <- private$.parseIndividuals(jsonData$individuals)

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
      if (is.null(groups)) {
        return(list())
      }
      result <- list()
      for (name in names(groups)) {
        entries <- groups[[name]]
        paths <- character(0)
        values <- numeric(0)
        units <- character(0)
        for (entry in entries) {
          paths <- c(
            paths,
            paste(
              entry$containerPath,
              entry$parameterName,
              sep = "|"
            )
          )
          values <- c(values, as.numeric(entry$value))
          units <- c(units, entry$units %||% "")
        }
        result[[name]] <- list(paths = paths, values = values, units = units)
      }
      result
    },

    .parseIndividuals = function(individualsData) {
      if (is.null(individualsData)) {
        return(list())
      }
      result <- list()
      for (entry in individualsData) {
        pset <- NULL
        if (!is.null(entry$parameters)) {
          for (p in entry$parameters) {
            pset <- .addParameterEntry(
              pset,
              p$containerPath,
              p$parameterName,
              p$value,
              p$units %||% ""
            )
          }
        }
        indiv <- list(
          species = entry$species,
          population = entry$population,
          gender = entry$gender,
          weight = as.double(entry$weight),
          height = as.double(entry$height),
          age = as.double(entry$age),
          proteinOntogenies = entry$proteinOntogenies,
          parameters = pset
        )
        class(indiv) <- c("Individual", "list")
        result[[entry$individualId]] <- indiv
      }
      result
    },

    .parseApplications = function(appsData) {
      if (is.null(appsData)) {
        return(list())
      }
      result <- list()
      for (id in names(appsData)) {
        entry <- appsData[[id]]
        pset <- NULL
        if (!is.null(entry$parameters)) {
          for (p in entry$parameters) {
            pset <- .addParameterEntry(
              pset,
              p$containerPath,
              p$parameterName,
              p$value,
              p$units %||% ""
            )
          }
        }
        app <- list(parameters = pset)
        class(app) <- c("Application", "list")
        result[[id]] <- app
      }
      result
    },

    .parsePopulations = function(populationsData) {
      if (is.null(populationsData)) {
        return(list())
      }
      result <- list()
      for (entry in populationsData) {
        popData <- list()
        for (field in names(entry)) {
          if (field == "populationId") {
            next
          }
          val <- entry[[field]]
          if (!is.null(val)) {
            numericFields <- c(
              "numberOfIndividuals",
              "proportionOfFemales",
              "weightMin",
              "weightMax",
              "heightMin",
              "heightMax",
              "ageMin",
              "ageMax",
              "BMIMin",
              "BMIMax"
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
      if (is.null(scenariosData)) {
        return(list())
      }
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
        if (!is.null(entry$modelParameters)) {
          sc$modelParameters <- unlist(entry$modelParameters)
        }
        if (!is.null(entry$simulationTime)) {
          sc$simulationTime <- .parseSimulationTimeIntervals(
            entry$simulationTime
          )
          sc$simulationTimeUnit <- entry$simulationTimeUnit
        }
        if (!is.null(entry$steadyState) && isTRUE(entry$steadyState)) {
          sc$simulateSteadyState <- TRUE
        }
        if (!is.null(entry$steadyStateTime)) {
          if (is.null(entry$steadyStateTimeUnit)) {
            stop(
              "Scenario '",
              entry$name,
              "' has 'steadyStateTime' set but ",
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
          unknown <- setdiff(pathIds, names(self$outputPaths))
          if (length(unknown) > 0) {
            stop(
              "Scenario '",
              entry$name,
              "' references unknown outputPathIds: ",
              paste(unknown, collapse = ", ")
            )
          }
          sc$outputPaths <- unname(self$outputPaths[pathIds])
        }
        result[[entry$name]] <- sc
      }
      result
    },

    .parseObservedData = function(observedDataConfig) {
      if (is.null(observedDataConfig)) {
        return(list())
      }
      for (entry in observedDataConfig) {
        if (
          is.null(entry$type) ||
            !entry$type %in% c("excel", "pkml", "script", "programmatic")
        ) {
          stop(
            "Each observedData entry must have a 'type' of 'excel', 'pkml', 'script', or 'programmatic'."
          )
        }
        if (entry$type == "excel") {
          if (is.null(entry$file)) {
            stop(messages$excelEntryMissingFile())
          }
          if (is.null(entry$importerConfiguration)) {
            stop(messages$excelEntryMissingImporter())
          }
          if (is.null(entry$sheets)) stop(messages$excelEntryMissingSheets())
        }
        if (entry$type %in% c("pkml", "script")) {
          if (is.null(entry$file)) stop(messages$entryMissingFile(entry$type))
        }
      }
      observedDataConfig
    },

    .parsePlots = function(plotsData) {
      if (is.null(plotsData)) {
        return(NULL)
      }
      list(
        dataCombined = .parseNestedDataCombined(
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
      as.data.frame(dplyr::bind_rows(rows))
    }
  ),
  public = list(
    #' @description Internal method to clear the `modified` flag after saving.
    #' Not intended for end-user use.
    #' @keywords internal
    .markSaved = function() {
      private$.modified <- FALSE
    },
    #' @description Internal method to set the `modified` flag after a
    #' programmatic mutation. Not intended for end-user use.
    #' @keywords internal
    .markModified = function() {
      private$.modified <- TRUE
    },
    #' @description Internal method to retrieve all programmatic DataSets.
    #' Not intended for end-user use.
    #' @keywords internal
    .getProgrammaticDataSets = function() {
      private$.programmaticDataSets
    },
    #' @description Internal method to register a programmatic DataSet.
    #' Not intended for end-user use.
    #' @param name Character. Key under which the DataSet is stored.
    #' @param dataSet A `DataSet` object.
    #' @keywords internal
    .addProgrammaticDataSet = function(name, dataSet) {
      private$.programmaticDataSets[[name]] <- dataSet
    },
    #' @description Internal method to drop a programmatic DataSet by name.
    #' @param name Character.
    #' @keywords internal
    .removeProgrammaticDataSet = function(name) {
      private$.programmaticDataSets[[name]] <- NULL
    },
    #' @description Internal method to retrieve the raw filePaths metadata
    #' (a named list of `list(value, description)` entries). Not intended for
    #' end-user use; consumed by the Excel import/export bridge.
    #' @keywords internal
    .getFilePathsData = function() {
      private$.filePathsData
    },
    #' @description Internal method to cache observed data names.
    #' @param names Character vector of DataSet names.
    #' @keywords internal
    .cacheObservedDataNames = function(names) {
      private$.observedDataNamesCache <- names
    },
    #' @description Internal method to invalidate the cached observed data
    #' names. Use after mutating `observedData`.
    #' @keywords internal
    .invalidateObservedDataNamesCache = function() {
      private$.observedDataNamesCache <- NULL
    },
    #' @description Internal method to append to the cached observed data names.
    #' @param name Character scalar to append.
    #' @keywords internal
    .appendObservedDataNameCache = function(name) {
      private$.observedDataNamesCache <- c(
        private$.observedDataNamesCache,
        name
      )
    },
    #' @description Internal method to get cached observed data names.
    #' @keywords internal
    .getObservedDataNamesCache = function() {
      private$.observedDataNamesCache
    },
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
    #' @description Add an individual programmatically.
    #' Delegates to the standalone [addIndividual()] function.
    #' @param individualId Character. Unique ID.
    #' @param species Character. Species name.
    #' @param ... Additional fields passed to [addIndividual()].
    addIndividual = function(individualId, species, ...) {
      addIndividual(
        project = self,
        individualId = individualId,
        species = species,
        ...
      )
    },
    #' @description Remove an individual programmatically.
    #' Delegates to the standalone [removeIndividual()] function.
    #' @param individualId Character.
    removeIndividual = function(individualId) {
      removeIndividual(project = self, individualId = individualId)
    },
    #' @description Add a parameter to a named individual.
    #' @param individualId Character.
    #' @param containerPath Character.
    #' @param parameterName Character.
    #' @param value Numeric.
    #' @param units Character.
    addIndividualParameter = function(
      individualId,
      containerPath,
      parameterName,
      value,
      units
    ) {
      addIndividualParameter(
        project = self,
        individualId = individualId,
        containerPath = containerPath,
        parameterName = parameterName,
        value = value,
        units = units
      )
    },
    #' @description Remove a parameter from a named individual.
    #' @param individualId Character.
    #' @param containerPath Character.
    #' @param parameterName Character.
    removeIndividualParameter = function(
      individualId,
      containerPath,
      parameterName
    ) {
      removeIndividualParameter(
        project = self,
        individualId = individualId,
        containerPath = containerPath,
        parameterName = parameterName
      )
    },
    #' @description Add a population programmatically.
    #' @param populationId Character.
    #' @param species Character.
    #' @param numberOfIndividuals Integer.
    #' @param ... Passed to [addPopulation()].
    addPopulation = function(populationId, species, numberOfIndividuals, ...) {
      addPopulation(
        project = self,
        populationId = populationId,
        species = species,
        numberOfIndividuals = numberOfIndividuals,
        ...
      )
    },
    #' @description Remove a population programmatically.
    #' @param populationId Character.
    removePopulation = function(populationId) {
      removePopulation(project = self, populationId = populationId)
    },
    #' @description Add a parameter to a named model-parameter set.
    #' @param id Character.
    #' @param containerPath Character.
    #' @param parameterName Character.
    #' @param value Numeric.
    #' @param units Character.
    addModelParameter = function(
      id,
      containerPath,
      parameterName,
      value,
      units
    ) {
      addModelParameter(self, id, containerPath, parameterName, value, units)
    },
    #' @description Remove a parameter from a named model-parameter set.
    #' @param id Character.
    #' @param containerPath Character.
    #' @param parameterName Character.
    removeModelParameter = function(id, containerPath, parameterName) {
      removeModelParameter(self, id, containerPath, parameterName)
    },
    #' @description Add an application protocol programmatically.
    #' @param applicationId Character.
    addApplication = function(applicationId) {
      addApplication(project = self, applicationId = applicationId)
    },
    #' @description Remove an application protocol programmatically.
    #' @param applicationId Character.
    removeApplication = function(applicationId) {
      removeApplication(project = self, applicationId = applicationId)
    },
    #' @description Add a parameter to a named application.
    #' @param applicationId Character.
    #' @param containerPath Character.
    #' @param parameterName Character.
    #' @param value Numeric.
    #' @param units Character.
    addApplicationParameter = function(
      applicationId,
      containerPath,
      parameterName,
      value,
      units
    ) {
      addApplicationParameter(
        project = self,
        applicationId = applicationId,
        containerPath = containerPath,
        parameterName = parameterName,
        value = value,
        units = units
      )
    },
    #' @description Remove a parameter from a named application.
    #' @param applicationId Character.
    #' @param containerPath Character.
    #' @param parameterName Character.
    removeApplicationParameter = function(
      applicationId,
      containerPath,
      parameterName
    ) {
      removeApplicationParameter(
        project = self,
        applicationId = applicationId,
        containerPath = containerPath,
        parameterName = parameterName
      )
    },
    #' @description Add one or more output paths programmatically.
    #' @param id Character vector.
    #' @param path Character vector.
    addOutputPath = function(id, path) {
      addOutputPath(self, id, path)
    },
    #' @description Remove an output path programmatically.
    #' @param id Character.
    removeOutputPath = function(id) {
      removeOutputPath(self, id)
    },
    #' @description Remove a scenario programmatically.
    #' @param name Character.
    removeScenario = function(name) {
      removeScenario(self, name)
    },
    #' @description Add observed data programmatically
    #'
    #' Add an observedData entry to the project. Accepts either:
    #' - A `DataSet` object directly (creates a "programmatic" entry, uses `dataSet$name`)
    #' - A configuration list with `type` and relevant fields (excel, pkml, script)
    #'
    #' @param entry Either a `DataSet` object or a list with observedData config.
    #'   For DataSet: stored internally using `dataSet$name` as the key.
    #'   For config list: must include `type` ("excel", "pkml", "script") and
    #'   relevant fields (e.g., `file`, `importerConfiguration`, `sheets`).
    #' @return Invisibly returns self for chaining
    addObservedData = function(entry) {
      addObservedData(project = self, entry = entry)
    },
    #' @description Remove observed data programmatically.
    #' Delegates to the standalone [removeObservedData()] function.
    #' @param name DataSet name or config entry file basename.
    removeObservedData = function(name) {
      removeObservedData(project = self, name = name)
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
      rel <- function(p) private$.to_relative_path(p)
      ospsuite.utils::ospPrintItems(list(
        "Schema version" = self$schemaVersion %||% "unknown",
        "Working Directory" = getwd(),
        "Configuration file" = rel(self$projectFilePath),
        "Model folder" = rel(self$modelFolder),
        "Data folder" = rel(self$dataFolder),
        "Output folder" = rel(self$outputFolder)
      ))

      # Count plots breakdown
      plotCounts <- vapply(
        c(
          "dataCombined",
          "plotConfiguration",
          "plotGrids",
          "exportConfiguration"
        ),
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
    #' Check synchronization status
    #' @description Compares in-memory project state with source files.
    #' @param silent Logical. If `TRUE`, suppresses informational messages.
    #'   Defaults to `FALSE`.
    #' @return A list with components:
    #'   \item{in_sync}{Logical. `TRUE` if all sources are synchronized.}
    #'   \item{unsaved_changes}{Logical. `TRUE` if in-memory differs from JSON.}
    #'   \item{json_modified}{Logical. `TRUE` if JSON file differs from loaded.}
    #'   \item{excel_modified}{Logical. `TRUE` if Excel files differ from JSON.}
    #'   \item{details}{List with detailed comparison results.}
    sync = function(silent = FALSE) {
      .projectSync(self, silent = silent)
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
    #' @field observedData List of observed data source declarations parsed from
    #'   JSON. Each entry is a list with `type` ("excel" or "pkml") and
    #'   source-specific fields. See the JSON schema documentation for details.
    observedData = NULL
  )
)

#' @rdname Project
#' @usage NULL
#' @export
ProjectConfiguration <- function(
  projectConfigurationFilePath = character(),
  ...
) {
  lifecycle::deprecate_soft(
    what = "ProjectConfiguration()",
    with = "Project$new()",
    when = "7.0.0"
  )
  Project$new(projectFilePath = projectConfigurationFilePath, ...)
}

# Internal Helper Functions ----

#' Parse nested dataCombined JSON to flat data.frame
#' @param nestedData List of dataCombined objects with simulated/observed arrays
#' @returns data.frame with DataCombinedName, dataType, and all entry fields
#' @keywords internal
#' @noRd
.parseNestedDataCombined <- function(nestedData) {
  if (is.null(nestedData) || length(nestedData) == 0) {
    return(data.frame())
  }

  transformCols <- c(
    "xOffsets",
    "xOffsetsUnits",
    "yOffsets",
    "yOffsetsUnits",
    "xScaleFactors",
    "yScaleFactors"
  )

  rows <- list()
  for (dataCombined in nestedData) {
    dataCombinedName <- dataCombined$name

    # Process simulated entries
    if (!is.null(dataCombined$simulated) && length(dataCombined$simulated) > 0) {
      for (entry in dataCombined$simulated) {
        row <- list(
          DataCombinedName = dataCombinedName,
          dataType = "simulated",
          label = entry$label,
          scenario = entry$scenario,
          path = entry$path,
          dataSet = NA,
          group = entry$group %||% NA
        )
        for (col in transformCols) {
          row[[col]] <- entry[[col]] %||% NA
        }
        rows[[length(rows) + 1]] <- as.data.frame(row, stringsAsFactors = FALSE)
      }
    }

    # Process observed entries
    if (!is.null(dataCombined$observed) && length(dataCombined$observed) > 0) {
      for (entry in dataCombined$observed) {
        row <- list(
          DataCombinedName = dataCombinedName,
          dataType = "observed",
          label = entry$label,
          scenario = NA,
          path = NA,
          dataSet = entry$dataSet,
          group = entry$group %||% NA
        )
        for (col in transformCols) {
          row[[col]] <- entry[[col]] %||% NA
        }
        rows[[length(rows) + 1]] <- as.data.frame(row, stringsAsFactors = FALSE)
      }
    }
  }

  if (length(rows) == 0) {
    return(data.frame())
  }

  do.call(rbind, rows)
}

# Public Functions ----

#' Load a project from a JSON configuration file
#'
#' @description Load a `Project` from a JSON file. This is the
#'   primary entry point for working with esqlabsR projects.
#'
#' @param path Path to the `Project.json` file. Defaults to
#'   `Project.json` in the working directory.
#'
#' @returns Object of type `Project`
#' @export
loadProject <- function(path = "Project.json") {
  Project$new(projectFilePath = path)
}

#' Save a project to a JSON file
#'
#' @description Serializes the in-memory `Project` object back to JSON with
#'   round-trip fidelity. This allows persisting changes made programmatically
#'   (e.g., via [addScenario()]).
#'
#' @param project A `Project` object.
#' @param path Path where the JSON file should be written. If `NULL` (default),
#'   uses `project$jsonPath` (the path the project was loaded from).
#'
#' @returns Invisibly returns the path where the file was written.
#' @export
#'
#' @examples
#' \dontrun{
#' project <- loadProject("Project.json")
#' addScenario(project, "NewScenario", "Model.pkml", individualId = "Indiv1")
#' saveProject(project)
#' }
saveProject <- function(project, path = NULL) {
  validateIsOfType(project, "Project")

  if (is.null(path)) {
    path <- project$jsonPath
    if (is.null(path)) {
      stop(
        "No path specified and project has no jsonPath. Provide a path argument."
      )
    }
  }

  jsonData <- .projectToJson(project)
  jsonlite::write_json(
    jsonData,
    path,
    auto_unbox = TRUE,
    null = "null",
    pretty = TRUE
  )

  project$.markSaved()
  invisible(path)
}

#' @rdname loadProject
#' @export
createProjectConfiguration <- function(path = "Project.json") {
  lifecycle::deprecate_soft(
    what = "createProjectConfiguration()",
    with = "loadProject()",
    when = "6.0.0"
  )
  loadProject(path)
}

#' @rdname loadProject
#' @export
createDefaultProjectConfiguration <- function(path = "Project.json") {
  lifecycle::deprecate_soft(
    what = "createDefaultProjectConfiguration()",
    with = "loadProject()",
    when = "5.3.0"
  )
  loadProject(path)
}

#' Check if a directory contains an esqlabsR project
#'
#' @description Checks if a directory already contains an esqlabsR project by
#' looking for the presence of Project.xlsx file or Configurations
#' folder.
#'
#' @param destination A string defining the path to check for an existing
#'   project. Defaults to current working directory.
#'
#' @returns TRUE if an esqlabsR project exists in the directory, FALSE
#'   otherwise.
#' @export
#' @examples
#' \dontrun{
#' # Check if current directory has a project
#' hasProject <- isProjectInitialized()
#'
#' # Check if specific directory has a project
#' hasProject <- isProjectInitialized("path/to/project")
#' }
isProjectInitialized <- function(destination = ".") {
  destination <- fs::path_abs(destination)

  if (!fs::dir_exists(destination)) {
    return(FALSE)
  }

  # Check for Project.xlsx file
  hasConfigFile <- length(fs::dir_ls(destination, glob = "*Project*.xlsx")) > 0

  # Check for Configurations folder
  hasConfigFolder <- fs::dir_exists(file.path(destination, "Configurations"))

  return(hasConfigFile || hasConfigFolder)
}

#' Initialize esqlabsR Project Folders and required Files
#'
#' @description
#'
#' Creates the default project folder structure with Excel file templates in the
#' working directory.
#'
#' @param destination A string defining the path where to initialize the
#'   project. default to current working directory.
#' @param type Type of project to create: `"minimal"` (default) creates an empty
#'   project with just the directory structure, `"example"` creates a project
#'   with example data, models, and configurations.
#' @param createExcel If `TRUE` (default), generates Excel configuration files
#'   from the JSON. Set to `FALSE` for a JSON-only workflow.
#' @param overwrite If TRUE, overwrites existing project without asking for
#'   permission. If FALSE and a project already exists, asks user for permission
#'   to overwrite.
#' @export
initProject <- function(
  destination = ".",
  type = c("minimal", "example"),
  createExcel = TRUE,
  overwrite = FALSE
) {
  destination <- fs::path_abs(destination)
  type <- match.arg(type)

  if (!fs::dir_exists(destination)) {
    stop(
      messages$pathNotFound(destination)
    )
  }

  source_folder <- switch(
    type,
    "minimal" = .projectDirectory("Blank"),
    "example" = .projectDirectory("Example")
  )

  # Check if project already exists
  if (isProjectInitialized(destination)) {
    if (overwrite) {
      # Overwrite without asking
      message(messages$overwriteDestination(destination))
    } else {
      # Ask for permission to overwrite
      qs <- sample(c("Absolutely not", "Yes", "No way"))

      out <- utils::menu(
        title = "The destination folder seems to already contain an esqlabsR project. Do you want to overwrite it?",
        choices = qs
      )

      if (out == 0L || qs[[out]] != "Yes") {
        stop(messages$abortedByUser())
      }

      message(messages$overwriteDestination(destination))
    }
  }

  # Copy Blank template files (just the JSON)
  res <- file.copy(
    list.files(source_folder, full.names = TRUE),
    destination,
    recursive = TRUE,
    overwrite = TRUE
  )

  # Create empty directory structure
  dirs_to_create <- c(
    "Models/Simulations",
    "Data",
    "Populations",
    "Results/Figures",
    "Results/SimulationResults"
  )
  for (d in dirs_to_create) {
    dir.create(
      file.path(destination, d),
      recursive = TRUE,
      showWarnings = FALSE
    )
  }

  if (createExcel) {
    jsonPath <- file.path(destination, "Project.json")
    project <- loadProject(jsonPath)
    exportProjectToExcel(project, outputDir = destination, silent = TRUE)
  }

  invisible(destination)
}

#' Get the path to the example Project.json
#'
#' @returns A string representing the path to the example
#'   `Project.json` file shipped with the package.
#' @export
#' @examples
#' exampleProjectPath()
exampleProjectPath <- function() {
  file.path(.projectDirectory("Example"), "Project.json")
}

#' @rdname exampleProjectPath
#' @export
exampleProjectConfigurationPath <- function() {
  lifecycle::deprecate_soft(
    what = "exampleProjectConfigurationPath()",
    with = "exampleProjectPath()",
    when = "7.0.0"
  )
  exampleProjectPath()
}

# Internal Helper Functions ----

#' Convert a Project object to a JSON-serializable list
#' @param project A `Project` object.
#' @returns A list matching the Project.json schema.
#' @keywords internal
#' @noRd
.projectToJson <- function(project) {
  list(
    schemaVersion = project$schemaVersion %||% "2.0",
    # Preserve the version stamp from the loaded file; only fall back to the
    # current package version when the project was constructed in-memory and
    # has no recorded version. This keeps round-trip save/load stable.
    esqlabsRVersion = project$esqlabsRVersion %||%
      as.character(utils::packageVersion("esqlabsR")),
    filePaths = .filePathsToJson(project),
    observedData = project$observedData %||% list(),
    outputPaths = as.list(project$outputPaths) %||% list(),
    scenarios = .scenariosToJson(project$scenarios, project$outputPaths),
    modelParameters = .parameterGroupsToJson(project$modelParameters),
    individuals = .individualsToJson(project$individuals),
    populations = .populationsToJson(project$populations),
    applications = .applicationsToJson(project$applications),
    plots = .plotsToJson(project$plots)
  )
}

#' @keywords internal
#' @noRd
.filePathsToJson <- function(project) {
  list(
    modelFolder = .relativePathOrNull(project, "modelFolder"),
    configurationsFolder = .relativePathOrNull(project, "configurationsFolder"),
    modelParamsFile = .relativeFilename(project, "modelParamsFile"),
    individualsFile = .relativeFilename(project, "individualsFile"),
    populationsFile = .relativeFilename(project, "populationsFile"),
    populationsFolder = .relativeFilename(project, "populationsFolder"),
    scenariosFile = .relativeFilename(project, "scenariosFile"),
    applicationsFile = .relativeFilename(project, "applicationsFile"),
    plotsFile = .relativeFilename(project, "plotsFile"),
    dataFolder = .relativePathOrNull(project, "dataFolder"),
    outputFolder = .relativePathOrNull(project, "outputFolder")
  )
}

#' @keywords internal
#' @noRd
.relativePathOrNull <- function(project, fieldName) {
  absPath <- suppressWarnings(project[[fieldName]])
  if (is.null(absPath)) {
    return(NULL)
  }
  if (is.null(project$projectDirPath)) {
    return(absPath)
  }
  fs::path_rel(absPath, project$projectDirPath)
}

#' @keywords internal
#' @noRd
.relativeFilename <- function(project, fieldName) {
  absPath <- suppressWarnings(project[[fieldName]])
  if (is.null(absPath)) {
    return(NULL)
  }
  basename(absPath)
}

#' @keywords internal
#' @noRd
.scenariosToJson <- function(scenarios, outputPaths) {
  if (is.null(scenarios) || length(scenarios) == 0) {
    return(list())
  }

  lapply(scenarios, function(sc) {
    outputPathIds <- NULL
    if (
      !is.null(sc$outputPaths) &&
        length(sc$outputPaths) > 0 &&
        !is.null(outputPaths)
    ) {
      outputPathIds <- names(outputPaths)[match(sc$outputPaths, outputPaths)]
      outputPathIds <- outputPathIds[!is.na(outputPathIds)]
      if (length(outputPathIds) == 0) outputPathIds <- NULL
    }

    simTimeStr <- NULL
    if (!is.null(sc$simulationTime)) {
      intervals <- vapply(
        sc$simulationTime,
        function(int) {
          paste(int, collapse = ", ")
        },
        character(1)
      )
      simTimeStr <- paste(intervals, collapse = "; ")
    }

    list(
      name = sc$scenarioName,
      individualId = sc$individualId,
      populationId = if (sc$simulationType == "Population") {
        sc$populationId
      } else {
        NULL
      },
      readPopulationFromCSV = sc$readPopulationFromCSV,
      modelParameters = as.list(sc$modelParameters),
      applicationProtocol = if (
        is.null(sc$applicationProtocol) || is.na(sc$applicationProtocol)
      ) {
        NULL
      } else {
        sc$applicationProtocol
      },
      simulationTime = simTimeStr,
      simulationTimeUnit = sc$simulationTimeUnit,
      steadyState = sc$simulateSteadyState,
      steadyStateTime = if (
        sc$simulateSteadyState && !is.null(sc$steadyStateTime)
      ) {
        ospsuite::toUnit(
          quantityOrDimension = ospsuite::ospDimensions$Time,
          values = sc$steadyStateTime,
          targetUnit = sc$steadyStateTimeUnit %||% "min"
        )
      } else {
        NULL
      },
      steadyStateTimeUnit = if (sc$simulateSteadyState) {
        sc$steadyStateTimeUnit
      } else {
        NULL
      },
      overwriteFormulasInSS = sc$overwriteFormulasInSS,
      modelFile = sc$modelFile,
      outputPathIds = if (!is.null(outputPathIds)) {
        as.list(outputPathIds)
      } else {
        NULL
      }
    )
  })
}

#' @keywords internal
#' @noRd
.parameterGroupsToJson <- function(groups) {
  if (is.null(groups) || length(groups) == 0) {
    return(list())
  }

  result <- list()
  for (name in names(groups)) {
    group <- groups[[name]]
    entries <- list()
    for (i in seq_along(group$paths)) {
      split <- .splitParameterPathIntoContainerAndName(group$paths[i])
      entries[[i]] <- list(
        containerPath = split$containerPath,
        parameterName = split$parameterName,
        value = group$values[i],
        units = if (group$units[i] == "") NULL else group$units[i]
      )
    }
    result[[name]] <- entries
  }
  result
}

#' @keywords internal
#' @noRd
.applicationsToJson <- function(applications) {
  if (is.null(applications) || length(applications) == 0) {
    return(structure(list(), names = character(0)))
  }
  result <- list()
  for (id in names(applications)) {
    pset <- applications[[id]]$parameters
    params <- list()
    if (!is.null(pset) && length(pset$paths) > 0) {
      params <- vector("list", length(pset$paths))
      for (j in seq_along(pset$paths)) {
        split <- .splitParameterPathIntoContainerAndName(pset$paths[[j]])
        params[[j]] <- list(
          containerPath = split$containerPath,
          parameterName = split$parameterName,
          value = pset$values[[j]],
          units = pset$units[[j]]
        )
      }
    }
    result[[id]] <- list(parameters = params)
  }
  result
}

#' @keywords internal
#' @noRd
.individualsToJson <- function(individuals, parameterSetMapping = NULL) {
  if (is.null(individuals) || length(individuals) == 0) {
    return(list())
  }
  result <- vector("list", length(individuals))
  for (i in seq_along(individuals)) {
    id <- names(individuals)[[i]]
    indiv <- individuals[[i]]
    entry <- list(individualId = id)
    for (field in c("species", "population", "gender", "proteinOntogenies")) {
      if (!is.null(indiv[[field]])) entry[[field]] <- indiv[[field]]
    }
    for (field in c("weight", "height", "age")) {
      if (!is.null(indiv[[field]]) && !is.na(indiv[[field]])) {
        entry[[field]] <- as.double(indiv[[field]])
      }
    }
    pset <- indiv$parameters
    if (!is.null(pset) && length(pset$paths) > 0) {
      params <- vector("list", length(pset$paths))
      for (j in seq_along(pset$paths)) {
        split <- .splitParameterPathIntoContainerAndName(pset$paths[[j]])
        params[[j]] <- list(
          containerPath = split$containerPath,
          parameterName = split$parameterName,
          value = pset$values[[j]],
          units = pset$units[[j]]
        )
      }
      entry$parameters <- params
    }
    result[[i]] <- entry
  }
  result
}

#' @keywords internal
#' @noRd
.populationsToJson <- function(populations) {
  if (is.null(populations) || length(populations) == 0) {
    return(list())
  }

  lapply(names(populations), function(id) {
    pop <- populations[[id]]
    c(list(populationId = id), pop)
  })
}

#' Convert flat dataCombined data.frame to nested JSON structure
#' @param df data.frame with DataCombinedName, dataType, and entry fields
#' @returns List of dataCombined objects with simulated/observed arrays
#' @keywords internal
#' @noRd
.dataCombinedToNestedJson <- function(df) {
  if (is.null(df) || nrow(df) == 0) {
    return(list())
  }

  transformCols <- c(
    "xOffsets",
    "xOffsetsUnits",
    "yOffsets",
    "yOffsetsUnits",
    "xScaleFactors",
    "yScaleFactors"
  )

  dataCombinedNames <- unique(df$DataCombinedName)

  lapply(dataCombinedNames, function(dataCombinedName) {
    dataCombinedRows <- df[df$DataCombinedName == dataCombinedName, , drop = FALSE]

    simRows <- dataCombinedRows[dataCombinedRows$dataType == "simulated", , drop = FALSE]
    obsRows <- dataCombinedRows[dataCombinedRows$dataType == "observed", , drop = FALSE]

    simulated <- lapply(seq_len(nrow(simRows)), function(i) {
      row <- simRows[i, ]
      entry <- list(
        label = row$label,
        scenario = row$scenario,
        path = row$path,
        group = if (is.na(row$group)) NULL else row$group
      )
      for (col in transformCols) {
        entry[[col]] <- if (is.na(row[[col]])) NULL else row[[col]]
      }
      entry
    })

    observed <- lapply(seq_len(nrow(obsRows)), function(i) {
      row <- obsRows[i, ]
      entry <- list(
        label = row$label,
        dataSet = row$dataSet,
        group = if (is.na(row$group)) NULL else row$group
      )
      for (col in transformCols) {
        entry[[col]] <- if (is.na(row[[col]])) NULL else row[[col]]
      }
      entry
    })

    list(
      name = dataCombinedName,
      simulated = simulated,
      observed = observed
    )
  })
}

#' @keywords internal
#' @noRd
.plotsToJson <- function(plots) {
  if (is.null(plots)) {
    return(list(
      dataCombined = list(),
      plotConfiguration = list(),
      plotGrids = list(),
      exportConfiguration = list()
    ))
  }

  list(
    dataCombined = .dataCombinedToNestedJson(plots$dataCombined),
    plotConfiguration = .dataFrameToListOfLists(plots$plotConfiguration),
    plotGrids = .dataFrameToListOfLists(plots$plotGrids),
    exportConfiguration = .dataFrameToListOfLists(plots$exportConfiguration)
  )
}

#' @keywords internal
#' @noRd
.dataFrameToListOfLists <- function(df) {
  if (is.null(df) || nrow(df) == 0) {
    return(list())
  }

  lapply(seq_len(nrow(df)), function(i) {
    row <- as.list(df[i, , drop = FALSE])
    row <- lapply(row, function(x) if (is.na(x)) NULL else x)
    row
  })
}

#' Check synchronization status of a Project
#' @param project A `Project` object.
#' @param silent Logical. If `TRUE`, suppresses messages.
#' @returns A list with sync status details.
#' @keywords internal
#' @noRd
.projectSync <- function(project, silent = FALSE) {
  result <- list(
    in_sync = TRUE,
    unsaved_changes = FALSE,
    json_modified = FALSE,
    excel_modified = FALSE,
    details = list()
  )

  jsonPath <- project$jsonPath
  if (is.null(jsonPath) || !file.exists(jsonPath)) {
    result$in_sync <- project$modified == FALSE
    result$unsaved_changes <- project$modified

    # Even without a JSON file, sibling Excel files may exist; flag those as
    # excel_modified relative to the absent JSON so callers don't get a false
    # in_sync = TRUE.
    if (!is.null(jsonPath)) {
      excelPath <- sub("\\.json$", ".xlsx", jsonPath)
      if (file.exists(excelPath)) {
        result$excel_modified <- TRUE
        result$in_sync <- FALSE
      }
    }

    if (!silent && result$unsaved_changes) {
      message("Project has unsaved changes (no JSON file to compare).")
    }
    return(invisible(result))
  }

  if (project$modified) {
    result$unsaved_changes <- TRUE
    result$in_sync <- FALSE
  } else {
    fileProject <- loadProject(jsonPath)
    currentJson <- jsonlite::toJSON(
      .projectToJson(project),
      auto_unbox = TRUE,
      null = "null"
    )
    fileJson <- jsonlite::toJSON(
      .projectToJson(fileProject),
      auto_unbox = TRUE,
      null = "null"
    )

    if (!identical(currentJson, fileJson)) {
      result$json_modified <- TRUE
      result$in_sync <- FALSE
    }
  }

  excelPath <- sub("\\.json$", ".xlsx", jsonPath)
  if (file.exists(excelPath)) {
    excelStatus <- tryCatch(
      projectStatus(
        projectConfigPath = excelPath,
        jsonPath = jsonPath,
        silent = TRUE
      ),
      error = function(e) list(in_sync = TRUE)
    )
    if (!isTRUE(excelStatus$in_sync)) {
      result$excel_modified <- TRUE
      result$in_sync <- FALSE
      result$details$excel <- excelStatus$details
    }
  }

  if (!silent) {
    if (result$in_sync) {
      message("Project is in sync with all source files.")
    } else {
      if (result$unsaved_changes) {
        cli::cli_alert_warning("In-memory changes not saved to JSON.")
      }
      if (result$json_modified) {
        cli::cli_alert_warning("JSON file has been modified externally.")
      }
      if (result$excel_modified) {
        cli::cli_alert_warning("Excel files differ from JSON.")
      }
    }
  }

  invisible(result)
}

#' Validate project file
#' @param projectConfigPath Path to Project.xlsx
#' @return validationResult object
#' @keywords internal
.validateProject <- function(projectConfigPath) {
  result <- validationResult$new()

  # Check file exists
  if (!file.exists(projectConfigPath)) {
    result$add_critical_error(
      "File",
      messages$validationFileNotFound(projectConfigPath)
    )
    return(result)
  }

  # Try to load project configuration directly (not using .safe_validate due to scoping issues)
  tryCatch(
    {
      withCallingHandlers(
        {
          config <- loadProject(path = projectConfigPath)

          # Check all referenced files exist
          files_to_check <- list(
            scenarios = config$scenariosFile,
            individuals = config$individualsFile,
            populations = config$populationsFile,
            models = config$modelParamsFile,
            applications = config$applicationsFile,
            plots = config$plotsFile
          )

          for (name in names(files_to_check)) {
            if (
              !is.na(files_to_check[[name]]) &&
                !file.exists(files_to_check[[name]])
            ) {
              result$add_warning(
                "File Reference",
                paste0(
                  "Referenced ",
                  name,
                  " file not found: ",
                  files_to_check[[name]]
                )
              )
            }
          }

          result$set_data(config)
        },
        warning = function(w) {
          category <- .categorize_message(conditionMessage(w))
          result$add_warning(category, conditionMessage(w))
          invokeRestart("muffleWarning")
        }
      )
    },
    error = function(e) {
      category <- .categorize_message(conditionMessage(e))
      result$add_critical_error(category, conditionMessage(e))
    }
  )

  return(result)
}
