# Project R6 class ----

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
    #' @description Add a DataCombined programmatically.
    #' Delegates to the standalone [addDataCombined()] function.
    #' @param name DataCombined name.
    #' @param simulated List of simulated entry lists.
    #' @param observed List of observed entry lists.
    addDataCombined = function(name, simulated = list(), observed = list()) {
      addDataCombined(
        project = self,
        name = name,
        simulated = simulated,
        observed = observed
      )
    },
    #' @description Remove a DataCombined programmatically.
    #' Delegates to the standalone [removeDataCombined()] function.
    #' @param name DataCombined name.
    removeDataCombined = function(name) {
      removeDataCombined(project = self, name = name)
    },
    #' @description Add a plot configuration programmatically.
    #' Delegates to the standalone [addPlot()] function.
    #' @param plotID Unique plot identifier.
    #' @param dataCombinedName DataCombined the plot draws from.
    #' @param plotType One of the supported plot types.
    #' @param ... Optional plot-configuration fields.
    addPlot = function(plotID, dataCombinedName, plotType, ...) {
      addPlot(
        project = self,
        plotID = plotID,
        dataCombinedName = dataCombinedName,
        plotType = plotType,
        ...
      )
    },
    #' @description Remove a plot configuration programmatically.
    #' Delegates to the standalone [removePlot()] function.
    #' @param plotID Plot identifier.
    removePlot = function(plotID) {
      removePlot(project = self, plotID = plotID)
    },
    #' @description Add a plot grid programmatically.
    #' Delegates to the standalone [addPlotGrid()] function.
    #' @param name Plot-grid name.
    #' @param plotIDs Character vector of plot IDs.
    #' @param ... Optional plot-grid fields.
    addPlotGrid = function(name, plotIDs, ...) {
      addPlotGrid(
        project = self,
        name = name,
        plotIDs = plotIDs,
        ...
      )
    },
    #' @description Remove a plot grid programmatically.
    #' Delegates to the standalone [removePlotGrid()] function.
    #' @param name Plot-grid name.
    removePlotGrid = function(name) {
      removePlotGrid(project = self, name = name)
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

# DataCombined JSON <-> flat data.frame ----

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
    if (
      !is.null(dataCombined$simulated) && length(dataCombined$simulated) > 0
    ) {
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

# Project sync and validation ----

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
