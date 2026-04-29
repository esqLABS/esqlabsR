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

# Public CRUD: applications ----

#' Add an application protocol to a Project
#'
#' @param project A `Project` object.
#' @param applicationId Character scalar, unique protocol id.
#'
#' @returns The `project` object, invisibly.
#' @export
#' @family application
addApplication <- function(project, applicationId) {
  validateIsOfType(project, "Project")
  if (
    !is.character(applicationId) ||
      length(applicationId) != 1 ||
      is.na(applicationId) ||
      nchar(applicationId) == 0
  ) {
    stop("applicationId must be a non-empty string")
  }
  if (applicationId %in% names(project$applications)) {
    stop(paste0("application '", applicationId, "' already exists"))
  }
  app <- list(parameters = NULL)
  class(app) <- c("Application", "list")
  project$applications[[applicationId]] <- app
  project$.markModified()
  invisible(project)
}

#' Remove an application protocol from a Project
#'
#' @param project A `Project` object.
#' @param applicationId Character scalar.
#' @returns The `project` object, invisibly.
#' @export
#' @family application
removeApplication <- function(project, applicationId) {
  validateIsOfType(project, "Project")
  if (!is.character(applicationId) || length(applicationId) != 1) {
    stop("applicationId must be a string scalar")
  }
  if (!(applicationId %in% names(project$applications))) {
    cli::cli_warn("application {.val {applicationId}} not found; no-op.")
    return(invisible(project))
  }
  .warnIfReferenced(project, "application", applicationId)
  project$applications[[applicationId]] <- NULL
  project$.markModified()
  invisible(project)
}

#' Add a parameter to a named application
#'
#' @param project A `Project` object.
#' @param applicationId Character scalar.
#' @param containerPath Character scalar.
#' @param parameterName Character scalar.
#' @param value Numeric scalar.
#' @param units Character scalar.
#' @returns The `project` object, invisibly.
#' @export
#' @family parameters
addApplicationParameter <- function(
  project,
  applicationId,
  containerPath,
  parameterName,
  value,
  units
) {
  validateIsOfType(project, "Project")
  if (!(applicationId %in% names(project$applications))) {
    stop(paste0("application '", applicationId, "' not found"))
  }
  project$applications[[applicationId]] <- addParameter(
    project$applications[[applicationId]],
    containerPath = containerPath,
    parameterName = parameterName,
    value = value,
    units = units
  )
  project$.markModified()
  invisible(project)
}

#' Remove a parameter from a named application
#'
#' @inheritParams addApplicationParameter
#' @returns The `project` object, invisibly.
#' @export
#' @family parameters
removeApplicationParameter <- function(
  project,
  applicationId,
  containerPath,
  parameterName
) {
  validateIsOfType(project, "Project")
  if (!(applicationId %in% names(project$applications))) {
    stop(paste0("application '", applicationId, "' not found"))
  }
  project$applications[[applicationId]] <- removeParameter(
    project$applications[[applicationId]],
    containerPath = containerPath,
    parameterName = parameterName
  )
  project$.markModified()
  invisible(project)
}

# Public CRUD: individuals ----

#' Add an individual to a Project
#'
#' @param project A `Project` object.
#' @param individualId Character scalar, unique ID for the individual.
#' @param species Character scalar, species name.
#' @param ... Optional named fields: `population`, `gender`, `weight`,
#'   `height`, `age`, `proteinOntogenies`. Numeric fields are coerced
#'   via `as.double()`. Unknown fields trigger an error.
#'
#' @returns The `project` object, invisibly.
#' @export
#' @family individual
addIndividual <- function(project, individualId, species, ...) {
  validateIsOfType(project, "Project")
  errors <- character()

  if (
    !is.character(individualId) ||
      length(individualId) != 1 ||
      is.na(individualId) ||
      nchar(individualId) == 0
  ) {
    errors <- c(errors, "individualId must be a non-empty string")
  } else if (individualId %in% names(project$individuals)) {
    errors <- c(
      errors,
      paste0("individual '", individualId, "' already exists")
    )
  }

  if (
    !is.character(species) ||
      length(species) != 1 ||
      is.na(species) ||
      nchar(species) == 0
  ) {
    errors <- c(errors, "species must be a non-empty string")
  }

  dots <- list(...)
  allowed <- c(
    "population",
    "gender",
    "weight",
    "height",
    "age",
    "proteinOntogenies",
    "parameters"
  )
  unknown <- setdiff(names(dots), allowed)
  if (length(unknown) > 0) {
    errors <- c(
      errors,
      paste0(
        "unknown fields: ",
        paste(unknown, collapse = ", "),
        ". Allowed: ",
        paste(allowed, collapse = ", ")
      )
    )
  }

  if (length(errors) > 0) {
    stop(paste0(
      "Cannot add individual '",
      individualId,
      "':\n- ",
      paste(errors, collapse = "\n- ")
    ))
  }

  entry <- list(species = species, parameters = NULL)
  for (field in c("population", "gender", "proteinOntogenies")) {
    if (!is.null(dots[[field]])) entry[[field]] <- dots[[field]]
  }
  for (field in c("weight", "height", "age")) {
    if (!is.null(dots[[field]])) entry[[field]] <- as.double(dots[[field]])
  }
  if (!is.null(dots$parameters)) {
    pset <- NULL
    for (p in dots$parameters) {
      pset <- .addParameterEntry(
        pset,
        p$containerPath,
        p$parameterName,
        p$value,
        p$units
      )
    }
    entry$parameters <- pset
  }
  class(entry) <- c("Individual", "list")

  project$individuals[[individualId]] <- entry
  project$.markModified()
  invisible(project)
}

#' Remove an individual from a Project
#'
#' @param project A `Project` object.
#' @param individualId Character scalar, ID of the individual to remove.
#' @returns The `project` object, invisibly.
#' @export
#' @family individual
removeIndividual <- function(project, individualId) {
  validateIsOfType(project, "Project")
  if (
    !is.character(individualId) ||
      length(individualId) != 1 ||
      is.na(individualId) ||
      nchar(individualId) == 0
  ) {
    stop("individualId must be a non-empty string")
  }
  if (!(individualId %in% names(project$individuals))) {
    cli::cli_warn("individual {.val {individualId}} not found; no-op.")
    return(invisible(project))
  }
  .warnIfReferenced(project, "individual", individualId)
  project$individuals[[individualId]] <- NULL
  project$.markModified()
  invisible(project)
}

# Public CRUD: populations ----

#' Add a population to a Project
#'
#' @param project A `Project` object.
#' @param populationId Character scalar, unique ID.
#' @param species Character scalar.
#' @param numberOfIndividuals Integer, positive.
#' @param ... Optional named fields. Accepted: `proportionOfFemales`,
#'   `weightMin`, `weightMax`, `heightMin`, `heightMax`, `ageMin`, `ageMax`,
#'   `BMIMin`, `BMIMax`, `gender`, `weightUnit`, `heightUnit`, `ageUnit`,
#'   `BMIUnit`, `population`, `diseaseState`. Numeric range fields are
#'   coerced via `as.double()`.
#'
#' @returns The `project` object, invisibly.
#' @export
#' @family population
addPopulation <- function(
  project,
  populationId,
  species,
  numberOfIndividuals,
  ...
) {
  validateIsOfType(project, "Project")
  errors <- character()

  if (
    !is.character(populationId) ||
      length(populationId) != 1 ||
      is.na(populationId) ||
      nchar(populationId) == 0
  ) {
    errors <- c(errors, "populationId must be a non-empty string")
  } else if (populationId %in% names(project$populations)) {
    errors <- c(
      errors,
      paste0("population '", populationId, "' already exists")
    )
  }

  if (
    !is.character(species) ||
      length(species) != 1 ||
      is.na(species) ||
      nchar(species) == 0
  ) {
    errors <- c(errors, "species must be a non-empty string")
  }

  if (
    !is.numeric(numberOfIndividuals) ||
      length(numberOfIndividuals) != 1 ||
      is.na(numberOfIndividuals) ||
      numberOfIndividuals <= 0
  ) {
    errors <- c(errors, "numberOfIndividuals must be a positive number")
  }

  dots <- list(...)
  numericFields <- c(
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
  stringFields <- c(
    "gender",
    "weightUnit",
    "heightUnit",
    "ageUnit",
    "BMIUnit",
    "population",
    "diseaseState"
  )
  allowed <- c(numericFields, stringFields)
  unknown <- setdiff(names(dots), allowed)
  if (length(unknown) > 0) {
    errors <- c(
      errors,
      paste0(
        "unknown fields: ",
        paste(unknown, collapse = ", "),
        ". Allowed: ",
        paste(allowed, collapse = ", ")
      )
    )
  }

  if (length(errors) > 0) {
    stop(paste0(
      "Cannot add population '",
      populationId,
      "':\n- ",
      paste(errors, collapse = "\n- ")
    ))
  }

  entry <- list(
    species = species,
    numberOfIndividuals = as.double(numberOfIndividuals)
  )
  for (field in numericFields) {
    if (!is.null(dots[[field]])) entry[[field]] <- as.double(dots[[field]])
  }
  for (field in stringFields) {
    if (!is.null(dots[[field]])) entry[[field]] <- dots[[field]]
  }

  project$populations[[populationId]] <- entry
  project$.markModified()
  invisible(project)
}

#' Remove a population from a Project
#' @param project A `Project` object.
#' @param populationId Character scalar.
#' @returns The `project` object, invisibly.
#' @export
#' @family population
removePopulation <- function(project, populationId) {
  validateIsOfType(project, "Project")
  if (
    !is.character(populationId) ||
      length(populationId) != 1 ||
      is.na(populationId) ||
      nchar(populationId) == 0
  ) {
    stop("populationId must be a non-empty string")
  }
  if (!(populationId %in% names(project$populations))) {
    cli::cli_warn("population {.val {populationId}} not found; no-op.")
    return(invisible(project))
  }
  .warnIfReferenced(project, "population", populationId)
  project$populations[[populationId]] <- NULL
  project$.markModified()
  invisible(project)
}

# Public CRUD: model parameters and inline parameters ----

#' Add a parameter to a named model-parameter set
#'
#' @description Adds one parameter entry to the named set in
#' `project$modelParameters`. The set is created on demand if it does not
#' yet exist. Last-write-wins on duplicate paths.
#'
#' @param project A `Project` object.
#' @param id Character scalar, set name. Created if not present.
#' @param containerPath Character scalar.
#' @param parameterName Character scalar.
#' @param value Numeric scalar.
#' @param units Character scalar.
#'
#' @returns The `project` object, invisibly.
#' @export
#' @family parameters
addModelParameter <- function(
  project,
  id,
  containerPath,
  parameterName,
  value,
  units
) {
  validateIsOfType(project, "Project")
  if (!is.character(id) || length(id) != 1 || is.na(id) || nchar(id) == 0) {
    stop("id must be a non-empty string")
  }
  current <- project$modelParameters[[id]]
  project$modelParameters[[id]] <- .addParameterEntry(
    current,
    containerPath,
    parameterName,
    value,
    units
  )
  project$.markModified()
  invisible(project)
}

#' Remove a parameter from a named model-parameter set
#'
#' @description Removes one parameter entry from the named set. If the
#' removed entry was the last in the set, the set itself is auto-removed
#' from `project$modelParameters`. Warns if the set or entry doesn't exist.
#'
#' @param project A `Project` object.
#' @param id Character scalar, set name.
#' @param containerPath Character scalar.
#' @param parameterName Character scalar.
#'
#' @returns The `project` object, invisibly.
#' @export
#' @family parameters
removeModelParameter <- function(project, id, containerPath, parameterName) {
  validateIsOfType(project, "Project")
  if (!is.character(id) || length(id) != 1) {
    stop("id must be a string scalar")
  }
  if (!(id %in% names(project$modelParameters))) {
    cli::cli_warn("model parameter set {.val {id}} not found; no-op.")
    return(invisible(project))
  }
  updated <- .removeParameterEntry(
    project$modelParameters[[id]],
    containerPath,
    parameterName
  )
  if (is.null(updated)) {
    .warnIfReferenced(project, "modelParameterSet", id)
    project$modelParameters[[id]] <- NULL
  } else {
    project$modelParameters[[id]] <- updated
  }
  project$.markModified()
  invisible(project)
}

#' Add a parameter to a named individual
#'
#' @description Convenience wrapper around `addParameter()` that looks up the
#' individual by id, dispatches `addParameter`, and writes the result back.
#' Errors if the id doesn't resolve.
#'
#' @param project A `Project` object.
#' @param individualId Character scalar.
#' @param containerPath Character scalar.
#' @param parameterName Character scalar.
#' @param value Numeric scalar.
#' @param units Character scalar.
#'
#' @returns The `project` object, invisibly.
#' @export
#' @family parameters
addIndividualParameter <- function(
  project,
  individualId,
  containerPath,
  parameterName,
  value,
  units
) {
  validateIsOfType(project, "Project")
  if (!(individualId %in% names(project$individuals))) {
    stop(paste0("individual '", individualId, "' not found"))
  }
  project$individuals[[individualId]] <- addParameter(
    project$individuals[[individualId]],
    containerPath = containerPath,
    parameterName = parameterName,
    value = value,
    units = units
  )
  project$.markModified()
  invisible(project)
}

#' Remove a parameter from a named individual
#'
#' @inheritParams addIndividualParameter
#' @returns The `project` object, invisibly.
#' @export
#' @family parameters
removeIndividualParameter <- function(
  project,
  individualId,
  containerPath,
  parameterName
) {
  validateIsOfType(project, "Project")
  if (!(individualId %in% names(project$individuals))) {
    stop(paste0("individual '", individualId, "' not found"))
  }
  project$individuals[[individualId]] <- removeParameter(
    project$individuals[[individualId]],
    containerPath = containerPath,
    parameterName = parameterName
  )
  project$.markModified()
  invisible(project)
}

# Public CRUD: scenarios and output paths ----

#' Add a scenario programmatically to a Project
#'
#' @description Creates a new `Scenario` and adds it to the
#'   `project$scenarios` list after validating all references.
#'
#' @param project A `Project` object.
#' @param scenarioName Character. Name for the new scenario. Must not already
#'   exist in `project$scenarios`.
#' @param modelFile Character. Name of the `.pkml` model file (relative to
#'   model folder).
#' @param individualId Character or NULL. ID referencing
#'   `project$individuals`.
#' @param populationId Character or NULL. ID referencing
#'   `project$populations`.
#' @param applicationProtocol Character or NULL. Protocol name referencing
#'   `project$applications`.
#' @param modelParameters Character vector or NULL. Group names referencing
#'   `project$modelParameters`.
#' @param outputPathIds Character vector or NULL. IDs referencing
#'   `project$outputPaths`.
#' @param simulationTime Character or NULL. Format `"start, end, resolution"`
#'   or `"start, end, resolution; start, end, resolution"` for multiple
#'   intervals.
#' @param simulationTimeUnit Character. Time unit string. Default `"h"`.
#' @param steadyState Logical. Whether to simulate steady state. Default
#'   `FALSE`.
#' @param steadyStateTime Numeric. Steady-state time in minutes. Default
#'   `1000`.
#' @param overwriteFormulasInSS Logical. Overwrite formulas during steady
#'   state. Default `FALSE`.
#' @param readPopulationFromCSV Logical. Load population from CSV. Default
#'   `FALSE`.
#'
#' @returns The `project` object, invisibly.
#'
#' @export
#' @family scenario
addScenario <- function(
  project,
  scenarioName,
  modelFile,
  individualId = NULL,
  populationId = NULL,
  applicationProtocol = NULL,
  modelParameters = NULL,
  outputPathIds = NULL,
  simulationTime = NULL,
  simulationTimeUnit = "h",
  steadyState = FALSE,
  steadyStateTime = 1000,
  overwriteFormulasInSS = FALSE,
  readPopulationFromCSV = FALSE
) {
  validateIsOfType(project, "Project")
  project <- project
  errors <- character()

  # Validate required args
  if (
    !is.character(scenarioName) ||
      length(scenarioName) != 1 ||
      is.na(scenarioName) ||
      nchar(scenarioName) == 0
  ) {
    errors <- c(errors, "scenarioName must be a non-empty string")
  } else if (scenarioName %in% names(project$scenarios)) {
    errors <- c(errors, paste0("scenario '", scenarioName, "' already exists"))
  }

  if (
    !is.character(modelFile) ||
      length(modelFile) != 1 ||
      is.na(modelFile) ||
      nchar(modelFile) == 0
  ) {
    errors <- c(errors, "modelFile must be a non-empty string")
  }

  # Validate optional references
  if (
    !is.null(individualId) && !(individualId %in% names(project$individuals))
  ) {
    errors <- c(
      errors,
      paste0("individualId '", individualId, "' not found in individuals")
    )
  }
  if (
    !is.null(populationId) && !(populationId %in% names(project$populations))
  ) {
    errors <- c(
      errors,
      paste0("populationId '", populationId, "' not found in populations")
    )
  }
  if (
    !is.null(applicationProtocol) &&
      !(applicationProtocol %in% names(project$applications))
  ) {
    errors <- c(
      errors,
      paste0(
        "applicationProtocol '",
        applicationProtocol,
        "' not found in applications"
      )
    )
  }
  if (!is.null(modelParameters)) {
    bad <- setdiff(modelParameters, names(project$modelParameters))
    if (length(bad) > 0) {
      errors <- c(
        errors,
        paste0(
          "modelParameters not found in modelParameters: ",
          paste(bad, collapse = ", ")
        )
      )
    }
  }
  if (!is.null(outputPathIds)) {
    bad <- setdiff(outputPathIds, names(project$outputPaths))
    if (length(bad) > 0) {
      errors <- c(
        errors,
        paste0(
          "outputPathIds not found in outputPaths: ",
          paste(bad, collapse = ", ")
        )
      )
    }
  }

  if (length(errors) > 0) {
    stop(paste0(
      "Cannot add scenario '",
      scenarioName,
      "':\n- ",
      paste(errors, collapse = "\n- ")
    ))
  }

  # Build Scenario object
  sc <- Scenario$new()
  sc$scenarioName <- scenarioName
  sc$modelFile <- modelFile
  sc$individualId <- individualId
  sc$applicationProtocol <- applicationProtocol %||% NA

  if (!is.null(populationId)) {
    sc$populationId <- populationId
    sc$simulationType <- "Population"
  }

  sc$modelParameters <- modelParameters
  sc$readPopulationFromCSV <- readPopulationFromCSV

  if (!is.null(outputPathIds)) {
    sc$outputPaths <- unname(project$outputPaths[outputPathIds])
  }

  if (!is.null(simulationTime)) {
    sc$simulationTime <- .parseSimulationTimeIntervals(simulationTime)
    sc$simulationTimeUnit <- simulationTimeUnit
  }

  sc$simulateSteadyState <- steadyState
  sc$steadyStateTime <- steadyStateTime
  sc$overwriteFormulasInSS <- overwriteFormulasInSS

  # Add to configuration
  project$scenarios[[scenarioName]] <- sc
  project$.markModified()

  invisible(project)
}

#' Add output paths to a Project
#'
#' @param project A `Project` object.
#' @param id Character vector of output path IDs (unique within the call
#'   and not already present in `project$outputPaths`).
#' @param path Character vector of output paths, same length as `id`.
#' @returns The `project` object, invisibly.
#' @export
#' @family scenario
addOutputPath <- function(project, id, path) {
  validateIsOfType(project, "Project")
  errors <- character()

  if (
    !is.character(id) || length(id) < 1 || any(is.na(id)) || any(nchar(id) == 0)
  ) {
    errors <- c(errors, "id must be a non-empty character vector")
  }
  if (!is.character(path) || length(path) != length(id)) {
    errors <- c(
      errors,
      "id and path must be character vectors of the same length"
    )
  }
  if (is.character(id) && any(duplicated(id))) {
    errors <- c(
      errors,
      paste0(
        "duplicate ids within call: ",
        paste(unique(id[duplicated(id)]), collapse = ", ")
      )
    )
  }
  if (is.character(id)) {
    collisions <- intersect(id, names(project$outputPaths))
    if (length(collisions) > 0) {
      errors <- c(
        errors,
        paste0(
          "outputPath id already exists: ",
          paste(collisions, collapse = ", ")
        )
      )
    }
  }

  if (length(errors) > 0) {
    stop(paste0(
      "Cannot add outputPath:\n- ",
      paste(errors, collapse = "\n- ")
    ))
  }

  newPaths <- path
  names(newPaths) <- id
  project$outputPaths <- c(project$outputPaths, newPaths)
  project$.markModified()
  invisible(project)
}

#' Remove an output path from a Project
#' @param project A `Project` object.
#' @param id Character scalar.
#' @returns The `project` object, invisibly.
#' @export
#' @family scenario
removeOutputPath <- function(project, id) {
  validateIsOfType(project, "Project")
  if (!is.character(id) || length(id) != 1 || is.na(id) || nchar(id) == 0) {
    stop("id must be a non-empty string")
  }
  if (!(id %in% names(project$outputPaths))) {
    cli::cli_warn("outputPath {.val {id}} not found; no-op.")
    return(invisible(project))
  }
  .warnIfReferenced(project, "outputPath", id)
  project$outputPaths <- project$outputPaths[setdiff(
    names(project$outputPaths),
    id
  )]
  project$.markModified()
  invisible(project)
}

#' Remove a scenario from a Project
#' @param project A `Project` object.
#' @param name Character scalar, scenario name.
#' @returns The `project` object, invisibly.
#' @export
#' @family scenario
removeScenario <- function(project, name) {
  validateIsOfType(project, "Project")
  if (
    !is.character(name) || length(name) != 1 || is.na(name) || nchar(name) == 0
  ) {
    stop("name must be a non-empty string")
  }
  if (!(name %in% names(project$scenarios))) {
    cli::cli_warn("scenario {.val {name}} not found; no-op.")
    return(invisible(project))
  }
  project$scenarios[[name]] <- NULL
  project$.markModified()
  invisible(project)
}

# Public CRUD: observed data ----

#' Get names of all observed data in a Project
#'
#' Returns the names of all DataSets that would be returned by
#' `loadObservedData()`. On first call, this loads the data to discover names;
#' subsequent calls return cached names unless the cache is invalidated.
#'
#' @param project A `Project` object (see [loadProject()]).
#' @return A character vector of DataSet names.
#' @examples
#' \dontrun{
#' project <- loadProject(exampleProjectPath())
#' getObservedDataNames(project)
#' }
#' @export
getObservedDataNames <- function(project) {
  validateIsOfType(project, "Project")

  cached <- project$.getObservedDataNamesCache()
  if (!is.null(cached)) {
    return(cached)
  }

  # Load to populate cache
  loadObservedData(project)
  project$.getObservedDataNamesCache()
}

#' Add observed data to a Project
#'
#' @description Add an observedData entry. Accepts either a `DataSet`
#' (creates a `type="programmatic"` entry keyed by `dataSet$name`) or a
#' configuration list with `type` field ("excel", "pkml", or "script")
#' plus source-specific fields.
#'
#' @param project A `Project` object.
#' @param entry Either a `DataSet` object or a configuration list.
#' @returns The `project` object, invisibly.
#' @export
#' @family observedData
addObservedData <- function(project, entry) {
  validateIsOfType(project, "Project")

  if (inherits(entry, "DataSet")) {
    name <- entry$name
    existingNames <- getObservedDataNames(project)
    if (name %in% existingNames) {
      stop(messages$observedDataNameExists(name))
    }
    project$.addProgrammaticDataSet(name, entry)
    project$.appendObservedDataNameCache(name)
    newEntry <- list(type = "programmatic", name = name)
    project$observedData <- c(project$observedData, list(newEntry))
    project$.markModified()
    cli::cli_inform(c(
      "i" = paste0(
        "For reproducibility, consider declaring this DataSet via a script ",
        "in your Project.json using the observedData field with ",
        "type = \"script\" and file = \"scripts/your_script.R\"."
      )
    ))
  } else if (is.list(entry)) {
    if (is.null(entry$type)) {
      stop(messages$observedDataConfigMissingType())
    }
    validTypes <- c("excel", "pkml", "script")
    if (!(entry$type %in% validTypes)) {
      stop(messages$observedDataInvalidType(entry$type, validTypes))
    }
    project$.invalidateObservedDataNamesCache()
    project$observedData <- c(project$observedData, list(entry))
    project$.markModified()
  } else {
    stop(messages$observedDataInvalidEntry())
  }
  invisible(project)
}

#' Remove observed data from a Project
#'
#' @description Removes by DataSet name (for `type="programmatic"` entries)
#' or by `file` basename (for `type="excel"/"pkml"/"script"` entries).
#'
#' @param project A `Project` object.
#' @param name DataSet name or config entry file basename.
#' @returns The `project` object, invisibly.
#' @export
#' @family observedData
removeObservedData <- function(project, name) {
  validateIsOfType(project, "Project")
  if (
    !is.character(name) ||
      length(name) != 1 ||
      is.na(name) ||
      nchar(name) == 0
  ) {
    stop("name must be a non-empty string")
  }

  progDS <- project$.getProgrammaticDataSets()
  if (name %in% names(progDS)) {
    project$.removeProgrammaticDataSet(name)
    # Match by the name stamped on the sentinel; falls back to the first
    # programmatic entry for older configurations whose sentinels predate
    # the `name` field.
    matchIdx <- which(vapply(
      project$observedData,
      function(e) identical(e$type, "programmatic") && identical(e$name, name),
      logical(1)
    ))
    if (length(matchIdx) == 0) {
      matchIdx <- which(vapply(
        project$observedData,
        function(e) identical(e$type, "programmatic"),
        logical(1)
      ))
    }
    if (length(matchIdx) > 0) {
      project$observedData <- project$observedData[-matchIdx[[1]]]
    }
    project$.invalidateObservedDataNamesCache()
    project$.markModified()
    return(invisible(project))
  }

  matchIdx <- which(vapply(
    project$observedData,
    function(e) {
      !is.null(e$file) && identical(basename(e$file), name)
    },
    logical(1)
  ))

  if (length(matchIdx) == 0) {
    cli::cli_warn(messages$observedDataNotFound(name))
    return(invisible(project))
  }

  project$observedData <- project$observedData[-matchIdx[[1]]]
  project$.invalidateObservedDataNamesCache()
  project$.markModified()
  invisible(project)
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

# Project → JSON serialization ----

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
    dataCombinedRows <- df[
      df$DataCombinedName == dataCombinedName,
      ,
      drop = FALSE
    ]

    simRows <- dataCombinedRows[
      dataCombinedRows$dataType == "simulated",
      ,
      drop = FALSE
    ]
    obsRows <- dataCombinedRows[
      dataCombinedRows$dataType == "observed",
      ,
      drop = FALSE
    ]

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

# Excel ↔ JSON bridge: public API ----

#' Import project configuration from Excel files to v2.0 JSON
#'
#' @description Reads all Excel configuration files in an esqlabsR project and
#' produces a single v2.0 JSON file. This is the migration path from
#' Excel-based projects to the JSON-primary workflow.
#'
#' @param projectConfigPath Path to the `Project.xlsx` file.
#'   Defaults to `"Project.xlsx"`.
#' @param outputDir Directory where the JSON file will be saved. If `NULL`
#'   (default), the JSON file is created in the same directory as the source
#'   Excel file.
#' @param silent Logical. If `TRUE`, suppresses informational messages.
#'   Defaults to `FALSE`.
#'
#' @return Invisibly returns the path to the created JSON file.
#' @export
importProjectFromExcel <- function(
  projectConfigPath = "Project.xlsx",
  outputDir = NULL,
  silent = FALSE
) {
  validateIsString(projectConfigPath)

  if (!file.exists(projectConfigPath)) {
    stop(messages$fileNotFound(projectConfigPath))
  }

  # Read the Project.xlsx to get path settings
  pcExcel <- readExcel(projectConfigPath)
  pcDir <- dirname(fs::path_abs(projectConfigPath))

  # Build a lookup of Property -> Value from the Excel file
  pcProps <- stats::setNames(
    as.character(pcExcel$Value),
    as.character(pcExcel$Property)
  )

  # Read version metadata (with fallback for old Excel files)
  schemaVersion <- pcProps[["schemaVersion"]] %||% "2.0"
  esqlabsRVersion <- pcProps[["esqlabsRVersion"]] %||%
    as.character(utils::packageVersion("esqlabsR"))

  # Remove version metadata from file path properties
  pcProps <- pcProps[!names(pcProps) %in% c("schemaVersion", "esqlabsRVersion")]

  # Resolve the configurations folder relative to the Excel file
  configsFolder <- pcProps[["configurationsFolder"]]
  if (!is.null(configsFolder) && !is.na(configsFolder)) {
    configsFolder <- normalizePath(
      file.path(pcDir, configsFolder),
      mustWork = FALSE
    )
  }

  # Helper to resolve a config file path
  resolveConfigFile <- function(fileName) {
    if (is.null(fileName) || is.na(fileName) || fileName == "") {
      return(NULL)
    }
    if (is.null(configsFolder)) {
      return(NULL)
    }
    normalizePath(file.path(configsFolder, fileName), mustWork = FALSE)
  }

  # Build the JSON structure — schemaVersion comes from the Excel source;
  # if the Excel predates versioning, default to "2.0".
  jsonData <- list(
    schemaVersion = schemaVersion,
    esqlabsRVersion = as.character(utils::packageVersion("esqlabsR"))
  )

  # filePaths section — raw path properties
  jsonData$filePaths <- as.list(pcProps)

  # --- OutputPaths ---
  scenariosFile <- resolveConfigFile(pcProps[["scenariosFile"]])
  if (!is.null(scenariosFile) && file.exists(scenariosFile)) {
    sheets <- readxl::excel_sheets(scenariosFile)
    if ("OutputPaths" %in% sheets) {
      outputPathsDf <- readExcel(scenariosFile, sheet = "OutputPaths")
      outputPaths <- stats::setNames(
        as.character(outputPathsDf$OutputPath),
        as.character(outputPathsDf$OutputPathId)
      )
      jsonData$outputPaths <- as.list(outputPaths)
    }
  }

  # --- Scenarios ---
  if (!is.null(scenariosFile) && file.exists(scenariosFile)) {
    sheets <- readxl::excel_sheets(scenariosFile)
    if ("Scenarios" %in% sheets) {
      scenarioDf <- readExcel(scenariosFile, sheet = "Scenarios")
      scenarioDf <- dplyr::filter(scenarioDf, !is.na(Scenario_name))
      jsonData$scenarios <- .parseExcelScenarios(scenarioDf, schemaVersion)
    }
  }

  # --- ModelParameters ---
  modelParamsFile <- resolveConfigFile(pcProps[["modelParamsFile"]])
  if (!is.null(modelParamsFile) && file.exists(modelParamsFile)) {
    jsonData$modelParameters <- .parseExcelParameterSheets(
      modelParamsFile,
      schemaVersion = schemaVersion
    )
  }

  # --- Individuals ---
  individualsFile <- resolveConfigFile(pcProps[["individualsFile"]])
  if (!is.null(individualsFile) && file.exists(individualsFile)) {
    sheets <- readxl::excel_sheets(individualsFile)
    if ("IndividualBiometrics" %in% sheets) {
      indivDf <- readExcel(individualsFile, sheet = "IndividualBiometrics")
      jsonData$individuals <- .parseExcelIndividuals(indivDf, schemaVersion)
    }
    # Per-individual parameter sheets: each sheet whose name matches an
    # individualId becomes that individual's inline `parameters` array.
    indivParamSheets <- setdiff(sheets, "IndividualBiometrics")
    if (length(indivParamSheets) > 0 && !is.null(jsonData$individuals)) {
      paramGroups <- .parseExcelParameterSheets(
        individualsFile,
        sheetNames = indivParamSheets,
        schemaVersion = schemaVersion
      )
      for (i in seq_along(jsonData$individuals)) {
        id <- jsonData$individuals[[i]]$individualId
        grp <- paramGroups[[id]]
        if (!is.null(grp)) {
          jsonData$individuals[[i]]$parameters <- grp
        }
      }
    }
  }

  # --- Populations ---
  populationsFile <- resolveConfigFile(pcProps[["populationsFile"]])
  if (!is.null(populationsFile) && file.exists(populationsFile)) {
    popDf <- readExcel(populationsFile, sheet = 1)
    jsonData$populations <- .parseExcelPopulations(popDf, schemaVersion)
  }

  # --- Applications ---
  applicationsFile <- resolveConfigFile(pcProps[["applicationsFile"]])
  if (!is.null(applicationsFile) && file.exists(applicationsFile)) {
    appGroups <- .parseExcelParameterSheets(
      applicationsFile,
      schemaVersion = schemaVersion
    )
    appsObj <- list()
    for (id in names(appGroups)) {
      appsObj[[id]] <- list(parameters = appGroups[[id]])
    }
    jsonData$applications <- appsObj
  }

  # --- Plots ---
  plotsFile <- resolveConfigFile(pcProps[["plotsFile"]])
  if (!is.null(plotsFile) && file.exists(plotsFile)) {
    jsonData$plots <- .parseExcelPlots(plotsFile, schemaVersion)
  }

  # --- Determine output path ---
  if (is.null(outputDir)) {
    outputDir <- pcDir
  }

  outputFileName <- sub("\\.xlsx$", ".json", basename(projectConfigPath))
  outputPath <- file.path(outputDir, outputFileName)

  if (!dir.exists(dirname(outputPath))) {
    dir.create(dirname(outputPath), recursive = TRUE)
  }

  # Write JSON
  jsonText <- jsonlite::toJSON(
    jsonData,
    pretty = TRUE,
    auto_unbox = TRUE,
    digits = NA,
    null = "null"
  )
  writeLines(jsonText, outputPath)

  if (interactive() && !silent) {
    inputFile <- fs::path_rel(projectConfigPath, start = getwd())
    outputFile <- fs::path_rel(outputPath, start = getwd())
    message(messages$createdFileSnapshot(inputFile, outputFile))
  }

  invisible(outputPath)
}

#' @rdname importProjectFromExcel
#' @param ... Arguments passed to `importProjectFromExcel()`.
#' @export
snapshotProjectConfiguration <- function(...) {
  lifecycle::deprecate_soft(
    what = "snapshotProjectConfiguration()",
    with = "importProjectFromExcel()",
    when = "6.0.0"
  )
  importProjectFromExcel(...)
}

#' Export a Project to Excel files
#'
#' @description Writes Excel configuration files from a `Project`
#' object (typically loaded from JSON). This is the reverse of
#' `importProjectFromExcel()`.
#'
#' @param project A `Project` object.
#' @param outputDir Directory where the Excel files will be created. Defaults
#'   to the directory of the source JSON file.
#' @param silent Logical. If `TRUE`, suppresses informational messages.
#'   Defaults to `FALSE`.
#'
#' @return Invisibly returns the path to the created
#'   `Project.xlsx`.
#' @export
exportProjectToExcel <- function(
  project,
  outputDir = NULL,
  silent = FALSE
) {
  validateIsOfType(project, "Project")

  if (is.null(outputDir)) {
    outputDir <- project$projectDirPath %||% "."
  }

  if (!dir.exists(outputDir)) {
    dir.create(outputDir, recursive = TRUE)
  }

  configDir <- file.path(outputDir, "Configurations")
  if (!dir.exists(configDir)) {
    dir.create(configDir, recursive = TRUE, showWarnings = FALSE)
  }

  # --- Project.xlsx ---
  # Version metadata rows
  props <- c("schemaVersion", "esqlabsRVersion")
  vals <- c("2.0", as.character(utils::packageVersion("esqlabsR")))
  descs <- c(
    "Project structure schema version",
    "esqlabsR version used to generate this file"
  )

  # File path property rows
  filePathsData <- .extractFilePathsData(project)
  for (propName in names(filePathsData)) {
    props <- c(props, propName)
    vals <- c(vals, filePathsData[[propName]]$value %||% "")
    descs <- c(descs, filePathsData[[propName]]$description %||% "")
  }
  projConfigDf <- data.frame(
    Property = props,
    Value = vals,
    Description = descs,
    stringsAsFactors = FALSE
  )
  projConfigPath <- file.path(outputDir, "Project.xlsx")
  .writeExcel(projConfigDf, projConfigPath)

  # --- ModelParameters.xlsx ---
  if (
    !is.null(project$modelParameters) && length(project$modelParameters) > 0
  ) {
    sheets <- .parameterStructuresToExcelSheets(project$modelParameters)
    .writeExcel(sheets, file.path(configDir, "ModelParameters.xlsx"))
  }

  # --- Individuals.xlsx ---
  indivSheets <- list()
  if (!is.null(project$individuals) && length(project$individuals) > 0) {
    indivSheets[["IndividualBiometrics"]] <- .individualsToExcelDf(
      project$individuals
    )
    # One sheet per individual carrying their inline parameters
    indivParamSets <- list()
    for (id in names(project$individuals)) {
      pset <- project$individuals[[id]]$parameters
      if (!is.null(pset) && length(pset$paths) > 0) {
        indivParamSets[[id]] <- pset
      }
    }
    if (length(indivParamSets) > 0) {
      paramSheets <- .parameterStructuresToExcelSheets(indivParamSets)
      indivSheets <- c(indivSheets, paramSheets)
    }
  }
  if (length(indivSheets) > 0) {
    .writeExcel(indivSheets, file.path(configDir, "Individuals.xlsx"))
  }

  # --- Populations.xlsx ---
  if (!is.null(project$populations) && length(project$populations) > 0) {
    popDf <- .populationsToExcelDf(project$populations)
    .writeExcel(popDf, file.path(configDir, "Populations.xlsx"))
  }

  # --- Scenarios.xlsx ---
  scenSheets <- list()
  if (
    !is.null(project$scenarios) &&
      length(project$scenarios) > 0
  ) {
    scenSheets[["Scenarios"]] <- .scenarioConfigurationsToExcelDf(
      project$scenarios,
      outputPaths = project$outputPaths
    )
  }
  if (!is.null(project$outputPaths) && length(project$outputPaths) > 0) {
    scenSheets[["OutputPaths"]] <- data.frame(
      OutputPathId = names(project$outputPaths),
      OutputPath = unname(project$outputPaths),
      stringsAsFactors = FALSE
    )
  }
  if (length(scenSheets) > 0) {
    .writeExcel(scenSheets, file.path(configDir, "Scenarios.xlsx"))
  }

  # --- Applications.xlsx ---
  if (!is.null(project$applications) && length(project$applications) > 0) {
    appParamSets <- list()
    for (id in names(project$applications)) {
      pset <- project$applications[[id]]$parameters
      if (!is.null(pset) && length(pset$paths) > 0) {
        appParamSets[[id]] <- pset
      }
    }
    if (length(appParamSets) > 0) {
      appSheets <- .parameterStructuresToExcelSheets(appParamSets)
      .writeExcel(appSheets, file.path(configDir, "Applications.xlsx"))
    }
  }

  # --- Plots.xlsx ---
  if (!is.null(project$plots)) {
    plotSheets <- list()
    for (sheetName in names(project$plots)) {
      df <- project$plots[[sheetName]]
      if (is.data.frame(df) && nrow(df) > 0) {
        plotSheets[[sheetName]] <- df
      }
    }
    if (length(plotSheets) > 0) {
      .writeExcel(plotSheets, file.path(configDir, "Plots.xlsx"))
    }
  }

  if (interactive() && !silent) {
    relPath <- fs::path_rel(projConfigPath, start = getwd())
    message(messages$restoredProject(
      project$jsonPath %||% "Project",
      relPath
    ))
  }

  invisible(projConfigPath)
}

#' @rdname exportProjectToExcel
#' @param jsonPath Path to the JSON configuration file. Defaults to
#'   `"Project.json"`.
#' @param ... Additional arguments (unused).
#' @export
restoreProjectConfiguration <- function(
  jsonPath = "Project.json",
  outputDir = NULL,
  silent = FALSE,
  ...
) {
  lifecycle::deprecate_soft(
    what = "restoreProjectConfiguration()",
    with = "exportProjectToExcel()",
    when = "6.0.0"
  )
  project <- loadProject(jsonPath)
  exportProjectToExcel(
    project = project,
    outputDir = outputDir,
    silent = silent
  )
  invisible(project)
}

#' Check if Excel configuration files are in sync with JSON
#'
#' @description Compares Excel configuration files against their JSON
#' configuration to determine if they are synchronized.
#'
#' @param projectConfigPath Path to a `Project.xlsx` file.
#'   Defaults to `"Project.xlsx"`.
#' @param jsonPath Path to the JSON configuration file. If `NULL` (default),
#'   the function looks for a JSON file with the same base name.
#' @param silent Logical indicating whether to suppress informational messages.
#'   Defaults to `FALSE`.
#'
#' @return A list with components: \item{in_sync}{Logical indicating whether
#'   all files are synchronized} \item{details}{A list with detailed comparison
#'   results}
#'
#' @import cli
#' @export
projectStatus <- function(
  projectConfigPath = "Project.xlsx",
  jsonPath = NULL,
  silent = FALSE,
  ignoreVersionCheck = TRUE
) {
  # Accept either a path string or a Project object for
  # backwards compatibility
  if (inherits(projectConfigPath, "Project")) {
    pcObj <- projectConfigPath
    # projectFilePath stores the JSON path; derive the Excel path
    pcJsonPath <- pcObj$projectFilePath
    projectConfigPath <- sub("\\.json$", ".xlsx", pcJsonPath)
    if (is.null(jsonPath)) {
      jsonPath <- pcJsonPath
    }
  }

  if (!file.exists(projectConfigPath)) {
    stop(messages$fileNotFound(projectConfigPath))
  }

  # Determine JSON path if not provided
  if (is.null(jsonPath)) {
    jsonPath <- sub("\\.xlsx$", ".json", projectConfigPath)
  }

  if (!file.exists(jsonPath)) {
    stop("JSON file does not exist: ", jsonPath)
  }

  # Create temporary snapshot from current Excel files
  tempDir <- tempfile("config_snapshot")
  dir.create(tempDir, showWarnings = FALSE, recursive = TRUE)
  on.exit(unlink(tempDir, recursive = TRUE), add = TRUE)

  tempJsonPath <- file.path(tempDir, basename(jsonPath))
  importProjectFromExcel(
    projectConfigPath,
    outputDir = tempDir,
    silent = TRUE
  )

  # Load both JSON files as lists so we can strip volatile fields
  originalJsonObj <- jsonlite::fromJSON(jsonPath, simplifyVector = FALSE)
  currentJsonObj <- jsonlite::fromJSON(tempJsonPath, simplifyVector = FALSE)

  # Remove esqlabsRVersion — it changes with package updates and would cause
  # false out-of-sync reports
  originalJsonObj[["esqlabsRVersion"]] <- NULL
  currentJsonObj[["esqlabsRVersion"]] <- NULL

  if (identical(originalJsonObj, currentJsonObj)) {
    result <- list(
      in_sync = TRUE,
      details = list(),
      unsaved_changes = FALSE
    )
    if (!silent) {
      message(messages$excelInSync())
    }
  } else {
    fileChanges <- list()
    dataChanges <- list()
    fileStatus <- list()

    originalFiles <- names(originalJsonObj)
    currentFiles <- names(currentJsonObj)

    missingFiles <- setdiff(originalFiles, currentFiles)
    for (file in missingFiles) {
      fileChanges[[file]] <- "Section missing in current Excel"
      fileStatus[[file]] <- "out-of-sync"
    }

    addedFiles <- setdiff(currentFiles, originalFiles)
    for (file in addedFiles) {
      fileChanges[[file]] <- "New section not present in snapshot"
      fileStatus[[file]] <- "out-of-sync"
    }

    commonFiles <- intersect(originalFiles, currentFiles)

    for (file in commonFiles) {
      if (!(file %in% names(fileStatus))) {
        fileStatus[[file]] <- "in-sync"
      }
      if (!identical(originalJsonObj[[file]], currentJsonObj[[file]])) {
        fileStatus[[file]] <- "out-of-sync"
        dataChanges[[file]] <- "data differs"
      }
    }

    differences <- list(
      file_status = fileStatus,
      file_changes = if (length(fileChanges) > 0) fileChanges else NULL,
      data_changes = if (length(dataChanges) > 0) dataChanges else NULL
    )

    result <- list(
      in_sync = FALSE,
      details = differences,
      unsaved_changes = FALSE
    )

    if (!silent) {
      warning(messages$excelNotInSync())

      cli::cli_h2("File Sync Status:")
      for (file in names(fileStatus)) {
        status_text <- fileStatus[[file]]
        if (status_text == "in-sync") {
          cli::cli_text(
            "{.green {cli::symbol$tick}} {file}: {status_text}"
          )
        } else {
          cli::cli_text(
            "{.red {cli::symbol$cross}} {file}: {status_text}"
          )
        }
      }

      cli::cli_h2("Suggested Actions:")
      cli::cli_text("To resolve these differences, you can:")
      cli::cli_ul()
      cli::cli_li(
        "{.run importProjectFromExcel()} - Update JSON from Excel files."
      )
      cli::cli_li(
        "{.run exportProjectToExcel()} - Recreate Excel files from JSON."
      )
      cli::cli_end()
    }
  }

  invisible(result)
}

#' @rdname projectStatus
#' @export
projectConfigurationStatus <- function(...) {
  lifecycle::deprecate_soft(
    what = "projectConfigurationStatus()",
    with = "projectStatus()",
    when = "6.0.0"
  )
  projectStatus(...)
}

# Excel ↔ JSON bridge: internal helpers ----

#' Parse parameter sheets from an Excel file into JSON structure
#' @param filePath Path to the Excel file
#' @param sheetNames Sheets to read. If NULL, reads all sheets.
#' @param schemaVersion Project structure schema version. Used to adapt parsing
#'   logic when importing Excel files generated by an older schema.
#' @returns Named list of parameter arrays
#' @keywords internal
#' @noRd
.parseExcelParameterSheets <- function(
  filePath,
  sheetNames = NULL,
  schemaVersion = "2.0"
) {
  if (is.null(sheetNames)) {
    sheetNames <- readxl::excel_sheets(filePath)
  }
  result <- list()
  for (sheet in sheetNames) {
    df <- readExcel(filePath, sheet = sheet)
    entries <- list()
    if (nrow(df) > 0) {
      for (i in seq_len(nrow(df))) {
        entry <- list(
          containerPath = as.character(df[["Container Path"]][[i]]),
          parameterName = as.character(df[["Parameter Name"]][[i]]),
          value = as.numeric(df[["Value"]][[i]]),
          units = if (is.na(df[["Units"]][[i]]) || df[["Units"]][[i]] == "") {
            NULL
          } else {
            as.character(df[["Units"]][[i]])
          }
        )
        entries[[i]] <- entry
      }
    }
    result[[sheet]] <- entries
  }
  result
}

#' Parse Scenarios Excel sheet into JSON structure
#' @param scenarioDf Data frame from the Scenarios sheet
#' @param schemaVersion Project structure schema version. Used to adapt parsing
#'   logic when importing Excel files generated by an older schema.
#' @returns List of scenario objects
#' @keywords internal
#' @noRd
.parseExcelScenarios <- function(scenarioDf, schemaVersion = "2.0") {
  scenarios <- list()
  for (i in seq_len(nrow(scenarioDf))) {
    row <- scenarioDf[i, ]
    scenario <- list(
      name = as.character(row$Scenario_name),
      individualId = .naToNull(as.character(row$IndividualId)),
      populationId = .naToNull(as.character(row$PopulationId)),
      readPopulationFromCSV = .naToNull(as.logical(row$ReadPopulationFromCSV)),
      modelParameters = .parseCommaListToArray(row$ModelParameterSheets),
      applicationProtocol = .naToNull(as.character(row$ApplicationProtocol)),
      simulationTime = .naToNull(as.character(row$SimulationTime)),
      simulationTimeUnit = .naToNull(as.character(row$SimulationTimeUnit)),
      steadyState = .naToNull(as.logical(row$SteadyState)),
      steadyStateTime = .naToNull(as.numeric(row$SteadyStateTime)),
      steadyStateTimeUnit = .naToNull(as.character(row$SteadyStateTimeUnit)),
      overwriteFormulasInSS = .naToNull(as.logical(row$OverwriteFormulasInSS)),
      modelFile = as.character(row$ModelFile),
      outputPathIds = .parseCommaListToArray(row$OutputPathsIds)
    )
    scenarios[[i]] <- scenario
  }
  scenarios
}

#' Parse IndividualBiometrics Excel sheet into JSON structure
#' @param indivDf Data frame from the IndividualBiometrics sheet
#' @param schemaVersion Project structure schema version. Used to adapt parsing
#'   logic when importing Excel files generated by an older schema.
#' @returns List of individual objects
#' @keywords internal
#' @noRd
.parseExcelIndividuals <- function(indivDf, schemaVersion = "2.0") {
  individuals <- list()
  for (i in seq_len(nrow(indivDf))) {
    row <- indivDf[i, ]
    indiv <- list(
      individualId = as.character(row$IndividualId),
      species = as.character(row$Species),
      population = as.character(row$Population),
      gender = as.character(row$Gender),
      weight = .naToNull(as.numeric(row$`Weight [kg]`)),
      height = .naToNull(as.numeric(row$`Height [cm]`)),
      age = .naToNull(as.numeric(row$`Age [year(s)]`)),
      proteinOntogenies = .naToNull(as.character(row$`Protein Ontogenies`))
    )
    individuals[[i]] <- indiv
  }
  individuals
}

#' Parse Populations Excel sheet into JSON structure
#' @param popDf Data frame from the Demographics sheet
#' @param schemaVersion Project structure schema version. Used to adapt parsing
#'   logic when importing Excel files generated by an older schema.
#' @returns List of population objects
#' @keywords internal
#' @noRd
.parseExcelPopulations <- function(popDf, schemaVersion = "2.0") {
  populations <- list()
  for (i in seq_len(nrow(popDf))) {
    row <- popDf[i, ]
    pop <- list(
      populationId = as.character(row$PopulationName),
      species = as.character(row$species),
      population = as.character(row$population),
      numberOfIndividuals = .naToNull(as.numeric(row$numberOfIndividuals)),
      proportionOfFemales = .naToNull(as.numeric(row$proportionOfFemales)),
      weightMin = .naToNull(as.numeric(row$weightMin)),
      weightMax = .naToNull(as.numeric(row$weightMax)),
      weightUnit = .naToNull(as.character(row$weightUnit)),
      heightMin = .naToNull(as.numeric(row$heightMin)),
      heightMax = .naToNull(as.numeric(row$heightMax)),
      heightUnit = .naToNull(as.character(row$heightUnit)),
      ageMin = .naToNull(as.numeric(row$ageMin)),
      ageMax = .naToNull(as.numeric(row$ageMax)),
      BMIMin = .naToNull(as.numeric(row$BMIMin)),
      BMIMax = .naToNull(as.numeric(row$BMIMax)),
      BMIUnit = .naToNull(as.character(row$BMIUnit)),
      proteinOntogenies = .naToNull(as.character(row$`Protein Ontogenies`))
    )
    populations[[i]] <- pop
  }
  populations
}

#' Parse Plots Excel file into JSON structure
#' @param plotsFile Path to the Plots.xlsx file
#' @param schemaVersion Project structure schema version. Used to adapt parsing
#'   logic when importing Excel files generated by an older schema.
#' @returns Named list with dataCombined, plotConfiguration, plotGrids,
#'   exportConfiguration
#' @keywords internal
#' @noRd
.parseExcelPlots <- function(plotsFile, schemaVersion = "2.0") {
  sheets <- readxl::excel_sheets(plotsFile)
  result <- list()
  for (sheet in sheets) {
    df <- readExcel(plotsFile, sheet = sheet)
    if (nrow(df) == 0) {
      result[[sheet]] <- list()
      next
    }
    entries <- list()
    for (i in seq_len(nrow(df))) {
      entry <- list()
      for (col in names(df)) {
        val <- df[[col]][[i]]
        entry[[col]] <- .naToNull(val)
      }
      entries[[i]] <- entry
    }
    result[[sheet]] <- entries
  }
  result
}

#' Convert parameter structures to Excel sheet data frames
#' @param parameterGroups Named list of parameter structures (paths, values,
#'   units)
#' @returns Named list of data frames suitable for Excel sheets
#' @keywords internal
#' @noRd
.parameterStructuresToExcelSheets <- function(parameterGroups) {
  sheets <- list()
  for (name in names(parameterGroups)) {
    params <- parameterGroups[[name]]
    if (is.null(params) || length(params$paths) == 0) {
      sheets[[name]] <- data.frame(
        `Container Path` = character(0),
        `Parameter Name` = character(0),
        Value = numeric(0),
        Units = character(0),
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
      next
    }
    splitPaths <- lapply(
      params$paths,
      .splitParameterPathIntoContainerAndName
    )
    sheets[[name]] <- data.frame(
      `Container Path` = vapply(
        splitPaths,
        function(x) x$containerPath,
        character(1)
      ),
      `Parameter Name` = vapply(
        splitPaths,
        function(x) x$parameterName,
        character(1)
      ),
      Value = params$values,
      Units = params$units,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  }
  sheets
}

#' Convert individuals data to an IndividualBiometrics data frame
#' @param individuals Named list of IndividualCharacteristics objects
#' @returns A data frame
#' @keywords internal
#' @noRd
.individualsToExcelDf <- function(individuals) {
  rows <- list()
  for (indivId in names(individuals)) {
    ic <- individuals[[indivId]]
    ontoStr <- ic$proteinOntogenies %||% NA

    rows[[length(rows) + 1]] <- data.frame(
      IndividualId = indivId,
      Species = as.character(ic$species),
      Population = as.character(ic$population %||% NA),
      Gender = as.character(ic$gender),
      `Weight [kg]` = as.double(ic$weight %||% NA),
      `Height [cm]` = as.double(ic$height %||% NA),
      `Age [year(s)]` = as.double(ic$age %||% NA),
      `Protein Ontogenies` = ontoStr,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  }
  do.call(rbind, rows)
}

#' Convert populations data to an Excel data frame
#' @param populations Named list of PopulationCharacteristics objects
#' @returns A data frame
#' @keywords internal
#' @noRd
.populationsToExcelDf <- function(populations) {
  rows <- list()
  for (popId in names(populations)) {
    popData <- populations[[popId]]
    ontoStr <- popData$proteinOntogenies %||% NA

    rows[[length(rows) + 1]] <- data.frame(
      PopulationName = popId,
      species = as.character(popData$species),
      population = as.character(popData$population %||% NA),
      numberOfIndividuals = as.double(popData$numberOfIndividuals %||% NA),
      proportionOfFemales = as.double(popData$proportionOfFemales %||% NA),
      weightMin = as.double(popData$weightMin %||% NA),
      weightMax = as.double(popData$weightMax %||% NA),
      weightUnit = as.character(popData$weightUnit %||% NA),
      heightMin = as.double(popData$heightMin %||% NA),
      heightMax = as.double(popData$heightMax %||% NA),
      heightUnit = as.character(popData$heightUnit %||% NA),
      ageMin = as.double(popData$ageMin %||% NA),
      ageMax = as.double(popData$ageMax %||% NA),
      BMIMin = as.double(popData$BMIMin %||% NA),
      BMIMax = as.double(popData$BMIMax %||% NA),
      BMIUnit = as.character(popData$BMIUnit %||% NA),
      `Protein Ontogenies` = ontoStr,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  }
  do.call(rbind, rows)
}

#' Convert Scenario objects to an Excel data frame
#' @param scenarioConfigs Named list of Scenario objects
#' @param outputPaths Named character vector of output paths (names are IDs,
#'   values are path strings) from `Project$outputPaths`.
#'   Used to reverse-lookup scenario output paths back to IDs.
#' @returns A data frame
#' @keywords internal
#' @noRd
.scenarioConfigurationsToExcelDf <- function(
  scenarioConfigs,
  outputPaths = NULL
) {
  rows <- list()
  for (name in names(scenarioConfigs)) {
    sc <- scenarioConfigs[[name]]
    paramGroupNames <- sc$modelParameters
    paramGroupsStr <- if (
      !is.null(paramGroupNames) && length(paramGroupNames) > 0
    ) {
      paste(paramGroupNames, collapse = ", ")
    } else {
      NA
    }
    # simulationTime → string representation
    simTimeStr <- NA
    if (!is.null(sc$simulationTime)) {
      intervals <- vapply(
        sc$simulationTime,
        function(interval) {
          paste(interval, collapse = ", ")
        },
        character(1)
      )
      simTimeStr <- paste(intervals, collapse = "; ")
    }
    # outputPaths → reverse-lookup IDs from project$outputPaths
    outputPathIdsStr <- NA
    if (!is.null(sc$outputPaths) && !is.null(outputPaths)) {
      matchedIds <- names(outputPaths)[match(sc$outputPaths, outputPaths)]
      matchedIds <- matchedIds[!is.na(matchedIds)]
      if (length(matchedIds) > 0) {
        outputPathIdsStr <- paste(matchedIds, collapse = ", ")
      }
    }

    # Reconstruct steadyStateTime back to the original unit
    ssTime <- NA
    ssTimeUnit <- NA
    if (
      !is.null(sc$steadyStateTime) &&
        !is.na(sc$steadyStateTime) &&
        sc$steadyStateTime > 0
    ) {
      ssTimeUnit <- sc$steadyStateTimeUnit %||% "min"
      ssTime <- ospsuite::toUnit(
        quantityOrDimension = ospDimensions$Time,
        values = sc$steadyStateTime,
        targetUnit = ssTimeUnit
      )
    }

    rows[[length(rows) + 1]] <- data.frame(
      Scenario_name = sc$scenarioName,
      IndividualId = sc$individualId %||% NA,
      PopulationId = if (sc$simulationType == "Population") {
        sc$populationId
      } else {
        NA
      },
      ReadPopulationFromCSV = sc$readPopulationFromCSV %||% FALSE,
      ModelParameterSheets = paramGroupsStr,
      ApplicationProtocol = sc$applicationProtocol %||% NA,
      SimulationTime = simTimeStr,
      SimulationTimeUnit = sc$simulationTimeUnit %||% NA,
      SteadyState = sc$simulateSteadyState %||% FALSE,
      SteadyStateTime = ssTime,
      SteadyStateTimeUnit = ssTimeUnit,
      OverwriteFormulasInSS = sc$overwriteFormulasInSS %||% FALSE,
      ModelFile = sc$modelFile %||% NA,
      OutputPathsIds = outputPathIdsStr,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  }
  do.call(rbind, rows)
}

#' Extract private .filePathsData from a Project
#' @param project Project object
#' @returns Named list of property data
#' @keywords internal
#' @noRd
.extractFilePathsData <- function(project) {
  project$.getFilePathsData()
}

#' Convert NA to NULL for JSON serialization
#' @keywords internal
#' @noRd
.naToNull <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return(NULL)
  }
  if (length(x) == 1L && is.na(x)) {
    return(NULL)
  }
  x
}

#' Parse a comma-separated string into a character vector, or NULL
#' @keywords internal
#' @noRd
.parseCommaListToArray <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x) || x == "") {
    return(NULL)
  }
  parts <- trimws(strsplit(as.character(x), ",", fixed = TRUE)[[1]])
  parts[nzchar(parts)]
}
