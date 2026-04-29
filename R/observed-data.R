# Observed data: section management + loader dispatch.
#
# Section concerns own Project$observedData end-to-end. Called by:
#   - Project$.read_json() via .parseObservedData()
#   - .runProjectValidation() via .validateObservedData()
#   - .projectToJson() — observedData is pass-through (in-memory shape == JSON
#     shape), so there is no .observedDataToJson() function; .projectToJson()
#     just copies project$observedData verbatim.
#   - users via the public addObservedData / removeObservedData /
#     getObservedDataNames / loadObservedData functions.
#
# loadObservedData() is the runtime loader that resolves declared sources
# (excel / pkml / script / programmatic) to ospsuite::DataSet objects.

# Parse ----

#' @keywords internal
#' @noRd
.parseObservedData <- function(observedDataConfig) {
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
}

# Validate ----

#' Validate observedData section of a Project
#' @param observedData List of observedData entries from project$observedData
#' @param dataFolder Path to the project's data folder
#' @return validationResult object
#' @keywords internal
.validateObservedData <- function(observedData, dataFolder) {
  result <- validationResult$new()

  if (is.null(observedData) || length(observedData) == 0) {
    result$add_warning("Data", "No observedData defined")
    return(result)
  }

  validTypes <- c("excel", "pkml", "script", "programmatic")

  for (i in seq_along(observedData)) {
    entry <- observedData[[i]]
    entryLabel <- paste0("observedData entry ", i)

    # Check type field
    if (is.null(entry$type)) {
      result$add_critical_error(
        "Missing Fields",
        paste0(entryLabel, " is missing required field 'type'")
      )
      next
    }

    if (!entry$type %in% validTypes) {
      result$add_critical_error(
        "Invalid Value",
        paste0(
          entryLabel,
          " has invalid type '",
          entry$type,
          "'. Must be one of: ",
          paste(validTypes, collapse = ", ")
        )
      )
      next
    }

    # Type-specific validation
    if (entry$type == "excel") {
      if (is.null(entry$file)) {
        result$add_critical_error(
          "Missing Fields",
          paste0(entryLabel, " (excel) is missing required field 'file'")
        )
      } else {
        filePath <- file.path(dataFolder, entry$file)
        if (!file.exists(filePath)) {
          result$add_warning(
            "File Not Found",
            paste0(entryLabel, " references non-existent file: ", entry$file)
          )
        }
      }

      if (is.null(entry$importerConfiguration)) {
        result$add_critical_error(
          "Missing Fields",
          paste0(
            entryLabel,
            " (excel) is missing required field 'importerConfiguration'"
          )
        )
      } else {
        importerPath <- file.path(dataFolder, entry$importerConfiguration)
        if (!file.exists(importerPath)) {
          result$add_warning(
            "File Not Found",
            paste0(
              entryLabel,
              " references non-existent importer config: ",
              entry$importerConfiguration
            )
          )
        }
      }

      if (is.null(entry$sheets) || length(entry$sheets) == 0) {
        result$add_critical_error(
          "Missing Fields",
          paste0(entryLabel, " (excel) is missing required field 'sheets'")
        )
      }
    }

    if (entry$type %in% c("pkml", "script")) {
      if (is.null(entry$file)) {
        result$add_critical_error(
          "Missing Fields",
          paste0(
            entryLabel,
            " (",
            entry$type,
            ") is missing required field 'file'"
          )
        )
      } else {
        filePath <- file.path(dataFolder, entry$file)
        if (!file.exists(filePath)) {
          result$add_warning(
            "File Not Found",
            paste0(entryLabel, " references non-existent file: ", entry$file)
          )
        }
      }
    }
  }

  result
}

#' @keywords internal
#' @noRd
.observedDataValidatorAdapter <- function(project) {
  .validateObservedData(project$observedData, project$dataFolder)
}

# Load ----

#' Load observed data declared in a Project
#'
#' Reads the `observedData` declarations from a `Project` object and returns
#' the corresponding `DataSet` objects. Supports multiple source types:
#' - **excel**: loaded via the configured data importer
#' - **pkml**: loaded directly from PKML files
#' - **script**: R scripts are sourced and must return DataSet or list of DataSet objects
#' - **programmatic**: DataSets added via `project$addObservedData()`
#'
#' @param project A `Project` object (see [loadProject()]).
#' @return A named list of `ospsuite::DataSet` objects. Empty list if no
#'   observed data is declared.
#' @examples
#' \dontrun{
#' project <- loadProject(exampleProjectPath())
#' dataSets <- loadObservedData(project)
#' }
#' @export
loadObservedData <- function(project) {
  validateIsOfType(project, "Project")

  if (is.null(project$observedData)) {
    return(list())
  }

  allDataSets <- list()
  for (entry in project$observedData) {
    dataSets <- switch(
      entry$type,
      "excel" = {
        filePath <- file.path(project$dataFolder, entry$file)
        importerPath <- file.path(
          project$dataFolder,
          entry$importerConfiguration
        )
        importerConfig <- ospsuite::loadDataImporterConfiguration(
          configurationFilePath = importerPath
        )
        importerConfig$sheets <- unlist(entry$sheets)
        ospsuite::loadDataSetsFromExcel(
          xlsFilePath = filePath,
          importerConfigurationOrPath = importerConfig,
          importAllSheets = FALSE
        )
      },
      "pkml" = {
        filePath <- file.path(project$dataFolder, entry$file)
        ds <- ospsuite::loadDataSetFromPKML(filePath = filePath)
        stats::setNames(list(ds), ds$name)
      },
      "script" = {
        filePath <- file.path(project$dataFolder, entry$file)
        if (!file.exists(filePath)) {
          stop(messages$scriptFileNotFound(filePath))
        }
        cli::cli_inform(c(
          "i" = "Sourcing observed-data script: {.path {filePath}}"
        ))
        result <- source(filePath, local = TRUE)$value
        if (inherits(result, "DataSet")) {
          stats::setNames(list(result), result$name)
        } else if (
          is.list(result) && all(sapply(result, inherits, "DataSet"))
        ) {
          result
        } else {
          stop(messages$scriptWrongReturnType(filePath, class(result)[[1]]))
        }
      },
      "programmatic" = {
        NULL
      }
    )
    allDataSets <- c(allDataSets, dataSets)
  }

  # Add programmatic DataSets (keyed by dataSet$name)
  allDataSets <- c(allDataSets, project$.getProgrammaticDataSets())

  # Cache the names
  project$.cacheObservedDataNames(names(allDataSets))

  allDataSets
}

# Public CRUD ----

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
