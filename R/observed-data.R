# Observed data: project-driven loader dispatch.
#
# Owns Project$observedData end-to-end. Called by:
#   - users via loadObservedData(project) — the public dispatcher.
#   - createDataCombined() internally to resolve observed sources.
#   - validateProject() via .observedDataValidatorAdapter()
#
# The four declared source types in v2.0 Project.json are:
#   - excel  : ospsuite::loadDataSetsFromExcel via importer config
#   - pkml   : ospsuite::loadDataSetFromPKML
#   - script : R script sourced; must return DataSet or list of DataSets
#   - programmatic : DataSets added at runtime via project$addObservedData()
#                    (lands with the mutation API in a later milestone —
#                    currently errors fast).

# Section validation adapter ----

#' @keywords internal
#' @noRd
.observedDataValidatorAdapter <- function(project) {
  .validateObservedData(project$observedData, project$dataFolder)
}

#' Validate the `observedData` section of a Project
#'
#' Per-entry checks: `type` is set and is one of `excel`, `pkml`,
#' `script`, `programmatic`; per-type required fields are present;
#' referenced files exist on disk (warnings only — missing files do
#' not block parsing).
#'
#' @param observedData List from `project$observedData`.
#' @param dataFolder Resolved absolute path to the project's data
#'   folder, or `NULL` when unset.
#' @return validationResult.
#' @keywords internal
#' @noRd
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

    if (entry$type == "excel") {
      if (is.null(entry$file)) {
        result$add_critical_error(
          "Missing Fields",
          paste0(entryLabel, " (excel) is missing required field 'file'")
        )
      } else if (!is.null(dataFolder)) {
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
      } else if (!is.null(dataFolder)) {
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
      } else if (!is.null(dataFolder)) {
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

#' Load observed data declared in a Project
#'
#' @description
#' Reads the `observedData` declarations from a [Project][loadProject()]
#' and returns the corresponding [`ospsuite::DataSet`] objects. Source
#' types: `excel` (via importer configuration), `pkml`, `script`. The
#' `programmatic` type is reserved for a later milestone and currently
#' errors fast.
#'
#' @param project A `Project` object (see [loadProject()]).
#' @returns A named list of [`ospsuite::DataSet`] objects. Empty list when
#'   `project$observedData` is empty or `NULL`.
#' @examples
#' \dontrun{
#' project <- loadProject("path/to/Project.json")
#' dataSets <- loadObservedData(project)
#' }
#' @export
loadObservedData <- function(project) {
  ospsuite.utils::validateIsOfType(project, "Project")
  if (is.null(project$observedData) || length(project$observedData) == 0) {
    return(list())
  }
  allDataSets <- list()
  for (i in seq_along(project$observedData)) {
    entry <- project$observedData[[i]]
    .validateObservedDataEntry(entry, i)
    dataSets <- switch(
      entry$type,
      "excel" = .loadObservedExcel(entry, project$dataFolder),
      "pkml" = .loadObservedPkml(entry, project$dataFolder),
      "script" = .loadObservedScript(entry, project$dataFolder),
      "programmatic" = cli::cli_abort(
        messages$observedDataProgrammaticNotYetAvailable()
      )
    )
    allDataSets <- c(allDataSets, dataSets)
  }
  allDataSets
}

# Internal helpers ----

.validateObservedDataEntry <- function(entry, entryIndex) {
  validTypes <- c("excel", "pkml", "script", "programmatic")
  if (is.null(entry$type) || !(entry$type %in% validTypes)) {
    cli::cli_abort(
      messages$observedDataInvalidEntryType(
        entry$type %||% "<unset>",
        validTypes
      )
    )
  }
  required <- switch(
    entry$type,
    "excel" = c("file", "importerConfiguration", "sheets"),
    "pkml" = "file",
    "script" = "file",
    "programmatic" = character(0)
  )
  for (field in required) {
    if (is.null(entry[[field]])) {
      cli::cli_abort(
        messages$observedDataMissingField(entryIndex, entry$type, field)
      )
    }
  }
  invisible(TRUE)
}

.resolveDataPath <- function(file, dataFolder) {
  if (is.null(dataFolder)) {
    cli::cli_abort(messages$observedDataDataFolderNotDeclared(file))
  }
  filePath <- file.path(dataFolder, file)
  if (!file.exists(filePath)) {
    cli::cli_abort(messages$observedDataFileNotFound(filePath))
  }
  filePath
}

.loadObservedExcel <- function(entry, dataFolder) {
  filePath <- .resolveDataPath(entry$file, dataFolder)
  importerPath <- .resolveDataPath(entry$importerConfiguration, dataFolder)
  importerConfig <- ospsuite::loadDataImporterConfiguration(
    configurationFilePath = importerPath
  )
  importerConfig$sheets <- unlist(entry$sheets)
  ospsuite::loadDataSetsFromExcel(
    xlsFilePath = filePath,
    importerConfigurationOrPath = importerConfig,
    importAllSheets = FALSE
  )
}

.loadObservedPkml <- function(entry, dataFolder) {
  filePath <- .resolveDataPath(entry$file, dataFolder)
  ds <- ospsuite::loadDataSetFromPKML(filePath = filePath)
  stats::setNames(list(ds), ds$name)
}

.loadObservedScript <- function(entry, dataFolder) {
  filePath <- .resolveDataPath(entry$file, dataFolder)
  cli::cli_inform(c(
    "i" = "Sourcing observed-data script: {.path {filePath}}"
  ))
  result <- source(filePath, local = TRUE)$value
  if (inherits(result, "DataSet")) {
    return(stats::setNames(list(result), result$name))
  }
  if (
    is.list(result) &&
      length(result) > 0 &&
      all(vapply(result, inherits, logical(1), "DataSet"))
  ) {
    return(result)
  }
  cli::cli_abort(
    messages$observedDataScriptWrongReturnType(filePath, class(result)[[1]])
  )
}
