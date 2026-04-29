# Observed data loading ----

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
