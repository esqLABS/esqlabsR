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
