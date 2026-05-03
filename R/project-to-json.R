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

