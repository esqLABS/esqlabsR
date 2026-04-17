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
      stop("No path specified and project has no jsonPath. Provide a path argument.")
    }
  }

  jsonData <- .projectToJson(project)
  jsonlite::write_json(jsonData, path, auto_unbox = TRUE, null = "null", pretty = TRUE)

  project$modified <- FALSE
  invisible(path)
}

#' Convert a Project object to a JSON-serializable list
#' @param project A `Project` object.
#' @returns A list matching the Project.json schema.
#' @keywords internal
#' @noRd
.projectToJson <- function(project) {
  list(
    schemaVersion = project$schemaVersion %||% "2.0",
    esqlabsRVersion = as.character(utils::packageVersion("esqlabsR")),
    filePaths = .filePathsToJson(project),
    observedData = project$observedData %||% list(),
    outputPaths = as.list(project$outputPaths) %||% list(),
    scenarios = .scenariosToJson(project$scenarios, project$outputPaths),
    modelParameters = .parameterGroupsToJson(project$modelParameters),
    individuals = .individualsToJson(project$individuals, project$individualParameterSetMapping),
    individualParameterSets = .parameterGroupsToJson(project$individualParameterSets),
    populations = .populationsToJson(project$populations),
    applications = .parameterGroupsToJson(project$applications),
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
    dataFile = .relativeFilename(project, "dataFile"),
    dataImporterConfigurationFile = .relativeFilename(project, "dataImporterConfigurationFile"),
    outputFolder = .relativePathOrNull(project, "outputFolder")
  )
}

#' @keywords internal
#' @noRd
.relativePathOrNull <- function(project, fieldName) {
  absPath <- suppressWarnings(project[[fieldName]])
  if (is.null(absPath)) return(NULL)
  if (is.null(project$projectDirPath)) return(absPath)
  fs::path_rel(absPath, project$projectDirPath)
}

#' @keywords internal
#' @noRd
.relativeFilename <- function(project, fieldName) {
  absPath <- suppressWarnings(project[[fieldName]])
  if (is.null(absPath)) return(NULL)
  basename(absPath)
}

#' @keywords internal
#' @noRd
.scenariosToJson <- function(scenarios, outputPaths) {
  if (is.null(scenarios) || length(scenarios) == 0) return(list())

  lapply(scenarios, function(sc) {
    outputPathIds <- NULL
    if (!is.null(sc$outputPaths) && length(sc$outputPaths) > 0 && !is.null(outputPaths)) {
      outputPathIds <- names(outputPaths)[match(sc$outputPaths, outputPaths)]
      outputPathIds <- outputPathIds[!is.na(outputPathIds)]
      if (length(outputPathIds) == 0) outputPathIds <- NULL
    }

    simTimeStr <- NULL
    if (!is.null(sc$simulationTime)) {
      intervals <- vapply(sc$simulationTime, function(int) {
        paste(int, collapse = ", ")
      }, character(1))
      simTimeStr <- paste(intervals, collapse = "; ")
    }

    list(
      name = sc$scenarioName,
      individualId = sc$individualId,
      populationId = if (sc$simulationType == "Population") sc$populationId else NULL,
      readPopulationFromCSV = sc$readPopulationFromCSV,
      modelParameterGroups = as.list(sc$parameterGroups),
      applicationProtocol = if (is.null(sc$applicationProtocol) || is.na(sc$applicationProtocol)) NULL else sc$applicationProtocol,
      simulationTime = simTimeStr,
      simulationTimeUnit = sc$simulationTimeUnit,
      steadyState = sc$simulateSteadyState,
      steadyStateTime = if (sc$simulateSteadyState && !is.null(sc$steadyStateTime)) {
        ospsuite::toUnit(
          quantityOrDimension = ospsuite::ospDimensions$Time,
          values = sc$steadyStateTime,
          targetUnit = sc$steadyStateTimeUnit %||% "min"
        )
      } else NULL,
      steadyStateTimeUnit = if (sc$simulateSteadyState) sc$steadyStateTimeUnit else NULL,
      overwriteFormulasInSS = sc$overwriteFormulasInSS,
      modelFile = sc$modelFile,
      outputPathIds = if (!is.null(outputPathIds)) as.list(outputPathIds) else NULL
    )
  })
}

#' @keywords internal
#' @noRd
.parameterGroupsToJson <- function(groups) {
  if (is.null(groups) || length(groups) == 0) return(list())

  result <- list()
  for (name in names(groups)) {
    group <- groups[[name]]
    entries <- list()
    for (i in seq_along(group$paths)) {
      pathParts <- strsplit(group$paths[i], "\\|")[[1]]
      containerPath <- paste(pathParts[-length(pathParts)], collapse = "|")
      parameterName <- pathParts[length(pathParts)]
      entries[[i]] <- list(
        containerPath = containerPath,
        parameterName = parameterName,
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
.individualsToJson <- function(individuals, parameterSetMapping) {
  if (is.null(individuals) || length(individuals) == 0) return(list())

  lapply(names(individuals), function(id) {
    indiv <- individuals[[id]]
    paramSets <- parameterSetMapping[[id]]
    list(
      individualId = id,
      species = indiv$species,
      population = indiv$population,
      gender = indiv$gender,
      weight = indiv$weight,
      height = indiv$height,
      age = indiv$age,
      proteinOntogenies = indiv$proteinOntogenies,
      parameterSets = if (!is.null(paramSets) && length(paramSets) > 0) as.list(paramSets) else NULL
    )
  })
}

#' @keywords internal
#' @noRd
.populationsToJson <- function(populations) {
  if (is.null(populations) || length(populations) == 0) return(list())

  lapply(names(populations), function(id) {
    pop <- populations[[id]]
    c(list(populationId = id), pop)
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
    dataCombined = .dataFrameToListOfLists(plots$dataCombined),
    plotConfiguration = .dataFrameToListOfLists(plots$plotConfiguration),
    plotGrids = .dataFrameToListOfLists(plots$plotGrids),
    exportConfiguration = .dataFrameToListOfLists(plots$exportConfiguration)
  )
}

#' @keywords internal
#' @noRd
.dataFrameToListOfLists <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(list())

  lapply(seq_len(nrow(df)), function(i) {
    row <- as.list(df[i, , drop = FALSE])
    row <- lapply(row, function(x) if (is.na(x)) NULL else x)
    row
  })
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
  hasConfigFile <- any(stringr::str_detect(
    "Project.*xlsx$",
    fs::dir_ls(destination)
  ))

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
initProject <- function(destination = ".", type = c("minimal", "example"), createExcel = TRUE, overwrite = FALSE) {
  destination <- fs::path_abs(destination)
  type <- match.arg(type)

  if (!fs::dir_exists(destination)) {
    stop(
      messages$pathNotFound(destination)
    )
  }

  source_folder <- switch(type,
    "minimal" = projectDirectory("Blank"),
    "example" = projectDirectory("Example")
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
    dir.create(file.path(destination, d), recursive = TRUE, showWarnings = FALSE)
  }

  if (createExcel) {
    jsonPath <- file.path(destination, "Project.json")
    pc <- loadProject(jsonPath)
    exportProjectToExcel(pc, outputDir = destination, silent = TRUE)
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
  file.path(projectDirectory("Example"), "Project.json")
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
