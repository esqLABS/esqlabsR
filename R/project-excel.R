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
