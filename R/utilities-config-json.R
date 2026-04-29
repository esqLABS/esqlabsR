# Public API ----

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

#' @rdname importProjectFromExcel
#' @export
importProjectConfigurationFromExcel <- function(...) {
  lifecycle::deprecate_soft(
    what = "importProjectConfigurationFromExcel()",
    with = "importProjectFromExcel()",
    when = "7.0.0"
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
  if (!is.null(project$modelParameters) && length(project$modelParameters) > 0) {
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

#' @rdname exportProjectToExcel
#' @export
exportProjectConfigurationToExcel <- function(...) {
  lifecycle::deprecate_soft(
    what = "exportProjectConfigurationToExcel()",
    with = "exportProjectToExcel()",
    when = "7.0.0"
  )
  exportProjectToExcel(...)
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
    when = "7.0.0"
  )
  projectStatus(...)
}

# Excel → JSON conversion helpers ----

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

# JSON → Excel conversion helpers (for export) ----

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

# Generic helpers ----

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
