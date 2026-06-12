# Excel <-> JSON bridge: public API ----

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
    cli::cli_abort(messages$fileNotFound(projectConfigPath))
  }

  # Read the Project.xlsx to get path settings
  pcExcel <- readExcel(projectConfigPath)
  pcDir <- dirname(fs::path_abs(projectConfigPath))

  # Build a lookup of Property -> Value from the Excel file
  pcProps <- stats::setNames(
    as.character(pcExcel$Value),
    as.character(pcExcel$Property)
  )

  # NULL-safe property lookup (single-bracket "[" returns NA when key absent
  # in named character; collapse that to NULL).
  prop <- function(name) {
    if (!(name %in% names(pcProps))) {
      return(NULL)
    }
    val <- pcProps[[name]]
    if (length(val) == 0 || is.na(val)) NULL else val
  }

  # Read version metadata (with fallback for old Excel files)
  schemaVersion <- prop("schemaVersion") %||% "2.0"

  # Remove version metadata from file path properties
  pcProps <- pcProps[!names(pcProps) %in% c("schemaVersion", "esqlabsRVersion")]

  # Resolve the configurations folder relative to the Excel file
  configsFolder <- prop("configurationsFolder")
  if (!is.null(configsFolder)) {
    if (!fs::is_absolute_path(configsFolder)) {
      configsFolder <- file.path(pcDir, configsFolder)
    }
    configsFolder <- normalizePath(configsFolder, mustWork = FALSE)
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

  # Default config filenames for sections whose path property is omitted
  # from Project.xlsx (e.g. exports of programmatic projects that never
  # set a custom path).
  defaultConfigFile <- list(
    modelParamsFile = "ModelParameters.xlsx",
    individualsFile = "Individuals.xlsx",
    populationsFile = "Populations.xlsx",
    scenariosFile = "Scenarios.xlsx",
    applicationsFile = "Applications.xlsx",
    plotsFile = "Plots.xlsx"
  )
  # Property lookup with default-filename fallback.
  propOrDefault <- function(name) {
    prop(name) %||% defaultConfigFile[[name]]
  }

  # Build the JSON structure -- schemaVersion comes from the Excel source;
  # if the Excel predates versioning, default to "2.0".
  jsonData <- list(
    schemaVersion = schemaVersion,
    esqlabsRVersion = as.character(utils::packageVersion("esqlabsR"))
  )

  # filePaths section -- raw path properties
  jsonData$filePaths <- as.list(pcProps)

  # --- OutputPaths ---
  scenariosFile <- resolveConfigFile(propOrDefault("scenariosFile"))
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
      jsonData$scenarios <- .parseExcelScenarios(scenarioDf)
    }
  }

  # --- ModelParameterSets ---
  modelParamsFile <- resolveConfigFile(propOrDefault("modelParamsFile"))
  if (!is.null(modelParamsFile) && file.exists(modelParamsFile)) {
    jsonData$modelParameterSets <- .parseExcelParameterSheets(
      modelParamsFile
    )
  }

  # --- Individuals ---
  individualsFile <- resolveConfigFile(propOrDefault("individualsFile"))
  if (!is.null(individualsFile) && file.exists(individualsFile)) {
    sheets <- readxl::excel_sheets(individualsFile)
    if ("IndividualBiometrics" %in% sheets) {
      indivDf <- readExcel(individualsFile, sheet = "IndividualBiometrics")
      jsonData$individuals <- .parseExcelIndividuals(indivDf)
    }
    # Non-biometrics sheets are individual parameter sets, keyed by sheet name.
    paramSheetNames <- setdiff(sheets, "IndividualBiometrics")
    if (length(paramSheetNames) > 0) {
      paramSets <- .parseExcelParameterSheets(
        individualsFile,
        sheetNames = paramSheetNames
      )
      jsonData$individualParameterSets <- paramSets
    }
  }

  # --- Populations ---
  populationsFile <- resolveConfigFile(propOrDefault("populationsFile"))
  if (!is.null(populationsFile) && file.exists(populationsFile)) {
    popDf <- readExcel(populationsFile, sheet = 1)
    jsonData$populations <- .parseExcelPopulations(popDf)
  }

  # --- Applications ---
  applicationsFile <- resolveConfigFile(propOrDefault("applicationsFile"))
  if (!is.null(applicationsFile) && file.exists(applicationsFile)) {
    sheets <- readxl::excel_sheets(applicationsFile)
    appsObj <- list()
    if ("ApplicationProtocols" %in% sheets) {
      appsDf <- readExcel(applicationsFile, sheet = "ApplicationProtocols")
      hasParameterSets <- "ParameterSets" %in% names(appsDf)
      for (i in seq_len(nrow(appsDf))) {
        id <- as.character(appsDf$ApplicationId[i])
        appEntry <- list()
        if (hasParameterSets) {
          raw <- appsDf$ParameterSets[i]
          if (!is.null(raw) && !is.na(raw) && nchar(as.character(raw)) > 0) {
            appEntry$parameterSets <- as.list(
              .parseCommaListToArray(as.character(raw))
            )
          }
        }
        appsObj[[id]] <- appEntry
      }
    }
    if (length(appsObj) > 0) {
      jsonData$applications <- appsObj
    }
    paramSheetNames <- setdiff(sheets, "ApplicationProtocols")
    if (length(paramSheetNames) > 0) {
      paramSets <- .parseExcelParameterSheets(
        applicationsFile,
        sheetNames = paramSheetNames
      )
      jsonData$applicationParameterSets <- paramSets
    }
  }

  # --- Plots ---
  plotsFile <- resolveConfigFile(propOrDefault("plotsFile"))
  if (!is.null(plotsFile) && file.exists(plotsFile)) {
    jsonData$plots <- .parseExcelPlots(plotsFile)
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
  lifecycle::deprecate_warn(
    "6.0.0",
    "snapshotProjectConfiguration()",
    "importProjectFromExcel()"
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
    !is.null(project$modelParameterSets) &&
      length(project$modelParameterSets) > 0
  ) {
    sheets <- .parameterStructuresToExcelSheets(project$modelParameterSets)
    .writeExcel(sheets, file.path(configDir, "ModelParameters.xlsx"))
  }

  # --- Individuals.xlsx ---
  indivSheets <- list()
  if (!is.null(project$individuals) && length(project$individuals) > 0) {
    indivSheets[["IndividualBiometrics"]] <- .individualsToExcelDf(
      project$individuals
    )
  }
  if (
    !is.null(project$individualParameterSets) &&
      length(project$individualParameterSets) > 0
  ) {
    paramSheets <- .parameterStructuresToExcelSheets(
      project$individualParameterSets
    )
    indivSheets <- c(indivSheets, paramSheets)
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
      OutputPath = unlist(project$outputPaths, use.names = FALSE),
      stringsAsFactors = FALSE
    )
  }
  if (length(scenSheets) > 0) {
    .writeExcel(scenSheets, file.path(configDir, "Scenarios.xlsx"))
  }

  # --- Applications.xlsx ---
  appSheets <- list()
  if (!is.null(project$applications) && length(project$applications) > 0) {
    appSheets[["ApplicationProtocols"]] <- .applicationsToExcelDf(
      project$applications
    )
  }
  if (
    !is.null(project$applicationParameterSets) &&
      length(project$applicationParameterSets) > 0
  ) {
    paramSheets <- .parameterStructuresToExcelSheets(
      project$applicationParameterSets
    )
    appSheets <- c(appSheets, paramSheets)
  }
  if (length(appSheets) > 0) {
    .writeExcel(appSheets, file.path(configDir, "Applications.xlsx"))
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
    message(messages$restoredProjectConfiguration(
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
  lifecycle::deprecate_warn(
    "6.0.0",
    "restoreProjectConfiguration()",
    "exportProjectToExcel()"
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
  silent = FALSE
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
    cli::cli_abort(messages$fileNotFound(projectConfigPath))
  }

  # Determine JSON path if not provided
  if (is.null(jsonPath)) {
    jsonPath <- sub("\\.xlsx$", ".json", projectConfigPath)
  }

  if (!file.exists(jsonPath)) {
    cli::cli_abort("JSON file does not exist: {.path {jsonPath}}")
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

  # Remove esqlabsRVersion -- it changes with package updates and would cause
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
      cli::cli_warn(messages$excelNotInSync())

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
  lifecycle::deprecate_warn(
    "6.0.0",
    "projectConfigurationStatus()",
    "projectStatus()"
  )
  projectStatus(...)
}

# Excel <-> JSON bridge: sync helper ----

#' Sync-status helper called by `Project$sync()`
#'
#' @param project A `Project` object.
#' @param silent Logical. If `TRUE`, suppresses informational messages.
#' @returns Invisibly returns a named list with sync status components.
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

# Excel <-> JSON bridge: internal helpers ----

#' Parse parameter sheets from an Excel file into JSON structure
#' @param filePath Path to the Excel file
#' @param sheetNames Sheets to read. If NULL, reads all sheets.
#' @returns Named list of parameter arrays
#' @keywords internal
#' @noRd
.parseExcelParameterSheets <- function(
  filePath,
  sheetNames = NULL
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
#' @returns List of scenario objects
#' @keywords internal
#' @noRd
.parseExcelScenarios <- function(scenarioDf) {
  scenarios <- list()
  for (i in seq_len(nrow(scenarioDf))) {
    row <- scenarioDf[i, ]
    scenario <- list(
      name = as.character(row$Scenario_name),
      individualId = .naToNull(as.character(row$IndividualId)),
      populationId = .naToNull(as.character(row$PopulationId)),
      readPopulationFromCSV = .naToNull(as.logical(row$ReadPopulationFromCSV)),
      modelParameterSets = .parseCommaListToArray(row$ModelParameterSheets),
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
#' @returns List of individual objects
#' @keywords internal
#' @noRd
.parseExcelIndividuals <- function(indivDf) {
  individuals <- list()
  hasParameterSets <- "ParameterSets" %in% names(indivDf)
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
    if (hasParameterSets) {
      raw <- row$ParameterSets
      if (!is.null(raw) && !is.na(raw) && nchar(as.character(raw)) > 0) {
        indiv$parameterSets <- as.list(
          .parseCommaListToArray(as.character(raw))
        )
      }
    }
    individuals[[i]] <- indiv
  }
  individuals
}

#' Parse Populations Excel sheet into JSON structure
#' @param popDf Data frame from the Demographics sheet
#' @returns List of population objects
#' @keywords internal
#' @noRd
.parseExcelPopulations <- function(popDf) {
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
#' @returns Named list with dataCombined, plotConfiguration, plotGrids
#' @keywords internal
#' @noRd
.parseExcelPlots <- function(plotsFile) {
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
#' @param parameterSets Named list of parameter structures (paths, values,
#'   units)
#' @returns Named list of data frames suitable for Excel sheets
#' @keywords internal
#' @noRd
.parameterStructuresToExcelSheets <- function(parameterSets) {
  sheets <- list()
  for (name in names(parameterSets)) {
    params <- parameterSets[[name]]
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
    pSetsStr <- .formatArrayToCommaList(ic$parameterSets)

    rows[[length(rows) + 1]] <- data.frame(
      IndividualId = indivId,
      Species = as.character(ic$species),
      Population = as.character(ic$population %||% NA),
      Gender = as.character(ic$gender),
      `Weight [kg]` = as.double(ic$weight %||% NA),
      `Height [cm]` = as.double(ic$height %||% NA),
      `Age [year(s)]` = as.double(ic$age %||% NA),
      `Protein Ontogenies` = ontoStr,
      ParameterSets = pSetsStr,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  }
  do.call(rbind, rows)
}

#' Convert applications data to an ApplicationProtocols data frame
#' @param applications Named list of application records
#' @returns A data frame with one row per application
#' @keywords internal
#' @noRd
.applicationsToExcelDf <- function(applications) {
  rows <- list()
  for (appId in names(applications)) {
    ac <- applications[[appId]]
    pSetsStr <- .formatArrayToCommaList(ac$parameterSets)

    rows[[length(rows) + 1]] <- data.frame(
      ApplicationId = appId,
      ParameterSets = pSetsStr,
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
    paramSetsStr <- .formatArrayToCommaList(sc$modelParameterSets)
    # simulationTime -> string representation
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
    # outputPaths -> reverse-lookup IDs from project$outputPaths
    outputPathIdsStr <- NA
    if (!is.null(sc$outputPaths) && !is.null(outputPaths)) {
      matchedIds <- names(outputPaths)[match(sc$outputPaths, outputPaths)]
      matchedIds <- matchedIds[!is.na(matchedIds)]
      if (length(matchedIds) > 0) {
        outputPathIdsStr <- .formatArrayToCommaList(matchedIds)
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
      ModelParameterSheets = paramSetsStr,
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

#' Format a character vector as a comma-separated Excel-bridge cell
#'
#' Identifiers containing commas (or backslashes) are escaped so that
#' `.parseCommaListToArray()` reverses cleanly. The escape rule:
#' `\\` for a literal backslash, `\,` for a literal comma. List items
#' are joined with `", "`.
#'
#' @param x Character vector.
#' @returns A length-1 string, or `NA_character_` if `x` is empty.
#' @keywords internal
#' @noRd
.formatArrayToCommaList <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return(NA_character_)
  }
  escaped <- gsub("\\", "\\\\", x, fixed = TRUE)
  escaped <- gsub(",", "\\,", escaped, fixed = TRUE)
  paste(escaped, collapse = ", ")
}

#' Parse a comma-separated string into a character vector, or NULL
#'
#' Honors `\\` as a literal backslash and `\,` as a literal comma so
#' that ids written via `.formatArrayToCommaList()` round-trip even
#' when they contain commas.
#'
#' @keywords internal
#' @noRd
.parseCommaListToArray <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x) || x == "") {
    return(NULL)
  }
  raw <- as.character(x)
  # Walk the string character by character: track whether the previous
  # character was an unescaped backslash (escape state). Split on
  # unescaped commas; collapse `\\` to `\` and `\,` to `,`.
  chars <- strsplit(raw, "", fixed = TRUE)[[1]]
  parts <- character()
  current <- ""
  escape <- FALSE
  for (ch in chars) {
    if (escape) {
      current <- paste0(current, ch)
      escape <- FALSE
    } else if (ch == "\\") {
      escape <- TRUE
    } else if (ch == ",") {
      parts <- c(parts, current)
      current <- ""
    } else {
      current <- paste0(current, ch)
    }
  }
  parts <- c(parts, current)
  parts <- trimws(parts)
  parts[nzchar(parts)]
}
