#' Import project configuration from Excel files to v2.0 JSON
#'
#' @description Reads all Excel configuration files in an esqlabsR project and
#' produces a single v2.0 JSON file. This is the migration path from
#' Excel-based projects to the JSON-primary workflow.
#'
#' @param projectConfigPath Path to the `ProjectConfiguration.xlsx` file.
#'   Defaults to `"ProjectConfiguration.xlsx"`.
#' @param outputDir Directory where the JSON file will be saved. If `NULL`
#'   (default), the JSON file is created in the same directory as the source
#'   Excel file.
#' @param silent Logical. If `TRUE`, suppresses informational messages.
#'   Defaults to `FALSE`.
#'
#' @return Invisibly returns the path to the created JSON file.
#' @export
importProjectConfigurationFromExcel <- function(
  projectConfigPath = "ProjectConfiguration.xlsx",
  outputDir = NULL,
  silent = FALSE
) {
  validateIsString(projectConfigPath)

  if (!file.exists(projectConfigPath)) {
    stop(messages$fileNotFound(projectConfigPath))
  }

  # Read the ProjectConfiguration.xlsx to get path settings
  pcExcel <- readExcel(projectConfigPath)
  pcDir <- dirname(fs::path_abs(projectConfigPath))

  # Build a lookup of Property -> Value from the Excel file
  pcProps <- stats::setNames(
    as.character(pcExcel$Value),
    as.character(pcExcel$Property)
  )

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
    normalizePath(file.path(configsFolder, fileName), mustWork = FALSE)
  }

  # Build the v2.0 JSON structure
  jsonData <- list(
    schemaVersion = "2.0",
    esqlabsRVersion = as.character(utils::packageVersion("esqlabsR"))
  )

  # projectConfiguration section — raw path properties
  jsonData$projectConfiguration <- as.list(pcProps)

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
      jsonData$scenarios <- .excelScenariosToV2(scenarioDf)
    }
  }

  # --- ModelParameters ---
  modelParamsFile <- resolveConfigFile(pcProps[["modelParamsFile"]])
  if (!is.null(modelParamsFile) && file.exists(modelParamsFile)) {
    jsonData$modelParameters <- .excelParameterSheetsToV2(modelParamsFile)
  }

  # --- Merge species parameters from SpeciesParameters.xlsx ---
  speciesParamsFile <- system.file(
    "extdata", "SpeciesParameters.xlsx", package = "esqlabsR"
  )
  if (file.exists(speciesParamsFile)) {
    speciesSheets <- readxl::excel_sheets(speciesParamsFile)
    speciesParams <- .excelParameterSheetsToV2(
      speciesParamsFile, sheetNames = speciesSheets
    )
    if (is.null(jsonData$modelParameters)) {
      jsonData$modelParameters <- speciesParams
    } else {
      # Only add species sheets that don't already exist (user overrides win)
      for (sheetName in names(speciesParams)) {
        if (is.null(jsonData$modelParameters[[sheetName]])) {
          jsonData$modelParameters[[sheetName]] <- speciesParams[[sheetName]]
        }
      }
    }
  }

  # --- Individuals ---
  individualsFile <- resolveConfigFile(pcProps[["individualsFile"]])
  if (!is.null(individualsFile) && file.exists(individualsFile)) {
    sheets <- readxl::excel_sheets(individualsFile)
    if ("IndividualBiometrics" %in% sheets) {
      indivDf <- readExcel(individualsFile, sheet = "IndividualBiometrics")
      jsonData$individuals <- .excelIndividualsToV2(indivDf)
    }
    # Individual parameter sets — all sheets except IndividualBiometrics
    paramSetSheets <- setdiff(sheets, "IndividualBiometrics")
    if (length(paramSetSheets) > 0) {
      jsonData$individualParameterSets <- .excelParameterSheetsToV2(
        individualsFile,
        sheetNames = paramSetSheets
      )
    }
  }

  # --- Populations ---
  populationsFile <- resolveConfigFile(pcProps[["populationsFile"]])
  if (!is.null(populationsFile) && file.exists(populationsFile)) {
    popDf <- readExcel(populationsFile, sheet = 1)
    jsonData$populations <- .excelPopulationsToV2(popDf)
  }

  # --- Applications ---
  applicationsFile <- resolveConfigFile(pcProps[["applicationsFile"]])
  if (!is.null(applicationsFile) && file.exists(applicationsFile)) {
    jsonData$applications <- .excelParameterSheetsToV2(applicationsFile)
  }

  # --- Plots ---
  plotsFile <- resolveConfigFile(pcProps[["plotsFile"]])
  if (!is.null(plotsFile) && file.exists(plotsFile)) {
    jsonData$plots <- .excelPlotsToV2(plotsFile)
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

#' @rdname importProjectConfigurationFromExcel
#' @param ... Arguments passed to `importProjectConfigurationFromExcel()`.
#' @export
snapshotProjectConfiguration <- function(...) {
  lifecycle::deprecate_soft(
    what = "snapshotProjectConfiguration()",
    with = "importProjectConfigurationFromExcel()",
    when = "6.0.0"
  )
  importProjectConfigurationFromExcel(...)
}

#' Export a ProjectConfiguration to Excel files
#'
#' @description Writes Excel configuration files from a `ProjectConfiguration`
#' object (typically loaded from JSON). This is the reverse of
#' `importProjectConfigurationFromExcel()`.
#'
#' @param projectConfiguration A `ProjectConfiguration` object.
#' @param outputDir Directory where the Excel files will be created. Defaults
#'   to the directory of the source JSON file.
#' @param silent Logical. If `TRUE`, suppresses informational messages.
#'   Defaults to `FALSE`.
#'
#' @return Invisibly returns the path to the created
#'   `ProjectConfiguration.xlsx`.
#' @export
exportProjectConfigurationToExcel <- function(
  projectConfiguration,
  outputDir = NULL,
  silent = FALSE
) {
  validateIsOfType(projectConfiguration, "ProjectConfiguration")

  if (is.null(outputDir)) {
    outputDir <- projectConfiguration$projectConfigurationDirPath %||% "."
  }

  if (!dir.exists(outputDir)) {
    dir.create(outputDir, recursive = TRUE)
  }

  configDir <- file.path(outputDir, "Configurations")
  if (!dir.exists(configDir)) {
    dir.create(configDir, recursive = TRUE, showWarnings = FALSE)
  }

  pc <- projectConfiguration

  # --- ProjectConfiguration.xlsx ---
  pcData <- list(
    modelFolder = "modelFolder",
    configurationsFolder = "configurationsFolder",
    modelParamsFile = "modelParamsFile",
    individualsFile = "individualsFile",
    populationsFile = "populationsFile",
    populationsFolder = "populationsFolder",
    scenariosFile = "scenariosFile",
    applicationsFile = "applicationsFile",
    plotsFile = "plotsFile",
    dataFolder = "dataFolder",
    dataFile = "dataFile",
    dataImporterConfigurationFile = "dataImporterConfigurationFile",
    outputFolder = "outputFolder"
  )

  # Build a data.frame from the raw stored values
  props <- character(0)
  vals <- character(0)
  descs <- character(0)
  pcInternal <- .extractProjectConfigurationData(pc)
  for (propName in names(pcInternal)) {
    props <- c(props, propName)
    vals <- c(vals, pcInternal[[propName]]$value %||% "")
    descs <- c(descs, pcInternal[[propName]]$description %||% "")
  }
  projConfigDf <- data.frame(
    Property = props,
    Value = vals,
    Description = descs,
    stringsAsFactors = FALSE
  )
  projConfigPath <- file.path(outputDir, "ProjectConfiguration.xlsx")
  .writeExcel(projConfigDf, projConfigPath)

  # --- ModelParameters.xlsx ---
  if (!is.null(pc$modelParameters) && length(pc$modelParameters) > 0) {
    sheets <- .parameterStructuresToExcelSheets(pc$modelParameters)
    .writeExcel(sheets, file.path(configDir, "ModelParameters.xlsx"))
  }

  # --- Individuals.xlsx ---
  indivSheets <- list()
  if (!is.null(pc$individuals) && length(pc$individuals) > 0) {
    indivSheets[["IndividualBiometrics"]] <- .individualsToExcelDf(
      pc$individuals,
      pc$individualParameterSetMapping
    )
  }
  if (
    !is.null(pc$individualParameterSets) &&
      length(pc$individualParameterSets) > 0
  ) {
    paramSheets <- .parameterStructuresToExcelSheets(
      pc$individualParameterSets
    )
    indivSheets <- c(indivSheets, paramSheets)
  }
  if (length(indivSheets) > 0) {
    .writeExcel(indivSheets, file.path(configDir, "Individuals.xlsx"))
  }

  # --- Populations.xlsx ---
  if (!is.null(pc$populations) && length(pc$populations) > 0) {
    popDf <- .populationsToExcelDf(pc$populations)
    .writeExcel(popDf, file.path(configDir, "Populations.xlsx"))
  }

  # --- Scenarios.xlsx ---
  scenSheets <- list()
  if (
    !is.null(pc$scenarios) &&
      length(pc$scenarios) > 0
  ) {
    scenSheets[["Scenarios"]] <- .scenarioConfigurationsToExcelDf(
      pc$scenarios,
      outputPaths = pc$outputPaths
    )
  }
  if (!is.null(pc$outputPaths) && length(pc$outputPaths) > 0) {
    scenSheets[["OutputPaths"]] <- data.frame(
      OutputPathId = names(pc$outputPaths),
      OutputPath = unname(pc$outputPaths),
      stringsAsFactors = FALSE
    )
  }
  if (length(scenSheets) > 0) {
    .writeExcel(scenSheets, file.path(configDir, "Scenarios.xlsx"))
  }

  # --- Applications.xlsx ---
  if (!is.null(pc$applications) && length(pc$applications) > 0) {
    appSheets <- .parameterStructuresToExcelSheets(pc$applications)
    .writeExcel(appSheets, file.path(configDir, "Applications.xlsx"))
  }

  # --- Plots.xlsx ---
  if (!is.null(pc$plots)) {
    plotSheets <- list()
    for (sheetName in names(pc$plots)) {
      df <- pc$plots[[sheetName]]
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
      pc$jsonPath %||% "ProjectConfiguration",
      relPath
    ))
  }

  invisible(projConfigPath)
}

#' @rdname exportProjectConfigurationToExcel
#' @param jsonPath Path to the JSON configuration file. Defaults to
#'   `"ProjectConfiguration.json"`.
#' @param ... Additional arguments (unused).
#' @export
restoreProjectConfiguration <- function(
  jsonPath = "ProjectConfiguration.json",
  outputDir = NULL,
  silent = FALSE,
  ...
) {
  lifecycle::deprecate_soft(
    what = "restoreProjectConfiguration()",
    with = "exportProjectConfigurationToExcel()",
    when = "6.0.0"
  )
  pc <- loadProject(jsonPath)
  exportProjectConfigurationToExcel(
    projectConfiguration = pc,
    outputDir = outputDir,
    silent = silent
  )
  invisible(pc)
}

#' Check if Excel configuration files are in sync with JSON
#'
#' @description Compares Excel configuration files against their JSON
#' configuration to determine if they are synchronized.
#'
#' @param projectConfigPath Path to a `ProjectConfiguration.xlsx` file.
#'   Defaults to `"ProjectConfiguration.xlsx"`.
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
projectConfigurationStatus <- function(
  projectConfigPath = "ProjectConfiguration.xlsx",
  jsonPath = NULL,
  silent = FALSE
) {
  # Accept either a path string or a ProjectConfiguration object for
  # backwards compatibility
  if (inherits(projectConfigPath, "ProjectConfiguration")) {
    pcObj <- projectConfigPath
    projectConfigPath <- pcObj$projectConfigurationFilePath
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
  importProjectConfigurationFromExcel(
    projectConfigPath,
    outputDir = tempDir,
    silent = TRUE
  )

  # Load both JSON files for comparison
  originalJson <- readLines(jsonPath, warn = FALSE)
  currentJson <- readLines(tempJsonPath, warn = FALSE)

  if (identical(originalJson, currentJson)) {
    result <- list(
      in_sync = TRUE,
      details = list(),
      unsaved_changes = FALSE
    )
    if (!silent) {
      message(messages$excelInSync())
    }
  } else {
    originalJsonObj <- jsonlite::fromJSON(jsonPath, simplifyVector = FALSE)
    currentJsonObj <- jsonlite::fromJSON(tempJsonPath, simplifyVector = FALSE)

    fileChanges <- list()
    sheetChanges <- list()
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
      if (
        !identical(originalJsonObj[[file]], currentJsonObj[[file]])
      ) {
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
        "{.run importProjectConfigurationFromExcel()} - Update JSON from Excel files."
      )
      cli::cli_li(
        "{.run exportProjectConfigurationToExcel()} - Recreate Excel files from JSON."
      )
      cli::cli_end()
    }
  }

  invisible(result)
}

# ===========================================================================
# v2.0 JSON conversion helpers — Excel → JSON
# ===========================================================================

#' Convert parameter Excel sheets to v2.0 JSON format
#' @param filePath Path to the Excel file
#' @param sheetNames Sheets to read. If NULL, reads all sheets.
#' @returns Named list of parameter arrays
#' @keywords internal
#' @noRd
.excelParameterSheetsToV2 <- function(filePath, sheetNames = NULL) {
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
          units = if (
            is.na(df[["Units"]][[i]]) || df[["Units"]][[i]] == ""
          ) {
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

#' Convert Scenarios Excel sheet to v2.0 JSON format
#' @param scenarioDf Data frame from the Scenarios sheet
#' @returns List of scenario objects
#' @keywords internal
#' @noRd
.excelScenariosToV2 <- function(scenarioDf) {
  scenarios <- list()
  for (i in seq_len(nrow(scenarioDf))) {
    row <- scenarioDf[i, ]
    scenario <- list(
      name = as.character(row$Scenario_name),
      individualId = .naToNull(as.character(row$IndividualId)),
      populationId = .naToNull(as.character(row$PopulationId)),
      readPopulationFromCSV = .naToNull(as.logical(row$ReadPopulationFromCSV)),
      modelParameterGroups = .parseCommaListToArray(row$ModelParameterSheets),
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

#' Convert IndividualBiometrics Excel sheet to v2.0 JSON format
#' @param indivDf Data frame from the IndividualBiometrics sheet
#' @returns List of individual objects
#' @keywords internal
#' @noRd
.excelIndividualsToV2 <- function(indivDf) {
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
    # Individual Parameter Sets column — if present
    if ("Individual Parameter Sets" %in% names(indivDf)) {
      indiv$parameterSets <- .parseCommaListToArray(
        row$`Individual Parameter Sets`
      )
    }
    individuals[[i]] <- indiv
  }
  individuals
}

#' Convert Populations Excel sheet to v2.0 JSON format
#' @param popDf Data frame from the Demographics sheet
#' @returns List of population objects
#' @keywords internal
#' @noRd
.excelPopulationsToV2 <- function(popDf) {
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

#' Convert Plots Excel file to v2.0 JSON format
#' @param plotsFile Path to the Plots.xlsx file
#' @returns Named list with dataCombined, plotConfiguration, plotGrids,
#'   exportConfiguration
#' @keywords internal
#' @noRd
.excelPlotsToV2 <- function(plotsFile) {
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

# ===========================================================================
# v2.0 JSON conversion helpers — JSON → Excel (for export)
# ===========================================================================

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
#' @param parameterSetMapping Named list mapping individualId to parameter set
#'   names
#' @returns A data frame
#' @keywords internal
#' @noRd
.individualsToExcelDf <- function(individuals, parameterSetMapping = NULL) {
  rows <- list()
  for (indivId in names(individuals)) {
    ic <- individuals[[indivId]]
    paramSets <- parameterSetMapping[[indivId]]
    paramSetsStr <- if (!is.null(paramSets) && length(paramSets) > 0) {
      paste(paramSets, collapse = ", ")
    } else {
      NA
    }
    ontoStr <- ic$proteinOntogenies %||% NA
    if (is.na(ontoStr)) ontoStr <- NA

    rows[[length(rows) + 1]] <- data.frame(
      IndividualId = indivId,
      Species = as.character(ic$species),
      Population = as.character(ic$population %||% NA),
      Gender = as.character(ic$gender),
      `Weight [kg]` = as.double(ic$weight %||% NA),
      `Height [cm]` = as.double(ic$height %||% NA),
      `Age [year(s)]` = as.double(ic$age %||% NA),
      `Protein Ontogenies` = ontoStr,
      `Individual Parameter Sets` = paramSetsStr,
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
    if (is.na(ontoStr)) ontoStr <- NA

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
#'   values are path strings) from `ProjectConfiguration$outputPaths`.
#'   Used to reverse-lookup scenario output paths back to IDs.
#' @returns A data frame
#' @keywords internal
#' @noRd
.scenarioConfigurationsToExcelDf <- function(scenarioConfigs, outputPaths = NULL) {
  rows <- list()
  for (name in names(scenarioConfigs)) {
    sc <- scenarioConfigs[[name]]
    paramGroupNames <- sc$parameterGroups
    paramGroupsStr <- if (!is.null(paramGroupNames) && length(paramGroupNames) > 0) {
      paste(paramGroupNames, collapse = ", ")
    } else {
      NA
    }
    # simulationTime → string representation
    simTimeStr <- NA
    if (!is.null(sc$simulationTime)) {
      intervals <- vapply(sc$simulationTime, function(interval) {
        paste(interval, collapse = ", ")
      }, character(1))
      simTimeStr <- paste(intervals, collapse = "; ")
    }
    # outputPaths → reverse-lookup IDs from pc$outputPaths
    outputPathIdsStr <- NA
    if (!is.null(sc$outputPaths) && !is.null(outputPaths)) {
      matchedIds <- names(outputPaths)[match(sc$outputPaths, outputPaths)]
      matchedIds <- matchedIds[!is.na(matchedIds)]
      if (length(matchedIds) > 0) {
        outputPathIdsStr <- paste(matchedIds, collapse = ", ")
      }
    }

    # Reconstruct steadyStateTime back to the unit
    ssTime <- NA
    ssTimeUnit <- NA
    if (
      !is.null(sc$steadyStateTime) && !is.na(sc$steadyStateTime) &&
        sc$steadyStateTime > 0
    ) {
      # steadyStateTime is stored in base units (min), convert back
      ssTimeUnit <- "min"
      ssTime <- sc$steadyStateTime
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

#' Extract private .projectConfigurationData from a ProjectConfiguration
#' @param pc ProjectConfiguration object
#' @returns Named list of property data
#' @keywords internal
#' @noRd
.extractProjectConfigurationData <- function(pc) {
  # Access via the environment since .projectConfigurationData is private
  pcEnv <- pc$.__enclos_env__$private
  pcEnv$.projectConfigurationData
}

# ===========================================================================
# Generic helpers
# ===========================================================================

#' Extract a numeric value from an R6 SnapshotParameter or plain value
#' @keywords internal
#' @noRd
.extractNumericValue <- function(x) {
  if (is.null(x)) return(NA_real_)
  if (R6::is.R6(x) && !is.null(x$value)) return(as.numeric(x$value))
  as.numeric(x)
}

#' Extract a character value from an R6 object or plain value
#' @keywords internal
#' @noRd
.extractCharValue <- function(x) {
  if (is.null(x)) return(NA_character_)
  if (R6::is.R6(x)) {
    if (!is.null(x$displayName)) return(as.character(x$displayName))
    if (!is.null(x$value)) return(as.character(x$value))
  }
  as.character(x)
}

#' Convert NA to NULL for JSON serialization
#' @keywords internal
#' @noRd
.naToNull <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return(NULL)
  }
  if (is.na(x)) {
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
  trimws(scan(
    text = as.character(x),
    what = "character",
    sep = ",",
    quiet = TRUE
  ))
}
