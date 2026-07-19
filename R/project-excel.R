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
#' @family project persistence
importProjectFromExcel <- function(
  projectConfigPath = "Project.xlsx",
  outputDir = NULL,
  silent = FALSE
) {
  validateIsString(projectConfigPath)

  if (!file.exists(projectConfigPath)) {
    cli::cli_abort(messages$fileNotFound(projectConfigPath))
  }

  # Read the Project.xlsx to get path settings. A corrupt or empty file is not
  # a valid Excel workbook, so `readxl` raises a raw "zip file cannot be opened"
  # error that names nothing useful; wrap it in a clear message naming the path.
  pcExcel <- tryCatch(
    readExcel(projectConfigPath),
    error = function(e) {
      cli::cli_abort(
        c(
          "{.path {projectConfigPath}} is not a readable Excel project file.",
          "i" = "It must be a valid {.field .xlsx} workbook \\
          (the project's {.file Project.xlsx})."
        ),
        parent = e
      )
    }
  )
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

  # Read container metadata. `name` / `description` are top-level container
  # fields written by `exportProjectToExcel()`; read them back here so the
  # round trip restores them. An absent row (an old Excel file) or an
  # empty-string row (a project that carried no name/description on export)
  # both resolve to NULL, so a nameless project does not gain an empty name.
  emptyToNull <- function(x) if (is.null(x) || !nzchar(x)) NULL else x
  projectName <- emptyToNull(prop("name"))
  projectDescription <- emptyToNull(prop("description"))

  # Remove version and container metadata from file path properties
  pcProps <- pcProps[
    !names(pcProps) %in%
      c("schemaVersion", "esqlabsRVersion", "name", "description")
  ]

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
    plotsFile = "Plots.xlsx",
    parameterIdentificationFile = "ParameterIdentification.xlsx",
    initialConditionsFile = "InitialConditions.xlsx"
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
  # Carry the container metadata through only when present, so an old Excel
  # file (no `name` / `description` rows) yields a project without them rather
  # than null-valued fields.
  if (!is.null(projectName)) {
    jsonData$name <- projectName
  }
  if (!is.null(projectDescription)) {
    jsonData$description <- projectDescription
  }

  # Path properties from Project.xlsx split into the two container blocks: the
  # four live working folders (`filePaths`) and the seven Excel-bridge sheet
  # names (`excel`). Any other property is treated as a live working folder.
  pathProps <- as.list(pcProps)
  excelProps <- pathProps[names(pathProps) %in% .excelFilePathFields]
  filePathProps <- pathProps[!(names(pathProps) %in% .excelFilePathFields)]
  jsonData$filePaths <- filePathProps
  if (length(excelProps) > 0L) {
    jsonData$excel <- excelProps
  }

  # The unified `parameterSets` section is accumulated across several sources
  # (the model-parameters workbook, plus the non-primary sheets of the
  # individuals and applications workbooks). Seed it here so the section
  # descriptors below can append to it. An id defined in more than one source
  # is a collision that aborts the eventual load (`.mergeParameterSetSections`).
  jsonData$parameterSets <- list()

  # Each import section is described by the config-file property that locates
  # its workbook and a `parse(file, jsonData)` closure that reads that workbook
  # and returns the updated `jsonData`. One loop below resolves each property,
  # skips a section whose workbook is absent, and applies its closure, so every
  # section shares one existence guard rather than repeating it. The closures
  # keep each section's own (heterogeneous) sheet handling explicit.
  sections <- list(
    # OutputPaths and Scenarios both live in the scenarios workbook.
    list(
      property = "scenariosFile",
      parse = function(file, jsonData) {
        sheets <- readxl::excel_sheets(file)
        if ("OutputPaths" %in% sheets) {
          outputPathsDf <- readExcel(file, sheet = "OutputPaths")
          outputPaths <- stats::setNames(
            as.character(outputPathsDf$OutputPath),
            as.character(outputPathsDf$OutputPathId)
          )
          jsonData$outputPaths <- as.list(outputPaths)
        }
        if ("Scenarios" %in% sheets) {
          scenarioDf <- readExcel(file, sheet = "Scenarios")
          scenarioDf <- dplyr::filter(scenarioDf, !is.na(Scenario_name))
          jsonData$scenarios <- .parseExcelScenarios(scenarioDf)
        }
        jsonData
      }
    ),
    # Model parameters: every sheet is a parameter set.
    list(
      property = "modelParamsFile",
      parse = function(file, jsonData) {
        jsonData$parameterSets <- c(
          jsonData$parameterSets,
          .parseExcelParameterSheets(file)
        )
        jsonData
      }
    ),
    # Individuals: the biometrics sheet is the individuals section; every other
    # sheet is a parameter set keyed by sheet name.
    list(
      property = "individualsFile",
      parse = function(file, jsonData) {
        sheets <- readxl::excel_sheets(file)
        if ("IndividualBiometrics" %in% sheets) {
          indivDf <- readExcel(file, sheet = "IndividualBiometrics")
          jsonData$individuals <- .parseExcelIndividuals(indivDf)
        }
        paramSheetNames <- setdiff(sheets, "IndividualBiometrics")
        if (length(paramSheetNames) > 0) {
          jsonData$parameterSets <- c(
            jsonData$parameterSets,
            .parseExcelParameterSheets(file, sheetNames = paramSheetNames)
          )
        }
        jsonData
      }
    ),
    list(
      property = "populationsFile",
      parse = function(file, jsonData) {
        popDf <- readExcel(file, sheet = 1)
        jsonData$populations <- .parseExcelPopulations(popDf)
        jsonData
      }
    ),
    # Applications: the protocols sheet is the applications section; every other
    # sheet is a parameter set keyed by sheet name.
    list(
      property = "applicationsFile",
      parse = function(file, jsonData) {
        sheets <- readxl::excel_sheets(file)
        if ("ApplicationProtocols" %in% sheets) {
          appsDf <- readExcel(file, sheet = "ApplicationProtocols")
          appsObj <- .parseExcelApplications(appsDf)
          if (length(appsObj) > 0) {
            jsonData$applications <- appsObj
          }
        }
        paramSheetNames <- setdiff(sheets, "ApplicationProtocols")
        if (length(paramSheetNames) > 0) {
          jsonData$parameterSets <- c(
            jsonData$parameterSets,
            .parseExcelParameterSheets(file, sheetNames = paramSheetNames)
          )
        }
        jsonData
      }
    ),
    list(
      property = "initialConditionsFile",
      parse = function(file, jsonData) {
        jsonData$initialConditions <- .parseExcelInitialConditions(file)
        jsonData
      }
    ),
    list(
      property = "plotsFile",
      parse = function(file, jsonData) {
        jsonData$plots <- .parseExcelPlots(file)
        jsonData
      }
    ),
    list(
      property = "parameterIdentificationFile",
      parse = function(file, jsonData) {
        jsonData$parameterIdentification <-
          .parseExcelParameterIdentification(file)
        jsonData
      }
    )
  )

  for (section in sections) {
    file <- resolveConfigFile(propOrDefault(section$property))
    if (!is.null(file) && file.exists(file)) {
      jsonData <- section$parse(file, jsonData)
    }
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

  # Canonicalize every id (and every reference to one) so the imported project
  # uses safe, lowercase, single-path-segment ids. This is the same transform
  # the authoring API applies, run here because the definition-file tree keys files
  # by id and so requires canonical ids; applying it to definitions and
  # references together keeps foreign keys resolvable (a reference made from the
  # same Excel spelling as its definition still resolves). Excel ids that were
  # not already canonical (e.g. `Global`, `Aciclovir_PVB`) become `global`,
  # `aciclovir_pvb`.
  jsonData <- .canonicalizeProjectJsonIds(jsonData)

  # Write the single inlined `Project.json`. The inlined form is kept (rather
  # than only the tree) because the Excel axis of `projectStatus()` re-imports
  # the Excel into a fresh JSON and compares it section-by-section against this
  # file's raw content; emptying the inline sections would blind that
  # comparison.
  jsonText <- jsonlite::toJSON(
    jsonData,
    pretty = TRUE,
    auto_unbox = TRUE,
    digits = NA,
    null = "null"
  )
  writeLines(jsonText, outputPath)

  # Also explode the imported project into its `definitions/<kind>/` tree next
  # to the container, so the import yields a ready-to-use tree project: a later
  # `loadProject(outputPath)` reads the tree (which wins over the inline
  # sections), and edits are write-through to it, with no separate materialize
  # step. The container the tree writer leaves in place still carries the
  # inlined sections above (the tree writer only adds the `definitions/` files),
  # so the sync comparison keeps working. `Project$new()` (not `loadProject()`)
  # parses the just-written snapshot without running the cross-reference warning
  # pass, so a project with dangling refs imports quietly under `silent`.
  importedProject <- Project$new(projectFilePath = outputPath)
  for (kind in .definitionKindNames()) {
    .writeDefinitionTree(
      .sectionForKind(importedProject, kind),
      kind,
      importedProject,
      outputDir
    )
  }

  if (interactive() && !silent) {
    inputFile <- fs::path_rel(projectConfigPath, start = getwd())
    outputFile <- fs::path_rel(outputPath, start = getwd())
    msg <- messages$createdFileSnapshot(inputFile, outputFile)
    cli::cli_inform("{msg}")
  }

  invisible(outputPath)
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
#' @family project persistence
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

  # Container metadata rows. `name` and `description` are top-level container
  # fields; writing them here (and reading them back on import) keeps the
  # round trip lossless for the project's human-readable metadata.
  props <- c(props, "name", "description")
  vals <- c(vals, project$name %||% "", project$description %||% "")
  descs <- c(descs, "Project name", "Project description")

  # File path property rows. Both container blocks are written into the single
  # `Project.xlsx` Property table: the live working folders (`filePaths`) and
  # the Excel-bridge sheet names (`excel`). Re-importing reads them back and
  # re-splits them into the two blocks, so the round trip is lossless.
  pathPropsData <- c(.extractFilePathsData(project), .extractExcelData(project))
  for (propName in names(pathPropsData)) {
    props <- c(props, propName)
    vals <- c(vals, pathPropsData[[propName]]$value %||% "")
    descs <- c(descs, pathPropsData[[propName]]$description %||% "")
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
  # The project's single `parameterSets` section is exported as one workbook,
  # one sheet per set. Re-importing reads them all back into the same unified
  # section, so the round trip is lossless under the unified model.
  if (
    !is.null(project$parameterSets) &&
      length(project$parameterSets) > 0
  ) {
    sheets <- .parameterStructuresToExcelSheets(project$parameterSets)
    .writeExcel(sheets, file.path(configDir, "ModelParameters.xlsx"))
  }

  # --- InitialConditions.xlsx ---
  # One sheet per initial-condition set. The tolerant columns (`Is Present`,
  # `Scale Divisor`, `Neg. Values Allowed`) are regenerated with defaults, so
  # they are not preserved across an export/import round-trip.
  if (
    !is.null(project$initialConditions) &&
      length(project$initialConditions) > 0
  ) {
    icSheets <- .initialConditionsToExcelSheets(project$initialConditions)
    .writeExcel(icSheets, file.path(configDir, "InitialConditions.xlsx"))
  }

  # --- Individuals.xlsx ---
  indivSheets <- list()
  if (!is.null(project$individuals) && length(project$individuals) > 0) {
    indivSheets[["IndividualBiometrics"]] <- .individualsToExcelDf(
      project$individuals
    )
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
  # Parameter sets all live in ModelParameters.xlsx now (one unified section),
  # so this workbook carries only the application protocols.
  appSheets <- list()
  if (!is.null(project$applications) && length(project$applications) > 0) {
    appSheets[["ApplicationProtocols"]] <- .applicationsToExcelDf(
      project$applications
    )
  }
  if (length(appSheets) > 0) {
    .writeExcel(appSheets, file.path(configDir, "Applications.xlsx"))
  }

  # --- Plots.xlsx ---
  # The three plots sections are keyed lists; render each back to the Excel
  # sheet shape (`DataCombined` long-format, `plotConfiguration`, `plotGrids`)
  # so the export round-trips through `.parseExcelPlots()`. Empty sections are
  # skipped.
  dataCombined <- .unwrapDefinitionList(project$dataCombined)
  plots <- .unwrapDefinitionList(project$plots)
  plotGrids <- .unwrapDefinitionList(project$plotGrids)
  if (
    length(dataCombined %||% list()) > 0 ||
      length(plots %||% list()) > 0 ||
      length(plotGrids %||% list()) > 0
  ) {
    plotSheets <- list()
    dcSheet <- .dataCombinedToExcelDf(dataCombined)
    if (!is.null(dcSheet)) {
      plotSheets[["DataCombined"]] <- dcSheet
    }
    pcSheet <- .plotEntriesToExcelDf(plots)
    if (!is.null(pcSheet)) {
      plotSheets[["plotConfiguration"]] <- pcSheet
    }
    pgSheet <- .plotEntriesToExcelDf(plotGrids)
    if (!is.null(pgSheet)) {
      plotSheets[["plotGrids"]] <- pgSheet
    }
    if (length(plotSheets) > 0) {
      .writeExcel(plotSheets, file.path(configDir, "Plots.xlsx"))
    }
  }

  # --- ParameterIdentification.xlsx ---
  # The nested PI section becomes three `taskId`-joined sheets, inverted on
  # import by `.parseExcelParameterIdentification()`. Skipped when empty.
  piTasks <- .unwrapDefinitionList(project$parameterIdentification)
  if (length(piTasks %||% list()) > 0) {
    piSheets <- .parameterIdentificationToExcelSheets(piTasks)
    .writeExcel(piSheets, file.path(configDir, "ParameterIdentification.xlsx"))
  }

  if (interactive() && !silent) {
    relPath <- fs::path_rel(projConfigPath, start = getwd())
    msg <- messages$restoredProjectConfiguration(
      project$jsonPath %||% "Project",
      relPath
    )
    cli::cli_inform("{msg}")
  }

  invisible(projConfigPath)
}

# Compare a project's JSON against its Excel side-car and report whether they
# are in sync. Drives the Excel axis of `projectStatus()` (via
# `.projectSyncStatus()`), returning the
# `list(excel_in_sync = <logical>, details = <list>)` contract.
# Re-imports the Excel into a temporary JSON and diffs it section-by-section
# against the project's `Project.json` (ignoring the volatile `esqlabsRVersion`).
#
# @keywords internal
# @noRd
.compareJsonToExcel <- function(jsonPath, projectConfigPath, silent = FALSE) {
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

  # The Excel re-import canonicalizes every id (via
  # `.canonicalizeProjectJsonIds()`), but the original JSON may carry a
  # non-canonical id. Canonicalize the original the same way before comparing
  # so id canonicalization is not itself counted as drift (which would make an
  # otherwise-in-sync project report out-of-sync). An already-canonical id is
  # unchanged, so this is a no-op for a canonical original. Warnings are
  # suppressed (an in-place re-canonicalization of an already-canonical id
  # emits none anyway).
  originalJsonObj <- suppressWarnings(
    .canonicalizeProjectJsonIds(originalJsonObj)
  )

  # Remove esqlabsRVersion -- it changes with package updates and would cause
  # false out-of-sync reports
  originalJsonObj[["esqlabsRVersion"]] <- NULL
  currentJsonObj[["esqlabsRVersion"]] <- NULL

  if (identical(originalJsonObj, currentJsonObj)) {
    result <- list(
      excel_in_sync = TRUE,
      details = list()
    )
    if (!silent) {
      cli::cli_inform(messages$excelInSync())
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
      excel_in_sync = FALSE,
      details = differences
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

# Excel <-> JSON bridge: sync helper ----

#' Report a project's two-axis sync status
#'
#' @description Human-oriented, read-only report of how a `Project`'s in-memory
#'   state diverges from disk, on two axes:
#'
#'   - memory vs. tree: whether there are unsaved in-memory edits (the project
#'     is dirty). Reported as `NA` for an unbound in-memory project.
#'   - memory vs. Excel: when a `Project.xlsx` side-car is configured, whether
#'     it is a stale export of the current project (one-way: would re-exporting
#'     change it). Reported as `NA` when no side-car is configured or it cannot
#'     be read.
#'
#'   `projectStatus()` never reconciles either axis. To sync the tree, call
#'   [saveProject()]; to sync Excel, call [exportProjectToExcel()] or
#'   [importProjectFromExcel()].
#'
#' @param project A `Project` object.
#' @param silent Logical. If `TRUE`, suppresses the printed report and only
#'   returns the structured result (the same shape as `project$status`).
#'   Defaults to `FALSE`.
#'
#' @returns Invisibly, a `list(tree_in_sync, excel_in_sync, details)` (see
#'   the `status` field of [Project]).
#' @export
#' @family project persistence
#' @seealso [saveProject()], [reloadProject()], [exportProjectToExcel()],
#'   [importProjectFromExcel()].
#' @examples
#' \dontrun{
#' project <- loadProject("Project.json")
#' projectStatus(project) # human-readable two-axis report
#' project$status # the same information as a structured list
#' }
projectStatus <- function(project, silent = FALSE) {
  validateIsOfType(project, "Project")
  invisible(.projectSyncStatus(project, silent = silent))
}

#' Two-axis sync-status engine behind `projectStatus()` and `project$status`
#'
#' Reports both sync axes of an explicit-save project:
#'   - memory vs. tree: whether there are unsaved in-memory edits, driven by
#'     the project's internal dirty bit (`NA` for an unbound in-memory project);
#'   - memory vs. Excel: whether a configured `Project.xlsx` side-car is a stale
#'     export of the current project, reusing the `.compareJsonToExcel()`
#'     comparison (`NA` when there is no side-car or it cannot be read).
#'
#' @param project A `Project` object.
#' @param silent Logical. If `TRUE`, suppresses the printed report.
#' @returns Invisibly a named list with `tree_in_sync` (logical, or `NA` for an
#'   unbound in-memory project), `excel_in_sync` (logical, or `NA` when there
#'   is no Excel side-car to compare against, or when it cannot be
#'   read/compared) and `details` (per-axis differences, empty when both axes
#'   are in sync). When not `silent`, both axes are reported and a comparison
#'   failure surfaces a warning.
#' @keywords internal
#' @noRd
.projectSyncStatus <- function(project, silent = FALSE) {
  result <- list(tree_in_sync = NA, excel_in_sync = NA, details = list())

  # Axis 1: memory vs. tree. The dirty bit is the divergence signal; an unbound
  # in-memory project has no tree, reported as `NA`.
  jsonPath <- project$jsonPath
  if (is.null(jsonPath)) {
    result$tree_in_sync <- NA
  } else {
    result$tree_in_sync <- !project$.isModified()
    if (!result$tree_in_sync) {
      result$details$tree <- "unsaved in-memory edits"
    }
  }

  if (!silent) {
    if (is.na(result$tree_in_sync)) {
      cli::cli_alert_info(messages$syncNoTree())
    } else if (isTRUE(result$tree_in_sync)) {
      cli::cli_alert_success(messages$syncTreeClean())
    } else {
      cli::cli_alert_warning(messages$syncTreeDirty())
    }
  }

  # Axis 2: memory vs. Excel side-car.
  if (is.null(jsonPath) || !file.exists(jsonPath)) {
    if (!silent) {
      cli::cli_alert_info(messages$syncNoExcel())
    }
    return(invisible(result))
  }

  # Derive the Excel side-car by swapping the extension to `.xlsx`. Using
  # `path_ext_set` (rather than a `.json`-only substitution) keeps the
  # derivation correct for any container extension, including a `.esqlabsR`
  # snapshot, so a snapshot-loaded project does not mistake itself for its own
  # Excel side-car.
  excelPath <- as.character(fs::path_ext_set(jsonPath, "xlsx"))
  if (!file.exists(excelPath)) {
    if (!silent) {
      cli::cli_alert_info(messages$syncNoExcel())
    }
    return(invisible(result))
  }

  # A corrupt or unreadable Excel side-car cannot be compared. Report that
  # honestly as `NA` (the documented "cannot compare" state) rather than
  # claiming the project is in sync, and surface a warning in the non-silent
  # branch so the failure is not swallowed.
  compareError <- NULL
  excelStatus <- tryCatch(
    .compareJsonToExcel(
      jsonPath = jsonPath,
      projectConfigPath = excelPath,
      silent = TRUE
    ),
    error = function(e) {
      compareError <<- e
      NULL
    }
  )

  if (!is.null(compareError)) {
    result$excel_in_sync <- NA
    if (!silent) {
      cli::cli_warn(
        c(
          "Cannot compare the Excel side-car to the project.",
          "x" = conditionMessage(compareError),
          "i" = "The {.field excel_in_sync} status is reported as {.val NA}."
        )
      )
    }
    return(invisible(result))
  }

  result$excel_in_sync <- isTRUE(excelStatus$excel_in_sync)
  if (!result$excel_in_sync) {
    result$details$excel <- excelStatus$details
  }

  if (!silent) {
    if (result$excel_in_sync) {
      cli::cli_inform(messages$excelInSync())
    } else {
      cli::cli_alert_warning("Excel files differ from the project.")
    }
  }

  invisible(result)
}

# Excel <-> JSON bridge: internal helpers ----

#' Canonicalize every id and id-reference in an imported `Project.json` list
#'
#' Runs `.canonicalizeOneId()` over the keyed-section ids (`outputPaths`,
#' `parameterSets`, `applications` map keys; the scenario `name` and the
#' `individualId` / `populationId` self-id fields of the individual /
#' population records) and over every reference to one (a scenario's
#' `individual`, `population`, `application`, `parameterSets`,
#' `outputPaths`; an individual's or application's `parameterSets`). The same
#' deterministic transform is applied to a definition and to a reference, so a
#' reference made from the same Excel spelling as its definition still resolves
#' after canonicalization. Used by `importProjectFromExcel()` so the imported
#' project carries safe, lowercase, single-path-segment ids that the definition
#' tree can key files by. Silent (no per-id warning): an Excel import renames in
#' bulk and the migrate-from-excel guide documents the renaming.
#'
#' @keywords internal
#' @noRd
.canonicalizeProjectJsonIds <- function(jsonData) {
  canonScalar <- function(x) {
    if (is.null(x)) {
      return(x)
    }
    .canonicalizeOneId(as.character(x))
  }
  canonVec <- function(x) {
    if (is.null(x)) {
      return(x)
    }
    lapply(x, function(e) .canonicalizeOneId(as.character(e)))
  }
  canonNames <- function(section) {
    if (is.null(section) || length(section) == 0L) {
      return(section)
    }
    nms <- names(section)
    if (!is.null(nms)) {
      # Route the section's keyed ids through the collision-CHECKING path so
      # that two ids collapsing to the same canonical id abort the migration
      # (matching interactive authoring), rather than letting a downstream
      # rename silently drop the second definition. `.canonicalizeId()` also warns
      # per changed id; an Excel import renames in bulk and the migrate guide
      # documents that, so the per-id warning is suppressed while the
      # collision abort is allowed to propagate.
      names(section) <- suppressWarnings(.canonicalizeId(nms))
    }
    section
  }

  jsonData$outputPaths <- canonNames(jsonData$outputPaths)
  jsonData$parameterSets <- canonNames(jsonData$parameterSets)
  jsonData$initialConditions <- canonNames(jsonData$initialConditions)
  jsonData$applications <- canonNames(jsonData$applications)

  if (!is.null(jsonData$applications)) {
    jsonData$applications <- lapply(jsonData$applications, function(app) {
      if (!is.null(app$parameterSets)) {
        app$parameterSets <- canonVec(app$parameterSets)
      }
      app
    })
  }

  if (!is.null(jsonData$scenarios)) {
    jsonData$scenarios <- lapply(jsonData$scenarios, function(sc) {
      sc$name <- canonScalar(sc$name)
      sc$individual <- canonScalar(sc$individual)
      sc$population <- canonScalar(sc$population)
      sc$application <- canonScalar(sc$application)
      sc$parameterSets <- canonVec(sc$parameterSets)
      sc$initialConditions <- canonVec(sc$initialConditions)
      sc$outputPaths <- canonVec(sc$outputPaths)
      sc
    })
  }

  if (!is.null(jsonData$individuals)) {
    jsonData$individuals <- lapply(jsonData$individuals, function(ind) {
      ind$individualId <- canonScalar(ind$individualId)
      if (!is.null(ind$parameterSets)) {
        ind$parameterSets <- canonVec(ind$parameterSets)
      }
      ind
    })
  }

  if (!is.null(jsonData$populations)) {
    jsonData$populations <- lapply(jsonData$populations, function(pop) {
      pop$populationId <- canonScalar(pop$populationId)
      pop
    })
  }

  # A legacy (pre-6.0.0-split) snapshot nests the three plots parts under one
  # `plots` object (`plots = {dataCombined, plotConfiguration, plotGrids}`).
  # The current shape is three top-level sections (`dataCombined`, `plots` the
  # plot list, `plotGrids`). Lift a legacy nested object to the three top-level
  # keys so a legacy snapshot still migrates losslessly into the tree. A new
  # snapshot already carries the three top-level keys and is untouched here (its
  # `plots` is an array of plot records, not an object with a `dataCombined`
  # field).
  legacyPlots <- jsonData$plots
  if (
    is.list(legacyPlots) &&
      !is.null(names(legacyPlots)) &&
      any(
        c("dataCombined", "plotConfiguration", "plotGrids") %in%
          names(legacyPlots)
      )
  ) {
    jsonData$dataCombined <- legacyPlots$dataCombined
    jsonData$plots <- legacyPlots$plotConfiguration
    jsonData$plotGrids <- legacyPlots$plotGrids
  }

  # The three plots sections each persist as a keyed definition tree that keys
  # files by a canonical id (`dataCombinedId` / `plotId` / `plotGridId`), so
  # canonicalize those ids and every reference among the three together with the
  # same deterministic helper, so the migrated tree's inner cross-references
  # still resolve. A plot's `dataCombined` rows also reference a scenario by id;
  # canonicalize that so it resolves against the (canonicalized) scenario
  # definitions. The `dataSet` / `observedData` references point at observed
  # data, whose ids are file basenames / DataSet names matched verbatim and
  # never canonicalized, so they are deliberately left untouched.
  if (!is.null(jsonData$dataCombined)) {
    jsonData$dataCombined <- lapply(
      jsonData$dataCombined,
      function(dc) {
        dc$dataCombinedId <- canonScalar(dc$dataCombinedId)
        if (!is.null(dc$simulated)) {
          dc$simulated <- lapply(dc$simulated, function(sim) {
            sim$scenario <- canonScalar(sim$scenario)
            sim
          })
        }
        dc
      }
    )
  }
  if (!is.null(jsonData$plots)) {
    jsonData$plots <- lapply(
      jsonData$plots,
      function(plot) {
        plot$plotId <- canonScalar(plot$plotId)
        plot$dataCombined <- canonScalar(plot$dataCombined)
        plot
      }
    )
  }
  if (!is.null(jsonData$plotGrids)) {
    jsonData$plotGrids <- lapply(
      jsonData$plotGrids,
      function(grid) {
        grid$plotGridId <- canonScalar(grid$plotGridId)
        # `plots` is the grid's plot-id set stored as one comma-separated
        # string. A plot id may legally contain a comma, so decode and re-encode
        # with the escape-aware pair (`.splitPlotIDs()` / `.joinPlotIDs()`) that
        # every other reader/writer of this string uses; a plain
        # `strsplit(",")` / `paste(collapse = ", ")` here shreds a comma-bearing
        # id into several. Canonicalize each id in between.
        if (!is.null(grid$plots)) {
          ids <- .splitPlotIDs(as.character(grid$plots))
          ids <- vapply(ids, .canonicalizeOneId, character(1))
          grid$plots <- .joinPlotIDs(ids)
        }
        grid
      }
    )
  }

  # A parameter-identification task is keyed by its `id` (the definition-file id)
  # and references scenarios and output paths; canonicalize the task id and
  # every scenario / output-path reference it carries (at the task level and on
  # each parameter and output mapping) so the migrated tree's foreign keys
  # resolve. A mapping's `observedData` references observed data (verbatim
  # ids), and a parameter's / mapping's own `id` is an inner id, not an
  # definition-file id, so those are left untouched.
  if (!is.null(jsonData$parameterIdentification)) {
    jsonData$parameterIdentification <- lapply(
      jsonData$parameterIdentification,
      function(task) {
        task$id <- canonScalar(task$id)
        task$scenarios <- canonVec(task$scenarios)
        if (!is.null(task$parameters)) {
          task$parameters <- lapply(task$parameters, function(param) {
            param$scenarios <- canonVec(param$scenarios)
            param
          })
        }
        if (!is.null(task$outputMappings)) {
          task$outputMappings <- lapply(task$outputMappings, function(mapping) {
            mapping$scenarios <- canonVec(mapping$scenarios)
            mapping$outputPath <- canonScalar(mapping$outputPath)
            mapping
          })
        }
        task
      }
    )
  }

  jsonData
}

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
          value = .parseNumericCell(
            df[["Value"]][[i]],
            sheet = sheet,
            row = i,
            column = "Value"
          ),
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

#' Parse InitialConditions Excel file into JSON structure
#'
#' Reads `InitialConditions.xlsx` sheet by sheet and returns a named list
#' where each key is a sheet name (the initial-condition set id) and each value
#' is a list of records with fields `path`, `value`, and `unit`. The flat path
#' is built by joining `Container Path` and `Molecule Name` with `|`.
#'
#' Validation is shared with [readInitialConditionsFromXLS()] via the internal
#' `.readInitialConditionsRows()` reader, so a malformed Excel sheet (wrong
#' columns, invalid `Is Present`, blank path, missing value, blank unit) aborts
#' the import rather than serialising bad records into the JSON project.
#'
#' Only `path`, `value`, and `unit` are carried into the record. `Is Present`,
#' `Scale Divisor`, and `Neg. Values Allowed` are NOT preserved: `Is
#' Present=FALSE`/`0` rows are dropped at read time, and the other two columns
#' are unused by esqlabsR (the simulation consumes only path/value/unit). On an
#' Excel export they are regenerated with defaults (`Is Present=TRUE`, `Scale
#' Divisor=1`, `Neg. Values Allowed=FALSE`), so a non-default value in those
#' columns is not preserved across an Excel -> JSON -> Excel round-trip. Units
#' are mandatory for present molecules, so a record never carries a blank unit.
#'
#' @param filePath Path to the Excel file.
#' @param sheetNames Sheets to read. If NULL, reads all sheets.
#' @returns Named list of initial-conditions arrays.
#' @keywords internal
#' @noRd
.parseExcelInitialConditions <- function(filePath, sheetNames = NULL) {
  if (is.null(sheetNames)) {
    sheetNames <- readxl::excel_sheets(filePath)
  }
  rows <- .readInitialConditionsRows(filePath = filePath, sheets = sheetNames)

  result <- list()
  # Seed every requested sheet so empty sheets still surface as empty arrays.
  for (sheet in sheetNames) {
    result[[sheet]] <- list()
  }
  for (row in rows) {
    sheet <- row$sheet
    result[[sheet]][[length(result[[sheet]]) + 1L]] <- list(
      path = row$fullPath,
      value = row$value,
      unit = row$unit
    )
  }
  result
}

# The columns a Scenarios sheet must carry. `InitialConditions` is a newer,
# optional column (an older sheet omits it), so it is validated separately.
# Column access below uses `[[` (exact match) rather than `$` (partial match),
# so a renamed column (e.g. `OutputPathsId` for `OutputPathsIds`) is caught by
# the schema guard instead of silently resolving to a partial-prefix match or
# yielding NULL.
.scenarioSheetRequiredColumns <- c(
  "Scenario_name",
  "IndividualId",
  "PopulationId",
  "ReadPopulationFromCSV",
  "ModelParameterSheets",
  "ApplicationProtocol",
  "SimulationTime",
  "SimulationTimeUnit",
  "SteadyState",
  "SteadyStateTime",
  "SteadyStateTimeUnit",
  "OverwriteFormulasInSS",
  "ModelFile",
  "OutputPathsIds"
)

#' Parse Scenarios Excel sheet into JSON structure
#' @param scenarioDf Data frame from the Scenarios sheet
#' @returns List of scenario objects
#' @keywords internal
#' @noRd
.parseExcelScenarios <- function(scenarioDf) {
  requiredColumns <- .scenarioSheetRequiredColumns
  missingColumns <- setdiff(requiredColumns, names(scenarioDf))
  if (length(missingColumns) > 0L) {
    cli::cli_abort(c(
      "The {.field Scenarios} sheet is missing required \\
      column{?s}: {.val {missingColumns}}.",
      "i" = "Expected column{?s}: {.val {requiredColumns}}."
    ))
  }

  scenarios <- list()
  for (i in seq_len(nrow(scenarioDf))) {
    row <- scenarioDf[i, ]
    scenario <- list(
      name = as.character(row[["Scenario_name"]]),
      individual = .naToNull(as.character(row[["IndividualId"]])),
      population = .naToNull(as.character(row[["PopulationId"]])),
      readPopulationFromCSV = .naToNull(
        .toLogical(row[["ReadPopulationFromCSV"]], "ReadPopulationFromCSV")
      ),
      parameterSets = .parseCommaListToArray(row[["ModelParameterSheets"]]),
      # `InitialConditions` is a newer column; an older Scenarios sheet omits it,
      # so guard the lookup rather than abort on its absence.
      initialConditions = .parseCommaListToArray(
        if ("InitialConditions" %in% names(row)) {
          row[["InitialConditions"]]
        } else {
          NA
        }
      ),
      application = .naToNull(as.character(row[["ApplicationProtocol"]])),
      simulationTime = .naToNull(as.character(row[["SimulationTime"]])),
      simulationTimeUnit = .naToNull(as.character(row[["SimulationTimeUnit"]])),
      steadyState = .naToNull(.toLogical(row[["SteadyState"]], "SteadyState")),
      steadyStateTime = .naToNull(as.numeric(row[["SteadyStateTime"]])),
      steadyStateTimeUnit = .naToNull(
        as.character(row[["SteadyStateTimeUnit"]])
      ),
      overwriteFormulasInSS = .naToNull(
        .toLogical(row[["OverwriteFormulasInSS"]], "OverwriteFormulasInSS")
      ),
      modelFile = as.character(row[["ModelFile"]]),
      outputPaths = .parseCommaListToArray(row[["OutputPathsIds"]])
    )
    scenarios[[i]] <- scenario
  }
  scenarios
}

#' Parse the ApplicationProtocols Excel sheet into JSON structure
#'
#' One record per row keyed by `ApplicationId`. When the sheet carries a
#' `ParameterSets` column, its comma-separated cell becomes the record's
#' `parameterSets` array; a blank cell yields a record with no `parameterSets`.
#'
#' @param appsDf Data frame from the ApplicationProtocols sheet.
#' @returns Named list of application records (empty when `appsDf` has no rows).
#' @keywords internal
#' @noRd
.parseExcelApplications <- function(appsDf) {
  appsObj <- list()
  hasParameterSets <- "ParameterSets" %in% names(appsDf)
  for (i in seq_len(nrow(appsDf))) {
    id <- as.character(appsDf[["ApplicationId"]][[i]])
    appEntry <- list()
    if (hasParameterSets) {
      raw <- appsDf[["ParameterSets"]][[i]]
      if (!is.null(raw) && !is.na(raw) && nchar(as.character(raw)) > 0) {
        appEntry$parameterSets <- as.list(
          .parseCommaListToArray(as.character(raw))
        )
      }
    }
    appsObj[[id]] <- appEntry
  }
  appsObj
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

#' Parse Plots Excel file into the project's nested plots JSON structure
#'
#' Maps the legacy Excel plot sheets onto the v2.0 plots section so the import
#' round-trips through the definition-file tree (which keys files by `plotId` /
#' `plotGridId` / `dataCombinedId`). The `DataCombined` sheet is long-format
#' (one row per simulated/observed curve, grouped by `DataCombinedName`); the
#' `plotConfiguration` and `plotGrids` sheets carry the legacy `plotID` /
#' `DataCombinedName` / `name` / `plotIDs` column spellings. Only these three
#' sheets are plot sources; any other sheet (`exportConfiguration`,
#' `dataTypes`, `plotTypes`, `ObservedDataNames`) is ignored. A row missing a
#' usable id is dropped (so a stray/blank legacy row cannot abort the load).
#'
#' @param plotsFile Path to the Plots.xlsx file.
#' @returns Named list with `dataCombined` (nested), `plotConfiguration`, and
#'   `plotGrids` (arrays of records with canonical field names).
#' @keywords internal
#' @noRd
.parseExcelPlots <- function(plotsFile) {
  sheets <- readxl::excel_sheets(plotsFile)
  readSheet <- function(name) {
    if (!name %in% sheets) {
      return(NULL)
    }
    df <- readExcel(plotsFile, sheet = name)
    if (nrow(df) == 0) {
      return(NULL)
    }
    df
  }
  rowToFields <- function(df, i, idColumn, idField, drop = character()) {
    fields <- list()
    for (col in names(df)) {
      if (col %in% drop) {
        next
      }
      val <- .naToNull(df[[col]][[i]])
      if (is.null(val)) {
        next
      }
      field <- if (identical(col, idColumn)) idField else col
      fields[[field]] <- val
    }
    fields
  }

  list(
    dataCombined = .parseExcelDataCombinedSheet(readSheet("DataCombined")),
    plotConfiguration = .parseExcelPlotSheet(
      readSheet("plotConfiguration"),
      rowToFields
    ),
    plotGrids = .parseExcelPlotGridSheet(readSheet("plotGrids"), rowToFields)
  )
}

# The numeric fields on a DataCombined simulated / observed curve. Their unit
# siblings (`xOffsetsUnits` / `yOffsetsUnits`) stay character and are not listed.
.dataCombinedNumericFields <- c(
  "xOffsets",
  "yOffsets",
  "xScaleFactors",
  "yScaleFactors"
)

# Group the long-format DataCombined sheet (one row per simulated/observed
# curve, distinguished by the `dataType` column) into nested dataCombined
# records keyed by `dataCombinedId` (the `DataCombinedName` column). A row with
# no `DataCombinedName` is dropped.
#
# @keywords internal
# @noRd
.parseExcelDataCombinedSheet <- function(df) {
  if (is.null(df)) {
    return(list())
  }
  grouped <- list()
  for (i in seq_len(nrow(df))) {
    name <- .naToNull(df$DataCombinedName[[i]])
    if (is.null(name)) {
      next
    }
    name <- as.character(name)
    dataType <- .naToNull(df$dataType[[i]])
    entry <- list()
    for (col in names(df)) {
      if (col %in% c("DataCombinedName", "dataType")) {
        next
      }
      val <- .naToNull(df[[col]][[i]])
      if (!is.null(val)) {
        # The offset / scale-factor fields are numeric; a data.frame column that
        # also holds text in another row is read as character, so re-coerce
        # them so they round-trip as numbers rather than strings.
        if (col %in% .dataCombinedNumericFields) {
          val <- as.numeric(val)
        }
        entry[[col]] <- val
      }
    }
    if (is.null(grouped[[name]])) {
      grouped[[name]] <- list(
        dataCombinedId = name,
        simulated = list(),
        observed = list()
      )
    }
    if (identical(as.character(dataType), "observed")) {
      grouped[[name]]$observed <- c(grouped[[name]]$observed, list(entry))
    } else {
      grouped[[name]]$simulated <- c(grouped[[name]]$simulated, list(entry))
    }
  }
  unname(grouped)
}

# @keywords internal
# @noRd
.parseExcelPlotSheet <- function(df, rowToFields) {
  if (is.null(df)) {
    return(list())
  }
  records <- list()
  for (i in seq_len(nrow(df))) {
    fields <- rowToFields(
      df,
      i,
      idColumn = "plotID",
      idField = "plotId",
      drop = "DataCombinedName"
    )
    # Map the legacy `DataCombinedName` column onto the canonical JSON key; a
    # sheet that already uses `dataCombined` (e.g. one written by
    # `exportProjectToExcel()`) passes that column through verbatim instead.
    if ("DataCombinedName" %in% names(df)) {
      dataCombinedName <- .naToNull(df$DataCombinedName[[i]])
      if (!is.null(dataCombinedName)) {
        fields$dataCombined <- dataCombinedName
      }
    }
    if (is.null(fields$plotId)) {
      next
    }
    records[[length(records) + 1L]] <- fields
  }
  records
}

# @keywords internal
# @noRd
.parseExcelPlotGridSheet <- function(df, rowToFields) {
  if (is.null(df)) {
    return(list())
  }
  records <- list()
  for (i in seq_len(nrow(df))) {
    fields <- rowToFields(
      df,
      i,
      idColumn = "name",
      idField = "plotGridId",
      drop = "plotIDs"
    )
    # Map the legacy `plotIDs` column onto the canonical `plots` JSON key; a
    # sheet that already uses `plots` passes that column through verbatim.
    if ("plotIDs" %in% names(df)) {
      plotIds <- .naToNull(df$plotIDs[[i]])
      if (!is.null(plotIds)) {
        fields$plots <- plotIds
      }
    }
    if (is.null(fields$plotGridId)) {
      next
    }
    records[[length(records) + 1L]] <- fields
  }
  records
}

# Flatten the nested parameterIdentification section into Excel sheets.
# Three related sheets, joined by a `taskId` foreign key: `PITasks` (one row
# per task, the small `configuration` dict flattened to `config.<key>`
# columns), `PIParameters`, and `PIOutputMappings` (one row per nested
# record). `scenarios` arrays become comma-separated cells.
# `.parseExcelParameterIdentification()` inverts this. Returns a named list of
# data frames (one per non-empty sheet).
#
# @keywords internal
# @noRd
.parameterIdentificationToExcelSheets <- function(tasks) {
  taskRows <- list()
  paramRows <- list()
  mappingRows <- list()
  for (task in tasks) {
    taskRow <- list(
      taskId = task$id,
      scenarios = .formatArrayToCommaList(unlist(task$scenarios))
    )
    for (key in names(task$configuration %||% list())) {
      taskRow[[paste0("config.", key)]] <- task$configuration[[key]] %||% NA
    }
    taskRows[[length(taskRows) + 1]] <- as.data.frame(
      taskRow,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    for (p in task$parameters %||% list()) {
      paramRows[[length(paramRows) + 1]] <- as.data.frame(
        list(
          taskId = task$id,
          id = p$id,
          scenarios = .formatArrayToCommaList(unlist(p$scenarios)),
          path = p$path %||% NA,
          units = p$units %||% NA,
          minValue = p$minValue %||% NA,
          maxValue = p$maxValue %||% NA,
          startValue = p$startValue %||% NA
        ),
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
    }
    for (m in task$outputMappings %||% list()) {
      mappingRows[[length(mappingRows) + 1]] <- as.data.frame(
        list(
          taskId = task$id,
          id = m$id,
          scenarios = .formatArrayToCommaList(unlist(m$scenarios)),
          outputPath = m$outputPath %||% NA,
          observedData = m$observedData %||% NA,
          scaling = m$scaling %||% NA,
          xOffset = m$xOffset %||% NA,
          yOffset = m$yOffset %||% NA,
          xFactor = m$xFactor %||% NA,
          yFactor = m$yFactor %||% NA,
          weight = if (is.null(m$weight)) {
            NA
          } else {
            .formatArrayToCommaList(unlist(m$weight))
          }
        ),
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
    }
  }
  sheets <- list(PITasks = as.data.frame(dplyr::bind_rows(taskRows)))
  if (length(paramRows) > 0) {
    sheets[["PIParameters"]] <- as.data.frame(dplyr::bind_rows(paramRows))
  }
  if (length(mappingRows) > 0) {
    sheets[["PIOutputMappings"]] <- as.data.frame(dplyr::bind_rows(mappingRows))
  }
  sheets
}

# Invert `.parameterIdentificationToExcelSheets()` to the JSON PI array. Reads
# the three sheets and reassembles each task's nested `parameters` /
# `outputMappings` arrays (joined by `taskId`) and its `configuration` dict
# (from the `config.*` columns), producing the
# `{id, scenarios[], parameters[], outputMappings[], configuration}` shape
# `.parsePITasks()` consumes. Returns an unnamed list of PITask JSON objects.
#
# @keywords internal
# @noRd
.parseExcelParameterIdentification <- function(piFile) {
  sheets <- readxl::excel_sheets(piFile)
  if (!("PITasks" %in% sheets)) {
    return(list())
  }
  taskDf <- readExcel(piFile, sheet = "PITasks")
  paramDf <- if ("PIParameters" %in% sheets) {
    readExcel(piFile, sheet = "PIParameters")
  } else {
    NULL
  }
  mappingDf <- if ("PIOutputMappings" %in% sheets) {
    readExcel(piFile, sheet = "PIOutputMappings")
  } else {
    NULL
  }
  lapply(seq_len(nrow(taskDf)), function(i) {
    taskId <- as.character(taskDf$taskId[[i]])
    configCols <- grep("^config\\.", names(taskDf), value = TRUE)
    configuration <- list()
    for (col in configCols) {
      val <- .naToNull(taskDf[[col]][[i]])
      if (!is.null(val)) {
        configuration[[sub("^config\\.", "", col)]] <- val
      }
    }
    list(
      id = taskId,
      scenarios = as.list(.parseCommaListToArray(taskDf$scenarios[[i]])),
      parameters = .parseExcelPIRows(paramDf, taskId, "parameter"),
      outputMappings = .parseExcelPIRows(mappingDf, taskId, "mapping"),
      configuration = configuration
    )
  })
}

# Parse the PIParameters / PIOutputMappings rows for one task. Filters `df` to
# the rows whose `taskId` matches, drops the `taskId` bookkeeping column,
# splits `scenarios` (and `weight`, when present) back to arrays, and drops NA
# cells so optional fields stay absent.
#
# @keywords internal
# @noRd
.parseExcelPIRows <- function(df, taskId, kind) {
  if (is.null(df) || nrow(df) == 0) {
    return(list())
  }
  rows <- df[as.character(df$taskId) == taskId, , drop = FALSE]
  if (nrow(rows) == 0) {
    return(list())
  }
  cols <- setdiff(names(rows), "taskId")
  lapply(seq_len(nrow(rows)), function(i) {
    record <- list()
    for (col in cols) {
      val <- .naToNull(rows[[col]][[i]])
      if (is.null(val)) {
        next
      }
      record[[col]] <- if (col %in% c("scenarios", "weight")) {
        as.list(.parseCommaListToArray(val))
      } else {
        val
      }
    }
    record
  })
}

# Render the nested `dataCombined` keyed list back to the long-format
# `DataCombined` sheet (one row per simulated/observed curve, the `dataType`
# column distinguishing them, the `DataCombinedName` column the list key). The
# canonical-field export round-trips through `.parseExcelDataCombinedSheet()`.
# Returns NULL for an empty section.
#
# @keywords internal
# @noRd
.dataCombinedToExcelDf <- function(dataCombined) {
  dataCombined <- dataCombined %||% list()
  rows <- list()
  for (id in names(dataCombined)) {
    dc <- dataCombined[[id]]
    addRows <- function(entries, dataType) {
      for (entry in entries %||% list()) {
        row <- c(
          list(DataCombinedName = id, dataType = dataType),
          entry
        )
        rows[[length(rows) + 1L]] <<- row
      }
    }
    addRows(dc$simulated, "simulated")
    addRows(dc$observed, "observed")
  }
  .recordsToExcelDf(rows)
}

# Render a keyed list of plot / grid entries (each a named list of canonical
# fields) back to one data.frame row per entry. The in-memory reference field is
# mapped back to its suffixless on-disk key (`dataCombinedId` -> `dataCombined`,
# `plotIds` -> `plots`) so the exported column header matches the JSON key and
# the sheet round-trips through `.parseExcelPlots()`. Returns NULL for an empty
# part.
#
# @keywords internal
# @noRd
.plotEntriesToExcelDf <- function(entries) {
  entries <- entries %||% list()
  .recordsToExcelDf(unname(lapply(entries, function(e) {
    e <- .plotRefFieldToKey(e, class(e)[[1]])
    class(e) <- "list"
    e
  })))
}

# Bind a list of named-list records into a single data.frame, padding missing
# fields with NA across rows. Returns NULL for zero records.
#
# @keywords internal
# @noRd
.recordsToExcelDf <- function(records) {
  if (length(records) == 0) {
    return(NULL)
  }
  allCols <- unique(unlist(lapply(records, names)))
  rowDfs <- lapply(records, function(rec) {
    cells <- lapply(allCols, function(col) {
      val <- rec[[col]]
      if (is.null(val)) NA else paste(val, collapse = ", ")
    })
    names(cells) <- allCols
    as.data.frame(cells, stringsAsFactors = FALSE, check.names = FALSE)
  })
  as.data.frame(
    dplyr::bind_rows(rowDfs),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

#' Convert parameter sets to Excel sheet data frames
#'
#' Each set is a `parameterSets` section entry: a list of records shaped
#' `list(containerPath, parameterName, value, units)`. `.parameterSetToStructure()`
#' collapses that record list into the parallel `list(paths, values, units)`
#' vectors this writer needs (and returns `NULL` for an empty set), so the
#' exported sheet carries the set's values, paths, and units.
#'
#' @param parameterSets Named list of `parameterSets` section entries.
#' @returns Named list of data frames suitable for Excel sheets
#' @keywords internal
#' @noRd
.parameterStructuresToExcelSheets <- function(parameterSets) {
  sheets <- list()
  for (name in names(parameterSets)) {
    params <- .parameterSetToStructure(parameterSets[[name]])
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

#' Convert initial-conditions structures to Excel sheet data frames
#'
#' The record carries only `path`, `value`, and `unit`, so the `Is Present`,
#' `Scale Divisor`, and `Neg. Values Allowed` columns are emitted as fixed
#' defaults (`TRUE`, `1`, `FALSE`); these columns are not preserved across an
#' Excel -> JSON -> Excel round-trip.
#'
#' @param initialConditions Named list of initial-conditions sets (each set is a
#'   list of records with fields `path`, `value`, `unit`).
#' @returns Named list of data frames suitable for Excel sheets.
#' @keywords internal
#' @noRd
.initialConditionsToExcelSheets <- function(initialConditions) {
  sheets <- list()
  for (name in names(initialConditions)) {
    entries <- initialConditions[[name]]
    if (is.null(entries) || length(entries) == 0L) {
      sheets[[name]] <- data.frame(
        `Container Path` = character(0),
        `Molecule Name` = character(0),
        `Is Present` = logical(0),
        Value = numeric(0),
        Units = character(0),
        `Scale Divisor` = numeric(0),
        `Neg. Values Allowed` = logical(0),
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
      next
    }
    splitPaths <- lapply(entries, function(e) {
      parts <- strsplit(e$path, "|", fixed = TRUE)[[1]]
      list(
        containerPath = paste(parts[-length(parts)], collapse = "|"),
        moleculeName = parts[[length(parts)]]
      )
    })
    sheets[[name]] <- data.frame(
      `Container Path` = vapply(
        splitPaths,
        function(x) x$containerPath,
        character(1)
      ),
      `Molecule Name` = vapply(
        splitPaths,
        function(x) x$moleculeName,
        character(1)
      ),
      `Is Present` = rep(TRUE, length(entries)),
      Value = vapply(entries, function(e) as.double(e$value), double(1)),
      Units = vapply(entries, function(e) e$unit %||% "", character(1)),
      `Scale Divisor` = rep(1, length(entries)),
      `Neg. Values Allowed` = rep(FALSE, length(entries)),
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
#' @param outputPaths Retained for call-site compatibility; unused. Each
#'   scenario carries its output-path ids as the names of its own
#'   `outputPaths` vector, so no project-level reverse-lookup is needed.
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
    initialConditionsStr <- .formatArrayToCommaList(sc$initialConditions)
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
    # outputPaths -> the ids are the names of `sc$outputPaths` (a named vector
    # of id -> resolved path). Export those names directly rather than
    # reverse-looking-them-up by path value: two distinct ids may resolve to the
    # same literal path, and a value-based `match()` would collapse them to one
    # id and drop the other.
    outputPathIdsStr <- NA
    if (!is.null(sc$outputPaths)) {
      ids <- names(sc$outputPaths)
      ids <- ids[!is.na(ids) & nzchar(ids)]
      if (length(ids) > 0) {
        outputPathIdsStr <- .formatArrayToCommaList(ids)
      }
    }

    # Reconstruct steadyStateTime back to the original unit, but only for a
    # scenario that actually runs steady-state. A non-steady-state scenario
    # carries the parser's default `steadyStateTime` (with a null unit); writing
    # it here would fabricate a unit and a steady-state time that re-import then
    # materializes as a spurious configuration, so it is left blank instead.
    ssTime <- NA
    ssTimeUnit <- NA
    if (
      isTRUE(sc$simulateSteadyState) &&
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
      InitialConditions = initialConditionsStr,
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

#' Extract private .filePathsData from a Project (the live working folders)
#' @param project Project object
#' @returns Named list of property data
#' @keywords internal
#' @noRd
.extractFilePathsData <- function(project) {
  project$.getFilePathsData()
}

#' Extract private .excelData from a Project (the Excel-bridge sheet names)
#' @param project Project object
#' @returns Named list of property data (empty when no Excel side-car)
#' @keywords internal
#' @noRd
.extractExcelData <- function(project) {
  project$.getExcelData()
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

#' Coerce a single Excel logical cell tolerantly to `TRUE`/`FALSE`/`NA`
#'
#' Bare `as.logical()` only recognises `"TRUE"`/`"FALSE"`/`"T"`/`"F"`; a legacy
#' Excel sheet may store a boolean as `1`/`0`, `Yes`/`No`, or `true`/`false`,
#' all of which `as.logical()` silently turns into `NA` (which then defaults to
#' `FALSE` downstream, flipping the meaning). This helper accepts those common
#' spellings, case-insensitively, and aborts on a genuinely unparseable value
#' naming the field so a typo is caught rather than silently dropped. A blank
#' cell (`NA` / empty string) stays `NA` (the field is absent).
#'
#' @param x A length-1 cell value (logical, numeric, or character).
#' @param field Name of the field, used in the abort message.
#' @returns A length-1 logical (`TRUE`, `FALSE`, or `NA`).
#' @keywords internal
#' @noRd
.toLogical <- function(x, field) {
  if (is.null(x) || length(x) == 0L || is.na(x)) {
    return(NA)
  }
  if (is.logical(x)) {
    return(x)
  }
  if (is.numeric(x)) {
    if (x == 1) {
      return(TRUE)
    }
    if (x == 0) {
      return(FALSE)
    }
  } else {
    token <- tolower(trimws(as.character(x)))
    if (token == "") {
      return(NA)
    }
    if (token %in% c("true", "t", "yes", "y", "1")) {
      return(TRUE)
    }
    if (token %in% c("false", "f", "no", "n", "0")) {
      return(FALSE)
    }
  }
  cli::cli_abort(c(
    "Cannot interpret {.field {field}} value {.val {x}} as a logical.",
    "i" = "Use a boolean-like value \\
    ({.val TRUE}/{.val FALSE}, {.val 1}/{.val 0}, {.val Yes}/{.val No})."
  ))
}

#' Coerce a single Excel numeric cell, aborting on a non-blank unparseable value
#'
#' A blank cell (`NA` / empty string) yields `NA` (an absent value is allowed).
#' A non-blank cell that does not coerce to a number (text, or a comma-decimal
#' such as `1,5`) aborts naming the sheet, row, and column, rather than silently
#' becoming `NA` and serialising a value-less parameter into the JSON project.
#'
#' @param x A length-1 cell value.
#' @param sheet,row,column The cell's location, used in the abort message.
#' @returns A length-1 numeric (`NA` for a blank cell).
#' @keywords internal
#' @noRd
.parseNumericCell <- function(x, sheet, row, column) {
  if (is.null(x) || length(x) == 0L) {
    return(NA_real_)
  }
  if (is.na(x) || (is.character(x) && trimws(x) == "")) {
    return(NA_real_)
  }
  value <- suppressWarnings(as.numeric(x))
  if (is.na(value)) {
    cli::cli_abort(c(
      "Cannot interpret the {.field {column}} cell as a number.",
      "x" = "Sheet {.val {sheet}}, row {row}: {.val {x}}.",
      "i" = "A blank cell is allowed; a non-blank cell must be numeric \\
      (use {.val .} as the decimal separator)."
    ))
  }
  value
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
