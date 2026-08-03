# Excel <-> JSON bridge: public API ----

#' Import project configuration from Excel files
#'
#' @description Reads all Excel configuration files in an esqlabsR project and
#' converts them to the JSON project format: a `Project.json` project file plus
#' one file per definition in the `definitions/` folder. The result is a
#' ready-to-use project — `loadProject("<outputDir>/Project.json")` can open it
#' directly. This is the migration path from Excel-based projects to the
#' JSON-primary workflow.
#'
#' The `configurationsFolder` and the per-section workbook filenames are read
#' from the Excel file and must stay under the project folder: a value that
#' escapes it (a `../` climb or an absolute path) aborts naming the field. A
#' folder deliberately placed outside the project with the `${VAR}`
#' environment-variable form is still allowed.
#'
#' @param projectConfigPath Path to the `Project.xlsx` file.
#'   Defaults to `"Project.xlsx"`.
#' @param outputDir Directory where the JSON project is created. If `NULL`
#'   (default), it is created in the same directory as the source Excel file.
#' @param overwrite Logical. Guards against silently replacing an existing JSON
#'   project. With `overwrite = FALSE` (default), the import aborts when a
#'   project file or a non-empty `definitions/` tree already exists in
#'   `outputDir`, because re-importing replaces the JSON project with the Excel
#'   state and deletes any definitions authored only on the JSON side. Pass
#'   `overwrite = TRUE` to replace the existing JSON project with the Excel
#'   state.
#' @param silent Logical. If `TRUE`, suppresses the import summary (the project
#'   written, its per-section definition counts, and the folders copied or
#'   missing). Defaults to `FALSE`.
#' @param copyAssets Logical. Whether to copy the input folders the project
#'   references (models, data, csv populations) into `outputDir`, which is what
#'   makes the imported project runnable where it was written. Defaults to
#'   `TRUE`. Set it to `FALSE` when only the definitions are wanted and the
#'   assets would be wasted work, as when the import feeds a throwaway
#'   comparison snapshot.
#' @param projectFileName Name of the project file the import writes in
#'   `outputDir`. Defaults to `"Project.json"`, the same name [initProject()]
#'   and [loadProject()] use, so an imported project opens like any other. Pass
#'   another name (for example `"MyStudy"`, or `"MyStudy.json"`) to label the
#'   project file after the study rather than by the generic name; a `.json`
#'   extension is appended when the name does not already end in one. It must be
#'   a plain filename, not a path.
#'
#'   This names the project file only. The `definitions/` tree beside it, and
#'   the copied `Models/` and `Data/` folders, are shared by whatever is in
#'   `outputDir`, so a second import into the same folder still replaces the
#'   first project rather than sitting alongside it. Give each project its own
#'   `outputDir`.
#'
#' @returns Invisibly returns the path to the created project file.
#' @export
#' @family projectPersistence
importProjectFromExcel <- function(
  projectConfigPath = "Project.xlsx",
  outputDir = NULL,
  overwrite = FALSE,
  silent = FALSE,
  copyAssets = TRUE,
  projectFileName = "Project.json"
) {
  validateIsString(projectConfigPath)
  validateIsLogical(copyAssets)
  .validateFilenameSegment(projectFileName, messages$invalidProjectFileName)

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

  # Resolve the configurations folder relative to the Excel file. The
  # `configurationsFolder` and the per-section workbook filenames come from the
  # author-controlled Property column, so they are contained under the Excel
  # project directory (`pcDir`) the same way the JSON-side read paths are
  # contained under their working folders (see `.resolveWorkingFolder()` /
  # `.resolveProjectPath()`): a crafted workbook cannot name a folder or file
  # that escapes the project root via `../` or an absolute path. The `${VAR}`
  # environment-variable form remains the sanctioned way to point at an
  # out-of-project location, so a value that declares one is exempt.
  configsFolderRaw <- prop("configurationsFolder")
  configsFolder <- configsFolderRaw
  if (!is.null(configsFolder)) {
    # Containment is judged on the raw (pre-expansion) value: a `${VAR}` opts
    # into an out-of-project location and is exempt, everything else must stay
    # under `pcDir`. Resolution then expands the variable and joins a relative
    # value onto `pcDir`, matching `.cleanPath()`'s expand-then-resolve order.
    if (!.declaresEnvVarPath(configsFolderRaw)) {
      configsFolder <- .resolveProjectPath(
        configsFolder,
        pcDir,
        "configurationsFolder"
      )
    } else {
      configsFolder <- .replaceEnvVarPath(configsFolder)
      if (!fs::is_absolute_path(configsFolder)) {
        configsFolder <- file.path(pcDir, configsFolder)
      }
    }
    configsFolder <- normalizePath(configsFolder, mustWork = FALSE)
  }

  # Helper to resolve a config file path, contained under `configsFolder`.
  resolveConfigFile <- function(fileName, fieldName = "configuration file") {
    if (is.null(fileName) || is.na(fileName) || fileName == "") {
      return(NULL)
    }
    if (is.null(configsFolder)) {
      return(NULL)
    }
    # Abort if the author-controlled filename escapes the configurations folder
    # (a `../`-climbing or absolute value); a legitimate missing file is left to
    # the caller's own existence check, so containment must run before that.
    resolved <- .resolveProjectPath(fileName, configsFolder, fieldName)
    normalizePath(resolved, mustWork = FALSE)
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
  filePathProps <- .resolveExcelPopulationsFolder(
    filePathProps,
    pcDir,
    configsFolder
  )
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
    # Model parameters: every sheet is a parameter set. This is the first
    # workbook to contribute to `parameterSets`, so nothing here is ever renamed;
    # it goes through the shared appender all the same, so every contributor
    # reaches the section the one way.
    list(
      property = "modelParamsFile",
      parse = function(file, jsonData) {
        appended <- .appendParameterSets(
          jsonData$parameterSets,
          .parseExcelParameterSheets(file),
          file
        )
        jsonData$parameterSets <- appended$sets
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
          parsedSets <- .parseExcelParameterSheets(
            file,
            sheetNames = paramSheetNames
          )
          appended <- .appendParameterSets(
            jsonData$parameterSets,
            parsedSets,
            file
          )
          jsonData$parameterSets <- appended$sets
          # Only a sheet that parsed as a parameter set can be linked; a skipped
          # non-parameter sheet has no set for the individual to point at, from
          # either direction: not as its own same-named override below, and not
          # through the `Individual Parameter Sets` column here.
          setSheetNames <- names(parsedSets)
          jsonData$individuals <- .dropSkippedSheetRefs(
            jsonData$individuals,
            setdiff(paramSheetNames, setSheetNames)
          )
          # A sheet named after an individual is that individual's own parameter
          # override; link it so the override is applied. The match is on the
          # canonical id, so `Indiv1` sheet links to the `Indiv1` individual
          # regardless of case, and the sheet's set id is added to the
          # individual's `parameterSets` (deduplicated, keeping any set the
          # `ParameterSets` column already named).
          sheetCanonical <- vapply(
            setSheetNames,
            .canonicalizeOneId,
            character(1)
          )
          jsonData$individuals <- lapply(jsonData$individuals, function(indiv) {
            # Both references an individual carries name sheets of *this*
            # workbook (its own same-named sheet, and the `Individual Parameter
            # Sets` column), so a sheet the appender had to rename is followed to
            # its new id rather than left pointing at the earlier workbook's set
            # that took the plain id. Guarded on a rename having happened, so an
            # individual that declares no sets keeps the field absent rather than
            # gaining an empty one.
            if (
              length(appended$renames) > 0L && !is.null(indiv$parameterSets)
            ) {
              indiv$parameterSets <- as.list(.applyIdRenames(
                unlist(indiv$parameterSets),
                appended$renames
              ))
            }
            indivCanonical <- .canonicalizeOneId(indiv$individualId)
            # A blank/NA individual id (e.g. a trailing blank row) matches
            # nothing: an `NA ==` comparison yields all-NA, and indexing by an
            # all-NA logical returns a vector of NAs, not an empty one, so guard
            # it explicitly rather than injecting NA into `parameterSets`.
            if (is.na(indivCanonical)) {
              return(indiv)
            }
            match <- setSheetNames[indivCanonical == sheetCanonical]
            if (length(match) > 0L) {
              indiv$parameterSets <- as.list(unique(c(
                unlist(indiv$parameterSets),
                .applyIdRenames(match, appended$renames)
              )))
            }
            indiv
          })
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
    # Applications. Two workbook layouts are supported. The newer layout carries
    # a single `ApplicationProtocols` sheet listing the applications, with every
    # other sheet a parameter set it references. The 5.x layout has no
    # `ApplicationProtocols` sheet: it stores one sheet per protocol, each a
    # parameter-set-shaped sheet, so each such sheet becomes both a parameter set
    # (keyed by sheet name) and an `Application` wrapping it, so a scenario that
    # names the protocol by id resolves.
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
          paramSheetNames <- setdiff(sheets, "ApplicationProtocols")
          if (length(paramSheetNames) > 0) {
            parsedSets <- .parseExcelParameterSheets(
              file,
              sheetNames = paramSheetNames
            )
            appended <- .appendParameterSets(
              jsonData$parameterSets,
              parsedSets,
              file
            )
            jsonData$parameterSets <- appended$sets
            # A `ParameterSets` cell naming a skipped sheet points at a set that
            # was never created, so drop that reference before following any
            # rename (a skipped sheet is never renamed, so the order is free).
            jsonData$applications <- .dropSkippedSheetRefs(
              jsonData$applications,
              setdiff(paramSheetNames, names(parsedSets))
            )
            # The `ParameterSets` column names sheets of this workbook, so follow
            # a renamed sheet to its new id. Guarded on a rename having happened,
            # so a protocol that declares no sets keeps the field absent.
            if (length(appended$renames) > 0L) {
              jsonData$applications <- lapply(
                jsonData$applications,
                function(app) {
                  if (!is.null(app$parameterSets)) {
                    app$parameterSets <- as.list(.applyIdRenames(
                      unlist(app$parameterSets),
                      appended$renames
                    ))
                  }
                  app
                }
              )
            }
          }
        } else if (length(sheets) > 0) {
          parsedSets <- .parseExcelParameterSheets(file, sheetNames = sheets)
          appended <- .appendParameterSets(
            jsonData$parameterSets,
            parsedSets,
            file
          )
          jsonData$parameterSets <- appended$sets
          # One application per protocol sheet, keyed by sheet name and wrapping
          # its own parameter set: normally the same-named one (both ids
          # canonicalize identically, so the reference resolves), or the renamed
          # id when an earlier workbook already held that name. The record carries
          # no inner `id`; the key is the id, matching
          # `.parseExcelApplications()`. Only a sheet that parsed as a parameter
          # set becomes a protocol, so no application wraps a set that was
          # skipped.
          protocolSheets <- names(parsedSets)
          if (length(protocolSheets) > 0) {
            setIds <- .applyIdRenames(protocolSheets, appended$renames)
            jsonData$applications <- c(
              jsonData$applications,
              stats::setNames(
                lapply(setIds, function(setId) {
                  list(parameterSets = list(setId))
                }),
                protocolSheets
              )
            )
          }
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
        # The scenarios section is parsed above, so the oldest 5.x mapping layout
        # can reach the output paths its rows identify their outputs through.
        tasks <- .parseExcelParameterIdentification(file, jsonData$scenarios)
        # A 5.x output mapping may name its output path by full OSPS path rather
        # than by an output-path id. Rewrite such a full path to the id of the
        # matching `outputPaths` definition so the reference resolves.
        jsonData$parameterIdentification <- .resolvePIOutputPathRefs(
          tasks,
          jsonData$outputPaths
        )
        jsonData
      }
    )
  )

  for (section in sections) {
    file <- resolveConfigFile(
      propOrDefault(section$property),
      fieldName = section$property
    )
    if (!is.null(file) && file.exists(file)) {
      jsonData <- section$parse(file, jsonData)
    }
  }

  # --- Determine output path ---
  if (is.null(outputDir)) {
    outputDir <- pcDir
  }

  # Observed data. The project records a single `dataFile` (an experimental-data
  # workbook) and a `dataImporterConfigurationFile` under `dataFolder`, not a
  # per-section workbook, so it is parsed outside the `sections` loop. The
  # importer reifies it as one `excel` observed-data definition keyed by the
  # data-file basename, listing the workbook's sheets; the loader resolves the
  # `file` / `importerConfiguration` basenames against `dataFolder`.
  jsonData <- .parseExcelObservedData(jsonData, prop, pcDir, outputDir)

  # Append `.json` rather than `fs::path_ext_set()`, which would *replace* an
  # existing extension: a dotted stem like `trial.v1` reads as an extension, so
  # setting it would strip the `.v1` and collapse `trial.v1` and `trial.v2` onto
  # one container.
  outputPath <- file.path(outputDir, .withJsonExtension(projectFileName))

  # Guard against silently replacing an existing JSON project. The import writes
  # the container and fully reconciles the `definitions/` tree (deleting any
  # definition authored only on the JSON side), so re-importing over a JSON
  # project the user has since edited would erase that work. Abort unless
  # `overwrite = TRUE` when the target container file already exists, or a
  # non-empty `definitions/` tree already sits in `outputDir`.
  definitionsDir <- file.path(outputDir, "definitions")
  hasDefinitionTree <- dir.exists(definitionsDir) &&
    any(grepl("\\.json$", list.files(definitionsDir, recursive = TRUE)))
  if (!overwrite && (file.exists(outputPath) || hasDefinitionTree)) {
    cli::cli_abort(messages$importWouldOverwriteProject(outputDir))
  }

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

  # An observed curve that names no data set is kept as authored, so a user who
  # can still supply the missing piece is not silently robbed of the row. But
  # nothing else would tell them why the freshly imported project is invalid, so
  # say it here rather than leaving it to be discovered at the next call. After
  # canonicalization, so the ids named are the ids the tree and
  # `validateProject()` use.
  .warnIncompleteObservedCurves(jsonData$dataCombined)

  # Bootstrap an in-memory project from the imported data by writing the inlined
  # JSON to the container path and parsing it back. This inlined form is only a
  # transient bootstrap: `.writeProjectTree()` below overwrites the container
  # with the slim (`containerOnly`) shape, so the inlined file never survives the
  # call. `Project$new()` (not `loadProject()`) parses it without running the
  # cross-reference warning pass, so a project with dangling refs imports quietly
  # under `silent`.
  jsonText <- jsonlite::toJSON(
    jsonData,
    pretty = TRUE,
    auto_unbox = TRUE,
    digits = NA,
    null = "null"
  )
  writeLines(jsonText, outputPath)
  importedProject <- Project$new(projectFilePath = outputPath)

  # Write the imported project as a normal tree project: the slim
  # (`containerOnly`) `Project.json` container plus the `definitions/<kind>/`
  # tree, exactly what `saveProject()` and `initProject()` produce. There is one
  # canonical on-disk `Project.json` shape, so an imported project is
  # indistinguishable from any other tree project. The Excel sync check compares
  # the in-memory project against the workbook (see `.compareJsonToExcel()`), so
  # it no longer needs an inlined container to diff against.
  .writeProjectTree(importedProject, outputDir, containerPath = outputPath)

  # The definitions reference models, data, and population files by a path
  # relative to the project folder, so importing into a different folder would
  # leave every one of those references dangling. Bring the referenced input
  # folders along, so the imported project runs where it was written.
  assets <- if (copyAssets) {
    .copyExcelProjectAssets(filePathProps, pcDir, outputDir, overwrite)
  } else {
    list(copied = character(), notCopied = character())
  }

  # Report what the import produced. Not gated on an interactive session: an
  # import run from a script is exactly the case where the call would otherwise
  # finish with no sign of what (or whether) anything was written. `silent` is
  # the way to turn it off.
  if (!silent) {
    inputFile <- .readablePath(projectConfigPath)
    outputFile <- .readablePath(outputPath)
    msg <- messages$importedProject(inputFile, outputFile)
    cli::cli_inform("{msg}")
    # The per-section counts, rendered by the project's own definitions block so
    # the summary and `print(project)` can never disagree about the labels. That
    # block prints to stdout, so it is captured and re-emitted verbatim on the
    # message stream, keeping the whole summary on one stream (and out of the way
    # of anything capturing the function's own output).
    cli::cli_verbatim(utils::capture.output(print(
      importedProject$definitions
    )))
    if (length(assets$copied) > 0L) {
      msg <- messages$importCopiedAssetFolders(assets$copied)
      cli::cli_inform("{msg}")
    }
    if (length(assets$notCopied) > 0L) {
      msg <- messages$importUncopiedAssetFolders(assets$notCopied)
      cli::cli_warn("{msg}")
    }
  }

  invisible(outputPath)
}

#' Export a Project to Excel files
#'
#' @description Writes Excel configuration files from a `Project`
#' object (typically loaded from JSON). This is the reverse of
#' `importProjectFromExcel()`.
#'
#' The top-level workbook is named after the project file, so a project loaded
#' from `MyStudy.json` exports `MyStudy.xlsx` (and one loaded from the
#' canonical `Project.json` exports `Project.xlsx`). That is the pairing
#' [projectStatus()] reads back, so the workbook this writes is the one the
#' status check then compares the project against.
#'
#' @param project A `Project` object.
#' @param outputDir Directory where the Excel files will be created. Defaults
#'   to the directory of the source JSON file.
#' @param overwrite Logical. Guards against silently overwriting existing Excel
#'   workbooks. With `overwrite = FALSE` (default), the export aborts when the
#'   project's own workbook or any `Configurations/` workbook already exists in
#'   `outputDir`, because the export replaces each workbook wholesale and would
#'   discard any hand-edits it carries. Pass `overwrite = TRUE` to replace the
#'   existing workbooks.
#' @param silent Logical. If `TRUE`, suppresses informational messages.
#'   Defaults to `FALSE`.
#'
#' @returns Invisibly returns the path to the created workbook.
#' @export
#' @family projectPersistence
exportProjectToExcel <- function(
  project,
  outputDir = NULL,
  overwrite = FALSE,
  silent = FALSE
) {
  validateIsOfType(project, "Project")

  if (is.null(outputDir)) {
    outputDir <- project$info$projectDirPath %||% "."
  }

  configDir <- file.path(outputDir, "Configurations")

  # The side-car is named after the project file, so a project loaded from
  # `MyStudy.json` exports `MyStudy.xlsx`. That is the pairing `projectStatus()`
  # reads back, so exporting produces the workbook the status check then
  # compares against.
  workbookName <- .excelSideCarName(project$info$projectFilePath)

  # Guard against silently overwriting existing workbooks. Every workbook is
  # written with `writexl::write_xlsx()`, which replaces the target file
  # wholesale, and the default `outputDir` is the project's own directory, so a
  # bare `exportProjectToExcel(project)` would overwrite the project's own
  # workbook and `Configurations/*.xlsx` side-cars (hand-edits included).
  # Abort unless `overwrite = TRUE` when that workbook or any `Configurations/`
  # workbook already exists in `outputDir`.
  existingWorkbooks <- c(
    if (file.exists(file.path(outputDir, workbookName))) workbookName,
    if (dir.exists(configDir)) {
      list.files(configDir, pattern = "\\.xlsx$")
    }
  )
  if (!overwrite && length(existingWorkbooks) > 0L) {
    cli::cli_abort(messages$exportWouldOverwriteWorkbooks(outputDir))
  }

  if (!dir.exists(outputDir)) {
    dir.create(outputDir, recursive = TRUE)
  }

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
  vals <- c(vals, project$info$name %||% "", project$info$description %||% "")
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
  projConfigPath <- file.path(outputDir, workbookName)
  .writeExcel(projConfigDf, projConfigPath)

  # --- ModelParameters.xlsx ---
  # The project's single `parameterSets` section is exported as one workbook,
  # one sheet per set. Re-importing reads them all back into the same unified
  # section, so the round trip is lossless under the unified model.
  if (
    !is.null(project$definitions$parameterSets) &&
      length(project$definitions$parameterSets) > 0
  ) {
    sheets <- .parameterStructuresToExcelSheets(
      project$definitions$parameterSets
    )
    .writeExcel(sheets, file.path(configDir, "ModelParameters.xlsx"))
  }

  # --- InitialConditions.xlsx ---
  # One sheet per initial-condition set. The tolerant columns (`Is Present`,
  # `Scale Divisor`, `Neg. Values Allowed`) are regenerated with defaults, so
  # they are not preserved across an export/import round-trip.
  if (
    !is.null(project$definitions$initialConditions) &&
      length(project$definitions$initialConditions) > 0
  ) {
    icSheets <- .initialConditionsToExcelSheets(
      project$definitions$initialConditions
    )
    .writeExcel(icSheets, file.path(configDir, "InitialConditions.xlsx"))
  }

  # --- Individuals.xlsx ---
  indivSheets <- list()
  if (
    !is.null(project$definitions$individuals) &&
      length(project$definitions$individuals) > 0
  ) {
    indivSheets[["IndividualBiometrics"]] <- .individualsToExcelDf(
      project$definitions$individuals
    )
  }
  if (length(indivSheets) > 0) {
    .writeExcel(indivSheets, file.path(configDir, "Individuals.xlsx"))
  }

  # --- Populations.xlsx ---
  if (
    !is.null(project$definitions$populations) &&
      length(project$definitions$populations) > 0
  ) {
    popDf <- .populationsToExcelDf(project$definitions$populations)
    .writeExcel(popDf, file.path(configDir, "Populations.xlsx"))
  }

  # --- Scenarios.xlsx ---
  scenSheets <- list()
  if (
    !is.null(project$definitions$scenarios) &&
      length(project$definitions$scenarios) > 0
  ) {
    scenSheets[["Scenarios"]] <- .scenarioConfigurationsToExcelDf(
      project$definitions$scenarios,
      outputPaths = project$definitions$outputPaths
    )
  }
  if (
    !is.null(project$definitions$outputPaths) &&
      length(project$definitions$outputPaths) > 0
  ) {
    scenSheets[["OutputPaths"]] <- data.frame(
      OutputPathId = names(project$definitions$outputPaths),
      OutputPath = unlist(project$definitions$outputPaths, use.names = FALSE),
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
  if (
    !is.null(project$definitions$applications) &&
      length(project$definitions$applications) > 0
  ) {
    appSheets[["ApplicationProtocols"]] <- .applicationsToExcelDf(
      project$definitions$applications
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
  dataCombined <- .unwrapDefinitionList(project$definitions$dataCombined)
  plots <- .unwrapDefinitionList(project$definitions$plots)
  plotGrids <- .unwrapDefinitionList(project$definitions$plotGrids)
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
  piTasks <- .unwrapDefinitionList(project$definitions$parameterIdentification)
  if (length(piTasks %||% list()) > 0) {
    piSheets <- .parameterIdentificationToExcelSheets(piTasks)
    .writeExcel(piSheets, file.path(configDir, "ParameterIdentification.xlsx"))
  }

  if (interactive() && !silent) {
    relPath <- fs::path_rel(projConfigPath, start = getwd())
    msg <- messages$restoredProjectConfiguration(
      project$info$projectFilePath %||% "Project",
      relPath
    )
    cli::cli_inform("{msg}")
  }

  invisible(projConfigPath)
}

# Compare an in-memory project against its Excel side-car and report whether
# they are in sync. Drives the Excel axis of `projectStatus()` (via
# `.projectSyncStatus()`), returning the
# `list(excel_in_sync = <logical>, details = <list>)` contract.
#
# The comparison is one-way and container-shape-independent: "would re-exporting
# the current in-memory project change the workbook?". Both sides are the same
# in-memory serialization (`.projectToJson()`) of a loaded project:
#   - memory side: the live `project`, so the comparison reflects the project as
#     it is now (unsaved edits included), not the on-disk container;
#   - Excel side: a fresh re-import of the side-car workbook, loaded back into a
#     `Project` and serialized the same way.
# Serializing both from an in-memory project (rather than reading a container
# file raw) is what makes the comparison container-shape-independent: a tree
# project's on-disk container keeps every section emptied (the tree owns them),
# so reading either container raw would blind the comparison and report false
# drift on every section. The volatile `esqlabsRVersion` is ignored.
#
# @keywords internal
# @noRd
.compareJsonToExcel <- function(project, projectConfigPath, silent = FALSE) {
  # Create temporary snapshot from current Excel files
  tempDir <- tempfile("config_snapshot")
  dir.create(tempDir, showWarnings = FALSE, recursive = TRUE)
  on.exit(unlink(tempDir, recursive = TRUE), add = TRUE)

  # `copyAssets = FALSE`: this snapshot exists only to be serialized and compared,
  # then deleted with `tempDir`. Copying the referenced models, data, and
  # population folders into it would turn every `projectStatus()` read into a
  # recursive copy of the whole asset tree, and a copy failure (a locked file, a
  # full disk) would surface as "cannot compare the Excel files".
  tempJsonPath <- importProjectFromExcel(
    projectConfigPath,
    outputDir = tempDir,
    silent = TRUE,
    copyAssets = FALSE
  )

  # The memory side: serialize the live in-memory project. The Excel side: load
  # the just-imported temp tree project and serialize it the same way. Both go
  # through `.projectToJson()`, so the two sides are compared in one shape
  # regardless of how either container is stored on disk. `Project$new()` (not
  # `loadProject()`) skips the cross-reference warning pass, so a project with
  # dangling refs compares quietly.
  originalJsonObj <- .projectToJson(project)
  currentJsonObj <- .projectToJson(Project$new(projectFilePath = tempJsonPath))

  # Both sides canonicalize every id (via `.canonicalizeProjectJsonIds()`) so id
  # canonicalization is never counted as drift (which would make an otherwise
  # in-sync project report out-of-sync). An already-canonical id is unchanged,
  # so this is a no-op for a canonical project. Warnings are suppressed (an
  # in-place re-canonicalization of an already-canonical id emits none anyway).
  originalJsonObj <- suppressWarnings(
    .canonicalizeProjectJsonIds(originalJsonObj)
  )
  currentJsonObj <- suppressWarnings(
    .canonicalizeProjectJsonIds(currentJsonObj)
  )

  # Both sides are compared with their object keys in one order, because the
  # order a record's fields sit in carries no meaning and the round trip does not
  # preserve it: a sheet has one column order (the union of every record's
  # fields), so a record whose own fields are ordered differently comes back
  # reordered. Only named lists are sorted; an array's order is meaningful (a
  # task's parameter list) and is left as it is.
  originalJsonObj <- .sortJsonKeys(originalJsonObj)
  currentJsonObj <- .sortJsonKeys(currentJsonObj)

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
        "{.run importProjectFromExcel(overwrite = TRUE)} - Update JSON from \\
        Excel files."
      )
      cli::cli_li(
        "{.run exportProjectToExcel(overwrite = TRUE)} - Recreate Excel files \\
        from JSON."
      )
      cli::cli_end()
    }
  }

  invisible(result)
}

# Sort the keys of every named list in a JSON-shaped structure, recursively, so
# two structures that differ only in field order compare equal. An unnamed list
# is an array, whose order is part of its meaning, so it is recursed into but not
# reordered. Sorted in the C locale so the order does not depend on the session's.
#
# @keywords internal
# @noRd
.sortJsonKeys <- function(x) {
  if (!is.list(x)) {
    return(x)
  }
  x <- lapply(x, .sortJsonKeys)
  names <- names(x)
  if (is.null(names) || anyNA(names) || !all(nzchar(names))) {
    return(x)
  }
  x[order(names, method = "radix")]
}

# Excel <-> JSON bridge: sync helper ----

#' Check a loaded project for unsaved changes and outdated Excel files
#'
#' @description Prints a report of how the `Project` in your R session
#'   compares to the files on disk, in two parts:
#'
#'   - project vs. saved files: whether the project carries changes that have
#'     not been saved with [saveProject()] yet. Reported as `NA` for a project
#'     that exists only in the R session, without a folder on disk.
#'   - project vs. Excel: when the project has a `Project.xlsx` Excel file,
#'     whether that file still matches the current project (one-way: would
#'     exporting again change it). Reported as `NA` when there is no Excel
#'     file or it cannot be read.
#'
#'   `projectStatus()` only reports; it never changes any files. To save your
#'   changes, call [saveProject()]; to bring the Excel files up to date, call
#'   [exportProjectToExcel()] or [importProjectFromExcel()].
#'
#' @param project A `Project` object.
#' @param silent Logical. If `TRUE`, suppresses the printed report and only
#'   returns the structured result (the same shape as `project$status`).
#'   Defaults to `FALSE`.
#'
#' @returns Invisibly, a `list(tree_in_sync, excel_in_sync, details)` (see
#'   the `status` field of [Project]).
#' @export
#' @family projectPersistence
#' @seealso [saveProject()], [reloadProject()], [exportProjectToExcel()],
#'   [importProjectFromExcel()].
#' @examples
#' \dontrun{
#' project <- loadProject("Project.json")
#' projectStatus(project) # readable report
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
  jsonPath <- project$info$projectFilePath
  if (is.null(jsonPath)) {
    result$tree_in_sync <- NA
  } else {
    result$tree_in_sync <- !project$isModified()
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
  excelPath <- file.path(dirname(jsonPath), .excelSideCarName(jsonPath))
  if (!file.exists(excelPath)) {
    if (!silent) {
      cli::cli_alert_info(messages$syncNoExcel(excelPath))
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
      project = project,
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
          "Cannot compare the Excel configuration files to the project.",
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
      names(section) <- .silentlyCanonicalized(.canonicalizeId(nms))
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
    canonEntryScenario <- function(entry) {
      entry$scenario <- canonScalar(entry$scenario)
      entry
    }
    jsonData$dataCombined <- lapply(
      jsonData$dataCombined,
      function(dc) {
        dc$dataCombinedId <- canonScalar(dc$dataCombinedId)
        # Both entry blocks may name a scenario, and an observed row's `scenario`
        # is the same kind of reference as a simulated row's, so it gets the same
        # transform: leaving it at the Excel spelling would put two casings of one
        # scenario in a single definition file, and any check keyed on the
        # canonical id would miss the observed block.
        if (!is.null(dc$simulated)) {
          dc$simulated <- lapply(dc$simulated, canonEntryScenario)
        }
        if (!is.null(dc$observed)) {
          dc$observed <- lapply(dc$observed, canonEntryScenario)
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

#' The name of the Excel side-car that pairs with a project file
#'
#' A project and its Excel side-car are paired by name: `MyStudy.json` pairs
#' with `MyStudy.xlsx`. Both halves derive it here, so
#' [exportProjectToExcel()] writes the workbook [projectStatus()] then looks
#' for. A project with no file on disk has no name to derive one from, so it
#' falls back to the canonical `Project.xlsx`.
#'
#' @param projectFilePath The project's `info$projectFilePath`, possibly `NULL`.
#' @returns The side-car's file name, as a single string.
#' @keywords internal
#' @noRd
.excelSideCarName <- function(projectFilePath) {
  if (is.null(projectFilePath)) {
    return("Project.xlsx")
  }
  as.character(fs::path_ext_set(fs::path_file(projectFilePath), "xlsx"))
}

#' The project file name with a `.json` extension
#'
#' Adds the extension when it is missing, and leaves a name that already ends in
#' `.json` alone. Everything before it is kept verbatim, so a dotted stem
#' (`trial.v1`) stays distinct from its siblings.
#'
#' @param name A project file name.
#' @returns The name, ending in `.json`.
#' @keywords internal
#' @noRd
.withJsonExtension <- function(name) {
  if (identical(tolower(fs::path_ext(name)), "json")) {
    return(name)
  }
  paste0(name, ".json")
}

#' Spell a path the way it reads best from the working directory
#'
#' A path under the working directory is clearest relative to it. A path
#' somewhere else (a temp folder, another drive) relativizes into a long
#' `../../..` climb that is harder to read than the absolute path, so take
#' whichever spelling is shorter.
#'
#' @param path A file or directory path.
#' @returns The path as a single string.
#' @keywords internal
#' @noRd
.readablePath <- function(path) {
  relative <- as.character(fs::path_rel(path, start = getwd()))
  absolute <- as.character(fs::path_abs(path))
  if (nchar(relative) <= nchar(absolute)) relative else absolute
}

#' Point `populationsFolder` at the folder that actually holds the csv files
#'
#' An Excel project spells `populationsFolder` as a folder *name* under the
#' configurations folder (`PopulationsCSV`, resolved against
#' `configurationsFolder`), while a project resolves every working folder against
#' the project root. Read literally, the value then names a root-level folder
#' that does not exist: the imported project resolves the populations folder
#' nowhere, and `.copyExcelProjectAssets()` finds nothing to copy, so a
#' csv-population scenario fails at run time with the folder left behind.
#'
#' Both layouts are accepted, resolved in the order the value itself suggests: a
#' folder present at the project root is taken as authored (a project already
#' following the root-level layout keeps working), otherwise the folder under the
#' configurations folder is adopted and the value rewritten to its
#' project-root-relative path. That one rewritten value then means the same thing
#' in the source project and in the imported one, which is the invariant the
#' asset copy relies on, so the copy needs no special case.
#'
#' @param filePathProps The project's raw `filePaths` properties.
#' @param pcDir Absolute path to the Excel project's folder.
#' @param configsFolder Absolute path to the resolved configurations folder, or
#'   `NULL` when the project declares none.
#' @returns `filePathProps`, with `populationsFolder` rewritten when the folder
#'   was found under the configurations folder rather than at the project root.
#' @keywords internal
#' @noRd
.resolveExcelPopulationsFolder <- function(
  filePathProps,
  pcDir,
  configsFolder
) {
  value <- filePathProps[["populationsFolder"]]
  if (
    is.null(value) ||
      is.na(value) ||
      !nzchar(value) ||
      is.null(configsFolder) ||
      # A `${VAR}` or an absolute value names a location the author placed
      # deliberately, and is resolved (or exempted) as spelled.
      .declaresEnvVarPath(value) ||
      fs::is_absolute_path(value)
  ) {
    return(filePathProps)
  }
  root <- normalizePath(pcDir, mustWork = FALSE)
  if (fs::dir_exists(fs::path(root, value))) {
    return(filePathProps)
  }
  underConfigs <- fs::path(
    normalizePath(configsFolder, mustWork = FALSE),
    value
  )
  if (!fs::dir_exists(underConfigs)) {
    return(filePathProps)
  }
  relative <- as.character(fs::path_rel(underConfigs, root))
  # A configurations folder placed outside the project (the `${VAR}` form) puts
  # the csv folder outside it too, which a working folder may not name. Leave the
  # value as authored and let the folder be reported as not copied.
  if (.pathEscapesRoot(relative, root)) {
    return(filePathProps)
  }
  filePathProps[["populationsFolder"]] <- relative
  filePathProps
}

#' The working folders whose contents an imported project needs to run
#'
#' The input folders a definition can reference: models (under either the current
#' `simulationsFolder` key or the pre-6.0.0 `modelFolder` an Excel project still
#' spells it with), observed data, and csv populations. `outputFolder` is
#' deliberately absent: it holds results the project writes, not inputs it reads.
#'
#' @keywords internal
#' @noRd
.excelProjectAssetFolders <- c(
  "simulationsFolder",
  "modelFolder",
  "dataFolder",
  "populationsFolder"
)

#' Copy an Excel project's referenced input folders next to the imported project
#'
#' A definition references a model, a data file, or a csv population by a path
#' relative to the project folder, so importing into a folder other than the
#' Excel project's own leaves every such reference dangling. Copying the
#' referenced folders to the same relative location under `outputDir` makes those
#' paths resolve again, which is what makes the imported project runnable rather
#' than a definitions tree pointing at files that are not there.
#'
#' Whole folders are copied rather than only the individually referenced files,
#' because a folder also holds assets nothing names statically (an importer
#' configuration, a population csv chosen at run time, a model a scenario is
#' added for later).
#'
#' @param filePathProps The project's raw `filePaths` properties (folder values
#'   exactly as the Excel file spells them).
#' @param sourceDir Absolute path to the Excel project's folder.
#' @param outputDir Directory the JSON project was written to.
#' @param overwrite Whether the import was allowed to replace existing content.
#'   With `FALSE`, a target folder that already holds files is left as it is and
#'   reported, so the flag governs the assets as well as the definition tree.
#' @returns `list(copied, notCopied)`: the folder values copied, and those a
#'   definition may reference but that could not be copied (absent from the
#'   source project, naming a location outside it, or already present in the
#'   output when `overwrite` is `FALSE`). Both empty when the project was
#'   imported in place (nothing to copy).
#' @keywords internal
#' @noRd
.copyExcelProjectAssets <- function(
  filePathProps,
  sourceDir,
  outputDir,
  overwrite = FALSE
) {
  result <- list(copied = character(), notCopied = character())
  # Imported in place: the folders are already where the definitions expect them.
  # Compared with `normalizePath()` rather than the lexical `fs::path_norm()`,
  # matching the containment code above, so the same directory reached by two
  # spellings (a symlinked root, a case-different drive letter) is recognized as
  # one and no folder is copied onto itself.
  sourceNorm <- normalizePath(sourceDir, mustWork = FALSE)
  if (sourceNorm == normalizePath(outputDir, mustWork = FALSE)) {
    return(result)
  }

  for (field in .excelProjectAssetFolders) {
    value <- filePathProps[[field]]
    if (is.null(value) || is.na(value) || !nzchar(value)) {
      next
    }
    # An absolute folder deliberately points outside the project: it resolves the
    # same from the new location, so copying it would duplicate data the author
    # chose to keep in one place. A `${VAR}` that expands to an absolute path is
    # the same case. One that expands to a relative path is not: a relative path
    # is resolved against the project file, so it names a folder inside the
    # project and has to travel with it, under the expanded name the loader will
    # look for. An unset variable expands to itself, matches no folder, and is
    # reported below rather than skipped in silence.
    copyAs <- value
    if (.declaresEnvVarPath(value)) {
      copyAs <- .replaceEnvVarPath(value)
    }
    if (fs::is_absolute_path(copyAs)) {
      next
    }
    # A `../`-climbing value names something the project does not own. Copying it
    # would read outside the source project and, worse, write outside
    # `outputDir`, so it is contained the same way every other author-controlled
    # path in this file is (`.resolveProjectPath()`) and reported rather than
    # copied.
    if (.pathEscapesRoot(copyAs, sourceDir)) {
      result$notCopied <- c(result$notCopied, value)
      next
    }
    from <- fs::path_norm(fs::path(sourceDir, copyAs))
    # A folder value of `"."` resolves to the project folder itself; copying that
    # would drag the whole Excel project (workbooks included) into the output.
    if (normalizePath(from, mustWork = FALSE) == sourceNorm) {
      next
    }
    if (!fs::dir_exists(from)) {
      result$notCopied <- c(result$notCopied, value)
      next
    }
    to <- fs::path(outputDir, copyAs)
    # A target folder the user already put files in is theirs, not the import's
    # to replace: `overwrite = FALSE` means it here too, so a curated model or
    # data file placed in the output beforehand survives and is reported instead
    # of being silently replaced.
    if (
      !overwrite &&
        fs::dir_exists(to) &&
        length(fs::dir_ls(to, all = TRUE)) > 0L
    ) {
      result$notCopied <- c(result$notCopied, value)
      next
    }
    fs::dir_create(fs::path_dir(to))
    fs::dir_copy(from, to, overwrite = TRUE)
    result$copied <- c(result$copied, value)
  }
  # `simulationsFolder` and `modelFolder` are two spellings of one folder, so a
  # project carrying both would report it twice.
  result$copied <- unique(result$copied)
  result$notCopied <- unique(result$notCopied)
  result
}

#' Append parsed parameter sheets to the accumulating `parameterSets` section
#'
#' The Excel layout spreads parameter sets over three workbooks (model
#' parameters, individuals, applications) that were three separate namespaces
#' before 6.0.0; they now share the single `parameterSets` namespace, so the same
#' sheet name in two workbooks would land on one id. The earlier workbook keeps
#' the plain id and the later sheet is renamed, since a definition tree keys one
#' file per id and cannot hold both.
#'
#' The uniquifying runs on the *canonical* id, not the raw sheet name: `Rat` and
#' `rat` in two workbooks are as much of a clash as `Rat` twice, because both
#' canonicalize to the same definition filename. The suffix `make.unique()` picks
#' is carried back onto the raw sheet name, so the renamed set keeps its readable
#' spelling (`Indiv1` -> `Indiv1_1`).
#'
#' Two sheets of the *same* workbook that canonicalize to one id are renamed the
#' same way. That is a deliberate divergence from `.canonicalizeId()`, which
#' treats one such pair as ambiguity and aborts: a legacy workbook cannot be
#' hand-edited retroactively, so an import renames and says so rather than
#' refusing the whole migration. The warning names neither an earlier workbook
#' nor a later one, since either sheet may be the one that lost the plain id.
#'
#' @param existing The `parameterSets` accumulated so far.
#' @param incoming Newly parsed sets, keyed by sheet name.
#' @param source Path to the workbook `incoming` was read from, named in the
#'   rename warning.
#' @returns `list(sets, renames)`: the merged section, and a named character
#'   vector mapping each renamed set's *canonical* id to its new raw id (empty
#'   when nothing clashed). The caller re-points the references that workbook
#'   itself makes with [.applyIdRenames()].
#' @keywords internal
#' @noRd
.appendParameterSets <- function(existing, incoming, source) {
  if (length(incoming) == 0L) {
    return(list(sets = existing, renames = character()))
  }
  ids <- c(names(existing), names(incoming))
  canonical <- vapply(ids, .canonicalizeOneId, character(1), USE.NAMES = FALSE)
  # `make.unique()` leaves each first occurrence untouched and suffixes the
  # later ones, and `existing` comes first, so an already-accumulated id is
  # never renamed out from under a reference that resolved to it.
  uniqued <- make.unique(canonical, sep = "_")
  isIncoming <- seq_along(ids) > length(existing)
  suffix <- substring(
    uniqued[isIncoming],
    nchar(canonical[isIncoming]) + 1L
  )
  newIds <- paste0(names(incoming), suffix)
  # Keyed by the canonical id, not the raw sheet name, because a reference is
  # only required to canonicalize onto its definition, not to match its
  # spelling: a cell naming `rat` for a sheet called `Rat` still points at that
  # sheet. `.applyIdRenames()` canonicalizes its lookups to match.
  renames <- stats::setNames(newIds, canonical[isIncoming])[nzchar(suffix)]

  if (length(renames) > 0L) {
    # Same safe-glue handling as the canonicalization warnings: the pairs are
    # bound as variables and the whole message is glue-parsed once, so a sheet
    # name containing `{` is never evaluated as a cli expression. The bullets
    # name the sheet as the workbook spells it, so the user can find it, rather
    # than the canonical id the map is keyed by.
    rendered <- .canonicalizedIdBullets(
      names(incoming)[nzchar(suffix)],
      unname(renames)
    )
    rendered$envir$sourceLabel <- basename(source)
    rendered$envir$renamedCount <- length(renames)
    # Classed so a caller that expects the rename (a test, or a migration script
    # that has already reported it) can muffle just this warning without
    # swallowing others.
    cli::cli_warn(
      messages$importRenamedDuplicateParameterSets(rendered$bullets),
      .envir = rendered$envir,
      class = "esqlabsR_importRenamedParameterSets"
    )
  }

  list(
    sets = c(existing, stats::setNames(incoming, newIds)),
    renames = renames
  )
}

#' Re-point parameter-set references through a rename map
#'
#' Applies the `renames` map [.appendParameterSets()] returned to a vector of
#' referenced ids, leaving an id that was not renamed untouched. Used on the
#' references a workbook makes into its *own* former namespace (an individual's
#' sheet link, an application's `ParameterSets` column), so a set that had to be
#' renamed is still reached by the workbook that owns it.
#'
#' The lookup is canonical on both sides, matching how the clash was detected in
#' the first place: a reference only has to canonicalize onto its definition, so a
#' cell spelling `indiv1` for a sheet named `Indiv1` is re-pointed too. Matching
#' the raw spellings instead would leave such a reference resolving to the
#' earlier workbook's set, which is the mis-resolution this whole path prevents.
#'
#' @param ids Referenced parameter-set ids (character vector).
#' @param renames Named character vector, canonical old id -> new raw id.
#' @returns `ids` with every renamed entry replaced.
#' @keywords internal
#' @noRd
.applyIdRenames <- function(ids, renames) {
  if (length(renames) == 0L || length(ids) == 0L) {
    return(ids)
  }
  ids <- as.character(ids)
  canonical <- vapply(ids, .canonicalizeOneId, character(1), USE.NAMES = FALSE)
  mapped <- unname(renames[canonical])
  ifelse(is.na(mapped), ids, mapped)
}

#' Drop references to sheets of this workbook that were not parameter sheets
#'
#' A definition's `parameterSets` cell names sheets of its own workbook, so a
#' sheet `.parseExcelParameterSheets()` skipped leaves a reference to a set that
#' was never created. Only those references are dropped: the same cell may name
#' a set defined in another workbook, which is a separate question answered
#' elsewhere. Matching is on the canonical id, since that is what both the
#' definition and the reference become.
#'
#' @param definitions Named list of records that may carry `parameterSets`.
#' @param skippedSheets Sheet names the parser skipped.
#' @returns `definitions`, each dangling reference removed and the field dropped
#'   entirely where nothing is left, so a definition that referenced only
#'   skipped sheets ends up without the field rather than with an empty one.
#' @keywords internal
#' @noRd
.dropSkippedSheetRefs <- function(definitions, skippedSheets) {
  if (length(definitions) == 0L || length(skippedSheets) == 0L) {
    return(definitions)
  }
  skipped <- vapply(skippedSheets, .canonicalizeOneId, character(1))
  lapply(definitions, function(definition) {
    refs <- unlist(definition$parameterSets)
    if (is.null(refs)) {
      return(definition)
    }
    dangling <- vapply(refs, .canonicalizeOneId, character(1)) %in% skipped
    kept <- refs[!dangling]
    definition$parameterSets <- if (length(kept) > 0L) as.list(kept) else NULL
    definition
  })
}

#' Parse parameter sheets from an Excel file into JSON structure
#'
#' A parameter workbook routinely carries a notes, organ-list, or fit-bounds
#' sheet beside its parameter sheets. Such a sheet is recognized by its columns
#' and skipped with a warning, so one of them does not stop a migration.
#'
#' A fit-bounds sheet authored by copying a real parameter sheet carries all four
#' columns, so it is a parameter sheet by the test above and its rows are read.
#' A row whose `Value` is text rather than a number (`lower`, `upper`) is skipped
#' with a warning naming the sheet, row and cell. Skipping the row rather than
#' the workbook is deliberate: nothing here can tell a deliberate note from a
#' typo in a real parameter, and the row is the smallest unit that can be lost.
#'
#' @param filePath Path to the Excel file
#' @param sheetNames Sheets to read. If NULL, reads all sheets.
#' @returns Named list of parameter arrays, keyed by sheet name and holding only
#'   the sheets that are parameter sheets. A caller that derives other
#'   definitions from these sheets (an application per protocol sheet, an
#'   individual's own override) must key them off these names rather than the
#'   workbook's, so nothing references a set that was skipped.
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
  skipped <- character()
  # Parallel accumulators for the skipped rows, so one warning covers the whole
  # workbook rather than one per row.
  badSheets <- character()
  badRows <- integer()
  badValues <- character()
  for (sheet in sheetNames) {
    df <- readExcel(filePath, sheet = sheet)
    # The check belongs to the sheet, not the cell: a missing column reads as
    # `NULL`, which the row loop below would turn into an empty `containerPath`
    # / `parameterName` and a value-less entry rather than an error, so guarding
    # per cell would trade a loud abort for a silently corrupted section.
    if (!all(.parameterSheetColumns %in% names(df))) {
      skipped <- c(skipped, sheet)
      next
    }
    # Where each kept row sits in the workbook, so a row reported to the user can
    # be found there. Blank rows were dropped on read, which makes `i` untrue as a
    # workbook coordinate; the fallback covers a frame with no such record.
    sheetRow <- attr(df, "sheetRow") %||% seq_len(nrow(df))
    entries <- list()
    if (nrow(df) > 0) {
      for (i in seq_len(nrow(df))) {
        value <- df[["Value"]][[i]]
        if (.isUnusableNumericCell(value)) {
          badSheets <- c(badSheets, sheet)
          # +1 for the header, so the number is the row Excel shows.
          badRows <- c(badRows, sheetRow[[i]] + 1L)
          badValues <- c(badValues, as.character(value))
          next
        }
        entry <- list(
          containerPath = as.character(df[["Container Path"]][[i]]),
          parameterName = as.character(df[["Parameter Name"]][[i]]),
          # A blank cell stays an absent value, as it was before the check above.
          value = if (.isBlankCell(value)) NA_real_ else as.numeric(value),
          units = if (is.na(df[["Units"]][[i]]) || df[["Units"]][[i]] == "") {
            NULL
          } else {
            as.character(df[["Units"]][[i]])
          }
        )
        entries[[length(entries) + 1L]] <- entry
      }
    }
    # A sheet that had rows but kept none describes no parameter at all, so it is
    # left out rather than becoming an empty parameter set (and, in the 5.x
    # applications layout, an application wrapping one). Both callers derive the
    # sheets they may reference from the names of what this returns, so omitting
    # it also unlinks the references to it. A header-only sheet is a different
    # thing, a real set that happens to be empty, and still comes through.
    if (nrow(df) > 0 && length(entries) == 0L) {
      next
    }
    result[[sheet]] <- entries
  }
  if (length(skipped) > 0L) {
    .warnFormatted(
      messages$importSkippedNonParameterSheets(
        filePath,
        skipped,
        .parameterSheetColumns
      ),
      "esqlabsR_importSkippedNonParameterSheets"
    )
  }
  if (length(badRows) > 0L) {
    .warnFormatted(
      messages$importSkippedNonNumericRows(
        filePath,
        badSheets,
        badRows,
        badValues
      ),
      "esqlabsR_importSkippedNonNumericRows"
    )
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
      # A blank unit cell defaults to the hour `addScenario()` also defaults to,
      # rather than null, so the same blank cell means one unit whichever
      # entrypoint wrote the project (and an imported scenario is byte-identical
      # to the same scenario authored through the API).
      simulationTimeUnit = .naToDefault(
        as.character(row[["SimulationTimeUnit"]]),
        "h"
      ),
      steadyState = .naToNull(.toLogical(row[["SteadyState"]], "SteadyState")),
      # A blank steady-state time/unit defaults to the same values the authoring
      # API and the legacy 5.x reader use (`1000` / `"min"`), rather than null, so
      # a project round-tripped through the authoring API is byte-identical to
      # the imported one. The value is only used when `steadyState` is TRUE.
      steadyStateTime = .naToDefault(
        as.numeric(row[["SteadyStateTime"]]),
        1000
      ),
      steadyStateTimeUnit = .naToDefault(
        as.character(row[["SteadyStateTimeUnit"]]),
        "min"
      ),
      # `OverwriteFormulasInSS` is a newer column; a pre-6.0 Scenarios sheet
      # omits it, so guard the lookup rather than abort on its absence (matching
      # `InitialConditions` above). An absent or blank value defaults to FALSE.
      overwriteFormulasInSS = .naToNull(
        .toLogical(
          if ("OverwriteFormulasInSS" %in% names(row)) {
            row[["OverwriteFormulasInSS"]]
          } else {
            NA
          },
          "OverwriteFormulasInSS"
        )
      ),
      modelFile = as.character(row[["ModelFile"]]),
      outputPaths = .parseCommaListToArray(row[["OutputPathsIds"]])
    )
    scenarios[[i]] <- scenario
  }
  scenarios
}

#' Report the imported data combinations whose observed curve names no data set
#'
#' `.parseExcelDataCombinedSheet()` builds an observed entry for every row the
#' sheet marked `observed`, whatever its other cells hold, so a blank (or absent)
#' `dataSet` column yields an entry with nothing to resolve against. The entry is
#' left as authored, since the user may still be able to fill the cell, and the
#' one thing this owes them is saying so: `validateProject()` reports each such
#' entry as a critical error, and without this the first sign of it is that error
#' on a project they have not touched yet.
#'
#' @param dataCombined The parsed `dataCombined` section (an unnamed list).
#' @returns Nothing, called for its warning.
#' @keywords internal
#' @noRd
.warnIncompleteObservedCurves <- function(dataCombined) {
  incomplete <- vapply(
    dataCombined %||% list(),
    function(dc) {
      any(vapply(
        dc$observed %||% list(),
        function(entry) .isBlankCell(entry$dataSet),
        logical(1)
      ))
    },
    logical(1)
  )
  if (!any(incomplete)) {
    return(invisible(NULL))
  }
  ids <- vapply(
    dataCombined[incomplete],
    function(dc) as.character(dc$dataCombinedId),
    character(1)
  )
  .warnFormatted(
    messages$importIncompleteObservedCurves(ids),
    "esqlabsR_importIncompleteObservedCurves"
  )
}

#' Report an unreachable observed-data path and import no observed data
#'
#' Carries the same condition class as the missing-data-file warning, so the
#' caller that expects no observed data (the legacy-snapshot upgrade, which
#' never ships the data workbook) muffles both with one handler.
#'
#' @param fieldName Which boundary was crossed: `"dataFolder"` (outside the
#'   project) or `"dataFile"` (outside the data folder).
#' @param jsonData The accumulating project JSON list, returned unchanged.
#' @returns `jsonData`.
#' @keywords internal
#' @noRd
.skipOutOfProjectObservedData <- function(fieldName, jsonData) {
  cli::cli_warn(
    switch(
      fieldName,
      dataFolder = messages$importSkippedOutOfProjectDataFolder(),
      dataFile = messages$importSkippedOutOfProjectDataFile()
    ),
    class = "esqlabsR_importSkippedObservedData"
  )
  jsonData
}

#' Reify the project's experimental-data file as an observed-data definition
#'
#' The project configuration records a single `dataFile` under `dataFolder`
#' (optionally with a `dataImporterConfigurationFile`). This builds one `excel`
#' observed-data entry from it, keyed by the data-file basename and listing the
#' workbook's sheets, so a plot or PI mapping that references observed data has
#' something to resolve against. `file` and `importerConfiguration` are stored as
#' basenames; the loader resolves them under `dataFolder`. A no-op when no
#' `dataFile` is configured, or its workbook is absent, or `dataFolder` /
#' `dataFile` points outside the project (with a warning in the latter two
#' cases, since data the imported project cannot reach is a migration gap the
#' user should see). A `dataFolder` naming a `${VAR}` is the sanctioned way to
#' keep the data outside the project, so it is expanded and imported normally.
#'
#' @param jsonData The accumulating project JSON list.
#' @param prop The `Property -> Value` lookup closure from the importer.
#' @param pcDir Absolute path to the project-configuration directory.
#' @param projectDir Directory the imported `Project.json` is written to. A
#'   relative path is resolved against the project file, so a `${VAR}` that
#'   expands to one is anchored here rather than at the Excel source, matching
#'   how the loader (`Project$.resolveWorkingFolder()`) will resolve the very
#'   same stored value.
#' @returns `jsonData` with an `observedData` section added when applicable.
#' @keywords internal
#' @noRd
.parseExcelObservedData <- function(jsonData, prop, pcDir, projectDir = pcDir) {
  dataFile <- prop("dataFile")
  if (is.null(dataFile) || is.na(dataFile) || dataFile == "") {
    return(jsonData)
  }
  dataFolderRaw <- prop("dataFolder") %||% "."
  dataFolder <- if (.declaresEnvVarPath(dataFolderRaw)) {
    # A `${VAR}` is the sanctioned way to keep the data outside the project (a
    # synced drive shared between projects), so it is expanded and exempt from
    # containment. Only the raw `${VAR}` is stored, so the same value is
    # expanded again on every load: a relative expansion must therefore be
    # anchored to the project file, not to the Excel source it was read from, or
    # the folder found here would not be the folder found after the import.
    expanded <- .replaceEnvVarPath(dataFolderRaw)
    if (fs::is_absolute_path(expanded)) {
      expanded
    } else {
      file.path(projectDir, expanded)
    }
  } else if (.pathEscapesRoot(dataFolderRaw, pcDir)) {
    # An out-of-project `dataFolder` (typically a 5.x project whose observed
    # data was shared through a synced drive) leaves the data unavailable to the
    # new project, which is the missing-data-file situation from the user's
    # point of view, so it is reported and skipped rather than aborting a
    # migration that has nothing else wrong with it. This is a read path only;
    # a path the project writes to is still contained.
    return(.skipOutOfProjectObservedData("dataFolder", jsonData))
  } else {
    .resolveProjectPath(dataFolderRaw, pcDir, "dataFolder")
  }
  if (.pathEscapesRoot(dataFile, dataFolder)) {
    return(.skipOutOfProjectObservedData("dataFile", jsonData))
  }
  dataFilePath <- .absoluteAgainstRoot(
    dataFile,
    as.character(fs::path_abs(dataFolder))
  )
  if (!file.exists(dataFilePath)) {
    # Classed so a caller that expects a missing data file (the legacy-snapshot
    # upgrade, which never carries the data workbook) can muffle just this
    # warning without swallowing others.
    cli::cli_warn(
      messages$importSkippedObservedData(dataFile),
      class = "esqlabsR_importSkippedObservedData"
    )
    return(jsonData)
  }

  importerConfig <- prop("dataImporterConfigurationFile")
  # `file` / `importerConfiguration` are stored as given (relative to
  # `dataFolder`), not truncated to a basename: the loader resolves them under
  # `dataFolder` (`.resolveDataPath()`), so a file in a subfolder would be lost
  # if only its basename were kept. Only the section key is reduced to a
  # basename, since an id becomes a single filename segment and cannot hold a
  # path separator.
  entry <- list(
    type = "excel",
    file = dataFile,
    sheets = as.list(readxl::excel_sheets(dataFilePath))
  )
  if (
    !is.null(importerConfig) && !is.na(importerConfig) && importerConfig != ""
  ) {
    entry$importerConfiguration <- importerConfig
  }
  jsonData$observedData <- stats::setNames(
    list(entry),
    basename(dataFile)
  )
  jsonData
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
  .requireExcelColumns(
    indivDf,
    c("IndividualId", "Species", "Population"),
    "IndividualBiometrics"
  )
  individuals <- list()
  hasParameterSets <- "ParameterSets" %in% names(indivDf)
  for (i in seq_len(nrow(indivDf))) {
    row <- indivDf[i, ]
    # Cells are read with `[[`, not `$`: a sheet parsed by `readExcel()` is a
    # tibble, whose `$` warns about an unknown column, so a workbook omitting an
    # optional column (`Protein Ontogenies` on an individual with no ontogenies,
    # a biometric a legacy sheet never carried) would leak a raw tibble warning
    # to the user. `[[` yields `NULL` for an absent column, which `.naToNull()`
    # turns into an absent field. Matches `.parseExcelScenarios()` below.
    indiv <- list(
      individualId = as.character(row[["IndividualId"]]),
      species = as.character(row[["Species"]]),
      population = as.character(row[["Population"]]),
      # A blank Gender cell defaults to UNKNOWN (the only valid PK-Sim gender
      # for some animal species). A whitespace-only or empty-string cell counts
      # as blank too, not just an NA.
      gender = .blankToDefault(as.character(row[["Gender"]]), "UNKNOWN"),
      weight = .naToNull(as.numeric(row[["Weight [kg]"]])),
      height = .naToNull(as.numeric(row[["Height [cm]"]])),
      age = .naToNull(as.numeric(row[["Age [year(s)]"]])),
      proteinOntogenies = .excelProteinOntogenies(
        row,
        "individual",
        as.character(row[["IndividualId"]])
      )
    )
    if (hasParameterSets) {
      raw <- row[["ParameterSets"]]
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
  .requireExcelColumns(
    popDf,
    c("PopulationName", "species", "population"),
    "Demographics"
  )
  populations <- list()
  for (i in seq_len(nrow(popDf))) {
    row <- popDf[i, ]
    # Cells are read with `[[` rather than `$`, for the reason given in
    # `.parseExcelIndividuals()` above: every demographic here is optional, and
    # `$` on a parsed sheet warns about a column the workbook omits.
    pop <- list(
      populationId = as.character(row[["PopulationName"]]),
      species = as.character(row[["species"]]),
      population = as.character(row[["population"]]),
      numberOfIndividuals = .naToNull(as.numeric(row[["numberOfIndividuals"]])),
      proportionOfFemales = .naToNull(as.numeric(row[["proportionOfFemales"]])),
      weightMin = .naToNull(as.numeric(row[["weightMin"]])),
      weightMax = .naToNull(as.numeric(row[["weightMax"]])),
      weightUnit = .naToNull(as.character(row[["weightUnit"]])),
      heightMin = .naToNull(as.numeric(row[["heightMin"]])),
      heightMax = .naToNull(as.numeric(row[["heightMax"]])),
      heightUnit = .naToNull(as.character(row[["heightUnit"]])),
      ageMin = .naToNull(as.numeric(row[["ageMin"]])),
      ageMax = .naToNull(as.numeric(row[["ageMax"]])),
      BMIMin = .naToNull(as.numeric(row[["BMIMin"]])),
      BMIMax = .naToNull(as.numeric(row[["BMIMax"]])),
      BMIUnit = .naToNull(as.character(row[["BMIUnit"]])),
      proteinOntogenies = .excelProteinOntogenies(
        row,
        "population",
        as.character(row[["PopulationName"]])
      )
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
      fields[[field]] <- .plotFieldValue(field, val)
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

# The plot / plot-grid fields that carry numbers rather than text, the field-type
# contract `.dataCombinedNumericFields` gives the sibling section. A
# hand-maintained workbook routinely stores one of these as text (`nsd` typed as
# `"1.96"`), which readxl reads as a string, so the imported definition would hold
# a string where the same field authored with `addPlot()` holds a number.
#
# Several of them are multi-value fields both entrypoints keep as one
# comma-separated string, so the coercion is conditional on the cell holding a
# single number (`.plotFieldValue()`): `"0, 24"` stays the string it is.
.plotNumericFields <- c(
  "nsd",
  "quantiles",
  "foldDistance",
  "xValuesLimits",
  "yValuesLimits",
  "xAxisLimits",
  "yAxisLimits"
)

# One plots cell, coerced to a number when the field is a numeric one and the
# cell holds a single number. Anything else (text, a multi-value cell, a value
# already numeric) is returned as read.
#
# @keywords internal
# @noRd
.plotFieldValue <- function(field, value) {
  if (!(field %in% .plotNumericFields) || is.numeric(value)) {
    return(value)
  }
  numeric <- suppressWarnings(as.numeric(value))
  if (length(numeric) == 1L && !is.na(numeric)) numeric else value
}

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
    # sheet that already uses `plots` supplies that column instead.
    if ("plotIDs" %in% names(df)) {
      plotIds <- .naToNull(df$plotIDs[[i]])
      if (!is.null(plotIds)) {
        fields$plots <- plotIds
      }
    }
    # A grid's membership is a multi-value cell, so decode it with the Excel
    # comma-list convention (`"A", "B, with comma"` as well as the backslash
    # escaping this package writes) and re-encode it into the single escaped
    # string the `plots` JSON key holds. Reading it with `.splitPlotIDs()`
    # instead would keep a quoted cell's `"` characters, which canonicalization
    # then turns into `_`, so every member of a quoted grid dangles.
    if (!is.null(fields$plots)) {
      fields$plots <- .joinPlotIDs(
        .parseCommaListToArray(as.character(fields$plots))
      )
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
    flatConfig <- .flattenPIConfiguration(task$configuration)
    for (key in names(flatConfig)) {
      taskRow[[paste0("config.", key)]] <- flatConfig[[key]] %||% NA
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
# @param scenarios The already-parsed `scenarios` section, needed only by the
#   5.x layout, whose oldest revision identifies a mapping's output through the
#   scenario rather than in the mapping row.
#
# @keywords internal
# @noRd
.parseExcelParameterIdentification <- function(piFile, scenarios = NULL) {
  sheets <- readxl::excel_sheets(piFile)
  # Two layouts. The newer one has a single `PITasks` sheet (one row per task,
  # config inline in `config.*` columns). The 5.x layout has no `PITasks` sheet
  # and instead keys every sheet by a `PITaskName` column, with the
  # configuration split across `PIConfiguration` / `AlgorithmOptions` /
  # `CIOptions`. Dispatch on which is present.
  if (!("PITasks" %in% sheets)) {
    return(.parseExcelPI5x(piFile, sheets, scenarios))
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
    flatConfig <- list()
    for (col in configCols) {
      val <- .naToNull(taskDf[[col]][[i]])
      if (!is.null(val)) {
        flatConfig[[sub("^config\\.", "", col)]] <- val
      }
    }
    configuration <- .nestPIConfiguration(flatConfig)
    list(
      id = taskId,
      scenarios = as.list(.parseCommaListToArray(taskDf$scenarios[[i]])),
      parameters = .parseExcelPIRows(paramDf, taskId, "parameter"),
      outputMappings = .parseExcelPIRows(mappingDf, taskId, "mapping"),
      configuration = configuration
    )
  })
}

# The nested groups of a PI task's `configuration`: each holds its own option
# dict rather than a scalar, so flattening the configuration onto sheet columns
# has to carry the group name along and importing has to put it back.
#
# @keywords internal
# @noRd
.piConfigurationGroups <- c(
  "objectiveFunction",
  "algorithmOptions",
  "ciOptions",
  "simulationRunOptions"
)

# Flatten a PI task's `configuration` to the one-scalar-per-column shape a sheet
# can hold: a scalar setting keeps its name, an option inside a nested group
# becomes `<group>.<option>`. `.nestPIConfiguration()` inverts it, so a
# configuration survives an export and re-import as the nested object it is.
#
# @keywords internal
# @noRd
.flattenPIConfiguration <- function(configuration) {
  flat <- list()
  for (key in names(configuration %||% list())) {
    value <- configuration[[key]]
    if (is.list(value)) {
      for (option in names(value)) {
        flat[[paste0(key, ".", option)]] <- value[[option]]
      }
    } else {
      flat[[key]] <- value
    }
  }
  flat
}

# Rebuild the nested `configuration` object from the flat `config.*` cells
# `.flattenPIConfiguration()` wrote. Only a name whose first segment is one of the
# nested groups is split, and only on its first dot, so an option name that
# itself contains a dot stays one name.
#
# @keywords internal
# @noRd
.nestPIConfiguration <- function(flat) {
  configuration <- list()
  for (name in names(flat)) {
    group <- sub("\\..*$", "", name)
    if (group %in% .piConfigurationGroups && grepl(".", name, fixed = TRUE)) {
      option <- sub("^[^.]*\\.", "", name)
      configuration[[group]][[option]] <- flat[[name]]
    } else {
      configuration[[name]] <- flat[[name]]
    }
  }
  configuration
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

# Parse the legacy 5.x parameter-identification layout: no `PITasks` sheet, but
# `PIParameters` / `PIOutputMappings` / `PIConfiguration` / `AlgorithmOptions` /
# `CIOptions` sheets each keyed by a `PITaskName` column. One task is built per
# distinct `PITaskName`, gathering its parameters, output mappings, and
# configuration from those sheets, producing the same
# `{id, scenarios[], parameters[], outputMappings[], configuration}` shape the
# newer layout does. Returns an unnamed list of PITask JSON objects (empty when
# no `PITaskName`-keyed sheet is present).
#
# @keywords internal
# @noRd
.parseExcelPI5x <- function(piFile, sheets, scenarios = NULL) {
  read5xSheet <- function(name) {
    if (name %in% sheets) readExcel(piFile, sheet = name) else NULL
  }
  paramDf <- read5xSheet("PIParameters")
  mappingDf <- read5xSheet("PIOutputMappings")
  configDf <- read5xSheet("PIConfiguration")
  algDf <- read5xSheet("AlgorithmOptions")
  ciDf <- read5xSheet("CIOptions")

  taskNames <- unique(unlist(lapply(
    list(mappingDf, paramDf, configDf, algDf, ciDf),
    function(df) {
      if (!is.null(df) && "PITaskName" %in% names(df)) {
        as.character(df$PITaskName)
      }
    }
  )))
  taskNames <- taskNames[!is.na(taskNames)]
  if (length(taskNames) == 0L) {
    return(list())
  }

  lapply(taskNames, function(task) {
    params <- .parseExcelPI5xParams(paramDf, task)
    mappings <- .parseExcelPI5xMappings(mappingDf, task, scenarios)
    # A task's scenarios are the union of the scenarios its parameters and
    # mappings reference (the 5.x layout has no separate task-scenario list).
    # Named apart from the `scenarios` formal, which holds the project's scenario
    # records: one name for both would resolve correctly only as long as nobody
    # moves the line above.
    taskScenarios <- unique(unlist(lapply(
      c(params, mappings),
      function(x) unlist(x$scenarios)
    )))
    list(
      id = task,
      scenarios = as.list(taskScenarios),
      parameters = params,
      outputMappings = mappings,
      configuration = .parseExcelPI5xConfig(configDf, algDf, ciDf, task)
    )
  })
}

# Build one task's `parameters` from the 5.x `PIParameters` rows. Maps the
# `Container Path` + `Parameter Name` columns to the flat `path`, and coins a
# per-parameter `id` from the parameter name (de-duplicated within the task).
#
# The rows sharing a `Group` describe ONE free parameter estimated across several
# scenarios, not one parameter each (see `.pi5xParameterGroups()`), so such rows
# become a single record whose `scenarios` is the union of theirs.
#
# @keywords internal
# @noRd
.parseExcelPI5xParams <- function(df, task) {
  rows <- .pi5xTaskRows(df, task)
  ids <- character()
  lapply(.pi5xParameterGroups(rows, task), function(group) {
    row <- rows[group[[1]], ]
    id <- .pi5xUniqueId(as.character(row[["Parameter Name"]]), ids)
    ids[[length(ids) + 1L]] <<- id
    scenarios <- unique(unlist(lapply(group, function(i) {
      .parseCommaListToArray(rows[i, ][["Scenarios"]])
    })))
    .dropNulls(list(
      id = id,
      scenarios = as.list(scenarios),
      path = .pi5xPath(row[["Container Path"]], row[["Parameter Name"]]),
      units = .naToNull(as.character(row[["Units"]])),
      minValue = .naToNull(as.numeric(row[["MinValue"]])),
      maxValue = .naToNull(as.numeric(row[["MaxValue"]])),
      startValue = .naToNull(as.numeric(row[["StartValue"]]))
    ))
  })
}

# Which rows of a task's 5.x `PIParameters` sheet describe one free parameter.
#
# The sheet's `Group` column is what makes several rows one optimisation variable:
# rows sharing a group, a container path and a parameter name are the same
# parameter estimated across the scenarios they name between them, which is how
# the identification was set up and therefore what it estimates. A blank `Group`
# is its own parameter, and two rows in one group at different paths are
# different parameters, matching how esqlabsR 5.x read the sheet.
#
# The bounds have to agree inside a group, since one parameter has one set of
# them. 5.x refused to build such a task at all; here the rows are left
# unmerged (one parameter each, so nothing is invented and nothing is lost) and
# named in a warning, so the rest of a project with one bad group still migrates.
#
# @param rows One task's rows of the sheet.
# @param task The task id, for the warning.
# @returns A list of integer vectors of row indices, one per parameter.
# @keywords internal
# @noRd
.pi5xParameterGroups <- function(rows, task) {
  if (nrow(rows) == 0L) {
    return(list())
  }
  cell <- function(column, i) as.character(.cellValue(rows, column, i))
  # `\r` cannot occur in an Excel cell, so it separates the key parts without a
  # path or a name that contains the separator merging two distinct parameters.
  keys <- vapply(
    seq_len(nrow(rows)),
    function(i) {
      group <- .cellValue(rows, "Group", i)
      if (.isBlankCell(group)) {
        return(paste0("\r", i))
      }
      paste(
        as.character(group),
        cell("Container Path", i),
        cell("Parameter Name", i),
        sep = "\r"
      )
    },
    character(1)
  )
  boundsOf <- function(i) {
    vapply(
      c("MinValue", "MaxValue", "StartValue"),
      function(column) {
        suppressWarnings(as.numeric(.cellValue(rows, column, i)))
      },
      numeric(1)
    )
  }

  groups <- list()
  mismatched <- character()
  for (key in unique(keys)) {
    members <- which(keys == key)
    bounds <- boundsOf(members[[1]])
    agree <- all(vapply(
      members,
      function(i) isTRUE(all.equal(bounds, boundsOf(i))),
      logical(1)
    ))
    if (agree) {
      groups[[length(groups) + 1L]] <- members
      next
    }
    mismatched <- c(
      mismatched,
      .pi5xPath(
        cell("Container Path", members[[1]]),
        cell("Parameter Name", members[[1]])
      ) %||%
        cell("Parameter Name", members[[1]])
    )
    for (i in members) {
      groups[[length(groups) + 1L]] <- i
    }
  }

  if (length(mismatched) > 0L) {
    .warnFormatted(
      messages$importUnmergedPIParameterGroups(task, mismatched),
      "esqlabsR_importUnmergedPIParameterGroups"
    )
  }
  groups
}

# Build one task's `outputMappings` from the 5.x `PIOutputMappings` rows. Coins
# a per-mapping `id` from the output path's last segment (de-duplicated within
# the task) and maps the offset/factor/weight columns.
#
# The sheet's oldest revision has no `OutputPath` column at all: it predates the
# column, and identified a mapping's output through the scenario, one mapping per
# output path the scenario declares. `.pi5xDerivedMappingRows()` reproduces that,
# so such a workbook restores instead of failing on every mapping.
#
# A blank cell in a column that IS present is a different thing, an authoring gap
# in the newer layout rather than an older schema, so it is left alone: the mapping
# keeps no `outputPath` and `validateProject()` reports it. The two cases are told
# apart by the column, not the cell, and only the column-less one is recovered.
#
# @keywords internal
# @noRd
.parseExcelPI5xMappings <- function(df, task, scenarios = NULL) {
  rows <- .pi5xTaskRows(df, task)
  if (nrow(rows) > 0L && !("OutputPath" %in% names(rows))) {
    rows <- .pi5xDerivedMappingRows(rows, task, scenarios)
  }
  ids <- character()
  lapply(seq_len(nrow(rows)), function(i) {
    row <- rows[i, ]
    outputPath <- as.character(row[["OutputPath"]])
    id <- .pi5xUniqueId(sub(".*\\|", "", outputPath), ids)
    ids[[length(ids) + 1L]] <<- id
    .dropNulls(list(
      id = id,
      scenarios = as.list(.parseCommaListToArray(row[["Scenarios"]])),
      outputPath = .naToNull(outputPath),
      observedData = .naToNull(as.character(row[["DataSet"]])),
      scaling = .naToNull(as.character(row[["Scaling"]])),
      xOffset = .naToNull(as.numeric(row[["xOffset"]])),
      yOffset = .naToNull(as.numeric(row[["yOffset"]])),
      xFactor = .naToNull(as.numeric(row[["xFactor"]])),
      yFactor = .naToNull(as.numeric(row[["yFactor"]])),
      weight = .naToNull(as.numeric(row[["Weight"]]))
    ))
  })
}

# Give the rows of an `OutputPath`-less `PIOutputMappings` sheet the column they
# lack, by taking each row's outputs from the scenarios it names.
#
# One row becomes one row per output path its scenarios declare, carrying the
# scenarios that declare that path and every other cell of the original row. A row
# that names no scenario, or whose scenarios declare no output path, still yields
# one row, with no path: nothing here can identify its output, and the mapping is
# better kept for `validateProject()` to report than dropped from a task the user
# may be able to complete. Those rows are named in one warning per task, which can
# say which cell to fill and where, as validation cannot.
#
# @param rows One task's rows of the sheet.
# @param task The task id, for the warning.
# @param scenarios The parsed `scenarios` section (an unnamed list of records
#   carrying `name` and `outputPaths`), or NULL when the project has none.
# @returns A data frame with the same columns plus `OutputPath`, one row per
#   (row, output path) pair, plus one `NA`-path row per input row no path could be
#   derived for.
# @keywords internal
# @noRd
.pi5xDerivedMappingRows <- function(rows, task, scenarios) {
  # Matched on the canonical id, as every other cross-sheet reference in this
  # import is: the two sheets are hand-maintained separately, so one spelling a
  # scenario `aciclovir_iv` and the other `Aciclovir_IV` is ordinary. Comparing
  # the raw text would resolve nothing and then blame the scenario for having no
  # output path, which is the one wrong thing to tell the user.
  canonical <- vapply(
    scenarios %||% list(),
    function(scenario) .canonicalizeOneId(as.character(scenario$name)),
    character(1)
  )
  outputPathsOf <- function(name) {
    match <- which(canonical == .canonicalizeOneId(name))
    if (length(match) == 0L) {
      return(NULL)
    }
    unlist(scenarios[[match[[1]]]]$outputPaths)
  }

  derived <- list()
  # The scenario cells of the rows no path could be derived for: that cell is what
  # the user has to look at to fix it.
  unresolved <- character()
  for (i in seq_len(nrow(rows))) {
    named <- .parseCommaListToArray(rows[i, ][["Scenarios"]])
    # Which of this row's scenarios declare each output path, so a path shared by
    # two of them becomes one mapping naming both rather than two mappings.
    byPath <- list()
    for (name in named %||% character()) {
      for (path in outputPathsOf(name)) {
        byPath[[path]] <- c(byPath[[path]], name)
      }
    }
    if (length(byPath) == 0L) {
      # Reported but kept, as `NA`: the mapping loads without an output path and
      # `validateProject()` names it, rather than the row disappearing from a task
      # the user may well be able to complete. The warning is here all the same,
      # because it can say what to fill in and where, which validation cannot.
      unresolved <- c(
        unresolved,
        if (length(named) == 0L) "" else paste(named, collapse = ", ")
      )
      byPath <- stats::setNames(list(named), NA_character_)
    }
    for (path in names(byPath)) {
      row <- rows[i, , drop = FALSE]
      row[["OutputPath"]] <- path
      row[["Scenarios"]] <- .formatArrayToCommaList(byPath[[path]])
      derived[[length(derived) + 1L]] <- row
    }
  }

  if (length(unresolved) > 0L) {
    .warnFormatted(
      messages$importIncompletePIOutputMappings(task, unresolved),
      "esqlabsR_importIncompletePIOutputMappings"
    )
  }
  if (length(derived) == 0L) {
    return(rows[0, , drop = FALSE])
  }
  as.data.frame(dplyr::bind_rows(derived))
}

# Build one task's nested `configuration` from the 5.x `PIConfiguration` row and
# the `AlgorithmOptions` / `CIOptions` option rows. Column-to-field mapping
# mirrors `.buildPIConfiguration()`'s accepted keys; absent values are omitted.
#
# @keywords internal
# @noRd
.parseExcelPI5xConfig <- function(configDf, algDf, ciDf, task) {
  configuration <- list()
  configRows <- .pi5xTaskRows(configDf, task)
  if (nrow(configRows) > 0L) {
    row <- configRows[1, ]
    scalar <- list(
      algorithm = .naToNull(as.character(row[["Algorithm"]])),
      ciMethod = .naToNull(as.character(row[["CIMethod"]])),
      autoEstimateCI = .naToNull(
        .toLogical(row[["AutoEstimateCI"]], "AutoEstimateCI")
      ),
      printEvaluationFeedback = .naToNull(
        .toLogical(row[["PrintEvaluationFeedback"]], "PrintEvaluationFeedback")
      )
    )
    for (nm in names(scalar)) {
      if (!is.null(scalar[[nm]])) configuration[[nm]] <- scalar[[nm]]
    }

    objective <- .dropNulls(list(
      type = .naToNull(as.character(row[["ObjectiveFunctionType"]])),
      residualWeightingMethod = .naToNull(
        as.character(row[["ResidualWeightingMethod"]])
      ),
      robustMethod = .naToNull(as.character(row[["RobustMethod"]])),
      scaleVar = .naToNull(as.character(row[["ScaleVar"]])),
      linScaleCV = .naToNull(as.numeric(row[["LinScaleCV"]])),
      logScaleSD = .naToNull(as.numeric(row[["LogScaleSD"]]))
    ))
    if (length(objective) > 0L) {
      configuration$objectiveFunction <- objective
    }

    runOptions <- .dropNulls(list(
      numberOfCores = .naToNull(as.numeric(row[["numberOfCores"]])),
      checkForNegativeValues = .naToNull(
        .toLogical(row[["checkForNegativeValues"]], "checkForNegativeValues")
      )
    ))
    if (length(runOptions) > 0L) {
      configuration$simulationRunOptions <- runOptions
    }
  }

  algorithmOptions <- .pi5xOptionRows(algDf, task)
  if (length(algorithmOptions) > 0L) {
    configuration$algorithmOptions <- algorithmOptions
  }
  ciOptions <- .pi5xOptionRows(ciDf, task)
  if (length(ciOptions) > 0L) {
    configuration$ciOptions <- ciOptions
  }
  configuration
}

# The rows of a 5.x PI sheet belonging to one task (empty data frame when the
# sheet is absent or has no matching rows).
#
# @keywords internal
# @noRd
.pi5xTaskRows <- function(df, task) {
  if (is.null(df)) {
    return(data.frame())
  }
  if (!("PITaskName" %in% names(df)) || nrow(df) == 0L) {
    return(df[0, , drop = FALSE])
  }
  df[
    !is.na(df$PITaskName) & as.character(df$PITaskName) == task,
    ,
    drop = FALSE
  ]
}

# The `OptionName` -> `OptionValue` rows of an AlgorithmOptions / CIOptions sheet
# for one task, as a named list (empty when none). A numeric-looking value is
# stored as a number, else as its string.
#
# @keywords internal
# @noRd
.pi5xOptionRows <- function(df, task) {
  rows <- .pi5xTaskRows(df, task)
  if (nrow(rows) == 0L) {
    return(list())
  }
  options <- list()
  for (i in seq_len(nrow(rows))) {
    name <- .cellValue(rows, "OptionName", i)
    if (.isBlankCell(name)) {
      next
    }
    name <- as.character(name)
    raw <- .cellValue(rows, "OptionValue", i)
    numeric <- suppressWarnings(as.numeric(raw))
    token <- tolower(trimws(as.character(raw)))
    options[[name]] <- if (!is.na(numeric)) {
      # A numeric value stays numeric, so a `1`/`0` option is not misread as a
      # boolean.
      numeric
    } else if (token %in% c("true", "false")) {
      # An explicit boolean-string flag (a common 5.x option encoding) becomes a
      # real logical, not the literal string a downstream consumer would reject.
      token == "true"
    } else {
      as.character(raw)
    }
  }
  options
}

# Join a container path and parameter name into the flat `path` the PI
# definition uses (`<container>|<parameter>`); a blank container yields just the
# parameter name. A blank/NA parameter name yields NULL (no meaningful path),
# so the caller drops the field and validation flags the incomplete parameter
# rather than a `"<container>|NA"` path that looks valid but references nothing.
#
# @keywords internal
# @noRd
.pi5xPath <- function(containerPath, parameterName) {
  if (.isBlankCell(parameterName)) {
    return(NULL)
  }
  parameter <- as.character(parameterName)
  if (.isBlankCell(containerPath)) {
    return(parameter)
  }
  paste(as.character(containerPath), parameter, sep = "|")
}

# Coin an id unique within `existing` by suffixing `_2`, `_3`, ... on a clash.
# The 5.x PI sheets carry no id column, so parameters and mappings need a
# synthesised id. It is canonicalized here (the sheets derive it from a
# parameter name or a path segment, which carry spaces and mixed case) so the
# coined id is a clean, stable single segment.
#
# @keywords internal
# @noRd
.pi5xUniqueId <- function(base, existing) {
  base <- if (.isBlankCell(base)) "item" else as.character(base)
  base <- .canonicalizeOneId(base)
  candidate <- base
  n <- 1L
  while (candidate %in% existing) {
    n <- n + 1L
    candidate <- paste0(base, "_", n)
  }
  candidate
}

# Drop the NULL elements of a list, so an optional field left NULL stays absent
# from the JSON rather than serialising as null.
#
# @keywords internal
# @noRd
.dropNulls <- function(x) {
  x[!vapply(x, is.null, logical(1))]
}

# Rewrite each PI output mapping whose `outputPath` is a full OSPS path (rather
# than an output-path id) to the id of the `outputPaths` definition with that
# value, so the reference resolves. A value with no matching definition is left
# as-is (the cross-reference validator then reports it, which is the honest
# signal that the legacy sheet names a path no output-path defines).
#
# @keywords internal
# @noRd
.resolvePIOutputPathRefs <- function(tasks, outputPaths) {
  if (length(tasks) == 0L || length(outputPaths) == 0L) {
    return(tasks)
  }
  # value -> id, so a full path can be looked up back to its output-path id.
  valueToId <- stats::setNames(
    names(outputPaths),
    vapply(outputPaths, as.character, character(1))
  )
  lapply(tasks, function(task) {
    task$outputMappings <- lapply(task$outputMappings, function(mapping) {
      path <- mapping$outputPath
      if (!is.null(path) && path %in% names(valueToId)) {
        mapping$outputPath <- valueToId[[path]]
      }
      mapping
    })
    task
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
  # A column every record holds a single number in is written as numbers, so a
  # number survives the round trip as a number: collapsing it to text would make
  # the field come back as a string on the next import and report the project as
  # out of sync with its own workbook. A column mixing text and numbers is
  # written as text, both because a single Excel column has one type and because
  # binding a character cell to a numeric one below would abort the export.
  numericCols <- allCols[vapply(
    allCols,
    function(col) {
      values <- Filter(
        Negate(is.null),
        lapply(records, function(rec) rec[[col]])
      )
      length(values) > 0L &&
        all(vapply(
          values,
          function(v) is.numeric(v) && length(v) == 1L,
          logical(1)
        ))
    },
    logical(1)
  )]
  rowDfs <- lapply(records, function(rec) {
    cells <- lapply(allCols, function(col) {
      val <- rec[[col]]
      if (col %in% numericCols) {
        val %||% NA_real_
      } else if (is.null(val)) {
        NA
      } else {
        paste(val, collapse = ", ")
      }
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
    ontoStr <- .formatOntogeniesToCell(ic$proteinOntogenies)
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
    ontoStr <- .formatOntogeniesToCell(popData$proteinOntogenies)

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
  project$rawFilePaths()
}

#' Extract private .excelData from a Project (the Excel-bridge sheet names)
#' @param project Project object
#' @returns Named list of property data (empty when no Excel side-car)
#' @keywords internal
#' @noRd
.extractExcelData <- function(project) {
  project$rawExcel()
}

#' Is a single Excel cell empty?
#'
#' Empty means any of: the sheet has no such column (which reads as `NULL` or a
#' zero-length value), the cell is `NA`, or it holds only whitespace.
#'
#' The bare `is.na(x) || x == ""` this replaces is only safe on a cell that
#' exists. On an absent column it evaluates to `NA`, and `if (NA)` aborts the
#' whole parse with `missing value where TRUE/FALSE needed`, a message that
#' names neither the sheet nor the column. A hand-maintained 5.x sheet routinely
#' lacks a column the parser reads, so the test lives here once rather than
#' being re-derived per cell.
#'
#' @param x A single cell value.
#' @returns `TRUE` when the cell carries no usable value. A value of length
#'   other than one is empty too: it is not a cell.
#' @keywords internal
#' @noRd
.isBlankCell <- function(x) {
  if (is.null(x) || length(x) != 1L || is.na(x)) {
    return(TRUE)
  }
  trimws(as.character(x)) == ""
}

#' Does a cell that must hold a number hold something else?
#'
#' The rule for every numeric Excel cell in one place: a blank cell is allowed
#' (an absent value), and a non-blank cell must coerce to a number. Text, and a
#' comma-decimal such as `1,5`, do not.
#'
#' @param x A single cell value.
#' @returns `TRUE` only for a non-blank cell that is not a number.
#' @keywords internal
#' @noRd
.isUnusableNumericCell <- function(x) {
  !.isBlankCell(x) && is.na(suppressWarnings(as.numeric(x)))
}

#' Emit a pre-built, still-unglued warning
#'
#' Every import warning that names something a modeller wrote (a sheet name, a
#' definition id, a task name) has to survive a `{`/`}` in that text, so its
#' message builder returns the templates unglued together with an environment
#' binding their variables and the emitting call hands both to one `cli_warn()`:
#' each template is then glue-parsed exactly once, and a value is only ever
#' reached through a variable, never parsed. This owns that emit so the contract
#' is honored the same way by every such warning.
#'
#' @param warning A `list(bullets =, envir =)` from the `messages` catalog.
#' @param class Condition class for the emitted warning.
#' @returns Nothing, called for its warning.
#' @keywords internal
#' @noRd
.warnFormatted <- function(warning, class) {
  cli::cli_warn(warning$bullets, class = class, .envir = warning$envir)
}

#' One cell of a parsed sheet, `NA` where the sheet has no such column
#'
#' `df[["Missing"]][[i]]` aborts with a subscript error rather than yielding an
#' absent value, so an optional column is read through here.
#'
#' @param df A parsed sheet.
#' @param column Column name.
#' @param i Row index.
#' @returns The cell value, or `NA` when the column or the row is absent.
#' @keywords internal
#' @noRd
.cellValue <- function(df, column, i = 1L) {
  values <- df[[column]]
  if (is.null(values) || length(values) < i) {
    return(NA)
  }
  values[[i]]
}

#' Abort when a parsed sheet lacks a column the parser requires
#'
#' An absent optional column is normal and read as `NULL`. An absent *required*
#' one has no such reading: the field would come out zero-length and fail later
#' on a value that names nothing, so name the sheet and the columns here instead.
#'
#' @param df A parsed sheet.
#' @param required Column names the parser requires.
#' @param sheet Sheet name, for the message.
#' @returns `df`, invisibly.
#' @keywords internal
#' @noRd
.requireExcelColumns <- function(df, required, sheet) {
  # The workbook is at fault, not the function that read it, and this helper's
  # name means nothing to the reader; attribute the abort to no function.
  rlang::local_error_call(NULL)
  columns <- setdiff(required, names(df))
  if (length(columns) > 0L) {
    cli::cli_abort(messages$excelSheetMissingRequiredColumns(sheet, columns))
  }
  invisible(df)
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

#' Read one row's protein-ontogeny declaration, in either workbook spelling
#'
#' Two spellings are in circulation, and a real 5.x workbook uses the second:
#'
#'   - a single `Protein Ontogenies` cell holding
#'     `Protein:Ontogeny,Protein:Ontogeny`, which is also what
#'     [exportProjectToExcel()] writes;
#'   - a `Protein` + `Ontogeny` column pair, each holding a comma-separated list,
#'     paired up positionally (`Protein` cell `A,B` with `Ontogeny` cell `X,Y`
#'     means `A:X` and `B:Y`).
#'
#' The pair is folded into the single-cell spelling, so both layouts import to
#' the same definition and an import -> export -> import round trip is a fixed
#' point. A declaration that cannot be paired (only one of the two columns
#' filled, or a differing number of proteins and ontogenies) warns naming the
#' record, because the alternative is ontogenies leaving the project in silence.
#' The single cell wins when both spellings carry a value.
#'
#' @param row One row of the sheet.
#' @param recordType `"individual"` or `"population"`, for the warning.
#' @param recordId The row's id, for the warning.
#' @returns A single `Protein:Ontogeny,...` string, or `NULL` when the row
#'   declares no ontogenies (or none that can be read).
#' @keywords internal
#' @noRd
.excelProteinOntogenies <- function(row, recordType, recordId) {
  # A filled single cell is stored verbatim, exactly as it was before the pair
  # spelling was read at all, so a workbook in the current spelling imports
  # byte-identically.
  single <- .naToNull(as.character(row[["Protein Ontogenies"]]))
  if (!is.null(single) && length(.splitProteinOntogenies(single)) > 0L) {
    return(single)
  }
  proteins <- .splitProteinOntogenies(row[["Protein"]])
  ontogenies <- .splitProteinOntogenies(row[["Ontogeny"]])
  if (length(proteins) == 0L && length(ontogenies) == 0L) {
    return(NULL)
  }
  if (length(proteins) != length(ontogenies)) {
    cli::cli_warn(messages$excelOntogeniesNotReadable(
      recordType,
      recordId,
      proteins,
      ontogenies
    ))
    return(NULL)
  }
  paste(paste0(proteins, ":", ontogenies), collapse = ",")
}

#' Replace an absent/`NA` single cell with a default, else keep the value
#' @keywords internal
#' @noRd
.naToDefault <- function(x, default) {
  if (is.null(x) || length(x) == 0 || (length(x) == 1L && is.na(x))) {
    return(default)
  }
  x
}

#' Replace an absent/`NA`/blank single cell with a default, else keep the value
#'
#' Like `.naToDefault()`, but a cell holding only whitespace (or an empty
#' string) counts as blank too. Use it where an empty cell must become a real
#' value rather than an invalid empty string.
#' @keywords internal
#' @noRd
.blankToDefault <- function(x, default) {
  x <- .naToDefault(x, default)
  if (length(x) == 1L && !is.na(x) && nchar(trimws(x)) == 0) {
    return(default)
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

#' Format a protein-ontogeny field as a single `Protein Ontogenies` cell
#'
#' Deliberately not `.formatArrayToCommaList()`: the ontogeny reader splits the
#' cell on plain commas and joins with none, so escaping a comma or padding the
#' separator would make the cell unreadable on the way back in. An entry cannot
#' contain a comma (it is a `Protein:Ontogeny` pair), so nothing needs escaping.
#'
#' @param x The field's value: a character vector of entries, a single
#'   comma-joined string, a list, or `NULL`.
#' @returns A length-1 string, or `NA_character_` when no ontogenies are set.
#' @keywords internal
#' @noRd
.formatOntogeniesToCell <- function(x) {
  entries <- .splitProteinOntogenies(x)
  if (length(entries) == 0L) {
    return(NA_character_)
  }
  paste(entries, collapse = ",")
}

#' Parse a comma-separated string into a character vector, or NULL
#'
#' Two conventions for protecting a comma inside a value are honored, so both
#' the values this package writes and legacy 5.x cells parse:
#'   - backslash escaping (`.formatArrayToCommaList()`'s output): `\\` is a
#'     literal backslash and `\,` a literal comma.
#'   - double-quote wrapping (the 5.x `"A", "B", "C, with comma"` cell): a
#'     comma inside a quoted run is literal, and the wrapping quotes are
#'     stripped from the token.
#'
#' @keywords internal
#' @noRd
.parseCommaListToArray <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x) || x == "") {
    return(NULL)
  }
  raw <- as.character(x)
  # Walk the string character by character, tracking escape state (previous
  # character was an unescaped backslash) and quote state (inside a `"..."`
  # run). Split on a comma only when it is neither escaped nor quoted; collapse
  # `\\` to `\` and `\,` to `,`; drop the wrapping quotes.
  chars <- strsplit(raw, "", fixed = TRUE)[[1]]
  parts <- character()
  current <- ""
  escape <- FALSE
  inQuote <- FALSE
  for (ch in chars) {
    if (escape) {
      current <- paste0(current, ch)
      escape <- FALSE
    } else if (ch == "\\") {
      escape <- TRUE
    } else if (ch == "\"") {
      inQuote <- !inQuote
    } else if (ch == "," && !inQuote) {
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
