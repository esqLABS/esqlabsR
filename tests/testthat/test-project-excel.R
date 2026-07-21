test_that("exportProjectToExcel writes OutputPaths sheet with atomic columns", {
  work_dir <- withr::local_tempdir()
  file.copy(dirname(exampleProjectPath()), work_dir, recursive = TRUE)
  project <- loadProject(file.path(work_dir, "Example", "Project.json"))

  excel_out <- withr::local_tempdir()
  exportProjectToExcel(project, outputDir = excel_out, silent = TRUE)

  scenariosFile <- file.path(excel_out, "Configurations", "Scenarios.xlsx")
  outputPathsDf <- readxl::read_excel(scenariosFile, sheet = "OutputPaths")
  expect_named(outputPathsDf, c("OutputPathId", "OutputPath"))
})

test_that("Project round-trips through Excel preserving outputPaths", {
  work_dir <- withr::local_tempdir()
  file.copy(dirname(exampleProjectPath()), work_dir, recursive = TRUE)
  project <- loadProject(file.path(work_dir, "Example", "Project.json"))

  excel_out <- withr::local_tempdir()
  exportProjectToExcel(project, outputDir = excel_out, silent = TRUE)
  reimportedJson <- importProjectFromExcel(
    file.path(excel_out, "Project.xlsx"),
    silent = TRUE
  )
  reimported <- loadProject(reimportedJson)

  expect_equal(
    unlist(reimported$definitions$outputPaths),
    unlist(project$definitions$outputPaths)
  )
})

test_that("Excel round-trip preserves parameter set values, paths, and units", {
  work_dir <- withr::local_tempdir()
  file.copy(dirname(exampleProjectPath()), work_dir, recursive = TRUE)
  project <- loadProject(file.path(work_dir, "Example", "Project.json"))

  excel_out <- withr::local_tempdir()
  exportProjectToExcel(project, outputDir = excel_out, silent = TRUE)
  reimportedJson <- suppressWarnings(importProjectFromExcel(
    file.path(excel_out, "Project.xlsx"),
    outputDir = withr::local_tempdir(),
    silent = TRUE
  ))
  reimported <- suppressWarnings(loadProject(reimportedJson))

  # The whole section must survive byte-equivalent: every set keeps its full
  # list of records (containerPath / parameterName / value / units), not just
  # the sheet names. A regression here would silently empty every sheet.
  # `.unwrapDefinitionList()` peels the read-only accessor wrapper so the stored
  # plain lists compare cleanly.
  expect_equal(
    .unwrapDefinitionList(reimported$definitions$parameterSets),
    .unwrapDefinitionList(project$definitions$parameterSets)
  )
})

test_that("Excel round-trip preserves initial-condition sets and scenario refs", {
  project <- testProject()
  addInitialConditions(project, "icset")
  suppressMessages(
    addInitialConditionEntry(
      project,
      "icset",
      path = c("Organism|A|Concentration", "Organism|B|Concentration"),
      value = c(1.5, 0.5),
      unit = c("mg/l", "µmol/l")
    )
  )
  setScenario(project, "testscenario", initialConditions = "icset")

  excel_out <- withr::local_tempdir()
  exportProjectToExcel(project, outputDir = excel_out, silent = TRUE)
  # The workbook is written from the tree.
  expect_true(file.exists(file.path(
    excel_out,
    "Configurations",
    "InitialConditions.xlsx"
  )))

  reimportedJson <- suppressWarnings(importProjectFromExcel(
    file.path(excel_out, "Project.xlsx"),
    outputDir = withr::local_tempdir(),
    silent = TRUE
  ))
  reimported <- suppressWarnings(loadProject(reimportedJson))

  # The IC set round-trips byte-equivalent (records carry path / value / unit).
  # Compare per-set by name: the Excel sheet order need not match the in-memory
  # map order, so assert the same set ids and each set's records, not key order.
  before <- .unwrapDefinitionList(project$definitions$initialConditions)
  after <- .unwrapDefinitionList(reimported$definitions$initialConditions)
  expect_setequal(names(after), names(before))
  for (id in names(before)) {
    expect_equal(after[[id]], before[[id]])
  }
  # The scenario's reference to the set survives both directions.
  expect_identical(
    reimported$definitions$scenarios[["testscenario"]]$initialConditions,
    "icset"
  )
})

test_that("Excel round-trip preserves parameter identification tasks", {
  work_dir <- withr::local_tempdir()
  file.copy(dirname(exampleProjectPath()), work_dir, recursive = TRUE)
  project <- loadProject(file.path(work_dir, "Example", "Project.json"))

  excel_out <- withr::local_tempdir()
  exportProjectToExcel(project, outputDir = excel_out, silent = TRUE)
  reimportedJson <- suppressWarnings(importProjectFromExcel(
    file.path(excel_out, "Project.xlsx"),
    outputDir = withr::local_tempdir(),
    silent = TRUE
  ))
  reimported <- suppressWarnings(loadProject(reimportedJson))

  # The whole PI section must survive the three-sheet (PITasks / PIParameters /
  # PIOutputMappings) round-trip: each task keeps its scenarios, its flattened
  # `configuration`, and every nested parameter and output-mapping record.
  # Excel cannot store an empty string distinctly from an empty cell, so a
  # parameter whose `units` is "" reimports as NULL; the model treats "" and
  # NULL as the same "unitless" state (see the PIParameter constructor test),
  # so the comparison normalizes that one equivalence rather than asserting a
  # byte-identical empty string.
  normalizeUnitlessParams <- function(tasks) {
    lapply(tasks, function(task) {
      task$parameters <- lapply(task$parameters, function(p) {
        # Keep the `units` key present but set its value to NULL (single-bracket
        # list assignment; `p$units <- NULL` would drop the key instead), so it
        # matches the reimported record, which carries a present-but-NULL units.
        if (identical(p$units, "")) {
          p["units"] <- list(NULL)
        }
        p
      })
      task
    })
  }
  expect_equal(
    .unwrapDefinitionList(reimported$definitions$parameterIdentification),
    normalizeUnitlessParams(.unwrapDefinitionList(
      project$definitions$parameterIdentification
    ))
  )
})

test_that("Excel round-trip preserves DataCombined numeric offsets and scales", {
  work_dir <- withr::local_tempdir()
  file.copy(dirname(exampleProjectPath()), work_dir, recursive = TRUE)
  project <- loadProject(file.path(work_dir, "Example", "Project.json"))

  # The example DataCombined leaves all offset / scale fields null, so add one
  # with populated numerics (fractional, so the values are unambiguously double
  # and cannot be re-read as integer by the JSON layer) to prove they survive
  # as numbers, not strings. The unit siblings stay character.
  addDataCombined(
    project,
    id = "dc_numeric",
    simulated = list(list(
      label = "sim",
      scenario = "aciclovir_iv",
      path = paste0(
        "Organism|PeripheralVenousBlood|Aciclovir|",
        "Plasma (Peripheral Venous Blood)"
      ),
      xOffsets = 2.5,
      xOffsetsUnits = "h",
      yOffsets = -1.25,
      yOffsetsUnits = "mg/l",
      xScaleFactors = 1.5,
      yScaleFactors = 0.5
    )),
    observed = list(list(
      label = "obs",
      dataSet = "someObservedSet",
      xOffsets = 3.25,
      xScaleFactors = 4.5
    ))
  )

  excel_out <- withr::local_tempdir()
  exportProjectToExcel(project, outputDir = excel_out, silent = TRUE)
  reimportedJson <- suppressWarnings(importProjectFromExcel(
    file.path(excel_out, "Project.xlsx"),
    outputDir = withr::local_tempdir(),
    silent = TRUE
  ))
  reimported <- suppressWarnings(loadProject(reimportedJson))
  dc <- .unwrapDefinitionList(reimported$definitions$dataCombined)[[
    "dc_numeric"
  ]]
  sim <- dc$simulated[[1]]
  obs <- dc$observed[[1]]

  expect_identical(sim$xOffsets, 2.5)
  expect_identical(sim$yOffsets, -1.25)
  expect_identical(sim$xScaleFactors, 1.5)
  expect_identical(sim$yScaleFactors, 0.5)
  expect_identical(sim$xOffsetsUnits, "h")
  expect_identical(sim$yOffsetsUnits, "mg/l")
  expect_identical(obs$xOffsets, 3.25)
  expect_identical(obs$xScaleFactors, 4.5)
})

test_that("Excel round-trip does not fabricate a steady-state unit", {
  work_dir <- withr::local_tempdir()
  file.copy(dirname(exampleProjectPath()), work_dir, recursive = TRUE)
  project <- loadProject(file.path(work_dir, "Example", "Project.json"))

  excel_out <- withr::local_tempdir()
  exportProjectToExcel(project, outputDir = excel_out, silent = TRUE)
  reimportedJson <- suppressWarnings(importProjectFromExcel(
    file.path(excel_out, "Project.xlsx"),
    outputDir = withr::local_tempdir(),
    silent = TRUE
  ))
  reimported <- suppressWarnings(loadProject(reimportedJson))
  before <- .unwrapDefinitionList(project$definitions$scenarios)
  after <- .unwrapDefinitionList(reimported$definitions$scenarios)

  # A non-steady-state scenario carries the parser's default steadyStateTime and
  # a null unit; the export must not fabricate a unit for it, so the unit stays
  # null across the round trip. (The steadyStateTime value's int-vs-double type
  # for a whole-number steady-state time is a separate JSON-layer concern owned
  # elsewhere, so this asserts the unit, the part the Excel bridge controls.)
  nonSteady <- "aciclovir_iv"
  expect_false(isTRUE(before[[nonSteady]]$simulateSteadyState))
  expect_null(before[[nonSteady]]$steadyStateTimeUnit)
  expect_null(after[[nonSteady]]$steadyStateTimeUnit)

  # A genuine steady-state scenario keeps its declared unit.
  steady <- "aciclovir_iv_steadystate"
  expect_true(isTRUE(before[[steady]]$simulateSteadyState))
  expect_identical(
    after[[steady]]$steadyStateTimeUnit,
    before[[steady]]$steadyStateTimeUnit
  )
})

test_that("Excel round-trip preserves individuals, populations, and applications", {
  work_dir <- withr::local_tempdir()
  file.copy(dirname(exampleProjectPath()), work_dir, recursive = TRUE)
  project <- loadProject(file.path(work_dir, "Example", "Project.json"))

  excel_out <- withr::local_tempdir()
  exportProjectToExcel(project, outputDir = excel_out, silent = TRUE)
  reimportedJson <- suppressWarnings(importProjectFromExcel(
    file.path(excel_out, "Project.xlsx"),
    outputDir = withr::local_tempdir(),
    silent = TRUE
  ))
  reimported <- suppressWarnings(loadProject(reimportedJson))

  expect_equal(
    .unwrapDefinitionList(reimported$definitions$individuals),
    .unwrapDefinitionList(project$definitions$individuals)
  )
  expect_equal(
    .unwrapDefinitionList(reimported$definitions$populations),
    .unwrapDefinitionList(project$definitions$populations)
  )
  expect_equal(
    .unwrapDefinitionList(reimported$definitions$applications),
    .unwrapDefinitionList(project$definitions$applications)
  )
})

test_that("Excel round-trip preserves a comma-bearing plot id inside a grid", {
  work_dir <- withr::local_tempdir()
  file.copy(dirname(exampleProjectPath()), work_dir, recursive = TRUE)
  project <- loadProject(file.path(work_dir, "Example", "Project.json"))

  # A comma is a legal plot-id character; a grid stores its membership as one
  # comma-separated string, so a comma-bearing id must be escaped or it is
  # shredded into several at the Excel boundary.
  addPlot(
    project,
    id = "cmax, ss",
    dataCombined = "aciclovir_individual",
    plotType = "individual"
  )
  addPlotGrid(project, id = "grid_comma", plots = c("p1", "cmax, ss"))

  excel_out <- withr::local_tempdir()
  exportProjectToExcel(project, outputDir = excel_out, silent = TRUE)
  reimportedJson <- suppressWarnings(importProjectFromExcel(
    file.path(excel_out, "Project.xlsx"),
    outputDir = withr::local_tempdir(),
    silent = TRUE
  ))
  reimported <- suppressWarnings(loadProject(reimportedJson))
  grid <- .unwrapDefinitionList(reimported$definitions$plotGrids)[[
    "grid_comma"
  ]]

  expect_identical(.splitPlotIDs(grid$plotIds), c("p1", "cmax, ss"))
})

test_that("Excel round-trip preserves project name and description", {
  work_dir <- withr::local_tempdir()
  file.copy(dirname(exampleProjectPath()), work_dir, recursive = TRUE)
  project <- loadProject(file.path(work_dir, "Example", "Project.json"))
  project$info$name <- "RT_Name"
  project$info$description <- "RT_Desc"

  excel_out <- withr::local_tempdir()
  exportProjectToExcel(project, outputDir = excel_out, silent = TRUE)
  reimportedJson <- importProjectFromExcel(
    file.path(excel_out, "Project.xlsx"),
    outputDir = withr::local_tempdir(),
    silent = TRUE
  )
  reimported <- suppressWarnings(loadProject(reimportedJson))

  expect_identical(reimported$info$name, "RT_Name")
  expect_identical(reimported$info$description, "RT_Desc")
})

test_that("Excel round-trip preserves the filePaths/excel container split", {
  work_dir <- withr::local_tempdir()
  file.copy(dirname(exampleProjectPath()), work_dir, recursive = TRUE)
  project <- loadProject(file.path(work_dir, "Example", "Project.json"))

  excel_out <- withr::local_tempdir()
  exportProjectToExcel(project, outputDir = excel_out, silent = TRUE)
  reimportedJson <- importProjectFromExcel(
    file.path(excel_out, "Project.xlsx"),
    silent = TRUE
  )
  reimported <- loadProject(reimportedJson)

  # The four live folders stay in filePaths; the seven Excel-bridge sheet names
  # re-split back into the excel block (not leaking into filePaths).
  expect_named(
    reimported$rawFilePaths(),
    c("simulationsFolder", "populationsFolder", "dataFolder", "outputFolder"),
    ignore.order = TRUE
  )
  expect_named(
    reimported$rawExcel(),
    c(
      "configurationsFolder",
      "modelParamsFile",
      "individualsFile",
      "populationsFile",
      "scenariosFile",
      "applicationsFile",
      "plotsFile"
    ),
    ignore.order = TRUE
  )
})

test_that("importProjectFromExcel writes a usable definitions/ tree", {
  out <- withr::local_tempdir()
  jsonPath <- importProjectFromExcel(
    testProjectExcelPath(),
    outputDir = out,
    silent = TRUE
  )

  # The import yields a ready-to-use tree project: the container plus a
  # per-kind tree under definitions/, so loadProject() reads a tree project
  # with no extra materialize step.
  expect_true(file.exists(jsonPath))
  expect_true(dir.exists(file.path(out, "definitions", "scenarios")))
  expect_gt(
    length(list.files(file.path(out, "definitions", "scenarios"))),
    0L
  )
})

# The import writes one canonical on-disk shape: a slim (`containerOnly`)
# `Project.json` plus a `definitions/` tree, exactly what `saveProject()` /
# `initProject()` produce. There is no second, fully-inlined container variant.
# So the container's own sections are empty on disk and the real definitions
# live in the tree; loading the container standalone in a tree-free directory
# reads an empty project, and only loading it alongside its tree reads the
# definitions.
test_that("importProjectFromExcel writes a slim container, definitions in the tree", {
  out <- withr::local_tempdir()
  jsonPath <- importProjectFromExcel(
    testProjectExcelPath(),
    outputDir = out,
    silent = TRUE
  )

  # The on-disk container carries empty sections (the tree owns them).
  raw <- jsonlite::fromJSON(jsonPath, simplifyVector = FALSE)
  expect_length(raw$scenarios, 0L)
  expect_length(raw$parameterSets, 0L)
  expect_length(raw$individuals, 0L)
  expect_length(raw$outputPaths, 0L)

  # Loading the full tree project reads the real definitions from the tree.
  fromTree <- suppressWarnings(loadProject(jsonPath))
  expect_gt(length(fromTree$definitions$scenarios), 0L)
  expect_gt(length(fromTree$definitions$parameterSets), 0L)

  # Loading only the container (copied to a tree-free directory) reads an empty
  # project: the definitions live in the tree, not inlined in the container.
  inlineDir <- withr::local_tempdir()
  inlineJson <- file.path(inlineDir, "Project.json")
  file.copy(jsonPath, inlineJson)
  containerOnly <- suppressWarnings(loadProject(inlineJson))
  expect_length(containerOnly$definitions$scenarios, 0L)
  expect_length(containerOnly$definitions$parameterSets, 0L)
})

# Pin the imported content to the known TestProjectExcel fixture rather than
# only comparing the import against itself. The fixture's canonical ids and the
# two output-path literals are stable, so they can be asserted directly.
test_that("the Excel import carries the known fixture ids and values", {
  out <- withr::local_tempdir()
  jsonPath <- suppressWarnings(importProjectFromExcel(
    testProjectExcelPath(),
    outputDir = out,
    silent = TRUE
  ))
  project <- suppressWarnings(loadProject(jsonPath))

  # Output paths: both ids and their OSPS-notation literals are pinned.
  expect_setequal(
    names(project$definitions$outputPaths),
    c("aciclovir_pvb", "aciclovir_fat_cell")
  )
  expect_identical(
    project$definitions$outputPaths[["aciclovir_pvb"]],
    "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)"
  )
  expect_identical(
    project$definitions$outputPaths[["aciclovir_fat_cell"]],
    "Organism|Fat|Intracellular|Aciclovir|Concentration in container"
  )

  # The single biometric individual and its two populations are present.
  expect_identical(names(project$definitions$individuals), "indiv1")
  expect_setequal(
    names(project$definitions$populations),
    c("testpopulation", "testpopulation_noonto")
  )

  # The known scenarios are all imported (canonical, lowercased ids).
  expect_true(all(
    c("testscenario", "pitestscenario", "populationscenario") %in%
      names(project$definitions$scenarios)
  ))
})

# Loading the imported project emits the expected cross-reference warnings,
# they are asserted rather than suppressed. The TestProjectExcel fixture has a
# known legacy gap: the per-sheet Excel project encodes application protocols as
# parameter-set sheets, so the Excel->JSON bridge does not populate an
# `applications` section, leaving each scenario's `applicationProtocol`
# reference dangling. This is the documented Excel round-trip lossiness, not a
# regression, so the dangling-applicationProtocol warning is the expected signal.
test_that("loading the Excel import warns about the dangling applicationProtocol refs", {
  out <- withr::local_tempdir()
  jsonPath <- suppressWarnings(importProjectFromExcel(
    testProjectExcelPath(),
    outputDir = out,
    silent = TRUE
  ))

  # No `applications` section is produced by the per-sheet Excel project, so the
  # scenarios' applicationProtocol references cannot resolve on load.
  expect_warning(
    loadProject(jsonPath),
    "undefined application"
  )
})

# The migration canonicalizes every id to a safe, lowercase form. When two
# distinct source ids collapse to the same canonical id, the migration must
# abort (matching interactive authoring) rather than silently let a downstream
# rename drop the second definition.
test_that("importProjectFromExcel aborts when two ids canonicalize to the same value", {
  work_dir <- withr::local_tempdir()
  file.copy(
    dirname(testProjectExcelPath()),
    work_dir,
    recursive = TRUE
  )
  projectDir <- file.path(work_dir, "TestProjectExcel")
  scenariosFile <- file.path(projectDir, "Configurations", "Scenarios.xlsx")

  # Rewrite the OutputPaths sheet so two ids (`Aciclovir_PVB` and its
  # case variant) canonicalize to the same `aciclovir_pvb`.
  collidingOutputPaths <- data.frame(
    OutputPathId = c("Aciclovir_PVB", "aciclovir_pvb"),
    OutputPath = c(
      "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)",
      "Organism|Fat|Intracellular|Aciclovir|Concentration in container"
    ),
    stringsAsFactors = FALSE
  )
  scenariosSheet <- readExcel(scenariosFile, sheet = "Scenarios")
  .writeExcel(
    list(OutputPaths = collidingOutputPaths, Scenarios = scenariosSheet),
    scenariosFile
  )

  expect_snapshot(
    error = TRUE,
    importProjectFromExcel(
      file.path(projectDir, "ProjectConfiguration.xlsx"),
      outputDir = withr::local_tempdir(),
      silent = TRUE
    )
  )
})

# Two distinct output-path ids may resolve to the same literal path. The export
# must key the scenario's ids off `names(sc$outputPaths)` (the ids themselves),
# not a value-based reverse-lookup that would collapse both to one id and drop
# the other.
test_that("exportProjectToExcel keeps both ids when two share one output path", {
  project <- testProject()
  suppressMessages(suppressWarnings({
    addOutputPath(
      project,
      id = c("op_dup_a", "op_dup_b"),
      path = "Organism|A|Concentration"
    )
    setScenario(
      project,
      "testscenario",
      outputPaths = c("op_dup_a", "op_dup_b")
    )
  }))

  excel_out <- withr::local_tempdir()
  suppressMessages(suppressWarnings(
    exportProjectToExcel(project, outputDir = excel_out, silent = TRUE)
  ))

  scenariosFile <- file.path(excel_out, "Configurations", "Scenarios.xlsx")
  scenariosSheet <- readExcel(scenariosFile, sheet = "Scenarios")
  row <- scenariosSheet[scenariosSheet$Scenario_name == "testscenario", ]
  exportedIds <- .parseCommaListToArray(row$OutputPathsIds)

  expect_setequal(exportedIds, c("op_dup_a", "op_dup_b"))
})

# A non-blank parameter `Value` cell that does not coerce to a number (text, a
# comma-decimal) must abort naming the sheet and row, rather than silently
# becoming NA and serialising a value-less parameter into the JSON project.
test_that(".parseExcelParameterSheets aborts on a non-numeric Value cell", {
  paramFile <- withr::local_tempfile(fileext = ".xlsx")
  df <- data.frame(
    `Container Path` = "Organism|A",
    `Parameter Name` = "P",
    Value = "not_a_number",
    Units = "mg",
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  .writeExcel(list(Global = df), paramFile)

  expect_snapshot(error = TRUE, .parseExcelParameterSheets(paramFile))
})

# A blank `Value` cell stays allowed (NA), so a partially-filled sheet still
# imports without aborting.
test_that(".parseExcelParameterSheets allows a blank Value cell", {
  paramFile <- withr::local_tempfile(fileext = ".xlsx")
  df <- data.frame(
    `Container Path` = "Organism|A",
    `Parameter Name` = "P",
    Value = NA_real_,
    Units = "mg",
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  .writeExcel(list(Global = df), paramFile)

  parsed <- .parseExcelParameterSheets(paramFile)
  expect_identical(parsed$Global[[1]]$value, NA_real_)
})

# The comparison is between the in-memory project and a fresh Excel re-import.
# Both sides canonicalize every id, so id canonicalization is never counted as
# drift: a project imported from Excel and loaded back is in sync with the Excel
# it came from.
test_that(".compareJsonToExcel does not count id canonicalization as drift", {
  out <- withr::local_tempdir()
  excelPath <- testProjectExcelPath()

  jsonPath <- suppressWarnings(importProjectFromExcel(
    excelPath,
    outputDir = out,
    silent = TRUE
  ))
  project <- suppressWarnings(loadProject(jsonPath))

  inSync <- suppressWarnings(.compareJsonToExcel(
    project = project,
    projectConfigPath = excelPath,
    silent = TRUE
  ))
  expect_true(inSync$excel_in_sync)
})

# Regression (#1123): a dirty saveProject() on a normal tree project must not
# flip the Excel axis's per-section verdicts. saveProject() writes the container
# with the tree-owned sections emptied, and the old Excel comparison read that
# emptied container raw, so a single unrelated edit made every definition section
# look "out-of-sync". The reworked comparison serializes the in-memory project
# instead, so the per-section verdicts are unchanged by a save that touched only
# an unrelated field (any pre-existing round-trip drift stays exactly as it was,
# and no new section drift appears). The overall verdict still turns out-of-sync
# because the edited field genuinely changed.
test_that("projectStatus() does not report false section drift after a dirty save", {
  tp <- with_temp_project()
  project <- tp$project

  # Per-section verdict before the edit: the honest baseline (some sections of
  # the example project may already differ through Excel round-trip lossiness).
  before <- suppressWarnings(projectStatus(project, silent = TRUE))
  statusBefore <- before$details$excel$file_status

  # A single container-metadata edit, then save. No definition section changed.
  project$info$description <- "edited after import"
  suppressWarnings(saveProject(project))

  after <- suppressWarnings(projectStatus(project, silent = TRUE))
  statusAfter <- after$details$excel$file_status

  # The save left every definition section's verdict exactly as it was: an
  # emptied on-disk container no longer blinds the comparison into flagging
  # untouched sections.
  definitionSections <- c(
    "observedData",
    "outputPaths",
    "scenarios",
    "parameterSets",
    "initialConditions",
    "individuals",
    "populations",
    "applications",
    "dataCombined",
    "plots",
    "plotGrids",
    "parameterIdentification"
  )
  for (section in definitionSections) {
    expect_identical(
      statusAfter[[section]],
      statusBefore[[section]],
      info = section
    )
  }

  # The edited field is what turns the overall verdict out-of-sync.
  expect_identical(statusAfter$description, "out-of-sync")
})

# A corrupt or unreadable Excel side-car cannot be compared. The Excel axis of
# projectStatus() must report that honestly as NA (the "cannot compare" state),
# not silently claim the project is in sync, and must warn when not silent. The
# tree axis is unaffected (a freshly loaded project is in sync).
test_that("projectStatus() reports the Excel axis as NA (and warns) when the side-car is unreadable", {
  work_dir <- withr::local_tempdir()
  file.copy(dirname(exampleProjectPath()), work_dir, recursive = TRUE)
  jsonPath <- file.path(work_dir, "Example", "Project.json")
  project <- suppressWarnings(loadProject(jsonPath))

  # A corrupt Project.xlsx side-car next to the container: not a valid workbook,
  # so the comparison's re-import aborts.
  writeLines(
    "this is not a valid xlsx workbook",
    file.path(work_dir, "Example", "Project.xlsx")
  )

  # Silent: the Excel axis is one of two; it is NA, the tree axis is in sync.
  status <- suppressWarnings(projectStatus(project, silent = TRUE))
  expect_named(status, c("tree_in_sync", "excel_in_sync", "details"))
  expect_identical(status$excel_in_sync, NA)
  expect_true(status$tree_in_sync)

  # Non-silent: a warning surfaces naming the comparison failure.
  expect_warning(
    projectStatus(project, silent = FALSE),
    "Cannot compare the Excel configuration files"
  )
})

# Regression (#1125): a project with dangling references (the TestProjectExcel
# fixture encodes application protocols as parameter-set sheets, so the imported
# scenarios carry dangling `application` refs) exported back to Excel must not
# hard-abort the status check. The dangling refs survive in mixed case and, on
# the comparison's Excel re-import, collide under id canonicalization. The abort
# is now caught and reported as the "cannot compare" NA state with a warning,
# rather than propagating as a hard error out of `projectStatus()`.
test_that("projectStatus() does not abort on a dangling-ref canonicalization collision", {
  # Copy the dangling-ref fixture with the entry workbook named Project.xlsx, so
  # the imported container is Project.json and the exported side-car stem matches
  # what the status check derives.
  work <- withr::local_tempdir()
  file.copy(
    list.files(
      testthat::test_path("data", "TestProjectExcel"),
      full.names = TRUE
    ),
    work,
    recursive = TRUE
  )
  file.rename(
    file.path(work, "ProjectConfiguration.xlsx"),
    file.path(work, "Project.xlsx")
  )

  jsonPath <- suppressWarnings(importProjectFromExcel(
    file.path(work, "Project.xlsx"),
    outputDir = work,
    silent = TRUE
  ))
  project <- suppressWarnings(loadProject(jsonPath))
  suppressWarnings(exportProjectToExcel(
    project,
    outputDir = work,
    silent = TRUE
  ))

  # Silent: no hard error; the collision surfaces as the NA "cannot compare"
  # state, not an abort.
  status <- suppressWarnings(projectStatus(project, silent = TRUE))
  expect_identical(status$excel_in_sync, NA)

  # Non-silent: the collision is reported as a comparison-failure warning, not
  # thrown as an error.
  expect_warning(
    suppressMessages(projectStatus(project, silent = FALSE)),
    "Cannot compare the Excel configuration files"
  )
})

# A legacy Scenarios sheet may spell booleans as `1`/`0`, `Yes`/`No`, or
# `true`/`false`; bare `as.logical()` turns the string forms into NA (silently
# defaulting to FALSE downstream), so the parser must interpret them tolerantly.
test_that(".parseExcelScenarios interprets legacy boolean spellings", {
  scenarioDf <- data.frame(
    Scenario_name = c("s_yes", "s_no", "s_num"),
    IndividualId = NA_character_,
    PopulationId = NA_character_,
    ReadPopulationFromCSV = c("Yes", "no", "1"),
    ModelParameterSheets = NA_character_,
    ApplicationProtocol = NA_character_,
    SimulationTime = NA_character_,
    SimulationTimeUnit = NA_character_,
    SteadyState = c("true", "FALSE", "0"),
    SteadyStateTime = NA_real_,
    SteadyStateTimeUnit = NA_character_,
    OverwriteFormulasInSS = c("y", "n", "1"),
    ModelFile = "m.pkml",
    OutputPathsIds = "op1",
    stringsAsFactors = FALSE
  )
  scenarios <- .parseExcelScenarios(scenarioDf)

  expect_identical(scenarios[[1]]$readPopulationFromCSV, TRUE)
  expect_identical(scenarios[[1]]$steadyState, TRUE)
  expect_identical(scenarios[[1]]$overwriteFormulasInSS, TRUE)

  expect_identical(scenarios[[2]]$readPopulationFromCSV, FALSE)
  expect_identical(scenarios[[2]]$steadyState, FALSE)
  expect_identical(scenarios[[2]]$overwriteFormulasInSS, FALSE)

  expect_identical(scenarios[[3]]$readPopulationFromCSV, TRUE)
  expect_identical(scenarios[[3]]$steadyState, FALSE)
  expect_identical(scenarios[[3]]$overwriteFormulasInSS, TRUE)
})

test_that(".parseExcelScenarios aborts on an unparseable boolean cell", {
  scenarioDf <- data.frame(
    Scenario_name = "s1",
    IndividualId = NA_character_,
    PopulationId = NA_character_,
    ReadPopulationFromCSV = NA_character_,
    ModelParameterSheets = NA_character_,
    ApplicationProtocol = NA_character_,
    SimulationTime = NA_character_,
    SimulationTimeUnit = NA_character_,
    SteadyState = "maybe",
    SteadyStateTime = NA_real_,
    SteadyStateTimeUnit = NA_character_,
    OverwriteFormulasInSS = NA_character_,
    ModelFile = "m.pkml",
    OutputPathsIds = "op1",
    stringsAsFactors = FALSE
  )
  expect_snapshot(error = TRUE, .parseExcelScenarios(scenarioDf))
})

# A renamed or absent scenario-sheet column (e.g. `OutputPathsId` for
# `OutputPathsIds`) must abort naming the missing column, rather than silently
# yielding a scenario with no output paths (the partial-match `$` access
# previously masked this).
test_that(".parseExcelScenarios aborts on a renamed required column", {
  scenarioDf <- data.frame(
    Scenario_name = "s1",
    IndividualId = NA_character_,
    PopulationId = NA_character_,
    ReadPopulationFromCSV = NA,
    ModelParameterSheets = NA_character_,
    ApplicationProtocol = NA_character_,
    SimulationTime = NA_character_,
    SimulationTimeUnit = NA_character_,
    SteadyState = NA,
    SteadyStateTime = NA_real_,
    SteadyStateTimeUnit = NA_character_,
    OverwriteFormulasInSS = NA,
    ModelFile = "m.pkml",
    # `OutputPathsIds` misspelled as `OutputPathsId`.
    OutputPathsId = "op1",
    stringsAsFactors = FALSE
  )
  expect_snapshot(error = TRUE, .parseExcelScenarios(scenarioDf))
})
