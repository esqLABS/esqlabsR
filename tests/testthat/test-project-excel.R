test_that("exportProjectToExcel writes OutputPaths sheet with atomic columns", {
  project <- exampleProject()

  excel_out <- withr::local_tempdir()
  exportProjectToExcel(project, outputDir = excel_out, silent = TRUE)

  scenariosFile <- file.path(excel_out, "Configurations", "Scenarios.xlsx")
  outputPathsDf <- readxl::read_excel(scenariosFile, sheet = "OutputPaths")
  expect_named(outputPathsDf, c("OutputPathId", "OutputPath"))
})

test_that("Project round-trips through Excel preserving outputPaths", {
  project <- exampleProject()

  excel_out <- withr::local_tempdir()
  exportProjectToExcel(project, outputDir = excel_out, silent = TRUE)
  # The exported workbooks declare the project's data workbook but do not carry
  # it, so re-importing from a scratch directory reports it as missing and skips
  # the observed data.
  expect_warning(
    reimportedJson <- importProjectFromExcel(
      file.path(excel_out, "Project.xlsx"),
      silent = TRUE
    ),
    "was not found"
  )
  # That skip is what leaves the re-imported project one reference it cannot
  # resolve.
  expect_warning(
    reimported <- loadProject(reimportedJson),
    "unresolved cross-reference"
  )

  expect_equal(
    unlist(reimported$definitions$outputPaths),
    unlist(project$definitions$outputPaths)
  )
})

test_that("Excel round-trip preserves parameter set values, paths, and units", {
  project <- exampleProject()

  reimported <- excelRoundTrip(project)

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
  project <- exampleProject()

  reimported <- excelRoundTrip(project)

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
  project <- exampleProject()

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

  reimported <- excelRoundTrip(project)
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

test_that("Excel round-trip defaults a non-steady-state scenario's steady-state time and unit", {
  project <- exampleProject()

  reimported <- excelRoundTrip(project)
  before <- .unwrapDefinitionList(project$definitions$scenarios)
  after <- .unwrapDefinitionList(reimported$definitions$scenarios)

  # A non-steady-state scenario has no meaningful steady-state time, so the
  # import defaults it to the same `1000` / `"min"` the authoring API writes,
  # rather than null. This keeps an imported project byte-identical to the same
  # project re-authored through `addScenario()`, so no round-trip diff (#1158).
  # The value is only used when steady-state is on.
  nonSteady <- "aciclovir_iv"
  expect_false(isTRUE(before[[nonSteady]]$steadyState))
  expect_identical(after[[nonSteady]]$steadyStateTime, 1000)
  expect_identical(after[[nonSteady]]$steadyStateTimeUnit, "min")

  # A genuine steady-state scenario keeps its declared unit.
  steady <- "aciclovir_iv_steadystate"
  expect_true(isTRUE(before[[steady]]$steadyState))
  expect_identical(
    after[[steady]]$steadyStateTimeUnit,
    before[[steady]]$steadyStateTimeUnit
  )
})

test_that("Excel round-trip keeps a steady-state time declared with the flag off", {
  project <- exampleProject()
  # A preset steady-state time the scenario is not currently using is valid and
  # the JSON writer keeps it, so the workbook has to as well: blanking the two
  # columns here sent the value through the import's blank-cell defaults and
  # came back as `1000` / `"min"`.
  setScenario(
    project,
    "aciclovir_iv",
    steadyState = FALSE,
    steadyStateTime = 1,
    steadyStateTimeUnit = "h"
  )
  before <- .unwrapDefinitionList(project$definitions$scenarios)
  after <- .unwrapDefinitionList(excelRoundTrip(project)$definitions$scenarios)

  expect_false(isTRUE(after[["aciclovir_iv"]]$steadyState))
  expect_identical(after[["aciclovir_iv"]]$steadyStateTimeUnit, "h")
  expect_equal(
    after[["aciclovir_iv"]]$steadyStateTime,
    before[["aciclovir_iv"]]$steadyStateTime
  )
})

test_that("Excel round-trip keeps a declared steady-state time of zero", {
  project <- exampleProject()
  # Zero is an authored value like any other: the parser reads it, nothing
  # constrains its sign, and the JSON writer emits it. Gating the export on a
  # positive time blanked the cells, and the import's blank-cell defaults then
  # substituted `1000` / `"min"` for it.
  setScenario(
    project,
    "aciclovir_iv",
    steadyState = FALSE,
    steadyStateTime = 0,
    steadyStateTimeUnit = "h"
  )
  after <- .unwrapDefinitionList(excelRoundTrip(project)$definitions$scenarios)

  expect_identical(after[["aciclovir_iv"]]$steadyStateTimeUnit, "h")
  expect_equal(after[["aciclovir_iv"]]$steadyStateTime, 0)
})

test_that("Excel round-trip preserves individuals, populations, and applications", {
  project <- exampleProject()

  reimported <- excelRoundTrip(project)

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

test_that("Excel round-trip preserves a grid whose plot id had a comma canonicalized out", {
  project <- exampleProject()

  # A comma is canonicalized to `_` in every id (#1158), so no plot id ever
  # reaches the Excel boundary with a comma to be shredded. The grid membership
  # still round-trips because the canonical `cmax__ss` survives the comma-
  # escaping join/split the grid uses to store its members.
  suppressWarnings(addPlot(
    project,
    id = "cmax, ss",
    dataCombined = "aciclovir_individual",
    plotType = "individual"
  ))
  suppressWarnings(addPlotGrid(
    project,
    id = "grid_comma",
    plots = c("p1", "cmax, ss")
  ))

  reimported <- excelRoundTrip(project)
  grid <- .unwrapDefinitionList(reimported$definitions$plotGrids)[[
    "grid_comma"
  ]]

  expect_identical(.splitPlotIDs(grid$plots), c("p1", "cmax__ss"))
})

test_that("exportProjectToExcel names the scenarios whose solverSettings it cannot carry", {
  project <- exampleProject()
  setScenario(project, "aciclovir_iv", solverSettings = list(relTol = 1e-6))
  excel_out <- withr::local_tempdir()

  warning <- expect_warning(
    exportProjectToExcel(project, outputDir = excel_out, silent = TRUE),
    "aciclovir_iv",
    class = "esqlabsR_exportScenarioSolverSettings"
  )
  # The message names what a re-imported project solves the scenario with
  # instead: the project-wide defaults, which the workbooks do carry.
  expect_snapshot(cat(conditionMessage(warning)))
  # The block is dropped, not the export: the workbooks are still written.
  expect_true(file.exists(file.path(
    excel_out,
    "Configurations",
    "Scenarios.xlsx"
  )))
})

test_that("exportProjectToExcel is silent about solver settings when no scenario has any", {
  project <- exampleProject()
  excel_out <- withr::local_tempdir()

  # Every other warning the export may raise still passes through; only the
  # solver-settings one must be absent.
  withCallingHandlers(
    exportProjectToExcel(project, outputDir = excel_out, silent = TRUE),
    esqlabsR_exportScenarioSolverSettings = function(w) {
      testthat::fail("warned about solver settings for a project with none")
    }
  )
  expect_true(file.exists(file.path(excel_out, "Project.xlsx")))
})

# The project-wide solver defaults travel on a sheet of their own, after the
# Property table every reader takes as the first sheet. Each value is text that
# reads back as the identical value: a tolerance that needs every digit keeps
# them, and a step count is written without an exponent.
test_that("exportProjectToExcel writes the project's defaultSolverSettings to their own sheet", {
  project <- exampleProject()
  project$defaultSolverSettings <- list(
    relTol = 1e-6,
    absTol = 0.1 + 0.2,
    mxStep = 100000L,
    useJacobian = FALSE
  )
  excel_out <- withr::local_tempdir()

  exportProjectToExcel(project, outputDir = excel_out, silent = TRUE)

  workbook <- file.path(excel_out, "Project.xlsx")
  expect_identical(
    readxl::excel_sheets(workbook),
    c("Project", "DefaultSolverSettings")
  )
  expect_equal(
    as.data.frame(readxl::read_excel(
      workbook,
      sheet = "DefaultSolverSettings"
    )),
    data.frame(
      Setting = c("relTol", "absTol", "mxStep", "useJacobian"),
      Value = c("1e-06", "0.30000000000000004", "100000", "FALSE"),
      Description = c(
        "Relative tolerance",
        "Absolute tolerance",
        "Maximum number of internal steps per output interval",
        "Whether the analytic Jacobian is used (TRUE or FALSE)"
      )
    )
  )
})

test_that("exportProjectToExcel writes no solver-settings sheet for a project without defaults", {
  project <- exampleProject()
  expect_null(project$defaultSolverSettings)
  excel_out <- withr::local_tempdir()

  exportProjectToExcel(project, outputDir = excel_out, silent = TRUE)

  expect_identical(
    readxl::excel_sheets(file.path(excel_out, "Project.xlsx")),
    "Project"
  )
})

test_that("Excel round-trip preserves the project's defaultSolverSettings", {
  project <- exampleProject()
  project$defaultSolverSettings <- list(
    relTol = 1e-8,
    hMax = 60,
    mxStep = 50000L,
    checkForNegativeValues = TRUE
  )

  reimported <- excelRoundTrip(project)

  expect_equal(
    reimported$defaultSolverSettings,
    list(
      relTol = 1e-8,
      hMax = 60,
      mxStep = 50000,
      checkForNegativeValues = TRUE
    )
  )
})

# Before the sheet existed, a project whose only Excel gap was its solver
# defaults was reported out of sync right after its own export.
test_that("projectStatus() finds the defaultSolverSettings in sync after an export", {
  project <- exampleProject()
  project$defaultSolverSettings <- list(relTol = 1e-6)
  suppressMessages(saveProject(project))
  exportProjectToExcel(project, overwrite = TRUE, silent = TRUE)

  status <- suppressWarnings(suppressMessages(
    projectStatus(project, silent = TRUE)
  ))

  expect_false(
    "defaultSolverSettings" %in%
      c(
        names(status$details$excel$file_changes),
        names(status$details$excel$data_changes)
      )
  )
})

# A modeler editing the sheet by hand may type a tolerance as an Excel number
# and a switch as an Excel boolean, rather than as the text the export writes.
# Both keep their type, so neither is rejected for being written the natural way.
test_that("the DefaultSolverSettings sheet reads numbers and booleans typed into Excel", {
  excel_out <- withr::local_tempdir()
  numbers <- file.path(excel_out, "Numbers.xlsx")
  writexl::write_xlsx(
    list(
      Project = data.frame(Property = "schemaVersion", Value = "2.0"),
      DefaultSolverSettings = data.frame(
        Setting = c("relTol", "mxStep"),
        Value = c(1e-6, 5000)
      )
    ),
    numbers
  )
  flags <- file.path(excel_out, "Flags.xlsx")
  writexl::write_xlsx(
    list(
      Project = data.frame(Property = "schemaVersion", Value = "2.0"),
      DefaultSolverSettings = data.frame(
        Setting = c("useJacobian", "checkForNegativeValues"),
        Value = c(TRUE, FALSE)
      )
    ),
    flags
  )

  expect_identical(
    .readExcelDefaultSolverSettings(numbers),
    list(relTol = 1e-6, mxStep = 5000)
  )
  expect_identical(
    .readExcelDefaultSolverSettings(flags),
    list(useJacobian = TRUE, checkForNegativeValues = FALSE)
  )
})

# A hand-edited sheet is checked by the rules `Project.json` gets, with the same
# message, plus the two mistakes only a sheet can make. A value that does not
# parse reaches the check as written instead of being dropped, so it is named.
test_that("importProjectFromExcel reports an invalid DefaultSolverSettings sheet", {
  project <- exampleProject()
  excel_out <- withr::local_tempdir()
  exportProjectToExcel(project, outputDir = excel_out, silent = TRUE)
  workbook <- file.path(excel_out, "Project.xlsx")
  editWorkbookSheets(workbook, function(sheets) {
    sheets$DefaultSolverSettings <- data.frame(
      Setting = c("relTol", "relTol", NA, "reltol", "mxStep", "useJacobian"),
      Value = c("1e-6", "1e-8", "1e-4", "1e-6", "1.5", "yes")
    )
    sheets
  })

  expect_snapshot(
    error = TRUE,
    transform = .redactProjectWorkbookPath,
    importProjectFromExcel(
      workbook,
      outputDir = withr::local_tempdir(),
      silent = TRUE
    )
  )
})

test_that("importProjectFromExcel aborts on a DefaultSolverSettings sheet without its columns", {
  project <- exampleProject()
  excel_out <- withr::local_tempdir()
  exportProjectToExcel(project, outputDir = excel_out, silent = TRUE)
  workbook <- file.path(excel_out, "Project.xlsx")
  editWorkbookSheets(workbook, function(sheets) {
    sheets$DefaultSolverSettings <- data.frame(
      Name = "relTol",
      Tolerance = 1e-6
    )
    sheets
  })

  expect_snapshot(
    error = TRUE,
    importProjectFromExcel(
      workbook,
      outputDir = withr::local_tempdir(),
      silent = TRUE
    )
  )
})

test_that("Excel round-trip preserves project name and description", {
  project <- exampleProject()
  project$info$name <- "RT_Name"
  project$info$description <- "RT_Desc"

  reimported <- excelRoundTrip(project)

  expect_identical(reimported$info$name, "RT_Name")
  expect_identical(reimported$info$description, "RT_Desc")
})

test_that("Excel round-trip preserves the filePaths/excel container split", {
  project <- exampleProject()

  excel_out <- withr::local_tempdir()
  exportProjectToExcel(project, outputDir = excel_out, silent = TRUE)
  # The exported workbooks declare the project's data workbook but do not carry
  # it, so re-importing from a scratch directory reports it as missing, skips
  # the observed data, and leaves the re-imported project one reference it
  # cannot resolve.
  expect_warning(
    reimportedJson <- importProjectFromExcel(
      file.path(excel_out, "Project.xlsx"),
      silent = TRUE
    ),
    "was not found"
  )
  expect_warning(
    reimported <- loadProject(reimportedJson),
    "unresolved cross-reference"
  )

  # The live folders stay in filePaths, joined by the experimental-data workbook
  # the export derives from the observed-data declaration; the Excel-bridge
  # sheet names re-split back into the excel block (not leaking into filePaths).
  expect_named(
    reimported$rawFilePaths(),
    c(
      "simulationsFolder",
      "populationsFolder",
      "dataFolder",
      "dataFile",
      "dataImporterConfigurationFile",
      "outputFolder"
    ),
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
      "plotsFile",
      "parameterIdentificationFile"
    ),
    ignore.order = TRUE
  )
})

# The `Project.xlsx` Property table is where a project names its
# experimental-data workbook and its parameter-identification workbook. A
# project imported from Excel carries those rows in its containers, but one
# authored in JSON keeps the data source inside the observed-data definition and
# carries none of them, so exporting only the container blocks wrote a project
# that declared no observed data at all. Re-importing that dropped the section
# without a word (the importer's missing-data warning fires only for a
# `dataFile` that is declared), and every parameter-identification mapping
# reading a data set then dangled.
test_that("exporting a JSON-authored project declares its data and PI workbooks", {
  project <- exampleProject()
  projectDir <- project$info$projectDirPath

  # Exported into the project's own folder, so the exported `dataFolder` still
  # names the folder the data workbook actually sits in.
  exportProjectToExcel(
    project,
    outputDir = projectDir,
    overwrite = TRUE,
    silent = TRUE
  )

  properties <- excelProjectProperties(file.path(projectDir, "Project.xlsx"))
  expect_identical(
    properties[["dataFile"]],
    "Aciclovir_TimeValuesData.xlsx"
  )
  expect_identical(
    properties[["dataImporterConfigurationFile"]],
    "esqlabs_dataImporter_configuration.xml"
  )
  # The export writes `Configurations/ParameterIdentification.xlsx`, so the
  # exported project declares the workbook it wrote rather than relying on the
  # importer's conventional-filename fallback.
  expect_identical(
    properties[["parameterIdentificationFile"]],
    "ParameterIdentification.xlsx"
  )

  reimported <- loadProject(importProjectFromExcel(
    file.path(projectDir, "Project.xlsx"),
    outputDir = withr::local_tempdir(),
    silent = TRUE
  ))
  expect_length(reimported$definitions$observedData, 1L)
  # The declaration is back, so the PI task's output mappings resolve against a
  # data set that exists instead of dangling.
  expect_identical(
    validateProject(reimported)$crossReferences$critical_errors,
    list()
  )
})

# The Property table names one experimental-data workbook, so a project
# declaring several observed-data sources cannot state them all. The first
# `excel` source is written and the rest are named in a warning, rather than one
# of several being picked silently.
test_that("exporting several observed-data declarations writes one and names the rest", {
  project <- exampleProject()
  addObservedData(
    project,
    list(
      type = "excel",
      id = "laskin_group_b",
      file = "Laskin_GroupB_TimeValuesData.xlsx",
      importerConfiguration = "esqlabs_dataImporter_configuration.xml",
      sheets = list("Laskin 1982.Group B")
    )
  )

  excelOut <- withr::local_tempdir()
  expect_warning(
    exportProjectToExcel(project, outputDir = excelOut, silent = TRUE),
    "laskin_group_b"
  )

  properties <- excelProjectProperties(file.path(excelOut, "Project.xlsx"))
  expect_identical(properties[["dataFile"]], "Aciclovir_TimeValuesData.xlsx")
  expect_identical(
    properties[["dataImporterConfigurationFile"]],
    "esqlabs_dataImporter_configuration.xml"
  )
})

# A value the containers carry always wins, so deriving the missing rows never
# changes what a project imported from Excel exports.
test_that("an Excel-imported project's own workbook names survive the export", {
  project <- suppressWarnings(loadProject(importProjectFromExcel(
    testProjectExcelPath(),
    outputDir = withr::local_tempdir(),
    silent = TRUE
  )))
  # A second declaration whose file differs from the one the container names:
  # without the container-wins rule the export would write this one instead.
  addObservedData(
    project,
    list(
      type = "excel",
      id = "later_addition",
      file = "Later_TimeValuesData.xlsx",
      importerConfiguration = "esqlabs_dataImporter_configuration.xml",
      sheets = list("Sheet1")
    )
  )

  excelOut <- withr::local_tempdir()
  suppressWarnings(
    exportProjectToExcel(project, outputDir = excelOut, silent = TRUE)
  )

  # The three rows the fixture's own property sheet declares, unchanged.
  properties <- excelProjectProperties(file.path(excelOut, "Project.xlsx"))
  expect_identical(
    properties[["dataFile"]],
    "TestProject_TimeValuesData.xlsx"
  )
  expect_identical(
    properties[["dataImporterConfigurationFile"]],
    "esqlabs_dataImporter_configuration.xml"
  )
  expect_identical(
    properties[["parameterIdentificationFile"]],
    "ParameterIdentification.xlsx"
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

# Regression (#1126): re-importing over an existing JSON project deletes any
# definition authored only on the JSON side (the tree reconcile empties every
# `definitions/<kind>/` not present in the Excel). importProjectFromExcel()
# aborts by default when a JSON project already exists in `outputDir`, and only
# replaces it when `overwrite = TRUE`.
test_that("importProjectFromExcel aborts over an existing JSON project unless overwrite = TRUE", {
  out <- withr::local_tempdir()

  # First import succeeds: the output directory holds no JSON project yet.
  suppressWarnings(importProjectFromExcel(
    testProjectExcelPath(),
    outputDir = out,
    silent = TRUE
  ))

  # A second import over it aborts by default (the JSON project is now present).
  expect_snapshot(
    error = TRUE,
    transform = .redactTmpDir,
    suppressWarnings(importProjectFromExcel(
      testProjectExcelPath(),
      outputDir = out,
      silent = TRUE
    ))
  )

  # With overwrite = TRUE it replaces the existing JSON project.
  expect_no_error(suppressWarnings(importProjectFromExcel(
    testProjectExcelPath(),
    outputDir = out,
    overwrite = TRUE,
    silent = TRUE
  )))
})

# The Excel round trip of `vignette("how-to-set-up-a-project")`: export, edit the
# workbooks, import them back over the project they came from. The import has to
# take every definition from the workbooks, so an edit, an added row, and a
# deleted row all arrive, and a definition that exists only in the JSON files is
# deleted, as `overwrite` documents.
test_that("importProjectFromExcel(overwrite = TRUE) replaces the project with the edited workbooks", {
  project <- exampleProject()
  projectDir <- project$info$projectDirPath
  exportProjectToExcel(project, outputDir = projectDir, silent = TRUE)
  addOutputPath(project, id = "json_only", path = "Organism|Lumen|Aciclovir")
  saveProject(project)

  configurations <- file.path(projectDir, "Configurations")
  editWorkbookSheets(
    file.path(configurations, "Scenarios.xlsx"),
    function(sheets) {
      scenarios <- sheets$Scenarios
      row <- which(scenarios$Scenario_name == "aciclovir_iv")
      scenarios$SimulationTime[row] <- "0, 48, 10"
      newRow <- scenarios[row, ]
      newRow$Scenario_name <- "aciclovir_iv_copy"
      scenarios <- rbind(scenarios, newRow)
      sheets$Scenarios <- scenarios[
        scenarios$Scenario_name != "aciclovir_iv_steadystate",
      ]
      sheets
    }
  )
  editWorkbookSheets(
    file.path(configurations, "ModelParameters.xlsx"),
    function(sheets) {
      sheets$global$Value[1] <- 0.5
      newRow <- sheets$global[1, ]
      newRow[["Parameter Name"]] <- "Volume"
      sheets$global <- rbind(sheets$global, newRow)
      sheets$aciclovir <- sheets$aciclovir[0, ]
      sheets
    }
  )

  summaryText <- paste(
    utils::capture.output(
      importProjectFromExcel(
        file.path(projectDir, "Project.xlsx"),
        overwrite = TRUE
      ),
      type = "message"
    ),
    collapse = "\n"
  )
  # The counts are those of the workbooks: two output paths, not the three the
  # JSON project held before the import.
  expect_match(summaryText, "Output Paths: 2", fixed = TRUE)

  imported <- loadProject(file.path(projectDir, "Project.json"))
  scenarios <- imported$definitions$scenarios
  expect_setequal(
    names(scenarios),
    c("aciclovir_iv", "aciclovir_iv_copy", "aciclovir_iv_population")
  )
  expect_equal(scenarios$aciclovir_iv$simulationTime, list(c(0, 48, 10)))
  expect_false(file.exists(file.path(
    projectDir,
    "definitions",
    "scenarios",
    "aciclovir_iv_steadystate.json"
  )))

  parameterSets <- imported$definitions$parameterSets
  expect_equal(
    vapply(parameterSets$global, `[[`, character(1), "parameterName"),
    c("EHC continuous fraction", "Volume")
  )
  expect_equal(parameterSets$global[[1]]$value, 0.5)
  expect_length(parameterSets$aciclovir, 0L)

  expect_false("json_only" %in% names(imported$definitions$outputPaths))
  expect_false(file.exists(file.path(
    projectDir,
    "definitions",
    "output-paths",
    "json_only.json"
  )))
  expect_true(projectStatus(imported, silent = TRUE)$excel_in_sync)
})

# Both of importProjectFromExcel()'s preconditions are checked before the
# workbook is read, so a call that is going to refuse says so instead of first
# reporting on the contents of a project it will not import (an id collision
# aborts the read, an incomplete observed curve warns during it). The source
# path is checked ahead of the destination, so a mistyped `projectConfigPath` is
# named rather than masked by an `outputDir` that already holds a project.
test_that("importProjectFromExcel checks its preconditions before reading the workbook", {
  out <- withr::local_tempdir()
  writeLines("{}", file.path(out, "Project.json"))

  # A source that exists but is not a readable workbook: the overwrite refusal
  # still wins, which it can only do if the guard runs before the read.
  unreadable <- file.path(withr::local_tempdir(), "Project.xlsx")
  writeLines("not a workbook", unreadable)
  expect_snapshot(
    error = TRUE,
    transform = .redactTmpDir,
    importProjectFromExcel(unreadable, outputDir = out, silent = TRUE)
  )

  # A missing source is named ahead of the occupied destination.
  expect_snapshot(
    error = TRUE,
    transform = .redactTmpDir,
    importProjectFromExcel(
      file.path(out, "Missing.xlsx"),
      outputDir = out,
      silent = TRUE
    )
  )
})

# The unreadable-workbook abort is raised inside `.excelToProjectJson()`, a
# private helper no caller ever names, so it has to be attributed to the
# entrypoint the user did call: an "Error in `.excelToProjectJson()`" prefix
# points at a function that appears nowhere in the user's script. The two sibling
# preconditions above already abort under the entrypoint's name, so this keeps
# one import's whole error surface reading alike. Snapshotted rather than
# pattern-matched because the attribution lives in the rendered text and nowhere
# else, which is how an extraction changed it unnoticed.
test_that("importProjectFromExcel names itself when the workbook cannot be read", {
  sourceDir <- withr::local_tempdir()
  workbook <- file.path(sourceDir, "Project.xlsx")
  writeLines("this is not a valid xlsx workbook", workbook)

  expect_snapshot(
    error = TRUE,
    transform = .redactProjectWorkbookPath,
    importProjectFromExcel(
      workbook,
      outputDir = withr::local_tempdir(),
      silent = TRUE
    )
  )
})

# `importProjectFromExcel()` reads only Excel files. Any other file gets the
# same refusal before anything is written: with the default `outputDir`, the
# folder holding the file is where the import would have written, so that
# folder must come out unchanged (#1417).
test_that("importProjectFromExcel refuses the esqlabsR 5 snapshot JSON and writes nothing", {
  # The JSON file alone.
  dir <- withr::local_tempdir()
  snapshot <- file.path(dir, "ProjectConfiguration.json")
  file.copy(
    test_path("data", "TestProjectExcel", "ProjectConfiguration.json"),
    snapshot
  )
  expect_snapshot(
    importProjectFromExcel(snapshot),
    error = TRUE,
    transform = .redactTmpDir
  )
  expect_identical(
    list.files(dir, all.files = TRUE, no.. = TRUE),
    basename(snapshot)
  )

  # The JSON file beside the Excel files it was saved from.
  projectDir <- localExcelProjectDir()
  before <- list.files(projectDir, recursive = TRUE, all.files = TRUE)
  expect_snapshot(
    importProjectFromExcel(file.path(projectDir, "ProjectConfiguration.json")),
    error = TRUE,
    transform = .redactTmpDir
  )
  expect_identical(
    list.files(projectDir, recursive = TRUE, all.files = TRUE),
    before
  )
})

test_that("importProjectFromExcel refuses any other file that is not an Excel file and writes nothing", {
  # A project file of the current format.
  project <- testProject()
  projectDir <- dirname(project$info$projectFilePath)
  before <- list.files(projectDir, recursive = TRUE, all.files = TRUE)
  expect_snapshot(
    importProjectFromExcel(project$info$projectFilePath),
    error = TRUE,
    transform = .redactTmpDir
  )
  expect_identical(
    list.files(projectDir, recursive = TRUE, all.files = TRUE),
    before
  )

  # A `.json` file that does not parse, and a csv table.
  dir <- withr::local_tempdir()
  broken <- file.path(dir, "ProjectConfiguration.json")
  writeLines("{\"projectConfiguration\": {\"column_names\": [", broken)
  table <- file.path(dir, "ObservedData.csv")
  writeLines(c("Time,Concentration", "0.5,1.2"), table)
  expect_snapshot(
    importProjectFromExcel(broken),
    error = TRUE,
    transform = .redactTmpDir
  )
  expect_snapshot(
    importProjectFromExcel(table),
    error = TRUE,
    transform = .redactTmpDir
  )
  expect_setequal(
    list.files(dir, all.files = TRUE, no.. = TRUE),
    basename(c(broken, table))
  )
})

# Regression (#1126): exportProjectToExcel() replaces Project.xlsx and the
# Configurations workbooks wholesale, defaulting outputDir to the project's own
# directory, so a bare call would silently overwrite hand-maintained workbooks.
# It aborts by default when workbooks already exist in `outputDir`, and only
# overwrites them when `overwrite = TRUE`.
test_that("exportProjectToExcel aborts over existing workbooks unless overwrite = TRUE", {
  project <- exampleProject()

  excelOut <- withr::local_tempdir()

  # First export succeeds into a fresh directory.
  exportProjectToExcel(project, outputDir = excelOut, silent = TRUE)

  # A second export over the same directory aborts by default.
  expect_snapshot(
    error = TRUE,
    transform = .redactTmpDir,
    exportProjectToExcel(project, outputDir = excelOut, silent = TRUE)
  )

  # With overwrite = TRUE it replaces the existing workbooks.
  expect_no_error(
    exportProjectToExcel(
      project,
      outputDir = excelOut,
      overwrite = TRUE,
      silent = TRUE
    )
  )
})

# Regression (#1139): the Excel-import paths come from the author-controlled
# Property column, so they must be contained under the project folder the same
# way #1034 contained the JSON-side read paths. A `configurationsFolder` or a
# per-section workbook filename that escapes the project root (`../` climb or an
# absolute path) aborts naming the field, while the `${VAR}` env-var form stays
# a sanctioned escape hatch.

# Copy the TestProjectExcel fixture and rewrite one Property in its
# ProjectConfiguration.xlsx, returning the copied project directory.
.excelFixtureWithProperty <- function(property, value, envir = parent.frame()) {
  work <- withr::local_tempdir(.local_envir = envir)
  file.copy(
    list.files(
      testthat::test_path("data", "TestProjectExcel"),
      full.names = TRUE
    ),
    work,
    recursive = TRUE
  )
  xlsx <- file.path(work, "ProjectConfiguration.xlsx")
  df <- as.data.frame(readxl::read_excel(xlsx))
  if (property %in% df$Property) {
    df$Value[df$Property == property] <- value
  } else {
    df <- rbind(
      df,
      data.frame(Property = property, Value = value, Description = "")
    )
  }
  writexl::write_xlsx(df, xlsx)
  work
}

test_that("importProjectFromExcel aborts on a configurationsFolder that escapes the project", {
  # Absolute escape.
  workAbs <- .excelFixtureWithProperty("configurationsFolder", "/etc")
  expect_error(
    suppressWarnings(importProjectFromExcel(
      file.path(workAbs, "ProjectConfiguration.xlsx"),
      outputDir = withr::local_tempdir(),
      silent = TRUE
    )),
    "configurationsFolder"
  )

  # Relative `../` climb escape.
  workRel <- .excelFixtureWithProperty(
    "configurationsFolder",
    "../../../../etc"
  )
  expect_error(
    suppressWarnings(importProjectFromExcel(
      file.path(workRel, "ProjectConfiguration.xlsx"),
      outputDir = withr::local_tempdir(),
      silent = TRUE
    )),
    "configurationsFolder"
  )
})

test_that("importProjectFromExcel aborts on a workbook filename that escapes the configurations folder", {
  work <- .excelFixtureWithProperty(
    "scenariosFile",
    "../../../../secret.xlsx"
  )
  expect_error(
    suppressWarnings(importProjectFromExcel(
      file.path(work, "ProjectConfiguration.xlsx"),
      outputDir = withr::local_tempdir(),
      silent = TRUE
    )),
    "scenariosFile"
  )
})

test_that("importProjectFromExcel expands a ${VAR} configurationsFolder (escape hatch)", {
  # A `${VAR}` value opts out of the containment check and is expanded against
  # the environment. Point it at the fixture's own directory through the
  # variable and confirm the sections actually import: an unexpanded literal
  # would resolve to a nonexistent path and silently drop every section.
  work <- .excelFixtureWithProperty(
    "configurationsFolder",
    "${MY_CONFIGS}/Configurations"
  )
  withr::local_envvar(c(MY_CONFIGS = work))

  jsonPath <- suppressWarnings(importProjectFromExcel(
    file.path(work, "ProjectConfiguration.xlsx"),
    outputDir = withr::local_tempdir(),
    silent = TRUE
  ))
  project <- suppressWarnings(loadProject(jsonPath))
  # The scenarios workbook under the env-var folder was read, not dropped.
  expect_gt(length(project$definitions$scenarios), 0L)
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

  # The 5.x PITaskName-keyed layout imports all three tasks.
  expect_setequal(
    names(project$definitions$parameterIdentification),
    c("aciclovirsimple", "aciclovirsimplepathid", "aciclovirmultiscenario")
  )
})

# An output mapping that names its output path by full OSPS path (not by id) is
# rewritten to the id of the matching `outputPaths` definition, so the reference
# resolves rather than dangling (#1158).
test_that(".resolvePIOutputPathRefs rewrites a full-path mapping to the output-path id", {
  outputPaths <- list(aciclovir_pvb = "Organism|PVB|Aciclovir|Plasma")
  tasks <- list(list(
    id = "t1",
    outputMappings = list(
      list(id = "m1", outputPath = "Organism|PVB|Aciclovir|Plasma"),
      list(id = "m2", outputPath = "already_an_id")
    )
  ))
  resolved <- .resolvePIOutputPathRefs(tasks, outputPaths)
  expect_identical(
    resolved[[1]]$outputMappings[[1]]$outputPath,
    "aciclovir_pvb"
  )
  # A value with no matching definition is left as-is.
  expect_identical(
    resolved[[1]]$outputMappings[[2]]$outputPath,
    "already_an_id"
  )
})

# Blank/NA-cell guards in the 5.x parsers (PR #1160 review). A real legacy
# workbook can carry a trailing blank row or an empty cell; these must be
# skipped or dropped, not turned into an injected `NA`, a `"...|NA"` path, or a
# stringified boolean.

test_that("the individuals import skips a blank IndividualId row rather than injecting NA", {
  # A trailing blank-id biometrics row plus a parameter-set sheet used to inject
  # NA into every individual's parameterSets (an all-NA logical index returns a
  # vector of NAs, not an empty one).
  indivDf <- data.frame(
    IndividualId = c("Indiv1", NA),
    Species = c("Human", NA),
    Population = c("European_ICRP_2002", NA),
    Gender = c("MALE", NA),
    `Weight [kg]` = c(73, NA),
    `Height [cm]` = c(176, NA),
    `Age [year(s)]` = c(30, NA),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  individuals <- .parseExcelIndividuals(indivDf)
  paramSheetNames <- "Indiv1"
  sheetCanonical <- vapply(paramSheetNames, .canonicalizeOneId, character(1))
  linked <- lapply(individuals, function(indiv) {
    indivCanonical <- .canonicalizeOneId(indiv$id)
    if (is.na(indivCanonical)) {
      return(indiv)
    }
    match <- paramSheetNames[indivCanonical == sheetCanonical]
    if (length(match) > 0L) {
      indiv$parameterSets <- as.list(unique(c(
        unlist(indiv$parameterSets),
        match
      )))
    }
    indiv
  })
  expect_identical(unlist(linked[[1]]$parameterSets), "Indiv1")
  # The blank-id row gains no parameterSets (no injected NA).
  expect_null(linked[[2]]$parameterSets)
})

test_that("the individuals import defaults a blank Gender cell to UNKNOWN", {
  # An animal individual whose only valid PK-Sim gender is UNKNOWN carries no
  # Gender in the sheet; the importer must default it rather than write NA,
  # which the validator would flag as a critical error.
  indivDf <- data.frame(
    IndividualId = c("Human1", "Dog1"),
    Species = c("Human", "Dog"),
    Population = c("European_ICRP_2002", "Beagle"),
    Gender = c("MALE", NA),
    `Weight [kg]` = c(73, 10),
    `Height [cm]` = c(176, NA),
    `Age [year(s)]` = c(30, NA),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  individuals <- .parseExcelIndividuals(indivDf)
  expect_identical(individuals[[1]]$gender, "MALE")
  expect_identical(individuals[[2]]$gender, "UNKNOWN")
})

test_that("the individuals import treats a blank-string Gender cell as absent", {
  # A cell holding "" or only whitespace is blank, not a gender; it must
  # default to UNKNOWN rather than import as an invalid empty gender.
  indivDf <- data.frame(
    IndividualId = c("Empty1", "Spaces1"),
    Species = c("Dog", "Dog"),
    Population = c("Beagle", "Beagle"),
    Gender = c("", "   "),
    `Weight [kg]` = c(10, 10),
    `Height [cm]` = c(NA, NA),
    `Age [year(s)]` = c(NA, NA),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  individuals <- .parseExcelIndividuals(indivDf)
  expect_identical(individuals[[1]]$gender, "UNKNOWN")
  expect_identical(individuals[[2]]$gender, "UNKNOWN")
})

test_that("the individuals and populations import stay quiet on a sheet with no Protein Ontogenies column", {
  # `readExcel()` hands the parsers a tibble, and a tibble's `$` warns about an
  # unknown column, so a workbook omitting the optional `Protein Ontogenies`
  # column used to leak a raw "Unknown or uninitialised column" warning to the
  # user. The sheets are built as tibbles here on purpose: a data.frame does not
  # warn, which is why the tests above never caught this.
  indivDf <- dplyr::tibble(
    IndividualId = "Indiv1",
    Species = "Human",
    Population = "European_ICRP_2002",
    Gender = "MALE",
    `Weight [kg]` = 73,
    `Height [cm]` = 176,
    `Age [year(s)]` = 30
  )
  expect_no_warning(individuals <- .parseExcelIndividuals(indivDf))
  expect_null(individuals[[1]]$proteinOntogenies)

  popDf <- dplyr::tibble(
    PopulationName = "Pop1",
    species = "Human",
    population = "European_ICRP_2002",
    numberOfIndividuals = 10
  )
  expect_no_warning(populations <- .parseExcelPopulations(popDf))
  expect_null(populations[[1]]$proteinOntogenies)

  # A missing *required* column is a different case: it has no silent reading,
  # and staying quiet about it would surface later as a zero-length value that
  # names nothing.
  expect_snapshot(
    error = TRUE,
    .parseExcelIndividuals(dplyr::tibble(Species = "Human", Gender = "MALE"))
  )
  expect_snapshot(
    error = TRUE,
    .parseExcelPopulations(dplyr::tibble(species = "Human"))
  )

  # A sheet that does carry the column still reads its value.
  withOntogenies <- dplyr::tibble(
    IndividualId = "Indiv2",
    Species = "Human",
    Population = "European_ICRP_2002",
    Gender = "MALE",
    `Weight [kg]` = 73,
    `Height [cm]` = 176,
    `Age [year(s)]` = 30,
    `Protein Ontogenies` = "CYP3A4"
  )
  expect_identical(
    .parseExcelIndividuals(withOntogenies)[[1]]$proteinOntogenies,
    "CYP3A4"
  )
})

test_that("hand-authored ontogenies survive an Excel round trip without drifting", {
  # An authoring call stores one entry per ontogeny, and the import reads the
  # workbook back into that same shape, so exporting an authored project and
  # reading the workbook returns the value it started with and the two sections
  # carrying ontogenies are not reported as drifted. Importing the pairs as one
  # comma-joined string instead differs from the authored value from the second
  # ontogeny on, which no authored project could ever get back in sync.
  tp <- with_temp_project()
  project <- tp$project
  ontogenies <- c("CYP3A4:CYP3A4", "CYP2D6:CYP2C8")
  setIndividual(project, "adult_male", proteinOntogenies = ontogenies)
  setPopulation(project, "european_adults", proteinOntogenies = ontogenies)
  saveProject(project)
  suppressMessages(exportProjectToExcel(
    project,
    outputDir = tp$path,
    overwrite = TRUE,
    silent = TRUE
  ))

  reimported <- suppressWarnings(loadProject(importProjectFromExcel(
    file.path(tp$path, "Project.xlsx"),
    outputDir = withr::local_tempdir(),
    silent = TRUE
  )))
  expect_identical(
    reimported$definitions$individuals[["adult_male"]]$proteinOntogenies,
    ontogenies
  )
  expect_identical(
    reimported$definitions$populations[["european_adults"]]$proteinOntogenies,
    ontogenies
  )

  # The example project has its own round-trip drift in other sections, so the
  # verdict here is per section rather than the overall `excel_in_sync`.
  status <- suppressWarnings(suppressMessages(
    projectStatus(project, silent = TRUE)
  ))
  drifted <- names(status$details$excel$data_changes)
  expect_false("individuals" %in% drifted)
  expect_false("populations" %in% drifted)
})

test_that(".parseExcelObservedData keeps a subfolder path rather than truncating to basename", {
  # The loader resolves `file` under `dataFolder`, so a file named in a subfolder
  # must keep its relative path; truncating to the basename would make it
  # unresolvable on load.
  work <- withr::local_tempdir()
  dir.create(file.path(work, "Data", "Sub"), recursive = TRUE)
  dataPath <- file.path(work, "Data", "Sub", "Values.xlsx")
  .writeExcel(list(Sheet1 = data.frame(x = 1)), dataPath)
  prop <- function(name) {
    switch(name, dataFile = "Sub/Values.xlsx", dataFolder = "Data", NULL)
  }
  result <- .parseExcelObservedData(list(), prop, work)
  entry <- result$observedData[[1]]
  expect_identical(entry$file, "Sub/Values.xlsx")
  # The declaration states its own canonicalized id, so the section is keyed and
  # the definition file is `values.json` rather than `Values.xlsx.json`.
  expect_identical(names(result$observedData), "values")
  expect_identical(entry$id, "values")
})

# The project records a single experimental-data workbook under `dataFolder`.
# The importer reifies it as one `excel` observed-data definition listing the
# workbook's sheets, so a plot or PI mapping that references observed data has
# something to resolve against (#1158).
test_that("the Excel import reifies the configured data file as observed data", {
  out <- withr::local_tempdir()
  jsonPath <- suppressWarnings(importProjectFromExcel(
    testProjectExcelPath(),
    outputDir = out,
    silent = TRUE
  ))
  project <- suppressWarnings(loadProject(jsonPath))

  expect_length(project$definitions$observedData, 1L)
  entry <- .unwrapDefinitionList(project$definitions$observedData)[[1]]
  expect_identical(entry$type, "excel")
  expect_identical(entry$file, "TestProject_TimeValuesData.xlsx")
  expect_identical(
    entry$importerConfiguration,
    "esqlabs_dataImporter_configuration.xml"
  )
  expect_true("Laskin 1982.Group A" %in% unlist(entry$sheets))
})

# A configured `dataFile` that is not on disk is a migration gap the user should
# see: the importer warns and imports no observed data rather than aborting or
# silently proceeding (#1158).
test_that(".parseExcelObservedData warns and skips when the data file is absent", {
  prop <- function(name) {
    switch(name, dataFile = "Missing.xlsx", dataFolder = "Data/", NULL)
  }
  expect_warning(
    result <- .parseExcelObservedData(
      list(),
      prop,
      testthat::test_path("data", "TestProjectExcel")
    ),
    "was not found"
  )
  expect_null(result$observedData)
})

# An absolute `dataFolder` pointing at a synced drive is common in real 5.x
# projects. It leaves the data unavailable to the imported project, which is the
# missing-data-file situation from the user's point of view, so it is reported
# and skipped rather than aborting the whole migration (#1182).
test_that(".parseExcelObservedData warns and skips an out-of-project dataFolder", {
  work <- withr::local_tempdir()
  shared <- withr::local_tempdir()
  .writeExcel(
    list(Sheet1 = data.frame(x = 1)),
    file.path(shared, "Values.xlsx")
  )

  for (folder in c(shared, "../elsewhere")) {
    prop <- function(name) {
      switch(name, dataFile = "Values.xlsx", dataFolder = folder, NULL)
    }
    expect_warning(
      result <- .parseExcelObservedData(list(), prop, work),
      "points outside the project folder"
    )
    expect_null(result$observedData)
  }

  # The path is absolute by definition here, so echoing it would put the user's
  # account name in the message.
  prop <- function(name) {
    switch(name, dataFile = "Values.xlsx", dataFolder = shared, NULL)
  }
  warning <- tryCatch(
    .parseExcelObservedData(list(), prop, work),
    warning = function(w) conditionMessage(w)
  )
  expect_false(grepl(shared, warning, fixed = TRUE))
})

# `${VAR}` is the sanctioned way to keep the data outside the project, so it is
# expanded and exempt from containment rather than read as a literal folder name
# (which resolved to nothing and lost the data with a misleading "not found").
test_that(".parseExcelObservedData expands a ${VAR} dataFolder", {
  work <- withr::local_tempdir()
  shared <- withr::local_tempdir()
  .writeExcel(
    list(Sheet1 = data.frame(x = 1)),
    file.path(shared, "Values.xlsx")
  )
  withr::local_envvar(ESQLABSR_TEST_DATA = shared)

  prop <- function(name) {
    switch(
      name,
      dataFile = "Values.xlsx",
      dataFolder = "${ESQLABSR_TEST_DATA}",
      NULL
    )
  }
  expect_silent(result <- .parseExcelObservedData(list(), prop, work))
  expect_named(result$observedData, "values")
  expect_identical(result$observedData[[1]]$sheets, list("Sheet1"))
})

# A `dataFile` that climbs out of its `dataFolder` is the same situation one
# level down, and gets the same warn-and-skip (#1182). It names the boundary it
# actually crossed: such a file is usually still inside the project, so saying
# "outside the project folder" would be untrue and its remedy a no-op.
test_that(".parseExcelObservedData warns and skips a dataFile outside dataFolder", {
  prop <- function(name) {
    switch(name, dataFile = "../Values.xlsx", dataFolder = "Data", NULL)
  }
  expect_warning(
    result <- .parseExcelObservedData(
      list(),
      prop,
      testthat::test_path("data", "TestProjectExcel")
    ),
    "dataFile.+points outside.+dataFolder"
  )
  expect_null(result$observedData)

  warning <- tryCatch(
    .parseExcelObservedData(
      list(),
      prop,
      testthat::test_path("data", "TestProjectExcel")
    ),
    warning = function(w) conditionMessage(w)
  )
  expect_false(grepl("outside the project folder", warning, fixed = TRUE))
})

# The whole point of the downgrade: a project whose data sits on a synced drive
# migrates unattended. The value stays in `filePaths` as the workbook spells it,
# and the imported project loads, because nothing reads the folder once no
# observed data was imported (#1182).
test_that("an absolute dataFolder no longer blocks the import", {
  projectDir <- localExcelProjectDir()
  configPath <- file.path(projectDir, "ProjectConfiguration.xlsx")

  shared <- withr::local_tempdir()
  file.copy(
    list.files(file.path(projectDir, "Data"), full.names = TRUE),
    shared
  )
  props <- readExcel(configPath)
  props$Value[props$Property == "dataFolder"] <- shared
  .writeExcel(props, configPath)

  jsonPath <- suppressWarnings(importProjectFromExcel(
    configPath,
    outputDir = withr::local_tempdir(),
    silent = TRUE
  ))
  project <- suppressWarnings(loadProject(jsonPath))

  expect_length(project$definitions$observedData, 0L)
  expect_identical(
    jsonlite::fromJSON(jsonPath, simplifyVector = FALSE)$filePaths$dataFolder,
    shared
  )
})

# A parameter workbook routinely carries a notes, organ-list, or fit-bounds
# sheet beside its parameter sheets. Recognizing it by its columns and skipping
# it keeps one such sheet from stopping the whole migration, and keeps the row
# loop from emitting entries with empty paths and value-less parameters, which
# is what a per-cell guard would have produced (#1181).
test_that(".parseExcelParameterSheets skips a sheet without the parameter columns", {
  path <- file.path(withr::local_tempdir(), "ModelParameters.xlsx")
  .writeExcel(
    list(
      Global = data.frame(
        `Container Path` = "Organism",
        `Parameter Name` = "Age",
        Value = 30,
        Units = "year(s)",
        check.names = FALSE,
        stringsAsFactors = FALSE
      ),
      `Organ notes` = data.frame(
        Organ = c("Liver", "Kidney"),
        Comment = c("see ref", "TBD"),
        stringsAsFactors = FALSE
      )
    ),
    path
  )

  expect_warning(
    result <- .parseExcelParameterSheets(path),
    "Organ notes"
  )
  expect_named(result, "Global")
  expect_length(result$Global, 1L)
})

# A sheet name is free text, so it can hold `{}` (`Fit {old}`, `PK {2019}`). The
# warning must quote it, not evaluate it: rendering the value into the message
# text and letting the emitting `cli_warn()` glue-parse that text again would
# abort the import on exactly the kind of scratch sheet this skip exists to
# tolerate.
test_that("the skipped-sheet warning quotes a sheet name containing braces", {
  path <- file.path(withr::local_tempdir(), "ModelParameters.xlsx")
  .writeExcel(
    list(`Notes {draft}` = data.frame(Organ = "Liver", Comment = "x")),
    path
  )

  expect_warning(
    result <- .parseExcelParameterSheets(path),
    "Notes \\{draft\\}"
  )
  expect_length(result, 0L)
})

# An exported parameter set with no entries is written as a header-only sheet.
# It carries the four columns, so it is a parameter sheet and must survive the
# round trip as an empty set rather than being mistaken for a notes sheet.
test_that(".parseExcelParameterSheets keeps a header-only parameter sheet", {
  path <- file.path(withr::local_tempdir(), "ModelParameters.xlsx")
  .writeExcel(
    list(Empty = .parameterStructuresToExcelSheets(list(Empty = list()))$Empty),
    path
  )

  expect_silent(result <- .parseExcelParameterSheets(path))
  expect_named(result, "Empty")
  expect_length(result$Empty, 0L)
})

# The issue's reproducer: a notes sheet in the model-parameters workbook used to
# abort the whole import with a bare `missing value where TRUE/FALSE needed`
# naming neither the file nor the sheet (#1181).
test_that("a non-parameter sheet in a parameter workbook no longer aborts the import", {
  projectDir <- localExcelProjectDir()
  paramsFile <- file.path(projectDir, "Configurations", "ModelParameters.xlsx")

  sheetNames <- readxl::excel_sheets(paramsFile)
  sheets <- stats::setNames(
    lapply(sheetNames, function(s) readExcel(paramsFile, sheet = s)),
    sheetNames
  )
  sheets[["Organ notes"]] <- data.frame(
    Organ = c("Liver", "Kidney"),
    Comment = c("see ref", "TBD"),
    stringsAsFactors = FALSE
  )
  .writeExcel(sheets, paramsFile)

  jsonPath <- suppressWarnings(importProjectFromExcel(
    file.path(projectDir, "ProjectConfiguration.xlsx"),
    outputDir = withr::local_tempdir(),
    silent = TRUE
  ))
  project <- suppressWarnings(loadProject(jsonPath))

  # The notes sheet contributes no set; the real parameter sheets still do.
  expect_false("organ_notes" %in% names(project$definitions$parameterSets))
  expect_true("global" %in% names(project$definitions$parameterSets))
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

# An import run from a script has no other sign of what was written, or that it
# succeeded, so the summary is gated on `silent` alone, never on the session
# being interactive.
# Regression (#1174): the import used to name the project file after the
# workbook it read, so a legacy `ProjectConfiguration.xlsx` produced a
# `ProjectConfiguration.json` that the `Project.json` default of `loadProject()`
# and `initProject()` did not match. The name is now canonical whatever the
# workbook is called, so the obvious next call after an import works.
test_that("importProjectFromExcel writes Project.json whatever the workbook is called", {
  outputDir <- withr::local_tempdir()

  jsonPath <- suppressWarnings(importProjectFromExcel(
    testProjectExcelPath(),
    outputDir = outputDir,
    silent = TRUE
  ))

  expect_identical(fs::path_file(jsonPath), "Project.json")
  expect_false(file.exists(file.path(outputDir, "ProjectConfiguration.json")))
  expect_s3_class(
    suppressWarnings(loadProject(file.path(outputDir, "Project.json"))),
    "Project"
  )
})

test_that("importProjectFromExcel honours projectFileName, and saving keeps it", {
  outputDir <- withr::local_tempdir()

  jsonPath <- suppressWarnings(importProjectFromExcel(
    testProjectExcelPath(),
    outputDir = outputDir,
    silent = TRUE,
    projectFileName = "MyStudy.json"
  ))

  expect_identical(fs::path_file(jsonPath), "MyStudy.json")
  expect_false(file.exists(file.path(outputDir, "Project.json")))

  # A project saves back to the file it was loaded from, so the chosen name
  # survives an edit rather than forking a stray `Project.json`.
  project <- suppressWarnings(loadProject(jsonPath))
  project$info$description <- "edited"
  saveProject(project)

  expect_false(file.exists(file.path(outputDir, "Project.json")))
  expect_identical(
    suppressWarnings(loadProject(jsonPath))$info$description,
    "edited"
  )
})

test_that("importProjectFromExcel adds the .json extension to projectFileName", {
  outputDir <- withr::local_tempdir()

  jsonPath <- suppressWarnings(importProjectFromExcel(
    testProjectExcelPath(),
    outputDir = outputDir,
    silent = TRUE,
    projectFileName = "MyStudy"
  ))

  expect_identical(fs::path_file(jsonPath), "MyStudy.json")
})

# The extension is appended, not set: `fs::path_ext_set()` would read the `.v1`
# of a dotted stem as an extension and replace it, silently collapsing
# `trial.v1` and `trial.v2` onto one project file.
test_that("importProjectFromExcel keeps a dotted projectFileName stem intact", {
  outputDir <- withr::local_tempdir()

  first <- suppressWarnings(importProjectFromExcel(
    testProjectExcelPath(),
    outputDir = outputDir,
    silent = TRUE,
    projectFileName = "trial.v1"
  ))
  second <- suppressWarnings(importProjectFromExcel(
    testProjectExcelPath(),
    outputDir = outputDir,
    silent = TRUE,
    overwrite = TRUE,
    projectFileName = "trial.v2"
  ))

  expect_identical(fs::path_file(first), "trial.v1.json")
  expect_identical(fs::path_file(second), "trial.v2.json")
})

test_that("importProjectFromExcel rejects a projectFileName that is a path", {
  expect_snapshot(
    error = TRUE,
    importProjectFromExcel(
      testProjectExcelPath(),
      outputDir = withr::local_tempdir(),
      silent = TRUE,
      projectFileName = "../Project.json"
    )
  )
})

test_that("importProjectFromExcel says what is wrong with a projectFileName that is no name at all", {
  # An empty string, an `NA` and a number are not names that "contain a path
  # separator", so the rejection describes the value it got instead (#1213).
  for (bad in list("", NA_character_, 42, c("a", "b"))) {
    err <- expect_error(
      importProjectFromExcel(
        testProjectExcelPath(),
        outputDir = withr::local_tempdir(),
        silent = TRUE,
        projectFileName = bad
      )
    )
    expect_match(conditionMessage(err), "not a single non-empty string")
    expect_no_match(conditionMessage(err), "contains a path separator")
  }
})

test_that("importProjectFromExcel reports what it produced, and stays quiet under silent", {
  projectDir <- localExcelProjectDir()
  configPath <- file.path(projectDir, "ProjectConfiguration.xlsx")

  summaryText <- paste(
    utils::capture.output(
      suppressWarnings(importProjectFromExcel(
        configPath,
        outputDir = withr::local_tempdir()
      )),
      type = "message"
    ),
    collapse = "\n"
  )

  # The output path and the per-section counts. The folders the import copies
  # are expected, so the summary does not list them.
  expect_match(summaryText, "Project.json", fixed = TRUE)
  expect_match(summaryText, "Scenarios: 8", fixed = TRUE)
  expect_no_match(summaryText, "Copied", fixed = TRUE)

  expect_silent(suppressWarnings(importProjectFromExcel(
    configPath,
    outputDir = withr::local_tempdir(),
    silent = TRUE
  )))
})

# A definition references a model or a data file by a path relative to the
# project folder, so those folders have to travel with the definitions for an
# import into a different folder to resolve. Otherwise the output is a
# definitions tree pointing at files that are not there.
test_that("importProjectFromExcel copies the referenced input folders into a separate outputDir", {
  projectDir <- localExcelProjectDir()

  outputDir <- withr::local_tempdir()
  jsonPath <- suppressWarnings(importProjectFromExcel(
    file.path(projectDir, "ProjectConfiguration.xlsx"),
    outputDir = outputDir,
    silent = TRUE
  ))

  # The models and the observed data travelled with the definitions.
  expect_true(file.exists(file.path(
    outputDir,
    "Models",
    "Simulations",
    "Aciclovir.pkml"
  )))
  expect_true(file.exists(file.path(
    outputDir,
    "Data",
    "TestProject_TimeValuesData.xlsx"
  )))
  # Including an asset nothing references statically (the importer config).
  expect_true(file.exists(file.path(
    outputDir,
    "Data",
    "esqlabs_dataImporter_configuration.xml"
  )))

  # So the imported project no longer validates with File-Not-Found warnings for
  # its own models and data.
  report <- suppressWarnings(validateProject(suppressWarnings(loadProject(
    jsonPath
  ))))
  fileWarnings <- unlist(lapply(report, function(section) {
    vapply(section$warnings, function(w) w$category, character(1))
  }))
  expect_false("File Not Found" %in% fileWarnings)
})

# An Excel project spells `populationsFolder` as a folder name under the
# configurations folder, while a project resolves working folders against its own
# root. Read literally the folder was neither copied nor resolvable, so a
# csv-population scenario failed at run time with `validateProject()` reporting
# nothing (#1213).
test_that("importProjectFromExcel brings the population csv folder along and resolves it", {
  projectDir <- localExcelProjectDir()

  outputDir <- withr::local_tempdir()
  jsonPath <- suppressWarnings(importProjectFromExcel(
    file.path(projectDir, "ProjectConfiguration.xlsx"),
    outputDir = outputDir,
    silent = TRUE
  ))
  project <- suppressWarnings(loadProject(jsonPath))

  # The folder the project resolves is the folder the csv file was copied into,
  # which is the whole point: the two used to disagree.
  expect_true(dir.exists(project$paths$populationsFolder))
  expect_true(file.exists(file.path(
    project$paths$populationsFolder,
    "TestPopulation.csv"
  )))
  # And the scenario reading it no longer validates clean-but-unrunnable.
  report <- suppressWarnings(validateProject(project))
  notFound <- unlist(lapply(report, function(section) {
    vapply(section$warnings, function(w) w$category, character(1))
  }))
  expect_false("File Not Found" %in% notFound)
})

test_that("importProjectFromExcel does not copy the results folder or the Excel workbooks", {
  projectDir <- localExcelProjectDir()

  outputDir <- withr::local_tempdir()
  suppressWarnings(importProjectFromExcel(
    file.path(projectDir, "ProjectConfiguration.xlsx"),
    outputDir = outputDir,
    silent = TRUE
  ))

  # `outputFolder` holds what the project writes, not what it reads.
  expect_false(dir.exists(file.path(outputDir, "Results")))
  # The workbooks are the source, not an asset of the JSON project. The
  # configurations folder itself does travel, because the population csv files
  # the project reads live under it, so what is asserted is that no workbook
  # came with them.
  expect_length(
    list.files(
      file.path(outputDir, "Configurations"),
      pattern = "\\.xlsx$",
      recursive = TRUE
    ),
    0L
  )
})

# A `../`-climbing folder value names something the project does not own.
# Copying it would read outside the source project and write outside outputDir,
# overwriting whatever sits beside it.
test_that("importProjectFromExcel refuses to copy a folder that escapes the project", {
  projectDir <- localExcelProjectDir()

  # A sibling of the project, which must not be touched.
  bystander <- file.path(dirname(projectDir), "Bystander")
  dir.create(bystander)
  writeLines("keep me", file.path(bystander, "keep.txt"))

  configPath <- file.path(projectDir, "ProjectConfiguration.xlsx")
  config <- readExcel(configPath)
  config$Value[config$Property == "modelFolder"] <- "../Bystander"
  .writeExcel(config, configPath)

  outputDir <- withr::local_tempdir()
  expect_warning(
    withCallingHandlers(
      importProjectFromExcel(configPath, outputDir = outputDir),
      esqlabsR_importSkippedObservedData = function(cnd) {
        invokeRestart("muffleWarning")
      }
    ),
    "not copied"
  )

  # Nothing was written outside outputDir, and the escaping folder did not
  # travel into it either.
  expect_false(dir.exists(file.path(dirname(outputDir), "Bystander")))
  expect_false(dir.exists(file.path(outputDir, "Bystander")))
  expect_true(file.exists(file.path(bystander, "keep.txt")))
})

# The asset copy makes a *user-facing* import runnable. `.compareJsonToExcel()`
# imports into a throwaway folder purely to serialize and diff it, and that runs
# on every `projectStatus()` read, so copying the whole asset tree there would
# turn a cheap status query into a recursive tree copy.
test_that("importProjectFromExcel skips the asset copy under copyAssets = FALSE", {
  projectDir <- localExcelProjectDir()

  outputDir <- withr::local_tempdir()
  suppressWarnings(importProjectFromExcel(
    file.path(projectDir, "ProjectConfiguration.xlsx"),
    outputDir = outputDir,
    silent = TRUE,
    copyAssets = FALSE
  ))

  # The definitions are written; none of the referenced folders travelled.
  expect_true(dir.exists(file.path(outputDir, "definitions")))
  expect_false(dir.exists(file.path(outputDir, "Models")))
  expect_false(dir.exists(file.path(outputDir, "Data")))
})

test_that("projectStatus() does not copy the asset tree into its comparison snapshot", {
  projectDir <- localExcelProjectDir()

  outputDir <- withr::local_tempdir()
  jsonPath <- suppressWarnings(importProjectFromExcel(
    file.path(projectDir, "ProjectConfiguration.xlsx"),
    outputDir = outputDir,
    silent = TRUE
  ))
  project <- suppressWarnings(loadProject(jsonPath))

  # Count the copies the status read performs by watching `fs::dir_copy()`.
  copies <- 0L
  local_mocked_bindings(
    dir_copy = function(...) {
      copies <<- copies + 1L
      invisible(NULL)
    },
    .package = "fs"
  )
  suppressWarnings(projectStatus(project, silent = TRUE))

  expect_identical(copies, 0L)
})

# `overwrite` governs the definition tree; it has to govern the assets too, or a
# user who curates a model or data file in the output folder and then imports the
# legacy workbook beside it loses that file with no warning and no way to decline.
test_that("importProjectFromExcel leaves a non-empty asset folder alone unless overwrite = TRUE", {
  projectDir <- localExcelProjectDir()
  configPath <- file.path(projectDir, "ProjectConfiguration.xlsx")

  outputDir <- withr::local_tempdir()
  curated <- file.path(outputDir, "Data", "curated.txt")
  dir.create(dirname(curated), recursive = TRUE)
  writeLines("hand-curated", curated)

  expect_warning(
    withCallingHandlers(
      importProjectFromExcel(configPath, outputDir = outputDir),
      esqlabsR_importSkippedObservedData = function(cnd) {
        invokeRestart("muffleWarning")
      }
    ),
    "not copied"
  )
  # The curated file survives, and the workbook's own data did not land on it.
  expect_identical(readLines(curated), "hand-curated")
  expect_false(file.exists(file.path(
    outputDir,
    "Data",
    "TestProject_TimeValuesData.xlsx"
  )))

  # With overwrite = TRUE the assets do travel.
  suppressWarnings(importProjectFromExcel(
    configPath,
    outputDir = outputDir,
    overwrite = TRUE,
    silent = TRUE
  ))
  expect_true(file.exists(file.path(
    outputDir,
    "Data",
    "TestProject_TimeValuesData.xlsx"
  )))
})

# `fs::path_norm()` is lexical: it never resolves a symlink, so the same folder
# reached by two spellings compared unequal and every asset folder was copied
# onto itself.
test_that("importProjectFromExcel detects an in-place import through a symlinked path", {
  skip_on_os("windows")
  projectDir <- localExcelProjectDir()

  # Reach the very same directory by a symlink, and hand that spelling to
  # outputDir while the workbook is named by its real path.
  linked <- file.path(dirname(projectDir), "linked")
  file.symlink(projectDir, linked)

  before <- sort(list.files(projectDir, recursive = TRUE))
  suppressWarnings(importProjectFromExcel(
    file.path(projectDir, "ProjectConfiguration.xlsx"),
    outputDir = linked,
    overwrite = TRUE,
    silent = TRUE
  ))
  after <- sort(list.files(projectDir, recursive = TRUE))

  # Recognized as in place: the project file and its definition files were
  # added, no folder was duplicated into its own subtree.
  added <- setdiff(after, before)
  expect_match(added[added != "Project.json"], "^definitions/", all = TRUE)
  expect_false(dir.exists(file.path(projectDir, "Models", "Models")))
  expect_false(dir.exists(file.path(projectDir, "Data", "Data")))
})

test_that("importProjectFromExcel names a referenced folder the Excel project does not have", {
  projectDir <- localExcelProjectDir()

  # The configuration keeps naming `Data/`, but the folder is gone: there is
  # nothing to copy, and the user has to be told which folder to place.
  unlink(file.path(projectDir, "Data"), recursive = TRUE)

  expect_warning(
    withCallingHandlers(
      importProjectFromExcel(
        file.path(projectDir, "ProjectConfiguration.xlsx"),
        outputDir = withr::local_tempdir()
      ),
      # The absent data file also warns from the observed-data parse; muffle it
      # so the assertion is on the asset report alone.
      esqlabsR_importSkippedObservedData = function(cnd) {
        invokeRestart("muffleWarning")
      }
    ),
    "Data/"
  )
})

test_that("importProjectFromExcel in place leaves the referenced folders untouched", {
  projectDir <- localExcelProjectDir()

  before <- sort(list.files(projectDir, recursive = TRUE))
  suppressWarnings(importProjectFromExcel(
    file.path(projectDir, "ProjectConfiguration.xlsx"),
    silent = TRUE,
    overwrite = TRUE
  ))
  after <- sort(list.files(projectDir, recursive = TRUE))

  # Everything the import added is the project file or a definition file; no
  # folder was copied onto itself, which would have duplicated the models and
  # data into subfolders.
  added <- setdiff(after, before)
  expect_match(added[added != "Project.json"], "^definitions/", all = TRUE)
})

# A data folder of `"."` is the project folder itself, which is never copied as
# a folder: that would bring every workbook along. The data workbook and the
# importer configuration the observed-data definition names have to travel
# anyway, or the imported project cannot read its observed data (#1409).
test_that("importProjectFromExcel copies the observed-data files when the data folder is the project folder", {
  projectDir <- localExcelProjectWithRootData()
  outputDir <- withr::local_tempdir()

  # Nothing is reported: every file the import needed was copied.
  expect_identical(
    importAssetLines(importProjectFromExcel(
      file.path(projectDir, "ProjectConfiguration.xlsx"),
      outputDir = outputDir
    )),
    character()
  )

  # The two data files and nothing else from the project folder: no workbook,
  # and not the `ProjectConfiguration.json` the fixture also holds.
  expect_setequal(
    list.files(outputDir),
    c(
      "Configurations",
      "definitions",
      "esqlabs_dataImporter_configuration.xml",
      "Models",
      "Project.json",
      "TestProject_TimeValuesData.xlsx"
    )
  )
  # The fixture's workbooks carry an unresolved PI reference, which the load
  # warns about; it has nothing to do with the observed-data source.
  observedData <- loadObservedData(suppressWarnings(loadProject(outputDir)))
  expect_gt(length(observedData), 0L)
})

test_that("importProjectFromExcel keeps an observed-data file already in outputDir and says so", {
  projectDir <- localExcelProjectWithRootData()
  outputDir <- withr::local_tempdir()
  curated <- file.path(outputDir, "TestProject_TimeValuesData.xlsx")
  writeLines("hand-curated", curated)

  expect_snapshot(writeLines(importAssetLines(importProjectFromExcel(
    file.path(projectDir, "ProjectConfiguration.xlsx"),
    outputDir = outputDir
  ))))
  expect_identical(readLines(curated), "hand-curated")
  expect_true(file.exists(file.path(
    outputDir,
    "esqlabs_dataImporter_configuration.xml"
  )))

  # With overwrite = TRUE the data workbook replaces the file.
  suppressMessages(importProjectFromExcel(
    file.path(projectDir, "ProjectConfiguration.xlsx"),
    outputDir = outputDir,
    overwrite = TRUE
  ))
  expect_identical(
    tools::md5sum(curated)[[1]],
    tools::md5sum(file.path(projectDir, "TestProject_TimeValuesData.xlsx"))[[
      1
    ]]
  )
})

# An observed DataCombined row may name a scenario just as a simulated row does.
# Both are the same kind of reference, so both must land on the canonical id;
# leaving the observed one at its Excel spelling puts two casings of one scenario
# in a single definition file and hides the observed block from any check keyed on
# the canonical id.
test_that("importProjectFromExcel canonicalizes an observed dataCombined entry's scenario reference", {
  projectDir <- localExcelProjectDir()
  plotsFile <- file.path(projectDir, "Configurations", "Plots.xlsx")

  # Name a scenario on the observed rows, with the mixed-case spelling the
  # Scenarios sheet uses.
  sheets <- readxl::excel_sheets(plotsFile)
  contents <- stats::setNames(
    lapply(sheets, function(s) readExcel(plotsFile, sheet = s)),
    sheets
  )
  observedRows <- contents$DataCombined$dataType == "observed"
  contents$DataCombined$scenario[observedRows] <- "TestScenario"
  .writeExcel(contents, plotsFile)

  jsonPath <- suppressWarnings(importProjectFromExcel(
    file.path(projectDir, "ProjectConfiguration.xlsx"),
    outputDir = withr::local_tempdir(),
    silent = TRUE
  ))
  project <- suppressWarnings(loadProject(jsonPath))

  dataCombined <- .unwrapDefinitionList(project$definitions$dataCombined)
  observedScenarios <- unlist(lapply(
    dataCombined,
    function(dc) vapply(dc$observed, function(e) e$scenario, character(1))
  ))
  expect_setequal(observedScenarios, "testscenario")
  # The simulated block was already canonical; both blocks now agree.
  simulatedScenarios <- unlist(lapply(
    dataCombined,
    function(dc) vapply(dc$simulated, function(e) e$scenario, character(1))
  ))
  expect_contains(simulatedScenarios, "testscenario")
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
# comma-decimal) skips its row, naming the sheet, row and cell, rather than
# silently becoming NA and serialising a value-less parameter into the JSON
# project. Skipping the row rather than aborting the import is what lets a
# project carrying a fit-bounds sheet migrate at all (#1189).
test_that(".parseExcelParameterSheets skips a non-numeric Value cell", {
  paramFile <- withr::local_tempfile(fileext = ".xlsx")
  df <- data.frame(
    `Container Path` = "Organism|A",
    `Parameter Name` = c("P", "Q", "R"),
    Value = c("not_a_number", "1,5", "2"),
    Units = "mg",
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  .writeExcel(list(Global = df), paramFile)

  # Not a snapshot: the warning names the workbook, whose path is a temp file
  # here, so a snapshot would record a machine-specific string.
  expect_warning(
    parsed <- .parseExcelParameterSheets(paramFile),
    class = "esqlabsR_importSkippedNonNumericRows"
  )
  # Each skipped row is named with its sheet, row and cell. The rows are the
  # workbook's, so the first data row is row 2, below the header.
  expect_warning(
    .parseExcelParameterSheets(paramFile),
    'row 2: "not_a_number"'
  )
  expect_warning(.parseExcelParameterSheets(paramFile), 'row 3: "1,5"')
  # Only the numeric row survives; a comma-decimal is text, so it is skipped
  # rather than silently read as 1.
  expect_length(parsed$Global, 1L)
  expect_identical(parsed$Global[[1]]$value, 2)
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

test_that(".compareJsonToExcel still reports a real edit as drift", {
  # The comparison deliberately ignores several differences the round trip does
  # not preserve (key order, definition order, storage type), so it needs a case
  # proving it has not been normalized into always saying "in sync".
  out <- withr::local_tempdir()
  excelPath <- testProjectExcelPath()
  jsonPath <- suppressWarnings(importProjectFromExcel(
    excelPath,
    outputDir = out,
    silent = TRUE
  ))
  project <- suppressWarnings(loadProject(jsonPath))

  addOutputPath(project, "drifted", "Organism|Liver|Volume")

  drifted <- suppressWarnings(.compareJsonToExcel(
    project = project,
    projectConfigPath = excelPath,
    silent = TRUE
  ))
  expect_false(drifted$excel_in_sync)
  expect_identical(drifted$details$data_changes$outputPaths, "data differs")
})

test_that(".compareJsonToExcel writes nothing", {
  # It answers a read-only question, so it must leave the Excel project folder
  # exactly as it found it (it used to run the importer for real against a
  # temporary directory).
  out <- withr::local_tempdir()
  excelPath <- testProjectExcelPath()
  jsonPath <- suppressWarnings(importProjectFromExcel(
    excelPath,
    outputDir = out,
    silent = TRUE
  ))
  project <- suppressWarnings(loadProject(jsonPath))

  excelDir <- dirname(excelPath)
  before <- list.files(excelDir, recursive = TRUE, all.files = TRUE)
  suppressWarnings(.compareJsonToExcel(
    project = project,
    projectConfigPath = excelPath,
    silent = TRUE
  ))
  expect_identical(
    list.files(excelDir, recursive = TRUE, all.files = TRUE),
    before
  )
})

# `.sortJsonSections()` puts the two sides of the Excel comparison into one
# order by serializing each record into a sort key, so that key has to be as
# discriminating as the `identical()` it feeds. Records tying on the key keep
# their source order, because `order(method = "radix")` is stable, and source
# order is exactly what the sort exists to normalize away.
#
# Every array section a real project holds carries a unique id per record
# (`id`), so no two records tie and the
# key's precision does not currently reach the verdict. This guards the sort
# itself rather than a live false-drift report: the records here are given the
# same id so they differ only past the 4th decimal, which is where jsonlite's
# default rounding would collapse them onto one key.
test_that(".sortJsonSections orders records that differ past the 4th decimal", {
  low <- list(id = "s", value = 1.00001)
  high <- list(id = "s", value = 1.00002)

  expect_identical(
    .sortJsonSections(list(section = list(low, high))),
    .sortJsonSections(list(section = list(high, low)))
  )
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
# The side-car is derived from the project file's name, so a project whose
# folder holds no matching workbook must say which file was looked for and how
# to produce it, rather than naming a fixed `Project.xlsx` it never checked.
test_that("projectStatus() names the missing Excel side-car it looked for", {
  outputDir <- withr::local_tempdir()
  jsonPath <- suppressWarnings(importProjectFromExcel(
    testProjectExcelPath(),
    outputDir = outputDir,
    silent = TRUE,
    projectFileName = "MyStudy"
  ))
  project <- suppressWarnings(loadProject(jsonPath))

  expect_message(
    suppressWarnings(projectStatus(project)),
    "MyStudy\\.xlsx.*exportProjectToExcel"
  )
  expect_identical(
    suppressWarnings(projectStatus(project, silent = TRUE))$excel_in_sync,
    NA
  )

  # The advice has to be followable: exporting writes the very workbook the
  # status check looked for, so the Excel axis stops reporting "cannot compare".
  suppressWarnings(exportProjectToExcel(
    project,
    outputDir = outputDir,
    silent = TRUE
  ))
  expect_true(file.exists(file.path(outputDir, "MyStudy.xlsx")))
  expect_false(is.na(
    suppressWarnings(projectStatus(project, silent = TRUE))$excel_in_sync
  ))
})

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

  # Non-silent: a warning surfaces naming the comparison failure. Snapshotted
  # whole because this is the second surface of the same unreadable-workbook
  # text, re-raised as one bullet of this warning: the abort's own header must
  # stay unglyphed for it to render here as a single bullet rather than a doubled
  # one, and only the rendered text shows that.
  expect_snapshot(
    transform = .redactProjectWorkbookPath,
    projectStatus(project, silent = FALSE)
  )
})

# Regression (#1125): when the Excel side-car cannot be re-imported for the
# comparison (here because two output-path ids in it collapse to one canonical
# id), the status check must catch that abort and report the Excel axis as the
# NA "cannot compare" state with a warning, rather than propagating a hard error
# out of `projectStatus()`. The collision is injected into the exported side-car
# (not the source), so the project itself loads cleanly and only the comparison
# re-import hits the collision.
test_that("projectStatus() does not abort on a side-car canonicalization collision", {
  work <- withr::local_tempdir()
  file.copy(
    list.files(
      testthat::test_path("data", "TestProjectExcel"),
      full.names = TRUE
    ),
    work,
    recursive = TRUE
  )

  jsonPath <- suppressWarnings(importProjectFromExcel(
    file.path(work, "ProjectConfiguration.xlsx"),
    outputDir = work,
    silent = TRUE
  ))
  project <- suppressWarnings(loadProject(jsonPath))
  # The fixture already carries Configurations workbooks, so exporting the
  # side-car over them needs overwrite = TRUE.
  suppressWarnings(exportProjectToExcel(
    project,
    outputDir = work,
    overwrite = TRUE,
    silent = TRUE
  ))

  # Rewrite the exported Scenarios workbook's OutputPaths sheet so two ids differ
  # only by case and collapse to one canonical id. The comparison's re-import of
  # this side-car then aborts, which the status check must catch.
  scenariosFile <- file.path(work, "Configurations", "Scenarios.xlsx")
  scenariosSheet <- readExcel(scenariosFile, sheet = "Scenarios")
  .writeExcel(
    list(
      OutputPaths = data.frame(
        OutputPathId = c("Aciclovir_PVB", "aciclovir_pvb"),
        OutputPath = c(
          "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)",
          "Organism|Fat|Intracellular|Aciclovir|Concentration in container"
        ),
        stringsAsFactors = FALSE
      ),
      Scenarios = scenariosSheet
    ),
    scenariosFile
  )

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

# A Scenarios sheet may spell booleans as `true`/`false` in any case; bare
# `as.logical()` turns the lower-case forms into NA (silently defaulting to
# FALSE downstream), so the parser must interpret them tolerantly.
test_that(".parseExcelScenarios interprets true/false spellings in any case", {
  scenarioDf <- data.frame(
    Scenario_name = c("s_true", "s_false"),
    IndividualId = NA_character_,
    PopulationId = NA_character_,
    ReadPopulationFromCSV = c("True", "false"),
    ModelParameterSheets = NA_character_,
    ApplicationProtocol = NA_character_,
    SimulationTime = NA_character_,
    SimulationTimeUnit = NA_character_,
    SteadyState = c("true", "FALSE"),
    SteadyStateTime = NA_real_,
    SteadyStateTimeUnit = NA_character_,
    OverwriteFormulasInSS = c("TRUE", "False"),
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
  # `OutputPathsIds` misspelled as `OutputPathsId`.
  scenarioDf <- scenarioSheetRow(OutputPathsIds = NULL, OutputPathsId = "op1")
  expect_snapshot(error = TRUE, .parseExcelScenarios(scenarioDf))
})

# `OverwriteFormulasInSS` is newer than the 5.x layout, so a pre-6.0 Scenarios
# sheet omits it. Its absence must default to FALSE rather than abort, matching
# the sibling `InitialConditions` column (#1158).
test_that(".parseExcelScenarios defaults OverwriteFormulasInSS when the column is absent", {
  # No `OverwriteFormulasInSS` column, as in a pre-6.0 sheet.
  scenarioDf <- scenarioSheetRow(OverwriteFormulasInSS = NULL)
  scenarios <- .parseExcelScenarios(scenarioDf)
  # The parser drops the absent value to NULL; the JSON serializer defaults it
  # to FALSE (`%||% FALSE`), so a round-trip through the tree reads FALSE.
  expect_null(scenarios[[1]]$overwriteFormulasInSS)
})

# A blank steady-state time/unit defaults to the same values the authoring API
# writes (`1000` / `"min"`), not null, so an imported project is byte-identical
# to the same project re-authored through `addScenario()` (#1158).
test_that(".parseExcelScenarios defaults a blank steady-state time and unit", {
  scenarioDf <- scenarioSheetRow(SteadyState = FALSE)
  scenarios <- .parseExcelScenarios(scenarioDf)
  expect_identical(scenarios[[1]]$steadyStateTime, 1000)
  expect_identical(scenarios[[1]]$steadyStateTimeUnit, "min")
})

# A multi-value cell may protect a comma with either backslash escaping (this
# package's own writer) or double-quote wrapping (the legacy 5.x convention).
# Both must parse, and the quoted form must strip the quotes and keep a quoted
# comma inside a single token rather than splitting on it (#1158).
test_that(".parseCommaListToArray parses the quoted-CSV and backslash conventions", {
  # Quoted-CSV: quotes stripped, the comma inside the quoted run kept.
  expect_identical(
    .parseCommaListToArray('"Global", "Aciclovir", "Sheet, with comma"'),
    c("Global", "Aciclovir", "Sheet, with comma")
  )
  # Backslash convention (the writer's output): `\,` is a literal comma.
  expect_identical(
    .parseCommaListToArray("global, aciclovir, sheet\\, with comma"),
    c("global", "aciclovir", "sheet, with comma")
  )
  # A plain unquoted list is unaffected.
  expect_identical(.parseCommaListToArray("a, b, c"), c("a", "b", "c"))
})

# The same quoting convention reaches a cell holding one value, where the whole
# cell is the value, so only the wrapping pair may go: a comma or a quote inside
# the text is content, not syntax, and an unquoted cell must come back as read.
test_that(".stripWrappingQuotes drops only the wrapping pair", {
  expect_identical(.stripWrappingQuotes('"AciclovirPVB"'), "AciclovirPVB")
  # No wrapping pair, so nothing is a quoting artifact.
  expect_identical(.stripWrappingQuotes("AciclovirPVB"), "AciclovirPVB")
  expect_identical(
    .stripWrappingQuotes('Aciclovir "PVB" plasma'),
    'Aciclovir "PVB" plasma'
  )
  # Wrapped, and quoting an inner run that stays exactly as written.
  expect_identical(
    .stripWrappingQuotes('"Aciclovir "PVB" plasma"'),
    'Aciclovir "PVB" plasma'
  )
  # A single-value cell is one value even when it contains a comma.
  expect_identical(
    .stripWrappingQuotes('"Sheet, with comma"'),
    "Sheet, with comma"
  )
  # A lone quote has no pair to drop.
  expect_identical(.stripWrappingQuotes('"'), '"')
  # A numeric cell is handed back untouched, not stringified.
  expect_identical(.stripWrappingQuotes(1.96), 1.96)
})

# Only the raw `${VAR}` is stored, so it is expanded afresh on every load and a
# relative expansion is resolved against the project file. The import therefore
# has to anchor it the same way, and the folder has to travel with the project,
# or the folder found at import is not the folder found afterwards (#1182).
test_that("a ${VAR} folder expanding to a relative path travels with the project", {
  src <- withr::local_tempdir()
  out <- withr::local_tempdir()
  dir.create(file.path(src, "Inner", "Data"), recursive = TRUE)
  writeLines("x", file.path(src, "Inner", "Data", "values.csv"))
  withr::local_envvar(ESQLABSR_TEST_REL = "Inner/Data")

  result <- .copyExcelProjectAssets(
    list(dataFolder = "${ESQLABSR_TEST_REL}"),
    src,
    out,
    overwrite = TRUE
  )

  # Copied under the expanded name, which is where the loader will look, and
  # reported under the raw one, which is what the project spells.
  expect_identical(result$copied, "${ESQLABSR_TEST_REL}")
  expect_true(file.exists(file.path(out, "Inner", "Data", "values.csv")))
})

# An absolute expansion resolves identically from anywhere, so it is left where
# it is; an unset variable matches nothing and is reported rather than skipped
# without a word.
test_that("a ${VAR} folder is skipped when absolute and reported when unset", {
  src <- withr::local_tempdir()
  out <- withr::local_tempdir()
  elsewhere <- withr::local_tempdir()

  withr::local_envvar(ESQLABSR_TEST_ABS = elsewhere)
  absolute <- .copyExcelProjectAssets(
    list(dataFolder = "${ESQLABSR_TEST_ABS}"),
    src,
    out,
    overwrite = TRUE
  )
  expect_length(absolute$copied, 0L)
  expect_length(absolute$notCopied, 0L)

  unset <- .copyExcelProjectAssets(
    list(dataFolder = "${ESQLABSR_TEST_NEVER_SET}"),
    src,
    out,
    overwrite = TRUE
  )
  expect_identical(unset$notCopied, "${ESQLABSR_TEST_NEVER_SET}")
})

# From a data folder that is the project folder, only the files the project
# configuration names travel, and a name that climbs out of the project is not
# followed: copying it would read outside the source and write outside
# `outputDir` (#1409).
test_that("a data folder that is the project folder copies only its named files inside the project", {
  base <- withr::local_tempdir()
  src <- file.path(base, "Project")
  dir.create(src)
  writeLines("observed values", file.path(src, "Values.xlsx"))
  writeLines("unrelated", file.path(src, "Other.xlsx"))
  writeLines("<importer/>", file.path(base, "Importer.xml"))
  out <- file.path(withr::local_tempdir(), "Output")
  dir.create(out)

  result <- .copyExcelProjectAssets(
    list(
      dataFolder = ".",
      dataFile = "Values.xlsx",
      dataImporterConfigurationFile = "../Importer.xml"
    ),
    src,
    out,
    overwrite = TRUE
  )

  expect_identical(
    result,
    list(
      copied = character(),
      notCopied = character(),
      copiedFiles = "Values.xlsx",
      notCopiedFiles = character()
    )
  )
  expect_identical(list.files(out), "Values.xlsx")
  expect_false(file.exists(file.path(dirname(out), "Importer.xml")))
})

# A data file inside a folder the same import copies whole has already
# traveled with it, so it is neither copied again nor reported as one the
# output already held.
test_that("a data file inside a copied folder is not reported as kept", {
  src <- withr::local_tempdir()
  dir.create(file.path(src, "Models"))
  writeLines("observed values", file.path(src, "Models", "Values.xlsx"))
  out <- withr::local_tempdir()

  result <- .copyExcelProjectAssets(
    list(
      simulationsFolder = "Models",
      dataFolder = ".",
      dataFile = "Models/Values.xlsx"
    ),
    src,
    out,
    overwrite = FALSE
  )

  expect_identical(
    result,
    list(
      copied = "Models",
      notCopied = character(),
      copiedFiles = character(),
      notCopiedFiles = character()
    )
  )
  expect_true(file.exists(file.path(out, "Models", "Values.xlsx")))
})

# An `outputDir` given relative to the working directory names the same folders
# as its absolute spelling, so the check for a file inside a copied folder must
# not take every observed-data file for one and leave it behind (#1409).
test_that("importProjectFromExcel copies the observed-data files into a relative outputDir", {
  projectDir <- localExcelProjectWithRootData()
  workDir <- withr::local_tempdir()
  withr::local_dir(workDir)

  suppressWarnings(importProjectFromExcel(
    file.path(projectDir, "ProjectConfiguration.xlsx"),
    outputDir = "out",
    silent = TRUE
  ))

  expect_true(all(file.exists(file.path(
    workDir,
    "out",
    c(
      "TestProject_TimeValuesData.xlsx",
      "esqlabs_dataImporter_configuration.xml"
    )
  ))))
  # The models folder traveled too, so the data files were compared against a
  # folder this import copied.
  expect_true(dir.exists(file.path(workDir, "out", "Models", "Simulations")))
  # The fixture's workbooks carry an unresolved PI reference, which the load
  # warns about; it has nothing to do with the observed-data source.
  project <- suppressWarnings(loadProject(file.path(workDir, "out")))
  expect_gt(length(loadObservedData(project)), 0L)
})

# The observed-data parser anchors a relative expansion at the project file it
# is writing, not at the Excel source it is reading, so import and load agree
# on which folder the stored `${VAR}` names (#1182).
test_that(".parseExcelObservedData anchors a relative ${VAR} at the project file", {
  source <- withr::local_tempdir()
  project <- withr::local_tempdir()
  dir.create(file.path(project, "Shared"))
  .writeExcel(
    list(Sheet1 = data.frame(x = 1)),
    file.path(project, "Shared", "Values.xlsx")
  )
  withr::local_envvar(ESQLABSR_TEST_REL = "Shared")

  prop <- function(name) {
    switch(
      name,
      dataFile = "Values.xlsx",
      dataFolder = "${ESQLABSR_TEST_REL}",
      NULL
    )
  }
  # Found under the project directory, though the Excel source is elsewhere.
  expect_silent(
    result <- .parseExcelObservedData(list(), prop, source, project)
  )
  expect_named(result$observedData, "values")
})

# The reported case: a populations sheet whose real rows are followed by rows
# that hold nothing. Each was taken for a population definition and the import
# aborted on the first of them for having no id (#1191).
test_that("blank rows in a populations sheet do not become definitions", {
  projectDir <- localExcelProjectDir()
  popFile <- file.path(projectDir, "Configurations", "Populations.xlsx")

  sheetNames <- readxl::excel_sheets(popFile)
  sheets <- stats::setNames(
    lapply(sheetNames, function(s) readExcel(popFile, sheet = s)),
    sheetNames
  )
  populations <- sheets[[1]]
  realRows <- nrow(populations)
  # Blank rows between the real ones, so the sheet reports them rather than
  # trimming them as it would trailing blanks written by `writexl`.
  blank <- populations[rep(NA_integer_, 3), ]
  sheets[[1]] <- rbind(populations[1, ], blank, populations[-1, ])
  .writeExcel(sheets, popFile)

  jsonPath <- suppressWarnings(importProjectFromExcel(
    file.path(projectDir, "ProjectConfiguration.xlsx"),
    outputDir = withr::local_tempdir(),
    silent = TRUE
  ))
  project <- suppressWarnings(loadProject(jsonPath))

  expect_length(project$definitions$populations, realRows)
})

# The reported case: a fit-bounds sheet authored by copying a real parameter
# sheet carries all four parameter columns, so it is a parameter sheet and its
# rows are read, and a `Value` of "lower" aborted the whole import (#1189).
test_that("a non-numeric Value skips the row, not the import", {
  projectDir <- localExcelProjectDir()
  paramFile <- file.path(projectDir, "Configurations", "ModelParameters.xlsx")

  sheetNames <- readxl::excel_sheets(paramFile)
  sheets <- stats::setNames(
    lapply(sheetNames, function(s) readExcel(paramFile, sheet = s)),
    sheetNames
  )
  sheets[["RefConc_fit"]] <- data.frame(
    `Container Path` = c("Target", "Target", "Target"),
    `Parameter Name` = c("Reference concentration", "Kd", "koff"),
    Value = c("lower", "2.5", "upper"),
    Units = c("nmol/l", "nmol/l", "1/min"),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  .writeExcel(sheets, paramFile)

  expect_warning(
    jsonPath <- importProjectFromExcel(
      file.path(projectDir, "ProjectConfiguration.xlsx"),
      outputDir = withr::local_tempdir(),
      silent = TRUE
    ),
    class = "esqlabsR_importSkippedNonNumericRows"
  )
  project <- suppressWarnings(loadProject(jsonPath))

  # The two unparseable rows are gone; the numeric one survives with its value.
  set <- .unwrapDefinitionList(project$definitions$parameterSets)[[
    "refconc_fit"
  ]]
  expect_length(set, 1L)
  expect_identical(set[[1]]$parameterName, "Kd")
  expect_identical(set[[1]]$value, 2.5)
})

# The reported case: a fresh, never-edited import came back invalid with one
# critical error per observed curve, and nothing in the import said why. The
# curve is kept (the user may still be able to fill the cell), so the import
# names the affected data combinations and states the consequence (#1183).
test_that("an observed curve with no dataSet is kept and reported", {
  projectDir <- localExcelProjectDir()
  plotsFile <- file.path(projectDir, "Configurations", "Plots.xlsx")

  sheetNames <- readxl::excel_sheets(plotsFile)
  sheets <- stats::setNames(
    lapply(sheetNames, function(s) readExcel(plotsFile, sheet = s)),
    sheetNames
  )
  observed <- sheets$DataCombined$dataType == "observed"
  sheets$DataCombined$dataSet[observed] <- NA
  .writeExcel(sheets, plotsFile)

  expect_warning(
    jsonPath <- importProjectFromExcel(
      file.path(projectDir, "ProjectConfiguration.xlsx"),
      outputDir = withr::local_tempdir(),
      silent = TRUE
    ),
    class = "esqlabsR_importIncompleteObservedCurves"
  )
  project <- suppressWarnings(loadProject(jsonPath))

  # Nothing dropped: the observed curve is still there, and so is the simulated
  # one beside it.
  dc <- .unwrapDefinitionList(project$definitions$dataCombined)[[
    "aciclovirpvb"
  ]]
  expect_length(dc$observed, 1L)
  expect_length(dc$simulated, 1L)
  expect_null(dc$observed[[1]]$dataSet)

  # And the consequence the warning states is the one that actually happens.
  results <- suppressWarnings(validateProject(project))
  errors <- unlist(lapply(results, function(section) {
    vapply(section$critical_errors %||% list(), \(e) e$message, character(1))
  }))
  expect_true(any(grepl(
    "observed entry missing required field: dataSet",
    errors
  )))
})

test_that("a missing data file says the project will not validate", {
  projectDir <- localExcelProjectDir()
  unlink(list.files(file.path(projectDir, "Data"), full.names = TRUE))

  expect_warning(
    importProjectFromExcel(
      file.path(projectDir, "ProjectConfiguration.xlsx"),
      outputDir = withr::local_tempdir(),
      silent = TRUE
    ),
    "validateProject"
  )
})

# The warning's wording is the whole of the fix for #1183, so it is snapshotted
# rather than only matched on its class. Called directly, not through an import,
# so no temp path lands in the snapshot.
test_that(".warnIncompleteObservedCurves names the affected combinations", {
  expect_snapshot(.warnIncompleteObservedCurves(list(
    list(
      id = "plasma",
      simulated = list(list(label = "sim")),
      observed = list(list(label = "obs"))
    ),
    list(
      id = "urine",
      observed = list(list(label = "obs", dataSet = "d1"))
    ),
    list(
      id = "fat",
      observed = list(list(label = "obs", dataSet = ""))
    )
  )))

  # Nothing to say when every observed curve names a data set.
  expect_silent(.warnIncompleteObservedCurves(list(
    list(id = "urine", observed = list(list(dataSet = "d1")))
  )))
  expect_silent(.warnIncompleteObservedCurves(NULL))
})

# Blank rows are dropped on read, so the parsed-frame index is not the row Excel
# shows. Findability is the whole purpose of the reported number, so it is the
# workbook row: blank rows above the offender counted, header included.
test_that("a skipped row is reported at its workbook row", {
  paramFile <- withr::local_tempfile(fileext = ".xlsx")
  df <- data.frame(
    `Container Path` = c("Organism|A", NA, NA, "Organism|A"),
    `Parameter Name` = c("P", NA, NA, "Q"),
    Value = c("1", NA, NA, "lower"),
    Units = c("mg", NA, NA, "mg"),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  .writeExcel(list(Global = df), paramFile)

  # The offending cell is on sheet row 5: header, one data row, two blank rows.
  expect_warning(.parseExcelParameterSheets(paramFile), "row 5")
})

# A sheet that had rows but kept none describes no parameter, so it becomes no
# definition. A header-only sheet is a real (empty) set and is unaffected; the
# test above covers that case.
test_that("a sheet whose every row is skipped becomes no parameter set", {
  paramFile <- withr::local_tempfile(fileext = ".xlsx")
  .writeExcel(
    list(
      Global = data.frame(
        `Container Path` = "Organism|A",
        `Parameter Name` = "P",
        Value = "1",
        Units = "mg",
        check.names = FALSE
      ),
      RefConc_fit = data.frame(
        `Container Path` = "Target",
        `Parameter Name` = c("Reference concentration", "Kd"),
        Value = c("lower", "upper"),
        Units = "nmol/l",
        check.names = FALSE
      )
    ),
    paramFile
  )

  expect_warning(
    parsed <- .parseExcelParameterSheets(paramFile),
    class = "esqlabsR_importSkippedNonNumericRows"
  )
  expect_named(parsed, "Global")
})

# Legacy Excel fixture: characterization of the #1213 migration findings ----
#
# These tests pin what the importer does *today* with the workbook shapes real
# pre-5.6 projects carry, using the `TestProjectExcelLegacy/` fixture. Several of
# the behaviours pinned here are the defects reported in #1213: each such test
# passes against the current behaviour and names the finding it covers, so it
# flips visibly when that finding is fixed rather than quietly keeping a green
# suite over a silent data loss.
#
# The sibling `TestProjectExcel/` fixture uses the modern spelling of every one
# of these shapes, which is why none of them has been reproducible until now.

# #1213 item 17: the imported observed-data declaration states its own `id`, the
# canonicalized basename of the data file without its extension. Deriving one
# instead named the definition file after the basename verbatim, so it came out
# with a double extension and kept whatever spaces, commas and casing the data
# file's name carried, and the declaration was addressable only by an id the
# authoring API could not have produced.
test_that("the imported observed-data declaration carries a canonicalized id", {
  imported <- importLegacyExcelProject(localLegacyExcelProject())
  entry <- imported$project$definitions$observedData[[1]]

  expect_identical(entry$id, "testproject_timevaluesdata")
  expect_identical(
    list.files(file.path(imported$outputDir, "definitions", "observed-data")),
    "testproject_timevaluesdata.json"
  )
  # And the id is the handle `removeObservedData()` matches on.
  expect_identical(
    .observedDataSectionIds(imported$project$definitions$observedData),
    "testproject_timevaluesdata"
  )
})

test_that("a data file whose name is not filename-safe still yields a canonical id", {
  projectDir <- localLegacyExcelProject()
  file.rename(
    file.path(projectDir, "Data", "TestProject_TimeValuesData.xlsx"),
    file.path(projectDir, "Data", "My Data, v2.xlsx")
  )
  editWorkbookSheets(
    legacyExcelProjectPath(projectDir),
    function(sheets) {
      row <- sheets$Sheet1$Property == "dataFile"
      sheets$Sheet1$Value[row] <- "My Data, v2.xlsx"
      sheets
    }
  )
  imported <- importLegacyExcelProject(projectDir)

  expect_identical(
    imported$project$definitions$observedData[[1]]$id,
    "my_data__v2"
  )
  expect_identical(
    list.files(file.path(imported$outputDir, "definitions", "observed-data")),
    "my_data__v2.json"
  )
  # The stored `file` is untouched: it is a path the loader resolves under the
  # data folder, not an id.
  expect_identical(
    imported$project$definitions$observedData[[1]]$file,
    "My Data, v2.xlsx"
  )
})

# #1213 item 13: the `crossReferences` phase resolves a mapping's `observedData`
# alongside its `outputPathId`. A mapping naming a data set no observed-data source
# holds used to be reported as no error at all, and `runPI()` then aborted at build
# time on a project `validateProject()` had called clean.
test_that("a parameter-identification observedData reference resolves against the loaded data-set names", {
  imported <- importLegacyExcelProject(localLegacyExcelProject())
  task <- imported$project$definitions$parameterIdentification[["aciclovirfit"]]

  # The fixture's mapping names a data set the workbook's data file really holds,
  # so the reference resolves and the project is clean.
  expect_true(
    task$outputMappings[[1]]$observedDataId %in%
      getObservedDataNames(imported$project)
  )
  summary <- validationSummary(suppressWarnings(validateProject(
    imported$project
  )))
  expect_equal(summary$total_critical_errors, 0)
})

test_that("a dangling parameter-identification observedData reference is reported", {
  projectDir <- localLegacyExcelProject()
  editWorkbookSheets(
    file.path(projectDir, "Configurations", "ParameterIdentification.xlsx"),
    function(sheets) {
      sheets$PIOutputMappings$DataSet <- "Laskin 1982.Group B_Aciclovir"
      sheets
    }
  )
  imported <- importLegacyExcelProject(projectDir)

  results <- suppressWarnings(validateProject(imported$project))
  messages <- vapply(
    results$crossReferences$critical_errors,
    function(e) e$message,
    ""
  )
  expect_length(messages, 2L)
  expect_true(all(grepl("undefined observed data", messages)))

  # And `runPI()` is gated on that phase, so the mapping never reaches the build
  # that used to be the first thing to notice.
  expect_error(
    runPI(imported$project),
    "critical validation error"
  )
})

# #1213 item 19: a blank `SimulationTimeUnit` cell imports as the same `"h"`
# `addScenario()` defaults the absent value to, so the blank cell means one unit
# whichever entrypoint wrote the project.
#
# The scenarios sheet also has no `OverwriteFormulasInSS` column at all, and the
# absent column imports as `FALSE` rather than as an absent field.
test_that("a blank simulationTime unit imports as the authoring default, and an absent OverwriteFormulasInSS column as FALSE", {
  imported <- importLegacyExcelProject(localLegacyExcelProject())
  scenario <- imported$project$definitions$scenarios[["adultscenario"]]

  # The workbook leaves the cell blank, and authoring the same scenario gives it
  # the same unit.
  expect_identical(scenario$simulationTimeUnit, "h")
  expect_identical(formals(addScenario)$simulationTimeUnit, "h")

  # A scenario whose cell IS filled keeps its unit, so the above is the blank
  # cell and not the whole column being dropped.
  expect_identical(
    imported$project$definitions$scenarios[[
      "childscenario"
    ]]$simulationTimeUnit,
    "h"
  )

  expect_false(scenario$overwriteFormulasInSS)
})

# #1213 item 18: the plots section has the same field-type contract
# `dataCombined` has, so a workbook storing `nsd` as text yields the number the
# same field authored with `addPlot()` holds. A multi-value cell stays the
# comma-separated string both entrypoints keep it in.
test_that("a plots field stored as text imports as a number", {
  imported <- importLegacyExcelProject(localLegacyExcelProject())
  plots <- imported$project$definitions$plots

  expect_identical(plots[["p3"]]$nsd, 1.96)
  expect_identical(plots[["p3"]]$xValuesLimits, "0, 24")
  expect_identical(plots[["p2"]]$foldDistance, "2, 3")
})

# #1213 item 18, what the field types were costing: a freshly exported workbook
# read back had to describe the same project, and it did not, so `projectStatus()`
# reported every imported project as out of sync with the Excel side-car it had
# just written.
test_that("a freshly exported workbook reports an imported project as in sync", {
  projectDir <- localLegacyExcelProject()
  imported <- importLegacyExcelProject(projectDir)
  suppressMessages(exportProjectToExcel(
    imported$project,
    outputDir = imported$outputDir,
    overwrite = TRUE,
    silent = TRUE
  ))

  status <- suppressWarnings(suppressMessages(projectStatus(imported$project)))
  expect_true(status$excel_in_sync)
})

# #1213 item 10 / the residue #1207 recorded: a 5.x multi-value cell is quoted,
# and a value may itself contain a comma, which is exactly what the quoting is
# for. A single-value cell in the same workbook is quoted the same way and means
# the same name without the quotes, so both sides of a `DataCombinedName`
# reference read the name the modeller wrote rather than `_name_`.
test_that("quoted legacy cells resolve to the names they were written with", {
  imported <- importLegacyExcelProject(localLegacyExcelProject())

  # The quoted multi-value cell splits on the separating commas only, so the
  # sheet name containing a comma survives as one reference.
  expect_identical(
    imported$project$definitions$scenarios[[
      "adultscenario"
    ]]$parameterSets,
    c("global", "aciclovir", "sheet__with_comma")
  )

  # The quoted single-value `DataCombinedName` is unquoted on the defining side,
  # so the id matches the one `addDataCombined(project, "AciclovirPVB", ...)`
  # would have produced.
  expect_setequal(
    names(imported$project$definitions$dataCombined),
    c("aciclovirpvb", "aciclovirpop")
  )
  # And on the referencing side, so the plot still points at it.
  expect_identical(
    imported$project$definitions$plots[["p1"]]$dataCombined,
    "aciclovirpvb"
  )

  # Consistently quoted, so the project validates: only inconsistent quoting
  # dangles (#1207).
  summary <- validationSummary(suppressWarnings(validateProject(
    imported$project
  )))
  expect_equal(summary$total_critical_errors, 0)
})

# A quoted and an unquoted spelling of one name are the same name, so a sheet
# that groups its rows by that name has to group on the unquoted spelling. The
# `DataCombined` sheet is long-format (one row per curve), so grouping on the raw
# cell puts an inconsistently spelled combination's curves in two records, which
# then land on one definition key and the first one's curves are dropped.
test_that("a DataCombined name spelled quoted and unquoted is one combination", {
  projectDir <- localLegacyExcelProject()
  editWorkbookSheets(
    file.path(projectDir, "Configurations", "Plots.xlsx"),
    function(sheets) {
      # Row 1 (simulated) keeps the fixture's `"AciclovirPVB"`; row 2 (observed)
      # spells the same name without the quotes.
      sheets$DataCombined$DataCombinedName[[2]] <- "AciclovirPVB"
      sheets
    }
  )
  imported <- importLegacyExcelProject(projectDir)
  combined <- imported$project$definitions$dataCombined

  expect_setequal(names(combined), c("aciclovirpvb", "aciclovirpop"))

  # Both curves are on the one combination: neither row's is dropped.
  expect_length(combined[["aciclovirpvb"]]$simulated, 1L)
  expect_length(combined[["aciclovirpvb"]]$observed, 1L)
})

# A plots sheet keys one definition per row rather than grouping, so two rows
# naming one plot are a reused id, whichever way each spells it. That is the
# reported loss `.warnDuplicatePlotIds()` already covers for two rows spelling the
# id identically; a quoted spelling has to reach the same report rather than
# passing as a second plot and then quietly overwriting the first.
test_that("a plot id spelled quoted and unquoted is one reused id, and reported", {
  projectDir <- localLegacyExcelProject()
  editWorkbookSheets(
    file.path(projectDir, "Configurations", "Plots.xlsx"),
    function(sheets) {
      sheets$plotConfiguration$plotID[[2]] <- "\"P1\""
      sheets
    }
  )
  imported <- importLegacyExcelProject(projectDir)

  expect_setequal(names(imported$project$definitions$plots), c("p1", "p3"))
  expect_true(any(grepl("more than", imported$warnings, fixed = TRUE)))
  expect_true(any(grepl("plotConfiguration", imported$warnings, fixed = TRUE)))
})

# A section stored as an array of records is keyed by each record's own id field
# just as a keyed section is keyed by its names, so two *distinct* names that
# canonicalize onto one id are the same ambiguity `canonNames()` aborts on, and
# not something to resolve by keeping whichever record came last. `DataCombined`
# has no duplicate-id report of its own, so without the abort the collision costs
# a curve in silence.
test_that("two DataCombined names that canonicalize alike abort the import", {
  projectDir <- localLegacyExcelProject()
  editWorkbookSheets(
    file.path(projectDir, "Configurations", "Plots.xlsx"),
    function(sheets) {
      # A space and a comma both canonicalize to `_`, so these are two names for
      # two combinations that would be filed as one.
      sheets$DataCombined$DataCombinedName[[1]] <- "Aciclovir PVB"
      sheets$DataCombined$DataCombinedName[[2]] <- "Aciclovir,PVB"
      sheets
    }
  )
  expect_error(
    suppressWarnings(importProjectFromExcel(
      legacyExcelProjectPath(projectDir),
      outputDir = withr::local_tempdir("LegacyOut_"),
      silent = TRUE
    )),
    "collide after canonicalization"
  )
})

# #1213 item 5: an Excel project spells `populationsFolder` as a folder name under
# the configurations folder, while a project resolves its working folders against
# its own root. The import rewrites the value to the project-root-relative path of
# the folder that actually holds the csv files, so the folder travels with the
# project and the value means the same thing on both sides.
test_that("the populations CSV folder travels with an imported project", {
  projectDir <- localLegacyExcelProject()
  imported <- importLegacyExcelProject(projectDir)

  # It is there in the Excel project, under the configurations folder.
  expect_true(dir.exists(
    file.path(projectDir, "Configurations", "PopulationsCSV")
  ))

  # The sibling asset folders travelled.
  expect_true(dir.exists(file.path(
    imported$outputDir,
    "Models",
    "Simulations"
  )))
  expect_true(dir.exists(file.path(imported$outputDir, "Data")))

  # So did the populations folder, keeping the layout it was authored in.
  expect_true(dir.exists(
    file.path(imported$outputDir, "Configurations", "PopulationsCSV")
  ))
  expect_false(dir.exists(file.path(imported$outputDir, "PopulationsCSV")))

  # And the folder the project resolves is the one the csv file was copied into.
  expect_true(file.exists(file.path(
    imported$project$paths$populationsFolder,
    "CsvPop.csv"
  )))

  # Nothing was left behind, so nothing is reported as uncopied.
  expect_false(any(grepl("not copied", imported$warnings, fixed = TRUE)))
})

# The tests below each derive a variant of the legacy fixture by mutating exactly
# one workbook of a throwaway copy, so a variant differs from the base in one
# dimension and cannot interfere with another test's subject.

# #1213 item 8: workbook resolution is property-driven, so a workbook the property
# sheet names but that is not on disk leaves its section empty. That used to happen
# in complete silence, with the one cue suppressed as well, because the summary's
# count block lists only the sections that hold something. The import now names the
# workbook that is not there, and the summary names the sections that came out
# empty.
test_that("a named but absent workbook is reported, and its empty section named", {
  projectDir <- localLegacyExcelProject()
  file.remove(file.path(projectDir, "Configurations", "Populations.xlsx"))

  messages <- character()
  imported <- withCallingHandlers(
    importLegacyExcelProject(projectDir, silent = FALSE),
    message = function(m) {
      messages <<- c(messages, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )

  # Both populations are gone, and the workbook that is missing is named.
  expect_length(imported$project$definitions$populations, 0L)
  expect_true(any(grepl("Populations.xlsx", imported$warnings, fixed = TRUE)))

  # The count block still lists only what imported something, so the summary names
  # the empty sections separately. Matched on the section line, since the folder
  # the import copies is named after the section too.
  summary <- paste(messages, collapse = "")
  expect_match(summary, "* Individuals:", fixed = TRUE)
  expect_no_match(summary, "* Populations:", fixed = TRUE)
  expect_match(summary, "imported no definitions", fixed = TRUE)
  expect_match(summary, "populations", fixed = TRUE)

  # And the downstream trace is unchanged: the two scenarios that name a
  # population dangle at load.
  expect_true(any(grepl("undefined population", imported$warnings)))
})

# #1213 item 8, the mirror failures of the same property-driven resolution: a
# workbook nothing names is never opened however much it holds, and one read under
# its conventional name is content the project does not declare. Both are now
# reported.
test_that("an unread workbook is reported, and one read by convention is named", {
  projectDir <- localLegacyExcelProject()
  configDir <- file.path(projectDir, "Configurations")
  # A second, nested set of workbooks the property sheet does not mention, the
  # shape three of the tested projects had.
  dir.create(file.path(configDir, "v2"))
  file.copy(
    file.path(configDir, "Scenarios.xlsx"),
    file.path(configDir, "v2", "Scenarios.xlsx")
  )
  # And an initial-conditions workbook, whose property row the sheet never
  # carried, sitting under the conventional filename.
  writexl::write_xlsx(
    list(
      Global = data.frame(
        `Container Path` = "Organism|Liver",
        `Molecule Name` = "Aciclovir",
        `Is Present` = TRUE,
        Value = 1,
        Units = "\u00b5mol/l",
        `Scale Divisor` = 1,
        `Neg. Values Allowed` = FALSE,
        check.names = FALSE
      )
    ),
    file.path(configDir, "InitialConditions.xlsx")
  )

  messages <- character()
  imported <- withCallingHandlers(
    importLegacyExcelProject(projectDir, silent = FALSE),
    message = function(m) {
      messages <<- c(messages, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )

  # The nested copy contributed nothing and is named.
  expect_length(imported$project$definitions$scenarios, 6L)
  expect_true(any(grepl("not read", imported$warnings, fixed = TRUE)))
  expect_true(any(grepl("Scenarios.xlsx", imported$warnings, fixed = TRUE)))

  # The conventional workbook was imported, and the summary says the project does
  # not name it.
  expect_length(imported$project$definitions$initialConditions, 1L)
  summary <- paste(messages, collapse = "")
  expect_match(summary, "InitialConditions.xlsx", fixed = TRUE)
  expect_match(summary, "conventional name", fixed = TRUE)
})

# #1213 item 10: each plot is one definition keyed by its id, so a second row
# carrying an id an earlier row already used builds the same definition and the
# earlier row's plot is gone. The reported count is the count of surviving plots,
# which is why the loss used to be invisible unless the reader counted the
# workbook themselves; the import now names the reused id.
test_that("two plot rows sharing an id lose one plot, and it is reported", {
  projectDir <- localLegacyExcelProject()
  editWorkbookSheets(
    file.path(projectDir, "Configurations", "Plots.xlsx"),
    function(sheets) {
      # Row 2 (`observedVsSimulated`) takes row 1's id.
      sheets$plotConfiguration$plotID[[2]] <- "P1"
      sheets
    }
  )
  imported <- importLegacyExcelProject(projectDir)

  # Three workbook rows, two plots.
  expect_length(imported$project$definitions$plots, 2L)
  expect_setequal(names(imported$project$definitions$plots), c("p1", "p3"))

  # The later row won: `p1` carries row 2's fields, not row 1's.
  expect_identical(
    imported$project$definitions$plots[["p1"]]$plotType,
    "observedVsSimulated"
  )

  # And the loss is reported, naming the id more than one row used.
  expect_true(any(grepl("more than", imported$warnings, fixed = TRUE)))
  expect_true(any(grepl("plotConfiguration", imported$warnings, fixed = TRUE)))
})

# #1213 item 16: `.canonicalizeId()` folds Unicode compatibility variants and drops
# the invisible formatting characters, so no invisible reaches a definition
# filename from either entrypoint. A no-break space (U+00A0) folds onto the plain
# space it renders as and becomes the same `_`; a zero-width space (U+200B) is
# dropped, so an id carrying one is the id without it.
#
# This is live data rather than a synthetic probe: one tested project carried 12
# real ids containing U+00A0.
test_that("an id containing a no-break space canonicalizes into a plain filename", {
  nbsp <- "\u00a0"
  projectDir <- localLegacyExcelProject()
  editWorkbookSheets(
    file.path(projectDir, "Configurations", "Scenarios.xlsx"),
    function(sheets) {
      sheets$OutputPaths <- rbind(
        sheets$OutputPaths,
        data.frame(
          OutputPathId = paste0("Renal", nbsp, "Clearance"),
          OutputPath = "Organism|Kidney|Aciclovir|Concentration in container"
        )
      )
      # Reference it with the same invisible the definition carries, so what is
      # under test is only whether both sides land on one id.
      sheets$Scenarios$OutputPathsIds[[2]] <- paste0(
        "Aciclovir_PVB, Aciclovir_Fat, Renal",
        nbsp,
        "Clearance"
      )
      sheets
    }
  )
  imported <- importLegacyExcelProject(projectDir)
  ids <- names(imported$project$definitions$outputPaths)

  # The no-break space became the `_` an ordinary space becomes.
  expect_true("renal_clearance" %in% ids)

  # So the definition filenames carry no invisible at all.
  files <- list.files(file.path(
    imported$outputDir,
    "definitions",
    "output-paths"
  ))
  expect_true("renal_clearance.json" %in% files)
  expect_false(any(grepl("[^ -~]", files)))

  # And the reference resolves onto it, so validation is clean.
  summary <- validationSummary(suppressWarnings(validateProject(
    imported$project
  )))
  expect_equal(summary$total_critical_errors, 0)
})

# The other half of item 16: two ids differing only by a zero-width space used to
# become two definition files whose names render identically, with nothing said.
# Dropping the invisible makes them one id, so the import aborts on the collision
# the same way interactive authoring does, naming both spellings.
test_that("two ids differing only by a zero-width space abort the import as a collision", {
  zwsp <- "\u200b"
  projectDir <- localLegacyExcelProject()
  editWorkbookSheets(
    file.path(projectDir, "Configurations", "Scenarios.xlsx"),
    function(sheets) {
      sheets$OutputPaths$OutputPathId <- c(
        "OutPath",
        paste0("Out", zwsp, "Path")
      )
      sheets
    }
  )
  expect_error(
    suppressWarnings(importProjectFromExcel(
      legacyExcelProjectPath(projectDir),
      outputDir = withr::local_tempdir("LegacyOut_"),
      silent = TRUE
    )),
    "collide after canonicalization"
  )
})

test_that("Excel round-trip preserves a simulated entry that omits its label", {
  project <- exampleProject()

  addDataCombined(
    project,
    id = "dc_no_label",
    simulated = list(list(
      scenario = "aciclovir_iv",
      path = "Aciclovir_PVB"
    ))
  )

  reimported <- excelRoundTrip(project)
  dc <- .unwrapDefinitionList(reimported$definitions$dataCombined)[[
    "dc_no_label"
  ]]

  # The empty label cell comes back as an omission (no `label` member at all),
  # so the curve keeps its default name across the round trip, and the rest of
  # the entry survives unchanged.
  expect_identical(
    dc$simulated[[1]],
    list(scenario = "aciclovir_iv", path = "Aciclovir_PVB")
  )
})

# Scenario label ----

test_that("a scenario label survives an Excel export and import", {
  project <- exampleProject()
  setScenario(project, "aciclovir_iv", label = "Aciclovir IV 250 mg")

  reimported <- excelRoundTrip(project)

  expect_identical(
    reimported$definitions$scenarios[["aciclovir_iv"]]$label,
    "Aciclovir IV 250 mg"
  )
  expect_null(
    reimported$definitions$scenarios[["aciclovir_iv_population"]]$label
  )
})

test_that("exportProjectToExcel writes the Label column only when a scenario has a label", {
  project <- exampleProject()
  columns <- function(project) {
    names(.scenarioConfigurationsToExcelDf(project$definitions$scenarios))
  }

  expect_false("Label" %in% columns(project))

  setScenario(project, "aciclovir_iv", label = "Aciclovir IV 250 mg")
  expect_identical(columns(project)[1:2], c("Scenario_name", "Label"))
})

test_that(".parseExcelScenarios reads the optional Label column", {
  withLabel <- .parseExcelScenarios(
    scenarioSheetRow(Label = "Aciclovir IV 250 mg")
  )
  blankLabel <- .parseExcelScenarios(scenarioSheetRow(Label = NA_character_))
  noColumn <- .parseExcelScenarios(scenarioSheetRow())

  expect_identical(withLabel[[1]]$label, "Aciclovir IV 250 mg")
  expect_null(blankLabel[[1]]$label)
  expect_null(noColumn[[1]]$label)
})

# DataCombined output-path ids ----

test_that(".dataCombinedPathsAsOutputPathIds writes the id only for a path one output path has", {
  plasma <- "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)"
  fat <- "Organism|Fat|Intracellular|Aciclovir|Concentration in container"
  urine <- "Organism|Kidney|Urine|Aciclovir|Concentration"
  curve <- function(path) {
    list(label = path, scenario = "aciclovir_iv", path = path)
  }
  jsonData <- list(
    outputPaths = list(
      aciclovir_pvb = plasma,
      aciclovir_fat = fat,
      fat_tissue = fat
    ),
    dataCombined = list(list(
      id = "aciclovir",
      simulated = list(curve(plasma), curve(urine), curve(fat)),
      observed = list(list(label = "Laskin 1982", dataSet = "laskin"))
    ))
  )

  expected <- jsonData
  # One output path has the plasma path: its id is written.
  expected$dataCombined[[1]]$simulated[[1]]$path <- "aciclovir_pvb"
  # No output path has the urine path, and two have the fat path: both stay.
  expect_identical(.dataCombinedPathsAsOutputPathIds(jsonData), expected)
})

test_that("a DataCombined output-path id survives an Excel export and import", {
  project <- exampleProject()

  reimported <- excelRoundTrip(project)

  expect_identical(
    reimported$definitions$dataCombined[["aciclovir_individual"]]$simulated[[
      1
    ]]$path,
    "aciclovir_pvb"
  )
})
