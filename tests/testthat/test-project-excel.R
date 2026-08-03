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

test_that("Excel round-trip defaults a non-steady-state scenario's steady-state time and unit", {
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

  # A non-steady-state scenario has no meaningful steady-state time, so the
  # import defaults it to the same `1000` / `"min"` the authoring API writes,
  # rather than null. This keeps an imported project byte-identical to the same
  # project re-authored through `addScenario()`, so no round-trip diff (#1158).
  # The value is only used when steady-state is on.
  nonSteady <- "aciclovir_iv"
  expect_false(isTRUE(before[[nonSteady]]$simulateSteadyState))
  expect_identical(after[[nonSteady]]$steadyStateTime, 1000)
  expect_identical(after[[nonSteady]]$steadyStateTimeUnit, "min")

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

test_that("Excel round-trip preserves a grid whose plot id had a comma canonicalized out", {
  work_dir <- withr::local_tempdir()
  file.copy(dirname(exampleProjectPath()), work_dir, recursive = TRUE)
  project <- loadProject(file.path(work_dir, "Example", "Project.json"))

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

  expect_identical(.splitPlotIDs(grid$plotIds), c("p1", "cmax__ss"))
})

# Regression (#1184): a grid's `plotIDs` cell is a multi-value Excel cell, and
# the 5.x convention wraps each value in `""` so it may itself contain a comma.
# Decoding that cell with the in-memory codec kept the quotes, which
# canonicalization then turned into underscores (`"P1"` -> `_p1_`), so every
# member of a quoted grid dangled and the freshly imported project failed its
# own validateProject().
test_that("a quoted 5.x plotIDs cell imports with the grid's members intact", {
  work_dir <- withr::local_tempdir()
  file.copy(dirname(testProjectExcelPath()), work_dir, recursive = TRUE)
  projectDir <- file.path(work_dir, "TestProjectExcel")
  plotsFile <- file.path(projectDir, "Configurations", "Plots.xlsx")

  sheetNames <- readxl::excel_sheets(plotsFile)
  sheets <- stats::setNames(
    lapply(sheetNames, function(s) readExcel(plotsFile, sheet = s)),
    sheetNames
  )
  # Rewrite `P1, P2, P3` as `"P1", "P2", "P3"`, the quoted form v5 accepted.
  sheets$plotGrids$plotIDs <- vapply(
    strsplit(sheets$plotGrids$plotIDs, ",[[:space:]]*"),
    function(ids) paste0("\"", ids, "\"", collapse = ", "),
    character(1)
  )
  .writeExcel(sheets, plotsFile)

  jsonPath <- suppressWarnings(importProjectFromExcel(
    file.path(projectDir, "ProjectConfiguration.xlsx"),
    outputDir = withr::local_tempdir(),
    silent = TRUE
  ))
  project <- suppressWarnings(loadProject(jsonPath))

  grids <- .unwrapDefinitionList(project$definitions$plotGrids)
  expect_identical(
    .splitPlotIDs(grids[["aciclovir"]]$plotIds),
    c("p1", "p2", "p3")
  )
  expect_identical(.splitPlotIDs(grids[["aciclovir2"]]$plotIds), "p2")

  # The whole point: the imported project no longer reports the grid members as
  # unknown plot ids.
  plotErrors <- vapply(
    suppressWarnings(validateProject(project))$plots$critical_errors,
    function(e) e$message,
    character(1)
  )
  expect_false(any(grepl("references unknown plotIds", plotErrors)))
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

# Regression (#1126): exportProjectToExcel() replaces Project.xlsx and the
# Configurations workbooks wholesale, defaulting outputDir to the project's own
# directory, so a bare call would silently overwrite hand-maintained workbooks.
# It aborts by default when workbooks already exist in `outputDir`, and only
# overwrites them when `overwrite = TRUE`.
test_that("exportProjectToExcel aborts over existing workbooks unless overwrite = TRUE", {
  work_dir <- withr::local_tempdir()
  file.copy(dirname(exampleProjectPath()), work_dir, recursive = TRUE)
  project <- loadProject(file.path(work_dir, "Example", "Project.json"))

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

# The 5.x parameter-identification layout has no `PITasks` sheet; each sheet is
# keyed by a `PITaskName` column, with the configuration split across
# `PIConfiguration` / `AlgorithmOptions` / `CIOptions`. The importer reassembles
# each task's parameters, output mappings, and nested configuration into the
# same shape the newer single-sheet layout produces (#1158).
test_that(".parseExcelParameterIdentification parses the 5.x PITaskName layout", {
  piFile <- testthat::test_path(
    "data",
    "TestProjectExcel",
    "Configurations",
    "ParameterIdentification.xlsx"
  )
  tasks <- .parseExcelParameterIdentification(piFile)
  expect_setequal(
    vapply(tasks, function(t) t$id, character(1)),
    c("AciclovirSimple", "AciclovirSimplePathId", "AciclovirMultiScenario")
  )

  simple <- tasks[[which(
    vapply(tasks, function(t) t$id == "AciclovirSimple", logical(1))
  )]]

  # A parameter joins `Container Path` and `Parameter Name` into the flat path
  # and coins a canonical id from the parameter name.
  expect_length(simple$parameters, 1L)
  param <- simple$parameters[[1]]
  expect_identical(param$id, "lipophilicity")
  expect_identical(param$path, "Aciclovir|Lipophilicity")
  expect_identical(param$units, "Log Units")
  expect_identical(param$minValue, -10)

  # The configuration gathers the scalar fields, the algorithm options, and the
  # CI options from their separate sheets.
  expect_identical(simple$configuration$algorithm, "BOBYQA")
  expect_identical(simple$configuration$ciMethod, "hessian")
  expect_identical(simple$configuration$algorithmOptions$maxeval, 100)
  expect_identical(simple$configuration$ciOptions$confLevel, 0.95)

  # An output mapping keyed by a full OSPS path resolves to the output-path id
  # when the value matches an `outputPaths` definition; the observed-data DataSet
  # name is kept verbatim.
  mapping <- simple$outputMappings[[1]]
  expect_true(startsWith(mapping$observedData, "Laskin 1982.Group A"))
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

test_that(".pi5xPath drops a blank parameter name instead of building a `|NA` path", {
  expect_null(.pi5xPath("Aciclovir", NA))
  expect_null(.pi5xPath("Aciclovir", ""))
  expect_identical(
    .pi5xPath("Aciclovir", "Lipophilicity"),
    "Aciclovir|Lipophilicity"
  )
  # A blank container with a real parameter still yields the parameter alone.
  expect_identical(.pi5xPath(NA, "Lipophilicity"), "Lipophilicity")
})

test_that(".pi5xOptionRows coerces a boolean-string option but keeps numbers numeric", {
  df <- data.frame(
    PITaskName = c("t", "t", "t", "t"),
    OptionName = c("flag_true", "flag_false", "count", "method"),
    OptionValue = c("TRUE", "false", "5", "hessian"),
    stringsAsFactors = FALSE
  )
  o <- .pi5xOptionRows(df, "t")
  expect_identical(o$flag_true, TRUE)
  expect_identical(o$flag_false, FALSE)
  # A numeric value stays numeric (not misread as a boolean).
  expect_identical(o$count, 5)
  expect_identical(o$method, "hessian")
})

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
    indivCanonical <- .canonicalizeOneId(indiv$individualId)
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
  # The section key is the basename (an id cannot hold a path separator).
  expect_identical(names(result$observedData), "Values.xlsx")
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
  expect_named(result$observedData, "Values.xlsx")
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
  work_dir <- withr::local_tempdir()
  file.copy(dirname(testProjectExcelPath()), work_dir, recursive = TRUE)
  projectDir <- file.path(work_dir, "TestProjectExcel")
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

# A sheet named after an individual (the `Indiv1` sheet in the fixture) is that
# individual's own parameter override. The importer creates it as a parameter
# set AND links it on the individual, so the override is applied rather than
# orphaned (#1158).
test_that("the Excel import links an individual to its own override parameter set", {
  out <- withr::local_tempdir()
  jsonPath <- suppressWarnings(importProjectFromExcel(
    testProjectExcelPath(),
    outputDir = out,
    silent = TRUE
  ))
  project <- suppressWarnings(loadProject(jsonPath))

  expect_true("indiv1" %in% names(project$definitions$parameterSets))
  indiv <- .unwrapDefinitionList(project$definitions$individuals)[["indiv1"]]
  expect_true("indiv1" %in% unlist(indiv$parameterSets))
})

# The TestProjectExcel fixture uses the 5.x one-sheet-per-protocol layout (no
# `ApplicationProtocols` sheet). The importer builds one `Application` per
# protocol sheet, each wrapping the same-named parameter set, so a scenario that
# names a protocol by id resolves on load rather than dangling (#1158).
test_that("the per-protocol-sheet import populates applications and resolves the scenario refs", {
  out <- withr::local_tempdir()
  jsonPath <- suppressWarnings(importProjectFromExcel(
    testProjectExcelPath(),
    outputDir = out,
    silent = TRUE
  ))

  project <- suppressWarnings(loadProject(jsonPath))
  expect_setequal(
    names(project$definitions$applications),
    c("aciclovir_iv_250mg", "protocol_250mg", "protocol_500mg")
  )
  # Each application wraps the same-named parameter set.
  expect_identical(
    .unwrapDefinitionList(project$definitions$applications)[[
      "aciclovir_iv_250mg"
    ]]$parameterSets,
    "aciclovir_iv_250mg"
  )

  # The scenarios' `application` references now resolve: loading emits no
  # "undefined application" warning (other cross-reference warnings may remain
  # until the rest of the legacy layout is migrated).
  w <- character()
  withCallingHandlers(
    loadProject(jsonPath),
    warning = function(cnd) {
      w <<- c(w, conditionMessage(cnd))
      invokeRestart("muffleWarning")
    }
  )
  expect_false(any(grepl("undefined application", w)))
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
  work_dir <- withr::local_tempdir()
  file.copy(dirname(testProjectExcelPath()), work_dir, recursive = TRUE)
  projectDir <- file.path(work_dir, "TestProjectExcel")
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

# In the 5.x applications layout every sheet is a protocol, so a skipped sheet
# must not become an `Application` either: it would wrap a parameter set that
# was never created and dangle on load (#1181).
test_that("a non-parameter sheet in the applications workbook becomes neither a set nor an application", {
  work_dir <- withr::local_tempdir()
  file.copy(dirname(testProjectExcelPath()), work_dir, recursive = TRUE)
  projectDir <- file.path(work_dir, "TestProjectExcel")
  appsFile <- file.path(projectDir, "Configurations", "Applications.xlsx")

  sheetNames <- readxl::excel_sheets(appsFile)
  sheets <- stats::setNames(
    lapply(sheetNames, function(s) readExcel(appsFile, sheet = s)),
    sheetNames
  )
  sheets[["Fit bounds"]] <- data.frame(
    Bound = c("lower", "upper"),
    Value = c(0.1, 10),
    stringsAsFactors = FALSE
  )
  .writeExcel(sheets, appsFile)

  jsonPath <- suppressWarnings(importProjectFromExcel(
    file.path(projectDir, "ProjectConfiguration.xlsx"),
    outputDir = withr::local_tempdir(),
    silent = TRUE
  ))
  project <- suppressWarnings(loadProject(jsonPath))

  expect_false("fit_bounds" %in% names(project$definitions$applications))
  expect_false("fit_bounds" %in% names(project$definitions$parameterSets))
  expect_setequal(
    names(project$definitions$applications),
    c("aciclovir_iv_250mg", "protocol_250mg", "protocol_500mg")
  )
})

# A sheet named after an individual is normally that individual's own override.
# When such a sheet is not a parameter sheet it is skipped, so the individual
# must not gain a `parameterSets` reference to a set that does not exist (#1181).
test_that("a skipped sheet named after an individual is not linked to that individual", {
  work_dir <- withr::local_tempdir()
  file.copy(dirname(testProjectExcelPath()), work_dir, recursive = TRUE)
  projectDir <- file.path(work_dir, "TestProjectExcel")
  indivFile <- file.path(projectDir, "Configurations", "Individuals.xlsx")

  .writeExcel(
    list(
      IndividualBiometrics = readExcel(
        indivFile,
        sheet = "IndividualBiometrics"
      ),
      Indiv1 = data.frame(
        Note = "scratch, not a parameter sheet",
        stringsAsFactors = FALSE
      )
    ),
    indivFile
  )

  jsonPath <- suppressWarnings(importProjectFromExcel(
    file.path(projectDir, "ProjectConfiguration.xlsx"),
    outputDir = withr::local_tempdir(),
    silent = TRUE
  ))
  project <- suppressWarnings(loadProject(jsonPath))

  expect_false("indiv1" %in% names(project$definitions$parameterSets))
  indiv <- .unwrapDefinitionList(project$definitions$individuals)[["indiv1"]]
  expect_false("indiv1" %in% unlist(indiv$parameterSets))
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

test_that("importProjectFromExcel reports what it produced, and stays quiet under silent", {
  work_dir <- withr::local_tempdir()
  file.copy(dirname(testProjectExcelPath()), work_dir, recursive = TRUE)
  projectDir <- file.path(work_dir, "TestProjectExcel")
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

  # The output path, the per-section counts, and the assets that travelled.
  expect_match(summaryText, "Project.json", fixed = TRUE)
  expect_match(summaryText, "Scenarios: 8", fixed = TRUE)
  expect_match(summaryText, "Copied 2 referenced folders", fixed = TRUE)

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
  work_dir <- withr::local_tempdir()
  file.copy(dirname(testProjectExcelPath()), work_dir, recursive = TRUE)
  projectDir <- file.path(work_dir, "TestProjectExcel")

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

test_that("importProjectFromExcel does not copy the results folder or the Excel workbooks", {
  work_dir <- withr::local_tempdir()
  file.copy(dirname(testProjectExcelPath()), work_dir, recursive = TRUE)
  projectDir <- file.path(work_dir, "TestProjectExcel")

  outputDir <- withr::local_tempdir()
  suppressWarnings(importProjectFromExcel(
    file.path(projectDir, "ProjectConfiguration.xlsx"),
    outputDir = outputDir,
    silent = TRUE
  ))

  # `outputFolder` holds what the project writes, not what it reads.
  expect_false(dir.exists(file.path(outputDir, "Results")))
  # The Excel side is the source, not an asset of the JSON project.
  expect_false(dir.exists(file.path(outputDir, "Configurations")))
})

# A `../`-climbing folder value names something the project does not own.
# Copying it would read outside the source project and write outside outputDir,
# overwriting whatever sits beside it.
test_that("importProjectFromExcel refuses to copy a folder that escapes the project", {
  work_dir <- withr::local_tempdir()
  file.copy(dirname(testProjectExcelPath()), work_dir, recursive = TRUE)
  projectDir <- file.path(work_dir, "TestProjectExcel")

  # A sibling of the project, which must not be touched.
  bystander <- file.path(work_dir, "Bystander")
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
  work_dir <- withr::local_tempdir()
  file.copy(dirname(testProjectExcelPath()), work_dir, recursive = TRUE)
  projectDir <- file.path(work_dir, "TestProjectExcel")

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
  work_dir <- withr::local_tempdir()
  file.copy(dirname(testProjectExcelPath()), work_dir, recursive = TRUE)
  projectDir <- file.path(work_dir, "TestProjectExcel")

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
  work_dir <- withr::local_tempdir()
  file.copy(dirname(testProjectExcelPath()), work_dir, recursive = TRUE)
  projectDir <- file.path(work_dir, "TestProjectExcel")
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
  work_dir <- withr::local_tempdir()
  file.copy(dirname(testProjectExcelPath()), work_dir, recursive = TRUE)
  projectDir <- file.path(work_dir, "TestProjectExcel")

  # Reach the very same directory by a symlink, and hand that spelling to
  # outputDir while the workbook is named by its real path.
  linked <- file.path(work_dir, "linked")
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

# The clash is detected on the canonical id, so the rename map has to be looked
# up canonically too: a cell spelling the sheet in a different case must follow
# the rename, not keep resolving to the earlier workbook's set.
test_that("importProjectFromExcel re-points a case-differing parameter-set reference", {
  work_dir <- withr::local_tempdir()
  file.copy(dirname(testProjectExcelPath()), work_dir, recursive = TRUE)
  projectDir <- file.path(work_dir, "TestProjectExcel")
  individualsFile <- file.path(projectDir, "Configurations", "Individuals.xlsx")

  # The individuals workbook's sheet is `Indiv1`; its own `ParameterSets` cell
  # names it in lower case. Give the model-parameters workbook a clashing sheet
  # so the individuals one is the one that gets renamed.
  modelParamsFile <- file.path(
    projectDir,
    "Configurations",
    "ModelParameters.xlsx"
  )
  mpSheets <- readxl::excel_sheets(modelParamsFile)
  clashing <- data.frame(
    "Container Path" = "Organism|Liver",
    "Parameter Name" = "Volume",
    Value = 1,
    Units = "l",
    check.names = FALSE
  )
  .writeExcel(
    c(
      stats::setNames(
        lapply(mpSheets, function(s) readExcel(modelParamsFile, sheet = s)),
        mpSheets
      ),
      list(Indiv1 = clashing)
    ),
    modelParamsFile
  )

  indivSheets <- readxl::excel_sheets(individualsFile)
  contents <- stats::setNames(
    lapply(indivSheets, function(s) readExcel(individualsFile, sheet = s)),
    indivSheets
  )
  contents$IndividualBiometrics$`Individual Parameter Sets` <- "indiv1"
  .writeExcel(contents, individualsFile)

  jsonPath <- suppressWarnings(importProjectFromExcel(
    file.path(projectDir, "ProjectConfiguration.xlsx"),
    outputDir = withr::local_tempdir(),
    silent = TRUE
  ))
  project <- suppressWarnings(loadProject(jsonPath))

  # The lower-case cell followed the rename instead of resolving to the
  # model-parameters sheet that took the plain id.
  expect_identical(
    unlist(project$definitions$individuals[["indiv1"]]$parameterSets),
    "indiv1_1"
  )
})

test_that("importProjectFromExcel names a referenced folder the Excel project does not have", {
  work_dir <- withr::local_tempdir()
  file.copy(dirname(testProjectExcelPath()), work_dir, recursive = TRUE)
  projectDir <- file.path(work_dir, "TestProjectExcel")

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
  work_dir <- withr::local_tempdir()
  file.copy(dirname(testProjectExcelPath()), work_dir, recursive = TRUE)
  projectDir <- file.path(work_dir, "TestProjectExcel")

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

# An observed DataCombined row may name a scenario just as a simulated row does.
# Both are the same kind of reference, so both must land on the canonical id;
# leaving the observed one at its Excel spelling puts two casings of one scenario
# in a single definition file and hides the observed block from any check keyed on
# the canonical id.
test_that("importProjectFromExcel canonicalizes an observed dataCombined entry's scenario reference", {
  work_dir <- withr::local_tempdir()
  file.copy(dirname(testProjectExcelPath()), work_dir, recursive = TRUE)
  projectDir <- file.path(work_dir, "TestProjectExcel")
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

# Before 6.0.0 the model-parameters, individuals, and applications workbooks were
# three separate parameter-set namespaces, so a legacy project may legitimately
# use one sheet name in two of them. They now share a single namespace: the
# workbook parsed first keeps the plain id, the later sheet is renamed, and the
# references that later workbook makes follow the rename. Without the re-pointing
# the renamed set is orphaned and the referrer silently resolves to the *other*
# workbook's set.
test_that("importProjectFromExcel renames a duplicate parameter-set id and re-points its own workbook's references", {
  work_dir <- withr::local_tempdir()
  file.copy(dirname(testProjectExcelPath()), work_dir, recursive = TRUE)
  projectDir <- file.path(work_dir, "TestProjectExcel")
  modelParamsFile <- file.path(
    projectDir,
    "Configurations",
    "ModelParameters.xlsx"
  )

  # Add model-parameter sheets clashing with the individuals workbook's `Indiv1`
  # sheet and the applications workbook's `Protocol_250mg` sheet. Model
  # parameters are parsed first, so both of those get renamed.
  clashing <- data.frame(
    "Container Path" = "Organism|Liver",
    "Parameter Name" = "Volume",
    Value = 1,
    Units = "l",
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  sheets <- readxl::excel_sheets(modelParamsFile)
  .writeExcel(
    c(
      stats::setNames(
        lapply(sheets, function(s) readExcel(modelParamsFile, sheet = s)),
        sheets
      ),
      list(Indiv1 = clashing, Protocol_250mg = clashing)
    ),
    modelParamsFile
  )

  jsonPath <- suppressWarnings(importProjectFromExcel(
    file.path(projectDir, "ProjectConfiguration.xlsx"),
    outputDir = withr::local_tempdir(),
    silent = TRUE
  ))
  project <- suppressWarnings(loadProject(jsonPath))

  # Both sets survive: the model-parameters sheet keeps the plain id, the later
  # workbook's sheet gets the suffixed one.
  sets <- .unwrapDefinitionList(project$definitions$parameterSets)
  expect_contains(
    names(sets),
    c("indiv1", "indiv1_1", "protocol_250mg", "protocol_250mg_1")
  )

  # The individual still carries its OWN parameter set, not the model-parameters
  # sheet that took the id.
  expect_identical(
    unlist(project$definitions$individuals[["indiv1"]]$parameterSets),
    "indiv1_1"
  )
  # Same for the 5.x application wrapper built around its protocol sheet.
  expect_identical(
    unlist(project$definitions$applications[["protocol_250mg"]]$parameterSets),
    "protocol_250mg_1"
  )
})

test_that("importProjectFromExcel warns naming each renamed duplicate parameter set", {
  work_dir <- withr::local_tempdir()
  file.copy(dirname(testProjectExcelPath()), work_dir, recursive = TRUE)
  projectDir <- file.path(work_dir, "TestProjectExcel")
  individualsFile <- file.path(
    projectDir,
    "Configurations",
    "Individuals.xlsx"
  )

  # Rename the individuals workbook's parameter sheet onto a model-parameters
  # sheet name (`Global`), so the individuals workbook is the one that loses the
  # id and the warning names it.
  sheets <- readxl::excel_sheets(individualsFile)
  contents <- stats::setNames(
    lapply(sheets, function(s) readExcel(individualsFile, sheet = s)),
    sheets
  )
  names(contents)[names(contents) == "Indiv1"] <- "Global"
  .writeExcel(contents, individualsFile)

  # Snapshot the rename warning alone, caught by its own condition class: the
  # import also raises locale-dependent unit-encoding warnings that would make a
  # whole-call snapshot machine-specific.
  renameWarning <- NULL
  suppressWarnings(withCallingHandlers(
    importProjectFromExcel(
      file.path(projectDir, "ProjectConfiguration.xlsx"),
      outputDir = withr::local_tempdir(),
      silent = TRUE
    ),
    esqlabsR_importRenamedParameterSets = function(cnd) {
      renameWarning <<- conditionMessage(cnd)
      invokeRestart("muffleWarning")
    }
  ))

  expect_snapshot(cat(renameWarning))
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

  # Non-silent: a warning surfaces naming the comparison failure.
  expect_warning(
    projectStatus(project, silent = FALSE),
    "Cannot compare the Excel configuration files"
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

# `OverwriteFormulasInSS` is newer than the 5.x layout, so a pre-6.0 Scenarios
# sheet omits it. Its absence must default to FALSE rather than abort, matching
# the sibling `InitialConditions` column (#1158).
test_that(".parseExcelScenarios defaults OverwriteFormulasInSS when the column is absent", {
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
    # No `OverwriteFormulasInSS` column, as in a pre-6.0 sheet.
    ModelFile = "m.pkml",
    OutputPathsIds = "op1",
    stringsAsFactors = FALSE
  )
  scenarios <- .parseExcelScenarios(scenarioDf)
  # The parser drops the absent value to NULL; the JSON serializer defaults it
  # to FALSE (`%||% FALSE`), so a round-trip through the tree reads FALSE.
  expect_null(scenarios[[1]]$overwriteFormulasInSS)
})

# A blank steady-state time/unit defaults to the same values the authoring API
# writes (`1000` / `"min"`), not null, so an imported project is byte-identical
# to the same project re-authored through `addScenario()` (#1158).
test_that(".parseExcelScenarios defaults a blank steady-state time and unit", {
  scenarioDf <- data.frame(
    Scenario_name = "s1",
    IndividualId = NA_character_,
    PopulationId = NA_character_,
    ReadPopulationFromCSV = NA,
    ModelParameterSheets = NA_character_,
    ApplicationProtocol = NA_character_,
    SimulationTime = NA_character_,
    SimulationTimeUnit = NA_character_,
    SteadyState = FALSE,
    SteadyStateTime = NA_real_,
    SteadyStateTimeUnit = NA_character_,
    OverwriteFormulasInSS = NA,
    ModelFile = "m.pkml",
    OutputPathsIds = "op1",
    stringsAsFactors = FALSE
  )
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
  expect_named(result$observedData, "Values.xlsx")
})

# A `ParameterSets` / `Individual Parameter Sets` cell names sheets of its own
# workbook. When one of those sheets is skipped it defines no set, so the
# reference has to go too, or the import writes a definition pointing at a set
# that was never created (#1181).
test_that(".dropSkippedSheetRefs removes only references to skipped sheets", {
  definitions <- list(
    a = list(parameterSets = list("Kept", "Skipped")),
    b = list(parameterSets = list("Skipped")),
    c = list(parameterSets = list("Kept")),
    d = list(other = "field")
  )

  result <- .dropSkippedSheetRefs(definitions, "Skipped")

  expect_identical(unlist(result$a$parameterSets), "Kept")
  # Nothing left: the field goes rather than becoming an empty list.
  expect_null(result$b$parameterSets)
  expect_identical(unlist(result$c$parameterSets), "Kept")
  expect_identical(result$d, definitions$d)
  # A reference matching on canonical id alone is still dropped.
  expect_null(
    .dropSkippedSheetRefs(
      list(a = list(parameterSets = list("skipped"))),
      "Skipped"
    )$a$parameterSets
  )
})

# The `ApplicationProtocols` layout takes its references from a column rather
# than from the sheet list, so it needs the same treatment as the 5.x layout
# (#1181).
test_that("an application referencing a skipped sheet loses the reference", {
  work_dir <- withr::local_tempdir()
  file.copy(dirname(testProjectExcelPath()), work_dir, recursive = TRUE)
  projectDir <- file.path(work_dir, "TestProjectExcel")
  appsFile <- file.path(projectDir, "Configurations", "Applications.xlsx")

  # Rebuild the workbook in the newer layout: a protocol naming two sheets, one
  # a real parameter sheet and one a notes sheet.
  .writeExcel(
    list(
      ApplicationProtocols = data.frame(
        ApplicationId = "Protocol_250mg",
        ParameterSets = "RealSheet, Scratch notes",
        stringsAsFactors = FALSE
      ),
      RealSheet = readExcel(appsFile, sheet = "Protocol_250mg"),
      `Scratch notes` = data.frame(Note = "not a parameter sheet")
    ),
    appsFile
  )

  jsonPath <- suppressWarnings(importProjectFromExcel(
    file.path(projectDir, "ProjectConfiguration.xlsx"),
    outputDir = withr::local_tempdir(),
    silent = TRUE
  ))
  project <- suppressWarnings(loadProject(jsonPath))

  application <- .unwrapDefinitionList(project$definitions$applications)[[
    "protocol_250mg"
  ]]
  expect_identical(unlist(application$parameterSets), "realsheet")
  expect_false("scratch_notes" %in% names(project$definitions$parameterSets))
})

# The reported case: a populations sheet whose real rows are followed by rows
# that hold nothing. Each was taken for a population definition and the import
# aborted on the first of them for having no id (#1191).
test_that("blank rows in a populations sheet do not become definitions", {
  work_dir <- withr::local_tempdir()
  file.copy(dirname(testProjectExcelPath()), work_dir, recursive = TRUE)
  projectDir <- file.path(work_dir, "TestProjectExcel")
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
  work_dir <- withr::local_tempdir()
  file.copy(dirname(testProjectExcelPath()), work_dir, recursive = TRUE)
  projectDir <- file.path(work_dir, "TestProjectExcel")
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

# The reported case: a `PIOutputMappings` sheet from the layout that predates the
# `OutputPath` column. Every mapping came out with no output path and the restore
# stopped on the first one. The column's absence is now read as that older
# layout, whose outputs come from the scenario (#1192).
test_that("a PIOutputMappings sheet with no OutputPath column imports", {
  work_dir <- withr::local_tempdir()
  file.copy(dirname(testProjectExcelPath()), work_dir, recursive = TRUE)
  projectDir <- file.path(work_dir, "TestProjectExcel")
  configDir <- file.path(projectDir, "Configurations")

  rewriteSheet <- function(file, sheet, edit) {
    sheetNames <- readxl::excel_sheets(file)
    sheets <- stats::setNames(
      lapply(sheetNames, function(s) readExcel(file, sheet = s)),
      sheetNames
    )
    sheets[[sheet]] <- edit(sheets[[sheet]])
    .writeExcel(sheets, file)
  }

  # The fixture's PI scenarios declare no output path, so give the one this task
  # uses two of them: the derivation has to fan a single row out over both.
  rewriteSheet(
    file.path(configDir, "Scenarios.xlsx"),
    "Scenarios",
    function(df) {
      df$OutputPathsIds[df$Scenario_name == "PITestScenario"] <-
        "Aciclovir_PVB, Aciclovir_fat_cell"
      df
    }
  )
  # Reproduce the older layout: no `OutputPath` column at all.
  rewriteSheet(
    file.path(configDir, "ParameterIdentification.xlsx"),
    "PIOutputMappings",
    function(df) df[, setdiff(names(df), "OutputPath"), drop = FALSE]
  )

  jsonPath <- suppressWarnings(importProjectFromExcel(
    file.path(projectDir, "ProjectConfiguration.xlsx"),
    outputDir = withr::local_tempdir(),
    silent = TRUE
  ))
  project <- suppressWarnings(loadProject(jsonPath))

  task <- .unwrapDefinitionList(project$definitions$parameterIdentification)[[
    "aciclovirsimple"
  ]]
  # One mapping per output path the scenario declares, each carrying the row's
  # observed data set.
  expect_setequal(
    vapply(task$outputMappings, function(m) m$outputPath, character(1)),
    c("aciclovir_pvb", "aciclovir_fat_cell")
  )
  expect_true(all(vapply(
    task$outputMappings,
    function(m) grepl("^Laskin 1982", m$observedData),
    logical(1)
  )))

  # The whole point: the project the import produced is loadable, and the task
  # whose scenario declares output paths has no complaint against it.
  results <- suppressWarnings(validateProject(project))
  errors <- unlist(lapply(results, function(section) {
    vapply(section$critical_errors %||% list(), \(e) e$message, character(1))
  })) %||%
    character()
  expect_false(any(grepl("aciclovirsimple'.*outputPath", errors)))

  # The fixture's other task runs against scenarios that declare no output path,
  # so its mappings cannot be derived. They load anyway and validation names them,
  # rather than vanishing from the task.
  other <- .unwrapDefinitionList(project$definitions$parameterIdentification)[[
    "aciclovirmultiscenario"
  ]]
  expect_length(other$outputMappings, 2L)
  expect_true(any(grepl(
    "aciclovirmultiscenario'.*does not define an outputPath",
    errors
  )))
})

# The reported case: a fresh, never-edited import came back invalid with one
# critical error per observed curve, and nothing in the import said why. The
# curve is kept (the user may still be able to fill the cell), so the import
# names the affected data combinations and states the consequence (#1183).
test_that("an observed curve with no dataSet is kept and reported", {
  work_dir <- withr::local_tempdir()
  file.copy(dirname(testProjectExcelPath()), work_dir, recursive = TRUE)
  projectDir <- file.path(work_dir, "TestProjectExcel")
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
  work_dir <- withr::local_tempdir()
  file.copy(dirname(testProjectExcelPath()), work_dir, recursive = TRUE)
  projectDir <- file.path(work_dir, "TestProjectExcel")
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
      dataCombinedId = "plasma",
      simulated = list(list(label = "sim")),
      observed = list(list(label = "obs"))
    ),
    list(
      dataCombinedId = "urine",
      observed = list(list(label = "obs", dataSet = "d1"))
    ),
    list(
      dataCombinedId = "fat",
      observed = list(list(label = "obs", dataSet = ""))
    )
  )))

  # Nothing to say when every observed curve names a data set.
  expect_silent(.warnIncompleteObservedCurves(list(
    list(dataCombinedId = "urine", observed = list(list(dataSet = "d1")))
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

# #1213 item 1: a two-column `Protein` + `Ontogeny` pair is the pre-5.6 way to
# declare ontogenies, and the only spelling any workbook older than 5.6 has. The
# importer reads a single `Protein Ontogenies` column, so it finds nothing and
# every ontogeny is discarded. Nothing in the output says so, which is what makes
# this worse than an error: two individuals or populations differing only in their
# ontogenies import identically.
test_that("a legacy two-column ontogeny pair imports with no ontogenies at all", {
  imported <- importLegacyExcelProject(localLegacyExcelProject())

  # `adult` declares one ontogeny in the workbook, `child` two, `adultpop` two.
  expect_null(
    imported$project$definitions$individuals[["adult"]]$proteinOntogenies
  )
  expect_null(
    imported$project$definitions$individuals[["child"]]$proteinOntogenies
  )
  expect_null(
    imported$project$definitions$populations[["adultpop"]]$proteinOntogenies
  )

  # And the loss is silent: no warning mentions ontogenies.
  expect_false(any(grepl("ontogen", imported$warnings, ignore.case = TRUE)))
})

# #1213 item 1, the other half of the same silence: the individual that declares
# two ontogenies and the one that declares one are indistinguishable after
# import, and so is a population that declares two from one that declares none.
# So nothing in the imported project can be used to detect the loss either.
test_that("declaring ontogenies leaves no trace the import can be checked against", {
  imported <- importLegacyExcelProject(localLegacyExcelProject())
  populations <- imported$project$definitions$populations

  # `adultpop` declares two ontogenies and `csvpop` none; they differ only in
  # fields unrelated to ontogenies.
  expect_identical(
    populations[["adultpop"]]$proteinOntogenies,
    populations[["csvpop"]]$proteinOntogenies
  )
})

# #1213 item 8: v5 merged `PIParameters` rows by
# `(Group, Container Path, Parameter Name)`, so the two rows sharing group 2 here
# were ONE free parameter estimated across both scenarios. The importer never
# reads `Group`, so they become two independent free parameters with independent
# estimates. That changes what the identification computes, not just how the
# project is laid out.
test_that("a repeated parameter-identification Group becomes independent parameters", {
  imported <- importLegacyExcelProject(localLegacyExcelProject())
  task <- imported$project$definitions$parameterIdentification[["aciclovirfit"]]

  # The workbook's three rows are group 1 (Lipophilicity) and group 2 twice
  # (TSspec, once per scenario), so v5 estimated two free parameters. v6
  # estimates three.
  expect_length(task$parameters, 3L)
  expect_identical(
    vapply(task$parameters, function(p) p$id, character(1)),
    c("lipophilicity", "tsspec", "tsspec_2")
  )

  # Nothing reports the split.
  expect_false(any(grepl("group", imported$warnings, ignore.case = TRUE)))
})

# #1213 item 14: the legacy `Units` cell was ignored by v5 and is now assigned as
# the parameter's display unit, so a cell v5 read past can abort `runPI()` (a
# `mg` unit on a parameter whose dimension is an inversed time). The import side
# is what carries it through, so that is what is pinned here.
test_that("a legacy parameter-identification Units cell is carried through", {
  imported <- importLegacyExcelProject(localLegacyExcelProject())
  task <- imported$project$definitions$parameterIdentification[["aciclovirfit"]]

  expect_identical(
    vapply(
      task$parameters,
      function(p) p$units %||% NA_character_,
      character(1)
    ),
    c("Log Units", "mg", "mg")
  )
})

# #1213 item 13: the `crossReferences` phase resolves a mapping's `outputPathId`
# but never its `observedData`, so a mapping naming an observed data set the
# project does not define is reported as no error at all. `runPI()` then aborts at
# build time on a project `validateProject()` called clean.
test_that("a dangling parameter-identification observedData reference is not reported", {
  imported <- importLegacyExcelProject(localLegacyExcelProject())
  task <- imported$project$definitions$parameterIdentification[["aciclovirfit"]]

  # The reference is carried through and resolves to nothing: the observed-data
  # section holds one positional entry with no id to match against.
  expect_type(task$outputMappings[[1]]$observedDataId, "character")
  expect_null(names(imported$project$definitions$observedData))

  # And validation grades the whole project clean regardless.
  summary <- validationSummary(suppressWarnings(validateProject(
    imported$project
  )))
  expect_equal(summary$total_critical_errors, 0)
})

# #1213 item 19: a blank `SimulationTimeUnit` cell imports as `null`, while
# `addScenario()` defaults the same absent value to `"h"`. So the same blank cell
# means a different unit depending on which entrypoint wrote the project.
#
# #1213 item 9 in the same fixture: the scenarios sheet has no
# `OverwriteFormulasInSS` column at all, and the absent column imports as `FALSE`
# rather than as an absent field.
test_that("a blank simulationTime unit and an absent OverwriteFormulasInSS column import as null and FALSE", {
  imported <- importLegacyExcelProject(localLegacyExcelProject())
  scenario <- imported$project$definitions$scenarios[["adultscenario"]]

  # The workbook leaves the cell blank; authoring the same scenario would give it
  # `"h"`.
  expect_null(scenario$simulationTimeUnit)
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

# #1213 item 18: `rowToFields()` copies whatever type readxl guessed for a plots
# column, with no numeric-coercion list of the kind `dataCombined` has. A workbook
# storing `nsd` as text therefore yields the string `"1.96"` where the same field
# authored programmatically is a number, which is the root cause behind every
# `excel_in_sync = FALSE` report on a freshly imported tree.
test_that("a plots field stored as text imports as a string, not a number", {
  imported <- importLegacyExcelProject(localLegacyExcelProject())

  expect_identical(imported$project$definitions$plots[["p3"]]$nsd, "1.96")
})

# #1213 item 10 / the residue #1207 recorded: a 5.x multi-value cell is quoted,
# and a value may itself contain a comma, which is exactly what the quoting is
# for. That half works. A quoted *single*-value reference cell is read raw on both
# the defining and the referencing side, so a consistently quoted workbook
# resolves but its ids carry the quote characters as underscores: the id is
# `_name_` rather than the name the modeller wrote.
test_that("quoted legacy cells resolve, and a quoted single-value id keeps its quotes as underscores", {
  imported <- importLegacyExcelProject(localLegacyExcelProject())

  # The quoted multi-value cell splits on the separating commas only, so the
  # sheet name containing a comma survives as one reference.
  expect_identical(
    imported$project$definitions$scenarios[[
      "adultscenario"
    ]]$modelParameterSets,
    c("global", "aciclovir", "sheet__with_comma")
  )

  # The quoted single-value `DataCombinedName` becomes `_name_` on both sides, so
  # nothing dangles and nothing reads as the authored name either.
  expect_setequal(
    names(imported$project$definitions$dataCombined),
    c("_aciclovirpvb_", "_aciclovirpop_")
  )
  expect_identical(
    imported$project$definitions$plots[["p1"]]$dataCombined,
    "_aciclovirpvb_"
  )

  # Consistently quoted, so the project validates: only inconsistent quoting
  # dangles (#1207).
  summary <- validationSummary(suppressWarnings(validateProject(
    imported$project
  )))
  expect_equal(summary$total_critical_errors, 0)
})

# #1213 item 5: `populationsFolder` is resolved against the project root, while
# the convention (this fixture, the bundled one, and the legacy-snapshot
# materializer's own output) places it under the configurations folder. So the
# folder is never found and never copied, and the report that would say so is
# gated on `!silent`, which is off on the path where this always applies.
#
# The result is the worst available shape: `validateProject()` reports no error,
# because it checks `modelFile` and observed-data existence but has no check for
# the population CSV, and the scenario then dies at run time on a raw .NET error.
test_that("the populations CSV folder does not travel with an imported project", {
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

  # The populations folder did not, under either spelling.
  expect_false(dir.exists(file.path(imported$outputDir, "PopulationsCSV")))
  expect_false(dir.exists(
    file.path(imported$outputDir, "Configurations", "PopulationsCSV")
  ))

  # And `silent = TRUE` suppresses the one report that would have said so.
  expect_false(any(grepl("not copied", imported$warnings, fixed = TRUE)))

  # Validation still calls the project clean, so nothing warns the user before
  # the scenario reaches the solver.
  summary <- validationSummary(suppressWarnings(validateProject(
    imported$project
  )))
  expect_equal(summary$total_critical_errors, 0)
})

# The tests below each derive a variant of the legacy fixture by mutating exactly
# one workbook of a throwaway copy, so a variant differs from the base in one
# dimension and cannot interfere with another test's subject.

# #1213 item 8: workbook resolution is purely property-driven. The section loop
# has no `else` branch for a workbook the property sheet names but that is not on
# disk, so the section imports as zero in complete silence. The one cue that could
# have caught it is suppressed too, because a zero-count section is left out of
# the import summary altogether.
test_that("a named but absent workbook imports as an empty section, unreported", {
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

  # Both populations are gone, and nothing names the workbook that is missing.
  expect_length(imported$project$definitions$populations, 0L)
  expect_false(any(grepl("Populations.xlsx", imported$warnings, fixed = TRUE)))

  # The summary lists the sections that imported something and omits the one that
  # imported nothing, so the zero is not visible there either.
  summary <- paste(messages, collapse = "")
  expect_match(summary, "Individuals")
  expect_no_match(summary, "Populations")

  # The only trace is downstream, at load: the two scenarios that name a
  # population now dangle, which describes the symptom rather than the cause.
  expect_true(any(grepl("undefined population", imported$warnings)))
})

# #1213 item 9, first route: a parameter sheet whose column headers are duplicated
# is correctly rejected as not a parameter sheet, and the rejection is reported.
# What is not reported is the consequence: the individual named after that sheet
# silently loses its whole parametrization, while its siblings keep theirs.
test_that("a parameter sheet with duplicated headers costs its individual the parametrization", {
  projectDir <- localLegacyExcelProject()
  editWorkbookSheets(
    file.path(projectDir, "Configurations", "Individuals.xlsx"),
    function(sheets) {
      names(sheets$Child) <- c(
        "Container Path",
        "Parameter Name",
        "Value",
        "Value"
      )
      sheets
    }
  )
  imported <- importLegacyExcelProject(projectDir)

  # `adult` links to its own sheet; `child` has nothing left to link to.
  expect_identical(
    unlist(imported$project$definitions$individuals[["adult"]]$parameterSets),
    "adult"
  )
  expect_null(imported$project$definitions$individuals[["child"]]$parameterSets)
  expect_false("child" %in% names(imported$project$definitions$parameterSets))

  # The sheet is reported, and that is the whole of what the import says: one
  # warning, about the sheet. Nothing follows it to say which individual just
  # lost its parametrization.
  expect_length(imported$warnings, 1L)
  expect_match(imported$warnings, "Skipped sheet")
})

# #1213 item 9, second route: individuals are keyed off the biometrics rows, so an
# individual that has a parameter sheet but no biometrics row is dropped entirely.
# Its sheet still becomes a parameter set, now owned by nobody, and the scenario
# that names the individual is left dangling. Nothing reconciles the two at import
# time; the only report comes later, from validation, and it describes the dangling
# reference rather than the dropped individual.
test_that("an individual with a parameter sheet but no biometrics row is dropped", {
  projectDir <- localLegacyExcelProject()
  editWorkbookSheets(
    file.path(projectDir, "Configurations", "Individuals.xlsx"),
    function(sheets) {
      biometrics <- sheets$IndividualBiometrics
      sheets$IndividualBiometrics <-
        biometrics[biometrics$IndividualId != "Child", , drop = FALSE]
      sheets
    }
  )
  imported <- importLegacyExcelProject(projectDir)

  expect_named(imported$project$definitions$individuals, "adult")

  # The orphaned sheet survives as a parameter set with no individual.
  expect_true("child" %in% names(imported$project$definitions$parameterSets))

  # The scenario keeps pointing at the individual that no longer exists.
  expect_identical(
    imported$project$definitions$scenarios[["childscenario"]]$individualId,
    "child"
  )

  # Nothing at import time says an individual was dropped; the report that does
  # arrive names the reference, not the cause.
  expect_false(any(grepl("dropped|discarded", imported$warnings)))
  expect_true(any(grepl("undefined individual", imported$warnings)))
})

# #1213 item 10: the plot definitions are keyed by id, with no duplicate check, so
# a second row carrying an id an earlier row already used overwrites it. The
# reported count is the count of surviving plots, so a workbook row that vanished
# is invisible unless the reader counts the workbook themselves.
test_that("two plot rows sharing an id silently lose one plot", {
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

  # And nothing reports a duplicate id.
  expect_false(any(grepl("duplicate", imported$warnings, ignore.case = TRUE)))
})

# #1213 item 16: `.canonicalizeId()` replaces whitespace through `[[:space:]]`,
# which matches neither U+00A0 (no-break space) nor U+200B (zero-width space). So
# an invisible character survives canonicalization into the id and from there into
# the definition filename. Two ids differing only by a zero-width space become two
# distinct definition files whose names render identically, and the project
# validates clean, so nothing tells the author their two ids are not one typo.
#
# This is live data rather than a synthetic probe: one tested project carried 12
# real ids containing U+00A0.
test_that("an id containing an invisible character survives into the definition filename", {
  nbsp <- " "
  zwsp <- "​"
  projectDir <- localLegacyExcelProject()
  editWorkbookSheets(
    file.path(projectDir, "Configurations", "Scenarios.xlsx"),
    function(sheets) {
      sheets$OutputPaths$OutputPathId <- c(
        "OutPath",
        paste0("Out", zwsp, "Path")
      )
      sheets$OutputPaths <- rbind(
        sheets$OutputPaths,
        data.frame(
          OutputPathId = paste0("Renal", nbsp, "Clearance"),
          OutputPath = "Organism|Kidney|Aciclovir|Concentration in container"
        )
      )
      # Reference them consistently, so nothing dangles and the invisible
      # characters are the only thing under test.
      sheets$Scenarios$OutputPathsIds <- c(
        "OutPath",
        paste0("OutPath, Out", zwsp, "Path"),
        NA,
        NA,
        "OutPath",
        "OutPath"
      )
      sheets
    }
  )
  imported <- importLegacyExcelProject(projectDir)
  ids <- names(imported$project$definitions$outputPaths)

  # Three distinct ids, two of which render identically.
  expect_length(ids, 3L)
  expect_true(paste0("out", zwsp, "path") %in% ids)
  expect_true(paste0("renal", nbsp, "clearance") %in% ids)

  # The invisible characters reach the filenames too.
  files <- list.files(file.path(
    imported$outputDir,
    "definitions",
    "output-paths"
  ))
  expect_true(paste0("out", zwsp, "path.json") %in% files)
  expect_true(paste0("renal", nbsp, "clearance.json") %in% files)

  # `outpath.json` and `out<U+200B>path.json` are two files that look like one.
  expect_length(unique(files), 3L)
  expect_length(unique(gsub(zwsp, "", files, fixed = TRUE)), 2L)

  # Nothing warns, and validation is clean.
  expect_false(any(grepl("id", imported$warnings, ignore.case = TRUE)))
  summary <- validationSummary(suppressWarnings(validateProject(
    imported$project
  )))
  expect_equal(summary$total_critical_errors, 0)
})
