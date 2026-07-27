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
  expect_true(
    "testscenario" %in%
      unlist(lapply(
        dataCombined,
        function(dc) vapply(dc$simulated, function(e) e$scenario, character(1))
      ))
  )
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
  expect_true(all(
    c("indiv1", "indiv1_1", "protocol_250mg", "protocol_250mg_1") %in%
      names(sets)
  ))

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

# Regression (#1125): when the Excel side-car cannot be re-imported for the
# comparison (here because two output-path ids in it collapse to one canonical
# id), the status check must catch that abort and report the Excel axis as the
# NA "cannot compare" state with a warning, rather than propagating a hard error
# out of `projectStatus()`. The collision is injected into the exported side-car
# (not the source), so the project itself loads cleanly and only the comparison
# re-import hits the collision.
test_that("projectStatus() does not abort on a side-car canonicalization collision", {
  # Copy the fixture with the entry workbook named Project.xlsx, so the imported
  # container is Project.json and the exported side-car stem matches what the
  # status check derives.
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
