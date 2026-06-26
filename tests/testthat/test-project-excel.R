test_that("snapshotProjectConfiguration warns with lifecycle_warning_deprecated", {
  withr::local_options(lifecycle_verbosity = "warning")
  expect_warning(
    tryCatch(
      snapshotProjectConfiguration(
        "nonexistent.xlsx",
        outputDir = withr::local_tempdir(),
        silent = TRUE
      ),
      error = function(e) NULL
    ),
    class = "lifecycle_warning_deprecated"
  )
})

test_that("restoreProjectConfiguration warns with lifecycle_warning_deprecated", {
  withr::local_options(lifecycle_verbosity = "warning")
  expect_warning(
    tryCatch(
      restoreProjectConfiguration(
        "nonexistent.json",
        outputDir = withr::local_tempdir(),
        silent = TRUE
      ),
      error = function(e) NULL
    ),
    class = "lifecycle_warning_deprecated"
  )
})

test_that("projectConfigurationStatus warns with lifecycle_warning_deprecated", {
  withr::local_options(lifecycle_verbosity = "warning")
  expect_warning(
    tryCatch(
      projectConfigurationStatus("nonexistent.xlsx"),
      error = function(e) NULL
    ),
    class = "lifecycle_warning_deprecated"
  )
})

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
    unlist(reimported$outputPaths),
    unlist(project$outputPaths)
  )
})

test_that("Project round-trips through Excel preserving modelParameterSets", {
  work_dir <- withr::local_tempdir()
  file.copy(dirname(exampleProjectPath()), work_dir, recursive = TRUE)
  project <- loadProject(file.path(work_dir, "Example", "Project.json"))

  expect_gt(length(project$modelParameterSets), 0)

  excel_out <- withr::local_tempdir()
  exportProjectToExcel(project, outputDir = excel_out, silent = TRUE)
  reimportedJson <- importProjectFromExcel(
    file.path(excel_out, "Project.xlsx"),
    silent = TRUE
  )
  reimported <- loadProject(reimportedJson)

  toJson <- function(x) {
    jsonlite::toJSON(x, auto_unbox = TRUE, null = "null", digits = NA)
  }
  expect_equal(
    toJson(reimported$modelParameterSets),
    toJson(project$modelParameterSets)
  )
})

test_that("parameter sets round-trip through Excel sheet-name sanitization", {
  # A set name that is both > 31 chars and uses Excel-forbidden chars,
  # plus an empty set, must round-trip without crashing the writer and
  # without orphaning the canonical name scenarios reference.
  badName <- "Liver/Plasma Ratio: very-long-name [PK]"
  paramSets <- list()
  paramSets[[badName]] <- list(
    list(
      containerPath = "Organism|Liver",
      parameterName = "Foo",
      value = 1.5,
      units = NULL
    )
  )
  paramSets[["EmptySet"]] <- list()

  sheets <- esqlabsR:::.parameterStructuresToExcelSheets(paramSets)
  expect_true(all(nchar(names(sheets)) <= 31))

  tmp <- withr::local_tempfile(fileext = ".xlsx")
  expect_no_error(writexl::write_xlsx(sheets, path = tmp))

  reimported <- esqlabsR:::.parseExcelParameterSheets(tmp)
  expect_setequal(names(reimported), c(badName, "EmptySet"))
  expect_length(reimported[["EmptySet"]], 0)

  toJson <- function(x) {
    jsonlite::toJSON(x, auto_unbox = TRUE, null = "null", digits = NA)
  }
  expect_equal(
    toJson(reimported[[badName]]),
    toJson(paramSets[[badName]])
  )
})

test_that("Project round-trips through Excel preserving plots sections", {
  work_dir <- withr::local_tempdir()
  file.copy(dirname(exampleProjectPath()), work_dir, recursive = TRUE)
  project <- loadProject(file.path(work_dir, "Example", "Project.json"))

  expect_gt(length(project$plots$dataCombined), 0)

  toJson <- function(x) {
    jsonlite::toJSON(x, auto_unbox = TRUE, null = "null", digits = NA)
  }
  roundtrip <- function(p) {
    excel_out <- withr::local_tempdir()
    exportProjectToExcel(p, outputDir = excel_out, silent = TRUE)
    json <- importProjectFromExcel(
      file.path(excel_out, "Project.xlsx"),
      silent = TRUE
    )
    loadProject(json)
  }

  # dataCombined survives the round trip (it was previously dropped on
  # export). The fixed point holds from the first export onward; compare
  # two successive round trips so source-only present-null fields, which
  # collapse to absent on the first pass, do not register as drift.
  rt1 <- roundtrip(project)
  expect_gt(length(rt1$plots$dataCombined), 0)
  expect_named(rt1$plots$dataCombined, names(project$plots$dataCombined))

  rt2 <- roundtrip(rt1)
  for (section in c("dataCombined", "plotConfiguration", "plotGrids")) {
    expect_equal(
      toJson(rt2$plots[[section]]),
      toJson(rt1$plots[[section]])
    )
  }
})

test_that("Project round-trips through Excel preserving observedData", {
  work_dir <- withr::local_tempdir()
  file.copy(dirname(exampleProjectPath()), work_dir, recursive = TRUE)
  project <- loadProject(file.path(work_dir, "Example", "Project.json"))

  expect_gt(length(project$observedData), 0)

  excel_out <- withr::local_tempdir()
  exportProjectToExcel(project, outputDir = excel_out, silent = TRUE)
  reimportedJson <- importProjectFromExcel(
    file.path(excel_out, "Project.xlsx"),
    silent = TRUE
  )
  reimported <- loadProject(reimportedJson)

  toJson <- function(x) {
    jsonlite::toJSON(x, auto_unbox = TRUE, null = "null", digits = NA)
  }
  expect_equal(
    toJson(reimported$observedData),
    toJson(project$observedData)
  )
})

test_that(".observedDataToExcelDf round-trips heterogeneous entry types", {
  observedData <- list(
    list(
      type = "excel",
      file = "a.xlsx",
      importerConfiguration = "cfg.xml",
      sheets = list("S1", "S2, with comma")
    ),
    list(type = "pkml", file = "b.pkml"),
    list(type = "script", file = "c.R")
  )

  df <- esqlabsR:::.observedDataToExcelDf(observedData)
  tmp <- withr::local_tempfile(fileext = ".xlsx")
  writexl::write_xlsx(list(ObservedData = df), tmp)
  reimported <- esqlabsR:::.parseExcelObservedData(
    readExcel(tmp, sheet = "ObservedData")
  )

  toJson <- function(x) {
    jsonlite::toJSON(x, auto_unbox = TRUE, null = "null", digits = NA)
  }
  expect_equal(toJson(reimported), toJson(observedData))
})

test_that("Project round-trips through Excel preserving parameterIdentification", {
  work_dir <- withr::local_tempdir()
  file.copy(dirname(exampleProjectPath()), work_dir, recursive = TRUE)
  project <- loadProject(file.path(work_dir, "Example", "Project.json"))

  expect_gt(length(project$parameterIdentification), 0)

  toJson <- function(x) {
    jsonlite::toJSON(x, auto_unbox = TRUE, null = "null", digits = NA)
  }
  roundtrip <- function(p) {
    excel_out <- withr::local_tempdir()
    exportProjectToExcel(p, outputDir = excel_out, silent = TRUE)
    json <- importProjectFromExcel(
      file.path(excel_out, "Project.xlsx"),
      silent = TRUE
    )
    loadProject(json)
  }

  # The nested PITask/PIParameter/PIOutputMapping/configuration structure
  # survives. Two successive round trips reach a fixed point (the source's
  # empty-string units normalize to absent on the first pass).
  rt1 <- roundtrip(project)
  expect_named(
    rt1$parameterIdentification,
    names(project$parameterIdentification)
  )

  task <- rt1$parameterIdentification[[1]]
  expect_length(
    task$parameters,
    length(project$parameterIdentification[[1]]$parameters)
  )
  expect_length(
    task$outputMappings,
    length(project$parameterIdentification[[1]]$outputMappings)
  )

  rt2 <- roundtrip(rt1)
  expect_equal(
    toJson(rt2$parameterIdentification),
    toJson(rt1$parameterIdentification)
  )
})

test_that("Excel scenario round-trip matches the canonical JSON serializer", {
  work_dir <- withr::local_tempdir()
  file.copy(dirname(exampleProjectPath()), work_dir, recursive = TRUE)
  project <- loadProject(file.path(work_dir, "Example", "Project.json"))

  # The Example exercises the three scenario drift cases: a Population
  # scenario (populationId must survive verbatim), a steady-state scenario
  # with a declared unit, and Individual scenarios carrying a
  # steadyStateTime but no unit and simulateSteadyState = FALSE (the
  # canonical serializer drops the time; a value-based guard would
  # resurrect it with a fabricated unit).
  excel_out <- withr::local_tempdir()
  exportProjectToExcel(project, outputDir = excel_out, silent = TRUE)
  reimported <- loadProject(importProjectFromExcel(
    file.path(excel_out, "Project.xlsx"),
    silent = TRUE
  ))

  toJson <- function(x) {
    jsonlite::toJSON(x, auto_unbox = TRUE, null = "null", digits = NA)
  }
  expect_equal(
    toJson(esqlabsR:::.scenariosToJson(reimported)),
    toJson(esqlabsR:::.scenariosToJson(project))
  )
})
