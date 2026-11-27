# convertProjectToV6() runs only on throwaway copies of the fixtures: it moves
# the workbooks of the folder it is given.

# Every file and folder under `dir`, with the content hash of each file, so a
# test can say the folder is exactly as it was.
.folderState <- function(dir) {
  paths <- sort(list.files(
    dir,
    recursive = TRUE,
    all.files = TRUE,
    include.dirs = TRUE
  ))
  full <- file.path(dir, paths)
  isDir <- dir.exists(full)
  hashes <- rep("<dir>", length(paths))
  hashes[!isDir] <- unname(tools::md5sum(full[!isDir]))
  stats::setNames(hashes, paths)
}

# Refusals ----

test_that("convertProjectToV6() refuses a path that is not an esqlabsR 5 workbook", {
  projectDir <- localExcelProjectDir()
  before <- .folderState(projectDir)

  expect_snapshot(
    convertProjectToV6(file.path(projectDir, "Missing.xlsx")),
    error = TRUE,
    transform = .redactTmpDir
  )
  expect_snapshot(
    convertProjectToV6(file.path(projectDir, "Data")),
    error = TRUE,
    transform = .redactTmpDir
  )
  expect_snapshot(
    convertProjectToV6(
      file.path(projectDir, "Data", "esqlabs_dataImporter_configuration.xml")
    ),
    error = TRUE,
    transform = .redactTmpDir
  )

  # A data workbook has sheets but no property table.
  expect_snapshot(
    convertProjectToV6(
      file.path(projectDir, "Data", "TestProject_TimeValuesData.xlsx")
    ),
    error = TRUE,
    transform = .redactTmpDir
  )

  expect_identical(.folderState(projectDir), before)
})

test_that("convertProjectToV6() refuses a workbook that is already in the v6 layout", {
  project <- testProject()
  workbook <- exportProjectToExcel(project, silent = TRUE)
  file.remove(project$info$projectFilePath)
  unlink(
    file.path(project$info$projectDirPath, "definitions"),
    recursive = TRUE
  )
  before <- .folderState(project$info$projectDirPath)

  expect_snapshot(
    convertProjectToV6(workbook),
    error = TRUE,
    transform = .redactTmpDir
  )
  expect_identical(.folderState(project$info$projectDirPath), before)
})

test_that("convertProjectToV6() refuses a folder that holds a v6 project or an earlier conversion", {
  configPath <- function(projectDir) {
    file.path(projectDir, "ProjectConfiguration.xlsx")
  }

  projectDir <- localExcelProjectDir()
  writeLines("{}", file.path(projectDir, "Project.json"))
  dir.create(
    file.path(projectDir, "definitions", "scenarios"),
    recursive = TRUE
  )
  writeLines("{}", file.path(projectDir, "definitions", "scenarios", "a.json"))
  before <- .folderState(projectDir)
  expect_snapshot(
    convertProjectToV6(configPath(projectDir)),
    error = TRUE,
    transform = .redactTmpDir
  )
  expect_identical(.folderState(projectDir), before)

  projectDir <- localExcelProjectDir()
  dir.create(file.path(projectDir, "esqlabsR5"))
  file.copy(configPath(projectDir), file.path(projectDir, "Project.xlsx"))
  before <- .folderState(projectDir)
  expect_snapshot(
    convertProjectToV6(configPath(projectDir)),
    error = TRUE,
    transform = .redactTmpDir
  )
  expect_identical(.folderState(projectDir), before)
})

# Moving, writing, importing, restoring ----

# Convert a copied esqlabsR 5 project in place and load the result.
#
# Returns `project`, the `projectDir` it was converted in, and the messages of
# every warning the conversion and the load raised, collected rather than
# suppressed, as `importLegacyExcelProject()` does, so a test can say which
# warnings are about its subject.
convertExcelProject <- function(
  projectDir,
  configFile = "ProjectConfiguration.xlsx",
  silent = TRUE
) {
  warnings <- character()
  collect <- function(w) {
    warnings <<- c(warnings, conditionMessage(w))
    invokeRestart("muffleWarning")
  }
  jsonPath <- withCallingHandlers(
    convertProjectToV6(file.path(projectDir, configFile), silent = silent),
    warning = collect
  )
  project <- withCallingHandlers(loadProject(jsonPath), warning = collect)
  list(project = project, projectDir = projectDir, warnings = warnings)
}

# The part of a folder state under one subfolder.
.subfolderState <- function(state, subfolder) {
  state[startsWith(names(state), paste0(subfolder, "/"))]
}

test_that("convertProjectToV6() moves the esqlabsR 5 workbooks and writes a project beside them", {
  projectDir <- localExcelProjectDir()
  before <- .folderState(projectDir)

  expect_invisible(
    jsonPath <- suppressWarnings(convertProjectToV6(
      file.path(projectDir, "ProjectConfiguration.xlsx"),
      silent = TRUE
    ))
  )
  expect_identical(
    normalizePath(jsonPath),
    normalizePath(file.path(projectDir, "Project.json"))
  )
  after <- .folderState(projectDir)

  # The property workbook and every workbook the project names moved, unchanged.
  moved <- c(
    "ProjectConfiguration.xlsx",
    file.path(
      "Configurations",
      c(
        "Applications.xlsx",
        "Individuals.xlsx",
        "ModelParameters.xlsx",
        "ParameterIdentification.xlsx",
        "Plots.xlsx",
        "Populations.xlsx",
        "Scenarios.xlsx"
      )
    )
  )
  expect_setequal(
    names(.subfolderState(after, "esqlabsR5")),
    c("esqlabsR5/Configurations", file.path("esqlabsR5", moved))
  )
  expect_identical(
    unname(after[file.path("esqlabsR5", moved)]),
    unname(before[moved])
  )

  # Models, data and population files stay where they were.
  for (subfolder in c("Models", "Data", "Configurations/PopulationsCSV")) {
    expect_identical(
      .subfolderState(after, subfolder),
      .subfolderState(before, subfolder)
    )
  }
  expect_true(file.exists(file.path(projectDir, "Project.xlsx")))
  expect_true(dir.exists(file.path(projectDir, "definitions", "scenarios")))
})

test_that("convertProjectToV6() puts the folder back when the import fails", {
  projectDir <- localLegacyExcelProject()
  # A space and a comma both canonicalize to `_`, so the import refuses these
  # two scenario names.
  editWorkbookSheets(
    file.path(projectDir, "Configurations", "Scenarios.xlsx"),
    function(sheets) {
      sheets$Scenarios$Scenario_name[1:2] <- c(
        "Adult Scenario",
        "Adult,Scenario"
      )
      sheets
    }
  )
  before <- .folderState(projectDir)

  expect_error(
    suppressWarnings(convertProjectToV6(
      legacyExcelProjectPath(projectDir),
      silent = TRUE
    )),
    "collide after canonicalization"
  )
  expect_identical(.folderState(projectDir), before)
})

test_that("convertProjectToV6() puts the folder back when a workbook cannot be written", {
  projectDir <- localLegacyExcelProject()
  before <- .folderState(projectDir)

  # The disk fills up while the first v6 workbook is written, after every
  # esqlabsR 5 workbook has moved, and leaves a partial file behind.
  local_mocked_bindings(.writeExcel = function(data, path, ...) {
    writeLines("partial", path)
    stop("The disk is full.")
  })

  expect_error(
    suppressWarnings(convertProjectToV6(
      legacyExcelProjectPath(projectDir),
      silent = TRUE
    )),
    "The disk is full."
  )
  expect_identical(.folderState(projectDir), before)
})

test_that("convertProjectToV6() puts the folder back when it is interrupted", {
  projectDir <- localLegacyExcelProject()
  before <- .folderState(projectDir)

  local_mocked_bindings(importProjectFromExcel = function(...) {
    rlang::interrupt()
  })

  result <- tryCatch(
    convertProjectToV6(legacyExcelProjectPath(projectDir), silent = TRUE),
    interrupt = function(cnd) "interrupted"
  )
  expect_identical(result, "interrupted")
  expect_identical(.folderState(projectDir), before)
})
