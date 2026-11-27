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

# Set one property of a copied project's esqlabsR 5 property sheet.
.setProperty <- function(projectDir, property, value) {
  configPath <- file.path(projectDir, "ProjectConfiguration.xlsx")
  properties <- readExcel(configPath)
  properties$Value[properties$Property == property] <- value
  .writeExcel(properties, configPath)
  invisible(configPath)
}

# The `filePaths` block of a converted project's `Project.json`.
.convertedFilePaths <- function(projectDir) {
  jsonlite::read_json(file.path(projectDir, "Project.json"))$filePaths
}

# Refusals ----

test_that("convertProjectToV6() takes one path and one TRUE or FALSE", {
  expect_snapshot(
    {
      convertProjectToV6(c("A.xlsx", "B.xlsx"))
      convertProjectToV6(NA_character_)
      convertProjectToV6("A.xlsx", silent = c(TRUE, FALSE))
      convertProjectToV6("A.xlsx", silent = NA)
    },
    error = TRUE
  )
})

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

  # A file named like a workbook that is none.
  writeLines("not a workbook", file.path(projectDir, "Broken.xlsx"))
  # Not a snapshot: the chained `readxl` error spells the path differently on
  # each platform.
  expect_error(
    convertProjectToV6(file.path(projectDir, "Broken.xlsx")),
    "cannot be read as an Excel workbook"
  )
  file.remove(file.path(projectDir, "Broken.xlsx"))

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
convertExcelProject <- function(projectDir, silent = TRUE) {
  warnings <- character()
  collect <- function(w) {
    warnings <<- c(warnings, conditionMessage(w))
    invokeRestart("muffleWarning")
  }
  jsonPath <- withCallingHandlers(
    convertProjectToV6(
      file.path(projectDir, "ProjectConfiguration.xlsx"),
      silent = silent
    ),
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

  # The property workbook, the esqlabsR 5 snapshot JSON beside it, and every
  # workbook the project names moved, unchanged.
  moved <- c(
    "ProjectConfiguration.xlsx",
    "ProjectConfiguration.json",
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

  expect_snapshot(
    convertProjectToV6(legacyExcelProjectPath(projectDir), silent = TRUE),
    error = TRUE,
    transform = .redactTmpDir
  )
  expect_identical(.folderState(projectDir), before)
})

test_that("a sheet the import cannot read stops the conversion, and the folder is put back", {
  projectDir <- localLegacyExcelProject()
  editWorkbookSheets(
    file.path(projectDir, "Configurations", "Scenarios.xlsx"),
    function(sheets) {
      sheets$Scenarios$Scenario_name <- NULL
      sheets
    }
  )
  before <- .folderState(projectDir)

  expect_error(
    suppressWarnings(convertProjectToV6(
      legacyExcelProjectPath(projectDir),
      silent = TRUE
    )),
    "Scenario_name"
  )
  expect_identical(.folderState(projectDir), before)
})

test_that("the restore keeps what a definitions folder held before the conversion", {
  projectDir <- localLegacyExcelProject()
  dir.create(file.path(projectDir, "definitions"))
  writeLines("notes", file.path(projectDir, "definitions", "README.md"))
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

test_that("a file the restore cannot move back is named, and kept in esqlabsR5/", {
  projectDir <- localLegacyExcelProject()
  local_mocked_bindings(
    file_move = function(path, new_path) {
      if (grepl("esqlabsR5", path, fixed = TRUE)) {
        stop("The file is locked.")
      }
      file.rename(path, new_path)
      invisible(new_path)
    },
    .package = "fs"
  )
  local_mocked_bindings(importProjectFromExcel = function(...) {
    stop("The import failed.")
  })

  warnings <- character()
  expect_error(
    withCallingHandlers(
      convertProjectToV6(legacyExcelProjectPath(projectDir), silent = TRUE),
      warning = function(w) {
        warnings <<- c(warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    ),
    "The import failed."
  )
  expect_snapshot(cat(warnings, sep = "\n"))
  expect_true(file.exists(file.path(
    projectDir,
    "esqlabsR5",
    "ProjectConfiguration.xlsx"
  )))
  expect_false(file.exists(file.path(projectDir, "Project.xlsx")))
})

test_that("convertProjectToV6() puts the folder back when a workbook cannot be written", {
  projectDir <- localLegacyExcelProject()
  before <- .folderState(projectDir)

  # The disk fills up while the third v6 workbook is written, after every
  # esqlabsR 5 workbook has moved and two v6 workbooks were written, and leaves
  # a partial file behind.
  writes <- 0L
  local_mocked_bindings(.writeExcel = function(data, path, ...) {
    writes <<- writes + 1L
    if (writes == 3L) {
      writeLines("partial", path)
      stop("The disk is full.")
    }
    writexl::write_xlsx(data, path)
  })

  expect_error(
    suppressWarnings(convertProjectToV6(
      legacyExcelProjectPath(projectDir),
      silent = TRUE
    )),
    "The disk is full."
  )
  expect_identical(writes, 3L)
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

# The v6 workbooks ----

# One line per sheet of each workbook the conversion wrote: the workbook, the
# sheet, its row count and its columns.
.writtenWorkbookLayout <- function(projectDir) {
  files <- c(
    "Project.xlsx",
    file.path(
      "Configurations",
      list.files(file.path(projectDir, "Configurations"), pattern = "\\.xlsx$")
    )
  )
  unlist(lapply(files, function(file) {
    path <- file.path(projectDir, file)
    vapply(
      readxl::excel_sheets(path),
      function(sheet) {
        df <- readExcel(path, sheet = sheet)
        sprintf(
          "%s [%s] %d rows: %s",
          file,
          sheet,
          nrow(df),
          paste(names(df), collapse = " | ")
        )
      },
      character(1),
      USE.NAMES = FALSE
    )
  }))
}

# The property table of a converted project's `Project.xlsx`, with the
# esqlabsR version a snapshot cannot pin.
.convertedProperties <- function(projectDir) {
  table <- as.data.frame(readExcel(file.path(projectDir, "Project.xlsx")))
  table$Value[table$Property == "esqlabsRVersion"] <- "<version>"
  table[, c("Property", "Value")]
}

test_that("convertProjectToV6() writes the v6 workbooks", {
  projectDir <- localExcelProjectDir()
  suppressWarnings(convertProjectToV6(
    file.path(projectDir, "ProjectConfiguration.xlsx"),
    silent = TRUE
  ))

  expect_snapshot(cat(.writtenWorkbookLayout(projectDir), sep = "\n"))
})

test_that("convertProjectToV6() writes the v6 property sheet", {
  projectDir <- localLegacyExcelProject()
  # A workbook the esqlabsR 5 property sheet does not name is read under its
  # usual name, and the v6 property sheet names it.
  .writeExcel(
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
    file.path(projectDir, "Configurations", "InitialConditions.xlsx")
  )
  converted <- convertExcelProject(projectDir)

  expect_snapshot(print(.convertedProperties(projectDir)))
  expect_true(dir.exists(converted$project$paths$simulationsFolder))
  # The workbook is in the v6 layout already, so it is carried over unchanged.
  expect_length(converted$project$definitions$initialConditions, 1L)
  expect_identical(
    unname(tools::md5sum(file.path(
      projectDir,
      "Configurations",
      "InitialConditions.xlsx"
    ))),
    unname(tools::md5sum(file.path(
      projectDir,
      "esqlabsR5",
      "Configurations",
      "InitialConditions.xlsx"
    )))
  )
})

test_that("convertProjectToV6() carries a DefaultSolverSettings sheet over", {
  projectDir <- localLegacyExcelProject()
  configPath <- legacyExcelProjectPath(projectDir)
  .writeExcel(
    list(
      Sheet1 = readExcel(configPath),
      DefaultSolverSettings = data.frame(
        Setting = "relTol",
        Value = "1e-6",
        Description = NA_character_
      )
    ),
    configPath
  )

  converted <- convertExcelProject(projectDir)

  expect_identical(
    converted$project$defaultSolverSettings,
    list(relTol = 1e-6)
  )
})

test_that("a converted project is in sync with the v6 workbooks", {
  converted <- convertExcelProject(localExcelProjectDir())

  status <- suppressMessages(projectStatus(converted$project))

  expect_true(status$excel_in_sync)
})

test_that("convertProjectToV6() names the workbooks it writes, and only those", {
  projectDir <- localLegacyExcelProject()
  configDir <- file.path(projectDir, "Configurations")
  # No model-parameters workbook, and an individuals workbook holding only
  # parameter sheets: the sets go to a new model-parameters workbook, and no
  # individuals workbook is left to write.
  file.remove(file.path(configDir, "ModelParameters.xlsx"))
  editWorkbookSheets(
    file.path(configDir, "Individuals.xlsx"),
    function(sheets) sheets[c("Adult", "Child")]
  )
  configPath <- legacyExcelProjectPath(projectDir)
  properties <- readExcel(configPath)
  .writeExcel(
    properties[properties$Property != "modelParamsFile", ],
    configPath
  )

  suppressWarnings(convertProjectToV6(configPath, silent = TRUE))

  written <- .convertedProperties(projectDir)
  expect_identical(
    written$Value[written$Property == "modelParamsFile"],
    "ModelParameters.xlsx"
  )
  expect_false("individualsFile" %in% written$Property)
  expect_false(file.exists(file.path(configDir, "Individuals.xlsx")))
  expect_identical(
    readxl::excel_sheets(file.path(configDir, "ModelParameters.xlsx")),
    c("Adult", "Child", "IV_250mg")
  )
})

test_that("convertProjectToV6() refuses a workbook linked from outside the project folder", {
  projectDir <- localExcelProjectDir()
  shared <- file.path(withr::local_tempdir(), "SharedConfigs")
  file.rename(file.path(projectDir, "Configurations"), shared)
  linked <- file.symlink(shared, file.path(projectDir, "Configurations"))
  skip_if_not(linked, "Symbolic links cannot be created here.")
  before <- .folderState(projectDir)
  sharedBefore <- .folderState(shared)

  expect_snapshot(
    convertProjectToV6(file.path(projectDir, "ProjectConfiguration.xlsx")),
    error = TRUE,
    transform = .redactTmpDir
  )
  expect_identical(.folderState(projectDir), before)
  expect_identical(.folderState(shared), sharedBefore)

  # The property workbook itself can be the link.
  projectDir <- localExcelProjectDir()
  elsewhere <- file.path(withr::local_tempdir(), "ProjectConfiguration.xlsx")
  configPath <- file.path(projectDir, "ProjectConfiguration.xlsx")
  file.rename(configPath, elsewhere)
  file.symlink(elsewhere, configPath)
  expect_error(
    convertProjectToV6(configPath),
    "outside the project folder once links are followed"
  )
  expect_true(file.exists(elsewhere))
})

test_that("convertProjectToV6() refuses a workbook file name outside the configurations folder", {
  projectDir <- localExcelProjectDir()
  configPath <- file.path(projectDir, "ProjectConfiguration.xlsx")
  properties <- readExcel(configPath)
  properties$Value[properties$Property == "plotsFile"] <- "../Plots.xlsx"
  .writeExcel(properties, configPath)
  before <- .folderState(projectDir)

  expect_snapshot(
    convertProjectToV6(configPath),
    error = TRUE,
    transform = .redactTmpDir
  )
  expect_identical(.folderState(projectDir), before)
})

test_that("convertProjectToV6() refuses a project whose workbooks it cannot find", {
  withr::local_envvar(MYCONF = "Configurations")
  for (configurationsFolder in c("Missing", "MYCONF", NA)) {
    projectDir <- localExcelProjectDir()
    .setProperty(projectDir, "configurationsFolder", configurationsFolder)
    before <- .folderState(projectDir)

    expect_snapshot(
      convertProjectToV6(file.path(projectDir, "ProjectConfiguration.xlsx")),
      error = TRUE,
      transform = .redactTmpDir
    )
    expect_identical(.folderState(projectDir), before)
  }
})

test_that("convertProjectToV6() refuses the esqlabsR 5 workbooks a conversion kept", {
  projectDir <- localLegacyExcelProject()
  convertExcelProject(projectDir)
  before <- .folderState(projectDir)

  expect_snapshot(
    convertProjectToV6(
      file.path(projectDir, "esqlabsR5", "ProjectConfiguration.xlsx")
    ),
    error = TRUE,
    transform = .redactTmpDir
  )
  expect_identical(.folderState(projectDir), before)
})

test_that("convertProjectToV6() refuses a folder outside the project folder", {
  shared <- withr::local_tempdir()
  cases <- list(
    dataFolder = shared,
    dataFolder = "../elsewhere",
    modelFolder = "../Models/Simulations",
    outputFolder = "Results/../../Results",
    configurationsFolder = "../Configurations",
    configurationsFolder = "${ESQLABSR_TEST_CONFIGURATIONS}"
  )
  for (i in seq_along(cases)) {
    projectDir <- localExcelProjectDir()
    configPath <- file.path(projectDir, "ProjectConfiguration.xlsx")
    properties <- readExcel(configPath)
    properties$Value[properties$Property == names(cases)[[i]]] <- cases[[i]]
    .writeExcel(properties, configPath)
    before <- .folderState(projectDir)

    expect_snapshot(
      convertProjectToV6(configPath),
      error = TRUE,
      transform = .redactTmpDir
    )
    expect_identical(.folderState(projectDir), before)
  }
})

# Environment variables in folder values (#1411) ----

test_that("convertProjectToV6() writes a folder on a drive named by a variable as ${NAME}", {
  # The data sits on a shared drive that esqlabsR 5 reached through SHARED.
  shared <- withr::local_tempdir()
  projectDir <- localExcelProjectDir()
  file.rename(file.path(projectDir, "Data"), file.path(shared, "Data"))
  withr::local_envvar(SHARED = normalizePath(shared, "/"))
  .setProperty(projectDir, "dataFolder", "SHARED/Data")

  converted <- convertExcelProject(projectDir)

  expect_identical(.convertedFilePaths(projectDir)$dataFolder, "${SHARED}/Data")
  expect_identical(
    list.files(file.path(projectDir, "definitions", "observed-data")),
    "testproject_timevaluesdata.json"
  )
  expect_false(any(grepl("not found", converted$warnings, fixed = TRUE)))
})

test_that("convertProjectToV6() writes a variable naming a folder of the project as ${NAME}", {
  projectDir <- localExcelProjectDir()
  # Not `MODELS`: on Windows a variable name ignores case, so it would match the
  # fixture's own `Models` folder.
  withr::local_envvar(
    OBSDATA = "Data",
    MDL = "Models",
    # esqlabsR 5 reads the populations folder inside the configurations
    # folder.
    POPCSV = "PopulationsCSV",
    OUT = "Results"
  )
  .setProperty(projectDir, "dataFolder", "OBSDATA")
  .setProperty(projectDir, "modelFolder", "MDL/Simulations/")
  .setProperty(projectDir, "populationsFolder", "POPCSV")
  .setProperty(projectDir, "outputFolder", "OUT/")

  converted <- convertExcelProject(projectDir)

  expect_identical(
    .convertedFilePaths(projectDir)[c(
      "dataFolder",
      "simulationsFolder",
      "populationsFolder",
      "outputFolder"
    )],
    list(
      dataFolder = "${OBSDATA}",
      simulationsFolder = "${MDL}/Simulations/",
      populationsFolder = "Configurations/${POPCSV}",
      outputFolder = "${OUT}/"
    )
  )
  paths <- converted$project$paths
  expect_true(dir.exists(paths$simulationsFolder))
  expect_true(file.exists(file.path(
    paths$populationsFolder,
    "TestPopulation.csv"
  )))
  expect_length(converted$project$definitions$observedData, 1L)
})

test_that("convertProjectToV6() keeps a part of a folder value that holds 'path'", {
  projectDir <- localExcelProjectDir()
  dir.create(file.path(projectDir, "DataPath"))
  file.copy(
    list.files(file.path(projectDir, "Data"), full.names = TRUE),
    file.path(projectDir, "DataPath")
  )
  withr::local_envvar(DataPath = "Elsewhere")
  .setProperty(projectDir, "dataFolder", "DataPath")

  converted <- convertExcelProject(projectDir)

  expect_identical(.convertedFilePaths(projectDir)$dataFolder, "DataPath")
  expect_length(converted$project$definitions$observedData, 1L)
})

test_that("the conversion summary names each folder value it rewrote", {
  projectDir <- localExcelProjectDir()
  withr::local_envvar(OBSDATA = "Data", MDL = "Models")
  .setProperty(projectDir, "dataFolder", "OBSDATA")
  .setProperty(projectDir, "modelFolder", "MDL/Simulations/")

  summary <- character()
  suppressWarnings(withCallingHandlers(
    convertProjectToV6(file.path(projectDir, "ProjectConfiguration.xlsx")),
    message = function(m) {
      if (inherits(m, "esqlabsR_conversionSummary")) {
        summary <<- c(summary, conditionMessage(m))
      }
      invokeRestart("muffleMessage")
    }
  ))

  expect_snapshot(cat(summary, sep = "\n"), transform = .redactTmpDir)
})

# Parameter sets ----

# A one-row parameter sheet, for a test to add to a workbook.
.parameterSheet <- function(parameterName = "Volume", value = 1) {
  data.frame(
    `Container Path` = "Organism|Liver",
    `Parameter Name` = parameterName,
    Value = value,
    Units = "l",
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

test_that("convertProjectToV6() moves every parameter set to the model-parameters workbook", {
  projectDir <- localLegacyExcelProject()
  converted <- convertExcelProject(projectDir)
  configDir <- file.path(projectDir, "Configurations")

  expect_identical(
    readxl::excel_sheets(file.path(configDir, "ModelParameters.xlsx")),
    c("Global", "Aciclovir", "Sheet, with comma", "Adult", "Child", "IV_250mg")
  )
  protocols <- readExcel(
    file.path(configDir, "Applications.xlsx"),
    sheet = "ApplicationProtocols"
  )
  expect_identical(protocols$ApplicationId, "IV_250mg")
  expect_identical(protocols$ParameterSets, "IV_250mg")
  biometrics <- readExcel(
    file.path(configDir, "Individuals.xlsx"),
    sheet = "IndividualBiometrics"
  )
  expect_identical(
    readxl::excel_sheets(file.path(configDir, "Individuals.xlsx")),
    "IndividualBiometrics"
  )
  expect_identical(biometrics$ParameterSets, c("Adult", "Child"))
  expect_false("Individual Parameter Sets" %in% names(biometrics))

  expect_identical(
    unlist(converted$project$definitions$individuals[["child"]]$parameterSets),
    "child"
  )
  expect_identical(converted$warnings, character())
})

test_that("a renamed parameter set keeps a name Excel can hold", {
  projectDir <- localLegacyExcelProject()
  # As long as an Excel sheet name can be.
  longName <- "Adult_with_a_long_name_31_chars"
  expect_identical(nchar(longName), 31L)
  editWorkbookSheets(
    file.path(projectDir, "Configurations", "ModelParameters.xlsx"),
    function(sheets) {
      c(sheets, stats::setNames(list(.parameterSheet()), longName))
    }
  )
  editWorkbookSheets(
    file.path(projectDir, "Configurations", "Individuals.xlsx"),
    function(sheets) {
      sheets[[longName]] <- .parameterSheet("Weight", 2)
      sheets$IndividualBiometrics$IndividualId[[1]] <- longName
      sheets
    }
  )

  converted <- suppressWarnings(convertExcelProject(projectDir))

  renamed <- "Adult_with_a_long_name_31_cha_2"
  expect_identical(nchar(renamed), 31L)
  expect_contains(
    readxl::excel_sheets(
      file.path(projectDir, "Configurations", "ModelParameters.xlsx")
    ),
    c(longName, renamed)
  )
  individual <- converted$project$definitions$individuals[[tolower(longName)]]
  expect_identical(unlist(individual$parameterSets), tolower(renamed))
})

test_that("an Individual Parameter Sets cell naming another set is reported", {
  projectDir <- localLegacyExcelProject()
  editWorkbookSheets(
    file.path(projectDir, "Configurations", "Individuals.xlsx"),
    function(sheets) {
      sheets$IndividualBiometrics$`Individual Parameter Sets` <- c(
        "Adult, Global",
        "Child"
      )
      sheets
    }
  )

  warning <- expect_warning(
    convertProjectToV6(legacyExcelProjectPath(projectDir), silent = TRUE),
    class = "esqlabsR_convertIndividualParameterSets"
  )
  expect_snapshot(cat(conditionMessage(warning)), transform = .redactTmpDir)

  project <- suppressWarnings(loadProject(file.path(
    projectDir,
    "Project.json"
  )))
  expect_identical(
    unlist(project$definitions$individuals[["adult"]]$parameterSets),
    "adult"
  )
})

# Individuals and populations ----

test_that("convertProjectToV6() writes a Protein + Ontogeny pair as one Protein Ontogenies column", {
  projectDir <- localLegacyExcelProject()
  convertExcelProject(projectDir)
  configDir <- file.path(projectDir, "Configurations")

  biometrics <- readExcel(
    file.path(configDir, "Individuals.xlsx"),
    sheet = "IndividualBiometrics"
  )
  expect_identical(
    biometrics$`Protein Ontogenies`,
    c("CYP3A4:CYP3A4", "CYP3A4:CYP3A4,CYP2D6:CYP2C8")
  )
  expect_false(any(c("Protein", "Ontogeny") %in% names(biometrics)))
  populations <- readExcel(file.path(configDir, "Populations.xlsx"))
  expect_identical(
    populations$`Protein Ontogenies`,
    c("CYP3A4:CYP3A4,CYP2D6:CYP2C8", NA)
  )
  expect_false(any(c("Protein", "Ontogeny") %in% names(populations)))
})

test_that("a filled UserDefinedVariability sheet is reported, an empty one is not", {
  projectDir <- localLegacyExcelProject()
  expect_false(any(grepl(
    "UserDefinedVariability",
    convertExcelProject(projectDir)$warnings,
    fixed = TRUE
  )))

  projectDir <- localLegacyExcelProject()
  editWorkbookSheets(
    file.path(projectDir, "Configurations", "Populations.xlsx"),
    function(sheets) {
      sheets$UserDefinedVariability <- data.frame(
        `Container Path` = "Organism|Liver",
        `Parameter Name` = "Volume",
        Mean = 1.5,
        SD = 0.2,
        Distribution = "Normal",
        check.names = FALSE
      )
      sheets
    }
  )
  warning <- expect_warning(
    convertProjectToV6(legacyExcelProjectPath(projectDir), silent = TRUE),
    class = "esqlabsR_convertUserDefinedVariability"
  )
  expect_snapshot(cat(conditionMessage(warning)), transform = .redactTmpDir)
})

# Scenarios ----

test_that("convertProjectToV6() writes scenario booleans as TRUE / FALSE and keeps a changed name as the label", {
  projectDir <- localLegacyExcelProject()
  editWorkbookSheets(
    file.path(projectDir, "Configurations", "Scenarios.xlsx"),
    function(sheets) {
      scenarios <- sheets$Scenarios
      scenarios$Scenario_name[scenarios$Scenario_name == "PIScenario2"] <-
        "PI Scenario 2"
      scenarios$SteadyState <- c("Yes", "no", "1", "0", NA, "TRUE")
      sheets$Scenarios <- scenarios
      sheets
    }
  )
  convertExcelProject(projectDir)

  scenarios <- readExcel(
    file.path(projectDir, "Configurations", "Scenarios.xlsx"),
    sheet = "Scenarios"
  )
  expect_identical(names(scenarios)[1:2], c("Scenario_name", "Label"))
  expect_identical(
    scenarios$Label,
    ifelse(scenarios$Scenario_name == "PI Scenario 2", "PI Scenario 2", NA)
  )
  expect_identical(scenarios$SteadyState, c(TRUE, FALSE, TRUE, FALSE, NA, TRUE))
  expect_type(scenarios$ReadPopulationFromCSV, "logical")
})

test_that("convertProjectToV6() stops on a scenario boolean it cannot read, before changing the folder", {
  projectDir <- localLegacyExcelProject()
  editWorkbookSheets(
    file.path(projectDir, "Configurations", "Scenarios.xlsx"),
    function(sheets) {
      sheets$Scenarios$SteadyState[[1]] <- "maybe"
      sheets
    }
  )
  before <- .folderState(projectDir)

  expect_snapshot(
    convertProjectToV6(legacyExcelProjectPath(projectDir)),
    error = TRUE,
    transform = .redactTmpDir
  )
  expect_identical(.folderState(projectDir), before)
})

# Plots ----

test_that("convertProjectToV6() writes the plot sheets with the v6 columns", {
  plots <- data.frame(
    plotID = c("P1", "P2", "P3"),
    DataCombinedName = c("AciclovirPVB", NA, "AciclovirPVB"),
    dataCombined = c(NA, "AciclovirPop", "Ignored"),
    plotType = "individual",
    xAxisScale = c("lin", "log", NA),
    displayLLOQ = c("no", "Yes", NA),
    stringsAsFactors = FALSE
  )
  expect_identical(
    .v5ConvertPlotSheet(plots),
    data.frame(
      plotId = c("P1", "P2", "P3"),
      dataCombined = c("AciclovirPVB", "AciclovirPop", "AciclovirPVB"),
      plotType = "individual",
      xAxisScale = c("linear", "log", NA),
      displayLLOQ = c(FALSE, TRUE, NA),
      stringsAsFactors = FALSE
    )
  )

  grids <- data.frame(
    name = c("Aciclovir", "Aciclovir2"),
    plotIDs = c("\"P1\", \"P2\"", "P2"),
    title = c("Aciclovir", NA),
    stringsAsFactors = FALSE
  )
  expect_identical(
    .v5ConvertPlotGridSheet(grids),
    data.frame(
      plotGridId = c("Aciclovir", "Aciclovir2"),
      plots = c("\"P1\", \"P2\"", "P2"),
      title = c("Aciclovir", NA),
      stringsAsFactors = FALSE
    )
  )
})

test_that("a true-or-false plot setting that is no boolean spelling is kept for validation", {
  expect_identical(
    .v5PlotLogicalColumn(c("no", "sometimes", NA), "displayLLOQ"),
    c("FALSE", "sometimes", NA)
  )
})

# Parameter identification ----

test_that("convertProjectToV6() writes the parameter-identification sheets an export writes", {
  .localSnapshotOptions()
  projectDir <- localLegacyExcelProject()
  converted <- convertExcelProject(projectDir)
  piFile <- file.path(
    projectDir,
    "Configurations",
    "ParameterIdentification.xlsx"
  )

  expect_snapshot(
    for (sheet in readxl::excel_sheets(piFile)) {
      cat(sheet, "\n")
      print(as.data.frame(readExcel(piFile, sheet = sheet)))
    }
  )

  # The export of the converted project writes the same task back.
  exported <- .parameterIdentificationToExcelSheets(.unwrapDefinitionList(
    converted$project$definitions$parameterIdentification
  ))
  expect_identical(
    exported$PIParameters$id,
    readExcel(piFile, sheet = "PIParameters")$id
  )
})

test_that("an ObservedDataSheet the data workbook lacks is reported", {
  projectDir <- localLegacyExcelProject()
  editWorkbookSheets(
    file.path(projectDir, "Configurations", "ParameterIdentification.xlsx"),
    function(sheets) {
      sheets$PIOutputMappings$ObservedDataSheet <- "Laskin 1982.Group B"
      sheets
    }
  )

  warning <- expect_warning(
    convertProjectToV6(legacyExcelProjectPath(projectDir), silent = TRUE),
    class = "esqlabsR_importUnimportedPIObservedDataSheets"
  )
  expect_snapshot(cat(conditionMessage(warning)))
})

test_that("a missing data workbook leaves the ObservedDataSheet check to the import", {
  projectDir <- localLegacyExcelProject()
  unlink(file.path(projectDir, "Data"), recursive = TRUE)

  converted <- convertExcelProject(projectDir)

  expect_false(any(grepl("ObservedDataSheet", converted$warnings)))
  expect_true(any(grepl("was not found", converted$warnings)))
})

# esqlabsR 5 readers, unit by unit ----
#
# Copies of the esqlabsR 5 readers the import used to own; each block pins the
# behavior of the reader the conversion now uses.

test_that(".v5UniqueImportedId falls back instead of aborting on an empty cell", {
  expect_identical(.v5UniqueImportedId(character(0), character()), "item")
  expect_identical(.v5UniqueImportedId(NULL, character()), "item")
  expect_identical(.v5UniqueImportedId(NA, character()), "item")
  expect_identical(.v5UniqueImportedId("   ", character()), "item")
  # A usable cell is unaffected, and a clash still gets a suffix.
  expect_identical(.v5UniqueImportedId("Conc", character()), "conc")
  expect_identical(.v5UniqueImportedId("Conc", "conc"), "conc_2")
})

# The 5.x parameter-identification layout has no `PITasks` sheet; each sheet is
# keyed by a `PITaskName` column, with the configuration split across
# `PIConfiguration` / `AlgorithmOptions` / `CIOptions`. The converter reassembles
# each task's parameters, output mappings, and nested configuration into the
# shape a v6 task has (#1158).
test_that(".v5ParsePI parses the 5.x PITaskName layout", {
  piFile <- testthat::test_path(
    "data",
    "TestProjectExcel",
    "Configurations",
    "ParameterIdentification.xlsx"
  )
  tasks <- .v5ParsePI(
    .v5ReadSheets(piFile, readxl::excel_sheets(piFile)),
    list()
  )
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

  # The observed-data DataSet name is kept verbatim.
  mapping <- simple$outputMappings[[1]]
  expect_true(startsWith(mapping$observedData, "Laskin 1982.Group A"))
})

# An output mapping that names its output path by full OSPS path (not by id) is
# rewritten to the id of the matching `outputPaths` definition, so the reference
# resolves rather than dangling (#1158).
test_that(".v5ResolvePIOutputPathRefs rewrites a full-path mapping to the output-path id", {
  outputPaths <- c(Aciclovir_PVB = "Organism|PVB|Aciclovir|Plasma")
  tasks <- list(list(
    id = "t1",
    outputMappings = list(
      list(id = "m1", outputPath = "Organism|PVB|Aciclovir|Plasma"),
      list(id = "m2", outputPath = "already_an_id")
    )
  ))
  resolved <- .v5ResolvePIOutputPathRefs(tasks, outputPaths)
  expect_identical(
    resolved[[1]]$outputMappings[[1]]$outputPath,
    "Aciclovir_PVB"
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

test_that(".v5PIPath drops a blank parameter name instead of building a `|NA` path", {
  expect_null(.v5PIPath("Aciclovir", NA))
  expect_null(.v5PIPath("Aciclovir", ""))
  expect_identical(
    .v5PIPath("Aciclovir", "Lipophilicity"),
    "Aciclovir|Lipophilicity"
  )
  # A blank container with a real parameter still yields the parameter alone.
  expect_identical(.v5PIPath(NA, "Lipophilicity"), "Lipophilicity")
})

test_that(".v5PIPath tolerates an absent container or parameter cell", {
  expect_null(.v5PIPath(NULL, NULL))
  expect_null(.v5PIPath(character(0), character(0)))
  expect_null(.v5PIPath("Organism", NA))
  # A blank container yields the bare parameter name.
  expect_identical(.v5PIPath(NA, "Param"), "Param")
  expect_identical(.v5PIPath("   ", "Param"), "Param")
  expect_identical(.v5PIPath("Organism", "Param"), "Organism|Param")
})

test_that(".v5PIOptionRows coerces a boolean-string option but keeps numbers numeric", {
  df <- data.frame(
    PITaskName = c("t", "t", "t", "t"),
    OptionName = c("flag_true", "flag_false", "count", "method"),
    OptionValue = c("TRUE", "false", "5", "hessian"),
    stringsAsFactors = FALSE
  )
  o <- .v5PIOptionRows(df, "t")
  expect_identical(o$flag_true, TRUE)
  expect_identical(o$flag_false, FALSE)
  # A numeric value stays numeric (not misread as a boolean).
  expect_identical(o$count, 5)
  expect_identical(o$method, "hessian")
})

test_that(".v5PIOptionRows tolerates a sheet without the option columns", {
  noColumns <- data.frame(PITaskName = "t1", stringsAsFactors = FALSE)
  expect_identical(.v5PIOptionRows(noColumns, "t1"), list())

  blankName <- data.frame(
    PITaskName = c("t1", "t1"),
    OptionName = c("MaxIterations", NA),
    OptionValue = c("50", "ignored"),
    stringsAsFactors = FALSE
  )
  expect_identical(.v5PIOptionRows(blankName, "t1"), list(MaxIterations = 50))
})

# A mapping sheet whose `OutputPath` cell is blank parses: the mapping keeps no
# `outputPath`, so validation reports the incomplete mapping (#1190).
test_that(".v5ParsePIMappings parses a row with no output path", {
  blankCell <- data.frame(
    PITaskName = c("t1", "t1"),
    OutputPath = c("Organism|A|Conc", NA),
    Scenarios = c("s1", "s1"),
    DataSet = c("d1", "d2"),
    stringsAsFactors = FALSE
  )
  mappings <- .v5ParsePIMappings(blankCell, "t1", list())
  expect_length(mappings, 2L)
  expect_identical(mappings[[1]]$outputPath, "Organism|A|Conc")
  expect_null(mappings[[2]]$outputPath)
  # Still gets a usable, unique id.
  expect_identical(mappings[[2]]$id, "item")
})

# The sheet's oldest revision has no `OutputPath` column at all: it identified a
# mapping's output through the scenario, one mapping per output path the
# scenario declares (#1192).
test_that(".v5ParsePIMappings derives a missing OutputPath column", {
  noColumn <- data.frame(
    PITaskName = "t1",
    Scenarios = "s1, s2",
    DataSet = "d1",
    Scaling = "log",
    stringsAsFactors = FALSE
  )
  scenarios <- list(
    list(id = "s1", outputPaths = list("pvb", "urine")),
    list(id = "s2", outputPaths = list("pvb"))
  )

  mappings <- .v5ParsePIMappings(noColumn, "t1", scenarios)

  # One mapping per output path, not per scenario.
  expect_length(mappings, 2L)
  expect_identical(mappings[[1]]$outputPath, "pvb")
  expect_identical(unlist(mappings[[1]]$scenarios), c("s1", "s2"))
  expect_identical(mappings[[2]]$outputPath, "urine")
  expect_identical(unlist(mappings[[2]]$scenarios), "s1")
  expect_identical(mappings[[1]]$observedData, "d1")
  expect_identical(mappings[[1]]$scaling, "log")
})

test_that(".v5ParsePIMappings keeps and reports an underivable path", {
  noColumn <- data.frame(
    PITaskName = "t1",
    Scenarios = "s1",
    DataSet = "d1",
    stringsAsFactors = FALSE
  )

  expect_warning(
    mappings <- .v5ParsePIMappings(noColumn, "t1", list(list(id = "s1"))),
    class = "esqlabsR_importIncompletePIOutputMappings"
  )
  expect_length(mappings, 1L)
  expect_null(mappings[[1]]$outputPath)
  expect_identical(mappings[[1]]$observedData, "d1")

  warning <- expect_warning(
    expect_length(.v5ParsePIMappings(noColumn, "t1", NULL), 1L),
    class = "esqlabsR_importIncompletePIOutputMappings"
  )
  expect_snapshot(cat(conditionMessage(warning)))
})

test_that(".v5ParsePIMappings matches a scenario on its canonical id", {
  noColumn <- data.frame(
    PITaskName = "t1",
    Scenarios = "aciclovir iv",
    DataSet = "d1",
    stringsAsFactors = FALSE
  )
  scenarios <- list(list(id = "Aciclovir_IV", outputPaths = list("pvb")))

  mappings <- .v5ParsePIMappings(noColumn, "t1", scenarios)

  expect_length(mappings, 1L)
  expect_identical(mappings[[1]]$outputPath, "pvb")
})

test_that(".v5ParsePIMappings keeps a derived row with no DataSet", {
  noColumn <- data.frame(
    PITaskName = "t1",
    Scenarios = c("s1", "s1"),
    DataSet = c(NA, "d1"),
    stringsAsFactors = FALSE
  )
  scenarios <- list(list(id = "s1", outputPaths = list("pvb")))

  mappings <- .v5ParsePIMappings(noColumn, "t1", scenarios)

  expect_length(mappings, 2L)
  expect_null(mappings[[1]]$observedData)
  expect_identical(mappings[[1]]$outputPath, "pvb")
  expect_identical(mappings[[2]]$observedData, "d1")
})

# A `ParameterSets` cell names sheets of its own workbook. When one of those
# sheets is skipped it defines no set, so the reference has to go too, or the
# conversion writes a reference to a set that was never created (#1181).
test_that(".v5DropSkippedSheetRefs removes only references to skipped sheets", {
  expect_identical(
    .v5DropSkippedSheetRefs(c("Kept", "Skipped"), "Skipped"),
    "Kept"
  )
  # Nothing left: no reference remains.
  expect_length(.v5DropSkippedSheetRefs("Skipped", "Skipped"), 0L)
  expect_identical(.v5DropSkippedSheetRefs("Kept", "Skipped"), "Kept")
  expect_null(.v5DropSkippedSheetRefs(NULL, "Skipped"))
  # A reference matching on canonical id alone is still dropped.
  expect_length(.v5DropSkippedSheetRefs("skipped", "Skipped"), 0L)
})

test_that(".v5ScenarioLabels strips wrapping quotes before it compares and keeps", {
  expect_identical(
    .v5ScenarioLabels(
      c(
        "\"aciclovir_iv\"",
        "\"Aciclovir_IV\"",
        "\"Aciclovir IV\"",
        "Aciclovir IV"
      ),
      c(NA, NA, NA, "Aciclovir IV 250 mg")
    ),
    c(NA, NA, "Aciclovir IV", "Aciclovir IV 250 mg")
  )
})

# A legacy Scenarios sheet may spell booleans as `1`/`0` or `Yes`/`No`; the
# converter writes them as true/false, which the import reads.
test_that("the converted Scenarios sheet keeps the meaning of legacy boolean spellings", {
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
  scenarios <- .parseExcelScenarios(.v5ConvertScenariosSheet(scenarioDf))

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

# Regression (#1213): a real 5.x workbook spells the ontogenies as a `Protein` +
# `Ontogeny` column pair, not as the single `Protein Ontogenies` cell this
# fixture set uses. Reading only the single column returned nothing for every
# legacy workbook, so every ontogeny was discarded, and because two definitions
# differing only in their ontogenies then imported identically nothing in the
# output revealed the loss.

# One legacy-spelling row: the comma-separated `Protein` and `Ontogeny` cells
# pair up positionally.
.legacyOntogenyIndividual <- function(id, proteins, ontogenies) {
  dplyr::tibble(
    IndividualId = id,
    Species = "Human",
    Population = "European_ICRP_2002",
    Gender = "MALE",
    `Weight [kg]` = 73,
    `Height [cm]` = 176,
    `Age [year(s)]` = 30,
    Protein = proteins,
    Ontogeny = ontogenies
  )
}

.legacyOntogenyPopulation <- function(id, proteins, ontogenies) {
  dplyr::tibble(
    PopulationName = id,
    species = "Human",
    population = "European_ICRP_2002",
    numberOfIndividuals = 10,
    Protein = proteins,
    Ontogeny = ontogenies
  )
}

# The individuals and populations the import reads from a converted sheet.
.convertedIndividuals <- function(df) {
  .parseExcelIndividuals(.v5MergeOntogenyColumns(
    as.data.frame(df, check.names = FALSE),
    "individual",
    "IndividualId"
  ))
}

.convertedPopulations <- function(df) {
  .parseExcelPopulations(.v5MergeOntogenyColumns(
    as.data.frame(df, check.names = FALSE),
    "population",
    "PopulationName"
  ))
}

test_that("a legacy two-column Protein + Ontogeny sheet converts its ontogenies", {
  indiv <- .convertedIndividuals(.legacyOntogenyIndividual(
    "Indiv1",
    "CYP3A4,CYP2D6",
    "CYP3A4,CYP2C8"
  ))
  expect_identical(
    indiv[[1]]$proteinOntogenies,
    c("CYP3A4:CYP3A4", "CYP2D6:CYP2C8")
  )

  pop <- .convertedPopulations(.legacyOntogenyPopulation(
    "Pop1",
    "CYP3A4",
    "CYP3A4"
  ))
  expect_identical(pop[[1]]$proteinOntogenies, "CYP3A4:CYP3A4")

  # Whitespace around a value is workbook noise, not part of the protein name.
  spaced <- .convertedIndividuals(.legacyOntogenyIndividual(
    "Indiv2",
    "CYP3A4, CYP2D6",
    "CYP3A4 ,CYP2C8"
  ))
  expect_identical(
    spaced[[1]]$proteinOntogenies,
    c("CYP3A4:CYP3A4", "CYP2D6:CYP2C8")
  )
})

test_that("two populations differing only in their ontogenies do not convert identically", {
  populations <- .convertedPopulations(rbind(
    .legacyOntogenyPopulation("Pop1", "CYP3A4", "CYP3A4"),
    .legacyOntogenyPopulation("Pop2", "CYP2D6", "CYP2C8")
  ))
  expect_identical(populations[[1]]$proteinOntogenies, "CYP3A4:CYP3A4")
  expect_identical(populations[[2]]$proteinOntogenies, "CYP2D6:CYP2C8")
  expect_false(identical(
    populations[[1]]$proteinOntogenies,
    populations[[2]]$proteinOntogenies
  ))
})

test_that("the current single-column spelling wins over the pair columns", {
  both <- .legacyOntogenyIndividual("Indiv1", "CYP2D6", "CYP2C8")
  both[["Protein Ontogenies"]] <- "CYP3A4:CYP3A4"
  expect_identical(
    .convertedIndividuals(both)[[1]]$proteinOntogenies,
    "CYP3A4:CYP3A4"
  )
})

test_that("an unpairable ontogeny declaration warns instead of dropping it in silence", {
  # Each protein needs its own ontogeny: with an unmatched count the positional
  # pairing has no answer, and the value would otherwise leave the project with
  # no message at all.
  expect_snapshot(
    indiv <- .convertedIndividuals(.legacyOntogenyIndividual(
      "Indiv1",
      "CYP3A4,CYP2D6",
      "CYP3A4"
    ))
  )
  expect_null(indiv[[1]]$proteinOntogenies)

  # A protein named with no ontogeny at all is the same failure.
  expect_snapshot(
    pop <- .convertedPopulations(.legacyOntogenyPopulation(
      "Pop1",
      "CYP3A4",
      NA
    ))
  )
  expect_null(pop[[1]]$proteinOntogenies)

  # Neither column filled is not a declaration, so it stays quiet.
  expect_no_warning(
    quiet <- .convertedPopulations(.legacyOntogenyPopulation("Pop2", NA, NA))
  )
  expect_null(quiet[[1]]$proteinOntogenies)
})

test_that("a plot sheet with two id columns takes the v6 one and warns when they disagree", {
  plotsFile <- withr::local_tempfile(fileext = ".xlsx")
  writexl::write_xlsx(
    list(
      plotConfiguration = data.frame(
        plotID = c("p_old", "same"),
        plotId = c("p_new", "same"),
        DataCombinedName = c("dc", "dc"),
        plotType = c("individual", "individual")
      ),
      plotGrids = data.frame(
        name = "g_old",
        plotGridId = "g_new",
        plotIDs = "p_new"
      )
    ),
    plotsFile
  )

  expect_warning(
    expect_warning(
      converted <- .v5ConvertPlotsWorkbook(plotsFile),
      class = "esqlabsR_importConflictingPlotIdColumns"
    ),
    class = "esqlabsR_importConflictingPlotIdColumns"
  )
  convertedFile <- withr::local_tempfile(fileext = ".xlsx")
  .writeExcel(converted$sheets, convertedFile)
  parsed <- .parseExcelPlots(convertedFile)
  expect_identical(
    vapply(parsed$plotConfiguration, function(p) p$id, character(1)),
    c("p_new", "same")
  )
  expect_identical(parsed$plotGrids[[1]]$id, "g_new")
})

# The converted project ----
#
# Each block converts a throwaway copy of a fixture, often with one workbook
# changed, and checks the project the conversion produced.

# Regression (#1184): a grid's `plotIDs` cell is a multi-value Excel cell, and
# the 5.x convention wraps each value in `""` so it may itself contain a comma.
# Decoding that cell with the in-memory codec kept the quotes, which
# canonicalization then turned into underscores (`"P1"` -> `_p1_`), so every
# member of a quoted grid dangled and the freshly imported project failed its
# own validateProject().
test_that("a quoted 5.x plotIDs cell converts with the grid's members intact", {
  projectDir <- localExcelProjectDir()
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

  project <- convertExcelProject(projectDir)$project

  grids <- .unwrapDefinitionList(project$definitions$plotGrids)
  expect_identical(
    .splitPlotIDs(grids[["aciclovir"]]$plots),
    c("p1", "p2", "p3")
  )
  expect_identical(.splitPlotIDs(grids[["aciclovir2"]]$plots), "p2")

  # The whole point: the converted project does not report the grid members as
  # unknown plot ids.
  plotErrors <- vapply(
    suppressWarnings(validateProject(project))$plots$critical_errors,
    function(e) e$message,
    character(1)
  )
  expect_false(any(grepl("references unknown plots", plotErrors)))
})

# Copy the TestProjectExcel fixture and rewrite its individuals and populations
# workbooks into the legacy `Protein` + `Ontogeny` spelling, returning the copied
# project directory. The fixture itself keeps the current single-column spelling
# (which is why it never caught the drop), so the legacy layout is produced here
# rather than checked in as a second binary workbook.
.excelFixtureWithLegacyOntogenies <- function(envir = parent.frame()) {
  work <- withr::local_tempdir(.local_envir = envir)
  file.copy(
    list.files(
      testthat::test_path("data", "TestProjectExcel"),
      full.names = TRUE
    ),
    work,
    recursive = TRUE
  )
  rewrite <- function(workbook, sheet, proteins, ontogenies) {
    path <- file.path(work, "Configurations", workbook)
    names <- readxl::excel_sheets(path)
    sheets <- stats::setNames(
      lapply(names, function(s) readExcel(path, sheet = s)),
      names
    )
    df <- sheets[[sheet]]
    df[["Protein Ontogenies"]] <- NULL
    df[["Protein"]] <- proteins
    df[["Ontogeny"]] <- ontogenies
    sheets[[sheet]] <- df
    .writeExcel(sheets, path)
  }
  # The fixture's individual and its first population both carry
  # `CYP3A4:CYP3A4,CYP2D6:CYP2C8`; its second population carries none.
  rewrite(
    "Individuals.xlsx",
    "IndividualBiometrics",
    "CYP3A4,CYP2D6",
    "CYP3A4,CYP2C8"
  )
  rewrite(
    "Populations.xlsx",
    "Demographics",
    c("CYP3A4,CYP2D6", NA),
    c("CYP3A4,CYP2C8", NA)
  )
  work
}

test_that("a legacy-spelling workbook converts its ontogenies, and round-trips through Excel", {
  work <- .excelFixtureWithLegacyOntogenies()
  project <- convertExcelProject(work)$project

  expected <- c("CYP3A4:CYP3A4", "CYP2D6:CYP2C8")
  expect_identical(
    project$definitions$individuals[["indiv1"]]$proteinOntogenies,
    expected
  )
  expect_identical(
    project$definitions$populations[["testpopulation"]]$proteinOntogenies,
    expected
  )
  # The population authored without ontogenies keeps none, so the two are
  # distinguishable in the converted tree.
  expect_null(
    project$definitions$populations[[
      "testpopulation_noonto"
    ]]$proteinOntogenies
  )

  # Exporting writes the single-cell spelling, and re-importing that reads the
  # same pairs back: the ontogenies are a fixed point of the round trip.
  excelOut <- withr::local_tempdir()
  exportProjectToExcel(project, outputDir = excelOut, silent = TRUE)
  reimported <- suppressWarnings(loadProject(importProjectFromExcel(
    file.path(excelOut, "Project.xlsx"),
    outputDir = withr::local_tempdir(),
    silent = TRUE
  )))
  expect_identical(
    reimported$definitions$individuals[["indiv1"]]$proteinOntogenies,
    expected
  )
  expect_identical(
    reimported$definitions$populations[["testpopulation"]]$proteinOntogenies,
    expected
  )
})

# A sheet named after an individual (the `Indiv1` sheet in the fixture) is that
# individual's own parameter override. The converter moves it to the
# model-parameters workbook AND links it on the individual, so the override is
# applied rather than orphaned (#1158).
test_that("the conversion links an individual to its own override parameter set", {
  project <- convertExcelProject(localExcelProjectDir())$project

  expect_true("indiv1" %in% names(project$definitions$parameterSets))
  indiv <- .unwrapDefinitionList(project$definitions$individuals)[["indiv1"]]
  expect_true("indiv1" %in% unlist(indiv$parameterSets))
})

# The TestProjectExcel fixture uses the 5.x one-sheet-per-protocol layout (no
# `ApplicationProtocols` sheet). The converter writes one protocol per sheet,
# each naming the same-named parameter set, so a scenario that names a protocol
# by id resolves on load rather than dangling (#1158).
test_that("the per-protocol-sheet conversion populates applications and resolves the scenario refs", {
  converted <- convertExcelProject(localExcelProjectDir())
  project <- converted$project

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

  # The scenarios' `application` references resolve: loading emits no
  # "undefined application" warning.
  expect_false(any(grepl("undefined application", converted$warnings)))
})

# In the 5.x applications layout every sheet is a protocol, so a skipped sheet
# must not become an `Application` either: it would wrap a parameter set that
# was never created and dangle on load (#1181).
test_that("a non-parameter sheet in the applications workbook becomes neither a set nor an application", {
  projectDir <- localExcelProjectDir()
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

  project <- convertExcelProject(projectDir)$project

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
  projectDir <- localExcelProjectDir()
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

  project <- convertExcelProject(projectDir)$project

  expect_false("indiv1" %in% names(project$definitions$parameterSets))
  indiv <- .unwrapDefinitionList(project$definitions$individuals)[["indiv1"]]
  expect_false("indiv1" %in% unlist(indiv$parameterSets))
})

test_that("a converted project resolves the population csv folder where the csvs are", {
  # Nothing is copied, so the value itself has to name the folder that holds
  # the csv files.
  project <- convertExcelProject(localExcelProjectDir())$project

  expect_true(file.exists(file.path(
    project$paths$populationsFolder,
    "TestPopulation.csv"
  )))
})

test_that("a populations folder already at the project root is taken as authored", {
  # Both layouts are accepted; a project following the root-level layout must
  # keep resolving there rather than be redirected under the configurations
  # folder.
  projectDir <- localExcelProjectDir()
  file.rename(
    file.path(projectDir, "Configurations", "PopulationsCSV"),
    file.path(projectDir, "PopulationsCSV")
  )

  convertExcelProject(projectDir)

  expect_identical(
    .convertedFilePaths(projectDir)$populationsFolder,
    "PopulationsCSV"
  )
  expect_true(file.exists(file.path(
    projectDir,
    "PopulationsCSV",
    "TestPopulation.csv"
  )))
})

# The clash is detected on the canonical id, so the rename map has to be looked
# up canonically too: a cell spelling the sheet in a different case must follow
# the rename, not keep resolving to the earlier workbook's set.
test_that("the conversion re-points a case-differing parameter-set reference", {
  projectDir <- localExcelProjectDir()
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
  .writeExcel(
    c(
      stats::setNames(
        lapply(mpSheets, function(s) readExcel(modelParamsFile, sheet = s)),
        mpSheets
      ),
      list(Indiv1 = .parameterSheet())
    ),
    modelParamsFile
  )

  indivSheets <- readxl::excel_sheets(individualsFile)
  contents <- stats::setNames(
    lapply(indivSheets, function(s) readExcel(individualsFile, sheet = s)),
    indivSheets
  )
  contents$IndividualBiometrics$ParameterSets <- "indiv1"
  .writeExcel(contents, individualsFile)

  project <- convertExcelProject(projectDir)$project

  # The lower-case cell followed the rename instead of resolving to the
  # model-parameters sheet that took the plain id.
  expect_identical(
    unlist(project$definitions$individuals[["indiv1"]]$parameterSets),
    "indiv1_2"
  )
})

# Before 6.0.0 the model-parameters, individuals, and applications workbooks were
# three separate parameter-set namespaces, so a legacy project may legitimately
# use one sheet name in two of them. They now share a single namespace: the
# workbook read first keeps the plain id, the later sheet is renamed, and the
# references that later workbook makes follow the rename. Without the re-pointing
# the renamed set is orphaned and the referrer silently resolves to the *other*
# workbook's set.
test_that("the conversion renames a duplicate parameter-set id and re-points its own workbook's references", {
  projectDir <- localExcelProjectDir()
  modelParamsFile <- file.path(
    projectDir,
    "Configurations",
    "ModelParameters.xlsx"
  )

  # Add model-parameter sheets clashing with the individuals workbook's `Indiv1`
  # sheet and the applications workbook's `Protocol_250mg` sheet. Model
  # parameters are read first, so both of those get renamed.
  sheets <- readxl::excel_sheets(modelParamsFile)
  .writeExcel(
    c(
      stats::setNames(
        lapply(sheets, function(s) readExcel(modelParamsFile, sheet = s)),
        sheets
      ),
      list(Indiv1 = .parameterSheet(), Protocol_250mg = .parameterSheet())
    ),
    modelParamsFile
  )

  project <- convertExcelProject(projectDir)$project

  # Both sets survive: the model-parameters sheet keeps the plain id, the later
  # workbook's sheet gets the suffixed one.
  sets <- .unwrapDefinitionList(project$definitions$parameterSets)
  expect_contains(
    names(sets),
    c("indiv1", "indiv1_2", "protocol_250mg", "protocol_250mg_2")
  )

  # The individual still carries its OWN parameter set, not the model-parameters
  # sheet that took the id.
  expect_identical(
    unlist(project$definitions$individuals[["indiv1"]]$parameterSets),
    "indiv1_2"
  )
  # Same for the application protocol built around its protocol sheet.
  expect_identical(
    unlist(project$definitions$applications[["protocol_250mg"]]$parameterSets),
    "protocol_250mg_2"
  )
})

test_that("the conversion warns naming each renamed duplicate parameter set", {
  projectDir <- localExcelProjectDir()
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

  # Snapshot the rename warning alone, caught by its own condition class.
  renameWarning <- NULL
  suppressWarnings(withCallingHandlers(
    convertProjectToV6(
      file.path(projectDir, "ProjectConfiguration.xlsx"),
      silent = TRUE
    ),
    esqlabsR_importRenamedParameterSets = function(cnd) {
      renameWarning <<- conditionMessage(cnd)
      invokeRestart("muffleWarning")
    }
  ))

  expect_snapshot(cat(renameWarning))
})

# The `ApplicationProtocols` layout takes its references from a column rather
# than from the sheet list, so it needs the same treatment as the 5.x layout
# (#1181).
test_that("an application referencing a skipped sheet loses the reference", {
  projectDir <- localExcelProjectDir()
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

  project <- convertExcelProject(projectDir)$project

  application <- .unwrapDefinitionList(project$definitions$applications)[[
    "protocol_250mg"
  ]]
  expect_identical(unlist(application$parameterSets), "realsheet")
  expect_false("scratch_notes" %in% names(project$definitions$parameterSets))
})

# The reported case: a `PIOutputMappings` sheet from the layout that predates the
# `OutputPath` column. Every mapping came out with no output path and the restore
# stopped on the first one. The column's absence is read as that older layout,
# whose outputs come from the scenario (#1192).
test_that("a PIOutputMappings sheet with no OutputPath column converts", {
  projectDir <- localExcelProjectDir()
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

  project <- convertExcelProject(projectDir)$project

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

  # The whole point: the converted project is loadable, and the task whose
  # scenario declares output paths has no complaint against it.
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

test_that("the conversion writes the output-path id of a 5.x DataCombined curve", {
  dataCombined <- convertExcelProject(
    localLegacyExcelProject()
  )$project$definitions$dataCombined

  expect_identical(
    vapply(
      c("aciclovirpvb", "aciclovirpop"),
      function(id) dataCombined[[id]]$simulated[[1]]$path,
      character(1),
      USE.NAMES = FALSE
    ),
    c("aciclovir_pvb", "aciclovir_pvb")
  )
})

# The legacy fixture (#1213) ----
#
# These tests pin what the conversion does with the workbook shapes real
# pre-5.6 projects carry, using the `TestProjectExcelLegacy/` fixture. The
# sibling `TestProjectExcel/` fixture uses the modern spelling of every one of
# these shapes.

# #1213 item 1: a two-column `Protein` + `Ontogeny` pair is the pre-5.6 way to
# declare ontogenies. Each cell of the pair is a comma list, zipped positionally
# with the other, and the pair is folded into the single-cell
# `Protein:Ontogeny,...` spelling. A pair that reads cleanly is silent; only an
# unpairable declaration is reported.
test_that("a legacy two-column ontogeny pair converts its ontogenies", {
  converted <- convertExcelProject(localLegacyExcelProject())

  # `adult` declares one ontogeny in the workbook, `child` two, `adultpop` two.
  expect_identical(
    converted$project$definitions$individuals[["adult"]]$proteinOntogenies,
    "CYP3A4:CYP3A4"
  )
  expect_identical(
    converted$project$definitions$individuals[["child"]]$proteinOntogenies,
    c("CYP3A4:CYP3A4", "CYP2D6:CYP2C8")
  )
  expect_identical(
    converted$project$definitions$populations[["adultpop"]]$proteinOntogenies,
    c("CYP3A4:CYP3A4", "CYP2D6:CYP2C8")
  )

  # Every pair in this fixture is readable, so nothing is reported about them.
  expect_false(any(grepl("ontogen", converted$warnings, ignore.case = TRUE)))
})

# #1213 item 1, the sharpest statement of the same thing: two populations authored
# to differ in NOTHING but their ontogenies must not convert to the same
# definition.
test_that("two populations differing only in their ontogenies convert differently", {
  projectDir <- localLegacyExcelProject()
  editWorkbookSheets(
    file.path(projectDir, "Configurations", "Populations.xlsx"),
    function(sheets) {
      demographics <- sheets$Demographics
      # A copy of `AdultPop` whose only difference is that it declares no
      # ontogenies.
      twin <- demographics[demographics$PopulationName == "AdultPop", ]
      twin$PopulationName <- "AdultPopNoOnto"
      twin$Protein <- NA
      twin$Ontogeny <- NA
      sheets$Demographics <- rbind(demographics, twin)
      sheets
    }
  )
  converted <- convertExcelProject(projectDir)

  definitionsDir <- file.path(
    converted$projectDir,
    "definitions",
    "populations"
  )
  withOntogenies <- readLines(file.path(definitionsDir, "adultpop.json"))
  withoutOntogenies <- readLines(file.path(
    definitionsDir,
    "adultpopnoonto.json"
  ))

  # The ontogenies are the whole of the difference, once the id line is set aside.
  dropId <- function(lines) {
    grep("\"id\"", lines, fixed = TRUE, invert = TRUE, value = TRUE)
  }
  expect_false(identical(dropId(withOntogenies), dropId(withoutOntogenies)))
  expect_match(
    grep("proteinOntogenies", withOntogenies, value = TRUE),
    '["CYP3A4:CYP3A4", "CYP2D6:CYP2C8"]',
    fixed = TRUE
  )
  expect_false(any(grepl("proteinOntogenies", withoutOntogenies, fixed = TRUE)))

  # Both files were written, so the comparison above is a real difference.
  expect_true(any(grepl("adultpop", withOntogenies, fixed = TRUE)))
  expect_true(any(grepl("adultpopnoonto", withoutOntogenies, fixed = TRUE)))
})

# #1213 item 4: the rows of a `PIParameters` sheet sharing a `Group` (with the
# same container path and parameter name) are ONE free parameter estimated across
# the scenarios they name between them, so the two rows sharing group 2 here
# convert to one parameter naming both scenarios rather than two independent ones.
test_that("a repeated parameter-identification Group converts to one parameter", {
  converted <- convertExcelProject(localLegacyExcelProject())
  task <- converted$project$definitions$parameterIdentification[[
    "aciclovirfit"
  ]]

  # The workbook's three rows are group 1 (Lipophilicity) and group 2 twice
  # (TSspec, once per scenario), so two free parameters are estimated.
  expect_length(task$parameters, 2L)
  expect_identical(
    vapply(task$parameters, function(p) p$id, character(1)),
    c("lipophilicity", "tsspec")
  )

  # The merged parameter is estimated across both of its rows' scenarios.
  expect_identical(
    unlist(task$parameters[[2]]$scenarios),
    c("piscenario", "piscenario2")
  )
  expect_identical(unlist(task$parameters[[1]]$scenarios), "piscenario")

  # Nothing to report: the group's bounds agree, so it merged.
  expect_false(any(grepl("Group", converted$warnings, fixed = TRUE)))
})

# #1213 item 4, the check esqlabsR 5.x made alongside the merge: one parameter has
# one set of bounds, so rows of a group that disagree about them cannot be one
# parameter. 5.x refused to build the task at all; the conversion keeps the rows
# unmerged (nothing invented, nothing lost) and says so.
test_that("a parameter-identification Group whose bounds disagree is reported and left unmerged", {
  projectDir <- localLegacyExcelProject()
  editWorkbookSheets(
    file.path(projectDir, "Configurations", "ParameterIdentification.xlsx"),
    function(sheets) {
      sheets$PIParameters$MaxValue[[3]] <- 20
      sheets
    }
  )
  converted <- convertExcelProject(projectDir)
  task <- converted$project$definitions$parameterIdentification[[
    "aciclovirfit"
  ]]

  expect_identical(
    vapply(task$parameters, function(p) p$id, character(1)),
    c("lipophilicity", "tsspec", "tsspec_2")
  )
  # Matched on single words: the rendered warning wraps, so a longer phrase can
  # straddle a line break.
  expect_true(any(grepl("bounds", converted$warnings, fixed = TRUE)))
  expect_true(any(grepl("Group", converted$warnings, fixed = TRUE)))
})

# #1213 item 14: the legacy `Units` cell is carried through as the parameter's
# declared display unit, which is lossless.
test_that("a legacy parameter-identification Units cell is carried through", {
  converted <- convertExcelProject(localLegacyExcelProject())
  task <- converted$project$definitions$parameterIdentification[[
    "aciclovirfit"
  ]]

  expect_identical(
    vapply(
      task$parameters,
      function(p) p$units %||% NA_character_,
      character(1)
    ),
    c("Log Units", "mg")
  )
})

# The 5.x parameter-identification layout builds one task per distinct
# `PITaskName`, gathering that task's rows from five sheets. A sheet quoting the
# task name names the same task, so its rows belong to it rather than to a second
# task that overwrites the first.
test_that("a PI task name spelled quoted and unquoted is one task", {
  projectDir <- localLegacyExcelProject()
  editWorkbookSheets(
    file.path(projectDir, "Configurations", "ParameterIdentification.xlsx"),
    function(sheets) {
      sheets$PIParameters$PITaskName <- paste0(
        '"',
        sheets$PIParameters$PITaskName,
        '"'
      )
      sheets
    }
  )
  tasks <- convertExcelProject(
    projectDir
  )$project$definitions$parameterIdentification

  expect_named(tasks, "aciclovirfit")

  # The quoted sheet's parameters and the unquoted sheets' output mappings are
  # both on the one task.
  expect_length(tasks[["aciclovirfit"]]$parameters, 2L)
  expect_length(tasks[["aciclovirfit"]]$outputMappings, 2L)
})

# #1213 item 5: an esqlabsR 5 project spells `populationsFolder` as a folder name
# under the configurations folder, while a project resolves its working folders
# against its own root. The conversion writes the value as the path of that
# folder from the project folder.
test_that("the populations CSV folder resolves in a converted project", {
  projectDir <- localLegacyExcelProject()
  converted <- convertExcelProject(projectDir)

  # It stays where it is, under the configurations folder.
  expect_true(dir.exists(
    file.path(projectDir, "Configurations", "PopulationsCSV")
  ))
  expect_false(dir.exists(file.path(projectDir, "PopulationsCSV")))

  # And the folder the project resolves is the one holding the csv file.
  expect_true(file.exists(file.path(
    converted$project$paths$populationsFolder,
    "CsvPop.csv"
  )))

  summary <- validationSummary(suppressWarnings(validateProject(
    converted$project
  )))
  expect_equal(summary$total_critical_errors, 0)
})

# #1213 item 9, first route: a parameter sheet whose column headers are duplicated
# is correctly rejected as not a parameter sheet. The rejection names the sheet,
# and a second warning names the consequence: the individual that sheet
# parametrized is converted with no parametrization at all, while its siblings
# keep theirs and the project still validates clean.
test_that("a parameter sheet with duplicated headers names the individual it costs the parametrization", {
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
  converted <- convertExcelProject(projectDir)

  # `adult` links to its own sheet; `child` has nothing left to link to.
  expect_identical(
    unlist(converted$project$definitions$individuals[["adult"]]$parameterSets),
    "adult"
  )
  expect_null(
    converted$project$definitions$individuals[["child"]]$parameterSets
  )
  expect_false("child" %in% names(converted$project$definitions$parameterSets))

  # Two warnings: the sheet that was skipped, and the individual it left
  # unparametrized.
  expect_length(converted$warnings, 2L)
  expect_match(converted$warnings[[1]], "Skipped sheet")
  expect_match(converted$warnings[[2]], "Child")
  expect_match(converted$warnings[[2]], "without the parameter set")
})

# #1213 item 9, second route: individuals are keyed off the biometrics rows, so an
# individual that has a parameter sheet but no biometrics row is not converted.
# Its sheet still becomes a parameter set owned by nobody, and the scenario naming
# the individual is left dangling. Validation reports that dangling reference,
# which is the symptom; the conversion names the cause, the sheet whose individual
# has no biometrics row, and the row to add.
test_that("an individual with a parameter sheet but no biometrics row is dropped, and the sheet is named", {
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
  converted <- convertExcelProject(projectDir)

  expect_named(converted$project$definitions$individuals, "adult")

  # The orphaned sheet survives as a parameter set with no individual.
  expect_true("child" %in% names(converted$project$definitions$parameterSets))

  # The scenario keeps pointing at the individual that no longer exists.
  expect_identical(
    converted$project$definitions$scenarios[["childscenario"]]$individual,
    "child"
  )

  # The conversion names the sheet whose individual is not defined, and
  # validation still reports the reference the scenario is left with.
  expect_true(any(grepl("does not", converted$warnings, fixed = TRUE)))
  expect_true(any(grepl("Child", converted$warnings, fixed = TRUE)))
  expect_true(any(grepl("undefined individual", converted$warnings)))
})

# #1213 item 15: the 5.x `PIOutputMappings` sheet names the sheet of the data
# workbook each mapping's data set comes from. A project has no per-mapping
# counterpart for it, so the column is not carried and the base fixture, whose
# sheet is in the workbook, says nothing. A named sheet that is NOT in the data
# workbook is a mapping whose data set the project does not hold, and that is
# reported.
test_that("an ObservedDataSheet naming a sheet outside the data workbook is reported", {
  expect_false(any(grepl(
    "ObservedDataSheet",
    convertExcelProject(localLegacyExcelProject())$warnings,
    fixed = TRUE
  )))

  projectDir <- localLegacyExcelProject()
  editWorkbookSheets(
    file.path(projectDir, "Configurations", "ParameterIdentification.xlsx"),
    function(sheets) {
      sheets$PIOutputMappings$ObservedDataSheet <- "Laskin 1982.Group B"
      sheets
    }
  )
  converted <- convertExcelProject(projectDir)

  expect_true(any(grepl(
    "ObservedDataSheet",
    converted$warnings,
    fixed = TRUE
  )))
  expect_true(any(grepl("Group B", converted$warnings, fixed = TRUE)))
})

# #1213 item 26: the 5.x `exportConfiguration` sheet says where each grid's figure
# is written and at what size, and a project has no field for any of it, so the
# rows cannot be converted and an export cannot write them back. That decision is
# the user's figure layout, so it is reported rather than dropped in silence. The
# base fixture's sheet is header-only, which is what a workbook carries whether or
# not it was filled in, and says nothing.
test_that("a filled legacy plots exportConfiguration sheet is reported, an empty one is not", {
  expect_false(any(grepl(
    "exportConfiguration",
    convertExcelProject(localLegacyExcelProject())$warnings,
    fixed = TRUE
  )))

  projectDir <- localLegacyExcelProject()
  editWorkbookSheets(
    file.path(projectDir, "Configurations", "Plots.xlsx"),
    function(sheets) {
      sheets$exportConfiguration <- data.frame(
        plotGridName = "Aciclovir",
        outputName = "aciclovir-pvb",
        width = 20,
        stringsAsFactors = FALSE
      )
      sheets
    }
  )
  converted <- convertExcelProject(projectDir)

  expect_true(any(grepl(
    "exportConfiguration",
    converted$warnings,
    fixed = TRUE
  )))
  expect_true(any(grepl("Aciclovir", converted$warnings, fixed = TRUE)))
  expect_true(any(grepl("exportPlot", converted$warnings, fixed = TRUE)))
})

# #969: a 5.x `plotConfiguration` sheet styles each plot with columns such as
# `linesColor` or `legendPosition`. They are plot fields, so they are kept and
# converted by the field table (a single number to a number, a yes/no cell to a
# logical), and `"lin"`, the 5.x spelling of the linear scale, becomes
# `"linear"`. The export writes them back, so the workbook stays in sync.
test_that("the conversion keeps the 5.x plot settings and converts lin", {
  projectDir <- localLegacyExcelProject()
  editWorkbookSheets(
    file.path(projectDir, "Configurations", "Plots.xlsx"),
    function(sheets) {
      plots <- sheets$plotConfiguration
      plots$linesColor <- c("red, blue", NA, NA)
      plots$legendPosition <- c("insideTopRight", NA, NA)
      plots$titleSize <- c("14", NA, NA)
      plots$displayLLOQ <- c("no", NA, NA)
      plots$xAxisScale <- c("lin", NA, "log")
      plots$lloqOnBothAxes <- c(NA, "no", NA)
      sheets$plotConfiguration <- plots
      sheets
    }
  )

  converted <- convertExcelProject(projectDir)
  plots <- converted$project$definitions$plots

  checkedFields <- c(
    "linesColor",
    "legendPosition",
    "titleSize",
    "displayLLOQ",
    "xAxisScale",
    "lloqOnBothAxes"
  )
  expect_equal(
    lapply(plots, function(p) unclass(p)[intersect(checkedFields, names(p))]),
    list(
      p1 = list(
        linesColor = "red, blue",
        legendPosition = "insideTopRight",
        titleSize = 14,
        displayLLOQ = FALSE,
        xAxisScale = "linear"
      ),
      p2 = list(lloqOnBothAxes = FALSE),
      p3 = list(xAxisScale = "log")
    )
  )

  suppressMessages(exportProjectToExcel(
    converted$project,
    outputDir = converted$projectDir,
    overwrite = TRUE,
    silent = TRUE
  ))
  status <- suppressWarnings(suppressMessages(projectStatus(
    converted$project
  )))
  expect_true(status$excel_in_sync)
})

test_that("converting a 5.x workbook keeps a Scenario_name that canonicalizing changes beyond case as the label", {
  projectDir <- localLegacyExcelProject()
  editWorkbookSheets(
    file.path(projectDir, "Configurations", "Scenarios.xlsx"),
    function(sheets) {
      scenarios <- sheets$Scenarios
      scenarios$Scenario_name[scenarios$Scenario_name == "PIScenario2"] <-
        "PI Scenario 2"
      sheets$Scenarios <- scenarios
      sheets
    }
  )

  scenarios <- convertExcelProject(projectDir)$project$definitions$scenarios

  # `PI Scenario 2` loses its spaces in the id and keeps them in the label;
  # `AdultScenario` only loses its capitals, so it gets no label.
  expect_identical(scenarios[["pi_scenario_2"]]$label, "PI Scenario 2")
  expect_null(scenarios[["adultscenario"]]$label)
})

test_that("a Label cell wins over the Scenario_name kept as the label", {
  projectDir <- localLegacyExcelProject()
  editWorkbookSheets(
    file.path(projectDir, "Configurations", "Scenarios.xlsx"),
    function(sheets) {
      scenarios <- sheets$Scenarios
      scenarios$Label <- NA_character_
      scenarios$Label[scenarios$Scenario_name == "AdultScenario"] <-
        "Adult, 2.5 mg/kg IV"
      sheets$Scenarios <- scenarios
      sheets
    }
  )

  scenarios <- convertExcelProject(projectDir)$project$definitions$scenarios

  expect_identical(scenarios[["adultscenario"]]$label, "Adult, 2.5 mg/kg IV")
  expect_null(scenarios[["childscenario"]]$label)
})

test_that(".v5ToLogical reads the boolean spellings of esqlabsR 5", {
  expect_identical(
    vapply(
      list(TRUE, 1, 0, "Yes", "n", "  ", NA, NULL),
      function(x) .v5ToLogical(x, "SteadyState"),
      logical(1)
    ),
    c(TRUE, TRUE, FALSE, TRUE, FALSE, NA, NA, NA)
  )
  expect_error(.v5ToLogical(2, "SteadyState"), "SteadyState")
})

test_that(".v5ParsePIConfig nests the objective function and run options", {
  configDf <- data.frame(
    PITaskName = "t1",
    Algorithm = "BOBYQA",
    AutoEstimateCI = "Yes",
    ObjectiveFunctionType = "lsq",
    ResidualWeightingMethod = "std",
    LinScaleCV = 0.2,
    numberOfCores = 2,
    checkForNegativeValues = 0,
    stringsAsFactors = FALSE
  )

  expect_identical(
    .v5ParsePIConfig(configDf, NULL, NULL, "t1"),
    list(
      algorithm = "BOBYQA",
      autoEstimateCI = TRUE,
      objectiveFunction = list(
        type = "lsq",
        residualWeightingMethod = "std",
        linScaleCV = 0.2
      ),
      simulationRunOptions = list(
        numberOfCores = 2,
        checkForNegativeValues = FALSE
      )
    )
  )
})
