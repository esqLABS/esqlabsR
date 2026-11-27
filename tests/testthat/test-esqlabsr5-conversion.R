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
  expect_length(converted$project$definitions$initialConditions, 1L)
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
    POPCSV = "Configurations/PopulationsCSV",
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
      populationsFolder = "${POPCSV}",
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
