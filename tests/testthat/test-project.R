# JSON loading ----

test_that("Project loads from valid JSON and populates all fields", {
  jsonPath <- testProjectJSONPath()
  project <- Project$new(jsonPath)

  expect_true(isOfType(project, "Project"))
  expect_true(fs::file_exists(project$modelFolder))
  expect_true(fs::dir_exists(project$configurationsFolder))

  expect_length(project$scenarios, 5)
  expect_true("TestScenario" %in% names(project$scenarios))
  expect_length(project$modelParameters, 4)
  expect_true("Global" %in% names(project$modelParameters))
  expect_length(project$individuals, 1)
  expect_true("Indiv1" %in% names(project$individuals))
  expect_length(project$populations, 2)
  expect_length(project$applications, 1)
  expect_length(project$outputPaths, 2)
  expect_false(is.null(project$plots))
  expect_equal(as.character(project$jsonPath), normalizePath(jsonPath))
})

test_that("Project errors on missing schemaVersion", {
  temp <- tempfile(fileext = ".json")
  writeLines('{"filePaths": {}}', temp)
  expect_error(Project$new(temp), "schemaVersion")
  unlink(temp)
})

test_that("Project errors on unsupported schemaVersion", {
  temp <- tempfile(fileext = ".json")
  writeLines('{"schemaVersion": "99.0", "filePaths": {}}', temp)
  expect_error(Project$new(temp), "schemaVersion")
  unlink(temp)
})

test_that("Project resolves paths relative to JSON directory", {
  jsonPath <- testProjectJSONPath()
  project <- Project$new(jsonPath)
  jsonDir <- dirname(jsonPath)
  expect_equal(
    as.character(project$modelFolder),
    normalizePath(file.path(jsonDir, "Models/Simulations/"))
  )
})

test_that("Project parses model parameters into list(paths, values, units)", {
  project <- testProject()
  global <- project$modelParameters[["Global"]]
  expect_equal(global$paths, "Organism|Liver|EHC continuous fraction")
  expect_equal(global$values, 1)
  expect_equal(global$units, "")
})

test_that("Project parses scenarios into Scenario objects", {
  project <- testProject()
  sc <- project$scenarios[["TestScenario2"]]
  expect_s3_class(sc, "Scenario")
  expect_equal(sc$scenarioName, "TestScenario2")
  expect_true(sc$simulateSteadyState)
  expect_equal(length(sc$simulationTime), 2)
  expect_equal(
    sc$modelParameters,
    c("Global", "Aciclovir", "Sheet, with comma")
  )
})

test_that("Individuals are stored as plain lists", {
  project <- testProject()
  indiv <- project$individuals[["Indiv1"]]
  expect_type(indiv, "list")
  expect_true("species" %in% names(indiv))
  expect_true("population" %in% names(indiv))
  expect_true("gender" %in% names(indiv))
  expect_true("weight" %in% names(indiv))
  expect_true("height" %in% names(indiv))
  expect_true("age" %in% names(indiv))
  # NOT an IndividualCharacteristics object
  expect_false(isOfType(indiv, "IndividualCharacteristics"))
})

test_that("Populations are stored as plain lists", {
  project <- testProject()
  pop <- project$populations[["TestPopulation"]]
  expect_type(pop, "list")
  expect_true("species" %in% names(pop))
  expect_true("population" %in% names(pop))
  expect_true("numberOfIndividuals" %in% names(pop))
  # NOT a PopulationCharacteristics object
  expect_false(isOfType(pop, "PopulationCharacteristics"))
})

# Path property setters ----

test_that("setting non-existent outputFolder does not warn", {
  project <- testProject()
  expect_no_warning(project$outputFolder <- "this/directory/does/not/exist")
})

test_that("setting non-existent dataFolder warns", {
  project <- testProject()
  expect_warning(project$dataFolder <- "this/directory/does/not/exist")
})

test_that("setting invalid paths warns for each path property", {
  project <- testProject()
  expect_warning(project$configurationsFolder <- "Wrong/Folder")
  expect_warning(project$dataFolder <- "folder/data/does/not/exist")
  expect_warning(project$modelFolder <- "folder/model/does/not/exist")
  expect_warning(
    project$populationsFolder <- "folder/populations/does/not/exist"
  )
  expect_warning(project$modelParamsFile <- "modelparams_donotexist.xlsx")
  expect_warning(project$individualsFile <- "individuals_donotexist.xlsx")
  expect_warning(project$populationsFile <- "populations_donotexist.xlsx")
  expect_warning(project$scenariosFile <- "scenarios_donotexist.xlsx")
  expect_warning(project$applicationsFile <- "applications_donotexist.xlsx")
  expect_warning(project$plotsFile <- "plots_donotexist.xlsx")
})

# Modified flag ----

test_that("modified flag is FALSE when first created from JSON", {
  project <- testProject()
  expect_false(project$modified)
})

test_that("modified flag is FALSE for empty Project", {
  project <- Project$new()
  expect_false(project$modified)
})

test_that("modified flag is read-only", {
  project <- testProject()
  expect_error(project$modified <- TRUE, "modified is readonly")
  expect_error(project$modified <- FALSE, "modified is readonly")
})

test_that("modified flag becomes TRUE when any property is changed", {
  properties <- c(
    "modelFolder",
    "configurationsFolder",
    "modelParamsFile",
    "individualsFile",
    "populationsFile",
    "populationsFolder",
    "scenariosFile",
    "applicationsFile",
    "plotsFile",
    "dataFolder",
    "outputFolder"
  )
  for (prop in properties) {
    project <- testProject()
    expect_false(project$modified)
    suppressWarnings(project[[prop]] <- "new/value")
    expect_true(project$modified, info = paste("property:", prop))
  }
})

test_that("modified flag persists across multiple property changes", {
  project <- testProject()
  expect_false(project$modified)
  suppressWarnings(project$modelFolder <- "new/model/folder")
  expect_true(project$modified)
  suppressWarnings(project$dataFolder <- "new/data/folder")
  expect_true(project$modified)
  project$outputFolder <- "new/output/folder"
  expect_true(project$modified)
})

test_that("modified flag is independent between clones", {
  project <- testProject()
  cloned <- project$clone()
  expect_false(cloned$modified)

  suppressWarnings(project$modelFolder <- "modified/folder")
  expect_true(project$modified)
  expect_false(cloned$modified)

  suppressWarnings(cloned$dataFolder <- "modified/data/folder")
  expect_true(project$modified)
  expect_true(cloned$modified)
})

test_that("Scenarios are parsed as Scenario objects (not ScenarioConfiguration)", {
  project <- testProject()
  expect_true(length(project$scenarios) > 0)
  sc <- project$scenarios[["TestScenario"]]
  expect_s3_class(sc, "Scenario")
  expect_equal(sc$scenarioName, "TestScenario")
  expect_false(is.null(sc$modelFile))
  expect_equal(sc$modelParameters, c("Global"))
})

test_that("project$scenarios replaces project$scenarioConfigurations", {
  project <- testProject()
  expect_true(!is.null(project$scenarios))
})

# Print method ----

test_that("print() shows category counts for loaded project", {
  project <- testProject()
  output <- capture.output(print(project))
  output_text <- paste(output, collapse = "\n")

  expect_match(output_text, "Scenarios:\\s+5")
  expect_match(output_text, "Individuals:\\s+1")
  expect_match(output_text, "Populations:\\s+2")
  expect_match(output_text, "Model Parameters:\\s+4 groups")
  expect_match(output_text, "Applications:\\s+1")
  expect_match(output_text, "Output Paths:\\s+2")
  expect_match(output_text, "2 dataCombined")
  expect_match(output_text, "4 plotConfiguration")
  expect_match(output_text, "3 plotGrids")
})

test_that("print() does not show Excel configuration file paths", {
  project <- testProject()
  output <- capture.output(print(project))
  output_text <- paste(output, collapse = "\n")

  expect_no_match(output_text, "Model params file")
  expect_no_match(output_text, "Individuals file")
  expect_no_match(output_text, "Populations file")
  expect_no_match(output_text, "Scenarios file")
  expect_no_match(output_text, "Applications file")
  expect_no_match(output_text, "Plots file")
  expect_no_match(output_text, "Configurations folder")
  expect_no_match(output_text, "Environment Variables")
})

test_that("print() shows 0 counts for empty Project", {
  project <- Project$new()
  output <- capture.output(print(project))
  output_text <- paste(output, collapse = "\n")

  expect_match(output_text, "Scenarios:\\s+0")
  expect_match(output_text, "Individuals:\\s+0")
  expect_match(output_text, "Populations:\\s+0")
  expect_match(output_text, "Model Parameters:\\s+0 groups")
  expect_match(output_text, "Applications:\\s+0")
  expect_match(output_text, "Output Paths:\\s+0")
  expect_match(output_text, "Plots:\\s+0")
})

test_that("print() shows relative paths instead of absolute paths", {
  project <- testProject()
  output <- capture.output(print(project))
  output_text <- paste(output, collapse = "\n")

  expect_match(output_text, "Model folder:\\s+Models/Simulations")
  expect_match(output_text, "Data folder:\\s+Data")
  expect_match(output_text, "Output folder:\\s+Results")
  expect_no_match(output_text, project$projectDirPath, fixed = TRUE)
})

# observedData parsing ----

test_that("observedData field is populated from JSON", {
  project <- testProject()
  expect_type(project$observedData, "list")
  expect_true(length(project$observedData) > 0)
  expect_equal(project$observedData[[1]]$type, "excel")
  expect_equal(project$observedData[[1]]$sheets, list("Laskin 1982.Group A"))
})

test_that("observedData is empty list when JSON has no observedData", {
  tmp <- tempfile(fileext = ".json")
  writeLines(
    '{"schemaVersion": "2.0", "filePaths": {}}',
    tmp
  )
  project <- Project$new(tmp)
  expect_type(project$observedData, "list")
  expect_length(project$observedData, 0)
  unlink(tmp)
})

# loadProject error paths ----

test_that("loadProject errors on non-existent file path", {
  expect_error(
    loadProject("/non/existent/path/Project.json"),
    "File not found"
  )
})

test_that("loadProject errors on non-JSON file content", {
  tmp <- tempfile(fileext = ".json")
  writeLines("This is not valid JSON content", tmp)
  expect_error(loadProject(tmp))
  unlink(tmp)
})

test_that("saveProject preserves esqlabsRVersion from source JSON", {
  jsonPath <- testProjectJSONPath()
  rawJson <- jsonlite::fromJSON(jsonPath, simplifyVector = FALSE)
  storedVersion <- rawJson$esqlabsRVersion
  expect_false(is.null(storedVersion))

  project <- loadProject(jsonPath)

  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp), add = TRUE)
  saveProject(project, tmp)

  reloaded <- jsonlite::fromJSON(tmp, simplifyVector = FALSE)
  expect_equal(reloaded$esqlabsRVersion, storedVersion)
})

test_that("loadProject errors when scenario references unknown outputPathIds", {
  jsonPath <- testProjectJSONPath()
  cfg <- jsonlite::fromJSON(jsonPath, simplifyVector = FALSE)

  cfg$scenarios[[1]]$outputPathIds <- list("DoesNotExist")

  tmpDir <- withr::local_tempdir()
  tmpJson <- file.path(tmpDir, "Project.json")
  jsonlite::write_json(cfg, tmpJson, auto_unbox = TRUE, null = "null")

  expect_error(loadProject(tmpJson), "DoesNotExist")
})

test_that("loadProject errors when steadyStateTime is set but steadyStateTimeUnit is null", {
  jsonPath <- testProjectJSONPath()
  cfg <- jsonlite::fromJSON(jsonPath, simplifyVector = FALSE)

  # Set steadyStateTime but leave steadyStateTimeUnit as NULL

  cfg$scenarios[[1]]$steadyStateTime <- 500
  cfg$scenarios[[1]]$steadyStateTimeUnit <- NULL

  tmpDir <- withr::local_tempdir()
  tmpJson <- file.path(tmpDir, "Project.json")
  jsonlite::write_json(cfg, tmpJson, auto_unbox = TRUE, null = "null")

  expect_error(
    loadProject(tmpJson),
    "steadyStateTimeUnit.*null"
  )
})

# asList accessor ----

test_that("asList accessor returns complete project structure as list", {
  project <- testProject()
  data <- project$asList

  expect_type(data, "list")
  expect_equal(data$schemaVersion, "2.0")
  expect_true("filePaths" %in% names(data))
  expect_true("scenarios" %in% names(data))
  expect_true("modelParameters" %in% names(data))
  expect_true("individuals" %in% names(data))
  expect_true("populations" %in% names(data))
  expect_true("applications" %in% names(data))
  expect_true("outputPaths" %in% names(data))
  expect_true("plots" %in% names(data))
})

test_that("asList accessor is read-only", {
  project <- testProject()
  expect_error(project$asList <- list(), "readonly")
})

test_that("asList accessor preserves schemaVersion from source JSON", {
  jsonPath <- testProjectJSONPath()
  rawJson <- jsonlite::fromJSON(jsonPath, simplifyVector = FALSE)
  project <- Project$new(jsonPath)

  expect_equal(project$asList$schemaVersion, rawJson$schemaVersion)
})

test_that("asList accessor preserves filePaths values from source JSON", {
  jsonPath <- testProjectJSONPath()
  rawJson <- jsonlite::fromJSON(jsonPath, simplifyVector = FALSE)
  project <- Project$new(jsonPath)

  for (field in names(rawJson$filePaths)) {
    raw_val <- rawJson$filePaths[[field]]
    data_val <- project$asList$filePaths[[field]]
    if (!is.null(raw_val)) {
      expect_equal(
        gsub("/$", "", as.character(data_val)),
        gsub("/$", "", as.character(raw_val)),
        info = paste("filePaths field:", field)
      )
    }
  }
})

test_that("asList accessor preserves scenario count from source JSON", {
  jsonPath <- testProjectJSONPath()
  rawJson <- jsonlite::fromJSON(jsonPath, simplifyVector = FALSE)
  project <- Project$new(jsonPath)

  expect_equal(length(project$asList$scenarios), length(rawJson$scenarios))
})

test_that("asList accessor preserves scenario names from source JSON", {
  jsonPath <- testProjectJSONPath()
  rawJson <- jsonlite::fromJSON(jsonPath, simplifyVector = FALSE)
  project <- Project$new(jsonPath)

  rawNames <- vapply(rawJson$scenarios, function(s) s$name, character(1))
  dataNames <- vapply(
    project$asList$scenarios,
    function(s) s$name,
    character(1)
  )
  expect_equal(sort(unname(dataNames)), sort(unname(rawNames)))
})

test_that("asList accessor preserves modelParameters from source JSON", {
  jsonPath <- testProjectJSONPath()
  rawJson <- jsonlite::fromJSON(jsonPath, simplifyVector = FALSE)
  project <- Project$new(jsonPath)

  expect_equal(
    names(project$asList$modelParameters),
    names(rawJson$modelParameters)
  )
})

test_that("asList accessor preserves individuals from source JSON", {
  jsonPath <- testProjectJSONPath()
  rawJson <- jsonlite::fromJSON(jsonPath, simplifyVector = FALSE)
  project <- Project$new(jsonPath)

  expect_equal(length(project$asList$individuals), length(rawJson$individuals))
})

test_that("asList accessor preserves populations from source JSON", {
  jsonPath <- testProjectJSONPath()
  rawJson <- jsonlite::fromJSON(jsonPath, simplifyVector = FALSE)
  project <- Project$new(jsonPath)

  expect_equal(length(project$asList$populations), length(rawJson$populations))
})

test_that("asList accessor preserves outputPaths from source JSON", {
  jsonPath <- testProjectJSONPath()
  rawJson <- jsonlite::fromJSON(jsonPath, simplifyVector = FALSE)
  project <- Project$new(jsonPath)

  expect_equal(project$asList$outputPaths, rawJson$outputPaths)
})

test_that("asList accessor reflects in-memory changes after addScenario", {
  project <- testProject()
  originalCount <- length(project$asList$scenarios)

  addScenario(
    project,
    "NewTestScenario",
    "Aciclovir.pkml",
    individualId = "Indiv1"
  )

  expect_equal(length(project$asList$scenarios), originalCount + 1)
  newNames <- vapply(project$asList$scenarios, function(s) s$name, character(1))
  expect_true("NewTestScenario" %in% newNames)
})

test_that("asList accessor returns default structure for empty Project", {
  project <- Project$new()
  data <- project$asList
  expect_equal(data$schemaVersion, "2.0")
  expect_equal(length(data$scenarios), 0)
  expect_equal(length(data$individuals), 0)
  expect_equal(length(data$populations), 0)
})

# sync method ----

test_that("sync returns in_sync TRUE for freshly loaded project", {
  project <- testProject()
  result <- project$sync(silent = TRUE)

  expect_type(result, "list")
  expect_true(result$in_sync)
  expect_false(result$unsaved_changes)
})

test_that("sync detects unsaved changes after modification", {
  project <- testProject()
  addScenario(project, "NewScenario", "Aciclovir.pkml", individualId = "Indiv1")

  result <- project$sync(silent = TRUE)

  expect_false(result$in_sync)
  expect_true(result$unsaved_changes)
})

test_that("sync shows unsaved_changes FALSE after saveProject", {
  temp_proj <- local_test_project()
  project <- loadProject(temp_proj$snapshot_path)
  addScenario(project, "NewScenario", "Aciclovir.pkml", individualId = "Indiv1")

  saveProject(project)
  result <- project$sync(silent = TRUE)

  expect_false(result$unsaved_changes)
  expect_false(result$json_modified)
  expect_true(result$excel_modified)
})

test_that("sync detects external JSON file changes", {
  temp_proj <- local_test_project()
  project <- loadProject(temp_proj$snapshot_path)

  cfg <- jsonlite::fromJSON(temp_proj$snapshot_path, simplifyVector = FALSE)
  cfg$scenarios[[1]]$name <- "RenamedByExternalEdit"
  jsonlite::write_json(
    cfg,
    temp_proj$snapshot_path,
    auto_unbox = TRUE,
    null = "null"
  )

  result <- project$sync(silent = TRUE)

  expect_false(result$in_sync)
  expect_true(result$json_modified)
})

test_that("sync detects Excel files when JSON is missing", {
  temp_proj <- local_test_project()
  project <- loadProject(temp_proj$snapshot_path)

  # Delete the JSON but keep Excel files. Sync should not silently report
  # in_sync; it should flag the Excel-side state as modified relative to
  # the absent JSON.
  unlink(temp_proj$snapshot_path)
  expect_false(file.exists(temp_proj$snapshot_path))

  result <- project$sync(silent = TRUE)

  expect_false(result$in_sync)
  expect_true(result$excel_modified)
})

test_that("sync reports excel_modified when Excel differs from JSON", {
  temp_proj <- local_test_project()
  project <- loadProject(temp_proj$snapshot_path)

  scenariosXlsx <- file.path(temp_proj$configurations_dir, "Scenarios.xlsx")
  scenariosDf <- readExcel(scenariosXlsx, sheet = "Scenarios")
  scenariosDf$Scenario_name[[1]] <- "ModifiedInExcel"
  .writeExcel(list(Scenarios = scenariosDf), scenariosXlsx)

  result <- project$sync(silent = TRUE)

  expect_false(result$in_sync)
  expect_true(result$excel_modified)
})


test_that("full project can be built programmatically using add* functions", {
  project <- Project$new()
  project$modelFolder <- testProjectJSONPath() |>
    dirname() |>
    file.path("Models")
  project$dataFolder <- testProjectJSONPath() |>
    dirname() |>
    file.path("Data")
  project$outputFolder <- tempdir()

  addIndividual(
    project,
    "I1",
    species = "Human",
    weight = 70,
    height = 175,
    age = 30
  )
  addPopulation(project, "P1", species = "Human", numberOfIndividuals = 5)
  addModelParameter(
    project,
    "G1",
    containerPath = "Organism|Liver",
    parameterName = "Volume",
    value = 1.8,
    units = "L"
  )
  addApplication(project, "Oral")
  addApplicationParameter(
    project,
    "Oral",
    containerPath = "Applications|Oral",
    parameterName = "Dose",
    value = 10,
    units = "mg"
  )
  addOutputPath(
    project,
    "Out1",
    "Organism|PeripheralVenousBlood|Aciclovir|Concentration in container"
  )
  addScenario(
    project,
    "S1",
    "Aciclovir.pkml",
    individualId = "I1",
    applicationProtocol = "Oral",
    modelParameters = "G1",
    outputPathIds = "Out1"
  )

  expect_true(project$modified)
  expect_equal(length(project$individuals), 1)
  expect_equal(length(project$populations), 1)
  expect_equal(length(project$modelParameters), 1)
  expect_equal(length(project$applications), 1)
  expect_equal(length(project$outputPaths), 1)
  expect_equal(length(project$scenarios), 1)

  # Round-trip
  tmp <- tempfile(fileext = ".json")
  saveProject(project, tmp)
  reloaded <- loadProject(tmp)
  expect_equal(names(reloaded$individuals), "I1")
  expect_equal(names(reloaded$scenarios), "S1")
  expect_equal(reloaded$scenarios[["S1"]]$individualId, "I1")
})

test_that("parsed individuals and applications carry S3 class tags", {
  project <- testProject()
  if (length(project$individuals) > 0) {
    expect_true(inherits(project$individuals[[1]], "Individual"))
  }
  if (length(project$applications) > 0) {
    expect_true(inherits(project$applications[[1]], "Application"))
  }
})

# saveProject() ----

test_that("saveProject writes valid JSON that can be reloaded", {
  project <- testProject()
  tmp <- tempfile(fileext = ".json")
  withr::defer(unlink(tmp))

  saveProject(project, tmp)

  expect_true(file.exists(tmp))
  pc2 <- loadProject(tmp)
  expect_s3_class(pc2, "Project")
})

test_that("saveProject defaults to project$jsonPath when path is NULL", {
  tmpDir <- withr::local_tempdir()
  jsonPath <- file.path(tmpDir, "Project.json")
  file.copy(testProjectJSONPath(), jsonPath)

  project <- loadProject(jsonPath)
  project$scenarios[["NewScenario"]] <- Scenario$new()
  project$scenarios[["NewScenario"]]$scenarioName <- "NewScenario"
  project$scenarios[["NewScenario"]]$modelFile <- "Test.pkml"

  saveProject(project)

  pc2 <- loadProject(jsonPath)
  expect_true("NewScenario" %in% names(pc2$scenarios))
})

test_that("saveProject errors when path is NULL and project has no jsonPath", {
  project <- Project$new()
  expect_error(saveProject(project), "path")
})



test_that("saveProject resets modified flag to FALSE", {
  project <- testProject()
  project$.markModified()
  tmp <- tempfile(fileext = ".json")
  withr::defer(unlink(tmp))

  saveProject(project, tmp)

  expect_false(project$modified)
})


test_that("saveProject defaults esqlabsRVersion to current package version when project lacks one", {
  project <- testProject()
  project$esqlabsRVersion <- NULL
  tmp <- tempfile(fileext = ".json")
  withr::defer(unlink(tmp))

  saveProject(project, tmp)

  jsonData <- jsonlite::fromJSON(tmp, simplifyVector = FALSE)
  expect_equal(
    jsonData$esqlabsRVersion,
    as.character(utils::packageVersion("esqlabsR"))
  )
})

# initProject() / isProjectInitialized() ----

test_that("isProjectInitialized correctly identifies project directories", {
  tempDir <- withr::local_tempdir(pattern = "test_project_check")

  # Should return FALSE for empty directory
  expect_false(isProjectInitialized(tempDir))

  # Should return TRUE when Project.xlsx exists
  initProject(destination = tempDir, overwrite = TRUE)
  expect_true(isProjectInitialized(tempDir))

  # Clean up and test with Configurations folder only
  unlink(file.path(tempDir, "Project.xlsx"))
  expect_true(isProjectInitialized(tempDir))
})

test_that("isProjectInitialized handles non-existent directories", {
  expect_false(isProjectInitialized("non_existent_directory"))
})

test_that("isProjectInitialized detects Project.xlsx without Configurations folder", {
  tempDir <- withr::local_tempdir(pattern = "test_project_xlsx_only")

  # Create only an Excel file matching the Project.xlsx pattern
  file.create(file.path(tempDir, "Project.xlsx"))
  expect_true(isProjectInitialized(tempDir))

  # Empty directory containing an unrelated xlsx is not a project
  unlink(file.path(tempDir, "Project.xlsx"))
  file.create(file.path(tempDir, "Other.xlsx"))
  expect_false(isProjectInitialized(tempDir))
})

test_that("initProject with overwrite = TRUE doesn't ask for permission", {
  temp_dir <- withr::local_tempdir("test_init_overwrite")

  initProject(destination = temp_dir, overwrite = TRUE)
  expect_true(isProjectInitialized(temp_dir))

  # Initialize again with overwrite = TRUE — should not ask for permission
  initProject(destination = temp_dir, overwrite = TRUE)
  expect_true(isProjectInitialized(temp_dir))
})

test_that("initProject creates proper project structure", {
  temp_dir <- withr::local_tempdir("test_init_structure")

  initProject(destination = temp_dir, overwrite = TRUE)

  # JSON should exist (copied from Blank project template)
  expect_true(file.exists(file.path(temp_dir, "Project.json")))

  # Excel files should be generated from JSON
  expect_true(file.exists(file.path(temp_dir, "Project.xlsx")))
  expect_true(dir.exists(file.path(temp_dir, "Configurations")))

  # Directory structure should exist
  expect_true(dir.exists(file.path(temp_dir, "Models", "Simulations")))
  expect_true(dir.exists(file.path(temp_dir, "Data")))
  expect_true(dir.exists(file.path(temp_dir, "Results", "Figures")))
  expect_true(dir.exists(file.path(temp_dir, "Results", "SimulationResults")))
  expect_true(dir.exists(file.path(temp_dir, "Populations")))
})

test_that("initProject creates project from Blank template with no scenarios", {
  temp_dir <- withr::local_tempdir("test_init_blank")

  initProject(destination = temp_dir, overwrite = TRUE)

  # The generated project should be loadable
  project <- loadProject(file.path(temp_dir, "Project.json"))
  expect_s3_class(project, "Project")

  # Blank project should have no scenarios
  expect_equal(length(project$scenarios), 0)
})

test_that("exampleProjectPath points to Example project", {
  path <- exampleProjectPath()
  expect_true(grepl("Example", path))
  expect_true(file.exists(path))
})


# loadProject auto cross-reference warnings ----

test_that("loadProject warns when scenarios reference undefined cross-references", {
  jsonPath <- testProjectJSONPath()
  raw <- jsonlite::fromJSON(jsonPath, simplifyVector = FALSE)
  raw$scenarios[[1]]$individualId <- "DoesNotExist"
  tmp <- file.path(tempfile(), "Project.json")
  dir.create(dirname(tmp), recursive = TRUE)
  jsonlite::write_json(raw, tmp, auto_unbox = TRUE, null = "null", pretty = TRUE)
  withr::defer(unlink(dirname(tmp), recursive = TRUE))

  # Other path-resolution warnings may also fire — assert ours specifically.
  warns <- character()
  withCallingHandlers(
    loadProject(tmp),
    warning = function(w) {
      warns <<- c(warns, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_true(any(grepl("cross-reference", warns)))
})

test_that("loadProject does not warn on a clean project", {
  jsonPath <- testProjectJSONPath()
  warns <- character()
  withCallingHandlers(
    loadProject(jsonPath),
    warning = function(w) {
      warns <<- c(warns, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_false(any(grepl("cross-reference", warns)))
})
