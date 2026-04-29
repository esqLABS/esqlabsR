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

test_that("Project parses plots into data.frames", {
  project <- testProject()
  expect_s3_class(project$plots$dataCombined, "data.frame")
  expect_equal(nrow(project$plots$dataCombined), 4)
  expect_s3_class(project$plots$plotConfiguration, "data.frame")
  expect_equal(nrow(project$plots$plotConfiguration), 4)
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
  expect_match(output_text, "4 dataCombined")
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

test_that(".parseNestedDataCombined converts nested structure to flat data.frame", {
  nested <- list(
    list(
      name = "DC1",
      simulated = list(
        list(
          label = "Sim1",
          scenario = "Scenario1",
          path = "Path1",
          group = "Group1",
          xOffsets = NULL,
          xOffsetsUnits = NULL,
          yOffsets = NULL,
          yOffsetsUnits = NULL,
          xScaleFactors = NULL,
          yScaleFactors = NULL
        )
      ),
      observed = list(
        list(
          label = "Obs1",
          dataSet = "DataSet1",
          group = "Group1",
          xOffsets = 1,
          xOffsetsUnits = "h",
          yOffsets = NULL,
          yOffsetsUnits = NULL,
          xScaleFactors = NULL,
          yScaleFactors = NULL
        )
      )
    )
  )

  result <- .parseNestedDataCombined(nested)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)
  expect_equal(result$DataCombinedName, c("DC1", "DC1"))
  expect_equal(result$dataType, c("simulated", "observed"))
  expect_equal(result$label, c("Sim1", "Obs1"))
  expect_equal(result$scenario, c("Scenario1", NA))
  expect_equal(result$path, c("Path1", NA))
  expect_equal(result$dataSet, c(NA, "DataSet1"))
  expect_equal(result$xOffsets, c(NA, 1))
  expect_equal(result$xOffsetsUnits, c(NA, "h"))
})

test_that(".parseNestedDataCombined handles empty input", {
  expect_equal(nrow(.parseNestedDataCombined(NULL)), 0)
  expect_equal(nrow(.parseNestedDataCombined(list())), 0)
})

test_that(".dataCombinedToNestedJson converts flat data.frame to nested structure", {
  df <- data.frame(
    DataCombinedName = c("DC1", "DC1", "DC2"),
    dataType = c("simulated", "observed", "simulated"),
    label = c("Sim1", "Obs1", "Sim2"),
    scenario = c("Scenario1", NA, "Scenario2"),
    path = c("Path1", NA, "Path2"),
    dataSet = c(NA, "DataSet1", NA),
    group = c("Group1", "Group1", "Group2"),
    xOffsets = c(NA, 1, NA),
    xOffsetsUnits = c(NA, "h", NA),
    yOffsets = c(NA, NA, NA),
    yOffsetsUnits = c(NA, NA, NA),
    xScaleFactors = c(NA, NA, NA),
    yScaleFactors = c(NA, NA, NA),
    stringsAsFactors = FALSE
  )

  result <- .dataCombinedToNestedJson(df)

  expect_length(result, 2)
  expect_equal(result[[1]]$name, "DC1")
  expect_length(result[[1]]$simulated, 1)
  expect_length(result[[1]]$observed, 1)
  expect_equal(result[[1]]$simulated[[1]]$label, "Sim1")
  expect_equal(result[[1]]$simulated[[1]]$scenario, "Scenario1")
  expect_equal(result[[1]]$observed[[1]]$dataSet, "DataSet1")
  expect_equal(result[[1]]$observed[[1]]$xOffsets, 1)
  expect_equal(result[[2]]$name, "DC2")
  expect_length(result[[2]]$simulated, 1)
  expect_length(result[[2]]$observed, 0)
})

test_that(".dataCombinedToNestedJson handles empty input", {
  expect_equal(.dataCombinedToNestedJson(NULL), list())
  expect_equal(.dataCombinedToNestedJson(data.frame()), list())
})

test_that("dataCombined round-trip: nested JSON -> flat df -> nested JSON", {
  original <- list(
    list(
      name = "DC1",
      simulated = list(
        list(
          label = "Sim1",
          scenario = "Scenario1",
          path = "Path1",
          group = "Group1",
          xOffsets = NULL,
          xOffsetsUnits = NULL,
          yOffsets = NULL,
          yOffsetsUnits = NULL,
          xScaleFactors = NULL,
          yScaleFactors = NULL
        )
      ),
      observed = list(
        list(
          label = "Obs1",
          dataSet = "DataSet1",
          group = "Group1",
          xOffsets = 1,
          xOffsetsUnits = "h",
          yOffsets = NULL,
          yOffsetsUnits = NULL,
          xScaleFactors = NULL,
          yScaleFactors = NULL
        )
      )
    )
  )

  flat <- .parseNestedDataCombined(original)
  roundtrip <- .dataCombinedToNestedJson(flat)

  expect_equal(roundtrip[[1]]$name, original[[1]]$name)
  expect_equal(
    roundtrip[[1]]$simulated[[1]]$label,
    original[[1]]$simulated[[1]]$label
  )
  expect_equal(
    roundtrip[[1]]$simulated[[1]]$scenario,
    original[[1]]$simulated[[1]]$scenario
  )
  expect_equal(
    roundtrip[[1]]$observed[[1]]$dataSet,
    original[[1]]$observed[[1]]$dataSet
  )
  expect_equal(
    roundtrip[[1]]$observed[[1]]$xOffsets,
    original[[1]]$observed[[1]]$xOffsets
  )
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

test_that("individual inline parameters round-trip through saveProject/loadProject", {
  project <- Project$new()
  project$modelFolder <- tempdir()
  addIndividual(
    project,
    "RT",
    species = "Human",
    weight = 70,
    height = 175,
    age = 30,
    parameters = list(
      list(
        containerPath = "Organism|Liver",
        parameterName = "Volume",
        value = 1.8,
        units = "L"
      ),
      list(
        containerPath = "Organism|Kidney",
        parameterName = "GFR",
        value = 90,
        units = "ml/min"
      )
    )
  )
  tmp <- tempfile(fileext = ".json")
  saveProject(project, tmp)
  reloaded <- loadProject(tmp)
  pset <- reloaded$individuals[["RT"]]$parameters
  expect_equal(pset$paths, c("Organism|Liver|Volume", "Organism|Kidney|GFR"))
  expect_equal(pset$values, c(1.8, 90))
  expect_equal(pset$units, c("L", "ml/min"))
})

test_that("individual without parameters round-trips with parameters = NULL", {
  project <- Project$new()
  project$modelFolder <- tempdir()
  addIndividual(project, "RT_NoParam", species = "Human")
  tmp <- tempfile(fileext = ".json")
  saveProject(project, tmp)
  reloaded <- loadProject(tmp)
  expect_null(reloaded$individuals[["RT_NoParam"]]$parameters)
})

test_that("loaded JSON with individual parameters: array gets parsed correctly", {
  json <- '{
    "schemaVersion": "2.0",
    "esqlabsRVersion": "6.0.0",
    "individuals": [{
      "individualId": "JsonI",
      "species": "Human",
      "parameters": [
        {"containerPath": "Organism|Liver", "parameterName": "Volume",
         "value": 1.8, "units": "L"}
      ]
    }]
  }'
  tmp <- tempfile(fileext = ".json")
  writeLines(json, tmp)
  project <- loadProject(tmp)
  pset <- project$individuals[["JsonI"]]$parameters
  expect_equal(pset$paths, "Organism|Liver|Volume")
  expect_equal(pset$values, 1.8)
  expect_equal(pset$units, "L")
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

test_that("saveProject produces round-trip fidelity for scenarios", {
  project <- testProject()
  tmp <- tempfile(fileext = ".json")
  withr::defer(unlink(tmp))

  saveProject(project, tmp)
  pc2 <- loadProject(tmp)

  expect_equal(names(project$scenarios), names(pc2$scenarios))

  sc1 <- project$scenarios[["TestScenario2"]]
  sc2 <- pc2$scenarios[["TestScenario2"]]
  expect_equal(sc1$scenarioName, sc2$scenarioName)
  expect_equal(sc1$modelFile, sc2$modelFile)
  expect_equal(sc1$individualId, sc2$individualId)
  expect_equal(sc1$applicationProtocol, sc2$applicationProtocol)
  expect_equal(sc1$simulateSteadyState, sc2$simulateSteadyState)
  expect_equal(sc1$modelParameters, sc2$modelParameters)
})

test_that("saveProject produces round-trip fidelity for modelParameters", {
  project <- testProject()
  tmp <- tempfile(fileext = ".json")
  withr::defer(unlink(tmp))

  saveProject(project, tmp)
  pc2 <- loadProject(tmp)

  expect_equal(names(project$modelParameters), names(pc2$modelParameters))
  expect_equal(
    project$modelParameters[["Global"]],
    pc2$modelParameters[["Global"]]
  )
})

test_that("saveProject produces round-trip fidelity for individuals", {
  project <- testProject()
  tmp <- tempfile(fileext = ".json")
  withr::defer(unlink(tmp))

  saveProject(project, tmp)
  pc2 <- loadProject(tmp)

  expect_equal(names(project$individuals), names(pc2$individuals))
  indiv1 <- project$individuals[["Indiv1"]]
  indiv2 <- pc2$individuals[["Indiv1"]]
  expect_equal(indiv1$species, indiv2$species)
  expect_equal(indiv1$weight, indiv2$weight)
})

test_that("saveProject produces round-trip fidelity for populations", {
  project <- testProject()
  tmp <- tempfile(fileext = ".json")
  withr::defer(unlink(tmp))

  saveProject(project, tmp)
  pc2 <- loadProject(tmp)

  expect_equal(names(project$populations), names(pc2$populations))
  pop1 <- project$populations[["TestPopulation"]]
  pop2 <- pc2$populations[["TestPopulation"]]
  expect_equal(pop1$species, pop2$species)
  expect_equal(pop1$numberOfIndividuals, pop2$numberOfIndividuals)
})

test_that("saveProject produces round-trip fidelity for applications", {
  project <- testProject()
  tmp <- tempfile(fileext = ".json")
  withr::defer(unlink(tmp))

  saveProject(project, tmp)
  pc2 <- loadProject(tmp)

  expect_equal(names(project$applications), names(pc2$applications))
  expect_equal(
    project$applications[["Aciclovir_iv_250mg"]],
    pc2$applications[["Aciclovir_iv_250mg"]]
  )
})

test_that("saveProject produces round-trip fidelity for outputPaths", {
  project <- testProject()
  tmp <- tempfile(fileext = ".json")
  withr::defer(unlink(tmp))

  saveProject(project, tmp)
  pc2 <- loadProject(tmp)

  expect_equal(project$outputPaths, pc2$outputPaths)
})

test_that("saveProject produces round-trip fidelity for plots", {
  project <- testProject()
  tmp <- tempfile(fileext = ".json")
  withr::defer(unlink(tmp))

  saveProject(project, tmp)
  pc2 <- loadProject(tmp)

  expect_equal(nrow(project$plots$dataCombined), nrow(pc2$plots$dataCombined))
  expect_equal(
    nrow(project$plots$plotConfiguration),
    nrow(pc2$plots$plotConfiguration)
  )
  expect_equal(nrow(project$plots$plotGrids), nrow(pc2$plots$plotGrids))
})

test_that("saveProject resets modified flag to FALSE", {
  project <- testProject()
  project$.markModified()
  tmp <- tempfile(fileext = ".json")
  withr::defer(unlink(tmp))

  saveProject(project, tmp)

  expect_false(project$modified)
})

test_that("saveProject persists scenario added via addScenario", {
  tmpDir <- withr::local_tempdir()
  jsonPath <- file.path(tmpDir, "Project.json")
  file.copy(testProjectJSONPath(), jsonPath)

  project <- loadProject(jsonPath)
  addScenario(
    project = project,
    scenarioName = "ProgrammaticScenario",
    modelFile = "Aciclovir.pkml",
    individualId = "Indiv1",
    applicationProtocol = "Aciclovir_iv_250mg"
  )

  saveProject(project, jsonPath)

  pc2 <- loadProject(jsonPath)
  expect_true("ProgrammaticScenario" %in% names(pc2$scenarios))
  expect_equal(
    pc2$scenarios[["ProgrammaticScenario"]]$individualId,
    "Indiv1"
  )
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
test_that("addApplication adds an application object with empty parameters", {
  project <- testProject()
  initial <- length(project$applications)
  addApplication(project, "Oral_10mg")
  expect_equal(length(project$applications), initial + 1)
  expect_true(inherits(project$applications[["Oral_10mg"]], "Application"))
  expect_null(project$applications[["Oral_10mg"]]$parameters)
  expect_true(project$modified)
})

test_that("addApplication errors on duplicate id", {
  project <- testProject()
  existing <- names(project$applications)[[1]]
  expect_error(
    addApplication(project, existing),
    regexp = "already exists"
  )
})

test_that("addApplicationParameter appends to an application", {
  project <- testProject()
  addApplication(project, "OralX")
  addApplicationParameter(
    project,
    "OralX",
    containerPath = "Events|Oral|Schema",
    parameterName = "Dose",
    value = 250,
    units = "mg"
  )
  pset <- project$applications[["OralX"]]$parameters
  expect_equal(pset$paths, "Events|Oral|Schema|Dose")
  expect_equal(pset$values, 250)
  expect_equal(pset$units, "mg")
  expect_true(inherits(project$applications[["OralX"]], "Application"))
})

test_that("addApplicationParameter errors on unknown application id", {
  project <- testProject()
  expect_error(
    addApplicationParameter(
      project,
      "NoSuchApp_QQ",
      containerPath = "a",
      parameterName = "x",
      value = 1,
      units = "mg"
    ),
    regexp = "not found"
  )
})

test_that("removeApplication removes the application", {
  project <- testProject()
  addApplication(project, "TmpApp")
  project$.markSaved()
  removeApplication(project, "TmpApp")
  expect_false("TmpApp" %in% names(project$applications))
  expect_true(project$modified)
})

test_that("removeApplicationParameter drops the entry", {
  project <- testProject()
  addApplication(project, "AppRm")
  addApplicationParameter(
    project,
    "AppRm",
    containerPath = "Events|Oral|Schema",
    parameterName = "Dose",
    value = 250,
    units = "mg"
  )
  removeApplicationParameter(
    project,
    "AppRm",
    containerPath = "Events|Oral|Schema",
    parameterName = "Dose"
  )
  expect_null(project$applications[["AppRm"]]$parameters)
})

test_that("application round-trips with parameters", {
  project <- Project$new()
  project$modelFolder <- tempdir()
  addApplication(project, "OralRT")
  addApplicationParameter(
    project,
    "OralRT",
    containerPath = "Events|Oral|Schema",
    parameterName = "Dose",
    value = 100,
    units = "mg"
  )
  tmp <- tempfile(fileext = ".json")
  saveProject(project, tmp)
  reloaded <- loadProject(tmp)
  pset <- reloaded$applications[["OralRT"]]$parameters
  expect_equal(pset$paths, "Events|Oral|Schema|Dose")
  expect_equal(pset$values, 100)
  expect_equal(pset$units, "mg")
  expect_true(inherits(reloaded$applications[["OralRT"]], "Application"))
})

test_that("loaded JSON with applications object shape parses correctly", {
  json <- '{
    "schemaVersion": "2.0",
    "esqlabsRVersion": "6.0.0",
    "applications": {
      "OralFromJson": {
        "parameters": [
          {"containerPath": "Events|Oral|Schema", "parameterName": "Dose",
           "value": 50, "units": "mg"}
        ]
      }
    }
  }'
  tmp <- tempfile(fileext = ".json")
  writeLines(json, tmp)
  project <- loadProject(tmp)
  pset <- project$applications[["OralFromJson"]]$parameters
  expect_equal(pset$paths, "Events|Oral|Schema|Dose")
  expect_equal(pset$values, 50)
})

test_that("project$addApplication and addApplicationParameter delegate", {
  pc1 <- testProject()
  pc2 <- testProject()
  addApplication(pc1, "DM")
  pc2$addApplication("DM")
  addApplicationParameter(
    pc1,
    "DM",
    containerPath = "a",
    parameterName = "x",
    value = 1,
    units = "mg"
  )
  pc2$addApplicationParameter(
    "DM",
    containerPath = "a",
    parameterName = "x",
    value = 1,
    units = "mg"
  )
  expect_equal(
    pc1$applications[["DM"]]$parameters,
    pc2$applications[["DM"]]$parameters
  )
})
test_that("addIndividual adds entry with required fields", {
  project <- testProject()
  initial <- length(project$individuals)
  addIndividual(project, "NewIndiv", species = "Human")
  expect_equal(length(project$individuals), initial + 1)
  expect_equal(project$individuals[["NewIndiv"]]$species, "Human")
  expect_true(project$modified)
})

test_that("addIndividual accepts optional fields via ...", {
  project <- testProject()
  addIndividual(
    project,
    "Indiv2",
    species = "Human",
    population = "European_ICRP_2002",
    gender = "Male",
    weight = 70,
    height = 175,
    age = 30
  )
  entry <- project$individuals[["Indiv2"]]
  expect_equal(entry$weight, 70)
  expect_equal(entry$height, 175)
  expect_equal(entry$age, 30)
  expect_equal(entry$gender, "Male")
})

test_that("addIndividual coerces numeric fields", {
  project <- testProject()
  addIndividual(project, "I3", species = "Human", weight = "70", height = "175")
  expect_identical(project$individuals[["I3"]]$weight, 70)
  expect_identical(project$individuals[["I3"]]$height, 175)
})

test_that("addIndividual errors on duplicate id", {
  project <- testProject()
  existing <- names(project$individuals)[[1]]
  expect_error(
    addIndividual(project, existing, species = "Human"),
    regexp = "already exists"
  )
})

test_that("addIndividual errors on unknown ... fields", {
  project <- testProject()
  expect_error(
    addIndividual(project, "IBad", species = "Human", banana = 1, kiwi = 2),
    regexp = "banana.*kiwi|kiwi.*banana"
  )
})

test_that("addIndividual errors on empty species", {
  project <- testProject()
  expect_error(
    addIndividual(project, "IEmpty", species = ""),
    regexp = "species"
  )
})

test_that("addIndividual returns project invisibly", {
  project <- testProject()
  out <- withVisible(addIndividual(project, "IRet", species = "Human"))
  expect_false(out$visible)
  expect_identical(out$value, project)
})

test_that("removeIndividual removes entry and sets modified", {
  project <- testProject()
  addIndividual(project, "ToGo", species = "Human")
  project$.markSaved()
  removeIndividual(project, "ToGo")
  expect_false("ToGo" %in% names(project$individuals))
  expect_true(project$modified)
})

test_that("removeIndividual warns on missing id and does not change project", {
  project <- testProject()
  before <- project$individuals
  expect_warning(
    removeIndividual(project, "NoSuchIndiv_QQ"),
    regexp = "not found"
  )
  expect_identical(project$individuals, before)
})

test_that("removeIndividual warns when referenced by a scenario", {
  project <- testProject()
  # Indiv1 is referenced by scenarios in TestProject
  expect_warning(
    removeIndividual(project, "Indiv1"),
    regexp = "referenced"
  )
  expect_false("Indiv1" %in% names(project$individuals))
})

test_that("project$addIndividual delegates to standalone", {
  pc1 <- testProject()
  pc2 <- testProject()
  addIndividual(pc1, "Method", species = "Human", weight = 60)
  pc2$addIndividual("Method", species = "Human", weight = 60)
  expect_equal(pc1$individuals[["Method"]], pc2$individuals[["Method"]])
})

test_that("addIndividual survives saveProject/loadProject round-trip", {
  project <- testProject()
  addIndividual(
    project,
    "RTrip",
    species = "Human",
    weight = 65,
    height = 170,
    age = 40
  )
  tmp <- tempfile(fileext = ".json")
  saveProject(project, tmp)
  reloaded <- loadProject(tmp)
  entry <- reloaded$individuals[["RTrip"]]
  expect_equal(entry$species, "Human")
  expect_equal(entry$weight, 65)
  expect_equal(entry$height, 170)
  expect_equal(entry$age, 40)
})

test_that("addIndividualParameter adds an entry to a named individual", {
  project <- testProject()
  addIndividual(project, "I_param", species = "Human")
  project$.markSaved()
  addIndividualParameter(
    project,
    "I_param",
    containerPath = "Organism|Liver",
    parameterName = "Volume",
    value = 1.8,
    units = "L"
  )
  expect_equal(
    project$individuals[["I_param"]]$parameters$paths,
    "Organism|Liver|Volume"
  )
  expect_equal(project$individuals[["I_param"]]$parameters$values, 1.8)
  expect_true(project$modified)
})

test_that("addIndividualParameter errors on unknown individualId", {
  project <- testProject()
  expect_error(
    addIndividualParameter(
      project,
      "NoSuchIndiv_QQ",
      containerPath = "a",
      parameterName = "x",
      value = 1,
      units = "L"
    ),
    regexp = "not found|does not exist"
  )
})

test_that("addIndividualParameter preserves S3 class on the individual", {
  project <- testProject()
  addIndividual(project, "I_class", species = "Human")
  addIndividualParameter(
    project,
    "I_class",
    containerPath = "a",
    parameterName = "x",
    value = 1,
    units = "L"
  )
  expect_true(inherits(project$individuals[["I_class"]], "Individual"))
})

test_that("removeIndividualParameter drops the entry", {
  project <- testProject()
  addIndividual(project, "I_rm", species = "Human")
  addIndividualParameter(
    project,
    "I_rm",
    containerPath = "Organism|Liver",
    parameterName = "Volume",
    value = 1.8,
    units = "L"
  )
  project$.markSaved()
  removeIndividualParameter(
    project,
    "I_rm",
    containerPath = "Organism|Liver",
    parameterName = "Volume"
  )
  expect_null(project$individuals[["I_rm"]]$parameters)
  expect_true(project$modified)
})

test_that("removeIndividualParameter warns when entry not found", {
  project <- testProject()
  addIndividual(project, "I_nf", species = "Human")
  expect_warning(
    removeIndividualParameter(
      project,
      "I_nf",
      containerPath = "Organism|Liver",
      parameterName = "Volume"
    ),
    regexp = "not found"
  )
})

test_that("addIndividual accepts an inline parameters arg", {
  project <- testProject()
  addIndividual(
    project,
    "I_inline",
    species = "Human",
    parameters = list(
      list(
        containerPath = "Organism|Liver",
        parameterName = "Volume",
        value = 1.8,
        units = "L"
      ),
      list(
        containerPath = "Organism|Kidney",
        parameterName = "GFR",
        value = 90,
        units = "ml/min"
      )
    )
  )
  pset <- project$individuals[["I_inline"]]$parameters
  expect_equal(pset$paths, c("Organism|Liver|Volume", "Organism|Kidney|GFR"))
  expect_equal(pset$values, c(1.8, 90))
})

test_that("project$addIndividualParameter delegates to standalone", {
  pc1 <- testProject()
  pc2 <- testProject()
  addIndividual(pc1, "X", species = "Human")
  addIndividual(pc2, "X", species = "Human")
  addIndividualParameter(
    pc1,
    "X",
    containerPath = "a",
    parameterName = "x",
    value = 1,
    units = "L"
  )
  pc2$addIndividualParameter(
    "X",
    containerPath = "a",
    parameterName = "x",
    value = 1,
    units = "L"
  )
  expect_equal(
    pc1$individuals[["X"]]$parameters,
    pc2$individuals[["X"]]$parameters
  )
})
# ---- importProjectFromExcel tests ----

test_that("importProjectFromExcel creates v2.0 JSON from Excel files", {
  paths <- local_test_project()

  outputDir <- withr::local_tempdir("test_import")

  jsonPath <- importProjectFromExcel(
    paths$project_config_path,
    outputDir,
    silent = TRUE
  )

  expect_true(file.exists(jsonPath))

  jsonData <- jsonlite::fromJSON(jsonPath, simplifyVector = FALSE)
  expect_equal(jsonData$schemaVersion, "2.0")
  expect_true("filePaths" %in% names(jsonData))
  expect_true("scenarios" %in% names(jsonData))
  expect_true("modelParameters" %in% names(jsonData))
  expect_true("individuals" %in% names(jsonData))
  expect_true("populations" %in% names(jsonData))
  expect_true("applications" %in% names(jsonData))
  expect_true("outputPaths" %in% names(jsonData))
  expect_true("plots" %in% names(jsonData))

  # Check v2.0 schema structure
  expect_true(is.list(jsonData$scenarios))
  expect_true(length(jsonData$scenarios) > 0)
  expect_true(!is.null(jsonData$scenarios[[1]]$name))

  # modelParameters should be named lists of arrays
  expect_true(length(jsonData$modelParameters) > 0)
  firstSheet <- jsonData$modelParameters[[1]]
  expect_true(is.list(firstSheet))
  expect_true(!is.null(firstSheet[[1]]$containerPath))
})

test_that("importProjectFromExcel creates JSON in source dir by default", {
  paths <- local_test_project()

  jsonPath <- importProjectFromExcel(
    paths$project_config_path,
    silent = TRUE
  )

  expectedPath <- sub("\\.xlsx$", ".json", paths$project_config_path)
  expect_equal(normalizePath(jsonPath), normalizePath(expectedPath))
  expect_true(file.exists(jsonPath))
})

test_that("importProjectFromExcel JSON can be loaded by Project", {
  paths <- local_test_project()

  outputDir <- withr::local_tempdir("test_loadable")

  jsonPath <- importProjectFromExcel(
    paths$project_config_path,
    outputDir,
    silent = TRUE
  )

  expect_no_error({
    project <- loadProject(jsonPath)
  })

  expect_s3_class(project, "Project")
  expect_true(length(project$scenarios) > 0)
  expect_true(length(project$modelParameters) > 0)
  expect_true(length(project$individuals) > 0)
})

test_that("importProjectFromExcel does not merge bundled species parameters into modelParameters", {
  temp_project <- local_test_project()
  jsonPath <- importProjectFromExcel(
    temp_project$project_config_path,
    silent = TRUE
  )
  jsonData <- jsonlite::fromJSON(jsonPath, simplifyVector = FALSE)

  speciesFile <- system.file(
    "extdata",
    "SpeciesParameters.xlsx",
    package = "esqlabsR"
  )
  speciesSheets <- readxl::excel_sheets(speciesFile)

  # No bundled species sheet should appear in modelParameters
  speciesInModel <- intersect(speciesSheets, names(jsonData$modelParameters))
  expect_length(speciesInModel, 0)
})

# ---- exportProjectToExcel tests ----

test_that("exportProjectToExcel creates Excel files from Project", {
  project <- testProject()

  outputDir <- withr::local_tempdir("test_export")

  projConfigPath <- exportProjectToExcel(
    project,
    outputDir = outputDir,
    silent = TRUE
  )

  # Check that Project.xlsx was created
  expect_true(file.exists(projConfigPath))

  # Check configurations directory was created
  configDir <- file.path(outputDir, "Configurations")
  expect_true(dir.exists(configDir))

  # Check key Excel files were created
  expect_true(file.exists(file.path(configDir, "ModelParameters.xlsx")))
  expect_true(file.exists(file.path(configDir, "Scenarios.xlsx")))
  expect_true(file.exists(file.path(configDir, "Individuals.xlsx")))
  expect_true(file.exists(file.path(configDir, "Applications.xlsx")))
})

test_that("exportProjectToExcel works with plain-data individuals and scenarios", {
  project <- testProject()
  tempDir <- withr::local_tempdir()
  exportProjectToExcel(project, outputDir = tempDir, silent = TRUE)

  # Check that key files were created
  expect_true(file.exists(file.path(
    tempDir,
    "Configurations",
    "Scenarios.xlsx"
  )))
  expect_true(file.exists(file.path(
    tempDir,
    "Configurations",
    "Individuals.xlsx"
  )))
})

test_that("exportProjectToExcel preserves model parameters", {
  project <- testProject()

  outputDir <- withr::local_tempdir("test_export_params")
  exportProjectToExcel(project, outputDir = outputDir, silent = TRUE)

  # Read back the ModelParameters.xlsx
  paramsFile <- file.path(outputDir, "Configurations", "ModelParameters.xlsx")
  sheets <- readxl::excel_sheets(paramsFile)
  expect_true("Global" %in% sheets)

  globalDf <- readExcel(paramsFile, sheet = "Global")
  expect_true("Container Path" %in% names(globalDf))
  expect_true("Parameter Name" %in% names(globalDf))
  expect_true("Value" %in% names(globalDf))
})

# ---- projectStatus tests ----

test_that("projectStatus correctly handles missing JSON file", {
  test_proj <- local_test_project()

  json_path <- file.path(test_proj$dir, "NonExistent.json")
  expect_error(
    projectStatus(test_proj$project_config_path, json_path),
    "JSON file does not exist"
  )
})

test_that("projectStatus detects in-sync state after fresh import", {
  test_proj <- local_test_project()

  # Import fresh JSON from Excel
  importProjectFromExcel(
    test_proj$project_config_path,
    silent = TRUE
  )

  jsonPath <- sub("\\.xlsx$", ".json", test_proj$project_config_path)
  expect_true(file.exists(jsonPath))

  status_result <- projectStatus(
    test_proj$project_config_path,
    jsonPath,
    silent = TRUE
  )
  expect_true(status_result$in_sync)
})

test_that("projectStatus detects out-of-sync state when JSON is modified", {
  test_proj <- local_test_project()

  # Import fresh JSON from Excel to ensure a baseline in-sync state
  importProjectFromExcel(
    test_proj$project_config_path,
    silent = TRUE
  )

  jsonPath <- sub("\\.xlsx$", ".json", test_proj$project_config_path)
  expect_true(file.exists(jsonPath))

  # Modify the JSON to make it out of sync with Excel
  jsonData <- jsonlite::fromJSON(jsonPath, simplifyVector = FALSE)
  original_name <- jsonData$scenarios[[1]]$name
  jsonData$scenarios[[1]]$name <- paste0(original_name, "_MODIFIED")
  writeLines(
    jsonlite::toJSON(jsonData, auto_unbox = TRUE, pretty = TRUE),
    jsonPath
  )

  status_result <- projectStatus(
    test_proj$project_config_path,
    jsonPath,
    silent = TRUE
  )
  expect_false(status_result$in_sync)
})

# ---- Deprecated wrapper tests ----

test_that("snapshotProjectConfiguration emits deprecation warning", {
  paths <- local_test_project()
  outputDir <- withr::local_tempdir("test_deprecated_snapshot")

  lifecycle::expect_deprecated(
    snapshotProjectConfiguration(
      paths$project_config_path,
      outputDir,
      silent = TRUE
    )
  )
})

test_that("restoreProjectConfiguration emits deprecation warning", {
  paths <- local_test_project()

  lifecycle::expect_deprecated(
    restoreProjectConfiguration(
      paths$snapshot_path,
      outputDir = withr::local_tempdir("test_deprecated_restore"),
      silent = TRUE
    )
  )
})

test_that("createProjectConfiguration emits deprecation warning", {
  project <- testProject()
  lifecycle::expect_deprecated(
    createProjectConfiguration(project$projectFilePath)
  )
})

# Keep test for PK-Sim CSV handling - valuable edge case
test_that("importProjectFromExcel handles PK-Sim exported population CSV files with metadata comment rows", {
  test_proj <- local_test_project()

  # Create PopulationsCSV directory if needed
  pop_csv_dir <- file.path(test_proj$configurations_dir, "PopulationsCSV")
  if (!dir.exists(pop_csv_dir)) {
    dir.create(pop_csv_dir, recursive = TRUE)
  }

  # Add a PK-Sim format CSV (with # metadata comment rows)
  pksim_csv_path <- file.path(pop_csv_dir, "PKSimPopulation.csv")

  # Create PK-Sim format CSV with metadata comment rows
  pksim_csv_lines <- c(
    "#Project: TestProject_V1",
    "#PK-Sim version: 12.1.222",
    "IndividualId,Gender,Organism|Weight",
    "1,Male,70",
    "2,Female,60"
  )
  writeLines(pksim_csv_lines, pksim_csv_path)

  # Import should succeed without error
  outputDir <- withr::local_tempdir("test_pksim_csv")
  expect_no_error({
    jsonPath <- importProjectFromExcel(
      test_proj$project_config_path,
      outputDir,
      silent = TRUE
    )
  })
})

# ---- internal helpers ----

test_that(".parseCommaListToArray handles values containing apostrophes", {
  expect_equal(
    .parseCommaListToArray("5'-Reductase, CYP3A4"),
    c("5'-Reductase", "CYP3A4")
  )
})

test_that(".parseCommaListToArray returns NULL on empty/NA input", {
  expect_null(.parseCommaListToArray(NULL))
  expect_null(.parseCommaListToArray(NA))
  expect_null(.parseCommaListToArray(""))
})

test_that(".parseCommaListToArray trims whitespace", {
  expect_equal(.parseCommaListToArray(" a , b ,c"), c("a", "b", "c"))
})

test_that(".naToNull leaves multi-element vectors unchanged", {
  expect_equal(.naToNull(c("a", "b")), c("a", "b"))
  expect_equal(.naToNull(c(1, NA)), c(1, NA))
})

test_that(".naToNull returns NULL for scalar NA", {
  expect_null(.naToNull(NA))
  expect_null(.naToNull(NA_character_))
  expect_null(.naToNull(NA_real_))
})
test_that("addObservedData with DataSet adds entry and stores DataSet by name", {
  project <- testProject()
  ds <- ospsuite::DataSet$new(name = "ProgrammaticData")
  ds$setValues(xValues = 1:3, yValues = 1:3)
  initialCount <- length(project$observedData)
  expect_message(
    project$addObservedData(ds),
    regexp = "reproducibility"
  )
  expect_equal(length(project$observedData), initialCount + 1)
  newEntry <- project$observedData[[length(project$observedData)]]
  expect_equal(newEntry$type, "programmatic")
  expect_equal(newEntry$name, "ProgrammaticData")
  expect_equal(project$.getProgrammaticDataSets()[["ProgrammaticData"]], ds)
})

test_that("addObservedData with config list adds entry directly", {
  project <- testProject()
  initialCount <- length(project$observedData)
  config <- list(
    type = "script",
    file = "scripts/generate_data.R"
  )
  project$addObservedData(config)
  expect_equal(length(project$observedData), initialCount + 1)
  newEntry <- project$observedData[[length(project$observedData)]]
  expect_equal(newEntry$type, "script")
  expect_equal(newEntry$file, "scripts/generate_data.R")
})

test_that("addObservedData with config list validates type", {
  project <- testProject()
  expect_error(
    project$addObservedData(list(type = "invalid", file = "test.xlsx")),
    regexp = "Invalid type"
  )
  expect_error(
    project$addObservedData(list(file = "test.xlsx")),
    regexp = "must include.*type"
  )
})

test_that("loadObservedData merges programmatic and JSON-declared DataSets", {
  project <- testProject()
  ds <- ospsuite::DataSet$new(name = "ProgrammaticData")
  ds$setValues(xValues = 1:3, yValues = 1:3)
  suppressMessages(project$addObservedData(ds))
  result <- loadObservedData(project)
  expect_true("ProgrammaticData" %in% names(result))
  expect_equal(result[["ProgrammaticData"]], ds)
  expect_true(length(result) > 1)
})


test_that("addObservedData errors on duplicate DataSet name", {
  project <- testProject()
  ds1 <- ospsuite::DataSet$new(name = "SameName")
  ds1$setValues(xValues = 1:3, yValues = 1:3)
  ds2 <- ospsuite::DataSet$new(name = "SameName")
  ds2$setValues(xValues = 1:3, yValues = 4:6)
  suppressMessages(project$addObservedData(ds1))
  expect_error(
    suppressMessages(project$addObservedData(ds2)),
    regexp = "already exists"
  )
})

test_that("getObservedDataNames returns all DataSet names", {
  project <- testProject()
  names_before <- getObservedDataNames(project)
  expect_true(length(names_before) >= 1)

  ds <- ospsuite::DataSet$new(name = "NewData")
  ds$setValues(xValues = 1:3, yValues = 1:3)
  suppressMessages(project$addObservedData(ds))

  names_after <- getObservedDataNames(project)
  expect_true("NewData" %in% names_after)
  expect_equal(length(names_after), length(names_before) + 1)
})

test_that("addObservedData errors when name conflicts with existing loaded data", {
  project <- testProject()
  existing_name <- getObservedDataNames(project)[[1]]
  ds <- ospsuite::DataSet$new(name = existing_name)
  ds$setValues(xValues = 1:3, yValues = 1:3)
  expect_error(
    suppressMessages(project$addObservedData(ds)),
    regexp = "already exists"
  )
})

test_that("standalone addObservedData (DataSet) matches R6 method behavior", {
  pc1 <- testProject()
  pc2 <- testProject()
  ds1 <- ospsuite::DataSet$new(name = "StandaloneDS")
  ds1$setValues(xValues = 1:3, yValues = 1:3)
  ds2 <- ospsuite::DataSet$new(name = "StandaloneDS")
  ds2$setValues(xValues = 1:3, yValues = 1:3)

  suppressMessages(addObservedData(pc1, ds1))
  suppressMessages(pc2$addObservedData(ds2))

  expect_equal(length(pc1$observedData), length(pc2$observedData))
  expect_equal(
    names(pc1$.getProgrammaticDataSets()),
    names(pc2$.getProgrammaticDataSets())
  )
  expect_equal(
    pc1$.getProgrammaticDataSets()[["StandaloneDS"]]$name,
    pc2$.getProgrammaticDataSets()[["StandaloneDS"]]$name
  )
  expect_equal(
    pc1$.getProgrammaticDataSets()[["StandaloneDS"]]$xValues,
    pc2$.getProgrammaticDataSets()[["StandaloneDS"]]$xValues
  )
  expect_equal(
    pc1$.getProgrammaticDataSets()[["StandaloneDS"]]$yValues,
    pc2$.getProgrammaticDataSets()[["StandaloneDS"]]$yValues
  )
})

test_that("standalone addObservedData (config list) appends entry", {
  project <- testProject()
  initial <- length(project$observedData)
  addObservedData(project, list(type = "script", file = "scripts/x.R"))
  expect_equal(length(project$observedData), initial + 1)
  expect_true(project$modified)
})

test_that("standalone addObservedData returns project invisibly", {
  project <- testProject()
  out <- withVisible(
    addObservedData(project, list(type = "script", file = "scripts/y.R"))
  )
  expect_false(out$visible)
  expect_identical(out$value, project)
})

test_that("removeObservedData removes programmatic DataSet by name", {
  project <- testProject()
  ds <- ospsuite::DataSet$new(name = "ToRemove")
  ds$setValues(xValues = 1:3, yValues = 1:3)
  suppressMessages(addObservedData(project, ds))
  project$.markSaved()
  before <- length(project$observedData)
  removeObservedData(project, "ToRemove")
  expect_equal(length(project$observedData), before - 1)
  expect_false("ToRemove" %in% names(project$.getProgrammaticDataSets()))
  expect_true(project$modified)
})

test_that("removeObservedData removes config entry by file basename", {
  project <- testProject()
  addObservedData(project, list(type = "script", file = "scripts/delete-me.R"))
  before <- length(project$observedData)
  removeObservedData(project, "delete-me.R")
  expect_equal(length(project$observedData), before - 1)
})

test_that("removeObservedData warns on missing name", {
  project <- testProject()
  expect_warning(
    removeObservedData(project, "NoSuchDataSet_ZZZ"),
    regexp = "not found"
  )
})

test_that("removeObservedData removes the correct DataSet when multiple programmatic exist", {
  project <- testProject()
  dsA <- ospsuite::DataSet$new(name = "DS_A")
  dsA$setValues(xValues = 1:3, yValues = c(10, 20, 30))
  dsB <- ospsuite::DataSet$new(name = "DS_B")
  dsB$setValues(xValues = 1:3, yValues = c(100, 200, 300))

  suppressMessages(addObservedData(project, dsA))
  suppressMessages(addObservedData(project, dsB))

  # Remove A; B must still be reachable via loadObservedData
  removeObservedData(project, "DS_A")
  loaded <- loadObservedData(project)
  expect_false("DS_A" %in% names(loaded))
  expect_true("DS_B" %in% names(loaded))
  expect_equal(loaded[["DS_B"]]$yValues, c(100, 200, 300), tolerance = 1e-4)
})

test_that("addObservedData annotates programmatic sentinel with the DataSet name", {
  project <- testProject()
  ds <- ospsuite::DataSet$new(name = "Annotated")
  ds$setValues(xValues = 1:3, yValues = 1:3)
  suppressMessages(addObservedData(project, ds))

  # The programmatic sentinel in observedData should record the DataSet
  # name so the entry is identifiable when serialised or removed.
  programmaticEntries <- Filter(
    function(e) identical(e$type, "programmatic"),
    project$observedData
  )
  names_recorded <- vapply(
    programmaticEntries,
    function(e) e$name %||% NA_character_,
    character(1)
  )
  expect_true("Annotated" %in% names_recorded)
})

test_that("removeObservedData removes the named programmatic sentinel, not the first one", {
  project <- testProject()
  dsA <- ospsuite::DataSet$new(name = "DS_A")
  dsA$setValues(xValues = 1:3, yValues = c(10, 20, 30))
  dsB <- ospsuite::DataSet$new(name = "DS_B")
  dsB$setValues(xValues = 1:3, yValues = c(100, 200, 300))

  suppressMessages(addObservedData(project, dsA))
  suppressMessages(addObservedData(project, dsB))

  removeObservedData(project, "DS_B")

  programmaticEntries <- Filter(
    function(e) identical(e$type, "programmatic"),
    project$observedData
  )
  names_recorded <- vapply(
    programmaticEntries,
    function(e) e$name %||% NA_character_,
    character(1)
  )
  expect_true("DS_A" %in% names_recorded)
  expect_false("DS_B" %in% names_recorded)
})
test_that("addPopulation adds entry with required fields", {
  project <- testProject()
  initial <- length(project$populations)
  addPopulation(project, "NewPop", species = "Human", numberOfIndividuals = 10)
  expect_equal(length(project$populations), initial + 1)
  entry <- project$populations[["NewPop"]]
  expect_equal(entry$species, "Human")
  expect_equal(entry$numberOfIndividuals, 10)
  expect_true(project$modified)
})

test_that("addPopulation accepts range fields via ...", {
  project <- testProject()
  addPopulation(
    project,
    "Pop2",
    species = "Human",
    numberOfIndividuals = 20,
    weightMin = 50,
    weightMax = 90,
    ageMin = 18,
    ageMax = 65,
    proportionOfFemales = 50
  )
  entry <- project$populations[["Pop2"]]
  expect_equal(entry$weightMin, 50)
  expect_equal(entry$ageMax, 65)
  expect_equal(entry$proportionOfFemales, 50)
})

test_that("addPopulation errors on duplicate id", {
  project <- testProject()
  existing <- names(project$populations)[[1]]
  expect_error(
    addPopulation(
      project,
      existing,
      species = "Human",
      numberOfIndividuals = 5
    ),
    regexp = "already exists"
  )
})

test_that("addPopulation errors on unknown ... field", {
  project <- testProject()
  expect_error(
    addPopulation(
      project,
      "BadPop",
      species = "Human",
      numberOfIndividuals = 10,
      mango = 42
    ),
    regexp = "mango"
  )
})

test_that("addPopulation errors on non-positive numberOfIndividuals", {
  project <- testProject()
  expect_error(
    addPopulation(project, "Zero", species = "Human", numberOfIndividuals = 0),
    regexp = "numberOfIndividuals"
  )
})

test_that("removePopulation removes entry and sets modified", {
  project <- testProject()
  addPopulation(project, "Gone", species = "Human", numberOfIndividuals = 5)
  project$.markSaved()
  removePopulation(project, "Gone")
  expect_false("Gone" %in% names(project$populations))
  expect_true(project$modified)
})

test_that("removePopulation warns on missing id", {
  project <- testProject()
  expect_warning(
    removePopulation(project, "NoSuchPop_QQQ"),
    regexp = "not found"
  )
})

test_that("project$addPopulation delegates to standalone", {
  pc1 <- testProject()
  pc2 <- testProject()
  addPopulation(pc1, "Via1", species = "Human", numberOfIndividuals = 8)
  pc2$addPopulation("Via1", species = "Human", numberOfIndividuals = 8)
  expect_equal(pc1$populations[["Via1"]], pc2$populations[["Via1"]])
})

test_that("addPopulation survives round-trip", {
  project <- testProject()
  addPopulation(
    project,
    "RT",
    species = "Human",
    numberOfIndividuals = 10,
    weightMin = 50,
    weightMax = 90
  )
  tmp <- tempfile(fileext = ".json")
  saveProject(project, tmp)
  reloaded <- loadProject(tmp)
  entry <- reloaded$populations[["RT"]]
  expect_equal(entry$numberOfIndividuals, 10)
  expect_equal(entry$weightMin, 50)
})
test_that("addModelParameter creates a new parameter set on first call", {
  project <- testProject()
  addModelParameter(
    project,
    id = "MyNewSet",
    containerPath = "Organism|Liver",
    parameterName = "Volume",
    value = 1.8,
    units = "L"
  )
  expect_true("MyNewSet" %in% names(project$modelParameters))
  expect_equal(
    project$modelParameters[["MyNewSet"]]$paths,
    "Organism|Liver|Volume"
  )
  expect_equal(project$modelParameters[["MyNewSet"]]$values, 1.8)
  expect_equal(project$modelParameters[["MyNewSet"]]$units, "L")
  expect_true(project$modified)
})

test_that("addModelParameter appends to an existing set", {
  project <- testProject()
  addModelParameter(
    project,
    "S1",
    containerPath = "a",
    parameterName = "x",
    value = 1,
    units = "L"
  )
  addModelParameter(
    project,
    "S1",
    containerPath = "b",
    parameterName = "y",
    value = 2,
    units = "L"
  )
  expect_equal(project$modelParameters[["S1"]]$paths, c("a|x", "b|y"))
  expect_equal(project$modelParameters[["S1"]]$values, c(1, 2))
})

test_that("addModelParameter overwrites duplicate path silently", {
  project <- testProject()
  addModelParameter(
    project,
    "S2",
    containerPath = "a",
    parameterName = "x",
    value = 1,
    units = "L"
  )
  addModelParameter(
    project,
    "S2",
    containerPath = "a",
    parameterName = "x",
    value = 99,
    units = "L"
  )
  expect_equal(project$modelParameters[["S2"]]$paths, "a|x")
  expect_equal(project$modelParameters[["S2"]]$values, 99)
})

test_that("removeModelParameter drops the entry", {
  project <- testProject()
  addModelParameter(
    project,
    "S3",
    containerPath = "a",
    parameterName = "x",
    value = 1,
    units = "L"
  )
  addModelParameter(
    project,
    "S3",
    containerPath = "b",
    parameterName = "y",
    value = 2,
    units = "L"
  )
  project$.markSaved()
  removeModelParameter(project, "S3", containerPath = "a", parameterName = "x")
  expect_equal(project$modelParameters[["S3"]]$paths, "b|y")
  expect_true(project$modified)
})

test_that("removeModelParameter auto-removes empty set when last entry deleted", {
  project <- testProject()
  addModelParameter(
    project,
    "S4",
    containerPath = "a",
    parameterName = "x",
    value = 1,
    units = "L"
  )
  removeModelParameter(project, "S4", containerPath = "a", parameterName = "x")
  expect_false("S4" %in% names(project$modelParameters))
})

test_that("removeModelParameter warns on unknown set", {
  project <- testProject()
  expect_warning(
    removeModelParameter(
      project,
      "NoSuchSet_QQ",
      containerPath = "a",
      parameterName = "x"
    ),
    regexp = "not found"
  )
})

test_that("removeModelParameter warns on unknown entry within an existing set", {
  project <- testProject()
  addModelParameter(
    project,
    "S5",
    containerPath = "a",
    parameterName = "x",
    value = 1,
    units = "L"
  )
  expect_warning(
    removeModelParameter(
      project,
      "S5",
      containerPath = "z",
      parameterName = "missing"
    ),
    regexp = "not found"
  )
})

test_that("addModelParameter survives round-trip", {
  project <- Project$new()
  project$modelFolder <- tempdir()
  addModelParameter(
    project,
    "RTSet",
    containerPath = "Organism|Liver",
    parameterName = "Volume",
    value = 2.0,
    units = "L"
  )
  tmp <- tempfile(fileext = ".json")
  saveProject(project, tmp)
  reloaded <- loadProject(tmp)
  expect_equal(
    reloaded$modelParameters[["RTSet"]]$paths,
    "Organism|Liver|Volume"
  )
  expect_equal(reloaded$modelParameters[["RTSet"]]$values, 2.0)
})

test_that("project$addModelParameter delegates", {
  pc1 <- testProject()
  pc2 <- testProject()
  addModelParameter(
    pc1,
    "DelSet",
    containerPath = "a",
    parameterName = "x",
    value = 1,
    units = "L"
  )
  pc2$addModelParameter(
    "DelSet",
    containerPath = "a",
    parameterName = "x",
    value = 1,
    units = "L"
  )
  expect_equal(
    pc1$modelParameters[["DelSet"]],
    pc2$modelParameters[["DelSet"]]
  )
})
test_that("addScenario errors on duplicate scenario name", {
  project <- testProject()
  existing_name <- names(project$scenarios)[[1]]
  expect_error(
    addScenario(
      project,
      scenarioName = existing_name,
      modelFile = "model.pkml"
    ),
    "already exists"
  )
})

test_that("addScenario errors on invalid individualId", {
  project <- testProject()
  expect_error(
    addScenario(
      project,
      scenarioName = "NewScenario",
      modelFile = "model.pkml",
      individualId = "NonExistent"
    ),
    "individualId.*NonExistent.*not found"
  )
})

test_that("addScenario collects all validation errors in one message", {
  project <- testProject()
  expect_error(
    addScenario(
      project,
      scenarioName = "NewScenario",
      modelFile = "model.pkml",
      individualId = "BadIndiv",
      populationId = "BadPop",
      applicationProtocol = "BadApp"
    ),
    "BadIndiv.*BadPop.*BadApp"
  )
})

test_that("addScenario errors on empty scenarioName", {
  project <- testProject()
  expect_error(
    addScenario(project, scenarioName = "", modelFile = "model.pkml"),
    "non-empty string"
  )
})

# Happy path ----

test_that("addScenario adds a valid scenario with correct fields", {
  project <- testProject()
  original_count <- length(project$scenarios)

  addScenario(
    project,
    scenarioName = "ProgrammaticScenario",
    modelFile = "Aciclovir.pkml",
    individualId = "Indiv1"
  )

  expect_length(project$scenarios, original_count + 1)
  expect_true("ProgrammaticScenario" %in% names(project$scenarios))

  sc <- project$scenarios[["ProgrammaticScenario"]]
  expect_s3_class(sc, "Scenario")
  expect_equal(sc$scenarioName, "ProgrammaticScenario")
  expect_equal(sc$modelFile, "Aciclovir.pkml")
  expect_equal(sc$individualId, "Indiv1")
  expect_equal(sc$simulationType, "Individual")
})

test_that("addScenario with populationId sets simulationType to Population", {
  project <- testProject()
  pop_name <- names(project$populations)[[1]]

  addScenario(
    project,
    scenarioName = "PopScenario",
    modelFile = "Aciclovir.pkml",
    populationId = pop_name
  )

  sc <- project$scenarios[["PopScenario"]]
  expect_equal(sc$simulationType, "Population")
  expect_equal(sc$populationId, pop_name)
})

test_that("addScenario parses simulationTime string into list of vectors", {
  project <- testProject()

  addScenario(
    project,
    scenarioName = "TimeScenario",
    modelFile = "Aciclovir.pkml",
    simulationTime = "0, 100, 1",
    simulationTimeUnit = "h"
  )

  sc <- project$scenarios[["TimeScenario"]]
  expect_equal(sc$simulationTime, list(c(0, 100, 1)))
  expect_equal(sc$simulationTimeUnit, "h")
})

test_that("addScenario resolves outputPathIds to output path strings", {
  project <- testProject()
  path_ids <- names(project$outputPaths)

  addScenario(
    project,
    scenarioName = "OutputScenario",
    modelFile = "Aciclovir.pkml",
    outputPathIds = path_ids
  )

  sc <- project$scenarios[["OutputScenario"]]
  expect_equal(sc$outputPaths, unname(project$outputPaths[path_ids]))
})

test_that("addScenario sets modified flag to TRUE", {
  project <- testProject()
  expect_false(project$modified)

  addScenario(
    project,
    scenarioName = "ModifiedScenario",
    modelFile = "Aciclovir.pkml"
  )

  expect_true(project$modified)
})

test_that("project$addScenario() delegates to standalone addScenario()", {
  project <- testProject()

  project$addScenario(
    scenarioName = "MethodScenario",
    modelFile = "Aciclovir.pkml",
    individualId = "Indiv1"
  )

  expect_true("MethodScenario" %in% names(project$scenarios))
  sc <- project$scenarios[["MethodScenario"]]
  expect_equal(sc$individualId, "Indiv1")
})

test_that("addScenario populates all optional fields correctly", {
  project <- testProject()
  param_group <- names(project$modelParameters)[[1]]

  addScenario(
    project,
    scenarioName = "FullScenario",
    modelFile = "Aciclovir.pkml",
    individualId = "Indiv1",
    applicationProtocol = names(project$applications)[[1]],
    modelParameters = param_group,
    simulationTime = "0, 50, 1; 50, 100, 2",
    simulationTimeUnit = "min",
    steadyState = TRUE,
    steadyStateTime = 500,
    overwriteFormulasInSS = TRUE,
    readPopulationFromCSV = FALSE
  )

  sc <- project$scenarios[["FullScenario"]]
  expect_equal(sc$applicationProtocol, names(project$applications)[[1]])
  expect_equal(sc$modelParameters, param_group)
  expect_equal(sc$simulationTime, list(c(0, 50, 1), c(50, 100, 2)))
  expect_equal(sc$simulationTimeUnit, "min")
  expect_true(sc$simulateSteadyState)
  expect_equal(sc$steadyStateTime, 500)
  expect_true(sc$overwriteFormulasInSS)
  expect_false(sc$readPopulationFromCSV)
})

test_that("addScenario returns project invisibly", {
  project <- testProject()
  result <- withVisible(addScenario(
    project,
    scenarioName = "InvisibleScenario",
    modelFile = "Aciclovir.pkml"
  ))
  expect_false(result$visible)
  expect_identical(result$value, project)
})

test_that("addOutputPath adds single entry", {
  project <- testProject()
  initial <- length(project$outputPaths)
  addOutputPath(
    project,
    id = "MyOut",
    path = "Organism|PeripheralVenousBlood|C"
  )
  expect_equal(length(project$outputPaths), initial + 1)
  expect_equal(
    project$outputPaths[["MyOut"]],
    "Organism|PeripheralVenousBlood|C"
  )
  expect_true(project$modified)
})

test_that("addOutputPath accepts vectors of equal length", {
  project <- testProject()
  addOutputPath(
    project,
    id = c("O1", "O2"),
    path = c("Organism|Liver|C", "Organism|Kidney|C")
  )
  expect_equal(project$outputPaths[["O1"]], "Organism|Liver|C")
  expect_equal(project$outputPaths[["O2"]], "Organism|Kidney|C")
})

test_that("addOutputPath errors on length mismatch", {
  project <- testProject()
  expect_error(
    addOutputPath(project, c("A", "B"), c("Organism|Liver|C")),
    regexp = "same length"
  )
})

test_that("addOutputPath errors on duplicate id within call", {
  project <- testProject()
  expect_error(
    addOutputPath(project, c("Dup", "Dup"), c("a", "b")),
    regexp = "duplicate"
  )
})

test_that("addOutputPath errors on existing id", {
  project <- testProject()
  existing <- names(project$outputPaths)[[1]]
  expect_error(
    addOutputPath(project, existing, "Organism|Liver|C"),
    regexp = "already exists"
  )
})

test_that("removeOutputPath removes entry", {
  project <- testProject()
  addOutputPath(project, "GoingAway", "Organism|Liver|C")
  project$.markSaved()
  removeOutputPath(project, "GoingAway")
  expect_false("GoingAway" %in% names(project$outputPaths))
  expect_true(project$modified)
})

test_that("removeOutputPath warns on missing id", {
  project <- testProject()
  expect_warning(
    removeOutputPath(project, "NoSuchPath_ZZ"),
    regexp = "not found"
  )
})

test_that("removeScenario removes scenario", {
  project <- testProject()
  addScenario(
    project,
    "TempScenario",
    "Aciclovir.pkml",
    individualId = "Indiv1"
  )
  project$.markSaved()
  removeScenario(project, "TempScenario")
  expect_false("TempScenario" %in% names(project$scenarios))
  expect_true(project$modified)
})

test_that("removeScenario warns on missing", {
  project <- testProject()
  expect_warning(
    removeScenario(project, "NoSuchScenario_ZZ"),
    regexp = "not found"
  )
})

test_that("project$addOutputPath / removeOutputPath / removeScenario delegate", {
  pc1 <- testProject()
  pc2 <- testProject()
  addOutputPath(pc1, "M1", "Organism|Liver|C")
  pc2$addOutputPath("M1", "Organism|Liver|C")
  expect_equal(pc1$outputPaths[["M1"]], pc2$outputPaths[["M1"]])

  removeOutputPath(pc1, "M1")
  pc2$removeOutputPath("M1")
  expect_false("M1" %in% names(pc1$outputPaths))
  expect_false("M1" %in% names(pc2$outputPaths))
})

test_that("addOutputPath survives round-trip", {
  project <- testProject()
  addOutputPath(project, "RTPath", "Organism|Liver|C")
  tmp <- tempfile(fileext = ".json")
  saveProject(project, tmp)
  reloaded <- loadProject(tmp)
  expect_equal(reloaded$outputPaths[["RTPath"]], "Organism|Liver|C")
})

test_that("removeOutputPath warns when referenced by a scenario", {
  project <- testProject()
  # Find any outputPath referenced by at least one scenario (fixture-dependent)
  refId <- NULL
  for (id in names(project$outputPaths)) {
    hit <- vapply(
      project$scenarios,
      function(s) project$outputPaths[[id]] %in% s$outputPaths,
      logical(1)
    )
    if (any(hit)) {
      refId <- id
      break
    }
  }
  skip_if(is.null(refId), "no referenced outputPath in test fixture")

  expect_warning(
    removeOutputPath(project, refId),
    regexp = "referenced"
  )
  expect_false(refId %in% names(project$outputPaths))
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
