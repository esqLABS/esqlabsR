# JSON loading ----

test_that("Project loads from valid JSON and populates all fields", {
  jsonPath <- testProjectJSONPath()
  pc <- Project$new(jsonPath)

  expect_true(isOfType(pc, "Project"))
  expect_true(fs::file_exists(pc$modelFolder))
  expect_true(fs::dir_exists(pc$configurationsFolder))

  expect_length(pc$scenarios, 5)
  expect_true("TestScenario" %in% names(pc$scenarios))
  expect_length(pc$modelParameters, 4)
  expect_true("Global" %in% names(pc$modelParameters))
  expect_length(pc$individuals, 1)
  expect_true("Indiv1" %in% names(pc$individuals))
  expect_length(pc$populations, 2)
  expect_length(pc$applications, 1)
  expect_length(pc$outputPaths, 2)
  expect_false(is.null(pc$plots))
  expect_equal(as.character(pc$jsonPath), normalizePath(jsonPath))
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
  pc <- Project$new(jsonPath)
  jsonDir <- dirname(jsonPath)
  expect_equal(
    as.character(pc$modelFolder),
    normalizePath(file.path(jsonDir, "Models/Simulations/"))
  )
})

test_that("Project parses model parameters into list(paths, values, units)", {
  pc <- testProject()
  global <- pc$modelParameters[["Global"]]
  expect_equal(global$paths, "Organism|Liver|EHC continuous fraction")
  expect_equal(global$values, 1)
  expect_equal(global$units, "")
})

test_that("Project parses scenarios into Scenario objects", {
  pc <- testProject()
  sc <- pc$scenarios[["TestScenario2"]]
  expect_s3_class(sc, "Scenario")
  expect_equal(sc$scenarioName, "TestScenario2")
  expect_true(sc$simulateSteadyState)
  expect_equal(length(sc$simulationTime), 2)
  expect_equal(
    sc$parameterGroups,
    c("Global", "Aciclovir", "Sheet, with comma")
  )
})

test_that("Individuals are stored as plain lists", {
  pc <- testProject()
  indiv <- pc$individuals[["Indiv1"]]
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
  pc <- testProject()
  pop <- pc$populations[["TestPopulation"]]
  expect_type(pop, "list")
  expect_true("species" %in% names(pop))
  expect_true("population" %in% names(pop))
  expect_true("numberOfIndividuals" %in% names(pop))
  # NOT a PopulationCharacteristics object
  expect_false(isOfType(pop, "PopulationCharacteristics"))
})

test_that("Project parses plots into data.frames", {
  pc <- testProject()
  expect_s3_class(pc$plots$dataCombined, "data.frame")
  expect_equal(nrow(pc$plots$dataCombined), 4)
  expect_s3_class(pc$plots$plotConfiguration, "data.frame")
  expect_equal(nrow(pc$plots$plotConfiguration), 4)
})

# Path property setters ----

test_that("setting non-existent outputFolder does not warn", {
  pc <- testProject()
  expect_no_warning(pc$outputFolder <- "this/directory/does/not/exist")
})

test_that("setting non-existent dataFolder warns", {
  pc <- testProject()
  expect_warning(pc$dataFolder <- "this/directory/does/not/exist")
})

test_that("setting invalid paths warns for each path property", {
  pc <- testProject()
  expect_warning(pc$configurationsFolder <- "Wrong/Folder")
  expect_warning(pc$dataFolder <- "folder/data/does/not/exist")
  expect_warning(pc$modelFolder <- "folder/model/does/not/exist")
  expect_warning(pc$populationsFolder <- "folder/populations/does/not/exist")
  expect_warning(pc$modelParamsFile <- "modelparams_donotexist.xlsx")
  expect_warning(pc$individualsFile <- "individuals_donotexist.xlsx")
  expect_warning(pc$populationsFile <- "populations_donotexist.xlsx")
  expect_warning(pc$scenariosFile <- "scenarios_donotexist.xlsx")
  expect_warning(pc$applicationsFile <- "applications_donotexist.xlsx")
  expect_warning(pc$plotsFile <- "plots_donotexist.xlsx")
})

# Modified flag ----

test_that("modified flag is FALSE when first created from JSON", {
  pc <- testProject()
  expect_false(pc$modified)
})

test_that("modified flag is FALSE for empty Project", {
  pc <- Project$new()
  expect_false(pc$modified)
})

test_that("modified flag can be set to a logical value", {
  pc <- testProject()
  expect_false(pc$modified)
  pc$modified <- TRUE
  expect_true(pc$modified)
  pc$modified <- FALSE
  expect_false(pc$modified)
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
    pc <- testProject()
    expect_false(pc$modified)
    suppressWarnings(pc[[prop]] <- "new/value")
    expect_true(pc$modified, info = paste("property:", prop))
  }
})

test_that("modified flag persists across multiple property changes", {
  pc <- testProject()
  expect_false(pc$modified)
  suppressWarnings(pc$modelFolder <- "new/model/folder")
  expect_true(pc$modified)
  suppressWarnings(pc$dataFolder <- "new/data/folder")
  expect_true(pc$modified)
  pc$outputFolder <- "new/output/folder"
  expect_true(pc$modified)
})

test_that("modified flag is independent between clones", {
  pc <- testProject()
  cloned <- pc$clone()
  expect_false(cloned$modified)

  suppressWarnings(pc$modelFolder <- "modified/folder")
  expect_true(pc$modified)
  expect_false(cloned$modified)

  suppressWarnings(cloned$dataFolder <- "modified/data/folder")
  expect_true(pc$modified)
  expect_true(cloned$modified)
})

test_that("Scenarios are parsed as Scenario objects (not ScenarioConfiguration)", {
  pc <- testProject()
  expect_true(length(pc$scenarios) > 0)
  sc <- pc$scenarios[["TestScenario"]]
  expect_s3_class(sc, "Scenario")
  expect_equal(sc$scenarioName, "TestScenario")
  expect_false(is.null(sc$modelFile))
  expect_equal(sc$parameterGroups, c("Global"))
})

test_that("pc$scenarios replaces pc$scenarioConfigurations", {
  pc <- testProject()
  expect_true(!is.null(pc$scenarios))
})

# Print method ----

test_that("print() shows category counts for loaded project", {
  pc <- testProject()
  output <- capture.output(print(pc))
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
  pc <- testProject()
  output <- capture.output(print(pc))
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
  pc <- Project$new()
  output <- capture.output(print(pc))
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
  pc <- testProject()
  output <- capture.output(print(pc))
  output_text <- paste(output, collapse = "\n")

  expect_match(output_text, "Model folder:\\s+Models/Simulations")
  expect_match(output_text, "Data folder:\\s+Data")
  expect_match(output_text, "Output folder:\\s+Results")
  expect_no_match(output_text, pc$projectDirPath, fixed = TRUE)
})

# observedData parsing ----

test_that("observedData field is populated from JSON", {
  pc <- testProject()
  expect_type(pc$observedData, "list")
  expect_true(length(pc$observedData) > 0)
  expect_equal(pc$observedData[[1]]$type, "excel")
  expect_equal(pc$observedData[[1]]$sheets, list("Laskin 1982.Group A"))
})

test_that("observedData is empty list when JSON has no observedData", {
  tmp <- tempfile(fileext = ".json")
  writeLines(
    '{"schemaVersion": "2.0", "filePaths": {}}',
    tmp
  )
  pc <- Project$new(tmp)
  expect_type(pc$observedData, "list")
  expect_length(pc$observedData, 0)
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

# data accessor ----

test_that("data accessor returns complete project structure as list", {
  pc <- testProject()
  data <- pc$data

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

test_that("data accessor is read-only", {
  pc <- testProject()
  expect_error(pc$data <- list(), "readonly")
})

test_that("data accessor preserves schemaVersion from source JSON", {
  jsonPath <- testProjectJSONPath()
  rawJson <- jsonlite::fromJSON(jsonPath, simplifyVector = FALSE)
  pc <- Project$new(jsonPath)

  expect_equal(pc$data$schemaVersion, rawJson$schemaVersion)
})

test_that("data accessor preserves filePaths values from source JSON", {
  jsonPath <- testProjectJSONPath()
  rawJson <- jsonlite::fromJSON(jsonPath, simplifyVector = FALSE)
  pc <- Project$new(jsonPath)

  for (field in names(rawJson$filePaths)) {
    raw_val <- rawJson$filePaths[[field]]
    data_val <- pc$data$filePaths[[field]]
    if (!is.null(raw_val)) {
      expect_equal(
        gsub("/$", "", as.character(data_val)),
        gsub("/$", "", as.character(raw_val)),
        info = paste("filePaths field:", field)
      )
    }
  }
})

test_that("data accessor preserves scenario count from source JSON", {
  jsonPath <- testProjectJSONPath()
  rawJson <- jsonlite::fromJSON(jsonPath, simplifyVector = FALSE)
  pc <- Project$new(jsonPath)

  expect_equal(length(pc$data$scenarios), length(rawJson$scenarios))
})

test_that("data accessor preserves scenario names from source JSON", {
  jsonPath <- testProjectJSONPath()
  rawJson <- jsonlite::fromJSON(jsonPath, simplifyVector = FALSE)
  pc <- Project$new(jsonPath)

  rawNames <- vapply(rawJson$scenarios, function(s) s$name, character(1))
  dataNames <- vapply(pc$data$scenarios, function(s) s$name, character(1))
  expect_equal(sort(unname(dataNames)), sort(unname(rawNames)))
})

test_that("data accessor preserves modelParameters from source JSON", {
  jsonPath <- testProjectJSONPath()
  rawJson <- jsonlite::fromJSON(jsonPath, simplifyVector = FALSE)
  pc <- Project$new(jsonPath)

  expect_equal(names(pc$data$modelParameters), names(rawJson$modelParameters))
})

test_that("data accessor preserves individuals from source JSON", {
  jsonPath <- testProjectJSONPath()
  rawJson <- jsonlite::fromJSON(jsonPath, simplifyVector = FALSE)
  pc <- Project$new(jsonPath)

  expect_equal(length(pc$data$individuals), length(rawJson$individuals))
})

test_that("data accessor preserves populations from source JSON", {
  jsonPath <- testProjectJSONPath()
  rawJson <- jsonlite::fromJSON(jsonPath, simplifyVector = FALSE)
  pc <- Project$new(jsonPath)

  expect_equal(length(pc$data$populations), length(rawJson$populations))
})

test_that("data accessor preserves outputPaths from source JSON", {
  jsonPath <- testProjectJSONPath()
  rawJson <- jsonlite::fromJSON(jsonPath, simplifyVector = FALSE)
  pc <- Project$new(jsonPath)

  expect_equal(pc$data$outputPaths, rawJson$outputPaths)
})

test_that("data accessor reflects in-memory changes after addScenario", {
  pc <- testProject()
  originalCount <- length(pc$data$scenarios)

  addScenario(pc, "NewTestScenario", "Aciclovir.pkml", individualId = "Indiv1")

  expect_equal(length(pc$data$scenarios), originalCount + 1)
  newNames <- vapply(pc$data$scenarios, function(s) s$name, character(1))
  expect_true("NewTestScenario" %in% newNames)
})

test_that("data accessor returns default structure for empty Project", {
  pc <- Project$new()
  data <- pc$data
  expect_equal(data$schemaVersion, "2.0")
  expect_equal(length(data$scenarios), 0)
  expect_equal(length(data$individuals), 0)
  expect_equal(length(data$populations), 0)
})

# sync method ----

test_that("sync returns in_sync TRUE for freshly loaded project", {
  pc <- testProject()
  result <- pc$sync(silent = TRUE)

  expect_type(result, "list")
  expect_true(result$in_sync)
  expect_false(result$unsaved_changes)
})

test_that("sync detects unsaved changes after modification", {
  pc <- testProject()
  addScenario(pc, "NewScenario", "Aciclovir.pkml", individualId = "Indiv1")

  result <- pc$sync(silent = TRUE)

  expect_false(result$in_sync)
  expect_true(result$unsaved_changes)
})

test_that("sync shows unsaved_changes FALSE after saveProject", {
  temp_proj <- local_test_project()
  pc <- loadProject(temp_proj$snapshot_path)
  addScenario(pc, "NewScenario", "Aciclovir.pkml", individualId = "Indiv1")

  saveProject(pc)
  result <- pc$sync(silent = TRUE)

  expect_false(result$unsaved_changes)
  expect_false(result$json_modified)
  expect_true(result$excel_modified)
})

test_that("sync detects external JSON file changes", {
  temp_proj <- local_test_project()
  pc <- loadProject(temp_proj$snapshot_path)

  cfg <- jsonlite::fromJSON(temp_proj$snapshot_path, simplifyVector = FALSE)
  cfg$scenarios[[1]]$name <- "RenamedByExternalEdit"
  jsonlite::write_json(
    cfg,
    temp_proj$snapshot_path,
    auto_unbox = TRUE,
    null = "null"
  )

  result <- pc$sync(silent = TRUE)

  expect_false(result$in_sync)
  expect_true(result$json_modified)
})

test_that("sync reports excel_modified when Excel differs from JSON", {
  temp_proj <- local_test_project()
  pc <- loadProject(temp_proj$snapshot_path)

  scenariosXlsx <- file.path(temp_proj$configurations_dir, "Scenarios.xlsx")
  scenariosDf <- readExcel(scenariosXlsx, sheet = "Scenarios")
  scenariosDf$Scenario_name[[1]] <- "ModifiedInExcel"
  .writeExcel(list(Scenarios = scenariosDf), scenariosXlsx)

  result <- pc$sync(silent = TRUE)

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
  pc <- Project$new()
  pc$modelFolder <- testProjectJSONPath() |>
    dirname() |>
    file.path("Models")
  pc$dataFolder <- testProjectJSONPath() |>
    dirname() |>
    file.path("Data")
  pc$outputFolder <- tempdir()

  addIndividual(
    pc,
    "I1",
    species = "Human",
    weight = 70,
    height = 175,
    age = 30
  )
  addPopulation(pc, "P1", species = "Human", numberOfIndividuals = 5)
  addModelParameterGroup(
    pc,
    "G1",
    paths = "Organism|Liver|Volume",
    values = 1.8,
    units = "L"
  )
  addApplicationGroup(
    pc,
    "Oral",
    paths = "Applications|Oral|Dose",
    values = 10,
    units = "mg"
  )
  addOutputPath(
    pc,
    "Out1",
    "Organism|PeripheralVenousBlood|Aciclovir|Concentration in container"
  )
  addScenario(
    pc,
    "S1",
    "Aciclovir.pkml",
    individualId = "I1",
    applicationProtocol = "Oral",
    parameterGroups = "G1",
    outputPathIds = "Out1"
  )

  expect_true(pc$modified)
  expect_equal(length(pc$individuals), 1)
  expect_equal(length(pc$populations), 1)
  expect_equal(length(pc$modelParameters), 1)
  expect_equal(length(pc$applications), 1)
  expect_equal(length(pc$outputPaths), 1)
  expect_equal(length(pc$scenarios), 1)

  # Round-trip
  tmp <- tempfile(fileext = ".json")
  saveProject(pc, tmp)
  reloaded <- loadProject(tmp)
  expect_equal(names(reloaded$individuals), "I1")
  expect_equal(names(reloaded$scenarios), "S1")
  expect_equal(reloaded$scenarios[["S1"]]$individualId, "I1")
})
