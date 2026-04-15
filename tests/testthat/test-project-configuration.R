# JSON loading ----

test_that("ProjectConfiguration loads from valid JSON and populates all fields", {
  jsonPath <- testProjectConfigurationJSONPath()
  pc <- ProjectConfiguration$new(jsonPath)

  expect_true(isOfType(pc, "ProjectConfiguration"))
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

test_that("ProjectConfiguration errors on missing schemaVersion", {
  temp <- tempfile(fileext = ".json")
  writeLines('{"projectConfiguration": {}}', temp)
  expect_error(ProjectConfiguration$new(temp), "schemaVersion")
  unlink(temp)
})

test_that("ProjectConfiguration errors on unsupported schemaVersion", {
  temp <- tempfile(fileext = ".json")
  writeLines('{"schemaVersion": "99.0", "projectConfiguration": {}}', temp)
  expect_error(ProjectConfiguration$new(temp), "schemaVersion")
  unlink(temp)
})

test_that("ProjectConfiguration resolves paths relative to JSON directory", {
  jsonPath <- testProjectConfigurationJSONPath()
  pc <- ProjectConfiguration$new(jsonPath)
  jsonDir <- dirname(jsonPath)
  expect_equal(
    as.character(pc$modelFolder),
    normalizePath(file.path(jsonDir, "Models/Simulations/"))
  )
})

test_that("ProjectConfiguration parses model parameters into list(paths, values, units)", {
  pc <- testProjectConfigurationJSON()
  global <- pc$modelParameters[["Global"]]
  expect_equal(global$paths, "Organism|Liver|EHC continuous fraction")
  expect_equal(global$values, 1)
  expect_equal(global$units, "")
})

test_that("ProjectConfiguration parses scenarios into Scenario objects", {
  pc <- testProjectConfigurationJSON()
  sc <- pc$scenarios[["TestScenario2"]]
  expect_s3_class(sc, "Scenario")
  expect_equal(sc$scenarioName, "TestScenario2")
  expect_true(sc$simulateSteadyState)
  expect_equal(length(sc$simulationTime), 2)
  expect_equal(sc$parameterGroups, c("Global", "Aciclovir", "Sheet, with comma"))
})

test_that("Individuals are stored as plain lists", {
  pc <- testProjectConfigurationJSON()
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
  pc <- testProjectConfigurationJSON()
  pop <- pc$populations[["TestPopulation"]]
  expect_type(pop, "list")
  expect_true("species" %in% names(pop))
  expect_true("population" %in% names(pop))
  expect_true("numberOfIndividuals" %in% names(pop))
  # NOT a PopulationCharacteristics object
  expect_false(isOfType(pop, "PopulationCharacteristics"))
})

test_that("ProjectConfiguration parses plots into data.frames", {
  pc <- testProjectConfigurationJSON()
  expect_s3_class(pc$plots$dataCombined, "data.frame")
  expect_equal(nrow(pc$plots$dataCombined), 4)
  expect_s3_class(pc$plots$plotConfiguration, "data.frame")
  expect_equal(nrow(pc$plots$plotConfiguration), 4)
})

# Path property setters ----

test_that("setting non-existent outputFolder does not warn", {
  pc <- testProjectConfigurationJSON()
  expect_no_warning(pc$outputFolder <- "this/directory/does/not/exist")
})

test_that("setting non-existent dataFolder warns", {
  pc <- testProjectConfigurationJSON()
  expect_warning(pc$dataFolder <- "this/directory/does/not/exist")
})

test_that("setting invalid paths warns for each path property", {
  pc <- testProjectConfigurationJSON()
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
  expect_warning(pc$dataFile <- "data_donotexist.xlsx")
  expect_warning(pc$dataImporterConfigurationFile <- "importer_donotexist.xml")
})

# Modified flag ----

test_that("modified flag is FALSE when first created from JSON", {
  pc <- testProjectConfigurationJSON()
  expect_false(pc$modified)
})

test_that("modified flag is FALSE for empty ProjectConfiguration", {
  pc <- ProjectConfiguration$new()
  expect_false(pc$modified)
})

test_that("modified flag is read-only", {
  pc <- testProjectConfigurationJSON()
  expect_error(pc$modified <- TRUE, "modified is readonly")
})

test_that("modified flag becomes TRUE when any property is changed", {
  properties <- c(
    "modelFolder", "configurationsFolder", "modelParamsFile",
    "individualsFile", "populationsFile", "populationsFolder",
    "scenariosFile", "applicationsFile", "plotsFile",
    "dataFolder", "dataFile", "dataImporterConfigurationFile",
    "outputFolder"
  )
  for (prop in properties) {
    pc <- testProjectConfigurationJSON()
    expect_false(pc$modified)
    suppressWarnings(pc[[prop]] <- "new/value")
    expect_true(pc$modified, info = paste("property:", prop))
  }
})

test_that("modified flag persists across multiple property changes", {
  pc <- testProjectConfigurationJSON()
  expect_false(pc$modified)
  suppressWarnings(pc$modelFolder <- "new/model/folder")
  expect_true(pc$modified)
  suppressWarnings(pc$dataFolder <- "new/data/folder")
  expect_true(pc$modified)
  pc$outputFolder <- "new/output/folder"
  expect_true(pc$modified)
})

test_that("modified flag is independent between clones", {
  pc <- testProjectConfigurationJSON()
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
  pc <- testProjectConfigurationJSON()
  expect_true(length(pc$scenarios) > 0)
  sc <- pc$scenarios[["TestScenario"]]
  expect_s3_class(sc, "Scenario")
  expect_equal(sc$scenarioName, "TestScenario")
  expect_false(is.null(sc$modelFile))
  expect_equal(sc$parameterGroups, c("Global"))
})

test_that("pc$scenarios replaces pc$scenarioConfigurations", {
  pc <- testProjectConfigurationJSON()
  expect_true(!is.null(pc$scenarios))
})

# Print method ----

test_that("print() shows category counts for loaded project", {
  pc <- testProjectConfigurationJSON()
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

test_that("print() does not show folder paths or Excel file paths", {
  pc <- testProjectConfigurationJSON()
  output <- capture.output(print(pc))
  output_text <- paste(output, collapse = "\n")

  expect_no_match(output_text, "Model Folder")
  expect_no_match(output_text, "Configurations Folder")
  expect_no_match(output_text, "Data Folder")
  expect_no_match(output_text, "Output Folder")
  expect_no_match(output_text, "Populations Folder")
  expect_no_match(output_text, "\\.xlsx")
  expect_no_match(output_text, "Environment Variables")
})

test_that("print() shows 0 counts for empty ProjectConfiguration", {
  pc <- ProjectConfiguration$new()
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

# observedData parsing ----

test_that("observedData field is populated from JSON", {
  pc <- testProjectConfigurationJSON()
  expect_type(pc$observedData, "list")
  expect_true(length(pc$observedData) > 0)
  expect_equal(pc$observedData[[1]]$type, "excel")
  expect_equal(pc$observedData[[1]]$sheets, list("Laskin 1982.Group A"))
})

test_that("observedData is empty list when JSON has no observedData", {
  tmp <- tempfile(fileext = ".json")
  writeLines(
    '{"schemaVersion": "2.0", "projectConfiguration": {}}',
    tmp
  )
  pc <- ProjectConfiguration$new(tmp)
  expect_type(pc$observedData, "list")
  expect_length(pc$observedData, 0)
  unlink(tmp)
})
