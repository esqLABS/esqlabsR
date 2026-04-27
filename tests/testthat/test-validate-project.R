# Tests for validateProject() - JSON-based validation

test_that("validateProject returns ValidationResults with critical error for non-existent file", {
  results <- validateProject("nonexistent_file.json")

  expect_true(inherits(results, "ValidationResults"))
  expect_true("project" %in% names(results))
  expect_false(results$project$is_valid())
  expect_true(results$project$has_critical_errors())
})

test_that("validateProject returns ValidationResults with critical error for invalid JSON", {
  temp_file <- tempfile(fileext = ".json")
  writeLines("{ invalid json }", temp_file)

  results <- validateProject(temp_file)

  expect_true(inherits(results, "ValidationResults"))
  expect_true("project" %in% names(results))
  expect_false(results$project$is_valid())

  unlink(temp_file)
})

test_that("validateProject accepts loaded Project object", {
  project_path <- system.file(
    "extdata/projects/Example/Project.json",
    package = "esqlabsR"
  )
  skip_if_not(file.exists(project_path))

  project <- loadProject(project_path)
  results <- validateProject(project)

  expect_true(inherits(results, "ValidationResults"))
  expect_false(isAnyCriticalErrors(results))
})

test_that("validateProject accepts path to Project.json", {
  project_path <- system.file(
    "extdata/projects/Example/Project.json",
    package = "esqlabsR"
  )
  skip_if_not(file.exists(project_path))

  results <- validateProject(project_path)

  expect_true(inherits(results, "ValidationResults"))
  expect_false(isAnyCriticalErrors(results))
})

test_that("validateProject returns all expected result sections", {
  project_path <- system.file(
    "extdata/projects/Example/Project.json",
    package = "esqlabsR"
  )
  skip_if_not(file.exists(project_path))

  results <- validateProject(project_path)

  expected_sections <- c(
    "individuals",
    "populations",
    "scenarios",
    "outputPaths",
    "modelParameters",
    "applications",
    "plots",
    "crossReferences"
  )
  for (section in expected_sections) {
    expect_true(
      section %in% names(results),
      info = paste("Missing section:", section)
    )
  }
})

# Section validators tests

test_that(".validateIndividuals warns when no individuals defined", {
  result <- esqlabsR:::.validateIndividuals(list())

  expect_true(result$is_valid())
  expect_true(length(result$warnings) > 0)
})

test_that(".validateIndividuals detects missing required fields", {
  individuals <- list(
    Indiv1 = list(
      species = "Human",
      gender = NULL
    )
  )

  result <- esqlabsR:::.validateIndividuals(individuals)

  expect_false(result$is_valid())
  expect_true(result$has_critical_errors())
})

test_that(".validateIndividuals warns about non-numeric fields", {
  individuals <- list(
    Indiv1 = list(
      species = "Human",
      gender = "MALE",
      weight = "seventy",
      height = 176,
      age = 30
    )
  )

  result <- esqlabsR:::.validateIndividuals(individuals)

  expect_true(result$is_valid())
  expect_true(length(result$warnings) > 0)
})

test_that(".validatePopulations warns when no populations defined", {
  result <- esqlabsR:::.validatePopulations(list())

  expect_true(result$is_valid())
  expect_true(length(result$warnings) > 0)
})

test_that(".validatePopulations detects missing required fields", {
  populations <- list(
    Pop1 = list(population = "European")
  )

  result <- esqlabsR:::.validatePopulations(populations)

  expect_false(result$is_valid())
})

test_that(".validatePopulations warns about proportionOfFemales out of range", {
  populations <- list(
    Pop1 = list(
      species = "Human",
      population = "European",
      proportionOfFemales = 150
    )
  )

  result <- esqlabsR:::.validatePopulations(populations)

  expect_true(result$is_valid())
  expect_true(length(result$warnings) > 0)
})

test_that(".validatePopulations warns about inverted min/max ranges", {
  populations <- list(
    Pop1 = list(
      species = "Human",
      population = "European",
      ageMin = 50,
      ageMax = 20
    )
  )

  result <- esqlabsR:::.validatePopulations(populations)

  expect_true(result$is_valid())
  expect_true(length(result$warnings) > 0)
})

test_that(".validateScenarios warns when no scenarios defined", {
  result <- esqlabsR:::.validateScenarios(list())

  expect_true(result$is_valid())
  expect_true(length(result$warnings) > 0)
})

test_that(".validateScenarios detects missing modelFile", {
  sc <- Scenario$new()
  sc$scenarioName <- "Test"
  sc$modelFile <- NULL
  sc$individualId <- "Indiv1"

  scenarios <- list(Test = sc)

  result <- esqlabsR:::.validateScenarios(scenarios)

  expect_false(result$is_valid())
  expect_true(result$has_critical_errors())
})

test_that(".validateScenarios detects population scenario without populationId", {
  sc <- Scenario$new()
  sc$scenarioName <- "Test"
  sc$modelFile <- "model.pkml"
  sc$individualId <- "Indiv1"
  sc$simulationType <- "Population"
  sc$populationId <- NULL

  scenarios <- list(Test = sc)

  result <- esqlabsR:::.validateScenarios(scenarios)

  expect_false(result$is_valid())
})

test_that(".validateOutputPaths warns when no output paths defined", {
  result <- esqlabsR:::.validateOutputPaths(NULL)

  expect_true(result$is_valid())
  expect_true(length(result$warnings) > 0)
})

test_that(".validateOutputPaths detects empty path values", {
  outputPaths <- c(Path1 = "Organism|Blood", Path2 = "")

  result <- esqlabsR:::.validateOutputPaths(outputPaths)

  expect_false(result$is_valid())
})

test_that(".validateParameterGroups warns when no groups defined", {
  result <- esqlabsR:::.validateParameterGroups(list(), "modelParameters")

  expect_true(result$is_valid())
  expect_true(length(result$warnings) > 0)
})

test_that(".validateParameterGroups detects mismatched paths/values lengths", {
  groups <- list(
    Group1 = list(
      paths = c("path1", "path2"),
      values = c(1),
      units = c("", "")
    )
  )

  result <- esqlabsR:::.validateParameterGroups(groups, "modelParameters")

  expect_false(result$is_valid())
})

test_that(".validateParameterGroups detects empty paths", {
  groups <- list(
    Group1 = list(
      paths = c("path1", ""),
      values = c(1, 2),
      units = c("", "")
    )
  )

  result <- esqlabsR:::.validateParameterGroups(groups, "modelParameters")

  expect_false(result$is_valid())
})

test_that(".validatePlots warns when no plots defined", {
  result <- esqlabsR:::.validatePlots(NULL)

  expect_true(result$is_valid())
  expect_true(length(result$warnings) > 0)
})

test_that(".validatePlots detects missing required columns in dataCombined", {
  plots <- list(
    dataCombined = data.frame(WrongColumn = "value"),
    plotConfiguration = data.frame(
      plotID = "P1",
      DataCombinedName = "DC1",
      plotType = "individual"
    )
  )

  result <- esqlabsR:::.validatePlots(plots)

  expect_false(result$is_valid())
})

test_that(".validatePlots detects duplicate plotIDs", {
  plots <- list(
    dataCombined = data.frame(
      DataCombinedName = c("DC1", "DC2"),
      dataType = c("simulated", "simulated")
    ),
    plotConfiguration = data.frame(
      plotID = c("P1", "P1"),
      DataCombinedName = c("DC1", "DC2"),
      plotType = c("individual", "individual")
    )
  )

  result <- esqlabsR:::.validatePlots(plots)

  expect_false(result$is_valid())
})

# Cross-reference tests

test_that(".validateCrossReferences detects invalid individual references", {
  project_path <- system.file(
    "extdata/projects/Example/Project.json",
    package = "esqlabsR"
  )
  skip_if_not(file.exists(project_path))

  project <- loadProject(project_path)

  sc <- Scenario$new()
  sc$scenarioName <- "BadScenario"
  sc$modelFile <- "model.pkml"
  sc$individualId <- "NonExistentIndividual"
  project$scenarios[["BadScenario"]] <- sc

  mock_results <- list(
    individuals = validationResult$new(),
    populations = validationResult$new(),
    scenarios = validationResult$new(),
    plots = validationResult$new()
  )

  result <- esqlabsR:::.validateCrossReferences(project, mock_results)

  expect_true(result$has_critical_errors())
})

test_that(".validateCrossReferences detects invalid population references", {
  project_path <- system.file(
    "extdata/projects/Example/Project.json",
    package = "esqlabsR"
  )
  skip_if_not(file.exists(project_path))

  project <- loadProject(project_path)

  sc <- Scenario$new()
  sc$scenarioName <- "BadPopScenario"
  sc$modelFile <- "model.pkml"
  sc$individualId <- names(project$individuals)[1]
  sc$simulationType <- "Population"
  sc$populationId <- "NonExistentPopulation"
  project$scenarios[["BadPopScenario"]] <- sc

  mock_results <- list(
    individuals = validationResult$new(),
    populations = validationResult$new(),
    scenarios = validationResult$new(),
    plots = validationResult$new()
  )

  result <- esqlabsR:::.validateCrossReferences(project, mock_results)

  expect_true(result$has_critical_errors())
})

test_that(".validateCrossReferences detects invalid scenario references in plots", {
  project_path <- system.file(
    "extdata/projects/Example/Project.json",
    package = "esqlabsR"
  )
  skip_if_not(file.exists(project_path))

  project <- loadProject(project_path)

  project$plots$dataCombined <- rbind(
    project$plots$dataCombined,
    data.frame(
      DataCombinedName = "BadDC",
      dataType = "simulated",
      scenario = "NonExistentScenario",
      label = "test",
      path = "test",
      dataSet = NA,
      group = NA,
      xOffsets = NA,
      xOffsetsUnits = NA,
      yOffsets = NA,
      yOffsetsUnits = NA,
      xScaleFactors = NA,
      yScaleFactors = NA
    )
  )

  mock_results <- list(
    individuals = validationResult$new(),
    populations = validationResult$new(),
    scenarios = validationResult$new(),
    plots = validationResult$new()
  )

  result <- esqlabsR:::.validateCrossReferences(project, mock_results)

  expect_true(result$has_critical_errors())
})

test_that(".validateCrossReferences skips when previous critical errors exist", {
  project_path <- system.file(
    "extdata/projects/Example/Project.json",
    package = "esqlabsR"
  )
  skip_if_not(file.exists(project_path))

  project <- loadProject(project_path)

  mock_results <- list(
    individuals = validationResult$new()
  )
  mock_results$individuals$add_critical_error("Test", "Previous error")

  result <- esqlabsR:::.validateCrossReferences(project, mock_results)

  expect_false(result$has_critical_errors())
  expect_true(length(result$warnings) > 0)
})

# Helper function tests

test_that("isAnyCriticalErrors detects errors across results", {
  result1 <- validationResult$new()
  result2 <- validationResult$new()
  result2$add_critical_error("Test", "Error")

  results <- list(a = result1, b = result2)
  class(results) <- c("ValidationResults", class(results))

  expect_true(isAnyCriticalErrors(results))
})

test_that("isAnyCriticalErrors returns FALSE when no errors", {
  result1 <- validationResult$new()
  result2 <- validationResult$new()

  results <- list(a = result1, b = result2)
  class(results) <- c("ValidationResults", class(results))

  expect_false(isAnyCriticalErrors(results))
})

test_that("validationSummary correctly counts errors and warnings", {
  result1 <- validationResult$new()
  result1$add_critical_error("Test", "Error 1")
  result1$add_critical_error("Test", "Error 2")
  result1$add_warning("Test", "Warning 1")

  result2 <- validationResult$new()
  result2$add_warning("Test", "Warning 2")

  results <- list(scenarios = result1, plots = result2)
  class(results) <- c("ValidationResults", class(results))

  summary <- validationSummary(results)

  expect_equal(summary$total_critical_errors, 2)
  expect_equal(summary$total_warnings, 2)
  expect_true("scenarios" %in% summary$files_with_errors)
  expect_true("plots" %in% summary$files_with_warnings)
})

test_that(".validateObservedData warns when no observedData defined", {
  result <- esqlabsR:::.validateObservedData(NULL, tempdir())
  expect_true(result$is_valid())
  expect_true(length(result$warnings) > 0)
})

test_that(".validateObservedData detects missing type field", {
  observedData <- list(list(file = "test.xlsx"))
  result <- esqlabsR:::.validateObservedData(observedData, tempdir())
  expect_false(result$is_valid())
  expect_true(any(grepl("type", unlist(result$critical_errors))))
})

test_that(".validateObservedData detects invalid type", {
  observedData <- list(list(type = "invalid"))
  result <- esqlabsR:::.validateObservedData(observedData, tempdir())
  expect_false(result$is_valid())
  expect_true(any(grepl("invalid", unlist(result$critical_errors))))
})

test_that(".validateObservedData detects missing excel required fields", {
  observedData <- list(list(type = "excel"))
  result <- esqlabsR:::.validateObservedData(observedData, tempdir())
  expect_false(result$is_valid())
  errors <- unlist(result$critical_errors)
  expect_true(any(grepl("file", errors)))
  expect_true(any(grepl("importerConfiguration", errors)))
  expect_true(any(grepl("sheets", errors)))
})

test_that(".validateObservedData detects missing pkml/script file field", {
  observedData <- list(
    list(type = "pkml"),
    list(type = "script")
  )
  result <- esqlabsR:::.validateObservedData(observedData, tempdir())
  expect_false(result$is_valid())
  expect_equal(length(result$critical_errors), 2)
})

test_that(".validateObservedData warns for non-existent files", {
  observedData <- list(list(
    type = "excel",
    file = "nonexistent.xlsx",
    importerConfiguration = "nonexistent.xml",
    sheets = list("Sheet1")
  ))
  result <- esqlabsR:::.validateObservedData(observedData, tempdir())
  expect_true(result$is_valid())
  expect_equal(length(result$warnings), 2)
})

test_that(".validateObservedData passes for valid programmatic entry", {
  observedData <- list(list(type = "programmatic"))
  result <- esqlabsR:::.validateObservedData(observedData, tempdir())
  expect_true(result$is_valid())
  expect_equal(length(result$critical_errors), 0)
})

test_that("validateProject includes observedData in results", {
  project_path <- system.file(
    "extdata/projects/Example/Project.json",
    package = "esqlabsR"
  )
  skip_if_not(file.exists(project_path))

  results <- validateProject(project_path)
  expect_true("observedData" %in% names(results))
  expect_true(inherits(results$observedData, "validationResult"))
})
