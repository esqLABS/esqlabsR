# Tests for validation infrastructure (validationResult class)

test_that("validationResult class works correctly", {
  result <- validationResult$new()

  expect_true(result$is_valid())
  expect_false(result$has_critical_errors())

  result$add_critical_error("Test", "Test error")
  expect_false(result$is_valid())
  expect_true(result$has_critical_errors())

  result$add_warning("Test", "Test warning")
  expect_equal(length(result$warnings), 1)

  summary <- result$get_summary()
  expect_equal(summary$critical_error_count, 1)
  expect_equal(summary$warning_count, 1)
})

test_that("validationResult get_formatted_messages works correctly", {
  result <- validationResult$new()
  result$add_critical_error("Structure", "Missing required field")
  result$add_warning("Data", "Value out of range")

  formatted <- result$get_formatted_messages()

  expect_true(is.list(formatted))
  expect_true("critical" %in% names(formatted))
  expect_true("warnings" %in% names(formatted))
  expect_equal(length(formatted$critical), 1)
  expect_equal(length(formatted$warnings), 1)
  expect_true(grepl("Structure", formatted$critical[[1]]))
  expect_true(grepl("Data", formatted$warnings[[1]]))
})

test_that("validationResult add_critical_error with details works", {
  result <- validationResult$new()
  result$add_critical_error(
    "Structure",
    "Missing field",
    details = list(sheet = "Sheet1", row = 5)
  )

  expect_equal(length(result$critical_errors), 1)
  expect_equal(result$critical_errors[[1]]$details$sheet, "Sheet1")
  expect_equal(result$critical_errors[[1]]$details$row, 5)
})

test_that("validationResult add_warning with details works", {
  result <- validationResult$new()
  result$add_warning(
    "Data",
    "Value warning",
    details = list(column = "Age", value = -5)
  )

  expect_equal(length(result$warnings), 1)
  expect_equal(result$warnings[[1]]$details$column, "Age")
  expect_equal(result$warnings[[1]]$details$value, -5)
})

test_that("validationResult set_data works correctly", {
  result <- validationResult$new()
  test_data <- data.frame(a = 1:3, b = letters[1:3])

  expect_null(result$data)
  result$set_data(test_data)
  expect_equal(result$data, test_data)
})

# Tests for helper functions

test_that(".check_no_duplicates detects duplicates", {
  result <- validationResult$new()
  ids <- c("a", "b", "a", "c")

  result <- esqlabsR:::.check_no_duplicates(ids, "testId", result)

  expect_true(result$has_critical_errors())
  expect_true(grepl("Duplicate", result$critical_errors[[1]]$message))
})

test_that(".check_no_duplicates passes with no duplicates", {
  result <- validationResult$new()
  ids <- c("a", "b", "c")

  result <- esqlabsR:::.check_no_duplicates(ids, "testId", result)

  expect_false(result$has_critical_errors())
})

test_that(".check_no_duplicates handles NA values", {
  result <- validationResult$new()
  ids <- c("a", NA, "b", NA)

  result <- esqlabsR:::.check_no_duplicates(ids, "testId", result)

  expect_false(result$has_critical_errors())
})

test_that(".check_required_fields detects missing fields", {
  result <- validationResult$new()
  entry <- list(field1 = "value", field2 = NULL)

  result <- esqlabsR:::.check_required_fields(
    entry,
    c("field1", "field2", "field3"),
    "test entry",
    result
  )

  expect_true(result$has_critical_errors())
  expect_equal(length(result$critical_errors), 2)
})

test_that(".check_required_fields detects empty string fields", {
  result <- validationResult$new()
  entry <- list(field1 = "value", field2 = "")

  result <- esqlabsR:::.check_required_fields(
    entry,
    c("field1", "field2"),
    "test entry",
    result
  )

  expect_true(result$has_critical_errors())
})

test_that(".check_required_fields detects NA fields", {
  result <- validationResult$new()
  entry <- list(field1 = "value", field2 = NA)

  result <- esqlabsR:::.check_required_fields(
    entry,
    c("field1", "field2"),
    "test entry",
    result
  )

  expect_true(result$has_critical_errors())
})

test_that(".check_required_fields passes with all fields present", {
  result <- validationResult$new()
  entry <- list(field1 = "value1", field2 = "value2")

  result <- esqlabsR:::.check_required_fields(
    entry,
    c("field1", "field2"),
    "test entry",
    result
  )

  expect_false(result$has_critical_errors())
})

test_that(".categorize_message categorizes messages correctly", {
  expect_equal(
    esqlabsR:::.categorize_message("missing field X"),
    "Missing Fields"
  )
  expect_equal(
    esqlabsR:::.categorize_message("required field not found"),
    "Missing Fields"
  )
  expect_equal(
    esqlabsR:::.categorize_message("duplicate ID found"),
    "Uniqueness"
  )
  expect_equal(
    esqlabsR:::.categorize_message("references undefined scenario"),
    "Invalid Reference"
  )
  expect_equal(
    esqlabsR:::.categorize_message("length mismatch"),
    "Structure"
  )
  expect_equal(
    esqlabsR:::.categorize_message("some other error"),
    "Validation"
  )
})

# validateProject() - JSON-based validation -------------------------------

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

test_that(".validatePlots flags simulated entries missing scenario", {
  plots <- list(
    dataCombined = list(
      DC1 = list(
        simulated = list(list(label = "x", path = "p")),  # missing scenario
        observed = list()
      )
    ),
    plotConfiguration = data.frame(
      plotID = "P1",
      DataCombinedName = "DC1",
      plotType = "individual"
    )
  )

  result <- esqlabsR:::.validatePlots(plots)

  expect_true(result$has_critical_errors())
})

test_that(".validatePlots detects duplicate plotIDs", {
  plots <- list(
    dataCombined = list(
      DC1 = list(
        simulated = list(list(label = "a", scenario = "S", path = "P")),
        observed = list()
      ),
      DC2 = list(
        simulated = list(list(label = "b", scenario = "S", path = "P")),
        observed = list()
      )
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

  project$plots$dataCombined$BadDC <- list(
    simulated = list(
      list(label = "test", scenario = "NonExistentScenario", path = "test")
    ),
    observed = list()
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

# .runProjectValidation: targeted runs ----

test_that(".runProjectValidation returns only requested sections", {
  project <- testProject()
  results <- esqlabsR:::.runProjectValidation(
    project,
    sections = c("scenarios", "crossReferences")
  )
  expect_setequal(names(results), c("scenarios", "crossReferences"))
})

test_that(".runProjectValidation NULL sections runs the full set", {
  project <- testProject()
  results <- esqlabsR:::.runProjectValidation(project, sections = NULL)
  expect_true(all(
    c(
      "individuals",
      "populations",
      "scenarios",
      "outputPaths",
      "modelParameters",
      "applications",
      "plots",
      "observedData",
      "crossReferences"
    ) %in%
      names(results)
  ))
})

test_that(".runProjectValidation runs crossReferences last so it sees prior results", {
  project <- testProject()
  # Inject a structural error in scenarios; crossReferences should then skip.
  project$scenarios[[1]]$modelFile <- NULL
  results <- esqlabsR:::.runProjectValidation(
    project,
    sections = c("scenarios", "crossReferences")
  )
  expect_true(results$scenarios$has_critical_errors())
  # crossReferences should have skipped (no critical errors, but a "Skipped"
  # warning).
  expect_false(results$crossReferences$has_critical_errors())
  expect_true(length(results$crossReferences$warnings) > 0)
})

# validatedSinceMutation flag ----

test_that("loadProject leaves validatedSinceMutation FALSE", {
  project <- testProject()
  expect_false(project$validatedSinceMutation)
})

test_that("validateProject sets validatedSinceMutation when clean", {
  project <- testProject()
  validateProject(project)
  expect_true(project$validatedSinceMutation)
})

test_that("validateProject leaves validatedSinceMutation FALSE on critical errors", {
  project <- testProject()
  # Force a cross-reference error.
  project$scenarios[[1]]$individualId <- "DoesNotExist"
  validateProject(project)
  expect_false(project$validatedSinceMutation)
})

test_that("any project mutation clears validatedSinceMutation", {
  project <- testProject()
  validateProject(project)
  expect_true(project$validatedSinceMutation)

  project$.markModified()
  expect_false(project$validatedSinceMutation)
})

test_that("active-binding setter clears validatedSinceMutation", {
  project <- testProject()
  validateProject(project)
  expect_true(project$validatedSinceMutation)

  project$dataFolder <- project$dataFolder
  expect_false(project$validatedSinceMutation)
})

test_that("validatedSinceMutation is read-only", {
  project <- testProject()
  expect_error(project$validatedSinceMutation <- TRUE, "readonly")
})

# .ensureValid ----

test_that(".ensureValid is a no-op when validatedSinceMutation is TRUE", {
  project <- testProject()
  project$.markValidated()

  called <- 0L
  testthat::local_mocked_bindings(
    .runProjectValidation = function(...) {
      called <<- called + 1L
      list()
    }
  )

  esqlabsR:::.ensureValid(
    project,
    sections = c("scenarios", "crossReferences"),
    opName = "test"
  )
  expect_equal(called, 0L)
})

test_that(".ensureValid runs targeted validation when not cached", {
  project <- testProject()
  expect_silent(
    esqlabsR:::.ensureValid(
      project,
      sections = c("scenarios", "crossReferences"),
      opName = "test"
    )
  )
})

test_that(".ensureValid aborts on critical errors with operation name", {
  project <- testProject()
  project$scenarios[[1]]$individualId <- "DoesNotExist"
  expect_error(
    esqlabsR:::.ensureValid(
      project,
      sections = c("scenarios", "crossReferences"),
      opName = "do the thing"
    ),
    "do the thing"
  )
})

test_that(".ensureValid targeted run does NOT set validatedSinceMutation", {
  project <- testProject()
  esqlabsR:::.ensureValid(
    project,
    sections = c("scenarios", "crossReferences"),
    opName = "test"
  )
  # Targeted: only proves a subset; cache flag must remain FALSE so a later
  # full-validation call still runs.
  expect_false(project$validatedSinceMutation)
})

