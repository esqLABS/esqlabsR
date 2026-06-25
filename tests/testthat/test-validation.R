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

test_that("isAnyCriticalErrors detects errors in validation results", {
  result_with_error <- validationResult$new()
  result_with_error$add_critical_error("Test", "Error message")

  result_no_error <- validationResult$new()

  validationResults <- list(
    file1 = result_no_error,
    file2 = result_with_error
  )

  expect_true(isAnyCriticalErrors(validationResults))
})

test_that("isAnyCriticalErrors returns FALSE when no errors", {
  result1 <- validationResult$new()
  result2 <- validationResult$new()

  validationResults <- list(
    file1 = result1,
    file2 = result2
  )

  expect_false(isAnyCriticalErrors(validationResults))
})

test_that("isAnyCriticalErrors handles non-validationResult objects", {
  result_with_error <- validationResult$new()
  result_with_error$add_critical_error("Test", "Error message")

  validationResults <- list(
    file1 = result_with_error,
    file2 = "not a validation result",
    file3 = NULL
  )

  expect_true(isAnyCriticalErrors(validationResults))
})

test_that("validationSummary correctly counts errors and warnings", {
  result1 <- validationResult$new()
  result1$add_critical_error("Test", "Error 1")
  result1$add_critical_error("Test", "Error 2")
  result1$add_warning("Test", "Warning 1")

  result2 <- validationResult$new()
  result2$add_warning("Test", "Warning 2")

  validationResults <- list(
    scenarios = result1,
    plots = result2
  )
  class(validationResults) <- c("ValidationResults", class(validationResults))

  summary <- validationSummary(validationResults)

  expect_equal(summary$total_critical_errors, 2)
  expect_equal(summary$total_warnings, 2)
  expect_equal(length(summary$files_with_errors), 1)
  expect_equal(length(summary$files_with_warnings), 2)
  expect_true("scenarios" %in% summary$files_with_errors)
  expect_true("scenarios" %in% summary$files_with_warnings)
  expect_true("plots" %in% summary$files_with_warnings)
})

test_that("validationSummary handles empty validation results", {
  validationResults <- list()
  class(validationResults) <- c("ValidationResults", class(validationResults))

  summary <- validationSummary(validationResults)

  expect_equal(summary$total_critical_errors, 0)
  expect_equal(summary$total_warnings, 0)
  expect_equal(length(summary$files_with_errors), 0)
  expect_equal(length(summary$files_with_warnings), 0)
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

# Project (JSON) validator: validateProject() ----

# Lightweight Project factory for adapter unit tests. Creates an empty
# in-memory Project and overrides specific fields so each test can target
# one adapter without dragging the whole fixture in.
.fakeProject <- function(...) {
  project <- Project$new()
  project$schemaVersion <- "2.0"
  project$esqlabsRVersion <- NA_character_
  project$outputPaths <- list()
  project$scenarios <- list()
  project$modelParameterSets <- list()
  project$individualParameterSets <- list()
  project$applicationParameterSets <- list()
  project$individuals <- list()
  project$populations <- list()
  project$applications <- list()
  project$observedData <- list()
  project$plots <- NULL
  overrides <- list(...)
  for (nm in names(overrides)) {
    project[[nm]] <- overrides[[nm]]
  }
  project
}

test_that("validateProject() rejects non-Project inputs", {
  expect_snapshot(error = TRUE, validateProject("not a project"))
})

test_that("validateProject() returns sections in canonical order", {
  project <- .fakeProject()
  results <- validateProject(project)

  expect_s3_class(results, "ValidationResults")
  expect_named(
    results,
    c(names(esqlabsR:::.validationAdapters), "crossReferences")
  )
})

test_that("validateProject() flips validatedSinceMutation when clean", {
  project <- loadProject(testthat::test_path("data/TestProject/Project.json"))
  expect_false(project$validatedSinceMutation)

  results <- validateProject(project)

  expect_false(isAnyCriticalErrors(results))
  expect_true(project$validatedSinceMutation)
})

test_that("validateProject() leaves validatedSinceMutation FALSE on errors", {
  project <- .fakeProject(
    scenarios = list(Bad = esqlabsR:::Scenario$new())
  )

  results <- validateProject(project)

  expect_true(isAnyCriticalErrors(results))
  expect_false(project$validatedSinceMutation)
})

test_that("validatedSinceMutation is read-only", {
  project <- .fakeProject()
  expect_snapshot(error = TRUE, project$validatedSinceMutation <- TRUE)
})

test_that(".markValidated and .markModified flip the flag", {
  project <- .fakeProject()
  expect_false(project$validatedSinceMutation)

  project$.markValidated()
  expect_true(project$validatedSinceMutation)

  project$.markModified()
  expect_false(project$validatedSinceMutation)
})

# Section adapter: outputPaths ----

test_that(".validateOutputPaths warns on empty section", {
  result <- esqlabsR:::.validateOutputPaths(list())
  expect_length(result$warnings, 1)
  expect_length(result$critical_errors, 0)
})

test_that(".validateOutputPaths flags duplicate ids and empty values", {
  paths <- c(a = "X|y", a = "X|y", b = "")
  result <- esqlabsR:::.validateOutputPaths(paths)
  expect_gte(length(result$critical_errors), 2)
})

test_that(".validateOutputPaths warns when ids collide on a path", {
  paths <- c(a = "X|y", b = "X|y")
  result <- esqlabsR:::.validateOutputPaths(paths)
  expect_length(result$critical_errors, 0)
  expect_length(result$warnings, 1)
})

# Section adapter: scenarios ----

test_that(".validateScenarios flags missing modelFile and bad simulationType", {
  sc <- esqlabsR:::Scenario$new()
  sc$modelFile <- ""
  result <- esqlabsR:::.validateScenarios(list(s1 = sc))
  expect_gte(length(result$critical_errors), 1)
})

test_that(".validateScenarios flags Population scenario without populationId", {
  sc <- esqlabsR:::Scenario$new()
  sc$modelFile <- "model.pkml"
  sc$simulationType <- "Population"
  sc$populationId <- ""
  result <- esqlabsR:::.validateScenarios(list(s1 = sc))
  msgs <- vapply(result$critical_errors, \(e) e$message, character(1))
  expect_true(any(grepl("populationId", msgs)))
})

# Section adapter: individuals ----

test_that(".validateIndividuals warns on empty section", {
  result <- esqlabsR:::.validateIndividuals(list())
  expect_length(result$warnings, 1)
  expect_length(result$critical_errors, 0)
})

test_that(".validateIndividuals catches missing required fields", {
  individuals <- list(
    Adult = list(species = "Human"),
    Bad = list()
  )
  result <- esqlabsR:::.validateIndividuals(individuals)
  msgs <- vapply(result$critical_errors, \(e) e$message, character(1))
  expect_true(any(grepl("gender", msgs)))
  expect_true(any(grepl("species", msgs)))
})

# Section adapter: populations ----

test_that(".validatePopulations warns on inverted ranges", {
  populations <- list(
    P1 = list(
      species = "Human",
      ageMin = 60,
      ageMax = 30
    )
  )
  result <- esqlabsR:::.validatePopulations(populations)
  msgs <- vapply(result$warnings, \(w) w$message, character(1))
  expect_true(any(grepl("ageMin > ageMax", msgs)))
})

# Section adapter: parameter sets ----

# Fixtures here are built through the public mutators so the validator is
# exercised against the array-of-records shape the parser and mutators
# actually produce, never a hand-rolled legacy `paths`/`values` shape.

test_that(".validateParameterSets flags empty paths in a real model set", {
  project <- loadProject(testthat::test_path("data/TestProject/Project.json"))
  # Real parsed shape: list of {containerPath, parameterName, value, units}.
  project$modelParameterSets[["Global"]] <- list(
    list(containerPath = "", parameterName = "", value = 1, units = NULL)
  )
  result <- esqlabsR:::.validateParameterSets(
    project$modelParameterSets,
    "modelParameterSets"
  )
  msgs <- vapply(result$critical_errors, \(e) e$message, character(1))
  expect_true(any(grepl("empty parameter paths", msgs)))
})

test_that(".validateParameterSets flags empty containerPath with valid parameterName", {
  project <- loadProject(testthat::test_path("data/TestProject/Project.json"))
  project$modelParameterSets[["Global"]] <- list(
    list(containerPath = "", parameterName = "BMI", value = 1, units = NULL)
  )
  result <- esqlabsR:::.validateParameterSets(
    project$modelParameterSets,
    "modelParameterSets"
  )
  msgs <- vapply(result$critical_errors, \(e) e$message, character(1))
  expect_true(any(grepl("empty parameter paths", msgs)))
})

test_that(".validateParameterSets flags non-numeric values in a real set", {
  project <- loadProject(testthat::test_path("data/TestProject/Project.json"))
  project$modelParameterSets[["Global"]] <- list(
    list(containerPath = "A", parameterName = "p", value = "abc", units = NULL)
  )
  result <- esqlabsR:::.validateParameterSets(
    project$modelParameterSets,
    "modelParameterSets"
  )
  msgs <- vapply(result$warnings, \(w) w$message, character(1))
  expect_true(any(grepl("non-numeric value", msgs)))
})

test_that(".validateParameterSets flags all three parameter-set sections", {
  project <- loadProject(testthat::test_path("data/TestProject/Project.json"))
  badEntry <- list(
    list(
      containerPath = "Organism",
      parameterName = "",
      value = 1,
      units = NULL
    )
  )
  project$modelParameterSets[["Global"]] <- badEntry
  project$individualParameterSets[["Indiv1_default"]] <- badEntry
  project$applicationParameterSets[["Aciclovir_iv_250mg_default"]] <- badEntry

  for (section in c(
    "modelParameterSets",
    "individualParameterSets",
    "applicationParameterSets"
  )) {
    result <- esqlabsR:::.validateParameterSets(project[[section]], section)
    expect_gte(length(result$critical_errors), 1)
  }
})

test_that("validateProject() flags an invalid parameter set in the real shape", {
  project <- loadProject(testthat::test_path("data/TestProject/Project.json"))
  project$modelParameterSets[["Global"]] <- list(
    list(containerPath = "", parameterName = "", value = "abc", units = NULL)
  )
  results <- suppressWarnings(validateProject(project))
  expect_true(isAnyCriticalErrors(results))
})

# Section adapter: applications ----

test_that(".validateApplications warns on empty section but emits no critical errors", {
  result <- esqlabsR:::.validateApplications(list())
  expect_s3_class(result, "validationResult")
  expect_length(result$critical_errors, 0)
  expect_length(result$warnings, 1)
})

# Section adapter: observedData ----

test_that(".validateObservedData flags unknown type and missing required fields", {
  observedData <- list(
    list(type = "weird", file = "x"),
    list(type = "excel"),
    list()
  )
  result <- esqlabsR:::.validateObservedData(observedData, NULL)
  msgs <- vapply(result$critical_errors, \(e) e$message, character(1))
  expect_true(any(grepl("invalid type 'weird'", msgs)))
  expect_true(any(grepl("file", msgs)))
  expect_true(any(grepl("type", msgs)))
})

test_that(".validateObservedData warns on missing files when dataFolder set", {
  tmp <- withr::local_tempdir()
  observedData <- list(
    list(type = "pkml", file = "missing.pkml")
  )
  result <- esqlabsR:::.validateObservedData(observedData, tmp)
  msgs <- vapply(result$warnings, \(w) w$message, character(1))
  expect_true(any(grepl("non-existent file", msgs)))
})

# Section adapter: plots ----

test_that(".validatePlots warns when project has no plots section", {
  result <- esqlabsR:::.validatePlots(NULL)
  expect_length(result$warnings, 1)
  expect_length(result$critical_errors, 0)
})

test_that(".validatePlots flags missing scenario in dataCombined", {
  plots <- list(
    dataCombined = list(
      DC1 = list(
        simulated = list(list(scenario = ""))
      )
    ),
    plotConfiguration = data.frame(),
    plotGrids = data.frame()
  )
  result <- esqlabsR:::.validatePlots(plots)
  msgs <- vapply(result$critical_errors, \(e) e$message, character(1))
  expect_true(any(grepl("missing 'scenario'", msgs)))
})

test_that(".validatePlots flags duplicate plotIDs and unknown DataCombinedName", {
  plots <- list(
    dataCombined = list(DC1 = list(simulated = list(list(scenario = "S1")))),
    plotConfiguration = data.frame(
      plotID = c("p1", "p1"),
      DataCombinedName = c("DC1", "Unknown"),
      plotType = c("individual", "individual"),
      stringsAsFactors = FALSE
    ),
    plotGrids = data.frame()
  )
  result <- esqlabsR:::.validatePlots(plots)
  msgs <- vapply(result$critical_errors, \(e) e$message, character(1))
  expect_true(any(grepl("Duplicate plotID", msgs)))
  expect_true(any(grepl("unknown DataCombinedName", msgs)))
})

# Cross-references ----

test_that(".validateCrossReferences flags scenario referencing missing individualId", {
  sc <- esqlabsR:::Scenario$new()
  sc$modelFile <- "x.pkml"
  sc$individualId <- "Ghost"
  project <- .fakeProject(scenarios = list(s1 = sc))
  result <- esqlabsR:::.validateCrossReferences(project, list())
  msgs <- vapply(result$critical_errors, \(e) e$message, character(1))
  expect_true(any(grepl("undefined individualId 'Ghost'", msgs)))
})

test_that(".validateCrossReferences flags individual referencing unknown parameter set", {
  individuals <- list(
    I1 = list(
      species = "Human",
      gender = "MALE",
      parameterSets = "nope"
    )
  )
  project <- .fakeProject(
    individuals = individuals,
    individualParameterSets = list(
      real = list(list(
        containerPath = "A",
        parameterName = "p",
        value = 1,
        units = NULL
      ))
    )
  )
  result <- esqlabsR:::.validateCrossReferences(project, list())
  msgs <- vapply(result$critical_errors, \(e) e$message, character(1))
  expect_true(any(grepl("undefined individualParameterSets", msgs)))
})

test_that(".validateCrossReferences resolves individuals/populations as named lists", {
  individuals <- list(
    I1 = list(species = "Human", gender = "MALE")
  )
  populations <- list(P1 = list(species = "Human"))
  sc <- esqlabsR:::Scenario$new()
  sc$modelFile <- "x.pkml"
  sc$individualId <- "I1"
  sc$populationId <- "P1"
  sc$simulationType <- "Population"
  project <- .fakeProject(
    scenarios = list(s1 = sc),
    individuals = individuals,
    populations = populations
  )
  result <- esqlabsR:::.validateCrossReferences(project, list())
  expect_length(result$critical_errors, 0)
})

test_that(".validateCrossReferences skips and warns when prior section had critical errors", {
  prior <- list(scenarios = validationResult$new())
  prior$scenarios$add_critical_error("X", "broken")
  project <- .fakeProject()
  result <- esqlabsR:::.validateCrossReferences(project, prior)
  expect_length(result$critical_errors, 0)
  expect_length(result$warnings, 1)
  expect_match(result$warnings[[1]]$message, "skipped")
})

# Dispatcher behaviour ----

test_that(".runProjectValidation honors a targeted sections vector", {
  project <- .fakeProject()
  results <- esqlabsR:::.runProjectValidation(
    project,
    sections = c("scenarios", "outputPaths", "unknownSection")
  )
  expect_named(results, c("scenarios", "outputPaths"))
})

# .ensureValid + runtime guards ----

test_that(".ensureValid short-circuits when validatedSinceMutation is TRUE", {
  project <- .fakeProject()
  project$.markValidated()
  expect_invisible(esqlabsR:::.ensureValid(
    project,
    sections = c("scenarios"),
    opName = "test"
  ))
})

test_that(".ensureValid aborts with a formatted summary on critical errors", {
  sc <- esqlabsR:::Scenario$new()
  sc$modelFile <- ""
  project <- .fakeProject(scenarios = list(s1 = sc))
  expect_snapshot(
    error = TRUE,
    esqlabsR:::.ensureValid(
      project,
      sections = c("scenarios"),
      opName = "runScenarios"
    )
  )
})

test_that("createPlots(validate = FALSE) skips the validation guard", {
  project <- .fakeProject()
  expect_equal(createPlots(project, validate = FALSE), list())
})

test_that("createPlots(validate = TRUE) aborts on a clearly broken project", {
  plots <- list(
    dataCombined = list(),
    plotConfiguration = data.frame(
      plotID = "p1",
      DataCombinedName = "Ghost",
      plotType = "individual",
      stringsAsFactors = FALSE
    ),
    plotGrids = data.frame(
      name = "G1",
      plotIDs = "p1",
      stringsAsFactors = FALSE
    )
  )
  project <- .fakeProject(plots = plots)
  expect_snapshot(error = TRUE, createPlots(project))
})

# .abortValidationErrors: glue metacharacters in messages ----

test_that(".abortValidationErrors escapes glue metacharacters in messages", {
  result <- validationResult$new()
  result$add_critical_error("Structure", "Scenario \"Dose {mg}\" is broken")
  result$add_critical_error("Structure", "Scenario S{1} also broken")
  results <- list(scenarios = result)
  expect_snapshot(
    error = TRUE,
    esqlabsR:::.abortValidationErrors(results, "runScenarios")
  )
})

# Cross-reference: scenario -> outputPath ----

test_that("validateProject() flags a scenario referencing a removed outputPath", {
  project <- loadProject(testthat::test_path("data/TestProject/Project.json"))
  # Aciclovir_fat_cell is used by TestScenario_steadystate and by no PI task,
  # isolating the scenario -> outputPath reference.
  suppressWarnings(removeOutputPath(project, "Aciclovir_fat_cell"))
  results <- suppressWarnings(validateProject(project))
  msgs <- vapply(
    results$crossReferences$critical_errors,
    \(e) e$message,
    character(1)
  )
  expect_true(any(grepl("Aciclovir_fat_cell", msgs)))
})

# Cross-reference: dataCombined -> observed dataSet ----

test_that(".validatePlots flags an empty observed dataSet reference", {
  project <- loadProject(testthat::test_path("data/TestProject/Project.json"))
  project$plots <- NULL
  addDataCombined(
    project,
    name = "DC1",
    simulated = list(list(
      label = "sim",
      scenario = "TestScenario",
      path = "Organism|A"
    ))
  )
  # Inject an empty observed dataSet directly, mimicking a hand-edited
  # Project.json that bypassed the addDataCombined() guard.
  project$plots$dataCombined$DC1$observed <- list(
    list(label = "obs", dataSet = "")
  )
  result <- esqlabsR:::.validatePlots(project$plots)
  msgs <- vapply(result$critical_errors, \(e) e$message, character(1))
  expect_true(any(grepl("dataSet", msgs)))
})

test_that("removeObservedData warns when a dataCombined still references it", {
  project <- loadProject(testthat::test_path("data/TestProject/Project.json"))
  ds <- ospsuite::DataSet$new(name = "MyObs")
  ds$setValues(c(1, 2, 3), c(10, 20, 30))
  suppressMessages(addObservedData(project, ds))
  project$plots <- NULL
  addDataCombined(
    project,
    name = "DC1",
    observed = list(list(label = "obs", dataSet = "MyObs"))
  )
  expect_snapshot(removeObservedData(project, "MyObs"))
})

# Section adapter: plots, plotType enum ----

test_that(".validatePlots flags an unknown plotType from JSON", {
  project <- loadProject(testthat::test_path("data/TestProject/Project.json"))
  project$plots <- NULL
  addDataCombined(
    project,
    name = "DC1",
    simulated = list(list(
      label = "sim",
      scenario = "TestScenario",
      path = "Organism|A"
    ))
  )
  # Inject an unknown plotType directly, bypassing the addPlot() enum guard
  # to mimic a hand-edited Project.json.
  project$plots$plotConfiguration <- data.frame(
    plotID = "p1",
    DataCombinedName = "DC1",
    plotType = "bogusType",
    stringsAsFactors = FALSE
  )
  result <- esqlabsR:::.validatePlots(project$plots)
  msgs <- vapply(result$critical_errors, \(e) e$message, character(1))
  expect_true(any(grepl("plotType", msgs)))
})

test_that(".validatePlots hard-errors on unknown plotGrid plotIDs", {
  project <- loadProject(testthat::test_path("data/TestProject/Project.json"))
  project$plots <- NULL
  addDataCombined(
    project,
    name = "DC1",
    simulated = list(list(
      label = "sim",
      scenario = "TestScenario",
      path = "Organism|A"
    ))
  )
  addPlot(project, "p1", "DC1", "individual")
  project$plots$plotGrids <- data.frame(
    name = "G1",
    plotIDs = "ghost",
    stringsAsFactors = FALSE
  )
  result <- esqlabsR:::.validatePlots(project$plots)
  msgs <- vapply(result$critical_errors, \(e) e$message, character(1))
  expect_true(any(grepl("ghost", msgs)))
})

test_that(".validatePlots flags unknown plotGrid ids even with empty plotConfig", {
  plots <- list(
    dataCombined = list(),
    plotConfiguration = data.frame(),
    plotGrids = data.frame(
      name = "G1",
      plotIDs = "ghost",
      stringsAsFactors = FALSE
    )
  )
  result <- esqlabsR:::.validatePlots(plots)
  msgs <- vapply(result$critical_errors, \(e) e$message, character(1))
  expect_true(any(grepl("ghost", msgs)))
})

# Section adapter: PI per-task scenario consistency ----

test_that(".validatePI flags a parameter scenario outside the task's scenarios", {
  project <- loadProject(testthat::test_path("data/TestProject/Project.json"))
  # Task scoped to TestScenario; parameter references a different (valid)
  # project scenario that is not part of the task.
  addPITask(
    project,
    id = "T2",
    scenarios = c("TestScenario"),
    parameters = list(esqlabsR:::PIParameter(
      id = "P1",
      scenarios = c("TestScenario_steadystate"),
      path = "Organism|Liver|EHC continuous fraction",
      minValue = 0.5,
      maxValue = 1,
      startValue = 0.8
    )),
    outputMappings = list(esqlabsR:::PIOutputMapping(
      id = "M1",
      scenarios = c("TestScenario"),
      outputPathId = "Aciclovir_PVB",
      observedDataId = "obs"
    ))
  )
  result <- esqlabsR:::.validatePI(project$parameterIdentification)
  msgs <- vapply(result$critical_errors, \(e) e$message, character(1))
  expect_true(any(grepl("TestScenario_steadystate", msgs)))
})

test_that(".validatePI flags a mapping scenario outside the task's scenarios", {
  project <- loadProject(testthat::test_path("data/TestProject/Project.json"))
  addPITask(
    project,
    id = "T2",
    scenarios = c("TestScenario"),
    parameters = list(esqlabsR:::PIParameter(
      id = "P1",
      scenarios = c("TestScenario"),
      path = "Organism|Liver|EHC continuous fraction",
      minValue = 0.5,
      maxValue = 1,
      startValue = 0.8
    )),
    outputMappings = list(esqlabsR:::PIOutputMapping(
      id = "M1",
      scenarios = c("PopulationScenario"),
      outputPathId = "Aciclovir_PVB",
      observedDataId = "obs"
    ))
  )
  result <- esqlabsR:::.validatePI(project$parameterIdentification)
  msgs <- vapply(result$critical_errors, \(e) e$message, character(1))
  expect_true(any(grepl("PopulationScenario", msgs)))
})

# Cross-reference branches with previously zero coverage ----

test_that("validateProject() flags a scenario referencing a removed population", {
  project <- loadProject(testthat::test_path("data/TestProject/Project.json"))
  suppressWarnings(removePopulation(project, "TestPopulation"))
  results <- suppressWarnings(validateProject(project))
  msgs <- vapply(
    results$crossReferences$critical_errors,
    \(e) e$message,
    character(1)
  )
  expect_true(any(grepl("populationId", msgs)))
})

test_that("validateProject() flags a scenario referencing a removed model set", {
  project <- loadProject(testthat::test_path("data/TestProject/Project.json"))
  suppressWarnings(removeModelParameterSet(project, "Global"))
  results <- suppressWarnings(validateProject(project))
  msgs <- vapply(
    results$crossReferences$critical_errors,
    \(e) e$message,
    character(1)
  )
  expect_true(any(grepl("model parameter sets", msgs)))
})

test_that("validateProject() flags a scenario referencing a removed application", {
  project <- loadProject(testthat::test_path("data/TestProject/Project.json"))
  suppressWarnings(removeApplication(project, "Aciclovir_iv_250mg"))
  results <- suppressWarnings(validateProject(project))
  msgs <- vapply(
    results$crossReferences$critical_errors,
    \(e) e$message,
    character(1)
  )
  expect_true(any(grepl("applicationProtocol", msgs)))
})

test_that("validateProject() flags an application referencing a removed set", {
  project <- loadProject(testthat::test_path("data/TestProject/Project.json"))
  suppressWarnings(removeApplicationParameterSet(
    project,
    "Aciclovir_iv_250mg_default"
  ))
  results <- suppressWarnings(validateProject(project))
  msgs <- vapply(
    results$crossReferences$critical_errors,
    \(e) e$message,
    character(1)
  )
  expect_true(any(grepl("applicationParameterSets", msgs)))
})

test_that("validateProject() flags a dataCombined referencing an unknown scenario", {
  project <- loadProject(testthat::test_path("data/TestProject/Project.json"))
  project$plots <- NULL
  addDataCombined(
    project,
    name = "DC1",
    simulated = list(list(
      label = "sim",
      scenario = "GhostScenario",
      path = "Organism|A"
    ))
  )
  results <- suppressWarnings(validateProject(project))
  msgs <- vapply(
    results$crossReferences$critical_errors,
    \(e) e$message,
    character(1)
  )
  expect_true(any(grepl("GhostScenario", msgs)))
})

test_that("validateProject() flags a PI outputMapping referencing a removed outputPath", {
  project <- loadProject(testthat::test_path("data/TestProject/Project.json"))
  # The bundled PI task maps Aciclovir_PVB; removing it strands the mapping.
  suppressWarnings(removeOutputPath(project, "Aciclovir_PVB"))
  results <- suppressWarnings(validateProject(project))
  msgs <- vapply(
    results$crossReferences$critical_errors,
    \(e) e$message,
    character(1)
  )
  expect_true(any(grepl("outputPathId", msgs)))
})
