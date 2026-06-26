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

test_that(".validateParameterSets flags mismatched paths/values", {
  sets <- list(
    bad = list(paths = c("A|p", "B|q"), values = 1, units = c("", ""))
  )
  result <- esqlabsR:::.validateParameterSets(sets, "modelParameterSets")
  msgs <- vapply(result$critical_errors, \(e) e$message, character(1))
  expect_true(any(grepl("different lengths", msgs)))
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

test_that(".validatePlots flags duplicate plotIds and unknown dataCombinedName", {
  plots <- list(
    dataCombined = list(DC1 = list(simulated = list(list(scenario = "S1")))),
    plotConfiguration = data.frame(
      plotId = c("p1", "p1"),
      dataCombinedName = c("DC1", "Unknown"),
      plotType = c("individual", "individual"),
      stringsAsFactors = FALSE
    ),
    plotGrids = data.frame()
  )
  result <- esqlabsR:::.validatePlots(plots)
  msgs <- vapply(result$critical_errors, \(e) e$message, character(1))
  expect_true(any(grepl("Duplicate plotId", msgs)))
  expect_true(any(grepl("unknown dataCombinedName", msgs)))
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
      real = list(paths = "A|p", values = 1, units = "")
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
      plotId = "p1",
      dataCombinedName = "Ghost",
      plotType = "individual",
      stringsAsFactors = FALSE
    ),
    plotGrids = data.frame(
      name = "G1",
      plotIds = "p1",
      stringsAsFactors = FALSE
    )
  )
  project <- .fakeProject(plots = plots)
  expect_snapshot(error = TRUE, createPlots(project))
})
