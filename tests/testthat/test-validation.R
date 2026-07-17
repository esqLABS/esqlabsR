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
  expect_named(
    summary,
    c("has_critical_errors", "critical_error_count", "warning_count")
  )
  expect_equal(summary$critical_error_count, 1)
  expect_equal(summary$warning_count, 1)
})

test_that("validationResult exposes no dead `data` surface", {
  result <- validationResult$new()

  # `data`/`set_data()`/`has_data` were removed (#1066): in the JSON-primary
  # model the parsed Project is the validated payload, nothing populates a
  # separate one.
  expect_null(result$data)
  expect_null(result$set_data)
  expect_false("has_data" %in% names(result$get_summary()))
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

  expect_named(
    summary,
    c(
      "total_critical_errors",
      "total_warnings",
      "sections_with_errors",
      "sections_with_warnings"
    )
  )
  expect_equal(summary$total_critical_errors, 2)
  expect_equal(summary$total_warnings, 2)
  expect_setequal(summary$sections_with_errors, "scenarios")
  expect_setequal(summary$sections_with_warnings, c("scenarios", "plots"))
})

test_that("validationSummary handles empty validation results", {
  validationResults <- list()
  class(validationResults) <- c("ValidationResults", class(validationResults))

  summary <- validationSummary(validationResults)

  expect_equal(summary$total_critical_errors, 0)
  expect_equal(summary$total_warnings, 0)
  expect_equal(summary$sections_with_errors, character())
  expect_equal(summary$sections_with_warnings, character())
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
# one adapter without dragging the whole fixture in. `.fakeProject()` lives
# in tests/testthat/helpers.R so several test files can share it.

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
  project <- testProject()
  expect_false(project$validatedSinceMutation)

  results <- validateProject(project)

  expect_false(isAnyCriticalErrors(results))
  expect_true(project$validatedSinceMutation)
})

test_that("validateProject() leaves validatedSinceMutation FALSE on errors", {
  project <- .fakeProject(
    scenarios = list(Bad = esqlabsR:::Scenario())
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

  .markValidated(project)
  expect_true(project$validatedSinceMutation)

  .markModified(project)
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
  sc <- esqlabsR:::Scenario()
  sc$modelFile <- ""
  result <- esqlabsR:::.validateScenarios(list(s1 = sc))
  expect_gte(length(result$critical_errors), 1)
})

test_that(".validateScenarios flags Population scenario without populationId", {
  sc <- esqlabsR:::Scenario()
  sc$modelFile <- "model.pkml"
  sc$simulationType <- "Population"
  sc$populationId <- ""
  result <- esqlabsR:::.validateScenarios(list(s1 = sc))
  msgs <- vapply(result$critical_errors, \(e) e$message, character(1))
  expect_match(msgs, "populationId", all = FALSE)
})

test_that(".validateScenarios warns when modelFile does not exist on disk", {
  sc <- esqlabsR:::Scenario(
    modelFile = "missing.pkml",
    simulationType = "Individual"
  )
  result <- esqlabsR:::.validateScenarios(
    list(s1 = sc),
    modelFolder = withr::local_tempdir()
  )
  msgs <- vapply(result$warnings, \(w) w$message, character(1))
  expect_match(msgs, "missing\\.pkml", all = FALSE)
})

test_that(".validateScenarios passes when modelFile exists on disk", {
  dir <- withr::local_tempdir()
  file.create(file.path(dir, "model.pkml"))
  sc <- esqlabsR:::Scenario(
    modelFile = "model.pkml",
    simulationType = "Individual"
  )
  result <- esqlabsR:::.validateScenarios(list(s1 = sc), modelFolder = dir)
  file_not_found_warns <- Filter(
    \(w) w$section == "File Not Found",
    result$warnings
  )
  expect_length(file_not_found_warns, 0)
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
  expect_match(msgs, "gender", all = FALSE)
  expect_match(msgs, "species", all = FALSE)
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
  expect_match(msgs, "ageMin > ageMax", all = FALSE)
})

# Section adapter: parameter sets ----

# Fixtures here are built through the public mutators so the validator is
# exercised against the array-of-records shape the parser and mutators
# actually produce, never a hand-rolled legacy `paths`/`values` shape.

test_that(".validateParameterSets flags empty paths in a real set", {
  # Real parsed shape: list of {containerPath, parameterName, value, units}.
  # The validator takes the section list directly, so feed it a bad section
  # without putting it on a project (the section accessor is read-only).
  parameterSets <- list(
    global = list(
      list(containerPath = "", parameterName = "", value = 1, units = NULL)
    )
  )
  result <- esqlabsR:::.validateParameterSets(parameterSets, "parameterSets")
  msgs <- vapply(result$critical_errors, \(e) e$message, character(1))
  expect_match(msgs, "empty parameter paths", all = FALSE)
})

test_that(".validateParameterSets flags empty containerPath with valid parameterName", {
  parameterSets <- list(
    global = list(
      list(containerPath = "", parameterName = "BMI", value = 1, units = NULL)
    )
  )
  result <- esqlabsR:::.validateParameterSets(parameterSets, "parameterSets")
  msgs <- vapply(result$critical_errors, \(e) e$message, character(1))
  expect_match(msgs, "empty parameter paths", all = FALSE)
})

test_that(".validateParameterSets flags non-numeric values in a real set", {
  parameterSets <- list(
    global = list(
      list(
        containerPath = "A",
        parameterName = "p",
        value = "abc",
        units = NULL
      )
    )
  )
  result <- esqlabsR:::.validateParameterSets(parameterSets, "parameterSets")
  msgs <- vapply(result$warnings, \(w) w$message, character(1))
  expect_match(msgs, "non-numeric value", all = FALSE)
})

test_that(".validateParameterSets flags a bad set in the unified section", {
  # The three former parameter-set kinds (model / individual / application)
  # now live in one `parameterSets` map; one bad set anywhere is flagged.
  badEntry <- list(
    list(
      containerPath = "Organism",
      parameterName = "",
      value = 1,
      units = NULL
    )
  )
  parameterSets <- list(
    global = badEntry,
    indiv1_default = badEntry,
    aciclovir_iv_250mg_default = badEntry
  )
  result <- esqlabsR:::.validateParameterSets(parameterSets, "parameterSets")
  expect_gte(length(result$critical_errors), 1)
})

test_that("validateProject() flags an invalid parameter set in the real shape", {
  # A bad section needs to be on the project for whole-project validation; an
  # in-memory `.fakeProject()` takes the section without write validation (the
  # section accessor is read-only and a loaded project would validate on write).
  project <- .fakeProject(
    parameterSets = list(
      global = list(
        list(
          containerPath = "",
          parameterName = "",
          value = "abc",
          units = NULL
        )
      )
    )
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
  expect_match(msgs, "invalid type 'weird'", all = FALSE)
  expect_match(msgs, "file", all = FALSE)
  expect_match(msgs, "type", all = FALSE)
})

test_that(".validateObservedData warns on missing files when dataFolder set", {
  tmp <- withr::local_tempdir()
  observedData <- list(
    list(type = "pkml", file = "missing.pkml")
  )
  result <- esqlabsR:::.validateObservedData(observedData, tmp)
  msgs <- vapply(result$warnings, \(w) w$message, character(1))
  expect_match(msgs, "non-existent file", all = FALSE)
})

# The two observed-data validators, the project-validator adapter
# (.validateObservedData) and the load/add-time guard (.validateObservedDataEntry),
# share one required-field spec. Both must treat `sheets` on an Excel source the
# same way: an absent `sheets` and a present-but-empty `sheets` are both a
# missing required field.

test_that("both observed-data validators require a present, non-empty sheets on excel", {
  missingSheets <- list(type = "excel", file = "x", importerConfiguration = "c")
  emptySheets <- list(
    type = "excel",
    file = "x",
    importerConfiguration = "c",
    sheets = list()
  )

  # .validateObservedData (project validator): a critical error naming `sheets`.
  missingResult <- esqlabsR:::.validateObservedData(list(missingSheets), NULL)
  emptyResult <- esqlabsR:::.validateObservedData(list(emptySheets), NULL)
  missingMsgs <- vapply(
    missingResult$critical_errors,
    \(e) e$message,
    character(1)
  )
  emptyMsgs <- vapply(emptyResult$critical_errors, \(e) e$message, character(1))
  expect_match(missingMsgs, "sheets", all = FALSE)
  expect_match(emptyMsgs, "sheets", all = FALSE)

  # .validateObservedDataEntry (load/add guard): aborts naming `sheets`.
  # Before unification an empty-but-present `sheets` slipped through here.
  expect_error(
    esqlabsR:::.validateObservedDataEntry(missingSheets, 1L),
    "sheets"
  )
  expect_error(
    esqlabsR:::.validateObservedDataEntry(emptySheets, 1L),
    "sheets"
  )
})

test_that("both observed-data validators accept a well-formed excel entry", {
  good <- list(
    type = "excel",
    file = "x.xlsx",
    importerConfiguration = "c.xml",
    sheets = list("Sheet1")
  )
  result <- esqlabsR:::.validateObservedData(list(good), NULL)
  expect_length(result$critical_errors, 0)
  expect_true(esqlabsR:::.validateObservedDataEntry(good, 1L))
})

# Section adapter: plots ----

test_that(".validatePlots warns when project has no plots sections", {
  result <- esqlabsR:::.validatePlots(NULL)
  expect_length(result$warnings, 1)
  expect_length(result$critical_errors, 0)
})

test_that(".validatePlots flags missing scenario in dataCombined", {
  dataCombined <- list(
    DC1 = list(
      simulated = list(list(scenario = ""))
    )
  )
  result <- esqlabsR:::.validatePlots(dataCombined, list(), list())
  msgs <- vapply(result$critical_errors, \(e) e$message, character(1))
  expect_match(msgs, "missing 'scenario'", all = FALSE)
})

test_that(".validatePlots flags a simulated entry missing its path", {
  # `path` is required to build a simulated dataCombined entry (the write-gate
  # `.checkDataCombinedEntry()` rejects its absence). A dataCombined loaded from
  # JSON can bypass that gate, so the lazy validator must flag the same gap.
  dataCombined <- list(
    DC1 = list(
      simulated = list(list(label = "L1", scenario = "S1"))
    )
  )
  result <- esqlabsR:::.validatePlots(dataCombined, list(), list())
  msgs <- vapply(result$critical_errors, \(e) e$message, character(1))
  expect_match(msgs, "Simulated entry.*missing 'path'", all = FALSE)
})

test_that(".validatePlots flags a missing label in a dataCombined entry", {
  dataCombined <- list(
    DC1 = list(
      # A simulated entry with a scenario but no label, and an observed entry
      # with a dataSet but no label: both are structurally incomplete.
      simulated = list(list(scenario = "S1", path = "P")),
      observed = list(list(dataSet = "obs1"))
    )
  )
  result <- esqlabsR:::.validatePlots(dataCombined, list(), list())
  expect_false(result$is_valid())
  msgs <- vapply(result$critical_errors, \(e) e$message, character(1))
  expect_match(msgs, "Simulated entry.*missing 'label'", all = FALSE)
  expect_match(msgs, "Observed entry.*missing 'label'", all = FALSE)
})

test_that(".validatePlots flags duplicate plotIds and unknown dataCombinedId", {
  dataCombined <- list(
    DC1 = list(simulated = list(list(label = "L1", scenario = "S1")))
  )
  # Two entries deliberately sharing the same plotId (a hand-edit could file
  # both under one key only by colliding; here two list slots carry the same
  # inner plotId) to exercise the duplicate-id check.
  plotConfig <- list(
    a = list(plotId = "p1", dataCombinedId = "dc1", plotType = "individual"),
    b = list(
      plotId = "p1",
      dataCombinedId = "Unknown",
      plotType = "individual"
    )
  )
  result <- esqlabsR:::.validatePlots(dataCombined, plotConfig, list())
  msgs <- vapply(result$critical_errors, \(e) e$message, character(1))
  expect_match(msgs, "Duplicate plotId", all = FALSE)
  expect_match(msgs, "unknown dataCombinedId", all = FALSE)
})

test_that(".validatePlots warns on plotType-irrelevant fields (non-blocking)", {
  dataCombined <- list(
    DC1 = list(
      simulated = list(list(label = "L1", scenario = "S1", path = "P"))
    )
  )
  # An individual plot carrying population-only `quantiles`, and a population
  # plot carrying observedVsSimulated-only `foldDistance`. Both misuses should
  # produce a warning, not a critical error (the plot still renders).
  plotConfig <- list(
    a = list(
      plotId = "p_ind",
      dataCombinedId = "DC1",
      plotType = "individual",
      quantiles = "0.05, 0.5, 0.95"
    ),
    b = list(
      plotId = "p_pop",
      dataCombinedId = "DC1",
      plotType = "population",
      foldDistance = "2"
    )
  )
  result <- esqlabsR:::.validatePlots(dataCombined, plotConfig, list())

  expect_length(result$critical_errors, 0)
  msgs <- vapply(result$warnings, \(w) w$message, character(1))
  expect_snapshot(cat(sort(msgs), sep = "\n"))
})

test_that(".validatePlots does not warn when the field matches its plotType", {
  dataCombined <- list(
    DC1 = list(
      simulated = list(list(label = "L1", scenario = "S1", path = "P"))
    )
  )
  # `quantiles`/`aggregation`/`nsd` on a population plot and `foldDistance` on
  # an observedVsSimulated plot are all legitimate: no irrelevant-field warning.
  plotConfig <- list(
    a = list(
      plotId = "p_pop",
      dataCombinedId = "DC1",
      plotType = "population",
      quantiles = "0.05, 0.5, 0.95",
      aggregation = "quantiles",
      nsd = "2"
    ),
    b = list(
      plotId = "p_ovs",
      dataCombinedId = "DC1",
      plotType = "observedVsSimulated",
      foldDistance = "2"
    )
  )
  result <- esqlabsR:::.validatePlots(dataCombined, plotConfig, list())

  expect_length(result$critical_errors, 0)
  irrelevant <- Filter(
    \(w) grepl("only applies to plotType", w$message),
    result$warnings
  )
  expect_length(irrelevant, 0)
})

# Cross-references ----

test_that(".validateCrossReferences flags scenario referencing missing individualId", {
  sc <- esqlabsR:::Scenario()
  sc$modelFile <- "x.pkml"
  sc$individualId <- "Ghost"
  project <- .fakeProject(scenarios = list(s1 = sc))
  result <- esqlabsR:::.validateCrossReferences(project, list())
  msgs <- vapply(result$critical_errors, \(e) e$message, character(1))
  expect_match(msgs, "undefined individual 'Ghost'", all = FALSE)
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
    parameterSets = list(
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
  expect_match(msgs, "undefined parameterSets", all = FALSE)
})

test_that(".validateCrossReferences suggests a near match for an individual's parameterSets", {
  individuals <- list(
    I1 = list(
      species = "Human",
      gender = "MALE",
      parameterSets = "PresysSet2"
    )
  )
  project <- .fakeProject(
    individuals = individuals,
    parameterSets = list(
      PresysSet1 = list(list(
        containerPath = "A",
        parameterName = "p",
        value = 1,
        units = NULL
      ))
    )
  )
  result <- esqlabsR:::.validateCrossReferences(project, list())
  msgs <- vapply(result$critical_errors, \(e) e$message, character(1))
  expect_snapshot(cat(msgs[grepl("Individual", msgs)], sep = "\n"))
})

test_that(".validateCrossReferences suggests a near match for an application's parameterSets", {
  applications <- list(
    A1 = list(parameterSets = "PresysSet2")
  )
  project <- .fakeProject(
    applications = applications,
    parameterSets = list(
      PresysSet1 = list(list(
        containerPath = "A",
        parameterName = "p",
        value = 1,
        units = NULL
      ))
    )
  )
  result <- esqlabsR:::.validateCrossReferences(project, list())
  msgs <- vapply(result$critical_errors, \(e) e$message, character(1))
  expect_snapshot(cat(msgs[grepl("Application", msgs)], sep = "\n"))
})

test_that(".validateCrossReferences flags a scenario's dangling initialConditions", {
  sc <- esqlabsR:::Scenario()
  sc$modelFile <- "x.pkml"
  sc$initialConditions <- "nope"
  project <- .fakeProject(
    scenarios = list(s1 = sc),
    initialConditions = list(real = esqlabsR:::.asInitialConditionSet(list()))
  )
  result <- esqlabsR:::.validateCrossReferences(project, list())
  msgs <- vapply(result$critical_errors, \(e) e$message, character(1))
  expect_match(msgs, "undefined initial-condition sets", all = FALSE)
})

test_that(".validateCrossReferences suggests a near match for a scenario's initialConditions", {
  sc <- esqlabsR:::Scenario()
  sc$modelFile <- "x.pkml"
  sc$initialConditions <- "presysset2"
  project <- .fakeProject(
    scenarios = list(s1 = sc),
    initialConditions = list(
      presysset1 = esqlabsR:::.asInitialConditionSet(list())
    )
  )
  result <- esqlabsR:::.validateCrossReferences(project, list())
  msgs <- vapply(result$critical_errors, \(e) e$message, character(1))
  expect_match(
    msgs[grepl("initial-condition", msgs)],
    "presysset1"
  )
})

test_that(".validateCrossReferences passes a valid scenario initialConditions ref", {
  sc <- esqlabsR:::Scenario()
  sc$modelFile <- "x.pkml"
  sc$initialConditions <- "real"
  project <- .fakeProject(
    scenarios = list(s1 = sc),
    initialConditions = list(real = esqlabsR:::.asInitialConditionSet(list()))
  )
  result <- esqlabsR:::.validateCrossReferences(project, list())
  expect_length(result$critical_errors, 0)
})

test_that(".validateCrossReferences resolves individuals/populations as named lists", {
  individuals <- list(
    I1 = list(species = "Human", gender = "MALE")
  )
  populations <- list(P1 = list(species = "Human"))
  sc <- esqlabsR:::Scenario()
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

test_that(".validateCrossReferences resolves a case-only mismatched reference", {
  # Disk keys are canonical (lower-cased); a hand-edited Project.json can carry a
  # non-canonical reference. `individualId = 'Adult'` against a disk key of
  # `adult` (and likewise for population / application / outputPath) must NOT be
  # flagged dangling: both sides are canonicalized before comparison.
  individuals <- list(adult = list(species = "Human", gender = "MALE"))
  populations <- list(elderly = list(species = "Human"))
  applications <- list(iv_bolus = list())
  sc <- esqlabsR:::Scenario()
  sc$modelFile <- "x.pkml"
  sc$individualId <- "Adult"
  sc$populationId <- "Elderly"
  sc$applicationProtocol <- "IV_Bolus"
  sc$simulationType <- "Population"
  sc$outputPaths <- stats::setNames("Organism|A", "OutPath_1")
  project <- .fakeProject(
    scenarios = list(s1 = sc),
    individuals = individuals,
    populations = populations,
    applications = applications,
    outputPaths = c(outpath_1 = "Organism|A")
  )
  result <- esqlabsR:::.validateCrossReferences(project, list())
  # No dangling-reference error: every case-only mismatch canonically resolves.
  expect_length(result$critical_errors, 0)
})

test_that(".validateCrossReferences skip guard consults full-project validity, not just the current run's sections", {
  # A broken scenarios section (empty modelFile -> critical error) plus a
  # dataCombined that references a non-existent scenario. A FULL run skips the
  # cross-reference pass (a prior section is broken), suppressing the
  # dataCombined error until the section is fixed. A targeted subset that does
  # NOT itself validate scenarios must reach the SAME conclusion (skip), rather
  # than emitting the dataCombined error the full run suppressed.
  sc <- esqlabsR:::Scenario()
  sc$modelFile <- "" # critical error in the scenarios section
  project <- .fakeProject(
    scenarios = list(s1 = sc),
    dataCombined = list(
      dc1 = list(simulated = list(list(label = "sim", scenario = "ghost")))
    )
  )

  # Full run: scenarios broken, so cross-references are skipped (a warning, no
  # critical error, and the "ghost" reference is NOT reported yet).
  fullResults <- esqlabsR:::.runProjectValidation(project, sections = NULL)
  fullCross <- fullResults$crossReferences
  expect_length(fullCross$critical_errors, 0)
  expect_length(fullCross$warnings, 1)

  # Targeted subset that omits the scenarios adapter: the guard must still see
  # the broken scenarios section (full-project validity) and skip identically.
  subsetResults <- esqlabsR:::.runProjectValidation(
    project,
    sections = c("plots", "crossReferences")
  )
  subsetCross <- subsetResults$crossReferences
  expect_length(subsetCross$critical_errors, 0)
  expect_length(subsetCross$warnings, 1)
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
  .markValidated(project)
  expect_invisible(esqlabsR:::.ensureValid(
    project,
    sections = c("scenarios"),
    opName = "test"
  ))
})

test_that(".ensureValid aborts with a formatted summary on critical errors", {
  sc <- esqlabsR:::Scenario()
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
  project <- .fakeProject(
    dataCombined = list(),
    plots = list(
      p1 = list(
        plotId = "p1",
        dataCombinedId = "Ghost",
        plotType = "individual"
      )
    ),
    plotGrids = list(
      g1 = list(plotGridId = "g1", plotIds = "p1")
    )
  )
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
  project <- testProject()
  # Aciclovir_fat_cell is used by TestScenario_steadystate and by no PI task,
  # isolating the scenario -> outputPath reference.
  suppressWarnings(removeOutputPath(project, "aciclovir_fat_cell"))
  results <- suppressWarnings(validateProject(project))
  msgs <- vapply(
    results$crossReferences$critical_errors,
    \(e) e$message,
    character(1)
  )
  expect_match(msgs, "aciclovir_fat_cell", all = FALSE)
})

# Cross-reference: dataCombined -> observed dataSet ----

test_that(".validatePlots flags an empty observed dataSet reference", {
  project <- .fakeProject()
  addDataCombined(
    project,
    id = "dc1",
    simulated = list(list(
      label = "sim",
      scenario = "testscenario",
      path = "Organism|A"
    ))
  )
  # Inject an empty observed dataSet directly, mimicking a hand-edited
  # Project.json that bypassed the addDataCombined() guard. The section
  # accessor is read-only; an in-memory project writes through .setSection()
  # without validating, so the malformed record survives for the validator.
  dc <- .getSection(project, "dataCombined")
  dc$dc1$observed <- list(list(label = "obs", dataSet = ""))
  .setSection(project, "dataCombined", dc)
  result <- esqlabsR:::.plotsValidatorAdapter(project)
  msgs <- vapply(result$critical_errors, \(e) e$message, character(1))
  expect_match(msgs, "dataSet", all = FALSE)
})

test_that("removeObservedData warns when a dataCombined still references it", {
  project <- .fakeProject()
  ds <- ospsuite::DataSet$new(name = "MyObs")
  ds$setValues(c(1, 2, 3), c(10, 20, 30))
  suppressMessages(addObservedData(project, ds))
  addDataCombined(
    project,
    id = "dc1",
    observed = list(list(label = "obs", dataSet = "MyObs"))
  )
  expect_snapshot(removeObservedData(project, "MyObs"))
})

# Section adapter: plots, plotType enum ----

test_that(".validatePlots flags an unknown plotType from JSON", {
  project <- .fakeProject()
  addDataCombined(
    project,
    id = "dc1",
    simulated = list(list(
      label = "sim",
      scenario = "testscenario",
      path = "Organism|A"
    ))
  )
  # Inject an unknown plotType directly, bypassing the addPlot() enum guard
  # to mimic a hand-edited Project.json (an in-memory .setSection() does not
  # validate the malformed record).
  .setSection(
    project,
    "plots",
    list(
      p1 = list(plotId = "p1", dataCombinedId = "dc1", plotType = "bogusType")
    )
  )
  result <- esqlabsR:::.plotsValidatorAdapter(project)
  msgs <- vapply(result$critical_errors, \(e) e$message, character(1))
  expect_match(msgs, "plotType", all = FALSE)
})

test_that(".validatePlots hard-errors on unknown plotGrid plotIDs", {
  project <- .fakeProject()
  addDataCombined(
    project,
    id = "dc1",
    simulated = list(list(
      label = "sim",
      scenario = "testscenario",
      path = "Organism|A"
    ))
  )
  addPlot(project, "p1", "dc1", "individual")
  .setSection(
    project,
    "plotGrids",
    list(g1 = list(plotGridId = "g1", plotIds = "ghost"))
  )
  result <- esqlabsR:::.plotsValidatorAdapter(project)
  msgs <- vapply(result$critical_errors, \(e) e$message, character(1))
  expect_match(msgs, "ghost", all = FALSE)
})

test_that(".validatePlots flags unknown plotGrid ids even with empty plotConfig", {
  result <- esqlabsR:::.validatePlots(
    list(),
    list(),
    list(g1 = list(plotGridId = "g1", plotIds = "ghost"))
  )
  msgs <- vapply(result$critical_errors, \(e) e$message, character(1))
  expect_match(msgs, "ghost", all = FALSE)
})

# Section adapter: PI per-task scenario consistency ----

test_that(".validatePI flags a parameter scenario outside the task's scenarios", {
  project <- testProject()
  # Task scoped to TestScenario; parameter references a different (valid)
  # project scenario that is not part of the task.
  addPITask(
    project,
    id = "T2",
    scenarios = c("testscenario"),
    parameters = list(esqlabsR:::PIParameter(
      id = "P1",
      scenarios = c("testscenario_steadystate"),
      path = "Organism|Liver|EHC continuous fraction",
      minValue = 0.5,
      maxValue = 1,
      startValue = 0.8
    )),
    outputMappings = list(esqlabsR:::PIOutputMapping(
      id = "M1",
      scenarios = c("testscenario"),
      outputPath = "aciclovir_pvb",
      observedData = "obs"
    ))
  )
  result <- esqlabsR:::.validatePI(project$parameterIdentification)
  msgs <- vapply(result$critical_errors, \(e) e$message, character(1))
  expect_match(msgs, "testscenario_steadystate", all = FALSE)
})

test_that(".validatePI flags a mapping scenario outside the task's scenarios", {
  project <- testProject()
  addPITask(
    project,
    id = "T2",
    scenarios = c("testscenario"),
    parameters = list(esqlabsR:::PIParameter(
      id = "P1",
      scenarios = c("testscenario"),
      path = "Organism|Liver|EHC continuous fraction",
      minValue = 0.5,
      maxValue = 1,
      startValue = 0.8
    )),
    outputMappings = list(esqlabsR:::PIOutputMapping(
      id = "M1",
      scenarios = c("populationscenario"),
      outputPath = "aciclovir_pvb",
      observedData = "obs"
    ))
  )
  result <- esqlabsR:::.validatePI(project$parameterIdentification)
  msgs <- vapply(result$critical_errors, \(e) e$message, character(1))
  expect_match(msgs, "populationscenario", all = FALSE)
})

# Cross-reference branches with previously zero coverage ----

test_that("validateProject() flags a scenario referencing a removed population", {
  project <- testProject()
  suppressWarnings(removePopulation(project, "testpopulation"))
  results <- suppressWarnings(validateProject(project))
  msgs <- vapply(
    results$crossReferences$critical_errors,
    \(e) e$message,
    character(1)
  )
  expect_match(msgs, "undefined population", all = FALSE)
})

test_that("validateProject() flags a scenario referencing a removed model set", {
  project <- testProject()
  suppressWarnings(removeParameterSet(project, "global"))
  results <- suppressWarnings(validateProject(project))
  msgs <- vapply(
    results$crossReferences$critical_errors,
    \(e) e$message,
    character(1)
  )
  expect_match(msgs, "model parameter sets", all = FALSE)
})

test_that("validateProject() flags a scenario referencing a removed application", {
  project <- testProject()
  suppressWarnings(removeApplication(project, "aciclovir_iv_250mg"))
  results <- suppressWarnings(validateProject(project))
  msgs <- vapply(
    results$crossReferences$critical_errors,
    \(e) e$message,
    character(1)
  )
  expect_match(msgs, "undefined application", all = FALSE)
})

test_that("validateProject() flags an application referencing a removed set", {
  project <- testProject()
  suppressWarnings(removeParameterSet(
    project,
    "aciclovir_iv_250mg_default"
  ))
  results <- suppressWarnings(validateProject(project))
  msgs <- vapply(
    results$crossReferences$critical_errors,
    \(e) e$message,
    character(1)
  )
  expect_match(msgs, "Application.*undefined parameterSets", all = FALSE)
})

test_that("validateProject() flags a dataCombined referencing an unknown scenario", {
  project <- .fakeProject()
  addDataCombined(
    project,
    id = "dc1",
    simulated = list(list(
      label = "sim",
      scenario = "ghostscenario",
      path = "Organism|A"
    ))
  )
  results <- suppressWarnings(validateProject(project))
  msgs <- vapply(
    results$crossReferences$critical_errors,
    \(e) e$message,
    character(1)
  )
  expect_match(msgs, "ghostscenario", all = FALSE)
})

test_that("validateProject() flags a PI outputMapping referencing a removed outputPath", {
  project <- testProject()
  # The bundled PI task maps Aciclovir_PVB; removing it strands the mapping.
  suppressWarnings(removeOutputPath(project, "aciclovir_pvb"))
  results <- suppressWarnings(validateProject(project))
  msgs <- vapply(
    results$crossReferences$critical_errors,
    \(e) e$message,
    character(1)
  )
  expect_match(msgs, "undefined outputPath", all = FALSE)
})

test_that("validateProject() flags a PI outputMapping with no outputPath", {
  project <- testProject()
  # Mimic a hand-edited Project.json whose PI outputMapping lost its
  # `outputPath`: the record reaches cross-reference validation with a NULL
  # `outputPathId`. This must be reported as a critical error, not crash the
  # cross-reference pass with "argument is of length zero".
  private <- project$.__enclos_env__$private
  tasks <- private$.parameterIdentification
  tasks[["aciclovirsimple"]]$outputMappings[[1]]$outputPathId <- NULL
  private$.parameterIdentification <- tasks

  results <- suppressWarnings(validateProject(project))
  msgs <- vapply(
    results$crossReferences$critical_errors,
    \(e) e$message,
    character(1)
  )
  expect_match(msgs, "does not define an outputPath", all = FALSE)
})

# print.ValidationResults ----

# Builds a deterministic `ValidationResults` list with a known mix of critical
# errors and warnings across several definition types, so the print snapshot is
# stable (no fixture paths, no timestamps in the rendered output).
.fakeValidationResults <- function() {
  scenarios <- validationResult$new()
  scenarios$add_critical_error(
    "Invalid Reference",
    "Scenario 'S1' references undefined individual 'ghost'"
  )
  scenarios$add_warning("Data", "Scenario 'S1' modelFile not found on disk")

  parameterSets <- validationResult$new()
  parameterSets$add_warning("Data", "No parameter sets defined")

  crossReferences <- validationResult$new()
  crossReferences$add_critical_error(
    "Invalid Reference",
    "dataCombined references undefined scenarios: ghost"
  )

  individuals <- validationResult$new() # clean section, should be folded away

  results <- list(
    scenarios = scenarios,
    individuals = individuals,
    parameterSets = parameterSets,
    crossReferences = crossReferences
  )
  class(results) <- c("ValidationResults", class(results))
  results
}

test_that("print.ValidationResults renders a grouped summary with glyphs", {
  expect_snapshot(print(.fakeValidationResults()))
})

test_that("print.ValidationResults renders an all-OK line for a clean result", {
  clean <- list(
    scenarios = validationResult$new(),
    individuals = validationResult$new()
  )
  class(clean) <- c("ValidationResults", class(clean))
  expect_snapshot(print(clean))
})

test_that("print.ValidationResults returns its argument invisibly and unchanged", {
  results <- .fakeValidationResults()
  expect_invisible(print(results))
  returned <- withVisible(print(results))
  expect_false(returned$visible)
  expect_identical(returned$value, results)
})

test_that("print.ValidationResults leaves the structured object indexable", {
  project <- testProject()
  results <- suppressWarnings(validateProject(project))
  before <- utils::capture.output(printed <- print(results))
  expect_identical(printed, results)
  # The machine-readable surface is untouched by printing.
  expect_s3_class(results$scenarios, "validationResult")
  expect_type(results$scenarios$critical_errors, "list")
  expect_named(results, names(suppressWarnings(validateProject(project))))
})

test_that("format.ValidationResults returns the printed lines as a character vector", {
  lines <- format(.fakeValidationResults())
  expect_type(lines, "character")
  expect_match(lines, "scenarios", all = FALSE)
})
