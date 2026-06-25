# Local test helper. Once Task 8 ships loadProject(), the production
# helpers.R::testProject() will work directly and this can be removed.
.testProject <- function() {
  testProject()
}

test_that(".parameterSetToStructure flattens record-shape into paths/values/units", {
  records <- list(
    list(
      containerPath = "A|B",
      parameterName = "P1",
      value = 1.5,
      units = "mg"
    ),
    list(containerPath = "A|C", parameterName = "P2", value = 2, units = NULL)
  )
  out <- esqlabsR:::.parameterSetToStructure(records)
  expect_equal(out$paths, c("A|B|P1", "A|C|P2"))
  expect_equal(out$values, c(1.5, 2))
  expect_equal(out$units, c("mg", ""))
})

test_that(".parameterSetToStructure returns NULL on empty input", {
  expect_null(esqlabsR:::.parameterSetToStructure(NULL))
  expect_null(esqlabsR:::.parameterSetToStructure(list()))
})

test_that(".mergeScenarioParameters returns NULL when no layer contributes", {
  project <- .testProject()
  scenario <- project$scenarios[["TestScenario"]]
  scenario$modelParameterSets <- NULL
  scenario$individualId <- NULL
  scenario$applicationProtocol <- NA
  expect_null(
    esqlabsR:::.mergeScenarioParameters(scenario, project, customParams = NULL)
  )
})

test_that(".mergeScenarioParameters layer 1 (modelParameterSets) iterates listed groups in order", {
  project <- .testProject()
  scenario <- project$scenarios[["TestScenario_steadystate"]]
  scenario$individualId <- NULL
  scenario$applicationProtocol <- NA
  merged <- esqlabsR:::.mergeScenarioParameters(scenario, project, NULL)
  expect_true("Organism|Liver|EHC continuous fraction" %in% merged$paths)
  expect_true("Aciclovir|Lipophilicity" %in% merged$paths)
})

test_that(".mergeScenarioParameters layer 4 (application) overrides layer 1 on overlapping path", {
  project <- .testProject()
  scenario <- project$scenarios[["TestScenario"]]
  project$modelParameterSets$Global <- list(
    list(
      containerPath = "OverlapContainer",
      parameterName = "OverlapParam",
      value = 1,
      units = NULL
    )
  )
  project$applicationParameterSets$Override <- list(
    list(
      containerPath = "OverlapContainer",
      parameterName = "OverlapParam",
      value = 99,
      units = NULL
    )
  )
  project$applications$Aciclovir_iv_250mg$parameterSets <- list("Override")
  scenario$individualId <- NULL
  merged <- esqlabsR:::.mergeScenarioParameters(scenario, project, NULL)
  idx <- match("OverlapContainer|OverlapParam", merged$paths)
  expect_equal(merged$values[idx], 99)
})

test_that(".mergeScenarioParameters layer 5 (customParams) wins over all earlier layers", {
  project <- .testProject()
  scenario <- project$scenarios[["TestScenario"]]
  customParams <- list(
    paths = "Organism|Liver|EHC continuous fraction",
    values = 42,
    units = ""
  )
  merged <- esqlabsR:::.mergeScenarioParameters(scenario, project, customParams)
  idx <- match("Organism|Liver|EHC continuous fraction", merged$paths)
  expect_equal(merged$values[idx], 42)
})

test_that(".mergeScenarioParameters skips application layer when applicationProtocol is NA", {
  project <- .testProject()
  scenario <- project$scenarios[["TestScenario"]]
  scenario$applicationProtocol <- NA
  scenario$modelParameterSets <- NULL
  scenario$individualId <- NULL
  expect_null(
    esqlabsR:::.mergeScenarioParameters(scenario, project, NULL)
  )
})

test_that(".mergeScenarioParameters errors when applicationProtocol is set but unknown", {
  project <- .testProject()
  scenario <- project$scenarios[["TestScenario"]]
  scenario$applicationProtocol <- "DoesNotExist"
  expect_error(
    esqlabsR:::.mergeScenarioParameters(scenario, project, NULL),
    regexp = "DoesNotExist"
  )
})

test_that(".mergeScenarioParameters silently skips an unknown modelParameterSets group", {
  project <- .testProject()
  scenario <- project$scenarios[["TestScenario"]]
  scenario$modelParameterSets <- c("Global", "DoesNotExist")
  scenario$individualId <- NULL
  scenario$applicationProtocol <- NA
  merged <- esqlabsR:::.mergeScenarioParameters(scenario, project, NULL)
  expect_true("Organism|Liver|EHC continuous fraction" %in% merged$paths)
})

test_that(".prepareScenario silently skips an unknown initialConditions group", {
  withr::local_options(lifecycle_verbosity = "quiet")
  project <- .testProject()
  scenario <- project$scenarios[["TestScenario"]]
  scenario$initialConditions <- c("TestInitialSet", "DoesNotExist")

  newCache <- function() {
    cache <- new.env(parent = emptyenv())
    cache$individuals <- list()
    cache$populations <- list()
    cache
  }

  # The unknown key contributes nothing: preparing with it present must
  # succeed (stopIfParameterNotFound = FALSE lets the synthetic IC path
  # warn rather than abort) and yield the same prepared simulation as the
  # scenario that references only the known set.
  expect_no_error(
    suppressWarnings(
      prepared <- esqlabsR:::.prepareScenario(
        scenario = scenario,
        project = project,
        customParams = NULL,
        cache = newCache(),
        simulationRunOptions = NULL,
        stopIfParameterNotFound = FALSE
      )
    )
  )
  expect_s3_class(prepared$simulation, "Simulation")
})

test_that(".mergeScenarioParameters silently skips an unknown individual parameter-set id", {
  project <- .testProject()
  scenario <- project$scenarios[["TestScenario"]]
  project$individuals[[1L]]$parameterSets <- list(
    "Indiv1_default",
    "DoesNotExist"
  )
  scenario$modelParameterSets <- NULL
  scenario$applicationProtocol <- NA
  merged <- esqlabsR:::.mergeScenarioParameters(scenario, project, NULL)
  expect_true("Organism|Kidney|GFR" %in% merged$paths)
})

test_that(".mergeScenarioParameters silently skips an unknown application parameter-set id", {
  project <- .testProject()
  scenario <- project$scenarios[["TestScenario"]]
  project$applications$Aciclovir_iv_250mg$parameterSets <- list(
    "Aciclovir_iv_250mg_default",
    "DoesNotExist"
  )
  scenario$modelParameterSets <- NULL
  scenario$individualId <- NULL
  merged <- esqlabsR:::.mergeScenarioParameters(scenario, project, NULL)
  expect_true(
    "Events|IV 250mg 10min|Application_1|ProtocolSchemaItem|Dose" %in%
      merged$paths
  )
})

test_that(".runScenariosFromProject returns the documented per-scenario list shape (individual)", {
  withr::local_options(lifecycle_verbosity = "quiet")
  project <- .testProject()
  out <- esqlabsR:::.runScenariosFromProject(
    project,
    scenarioNames = "TestScenario"
  )
  expect_named(out, "TestScenario")
  expect_named(
    out$TestScenario,
    c("simulation", "results", "outputValues", "population")
  )
  expect_s3_class(out$TestScenario$simulation, "Simulation")
  expect_null(out$TestScenario$population)
})

test_that(".runScenariosFromProject runs a steady-state scenario without error", {
  withr::local_options(lifecycle_verbosity = "quiet")
  project <- .testProject()
  out <- esqlabsR:::.runScenariosFromProject(
    project,
    scenarioNames = "TestScenario_steadystate"
  )
  expect_s3_class(out$TestScenario_steadystate$simulation, "Simulation")
})

test_that(".runScenariosFromProject runs a population scenario and attaches a Population", {
  withr::local_options(lifecycle_verbosity = "quiet")
  project <- .testProject()
  out <- esqlabsR:::.runScenariosFromProject(
    project,
    scenarioNames = "PopulationScenario"
  )
  expect_s3_class(out$PopulationScenario$population, "Population")
})

test_that(".runScenariosFromProject runs a CSV-population scenario and attaches a Population", {
  withr::local_options(lifecycle_verbosity = "quiet")
  project <- .testProject()
  out <- esqlabsR:::.runScenariosFromProject(
    project,
    scenarioNames = "PopulationScenarioFromCSV"
  )
  expect_s3_class(out$PopulationScenarioFromCSV$population, "Population")
})

test_that(".runScenariosFromProject errors on unknown scenarioNames", {
  withr::local_options(lifecycle_verbosity = "quiet")
  project <- .testProject()
  expect_error(
    esqlabsR:::.runScenariosFromProject(project, scenarioNames = "NopeNope"),
    regexp = "NopeNope"
  )
})

# Model file resolution ----

test_that("a scenario with an absolute modelFile runs when modelFolder is NULL", {
  withr::local_options(lifecycle_verbosity = "quiet")
  project <- .testProject()
  absModel <- normalizePath(file.path(project$modelFolder, "Aciclovir.pkml"))
  scenario <- project$scenarios[["TestScenario"]]
  scenario$modelFile <- absModel
  project$scenarios[["TestScenario"]] <- scenario
  project$modelFolder <- NULL

  out <- esqlabsR:::.runScenariosFromProject(
    project,
    scenarioNames = "TestScenario",
    validate = FALSE
  )
  expect_false(is.null(out$TestScenario$outputValues))
})

test_that("a relative modelFile with NULL modelFolder aborts with a clear message", {
  withr::local_options(lifecycle_verbosity = "quiet")
  project <- .testProject()
  project$modelFolder <- NULL
  expect_snapshot(
    error = TRUE,
    esqlabsR:::.runScenariosFromProject(
      project,
      scenarioNames = "TestScenario",
      validate = FALSE
    )
  )
})
