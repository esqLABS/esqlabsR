# Local test helper delegating to helpers.R::testProject(). It forwards the
# calling test's frame so the throwaway project copy (a write-through definition
# tree) lives until the test finishes, not until this wrapper returns.
.testProject <- function(envir = parent.frame()) {
  testProject(envir = envir)
}

test_that(".buildSimulationRunOptions returns NULL when no defaults are declared", {
  expect_null(esqlabsR:::.buildSimulationRunOptions(NULL))
  expect_null(esqlabsR:::.buildSimulationRunOptions(list()))
})

test_that(".buildSimulationRunOptions maps the three settable fields", {
  opts <- esqlabsR:::.buildSimulationRunOptions(list(
    numberOfCores = 3,
    checkForNegativeValues = TRUE,
    showProgress = FALSE
  ))
  expect_s3_class(opts, "SimulationRunOptions")
  expect_identical(opts$numberOfCores, 3L)
  expect_true(opts$checkForNegativeValues)
  expect_false(opts$showProgress)
})

test_that(".buildSimulationRunOptions leaves an unset field at its default", {
  baseline <- ospsuite::SimulationRunOptions$new()$checkForNegativeValues
  opts <- esqlabsR:::.buildSimulationRunOptions(list(numberOfCores = 1))
  expect_identical(opts$numberOfCores, 1L)
  expect_identical(opts$checkForNegativeValues, baseline)
})

test_that("runScenarios falls back to the project default run options when the caller passes none", {
  # Capture the run options `.runScenariosFromProject` resolves without a native
  # simulation: mock `.prepareScenario` to record them and abort immediately.
  withr::local_options(lifecycle_verbosity = "quiet")
  project <- .testProject()
  project$defaultSimulationRunOptions <- list(numberOfCores = 2)

  captured <- NULL
  local_mocked_bindings(
    .prepareScenario = function(scenario, project, ..., simulationRunOptions) {
      captured <<- simulationRunOptions
      stop("stop before simulating")
    }
  )
  expect_error(
    esqlabsR:::.runScenariosFromProject(
      project,
      scenarioNames = "testscenario"
    ),
    "stop before simulating"
  )
  expect_s3_class(captured, "SimulationRunOptions")
  expect_identical(captured$numberOfCores, 2L)
})

test_that("runScenarios lets an explicit simulationRunOptions argument win over the project default", {
  # The caller's argument must override `defaultSimulationRunOptions` entirely,
  # not merge with it. Same capture-then-abort mock, no native simulation.
  withr::local_options(lifecycle_verbosity = "quiet")
  project <- .testProject()
  project$defaultSimulationRunOptions <- list(numberOfCores = 2)
  callerOptions <- ospsuite::SimulationRunOptions$new(numberOfCores = 5)

  captured <- NULL
  local_mocked_bindings(
    .prepareScenario = function(scenario, project, ..., simulationRunOptions) {
      captured <<- simulationRunOptions
      stop("stop before simulating")
    }
  )
  expect_error(
    esqlabsR:::.runScenariosFromProject(
      project,
      scenarioNames = "testscenario",
      simulationRunOptions = callerOptions
    ),
    "stop before simulating"
  )
  expect_identical(captured, callerOptions)
  expect_identical(captured$numberOfCores, 5L)
})

test_that("runScenarios threads stopIfParameterNotFound through to .prepareScenario", {
  # The public arg must reach the per-scenario prep. Same capture-then-abort
  # mock, no native simulation. Default is TRUE; an explicit FALSE must win.
  withr::local_options(lifecycle_verbosity = "quiet")
  project <- .testProject()

  captured <- NULL
  local_mocked_bindings(
    .prepareScenario = function(
      scenario,
      project,
      ...,
      stopIfParameterNotFound
    ) {
      captured <<- stopIfParameterNotFound
      stop("stop before simulating")
    }
  )

  expect_error(
    runScenarios(project, scenarios = "testscenario"),
    "stop before simulating"
  )
  expect_true(captured)

  expect_error(
    runScenarios(
      project,
      scenarios = "testscenario",
      stopIfParameterNotFound = FALSE
    ),
    "stop before simulating"
  )
  expect_false(captured)
})

test_that(".collectScenarioResult aborts on a failed scenario when stopIfFails is TRUE", {
  scenario <- list(scenarioName = "s1", outputPaths = NULL)
  expect_error(
    esqlabsR:::.collectScenarioResult(
      scenario = scenario,
      simulation = NULL,
      results = NULL,
      population = NULL,
      stopIfFails = TRUE
    ),
    regexp = "No simulation results could be computed"
  )
})

test_that(".collectScenarioResult warns and returns NULL outputValues when stopIfFails is FALSE", {
  scenario <- list(scenarioName = "s1", outputPaths = NULL)
  expect_warning(
    out <- esqlabsR:::.collectScenarioResult(
      scenario = scenario,
      simulation = NULL,
      results = NULL,
      population = NULL,
      stopIfFails = FALSE
    ),
    regexp = "No simulation results could be computed"
  )
  expect_null(out$outputValues)
  expect_null(out$results)
})

test_that("runScenarios aborts by default when a scenario simulation produces no results", {
  # Force a failed run without native infra: mock the runner to return a NULL
  # result for every simulation id it is handed.
  withr::local_options(lifecycle_verbosity = "quiet")
  project <- .testProject()
  local_mocked_bindings(
    runSimulations = function(simulations, ...) {
      ids <- if (inherits(simulations, "Simulation")) {
        simulations$id
      } else {
        vapply(simulations, function(s) s$id, character(1))
      }
      stats::setNames(vector("list", length(ids)), ids)
    }
  )
  expect_error(
    runScenarios(project, scenarios = "testscenario"),
    regexp = "No simulation results could be computed"
  )
})

test_that("runScenarios with stopIfFails = FALSE warns and returns NULL outputValues", {
  withr::local_options(lifecycle_verbosity = "quiet")
  project <- .testProject()
  local_mocked_bindings(
    runSimulations = function(simulations, ...) {
      ids <- if (inherits(simulations, "Simulation")) {
        simulations$id
      } else {
        vapply(simulations, function(s) s$id, character(1))
      }
      stats::setNames(vector("list", length(ids)), ids)
    }
  )
  expect_warning(
    out <- runScenarios(
      project,
      scenarios = "testscenario",
      stopIfFails = FALSE
    ),
    regexp = "No simulation results could be computed"
  )
  expect_null(out$testscenario$outputValues)
})

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
  scenario <- project$definitions$scenarios[["testscenario"]]
  scenario$modelParameterSets <- NULL
  scenario$individualId <- NULL
  scenario$applicationProtocol <- NA
  expect_null(
    esqlabsR:::.mergeScenarioParameters(scenario, project, customParams = NULL)
  )
})

test_that(".mergeScenarioParameters layer 1 (modelParameterSets) iterates listed groups in order", {
  project <- .testProject()
  scenario <- project$definitions$scenarios[["testscenario_steadystate"]]
  scenario$individualId <- NULL
  scenario$applicationProtocol <- NA
  merged <- esqlabsR:::.mergeScenarioParameters(scenario, project, NULL)
  expect_true("Organism|Liver|EHC continuous fraction" %in% merged$paths)
  expect_true("Aciclovir|Lipophilicity" %in% merged$paths)
})

test_that(".mergeScenarioParameters layer 4 (application) overrides layer 1 on overlapping path", {
  project <- .testProject()
  scenario <- project$definitions$scenarios[["testscenario"]]
  parameterSets <- .getSection(project, "parameterSets")
  parameterSets$global <- list(
    list(
      containerPath = "OverlapContainer",
      parameterName = "OverlapParam",
      value = 1,
      units = NULL
    )
  )
  parameterSets$override <- list(
    list(
      containerPath = "OverlapContainer",
      parameterName = "OverlapParam",
      value = 99,
      units = NULL
    )
  )
  .setSection(project, "parameterSets", parameterSets)
  apps <- .getSection(project, "applications")
  apps$aciclovir_iv_250mg$parameterSets <- list("override")
  .setSection(project, "applications", apps)
  scenario$individualId <- NULL
  merged <- esqlabsR:::.mergeScenarioParameters(scenario, project, NULL)
  idx <- match("OverlapContainer|OverlapParam", merged$paths)
  expect_equal(merged$values[idx], 99)
})

test_that(".mergeScenarioParameters layer 5 (customParams) wins over all earlier layers", {
  project <- .testProject()
  scenario <- project$definitions$scenarios[["testscenario"]]
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
  scenario <- project$definitions$scenarios[["testscenario"]]
  scenario$applicationProtocol <- NA
  scenario$modelParameterSets <- NULL
  scenario$individualId <- NULL
  expect_null(
    esqlabsR:::.mergeScenarioParameters(scenario, project, NULL)
  )
})

test_that(".mergeScenarioParameters errors when applicationProtocol is set but unknown", {
  project <- .testProject()
  scenario <- project$definitions$scenarios[["testscenario"]]
  scenario$applicationProtocol <- "DoesNotExist"
  expect_error(
    esqlabsR:::.mergeScenarioParameters(scenario, project, NULL),
    regexp = "DoesNotExist"
  )
})

test_that(".mergeScenarioParameters silently skips an unknown modelParameterSets group", {
  project <- .testProject()
  scenario <- project$definitions$scenarios[["testscenario"]]
  scenario$modelParameterSets <- c("global", "DoesNotExist")
  scenario$individualId <- NULL
  scenario$applicationProtocol <- NA
  merged <- esqlabsR:::.mergeScenarioParameters(scenario, project, NULL)
  expect_true("Organism|Liver|EHC continuous fraction" %in% merged$paths)
})

test_that(".mergeScenarioParameters silently skips an unknown individual parameter-set id", {
  project <- .testProject()
  scenario <- project$definitions$scenarios[["testscenario"]]
  individuals <- .getSection(project, "individuals")
  individuals[[1L]]$parameterSets <- list(
    "indiv1_default",
    "DoesNotExist"
  )
  .setSection(project, "individuals", individuals)
  scenario$modelParameterSets <- NULL
  scenario$applicationProtocol <- NA
  merged <- esqlabsR:::.mergeScenarioParameters(scenario, project, NULL)
  expect_true("Organism|Kidney|GFR" %in% merged$paths)
})

test_that(".mergeScenarioParameters silently skips an unknown application parameter-set id", {
  project <- .testProject()
  scenario <- project$definitions$scenarios[["testscenario"]]
  apps <- .getSection(project, "applications")
  apps$aciclovir_iv_250mg$parameterSets <- list(
    "aciclovir_iv_250mg_default",
    "DoesNotExist"
  )
  .setSection(project, "applications", apps)
  scenario$modelParameterSets <- NULL
  scenario$individualId <- NULL
  merged <- esqlabsR:::.mergeScenarioParameters(scenario, project, NULL)
  expect_true(
    "Events|IV 250mg 10min|Application_1|ProtocolSchemaItem|Dose" %in%
      merged$paths
  )
})

# Initial conditions merge ----

test_that(".initialConditionSetToStructure flattens records into paths/values/units", {
  records <- list(
    list(path = "Organism|A", value = 1.5, unit = "mg/l"),
    list(path = "Organism|B", value = 2, unit = NULL)
  )
  out <- esqlabsR:::.initialConditionSetToStructure(records)
  expect_equal(out$paths, c("Organism|A", "Organism|B"))
  expect_equal(out$values, c(1.5, 2))
  expect_equal(out$units, c("mg/l", ""))
})

test_that(".initialConditionSetToStructure returns NULL on empty input", {
  expect_null(esqlabsR:::.initialConditionSetToStructure(NULL))
  expect_null(esqlabsR:::.initialConditionSetToStructure(list()))
})

test_that(".mergeScenarioInitialConditions returns NULL when no set is referenced", {
  project <- .testProject()
  scenario <- project$definitions$scenarios[["testscenario"]]
  scenario$initialConditions <- NULL
  expect_null(
    esqlabsR:::.mergeScenarioInitialConditions(scenario, project)
  )
})

test_that(".mergeScenarioInitialConditions folds referenced sets last-write-wins", {
  project <- .testProject()
  addInitialConditions(project, "icset")
  suppressMessages(
    addInitialConditionEntry(
      project,
      "icset",
      path = c("Organism|A", "Organism|B"),
      value = c(1, 2),
      unit = c("mg/l", "µmol/l")
    )
  )
  scenario <- project$definitions$scenarios[["testscenario"]]
  scenario$initialConditions <- "icset"
  merged <- esqlabsR:::.mergeScenarioInitialConditions(scenario, project)
  expect_equal(merged$paths, c("Organism|A", "Organism|B"))
  expect_equal(merged$values, c(1, 2))
})

test_that(".mergeScenarioInitialConditions silently skips an unknown set id", {
  project <- .testProject()
  addInitialConditions(project, "icset")
  suppressMessages(
    addInitialConditionEntry(project, "icset", "Organism|A", 1, "mg/l")
  )
  scenario <- project$definitions$scenarios[["testscenario"]]
  scenario$initialConditions <- c("icset", "DoesNotExist")
  merged <- esqlabsR:::.mergeScenarioInitialConditions(scenario, project)
  expect_equal(merged$paths, "Organism|A")
})

test_that(".runScenariosFromProject returns the documented per-scenario list shape (individual)", {
  withr::local_options(lifecycle_verbosity = "quiet")
  project <- .testProject()
  out <- esqlabsR:::.runScenariosFromProject(
    project,
    scenarioNames = "testscenario"
  )
  expect_named(out, "testscenario")
  expect_named(
    out$testscenario,
    c("simulation", "results", "outputValues", "population")
  )
  expect_s3_class(out$testscenario$simulation, "Simulation")
  expect_null(out$testscenario$population)
})

test_that(".runScenariosFromProject runs a steady-state scenario without error", {
  withr::local_options(lifecycle_verbosity = "quiet")
  project <- .testProject()
  out <- esqlabsR:::.runScenariosFromProject(
    project,
    scenarioNames = "testscenario_steadystate"
  )
  expect_s3_class(out$testscenario_steadystate$simulation, "Simulation")
})

test_that(".runScenariosFromProject applies a scenario's initialConditions", {
  withr::local_options(lifecycle_verbosity = "quiet")
  project <- .testProject()
  path <- "Organism|VenousBlood|Plasma|Aciclovir"
  addInitialConditions(project, "icset")
  suppressMessages(
    addInitialConditionEntry(project, "icset", path, 7, "µmol")
  )
  setScenario(project, "testscenario", initialConditions = "icset")

  out <- esqlabsR:::.runScenariosFromProject(
    project,
    scenarioNames = "testscenario"
  )
  applied <- ospsuite::getQuantityValuesByPath(
    quantityPaths = path,
    simulation = out$testscenario$simulation,
    units = "µmol"
  )
  expect_equal(applied, 7)
})

test_that(".runScenariosFromProject runs a population scenario and attaches a Population", {
  withr::local_options(lifecycle_verbosity = "quiet")
  project <- .testProject()
  out <- esqlabsR:::.runScenariosFromProject(
    project,
    scenarioNames = "populationscenario"
  )
  expect_s3_class(out$populationscenario$population, "Population")
})

test_that(".runScenariosFromProject runs a CSV-population scenario and attaches a Population", {
  withr::local_options(lifecycle_verbosity = "quiet")
  project <- .testProject()
  out <- esqlabsR:::.runScenariosFromProject(
    project,
    scenarioNames = "populationscenariofromcsv"
  )
  expect_s3_class(out$populationscenariofromcsv$population, "Population")
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

test_that("a scenario with an absolute modelFile runs when simulationsFolder is NULL", {
  withr::local_options(lifecycle_verbosity = "quiet")
  project <- .testProject()
  absModel <- normalizePath(file.path(
    project$paths$simulationsFolder,
    "Aciclovir.pkml"
  ))
  setScenario(project, "testscenario", modelFile = absModel)
  project$paths$simulationsFolder <- NULL

  out <- esqlabsR:::.runScenariosFromProject(
    project,
    scenarioNames = "testscenario",
    validate = FALSE
  )
  expect_false(is.null(out$testscenario$outputValues))
})

test_that("a relative modelFile with NULL simulationsFolder aborts with a clear message", {
  withr::local_options(lifecycle_verbosity = "quiet")
  project <- .testProject()
  project$paths$simulationsFolder <- NULL
  expect_snapshot(
    error = TRUE,
    esqlabsR:::.runScenariosFromProject(
      project,
      scenarioNames = "testscenario",
      validate = FALSE
    )
  )
})

test_that("a relative modelFile that escapes the simulations folder is rejected", {
  withr::local_options(lifecycle_verbosity = "quiet")
  project <- .testProject()
  setScenario(project, "testscenario", modelFile = "../../../../etc/passwd")
  expect_error(
    esqlabsR:::.runScenariosFromProject(
      project,
      scenarioNames = "testscenario",
      validate = FALSE
    ),
    "resolves outside the project folder"
  )
})

# Population file resolution ----

test_that("a CSV-population scenario with NULL populationsFolder aborts with a clear message", {
  withr::local_options(lifecycle_verbosity = "quiet")
  project <- .testProject()
  project$paths$populationsFolder <- NULL
  expect_snapshot(
    error = TRUE,
    esqlabsR:::.runScenariosFromProject(
      project,
      scenarioNames = "populationscenariofromcsv",
      validate = FALSE
    )
  )
})
