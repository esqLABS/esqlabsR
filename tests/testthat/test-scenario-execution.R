# Local test helper delegating to helpers.R::testProject(). It forwards the
# calling test's frame so the throwaway project copy (a write-through definition
# tree) lives until the test finishes, not until this wrapper returns.
.testProject <- function(envir = parent.frame()) {
  testProject(envir = envir)
}

# Repoint the fixture's individual at a non-human species, so the bundled
# `SpeciesParameters.xlsx` actually contributes: it ships a sheet per animal
# species and none for `Human`. `Rat` carries ~245 paths, of which the human
# Aciclovir fixture model has no `Organism|EndogenousIgG|...` container, which
# is what makes it the species-defaults regression case. Biometrics are cleared
# down to a weight, all an animal individual needs.
.useRatIndividual <- function(project) {
  setIndividual(
    project,
    "indiv1",
    species = "Rat",
    population = NULL,
    gender = "UNKNOWN",
    weight = 0.25,
    height = NULL,
    age = NULL
  )
  project
}

# A `runSimulations` stand-in that returns a NULL result for every simulation
# id it is handed, forcing the "no results" collection path without native
# infra. Passed to `local_mocked_bindings(runSimulations = ...)`.
.mockNoResults <- function(simulations, ...) {
  ids <- if (inherits(simulations, "Simulation")) {
    simulations$id
  } else {
    vapply(simulations, function(s) s$id, character(1))
  }
  stats::setNames(vector("list", length(ids)), ids)
}

test_that(".buildSimulationRunOptions returns NULL when no defaults are declared", {
  expect_null(.buildSimulationRunOptions(NULL))
  expect_null(.buildSimulationRunOptions(list()))
})

test_that(".buildSimulationRunOptions maps the three settable fields", {
  opts <- .buildSimulationRunOptions(list(
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
  opts <- .buildSimulationRunOptions(list(numberOfCores = 1))
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
    .runScenariosFromProject(
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
    .runScenariosFromProject(
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
    .collectScenarioResult(
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
  # A scenario that built (non-NULL simulation) but produced no results. A
  # NULL simulation instead means a build-time skip, which already warned.
  expect_warning(
    out <- .collectScenarioResult(
      scenario = scenario,
      simulation = structure(list(id = "sim1"), class = "Simulation"),
      results = NULL,
      population = NULL,
      stopIfFails = FALSE
    ),
    regexp = "No simulation results could be computed"
  )
  expect_null(out$outputValues)
  expect_null(out$results)
})

test_that(".collectScenarioResult does not warn again for a build-time skip", {
  # A NULL simulation means the scenario never built; `.buildScenarioSimulations()`
  # already warned, so collection records it silently.
  scenario <- list(scenarioName = "s1", outputPaths = NULL)
  expect_no_warning(
    out <- .collectScenarioResult(
      scenario = scenario,
      simulation = NULL,
      results = NULL,
      population = NULL,
      stopIfFails = FALSE
    )
  )
  expect_null(out$simulation)
  expect_null(out$outputValues)
})

test_that("runScenarios aborts by default when a scenario simulation produces no results", {
  # Force a failed run without native infra: mock the runner to return a NULL
  # result for every simulation id it is handed.
  withr::local_options(lifecycle_verbosity = "quiet")
  project <- .testProject()
  local_mocked_bindings(
    runSimulations = .mockNoResults
  )
  expect_error(
    runScenarios(project, scenarios = "testscenario"),
    regexp = "No simulation results could be computed"
  )
})

test_that(".buildScenarioSimulations aborts on a build-time failure by default", {
  project <- .testProject()
  addScenario(project, "second", modelFile = "Aciclovir.pkml")
  local_mocked_bindings(
    .prepareScenario = function(scenario, ...) {
      if (scenario$scenarioName == "second") {
        cli::cli_abort("boom: missing model parameter path")
      }
      list(simulation = NULL, population = NULL)
    }
  )
  expect_error(
    .buildScenarioSimulations(project, stopIfFails = TRUE),
    regexp = "boom: missing model parameter path"
  )
})

test_that(".buildScenarioSimulations skips a build-time failure when stopIfFails is FALSE", {
  project <- .testProject()
  addScenario(project, "second", modelFile = "Aciclovir.pkml")
  local_mocked_bindings(
    .prepareScenario = function(scenario, ...) {
      if (scenario$scenarioName == "second") {
        cli::cli_abort("boom: missing model parameter path")
      }
      list(simulation = NULL, population = NULL)
    }
  )
  expect_warning(
    built <- .buildScenarioSimulations(project, stopIfFails = FALSE),
    regexp = "Could not build .*second.*skipping"
  )
  # The good scenario built; the broken one is a NULL entry, not an abort.
  expect_false(is.null(built$prepared$testscenario))
  expect_null(built$prepared$second)
})

test_that("runScenarios(stopIfFails = FALSE) skips a build-time failure and collects it as no-results", {
  # A scenario failing at build time is surfaced-and-skipped, not fatal: the
  # run reaches result collection, where the skipped scenario records no
  # results. `testscenario` builds for real (native infra) and the mocked
  # runner returns NULL for it, so both scenarios collect as no-results. The
  # broken scenario carries `outputPaths` so collection would call
  # getAllQuantitiesMatching() on its (NULL) simulation; a skipped scenario
  # must not crash there.
  withr::local_options(lifecycle_verbosity = "quiet")
  project <- .testProject()
  addScenario(
    project,
    "second",
    modelFile = "does-not-exist.pkml",
    outputPaths = "aciclovir_pvb"
  )
  local_mocked_bindings(
    runSimulations = .mockNoResults
  )
  out <- suppressWarnings(
    runScenarios(project, stopIfFails = FALSE)
  )
  expect_true(all(c("testscenario", "second") %in% names(out)))
  # The unaffected scenario really built (not skipped into the same NULL shape).
  expect_s3_class(out$testscenario$simulation, "Simulation")
  expect_null(out$second$simulation)
  expect_null(out$second$outputValues)
})

test_that("a scenario skipped at build time warns once, not twice", {
  # The build-time warning already names the scenario; the no-results
  # collection must not warn a second time for the same event.
  withr::local_options(lifecycle_verbosity = "quiet")
  project <- .testProject()
  addScenario(project, "second", modelFile = "does-not-exist.pkml")
  local_mocked_bindings(runSimulations = .mockNoResults)
  warnings <- character()
  withCallingHandlers(
    runScenarios(project, scenarios = "second", stopIfFails = FALSE),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_length(grep("Could not build", warnings), 1)
  expect_length(grep("No simulation results could be computed", warnings), 0)
})

test_that("runScenarios builds an individual that carries no age or height", {
  # An animal individual legitimately carries only a weight. Passing an absent
  # age/height through `as.double()` would yield `numeric(0)` and crash
  # `createIndividualCharacteristics()`. Clearing age/height here and reaching
  # the "no results" path (the run is short-circuited by the mocked runner)
  # proves the individual-characteristics build succeeded.
  withr::local_options(lifecycle_verbosity = "quiet")
  project <- .testProject()
  setIndividual(project, "indiv1", age = NULL, height = NULL)
  local_mocked_bindings(
    runSimulations = .mockNoResults
  )
  expect_warning(
    out <- runScenarios(
      project,
      scenarios = "testscenario",
      stopIfFails = FALSE
    ),
    regexp = "No simulation results could be computed"
  )
  # A built simulation (not a build-time skip) proves the age/height-less
  # individual made it through `createIndividualCharacteristics()`.
  expect_s3_class(out$testscenario$simulation, "Simulation")
})

test_that(".readOntogeniesFromList reads two ontogenies in every stored shape", {
  # The field reaches the runner in three shapes: one entry per ontogeny as a
  # character vector (the authoring shape), a single comma-joined cell (the
  # Excel spelling), and a list (what a JSON array parses to). All three must
  # yield the same two `MoleculeOntogeny` objects; testing the vector for
  # NA-ness first aborted on the vector length before reading any ontogeny.
  expected <- list(
    c(molecule = "CYP3A4", ontogeny = "CYP3A4"),
    c(molecule = "CYP2D6", ontogeny = "CYP2C8")
  )
  asPairs <- function(ontogenies) {
    lapply(ontogenies, function(o) {
      c(molecule = o$molecule, ontogeny = o$ontogeny)
    })
  }
  for (stored in list(
    c("CYP3A4:CYP3A4", "CYP2D6:CYP2C8"),
    "CYP3A4:CYP3A4,CYP2D6:CYP2C8",
    "CYP3A4:CYP3A4, CYP2D6:CYP2C8",
    list("CYP3A4:CYP3A4", "CYP2D6:CYP2C8")
  )) {
    expect_identical(asPairs(.readOntogeniesFromList(stored)), expected)
  }
})

test_that(".readOntogeniesFromList treats an unspecified field as no ontogenies", {
  expect_null(.readOntogeniesFromList(NULL))
  expect_null(.readOntogeniesFromList(NA))
  expect_null(.readOntogeniesFromList(""))
  expect_null(.readOntogeniesFromList(character(0)))
  # A trailing separator leaves a blank entry, which is nothing rather than a
  # malformed pair.
  expect_length(.readOntogeniesFromList("CYP3A4:CYP3A4,"), 1)
})

test_that("runScenarios builds an individual carrying two protein ontogenies", {
  # Two ontogenies on one individual, stored one entry per ontogeny. Reaching
  # the "no results" path (the run is short-circuited by the mocked runner)
  # proves both were read and `createIndividualCharacteristics()` accepted them.
  withr::local_options(lifecycle_verbosity = "quiet")
  project <- .testProject()
  setIndividual(
    project,
    "indiv1",
    proteinOntogenies = c("CYP3A4:CYP3A4", "CYP2D6:CYP2C8")
  )
  local_mocked_bindings(runSimulations = .mockNoResults)
  expect_warning(
    out <- runScenarios(
      project,
      scenarios = "testscenario",
      stopIfFails = FALSE
    ),
    regexp = "No simulation results could be computed"
  )
  expect_s3_class(out$testscenario$simulation, "Simulation")
})

test_that("runScenarios with stopIfFails = FALSE warns and returns NULL outputValues", {
  withr::local_options(lifecycle_verbosity = "quiet")
  project <- .testProject()
  local_mocked_bindings(
    runSimulations = .mockNoResults
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
  out <- .parameterSetToStructure(records)
  expect_equal(out$paths, c("A|B|P1", "A|C|P2"))
  expect_equal(out$values, c(1.5, 2))
  expect_equal(out$units, c("mg", ""))
})

test_that(".parameterSetToStructure returns NULL on empty input", {
  expect_null(.parameterSetToStructure(NULL))
  expect_null(.parameterSetToStructure(list()))
})

test_that(".mergeScenarioParameters returns NULL when no layer contributes", {
  project <- .testProject()
  scenario <- project$definitions$scenarios[["testscenario"]]
  scenario$modelParameterSets <- NULL
  scenario$individualId <- NULL
  scenario$applicationProtocol <- NA
  expect_null(
    .mergeScenarioParameters(scenario, project, customParams = NULL)
  )
})

test_that(".mergeScenarioParameters layer 1 (modelParameterSets) iterates listed groups in order", {
  project <- .testProject()
  scenario <- project$definitions$scenarios[["testscenario_steadystate"]]
  scenario$individualId <- NULL
  scenario$applicationProtocol <- NA
  merged <- .mergeScenarioParameters(scenario, project, NULL)
  expect_true("Organism|Liver|EHC continuous fraction" %in% merged$paths)
  expect_true("Aciclovir|Lipophilicity" %in% merged$paths)
})

test_that(".mergeScenarioParameters layer 3 (application) overrides layer 1 on overlapping path", {
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
  merged <- .mergeScenarioParameters(scenario, project, NULL)
  idx <- match("OverlapContainer|OverlapParam", merged$paths)
  expect_equal(merged$values[idx], 99)
})

test_that(".mergeScenarioParameters layer 4 (customParams) wins over all earlier layers", {
  project <- .testProject()
  scenario <- project$definitions$scenarios[["testscenario"]]
  customParams <- list(
    paths = "Organism|Liver|EHC continuous fraction",
    values = 42,
    units = ""
  )
  merged <- .mergeScenarioParameters(scenario, project, customParams)
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
    .mergeScenarioParameters(scenario, project, NULL)
  )
})

test_that(".mergeScenarioParameters errors when applicationProtocol is set but unknown", {
  project <- .testProject()
  scenario <- project$definitions$scenarios[["testscenario"]]
  scenario$applicationProtocol <- "DoesNotExist"
  expect_error(
    .mergeScenarioParameters(scenario, project, NULL),
    regexp = "DoesNotExist"
  )
})

test_that(".mergeScenarioParameters silently skips an unknown modelParameterSets group", {
  project <- .testProject()
  scenario <- project$definitions$scenarios[["testscenario"]]
  scenario$modelParameterSets <- c("global", "DoesNotExist")
  scenario$individualId <- NULL
  scenario$applicationProtocol <- NA
  merged <- .mergeScenarioParameters(scenario, project, NULL)
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
  merged <- .mergeScenarioParameters(scenario, project, NULL)
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
  merged <- .mergeScenarioParameters(scenario, project, NULL)
  expect_true(
    "Events|IV 250mg 10min|Application_1|ProtocolSchemaItem|Dose" %in%
      merged$paths
  )
})

# Species defaults vs. user parameters ----
#
# The bundled species sheet and the user's own parameters are applied
# separately, with opposite strictness. Both halves are asserted on the same
# Rat scenario, since the point is that one run treats the two differently.

test_that("a bundled species path the model lacks does not stop the build", {
  # The Rat sheet carries `Organism|EndogenousIgG|...`, which the human
  # Aciclovir fixture has no container for. Building under the default
  # `stopIfParameterNotFound = TRUE` must still succeed: the sheet is
  # package-shipped and covers every model of the species, so a path this model
  # lacks is normal. Needing `stopIfParameterNotFound = FALSE` here would be
  # the regression.
  withr::local_options(lifecycle_verbosity = "quiet")
  project <- .useRatIndividual(.testProject())
  built <- buildSimulations(project, scenarios = "testscenario")
  expect_s3_class(built$testscenario$simulation, "Simulation")
})

test_that("the species sheet is applied once, not once per layer", {
  # Each application of a sheet path the model lacks emits one native
  # "Could not find quantity" warning, so a duplicated application shows up as
  # a doubled count for the single missing Rat path.
  withr::local_options(lifecycle_verbosity = "quiet")
  project <- .useRatIndividual(.testProject())
  applied <- 0L
  local_mocked_bindings(
    .getSpeciesParameters = function(species) {
      applied <<- applied + 1L
      NULL
    }
  )
  buildSimulations(project, scenarios = "testscenario")
  expect_identical(applied, 1L)
})

test_that("a user parameter path the model lacks still stops the build", {
  # The other half: silently ignoring a path the user wrote themselves would
  # hide a real mistake in their project, so the merged user layers stay strict
  # even though the species sheet above does not.
  withr::local_options(lifecycle_verbosity = "quiet")
  project <- .useRatIndividual(.testProject())
  # ospsuite's message carries the name of the outermost calling function, which
  # is the test runner and so differs between `testthat::test_file()`,
  # `devtools::test()` and `R CMD check`. Scrub only that bullet's call name,
  # leaving the `.validateEntitiesExist()` attribution and the offending path
  # asserted.
  expect_snapshot(
    buildSimulations(
      project,
      scenarios = "testscenario",
      customParams = list(
        paths = "Organism|NoSuchContainer|NoSuchParameter",
        values = 1,
        units = ""
      )
    ),
    error = TRUE,
    transform = \(lines) {
      gsub("^(\\s*! )`[^`]+\\(\\)`:", "\\1`<caller>`:", lines)
    }
  )
})

# Initial conditions merge ----

test_that(".initialConditionSetToStructure flattens records into paths/values/units", {
  records <- list(
    list(path = "Organism|A", value = 1.5, unit = "mg/l"),
    list(path = "Organism|B", value = 2, unit = NULL)
  )
  out <- .initialConditionSetToStructure(records)
  expect_equal(out$paths, c("Organism|A", "Organism|B"))
  expect_equal(out$values, c(1.5, 2))
  expect_equal(out$units, c("mg/l", ""))
})

test_that(".initialConditionSetToStructure returns NULL on empty input", {
  expect_null(.initialConditionSetToStructure(NULL))
  expect_null(.initialConditionSetToStructure(list()))
})

test_that(".mergeScenarioInitialConditions returns NULL when no set is referenced", {
  project <- .testProject()
  scenario <- project$definitions$scenarios[["testscenario"]]
  scenario$initialConditions <- NULL
  expect_null(
    .mergeScenarioInitialConditions(scenario, project)
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
  merged <- .mergeScenarioInitialConditions(scenario, project)
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
  merged <- .mergeScenarioInitialConditions(scenario, project)
  expect_equal(merged$paths, "Organism|A")
})

test_that(".runScenariosFromProject returns the documented per-scenario list shape (individual)", {
  withr::local_options(lifecycle_verbosity = "quiet")
  project <- .testProject()
  out <- .runScenariosFromProject(
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
  out <- .runScenariosFromProject(
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

  out <- .runScenariosFromProject(
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
  out <- .runScenariosFromProject(
    project,
    scenarioNames = "populationscenario"
  )
  expect_s3_class(out$populationscenario$population, "Population")
})

test_that(".runScenariosFromProject runs a CSV-population scenario and attaches a Population", {
  withr::local_options(lifecycle_verbosity = "quiet")
  project <- .testProject()
  out <- .runScenariosFromProject(
    project,
    scenarioNames = "populationscenariofromcsv"
  )
  expect_s3_class(out$populationscenariofromcsv$population, "Population")
})

test_that(".runScenariosFromProject errors on unknown scenarioNames", {
  withr::local_options(lifecycle_verbosity = "quiet")
  project <- .testProject()
  expect_error(
    .runScenariosFromProject(project, scenarioNames = "NopeNope"),
    regexp = "nopenope"
  )
})

# .buildScenarioSimulations ----

test_that(".buildScenarioSimulations resolves NULL to every scenario and returns the prep shape", {
  withr::local_options(lifecycle_verbosity = "quiet")
  project <- .testProject()
  # Building every scenario includes the fixture's two population scenarios,
  # which share one population id and resolve it two ways; that warning is
  # asserted on its own below.
  built <- suppressWarnings(.buildScenarioSimulations(project))
  expect_setequal(built$scenarioNames, names(project$definitions$scenarios))
  expect_named(built$prepared, built$scenarioNames)
  first <- built$prepared[[1]]
  expect_named(first, c("simulation", "population"))
  expect_s3_class(first$simulation, "Simulation")
})

test_that(".buildScenarioSimulations errors on an unknown scenario name", {
  withr::local_options(lifecycle_verbosity = "quiet")
  project <- .testProject()
  expect_error(
    .buildScenarioSimulations(project, scenarioNames = "NopeNope"),
    regexp = "nopenope"
  )
})

test_that(".buildScenarioSimulations matches a scenario name case-insensitively and warns on the rewrite", {
  # A caller passing the mixed-case name they authored with resolves to the
  # canonical (lowercased) id the scenario is filed under; the rewrite warns,
  # naming the id it resolved to, so a mistyped label is surfaced.
  withr::local_options(lifecycle_verbosity = "quiet")
  project <- .testProject()
  canonical <- names(project$definitions$scenarios)[[1]]
  built <- NULL
  expect_warning(
    built <- .buildScenarioSimulations(
      project,
      scenarioNames = toupper(canonical)
    ),
    regexp = canonical
  )
  expect_identical(built$scenarioNames, canonical)
})

test_that(".buildScenarioSimulations shares one build cache across scenarios in a call", {
  # Two scenarios referencing the same individual must build its
  # IndividualCharacteristics once, not once per scenario.
  withr::local_options(lifecycle_verbosity = "quiet")
  project <- .testProject()
  duplicateScenario(project, "testscenario", "testscenario_twin")

  built <- 0L
  realCreateIC <- ospsuite::createIndividualCharacteristics
  local_mocked_bindings(
    createIndividualCharacteristics = function(...) {
      built <<- built + 1L
      realCreateIC(...)
    },
    .package = "ospsuite"
  )
  .buildScenarioSimulations(
    project,
    scenarioNames = c("testscenario", "testscenario_twin")
  )
  expect_identical(built, 1L)
})

# Population source resolution ----

test_that(".resolveScenarioPopulation resolves a programmatic entry from the runtime store", {
  withr::local_options(lifecycle_verbosity = "quiet")
  project <- .testProject()
  pc <- ospsuite::createPopulationCharacteristics(
    species = ospsuite::Species$Human,
    population = ospsuite::HumanPopulation$Asian_Tanaka_1996,
    numberOfIndividuals = 2L,
    proportionOfFemales = 50
  )
  pop <- ospsuite::createPopulation(pc)$population

  suppressWarnings(removePopulation(project, "testpopulation"))
  suppressMessages(addPopulation(project, "testpopulation", pop))

  out <- .runScenariosFromProject(
    project,
    scenarioNames = "populationscenario"
  )
  expect_identical(out$populationscenario$population, pop)
})

test_that(".resolveScenarioPopulation aborts on an unresolved programmatic entry", {
  withr::local_options(lifecycle_verbosity = "quiet")
  project <- .testProject()
  # A programmatic sentinel with no backing object (as after a reload):
  # replace the existing spec entry so the lookup hits the sentinel.
  populations <- .getSection(project, "populations")
  populations[["testpopulation"]] <- .asPopulationSource(list(
    type = "programmatic"
  ))
  .setSection(project, "populations", populations)
  scenario <- project$definitions$scenarios[["populationscenario"]]
  cache <- new.env(parent = emptyenv())
  cache$populations <- list()
  expect_error(
    .resolveScenarioPopulation(scenario, project, cache),
    regexp = "injected in a previous session"
  )
})

test_that(".resolveScenarioPopulation keeps a spec and a csv population apart under one id, in either order", {
  # `populationscenario` and `populationscenariofromcsv` share the population id
  # `testpopulation`, and only the second sets `readPopulationFromCSV`, so one id
  # resolves two ways in one batch. Cached on the id alone, whichever scenario
  # ran first won and both got its population. The spec is set to 7 individuals
  # against the fixture csv's 2 so a crossover is visible in the counts.
  withr::local_options(lifecycle_verbosity = "quiet")
  counts <- function(scenarioNames) {
    project <- .testProject()
    setPopulation(project, "testpopulation", numberOfIndividuals = 7)
    built <- suppressWarnings(
      .buildScenarioSimulations(project, scenarioNames = scenarioNames)
    )
    vapply(built$prepared, function(p) p$population$count, integer(1))
  }

  specFirst <- counts(c("populationscenario", "populationscenariofromcsv"))
  csvFirst <- counts(c("populationscenariofromcsv", "populationscenario"))

  expect_identical(specFirst[["populationscenario"]], 7L)
  expect_identical(specFirst[["populationscenariofromcsv"]], 2L)
  expect_identical(csvFirst[["populationscenario"]], 7L)
  expect_identical(csvFirst[["populationscenariofromcsv"]], 2L)
})

test_that(".resolveScenarioPopulation warns when one id resolves two ways in a run", {
  withr::local_options(lifecycle_verbosity = "quiet")
  project <- .testProject()
  cache <- new.env(parent = emptyenv())
  cache$populations <- list()

  .resolveScenarioPopulation(
    project$definitions$scenarios[["populationscenario"]],
    project,
    cache
  )
  expect_snapshot(
    invisible(.resolveScenarioPopulation(
      project$definitions$scenarios[["populationscenariofromcsv"]],
      project,
      cache
    ))
  )
})

test_that(".resolveScenarioPopulation gives an injected population to a readPopulationFromCSV scenario", {
  # A `programmatic` entry carries an explicit `type`, so the scenario flag
  # cannot turn it into a csv read, and its cache key cannot collide with a spec
  # or csv population of the same id.
  withr::local_options(lifecycle_verbosity = "quiet")
  project <- .testProject()
  pop <- ospsuite::createPopulation(ospsuite::createPopulationCharacteristics(
    species = ospsuite::Species$Human,
    population = ospsuite::HumanPopulation$European_ICRP_2002,
    numberOfIndividuals = 3L,
    proportionOfFemales = 50
  ))$population
  suppressWarnings(removePopulation(project, "testpopulation"))
  suppressMessages(addPopulation(project, "testpopulation", pop))

  built <- .buildScenarioSimulations(
    project,
    scenarioNames = c("populationscenario", "populationscenariofromcsv")
  )
  expect_identical(built$prepared$populationscenario$population, pop)
  expect_identical(built$prepared$populationscenariofromcsv$population, pop)
})

test_that(".resolveScenarioPopulation loads a csv entry from its own file", {
  withr::local_options(lifecycle_verbosity = "quiet")
  project <- .testProject()
  loaded <- structure(list(), class = "Population")
  entry <- .asPopulationSource(list(
    type = "csv",
    file = "custom.csv"
  ))
  file.create(file.path(project$paths$populationsFolder, "custom.csv"))
  .setSection(
    project,
    "populations",
    list(testpopulation = entry)
  )
  scenario <- project$definitions$scenarios[["populationscenario"]]
  cache <- new.env(parent = emptyenv())
  cache$populations <- list()

  seen <- NULL
  local_mocked_bindings(
    loadPopulation = function(path) {
      seen <<- path
      loaded
    }
  )
  result <- .resolveScenarioPopulation(scenario, project, cache)
  expect_identical(result, loaded)
  expect_match(seen, "custom\\.csv$")
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

  out <- .runScenariosFromProject(
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
    .runScenariosFromProject(
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
    .runScenariosFromProject(
      project,
      scenarioNames = "testscenario",
      validate = FALSE
    ),
    "resolves outside the project folder"
  )
})

# Population file resolution ----

test_that("a CSV-population scenario names the missing csv file rather than failing in the backend", {
  # An absent file used to reach the backend, which aborts with a raw .NET
  # exception naming neither the scenario nor the folder (#1213).
  withr::local_options(lifecycle_verbosity = "quiet")
  project <- .testProject()
  file.remove(file.path(
    project$paths$populationsFolder,
    "testpopulation.csv"
  ))
  expect_error(
    .runScenariosFromProject(
      project,
      scenarioNames = "populationscenariofromcsv",
      validate = FALSE
    ),
    "testpopulation\\.csv"
  )
})

test_that("a CSV population resolves under the case its file is spelled with", {
  # The id is canonical (lowercase) and the file keeps the author's spelling, so
  # reading it must not depend on a case-insensitive filesystem (#1213).
  withr::local_options(lifecycle_verbosity = "quiet")
  project <- .testProject()
  folder <- project$paths$populationsFolder
  file.rename(
    file.path(folder, "testpopulation.csv"),
    file.path(folder, "TestPopulation.csv")
  )
  expect_no_error(
    .runScenariosFromProject(
      project,
      scenarioNames = "populationscenariofromcsv",
      validate = FALSE
    )
  )
})

test_that("a CSV-population scenario with NULL populationsFolder aborts with a clear message", {
  withr::local_options(lifecycle_verbosity = "quiet")
  project <- .testProject()
  project$paths$populationsFolder <- NULL
  expect_snapshot(
    error = TRUE,
    .runScenariosFromProject(
      project,
      scenarioNames = "populationscenariofromcsv",
      validate = FALSE
    )
  )
})

# Legacy Excel projects (#1213) ----

# #1213 item 2: the ontogenies the legacy fixture declares are what the runner
# actually receives once the importer reads them, one pair or two, in the
# comma-joined spelling the import folds them into.
test_that("one ontogeny, and a comma-joined pair of them, read fine", {
  expect_length(.readOntogeniesFromList("CYP3A4:CYP3A4"), 1L)
  expect_length(.readOntogeniesFromList("CYP3A4:CYP3A4,CYP2D6:CYP2C8"), 2L)
})

# #1213 item 6: a CSV population entry that names no `file` has its filename
# derived as `<populationId>.csv`, and the id is canonicalized, which lowercases
# it, while the copied file keeps the mixed case the workbook's author gave it.
# The derived name is matched against the folder listing case-insensitively, so
# the two spellings meet on a case-sensitive filesystem as well.
#
# Asserted on the strings and then on the resolved name, so the test states the
# case mismatch on every platform instead of passing on one and failing on
# another.
test_that("the derived population CSV filename resolves to the mixed-case file on disk", {
  projectDir <- localLegacyExcelProject()
  imported <- importLegacyExcelProject(projectDir)
  scenario <- imported$project$definitions$scenarios[["csvpopscenario"]]

  # The scenario reads its population from CSV, and the entry names no file, so
  # the runner derives one from the id.
  expect_true(scenario$readPopulationFromCSV)
  expect_null(imported$project$definitions$populations[["csvpop"]]$file)

  populationsFolder <- imported$project$paths$populationsFolder
  present <- list.files(populationsFolder)
  derived <- paste0(scenario$populationId, ".csv")

  expect_identical(present, "CsvPop.csv")
  expect_identical(derived, "csvpop.csv")

  # The two differ in case, and the file the run opens is the one on disk.
  expect_false(derived %in% present)
  expect_identical(
    .populationCsvFileName(scenario$populationId, populationsFolder),
    "CsvPop.csv"
  )
})
