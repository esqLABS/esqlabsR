test_that("loadScenarioResults throws an error when files don't exist", {
  nonExistentFolder <- file.path(tempdir(), "non-existent-folder")

  expect_error(
    loadScenarioResults(
      scenarioNames = "TestScenario",
      resultsFolder = nonExistentFolder
    )
  )
})

test_that("save/load round trip preserves the four-field record (individual)", {
  withr::local_options(lifecycle_verbosity = "quiet")
  project <- testProject()
  original <- runScenarios(project, scenarioNames = "TestScenario")

  resultsFolder <- withr::local_tempdir()
  saveScenarioResults(original, project, outputFolder = resultsFolder)

  reloaded <- loadScenarioResults("TestScenario", resultsFolder)

  expect_named(reloaded, "TestScenario")
  expect_named(
    reloaded$TestScenario,
    c("simulation", "results", "outputValues", "population")
  )
  expect_s3_class(reloaded$TestScenario$simulation, "Simulation")
  expect_s3_class(reloaded$TestScenario$results, "SimulationResults")
  expect_null(reloaded$TestScenario$population)
  expect_equal(
    reloaded$TestScenario$outputValues$data,
    original$TestScenario$outputValues$data
  )
})

test_that("save/load round trip preserves population and outputValues (population)", {
  withr::local_options(lifecycle_verbosity = "quiet")
  project <- testProject()
  original <- runScenarios(project, scenarioNames = "PopulationScenario")

  resultsFolder <- withr::local_tempdir()
  saveScenarioResults(original, project, outputFolder = resultsFolder)

  reloaded <- loadScenarioResults("PopulationScenario", resultsFolder)

  expect_named(
    reloaded$PopulationScenario,
    c("simulation", "results", "outputValues", "population")
  )
  expect_s3_class(reloaded$PopulationScenario$population, "Population")
  expect_equal(
    reloaded$PopulationScenario$outputValues$data,
    original$PopulationScenario$outputValues$data
  )
})

test_that("saveScenarioResults reports the real error rather than a path warning", {
  withr::local_options(lifecycle_verbosity = "quiet")
  project <- testProject()
  original <- runScenarios(project, scenarioNames = "TestScenario")

  # A simulated result whose `simulation` is not a Simulation forces
  # ospsuite::saveSimulation() to error; the warning must surface that error,
  # not a misleading "Cannot save to path" message.
  broken <- original
  broken$TestScenario$simulation <- "not a simulation"

  resultsFolder <- withr::local_tempdir()
  # The embedded error carries the calling-function name from
  # ospsuite.utils::validateIsOfType, which differs between `devtools::test()`
  # and `R CMD check`. Scrub it to a stable placeholder so the snapshot stays
  # harness-independent while still asserting the scenario name and real error.
  expect_snapshot(
    invisible(saveScenarioResults(
      broken,
      project,
      outputFolder = resultsFolder
    )),
    transform = \(lines) gsub("`[^`]+\\(\\)`:", "`<caller>`:", lines)
  )
})
