test_that("loadScenarioResults throws an error when files don't exist", {
  nonExistentFolder <- file.path(tempdir(), "non-existent-folder")

  expect_error(
    loadScenarioResults(
      scenarios = "testscenario",
      resultsFolder = nonExistentFolder
    )
  )
})

test_that("save/load round trip preserves the four-field record (individual)", {
  withr::local_options(lifecycle_verbosity = "quiet")
  project <- testProject()
  original <- runScenarios(project, scenarios = "testscenario")

  resultsFolder <- withr::local_tempdir()
  saveScenarioResults(original, project, outputFolder = resultsFolder)

  reloaded <- loadScenarioResults("testscenario", resultsFolder)

  expect_named(reloaded, "testscenario")
  expect_named(
    reloaded$testscenario,
    c("simulation", "results", "outputValues", "population")
  )
  expect_s3_class(reloaded$testscenario$simulation, "Simulation")
  expect_s3_class(reloaded$testscenario$results, "SimulationResults")
  expect_null(reloaded$testscenario$population)
  expect_equal(
    reloaded$testscenario$outputValues$data,
    original$testscenario$outputValues$data
  )
})

test_that("save/load round trip preserves population and outputValues (population)", {
  withr::local_options(lifecycle_verbosity = "quiet")
  project <- testProject()
  original <- runScenarios(project, scenarios = "populationscenario")

  resultsFolder <- withr::local_tempdir()
  saveScenarioResults(original, project, outputFolder = resultsFolder)

  reloaded <- loadScenarioResults("populationscenario", resultsFolder)

  expect_named(
    reloaded$populationscenario,
    c("simulation", "results", "outputValues", "population")
  )
  expect_s3_class(reloaded$populationscenario$population, "Population")
  expect_equal(
    reloaded$populationscenario$outputValues$data,
    original$populationscenario$outputValues$data
  )
})

test_that("saveScenarioResults reports the real error rather than a path warning", {
  withr::local_options(lifecycle_verbosity = "quiet")
  project <- testProject()
  original <- runScenarios(project, scenarios = "testscenario")

  # A simulated result whose `simulation` is not a Simulation forces
  # ospsuite::saveSimulation() to error; the warning must surface that error,
  # not a misleading "Cannot save to path" message.
  broken <- original
  broken$testscenario$simulation <- "not a simulation"

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

test_that("saveScenarioResults warning survives braces in the underlying error message", {
  withr::local_options(lifecycle_verbosity = "quiet")
  project <- testProject()
  original <- runScenarios(project, scenarios = "testscenario")

  # A broken simulation field triggers saveSimulation() to error; the error
  # message contains literal braces. Before the fix, cli_warn() tried to
  # re-interpret those braces as glue expressions and crashed.
  broken <- original
  broken$testscenario$simulation <- "not {a} simulation"

  resultsFolder <- withr::local_tempdir()
  expect_warning(
    saveScenarioResults(broken, project, outputFolder = resultsFolder),
    regexp = "Failed to save results for scenario"
  )
})

test_that("saveScenarioResults warning shows the original scenario name, not the path-sanitized one", {
  withr::local_options(lifecycle_verbosity = "quiet")
  project <- testProject()
  original <- runScenarios(project, scenarios = "testscenario")

  # Rename the entry to include a slash; saveScenarioResults replaces "/" with
  # "_" for file-path use. The warning must show the original "/" name, not the
  # sanitized "_" form.
  slashed <- setNames(original, "Group/A")
  slashed$`Group/A`$simulation <- "not a simulation"

  resultsFolder <- withr::local_tempdir()
  expect_warning(
    saveScenarioResults(slashed, project, outputFolder = resultsFolder),
    regexp = "Group/A",
    fixed = TRUE
  )
})
