# Project Initialization ---------------------------------------------------

test_that("default project can be initialized", {
  expect_no_error({
    testProject()
  })
})


# Project configuration --------------------------------------------------



test_that("Configurations are loaded when project is initialized", {
  project <- testProject()

  expect_true(
    !is.null(project$configurations)
  )
})

test_that("Configurations can be modified from project", {
  project <- testProject()

  project$configurations$scenarios$TestScenario$individual <- "Test Indiv"

  expect_equal(
    project$configurations$scenarios$TestScenario$individual,
    "Test Indiv"
  )

})

test_that("Configurations can be reloaded to default value",{
  project <- testProject()

  project$configurations$scenarios$TestScenario$individual <- "Test Indiv"


  project$reloadConfigurations()

  expect_equal(
    project$configurations$scenarios$TestScenario$individual,
    "Indiv1"
  )

})


# Project scenarios -----------------------------------------------------------

subsetScenarios <- c("TestScenario", "TestScenario2")


test_that("All scenarios are available after project is initialized", {
  project <- testProject()

  expect_snapshot(
    names(project$scenarios)
  )
})


test_that("All Scenarios are activated by default when project is created", {
  project <- testProject()

  expect_true(
    all(purrr::map_chr(project$scenarios, "status") == "active")
  )
})


test_that("The user can activate only some scenarios", {
  project <- testProject()

  project$selectScenarios(subsetScenarios)

  expect_snapshot(
    purrr::map_chr(project$scenarios, "status")
  )
})

test_that("Scenarios can be loaded manually", {
  project <- testProject()

  project$selectScenarios(subsetScenarios)

  project$loadScenarios()

  expect_snapshot(
    purrr::map_chr(project$scenarios, "status")
  )
})


test_that("Scenario can be run", {
  project <- testProject()

  project$selectScenarios(subsetScenarios)

  project$runScenarios()

  expect_snapshot(
    project$simulationResults
  )
})


test_that("No warnings during validation", {
  project <- testProject()
  expect_equal(project$get_warning_manager()$get_warnings(), list())
})
test_that("Individual is not defined", {
  project <- testProject()
  project$configurations$scenarios$TestScenario$individual <- "Non-declared individual"
  expect_equal(
    ls(project$get_warning_manager()$get_warnings()$TestScenario),
    "INDIVIDUAL_NOT_FOUND"
  )
})
test_that("Model is not defined", {
  project <- testProject()
  project$configurations$scenarios$TestScenario$model <- "Non-declared model"
  expect_equal(
    ls(project$get_warning_manager()$get_warnings()$TestScenario),
    "MODEL_NOT_FOUND"
  )
})


# Project export test ----------------------------------------------------------
test_that("exportToJSON(), missed export parameters", {
  p <- testProject()
  result <- p$exportToJSON(interactiveInput = FALSE)

  expect_null(result)
})


test_that("Export aborts with invalid directory in non-interactive mode", {
  p <- testProject()
  # Capture cli output using testthat::expect_message()
  expect_message(
    p$exportToJSON(interactiveInput = FALSE, dirPath = "invalid_path", fileName = "test"),
    "Invalid directory path. Export aborted."
  )
})

test_that("Interactive mode prompts correctly", {
  skip()
  skip_on_ci()

  p <- testProject()
  # Mock the readline inputs to simulate user input
  with_mock(
    `readline` = function(prompt) if (grepl("directory", prompt)) tempdir() else "test_project",
    {
      p$exportToJSON(interactiveInput = TRUE)

      # Check that the JSON file exists
      json_file <- file.path(tempdir(), "test_project.json")
      expect_true(file.exists(json_file))

      # Clean up by removing the JSON file
      file.remove(json_file)
    }
  )
})

