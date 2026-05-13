test_that("loadScenarioResults throws an error when files don't exist", {
  nonExistentFolder <- file.path(tempdir(), "non-existent-folder")

  expect_error(
    loadScenarioResults(
      scenarioNames = "TestScenario",
      resultsFolder = nonExistentFolder
    )
  )
})
