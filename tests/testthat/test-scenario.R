test_that("Scenario has correct default field values", {
  s <- Scenario$new()
  expect_null(s$scenarioName)
  expect_null(s$modelFile)
  expect_null(s$applicationProtocol)
  expect_null(s$individualId)
  expect_null(s$populationId)
  expect_null(s$outputPaths)
  expect_equal(s$simulationType, "Individual")
  expect_false(s$readPopulationFromCSV)
  expect_false(s$simulateSteadyState)
  expect_null(s$simulationTime)
  expect_null(s$simulationTimeUnit)
  expect_equal(s$steadyStateTime, 1000)
  expect_false(s$overwriteFormulasInSS)
  expect_null(s$parameterGroups)
})

test_that("Scenario fields can be set", {
  s <- Scenario$new()
  s$scenarioName <- "MyScenario"
  s$modelFile <- "model.pkml"
  s$individualId <- "Indiv1"
  s$parameterGroups <- c("Global", "Sheet2")
  expect_equal(s$scenarioName, "MyScenario")
  expect_equal(s$modelFile, "model.pkml")
  expect_equal(s$individualId, "Indiv1")
  expect_equal(s$parameterGroups, c("Global", "Sheet2"))
})

test_that("Scenario is cloneable", {
  s <- Scenario$new()
  s$scenarioName <- "Original"
  s2 <- s$clone()
  s2$scenarioName <- "Clone"
  expect_equal(s$scenarioName, "Original")
  expect_equal(s2$scenarioName, "Clone")
})

test_that("Scenario print method works", {
  s <- Scenario$new()
  s$scenarioName <- "TestScenario"
  s$modelFile <- "model.pkml"
  s$simulationType <- "Individual"
  expect_output(result <- s$print(), "TestScenario")
  expect_identical(result, s)
})
