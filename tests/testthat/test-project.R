# Project creation

test_that("default project can be initialized", {
  expect_no_error({
    testProject()
  })
})

# Project configurations

test_that("Configurations are loaded when project is initialized", {
  project <- testProject()

  expect_true(
    !is.null(project$configurations)
  )
})

# Project scenarios
test_that("All scenarios are available after project is initialized", {
  project <- testProject()

  expect_true(
    !is.null(project$availableScenarios)
  )
})

test_that("The user can select scenarios", {
  project <- testProject()

  subsetScenarios <- c("TestScenario", "TestScenario2")
  project$selectScenarios(subsetScenarios)

  expect_equal(
    project$activeScenarios,
    subsetScenarios
  )
})

test_that("All Scenarios are initialized when project is created", {
  project <- testProject()

  expect_named(
    project$scenarios,
    project$availableScenarios
  )
})


test_that("Scenarios are subsetted when user selects them", {
  project <- testProject()

  subsetScenarios <- c("TestScenario", "TestScenario2")
  project$selectScenarios(subsetScenarios)

  expect_named(
    project$scenarios,
    subsetScenarios
  )
})


# test_that("The user can run scenarios from project", {
#   project <- testProject()
#
#   subsetScenarios <- c("TestScenario", "TestScenario2")
#   project$setScenarioConfigurations(subsetScenarios)
#
#   project$runScenarios()
# })
