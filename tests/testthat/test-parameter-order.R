# Layer merge order in .prepareScenario:
#   1. modelParameters (iterated in scenario$modelParameters order)
#   2. species defaults (from bundled SpeciesParameters.xlsx)
#   3. individual inline parameters (project$individuals[[id]]$parameters)
#   4. application inline parameters (project$applications[[id]]$parameters)
#   5. customParams (caller-supplied)
# Last write wins, silent.

# All tests construct a minimal in-memory Project where every layer sets the
# *same* parameter path to a different value and assert which value survives.

# Helper: build a minimal project with one model parameter set, one individual,
# one application, one scenario referencing all three. PARAM is the single
# parameter path used across layers.
PARAM_PATH <- "Organism|Liver|Volume"
PARAM_CONTAINER <- "Organism|Liver"
PARAM_NAME <- "Volume"

.makeOrderTestProject <- function(
  modelParamsValues = NULL, # named list: setId -> value
  individualValue = NULL, # numeric scalar or NULL
  applicationValue = NULL, # numeric scalar or NULL
  scenarioModelParamIds = character() # ordered ids referenced by scenario
) {
  pc <- Project$new()
  pc$modelFolder <- tempdir()

  # Build model parameter sets
  for (setId in names(modelParamsValues)) {
    addModelParameter(
      pc,
      id = setId,
      containerPath = PARAM_CONTAINER,
      parameterName = PARAM_NAME,
      value = modelParamsValues[[setId]],
      units = "L"
    )
  }

  # Add individual (with optional parameter)
  addIndividual(pc, "I1", species = "Human")
  if (!is.null(individualValue)) {
    addIndividualParameter(
      pc, "I1",
      containerPath = PARAM_CONTAINER,
      parameterName = PARAM_NAME,
      value = individualValue,
      units = "L"
    )
  }

  # Add application (with optional parameter)
  addApplication(pc, "App1")
  if (!is.null(applicationValue)) {
    addApplicationParameter(
      pc, "App1",
      containerPath = PARAM_CONTAINER,
      parameterName = PARAM_NAME,
      value = applicationValue,
      units = "L"
    )
  }

  # Add scenario referencing the model parameter sets in the requested order
  addScenario(
    pc, "S1", "model.pkml",
    individualId = "I1",
    applicationProtocol = "App1",
    modelParameters = scenarioModelParamIds
  )

  pc
}

# Helper: extract the merged value for PARAM_PATH from .prepareScenario,
# bypassing the simulation load (which needs a real pkml). We patch
# ospsuite::loadSimulation to return a mock so we can call .prepareScenario
# end-to-end and read the merged params off the simulation state.
#
# Simpler approach: extract just the merge-loop portion. We introduce a
# helper .mergeScenarioParameters in Task 2 that returns the merged structure
# without loading the simulation; we test that helper directly.

.mergedValueFor <- function(pc, customParams = NULL) {
  scenario <- pc$scenarios[["S1"]]
  merged <- .mergeScenarioParameters(scenario, pc, customParams)
  idx <- which(merged$paths == PARAM_PATH)
  if (length(idx) == 0) NA_real_ else merged$values[[idx]]
}

test_that("within modelParameters, later set overrides earlier", {
  pc <- .makeOrderTestProject(
    modelParamsValues = list(SetA = 10, SetB = 20),
    scenarioModelParamIds = c("SetA", "SetB")
  )
  expect_equal(.mergedValueFor(pc), 20)

  pc2 <- .makeOrderTestProject(
    modelParamsValues = list(SetA = 10, SetB = 20),
    scenarioModelParamIds = c("SetB", "SetA")
  )
  expect_equal(.mergedValueFor(pc2), 10)
})

test_that("individual inline parameters override modelParameters layer", {
  pc <- .makeOrderTestProject(
    modelParamsValues = list(Set = 10),
    individualValue = 99,
    scenarioModelParamIds = "Set"
  )
  expect_equal(.mergedValueFor(pc), 99)
})

test_that("application inline parameters override individual inline parameters", {
  pc <- .makeOrderTestProject(
    modelParamsValues = list(Set = 10),
    individualValue = 99,
    applicationValue = 7,
    scenarioModelParamIds = "Set"
  )
  expect_equal(.mergedValueFor(pc), 7)
})

test_that("customParams override application inline parameters", {
  pc <- .makeOrderTestProject(
    modelParamsValues = list(Set = 10),
    individualValue = 99,
    applicationValue = 7,
    scenarioModelParamIds = "Set"
  )
  custom <- list(paths = PARAM_PATH, values = 1, units = "L")
  expect_equal(.mergedValueFor(pc, customParams = custom), 1)
})

test_that("layer that doesn't set the path doesn't disturb other layers", {
  # Only individual sets the path; everything else absent.
  pc <- .makeOrderTestProject(individualValue = 42)
  expect_equal(.mergedValueFor(pc), 42)
})

test_that("merge produces correct path|value|units triplets", {
  pc <- .makeOrderTestProject(
    modelParamsValues = list(Set = 10),
    scenarioModelParamIds = "Set"
  )
  scenario <- pc$scenarios[["S1"]]
  merged <- .mergeScenarioParameters(scenario, pc, customParams = NULL)
  expect_setequal(c("paths", "values", "units"), names(merged))
  expect_equal(length(merged$paths), length(merged$values))
  expect_equal(length(merged$paths), length(merged$units))
})
