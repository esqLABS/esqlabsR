# Round-trip ----

test_that("saveProject produces round-trip fidelity for scenarios", {
  project <- testProject()
  tmp <- tempfile(fileext = ".json")
  withr::defer(unlink(tmp))

  saveProject(project, tmp)
  pc2 <- loadProject(tmp)

  expect_equal(names(project$scenarios), names(pc2$scenarios))

  sc1 <- project$scenarios[["TestScenario2"]]
  sc2 <- pc2$scenarios[["TestScenario2"]]
  expect_equal(sc1$scenarioName, sc2$scenarioName)
  expect_equal(sc1$modelFile, sc2$modelFile)
  expect_equal(sc1$individualId, sc2$individualId)
  expect_equal(sc1$applicationProtocol, sc2$applicationProtocol)
  expect_equal(sc1$simulateSteadyState, sc2$simulateSteadyState)
  expect_equal(sc1$modelParameters, sc2$modelParameters)
})

test_that("saveProject persists scenario added via addScenario", {
  tmpDir <- withr::local_tempdir()
  jsonPath <- file.path(tmpDir, "Project.json")
  file.copy(testProjectJSONPath(), jsonPath)

  project <- loadProject(jsonPath)
  addScenario(
    project = project,
    scenarioName = "ProgrammaticScenario",
    modelFile = "Aciclovir.pkml",
    individualId = "Indiv1",
    applicationProtocol = "Aciclovir_iv_250mg"
  )

  saveProject(project, jsonPath)

  pc2 <- loadProject(jsonPath)
  expect_true("ProgrammaticScenario" %in% names(pc2$scenarios))
  expect_equal(
    pc2$scenarios[["ProgrammaticScenario"]]$individualId,
    "Indiv1"
  )
})

# Public CRUD: scenarios ----

test_that("addScenario errors on duplicate scenario name", {
  project <- testProject()
  existing_name <- names(project$scenarios)[[1]]
  expect_error(
    addScenario(
      project,
      scenarioName = existing_name,
      modelFile = "model.pkml"
    ),
    regexp = "already exists"
  )
})

test_that("addScenario errors on invalid individualId", {
  project <- testProject()
  expect_error(
    addScenario(
      project,
      scenarioName = "BadIndiv",
      modelFile = "model.pkml",
      individualId = "NonExistentIndividual"
    ),
    regexp = "individualId.*not found"
  )
})

test_that("addScenario collects all validation errors in one message", {
  project <- testProject()
  expect_error(
    addScenario(
      project,
      scenarioName = "",
      modelFile = "",
      individualId = "BadIndiv"
    ),
    regexp = "scenarioName.*modelFile|modelFile.*scenarioName"
  )
})

test_that("addScenario errors on empty scenarioName", {
  project <- testProject()
  expect_error(
    addScenario(
      project,
      scenarioName = "",
      modelFile = "model.pkml"
    ),
    regexp = "scenarioName"
  )
})

# Happy path ----

test_that("addScenario adds a valid scenario with correct fields", {
  project <- testProject()
  addScenario(
    project,
    scenarioName = "NewScenario",
    modelFile = "Aciclovir.pkml",
    individualId = "Indiv1",
    simulationTime = "0, 100, 100",
    simulationTimeUnit = "h"
  )

  expect_true("NewScenario" %in% names(project$scenarios))
  newScenario <- project$scenarios[["NewScenario"]]
  expect_s3_class(newScenario, "Scenario")
  expect_equal(newScenario$scenarioName, "NewScenario")
  expect_equal(newScenario$modelFile, "Aciclovir.pkml")
  expect_equal(newScenario$individualId, "Indiv1")
  expect_equal(newScenario$simulationType, "Individual")
})

test_that("addScenario with populationId sets simulationType to Population", {
  project <- testProject()
  addScenario(
    project,
    scenarioName = "PopScenario",
    modelFile = "Aciclovir.pkml",
    individualId = "Indiv1",
    populationId = "TestPopulation"
  )

  newScenario <- project$scenarios[["PopScenario"]]
  expect_equal(newScenario$populationId, "TestPopulation")
  expect_equal(newScenario$simulationType, "Population")
})

test_that("addScenario parses simulationTime string into list of vectors", {
  project <- testProject()
  addScenario(
    project,
    scenarioName = "TimeScenario",
    modelFile = "Aciclovir.pkml",
    individualId = "Indiv1",
    simulationTime = "0, 50, 100"
  )

  newScenario <- project$scenarios[["TimeScenario"]]
  expect_type(newScenario$simulationTime, "list")
  expect_equal(newScenario$simulationTime[[1]], c(0, 50, 100))
})

test_that("addScenario resolves outputPathIds to output path strings", {
  project <- testProject()

  outputPathIds <- names(project$outputPaths)[1:2]
  addScenario(
    project,
    scenarioName = "OutputScenario",
    modelFile = "Aciclovir.pkml",
    individualId = "Indiv1",
    outputPathIds = outputPathIds
  )

  newScenario <- project$scenarios[["OutputScenario"]]
  expect_equal(
    newScenario$outputPaths,
    unname(project$outputPaths[outputPathIds])
  )
})

test_that("addScenario sets modified flag to TRUE", {
  project <- testProject()
  expect_false(project$modified)

  addScenario(
    project,
    scenarioName = "ModifiedScenario",
    modelFile = "Aciclovir.pkml",
    individualId = "Indiv1"
  )

  expect_true(project$modified)
})

test_that("project$addScenario() delegates to standalone addScenario()", {
  project <- testProject()

  project$addScenario(
    scenarioName = "MethodScenario",
    modelFile = "Aciclovir.pkml",
    individualId = "Indiv1"
  )

  expect_true("MethodScenario" %in% names(project$scenarios))
  expect_s3_class(project$scenarios[["MethodScenario"]], "Scenario")
})

test_that("addScenario populates all optional fields correctly", {
  project <- testProject()
  addScenario(
    project,
    scenarioName = "FullScenario",
    modelFile = "Aciclovir.pkml",
    individualId = "Indiv1",
    populationId = "TestPopulation",
    applicationProtocol = "Aciclovir_iv_250mg",
    modelParameters = "Global",
    outputPathIds = names(project$outputPaths)[1],
    simulationTime = "0, 50, 100",
    simulationTimeUnit = "min",
    steadyState = TRUE,
    steadyStateTime = 500,
    overwriteFormulasInSS = TRUE,
    readPopulationFromCSV = TRUE
  )

  newScenario <- project$scenarios[["FullScenario"]]
  expect_equal(newScenario$applicationProtocol, "Aciclovir_iv_250mg")
  expect_equal(newScenario$modelParameters, "Global")
  expect_equal(newScenario$simulationTimeUnit, "min")
  expect_true(newScenario$simulateSteadyState)
  expect_equal(newScenario$steadyStateTime, 500)
  expect_true(newScenario$overwriteFormulasInSS)
  expect_true(newScenario$readPopulationFromCSV)
})

test_that("addScenario returns project invisibly", {
  project <- testProject()

  result <- addScenario(
    project,
    scenarioName = "InvisibleScenario",
    modelFile = "Aciclovir.pkml",
    individualId = "Indiv1"
  )

  expect_identical(result, project)
})

test_that("removeScenario removes scenario", {
  project <- testProject()
  addScenario(
    project,
    "TempScenario",
    "Aciclovir.pkml",
    individualId = "Indiv1"
  )
  project$.markSaved()
  removeScenario(project, "TempScenario")
  expect_false("TempScenario" %in% names(project$scenarios))
  expect_true(project$modified)
})

test_that("removeScenario warns on missing", {
  project <- testProject()
  expect_warning(
    removeScenario(project, "NoSuchScenario_ZZ"),
    regexp = "not found"
  )
})
