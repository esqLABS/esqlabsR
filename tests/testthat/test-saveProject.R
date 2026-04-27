# saveProject() ----

test_that("saveProject writes valid JSON that can be reloaded", {
  pc <- testProject()
  tmp <- tempfile(fileext = ".json")
  withr::defer(unlink(tmp))

  saveProject(pc, tmp)

  expect_true(file.exists(tmp))
  pc2 <- loadProject(tmp)
  expect_s3_class(pc2, "Project")
})

test_that("saveProject defaults to project$jsonPath when path is NULL", {
  tmpDir <- withr::local_tempdir()
  jsonPath <- file.path(tmpDir, "Project.json")
  file.copy(testProjectJSONPath(), jsonPath)

  pc <- loadProject(jsonPath)
  pc$scenarios[["NewScenario"]] <- Scenario$new()
  pc$scenarios[["NewScenario"]]$scenarioName <- "NewScenario"
  pc$scenarios[["NewScenario"]]$modelFile <- "Test.pkml"

  saveProject(pc)

  pc2 <- loadProject(jsonPath)
  expect_true("NewScenario" %in% names(pc2$scenarios))
})

test_that("saveProject errors when path is NULL and project has no jsonPath", {

  pc <- Project$new()
  expect_error(saveProject(pc), "path")
})

test_that("saveProject produces round-trip fidelity for scenarios", {
  pc <- testProject()
  tmp <- tempfile(fileext = ".json")
  withr::defer(unlink(tmp))

  saveProject(pc, tmp)
  pc2 <- loadProject(tmp)

  expect_equal(names(pc$scenarios), names(pc2$scenarios))

  sc1 <- pc$scenarios[["TestScenario2"]]
  sc2 <- pc2$scenarios[["TestScenario2"]]
  expect_equal(sc1$scenarioName, sc2$scenarioName)
  expect_equal(sc1$modelFile, sc2$modelFile)
  expect_equal(sc1$individualId, sc2$individualId)
  expect_equal(sc1$applicationProtocol, sc2$applicationProtocol)
  expect_equal(sc1$simulateSteadyState, sc2$simulateSteadyState)
  expect_equal(sc1$modelParameters, sc2$modelParameters)
})

test_that("saveProject produces round-trip fidelity for modelParameters", {
  pc <- testProject()
  tmp <- tempfile(fileext = ".json")
  withr::defer(unlink(tmp))

  saveProject(pc, tmp)
  pc2 <- loadProject(tmp)

  expect_equal(names(pc$modelParameters), names(pc2$modelParameters))
  expect_equal(pc$modelParameters[["Global"]], pc2$modelParameters[["Global"]])
})

test_that("saveProject produces round-trip fidelity for individuals", {
  pc <- testProject()
  tmp <- tempfile(fileext = ".json")
  withr::defer(unlink(tmp))

  saveProject(pc, tmp)
  pc2 <- loadProject(tmp)

  expect_equal(names(pc$individuals), names(pc2$individuals))
  indiv1 <- pc$individuals[["Indiv1"]]
  indiv2 <- pc2$individuals[["Indiv1"]]
  expect_equal(indiv1$species, indiv2$species)
  expect_equal(indiv1$weight, indiv2$weight)
})

test_that("saveProject produces round-trip fidelity for populations", {
  pc <- testProject()
  tmp <- tempfile(fileext = ".json")
  withr::defer(unlink(tmp))

  saveProject(pc, tmp)
  pc2 <- loadProject(tmp)

  expect_equal(names(pc$populations), names(pc2$populations))
  pop1 <- pc$populations[["TestPopulation"]]
  pop2 <- pc2$populations[["TestPopulation"]]
  expect_equal(pop1$species, pop2$species)
  expect_equal(pop1$numberOfIndividuals, pop2$numberOfIndividuals)
})

test_that("saveProject produces round-trip fidelity for applications", {
  pc <- testProject()
  tmp <- tempfile(fileext = ".json")
  withr::defer(unlink(tmp))

  saveProject(pc, tmp)
  pc2 <- loadProject(tmp)

  expect_equal(names(pc$applications), names(pc2$applications))
  expect_equal(
    pc$applications[["Aciclovir_iv_250mg"]],
    pc2$applications[["Aciclovir_iv_250mg"]]
  )
})

test_that("saveProject produces round-trip fidelity for outputPaths", {
  pc <- testProject()
  tmp <- tempfile(fileext = ".json")
  withr::defer(unlink(tmp))

  saveProject(pc, tmp)
  pc2 <- loadProject(tmp)

  expect_equal(pc$outputPaths, pc2$outputPaths)
})

test_that("saveProject produces round-trip fidelity for plots", {
  pc <- testProject()
  tmp <- tempfile(fileext = ".json")
  withr::defer(unlink(tmp))

  saveProject(pc, tmp)
  pc2 <- loadProject(tmp)

  expect_equal(nrow(pc$plots$dataCombined), nrow(pc2$plots$dataCombined))
  expect_equal(nrow(pc$plots$plotConfiguration), nrow(pc2$plots$plotConfiguration))
  expect_equal(nrow(pc$plots$plotGrids), nrow(pc2$plots$plotGrids))
})

test_that("saveProject resets modified flag to FALSE", {
  pc <- testProject()
  pc$modified <- TRUE
  tmp <- tempfile(fileext = ".json")
  withr::defer(unlink(tmp))

  saveProject(pc, tmp)

  expect_false(pc$modified)
})

test_that("saveProject persists scenario added via addScenario", {
  tmpDir <- withr::local_tempdir()
  jsonPath <- file.path(tmpDir, "Project.json")
  file.copy(testProjectJSONPath(), jsonPath)

  pc <- loadProject(jsonPath)
  addScenario(
    project = pc,
    scenarioName = "ProgrammaticScenario",
    modelFile = "Aciclovir.pkml",
    individualId = "Indiv1",
    applicationProtocol = "Aciclovir_iv_250mg"
  )

  saveProject(pc, jsonPath)

  pc2 <- loadProject(jsonPath)
  expect_true("ProgrammaticScenario" %in% names(pc2$scenarios))
  expect_equal(
    pc2$scenarios[["ProgrammaticScenario"]]$individualId,
    "Indiv1"
  )
})

test_that("saveProject updates esqlabsRVersion to current package version", {
  pc <- testProject()
  tmp <- tempfile(fileext = ".json")
  withr::defer(unlink(tmp))

  saveProject(pc, tmp)

  jsonData <- jsonlite::fromJSON(tmp, simplifyVector = FALSE)
  expect_equal(
    jsonData$esqlabsRVersion,
    as.character(utils::packageVersion("esqlabsR"))
  )
})
