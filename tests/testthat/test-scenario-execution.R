# Local test helper. Once Task 8 ships loadProject(), the production
# helpers.R::testProject() will work directly and this can be removed.
.testProject <- function() {
  esqlabsR:::.loadProjectJson(testProjectJSONPath())
}

test_that(".parameterGroupToStructure flattens record-shape into paths/values/units", {
  records <- list(
    list(containerPath = "A|B", parameterName = "P1", value = 1.5, units = "mg"),
    list(containerPath = "A|C", parameterName = "P2", value = 2,   units = NULL)
  )
  out <- esqlabsR:::.parameterGroupToStructure(records)
  expect_equal(out$paths,  c("A|B|P1", "A|C|P2"))
  expect_equal(out$values, c(1.5, 2))
  expect_equal(out$units,  c("mg", ""))
})

test_that(".parameterGroupToStructure returns NULL on empty input", {
  expect_null(esqlabsR:::.parameterGroupToStructure(NULL))
  expect_null(esqlabsR:::.parameterGroupToStructure(list()))
})

test_that(".findById returns the matching item by idField", {
  items <- list(
    list(populationId = "A", x = 1),
    list(populationId = "B", x = 2)
  )
  expect_equal(
    esqlabsR:::.findById(items, "populationId", "B")$x,
    2
  )
  expect_null(esqlabsR:::.findById(items, "populationId", "missing"))
  expect_null(esqlabsR:::.findById(NULL, "populationId", "A"))
})

test_that(".mergeScenarioParameters returns NULL when no layer contributes", {
  project <- .testProject()
  scenario <- project$scenarios[["TestScenario"]]
  scenario$modelParameters <- NULL
  scenario$individualId <- NULL
  scenario$applicationProtocol <- NA
  expect_null(
    esqlabsR:::.mergeScenarioParameters(scenario, project, customParams = NULL)
  )
})

test_that(".mergeScenarioParameters layer 1 (modelParameters) iterates listed groups in order", {
  project <- .testProject()
  scenario <- project$scenarios[["TestScenario_steadystate"]]
  scenario$individualId <- NULL
  scenario$applicationProtocol <- NA
  merged <- esqlabsR:::.mergeScenarioParameters(scenario, project, NULL)
  expect_true("Organism|Liver|EHC continuous fraction" %in% merged$paths)
  expect_true("Aciclovir|Lipophilicity" %in% merged$paths)
})

test_that(".mergeScenarioParameters layer 4 (application) overrides layer 1 on overlapping path", {
  project <- .testProject()
  scenario <- project$scenarios[["TestScenario"]]
  proj <- project$.__enclos_env__$private
  proj$.modelParameters$Global <- list(
    list(
      containerPath = "OverlapContainer",
      parameterName = "OverlapParam",
      value = 1,
      units = NULL
    )
  )
  proj$.applications$Aciclovir_iv_250mg$parameters <- list(
    list(
      containerPath = "OverlapContainer",
      parameterName = "OverlapParam",
      value = 99,
      units = NULL
    )
  )
  scenario$individualId <- NULL
  merged <- esqlabsR:::.mergeScenarioParameters(scenario, project, NULL)
  idx <- match("OverlapContainer|OverlapParam", merged$paths)
  expect_equal(merged$values[idx], 99)
})

test_that(".mergeScenarioParameters layer 5 (customParams) wins over all earlier layers", {
  project <- .testProject()
  scenario <- project$scenarios[["TestScenario"]]
  customParams <- list(
    paths  = "Organism|Liver|EHC continuous fraction",
    values = 42,
    units  = ""
  )
  merged <- esqlabsR:::.mergeScenarioParameters(scenario, project, customParams)
  idx <- match("Organism|Liver|EHC continuous fraction", merged$paths)
  expect_equal(merged$values[idx], 42)
})

test_that(".mergeScenarioParameters skips application layer when applicationProtocol is NA", {
  project <- .testProject()
  scenario <- project$scenarios[["TestScenario"]]
  scenario$applicationProtocol <- NA
  scenario$modelParameters <- NULL
  scenario$individualId <- NULL
  expect_null(
    esqlabsR:::.mergeScenarioParameters(scenario, project, NULL)
  )
})

test_that(".mergeScenarioParameters errors when applicationProtocol is set but unknown", {
  project <- .testProject()
  scenario <- project$scenarios[["TestScenario"]]
  scenario$applicationProtocol <- "DoesNotExist"
  expect_error(
    esqlabsR:::.mergeScenarioParameters(scenario, project, NULL),
    regexp = "DoesNotExist"
  )
})

test_that(".mergeScenarioParameters silently skips an unknown modelParameters group", {
  project <- .testProject()
  scenario <- project$scenarios[["TestScenario"]]
  scenario$modelParameters <- c("Global", "DoesNotExist")
  scenario$individualId <- NULL
  scenario$applicationProtocol <- NA
  merged <- esqlabsR:::.mergeScenarioParameters(scenario, project, NULL)
  expect_true("Organism|Liver|EHC continuous fraction" %in% merged$paths)
})

test_that(".runScenariosFromProject returns the documented per-scenario list shape (individual)", {
  withr::local_options(lifecycle_verbosity = "quiet")
  project <- .testProject()
  out <- esqlabsR:::.runScenariosFromProject(project, scenarioNames = "TestScenario")
  expect_named(out, "TestScenario")
  expect_named(
    out$TestScenario,
    c("simulation", "results", "outputValues", "population")
  )
  expect_s3_class(out$TestScenario$simulation, "Simulation")
  expect_null(out$TestScenario$population)
})

test_that(".runScenariosFromProject runs a steady-state scenario without error", {
  withr::local_options(lifecycle_verbosity = "quiet")
  project <- .testProject()
  out <- esqlabsR:::.runScenariosFromProject(project, scenarioNames = "TestScenario_steadystate")
  expect_s3_class(out$TestScenario_steadystate$simulation, "Simulation")
})

test_that(".runScenariosFromProject runs a population scenario and attaches a Population", {
  withr::local_options(lifecycle_verbosity = "quiet")
  project <- .testProject()
  out <- esqlabsR:::.runScenariosFromProject(project, scenarioNames = "PopulationScenario")
  expect_s3_class(out$PopulationScenario$population, "Population")
})

test_that(".runScenariosFromProject runs a CSV-population scenario and attaches a Population", {
  withr::local_options(lifecycle_verbosity = "quiet")
  project <- .testProject()
  out <- esqlabsR:::.runScenariosFromProject(project, scenarioNames = "PopulationScenarioFromCSV")
  expect_s3_class(out$PopulationScenarioFromCSV$population, "Population")
})

test_that(".runScenariosFromProject errors on unknown scenarioNames", {
  withr::local_options(lifecycle_verbosity = "quiet")
  project <- .testProject()
  expect_error(
    esqlabsR:::.runScenariosFromProject(project, scenarioNames = "NopeNope"),
    regexp = "NopeNope"
  )
})
