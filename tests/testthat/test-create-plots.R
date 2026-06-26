test_that(".parseExcelMultiValueField numeric conversion path is covered", {
  result <- esqlabsR:::.parseExcelMultiValueField(
    value = "72.5, 80.5",
    fieldName = "test",
    plotID = "P1",
    expectedLength = 2,
    expectedType = "numeric"
  )
  expect_equal(result, c(72.5, 80.5))
  expect_true(is.numeric(result))

  expect_error(
    esqlabsR:::.parseExcelMultiValueField(
      value = "72 80",
      fieldName = "test",
      plotID = "P1",
      expectedLength = 2,
      expectedType = "numeric"
    ),
    regexp = "Invalid format.*Expected.*Values separated by commas",
    fixed = FALSE
  )
})

test_that(".validateClassHasField is NA-safe when the object has NA names", {
  object <- list(1, 2)
  names(object) <- c(NA, "b")

  expect_false(esqlabsR:::.validateClassHasField(object, "x"))
  expect_true(esqlabsR:::.validateClassHasField(object, "b"))
})

# createPlots(project, ...) tests ----

test_that("createPlots errors on non-Project input", {
  expect_error(createPlots("not a project"), "expected <Project>")
})

test_that("createPlots returns empty list when project has no plots", {
  project <- testProject()
  expect_identical(createPlots(project), list())
})

test_that("createPlots builds plot grids for Example project", {
  project <- exampleProject()
  simulated <- runScenarios(project, scenarioNames = "Aciclovir_iv")
  gridName <- project$plots$plotGrids$name[[1]]

  result <- createPlots(
    project,
    plotGridNames = gridName,
    simulatedScenarios = simulated
  )

  expect_named(result, gridName)
  expect_s3_class(result[[gridName]], "patchwork")
})

test_that("createPlots succeeds when a plot belongs to no grid", {
  project <- exampleProject()
  path <- project$outputPaths$Aciclovir_PVB
  addDataCombined(
    project,
    "DC_outside",
    simulated = list(list(
      label = "sim",
      scenario = "Aciclovir_iv",
      path = path,
      group = "g"
    ))
  )
  # Plot referencing DC_outside is in no grid; createPlots(project) builds
  # all grids and must not abort just because DC_outside is unbuilt.
  addPlot(project, "P_outside", "DC_outside", "individual")
  simulated <- runScenarios(project, scenarioNames = "Aciclovir_iv")

  result <- suppressWarnings(createPlots(
    project,
    simulatedScenarios = simulated,
    validate = FALSE
  ))

  expect_named(result, "Individual_diagnostics")
  expect_s3_class(result[["Individual_diagnostics"]], "patchwork")
})

test_that("createPlots grid subset ignores plots in other grids", {
  project <- exampleProject()
  path <- project$outputPaths$Aciclovir_PVB
  addDataCombined(
    project,
    "DC_other",
    simulated = list(list(
      label = "sim",
      scenario = "Aciclovir_iv_population",
      path = path,
      group = "g"
    ))
  )
  addPlot(project, "P_other", "DC_other", "population")
  addPlotGrid(project, "Grid_other", plotIds = "P_other")
  simulated <- runScenarios(project, scenarioNames = "Aciclovir_iv")

  # Requesting only the original grid must not abort over Grid_other's DC,
  # which references a scenario absent from `simulated`.
  result <- suppressWarnings(createPlots(
    project,
    plotGridNames = "Individual_diagnostics",
    simulatedScenarios = simulated,
    validate = FALSE
  ))

  expect_named(result, "Individual_diagnostics")
})

test_that("createPlots aborts on unknown plotGridNames when stopIfNotFound", {
  project <- exampleProject()
  simulated <- runScenarios(project, scenarioNames = "Aciclovir_iv")

  expect_snapshot(
    error = TRUE,
    createPlots(
      project,
      plotGridNames = "DoesNotExist",
      simulatedScenarios = simulated,
      validate = FALSE,
      stopIfNotFound = TRUE
    )
  )
})

test_that("createPlots silently drops unknown plotGridNames when not stopIfNotFound", {
  project <- exampleProject()
  simulated <- runScenarios(project, scenarioNames = "Aciclovir_iv")

  result <- suppressWarnings(createPlots(
    project,
    plotGridNames = "DoesNotExist",
    simulatedScenarios = simulated,
    validate = FALSE,
    stopIfNotFound = FALSE
  ))
  expect_identical(result, list())
})

test_that("createPlots builds every plot type end to end", {
  project <- testProject()
  path <- project$outputPaths$Aciclovir_PVB
  dataSet <- names(loadObservedData(project))

  addDataCombined(
    project,
    "DC_ind",
    simulated = list(list(
      label = "sim",
      scenario = "TestScenario",
      path = path,
      group = "g"
    )),
    observed = list(list(label = "obs", dataSet = dataSet, group = "g"))
  )
  addDataCombined(
    project,
    "DC_pop",
    simulated = list(list(
      label = "sim",
      scenario = "PopulationScenario",
      path = path,
      group = "g"
    ))
  )
  addPlot(project, "P_ind", "DC_ind", "individual")
  addPlot(project, "P_pop", "DC_pop", "population")
  addPlot(project, "P_ovs", "DC_ind", "observedVsSimulated")
  addPlot(project, "P_rvs", "DC_ind", "residualsVsSimulated")
  addPlot(project, "P_rvt", "DC_ind", "residualsVsTime")
  addPlotGrid(
    project,
    "Grid_all",
    plotIds = c("P_ind", "P_pop", "P_ovs", "P_rvs", "P_rvt")
  )
  simulated <- runScenarios(
    project,
    scenarioNames = c("TestScenario", "PopulationScenario")
  )

  result <- suppressWarnings(createPlots(
    project,
    plotGridNames = "Grid_all",
    simulatedScenarios = simulated,
    validate = FALSE
  ))

  expect_named(result, "Grid_all")
  expect_s3_class(result[["Grid_all"]], "patchwork")
})

test_that("createPlots survives a save/load round trip that drops the title column", {
  dir <- withr::local_tempdir()
  initProject(
    destination = dir,
    type = "example",
    createExcel = FALSE,
    overwrite = TRUE
  )
  project <- loadProject(file.path(dir, "Project.json"))
  path <- project$outputPaths$Aciclovir_PVB
  addDataCombined(
    project,
    "DC_nt",
    simulated = list(list(
      label = "sim",
      scenario = "Aciclovir_iv",
      path = path,
      group = "g"
    ))
  )
  addPlot(project, "P_nt", "DC_nt", "individual")
  addPlotGrid(project, "Grid_nt", plotIds = "P_nt")
  # Drop the original titled plot/grid so no plot or grid sets a title; the
  # title column is then dropped on save and absent on reload.
  removePlotGrid(project, "Individual_diagnostics")
  removePlot(project, "P1")
  saveProject(project)

  reloaded <- loadProject(file.path(dir, "Project.json"))
  expect_false("title" %in% names(reloaded$plots$plotConfiguration))
  simulated <- runScenarios(reloaded, scenarioNames = "Aciclovir_iv")

  result <- suppressWarnings(createPlots(
    reloaded,
    simulatedScenarios = simulated,
    validate = FALSE
  ))
  expect_named(result, "Grid_nt")
})

test_that("createPlots observedVsSimulated survives a dropped foldDistance column", {
  dir <- withr::local_tempdir()
  initProject(
    destination = dir,
    type = "example",
    createExcel = FALSE,
    overwrite = TRUE
  )
  project <- loadProject(file.path(dir, "Project.json"))
  path <- project$outputPaths$Aciclovir_PVB
  dataSet <- names(loadObservedData(project))
  addDataCombined(
    project,
    "DC_ovs",
    simulated = list(list(
      label = "sim",
      scenario = "Aciclovir_iv",
      path = path,
      group = "g"
    )),
    observed = list(list(label = "obs", dataSet = dataSet, group = "g"))
  )
  # Title kept, foldDistance left unset so its column is dropped on save.
  addPlot(project, "P_ovs", "DC_ovs", "observedVsSimulated", title = "OvS")
  addPlotGrid(project, "Grid_ovs", plotIds = "P_ovs", title = "Grid OvS")
  removePlotGrid(project, "Individual_diagnostics")
  removePlot(project, "P1")
  saveProject(project)

  reloaded <- loadProject(file.path(dir, "Project.json"))
  expect_false("foldDistance" %in% names(reloaded$plots$plotConfiguration))
  simulated <- runScenarios(reloaded, scenarioNames = "Aciclovir_iv")

  result <- suppressWarnings(createPlots(
    reloaded,
    simulatedScenarios = simulated,
    validate = FALSE
  ))
  expect_named(result, "Grid_ovs")
})
