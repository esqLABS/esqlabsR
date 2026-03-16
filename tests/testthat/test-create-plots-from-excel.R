# Set up simulated scenarios and observed data shared with test-utilities-data-combined.R
testSetup <- setupTestSimulatedScenarios()
projectConfiguration <- testSetup$projectConfiguration
scenarioNames <- testSetup$scenarioNames
outputPaths <- testSetup$outputPaths
simulatedScenarios <- testSetup$simulatedScenarios
observedData <- testSetup$observedData
rm(testSetup)

# Load pre-simulated results for the "TestScenario". Required to ensure
# identify of the results, otherwise numerical noise interferes with snapshot testing
preSimulatedResults <- ospsuite::importResultsFromCSV(
  simulation = simulatedScenarios$TestScenario$simulation,
  filePaths = getTestDataFilePath("TestScenario_results.csv")
)
simulatedScenarios$TestScenario$results <- preSimulatedResults
simulatedScenarios$TestScenario$outputValues <- getOutputValues(
  simulationResults = preSimulatedResults
)

dataCombinedDf <- data.frame(list(
  "DataCombinedName" = c("AciclovirPVB", "AciclovirPVB"),
  "dataType" = c("simulated", "observed"),
  "label" = c("Aciclovir simulated", "Aciclovir observed"),
  "scenario" = c(scenarioNames[1], NA),
  "path" = c(outputPaths, NA),
  "dataSet" = c(NA, names(observedData)),
  "group" = c("Aciclovir PVB", "Aciclovir PVB"),
  "xOffsets" = c(NA, NA),
  "xOffsetsUnits" = c(NA, NA),
  "yOffsets" = c(NA, NA),
  "yOffsetsUnits" = c(NA, NA),
  "xScaleFactors" = c(NA, NA),
  "yScaleFactors" = c(NA, NA)
))
plotConfigurationDf <- data.frame(list(
  "plotID" = "P1",
  "DataCombinedName" = "AciclovirPVB",
  "plotType" = "individual",
  "title" = NA,
  "xUnit" = NA,
  "yUnit" = NA,
  "xAxisScale" = NA,
  "yAxisScale" = NA,
  "xValuesLimits" = NA,
  "yValuesLimits" = NA,
  "quantiles" = NA,
  "nsd" = NA,
  "foldDistance" = NA
))
plotGridsDf <- data.frame(list(
  "name" = "Aciclovir",
  "plotIDs" = "P1",
  "title" = "Aciclovir PVB"
))

exportConfigurationDf <- data.frame(list(
  "plotGridName" = character(0),
  "outputName" = character(0)
))

# Helper function to get the most recently created/modified directory
.getLatestDirectory <- function(baseDir) {
  allDirs <- list.dirs(baseDir, full.names = TRUE, recursive = FALSE)
  allDirs[which.max(file.info(allDirs)$mtime)]
}

# Validation DataCombined
test_that("It trows an error if mandatory field dataType is not filled out", {
  localDataCombinedDf <- dataCombinedDf
  localDataCombinedDf$dataType <- NA
  setup <- local_plots_test(
    projectConfiguration,
    dataCombinedDf = localDataCombinedDf,
    plotConfigurationDf = plotConfigurationDf,
    plotGridsDf = plotGridsDf,
    exportConfigurationDf = exportConfigurationDf
  )
  expect_error(
    createPlotsFromExcel(
      simulatedScenarios = simulatedScenarios,
      observedData = observedData,
      projectConfiguration = setup$projectConfiguration,
      stopIfNotFound = TRUE
    ),
    regexp = messages$missingDataType()
  )
})

test_that("It trows an error if mandatory field label is not filled out", {
  localDataCombinedDf <- dataCombinedDf
  localDataCombinedDf$label <- NA
  setup <- local_plots_test(
    projectConfiguration,
    dataCombinedDf = localDataCombinedDf,
    plotConfigurationDf = plotConfigurationDf,
    plotGridsDf = plotGridsDf,
    exportConfigurationDf = exportConfigurationDf
  )
  expect_error(
    createPlotsFromExcel(
      simulatedScenarios = simulatedScenarios,
      observedData = observedData,
      projectConfiguration = setup$projectConfiguration,
      stopIfNotFound = TRUE
    ),
    regexp = messages$missingLabel()
  )
})

test_that("It trows an error if no scenario is specified for a simulated data", {
  localDataCombinedDf <- dataCombinedDf
  localDataCombinedDf$scenario <- NA
  setup <- local_plots_test(
    projectConfiguration,
    dataCombinedDf = localDataCombinedDf,
    plotConfigurationDf = plotConfigurationDf,
    plotGridsDf = plotGridsDf,
    exportConfigurationDf = exportConfigurationDf
  )
  expect_error(
    createPlotsFromExcel(
      simulatedScenarios = simulatedScenarios,
      observedData = observedData,
      projectConfiguration = setup$projectConfiguration,
      stopIfNotFound = TRUE
    ),
    regexp = messages$missingScenarioName()
  )
})

test_that("It trows an error if no output path is specified for a simulated data", {
  localDataCombinedDf <- dataCombinedDf
  localDataCombinedDf$path <- NA
  setup <- local_plots_test(
    projectConfiguration,
    dataCombinedDf = localDataCombinedDf,
    plotConfigurationDf = plotConfigurationDf,
    plotGridsDf = plotGridsDf,
    exportConfigurationDf = exportConfigurationDf
  )
  expect_error(
    createPlotsFromExcel(
      simulatedScenarios = simulatedScenarios,
      observedData = observedData,
      projectConfiguration = setup$projectConfiguration,
      stopIfNotFound = TRUE
    ),
    regexp = messages$stopNoPathProvided("AciclovirPVB")
  )
})

test_that("It trows an error if wrong output path is specified for a simulated data", {
  localDataCombinedDf <- dataCombinedDf
  localDataCombinedDf$path <- "foo"
  setup <- local_plots_test(
    projectConfiguration,
    dataCombinedDf = localDataCombinedDf,
    plotConfigurationDf = plotConfigurationDf,
    plotGridsDf = plotGridsDf,
    exportConfigurationDf = exportConfigurationDf
  )
  expect_error(
    createPlotsFromExcel(
      simulatedScenarios = simulatedScenarios,
      observedData = observedData,
      projectConfiguration = setup$projectConfiguration,
      stopIfNotFound = TRUE
    ),
    regexp = messages$stopWrongOutputPath(
      dataCombinedName = localDataCombinedDf$DataCombinedName[[1]],
      scenarioName = localDataCombinedDf$scenario[[1]],
      path = localDataCombinedDf$path[[1]]
    )
  )
})

test_that("It trows an error if no data set is specified for observed data", {
  localDataCombinedDf <- dataCombinedDf
  localDataCombinedDf$dataSet <- NA
  setup <- local_plots_test(
    projectConfiguration,
    dataCombinedDf = localDataCombinedDf,
    plotConfigurationDf = plotConfigurationDf,
    plotGridsDf = plotGridsDf,
    exportConfigurationDf = exportConfigurationDf
  )
  expect_error(
    createPlotsFromExcel(
      simulatedScenarios = simulatedScenarios,
      observedData = observedData,
      projectConfiguration = setup$projectConfiguration,
      stopIfNotFound = TRUE
    ),
    regexp = messages$stopNoDataSetProvided("AciclovirPVB")
  )
})

test_that("It trows an error if defined scenario is missing and stopIfNotFound is TRUE", {
  localDataCombinedDf <- dataCombinedDf
  localDataCombinedDf$scenario <- c("TestScenario", "foo")
  setup <- local_plots_test(
    projectConfiguration,
    dataCombinedDf = localDataCombinedDf,
    plotConfigurationDf = plotConfigurationDf,
    plotGridsDf = plotGridsDf,
    exportConfigurationDf = exportConfigurationDf
  )
  expect_error(
    createPlotsFromExcel(
      simulatedScenarios = simulatedScenarios,
      observedData = observedData,
      projectConfiguration = setup$projectConfiguration,
      stopIfNotFound = TRUE
    ),
    regexp = messages$warningInvalidScenarioName("foo")
  )
})

test_that("It shows a warning for missing scenarios if stopIfNotFound is FALSE", {
  localDataCombinedDf <- dataCombinedDf
  localDataCombinedDf$scenario <- c(scenarioNames[1], "foo")
  setup <- local_plots_test(
    projectConfiguration,
    dataCombinedDf = localDataCombinedDf,
    plotConfigurationDf = plotConfigurationDf,
    plotGridsDf = plotGridsDf,
    exportConfigurationDf = exportConfigurationDf
  )
  expect_warning(
    createPlotsFromExcel(
      simulatedScenarios = simulatedScenarios,
      observedData = observedData,
      projectConfiguration = setup$projectConfiguration,
      stopIfNotFound = FALSE
    ),
    regexp = messages$warningInvalidScenarioName("foo")
  )
})

test_that("It trows an error if defined data set is missing and stopIfNotFound is TRUE", {
  localDataCombinedDf <- dataCombinedDf
  localDataCombinedDf$dataSet <- c(scenarioNames[1], names(observedData))
  setup <- local_plots_test(
    projectConfiguration,
    dataCombinedDf = localDataCombinedDf,
    plotConfigurationDf = plotConfigurationDf,
    plotGridsDf = plotGridsDf,
    exportConfigurationDf = exportConfigurationDf
  )
  expect_error(
    createPlotsFromExcel(
      simulatedScenarios = simulatedScenarios,
      observedData = observedData,
      projectConfiguration = setup$projectConfiguration,
      stopIfNotFound = TRUE
    ),
    regexp = messages$stopInvalidDataSetName(scenarioNames[1])
  )
})

test_that("It shows a warning for missing data set if stopIfNotFound is FALSE", {
  localDataCombinedDf <- dataCombinedDf
  localDataCombinedDf$dataSet <- c(scenarioNames[1], names(observedData))
  setup <- local_plots_test(
    projectConfiguration,
    dataCombinedDf = localDataCombinedDf,
    plotConfigurationDf = plotConfigurationDf,
    plotGridsDf = plotGridsDf,
    exportConfigurationDf = exportConfigurationDf
  )
  expect_warning(
    createPlotsFromExcel(
      simulatedScenarios = simulatedScenarios,
      observedData = observedData,
      projectConfiguration = setup$projectConfiguration,
      stopIfNotFound = FALSE
    ),
    regexp = messages$warningInvalidDataSetName(scenarioNames[1])
  )
})

test_that("It trows an error if mandatory field DataCombinedName is not filled out", {
  localPlotConfigDf <- plotConfigurationDf
  localPlotConfigDf$DataCombinedName <- NA
  setup <- local_plots_test(
    projectConfiguration,
    dataCombinedDf = dataCombinedDf,
    plotConfigurationDf = localPlotConfigDf,
    plotGridsDf = plotGridsDf,
    exportConfigurationDf = exportConfigurationDf
  )
  expect_error(
    createPlotsFromExcel(
      simulatedScenarios = simulatedScenarios,
      observedData = observedData,
      projectConfiguration = setup$projectConfiguration,
      stopIfNotFound = TRUE
    ),
    regexp = messages$missingDataCombinedName()
  )
})

test_that("It trows an error if mandatory field plotType is not filled out", {
  localPlotConfigDf <- plotConfigurationDf
  localPlotConfigDf$plotType <- NA
  setup <- local_plots_test(
    projectConfiguration,
    dataCombinedDf = dataCombinedDf,
    plotConfigurationDf = localPlotConfigDf,
    plotGridsDf = plotGridsDf,
    exportConfigurationDf = exportConfigurationDf
  )
  expect_error(
    createPlotsFromExcel(
      simulatedScenarios = simulatedScenarios,
      observedData = observedData,
      projectConfiguration = setup$projectConfiguration,
      stopIfNotFound = TRUE
    ),
    regexp = messages$missingPlotType()
  )
})

test_that("It trows an error if a plot requires a DataCombined that is not defined", {
  localPlotConfigDf <- plotConfigurationDf
  localPlotConfigDf$DataCombinedName <- "foo"
  setup <- local_plots_test(
    projectConfiguration,
    dataCombinedDf = dataCombinedDf,
    plotConfigurationDf = localPlotConfigDf,
    plotGridsDf = plotGridsDf,
    exportConfigurationDf = exportConfigurationDf
  )
  expect_error(
    createPlotsFromExcel(
      simulatedScenarios = simulatedScenarios,
      observedData = observedData,
      projectConfiguration = setup$projectConfiguration,
      stopIfNotFound = TRUE
    ),
    regexp = messages$stopInvalidDataCombinedName("foo")
  )
})

# Validation plotGrids
test_that("It returns NULL if no plotGrids are defined in the excel sheet", {
  localPlotGridsDf <- data.frame(list(
    "name" = NA,
    "plotIDs" = NA,
    "title" = NA
  ))
  setup <- local_plots_test(
    projectConfiguration,
    dataCombinedDf = dataCombinedDf,
    plotConfigurationDf = plotConfigurationDf,
    plotGridsDf = localPlotGridsDf,
    exportConfigurationDf = exportConfigurationDf
  )
  plots <- createPlotsFromExcel(
    simulatedScenarios = simulatedScenarios,
    observedData = observedData,
    projectConfiguration = setup$projectConfiguration,
    stopIfNotFound = TRUE
  )
  expect_null(plots)
})

test_that("It creates plots for all plot grids when plotGridNames is NULL", {
  plots <- createPlotsFromExcel(
    simulatedScenarios = simulatedScenarios,
    observedData = observedData,
    projectConfiguration = projectConfiguration,
    stopIfNotFound = TRUE
  )
  expect_equal(names(plots), c("Aciclovir", "Aciclovir2", "Aciclovir3"))
})

test_that("When custom DataCombined is passed, it is used instead of the one defined in the Excel", {
  dataCombinedList <- createDataCombinedFromExcel(
    projectConfiguration = projectConfiguration,
    plotGridNames = c("Aciclovir", "Aciclovir2", "Aciclovir3"),
    simulatedScenarios = simulatedScenarios,
    observedData = observedData
  )

  # Change the x-offset of a data combined
  dataCombinedList$AciclovirPVB$setDataTransformations(xOffsets = 0)

  plots <- createPlotsFromExcel(
    simulatedScenarios = simulatedScenarios,
    observedData = observedData,
    projectConfiguration = projectConfiguration,
    dataCombinedList = dataCombinedList,
    stopIfNotFound = TRUE
  )
  vdiffr::expect_doppelganger(title = "firstPlot", plots[[1]])
  vdiffr::expect_doppelganger(title = "secondPlot", plots[[2]])
})

test_that("It can create plots when custom data combined are passed that are missing in the excel", {
  dataCombinedList <- createDataCombinedFromExcel(
    projectConfiguration = projectConfiguration,
    plotGridNames = c("Aciclovir", "Aciclovir2", "Aciclovir3"),
    simulatedScenarios = simulatedScenarios,
    observedData = observedData
  )

  emptyDataCombinedDf <- data.frame(list(
    "DataCombinedName" = NA,
    "dataType" = NA,
    "label" = NA,
    "scenario" = NA,
    "path" = NA,
    "dataSet" = NA,
    "group" = NA,
    "xOffsets" = NA,
    "xOffsetsUnits" = NA,
    "yOffsets" = NA,
    "yOffsetsUnits" = NA,
    "xScaleFactors" = NA,
    "yScaleFactors" = NA
  ))

  setup <- local_plots_test(
    projectConfiguration,
    dataCombinedDf = emptyDataCombinedDf,
    plotConfigurationDf = plotConfigurationDf,
    plotGridsDf = plotGridsDf,
    exportConfigurationDf = exportConfigurationDf
  )
  plots <- createPlotsFromExcel(
    simulatedScenarios = simulatedScenarios,
    observedData = observedData,
    projectConfiguration = setup$projectConfiguration,
    dataCombinedList = dataCombinedList,
    stopIfNotFound = TRUE
  )
  vdiffr::expect_doppelganger(title = "firstPlotCustomDC", plots[[1]])
})

test_that("It creates plots only for specified plotGrids", {
  plots <- createPlotsFromExcel(
    plotGridNames = "Aciclovir",
    simulatedScenarios = simulatedScenarios,
    observedData = observedData,
    projectConfiguration = projectConfiguration,
    stopIfNotFound = TRUE
  )
  expect_equal(names(plots), c("Aciclovir"))
})

test_that("It trows an error when specified plot grid names are not defined in the sheet", {
  expect_error(
    createPlotsFromExcel(
      plotGridNames = c("foo", "Aciclovir", "bar"),
      simulatedScenarios = simulatedScenarios,
      observedData = observedData,
      projectConfiguration = projectConfiguration,
      stopIfNotFound = TRUE
    ),
    messages$invalidPlotGridNames(c("foo", "bar"))
  )
})

test_that("It throws an error if mandatory field plotIDs is not filled out", {
  localPlotGridsDf <- plotGridsDf
  localPlotGridsDf$plotIDs <- NA
  setup <- local_plots_test(
    projectConfiguration,
    dataCombinedDf = dataCombinedDf,
    plotConfigurationDf = plotConfigurationDf,
    plotGridsDf = localPlotGridsDf,
    exportConfigurationDf = exportConfigurationDf
  )
  expect_error(
    createPlotsFromExcel(
      simulatedScenarios = simulatedScenarios,
      observedData = observedData,
      projectConfiguration = setup$projectConfiguration,
      stopIfNotFound = TRUE
    ),
    regexp = messages$missingPlotIDs()
  )
})


test_that("It throws an error if plotIDs are not unique", {
  localPlotConfigDf <- data.frame(list(
    "plotID" = c("P1", "P1"),
    "DataCombinedName" = c("AciclovirPVB", "AciclovirPVB"),
    "plotType" = c("individual", "individual"),
    "title" = NA,
    "xUnit" = NA,
    "yUnit" = NA,
    "xAxisScale" = NA,
    "yAxisScale" = NA,
    "xValuesLimits" = NA,
    "yValuesLimits" = NA,
    "quantiles" = NA,
    "nsd" = NA,
    "foldDistance" = NA
  ))
  setup <- local_plots_test(
    projectConfiguration,
    dataCombinedDf = dataCombinedDf,
    plotConfigurationDf = localPlotConfigDf,
    plotGridsDf = plotGridsDf,
    exportConfigurationDf = exportConfigurationDf
  )
  setup$projectConfiguration$outputFolder <- setup$tempDir
  expect_error(
    createPlotsFromExcel(
      simulatedScenarios = simulatedScenarios,
      observedData = observedData,
      projectConfiguration = setup$projectConfiguration,
      stopIfNotFound = TRUE
    ),
    regexp = messages$PlotIDsMustBeUnique(c("P1")),
    fixed = TRUE
  )
})

test_that("It throws an error if plotGrid names are not unique", {
  localPlotGridsDf <- data.frame(list(
    "name" = c("Aciclovir", "Aciclovir"),
    "plotIDs" = c("P1", "P2"),
    "title" = c("Aciclovir PVB", "Aciclovir PVB2")
  ))
  setup <- local_plots_test(
    projectConfiguration,
    dataCombinedDf = dataCombinedDf,
    plotConfigurationDf = plotConfigurationDf,
    plotGridsDf = localPlotGridsDf,
    exportConfigurationDf = exportConfigurationDf
  )
  setup$projectConfiguration$outputFolder <- setup$tempDir
  expect_error(
    createPlotsFromExcel(
      simulatedScenarios = simulatedScenarios,
      observedData = observedData,
      projectConfiguration = setup$projectConfiguration,
      stopIfNotFound = TRUE
    ),
    regexp = messages$PlotGridsNamesMustBeUnique(c("Aciclovir")),
    fixed = TRUE
  )
})

test_that("It trows an error if a plot grid requires a plot id that is not defined", {
  localPlotGridsDf <- plotGridsDf
  localPlotGridsDf$plotIDs <- "foo"
  setup <- local_plots_test(
    projectConfiguration,
    dataCombinedDf = dataCombinedDf,
    plotConfigurationDf = plotConfigurationDf,
    plotGridsDf = localPlotGridsDf,
    exportConfigurationDf = exportConfigurationDf
  )
  expect_error(
    createPlotsFromExcel(
      simulatedScenarios = simulatedScenarios,
      observedData = observedData,
      projectConfiguration = setup$projectConfiguration,
      stopIfNotFound = TRUE
    ),
    regexp = messages$errorInvalidPlotID("foo")
  )
})

test_that("It exports plot grids as defined in sheet `exportConfiguration`", {
  localExportConfigDf <- data.frame(
    plotGridName = rep("Aciclovir", 2),
    outputName = c("Aciclovir1", "Aciclovir2"),
    height = c(10, NA)
  )
  setup <- local_plots_test(
    projectConfiguration,
    dataCombinedDf = dataCombinedDf,
    plotConfigurationDf = plotConfigurationDf,
    plotGridsDf = plotGridsDf,
    exportConfigurationDf = localExportConfigDf
  )
  setup$projectConfiguration$outputFolder <- setup$tempDir
  createPlotsFromExcel(
    simulatedScenarios = simulatedScenarios,
    observedData = observedData,
    projectConfiguration = setup$projectConfiguration,
    stopIfNotFound = TRUE
  )
  # Get the most recently created/modified folder in the Figures directory
  latestDir <- .getLatestDirectory(file.path(setup$tempDir, "Figures"))
  expect_true(file.exists(file.path(latestDir, "Aciclovir1.png")))
  expect_true(file.exists(file.path(latestDir, "Aciclovir2.png")))
})

test_that("It exports plot grids with specified output folder", {
  localExportConfigDf <- data.frame(
    plotGridName = rep("Aciclovir", 2),
    outputName = c("Aciclovir1", "Aciclovir2"),
    height = c(10, NA)
  )
  setup <- local_plots_test(
    projectConfiguration,
    dataCombinedDf = dataCombinedDf,
    plotConfigurationDf = plotConfigurationDf,
    plotGridsDf = plotGridsDf,
    exportConfigurationDf = localExportConfigDf
  )
  setup$projectConfiguration$outputFolder <- setup$tempDir
  createPlotsFromExcel(
    simulatedScenarios = simulatedScenarios,
    observedData = observedData,
    projectConfiguration = setup$projectConfiguration,
    stopIfNotFound = TRUE,
    outputFolder = setup$tempDir
  )
  # Get the most recently created/modified folder in the Figures directory
  latestDir <- .getLatestDirectory(file.path(setup$tempDir, "Figures"))
  expect_true(file.exists(file.path(latestDir, "Aciclovir1.png")))
  expect_true(file.exists(file.path(latestDir, "Aciclovir2.png")))
})

test_that("It throws an error when trying to set a property that is not supported by the configuration", {
  localPlotConfigDf <- plotConfigurationDf
  localPlotConfigDf$"blabla" <- "1,2,3"
  setup <- local_plots_test(
    projectConfiguration,
    dataCombinedDf = dataCombinedDf,
    plotConfigurationDf = localPlotConfigDf,
    plotGridsDf = plotGridsDf,
    exportConfigurationDf = exportConfigurationDf
  )
  expect_error(
    createPlotsFromExcel(
      simulatedScenarios = simulatedScenarios,
      observedData = observedData,
      projectConfiguration = setup$projectConfiguration,
      stopIfNotFound = TRUE
    ),
    regexp = messages$invalidConfigurationPropertyFromExcel(
      propertyName = "blabla",
      configurationType = "DefaultPlotConfiguration"
    )
  )
})

test_that("It correctly treats names with underscores", {
  localPlotConfigDf <- plotConfigurationDf
  localPlotConfigDf$plotID <- "P_1"
  localPlotGridsDf <- plotGridsDf
  localPlotGridsDf$plotIDs <- "P_1"
  localExportConfigDf <- data.frame(
    plotGridName = rep("Aciclovir", 2),
    outputName = c("Aciclovir1", "Aciclovir2"),
    height = c(10, NA)
  )
  setup <- local_plots_test(
    projectConfiguration,
    dataCombinedDf = dataCombinedDf,
    plotConfigurationDf = localPlotConfigDf,
    plotGridsDf = localPlotGridsDf,
    exportConfigurationDf = localExportConfigDf
  )
  setup$projectConfiguration$outputFolder <- setup$tempDir
  createPlotsFromExcel(
    simulatedScenarios = simulatedScenarios,
    observedData = observedData,
    projectConfiguration = setup$projectConfiguration,
    stopIfNotFound = TRUE
  )

  # Get the most recently created/modified folder in the Figures directory
  figuresPath <- file.path(setup$tempDir, "Figures")
  latestDir <- .getLatestDirectory(figuresPath)

  expect_true(file.exists(file.path(latestDir, "Aciclovir1.png")))
  expect_true(file.exists(file.path(latestDir, "Aciclovir2.png")))
})


test_that("It correctly treats empty rows", {
  localDataCombinedDf <- data.frame(list(
    "DataCombinedName" = c("AciclovirPVB", NA, "AciclovirPVB"),
    "dataType" = c("simulated", NA, "observed"),
    "label" = c("Aciclovir simulated", NA, "Aciclovir observed"),
    "scenario" = c(scenarioNames[1], NA, NA),
    "path" = c(outputPaths, NA, NA),
    "dataSet" = c(NA, NA, names(observedData)),
    "group" = c("Aciclovir PVB", NA, "Aciclovir PVB"),
    "xOffsets" = NA,
    "xOffsetsUnits" = NA,
    "yOffsets" = NA,
    "yOffsetsUnits" = NA,
    "xScaleFactors" = NA,
    "yScaleFactors" = NA
  ))
  localPlotConfigDf <- data.frame(list(
    "plotID" = c("P1", NA, "P2"),
    "DataCombinedName" = c("AciclovirPVB", NA, "AciclovirPVB"),
    "plotType" = c("individual", NA, "individual"),
    "title" = NA,
    "xUnit" = NA,
    "yUnit" = NA,
    "xAxisScale" = NA,
    "yAxisScale" = NA,
    "xValuesLimits" = NA,
    "yValuesLimits" = NA,
    "quantiles" = NA,
    "nsd" = NA,
    "foldDistance" = NA
  ))
  localPlotGridsDf <- data.frame(list(
    "name" = c("Aciclovir", NA, "Aciclovir 2"),
    "plotIDs" = c("P1", NA, "P2"),
    "title" = c("Aciclovir PVB", NA, "Aciclovir PVB 2")
  ))
  localExportConfigDf <- data.frame(
    plotGridName = c("Aciclovir", NA, "Aciclovir"),
    outputName = c("Aciclovir1", NA, "Aciclovir2"),
    height = c(10, NA, NA)
  )
  setup <- local_plots_test(
    projectConfiguration,
    dataCombinedDf = localDataCombinedDf,
    plotConfigurationDf = localPlotConfigDf,
    plotGridsDf = localPlotGridsDf,
    exportConfigurationDf = localExportConfigDf
  )
  setup$projectConfiguration$outputFolder <- setup$tempDir
  expect_no_error(
    createPlotsFromExcel(
      simulatedScenarios = simulatedScenarios,
      observedData = observedData,
      projectConfiguration = setup$projectConfiguration,
      stopIfNotFound = TRUE
    )
  )
})


test_that("It checks if OffsetsUnits are not empty if xOffsets", {
  # Test xOffsets - missing units
  localDf1 <- dataCombinedDf
  localDf1$xOffsets <- c(1, NA)
  localDf1$xOffsetsUnits <- c(NA, NA)
  setup1 <- local_plots_test(
    projectConfiguration,
    dataCombinedDf = localDf1,
    plotConfigurationDf = plotConfigurationDf,
    plotGridsDf = plotGridsDf,
    exportConfigurationDf = exportConfigurationDf
  )
  setup1$projectConfiguration$outputFolder <- setup1$tempDir
  expect_error(
    createPlotsFromExcel(
      simulatedScenarios = simulatedScenarios,
      observedData = observedData,
      projectConfiguration = setup1$projectConfiguration,
      stopIfNotFound = TRUE
    )
  )

  # Test xOffsets - with units
  localDf2 <- dataCombinedDf
  localDf2$xOffsets <- c(1, NA)
  localDf2$xOffsetsUnits <- c("min", NA)
  setup2 <- local_plots_test(
    projectConfiguration,
    dataCombinedDf = localDf2,
    plotConfigurationDf = plotConfigurationDf,
    plotGridsDf = plotGridsDf,
    exportConfigurationDf = exportConfigurationDf
  )
  setup2$projectConfiguration$outputFolder <- setup2$tempDir
  expect_no_error(
    createPlotsFromExcel(
      simulatedScenarios = simulatedScenarios,
      observedData = observedData,
      projectConfiguration = setup2$projectConfiguration,
      stopIfNotFound = TRUE
    )
  )

  # test yOffsets - missing units
  localDf3 <- dataCombinedDf
  localDf3$yOffsets <- c(1, NA)
  localDf3$yOffsetsUnits <- c(NA, NA)
  setup3 <- local_plots_test(
    projectConfiguration,
    dataCombinedDf = localDf3,
    plotConfigurationDf = plotConfigurationDf,
    plotGridsDf = plotGridsDf,
    exportConfigurationDf = exportConfigurationDf
  )
  setup3$projectConfiguration$outputFolder <- setup3$tempDir
  expect_error(
    createPlotsFromExcel(
      simulatedScenarios = simulatedScenarios,
      observedData = observedData,
      projectConfiguration = setup3$projectConfiguration,
      stopIfNotFound = TRUE
    )
  )

  # test yOffsets - with units
  localDf4 <- dataCombinedDf
  localDf4$yOffsets <- c(1, NA)
  localDf4$yOffsetsUnits <- c("µM", NA)
  setup4 <- local_plots_test(
    projectConfiguration,
    dataCombinedDf = localDf4,
    plotConfigurationDf = plotConfigurationDf,
    plotGridsDf = plotGridsDf,
    exportConfigurationDf = exportConfigurationDf
  )
  setup4$projectConfiguration$outputFolder <- setup4$tempDir
  expect_no_error(
    createPlotsFromExcel(
      simulatedScenarios = simulatedScenarios,
      observedData = observedData,
      projectConfiguration = setup4$projectConfiguration,
      stopIfNotFound = TRUE
    )
  )
})

test_that("It throws a warning when trying to export non-existent plot grid to file", {
  # Test xOffsets - missing units
  localDf1 <- dataCombinedDf
  localDf1$xOffsets <- c(1, NA)
  localDf1$xOffsetsUnits <- c(NA, NA)
  setup1 <- local_plots_test(
    projectConfiguration,
    dataCombinedDf = localDf1,
    plotConfigurationDf = plotConfigurationDf,
    plotGridsDf = plotGridsDf,
    exportConfigurationDf = exportConfigurationDf
  )
  setup1$projectConfiguration$outputFolder <- setup1$tempDir
  expect_error(
    createPlotsFromExcel(
      simulatedScenarios = simulatedScenarios,
      observedData = observedData,
      projectConfiguration = setup1$projectConfiguration,
      stopIfNotFound = TRUE
    )
  )

  # Test xOffsets - with units
  localDf2 <- dataCombinedDf
  localDf2$xOffsets <- c(1, NA)
  localDf2$xOffsetsUnits <- c("min", NA)
  setup2 <- local_plots_test(
    projectConfiguration,
    dataCombinedDf = localDf2,
    plotConfigurationDf = plotConfigurationDf,
    plotGridsDf = plotGridsDf,
    exportConfigurationDf = exportConfigurationDf
  )
  setup2$projectConfiguration$outputFolder <- setup2$tempDir
  expect_no_error(
    createPlotsFromExcel(
      simulatedScenarios = simulatedScenarios,
      observedData = observedData,
      projectConfiguration = setup2$projectConfiguration,
      stopIfNotFound = TRUE
    )
  )

  # test yOffsets - missing units
  localDf3 <- dataCombinedDf
  localDf3$yOffsets <- c(1, NA)
  localDf3$yOffsetsUnits <- c(NA, NA)
  setup3 <- local_plots_test(
    projectConfiguration,
    dataCombinedDf = localDf3,
    plotConfigurationDf = plotConfigurationDf,
    plotGridsDf = plotGridsDf,
    exportConfigurationDf = exportConfigurationDf
  )
  setup3$projectConfiguration$outputFolder <- setup3$tempDir
  expect_error(
    createPlotsFromExcel(
      simulatedScenarios = simulatedScenarios,
      observedData = observedData,
      projectConfiguration = setup3$projectConfiguration,
      stopIfNotFound = TRUE
    )
  )

  # test yOffsets - with units
  localDf4 <- dataCombinedDf
  localDf4$yOffsets <- c(1, NA)
  localDf4$yOffsetsUnits <- c("µM", NA)
  setup4 <- local_plots_test(
    projectConfiguration,
    dataCombinedDf = localDf4,
    plotConfigurationDf = plotConfigurationDf,
    plotGridsDf = plotGridsDf,
    exportConfigurationDf = exportConfigurationDf
  )
  setup4$projectConfiguration$outputFolder <- setup4$tempDir
  expect_no_error(
    createPlotsFromExcel(
      simulatedScenarios = simulatedScenarios,
      observedData = observedData,
      projectConfiguration = setup4$projectConfiguration,
      stopIfNotFound = TRUE
    )
  )
})

test_that("It throws a warning when trying to export non-existent plot grid to file", {
  localExportConfigDf <- data.frame(
    plotGridName = "invalidPlotGridName",
    outputName = "Aciclovir1"
  )
  setup <- local_plots_test(
    projectConfiguration,
    dataCombinedDf = dataCombinedDf,
    plotConfigurationDf = plotConfigurationDf,
    plotGridsDf = plotGridsDf,
    exportConfigurationDf = localExportConfigDf
  )
  expect_warning(
    createPlotsFromExcel(
      simulatedScenarios = simulatedScenarios,
      observedData = observedData,
      projectConfiguration = setup$projectConfiguration,
      stopIfNotFound = TRUE
    ),
    regexp = messages$missingPlotGrids(
      missingPlotGrids = "invalidPlotGridName"
    )
  )
})

test_that("It throws a warning when outputName is missing in sheet 'exportConfiguration'", {
  localExportConfigDf <- data.frame(
    plotGridName = "Aciclovir",
    outputName = NA
  )
  setup <- local_plots_test(
    projectConfiguration,
    dataCombinedDf = dataCombinedDf,
    plotConfigurationDf = plotConfigurationDf,
    plotGridsDf = plotGridsDf,
    exportConfigurationDf = localExportConfigDf
  )
  expect_warning(
    createPlotsFromExcel(
      simulatedScenarios = simulatedScenarios,
      observedData = observedData,
      projectConfiguration = setup$projectConfiguration,
      stopIfNotFound = TRUE
    ),
    regexp = messages$missingOutputFileName()
  )
})

test_that(".createConfigurationFromRow correctly reads values in quotes", {
  tempDir <- tempdir()
  projectConfigurationLocal <- projectConfiguration$clone()
  projectConfigurationLocal$configurationsFolder <- tempDir

  inputValues <- c(
    "Test without quotes",
    "Test, separated",
    "Test with \"quotes\"",
    "Test with \"quotes\" and, comma",
    "Test with \"quotes, comma\"",
    "Test with, \"quotes, comma\" and, comma"
  )

  withr::with_tempfile(
    new = "Plots.xlsx",
    tmpdir = tempDir,
    code = {
      plotGridsDfLocal <- as.data.frame(lapply(plotGridsDf, rep, 6))
      plotGridsDfLocal$tagSuffix <- inputValues
      .writeExcel(
        data = list(
          "plotGrids" = plotGridsDfLocal
        ),
        path = file.path(tempDir, "Plots.xlsx"),
      )

      plotGridsDfFromExcel <- readExcel(
        file.path(tempDir, "Plots.xlsx"),
        sheet = "plotGrids"
      )

      defaultPlotGridConfig <- createEsqlabsPlotGridConfiguration()
      parsedValues <- apply(plotGridsDfLocal, 1, \(row) {
        plotGridConfiguration <- .createConfigurationFromRow(
          defaultConfiguration = defaultPlotGridConfig,
          row[!(names(row) %in% c("name", "plotIDs"))]
        )
        return(plotGridConfiguration$tagSuffix)
      })

      expectedValues <- list(
        c("Test without quotes"),
        c("Test", "separated"),
        c("Test with quotes"),
        c("Test with quotes and", "comma"),
        "Test with quotes, comma",
        c("Test with", "quotes, comma and", "comma")
      )
      expect_equal(parsedValues, expectedValues)
    }
  )
})

test_that("It ignores a title argument in plotGrids when the title column is not present", {
  localPlotGridsDf <- plotGridsDf
  localPlotGridsDf$title <- NULL
  setup <- local_plots_test(
    projectConfiguration,
    dataCombinedDf = dataCombinedDf,
    plotConfigurationDf = plotConfigurationDf,
    plotGridsDf = localPlotGridsDf,
    exportConfigurationDf = exportConfigurationDf
  )
  expect_no_error(
    createPlotsFromExcel(
      simulatedScenarios = simulatedScenarios,
      observedData = observedData,
      projectConfiguration = setup$projectConfiguration,
      stopIfNotFound = TRUE
    )
  )
})

# Tests for issue #848: Better validation for Excel fields
test_that("It provides clear error when xValuesLimits uses space instead of comma (#848)", {
  localPlotConfigDf <- data.frame(list(
    "plotID" = "TestPlot",
    "DataCombinedName" = "AciclovirPVB",
    "plotType" = "individual",
    "title" = NA,
    "xUnit" = NA,
    "yUnit" = NA,
    "xAxisScale" = NA,
    "yAxisScale" = NA,
    "xValuesLimits" = "72 80",
    "yValuesLimits" = NA,
    "quantiles" = NA,
    "nsd" = NA,
    "foldDistance" = NA
  ))
  localPlotGridsDf <- data.frame(list(
    "name" = "TestGrid",
    "plotIDs" = "TestPlot",
    "title" = "Test Grid"
  ))
  setup <- local_plots_test(
    projectConfiguration,
    dataCombinedDf = dataCombinedDf,
    plotConfigurationDf = localPlotConfigDf,
    plotGridsDf = localPlotGridsDf,
    exportConfigurationDf = exportConfigurationDf
  )
  setup$projectConfiguration$plotsFile <- file.path(setup$tempDir, "Plots.xlsx")
  expect_error(
    createPlotsFromExcel(
      simulatedScenarios = simulatedScenarios,
      observedData = observedData,
      projectConfiguration = setup$projectConfiguration,
      stopIfNotFound = TRUE
    ),
    regexp = "Excel validation error.*xValuesLimits",
    fixed = FALSE
  )
})

test_that("It accepts correctly formatted comma-separated axis limits (#848)", {
  localPlotConfigDf <- data.frame(list(
    "plotID" = "TestPlot",
    "DataCombinedName" = "AciclovirPVB",
    "plotType" = "individual",
    "title" = NA,
    "xUnit" = NA,
    "yUnit" = NA,
    "xAxisScale" = NA,
    "yAxisScale" = NA,
    "xValuesLimits" = "72, 80",
    "yAxisLimits" = "0,100",
    "yValuesLimits" = NA,
    "quantiles" = NA,
    "nsd" = NA,
    "foldDistance" = NA
  ))
  localPlotGridsDf <- data.frame(list(
    "name" = "TestGrid",
    "plotIDs" = "TestPlot",
    "title" = "Test Grid"
  ))
  setup <- local_plots_test(
    projectConfiguration,
    dataCombinedDf = dataCombinedDf,
    plotConfigurationDf = localPlotConfigDf,
    plotGridsDf = localPlotGridsDf,
    exportConfigurationDf = exportConfigurationDf
  )
  setup$projectConfiguration$plotsFile <- file.path(setup$tempDir, "Plots.xlsx")
  expect_no_error(
    createPlotsFromExcel(
      simulatedScenarios = simulatedScenarios,
      observedData = observedData,
      projectConfiguration = setup$projectConfiguration,
      stopIfNotFound = TRUE
    )
  )
})

test_that("It provides clear error for wrong number of axis limit values (#848)", {
  localPlotConfigDf <- data.frame(list(
    "plotID" = "TestPlot2",
    "DataCombinedName" = "AciclovirPVB",
    "plotType" = "individual",
    "title" = NA,
    "xUnit" = NA,
    "yUnit" = NA,
    "xAxisScale" = NA,
    "yAxisScale" = NA,
    "xValuesLimits" = NA,
    "yValuesLimits" = "100",
    "quantiles" = NA,
    "nsd" = NA,
    "foldDistance" = NA
  ))
  localPlotGridsDf <- data.frame(list(
    "name" = "TestGrid",
    "plotIDs" = "TestPlot2",
    "title" = "Test Grid"
  ))
  setup <- local_plots_test(
    projectConfiguration,
    dataCombinedDf = dataCombinedDf,
    plotConfigurationDf = localPlotConfigDf,
    plotGridsDf = localPlotGridsDf,
    exportConfigurationDf = exportConfigurationDf
  )
  setup$projectConfiguration$plotsFile <- file.path(setup$tempDir, "Plots.xlsx")
  expect_error(
    createPlotsFromExcel(
      simulatedScenarios = simulatedScenarios,
      observedData = observedData,
      projectConfiguration = setup$projectConfiguration,
      stopIfNotFound = TRUE
    ),
    regexp = "Excel validation error.*yValuesLimits.*Expected: 2",
    fixed = FALSE
  )
})

test_that("It shows a warning when xAxisScale is log and xAxisLimits contain 0", {
  localPlotConfigDf <- plotConfigurationDf
  localPlotConfigDf$xAxisScale <- "log"
  localPlotConfigDf$xAxisLimits <- "0, 100"
  setup <- local_plots_test(
    projectConfiguration,
    dataCombinedDf = dataCombinedDf,
    plotConfigurationDf = localPlotConfigDf,
    plotGridsDf = plotGridsDf,
    exportConfigurationDf = exportConfigurationDf
  )
  expect_warning(
    createPlotsFromExcel(
      simulatedScenarios = simulatedScenarios,
      observedData = observedData,
      projectConfiguration = setup$projectConfiguration,
      stopIfNotFound = TRUE
    ),
    regexp = messages$warningLogScaleWithZeroLimit(
      plotID = "P1",
      axisLimitsField = "xAxisLimits",
      axis = "x"
    ),
    fixed = TRUE
  )
})

test_that("It shows a warning when yAxisScale is log and yAxisLimits contain 0", {
  localPlotConfigDf <- plotConfigurationDf
  localPlotConfigDf$yAxisScale <- "log"
  localPlotConfigDf$yAxisLimits <- "0, 100"
  setup <- local_plots_test(
    projectConfiguration,
    dataCombinedDf = dataCombinedDf,
    plotConfigurationDf = localPlotConfigDf,
    plotGridsDf = plotGridsDf,
    exportConfigurationDf = exportConfigurationDf
  )
  expect_warning(
    createPlotsFromExcel(
      simulatedScenarios = simulatedScenarios,
      observedData = observedData,
      projectConfiguration = setup$projectConfiguration,
      stopIfNotFound = TRUE
    ),
    regexp = messages$warningLogScaleWithZeroLimit(
      plotID = "P1",
      axisLimitsField = "yAxisLimits",
      axis = "y"
    ),
    fixed = TRUE
  )
})

test_that("It shows a warning when yAxisScale is log and yValuesLimits contain 0", {
  localPlotConfigDf <- plotConfigurationDf
  localPlotConfigDf$yAxisScale <- "log"
  localPlotConfigDf$yValuesLimits <- "0, 100"
  setup <- local_plots_test(
    projectConfiguration,
    dataCombinedDf = dataCombinedDf,
    plotConfigurationDf = localPlotConfigDf,
    plotGridsDf = plotGridsDf,
    exportConfigurationDf = exportConfigurationDf
  )
  expect_warning(
    createPlotsFromExcel(
      simulatedScenarios = simulatedScenarios,
      observedData = observedData,
      projectConfiguration = setup$projectConfiguration,
      stopIfNotFound = TRUE
    ),
    regexp = messages$warningLogScaleWithZeroLimit(
      plotID = "P1",
      axisLimitsField = "yValuesLimits",
      axis = "y"
    ),
    fixed = TRUE
  )
})
