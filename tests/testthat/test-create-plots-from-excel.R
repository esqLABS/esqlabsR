project <- testProject()

scenarioNames <- c("TestScenario", "PopulationScenario")
outputPaths <- "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)"

simulatedScenarios <- runScenarios(
  project,
  scenarioNames = scenarioNames
)

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

observedDataForSetup <- loadObservedData(project)

dataCombinedDf <- data.frame(list(
  "DataCombinedName" = c("AciclovirPVB", "AciclovirPVB"),
  "dataType" = c("simulated", "observed"),
  "label" = c("Aciclovir simulated", "Aciclovir observed"),
  "scenario" = c(scenarioNames[1], NA),
  "path" = c(outputPaths, NA),
  "dataSet" = c(NA, names(observedDataForSetup)[1]),
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
  "name" = character(0)
))

# Helper function to get the most recently created/modified directory
.getLatestDirectory <- function(baseDir) {
  allDirs <- list.dirs(baseDir, full.names = TRUE, recursive = FALSE)
  allDirs[which.max(file.info(allDirs)$mtime)]
}

# Helper to create a project with custom plots data
.withPlots <- function(
  pc = project,
  dataCombined = dataCombinedDf,
  plotConfiguration = plotConfigurationDf,
  plotGrids = plotGridsDf,
  exportConfiguration = exportConfigurationDf
) {
  pcLocal <- pc$clone()
  pcLocal$plots <- list(
    dataCombined = dataCombined,
    plotConfiguration = plotConfiguration,
    plotGrids = plotGrids,
    exportConfiguration = exportConfiguration
  )
  pcLocal
}

# Validation DataCombined
test_that("It throws an error if mandatory field dataType is not filled out", {
  dataCombinedDfLocal <- dataCombinedDf
  dataCombinedDfLocal$dataType <- NA
  pcLocal <- .withPlots(dataCombined = dataCombinedDfLocal)

  expect_error(
    createPlots(
      project = pcLocal,
      simulatedScenarios = simulatedScenarios,
      stopIfNotFound = TRUE
    ),
    regexp = messages$missingDataType()
  )
})

test_that("It throws an error if mandatory field label is not filled out", {
  dataCombinedDfLocal <- dataCombinedDf
  dataCombinedDfLocal$label <- NA
  pcLocal <- .withPlots(dataCombined = dataCombinedDfLocal)

  expect_error(
    createPlots(
      project = pcLocal,
      simulatedScenarios = simulatedScenarios,
      stopIfNotFound = TRUE
    ),
    regexp = messages$missingLabel()
  )
})

test_that("It throws an error if no scenario is specified for a simulated data", {
  dataCombinedDfLocal <- dataCombinedDf
  dataCombinedDfLocal$scenario <- NA
  pcLocal <- .withPlots(dataCombined = dataCombinedDfLocal)

  expect_error(
    createPlots(
      project = pcLocal,
      simulatedScenarios = simulatedScenarios,
      stopIfNotFound = TRUE
    ),
    regexp = messages$missingScenarioName()
  )
})

test_that("It throws an error if no output path is specified for a simulated data", {
  dataCombinedDfLocal <- dataCombinedDf
  dataCombinedDfLocal$path <- NA
  pcLocal <- .withPlots(dataCombined = dataCombinedDfLocal)

  expect_error(
    createPlots(
      project = pcLocal,
      simulatedScenarios = simulatedScenarios,
      stopIfNotFound = TRUE
    ),
    regexp = messages$stopNoPathProvided("AciclovirPVB")
  )
})

test_that("It throws an error if wrong output path is specified for a simulated data", {
  dataCombinedDfLocal <- dataCombinedDf
  dataCombinedDfLocal$path <- "foo"
  pcLocal <- .withPlots(dataCombined = dataCombinedDfLocal)

  expect_error(
    createPlots(
      project = pcLocal,
      simulatedScenarios = simulatedScenarios,
      stopIfNotFound = TRUE
    ),
    regexp = messages$stopWrongOutputPath(
      dataCombinedName = dataCombinedDfLocal$DataCombinedName[[1]],
      scenarioName = dataCombinedDfLocal$scenario[[1]],
      path = dataCombinedDfLocal$path[[1]]
    )
  )
})

test_that("It throws an error if no data set is specified for observed data", {
  dataCombinedDfLocal <- dataCombinedDf
  dataCombinedDfLocal$dataSet <- NA
  pcLocal <- .withPlots(dataCombined = dataCombinedDfLocal)

  expect_error(
    createPlots(
      project = pcLocal,
      simulatedScenarios = simulatedScenarios,
      stopIfNotFound = TRUE
    ),
    regexp = messages$stopNoDataSetProvided("AciclovirPVB")
  )
})

test_that("It throws an error if defined scenario is missing and stopIfNotFound is TRUE", {
  dataCombinedDfLocal <- dataCombinedDf
  dataCombinedDfLocal$scenario <- c("TestScenario", "foo")
  pcLocal <- .withPlots(dataCombined = dataCombinedDfLocal)

  expect_error(
    createPlots(
      project = pcLocal,
      simulatedScenarios = simulatedScenarios,
      stopIfNotFound = TRUE
    ),
    regexp = messages$warningInvalidScenarioName("foo")
  )
})

test_that("It shows a warning for missing scenarios if stopIfNotFound is FALSE", {
  dataCombinedDfLocal <- dataCombinedDf
  dataCombinedDfLocal$scenario <- c(scenarioNames[1], "foo")
  pcLocal <- .withPlots(dataCombined = dataCombinedDfLocal)

  expect_warning(
    createPlots(
      project = pcLocal,
      simulatedScenarios = simulatedScenarios,
      stopIfNotFound = FALSE
    ),
    regexp = messages$warningInvalidScenarioName("foo")
  )
})

test_that("It throws an error if defined data set is missing and stopIfNotFound is TRUE", {
  dataCombinedDfLocal <- dataCombinedDf
  dataCombinedDfLocal$dataSet <- c(scenarioNames[1], names(observedDataForSetup)[1])
  pcLocal <- .withPlots(dataCombined = dataCombinedDfLocal)

  expect_error(
    createPlots(
      project = pcLocal,
      simulatedScenarios = simulatedScenarios,
      stopIfNotFound = TRUE
    ),
    regexp = messages$stopInvalidDataSetName(scenarioNames[1])
  )
})

test_that("It shows a warning for missing data set if stopIfNotFound is FALSE", {
  dataCombinedDfLocal <- dataCombinedDf
  dataCombinedDfLocal$dataSet <- c(scenarioNames[1], names(observedDataForSetup)[1])
  pcLocal <- .withPlots(dataCombined = dataCombinedDfLocal)

  expect_warning(
    createPlots(
      project = pcLocal,
      simulatedScenarios = simulatedScenarios,
      stopIfNotFound = FALSE
    ),
    regexp = messages$warningInvalidDataSetName(scenarioNames[1])
  )
})

test_that("It throws an error if mandatory field DataCombinedName is not filled out", {
  plotConfigurationDfLocal <- plotConfigurationDf
  plotConfigurationDfLocal$DataCombinedName <- NA
  pcLocal <- .withPlots(plotConfiguration = plotConfigurationDfLocal)

  expect_error(
    createPlots(
      project = pcLocal,
      simulatedScenarios = simulatedScenarios,
      stopIfNotFound = TRUE
    ),
    regexp = messages$missingDataCombinedName()
  )
})

test_that("It throws an error if mandatory field plotType is not filled out", {
  plotConfigurationDfLocal <- plotConfigurationDf
  plotConfigurationDfLocal$plotType <- NA
  pcLocal <- .withPlots(plotConfiguration = plotConfigurationDfLocal)

  expect_error(
    createPlots(
      project = pcLocal,
      simulatedScenarios = simulatedScenarios,
      stopIfNotFound = TRUE
    ),
    regexp = messages$missingPlotType()
  )
})

test_that("It throws an error if a plot requires a DataCombined that is not defined", {
  plotConfigurationDfLocal <- plotConfigurationDf
  plotConfigurationDfLocal$DataCombinedName <- "foo"
  pcLocal <- .withPlots(plotConfiguration = plotConfigurationDfLocal)

  expect_error(
    createPlots(
      project = pcLocal,
      simulatedScenarios = simulatedScenarios,
      stopIfNotFound = TRUE
    ),
    regexp = messages$stopInvalidDataCombinedName("foo")
  )
})

# Validation plotGrids
test_that("It returns NULL if no plotGrids are defined", {
  plotGridsDfLocal <- data.frame(list(
    "name" = NA,
    "plotIDs" = NA,
    "title" = NA
  ))
  pcLocal <- .withPlots(plotGrids = plotGridsDfLocal)

  plots <- createPlots(
    project = pcLocal,
    simulatedScenarios = simulatedScenarios,
    stopIfNotFound = TRUE
  )
  expect_null(plots)
})

test_that("It creates plots for all plot grids when plotGridNames is NULL", {
  plots <- createPlots(
    project = project,
    simulatedScenarios = simulatedScenarios,
    stopIfNotFound = TRUE
  )
  expect_equal(names(plots), c("Aciclovir", "Aciclovir2", "Aciclovir3"))
})

test_that("When custom DataCombined is passed, it is used instead of the one from config", {
  dataCombinedList <- createDataCombined(
    project = project,
    plotGridNames = c("Aciclovir", "Aciclovir2", "Aciclovir3"),
    simulatedScenarios = simulatedScenarios
  )

  # Change the x-offset of a data combined
  dataCombinedList$AciclovirPVB$setDataTransformations(xOffsets = 0)

  plots <- createPlots(
    project = project,
    simulatedScenarios = simulatedScenarios,
    dataCombinedList = dataCombinedList,
    stopIfNotFound = TRUE
  )
  vdiffr::expect_doppelganger(title = "firstPlot", plots[[1]])
  vdiffr::expect_doppelganger(title = "secondPlot", plots[[2]])
})

test_that("It can create plots when custom data combined are passed that are missing in the config", {
  dataCombinedList <- createDataCombined(
    project = project,
    plotGridNames = c("Aciclovir", "Aciclovir2", "Aciclovir3"),
    simulatedScenarios = simulatedScenarios
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

  pcLocal <- .withPlots(dataCombined = emptyDataCombinedDf)

  plots <- createPlots(
    project = pcLocal,
    simulatedScenarios = simulatedScenarios,
    dataCombinedList = dataCombinedList,
    stopIfNotFound = TRUE
  )
  vdiffr::expect_doppelganger(title = "firstPlotCustomDC", plots[[1]])
})

test_that("It creates plots only for specified plotGrids", {
  plots <- createPlots(
    plotGridNames = "Aciclovir",
    project = project,
    simulatedScenarios = simulatedScenarios,
    stopIfNotFound = TRUE
  )
  expect_equal(names(plots), c("Aciclovir"))
})

test_that("It throws an error when specified plot grid names are not defined", {
  expect_error(
    createPlots(
      plotGridNames = c("foo", "Aciclovir", "bar"),
      project = project,
      simulatedScenarios = simulatedScenarios,
      stopIfNotFound = TRUE
    ),
    messages$invalidPlotGridNames(c("foo", "bar"))
  )
})

test_that("It throws an error if mandatory field plotIDs is not filled out", {
  plotGridsDfLocal <- plotGridsDf
  plotGridsDfLocal$plotIDs <- NA
  pcLocal <- .withPlots(plotGrids = plotGridsDfLocal)

  expect_error(
    createPlots(
      project = pcLocal,
      simulatedScenarios = simulatedScenarios,
      stopIfNotFound = TRUE
    ),
    regexp = messages$missingPlotIDs()
  )
})


test_that("It throws an error if plotIDs are not unique", {
  plotConfigurationDfLocal <- data.frame(list(
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
  pcLocal <- .withPlots(plotConfiguration = plotConfigurationDfLocal)

  expect_error(
    createPlots(
      project = pcLocal,
      simulatedScenarios = simulatedScenarios,
      stopIfNotFound = TRUE
    ),
    regexp = messages$PlotIDsMustBeUnique(c("P1")),
    fixed = TRUE
  )
})

test_that("It throws an error if plotGrid names are not unique", {
  plotGridsDfLocal <- data.frame(list(
    "name" = c("Aciclovir", "Aciclovir"),
    "plotIDs" = c("P1", "P2"),
    "title" = c("Aciclovir PVB", "Aciclovir PVB2")
  ))
  pcLocal <- .withPlots(plotGrids = plotGridsDfLocal)

  expect_error(
    createPlots(
      project = pcLocal,
      simulatedScenarios = simulatedScenarios,
      stopIfNotFound = TRUE
    ),
    regexp = messages$PlotGridsNamesMustBeUnique(c("Aciclovir")),
    fixed = TRUE
  )
})

test_that("It throws an error if a plot grid requires a plot id that is not defined", {
  plotGridsDfLocal <- plotGridsDf
  plotGridsDfLocal$plotIDs <- "foo"
  pcLocal <- .withPlots(plotGrids = plotGridsDfLocal)

  expect_error(
    createPlots(
      project = pcLocal,
      simulatedScenarios = simulatedScenarios,
      stopIfNotFound = TRUE
    ),
    regexp = messages$errorInvalidPlotID("foo")
  )
})

test_that("It exports plot grids as defined in exportConfiguration", {
  exportConfigurationDfLocal <- data.frame(
    plotGridName = rep("Aciclovir", 2),
    name = c("Aciclovir1", "Aciclovir2"),
    height = c(10, NA)
  )
  pcLocal <- .withPlots(exportConfiguration = exportConfigurationDfLocal)
  pcLocal$outputFolder <- tempdir()

  createPlots(
    project = pcLocal,
    simulatedScenarios = simulatedScenarios,
    stopIfNotFound = TRUE
  )
  # Get the most recently created/modified folder in the Figures directory
  latestDir <- .getLatestDirectory(file.path(tempdir(), "Figures"))
  expect_true(file.exists(file.path(latestDir, "Aciclovir1.png")))
  expect_true(file.exists(file.path(latestDir, "Aciclovir2.png")))
})

test_that("It exports plot grids with specified output folder", {
  exportConfigurationDfLocal <- data.frame(
    plotGridName = rep("Aciclovir", 2),
    name = c("Aciclovir1", "Aciclovir2"),
    height = c(10, NA)
  )
  pcLocal <- .withPlots(exportConfiguration = exportConfigurationDfLocal)
  pcLocal$outputFolder <- tempdir()

  createPlots(
    project = pcLocal,
    simulatedScenarios = simulatedScenarios,
    stopIfNotFound = TRUE,
    outputFolder = tempdir()
  )
  # Get the most recently created/modified folder in the Figures directory
  latestDir <- .getLatestDirectory(file.path(tempdir(), "Figures"))
  expect_true(file.exists(file.path(latestDir, "Aciclovir1.png")))
  expect_true(file.exists(file.path(latestDir, "Aciclovir2.png")))
})

test_that("It throws an error when trying to set a property that is not supported by the configuration", {
  plotConfigurationDfLocal <- plotConfigurationDf
  plotConfigurationDfLocal$"blabla" <- "1,2,3"
  pcLocal <- .withPlots(plotConfiguration = plotConfigurationDfLocal)

  expect_error(
    createPlots(
      project = pcLocal,
      simulatedScenarios = simulatedScenarios,
      stopIfNotFound = TRUE
    ),
    regexp = messages$invalidConfigurationProperty(
      propertyName = "blabla",
      configurationType = "DefaultPlotConfiguration"
    )
  )
})

test_that("It correctly treats names with underscores", {
  plotConfigurationDfLocal <- plotConfigurationDf
  plotConfigurationDfLocal$plotID <- "P_1"
  plotGridsDfLocal <- plotGridsDf
  plotGridsDfLocal$plotIDs <- "P_1"
  exportConfigurationDfLocal <- data.frame(
    plotGridName = rep("Aciclovir", 2),
    name = c("Aciclovir1", "Aciclovir2"),
    height = c(10, NA)
  )
  pcLocal <- .withPlots(
    plotConfiguration = plotConfigurationDfLocal,
    plotGrids = plotGridsDfLocal,
    exportConfiguration = exportConfigurationDfLocal
  )
  pcLocal$outputFolder <- tempdir()

  createPlots(
    project = pcLocal,
    simulatedScenarios = simulatedScenarios,
    stopIfNotFound = TRUE
  )

  # Get the most recently created/modified folder in the Figures directory
  figuresPath <- file.path(tempdir(), "Figures")
  latestDir <- .getLatestDirectory(figuresPath)

  expect_true(file.exists(file.path(latestDir, "Aciclovir1.png")))
  expect_true(file.exists(file.path(latestDir, "Aciclovir2.png")))
})


test_that("It correctly treats empty rows", {
  # datacombined with empty row
  dataCombinedDfLocal <- data.frame(list(
    "DataCombinedName" = c("AciclovirPVB", NA, "AciclovirPVB"),
    "dataType" = c("simulated", NA, "observed"),
    "label" = c("Aciclovir simulated", NA, "Aciclovir observed"),
    "scenario" = c(scenarioNames[1], NA, NA),
    "path" = c(outputPaths, NA, NA),
    "dataSet" = c(NA, NA, names(observedDataForSetup)[1]),
    "group" = c("Aciclovir PVB", NA, "Aciclovir PVB"),
    "xOffsets" = NA,
    "xOffsetsUnits" = NA,
    "yOffsets" = NA,
    "yOffsetsUnits" = NA,
    "xScaleFactors" = NA,
    "yScaleFactors" = NA
  ))
  # plotConfiguration with empty row
  plotConfigurationDfLocal <- data.frame(list(
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
  # plotGrid with empty row
  plotGridsDfLocal <- data.frame(list(
    "name" = c("Aciclovir", NA, "Aciclovir 2"),
    "plotIDs" = c("P1", NA, "P2"),
    "title" = c("Aciclovir PVB", NA, "Aciclovir PVB 2")
  ))
  # exportConfiguration with empty row
  exportConfigurationDfLocal <- data.frame(
    plotGridName = c("Aciclovir", NA, "Aciclovir"),
    name = c("Aciclovir1", NA, "Aciclovir2"),
    height = c(10, NA, NA)
  )
  pcLocal <- .withPlots(
    dataCombined = dataCombinedDfLocal,
    plotConfiguration = plotConfigurationDfLocal,
    plotGrids = plotGridsDfLocal,
    exportConfiguration = exportConfigurationDfLocal
  )
  pcLocal$outputFolder <- tempdir()

  expect_no_error(
    createPlots(
      project = pcLocal,
      simulatedScenarios = simulatedScenarios,
      stopIfNotFound = TRUE
    )
  )
})


test_that("It checks if OffsetsUnits are not empty if xOffsets", {
  # Test xOffsets without units
  dataCombinedDfLocal <- dataCombinedDf
  dataCombinedDfLocal$xOffsets <- c(1, NA)
  dataCombinedDfLocal$xOffsetsUnits <- c(NA, NA)
  pcLocal <- .withPlots(dataCombined = dataCombinedDfLocal)

  expect_error(
    createPlots(
      project = pcLocal,
      simulatedScenarios = simulatedScenarios,
      stopIfNotFound = TRUE
    )
  )

  # Test xOffsets with units
  dataCombinedDfLocal <- dataCombinedDf
  dataCombinedDfLocal$xOffsets <- c(1, NA)
  dataCombinedDfLocal$xOffsetsUnits <- c("min", NA)
  pcLocal <- .withPlots(dataCombined = dataCombinedDfLocal)

  expect_no_error(
    createPlots(
      project = pcLocal,
      simulatedScenarios = simulatedScenarios,
      stopIfNotFound = TRUE
    )
  )

  # test yOffsets without units
  dataCombinedDfLocal <- dataCombinedDf
  dataCombinedDfLocal$yOffsets <- c(1, NA)
  dataCombinedDfLocal$yOffsetsUnits <- c(NA, NA)
  pcLocal <- .withPlots(dataCombined = dataCombinedDfLocal)

  expect_error(
    createPlots(
      project = pcLocal,
      simulatedScenarios = simulatedScenarios,
      stopIfNotFound = TRUE
    )
  )

  # Test yOffsets with units
  dataCombinedDfLocal <- dataCombinedDf
  dataCombinedDfLocal$yOffsets <- c(1, NA)
  dataCombinedDfLocal$yOffsetsUnits <- c("\u00b5M", NA)
  pcLocal <- .withPlots(dataCombined = dataCombinedDfLocal)

  expect_no_error(
    createPlots(
      project = pcLocal,
      simulatedScenarios = simulatedScenarios,
      stopIfNotFound = TRUE
    )
  )
})

test_that("It throws a warning when trying to export non-existent plot grid to file", {
  exportConfigurationDfLocal <- data.frame(
    plotGridName = "invalidPlotGridName",
    name = "Aciclovir1"
  )
  pcLocal <- .withPlots(exportConfiguration = exportConfigurationDfLocal)

  expect_warning(
    createPlots(
      project = pcLocal,
      simulatedScenarios = simulatedScenarios,
      stopIfNotFound = TRUE
    ),
    regexp = messages$missingPlotGrids(
      missingPlotGrids = "invalidPlotGridName"
    )
  )
})

test_that("It throws a warning when output name is missing in exportConfiguration", {
  exportConfigurationDfLocal <- data.frame(
    plotGridName = "Aciclovir",
    name = NA
  )
  pcLocal <- .withPlots(exportConfiguration = exportConfigurationDfLocal)

  expect_warning(
    createPlots(
      project = pcLocal,
      simulatedScenarios = simulatedScenarios,
      stopIfNotFound = TRUE
    ),
    regexp = messages$missingOutputFileName()
  )
})

test_that(".createConfigurationFromRow correctly reads values in quotes", {
  plotGridsDfLocal <- as.data.frame(lapply(plotGridsDf, rep, 6))

  inputValues <- c(
    "Test without quotes",
    "Test, separated",
    "Test with \"quotes\"",
    "Test with \"quotes\" and, comma",
    "Test with \"quotes, comma\"",
    "Test with, \"quotes, comma\" and, comma"
  )
  plotGridsDfLocal$tagSuffix <- inputValues

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
})

test_that("It ignores a title argument in plotGrids when the title column is not present", {
  plotGridsDfLocal <- plotGridsDf
  plotGridsDfLocal$title <- NULL # Remove the title column
  pcLocal <- .withPlots(plotGrids = plotGridsDfLocal)

  expect_no_error(
    createPlots(
      project = pcLocal,
      simulatedScenarios = simulatedScenarios,
      stopIfNotFound = TRUE
    )
  )
})

# Tests for issue #848: Better validation for fields
test_that("It provides clear error when xValuesLimits uses space instead of comma (#848)", {
  plotConfigurationDfLocal <- data.frame(list(
    "plotID" = "TestPlot",
    "DataCombinedName" = "AciclovirPVB",
    "plotType" = "individual",
    "title" = NA,
    "xUnit" = NA,
    "yUnit" = NA,
    "xAxisScale" = NA,
    "yAxisScale" = NA,
    "xValuesLimits" = "72 80", # Space-separated - should trigger clear error
    "yValuesLimits" = NA,
    "quantiles" = NA,
    "nsd" = NA,
    "foldDistance" = NA
  ))

  plotGridsDfLocal <- data.frame(list(
    "name" = "TestGrid",
    "plotIDs" = "TestPlot",
    "title" = "Test Grid"
  ))

  pcLocal <- .withPlots(
    plotConfiguration = plotConfigurationDfLocal,
    plotGrids = plotGridsDfLocal
  )

  expect_error(
    createPlots(
      project = pcLocal,
      simulatedScenarios = simulatedScenarios,
      stopIfNotFound = TRUE
    ),
    regexp = "Validation error.*xValuesLimits",
    fixed = FALSE
  )
})

test_that("It accepts correctly formatted comma-separated axis limits (#848)", {
  plotConfigurationDfLocal <- data.frame(list(
    "plotID" = "TestPlot",
    "DataCombinedName" = "AciclovirPVB",
    "plotType" = "individual",
    "title" = NA,
    "xUnit" = NA,
    "yUnit" = NA,
    "xAxisScale" = NA,
    "yAxisScale" = NA,
    "xValuesLimits" = "72, 80", # Correct format
    "yAxisLimits" = "0,100", # Also correct (no space after comma)
    "yValuesLimits" = NA,
    "quantiles" = NA,
    "nsd" = NA,
    "foldDistance" = NA
  ))

  plotGridsDfLocal <- data.frame(list(
    "name" = "TestGrid",
    "plotIDs" = "TestPlot",
    "title" = "Test Grid"
  ))

  pcLocal <- .withPlots(
    plotConfiguration = plotConfigurationDfLocal,
    plotGrids = plotGridsDfLocal
  )

  expect_no_error(
    createPlots(
      project = pcLocal,
      simulatedScenarios = simulatedScenarios,
      stopIfNotFound = TRUE
    )
  )
})

test_that("It provides clear error for wrong number of axis limit values (#848)", {
  plotConfigurationDfLocal <- data.frame(list(
    "plotID" = "TestPlot2",
    "DataCombinedName" = "AciclovirPVB",
    "plotType" = "individual",
    "title" = NA,
    "xUnit" = NA,
    "yUnit" = NA,
    "xAxisScale" = NA,
    "yAxisScale" = NA,
    "xValuesLimits" = NA,
    "yValuesLimits" = "100", # Only one value - should need 2
    "quantiles" = NA,
    "nsd" = NA,
    "foldDistance" = NA
  ))

  plotGridsDfLocal <- data.frame(list(
    "name" = "TestGrid",
    "plotIDs" = "TestPlot2",
    "title" = "Test Grid"
  ))

  pcLocal <- .withPlots(
    plotConfiguration = plotConfigurationDfLocal,
    plotGrids = plotGridsDfLocal
  )

  expect_error(
    createPlots(
      project = pcLocal,
      simulatedScenarios = simulatedScenarios,
      stopIfNotFound = TRUE
    ),
    regexp = "Validation error.*yValuesLimits.*Expected: 2",
    fixed = FALSE
  )
})

test_that("It shows a warning when xAxisScale is log and xAxisLimits contain 0", {
  plotConfigurationDfLocal <- plotConfigurationDf
  plotConfigurationDfLocal$xAxisScale <- "log"
  plotConfigurationDfLocal$xAxisLimits <- "0, 100"
  pcLocal <- .withPlots(plotConfiguration = plotConfigurationDfLocal)

  expect_warning(
    createPlots(
      project = pcLocal,
      simulatedScenarios = simulatedScenarios,
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
  plotConfigurationDfLocal <- plotConfigurationDf
  plotConfigurationDfLocal$yAxisScale <- "log"
  plotConfigurationDfLocal$yAxisLimits <- "0, 100"
  pcLocal <- .withPlots(plotConfiguration = plotConfigurationDfLocal)

  expect_warning(
    createPlots(
      project = pcLocal,
      simulatedScenarios = simulatedScenarios,
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
  plotConfigurationDfLocal <- plotConfigurationDf
  plotConfigurationDfLocal$yAxisScale <- "log"
  plotConfigurationDfLocal$yValuesLimits <- "0, 100"
  pcLocal <- .withPlots(plotConfiguration = plotConfigurationDfLocal)

  expect_warning(
    createPlots(
      project = pcLocal,
      simulatedScenarios = simulatedScenarios,
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
