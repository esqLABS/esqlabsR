# esqlabsColors ----------------------------------------------------------

test_that("esqlabsColors input validation works as expected", {
  expect_error(
    esqlabsColors(-1),
    regexp = messages$nrOfColorsShouldBePositive(-1),
    fixed = TRUE
  )
})

test_that("esqlabsColors works with empty argument vector", {
  expect_length(esqlabsColors(0), 0)
})

test_that("esqlabsColors returns two colors", {
  expect_length(esqlabsColors(2), 2)
})

test_that("esqlabsColors returns three colors", {
  expect_length(esqlabsColors(3), 3)
})

test_that("esqlabsColors returns ten colors", {
  expect_length(esqlabsColors(10), 10)
})

test_that("esqlabsColors returns ten colors", {
  expect_length(esqlabsColors(10), 10)
})

# col2hsv -----------------------------------------------------------------

test_that("col2hsv returns expected HSV values for a given R color name", {
  expect_equal(
    col2hsv("yellow"),
    structure(
      c(0.166666666666667, 1, 1),
      .Dim = c(3L, 1L),
      .Dimnames = list(c("h", "s", "v"), NULL)
    )
  )

  expect_equal(
    col2hsv("white"),
    structure(
      c(0, 0, 1),
      .Dim = c(3L, 1L),
      .Dimnames = list(c("h", "s", "v"), NULL)
    )
  )
})

# createEsqlabsPlotConfiguration ------------------------------------------

test_that("createEsqlabsPlotConfiguration() creates object with chosen defaults", {
  myPC <- createEsqlabsPlotConfiguration()
  expect_true(isOfType(myPC, "DefaultPlotConfiguration"))
  expect_equal(myPC$titleSize, 10)
})

test_that("createEsqlabsPlotGridConfiguration() creates object with chosen defaults", {
  myPGC <- createEsqlabsPlotGridConfiguration()
  expect_true(isOfType(myPGC, "PlotGridConfiguration"))
  expect_equal(myPGC$tagLevels, "a")
})

test_that("createEsqlabsExportConfiguration() creates object with chosen defaults", {
  myProjConfig <- Project$new()
  myEC <- createEsqlabsExportConfiguration(myProjConfig$outputFolder)
  expect_true(isOfType(myEC, "ExportConfiguration"))
  expect_equal(myEC$units, "cm")
})


test_that("esqlabsPlotConfiguration fields match DefaultPlotConfiguration", {
  defaultConfig <- ospsuite::DefaultPlotConfiguration$new()
  esqlabsConfig <- createEsqlabsPlotConfiguration()

  # Check if all fields from DefaultPlotConfiguration are present in esqLabs configuration
  defaultFields <- names(defaultConfig)
  esqlabsFields <- names(esqlabsConfig)

  missingFields <- setdiff(defaultFields, esqlabsFields)
  expect_true(
    length(missingFields) == 0,
    info = paste("Missing fields:", paste(missingFields, collapse = ", "))
  )

  # Only override fields where differences are intentional
  # and backward compatibility with `ospsuite` plotting functions was verified
  esqlabsConfig$linesColor <- NULL
  esqlabsConfig$legendPosition <- NULL

  # Check if the types of the remaining fields are the same between both configurations
  for (field in defaultFields) {
    expect_equal(
      class(esqlabsConfig[[field]]),
      class(defaultConfig[[field]]),
      info = paste("Field", field, "has different types")
    )
  }
})

# single observed and simulated datasets
oneObsSimDC <- readRDS(getTestDataFilePath("oneObsSimDC"))

test_that(".parseMultiValueField numeric conversion path is covered", {
  # Direct test to ensure numeric conversion code path is covered
  result <- esqlabsR:::.parseMultiValueField(
    value = "72.5, 80.5",
    fieldName = "test",
    plotID = "P1",
    expectedLength = 2,
    expectedType = "numeric"
  )
  expect_equal(result, c(72.5, 80.5))
  expect_true(is.numeric(result))

  # Test space-separated numeric values trigger correct error
  expect_error(
    esqlabsR:::.parseMultiValueField(
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

test_that("createEsqlabsPlotConfiguration() works with ospsuite::plotIndividualTimeProfile", {
  esqlabsConfig <- createEsqlabsPlotConfiguration()

  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "time profile - esqlabsPlotConfiguration",
    fig = plotIndividualTimeProfile(oneObsSimDC, esqlabsConfig)
  )
})

# createPlots ------------------------------------------------------------

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
  baseProject = project,
  dataCombined = dataCombinedDf,
  plotConfiguration = plotConfigurationDf,
  plotGrids = plotGridsDf,
  exportConfiguration = exportConfigurationDf
) {
  pcLocal <- baseProject$clone()
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
  dataCombinedDfLocal$dataSet <- c(
    scenarioNames[1],
    names(observedDataForSetup)[1]
  )
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
  dataCombinedDfLocal$dataSet <- c(
    scenarioNames[1],
    names(observedDataForSetup)[1]
  )
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
    regexp = messages$stopDataCombinedNamesNotFound("foo")
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

# addPlot / removePlot ----

test_that("addPlot appends a row and marks the project modified", {
  pc <- testProject()
  pc$.markSaved()
  before <- nrow(pc$plots$plotConfiguration)

  addPlot(
    project = pc,
    plotID = "PNew",
    dataCombinedName = "AciclovirPVB",
    plotType = "individual",
    title = "Hello"
  )

  expect_equal(nrow(pc$plots$plotConfiguration), before + 1L)
  newRow <- pc$plots$plotConfiguration[
    pc$plots$plotConfiguration$plotID == "PNew",
  ]
  expect_equal(newRow$DataCombinedName, "AciclovirPVB")
  expect_equal(newRow$plotType, "individual")
  expect_equal(newRow$title, "Hello")
  expect_true(pc$modified)
})

test_that("addPlot errors on duplicate plotID", {
  pc <- testProject()
  expect_error(
    addPlot(pc, "P1", "AciclovirPVB", "individual"),
    regexp = "already exists"
  )
})

test_that("addPlot errors on unknown DataCombinedName", {
  pc <- testProject()
  expect_error(
    addPlot(pc, "PX", "NoSuchDC", "individual"),
    regexp = "not found"
  )
})

test_that("addPlot errors on unknown plotType", {
  pc <- testProject()
  expect_error(
    addPlot(pc, "PX", "AciclovirPVB", "bogusType"),
    regexp = "plotType"
  )
})

test_that("addPlot keeps optional NULL args as NA columns", {
  pc <- testProject()
  addPlot(
    pc,
    "PNullArg",
    "AciclovirPVB",
    "individual",
    title = NULL,
    subtitle = "set"
  )
  newRow <- pc$plots$plotConfiguration[
    pc$plots$plotConfiguration$plotID == "PNullArg",
  ]
  expect_true("title" %in% names(newRow))
  expect_true(is.na(newRow$title))
  expect_equal(newRow$subtitle, "set")
})

test_that("addPlot R6 method delegates to standalone", {
  pc1 <- testProject()
  pc2 <- testProject()
  addPlot(pc1, "PD1", "AciclovirPVB", "individual")
  pc2$addPlot("PD1", "AciclovirPVB", "individual")
  expect_equal(
    nrow(pc1$plots$plotConfiguration),
    nrow(pc2$plots$plotConfiguration)
  )
})

test_that("addPlot returns project invisibly", {
  pc <- testProject()
  out <- withVisible(addPlot(pc, "PInv", "AciclovirPVB", "individual"))
  expect_false(out$visible)
  expect_identical(out$value, pc)
})

test_that("removePlot drops the row and marks modified", {
  pc <- testProject()
  pc$.markSaved()
  before <- nrow(pc$plots$plotConfiguration)
  # P3 is referenced by plotGrid "Aciclovir"; the dangling-reference warning
  # is asserted in a dedicated test below.
  suppressWarnings(removePlot(pc, "P3"))
  expect_equal(nrow(pc$plots$plotConfiguration), before - 1L)
  expect_false("P3" %in% pc$plots$plotConfiguration$plotID)
  expect_true(pc$modified)
})

test_that("removePlot warns and is a no-op for unknown plotID", {
  pc <- testProject()
  pc$.markSaved()
  expect_warning(
    removePlot(pc, "NoSuchPlot_ZZZ"),
    regexp = "not found"
  )
  expect_false(pc$modified)
})

test_that("removePlot warns when the plot is referenced in a plotGrid", {
  pc <- testProject()
  expect_warning(
    removePlot(pc, "P1"),
    regexp = "referenced"
  )
})

# addPlotGrid / removePlotGrid ----

test_that("addPlotGrid joins plotIDs into a comma string and marks modified", {
  pc <- testProject()
  pc$.markSaved()
  before <- nrow(pc$plots$plotGrids)

  addPlotGrid(
    project = pc,
    name = "GridNew",
    plotIDs = c("P1", "P2"),
    title = "T"
  )

  expect_equal(nrow(pc$plots$plotGrids), before + 1L)
  newRow <- pc$plots$plotGrids[pc$plots$plotGrids$name == "GridNew", ]
  expect_equal(newRow$plotIDs, "P1, P2")
  expect_equal(newRow$title, "T")
  expect_true(pc$modified)
})

test_that("addPlotGrid errors on duplicate name", {
  pc <- testProject()
  expect_error(
    addPlotGrid(pc, "Aciclovir", c("P1")),
    regexp = "already exists"
  )
})

test_that("addPlotGrid errors when plotIDs reference unknown plots", {
  pc <- testProject()
  expect_error(
    addPlotGrid(pc, "GridX", c("P1", "NoSuchPlot")),
    regexp = "not found"
  )
})

test_that("addPlotGrid R6 method delegates to standalone", {
  pc1 <- testProject()
  pc2 <- testProject()
  addPlotGrid(pc1, "G1", c("P1"))
  pc2$addPlotGrid("G1", c("P1"))
  expect_equal(nrow(pc1$plots$plotGrids), nrow(pc2$plots$plotGrids))
})

test_that("addPlotGrid returns project invisibly", {
  pc <- testProject()
  out <- withVisible(addPlotGrid(pc, "GInv", c("P1")))
  expect_false(out$visible)
  expect_identical(out$value, pc)
})

test_that("removePlotGrid drops the row and marks modified", {
  pc <- testProject()
  pc$.markSaved()
  before <- nrow(pc$plots$plotGrids)
  removePlotGrid(pc, "Aciclovir2")
  expect_equal(nrow(pc$plots$plotGrids), before - 1L)
  expect_false("Aciclovir2" %in% pc$plots$plotGrids$name)
  expect_true(pc$modified)
})

test_that("removePlotGrid warns and is a no-op for unknown name", {
  pc <- testProject()
  pc$.markSaved()
  expect_warning(
    removePlotGrid(pc, "NoSuchGrid_ZZZ"),
    regexp = "not found"
  )
  expect_false(pc$modified)
})
