projectConfiguration <- test_ProjectConfiguration()

# Define which scenarios to run
scenarioNames <- c("TestScenario")
outputPaths <- "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)"

# Create `ScenarioConfiguration` objects from excel files
scenarioConfigurations <- readScenarioConfigurationFromExcel(
  scenarioNames = scenarioNames,
  projectConfiguration = projectConfiguration
)

# Set output paths for each scenario
for (scenarioConfiguration in scenarioConfigurations) {
  scenarioConfiguration$outputPaths <- outputPaths
}

# Run scenarios
scenarios <- createScenarios(scenarioConfigurations = scenarioConfigurations)

simulatedScenarios <- runScenarios(
  scenarios = scenarios
)

# Load observed data
dataSheets <- "Laskin 1982.Group A"
observedData <- esqlabsR::loadObservedData(
  projectConfiguration = projectConfiguration,
  sheets = dataSheets
)

dataCombinedDf <- data.frame(list(
  "DataCombinedName" = c("AciclovirPVB", "AciclovirPVB"),
  "dataType" = c("simulated", "observed"),
  "label" = c("Aciclovir simulated", "Aciclovir observed"),
  "scenario" = c(scenarioNames, NA),
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
  "xAxisLimits" = NA,
  "yAxisLimits" = NA,
  "quantiles" = NA,
  "foldDistance" = NA
))
plotGridsDf <- data.frame(list(
  "name" = "Aciclovir",
  "plotIDs" = "P1",
  "title" = "Aciclovir PVB"
))

exportConfigurationDf <- data.frame(list("plotGridName" = character(0), "outputName" = character(0)))

# Validation DataCombined
test_that("It trows an error if mandatory field dataType is not filled out", {
  tempDir <- tempdir()
  projectConfigurationLocal <- projectConfiguration$clone()
  projectConfigurationLocal$paramsFolder <- tempDir
  withr::with_tempfile(
    new = "Plots.xlsx",
    tmpdir = tempDir,
    code = {
      dataCombinedDfLocal <- dataCombinedDf
      plotConfigurationDfLocal <- plotConfigurationDf
      plotGridsDfLocal <- plotGridsDf
      dataCombinedDfLocal$dataType <- NA
      exportConfigurationDfLocal <- exportConfigurationDf
      writeExcel(data = list(
        "DataCombined" = dataCombinedDfLocal,
        "plotConfiguration" = plotConfigurationDfLocal,
        "plotGrids" = plotGridsDfLocal,
        "exportConfiguration" = exportConfigurationDfLocal
      ), path = file.path(tempDir, "Plots.xlsx"), )


      expect_error(createPlotsFromExcel(
        simulatedScenarios = simulatedScenarios,
        observedData = observedData,
        projectConfiguration = projectConfigurationLocal,
        stopIfNotFound = TRUE
      ), regexp = messages$missingDataType())
    }
  )
})

test_that("It trows an error if mandatory field label is not filled out", {
  tempDir <- tempdir()
  projectConfigurationLocal <- projectConfiguration$clone()
  projectConfigurationLocal$paramsFolder <- tempDir
  withr::with_tempfile(
    new = "Plots.xlsx",
    tmpdir = tempDir,
    code = {
      dataCombinedDfLocal <- dataCombinedDf
      plotConfigurationDfLocal <- plotConfigurationDf
      dataCombinedDfLocal$label <- NA
      plotGridsDfLocal <- plotGridsDf
      exportConfigurationDfLocal <- exportConfigurationDf
      writeExcel(data = list(
        "DataCombined" = dataCombinedDfLocal,
        "plotConfiguration" = plotConfigurationDfLocal,
        "plotGrids" = plotGridsDfLocal,
        "exportConfiguration" = exportConfigurationDfLocal
      ), path = file.path(tempDir, "Plots.xlsx"), )


      expect_error(createPlotsFromExcel(
        simulatedScenarios = simulatedScenarios,
        observedData = observedData,
        projectConfiguration = projectConfigurationLocal,
        stopIfNotFound = TRUE
      ), regexp = messages$missingLabel())
    }
  )
})

test_that("It trows an error if no scenario is specified for a simulated data", {
  tempDir <- tempdir()
  projectConfigurationLocal <- projectConfiguration$clone()
  projectConfigurationLocal$paramsFolder <- tempDir
  withr::with_tempfile(
    new = "Plots.xlsx",
    tmpdir = tempDir,
    code = {
      dataCombinedDfLocal <- dataCombinedDf
      plotConfigurationDfLocal <- plotConfigurationDf
      dataCombinedDfLocal$scenario <- NA
      plotGridsDfLocal <- plotGridsDf
      exportConfigurationDfLocal <- exportConfigurationDf
      writeExcel(data = list(
        "DataCombined" = dataCombinedDfLocal,
        "plotConfiguration" = plotConfigurationDfLocal,
        "plotGrids" = plotGridsDfLocal,
        "exportConfiguration" = exportConfigurationDfLocal
      ), path = file.path(tempDir, "Plots.xlsx"), )


      expect_error(createPlotsFromExcel(
        simulatedScenarios = simulatedScenarios,
        observedData = observedData,
        projectConfiguration = projectConfigurationLocal,
        stopIfNotFound = TRUE
      ), regexp = messages$missingScenarioName())
    }
  )
})

test_that("It trows an error if no output path is specified for a simulated data", {
  tempDir <- tempdir()
  projectConfigurationLocal <- projectConfiguration$clone()
  projectConfigurationLocal$paramsFolder <- tempDir
  withr::with_tempfile(
    new = "Plots.xlsx",
    tmpdir = tempDir,
    code = {
      dataCombinedDfLocal <- dataCombinedDf
      plotConfigurationDfLocal <- plotConfigurationDf
      dataCombinedDfLocal$path <- NA
      plotGridsDfLocal <- plotGridsDf
      exportConfigurationDfLocal <- exportConfigurationDf
      writeExcel(data = list(
        "DataCombined" = dataCombinedDfLocal,
        "plotConfiguration" = plotConfigurationDfLocal,
        "plotGrids" = plotGridsDfLocal,
        "exportConfiguration" = exportConfigurationDfLocal
      ), path = file.path(tempDir, "Plots.xlsx"), )


      expect_error(createPlotsFromExcel(
        simulatedScenarios = simulatedScenarios,
        observedData = observedData,
        projectConfiguration = projectConfigurationLocal,
        stopIfNotFound = TRUE
      ), regexp = messages$stopNoPathProvided("AciclovirPVB"))
    }
  )
})

test_that("It trows an error if wrong output path is specified for a simulated data", {
  tempDir <- tempdir()
  projectConfigurationLocal <- projectConfiguration$clone()
  projectConfigurationLocal$paramsFolder <- tempDir
  withr::with_tempfile(
    new = "Plots.xlsx",
    tmpdir = tempDir,
    code = {
      dataCombinedDfLocal <- dataCombinedDf
      plotConfigurationDfLocal <- plotConfigurationDf
      dataCombinedDfLocal$path <- "foo"
      plotGridsDfLocal <- plotGridsDf
      exportConfigurationDfLocal <- exportConfigurationDf
      writeExcel(data = list(
        "DataCombined" = dataCombinedDfLocal,
        "plotConfiguration" = plotConfigurationDfLocal,
        "plotGrids" = plotGridsDfLocal,
        "exportConfiguration" = exportConfigurationDfLocal
      ), path = file.path(tempDir, "Plots.xlsx"), )


      expect_error(createPlotsFromExcel(
        simulatedScenarios = simulatedScenarios,
        observedData = observedData,
        projectConfiguration = projectConfigurationLocal,
        stopIfNotFound = TRUE
      ), regexp = messages$stopWrongOutputPath(
        dataCombinedName = dataCombinedDfLocal$DataCombinedName[[1]],
        scenarioName = dataCombinedDfLocal$scenario[[1]],
        path = dataCombinedDfLocal$path[[1]]
      ))
    }
  )
})

test_that("It trows an error if no data set is specified for observed data", {
  tempDir <- tempdir()
  projectConfigurationLocal <- projectConfiguration$clone()
  projectConfigurationLocal$paramsFolder <- tempDir
  withr::with_tempfile(
    new = "Plots.xlsx",
    tmpdir = tempDir,
    code = {
      dataCombinedDfLocal <- dataCombinedDf
      plotConfigurationDfLocal <- plotConfigurationDf
      dataCombinedDfLocal$dataSet <- NA
      plotGridsDfLocal <- plotGridsDf
      exportConfigurationDfLocal <- exportConfigurationDf
      writeExcel(data = list(
        "DataCombined" = dataCombinedDfLocal,
        "plotConfiguration" = plotConfigurationDfLocal,
        "plotGrids" = plotGridsDfLocal,
        "exportConfiguration" = exportConfigurationDfLocal
      ), path = file.path(tempDir, "Plots.xlsx"), )


      expect_error(createPlotsFromExcel(
        simulatedScenarios = simulatedScenarios,
        observedData = observedData,
        projectConfiguration = projectConfigurationLocal,
        stopIfNotFound = TRUE
      ), regexp = messages$stopNoDataSetProvided("AciclovirPVB"))
    }
  )
})

test_that("It trows an error if defined scenario is missing and stopIfNotFound is TRUE", {
  tempDir <- tempdir()
  projectConfigurationLocal <- projectConfiguration$clone()
  projectConfigurationLocal$paramsFolder <- tempDir
  withr::with_tempfile(
    new = "Plots.xlsx",
    tmpdir = tempDir,
    code = {
      dataCombinedDfLocal <- dataCombinedDf
      plotConfigurationDfLocal <- plotConfigurationDf
      dataCombinedDfLocal$scenario <- c("TestScenario", "foo")
      plotGridsDfLocal <- plotGridsDf
      exportConfigurationDfLocal <- exportConfigurationDf
      writeExcel(data = list(
        "DataCombined" = dataCombinedDfLocal,
        "plotConfiguration" = plotConfigurationDfLocal,
        "plotGrids" = plotGridsDfLocal,
        "exportConfiguration" = exportConfigurationDfLocal
      ), path = file.path(tempDir, "Plots.xlsx"), )


      expect_error(createPlotsFromExcel(
        simulatedScenarios = simulatedScenarios,
        observedData = observedData,
        projectConfiguration = projectConfigurationLocal,
        stopIfNotFound = TRUE
      ), regexp = messages$stopInvalidScenarioName("foo"))
    }
  )
})

test_that("It shows a warning for missing scenarios if stopIfNotFound is FALSE", {
  tempDir <- tempdir()
  projectConfigurationLocal <- projectConfiguration$clone()
  projectConfigurationLocal$paramsFolder <- tempDir
  withr::with_tempfile(
    new = "Plots.xlsx",
    tmpdir = tempDir,
    code = {
      dataCombinedDfLocal <- dataCombinedDf
      plotConfigurationDfLocal <- plotConfigurationDf
      dataCombinedDfLocal$scenario <- c(scenarioNames, "foo")
      plotGridsDfLocal <- plotGridsDf
      exportConfigurationDfLocal <- exportConfigurationDf
      writeExcel(data = list(
        "DataCombined" = dataCombinedDfLocal,
        "plotConfiguration" = plotConfigurationDfLocal,
        "plotGrids" = plotGridsDfLocal,
        "exportConfiguration" = exportConfigurationDfLocal
      ), path = file.path(tempDir, "Plots.xlsx"), )


      expect_warning(createPlotsFromExcel(
        simulatedScenarios = simulatedScenarios,
        observedData = observedData,
        projectConfiguration = projectConfigurationLocal,
        stopIfNotFound = FALSE
      ), regexp = messages$warningInvalidScenarioName("foo"))
    }
  )
})

test_that("It trows an error if defined data set is missing and stopIfNotFound is TRUE", {
  tempDir <- tempdir()
  projectConfigurationLocal <- projectConfiguration$clone()
  projectConfigurationLocal$paramsFolder <- tempDir
  withr::with_tempfile(
    new = "Plots.xlsx",
    tmpdir = tempDir,
    code = {
      dataCombinedDfLocal <- dataCombinedDf
      plotConfigurationDfLocal <- plotConfigurationDf
      dataCombinedDfLocal$dataSet <- c(scenarioNames, names(observedData))
      plotGridsDfLocal <- plotGridsDf
      exportConfigurationDfLocal <- exportConfigurationDf
      writeExcel(data = list(
        "DataCombined" = dataCombinedDfLocal,
        "plotConfiguration" = plotConfigurationDfLocal,
        "plotGrids" = plotGridsDfLocal,
        "exportConfiguration" = exportConfigurationDfLocal
      ), path = file.path(tempDir, "Plots.xlsx"), )

      expect_error(createPlotsFromExcel(
        simulatedScenarios = simulatedScenarios,
        observedData = observedData,
        projectConfiguration = projectConfigurationLocal,
        stopIfNotFound = TRUE
      ), regexp = messages$stopInvalidDataSetName(scenarioNames))
    }
  )
})

test_that("It shows a warning for missing data set if stopIfNotFound is FALSE", {
  tempDir <- tempdir()
  projectConfigurationLocal <- projectConfiguration$clone()
  projectConfigurationLocal$paramsFolder <- tempDir
  withr::with_tempfile(
    new = "Plots.xlsx",
    tmpdir = tempDir,
    code = {
      dataCombinedDfLocal <- dataCombinedDf
      plotConfigurationDfLocal <- plotConfigurationDf
      dataCombinedDfLocal$dataSet <- c(scenarioNames, names(observedData))
      plotGridsDfLocal <- plotGridsDf
      exportConfigurationDfLocal <- exportConfigurationDf
      writeExcel(data = list(
        "DataCombined" = dataCombinedDfLocal,
        "plotConfiguration" = plotConfigurationDfLocal,
        "plotGrids" = plotGridsDfLocal,
        "exportConfiguration" = exportConfigurationDfLocal
      ), path = file.path(tempDir, "Plots.xlsx"), )

      expect_warning(createPlotsFromExcel(
        simulatedScenarios = simulatedScenarios,
        observedData = observedData,
        projectConfiguration = projectConfigurationLocal,
        stopIfNotFound = FALSE
      ), regexp = messages$warningInvalidDataSetName(scenarioNames))
    }
  )
})

test_that("It trows an error if mandatory field DataCombinedName is not filled out", {
  tempDir <- tempdir()
  projectConfigurationLocal <- projectConfiguration$clone()
  projectConfigurationLocal$paramsFolder <- tempDir
  withr::with_tempfile(
    new = "Plots.xlsx",
    tmpdir = tempDir,
    code = {
      dataCombinedDfLocal <- dataCombinedDf
      plotConfigurationDfLocal <- plotConfigurationDf
      plotConfigurationDfLocal$DataCombinedName <- NA
      plotGridsDfLocal <- plotGridsDf
      exportConfigurationDfLocal <- exportConfigurationDf
      writeExcel(data = list(
        "DataCombined" = dataCombinedDfLocal,
        "plotConfiguration" = plotConfigurationDfLocal,
        "plotGrids" = plotGridsDfLocal,
        "exportConfiguration" = exportConfigurationDfLocal
      ), path = file.path(tempDir, "Plots.xlsx"), )

      expect_error(createPlotsFromExcel(
        simulatedScenarios = simulatedScenarios,
        observedData = observedData,
        projectConfiguration = projectConfigurationLocal,
        stopIfNotFound = TRUE
      ), regexp = messages$missingDataCombinedName())
    }
  )
})

test_that("It trows an error if mandatory field plotType is not filled out", {
  tempDir <- tempdir()
  projectConfigurationLocal <- projectConfiguration$clone()
  projectConfigurationLocal$paramsFolder <- tempDir
  withr::with_tempfile(
    new = "Plots.xlsx",
    tmpdir = tempDir,
    code = {
      dataCombinedDfLocal <- dataCombinedDf
      plotConfigurationDfLocal <- plotConfigurationDf
      plotConfigurationDfLocal$plotType <- NA
      plotGridsDfLocal <- plotGridsDf
      exportConfigurationDfLocal <- exportConfigurationDf
      writeExcel(data = list(
        "DataCombined" = dataCombinedDfLocal,
        "plotConfiguration" = plotConfigurationDfLocal,
        "plotGrids" = plotGridsDfLocal,
        "exportConfiguration" = exportConfigurationDfLocal
      ), path = file.path(tempDir, "Plots.xlsx"), )

      expect_error(createPlotsFromExcel(
        simulatedScenarios = simulatedScenarios,
        observedData = observedData,
        projectConfiguration = projectConfigurationLocal,
        stopIfNotFound = TRUE
      ), regexp = messages$missingPlotType())
    }
  )
})

test_that("It trows an error if a plot requires a DataCombined that is not defined", {
  tempDir <- tempdir()
  projectConfigurationLocal <- projectConfiguration$clone()
  projectConfigurationLocal$paramsFolder <- tempDir
  withr::with_tempfile(
    new = "Plots.xlsx",
    tmpdir = tempDir,
    code = {
      dataCombinedDfLocal <- dataCombinedDf
      plotConfigurationDfLocal <- plotConfigurationDf
      plotConfigurationDfLocal$DataCombinedName <- "foo"
      plotGridsDfLocal <- plotGridsDf
      exportConfigurationDfLocal <- exportConfigurationDf
      writeExcel(data = list(
        "DataCombined" = dataCombinedDfLocal,
        "plotConfiguration" = plotConfigurationDfLocal,
        "plotGrids" = plotGridsDfLocal,
        "exportConfiguration" = exportConfigurationDfLocal
      ), path = file.path(tempDir, "Plots.xlsx"), )

      expect_error(createPlotsFromExcel(
        simulatedScenarios = simulatedScenarios,
        observedData = observedData,
        projectConfiguration = projectConfigurationLocal,
        stopIfNotFound = TRUE
      ), regexp = messages$stopInvalidDataCombinedName("foo"))
    }
  )
})

# Validation plotGrids
test_that("It returns NULL if no plotGrids are defined in the excel sheet", {
  tempDir <- tempdir()
  projectConfigurationLocal <- projectConfiguration$clone()
  projectConfigurationLocal$paramsFolder <- tempDir
  withr::with_tempfile(
    new = "Plots.xlsx",
    tmpdir = tempDir,
    code = {
      dataCombinedDfLocal <- dataCombinedDf
      plotConfigurationDfLocal <- plotConfigurationDf
      plotGridsDfLocal <- plotGridsDf
      plotGridsDfLocal <- data.frame(list(
        "name" = NA,
        "plotIDs" = NA,
        "title" = NA
      ))
      exportConfigurationDfLocal <- exportConfigurationDf
      writeExcel(data = list(
        "DataCombined" = dataCombinedDfLocal,
        "plotConfiguration" = plotConfigurationDfLocal,
        "plotGrids" = plotGridsDfLocal,
        "exportConfiguration" = exportConfigurationDfLocal
      ), path = file.path(tempDir, "Plots.xlsx"), )


      plots <- createPlotsFromExcel(
        simulatedScenarios = simulatedScenarios,
        observedData = observedData,
        projectConfiguration = projectConfigurationLocal,
        stopIfNotFound = TRUE
      )
      expect_null(plots)
    }
  )
})

test_that("It creates plots for all plot grids when plotGridNames is NULL", {
  plots <- createPlotsFromExcel(
    simulatedScenarios = simulatedScenarios,
    observedData = observedData,
    projectConfiguration = projectConfiguration,
    stopIfNotFound = TRUE
  )
  expect_equal(names(plots), c("Aciclovir", "Aciclovr2"))
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

test_that("It trows an error if mandatory field plotIDs is not filled out", {
  tempDir <- tempdir()
  projectConfigurationLocal <- projectConfiguration$clone()
  projectConfigurationLocal$paramsFolder <- tempDir
  withr::with_tempfile(
    new = "Plots.xlsx",
    tmpdir = tempDir,
    code = {
      dataCombinedDfLocal <- dataCombinedDf
      plotConfigurationDfLocal <- plotConfigurationDf
      plotGridsDfLocal <- plotGridsDf
      plotGridsDfLocal$plotIDs <- NA
      exportConfigurationDfLocal <- exportConfigurationDf
      writeExcel(data = list(
        "DataCombined" = dataCombinedDfLocal,
        "plotConfiguration" = plotConfigurationDfLocal,
        "plotGrids" = plotGridsDfLocal,
        "exportConfiguration" = exportConfigurationDfLocal
      ), path = file.path(tempDir, "Plots.xlsx"), )


      expect_error(createPlotsFromExcel(
        simulatedScenarios = simulatedScenarios,
        observedData = observedData,
        projectConfiguration = projectConfigurationLocal,
        stopIfNotFound = TRUE
      ), regexp = messages$missingPlotIDs())
    }
  )
})

test_that("It trows an error if a plot grid requires a plot id that is not defined", {
  tempDir <- tempdir()
  projectConfigurationLocal <- projectConfiguration$clone()
  projectConfigurationLocal$paramsFolder <- tempDir
  withr::with_tempfile(
    new = "Plots.xlsx",
    tmpdir = tempDir,
    code = {
      dataCombinedDfLocal <- dataCombinedDf
      plotConfigurationDfLocal <- plotConfigurationDf
      plotGridsDfLocal <- plotGridsDf
      plotGridsDfLocal$plotIDs <- "foo"
      exportConfigurationDfLocal <- exportConfigurationDf
      writeExcel(data = list(
        "DataCombined" = dataCombinedDfLocal,
        "plotConfiguration" = plotConfigurationDfLocal,
        "plotGrids" = plotGridsDfLocal,
        "exportConfiguration" = exportConfigurationDfLocal
      ), path = file.path(tempDir, "Plots.xlsx"), )

      expect_error(createPlotsFromExcel(
        simulatedScenarios = simulatedScenarios,
        observedData = observedData,
        projectConfiguration = projectConfigurationLocal,
        stopIfNotFound = TRUE
      ), regexp = messages$errorInvalidPlotID("foo"))
    }
  )
})

test_that("It exports plot grids as defined in sheet `exportConfiguration`", {
  tempDir <- tempdir()
  projectConfigurationLocal <- projectConfiguration$clone()
  projectConfigurationLocal$paramsFolder <- tempDir
  projectConfigurationLocal$outputFolder <- tempDir
  withr::with_tempfile(
    new = "Plots.xlsx",
    tmpdir = tempDir,
    code = {
      dataCombinedDfLocal <- dataCombinedDf
      plotConfigurationDfLocal <- plotConfigurationDf
      plotGridsDfLocal <- plotGridsDf
      exportConfigurationDfLocal <- data.frame(
        plotGridName = rep("Aciclovir", 2),
        outputName = c("Aciclovir1", "Aciclovir2"),
        height = c(10, NA)
      )
      writeExcel(data = list(
        "DataCombined" = dataCombinedDfLocal,
        "plotConfiguration" = plotConfigurationDfLocal,
        "plotGrids" = plotGridsDfLocal,
        "exportConfiguration" = exportConfigurationDfLocal
      ), path = file.path(tempDir, "Plots.xlsx"), )


      createPlotsFromExcel(
        simulatedScenarios = simulatedScenarios,
        observedData = observedData,
        projectConfiguration = projectConfigurationLocal,
        stopIfNotFound = TRUE
      )
      expect_true(file.exists(file.path(tempDir, "Aciclovir1.png")))
      expect_true(file.exists(file.path(tempDir, "Aciclovir2.png")))
    }
  )
})

test_that("It throws an error when trying to set a property that is not supported by the configuration", {
  tempDir <- tempdir()
  projectConfigurationLocal <- projectConfiguration$clone()
  projectConfigurationLocal$paramsFolder <- tempDir
  withr::with_tempfile(
    new = "Plots.xlsx",
    tmpdir = tempDir,
    code = {
      dataCombinedDfLocal <- dataCombinedDf
      plotConfigurationDfLocal <- plotConfigurationDf
      plotConfigurationDfLocal$"blabla" <- "1,2,3"

      plotGridsDfLocal <- plotGridsDf
      exportConfigurationDfLocal <- exportConfigurationDf
      writeExcel(data = list(
        "DataCombined" = dataCombinedDfLocal,
        "plotConfiguration" = plotConfigurationDfLocal,
        "plotGrids" = plotGridsDfLocal,
        "exportConfiguration" = exportConfigurationDfLocal
      ), path = file.path(tempDir, "Plots.xlsx"), )

      expect_error(createPlotsFromExcel(
        simulatedScenarios = simulatedScenarios,
        observedData = observedData,
        projectConfiguration = projectConfigurationLocal,
        stopIfNotFound = TRUE
      ), regexp = messages$invalidConfigurationPropertyFromExcel(
        propertyName = "blabla",
        configurationType = "DefaultPlotConfiguration"
      ))
    }
  )
})

test_that("It correctly treats names with underscores", {
  tempDir <- tempdir()
  projectConfigurationLocal <- projectConfiguration$clone()
  projectConfigurationLocal$paramsFolder <- tempDir
  projectConfigurationLocal$outputFolder <- tempDir
  withr::with_tempfile(
    new = "Plots.xlsx",
    tmpdir = tempDir,
    code = {
      dataCombinedDfLocal <- dataCombinedDf
      plotConfigurationDfLocal <- plotConfigurationDf
      plotConfigurationDfLocal$plotID <- "P_1"
      plotGridsDfLocal <- plotGridsDf
      plotGridsDfLocal$plotIDs <- "P_1"
      exportConfigurationDfLocal <- data.frame(
        plotGridName = rep("Aciclovir", 2),
        outputName = c("Aciclovir1", "Aciclovir2"),
        height = c(10, NA)
      )
      writeExcel(data = list(
        "DataCombined" = dataCombinedDfLocal,
        "plotConfiguration" = plotConfigurationDfLocal,
        "plotGrids" = plotGridsDfLocal,
        "exportConfiguration" = exportConfigurationDfLocal
      ), path = file.path(tempDir, "Plots.xlsx"), )


      createPlotsFromExcel(
        simulatedScenarios = simulatedScenarios,
        observedData = observedData,
        projectConfiguration = projectConfigurationLocal,
        stopIfNotFound = TRUE
      )
      expect_true(file.exists(file.path(tempDir, "Aciclovir1.png")))
      expect_true(file.exists(file.path(tempDir, "Aciclovir2.png")))
    }
  )
})

test_that("It throws a warning when trying to export non-existent plot grid to file", {
  tempDir <- tempdir()
  projectConfigurationLocal <- projectConfiguration$clone()
  projectConfigurationLocal$paramsFolder <- tempDir
  withr::with_tempfile(
    new = "Plots.xlsx",
    tmpdir = tempDir,
    code = {
      dataCombinedDfLocal <- dataCombinedDf
      plotConfigurationDfLocal <- plotConfigurationDf
      plotGridsDfLocal <- plotGridsDf
      exportConfigurationDfLocal <- data.frame(
        plotGridName = "invalidPlotGridName",
        outputName = "Aciclovir1"
      )
      writeExcel(data = list(
        "DataCombined" = dataCombinedDfLocal,
        "plotConfiguration" = plotConfigurationDfLocal,
        "plotGrids" = plotGridsDfLocal,
        "exportConfiguration" = exportConfigurationDfLocal
      ), path = file.path(tempDir, "Plots.xlsx"), )

      expect_warning(createPlotsFromExcel(
        simulatedScenarios = simulatedScenarios,
        observedData = observedData,
        projectConfiguration = projectConfigurationLocal,
        stopIfNotFound = TRUE
      ), regexp = messages$missingPlotGrids(missingPlotGrids = "invalidPlotGridName"))
    }
  )
})

test_that("It throws a warning when outputName is missing in sheet 'exportConfiguration'", {
  tempDir <- tempdir()
  projectConfigurationLocal <- projectConfiguration$clone()
  projectConfigurationLocal$paramsFolder <- tempDir
  withr::with_tempfile(
    new = "Plots.xlsx",
    tmpdir = tempDir,
    code = {
      dataCombinedDfLocal <- dataCombinedDf
      plotConfigurationDfLocal <- plotConfigurationDf
      plotGridsDfLocal <- plotGridsDf
      exportConfigurationDfLocal <- data.frame(
        plotGridName = "Aciclovir",
        outputName = NA
      )
      writeExcel(data = list(
        "DataCombined" = dataCombinedDfLocal,
        "plotConfiguration" = plotConfigurationDfLocal,
        "plotGrids" = plotGridsDfLocal,
        "exportConfiguration" = exportConfigurationDfLocal
      ), path = file.path(tempDir, "Plots.xlsx"), )

      expect_warning(createPlotsFromExcel(
        simulatedScenarios = simulatedScenarios,
        observedData = observedData,
        projectConfiguration = projectConfigurationLocal,
        stopIfNotFound = TRUE
      ), regexp = messages$missingOutputFileName())
    }
  )
})

test_that(".createConfigurationFromRow correctly reads values in quotes", {
  tempDir <- tempdir()
  projectConfigurationLocal <- projectConfiguration$clone()
  projectConfigurationLocal$paramsFolder <- tempDir

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
      writeExcel(data = list(
        "plotGrids" = plotGridsDfLocal
      ), path = file.path(tempDir, "Plots.xlsx"), )

      plotGridsDfFromExcel <- readExcel(file.path(tempDir, "Plots.xlsx"), sheet = "plotGrids")

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

# It returns an empty data combined when no data set is found
test_that("It returns an empty DataCombined when no data is available", {
  tempDir <- tempdir()
  projectConfigurationLocal <- projectConfiguration$clone()
  projectConfigurationLocal$paramsFolder <- tempDir
  withr::with_tempfile(
    new = "Plots.xlsx",
    tmpdir = tempDir,
    code = {
      dataCombinedDfLocal <- dataCombinedDf
      plotConfigurationDfLocal <- plotConfigurationDf
      plotGridsDfLocal <- plotGridsDf
      exportConfigurationDfLocal <- data.frame(
        plotGridName = c(NA),
        outputName = c(NA)
      )
      writeExcel(data = list(
        "DataCombined" = dataCombinedDfLocal,
        "plotConfiguration" = plotConfigurationDfLocal,
        "plotGrids" = plotGridsDfLocal,
        "exportConfiguration" = exportConfigurationDfLocal
      ), path = file.path(tempDir, "Plots.xlsx"), )

      dataCombined <- createDataCombinedFromExcel(
        file = file.path(tempDir, "Plots.xlsx"),
        stopIfNotFound = FALSE
      )
      expect_equal(dataCombined[[1]], DataCombined$new())
    }
  )
})
