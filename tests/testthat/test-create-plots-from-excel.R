projectConfiguration <- createDefaultProjectConfiguration(file.path("..", "data", "ProjectConfiguration_forTests.xlsx"))

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
simulatedScenarios <- runScenarios(
  scenarioConfigurations = scenarioConfigurations,
  customParams = NULL, saveSimulationsToPKML = FALSE
)

# Load observed data
dataSheets <- "Laskin 1982.Group A"
observedData <- esqlabsR::loadObservedData(
  projectConfiguration = projectConfiguration,
  sheets = dataSheets
)

dataCombinedDf <- data.frame(list("DataCombinedName" = c("AciclovirPVB", "AciclovirPVB"),
                                  "dataType" = c("simulated", "observed"),
                                  "label" = c("Aciclovir simulated", "Aciclovir observed"),
                                  "scenario" = c(scenarioNames, NA),
                                  "path" = c(outputPaths, NA),
                                             "dataSet" = c(NA, names(observedData)),
                                             "group" = c(NA, NA),
                                             "xOffsets" = c(NA, NA),
                                             "yOffsets" = c(NA, NA),
                                             "xScaleFactors" = c(NA, NA),
                                             "yScaleFactors" = c(NA, NA)))
plotConfigurationDf <- data.frame(list("plotID" = "P1",
                                       "DataCombinedName" = "AciclovirPVB",
                                                                    "plotType" = "individual",
                                                                    "title" = NA,
                                                                    "xUnit" = NA,
                                                                    "yUnit" = NA,
                                                                    "xAxisScale" = NA,
                                                                    "yAxisScale" = NA,
                                                                    "xLimLower" = NA,
                                                                    "xLimUpper" = NA,
                                                                    "yLimLower" = NA,
                                                                    "yLimUpper" = NA))
plotGridsDf <- data.frame(list("name" = "Aciclovir",
                                       "plotIDs" = "P1",
                                       "title" = "Aciclovir PVB"))

# Validation DataCombined
test_that("It trows an error if mandatory field dataType is not filled out", {
  tempDir <- tempdir()
  projectConfigurationLocal <- projectConfiguration$clone()
  projectConfigurationLocal$paramsFolder <- tempDir
  withr::with_tempfile(new = "Plots.xlsx",
                       tmpdir = tempDir,
                       code = {
                         dataCombinedDfLocal <- dataCombinedDf
                         plotConfigurationDfLocal <- plotConfigurationDf
                         plotGridsDfLocal <- plotGridsDf
                         dataCombinedDfLocal$dataType <- NA
                         writeExcel(data = list("DataCombined" = dataCombinedDfLocal,
                                                "plotConfiguration" = plotConfigurationDfLocal,
                                                "plotGrids" = plotGridsDfLocal), path = file.path(tempDir, "Plots.xlsx"), )


                         expect_error(createPlotsFromExcel(simulatedScenarios = simulatedScenarios,
                                                           observedData = observedData,
                                                           projectConfiguration = projectConfigurationLocal,
                                                           stopIfNotFound = TRUE), regexp = messages$missingDataType())
                       }
  )
}
)

test_that("It trows an error if mandatory field label is not filled out", {
  tempDir <- tempdir()
  projectConfigurationLocal <- projectConfiguration$clone()
  projectConfigurationLocal$paramsFolder <- tempDir
  withr::with_tempfile(new = "Plots.xlsx",
                       tmpdir = tempDir,
                       code = {
                         dataCombinedDfLocal <- dataCombinedDf
                         plotConfigurationDfLocal <- plotConfigurationDf
                         dataCombinedDfLocal$label <- NA
                         plotGridsDfLocal <- plotGridsDf
                         writeExcel(data = list("DataCombined" = dataCombinedDfLocal,
                                                "plotConfiguration" = plotConfigurationDfLocal,
                                                "plotGrids" = plotGridsDfLocal), path = file.path(tempDir, "Plots.xlsx"), )


                         expect_error(createPlotsFromExcel(simulatedScenarios = simulatedScenarios,
                                                           observedData = observedData,
                                                           projectConfiguration = projectConfigurationLocal,
                                                           stopIfNotFound = TRUE), regexp = messages$missingLabel())
                       }
  )
}
)

test_that("It trows an error if no scenario is specified for a simulated data", {
  tempDir <- tempdir()
  projectConfigurationLocal <- projectConfiguration$clone()
  projectConfigurationLocal$paramsFolder <- tempDir
  withr::with_tempfile(new = "Plots.xlsx",
                       tmpdir = tempDir,
                       code = {
                         dataCombinedDfLocal <- dataCombinedDf
                         plotConfigurationDfLocal <- plotConfigurationDf
                         dataCombinedDfLocal$scenario <- NA
                         plotGridsDfLocal <- plotGridsDf
                         writeExcel(data = list("DataCombined" = dataCombinedDfLocal,
                                                "plotConfiguration" = plotConfigurationDfLocal,
                                                "plotGrids" = plotGridsDfLocal), path = file.path(tempDir, "Plots.xlsx"), )


                         expect_error(createPlotsFromExcel(simulatedScenarios = simulatedScenarios,
                                                           observedData = observedData,
                                                           projectConfiguration = projectConfigurationLocal,
                                                           stopIfNotFound = TRUE), regexp = messages$missingScenarioName())
                       }
  )
}
)

test_that("It trows an error if no output path is specified for a simulated data", {
  tempDir <- tempdir()
  projectConfigurationLocal <- projectConfiguration$clone()
  projectConfigurationLocal$paramsFolder <- tempDir
  withr::with_tempfile(new = "Plots.xlsx",
                       tmpdir = tempDir,
                       code = {
                         dataCombinedDfLocal <- dataCombinedDf
                         plotConfigurationDfLocal <- plotConfigurationDf
                         dataCombinedDfLocal$path <- NA
                         plotGridsDfLocal <- plotGridsDf
                         writeExcel(data = list("DataCombined" = dataCombinedDfLocal,
                                                "plotConfiguration" = plotConfigurationDfLocal,
                                                "plotGrids" = plotGridsDfLocal), path = file.path(tempDir, "Plots.xlsx"), )


                         expect_error(createPlotsFromExcel(simulatedScenarios = simulatedScenarios,
                                                           observedData = observedData,
                                                           projectConfiguration = projectConfigurationLocal,
                                                           stopIfNotFound = TRUE), regexp = messages$stopNoPathProvided("AciclovirPVB"))
                       }
  )
}
)

test_that("It trows an error if no data set is specified for observed data", {
  tempDir <- tempdir()
  projectConfigurationLocal <- projectConfiguration$clone()
  projectConfigurationLocal$paramsFolder <- tempDir
  withr::with_tempfile(new = "Plots.xlsx",
                       tmpdir = tempDir,
                       code = {
                         dataCombinedDfLocal <- dataCombinedDf
                         plotConfigurationDfLocal <- plotConfigurationDf
                         dataCombinedDfLocal$dataSet <- NA
                         plotGridsDfLocal <- plotGridsDf
                         writeExcel(data = list("DataCombined" = dataCombinedDfLocal,
                                                "plotConfiguration" = plotConfigurationDfLocal,
                                                "plotGrids" = plotGridsDfLocal), path = file.path(tempDir, "Plots.xlsx"), )


                         expect_error(createPlotsFromExcel(simulatedScenarios = simulatedScenarios,
                                                           observedData = observedData,
                                                           projectConfiguration = projectConfigurationLocal,
                                                           stopIfNotFound = TRUE), regexp = messages$stopNoDataSetProvided("AciclovirPVB"))
                       }
  )
}
)

test_that("It trows an error if defined scenario is missing and stopIfNotFound is TRUE", {
  tempDir <- tempdir()
  projectConfigurationLocal <- projectConfiguration$clone()
  projectConfigurationLocal$paramsFolder <- tempDir
  withr::with_tempfile(new = "Plots.xlsx",
                       tmpdir = tempDir,
                       code = {
                         dataCombinedDfLocal <- dataCombinedDf
                         plotConfigurationDfLocal <- plotConfigurationDf
                         dataCombinedDfLocal$scenario <- c("AciclovirPVB", "foo")
                         plotGridsDfLocal <- plotGridsDf
                         writeExcel(data = list("DataCombined" = dataCombinedDfLocal,
                                                "plotConfiguration" = plotConfigurationDfLocal,
                                                "plotGrids" = plotGridsDfLocal), path = file.path(tempDir, "Plots.xlsx"), )


                         expect_error(createPlotsFromExcel(simulatedScenarios = simulatedScenarios,
                                                           observedData = observedData,
                                                           projectConfiguration = projectConfigurationLocal,
                                                           stopIfNotFound = TRUE), regexp = messages$stopInvalidScenarioName("foo"))
                       }
  )
}
)

#FAILING
test_that("It shows a warning for missing scenarios if stopIfNotFound is FALSE", {
  tempDir <- tempdir()
  projectConfigurationLocal <- projectConfiguration$clone()
  projectConfigurationLocal$paramsFolder <- tempDir
  withr::with_tempfile(new = "Plots.xlsx",
                       tmpdir = tempDir,
                       code = {
                         dataCombinedDfLocal <- dataCombinedDf
                         plotConfigurationDfLocal <- plotConfigurationDf
                         dataCombinedDfLocal$scenario <- c(scenarioNames, "foo")
                         plotGridsDfLocal <- plotGridsDf
                         writeExcel(data = list("DataCombined" = dataCombinedDfLocal,
                                                "plotConfiguration" = plotConfigurationDfLocal,
                                                "plotGrids" = plotGridsDfLocal), path = file.path(tempDir, "Plots.xlsx"), )


                        expect_warning(createPlotsFromExcel(simulatedScenarios = simulatedScenarios,
                                                           observedData = observedData,
                                                           projectConfiguration = projectConfigurationLocal,
                                                           stopIfNotFound = FALSE), regexp = messages$warningInvalidScenarioName("foo"))
                       }
  )
}
)

test_that("It trows an error if defined data set is missing and stopIfNotFound is TRUE", {
  tempDir <- tempdir()
  projectConfigurationLocal <- projectConfiguration$clone()
  projectConfigurationLocal$paramsFolder <- tempDir
  withr::with_tempfile(new = "Plots.xlsx",
                       tmpdir = tempDir,
                       code = {
                         dataCombinedDfLocal <- dataCombinedDf
                         plotConfigurationDfLocal <- plotConfigurationDf
                         dataCombinedDfLocal$dataSet <- c(scenarioNames, names(observedData))
                         plotGridsDfLocal <- plotGridsDf
                         writeExcel(data = list("DataCombined" = dataCombinedDfLocal,
                                                "plotConfiguration" = plotConfigurationDfLocal,
                                                "plotGrids" = plotGridsDfLocal), path = file.path(tempDir, "Plots.xlsx"), )

                         expect_error(createPlotsFromExcel(simulatedScenarios = simulatedScenarios,
                                                           observedData = observedData,
                                                           projectConfiguration = projectConfigurationLocal,
                                                           stopIfNotFound = TRUE), regexp = messages$stopInvalidDataSetName(scenarioNames))
                       }
  )
}
)

#FAILING
test_that("It shows a warning for missing data set if stopIfNotFound is FALSE", {
  tempDir <- tempdir()
  projectConfigurationLocal <- projectConfiguration$clone()
  projectConfigurationLocal$paramsFolder <- tempDir
  withr::with_tempfile(new = "Plots.xlsx",
                       tmpdir = tempDir,
                       code = {
                         dataCombinedDfLocal <- dataCombinedDf
                         plotConfigurationDfLocal <- plotConfigurationDf
                         dataCombinedDfLocal$scenario <- c(scenarioNames, "foo")
                         plotGridsDfLocal <- plotGridsDf
                         writeExcel(data = list("DataCombined" = dataCombinedDfLocal,
                                                "plotConfiguration" = plotConfigurationDfLocal,
                                                "plotGrids" = plotGridsDfLocal), path = file.path(tempDir, "Plots.xlsx"), )

                         expect_warning(createPlotsFromExcel(simulatedScenarios = simulatedScenarios,
                                                           observedData = observedData,
                                                           projectConfiguration = projectConfigurationLocal,
                                                           stopIfNotFound = TRUE), regexp = messages$warningInvalidDataSetName(scenarioNames))
                       }
  )
}
)

# Validation plotConfiguration
test_that("It returns NULL if no plotConfiguration are defined in the excel sheet", {
  tempDir <- tempdir()
  projectConfigurationLocal <- projectConfiguration$clone()
  projectConfigurationLocal$paramsFolder <- tempDir
  withr::with_tempfile(new = "Plots.xlsx",
                       tmpdir = tempDir,
                       code = {
                         dataCombinedDfLocal <- dataCombinedDf
                         plotConfigurationDfLocal <- plotConfigurationDf
                         plotConfigurationDfLocal <- data.frame(list("plotID" = NA,
                                                                     "plotType" = NA,
                                                                     "title" = NA,
                                                                     "xUnit" = NA,
                                                                     "yUnit" = NA,
                                                                     "xAxisScale" = NA,
                                                                     "yAxisScale" = NA,
                                                                     "xLimLower" = NA,
                                                                     "xLimUpper" = NA,
                                                                     "yLimLower" = NA,
                                                                     "yLimUpper" = NA))
                         plotGridsDfLocal <- plotGridsDf
                         writeExcel(data = list("DataCombined" = dataCombinedDfLocal,
                                                "plotConfiguration" = plotConfigurationDfLocal,
                                                "plotGrids" = plotGridsDfLocal), path = file.path(tempDir, "Plots.xlsx"), )

                         plots <- createPlotsFromExcel(simulatedScenarios = simulatedScenarios,
                                                       observedData = observedData,
                                                       projectConfiguration = projectConfigurationLocal,
                                                       stopIfNotFound = TRUE)
                         expect_null(plots)
                       }
  )
}
)

test_that("It trows an error if mandatory field DataCombinedName is not filled out", {
  tempDir <- tempdir()
  projectConfigurationLocal <- projectConfiguration$clone()
  projectConfigurationLocal$paramsFolder <- tempDir
  withr::with_tempfile(new = "Plots.xlsx",
                       tmpdir = tempDir,
                       code = {
                         dataCombinedDfLocal <- dataCombinedDf
                         plotConfigurationDfLocal <- plotConfigurationDf
                         plotConfigurationDfLocal$DataCombinedName <- NA
                         plotGridsDfLocal <- plotGridsDf
                         writeExcel(data = list("DataCombined" = dataCombinedDfLocal,
                                                "plotConfiguration" = plotConfigurationDfLocal,
                                                "plotGrids" = plotGridsDfLocal), path = file.path(tempDir, "Plots.xlsx"), )

                         expect_error(createPlotsFromExcel(simulatedScenarios = simulatedScenarios,
                                                           observedData = observedData,
                                                           projectConfiguration = projectConfigurationLocal,
                                                           stopIfNotFound = TRUE), regexp = messages$missingDataCombinedName())
                       }
  )
}
)

test_that("It trows an error if mandatory field plotType is not filled out", {
  tempDir <- tempdir()
  projectConfigurationLocal <- projectConfiguration$clone()
  projectConfigurationLocal$paramsFolder <- tempDir
  withr::with_tempfile(new = "Plots.xlsx",
                       tmpdir = tempDir,
                       code = {
                         dataCombinedDfLocal <- dataCombinedDf
                         plotConfigurationDfLocal <- plotConfigurationDf
                         plotConfigurationDfLocal$plotType <- NA
                         plotGridsDfLocal <- plotGridsDf
                         writeExcel(data = list("DataCombined" = dataCombinedDfLocal,
                                                "plotConfiguration" = plotConfigurationDfLocal,
                                                "plotGrids" = plotGridsDfLocal), path = file.path(tempDir, "Plots.xlsx"), )

                         expect_error(createPlotsFromExcel(simulatedScenarios = simulatedScenarios,
                                                           observedData = observedData,
                                                           projectConfiguration = projectConfigurationLocal,
                                                           stopIfNotFound = TRUE), regexp = messages$missingPlotType())
                       }
  )
}
)

test_that("It trows an error if a plot requires a DataCombined that is not defined", {
  tempDir <- tempdir()
  projectConfigurationLocal <- projectConfiguration$clone()
  projectConfigurationLocal$paramsFolder <- tempDir
  withr::with_tempfile(new = "Plots.xlsx",
                       tmpdir = tempDir,
                       code = {
                         dataCombinedDfLocal <- dataCombinedDf
                         plotConfigurationDfLocal <- plotConfigurationDf
                         plotConfigurationDfLocal$DataCombinedName <- "foo"
                         plotGridsDfLocal <- plotGridsDf
                         writeExcel(data = list("DataCombined" = dataCombinedDfLocal,
                                                "plotConfiguration" = plotConfigurationDfLocal,
                                                "plotGrids" = plotGridsDfLocal), path = file.path(tempDir, "Plots.xlsx"), )

                         expect_error(createPlotsFromExcel(simulatedScenarios = simulatedScenarios,
                                                           observedData = observedData,
                                                           projectConfiguration = projectConfigurationLocal,
                                                           stopIfNotFound = TRUE), regexp = messages$stopInvalidDataCombinedName("foo"))
                       }
  )
}
)

# Validation plotGrids
test_that("It returns NULL if no plotGrids are defined in the excel sheet", {
  tempDir <- tempdir()
  projectConfigurationLocal <- projectConfiguration$clone()
  projectConfigurationLocal$paramsFolder <- tempDir
  withr::with_tempfile(new = "Plots.xlsx",
                       tmpdir = tempDir,
                       code = {
                         dataCombinedDfLocal <- dataCombinedDf
                         plotConfigurationDfLocal <- plotConfigurationDf
                         plotGridsDfLocal <- plotGridsDf
                         plotGridsDfLocal <- data.frame(list("name" = NA,
                                                             "plotIDs" = NA,
                                                             "title" = NA))
                         writeExcel(data = list("DataCombined" = dataCombinedDfLocal,
                                                "plotConfiguration" = plotConfigurationDfLocal,
                                                "plotGrids" = plotGridsDfLocal), path = file.path(tempDir, "Plots.xlsx"), )


                         plots <- createPlotsFromExcel(simulatedScenarios = simulatedScenarios,
                                                       observedData = observedData,
                                                       projectConfiguration = projectConfigurationLocal,
                                                       stopIfNotFound = TRUE)
                         expect_null(plots)
                       }
  )
}
)

test_that("It trows an error if mandatory field plotIDs is not filled out", {
  tempDir <- tempdir()
  projectConfigurationLocal <- projectConfiguration$clone()
  projectConfigurationLocal$paramsFolder <- tempDir
  withr::with_tempfile(new = "Plots.xlsx",
                       tmpdir = tempDir,
                       code = {
                         dataCombinedDfLocal <- dataCombinedDf
                         plotConfigurationDfLocal <- plotConfigurationDf
                         plotGridsDfLocal <- plotGridsDf
                         plotGridsDfLocal$plotIDs <- NA
                         writeExcel(data = list("DataCombined" = dataCombinedDfLocal,
                                                "plotConfiguration" = plotConfigurationDfLocal,
                                                "plotGrids" = plotGridsDfLocal), path = file.path(tempDir, "Plots.xlsx"), )


                         expect_error(createPlotsFromExcel(simulatedScenarios = simulatedScenarios,
                                                           observedData = observedData,
                                                           projectConfiguration = projectConfigurationLocal,
                                                           stopIfNotFound = TRUE), regexp = messages$missingPlotIDs())
                       }
  )
}
)

test_that("It trows an error if a plot grid requires a plot id that is not defined", {
  tempDir <- tempdir()
  projectConfigurationLocal <- projectConfiguration$clone()
  projectConfigurationLocal$paramsFolder <- tempDir
  withr::with_tempfile(new = "Plots.xlsx",
                       tmpdir = tempDir,
                       code = {
                         dataCombinedDfLocal <- dataCombinedDf
                         plotConfigurationDfLocal <- plotConfigurationDf
                         plotGridsDfLocal <- plotGridsDf
                         plotGridsDfLocal$plotIDs <- "foo"
                         writeExcel(data = list("DataCombined" = dataCombinedDfLocal,
                                                "plotConfiguration" = plotConfigurationDfLocal,
                                                "plotGrids" = plotGridsDfLocal), path = file.path(tempDir, "Plots.xlsx"), )

                         expect_error(createPlotsFromExcel(simulatedScenarios = simulatedScenarios,
                                                           observedData = observedData,
                                                           projectConfiguration = projectConfigurationLocal,
                                                           stopIfNotFound = TRUE), regexp = messages$errorInvalidPlotID("foo"))
                       }
  )
}
)

# Creation of plots
test_that("It creates a plot if no data transformations are present", {
  plots <- createPlotsFromExcel(simulatedScenarios = simulatedScenarios,
                                    observedData = observedData,
                                    projectConfiguration = projectConfiguration,
                                    stopIfNotFound = TRUE)
}
)
