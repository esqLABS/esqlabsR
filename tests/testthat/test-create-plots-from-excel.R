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

########### Load observed data - data template v10########
# Which sheets to load
dataSheets <- "Laskin 1982.Group A"
observedData <- esqlabsR::loadObservedData(
  projectConfiguration = projectConfiguration,
  sheets = dataSheets
)

test_that("It returns NULL if no DataCombined are defined in the excel sheet", {
  tempDir <- tempdir()
  projectConfigurationLocal <- projectConfiguration$clone()
  projectConfigurationLocal$paramsFolder <- tempDir
  withr::with_tempfile(new = "Plots.xlsx",
                       tmpdir = tempDir,
                       code = {
                         df <- data.frame(list("DataCombinedName" = NA,
                                               "dataType" = NA,
                                               "label" = NA,
                                               "scenario" = NA,
                                               "path" = NA,
                                               "dataSet" = NA,
                                               "group" = NA,
                                               "xOffsets" = NA,
                                               "yOffsets" = NA,
                                               "xScaleFactors" = NA,
                                               "yScaleFactors" = NA))
                         writeExcel(data = list("DataCombined" = df), path = file.path(tempDir, "Plots.xlsx"), )


                         plots <- createPlotsFromExcel(simulatedScenarios = simulatedScenarios,
                                                       observedData = observedData,
                                                       projectConfiguration = projectConfigurationLocal,
                                                       stopIfNotFound = TRUE)
                         expect_null(plots)
                       }
  )
}
)

test_that("It trows an error if mandatory label field is not filled out", {
  tempDir <- tempdir()
  projectConfigurationLocal <- projectConfiguration$clone()
  projectConfigurationLocal$paramsFolder <- tempDir
  withr::with_tempfile(new = "Plots.xlsx",
                       tmpdir = tempDir,
                       code = {
                         df <- data.frame(list("DataCombinedName" = "AciclovirPVB_noLabel",
                                               "dataType" = NA,
                                               "label" = NA,
                                               "scenario" = NA,
                                               "path" = NA,
                                               "dataSet" = NA,
                                               "group" = NA,
                                               "xOffsets" = NA,
                                               "yOffsets" = NA,
                                               "xScaleFactors" = NA,
                                               "yScaleFactors" = NA))
                         writeExcel(data = list("DataCombined" = df), path = file.path(tempDir, "Plots.xlsx"), )


                         expect_error(createPlotsFromExcel(simulatedScenarios = simulatedScenarios,
                                                           observedData = observedData,
                                                           projectConfiguration = projectConfiguration,
                                                           stopIfNotFound = TRUE), regexp = messages$missingLabel())
                       }
  )


}
)

test_that("It trows an error if not output path is specified for a simulated data", {
  tempDir <- tempdir()
  projectConfigurationLocal <- projectConfiguration$clone()
  projectConfigurationLocal$paramsFolder <- tempDir
  withr::with_tempfile(new = "Plots.xlsx",
                       tmpdir = tempDir,
                       code = {
                         df <- data.frame(list("DataCombinedName" = "AciclovirPVB",
                                               "dataType" = "simulated",
                                               "label" = "Aciclovir simulated",
                                               "scenario" = "TestScenario",
                                               "path" = NA,
                                               "dataSet" = NA,
                                               "group" = NA,
                                               "xOffsets" = NA,
                                               "yOffsets" = NA,
                                               "xScaleFactors" = NA,
                                               "yScaleFactors" = NA))
                         writeExcel(data = list("DataCombined" = df), path = file.path(tempDir, "Plots.xlsx"), )


                         expect_error(createPlotsFromExcel(simulatedScenarios = simulatedScenarios,
                                                           observedData = observedData,
                                                           projectConfiguration = projectConfiguration,
                                                           stopIfNotFound = TRUE), regexp = messages$stopNoPathProvided("AciclovirPVB"))
                       }
  )


}
)

# It does not fail when no transformations are specified




test_that("It creates a plot if no data transformations are present", {
  plots <- createPlotsFromExcel(simulatedScenarios = simulatedScenarios,
                                    observedData = observedData,
                                    projectConfiguration = projectConfiguration,
                                    stopIfNotFound = TRUE)
}
)
