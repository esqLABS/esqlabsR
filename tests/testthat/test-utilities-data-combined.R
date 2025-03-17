projectConfiguration <- testProjectConfiguration()

# Define which scenarios to run
scenarioNames <- c("TestScenario", "PopulationScenario")
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

importerConfiguration <- ospsuite::loadDataImporterConfiguration(
  configurationFilePath = projectConfiguration$dataImporterConfigurationFile
)

# Load observed data
dataSheets <- "Laskin 1982.Group A"
observedData <- esqlabsR::loadObservedData(
  projectConfiguration = projectConfiguration,
  sheets = dataSheets,
  importerConfiguration = importerConfiguration
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

# It returns an empty data combined when no data set is found
test_that("It returns an empty DataCombined when no data is available", {
  tempDir <- tempdir()
  projectConfigurationLocal <- projectConfiguration$clone()
  projectConfigurationLocal$configurationsFolder <- tempDir
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
      .writeExcel(data = list(
        "DataCombined" = dataCombinedDfLocal,
        "plotConfiguration" = plotConfigurationDfLocal,
        "plotGrids" = plotGridsDfLocal,
        "exportConfiguration" = exportConfigurationDfLocal
      ), path = file.path(tempDir, "Plots.xlsx"), )

      # Warnings are suppressed because they are expected but not relevant for
      # this test.
      suppressWarnings({
        dataCombined <- createDataCombinedFromExcel(
          file = file.path(tempDir, "Plots.xlsx"),
          stopIfNotFound = FALSE
        )
      })


      expect_equal(dataCombined[[1]], DataCombined$new())
    }
  )
})

test_that("It creates specified DataCombined", {
  dc <- createDataCombinedFromExcel(
    file = projectConfiguration$plotsFile,
    sheet = "DataCombined",
    dataCombinedNames = "AciclovirPVB",
    simulatedScenarios = simulatedScenarios,
    observedData = observedData,
    stopIfNotFound = TRUE
  )
  expect_equal(names(dc), c("AciclovirPVB"))
  expect_true(isOfType(dc, "DataCombined"))
})

test_that("It creates DataCombined for the specified plots", {
  dc <- createDataCombinedForPlots(
    plotGridNames = c("P2", "P4"),
    projectConfiguration = projectConfiguration,
    simulatedScenarios = simulatedScenarios,
    observedData = observedData
  )
  expect_equal(names(dc), c("AciclovirPVB", "AciclovirPop"))
  expect_true(isOfType(dc, "DataCombined"))
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
